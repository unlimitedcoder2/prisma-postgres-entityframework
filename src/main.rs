use std::fs;

use psl::builtin_connectors::PostgresType;
use serde::{Deserialize, Serialize};
use sql_schema_describer::{
	ColumnArity, ColumnTypeFamily, DefaultKind, EnumId, EnumVariantWalker, EnumWalker,
	ForeignKeyAction, ForeignKeyWalker, IndexType, IndexWalker, PrismaValue, SqlSchema,
	TableColumnId, TableColumnWalker, TableDefaultValueWalker, TableWalker,
};
use std::any::Any;

pub struct UnsafeAccessDatabaseSchema(Box<dyn std::any::Any + Send + Sync>);

#[derive(Default, Debug)]
pub struct UnsafeAccessSqlDatabaseSchema {
	pub describer_schema: SqlSchema,
	pub prisma_level_defaults: Vec<TableColumnId>,
}

/// Foreign key action types (for ON DELETE|ON UPDATE).
#[derive(Serialize, Deserialize, PartialEq, Debug, Clone, Copy)]
#[serde(rename_all = "snake_case")]
pub enum DatabaseForeignKeyAction {
	/// Produce an error indicating that the deletion or update would create a foreign key
	/// constraint violation. If the constraint is deferred, this error will be produced at
	/// constraint check time if there still exist any referencing rows. This is the default action.
	NoAction,
	/// Produce an error indicating that the deletion or update would create a foreign key
	/// constraint violation. This is the same as NO ACTION except that the check is not deferrable.
	Restrict,
	/// Delete any rows referencing the deleted row, or update the values of the referencing
	/// column(s) to the new values of the referenced columns, respectively.
	Cascade,
	/// Set the referencing column(s) to null.
	SetNull,
	/// Set the referencing column(s) to their default values. (There must be a row in the
	/// referenced table matching the default values, if they are not null, or the operation
	/// will fail).
	SetDefault,
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone, Copy)]
#[serde(rename_all = "snake_case")]
pub enum DatabaseIndexType {
	/// Unique type.
	Unique,
	/// Normal type.
	Normal,
	/// Fulltext type.
	Fulltext,
	/// The table's primary key
	PrimaryKey,
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone, Copy)]
#[serde(rename_all = "snake_case")]
pub enum DatabaseColumnArity {
	/// Required column.
	Required,
	/// Nullable column.
	Nullable,
	/// List type column.
	List,
}

#[derive(Deserialize, Serialize, Debug, Clone)]
#[serde(tag = "t", content = "c", rename_all = "snake_case")]
enum DatabaseType {
	SmallInt,
	Int,
	BigInt,
	Float,
	Decimal,
	Boolean,
	Binary,
	Enum(EnumId),
	Unsupported(String),
	Money,
	Inet,
	Oid,
	Citext,
	// Real,
	Double,
	VarChar(Option<u32>),
	Char(Option<u32>),
	Text,
	ByteA,
	Timestamp(Option<u32>),
	Timestamptz(Option<u32>),
	Date,
	Time(Option<u32>),
	Timetz(Option<u32>),
	/* Bit(Option<u32>),
	VarBit(Option<u32>), */
	Uuid,
	Xml,
	Json,
	JsonB,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize, PartialOrd, Ord)]
#[serde(tag = "t", content = "c", rename_all = "snake_case")]
pub enum DatabasePrismaValue {
	String(String),
	Boolean(bool),
	Enum(String),
	Int(i64),
	Null,
	Bytes(Vec<u8>),
}

fn get_prisma_default(value: PrismaValue) -> Option<DatabasePrismaValue> {
	match value {
		PrismaValue::String(v) => Some(DatabasePrismaValue::String(v)),
		PrismaValue::Boolean(v) => Some(DatabasePrismaValue::Boolean(v)),
		PrismaValue::Enum(v) => Some(DatabasePrismaValue::Enum(v)),
		PrismaValue::Int(v) => Some(DatabasePrismaValue::Int(v)),
		PrismaValue::Null => Some(DatabasePrismaValue::Null),

		PrismaValue::BigInt(_) => None,
		PrismaValue::Json(_) => None,
		PrismaValue::Bytes(_) => None,
		PrismaValue::Uuid(_) => None,
		PrismaValue::DateTime(_) => None,
		PrismaValue::Float(_) => None,
		PrismaValue::List(_) => None,
		PrismaValue::Object(_) => None,
	}
}

/// A DefaultValue
#[derive(Serialize, PartialEq, Debug, Clone)]
#[serde(tag = "t", content = "c", rename_all = "snake_case")]
pub enum DatabaseDefaultKind {
	/// A constant value, parsed as String
	Value(DatabasePrismaValue),
	/// An expression generating a current timestamp.
	Now,
	/// An expression generating a sequence.
	Sequence(String),
	/// A unique row ID,
	UniqueRowid,
	/// An unrecognized Default Value
	DbGenerated(Option<String>),
}

#[derive(Serialize, Debug, Clone)]
struct DatabaseColumn {
	name: String,
	arity: DatabaseColumnArity,
	tpe: DatabaseType,
	default: Option<DatabaseDefaultKind>,
}

#[derive(Deserialize, Serialize, Debug, Clone)]
struct DatabaseForeignKey {
	name: String,
	table_name: String,
	referenced_table_name: String,
	column_names: Vec<String>,
	referenced_column_names: Vec<String>,
	on_delete_action: DatabaseForeignKeyAction,
}

impl Into<DatabaseForeignKey> for ForeignKeyWalker<'_> {
	fn into(self) -> DatabaseForeignKey {
		DatabaseForeignKey {
			name: self
				.constraint_name()
				.expect("No constraint name")
				.to_string(),
			table_name: self.table().name().to_string(),
			referenced_table_name: self.referenced_table_name().to_string(),
			column_names: self
				.constrained_columns()
				.map(|c| c.name().to_string())
				.collect::<Vec<String>>(),
			referenced_column_names: self
				.referenced_columns()
				.map(|c| c.name().to_string())
				.collect::<Vec<String>>(),
			on_delete_action: match self.on_delete_action() {
				ForeignKeyAction::NoAction => DatabaseForeignKeyAction::NoAction,
				ForeignKeyAction::Restrict => DatabaseForeignKeyAction::Restrict,
				ForeignKeyAction::Cascade => DatabaseForeignKeyAction::Cascade,
				ForeignKeyAction::SetNull => DatabaseForeignKeyAction::SetNull,
				ForeignKeyAction::SetDefault => DatabaseForeignKeyAction::SetDefault,
			},
		}
	}
}

#[derive(Serialize, Debug, Clone)]
struct DatabaseTable {
	name: String,
	columns: Vec<DatabaseColumn>,
	indexes: Vec<DatabaseIndex>,
}

#[derive(Deserialize, Serialize, Debug, Clone)]
struct DatabaseIndex {
	name: String,
	tpe: DatabaseIndexType,
	columns: Vec<String>,
}

#[derive(Serialize, Debug, Clone)]
struct DatabaseEnumVariant {
	name: String,
}

#[derive(Serialize, Debug, Clone)]
struct DatabaseEnum {
	id: EnumId,
	name: String,
	variants: Vec<DatabaseEnumVariant>,
}

#[derive(Serialize, Debug, Clone)]
struct DatabaseSchema {
	tables: Vec<DatabaseTable>,
	foreign_keys: Vec<DatabaseForeignKey>,
	enums: Vec<DatabaseEnum>,
}

fn get_default_value(default: Option<TableDefaultValueWalker<'_>>) -> Option<DatabaseDefaultKind> {
	match default {
		Some(d) => match d.kind() {
			DefaultKind::Value(v) => {
				Some(DatabaseDefaultKind::Value(get_prisma_default(v.clone())?))
			}
			DefaultKind::Now => Some(DatabaseDefaultKind::Now),
			DefaultKind::Sequence(s) => Some(DatabaseDefaultKind::Sequence(s.to_string())),
			DefaultKind::UniqueRowid => Some(DatabaseDefaultKind::UniqueRowid),
			DefaultKind::DbGenerated(s) => Some(DatabaseDefaultKind::DbGenerated(s.clone())),
		},
		None => None,
	}
}

impl Into<DatabaseColumn> for TableColumnWalker<'_> {
	fn into(self) -> DatabaseColumn {
		DatabaseColumn {
			name: self.name().to_string(),
			arity: match self.column_type().arity {
				ColumnArity::Required => DatabaseColumnArity::Required,
				ColumnArity::Nullable => DatabaseColumnArity::Nullable,
				ColumnArity::List => DatabaseColumnArity::List,
			},
			tpe: get_column_type(
				self.column_native_type::<PostgresType>(),
				self.column_type().family.clone(),
			),
			default: get_default_value(self.default()),
		}
	}
}

fn get_column_type(
	postgres_type: Option<&PostgresType>,
	sql_type: ColumnTypeFamily,
) -> DatabaseType {
	if let Some(pg_type) = postgres_type {
		match pg_type {
			PostgresType::SmallInt => DatabaseType::SmallInt,
			PostgresType::Integer => DatabaseType::Int,
			PostgresType::BigInt => DatabaseType::BigInt,
			PostgresType::Decimal(_) => DatabaseType::Decimal,
			PostgresType::Money => DatabaseType::Money,
			PostgresType::Inet => DatabaseType::Inet,
			PostgresType::Oid => DatabaseType::Oid,
			PostgresType::Citext => DatabaseType::Citext,
			PostgresType::Real => DatabaseType::Decimal,
			PostgresType::DoublePrecision => DatabaseType::Double,
			PostgresType::VarChar(len) => DatabaseType::VarChar(*len),
			PostgresType::Char(len) => DatabaseType::Char(*len),
			PostgresType::Text => DatabaseType::Text,
			PostgresType::ByteA => DatabaseType::ByteA,
			PostgresType::Timestamp(precision) => DatabaseType::Timestamp(*precision),
			PostgresType::Timestamptz(precision) => DatabaseType::Timestamptz(*precision),
			PostgresType::Date => DatabaseType::Date,
			PostgresType::Time(precision) => DatabaseType::Time(*precision),
			PostgresType::Timetz(precision) => DatabaseType::Timetz(*precision),
			PostgresType::Boolean => DatabaseType::Boolean,
			PostgresType::Bit(_) =>
			/* DatabaseType::Bit(*len) */
			{
				DatabaseType::Binary
			}
			PostgresType::VarBit(_) =>
			/* DatabaseType::VarBit(*len) */
			{
				DatabaseType::Binary
			}
			PostgresType::Uuid => DatabaseType::Uuid,
			PostgresType::Xml => DatabaseType::Xml,
			PostgresType::Json => DatabaseType::Json,
			PostgresType::JsonB => DatabaseType::JsonB,
		}
	} else {
		match sql_type {
			ColumnTypeFamily::Int => DatabaseType::Int,
			ColumnTypeFamily::BigInt => DatabaseType::BigInt,
			ColumnTypeFamily::Float => DatabaseType::Float,
			ColumnTypeFamily::Decimal => DatabaseType::Decimal,
			ColumnTypeFamily::Boolean => DatabaseType::Boolean,
			ColumnTypeFamily::String => DatabaseType::Text,
			ColumnTypeFamily::DateTime => DatabaseType::Timestamp(None),
			ColumnTypeFamily::Binary => DatabaseType::Binary,
			ColumnTypeFamily::Json => DatabaseType::Json,
			ColumnTypeFamily::Uuid => DatabaseType::Uuid,
			ColumnTypeFamily::Enum(id) => DatabaseType::Enum(id),
			ColumnTypeFamily::Unsupported(t) => DatabaseType::Unsupported(t),
		}
	}
}

impl Into<DatabaseTable> for TableWalker<'_> {
	fn into(self) -> DatabaseTable {
		DatabaseTable {
			name: self.name().to_string(),
			columns: self
				.columns()
				.map(|c| c.into())
				.collect::<Vec<DatabaseColumn>>(),
			indexes: self
				.indexes()
				.map(|i| i.into())
				.collect::<Vec<DatabaseIndex>>(),
		}
	}
}

impl Into<DatabaseIndex> for IndexWalker<'_> {
	fn into(self) -> DatabaseIndex {
		DatabaseIndex {
			name: self.name().to_string(),
			tpe: match self.index_type() {
				IndexType::Unique => DatabaseIndexType::Unique,
				IndexType::Normal => DatabaseIndexType::Normal,
				IndexType::Fulltext => DatabaseIndexType::Fulltext,
				IndexType::PrimaryKey => DatabaseIndexType::PrimaryKey,
			},
			columns: self
				.columns()
				.map(|c| c.name().to_string())
				.collect::<Vec<String>>(),
		}
	}
}

impl Into<DatabaseEnumVariant> for EnumVariantWalker<'_> {
	fn into(self) -> DatabaseEnumVariant {
		DatabaseEnumVariant {
			name: self.name().to_string(),
		}
	}
}

impl Into<DatabaseEnum> for EnumWalker<'_> {
	fn into(self) -> DatabaseEnum {
		DatabaseEnum {
			id: self.id,
			name: self.name().to_string(),
			variants: self.variants().map(|v| v.into()).collect(),
		}
	}
}

fn create_database_schema(schema: &SqlSchema) -> DatabaseSchema {
	DatabaseSchema {
		tables: schema
			.table_walkers()
			.map(|t| t.into())
			.collect::<Vec<DatabaseTable>>(),
		foreign_keys: schema
			.walk_foreign_keys()
			.map(|f| f.into())
			.collect::<Vec<DatabaseForeignKey>>(),
		enums: schema
			.enum_walkers()
			.map(|e| e.into())
			.collect::<Vec<DatabaseEnum>>(),
	}
}

fn create_json_from_prisma(input: String) -> Result<String, String> {
	let schema = match sql_schema_connector::custom::create_schema(input.clone().as_str()) {
		Ok(s) => s,
		Err(e) => {
			return Err(format!("Error creating schema: {}", e));
		}
	};

	let schema: UnsafeAccessDatabaseSchema = unsafe { std::mem::transmute(schema) };

	// This call requires use of the rust nightly branch and is being tracked in this pr https://github.com/rust-lang/rust/issues/90850
	// Its been open for three years so just gonna copy the implementation myself
	// let schema = unsafe { schema.0.downcast_unchecked::<UnsafeAccessSqlDatabaseSchema>() };
	let schema =
		unsafe { &*(&*schema.0 as *const dyn Any as *const UnsafeAccessSqlDatabaseSchema) };

	let db_schema: DatabaseSchema = create_database_schema(&schema.describer_schema);

	let json = match serde_json::to_string_pretty(&db_schema) {
		Ok(j) => j,
		Err(e) => {
			return Err(format!("Error serializing schema: {}", e));
		}
	};

	Ok(json)
}

fn main() {
	let args = std::env::args();
	let args = args.collect::<Vec<String>>();

	let mut args = args.iter();
	args.next();
	let schema_path = match args.next() {
		Some(path) => path,
		None => {
			println!("Usage: <schema_path> <output_path>");
			return;
		}
	};

	let output_path = match args.next() {
		Some(path) => path,
		None => {
			println!("Usage: <schema_path> <output_path>");
			return;
		}
	};

	let schema = match fs::read_to_string(schema_path.clone()) {
		Ok(schema) => schema,
		Err(e) => {
			match e.kind() {
				std::io::ErrorKind::NotFound => {
					println!("File not found: {}", schema_path.clone());
				}
				_ => {
					println!("Error reading file: {}", e);
				}
			}
			return;
		}
	};

	let db_schema = match create_json_from_prisma(schema) {
		Ok(db_schema) => db_schema,
		Err(e) => {
			println!("Error creating dbschema: {}", e);
			return;
		}
	};

	match fs::write(output_path, db_schema) {
		Ok(_) => {}
		Err(e) => {
			println!("Error writing to file: {}", e);
		}
	}
}
