{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Glue.Types.CodeGenConfigurationNode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.CodeGenConfigurationNode where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.Aggregate
import Amazonka.Glue.Types.ApplyMapping
import Amazonka.Glue.Types.AthenaConnectorSource
import Amazonka.Glue.Types.BasicCatalogTarget
import Amazonka.Glue.Types.CatalogKafkaSource
import Amazonka.Glue.Types.CatalogKinesisSource
import Amazonka.Glue.Types.CatalogSource
import Amazonka.Glue.Types.CustomCode
import Amazonka.Glue.Types.DirectKafkaSource
import Amazonka.Glue.Types.DirectKinesisSource
import Amazonka.Glue.Types.DropDuplicates
import Amazonka.Glue.Types.DropFields
import Amazonka.Glue.Types.DropNullFields
import Amazonka.Glue.Types.DynamicTransform
import Amazonka.Glue.Types.DynamoDBCatalogSource
import Amazonka.Glue.Types.EvaluateDataQuality
import Amazonka.Glue.Types.FillMissingValues
import Amazonka.Glue.Types.Filter
import Amazonka.Glue.Types.GovernedCatalogSource
import Amazonka.Glue.Types.GovernedCatalogTarget
import Amazonka.Glue.Types.JDBCConnectorSource
import Amazonka.Glue.Types.JDBCConnectorTarget
import Amazonka.Glue.Types.Join
import Amazonka.Glue.Types.Merge
import Amazonka.Glue.Types.MicrosoftSQLServerCatalogSource
import Amazonka.Glue.Types.MicrosoftSQLServerCatalogTarget
import Amazonka.Glue.Types.MySQLCatalogSource
import Amazonka.Glue.Types.MySQLCatalogTarget
import Amazonka.Glue.Types.OracleSQLCatalogSource
import Amazonka.Glue.Types.OracleSQLCatalogTarget
import Amazonka.Glue.Types.PIIDetection
import Amazonka.Glue.Types.PostgreSQLCatalogSource
import Amazonka.Glue.Types.PostgreSQLCatalogTarget
import Amazonka.Glue.Types.RedshiftSource
import Amazonka.Glue.Types.RedshiftTarget
import Amazonka.Glue.Types.RelationalCatalogSource
import Amazonka.Glue.Types.RenameField
import Amazonka.Glue.Types.S3CatalogSource
import Amazonka.Glue.Types.S3CatalogTarget
import Amazonka.Glue.Types.S3CsvSource
import Amazonka.Glue.Types.S3DirectTarget
import Amazonka.Glue.Types.S3GlueParquetTarget
import Amazonka.Glue.Types.S3JsonSource
import Amazonka.Glue.Types.S3ParquetSource
import Amazonka.Glue.Types.SelectFields
import Amazonka.Glue.Types.SelectFromCollection
import Amazonka.Glue.Types.SparkConnectorSource
import Amazonka.Glue.Types.SparkConnectorTarget
import Amazonka.Glue.Types.SparkSQL
import Amazonka.Glue.Types.Spigot
import Amazonka.Glue.Types.SplitFields
import Amazonka.Glue.Types.Union
import qualified Amazonka.Prelude as Prelude

-- | @CodeGenConfigurationNode@ enumerates all valid Node types. One and only
-- one of its member variables can be populated.
--
-- /See:/ 'newCodeGenConfigurationNode' smart constructor.
data CodeGenConfigurationNode = CodeGenConfigurationNode'
  { -- | Specifies a transform that groups rows by chosen fields and computes the
    -- aggregated value by specified function.
    aggregate :: Prelude.Maybe Aggregate,
    -- | Specifies a transform that maps data property keys in the data source to
    -- data property keys in the data target. You can rename keys, modify the
    -- data types for keys, and choose which keys to drop from the dataset.
    applyMapping :: Prelude.Maybe ApplyMapping,
    -- | Specifies a connector to an Amazon Athena data source.
    athenaConnectorSource :: Prelude.Maybe AthenaConnectorSource,
    -- | Specifies an Apache Kafka data store in the Data Catalog.
    catalogKafkaSource :: Prelude.Maybe CatalogKafkaSource,
    -- | Specifies a Kinesis data source in the Glue Data Catalog.
    catalogKinesisSource :: Prelude.Maybe CatalogKinesisSource,
    -- | Specifies a data store in the Glue Data Catalog.
    catalogSource :: Prelude.Maybe CatalogSource,
    -- | Specifies a target that uses a Glue Data Catalog table.
    catalogTarget :: Prelude.Maybe BasicCatalogTarget,
    -- | Specifies a transform that uses custom code you provide to perform the
    -- data transformation. The output is a collection of DynamicFrames.
    customCode :: Prelude.Maybe CustomCode,
    -- | Specifies an Apache Kafka data store.
    directKafkaSource :: Prelude.Maybe DirectKafkaSource,
    -- | Specifies a direct Amazon Kinesis data source.
    directKinesisSource :: Prelude.Maybe DirectKinesisSource,
    -- | Specifies a transform that removes rows of repeating data from a data
    -- set.
    dropDuplicates :: Prelude.Maybe DropDuplicates,
    -- | Specifies a transform that chooses the data property keys that you want
    -- to drop.
    dropFields :: Prelude.Maybe DropFields,
    -- | Specifies a transform that removes columns from the dataset if all
    -- values in the column are \'null\'. By default, Glue Studio will
    -- recognize null objects, but some values such as empty strings, strings
    -- that are \"null\", -1 integers or other placeholders such as zeros, are
    -- not automatically recognized as nulls.
    dropNullFields :: Prelude.Maybe DropNullFields,
    -- | Specifies a custom visual transform created by a user.
    dynamicTransform :: Prelude.Maybe DynamicTransform,
    dynamoDBCatalogSource :: Prelude.Maybe DynamoDBCatalogSource,
    -- | Specifies your data quality evaluation criteria.
    evaluateDataQuality :: Prelude.Maybe EvaluateDataQuality,
    -- | Specifies a transform that locates records in the dataset that have
    -- missing values and adds a new field with a value determined by
    -- imputation. The input data set is used to train the machine learning
    -- model that determines what the missing value should be.
    fillMissingValues :: Prelude.Maybe FillMissingValues,
    -- | Specifies a transform that splits a dataset into two, based on a filter
    -- condition.
    filter' :: Prelude.Maybe Filter,
    -- | Specifies a data source in a goverened Data Catalog.
    governedCatalogSource :: Prelude.Maybe GovernedCatalogSource,
    -- | Specifies a data target that writes to a goverened catalog.
    governedCatalogTarget :: Prelude.Maybe GovernedCatalogTarget,
    -- | Specifies a connector to a JDBC data source.
    jDBCConnectorSource :: Prelude.Maybe JDBCConnectorSource,
    -- | Specifies a data target that writes to Amazon S3 in Apache Parquet
    -- columnar storage.
    jDBCConnectorTarget :: Prelude.Maybe JDBCConnectorTarget,
    -- | Specifies a transform that joins two datasets into one dataset using a
    -- comparison phrase on the specified data property keys. You can use
    -- inner, outer, left, right, left semi, and left anti joins.
    join :: Prelude.Maybe Join,
    -- | Specifies a transform that merges a @DynamicFrame@ with a staging
    -- @DynamicFrame@ based on the specified primary keys to identify records.
    -- Duplicate records (records with the same primary keys) are not
    -- de-duplicated.
    merge :: Prelude.Maybe Merge,
    -- | Specifies a Microsoft SQL server data source in the Glue Data Catalog.
    microsoftSQLServerCatalogSource :: Prelude.Maybe MicrosoftSQLServerCatalogSource,
    -- | Specifies a target that uses Microsoft SQL.
    microsoftSQLServerCatalogTarget :: Prelude.Maybe MicrosoftSQLServerCatalogTarget,
    -- | Specifies a MySQL data source in the Glue Data Catalog.
    mySQLCatalogSource :: Prelude.Maybe MySQLCatalogSource,
    -- | Specifies a target that uses MySQL.
    mySQLCatalogTarget :: Prelude.Maybe MySQLCatalogTarget,
    -- | Specifies an Oracle data source in the Glue Data Catalog.
    oracleSQLCatalogSource :: Prelude.Maybe OracleSQLCatalogSource,
    -- | Specifies a target that uses Oracle SQL.
    oracleSQLCatalogTarget :: Prelude.Maybe OracleSQLCatalogTarget,
    -- | Specifies a transform that identifies, removes or masks PII data.
    pIIDetection :: Prelude.Maybe PIIDetection,
    -- | Specifies a PostgresSQL data source in the Glue Data Catalog.
    postgreSQLCatalogSource :: Prelude.Maybe PostgreSQLCatalogSource,
    -- | Specifies a target that uses Postgres SQL.
    postgreSQLCatalogTarget :: Prelude.Maybe PostgreSQLCatalogTarget,
    -- | Specifies an Amazon Redshift data store.
    redshiftSource :: Prelude.Maybe RedshiftSource,
    -- | Specifies a target that uses Amazon Redshift.
    redshiftTarget :: Prelude.Maybe RedshiftTarget,
    relationalCatalogSource :: Prelude.Maybe RelationalCatalogSource,
    -- | Specifies a transform that renames a single data property key.
    renameField :: Prelude.Maybe RenameField,
    -- | Specifies an Amazon S3 data store in the Glue Data Catalog.
    s3CatalogSource :: Prelude.Maybe S3CatalogSource,
    -- | Specifies a data target that writes to Amazon S3 using the Glue Data
    -- Catalog.
    s3CatalogTarget :: Prelude.Maybe S3CatalogTarget,
    -- | Specifies a command-separated value (CSV) data store stored in Amazon
    -- S3.
    s3CsvSource :: Prelude.Maybe S3CsvSource,
    -- | Specifies a data target that writes to Amazon S3.
    s3DirectTarget :: Prelude.Maybe S3DirectTarget,
    -- | Specifies a data target that writes to Amazon S3 in Apache Parquet
    -- columnar storage.
    s3GlueParquetTarget :: Prelude.Maybe S3GlueParquetTarget,
    -- | Specifies a JSON data store stored in Amazon S3.
    s3JsonSource :: Prelude.Maybe S3JsonSource,
    -- | Specifies an Apache Parquet data store stored in Amazon S3.
    s3ParquetSource :: Prelude.Maybe S3ParquetSource,
    -- | Specifies a transform that chooses the data property keys that you want
    -- to keep.
    selectFields :: Prelude.Maybe SelectFields,
    -- | Specifies a transform that chooses one @DynamicFrame@ from a collection
    -- of @DynamicFrames@. The output is the selected @DynamicFrame@
    selectFromCollection :: Prelude.Maybe SelectFromCollection,
    -- | Specifies a connector to an Apache Spark data source.
    sparkConnectorSource :: Prelude.Maybe SparkConnectorSource,
    -- | Specifies a target that uses an Apache Spark connector.
    sparkConnectorTarget :: Prelude.Maybe SparkConnectorTarget,
    -- | Specifies a transform where you enter a SQL query using Spark SQL syntax
    -- to transform the data. The output is a single @DynamicFrame@.
    sparkSQL :: Prelude.Maybe SparkSQL,
    -- | Specifies a transform that writes samples of the data to an Amazon S3
    -- bucket.
    spigot :: Prelude.Maybe Spigot,
    -- | Specifies a transform that splits data property keys into two
    -- @DynamicFrames@. The output is a collection of @DynamicFrames@: one with
    -- selected data property keys, and one with the remaining data property
    -- keys.
    splitFields :: Prelude.Maybe SplitFields,
    -- | Specifies a transform that combines the rows from two or more datasets
    -- into a single result.
    union :: Prelude.Maybe Union
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CodeGenConfigurationNode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aggregate', 'codeGenConfigurationNode_aggregate' - Specifies a transform that groups rows by chosen fields and computes the
-- aggregated value by specified function.
--
-- 'applyMapping', 'codeGenConfigurationNode_applyMapping' - Specifies a transform that maps data property keys in the data source to
-- data property keys in the data target. You can rename keys, modify the
-- data types for keys, and choose which keys to drop from the dataset.
--
-- 'athenaConnectorSource', 'codeGenConfigurationNode_athenaConnectorSource' - Specifies a connector to an Amazon Athena data source.
--
-- 'catalogKafkaSource', 'codeGenConfigurationNode_catalogKafkaSource' - Specifies an Apache Kafka data store in the Data Catalog.
--
-- 'catalogKinesisSource', 'codeGenConfigurationNode_catalogKinesisSource' - Specifies a Kinesis data source in the Glue Data Catalog.
--
-- 'catalogSource', 'codeGenConfigurationNode_catalogSource' - Specifies a data store in the Glue Data Catalog.
--
-- 'catalogTarget', 'codeGenConfigurationNode_catalogTarget' - Specifies a target that uses a Glue Data Catalog table.
--
-- 'customCode', 'codeGenConfigurationNode_customCode' - Specifies a transform that uses custom code you provide to perform the
-- data transformation. The output is a collection of DynamicFrames.
--
-- 'directKafkaSource', 'codeGenConfigurationNode_directKafkaSource' - Specifies an Apache Kafka data store.
--
-- 'directKinesisSource', 'codeGenConfigurationNode_directKinesisSource' - Specifies a direct Amazon Kinesis data source.
--
-- 'dropDuplicates', 'codeGenConfigurationNode_dropDuplicates' - Specifies a transform that removes rows of repeating data from a data
-- set.
--
-- 'dropFields', 'codeGenConfigurationNode_dropFields' - Specifies a transform that chooses the data property keys that you want
-- to drop.
--
-- 'dropNullFields', 'codeGenConfigurationNode_dropNullFields' - Specifies a transform that removes columns from the dataset if all
-- values in the column are \'null\'. By default, Glue Studio will
-- recognize null objects, but some values such as empty strings, strings
-- that are \"null\", -1 integers or other placeholders such as zeros, are
-- not automatically recognized as nulls.
--
-- 'dynamicTransform', 'codeGenConfigurationNode_dynamicTransform' - Specifies a custom visual transform created by a user.
--
-- 'dynamoDBCatalogSource', 'codeGenConfigurationNode_dynamoDBCatalogSource' - Undocumented member.
--
-- 'evaluateDataQuality', 'codeGenConfigurationNode_evaluateDataQuality' - Specifies your data quality evaluation criteria.
--
-- 'fillMissingValues', 'codeGenConfigurationNode_fillMissingValues' - Specifies a transform that locates records in the dataset that have
-- missing values and adds a new field with a value determined by
-- imputation. The input data set is used to train the machine learning
-- model that determines what the missing value should be.
--
-- 'filter'', 'codeGenConfigurationNode_filter' - Specifies a transform that splits a dataset into two, based on a filter
-- condition.
--
-- 'governedCatalogSource', 'codeGenConfigurationNode_governedCatalogSource' - Specifies a data source in a goverened Data Catalog.
--
-- 'governedCatalogTarget', 'codeGenConfigurationNode_governedCatalogTarget' - Specifies a data target that writes to a goverened catalog.
--
-- 'jDBCConnectorSource', 'codeGenConfigurationNode_jDBCConnectorSource' - Specifies a connector to a JDBC data source.
--
-- 'jDBCConnectorTarget', 'codeGenConfigurationNode_jDBCConnectorTarget' - Specifies a data target that writes to Amazon S3 in Apache Parquet
-- columnar storage.
--
-- 'join', 'codeGenConfigurationNode_join' - Specifies a transform that joins two datasets into one dataset using a
-- comparison phrase on the specified data property keys. You can use
-- inner, outer, left, right, left semi, and left anti joins.
--
-- 'merge', 'codeGenConfigurationNode_merge' - Specifies a transform that merges a @DynamicFrame@ with a staging
-- @DynamicFrame@ based on the specified primary keys to identify records.
-- Duplicate records (records with the same primary keys) are not
-- de-duplicated.
--
-- 'microsoftSQLServerCatalogSource', 'codeGenConfigurationNode_microsoftSQLServerCatalogSource' - Specifies a Microsoft SQL server data source in the Glue Data Catalog.
--
-- 'microsoftSQLServerCatalogTarget', 'codeGenConfigurationNode_microsoftSQLServerCatalogTarget' - Specifies a target that uses Microsoft SQL.
--
-- 'mySQLCatalogSource', 'codeGenConfigurationNode_mySQLCatalogSource' - Specifies a MySQL data source in the Glue Data Catalog.
--
-- 'mySQLCatalogTarget', 'codeGenConfigurationNode_mySQLCatalogTarget' - Specifies a target that uses MySQL.
--
-- 'oracleSQLCatalogSource', 'codeGenConfigurationNode_oracleSQLCatalogSource' - Specifies an Oracle data source in the Glue Data Catalog.
--
-- 'oracleSQLCatalogTarget', 'codeGenConfigurationNode_oracleSQLCatalogTarget' - Specifies a target that uses Oracle SQL.
--
-- 'pIIDetection', 'codeGenConfigurationNode_pIIDetection' - Specifies a transform that identifies, removes or masks PII data.
--
-- 'postgreSQLCatalogSource', 'codeGenConfigurationNode_postgreSQLCatalogSource' - Specifies a PostgresSQL data source in the Glue Data Catalog.
--
-- 'postgreSQLCatalogTarget', 'codeGenConfigurationNode_postgreSQLCatalogTarget' - Specifies a target that uses Postgres SQL.
--
-- 'redshiftSource', 'codeGenConfigurationNode_redshiftSource' - Specifies an Amazon Redshift data store.
--
-- 'redshiftTarget', 'codeGenConfigurationNode_redshiftTarget' - Specifies a target that uses Amazon Redshift.
--
-- 'relationalCatalogSource', 'codeGenConfigurationNode_relationalCatalogSource' - Undocumented member.
--
-- 'renameField', 'codeGenConfigurationNode_renameField' - Specifies a transform that renames a single data property key.
--
-- 's3CatalogSource', 'codeGenConfigurationNode_s3CatalogSource' - Specifies an Amazon S3 data store in the Glue Data Catalog.
--
-- 's3CatalogTarget', 'codeGenConfigurationNode_s3CatalogTarget' - Specifies a data target that writes to Amazon S3 using the Glue Data
-- Catalog.
--
-- 's3CsvSource', 'codeGenConfigurationNode_s3CsvSource' - Specifies a command-separated value (CSV) data store stored in Amazon
-- S3.
--
-- 's3DirectTarget', 'codeGenConfigurationNode_s3DirectTarget' - Specifies a data target that writes to Amazon S3.
--
-- 's3GlueParquetTarget', 'codeGenConfigurationNode_s3GlueParquetTarget' - Specifies a data target that writes to Amazon S3 in Apache Parquet
-- columnar storage.
--
-- 's3JsonSource', 'codeGenConfigurationNode_s3JsonSource' - Specifies a JSON data store stored in Amazon S3.
--
-- 's3ParquetSource', 'codeGenConfigurationNode_s3ParquetSource' - Specifies an Apache Parquet data store stored in Amazon S3.
--
-- 'selectFields', 'codeGenConfigurationNode_selectFields' - Specifies a transform that chooses the data property keys that you want
-- to keep.
--
-- 'selectFromCollection', 'codeGenConfigurationNode_selectFromCollection' - Specifies a transform that chooses one @DynamicFrame@ from a collection
-- of @DynamicFrames@. The output is the selected @DynamicFrame@
--
-- 'sparkConnectorSource', 'codeGenConfigurationNode_sparkConnectorSource' - Specifies a connector to an Apache Spark data source.
--
-- 'sparkConnectorTarget', 'codeGenConfigurationNode_sparkConnectorTarget' - Specifies a target that uses an Apache Spark connector.
--
-- 'sparkSQL', 'codeGenConfigurationNode_sparkSQL' - Specifies a transform where you enter a SQL query using Spark SQL syntax
-- to transform the data. The output is a single @DynamicFrame@.
--
-- 'spigot', 'codeGenConfigurationNode_spigot' - Specifies a transform that writes samples of the data to an Amazon S3
-- bucket.
--
-- 'splitFields', 'codeGenConfigurationNode_splitFields' - Specifies a transform that splits data property keys into two
-- @DynamicFrames@. The output is a collection of @DynamicFrames@: one with
-- selected data property keys, and one with the remaining data property
-- keys.
--
-- 'union', 'codeGenConfigurationNode_union' - Specifies a transform that combines the rows from two or more datasets
-- into a single result.
newCodeGenConfigurationNode ::
  CodeGenConfigurationNode
newCodeGenConfigurationNode =
  CodeGenConfigurationNode'
    { aggregate =
        Prelude.Nothing,
      applyMapping = Prelude.Nothing,
      athenaConnectorSource = Prelude.Nothing,
      catalogKafkaSource = Prelude.Nothing,
      catalogKinesisSource = Prelude.Nothing,
      catalogSource = Prelude.Nothing,
      catalogTarget = Prelude.Nothing,
      customCode = Prelude.Nothing,
      directKafkaSource = Prelude.Nothing,
      directKinesisSource = Prelude.Nothing,
      dropDuplicates = Prelude.Nothing,
      dropFields = Prelude.Nothing,
      dropNullFields = Prelude.Nothing,
      dynamicTransform = Prelude.Nothing,
      dynamoDBCatalogSource = Prelude.Nothing,
      evaluateDataQuality = Prelude.Nothing,
      fillMissingValues = Prelude.Nothing,
      filter' = Prelude.Nothing,
      governedCatalogSource = Prelude.Nothing,
      governedCatalogTarget = Prelude.Nothing,
      jDBCConnectorSource = Prelude.Nothing,
      jDBCConnectorTarget = Prelude.Nothing,
      join = Prelude.Nothing,
      merge = Prelude.Nothing,
      microsoftSQLServerCatalogSource = Prelude.Nothing,
      microsoftSQLServerCatalogTarget = Prelude.Nothing,
      mySQLCatalogSource = Prelude.Nothing,
      mySQLCatalogTarget = Prelude.Nothing,
      oracleSQLCatalogSource = Prelude.Nothing,
      oracleSQLCatalogTarget = Prelude.Nothing,
      pIIDetection = Prelude.Nothing,
      postgreSQLCatalogSource = Prelude.Nothing,
      postgreSQLCatalogTarget = Prelude.Nothing,
      redshiftSource = Prelude.Nothing,
      redshiftTarget = Prelude.Nothing,
      relationalCatalogSource = Prelude.Nothing,
      renameField = Prelude.Nothing,
      s3CatalogSource = Prelude.Nothing,
      s3CatalogTarget = Prelude.Nothing,
      s3CsvSource = Prelude.Nothing,
      s3DirectTarget = Prelude.Nothing,
      s3GlueParquetTarget = Prelude.Nothing,
      s3JsonSource = Prelude.Nothing,
      s3ParquetSource = Prelude.Nothing,
      selectFields = Prelude.Nothing,
      selectFromCollection = Prelude.Nothing,
      sparkConnectorSource = Prelude.Nothing,
      sparkConnectorTarget = Prelude.Nothing,
      sparkSQL = Prelude.Nothing,
      spigot = Prelude.Nothing,
      splitFields = Prelude.Nothing,
      union = Prelude.Nothing
    }

-- | Specifies a transform that groups rows by chosen fields and computes the
-- aggregated value by specified function.
codeGenConfigurationNode_aggregate :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe Aggregate)
codeGenConfigurationNode_aggregate = Lens.lens (\CodeGenConfigurationNode' {aggregate} -> aggregate) (\s@CodeGenConfigurationNode' {} a -> s {aggregate = a} :: CodeGenConfigurationNode)

-- | Specifies a transform that maps data property keys in the data source to
-- data property keys in the data target. You can rename keys, modify the
-- data types for keys, and choose which keys to drop from the dataset.
codeGenConfigurationNode_applyMapping :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe ApplyMapping)
codeGenConfigurationNode_applyMapping = Lens.lens (\CodeGenConfigurationNode' {applyMapping} -> applyMapping) (\s@CodeGenConfigurationNode' {} a -> s {applyMapping = a} :: CodeGenConfigurationNode)

-- | Specifies a connector to an Amazon Athena data source.
codeGenConfigurationNode_athenaConnectorSource :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe AthenaConnectorSource)
codeGenConfigurationNode_athenaConnectorSource = Lens.lens (\CodeGenConfigurationNode' {athenaConnectorSource} -> athenaConnectorSource) (\s@CodeGenConfigurationNode' {} a -> s {athenaConnectorSource = a} :: CodeGenConfigurationNode)

-- | Specifies an Apache Kafka data store in the Data Catalog.
codeGenConfigurationNode_catalogKafkaSource :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe CatalogKafkaSource)
codeGenConfigurationNode_catalogKafkaSource = Lens.lens (\CodeGenConfigurationNode' {catalogKafkaSource} -> catalogKafkaSource) (\s@CodeGenConfigurationNode' {} a -> s {catalogKafkaSource = a} :: CodeGenConfigurationNode)

-- | Specifies a Kinesis data source in the Glue Data Catalog.
codeGenConfigurationNode_catalogKinesisSource :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe CatalogKinesisSource)
codeGenConfigurationNode_catalogKinesisSource = Lens.lens (\CodeGenConfigurationNode' {catalogKinesisSource} -> catalogKinesisSource) (\s@CodeGenConfigurationNode' {} a -> s {catalogKinesisSource = a} :: CodeGenConfigurationNode)

-- | Specifies a data store in the Glue Data Catalog.
codeGenConfigurationNode_catalogSource :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe CatalogSource)
codeGenConfigurationNode_catalogSource = Lens.lens (\CodeGenConfigurationNode' {catalogSource} -> catalogSource) (\s@CodeGenConfigurationNode' {} a -> s {catalogSource = a} :: CodeGenConfigurationNode)

-- | Specifies a target that uses a Glue Data Catalog table.
codeGenConfigurationNode_catalogTarget :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe BasicCatalogTarget)
codeGenConfigurationNode_catalogTarget = Lens.lens (\CodeGenConfigurationNode' {catalogTarget} -> catalogTarget) (\s@CodeGenConfigurationNode' {} a -> s {catalogTarget = a} :: CodeGenConfigurationNode)

-- | Specifies a transform that uses custom code you provide to perform the
-- data transformation. The output is a collection of DynamicFrames.
codeGenConfigurationNode_customCode :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe CustomCode)
codeGenConfigurationNode_customCode = Lens.lens (\CodeGenConfigurationNode' {customCode} -> customCode) (\s@CodeGenConfigurationNode' {} a -> s {customCode = a} :: CodeGenConfigurationNode)

-- | Specifies an Apache Kafka data store.
codeGenConfigurationNode_directKafkaSource :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe DirectKafkaSource)
codeGenConfigurationNode_directKafkaSource = Lens.lens (\CodeGenConfigurationNode' {directKafkaSource} -> directKafkaSource) (\s@CodeGenConfigurationNode' {} a -> s {directKafkaSource = a} :: CodeGenConfigurationNode)

-- | Specifies a direct Amazon Kinesis data source.
codeGenConfigurationNode_directKinesisSource :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe DirectKinesisSource)
codeGenConfigurationNode_directKinesisSource = Lens.lens (\CodeGenConfigurationNode' {directKinesisSource} -> directKinesisSource) (\s@CodeGenConfigurationNode' {} a -> s {directKinesisSource = a} :: CodeGenConfigurationNode)

-- | Specifies a transform that removes rows of repeating data from a data
-- set.
codeGenConfigurationNode_dropDuplicates :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe DropDuplicates)
codeGenConfigurationNode_dropDuplicates = Lens.lens (\CodeGenConfigurationNode' {dropDuplicates} -> dropDuplicates) (\s@CodeGenConfigurationNode' {} a -> s {dropDuplicates = a} :: CodeGenConfigurationNode)

-- | Specifies a transform that chooses the data property keys that you want
-- to drop.
codeGenConfigurationNode_dropFields :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe DropFields)
codeGenConfigurationNode_dropFields = Lens.lens (\CodeGenConfigurationNode' {dropFields} -> dropFields) (\s@CodeGenConfigurationNode' {} a -> s {dropFields = a} :: CodeGenConfigurationNode)

-- | Specifies a transform that removes columns from the dataset if all
-- values in the column are \'null\'. By default, Glue Studio will
-- recognize null objects, but some values such as empty strings, strings
-- that are \"null\", -1 integers or other placeholders such as zeros, are
-- not automatically recognized as nulls.
codeGenConfigurationNode_dropNullFields :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe DropNullFields)
codeGenConfigurationNode_dropNullFields = Lens.lens (\CodeGenConfigurationNode' {dropNullFields} -> dropNullFields) (\s@CodeGenConfigurationNode' {} a -> s {dropNullFields = a} :: CodeGenConfigurationNode)

-- | Specifies a custom visual transform created by a user.
codeGenConfigurationNode_dynamicTransform :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe DynamicTransform)
codeGenConfigurationNode_dynamicTransform = Lens.lens (\CodeGenConfigurationNode' {dynamicTransform} -> dynamicTransform) (\s@CodeGenConfigurationNode' {} a -> s {dynamicTransform = a} :: CodeGenConfigurationNode)

-- | Undocumented member.
codeGenConfigurationNode_dynamoDBCatalogSource :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe DynamoDBCatalogSource)
codeGenConfigurationNode_dynamoDBCatalogSource = Lens.lens (\CodeGenConfigurationNode' {dynamoDBCatalogSource} -> dynamoDBCatalogSource) (\s@CodeGenConfigurationNode' {} a -> s {dynamoDBCatalogSource = a} :: CodeGenConfigurationNode)

-- | Specifies your data quality evaluation criteria.
codeGenConfigurationNode_evaluateDataQuality :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe EvaluateDataQuality)
codeGenConfigurationNode_evaluateDataQuality = Lens.lens (\CodeGenConfigurationNode' {evaluateDataQuality} -> evaluateDataQuality) (\s@CodeGenConfigurationNode' {} a -> s {evaluateDataQuality = a} :: CodeGenConfigurationNode)

-- | Specifies a transform that locates records in the dataset that have
-- missing values and adds a new field with a value determined by
-- imputation. The input data set is used to train the machine learning
-- model that determines what the missing value should be.
codeGenConfigurationNode_fillMissingValues :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe FillMissingValues)
codeGenConfigurationNode_fillMissingValues = Lens.lens (\CodeGenConfigurationNode' {fillMissingValues} -> fillMissingValues) (\s@CodeGenConfigurationNode' {} a -> s {fillMissingValues = a} :: CodeGenConfigurationNode)

-- | Specifies a transform that splits a dataset into two, based on a filter
-- condition.
codeGenConfigurationNode_filter :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe Filter)
codeGenConfigurationNode_filter = Lens.lens (\CodeGenConfigurationNode' {filter'} -> filter') (\s@CodeGenConfigurationNode' {} a -> s {filter' = a} :: CodeGenConfigurationNode)

-- | Specifies a data source in a goverened Data Catalog.
codeGenConfigurationNode_governedCatalogSource :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe GovernedCatalogSource)
codeGenConfigurationNode_governedCatalogSource = Lens.lens (\CodeGenConfigurationNode' {governedCatalogSource} -> governedCatalogSource) (\s@CodeGenConfigurationNode' {} a -> s {governedCatalogSource = a} :: CodeGenConfigurationNode)

-- | Specifies a data target that writes to a goverened catalog.
codeGenConfigurationNode_governedCatalogTarget :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe GovernedCatalogTarget)
codeGenConfigurationNode_governedCatalogTarget = Lens.lens (\CodeGenConfigurationNode' {governedCatalogTarget} -> governedCatalogTarget) (\s@CodeGenConfigurationNode' {} a -> s {governedCatalogTarget = a} :: CodeGenConfigurationNode)

-- | Specifies a connector to a JDBC data source.
codeGenConfigurationNode_jDBCConnectorSource :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe JDBCConnectorSource)
codeGenConfigurationNode_jDBCConnectorSource = Lens.lens (\CodeGenConfigurationNode' {jDBCConnectorSource} -> jDBCConnectorSource) (\s@CodeGenConfigurationNode' {} a -> s {jDBCConnectorSource = a} :: CodeGenConfigurationNode)

-- | Specifies a data target that writes to Amazon S3 in Apache Parquet
-- columnar storage.
codeGenConfigurationNode_jDBCConnectorTarget :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe JDBCConnectorTarget)
codeGenConfigurationNode_jDBCConnectorTarget = Lens.lens (\CodeGenConfigurationNode' {jDBCConnectorTarget} -> jDBCConnectorTarget) (\s@CodeGenConfigurationNode' {} a -> s {jDBCConnectorTarget = a} :: CodeGenConfigurationNode)

-- | Specifies a transform that joins two datasets into one dataset using a
-- comparison phrase on the specified data property keys. You can use
-- inner, outer, left, right, left semi, and left anti joins.
codeGenConfigurationNode_join :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe Join)
codeGenConfigurationNode_join = Lens.lens (\CodeGenConfigurationNode' {join} -> join) (\s@CodeGenConfigurationNode' {} a -> s {join = a} :: CodeGenConfigurationNode)

-- | Specifies a transform that merges a @DynamicFrame@ with a staging
-- @DynamicFrame@ based on the specified primary keys to identify records.
-- Duplicate records (records with the same primary keys) are not
-- de-duplicated.
codeGenConfigurationNode_merge :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe Merge)
codeGenConfigurationNode_merge = Lens.lens (\CodeGenConfigurationNode' {merge} -> merge) (\s@CodeGenConfigurationNode' {} a -> s {merge = a} :: CodeGenConfigurationNode)

-- | Specifies a Microsoft SQL server data source in the Glue Data Catalog.
codeGenConfigurationNode_microsoftSQLServerCatalogSource :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe MicrosoftSQLServerCatalogSource)
codeGenConfigurationNode_microsoftSQLServerCatalogSource = Lens.lens (\CodeGenConfigurationNode' {microsoftSQLServerCatalogSource} -> microsoftSQLServerCatalogSource) (\s@CodeGenConfigurationNode' {} a -> s {microsoftSQLServerCatalogSource = a} :: CodeGenConfigurationNode)

-- | Specifies a target that uses Microsoft SQL.
codeGenConfigurationNode_microsoftSQLServerCatalogTarget :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe MicrosoftSQLServerCatalogTarget)
codeGenConfigurationNode_microsoftSQLServerCatalogTarget = Lens.lens (\CodeGenConfigurationNode' {microsoftSQLServerCatalogTarget} -> microsoftSQLServerCatalogTarget) (\s@CodeGenConfigurationNode' {} a -> s {microsoftSQLServerCatalogTarget = a} :: CodeGenConfigurationNode)

-- | Specifies a MySQL data source in the Glue Data Catalog.
codeGenConfigurationNode_mySQLCatalogSource :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe MySQLCatalogSource)
codeGenConfigurationNode_mySQLCatalogSource = Lens.lens (\CodeGenConfigurationNode' {mySQLCatalogSource} -> mySQLCatalogSource) (\s@CodeGenConfigurationNode' {} a -> s {mySQLCatalogSource = a} :: CodeGenConfigurationNode)

-- | Specifies a target that uses MySQL.
codeGenConfigurationNode_mySQLCatalogTarget :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe MySQLCatalogTarget)
codeGenConfigurationNode_mySQLCatalogTarget = Lens.lens (\CodeGenConfigurationNode' {mySQLCatalogTarget} -> mySQLCatalogTarget) (\s@CodeGenConfigurationNode' {} a -> s {mySQLCatalogTarget = a} :: CodeGenConfigurationNode)

-- | Specifies an Oracle data source in the Glue Data Catalog.
codeGenConfigurationNode_oracleSQLCatalogSource :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe OracleSQLCatalogSource)
codeGenConfigurationNode_oracleSQLCatalogSource = Lens.lens (\CodeGenConfigurationNode' {oracleSQLCatalogSource} -> oracleSQLCatalogSource) (\s@CodeGenConfigurationNode' {} a -> s {oracleSQLCatalogSource = a} :: CodeGenConfigurationNode)

-- | Specifies a target that uses Oracle SQL.
codeGenConfigurationNode_oracleSQLCatalogTarget :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe OracleSQLCatalogTarget)
codeGenConfigurationNode_oracleSQLCatalogTarget = Lens.lens (\CodeGenConfigurationNode' {oracleSQLCatalogTarget} -> oracleSQLCatalogTarget) (\s@CodeGenConfigurationNode' {} a -> s {oracleSQLCatalogTarget = a} :: CodeGenConfigurationNode)

-- | Specifies a transform that identifies, removes or masks PII data.
codeGenConfigurationNode_pIIDetection :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe PIIDetection)
codeGenConfigurationNode_pIIDetection = Lens.lens (\CodeGenConfigurationNode' {pIIDetection} -> pIIDetection) (\s@CodeGenConfigurationNode' {} a -> s {pIIDetection = a} :: CodeGenConfigurationNode)

-- | Specifies a PostgresSQL data source in the Glue Data Catalog.
codeGenConfigurationNode_postgreSQLCatalogSource :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe PostgreSQLCatalogSource)
codeGenConfigurationNode_postgreSQLCatalogSource = Lens.lens (\CodeGenConfigurationNode' {postgreSQLCatalogSource} -> postgreSQLCatalogSource) (\s@CodeGenConfigurationNode' {} a -> s {postgreSQLCatalogSource = a} :: CodeGenConfigurationNode)

-- | Specifies a target that uses Postgres SQL.
codeGenConfigurationNode_postgreSQLCatalogTarget :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe PostgreSQLCatalogTarget)
codeGenConfigurationNode_postgreSQLCatalogTarget = Lens.lens (\CodeGenConfigurationNode' {postgreSQLCatalogTarget} -> postgreSQLCatalogTarget) (\s@CodeGenConfigurationNode' {} a -> s {postgreSQLCatalogTarget = a} :: CodeGenConfigurationNode)

-- | Specifies an Amazon Redshift data store.
codeGenConfigurationNode_redshiftSource :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe RedshiftSource)
codeGenConfigurationNode_redshiftSource = Lens.lens (\CodeGenConfigurationNode' {redshiftSource} -> redshiftSource) (\s@CodeGenConfigurationNode' {} a -> s {redshiftSource = a} :: CodeGenConfigurationNode)

-- | Specifies a target that uses Amazon Redshift.
codeGenConfigurationNode_redshiftTarget :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe RedshiftTarget)
codeGenConfigurationNode_redshiftTarget = Lens.lens (\CodeGenConfigurationNode' {redshiftTarget} -> redshiftTarget) (\s@CodeGenConfigurationNode' {} a -> s {redshiftTarget = a} :: CodeGenConfigurationNode)

-- | Undocumented member.
codeGenConfigurationNode_relationalCatalogSource :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe RelationalCatalogSource)
codeGenConfigurationNode_relationalCatalogSource = Lens.lens (\CodeGenConfigurationNode' {relationalCatalogSource} -> relationalCatalogSource) (\s@CodeGenConfigurationNode' {} a -> s {relationalCatalogSource = a} :: CodeGenConfigurationNode)

-- | Specifies a transform that renames a single data property key.
codeGenConfigurationNode_renameField :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe RenameField)
codeGenConfigurationNode_renameField = Lens.lens (\CodeGenConfigurationNode' {renameField} -> renameField) (\s@CodeGenConfigurationNode' {} a -> s {renameField = a} :: CodeGenConfigurationNode)

-- | Specifies an Amazon S3 data store in the Glue Data Catalog.
codeGenConfigurationNode_s3CatalogSource :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe S3CatalogSource)
codeGenConfigurationNode_s3CatalogSource = Lens.lens (\CodeGenConfigurationNode' {s3CatalogSource} -> s3CatalogSource) (\s@CodeGenConfigurationNode' {} a -> s {s3CatalogSource = a} :: CodeGenConfigurationNode)

-- | Specifies a data target that writes to Amazon S3 using the Glue Data
-- Catalog.
codeGenConfigurationNode_s3CatalogTarget :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe S3CatalogTarget)
codeGenConfigurationNode_s3CatalogTarget = Lens.lens (\CodeGenConfigurationNode' {s3CatalogTarget} -> s3CatalogTarget) (\s@CodeGenConfigurationNode' {} a -> s {s3CatalogTarget = a} :: CodeGenConfigurationNode)

-- | Specifies a command-separated value (CSV) data store stored in Amazon
-- S3.
codeGenConfigurationNode_s3CsvSource :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe S3CsvSource)
codeGenConfigurationNode_s3CsvSource = Lens.lens (\CodeGenConfigurationNode' {s3CsvSource} -> s3CsvSource) (\s@CodeGenConfigurationNode' {} a -> s {s3CsvSource = a} :: CodeGenConfigurationNode)

-- | Specifies a data target that writes to Amazon S3.
codeGenConfigurationNode_s3DirectTarget :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe S3DirectTarget)
codeGenConfigurationNode_s3DirectTarget = Lens.lens (\CodeGenConfigurationNode' {s3DirectTarget} -> s3DirectTarget) (\s@CodeGenConfigurationNode' {} a -> s {s3DirectTarget = a} :: CodeGenConfigurationNode)

-- | Specifies a data target that writes to Amazon S3 in Apache Parquet
-- columnar storage.
codeGenConfigurationNode_s3GlueParquetTarget :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe S3GlueParquetTarget)
codeGenConfigurationNode_s3GlueParquetTarget = Lens.lens (\CodeGenConfigurationNode' {s3GlueParquetTarget} -> s3GlueParquetTarget) (\s@CodeGenConfigurationNode' {} a -> s {s3GlueParquetTarget = a} :: CodeGenConfigurationNode)

-- | Specifies a JSON data store stored in Amazon S3.
codeGenConfigurationNode_s3JsonSource :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe S3JsonSource)
codeGenConfigurationNode_s3JsonSource = Lens.lens (\CodeGenConfigurationNode' {s3JsonSource} -> s3JsonSource) (\s@CodeGenConfigurationNode' {} a -> s {s3JsonSource = a} :: CodeGenConfigurationNode)

-- | Specifies an Apache Parquet data store stored in Amazon S3.
codeGenConfigurationNode_s3ParquetSource :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe S3ParquetSource)
codeGenConfigurationNode_s3ParquetSource = Lens.lens (\CodeGenConfigurationNode' {s3ParquetSource} -> s3ParquetSource) (\s@CodeGenConfigurationNode' {} a -> s {s3ParquetSource = a} :: CodeGenConfigurationNode)

-- | Specifies a transform that chooses the data property keys that you want
-- to keep.
codeGenConfigurationNode_selectFields :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe SelectFields)
codeGenConfigurationNode_selectFields = Lens.lens (\CodeGenConfigurationNode' {selectFields} -> selectFields) (\s@CodeGenConfigurationNode' {} a -> s {selectFields = a} :: CodeGenConfigurationNode)

-- | Specifies a transform that chooses one @DynamicFrame@ from a collection
-- of @DynamicFrames@. The output is the selected @DynamicFrame@
codeGenConfigurationNode_selectFromCollection :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe SelectFromCollection)
codeGenConfigurationNode_selectFromCollection = Lens.lens (\CodeGenConfigurationNode' {selectFromCollection} -> selectFromCollection) (\s@CodeGenConfigurationNode' {} a -> s {selectFromCollection = a} :: CodeGenConfigurationNode)

-- | Specifies a connector to an Apache Spark data source.
codeGenConfigurationNode_sparkConnectorSource :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe SparkConnectorSource)
codeGenConfigurationNode_sparkConnectorSource = Lens.lens (\CodeGenConfigurationNode' {sparkConnectorSource} -> sparkConnectorSource) (\s@CodeGenConfigurationNode' {} a -> s {sparkConnectorSource = a} :: CodeGenConfigurationNode)

-- | Specifies a target that uses an Apache Spark connector.
codeGenConfigurationNode_sparkConnectorTarget :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe SparkConnectorTarget)
codeGenConfigurationNode_sparkConnectorTarget = Lens.lens (\CodeGenConfigurationNode' {sparkConnectorTarget} -> sparkConnectorTarget) (\s@CodeGenConfigurationNode' {} a -> s {sparkConnectorTarget = a} :: CodeGenConfigurationNode)

-- | Specifies a transform where you enter a SQL query using Spark SQL syntax
-- to transform the data. The output is a single @DynamicFrame@.
codeGenConfigurationNode_sparkSQL :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe SparkSQL)
codeGenConfigurationNode_sparkSQL = Lens.lens (\CodeGenConfigurationNode' {sparkSQL} -> sparkSQL) (\s@CodeGenConfigurationNode' {} a -> s {sparkSQL = a} :: CodeGenConfigurationNode)

-- | Specifies a transform that writes samples of the data to an Amazon S3
-- bucket.
codeGenConfigurationNode_spigot :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe Spigot)
codeGenConfigurationNode_spigot = Lens.lens (\CodeGenConfigurationNode' {spigot} -> spigot) (\s@CodeGenConfigurationNode' {} a -> s {spigot = a} :: CodeGenConfigurationNode)

-- | Specifies a transform that splits data property keys into two
-- @DynamicFrames@. The output is a collection of @DynamicFrames@: one with
-- selected data property keys, and one with the remaining data property
-- keys.
codeGenConfigurationNode_splitFields :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe SplitFields)
codeGenConfigurationNode_splitFields = Lens.lens (\CodeGenConfigurationNode' {splitFields} -> splitFields) (\s@CodeGenConfigurationNode' {} a -> s {splitFields = a} :: CodeGenConfigurationNode)

-- | Specifies a transform that combines the rows from two or more datasets
-- into a single result.
codeGenConfigurationNode_union :: Lens.Lens' CodeGenConfigurationNode (Prelude.Maybe Union)
codeGenConfigurationNode_union = Lens.lens (\CodeGenConfigurationNode' {union} -> union) (\s@CodeGenConfigurationNode' {} a -> s {union = a} :: CodeGenConfigurationNode)

instance Data.FromJSON CodeGenConfigurationNode where
  parseJSON =
    Data.withObject
      "CodeGenConfigurationNode"
      ( \x ->
          CodeGenConfigurationNode'
            Prelude.<$> (x Data..:? "Aggregate")
            Prelude.<*> (x Data..:? "ApplyMapping")
            Prelude.<*> (x Data..:? "AthenaConnectorSource")
            Prelude.<*> (x Data..:? "CatalogKafkaSource")
            Prelude.<*> (x Data..:? "CatalogKinesisSource")
            Prelude.<*> (x Data..:? "CatalogSource")
            Prelude.<*> (x Data..:? "CatalogTarget")
            Prelude.<*> (x Data..:? "CustomCode")
            Prelude.<*> (x Data..:? "DirectKafkaSource")
            Prelude.<*> (x Data..:? "DirectKinesisSource")
            Prelude.<*> (x Data..:? "DropDuplicates")
            Prelude.<*> (x Data..:? "DropFields")
            Prelude.<*> (x Data..:? "DropNullFields")
            Prelude.<*> (x Data..:? "DynamicTransform")
            Prelude.<*> (x Data..:? "DynamoDBCatalogSource")
            Prelude.<*> (x Data..:? "EvaluateDataQuality")
            Prelude.<*> (x Data..:? "FillMissingValues")
            Prelude.<*> (x Data..:? "Filter")
            Prelude.<*> (x Data..:? "GovernedCatalogSource")
            Prelude.<*> (x Data..:? "GovernedCatalogTarget")
            Prelude.<*> (x Data..:? "JDBCConnectorSource")
            Prelude.<*> (x Data..:? "JDBCConnectorTarget")
            Prelude.<*> (x Data..:? "Join")
            Prelude.<*> (x Data..:? "Merge")
            Prelude.<*> (x Data..:? "MicrosoftSQLServerCatalogSource")
            Prelude.<*> (x Data..:? "MicrosoftSQLServerCatalogTarget")
            Prelude.<*> (x Data..:? "MySQLCatalogSource")
            Prelude.<*> (x Data..:? "MySQLCatalogTarget")
            Prelude.<*> (x Data..:? "OracleSQLCatalogSource")
            Prelude.<*> (x Data..:? "OracleSQLCatalogTarget")
            Prelude.<*> (x Data..:? "PIIDetection")
            Prelude.<*> (x Data..:? "PostgreSQLCatalogSource")
            Prelude.<*> (x Data..:? "PostgreSQLCatalogTarget")
            Prelude.<*> (x Data..:? "RedshiftSource")
            Prelude.<*> (x Data..:? "RedshiftTarget")
            Prelude.<*> (x Data..:? "RelationalCatalogSource")
            Prelude.<*> (x Data..:? "RenameField")
            Prelude.<*> (x Data..:? "S3CatalogSource")
            Prelude.<*> (x Data..:? "S3CatalogTarget")
            Prelude.<*> (x Data..:? "S3CsvSource")
            Prelude.<*> (x Data..:? "S3DirectTarget")
            Prelude.<*> (x Data..:? "S3GlueParquetTarget")
            Prelude.<*> (x Data..:? "S3JsonSource")
            Prelude.<*> (x Data..:? "S3ParquetSource")
            Prelude.<*> (x Data..:? "SelectFields")
            Prelude.<*> (x Data..:? "SelectFromCollection")
            Prelude.<*> (x Data..:? "SparkConnectorSource")
            Prelude.<*> (x Data..:? "SparkConnectorTarget")
            Prelude.<*> (x Data..:? "SparkSQL")
            Prelude.<*> (x Data..:? "Spigot")
            Prelude.<*> (x Data..:? "SplitFields")
            Prelude.<*> (x Data..:? "Union")
      )

instance Prelude.Hashable CodeGenConfigurationNode where
  hashWithSalt _salt CodeGenConfigurationNode' {..} =
    _salt
      `Prelude.hashWithSalt` aggregate
      `Prelude.hashWithSalt` applyMapping
      `Prelude.hashWithSalt` athenaConnectorSource
      `Prelude.hashWithSalt` catalogKafkaSource
      `Prelude.hashWithSalt` catalogKinesisSource
      `Prelude.hashWithSalt` catalogSource
      `Prelude.hashWithSalt` catalogTarget
      `Prelude.hashWithSalt` customCode
      `Prelude.hashWithSalt` directKafkaSource
      `Prelude.hashWithSalt` directKinesisSource
      `Prelude.hashWithSalt` dropDuplicates
      `Prelude.hashWithSalt` dropFields
      `Prelude.hashWithSalt` dropNullFields
      `Prelude.hashWithSalt` dynamicTransform
      `Prelude.hashWithSalt` dynamoDBCatalogSource
      `Prelude.hashWithSalt` evaluateDataQuality
      `Prelude.hashWithSalt` fillMissingValues
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` governedCatalogSource
      `Prelude.hashWithSalt` governedCatalogTarget
      `Prelude.hashWithSalt` jDBCConnectorSource
      `Prelude.hashWithSalt` jDBCConnectorTarget
      `Prelude.hashWithSalt` join
      `Prelude.hashWithSalt` merge
      `Prelude.hashWithSalt` microsoftSQLServerCatalogSource
      `Prelude.hashWithSalt` microsoftSQLServerCatalogTarget
      `Prelude.hashWithSalt` mySQLCatalogSource
      `Prelude.hashWithSalt` mySQLCatalogTarget
      `Prelude.hashWithSalt` oracleSQLCatalogSource
      `Prelude.hashWithSalt` oracleSQLCatalogTarget
      `Prelude.hashWithSalt` pIIDetection
      `Prelude.hashWithSalt` postgreSQLCatalogSource
      `Prelude.hashWithSalt` postgreSQLCatalogTarget
      `Prelude.hashWithSalt` redshiftSource
      `Prelude.hashWithSalt` redshiftTarget
      `Prelude.hashWithSalt` relationalCatalogSource
      `Prelude.hashWithSalt` renameField
      `Prelude.hashWithSalt` s3CatalogSource
      `Prelude.hashWithSalt` s3CatalogTarget
      `Prelude.hashWithSalt` s3CsvSource
      `Prelude.hashWithSalt` s3DirectTarget
      `Prelude.hashWithSalt` s3GlueParquetTarget
      `Prelude.hashWithSalt` s3JsonSource
      `Prelude.hashWithSalt` s3ParquetSource
      `Prelude.hashWithSalt` selectFields
      `Prelude.hashWithSalt` selectFromCollection
      `Prelude.hashWithSalt` sparkConnectorSource
      `Prelude.hashWithSalt` sparkConnectorTarget
      `Prelude.hashWithSalt` sparkSQL
      `Prelude.hashWithSalt` spigot
      `Prelude.hashWithSalt` splitFields
      `Prelude.hashWithSalt` union

instance Prelude.NFData CodeGenConfigurationNode where
  rnf CodeGenConfigurationNode' {..} =
    Prelude.rnf aggregate
      `Prelude.seq` Prelude.rnf applyMapping
      `Prelude.seq` Prelude.rnf athenaConnectorSource
      `Prelude.seq` Prelude.rnf catalogKafkaSource
      `Prelude.seq` Prelude.rnf catalogKinesisSource
      `Prelude.seq` Prelude.rnf catalogSource
      `Prelude.seq` Prelude.rnf catalogTarget
      `Prelude.seq` Prelude.rnf customCode
      `Prelude.seq` Prelude.rnf directKafkaSource
      `Prelude.seq` Prelude.rnf directKinesisSource
      `Prelude.seq` Prelude.rnf dropDuplicates
      `Prelude.seq` Prelude.rnf dropFields
      `Prelude.seq` Prelude.rnf dropNullFields
      `Prelude.seq` Prelude.rnf dynamicTransform
      `Prelude.seq` Prelude.rnf dynamoDBCatalogSource
      `Prelude.seq` Prelude.rnf evaluateDataQuality
      `Prelude.seq` Prelude.rnf fillMissingValues
      `Prelude.seq` Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf
        governedCatalogSource
      `Prelude.seq` Prelude.rnf
        governedCatalogTarget
      `Prelude.seq` Prelude.rnf
        jDBCConnectorSource
      `Prelude.seq` Prelude.rnf
        jDBCConnectorTarget
      `Prelude.seq` Prelude.rnf join
      `Prelude.seq` Prelude.rnf merge
      `Prelude.seq` Prelude.rnf
        microsoftSQLServerCatalogSource
      `Prelude.seq` Prelude.rnf
        microsoftSQLServerCatalogTarget
      `Prelude.seq` Prelude.rnf
        mySQLCatalogSource
      `Prelude.seq` Prelude.rnf
        mySQLCatalogTarget
      `Prelude.seq` Prelude.rnf
        oracleSQLCatalogSource
      `Prelude.seq` Prelude.rnf
        oracleSQLCatalogTarget
      `Prelude.seq` Prelude.rnf
        pIIDetection
      `Prelude.seq` Prelude.rnf
        postgreSQLCatalogSource
      `Prelude.seq` Prelude.rnf
        postgreSQLCatalogTarget
      `Prelude.seq` Prelude.rnf
        redshiftSource
      `Prelude.seq` Prelude.rnf
        redshiftTarget
      `Prelude.seq` Prelude.rnf
        relationalCatalogSource
      `Prelude.seq` Prelude.rnf
        renameField
      `Prelude.seq` Prelude.rnf
        s3CatalogSource
      `Prelude.seq` Prelude.rnf
        s3CatalogTarget
      `Prelude.seq` Prelude.rnf
        s3CsvSource
      `Prelude.seq` Prelude.rnf
        s3DirectTarget
      `Prelude.seq` Prelude.rnf
        s3GlueParquetTarget
      `Prelude.seq` Prelude.rnf
        s3JsonSource
      `Prelude.seq` Prelude.rnf
        s3ParquetSource
      `Prelude.seq` Prelude.rnf
        selectFields
      `Prelude.seq` Prelude.rnf
        selectFromCollection
      `Prelude.seq` Prelude.rnf
        sparkConnectorSource
      `Prelude.seq` Prelude.rnf
        sparkConnectorTarget
      `Prelude.seq` Prelude.rnf
        sparkSQL
      `Prelude.seq` Prelude.rnf
        spigot
      `Prelude.seq` Prelude.rnf
        splitFields
      `Prelude.seq` Prelude.rnf
        union

instance Data.ToJSON CodeGenConfigurationNode where
  toJSON CodeGenConfigurationNode' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Aggregate" Data..=) Prelude.<$> aggregate,
            ("ApplyMapping" Data..=) Prelude.<$> applyMapping,
            ("AthenaConnectorSource" Data..=)
              Prelude.<$> athenaConnectorSource,
            ("CatalogKafkaSource" Data..=)
              Prelude.<$> catalogKafkaSource,
            ("CatalogKinesisSource" Data..=)
              Prelude.<$> catalogKinesisSource,
            ("CatalogSource" Data..=) Prelude.<$> catalogSource,
            ("CatalogTarget" Data..=) Prelude.<$> catalogTarget,
            ("CustomCode" Data..=) Prelude.<$> customCode,
            ("DirectKafkaSource" Data..=)
              Prelude.<$> directKafkaSource,
            ("DirectKinesisSource" Data..=)
              Prelude.<$> directKinesisSource,
            ("DropDuplicates" Data..=)
              Prelude.<$> dropDuplicates,
            ("DropFields" Data..=) Prelude.<$> dropFields,
            ("DropNullFields" Data..=)
              Prelude.<$> dropNullFields,
            ("DynamicTransform" Data..=)
              Prelude.<$> dynamicTransform,
            ("DynamoDBCatalogSource" Data..=)
              Prelude.<$> dynamoDBCatalogSource,
            ("EvaluateDataQuality" Data..=)
              Prelude.<$> evaluateDataQuality,
            ("FillMissingValues" Data..=)
              Prelude.<$> fillMissingValues,
            ("Filter" Data..=) Prelude.<$> filter',
            ("GovernedCatalogSource" Data..=)
              Prelude.<$> governedCatalogSource,
            ("GovernedCatalogTarget" Data..=)
              Prelude.<$> governedCatalogTarget,
            ("JDBCConnectorSource" Data..=)
              Prelude.<$> jDBCConnectorSource,
            ("JDBCConnectorTarget" Data..=)
              Prelude.<$> jDBCConnectorTarget,
            ("Join" Data..=) Prelude.<$> join,
            ("Merge" Data..=) Prelude.<$> merge,
            ("MicrosoftSQLServerCatalogSource" Data..=)
              Prelude.<$> microsoftSQLServerCatalogSource,
            ("MicrosoftSQLServerCatalogTarget" Data..=)
              Prelude.<$> microsoftSQLServerCatalogTarget,
            ("MySQLCatalogSource" Data..=)
              Prelude.<$> mySQLCatalogSource,
            ("MySQLCatalogTarget" Data..=)
              Prelude.<$> mySQLCatalogTarget,
            ("OracleSQLCatalogSource" Data..=)
              Prelude.<$> oracleSQLCatalogSource,
            ("OracleSQLCatalogTarget" Data..=)
              Prelude.<$> oracleSQLCatalogTarget,
            ("PIIDetection" Data..=) Prelude.<$> pIIDetection,
            ("PostgreSQLCatalogSource" Data..=)
              Prelude.<$> postgreSQLCatalogSource,
            ("PostgreSQLCatalogTarget" Data..=)
              Prelude.<$> postgreSQLCatalogTarget,
            ("RedshiftSource" Data..=)
              Prelude.<$> redshiftSource,
            ("RedshiftTarget" Data..=)
              Prelude.<$> redshiftTarget,
            ("RelationalCatalogSource" Data..=)
              Prelude.<$> relationalCatalogSource,
            ("RenameField" Data..=) Prelude.<$> renameField,
            ("S3CatalogSource" Data..=)
              Prelude.<$> s3CatalogSource,
            ("S3CatalogTarget" Data..=)
              Prelude.<$> s3CatalogTarget,
            ("S3CsvSource" Data..=) Prelude.<$> s3CsvSource,
            ("S3DirectTarget" Data..=)
              Prelude.<$> s3DirectTarget,
            ("S3GlueParquetTarget" Data..=)
              Prelude.<$> s3GlueParquetTarget,
            ("S3JsonSource" Data..=) Prelude.<$> s3JsonSource,
            ("S3ParquetSource" Data..=)
              Prelude.<$> s3ParquetSource,
            ("SelectFields" Data..=) Prelude.<$> selectFields,
            ("SelectFromCollection" Data..=)
              Prelude.<$> selectFromCollection,
            ("SparkConnectorSource" Data..=)
              Prelude.<$> sparkConnectorSource,
            ("SparkConnectorTarget" Data..=)
              Prelude.<$> sparkConnectorTarget,
            ("SparkSQL" Data..=) Prelude.<$> sparkSQL,
            ("Spigot" Data..=) Prelude.<$> spigot,
            ("SplitFields" Data..=) Prelude.<$> splitFields,
            ("Union" Data..=) Prelude.<$> union
          ]
      )
