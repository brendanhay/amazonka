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
-- Module      : Amazonka.Glue.Types.AmazonRedshiftNodeData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.AmazonRedshiftNodeData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.AmazonRedshiftAdvancedOption
import Amazonka.Glue.Types.Option
import qualified Amazonka.Prelude as Prelude

-- | Specifies an Amazon Redshift node.
--
-- /See:/ 'newAmazonRedshiftNodeData' smart constructor.
data AmazonRedshiftNodeData = AmazonRedshiftNodeData'
  { -- | The access type for the Redshift connection. Can be a direct connection
    -- or catalog connections.
    accessType :: Prelude.Maybe Prelude.Text,
    -- | Specifies how writing to a Redshift cluser will occur.
    action :: Prelude.Maybe Prelude.Text,
    -- | Optional values when connecting to the Redshift cluster.
    advancedOptions :: Prelude.Maybe [AmazonRedshiftAdvancedOption],
    -- | The name of the Glue Data Catalog database when working with a data
    -- catalog.
    catalogDatabase :: Prelude.Maybe Option,
    -- | The Redshift schema name when working with a data catalog.
    catalogRedshiftSchema :: Prelude.Maybe Prelude.Text,
    -- | The database table to read from.
    catalogRedshiftTable :: Prelude.Maybe Prelude.Text,
    -- | The Glue Data Catalog table name when working with a data catalog.
    catalogTable :: Prelude.Maybe Option,
    -- | The Glue connection to the Redshift cluster.
    connection :: Prelude.Maybe Option,
    -- | Specifies the name of the connection that is associated with the catalog
    -- table used.
    crawlerConnection :: Prelude.Maybe Prelude.Text,
    -- | Optional. The role name use when connection to S3. The IAM role ill
    -- default to the role on the job when left blank.
    iamRole :: Prelude.Maybe Option,
    -- | The action used when to detemine how a MERGE in a Redshift sink will be
    -- handled.
    mergeAction :: Prelude.Maybe Prelude.Text,
    -- | The SQL used in a custom merge to deal with matching records.
    mergeClause :: Prelude.Maybe Prelude.Text,
    -- | The action used when to detemine how a MERGE in a Redshift sink will be
    -- handled when an existing record matches a new record.
    mergeWhenMatched :: Prelude.Maybe Prelude.Text,
    -- | The action used when to detemine how a MERGE in a Redshift sink will be
    -- handled when an existing record doesn\'t match a new record.
    mergeWhenNotMatched :: Prelude.Maybe Prelude.Text,
    -- | The SQL used before a MERGE or APPEND with upsert is run.
    postAction :: Prelude.Maybe Prelude.Text,
    -- | The SQL used before a MERGE or APPEND with upsert is run.
    preAction :: Prelude.Maybe Prelude.Text,
    -- | The SQL used to fetch the data from a Redshift sources when the
    -- SourceType is \'query\'.
    sampleQuery :: Prelude.Maybe Prelude.Text,
    -- | The Redshift schema name when working with a direct connection.
    schema :: Prelude.Maybe Option,
    -- | The list of column names used to determine a matching record when doing
    -- a MERGE or APPEND with upsert.
    selectedColumns :: Prelude.Maybe [Option],
    -- | The source type to specify whether a specific table is the source or a
    -- custom query.
    sourceType :: Prelude.Maybe Prelude.Text,
    -- | The name of the temporary staging table that is used when doing a MERGE
    -- or APPEND with upsert.
    stagingTable :: Prelude.Maybe Prelude.Text,
    -- | The Redshift table name when working with a direct connection.
    table :: Prelude.Maybe Option,
    -- | Specifies the prefix to a table.
    tablePrefix :: Prelude.Maybe Prelude.Text,
    -- | The array of schema output for a given node.
    tableSchema :: Prelude.Maybe [Option],
    -- | The Amazon S3 path where temporary data can be staged when copying out
    -- of the database.
    tempDir :: Prelude.Maybe Prelude.Text,
    -- | The action used on Redshift sinks when doing an APPEND.
    upsert :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AmazonRedshiftNodeData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessType', 'amazonRedshiftNodeData_accessType' - The access type for the Redshift connection. Can be a direct connection
-- or catalog connections.
--
-- 'action', 'amazonRedshiftNodeData_action' - Specifies how writing to a Redshift cluser will occur.
--
-- 'advancedOptions', 'amazonRedshiftNodeData_advancedOptions' - Optional values when connecting to the Redshift cluster.
--
-- 'catalogDatabase', 'amazonRedshiftNodeData_catalogDatabase' - The name of the Glue Data Catalog database when working with a data
-- catalog.
--
-- 'catalogRedshiftSchema', 'amazonRedshiftNodeData_catalogRedshiftSchema' - The Redshift schema name when working with a data catalog.
--
-- 'catalogRedshiftTable', 'amazonRedshiftNodeData_catalogRedshiftTable' - The database table to read from.
--
-- 'catalogTable', 'amazonRedshiftNodeData_catalogTable' - The Glue Data Catalog table name when working with a data catalog.
--
-- 'connection', 'amazonRedshiftNodeData_connection' - The Glue connection to the Redshift cluster.
--
-- 'crawlerConnection', 'amazonRedshiftNodeData_crawlerConnection' - Specifies the name of the connection that is associated with the catalog
-- table used.
--
-- 'iamRole', 'amazonRedshiftNodeData_iamRole' - Optional. The role name use when connection to S3. The IAM role ill
-- default to the role on the job when left blank.
--
-- 'mergeAction', 'amazonRedshiftNodeData_mergeAction' - The action used when to detemine how a MERGE in a Redshift sink will be
-- handled.
--
-- 'mergeClause', 'amazonRedshiftNodeData_mergeClause' - The SQL used in a custom merge to deal with matching records.
--
-- 'mergeWhenMatched', 'amazonRedshiftNodeData_mergeWhenMatched' - The action used when to detemine how a MERGE in a Redshift sink will be
-- handled when an existing record matches a new record.
--
-- 'mergeWhenNotMatched', 'amazonRedshiftNodeData_mergeWhenNotMatched' - The action used when to detemine how a MERGE in a Redshift sink will be
-- handled when an existing record doesn\'t match a new record.
--
-- 'postAction', 'amazonRedshiftNodeData_postAction' - The SQL used before a MERGE or APPEND with upsert is run.
--
-- 'preAction', 'amazonRedshiftNodeData_preAction' - The SQL used before a MERGE or APPEND with upsert is run.
--
-- 'sampleQuery', 'amazonRedshiftNodeData_sampleQuery' - The SQL used to fetch the data from a Redshift sources when the
-- SourceType is \'query\'.
--
-- 'schema', 'amazonRedshiftNodeData_schema' - The Redshift schema name when working with a direct connection.
--
-- 'selectedColumns', 'amazonRedshiftNodeData_selectedColumns' - The list of column names used to determine a matching record when doing
-- a MERGE or APPEND with upsert.
--
-- 'sourceType', 'amazonRedshiftNodeData_sourceType' - The source type to specify whether a specific table is the source or a
-- custom query.
--
-- 'stagingTable', 'amazonRedshiftNodeData_stagingTable' - The name of the temporary staging table that is used when doing a MERGE
-- or APPEND with upsert.
--
-- 'table', 'amazonRedshiftNodeData_table' - The Redshift table name when working with a direct connection.
--
-- 'tablePrefix', 'amazonRedshiftNodeData_tablePrefix' - Specifies the prefix to a table.
--
-- 'tableSchema', 'amazonRedshiftNodeData_tableSchema' - The array of schema output for a given node.
--
-- 'tempDir', 'amazonRedshiftNodeData_tempDir' - The Amazon S3 path where temporary data can be staged when copying out
-- of the database.
--
-- 'upsert', 'amazonRedshiftNodeData_upsert' - The action used on Redshift sinks when doing an APPEND.
newAmazonRedshiftNodeData ::
  AmazonRedshiftNodeData
newAmazonRedshiftNodeData =
  AmazonRedshiftNodeData'
    { accessType =
        Prelude.Nothing,
      action = Prelude.Nothing,
      advancedOptions = Prelude.Nothing,
      catalogDatabase = Prelude.Nothing,
      catalogRedshiftSchema = Prelude.Nothing,
      catalogRedshiftTable = Prelude.Nothing,
      catalogTable = Prelude.Nothing,
      connection = Prelude.Nothing,
      crawlerConnection = Prelude.Nothing,
      iamRole = Prelude.Nothing,
      mergeAction = Prelude.Nothing,
      mergeClause = Prelude.Nothing,
      mergeWhenMatched = Prelude.Nothing,
      mergeWhenNotMatched = Prelude.Nothing,
      postAction = Prelude.Nothing,
      preAction = Prelude.Nothing,
      sampleQuery = Prelude.Nothing,
      schema = Prelude.Nothing,
      selectedColumns = Prelude.Nothing,
      sourceType = Prelude.Nothing,
      stagingTable = Prelude.Nothing,
      table = Prelude.Nothing,
      tablePrefix = Prelude.Nothing,
      tableSchema = Prelude.Nothing,
      tempDir = Prelude.Nothing,
      upsert = Prelude.Nothing
    }

-- | The access type for the Redshift connection. Can be a direct connection
-- or catalog connections.
amazonRedshiftNodeData_accessType :: Lens.Lens' AmazonRedshiftNodeData (Prelude.Maybe Prelude.Text)
amazonRedshiftNodeData_accessType = Lens.lens (\AmazonRedshiftNodeData' {accessType} -> accessType) (\s@AmazonRedshiftNodeData' {} a -> s {accessType = a} :: AmazonRedshiftNodeData)

-- | Specifies how writing to a Redshift cluser will occur.
amazonRedshiftNodeData_action :: Lens.Lens' AmazonRedshiftNodeData (Prelude.Maybe Prelude.Text)
amazonRedshiftNodeData_action = Lens.lens (\AmazonRedshiftNodeData' {action} -> action) (\s@AmazonRedshiftNodeData' {} a -> s {action = a} :: AmazonRedshiftNodeData)

-- | Optional values when connecting to the Redshift cluster.
amazonRedshiftNodeData_advancedOptions :: Lens.Lens' AmazonRedshiftNodeData (Prelude.Maybe [AmazonRedshiftAdvancedOption])
amazonRedshiftNodeData_advancedOptions = Lens.lens (\AmazonRedshiftNodeData' {advancedOptions} -> advancedOptions) (\s@AmazonRedshiftNodeData' {} a -> s {advancedOptions = a} :: AmazonRedshiftNodeData) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Glue Data Catalog database when working with a data
-- catalog.
amazonRedshiftNodeData_catalogDatabase :: Lens.Lens' AmazonRedshiftNodeData (Prelude.Maybe Option)
amazonRedshiftNodeData_catalogDatabase = Lens.lens (\AmazonRedshiftNodeData' {catalogDatabase} -> catalogDatabase) (\s@AmazonRedshiftNodeData' {} a -> s {catalogDatabase = a} :: AmazonRedshiftNodeData)

-- | The Redshift schema name when working with a data catalog.
amazonRedshiftNodeData_catalogRedshiftSchema :: Lens.Lens' AmazonRedshiftNodeData (Prelude.Maybe Prelude.Text)
amazonRedshiftNodeData_catalogRedshiftSchema = Lens.lens (\AmazonRedshiftNodeData' {catalogRedshiftSchema} -> catalogRedshiftSchema) (\s@AmazonRedshiftNodeData' {} a -> s {catalogRedshiftSchema = a} :: AmazonRedshiftNodeData)

-- | The database table to read from.
amazonRedshiftNodeData_catalogRedshiftTable :: Lens.Lens' AmazonRedshiftNodeData (Prelude.Maybe Prelude.Text)
amazonRedshiftNodeData_catalogRedshiftTable = Lens.lens (\AmazonRedshiftNodeData' {catalogRedshiftTable} -> catalogRedshiftTable) (\s@AmazonRedshiftNodeData' {} a -> s {catalogRedshiftTable = a} :: AmazonRedshiftNodeData)

-- | The Glue Data Catalog table name when working with a data catalog.
amazonRedshiftNodeData_catalogTable :: Lens.Lens' AmazonRedshiftNodeData (Prelude.Maybe Option)
amazonRedshiftNodeData_catalogTable = Lens.lens (\AmazonRedshiftNodeData' {catalogTable} -> catalogTable) (\s@AmazonRedshiftNodeData' {} a -> s {catalogTable = a} :: AmazonRedshiftNodeData)

-- | The Glue connection to the Redshift cluster.
amazonRedshiftNodeData_connection :: Lens.Lens' AmazonRedshiftNodeData (Prelude.Maybe Option)
amazonRedshiftNodeData_connection = Lens.lens (\AmazonRedshiftNodeData' {connection} -> connection) (\s@AmazonRedshiftNodeData' {} a -> s {connection = a} :: AmazonRedshiftNodeData)

-- | Specifies the name of the connection that is associated with the catalog
-- table used.
amazonRedshiftNodeData_crawlerConnection :: Lens.Lens' AmazonRedshiftNodeData (Prelude.Maybe Prelude.Text)
amazonRedshiftNodeData_crawlerConnection = Lens.lens (\AmazonRedshiftNodeData' {crawlerConnection} -> crawlerConnection) (\s@AmazonRedshiftNodeData' {} a -> s {crawlerConnection = a} :: AmazonRedshiftNodeData)

-- | Optional. The role name use when connection to S3. The IAM role ill
-- default to the role on the job when left blank.
amazonRedshiftNodeData_iamRole :: Lens.Lens' AmazonRedshiftNodeData (Prelude.Maybe Option)
amazonRedshiftNodeData_iamRole = Lens.lens (\AmazonRedshiftNodeData' {iamRole} -> iamRole) (\s@AmazonRedshiftNodeData' {} a -> s {iamRole = a} :: AmazonRedshiftNodeData)

-- | The action used when to detemine how a MERGE in a Redshift sink will be
-- handled.
amazonRedshiftNodeData_mergeAction :: Lens.Lens' AmazonRedshiftNodeData (Prelude.Maybe Prelude.Text)
amazonRedshiftNodeData_mergeAction = Lens.lens (\AmazonRedshiftNodeData' {mergeAction} -> mergeAction) (\s@AmazonRedshiftNodeData' {} a -> s {mergeAction = a} :: AmazonRedshiftNodeData)

-- | The SQL used in a custom merge to deal with matching records.
amazonRedshiftNodeData_mergeClause :: Lens.Lens' AmazonRedshiftNodeData (Prelude.Maybe Prelude.Text)
amazonRedshiftNodeData_mergeClause = Lens.lens (\AmazonRedshiftNodeData' {mergeClause} -> mergeClause) (\s@AmazonRedshiftNodeData' {} a -> s {mergeClause = a} :: AmazonRedshiftNodeData)

-- | The action used when to detemine how a MERGE in a Redshift sink will be
-- handled when an existing record matches a new record.
amazonRedshiftNodeData_mergeWhenMatched :: Lens.Lens' AmazonRedshiftNodeData (Prelude.Maybe Prelude.Text)
amazonRedshiftNodeData_mergeWhenMatched = Lens.lens (\AmazonRedshiftNodeData' {mergeWhenMatched} -> mergeWhenMatched) (\s@AmazonRedshiftNodeData' {} a -> s {mergeWhenMatched = a} :: AmazonRedshiftNodeData)

-- | The action used when to detemine how a MERGE in a Redshift sink will be
-- handled when an existing record doesn\'t match a new record.
amazonRedshiftNodeData_mergeWhenNotMatched :: Lens.Lens' AmazonRedshiftNodeData (Prelude.Maybe Prelude.Text)
amazonRedshiftNodeData_mergeWhenNotMatched = Lens.lens (\AmazonRedshiftNodeData' {mergeWhenNotMatched} -> mergeWhenNotMatched) (\s@AmazonRedshiftNodeData' {} a -> s {mergeWhenNotMatched = a} :: AmazonRedshiftNodeData)

-- | The SQL used before a MERGE or APPEND with upsert is run.
amazonRedshiftNodeData_postAction :: Lens.Lens' AmazonRedshiftNodeData (Prelude.Maybe Prelude.Text)
amazonRedshiftNodeData_postAction = Lens.lens (\AmazonRedshiftNodeData' {postAction} -> postAction) (\s@AmazonRedshiftNodeData' {} a -> s {postAction = a} :: AmazonRedshiftNodeData)

-- | The SQL used before a MERGE or APPEND with upsert is run.
amazonRedshiftNodeData_preAction :: Lens.Lens' AmazonRedshiftNodeData (Prelude.Maybe Prelude.Text)
amazonRedshiftNodeData_preAction = Lens.lens (\AmazonRedshiftNodeData' {preAction} -> preAction) (\s@AmazonRedshiftNodeData' {} a -> s {preAction = a} :: AmazonRedshiftNodeData)

-- | The SQL used to fetch the data from a Redshift sources when the
-- SourceType is \'query\'.
amazonRedshiftNodeData_sampleQuery :: Lens.Lens' AmazonRedshiftNodeData (Prelude.Maybe Prelude.Text)
amazonRedshiftNodeData_sampleQuery = Lens.lens (\AmazonRedshiftNodeData' {sampleQuery} -> sampleQuery) (\s@AmazonRedshiftNodeData' {} a -> s {sampleQuery = a} :: AmazonRedshiftNodeData)

-- | The Redshift schema name when working with a direct connection.
amazonRedshiftNodeData_schema :: Lens.Lens' AmazonRedshiftNodeData (Prelude.Maybe Option)
amazonRedshiftNodeData_schema = Lens.lens (\AmazonRedshiftNodeData' {schema} -> schema) (\s@AmazonRedshiftNodeData' {} a -> s {schema = a} :: AmazonRedshiftNodeData)

-- | The list of column names used to determine a matching record when doing
-- a MERGE or APPEND with upsert.
amazonRedshiftNodeData_selectedColumns :: Lens.Lens' AmazonRedshiftNodeData (Prelude.Maybe [Option])
amazonRedshiftNodeData_selectedColumns = Lens.lens (\AmazonRedshiftNodeData' {selectedColumns} -> selectedColumns) (\s@AmazonRedshiftNodeData' {} a -> s {selectedColumns = a} :: AmazonRedshiftNodeData) Prelude.. Lens.mapping Lens.coerced

-- | The source type to specify whether a specific table is the source or a
-- custom query.
amazonRedshiftNodeData_sourceType :: Lens.Lens' AmazonRedshiftNodeData (Prelude.Maybe Prelude.Text)
amazonRedshiftNodeData_sourceType = Lens.lens (\AmazonRedshiftNodeData' {sourceType} -> sourceType) (\s@AmazonRedshiftNodeData' {} a -> s {sourceType = a} :: AmazonRedshiftNodeData)

-- | The name of the temporary staging table that is used when doing a MERGE
-- or APPEND with upsert.
amazonRedshiftNodeData_stagingTable :: Lens.Lens' AmazonRedshiftNodeData (Prelude.Maybe Prelude.Text)
amazonRedshiftNodeData_stagingTable = Lens.lens (\AmazonRedshiftNodeData' {stagingTable} -> stagingTable) (\s@AmazonRedshiftNodeData' {} a -> s {stagingTable = a} :: AmazonRedshiftNodeData)

-- | The Redshift table name when working with a direct connection.
amazonRedshiftNodeData_table :: Lens.Lens' AmazonRedshiftNodeData (Prelude.Maybe Option)
amazonRedshiftNodeData_table = Lens.lens (\AmazonRedshiftNodeData' {table} -> table) (\s@AmazonRedshiftNodeData' {} a -> s {table = a} :: AmazonRedshiftNodeData)

-- | Specifies the prefix to a table.
amazonRedshiftNodeData_tablePrefix :: Lens.Lens' AmazonRedshiftNodeData (Prelude.Maybe Prelude.Text)
amazonRedshiftNodeData_tablePrefix = Lens.lens (\AmazonRedshiftNodeData' {tablePrefix} -> tablePrefix) (\s@AmazonRedshiftNodeData' {} a -> s {tablePrefix = a} :: AmazonRedshiftNodeData)

-- | The array of schema output for a given node.
amazonRedshiftNodeData_tableSchema :: Lens.Lens' AmazonRedshiftNodeData (Prelude.Maybe [Option])
amazonRedshiftNodeData_tableSchema = Lens.lens (\AmazonRedshiftNodeData' {tableSchema} -> tableSchema) (\s@AmazonRedshiftNodeData' {} a -> s {tableSchema = a} :: AmazonRedshiftNodeData) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon S3 path where temporary data can be staged when copying out
-- of the database.
amazonRedshiftNodeData_tempDir :: Lens.Lens' AmazonRedshiftNodeData (Prelude.Maybe Prelude.Text)
amazonRedshiftNodeData_tempDir = Lens.lens (\AmazonRedshiftNodeData' {tempDir} -> tempDir) (\s@AmazonRedshiftNodeData' {} a -> s {tempDir = a} :: AmazonRedshiftNodeData)

-- | The action used on Redshift sinks when doing an APPEND.
amazonRedshiftNodeData_upsert :: Lens.Lens' AmazonRedshiftNodeData (Prelude.Maybe Prelude.Bool)
amazonRedshiftNodeData_upsert = Lens.lens (\AmazonRedshiftNodeData' {upsert} -> upsert) (\s@AmazonRedshiftNodeData' {} a -> s {upsert = a} :: AmazonRedshiftNodeData)

instance Data.FromJSON AmazonRedshiftNodeData where
  parseJSON =
    Data.withObject
      "AmazonRedshiftNodeData"
      ( \x ->
          AmazonRedshiftNodeData'
            Prelude.<$> (x Data..:? "AccessType")
            Prelude.<*> (x Data..:? "Action")
            Prelude.<*> ( x
                            Data..:? "AdvancedOptions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "CatalogDatabase")
            Prelude.<*> (x Data..:? "CatalogRedshiftSchema")
            Prelude.<*> (x Data..:? "CatalogRedshiftTable")
            Prelude.<*> (x Data..:? "CatalogTable")
            Prelude.<*> (x Data..:? "Connection")
            Prelude.<*> (x Data..:? "CrawlerConnection")
            Prelude.<*> (x Data..:? "IamRole")
            Prelude.<*> (x Data..:? "MergeAction")
            Prelude.<*> (x Data..:? "MergeClause")
            Prelude.<*> (x Data..:? "MergeWhenMatched")
            Prelude.<*> (x Data..:? "MergeWhenNotMatched")
            Prelude.<*> (x Data..:? "PostAction")
            Prelude.<*> (x Data..:? "PreAction")
            Prelude.<*> (x Data..:? "SampleQuery")
            Prelude.<*> (x Data..:? "Schema")
            Prelude.<*> ( x
                            Data..:? "SelectedColumns"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "SourceType")
            Prelude.<*> (x Data..:? "StagingTable")
            Prelude.<*> (x Data..:? "Table")
            Prelude.<*> (x Data..:? "TablePrefix")
            Prelude.<*> (x Data..:? "TableSchema" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "TempDir")
            Prelude.<*> (x Data..:? "Upsert")
      )

instance Prelude.Hashable AmazonRedshiftNodeData where
  hashWithSalt _salt AmazonRedshiftNodeData' {..} =
    _salt
      `Prelude.hashWithSalt` accessType
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` advancedOptions
      `Prelude.hashWithSalt` catalogDatabase
      `Prelude.hashWithSalt` catalogRedshiftSchema
      `Prelude.hashWithSalt` catalogRedshiftTable
      `Prelude.hashWithSalt` catalogTable
      `Prelude.hashWithSalt` connection
      `Prelude.hashWithSalt` crawlerConnection
      `Prelude.hashWithSalt` iamRole
      `Prelude.hashWithSalt` mergeAction
      `Prelude.hashWithSalt` mergeClause
      `Prelude.hashWithSalt` mergeWhenMatched
      `Prelude.hashWithSalt` mergeWhenNotMatched
      `Prelude.hashWithSalt` postAction
      `Prelude.hashWithSalt` preAction
      `Prelude.hashWithSalt` sampleQuery
      `Prelude.hashWithSalt` schema
      `Prelude.hashWithSalt` selectedColumns
      `Prelude.hashWithSalt` sourceType
      `Prelude.hashWithSalt` stagingTable
      `Prelude.hashWithSalt` table
      `Prelude.hashWithSalt` tablePrefix
      `Prelude.hashWithSalt` tableSchema
      `Prelude.hashWithSalt` tempDir
      `Prelude.hashWithSalt` upsert

instance Prelude.NFData AmazonRedshiftNodeData where
  rnf AmazonRedshiftNodeData' {..} =
    Prelude.rnf accessType
      `Prelude.seq` Prelude.rnf action
      `Prelude.seq` Prelude.rnf advancedOptions
      `Prelude.seq` Prelude.rnf catalogDatabase
      `Prelude.seq` Prelude.rnf catalogRedshiftSchema
      `Prelude.seq` Prelude.rnf catalogRedshiftTable
      `Prelude.seq` Prelude.rnf catalogTable
      `Prelude.seq` Prelude.rnf connection
      `Prelude.seq` Prelude.rnf crawlerConnection
      `Prelude.seq` Prelude.rnf iamRole
      `Prelude.seq` Prelude.rnf mergeAction
      `Prelude.seq` Prelude.rnf mergeClause
      `Prelude.seq` Prelude.rnf mergeWhenMatched
      `Prelude.seq` Prelude.rnf mergeWhenNotMatched
      `Prelude.seq` Prelude.rnf postAction
      `Prelude.seq` Prelude.rnf preAction
      `Prelude.seq` Prelude.rnf sampleQuery
      `Prelude.seq` Prelude.rnf schema
      `Prelude.seq` Prelude.rnf selectedColumns
      `Prelude.seq` Prelude.rnf sourceType
      `Prelude.seq` Prelude.rnf stagingTable
      `Prelude.seq` Prelude.rnf table
      `Prelude.seq` Prelude.rnf tablePrefix
      `Prelude.seq` Prelude.rnf
        tableSchema
      `Prelude.seq` Prelude.rnf tempDir
      `Prelude.seq` Prelude.rnf upsert

instance Data.ToJSON AmazonRedshiftNodeData where
  toJSON AmazonRedshiftNodeData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccessType" Data..=) Prelude.<$> accessType,
            ("Action" Data..=) Prelude.<$> action,
            ("AdvancedOptions" Data..=)
              Prelude.<$> advancedOptions,
            ("CatalogDatabase" Data..=)
              Prelude.<$> catalogDatabase,
            ("CatalogRedshiftSchema" Data..=)
              Prelude.<$> catalogRedshiftSchema,
            ("CatalogRedshiftTable" Data..=)
              Prelude.<$> catalogRedshiftTable,
            ("CatalogTable" Data..=) Prelude.<$> catalogTable,
            ("Connection" Data..=) Prelude.<$> connection,
            ("CrawlerConnection" Data..=)
              Prelude.<$> crawlerConnection,
            ("IamRole" Data..=) Prelude.<$> iamRole,
            ("MergeAction" Data..=) Prelude.<$> mergeAction,
            ("MergeClause" Data..=) Prelude.<$> mergeClause,
            ("MergeWhenMatched" Data..=)
              Prelude.<$> mergeWhenMatched,
            ("MergeWhenNotMatched" Data..=)
              Prelude.<$> mergeWhenNotMatched,
            ("PostAction" Data..=) Prelude.<$> postAction,
            ("PreAction" Data..=) Prelude.<$> preAction,
            ("SampleQuery" Data..=) Prelude.<$> sampleQuery,
            ("Schema" Data..=) Prelude.<$> schema,
            ("SelectedColumns" Data..=)
              Prelude.<$> selectedColumns,
            ("SourceType" Data..=) Prelude.<$> sourceType,
            ("StagingTable" Data..=) Prelude.<$> stagingTable,
            ("Table" Data..=) Prelude.<$> table,
            ("TablePrefix" Data..=) Prelude.<$> tablePrefix,
            ("TableSchema" Data..=) Prelude.<$> tableSchema,
            ("TempDir" Data..=) Prelude.<$> tempDir,
            ("Upsert" Data..=) Prelude.<$> upsert
          ]
      )
