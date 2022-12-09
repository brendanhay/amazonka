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
-- Module      : Amazonka.Glue.Types.Table
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.Table where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.Column
import Amazonka.Glue.Types.StorageDescriptor
import Amazonka.Glue.Types.TableIdentifier
import qualified Amazonka.Prelude as Prelude

-- | Represents a collection of related data organized in columns and rows.
--
-- /See:/ 'newTable' smart constructor.
data Table = Table'
  { -- | The ID of the Data Catalog in which the table resides.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The time when the table definition was created in the Data Catalog.
    createTime :: Prelude.Maybe Data.POSIX,
    -- | The person or entity who created the table.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | The name of the database where the table metadata resides. For Hive
    -- compatibility, this must be all lowercase.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | A description of the table.
    description :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the table has been registered with Lake Formation.
    isRegisteredWithLakeFormation :: Prelude.Maybe Prelude.Bool,
    -- | The last time that the table was accessed. This is usually taken from
    -- HDFS, and might not be reliable.
    lastAccessTime :: Prelude.Maybe Data.POSIX,
    -- | The last time that column statistics were computed for this table.
    lastAnalyzedTime :: Prelude.Maybe Data.POSIX,
    -- | The owner of the table.
    owner :: Prelude.Maybe Prelude.Text,
    -- | These key-value pairs define properties associated with the table.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A list of columns by which the table is partitioned. Only primitive
    -- types are supported as partition keys.
    --
    -- When you create a table used by Amazon Athena, and you do not specify
    -- any @partitionKeys@, you must at least set the value of @partitionKeys@
    -- to an empty list. For example:
    --
    -- @\"PartitionKeys\": []@
    partitionKeys :: Prelude.Maybe [Column],
    -- | The retention time for this table.
    retention :: Prelude.Maybe Prelude.Natural,
    -- | A storage descriptor containing information about the physical storage
    -- of this table.
    storageDescriptor :: Prelude.Maybe StorageDescriptor,
    -- | The type of this table (@EXTERNAL_TABLE@, @VIRTUAL_VIEW@, etc.).
    tableType :: Prelude.Maybe Prelude.Text,
    -- | A @TableIdentifier@ structure that describes a target table for resource
    -- linking.
    targetTable :: Prelude.Maybe TableIdentifier,
    -- | The last time that the table was updated.
    updateTime :: Prelude.Maybe Data.POSIX,
    -- | The ID of the table version.
    versionId :: Prelude.Maybe Prelude.Text,
    -- | If the table is a view, the expanded text of the view; otherwise @null@.
    viewExpandedText :: Prelude.Maybe Prelude.Text,
    -- | If the table is a view, the original text of the view; otherwise @null@.
    viewOriginalText :: Prelude.Maybe Prelude.Text,
    -- | The table name. For Hive compatibility, this must be entirely lowercase.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Table' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'table_catalogId' - The ID of the Data Catalog in which the table resides.
--
-- 'createTime', 'table_createTime' - The time when the table definition was created in the Data Catalog.
--
-- 'createdBy', 'table_createdBy' - The person or entity who created the table.
--
-- 'databaseName', 'table_databaseName' - The name of the database where the table metadata resides. For Hive
-- compatibility, this must be all lowercase.
--
-- 'description', 'table_description' - A description of the table.
--
-- 'isRegisteredWithLakeFormation', 'table_isRegisteredWithLakeFormation' - Indicates whether the table has been registered with Lake Formation.
--
-- 'lastAccessTime', 'table_lastAccessTime' - The last time that the table was accessed. This is usually taken from
-- HDFS, and might not be reliable.
--
-- 'lastAnalyzedTime', 'table_lastAnalyzedTime' - The last time that column statistics were computed for this table.
--
-- 'owner', 'table_owner' - The owner of the table.
--
-- 'parameters', 'table_parameters' - These key-value pairs define properties associated with the table.
--
-- 'partitionKeys', 'table_partitionKeys' - A list of columns by which the table is partitioned. Only primitive
-- types are supported as partition keys.
--
-- When you create a table used by Amazon Athena, and you do not specify
-- any @partitionKeys@, you must at least set the value of @partitionKeys@
-- to an empty list. For example:
--
-- @\"PartitionKeys\": []@
--
-- 'retention', 'table_retention' - The retention time for this table.
--
-- 'storageDescriptor', 'table_storageDescriptor' - A storage descriptor containing information about the physical storage
-- of this table.
--
-- 'tableType', 'table_tableType' - The type of this table (@EXTERNAL_TABLE@, @VIRTUAL_VIEW@, etc.).
--
-- 'targetTable', 'table_targetTable' - A @TableIdentifier@ structure that describes a target table for resource
-- linking.
--
-- 'updateTime', 'table_updateTime' - The last time that the table was updated.
--
-- 'versionId', 'table_versionId' - The ID of the table version.
--
-- 'viewExpandedText', 'table_viewExpandedText' - If the table is a view, the expanded text of the view; otherwise @null@.
--
-- 'viewOriginalText', 'table_viewOriginalText' - If the table is a view, the original text of the view; otherwise @null@.
--
-- 'name', 'table_name' - The table name. For Hive compatibility, this must be entirely lowercase.
newTable ::
  -- | 'name'
  Prelude.Text ->
  Table
newTable pName_ =
  Table'
    { catalogId = Prelude.Nothing,
      createTime = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      description = Prelude.Nothing,
      isRegisteredWithLakeFormation = Prelude.Nothing,
      lastAccessTime = Prelude.Nothing,
      lastAnalyzedTime = Prelude.Nothing,
      owner = Prelude.Nothing,
      parameters = Prelude.Nothing,
      partitionKeys = Prelude.Nothing,
      retention = Prelude.Nothing,
      storageDescriptor = Prelude.Nothing,
      tableType = Prelude.Nothing,
      targetTable = Prelude.Nothing,
      updateTime = Prelude.Nothing,
      versionId = Prelude.Nothing,
      viewExpandedText = Prelude.Nothing,
      viewOriginalText = Prelude.Nothing,
      name = pName_
    }

-- | The ID of the Data Catalog in which the table resides.
table_catalogId :: Lens.Lens' Table (Prelude.Maybe Prelude.Text)
table_catalogId = Lens.lens (\Table' {catalogId} -> catalogId) (\s@Table' {} a -> s {catalogId = a} :: Table)

-- | The time when the table definition was created in the Data Catalog.
table_createTime :: Lens.Lens' Table (Prelude.Maybe Prelude.UTCTime)
table_createTime = Lens.lens (\Table' {createTime} -> createTime) (\s@Table' {} a -> s {createTime = a} :: Table) Prelude.. Lens.mapping Data._Time

-- | The person or entity who created the table.
table_createdBy :: Lens.Lens' Table (Prelude.Maybe Prelude.Text)
table_createdBy = Lens.lens (\Table' {createdBy} -> createdBy) (\s@Table' {} a -> s {createdBy = a} :: Table)

-- | The name of the database where the table metadata resides. For Hive
-- compatibility, this must be all lowercase.
table_databaseName :: Lens.Lens' Table (Prelude.Maybe Prelude.Text)
table_databaseName = Lens.lens (\Table' {databaseName} -> databaseName) (\s@Table' {} a -> s {databaseName = a} :: Table)

-- | A description of the table.
table_description :: Lens.Lens' Table (Prelude.Maybe Prelude.Text)
table_description = Lens.lens (\Table' {description} -> description) (\s@Table' {} a -> s {description = a} :: Table)

-- | Indicates whether the table has been registered with Lake Formation.
table_isRegisteredWithLakeFormation :: Lens.Lens' Table (Prelude.Maybe Prelude.Bool)
table_isRegisteredWithLakeFormation = Lens.lens (\Table' {isRegisteredWithLakeFormation} -> isRegisteredWithLakeFormation) (\s@Table' {} a -> s {isRegisteredWithLakeFormation = a} :: Table)

-- | The last time that the table was accessed. This is usually taken from
-- HDFS, and might not be reliable.
table_lastAccessTime :: Lens.Lens' Table (Prelude.Maybe Prelude.UTCTime)
table_lastAccessTime = Lens.lens (\Table' {lastAccessTime} -> lastAccessTime) (\s@Table' {} a -> s {lastAccessTime = a} :: Table) Prelude.. Lens.mapping Data._Time

-- | The last time that column statistics were computed for this table.
table_lastAnalyzedTime :: Lens.Lens' Table (Prelude.Maybe Prelude.UTCTime)
table_lastAnalyzedTime = Lens.lens (\Table' {lastAnalyzedTime} -> lastAnalyzedTime) (\s@Table' {} a -> s {lastAnalyzedTime = a} :: Table) Prelude.. Lens.mapping Data._Time

-- | The owner of the table.
table_owner :: Lens.Lens' Table (Prelude.Maybe Prelude.Text)
table_owner = Lens.lens (\Table' {owner} -> owner) (\s@Table' {} a -> s {owner = a} :: Table)

-- | These key-value pairs define properties associated with the table.
table_parameters :: Lens.Lens' Table (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
table_parameters = Lens.lens (\Table' {parameters} -> parameters) (\s@Table' {} a -> s {parameters = a} :: Table) Prelude.. Lens.mapping Lens.coerced

-- | A list of columns by which the table is partitioned. Only primitive
-- types are supported as partition keys.
--
-- When you create a table used by Amazon Athena, and you do not specify
-- any @partitionKeys@, you must at least set the value of @partitionKeys@
-- to an empty list. For example:
--
-- @\"PartitionKeys\": []@
table_partitionKeys :: Lens.Lens' Table (Prelude.Maybe [Column])
table_partitionKeys = Lens.lens (\Table' {partitionKeys} -> partitionKeys) (\s@Table' {} a -> s {partitionKeys = a} :: Table) Prelude.. Lens.mapping Lens.coerced

-- | The retention time for this table.
table_retention :: Lens.Lens' Table (Prelude.Maybe Prelude.Natural)
table_retention = Lens.lens (\Table' {retention} -> retention) (\s@Table' {} a -> s {retention = a} :: Table)

-- | A storage descriptor containing information about the physical storage
-- of this table.
table_storageDescriptor :: Lens.Lens' Table (Prelude.Maybe StorageDescriptor)
table_storageDescriptor = Lens.lens (\Table' {storageDescriptor} -> storageDescriptor) (\s@Table' {} a -> s {storageDescriptor = a} :: Table)

-- | The type of this table (@EXTERNAL_TABLE@, @VIRTUAL_VIEW@, etc.).
table_tableType :: Lens.Lens' Table (Prelude.Maybe Prelude.Text)
table_tableType = Lens.lens (\Table' {tableType} -> tableType) (\s@Table' {} a -> s {tableType = a} :: Table)

-- | A @TableIdentifier@ structure that describes a target table for resource
-- linking.
table_targetTable :: Lens.Lens' Table (Prelude.Maybe TableIdentifier)
table_targetTable = Lens.lens (\Table' {targetTable} -> targetTable) (\s@Table' {} a -> s {targetTable = a} :: Table)

-- | The last time that the table was updated.
table_updateTime :: Lens.Lens' Table (Prelude.Maybe Prelude.UTCTime)
table_updateTime = Lens.lens (\Table' {updateTime} -> updateTime) (\s@Table' {} a -> s {updateTime = a} :: Table) Prelude.. Lens.mapping Data._Time

-- | The ID of the table version.
table_versionId :: Lens.Lens' Table (Prelude.Maybe Prelude.Text)
table_versionId = Lens.lens (\Table' {versionId} -> versionId) (\s@Table' {} a -> s {versionId = a} :: Table)

-- | If the table is a view, the expanded text of the view; otherwise @null@.
table_viewExpandedText :: Lens.Lens' Table (Prelude.Maybe Prelude.Text)
table_viewExpandedText = Lens.lens (\Table' {viewExpandedText} -> viewExpandedText) (\s@Table' {} a -> s {viewExpandedText = a} :: Table)

-- | If the table is a view, the original text of the view; otherwise @null@.
table_viewOriginalText :: Lens.Lens' Table (Prelude.Maybe Prelude.Text)
table_viewOriginalText = Lens.lens (\Table' {viewOriginalText} -> viewOriginalText) (\s@Table' {} a -> s {viewOriginalText = a} :: Table)

-- | The table name. For Hive compatibility, this must be entirely lowercase.
table_name :: Lens.Lens' Table Prelude.Text
table_name = Lens.lens (\Table' {name} -> name) (\s@Table' {} a -> s {name = a} :: Table)

instance Data.FromJSON Table where
  parseJSON =
    Data.withObject
      "Table"
      ( \x ->
          Table'
            Prelude.<$> (x Data..:? "CatalogId")
            Prelude.<*> (x Data..:? "CreateTime")
            Prelude.<*> (x Data..:? "CreatedBy")
            Prelude.<*> (x Data..:? "DatabaseName")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "IsRegisteredWithLakeFormation")
            Prelude.<*> (x Data..:? "LastAccessTime")
            Prelude.<*> (x Data..:? "LastAnalyzedTime")
            Prelude.<*> (x Data..:? "Owner")
            Prelude.<*> (x Data..:? "Parameters" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "PartitionKeys" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Retention")
            Prelude.<*> (x Data..:? "StorageDescriptor")
            Prelude.<*> (x Data..:? "TableType")
            Prelude.<*> (x Data..:? "TargetTable")
            Prelude.<*> (x Data..:? "UpdateTime")
            Prelude.<*> (x Data..:? "VersionId")
            Prelude.<*> (x Data..:? "ViewExpandedText")
            Prelude.<*> (x Data..:? "ViewOriginalText")
            Prelude.<*> (x Data..: "Name")
      )

instance Prelude.Hashable Table where
  hashWithSalt _salt Table' {..} =
    _salt `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` isRegisteredWithLakeFormation
      `Prelude.hashWithSalt` lastAccessTime
      `Prelude.hashWithSalt` lastAnalyzedTime
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` partitionKeys
      `Prelude.hashWithSalt` retention
      `Prelude.hashWithSalt` storageDescriptor
      `Prelude.hashWithSalt` tableType
      `Prelude.hashWithSalt` targetTable
      `Prelude.hashWithSalt` updateTime
      `Prelude.hashWithSalt` versionId
      `Prelude.hashWithSalt` viewExpandedText
      `Prelude.hashWithSalt` viewOriginalText
      `Prelude.hashWithSalt` name

instance Prelude.NFData Table where
  rnf Table' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf isRegisteredWithLakeFormation
      `Prelude.seq` Prelude.rnf lastAccessTime
      `Prelude.seq` Prelude.rnf lastAnalyzedTime
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf partitionKeys
      `Prelude.seq` Prelude.rnf retention
      `Prelude.seq` Prelude.rnf storageDescriptor
      `Prelude.seq` Prelude.rnf tableType
      `Prelude.seq` Prelude.rnf targetTable
      `Prelude.seq` Prelude.rnf updateTime
      `Prelude.seq` Prelude.rnf versionId
      `Prelude.seq` Prelude.rnf viewExpandedText
      `Prelude.seq` Prelude.rnf viewOriginalText
      `Prelude.seq` Prelude.rnf name
