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
-- Module      : Network.AWS.Glue.Types.Table
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Table where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.Column
import Network.AWS.Glue.Types.StorageDescriptor
import Network.AWS.Glue.Types.TableIdentifier
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents a collection of related data organized in columns and rows.
--
-- /See:/ 'newTable' smart constructor.
data Table = Table'
  { -- | If the table is a view, the original text of the view; otherwise @null@.
    viewOriginalText :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Data Catalog in which the table resides.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The type of this table (@EXTERNAL_TABLE@, @VIRTUAL_VIEW@, etc.).
    tableType :: Prelude.Maybe Prelude.Text,
    -- | A storage descriptor containing information about the physical storage
    -- of this table.
    storageDescriptor :: Prelude.Maybe StorageDescriptor,
    -- | The last time that column statistics were computed for this table.
    lastAnalyzedTime :: Prelude.Maybe Core.POSIX,
    -- | If the table is a view, the expanded text of the view; otherwise @null@.
    viewExpandedText :: Prelude.Maybe Prelude.Text,
    -- | A @TableIdentifier@ structure that describes a target table for resource
    -- linking.
    targetTable :: Prelude.Maybe TableIdentifier,
    -- | The retention time for this table.
    retention :: Prelude.Maybe Prelude.Natural,
    -- | The last time that the table was updated.
    updateTime :: Prelude.Maybe Core.POSIX,
    -- | The time when the table definition was created in the Data Catalog.
    createTime :: Prelude.Maybe Core.POSIX,
    -- | The owner of the table.
    owner :: Prelude.Maybe Prelude.Text,
    -- | A list of columns by which the table is partitioned. Only primitive
    -- types are supported as partition keys.
    --
    -- When you create a table used by Amazon Athena, and you do not specify
    -- any @partitionKeys@, you must at least set the value of @partitionKeys@
    -- to an empty list. For example:
    --
    -- @\"PartitionKeys\": []@
    partitionKeys :: Prelude.Maybe [Column],
    -- | A description of the table.
    description :: Prelude.Maybe Prelude.Text,
    -- | The last time that the table was accessed. This is usually taken from
    -- HDFS, and might not be reliable.
    lastAccessTime :: Prelude.Maybe Core.POSIX,
    -- | The person or entity who created the table.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the table has been registered with AWS Lake Formation.
    isRegisteredWithLakeFormation :: Prelude.Maybe Prelude.Bool,
    -- | These key-value pairs define properties associated with the table.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the database where the table metadata resides. For Hive
    -- compatibility, this must be all lowercase.
    databaseName :: Prelude.Maybe Prelude.Text,
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
-- 'viewOriginalText', 'table_viewOriginalText' - If the table is a view, the original text of the view; otherwise @null@.
--
-- 'catalogId', 'table_catalogId' - The ID of the Data Catalog in which the table resides.
--
-- 'tableType', 'table_tableType' - The type of this table (@EXTERNAL_TABLE@, @VIRTUAL_VIEW@, etc.).
--
-- 'storageDescriptor', 'table_storageDescriptor' - A storage descriptor containing information about the physical storage
-- of this table.
--
-- 'lastAnalyzedTime', 'table_lastAnalyzedTime' - The last time that column statistics were computed for this table.
--
-- 'viewExpandedText', 'table_viewExpandedText' - If the table is a view, the expanded text of the view; otherwise @null@.
--
-- 'targetTable', 'table_targetTable' - A @TableIdentifier@ structure that describes a target table for resource
-- linking.
--
-- 'retention', 'table_retention' - The retention time for this table.
--
-- 'updateTime', 'table_updateTime' - The last time that the table was updated.
--
-- 'createTime', 'table_createTime' - The time when the table definition was created in the Data Catalog.
--
-- 'owner', 'table_owner' - The owner of the table.
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
-- 'description', 'table_description' - A description of the table.
--
-- 'lastAccessTime', 'table_lastAccessTime' - The last time that the table was accessed. This is usually taken from
-- HDFS, and might not be reliable.
--
-- 'createdBy', 'table_createdBy' - The person or entity who created the table.
--
-- 'isRegisteredWithLakeFormation', 'table_isRegisteredWithLakeFormation' - Indicates whether the table has been registered with AWS Lake Formation.
--
-- 'parameters', 'table_parameters' - These key-value pairs define properties associated with the table.
--
-- 'databaseName', 'table_databaseName' - The name of the database where the table metadata resides. For Hive
-- compatibility, this must be all lowercase.
--
-- 'name', 'table_name' - The table name. For Hive compatibility, this must be entirely lowercase.
newTable ::
  -- | 'name'
  Prelude.Text ->
  Table
newTable pName_ =
  Table'
    { viewOriginalText = Prelude.Nothing,
      catalogId = Prelude.Nothing,
      tableType = Prelude.Nothing,
      storageDescriptor = Prelude.Nothing,
      lastAnalyzedTime = Prelude.Nothing,
      viewExpandedText = Prelude.Nothing,
      targetTable = Prelude.Nothing,
      retention = Prelude.Nothing,
      updateTime = Prelude.Nothing,
      createTime = Prelude.Nothing,
      owner = Prelude.Nothing,
      partitionKeys = Prelude.Nothing,
      description = Prelude.Nothing,
      lastAccessTime = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      isRegisteredWithLakeFormation = Prelude.Nothing,
      parameters = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      name = pName_
    }

-- | If the table is a view, the original text of the view; otherwise @null@.
table_viewOriginalText :: Lens.Lens' Table (Prelude.Maybe Prelude.Text)
table_viewOriginalText = Lens.lens (\Table' {viewOriginalText} -> viewOriginalText) (\s@Table' {} a -> s {viewOriginalText = a} :: Table)

-- | The ID of the Data Catalog in which the table resides.
table_catalogId :: Lens.Lens' Table (Prelude.Maybe Prelude.Text)
table_catalogId = Lens.lens (\Table' {catalogId} -> catalogId) (\s@Table' {} a -> s {catalogId = a} :: Table)

-- | The type of this table (@EXTERNAL_TABLE@, @VIRTUAL_VIEW@, etc.).
table_tableType :: Lens.Lens' Table (Prelude.Maybe Prelude.Text)
table_tableType = Lens.lens (\Table' {tableType} -> tableType) (\s@Table' {} a -> s {tableType = a} :: Table)

-- | A storage descriptor containing information about the physical storage
-- of this table.
table_storageDescriptor :: Lens.Lens' Table (Prelude.Maybe StorageDescriptor)
table_storageDescriptor = Lens.lens (\Table' {storageDescriptor} -> storageDescriptor) (\s@Table' {} a -> s {storageDescriptor = a} :: Table)

-- | The last time that column statistics were computed for this table.
table_lastAnalyzedTime :: Lens.Lens' Table (Prelude.Maybe Prelude.UTCTime)
table_lastAnalyzedTime = Lens.lens (\Table' {lastAnalyzedTime} -> lastAnalyzedTime) (\s@Table' {} a -> s {lastAnalyzedTime = a} :: Table) Prelude.. Lens.mapping Core._Time

-- | If the table is a view, the expanded text of the view; otherwise @null@.
table_viewExpandedText :: Lens.Lens' Table (Prelude.Maybe Prelude.Text)
table_viewExpandedText = Lens.lens (\Table' {viewExpandedText} -> viewExpandedText) (\s@Table' {} a -> s {viewExpandedText = a} :: Table)

-- | A @TableIdentifier@ structure that describes a target table for resource
-- linking.
table_targetTable :: Lens.Lens' Table (Prelude.Maybe TableIdentifier)
table_targetTable = Lens.lens (\Table' {targetTable} -> targetTable) (\s@Table' {} a -> s {targetTable = a} :: Table)

-- | The retention time for this table.
table_retention :: Lens.Lens' Table (Prelude.Maybe Prelude.Natural)
table_retention = Lens.lens (\Table' {retention} -> retention) (\s@Table' {} a -> s {retention = a} :: Table)

-- | The last time that the table was updated.
table_updateTime :: Lens.Lens' Table (Prelude.Maybe Prelude.UTCTime)
table_updateTime = Lens.lens (\Table' {updateTime} -> updateTime) (\s@Table' {} a -> s {updateTime = a} :: Table) Prelude.. Lens.mapping Core._Time

-- | The time when the table definition was created in the Data Catalog.
table_createTime :: Lens.Lens' Table (Prelude.Maybe Prelude.UTCTime)
table_createTime = Lens.lens (\Table' {createTime} -> createTime) (\s@Table' {} a -> s {createTime = a} :: Table) Prelude.. Lens.mapping Core._Time

-- | The owner of the table.
table_owner :: Lens.Lens' Table (Prelude.Maybe Prelude.Text)
table_owner = Lens.lens (\Table' {owner} -> owner) (\s@Table' {} a -> s {owner = a} :: Table)

-- | A list of columns by which the table is partitioned. Only primitive
-- types are supported as partition keys.
--
-- When you create a table used by Amazon Athena, and you do not specify
-- any @partitionKeys@, you must at least set the value of @partitionKeys@
-- to an empty list. For example:
--
-- @\"PartitionKeys\": []@
table_partitionKeys :: Lens.Lens' Table (Prelude.Maybe [Column])
table_partitionKeys = Lens.lens (\Table' {partitionKeys} -> partitionKeys) (\s@Table' {} a -> s {partitionKeys = a} :: Table) Prelude.. Lens.mapping Lens._Coerce

-- | A description of the table.
table_description :: Lens.Lens' Table (Prelude.Maybe Prelude.Text)
table_description = Lens.lens (\Table' {description} -> description) (\s@Table' {} a -> s {description = a} :: Table)

-- | The last time that the table was accessed. This is usually taken from
-- HDFS, and might not be reliable.
table_lastAccessTime :: Lens.Lens' Table (Prelude.Maybe Prelude.UTCTime)
table_lastAccessTime = Lens.lens (\Table' {lastAccessTime} -> lastAccessTime) (\s@Table' {} a -> s {lastAccessTime = a} :: Table) Prelude.. Lens.mapping Core._Time

-- | The person or entity who created the table.
table_createdBy :: Lens.Lens' Table (Prelude.Maybe Prelude.Text)
table_createdBy = Lens.lens (\Table' {createdBy} -> createdBy) (\s@Table' {} a -> s {createdBy = a} :: Table)

-- | Indicates whether the table has been registered with AWS Lake Formation.
table_isRegisteredWithLakeFormation :: Lens.Lens' Table (Prelude.Maybe Prelude.Bool)
table_isRegisteredWithLakeFormation = Lens.lens (\Table' {isRegisteredWithLakeFormation} -> isRegisteredWithLakeFormation) (\s@Table' {} a -> s {isRegisteredWithLakeFormation = a} :: Table)

-- | These key-value pairs define properties associated with the table.
table_parameters :: Lens.Lens' Table (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
table_parameters = Lens.lens (\Table' {parameters} -> parameters) (\s@Table' {} a -> s {parameters = a} :: Table) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the database where the table metadata resides. For Hive
-- compatibility, this must be all lowercase.
table_databaseName :: Lens.Lens' Table (Prelude.Maybe Prelude.Text)
table_databaseName = Lens.lens (\Table' {databaseName} -> databaseName) (\s@Table' {} a -> s {databaseName = a} :: Table)

-- | The table name. For Hive compatibility, this must be entirely lowercase.
table_name :: Lens.Lens' Table Prelude.Text
table_name = Lens.lens (\Table' {name} -> name) (\s@Table' {} a -> s {name = a} :: Table)

instance Core.FromJSON Table where
  parseJSON =
    Core.withObject
      "Table"
      ( \x ->
          Table'
            Prelude.<$> (x Core..:? "ViewOriginalText")
            Prelude.<*> (x Core..:? "CatalogId")
            Prelude.<*> (x Core..:? "TableType")
            Prelude.<*> (x Core..:? "StorageDescriptor")
            Prelude.<*> (x Core..:? "LastAnalyzedTime")
            Prelude.<*> (x Core..:? "ViewExpandedText")
            Prelude.<*> (x Core..:? "TargetTable")
            Prelude.<*> (x Core..:? "Retention")
            Prelude.<*> (x Core..:? "UpdateTime")
            Prelude.<*> (x Core..:? "CreateTime")
            Prelude.<*> (x Core..:? "Owner")
            Prelude.<*> (x Core..:? "PartitionKeys" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "LastAccessTime")
            Prelude.<*> (x Core..:? "CreatedBy")
            Prelude.<*> (x Core..:? "IsRegisteredWithLakeFormation")
            Prelude.<*> (x Core..:? "Parameters" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "DatabaseName")
            Prelude.<*> (x Core..: "Name")
      )

instance Prelude.Hashable Table

instance Prelude.NFData Table
