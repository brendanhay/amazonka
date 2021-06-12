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
-- Module      : Network.AWS.Glue.Types.Partition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Partition where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.StorageDescriptor
import qualified Network.AWS.Lens as Lens

-- | Represents a slice of table data.
--
-- /See:/ 'newPartition' smart constructor.
data Partition = Partition'
  { -- | The time at which the partition was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The name of the database table in which to create the partition.
    tableName :: Core.Maybe Core.Text,
    -- | The ID of the Data Catalog in which the partition resides.
    catalogId :: Core.Maybe Core.Text,
    -- | The values of the partition.
    values :: Core.Maybe [Core.Text],
    -- | Provides information about the physical location where the partition is
    -- stored.
    storageDescriptor :: Core.Maybe StorageDescriptor,
    -- | The last time at which column statistics were computed for this
    -- partition.
    lastAnalyzedTime :: Core.Maybe Core.POSIX,
    -- | The last time at which the partition was accessed.
    lastAccessTime :: Core.Maybe Core.POSIX,
    -- | These key-value pairs define partition parameters.
    parameters :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The name of the catalog database in which to create the partition.
    databaseName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Partition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'partition_creationTime' - The time at which the partition was created.
--
-- 'tableName', 'partition_tableName' - The name of the database table in which to create the partition.
--
-- 'catalogId', 'partition_catalogId' - The ID of the Data Catalog in which the partition resides.
--
-- 'values', 'partition_values' - The values of the partition.
--
-- 'storageDescriptor', 'partition_storageDescriptor' - Provides information about the physical location where the partition is
-- stored.
--
-- 'lastAnalyzedTime', 'partition_lastAnalyzedTime' - The last time at which column statistics were computed for this
-- partition.
--
-- 'lastAccessTime', 'partition_lastAccessTime' - The last time at which the partition was accessed.
--
-- 'parameters', 'partition_parameters' - These key-value pairs define partition parameters.
--
-- 'databaseName', 'partition_databaseName' - The name of the catalog database in which to create the partition.
newPartition ::
  Partition
newPartition =
  Partition'
    { creationTime = Core.Nothing,
      tableName = Core.Nothing,
      catalogId = Core.Nothing,
      values = Core.Nothing,
      storageDescriptor = Core.Nothing,
      lastAnalyzedTime = Core.Nothing,
      lastAccessTime = Core.Nothing,
      parameters = Core.Nothing,
      databaseName = Core.Nothing
    }

-- | The time at which the partition was created.
partition_creationTime :: Lens.Lens' Partition (Core.Maybe Core.UTCTime)
partition_creationTime = Lens.lens (\Partition' {creationTime} -> creationTime) (\s@Partition' {} a -> s {creationTime = a} :: Partition) Core.. Lens.mapping Core._Time

-- | The name of the database table in which to create the partition.
partition_tableName :: Lens.Lens' Partition (Core.Maybe Core.Text)
partition_tableName = Lens.lens (\Partition' {tableName} -> tableName) (\s@Partition' {} a -> s {tableName = a} :: Partition)

-- | The ID of the Data Catalog in which the partition resides.
partition_catalogId :: Lens.Lens' Partition (Core.Maybe Core.Text)
partition_catalogId = Lens.lens (\Partition' {catalogId} -> catalogId) (\s@Partition' {} a -> s {catalogId = a} :: Partition)

-- | The values of the partition.
partition_values :: Lens.Lens' Partition (Core.Maybe [Core.Text])
partition_values = Lens.lens (\Partition' {values} -> values) (\s@Partition' {} a -> s {values = a} :: Partition) Core.. Lens.mapping Lens._Coerce

-- | Provides information about the physical location where the partition is
-- stored.
partition_storageDescriptor :: Lens.Lens' Partition (Core.Maybe StorageDescriptor)
partition_storageDescriptor = Lens.lens (\Partition' {storageDescriptor} -> storageDescriptor) (\s@Partition' {} a -> s {storageDescriptor = a} :: Partition)

-- | The last time at which column statistics were computed for this
-- partition.
partition_lastAnalyzedTime :: Lens.Lens' Partition (Core.Maybe Core.UTCTime)
partition_lastAnalyzedTime = Lens.lens (\Partition' {lastAnalyzedTime} -> lastAnalyzedTime) (\s@Partition' {} a -> s {lastAnalyzedTime = a} :: Partition) Core.. Lens.mapping Core._Time

-- | The last time at which the partition was accessed.
partition_lastAccessTime :: Lens.Lens' Partition (Core.Maybe Core.UTCTime)
partition_lastAccessTime = Lens.lens (\Partition' {lastAccessTime} -> lastAccessTime) (\s@Partition' {} a -> s {lastAccessTime = a} :: Partition) Core.. Lens.mapping Core._Time

-- | These key-value pairs define partition parameters.
partition_parameters :: Lens.Lens' Partition (Core.Maybe (Core.HashMap Core.Text Core.Text))
partition_parameters = Lens.lens (\Partition' {parameters} -> parameters) (\s@Partition' {} a -> s {parameters = a} :: Partition) Core.. Lens.mapping Lens._Coerce

-- | The name of the catalog database in which to create the partition.
partition_databaseName :: Lens.Lens' Partition (Core.Maybe Core.Text)
partition_databaseName = Lens.lens (\Partition' {databaseName} -> databaseName) (\s@Partition' {} a -> s {databaseName = a} :: Partition)

instance Core.FromJSON Partition where
  parseJSON =
    Core.withObject
      "Partition"
      ( \x ->
          Partition'
            Core.<$> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "TableName")
            Core.<*> (x Core..:? "CatalogId")
            Core.<*> (x Core..:? "Values" Core..!= Core.mempty)
            Core.<*> (x Core..:? "StorageDescriptor")
            Core.<*> (x Core..:? "LastAnalyzedTime")
            Core.<*> (x Core..:? "LastAccessTime")
            Core.<*> (x Core..:? "Parameters" Core..!= Core.mempty)
            Core.<*> (x Core..:? "DatabaseName")
      )

instance Core.Hashable Partition

instance Core.NFData Partition
