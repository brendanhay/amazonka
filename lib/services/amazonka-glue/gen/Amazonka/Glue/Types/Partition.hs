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
-- Module      : Amazonka.Glue.Types.Partition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.Partition where

import qualified Amazonka.Core as Core
import Amazonka.Glue.Types.StorageDescriptor
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents a slice of table data.
--
-- /See:/ 'newPartition' smart constructor.
data Partition = Partition'
  { -- | The time at which the partition was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The values of the partition.
    values :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the Data Catalog in which the partition resides.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The last time at which column statistics were computed for this
    -- partition.
    lastAnalyzedTime :: Prelude.Maybe Core.POSIX,
    -- | Provides information about the physical location where the partition is
    -- stored.
    storageDescriptor :: Prelude.Maybe StorageDescriptor,
    -- | The name of the catalog database in which to create the partition.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | These key-value pairs define partition parameters.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The last time at which the partition was accessed.
    lastAccessTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the database table in which to create the partition.
    tableName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'values', 'partition_values' - The values of the partition.
--
-- 'catalogId', 'partition_catalogId' - The ID of the Data Catalog in which the partition resides.
--
-- 'lastAnalyzedTime', 'partition_lastAnalyzedTime' - The last time at which column statistics were computed for this
-- partition.
--
-- 'storageDescriptor', 'partition_storageDescriptor' - Provides information about the physical location where the partition is
-- stored.
--
-- 'databaseName', 'partition_databaseName' - The name of the catalog database in which to create the partition.
--
-- 'parameters', 'partition_parameters' - These key-value pairs define partition parameters.
--
-- 'lastAccessTime', 'partition_lastAccessTime' - The last time at which the partition was accessed.
--
-- 'tableName', 'partition_tableName' - The name of the database table in which to create the partition.
newPartition ::
  Partition
newPartition =
  Partition'
    { creationTime = Prelude.Nothing,
      values = Prelude.Nothing,
      catalogId = Prelude.Nothing,
      lastAnalyzedTime = Prelude.Nothing,
      storageDescriptor = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      parameters = Prelude.Nothing,
      lastAccessTime = Prelude.Nothing,
      tableName = Prelude.Nothing
    }

-- | The time at which the partition was created.
partition_creationTime :: Lens.Lens' Partition (Prelude.Maybe Prelude.UTCTime)
partition_creationTime = Lens.lens (\Partition' {creationTime} -> creationTime) (\s@Partition' {} a -> s {creationTime = a} :: Partition) Prelude.. Lens.mapping Core._Time

-- | The values of the partition.
partition_values :: Lens.Lens' Partition (Prelude.Maybe [Prelude.Text])
partition_values = Lens.lens (\Partition' {values} -> values) (\s@Partition' {} a -> s {values = a} :: Partition) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Data Catalog in which the partition resides.
partition_catalogId :: Lens.Lens' Partition (Prelude.Maybe Prelude.Text)
partition_catalogId = Lens.lens (\Partition' {catalogId} -> catalogId) (\s@Partition' {} a -> s {catalogId = a} :: Partition)

-- | The last time at which column statistics were computed for this
-- partition.
partition_lastAnalyzedTime :: Lens.Lens' Partition (Prelude.Maybe Prelude.UTCTime)
partition_lastAnalyzedTime = Lens.lens (\Partition' {lastAnalyzedTime} -> lastAnalyzedTime) (\s@Partition' {} a -> s {lastAnalyzedTime = a} :: Partition) Prelude.. Lens.mapping Core._Time

-- | Provides information about the physical location where the partition is
-- stored.
partition_storageDescriptor :: Lens.Lens' Partition (Prelude.Maybe StorageDescriptor)
partition_storageDescriptor = Lens.lens (\Partition' {storageDescriptor} -> storageDescriptor) (\s@Partition' {} a -> s {storageDescriptor = a} :: Partition)

-- | The name of the catalog database in which to create the partition.
partition_databaseName :: Lens.Lens' Partition (Prelude.Maybe Prelude.Text)
partition_databaseName = Lens.lens (\Partition' {databaseName} -> databaseName) (\s@Partition' {} a -> s {databaseName = a} :: Partition)

-- | These key-value pairs define partition parameters.
partition_parameters :: Lens.Lens' Partition (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
partition_parameters = Lens.lens (\Partition' {parameters} -> parameters) (\s@Partition' {} a -> s {parameters = a} :: Partition) Prelude.. Lens.mapping Lens.coerced

-- | The last time at which the partition was accessed.
partition_lastAccessTime :: Lens.Lens' Partition (Prelude.Maybe Prelude.UTCTime)
partition_lastAccessTime = Lens.lens (\Partition' {lastAccessTime} -> lastAccessTime) (\s@Partition' {} a -> s {lastAccessTime = a} :: Partition) Prelude.. Lens.mapping Core._Time

-- | The name of the database table in which to create the partition.
partition_tableName :: Lens.Lens' Partition (Prelude.Maybe Prelude.Text)
partition_tableName = Lens.lens (\Partition' {tableName} -> tableName) (\s@Partition' {} a -> s {tableName = a} :: Partition)

instance Core.FromJSON Partition where
  parseJSON =
    Core.withObject
      "Partition"
      ( \x ->
          Partition'
            Prelude.<$> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "Values" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "CatalogId")
            Prelude.<*> (x Core..:? "LastAnalyzedTime")
            Prelude.<*> (x Core..:? "StorageDescriptor")
            Prelude.<*> (x Core..:? "DatabaseName")
            Prelude.<*> (x Core..:? "Parameters" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "LastAccessTime")
            Prelude.<*> (x Core..:? "TableName")
      )

instance Prelude.Hashable Partition where
  hashWithSalt salt' Partition' {..} =
    salt' `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` lastAccessTime
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` storageDescriptor
      `Prelude.hashWithSalt` lastAnalyzedTime
      `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` values
      `Prelude.hashWithSalt` creationTime

instance Prelude.NFData Partition where
  rnf Partition' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf lastAccessTime
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf storageDescriptor
      `Prelude.seq` Prelude.rnf lastAnalyzedTime
      `Prelude.seq` Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf values
