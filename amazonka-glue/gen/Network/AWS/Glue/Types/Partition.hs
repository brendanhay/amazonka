{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.Glue.Types.StorageDescriptor
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents a slice of table data.
--
-- /See:/ 'newPartition' smart constructor.
data Partition = Partition'
  { -- | The time at which the partition was created.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | The name of the database table in which to create the partition.
    tableName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Data Catalog in which the partition resides.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The values of the partition.
    values :: Prelude.Maybe [Prelude.Text],
    -- | Provides information about the physical location where the partition is
    -- stored.
    storageDescriptor :: Prelude.Maybe StorageDescriptor,
    -- | The last time at which column statistics were computed for this
    -- partition.
    lastAnalyzedTime :: Prelude.Maybe Prelude.POSIX,
    -- | The last time at which the partition was accessed.
    lastAccessTime :: Prelude.Maybe Prelude.POSIX,
    -- | These key-value pairs define partition parameters.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the catalog database in which to create the partition.
    databaseName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { creationTime = Prelude.Nothing,
      tableName = Prelude.Nothing,
      catalogId = Prelude.Nothing,
      values = Prelude.Nothing,
      storageDescriptor = Prelude.Nothing,
      lastAnalyzedTime = Prelude.Nothing,
      lastAccessTime = Prelude.Nothing,
      parameters = Prelude.Nothing,
      databaseName = Prelude.Nothing
    }

-- | The time at which the partition was created.
partition_creationTime :: Lens.Lens' Partition (Prelude.Maybe Prelude.UTCTime)
partition_creationTime = Lens.lens (\Partition' {creationTime} -> creationTime) (\s@Partition' {} a -> s {creationTime = a} :: Partition) Prelude.. Lens.mapping Prelude._Time

-- | The name of the database table in which to create the partition.
partition_tableName :: Lens.Lens' Partition (Prelude.Maybe Prelude.Text)
partition_tableName = Lens.lens (\Partition' {tableName} -> tableName) (\s@Partition' {} a -> s {tableName = a} :: Partition)

-- | The ID of the Data Catalog in which the partition resides.
partition_catalogId :: Lens.Lens' Partition (Prelude.Maybe Prelude.Text)
partition_catalogId = Lens.lens (\Partition' {catalogId} -> catalogId) (\s@Partition' {} a -> s {catalogId = a} :: Partition)

-- | The values of the partition.
partition_values :: Lens.Lens' Partition (Prelude.Maybe [Prelude.Text])
partition_values = Lens.lens (\Partition' {values} -> values) (\s@Partition' {} a -> s {values = a} :: Partition) Prelude.. Lens.mapping Prelude._Coerce

-- | Provides information about the physical location where the partition is
-- stored.
partition_storageDescriptor :: Lens.Lens' Partition (Prelude.Maybe StorageDescriptor)
partition_storageDescriptor = Lens.lens (\Partition' {storageDescriptor} -> storageDescriptor) (\s@Partition' {} a -> s {storageDescriptor = a} :: Partition)

-- | The last time at which column statistics were computed for this
-- partition.
partition_lastAnalyzedTime :: Lens.Lens' Partition (Prelude.Maybe Prelude.UTCTime)
partition_lastAnalyzedTime = Lens.lens (\Partition' {lastAnalyzedTime} -> lastAnalyzedTime) (\s@Partition' {} a -> s {lastAnalyzedTime = a} :: Partition) Prelude.. Lens.mapping Prelude._Time

-- | The last time at which the partition was accessed.
partition_lastAccessTime :: Lens.Lens' Partition (Prelude.Maybe Prelude.UTCTime)
partition_lastAccessTime = Lens.lens (\Partition' {lastAccessTime} -> lastAccessTime) (\s@Partition' {} a -> s {lastAccessTime = a} :: Partition) Prelude.. Lens.mapping Prelude._Time

-- | These key-value pairs define partition parameters.
partition_parameters :: Lens.Lens' Partition (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
partition_parameters = Lens.lens (\Partition' {parameters} -> parameters) (\s@Partition' {} a -> s {parameters = a} :: Partition) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the catalog database in which to create the partition.
partition_databaseName :: Lens.Lens' Partition (Prelude.Maybe Prelude.Text)
partition_databaseName = Lens.lens (\Partition' {databaseName} -> databaseName) (\s@Partition' {} a -> s {databaseName = a} :: Partition)

instance Prelude.FromJSON Partition where
  parseJSON =
    Prelude.withObject
      "Partition"
      ( \x ->
          Partition'
            Prelude.<$> (x Prelude..:? "CreationTime")
            Prelude.<*> (x Prelude..:? "TableName")
            Prelude.<*> (x Prelude..:? "CatalogId")
            Prelude.<*> (x Prelude..:? "Values" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "StorageDescriptor")
            Prelude.<*> (x Prelude..:? "LastAnalyzedTime")
            Prelude.<*> (x Prelude..:? "LastAccessTime")
            Prelude.<*> ( x Prelude..:? "Parameters"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "DatabaseName")
      )

instance Prelude.Hashable Partition

instance Prelude.NFData Partition
