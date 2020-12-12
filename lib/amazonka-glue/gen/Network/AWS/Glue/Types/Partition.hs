{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Partition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Partition
  ( Partition (..),

    -- * Smart constructor
    mkPartition,

    -- * Lenses
    pCreationTime,
    pValues,
    pCatalogId,
    pLastAnalyzedTime,
    pStorageDescriptor,
    pDatabaseName,
    pParameters,
    pLastAccessTime,
    pTableName,
  )
where

import Network.AWS.Glue.Types.StorageDescriptor
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a slice of table data.
--
-- /See:/ 'mkPartition' smart constructor.
data Partition = Partition'
  { creationTime ::
      Lude.Maybe Lude.Timestamp,
    values :: Lude.Maybe [Lude.Text],
    catalogId :: Lude.Maybe Lude.Text,
    lastAnalyzedTime :: Lude.Maybe Lude.Timestamp,
    storageDescriptor :: Lude.Maybe StorageDescriptor,
    databaseName :: Lude.Maybe Lude.Text,
    parameters :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    lastAccessTime :: Lude.Maybe Lude.Timestamp,
    tableName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Partition' with the minimum fields required to make a request.
--
-- * 'catalogId' - The ID of the Data Catalog in which the partition resides.
-- * 'creationTime' - The time at which the partition was created.
-- * 'databaseName' - The name of the catalog database in which to create the partition.
-- * 'lastAccessTime' - The last time at which the partition was accessed.
-- * 'lastAnalyzedTime' - The last time at which column statistics were computed for this partition.
-- * 'parameters' - These key-value pairs define partition parameters.
-- * 'storageDescriptor' - Provides information about the physical location where the partition is stored.
-- * 'tableName' - The name of the database table in which to create the partition.
-- * 'values' - The values of the partition.
mkPartition ::
  Partition
mkPartition =
  Partition'
    { creationTime = Lude.Nothing,
      values = Lude.Nothing,
      catalogId = Lude.Nothing,
      lastAnalyzedTime = Lude.Nothing,
      storageDescriptor = Lude.Nothing,
      databaseName = Lude.Nothing,
      parameters = Lude.Nothing,
      lastAccessTime = Lude.Nothing,
      tableName = Lude.Nothing
    }

-- | The time at which the partition was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pCreationTime :: Lens.Lens' Partition (Lude.Maybe Lude.Timestamp)
pCreationTime = Lens.lens (creationTime :: Partition -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: Partition)
{-# DEPRECATED pCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The values of the partition.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pValues :: Lens.Lens' Partition (Lude.Maybe [Lude.Text])
pValues = Lens.lens (values :: Partition -> Lude.Maybe [Lude.Text]) (\s a -> s {values = a} :: Partition)
{-# DEPRECATED pValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | The ID of the Data Catalog in which the partition resides.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pCatalogId :: Lens.Lens' Partition (Lude.Maybe Lude.Text)
pCatalogId = Lens.lens (catalogId :: Partition -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: Partition)
{-# DEPRECATED pCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | The last time at which column statistics were computed for this partition.
--
-- /Note:/ Consider using 'lastAnalyzedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pLastAnalyzedTime :: Lens.Lens' Partition (Lude.Maybe Lude.Timestamp)
pLastAnalyzedTime = Lens.lens (lastAnalyzedTime :: Partition -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastAnalyzedTime = a} :: Partition)
{-# DEPRECATED pLastAnalyzedTime "Use generic-lens or generic-optics with 'lastAnalyzedTime' instead." #-}

-- | Provides information about the physical location where the partition is stored.
--
-- /Note:/ Consider using 'storageDescriptor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pStorageDescriptor :: Lens.Lens' Partition (Lude.Maybe StorageDescriptor)
pStorageDescriptor = Lens.lens (storageDescriptor :: Partition -> Lude.Maybe StorageDescriptor) (\s a -> s {storageDescriptor = a} :: Partition)
{-# DEPRECATED pStorageDescriptor "Use generic-lens or generic-optics with 'storageDescriptor' instead." #-}

-- | The name of the catalog database in which to create the partition.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pDatabaseName :: Lens.Lens' Partition (Lude.Maybe Lude.Text)
pDatabaseName = Lens.lens (databaseName :: Partition -> Lude.Maybe Lude.Text) (\s a -> s {databaseName = a} :: Partition)
{-# DEPRECATED pDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | These key-value pairs define partition parameters.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pParameters :: Lens.Lens' Partition (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
pParameters = Lens.lens (parameters :: Partition -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {parameters = a} :: Partition)
{-# DEPRECATED pParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The last time at which the partition was accessed.
--
-- /Note:/ Consider using 'lastAccessTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pLastAccessTime :: Lens.Lens' Partition (Lude.Maybe Lude.Timestamp)
pLastAccessTime = Lens.lens (lastAccessTime :: Partition -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastAccessTime = a} :: Partition)
{-# DEPRECATED pLastAccessTime "Use generic-lens or generic-optics with 'lastAccessTime' instead." #-}

-- | The name of the database table in which to create the partition.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pTableName :: Lens.Lens' Partition (Lude.Maybe Lude.Text)
pTableName = Lens.lens (tableName :: Partition -> Lude.Maybe Lude.Text) (\s a -> s {tableName = a} :: Partition)
{-# DEPRECATED pTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Lude.FromJSON Partition where
  parseJSON =
    Lude.withObject
      "Partition"
      ( \x ->
          Partition'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "Values" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "CatalogId")
            Lude.<*> (x Lude..:? "LastAnalyzedTime")
            Lude.<*> (x Lude..:? "StorageDescriptor")
            Lude.<*> (x Lude..:? "DatabaseName")
            Lude.<*> (x Lude..:? "Parameters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "LastAccessTime")
            Lude.<*> (x Lude..:? "TableName")
      )
