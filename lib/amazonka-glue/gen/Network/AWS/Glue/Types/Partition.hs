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
    pCatalogId,
    pCreationTime,
    pDatabaseName,
    pLastAccessTime,
    pLastAnalyzedTime,
    pParameters,
    pStorageDescriptor,
    pTableName,
    pValues,
  )
where

import qualified Network.AWS.Glue.Types.CatalogId as Types
import qualified Network.AWS.Glue.Types.KeyString as Types
import qualified Network.AWS.Glue.Types.NameString as Types
import qualified Network.AWS.Glue.Types.ParametersMapValue as Types
import qualified Network.AWS.Glue.Types.StorageDescriptor as Types
import qualified Network.AWS.Glue.Types.ValueString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a slice of table data.
--
-- /See:/ 'mkPartition' smart constructor.
data Partition = Partition'
  { -- | The ID of the Data Catalog in which the partition resides.
    catalogId :: Core.Maybe Types.CatalogId,
    -- | The time at which the partition was created.
    creationTime :: Core.Maybe Core.NominalDiffTime,
    -- | The name of the catalog database in which to create the partition.
    databaseName :: Core.Maybe Types.NameString,
    -- | The last time at which the partition was accessed.
    lastAccessTime :: Core.Maybe Core.NominalDiffTime,
    -- | The last time at which column statistics were computed for this partition.
    lastAnalyzedTime :: Core.Maybe Core.NominalDiffTime,
    -- | These key-value pairs define partition parameters.
    parameters :: Core.Maybe (Core.HashMap Types.KeyString Types.ParametersMapValue),
    -- | Provides information about the physical location where the partition is stored.
    storageDescriptor :: Core.Maybe Types.StorageDescriptor,
    -- | The name of the database table in which to create the partition.
    tableName :: Core.Maybe Types.NameString,
    -- | The values of the partition.
    values :: Core.Maybe [Types.ValueString]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Partition' value with any optional fields omitted.
mkPartition ::
  Partition
mkPartition =
  Partition'
    { catalogId = Core.Nothing,
      creationTime = Core.Nothing,
      databaseName = Core.Nothing,
      lastAccessTime = Core.Nothing,
      lastAnalyzedTime = Core.Nothing,
      parameters = Core.Nothing,
      storageDescriptor = Core.Nothing,
      tableName = Core.Nothing,
      values = Core.Nothing
    }

-- | The ID of the Data Catalog in which the partition resides.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pCatalogId :: Lens.Lens' Partition (Core.Maybe Types.CatalogId)
pCatalogId = Lens.field @"catalogId"
{-# DEPRECATED pCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | The time at which the partition was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pCreationTime :: Lens.Lens' Partition (Core.Maybe Core.NominalDiffTime)
pCreationTime = Lens.field @"creationTime"
{-# DEPRECATED pCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The name of the catalog database in which to create the partition.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pDatabaseName :: Lens.Lens' Partition (Core.Maybe Types.NameString)
pDatabaseName = Lens.field @"databaseName"
{-# DEPRECATED pDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The last time at which the partition was accessed.
--
-- /Note:/ Consider using 'lastAccessTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pLastAccessTime :: Lens.Lens' Partition (Core.Maybe Core.NominalDiffTime)
pLastAccessTime = Lens.field @"lastAccessTime"
{-# DEPRECATED pLastAccessTime "Use generic-lens or generic-optics with 'lastAccessTime' instead." #-}

-- | The last time at which column statistics were computed for this partition.
--
-- /Note:/ Consider using 'lastAnalyzedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pLastAnalyzedTime :: Lens.Lens' Partition (Core.Maybe Core.NominalDiffTime)
pLastAnalyzedTime = Lens.field @"lastAnalyzedTime"
{-# DEPRECATED pLastAnalyzedTime "Use generic-lens or generic-optics with 'lastAnalyzedTime' instead." #-}

-- | These key-value pairs define partition parameters.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pParameters :: Lens.Lens' Partition (Core.Maybe (Core.HashMap Types.KeyString Types.ParametersMapValue))
pParameters = Lens.field @"parameters"
{-# DEPRECATED pParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | Provides information about the physical location where the partition is stored.
--
-- /Note:/ Consider using 'storageDescriptor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pStorageDescriptor :: Lens.Lens' Partition (Core.Maybe Types.StorageDescriptor)
pStorageDescriptor = Lens.field @"storageDescriptor"
{-# DEPRECATED pStorageDescriptor "Use generic-lens or generic-optics with 'storageDescriptor' instead." #-}

-- | The name of the database table in which to create the partition.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pTableName :: Lens.Lens' Partition (Core.Maybe Types.NameString)
pTableName = Lens.field @"tableName"
{-# DEPRECATED pTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | The values of the partition.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pValues :: Lens.Lens' Partition (Core.Maybe [Types.ValueString])
pValues = Lens.field @"values"
{-# DEPRECATED pValues "Use generic-lens or generic-optics with 'values' instead." #-}

instance Core.FromJSON Partition where
  parseJSON =
    Core.withObject "Partition" Core.$
      \x ->
        Partition'
          Core.<$> (x Core..:? "CatalogId")
          Core.<*> (x Core..:? "CreationTime")
          Core.<*> (x Core..:? "DatabaseName")
          Core.<*> (x Core..:? "LastAccessTime")
          Core.<*> (x Core..:? "LastAnalyzedTime")
          Core.<*> (x Core..:? "Parameters")
          Core.<*> (x Core..:? "StorageDescriptor")
          Core.<*> (x Core..:? "TableName")
          Core.<*> (x Core..:? "Values")
