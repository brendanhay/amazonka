{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Partition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.Partition
  ( Partition (..)
  -- * Smart constructor
  , mkPartition
  -- * Lenses
  , pCatalogId
  , pCreationTime
  , pDatabaseName
  , pLastAccessTime
  , pLastAnalyzedTime
  , pParameters
  , pStorageDescriptor
  , pTableName
  , pValues
  ) where

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
  { catalogId :: Core.Maybe Types.CatalogId
    -- ^ The ID of the Data Catalog in which the partition resides.
  , creationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time at which the partition was created.
  , databaseName :: Core.Maybe Types.NameString
    -- ^ The name of the catalog database in which to create the partition.
  , lastAccessTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The last time at which the partition was accessed.
  , lastAnalyzedTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The last time at which column statistics were computed for this partition.
  , parameters :: Core.Maybe (Core.HashMap Types.KeyString Types.ParametersMapValue)
    -- ^ These key-value pairs define partition parameters.
  , storageDescriptor :: Core.Maybe Types.StorageDescriptor
    -- ^ Provides information about the physical location where the partition is stored.
  , tableName :: Core.Maybe Types.NameString
    -- ^ The name of the database table in which to create the partition.
  , values :: Core.Maybe [Types.ValueString]
    -- ^ The values of the partition.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Partition' value with any optional fields omitted.
mkPartition
    :: Partition
mkPartition
  = Partition'{catalogId = Core.Nothing, creationTime = Core.Nothing,
               databaseName = Core.Nothing, lastAccessTime = Core.Nothing,
               lastAnalyzedTime = Core.Nothing, parameters = Core.Nothing,
               storageDescriptor = Core.Nothing, tableName = Core.Nothing,
               values = Core.Nothing}

-- | The ID of the Data Catalog in which the partition resides.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pCatalogId :: Lens.Lens' Partition (Core.Maybe Types.CatalogId)
pCatalogId = Lens.field @"catalogId"
{-# INLINEABLE pCatalogId #-}
{-# DEPRECATED catalogId "Use generic-lens or generic-optics with 'catalogId' instead"  #-}

-- | The time at which the partition was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pCreationTime :: Lens.Lens' Partition (Core.Maybe Core.NominalDiffTime)
pCreationTime = Lens.field @"creationTime"
{-# INLINEABLE pCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The name of the catalog database in which to create the partition.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pDatabaseName :: Lens.Lens' Partition (Core.Maybe Types.NameString)
pDatabaseName = Lens.field @"databaseName"
{-# INLINEABLE pDatabaseName #-}
{-# DEPRECATED databaseName "Use generic-lens or generic-optics with 'databaseName' instead"  #-}

-- | The last time at which the partition was accessed.
--
-- /Note:/ Consider using 'lastAccessTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pLastAccessTime :: Lens.Lens' Partition (Core.Maybe Core.NominalDiffTime)
pLastAccessTime = Lens.field @"lastAccessTime"
{-# INLINEABLE pLastAccessTime #-}
{-# DEPRECATED lastAccessTime "Use generic-lens or generic-optics with 'lastAccessTime' instead"  #-}

-- | The last time at which column statistics were computed for this partition.
--
-- /Note:/ Consider using 'lastAnalyzedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pLastAnalyzedTime :: Lens.Lens' Partition (Core.Maybe Core.NominalDiffTime)
pLastAnalyzedTime = Lens.field @"lastAnalyzedTime"
{-# INLINEABLE pLastAnalyzedTime #-}
{-# DEPRECATED lastAnalyzedTime "Use generic-lens or generic-optics with 'lastAnalyzedTime' instead"  #-}

-- | These key-value pairs define partition parameters.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pParameters :: Lens.Lens' Partition (Core.Maybe (Core.HashMap Types.KeyString Types.ParametersMapValue))
pParameters = Lens.field @"parameters"
{-# INLINEABLE pParameters #-}
{-# DEPRECATED parameters "Use generic-lens or generic-optics with 'parameters' instead"  #-}

-- | Provides information about the physical location where the partition is stored.
--
-- /Note:/ Consider using 'storageDescriptor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pStorageDescriptor :: Lens.Lens' Partition (Core.Maybe Types.StorageDescriptor)
pStorageDescriptor = Lens.field @"storageDescriptor"
{-# INLINEABLE pStorageDescriptor #-}
{-# DEPRECATED storageDescriptor "Use generic-lens or generic-optics with 'storageDescriptor' instead"  #-}

-- | The name of the database table in which to create the partition.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pTableName :: Lens.Lens' Partition (Core.Maybe Types.NameString)
pTableName = Lens.field @"tableName"
{-# INLINEABLE pTableName #-}
{-# DEPRECATED tableName "Use generic-lens or generic-optics with 'tableName' instead"  #-}

-- | The values of the partition.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pValues :: Lens.Lens' Partition (Core.Maybe [Types.ValueString])
pValues = Lens.field @"values"
{-# INLINEABLE pValues #-}
{-# DEPRECATED values "Use generic-lens or generic-optics with 'values' instead"  #-}

instance Core.FromJSON Partition where
        parseJSON
          = Core.withObject "Partition" Core.$
              \ x ->
                Partition' Core.<$>
                  (x Core..:? "CatalogId") Core.<*> x Core..:? "CreationTime"
                    Core.<*> x Core..:? "DatabaseName"
                    Core.<*> x Core..:? "LastAccessTime"
                    Core.<*> x Core..:? "LastAnalyzedTime"
                    Core.<*> x Core..:? "Parameters"
                    Core.<*> x Core..:? "StorageDescriptor"
                    Core.<*> x Core..:? "TableName"
                    Core.<*> x Core..:? "Values"
