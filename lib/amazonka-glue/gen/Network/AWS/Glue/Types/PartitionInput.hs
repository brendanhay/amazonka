{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.PartitionInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.PartitionInput
  ( PartitionInput (..)
  -- * Smart constructor
  , mkPartitionInput
  -- * Lenses
  , piLastAccessTime
  , piLastAnalyzedTime
  , piParameters
  , piStorageDescriptor
  , piValues
  ) where

import qualified Network.AWS.Glue.Types.KeyString as Types
import qualified Network.AWS.Glue.Types.ParametersMapValue as Types
import qualified Network.AWS.Glue.Types.StorageDescriptor as Types
import qualified Network.AWS.Glue.Types.ValueString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The structure used to create and update a partition.
--
-- /See:/ 'mkPartitionInput' smart constructor.
data PartitionInput = PartitionInput'
  { lastAccessTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The last time at which the partition was accessed.
  , lastAnalyzedTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The last time at which column statistics were computed for this partition.
  , parameters :: Core.Maybe (Core.HashMap Types.KeyString Types.ParametersMapValue)
    -- ^ These key-value pairs define partition parameters.
  , storageDescriptor :: Core.Maybe Types.StorageDescriptor
    -- ^ Provides information about the physical location where the partition is stored.
  , values :: Core.Maybe [Types.ValueString]
    -- ^ The values of the partition. Although this parameter is not required by the SDK, you must specify this parameter for a valid input.
--
-- The values for the keys for the new partition must be passed as an array of String objects that must be ordered in the same order as the partition keys appearing in the Amazon S3 prefix. Otherwise AWS Glue will add the values to the wrong keys.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'PartitionInput' value with any optional fields omitted.
mkPartitionInput
    :: PartitionInput
mkPartitionInput
  = PartitionInput'{lastAccessTime = Core.Nothing,
                    lastAnalyzedTime = Core.Nothing, parameters = Core.Nothing,
                    storageDescriptor = Core.Nothing, values = Core.Nothing}

-- | The last time at which the partition was accessed.
--
-- /Note:/ Consider using 'lastAccessTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piLastAccessTime :: Lens.Lens' PartitionInput (Core.Maybe Core.NominalDiffTime)
piLastAccessTime = Lens.field @"lastAccessTime"
{-# INLINEABLE piLastAccessTime #-}
{-# DEPRECATED lastAccessTime "Use generic-lens or generic-optics with 'lastAccessTime' instead"  #-}

-- | The last time at which column statistics were computed for this partition.
--
-- /Note:/ Consider using 'lastAnalyzedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piLastAnalyzedTime :: Lens.Lens' PartitionInput (Core.Maybe Core.NominalDiffTime)
piLastAnalyzedTime = Lens.field @"lastAnalyzedTime"
{-# INLINEABLE piLastAnalyzedTime #-}
{-# DEPRECATED lastAnalyzedTime "Use generic-lens or generic-optics with 'lastAnalyzedTime' instead"  #-}

-- | These key-value pairs define partition parameters.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piParameters :: Lens.Lens' PartitionInput (Core.Maybe (Core.HashMap Types.KeyString Types.ParametersMapValue))
piParameters = Lens.field @"parameters"
{-# INLINEABLE piParameters #-}
{-# DEPRECATED parameters "Use generic-lens or generic-optics with 'parameters' instead"  #-}

-- | Provides information about the physical location where the partition is stored.
--
-- /Note:/ Consider using 'storageDescriptor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piStorageDescriptor :: Lens.Lens' PartitionInput (Core.Maybe Types.StorageDescriptor)
piStorageDescriptor = Lens.field @"storageDescriptor"
{-# INLINEABLE piStorageDescriptor #-}
{-# DEPRECATED storageDescriptor "Use generic-lens or generic-optics with 'storageDescriptor' instead"  #-}

-- | The values of the partition. Although this parameter is not required by the SDK, you must specify this parameter for a valid input.
--
-- The values for the keys for the new partition must be passed as an array of String objects that must be ordered in the same order as the partition keys appearing in the Amazon S3 prefix. Otherwise AWS Glue will add the values to the wrong keys.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piValues :: Lens.Lens' PartitionInput (Core.Maybe [Types.ValueString])
piValues = Lens.field @"values"
{-# INLINEABLE piValues #-}
{-# DEPRECATED values "Use generic-lens or generic-optics with 'values' instead"  #-}

instance Core.FromJSON PartitionInput where
        toJSON PartitionInput{..}
          = Core.object
              (Core.catMaybes
                 [("LastAccessTime" Core..=) Core.<$> lastAccessTime,
                  ("LastAnalyzedTime" Core..=) Core.<$> lastAnalyzedTime,
                  ("Parameters" Core..=) Core.<$> parameters,
                  ("StorageDescriptor" Core..=) Core.<$> storageDescriptor,
                  ("Values" Core..=) Core.<$> values])
