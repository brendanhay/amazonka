{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.BatchUpdatePartitionFailureEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.BatchUpdatePartitionFailureEntry
  ( BatchUpdatePartitionFailureEntry (..)
  -- * Smart constructor
  , mkBatchUpdatePartitionFailureEntry
  -- * Lenses
  , bupfeErrorDetail
  , bupfePartitionValueList
  ) where

import qualified Network.AWS.Glue.Types.ErrorDetail as Types
import qualified Network.AWS.Glue.Types.ValueString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about a batch update partition error.
--
-- /See:/ 'mkBatchUpdatePartitionFailureEntry' smart constructor.
data BatchUpdatePartitionFailureEntry = BatchUpdatePartitionFailureEntry'
  { errorDetail :: Core.Maybe Types.ErrorDetail
    -- ^ The details about the batch update partition error.
  , partitionValueList :: Core.Maybe [Types.ValueString]
    -- ^ A list of values defining the partitions.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchUpdatePartitionFailureEntry' value with any optional fields omitted.
mkBatchUpdatePartitionFailureEntry
    :: BatchUpdatePartitionFailureEntry
mkBatchUpdatePartitionFailureEntry
  = BatchUpdatePartitionFailureEntry'{errorDetail = Core.Nothing,
                                      partitionValueList = Core.Nothing}

-- | The details about the batch update partition error.
--
-- /Note:/ Consider using 'errorDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bupfeErrorDetail :: Lens.Lens' BatchUpdatePartitionFailureEntry (Core.Maybe Types.ErrorDetail)
bupfeErrorDetail = Lens.field @"errorDetail"
{-# INLINEABLE bupfeErrorDetail #-}
{-# DEPRECATED errorDetail "Use generic-lens or generic-optics with 'errorDetail' instead"  #-}

-- | A list of values defining the partitions.
--
-- /Note:/ Consider using 'partitionValueList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bupfePartitionValueList :: Lens.Lens' BatchUpdatePartitionFailureEntry (Core.Maybe [Types.ValueString])
bupfePartitionValueList = Lens.field @"partitionValueList"
{-# INLINEABLE bupfePartitionValueList #-}
{-# DEPRECATED partitionValueList "Use generic-lens or generic-optics with 'partitionValueList' instead"  #-}

instance Core.FromJSON BatchUpdatePartitionFailureEntry where
        parseJSON
          = Core.withObject "BatchUpdatePartitionFailureEntry" Core.$
              \ x ->
                BatchUpdatePartitionFailureEntry' Core.<$>
                  (x Core..:? "ErrorDetail") Core.<*> x Core..:? "PartitionValueList"
