{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.PartitionError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.PartitionError
  ( PartitionError (..)
  -- * Smart constructor
  , mkPartitionError
  -- * Lenses
  , peErrorDetail
  , pePartitionValues
  ) where

import qualified Network.AWS.Glue.Types.ErrorDetail as Types
import qualified Network.AWS.Glue.Types.ValueString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about a partition error.
--
-- /See:/ 'mkPartitionError' smart constructor.
data PartitionError = PartitionError'
  { errorDetail :: Core.Maybe Types.ErrorDetail
    -- ^ The details about the partition error.
  , partitionValues :: Core.Maybe [Types.ValueString]
    -- ^ The values that define the partition.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PartitionError' value with any optional fields omitted.
mkPartitionError
    :: PartitionError
mkPartitionError
  = PartitionError'{errorDetail = Core.Nothing,
                    partitionValues = Core.Nothing}

-- | The details about the partition error.
--
-- /Note:/ Consider using 'errorDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peErrorDetail :: Lens.Lens' PartitionError (Core.Maybe Types.ErrorDetail)
peErrorDetail = Lens.field @"errorDetail"
{-# INLINEABLE peErrorDetail #-}
{-# DEPRECATED errorDetail "Use generic-lens or generic-optics with 'errorDetail' instead"  #-}

-- | The values that define the partition.
--
-- /Note:/ Consider using 'partitionValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pePartitionValues :: Lens.Lens' PartitionError (Core.Maybe [Types.ValueString])
pePartitionValues = Lens.field @"partitionValues"
{-# INLINEABLE pePartitionValues #-}
{-# DEPRECATED partitionValues "Use generic-lens or generic-optics with 'partitionValues' instead"  #-}

instance Core.FromJSON PartitionError where
        parseJSON
          = Core.withObject "PartitionError" Core.$
              \ x ->
                PartitionError' Core.<$>
                  (x Core..:? "ErrorDetail") Core.<*> x Core..:? "PartitionValues"
