{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.ArrayPropertiesDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.ArrayPropertiesDetail
  ( ArrayPropertiesDetail (..),

    -- * Smart constructor
    mkArrayPropertiesDetail,

    -- * Lenses
    apdIndex,
    apdSize,
    apdStatusSummary,
  )
where

import qualified Network.AWS.Batch.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object representing the array properties of a job.
--
-- /See:/ 'mkArrayPropertiesDetail' smart constructor.
data ArrayPropertiesDetail = ArrayPropertiesDetail'
  { -- | The job index within the array that is associated with this job. This parameter is returned for array job children.
    index :: Core.Maybe Core.Int,
    -- | The size of the array job. This parameter is returned for parent array jobs.
    size :: Core.Maybe Core.Int,
    -- | A summary of the number of array job children in each available job status. This parameter is returned for parent array jobs.
    statusSummary :: Core.Maybe (Core.HashMap Types.String Core.Int)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ArrayPropertiesDetail' value with any optional fields omitted.
mkArrayPropertiesDetail ::
  ArrayPropertiesDetail
mkArrayPropertiesDetail =
  ArrayPropertiesDetail'
    { index = Core.Nothing,
      size = Core.Nothing,
      statusSummary = Core.Nothing
    }

-- | The job index within the array that is associated with this job. This parameter is returned for array job children.
--
-- /Note:/ Consider using 'index' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apdIndex :: Lens.Lens' ArrayPropertiesDetail (Core.Maybe Core.Int)
apdIndex = Lens.field @"index"
{-# DEPRECATED apdIndex "Use generic-lens or generic-optics with 'index' instead." #-}

-- | The size of the array job. This parameter is returned for parent array jobs.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apdSize :: Lens.Lens' ArrayPropertiesDetail (Core.Maybe Core.Int)
apdSize = Lens.field @"size"
{-# DEPRECATED apdSize "Use generic-lens or generic-optics with 'size' instead." #-}

-- | A summary of the number of array job children in each available job status. This parameter is returned for parent array jobs.
--
-- /Note:/ Consider using 'statusSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apdStatusSummary :: Lens.Lens' ArrayPropertiesDetail (Core.Maybe (Core.HashMap Types.String Core.Int))
apdStatusSummary = Lens.field @"statusSummary"
{-# DEPRECATED apdStatusSummary "Use generic-lens or generic-optics with 'statusSummary' instead." #-}

instance Core.FromJSON ArrayPropertiesDetail where
  parseJSON =
    Core.withObject "ArrayPropertiesDetail" Core.$
      \x ->
        ArrayPropertiesDetail'
          Core.<$> (x Core..:? "index")
          Core.<*> (x Core..:? "size")
          Core.<*> (x Core..:? "statusSummary")
