{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.ArrayPropertiesSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.ArrayPropertiesSummary
  ( ArrayPropertiesSummary (..),

    -- * Smart constructor
    mkArrayPropertiesSummary,

    -- * Lenses
    apsIndex,
    apsSize,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object representing the array properties of a job.
--
-- /See:/ 'mkArrayPropertiesSummary' smart constructor.
data ArrayPropertiesSummary = ArrayPropertiesSummary'
  { -- | The job index within the array that is associated with this job. This parameter is returned for children of array jobs.
    index :: Core.Maybe Core.Int,
    -- | The size of the array job. This parameter is returned for parent array jobs.
    size :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ArrayPropertiesSummary' value with any optional fields omitted.
mkArrayPropertiesSummary ::
  ArrayPropertiesSummary
mkArrayPropertiesSummary =
  ArrayPropertiesSummary'
    { index = Core.Nothing,
      size = Core.Nothing
    }

-- | The job index within the array that is associated with this job. This parameter is returned for children of array jobs.
--
-- /Note:/ Consider using 'index' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apsIndex :: Lens.Lens' ArrayPropertiesSummary (Core.Maybe Core.Int)
apsIndex = Lens.field @"index"
{-# DEPRECATED apsIndex "Use generic-lens or generic-optics with 'index' instead." #-}

-- | The size of the array job. This parameter is returned for parent array jobs.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apsSize :: Lens.Lens' ArrayPropertiesSummary (Core.Maybe Core.Int)
apsSize = Lens.field @"size"
{-# DEPRECATED apsSize "Use generic-lens or generic-optics with 'size' instead." #-}

instance Core.FromJSON ArrayPropertiesSummary where
  parseJSON =
    Core.withObject "ArrayPropertiesSummary" Core.$
      \x ->
        ArrayPropertiesSummary'
          Core.<$> (x Core..:? "index") Core.<*> (x Core..:? "size")
