{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.BatchArrayProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.BatchArrayProperties
  ( BatchArrayProperties (..),

    -- * Smart constructor
    mkBatchArrayProperties,

    -- * Lenses
    bapSize,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The array properties for the submitted job, such as the size of the array. The array size can be between 2 and 10,000. If you specify array properties for a job, it becomes an array job. This parameter is used only if the target is an AWS Batch job.
--
-- /See:/ 'mkBatchArrayProperties' smart constructor.
newtype BatchArrayProperties = BatchArrayProperties'
  { -- | The size of the array, if this is an array batch job. Valid values are integers between 2 and 10,000.
    size :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BatchArrayProperties' value with any optional fields omitted.
mkBatchArrayProperties ::
  BatchArrayProperties
mkBatchArrayProperties = BatchArrayProperties' {size = Core.Nothing}

-- | The size of the array, if this is an array batch job. Valid values are integers between 2 and 10,000.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bapSize :: Lens.Lens' BatchArrayProperties (Core.Maybe Core.Int)
bapSize = Lens.field @"size"
{-# DEPRECATED bapSize "Use generic-lens or generic-optics with 'size' instead." #-}

instance Core.FromJSON BatchArrayProperties where
  toJSON BatchArrayProperties {..} =
    Core.object (Core.catMaybes [("Size" Core..=) Core.<$> size])

instance Core.FromJSON BatchArrayProperties where
  parseJSON =
    Core.withObject "BatchArrayProperties" Core.$
      \x -> BatchArrayProperties' Core.<$> (x Core..:? "Size")
