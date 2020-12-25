{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.DurationRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.DurationRange
  ( DurationRange (..),

    -- * Smart constructor
    mkDurationRange,

    -- * Lenses
    drMaxSeconds,
    drMinSeconds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | This data type is used in the 'AssessmentTemplateFilter' data type.
--
-- /See:/ 'mkDurationRange' smart constructor.
data DurationRange = DurationRange'
  { -- | The maximum value of the duration range. Must be less than or equal to 604800 seconds (1 week).
    maxSeconds :: Core.Maybe Core.Natural,
    -- | The minimum value of the duration range. Must be greater than zero.
    minSeconds :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DurationRange' value with any optional fields omitted.
mkDurationRange ::
  DurationRange
mkDurationRange =
  DurationRange'
    { maxSeconds = Core.Nothing,
      minSeconds = Core.Nothing
    }

-- | The maximum value of the duration range. Must be less than or equal to 604800 seconds (1 week).
--
-- /Note:/ Consider using 'maxSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drMaxSeconds :: Lens.Lens' DurationRange (Core.Maybe Core.Natural)
drMaxSeconds = Lens.field @"maxSeconds"
{-# DEPRECATED drMaxSeconds "Use generic-lens or generic-optics with 'maxSeconds' instead." #-}

-- | The minimum value of the duration range. Must be greater than zero.
--
-- /Note:/ Consider using 'minSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drMinSeconds :: Lens.Lens' DurationRange (Core.Maybe Core.Natural)
drMinSeconds = Lens.field @"minSeconds"
{-# DEPRECATED drMinSeconds "Use generic-lens or generic-optics with 'minSeconds' instead." #-}

instance Core.FromJSON DurationRange where
  toJSON DurationRange {..} =
    Core.object
      ( Core.catMaybes
          [ ("maxSeconds" Core..=) Core.<$> maxSeconds,
            ("minSeconds" Core..=) Core.<$> minSeconds
          ]
      )
