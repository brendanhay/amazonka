{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.TimeRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.TimeRange
  ( TimeRange (..),

    -- * Smart constructor
    mkTimeRange,

    -- * Lenses
    trEnd,
    trStart,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a time range.
--
-- /See:/ 'mkTimeRange' smart constructor.
data TimeRange = TimeRange'
  { -- | The end time of the time range.
    end :: Core.Maybe Core.NominalDiffTime,
    -- | The start time of the time range.
    start :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'TimeRange' value with any optional fields omitted.
mkTimeRange ::
  TimeRange
mkTimeRange = TimeRange' {end = Core.Nothing, start = Core.Nothing}

-- | The end time of the time range.
--
-- /Note:/ Consider using 'end' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trEnd :: Lens.Lens' TimeRange (Core.Maybe Core.NominalDiffTime)
trEnd = Lens.field @"end"
{-# DEPRECATED trEnd "Use generic-lens or generic-optics with 'end' instead." #-}

-- | The start time of the time range.
--
-- /Note:/ Consider using 'start' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trStart :: Lens.Lens' TimeRange (Core.Maybe Core.NominalDiffTime)
trStart = Lens.field @"start"
{-# DEPRECATED trStart "Use generic-lens or generic-optics with 'start' instead." #-}

instance Core.FromJSON TimeRange where
  toJSON TimeRange {..} =
    Core.object
      ( Core.catMaybes
          [("end" Core..=) Core.<$> end, ("start" Core..=) Core.<$> start]
      )
