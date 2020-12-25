{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.QuietTime
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.QuietTime
  ( QuietTime (..),

    -- * Smart constructor
    mkQuietTime,

    -- * Lenses
    qtEnd,
    qtStart,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the start and end times that define a time range when messages aren't sent to endpoints.
--
-- /See:/ 'mkQuietTime' smart constructor.
data QuietTime = QuietTime'
  { -- | The specific time when quiet time ends. This value has to use 24-hour notation and be in HH:MM format, where HH is the hour (with a leading zero, if applicable) and MM is the minutes. For example, use 02:30 to represent 2:30 AM, or 14:30 to represent 2:30 PM.
    end :: Core.Maybe Core.Text,
    -- | The specific time when quiet time begins. This value has to use 24-hour notation and be in HH:MM format, where HH is the hour (with a leading zero, if applicable) and MM is the minutes. For example, use 02:30 to represent 2:30 AM, or 14:30 to represent 2:30 PM.
    start :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'QuietTime' value with any optional fields omitted.
mkQuietTime ::
  QuietTime
mkQuietTime = QuietTime' {end = Core.Nothing, start = Core.Nothing}

-- | The specific time when quiet time ends. This value has to use 24-hour notation and be in HH:MM format, where HH is the hour (with a leading zero, if applicable) and MM is the minutes. For example, use 02:30 to represent 2:30 AM, or 14:30 to represent 2:30 PM.
--
-- /Note:/ Consider using 'end' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qtEnd :: Lens.Lens' QuietTime (Core.Maybe Core.Text)
qtEnd = Lens.field @"end"
{-# DEPRECATED qtEnd "Use generic-lens or generic-optics with 'end' instead." #-}

-- | The specific time when quiet time begins. This value has to use 24-hour notation and be in HH:MM format, where HH is the hour (with a leading zero, if applicable) and MM is the minutes. For example, use 02:30 to represent 2:30 AM, or 14:30 to represent 2:30 PM.
--
-- /Note:/ Consider using 'start' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qtStart :: Lens.Lens' QuietTime (Core.Maybe Core.Text)
qtStart = Lens.field @"start"
{-# DEPRECATED qtStart "Use generic-lens or generic-optics with 'start' instead." #-}

instance Core.FromJSON QuietTime where
  toJSON QuietTime {..} =
    Core.object
      ( Core.catMaybes
          [("End" Core..=) Core.<$> end, ("Start" Core..=) Core.<$> start]
      )

instance Core.FromJSON QuietTime where
  parseJSON =
    Core.withObject "QuietTime" Core.$
      \x ->
        QuietTime'
          Core.<$> (x Core..:? "End") Core.<*> (x Core..:? "Start")
