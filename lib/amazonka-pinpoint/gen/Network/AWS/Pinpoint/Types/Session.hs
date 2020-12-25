{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.Session
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.Session
  ( Session (..),

    -- * Smart constructor
    mkSession,

    -- * Lenses
    sfStartTimestamp,
    sfId,
    sfDuration,
    sfStopTimestamp,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information about a session.
--
-- /See:/ 'mkSession' smart constructor.
data Session = Session'
  { -- | The date and time when the session began.
    startTimestamp :: Core.Text,
    -- | The unique identifier for the session.
    id :: Core.Text,
    -- | The duration of the session, in milliseconds.
    duration :: Core.Maybe Core.Int,
    -- | The date and time when the session ended.
    stopTimestamp :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Session' value with any optional fields omitted.
mkSession ::
  -- | 'startTimestamp'
  Core.Text ->
  -- | 'id'
  Core.Text ->
  Session
mkSession startTimestamp id =
  Session'
    { startTimestamp,
      id,
      duration = Core.Nothing,
      stopTimestamp = Core.Nothing
    }

-- | The date and time when the session began.
--
-- /Note:/ Consider using 'startTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfStartTimestamp :: Lens.Lens' Session Core.Text
sfStartTimestamp = Lens.field @"startTimestamp"
{-# DEPRECATED sfStartTimestamp "Use generic-lens or generic-optics with 'startTimestamp' instead." #-}

-- | The unique identifier for the session.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfId :: Lens.Lens' Session Core.Text
sfId = Lens.field @"id"
{-# DEPRECATED sfId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The duration of the session, in milliseconds.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfDuration :: Lens.Lens' Session (Core.Maybe Core.Int)
sfDuration = Lens.field @"duration"
{-# DEPRECATED sfDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

-- | The date and time when the session ended.
--
-- /Note:/ Consider using 'stopTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfStopTimestamp :: Lens.Lens' Session (Core.Maybe Core.Text)
sfStopTimestamp = Lens.field @"stopTimestamp"
{-# DEPRECATED sfStopTimestamp "Use generic-lens or generic-optics with 'stopTimestamp' instead." #-}

instance Core.FromJSON Session where
  toJSON Session {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("StartTimestamp" Core..= startTimestamp),
            Core.Just ("Id" Core..= id),
            ("Duration" Core..=) Core.<$> duration,
            ("StopTimestamp" Core..=) Core.<$> stopTimestamp
          ]
      )
