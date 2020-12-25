{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.InputLogEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.InputLogEvent
  ( InputLogEvent (..),

    -- * Smart constructor
    mkInputLogEvent,

    -- * Lenses
    ileTimestamp,
    ileMessage,
  )
where

import qualified Network.AWS.CloudWatchLogs.Types.Message as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a log event, which is a record of activity that was recorded by the application or resource being monitored.
--
-- /See:/ 'mkInputLogEvent' smart constructor.
data InputLogEvent = InputLogEvent'
  { -- | The time the event occurred, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
    timestamp :: Core.Natural,
    -- | The raw event message.
    message :: Types.Message
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InputLogEvent' value with any optional fields omitted.
mkInputLogEvent ::
  -- | 'timestamp'
  Core.Natural ->
  -- | 'message'
  Types.Message ->
  InputLogEvent
mkInputLogEvent timestamp message =
  InputLogEvent' {timestamp, message}

-- | The time the event occurred, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ileTimestamp :: Lens.Lens' InputLogEvent Core.Natural
ileTimestamp = Lens.field @"timestamp"
{-# DEPRECATED ileTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

-- | The raw event message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ileMessage :: Lens.Lens' InputLogEvent Types.Message
ileMessage = Lens.field @"message"
{-# DEPRECATED ileMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Core.FromJSON InputLogEvent where
  toJSON InputLogEvent {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("timestamp" Core..= timestamp),
            Core.Just ("message" Core..= message)
          ]
      )
