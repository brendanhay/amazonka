{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.OutputLogEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.OutputLogEvent
  ( OutputLogEvent (..),

    -- * Smart constructor
    mkOutputLogEvent,

    -- * Lenses
    oleIngestionTime,
    oleMessage,
    oleTimestamp,
  )
where

import qualified Network.AWS.CloudWatchLogs.Types.EventMessage as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a log event.
--
-- /See:/ 'mkOutputLogEvent' smart constructor.
data OutputLogEvent = OutputLogEvent'
  { -- | The time the event was ingested, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
    ingestionTime :: Core.Maybe Core.Natural,
    -- | The data contained in the log event.
    message :: Core.Maybe Types.EventMessage,
    -- | The time the event occurred, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
    timestamp :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OutputLogEvent' value with any optional fields omitted.
mkOutputLogEvent ::
  OutputLogEvent
mkOutputLogEvent =
  OutputLogEvent'
    { ingestionTime = Core.Nothing,
      message = Core.Nothing,
      timestamp = Core.Nothing
    }

-- | The time the event was ingested, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- /Note:/ Consider using 'ingestionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oleIngestionTime :: Lens.Lens' OutputLogEvent (Core.Maybe Core.Natural)
oleIngestionTime = Lens.field @"ingestionTime"
{-# DEPRECATED oleIngestionTime "Use generic-lens or generic-optics with 'ingestionTime' instead." #-}

-- | The data contained in the log event.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oleMessage :: Lens.Lens' OutputLogEvent (Core.Maybe Types.EventMessage)
oleMessage = Lens.field @"message"
{-# DEPRECATED oleMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The time the event occurred, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oleTimestamp :: Lens.Lens' OutputLogEvent (Core.Maybe Core.Natural)
oleTimestamp = Lens.field @"timestamp"
{-# DEPRECATED oleTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

instance Core.FromJSON OutputLogEvent where
  parseJSON =
    Core.withObject "OutputLogEvent" Core.$
      \x ->
        OutputLogEvent'
          Core.<$> (x Core..:? "ingestionTime")
          Core.<*> (x Core..:? "message")
          Core.<*> (x Core..:? "timestamp")
