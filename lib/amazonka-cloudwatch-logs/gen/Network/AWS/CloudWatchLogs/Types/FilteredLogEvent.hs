{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.FilteredLogEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.FilteredLogEvent
  ( FilteredLogEvent (..),

    -- * Smart constructor
    mkFilteredLogEvent,

    -- * Lenses
    fleEventId,
    fleIngestionTime,
    fleLogStreamName,
    fleMessage,
    fleTimestamp,
  )
where

import qualified Network.AWS.CloudWatchLogs.Types.EventId as Types
import qualified Network.AWS.CloudWatchLogs.Types.LogStreamName as Types
import qualified Network.AWS.CloudWatchLogs.Types.Message as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a matched event.
--
-- /See:/ 'mkFilteredLogEvent' smart constructor.
data FilteredLogEvent = FilteredLogEvent'
  { -- | The ID of the event.
    eventId :: Core.Maybe Types.EventId,
    -- | The time the event was ingested, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
    ingestionTime :: Core.Maybe Core.Natural,
    -- | The name of the log stream to which this event belongs.
    logStreamName :: Core.Maybe Types.LogStreamName,
    -- | The data contained in the log event.
    message :: Core.Maybe Types.Message,
    -- | The time the event occurred, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
    timestamp :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FilteredLogEvent' value with any optional fields omitted.
mkFilteredLogEvent ::
  FilteredLogEvent
mkFilteredLogEvent =
  FilteredLogEvent'
    { eventId = Core.Nothing,
      ingestionTime = Core.Nothing,
      logStreamName = Core.Nothing,
      message = Core.Nothing,
      timestamp = Core.Nothing
    }

-- | The ID of the event.
--
-- /Note:/ Consider using 'eventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fleEventId :: Lens.Lens' FilteredLogEvent (Core.Maybe Types.EventId)
fleEventId = Lens.field @"eventId"
{-# DEPRECATED fleEventId "Use generic-lens or generic-optics with 'eventId' instead." #-}

-- | The time the event was ingested, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- /Note:/ Consider using 'ingestionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fleIngestionTime :: Lens.Lens' FilteredLogEvent (Core.Maybe Core.Natural)
fleIngestionTime = Lens.field @"ingestionTime"
{-# DEPRECATED fleIngestionTime "Use generic-lens or generic-optics with 'ingestionTime' instead." #-}

-- | The name of the log stream to which this event belongs.
--
-- /Note:/ Consider using 'logStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fleLogStreamName :: Lens.Lens' FilteredLogEvent (Core.Maybe Types.LogStreamName)
fleLogStreamName = Lens.field @"logStreamName"
{-# DEPRECATED fleLogStreamName "Use generic-lens or generic-optics with 'logStreamName' instead." #-}

-- | The data contained in the log event.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fleMessage :: Lens.Lens' FilteredLogEvent (Core.Maybe Types.Message)
fleMessage = Lens.field @"message"
{-# DEPRECATED fleMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The time the event occurred, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fleTimestamp :: Lens.Lens' FilteredLogEvent (Core.Maybe Core.Natural)
fleTimestamp = Lens.field @"timestamp"
{-# DEPRECATED fleTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

instance Core.FromJSON FilteredLogEvent where
  parseJSON =
    Core.withObject "FilteredLogEvent" Core.$
      \x ->
        FilteredLogEvent'
          Core.<$> (x Core..:? "eventId")
          Core.<*> (x Core..:? "ingestionTime")
          Core.<*> (x Core..:? "logStreamName")
          Core.<*> (x Core..:? "message")
          Core.<*> (x Core..:? "timestamp")
