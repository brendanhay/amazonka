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
    fleIngestionTime,
    fleLogStreamName,
    fleMessage,
    fleTimestamp,
    fleEventId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a matched event.
--
-- /See:/ 'mkFilteredLogEvent' smart constructor.
data FilteredLogEvent = FilteredLogEvent'
  { ingestionTime ::
      Lude.Maybe Lude.Natural,
    logStreamName :: Lude.Maybe Lude.Text,
    message :: Lude.Maybe Lude.Text,
    timestamp :: Lude.Maybe Lude.Natural,
    eventId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FilteredLogEvent' with the minimum fields required to make a request.
--
-- * 'eventId' - The ID of the event.
-- * 'ingestionTime' - The time the event was ingested, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
-- * 'logStreamName' - The name of the log stream to which this event belongs.
-- * 'message' - The data contained in the log event.
-- * 'timestamp' - The time the event occurred, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
mkFilteredLogEvent ::
  FilteredLogEvent
mkFilteredLogEvent =
  FilteredLogEvent'
    { ingestionTime = Lude.Nothing,
      logStreamName = Lude.Nothing,
      message = Lude.Nothing,
      timestamp = Lude.Nothing,
      eventId = Lude.Nothing
    }

-- | The time the event was ingested, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- /Note:/ Consider using 'ingestionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fleIngestionTime :: Lens.Lens' FilteredLogEvent (Lude.Maybe Lude.Natural)
fleIngestionTime = Lens.lens (ingestionTime :: FilteredLogEvent -> Lude.Maybe Lude.Natural) (\s a -> s {ingestionTime = a} :: FilteredLogEvent)
{-# DEPRECATED fleIngestionTime "Use generic-lens or generic-optics with 'ingestionTime' instead." #-}

-- | The name of the log stream to which this event belongs.
--
-- /Note:/ Consider using 'logStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fleLogStreamName :: Lens.Lens' FilteredLogEvent (Lude.Maybe Lude.Text)
fleLogStreamName = Lens.lens (logStreamName :: FilteredLogEvent -> Lude.Maybe Lude.Text) (\s a -> s {logStreamName = a} :: FilteredLogEvent)
{-# DEPRECATED fleLogStreamName "Use generic-lens or generic-optics with 'logStreamName' instead." #-}

-- | The data contained in the log event.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fleMessage :: Lens.Lens' FilteredLogEvent (Lude.Maybe Lude.Text)
fleMessage = Lens.lens (message :: FilteredLogEvent -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: FilteredLogEvent)
{-# DEPRECATED fleMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The time the event occurred, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fleTimestamp :: Lens.Lens' FilteredLogEvent (Lude.Maybe Lude.Natural)
fleTimestamp = Lens.lens (timestamp :: FilteredLogEvent -> Lude.Maybe Lude.Natural) (\s a -> s {timestamp = a} :: FilteredLogEvent)
{-# DEPRECATED fleTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

-- | The ID of the event.
--
-- /Note:/ Consider using 'eventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fleEventId :: Lens.Lens' FilteredLogEvent (Lude.Maybe Lude.Text)
fleEventId = Lens.lens (eventId :: FilteredLogEvent -> Lude.Maybe Lude.Text) (\s a -> s {eventId = a} :: FilteredLogEvent)
{-# DEPRECATED fleEventId "Use generic-lens or generic-optics with 'eventId' instead." #-}

instance Lude.FromJSON FilteredLogEvent where
  parseJSON =
    Lude.withObject
      "FilteredLogEvent"
      ( \x ->
          FilteredLogEvent'
            Lude.<$> (x Lude..:? "ingestionTime")
            Lude.<*> (x Lude..:? "logStreamName")
            Lude.<*> (x Lude..:? "message")
            Lude.<*> (x Lude..:? "timestamp")
            Lude.<*> (x Lude..:? "eventId")
      )
