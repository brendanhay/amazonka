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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a log event.
--
-- /See:/ 'mkOutputLogEvent' smart constructor.
data OutputLogEvent = OutputLogEvent'
  { -- | The time the event was ingested, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
    ingestionTime :: Lude.Maybe Lude.Natural,
    -- | The data contained in the log event.
    message :: Lude.Maybe Lude.Text,
    -- | The time the event occurred, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
    timestamp :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OutputLogEvent' with the minimum fields required to make a request.
--
-- * 'ingestionTime' - The time the event was ingested, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
-- * 'message' - The data contained in the log event.
-- * 'timestamp' - The time the event occurred, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
mkOutputLogEvent ::
  OutputLogEvent
mkOutputLogEvent =
  OutputLogEvent'
    { ingestionTime = Lude.Nothing,
      message = Lude.Nothing,
      timestamp = Lude.Nothing
    }

-- | The time the event was ingested, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- /Note:/ Consider using 'ingestionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oleIngestionTime :: Lens.Lens' OutputLogEvent (Lude.Maybe Lude.Natural)
oleIngestionTime = Lens.lens (ingestionTime :: OutputLogEvent -> Lude.Maybe Lude.Natural) (\s a -> s {ingestionTime = a} :: OutputLogEvent)
{-# DEPRECATED oleIngestionTime "Use generic-lens or generic-optics with 'ingestionTime' instead." #-}

-- | The data contained in the log event.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oleMessage :: Lens.Lens' OutputLogEvent (Lude.Maybe Lude.Text)
oleMessage = Lens.lens (message :: OutputLogEvent -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: OutputLogEvent)
{-# DEPRECATED oleMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The time the event occurred, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oleTimestamp :: Lens.Lens' OutputLogEvent (Lude.Maybe Lude.Natural)
oleTimestamp = Lens.lens (timestamp :: OutputLogEvent -> Lude.Maybe Lude.Natural) (\s a -> s {timestamp = a} :: OutputLogEvent)
{-# DEPRECATED oleTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

instance Lude.FromJSON OutputLogEvent where
  parseJSON =
    Lude.withObject
      "OutputLogEvent"
      ( \x ->
          OutputLogEvent'
            Lude.<$> (x Lude..:? "ingestionTime")
            Lude.<*> (x Lude..:? "message")
            Lude.<*> (x Lude..:? "timestamp")
      )
