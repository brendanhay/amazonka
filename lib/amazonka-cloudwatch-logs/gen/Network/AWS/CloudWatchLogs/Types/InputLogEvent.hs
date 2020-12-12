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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a log event, which is a record of activity that was recorded by the application or resource being monitored.
--
-- /See:/ 'mkInputLogEvent' smart constructor.
data InputLogEvent = InputLogEvent'
  { timestamp :: Lude.Natural,
    message :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InputLogEvent' with the minimum fields required to make a request.
--
-- * 'message' - The raw event message.
-- * 'timestamp' - The time the event occurred, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
mkInputLogEvent ::
  -- | 'timestamp'
  Lude.Natural ->
  -- | 'message'
  Lude.Text ->
  InputLogEvent
mkInputLogEvent pTimestamp_ pMessage_ =
  InputLogEvent' {timestamp = pTimestamp_, message = pMessage_}

-- | The time the event occurred, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ileTimestamp :: Lens.Lens' InputLogEvent Lude.Natural
ileTimestamp = Lens.lens (timestamp :: InputLogEvent -> Lude.Natural) (\s a -> s {timestamp = a} :: InputLogEvent)
{-# DEPRECATED ileTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

-- | The raw event message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ileMessage :: Lens.Lens' InputLogEvent Lude.Text
ileMessage = Lens.lens (message :: InputLogEvent -> Lude.Text) (\s a -> s {message = a} :: InputLogEvent)
{-# DEPRECATED ileMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.ToJSON InputLogEvent where
  toJSON InputLogEvent' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("timestamp" Lude..= timestamp),
            Lude.Just ("message" Lude..= message)
          ]
      )
