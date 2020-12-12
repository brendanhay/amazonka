{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.JourneyLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.JourneyLimits
  ( JourneyLimits (..),

    -- * Smart constructor
    mkJourneyLimits,

    -- * Lenses
    jlMessagesPerSecond,
    jlEndpointReentryCap,
    jlDailyCap,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies limits on the messages that a journey can send and the number of times participants can enter a journey.
--
-- /See:/ 'mkJourneyLimits' smart constructor.
data JourneyLimits = JourneyLimits'
  { messagesPerSecond ::
      Lude.Maybe Lude.Int,
    endpointReentryCap :: Lude.Maybe Lude.Int,
    dailyCap :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JourneyLimits' with the minimum fields required to make a request.
--
-- * 'dailyCap' - The maximum number of messages that the journey can send to a single participant during a 24-hour period. The maximum value is 100.
-- * 'endpointReentryCap' - The maximum number of times that a participant can enter the journey. The maximum value is 100. To allow participants to enter the journey an unlimited number of times, set this value to 0.
-- * 'messagesPerSecond' - The maximum number of messages that the journey can send each second.
mkJourneyLimits ::
  JourneyLimits
mkJourneyLimits =
  JourneyLimits'
    { messagesPerSecond = Lude.Nothing,
      endpointReentryCap = Lude.Nothing,
      dailyCap = Lude.Nothing
    }

-- | The maximum number of messages that the journey can send each second.
--
-- /Note:/ Consider using 'messagesPerSecond' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jlMessagesPerSecond :: Lens.Lens' JourneyLimits (Lude.Maybe Lude.Int)
jlMessagesPerSecond = Lens.lens (messagesPerSecond :: JourneyLimits -> Lude.Maybe Lude.Int) (\s a -> s {messagesPerSecond = a} :: JourneyLimits)
{-# DEPRECATED jlMessagesPerSecond "Use generic-lens or generic-optics with 'messagesPerSecond' instead." #-}

-- | The maximum number of times that a participant can enter the journey. The maximum value is 100. To allow participants to enter the journey an unlimited number of times, set this value to 0.
--
-- /Note:/ Consider using 'endpointReentryCap' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jlEndpointReentryCap :: Lens.Lens' JourneyLimits (Lude.Maybe Lude.Int)
jlEndpointReentryCap = Lens.lens (endpointReentryCap :: JourneyLimits -> Lude.Maybe Lude.Int) (\s a -> s {endpointReentryCap = a} :: JourneyLimits)
{-# DEPRECATED jlEndpointReentryCap "Use generic-lens or generic-optics with 'endpointReentryCap' instead." #-}

-- | The maximum number of messages that the journey can send to a single participant during a 24-hour period. The maximum value is 100.
--
-- /Note:/ Consider using 'dailyCap' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jlDailyCap :: Lens.Lens' JourneyLimits (Lude.Maybe Lude.Int)
jlDailyCap = Lens.lens (dailyCap :: JourneyLimits -> Lude.Maybe Lude.Int) (\s a -> s {dailyCap = a} :: JourneyLimits)
{-# DEPRECATED jlDailyCap "Use generic-lens or generic-optics with 'dailyCap' instead." #-}

instance Lude.FromJSON JourneyLimits where
  parseJSON =
    Lude.withObject
      "JourneyLimits"
      ( \x ->
          JourneyLimits'
            Lude.<$> (x Lude..:? "MessagesPerSecond")
            Lude.<*> (x Lude..:? "EndpointReentryCap")
            Lude.<*> (x Lude..:? "DailyCap")
      )

instance Lude.ToJSON JourneyLimits where
  toJSON JourneyLimits' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MessagesPerSecond" Lude..=) Lude.<$> messagesPerSecond,
            ("EndpointReentryCap" Lude..=) Lude.<$> endpointReentryCap,
            ("DailyCap" Lude..=) Lude.<$> dailyCap
          ]
      )
