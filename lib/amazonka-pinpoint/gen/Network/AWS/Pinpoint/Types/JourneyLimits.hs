{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.JourneyLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.JourneyLimits
  ( JourneyLimits (..)
  -- * Smart constructor
  , mkJourneyLimits
  -- * Lenses
  , jlDailyCap
  , jlEndpointReentryCap
  , jlMessagesPerSecond
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies limits on the messages that a journey can send and the number of times participants can enter a journey.
--
-- /See:/ 'mkJourneyLimits' smart constructor.
data JourneyLimits = JourneyLimits'
  { dailyCap :: Core.Maybe Core.Int
    -- ^ The maximum number of messages that the journey can send to a single participant during a 24-hour period. The maximum value is 100.
  , endpointReentryCap :: Core.Maybe Core.Int
    -- ^ The maximum number of times that a participant can enter the journey. The maximum value is 100. To allow participants to enter the journey an unlimited number of times, set this value to 0.
  , messagesPerSecond :: Core.Maybe Core.Int
    -- ^ The maximum number of messages that the journey can send each second.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'JourneyLimits' value with any optional fields omitted.
mkJourneyLimits
    :: JourneyLimits
mkJourneyLimits
  = JourneyLimits'{dailyCap = Core.Nothing,
                   endpointReentryCap = Core.Nothing,
                   messagesPerSecond = Core.Nothing}

-- | The maximum number of messages that the journey can send to a single participant during a 24-hour period. The maximum value is 100.
--
-- /Note:/ Consider using 'dailyCap' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jlDailyCap :: Lens.Lens' JourneyLimits (Core.Maybe Core.Int)
jlDailyCap = Lens.field @"dailyCap"
{-# INLINEABLE jlDailyCap #-}
{-# DEPRECATED dailyCap "Use generic-lens or generic-optics with 'dailyCap' instead"  #-}

-- | The maximum number of times that a participant can enter the journey. The maximum value is 100. To allow participants to enter the journey an unlimited number of times, set this value to 0.
--
-- /Note:/ Consider using 'endpointReentryCap' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jlEndpointReentryCap :: Lens.Lens' JourneyLimits (Core.Maybe Core.Int)
jlEndpointReentryCap = Lens.field @"endpointReentryCap"
{-# INLINEABLE jlEndpointReentryCap #-}
{-# DEPRECATED endpointReentryCap "Use generic-lens or generic-optics with 'endpointReentryCap' instead"  #-}

-- | The maximum number of messages that the journey can send each second.
--
-- /Note:/ Consider using 'messagesPerSecond' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jlMessagesPerSecond :: Lens.Lens' JourneyLimits (Core.Maybe Core.Int)
jlMessagesPerSecond = Lens.field @"messagesPerSecond"
{-# INLINEABLE jlMessagesPerSecond #-}
{-# DEPRECATED messagesPerSecond "Use generic-lens or generic-optics with 'messagesPerSecond' instead"  #-}

instance Core.FromJSON JourneyLimits where
        toJSON JourneyLimits{..}
          = Core.object
              (Core.catMaybes
                 [("DailyCap" Core..=) Core.<$> dailyCap,
                  ("EndpointReentryCap" Core..=) Core.<$> endpointReentryCap,
                  ("MessagesPerSecond" Core..=) Core.<$> messagesPerSecond])

instance Core.FromJSON JourneyLimits where
        parseJSON
          = Core.withObject "JourneyLimits" Core.$
              \ x ->
                JourneyLimits' Core.<$>
                  (x Core..:? "DailyCap") Core.<*> x Core..:? "EndpointReentryCap"
                    Core.<*> x Core..:? "MessagesPerSecond"
