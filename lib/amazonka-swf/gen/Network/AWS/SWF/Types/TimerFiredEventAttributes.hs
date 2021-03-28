{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.TimerFiredEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.TimerFiredEventAttributes
  ( TimerFiredEventAttributes (..)
  -- * Smart constructor
  , mkTimerFiredEventAttributes
  -- * Lenses
  , tfeaTimerId
  , tfeaStartedEventId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.TimerId as Types

-- | Provides the details of the @TimerFired@ event.
--
-- /See:/ 'mkTimerFiredEventAttributes' smart constructor.
data TimerFiredEventAttributes = TimerFiredEventAttributes'
  { timerId :: Types.TimerId
    -- ^ The unique ID of the timer that fired.
  , startedEventId :: Core.Integer
    -- ^ The ID of the @TimerStarted@ event that was recorded when this timer was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TimerFiredEventAttributes' value with any optional fields omitted.
mkTimerFiredEventAttributes
    :: Types.TimerId -- ^ 'timerId'
    -> Core.Integer -- ^ 'startedEventId'
    -> TimerFiredEventAttributes
mkTimerFiredEventAttributes timerId startedEventId
  = TimerFiredEventAttributes'{timerId, startedEventId}

-- | The unique ID of the timer that fired.
--
-- /Note:/ Consider using 'timerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfeaTimerId :: Lens.Lens' TimerFiredEventAttributes Types.TimerId
tfeaTimerId = Lens.field @"timerId"
{-# INLINEABLE tfeaTimerId #-}
{-# DEPRECATED timerId "Use generic-lens or generic-optics with 'timerId' instead"  #-}

-- | The ID of the @TimerStarted@ event that was recorded when this timer was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'startedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfeaStartedEventId :: Lens.Lens' TimerFiredEventAttributes Core.Integer
tfeaStartedEventId = Lens.field @"startedEventId"
{-# INLINEABLE tfeaStartedEventId #-}
{-# DEPRECATED startedEventId "Use generic-lens or generic-optics with 'startedEventId' instead"  #-}

instance Core.FromJSON TimerFiredEventAttributes where
        parseJSON
          = Core.withObject "TimerFiredEventAttributes" Core.$
              \ x ->
                TimerFiredEventAttributes' Core.<$>
                  (x Core..: "timerId") Core.<*> x Core..: "startedEventId"
