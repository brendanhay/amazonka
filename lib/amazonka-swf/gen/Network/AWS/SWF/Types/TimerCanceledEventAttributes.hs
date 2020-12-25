{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.TimerCanceledEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.TimerCanceledEventAttributes
  ( TimerCanceledEventAttributes (..),

    -- * Smart constructor
    mkTimerCanceledEventAttributes,

    -- * Lenses
    tceaTimerId,
    tceaStartedEventId,
    tceaDecisionTaskCompletedEventId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.TimerId as Types

-- | Provides the details of the @TimerCanceled@ event.
--
-- /See:/ 'mkTimerCanceledEventAttributes' smart constructor.
data TimerCanceledEventAttributes = TimerCanceledEventAttributes'
  { -- | The unique ID of the timer that was canceled.
    timerId :: Types.TimerId,
    -- | The ID of the @TimerStarted@ event that was recorded when this timer was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
    startedEventId :: Core.Integer,
    -- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @CancelTimer@ decision to cancel this timer. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
    decisionTaskCompletedEventId :: Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TimerCanceledEventAttributes' value with any optional fields omitted.
mkTimerCanceledEventAttributes ::
  -- | 'timerId'
  Types.TimerId ->
  -- | 'startedEventId'
  Core.Integer ->
  -- | 'decisionTaskCompletedEventId'
  Core.Integer ->
  TimerCanceledEventAttributes
mkTimerCanceledEventAttributes
  timerId
  startedEventId
  decisionTaskCompletedEventId =
    TimerCanceledEventAttributes'
      { timerId,
        startedEventId,
        decisionTaskCompletedEventId
      }

-- | The unique ID of the timer that was canceled.
--
-- /Note:/ Consider using 'timerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tceaTimerId :: Lens.Lens' TimerCanceledEventAttributes Types.TimerId
tceaTimerId = Lens.field @"timerId"
{-# DEPRECATED tceaTimerId "Use generic-lens or generic-optics with 'timerId' instead." #-}

-- | The ID of the @TimerStarted@ event that was recorded when this timer was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'startedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tceaStartedEventId :: Lens.Lens' TimerCanceledEventAttributes Core.Integer
tceaStartedEventId = Lens.field @"startedEventId"
{-# DEPRECATED tceaStartedEventId "Use generic-lens or generic-optics with 'startedEventId' instead." #-}

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @CancelTimer@ decision to cancel this timer. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'decisionTaskCompletedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tceaDecisionTaskCompletedEventId :: Lens.Lens' TimerCanceledEventAttributes Core.Integer
tceaDecisionTaskCompletedEventId = Lens.field @"decisionTaskCompletedEventId"
{-# DEPRECATED tceaDecisionTaskCompletedEventId "Use generic-lens or generic-optics with 'decisionTaskCompletedEventId' instead." #-}

instance Core.FromJSON TimerCanceledEventAttributes where
  parseJSON =
    Core.withObject "TimerCanceledEventAttributes" Core.$
      \x ->
        TimerCanceledEventAttributes'
          Core.<$> (x Core..: "timerId")
          Core.<*> (x Core..: "startedEventId")
          Core.<*> (x Core..: "decisionTaskCompletedEventId")
