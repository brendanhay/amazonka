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
import qualified Network.AWS.Prelude as Lude

-- | Provides the details of the @TimerCanceled@ event.
--
-- /See:/ 'mkTimerCanceledEventAttributes' smart constructor.
data TimerCanceledEventAttributes = TimerCanceledEventAttributes'
  { -- | The unique ID of the timer that was canceled.
    timerId :: Lude.Text,
    -- | The ID of the @TimerStarted@ event that was recorded when this timer was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
    startedEventId :: Lude.Integer,
    -- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @CancelTimer@ decision to cancel this timer. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
    decisionTaskCompletedEventId :: Lude.Integer
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TimerCanceledEventAttributes' with the minimum fields required to make a request.
--
-- * 'timerId' - The unique ID of the timer that was canceled.
-- * 'startedEventId' - The ID of the @TimerStarted@ event that was recorded when this timer was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
-- * 'decisionTaskCompletedEventId' - The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @CancelTimer@ decision to cancel this timer. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
mkTimerCanceledEventAttributes ::
  -- | 'timerId'
  Lude.Text ->
  -- | 'startedEventId'
  Lude.Integer ->
  -- | 'decisionTaskCompletedEventId'
  Lude.Integer ->
  TimerCanceledEventAttributes
mkTimerCanceledEventAttributes
  pTimerId_
  pStartedEventId_
  pDecisionTaskCompletedEventId_ =
    TimerCanceledEventAttributes'
      { timerId = pTimerId_,
        startedEventId = pStartedEventId_,
        decisionTaskCompletedEventId = pDecisionTaskCompletedEventId_
      }

-- | The unique ID of the timer that was canceled.
--
-- /Note:/ Consider using 'timerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tceaTimerId :: Lens.Lens' TimerCanceledEventAttributes Lude.Text
tceaTimerId = Lens.lens (timerId :: TimerCanceledEventAttributes -> Lude.Text) (\s a -> s {timerId = a} :: TimerCanceledEventAttributes)
{-# DEPRECATED tceaTimerId "Use generic-lens or generic-optics with 'timerId' instead." #-}

-- | The ID of the @TimerStarted@ event that was recorded when this timer was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'startedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tceaStartedEventId :: Lens.Lens' TimerCanceledEventAttributes Lude.Integer
tceaStartedEventId = Lens.lens (startedEventId :: TimerCanceledEventAttributes -> Lude.Integer) (\s a -> s {startedEventId = a} :: TimerCanceledEventAttributes)
{-# DEPRECATED tceaStartedEventId "Use generic-lens or generic-optics with 'startedEventId' instead." #-}

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @CancelTimer@ decision to cancel this timer. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'decisionTaskCompletedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tceaDecisionTaskCompletedEventId :: Lens.Lens' TimerCanceledEventAttributes Lude.Integer
tceaDecisionTaskCompletedEventId = Lens.lens (decisionTaskCompletedEventId :: TimerCanceledEventAttributes -> Lude.Integer) (\s a -> s {decisionTaskCompletedEventId = a} :: TimerCanceledEventAttributes)
{-# DEPRECATED tceaDecisionTaskCompletedEventId "Use generic-lens or generic-optics with 'decisionTaskCompletedEventId' instead." #-}

instance Lude.FromJSON TimerCanceledEventAttributes where
  parseJSON =
    Lude.withObject
      "TimerCanceledEventAttributes"
      ( \x ->
          TimerCanceledEventAttributes'
            Lude.<$> (x Lude..: "timerId")
            Lude.<*> (x Lude..: "startedEventId")
            Lude.<*> (x Lude..: "decisionTaskCompletedEventId")
      )
