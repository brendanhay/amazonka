{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.TimerStartedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.TimerStartedEventAttributes
  ( TimerStartedEventAttributes (..),

    -- * Smart constructor
    mkTimerStartedEventAttributes,

    -- * Lenses
    tseaControl,
    tseaTimerId,
    tseaStartToFireTimeout,
    tseaDecisionTaskCompletedEventId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides the details of the @TimerStarted@ event.
--
-- /See:/ 'mkTimerStartedEventAttributes' smart constructor.
data TimerStartedEventAttributes = TimerStartedEventAttributes'
  { control ::
      Lude.Maybe Lude.Text,
    timerId :: Lude.Text,
    startToFireTimeout :: Lude.Text,
    decisionTaskCompletedEventId ::
      Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TimerStartedEventAttributes' with the minimum fields required to make a request.
--
-- * 'control' - Data attached to the event that can be used by the decider in subsequent workflow tasks.
-- * 'decisionTaskCompletedEventId' - The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @StartTimer@ decision for this activity task. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
-- * 'startToFireTimeout' - The duration of time after which the timer fires.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ .
-- * 'timerId' - The unique ID of the timer that was started.
mkTimerStartedEventAttributes ::
  -- | 'timerId'
  Lude.Text ->
  -- | 'startToFireTimeout'
  Lude.Text ->
  -- | 'decisionTaskCompletedEventId'
  Lude.Integer ->
  TimerStartedEventAttributes
mkTimerStartedEventAttributes
  pTimerId_
  pStartToFireTimeout_
  pDecisionTaskCompletedEventId_ =
    TimerStartedEventAttributes'
      { control = Lude.Nothing,
        timerId = pTimerId_,
        startToFireTimeout = pStartToFireTimeout_,
        decisionTaskCompletedEventId = pDecisionTaskCompletedEventId_
      }

-- | Data attached to the event that can be used by the decider in subsequent workflow tasks.
--
-- /Note:/ Consider using 'control' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tseaControl :: Lens.Lens' TimerStartedEventAttributes (Lude.Maybe Lude.Text)
tseaControl = Lens.lens (control :: TimerStartedEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {control = a} :: TimerStartedEventAttributes)
{-# DEPRECATED tseaControl "Use generic-lens or generic-optics with 'control' instead." #-}

-- | The unique ID of the timer that was started.
--
-- /Note:/ Consider using 'timerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tseaTimerId :: Lens.Lens' TimerStartedEventAttributes Lude.Text
tseaTimerId = Lens.lens (timerId :: TimerStartedEventAttributes -> Lude.Text) (\s a -> s {timerId = a} :: TimerStartedEventAttributes)
{-# DEPRECATED tseaTimerId "Use generic-lens or generic-optics with 'timerId' instead." #-}

-- | The duration of time after which the timer fires.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ .
--
-- /Note:/ Consider using 'startToFireTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tseaStartToFireTimeout :: Lens.Lens' TimerStartedEventAttributes Lude.Text
tseaStartToFireTimeout = Lens.lens (startToFireTimeout :: TimerStartedEventAttributes -> Lude.Text) (\s a -> s {startToFireTimeout = a} :: TimerStartedEventAttributes)
{-# DEPRECATED tseaStartToFireTimeout "Use generic-lens or generic-optics with 'startToFireTimeout' instead." #-}

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @StartTimer@ decision for this activity task. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'decisionTaskCompletedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tseaDecisionTaskCompletedEventId :: Lens.Lens' TimerStartedEventAttributes Lude.Integer
tseaDecisionTaskCompletedEventId = Lens.lens (decisionTaskCompletedEventId :: TimerStartedEventAttributes -> Lude.Integer) (\s a -> s {decisionTaskCompletedEventId = a} :: TimerStartedEventAttributes)
{-# DEPRECATED tseaDecisionTaskCompletedEventId "Use generic-lens or generic-optics with 'decisionTaskCompletedEventId' instead." #-}

instance Lude.FromJSON TimerStartedEventAttributes where
  parseJSON =
    Lude.withObject
      "TimerStartedEventAttributes"
      ( \x ->
          TimerStartedEventAttributes'
            Lude.<$> (x Lude..:? "control")
            Lude.<*> (x Lude..: "timerId")
            Lude.<*> (x Lude..: "startToFireTimeout")
            Lude.<*> (x Lude..: "decisionTaskCompletedEventId")
      )
