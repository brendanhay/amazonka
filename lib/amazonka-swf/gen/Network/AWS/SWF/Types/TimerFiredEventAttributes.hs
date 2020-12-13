{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.TimerFiredEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.TimerFiredEventAttributes
  ( TimerFiredEventAttributes (..),

    -- * Smart constructor
    mkTimerFiredEventAttributes,

    -- * Lenses
    tfeaTimerId,
    tfeaStartedEventId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides the details of the @TimerFired@ event.
--
-- /See:/ 'mkTimerFiredEventAttributes' smart constructor.
data TimerFiredEventAttributes = TimerFiredEventAttributes'
  { -- | The unique ID of the timer that fired.
    timerId :: Lude.Text,
    -- | The ID of the @TimerStarted@ event that was recorded when this timer was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
    startedEventId :: Lude.Integer
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TimerFiredEventAttributes' with the minimum fields required to make a request.
--
-- * 'timerId' - The unique ID of the timer that fired.
-- * 'startedEventId' - The ID of the @TimerStarted@ event that was recorded when this timer was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
mkTimerFiredEventAttributes ::
  -- | 'timerId'
  Lude.Text ->
  -- | 'startedEventId'
  Lude.Integer ->
  TimerFiredEventAttributes
mkTimerFiredEventAttributes pTimerId_ pStartedEventId_ =
  TimerFiredEventAttributes'
    { timerId = pTimerId_,
      startedEventId = pStartedEventId_
    }

-- | The unique ID of the timer that fired.
--
-- /Note:/ Consider using 'timerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfeaTimerId :: Lens.Lens' TimerFiredEventAttributes Lude.Text
tfeaTimerId = Lens.lens (timerId :: TimerFiredEventAttributes -> Lude.Text) (\s a -> s {timerId = a} :: TimerFiredEventAttributes)
{-# DEPRECATED tfeaTimerId "Use generic-lens or generic-optics with 'timerId' instead." #-}

-- | The ID of the @TimerStarted@ event that was recorded when this timer was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'startedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfeaStartedEventId :: Lens.Lens' TimerFiredEventAttributes Lude.Integer
tfeaStartedEventId = Lens.lens (startedEventId :: TimerFiredEventAttributes -> Lude.Integer) (\s a -> s {startedEventId = a} :: TimerFiredEventAttributes)
{-# DEPRECATED tfeaStartedEventId "Use generic-lens or generic-optics with 'startedEventId' instead." #-}

instance Lude.FromJSON TimerFiredEventAttributes where
  parseJSON =
    Lude.withObject
      "TimerFiredEventAttributes"
      ( \x ->
          TimerFiredEventAttributes'
            Lude.<$> (x Lude..: "timerId") Lude.<*> (x Lude..: "startedEventId")
      )
