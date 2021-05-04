{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.TimerCanceledEventAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.TimerCanceledEventAttributes where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides the details of the @TimerCanceled@ event.
--
-- /See:/ 'newTimerCanceledEventAttributes' smart constructor.
data TimerCanceledEventAttributes = TimerCanceledEventAttributes'
  { -- | The unique ID of the timer that was canceled.
    timerId :: Prelude.Text,
    -- | The ID of the @TimerStarted@ event that was recorded when this timer was
    -- started. This information can be useful for diagnosing problems by
    -- tracing back the chain of events leading up to this event.
    startedEventId :: Prelude.Integer,
    -- | The ID of the @DecisionTaskCompleted@ event corresponding to the
    -- decision task that resulted in the @CancelTimer@ decision to cancel this
    -- timer. This information can be useful for diagnosing problems by tracing
    -- back the chain of events leading up to this event.
    decisionTaskCompletedEventId :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TimerCanceledEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timerId', 'timerCanceledEventAttributes_timerId' - The unique ID of the timer that was canceled.
--
-- 'startedEventId', 'timerCanceledEventAttributes_startedEventId' - The ID of the @TimerStarted@ event that was recorded when this timer was
-- started. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
--
-- 'decisionTaskCompletedEventId', 'timerCanceledEventAttributes_decisionTaskCompletedEventId' - The ID of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the @CancelTimer@ decision to cancel this
-- timer. This information can be useful for diagnosing problems by tracing
-- back the chain of events leading up to this event.
newTimerCanceledEventAttributes ::
  -- | 'timerId'
  Prelude.Text ->
  -- | 'startedEventId'
  Prelude.Integer ->
  -- | 'decisionTaskCompletedEventId'
  Prelude.Integer ->
  TimerCanceledEventAttributes
newTimerCanceledEventAttributes
  pTimerId_
  pStartedEventId_
  pDecisionTaskCompletedEventId_ =
    TimerCanceledEventAttributes'
      { timerId = pTimerId_,
        startedEventId = pStartedEventId_,
        decisionTaskCompletedEventId =
          pDecisionTaskCompletedEventId_
      }

-- | The unique ID of the timer that was canceled.
timerCanceledEventAttributes_timerId :: Lens.Lens' TimerCanceledEventAttributes Prelude.Text
timerCanceledEventAttributes_timerId = Lens.lens (\TimerCanceledEventAttributes' {timerId} -> timerId) (\s@TimerCanceledEventAttributes' {} a -> s {timerId = a} :: TimerCanceledEventAttributes)

-- | The ID of the @TimerStarted@ event that was recorded when this timer was
-- started. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
timerCanceledEventAttributes_startedEventId :: Lens.Lens' TimerCanceledEventAttributes Prelude.Integer
timerCanceledEventAttributes_startedEventId = Lens.lens (\TimerCanceledEventAttributes' {startedEventId} -> startedEventId) (\s@TimerCanceledEventAttributes' {} a -> s {startedEventId = a} :: TimerCanceledEventAttributes)

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the @CancelTimer@ decision to cancel this
-- timer. This information can be useful for diagnosing problems by tracing
-- back the chain of events leading up to this event.
timerCanceledEventAttributes_decisionTaskCompletedEventId :: Lens.Lens' TimerCanceledEventAttributes Prelude.Integer
timerCanceledEventAttributes_decisionTaskCompletedEventId = Lens.lens (\TimerCanceledEventAttributes' {decisionTaskCompletedEventId} -> decisionTaskCompletedEventId) (\s@TimerCanceledEventAttributes' {} a -> s {decisionTaskCompletedEventId = a} :: TimerCanceledEventAttributes)

instance
  Prelude.FromJSON
    TimerCanceledEventAttributes
  where
  parseJSON =
    Prelude.withObject
      "TimerCanceledEventAttributes"
      ( \x ->
          TimerCanceledEventAttributes'
            Prelude.<$> (x Prelude..: "timerId")
            Prelude.<*> (x Prelude..: "startedEventId")
            Prelude.<*> (x Prelude..: "decisionTaskCompletedEventId")
      )

instance
  Prelude.Hashable
    TimerCanceledEventAttributes

instance Prelude.NFData TimerCanceledEventAttributes
