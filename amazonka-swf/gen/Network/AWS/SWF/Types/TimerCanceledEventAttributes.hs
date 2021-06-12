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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides the details of the @TimerCanceled@ event.
--
-- /See:/ 'newTimerCanceledEventAttributes' smart constructor.
data TimerCanceledEventAttributes = TimerCanceledEventAttributes'
  { -- | The unique ID of the timer that was canceled.
    timerId :: Core.Text,
    -- | The ID of the @TimerStarted@ event that was recorded when this timer was
    -- started. This information can be useful for diagnosing problems by
    -- tracing back the chain of events leading up to this event.
    startedEventId :: Core.Integer,
    -- | The ID of the @DecisionTaskCompleted@ event corresponding to the
    -- decision task that resulted in the @CancelTimer@ decision to cancel this
    -- timer. This information can be useful for diagnosing problems by tracing
    -- back the chain of events leading up to this event.
    decisionTaskCompletedEventId :: Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'startedEventId'
  Core.Integer ->
  -- | 'decisionTaskCompletedEventId'
  Core.Integer ->
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
timerCanceledEventAttributes_timerId :: Lens.Lens' TimerCanceledEventAttributes Core.Text
timerCanceledEventAttributes_timerId = Lens.lens (\TimerCanceledEventAttributes' {timerId} -> timerId) (\s@TimerCanceledEventAttributes' {} a -> s {timerId = a} :: TimerCanceledEventAttributes)

-- | The ID of the @TimerStarted@ event that was recorded when this timer was
-- started. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
timerCanceledEventAttributes_startedEventId :: Lens.Lens' TimerCanceledEventAttributes Core.Integer
timerCanceledEventAttributes_startedEventId = Lens.lens (\TimerCanceledEventAttributes' {startedEventId} -> startedEventId) (\s@TimerCanceledEventAttributes' {} a -> s {startedEventId = a} :: TimerCanceledEventAttributes)

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the @CancelTimer@ decision to cancel this
-- timer. This information can be useful for diagnosing problems by tracing
-- back the chain of events leading up to this event.
timerCanceledEventAttributes_decisionTaskCompletedEventId :: Lens.Lens' TimerCanceledEventAttributes Core.Integer
timerCanceledEventAttributes_decisionTaskCompletedEventId = Lens.lens (\TimerCanceledEventAttributes' {decisionTaskCompletedEventId} -> decisionTaskCompletedEventId) (\s@TimerCanceledEventAttributes' {} a -> s {decisionTaskCompletedEventId = a} :: TimerCanceledEventAttributes)

instance Core.FromJSON TimerCanceledEventAttributes where
  parseJSON =
    Core.withObject
      "TimerCanceledEventAttributes"
      ( \x ->
          TimerCanceledEventAttributes'
            Core.<$> (x Core..: "timerId")
            Core.<*> (x Core..: "startedEventId")
            Core.<*> (x Core..: "decisionTaskCompletedEventId")
      )

instance Core.Hashable TimerCanceledEventAttributes

instance Core.NFData TimerCanceledEventAttributes
