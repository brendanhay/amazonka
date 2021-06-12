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
-- Module      : Network.AWS.SWF.Types.TimerStartedEventAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.TimerStartedEventAttributes where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides the details of the @TimerStarted@ event.
--
-- /See:/ 'newTimerStartedEventAttributes' smart constructor.
data TimerStartedEventAttributes = TimerStartedEventAttributes'
  { -- | Data attached to the event that can be used by the decider in subsequent
    -- workflow tasks.
    control :: Core.Maybe Core.Text,
    -- | The unique ID of the timer that was started.
    timerId :: Core.Text,
    -- | The duration of time after which the timer fires.
    --
    -- The duration is specified in seconds, an integer greater than or equal
    -- to @0@.
    startToFireTimeout :: Core.Text,
    -- | The ID of the @DecisionTaskCompleted@ event corresponding to the
    -- decision task that resulted in the @StartTimer@ decision for this
    -- activity task. This information can be useful for diagnosing problems by
    -- tracing back the chain of events leading up to this event.
    decisionTaskCompletedEventId :: Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TimerStartedEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'control', 'timerStartedEventAttributes_control' - Data attached to the event that can be used by the decider in subsequent
-- workflow tasks.
--
-- 'timerId', 'timerStartedEventAttributes_timerId' - The unique ID of the timer that was started.
--
-- 'startToFireTimeout', 'timerStartedEventAttributes_startToFireTimeout' - The duration of time after which the timer fires.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@.
--
-- 'decisionTaskCompletedEventId', 'timerStartedEventAttributes_decisionTaskCompletedEventId' - The ID of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the @StartTimer@ decision for this
-- activity task. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
newTimerStartedEventAttributes ::
  -- | 'timerId'
  Core.Text ->
  -- | 'startToFireTimeout'
  Core.Text ->
  -- | 'decisionTaskCompletedEventId'
  Core.Integer ->
  TimerStartedEventAttributes
newTimerStartedEventAttributes
  pTimerId_
  pStartToFireTimeout_
  pDecisionTaskCompletedEventId_ =
    TimerStartedEventAttributes'
      { control =
          Core.Nothing,
        timerId = pTimerId_,
        startToFireTimeout = pStartToFireTimeout_,
        decisionTaskCompletedEventId =
          pDecisionTaskCompletedEventId_
      }

-- | Data attached to the event that can be used by the decider in subsequent
-- workflow tasks.
timerStartedEventAttributes_control :: Lens.Lens' TimerStartedEventAttributes (Core.Maybe Core.Text)
timerStartedEventAttributes_control = Lens.lens (\TimerStartedEventAttributes' {control} -> control) (\s@TimerStartedEventAttributes' {} a -> s {control = a} :: TimerStartedEventAttributes)

-- | The unique ID of the timer that was started.
timerStartedEventAttributes_timerId :: Lens.Lens' TimerStartedEventAttributes Core.Text
timerStartedEventAttributes_timerId = Lens.lens (\TimerStartedEventAttributes' {timerId} -> timerId) (\s@TimerStartedEventAttributes' {} a -> s {timerId = a} :: TimerStartedEventAttributes)

-- | The duration of time after which the timer fires.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@.
timerStartedEventAttributes_startToFireTimeout :: Lens.Lens' TimerStartedEventAttributes Core.Text
timerStartedEventAttributes_startToFireTimeout = Lens.lens (\TimerStartedEventAttributes' {startToFireTimeout} -> startToFireTimeout) (\s@TimerStartedEventAttributes' {} a -> s {startToFireTimeout = a} :: TimerStartedEventAttributes)

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the @StartTimer@ decision for this
-- activity task. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
timerStartedEventAttributes_decisionTaskCompletedEventId :: Lens.Lens' TimerStartedEventAttributes Core.Integer
timerStartedEventAttributes_decisionTaskCompletedEventId = Lens.lens (\TimerStartedEventAttributes' {decisionTaskCompletedEventId} -> decisionTaskCompletedEventId) (\s@TimerStartedEventAttributes' {} a -> s {decisionTaskCompletedEventId = a} :: TimerStartedEventAttributes)

instance Core.FromJSON TimerStartedEventAttributes where
  parseJSON =
    Core.withObject
      "TimerStartedEventAttributes"
      ( \x ->
          TimerStartedEventAttributes'
            Core.<$> (x Core..:? "control")
            Core.<*> (x Core..: "timerId")
            Core.<*> (x Core..: "startToFireTimeout")
            Core.<*> (x Core..: "decisionTaskCompletedEventId")
      )

instance Core.Hashable TimerStartedEventAttributes

instance Core.NFData TimerStartedEventAttributes
