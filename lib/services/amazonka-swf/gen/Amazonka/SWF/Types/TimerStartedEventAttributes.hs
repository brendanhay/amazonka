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
-- Module      : Amazonka.SWF.Types.TimerStartedEventAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.TimerStartedEventAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides the details of the @TimerStarted@ event.
--
-- /See:/ 'newTimerStartedEventAttributes' smart constructor.
data TimerStartedEventAttributes = TimerStartedEventAttributes'
  { -- | Data attached to the event that can be used by the decider in subsequent
    -- workflow tasks.
    control :: Prelude.Maybe Prelude.Text,
    -- | The unique ID of the timer that was started.
    timerId :: Prelude.Text,
    -- | The duration of time after which the timer fires.
    --
    -- The duration is specified in seconds, an integer greater than or equal
    -- to @0@.
    startToFireTimeout :: Prelude.Text,
    -- | The ID of the @DecisionTaskCompleted@ event corresponding to the
    -- decision task that resulted in the @StartTimer@ decision for this
    -- activity task. This information can be useful for diagnosing problems by
    -- tracing back the chain of events leading up to this event.
    decisionTaskCompletedEventId :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'startToFireTimeout'
  Prelude.Text ->
  -- | 'decisionTaskCompletedEventId'
  Prelude.Integer ->
  TimerStartedEventAttributes
newTimerStartedEventAttributes
  pTimerId_
  pStartToFireTimeout_
  pDecisionTaskCompletedEventId_ =
    TimerStartedEventAttributes'
      { control =
          Prelude.Nothing,
        timerId = pTimerId_,
        startToFireTimeout = pStartToFireTimeout_,
        decisionTaskCompletedEventId =
          pDecisionTaskCompletedEventId_
      }

-- | Data attached to the event that can be used by the decider in subsequent
-- workflow tasks.
timerStartedEventAttributes_control :: Lens.Lens' TimerStartedEventAttributes (Prelude.Maybe Prelude.Text)
timerStartedEventAttributes_control = Lens.lens (\TimerStartedEventAttributes' {control} -> control) (\s@TimerStartedEventAttributes' {} a -> s {control = a} :: TimerStartedEventAttributes)

-- | The unique ID of the timer that was started.
timerStartedEventAttributes_timerId :: Lens.Lens' TimerStartedEventAttributes Prelude.Text
timerStartedEventAttributes_timerId = Lens.lens (\TimerStartedEventAttributes' {timerId} -> timerId) (\s@TimerStartedEventAttributes' {} a -> s {timerId = a} :: TimerStartedEventAttributes)

-- | The duration of time after which the timer fires.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@.
timerStartedEventAttributes_startToFireTimeout :: Lens.Lens' TimerStartedEventAttributes Prelude.Text
timerStartedEventAttributes_startToFireTimeout = Lens.lens (\TimerStartedEventAttributes' {startToFireTimeout} -> startToFireTimeout) (\s@TimerStartedEventAttributes' {} a -> s {startToFireTimeout = a} :: TimerStartedEventAttributes)

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the @StartTimer@ decision for this
-- activity task. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
timerStartedEventAttributes_decisionTaskCompletedEventId :: Lens.Lens' TimerStartedEventAttributes Prelude.Integer
timerStartedEventAttributes_decisionTaskCompletedEventId = Lens.lens (\TimerStartedEventAttributes' {decisionTaskCompletedEventId} -> decisionTaskCompletedEventId) (\s@TimerStartedEventAttributes' {} a -> s {decisionTaskCompletedEventId = a} :: TimerStartedEventAttributes)

instance Data.FromJSON TimerStartedEventAttributes where
  parseJSON =
    Data.withObject
      "TimerStartedEventAttributes"
      ( \x ->
          TimerStartedEventAttributes'
            Prelude.<$> (x Data..:? "control")
            Prelude.<*> (x Data..: "timerId")
            Prelude.<*> (x Data..: "startToFireTimeout")
            Prelude.<*> (x Data..: "decisionTaskCompletedEventId")
      )

instance Prelude.Hashable TimerStartedEventAttributes where
  hashWithSalt _salt TimerStartedEventAttributes' {..} =
    _salt
      `Prelude.hashWithSalt` control
      `Prelude.hashWithSalt` timerId
      `Prelude.hashWithSalt` startToFireTimeout
      `Prelude.hashWithSalt` decisionTaskCompletedEventId

instance Prelude.NFData TimerStartedEventAttributes where
  rnf TimerStartedEventAttributes' {..} =
    Prelude.rnf control `Prelude.seq`
      Prelude.rnf timerId `Prelude.seq`
        Prelude.rnf startToFireTimeout `Prelude.seq`
          Prelude.rnf decisionTaskCompletedEventId
