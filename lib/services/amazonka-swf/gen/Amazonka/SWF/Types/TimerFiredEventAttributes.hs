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
-- Module      : Amazonka.SWF.Types.TimerFiredEventAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.TimerFiredEventAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides the details of the @TimerFired@ event.
--
-- /See:/ 'newTimerFiredEventAttributes' smart constructor.
data TimerFiredEventAttributes = TimerFiredEventAttributes'
  { -- | The unique ID of the timer that fired.
    timerId :: Prelude.Text,
    -- | The ID of the @TimerStarted@ event that was recorded when this timer was
    -- started. This information can be useful for diagnosing problems by
    -- tracing back the chain of events leading up to this event.
    startedEventId :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimerFiredEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timerId', 'timerFiredEventAttributes_timerId' - The unique ID of the timer that fired.
--
-- 'startedEventId', 'timerFiredEventAttributes_startedEventId' - The ID of the @TimerStarted@ event that was recorded when this timer was
-- started. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
newTimerFiredEventAttributes ::
  -- | 'timerId'
  Prelude.Text ->
  -- | 'startedEventId'
  Prelude.Integer ->
  TimerFiredEventAttributes
newTimerFiredEventAttributes
  pTimerId_
  pStartedEventId_ =
    TimerFiredEventAttributes'
      { timerId = pTimerId_,
        startedEventId = pStartedEventId_
      }

-- | The unique ID of the timer that fired.
timerFiredEventAttributes_timerId :: Lens.Lens' TimerFiredEventAttributes Prelude.Text
timerFiredEventAttributes_timerId = Lens.lens (\TimerFiredEventAttributes' {timerId} -> timerId) (\s@TimerFiredEventAttributes' {} a -> s {timerId = a} :: TimerFiredEventAttributes)

-- | The ID of the @TimerStarted@ event that was recorded when this timer was
-- started. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
timerFiredEventAttributes_startedEventId :: Lens.Lens' TimerFiredEventAttributes Prelude.Integer
timerFiredEventAttributes_startedEventId = Lens.lens (\TimerFiredEventAttributes' {startedEventId} -> startedEventId) (\s@TimerFiredEventAttributes' {} a -> s {startedEventId = a} :: TimerFiredEventAttributes)

instance Data.FromJSON TimerFiredEventAttributes where
  parseJSON =
    Data.withObject
      "TimerFiredEventAttributes"
      ( \x ->
          TimerFiredEventAttributes'
            Prelude.<$> (x Data..: "timerId")
            Prelude.<*> (x Data..: "startedEventId")
      )

instance Prelude.Hashable TimerFiredEventAttributes where
  hashWithSalt _salt TimerFiredEventAttributes' {..} =
    _salt `Prelude.hashWithSalt` timerId
      `Prelude.hashWithSalt` startedEventId

instance Prelude.NFData TimerFiredEventAttributes where
  rnf TimerFiredEventAttributes' {..} =
    Prelude.rnf timerId
      `Prelude.seq` Prelude.rnf startedEventId
