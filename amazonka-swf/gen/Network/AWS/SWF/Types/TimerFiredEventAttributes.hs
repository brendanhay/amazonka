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
-- Module      : Network.AWS.SWF.Types.TimerFiredEventAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.TimerFiredEventAttributes where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides the details of the @TimerFired@ event.
--
-- /See:/ 'newTimerFiredEventAttributes' smart constructor.
data TimerFiredEventAttributes = TimerFiredEventAttributes'
  { -- | The unique ID of the timer that fired.
    timerId :: Core.Text,
    -- | The ID of the @TimerStarted@ event that was recorded when this timer was
    -- started. This information can be useful for diagnosing problems by
    -- tracing back the chain of events leading up to this event.
    startedEventId :: Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'startedEventId'
  Core.Integer ->
  TimerFiredEventAttributes
newTimerFiredEventAttributes
  pTimerId_
  pStartedEventId_ =
    TimerFiredEventAttributes'
      { timerId = pTimerId_,
        startedEventId = pStartedEventId_
      }

-- | The unique ID of the timer that fired.
timerFiredEventAttributes_timerId :: Lens.Lens' TimerFiredEventAttributes Core.Text
timerFiredEventAttributes_timerId = Lens.lens (\TimerFiredEventAttributes' {timerId} -> timerId) (\s@TimerFiredEventAttributes' {} a -> s {timerId = a} :: TimerFiredEventAttributes)

-- | The ID of the @TimerStarted@ event that was recorded when this timer was
-- started. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
timerFiredEventAttributes_startedEventId :: Lens.Lens' TimerFiredEventAttributes Core.Integer
timerFiredEventAttributes_startedEventId = Lens.lens (\TimerFiredEventAttributes' {startedEventId} -> startedEventId) (\s@TimerFiredEventAttributes' {} a -> s {startedEventId = a} :: TimerFiredEventAttributes)

instance Core.FromJSON TimerFiredEventAttributes where
  parseJSON =
    Core.withObject
      "TimerFiredEventAttributes"
      ( \x ->
          TimerFiredEventAttributes'
            Core.<$> (x Core..: "timerId")
            Core.<*> (x Core..: "startedEventId")
      )

instance Core.Hashable TimerFiredEventAttributes

instance Core.NFData TimerFiredEventAttributes
