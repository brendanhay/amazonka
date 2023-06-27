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
-- Module      : Amazonka.SWF.Types.ActivityTaskCompletedEventAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.ActivityTaskCompletedEventAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides the details of the @ActivityTaskCompleted@ event.
--
-- /See:/ 'newActivityTaskCompletedEventAttributes' smart constructor.
data ActivityTaskCompletedEventAttributes = ActivityTaskCompletedEventAttributes'
  { -- | The results of the activity task.
    result :: Prelude.Maybe Prelude.Text,
    -- | The ID of the @ActivityTaskScheduled@ event that was recorded when this
    -- activity task was scheduled. This information can be useful for
    -- diagnosing problems by tracing back the chain of events leading up to
    -- this event.
    scheduledEventId :: Prelude.Integer,
    -- | The ID of the @ActivityTaskStarted@ event recorded when this activity
    -- task was started. This information can be useful for diagnosing problems
    -- by tracing back the chain of events leading up to this event.
    startedEventId :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActivityTaskCompletedEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'result', 'activityTaskCompletedEventAttributes_result' - The results of the activity task.
--
-- 'scheduledEventId', 'activityTaskCompletedEventAttributes_scheduledEventId' - The ID of the @ActivityTaskScheduled@ event that was recorded when this
-- activity task was scheduled. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
--
-- 'startedEventId', 'activityTaskCompletedEventAttributes_startedEventId' - The ID of the @ActivityTaskStarted@ event recorded when this activity
-- task was started. This information can be useful for diagnosing problems
-- by tracing back the chain of events leading up to this event.
newActivityTaskCompletedEventAttributes ::
  -- | 'scheduledEventId'
  Prelude.Integer ->
  -- | 'startedEventId'
  Prelude.Integer ->
  ActivityTaskCompletedEventAttributes
newActivityTaskCompletedEventAttributes
  pScheduledEventId_
  pStartedEventId_ =
    ActivityTaskCompletedEventAttributes'
      { result =
          Prelude.Nothing,
        scheduledEventId = pScheduledEventId_,
        startedEventId = pStartedEventId_
      }

-- | The results of the activity task.
activityTaskCompletedEventAttributes_result :: Lens.Lens' ActivityTaskCompletedEventAttributes (Prelude.Maybe Prelude.Text)
activityTaskCompletedEventAttributes_result = Lens.lens (\ActivityTaskCompletedEventAttributes' {result} -> result) (\s@ActivityTaskCompletedEventAttributes' {} a -> s {result = a} :: ActivityTaskCompletedEventAttributes)

-- | The ID of the @ActivityTaskScheduled@ event that was recorded when this
-- activity task was scheduled. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
activityTaskCompletedEventAttributes_scheduledEventId :: Lens.Lens' ActivityTaskCompletedEventAttributes Prelude.Integer
activityTaskCompletedEventAttributes_scheduledEventId = Lens.lens (\ActivityTaskCompletedEventAttributes' {scheduledEventId} -> scheduledEventId) (\s@ActivityTaskCompletedEventAttributes' {} a -> s {scheduledEventId = a} :: ActivityTaskCompletedEventAttributes)

-- | The ID of the @ActivityTaskStarted@ event recorded when this activity
-- task was started. This information can be useful for diagnosing problems
-- by tracing back the chain of events leading up to this event.
activityTaskCompletedEventAttributes_startedEventId :: Lens.Lens' ActivityTaskCompletedEventAttributes Prelude.Integer
activityTaskCompletedEventAttributes_startedEventId = Lens.lens (\ActivityTaskCompletedEventAttributes' {startedEventId} -> startedEventId) (\s@ActivityTaskCompletedEventAttributes' {} a -> s {startedEventId = a} :: ActivityTaskCompletedEventAttributes)

instance
  Data.FromJSON
    ActivityTaskCompletedEventAttributes
  where
  parseJSON =
    Data.withObject
      "ActivityTaskCompletedEventAttributes"
      ( \x ->
          ActivityTaskCompletedEventAttributes'
            Prelude.<$> (x Data..:? "result")
            Prelude.<*> (x Data..: "scheduledEventId")
            Prelude.<*> (x Data..: "startedEventId")
      )

instance
  Prelude.Hashable
    ActivityTaskCompletedEventAttributes
  where
  hashWithSalt
    _salt
    ActivityTaskCompletedEventAttributes' {..} =
      _salt
        `Prelude.hashWithSalt` result
        `Prelude.hashWithSalt` scheduledEventId
        `Prelude.hashWithSalt` startedEventId

instance
  Prelude.NFData
    ActivityTaskCompletedEventAttributes
  where
  rnf ActivityTaskCompletedEventAttributes' {..} =
    Prelude.rnf result
      `Prelude.seq` Prelude.rnf scheduledEventId
      `Prelude.seq` Prelude.rnf startedEventId
