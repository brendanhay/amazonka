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
-- Module      : Amazonka.SWF.Types.ActivityTaskCanceledEventAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.ActivityTaskCanceledEventAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides the details of the @ActivityTaskCanceled@ event.
--
-- /See:/ 'newActivityTaskCanceledEventAttributes' smart constructor.
data ActivityTaskCanceledEventAttributes = ActivityTaskCanceledEventAttributes'
  { -- | Details of the cancellation.
    details :: Prelude.Maybe Prelude.Text,
    -- | If set, contains the ID of the last @ActivityTaskCancelRequested@ event
    -- recorded for this activity task. This information can be useful for
    -- diagnosing problems by tracing back the chain of events leading up to
    -- this event.
    latestCancelRequestedEventId :: Prelude.Maybe Prelude.Integer,
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
-- Create a value of 'ActivityTaskCanceledEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'details', 'activityTaskCanceledEventAttributes_details' - Details of the cancellation.
--
-- 'latestCancelRequestedEventId', 'activityTaskCanceledEventAttributes_latestCancelRequestedEventId' - If set, contains the ID of the last @ActivityTaskCancelRequested@ event
-- recorded for this activity task. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
--
-- 'scheduledEventId', 'activityTaskCanceledEventAttributes_scheduledEventId' - The ID of the @ActivityTaskScheduled@ event that was recorded when this
-- activity task was scheduled. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
--
-- 'startedEventId', 'activityTaskCanceledEventAttributes_startedEventId' - The ID of the @ActivityTaskStarted@ event recorded when this activity
-- task was started. This information can be useful for diagnosing problems
-- by tracing back the chain of events leading up to this event.
newActivityTaskCanceledEventAttributes ::
  -- | 'scheduledEventId'
  Prelude.Integer ->
  -- | 'startedEventId'
  Prelude.Integer ->
  ActivityTaskCanceledEventAttributes
newActivityTaskCanceledEventAttributes
  pScheduledEventId_
  pStartedEventId_ =
    ActivityTaskCanceledEventAttributes'
      { details =
          Prelude.Nothing,
        latestCancelRequestedEventId =
          Prelude.Nothing,
        scheduledEventId = pScheduledEventId_,
        startedEventId = pStartedEventId_
      }

-- | Details of the cancellation.
activityTaskCanceledEventAttributes_details :: Lens.Lens' ActivityTaskCanceledEventAttributes (Prelude.Maybe Prelude.Text)
activityTaskCanceledEventAttributes_details = Lens.lens (\ActivityTaskCanceledEventAttributes' {details} -> details) (\s@ActivityTaskCanceledEventAttributes' {} a -> s {details = a} :: ActivityTaskCanceledEventAttributes)

-- | If set, contains the ID of the last @ActivityTaskCancelRequested@ event
-- recorded for this activity task. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
activityTaskCanceledEventAttributes_latestCancelRequestedEventId :: Lens.Lens' ActivityTaskCanceledEventAttributes (Prelude.Maybe Prelude.Integer)
activityTaskCanceledEventAttributes_latestCancelRequestedEventId = Lens.lens (\ActivityTaskCanceledEventAttributes' {latestCancelRequestedEventId} -> latestCancelRequestedEventId) (\s@ActivityTaskCanceledEventAttributes' {} a -> s {latestCancelRequestedEventId = a} :: ActivityTaskCanceledEventAttributes)

-- | The ID of the @ActivityTaskScheduled@ event that was recorded when this
-- activity task was scheduled. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
activityTaskCanceledEventAttributes_scheduledEventId :: Lens.Lens' ActivityTaskCanceledEventAttributes Prelude.Integer
activityTaskCanceledEventAttributes_scheduledEventId = Lens.lens (\ActivityTaskCanceledEventAttributes' {scheduledEventId} -> scheduledEventId) (\s@ActivityTaskCanceledEventAttributes' {} a -> s {scheduledEventId = a} :: ActivityTaskCanceledEventAttributes)

-- | The ID of the @ActivityTaskStarted@ event recorded when this activity
-- task was started. This information can be useful for diagnosing problems
-- by tracing back the chain of events leading up to this event.
activityTaskCanceledEventAttributes_startedEventId :: Lens.Lens' ActivityTaskCanceledEventAttributes Prelude.Integer
activityTaskCanceledEventAttributes_startedEventId = Lens.lens (\ActivityTaskCanceledEventAttributes' {startedEventId} -> startedEventId) (\s@ActivityTaskCanceledEventAttributes' {} a -> s {startedEventId = a} :: ActivityTaskCanceledEventAttributes)

instance
  Data.FromJSON
    ActivityTaskCanceledEventAttributes
  where
  parseJSON =
    Data.withObject
      "ActivityTaskCanceledEventAttributes"
      ( \x ->
          ActivityTaskCanceledEventAttributes'
            Prelude.<$> (x Data..:? "details")
            Prelude.<*> (x Data..:? "latestCancelRequestedEventId")
            Prelude.<*> (x Data..: "scheduledEventId")
            Prelude.<*> (x Data..: "startedEventId")
      )

instance
  Prelude.Hashable
    ActivityTaskCanceledEventAttributes
  where
  hashWithSalt
    _salt
    ActivityTaskCanceledEventAttributes' {..} =
      _salt `Prelude.hashWithSalt` details
        `Prelude.hashWithSalt` latestCancelRequestedEventId
        `Prelude.hashWithSalt` scheduledEventId
        `Prelude.hashWithSalt` startedEventId

instance
  Prelude.NFData
    ActivityTaskCanceledEventAttributes
  where
  rnf ActivityTaskCanceledEventAttributes' {..} =
    Prelude.rnf details
      `Prelude.seq` Prelude.rnf latestCancelRequestedEventId
      `Prelude.seq` Prelude.rnf scheduledEventId
      `Prelude.seq` Prelude.rnf startedEventId
