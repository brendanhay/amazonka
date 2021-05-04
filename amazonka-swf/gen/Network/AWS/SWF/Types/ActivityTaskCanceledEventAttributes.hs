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
-- Module      : Network.AWS.SWF.Types.ActivityTaskCanceledEventAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ActivityTaskCanceledEventAttributes where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides the details of the @ActivityTaskCanceled@ event.
--
-- /See:/ 'newActivityTaskCanceledEventAttributes' smart constructor.
data ActivityTaskCanceledEventAttributes = ActivityTaskCanceledEventAttributes'
  { -- | If set, contains the ID of the last @ActivityTaskCancelRequested@ event
    -- recorded for this activity task. This information can be useful for
    -- diagnosing problems by tracing back the chain of events leading up to
    -- this event.
    latestCancelRequestedEventId :: Prelude.Maybe Prelude.Integer,
    -- | Details of the cancellation.
    details :: Prelude.Maybe Prelude.Text,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ActivityTaskCanceledEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'latestCancelRequestedEventId', 'activityTaskCanceledEventAttributes_latestCancelRequestedEventId' - If set, contains the ID of the last @ActivityTaskCancelRequested@ event
-- recorded for this activity task. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
--
-- 'details', 'activityTaskCanceledEventAttributes_details' - Details of the cancellation.
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
      { latestCancelRequestedEventId =
          Prelude.Nothing,
        details = Prelude.Nothing,
        scheduledEventId = pScheduledEventId_,
        startedEventId = pStartedEventId_
      }

-- | If set, contains the ID of the last @ActivityTaskCancelRequested@ event
-- recorded for this activity task. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
activityTaskCanceledEventAttributes_latestCancelRequestedEventId :: Lens.Lens' ActivityTaskCanceledEventAttributes (Prelude.Maybe Prelude.Integer)
activityTaskCanceledEventAttributes_latestCancelRequestedEventId = Lens.lens (\ActivityTaskCanceledEventAttributes' {latestCancelRequestedEventId} -> latestCancelRequestedEventId) (\s@ActivityTaskCanceledEventAttributes' {} a -> s {latestCancelRequestedEventId = a} :: ActivityTaskCanceledEventAttributes)

-- | Details of the cancellation.
activityTaskCanceledEventAttributes_details :: Lens.Lens' ActivityTaskCanceledEventAttributes (Prelude.Maybe Prelude.Text)
activityTaskCanceledEventAttributes_details = Lens.lens (\ActivityTaskCanceledEventAttributes' {details} -> details) (\s@ActivityTaskCanceledEventAttributes' {} a -> s {details = a} :: ActivityTaskCanceledEventAttributes)

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
  Prelude.FromJSON
    ActivityTaskCanceledEventAttributes
  where
  parseJSON =
    Prelude.withObject
      "ActivityTaskCanceledEventAttributes"
      ( \x ->
          ActivityTaskCanceledEventAttributes'
            Prelude.<$> (x Prelude..:? "latestCancelRequestedEventId")
            Prelude.<*> (x Prelude..:? "details")
            Prelude.<*> (x Prelude..: "scheduledEventId")
            Prelude.<*> (x Prelude..: "startedEventId")
      )

instance
  Prelude.Hashable
    ActivityTaskCanceledEventAttributes

instance
  Prelude.NFData
    ActivityTaskCanceledEventAttributes
