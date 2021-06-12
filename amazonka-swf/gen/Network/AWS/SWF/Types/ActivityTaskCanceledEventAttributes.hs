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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides the details of the @ActivityTaskCanceled@ event.
--
-- /See:/ 'newActivityTaskCanceledEventAttributes' smart constructor.
data ActivityTaskCanceledEventAttributes = ActivityTaskCanceledEventAttributes'
  { -- | If set, contains the ID of the last @ActivityTaskCancelRequested@ event
    -- recorded for this activity task. This information can be useful for
    -- diagnosing problems by tracing back the chain of events leading up to
    -- this event.
    latestCancelRequestedEventId :: Core.Maybe Core.Integer,
    -- | Details of the cancellation.
    details :: Core.Maybe Core.Text,
    -- | The ID of the @ActivityTaskScheduled@ event that was recorded when this
    -- activity task was scheduled. This information can be useful for
    -- diagnosing problems by tracing back the chain of events leading up to
    -- this event.
    scheduledEventId :: Core.Integer,
    -- | The ID of the @ActivityTaskStarted@ event recorded when this activity
    -- task was started. This information can be useful for diagnosing problems
    -- by tracing back the chain of events leading up to this event.
    startedEventId :: Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Integer ->
  -- | 'startedEventId'
  Core.Integer ->
  ActivityTaskCanceledEventAttributes
newActivityTaskCanceledEventAttributes
  pScheduledEventId_
  pStartedEventId_ =
    ActivityTaskCanceledEventAttributes'
      { latestCancelRequestedEventId =
          Core.Nothing,
        details = Core.Nothing,
        scheduledEventId = pScheduledEventId_,
        startedEventId = pStartedEventId_
      }

-- | If set, contains the ID of the last @ActivityTaskCancelRequested@ event
-- recorded for this activity task. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
activityTaskCanceledEventAttributes_latestCancelRequestedEventId :: Lens.Lens' ActivityTaskCanceledEventAttributes (Core.Maybe Core.Integer)
activityTaskCanceledEventAttributes_latestCancelRequestedEventId = Lens.lens (\ActivityTaskCanceledEventAttributes' {latestCancelRequestedEventId} -> latestCancelRequestedEventId) (\s@ActivityTaskCanceledEventAttributes' {} a -> s {latestCancelRequestedEventId = a} :: ActivityTaskCanceledEventAttributes)

-- | Details of the cancellation.
activityTaskCanceledEventAttributes_details :: Lens.Lens' ActivityTaskCanceledEventAttributes (Core.Maybe Core.Text)
activityTaskCanceledEventAttributes_details = Lens.lens (\ActivityTaskCanceledEventAttributes' {details} -> details) (\s@ActivityTaskCanceledEventAttributes' {} a -> s {details = a} :: ActivityTaskCanceledEventAttributes)

-- | The ID of the @ActivityTaskScheduled@ event that was recorded when this
-- activity task was scheduled. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
activityTaskCanceledEventAttributes_scheduledEventId :: Lens.Lens' ActivityTaskCanceledEventAttributes Core.Integer
activityTaskCanceledEventAttributes_scheduledEventId = Lens.lens (\ActivityTaskCanceledEventAttributes' {scheduledEventId} -> scheduledEventId) (\s@ActivityTaskCanceledEventAttributes' {} a -> s {scheduledEventId = a} :: ActivityTaskCanceledEventAttributes)

-- | The ID of the @ActivityTaskStarted@ event recorded when this activity
-- task was started. This information can be useful for diagnosing problems
-- by tracing back the chain of events leading up to this event.
activityTaskCanceledEventAttributes_startedEventId :: Lens.Lens' ActivityTaskCanceledEventAttributes Core.Integer
activityTaskCanceledEventAttributes_startedEventId = Lens.lens (\ActivityTaskCanceledEventAttributes' {startedEventId} -> startedEventId) (\s@ActivityTaskCanceledEventAttributes' {} a -> s {startedEventId = a} :: ActivityTaskCanceledEventAttributes)

instance
  Core.FromJSON
    ActivityTaskCanceledEventAttributes
  where
  parseJSON =
    Core.withObject
      "ActivityTaskCanceledEventAttributes"
      ( \x ->
          ActivityTaskCanceledEventAttributes'
            Core.<$> (x Core..:? "latestCancelRequestedEventId")
            Core.<*> (x Core..:? "details")
            Core.<*> (x Core..: "scheduledEventId")
            Core.<*> (x Core..: "startedEventId")
      )

instance
  Core.Hashable
    ActivityTaskCanceledEventAttributes

instance
  Core.NFData
    ActivityTaskCanceledEventAttributes
