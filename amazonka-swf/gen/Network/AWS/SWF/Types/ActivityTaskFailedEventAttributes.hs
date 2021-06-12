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
-- Module      : Network.AWS.SWF.Types.ActivityTaskFailedEventAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ActivityTaskFailedEventAttributes where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides the details of the @ActivityTaskFailed@ event.
--
-- /See:/ 'newActivityTaskFailedEventAttributes' smart constructor.
data ActivityTaskFailedEventAttributes = ActivityTaskFailedEventAttributes'
  { -- | The details of the failure.
    details :: Core.Maybe Core.Text,
    -- | The reason provided for the failure.
    reason :: Core.Maybe Core.Text,
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
-- Create a value of 'ActivityTaskFailedEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'details', 'activityTaskFailedEventAttributes_details' - The details of the failure.
--
-- 'reason', 'activityTaskFailedEventAttributes_reason' - The reason provided for the failure.
--
-- 'scheduledEventId', 'activityTaskFailedEventAttributes_scheduledEventId' - The ID of the @ActivityTaskScheduled@ event that was recorded when this
-- activity task was scheduled. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
--
-- 'startedEventId', 'activityTaskFailedEventAttributes_startedEventId' - The ID of the @ActivityTaskStarted@ event recorded when this activity
-- task was started. This information can be useful for diagnosing problems
-- by tracing back the chain of events leading up to this event.
newActivityTaskFailedEventAttributes ::
  -- | 'scheduledEventId'
  Core.Integer ->
  -- | 'startedEventId'
  Core.Integer ->
  ActivityTaskFailedEventAttributes
newActivityTaskFailedEventAttributes
  pScheduledEventId_
  pStartedEventId_ =
    ActivityTaskFailedEventAttributes'
      { details =
          Core.Nothing,
        reason = Core.Nothing,
        scheduledEventId = pScheduledEventId_,
        startedEventId = pStartedEventId_
      }

-- | The details of the failure.
activityTaskFailedEventAttributes_details :: Lens.Lens' ActivityTaskFailedEventAttributes (Core.Maybe Core.Text)
activityTaskFailedEventAttributes_details = Lens.lens (\ActivityTaskFailedEventAttributes' {details} -> details) (\s@ActivityTaskFailedEventAttributes' {} a -> s {details = a} :: ActivityTaskFailedEventAttributes)

-- | The reason provided for the failure.
activityTaskFailedEventAttributes_reason :: Lens.Lens' ActivityTaskFailedEventAttributes (Core.Maybe Core.Text)
activityTaskFailedEventAttributes_reason = Lens.lens (\ActivityTaskFailedEventAttributes' {reason} -> reason) (\s@ActivityTaskFailedEventAttributes' {} a -> s {reason = a} :: ActivityTaskFailedEventAttributes)

-- | The ID of the @ActivityTaskScheduled@ event that was recorded when this
-- activity task was scheduled. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
activityTaskFailedEventAttributes_scheduledEventId :: Lens.Lens' ActivityTaskFailedEventAttributes Core.Integer
activityTaskFailedEventAttributes_scheduledEventId = Lens.lens (\ActivityTaskFailedEventAttributes' {scheduledEventId} -> scheduledEventId) (\s@ActivityTaskFailedEventAttributes' {} a -> s {scheduledEventId = a} :: ActivityTaskFailedEventAttributes)

-- | The ID of the @ActivityTaskStarted@ event recorded when this activity
-- task was started. This information can be useful for diagnosing problems
-- by tracing back the chain of events leading up to this event.
activityTaskFailedEventAttributes_startedEventId :: Lens.Lens' ActivityTaskFailedEventAttributes Core.Integer
activityTaskFailedEventAttributes_startedEventId = Lens.lens (\ActivityTaskFailedEventAttributes' {startedEventId} -> startedEventId) (\s@ActivityTaskFailedEventAttributes' {} a -> s {startedEventId = a} :: ActivityTaskFailedEventAttributes)

instance
  Core.FromJSON
    ActivityTaskFailedEventAttributes
  where
  parseJSON =
    Core.withObject
      "ActivityTaskFailedEventAttributes"
      ( \x ->
          ActivityTaskFailedEventAttributes'
            Core.<$> (x Core..:? "details")
            Core.<*> (x Core..:? "reason")
            Core.<*> (x Core..: "scheduledEventId")
            Core.<*> (x Core..: "startedEventId")
      )

instance
  Core.Hashable
    ActivityTaskFailedEventAttributes

instance
  Core.NFData
    ActivityTaskFailedEventAttributes
