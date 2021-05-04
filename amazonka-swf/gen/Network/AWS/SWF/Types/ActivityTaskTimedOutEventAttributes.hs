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
-- Module      : Network.AWS.SWF.Types.ActivityTaskTimedOutEventAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ActivityTaskTimedOutEventAttributes where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SWF.Types.ActivityTaskTimeoutType

-- | Provides the details of the @ActivityTaskTimedOut@ event.
--
-- /See:/ 'newActivityTaskTimedOutEventAttributes' smart constructor.
data ActivityTaskTimedOutEventAttributes = ActivityTaskTimedOutEventAttributes'
  { -- | Contains the content of the @details@ parameter for the last call made
    -- by the activity to @RecordActivityTaskHeartbeat@.
    details :: Prelude.Maybe Prelude.Text,
    -- | The type of the timeout that caused this event.
    timeoutType :: ActivityTaskTimeoutType,
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
-- Create a value of 'ActivityTaskTimedOutEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'details', 'activityTaskTimedOutEventAttributes_details' - Contains the content of the @details@ parameter for the last call made
-- by the activity to @RecordActivityTaskHeartbeat@.
--
-- 'timeoutType', 'activityTaskTimedOutEventAttributes_timeoutType' - The type of the timeout that caused this event.
--
-- 'scheduledEventId', 'activityTaskTimedOutEventAttributes_scheduledEventId' - The ID of the @ActivityTaskScheduled@ event that was recorded when this
-- activity task was scheduled. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
--
-- 'startedEventId', 'activityTaskTimedOutEventAttributes_startedEventId' - The ID of the @ActivityTaskStarted@ event recorded when this activity
-- task was started. This information can be useful for diagnosing problems
-- by tracing back the chain of events leading up to this event.
newActivityTaskTimedOutEventAttributes ::
  -- | 'timeoutType'
  ActivityTaskTimeoutType ->
  -- | 'scheduledEventId'
  Prelude.Integer ->
  -- | 'startedEventId'
  Prelude.Integer ->
  ActivityTaskTimedOutEventAttributes
newActivityTaskTimedOutEventAttributes
  pTimeoutType_
  pScheduledEventId_
  pStartedEventId_ =
    ActivityTaskTimedOutEventAttributes'
      { details =
          Prelude.Nothing,
        timeoutType = pTimeoutType_,
        scheduledEventId = pScheduledEventId_,
        startedEventId = pStartedEventId_
      }

-- | Contains the content of the @details@ parameter for the last call made
-- by the activity to @RecordActivityTaskHeartbeat@.
activityTaskTimedOutEventAttributes_details :: Lens.Lens' ActivityTaskTimedOutEventAttributes (Prelude.Maybe Prelude.Text)
activityTaskTimedOutEventAttributes_details = Lens.lens (\ActivityTaskTimedOutEventAttributes' {details} -> details) (\s@ActivityTaskTimedOutEventAttributes' {} a -> s {details = a} :: ActivityTaskTimedOutEventAttributes)

-- | The type of the timeout that caused this event.
activityTaskTimedOutEventAttributes_timeoutType :: Lens.Lens' ActivityTaskTimedOutEventAttributes ActivityTaskTimeoutType
activityTaskTimedOutEventAttributes_timeoutType = Lens.lens (\ActivityTaskTimedOutEventAttributes' {timeoutType} -> timeoutType) (\s@ActivityTaskTimedOutEventAttributes' {} a -> s {timeoutType = a} :: ActivityTaskTimedOutEventAttributes)

-- | The ID of the @ActivityTaskScheduled@ event that was recorded when this
-- activity task was scheduled. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
activityTaskTimedOutEventAttributes_scheduledEventId :: Lens.Lens' ActivityTaskTimedOutEventAttributes Prelude.Integer
activityTaskTimedOutEventAttributes_scheduledEventId = Lens.lens (\ActivityTaskTimedOutEventAttributes' {scheduledEventId} -> scheduledEventId) (\s@ActivityTaskTimedOutEventAttributes' {} a -> s {scheduledEventId = a} :: ActivityTaskTimedOutEventAttributes)

-- | The ID of the @ActivityTaskStarted@ event recorded when this activity
-- task was started. This information can be useful for diagnosing problems
-- by tracing back the chain of events leading up to this event.
activityTaskTimedOutEventAttributes_startedEventId :: Lens.Lens' ActivityTaskTimedOutEventAttributes Prelude.Integer
activityTaskTimedOutEventAttributes_startedEventId = Lens.lens (\ActivityTaskTimedOutEventAttributes' {startedEventId} -> startedEventId) (\s@ActivityTaskTimedOutEventAttributes' {} a -> s {startedEventId = a} :: ActivityTaskTimedOutEventAttributes)

instance
  Prelude.FromJSON
    ActivityTaskTimedOutEventAttributes
  where
  parseJSON =
    Prelude.withObject
      "ActivityTaskTimedOutEventAttributes"
      ( \x ->
          ActivityTaskTimedOutEventAttributes'
            Prelude.<$> (x Prelude..:? "details")
            Prelude.<*> (x Prelude..: "timeoutType")
            Prelude.<*> (x Prelude..: "scheduledEventId")
            Prelude.<*> (x Prelude..: "startedEventId")
      )

instance
  Prelude.Hashable
    ActivityTaskTimedOutEventAttributes

instance
  Prelude.NFData
    ActivityTaskTimedOutEventAttributes
