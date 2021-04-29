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
-- Module      : Network.AWS.SWF.Types.ActivityTaskCancelRequestedEventAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ActivityTaskCancelRequestedEventAttributes where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides the details of the @ActivityTaskCancelRequested@ event.
--
-- /See:/ 'newActivityTaskCancelRequestedEventAttributes' smart constructor.
data ActivityTaskCancelRequestedEventAttributes = ActivityTaskCancelRequestedEventAttributes'
  { -- | The ID of the @DecisionTaskCompleted@ event corresponding to the
    -- decision task that resulted in the @RequestCancelActivityTask@ decision
    -- for this cancellation request. This information can be useful for
    -- diagnosing problems by tracing back the chain of events leading up to
    -- this event.
    decisionTaskCompletedEventId :: Prelude.Integer,
    -- | The unique ID of the task.
    activityId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ActivityTaskCancelRequestedEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'decisionTaskCompletedEventId', 'activityTaskCancelRequestedEventAttributes_decisionTaskCompletedEventId' - The ID of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the @RequestCancelActivityTask@ decision
-- for this cancellation request. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
--
-- 'activityId', 'activityTaskCancelRequestedEventAttributes_activityId' - The unique ID of the task.
newActivityTaskCancelRequestedEventAttributes ::
  -- | 'decisionTaskCompletedEventId'
  Prelude.Integer ->
  -- | 'activityId'
  Prelude.Text ->
  ActivityTaskCancelRequestedEventAttributes
newActivityTaskCancelRequestedEventAttributes
  pDecisionTaskCompletedEventId_
  pActivityId_ =
    ActivityTaskCancelRequestedEventAttributes'
      { decisionTaskCompletedEventId =
          pDecisionTaskCompletedEventId_,
        activityId = pActivityId_
      }

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the @RequestCancelActivityTask@ decision
-- for this cancellation request. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
activityTaskCancelRequestedEventAttributes_decisionTaskCompletedEventId :: Lens.Lens' ActivityTaskCancelRequestedEventAttributes Prelude.Integer
activityTaskCancelRequestedEventAttributes_decisionTaskCompletedEventId = Lens.lens (\ActivityTaskCancelRequestedEventAttributes' {decisionTaskCompletedEventId} -> decisionTaskCompletedEventId) (\s@ActivityTaskCancelRequestedEventAttributes' {} a -> s {decisionTaskCompletedEventId = a} :: ActivityTaskCancelRequestedEventAttributes)

-- | The unique ID of the task.
activityTaskCancelRequestedEventAttributes_activityId :: Lens.Lens' ActivityTaskCancelRequestedEventAttributes Prelude.Text
activityTaskCancelRequestedEventAttributes_activityId = Lens.lens (\ActivityTaskCancelRequestedEventAttributes' {activityId} -> activityId) (\s@ActivityTaskCancelRequestedEventAttributes' {} a -> s {activityId = a} :: ActivityTaskCancelRequestedEventAttributes)

instance
  Prelude.FromJSON
    ActivityTaskCancelRequestedEventAttributes
  where
  parseJSON =
    Prelude.withObject
      "ActivityTaskCancelRequestedEventAttributes"
      ( \x ->
          ActivityTaskCancelRequestedEventAttributes'
            Prelude.<$> (x Prelude..: "decisionTaskCompletedEventId")
              Prelude.<*> (x Prelude..: "activityId")
      )

instance
  Prelude.Hashable
    ActivityTaskCancelRequestedEventAttributes

instance
  Prelude.NFData
    ActivityTaskCancelRequestedEventAttributes
