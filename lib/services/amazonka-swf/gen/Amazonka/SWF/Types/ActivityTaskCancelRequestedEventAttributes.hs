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
-- Module      : Amazonka.SWF.Types.ActivityTaskCancelRequestedEventAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.ActivityTaskCancelRequestedEventAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Data.FromJSON
    ActivityTaskCancelRequestedEventAttributes
  where
  parseJSON =
    Data.withObject
      "ActivityTaskCancelRequestedEventAttributes"
      ( \x ->
          ActivityTaskCancelRequestedEventAttributes'
            Prelude.<$> (x Data..: "decisionTaskCompletedEventId")
              Prelude.<*> (x Data..: "activityId")
      )

instance
  Prelude.Hashable
    ActivityTaskCancelRequestedEventAttributes
  where
  hashWithSalt
    _salt
    ActivityTaskCancelRequestedEventAttributes' {..} =
      _salt
        `Prelude.hashWithSalt` decisionTaskCompletedEventId
        `Prelude.hashWithSalt` activityId

instance
  Prelude.NFData
    ActivityTaskCancelRequestedEventAttributes
  where
  rnf ActivityTaskCancelRequestedEventAttributes' {..} =
    Prelude.rnf decisionTaskCompletedEventId
      `Prelude.seq` Prelude.rnf activityId
