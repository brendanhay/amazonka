{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.OpenSearch.UpdateScheduledAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reschedules a planned domain configuration change for a later time. This
-- change can be a scheduled
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/service-software.html service software update>
-- or a
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/auto-tune.html#auto-tune-types blue\/green Auto-Tune enhancement>.
module Amazonka.OpenSearch.UpdateScheduledAction
  ( -- * Creating a Request
    UpdateScheduledAction (..),
    newUpdateScheduledAction,

    -- * Request Lenses
    updateScheduledAction_desiredStartTime,
    updateScheduledAction_domainName,
    updateScheduledAction_actionID,
    updateScheduledAction_actionType,
    updateScheduledAction_scheduleAt,

    -- * Destructuring the Response
    UpdateScheduledActionResponse (..),
    newUpdateScheduledActionResponse,

    -- * Response Lenses
    updateScheduledActionResponse_scheduledAction,
    updateScheduledActionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateScheduledAction' smart constructor.
data UpdateScheduledAction = UpdateScheduledAction'
  { -- | The time to implement the change, in Coordinated Universal Time (UTC).
    -- Only specify this parameter if you set @ScheduleAt@ to @TIMESTAMP@.
    desiredStartTime :: Prelude.Maybe Prelude.Integer,
    -- | The name of the domain to reschedule an action for.
    domainName :: Prelude.Text,
    -- | The unique identifier of the action to reschedule. To retrieve this ID,
    -- send a
    -- <https://docs.aws.amazon.com/opensearch-service/latest/APIReference/API_ListScheduledActions.html ListScheduledActions>
    -- request.
    actionID :: Prelude.Text,
    -- | The type of action to reschedule. Can be one of
    -- @SERVICE_SOFTWARE_UPDATE@, @JVM_HEAP_SIZE_TUNING@, or
    -- @JVM_YOUNG_GEN_TUNING@. To retrieve this value, send a
    -- <https://docs.aws.amazon.com/opensearch-service/latest/APIReference/API_ListScheduledActions.html ListScheduledActions>
    -- request.
    actionType :: ActionType,
    -- | When to schedule the action.
    --
    -- -   @NOW@ - Immediately schedules the update to happen in the current
    --     hour if there\'s capacity available.
    --
    -- -   @TIMESTAMP@ - Lets you specify a custom date and time to apply the
    --     update. If you specify this value, you must also provide a value for
    --     @DesiredStartTime@.
    --
    -- -   @OFF_PEAK_WINDOW@ - Marks the action to be picked up during an
    --     upcoming off-peak window. There\'s no guarantee that the change will
    --     be implemented during the next immediate window. Depending on
    --     capacity, it might happen in subsequent days.
    scheduleAt :: ScheduleAt
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateScheduledAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'desiredStartTime', 'updateScheduledAction_desiredStartTime' - The time to implement the change, in Coordinated Universal Time (UTC).
-- Only specify this parameter if you set @ScheduleAt@ to @TIMESTAMP@.
--
-- 'domainName', 'updateScheduledAction_domainName' - The name of the domain to reschedule an action for.
--
-- 'actionID', 'updateScheduledAction_actionID' - The unique identifier of the action to reschedule. To retrieve this ID,
-- send a
-- <https://docs.aws.amazon.com/opensearch-service/latest/APIReference/API_ListScheduledActions.html ListScheduledActions>
-- request.
--
-- 'actionType', 'updateScheduledAction_actionType' - The type of action to reschedule. Can be one of
-- @SERVICE_SOFTWARE_UPDATE@, @JVM_HEAP_SIZE_TUNING@, or
-- @JVM_YOUNG_GEN_TUNING@. To retrieve this value, send a
-- <https://docs.aws.amazon.com/opensearch-service/latest/APIReference/API_ListScheduledActions.html ListScheduledActions>
-- request.
--
-- 'scheduleAt', 'updateScheduledAction_scheduleAt' - When to schedule the action.
--
-- -   @NOW@ - Immediately schedules the update to happen in the current
--     hour if there\'s capacity available.
--
-- -   @TIMESTAMP@ - Lets you specify a custom date and time to apply the
--     update. If you specify this value, you must also provide a value for
--     @DesiredStartTime@.
--
-- -   @OFF_PEAK_WINDOW@ - Marks the action to be picked up during an
--     upcoming off-peak window. There\'s no guarantee that the change will
--     be implemented during the next immediate window. Depending on
--     capacity, it might happen in subsequent days.
newUpdateScheduledAction ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'actionID'
  Prelude.Text ->
  -- | 'actionType'
  ActionType ->
  -- | 'scheduleAt'
  ScheduleAt ->
  UpdateScheduledAction
newUpdateScheduledAction
  pDomainName_
  pActionID_
  pActionType_
  pScheduleAt_ =
    UpdateScheduledAction'
      { desiredStartTime =
          Prelude.Nothing,
        domainName = pDomainName_,
        actionID = pActionID_,
        actionType = pActionType_,
        scheduleAt = pScheduleAt_
      }

-- | The time to implement the change, in Coordinated Universal Time (UTC).
-- Only specify this parameter if you set @ScheduleAt@ to @TIMESTAMP@.
updateScheduledAction_desiredStartTime :: Lens.Lens' UpdateScheduledAction (Prelude.Maybe Prelude.Integer)
updateScheduledAction_desiredStartTime = Lens.lens (\UpdateScheduledAction' {desiredStartTime} -> desiredStartTime) (\s@UpdateScheduledAction' {} a -> s {desiredStartTime = a} :: UpdateScheduledAction)

-- | The name of the domain to reschedule an action for.
updateScheduledAction_domainName :: Lens.Lens' UpdateScheduledAction Prelude.Text
updateScheduledAction_domainName = Lens.lens (\UpdateScheduledAction' {domainName} -> domainName) (\s@UpdateScheduledAction' {} a -> s {domainName = a} :: UpdateScheduledAction)

-- | The unique identifier of the action to reschedule. To retrieve this ID,
-- send a
-- <https://docs.aws.amazon.com/opensearch-service/latest/APIReference/API_ListScheduledActions.html ListScheduledActions>
-- request.
updateScheduledAction_actionID :: Lens.Lens' UpdateScheduledAction Prelude.Text
updateScheduledAction_actionID = Lens.lens (\UpdateScheduledAction' {actionID} -> actionID) (\s@UpdateScheduledAction' {} a -> s {actionID = a} :: UpdateScheduledAction)

-- | The type of action to reschedule. Can be one of
-- @SERVICE_SOFTWARE_UPDATE@, @JVM_HEAP_SIZE_TUNING@, or
-- @JVM_YOUNG_GEN_TUNING@. To retrieve this value, send a
-- <https://docs.aws.amazon.com/opensearch-service/latest/APIReference/API_ListScheduledActions.html ListScheduledActions>
-- request.
updateScheduledAction_actionType :: Lens.Lens' UpdateScheduledAction ActionType
updateScheduledAction_actionType = Lens.lens (\UpdateScheduledAction' {actionType} -> actionType) (\s@UpdateScheduledAction' {} a -> s {actionType = a} :: UpdateScheduledAction)

-- | When to schedule the action.
--
-- -   @NOW@ - Immediately schedules the update to happen in the current
--     hour if there\'s capacity available.
--
-- -   @TIMESTAMP@ - Lets you specify a custom date and time to apply the
--     update. If you specify this value, you must also provide a value for
--     @DesiredStartTime@.
--
-- -   @OFF_PEAK_WINDOW@ - Marks the action to be picked up during an
--     upcoming off-peak window. There\'s no guarantee that the change will
--     be implemented during the next immediate window. Depending on
--     capacity, it might happen in subsequent days.
updateScheduledAction_scheduleAt :: Lens.Lens' UpdateScheduledAction ScheduleAt
updateScheduledAction_scheduleAt = Lens.lens (\UpdateScheduledAction' {scheduleAt} -> scheduleAt) (\s@UpdateScheduledAction' {} a -> s {scheduleAt = a} :: UpdateScheduledAction)

instance Core.AWSRequest UpdateScheduledAction where
  type
    AWSResponse UpdateScheduledAction =
      UpdateScheduledActionResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateScheduledActionResponse'
            Prelude.<$> (x Data..?> "ScheduledAction")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateScheduledAction where
  hashWithSalt _salt UpdateScheduledAction' {..} =
    _salt
      `Prelude.hashWithSalt` desiredStartTime
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` actionID
      `Prelude.hashWithSalt` actionType
      `Prelude.hashWithSalt` scheduleAt

instance Prelude.NFData UpdateScheduledAction where
  rnf UpdateScheduledAction' {..} =
    Prelude.rnf desiredStartTime
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf actionID
      `Prelude.seq` Prelude.rnf actionType
      `Prelude.seq` Prelude.rnf scheduleAt

instance Data.ToHeaders UpdateScheduledAction where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateScheduledAction where
  toJSON UpdateScheduledAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DesiredStartTime" Data..=)
              Prelude.<$> desiredStartTime,
            Prelude.Just ("ActionID" Data..= actionID),
            Prelude.Just ("ActionType" Data..= actionType),
            Prelude.Just ("ScheduleAt" Data..= scheduleAt)
          ]
      )

instance Data.ToPath UpdateScheduledAction where
  toPath UpdateScheduledAction' {..} =
    Prelude.mconcat
      [ "/2021-01-01/opensearch/domain/",
        Data.toBS domainName,
        "/scheduledAction/update"
      ]

instance Data.ToQuery UpdateScheduledAction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateScheduledActionResponse' smart constructor.
data UpdateScheduledActionResponse = UpdateScheduledActionResponse'
  { -- | Information about the rescheduled action.
    scheduledAction :: Prelude.Maybe ScheduledAction,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateScheduledActionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scheduledAction', 'updateScheduledActionResponse_scheduledAction' - Information about the rescheduled action.
--
-- 'httpStatus', 'updateScheduledActionResponse_httpStatus' - The response's http status code.
newUpdateScheduledActionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateScheduledActionResponse
newUpdateScheduledActionResponse pHttpStatus_ =
  UpdateScheduledActionResponse'
    { scheduledAction =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the rescheduled action.
updateScheduledActionResponse_scheduledAction :: Lens.Lens' UpdateScheduledActionResponse (Prelude.Maybe ScheduledAction)
updateScheduledActionResponse_scheduledAction = Lens.lens (\UpdateScheduledActionResponse' {scheduledAction} -> scheduledAction) (\s@UpdateScheduledActionResponse' {} a -> s {scheduledAction = a} :: UpdateScheduledActionResponse)

-- | The response's http status code.
updateScheduledActionResponse_httpStatus :: Lens.Lens' UpdateScheduledActionResponse Prelude.Int
updateScheduledActionResponse_httpStatus = Lens.lens (\UpdateScheduledActionResponse' {httpStatus} -> httpStatus) (\s@UpdateScheduledActionResponse' {} a -> s {httpStatus = a} :: UpdateScheduledActionResponse)

instance Prelude.NFData UpdateScheduledActionResponse where
  rnf UpdateScheduledActionResponse' {..} =
    Prelude.rnf scheduledAction
      `Prelude.seq` Prelude.rnf httpStatus
