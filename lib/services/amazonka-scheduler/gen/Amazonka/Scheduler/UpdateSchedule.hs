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
-- Module      : Amazonka.Scheduler.UpdateSchedule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified schedule. When you call @UpdateSchedule@,
-- EventBridge Scheduler uses all values, including empty values, specified
-- in the request and overrides the existing schedule. This is by design.
-- This means that if you do not set an optional field in your request,
-- that field will be set to its system-default value after the update.
--
-- Before calling this operation, we recommend that you call the
-- @GetSchedule@ API operation and make a note of all optional parameters
-- for your @UpdateSchedule@ call.
module Amazonka.Scheduler.UpdateSchedule
  ( -- * Creating a Request
    UpdateSchedule (..),
    newUpdateSchedule,

    -- * Request Lenses
    updateSchedule_scheduleExpressionTimezone,
    updateSchedule_clientToken,
    updateSchedule_endDate,
    updateSchedule_state,
    updateSchedule_groupName,
    updateSchedule_description,
    updateSchedule_kmsKeyArn,
    updateSchedule_startDate,
    updateSchedule_flexibleTimeWindow,
    updateSchedule_name,
    updateSchedule_scheduleExpression,
    updateSchedule_target,

    -- * Destructuring the Response
    UpdateScheduleResponse (..),
    newUpdateScheduleResponse,

    -- * Response Lenses
    updateScheduleResponse_httpStatus,
    updateScheduleResponse_scheduleArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Scheduler.Types

-- | /See:/ 'newUpdateSchedule' smart constructor.
data UpdateSchedule = UpdateSchedule'
  { -- | The timezone in which the scheduling expression is evaluated.
    scheduleExpressionTimezone :: Prelude.Maybe Prelude.Text,
    -- | Unique, case-sensitive identifier you provide to ensure the idempotency
    -- of the request. If you do not specify a client token, EventBridge
    -- Scheduler uses a randomly generated token for the request to ensure
    -- idempotency.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The date, in UTC, before which the schedule can invoke its target.
    -- Depending on the schedule\'s recurrence expression, invocations might
    -- stop on, or before, the @EndDate@ you specify. EventBridge Scheduler
    -- ignores @EndDate@ for one-time schedules.
    endDate :: Prelude.Maybe Core.POSIX,
    -- | Specifies whether the schedule is enabled or disabled.
    state :: Prelude.Maybe ScheduleState,
    -- | The name of the schedule group with which the schedule is associated.
    -- You must provide this value in order for EventBridge Scheduler to find
    -- the schedule you want to update. If you omit this value, EventBridge
    -- Scheduler assumes the group is associated to the default group.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The description you specify for the schedule.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ARN for the customer managed KMS key that that you want EventBridge
    -- Scheduler to use to encrypt and decrypt your data.
    kmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The date, in UTC, after which the schedule can begin invoking its
    -- target. Depending on the schedule\'s recurrence expression, invocations
    -- might occur on, or after, the @StartDate@ you specify. EventBridge
    -- Scheduler ignores @StartDate@ for one-time schedules.
    startDate :: Prelude.Maybe Core.POSIX,
    -- | Allows you to configure a time window during which EventBridge Scheduler
    -- invokes the schedule.
    flexibleTimeWindow :: FlexibleTimeWindow,
    -- | The name of the schedule that you are updating.
    name :: Prelude.Text,
    -- | The expression that defines when the schedule runs. The following
    -- formats are supported.
    --
    -- -   @at@ expression - @at(yyyy-mm-ddThh:mm:ss)@
    --
    -- -   @rate@ expression - @rate(unit value)@
    --
    -- -   @cron@ expression - @cron(fields)@
    --
    -- You can use @at@ expressions to create one-time schedules that invoke a
    -- target once, at the time and in the time zone, that you specify. You can
    -- use @rate@ and @cron@ expressions to create recurring schedules.
    -- Rate-based schedules are useful when you want to invoke a target at
    -- regular intervals, such as every 15 minutes or every five days.
    -- Cron-based schedules are useful when you want to invoke a target
    -- periodically at a specific time, such as at 8:00 am (UTC+0) every 1st
    -- day of the month.
    --
    -- A @cron@ expression consists of six fields separated by white spaces:
    -- @(minutes hours day_of_month month day_of_week year)@.
    --
    -- A @rate@ expression consists of a /value/ as a positive integer, and a
    -- /unit/ with the following options: @minute@ | @minutes@ | @hour@ |
    -- @hours@ | @day@ | @days@
    --
    -- For more information and examples, see
    -- <https://docs.aws.amazon.com/scheduler/latest/UserGuide/schedule-types.html Schedule types on EventBridge Scheduler>
    -- in the /EventBridge Scheduler User Guide/.
    scheduleExpression :: Prelude.Text,
    -- | The schedule target. You can use this operation to change the target
    -- that your schedule invokes.
    target :: Target
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scheduleExpressionTimezone', 'updateSchedule_scheduleExpressionTimezone' - The timezone in which the scheduling expression is evaluated.
--
-- 'clientToken', 'updateSchedule_clientToken' - Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request. If you do not specify a client token, EventBridge
-- Scheduler uses a randomly generated token for the request to ensure
-- idempotency.
--
-- 'endDate', 'updateSchedule_endDate' - The date, in UTC, before which the schedule can invoke its target.
-- Depending on the schedule\'s recurrence expression, invocations might
-- stop on, or before, the @EndDate@ you specify. EventBridge Scheduler
-- ignores @EndDate@ for one-time schedules.
--
-- 'state', 'updateSchedule_state' - Specifies whether the schedule is enabled or disabled.
--
-- 'groupName', 'updateSchedule_groupName' - The name of the schedule group with which the schedule is associated.
-- You must provide this value in order for EventBridge Scheduler to find
-- the schedule you want to update. If you omit this value, EventBridge
-- Scheduler assumes the group is associated to the default group.
--
-- 'description', 'updateSchedule_description' - The description you specify for the schedule.
--
-- 'kmsKeyArn', 'updateSchedule_kmsKeyArn' - The ARN for the customer managed KMS key that that you want EventBridge
-- Scheduler to use to encrypt and decrypt your data.
--
-- 'startDate', 'updateSchedule_startDate' - The date, in UTC, after which the schedule can begin invoking its
-- target. Depending on the schedule\'s recurrence expression, invocations
-- might occur on, or after, the @StartDate@ you specify. EventBridge
-- Scheduler ignores @StartDate@ for one-time schedules.
--
-- 'flexibleTimeWindow', 'updateSchedule_flexibleTimeWindow' - Allows you to configure a time window during which EventBridge Scheduler
-- invokes the schedule.
--
-- 'name', 'updateSchedule_name' - The name of the schedule that you are updating.
--
-- 'scheduleExpression', 'updateSchedule_scheduleExpression' - The expression that defines when the schedule runs. The following
-- formats are supported.
--
-- -   @at@ expression - @at(yyyy-mm-ddThh:mm:ss)@
--
-- -   @rate@ expression - @rate(unit value)@
--
-- -   @cron@ expression - @cron(fields)@
--
-- You can use @at@ expressions to create one-time schedules that invoke a
-- target once, at the time and in the time zone, that you specify. You can
-- use @rate@ and @cron@ expressions to create recurring schedules.
-- Rate-based schedules are useful when you want to invoke a target at
-- regular intervals, such as every 15 minutes or every five days.
-- Cron-based schedules are useful when you want to invoke a target
-- periodically at a specific time, such as at 8:00 am (UTC+0) every 1st
-- day of the month.
--
-- A @cron@ expression consists of six fields separated by white spaces:
-- @(minutes hours day_of_month month day_of_week year)@.
--
-- A @rate@ expression consists of a /value/ as a positive integer, and a
-- /unit/ with the following options: @minute@ | @minutes@ | @hour@ |
-- @hours@ | @day@ | @days@
--
-- For more information and examples, see
-- <https://docs.aws.amazon.com/scheduler/latest/UserGuide/schedule-types.html Schedule types on EventBridge Scheduler>
-- in the /EventBridge Scheduler User Guide/.
--
-- 'target', 'updateSchedule_target' - The schedule target. You can use this operation to change the target
-- that your schedule invokes.
newUpdateSchedule ::
  -- | 'flexibleTimeWindow'
  FlexibleTimeWindow ->
  -- | 'name'
  Prelude.Text ->
  -- | 'scheduleExpression'
  Prelude.Text ->
  -- | 'target'
  Target ->
  UpdateSchedule
newUpdateSchedule
  pFlexibleTimeWindow_
  pName_
  pScheduleExpression_
  pTarget_ =
    UpdateSchedule'
      { scheduleExpressionTimezone =
          Prelude.Nothing,
        clientToken = Prelude.Nothing,
        endDate = Prelude.Nothing,
        state = Prelude.Nothing,
        groupName = Prelude.Nothing,
        description = Prelude.Nothing,
        kmsKeyArn = Prelude.Nothing,
        startDate = Prelude.Nothing,
        flexibleTimeWindow = pFlexibleTimeWindow_,
        name = pName_,
        scheduleExpression = pScheduleExpression_,
        target = pTarget_
      }

-- | The timezone in which the scheduling expression is evaluated.
updateSchedule_scheduleExpressionTimezone :: Lens.Lens' UpdateSchedule (Prelude.Maybe Prelude.Text)
updateSchedule_scheduleExpressionTimezone = Lens.lens (\UpdateSchedule' {scheduleExpressionTimezone} -> scheduleExpressionTimezone) (\s@UpdateSchedule' {} a -> s {scheduleExpressionTimezone = a} :: UpdateSchedule)

-- | Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request. If you do not specify a client token, EventBridge
-- Scheduler uses a randomly generated token for the request to ensure
-- idempotency.
updateSchedule_clientToken :: Lens.Lens' UpdateSchedule (Prelude.Maybe Prelude.Text)
updateSchedule_clientToken = Lens.lens (\UpdateSchedule' {clientToken} -> clientToken) (\s@UpdateSchedule' {} a -> s {clientToken = a} :: UpdateSchedule)

-- | The date, in UTC, before which the schedule can invoke its target.
-- Depending on the schedule\'s recurrence expression, invocations might
-- stop on, or before, the @EndDate@ you specify. EventBridge Scheduler
-- ignores @EndDate@ for one-time schedules.
updateSchedule_endDate :: Lens.Lens' UpdateSchedule (Prelude.Maybe Prelude.UTCTime)
updateSchedule_endDate = Lens.lens (\UpdateSchedule' {endDate} -> endDate) (\s@UpdateSchedule' {} a -> s {endDate = a} :: UpdateSchedule) Prelude.. Lens.mapping Core._Time

-- | Specifies whether the schedule is enabled or disabled.
updateSchedule_state :: Lens.Lens' UpdateSchedule (Prelude.Maybe ScheduleState)
updateSchedule_state = Lens.lens (\UpdateSchedule' {state} -> state) (\s@UpdateSchedule' {} a -> s {state = a} :: UpdateSchedule)

-- | The name of the schedule group with which the schedule is associated.
-- You must provide this value in order for EventBridge Scheduler to find
-- the schedule you want to update. If you omit this value, EventBridge
-- Scheduler assumes the group is associated to the default group.
updateSchedule_groupName :: Lens.Lens' UpdateSchedule (Prelude.Maybe Prelude.Text)
updateSchedule_groupName = Lens.lens (\UpdateSchedule' {groupName} -> groupName) (\s@UpdateSchedule' {} a -> s {groupName = a} :: UpdateSchedule)

-- | The description you specify for the schedule.
updateSchedule_description :: Lens.Lens' UpdateSchedule (Prelude.Maybe Prelude.Text)
updateSchedule_description = Lens.lens (\UpdateSchedule' {description} -> description) (\s@UpdateSchedule' {} a -> s {description = a} :: UpdateSchedule)

-- | The ARN for the customer managed KMS key that that you want EventBridge
-- Scheduler to use to encrypt and decrypt your data.
updateSchedule_kmsKeyArn :: Lens.Lens' UpdateSchedule (Prelude.Maybe Prelude.Text)
updateSchedule_kmsKeyArn = Lens.lens (\UpdateSchedule' {kmsKeyArn} -> kmsKeyArn) (\s@UpdateSchedule' {} a -> s {kmsKeyArn = a} :: UpdateSchedule)

-- | The date, in UTC, after which the schedule can begin invoking its
-- target. Depending on the schedule\'s recurrence expression, invocations
-- might occur on, or after, the @StartDate@ you specify. EventBridge
-- Scheduler ignores @StartDate@ for one-time schedules.
updateSchedule_startDate :: Lens.Lens' UpdateSchedule (Prelude.Maybe Prelude.UTCTime)
updateSchedule_startDate = Lens.lens (\UpdateSchedule' {startDate} -> startDate) (\s@UpdateSchedule' {} a -> s {startDate = a} :: UpdateSchedule) Prelude.. Lens.mapping Core._Time

-- | Allows you to configure a time window during which EventBridge Scheduler
-- invokes the schedule.
updateSchedule_flexibleTimeWindow :: Lens.Lens' UpdateSchedule FlexibleTimeWindow
updateSchedule_flexibleTimeWindow = Lens.lens (\UpdateSchedule' {flexibleTimeWindow} -> flexibleTimeWindow) (\s@UpdateSchedule' {} a -> s {flexibleTimeWindow = a} :: UpdateSchedule)

-- | The name of the schedule that you are updating.
updateSchedule_name :: Lens.Lens' UpdateSchedule Prelude.Text
updateSchedule_name = Lens.lens (\UpdateSchedule' {name} -> name) (\s@UpdateSchedule' {} a -> s {name = a} :: UpdateSchedule)

-- | The expression that defines when the schedule runs. The following
-- formats are supported.
--
-- -   @at@ expression - @at(yyyy-mm-ddThh:mm:ss)@
--
-- -   @rate@ expression - @rate(unit value)@
--
-- -   @cron@ expression - @cron(fields)@
--
-- You can use @at@ expressions to create one-time schedules that invoke a
-- target once, at the time and in the time zone, that you specify. You can
-- use @rate@ and @cron@ expressions to create recurring schedules.
-- Rate-based schedules are useful when you want to invoke a target at
-- regular intervals, such as every 15 minutes or every five days.
-- Cron-based schedules are useful when you want to invoke a target
-- periodically at a specific time, such as at 8:00 am (UTC+0) every 1st
-- day of the month.
--
-- A @cron@ expression consists of six fields separated by white spaces:
-- @(minutes hours day_of_month month day_of_week year)@.
--
-- A @rate@ expression consists of a /value/ as a positive integer, and a
-- /unit/ with the following options: @minute@ | @minutes@ | @hour@ |
-- @hours@ | @day@ | @days@
--
-- For more information and examples, see
-- <https://docs.aws.amazon.com/scheduler/latest/UserGuide/schedule-types.html Schedule types on EventBridge Scheduler>
-- in the /EventBridge Scheduler User Guide/.
updateSchedule_scheduleExpression :: Lens.Lens' UpdateSchedule Prelude.Text
updateSchedule_scheduleExpression = Lens.lens (\UpdateSchedule' {scheduleExpression} -> scheduleExpression) (\s@UpdateSchedule' {} a -> s {scheduleExpression = a} :: UpdateSchedule)

-- | The schedule target. You can use this operation to change the target
-- that your schedule invokes.
updateSchedule_target :: Lens.Lens' UpdateSchedule Target
updateSchedule_target = Lens.lens (\UpdateSchedule' {target} -> target) (\s@UpdateSchedule' {} a -> s {target = a} :: UpdateSchedule)

instance Core.AWSRequest UpdateSchedule where
  type
    AWSResponse UpdateSchedule =
      UpdateScheduleResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateScheduleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "ScheduleArn")
      )

instance Prelude.Hashable UpdateSchedule where
  hashWithSalt _salt UpdateSchedule' {..} =
    _salt
      `Prelude.hashWithSalt` scheduleExpressionTimezone
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` endDate
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` kmsKeyArn
      `Prelude.hashWithSalt` startDate
      `Prelude.hashWithSalt` flexibleTimeWindow
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` scheduleExpression
      `Prelude.hashWithSalt` target

instance Prelude.NFData UpdateSchedule where
  rnf UpdateSchedule' {..} =
    Prelude.rnf scheduleExpressionTimezone
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf endDate
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf kmsKeyArn
      `Prelude.seq` Prelude.rnf startDate
      `Prelude.seq` Prelude.rnf flexibleTimeWindow
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf scheduleExpression
      `Prelude.seq` Prelude.rnf target

instance Core.ToHeaders UpdateSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateSchedule where
  toJSON UpdateSchedule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ScheduleExpressionTimezone" Core..=)
              Prelude.<$> scheduleExpressionTimezone,
            ("ClientToken" Core..=) Prelude.<$> clientToken,
            ("EndDate" Core..=) Prelude.<$> endDate,
            ("State" Core..=) Prelude.<$> state,
            ("GroupName" Core..=) Prelude.<$> groupName,
            ("Description" Core..=) Prelude.<$> description,
            ("KmsKeyArn" Core..=) Prelude.<$> kmsKeyArn,
            ("StartDate" Core..=) Prelude.<$> startDate,
            Prelude.Just
              ("FlexibleTimeWindow" Core..= flexibleTimeWindow),
            Prelude.Just
              ("ScheduleExpression" Core..= scheduleExpression),
            Prelude.Just ("Target" Core..= target)
          ]
      )

instance Core.ToPath UpdateSchedule where
  toPath UpdateSchedule' {..} =
    Prelude.mconcat ["/schedules/", Core.toBS name]

instance Core.ToQuery UpdateSchedule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateScheduleResponse' smart constructor.
data UpdateScheduleResponse = UpdateScheduleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the schedule that you updated.
    scheduleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateScheduleResponse_httpStatus' - The response's http status code.
--
-- 'scheduleArn', 'updateScheduleResponse_scheduleArn' - The Amazon Resource Name (ARN) of the schedule that you updated.
newUpdateScheduleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'scheduleArn'
  Prelude.Text ->
  UpdateScheduleResponse
newUpdateScheduleResponse pHttpStatus_ pScheduleArn_ =
  UpdateScheduleResponse'
    { httpStatus = pHttpStatus_,
      scheduleArn = pScheduleArn_
    }

-- | The response's http status code.
updateScheduleResponse_httpStatus :: Lens.Lens' UpdateScheduleResponse Prelude.Int
updateScheduleResponse_httpStatus = Lens.lens (\UpdateScheduleResponse' {httpStatus} -> httpStatus) (\s@UpdateScheduleResponse' {} a -> s {httpStatus = a} :: UpdateScheduleResponse)

-- | The Amazon Resource Name (ARN) of the schedule that you updated.
updateScheduleResponse_scheduleArn :: Lens.Lens' UpdateScheduleResponse Prelude.Text
updateScheduleResponse_scheduleArn = Lens.lens (\UpdateScheduleResponse' {scheduleArn} -> scheduleArn) (\s@UpdateScheduleResponse' {} a -> s {scheduleArn = a} :: UpdateScheduleResponse)

instance Prelude.NFData UpdateScheduleResponse where
  rnf UpdateScheduleResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf scheduleArn
