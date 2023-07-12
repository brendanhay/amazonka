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
-- Module      : Amazonka.Scheduler.CreateSchedule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the specified schedule.
module Amazonka.Scheduler.CreateSchedule
  ( -- * Creating a Request
    CreateSchedule (..),
    newCreateSchedule,

    -- * Request Lenses
    createSchedule_clientToken,
    createSchedule_description,
    createSchedule_endDate,
    createSchedule_groupName,
    createSchedule_kmsKeyArn,
    createSchedule_scheduleExpressionTimezone,
    createSchedule_startDate,
    createSchedule_state,
    createSchedule_flexibleTimeWindow,
    createSchedule_name,
    createSchedule_scheduleExpression,
    createSchedule_target,

    -- * Destructuring the Response
    CreateScheduleResponse (..),
    newCreateScheduleResponse,

    -- * Response Lenses
    createScheduleResponse_httpStatus,
    createScheduleResponse_scheduleArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Scheduler.Types

-- | /See:/ 'newCreateSchedule' smart constructor.
data CreateSchedule = CreateSchedule'
  { -- | Unique, case-sensitive identifier you provide to ensure the idempotency
    -- of the request. If you do not specify a client token, EventBridge
    -- Scheduler uses a randomly generated token for the request to ensure
    -- idempotency.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The description you specify for the schedule.
    description :: Prelude.Maybe Prelude.Text,
    -- | The date, in UTC, before which the schedule can invoke its target.
    -- Depending on the schedule\'s recurrence expression, invocations might
    -- stop on, or before, the @EndDate@ you specify. EventBridge Scheduler
    -- ignores @EndDate@ for one-time schedules.
    endDate :: Prelude.Maybe Data.POSIX,
    -- | The name of the schedule group to associate with this schedule. If you
    -- omit this, the default schedule group is used.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the customer managed KMS key that
    -- EventBridge Scheduler will use to encrypt and decrypt your data.
    kmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The timezone in which the scheduling expression is evaluated.
    scheduleExpressionTimezone :: Prelude.Maybe Prelude.Text,
    -- | The date, in UTC, after which the schedule can begin invoking its
    -- target. Depending on the schedule\'s recurrence expression, invocations
    -- might occur on, or after, the @StartDate@ you specify. EventBridge
    -- Scheduler ignores @StartDate@ for one-time schedules.
    startDate :: Prelude.Maybe Data.POSIX,
    -- | Specifies whether the schedule is enabled or disabled.
    state :: Prelude.Maybe ScheduleState,
    -- | Allows you to configure a time window during which EventBridge Scheduler
    -- invokes the schedule.
    flexibleTimeWindow :: FlexibleTimeWindow,
    -- | The name of the schedule that you are creating.
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
    -- | The schedule\'s target.
    target :: Target
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createSchedule_clientToken' - Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request. If you do not specify a client token, EventBridge
-- Scheduler uses a randomly generated token for the request to ensure
-- idempotency.
--
-- 'description', 'createSchedule_description' - The description you specify for the schedule.
--
-- 'endDate', 'createSchedule_endDate' - The date, in UTC, before which the schedule can invoke its target.
-- Depending on the schedule\'s recurrence expression, invocations might
-- stop on, or before, the @EndDate@ you specify. EventBridge Scheduler
-- ignores @EndDate@ for one-time schedules.
--
-- 'groupName', 'createSchedule_groupName' - The name of the schedule group to associate with this schedule. If you
-- omit this, the default schedule group is used.
--
-- 'kmsKeyArn', 'createSchedule_kmsKeyArn' - The Amazon Resource Name (ARN) for the customer managed KMS key that
-- EventBridge Scheduler will use to encrypt and decrypt your data.
--
-- 'scheduleExpressionTimezone', 'createSchedule_scheduleExpressionTimezone' - The timezone in which the scheduling expression is evaluated.
--
-- 'startDate', 'createSchedule_startDate' - The date, in UTC, after which the schedule can begin invoking its
-- target. Depending on the schedule\'s recurrence expression, invocations
-- might occur on, or after, the @StartDate@ you specify. EventBridge
-- Scheduler ignores @StartDate@ for one-time schedules.
--
-- 'state', 'createSchedule_state' - Specifies whether the schedule is enabled or disabled.
--
-- 'flexibleTimeWindow', 'createSchedule_flexibleTimeWindow' - Allows you to configure a time window during which EventBridge Scheduler
-- invokes the schedule.
--
-- 'name', 'createSchedule_name' - The name of the schedule that you are creating.
--
-- 'scheduleExpression', 'createSchedule_scheduleExpression' - The expression that defines when the schedule runs. The following
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
-- 'target', 'createSchedule_target' - The schedule\'s target.
newCreateSchedule ::
  -- | 'flexibleTimeWindow'
  FlexibleTimeWindow ->
  -- | 'name'
  Prelude.Text ->
  -- | 'scheduleExpression'
  Prelude.Text ->
  -- | 'target'
  Target ->
  CreateSchedule
newCreateSchedule
  pFlexibleTimeWindow_
  pName_
  pScheduleExpression_
  pTarget_ =
    CreateSchedule'
      { clientToken = Prelude.Nothing,
        description = Prelude.Nothing,
        endDate = Prelude.Nothing,
        groupName = Prelude.Nothing,
        kmsKeyArn = Prelude.Nothing,
        scheduleExpressionTimezone = Prelude.Nothing,
        startDate = Prelude.Nothing,
        state = Prelude.Nothing,
        flexibleTimeWindow = pFlexibleTimeWindow_,
        name = pName_,
        scheduleExpression = pScheduleExpression_,
        target = pTarget_
      }

-- | Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request. If you do not specify a client token, EventBridge
-- Scheduler uses a randomly generated token for the request to ensure
-- idempotency.
createSchedule_clientToken :: Lens.Lens' CreateSchedule (Prelude.Maybe Prelude.Text)
createSchedule_clientToken = Lens.lens (\CreateSchedule' {clientToken} -> clientToken) (\s@CreateSchedule' {} a -> s {clientToken = a} :: CreateSchedule)

-- | The description you specify for the schedule.
createSchedule_description :: Lens.Lens' CreateSchedule (Prelude.Maybe Prelude.Text)
createSchedule_description = Lens.lens (\CreateSchedule' {description} -> description) (\s@CreateSchedule' {} a -> s {description = a} :: CreateSchedule)

-- | The date, in UTC, before which the schedule can invoke its target.
-- Depending on the schedule\'s recurrence expression, invocations might
-- stop on, or before, the @EndDate@ you specify. EventBridge Scheduler
-- ignores @EndDate@ for one-time schedules.
createSchedule_endDate :: Lens.Lens' CreateSchedule (Prelude.Maybe Prelude.UTCTime)
createSchedule_endDate = Lens.lens (\CreateSchedule' {endDate} -> endDate) (\s@CreateSchedule' {} a -> s {endDate = a} :: CreateSchedule) Prelude.. Lens.mapping Data._Time

-- | The name of the schedule group to associate with this schedule. If you
-- omit this, the default schedule group is used.
createSchedule_groupName :: Lens.Lens' CreateSchedule (Prelude.Maybe Prelude.Text)
createSchedule_groupName = Lens.lens (\CreateSchedule' {groupName} -> groupName) (\s@CreateSchedule' {} a -> s {groupName = a} :: CreateSchedule)

-- | The Amazon Resource Name (ARN) for the customer managed KMS key that
-- EventBridge Scheduler will use to encrypt and decrypt your data.
createSchedule_kmsKeyArn :: Lens.Lens' CreateSchedule (Prelude.Maybe Prelude.Text)
createSchedule_kmsKeyArn = Lens.lens (\CreateSchedule' {kmsKeyArn} -> kmsKeyArn) (\s@CreateSchedule' {} a -> s {kmsKeyArn = a} :: CreateSchedule)

-- | The timezone in which the scheduling expression is evaluated.
createSchedule_scheduleExpressionTimezone :: Lens.Lens' CreateSchedule (Prelude.Maybe Prelude.Text)
createSchedule_scheduleExpressionTimezone = Lens.lens (\CreateSchedule' {scheduleExpressionTimezone} -> scheduleExpressionTimezone) (\s@CreateSchedule' {} a -> s {scheduleExpressionTimezone = a} :: CreateSchedule)

-- | The date, in UTC, after which the schedule can begin invoking its
-- target. Depending on the schedule\'s recurrence expression, invocations
-- might occur on, or after, the @StartDate@ you specify. EventBridge
-- Scheduler ignores @StartDate@ for one-time schedules.
createSchedule_startDate :: Lens.Lens' CreateSchedule (Prelude.Maybe Prelude.UTCTime)
createSchedule_startDate = Lens.lens (\CreateSchedule' {startDate} -> startDate) (\s@CreateSchedule' {} a -> s {startDate = a} :: CreateSchedule) Prelude.. Lens.mapping Data._Time

-- | Specifies whether the schedule is enabled or disabled.
createSchedule_state :: Lens.Lens' CreateSchedule (Prelude.Maybe ScheduleState)
createSchedule_state = Lens.lens (\CreateSchedule' {state} -> state) (\s@CreateSchedule' {} a -> s {state = a} :: CreateSchedule)

-- | Allows you to configure a time window during which EventBridge Scheduler
-- invokes the schedule.
createSchedule_flexibleTimeWindow :: Lens.Lens' CreateSchedule FlexibleTimeWindow
createSchedule_flexibleTimeWindow = Lens.lens (\CreateSchedule' {flexibleTimeWindow} -> flexibleTimeWindow) (\s@CreateSchedule' {} a -> s {flexibleTimeWindow = a} :: CreateSchedule)

-- | The name of the schedule that you are creating.
createSchedule_name :: Lens.Lens' CreateSchedule Prelude.Text
createSchedule_name = Lens.lens (\CreateSchedule' {name} -> name) (\s@CreateSchedule' {} a -> s {name = a} :: CreateSchedule)

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
createSchedule_scheduleExpression :: Lens.Lens' CreateSchedule Prelude.Text
createSchedule_scheduleExpression = Lens.lens (\CreateSchedule' {scheduleExpression} -> scheduleExpression) (\s@CreateSchedule' {} a -> s {scheduleExpression = a} :: CreateSchedule)

-- | The schedule\'s target.
createSchedule_target :: Lens.Lens' CreateSchedule Target
createSchedule_target = Lens.lens (\CreateSchedule' {target} -> target) (\s@CreateSchedule' {} a -> s {target = a} :: CreateSchedule)

instance Core.AWSRequest CreateSchedule where
  type
    AWSResponse CreateSchedule =
      CreateScheduleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateScheduleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ScheduleArn")
      )

instance Prelude.Hashable CreateSchedule where
  hashWithSalt _salt CreateSchedule' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` endDate
      `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` kmsKeyArn
      `Prelude.hashWithSalt` scheduleExpressionTimezone
      `Prelude.hashWithSalt` startDate
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` flexibleTimeWindow
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` scheduleExpression
      `Prelude.hashWithSalt` target

instance Prelude.NFData CreateSchedule where
  rnf CreateSchedule' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf endDate
      `Prelude.seq` Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf kmsKeyArn
      `Prelude.seq` Prelude.rnf scheduleExpressionTimezone
      `Prelude.seq` Prelude.rnf startDate
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf flexibleTimeWindow
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf scheduleExpression
      `Prelude.seq` Prelude.rnf target

instance Data.ToHeaders CreateSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateSchedule where
  toJSON CreateSchedule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("Description" Data..=) Prelude.<$> description,
            ("EndDate" Data..=) Prelude.<$> endDate,
            ("GroupName" Data..=) Prelude.<$> groupName,
            ("KmsKeyArn" Data..=) Prelude.<$> kmsKeyArn,
            ("ScheduleExpressionTimezone" Data..=)
              Prelude.<$> scheduleExpressionTimezone,
            ("StartDate" Data..=) Prelude.<$> startDate,
            ("State" Data..=) Prelude.<$> state,
            Prelude.Just
              ("FlexibleTimeWindow" Data..= flexibleTimeWindow),
            Prelude.Just
              ("ScheduleExpression" Data..= scheduleExpression),
            Prelude.Just ("Target" Data..= target)
          ]
      )

instance Data.ToPath CreateSchedule where
  toPath CreateSchedule' {..} =
    Prelude.mconcat ["/schedules/", Data.toBS name]

instance Data.ToQuery CreateSchedule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateScheduleResponse' smart constructor.
data CreateScheduleResponse = CreateScheduleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the schedule.
    scheduleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createScheduleResponse_httpStatus' - The response's http status code.
--
-- 'scheduleArn', 'createScheduleResponse_scheduleArn' - The Amazon Resource Name (ARN) of the schedule.
newCreateScheduleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'scheduleArn'
  Prelude.Text ->
  CreateScheduleResponse
newCreateScheduleResponse pHttpStatus_ pScheduleArn_ =
  CreateScheduleResponse'
    { httpStatus = pHttpStatus_,
      scheduleArn = pScheduleArn_
    }

-- | The response's http status code.
createScheduleResponse_httpStatus :: Lens.Lens' CreateScheduleResponse Prelude.Int
createScheduleResponse_httpStatus = Lens.lens (\CreateScheduleResponse' {httpStatus} -> httpStatus) (\s@CreateScheduleResponse' {} a -> s {httpStatus = a} :: CreateScheduleResponse)

-- | The Amazon Resource Name (ARN) of the schedule.
createScheduleResponse_scheduleArn :: Lens.Lens' CreateScheduleResponse Prelude.Text
createScheduleResponse_scheduleArn = Lens.lens (\CreateScheduleResponse' {scheduleArn} -> scheduleArn) (\s@CreateScheduleResponse' {} a -> s {scheduleArn = a} :: CreateScheduleResponse)

instance Prelude.NFData CreateScheduleResponse where
  rnf CreateScheduleResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf scheduleArn
