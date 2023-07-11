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
-- Module      : Amazonka.Scheduler.GetSchedule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the specified schedule.
module Amazonka.Scheduler.GetSchedule
  ( -- * Creating a Request
    GetSchedule (..),
    newGetSchedule,

    -- * Request Lenses
    getSchedule_groupName,
    getSchedule_name,

    -- * Destructuring the Response
    GetScheduleResponse (..),
    newGetScheduleResponse,

    -- * Response Lenses
    getScheduleResponse_arn,
    getScheduleResponse_creationDate,
    getScheduleResponse_description,
    getScheduleResponse_endDate,
    getScheduleResponse_flexibleTimeWindow,
    getScheduleResponse_groupName,
    getScheduleResponse_kmsKeyArn,
    getScheduleResponse_lastModificationDate,
    getScheduleResponse_name,
    getScheduleResponse_scheduleExpression,
    getScheduleResponse_scheduleExpressionTimezone,
    getScheduleResponse_startDate,
    getScheduleResponse_state,
    getScheduleResponse_target,
    getScheduleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Scheduler.Types

-- | /See:/ 'newGetSchedule' smart constructor.
data GetSchedule = GetSchedule'
  { -- | The name of the schedule group associated with this schedule. If you
    -- omit this, EventBridge Scheduler assumes that the schedule is associated
    -- with the default group.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The name of the schedule to retrieve.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupName', 'getSchedule_groupName' - The name of the schedule group associated with this schedule. If you
-- omit this, EventBridge Scheduler assumes that the schedule is associated
-- with the default group.
--
-- 'name', 'getSchedule_name' - The name of the schedule to retrieve.
newGetSchedule ::
  -- | 'name'
  Prelude.Text ->
  GetSchedule
newGetSchedule pName_ =
  GetSchedule'
    { groupName = Prelude.Nothing,
      name = pName_
    }

-- | The name of the schedule group associated with this schedule. If you
-- omit this, EventBridge Scheduler assumes that the schedule is associated
-- with the default group.
getSchedule_groupName :: Lens.Lens' GetSchedule (Prelude.Maybe Prelude.Text)
getSchedule_groupName = Lens.lens (\GetSchedule' {groupName} -> groupName) (\s@GetSchedule' {} a -> s {groupName = a} :: GetSchedule)

-- | The name of the schedule to retrieve.
getSchedule_name :: Lens.Lens' GetSchedule Prelude.Text
getSchedule_name = Lens.lens (\GetSchedule' {name} -> name) (\s@GetSchedule' {} a -> s {name = a} :: GetSchedule)

instance Core.AWSRequest GetSchedule where
  type AWSResponse GetSchedule = GetScheduleResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetScheduleResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "CreationDate")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "EndDate")
            Prelude.<*> (x Data..?> "FlexibleTimeWindow")
            Prelude.<*> (x Data..?> "GroupName")
            Prelude.<*> (x Data..?> "KmsKeyArn")
            Prelude.<*> (x Data..?> "LastModificationDate")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "ScheduleExpression")
            Prelude.<*> (x Data..?> "ScheduleExpressionTimezone")
            Prelude.<*> (x Data..?> "StartDate")
            Prelude.<*> (x Data..?> "State")
            Prelude.<*> (x Data..?> "Target")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSchedule where
  hashWithSalt _salt GetSchedule' {..} =
    _salt
      `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` name

instance Prelude.NFData GetSchedule where
  rnf GetSchedule' {..} =
    Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders GetSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetSchedule where
  toPath GetSchedule' {..} =
    Prelude.mconcat ["/schedules/", Data.toBS name]

instance Data.ToQuery GetSchedule where
  toQuery GetSchedule' {..} =
    Prelude.mconcat ["groupName" Data.=: groupName]

-- | /See:/ 'newGetScheduleResponse' smart constructor.
data GetScheduleResponse = GetScheduleResponse'
  { -- | The Amazon Resource Name (ARN) of the schedule.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time at which the schedule was created.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The description of the schedule.
    description :: Prelude.Maybe Prelude.Text,
    -- | The date, in UTC, before which the schedule can invoke its target.
    -- Depending on the schedule\'s recurrence expression, invocations might
    -- stop on, or before, the @EndDate@ you specify. EventBridge Scheduler
    -- ignores @EndDate@ for one-time schedules.
    endDate :: Prelude.Maybe Data.POSIX,
    -- | Allows you to configure a time window during which EventBridge Scheduler
    -- invokes the schedule.
    flexibleTimeWindow :: Prelude.Maybe FlexibleTimeWindow,
    -- | The name of the schedule group associated with this schedule.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The ARN for a customer managed KMS Key that is be used to encrypt and
    -- decrypt your data.
    kmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The time at which the schedule was last modified.
    lastModificationDate :: Prelude.Maybe Data.POSIX,
    -- | The name of the schedule.
    name :: Prelude.Maybe Prelude.Text,
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
    scheduleExpression :: Prelude.Maybe Prelude.Text,
    -- | The timezone in which the scheduling expression is evaluated.
    scheduleExpressionTimezone :: Prelude.Maybe Prelude.Text,
    -- | The date, in UTC, after which the schedule can begin invoking its
    -- target. Depending on the schedule\'s recurrence expression, invocations
    -- might occur on, or after, the @StartDate@ you specify. EventBridge
    -- Scheduler ignores @StartDate@ for one-time schedules.
    startDate :: Prelude.Maybe Data.POSIX,
    -- | Specifies whether the schedule is enabled or disabled.
    state :: Prelude.Maybe ScheduleState,
    -- | The schedule target.
    target :: Prelude.Maybe Target,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getScheduleResponse_arn' - The Amazon Resource Name (ARN) of the schedule.
--
-- 'creationDate', 'getScheduleResponse_creationDate' - The time at which the schedule was created.
--
-- 'description', 'getScheduleResponse_description' - The description of the schedule.
--
-- 'endDate', 'getScheduleResponse_endDate' - The date, in UTC, before which the schedule can invoke its target.
-- Depending on the schedule\'s recurrence expression, invocations might
-- stop on, or before, the @EndDate@ you specify. EventBridge Scheduler
-- ignores @EndDate@ for one-time schedules.
--
-- 'flexibleTimeWindow', 'getScheduleResponse_flexibleTimeWindow' - Allows you to configure a time window during which EventBridge Scheduler
-- invokes the schedule.
--
-- 'groupName', 'getScheduleResponse_groupName' - The name of the schedule group associated with this schedule.
--
-- 'kmsKeyArn', 'getScheduleResponse_kmsKeyArn' - The ARN for a customer managed KMS Key that is be used to encrypt and
-- decrypt your data.
--
-- 'lastModificationDate', 'getScheduleResponse_lastModificationDate' - The time at which the schedule was last modified.
--
-- 'name', 'getScheduleResponse_name' - The name of the schedule.
--
-- 'scheduleExpression', 'getScheduleResponse_scheduleExpression' - The expression that defines when the schedule runs. The following
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
-- 'scheduleExpressionTimezone', 'getScheduleResponse_scheduleExpressionTimezone' - The timezone in which the scheduling expression is evaluated.
--
-- 'startDate', 'getScheduleResponse_startDate' - The date, in UTC, after which the schedule can begin invoking its
-- target. Depending on the schedule\'s recurrence expression, invocations
-- might occur on, or after, the @StartDate@ you specify. EventBridge
-- Scheduler ignores @StartDate@ for one-time schedules.
--
-- 'state', 'getScheduleResponse_state' - Specifies whether the schedule is enabled or disabled.
--
-- 'target', 'getScheduleResponse_target' - The schedule target.
--
-- 'httpStatus', 'getScheduleResponse_httpStatus' - The response's http status code.
newGetScheduleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetScheduleResponse
newGetScheduleResponse pHttpStatus_ =
  GetScheduleResponse'
    { arn = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      description = Prelude.Nothing,
      endDate = Prelude.Nothing,
      flexibleTimeWindow = Prelude.Nothing,
      groupName = Prelude.Nothing,
      kmsKeyArn = Prelude.Nothing,
      lastModificationDate = Prelude.Nothing,
      name = Prelude.Nothing,
      scheduleExpression = Prelude.Nothing,
      scheduleExpressionTimezone = Prelude.Nothing,
      startDate = Prelude.Nothing,
      state = Prelude.Nothing,
      target = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the schedule.
getScheduleResponse_arn :: Lens.Lens' GetScheduleResponse (Prelude.Maybe Prelude.Text)
getScheduleResponse_arn = Lens.lens (\GetScheduleResponse' {arn} -> arn) (\s@GetScheduleResponse' {} a -> s {arn = a} :: GetScheduleResponse)

-- | The time at which the schedule was created.
getScheduleResponse_creationDate :: Lens.Lens' GetScheduleResponse (Prelude.Maybe Prelude.UTCTime)
getScheduleResponse_creationDate = Lens.lens (\GetScheduleResponse' {creationDate} -> creationDate) (\s@GetScheduleResponse' {} a -> s {creationDate = a} :: GetScheduleResponse) Prelude.. Lens.mapping Data._Time

-- | The description of the schedule.
getScheduleResponse_description :: Lens.Lens' GetScheduleResponse (Prelude.Maybe Prelude.Text)
getScheduleResponse_description = Lens.lens (\GetScheduleResponse' {description} -> description) (\s@GetScheduleResponse' {} a -> s {description = a} :: GetScheduleResponse)

-- | The date, in UTC, before which the schedule can invoke its target.
-- Depending on the schedule\'s recurrence expression, invocations might
-- stop on, or before, the @EndDate@ you specify. EventBridge Scheduler
-- ignores @EndDate@ for one-time schedules.
getScheduleResponse_endDate :: Lens.Lens' GetScheduleResponse (Prelude.Maybe Prelude.UTCTime)
getScheduleResponse_endDate = Lens.lens (\GetScheduleResponse' {endDate} -> endDate) (\s@GetScheduleResponse' {} a -> s {endDate = a} :: GetScheduleResponse) Prelude.. Lens.mapping Data._Time

-- | Allows you to configure a time window during which EventBridge Scheduler
-- invokes the schedule.
getScheduleResponse_flexibleTimeWindow :: Lens.Lens' GetScheduleResponse (Prelude.Maybe FlexibleTimeWindow)
getScheduleResponse_flexibleTimeWindow = Lens.lens (\GetScheduleResponse' {flexibleTimeWindow} -> flexibleTimeWindow) (\s@GetScheduleResponse' {} a -> s {flexibleTimeWindow = a} :: GetScheduleResponse)

-- | The name of the schedule group associated with this schedule.
getScheduleResponse_groupName :: Lens.Lens' GetScheduleResponse (Prelude.Maybe Prelude.Text)
getScheduleResponse_groupName = Lens.lens (\GetScheduleResponse' {groupName} -> groupName) (\s@GetScheduleResponse' {} a -> s {groupName = a} :: GetScheduleResponse)

-- | The ARN for a customer managed KMS Key that is be used to encrypt and
-- decrypt your data.
getScheduleResponse_kmsKeyArn :: Lens.Lens' GetScheduleResponse (Prelude.Maybe Prelude.Text)
getScheduleResponse_kmsKeyArn = Lens.lens (\GetScheduleResponse' {kmsKeyArn} -> kmsKeyArn) (\s@GetScheduleResponse' {} a -> s {kmsKeyArn = a} :: GetScheduleResponse)

-- | The time at which the schedule was last modified.
getScheduleResponse_lastModificationDate :: Lens.Lens' GetScheduleResponse (Prelude.Maybe Prelude.UTCTime)
getScheduleResponse_lastModificationDate = Lens.lens (\GetScheduleResponse' {lastModificationDate} -> lastModificationDate) (\s@GetScheduleResponse' {} a -> s {lastModificationDate = a} :: GetScheduleResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the schedule.
getScheduleResponse_name :: Lens.Lens' GetScheduleResponse (Prelude.Maybe Prelude.Text)
getScheduleResponse_name = Lens.lens (\GetScheduleResponse' {name} -> name) (\s@GetScheduleResponse' {} a -> s {name = a} :: GetScheduleResponse)

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
getScheduleResponse_scheduleExpression :: Lens.Lens' GetScheduleResponse (Prelude.Maybe Prelude.Text)
getScheduleResponse_scheduleExpression = Lens.lens (\GetScheduleResponse' {scheduleExpression} -> scheduleExpression) (\s@GetScheduleResponse' {} a -> s {scheduleExpression = a} :: GetScheduleResponse)

-- | The timezone in which the scheduling expression is evaluated.
getScheduleResponse_scheduleExpressionTimezone :: Lens.Lens' GetScheduleResponse (Prelude.Maybe Prelude.Text)
getScheduleResponse_scheduleExpressionTimezone = Lens.lens (\GetScheduleResponse' {scheduleExpressionTimezone} -> scheduleExpressionTimezone) (\s@GetScheduleResponse' {} a -> s {scheduleExpressionTimezone = a} :: GetScheduleResponse)

-- | The date, in UTC, after which the schedule can begin invoking its
-- target. Depending on the schedule\'s recurrence expression, invocations
-- might occur on, or after, the @StartDate@ you specify. EventBridge
-- Scheduler ignores @StartDate@ for one-time schedules.
getScheduleResponse_startDate :: Lens.Lens' GetScheduleResponse (Prelude.Maybe Prelude.UTCTime)
getScheduleResponse_startDate = Lens.lens (\GetScheduleResponse' {startDate} -> startDate) (\s@GetScheduleResponse' {} a -> s {startDate = a} :: GetScheduleResponse) Prelude.. Lens.mapping Data._Time

-- | Specifies whether the schedule is enabled or disabled.
getScheduleResponse_state :: Lens.Lens' GetScheduleResponse (Prelude.Maybe ScheduleState)
getScheduleResponse_state = Lens.lens (\GetScheduleResponse' {state} -> state) (\s@GetScheduleResponse' {} a -> s {state = a} :: GetScheduleResponse)

-- | The schedule target.
getScheduleResponse_target :: Lens.Lens' GetScheduleResponse (Prelude.Maybe Target)
getScheduleResponse_target = Lens.lens (\GetScheduleResponse' {target} -> target) (\s@GetScheduleResponse' {} a -> s {target = a} :: GetScheduleResponse)

-- | The response's http status code.
getScheduleResponse_httpStatus :: Lens.Lens' GetScheduleResponse Prelude.Int
getScheduleResponse_httpStatus = Lens.lens (\GetScheduleResponse' {httpStatus} -> httpStatus) (\s@GetScheduleResponse' {} a -> s {httpStatus = a} :: GetScheduleResponse)

instance Prelude.NFData GetScheduleResponse where
  rnf GetScheduleResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf endDate
      `Prelude.seq` Prelude.rnf flexibleTimeWindow
      `Prelude.seq` Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf kmsKeyArn
      `Prelude.seq` Prelude.rnf lastModificationDate
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf scheduleExpression
      `Prelude.seq` Prelude.rnf scheduleExpressionTimezone
      `Prelude.seq` Prelude.rnf startDate
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf target
      `Prelude.seq` Prelude.rnf httpStatus
