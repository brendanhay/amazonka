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
-- Module      : Amazonka.SSM.CreateMaintenanceWindow
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new maintenance window.
--
-- The value you specify for @Duration@ determines the specific end time
-- for the maintenance window based on the time it begins. No maintenance
-- window tasks are permitted to start after the resulting endtime minus
-- the number of hours you specify for @Cutoff@. For example, if the
-- maintenance window starts at 3 PM, the duration is three hours, and the
-- value you specify for @Cutoff@ is one hour, no maintenance window tasks
-- can start after 5 PM.
module Amazonka.SSM.CreateMaintenanceWindow
  ( -- * Creating a Request
    CreateMaintenanceWindow (..),
    newCreateMaintenanceWindow,

    -- * Request Lenses
    createMaintenanceWindow_clientToken,
    createMaintenanceWindow_description,
    createMaintenanceWindow_endDate,
    createMaintenanceWindow_scheduleOffset,
    createMaintenanceWindow_scheduleTimezone,
    createMaintenanceWindow_startDate,
    createMaintenanceWindow_tags,
    createMaintenanceWindow_name,
    createMaintenanceWindow_schedule,
    createMaintenanceWindow_duration,
    createMaintenanceWindow_cutoff,
    createMaintenanceWindow_allowUnassociatedTargets,

    -- * Destructuring the Response
    CreateMaintenanceWindowResponse (..),
    newCreateMaintenanceWindowResponse,

    -- * Response Lenses
    createMaintenanceWindowResponse_windowId,
    createMaintenanceWindowResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newCreateMaintenanceWindow' smart constructor.
data CreateMaintenanceWindow = CreateMaintenanceWindow'
  { -- | User-provided idempotency token.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | An optional description for the maintenance window. We recommend
    -- specifying a description to help you organize your maintenance windows.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The date and time, in ISO-8601 Extended format, for when you want the
    -- maintenance window to become inactive. @EndDate@ allows you to set a
    -- date and time in the future when the maintenance window will no longer
    -- run.
    endDate :: Prelude.Maybe Prelude.Text,
    -- | The number of days to wait after the date and time specified by a cron
    -- expression before running the maintenance window.
    --
    -- For example, the following cron expression schedules a maintenance
    -- window to run on the third Tuesday of every month at 11:30 PM.
    --
    -- @cron(30 23 ? * TUE#3 *)@
    --
    -- If the schedule offset is @2@, the maintenance window won\'t run until
    -- two days later.
    scheduleOffset :: Prelude.Maybe Prelude.Natural,
    -- | The time zone that the scheduled maintenance window executions are based
    -- on, in Internet Assigned Numbers Authority (IANA) format. For example:
    -- \"America\/Los_Angeles\", \"UTC\", or \"Asia\/Seoul\". For more
    -- information, see the
    -- <https://www.iana.org/time-zones Time Zone Database> on the IANA
    -- website.
    scheduleTimezone :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in ISO-8601 Extended format, for when you want the
    -- maintenance window to become active. @StartDate@ allows you to delay
    -- activation of the maintenance window until the specified future date.
    startDate :: Prelude.Maybe Prelude.Text,
    -- | Optional metadata that you assign to a resource. Tags enable you to
    -- categorize a resource in different ways, such as by purpose, owner, or
    -- environment. For example, you might want to tag a maintenance window to
    -- identify the type of tasks it will run, the types of targets, and the
    -- environment it will run in. In this case, you could specify the
    -- following key-value pairs:
    --
    -- -   @Key=TaskType,Value=AgentUpdate@
    --
    -- -   @Key=OS,Value=Windows@
    --
    -- -   @Key=Environment,Value=Production@
    --
    -- To add tags to an existing maintenance window, use the AddTagsToResource
    -- operation.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the maintenance window.
    name :: Prelude.Text,
    -- | The schedule of the maintenance window in the form of a cron or rate
    -- expression.
    schedule :: Prelude.Text,
    -- | The duration of the maintenance window in hours.
    duration :: Prelude.Natural,
    -- | The number of hours before the end of the maintenance window that Amazon
    -- Web Services Systems Manager stops scheduling new tasks for execution.
    cutoff :: Prelude.Natural,
    -- | Enables a maintenance window task to run on managed nodes, even if you
    -- haven\'t registered those nodes as targets. If enabled, then you must
    -- specify the unregistered managed nodes (by node ID) when you register a
    -- task with the maintenance window.
    --
    -- If you don\'t enable this option, then you must specify
    -- previously-registered targets when you register a task with the
    -- maintenance window.
    allowUnassociatedTargets :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMaintenanceWindow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createMaintenanceWindow_clientToken' - User-provided idempotency token.
--
-- 'description', 'createMaintenanceWindow_description' - An optional description for the maintenance window. We recommend
-- specifying a description to help you organize your maintenance windows.
--
-- 'endDate', 'createMaintenanceWindow_endDate' - The date and time, in ISO-8601 Extended format, for when you want the
-- maintenance window to become inactive. @EndDate@ allows you to set a
-- date and time in the future when the maintenance window will no longer
-- run.
--
-- 'scheduleOffset', 'createMaintenanceWindow_scheduleOffset' - The number of days to wait after the date and time specified by a cron
-- expression before running the maintenance window.
--
-- For example, the following cron expression schedules a maintenance
-- window to run on the third Tuesday of every month at 11:30 PM.
--
-- @cron(30 23 ? * TUE#3 *)@
--
-- If the schedule offset is @2@, the maintenance window won\'t run until
-- two days later.
--
-- 'scheduleTimezone', 'createMaintenanceWindow_scheduleTimezone' - The time zone that the scheduled maintenance window executions are based
-- on, in Internet Assigned Numbers Authority (IANA) format. For example:
-- \"America\/Los_Angeles\", \"UTC\", or \"Asia\/Seoul\". For more
-- information, see the
-- <https://www.iana.org/time-zones Time Zone Database> on the IANA
-- website.
--
-- 'startDate', 'createMaintenanceWindow_startDate' - The date and time, in ISO-8601 Extended format, for when you want the
-- maintenance window to become active. @StartDate@ allows you to delay
-- activation of the maintenance window until the specified future date.
--
-- 'tags', 'createMaintenanceWindow_tags' - Optional metadata that you assign to a resource. Tags enable you to
-- categorize a resource in different ways, such as by purpose, owner, or
-- environment. For example, you might want to tag a maintenance window to
-- identify the type of tasks it will run, the types of targets, and the
-- environment it will run in. In this case, you could specify the
-- following key-value pairs:
--
-- -   @Key=TaskType,Value=AgentUpdate@
--
-- -   @Key=OS,Value=Windows@
--
-- -   @Key=Environment,Value=Production@
--
-- To add tags to an existing maintenance window, use the AddTagsToResource
-- operation.
--
-- 'name', 'createMaintenanceWindow_name' - The name of the maintenance window.
--
-- 'schedule', 'createMaintenanceWindow_schedule' - The schedule of the maintenance window in the form of a cron or rate
-- expression.
--
-- 'duration', 'createMaintenanceWindow_duration' - The duration of the maintenance window in hours.
--
-- 'cutoff', 'createMaintenanceWindow_cutoff' - The number of hours before the end of the maintenance window that Amazon
-- Web Services Systems Manager stops scheduling new tasks for execution.
--
-- 'allowUnassociatedTargets', 'createMaintenanceWindow_allowUnassociatedTargets' - Enables a maintenance window task to run on managed nodes, even if you
-- haven\'t registered those nodes as targets. If enabled, then you must
-- specify the unregistered managed nodes (by node ID) when you register a
-- task with the maintenance window.
--
-- If you don\'t enable this option, then you must specify
-- previously-registered targets when you register a task with the
-- maintenance window.
newCreateMaintenanceWindow ::
  -- | 'name'
  Prelude.Text ->
  -- | 'schedule'
  Prelude.Text ->
  -- | 'duration'
  Prelude.Natural ->
  -- | 'cutoff'
  Prelude.Natural ->
  -- | 'allowUnassociatedTargets'
  Prelude.Bool ->
  CreateMaintenanceWindow
newCreateMaintenanceWindow
  pName_
  pSchedule_
  pDuration_
  pCutoff_
  pAllowUnassociatedTargets_ =
    CreateMaintenanceWindow'
      { clientToken =
          Prelude.Nothing,
        description = Prelude.Nothing,
        endDate = Prelude.Nothing,
        scheduleOffset = Prelude.Nothing,
        scheduleTimezone = Prelude.Nothing,
        startDate = Prelude.Nothing,
        tags = Prelude.Nothing,
        name = pName_,
        schedule = pSchedule_,
        duration = pDuration_,
        cutoff = pCutoff_,
        allowUnassociatedTargets =
          pAllowUnassociatedTargets_
      }

-- | User-provided idempotency token.
createMaintenanceWindow_clientToken :: Lens.Lens' CreateMaintenanceWindow (Prelude.Maybe Prelude.Text)
createMaintenanceWindow_clientToken = Lens.lens (\CreateMaintenanceWindow' {clientToken} -> clientToken) (\s@CreateMaintenanceWindow' {} a -> s {clientToken = a} :: CreateMaintenanceWindow)

-- | An optional description for the maintenance window. We recommend
-- specifying a description to help you organize your maintenance windows.
createMaintenanceWindow_description :: Lens.Lens' CreateMaintenanceWindow (Prelude.Maybe Prelude.Text)
createMaintenanceWindow_description = Lens.lens (\CreateMaintenanceWindow' {description} -> description) (\s@CreateMaintenanceWindow' {} a -> s {description = a} :: CreateMaintenanceWindow) Prelude.. Lens.mapping Data._Sensitive

-- | The date and time, in ISO-8601 Extended format, for when you want the
-- maintenance window to become inactive. @EndDate@ allows you to set a
-- date and time in the future when the maintenance window will no longer
-- run.
createMaintenanceWindow_endDate :: Lens.Lens' CreateMaintenanceWindow (Prelude.Maybe Prelude.Text)
createMaintenanceWindow_endDate = Lens.lens (\CreateMaintenanceWindow' {endDate} -> endDate) (\s@CreateMaintenanceWindow' {} a -> s {endDate = a} :: CreateMaintenanceWindow)

-- | The number of days to wait after the date and time specified by a cron
-- expression before running the maintenance window.
--
-- For example, the following cron expression schedules a maintenance
-- window to run on the third Tuesday of every month at 11:30 PM.
--
-- @cron(30 23 ? * TUE#3 *)@
--
-- If the schedule offset is @2@, the maintenance window won\'t run until
-- two days later.
createMaintenanceWindow_scheduleOffset :: Lens.Lens' CreateMaintenanceWindow (Prelude.Maybe Prelude.Natural)
createMaintenanceWindow_scheduleOffset = Lens.lens (\CreateMaintenanceWindow' {scheduleOffset} -> scheduleOffset) (\s@CreateMaintenanceWindow' {} a -> s {scheduleOffset = a} :: CreateMaintenanceWindow)

-- | The time zone that the scheduled maintenance window executions are based
-- on, in Internet Assigned Numbers Authority (IANA) format. For example:
-- \"America\/Los_Angeles\", \"UTC\", or \"Asia\/Seoul\". For more
-- information, see the
-- <https://www.iana.org/time-zones Time Zone Database> on the IANA
-- website.
createMaintenanceWindow_scheduleTimezone :: Lens.Lens' CreateMaintenanceWindow (Prelude.Maybe Prelude.Text)
createMaintenanceWindow_scheduleTimezone = Lens.lens (\CreateMaintenanceWindow' {scheduleTimezone} -> scheduleTimezone) (\s@CreateMaintenanceWindow' {} a -> s {scheduleTimezone = a} :: CreateMaintenanceWindow)

-- | The date and time, in ISO-8601 Extended format, for when you want the
-- maintenance window to become active. @StartDate@ allows you to delay
-- activation of the maintenance window until the specified future date.
createMaintenanceWindow_startDate :: Lens.Lens' CreateMaintenanceWindow (Prelude.Maybe Prelude.Text)
createMaintenanceWindow_startDate = Lens.lens (\CreateMaintenanceWindow' {startDate} -> startDate) (\s@CreateMaintenanceWindow' {} a -> s {startDate = a} :: CreateMaintenanceWindow)

-- | Optional metadata that you assign to a resource. Tags enable you to
-- categorize a resource in different ways, such as by purpose, owner, or
-- environment. For example, you might want to tag a maintenance window to
-- identify the type of tasks it will run, the types of targets, and the
-- environment it will run in. In this case, you could specify the
-- following key-value pairs:
--
-- -   @Key=TaskType,Value=AgentUpdate@
--
-- -   @Key=OS,Value=Windows@
--
-- -   @Key=Environment,Value=Production@
--
-- To add tags to an existing maintenance window, use the AddTagsToResource
-- operation.
createMaintenanceWindow_tags :: Lens.Lens' CreateMaintenanceWindow (Prelude.Maybe [Tag])
createMaintenanceWindow_tags = Lens.lens (\CreateMaintenanceWindow' {tags} -> tags) (\s@CreateMaintenanceWindow' {} a -> s {tags = a} :: CreateMaintenanceWindow) Prelude.. Lens.mapping Lens.coerced

-- | The name of the maintenance window.
createMaintenanceWindow_name :: Lens.Lens' CreateMaintenanceWindow Prelude.Text
createMaintenanceWindow_name = Lens.lens (\CreateMaintenanceWindow' {name} -> name) (\s@CreateMaintenanceWindow' {} a -> s {name = a} :: CreateMaintenanceWindow)

-- | The schedule of the maintenance window in the form of a cron or rate
-- expression.
createMaintenanceWindow_schedule :: Lens.Lens' CreateMaintenanceWindow Prelude.Text
createMaintenanceWindow_schedule = Lens.lens (\CreateMaintenanceWindow' {schedule} -> schedule) (\s@CreateMaintenanceWindow' {} a -> s {schedule = a} :: CreateMaintenanceWindow)

-- | The duration of the maintenance window in hours.
createMaintenanceWindow_duration :: Lens.Lens' CreateMaintenanceWindow Prelude.Natural
createMaintenanceWindow_duration = Lens.lens (\CreateMaintenanceWindow' {duration} -> duration) (\s@CreateMaintenanceWindow' {} a -> s {duration = a} :: CreateMaintenanceWindow)

-- | The number of hours before the end of the maintenance window that Amazon
-- Web Services Systems Manager stops scheduling new tasks for execution.
createMaintenanceWindow_cutoff :: Lens.Lens' CreateMaintenanceWindow Prelude.Natural
createMaintenanceWindow_cutoff = Lens.lens (\CreateMaintenanceWindow' {cutoff} -> cutoff) (\s@CreateMaintenanceWindow' {} a -> s {cutoff = a} :: CreateMaintenanceWindow)

-- | Enables a maintenance window task to run on managed nodes, even if you
-- haven\'t registered those nodes as targets. If enabled, then you must
-- specify the unregistered managed nodes (by node ID) when you register a
-- task with the maintenance window.
--
-- If you don\'t enable this option, then you must specify
-- previously-registered targets when you register a task with the
-- maintenance window.
createMaintenanceWindow_allowUnassociatedTargets :: Lens.Lens' CreateMaintenanceWindow Prelude.Bool
createMaintenanceWindow_allowUnassociatedTargets = Lens.lens (\CreateMaintenanceWindow' {allowUnassociatedTargets} -> allowUnassociatedTargets) (\s@CreateMaintenanceWindow' {} a -> s {allowUnassociatedTargets = a} :: CreateMaintenanceWindow)

instance Core.AWSRequest CreateMaintenanceWindow where
  type
    AWSResponse CreateMaintenanceWindow =
      CreateMaintenanceWindowResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateMaintenanceWindowResponse'
            Prelude.<$> (x Data..?> "WindowId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateMaintenanceWindow where
  hashWithSalt _salt CreateMaintenanceWindow' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` endDate
      `Prelude.hashWithSalt` scheduleOffset
      `Prelude.hashWithSalt` scheduleTimezone
      `Prelude.hashWithSalt` startDate
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` schedule
      `Prelude.hashWithSalt` duration
      `Prelude.hashWithSalt` cutoff
      `Prelude.hashWithSalt` allowUnassociatedTargets

instance Prelude.NFData CreateMaintenanceWindow where
  rnf CreateMaintenanceWindow' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf endDate
      `Prelude.seq` Prelude.rnf scheduleOffset
      `Prelude.seq` Prelude.rnf scheduleTimezone
      `Prelude.seq` Prelude.rnf startDate
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf schedule
      `Prelude.seq` Prelude.rnf duration
      `Prelude.seq` Prelude.rnf cutoff
      `Prelude.seq` Prelude.rnf allowUnassociatedTargets

instance Data.ToHeaders CreateMaintenanceWindow where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.CreateMaintenanceWindow" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateMaintenanceWindow where
  toJSON CreateMaintenanceWindow' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("Description" Data..=) Prelude.<$> description,
            ("EndDate" Data..=) Prelude.<$> endDate,
            ("ScheduleOffset" Data..=)
              Prelude.<$> scheduleOffset,
            ("ScheduleTimezone" Data..=)
              Prelude.<$> scheduleTimezone,
            ("StartDate" Data..=) Prelude.<$> startDate,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Schedule" Data..= schedule),
            Prelude.Just ("Duration" Data..= duration),
            Prelude.Just ("Cutoff" Data..= cutoff),
            Prelude.Just
              ( "AllowUnassociatedTargets"
                  Data..= allowUnassociatedTargets
              )
          ]
      )

instance Data.ToPath CreateMaintenanceWindow where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateMaintenanceWindow where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateMaintenanceWindowResponse' smart constructor.
data CreateMaintenanceWindowResponse = CreateMaintenanceWindowResponse'
  { -- | The ID of the created maintenance window.
    windowId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMaintenanceWindowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'windowId', 'createMaintenanceWindowResponse_windowId' - The ID of the created maintenance window.
--
-- 'httpStatus', 'createMaintenanceWindowResponse_httpStatus' - The response's http status code.
newCreateMaintenanceWindowResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateMaintenanceWindowResponse
newCreateMaintenanceWindowResponse pHttpStatus_ =
  CreateMaintenanceWindowResponse'
    { windowId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the created maintenance window.
createMaintenanceWindowResponse_windowId :: Lens.Lens' CreateMaintenanceWindowResponse (Prelude.Maybe Prelude.Text)
createMaintenanceWindowResponse_windowId = Lens.lens (\CreateMaintenanceWindowResponse' {windowId} -> windowId) (\s@CreateMaintenanceWindowResponse' {} a -> s {windowId = a} :: CreateMaintenanceWindowResponse)

-- | The response's http status code.
createMaintenanceWindowResponse_httpStatus :: Lens.Lens' CreateMaintenanceWindowResponse Prelude.Int
createMaintenanceWindowResponse_httpStatus = Lens.lens (\CreateMaintenanceWindowResponse' {httpStatus} -> httpStatus) (\s@CreateMaintenanceWindowResponse' {} a -> s {httpStatus = a} :: CreateMaintenanceWindowResponse)

instance
  Prelude.NFData
    CreateMaintenanceWindowResponse
  where
  rnf CreateMaintenanceWindowResponse' {..} =
    Prelude.rnf windowId
      `Prelude.seq` Prelude.rnf httpStatus
