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
-- Module      : Network.AWS.SSM.CreateMaintenanceWindow
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.SSM.CreateMaintenanceWindow
  ( -- * Creating a Request
    CreateMaintenanceWindow (..),
    newCreateMaintenanceWindow,

    -- * Request Lenses
    createMaintenanceWindow_startDate,
    createMaintenanceWindow_scheduleOffset,
    createMaintenanceWindow_tags,
    createMaintenanceWindow_description,
    createMaintenanceWindow_scheduleTimezone,
    createMaintenanceWindow_endDate,
    createMaintenanceWindow_clientToken,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newCreateMaintenanceWindow' smart constructor.
data CreateMaintenanceWindow = CreateMaintenanceWindow'
  { -- | The date and time, in ISO-8601 Extended format, for when you want the
    -- maintenance window to become active. StartDate allows you to delay
    -- activation of the maintenance window until the specified future date.
    startDate :: Core.Maybe Core.Text,
    -- | The number of days to wait after the date and time specified by a CRON
    -- expression before running the maintenance window.
    --
    -- For example, the following cron expression schedules a maintenance
    -- window to run on the third Tuesday of every month at 11:30 PM.
    --
    -- @cron(30 23 ? * TUE#3 *)@
    --
    -- If the schedule offset is @2@, the maintenance window won\'t run until
    -- two days later.
    scheduleOffset :: Core.Maybe Core.Natural,
    -- | Optional metadata that you assign to a resource. Tags enable you to
    -- categorize a resource in different ways, such as by purpose, owner, or
    -- environment. For example, you might want to tag a maintenance window to
    -- identify the type of tasks it will run, the types of targets, and the
    -- environment it will run in. In this case, you could specify the
    -- following key name\/value pairs:
    --
    -- -   @Key=TaskType,Value=AgentUpdate@
    --
    -- -   @Key=OS,Value=Windows@
    --
    -- -   @Key=Environment,Value=Production@
    --
    -- To add tags to an existing maintenance window, use the AddTagsToResource
    -- action.
    tags :: Core.Maybe [Tag],
    -- | An optional description for the maintenance window. We recommend
    -- specifying a description to help you organize your maintenance windows.
    description :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The time zone that the scheduled maintenance window executions are based
    -- on, in Internet Assigned Numbers Authority (IANA) format. For example:
    -- \"America\/Los_Angeles\", \"UTC\", or \"Asia\/Seoul\". For more
    -- information, see the
    -- <https://www.iana.org/time-zones Time Zone Database> on the IANA
    -- website.
    scheduleTimezone :: Core.Maybe Core.Text,
    -- | The date and time, in ISO-8601 Extended format, for when you want the
    -- maintenance window to become inactive. EndDate allows you to set a date
    -- and time in the future when the maintenance window will no longer run.
    endDate :: Core.Maybe Core.Text,
    -- | User-provided idempotency token.
    clientToken :: Core.Maybe Core.Text,
    -- | The name of the maintenance window.
    name :: Core.Text,
    -- | The schedule of the maintenance window in the form of a cron or rate
    -- expression.
    schedule :: Core.Text,
    -- | The duration of the maintenance window in hours.
    duration :: Core.Natural,
    -- | The number of hours before the end of the maintenance window that
    -- Systems Manager stops scheduling new tasks for execution.
    cutoff :: Core.Natural,
    -- | Enables a maintenance window task to run on managed instances, even if
    -- you have not registered those instances as targets. If enabled, then you
    -- must specify the unregistered instances (by instance ID) when you
    -- register a task with the maintenance window.
    --
    -- If you don\'t enable this option, then you must specify
    -- previously-registered targets when you register a task with the
    -- maintenance window.
    allowUnassociatedTargets :: Core.Bool
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateMaintenanceWindow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startDate', 'createMaintenanceWindow_startDate' - The date and time, in ISO-8601 Extended format, for when you want the
-- maintenance window to become active. StartDate allows you to delay
-- activation of the maintenance window until the specified future date.
--
-- 'scheduleOffset', 'createMaintenanceWindow_scheduleOffset' - The number of days to wait after the date and time specified by a CRON
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
-- 'tags', 'createMaintenanceWindow_tags' - Optional metadata that you assign to a resource. Tags enable you to
-- categorize a resource in different ways, such as by purpose, owner, or
-- environment. For example, you might want to tag a maintenance window to
-- identify the type of tasks it will run, the types of targets, and the
-- environment it will run in. In this case, you could specify the
-- following key name\/value pairs:
--
-- -   @Key=TaskType,Value=AgentUpdate@
--
-- -   @Key=OS,Value=Windows@
--
-- -   @Key=Environment,Value=Production@
--
-- To add tags to an existing maintenance window, use the AddTagsToResource
-- action.
--
-- 'description', 'createMaintenanceWindow_description' - An optional description for the maintenance window. We recommend
-- specifying a description to help you organize your maintenance windows.
--
-- 'scheduleTimezone', 'createMaintenanceWindow_scheduleTimezone' - The time zone that the scheduled maintenance window executions are based
-- on, in Internet Assigned Numbers Authority (IANA) format. For example:
-- \"America\/Los_Angeles\", \"UTC\", or \"Asia\/Seoul\". For more
-- information, see the
-- <https://www.iana.org/time-zones Time Zone Database> on the IANA
-- website.
--
-- 'endDate', 'createMaintenanceWindow_endDate' - The date and time, in ISO-8601 Extended format, for when you want the
-- maintenance window to become inactive. EndDate allows you to set a date
-- and time in the future when the maintenance window will no longer run.
--
-- 'clientToken', 'createMaintenanceWindow_clientToken' - User-provided idempotency token.
--
-- 'name', 'createMaintenanceWindow_name' - The name of the maintenance window.
--
-- 'schedule', 'createMaintenanceWindow_schedule' - The schedule of the maintenance window in the form of a cron or rate
-- expression.
--
-- 'duration', 'createMaintenanceWindow_duration' - The duration of the maintenance window in hours.
--
-- 'cutoff', 'createMaintenanceWindow_cutoff' - The number of hours before the end of the maintenance window that
-- Systems Manager stops scheduling new tasks for execution.
--
-- 'allowUnassociatedTargets', 'createMaintenanceWindow_allowUnassociatedTargets' - Enables a maintenance window task to run on managed instances, even if
-- you have not registered those instances as targets. If enabled, then you
-- must specify the unregistered instances (by instance ID) when you
-- register a task with the maintenance window.
--
-- If you don\'t enable this option, then you must specify
-- previously-registered targets when you register a task with the
-- maintenance window.
newCreateMaintenanceWindow ::
  -- | 'name'
  Core.Text ->
  -- | 'schedule'
  Core.Text ->
  -- | 'duration'
  Core.Natural ->
  -- | 'cutoff'
  Core.Natural ->
  -- | 'allowUnassociatedTargets'
  Core.Bool ->
  CreateMaintenanceWindow
newCreateMaintenanceWindow
  pName_
  pSchedule_
  pDuration_
  pCutoff_
  pAllowUnassociatedTargets_ =
    CreateMaintenanceWindow'
      { startDate = Core.Nothing,
        scheduleOffset = Core.Nothing,
        tags = Core.Nothing,
        description = Core.Nothing,
        scheduleTimezone = Core.Nothing,
        endDate = Core.Nothing,
        clientToken = Core.Nothing,
        name = pName_,
        schedule = pSchedule_,
        duration = pDuration_,
        cutoff = pCutoff_,
        allowUnassociatedTargets =
          pAllowUnassociatedTargets_
      }

-- | The date and time, in ISO-8601 Extended format, for when you want the
-- maintenance window to become active. StartDate allows you to delay
-- activation of the maintenance window until the specified future date.
createMaintenanceWindow_startDate :: Lens.Lens' CreateMaintenanceWindow (Core.Maybe Core.Text)
createMaintenanceWindow_startDate = Lens.lens (\CreateMaintenanceWindow' {startDate} -> startDate) (\s@CreateMaintenanceWindow' {} a -> s {startDate = a} :: CreateMaintenanceWindow)

-- | The number of days to wait after the date and time specified by a CRON
-- expression before running the maintenance window.
--
-- For example, the following cron expression schedules a maintenance
-- window to run on the third Tuesday of every month at 11:30 PM.
--
-- @cron(30 23 ? * TUE#3 *)@
--
-- If the schedule offset is @2@, the maintenance window won\'t run until
-- two days later.
createMaintenanceWindow_scheduleOffset :: Lens.Lens' CreateMaintenanceWindow (Core.Maybe Core.Natural)
createMaintenanceWindow_scheduleOffset = Lens.lens (\CreateMaintenanceWindow' {scheduleOffset} -> scheduleOffset) (\s@CreateMaintenanceWindow' {} a -> s {scheduleOffset = a} :: CreateMaintenanceWindow)

-- | Optional metadata that you assign to a resource. Tags enable you to
-- categorize a resource in different ways, such as by purpose, owner, or
-- environment. For example, you might want to tag a maintenance window to
-- identify the type of tasks it will run, the types of targets, and the
-- environment it will run in. In this case, you could specify the
-- following key name\/value pairs:
--
-- -   @Key=TaskType,Value=AgentUpdate@
--
-- -   @Key=OS,Value=Windows@
--
-- -   @Key=Environment,Value=Production@
--
-- To add tags to an existing maintenance window, use the AddTagsToResource
-- action.
createMaintenanceWindow_tags :: Lens.Lens' CreateMaintenanceWindow (Core.Maybe [Tag])
createMaintenanceWindow_tags = Lens.lens (\CreateMaintenanceWindow' {tags} -> tags) (\s@CreateMaintenanceWindow' {} a -> s {tags = a} :: CreateMaintenanceWindow) Core.. Lens.mapping Lens._Coerce

-- | An optional description for the maintenance window. We recommend
-- specifying a description to help you organize your maintenance windows.
createMaintenanceWindow_description :: Lens.Lens' CreateMaintenanceWindow (Core.Maybe Core.Text)
createMaintenanceWindow_description = Lens.lens (\CreateMaintenanceWindow' {description} -> description) (\s@CreateMaintenanceWindow' {} a -> s {description = a} :: CreateMaintenanceWindow) Core.. Lens.mapping Core._Sensitive

-- | The time zone that the scheduled maintenance window executions are based
-- on, in Internet Assigned Numbers Authority (IANA) format. For example:
-- \"America\/Los_Angeles\", \"UTC\", or \"Asia\/Seoul\". For more
-- information, see the
-- <https://www.iana.org/time-zones Time Zone Database> on the IANA
-- website.
createMaintenanceWindow_scheduleTimezone :: Lens.Lens' CreateMaintenanceWindow (Core.Maybe Core.Text)
createMaintenanceWindow_scheduleTimezone = Lens.lens (\CreateMaintenanceWindow' {scheduleTimezone} -> scheduleTimezone) (\s@CreateMaintenanceWindow' {} a -> s {scheduleTimezone = a} :: CreateMaintenanceWindow)

-- | The date and time, in ISO-8601 Extended format, for when you want the
-- maintenance window to become inactive. EndDate allows you to set a date
-- and time in the future when the maintenance window will no longer run.
createMaintenanceWindow_endDate :: Lens.Lens' CreateMaintenanceWindow (Core.Maybe Core.Text)
createMaintenanceWindow_endDate = Lens.lens (\CreateMaintenanceWindow' {endDate} -> endDate) (\s@CreateMaintenanceWindow' {} a -> s {endDate = a} :: CreateMaintenanceWindow)

-- | User-provided idempotency token.
createMaintenanceWindow_clientToken :: Lens.Lens' CreateMaintenanceWindow (Core.Maybe Core.Text)
createMaintenanceWindow_clientToken = Lens.lens (\CreateMaintenanceWindow' {clientToken} -> clientToken) (\s@CreateMaintenanceWindow' {} a -> s {clientToken = a} :: CreateMaintenanceWindow)

-- | The name of the maintenance window.
createMaintenanceWindow_name :: Lens.Lens' CreateMaintenanceWindow Core.Text
createMaintenanceWindow_name = Lens.lens (\CreateMaintenanceWindow' {name} -> name) (\s@CreateMaintenanceWindow' {} a -> s {name = a} :: CreateMaintenanceWindow)

-- | The schedule of the maintenance window in the form of a cron or rate
-- expression.
createMaintenanceWindow_schedule :: Lens.Lens' CreateMaintenanceWindow Core.Text
createMaintenanceWindow_schedule = Lens.lens (\CreateMaintenanceWindow' {schedule} -> schedule) (\s@CreateMaintenanceWindow' {} a -> s {schedule = a} :: CreateMaintenanceWindow)

-- | The duration of the maintenance window in hours.
createMaintenanceWindow_duration :: Lens.Lens' CreateMaintenanceWindow Core.Natural
createMaintenanceWindow_duration = Lens.lens (\CreateMaintenanceWindow' {duration} -> duration) (\s@CreateMaintenanceWindow' {} a -> s {duration = a} :: CreateMaintenanceWindow)

-- | The number of hours before the end of the maintenance window that
-- Systems Manager stops scheduling new tasks for execution.
createMaintenanceWindow_cutoff :: Lens.Lens' CreateMaintenanceWindow Core.Natural
createMaintenanceWindow_cutoff = Lens.lens (\CreateMaintenanceWindow' {cutoff} -> cutoff) (\s@CreateMaintenanceWindow' {} a -> s {cutoff = a} :: CreateMaintenanceWindow)

-- | Enables a maintenance window task to run on managed instances, even if
-- you have not registered those instances as targets. If enabled, then you
-- must specify the unregistered instances (by instance ID) when you
-- register a task with the maintenance window.
--
-- If you don\'t enable this option, then you must specify
-- previously-registered targets when you register a task with the
-- maintenance window.
createMaintenanceWindow_allowUnassociatedTargets :: Lens.Lens' CreateMaintenanceWindow Core.Bool
createMaintenanceWindow_allowUnassociatedTargets = Lens.lens (\CreateMaintenanceWindow' {allowUnassociatedTargets} -> allowUnassociatedTargets) (\s@CreateMaintenanceWindow' {} a -> s {allowUnassociatedTargets = a} :: CreateMaintenanceWindow)

instance Core.AWSRequest CreateMaintenanceWindow where
  type
    AWSResponse CreateMaintenanceWindow =
      CreateMaintenanceWindowResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateMaintenanceWindowResponse'
            Core.<$> (x Core..?> "WindowId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateMaintenanceWindow

instance Core.NFData CreateMaintenanceWindow

instance Core.ToHeaders CreateMaintenanceWindow where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.CreateMaintenanceWindow" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateMaintenanceWindow where
  toJSON CreateMaintenanceWindow' {..} =
    Core.object
      ( Core.catMaybes
          [ ("StartDate" Core..=) Core.<$> startDate,
            ("ScheduleOffset" Core..=) Core.<$> scheduleOffset,
            ("Tags" Core..=) Core.<$> tags,
            ("Description" Core..=) Core.<$> description,
            ("ScheduleTimezone" Core..=)
              Core.<$> scheduleTimezone,
            ("EndDate" Core..=) Core.<$> endDate,
            ("ClientToken" Core..=) Core.<$> clientToken,
            Core.Just ("Name" Core..= name),
            Core.Just ("Schedule" Core..= schedule),
            Core.Just ("Duration" Core..= duration),
            Core.Just ("Cutoff" Core..= cutoff),
            Core.Just
              ( "AllowUnassociatedTargets"
                  Core..= allowUnassociatedTargets
              )
          ]
      )

instance Core.ToPath CreateMaintenanceWindow where
  toPath = Core.const "/"

instance Core.ToQuery CreateMaintenanceWindow where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateMaintenanceWindowResponse' smart constructor.
data CreateMaintenanceWindowResponse = CreateMaintenanceWindowResponse'
  { -- | The ID of the created maintenance window.
    windowId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CreateMaintenanceWindowResponse
newCreateMaintenanceWindowResponse pHttpStatus_ =
  CreateMaintenanceWindowResponse'
    { windowId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the created maintenance window.
createMaintenanceWindowResponse_windowId :: Lens.Lens' CreateMaintenanceWindowResponse (Core.Maybe Core.Text)
createMaintenanceWindowResponse_windowId = Lens.lens (\CreateMaintenanceWindowResponse' {windowId} -> windowId) (\s@CreateMaintenanceWindowResponse' {} a -> s {windowId = a} :: CreateMaintenanceWindowResponse)

-- | The response's http status code.
createMaintenanceWindowResponse_httpStatus :: Lens.Lens' CreateMaintenanceWindowResponse Core.Int
createMaintenanceWindowResponse_httpStatus = Lens.lens (\CreateMaintenanceWindowResponse' {httpStatus} -> httpStatus) (\s@CreateMaintenanceWindowResponse' {} a -> s {httpStatus = a} :: CreateMaintenanceWindowResponse)

instance Core.NFData CreateMaintenanceWindowResponse
