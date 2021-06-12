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
-- Module      : Network.AWS.SSM.GetMaintenanceWindow
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a maintenance window.
module Network.AWS.SSM.GetMaintenanceWindow
  ( -- * Creating a Request
    GetMaintenanceWindow (..),
    newGetMaintenanceWindow,

    -- * Request Lenses
    getMaintenanceWindow_windowId,

    -- * Destructuring the Response
    GetMaintenanceWindowResponse (..),
    newGetMaintenanceWindowResponse,

    -- * Response Lenses
    getMaintenanceWindowResponse_createdDate,
    getMaintenanceWindowResponse_startDate,
    getMaintenanceWindowResponse_duration,
    getMaintenanceWindowResponse_scheduleOffset,
    getMaintenanceWindowResponse_enabled,
    getMaintenanceWindowResponse_modifiedDate,
    getMaintenanceWindowResponse_cutoff,
    getMaintenanceWindowResponse_name,
    getMaintenanceWindowResponse_windowId,
    getMaintenanceWindowResponse_description,
    getMaintenanceWindowResponse_scheduleTimezone,
    getMaintenanceWindowResponse_endDate,
    getMaintenanceWindowResponse_nextExecutionTime,
    getMaintenanceWindowResponse_allowUnassociatedTargets,
    getMaintenanceWindowResponse_schedule,
    getMaintenanceWindowResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newGetMaintenanceWindow' smart constructor.
data GetMaintenanceWindow = GetMaintenanceWindow'
  { -- | The ID of the maintenance window for which you want to retrieve
    -- information.
    windowId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetMaintenanceWindow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'windowId', 'getMaintenanceWindow_windowId' - The ID of the maintenance window for which you want to retrieve
-- information.
newGetMaintenanceWindow ::
  -- | 'windowId'
  Core.Text ->
  GetMaintenanceWindow
newGetMaintenanceWindow pWindowId_ =
  GetMaintenanceWindow' {windowId = pWindowId_}

-- | The ID of the maintenance window for which you want to retrieve
-- information.
getMaintenanceWindow_windowId :: Lens.Lens' GetMaintenanceWindow Core.Text
getMaintenanceWindow_windowId = Lens.lens (\GetMaintenanceWindow' {windowId} -> windowId) (\s@GetMaintenanceWindow' {} a -> s {windowId = a} :: GetMaintenanceWindow)

instance Core.AWSRequest GetMaintenanceWindow where
  type
    AWSResponse GetMaintenanceWindow =
      GetMaintenanceWindowResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMaintenanceWindowResponse'
            Core.<$> (x Core..?> "CreatedDate")
            Core.<*> (x Core..?> "StartDate")
            Core.<*> (x Core..?> "Duration")
            Core.<*> (x Core..?> "ScheduleOffset")
            Core.<*> (x Core..?> "Enabled")
            Core.<*> (x Core..?> "ModifiedDate")
            Core.<*> (x Core..?> "Cutoff")
            Core.<*> (x Core..?> "Name")
            Core.<*> (x Core..?> "WindowId")
            Core.<*> (x Core..?> "Description")
            Core.<*> (x Core..?> "ScheduleTimezone")
            Core.<*> (x Core..?> "EndDate")
            Core.<*> (x Core..?> "NextExecutionTime")
            Core.<*> (x Core..?> "AllowUnassociatedTargets")
            Core.<*> (x Core..?> "Schedule")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetMaintenanceWindow

instance Core.NFData GetMaintenanceWindow

instance Core.ToHeaders GetMaintenanceWindow where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.GetMaintenanceWindow" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetMaintenanceWindow where
  toJSON GetMaintenanceWindow' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("WindowId" Core..= windowId)]
      )

instance Core.ToPath GetMaintenanceWindow where
  toPath = Core.const "/"

instance Core.ToQuery GetMaintenanceWindow where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetMaintenanceWindowResponse' smart constructor.
data GetMaintenanceWindowResponse = GetMaintenanceWindowResponse'
  { -- | The date the maintenance window was created.
    createdDate :: Core.Maybe Core.POSIX,
    -- | The date and time, in ISO-8601 Extended format, for when the maintenance
    -- window is scheduled to become active. The maintenance window will not
    -- run before this specified time.
    startDate :: Core.Maybe Core.Text,
    -- | The duration of the maintenance window in hours.
    duration :: Core.Maybe Core.Natural,
    -- | The number of days to wait to run a maintenance window after the
    -- scheduled CRON expression date and time.
    scheduleOffset :: Core.Maybe Core.Natural,
    -- | Indicates whether the maintenance window is enabled.
    enabled :: Core.Maybe Core.Bool,
    -- | The date the maintenance window was last modified.
    modifiedDate :: Core.Maybe Core.POSIX,
    -- | The number of hours before the end of the maintenance window that
    -- Systems Manager stops scheduling new tasks for execution.
    cutoff :: Core.Maybe Core.Natural,
    -- | The name of the maintenance window.
    name :: Core.Maybe Core.Text,
    -- | The ID of the created maintenance window.
    windowId :: Core.Maybe Core.Text,
    -- | The description of the maintenance window.
    description :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The time zone that the scheduled maintenance window executions are based
    -- on, in Internet Assigned Numbers Authority (IANA) format. For example:
    -- \"America\/Los_Angeles\", \"UTC\", or \"Asia\/Seoul\". For more
    -- information, see the
    -- <https://www.iana.org/time-zones Time Zone Database> on the IANA
    -- website.
    scheduleTimezone :: Core.Maybe Core.Text,
    -- | The date and time, in ISO-8601 Extended format, for when the maintenance
    -- window is scheduled to become inactive. The maintenance window will not
    -- run after this specified time.
    endDate :: Core.Maybe Core.Text,
    -- | The next time the maintenance window will actually run, taking into
    -- account any specified times for the maintenance window to become active
    -- or inactive.
    nextExecutionTime :: Core.Maybe Core.Text,
    -- | Whether targets must be registered with the maintenance window before
    -- tasks can be defined for those targets.
    allowUnassociatedTargets :: Core.Maybe Core.Bool,
    -- | The schedule of the maintenance window in the form of a cron or rate
    -- expression.
    schedule :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetMaintenanceWindowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdDate', 'getMaintenanceWindowResponse_createdDate' - The date the maintenance window was created.
--
-- 'startDate', 'getMaintenanceWindowResponse_startDate' - The date and time, in ISO-8601 Extended format, for when the maintenance
-- window is scheduled to become active. The maintenance window will not
-- run before this specified time.
--
-- 'duration', 'getMaintenanceWindowResponse_duration' - The duration of the maintenance window in hours.
--
-- 'scheduleOffset', 'getMaintenanceWindowResponse_scheduleOffset' - The number of days to wait to run a maintenance window after the
-- scheduled CRON expression date and time.
--
-- 'enabled', 'getMaintenanceWindowResponse_enabled' - Indicates whether the maintenance window is enabled.
--
-- 'modifiedDate', 'getMaintenanceWindowResponse_modifiedDate' - The date the maintenance window was last modified.
--
-- 'cutoff', 'getMaintenanceWindowResponse_cutoff' - The number of hours before the end of the maintenance window that
-- Systems Manager stops scheduling new tasks for execution.
--
-- 'name', 'getMaintenanceWindowResponse_name' - The name of the maintenance window.
--
-- 'windowId', 'getMaintenanceWindowResponse_windowId' - The ID of the created maintenance window.
--
-- 'description', 'getMaintenanceWindowResponse_description' - The description of the maintenance window.
--
-- 'scheduleTimezone', 'getMaintenanceWindowResponse_scheduleTimezone' - The time zone that the scheduled maintenance window executions are based
-- on, in Internet Assigned Numbers Authority (IANA) format. For example:
-- \"America\/Los_Angeles\", \"UTC\", or \"Asia\/Seoul\". For more
-- information, see the
-- <https://www.iana.org/time-zones Time Zone Database> on the IANA
-- website.
--
-- 'endDate', 'getMaintenanceWindowResponse_endDate' - The date and time, in ISO-8601 Extended format, for when the maintenance
-- window is scheduled to become inactive. The maintenance window will not
-- run after this specified time.
--
-- 'nextExecutionTime', 'getMaintenanceWindowResponse_nextExecutionTime' - The next time the maintenance window will actually run, taking into
-- account any specified times for the maintenance window to become active
-- or inactive.
--
-- 'allowUnassociatedTargets', 'getMaintenanceWindowResponse_allowUnassociatedTargets' - Whether targets must be registered with the maintenance window before
-- tasks can be defined for those targets.
--
-- 'schedule', 'getMaintenanceWindowResponse_schedule' - The schedule of the maintenance window in the form of a cron or rate
-- expression.
--
-- 'httpStatus', 'getMaintenanceWindowResponse_httpStatus' - The response's http status code.
newGetMaintenanceWindowResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetMaintenanceWindowResponse
newGetMaintenanceWindowResponse pHttpStatus_ =
  GetMaintenanceWindowResponse'
    { createdDate =
        Core.Nothing,
      startDate = Core.Nothing,
      duration = Core.Nothing,
      scheduleOffset = Core.Nothing,
      enabled = Core.Nothing,
      modifiedDate = Core.Nothing,
      cutoff = Core.Nothing,
      name = Core.Nothing,
      windowId = Core.Nothing,
      description = Core.Nothing,
      scheduleTimezone = Core.Nothing,
      endDate = Core.Nothing,
      nextExecutionTime = Core.Nothing,
      allowUnassociatedTargets = Core.Nothing,
      schedule = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date the maintenance window was created.
getMaintenanceWindowResponse_createdDate :: Lens.Lens' GetMaintenanceWindowResponse (Core.Maybe Core.UTCTime)
getMaintenanceWindowResponse_createdDate = Lens.lens (\GetMaintenanceWindowResponse' {createdDate} -> createdDate) (\s@GetMaintenanceWindowResponse' {} a -> s {createdDate = a} :: GetMaintenanceWindowResponse) Core.. Lens.mapping Core._Time

-- | The date and time, in ISO-8601 Extended format, for when the maintenance
-- window is scheduled to become active. The maintenance window will not
-- run before this specified time.
getMaintenanceWindowResponse_startDate :: Lens.Lens' GetMaintenanceWindowResponse (Core.Maybe Core.Text)
getMaintenanceWindowResponse_startDate = Lens.lens (\GetMaintenanceWindowResponse' {startDate} -> startDate) (\s@GetMaintenanceWindowResponse' {} a -> s {startDate = a} :: GetMaintenanceWindowResponse)

-- | The duration of the maintenance window in hours.
getMaintenanceWindowResponse_duration :: Lens.Lens' GetMaintenanceWindowResponse (Core.Maybe Core.Natural)
getMaintenanceWindowResponse_duration = Lens.lens (\GetMaintenanceWindowResponse' {duration} -> duration) (\s@GetMaintenanceWindowResponse' {} a -> s {duration = a} :: GetMaintenanceWindowResponse)

-- | The number of days to wait to run a maintenance window after the
-- scheduled CRON expression date and time.
getMaintenanceWindowResponse_scheduleOffset :: Lens.Lens' GetMaintenanceWindowResponse (Core.Maybe Core.Natural)
getMaintenanceWindowResponse_scheduleOffset = Lens.lens (\GetMaintenanceWindowResponse' {scheduleOffset} -> scheduleOffset) (\s@GetMaintenanceWindowResponse' {} a -> s {scheduleOffset = a} :: GetMaintenanceWindowResponse)

-- | Indicates whether the maintenance window is enabled.
getMaintenanceWindowResponse_enabled :: Lens.Lens' GetMaintenanceWindowResponse (Core.Maybe Core.Bool)
getMaintenanceWindowResponse_enabled = Lens.lens (\GetMaintenanceWindowResponse' {enabled} -> enabled) (\s@GetMaintenanceWindowResponse' {} a -> s {enabled = a} :: GetMaintenanceWindowResponse)

-- | The date the maintenance window was last modified.
getMaintenanceWindowResponse_modifiedDate :: Lens.Lens' GetMaintenanceWindowResponse (Core.Maybe Core.UTCTime)
getMaintenanceWindowResponse_modifiedDate = Lens.lens (\GetMaintenanceWindowResponse' {modifiedDate} -> modifiedDate) (\s@GetMaintenanceWindowResponse' {} a -> s {modifiedDate = a} :: GetMaintenanceWindowResponse) Core.. Lens.mapping Core._Time

-- | The number of hours before the end of the maintenance window that
-- Systems Manager stops scheduling new tasks for execution.
getMaintenanceWindowResponse_cutoff :: Lens.Lens' GetMaintenanceWindowResponse (Core.Maybe Core.Natural)
getMaintenanceWindowResponse_cutoff = Lens.lens (\GetMaintenanceWindowResponse' {cutoff} -> cutoff) (\s@GetMaintenanceWindowResponse' {} a -> s {cutoff = a} :: GetMaintenanceWindowResponse)

-- | The name of the maintenance window.
getMaintenanceWindowResponse_name :: Lens.Lens' GetMaintenanceWindowResponse (Core.Maybe Core.Text)
getMaintenanceWindowResponse_name = Lens.lens (\GetMaintenanceWindowResponse' {name} -> name) (\s@GetMaintenanceWindowResponse' {} a -> s {name = a} :: GetMaintenanceWindowResponse)

-- | The ID of the created maintenance window.
getMaintenanceWindowResponse_windowId :: Lens.Lens' GetMaintenanceWindowResponse (Core.Maybe Core.Text)
getMaintenanceWindowResponse_windowId = Lens.lens (\GetMaintenanceWindowResponse' {windowId} -> windowId) (\s@GetMaintenanceWindowResponse' {} a -> s {windowId = a} :: GetMaintenanceWindowResponse)

-- | The description of the maintenance window.
getMaintenanceWindowResponse_description :: Lens.Lens' GetMaintenanceWindowResponse (Core.Maybe Core.Text)
getMaintenanceWindowResponse_description = Lens.lens (\GetMaintenanceWindowResponse' {description} -> description) (\s@GetMaintenanceWindowResponse' {} a -> s {description = a} :: GetMaintenanceWindowResponse) Core.. Lens.mapping Core._Sensitive

-- | The time zone that the scheduled maintenance window executions are based
-- on, in Internet Assigned Numbers Authority (IANA) format. For example:
-- \"America\/Los_Angeles\", \"UTC\", or \"Asia\/Seoul\". For more
-- information, see the
-- <https://www.iana.org/time-zones Time Zone Database> on the IANA
-- website.
getMaintenanceWindowResponse_scheduleTimezone :: Lens.Lens' GetMaintenanceWindowResponse (Core.Maybe Core.Text)
getMaintenanceWindowResponse_scheduleTimezone = Lens.lens (\GetMaintenanceWindowResponse' {scheduleTimezone} -> scheduleTimezone) (\s@GetMaintenanceWindowResponse' {} a -> s {scheduleTimezone = a} :: GetMaintenanceWindowResponse)

-- | The date and time, in ISO-8601 Extended format, for when the maintenance
-- window is scheduled to become inactive. The maintenance window will not
-- run after this specified time.
getMaintenanceWindowResponse_endDate :: Lens.Lens' GetMaintenanceWindowResponse (Core.Maybe Core.Text)
getMaintenanceWindowResponse_endDate = Lens.lens (\GetMaintenanceWindowResponse' {endDate} -> endDate) (\s@GetMaintenanceWindowResponse' {} a -> s {endDate = a} :: GetMaintenanceWindowResponse)

-- | The next time the maintenance window will actually run, taking into
-- account any specified times for the maintenance window to become active
-- or inactive.
getMaintenanceWindowResponse_nextExecutionTime :: Lens.Lens' GetMaintenanceWindowResponse (Core.Maybe Core.Text)
getMaintenanceWindowResponse_nextExecutionTime = Lens.lens (\GetMaintenanceWindowResponse' {nextExecutionTime} -> nextExecutionTime) (\s@GetMaintenanceWindowResponse' {} a -> s {nextExecutionTime = a} :: GetMaintenanceWindowResponse)

-- | Whether targets must be registered with the maintenance window before
-- tasks can be defined for those targets.
getMaintenanceWindowResponse_allowUnassociatedTargets :: Lens.Lens' GetMaintenanceWindowResponse (Core.Maybe Core.Bool)
getMaintenanceWindowResponse_allowUnassociatedTargets = Lens.lens (\GetMaintenanceWindowResponse' {allowUnassociatedTargets} -> allowUnassociatedTargets) (\s@GetMaintenanceWindowResponse' {} a -> s {allowUnassociatedTargets = a} :: GetMaintenanceWindowResponse)

-- | The schedule of the maintenance window in the form of a cron or rate
-- expression.
getMaintenanceWindowResponse_schedule :: Lens.Lens' GetMaintenanceWindowResponse (Core.Maybe Core.Text)
getMaintenanceWindowResponse_schedule = Lens.lens (\GetMaintenanceWindowResponse' {schedule} -> schedule) (\s@GetMaintenanceWindowResponse' {} a -> s {schedule = a} :: GetMaintenanceWindowResponse)

-- | The response's http status code.
getMaintenanceWindowResponse_httpStatus :: Lens.Lens' GetMaintenanceWindowResponse Core.Int
getMaintenanceWindowResponse_httpStatus = Lens.lens (\GetMaintenanceWindowResponse' {httpStatus} -> httpStatus) (\s@GetMaintenanceWindowResponse' {} a -> s {httpStatus = a} :: GetMaintenanceWindowResponse)

instance Core.NFData GetMaintenanceWindowResponse
