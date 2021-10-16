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
    getMaintenanceWindowResponse_cutoff,
    getMaintenanceWindowResponse_modifiedDate,
    getMaintenanceWindowResponse_name,
    getMaintenanceWindowResponse_windowId,
    getMaintenanceWindowResponse_scheduleTimezone,
    getMaintenanceWindowResponse_description,
    getMaintenanceWindowResponse_endDate,
    getMaintenanceWindowResponse_allowUnassociatedTargets,
    getMaintenanceWindowResponse_nextExecutionTime,
    getMaintenanceWindowResponse_schedule,
    getMaintenanceWindowResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newGetMaintenanceWindow' smart constructor.
data GetMaintenanceWindow = GetMaintenanceWindow'
  { -- | The ID of the maintenance window for which you want to retrieve
    -- information.
    windowId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetMaintenanceWindow
newGetMaintenanceWindow pWindowId_ =
  GetMaintenanceWindow' {windowId = pWindowId_}

-- | The ID of the maintenance window for which you want to retrieve
-- information.
getMaintenanceWindow_windowId :: Lens.Lens' GetMaintenanceWindow Prelude.Text
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
            Prelude.<$> (x Core..?> "CreatedDate")
            Prelude.<*> (x Core..?> "StartDate")
            Prelude.<*> (x Core..?> "Duration")
            Prelude.<*> (x Core..?> "ScheduleOffset")
            Prelude.<*> (x Core..?> "Enabled")
            Prelude.<*> (x Core..?> "Cutoff")
            Prelude.<*> (x Core..?> "ModifiedDate")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "WindowId")
            Prelude.<*> (x Core..?> "ScheduleTimezone")
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> (x Core..?> "EndDate")
            Prelude.<*> (x Core..?> "AllowUnassociatedTargets")
            Prelude.<*> (x Core..?> "NextExecutionTime")
            Prelude.<*> (x Core..?> "Schedule")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetMaintenanceWindow

instance Prelude.NFData GetMaintenanceWindow

instance Core.ToHeaders GetMaintenanceWindow where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.GetMaintenanceWindow" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetMaintenanceWindow where
  toJSON GetMaintenanceWindow' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("WindowId" Core..= windowId)]
      )

instance Core.ToPath GetMaintenanceWindow where
  toPath = Prelude.const "/"

instance Core.ToQuery GetMaintenanceWindow where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetMaintenanceWindowResponse' smart constructor.
data GetMaintenanceWindowResponse = GetMaintenanceWindowResponse'
  { -- | The date the maintenance window was created.
    createdDate :: Prelude.Maybe Core.POSIX,
    -- | The date and time, in ISO-8601 Extended format, for when the maintenance
    -- window is scheduled to become active. The maintenance window won\'t run
    -- before this specified time.
    startDate :: Prelude.Maybe Prelude.Text,
    -- | The duration of the maintenance window in hours.
    duration :: Prelude.Maybe Prelude.Natural,
    -- | The number of days to wait to run a maintenance window after the
    -- scheduled cron expression date and time.
    scheduleOffset :: Prelude.Maybe Prelude.Natural,
    -- | Indicates whether the maintenance window is enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The number of hours before the end of the maintenance window that Amazon
    -- Web Services Systems Manager stops scheduling new tasks for execution.
    cutoff :: Prelude.Maybe Prelude.Natural,
    -- | The date the maintenance window was last modified.
    modifiedDate :: Prelude.Maybe Core.POSIX,
    -- | The name of the maintenance window.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the created maintenance window.
    windowId :: Prelude.Maybe Prelude.Text,
    -- | The time zone that the scheduled maintenance window executions are based
    -- on, in Internet Assigned Numbers Authority (IANA) format. For example:
    -- \"America\/Los_Angeles\", \"UTC\", or \"Asia\/Seoul\". For more
    -- information, see the
    -- <https://www.iana.org/time-zones Time Zone Database> on the IANA
    -- website.
    scheduleTimezone :: Prelude.Maybe Prelude.Text,
    -- | The description of the maintenance window.
    description :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The date and time, in ISO-8601 Extended format, for when the maintenance
    -- window is scheduled to become inactive. The maintenance window won\'t
    -- run after this specified time.
    endDate :: Prelude.Maybe Prelude.Text,
    -- | Whether targets must be registered with the maintenance window before
    -- tasks can be defined for those targets.
    allowUnassociatedTargets :: Prelude.Maybe Prelude.Bool,
    -- | The next time the maintenance window will actually run, taking into
    -- account any specified times for the maintenance window to become active
    -- or inactive.
    nextExecutionTime :: Prelude.Maybe Prelude.Text,
    -- | The schedule of the maintenance window in the form of a cron or rate
    -- expression.
    schedule :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
-- window is scheduled to become active. The maintenance window won\'t run
-- before this specified time.
--
-- 'duration', 'getMaintenanceWindowResponse_duration' - The duration of the maintenance window in hours.
--
-- 'scheduleOffset', 'getMaintenanceWindowResponse_scheduleOffset' - The number of days to wait to run a maintenance window after the
-- scheduled cron expression date and time.
--
-- 'enabled', 'getMaintenanceWindowResponse_enabled' - Indicates whether the maintenance window is enabled.
--
-- 'cutoff', 'getMaintenanceWindowResponse_cutoff' - The number of hours before the end of the maintenance window that Amazon
-- Web Services Systems Manager stops scheduling new tasks for execution.
--
-- 'modifiedDate', 'getMaintenanceWindowResponse_modifiedDate' - The date the maintenance window was last modified.
--
-- 'name', 'getMaintenanceWindowResponse_name' - The name of the maintenance window.
--
-- 'windowId', 'getMaintenanceWindowResponse_windowId' - The ID of the created maintenance window.
--
-- 'scheduleTimezone', 'getMaintenanceWindowResponse_scheduleTimezone' - The time zone that the scheduled maintenance window executions are based
-- on, in Internet Assigned Numbers Authority (IANA) format. For example:
-- \"America\/Los_Angeles\", \"UTC\", or \"Asia\/Seoul\". For more
-- information, see the
-- <https://www.iana.org/time-zones Time Zone Database> on the IANA
-- website.
--
-- 'description', 'getMaintenanceWindowResponse_description' - The description of the maintenance window.
--
-- 'endDate', 'getMaintenanceWindowResponse_endDate' - The date and time, in ISO-8601 Extended format, for when the maintenance
-- window is scheduled to become inactive. The maintenance window won\'t
-- run after this specified time.
--
-- 'allowUnassociatedTargets', 'getMaintenanceWindowResponse_allowUnassociatedTargets' - Whether targets must be registered with the maintenance window before
-- tasks can be defined for those targets.
--
-- 'nextExecutionTime', 'getMaintenanceWindowResponse_nextExecutionTime' - The next time the maintenance window will actually run, taking into
-- account any specified times for the maintenance window to become active
-- or inactive.
--
-- 'schedule', 'getMaintenanceWindowResponse_schedule' - The schedule of the maintenance window in the form of a cron or rate
-- expression.
--
-- 'httpStatus', 'getMaintenanceWindowResponse_httpStatus' - The response's http status code.
newGetMaintenanceWindowResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetMaintenanceWindowResponse
newGetMaintenanceWindowResponse pHttpStatus_ =
  GetMaintenanceWindowResponse'
    { createdDate =
        Prelude.Nothing,
      startDate = Prelude.Nothing,
      duration = Prelude.Nothing,
      scheduleOffset = Prelude.Nothing,
      enabled = Prelude.Nothing,
      cutoff = Prelude.Nothing,
      modifiedDate = Prelude.Nothing,
      name = Prelude.Nothing,
      windowId = Prelude.Nothing,
      scheduleTimezone = Prelude.Nothing,
      description = Prelude.Nothing,
      endDate = Prelude.Nothing,
      allowUnassociatedTargets = Prelude.Nothing,
      nextExecutionTime = Prelude.Nothing,
      schedule = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date the maintenance window was created.
getMaintenanceWindowResponse_createdDate :: Lens.Lens' GetMaintenanceWindowResponse (Prelude.Maybe Prelude.UTCTime)
getMaintenanceWindowResponse_createdDate = Lens.lens (\GetMaintenanceWindowResponse' {createdDate} -> createdDate) (\s@GetMaintenanceWindowResponse' {} a -> s {createdDate = a} :: GetMaintenanceWindowResponse) Prelude.. Lens.mapping Core._Time

-- | The date and time, in ISO-8601 Extended format, for when the maintenance
-- window is scheduled to become active. The maintenance window won\'t run
-- before this specified time.
getMaintenanceWindowResponse_startDate :: Lens.Lens' GetMaintenanceWindowResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowResponse_startDate = Lens.lens (\GetMaintenanceWindowResponse' {startDate} -> startDate) (\s@GetMaintenanceWindowResponse' {} a -> s {startDate = a} :: GetMaintenanceWindowResponse)

-- | The duration of the maintenance window in hours.
getMaintenanceWindowResponse_duration :: Lens.Lens' GetMaintenanceWindowResponse (Prelude.Maybe Prelude.Natural)
getMaintenanceWindowResponse_duration = Lens.lens (\GetMaintenanceWindowResponse' {duration} -> duration) (\s@GetMaintenanceWindowResponse' {} a -> s {duration = a} :: GetMaintenanceWindowResponse)

-- | The number of days to wait to run a maintenance window after the
-- scheduled cron expression date and time.
getMaintenanceWindowResponse_scheduleOffset :: Lens.Lens' GetMaintenanceWindowResponse (Prelude.Maybe Prelude.Natural)
getMaintenanceWindowResponse_scheduleOffset = Lens.lens (\GetMaintenanceWindowResponse' {scheduleOffset} -> scheduleOffset) (\s@GetMaintenanceWindowResponse' {} a -> s {scheduleOffset = a} :: GetMaintenanceWindowResponse)

-- | Indicates whether the maintenance window is enabled.
getMaintenanceWindowResponse_enabled :: Lens.Lens' GetMaintenanceWindowResponse (Prelude.Maybe Prelude.Bool)
getMaintenanceWindowResponse_enabled = Lens.lens (\GetMaintenanceWindowResponse' {enabled} -> enabled) (\s@GetMaintenanceWindowResponse' {} a -> s {enabled = a} :: GetMaintenanceWindowResponse)

-- | The number of hours before the end of the maintenance window that Amazon
-- Web Services Systems Manager stops scheduling new tasks for execution.
getMaintenanceWindowResponse_cutoff :: Lens.Lens' GetMaintenanceWindowResponse (Prelude.Maybe Prelude.Natural)
getMaintenanceWindowResponse_cutoff = Lens.lens (\GetMaintenanceWindowResponse' {cutoff} -> cutoff) (\s@GetMaintenanceWindowResponse' {} a -> s {cutoff = a} :: GetMaintenanceWindowResponse)

-- | The date the maintenance window was last modified.
getMaintenanceWindowResponse_modifiedDate :: Lens.Lens' GetMaintenanceWindowResponse (Prelude.Maybe Prelude.UTCTime)
getMaintenanceWindowResponse_modifiedDate = Lens.lens (\GetMaintenanceWindowResponse' {modifiedDate} -> modifiedDate) (\s@GetMaintenanceWindowResponse' {} a -> s {modifiedDate = a} :: GetMaintenanceWindowResponse) Prelude.. Lens.mapping Core._Time

-- | The name of the maintenance window.
getMaintenanceWindowResponse_name :: Lens.Lens' GetMaintenanceWindowResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowResponse_name = Lens.lens (\GetMaintenanceWindowResponse' {name} -> name) (\s@GetMaintenanceWindowResponse' {} a -> s {name = a} :: GetMaintenanceWindowResponse)

-- | The ID of the created maintenance window.
getMaintenanceWindowResponse_windowId :: Lens.Lens' GetMaintenanceWindowResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowResponse_windowId = Lens.lens (\GetMaintenanceWindowResponse' {windowId} -> windowId) (\s@GetMaintenanceWindowResponse' {} a -> s {windowId = a} :: GetMaintenanceWindowResponse)

-- | The time zone that the scheduled maintenance window executions are based
-- on, in Internet Assigned Numbers Authority (IANA) format. For example:
-- \"America\/Los_Angeles\", \"UTC\", or \"Asia\/Seoul\". For more
-- information, see the
-- <https://www.iana.org/time-zones Time Zone Database> on the IANA
-- website.
getMaintenanceWindowResponse_scheduleTimezone :: Lens.Lens' GetMaintenanceWindowResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowResponse_scheduleTimezone = Lens.lens (\GetMaintenanceWindowResponse' {scheduleTimezone} -> scheduleTimezone) (\s@GetMaintenanceWindowResponse' {} a -> s {scheduleTimezone = a} :: GetMaintenanceWindowResponse)

-- | The description of the maintenance window.
getMaintenanceWindowResponse_description :: Lens.Lens' GetMaintenanceWindowResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowResponse_description = Lens.lens (\GetMaintenanceWindowResponse' {description} -> description) (\s@GetMaintenanceWindowResponse' {} a -> s {description = a} :: GetMaintenanceWindowResponse) Prelude.. Lens.mapping Core._Sensitive

-- | The date and time, in ISO-8601 Extended format, for when the maintenance
-- window is scheduled to become inactive. The maintenance window won\'t
-- run after this specified time.
getMaintenanceWindowResponse_endDate :: Lens.Lens' GetMaintenanceWindowResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowResponse_endDate = Lens.lens (\GetMaintenanceWindowResponse' {endDate} -> endDate) (\s@GetMaintenanceWindowResponse' {} a -> s {endDate = a} :: GetMaintenanceWindowResponse)

-- | Whether targets must be registered with the maintenance window before
-- tasks can be defined for those targets.
getMaintenanceWindowResponse_allowUnassociatedTargets :: Lens.Lens' GetMaintenanceWindowResponse (Prelude.Maybe Prelude.Bool)
getMaintenanceWindowResponse_allowUnassociatedTargets = Lens.lens (\GetMaintenanceWindowResponse' {allowUnassociatedTargets} -> allowUnassociatedTargets) (\s@GetMaintenanceWindowResponse' {} a -> s {allowUnassociatedTargets = a} :: GetMaintenanceWindowResponse)

-- | The next time the maintenance window will actually run, taking into
-- account any specified times for the maintenance window to become active
-- or inactive.
getMaintenanceWindowResponse_nextExecutionTime :: Lens.Lens' GetMaintenanceWindowResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowResponse_nextExecutionTime = Lens.lens (\GetMaintenanceWindowResponse' {nextExecutionTime} -> nextExecutionTime) (\s@GetMaintenanceWindowResponse' {} a -> s {nextExecutionTime = a} :: GetMaintenanceWindowResponse)

-- | The schedule of the maintenance window in the form of a cron or rate
-- expression.
getMaintenanceWindowResponse_schedule :: Lens.Lens' GetMaintenanceWindowResponse (Prelude.Maybe Prelude.Text)
getMaintenanceWindowResponse_schedule = Lens.lens (\GetMaintenanceWindowResponse' {schedule} -> schedule) (\s@GetMaintenanceWindowResponse' {} a -> s {schedule = a} :: GetMaintenanceWindowResponse)

-- | The response's http status code.
getMaintenanceWindowResponse_httpStatus :: Lens.Lens' GetMaintenanceWindowResponse Prelude.Int
getMaintenanceWindowResponse_httpStatus = Lens.lens (\GetMaintenanceWindowResponse' {httpStatus} -> httpStatus) (\s@GetMaintenanceWindowResponse' {} a -> s {httpStatus = a} :: GetMaintenanceWindowResponse)

instance Prelude.NFData GetMaintenanceWindowResponse
