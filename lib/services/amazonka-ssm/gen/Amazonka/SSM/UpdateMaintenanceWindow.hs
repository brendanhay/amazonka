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
-- Module      : Amazonka.SSM.UpdateMaintenanceWindow
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing maintenance window. Only specified parameters are
-- modified.
--
-- The value you specify for @Duration@ determines the specific end time
-- for the maintenance window based on the time it begins. No maintenance
-- window tasks are permitted to start after the resulting endtime minus
-- the number of hours you specify for @Cutoff@. For example, if the
-- maintenance window starts at 3 PM, the duration is three hours, and the
-- value you specify for @Cutoff@ is one hour, no maintenance window tasks
-- can start after 5 PM.
module Amazonka.SSM.UpdateMaintenanceWindow
  ( -- * Creating a Request
    UpdateMaintenanceWindow (..),
    newUpdateMaintenanceWindow,

    -- * Request Lenses
    updateMaintenanceWindow_allowUnassociatedTargets,
    updateMaintenanceWindow_cutoff,
    updateMaintenanceWindow_description,
    updateMaintenanceWindow_duration,
    updateMaintenanceWindow_enabled,
    updateMaintenanceWindow_endDate,
    updateMaintenanceWindow_name,
    updateMaintenanceWindow_replace,
    updateMaintenanceWindow_schedule,
    updateMaintenanceWindow_scheduleOffset,
    updateMaintenanceWindow_scheduleTimezone,
    updateMaintenanceWindow_startDate,
    updateMaintenanceWindow_windowId,

    -- * Destructuring the Response
    UpdateMaintenanceWindowResponse (..),
    newUpdateMaintenanceWindowResponse,

    -- * Response Lenses
    updateMaintenanceWindowResponse_allowUnassociatedTargets,
    updateMaintenanceWindowResponse_cutoff,
    updateMaintenanceWindowResponse_description,
    updateMaintenanceWindowResponse_duration,
    updateMaintenanceWindowResponse_enabled,
    updateMaintenanceWindowResponse_endDate,
    updateMaintenanceWindowResponse_name,
    updateMaintenanceWindowResponse_schedule,
    updateMaintenanceWindowResponse_scheduleOffset,
    updateMaintenanceWindowResponse_scheduleTimezone,
    updateMaintenanceWindowResponse_startDate,
    updateMaintenanceWindowResponse_windowId,
    updateMaintenanceWindowResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newUpdateMaintenanceWindow' smart constructor.
data UpdateMaintenanceWindow = UpdateMaintenanceWindow'
  { -- | Whether targets must be registered with the maintenance window before
    -- tasks can be defined for those targets.
    allowUnassociatedTargets :: Prelude.Maybe Prelude.Bool,
    -- | The number of hours before the end of the maintenance window that Amazon
    -- Web Services Systems Manager stops scheduling new tasks for execution.
    cutoff :: Prelude.Maybe Prelude.Natural,
    -- | An optional description for the update request.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The duration of the maintenance window in hours.
    duration :: Prelude.Maybe Prelude.Natural,
    -- | Whether the maintenance window is enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The date and time, in ISO-8601 Extended format, for when you want the
    -- maintenance window to become inactive. @EndDate@ allows you to set a
    -- date and time in the future when the maintenance window will no longer
    -- run.
    endDate :: Prelude.Maybe Prelude.Text,
    -- | The name of the maintenance window.
    name :: Prelude.Maybe Prelude.Text,
    -- | If @True@, then all fields that are required by the
    -- CreateMaintenanceWindow operation are also required for this API
    -- request. Optional fields that aren\'t specified are set to null.
    replace :: Prelude.Maybe Prelude.Bool,
    -- | The schedule of the maintenance window in the form of a cron or rate
    -- expression.
    schedule :: Prelude.Maybe Prelude.Text,
    -- | The number of days to wait after the date and time specified by a cron
    -- expression before running the maintenance window.
    --
    -- For example, the following cron expression schedules a maintenance
    -- window to run the third Tuesday of every month at 11:30 PM.
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
    -- | The ID of the maintenance window to update.
    windowId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMaintenanceWindow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowUnassociatedTargets', 'updateMaintenanceWindow_allowUnassociatedTargets' - Whether targets must be registered with the maintenance window before
-- tasks can be defined for those targets.
--
-- 'cutoff', 'updateMaintenanceWindow_cutoff' - The number of hours before the end of the maintenance window that Amazon
-- Web Services Systems Manager stops scheduling new tasks for execution.
--
-- 'description', 'updateMaintenanceWindow_description' - An optional description for the update request.
--
-- 'duration', 'updateMaintenanceWindow_duration' - The duration of the maintenance window in hours.
--
-- 'enabled', 'updateMaintenanceWindow_enabled' - Whether the maintenance window is enabled.
--
-- 'endDate', 'updateMaintenanceWindow_endDate' - The date and time, in ISO-8601 Extended format, for when you want the
-- maintenance window to become inactive. @EndDate@ allows you to set a
-- date and time in the future when the maintenance window will no longer
-- run.
--
-- 'name', 'updateMaintenanceWindow_name' - The name of the maintenance window.
--
-- 'replace', 'updateMaintenanceWindow_replace' - If @True@, then all fields that are required by the
-- CreateMaintenanceWindow operation are also required for this API
-- request. Optional fields that aren\'t specified are set to null.
--
-- 'schedule', 'updateMaintenanceWindow_schedule' - The schedule of the maintenance window in the form of a cron or rate
-- expression.
--
-- 'scheduleOffset', 'updateMaintenanceWindow_scheduleOffset' - The number of days to wait after the date and time specified by a cron
-- expression before running the maintenance window.
--
-- For example, the following cron expression schedules a maintenance
-- window to run the third Tuesday of every month at 11:30 PM.
--
-- @cron(30 23 ? * TUE#3 *)@
--
-- If the schedule offset is @2@, the maintenance window won\'t run until
-- two days later.
--
-- 'scheduleTimezone', 'updateMaintenanceWindow_scheduleTimezone' - The time zone that the scheduled maintenance window executions are based
-- on, in Internet Assigned Numbers Authority (IANA) format. For example:
-- \"America\/Los_Angeles\", \"UTC\", or \"Asia\/Seoul\". For more
-- information, see the
-- <https://www.iana.org/time-zones Time Zone Database> on the IANA
-- website.
--
-- 'startDate', 'updateMaintenanceWindow_startDate' - The date and time, in ISO-8601 Extended format, for when you want the
-- maintenance window to become active. @StartDate@ allows you to delay
-- activation of the maintenance window until the specified future date.
--
-- 'windowId', 'updateMaintenanceWindow_windowId' - The ID of the maintenance window to update.
newUpdateMaintenanceWindow ::
  -- | 'windowId'
  Prelude.Text ->
  UpdateMaintenanceWindow
newUpdateMaintenanceWindow pWindowId_ =
  UpdateMaintenanceWindow'
    { allowUnassociatedTargets =
        Prelude.Nothing,
      cutoff = Prelude.Nothing,
      description = Prelude.Nothing,
      duration = Prelude.Nothing,
      enabled = Prelude.Nothing,
      endDate = Prelude.Nothing,
      name = Prelude.Nothing,
      replace = Prelude.Nothing,
      schedule = Prelude.Nothing,
      scheduleOffset = Prelude.Nothing,
      scheduleTimezone = Prelude.Nothing,
      startDate = Prelude.Nothing,
      windowId = pWindowId_
    }

-- | Whether targets must be registered with the maintenance window before
-- tasks can be defined for those targets.
updateMaintenanceWindow_allowUnassociatedTargets :: Lens.Lens' UpdateMaintenanceWindow (Prelude.Maybe Prelude.Bool)
updateMaintenanceWindow_allowUnassociatedTargets = Lens.lens (\UpdateMaintenanceWindow' {allowUnassociatedTargets} -> allowUnassociatedTargets) (\s@UpdateMaintenanceWindow' {} a -> s {allowUnassociatedTargets = a} :: UpdateMaintenanceWindow)

-- | The number of hours before the end of the maintenance window that Amazon
-- Web Services Systems Manager stops scheduling new tasks for execution.
updateMaintenanceWindow_cutoff :: Lens.Lens' UpdateMaintenanceWindow (Prelude.Maybe Prelude.Natural)
updateMaintenanceWindow_cutoff = Lens.lens (\UpdateMaintenanceWindow' {cutoff} -> cutoff) (\s@UpdateMaintenanceWindow' {} a -> s {cutoff = a} :: UpdateMaintenanceWindow)

-- | An optional description for the update request.
updateMaintenanceWindow_description :: Lens.Lens' UpdateMaintenanceWindow (Prelude.Maybe Prelude.Text)
updateMaintenanceWindow_description = Lens.lens (\UpdateMaintenanceWindow' {description} -> description) (\s@UpdateMaintenanceWindow' {} a -> s {description = a} :: UpdateMaintenanceWindow) Prelude.. Lens.mapping Data._Sensitive

-- | The duration of the maintenance window in hours.
updateMaintenanceWindow_duration :: Lens.Lens' UpdateMaintenanceWindow (Prelude.Maybe Prelude.Natural)
updateMaintenanceWindow_duration = Lens.lens (\UpdateMaintenanceWindow' {duration} -> duration) (\s@UpdateMaintenanceWindow' {} a -> s {duration = a} :: UpdateMaintenanceWindow)

-- | Whether the maintenance window is enabled.
updateMaintenanceWindow_enabled :: Lens.Lens' UpdateMaintenanceWindow (Prelude.Maybe Prelude.Bool)
updateMaintenanceWindow_enabled = Lens.lens (\UpdateMaintenanceWindow' {enabled} -> enabled) (\s@UpdateMaintenanceWindow' {} a -> s {enabled = a} :: UpdateMaintenanceWindow)

-- | The date and time, in ISO-8601 Extended format, for when you want the
-- maintenance window to become inactive. @EndDate@ allows you to set a
-- date and time in the future when the maintenance window will no longer
-- run.
updateMaintenanceWindow_endDate :: Lens.Lens' UpdateMaintenanceWindow (Prelude.Maybe Prelude.Text)
updateMaintenanceWindow_endDate = Lens.lens (\UpdateMaintenanceWindow' {endDate} -> endDate) (\s@UpdateMaintenanceWindow' {} a -> s {endDate = a} :: UpdateMaintenanceWindow)

-- | The name of the maintenance window.
updateMaintenanceWindow_name :: Lens.Lens' UpdateMaintenanceWindow (Prelude.Maybe Prelude.Text)
updateMaintenanceWindow_name = Lens.lens (\UpdateMaintenanceWindow' {name} -> name) (\s@UpdateMaintenanceWindow' {} a -> s {name = a} :: UpdateMaintenanceWindow)

-- | If @True@, then all fields that are required by the
-- CreateMaintenanceWindow operation are also required for this API
-- request. Optional fields that aren\'t specified are set to null.
updateMaintenanceWindow_replace :: Lens.Lens' UpdateMaintenanceWindow (Prelude.Maybe Prelude.Bool)
updateMaintenanceWindow_replace = Lens.lens (\UpdateMaintenanceWindow' {replace} -> replace) (\s@UpdateMaintenanceWindow' {} a -> s {replace = a} :: UpdateMaintenanceWindow)

-- | The schedule of the maintenance window in the form of a cron or rate
-- expression.
updateMaintenanceWindow_schedule :: Lens.Lens' UpdateMaintenanceWindow (Prelude.Maybe Prelude.Text)
updateMaintenanceWindow_schedule = Lens.lens (\UpdateMaintenanceWindow' {schedule} -> schedule) (\s@UpdateMaintenanceWindow' {} a -> s {schedule = a} :: UpdateMaintenanceWindow)

-- | The number of days to wait after the date and time specified by a cron
-- expression before running the maintenance window.
--
-- For example, the following cron expression schedules a maintenance
-- window to run the third Tuesday of every month at 11:30 PM.
--
-- @cron(30 23 ? * TUE#3 *)@
--
-- If the schedule offset is @2@, the maintenance window won\'t run until
-- two days later.
updateMaintenanceWindow_scheduleOffset :: Lens.Lens' UpdateMaintenanceWindow (Prelude.Maybe Prelude.Natural)
updateMaintenanceWindow_scheduleOffset = Lens.lens (\UpdateMaintenanceWindow' {scheduleOffset} -> scheduleOffset) (\s@UpdateMaintenanceWindow' {} a -> s {scheduleOffset = a} :: UpdateMaintenanceWindow)

-- | The time zone that the scheduled maintenance window executions are based
-- on, in Internet Assigned Numbers Authority (IANA) format. For example:
-- \"America\/Los_Angeles\", \"UTC\", or \"Asia\/Seoul\". For more
-- information, see the
-- <https://www.iana.org/time-zones Time Zone Database> on the IANA
-- website.
updateMaintenanceWindow_scheduleTimezone :: Lens.Lens' UpdateMaintenanceWindow (Prelude.Maybe Prelude.Text)
updateMaintenanceWindow_scheduleTimezone = Lens.lens (\UpdateMaintenanceWindow' {scheduleTimezone} -> scheduleTimezone) (\s@UpdateMaintenanceWindow' {} a -> s {scheduleTimezone = a} :: UpdateMaintenanceWindow)

-- | The date and time, in ISO-8601 Extended format, for when you want the
-- maintenance window to become active. @StartDate@ allows you to delay
-- activation of the maintenance window until the specified future date.
updateMaintenanceWindow_startDate :: Lens.Lens' UpdateMaintenanceWindow (Prelude.Maybe Prelude.Text)
updateMaintenanceWindow_startDate = Lens.lens (\UpdateMaintenanceWindow' {startDate} -> startDate) (\s@UpdateMaintenanceWindow' {} a -> s {startDate = a} :: UpdateMaintenanceWindow)

-- | The ID of the maintenance window to update.
updateMaintenanceWindow_windowId :: Lens.Lens' UpdateMaintenanceWindow Prelude.Text
updateMaintenanceWindow_windowId = Lens.lens (\UpdateMaintenanceWindow' {windowId} -> windowId) (\s@UpdateMaintenanceWindow' {} a -> s {windowId = a} :: UpdateMaintenanceWindow)

instance Core.AWSRequest UpdateMaintenanceWindow where
  type
    AWSResponse UpdateMaintenanceWindow =
      UpdateMaintenanceWindowResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateMaintenanceWindowResponse'
            Prelude.<$> (x Data..?> "AllowUnassociatedTargets")
            Prelude.<*> (x Data..?> "Cutoff")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "Duration")
            Prelude.<*> (x Data..?> "Enabled")
            Prelude.<*> (x Data..?> "EndDate")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "Schedule")
            Prelude.<*> (x Data..?> "ScheduleOffset")
            Prelude.<*> (x Data..?> "ScheduleTimezone")
            Prelude.<*> (x Data..?> "StartDate")
            Prelude.<*> (x Data..?> "WindowId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateMaintenanceWindow where
  hashWithSalt _salt UpdateMaintenanceWindow' {..} =
    _salt
      `Prelude.hashWithSalt` allowUnassociatedTargets
      `Prelude.hashWithSalt` cutoff
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` duration
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` endDate
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` replace
      `Prelude.hashWithSalt` schedule
      `Prelude.hashWithSalt` scheduleOffset
      `Prelude.hashWithSalt` scheduleTimezone
      `Prelude.hashWithSalt` startDate
      `Prelude.hashWithSalt` windowId

instance Prelude.NFData UpdateMaintenanceWindow where
  rnf UpdateMaintenanceWindow' {..} =
    Prelude.rnf allowUnassociatedTargets `Prelude.seq`
      Prelude.rnf cutoff `Prelude.seq`
        Prelude.rnf description `Prelude.seq`
          Prelude.rnf duration `Prelude.seq`
            Prelude.rnf enabled `Prelude.seq`
              Prelude.rnf endDate `Prelude.seq`
                Prelude.rnf name `Prelude.seq`
                  Prelude.rnf replace `Prelude.seq`
                    Prelude.rnf schedule `Prelude.seq`
                      Prelude.rnf scheduleOffset `Prelude.seq`
                        Prelude.rnf scheduleTimezone `Prelude.seq`
                          Prelude.rnf startDate `Prelude.seq`
                            Prelude.rnf windowId

instance Data.ToHeaders UpdateMaintenanceWindow where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.UpdateMaintenanceWindow" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateMaintenanceWindow where
  toJSON UpdateMaintenanceWindow' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AllowUnassociatedTargets" Data..=)
              Prelude.<$> allowUnassociatedTargets,
            ("Cutoff" Data..=) Prelude.<$> cutoff,
            ("Description" Data..=) Prelude.<$> description,
            ("Duration" Data..=) Prelude.<$> duration,
            ("Enabled" Data..=) Prelude.<$> enabled,
            ("EndDate" Data..=) Prelude.<$> endDate,
            ("Name" Data..=) Prelude.<$> name,
            ("Replace" Data..=) Prelude.<$> replace,
            ("Schedule" Data..=) Prelude.<$> schedule,
            ("ScheduleOffset" Data..=)
              Prelude.<$> scheduleOffset,
            ("ScheduleTimezone" Data..=)
              Prelude.<$> scheduleTimezone,
            ("StartDate" Data..=) Prelude.<$> startDate,
            Prelude.Just ("WindowId" Data..= windowId)
          ]
      )

instance Data.ToPath UpdateMaintenanceWindow where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateMaintenanceWindow where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateMaintenanceWindowResponse' smart constructor.
data UpdateMaintenanceWindowResponse = UpdateMaintenanceWindowResponse'
  { -- | Whether targets must be registered with the maintenance window before
    -- tasks can be defined for those targets.
    allowUnassociatedTargets :: Prelude.Maybe Prelude.Bool,
    -- | The number of hours before the end of the maintenance window that Amazon
    -- Web Services Systems Manager stops scheduling new tasks for execution.
    cutoff :: Prelude.Maybe Prelude.Natural,
    -- | An optional description of the update.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The duration of the maintenance window in hours.
    duration :: Prelude.Maybe Prelude.Natural,
    -- | Whether the maintenance window is enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The date and time, in ISO-8601 Extended format, for when the maintenance
    -- window is scheduled to become inactive. The maintenance window won\'t
    -- run after this specified time.
    endDate :: Prelude.Maybe Prelude.Text,
    -- | The name of the maintenance window.
    name :: Prelude.Maybe Prelude.Text,
    -- | The schedule of the maintenance window in the form of a cron or rate
    -- expression.
    schedule :: Prelude.Maybe Prelude.Text,
    -- | The number of days to wait to run a maintenance window after the
    -- scheduled cron expression date and time.
    scheduleOffset :: Prelude.Maybe Prelude.Natural,
    -- | The time zone that the scheduled maintenance window executions are based
    -- on, in Internet Assigned Numbers Authority (IANA) format. For example:
    -- \"America\/Los_Angeles\", \"UTC\", or \"Asia\/Seoul\". For more
    -- information, see the
    -- <https://www.iana.org/time-zones Time Zone Database> on the IANA
    -- website.
    scheduleTimezone :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in ISO-8601 Extended format, for when the maintenance
    -- window is scheduled to become active. The maintenance window won\'t run
    -- before this specified time.
    startDate :: Prelude.Maybe Prelude.Text,
    -- | The ID of the created maintenance window.
    windowId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMaintenanceWindowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowUnassociatedTargets', 'updateMaintenanceWindowResponse_allowUnassociatedTargets' - Whether targets must be registered with the maintenance window before
-- tasks can be defined for those targets.
--
-- 'cutoff', 'updateMaintenanceWindowResponse_cutoff' - The number of hours before the end of the maintenance window that Amazon
-- Web Services Systems Manager stops scheduling new tasks for execution.
--
-- 'description', 'updateMaintenanceWindowResponse_description' - An optional description of the update.
--
-- 'duration', 'updateMaintenanceWindowResponse_duration' - The duration of the maintenance window in hours.
--
-- 'enabled', 'updateMaintenanceWindowResponse_enabled' - Whether the maintenance window is enabled.
--
-- 'endDate', 'updateMaintenanceWindowResponse_endDate' - The date and time, in ISO-8601 Extended format, for when the maintenance
-- window is scheduled to become inactive. The maintenance window won\'t
-- run after this specified time.
--
-- 'name', 'updateMaintenanceWindowResponse_name' - The name of the maintenance window.
--
-- 'schedule', 'updateMaintenanceWindowResponse_schedule' - The schedule of the maintenance window in the form of a cron or rate
-- expression.
--
-- 'scheduleOffset', 'updateMaintenanceWindowResponse_scheduleOffset' - The number of days to wait to run a maintenance window after the
-- scheduled cron expression date and time.
--
-- 'scheduleTimezone', 'updateMaintenanceWindowResponse_scheduleTimezone' - The time zone that the scheduled maintenance window executions are based
-- on, in Internet Assigned Numbers Authority (IANA) format. For example:
-- \"America\/Los_Angeles\", \"UTC\", or \"Asia\/Seoul\". For more
-- information, see the
-- <https://www.iana.org/time-zones Time Zone Database> on the IANA
-- website.
--
-- 'startDate', 'updateMaintenanceWindowResponse_startDate' - The date and time, in ISO-8601 Extended format, for when the maintenance
-- window is scheduled to become active. The maintenance window won\'t run
-- before this specified time.
--
-- 'windowId', 'updateMaintenanceWindowResponse_windowId' - The ID of the created maintenance window.
--
-- 'httpStatus', 'updateMaintenanceWindowResponse_httpStatus' - The response's http status code.
newUpdateMaintenanceWindowResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateMaintenanceWindowResponse
newUpdateMaintenanceWindowResponse pHttpStatus_ =
  UpdateMaintenanceWindowResponse'
    { allowUnassociatedTargets =
        Prelude.Nothing,
      cutoff = Prelude.Nothing,
      description = Prelude.Nothing,
      duration = Prelude.Nothing,
      enabled = Prelude.Nothing,
      endDate = Prelude.Nothing,
      name = Prelude.Nothing,
      schedule = Prelude.Nothing,
      scheduleOffset = Prelude.Nothing,
      scheduleTimezone = Prelude.Nothing,
      startDate = Prelude.Nothing,
      windowId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Whether targets must be registered with the maintenance window before
-- tasks can be defined for those targets.
updateMaintenanceWindowResponse_allowUnassociatedTargets :: Lens.Lens' UpdateMaintenanceWindowResponse (Prelude.Maybe Prelude.Bool)
updateMaintenanceWindowResponse_allowUnassociatedTargets = Lens.lens (\UpdateMaintenanceWindowResponse' {allowUnassociatedTargets} -> allowUnassociatedTargets) (\s@UpdateMaintenanceWindowResponse' {} a -> s {allowUnassociatedTargets = a} :: UpdateMaintenanceWindowResponse)

-- | The number of hours before the end of the maintenance window that Amazon
-- Web Services Systems Manager stops scheduling new tasks for execution.
updateMaintenanceWindowResponse_cutoff :: Lens.Lens' UpdateMaintenanceWindowResponse (Prelude.Maybe Prelude.Natural)
updateMaintenanceWindowResponse_cutoff = Lens.lens (\UpdateMaintenanceWindowResponse' {cutoff} -> cutoff) (\s@UpdateMaintenanceWindowResponse' {} a -> s {cutoff = a} :: UpdateMaintenanceWindowResponse)

-- | An optional description of the update.
updateMaintenanceWindowResponse_description :: Lens.Lens' UpdateMaintenanceWindowResponse (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowResponse_description = Lens.lens (\UpdateMaintenanceWindowResponse' {description} -> description) (\s@UpdateMaintenanceWindowResponse' {} a -> s {description = a} :: UpdateMaintenanceWindowResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The duration of the maintenance window in hours.
updateMaintenanceWindowResponse_duration :: Lens.Lens' UpdateMaintenanceWindowResponse (Prelude.Maybe Prelude.Natural)
updateMaintenanceWindowResponse_duration = Lens.lens (\UpdateMaintenanceWindowResponse' {duration} -> duration) (\s@UpdateMaintenanceWindowResponse' {} a -> s {duration = a} :: UpdateMaintenanceWindowResponse)

-- | Whether the maintenance window is enabled.
updateMaintenanceWindowResponse_enabled :: Lens.Lens' UpdateMaintenanceWindowResponse (Prelude.Maybe Prelude.Bool)
updateMaintenanceWindowResponse_enabled = Lens.lens (\UpdateMaintenanceWindowResponse' {enabled} -> enabled) (\s@UpdateMaintenanceWindowResponse' {} a -> s {enabled = a} :: UpdateMaintenanceWindowResponse)

-- | The date and time, in ISO-8601 Extended format, for when the maintenance
-- window is scheduled to become inactive. The maintenance window won\'t
-- run after this specified time.
updateMaintenanceWindowResponse_endDate :: Lens.Lens' UpdateMaintenanceWindowResponse (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowResponse_endDate = Lens.lens (\UpdateMaintenanceWindowResponse' {endDate} -> endDate) (\s@UpdateMaintenanceWindowResponse' {} a -> s {endDate = a} :: UpdateMaintenanceWindowResponse)

-- | The name of the maintenance window.
updateMaintenanceWindowResponse_name :: Lens.Lens' UpdateMaintenanceWindowResponse (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowResponse_name = Lens.lens (\UpdateMaintenanceWindowResponse' {name} -> name) (\s@UpdateMaintenanceWindowResponse' {} a -> s {name = a} :: UpdateMaintenanceWindowResponse)

-- | The schedule of the maintenance window in the form of a cron or rate
-- expression.
updateMaintenanceWindowResponse_schedule :: Lens.Lens' UpdateMaintenanceWindowResponse (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowResponse_schedule = Lens.lens (\UpdateMaintenanceWindowResponse' {schedule} -> schedule) (\s@UpdateMaintenanceWindowResponse' {} a -> s {schedule = a} :: UpdateMaintenanceWindowResponse)

-- | The number of days to wait to run a maintenance window after the
-- scheduled cron expression date and time.
updateMaintenanceWindowResponse_scheduleOffset :: Lens.Lens' UpdateMaintenanceWindowResponse (Prelude.Maybe Prelude.Natural)
updateMaintenanceWindowResponse_scheduleOffset = Lens.lens (\UpdateMaintenanceWindowResponse' {scheduleOffset} -> scheduleOffset) (\s@UpdateMaintenanceWindowResponse' {} a -> s {scheduleOffset = a} :: UpdateMaintenanceWindowResponse)

-- | The time zone that the scheduled maintenance window executions are based
-- on, in Internet Assigned Numbers Authority (IANA) format. For example:
-- \"America\/Los_Angeles\", \"UTC\", or \"Asia\/Seoul\". For more
-- information, see the
-- <https://www.iana.org/time-zones Time Zone Database> on the IANA
-- website.
updateMaintenanceWindowResponse_scheduleTimezone :: Lens.Lens' UpdateMaintenanceWindowResponse (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowResponse_scheduleTimezone = Lens.lens (\UpdateMaintenanceWindowResponse' {scheduleTimezone} -> scheduleTimezone) (\s@UpdateMaintenanceWindowResponse' {} a -> s {scheduleTimezone = a} :: UpdateMaintenanceWindowResponse)

-- | The date and time, in ISO-8601 Extended format, for when the maintenance
-- window is scheduled to become active. The maintenance window won\'t run
-- before this specified time.
updateMaintenanceWindowResponse_startDate :: Lens.Lens' UpdateMaintenanceWindowResponse (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowResponse_startDate = Lens.lens (\UpdateMaintenanceWindowResponse' {startDate} -> startDate) (\s@UpdateMaintenanceWindowResponse' {} a -> s {startDate = a} :: UpdateMaintenanceWindowResponse)

-- | The ID of the created maintenance window.
updateMaintenanceWindowResponse_windowId :: Lens.Lens' UpdateMaintenanceWindowResponse (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowResponse_windowId = Lens.lens (\UpdateMaintenanceWindowResponse' {windowId} -> windowId) (\s@UpdateMaintenanceWindowResponse' {} a -> s {windowId = a} :: UpdateMaintenanceWindowResponse)

-- | The response's http status code.
updateMaintenanceWindowResponse_httpStatus :: Lens.Lens' UpdateMaintenanceWindowResponse Prelude.Int
updateMaintenanceWindowResponse_httpStatus = Lens.lens (\UpdateMaintenanceWindowResponse' {httpStatus} -> httpStatus) (\s@UpdateMaintenanceWindowResponse' {} a -> s {httpStatus = a} :: UpdateMaintenanceWindowResponse)

instance
  Prelude.NFData
    UpdateMaintenanceWindowResponse
  where
  rnf UpdateMaintenanceWindowResponse' {..} =
    Prelude.rnf allowUnassociatedTargets `Prelude.seq`
      Prelude.rnf cutoff `Prelude.seq`
        Prelude.rnf description `Prelude.seq`
          Prelude.rnf duration `Prelude.seq`
            Prelude.rnf enabled `Prelude.seq`
              Prelude.rnf endDate `Prelude.seq`
                Prelude.rnf name `Prelude.seq`
                  Prelude.rnf schedule `Prelude.seq`
                    Prelude.rnf scheduleOffset `Prelude.seq`
                      Prelude.rnf scheduleTimezone `Prelude.seq`
                        Prelude.rnf startDate `Prelude.seq`
                          Prelude.rnf windowId `Prelude.seq`
                            Prelude.rnf httpStatus
