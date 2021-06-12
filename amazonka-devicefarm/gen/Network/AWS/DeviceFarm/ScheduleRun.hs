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
-- Module      : Network.AWS.DeviceFarm.ScheduleRun
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Schedules a run.
module Network.AWS.DeviceFarm.ScheduleRun
  ( -- * Creating a Request
    ScheduleRun (..),
    newScheduleRun,

    -- * Request Lenses
    scheduleRun_devicePoolArn,
    scheduleRun_deviceSelectionConfiguration,
    scheduleRun_configuration,
    scheduleRun_executionConfiguration,
    scheduleRun_name,
    scheduleRun_appArn,
    scheduleRun_projectArn,
    scheduleRun_test,

    -- * Destructuring the Response
    ScheduleRunResponse (..),
    newScheduleRunResponse,

    -- * Response Lenses
    scheduleRunResponse_run,
    scheduleRunResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the schedule run operation.
--
-- /See:/ 'newScheduleRun' smart constructor.
data ScheduleRun = ScheduleRun'
  { -- | The ARN of the device pool for the run to be scheduled.
    devicePoolArn :: Core.Maybe Core.Text,
    -- | The filter criteria used to dynamically select a set of devices for a
    -- test run and the maximum number of devices to be included in the run.
    --
    -- Either __@devicePoolArn@__ or __@deviceSelectionConfiguration@__ is
    -- required in a request.
    deviceSelectionConfiguration :: Core.Maybe DeviceSelectionConfiguration,
    -- | Information about the settings for the run to be scheduled.
    configuration :: Core.Maybe ScheduleRunConfiguration,
    -- | Specifies configuration information about a test run, such as the
    -- execution timeout (in minutes).
    executionConfiguration :: Core.Maybe ExecutionConfiguration,
    -- | The name for the run to be scheduled.
    name :: Core.Maybe Core.Text,
    -- | The ARN of an application package to run tests against, created with
    -- CreateUpload. See ListUploads.
    appArn :: Core.Maybe Core.Text,
    -- | The ARN of the project for the run to be scheduled.
    projectArn :: Core.Text,
    -- | Information about the test for the run to be scheduled.
    test :: ScheduleRunTest
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ScheduleRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'devicePoolArn', 'scheduleRun_devicePoolArn' - The ARN of the device pool for the run to be scheduled.
--
-- 'deviceSelectionConfiguration', 'scheduleRun_deviceSelectionConfiguration' - The filter criteria used to dynamically select a set of devices for a
-- test run and the maximum number of devices to be included in the run.
--
-- Either __@devicePoolArn@__ or __@deviceSelectionConfiguration@__ is
-- required in a request.
--
-- 'configuration', 'scheduleRun_configuration' - Information about the settings for the run to be scheduled.
--
-- 'executionConfiguration', 'scheduleRun_executionConfiguration' - Specifies configuration information about a test run, such as the
-- execution timeout (in minutes).
--
-- 'name', 'scheduleRun_name' - The name for the run to be scheduled.
--
-- 'appArn', 'scheduleRun_appArn' - The ARN of an application package to run tests against, created with
-- CreateUpload. See ListUploads.
--
-- 'projectArn', 'scheduleRun_projectArn' - The ARN of the project for the run to be scheduled.
--
-- 'test', 'scheduleRun_test' - Information about the test for the run to be scheduled.
newScheduleRun ::
  -- | 'projectArn'
  Core.Text ->
  -- | 'test'
  ScheduleRunTest ->
  ScheduleRun
newScheduleRun pProjectArn_ pTest_ =
  ScheduleRun'
    { devicePoolArn = Core.Nothing,
      deviceSelectionConfiguration = Core.Nothing,
      configuration = Core.Nothing,
      executionConfiguration = Core.Nothing,
      name = Core.Nothing,
      appArn = Core.Nothing,
      projectArn = pProjectArn_,
      test = pTest_
    }

-- | The ARN of the device pool for the run to be scheduled.
scheduleRun_devicePoolArn :: Lens.Lens' ScheduleRun (Core.Maybe Core.Text)
scheduleRun_devicePoolArn = Lens.lens (\ScheduleRun' {devicePoolArn} -> devicePoolArn) (\s@ScheduleRun' {} a -> s {devicePoolArn = a} :: ScheduleRun)

-- | The filter criteria used to dynamically select a set of devices for a
-- test run and the maximum number of devices to be included in the run.
--
-- Either __@devicePoolArn@__ or __@deviceSelectionConfiguration@__ is
-- required in a request.
scheduleRun_deviceSelectionConfiguration :: Lens.Lens' ScheduleRun (Core.Maybe DeviceSelectionConfiguration)
scheduleRun_deviceSelectionConfiguration = Lens.lens (\ScheduleRun' {deviceSelectionConfiguration} -> deviceSelectionConfiguration) (\s@ScheduleRun' {} a -> s {deviceSelectionConfiguration = a} :: ScheduleRun)

-- | Information about the settings for the run to be scheduled.
scheduleRun_configuration :: Lens.Lens' ScheduleRun (Core.Maybe ScheduleRunConfiguration)
scheduleRun_configuration = Lens.lens (\ScheduleRun' {configuration} -> configuration) (\s@ScheduleRun' {} a -> s {configuration = a} :: ScheduleRun)

-- | Specifies configuration information about a test run, such as the
-- execution timeout (in minutes).
scheduleRun_executionConfiguration :: Lens.Lens' ScheduleRun (Core.Maybe ExecutionConfiguration)
scheduleRun_executionConfiguration = Lens.lens (\ScheduleRun' {executionConfiguration} -> executionConfiguration) (\s@ScheduleRun' {} a -> s {executionConfiguration = a} :: ScheduleRun)

-- | The name for the run to be scheduled.
scheduleRun_name :: Lens.Lens' ScheduleRun (Core.Maybe Core.Text)
scheduleRun_name = Lens.lens (\ScheduleRun' {name} -> name) (\s@ScheduleRun' {} a -> s {name = a} :: ScheduleRun)

-- | The ARN of an application package to run tests against, created with
-- CreateUpload. See ListUploads.
scheduleRun_appArn :: Lens.Lens' ScheduleRun (Core.Maybe Core.Text)
scheduleRun_appArn = Lens.lens (\ScheduleRun' {appArn} -> appArn) (\s@ScheduleRun' {} a -> s {appArn = a} :: ScheduleRun)

-- | The ARN of the project for the run to be scheduled.
scheduleRun_projectArn :: Lens.Lens' ScheduleRun Core.Text
scheduleRun_projectArn = Lens.lens (\ScheduleRun' {projectArn} -> projectArn) (\s@ScheduleRun' {} a -> s {projectArn = a} :: ScheduleRun)

-- | Information about the test for the run to be scheduled.
scheduleRun_test :: Lens.Lens' ScheduleRun ScheduleRunTest
scheduleRun_test = Lens.lens (\ScheduleRun' {test} -> test) (\s@ScheduleRun' {} a -> s {test = a} :: ScheduleRun)

instance Core.AWSRequest ScheduleRun where
  type AWSResponse ScheduleRun = ScheduleRunResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ScheduleRunResponse'
            Core.<$> (x Core..?> "run")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ScheduleRun

instance Core.NFData ScheduleRun

instance Core.ToHeaders ScheduleRun where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.ScheduleRun" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ScheduleRun where
  toJSON ScheduleRun' {..} =
    Core.object
      ( Core.catMaybes
          [ ("devicePoolArn" Core..=) Core.<$> devicePoolArn,
            ("deviceSelectionConfiguration" Core..=)
              Core.<$> deviceSelectionConfiguration,
            ("configuration" Core..=) Core.<$> configuration,
            ("executionConfiguration" Core..=)
              Core.<$> executionConfiguration,
            ("name" Core..=) Core.<$> name,
            ("appArn" Core..=) Core.<$> appArn,
            Core.Just ("projectArn" Core..= projectArn),
            Core.Just ("test" Core..= test)
          ]
      )

instance Core.ToPath ScheduleRun where
  toPath = Core.const "/"

instance Core.ToQuery ScheduleRun where
  toQuery = Core.const Core.mempty

-- | Represents the result of a schedule run request.
--
-- /See:/ 'newScheduleRunResponse' smart constructor.
data ScheduleRunResponse = ScheduleRunResponse'
  { -- | Information about the scheduled run.
    run :: Core.Maybe Run,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ScheduleRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'run', 'scheduleRunResponse_run' - Information about the scheduled run.
--
-- 'httpStatus', 'scheduleRunResponse_httpStatus' - The response's http status code.
newScheduleRunResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ScheduleRunResponse
newScheduleRunResponse pHttpStatus_ =
  ScheduleRunResponse'
    { run = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the scheduled run.
scheduleRunResponse_run :: Lens.Lens' ScheduleRunResponse (Core.Maybe Run)
scheduleRunResponse_run = Lens.lens (\ScheduleRunResponse' {run} -> run) (\s@ScheduleRunResponse' {} a -> s {run = a} :: ScheduleRunResponse)

-- | The response's http status code.
scheduleRunResponse_httpStatus :: Lens.Lens' ScheduleRunResponse Core.Int
scheduleRunResponse_httpStatus = Lens.lens (\ScheduleRunResponse' {httpStatus} -> httpStatus) (\s@ScheduleRunResponse' {} a -> s {httpStatus = a} :: ScheduleRunResponse)

instance Core.NFData ScheduleRunResponse
