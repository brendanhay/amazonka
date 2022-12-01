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
-- Module      : Amazonka.DeviceFarm.ScheduleRun
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Schedules a run.
module Amazonka.DeviceFarm.ScheduleRun
  ( -- * Creating a Request
    ScheduleRun (..),
    newScheduleRun,

    -- * Request Lenses
    scheduleRun_name,
    scheduleRun_deviceSelectionConfiguration,
    scheduleRun_configuration,
    scheduleRun_executionConfiguration,
    scheduleRun_appArn,
    scheduleRun_devicePoolArn,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DeviceFarm.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents a request to the schedule run operation.
--
-- /See:/ 'newScheduleRun' smart constructor.
data ScheduleRun = ScheduleRun'
  { -- | The name for the run to be scheduled.
    name :: Prelude.Maybe Prelude.Text,
    -- | The filter criteria used to dynamically select a set of devices for a
    -- test run and the maximum number of devices to be included in the run.
    --
    -- Either __@devicePoolArn@__ or __@deviceSelectionConfiguration@__ is
    -- required in a request.
    deviceSelectionConfiguration :: Prelude.Maybe DeviceSelectionConfiguration,
    -- | Information about the settings for the run to be scheduled.
    configuration :: Prelude.Maybe ScheduleRunConfiguration,
    -- | Specifies configuration information about a test run, such as the
    -- execution timeout (in minutes).
    executionConfiguration :: Prelude.Maybe ExecutionConfiguration,
    -- | The ARN of an application package to run tests against, created with
    -- CreateUpload. See ListUploads.
    appArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the device pool for the run to be scheduled.
    devicePoolArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the project for the run to be scheduled.
    projectArn :: Prelude.Text,
    -- | Information about the test for the run to be scheduled.
    test :: ScheduleRunTest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScheduleRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'scheduleRun_name' - The name for the run to be scheduled.
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
-- 'appArn', 'scheduleRun_appArn' - The ARN of an application package to run tests against, created with
-- CreateUpload. See ListUploads.
--
-- 'devicePoolArn', 'scheduleRun_devicePoolArn' - The ARN of the device pool for the run to be scheduled.
--
-- 'projectArn', 'scheduleRun_projectArn' - The ARN of the project for the run to be scheduled.
--
-- 'test', 'scheduleRun_test' - Information about the test for the run to be scheduled.
newScheduleRun ::
  -- | 'projectArn'
  Prelude.Text ->
  -- | 'test'
  ScheduleRunTest ->
  ScheduleRun
newScheduleRun pProjectArn_ pTest_ =
  ScheduleRun'
    { name = Prelude.Nothing,
      deviceSelectionConfiguration = Prelude.Nothing,
      configuration = Prelude.Nothing,
      executionConfiguration = Prelude.Nothing,
      appArn = Prelude.Nothing,
      devicePoolArn = Prelude.Nothing,
      projectArn = pProjectArn_,
      test = pTest_
    }

-- | The name for the run to be scheduled.
scheduleRun_name :: Lens.Lens' ScheduleRun (Prelude.Maybe Prelude.Text)
scheduleRun_name = Lens.lens (\ScheduleRun' {name} -> name) (\s@ScheduleRun' {} a -> s {name = a} :: ScheduleRun)

-- | The filter criteria used to dynamically select a set of devices for a
-- test run and the maximum number of devices to be included in the run.
--
-- Either __@devicePoolArn@__ or __@deviceSelectionConfiguration@__ is
-- required in a request.
scheduleRun_deviceSelectionConfiguration :: Lens.Lens' ScheduleRun (Prelude.Maybe DeviceSelectionConfiguration)
scheduleRun_deviceSelectionConfiguration = Lens.lens (\ScheduleRun' {deviceSelectionConfiguration} -> deviceSelectionConfiguration) (\s@ScheduleRun' {} a -> s {deviceSelectionConfiguration = a} :: ScheduleRun)

-- | Information about the settings for the run to be scheduled.
scheduleRun_configuration :: Lens.Lens' ScheduleRun (Prelude.Maybe ScheduleRunConfiguration)
scheduleRun_configuration = Lens.lens (\ScheduleRun' {configuration} -> configuration) (\s@ScheduleRun' {} a -> s {configuration = a} :: ScheduleRun)

-- | Specifies configuration information about a test run, such as the
-- execution timeout (in minutes).
scheduleRun_executionConfiguration :: Lens.Lens' ScheduleRun (Prelude.Maybe ExecutionConfiguration)
scheduleRun_executionConfiguration = Lens.lens (\ScheduleRun' {executionConfiguration} -> executionConfiguration) (\s@ScheduleRun' {} a -> s {executionConfiguration = a} :: ScheduleRun)

-- | The ARN of an application package to run tests against, created with
-- CreateUpload. See ListUploads.
scheduleRun_appArn :: Lens.Lens' ScheduleRun (Prelude.Maybe Prelude.Text)
scheduleRun_appArn = Lens.lens (\ScheduleRun' {appArn} -> appArn) (\s@ScheduleRun' {} a -> s {appArn = a} :: ScheduleRun)

-- | The ARN of the device pool for the run to be scheduled.
scheduleRun_devicePoolArn :: Lens.Lens' ScheduleRun (Prelude.Maybe Prelude.Text)
scheduleRun_devicePoolArn = Lens.lens (\ScheduleRun' {devicePoolArn} -> devicePoolArn) (\s@ScheduleRun' {} a -> s {devicePoolArn = a} :: ScheduleRun)

-- | The ARN of the project for the run to be scheduled.
scheduleRun_projectArn :: Lens.Lens' ScheduleRun Prelude.Text
scheduleRun_projectArn = Lens.lens (\ScheduleRun' {projectArn} -> projectArn) (\s@ScheduleRun' {} a -> s {projectArn = a} :: ScheduleRun)

-- | Information about the test for the run to be scheduled.
scheduleRun_test :: Lens.Lens' ScheduleRun ScheduleRunTest
scheduleRun_test = Lens.lens (\ScheduleRun' {test} -> test) (\s@ScheduleRun' {} a -> s {test = a} :: ScheduleRun)

instance Core.AWSRequest ScheduleRun where
  type AWSResponse ScheduleRun = ScheduleRunResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ScheduleRunResponse'
            Prelude.<$> (x Core..?> "run")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ScheduleRun where
  hashWithSalt _salt ScheduleRun' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` deviceSelectionConfiguration
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` executionConfiguration
      `Prelude.hashWithSalt` appArn
      `Prelude.hashWithSalt` devicePoolArn
      `Prelude.hashWithSalt` projectArn
      `Prelude.hashWithSalt` test

instance Prelude.NFData ScheduleRun where
  rnf ScheduleRun' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf deviceSelectionConfiguration
      `Prelude.seq` Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf executionConfiguration
      `Prelude.seq` Prelude.rnf appArn
      `Prelude.seq` Prelude.rnf devicePoolArn
      `Prelude.seq` Prelude.rnf projectArn
      `Prelude.seq` Prelude.rnf test

instance Core.ToHeaders ScheduleRun where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.ScheduleRun" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ScheduleRun where
  toJSON ScheduleRun' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("name" Core..=) Prelude.<$> name,
            ("deviceSelectionConfiguration" Core..=)
              Prelude.<$> deviceSelectionConfiguration,
            ("configuration" Core..=) Prelude.<$> configuration,
            ("executionConfiguration" Core..=)
              Prelude.<$> executionConfiguration,
            ("appArn" Core..=) Prelude.<$> appArn,
            ("devicePoolArn" Core..=) Prelude.<$> devicePoolArn,
            Prelude.Just ("projectArn" Core..= projectArn),
            Prelude.Just ("test" Core..= test)
          ]
      )

instance Core.ToPath ScheduleRun where
  toPath = Prelude.const "/"

instance Core.ToQuery ScheduleRun where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the result of a schedule run request.
--
-- /See:/ 'newScheduleRunResponse' smart constructor.
data ScheduleRunResponse = ScheduleRunResponse'
  { -- | Information about the scheduled run.
    run :: Prelude.Maybe Run,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ScheduleRunResponse
newScheduleRunResponse pHttpStatus_ =
  ScheduleRunResponse'
    { run = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the scheduled run.
scheduleRunResponse_run :: Lens.Lens' ScheduleRunResponse (Prelude.Maybe Run)
scheduleRunResponse_run = Lens.lens (\ScheduleRunResponse' {run} -> run) (\s@ScheduleRunResponse' {} a -> s {run = a} :: ScheduleRunResponse)

-- | The response's http status code.
scheduleRunResponse_httpStatus :: Lens.Lens' ScheduleRunResponse Prelude.Int
scheduleRunResponse_httpStatus = Lens.lens (\ScheduleRunResponse' {httpStatus} -> httpStatus) (\s@ScheduleRunResponse' {} a -> s {httpStatus = a} :: ScheduleRunResponse)

instance Prelude.NFData ScheduleRunResponse where
  rnf ScheduleRunResponse' {..} =
    Prelude.rnf run
      `Prelude.seq` Prelude.rnf httpStatus
