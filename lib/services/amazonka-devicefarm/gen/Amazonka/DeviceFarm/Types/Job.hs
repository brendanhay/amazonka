{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DeviceFarm.Types.Job
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.Job where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DeviceFarm.Types.Counters
import Amazonka.DeviceFarm.Types.Device
import Amazonka.DeviceFarm.Types.DeviceMinutes
import Amazonka.DeviceFarm.Types.ExecutionResult
import Amazonka.DeviceFarm.Types.ExecutionStatus
import Amazonka.DeviceFarm.Types.TestType
import qualified Amazonka.Prelude as Prelude

-- | Represents a device.
--
-- /See:/ 'newJob' smart constructor.
data Job = Job'
  { -- | The job\'s ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The job\'s result counters.
    counters :: Prelude.Maybe Counters,
    -- | When the job was created.
    created :: Prelude.Maybe Data.POSIX,
    -- | The device (phone or tablet).
    device :: Prelude.Maybe Device,
    -- | Represents the total (metered or unmetered) minutes used by the job.
    deviceMinutes :: Prelude.Maybe DeviceMinutes,
    -- | The ARN of the instance.
    instanceArn :: Prelude.Maybe Prelude.Text,
    -- | A message about the job\'s result.
    message :: Prelude.Maybe Prelude.Text,
    -- | The job\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The job\'s result.
    --
    -- Allowed values include:
    --
    -- -   PENDING
    --
    -- -   PASSED
    --
    -- -   WARNED
    --
    -- -   FAILED
    --
    -- -   SKIPPED
    --
    -- -   ERRORED
    --
    -- -   STOPPED
    result :: Prelude.Maybe ExecutionResult,
    -- | The job\'s start time.
    started :: Prelude.Maybe Data.POSIX,
    -- | The job\'s status.
    --
    -- Allowed values include:
    --
    -- -   PENDING
    --
    -- -   PENDING_CONCURRENCY
    --
    -- -   PENDING_DEVICE
    --
    -- -   PROCESSING
    --
    -- -   SCHEDULING
    --
    -- -   PREPARING
    --
    -- -   RUNNING
    --
    -- -   COMPLETED
    --
    -- -   STOPPING
    status :: Prelude.Maybe ExecutionStatus,
    -- | The job\'s stop time.
    stopped :: Prelude.Maybe Data.POSIX,
    -- | The job\'s type.
    --
    -- Allowed values include the following:
    --
    -- -   BUILTIN_FUZZ
    --
    -- -   BUILTIN_EXPLORER. For Android, an app explorer that traverses an
    --     Android app, interacting with it and capturing screenshots at the
    --     same time.
    --
    -- -   APPIUM_JAVA_JUNIT
    --
    -- -   APPIUM_JAVA_TESTNG
    --
    -- -   APPIUM_PYTHON
    --
    -- -   APPIUM_NODE
    --
    -- -   APPIUM_RUBY
    --
    -- -   APPIUM_WEB_JAVA_JUNIT
    --
    -- -   APPIUM_WEB_JAVA_TESTNG
    --
    -- -   APPIUM_WEB_PYTHON
    --
    -- -   APPIUM_WEB_NODE
    --
    -- -   APPIUM_WEB_RUBY
    --
    -- -   CALABASH
    --
    -- -   INSTRUMENTATION
    --
    -- -   UIAUTOMATION
    --
    -- -   UIAUTOMATOR
    --
    -- -   XCTEST
    --
    -- -   XCTEST_UI
    type' :: Prelude.Maybe TestType,
    -- | This value is set to true if video capture is enabled. Otherwise, it is
    -- set to false.
    videoCapture :: Prelude.Maybe Prelude.Bool,
    -- | The endpoint for streaming device video.
    videoEndpoint :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Job' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'job_arn' - The job\'s ARN.
--
-- 'counters', 'job_counters' - The job\'s result counters.
--
-- 'created', 'job_created' - When the job was created.
--
-- 'device', 'job_device' - The device (phone or tablet).
--
-- 'deviceMinutes', 'job_deviceMinutes' - Represents the total (metered or unmetered) minutes used by the job.
--
-- 'instanceArn', 'job_instanceArn' - The ARN of the instance.
--
-- 'message', 'job_message' - A message about the job\'s result.
--
-- 'name', 'job_name' - The job\'s name.
--
-- 'result', 'job_result' - The job\'s result.
--
-- Allowed values include:
--
-- -   PENDING
--
-- -   PASSED
--
-- -   WARNED
--
-- -   FAILED
--
-- -   SKIPPED
--
-- -   ERRORED
--
-- -   STOPPED
--
-- 'started', 'job_started' - The job\'s start time.
--
-- 'status', 'job_status' - The job\'s status.
--
-- Allowed values include:
--
-- -   PENDING
--
-- -   PENDING_CONCURRENCY
--
-- -   PENDING_DEVICE
--
-- -   PROCESSING
--
-- -   SCHEDULING
--
-- -   PREPARING
--
-- -   RUNNING
--
-- -   COMPLETED
--
-- -   STOPPING
--
-- 'stopped', 'job_stopped' - The job\'s stop time.
--
-- 'type'', 'job_type' - The job\'s type.
--
-- Allowed values include the following:
--
-- -   BUILTIN_FUZZ
--
-- -   BUILTIN_EXPLORER. For Android, an app explorer that traverses an
--     Android app, interacting with it and capturing screenshots at the
--     same time.
--
-- -   APPIUM_JAVA_JUNIT
--
-- -   APPIUM_JAVA_TESTNG
--
-- -   APPIUM_PYTHON
--
-- -   APPIUM_NODE
--
-- -   APPIUM_RUBY
--
-- -   APPIUM_WEB_JAVA_JUNIT
--
-- -   APPIUM_WEB_JAVA_TESTNG
--
-- -   APPIUM_WEB_PYTHON
--
-- -   APPIUM_WEB_NODE
--
-- -   APPIUM_WEB_RUBY
--
-- -   CALABASH
--
-- -   INSTRUMENTATION
--
-- -   UIAUTOMATION
--
-- -   UIAUTOMATOR
--
-- -   XCTEST
--
-- -   XCTEST_UI
--
-- 'videoCapture', 'job_videoCapture' - This value is set to true if video capture is enabled. Otherwise, it is
-- set to false.
--
-- 'videoEndpoint', 'job_videoEndpoint' - The endpoint for streaming device video.
newJob ::
  Job
newJob =
  Job'
    { arn = Prelude.Nothing,
      counters = Prelude.Nothing,
      created = Prelude.Nothing,
      device = Prelude.Nothing,
      deviceMinutes = Prelude.Nothing,
      instanceArn = Prelude.Nothing,
      message = Prelude.Nothing,
      name = Prelude.Nothing,
      result = Prelude.Nothing,
      started = Prelude.Nothing,
      status = Prelude.Nothing,
      stopped = Prelude.Nothing,
      type' = Prelude.Nothing,
      videoCapture = Prelude.Nothing,
      videoEndpoint = Prelude.Nothing
    }

-- | The job\'s ARN.
job_arn :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_arn = Lens.lens (\Job' {arn} -> arn) (\s@Job' {} a -> s {arn = a} :: Job)

-- | The job\'s result counters.
job_counters :: Lens.Lens' Job (Prelude.Maybe Counters)
job_counters = Lens.lens (\Job' {counters} -> counters) (\s@Job' {} a -> s {counters = a} :: Job)

-- | When the job was created.
job_created :: Lens.Lens' Job (Prelude.Maybe Prelude.UTCTime)
job_created = Lens.lens (\Job' {created} -> created) (\s@Job' {} a -> s {created = a} :: Job) Prelude.. Lens.mapping Data._Time

-- | The device (phone or tablet).
job_device :: Lens.Lens' Job (Prelude.Maybe Device)
job_device = Lens.lens (\Job' {device} -> device) (\s@Job' {} a -> s {device = a} :: Job)

-- | Represents the total (metered or unmetered) minutes used by the job.
job_deviceMinutes :: Lens.Lens' Job (Prelude.Maybe DeviceMinutes)
job_deviceMinutes = Lens.lens (\Job' {deviceMinutes} -> deviceMinutes) (\s@Job' {} a -> s {deviceMinutes = a} :: Job)

-- | The ARN of the instance.
job_instanceArn :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_instanceArn = Lens.lens (\Job' {instanceArn} -> instanceArn) (\s@Job' {} a -> s {instanceArn = a} :: Job)

-- | A message about the job\'s result.
job_message :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_message = Lens.lens (\Job' {message} -> message) (\s@Job' {} a -> s {message = a} :: Job)

-- | The job\'s name.
job_name :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_name = Lens.lens (\Job' {name} -> name) (\s@Job' {} a -> s {name = a} :: Job)

-- | The job\'s result.
--
-- Allowed values include:
--
-- -   PENDING
--
-- -   PASSED
--
-- -   WARNED
--
-- -   FAILED
--
-- -   SKIPPED
--
-- -   ERRORED
--
-- -   STOPPED
job_result :: Lens.Lens' Job (Prelude.Maybe ExecutionResult)
job_result = Lens.lens (\Job' {result} -> result) (\s@Job' {} a -> s {result = a} :: Job)

-- | The job\'s start time.
job_started :: Lens.Lens' Job (Prelude.Maybe Prelude.UTCTime)
job_started = Lens.lens (\Job' {started} -> started) (\s@Job' {} a -> s {started = a} :: Job) Prelude.. Lens.mapping Data._Time

-- | The job\'s status.
--
-- Allowed values include:
--
-- -   PENDING
--
-- -   PENDING_CONCURRENCY
--
-- -   PENDING_DEVICE
--
-- -   PROCESSING
--
-- -   SCHEDULING
--
-- -   PREPARING
--
-- -   RUNNING
--
-- -   COMPLETED
--
-- -   STOPPING
job_status :: Lens.Lens' Job (Prelude.Maybe ExecutionStatus)
job_status = Lens.lens (\Job' {status} -> status) (\s@Job' {} a -> s {status = a} :: Job)

-- | The job\'s stop time.
job_stopped :: Lens.Lens' Job (Prelude.Maybe Prelude.UTCTime)
job_stopped = Lens.lens (\Job' {stopped} -> stopped) (\s@Job' {} a -> s {stopped = a} :: Job) Prelude.. Lens.mapping Data._Time

-- | The job\'s type.
--
-- Allowed values include the following:
--
-- -   BUILTIN_FUZZ
--
-- -   BUILTIN_EXPLORER. For Android, an app explorer that traverses an
--     Android app, interacting with it and capturing screenshots at the
--     same time.
--
-- -   APPIUM_JAVA_JUNIT
--
-- -   APPIUM_JAVA_TESTNG
--
-- -   APPIUM_PYTHON
--
-- -   APPIUM_NODE
--
-- -   APPIUM_RUBY
--
-- -   APPIUM_WEB_JAVA_JUNIT
--
-- -   APPIUM_WEB_JAVA_TESTNG
--
-- -   APPIUM_WEB_PYTHON
--
-- -   APPIUM_WEB_NODE
--
-- -   APPIUM_WEB_RUBY
--
-- -   CALABASH
--
-- -   INSTRUMENTATION
--
-- -   UIAUTOMATION
--
-- -   UIAUTOMATOR
--
-- -   XCTEST
--
-- -   XCTEST_UI
job_type :: Lens.Lens' Job (Prelude.Maybe TestType)
job_type = Lens.lens (\Job' {type'} -> type') (\s@Job' {} a -> s {type' = a} :: Job)

-- | This value is set to true if video capture is enabled. Otherwise, it is
-- set to false.
job_videoCapture :: Lens.Lens' Job (Prelude.Maybe Prelude.Bool)
job_videoCapture = Lens.lens (\Job' {videoCapture} -> videoCapture) (\s@Job' {} a -> s {videoCapture = a} :: Job)

-- | The endpoint for streaming device video.
job_videoEndpoint :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_videoEndpoint = Lens.lens (\Job' {videoEndpoint} -> videoEndpoint) (\s@Job' {} a -> s {videoEndpoint = a} :: Job)

instance Data.FromJSON Job where
  parseJSON =
    Data.withObject
      "Job"
      ( \x ->
          Job'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "counters")
            Prelude.<*> (x Data..:? "created")
            Prelude.<*> (x Data..:? "device")
            Prelude.<*> (x Data..:? "deviceMinutes")
            Prelude.<*> (x Data..:? "instanceArn")
            Prelude.<*> (x Data..:? "message")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "result")
            Prelude.<*> (x Data..:? "started")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "stopped")
            Prelude.<*> (x Data..:? "type")
            Prelude.<*> (x Data..:? "videoCapture")
            Prelude.<*> (x Data..:? "videoEndpoint")
      )

instance Prelude.Hashable Job where
  hashWithSalt _salt Job' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` counters
      `Prelude.hashWithSalt` created
      `Prelude.hashWithSalt` device
      `Prelude.hashWithSalt` deviceMinutes
      `Prelude.hashWithSalt` instanceArn
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` result
      `Prelude.hashWithSalt` started
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` stopped
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` videoCapture
      `Prelude.hashWithSalt` videoEndpoint

instance Prelude.NFData Job where
  rnf Job' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf counters
      `Prelude.seq` Prelude.rnf created
      `Prelude.seq` Prelude.rnf device
      `Prelude.seq` Prelude.rnf deviceMinutes
      `Prelude.seq` Prelude.rnf instanceArn
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf result
      `Prelude.seq` Prelude.rnf started
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf stopped
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf videoCapture
      `Prelude.seq` Prelude.rnf videoEndpoint
