{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.DeviceFarm.Types.Job
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.Job where

import Network.AWS.DeviceFarm.Types.Counters
import Network.AWS.DeviceFarm.Types.Device
import Network.AWS.DeviceFarm.Types.DeviceMinutes
import Network.AWS.DeviceFarm.Types.ExecutionResult
import Network.AWS.DeviceFarm.Types.ExecutionStatus
import Network.AWS.DeviceFarm.Types.TestType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents a device.
--
-- /See:/ 'newJob' smart constructor.
data Job = Job'
  { -- | The job\'s result counters.
    counters :: Prelude.Maybe Counters,
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
    started :: Prelude.Maybe Prelude.POSIX,
    -- | A message about the job\'s result.
    message :: Prelude.Maybe Prelude.Text,
    -- | The device (phone or tablet).
    device :: Prelude.Maybe Device,
    -- | This value is set to true if video capture is enabled. Otherwise, it is
    -- set to false.
    videoCapture :: Prelude.Maybe Prelude.Bool,
    -- | The job\'s ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The endpoint for streaming device video.
    videoEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The job\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the instance.
    instanceArn :: Prelude.Maybe Prelude.Text,
    -- | The job\'s stop time.
    stopped :: Prelude.Maybe Prelude.POSIX,
    -- | When the job was created.
    created :: Prelude.Maybe Prelude.POSIX,
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
    -- | Represents the total (metered or unmetered) minutes used by the job.
    deviceMinutes :: Prelude.Maybe DeviceMinutes
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Job' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'counters', 'job_counters' - The job\'s result counters.
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
-- 'message', 'job_message' - A message about the job\'s result.
--
-- 'device', 'job_device' - The device (phone or tablet).
--
-- 'videoCapture', 'job_videoCapture' - This value is set to true if video capture is enabled. Otherwise, it is
-- set to false.
--
-- 'arn', 'job_arn' - The job\'s ARN.
--
-- 'videoEndpoint', 'job_videoEndpoint' - The endpoint for streaming device video.
--
-- 'name', 'job_name' - The job\'s name.
--
-- 'instanceArn', 'job_instanceArn' - The ARN of the instance.
--
-- 'stopped', 'job_stopped' - The job\'s stop time.
--
-- 'created', 'job_created' - When the job was created.
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
-- 'deviceMinutes', 'job_deviceMinutes' - Represents the total (metered or unmetered) minutes used by the job.
newJob ::
  Job
newJob =
  Job'
    { counters = Prelude.Nothing,
      status = Prelude.Nothing,
      result = Prelude.Nothing,
      started = Prelude.Nothing,
      message = Prelude.Nothing,
      device = Prelude.Nothing,
      videoCapture = Prelude.Nothing,
      arn = Prelude.Nothing,
      videoEndpoint = Prelude.Nothing,
      name = Prelude.Nothing,
      instanceArn = Prelude.Nothing,
      stopped = Prelude.Nothing,
      created = Prelude.Nothing,
      type' = Prelude.Nothing,
      deviceMinutes = Prelude.Nothing
    }

-- | The job\'s result counters.
job_counters :: Lens.Lens' Job (Prelude.Maybe Counters)
job_counters = Lens.lens (\Job' {counters} -> counters) (\s@Job' {} a -> s {counters = a} :: Job)

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
job_started = Lens.lens (\Job' {started} -> started) (\s@Job' {} a -> s {started = a} :: Job) Prelude.. Lens.mapping Prelude._Time

-- | A message about the job\'s result.
job_message :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_message = Lens.lens (\Job' {message} -> message) (\s@Job' {} a -> s {message = a} :: Job)

-- | The device (phone or tablet).
job_device :: Lens.Lens' Job (Prelude.Maybe Device)
job_device = Lens.lens (\Job' {device} -> device) (\s@Job' {} a -> s {device = a} :: Job)

-- | This value is set to true if video capture is enabled. Otherwise, it is
-- set to false.
job_videoCapture :: Lens.Lens' Job (Prelude.Maybe Prelude.Bool)
job_videoCapture = Lens.lens (\Job' {videoCapture} -> videoCapture) (\s@Job' {} a -> s {videoCapture = a} :: Job)

-- | The job\'s ARN.
job_arn :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_arn = Lens.lens (\Job' {arn} -> arn) (\s@Job' {} a -> s {arn = a} :: Job)

-- | The endpoint for streaming device video.
job_videoEndpoint :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_videoEndpoint = Lens.lens (\Job' {videoEndpoint} -> videoEndpoint) (\s@Job' {} a -> s {videoEndpoint = a} :: Job)

-- | The job\'s name.
job_name :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_name = Lens.lens (\Job' {name} -> name) (\s@Job' {} a -> s {name = a} :: Job)

-- | The ARN of the instance.
job_instanceArn :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_instanceArn = Lens.lens (\Job' {instanceArn} -> instanceArn) (\s@Job' {} a -> s {instanceArn = a} :: Job)

-- | The job\'s stop time.
job_stopped :: Lens.Lens' Job (Prelude.Maybe Prelude.UTCTime)
job_stopped = Lens.lens (\Job' {stopped} -> stopped) (\s@Job' {} a -> s {stopped = a} :: Job) Prelude.. Lens.mapping Prelude._Time

-- | When the job was created.
job_created :: Lens.Lens' Job (Prelude.Maybe Prelude.UTCTime)
job_created = Lens.lens (\Job' {created} -> created) (\s@Job' {} a -> s {created = a} :: Job) Prelude.. Lens.mapping Prelude._Time

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

-- | Represents the total (metered or unmetered) minutes used by the job.
job_deviceMinutes :: Lens.Lens' Job (Prelude.Maybe DeviceMinutes)
job_deviceMinutes = Lens.lens (\Job' {deviceMinutes} -> deviceMinutes) (\s@Job' {} a -> s {deviceMinutes = a} :: Job)

instance Prelude.FromJSON Job where
  parseJSON =
    Prelude.withObject
      "Job"
      ( \x ->
          Job'
            Prelude.<$> (x Prelude..:? "counters")
            Prelude.<*> (x Prelude..:? "status")
            Prelude.<*> (x Prelude..:? "result")
            Prelude.<*> (x Prelude..:? "started")
            Prelude.<*> (x Prelude..:? "message")
            Prelude.<*> (x Prelude..:? "device")
            Prelude.<*> (x Prelude..:? "videoCapture")
            Prelude.<*> (x Prelude..:? "arn")
            Prelude.<*> (x Prelude..:? "videoEndpoint")
            Prelude.<*> (x Prelude..:? "name")
            Prelude.<*> (x Prelude..:? "instanceArn")
            Prelude.<*> (x Prelude..:? "stopped")
            Prelude.<*> (x Prelude..:? "created")
            Prelude.<*> (x Prelude..:? "type")
            Prelude.<*> (x Prelude..:? "deviceMinutes")
      )

instance Prelude.Hashable Job

instance Prelude.NFData Job
