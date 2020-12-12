{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Job
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.Job
  ( Job (..),

    -- * Smart constructor
    mkJob,

    -- * Lenses
    jInstanceARN,
    jStatus,
    jCounters,
    jArn,
    jCreated,
    jDevice,
    jStopped,
    jResult,
    jName,
    jVideoEndpoint,
    jDeviceMinutes,
    jVideoCapture,
    jType,
    jMessage,
    jStarted,
  )
where

import Network.AWS.DeviceFarm.Types.Counters
import Network.AWS.DeviceFarm.Types.Device
import Network.AWS.DeviceFarm.Types.DeviceMinutes
import Network.AWS.DeviceFarm.Types.ExecutionResult
import Network.AWS.DeviceFarm.Types.ExecutionStatus
import Network.AWS.DeviceFarm.Types.TestType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a device.
--
-- /See:/ 'mkJob' smart constructor.
data Job = Job'
  { instanceARN :: Lude.Maybe Lude.Text,
    status :: Lude.Maybe ExecutionStatus,
    counters :: Lude.Maybe Counters,
    arn :: Lude.Maybe Lude.Text,
    created :: Lude.Maybe Lude.Timestamp,
    device :: Lude.Maybe Device,
    stopped :: Lude.Maybe Lude.Timestamp,
    result :: Lude.Maybe ExecutionResult,
    name :: Lude.Maybe Lude.Text,
    videoEndpoint :: Lude.Maybe Lude.Text,
    deviceMinutes :: Lude.Maybe DeviceMinutes,
    videoCapture :: Lude.Maybe Lude.Bool,
    type' :: Lude.Maybe TestType,
    message :: Lude.Maybe Lude.Text,
    started :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Job' with the minimum fields required to make a request.
--
-- * 'arn' - The job's ARN.
-- * 'counters' - The job's result counters.
-- * 'created' - When the job was created.
-- * 'device' - The device (phone or tablet).
-- * 'deviceMinutes' - Represents the total (metered or unmetered) minutes used by the job.
-- * 'instanceARN' - The ARN of the instance.
-- * 'message' - A message about the job's result.
-- * 'name' - The job's name.
-- * 'result' - The job's result.
--
-- Allowed values include:
--
--     * PENDING
--
--
--     * PASSED
--
--
--     * WARNED
--
--
--     * FAILED
--
--
--     * SKIPPED
--
--
--     * ERRORED
--
--
--     * STOPPED
--
--
-- * 'started' - The job's start time.
-- * 'status' - The job's status.
--
-- Allowed values include:
--
--     * PENDING
--
--
--     * PENDING_CONCURRENCY
--
--
--     * PENDING_DEVICE
--
--
--     * PROCESSING
--
--
--     * SCHEDULING
--
--
--     * PREPARING
--
--
--     * RUNNING
--
--
--     * COMPLETED
--
--
--     * STOPPING
--
--
-- * 'stopped' - The job's stop time.
-- * 'type'' - The job's type.
--
-- Allowed values include the following:
--
--     * BUILTIN_FUZZ
--
--
--     * BUILTIN_EXPLORER. For Android, an app explorer that traverses an Android app, interacting with it and capturing screenshots at the same time.
--
--
--     * APPIUM_JAVA_JUNIT
--
--
--     * APPIUM_JAVA_TESTNG
--
--
--     * APPIUM_PYTHON
--
--
--     * APPIUM_NODE
--
--
--     * APPIUM_RUBY
--
--
--     * APPIUM_WEB_JAVA_JUNIT
--
--
--     * APPIUM_WEB_JAVA_TESTNG
--
--
--     * APPIUM_WEB_PYTHON
--
--
--     * APPIUM_WEB_NODE
--
--
--     * APPIUM_WEB_RUBY
--
--
--     * CALABASH
--
--
--     * INSTRUMENTATION
--
--
--     * UIAUTOMATION
--
--
--     * UIAUTOMATOR
--
--
--     * XCTEST
--
--
--     * XCTEST_UI
--
--
-- * 'videoCapture' - This value is set to true if video capture is enabled. Otherwise, it is set to false.
-- * 'videoEndpoint' - The endpoint for streaming device video.
mkJob ::
  Job
mkJob =
  Job'
    { instanceARN = Lude.Nothing,
      status = Lude.Nothing,
      counters = Lude.Nothing,
      arn = Lude.Nothing,
      created = Lude.Nothing,
      device = Lude.Nothing,
      stopped = Lude.Nothing,
      result = Lude.Nothing,
      name = Lude.Nothing,
      videoEndpoint = Lude.Nothing,
      deviceMinutes = Lude.Nothing,
      videoCapture = Lude.Nothing,
      type' = Lude.Nothing,
      message = Lude.Nothing,
      started = Lude.Nothing
    }

-- | The ARN of the instance.
--
-- /Note:/ Consider using 'instanceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jInstanceARN :: Lens.Lens' Job (Lude.Maybe Lude.Text)
jInstanceARN = Lens.lens (instanceARN :: Job -> Lude.Maybe Lude.Text) (\s a -> s {instanceARN = a} :: Job)
{-# DEPRECATED jInstanceARN "Use generic-lens or generic-optics with 'instanceARN' instead." #-}

-- | The job's status.
--
-- Allowed values include:
--
--     * PENDING
--
--
--     * PENDING_CONCURRENCY
--
--
--     * PENDING_DEVICE
--
--
--     * PROCESSING
--
--
--     * SCHEDULING
--
--
--     * PREPARING
--
--
--     * RUNNING
--
--
--     * COMPLETED
--
--
--     * STOPPING
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jStatus :: Lens.Lens' Job (Lude.Maybe ExecutionStatus)
jStatus = Lens.lens (status :: Job -> Lude.Maybe ExecutionStatus) (\s a -> s {status = a} :: Job)
{-# DEPRECATED jStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The job's result counters.
--
-- /Note:/ Consider using 'counters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jCounters :: Lens.Lens' Job (Lude.Maybe Counters)
jCounters = Lens.lens (counters :: Job -> Lude.Maybe Counters) (\s a -> s {counters = a} :: Job)
{-# DEPRECATED jCounters "Use generic-lens or generic-optics with 'counters' instead." #-}

-- | The job's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jArn :: Lens.Lens' Job (Lude.Maybe Lude.Text)
jArn = Lens.lens (arn :: Job -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Job)
{-# DEPRECATED jArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | When the job was created.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jCreated :: Lens.Lens' Job (Lude.Maybe Lude.Timestamp)
jCreated = Lens.lens (created :: Job -> Lude.Maybe Lude.Timestamp) (\s a -> s {created = a} :: Job)
{-# DEPRECATED jCreated "Use generic-lens or generic-optics with 'created' instead." #-}

-- | The device (phone or tablet).
--
-- /Note:/ Consider using 'device' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jDevice :: Lens.Lens' Job (Lude.Maybe Device)
jDevice = Lens.lens (device :: Job -> Lude.Maybe Device) (\s a -> s {device = a} :: Job)
{-# DEPRECATED jDevice "Use generic-lens or generic-optics with 'device' instead." #-}

-- | The job's stop time.
--
-- /Note:/ Consider using 'stopped' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jStopped :: Lens.Lens' Job (Lude.Maybe Lude.Timestamp)
jStopped = Lens.lens (stopped :: Job -> Lude.Maybe Lude.Timestamp) (\s a -> s {stopped = a} :: Job)
{-# DEPRECATED jStopped "Use generic-lens or generic-optics with 'stopped' instead." #-}

-- | The job's result.
--
-- Allowed values include:
--
--     * PENDING
--
--
--     * PASSED
--
--
--     * WARNED
--
--
--     * FAILED
--
--
--     * SKIPPED
--
--
--     * ERRORED
--
--
--     * STOPPED
--
--
--
-- /Note:/ Consider using 'result' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jResult :: Lens.Lens' Job (Lude.Maybe ExecutionResult)
jResult = Lens.lens (result :: Job -> Lude.Maybe ExecutionResult) (\s a -> s {result = a} :: Job)
{-# DEPRECATED jResult "Use generic-lens or generic-optics with 'result' instead." #-}

-- | The job's name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jName :: Lens.Lens' Job (Lude.Maybe Lude.Text)
jName = Lens.lens (name :: Job -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Job)
{-# DEPRECATED jName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The endpoint for streaming device video.
--
-- /Note:/ Consider using 'videoEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jVideoEndpoint :: Lens.Lens' Job (Lude.Maybe Lude.Text)
jVideoEndpoint = Lens.lens (videoEndpoint :: Job -> Lude.Maybe Lude.Text) (\s a -> s {videoEndpoint = a} :: Job)
{-# DEPRECATED jVideoEndpoint "Use generic-lens or generic-optics with 'videoEndpoint' instead." #-}

-- | Represents the total (metered or unmetered) minutes used by the job.
--
-- /Note:/ Consider using 'deviceMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jDeviceMinutes :: Lens.Lens' Job (Lude.Maybe DeviceMinutes)
jDeviceMinutes = Lens.lens (deviceMinutes :: Job -> Lude.Maybe DeviceMinutes) (\s a -> s {deviceMinutes = a} :: Job)
{-# DEPRECATED jDeviceMinutes "Use generic-lens or generic-optics with 'deviceMinutes' instead." #-}

-- | This value is set to true if video capture is enabled. Otherwise, it is set to false.
--
-- /Note:/ Consider using 'videoCapture' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jVideoCapture :: Lens.Lens' Job (Lude.Maybe Lude.Bool)
jVideoCapture = Lens.lens (videoCapture :: Job -> Lude.Maybe Lude.Bool) (\s a -> s {videoCapture = a} :: Job)
{-# DEPRECATED jVideoCapture "Use generic-lens or generic-optics with 'videoCapture' instead." #-}

-- | The job's type.
--
-- Allowed values include the following:
--
--     * BUILTIN_FUZZ
--
--
--     * BUILTIN_EXPLORER. For Android, an app explorer that traverses an Android app, interacting with it and capturing screenshots at the same time.
--
--
--     * APPIUM_JAVA_JUNIT
--
--
--     * APPIUM_JAVA_TESTNG
--
--
--     * APPIUM_PYTHON
--
--
--     * APPIUM_NODE
--
--
--     * APPIUM_RUBY
--
--
--     * APPIUM_WEB_JAVA_JUNIT
--
--
--     * APPIUM_WEB_JAVA_TESTNG
--
--
--     * APPIUM_WEB_PYTHON
--
--
--     * APPIUM_WEB_NODE
--
--
--     * APPIUM_WEB_RUBY
--
--
--     * CALABASH
--
--
--     * INSTRUMENTATION
--
--
--     * UIAUTOMATION
--
--
--     * UIAUTOMATOR
--
--
--     * XCTEST
--
--
--     * XCTEST_UI
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jType :: Lens.Lens' Job (Lude.Maybe TestType)
jType = Lens.lens (type' :: Job -> Lude.Maybe TestType) (\s a -> s {type' = a} :: Job)
{-# DEPRECATED jType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | A message about the job's result.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jMessage :: Lens.Lens' Job (Lude.Maybe Lude.Text)
jMessage = Lens.lens (message :: Job -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: Job)
{-# DEPRECATED jMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The job's start time.
--
-- /Note:/ Consider using 'started' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jStarted :: Lens.Lens' Job (Lude.Maybe Lude.Timestamp)
jStarted = Lens.lens (started :: Job -> Lude.Maybe Lude.Timestamp) (\s a -> s {started = a} :: Job)
{-# DEPRECATED jStarted "Use generic-lens or generic-optics with 'started' instead." #-}

instance Lude.FromJSON Job where
  parseJSON =
    Lude.withObject
      "Job"
      ( \x ->
          Job'
            Lude.<$> (x Lude..:? "instanceArn")
            Lude.<*> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "counters")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "created")
            Lude.<*> (x Lude..:? "device")
            Lude.<*> (x Lude..:? "stopped")
            Lude.<*> (x Lude..:? "result")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "videoEndpoint")
            Lude.<*> (x Lude..:? "deviceMinutes")
            Lude.<*> (x Lude..:? "videoCapture")
            Lude.<*> (x Lude..:? "type")
            Lude.<*> (x Lude..:? "message")
            Lude.<*> (x Lude..:? "started")
      )
