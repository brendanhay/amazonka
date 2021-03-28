{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Job
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.Job
  ( Job (..)
  -- * Smart constructor
  , mkJob
  -- * Lenses
  , jArn
  , jCounters
  , jCreated
  , jDevice
  , jDeviceMinutes
  , jInstanceArn
  , jMessage
  , jName
  , jResult
  , jStarted
  , jStatus
  , jStopped
  , jType
  , jVideoCapture
  , jVideoEndpoint
  ) where

import qualified Network.AWS.DeviceFarm.Types.Arn as Types
import qualified Network.AWS.DeviceFarm.Types.Counters as Types
import qualified Network.AWS.DeviceFarm.Types.Device as Types
import qualified Network.AWS.DeviceFarm.Types.DeviceMinutes as Types
import qualified Network.AWS.DeviceFarm.Types.ExecutionResult as Types
import qualified Network.AWS.DeviceFarm.Types.ExecutionStatus as Types
import qualified Network.AWS.DeviceFarm.Types.InstanceArn as Types
import qualified Network.AWS.DeviceFarm.Types.Message as Types
import qualified Network.AWS.DeviceFarm.Types.Name as Types
import qualified Network.AWS.DeviceFarm.Types.TestType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a device.
--
-- /See:/ 'mkJob' smart constructor.
data Job = Job'
  { arn :: Core.Maybe Types.Arn
    -- ^ The job's ARN.
  , counters :: Core.Maybe Types.Counters
    -- ^ The job's result counters.
  , created :: Core.Maybe Core.NominalDiffTime
    -- ^ When the job was created.
  , device :: Core.Maybe Types.Device
    -- ^ The device (phone or tablet).
  , deviceMinutes :: Core.Maybe Types.DeviceMinutes
    -- ^ Represents the total (metered or unmetered) minutes used by the job.
  , instanceArn :: Core.Maybe Types.InstanceArn
    -- ^ The ARN of the instance.
  , message :: Core.Maybe Types.Message
    -- ^ A message about the job's result.
  , name :: Core.Maybe Types.Name
    -- ^ The job's name.
  , result :: Core.Maybe Types.ExecutionResult
    -- ^ The job's result.
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
  , started :: Core.Maybe Core.NominalDiffTime
    -- ^ The job's start time.
  , status :: Core.Maybe Types.ExecutionStatus
    -- ^ The job's status.
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
  , stopped :: Core.Maybe Core.NominalDiffTime
    -- ^ The job's stop time.
  , type' :: Core.Maybe Types.TestType
    -- ^ The job's type.
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
  , videoCapture :: Core.Maybe Core.Bool
    -- ^ This value is set to true if video capture is enabled. Otherwise, it is set to false.
  , videoEndpoint :: Core.Maybe Core.Text
    -- ^ The endpoint for streaming device video.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Job' value with any optional fields omitted.
mkJob
    :: Job
mkJob
  = Job'{arn = Core.Nothing, counters = Core.Nothing,
         created = Core.Nothing, device = Core.Nothing,
         deviceMinutes = Core.Nothing, instanceArn = Core.Nothing,
         message = Core.Nothing, name = Core.Nothing, result = Core.Nothing,
         started = Core.Nothing, status = Core.Nothing,
         stopped = Core.Nothing, type' = Core.Nothing,
         videoCapture = Core.Nothing, videoEndpoint = Core.Nothing}

-- | The job's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jArn :: Lens.Lens' Job (Core.Maybe Types.Arn)
jArn = Lens.field @"arn"
{-# INLINEABLE jArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The job's result counters.
--
-- /Note:/ Consider using 'counters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jCounters :: Lens.Lens' Job (Core.Maybe Types.Counters)
jCounters = Lens.field @"counters"
{-# INLINEABLE jCounters #-}
{-# DEPRECATED counters "Use generic-lens or generic-optics with 'counters' instead"  #-}

-- | When the job was created.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jCreated :: Lens.Lens' Job (Core.Maybe Core.NominalDiffTime)
jCreated = Lens.field @"created"
{-# INLINEABLE jCreated #-}
{-# DEPRECATED created "Use generic-lens or generic-optics with 'created' instead"  #-}

-- | The device (phone or tablet).
--
-- /Note:/ Consider using 'device' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jDevice :: Lens.Lens' Job (Core.Maybe Types.Device)
jDevice = Lens.field @"device"
{-# INLINEABLE jDevice #-}
{-# DEPRECATED device "Use generic-lens or generic-optics with 'device' instead"  #-}

-- | Represents the total (metered or unmetered) minutes used by the job.
--
-- /Note:/ Consider using 'deviceMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jDeviceMinutes :: Lens.Lens' Job (Core.Maybe Types.DeviceMinutes)
jDeviceMinutes = Lens.field @"deviceMinutes"
{-# INLINEABLE jDeviceMinutes #-}
{-# DEPRECATED deviceMinutes "Use generic-lens or generic-optics with 'deviceMinutes' instead"  #-}

-- | The ARN of the instance.
--
-- /Note:/ Consider using 'instanceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jInstanceArn :: Lens.Lens' Job (Core.Maybe Types.InstanceArn)
jInstanceArn = Lens.field @"instanceArn"
{-# INLINEABLE jInstanceArn #-}
{-# DEPRECATED instanceArn "Use generic-lens or generic-optics with 'instanceArn' instead"  #-}

-- | A message about the job's result.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jMessage :: Lens.Lens' Job (Core.Maybe Types.Message)
jMessage = Lens.field @"message"
{-# INLINEABLE jMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

-- | The job's name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jName :: Lens.Lens' Job (Core.Maybe Types.Name)
jName = Lens.field @"name"
{-# INLINEABLE jName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

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
jResult :: Lens.Lens' Job (Core.Maybe Types.ExecutionResult)
jResult = Lens.field @"result"
{-# INLINEABLE jResult #-}
{-# DEPRECATED result "Use generic-lens or generic-optics with 'result' instead"  #-}

-- | The job's start time.
--
-- /Note:/ Consider using 'started' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jStarted :: Lens.Lens' Job (Core.Maybe Core.NominalDiffTime)
jStarted = Lens.field @"started"
{-# INLINEABLE jStarted #-}
{-# DEPRECATED started "Use generic-lens or generic-optics with 'started' instead"  #-}

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
jStatus :: Lens.Lens' Job (Core.Maybe Types.ExecutionStatus)
jStatus = Lens.field @"status"
{-# INLINEABLE jStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The job's stop time.
--
-- /Note:/ Consider using 'stopped' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jStopped :: Lens.Lens' Job (Core.Maybe Core.NominalDiffTime)
jStopped = Lens.field @"stopped"
{-# INLINEABLE jStopped #-}
{-# DEPRECATED stopped "Use generic-lens or generic-optics with 'stopped' instead"  #-}

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
jType :: Lens.Lens' Job (Core.Maybe Types.TestType)
jType = Lens.field @"type'"
{-# INLINEABLE jType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | This value is set to true if video capture is enabled. Otherwise, it is set to false.
--
-- /Note:/ Consider using 'videoCapture' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jVideoCapture :: Lens.Lens' Job (Core.Maybe Core.Bool)
jVideoCapture = Lens.field @"videoCapture"
{-# INLINEABLE jVideoCapture #-}
{-# DEPRECATED videoCapture "Use generic-lens or generic-optics with 'videoCapture' instead"  #-}

-- | The endpoint for streaming device video.
--
-- /Note:/ Consider using 'videoEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jVideoEndpoint :: Lens.Lens' Job (Core.Maybe Core.Text)
jVideoEndpoint = Lens.field @"videoEndpoint"
{-# INLINEABLE jVideoEndpoint #-}
{-# DEPRECATED videoEndpoint "Use generic-lens or generic-optics with 'videoEndpoint' instead"  #-}

instance Core.FromJSON Job where
        parseJSON
          = Core.withObject "Job" Core.$
              \ x ->
                Job' Core.<$>
                  (x Core..:? "arn") Core.<*> x Core..:? "counters" Core.<*>
                    x Core..:? "created"
                    Core.<*> x Core..:? "device"
                    Core.<*> x Core..:? "deviceMinutes"
                    Core.<*> x Core..:? "instanceArn"
                    Core.<*> x Core..:? "message"
                    Core.<*> x Core..:? "name"
                    Core.<*> x Core..:? "result"
                    Core.<*> x Core..:? "started"
                    Core.<*> x Core..:? "status"
                    Core.<*> x Core..:? "stopped"
                    Core.<*> x Core..:? "type"
                    Core.<*> x Core..:? "videoCapture"
                    Core.<*> x Core..:? "videoEndpoint"
