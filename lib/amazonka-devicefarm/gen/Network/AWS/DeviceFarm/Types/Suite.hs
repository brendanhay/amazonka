{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Suite
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.Suite
  ( Suite (..),

    -- * Smart constructor
    mkSuite,

    -- * Lenses
    sArn,
    sCounters,
    sCreated,
    sDeviceMinutes,
    sMessage,
    sName,
    sResult,
    sStarted,
    sStatus,
    sStopped,
    sType,
  )
where

import qualified Network.AWS.DeviceFarm.Types.AmazonResourceName as Types
import qualified Network.AWS.DeviceFarm.Types.Counters as Types
import qualified Network.AWS.DeviceFarm.Types.DeviceMinutes as Types
import qualified Network.AWS.DeviceFarm.Types.ExecutionResult as Types
import qualified Network.AWS.DeviceFarm.Types.ExecutionStatus as Types
import qualified Network.AWS.DeviceFarm.Types.Message as Types
import qualified Network.AWS.DeviceFarm.Types.Name as Types
import qualified Network.AWS.DeviceFarm.Types.TestType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a collection of one or more tests.
--
-- /See:/ 'mkSuite' smart constructor.
data Suite = Suite'
  { -- | The suite's ARN.
    arn :: Core.Maybe Types.AmazonResourceName,
    -- | The suite's result counters.
    counters :: Core.Maybe Types.Counters,
    -- | When the suite was created.
    created :: Core.Maybe Core.NominalDiffTime,
    -- | Represents the total (metered or unmetered) minutes used by the test suite.
    deviceMinutes :: Core.Maybe Types.DeviceMinutes,
    -- | A message about the suite's result.
    message :: Core.Maybe Types.Message,
    -- | The suite's name.
    name :: Core.Maybe Types.Name,
    -- | The suite's result.
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
    result :: Core.Maybe Types.ExecutionResult,
    -- | The suite's start time.
    started :: Core.Maybe Core.NominalDiffTime,
    -- | The suite's status.
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
    status :: Core.Maybe Types.ExecutionStatus,
    -- | The suite's stop time.
    stopped :: Core.Maybe Core.NominalDiffTime,
    -- | The suite's type.
    --
    -- Must be one of the following values:
    --
    --     * BUILTIN_FUZZ
    --
    --
    --     * BUILTIN_EXPLORER
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
    type' :: Core.Maybe Types.TestType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Suite' value with any optional fields omitted.
mkSuite ::
  Suite
mkSuite =
  Suite'
    { arn = Core.Nothing,
      counters = Core.Nothing,
      created = Core.Nothing,
      deviceMinutes = Core.Nothing,
      message = Core.Nothing,
      name = Core.Nothing,
      result = Core.Nothing,
      started = Core.Nothing,
      status = Core.Nothing,
      stopped = Core.Nothing,
      type' = Core.Nothing
    }

-- | The suite's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sArn :: Lens.Lens' Suite (Core.Maybe Types.AmazonResourceName)
sArn = Lens.field @"arn"
{-# DEPRECATED sArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The suite's result counters.
--
-- /Note:/ Consider using 'counters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCounters :: Lens.Lens' Suite (Core.Maybe Types.Counters)
sCounters = Lens.field @"counters"
{-# DEPRECATED sCounters "Use generic-lens or generic-optics with 'counters' instead." #-}

-- | When the suite was created.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCreated :: Lens.Lens' Suite (Core.Maybe Core.NominalDiffTime)
sCreated = Lens.field @"created"
{-# DEPRECATED sCreated "Use generic-lens or generic-optics with 'created' instead." #-}

-- | Represents the total (metered or unmetered) minutes used by the test suite.
--
-- /Note:/ Consider using 'deviceMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDeviceMinutes :: Lens.Lens' Suite (Core.Maybe Types.DeviceMinutes)
sDeviceMinutes = Lens.field @"deviceMinutes"
{-# DEPRECATED sDeviceMinutes "Use generic-lens or generic-optics with 'deviceMinutes' instead." #-}

-- | A message about the suite's result.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMessage :: Lens.Lens' Suite (Core.Maybe Types.Message)
sMessage = Lens.field @"message"
{-# DEPRECATED sMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The suite's name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sName :: Lens.Lens' Suite (Core.Maybe Types.Name)
sName = Lens.field @"name"
{-# DEPRECATED sName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The suite's result.
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
sResult :: Lens.Lens' Suite (Core.Maybe Types.ExecutionResult)
sResult = Lens.field @"result"
{-# DEPRECATED sResult "Use generic-lens or generic-optics with 'result' instead." #-}

-- | The suite's start time.
--
-- /Note:/ Consider using 'started' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStarted :: Lens.Lens' Suite (Core.Maybe Core.NominalDiffTime)
sStarted = Lens.field @"started"
{-# DEPRECATED sStarted "Use generic-lens or generic-optics with 'started' instead." #-}

-- | The suite's status.
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
sStatus :: Lens.Lens' Suite (Core.Maybe Types.ExecutionStatus)
sStatus = Lens.field @"status"
{-# DEPRECATED sStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The suite's stop time.
--
-- /Note:/ Consider using 'stopped' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStopped :: Lens.Lens' Suite (Core.Maybe Core.NominalDiffTime)
sStopped = Lens.field @"stopped"
{-# DEPRECATED sStopped "Use generic-lens or generic-optics with 'stopped' instead." #-}

-- | The suite's type.
--
-- Must be one of the following values:
--
--     * BUILTIN_FUZZ
--
--
--     * BUILTIN_EXPLORER
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
sType :: Lens.Lens' Suite (Core.Maybe Types.TestType)
sType = Lens.field @"type'"
{-# DEPRECATED sType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON Suite where
  parseJSON =
    Core.withObject "Suite" Core.$
      \x ->
        Suite'
          Core.<$> (x Core..:? "arn")
          Core.<*> (x Core..:? "counters")
          Core.<*> (x Core..:? "created")
          Core.<*> (x Core..:? "deviceMinutes")
          Core.<*> (x Core..:? "message")
          Core.<*> (x Core..:? "name")
          Core.<*> (x Core..:? "result")
          Core.<*> (x Core..:? "started")
          Core.<*> (x Core..:? "status")
          Core.<*> (x Core..:? "stopped")
          Core.<*> (x Core..:? "type")
