{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Test
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.Test
  ( Test (..),

    -- * Smart constructor
    mkTest,

    -- * Lenses
    tArn,
    tCounters,
    tCreated,
    tDeviceMinutes,
    tMessage,
    tName,
    tResult,
    tStarted,
    tStatus,
    tStopped,
    tType,
  )
where

import qualified Network.AWS.DeviceFarm.Types.Arn as Types
import qualified Network.AWS.DeviceFarm.Types.Counters as Types
import qualified Network.AWS.DeviceFarm.Types.DeviceMinutes as Types
import qualified Network.AWS.DeviceFarm.Types.ExecutionResult as Types
import qualified Network.AWS.DeviceFarm.Types.ExecutionStatus as Types
import qualified Network.AWS.DeviceFarm.Types.Message as Types
import qualified Network.AWS.DeviceFarm.Types.Name as Types
import qualified Network.AWS.DeviceFarm.Types.TestType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a condition that is evaluated.
--
-- /See:/ 'mkTest' smart constructor.
data Test = Test'
  { -- | The test's ARN.
    arn :: Core.Maybe Types.Arn,
    -- | The test's result counters.
    counters :: Core.Maybe Types.Counters,
    -- | When the test was created.
    created :: Core.Maybe Core.NominalDiffTime,
    -- | Represents the total (metered or unmetered) minutes used by the test.
    deviceMinutes :: Core.Maybe Types.DeviceMinutes,
    -- | A message about the test's result.
    message :: Core.Maybe Types.Message,
    -- | The test's name.
    name :: Core.Maybe Types.Name,
    -- | The test's result.
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
    -- | The test's start time.
    started :: Core.Maybe Core.NominalDiffTime,
    -- | The test's status.
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
    -- | The test's stop time.
    stopped :: Core.Maybe Core.NominalDiffTime,
    -- | The test's type.
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

-- | Creates a 'Test' value with any optional fields omitted.
mkTest ::
  Test
mkTest =
  Test'
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

-- | The test's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tArn :: Lens.Lens' Test (Core.Maybe Types.Arn)
tArn = Lens.field @"arn"
{-# DEPRECATED tArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The test's result counters.
--
-- /Note:/ Consider using 'counters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tCounters :: Lens.Lens' Test (Core.Maybe Types.Counters)
tCounters = Lens.field @"counters"
{-# DEPRECATED tCounters "Use generic-lens or generic-optics with 'counters' instead." #-}

-- | When the test was created.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tCreated :: Lens.Lens' Test (Core.Maybe Core.NominalDiffTime)
tCreated = Lens.field @"created"
{-# DEPRECATED tCreated "Use generic-lens or generic-optics with 'created' instead." #-}

-- | Represents the total (metered or unmetered) minutes used by the test.
--
-- /Note:/ Consider using 'deviceMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tDeviceMinutes :: Lens.Lens' Test (Core.Maybe Types.DeviceMinutes)
tDeviceMinutes = Lens.field @"deviceMinutes"
{-# DEPRECATED tDeviceMinutes "Use generic-lens or generic-optics with 'deviceMinutes' instead." #-}

-- | A message about the test's result.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tMessage :: Lens.Lens' Test (Core.Maybe Types.Message)
tMessage = Lens.field @"message"
{-# DEPRECATED tMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The test's name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tName :: Lens.Lens' Test (Core.Maybe Types.Name)
tName = Lens.field @"name"
{-# DEPRECATED tName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The test's result.
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
tResult :: Lens.Lens' Test (Core.Maybe Types.ExecutionResult)
tResult = Lens.field @"result"
{-# DEPRECATED tResult "Use generic-lens or generic-optics with 'result' instead." #-}

-- | The test's start time.
--
-- /Note:/ Consider using 'started' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tStarted :: Lens.Lens' Test (Core.Maybe Core.NominalDiffTime)
tStarted = Lens.field @"started"
{-# DEPRECATED tStarted "Use generic-lens or generic-optics with 'started' instead." #-}

-- | The test's status.
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
tStatus :: Lens.Lens' Test (Core.Maybe Types.ExecutionStatus)
tStatus = Lens.field @"status"
{-# DEPRECATED tStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The test's stop time.
--
-- /Note:/ Consider using 'stopped' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tStopped :: Lens.Lens' Test (Core.Maybe Core.NominalDiffTime)
tStopped = Lens.field @"stopped"
{-# DEPRECATED tStopped "Use generic-lens or generic-optics with 'stopped' instead." #-}

-- | The test's type.
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
tType :: Lens.Lens' Test (Core.Maybe Types.TestType)
tType = Lens.field @"type'"
{-# DEPRECATED tType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON Test where
  parseJSON =
    Core.withObject "Test" Core.$
      \x ->
        Test'
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
