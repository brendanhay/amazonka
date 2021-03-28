{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Test
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.Test
  ( Test (..)
  -- * Smart constructor
  , mkTest
  -- * Lenses
  , tArn
  , tCounters
  , tCreated
  , tDeviceMinutes
  , tMessage
  , tName
  , tResult
  , tStarted
  , tStatus
  , tStopped
  , tType
  ) where

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
  { arn :: Core.Maybe Types.Arn
    -- ^ The test's ARN.
  , counters :: Core.Maybe Types.Counters
    -- ^ The test's result counters.
  , created :: Core.Maybe Core.NominalDiffTime
    -- ^ When the test was created.
  , deviceMinutes :: Core.Maybe Types.DeviceMinutes
    -- ^ Represents the total (metered or unmetered) minutes used by the test.
  , message :: Core.Maybe Types.Message
    -- ^ A message about the test's result.
  , name :: Core.Maybe Types.Name
    -- ^ The test's name.
  , result :: Core.Maybe Types.ExecutionResult
    -- ^ The test's result.
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
    -- ^ The test's start time.
  , status :: Core.Maybe Types.ExecutionStatus
    -- ^ The test's status.
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
    -- ^ The test's stop time.
  , type' :: Core.Maybe Types.TestType
    -- ^ The test's type.
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Test' value with any optional fields omitted.
mkTest
    :: Test
mkTest
  = Test'{arn = Core.Nothing, counters = Core.Nothing,
          created = Core.Nothing, deviceMinutes = Core.Nothing,
          message = Core.Nothing, name = Core.Nothing, result = Core.Nothing,
          started = Core.Nothing, status = Core.Nothing,
          stopped = Core.Nothing, type' = Core.Nothing}

-- | The test's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tArn :: Lens.Lens' Test (Core.Maybe Types.Arn)
tArn = Lens.field @"arn"
{-# INLINEABLE tArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The test's result counters.
--
-- /Note:/ Consider using 'counters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tCounters :: Lens.Lens' Test (Core.Maybe Types.Counters)
tCounters = Lens.field @"counters"
{-# INLINEABLE tCounters #-}
{-# DEPRECATED counters "Use generic-lens or generic-optics with 'counters' instead"  #-}

-- | When the test was created.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tCreated :: Lens.Lens' Test (Core.Maybe Core.NominalDiffTime)
tCreated = Lens.field @"created"
{-# INLINEABLE tCreated #-}
{-# DEPRECATED created "Use generic-lens or generic-optics with 'created' instead"  #-}

-- | Represents the total (metered or unmetered) minutes used by the test.
--
-- /Note:/ Consider using 'deviceMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tDeviceMinutes :: Lens.Lens' Test (Core.Maybe Types.DeviceMinutes)
tDeviceMinutes = Lens.field @"deviceMinutes"
{-# INLINEABLE tDeviceMinutes #-}
{-# DEPRECATED deviceMinutes "Use generic-lens or generic-optics with 'deviceMinutes' instead"  #-}

-- | A message about the test's result.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tMessage :: Lens.Lens' Test (Core.Maybe Types.Message)
tMessage = Lens.field @"message"
{-# INLINEABLE tMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

-- | The test's name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tName :: Lens.Lens' Test (Core.Maybe Types.Name)
tName = Lens.field @"name"
{-# INLINEABLE tName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

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
{-# INLINEABLE tResult #-}
{-# DEPRECATED result "Use generic-lens or generic-optics with 'result' instead"  #-}

-- | The test's start time.
--
-- /Note:/ Consider using 'started' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tStarted :: Lens.Lens' Test (Core.Maybe Core.NominalDiffTime)
tStarted = Lens.field @"started"
{-# INLINEABLE tStarted #-}
{-# DEPRECATED started "Use generic-lens or generic-optics with 'started' instead"  #-}

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
{-# INLINEABLE tStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The test's stop time.
--
-- /Note:/ Consider using 'stopped' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tStopped :: Lens.Lens' Test (Core.Maybe Core.NominalDiffTime)
tStopped = Lens.field @"stopped"
{-# INLINEABLE tStopped #-}
{-# DEPRECATED stopped "Use generic-lens or generic-optics with 'stopped' instead"  #-}

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
{-# INLINEABLE tType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON Test where
        parseJSON
          = Core.withObject "Test" Core.$
              \ x ->
                Test' Core.<$>
                  (x Core..:? "arn") Core.<*> x Core..:? "counters" Core.<*>
                    x Core..:? "created"
                    Core.<*> x Core..:? "deviceMinutes"
                    Core.<*> x Core..:? "message"
                    Core.<*> x Core..:? "name"
                    Core.<*> x Core..:? "result"
                    Core.<*> x Core..:? "started"
                    Core.<*> x Core..:? "status"
                    Core.<*> x Core..:? "stopped"
                    Core.<*> x Core..:? "type"
