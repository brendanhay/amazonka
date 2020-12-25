{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.GetDevicePoolCompatibility
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about compatibility with a device pool.
module Network.AWS.DeviceFarm.GetDevicePoolCompatibility
  ( -- * Creating a request
    GetDevicePoolCompatibility (..),
    mkGetDevicePoolCompatibility,

    -- ** Request lenses
    gdpcDevicePoolArn,
    gdpcAppArn,
    gdpcConfiguration,
    gdpcTest,
    gdpcTestType,

    -- * Destructuring the response
    GetDevicePoolCompatibilityResponse (..),
    mkGetDevicePoolCompatibilityResponse,

    -- ** Response lenses
    gdpcrrsCompatibleDevices,
    gdpcrrsIncompatibleDevices,
    gdpcrrsResponseStatus,
  )
where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the get device pool compatibility operation.
--
-- /See:/ 'mkGetDevicePoolCompatibility' smart constructor.
data GetDevicePoolCompatibility = GetDevicePoolCompatibility'
  { -- | The device pool's ARN.
    devicePoolArn :: Types.DevicePoolArn,
    -- | The ARN of the app that is associated with the specified device pool.
    appArn :: Core.Maybe Types.AppArn,
    -- | An object that contains information about the settings for a run.
    configuration :: Core.Maybe Types.ScheduleRunConfiguration,
    -- | Information about the uploaded test to be run against the device pool.
    test :: Core.Maybe Types.ScheduleRunTest,
    -- | The test type for the specified device pool.
    --
    -- Allowed values include the following:
    --
    --     * BUILTIN_FUZZ.
    --
    --
    --     * BUILTIN_EXPLORER. For Android, an app explorer that traverses an Android app, interacting with it and capturing screenshots at the same time.
    --
    --
    --     * APPIUM_JAVA_JUNIT.
    --
    --
    --     * APPIUM_JAVA_TESTNG.
    --
    --
    --     * APPIUM_PYTHON.
    --
    --
    --     * APPIUM_NODE.
    --
    --
    --     * APPIUM_RUBY.
    --
    --
    --     * APPIUM_WEB_JAVA_JUNIT.
    --
    --
    --     * APPIUM_WEB_JAVA_TESTNG.
    --
    --
    --     * APPIUM_WEB_PYTHON.
    --
    --
    --     * APPIUM_WEB_NODE.
    --
    --
    --     * APPIUM_WEB_RUBY.
    --
    --
    --     * CALABASH.
    --
    --
    --     * INSTRUMENTATION.
    --
    --
    --     * UIAUTOMATION.
    --
    --
    --     * UIAUTOMATOR.
    --
    --
    --     * XCTEST.
    --
    --
    --     * XCTEST_UI.
    testType :: Core.Maybe Types.TestType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDevicePoolCompatibility' value with any optional fields omitted.
mkGetDevicePoolCompatibility ::
  -- | 'devicePoolArn'
  Types.DevicePoolArn ->
  GetDevicePoolCompatibility
mkGetDevicePoolCompatibility devicePoolArn =
  GetDevicePoolCompatibility'
    { devicePoolArn,
      appArn = Core.Nothing,
      configuration = Core.Nothing,
      test = Core.Nothing,
      testType = Core.Nothing
    }

-- | The device pool's ARN.
--
-- /Note:/ Consider using 'devicePoolArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpcDevicePoolArn :: Lens.Lens' GetDevicePoolCompatibility Types.DevicePoolArn
gdpcDevicePoolArn = Lens.field @"devicePoolArn"
{-# DEPRECATED gdpcDevicePoolArn "Use generic-lens or generic-optics with 'devicePoolArn' instead." #-}

-- | The ARN of the app that is associated with the specified device pool.
--
-- /Note:/ Consider using 'appArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpcAppArn :: Lens.Lens' GetDevicePoolCompatibility (Core.Maybe Types.AppArn)
gdpcAppArn = Lens.field @"appArn"
{-# DEPRECATED gdpcAppArn "Use generic-lens or generic-optics with 'appArn' instead." #-}

-- | An object that contains information about the settings for a run.
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpcConfiguration :: Lens.Lens' GetDevicePoolCompatibility (Core.Maybe Types.ScheduleRunConfiguration)
gdpcConfiguration = Lens.field @"configuration"
{-# DEPRECATED gdpcConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

-- | Information about the uploaded test to be run against the device pool.
--
-- /Note:/ Consider using 'test' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpcTest :: Lens.Lens' GetDevicePoolCompatibility (Core.Maybe Types.ScheduleRunTest)
gdpcTest = Lens.field @"test"
{-# DEPRECATED gdpcTest "Use generic-lens or generic-optics with 'test' instead." #-}

-- | The test type for the specified device pool.
--
-- Allowed values include the following:
--
--     * BUILTIN_FUZZ.
--
--
--     * BUILTIN_EXPLORER. For Android, an app explorer that traverses an Android app, interacting with it and capturing screenshots at the same time.
--
--
--     * APPIUM_JAVA_JUNIT.
--
--
--     * APPIUM_JAVA_TESTNG.
--
--
--     * APPIUM_PYTHON.
--
--
--     * APPIUM_NODE.
--
--
--     * APPIUM_RUBY.
--
--
--     * APPIUM_WEB_JAVA_JUNIT.
--
--
--     * APPIUM_WEB_JAVA_TESTNG.
--
--
--     * APPIUM_WEB_PYTHON.
--
--
--     * APPIUM_WEB_NODE.
--
--
--     * APPIUM_WEB_RUBY.
--
--
--     * CALABASH.
--
--
--     * INSTRUMENTATION.
--
--
--     * UIAUTOMATION.
--
--
--     * UIAUTOMATOR.
--
--
--     * XCTEST.
--
--
--     * XCTEST_UI.
--
--
--
-- /Note:/ Consider using 'testType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpcTestType :: Lens.Lens' GetDevicePoolCompatibility (Core.Maybe Types.TestType)
gdpcTestType = Lens.field @"testType"
{-# DEPRECATED gdpcTestType "Use generic-lens or generic-optics with 'testType' instead." #-}

instance Core.FromJSON GetDevicePoolCompatibility where
  toJSON GetDevicePoolCompatibility {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("devicePoolArn" Core..= devicePoolArn),
            ("appArn" Core..=) Core.<$> appArn,
            ("configuration" Core..=) Core.<$> configuration,
            ("test" Core..=) Core.<$> test,
            ("testType" Core..=) Core.<$> testType
          ]
      )

instance Core.AWSRequest GetDevicePoolCompatibility where
  type
    Rs GetDevicePoolCompatibility =
      GetDevicePoolCompatibilityResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DeviceFarm_20150623.GetDevicePoolCompatibility")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDevicePoolCompatibilityResponse'
            Core.<$> (x Core..:? "compatibleDevices")
            Core.<*> (x Core..:? "incompatibleDevices")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the result of describe device pool compatibility request.
--
-- /See:/ 'mkGetDevicePoolCompatibilityResponse' smart constructor.
data GetDevicePoolCompatibilityResponse = GetDevicePoolCompatibilityResponse'
  { -- | Information about compatible devices.
    compatibleDevices :: Core.Maybe [Types.DevicePoolCompatibilityResult],
    -- | Information about incompatible devices.
    incompatibleDevices :: Core.Maybe [Types.DevicePoolCompatibilityResult],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDevicePoolCompatibilityResponse' value with any optional fields omitted.
mkGetDevicePoolCompatibilityResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetDevicePoolCompatibilityResponse
mkGetDevicePoolCompatibilityResponse responseStatus =
  GetDevicePoolCompatibilityResponse'
    { compatibleDevices =
        Core.Nothing,
      incompatibleDevices = Core.Nothing,
      responseStatus
    }

-- | Information about compatible devices.
--
-- /Note:/ Consider using 'compatibleDevices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpcrrsCompatibleDevices :: Lens.Lens' GetDevicePoolCompatibilityResponse (Core.Maybe [Types.DevicePoolCompatibilityResult])
gdpcrrsCompatibleDevices = Lens.field @"compatibleDevices"
{-# DEPRECATED gdpcrrsCompatibleDevices "Use generic-lens or generic-optics with 'compatibleDevices' instead." #-}

-- | Information about incompatible devices.
--
-- /Note:/ Consider using 'incompatibleDevices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpcrrsIncompatibleDevices :: Lens.Lens' GetDevicePoolCompatibilityResponse (Core.Maybe [Types.DevicePoolCompatibilityResult])
gdpcrrsIncompatibleDevices = Lens.field @"incompatibleDevices"
{-# DEPRECATED gdpcrrsIncompatibleDevices "Use generic-lens or generic-optics with 'incompatibleDevices' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpcrrsResponseStatus :: Lens.Lens' GetDevicePoolCompatibilityResponse Core.Int
gdpcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gdpcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
