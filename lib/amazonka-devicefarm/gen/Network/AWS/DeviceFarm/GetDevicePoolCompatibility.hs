{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    gdpcTest,
    gdpcAppARN,
    gdpcConfiguration,
    gdpcTestType,
    gdpcDevicePoolARN,

    -- * Destructuring the response
    GetDevicePoolCompatibilityResponse (..),
    mkGetDevicePoolCompatibilityResponse,

    -- ** Response lenses
    gdpcrsIncompatibleDevices,
    gdpcrsCompatibleDevices,
    gdpcrsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents a request to the get device pool compatibility operation.
--
-- /See:/ 'mkGetDevicePoolCompatibility' smart constructor.
data GetDevicePoolCompatibility = GetDevicePoolCompatibility'
  { test ::
      Lude.Maybe ScheduleRunTest,
    appARN :: Lude.Maybe Lude.Text,
    configuration ::
      Lude.Maybe ScheduleRunConfiguration,
    testType :: Lude.Maybe TestType,
    devicePoolARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDevicePoolCompatibility' with the minimum fields required to make a request.
--
-- * 'appARN' - The ARN of the app that is associated with the specified device pool.
-- * 'configuration' - An object that contains information about the settings for a run.
-- * 'devicePoolARN' - The device pool's ARN.
-- * 'test' - Information about the uploaded test to be run against the device pool.
-- * 'testType' - The test type for the specified device pool.
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
mkGetDevicePoolCompatibility ::
  -- | 'devicePoolARN'
  Lude.Text ->
  GetDevicePoolCompatibility
mkGetDevicePoolCompatibility pDevicePoolARN_ =
  GetDevicePoolCompatibility'
    { test = Lude.Nothing,
      appARN = Lude.Nothing,
      configuration = Lude.Nothing,
      testType = Lude.Nothing,
      devicePoolARN = pDevicePoolARN_
    }

-- | Information about the uploaded test to be run against the device pool.
--
-- /Note:/ Consider using 'test' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpcTest :: Lens.Lens' GetDevicePoolCompatibility (Lude.Maybe ScheduleRunTest)
gdpcTest = Lens.lens (test :: GetDevicePoolCompatibility -> Lude.Maybe ScheduleRunTest) (\s a -> s {test = a} :: GetDevicePoolCompatibility)
{-# DEPRECATED gdpcTest "Use generic-lens or generic-optics with 'test' instead." #-}

-- | The ARN of the app that is associated with the specified device pool.
--
-- /Note:/ Consider using 'appARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpcAppARN :: Lens.Lens' GetDevicePoolCompatibility (Lude.Maybe Lude.Text)
gdpcAppARN = Lens.lens (appARN :: GetDevicePoolCompatibility -> Lude.Maybe Lude.Text) (\s a -> s {appARN = a} :: GetDevicePoolCompatibility)
{-# DEPRECATED gdpcAppARN "Use generic-lens or generic-optics with 'appARN' instead." #-}

-- | An object that contains information about the settings for a run.
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpcConfiguration :: Lens.Lens' GetDevicePoolCompatibility (Lude.Maybe ScheduleRunConfiguration)
gdpcConfiguration = Lens.lens (configuration :: GetDevicePoolCompatibility -> Lude.Maybe ScheduleRunConfiguration) (\s a -> s {configuration = a} :: GetDevicePoolCompatibility)
{-# DEPRECATED gdpcConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

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
gdpcTestType :: Lens.Lens' GetDevicePoolCompatibility (Lude.Maybe TestType)
gdpcTestType = Lens.lens (testType :: GetDevicePoolCompatibility -> Lude.Maybe TestType) (\s a -> s {testType = a} :: GetDevicePoolCompatibility)
{-# DEPRECATED gdpcTestType "Use generic-lens or generic-optics with 'testType' instead." #-}

-- | The device pool's ARN.
--
-- /Note:/ Consider using 'devicePoolARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpcDevicePoolARN :: Lens.Lens' GetDevicePoolCompatibility Lude.Text
gdpcDevicePoolARN = Lens.lens (devicePoolARN :: GetDevicePoolCompatibility -> Lude.Text) (\s a -> s {devicePoolARN = a} :: GetDevicePoolCompatibility)
{-# DEPRECATED gdpcDevicePoolARN "Use generic-lens or generic-optics with 'devicePoolARN' instead." #-}

instance Lude.AWSRequest GetDevicePoolCompatibility where
  type
    Rs GetDevicePoolCompatibility =
      GetDevicePoolCompatibilityResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDevicePoolCompatibilityResponse'
            Lude.<$> (x Lude..?> "incompatibleDevices" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "compatibleDevices" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDevicePoolCompatibility where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "DeviceFarm_20150623.GetDevicePoolCompatibility" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetDevicePoolCompatibility where
  toJSON GetDevicePoolCompatibility' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("test" Lude..=) Lude.<$> test,
            ("appArn" Lude..=) Lude.<$> appARN,
            ("configuration" Lude..=) Lude.<$> configuration,
            ("testType" Lude..=) Lude.<$> testType,
            Lude.Just ("devicePoolArn" Lude..= devicePoolARN)
          ]
      )

instance Lude.ToPath GetDevicePoolCompatibility where
  toPath = Lude.const "/"

instance Lude.ToQuery GetDevicePoolCompatibility where
  toQuery = Lude.const Lude.mempty

-- | Represents the result of describe device pool compatibility request.
--
-- /See:/ 'mkGetDevicePoolCompatibilityResponse' smart constructor.
data GetDevicePoolCompatibilityResponse = GetDevicePoolCompatibilityResponse'
  { incompatibleDevices ::
      Lude.Maybe
        [DevicePoolCompatibilityResult],
    compatibleDevices ::
      Lude.Maybe
        [DevicePoolCompatibilityResult],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDevicePoolCompatibilityResponse' with the minimum fields required to make a request.
--
-- * 'compatibleDevices' - Information about compatible devices.
-- * 'incompatibleDevices' - Information about incompatible devices.
-- * 'responseStatus' - The response status code.
mkGetDevicePoolCompatibilityResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDevicePoolCompatibilityResponse
mkGetDevicePoolCompatibilityResponse pResponseStatus_ =
  GetDevicePoolCompatibilityResponse'
    { incompatibleDevices =
        Lude.Nothing,
      compatibleDevices = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about incompatible devices.
--
-- /Note:/ Consider using 'incompatibleDevices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpcrsIncompatibleDevices :: Lens.Lens' GetDevicePoolCompatibilityResponse (Lude.Maybe [DevicePoolCompatibilityResult])
gdpcrsIncompatibleDevices = Lens.lens (incompatibleDevices :: GetDevicePoolCompatibilityResponse -> Lude.Maybe [DevicePoolCompatibilityResult]) (\s a -> s {incompatibleDevices = a} :: GetDevicePoolCompatibilityResponse)
{-# DEPRECATED gdpcrsIncompatibleDevices "Use generic-lens or generic-optics with 'incompatibleDevices' instead." #-}

-- | Information about compatible devices.
--
-- /Note:/ Consider using 'compatibleDevices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpcrsCompatibleDevices :: Lens.Lens' GetDevicePoolCompatibilityResponse (Lude.Maybe [DevicePoolCompatibilityResult])
gdpcrsCompatibleDevices = Lens.lens (compatibleDevices :: GetDevicePoolCompatibilityResponse -> Lude.Maybe [DevicePoolCompatibilityResult]) (\s a -> s {compatibleDevices = a} :: GetDevicePoolCompatibilityResponse)
{-# DEPRECATED gdpcrsCompatibleDevices "Use generic-lens or generic-optics with 'compatibleDevices' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpcrsResponseStatus :: Lens.Lens' GetDevicePoolCompatibilityResponse Lude.Int
gdpcrsResponseStatus = Lens.lens (responseStatus :: GetDevicePoolCompatibilityResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDevicePoolCompatibilityResponse)
{-# DEPRECATED gdpcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
