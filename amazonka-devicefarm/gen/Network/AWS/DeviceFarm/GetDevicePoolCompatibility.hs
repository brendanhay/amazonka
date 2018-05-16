{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.GetDevicePoolCompatibility
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about compatibility with a device pool.
--
--
module Network.AWS.DeviceFarm.GetDevicePoolCompatibility
    (
    -- * Creating a Request
      getDevicePoolCompatibility
    , GetDevicePoolCompatibility
    -- * Request Lenses
    , gdpcTest
    , gdpcAppARN
    , gdpcConfiguration
    , gdpcTestType
    , gdpcDevicePoolARN

    -- * Destructuring the Response
    , getDevicePoolCompatibilityResponse
    , GetDevicePoolCompatibilityResponse
    -- * Response Lenses
    , gdpcrsIncompatibleDevices
    , gdpcrsCompatibleDevices
    , gdpcrsResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents a request to the get device pool compatibility operation.
--
--
--
-- /See:/ 'getDevicePoolCompatibility' smart constructor.
data GetDevicePoolCompatibility = GetDevicePoolCompatibility'
  { _gdpcTest          :: !(Maybe ScheduleRunTest)
  , _gdpcAppARN        :: !(Maybe Text)
  , _gdpcConfiguration :: !(Maybe ScheduleRunConfiguration)
  , _gdpcTestType      :: !(Maybe TestType)
  , _gdpcDevicePoolARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDevicePoolCompatibility' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdpcTest' - Information about the uploaded test to be run against the device pool.
--
-- * 'gdpcAppARN' - The ARN of the app that is associated with the specified device pool.
--
-- * 'gdpcConfiguration' - An object containing information about the settings for a run.
--
-- * 'gdpcTestType' - The test type for the specified device pool. Allowed values include the following:     * BUILTIN_FUZZ: The built-in fuzz type.     * BUILTIN_EXPLORER: For Android, an app explorer that will traverse an Android app, interacting with it and capturing screenshots at the same time.     * APPIUM_JAVA_JUNIT: The Appium Java JUnit type.     * APPIUM_JAVA_TESTNG: The Appium Java TestNG type.     * APPIUM_PYTHON: The Appium Python type.     * APPIUM_WEB_JAVA_JUNIT: The Appium Java JUnit type for Web apps.     * APPIUM_WEB_JAVA_TESTNG: The Appium Java TestNG type for Web apps.     * APPIUM_WEB_PYTHON: The Appium Python type for Web apps.     * CALABASH: The Calabash type.     * INSTRUMENTATION: The Instrumentation type.     * UIAUTOMATION: The uiautomation type.     * UIAUTOMATOR: The uiautomator type.     * XCTEST: The XCode test type.     * XCTEST_UI: The XCode UI test type.
--
-- * 'gdpcDevicePoolARN' - The device pool's ARN.
getDevicePoolCompatibility
    :: Text -- ^ 'gdpcDevicePoolARN'
    -> GetDevicePoolCompatibility
getDevicePoolCompatibility pDevicePoolARN_ =
  GetDevicePoolCompatibility'
    { _gdpcTest = Nothing
    , _gdpcAppARN = Nothing
    , _gdpcConfiguration = Nothing
    , _gdpcTestType = Nothing
    , _gdpcDevicePoolARN = pDevicePoolARN_
    }


-- | Information about the uploaded test to be run against the device pool.
gdpcTest :: Lens' GetDevicePoolCompatibility (Maybe ScheduleRunTest)
gdpcTest = lens _gdpcTest (\ s a -> s{_gdpcTest = a})

-- | The ARN of the app that is associated with the specified device pool.
gdpcAppARN :: Lens' GetDevicePoolCompatibility (Maybe Text)
gdpcAppARN = lens _gdpcAppARN (\ s a -> s{_gdpcAppARN = a})

-- | An object containing information about the settings for a run.
gdpcConfiguration :: Lens' GetDevicePoolCompatibility (Maybe ScheduleRunConfiguration)
gdpcConfiguration = lens _gdpcConfiguration (\ s a -> s{_gdpcConfiguration = a})

-- | The test type for the specified device pool. Allowed values include the following:     * BUILTIN_FUZZ: The built-in fuzz type.     * BUILTIN_EXPLORER: For Android, an app explorer that will traverse an Android app, interacting with it and capturing screenshots at the same time.     * APPIUM_JAVA_JUNIT: The Appium Java JUnit type.     * APPIUM_JAVA_TESTNG: The Appium Java TestNG type.     * APPIUM_PYTHON: The Appium Python type.     * APPIUM_WEB_JAVA_JUNIT: The Appium Java JUnit type for Web apps.     * APPIUM_WEB_JAVA_TESTNG: The Appium Java TestNG type for Web apps.     * APPIUM_WEB_PYTHON: The Appium Python type for Web apps.     * CALABASH: The Calabash type.     * INSTRUMENTATION: The Instrumentation type.     * UIAUTOMATION: The uiautomation type.     * UIAUTOMATOR: The uiautomator type.     * XCTEST: The XCode test type.     * XCTEST_UI: The XCode UI test type.
gdpcTestType :: Lens' GetDevicePoolCompatibility (Maybe TestType)
gdpcTestType = lens _gdpcTestType (\ s a -> s{_gdpcTestType = a})

-- | The device pool's ARN.
gdpcDevicePoolARN :: Lens' GetDevicePoolCompatibility Text
gdpcDevicePoolARN = lens _gdpcDevicePoolARN (\ s a -> s{_gdpcDevicePoolARN = a})

instance AWSRequest GetDevicePoolCompatibility where
        type Rs GetDevicePoolCompatibility =
             GetDevicePoolCompatibilityResponse
        request = postJSON deviceFarm
        response
          = receiveJSON
              (\ s h x ->
                 GetDevicePoolCompatibilityResponse' <$>
                   (x .?> "incompatibleDevices" .!@ mempty) <*>
                     (x .?> "compatibleDevices" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetDevicePoolCompatibility where

instance NFData GetDevicePoolCompatibility where

instance ToHeaders GetDevicePoolCompatibility where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.GetDevicePoolCompatibility" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetDevicePoolCompatibility where
        toJSON GetDevicePoolCompatibility'{..}
          = object
              (catMaybes
                 [("test" .=) <$> _gdpcTest,
                  ("appArn" .=) <$> _gdpcAppARN,
                  ("configuration" .=) <$> _gdpcConfiguration,
                  ("testType" .=) <$> _gdpcTestType,
                  Just ("devicePoolArn" .= _gdpcDevicePoolARN)])

instance ToPath GetDevicePoolCompatibility where
        toPath = const "/"

instance ToQuery GetDevicePoolCompatibility where
        toQuery = const mempty

-- | Represents the result of describe device pool compatibility request.
--
--
--
-- /See:/ 'getDevicePoolCompatibilityResponse' smart constructor.
data GetDevicePoolCompatibilityResponse = GetDevicePoolCompatibilityResponse'
  { _gdpcrsIncompatibleDevices :: !(Maybe [DevicePoolCompatibilityResult])
  , _gdpcrsCompatibleDevices   :: !(Maybe [DevicePoolCompatibilityResult])
  , _gdpcrsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDevicePoolCompatibilityResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdpcrsIncompatibleDevices' - Information about incompatible devices.
--
-- * 'gdpcrsCompatibleDevices' - Information about compatible devices.
--
-- * 'gdpcrsResponseStatus' - -- | The response status code.
getDevicePoolCompatibilityResponse
    :: Int -- ^ 'gdpcrsResponseStatus'
    -> GetDevicePoolCompatibilityResponse
getDevicePoolCompatibilityResponse pResponseStatus_ =
  GetDevicePoolCompatibilityResponse'
    { _gdpcrsIncompatibleDevices = Nothing
    , _gdpcrsCompatibleDevices = Nothing
    , _gdpcrsResponseStatus = pResponseStatus_
    }


-- | Information about incompatible devices.
gdpcrsIncompatibleDevices :: Lens' GetDevicePoolCompatibilityResponse [DevicePoolCompatibilityResult]
gdpcrsIncompatibleDevices = lens _gdpcrsIncompatibleDevices (\ s a -> s{_gdpcrsIncompatibleDevices = a}) . _Default . _Coerce

-- | Information about compatible devices.
gdpcrsCompatibleDevices :: Lens' GetDevicePoolCompatibilityResponse [DevicePoolCompatibilityResult]
gdpcrsCompatibleDevices = lens _gdpcrsCompatibleDevices (\ s a -> s{_gdpcrsCompatibleDevices = a}) . _Default . _Coerce

-- | -- | The response status code.
gdpcrsResponseStatus :: Lens' GetDevicePoolCompatibilityResponse Int
gdpcrsResponseStatus = lens _gdpcrsResponseStatus (\ s a -> s{_gdpcrsResponseStatus = a})

instance NFData GetDevicePoolCompatibilityResponse
         where
