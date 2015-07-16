{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.GetDevicePoolCompatibility
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets information about compatibility with a device pool.
--
-- <http://docs.aws.amazon.com/devicefarm/latest/APIReference/API_GetDevicePoolCompatibility.html>
module Network.AWS.DeviceFarm.GetDevicePoolCompatibility
    (
    -- * Request
      GetDevicePoolCompatibility
    -- ** Request constructor
    , getDevicePoolCompatibility
    -- ** Request lenses
    , gdpcTestType
    , gdpcDevicePoolARN
    , gdpcAppARN

    -- * Response
    , GetDevicePoolCompatibilityResponse
    -- ** Response constructor
    , getDevicePoolCompatibilityResponse
    -- ** Response lenses
    , gdpcrIncompatibleDevices
    , gdpcrCompatibleDevices
    , gdpcrStatus
    ) where

import           Network.AWS.DeviceFarm.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents a request to the get device pool compatibility operation.
--
-- /See:/ 'getDevicePoolCompatibility' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdpcTestType'
--
-- * 'gdpcDevicePoolARN'
--
-- * 'gdpcAppARN'
data GetDevicePoolCompatibility = GetDevicePoolCompatibility'
    { _gdpcTestType      :: !(Maybe TestType)
    , _gdpcDevicePoolARN :: !Text
    , _gdpcAppARN        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetDevicePoolCompatibility' smart constructor.
getDevicePoolCompatibility :: Text -> Text -> GetDevicePoolCompatibility
getDevicePoolCompatibility pDevicePoolARN pAppARN =
    GetDevicePoolCompatibility'
    { _gdpcTestType = Nothing
    , _gdpcDevicePoolARN = pDevicePoolARN
    , _gdpcAppARN = pAppARN
    }

-- | The test type for the specified device pool.
--
-- Allowed values include the following:
--
-- -   APPIUM_JAVA_JUNIT: The Appium Java JUnit type.
--
-- -   APPIUM_JAVA_TESTNG: The Appium Java TestNG type.
--
-- -   BUILTIN_EXPLORER: An app explorer that will traverse an app,
--     interacting with it and capturing screenshots at the same time.
--
-- -   BUILTIN_FUZZ: The built-in fuzz type.
--
-- -   CALABASH: The Calabash type.
--
-- -   INSTRUMENTATION: The Instrumentation type.
--
-- -   UIAUTOMATOR: The uiautomator type.
--
gdpcTestType :: Lens' GetDevicePoolCompatibility (Maybe TestType)
gdpcTestType = lens _gdpcTestType (\ s a -> s{_gdpcTestType = a});

-- | The device pool\'s ARN.
gdpcDevicePoolARN :: Lens' GetDevicePoolCompatibility Text
gdpcDevicePoolARN = lens _gdpcDevicePoolARN (\ s a -> s{_gdpcDevicePoolARN = a});

-- | The ARN of the app that is associated with the specified device pool.
gdpcAppARN :: Lens' GetDevicePoolCompatibility Text
gdpcAppARN = lens _gdpcAppARN (\ s a -> s{_gdpcAppARN = a});

instance AWSRequest GetDevicePoolCompatibility where
        type Sv GetDevicePoolCompatibility = DeviceFarm
        type Rs GetDevicePoolCompatibility =
             GetDevicePoolCompatibilityResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GetDevicePoolCompatibilityResponse' <$>
                   (x .?> "incompatibleDevices" .!@ mempty) <*>
                     (x .?> "compatibleDevices" .!@ mempty)
                     <*> (pure (fromEnum s)))

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
              ["testType" .= _gdpcTestType,
               "devicePoolArn" .= _gdpcDevicePoolARN,
               "appArn" .= _gdpcAppARN]

instance ToPath GetDevicePoolCompatibility where
        toPath = const "/"

instance ToQuery GetDevicePoolCompatibility where
        toQuery = const mempty

-- | Represents the result of describe device pool compatibility request.
--
-- /See:/ 'getDevicePoolCompatibilityResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdpcrIncompatibleDevices'
--
-- * 'gdpcrCompatibleDevices'
--
-- * 'gdpcrStatus'
data GetDevicePoolCompatibilityResponse = GetDevicePoolCompatibilityResponse'
    { _gdpcrIncompatibleDevices :: !(Maybe [DevicePoolCompatibilityResult])
    , _gdpcrCompatibleDevices   :: !(Maybe [DevicePoolCompatibilityResult])
    , _gdpcrStatus              :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetDevicePoolCompatibilityResponse' smart constructor.
getDevicePoolCompatibilityResponse :: Int -> GetDevicePoolCompatibilityResponse
getDevicePoolCompatibilityResponse pStatus =
    GetDevicePoolCompatibilityResponse'
    { _gdpcrIncompatibleDevices = Nothing
    , _gdpcrCompatibleDevices = Nothing
    , _gdpcrStatus = pStatus
    }

-- | Information about incompatible devices.
gdpcrIncompatibleDevices :: Lens' GetDevicePoolCompatibilityResponse [DevicePoolCompatibilityResult]
gdpcrIncompatibleDevices = lens _gdpcrIncompatibleDevices (\ s a -> s{_gdpcrIncompatibleDevices = a}) . _Default;

-- | Information about compatible devices.
gdpcrCompatibleDevices :: Lens' GetDevicePoolCompatibilityResponse [DevicePoolCompatibilityResult]
gdpcrCompatibleDevices = lens _gdpcrCompatibleDevices (\ s a -> s{_gdpcrCompatibleDevices = a}) . _Default;

-- | FIXME: Undocumented member.
gdpcrStatus :: Lens' GetDevicePoolCompatibilityResponse Int
gdpcrStatus = lens _gdpcrStatus (\ s a -> s{_gdpcrStatus = a});
