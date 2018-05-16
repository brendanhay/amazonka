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
-- Module      : Network.AWS.DeviceFarm.GetDeviceInstance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a device instance belonging to a private device fleet.
--
--
module Network.AWS.DeviceFarm.GetDeviceInstance
    (
    -- * Creating a Request
      getDeviceInstance
    , GetDeviceInstance
    -- * Request Lenses
    , gdiArn

    -- * Destructuring the Response
    , getDeviceInstanceResponse
    , GetDeviceInstanceResponse
    -- * Response Lenses
    , gdirsDeviceInstance
    , gdirsResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getDeviceInstance' smart constructor.
newtype GetDeviceInstance = GetDeviceInstance'
  { _gdiArn :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDeviceInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdiArn' - The Amazon Resource Name (ARN) of the instance you're requesting information about.
getDeviceInstance
    :: Text -- ^ 'gdiArn'
    -> GetDeviceInstance
getDeviceInstance pArn_ = GetDeviceInstance' {_gdiArn = pArn_}


-- | The Amazon Resource Name (ARN) of the instance you're requesting information about.
gdiArn :: Lens' GetDeviceInstance Text
gdiArn = lens _gdiArn (\ s a -> s{_gdiArn = a})

instance AWSRequest GetDeviceInstance where
        type Rs GetDeviceInstance = GetDeviceInstanceResponse
        request = postJSON deviceFarm
        response
          = receiveJSON
              (\ s h x ->
                 GetDeviceInstanceResponse' <$>
                   (x .?> "deviceInstance") <*> (pure (fromEnum s)))

instance Hashable GetDeviceInstance where

instance NFData GetDeviceInstance where

instance ToHeaders GetDeviceInstance where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.GetDeviceInstance" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetDeviceInstance where
        toJSON GetDeviceInstance'{..}
          = object (catMaybes [Just ("arn" .= _gdiArn)])

instance ToPath GetDeviceInstance where
        toPath = const "/"

instance ToQuery GetDeviceInstance where
        toQuery = const mempty

-- | /See:/ 'getDeviceInstanceResponse' smart constructor.
data GetDeviceInstanceResponse = GetDeviceInstanceResponse'
  { _gdirsDeviceInstance :: !(Maybe DeviceInstance)
  , _gdirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDeviceInstanceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdirsDeviceInstance' - An object containing information about your device instance.
--
-- * 'gdirsResponseStatus' - -- | The response status code.
getDeviceInstanceResponse
    :: Int -- ^ 'gdirsResponseStatus'
    -> GetDeviceInstanceResponse
getDeviceInstanceResponse pResponseStatus_ =
  GetDeviceInstanceResponse'
    {_gdirsDeviceInstance = Nothing, _gdirsResponseStatus = pResponseStatus_}


-- | An object containing information about your device instance.
gdirsDeviceInstance :: Lens' GetDeviceInstanceResponse (Maybe DeviceInstance)
gdirsDeviceInstance = lens _gdirsDeviceInstance (\ s a -> s{_gdirsDeviceInstance = a})

-- | -- | The response status code.
gdirsResponseStatus :: Lens' GetDeviceInstanceResponse Int
gdirsResponseStatus = lens _gdirsResponseStatus (\ s a -> s{_gdirsResponseStatus = a})

instance NFData GetDeviceInstanceResponse where
