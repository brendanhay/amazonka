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
-- Module      : Network.AWS.DeviceFarm.GetDevice
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a unique device type.
--
-- /See:/ <http://docs.aws.amazon.com/devicefarm/latest/APIReference/API_GetDevice.html AWS API Reference> for GetDevice.
module Network.AWS.DeviceFarm.GetDevice
    (
    -- * Creating a Request
      getDevice
    , GetDevice
    -- * Request Lenses
    , gdArn

    -- * Destructuring the Response
    , getDeviceResponse
    , GetDeviceResponse
    -- * Response Lenses
    , gdrsDevice
    , gdrsStatus
    ) where

import           Network.AWS.DeviceFarm.Types
import           Network.AWS.DeviceFarm.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents a request to the get device request.
--
-- /See:/ 'getDevice' smart constructor.
newtype GetDevice = GetDevice'
    { _gdArn :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetDevice' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdArn'
getDevice
    :: Text -- ^ 'gdArn'
    -> GetDevice
getDevice pArn_ =
    GetDevice'
    { _gdArn = pArn_
    }

-- | The device type\'s ARN.
gdArn :: Lens' GetDevice Text
gdArn = lens _gdArn (\ s a -> s{_gdArn = a});

instance AWSRequest GetDevice where
        type Sv GetDevice = DeviceFarm
        type Rs GetDevice = GetDeviceResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GetDeviceResponse' <$>
                   (x .?> "device") <*> (pure (fromEnum s)))

instance ToHeaders GetDevice where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.GetDevice" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetDevice where
        toJSON GetDevice'{..} = object ["arn" .= _gdArn]

instance ToPath GetDevice where
        toPath = const "/"

instance ToQuery GetDevice where
        toQuery = const mempty

-- | Represents the result of a get device request.
--
-- /See:/ 'getDeviceResponse' smart constructor.
data GetDeviceResponse = GetDeviceResponse'
    { _gdrsDevice :: !(Maybe Device)
    , _gdrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetDeviceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdrsDevice'
--
-- * 'gdrsStatus'
getDeviceResponse
    :: Int -- ^ 'gdrsStatus'
    -> GetDeviceResponse
getDeviceResponse pStatus_ =
    GetDeviceResponse'
    { _gdrsDevice = Nothing
    , _gdrsStatus = pStatus_
    }

-- | Undocumented member.
gdrsDevice :: Lens' GetDeviceResponse (Maybe Device)
gdrsDevice = lens _gdrsDevice (\ s a -> s{_gdrsDevice = a});

-- | The response status code.
gdrsStatus :: Lens' GetDeviceResponse Int
gdrsStatus = lens _gdrsStatus (\ s a -> s{_gdrsStatus = a});
