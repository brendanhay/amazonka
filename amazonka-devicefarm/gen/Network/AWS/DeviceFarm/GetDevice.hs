{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.GetDevice
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a unique device type.
--
-- <http://docs.aws.amazon.com/devicefarm/latest/APIReference/API_GetDevice.html>
module Network.AWS.DeviceFarm.GetDevice
    (
    -- * Request
      GetDevice
    -- ** Request constructor
    , getDevice
    -- ** Request lenses
    , gdArn

    -- * Response
    , GetDeviceResponse
    -- ** Response constructor
    , getDeviceResponse
    -- ** Response lenses
    , gdrsDevice
    , gdrsStatus
    ) where

import           Network.AWS.DeviceFarm.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents a request to the get device request.
--
-- /See:/ 'getDevice' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdArn'
newtype GetDevice = GetDevice'
    { _gdArn :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetDevice' smart constructor.
getDevice :: Text -> GetDevice
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
        request = postJSON "GetDevice"
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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdrsDevice'
--
-- * 'gdrsStatus'
data GetDeviceResponse = GetDeviceResponse'
    { _gdrsDevice :: !(Maybe Device)
    , _gdrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetDeviceResponse' smart constructor.
getDeviceResponse :: Int -> GetDeviceResponse
getDeviceResponse pStatus_ =
    GetDeviceResponse'
    { _gdrsDevice = Nothing
    , _gdrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
gdrsDevice :: Lens' GetDeviceResponse (Maybe Device)
gdrsDevice = lens _gdrsDevice (\ s a -> s{_gdrsDevice = a});

-- | FIXME: Undocumented member.
gdrsStatus :: Lens' GetDeviceResponse Int
gdrsStatus = lens _gdrsStatus (\ s a -> s{_gdrsStatus = a});
