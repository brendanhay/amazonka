{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.GetDevicePool
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a device pool.
--
-- <http://docs.aws.amazon.com/devicefarm/latest/APIReference/API_GetDevicePool.html>
module Network.AWS.DeviceFarm.GetDevicePool
    (
    -- * Request
      GetDevicePool
    -- ** Request constructor
    , getDevicePool
    -- ** Request lenses
    , gdpArn

    -- * Response
    , GetDevicePoolResponse
    -- ** Response constructor
    , getDevicePoolResponse
    -- ** Response lenses
    , gdprsDevicePool
    , gdprsStatus
    ) where

import           Network.AWS.DeviceFarm.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents a request to the get device pool operation.
--
-- /See:/ 'getDevicePool' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdpArn'
newtype GetDevicePool = GetDevicePool'
    { _gdpArn :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetDevicePool' smart constructor.
getDevicePool :: Text -> GetDevicePool
getDevicePool pArn_ =
    GetDevicePool'
    { _gdpArn = pArn_
    }

-- | The device pool\'s ARN.
gdpArn :: Lens' GetDevicePool Text
gdpArn = lens _gdpArn (\ s a -> s{_gdpArn = a});

instance AWSRequest GetDevicePool where
        type Sv GetDevicePool = DeviceFarm
        type Rs GetDevicePool = GetDevicePoolResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GetDevicePoolResponse' <$>
                   (x .?> "devicePool") <*> (pure (fromEnum s)))

instance ToHeaders GetDevicePool where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.GetDevicePool" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetDevicePool where
        toJSON GetDevicePool'{..} = object ["arn" .= _gdpArn]

instance ToPath GetDevicePool where
        toPath = const "/"

instance ToQuery GetDevicePool where
        toQuery = const mempty

-- | Represents the result of a get device pool request.
--
-- /See:/ 'getDevicePoolResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdprsDevicePool'
--
-- * 'gdprsStatus'
data GetDevicePoolResponse = GetDevicePoolResponse'
    { _gdprsDevicePool :: !(Maybe DevicePool)
    , _gdprsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetDevicePoolResponse' smart constructor.
getDevicePoolResponse :: Int -> GetDevicePoolResponse
getDevicePoolResponse pStatus_ =
    GetDevicePoolResponse'
    { _gdprsDevicePool = Nothing
    , _gdprsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
gdprsDevicePool :: Lens' GetDevicePoolResponse (Maybe DevicePool)
gdprsDevicePool = lens _gdprsDevicePool (\ s a -> s{_gdprsDevicePool = a});

-- | FIXME: Undocumented member.
gdprsStatus :: Lens' GetDevicePoolResponse Int
gdprsStatus = lens _gdprsStatus (\ s a -> s{_gdprsStatus = a});
