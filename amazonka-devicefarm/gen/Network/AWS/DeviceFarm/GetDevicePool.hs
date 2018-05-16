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
-- Module      : Network.AWS.DeviceFarm.GetDevicePool
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a device pool.
--
--
module Network.AWS.DeviceFarm.GetDevicePool
    (
    -- * Creating a Request
      getDevicePool
    , GetDevicePool
    -- * Request Lenses
    , gdpArn

    -- * Destructuring the Response
    , getDevicePoolResponse
    , GetDevicePoolResponse
    -- * Response Lenses
    , gdprsDevicePool
    , gdprsResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents a request to the get device pool operation.
--
--
--
-- /See:/ 'getDevicePool' smart constructor.
newtype GetDevicePool = GetDevicePool'
  { _gdpArn :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDevicePool' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdpArn' - The device pool's ARN.
getDevicePool
    :: Text -- ^ 'gdpArn'
    -> GetDevicePool
getDevicePool pArn_ = GetDevicePool' {_gdpArn = pArn_}


-- | The device pool's ARN.
gdpArn :: Lens' GetDevicePool Text
gdpArn = lens _gdpArn (\ s a -> s{_gdpArn = a})

instance AWSRequest GetDevicePool where
        type Rs GetDevicePool = GetDevicePoolResponse
        request = postJSON deviceFarm
        response
          = receiveJSON
              (\ s h x ->
                 GetDevicePoolResponse' <$>
                   (x .?> "devicePool") <*> (pure (fromEnum s)))

instance Hashable GetDevicePool where

instance NFData GetDevicePool where

instance ToHeaders GetDevicePool where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.GetDevicePool" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetDevicePool where
        toJSON GetDevicePool'{..}
          = object (catMaybes [Just ("arn" .= _gdpArn)])

instance ToPath GetDevicePool where
        toPath = const "/"

instance ToQuery GetDevicePool where
        toQuery = const mempty

-- | Represents the result of a get device pool request.
--
--
--
-- /See:/ 'getDevicePoolResponse' smart constructor.
data GetDevicePoolResponse = GetDevicePoolResponse'
  { _gdprsDevicePool     :: !(Maybe DevicePool)
  , _gdprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDevicePoolResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdprsDevicePool' - An object containing information about the requested device pool.
--
-- * 'gdprsResponseStatus' - -- | The response status code.
getDevicePoolResponse
    :: Int -- ^ 'gdprsResponseStatus'
    -> GetDevicePoolResponse
getDevicePoolResponse pResponseStatus_ =
  GetDevicePoolResponse'
    {_gdprsDevicePool = Nothing, _gdprsResponseStatus = pResponseStatus_}


-- | An object containing information about the requested device pool.
gdprsDevicePool :: Lens' GetDevicePoolResponse (Maybe DevicePool)
gdprsDevicePool = lens _gdprsDevicePool (\ s a -> s{_gdprsDevicePool = a})

-- | -- | The response status code.
gdprsResponseStatus :: Lens' GetDevicePoolResponse Int
gdprsResponseStatus = lens _gdprsResponseStatus (\ s a -> s{_gdprsResponseStatus = a})

instance NFData GetDevicePoolResponse where
