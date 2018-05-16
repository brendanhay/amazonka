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
-- Module      : Network.AWS.AlexaBusiness.StartDeviceSync
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets a device and its account to the known default settings, by clearing all information and settings set by previous users.
--
--
module Network.AWS.AlexaBusiness.StartDeviceSync
    (
    -- * Creating a Request
      startDeviceSync
    , StartDeviceSync
    -- * Request Lenses
    , sdsDeviceARN
    , sdsRoomARN
    , sdsFeatures

    -- * Destructuring the Response
    , startDeviceSyncResponse
    , StartDeviceSyncResponse
    -- * Response Lenses
    , sdsrsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startDeviceSync' smart constructor.
data StartDeviceSync = StartDeviceSync'
  { _sdsDeviceARN :: !(Maybe Text)
  , _sdsRoomARN   :: !(Maybe Text)
  , _sdsFeatures  :: ![Feature]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartDeviceSync' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdsDeviceARN' - The ARN of the device to sync. Required.
--
-- * 'sdsRoomARN' - The ARN of the room with which the device to sync is associated. Required.
--
-- * 'sdsFeatures' - Request structure to start the device sync. Required.
startDeviceSync
    :: StartDeviceSync
startDeviceSync =
  StartDeviceSync'
    {_sdsDeviceARN = Nothing, _sdsRoomARN = Nothing, _sdsFeatures = mempty}


-- | The ARN of the device to sync. Required.
sdsDeviceARN :: Lens' StartDeviceSync (Maybe Text)
sdsDeviceARN = lens _sdsDeviceARN (\ s a -> s{_sdsDeviceARN = a})

-- | The ARN of the room with which the device to sync is associated. Required.
sdsRoomARN :: Lens' StartDeviceSync (Maybe Text)
sdsRoomARN = lens _sdsRoomARN (\ s a -> s{_sdsRoomARN = a})

-- | Request structure to start the device sync. Required.
sdsFeatures :: Lens' StartDeviceSync [Feature]
sdsFeatures = lens _sdsFeatures (\ s a -> s{_sdsFeatures = a}) . _Coerce

instance AWSRequest StartDeviceSync where
        type Rs StartDeviceSync = StartDeviceSyncResponse
        request = postJSON alexaBusiness
        response
          = receiveEmpty
              (\ s h x ->
                 StartDeviceSyncResponse' <$> (pure (fromEnum s)))

instance Hashable StartDeviceSync where

instance NFData StartDeviceSync where

instance ToHeaders StartDeviceSync where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.StartDeviceSync" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartDeviceSync where
        toJSON StartDeviceSync'{..}
          = object
              (catMaybes
                 [("DeviceArn" .=) <$> _sdsDeviceARN,
                  ("RoomArn" .=) <$> _sdsRoomARN,
                  Just ("Features" .= _sdsFeatures)])

instance ToPath StartDeviceSync where
        toPath = const "/"

instance ToQuery StartDeviceSync where
        toQuery = const mempty

-- | /See:/ 'startDeviceSyncResponse' smart constructor.
newtype StartDeviceSyncResponse = StartDeviceSyncResponse'
  { _sdsrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartDeviceSyncResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdsrsResponseStatus' - -- | The response status code.
startDeviceSyncResponse
    :: Int -- ^ 'sdsrsResponseStatus'
    -> StartDeviceSyncResponse
startDeviceSyncResponse pResponseStatus_ =
  StartDeviceSyncResponse' {_sdsrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
sdsrsResponseStatus :: Lens' StartDeviceSyncResponse Int
sdsrsResponseStatus = lens _sdsrsResponseStatus (\ s a -> s{_sdsrsResponseStatus = a})

instance NFData StartDeviceSyncResponse where
