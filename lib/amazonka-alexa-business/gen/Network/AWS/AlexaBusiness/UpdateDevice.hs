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
-- Module      : Network.AWS.AlexaBusiness.UpdateDevice
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the device name by device ARN.
--
--
module Network.AWS.AlexaBusiness.UpdateDevice
    (
    -- * Creating a Request
      updateDevice
    , UpdateDevice
    -- * Request Lenses
    , udDeviceARN
    , udDeviceName

    -- * Destructuring the Response
    , updateDeviceResponse
    , UpdateDeviceResponse
    -- * Response Lenses
    , udrsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateDevice' smart constructor.
data UpdateDevice = UpdateDevice'
  { _udDeviceARN  :: !(Maybe Text)
  , _udDeviceName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDevice' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udDeviceARN' - The ARN of the device to update. Required.
--
-- * 'udDeviceName' - The updated device name. Required.
updateDevice
    :: UpdateDevice
updateDevice = UpdateDevice' {_udDeviceARN = Nothing, _udDeviceName = Nothing}


-- | The ARN of the device to update. Required.
udDeviceARN :: Lens' UpdateDevice (Maybe Text)
udDeviceARN = lens _udDeviceARN (\ s a -> s{_udDeviceARN = a})

-- | The updated device name. Required.
udDeviceName :: Lens' UpdateDevice (Maybe Text)
udDeviceName = lens _udDeviceName (\ s a -> s{_udDeviceName = a})

instance AWSRequest UpdateDevice where
        type Rs UpdateDevice = UpdateDeviceResponse
        request = postJSON alexaBusiness
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateDeviceResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateDevice where

instance NFData UpdateDevice where

instance ToHeaders UpdateDevice where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.UpdateDevice" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateDevice where
        toJSON UpdateDevice'{..}
          = object
              (catMaybes
                 [("DeviceArn" .=) <$> _udDeviceARN,
                  ("DeviceName" .=) <$> _udDeviceName])

instance ToPath UpdateDevice where
        toPath = const "/"

instance ToQuery UpdateDevice where
        toQuery = const mempty

-- | /See:/ 'updateDeviceResponse' smart constructor.
newtype UpdateDeviceResponse = UpdateDeviceResponse'
  { _udrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDeviceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udrsResponseStatus' - -- | The response status code.
updateDeviceResponse
    :: Int -- ^ 'udrsResponseStatus'
    -> UpdateDeviceResponse
updateDeviceResponse pResponseStatus_ =
  UpdateDeviceResponse' {_udrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
udrsResponseStatus :: Lens' UpdateDeviceResponse Int
udrsResponseStatus = lens _udrsResponseStatus (\ s a -> s{_udrsResponseStatus = a})

instance NFData UpdateDeviceResponse where
