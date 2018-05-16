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
-- Module      : Network.AWS.AlexaBusiness.GetDevice
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the details of a device by device ARN.
--
--
module Network.AWS.AlexaBusiness.GetDevice
    (
    -- * Creating a Request
      getDevice
    , GetDevice
    -- * Request Lenses
    , gdDeviceARN

    -- * Destructuring the Response
    , getDeviceResponse
    , GetDeviceResponse
    -- * Response Lenses
    , gdrsDevice
    , gdrsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getDevice' smart constructor.
newtype GetDevice = GetDevice'
  { _gdDeviceARN :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDevice' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdDeviceARN' - The ARN of the device for which to request details. Required.
getDevice
    :: GetDevice
getDevice = GetDevice' {_gdDeviceARN = Nothing}


-- | The ARN of the device for which to request details. Required.
gdDeviceARN :: Lens' GetDevice (Maybe Text)
gdDeviceARN = lens _gdDeviceARN (\ s a -> s{_gdDeviceARN = a})

instance AWSRequest GetDevice where
        type Rs GetDevice = GetDeviceResponse
        request = postJSON alexaBusiness
        response
          = receiveJSON
              (\ s h x ->
                 GetDeviceResponse' <$>
                   (x .?> "Device") <*> (pure (fromEnum s)))

instance Hashable GetDevice where

instance NFData GetDevice where

instance ToHeaders GetDevice where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.GetDevice" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetDevice where
        toJSON GetDevice'{..}
          = object
              (catMaybes [("DeviceArn" .=) <$> _gdDeviceARN])

instance ToPath GetDevice where
        toPath = const "/"

instance ToQuery GetDevice where
        toQuery = const mempty

-- | /See:/ 'getDeviceResponse' smart constructor.
data GetDeviceResponse = GetDeviceResponse'
  { _gdrsDevice         :: !(Maybe Device)
  , _gdrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDeviceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdrsDevice' - The details of the device requested. Required.
--
-- * 'gdrsResponseStatus' - -- | The response status code.
getDeviceResponse
    :: Int -- ^ 'gdrsResponseStatus'
    -> GetDeviceResponse
getDeviceResponse pResponseStatus_ =
  GetDeviceResponse'
    {_gdrsDevice = Nothing, _gdrsResponseStatus = pResponseStatus_}


-- | The details of the device requested. Required.
gdrsDevice :: Lens' GetDeviceResponse (Maybe Device)
gdrsDevice = lens _gdrsDevice (\ s a -> s{_gdrsDevice = a})

-- | -- | The response status code.
gdrsResponseStatus :: Lens' GetDeviceResponse Int
gdrsResponseStatus = lens _gdrsResponseStatus (\ s a -> s{_gdrsResponseStatus = a})

instance NFData GetDeviceResponse where
