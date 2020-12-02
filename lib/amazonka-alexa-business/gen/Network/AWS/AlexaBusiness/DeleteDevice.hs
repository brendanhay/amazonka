{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.DeleteDevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a device from Alexa For Business.
module Network.AWS.AlexaBusiness.DeleteDevice
  ( -- * Creating a Request
    deleteDevice,
    DeleteDevice,

    -- * Request Lenses
    delDeviceARN,

    -- * Destructuring the Response
    deleteDeviceResponse,
    DeleteDeviceResponse,

    -- * Response Lenses
    ddrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteDevice' smart constructor.
newtype DeleteDevice = DeleteDevice' {_delDeviceARN :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteDevice' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delDeviceARN' - The ARN of the device for which to request details.
deleteDevice ::
  -- | 'delDeviceARN'
  Text ->
  DeleteDevice
deleteDevice pDeviceARN_ =
  DeleteDevice' {_delDeviceARN = pDeviceARN_}

-- | The ARN of the device for which to request details.
delDeviceARN :: Lens' DeleteDevice Text
delDeviceARN = lens _delDeviceARN (\s a -> s {_delDeviceARN = a})

instance AWSRequest DeleteDevice where
  type Rs DeleteDevice = DeleteDeviceResponse
  request = postJSON alexaBusiness
  response =
    receiveEmpty
      (\s h x -> DeleteDeviceResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteDevice

instance NFData DeleteDevice

instance ToHeaders DeleteDevice where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AlexaForBusiness.DeleteDevice" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteDevice where
  toJSON DeleteDevice' {..} =
    object (catMaybes [Just ("DeviceArn" .= _delDeviceARN)])

instance ToPath DeleteDevice where
  toPath = const "/"

instance ToQuery DeleteDevice where
  toQuery = const mempty

-- | /See:/ 'deleteDeviceResponse' smart constructor.
newtype DeleteDeviceResponse = DeleteDeviceResponse'
  { _ddrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteDeviceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddrsResponseStatus' - -- | The response status code.
deleteDeviceResponse ::
  -- | 'ddrsResponseStatus'
  Int ->
  DeleteDeviceResponse
deleteDeviceResponse pResponseStatus_ =
  DeleteDeviceResponse' {_ddrsResponseStatus = pResponseStatus_}

-- | -- | The response status code.
ddrsResponseStatus :: Lens' DeleteDeviceResponse Int
ddrsResponseStatus = lens _ddrsResponseStatus (\s a -> s {_ddrsResponseStatus = a})

instance NFData DeleteDeviceResponse
