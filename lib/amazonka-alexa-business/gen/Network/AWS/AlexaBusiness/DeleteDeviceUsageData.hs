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
-- Module      : Network.AWS.AlexaBusiness.DeleteDeviceUsageData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- When this action is called for a specified shared device, it allows authorized users to delete the device's entire previous history of voice input data and associated response data. This action can be called once every 24 hours for a specific shared device.
module Network.AWS.AlexaBusiness.DeleteDeviceUsageData
  ( -- * Creating a Request
    deleteDeviceUsageData,
    DeleteDeviceUsageData,

    -- * Request Lenses
    ddudDeviceARN,
    ddudDeviceUsageType,

    -- * Destructuring the Response
    deleteDeviceUsageDataResponse,
    DeleteDeviceUsageDataResponse,

    -- * Response Lenses
    ddudrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteDeviceUsageData' smart constructor.
data DeleteDeviceUsageData = DeleteDeviceUsageData'
  { _ddudDeviceARN ::
      !Text,
    _ddudDeviceUsageType :: !DeviceUsageType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteDeviceUsageData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddudDeviceARN' - The ARN of the device.
--
-- * 'ddudDeviceUsageType' - The type of usage data to delete.
deleteDeviceUsageData ::
  -- | 'ddudDeviceARN'
  Text ->
  -- | 'ddudDeviceUsageType'
  DeviceUsageType ->
  DeleteDeviceUsageData
deleteDeviceUsageData pDeviceARN_ pDeviceUsageType_ =
  DeleteDeviceUsageData'
    { _ddudDeviceARN = pDeviceARN_,
      _ddudDeviceUsageType = pDeviceUsageType_
    }

-- | The ARN of the device.
ddudDeviceARN :: Lens' DeleteDeviceUsageData Text
ddudDeviceARN = lens _ddudDeviceARN (\s a -> s {_ddudDeviceARN = a})

-- | The type of usage data to delete.
ddudDeviceUsageType :: Lens' DeleteDeviceUsageData DeviceUsageType
ddudDeviceUsageType = lens _ddudDeviceUsageType (\s a -> s {_ddudDeviceUsageType = a})

instance AWSRequest DeleteDeviceUsageData where
  type Rs DeleteDeviceUsageData = DeleteDeviceUsageDataResponse
  request = postJSON alexaBusiness
  response =
    receiveEmpty
      (\s h x -> DeleteDeviceUsageDataResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteDeviceUsageData

instance NFData DeleteDeviceUsageData

instance ToHeaders DeleteDeviceUsageData where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AlexaForBusiness.DeleteDeviceUsageData" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteDeviceUsageData where
  toJSON DeleteDeviceUsageData' {..} =
    object
      ( catMaybes
          [ Just ("DeviceArn" .= _ddudDeviceARN),
            Just ("DeviceUsageType" .= _ddudDeviceUsageType)
          ]
      )

instance ToPath DeleteDeviceUsageData where
  toPath = const "/"

instance ToQuery DeleteDeviceUsageData where
  toQuery = const mempty

-- | /See:/ 'deleteDeviceUsageDataResponse' smart constructor.
newtype DeleteDeviceUsageDataResponse = DeleteDeviceUsageDataResponse'
  { _ddudrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteDeviceUsageDataResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddudrsResponseStatus' - -- | The response status code.
deleteDeviceUsageDataResponse ::
  -- | 'ddudrsResponseStatus'
  Int ->
  DeleteDeviceUsageDataResponse
deleteDeviceUsageDataResponse pResponseStatus_ =
  DeleteDeviceUsageDataResponse'
    { _ddudrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
ddudrsResponseStatus :: Lens' DeleteDeviceUsageDataResponse Int
ddudrsResponseStatus = lens _ddudrsResponseStatus (\s a -> s {_ddudrsResponseStatus = a})

instance NFData DeleteDeviceUsageDataResponse
