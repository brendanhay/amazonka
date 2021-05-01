{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.DeleteDeviceUsageData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- When this action is called for a specified shared device, it allows
-- authorized users to delete the device\'s entire previous history of
-- voice input data and associated response data. This action can be called
-- once every 24 hours for a specific shared device.
module Network.AWS.AlexaBusiness.DeleteDeviceUsageData
  ( -- * Creating a Request
    DeleteDeviceUsageData (..),
    newDeleteDeviceUsageData,

    -- * Request Lenses
    deleteDeviceUsageData_deviceArn,
    deleteDeviceUsageData_deviceUsageType,

    -- * Destructuring the Response
    DeleteDeviceUsageDataResponse (..),
    newDeleteDeviceUsageDataResponse,

    -- * Response Lenses
    deleteDeviceUsageDataResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteDeviceUsageData' smart constructor.
data DeleteDeviceUsageData = DeleteDeviceUsageData'
  { -- | The ARN of the device.
    deviceArn :: Prelude.Text,
    -- | The type of usage data to delete.
    deviceUsageType :: DeviceUsageType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteDeviceUsageData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceArn', 'deleteDeviceUsageData_deviceArn' - The ARN of the device.
--
-- 'deviceUsageType', 'deleteDeviceUsageData_deviceUsageType' - The type of usage data to delete.
newDeleteDeviceUsageData ::
  -- | 'deviceArn'
  Prelude.Text ->
  -- | 'deviceUsageType'
  DeviceUsageType ->
  DeleteDeviceUsageData
newDeleteDeviceUsageData
  pDeviceArn_
  pDeviceUsageType_ =
    DeleteDeviceUsageData'
      { deviceArn = pDeviceArn_,
        deviceUsageType = pDeviceUsageType_
      }

-- | The ARN of the device.
deleteDeviceUsageData_deviceArn :: Lens.Lens' DeleteDeviceUsageData Prelude.Text
deleteDeviceUsageData_deviceArn = Lens.lens (\DeleteDeviceUsageData' {deviceArn} -> deviceArn) (\s@DeleteDeviceUsageData' {} a -> s {deviceArn = a} :: DeleteDeviceUsageData)

-- | The type of usage data to delete.
deleteDeviceUsageData_deviceUsageType :: Lens.Lens' DeleteDeviceUsageData DeviceUsageType
deleteDeviceUsageData_deviceUsageType = Lens.lens (\DeleteDeviceUsageData' {deviceUsageType} -> deviceUsageType) (\s@DeleteDeviceUsageData' {} a -> s {deviceUsageType = a} :: DeleteDeviceUsageData)

instance Prelude.AWSRequest DeleteDeviceUsageData where
  type
    Rs DeleteDeviceUsageData =
      DeleteDeviceUsageDataResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDeviceUsageDataResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDeviceUsageData

instance Prelude.NFData DeleteDeviceUsageData

instance Prelude.ToHeaders DeleteDeviceUsageData where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AlexaForBusiness.DeleteDeviceUsageData" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteDeviceUsageData where
  toJSON DeleteDeviceUsageData' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DeviceArn" Prelude..= deviceArn),
            Prelude.Just
              ("DeviceUsageType" Prelude..= deviceUsageType)
          ]
      )

instance Prelude.ToPath DeleteDeviceUsageData where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteDeviceUsageData where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDeviceUsageDataResponse' smart constructor.
data DeleteDeviceUsageDataResponse = DeleteDeviceUsageDataResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteDeviceUsageDataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteDeviceUsageDataResponse_httpStatus' - The response's http status code.
newDeleteDeviceUsageDataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDeviceUsageDataResponse
newDeleteDeviceUsageDataResponse pHttpStatus_ =
  DeleteDeviceUsageDataResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteDeviceUsageDataResponse_httpStatus :: Lens.Lens' DeleteDeviceUsageDataResponse Prelude.Int
deleteDeviceUsageDataResponse_httpStatus = Lens.lens (\DeleteDeviceUsageDataResponse' {httpStatus} -> httpStatus) (\s@DeleteDeviceUsageDataResponse' {} a -> s {httpStatus = a} :: DeleteDeviceUsageDataResponse)

instance Prelude.NFData DeleteDeviceUsageDataResponse
