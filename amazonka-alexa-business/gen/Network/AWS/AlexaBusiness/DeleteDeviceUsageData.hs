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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteDeviceUsageData' smart constructor.
data DeleteDeviceUsageData = DeleteDeviceUsageData'
  { -- | The ARN of the device.
    deviceArn :: Core.Text,
    -- | The type of usage data to delete.
    deviceUsageType :: DeviceUsageType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
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
deleteDeviceUsageData_deviceArn :: Lens.Lens' DeleteDeviceUsageData Core.Text
deleteDeviceUsageData_deviceArn = Lens.lens (\DeleteDeviceUsageData' {deviceArn} -> deviceArn) (\s@DeleteDeviceUsageData' {} a -> s {deviceArn = a} :: DeleteDeviceUsageData)

-- | The type of usage data to delete.
deleteDeviceUsageData_deviceUsageType :: Lens.Lens' DeleteDeviceUsageData DeviceUsageType
deleteDeviceUsageData_deviceUsageType = Lens.lens (\DeleteDeviceUsageData' {deviceUsageType} -> deviceUsageType) (\s@DeleteDeviceUsageData' {} a -> s {deviceUsageType = a} :: DeleteDeviceUsageData)

instance Core.AWSRequest DeleteDeviceUsageData where
  type
    AWSResponse DeleteDeviceUsageData =
      DeleteDeviceUsageDataResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDeviceUsageDataResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteDeviceUsageData

instance Core.NFData DeleteDeviceUsageData

instance Core.ToHeaders DeleteDeviceUsageData where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.DeleteDeviceUsageData" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteDeviceUsageData where
  toJSON DeleteDeviceUsageData' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DeviceArn" Core..= deviceArn),
            Core.Just
              ("DeviceUsageType" Core..= deviceUsageType)
          ]
      )

instance Core.ToPath DeleteDeviceUsageData where
  toPath = Core.const "/"

instance Core.ToQuery DeleteDeviceUsageData where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteDeviceUsageDataResponse' smart constructor.
data DeleteDeviceUsageDataResponse = DeleteDeviceUsageDataResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DeleteDeviceUsageDataResponse
newDeleteDeviceUsageDataResponse pHttpStatus_ =
  DeleteDeviceUsageDataResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteDeviceUsageDataResponse_httpStatus :: Lens.Lens' DeleteDeviceUsageDataResponse Core.Int
deleteDeviceUsageDataResponse_httpStatus = Lens.lens (\DeleteDeviceUsageDataResponse' {httpStatus} -> httpStatus) (\s@DeleteDeviceUsageDataResponse' {} a -> s {httpStatus = a} :: DeleteDeviceUsageDataResponse)

instance Core.NFData DeleteDeviceUsageDataResponse
