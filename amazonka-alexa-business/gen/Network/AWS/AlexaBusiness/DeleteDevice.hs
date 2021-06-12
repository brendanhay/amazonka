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
-- Module      : Network.AWS.AlexaBusiness.DeleteDevice
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a device from Alexa For Business.
module Network.AWS.AlexaBusiness.DeleteDevice
  ( -- * Creating a Request
    DeleteDevice (..),
    newDeleteDevice,

    -- * Request Lenses
    deleteDevice_deviceArn,

    -- * Destructuring the Response
    DeleteDeviceResponse (..),
    newDeleteDeviceResponse,

    -- * Response Lenses
    deleteDeviceResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteDevice' smart constructor.
data DeleteDevice = DeleteDevice'
  { -- | The ARN of the device for which to request details.
    deviceArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceArn', 'deleteDevice_deviceArn' - The ARN of the device for which to request details.
newDeleteDevice ::
  -- | 'deviceArn'
  Core.Text ->
  DeleteDevice
newDeleteDevice pDeviceArn_ =
  DeleteDevice' {deviceArn = pDeviceArn_}

-- | The ARN of the device for which to request details.
deleteDevice_deviceArn :: Lens.Lens' DeleteDevice Core.Text
deleteDevice_deviceArn = Lens.lens (\DeleteDevice' {deviceArn} -> deviceArn) (\s@DeleteDevice' {} a -> s {deviceArn = a} :: DeleteDevice)

instance Core.AWSRequest DeleteDevice where
  type AWSResponse DeleteDevice = DeleteDeviceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDeviceResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteDevice

instance Core.NFData DeleteDevice

instance Core.ToHeaders DeleteDevice where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AlexaForBusiness.DeleteDevice" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteDevice where
  toJSON DeleteDevice' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("DeviceArn" Core..= deviceArn)]
      )

instance Core.ToPath DeleteDevice where
  toPath = Core.const "/"

instance Core.ToQuery DeleteDevice where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteDeviceResponse' smart constructor.
data DeleteDeviceResponse = DeleteDeviceResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteDeviceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteDeviceResponse_httpStatus' - The response's http status code.
newDeleteDeviceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteDeviceResponse
newDeleteDeviceResponse pHttpStatus_ =
  DeleteDeviceResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteDeviceResponse_httpStatus :: Lens.Lens' DeleteDeviceResponse Core.Int
deleteDeviceResponse_httpStatus = Lens.lens (\DeleteDeviceResponse' {httpStatus} -> httpStatus) (\s@DeleteDeviceResponse' {} a -> s {httpStatus = a} :: DeleteDeviceResponse)

instance Core.NFData DeleteDeviceResponse
