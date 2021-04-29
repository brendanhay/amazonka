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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteDevice' smart constructor.
data DeleteDevice = DeleteDevice'
  { -- | The ARN of the device for which to request details.
    deviceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteDevice
newDeleteDevice pDeviceArn_ =
  DeleteDevice' {deviceArn = pDeviceArn_}

-- | The ARN of the device for which to request details.
deleteDevice_deviceArn :: Lens.Lens' DeleteDevice Prelude.Text
deleteDevice_deviceArn = Lens.lens (\DeleteDevice' {deviceArn} -> deviceArn) (\s@DeleteDevice' {} a -> s {deviceArn = a} :: DeleteDevice)

instance Prelude.AWSRequest DeleteDevice where
  type Rs DeleteDevice = DeleteDeviceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDeviceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDevice

instance Prelude.NFData DeleteDevice

instance Prelude.ToHeaders DeleteDevice where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AlexaForBusiness.DeleteDevice" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteDevice where
  toJSON DeleteDevice' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("DeviceArn" Prelude..= deviceArn)]
      )

instance Prelude.ToPath DeleteDevice where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteDevice where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDeviceResponse' smart constructor.
data DeleteDeviceResponse = DeleteDeviceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeleteDeviceResponse
newDeleteDeviceResponse pHttpStatus_ =
  DeleteDeviceResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteDeviceResponse_httpStatus :: Lens.Lens' DeleteDeviceResponse Prelude.Int
deleteDeviceResponse_httpStatus = Lens.lens (\DeleteDeviceResponse' {httpStatus} -> httpStatus) (\s@DeleteDeviceResponse' {} a -> s {httpStatus = a} :: DeleteDeviceResponse)

instance Prelude.NFData DeleteDeviceResponse
