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
-- Module      : Amazonka.Panorama.DeleteDevice
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a device.
module Amazonka.Panorama.DeleteDevice
  ( -- * Creating a Request
    DeleteDevice (..),
    newDeleteDevice,

    -- * Request Lenses
    deleteDevice_deviceId,

    -- * Destructuring the Response
    DeleteDeviceResponse (..),
    newDeleteDeviceResponse,

    -- * Response Lenses
    deleteDeviceResponse_deviceId,
    deleteDeviceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Panorama.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDevice' smart constructor.
data DeleteDevice = DeleteDevice'
  { -- | The device\'s ID.
    deviceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceId', 'deleteDevice_deviceId' - The device\'s ID.
newDeleteDevice ::
  -- | 'deviceId'
  Prelude.Text ->
  DeleteDevice
newDeleteDevice pDeviceId_ =
  DeleteDevice' {deviceId = pDeviceId_}

-- | The device\'s ID.
deleteDevice_deviceId :: Lens.Lens' DeleteDevice Prelude.Text
deleteDevice_deviceId = Lens.lens (\DeleteDevice' {deviceId} -> deviceId) (\s@DeleteDevice' {} a -> s {deviceId = a} :: DeleteDevice)

instance Core.AWSRequest DeleteDevice where
  type AWSResponse DeleteDevice = DeleteDeviceResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteDeviceResponse'
            Prelude.<$> (x Data..?> "DeviceId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDevice where
  hashWithSalt _salt DeleteDevice' {..} =
    _salt `Prelude.hashWithSalt` deviceId

instance Prelude.NFData DeleteDevice where
  rnf DeleteDevice' {..} = Prelude.rnf deviceId

instance Data.ToHeaders DeleteDevice where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteDevice where
  toPath DeleteDevice' {..} =
    Prelude.mconcat ["/devices/", Data.toBS deviceId]

instance Data.ToQuery DeleteDevice where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDeviceResponse' smart constructor.
data DeleteDeviceResponse = DeleteDeviceResponse'
  { -- | The device\'s ID.
    deviceId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDeviceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceId', 'deleteDeviceResponse_deviceId' - The device\'s ID.
--
-- 'httpStatus', 'deleteDeviceResponse_httpStatus' - The response's http status code.
newDeleteDeviceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDeviceResponse
newDeleteDeviceResponse pHttpStatus_ =
  DeleteDeviceResponse'
    { deviceId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The device\'s ID.
deleteDeviceResponse_deviceId :: Lens.Lens' DeleteDeviceResponse (Prelude.Maybe Prelude.Text)
deleteDeviceResponse_deviceId = Lens.lens (\DeleteDeviceResponse' {deviceId} -> deviceId) (\s@DeleteDeviceResponse' {} a -> s {deviceId = a} :: DeleteDeviceResponse)

-- | The response's http status code.
deleteDeviceResponse_httpStatus :: Lens.Lens' DeleteDeviceResponse Prelude.Int
deleteDeviceResponse_httpStatus = Lens.lens (\DeleteDeviceResponse' {httpStatus} -> httpStatus) (\s@DeleteDeviceResponse' {} a -> s {httpStatus = a} :: DeleteDeviceResponse)

instance Prelude.NFData DeleteDeviceResponse where
  rnf DeleteDeviceResponse' {..} =
    Prelude.rnf deviceId
      `Prelude.seq` Prelude.rnf httpStatus
