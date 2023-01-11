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
-- Module      : Amazonka.IoT1ClickDevices.UpdateDeviceState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Using a Boolean value (true or false), this operation enables or
-- disables the device given a device ID.
module Amazonka.IoT1ClickDevices.UpdateDeviceState
  ( -- * Creating a Request
    UpdateDeviceState (..),
    newUpdateDeviceState,

    -- * Request Lenses
    updateDeviceState_enabled,
    updateDeviceState_deviceId,

    -- * Destructuring the Response
    UpdateDeviceStateResponse (..),
    newUpdateDeviceStateResponse,

    -- * Response Lenses
    updateDeviceStateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT1ClickDevices.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateDeviceState' smart constructor.
data UpdateDeviceState = UpdateDeviceState'
  { -- | If true, the device is enabled. If false, the device is disabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The unique identifier of the device.
    deviceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDeviceState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'updateDeviceState_enabled' - If true, the device is enabled. If false, the device is disabled.
--
-- 'deviceId', 'updateDeviceState_deviceId' - The unique identifier of the device.
newUpdateDeviceState ::
  -- | 'deviceId'
  Prelude.Text ->
  UpdateDeviceState
newUpdateDeviceState pDeviceId_ =
  UpdateDeviceState'
    { enabled = Prelude.Nothing,
      deviceId = pDeviceId_
    }

-- | If true, the device is enabled. If false, the device is disabled.
updateDeviceState_enabled :: Lens.Lens' UpdateDeviceState (Prelude.Maybe Prelude.Bool)
updateDeviceState_enabled = Lens.lens (\UpdateDeviceState' {enabled} -> enabled) (\s@UpdateDeviceState' {} a -> s {enabled = a} :: UpdateDeviceState)

-- | The unique identifier of the device.
updateDeviceState_deviceId :: Lens.Lens' UpdateDeviceState Prelude.Text
updateDeviceState_deviceId = Lens.lens (\UpdateDeviceState' {deviceId} -> deviceId) (\s@UpdateDeviceState' {} a -> s {deviceId = a} :: UpdateDeviceState)

instance Core.AWSRequest UpdateDeviceState where
  type
    AWSResponse UpdateDeviceState =
      UpdateDeviceStateResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateDeviceStateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDeviceState where
  hashWithSalt _salt UpdateDeviceState' {..} =
    _salt `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` deviceId

instance Prelude.NFData UpdateDeviceState where
  rnf UpdateDeviceState' {..} =
    Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf deviceId

instance Data.ToHeaders UpdateDeviceState where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateDeviceState where
  toJSON UpdateDeviceState' {..} =
    Data.object
      ( Prelude.catMaybes
          [("enabled" Data..=) Prelude.<$> enabled]
      )

instance Data.ToPath UpdateDeviceState where
  toPath UpdateDeviceState' {..} =
    Prelude.mconcat
      ["/devices/", Data.toBS deviceId, "/state"]

instance Data.ToQuery UpdateDeviceState where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDeviceStateResponse' smart constructor.
data UpdateDeviceStateResponse = UpdateDeviceStateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDeviceStateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateDeviceStateResponse_httpStatus' - The response's http status code.
newUpdateDeviceStateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDeviceStateResponse
newUpdateDeviceStateResponse pHttpStatus_ =
  UpdateDeviceStateResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateDeviceStateResponse_httpStatus :: Lens.Lens' UpdateDeviceStateResponse Prelude.Int
updateDeviceStateResponse_httpStatus = Lens.lens (\UpdateDeviceStateResponse' {httpStatus} -> httpStatus) (\s@UpdateDeviceStateResponse' {} a -> s {httpStatus = a} :: UpdateDeviceStateResponse)

instance Prelude.NFData UpdateDeviceStateResponse where
  rnf UpdateDeviceStateResponse' {..} =
    Prelude.rnf httpStatus
