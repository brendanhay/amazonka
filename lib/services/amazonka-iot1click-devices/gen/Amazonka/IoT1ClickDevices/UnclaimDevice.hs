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
-- Module      : Amazonka.IoT1ClickDevices.UnclaimDevice
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a device from your AWS account using its device ID.
module Amazonka.IoT1ClickDevices.UnclaimDevice
  ( -- * Creating a Request
    UnclaimDevice (..),
    newUnclaimDevice,

    -- * Request Lenses
    unclaimDevice_deviceId,

    -- * Destructuring the Response
    UnclaimDeviceResponse (..),
    newUnclaimDeviceResponse,

    -- * Response Lenses
    unclaimDeviceResponse_state,
    unclaimDeviceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT1ClickDevices.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUnclaimDevice' smart constructor.
data UnclaimDevice = UnclaimDevice'
  { -- | The unique identifier of the device.
    deviceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnclaimDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceId', 'unclaimDevice_deviceId' - The unique identifier of the device.
newUnclaimDevice ::
  -- | 'deviceId'
  Prelude.Text ->
  UnclaimDevice
newUnclaimDevice pDeviceId_ =
  UnclaimDevice' {deviceId = pDeviceId_}

-- | The unique identifier of the device.
unclaimDevice_deviceId :: Lens.Lens' UnclaimDevice Prelude.Text
unclaimDevice_deviceId = Lens.lens (\UnclaimDevice' {deviceId} -> deviceId) (\s@UnclaimDevice' {} a -> s {deviceId = a} :: UnclaimDevice)

instance Core.AWSRequest UnclaimDevice where
  type
    AWSResponse UnclaimDevice =
      UnclaimDeviceResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UnclaimDeviceResponse'
            Prelude.<$> (x Data..?> "state")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UnclaimDevice where
  hashWithSalt _salt UnclaimDevice' {..} =
    _salt `Prelude.hashWithSalt` deviceId

instance Prelude.NFData UnclaimDevice where
  rnf UnclaimDevice' {..} = Prelude.rnf deviceId

instance Data.ToHeaders UnclaimDevice where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UnclaimDevice where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath UnclaimDevice where
  toPath UnclaimDevice' {..} =
    Prelude.mconcat
      ["/devices/", Data.toBS deviceId, "/unclaim"]

instance Data.ToQuery UnclaimDevice where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUnclaimDeviceResponse' smart constructor.
data UnclaimDeviceResponse = UnclaimDeviceResponse'
  { -- | The device\'s final claim state.
    state :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnclaimDeviceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'unclaimDeviceResponse_state' - The device\'s final claim state.
--
-- 'httpStatus', 'unclaimDeviceResponse_httpStatus' - The response's http status code.
newUnclaimDeviceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UnclaimDeviceResponse
newUnclaimDeviceResponse pHttpStatus_ =
  UnclaimDeviceResponse'
    { state = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The device\'s final claim state.
unclaimDeviceResponse_state :: Lens.Lens' UnclaimDeviceResponse (Prelude.Maybe Prelude.Text)
unclaimDeviceResponse_state = Lens.lens (\UnclaimDeviceResponse' {state} -> state) (\s@UnclaimDeviceResponse' {} a -> s {state = a} :: UnclaimDeviceResponse)

-- | The response's http status code.
unclaimDeviceResponse_httpStatus :: Lens.Lens' UnclaimDeviceResponse Prelude.Int
unclaimDeviceResponse_httpStatus = Lens.lens (\UnclaimDeviceResponse' {httpStatus} -> httpStatus) (\s@UnclaimDeviceResponse' {} a -> s {httpStatus = a} :: UnclaimDeviceResponse)

instance Prelude.NFData UnclaimDeviceResponse where
  rnf UnclaimDeviceResponse' {..} =
    Prelude.rnf state
      `Prelude.seq` Prelude.rnf httpStatus
