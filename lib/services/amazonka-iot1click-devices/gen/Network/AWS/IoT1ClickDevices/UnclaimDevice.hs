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
-- Module      : Network.AWS.IoT1ClickDevices.UnclaimDevice
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a device from your AWS account using its device ID.
module Network.AWS.IoT1ClickDevices.UnclaimDevice
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

import qualified Network.AWS.Core as Core
import Network.AWS.IoT1ClickDevices.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UnclaimDeviceResponse'
            Prelude.<$> (x Core..?> "state")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UnclaimDevice

instance Prelude.NFData UnclaimDevice

instance Core.ToHeaders UnclaimDevice where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UnclaimDevice where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath UnclaimDevice where
  toPath UnclaimDevice' {..} =
    Prelude.mconcat
      ["/devices/", Core.toBS deviceId, "/unclaim"]

instance Core.ToQuery UnclaimDevice where
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

instance Prelude.NFData UnclaimDeviceResponse
