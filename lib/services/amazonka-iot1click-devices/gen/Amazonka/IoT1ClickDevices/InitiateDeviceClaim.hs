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
-- Module      : Amazonka.IoT1ClickDevices.InitiateDeviceClaim
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Given a device ID, initiates a claim request for the associated device.
--
-- Claiming a device consists of initiating a claim, then publishing a
-- device event, and finalizing the claim. For a device of type button, a
-- device event can be published by simply clicking the device.
module Amazonka.IoT1ClickDevices.InitiateDeviceClaim
  ( -- * Creating a Request
    InitiateDeviceClaim (..),
    newInitiateDeviceClaim,

    -- * Request Lenses
    initiateDeviceClaim_deviceId,

    -- * Destructuring the Response
    InitiateDeviceClaimResponse (..),
    newInitiateDeviceClaimResponse,

    -- * Response Lenses
    initiateDeviceClaimResponse_state,
    initiateDeviceClaimResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT1ClickDevices.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newInitiateDeviceClaim' smart constructor.
data InitiateDeviceClaim = InitiateDeviceClaim'
  { -- | The unique identifier of the device.
    deviceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InitiateDeviceClaim' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceId', 'initiateDeviceClaim_deviceId' - The unique identifier of the device.
newInitiateDeviceClaim ::
  -- | 'deviceId'
  Prelude.Text ->
  InitiateDeviceClaim
newInitiateDeviceClaim pDeviceId_ =
  InitiateDeviceClaim' {deviceId = pDeviceId_}

-- | The unique identifier of the device.
initiateDeviceClaim_deviceId :: Lens.Lens' InitiateDeviceClaim Prelude.Text
initiateDeviceClaim_deviceId = Lens.lens (\InitiateDeviceClaim' {deviceId} -> deviceId) (\s@InitiateDeviceClaim' {} a -> s {deviceId = a} :: InitiateDeviceClaim)

instance Core.AWSRequest InitiateDeviceClaim where
  type
    AWSResponse InitiateDeviceClaim =
      InitiateDeviceClaimResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          InitiateDeviceClaimResponse'
            Prelude.<$> (x Core..?> "state")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable InitiateDeviceClaim where
  hashWithSalt _salt InitiateDeviceClaim' {..} =
    _salt `Prelude.hashWithSalt` deviceId

instance Prelude.NFData InitiateDeviceClaim where
  rnf InitiateDeviceClaim' {..} = Prelude.rnf deviceId

instance Core.ToHeaders InitiateDeviceClaim where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON InitiateDeviceClaim where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath InitiateDeviceClaim where
  toPath InitiateDeviceClaim' {..} =
    Prelude.mconcat
      ["/devices/", Core.toBS deviceId, "/initiate-claim"]

instance Core.ToQuery InitiateDeviceClaim where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newInitiateDeviceClaimResponse' smart constructor.
data InitiateDeviceClaimResponse = InitiateDeviceClaimResponse'
  { -- | The device\'s final claim state.
    state :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InitiateDeviceClaimResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'initiateDeviceClaimResponse_state' - The device\'s final claim state.
--
-- 'httpStatus', 'initiateDeviceClaimResponse_httpStatus' - The response's http status code.
newInitiateDeviceClaimResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  InitiateDeviceClaimResponse
newInitiateDeviceClaimResponse pHttpStatus_ =
  InitiateDeviceClaimResponse'
    { state =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The device\'s final claim state.
initiateDeviceClaimResponse_state :: Lens.Lens' InitiateDeviceClaimResponse (Prelude.Maybe Prelude.Text)
initiateDeviceClaimResponse_state = Lens.lens (\InitiateDeviceClaimResponse' {state} -> state) (\s@InitiateDeviceClaimResponse' {} a -> s {state = a} :: InitiateDeviceClaimResponse)

-- | The response's http status code.
initiateDeviceClaimResponse_httpStatus :: Lens.Lens' InitiateDeviceClaimResponse Prelude.Int
initiateDeviceClaimResponse_httpStatus = Lens.lens (\InitiateDeviceClaimResponse' {httpStatus} -> httpStatus) (\s@InitiateDeviceClaimResponse' {} a -> s {httpStatus = a} :: InitiateDeviceClaimResponse)

instance Prelude.NFData InitiateDeviceClaimResponse where
  rnf InitiateDeviceClaimResponse' {..} =
    Prelude.rnf state
      `Prelude.seq` Prelude.rnf httpStatus
