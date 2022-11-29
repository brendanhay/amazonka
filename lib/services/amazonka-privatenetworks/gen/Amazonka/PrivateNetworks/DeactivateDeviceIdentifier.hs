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
-- Module      : Amazonka.PrivateNetworks.DeactivateDeviceIdentifier
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deactivates the specified device identifier.
module Amazonka.PrivateNetworks.DeactivateDeviceIdentifier
  ( -- * Creating a Request
    DeactivateDeviceIdentifier (..),
    newDeactivateDeviceIdentifier,

    -- * Request Lenses
    deactivateDeviceIdentifier_clientToken,
    deactivateDeviceIdentifier_deviceIdentifierArn,

    -- * Destructuring the Response
    DeactivateDeviceIdentifierResponse (..),
    newDeactivateDeviceIdentifierResponse,

    -- * Response Lenses
    deactivateDeviceIdentifierResponse_httpStatus,
    deactivateDeviceIdentifierResponse_deviceIdentifier,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.PrivateNetworks.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeactivateDeviceIdentifier' smart constructor.
data DeactivateDeviceIdentifier = DeactivateDeviceIdentifier'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to ensure idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the device identifier.
    deviceIdentifierArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeactivateDeviceIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deactivateDeviceIdentifier_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to ensure idempotency>.
--
-- 'deviceIdentifierArn', 'deactivateDeviceIdentifier_deviceIdentifierArn' - The Amazon Resource Name (ARN) of the device identifier.
newDeactivateDeviceIdentifier ::
  -- | 'deviceIdentifierArn'
  Prelude.Text ->
  DeactivateDeviceIdentifier
newDeactivateDeviceIdentifier pDeviceIdentifierArn_ =
  DeactivateDeviceIdentifier'
    { clientToken =
        Prelude.Nothing,
      deviceIdentifierArn = pDeviceIdentifierArn_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to ensure idempotency>.
deactivateDeviceIdentifier_clientToken :: Lens.Lens' DeactivateDeviceIdentifier (Prelude.Maybe Prelude.Text)
deactivateDeviceIdentifier_clientToken = Lens.lens (\DeactivateDeviceIdentifier' {clientToken} -> clientToken) (\s@DeactivateDeviceIdentifier' {} a -> s {clientToken = a} :: DeactivateDeviceIdentifier)

-- | The Amazon Resource Name (ARN) of the device identifier.
deactivateDeviceIdentifier_deviceIdentifierArn :: Lens.Lens' DeactivateDeviceIdentifier Prelude.Text
deactivateDeviceIdentifier_deviceIdentifierArn = Lens.lens (\DeactivateDeviceIdentifier' {deviceIdentifierArn} -> deviceIdentifierArn) (\s@DeactivateDeviceIdentifier' {} a -> s {deviceIdentifierArn = a} :: DeactivateDeviceIdentifier)

instance Core.AWSRequest DeactivateDeviceIdentifier where
  type
    AWSResponse DeactivateDeviceIdentifier =
      DeactivateDeviceIdentifierResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeactivateDeviceIdentifierResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "deviceIdentifier")
      )

instance Prelude.Hashable DeactivateDeviceIdentifier where
  hashWithSalt _salt DeactivateDeviceIdentifier' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` deviceIdentifierArn

instance Prelude.NFData DeactivateDeviceIdentifier where
  rnf DeactivateDeviceIdentifier' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf deviceIdentifierArn

instance Core.ToHeaders DeactivateDeviceIdentifier where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeactivateDeviceIdentifier where
  toJSON DeactivateDeviceIdentifier' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("clientToken" Core..=) Prelude.<$> clientToken,
            Prelude.Just
              ("deviceIdentifierArn" Core..= deviceIdentifierArn)
          ]
      )

instance Core.ToPath DeactivateDeviceIdentifier where
  toPath =
    Prelude.const "/v1/device-identifiers/deactivate"

instance Core.ToQuery DeactivateDeviceIdentifier where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeactivateDeviceIdentifierResponse' smart constructor.
data DeactivateDeviceIdentifierResponse = DeactivateDeviceIdentifierResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about the device identifier.
    deviceIdentifier :: DeviceIdentifier
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeactivateDeviceIdentifierResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deactivateDeviceIdentifierResponse_httpStatus' - The response's http status code.
--
-- 'deviceIdentifier', 'deactivateDeviceIdentifierResponse_deviceIdentifier' - Information about the device identifier.
newDeactivateDeviceIdentifierResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'deviceIdentifier'
  DeviceIdentifier ->
  DeactivateDeviceIdentifierResponse
newDeactivateDeviceIdentifierResponse
  pHttpStatus_
  pDeviceIdentifier_ =
    DeactivateDeviceIdentifierResponse'
      { httpStatus =
          pHttpStatus_,
        deviceIdentifier = pDeviceIdentifier_
      }

-- | The response's http status code.
deactivateDeviceIdentifierResponse_httpStatus :: Lens.Lens' DeactivateDeviceIdentifierResponse Prelude.Int
deactivateDeviceIdentifierResponse_httpStatus = Lens.lens (\DeactivateDeviceIdentifierResponse' {httpStatus} -> httpStatus) (\s@DeactivateDeviceIdentifierResponse' {} a -> s {httpStatus = a} :: DeactivateDeviceIdentifierResponse)

-- | Information about the device identifier.
deactivateDeviceIdentifierResponse_deviceIdentifier :: Lens.Lens' DeactivateDeviceIdentifierResponse DeviceIdentifier
deactivateDeviceIdentifierResponse_deviceIdentifier = Lens.lens (\DeactivateDeviceIdentifierResponse' {deviceIdentifier} -> deviceIdentifier) (\s@DeactivateDeviceIdentifierResponse' {} a -> s {deviceIdentifier = a} :: DeactivateDeviceIdentifierResponse)

instance
  Prelude.NFData
    DeactivateDeviceIdentifierResponse
  where
  rnf DeactivateDeviceIdentifierResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf deviceIdentifier
