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
-- Module      : Amazonka.PrivateNetworks.ActivateDeviceIdentifier
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Activates the specified device identifier.
module Amazonka.PrivateNetworks.ActivateDeviceIdentifier
  ( -- * Creating a Request
    ActivateDeviceIdentifier (..),
    newActivateDeviceIdentifier,

    -- * Request Lenses
    activateDeviceIdentifier_clientToken,
    activateDeviceIdentifier_deviceIdentifierArn,

    -- * Destructuring the Response
    ActivateDeviceIdentifierResponse (..),
    newActivateDeviceIdentifierResponse,

    -- * Response Lenses
    activateDeviceIdentifierResponse_tags,
    activateDeviceIdentifierResponse_httpStatus,
    activateDeviceIdentifierResponse_deviceIdentifier,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.PrivateNetworks.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newActivateDeviceIdentifier' smart constructor.
data ActivateDeviceIdentifier = ActivateDeviceIdentifier'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to ensure idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the device identifier.
    deviceIdentifierArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActivateDeviceIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'activateDeviceIdentifier_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to ensure idempotency>.
--
-- 'deviceIdentifierArn', 'activateDeviceIdentifier_deviceIdentifierArn' - The Amazon Resource Name (ARN) of the device identifier.
newActivateDeviceIdentifier ::
  -- | 'deviceIdentifierArn'
  Prelude.Text ->
  ActivateDeviceIdentifier
newActivateDeviceIdentifier pDeviceIdentifierArn_ =
  ActivateDeviceIdentifier'
    { clientToken =
        Prelude.Nothing,
      deviceIdentifierArn = pDeviceIdentifierArn_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to ensure idempotency>.
activateDeviceIdentifier_clientToken :: Lens.Lens' ActivateDeviceIdentifier (Prelude.Maybe Prelude.Text)
activateDeviceIdentifier_clientToken = Lens.lens (\ActivateDeviceIdentifier' {clientToken} -> clientToken) (\s@ActivateDeviceIdentifier' {} a -> s {clientToken = a} :: ActivateDeviceIdentifier)

-- | The Amazon Resource Name (ARN) of the device identifier.
activateDeviceIdentifier_deviceIdentifierArn :: Lens.Lens' ActivateDeviceIdentifier Prelude.Text
activateDeviceIdentifier_deviceIdentifierArn = Lens.lens (\ActivateDeviceIdentifier' {deviceIdentifierArn} -> deviceIdentifierArn) (\s@ActivateDeviceIdentifier' {} a -> s {deviceIdentifierArn = a} :: ActivateDeviceIdentifier)

instance Core.AWSRequest ActivateDeviceIdentifier where
  type
    AWSResponse ActivateDeviceIdentifier =
      ActivateDeviceIdentifierResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ActivateDeviceIdentifierResponse'
            Prelude.<$> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "deviceIdentifier")
      )

instance Prelude.Hashable ActivateDeviceIdentifier where
  hashWithSalt _salt ActivateDeviceIdentifier' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` deviceIdentifierArn

instance Prelude.NFData ActivateDeviceIdentifier where
  rnf ActivateDeviceIdentifier' {..} =
    Prelude.rnf clientToken `Prelude.seq`
      Prelude.rnf deviceIdentifierArn

instance Data.ToHeaders ActivateDeviceIdentifier where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ActivateDeviceIdentifier where
  toJSON ActivateDeviceIdentifier' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just
              ("deviceIdentifierArn" Data..= deviceIdentifierArn)
          ]
      )

instance Data.ToPath ActivateDeviceIdentifier where
  toPath =
    Prelude.const "/v1/device-identifiers/activate"

instance Data.ToQuery ActivateDeviceIdentifier where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newActivateDeviceIdentifierResponse' smart constructor.
data ActivateDeviceIdentifierResponse = ActivateDeviceIdentifierResponse'
  { -- | The tags on the device identifier.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about the device identifier.
    deviceIdentifier :: DeviceIdentifier
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActivateDeviceIdentifierResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'activateDeviceIdentifierResponse_tags' - The tags on the device identifier.
--
-- 'httpStatus', 'activateDeviceIdentifierResponse_httpStatus' - The response's http status code.
--
-- 'deviceIdentifier', 'activateDeviceIdentifierResponse_deviceIdentifier' - Information about the device identifier.
newActivateDeviceIdentifierResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'deviceIdentifier'
  DeviceIdentifier ->
  ActivateDeviceIdentifierResponse
newActivateDeviceIdentifierResponse
  pHttpStatus_
  pDeviceIdentifier_ =
    ActivateDeviceIdentifierResponse'
      { tags =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        deviceIdentifier = pDeviceIdentifier_
      }

-- | The tags on the device identifier.
activateDeviceIdentifierResponse_tags :: Lens.Lens' ActivateDeviceIdentifierResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
activateDeviceIdentifierResponse_tags = Lens.lens (\ActivateDeviceIdentifierResponse' {tags} -> tags) (\s@ActivateDeviceIdentifierResponse' {} a -> s {tags = a} :: ActivateDeviceIdentifierResponse) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The response's http status code.
activateDeviceIdentifierResponse_httpStatus :: Lens.Lens' ActivateDeviceIdentifierResponse Prelude.Int
activateDeviceIdentifierResponse_httpStatus = Lens.lens (\ActivateDeviceIdentifierResponse' {httpStatus} -> httpStatus) (\s@ActivateDeviceIdentifierResponse' {} a -> s {httpStatus = a} :: ActivateDeviceIdentifierResponse)

-- | Information about the device identifier.
activateDeviceIdentifierResponse_deviceIdentifier :: Lens.Lens' ActivateDeviceIdentifierResponse DeviceIdentifier
activateDeviceIdentifierResponse_deviceIdentifier = Lens.lens (\ActivateDeviceIdentifierResponse' {deviceIdentifier} -> deviceIdentifier) (\s@ActivateDeviceIdentifierResponse' {} a -> s {deviceIdentifier = a} :: ActivateDeviceIdentifierResponse)

instance
  Prelude.NFData
    ActivateDeviceIdentifierResponse
  where
  rnf ActivateDeviceIdentifierResponse' {..} =
    Prelude.rnf tags `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf deviceIdentifier
