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
-- Module      : Amazonka.PrivateNetworks.GetDeviceIdentifier
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the specified device identifier.
module Amazonka.PrivateNetworks.GetDeviceIdentifier
  ( -- * Creating a Request
    GetDeviceIdentifier (..),
    newGetDeviceIdentifier,

    -- * Request Lenses
    getDeviceIdentifier_deviceIdentifierArn,

    -- * Destructuring the Response
    GetDeviceIdentifierResponse (..),
    newGetDeviceIdentifierResponse,

    -- * Response Lenses
    getDeviceIdentifierResponse_tags,
    getDeviceIdentifierResponse_deviceIdentifier,
    getDeviceIdentifierResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.PrivateNetworks.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDeviceIdentifier' smart constructor.
data GetDeviceIdentifier = GetDeviceIdentifier'
  { -- | The Amazon Resource Name (ARN) of the device identifier.
    deviceIdentifierArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDeviceIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceIdentifierArn', 'getDeviceIdentifier_deviceIdentifierArn' - The Amazon Resource Name (ARN) of the device identifier.
newGetDeviceIdentifier ::
  -- | 'deviceIdentifierArn'
  Prelude.Text ->
  GetDeviceIdentifier
newGetDeviceIdentifier pDeviceIdentifierArn_ =
  GetDeviceIdentifier'
    { deviceIdentifierArn =
        pDeviceIdentifierArn_
    }

-- | The Amazon Resource Name (ARN) of the device identifier.
getDeviceIdentifier_deviceIdentifierArn :: Lens.Lens' GetDeviceIdentifier Prelude.Text
getDeviceIdentifier_deviceIdentifierArn = Lens.lens (\GetDeviceIdentifier' {deviceIdentifierArn} -> deviceIdentifierArn) (\s@GetDeviceIdentifier' {} a -> s {deviceIdentifierArn = a} :: GetDeviceIdentifier)

instance Core.AWSRequest GetDeviceIdentifier where
  type
    AWSResponse GetDeviceIdentifier =
      GetDeviceIdentifierResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDeviceIdentifierResponse'
            Prelude.<$> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "deviceIdentifier")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDeviceIdentifier where
  hashWithSalt _salt GetDeviceIdentifier' {..} =
    _salt `Prelude.hashWithSalt` deviceIdentifierArn

instance Prelude.NFData GetDeviceIdentifier where
  rnf GetDeviceIdentifier' {..} =
    Prelude.rnf deviceIdentifierArn

instance Data.ToHeaders GetDeviceIdentifier where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetDeviceIdentifier where
  toPath GetDeviceIdentifier' {..} =
    Prelude.mconcat
      [ "/v1/device-identifiers/",
        Data.toBS deviceIdentifierArn
      ]

instance Data.ToQuery GetDeviceIdentifier where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDeviceIdentifierResponse' smart constructor.
data GetDeviceIdentifierResponse = GetDeviceIdentifierResponse'
  { -- | The device identifier tags.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | Information about the device identifier.
    deviceIdentifier :: Prelude.Maybe DeviceIdentifier,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDeviceIdentifierResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'getDeviceIdentifierResponse_tags' - The device identifier tags.
--
-- 'deviceIdentifier', 'getDeviceIdentifierResponse_deviceIdentifier' - Information about the device identifier.
--
-- 'httpStatus', 'getDeviceIdentifierResponse_httpStatus' - The response's http status code.
newGetDeviceIdentifierResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDeviceIdentifierResponse
newGetDeviceIdentifierResponse pHttpStatus_ =
  GetDeviceIdentifierResponse'
    { tags =
        Prelude.Nothing,
      deviceIdentifier = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The device identifier tags.
getDeviceIdentifierResponse_tags :: Lens.Lens' GetDeviceIdentifierResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getDeviceIdentifierResponse_tags = Lens.lens (\GetDeviceIdentifierResponse' {tags} -> tags) (\s@GetDeviceIdentifierResponse' {} a -> s {tags = a} :: GetDeviceIdentifierResponse) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | Information about the device identifier.
getDeviceIdentifierResponse_deviceIdentifier :: Lens.Lens' GetDeviceIdentifierResponse (Prelude.Maybe DeviceIdentifier)
getDeviceIdentifierResponse_deviceIdentifier = Lens.lens (\GetDeviceIdentifierResponse' {deviceIdentifier} -> deviceIdentifier) (\s@GetDeviceIdentifierResponse' {} a -> s {deviceIdentifier = a} :: GetDeviceIdentifierResponse)

-- | The response's http status code.
getDeviceIdentifierResponse_httpStatus :: Lens.Lens' GetDeviceIdentifierResponse Prelude.Int
getDeviceIdentifierResponse_httpStatus = Lens.lens (\GetDeviceIdentifierResponse' {httpStatus} -> httpStatus) (\s@GetDeviceIdentifierResponse' {} a -> s {httpStatus = a} :: GetDeviceIdentifierResponse)

instance Prelude.NFData GetDeviceIdentifierResponse where
  rnf GetDeviceIdentifierResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf deviceIdentifier
      `Prelude.seq` Prelude.rnf httpStatus
