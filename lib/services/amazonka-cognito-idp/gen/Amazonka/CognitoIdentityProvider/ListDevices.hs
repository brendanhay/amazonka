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
-- Module      : Amazonka.CognitoIdentityProvider.ListDevices
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the sign-in devices that Amazon Cognito has registered to the
-- current user.
module Amazonka.CognitoIdentityProvider.ListDevices
  ( -- * Creating a Request
    ListDevices (..),
    newListDevices,

    -- * Request Lenses
    listDevices_limit,
    listDevices_paginationToken,
    listDevices_accessToken,

    -- * Destructuring the Response
    ListDevicesResponse (..),
    newListDevicesResponse,

    -- * Response Lenses
    listDevicesResponse_devices,
    listDevicesResponse_paginationToken,
    listDevicesResponse_httpStatus,
  )
where

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the request to list the devices.
--
-- /See:/ 'newListDevices' smart constructor.
data ListDevices = ListDevices'
  { -- | The limit of the device request.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token for the list request.
    paginationToken :: Prelude.Maybe Prelude.Text,
    -- | A valid access token that Amazon Cognito issued to the user whose list
    -- of devices you want to view.
    accessToken :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDevices' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'listDevices_limit' - The limit of the device request.
--
-- 'paginationToken', 'listDevices_paginationToken' - The pagination token for the list request.
--
-- 'accessToken', 'listDevices_accessToken' - A valid access token that Amazon Cognito issued to the user whose list
-- of devices you want to view.
newListDevices ::
  -- | 'accessToken'
  Prelude.Text ->
  ListDevices
newListDevices pAccessToken_ =
  ListDevices'
    { limit = Prelude.Nothing,
      paginationToken = Prelude.Nothing,
      accessToken = Data._Sensitive Lens.# pAccessToken_
    }

-- | The limit of the device request.
listDevices_limit :: Lens.Lens' ListDevices (Prelude.Maybe Prelude.Natural)
listDevices_limit = Lens.lens (\ListDevices' {limit} -> limit) (\s@ListDevices' {} a -> s {limit = a} :: ListDevices)

-- | The pagination token for the list request.
listDevices_paginationToken :: Lens.Lens' ListDevices (Prelude.Maybe Prelude.Text)
listDevices_paginationToken = Lens.lens (\ListDevices' {paginationToken} -> paginationToken) (\s@ListDevices' {} a -> s {paginationToken = a} :: ListDevices)

-- | A valid access token that Amazon Cognito issued to the user whose list
-- of devices you want to view.
listDevices_accessToken :: Lens.Lens' ListDevices Prelude.Text
listDevices_accessToken = Lens.lens (\ListDevices' {accessToken} -> accessToken) (\s@ListDevices' {} a -> s {accessToken = a} :: ListDevices) Prelude.. Data._Sensitive

instance Core.AWSRequest ListDevices where
  type AWSResponse ListDevices = ListDevicesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDevicesResponse'
            Prelude.<$> (x Data..?> "Devices" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "PaginationToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDevices where
  hashWithSalt _salt ListDevices' {..} =
    _salt
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` paginationToken
      `Prelude.hashWithSalt` accessToken

instance Prelude.NFData ListDevices where
  rnf ListDevices' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf paginationToken
      `Prelude.seq` Prelude.rnf accessToken

instance Data.ToHeaders ListDevices where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.ListDevices" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListDevices where
  toJSON ListDevices' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Limit" Data..=) Prelude.<$> limit,
            ("PaginationToken" Data..=)
              Prelude.<$> paginationToken,
            Prelude.Just ("AccessToken" Data..= accessToken)
          ]
      )

instance Data.ToPath ListDevices where
  toPath = Prelude.const "/"

instance Data.ToQuery ListDevices where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the response to list devices.
--
-- /See:/ 'newListDevicesResponse' smart constructor.
data ListDevicesResponse = ListDevicesResponse'
  { -- | The devices returned in the list devices response.
    devices :: Prelude.Maybe [DeviceType],
    -- | The pagination token for the list device response.
    paginationToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDevicesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'devices', 'listDevicesResponse_devices' - The devices returned in the list devices response.
--
-- 'paginationToken', 'listDevicesResponse_paginationToken' - The pagination token for the list device response.
--
-- 'httpStatus', 'listDevicesResponse_httpStatus' - The response's http status code.
newListDevicesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDevicesResponse
newListDevicesResponse pHttpStatus_ =
  ListDevicesResponse'
    { devices = Prelude.Nothing,
      paginationToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The devices returned in the list devices response.
listDevicesResponse_devices :: Lens.Lens' ListDevicesResponse (Prelude.Maybe [DeviceType])
listDevicesResponse_devices = Lens.lens (\ListDevicesResponse' {devices} -> devices) (\s@ListDevicesResponse' {} a -> s {devices = a} :: ListDevicesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token for the list device response.
listDevicesResponse_paginationToken :: Lens.Lens' ListDevicesResponse (Prelude.Maybe Prelude.Text)
listDevicesResponse_paginationToken = Lens.lens (\ListDevicesResponse' {paginationToken} -> paginationToken) (\s@ListDevicesResponse' {} a -> s {paginationToken = a} :: ListDevicesResponse)

-- | The response's http status code.
listDevicesResponse_httpStatus :: Lens.Lens' ListDevicesResponse Prelude.Int
listDevicesResponse_httpStatus = Lens.lens (\ListDevicesResponse' {httpStatus} -> httpStatus) (\s@ListDevicesResponse' {} a -> s {httpStatus = a} :: ListDevicesResponse)

instance Prelude.NFData ListDevicesResponse where
  rnf ListDevicesResponse' {..} =
    Prelude.rnf devices
      `Prelude.seq` Prelude.rnf paginationToken
      `Prelude.seq` Prelude.rnf httpStatus
