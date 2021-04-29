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
-- Module      : Network.AWS.CognitoIdentityProvider.ListDevices
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the devices.
module Network.AWS.CognitoIdentityProvider.ListDevices
  ( -- * Creating a Request
    ListDevices (..),
    newListDevices,

    -- * Request Lenses
    listDevices_paginationToken,
    listDevices_limit,
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

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to list the devices.
--
-- /See:/ 'newListDevices' smart constructor.
data ListDevices = ListDevices'
  { -- | The pagination token for the list request.
    paginationToken :: Prelude.Maybe Prelude.Text,
    -- | The limit of the device request.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The access tokens for the request to list devices.
    accessToken :: Prelude.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListDevices' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'paginationToken', 'listDevices_paginationToken' - The pagination token for the list request.
--
-- 'limit', 'listDevices_limit' - The limit of the device request.
--
-- 'accessToken', 'listDevices_accessToken' - The access tokens for the request to list devices.
newListDevices ::
  -- | 'accessToken'
  Prelude.Text ->
  ListDevices
newListDevices pAccessToken_ =
  ListDevices'
    { paginationToken = Prelude.Nothing,
      limit = Prelude.Nothing,
      accessToken =
        Prelude._Sensitive Lens.# pAccessToken_
    }

-- | The pagination token for the list request.
listDevices_paginationToken :: Lens.Lens' ListDevices (Prelude.Maybe Prelude.Text)
listDevices_paginationToken = Lens.lens (\ListDevices' {paginationToken} -> paginationToken) (\s@ListDevices' {} a -> s {paginationToken = a} :: ListDevices)

-- | The limit of the device request.
listDevices_limit :: Lens.Lens' ListDevices (Prelude.Maybe Prelude.Natural)
listDevices_limit = Lens.lens (\ListDevices' {limit} -> limit) (\s@ListDevices' {} a -> s {limit = a} :: ListDevices)

-- | The access tokens for the request to list devices.
listDevices_accessToken :: Lens.Lens' ListDevices Prelude.Text
listDevices_accessToken = Lens.lens (\ListDevices' {accessToken} -> accessToken) (\s@ListDevices' {} a -> s {accessToken = a} :: ListDevices) Prelude.. Prelude._Sensitive

instance Prelude.AWSRequest ListDevices where
  type Rs ListDevices = ListDevicesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDevicesResponse'
            Prelude.<$> (x Prelude..?> "Devices" Prelude..!@ Prelude.mempty)
            Prelude.<*> (x Prelude..?> "PaginationToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDevices

instance Prelude.NFData ListDevices

instance Prelude.ToHeaders ListDevices where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCognitoIdentityProviderService.ListDevices" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListDevices where
  toJSON ListDevices' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("PaginationToken" Prelude..=)
              Prelude.<$> paginationToken,
            ("Limit" Prelude..=) Prelude.<$> limit,
            Prelude.Just ("AccessToken" Prelude..= accessToken)
          ]
      )

instance Prelude.ToPath ListDevices where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListDevices where
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
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
listDevicesResponse_devices = Lens.lens (\ListDevicesResponse' {devices} -> devices) (\s@ListDevicesResponse' {} a -> s {devices = a} :: ListDevicesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The pagination token for the list device response.
listDevicesResponse_paginationToken :: Lens.Lens' ListDevicesResponse (Prelude.Maybe Prelude.Text)
listDevicesResponse_paginationToken = Lens.lens (\ListDevicesResponse' {paginationToken} -> paginationToken) (\s@ListDevicesResponse' {} a -> s {paginationToken = a} :: ListDevicesResponse)

-- | The response's http status code.
listDevicesResponse_httpStatus :: Lens.Lens' ListDevicesResponse Prelude.Int
listDevicesResponse_httpStatus = Lens.lens (\ListDevicesResponse' {httpStatus} -> httpStatus) (\s@ListDevicesResponse' {} a -> s {httpStatus = a} :: ListDevicesResponse)

instance Prelude.NFData ListDevicesResponse
