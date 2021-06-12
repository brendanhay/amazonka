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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to list the devices.
--
-- /See:/ 'newListDevices' smart constructor.
data ListDevices = ListDevices'
  { -- | The pagination token for the list request.
    paginationToken :: Core.Maybe Core.Text,
    -- | The limit of the device request.
    limit :: Core.Maybe Core.Natural,
    -- | The access tokens for the request to list devices.
    accessToken :: Core.Sensitive Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
  Core.Text ->
  ListDevices
newListDevices pAccessToken_ =
  ListDevices'
    { paginationToken = Core.Nothing,
      limit = Core.Nothing,
      accessToken = Core._Sensitive Lens.# pAccessToken_
    }

-- | The pagination token for the list request.
listDevices_paginationToken :: Lens.Lens' ListDevices (Core.Maybe Core.Text)
listDevices_paginationToken = Lens.lens (\ListDevices' {paginationToken} -> paginationToken) (\s@ListDevices' {} a -> s {paginationToken = a} :: ListDevices)

-- | The limit of the device request.
listDevices_limit :: Lens.Lens' ListDevices (Core.Maybe Core.Natural)
listDevices_limit = Lens.lens (\ListDevices' {limit} -> limit) (\s@ListDevices' {} a -> s {limit = a} :: ListDevices)

-- | The access tokens for the request to list devices.
listDevices_accessToken :: Lens.Lens' ListDevices Core.Text
listDevices_accessToken = Lens.lens (\ListDevices' {accessToken} -> accessToken) (\s@ListDevices' {} a -> s {accessToken = a} :: ListDevices) Core.. Core._Sensitive

instance Core.AWSRequest ListDevices where
  type AWSResponse ListDevices = ListDevicesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDevicesResponse'
            Core.<$> (x Core..?> "Devices" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "PaginationToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListDevices

instance Core.NFData ListDevices

instance Core.ToHeaders ListDevices where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.ListDevices" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListDevices where
  toJSON ListDevices' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PaginationToken" Core..=)
              Core.<$> paginationToken,
            ("Limit" Core..=) Core.<$> limit,
            Core.Just ("AccessToken" Core..= accessToken)
          ]
      )

instance Core.ToPath ListDevices where
  toPath = Core.const "/"

instance Core.ToQuery ListDevices where
  toQuery = Core.const Core.mempty

-- | Represents the response to list devices.
--
-- /See:/ 'newListDevicesResponse' smart constructor.
data ListDevicesResponse = ListDevicesResponse'
  { -- | The devices returned in the list devices response.
    devices :: Core.Maybe [DeviceType],
    -- | The pagination token for the list device response.
    paginationToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
  Core.Int ->
  ListDevicesResponse
newListDevicesResponse pHttpStatus_ =
  ListDevicesResponse'
    { devices = Core.Nothing,
      paginationToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The devices returned in the list devices response.
listDevicesResponse_devices :: Lens.Lens' ListDevicesResponse (Core.Maybe [DeviceType])
listDevicesResponse_devices = Lens.lens (\ListDevicesResponse' {devices} -> devices) (\s@ListDevicesResponse' {} a -> s {devices = a} :: ListDevicesResponse) Core.. Lens.mapping Lens._Coerce

-- | The pagination token for the list device response.
listDevicesResponse_paginationToken :: Lens.Lens' ListDevicesResponse (Core.Maybe Core.Text)
listDevicesResponse_paginationToken = Lens.lens (\ListDevicesResponse' {paginationToken} -> paginationToken) (\s@ListDevicesResponse' {} a -> s {paginationToken = a} :: ListDevicesResponse)

-- | The response's http status code.
listDevicesResponse_httpStatus :: Lens.Lens' ListDevicesResponse Core.Int
listDevicesResponse_httpStatus = Lens.lens (\ListDevicesResponse' {httpStatus} -> httpStatus) (\s@ListDevicesResponse' {} a -> s {httpStatus = a} :: ListDevicesResponse)

instance Core.NFData ListDevicesResponse
