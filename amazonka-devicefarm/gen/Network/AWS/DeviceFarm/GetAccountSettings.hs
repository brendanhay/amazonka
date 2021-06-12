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
-- Module      : Network.AWS.DeviceFarm.GetAccountSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the number of unmetered iOS or unmetered Android devices that
-- have been purchased by the account.
module Network.AWS.DeviceFarm.GetAccountSettings
  ( -- * Creating a Request
    GetAccountSettings (..),
    newGetAccountSettings,

    -- * Destructuring the Response
    GetAccountSettingsResponse (..),
    newGetAccountSettingsResponse,

    -- * Response Lenses
    getAccountSettingsResponse_accountSettings,
    getAccountSettingsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request sent to retrieve the account settings.
--
-- /See:/ 'newGetAccountSettings' smart constructor.
data GetAccountSettings = GetAccountSettings'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAccountSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetAccountSettings ::
  GetAccountSettings
newGetAccountSettings = GetAccountSettings'

instance Core.AWSRequest GetAccountSettings where
  type
    AWSResponse GetAccountSettings =
      GetAccountSettingsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAccountSettingsResponse'
            Core.<$> (x Core..?> "accountSettings")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetAccountSettings

instance Core.NFData GetAccountSettings

instance Core.ToHeaders GetAccountSettings where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.GetAccountSettings" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetAccountSettings where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath GetAccountSettings where
  toPath = Core.const "/"

instance Core.ToQuery GetAccountSettings where
  toQuery = Core.const Core.mempty

-- | Represents the account settings return values from the
-- @GetAccountSettings@ request.
--
-- /See:/ 'newGetAccountSettingsResponse' smart constructor.
data GetAccountSettingsResponse = GetAccountSettingsResponse'
  { -- | The account settings.
    accountSettings :: Core.Maybe AccountSettings,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAccountSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountSettings', 'getAccountSettingsResponse_accountSettings' - The account settings.
--
-- 'httpStatus', 'getAccountSettingsResponse_httpStatus' - The response's http status code.
newGetAccountSettingsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetAccountSettingsResponse
newGetAccountSettingsResponse pHttpStatus_ =
  GetAccountSettingsResponse'
    { accountSettings =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The account settings.
getAccountSettingsResponse_accountSettings :: Lens.Lens' GetAccountSettingsResponse (Core.Maybe AccountSettings)
getAccountSettingsResponse_accountSettings = Lens.lens (\GetAccountSettingsResponse' {accountSettings} -> accountSettings) (\s@GetAccountSettingsResponse' {} a -> s {accountSettings = a} :: GetAccountSettingsResponse)

-- | The response's http status code.
getAccountSettingsResponse_httpStatus :: Lens.Lens' GetAccountSettingsResponse Core.Int
getAccountSettingsResponse_httpStatus = Lens.lens (\GetAccountSettingsResponse' {httpStatus} -> httpStatus) (\s@GetAccountSettingsResponse' {} a -> s {httpStatus = a} :: GetAccountSettingsResponse)

instance Core.NFData GetAccountSettingsResponse
