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
-- Module      : Amazonka.ResourceGroups.GetAccountSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current status of optional features in Resource Groups.
module Amazonka.ResourceGroups.GetAccountSettings
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResourceGroups.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAccountSettings' smart constructor.
data GetAccountSettings = GetAccountSettings'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAccountSettingsResponse'
            Prelude.<$> (x Data..?> "AccountSettings")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAccountSettings where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetAccountSettings where
  rnf _ = ()

instance Data.ToHeaders GetAccountSettings where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON GetAccountSettings where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath GetAccountSettings where
  toPath = Prelude.const "/get-account-settings"

instance Data.ToQuery GetAccountSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAccountSettingsResponse' smart constructor.
data GetAccountSettingsResponse = GetAccountSettingsResponse'
  { -- | The current settings for the optional features in Resource Groups.
    accountSettings :: Prelude.Maybe AccountSettings,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAccountSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountSettings', 'getAccountSettingsResponse_accountSettings' - The current settings for the optional features in Resource Groups.
--
-- 'httpStatus', 'getAccountSettingsResponse_httpStatus' - The response's http status code.
newGetAccountSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAccountSettingsResponse
newGetAccountSettingsResponse pHttpStatus_ =
  GetAccountSettingsResponse'
    { accountSettings =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current settings for the optional features in Resource Groups.
getAccountSettingsResponse_accountSettings :: Lens.Lens' GetAccountSettingsResponse (Prelude.Maybe AccountSettings)
getAccountSettingsResponse_accountSettings = Lens.lens (\GetAccountSettingsResponse' {accountSettings} -> accountSettings) (\s@GetAccountSettingsResponse' {} a -> s {accountSettings = a} :: GetAccountSettingsResponse)

-- | The response's http status code.
getAccountSettingsResponse_httpStatus :: Lens.Lens' GetAccountSettingsResponse Prelude.Int
getAccountSettingsResponse_httpStatus = Lens.lens (\GetAccountSettingsResponse' {httpStatus} -> httpStatus) (\s@GetAccountSettingsResponse' {} a -> s {httpStatus = a} :: GetAccountSettingsResponse)

instance Prelude.NFData GetAccountSettingsResponse where
  rnf GetAccountSettingsResponse' {..} =
    Prelude.rnf accountSettings
      `Prelude.seq` Prelude.rnf httpStatus
