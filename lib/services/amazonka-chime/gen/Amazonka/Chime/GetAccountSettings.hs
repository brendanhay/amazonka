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
-- Module      : Amazonka.Chime.GetAccountSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves account settings for the specified Amazon Chime account ID,
-- such as remote control and dialout settings. For more information about
-- these settings, see
-- <https://docs.aws.amazon.com/chime/latest/ag/policies.html Use the Policies Page>
-- in the /Amazon Chime Administration Guide/.
module Amazonka.Chime.GetAccountSettings
  ( -- * Creating a Request
    GetAccountSettings (..),
    newGetAccountSettings,

    -- * Request Lenses
    getAccountSettings_accountId,

    -- * Destructuring the Response
    GetAccountSettingsResponse (..),
    newGetAccountSettingsResponse,

    -- * Response Lenses
    getAccountSettingsResponse_accountSettings,
    getAccountSettingsResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAccountSettings' smart constructor.
data GetAccountSettings = GetAccountSettings'
  { -- | The Amazon Chime account ID.
    accountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAccountSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'getAccountSettings_accountId' - The Amazon Chime account ID.
newGetAccountSettings ::
  -- | 'accountId'
  Prelude.Text ->
  GetAccountSettings
newGetAccountSettings pAccountId_ =
  GetAccountSettings' {accountId = pAccountId_}

-- | The Amazon Chime account ID.
getAccountSettings_accountId :: Lens.Lens' GetAccountSettings Prelude.Text
getAccountSettings_accountId = Lens.lens (\GetAccountSettings' {accountId} -> accountId) (\s@GetAccountSettings' {} a -> s {accountId = a} :: GetAccountSettings)

instance Core.AWSRequest GetAccountSettings where
  type
    AWSResponse GetAccountSettings =
      GetAccountSettingsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAccountSettingsResponse'
            Prelude.<$> (x Data..?> "AccountSettings")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAccountSettings where
  hashWithSalt _salt GetAccountSettings' {..} =
    _salt `Prelude.hashWithSalt` accountId

instance Prelude.NFData GetAccountSettings where
  rnf GetAccountSettings' {..} = Prelude.rnf accountId

instance Data.ToHeaders GetAccountSettings where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetAccountSettings where
  toPath GetAccountSettings' {..} =
    Prelude.mconcat
      ["/accounts/", Data.toBS accountId, "/settings"]

instance Data.ToQuery GetAccountSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAccountSettingsResponse' smart constructor.
data GetAccountSettingsResponse = GetAccountSettingsResponse'
  { -- | The Amazon Chime account settings.
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
-- 'accountSettings', 'getAccountSettingsResponse_accountSettings' - The Amazon Chime account settings.
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

-- | The Amazon Chime account settings.
getAccountSettingsResponse_accountSettings :: Lens.Lens' GetAccountSettingsResponse (Prelude.Maybe AccountSettings)
getAccountSettingsResponse_accountSettings = Lens.lens (\GetAccountSettingsResponse' {accountSettings} -> accountSettings) (\s@GetAccountSettingsResponse' {} a -> s {accountSettings = a} :: GetAccountSettingsResponse)

-- | The response's http status code.
getAccountSettingsResponse_httpStatus :: Lens.Lens' GetAccountSettingsResponse Prelude.Int
getAccountSettingsResponse_httpStatus = Lens.lens (\GetAccountSettingsResponse' {httpStatus} -> httpStatus) (\s@GetAccountSettingsResponse' {} a -> s {httpStatus = a} :: GetAccountSettingsResponse)

instance Prelude.NFData GetAccountSettingsResponse where
  rnf GetAccountSettingsResponse' {..} =
    Prelude.rnf accountSettings
      `Prelude.seq` Prelude.rnf httpStatus
