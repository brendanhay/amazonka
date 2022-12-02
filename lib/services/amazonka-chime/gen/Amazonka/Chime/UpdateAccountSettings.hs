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
-- Module      : Amazonka.Chime.UpdateAccountSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the settings for the specified Amazon Chime account. You can
-- update settings for remote control of shared screens, or for the
-- dial-out option. For more information about these settings, see
-- <https://docs.aws.amazon.com/chime/latest/ag/policies.html Use the Policies Page>
-- in the /Amazon Chime Administration Guide/.
module Amazonka.Chime.UpdateAccountSettings
  ( -- * Creating a Request
    UpdateAccountSettings (..),
    newUpdateAccountSettings,

    -- * Request Lenses
    updateAccountSettings_accountId,
    updateAccountSettings_accountSettings,

    -- * Destructuring the Response
    UpdateAccountSettingsResponse (..),
    newUpdateAccountSettingsResponse,

    -- * Response Lenses
    updateAccountSettingsResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateAccountSettings' smart constructor.
data UpdateAccountSettings = UpdateAccountSettings'
  { -- | The Amazon Chime account ID.
    accountId :: Prelude.Text,
    -- | The Amazon Chime account settings to update.
    accountSettings :: AccountSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAccountSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'updateAccountSettings_accountId' - The Amazon Chime account ID.
--
-- 'accountSettings', 'updateAccountSettings_accountSettings' - The Amazon Chime account settings to update.
newUpdateAccountSettings ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'accountSettings'
  AccountSettings ->
  UpdateAccountSettings
newUpdateAccountSettings
  pAccountId_
  pAccountSettings_ =
    UpdateAccountSettings'
      { accountId = pAccountId_,
        accountSettings = pAccountSettings_
      }

-- | The Amazon Chime account ID.
updateAccountSettings_accountId :: Lens.Lens' UpdateAccountSettings Prelude.Text
updateAccountSettings_accountId = Lens.lens (\UpdateAccountSettings' {accountId} -> accountId) (\s@UpdateAccountSettings' {} a -> s {accountId = a} :: UpdateAccountSettings)

-- | The Amazon Chime account settings to update.
updateAccountSettings_accountSettings :: Lens.Lens' UpdateAccountSettings AccountSettings
updateAccountSettings_accountSettings = Lens.lens (\UpdateAccountSettings' {accountSettings} -> accountSettings) (\s@UpdateAccountSettings' {} a -> s {accountSettings = a} :: UpdateAccountSettings)

instance Core.AWSRequest UpdateAccountSettings where
  type
    AWSResponse UpdateAccountSettings =
      UpdateAccountSettingsResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateAccountSettingsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateAccountSettings where
  hashWithSalt _salt UpdateAccountSettings' {..} =
    _salt `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` accountSettings

instance Prelude.NFData UpdateAccountSettings where
  rnf UpdateAccountSettings' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf accountSettings

instance Data.ToHeaders UpdateAccountSettings where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateAccountSettings where
  toJSON UpdateAccountSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AccountSettings" Data..= accountSettings)
          ]
      )

instance Data.ToPath UpdateAccountSettings where
  toPath UpdateAccountSettings' {..} =
    Prelude.mconcat
      ["/accounts/", Data.toBS accountId, "/settings"]

instance Data.ToQuery UpdateAccountSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAccountSettingsResponse' smart constructor.
data UpdateAccountSettingsResponse = UpdateAccountSettingsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAccountSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateAccountSettingsResponse_httpStatus' - The response's http status code.
newUpdateAccountSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateAccountSettingsResponse
newUpdateAccountSettingsResponse pHttpStatus_ =
  UpdateAccountSettingsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateAccountSettingsResponse_httpStatus :: Lens.Lens' UpdateAccountSettingsResponse Prelude.Int
updateAccountSettingsResponse_httpStatus = Lens.lens (\UpdateAccountSettingsResponse' {httpStatus} -> httpStatus) (\s@UpdateAccountSettingsResponse' {} a -> s {httpStatus = a} :: UpdateAccountSettingsResponse)

instance Prelude.NFData UpdateAccountSettingsResponse where
  rnf UpdateAccountSettingsResponse' {..} =
    Prelude.rnf httpStatus
