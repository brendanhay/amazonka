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
-- Module      : Amazonka.Chime.UpdateUserSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the settings for the specified user, such as phone number
-- settings.
module Amazonka.Chime.UpdateUserSettings
  ( -- * Creating a Request
    UpdateUserSettings (..),
    newUpdateUserSettings,

    -- * Request Lenses
    updateUserSettings_accountId,
    updateUserSettings_userId,
    updateUserSettings_userSettings,

    -- * Destructuring the Response
    UpdateUserSettingsResponse (..),
    newUpdateUserSettingsResponse,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateUserSettings' smart constructor.
data UpdateUserSettings = UpdateUserSettings'
  { -- | The Amazon Chime account ID.
    accountId :: Prelude.Text,
    -- | The user ID.
    userId :: Prelude.Text,
    -- | The user settings to update.
    userSettings :: UserSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateUserSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'updateUserSettings_accountId' - The Amazon Chime account ID.
--
-- 'userId', 'updateUserSettings_userId' - The user ID.
--
-- 'userSettings', 'updateUserSettings_userSettings' - The user settings to update.
newUpdateUserSettings ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'userId'
  Prelude.Text ->
  -- | 'userSettings'
  UserSettings ->
  UpdateUserSettings
newUpdateUserSettings
  pAccountId_
  pUserId_
  pUserSettings_ =
    UpdateUserSettings'
      { accountId = pAccountId_,
        userId = pUserId_,
        userSettings = pUserSettings_
      }

-- | The Amazon Chime account ID.
updateUserSettings_accountId :: Lens.Lens' UpdateUserSettings Prelude.Text
updateUserSettings_accountId = Lens.lens (\UpdateUserSettings' {accountId} -> accountId) (\s@UpdateUserSettings' {} a -> s {accountId = a} :: UpdateUserSettings)

-- | The user ID.
updateUserSettings_userId :: Lens.Lens' UpdateUserSettings Prelude.Text
updateUserSettings_userId = Lens.lens (\UpdateUserSettings' {userId} -> userId) (\s@UpdateUserSettings' {} a -> s {userId = a} :: UpdateUserSettings)

-- | The user settings to update.
updateUserSettings_userSettings :: Lens.Lens' UpdateUserSettings UserSettings
updateUserSettings_userSettings = Lens.lens (\UpdateUserSettings' {userSettings} -> userSettings) (\s@UpdateUserSettings' {} a -> s {userSettings = a} :: UpdateUserSettings)

instance Core.AWSRequest UpdateUserSettings where
  type
    AWSResponse UpdateUserSettings =
      UpdateUserSettingsResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveNull UpdateUserSettingsResponse'

instance Prelude.Hashable UpdateUserSettings where
  hashWithSalt _salt UpdateUserSettings' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` userSettings

instance Prelude.NFData UpdateUserSettings where
  rnf UpdateUserSettings' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf userId
      `Prelude.seq` Prelude.rnf userSettings

instance Data.ToHeaders UpdateUserSettings where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateUserSettings where
  toJSON UpdateUserSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("UserSettings" Data..= userSettings)]
      )

instance Data.ToPath UpdateUserSettings where
  toPath UpdateUserSettings' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS accountId,
        "/users/",
        Data.toBS userId,
        "/settings"
      ]

instance Data.ToQuery UpdateUserSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateUserSettingsResponse' smart constructor.
data UpdateUserSettingsResponse = UpdateUserSettingsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateUserSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateUserSettingsResponse ::
  UpdateUserSettingsResponse
newUpdateUserSettingsResponse =
  UpdateUserSettingsResponse'

instance Prelude.NFData UpdateUserSettingsResponse where
  rnf _ = ()
