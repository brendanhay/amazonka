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
-- Module      : Amazonka.Chime.GetUserSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves settings for the specified user ID, such as any associated
-- phone number settings.
module Amazonka.Chime.GetUserSettings
  ( -- * Creating a Request
    GetUserSettings (..),
    newGetUserSettings,

    -- * Request Lenses
    getUserSettings_accountId,
    getUserSettings_userId,

    -- * Destructuring the Response
    GetUserSettingsResponse (..),
    newGetUserSettingsResponse,

    -- * Response Lenses
    getUserSettingsResponse_userSettings,
    getUserSettingsResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetUserSettings' smart constructor.
data GetUserSettings = GetUserSettings'
  { -- | The Amazon Chime account ID.
    accountId :: Prelude.Text,
    -- | The user ID.
    userId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetUserSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'getUserSettings_accountId' - The Amazon Chime account ID.
--
-- 'userId', 'getUserSettings_userId' - The user ID.
newGetUserSettings ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'userId'
  Prelude.Text ->
  GetUserSettings
newGetUserSettings pAccountId_ pUserId_ =
  GetUserSettings'
    { accountId = pAccountId_,
      userId = pUserId_
    }

-- | The Amazon Chime account ID.
getUserSettings_accountId :: Lens.Lens' GetUserSettings Prelude.Text
getUserSettings_accountId = Lens.lens (\GetUserSettings' {accountId} -> accountId) (\s@GetUserSettings' {} a -> s {accountId = a} :: GetUserSettings)

-- | The user ID.
getUserSettings_userId :: Lens.Lens' GetUserSettings Prelude.Text
getUserSettings_userId = Lens.lens (\GetUserSettings' {userId} -> userId) (\s@GetUserSettings' {} a -> s {userId = a} :: GetUserSettings)

instance Core.AWSRequest GetUserSettings where
  type
    AWSResponse GetUserSettings =
      GetUserSettingsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetUserSettingsResponse'
            Prelude.<$> (x Data..?> "UserSettings")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetUserSettings where
  hashWithSalt _salt GetUserSettings' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` userId

instance Prelude.NFData GetUserSettings where
  rnf GetUserSettings' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf userId

instance Data.ToHeaders GetUserSettings where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetUserSettings where
  toPath GetUserSettings' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS accountId,
        "/users/",
        Data.toBS userId,
        "/settings"
      ]

instance Data.ToQuery GetUserSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetUserSettingsResponse' smart constructor.
data GetUserSettingsResponse = GetUserSettingsResponse'
  { -- | The user settings.
    userSettings :: Prelude.Maybe UserSettings,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetUserSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userSettings', 'getUserSettingsResponse_userSettings' - The user settings.
--
-- 'httpStatus', 'getUserSettingsResponse_httpStatus' - The response's http status code.
newGetUserSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetUserSettingsResponse
newGetUserSettingsResponse pHttpStatus_ =
  GetUserSettingsResponse'
    { userSettings =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The user settings.
getUserSettingsResponse_userSettings :: Lens.Lens' GetUserSettingsResponse (Prelude.Maybe UserSettings)
getUserSettingsResponse_userSettings = Lens.lens (\GetUserSettingsResponse' {userSettings} -> userSettings) (\s@GetUserSettingsResponse' {} a -> s {userSettings = a} :: GetUserSettingsResponse)

-- | The response's http status code.
getUserSettingsResponse_httpStatus :: Lens.Lens' GetUserSettingsResponse Prelude.Int
getUserSettingsResponse_httpStatus = Lens.lens (\GetUserSettingsResponse' {httpStatus} -> httpStatus) (\s@GetUserSettingsResponse' {} a -> s {httpStatus = a} :: GetUserSettingsResponse)

instance Prelude.NFData GetUserSettingsResponse where
  rnf GetUserSettingsResponse' {..} =
    Prelude.rnf userSettings
      `Prelude.seq` Prelude.rnf httpStatus
