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
-- Module      : Amazonka.CognitoIdentityProvider.AdminSetUserSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- /This action is no longer supported./ You can use it to configure only
-- SMS MFA. You can\'t use it to configure time-based one-time password
-- (TOTP) software token MFA. To configure either type of MFA, use
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminSetUserMFAPreference.html AdminSetUserMFAPreference>
-- instead.
module Amazonka.CognitoIdentityProvider.AdminSetUserSettings
  ( -- * Creating a Request
    AdminSetUserSettings (..),
    newAdminSetUserSettings,

    -- * Request Lenses
    adminSetUserSettings_userPoolId,
    adminSetUserSettings_username,
    adminSetUserSettings_mfaOptions,

    -- * Destructuring the Response
    AdminSetUserSettingsResponse (..),
    newAdminSetUserSettingsResponse,

    -- * Response Lenses
    adminSetUserSettingsResponse_httpStatus,
  )
where

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | You can use this parameter to set an MFA configuration that uses the SMS
-- delivery medium.
--
-- /See:/ 'newAdminSetUserSettings' smart constructor.
data AdminSetUserSettings = AdminSetUserSettings'
  { -- | The ID of the user pool that contains the user whose options you\'re
    -- setting.
    userPoolId :: Prelude.Text,
    -- | The user name of the user whose options you\'re setting.
    username :: Data.Sensitive Prelude.Text,
    -- | You can use this parameter only to set an SMS configuration that uses
    -- SMS for delivery.
    mfaOptions :: [MFAOptionType]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdminSetUserSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userPoolId', 'adminSetUserSettings_userPoolId' - The ID of the user pool that contains the user whose options you\'re
-- setting.
--
-- 'username', 'adminSetUserSettings_username' - The user name of the user whose options you\'re setting.
--
-- 'mfaOptions', 'adminSetUserSettings_mfaOptions' - You can use this parameter only to set an SMS configuration that uses
-- SMS for delivery.
newAdminSetUserSettings ::
  -- | 'userPoolId'
  Prelude.Text ->
  -- | 'username'
  Prelude.Text ->
  AdminSetUserSettings
newAdminSetUserSettings pUserPoolId_ pUsername_ =
  AdminSetUserSettings'
    { userPoolId = pUserPoolId_,
      username = Data._Sensitive Lens.# pUsername_,
      mfaOptions = Prelude.mempty
    }

-- | The ID of the user pool that contains the user whose options you\'re
-- setting.
adminSetUserSettings_userPoolId :: Lens.Lens' AdminSetUserSettings Prelude.Text
adminSetUserSettings_userPoolId = Lens.lens (\AdminSetUserSettings' {userPoolId} -> userPoolId) (\s@AdminSetUserSettings' {} a -> s {userPoolId = a} :: AdminSetUserSettings)

-- | The user name of the user whose options you\'re setting.
adminSetUserSettings_username :: Lens.Lens' AdminSetUserSettings Prelude.Text
adminSetUserSettings_username = Lens.lens (\AdminSetUserSettings' {username} -> username) (\s@AdminSetUserSettings' {} a -> s {username = a} :: AdminSetUserSettings) Prelude.. Data._Sensitive

-- | You can use this parameter only to set an SMS configuration that uses
-- SMS for delivery.
adminSetUserSettings_mfaOptions :: Lens.Lens' AdminSetUserSettings [MFAOptionType]
adminSetUserSettings_mfaOptions = Lens.lens (\AdminSetUserSettings' {mfaOptions} -> mfaOptions) (\s@AdminSetUserSettings' {} a -> s {mfaOptions = a} :: AdminSetUserSettings) Prelude.. Lens.coerced

instance Core.AWSRequest AdminSetUserSettings where
  type
    AWSResponse AdminSetUserSettings =
      AdminSetUserSettingsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AdminSetUserSettingsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AdminSetUserSettings where
  hashWithSalt _salt AdminSetUserSettings' {..} =
    _salt
      `Prelude.hashWithSalt` userPoolId
      `Prelude.hashWithSalt` username
      `Prelude.hashWithSalt` mfaOptions

instance Prelude.NFData AdminSetUserSettings where
  rnf AdminSetUserSettings' {..} =
    Prelude.rnf userPoolId
      `Prelude.seq` Prelude.rnf username
      `Prelude.seq` Prelude.rnf mfaOptions

instance Data.ToHeaders AdminSetUserSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.AdminSetUserSettings" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AdminSetUserSettings where
  toJSON AdminSetUserSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("UserPoolId" Data..= userPoolId),
            Prelude.Just ("Username" Data..= username),
            Prelude.Just ("MFAOptions" Data..= mfaOptions)
          ]
      )

instance Data.ToPath AdminSetUserSettings where
  toPath = Prelude.const "/"

instance Data.ToQuery AdminSetUserSettings where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the response from the server to set user settings as an
-- administrator.
--
-- /See:/ 'newAdminSetUserSettingsResponse' smart constructor.
data AdminSetUserSettingsResponse = AdminSetUserSettingsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdminSetUserSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'adminSetUserSettingsResponse_httpStatus' - The response's http status code.
newAdminSetUserSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AdminSetUserSettingsResponse
newAdminSetUserSettingsResponse pHttpStatus_ =
  AdminSetUserSettingsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
adminSetUserSettingsResponse_httpStatus :: Lens.Lens' AdminSetUserSettingsResponse Prelude.Int
adminSetUserSettingsResponse_httpStatus = Lens.lens (\AdminSetUserSettingsResponse' {httpStatus} -> httpStatus) (\s@AdminSetUserSettingsResponse' {} a -> s {httpStatus = a} :: AdminSetUserSettingsResponse)

instance Prelude.NFData AdminSetUserSettingsResponse where
  rnf AdminSetUserSettingsResponse' {..} =
    Prelude.rnf httpStatus
