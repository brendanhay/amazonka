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
-- Module      : Network.AWS.CognitoIdentityProvider.AdminSetUserSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- /This action is no longer supported./ You can use it to configure only
-- SMS MFA. You can\'t use it to configure TOTP software token MFA. To
-- configure either type of MFA, use
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminSetUserMFAPreference.html AdminSetUserMFAPreference>
-- instead.
module Network.AWS.CognitoIdentityProvider.AdminSetUserSettings
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

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | You can use this parameter to set an MFA configuration that uses the SMS
-- delivery medium.
--
-- /See:/ 'newAdminSetUserSettings' smart constructor.
data AdminSetUserSettings = AdminSetUserSettings'
  { -- | The ID of the user pool that contains the user that you are setting
    -- options for.
    userPoolId :: Core.Text,
    -- | The user name of the user that you are setting options for.
    username :: Core.Sensitive Core.Text,
    -- | You can use this parameter only to set an SMS configuration that uses
    -- SMS for delivery.
    mfaOptions :: [MFAOptionType]
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'AdminSetUserSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userPoolId', 'adminSetUserSettings_userPoolId' - The ID of the user pool that contains the user that you are setting
-- options for.
--
-- 'username', 'adminSetUserSettings_username' - The user name of the user that you are setting options for.
--
-- 'mfaOptions', 'adminSetUserSettings_mfaOptions' - You can use this parameter only to set an SMS configuration that uses
-- SMS for delivery.
newAdminSetUserSettings ::
  -- | 'userPoolId'
  Core.Text ->
  -- | 'username'
  Core.Text ->
  AdminSetUserSettings
newAdminSetUserSettings pUserPoolId_ pUsername_ =
  AdminSetUserSettings'
    { userPoolId = pUserPoolId_,
      username = Core._Sensitive Lens.# pUsername_,
      mfaOptions = Core.mempty
    }

-- | The ID of the user pool that contains the user that you are setting
-- options for.
adminSetUserSettings_userPoolId :: Lens.Lens' AdminSetUserSettings Core.Text
adminSetUserSettings_userPoolId = Lens.lens (\AdminSetUserSettings' {userPoolId} -> userPoolId) (\s@AdminSetUserSettings' {} a -> s {userPoolId = a} :: AdminSetUserSettings)

-- | The user name of the user that you are setting options for.
adminSetUserSettings_username :: Lens.Lens' AdminSetUserSettings Core.Text
adminSetUserSettings_username = Lens.lens (\AdminSetUserSettings' {username} -> username) (\s@AdminSetUserSettings' {} a -> s {username = a} :: AdminSetUserSettings) Core.. Core._Sensitive

-- | You can use this parameter only to set an SMS configuration that uses
-- SMS for delivery.
adminSetUserSettings_mfaOptions :: Lens.Lens' AdminSetUserSettings [MFAOptionType]
adminSetUserSettings_mfaOptions = Lens.lens (\AdminSetUserSettings' {mfaOptions} -> mfaOptions) (\s@AdminSetUserSettings' {} a -> s {mfaOptions = a} :: AdminSetUserSettings) Core.. Lens._Coerce

instance Core.AWSRequest AdminSetUserSettings where
  type
    AWSResponse AdminSetUserSettings =
      AdminSetUserSettingsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AdminSetUserSettingsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AdminSetUserSettings

instance Core.NFData AdminSetUserSettings

instance Core.ToHeaders AdminSetUserSettings where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.AdminSetUserSettings" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AdminSetUserSettings where
  toJSON AdminSetUserSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("Username" Core..= username),
            Core.Just ("MFAOptions" Core..= mfaOptions)
          ]
      )

instance Core.ToPath AdminSetUserSettings where
  toPath = Core.const "/"

instance Core.ToQuery AdminSetUserSettings where
  toQuery = Core.const Core.mempty

-- | Represents the response from the server to set user settings as an
-- administrator.
--
-- /See:/ 'newAdminSetUserSettingsResponse' smart constructor.
data AdminSetUserSettingsResponse = AdminSetUserSettingsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  AdminSetUserSettingsResponse
newAdminSetUserSettingsResponse pHttpStatus_ =
  AdminSetUserSettingsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
adminSetUserSettingsResponse_httpStatus :: Lens.Lens' AdminSetUserSettingsResponse Core.Int
adminSetUserSettingsResponse_httpStatus = Lens.lens (\AdminSetUserSettingsResponse' {httpStatus} -> httpStatus) (\s@AdminSetUserSettingsResponse' {} a -> s {httpStatus = a} :: AdminSetUserSettingsResponse)

instance Core.NFData AdminSetUserSettingsResponse
