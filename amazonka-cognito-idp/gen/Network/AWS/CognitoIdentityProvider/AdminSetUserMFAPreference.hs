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
-- Module      : Network.AWS.CognitoIdentityProvider.AdminSetUserMFAPreference
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the user\'s multi-factor authentication (MFA) preference, including
-- which MFA options are enabled and if any are preferred. Only one factor
-- can be set as preferred. The preferred MFA factor will be used to
-- authenticate a user if multiple factors are enabled. If multiple options
-- are enabled and no preference is set, a challenge to choose an MFA
-- option will be returned during sign in.
module Network.AWS.CognitoIdentityProvider.AdminSetUserMFAPreference
  ( -- * Creating a Request
    AdminSetUserMFAPreference (..),
    newAdminSetUserMFAPreference,

    -- * Request Lenses
    adminSetUserMFAPreference_softwareTokenMfaSettings,
    adminSetUserMFAPreference_sMSMfaSettings,
    adminSetUserMFAPreference_username,
    adminSetUserMFAPreference_userPoolId,

    -- * Destructuring the Response
    AdminSetUserMFAPreferenceResponse (..),
    newAdminSetUserMFAPreferenceResponse,

    -- * Response Lenses
    adminSetUserMFAPreferenceResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAdminSetUserMFAPreference' smart constructor.
data AdminSetUserMFAPreference = AdminSetUserMFAPreference'
  { -- | The time-based one-time password software token MFA settings.
    softwareTokenMfaSettings :: Prelude.Maybe SoftwareTokenMfaSettingsType,
    -- | The SMS text message MFA settings.
    sMSMfaSettings :: Prelude.Maybe SMSMfaSettingsType,
    -- | The user pool username or alias.
    username :: Prelude.Sensitive Prelude.Text,
    -- | The user pool ID.
    userPoolId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AdminSetUserMFAPreference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'softwareTokenMfaSettings', 'adminSetUserMFAPreference_softwareTokenMfaSettings' - The time-based one-time password software token MFA settings.
--
-- 'sMSMfaSettings', 'adminSetUserMFAPreference_sMSMfaSettings' - The SMS text message MFA settings.
--
-- 'username', 'adminSetUserMFAPreference_username' - The user pool username or alias.
--
-- 'userPoolId', 'adminSetUserMFAPreference_userPoolId' - The user pool ID.
newAdminSetUserMFAPreference ::
  -- | 'username'
  Prelude.Text ->
  -- | 'userPoolId'
  Prelude.Text ->
  AdminSetUserMFAPreference
newAdminSetUserMFAPreference pUsername_ pUserPoolId_ =
  AdminSetUserMFAPreference'
    { softwareTokenMfaSettings =
        Prelude.Nothing,
      sMSMfaSettings = Prelude.Nothing,
      username = Prelude._Sensitive Lens.# pUsername_,
      userPoolId = pUserPoolId_
    }

-- | The time-based one-time password software token MFA settings.
adminSetUserMFAPreference_softwareTokenMfaSettings :: Lens.Lens' AdminSetUserMFAPreference (Prelude.Maybe SoftwareTokenMfaSettingsType)
adminSetUserMFAPreference_softwareTokenMfaSettings = Lens.lens (\AdminSetUserMFAPreference' {softwareTokenMfaSettings} -> softwareTokenMfaSettings) (\s@AdminSetUserMFAPreference' {} a -> s {softwareTokenMfaSettings = a} :: AdminSetUserMFAPreference)

-- | The SMS text message MFA settings.
adminSetUserMFAPreference_sMSMfaSettings :: Lens.Lens' AdminSetUserMFAPreference (Prelude.Maybe SMSMfaSettingsType)
adminSetUserMFAPreference_sMSMfaSettings = Lens.lens (\AdminSetUserMFAPreference' {sMSMfaSettings} -> sMSMfaSettings) (\s@AdminSetUserMFAPreference' {} a -> s {sMSMfaSettings = a} :: AdminSetUserMFAPreference)

-- | The user pool username or alias.
adminSetUserMFAPreference_username :: Lens.Lens' AdminSetUserMFAPreference Prelude.Text
adminSetUserMFAPreference_username = Lens.lens (\AdminSetUserMFAPreference' {username} -> username) (\s@AdminSetUserMFAPreference' {} a -> s {username = a} :: AdminSetUserMFAPreference) Prelude.. Prelude._Sensitive

-- | The user pool ID.
adminSetUserMFAPreference_userPoolId :: Lens.Lens' AdminSetUserMFAPreference Prelude.Text
adminSetUserMFAPreference_userPoolId = Lens.lens (\AdminSetUserMFAPreference' {userPoolId} -> userPoolId) (\s@AdminSetUserMFAPreference' {} a -> s {userPoolId = a} :: AdminSetUserMFAPreference)

instance Prelude.AWSRequest AdminSetUserMFAPreference where
  type
    Rs AdminSetUserMFAPreference =
      AdminSetUserMFAPreferenceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AdminSetUserMFAPreferenceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AdminSetUserMFAPreference

instance Prelude.NFData AdminSetUserMFAPreference

instance Prelude.ToHeaders AdminSetUserMFAPreference where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCognitoIdentityProviderService.AdminSetUserMFAPreference" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AdminSetUserMFAPreference where
  toJSON AdminSetUserMFAPreference' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SoftwareTokenMfaSettings" Prelude..=)
              Prelude.<$> softwareTokenMfaSettings,
            ("SMSMfaSettings" Prelude..=)
              Prelude.<$> sMSMfaSettings,
            Prelude.Just ("Username" Prelude..= username),
            Prelude.Just ("UserPoolId" Prelude..= userPoolId)
          ]
      )

instance Prelude.ToPath AdminSetUserMFAPreference where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AdminSetUserMFAPreference where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAdminSetUserMFAPreferenceResponse' smart constructor.
data AdminSetUserMFAPreferenceResponse = AdminSetUserMFAPreferenceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AdminSetUserMFAPreferenceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'adminSetUserMFAPreferenceResponse_httpStatus' - The response's http status code.
newAdminSetUserMFAPreferenceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AdminSetUserMFAPreferenceResponse
newAdminSetUserMFAPreferenceResponse pHttpStatus_ =
  AdminSetUserMFAPreferenceResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
adminSetUserMFAPreferenceResponse_httpStatus :: Lens.Lens' AdminSetUserMFAPreferenceResponse Prelude.Int
adminSetUserMFAPreferenceResponse_httpStatus = Lens.lens (\AdminSetUserMFAPreferenceResponse' {httpStatus} -> httpStatus) (\s@AdminSetUserMFAPreferenceResponse' {} a -> s {httpStatus = a} :: AdminSetUserMFAPreferenceResponse)

instance
  Prelude.NFData
    AdminSetUserMFAPreferenceResponse
