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
-- Module      : Network.AWS.CognitoIdentityProvider.SetUserMFAPreference
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Set the user\'s multi-factor authentication (MFA) method preference,
-- including which MFA factors are enabled and if any are preferred. Only
-- one factor can be set as preferred. The preferred MFA factor will be
-- used to authenticate a user if multiple factors are enabled. If multiple
-- options are enabled and no preference is set, a challenge to choose an
-- MFA option will be returned during sign in. If an MFA type is enabled
-- for a user, the user will be prompted for MFA during all sign in
-- attempts, unless device tracking is turned on and the device has been
-- trusted. If you would like MFA to be applied selectively based on the
-- assessed risk level of sign in attempts, disable MFA for users and turn
-- on Adaptive Authentication for the user pool.
module Network.AWS.CognitoIdentityProvider.SetUserMFAPreference
  ( -- * Creating a Request
    SetUserMFAPreference (..),
    newSetUserMFAPreference,

    -- * Request Lenses
    setUserMFAPreference_softwareTokenMfaSettings,
    setUserMFAPreference_sMSMfaSettings,
    setUserMFAPreference_accessToken,

    -- * Destructuring the Response
    SetUserMFAPreferenceResponse (..),
    newSetUserMFAPreferenceResponse,

    -- * Response Lenses
    setUserMFAPreferenceResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSetUserMFAPreference' smart constructor.
data SetUserMFAPreference = SetUserMFAPreference'
  { -- | The time-based one-time password software token MFA settings.
    softwareTokenMfaSettings :: Prelude.Maybe SoftwareTokenMfaSettingsType,
    -- | The SMS text message multi-factor authentication (MFA) settings.
    sMSMfaSettings :: Prelude.Maybe SMSMfaSettingsType,
    -- | The access token for the user.
    accessToken :: Prelude.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SetUserMFAPreference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'softwareTokenMfaSettings', 'setUserMFAPreference_softwareTokenMfaSettings' - The time-based one-time password software token MFA settings.
--
-- 'sMSMfaSettings', 'setUserMFAPreference_sMSMfaSettings' - The SMS text message multi-factor authentication (MFA) settings.
--
-- 'accessToken', 'setUserMFAPreference_accessToken' - The access token for the user.
newSetUserMFAPreference ::
  -- | 'accessToken'
  Prelude.Text ->
  SetUserMFAPreference
newSetUserMFAPreference pAccessToken_ =
  SetUserMFAPreference'
    { softwareTokenMfaSettings =
        Prelude.Nothing,
      sMSMfaSettings = Prelude.Nothing,
      accessToken =
        Prelude._Sensitive Lens.# pAccessToken_
    }

-- | The time-based one-time password software token MFA settings.
setUserMFAPreference_softwareTokenMfaSettings :: Lens.Lens' SetUserMFAPreference (Prelude.Maybe SoftwareTokenMfaSettingsType)
setUserMFAPreference_softwareTokenMfaSettings = Lens.lens (\SetUserMFAPreference' {softwareTokenMfaSettings} -> softwareTokenMfaSettings) (\s@SetUserMFAPreference' {} a -> s {softwareTokenMfaSettings = a} :: SetUserMFAPreference)

-- | The SMS text message multi-factor authentication (MFA) settings.
setUserMFAPreference_sMSMfaSettings :: Lens.Lens' SetUserMFAPreference (Prelude.Maybe SMSMfaSettingsType)
setUserMFAPreference_sMSMfaSettings = Lens.lens (\SetUserMFAPreference' {sMSMfaSettings} -> sMSMfaSettings) (\s@SetUserMFAPreference' {} a -> s {sMSMfaSettings = a} :: SetUserMFAPreference)

-- | The access token for the user.
setUserMFAPreference_accessToken :: Lens.Lens' SetUserMFAPreference Prelude.Text
setUserMFAPreference_accessToken = Lens.lens (\SetUserMFAPreference' {accessToken} -> accessToken) (\s@SetUserMFAPreference' {} a -> s {accessToken = a} :: SetUserMFAPreference) Prelude.. Prelude._Sensitive

instance Prelude.AWSRequest SetUserMFAPreference where
  type
    Rs SetUserMFAPreference =
      SetUserMFAPreferenceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          SetUserMFAPreferenceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SetUserMFAPreference

instance Prelude.NFData SetUserMFAPreference

instance Prelude.ToHeaders SetUserMFAPreference where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCognitoIdentityProviderService.SetUserMFAPreference" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON SetUserMFAPreference where
  toJSON SetUserMFAPreference' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SoftwareTokenMfaSettings" Prelude..=)
              Prelude.<$> softwareTokenMfaSettings,
            ("SMSMfaSettings" Prelude..=)
              Prelude.<$> sMSMfaSettings,
            Prelude.Just ("AccessToken" Prelude..= accessToken)
          ]
      )

instance Prelude.ToPath SetUserMFAPreference where
  toPath = Prelude.const "/"

instance Prelude.ToQuery SetUserMFAPreference where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSetUserMFAPreferenceResponse' smart constructor.
data SetUserMFAPreferenceResponse = SetUserMFAPreferenceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SetUserMFAPreferenceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'setUserMFAPreferenceResponse_httpStatus' - The response's http status code.
newSetUserMFAPreferenceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SetUserMFAPreferenceResponse
newSetUserMFAPreferenceResponse pHttpStatus_ =
  SetUserMFAPreferenceResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
setUserMFAPreferenceResponse_httpStatus :: Lens.Lens' SetUserMFAPreferenceResponse Prelude.Int
setUserMFAPreferenceResponse_httpStatus = Lens.lens (\SetUserMFAPreferenceResponse' {httpStatus} -> httpStatus) (\s@SetUserMFAPreferenceResponse' {} a -> s {httpStatus = a} :: SetUserMFAPreferenceResponse)

instance Prelude.NFData SetUserMFAPreferenceResponse
