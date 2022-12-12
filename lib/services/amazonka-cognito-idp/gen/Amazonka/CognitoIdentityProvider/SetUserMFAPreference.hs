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
-- Module      : Amazonka.CognitoIdentityProvider.SetUserMFAPreference
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Set the user\'s multi-factor authentication (MFA) method preference,
-- including which MFA factors are activated and if any are preferred. Only
-- one factor can be set as preferred. The preferred MFA factor will be
-- used to authenticate a user if multiple factors are activated. If
-- multiple options are activated and no preference is set, a challenge to
-- choose an MFA option will be returned during sign-in. If an MFA type is
-- activated for a user, the user will be prompted for MFA during all
-- sign-in attempts unless device tracking is turned on and the device has
-- been trusted. If you want MFA to be applied selectively based on the
-- assessed risk level of sign-in attempts, deactivate MFA for users and
-- turn on Adaptive Authentication for the user pool.
module Amazonka.CognitoIdentityProvider.SetUserMFAPreference
  ( -- * Creating a Request
    SetUserMFAPreference (..),
    newSetUserMFAPreference,

    -- * Request Lenses
    setUserMFAPreference_sMSMfaSettings,
    setUserMFAPreference_softwareTokenMfaSettings,
    setUserMFAPreference_accessToken,

    -- * Destructuring the Response
    SetUserMFAPreferenceResponse (..),
    newSetUserMFAPreferenceResponse,

    -- * Response Lenses
    setUserMFAPreferenceResponse_httpStatus,
  )
where

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSetUserMFAPreference' smart constructor.
data SetUserMFAPreference = SetUserMFAPreference'
  { -- | The SMS text message multi-factor authentication (MFA) settings.
    sMSMfaSettings :: Prelude.Maybe SMSMfaSettingsType,
    -- | The time-based one-time password (TOTP) software token MFA settings.
    softwareTokenMfaSettings :: Prelude.Maybe SoftwareTokenMfaSettingsType,
    -- | A valid access token that Amazon Cognito issued to the user whose MFA
    -- preference you want to set.
    accessToken :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetUserMFAPreference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sMSMfaSettings', 'setUserMFAPreference_sMSMfaSettings' - The SMS text message multi-factor authentication (MFA) settings.
--
-- 'softwareTokenMfaSettings', 'setUserMFAPreference_softwareTokenMfaSettings' - The time-based one-time password (TOTP) software token MFA settings.
--
-- 'accessToken', 'setUserMFAPreference_accessToken' - A valid access token that Amazon Cognito issued to the user whose MFA
-- preference you want to set.
newSetUserMFAPreference ::
  -- | 'accessToken'
  Prelude.Text ->
  SetUserMFAPreference
newSetUserMFAPreference pAccessToken_ =
  SetUserMFAPreference'
    { sMSMfaSettings =
        Prelude.Nothing,
      softwareTokenMfaSettings = Prelude.Nothing,
      accessToken = Data._Sensitive Lens.# pAccessToken_
    }

-- | The SMS text message multi-factor authentication (MFA) settings.
setUserMFAPreference_sMSMfaSettings :: Lens.Lens' SetUserMFAPreference (Prelude.Maybe SMSMfaSettingsType)
setUserMFAPreference_sMSMfaSettings = Lens.lens (\SetUserMFAPreference' {sMSMfaSettings} -> sMSMfaSettings) (\s@SetUserMFAPreference' {} a -> s {sMSMfaSettings = a} :: SetUserMFAPreference)

-- | The time-based one-time password (TOTP) software token MFA settings.
setUserMFAPreference_softwareTokenMfaSettings :: Lens.Lens' SetUserMFAPreference (Prelude.Maybe SoftwareTokenMfaSettingsType)
setUserMFAPreference_softwareTokenMfaSettings = Lens.lens (\SetUserMFAPreference' {softwareTokenMfaSettings} -> softwareTokenMfaSettings) (\s@SetUserMFAPreference' {} a -> s {softwareTokenMfaSettings = a} :: SetUserMFAPreference)

-- | A valid access token that Amazon Cognito issued to the user whose MFA
-- preference you want to set.
setUserMFAPreference_accessToken :: Lens.Lens' SetUserMFAPreference Prelude.Text
setUserMFAPreference_accessToken = Lens.lens (\SetUserMFAPreference' {accessToken} -> accessToken) (\s@SetUserMFAPreference' {} a -> s {accessToken = a} :: SetUserMFAPreference) Prelude.. Data._Sensitive

instance Core.AWSRequest SetUserMFAPreference where
  type
    AWSResponse SetUserMFAPreference =
      SetUserMFAPreferenceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          SetUserMFAPreferenceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SetUserMFAPreference where
  hashWithSalt _salt SetUserMFAPreference' {..} =
    _salt `Prelude.hashWithSalt` sMSMfaSettings
      `Prelude.hashWithSalt` softwareTokenMfaSettings
      `Prelude.hashWithSalt` accessToken

instance Prelude.NFData SetUserMFAPreference where
  rnf SetUserMFAPreference' {..} =
    Prelude.rnf sMSMfaSettings
      `Prelude.seq` Prelude.rnf softwareTokenMfaSettings
      `Prelude.seq` Prelude.rnf accessToken

instance Data.ToHeaders SetUserMFAPreference where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.SetUserMFAPreference" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SetUserMFAPreference where
  toJSON SetUserMFAPreference' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SMSMfaSettings" Data..=)
              Prelude.<$> sMSMfaSettings,
            ("SoftwareTokenMfaSettings" Data..=)
              Prelude.<$> softwareTokenMfaSettings,
            Prelude.Just ("AccessToken" Data..= accessToken)
          ]
      )

instance Data.ToPath SetUserMFAPreference where
  toPath = Prelude.const "/"

instance Data.ToQuery SetUserMFAPreference where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSetUserMFAPreferenceResponse' smart constructor.
data SetUserMFAPreferenceResponse = SetUserMFAPreferenceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData SetUserMFAPreferenceResponse where
  rnf SetUserMFAPreferenceResponse' {..} =
    Prelude.rnf httpStatus
