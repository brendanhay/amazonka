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
-- Module      : Amazonka.CognitoIdentityProvider.SetUserSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- /This action is no longer supported./ You can use it to configure only
-- SMS MFA. You can\'t use it to configure time-based one-time password
-- (TOTP) software token MFA. To configure either type of MFA, use
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_SetUserMFAPreference.html SetUserMFAPreference>
-- instead.
module Amazonka.CognitoIdentityProvider.SetUserSettings
  ( -- * Creating a Request
    SetUserSettings (..),
    newSetUserSettings,

    -- * Request Lenses
    setUserSettings_accessToken,
    setUserSettings_mfaOptions,

    -- * Destructuring the Response
    SetUserSettingsResponse (..),
    newSetUserSettingsResponse,

    -- * Response Lenses
    setUserSettingsResponse_httpStatus,
  )
where

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the request to set user settings.
--
-- /See:/ 'newSetUserSettings' smart constructor.
data SetUserSettings = SetUserSettings'
  { -- | A valid access token that Amazon Cognito issued to the user whose user
    -- settings you want to configure.
    accessToken :: Data.Sensitive Prelude.Text,
    -- | You can use this parameter only to set an SMS configuration that uses
    -- SMS for delivery.
    mfaOptions :: [MFAOptionType]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetUserSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessToken', 'setUserSettings_accessToken' - A valid access token that Amazon Cognito issued to the user whose user
-- settings you want to configure.
--
-- 'mfaOptions', 'setUserSettings_mfaOptions' - You can use this parameter only to set an SMS configuration that uses
-- SMS for delivery.
newSetUserSettings ::
  -- | 'accessToken'
  Prelude.Text ->
  SetUserSettings
newSetUserSettings pAccessToken_ =
  SetUserSettings'
    { accessToken =
        Data._Sensitive Lens.# pAccessToken_,
      mfaOptions = Prelude.mempty
    }

-- | A valid access token that Amazon Cognito issued to the user whose user
-- settings you want to configure.
setUserSettings_accessToken :: Lens.Lens' SetUserSettings Prelude.Text
setUserSettings_accessToken = Lens.lens (\SetUserSettings' {accessToken} -> accessToken) (\s@SetUserSettings' {} a -> s {accessToken = a} :: SetUserSettings) Prelude.. Data._Sensitive

-- | You can use this parameter only to set an SMS configuration that uses
-- SMS for delivery.
setUserSettings_mfaOptions :: Lens.Lens' SetUserSettings [MFAOptionType]
setUserSettings_mfaOptions = Lens.lens (\SetUserSettings' {mfaOptions} -> mfaOptions) (\s@SetUserSettings' {} a -> s {mfaOptions = a} :: SetUserSettings) Prelude.. Lens.coerced

instance Core.AWSRequest SetUserSettings where
  type
    AWSResponse SetUserSettings =
      SetUserSettingsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          SetUserSettingsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SetUserSettings where
  hashWithSalt _salt SetUserSettings' {..} =
    _salt
      `Prelude.hashWithSalt` accessToken
      `Prelude.hashWithSalt` mfaOptions

instance Prelude.NFData SetUserSettings where
  rnf SetUserSettings' {..} =
    Prelude.rnf accessToken
      `Prelude.seq` Prelude.rnf mfaOptions

instance Data.ToHeaders SetUserSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.SetUserSettings" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SetUserSettings where
  toJSON SetUserSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("AccessToken" Data..= accessToken),
            Prelude.Just ("MFAOptions" Data..= mfaOptions)
          ]
      )

instance Data.ToPath SetUserSettings where
  toPath = Prelude.const "/"

instance Data.ToQuery SetUserSettings where
  toQuery = Prelude.const Prelude.mempty

-- | The response from the server for a set user settings request.
--
-- /See:/ 'newSetUserSettingsResponse' smart constructor.
data SetUserSettingsResponse = SetUserSettingsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetUserSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'setUserSettingsResponse_httpStatus' - The response's http status code.
newSetUserSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SetUserSettingsResponse
newSetUserSettingsResponse pHttpStatus_ =
  SetUserSettingsResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
setUserSettingsResponse_httpStatus :: Lens.Lens' SetUserSettingsResponse Prelude.Int
setUserSettingsResponse_httpStatus = Lens.lens (\SetUserSettingsResponse' {httpStatus} -> httpStatus) (\s@SetUserSettingsResponse' {} a -> s {httpStatus = a} :: SetUserSettingsResponse)

instance Prelude.NFData SetUserSettingsResponse where
  rnf SetUserSettingsResponse' {..} =
    Prelude.rnf httpStatus
