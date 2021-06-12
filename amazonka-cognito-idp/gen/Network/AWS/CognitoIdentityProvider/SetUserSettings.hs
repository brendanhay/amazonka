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
-- Module      : Network.AWS.CognitoIdentityProvider.SetUserSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- /This action is no longer supported./ You can use it to configure only
-- SMS MFA. You can\'t use it to configure TOTP software token MFA. To
-- configure either type of MFA, use
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_SetUserMFAPreference.html SetUserMFAPreference>
-- instead.
module Network.AWS.CognitoIdentityProvider.SetUserSettings
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

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to set user settings.
--
-- /See:/ 'newSetUserSettings' smart constructor.
data SetUserSettings = SetUserSettings'
  { -- | The access token for the set user settings request.
    accessToken :: Core.Sensitive Core.Text,
    -- | You can use this parameter only to set an SMS configuration that uses
    -- SMS for delivery.
    mfaOptions :: [MFAOptionType]
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'SetUserSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessToken', 'setUserSettings_accessToken' - The access token for the set user settings request.
--
-- 'mfaOptions', 'setUserSettings_mfaOptions' - You can use this parameter only to set an SMS configuration that uses
-- SMS for delivery.
newSetUserSettings ::
  -- | 'accessToken'
  Core.Text ->
  SetUserSettings
newSetUserSettings pAccessToken_ =
  SetUserSettings'
    { accessToken =
        Core._Sensitive Lens.# pAccessToken_,
      mfaOptions = Core.mempty
    }

-- | The access token for the set user settings request.
setUserSettings_accessToken :: Lens.Lens' SetUserSettings Core.Text
setUserSettings_accessToken = Lens.lens (\SetUserSettings' {accessToken} -> accessToken) (\s@SetUserSettings' {} a -> s {accessToken = a} :: SetUserSettings) Core.. Core._Sensitive

-- | You can use this parameter only to set an SMS configuration that uses
-- SMS for delivery.
setUserSettings_mfaOptions :: Lens.Lens' SetUserSettings [MFAOptionType]
setUserSettings_mfaOptions = Lens.lens (\SetUserSettings' {mfaOptions} -> mfaOptions) (\s@SetUserSettings' {} a -> s {mfaOptions = a} :: SetUserSettings) Core.. Lens._Coerce

instance Core.AWSRequest SetUserSettings where
  type
    AWSResponse SetUserSettings =
      SetUserSettingsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          SetUserSettingsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable SetUserSettings

instance Core.NFData SetUserSettings

instance Core.ToHeaders SetUserSettings where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.SetUserSettings" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON SetUserSettings where
  toJSON SetUserSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AccessToken" Core..= accessToken),
            Core.Just ("MFAOptions" Core..= mfaOptions)
          ]
      )

instance Core.ToPath SetUserSettings where
  toPath = Core.const "/"

instance Core.ToQuery SetUserSettings where
  toQuery = Core.const Core.mempty

-- | The response from the server for a set user settings request.
--
-- /See:/ 'newSetUserSettingsResponse' smart constructor.
data SetUserSettingsResponse = SetUserSettingsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  SetUserSettingsResponse
newSetUserSettingsResponse pHttpStatus_ =
  SetUserSettingsResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
setUserSettingsResponse_httpStatus :: Lens.Lens' SetUserSettingsResponse Core.Int
setUserSettingsResponse_httpStatus = Lens.lens (\SetUserSettingsResponse' {httpStatus} -> httpStatus) (\s@SetUserSettingsResponse' {} a -> s {httpStatus = a} :: SetUserSettingsResponse)

instance Core.NFData SetUserSettingsResponse
