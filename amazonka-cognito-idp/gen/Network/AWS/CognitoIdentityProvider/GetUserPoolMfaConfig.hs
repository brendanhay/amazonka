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
-- Module      : Network.AWS.CognitoIdentityProvider.GetUserPoolMfaConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the user pool multi-factor authentication (MFA) configuration.
module Network.AWS.CognitoIdentityProvider.GetUserPoolMfaConfig
  ( -- * Creating a Request
    GetUserPoolMfaConfig (..),
    newGetUserPoolMfaConfig,

    -- * Request Lenses
    getUserPoolMfaConfig_userPoolId,

    -- * Destructuring the Response
    GetUserPoolMfaConfigResponse (..),
    newGetUserPoolMfaConfigResponse,

    -- * Response Lenses
    getUserPoolMfaConfigResponse_softwareTokenMfaConfiguration,
    getUserPoolMfaConfigResponse_smsMfaConfiguration,
    getUserPoolMfaConfigResponse_mfaConfiguration,
    getUserPoolMfaConfigResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetUserPoolMfaConfig' smart constructor.
data GetUserPoolMfaConfig = GetUserPoolMfaConfig'
  { -- | The user pool ID.
    userPoolId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetUserPoolMfaConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userPoolId', 'getUserPoolMfaConfig_userPoolId' - The user pool ID.
newGetUserPoolMfaConfig ::
  -- | 'userPoolId'
  Core.Text ->
  GetUserPoolMfaConfig
newGetUserPoolMfaConfig pUserPoolId_ =
  GetUserPoolMfaConfig' {userPoolId = pUserPoolId_}

-- | The user pool ID.
getUserPoolMfaConfig_userPoolId :: Lens.Lens' GetUserPoolMfaConfig Core.Text
getUserPoolMfaConfig_userPoolId = Lens.lens (\GetUserPoolMfaConfig' {userPoolId} -> userPoolId) (\s@GetUserPoolMfaConfig' {} a -> s {userPoolId = a} :: GetUserPoolMfaConfig)

instance Core.AWSRequest GetUserPoolMfaConfig where
  type
    AWSResponse GetUserPoolMfaConfig =
      GetUserPoolMfaConfigResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetUserPoolMfaConfigResponse'
            Core.<$> (x Core..?> "SoftwareTokenMfaConfiguration")
            Core.<*> (x Core..?> "SmsMfaConfiguration")
            Core.<*> (x Core..?> "MfaConfiguration")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetUserPoolMfaConfig

instance Core.NFData GetUserPoolMfaConfig

instance Core.ToHeaders GetUserPoolMfaConfig where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.GetUserPoolMfaConfig" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetUserPoolMfaConfig where
  toJSON GetUserPoolMfaConfig' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("UserPoolId" Core..= userPoolId)]
      )

instance Core.ToPath GetUserPoolMfaConfig where
  toPath = Core.const "/"

instance Core.ToQuery GetUserPoolMfaConfig where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetUserPoolMfaConfigResponse' smart constructor.
data GetUserPoolMfaConfigResponse = GetUserPoolMfaConfigResponse'
  { -- | The software token multi-factor (MFA) configuration.
    softwareTokenMfaConfiguration :: Core.Maybe SoftwareTokenMfaConfigType,
    -- | The SMS text message multi-factor (MFA) configuration.
    smsMfaConfiguration :: Core.Maybe SmsMfaConfigType,
    -- | The multi-factor (MFA) configuration. Valid values include:
    --
    -- -   @OFF@ MFA will not be used for any users.
    --
    -- -   @ON@ MFA is required for all users to sign in.
    --
    -- -   @OPTIONAL@ MFA will be required only for individual users who have
    --     an MFA factor enabled.
    mfaConfiguration :: Core.Maybe UserPoolMfaType,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetUserPoolMfaConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'softwareTokenMfaConfiguration', 'getUserPoolMfaConfigResponse_softwareTokenMfaConfiguration' - The software token multi-factor (MFA) configuration.
--
-- 'smsMfaConfiguration', 'getUserPoolMfaConfigResponse_smsMfaConfiguration' - The SMS text message multi-factor (MFA) configuration.
--
-- 'mfaConfiguration', 'getUserPoolMfaConfigResponse_mfaConfiguration' - The multi-factor (MFA) configuration. Valid values include:
--
-- -   @OFF@ MFA will not be used for any users.
--
-- -   @ON@ MFA is required for all users to sign in.
--
-- -   @OPTIONAL@ MFA will be required only for individual users who have
--     an MFA factor enabled.
--
-- 'httpStatus', 'getUserPoolMfaConfigResponse_httpStatus' - The response's http status code.
newGetUserPoolMfaConfigResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetUserPoolMfaConfigResponse
newGetUserPoolMfaConfigResponse pHttpStatus_ =
  GetUserPoolMfaConfigResponse'
    { softwareTokenMfaConfiguration =
        Core.Nothing,
      smsMfaConfiguration = Core.Nothing,
      mfaConfiguration = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The software token multi-factor (MFA) configuration.
getUserPoolMfaConfigResponse_softwareTokenMfaConfiguration :: Lens.Lens' GetUserPoolMfaConfigResponse (Core.Maybe SoftwareTokenMfaConfigType)
getUserPoolMfaConfigResponse_softwareTokenMfaConfiguration = Lens.lens (\GetUserPoolMfaConfigResponse' {softwareTokenMfaConfiguration} -> softwareTokenMfaConfiguration) (\s@GetUserPoolMfaConfigResponse' {} a -> s {softwareTokenMfaConfiguration = a} :: GetUserPoolMfaConfigResponse)

-- | The SMS text message multi-factor (MFA) configuration.
getUserPoolMfaConfigResponse_smsMfaConfiguration :: Lens.Lens' GetUserPoolMfaConfigResponse (Core.Maybe SmsMfaConfigType)
getUserPoolMfaConfigResponse_smsMfaConfiguration = Lens.lens (\GetUserPoolMfaConfigResponse' {smsMfaConfiguration} -> smsMfaConfiguration) (\s@GetUserPoolMfaConfigResponse' {} a -> s {smsMfaConfiguration = a} :: GetUserPoolMfaConfigResponse)

-- | The multi-factor (MFA) configuration. Valid values include:
--
-- -   @OFF@ MFA will not be used for any users.
--
-- -   @ON@ MFA is required for all users to sign in.
--
-- -   @OPTIONAL@ MFA will be required only for individual users who have
--     an MFA factor enabled.
getUserPoolMfaConfigResponse_mfaConfiguration :: Lens.Lens' GetUserPoolMfaConfigResponse (Core.Maybe UserPoolMfaType)
getUserPoolMfaConfigResponse_mfaConfiguration = Lens.lens (\GetUserPoolMfaConfigResponse' {mfaConfiguration} -> mfaConfiguration) (\s@GetUserPoolMfaConfigResponse' {} a -> s {mfaConfiguration = a} :: GetUserPoolMfaConfigResponse)

-- | The response's http status code.
getUserPoolMfaConfigResponse_httpStatus :: Lens.Lens' GetUserPoolMfaConfigResponse Core.Int
getUserPoolMfaConfigResponse_httpStatus = Lens.lens (\GetUserPoolMfaConfigResponse' {httpStatus} -> httpStatus) (\s@GetUserPoolMfaConfigResponse' {} a -> s {httpStatus = a} :: GetUserPoolMfaConfigResponse)

instance Core.NFData GetUserPoolMfaConfigResponse
