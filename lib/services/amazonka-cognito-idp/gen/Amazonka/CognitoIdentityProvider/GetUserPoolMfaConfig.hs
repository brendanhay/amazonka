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
-- Module      : Amazonka.CognitoIdentityProvider.GetUserPoolMfaConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the user pool multi-factor authentication (MFA) configuration.
module Amazonka.CognitoIdentityProvider.GetUserPoolMfaConfig
  ( -- * Creating a Request
    GetUserPoolMfaConfig (..),
    newGetUserPoolMfaConfig,

    -- * Request Lenses
    getUserPoolMfaConfig_userPoolId,

    -- * Destructuring the Response
    GetUserPoolMfaConfigResponse (..),
    newGetUserPoolMfaConfigResponse,

    -- * Response Lenses
    getUserPoolMfaConfigResponse_smsMfaConfiguration,
    getUserPoolMfaConfigResponse_softwareTokenMfaConfiguration,
    getUserPoolMfaConfigResponse_mfaConfiguration,
    getUserPoolMfaConfigResponse_httpStatus,
  )
where

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetUserPoolMfaConfig' smart constructor.
data GetUserPoolMfaConfig = GetUserPoolMfaConfig'
  { -- | The user pool ID.
    userPoolId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetUserPoolMfaConfig
newGetUserPoolMfaConfig pUserPoolId_ =
  GetUserPoolMfaConfig' {userPoolId = pUserPoolId_}

-- | The user pool ID.
getUserPoolMfaConfig_userPoolId :: Lens.Lens' GetUserPoolMfaConfig Prelude.Text
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
            Prelude.<$> (x Core..?> "SmsMfaConfiguration")
            Prelude.<*> (x Core..?> "SoftwareTokenMfaConfiguration")
            Prelude.<*> (x Core..?> "MfaConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetUserPoolMfaConfig where
  hashWithSalt _salt GetUserPoolMfaConfig' {..} =
    _salt `Prelude.hashWithSalt` userPoolId

instance Prelude.NFData GetUserPoolMfaConfig where
  rnf GetUserPoolMfaConfig' {..} =
    Prelude.rnf userPoolId

instance Core.ToHeaders GetUserPoolMfaConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.GetUserPoolMfaConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetUserPoolMfaConfig where
  toJSON GetUserPoolMfaConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("UserPoolId" Core..= userPoolId)]
      )

instance Core.ToPath GetUserPoolMfaConfig where
  toPath = Prelude.const "/"

instance Core.ToQuery GetUserPoolMfaConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetUserPoolMfaConfigResponse' smart constructor.
data GetUserPoolMfaConfigResponse = GetUserPoolMfaConfigResponse'
  { -- | The SMS text message multi-factor (MFA) configuration.
    smsMfaConfiguration :: Prelude.Maybe SmsMfaConfigType,
    -- | The software token multi-factor (MFA) configuration.
    softwareTokenMfaConfiguration :: Prelude.Maybe SoftwareTokenMfaConfigType,
    -- | The multi-factor (MFA) configuration. Valid values include:
    --
    -- -   @OFF@ MFA will not be used for any users.
    --
    -- -   @ON@ MFA is required for all users to sign in.
    --
    -- -   @OPTIONAL@ MFA will be required only for individual users who have
    --     an MFA factor enabled.
    mfaConfiguration :: Prelude.Maybe UserPoolMfaType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetUserPoolMfaConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'smsMfaConfiguration', 'getUserPoolMfaConfigResponse_smsMfaConfiguration' - The SMS text message multi-factor (MFA) configuration.
--
-- 'softwareTokenMfaConfiguration', 'getUserPoolMfaConfigResponse_softwareTokenMfaConfiguration' - The software token multi-factor (MFA) configuration.
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
  Prelude.Int ->
  GetUserPoolMfaConfigResponse
newGetUserPoolMfaConfigResponse pHttpStatus_ =
  GetUserPoolMfaConfigResponse'
    { smsMfaConfiguration =
        Prelude.Nothing,
      softwareTokenMfaConfiguration =
        Prelude.Nothing,
      mfaConfiguration = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The SMS text message multi-factor (MFA) configuration.
getUserPoolMfaConfigResponse_smsMfaConfiguration :: Lens.Lens' GetUserPoolMfaConfigResponse (Prelude.Maybe SmsMfaConfigType)
getUserPoolMfaConfigResponse_smsMfaConfiguration = Lens.lens (\GetUserPoolMfaConfigResponse' {smsMfaConfiguration} -> smsMfaConfiguration) (\s@GetUserPoolMfaConfigResponse' {} a -> s {smsMfaConfiguration = a} :: GetUserPoolMfaConfigResponse)

-- | The software token multi-factor (MFA) configuration.
getUserPoolMfaConfigResponse_softwareTokenMfaConfiguration :: Lens.Lens' GetUserPoolMfaConfigResponse (Prelude.Maybe SoftwareTokenMfaConfigType)
getUserPoolMfaConfigResponse_softwareTokenMfaConfiguration = Lens.lens (\GetUserPoolMfaConfigResponse' {softwareTokenMfaConfiguration} -> softwareTokenMfaConfiguration) (\s@GetUserPoolMfaConfigResponse' {} a -> s {softwareTokenMfaConfiguration = a} :: GetUserPoolMfaConfigResponse)

-- | The multi-factor (MFA) configuration. Valid values include:
--
-- -   @OFF@ MFA will not be used for any users.
--
-- -   @ON@ MFA is required for all users to sign in.
--
-- -   @OPTIONAL@ MFA will be required only for individual users who have
--     an MFA factor enabled.
getUserPoolMfaConfigResponse_mfaConfiguration :: Lens.Lens' GetUserPoolMfaConfigResponse (Prelude.Maybe UserPoolMfaType)
getUserPoolMfaConfigResponse_mfaConfiguration = Lens.lens (\GetUserPoolMfaConfigResponse' {mfaConfiguration} -> mfaConfiguration) (\s@GetUserPoolMfaConfigResponse' {} a -> s {mfaConfiguration = a} :: GetUserPoolMfaConfigResponse)

-- | The response's http status code.
getUserPoolMfaConfigResponse_httpStatus :: Lens.Lens' GetUserPoolMfaConfigResponse Prelude.Int
getUserPoolMfaConfigResponse_httpStatus = Lens.lens (\GetUserPoolMfaConfigResponse' {httpStatus} -> httpStatus) (\s@GetUserPoolMfaConfigResponse' {} a -> s {httpStatus = a} :: GetUserPoolMfaConfigResponse)

instance Prelude.NFData GetUserPoolMfaConfigResponse where
  rnf GetUserPoolMfaConfigResponse' {..} =
    Prelude.rnf smsMfaConfiguration
      `Prelude.seq` Prelude.rnf softwareTokenMfaConfiguration
      `Prelude.seq` Prelude.rnf mfaConfiguration
      `Prelude.seq` Prelude.rnf httpStatus
