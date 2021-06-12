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
-- Module      : Network.AWS.CognitoIdentityProvider.SetUserPoolMfaConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Set the user pool multi-factor authentication (MFA) configuration.
module Network.AWS.CognitoIdentityProvider.SetUserPoolMfaConfig
  ( -- * Creating a Request
    SetUserPoolMfaConfig (..),
    newSetUserPoolMfaConfig,

    -- * Request Lenses
    setUserPoolMfaConfig_softwareTokenMfaConfiguration,
    setUserPoolMfaConfig_smsMfaConfiguration,
    setUserPoolMfaConfig_mfaConfiguration,
    setUserPoolMfaConfig_userPoolId,

    -- * Destructuring the Response
    SetUserPoolMfaConfigResponse (..),
    newSetUserPoolMfaConfigResponse,

    -- * Response Lenses
    setUserPoolMfaConfigResponse_softwareTokenMfaConfiguration,
    setUserPoolMfaConfigResponse_smsMfaConfiguration,
    setUserPoolMfaConfigResponse_mfaConfiguration,
    setUserPoolMfaConfigResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSetUserPoolMfaConfig' smart constructor.
data SetUserPoolMfaConfig = SetUserPoolMfaConfig'
  { -- | The software token MFA configuration.
    softwareTokenMfaConfiguration :: Core.Maybe SoftwareTokenMfaConfigType,
    -- | The SMS text message MFA configuration.
    smsMfaConfiguration :: Core.Maybe SmsMfaConfigType,
    -- | The MFA configuration. Valid values include:
    --
    -- -   @OFF@ MFA will not be used for any users.
    --
    -- -   @ON@ MFA is required for all users to sign in.
    --
    -- -   @OPTIONAL@ MFA will be required only for individual users who have
    --     an MFA factor enabled.
    mfaConfiguration :: Core.Maybe UserPoolMfaType,
    -- | The user pool ID.
    userPoolId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SetUserPoolMfaConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'softwareTokenMfaConfiguration', 'setUserPoolMfaConfig_softwareTokenMfaConfiguration' - The software token MFA configuration.
--
-- 'smsMfaConfiguration', 'setUserPoolMfaConfig_smsMfaConfiguration' - The SMS text message MFA configuration.
--
-- 'mfaConfiguration', 'setUserPoolMfaConfig_mfaConfiguration' - The MFA configuration. Valid values include:
--
-- -   @OFF@ MFA will not be used for any users.
--
-- -   @ON@ MFA is required for all users to sign in.
--
-- -   @OPTIONAL@ MFA will be required only for individual users who have
--     an MFA factor enabled.
--
-- 'userPoolId', 'setUserPoolMfaConfig_userPoolId' - The user pool ID.
newSetUserPoolMfaConfig ::
  -- | 'userPoolId'
  Core.Text ->
  SetUserPoolMfaConfig
newSetUserPoolMfaConfig pUserPoolId_ =
  SetUserPoolMfaConfig'
    { softwareTokenMfaConfiguration =
        Core.Nothing,
      smsMfaConfiguration = Core.Nothing,
      mfaConfiguration = Core.Nothing,
      userPoolId = pUserPoolId_
    }

-- | The software token MFA configuration.
setUserPoolMfaConfig_softwareTokenMfaConfiguration :: Lens.Lens' SetUserPoolMfaConfig (Core.Maybe SoftwareTokenMfaConfigType)
setUserPoolMfaConfig_softwareTokenMfaConfiguration = Lens.lens (\SetUserPoolMfaConfig' {softwareTokenMfaConfiguration} -> softwareTokenMfaConfiguration) (\s@SetUserPoolMfaConfig' {} a -> s {softwareTokenMfaConfiguration = a} :: SetUserPoolMfaConfig)

-- | The SMS text message MFA configuration.
setUserPoolMfaConfig_smsMfaConfiguration :: Lens.Lens' SetUserPoolMfaConfig (Core.Maybe SmsMfaConfigType)
setUserPoolMfaConfig_smsMfaConfiguration = Lens.lens (\SetUserPoolMfaConfig' {smsMfaConfiguration} -> smsMfaConfiguration) (\s@SetUserPoolMfaConfig' {} a -> s {smsMfaConfiguration = a} :: SetUserPoolMfaConfig)

-- | The MFA configuration. Valid values include:
--
-- -   @OFF@ MFA will not be used for any users.
--
-- -   @ON@ MFA is required for all users to sign in.
--
-- -   @OPTIONAL@ MFA will be required only for individual users who have
--     an MFA factor enabled.
setUserPoolMfaConfig_mfaConfiguration :: Lens.Lens' SetUserPoolMfaConfig (Core.Maybe UserPoolMfaType)
setUserPoolMfaConfig_mfaConfiguration = Lens.lens (\SetUserPoolMfaConfig' {mfaConfiguration} -> mfaConfiguration) (\s@SetUserPoolMfaConfig' {} a -> s {mfaConfiguration = a} :: SetUserPoolMfaConfig)

-- | The user pool ID.
setUserPoolMfaConfig_userPoolId :: Lens.Lens' SetUserPoolMfaConfig Core.Text
setUserPoolMfaConfig_userPoolId = Lens.lens (\SetUserPoolMfaConfig' {userPoolId} -> userPoolId) (\s@SetUserPoolMfaConfig' {} a -> s {userPoolId = a} :: SetUserPoolMfaConfig)

instance Core.AWSRequest SetUserPoolMfaConfig where
  type
    AWSResponse SetUserPoolMfaConfig =
      SetUserPoolMfaConfigResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SetUserPoolMfaConfigResponse'
            Core.<$> (x Core..?> "SoftwareTokenMfaConfiguration")
            Core.<*> (x Core..?> "SmsMfaConfiguration")
            Core.<*> (x Core..?> "MfaConfiguration")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable SetUserPoolMfaConfig

instance Core.NFData SetUserPoolMfaConfig

instance Core.ToHeaders SetUserPoolMfaConfig where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.SetUserPoolMfaConfig" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON SetUserPoolMfaConfig where
  toJSON SetUserPoolMfaConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SoftwareTokenMfaConfiguration" Core..=)
              Core.<$> softwareTokenMfaConfiguration,
            ("SmsMfaConfiguration" Core..=)
              Core.<$> smsMfaConfiguration,
            ("MfaConfiguration" Core..=)
              Core.<$> mfaConfiguration,
            Core.Just ("UserPoolId" Core..= userPoolId)
          ]
      )

instance Core.ToPath SetUserPoolMfaConfig where
  toPath = Core.const "/"

instance Core.ToQuery SetUserPoolMfaConfig where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newSetUserPoolMfaConfigResponse' smart constructor.
data SetUserPoolMfaConfigResponse = SetUserPoolMfaConfigResponse'
  { -- | The software token MFA configuration.
    softwareTokenMfaConfiguration :: Core.Maybe SoftwareTokenMfaConfigType,
    -- | The SMS text message MFA configuration.
    smsMfaConfiguration :: Core.Maybe SmsMfaConfigType,
    -- | The MFA configuration. Valid values include:
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
-- Create a value of 'SetUserPoolMfaConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'softwareTokenMfaConfiguration', 'setUserPoolMfaConfigResponse_softwareTokenMfaConfiguration' - The software token MFA configuration.
--
-- 'smsMfaConfiguration', 'setUserPoolMfaConfigResponse_smsMfaConfiguration' - The SMS text message MFA configuration.
--
-- 'mfaConfiguration', 'setUserPoolMfaConfigResponse_mfaConfiguration' - The MFA configuration. Valid values include:
--
-- -   @OFF@ MFA will not be used for any users.
--
-- -   @ON@ MFA is required for all users to sign in.
--
-- -   @OPTIONAL@ MFA will be required only for individual users who have
--     an MFA factor enabled.
--
-- 'httpStatus', 'setUserPoolMfaConfigResponse_httpStatus' - The response's http status code.
newSetUserPoolMfaConfigResponse ::
  -- | 'httpStatus'
  Core.Int ->
  SetUserPoolMfaConfigResponse
newSetUserPoolMfaConfigResponse pHttpStatus_ =
  SetUserPoolMfaConfigResponse'
    { softwareTokenMfaConfiguration =
        Core.Nothing,
      smsMfaConfiguration = Core.Nothing,
      mfaConfiguration = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The software token MFA configuration.
setUserPoolMfaConfigResponse_softwareTokenMfaConfiguration :: Lens.Lens' SetUserPoolMfaConfigResponse (Core.Maybe SoftwareTokenMfaConfigType)
setUserPoolMfaConfigResponse_softwareTokenMfaConfiguration = Lens.lens (\SetUserPoolMfaConfigResponse' {softwareTokenMfaConfiguration} -> softwareTokenMfaConfiguration) (\s@SetUserPoolMfaConfigResponse' {} a -> s {softwareTokenMfaConfiguration = a} :: SetUserPoolMfaConfigResponse)

-- | The SMS text message MFA configuration.
setUserPoolMfaConfigResponse_smsMfaConfiguration :: Lens.Lens' SetUserPoolMfaConfigResponse (Core.Maybe SmsMfaConfigType)
setUserPoolMfaConfigResponse_smsMfaConfiguration = Lens.lens (\SetUserPoolMfaConfigResponse' {smsMfaConfiguration} -> smsMfaConfiguration) (\s@SetUserPoolMfaConfigResponse' {} a -> s {smsMfaConfiguration = a} :: SetUserPoolMfaConfigResponse)

-- | The MFA configuration. Valid values include:
--
-- -   @OFF@ MFA will not be used for any users.
--
-- -   @ON@ MFA is required for all users to sign in.
--
-- -   @OPTIONAL@ MFA will be required only for individual users who have
--     an MFA factor enabled.
setUserPoolMfaConfigResponse_mfaConfiguration :: Lens.Lens' SetUserPoolMfaConfigResponse (Core.Maybe UserPoolMfaType)
setUserPoolMfaConfigResponse_mfaConfiguration = Lens.lens (\SetUserPoolMfaConfigResponse' {mfaConfiguration} -> mfaConfiguration) (\s@SetUserPoolMfaConfigResponse' {} a -> s {mfaConfiguration = a} :: SetUserPoolMfaConfigResponse)

-- | The response's http status code.
setUserPoolMfaConfigResponse_httpStatus :: Lens.Lens' SetUserPoolMfaConfigResponse Core.Int
setUserPoolMfaConfigResponse_httpStatus = Lens.lens (\SetUserPoolMfaConfigResponse' {httpStatus} -> httpStatus) (\s@SetUserPoolMfaConfigResponse' {} a -> s {httpStatus = a} :: SetUserPoolMfaConfigResponse)

instance Core.NFData SetUserPoolMfaConfigResponse
