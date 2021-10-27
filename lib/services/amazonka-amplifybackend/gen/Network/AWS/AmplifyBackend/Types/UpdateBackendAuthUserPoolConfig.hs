{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmplifyBackend.Types.UpdateBackendAuthUserPoolConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AmplifyBackend.Types.UpdateBackendAuthUserPoolConfig where

import Network.AWS.AmplifyBackend.Types.UpdateBackendAuthForgotPasswordConfig
import Network.AWS.AmplifyBackend.Types.UpdateBackendAuthMFAConfig
import Network.AWS.AmplifyBackend.Types.UpdateBackendAuthOAuthConfig
import Network.AWS.AmplifyBackend.Types.UpdateBackendAuthPasswordPolicyConfig
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the Amazon Cognito user pool configuration for the
-- authorization resource to be configured for your Amplify project on an
-- update.
--
-- /See:/ 'newUpdateBackendAuthUserPoolConfig' smart constructor.
data UpdateBackendAuthUserPoolConfig = UpdateBackendAuthUserPoolConfig'
  { -- | Describes the password policy for your Amazon Cognito user pool,
    -- configured as a part of your Amplify project.
    passwordPolicy :: Prelude.Maybe UpdateBackendAuthPasswordPolicyConfig,
    -- | Describes whether to apply multi-factor authentication policies for your
    -- Amazon Cognito user pool configured as a part of your Amplify project.
    mfa :: Prelude.Maybe UpdateBackendAuthMFAConfig,
    -- | Describes the forgot password policy for your Amazon Cognito user pool,
    -- configured as a part of your Amplify project.
    forgotPassword :: Prelude.Maybe UpdateBackendAuthForgotPasswordConfig,
    -- | Describes the OAuth policy and rules for your Amazon Cognito user pool,
    -- configured as a part of your Amplify project.
    oAuth :: Prelude.Maybe UpdateBackendAuthOAuthConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBackendAuthUserPoolConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'passwordPolicy', 'updateBackendAuthUserPoolConfig_passwordPolicy' - Describes the password policy for your Amazon Cognito user pool,
-- configured as a part of your Amplify project.
--
-- 'mfa', 'updateBackendAuthUserPoolConfig_mfa' - Describes whether to apply multi-factor authentication policies for your
-- Amazon Cognito user pool configured as a part of your Amplify project.
--
-- 'forgotPassword', 'updateBackendAuthUserPoolConfig_forgotPassword' - Describes the forgot password policy for your Amazon Cognito user pool,
-- configured as a part of your Amplify project.
--
-- 'oAuth', 'updateBackendAuthUserPoolConfig_oAuth' - Describes the OAuth policy and rules for your Amazon Cognito user pool,
-- configured as a part of your Amplify project.
newUpdateBackendAuthUserPoolConfig ::
  UpdateBackendAuthUserPoolConfig
newUpdateBackendAuthUserPoolConfig =
  UpdateBackendAuthUserPoolConfig'
    { passwordPolicy =
        Prelude.Nothing,
      mfa = Prelude.Nothing,
      forgotPassword = Prelude.Nothing,
      oAuth = Prelude.Nothing
    }

-- | Describes the password policy for your Amazon Cognito user pool,
-- configured as a part of your Amplify project.
updateBackendAuthUserPoolConfig_passwordPolicy :: Lens.Lens' UpdateBackendAuthUserPoolConfig (Prelude.Maybe UpdateBackendAuthPasswordPolicyConfig)
updateBackendAuthUserPoolConfig_passwordPolicy = Lens.lens (\UpdateBackendAuthUserPoolConfig' {passwordPolicy} -> passwordPolicy) (\s@UpdateBackendAuthUserPoolConfig' {} a -> s {passwordPolicy = a} :: UpdateBackendAuthUserPoolConfig)

-- | Describes whether to apply multi-factor authentication policies for your
-- Amazon Cognito user pool configured as a part of your Amplify project.
updateBackendAuthUserPoolConfig_mfa :: Lens.Lens' UpdateBackendAuthUserPoolConfig (Prelude.Maybe UpdateBackendAuthMFAConfig)
updateBackendAuthUserPoolConfig_mfa = Lens.lens (\UpdateBackendAuthUserPoolConfig' {mfa} -> mfa) (\s@UpdateBackendAuthUserPoolConfig' {} a -> s {mfa = a} :: UpdateBackendAuthUserPoolConfig)

-- | Describes the forgot password policy for your Amazon Cognito user pool,
-- configured as a part of your Amplify project.
updateBackendAuthUserPoolConfig_forgotPassword :: Lens.Lens' UpdateBackendAuthUserPoolConfig (Prelude.Maybe UpdateBackendAuthForgotPasswordConfig)
updateBackendAuthUserPoolConfig_forgotPassword = Lens.lens (\UpdateBackendAuthUserPoolConfig' {forgotPassword} -> forgotPassword) (\s@UpdateBackendAuthUserPoolConfig' {} a -> s {forgotPassword = a} :: UpdateBackendAuthUserPoolConfig)

-- | Describes the OAuth policy and rules for your Amazon Cognito user pool,
-- configured as a part of your Amplify project.
updateBackendAuthUserPoolConfig_oAuth :: Lens.Lens' UpdateBackendAuthUserPoolConfig (Prelude.Maybe UpdateBackendAuthOAuthConfig)
updateBackendAuthUserPoolConfig_oAuth = Lens.lens (\UpdateBackendAuthUserPoolConfig' {oAuth} -> oAuth) (\s@UpdateBackendAuthUserPoolConfig' {} a -> s {oAuth = a} :: UpdateBackendAuthUserPoolConfig)

instance
  Prelude.Hashable
    UpdateBackendAuthUserPoolConfig

instance
  Prelude.NFData
    UpdateBackendAuthUserPoolConfig

instance Core.ToJSON UpdateBackendAuthUserPoolConfig where
  toJSON UpdateBackendAuthUserPoolConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("passwordPolicy" Core..=)
              Prelude.<$> passwordPolicy,
            ("mfa" Core..=) Prelude.<$> mfa,
            ("forgotPassword" Core..=)
              Prelude.<$> forgotPassword,
            ("oAuth" Core..=) Prelude.<$> oAuth
          ]
      )
