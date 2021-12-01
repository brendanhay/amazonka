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
-- Module      : Amazonka.AmplifyBackend.Types.CreateBackendAuthUserPoolConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyBackend.Types.CreateBackendAuthUserPoolConfig where

import Amazonka.AmplifyBackend.Types.CreateBackendAuthForgotPasswordConfig
import Amazonka.AmplifyBackend.Types.CreateBackendAuthMFAConfig
import Amazonka.AmplifyBackend.Types.CreateBackendAuthOAuthConfig
import Amazonka.AmplifyBackend.Types.CreateBackendAuthPasswordPolicyConfig
import Amazonka.AmplifyBackend.Types.RequiredSignUpAttributesElement
import Amazonka.AmplifyBackend.Types.SignInMethod
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the Amazon Cognito user pool configuration for the auth
-- resource to be configured for your Amplify project.
--
-- /See:/ 'newCreateBackendAuthUserPoolConfig' smart constructor.
data CreateBackendAuthUserPoolConfig = CreateBackendAuthUserPoolConfig'
  { -- | Describes the password policy for your Amazon Cognito user pool,
    -- configured as a part of your Amplify project.
    passwordPolicy :: Prelude.Maybe CreateBackendAuthPasswordPolicyConfig,
    -- | Describes whether to apply multi-factor authentication policies for your
    -- Amazon Cognito user pool configured as a part of your Amplify project.
    mfa :: Prelude.Maybe CreateBackendAuthMFAConfig,
    -- | Describes the forgotten password policy for your Amazon Cognito user
    -- pool, configured as a part of your Amplify project.
    forgotPassword :: Prelude.Maybe CreateBackendAuthForgotPasswordConfig,
    -- | Describes the OAuth policy and rules for your Amazon Cognito user pool,
    -- configured as a part of your Amplify project.
    oAuth :: Prelude.Maybe CreateBackendAuthOAuthConfig,
    -- | The required attributes to sign up new users in the user pool.
    requiredSignUpAttributes :: [RequiredSignUpAttributesElement],
    -- | Describes the sign-in methods that your Amplify app users use to log in
    -- using the Amazon Cognito user pool, configured as a part of your Amplify
    -- project.
    signInMethod :: SignInMethod,
    -- | The Amazon Cognito user pool name.
    userPoolName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBackendAuthUserPoolConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'passwordPolicy', 'createBackendAuthUserPoolConfig_passwordPolicy' - Describes the password policy for your Amazon Cognito user pool,
-- configured as a part of your Amplify project.
--
-- 'mfa', 'createBackendAuthUserPoolConfig_mfa' - Describes whether to apply multi-factor authentication policies for your
-- Amazon Cognito user pool configured as a part of your Amplify project.
--
-- 'forgotPassword', 'createBackendAuthUserPoolConfig_forgotPassword' - Describes the forgotten password policy for your Amazon Cognito user
-- pool, configured as a part of your Amplify project.
--
-- 'oAuth', 'createBackendAuthUserPoolConfig_oAuth' - Describes the OAuth policy and rules for your Amazon Cognito user pool,
-- configured as a part of your Amplify project.
--
-- 'requiredSignUpAttributes', 'createBackendAuthUserPoolConfig_requiredSignUpAttributes' - The required attributes to sign up new users in the user pool.
--
-- 'signInMethod', 'createBackendAuthUserPoolConfig_signInMethod' - Describes the sign-in methods that your Amplify app users use to log in
-- using the Amazon Cognito user pool, configured as a part of your Amplify
-- project.
--
-- 'userPoolName', 'createBackendAuthUserPoolConfig_userPoolName' - The Amazon Cognito user pool name.
newCreateBackendAuthUserPoolConfig ::
  -- | 'signInMethod'
  SignInMethod ->
  -- | 'userPoolName'
  Prelude.Text ->
  CreateBackendAuthUserPoolConfig
newCreateBackendAuthUserPoolConfig
  pSignInMethod_
  pUserPoolName_ =
    CreateBackendAuthUserPoolConfig'
      { passwordPolicy =
          Prelude.Nothing,
        mfa = Prelude.Nothing,
        forgotPassword = Prelude.Nothing,
        oAuth = Prelude.Nothing,
        requiredSignUpAttributes = Prelude.mempty,
        signInMethod = pSignInMethod_,
        userPoolName = pUserPoolName_
      }

-- | Describes the password policy for your Amazon Cognito user pool,
-- configured as a part of your Amplify project.
createBackendAuthUserPoolConfig_passwordPolicy :: Lens.Lens' CreateBackendAuthUserPoolConfig (Prelude.Maybe CreateBackendAuthPasswordPolicyConfig)
createBackendAuthUserPoolConfig_passwordPolicy = Lens.lens (\CreateBackendAuthUserPoolConfig' {passwordPolicy} -> passwordPolicy) (\s@CreateBackendAuthUserPoolConfig' {} a -> s {passwordPolicy = a} :: CreateBackendAuthUserPoolConfig)

-- | Describes whether to apply multi-factor authentication policies for your
-- Amazon Cognito user pool configured as a part of your Amplify project.
createBackendAuthUserPoolConfig_mfa :: Lens.Lens' CreateBackendAuthUserPoolConfig (Prelude.Maybe CreateBackendAuthMFAConfig)
createBackendAuthUserPoolConfig_mfa = Lens.lens (\CreateBackendAuthUserPoolConfig' {mfa} -> mfa) (\s@CreateBackendAuthUserPoolConfig' {} a -> s {mfa = a} :: CreateBackendAuthUserPoolConfig)

-- | Describes the forgotten password policy for your Amazon Cognito user
-- pool, configured as a part of your Amplify project.
createBackendAuthUserPoolConfig_forgotPassword :: Lens.Lens' CreateBackendAuthUserPoolConfig (Prelude.Maybe CreateBackendAuthForgotPasswordConfig)
createBackendAuthUserPoolConfig_forgotPassword = Lens.lens (\CreateBackendAuthUserPoolConfig' {forgotPassword} -> forgotPassword) (\s@CreateBackendAuthUserPoolConfig' {} a -> s {forgotPassword = a} :: CreateBackendAuthUserPoolConfig)

-- | Describes the OAuth policy and rules for your Amazon Cognito user pool,
-- configured as a part of your Amplify project.
createBackendAuthUserPoolConfig_oAuth :: Lens.Lens' CreateBackendAuthUserPoolConfig (Prelude.Maybe CreateBackendAuthOAuthConfig)
createBackendAuthUserPoolConfig_oAuth = Lens.lens (\CreateBackendAuthUserPoolConfig' {oAuth} -> oAuth) (\s@CreateBackendAuthUserPoolConfig' {} a -> s {oAuth = a} :: CreateBackendAuthUserPoolConfig)

-- | The required attributes to sign up new users in the user pool.
createBackendAuthUserPoolConfig_requiredSignUpAttributes :: Lens.Lens' CreateBackendAuthUserPoolConfig [RequiredSignUpAttributesElement]
createBackendAuthUserPoolConfig_requiredSignUpAttributes = Lens.lens (\CreateBackendAuthUserPoolConfig' {requiredSignUpAttributes} -> requiredSignUpAttributes) (\s@CreateBackendAuthUserPoolConfig' {} a -> s {requiredSignUpAttributes = a} :: CreateBackendAuthUserPoolConfig) Prelude.. Lens.coerced

-- | Describes the sign-in methods that your Amplify app users use to log in
-- using the Amazon Cognito user pool, configured as a part of your Amplify
-- project.
createBackendAuthUserPoolConfig_signInMethod :: Lens.Lens' CreateBackendAuthUserPoolConfig SignInMethod
createBackendAuthUserPoolConfig_signInMethod = Lens.lens (\CreateBackendAuthUserPoolConfig' {signInMethod} -> signInMethod) (\s@CreateBackendAuthUserPoolConfig' {} a -> s {signInMethod = a} :: CreateBackendAuthUserPoolConfig)

-- | The Amazon Cognito user pool name.
createBackendAuthUserPoolConfig_userPoolName :: Lens.Lens' CreateBackendAuthUserPoolConfig Prelude.Text
createBackendAuthUserPoolConfig_userPoolName = Lens.lens (\CreateBackendAuthUserPoolConfig' {userPoolName} -> userPoolName) (\s@CreateBackendAuthUserPoolConfig' {} a -> s {userPoolName = a} :: CreateBackendAuthUserPoolConfig)

instance
  Core.FromJSON
    CreateBackendAuthUserPoolConfig
  where
  parseJSON =
    Core.withObject
      "CreateBackendAuthUserPoolConfig"
      ( \x ->
          CreateBackendAuthUserPoolConfig'
            Prelude.<$> (x Core..:? "passwordPolicy")
            Prelude.<*> (x Core..:? "mfa")
            Prelude.<*> (x Core..:? "forgotPassword")
            Prelude.<*> (x Core..:? "oAuth")
            Prelude.<*> ( x Core..:? "requiredSignUpAttributes"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..: "signInMethod")
            Prelude.<*> (x Core..: "userPoolName")
      )

instance
  Prelude.Hashable
    CreateBackendAuthUserPoolConfig
  where
  hashWithSalt
    salt'
    CreateBackendAuthUserPoolConfig' {..} =
      salt' `Prelude.hashWithSalt` userPoolName
        `Prelude.hashWithSalt` signInMethod
        `Prelude.hashWithSalt` requiredSignUpAttributes
        `Prelude.hashWithSalt` oAuth
        `Prelude.hashWithSalt` forgotPassword
        `Prelude.hashWithSalt` mfa
        `Prelude.hashWithSalt` passwordPolicy

instance
  Prelude.NFData
    CreateBackendAuthUserPoolConfig
  where
  rnf CreateBackendAuthUserPoolConfig' {..} =
    Prelude.rnf passwordPolicy
      `Prelude.seq` Prelude.rnf userPoolName
      `Prelude.seq` Prelude.rnf signInMethod
      `Prelude.seq` Prelude.rnf requiredSignUpAttributes
      `Prelude.seq` Prelude.rnf oAuth
      `Prelude.seq` Prelude.rnf forgotPassword
      `Prelude.seq` Prelude.rnf mfa

instance Core.ToJSON CreateBackendAuthUserPoolConfig where
  toJSON CreateBackendAuthUserPoolConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("passwordPolicy" Core..=)
              Prelude.<$> passwordPolicy,
            ("mfa" Core..=) Prelude.<$> mfa,
            ("forgotPassword" Core..=)
              Prelude.<$> forgotPassword,
            ("oAuth" Core..=) Prelude.<$> oAuth,
            Prelude.Just
              ( "requiredSignUpAttributes"
                  Core..= requiredSignUpAttributes
              ),
            Prelude.Just ("signInMethod" Core..= signInMethod),
            Prelude.Just ("userPoolName" Core..= userPoolName)
          ]
      )
