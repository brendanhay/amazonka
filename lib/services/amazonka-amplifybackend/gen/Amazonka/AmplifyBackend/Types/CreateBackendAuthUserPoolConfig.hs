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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyBackend.Types.CreateBackendAuthUserPoolConfig where

import Amazonka.AmplifyBackend.Types.CreateBackendAuthForgotPasswordConfig
import Amazonka.AmplifyBackend.Types.CreateBackendAuthMFAConfig
import Amazonka.AmplifyBackend.Types.CreateBackendAuthOAuthConfig
import Amazonka.AmplifyBackend.Types.CreateBackendAuthPasswordPolicyConfig
import Amazonka.AmplifyBackend.Types.CreateBackendAuthVerificationMessageConfig
import Amazonka.AmplifyBackend.Types.RequiredSignUpAttributesElement
import Amazonka.AmplifyBackend.Types.SignInMethod
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the Amazon Cognito user pool configuration for the auth
-- resource to be configured for your Amplify project.
--
-- /See:/ 'newCreateBackendAuthUserPoolConfig' smart constructor.
data CreateBackendAuthUserPoolConfig = CreateBackendAuthUserPoolConfig'
  { -- | Describes the OAuth policy and rules for your Amazon Cognito user pool,
    -- configured as a part of your Amplify project.
    oAuth :: Prelude.Maybe CreateBackendAuthOAuthConfig,
    -- | Describes the password policy for your Amazon Cognito user pool,
    -- configured as a part of your Amplify project.
    passwordPolicy :: Prelude.Maybe CreateBackendAuthPasswordPolicyConfig,
    -- | __(DEPRECATED)__ Describes the forgotten password policy for your Amazon
    -- Cognito user pool, configured as a part of your Amplify project.
    forgotPassword :: Prelude.Maybe CreateBackendAuthForgotPasswordConfig,
    -- | Describes the email or SMS verification message for your Amazon Cognito
    -- user pool, configured as a part of your Amplify project.
    verificationMessage :: Prelude.Maybe CreateBackendAuthVerificationMessageConfig,
    -- | Describes whether to apply multi-factor authentication policies for your
    -- Amazon Cognito user pool configured as a part of your Amplify project.
    mfa :: Prelude.Maybe CreateBackendAuthMFAConfig,
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
-- 'oAuth', 'createBackendAuthUserPoolConfig_oAuth' - Describes the OAuth policy and rules for your Amazon Cognito user pool,
-- configured as a part of your Amplify project.
--
-- 'passwordPolicy', 'createBackendAuthUserPoolConfig_passwordPolicy' - Describes the password policy for your Amazon Cognito user pool,
-- configured as a part of your Amplify project.
--
-- 'forgotPassword', 'createBackendAuthUserPoolConfig_forgotPassword' - __(DEPRECATED)__ Describes the forgotten password policy for your Amazon
-- Cognito user pool, configured as a part of your Amplify project.
--
-- 'verificationMessage', 'createBackendAuthUserPoolConfig_verificationMessage' - Describes the email or SMS verification message for your Amazon Cognito
-- user pool, configured as a part of your Amplify project.
--
-- 'mfa', 'createBackendAuthUserPoolConfig_mfa' - Describes whether to apply multi-factor authentication policies for your
-- Amazon Cognito user pool configured as a part of your Amplify project.
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
      { oAuth =
          Prelude.Nothing,
        passwordPolicy = Prelude.Nothing,
        forgotPassword = Prelude.Nothing,
        verificationMessage = Prelude.Nothing,
        mfa = Prelude.Nothing,
        requiredSignUpAttributes = Prelude.mempty,
        signInMethod = pSignInMethod_,
        userPoolName = pUserPoolName_
      }

-- | Describes the OAuth policy and rules for your Amazon Cognito user pool,
-- configured as a part of your Amplify project.
createBackendAuthUserPoolConfig_oAuth :: Lens.Lens' CreateBackendAuthUserPoolConfig (Prelude.Maybe CreateBackendAuthOAuthConfig)
createBackendAuthUserPoolConfig_oAuth = Lens.lens (\CreateBackendAuthUserPoolConfig' {oAuth} -> oAuth) (\s@CreateBackendAuthUserPoolConfig' {} a -> s {oAuth = a} :: CreateBackendAuthUserPoolConfig)

-- | Describes the password policy for your Amazon Cognito user pool,
-- configured as a part of your Amplify project.
createBackendAuthUserPoolConfig_passwordPolicy :: Lens.Lens' CreateBackendAuthUserPoolConfig (Prelude.Maybe CreateBackendAuthPasswordPolicyConfig)
createBackendAuthUserPoolConfig_passwordPolicy = Lens.lens (\CreateBackendAuthUserPoolConfig' {passwordPolicy} -> passwordPolicy) (\s@CreateBackendAuthUserPoolConfig' {} a -> s {passwordPolicy = a} :: CreateBackendAuthUserPoolConfig)

-- | __(DEPRECATED)__ Describes the forgotten password policy for your Amazon
-- Cognito user pool, configured as a part of your Amplify project.
createBackendAuthUserPoolConfig_forgotPassword :: Lens.Lens' CreateBackendAuthUserPoolConfig (Prelude.Maybe CreateBackendAuthForgotPasswordConfig)
createBackendAuthUserPoolConfig_forgotPassword = Lens.lens (\CreateBackendAuthUserPoolConfig' {forgotPassword} -> forgotPassword) (\s@CreateBackendAuthUserPoolConfig' {} a -> s {forgotPassword = a} :: CreateBackendAuthUserPoolConfig)

-- | Describes the email or SMS verification message for your Amazon Cognito
-- user pool, configured as a part of your Amplify project.
createBackendAuthUserPoolConfig_verificationMessage :: Lens.Lens' CreateBackendAuthUserPoolConfig (Prelude.Maybe CreateBackendAuthVerificationMessageConfig)
createBackendAuthUserPoolConfig_verificationMessage = Lens.lens (\CreateBackendAuthUserPoolConfig' {verificationMessage} -> verificationMessage) (\s@CreateBackendAuthUserPoolConfig' {} a -> s {verificationMessage = a} :: CreateBackendAuthUserPoolConfig)

-- | Describes whether to apply multi-factor authentication policies for your
-- Amazon Cognito user pool configured as a part of your Amplify project.
createBackendAuthUserPoolConfig_mfa :: Lens.Lens' CreateBackendAuthUserPoolConfig (Prelude.Maybe CreateBackendAuthMFAConfig)
createBackendAuthUserPoolConfig_mfa = Lens.lens (\CreateBackendAuthUserPoolConfig' {mfa} -> mfa) (\s@CreateBackendAuthUserPoolConfig' {} a -> s {mfa = a} :: CreateBackendAuthUserPoolConfig)

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
  Data.FromJSON
    CreateBackendAuthUserPoolConfig
  where
  parseJSON =
    Data.withObject
      "CreateBackendAuthUserPoolConfig"
      ( \x ->
          CreateBackendAuthUserPoolConfig'
            Prelude.<$> (x Data..:? "oAuth")
            Prelude.<*> (x Data..:? "passwordPolicy")
            Prelude.<*> (x Data..:? "forgotPassword")
            Prelude.<*> (x Data..:? "verificationMessage")
            Prelude.<*> (x Data..:? "mfa")
            Prelude.<*> ( x Data..:? "requiredSignUpAttributes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "signInMethod")
            Prelude.<*> (x Data..: "userPoolName")
      )

instance
  Prelude.Hashable
    CreateBackendAuthUserPoolConfig
  where
  hashWithSalt
    _salt
    CreateBackendAuthUserPoolConfig' {..} =
      _salt `Prelude.hashWithSalt` oAuth
        `Prelude.hashWithSalt` passwordPolicy
        `Prelude.hashWithSalt` forgotPassword
        `Prelude.hashWithSalt` verificationMessage
        `Prelude.hashWithSalt` mfa
        `Prelude.hashWithSalt` requiredSignUpAttributes
        `Prelude.hashWithSalt` signInMethod
        `Prelude.hashWithSalt` userPoolName

instance
  Prelude.NFData
    CreateBackendAuthUserPoolConfig
  where
  rnf CreateBackendAuthUserPoolConfig' {..} =
    Prelude.rnf oAuth
      `Prelude.seq` Prelude.rnf passwordPolicy
      `Prelude.seq` Prelude.rnf forgotPassword
      `Prelude.seq` Prelude.rnf verificationMessage
      `Prelude.seq` Prelude.rnf mfa
      `Prelude.seq` Prelude.rnf requiredSignUpAttributes
      `Prelude.seq` Prelude.rnf signInMethod
      `Prelude.seq` Prelude.rnf userPoolName

instance Data.ToJSON CreateBackendAuthUserPoolConfig where
  toJSON CreateBackendAuthUserPoolConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("oAuth" Data..=) Prelude.<$> oAuth,
            ("passwordPolicy" Data..=)
              Prelude.<$> passwordPolicy,
            ("forgotPassword" Data..=)
              Prelude.<$> forgotPassword,
            ("verificationMessage" Data..=)
              Prelude.<$> verificationMessage,
            ("mfa" Data..=) Prelude.<$> mfa,
            Prelude.Just
              ( "requiredSignUpAttributes"
                  Data..= requiredSignUpAttributes
              ),
            Prelude.Just ("signInMethod" Data..= signInMethod),
            Prelude.Just ("userPoolName" Data..= userPoolName)
          ]
      )
