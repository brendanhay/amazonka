{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.LambdaConfigType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.LambdaConfigType where

import Network.AWS.CognitoIdentityProvider.Types.CustomEmailLambdaVersionConfigType
import Network.AWS.CognitoIdentityProvider.Types.CustomSMSLambdaVersionConfigType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the configuration for AWS Lambda triggers.
--
-- /See:/ 'newLambdaConfigType' smart constructor.
data LambdaConfigType = LambdaConfigType'
  { -- | A custom email sender AWS Lambda trigger.
    customEmailSender :: Prelude.Maybe CustomEmailLambdaVersionConfigType,
    -- | A pre-registration AWS Lambda trigger.
    preSignUp :: Prelude.Maybe Prelude.Text,
    -- | Defines the authentication challenge.
    defineAuthChallenge :: Prelude.Maybe Prelude.Text,
    -- | A post-authentication AWS Lambda trigger.
    postAuthentication :: Prelude.Maybe Prelude.Text,
    -- | A custom SMS sender AWS Lambda trigger.
    customSMSSender :: Prelude.Maybe CustomSMSLambdaVersionConfigType,
    -- | Creates an authentication challenge.
    createAuthChallenge :: Prelude.Maybe Prelude.Text,
    -- | A post-confirmation AWS Lambda trigger.
    postConfirmation :: Prelude.Maybe Prelude.Text,
    -- | A pre-authentication AWS Lambda trigger.
    preAuthentication :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name of Key Management Service
    -- </kms/latest/developerguide/concepts.html#master_keys Customer master keys>
    -- . Amazon Cognito uses the key to encrypt codes and temporary passwords
    -- sent to @CustomEmailSender@ and @CustomSMSSender@.
    kmsKeyID :: Prelude.Maybe Prelude.Text,
    -- | Verifies the authentication challenge response.
    verifyAuthChallengeResponse :: Prelude.Maybe Prelude.Text,
    -- | A custom Message AWS Lambda trigger.
    customMessage :: Prelude.Maybe Prelude.Text,
    -- | The user migration Lambda config type.
    userMigration :: Prelude.Maybe Prelude.Text,
    -- | A Lambda trigger that is invoked before token generation.
    preTokenGeneration :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LambdaConfigType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customEmailSender', 'lambdaConfigType_customEmailSender' - A custom email sender AWS Lambda trigger.
--
-- 'preSignUp', 'lambdaConfigType_preSignUp' - A pre-registration AWS Lambda trigger.
--
-- 'defineAuthChallenge', 'lambdaConfigType_defineAuthChallenge' - Defines the authentication challenge.
--
-- 'postAuthentication', 'lambdaConfigType_postAuthentication' - A post-authentication AWS Lambda trigger.
--
-- 'customSMSSender', 'lambdaConfigType_customSMSSender' - A custom SMS sender AWS Lambda trigger.
--
-- 'createAuthChallenge', 'lambdaConfigType_createAuthChallenge' - Creates an authentication challenge.
--
-- 'postConfirmation', 'lambdaConfigType_postConfirmation' - A post-confirmation AWS Lambda trigger.
--
-- 'preAuthentication', 'lambdaConfigType_preAuthentication' - A pre-authentication AWS Lambda trigger.
--
-- 'kmsKeyID', 'lambdaConfigType_kmsKeyID' - The Amazon Resource Name of Key Management Service
-- </kms/latest/developerguide/concepts.html#master_keys Customer master keys>
-- . Amazon Cognito uses the key to encrypt codes and temporary passwords
-- sent to @CustomEmailSender@ and @CustomSMSSender@.
--
-- 'verifyAuthChallengeResponse', 'lambdaConfigType_verifyAuthChallengeResponse' - Verifies the authentication challenge response.
--
-- 'customMessage', 'lambdaConfigType_customMessage' - A custom Message AWS Lambda trigger.
--
-- 'userMigration', 'lambdaConfigType_userMigration' - The user migration Lambda config type.
--
-- 'preTokenGeneration', 'lambdaConfigType_preTokenGeneration' - A Lambda trigger that is invoked before token generation.
newLambdaConfigType ::
  LambdaConfigType
newLambdaConfigType =
  LambdaConfigType'
    { customEmailSender =
        Prelude.Nothing,
      preSignUp = Prelude.Nothing,
      defineAuthChallenge = Prelude.Nothing,
      postAuthentication = Prelude.Nothing,
      customSMSSender = Prelude.Nothing,
      createAuthChallenge = Prelude.Nothing,
      postConfirmation = Prelude.Nothing,
      preAuthentication = Prelude.Nothing,
      kmsKeyID = Prelude.Nothing,
      verifyAuthChallengeResponse = Prelude.Nothing,
      customMessage = Prelude.Nothing,
      userMigration = Prelude.Nothing,
      preTokenGeneration = Prelude.Nothing
    }

-- | A custom email sender AWS Lambda trigger.
lambdaConfigType_customEmailSender :: Lens.Lens' LambdaConfigType (Prelude.Maybe CustomEmailLambdaVersionConfigType)
lambdaConfigType_customEmailSender = Lens.lens (\LambdaConfigType' {customEmailSender} -> customEmailSender) (\s@LambdaConfigType' {} a -> s {customEmailSender = a} :: LambdaConfigType)

-- | A pre-registration AWS Lambda trigger.
lambdaConfigType_preSignUp :: Lens.Lens' LambdaConfigType (Prelude.Maybe Prelude.Text)
lambdaConfigType_preSignUp = Lens.lens (\LambdaConfigType' {preSignUp} -> preSignUp) (\s@LambdaConfigType' {} a -> s {preSignUp = a} :: LambdaConfigType)

-- | Defines the authentication challenge.
lambdaConfigType_defineAuthChallenge :: Lens.Lens' LambdaConfigType (Prelude.Maybe Prelude.Text)
lambdaConfigType_defineAuthChallenge = Lens.lens (\LambdaConfigType' {defineAuthChallenge} -> defineAuthChallenge) (\s@LambdaConfigType' {} a -> s {defineAuthChallenge = a} :: LambdaConfigType)

-- | A post-authentication AWS Lambda trigger.
lambdaConfigType_postAuthentication :: Lens.Lens' LambdaConfigType (Prelude.Maybe Prelude.Text)
lambdaConfigType_postAuthentication = Lens.lens (\LambdaConfigType' {postAuthentication} -> postAuthentication) (\s@LambdaConfigType' {} a -> s {postAuthentication = a} :: LambdaConfigType)

-- | A custom SMS sender AWS Lambda trigger.
lambdaConfigType_customSMSSender :: Lens.Lens' LambdaConfigType (Prelude.Maybe CustomSMSLambdaVersionConfigType)
lambdaConfigType_customSMSSender = Lens.lens (\LambdaConfigType' {customSMSSender} -> customSMSSender) (\s@LambdaConfigType' {} a -> s {customSMSSender = a} :: LambdaConfigType)

-- | Creates an authentication challenge.
lambdaConfigType_createAuthChallenge :: Lens.Lens' LambdaConfigType (Prelude.Maybe Prelude.Text)
lambdaConfigType_createAuthChallenge = Lens.lens (\LambdaConfigType' {createAuthChallenge} -> createAuthChallenge) (\s@LambdaConfigType' {} a -> s {createAuthChallenge = a} :: LambdaConfigType)

-- | A post-confirmation AWS Lambda trigger.
lambdaConfigType_postConfirmation :: Lens.Lens' LambdaConfigType (Prelude.Maybe Prelude.Text)
lambdaConfigType_postConfirmation = Lens.lens (\LambdaConfigType' {postConfirmation} -> postConfirmation) (\s@LambdaConfigType' {} a -> s {postConfirmation = a} :: LambdaConfigType)

-- | A pre-authentication AWS Lambda trigger.
lambdaConfigType_preAuthentication :: Lens.Lens' LambdaConfigType (Prelude.Maybe Prelude.Text)
lambdaConfigType_preAuthentication = Lens.lens (\LambdaConfigType' {preAuthentication} -> preAuthentication) (\s@LambdaConfigType' {} a -> s {preAuthentication = a} :: LambdaConfigType)

-- | The Amazon Resource Name of Key Management Service
-- </kms/latest/developerguide/concepts.html#master_keys Customer master keys>
-- . Amazon Cognito uses the key to encrypt codes and temporary passwords
-- sent to @CustomEmailSender@ and @CustomSMSSender@.
lambdaConfigType_kmsKeyID :: Lens.Lens' LambdaConfigType (Prelude.Maybe Prelude.Text)
lambdaConfigType_kmsKeyID = Lens.lens (\LambdaConfigType' {kmsKeyID} -> kmsKeyID) (\s@LambdaConfigType' {} a -> s {kmsKeyID = a} :: LambdaConfigType)

-- | Verifies the authentication challenge response.
lambdaConfigType_verifyAuthChallengeResponse :: Lens.Lens' LambdaConfigType (Prelude.Maybe Prelude.Text)
lambdaConfigType_verifyAuthChallengeResponse = Lens.lens (\LambdaConfigType' {verifyAuthChallengeResponse} -> verifyAuthChallengeResponse) (\s@LambdaConfigType' {} a -> s {verifyAuthChallengeResponse = a} :: LambdaConfigType)

-- | A custom Message AWS Lambda trigger.
lambdaConfigType_customMessage :: Lens.Lens' LambdaConfigType (Prelude.Maybe Prelude.Text)
lambdaConfigType_customMessage = Lens.lens (\LambdaConfigType' {customMessage} -> customMessage) (\s@LambdaConfigType' {} a -> s {customMessage = a} :: LambdaConfigType)

-- | The user migration Lambda config type.
lambdaConfigType_userMigration :: Lens.Lens' LambdaConfigType (Prelude.Maybe Prelude.Text)
lambdaConfigType_userMigration = Lens.lens (\LambdaConfigType' {userMigration} -> userMigration) (\s@LambdaConfigType' {} a -> s {userMigration = a} :: LambdaConfigType)

-- | A Lambda trigger that is invoked before token generation.
lambdaConfigType_preTokenGeneration :: Lens.Lens' LambdaConfigType (Prelude.Maybe Prelude.Text)
lambdaConfigType_preTokenGeneration = Lens.lens (\LambdaConfigType' {preTokenGeneration} -> preTokenGeneration) (\s@LambdaConfigType' {} a -> s {preTokenGeneration = a} :: LambdaConfigType)

instance Prelude.FromJSON LambdaConfigType where
  parseJSON =
    Prelude.withObject
      "LambdaConfigType"
      ( \x ->
          LambdaConfigType'
            Prelude.<$> (x Prelude..:? "CustomEmailSender")
            Prelude.<*> (x Prelude..:? "PreSignUp")
            Prelude.<*> (x Prelude..:? "DefineAuthChallenge")
            Prelude.<*> (x Prelude..:? "PostAuthentication")
            Prelude.<*> (x Prelude..:? "CustomSMSSender")
            Prelude.<*> (x Prelude..:? "CreateAuthChallenge")
            Prelude.<*> (x Prelude..:? "PostConfirmation")
            Prelude.<*> (x Prelude..:? "PreAuthentication")
            Prelude.<*> (x Prelude..:? "KMSKeyID")
            Prelude.<*> (x Prelude..:? "VerifyAuthChallengeResponse")
            Prelude.<*> (x Prelude..:? "CustomMessage")
            Prelude.<*> (x Prelude..:? "UserMigration")
            Prelude.<*> (x Prelude..:? "PreTokenGeneration")
      )

instance Prelude.Hashable LambdaConfigType

instance Prelude.NFData LambdaConfigType

instance Prelude.ToJSON LambdaConfigType where
  toJSON LambdaConfigType' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CustomEmailSender" Prelude..=)
              Prelude.<$> customEmailSender,
            ("PreSignUp" Prelude..=) Prelude.<$> preSignUp,
            ("DefineAuthChallenge" Prelude..=)
              Prelude.<$> defineAuthChallenge,
            ("PostAuthentication" Prelude..=)
              Prelude.<$> postAuthentication,
            ("CustomSMSSender" Prelude..=)
              Prelude.<$> customSMSSender,
            ("CreateAuthChallenge" Prelude..=)
              Prelude.<$> createAuthChallenge,
            ("PostConfirmation" Prelude..=)
              Prelude.<$> postConfirmation,
            ("PreAuthentication" Prelude..=)
              Prelude.<$> preAuthentication,
            ("KMSKeyID" Prelude..=) Prelude.<$> kmsKeyID,
            ("VerifyAuthChallengeResponse" Prelude..=)
              Prelude.<$> verifyAuthChallengeResponse,
            ("CustomMessage" Prelude..=)
              Prelude.<$> customMessage,
            ("UserMigration" Prelude..=)
              Prelude.<$> userMigration,
            ("PreTokenGeneration" Prelude..=)
              Prelude.<$> preTokenGeneration
          ]
      )
