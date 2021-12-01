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
-- Module      : Amazonka.CognitoIdentityProvider.Types.LambdaConfigType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.LambdaConfigType where

import Amazonka.CognitoIdentityProvider.Types.CustomEmailLambdaVersionConfigType
import Amazonka.CognitoIdentityProvider.Types.CustomSMSLambdaVersionConfigType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies the configuration for Lambda triggers.
--
-- /See:/ 'newLambdaConfigType' smart constructor.
data LambdaConfigType = LambdaConfigType'
  { -- | A pre-authentication Lambda trigger.
    preAuthentication :: Prelude.Maybe Prelude.Text,
    -- | Creates an authentication challenge.
    createAuthChallenge :: Prelude.Maybe Prelude.Text,
    -- | Verifies the authentication challenge response.
    verifyAuthChallengeResponse :: Prelude.Maybe Prelude.Text,
    -- | A custom SMS sender Lambda trigger.
    customSMSSender :: Prelude.Maybe CustomSMSLambdaVersionConfigType,
    -- | A post-authentication Lambda trigger.
    postAuthentication :: Prelude.Maybe Prelude.Text,
    -- | A custom Message Lambda trigger.
    customMessage :: Prelude.Maybe Prelude.Text,
    -- | Defines the authentication challenge.
    defineAuthChallenge :: Prelude.Maybe Prelude.Text,
    -- | A custom email sender Lambda trigger.
    customEmailSender :: Prelude.Maybe CustomEmailLambdaVersionConfigType,
    -- | The Amazon Resource Name of Key Management Service
    -- </kms/latest/developerguide/concepts.html#master_keys Customer master keys>
    -- . Amazon Cognito uses the key to encrypt codes and temporary passwords
    -- sent to @CustomEmailSender@ and @CustomSMSSender@.
    kmsKeyID :: Prelude.Maybe Prelude.Text,
    -- | A post-confirmation Lambda trigger.
    postConfirmation :: Prelude.Maybe Prelude.Text,
    -- | A Lambda trigger that is invoked before token generation.
    preTokenGeneration :: Prelude.Maybe Prelude.Text,
    -- | The user migration Lambda config type.
    userMigration :: Prelude.Maybe Prelude.Text,
    -- | A pre-registration Lambda trigger.
    preSignUp :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaConfigType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'preAuthentication', 'lambdaConfigType_preAuthentication' - A pre-authentication Lambda trigger.
--
-- 'createAuthChallenge', 'lambdaConfigType_createAuthChallenge' - Creates an authentication challenge.
--
-- 'verifyAuthChallengeResponse', 'lambdaConfigType_verifyAuthChallengeResponse' - Verifies the authentication challenge response.
--
-- 'customSMSSender', 'lambdaConfigType_customSMSSender' - A custom SMS sender Lambda trigger.
--
-- 'postAuthentication', 'lambdaConfigType_postAuthentication' - A post-authentication Lambda trigger.
--
-- 'customMessage', 'lambdaConfigType_customMessage' - A custom Message Lambda trigger.
--
-- 'defineAuthChallenge', 'lambdaConfigType_defineAuthChallenge' - Defines the authentication challenge.
--
-- 'customEmailSender', 'lambdaConfigType_customEmailSender' - A custom email sender Lambda trigger.
--
-- 'kmsKeyID', 'lambdaConfigType_kmsKeyID' - The Amazon Resource Name of Key Management Service
-- </kms/latest/developerguide/concepts.html#master_keys Customer master keys>
-- . Amazon Cognito uses the key to encrypt codes and temporary passwords
-- sent to @CustomEmailSender@ and @CustomSMSSender@.
--
-- 'postConfirmation', 'lambdaConfigType_postConfirmation' - A post-confirmation Lambda trigger.
--
-- 'preTokenGeneration', 'lambdaConfigType_preTokenGeneration' - A Lambda trigger that is invoked before token generation.
--
-- 'userMigration', 'lambdaConfigType_userMigration' - The user migration Lambda config type.
--
-- 'preSignUp', 'lambdaConfigType_preSignUp' - A pre-registration Lambda trigger.
newLambdaConfigType ::
  LambdaConfigType
newLambdaConfigType =
  LambdaConfigType'
    { preAuthentication =
        Prelude.Nothing,
      createAuthChallenge = Prelude.Nothing,
      verifyAuthChallengeResponse = Prelude.Nothing,
      customSMSSender = Prelude.Nothing,
      postAuthentication = Prelude.Nothing,
      customMessage = Prelude.Nothing,
      defineAuthChallenge = Prelude.Nothing,
      customEmailSender = Prelude.Nothing,
      kmsKeyID = Prelude.Nothing,
      postConfirmation = Prelude.Nothing,
      preTokenGeneration = Prelude.Nothing,
      userMigration = Prelude.Nothing,
      preSignUp = Prelude.Nothing
    }

-- | A pre-authentication Lambda trigger.
lambdaConfigType_preAuthentication :: Lens.Lens' LambdaConfigType (Prelude.Maybe Prelude.Text)
lambdaConfigType_preAuthentication = Lens.lens (\LambdaConfigType' {preAuthentication} -> preAuthentication) (\s@LambdaConfigType' {} a -> s {preAuthentication = a} :: LambdaConfigType)

-- | Creates an authentication challenge.
lambdaConfigType_createAuthChallenge :: Lens.Lens' LambdaConfigType (Prelude.Maybe Prelude.Text)
lambdaConfigType_createAuthChallenge = Lens.lens (\LambdaConfigType' {createAuthChallenge} -> createAuthChallenge) (\s@LambdaConfigType' {} a -> s {createAuthChallenge = a} :: LambdaConfigType)

-- | Verifies the authentication challenge response.
lambdaConfigType_verifyAuthChallengeResponse :: Lens.Lens' LambdaConfigType (Prelude.Maybe Prelude.Text)
lambdaConfigType_verifyAuthChallengeResponse = Lens.lens (\LambdaConfigType' {verifyAuthChallengeResponse} -> verifyAuthChallengeResponse) (\s@LambdaConfigType' {} a -> s {verifyAuthChallengeResponse = a} :: LambdaConfigType)

-- | A custom SMS sender Lambda trigger.
lambdaConfigType_customSMSSender :: Lens.Lens' LambdaConfigType (Prelude.Maybe CustomSMSLambdaVersionConfigType)
lambdaConfigType_customSMSSender = Lens.lens (\LambdaConfigType' {customSMSSender} -> customSMSSender) (\s@LambdaConfigType' {} a -> s {customSMSSender = a} :: LambdaConfigType)

-- | A post-authentication Lambda trigger.
lambdaConfigType_postAuthentication :: Lens.Lens' LambdaConfigType (Prelude.Maybe Prelude.Text)
lambdaConfigType_postAuthentication = Lens.lens (\LambdaConfigType' {postAuthentication} -> postAuthentication) (\s@LambdaConfigType' {} a -> s {postAuthentication = a} :: LambdaConfigType)

-- | A custom Message Lambda trigger.
lambdaConfigType_customMessage :: Lens.Lens' LambdaConfigType (Prelude.Maybe Prelude.Text)
lambdaConfigType_customMessage = Lens.lens (\LambdaConfigType' {customMessage} -> customMessage) (\s@LambdaConfigType' {} a -> s {customMessage = a} :: LambdaConfigType)

-- | Defines the authentication challenge.
lambdaConfigType_defineAuthChallenge :: Lens.Lens' LambdaConfigType (Prelude.Maybe Prelude.Text)
lambdaConfigType_defineAuthChallenge = Lens.lens (\LambdaConfigType' {defineAuthChallenge} -> defineAuthChallenge) (\s@LambdaConfigType' {} a -> s {defineAuthChallenge = a} :: LambdaConfigType)

-- | A custom email sender Lambda trigger.
lambdaConfigType_customEmailSender :: Lens.Lens' LambdaConfigType (Prelude.Maybe CustomEmailLambdaVersionConfigType)
lambdaConfigType_customEmailSender = Lens.lens (\LambdaConfigType' {customEmailSender} -> customEmailSender) (\s@LambdaConfigType' {} a -> s {customEmailSender = a} :: LambdaConfigType)

-- | The Amazon Resource Name of Key Management Service
-- </kms/latest/developerguide/concepts.html#master_keys Customer master keys>
-- . Amazon Cognito uses the key to encrypt codes and temporary passwords
-- sent to @CustomEmailSender@ and @CustomSMSSender@.
lambdaConfigType_kmsKeyID :: Lens.Lens' LambdaConfigType (Prelude.Maybe Prelude.Text)
lambdaConfigType_kmsKeyID = Lens.lens (\LambdaConfigType' {kmsKeyID} -> kmsKeyID) (\s@LambdaConfigType' {} a -> s {kmsKeyID = a} :: LambdaConfigType)

-- | A post-confirmation Lambda trigger.
lambdaConfigType_postConfirmation :: Lens.Lens' LambdaConfigType (Prelude.Maybe Prelude.Text)
lambdaConfigType_postConfirmation = Lens.lens (\LambdaConfigType' {postConfirmation} -> postConfirmation) (\s@LambdaConfigType' {} a -> s {postConfirmation = a} :: LambdaConfigType)

-- | A Lambda trigger that is invoked before token generation.
lambdaConfigType_preTokenGeneration :: Lens.Lens' LambdaConfigType (Prelude.Maybe Prelude.Text)
lambdaConfigType_preTokenGeneration = Lens.lens (\LambdaConfigType' {preTokenGeneration} -> preTokenGeneration) (\s@LambdaConfigType' {} a -> s {preTokenGeneration = a} :: LambdaConfigType)

-- | The user migration Lambda config type.
lambdaConfigType_userMigration :: Lens.Lens' LambdaConfigType (Prelude.Maybe Prelude.Text)
lambdaConfigType_userMigration = Lens.lens (\LambdaConfigType' {userMigration} -> userMigration) (\s@LambdaConfigType' {} a -> s {userMigration = a} :: LambdaConfigType)

-- | A pre-registration Lambda trigger.
lambdaConfigType_preSignUp :: Lens.Lens' LambdaConfigType (Prelude.Maybe Prelude.Text)
lambdaConfigType_preSignUp = Lens.lens (\LambdaConfigType' {preSignUp} -> preSignUp) (\s@LambdaConfigType' {} a -> s {preSignUp = a} :: LambdaConfigType)

instance Core.FromJSON LambdaConfigType where
  parseJSON =
    Core.withObject
      "LambdaConfigType"
      ( \x ->
          LambdaConfigType'
            Prelude.<$> (x Core..:? "PreAuthentication")
            Prelude.<*> (x Core..:? "CreateAuthChallenge")
            Prelude.<*> (x Core..:? "VerifyAuthChallengeResponse")
            Prelude.<*> (x Core..:? "CustomSMSSender")
            Prelude.<*> (x Core..:? "PostAuthentication")
            Prelude.<*> (x Core..:? "CustomMessage")
            Prelude.<*> (x Core..:? "DefineAuthChallenge")
            Prelude.<*> (x Core..:? "CustomEmailSender")
            Prelude.<*> (x Core..:? "KMSKeyID")
            Prelude.<*> (x Core..:? "PostConfirmation")
            Prelude.<*> (x Core..:? "PreTokenGeneration")
            Prelude.<*> (x Core..:? "UserMigration")
            Prelude.<*> (x Core..:? "PreSignUp")
      )

instance Prelude.Hashable LambdaConfigType where
  hashWithSalt salt' LambdaConfigType' {..} =
    salt' `Prelude.hashWithSalt` preSignUp
      `Prelude.hashWithSalt` userMigration
      `Prelude.hashWithSalt` preTokenGeneration
      `Prelude.hashWithSalt` postConfirmation
      `Prelude.hashWithSalt` kmsKeyID
      `Prelude.hashWithSalt` customEmailSender
      `Prelude.hashWithSalt` defineAuthChallenge
      `Prelude.hashWithSalt` customMessage
      `Prelude.hashWithSalt` postAuthentication
      `Prelude.hashWithSalt` customSMSSender
      `Prelude.hashWithSalt` verifyAuthChallengeResponse
      `Prelude.hashWithSalt` createAuthChallenge
      `Prelude.hashWithSalt` preAuthentication

instance Prelude.NFData LambdaConfigType where
  rnf LambdaConfigType' {..} =
    Prelude.rnf preAuthentication
      `Prelude.seq` Prelude.rnf preSignUp
      `Prelude.seq` Prelude.rnf userMigration
      `Prelude.seq` Prelude.rnf preTokenGeneration
      `Prelude.seq` Prelude.rnf postConfirmation
      `Prelude.seq` Prelude.rnf kmsKeyID
      `Prelude.seq` Prelude.rnf customEmailSender
      `Prelude.seq` Prelude.rnf defineAuthChallenge
      `Prelude.seq` Prelude.rnf customMessage
      `Prelude.seq` Prelude.rnf postAuthentication
      `Prelude.seq` Prelude.rnf customSMSSender
      `Prelude.seq` Prelude.rnf verifyAuthChallengeResponse
      `Prelude.seq` Prelude.rnf createAuthChallenge

instance Core.ToJSON LambdaConfigType where
  toJSON LambdaConfigType' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PreAuthentication" Core..=)
              Prelude.<$> preAuthentication,
            ("CreateAuthChallenge" Core..=)
              Prelude.<$> createAuthChallenge,
            ("VerifyAuthChallengeResponse" Core..=)
              Prelude.<$> verifyAuthChallengeResponse,
            ("CustomSMSSender" Core..=)
              Prelude.<$> customSMSSender,
            ("PostAuthentication" Core..=)
              Prelude.<$> postAuthentication,
            ("CustomMessage" Core..=) Prelude.<$> customMessage,
            ("DefineAuthChallenge" Core..=)
              Prelude.<$> defineAuthChallenge,
            ("CustomEmailSender" Core..=)
              Prelude.<$> customEmailSender,
            ("KMSKeyID" Core..=) Prelude.<$> kmsKeyID,
            ("PostConfirmation" Core..=)
              Prelude.<$> postConfirmation,
            ("PreTokenGeneration" Core..=)
              Prelude.<$> preTokenGeneration,
            ("UserMigration" Core..=) Prelude.<$> userMigration,
            ("PreSignUp" Core..=) Prelude.<$> preSignUp
          ]
      )
