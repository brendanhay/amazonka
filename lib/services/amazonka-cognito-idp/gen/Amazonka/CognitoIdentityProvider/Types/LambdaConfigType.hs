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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.LambdaConfigType where

import Amazonka.CognitoIdentityProvider.Types.CustomEmailLambdaVersionConfigType
import Amazonka.CognitoIdentityProvider.Types.CustomSMSLambdaVersionConfigType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the configuration for Lambda triggers.
--
-- /See:/ 'newLambdaConfigType' smart constructor.
data LambdaConfigType = LambdaConfigType'
  { -- | Creates an authentication challenge.
    createAuthChallenge :: Prelude.Maybe Prelude.Text,
    -- | A custom email sender Lambda trigger.
    customEmailSender :: Prelude.Maybe CustomEmailLambdaVersionConfigType,
    -- | A custom Message Lambda trigger.
    customMessage :: Prelude.Maybe Prelude.Text,
    -- | A custom SMS sender Lambda trigger.
    customSMSSender :: Prelude.Maybe CustomSMSLambdaVersionConfigType,
    -- | Defines the authentication challenge.
    defineAuthChallenge :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an
    -- </kms/latest/developerguide/concepts.html#master_keys KMS key>. Amazon
    -- Cognito uses the key to encrypt codes and temporary passwords sent to
    -- @CustomEmailSender@ and @CustomSMSSender@.
    kmsKeyID :: Prelude.Maybe Prelude.Text,
    -- | A post-authentication Lambda trigger.
    postAuthentication :: Prelude.Maybe Prelude.Text,
    -- | A post-confirmation Lambda trigger.
    postConfirmation :: Prelude.Maybe Prelude.Text,
    -- | A pre-authentication Lambda trigger.
    preAuthentication :: Prelude.Maybe Prelude.Text,
    -- | A pre-registration Lambda trigger.
    preSignUp :: Prelude.Maybe Prelude.Text,
    -- | A Lambda trigger that is invoked before token generation.
    preTokenGeneration :: Prelude.Maybe Prelude.Text,
    -- | The user migration Lambda config type.
    userMigration :: Prelude.Maybe Prelude.Text,
    -- | Verifies the authentication challenge response.
    verifyAuthChallengeResponse :: Prelude.Maybe Prelude.Text
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
-- 'createAuthChallenge', 'lambdaConfigType_createAuthChallenge' - Creates an authentication challenge.
--
-- 'customEmailSender', 'lambdaConfigType_customEmailSender' - A custom email sender Lambda trigger.
--
-- 'customMessage', 'lambdaConfigType_customMessage' - A custom Message Lambda trigger.
--
-- 'customSMSSender', 'lambdaConfigType_customSMSSender' - A custom SMS sender Lambda trigger.
--
-- 'defineAuthChallenge', 'lambdaConfigType_defineAuthChallenge' - Defines the authentication challenge.
--
-- 'kmsKeyID', 'lambdaConfigType_kmsKeyID' - The Amazon Resource Name (ARN) of an
-- </kms/latest/developerguide/concepts.html#master_keys KMS key>. Amazon
-- Cognito uses the key to encrypt codes and temporary passwords sent to
-- @CustomEmailSender@ and @CustomSMSSender@.
--
-- 'postAuthentication', 'lambdaConfigType_postAuthentication' - A post-authentication Lambda trigger.
--
-- 'postConfirmation', 'lambdaConfigType_postConfirmation' - A post-confirmation Lambda trigger.
--
-- 'preAuthentication', 'lambdaConfigType_preAuthentication' - A pre-authentication Lambda trigger.
--
-- 'preSignUp', 'lambdaConfigType_preSignUp' - A pre-registration Lambda trigger.
--
-- 'preTokenGeneration', 'lambdaConfigType_preTokenGeneration' - A Lambda trigger that is invoked before token generation.
--
-- 'userMigration', 'lambdaConfigType_userMigration' - The user migration Lambda config type.
--
-- 'verifyAuthChallengeResponse', 'lambdaConfigType_verifyAuthChallengeResponse' - Verifies the authentication challenge response.
newLambdaConfigType ::
  LambdaConfigType
newLambdaConfigType =
  LambdaConfigType'
    { createAuthChallenge =
        Prelude.Nothing,
      customEmailSender = Prelude.Nothing,
      customMessage = Prelude.Nothing,
      customSMSSender = Prelude.Nothing,
      defineAuthChallenge = Prelude.Nothing,
      kmsKeyID = Prelude.Nothing,
      postAuthentication = Prelude.Nothing,
      postConfirmation = Prelude.Nothing,
      preAuthentication = Prelude.Nothing,
      preSignUp = Prelude.Nothing,
      preTokenGeneration = Prelude.Nothing,
      userMigration = Prelude.Nothing,
      verifyAuthChallengeResponse = Prelude.Nothing
    }

-- | Creates an authentication challenge.
lambdaConfigType_createAuthChallenge :: Lens.Lens' LambdaConfigType (Prelude.Maybe Prelude.Text)
lambdaConfigType_createAuthChallenge = Lens.lens (\LambdaConfigType' {createAuthChallenge} -> createAuthChallenge) (\s@LambdaConfigType' {} a -> s {createAuthChallenge = a} :: LambdaConfigType)

-- | A custom email sender Lambda trigger.
lambdaConfigType_customEmailSender :: Lens.Lens' LambdaConfigType (Prelude.Maybe CustomEmailLambdaVersionConfigType)
lambdaConfigType_customEmailSender = Lens.lens (\LambdaConfigType' {customEmailSender} -> customEmailSender) (\s@LambdaConfigType' {} a -> s {customEmailSender = a} :: LambdaConfigType)

-- | A custom Message Lambda trigger.
lambdaConfigType_customMessage :: Lens.Lens' LambdaConfigType (Prelude.Maybe Prelude.Text)
lambdaConfigType_customMessage = Lens.lens (\LambdaConfigType' {customMessage} -> customMessage) (\s@LambdaConfigType' {} a -> s {customMessage = a} :: LambdaConfigType)

-- | A custom SMS sender Lambda trigger.
lambdaConfigType_customSMSSender :: Lens.Lens' LambdaConfigType (Prelude.Maybe CustomSMSLambdaVersionConfigType)
lambdaConfigType_customSMSSender = Lens.lens (\LambdaConfigType' {customSMSSender} -> customSMSSender) (\s@LambdaConfigType' {} a -> s {customSMSSender = a} :: LambdaConfigType)

-- | Defines the authentication challenge.
lambdaConfigType_defineAuthChallenge :: Lens.Lens' LambdaConfigType (Prelude.Maybe Prelude.Text)
lambdaConfigType_defineAuthChallenge = Lens.lens (\LambdaConfigType' {defineAuthChallenge} -> defineAuthChallenge) (\s@LambdaConfigType' {} a -> s {defineAuthChallenge = a} :: LambdaConfigType)

-- | The Amazon Resource Name (ARN) of an
-- </kms/latest/developerguide/concepts.html#master_keys KMS key>. Amazon
-- Cognito uses the key to encrypt codes and temporary passwords sent to
-- @CustomEmailSender@ and @CustomSMSSender@.
lambdaConfigType_kmsKeyID :: Lens.Lens' LambdaConfigType (Prelude.Maybe Prelude.Text)
lambdaConfigType_kmsKeyID = Lens.lens (\LambdaConfigType' {kmsKeyID} -> kmsKeyID) (\s@LambdaConfigType' {} a -> s {kmsKeyID = a} :: LambdaConfigType)

-- | A post-authentication Lambda trigger.
lambdaConfigType_postAuthentication :: Lens.Lens' LambdaConfigType (Prelude.Maybe Prelude.Text)
lambdaConfigType_postAuthentication = Lens.lens (\LambdaConfigType' {postAuthentication} -> postAuthentication) (\s@LambdaConfigType' {} a -> s {postAuthentication = a} :: LambdaConfigType)

-- | A post-confirmation Lambda trigger.
lambdaConfigType_postConfirmation :: Lens.Lens' LambdaConfigType (Prelude.Maybe Prelude.Text)
lambdaConfigType_postConfirmation = Lens.lens (\LambdaConfigType' {postConfirmation} -> postConfirmation) (\s@LambdaConfigType' {} a -> s {postConfirmation = a} :: LambdaConfigType)

-- | A pre-authentication Lambda trigger.
lambdaConfigType_preAuthentication :: Lens.Lens' LambdaConfigType (Prelude.Maybe Prelude.Text)
lambdaConfigType_preAuthentication = Lens.lens (\LambdaConfigType' {preAuthentication} -> preAuthentication) (\s@LambdaConfigType' {} a -> s {preAuthentication = a} :: LambdaConfigType)

-- | A pre-registration Lambda trigger.
lambdaConfigType_preSignUp :: Lens.Lens' LambdaConfigType (Prelude.Maybe Prelude.Text)
lambdaConfigType_preSignUp = Lens.lens (\LambdaConfigType' {preSignUp} -> preSignUp) (\s@LambdaConfigType' {} a -> s {preSignUp = a} :: LambdaConfigType)

-- | A Lambda trigger that is invoked before token generation.
lambdaConfigType_preTokenGeneration :: Lens.Lens' LambdaConfigType (Prelude.Maybe Prelude.Text)
lambdaConfigType_preTokenGeneration = Lens.lens (\LambdaConfigType' {preTokenGeneration} -> preTokenGeneration) (\s@LambdaConfigType' {} a -> s {preTokenGeneration = a} :: LambdaConfigType)

-- | The user migration Lambda config type.
lambdaConfigType_userMigration :: Lens.Lens' LambdaConfigType (Prelude.Maybe Prelude.Text)
lambdaConfigType_userMigration = Lens.lens (\LambdaConfigType' {userMigration} -> userMigration) (\s@LambdaConfigType' {} a -> s {userMigration = a} :: LambdaConfigType)

-- | Verifies the authentication challenge response.
lambdaConfigType_verifyAuthChallengeResponse :: Lens.Lens' LambdaConfigType (Prelude.Maybe Prelude.Text)
lambdaConfigType_verifyAuthChallengeResponse = Lens.lens (\LambdaConfigType' {verifyAuthChallengeResponse} -> verifyAuthChallengeResponse) (\s@LambdaConfigType' {} a -> s {verifyAuthChallengeResponse = a} :: LambdaConfigType)

instance Data.FromJSON LambdaConfigType where
  parseJSON =
    Data.withObject
      "LambdaConfigType"
      ( \x ->
          LambdaConfigType'
            Prelude.<$> (x Data..:? "CreateAuthChallenge")
            Prelude.<*> (x Data..:? "CustomEmailSender")
            Prelude.<*> (x Data..:? "CustomMessage")
            Prelude.<*> (x Data..:? "CustomSMSSender")
            Prelude.<*> (x Data..:? "DefineAuthChallenge")
            Prelude.<*> (x Data..:? "KMSKeyID")
            Prelude.<*> (x Data..:? "PostAuthentication")
            Prelude.<*> (x Data..:? "PostConfirmation")
            Prelude.<*> (x Data..:? "PreAuthentication")
            Prelude.<*> (x Data..:? "PreSignUp")
            Prelude.<*> (x Data..:? "PreTokenGeneration")
            Prelude.<*> (x Data..:? "UserMigration")
            Prelude.<*> (x Data..:? "VerifyAuthChallengeResponse")
      )

instance Prelude.Hashable LambdaConfigType where
  hashWithSalt _salt LambdaConfigType' {..} =
    _salt `Prelude.hashWithSalt` createAuthChallenge
      `Prelude.hashWithSalt` customEmailSender
      `Prelude.hashWithSalt` customMessage
      `Prelude.hashWithSalt` customSMSSender
      `Prelude.hashWithSalt` defineAuthChallenge
      `Prelude.hashWithSalt` kmsKeyID
      `Prelude.hashWithSalt` postAuthentication
      `Prelude.hashWithSalt` postConfirmation
      `Prelude.hashWithSalt` preAuthentication
      `Prelude.hashWithSalt` preSignUp
      `Prelude.hashWithSalt` preTokenGeneration
      `Prelude.hashWithSalt` userMigration
      `Prelude.hashWithSalt` verifyAuthChallengeResponse

instance Prelude.NFData LambdaConfigType where
  rnf LambdaConfigType' {..} =
    Prelude.rnf createAuthChallenge
      `Prelude.seq` Prelude.rnf customEmailSender
      `Prelude.seq` Prelude.rnf customMessage
      `Prelude.seq` Prelude.rnf customSMSSender
      `Prelude.seq` Prelude.rnf defineAuthChallenge
      `Prelude.seq` Prelude.rnf kmsKeyID
      `Prelude.seq` Prelude.rnf postAuthentication
      `Prelude.seq` Prelude.rnf postConfirmation
      `Prelude.seq` Prelude.rnf preAuthentication
      `Prelude.seq` Prelude.rnf preSignUp
      `Prelude.seq` Prelude.rnf preTokenGeneration
      `Prelude.seq` Prelude.rnf userMigration
      `Prelude.seq` Prelude.rnf verifyAuthChallengeResponse

instance Data.ToJSON LambdaConfigType where
  toJSON LambdaConfigType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CreateAuthChallenge" Data..=)
              Prelude.<$> createAuthChallenge,
            ("CustomEmailSender" Data..=)
              Prelude.<$> customEmailSender,
            ("CustomMessage" Data..=) Prelude.<$> customMessage,
            ("CustomSMSSender" Data..=)
              Prelude.<$> customSMSSender,
            ("DefineAuthChallenge" Data..=)
              Prelude.<$> defineAuthChallenge,
            ("KMSKeyID" Data..=) Prelude.<$> kmsKeyID,
            ("PostAuthentication" Data..=)
              Prelude.<$> postAuthentication,
            ("PostConfirmation" Data..=)
              Prelude.<$> postConfirmation,
            ("PreAuthentication" Data..=)
              Prelude.<$> preAuthentication,
            ("PreSignUp" Data..=) Prelude.<$> preSignUp,
            ("PreTokenGeneration" Data..=)
              Prelude.<$> preTokenGeneration,
            ("UserMigration" Data..=) Prelude.<$> userMigration,
            ("VerifyAuthChallengeResponse" Data..=)
              Prelude.<$> verifyAuthChallengeResponse
          ]
      )
