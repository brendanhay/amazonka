{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.LambdaConfigType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.LambdaConfigType
  ( LambdaConfigType (..),

    -- * Smart constructor
    mkLambdaConfigType,

    -- * Lenses
    lctPreAuthentication,
    lctCreateAuthChallenge,
    lctVerifyAuthChallengeResponse,
    lctCustomSMSSender,
    lctPostAuthentication,
    lctCustomMessage,
    lctDefineAuthChallenge,
    lctCustomEmailSender,
    lctKMSKeyId,
    lctPostConfirmation,
    lctPreTokenGeneration,
    lctUserMigration,
    lctPreSignUp,
  )
where

import Network.AWS.CognitoIdentityProvider.Types.CustomEmailLambdaVersionConfigType
import Network.AWS.CognitoIdentityProvider.Types.CustomSMSLambdaVersionConfigType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the configuration for AWS Lambda triggers.
--
-- /See:/ 'mkLambdaConfigType' smart constructor.
data LambdaConfigType = LambdaConfigType'
  { -- | A pre-authentication AWS Lambda trigger.
    preAuthentication :: Lude.Maybe Lude.Text,
    -- | Creates an authentication challenge.
    createAuthChallenge :: Lude.Maybe Lude.Text,
    -- | Verifies the authentication challenge response.
    verifyAuthChallengeResponse :: Lude.Maybe Lude.Text,
    -- | A custom SMS sender AWS Lambda trigger.
    customSMSSender :: Lude.Maybe CustomSMSLambdaVersionConfigType,
    -- | A post-authentication AWS Lambda trigger.
    postAuthentication :: Lude.Maybe Lude.Text,
    -- | A custom Message AWS Lambda trigger.
    customMessage :: Lude.Maybe Lude.Text,
    -- | Defines the authentication challenge.
    defineAuthChallenge :: Lude.Maybe Lude.Text,
    -- | A custom email sender AWS Lambda trigger.
    customEmailSender :: Lude.Maybe CustomEmailLambdaVersionConfigType,
    -- | The Amazon Resource Name of Key Management Service </kms/latest/developerguide/concepts.html#master_keys Customer master keys> . Amazon Cognito uses the key to encrypt codes and temporary passwords sent to @CustomEmailSender@ and @CustomSMSSender@ .
    kmsKeyId :: Lude.Maybe Lude.Text,
    -- | A post-confirmation AWS Lambda trigger.
    postConfirmation :: Lude.Maybe Lude.Text,
    -- | A Lambda trigger that is invoked before token generation.
    preTokenGeneration :: Lude.Maybe Lude.Text,
    -- | The user migration Lambda config type.
    userMigration :: Lude.Maybe Lude.Text,
    -- | A pre-registration AWS Lambda trigger.
    preSignUp :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LambdaConfigType' with the minimum fields required to make a request.
--
-- * 'preAuthentication' - A pre-authentication AWS Lambda trigger.
-- * 'createAuthChallenge' - Creates an authentication challenge.
-- * 'verifyAuthChallengeResponse' - Verifies the authentication challenge response.
-- * 'customSMSSender' - A custom SMS sender AWS Lambda trigger.
-- * 'postAuthentication' - A post-authentication AWS Lambda trigger.
-- * 'customMessage' - A custom Message AWS Lambda trigger.
-- * 'defineAuthChallenge' - Defines the authentication challenge.
-- * 'customEmailSender' - A custom email sender AWS Lambda trigger.
-- * 'kmsKeyId' - The Amazon Resource Name of Key Management Service </kms/latest/developerguide/concepts.html#master_keys Customer master keys> . Amazon Cognito uses the key to encrypt codes and temporary passwords sent to @CustomEmailSender@ and @CustomSMSSender@ .
-- * 'postConfirmation' - A post-confirmation AWS Lambda trigger.
-- * 'preTokenGeneration' - A Lambda trigger that is invoked before token generation.
-- * 'userMigration' - The user migration Lambda config type.
-- * 'preSignUp' - A pre-registration AWS Lambda trigger.
mkLambdaConfigType ::
  LambdaConfigType
mkLambdaConfigType =
  LambdaConfigType'
    { preAuthentication = Lude.Nothing,
      createAuthChallenge = Lude.Nothing,
      verifyAuthChallengeResponse = Lude.Nothing,
      customSMSSender = Lude.Nothing,
      postAuthentication = Lude.Nothing,
      customMessage = Lude.Nothing,
      defineAuthChallenge = Lude.Nothing,
      customEmailSender = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      postConfirmation = Lude.Nothing,
      preTokenGeneration = Lude.Nothing,
      userMigration = Lude.Nothing,
      preSignUp = Lude.Nothing
    }

-- | A pre-authentication AWS Lambda trigger.
--
-- /Note:/ Consider using 'preAuthentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lctPreAuthentication :: Lens.Lens' LambdaConfigType (Lude.Maybe Lude.Text)
lctPreAuthentication = Lens.lens (preAuthentication :: LambdaConfigType -> Lude.Maybe Lude.Text) (\s a -> s {preAuthentication = a} :: LambdaConfigType)
{-# DEPRECATED lctPreAuthentication "Use generic-lens or generic-optics with 'preAuthentication' instead." #-}

-- | Creates an authentication challenge.
--
-- /Note:/ Consider using 'createAuthChallenge' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lctCreateAuthChallenge :: Lens.Lens' LambdaConfigType (Lude.Maybe Lude.Text)
lctCreateAuthChallenge = Lens.lens (createAuthChallenge :: LambdaConfigType -> Lude.Maybe Lude.Text) (\s a -> s {createAuthChallenge = a} :: LambdaConfigType)
{-# DEPRECATED lctCreateAuthChallenge "Use generic-lens or generic-optics with 'createAuthChallenge' instead." #-}

-- | Verifies the authentication challenge response.
--
-- /Note:/ Consider using 'verifyAuthChallengeResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lctVerifyAuthChallengeResponse :: Lens.Lens' LambdaConfigType (Lude.Maybe Lude.Text)
lctVerifyAuthChallengeResponse = Lens.lens (verifyAuthChallengeResponse :: LambdaConfigType -> Lude.Maybe Lude.Text) (\s a -> s {verifyAuthChallengeResponse = a} :: LambdaConfigType)
{-# DEPRECATED lctVerifyAuthChallengeResponse "Use generic-lens or generic-optics with 'verifyAuthChallengeResponse' instead." #-}

-- | A custom SMS sender AWS Lambda trigger.
--
-- /Note:/ Consider using 'customSMSSender' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lctCustomSMSSender :: Lens.Lens' LambdaConfigType (Lude.Maybe CustomSMSLambdaVersionConfigType)
lctCustomSMSSender = Lens.lens (customSMSSender :: LambdaConfigType -> Lude.Maybe CustomSMSLambdaVersionConfigType) (\s a -> s {customSMSSender = a} :: LambdaConfigType)
{-# DEPRECATED lctCustomSMSSender "Use generic-lens or generic-optics with 'customSMSSender' instead." #-}

-- | A post-authentication AWS Lambda trigger.
--
-- /Note:/ Consider using 'postAuthentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lctPostAuthentication :: Lens.Lens' LambdaConfigType (Lude.Maybe Lude.Text)
lctPostAuthentication = Lens.lens (postAuthentication :: LambdaConfigType -> Lude.Maybe Lude.Text) (\s a -> s {postAuthentication = a} :: LambdaConfigType)
{-# DEPRECATED lctPostAuthentication "Use generic-lens or generic-optics with 'postAuthentication' instead." #-}

-- | A custom Message AWS Lambda trigger.
--
-- /Note:/ Consider using 'customMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lctCustomMessage :: Lens.Lens' LambdaConfigType (Lude.Maybe Lude.Text)
lctCustomMessage = Lens.lens (customMessage :: LambdaConfigType -> Lude.Maybe Lude.Text) (\s a -> s {customMessage = a} :: LambdaConfigType)
{-# DEPRECATED lctCustomMessage "Use generic-lens or generic-optics with 'customMessage' instead." #-}

-- | Defines the authentication challenge.
--
-- /Note:/ Consider using 'defineAuthChallenge' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lctDefineAuthChallenge :: Lens.Lens' LambdaConfigType (Lude.Maybe Lude.Text)
lctDefineAuthChallenge = Lens.lens (defineAuthChallenge :: LambdaConfigType -> Lude.Maybe Lude.Text) (\s a -> s {defineAuthChallenge = a} :: LambdaConfigType)
{-# DEPRECATED lctDefineAuthChallenge "Use generic-lens or generic-optics with 'defineAuthChallenge' instead." #-}

-- | A custom email sender AWS Lambda trigger.
--
-- /Note:/ Consider using 'customEmailSender' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lctCustomEmailSender :: Lens.Lens' LambdaConfigType (Lude.Maybe CustomEmailLambdaVersionConfigType)
lctCustomEmailSender = Lens.lens (customEmailSender :: LambdaConfigType -> Lude.Maybe CustomEmailLambdaVersionConfigType) (\s a -> s {customEmailSender = a} :: LambdaConfigType)
{-# DEPRECATED lctCustomEmailSender "Use generic-lens or generic-optics with 'customEmailSender' instead." #-}

-- | The Amazon Resource Name of Key Management Service </kms/latest/developerguide/concepts.html#master_keys Customer master keys> . Amazon Cognito uses the key to encrypt codes and temporary passwords sent to @CustomEmailSender@ and @CustomSMSSender@ .
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lctKMSKeyId :: Lens.Lens' LambdaConfigType (Lude.Maybe Lude.Text)
lctKMSKeyId = Lens.lens (kmsKeyId :: LambdaConfigType -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: LambdaConfigType)
{-# DEPRECATED lctKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | A post-confirmation AWS Lambda trigger.
--
-- /Note:/ Consider using 'postConfirmation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lctPostConfirmation :: Lens.Lens' LambdaConfigType (Lude.Maybe Lude.Text)
lctPostConfirmation = Lens.lens (postConfirmation :: LambdaConfigType -> Lude.Maybe Lude.Text) (\s a -> s {postConfirmation = a} :: LambdaConfigType)
{-# DEPRECATED lctPostConfirmation "Use generic-lens or generic-optics with 'postConfirmation' instead." #-}

-- | A Lambda trigger that is invoked before token generation.
--
-- /Note:/ Consider using 'preTokenGeneration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lctPreTokenGeneration :: Lens.Lens' LambdaConfigType (Lude.Maybe Lude.Text)
lctPreTokenGeneration = Lens.lens (preTokenGeneration :: LambdaConfigType -> Lude.Maybe Lude.Text) (\s a -> s {preTokenGeneration = a} :: LambdaConfigType)
{-# DEPRECATED lctPreTokenGeneration "Use generic-lens or generic-optics with 'preTokenGeneration' instead." #-}

-- | The user migration Lambda config type.
--
-- /Note:/ Consider using 'userMigration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lctUserMigration :: Lens.Lens' LambdaConfigType (Lude.Maybe Lude.Text)
lctUserMigration = Lens.lens (userMigration :: LambdaConfigType -> Lude.Maybe Lude.Text) (\s a -> s {userMigration = a} :: LambdaConfigType)
{-# DEPRECATED lctUserMigration "Use generic-lens or generic-optics with 'userMigration' instead." #-}

-- | A pre-registration AWS Lambda trigger.
--
-- /Note:/ Consider using 'preSignUp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lctPreSignUp :: Lens.Lens' LambdaConfigType (Lude.Maybe Lude.Text)
lctPreSignUp = Lens.lens (preSignUp :: LambdaConfigType -> Lude.Maybe Lude.Text) (\s a -> s {preSignUp = a} :: LambdaConfigType)
{-# DEPRECATED lctPreSignUp "Use generic-lens or generic-optics with 'preSignUp' instead." #-}

instance Lude.FromJSON LambdaConfigType where
  parseJSON =
    Lude.withObject
      "LambdaConfigType"
      ( \x ->
          LambdaConfigType'
            Lude.<$> (x Lude..:? "PreAuthentication")
            Lude.<*> (x Lude..:? "CreateAuthChallenge")
            Lude.<*> (x Lude..:? "VerifyAuthChallengeResponse")
            Lude.<*> (x Lude..:? "CustomSMSSender")
            Lude.<*> (x Lude..:? "PostAuthentication")
            Lude.<*> (x Lude..:? "CustomMessage")
            Lude.<*> (x Lude..:? "DefineAuthChallenge")
            Lude.<*> (x Lude..:? "CustomEmailSender")
            Lude.<*> (x Lude..:? "KMSKeyID")
            Lude.<*> (x Lude..:? "PostConfirmation")
            Lude.<*> (x Lude..:? "PreTokenGeneration")
            Lude.<*> (x Lude..:? "UserMigration")
            Lude.<*> (x Lude..:? "PreSignUp")
      )

instance Lude.ToJSON LambdaConfigType where
  toJSON LambdaConfigType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PreAuthentication" Lude..=) Lude.<$> preAuthentication,
            ("CreateAuthChallenge" Lude..=) Lude.<$> createAuthChallenge,
            ("VerifyAuthChallengeResponse" Lude..=)
              Lude.<$> verifyAuthChallengeResponse,
            ("CustomSMSSender" Lude..=) Lude.<$> customSMSSender,
            ("PostAuthentication" Lude..=) Lude.<$> postAuthentication,
            ("CustomMessage" Lude..=) Lude.<$> customMessage,
            ("DefineAuthChallenge" Lude..=) Lude.<$> defineAuthChallenge,
            ("CustomEmailSender" Lude..=) Lude.<$> customEmailSender,
            ("KMSKeyID" Lude..=) Lude.<$> kmsKeyId,
            ("PostConfirmation" Lude..=) Lude.<$> postConfirmation,
            ("PreTokenGeneration" Lude..=) Lude.<$> preTokenGeneration,
            ("UserMigration" Lude..=) Lude.<$> userMigration,
            ("PreSignUp" Lude..=) Lude.<$> preSignUp
          ]
      )
