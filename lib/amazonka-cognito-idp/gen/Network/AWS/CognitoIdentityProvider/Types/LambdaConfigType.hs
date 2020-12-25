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
    lctCreateAuthChallenge,
    lctCustomEmailSender,
    lctCustomMessage,
    lctCustomSMSSender,
    lctDefineAuthChallenge,
    lctKMSKeyID,
    lctPostAuthentication,
    lctPostConfirmation,
    lctPreAuthentication,
    lctPreSignUp,
    lctPreTokenGeneration,
    lctUserMigration,
    lctVerifyAuthChallengeResponse,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types.CreateAuthChallenge as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.CustomEmailLambdaVersionConfigType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.CustomMessage as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.CustomSMSLambdaVersionConfigType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.DefineAuthChallenge as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.KMSKeyID as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.PostAuthentication as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.PostConfirmation as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.PreAuthentication as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.PreSignUp as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.PreTokenGeneration as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.UserMigration as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.VerifyAuthChallengeResponse as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the configuration for AWS Lambda triggers.
--
-- /See:/ 'mkLambdaConfigType' smart constructor.
data LambdaConfigType = LambdaConfigType'
  { -- | Creates an authentication challenge.
    createAuthChallenge :: Core.Maybe Types.CreateAuthChallenge,
    -- | A custom email sender AWS Lambda trigger.
    customEmailSender :: Core.Maybe Types.CustomEmailLambdaVersionConfigType,
    -- | A custom Message AWS Lambda trigger.
    customMessage :: Core.Maybe Types.CustomMessage,
    -- | A custom SMS sender AWS Lambda trigger.
    customSMSSender :: Core.Maybe Types.CustomSMSLambdaVersionConfigType,
    -- | Defines the authentication challenge.
    defineAuthChallenge :: Core.Maybe Types.DefineAuthChallenge,
    -- | The Amazon Resource Name of Key Management Service </kms/latest/developerguide/concepts.html#master_keys Customer master keys> . Amazon Cognito uses the key to encrypt codes and temporary passwords sent to @CustomEmailSender@ and @CustomSMSSender@ .
    kMSKeyID :: Core.Maybe Types.KMSKeyID,
    -- | A post-authentication AWS Lambda trigger.
    postAuthentication :: Core.Maybe Types.PostAuthentication,
    -- | A post-confirmation AWS Lambda trigger.
    postConfirmation :: Core.Maybe Types.PostConfirmation,
    -- | A pre-authentication AWS Lambda trigger.
    preAuthentication :: Core.Maybe Types.PreAuthentication,
    -- | A pre-registration AWS Lambda trigger.
    preSignUp :: Core.Maybe Types.PreSignUp,
    -- | A Lambda trigger that is invoked before token generation.
    preTokenGeneration :: Core.Maybe Types.PreTokenGeneration,
    -- | The user migration Lambda config type.
    userMigration :: Core.Maybe Types.UserMigration,
    -- | Verifies the authentication challenge response.
    verifyAuthChallengeResponse :: Core.Maybe Types.VerifyAuthChallengeResponse
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LambdaConfigType' value with any optional fields omitted.
mkLambdaConfigType ::
  LambdaConfigType
mkLambdaConfigType =
  LambdaConfigType'
    { createAuthChallenge = Core.Nothing,
      customEmailSender = Core.Nothing,
      customMessage = Core.Nothing,
      customSMSSender = Core.Nothing,
      defineAuthChallenge = Core.Nothing,
      kMSKeyID = Core.Nothing,
      postAuthentication = Core.Nothing,
      postConfirmation = Core.Nothing,
      preAuthentication = Core.Nothing,
      preSignUp = Core.Nothing,
      preTokenGeneration = Core.Nothing,
      userMigration = Core.Nothing,
      verifyAuthChallengeResponse = Core.Nothing
    }

-- | Creates an authentication challenge.
--
-- /Note:/ Consider using 'createAuthChallenge' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lctCreateAuthChallenge :: Lens.Lens' LambdaConfigType (Core.Maybe Types.CreateAuthChallenge)
lctCreateAuthChallenge = Lens.field @"createAuthChallenge"
{-# DEPRECATED lctCreateAuthChallenge "Use generic-lens or generic-optics with 'createAuthChallenge' instead." #-}

-- | A custom email sender AWS Lambda trigger.
--
-- /Note:/ Consider using 'customEmailSender' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lctCustomEmailSender :: Lens.Lens' LambdaConfigType (Core.Maybe Types.CustomEmailLambdaVersionConfigType)
lctCustomEmailSender = Lens.field @"customEmailSender"
{-# DEPRECATED lctCustomEmailSender "Use generic-lens or generic-optics with 'customEmailSender' instead." #-}

-- | A custom Message AWS Lambda trigger.
--
-- /Note:/ Consider using 'customMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lctCustomMessage :: Lens.Lens' LambdaConfigType (Core.Maybe Types.CustomMessage)
lctCustomMessage = Lens.field @"customMessage"
{-# DEPRECATED lctCustomMessage "Use generic-lens or generic-optics with 'customMessage' instead." #-}

-- | A custom SMS sender AWS Lambda trigger.
--
-- /Note:/ Consider using 'customSMSSender' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lctCustomSMSSender :: Lens.Lens' LambdaConfigType (Core.Maybe Types.CustomSMSLambdaVersionConfigType)
lctCustomSMSSender = Lens.field @"customSMSSender"
{-# DEPRECATED lctCustomSMSSender "Use generic-lens or generic-optics with 'customSMSSender' instead." #-}

-- | Defines the authentication challenge.
--
-- /Note:/ Consider using 'defineAuthChallenge' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lctDefineAuthChallenge :: Lens.Lens' LambdaConfigType (Core.Maybe Types.DefineAuthChallenge)
lctDefineAuthChallenge = Lens.field @"defineAuthChallenge"
{-# DEPRECATED lctDefineAuthChallenge "Use generic-lens or generic-optics with 'defineAuthChallenge' instead." #-}

-- | The Amazon Resource Name of Key Management Service </kms/latest/developerguide/concepts.html#master_keys Customer master keys> . Amazon Cognito uses the key to encrypt codes and temporary passwords sent to @CustomEmailSender@ and @CustomSMSSender@ .
--
-- /Note:/ Consider using 'kMSKeyID' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lctKMSKeyID :: Lens.Lens' LambdaConfigType (Core.Maybe Types.KMSKeyID)
lctKMSKeyID = Lens.field @"kMSKeyID"
{-# DEPRECATED lctKMSKeyID "Use generic-lens or generic-optics with 'kMSKeyID' instead." #-}

-- | A post-authentication AWS Lambda trigger.
--
-- /Note:/ Consider using 'postAuthentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lctPostAuthentication :: Lens.Lens' LambdaConfigType (Core.Maybe Types.PostAuthentication)
lctPostAuthentication = Lens.field @"postAuthentication"
{-# DEPRECATED lctPostAuthentication "Use generic-lens or generic-optics with 'postAuthentication' instead." #-}

-- | A post-confirmation AWS Lambda trigger.
--
-- /Note:/ Consider using 'postConfirmation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lctPostConfirmation :: Lens.Lens' LambdaConfigType (Core.Maybe Types.PostConfirmation)
lctPostConfirmation = Lens.field @"postConfirmation"
{-# DEPRECATED lctPostConfirmation "Use generic-lens or generic-optics with 'postConfirmation' instead." #-}

-- | A pre-authentication AWS Lambda trigger.
--
-- /Note:/ Consider using 'preAuthentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lctPreAuthentication :: Lens.Lens' LambdaConfigType (Core.Maybe Types.PreAuthentication)
lctPreAuthentication = Lens.field @"preAuthentication"
{-# DEPRECATED lctPreAuthentication "Use generic-lens or generic-optics with 'preAuthentication' instead." #-}

-- | A pre-registration AWS Lambda trigger.
--
-- /Note:/ Consider using 'preSignUp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lctPreSignUp :: Lens.Lens' LambdaConfigType (Core.Maybe Types.PreSignUp)
lctPreSignUp = Lens.field @"preSignUp"
{-# DEPRECATED lctPreSignUp "Use generic-lens or generic-optics with 'preSignUp' instead." #-}

-- | A Lambda trigger that is invoked before token generation.
--
-- /Note:/ Consider using 'preTokenGeneration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lctPreTokenGeneration :: Lens.Lens' LambdaConfigType (Core.Maybe Types.PreTokenGeneration)
lctPreTokenGeneration = Lens.field @"preTokenGeneration"
{-# DEPRECATED lctPreTokenGeneration "Use generic-lens or generic-optics with 'preTokenGeneration' instead." #-}

-- | The user migration Lambda config type.
--
-- /Note:/ Consider using 'userMigration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lctUserMigration :: Lens.Lens' LambdaConfigType (Core.Maybe Types.UserMigration)
lctUserMigration = Lens.field @"userMigration"
{-# DEPRECATED lctUserMigration "Use generic-lens or generic-optics with 'userMigration' instead." #-}

-- | Verifies the authentication challenge response.
--
-- /Note:/ Consider using 'verifyAuthChallengeResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lctVerifyAuthChallengeResponse :: Lens.Lens' LambdaConfigType (Core.Maybe Types.VerifyAuthChallengeResponse)
lctVerifyAuthChallengeResponse = Lens.field @"verifyAuthChallengeResponse"
{-# DEPRECATED lctVerifyAuthChallengeResponse "Use generic-lens or generic-optics with 'verifyAuthChallengeResponse' instead." #-}

instance Core.FromJSON LambdaConfigType where
  toJSON LambdaConfigType {..} =
    Core.object
      ( Core.catMaybes
          [ ("CreateAuthChallenge" Core..=) Core.<$> createAuthChallenge,
            ("CustomEmailSender" Core..=) Core.<$> customEmailSender,
            ("CustomMessage" Core..=) Core.<$> customMessage,
            ("CustomSMSSender" Core..=) Core.<$> customSMSSender,
            ("DefineAuthChallenge" Core..=) Core.<$> defineAuthChallenge,
            ("KMSKeyID" Core..=) Core.<$> kMSKeyID,
            ("PostAuthentication" Core..=) Core.<$> postAuthentication,
            ("PostConfirmation" Core..=) Core.<$> postConfirmation,
            ("PreAuthentication" Core..=) Core.<$> preAuthentication,
            ("PreSignUp" Core..=) Core.<$> preSignUp,
            ("PreTokenGeneration" Core..=) Core.<$> preTokenGeneration,
            ("UserMigration" Core..=) Core.<$> userMigration,
            ("VerifyAuthChallengeResponse" Core..=)
              Core.<$> verifyAuthChallengeResponse
          ]
      )

instance Core.FromJSON LambdaConfigType where
  parseJSON =
    Core.withObject "LambdaConfigType" Core.$
      \x ->
        LambdaConfigType'
          Core.<$> (x Core..:? "CreateAuthChallenge")
          Core.<*> (x Core..:? "CustomEmailSender")
          Core.<*> (x Core..:? "CustomMessage")
          Core.<*> (x Core..:? "CustomSMSSender")
          Core.<*> (x Core..:? "DefineAuthChallenge")
          Core.<*> (x Core..:? "KMSKeyID")
          Core.<*> (x Core..:? "PostAuthentication")
          Core.<*> (x Core..:? "PostConfirmation")
          Core.<*> (x Core..:? "PreAuthentication")
          Core.<*> (x Core..:? "PreSignUp")
          Core.<*> (x Core..:? "PreTokenGeneration")
          Core.<*> (x Core..:? "UserMigration")
          Core.<*> (x Core..:? "VerifyAuthChallengeResponse")
