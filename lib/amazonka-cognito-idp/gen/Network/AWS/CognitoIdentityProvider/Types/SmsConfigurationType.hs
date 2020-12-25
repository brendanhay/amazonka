{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.SmsConfigurationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.SmsConfigurationType
  ( SmsConfigurationType (..),

    -- * Smart constructor
    mkSmsConfigurationType,

    -- * Lenses
    sctSnsCallerArn,
    sctExternalId,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types.ExternalId as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.SnsCallerArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The SMS configuration type that includes the settings the Cognito User Pool needs to call for the Amazon SNS service to send an SMS message from your AWS account. The Cognito User Pool makes the request to the Amazon SNS Service by using an AWS IAM role that you provide for your AWS account.
--
-- /See:/ 'mkSmsConfigurationType' smart constructor.
data SmsConfigurationType = SmsConfigurationType'
  { -- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service (SNS) caller. This is the ARN of the IAM role in your AWS account which Cognito will use to send SMS messages. SMS messages are subject to a <https://docs.aws.amazon.com/cognito/latest/developerguide/user-pool-settings-email-phone-verification.html spending limit> .
    snsCallerArn :: Types.SnsCallerArn,
    -- | The external ID is a value that we recommend you use to add security to your IAM role which is used to call Amazon SNS to send SMS messages for your user pool. If you provide an @ExternalId@ , the Cognito User Pool will include it when attempting to assume your IAM role, so that you can set your roles trust policy to require the @ExternalID@ . If you use the Cognito Management Console to create a role for SMS MFA, Cognito will create a role with the required permissions and a trust policy that demonstrates use of the @ExternalId@ .
    externalId :: Core.Maybe Types.ExternalId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SmsConfigurationType' value with any optional fields omitted.
mkSmsConfigurationType ::
  -- | 'snsCallerArn'
  Types.SnsCallerArn ->
  SmsConfigurationType
mkSmsConfigurationType snsCallerArn =
  SmsConfigurationType' {snsCallerArn, externalId = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service (SNS) caller. This is the ARN of the IAM role in your AWS account which Cognito will use to send SMS messages. SMS messages are subject to a <https://docs.aws.amazon.com/cognito/latest/developerguide/user-pool-settings-email-phone-verification.html spending limit> .
--
-- /Note:/ Consider using 'snsCallerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sctSnsCallerArn :: Lens.Lens' SmsConfigurationType Types.SnsCallerArn
sctSnsCallerArn = Lens.field @"snsCallerArn"
{-# DEPRECATED sctSnsCallerArn "Use generic-lens or generic-optics with 'snsCallerArn' instead." #-}

-- | The external ID is a value that we recommend you use to add security to your IAM role which is used to call Amazon SNS to send SMS messages for your user pool. If you provide an @ExternalId@ , the Cognito User Pool will include it when attempting to assume your IAM role, so that you can set your roles trust policy to require the @ExternalID@ . If you use the Cognito Management Console to create a role for SMS MFA, Cognito will create a role with the required permissions and a trust policy that demonstrates use of the @ExternalId@ .
--
-- /Note:/ Consider using 'externalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sctExternalId :: Lens.Lens' SmsConfigurationType (Core.Maybe Types.ExternalId)
sctExternalId = Lens.field @"externalId"
{-# DEPRECATED sctExternalId "Use generic-lens or generic-optics with 'externalId' instead." #-}

instance Core.FromJSON SmsConfigurationType where
  toJSON SmsConfigurationType {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("SnsCallerArn" Core..= snsCallerArn),
            ("ExternalId" Core..=) Core.<$> externalId
          ]
      )

instance Core.FromJSON SmsConfigurationType where
  parseJSON =
    Core.withObject "SmsConfigurationType" Core.$
      \x ->
        SmsConfigurationType'
          Core.<$> (x Core..: "SnsCallerArn") Core.<*> (x Core..:? "ExternalId")
