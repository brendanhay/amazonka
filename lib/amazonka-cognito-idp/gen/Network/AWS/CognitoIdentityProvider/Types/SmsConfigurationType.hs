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
    sctExternalId,
    sctSNSCallerARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The SMS configuration type that includes the settings the Cognito User Pool needs to call for the Amazon SNS service to send an SMS message from your AWS account. The Cognito User Pool makes the request to the Amazon SNS Service by using an AWS IAM role that you provide for your AWS account.
--
-- /See:/ 'mkSmsConfigurationType' smart constructor.
data SmsConfigurationType = SmsConfigurationType'
  { externalId ::
      Lude.Maybe Lude.Text,
    snsCallerARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SmsConfigurationType' with the minimum fields required to make a request.
--
-- * 'externalId' - The external ID is a value that we recommend you use to add security to your IAM role which is used to call Amazon SNS to send SMS messages for your user pool. If you provide an @ExternalId@ , the Cognito User Pool will include it when attempting to assume your IAM role, so that you can set your roles trust policy to require the @ExternalID@ . If you use the Cognito Management Console to create a role for SMS MFA, Cognito will create a role with the required permissions and a trust policy that demonstrates use of the @ExternalId@ .
-- * 'snsCallerARN' - The Amazon Resource Name (ARN) of the Amazon Simple Notification Service (SNS) caller. This is the ARN of the IAM role in your AWS account which Cognito will use to send SMS messages. SMS messages are subject to a <https://docs.aws.amazon.com/cognito/latest/developerguide/user-pool-settings-email-phone-verification.html spending limit> .
mkSmsConfigurationType ::
  -- | 'snsCallerARN'
  Lude.Text ->
  SmsConfigurationType
mkSmsConfigurationType pSNSCallerARN_ =
  SmsConfigurationType'
    { externalId = Lude.Nothing,
      snsCallerARN = pSNSCallerARN_
    }

-- | The external ID is a value that we recommend you use to add security to your IAM role which is used to call Amazon SNS to send SMS messages for your user pool. If you provide an @ExternalId@ , the Cognito User Pool will include it when attempting to assume your IAM role, so that you can set your roles trust policy to require the @ExternalID@ . If you use the Cognito Management Console to create a role for SMS MFA, Cognito will create a role with the required permissions and a trust policy that demonstrates use of the @ExternalId@ .
--
-- /Note:/ Consider using 'externalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sctExternalId :: Lens.Lens' SmsConfigurationType (Lude.Maybe Lude.Text)
sctExternalId = Lens.lens (externalId :: SmsConfigurationType -> Lude.Maybe Lude.Text) (\s a -> s {externalId = a} :: SmsConfigurationType)
{-# DEPRECATED sctExternalId "Use generic-lens or generic-optics with 'externalId' instead." #-}

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service (SNS) caller. This is the ARN of the IAM role in your AWS account which Cognito will use to send SMS messages. SMS messages are subject to a <https://docs.aws.amazon.com/cognito/latest/developerguide/user-pool-settings-email-phone-verification.html spending limit> .
--
-- /Note:/ Consider using 'snsCallerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sctSNSCallerARN :: Lens.Lens' SmsConfigurationType Lude.Text
sctSNSCallerARN = Lens.lens (snsCallerARN :: SmsConfigurationType -> Lude.Text) (\s a -> s {snsCallerARN = a} :: SmsConfigurationType)
{-# DEPRECATED sctSNSCallerARN "Use generic-lens or generic-optics with 'snsCallerARN' instead." #-}

instance Lude.FromJSON SmsConfigurationType where
  parseJSON =
    Lude.withObject
      "SmsConfigurationType"
      ( \x ->
          SmsConfigurationType'
            Lude.<$> (x Lude..:? "ExternalId") Lude.<*> (x Lude..: "SnsCallerArn")
      )

instance Lude.ToJSON SmsConfigurationType where
  toJSON SmsConfigurationType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ExternalId" Lude..=) Lude.<$> externalId,
            Lude.Just ("SnsCallerArn" Lude..= snsCallerARN)
          ]
      )
