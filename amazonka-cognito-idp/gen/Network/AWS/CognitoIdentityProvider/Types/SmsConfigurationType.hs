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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.SmsConfigurationType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.SmsConfigurationType where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The SMS configuration type that includes the settings the Cognito User
-- Pool needs to call for the Amazon SNS service to send an SMS message
-- from your AWS account. The Cognito User Pool makes the request to the
-- Amazon SNS Service by using an AWS IAM role that you provide for your
-- AWS account.
--
-- /See:/ 'newSmsConfigurationType' smart constructor.
data SmsConfigurationType = SmsConfigurationType'
  { -- | The external ID is a value that we recommend you use to add security to
    -- your IAM role which is used to call Amazon SNS to send SMS messages for
    -- your user pool. If you provide an @ExternalId@, the Cognito User Pool
    -- will include it when attempting to assume your IAM role, so that you can
    -- set your roles trust policy to require the @ExternalID@. If you use the
    -- Cognito Management Console to create a role for SMS MFA, Cognito will
    -- create a role with the required permissions and a trust policy that
    -- demonstrates use of the @ExternalId@.
    externalId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
    -- (SNS) caller. This is the ARN of the IAM role in your AWS account which
    -- Cognito will use to send SMS messages. SMS messages are subject to a
    -- <https://docs.aws.amazon.com/cognito/latest/developerguide/user-pool-settings-email-phone-verification.html spending limit>.
    snsCallerArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SmsConfigurationType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'externalId', 'smsConfigurationType_externalId' - The external ID is a value that we recommend you use to add security to
-- your IAM role which is used to call Amazon SNS to send SMS messages for
-- your user pool. If you provide an @ExternalId@, the Cognito User Pool
-- will include it when attempting to assume your IAM role, so that you can
-- set your roles trust policy to require the @ExternalID@. If you use the
-- Cognito Management Console to create a role for SMS MFA, Cognito will
-- create a role with the required permissions and a trust policy that
-- demonstrates use of the @ExternalId@.
--
-- 'snsCallerArn', 'smsConfigurationType_snsCallerArn' - The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
-- (SNS) caller. This is the ARN of the IAM role in your AWS account which
-- Cognito will use to send SMS messages. SMS messages are subject to a
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/user-pool-settings-email-phone-verification.html spending limit>.
newSmsConfigurationType ::
  -- | 'snsCallerArn'
  Prelude.Text ->
  SmsConfigurationType
newSmsConfigurationType pSnsCallerArn_ =
  SmsConfigurationType'
    { externalId = Prelude.Nothing,
      snsCallerArn = pSnsCallerArn_
    }

-- | The external ID is a value that we recommend you use to add security to
-- your IAM role which is used to call Amazon SNS to send SMS messages for
-- your user pool. If you provide an @ExternalId@, the Cognito User Pool
-- will include it when attempting to assume your IAM role, so that you can
-- set your roles trust policy to require the @ExternalID@. If you use the
-- Cognito Management Console to create a role for SMS MFA, Cognito will
-- create a role with the required permissions and a trust policy that
-- demonstrates use of the @ExternalId@.
smsConfigurationType_externalId :: Lens.Lens' SmsConfigurationType (Prelude.Maybe Prelude.Text)
smsConfigurationType_externalId = Lens.lens (\SmsConfigurationType' {externalId} -> externalId) (\s@SmsConfigurationType' {} a -> s {externalId = a} :: SmsConfigurationType)

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
-- (SNS) caller. This is the ARN of the IAM role in your AWS account which
-- Cognito will use to send SMS messages. SMS messages are subject to a
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/user-pool-settings-email-phone-verification.html spending limit>.
smsConfigurationType_snsCallerArn :: Lens.Lens' SmsConfigurationType Prelude.Text
smsConfigurationType_snsCallerArn = Lens.lens (\SmsConfigurationType' {snsCallerArn} -> snsCallerArn) (\s@SmsConfigurationType' {} a -> s {snsCallerArn = a} :: SmsConfigurationType)

instance Prelude.FromJSON SmsConfigurationType where
  parseJSON =
    Prelude.withObject
      "SmsConfigurationType"
      ( \x ->
          SmsConfigurationType'
            Prelude.<$> (x Prelude..:? "ExternalId")
            Prelude.<*> (x Prelude..: "SnsCallerArn")
      )

instance Prelude.Hashable SmsConfigurationType

instance Prelude.NFData SmsConfigurationType

instance Prelude.ToJSON SmsConfigurationType where
  toJSON SmsConfigurationType' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ExternalId" Prelude..=) Prelude.<$> externalId,
            Prelude.Just
              ("SnsCallerArn" Prelude..= snsCallerArn)
          ]
      )
