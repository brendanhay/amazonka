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
-- Module      : Amazonka.CognitoIdentityProvider.Types.SmsConfigurationType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.SmsConfigurationType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The SMS configuration type is the settings that your Amazon Cognito user
-- pool must use to send an SMS message from your Amazon Web Services
-- account through Amazon Simple Notification Service. To send SMS messages
-- with Amazon SNS in the Amazon Web Services Region that you want, the
-- Amazon Cognito user pool uses an Identity and Access Management (IAM)
-- role in your Amazon Web Services account.
--
-- /See:/ 'newSmsConfigurationType' smart constructor.
data SmsConfigurationType = SmsConfigurationType'
  { -- | The external ID provides additional security for your IAM role. You can
    -- use an @ExternalId@ with the IAM role that you use with Amazon SNS to
    -- send SMS messages for your user pool. If you provide an @ExternalId@,
    -- your Amazon Cognito user pool includes it in the request to assume your
    -- IAM role. You can configure the role trust policy to require that Amazon
    -- Cognito, and any principal, provide the @ExternalID@. If you use the
    -- Amazon Cognito Management Console to create a role for SMS multi-factor
    -- authentication (MFA), Amazon Cognito creates a role with the required
    -- permissions and a trust policy that demonstrates use of the
    -- @ExternalId@.
    --
    -- For more information about the @ExternalId@ of a role, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_create_for-user_externalid.html How to use an external ID when granting access to your Amazon Web Services resources to a third party>
    externalId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Region to use with Amazon SNS integration. You
    -- can choose the same Region as your user pool, or a supported __Legacy
    -- Amazon SNS alternate Region__.
    --
    -- Amazon Cognito resources in the Asia Pacific (Seoul) Amazon Web Services
    -- Region must use your Amazon SNS configuration in the Asia Pacific
    -- (Tokyo) Region. For more information, see
    -- <https://docs.aws.amazon.com/cognito/latest/developerguide/user-pool-sms-settings.html SMS message settings for Amazon Cognito user pools>.
    snsRegion :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon SNS caller. This is the ARN
    -- of the IAM role in your Amazon Web Services account that Amazon Cognito
    -- will use to send SMS messages. SMS messages are subject to a
    -- <https://docs.aws.amazon.com/cognito/latest/developerguide/user-pool-settings-email-phone-verification.html spending limit>.
    snsCallerArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SmsConfigurationType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'externalId', 'smsConfigurationType_externalId' - The external ID provides additional security for your IAM role. You can
-- use an @ExternalId@ with the IAM role that you use with Amazon SNS to
-- send SMS messages for your user pool. If you provide an @ExternalId@,
-- your Amazon Cognito user pool includes it in the request to assume your
-- IAM role. You can configure the role trust policy to require that Amazon
-- Cognito, and any principal, provide the @ExternalID@. If you use the
-- Amazon Cognito Management Console to create a role for SMS multi-factor
-- authentication (MFA), Amazon Cognito creates a role with the required
-- permissions and a trust policy that demonstrates use of the
-- @ExternalId@.
--
-- For more information about the @ExternalId@ of a role, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_create_for-user_externalid.html How to use an external ID when granting access to your Amazon Web Services resources to a third party>
--
-- 'snsRegion', 'smsConfigurationType_snsRegion' - The Amazon Web Services Region to use with Amazon SNS integration. You
-- can choose the same Region as your user pool, or a supported __Legacy
-- Amazon SNS alternate Region__.
--
-- Amazon Cognito resources in the Asia Pacific (Seoul) Amazon Web Services
-- Region must use your Amazon SNS configuration in the Asia Pacific
-- (Tokyo) Region. For more information, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/user-pool-sms-settings.html SMS message settings for Amazon Cognito user pools>.
--
-- 'snsCallerArn', 'smsConfigurationType_snsCallerArn' - The Amazon Resource Name (ARN) of the Amazon SNS caller. This is the ARN
-- of the IAM role in your Amazon Web Services account that Amazon Cognito
-- will use to send SMS messages. SMS messages are subject to a
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/user-pool-settings-email-phone-verification.html spending limit>.
newSmsConfigurationType ::
  -- | 'snsCallerArn'
  Prelude.Text ->
  SmsConfigurationType
newSmsConfigurationType pSnsCallerArn_ =
  SmsConfigurationType'
    { externalId = Prelude.Nothing,
      snsRegion = Prelude.Nothing,
      snsCallerArn = pSnsCallerArn_
    }

-- | The external ID provides additional security for your IAM role. You can
-- use an @ExternalId@ with the IAM role that you use with Amazon SNS to
-- send SMS messages for your user pool. If you provide an @ExternalId@,
-- your Amazon Cognito user pool includes it in the request to assume your
-- IAM role. You can configure the role trust policy to require that Amazon
-- Cognito, and any principal, provide the @ExternalID@. If you use the
-- Amazon Cognito Management Console to create a role for SMS multi-factor
-- authentication (MFA), Amazon Cognito creates a role with the required
-- permissions and a trust policy that demonstrates use of the
-- @ExternalId@.
--
-- For more information about the @ExternalId@ of a role, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_create_for-user_externalid.html How to use an external ID when granting access to your Amazon Web Services resources to a third party>
smsConfigurationType_externalId :: Lens.Lens' SmsConfigurationType (Prelude.Maybe Prelude.Text)
smsConfigurationType_externalId = Lens.lens (\SmsConfigurationType' {externalId} -> externalId) (\s@SmsConfigurationType' {} a -> s {externalId = a} :: SmsConfigurationType)

-- | The Amazon Web Services Region to use with Amazon SNS integration. You
-- can choose the same Region as your user pool, or a supported __Legacy
-- Amazon SNS alternate Region__.
--
-- Amazon Cognito resources in the Asia Pacific (Seoul) Amazon Web Services
-- Region must use your Amazon SNS configuration in the Asia Pacific
-- (Tokyo) Region. For more information, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/user-pool-sms-settings.html SMS message settings for Amazon Cognito user pools>.
smsConfigurationType_snsRegion :: Lens.Lens' SmsConfigurationType (Prelude.Maybe Prelude.Text)
smsConfigurationType_snsRegion = Lens.lens (\SmsConfigurationType' {snsRegion} -> snsRegion) (\s@SmsConfigurationType' {} a -> s {snsRegion = a} :: SmsConfigurationType)

-- | The Amazon Resource Name (ARN) of the Amazon SNS caller. This is the ARN
-- of the IAM role in your Amazon Web Services account that Amazon Cognito
-- will use to send SMS messages. SMS messages are subject to a
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/user-pool-settings-email-phone-verification.html spending limit>.
smsConfigurationType_snsCallerArn :: Lens.Lens' SmsConfigurationType Prelude.Text
smsConfigurationType_snsCallerArn = Lens.lens (\SmsConfigurationType' {snsCallerArn} -> snsCallerArn) (\s@SmsConfigurationType' {} a -> s {snsCallerArn = a} :: SmsConfigurationType)

instance Data.FromJSON SmsConfigurationType where
  parseJSON =
    Data.withObject
      "SmsConfigurationType"
      ( \x ->
          SmsConfigurationType'
            Prelude.<$> (x Data..:? "ExternalId")
            Prelude.<*> (x Data..:? "SnsRegion")
            Prelude.<*> (x Data..: "SnsCallerArn")
      )

instance Prelude.Hashable SmsConfigurationType where
  hashWithSalt _salt SmsConfigurationType' {..} =
    _salt
      `Prelude.hashWithSalt` externalId
      `Prelude.hashWithSalt` snsRegion
      `Prelude.hashWithSalt` snsCallerArn

instance Prelude.NFData SmsConfigurationType where
  rnf SmsConfigurationType' {..} =
    Prelude.rnf externalId
      `Prelude.seq` Prelude.rnf snsRegion
      `Prelude.seq` Prelude.rnf snsCallerArn

instance Data.ToJSON SmsConfigurationType where
  toJSON SmsConfigurationType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ExternalId" Data..=) Prelude.<$> externalId,
            ("SnsRegion" Data..=) Prelude.<$> snsRegion,
            Prelude.Just ("SnsCallerArn" Data..= snsCallerArn)
          ]
      )
