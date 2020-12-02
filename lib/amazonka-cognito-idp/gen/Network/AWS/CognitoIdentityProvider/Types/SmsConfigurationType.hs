{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.SmsConfigurationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.SmsConfigurationType where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The SMS configuration type that includes the settings the Cognito User Pool needs to call for the Amazon SNS service to send an SMS message from your AWS account. The Cognito User Pool makes the request to the Amazon SNS Service by using an AWS IAM role that you provide for your AWS account.
--
--
--
-- /See:/ 'smsConfigurationType' smart constructor.
data SmsConfigurationType = SmsConfigurationType'
  { _sctExternalId ::
      !(Maybe Text),
    _sctSNSCallerARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SmsConfigurationType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sctExternalId' - The external ID is a value that we recommend you use to add security to your IAM role which is used to call Amazon SNS to send SMS messages for your user pool. If you provide an @ExternalId@ , the Cognito User Pool will include it when attempting to assume your IAM role, so that you can set your roles trust policy to require the @ExternalID@ . If you use the Cognito Management Console to create a role for SMS MFA, Cognito will create a role with the required permissions and a trust policy that demonstrates use of the @ExternalId@ .
--
-- * 'sctSNSCallerARN' - The Amazon Resource Name (ARN) of the Amazon Simple Notification Service (SNS) caller. This is the ARN of the IAM role in your AWS account which Cognito will use to send SMS messages. SMS messages are subject to a <https://docs.aws.amazon.com/cognito/latest/developerguide/user-pool-settings-email-phone-verification.html spending limit> .
smsConfigurationType ::
  -- | 'sctSNSCallerARN'
  Text ->
  SmsConfigurationType
smsConfigurationType pSNSCallerARN_ =
  SmsConfigurationType'
    { _sctExternalId = Nothing,
      _sctSNSCallerARN = pSNSCallerARN_
    }

-- | The external ID is a value that we recommend you use to add security to your IAM role which is used to call Amazon SNS to send SMS messages for your user pool. If you provide an @ExternalId@ , the Cognito User Pool will include it when attempting to assume your IAM role, so that you can set your roles trust policy to require the @ExternalID@ . If you use the Cognito Management Console to create a role for SMS MFA, Cognito will create a role with the required permissions and a trust policy that demonstrates use of the @ExternalId@ .
sctExternalId :: Lens' SmsConfigurationType (Maybe Text)
sctExternalId = lens _sctExternalId (\s a -> s {_sctExternalId = a})

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service (SNS) caller. This is the ARN of the IAM role in your AWS account which Cognito will use to send SMS messages. SMS messages are subject to a <https://docs.aws.amazon.com/cognito/latest/developerguide/user-pool-settings-email-phone-verification.html spending limit> .
sctSNSCallerARN :: Lens' SmsConfigurationType Text
sctSNSCallerARN = lens _sctSNSCallerARN (\s a -> s {_sctSNSCallerARN = a})

instance FromJSON SmsConfigurationType where
  parseJSON =
    withObject
      "SmsConfigurationType"
      ( \x ->
          SmsConfigurationType'
            <$> (x .:? "ExternalId") <*> (x .: "SnsCallerArn")
      )

instance Hashable SmsConfigurationType

instance NFData SmsConfigurationType

instance ToJSON SmsConfigurationType where
  toJSON SmsConfigurationType' {..} =
    object
      ( catMaybes
          [ ("ExternalId" .=) <$> _sctExternalId,
            Just ("SnsCallerArn" .= _sctSNSCallerARN)
          ]
      )
