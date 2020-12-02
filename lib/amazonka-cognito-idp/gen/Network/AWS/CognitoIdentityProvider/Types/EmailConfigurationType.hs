{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.EmailConfigurationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.EmailConfigurationType where

import Network.AWS.CognitoIdentityProvider.Types.EmailSendingAccountType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The email configuration type.
--
--
--
-- /See:/ 'emailConfigurationType' smart constructor.
data EmailConfigurationType = EmailConfigurationType'
  { _ectSourceARN ::
      !(Maybe Text),
    _ectFrom :: !(Maybe Text),
    _ectConfigurationSet :: !(Maybe Text),
    _ectReplyToEmailAddress :: !(Maybe Text),
    _ectEmailSendingAccount ::
      !(Maybe EmailSendingAccountType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EmailConfigurationType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ectSourceARN' - The Amazon Resource Name (ARN) of a verified email address in Amazon SES. This email address is used in one of the following ways, depending on the value that you specify for the @EmailSendingAccount@ parameter:     * If you specify @COGNITO_DEFAULT@ , Amazon Cognito uses this address as the custom FROM address when it emails your users by using its built-in email account.     * If you specify @DEVELOPER@ , Amazon Cognito emails your users with this address by calling Amazon SES on your behalf.
--
-- * 'ectFrom' - Identifies either the sender’s email address or the sender’s name with their email address. For example, @testuser@example.com@ or @Test User <testuser@example.com>@ . This address will appear before the body of the email.
--
-- * 'ectConfigurationSet' - The set of configuration rules that can be applied to emails sent using Amazon SES. A configuration set is applied to an email by including a reference to the configuration set in the headers of the email. Once applied, all of the rules in that configuration set are applied to the email. Configuration sets can be used to apply the following types of rules to emails:      * Event publishing – Amazon SES can track the number of send, delivery, open, click, bounce, and complaint events for each email sent. Use event publishing to send information about these events to other AWS services such as SNS and CloudWatch.     * IP pool management – When leasing dedicated IP addresses with Amazon SES, you can create groups of IP addresses, called dedicated IP pools. You can then associate the dedicated IP pools with configuration sets.
--
-- * 'ectReplyToEmailAddress' - The destination to which the receiver of the email should reply to.
--
-- * 'ectEmailSendingAccount' - Specifies whether Amazon Cognito emails your users by using its built-in email functionality or your Amazon SES email configuration. Specify one of the following values:     * COGNITO_DEFAULT    * When Amazon Cognito emails your users, it uses its built-in email functionality. When you use the default option, Amazon Cognito allows only a limited number of emails each day for your user pool. For typical production environments, the default email limit is below the required delivery volume. To achieve a higher delivery volume, specify DEVELOPER to use your Amazon SES email configuration. To look up the email delivery limit for the default option, see <https://docs.aws.amazon.com/cognito/latest/developerguide/limits.html Limits in Amazon Cognito> in the /Amazon Cognito Developer Guide/ . The default FROM address is no-reply@verificationemail.com. To customize the FROM address, provide the ARN of an Amazon SES verified email address for the @SourceArn@ parameter. If EmailSendingAccount is COGNITO_DEFAULT, the following parameters aren't allowed:     * EmailVerificationMessage     * EmailVerificationSubject     * InviteMessageTemplate.EmailMessage     * InviteMessageTemplate.EmailSubject     * VerificationMessageTemplate.EmailMessage     * VerificationMessageTemplate.EmailMessageByLink     * VerificationMessageTemplate.EmailSubject,     * VerificationMessageTemplate.EmailSubjectByLink     * DEVELOPER    * When Amazon Cognito emails your users, it uses your Amazon SES configuration. Amazon Cognito calls Amazon SES on your behalf to send email from your verified email address. When you use this option, the email delivery limits are the same limits that apply to your Amazon SES verified email address in your AWS account. If you use this option, you must provide the ARN of an Amazon SES verified email address for the @SourceArn@ parameter. Before Amazon Cognito can email your users, it requires additional permissions to call Amazon SES on your behalf. When you update your user pool with this option, Amazon Cognito creates a /service-linked role/ , which is a type of IAM role, in your AWS account. This role contains the permissions that allow Amazon Cognito to access Amazon SES and send email messages with your address. For more information about the service-linked role that Amazon Cognito creates, see <https://docs.aws.amazon.com/cognito/latest/developerguide/using-service-linked-roles.html Using Service-Linked Roles for Amazon Cognito> in the /Amazon Cognito Developer Guide/ .
emailConfigurationType ::
  EmailConfigurationType
emailConfigurationType =
  EmailConfigurationType'
    { _ectSourceARN = Nothing,
      _ectFrom = Nothing,
      _ectConfigurationSet = Nothing,
      _ectReplyToEmailAddress = Nothing,
      _ectEmailSendingAccount = Nothing
    }

-- | The Amazon Resource Name (ARN) of a verified email address in Amazon SES. This email address is used in one of the following ways, depending on the value that you specify for the @EmailSendingAccount@ parameter:     * If you specify @COGNITO_DEFAULT@ , Amazon Cognito uses this address as the custom FROM address when it emails your users by using its built-in email account.     * If you specify @DEVELOPER@ , Amazon Cognito emails your users with this address by calling Amazon SES on your behalf.
ectSourceARN :: Lens' EmailConfigurationType (Maybe Text)
ectSourceARN = lens _ectSourceARN (\s a -> s {_ectSourceARN = a})

-- | Identifies either the sender’s email address or the sender’s name with their email address. For example, @testuser@example.com@ or @Test User <testuser@example.com>@ . This address will appear before the body of the email.
ectFrom :: Lens' EmailConfigurationType (Maybe Text)
ectFrom = lens _ectFrom (\s a -> s {_ectFrom = a})

-- | The set of configuration rules that can be applied to emails sent using Amazon SES. A configuration set is applied to an email by including a reference to the configuration set in the headers of the email. Once applied, all of the rules in that configuration set are applied to the email. Configuration sets can be used to apply the following types of rules to emails:      * Event publishing – Amazon SES can track the number of send, delivery, open, click, bounce, and complaint events for each email sent. Use event publishing to send information about these events to other AWS services such as SNS and CloudWatch.     * IP pool management – When leasing dedicated IP addresses with Amazon SES, you can create groups of IP addresses, called dedicated IP pools. You can then associate the dedicated IP pools with configuration sets.
ectConfigurationSet :: Lens' EmailConfigurationType (Maybe Text)
ectConfigurationSet = lens _ectConfigurationSet (\s a -> s {_ectConfigurationSet = a})

-- | The destination to which the receiver of the email should reply to.
ectReplyToEmailAddress :: Lens' EmailConfigurationType (Maybe Text)
ectReplyToEmailAddress = lens _ectReplyToEmailAddress (\s a -> s {_ectReplyToEmailAddress = a})

-- | Specifies whether Amazon Cognito emails your users by using its built-in email functionality or your Amazon SES email configuration. Specify one of the following values:     * COGNITO_DEFAULT    * When Amazon Cognito emails your users, it uses its built-in email functionality. When you use the default option, Amazon Cognito allows only a limited number of emails each day for your user pool. For typical production environments, the default email limit is below the required delivery volume. To achieve a higher delivery volume, specify DEVELOPER to use your Amazon SES email configuration. To look up the email delivery limit for the default option, see <https://docs.aws.amazon.com/cognito/latest/developerguide/limits.html Limits in Amazon Cognito> in the /Amazon Cognito Developer Guide/ . The default FROM address is no-reply@verificationemail.com. To customize the FROM address, provide the ARN of an Amazon SES verified email address for the @SourceArn@ parameter. If EmailSendingAccount is COGNITO_DEFAULT, the following parameters aren't allowed:     * EmailVerificationMessage     * EmailVerificationSubject     * InviteMessageTemplate.EmailMessage     * InviteMessageTemplate.EmailSubject     * VerificationMessageTemplate.EmailMessage     * VerificationMessageTemplate.EmailMessageByLink     * VerificationMessageTemplate.EmailSubject,     * VerificationMessageTemplate.EmailSubjectByLink     * DEVELOPER    * When Amazon Cognito emails your users, it uses your Amazon SES configuration. Amazon Cognito calls Amazon SES on your behalf to send email from your verified email address. When you use this option, the email delivery limits are the same limits that apply to your Amazon SES verified email address in your AWS account. If you use this option, you must provide the ARN of an Amazon SES verified email address for the @SourceArn@ parameter. Before Amazon Cognito can email your users, it requires additional permissions to call Amazon SES on your behalf. When you update your user pool with this option, Amazon Cognito creates a /service-linked role/ , which is a type of IAM role, in your AWS account. This role contains the permissions that allow Amazon Cognito to access Amazon SES and send email messages with your address. For more information about the service-linked role that Amazon Cognito creates, see <https://docs.aws.amazon.com/cognito/latest/developerguide/using-service-linked-roles.html Using Service-Linked Roles for Amazon Cognito> in the /Amazon Cognito Developer Guide/ .
ectEmailSendingAccount :: Lens' EmailConfigurationType (Maybe EmailSendingAccountType)
ectEmailSendingAccount = lens _ectEmailSendingAccount (\s a -> s {_ectEmailSendingAccount = a})

instance FromJSON EmailConfigurationType where
  parseJSON =
    withObject
      "EmailConfigurationType"
      ( \x ->
          EmailConfigurationType'
            <$> (x .:? "SourceArn")
            <*> (x .:? "From")
            <*> (x .:? "ConfigurationSet")
            <*> (x .:? "ReplyToEmailAddress")
            <*> (x .:? "EmailSendingAccount")
      )

instance Hashable EmailConfigurationType

instance NFData EmailConfigurationType

instance ToJSON EmailConfigurationType where
  toJSON EmailConfigurationType' {..} =
    object
      ( catMaybes
          [ ("SourceArn" .=) <$> _ectSourceARN,
            ("From" .=) <$> _ectFrom,
            ("ConfigurationSet" .=) <$> _ectConfigurationSet,
            ("ReplyToEmailAddress" .=) <$> _ectReplyToEmailAddress,
            ("EmailSendingAccount" .=) <$> _ectEmailSendingAccount
          ]
      )
