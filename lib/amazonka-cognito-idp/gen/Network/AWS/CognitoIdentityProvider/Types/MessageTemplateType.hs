{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.MessageTemplateType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.MessageTemplateType where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The message template structure.
--
--
--
-- /See:/ 'messageTemplateType' smart constructor.
data MessageTemplateType = MessageTemplateType'
  { _mttEmailSubject ::
      !(Maybe Text),
    _mttSMSMessage :: !(Maybe Text),
    _mttEmailMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MessageTemplateType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mttEmailSubject' - The subject line for email messages. EmailSubject is allowed only if <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount> is DEVELOPER.
--
-- * 'mttSMSMessage' - The message template for SMS messages.
--
-- * 'mttEmailMessage' - The message template for email messages. EmailMessage is allowed only if <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount> is DEVELOPER.
messageTemplateType ::
  MessageTemplateType
messageTemplateType =
  MessageTemplateType'
    { _mttEmailSubject = Nothing,
      _mttSMSMessage = Nothing,
      _mttEmailMessage = Nothing
    }

-- | The subject line for email messages. EmailSubject is allowed only if <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount> is DEVELOPER.
mttEmailSubject :: Lens' MessageTemplateType (Maybe Text)
mttEmailSubject = lens _mttEmailSubject (\s a -> s {_mttEmailSubject = a})

-- | The message template for SMS messages.
mttSMSMessage :: Lens' MessageTemplateType (Maybe Text)
mttSMSMessage = lens _mttSMSMessage (\s a -> s {_mttSMSMessage = a})

-- | The message template for email messages. EmailMessage is allowed only if <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount> is DEVELOPER.
mttEmailMessage :: Lens' MessageTemplateType (Maybe Text)
mttEmailMessage = lens _mttEmailMessage (\s a -> s {_mttEmailMessage = a})

instance FromJSON MessageTemplateType where
  parseJSON =
    withObject
      "MessageTemplateType"
      ( \x ->
          MessageTemplateType'
            <$> (x .:? "EmailSubject")
            <*> (x .:? "SMSMessage")
            <*> (x .:? "EmailMessage")
      )

instance Hashable MessageTemplateType

instance NFData MessageTemplateType

instance ToJSON MessageTemplateType where
  toJSON MessageTemplateType' {..} =
    object
      ( catMaybes
          [ ("EmailSubject" .=) <$> _mttEmailSubject,
            ("SMSMessage" .=) <$> _mttSMSMessage,
            ("EmailMessage" .=) <$> _mttEmailMessage
          ]
      )
