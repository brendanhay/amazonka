{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.ConversationLogsRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.ConversationLogsRequest where

import Network.AWS.Lens
import Network.AWS.LexModels.Types.LogSettingsRequest
import Network.AWS.Prelude

-- | Provides the settings needed for conversation logs.
--
--
--
-- /See:/ 'conversationLogsRequest' smart constructor.
data ConversationLogsRequest = ConversationLogsRequest'
  { _clrLogSettings ::
      ![LogSettingsRequest],
    _clrIamRoleARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConversationLogsRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clrLogSettings' - The settings for your conversation logs. You can log the conversation text, conversation audio, or both.
--
-- * 'clrIamRoleARN' - The Amazon Resource Name (ARN) of an IAM role with permission to write to your CloudWatch Logs for text logs and your S3 bucket for audio logs. If audio encryption is enabled, this role also provides access permission for the AWS KMS key used for encrypting audio logs. For more information, see <https://docs.aws.amazon.com/lex/latest/dg/conversation-logs-role-and-policy.html Creating an IAM Role and Policy for Conversation Logs> .
conversationLogsRequest ::
  -- | 'clrIamRoleARN'
  Text ->
  ConversationLogsRequest
conversationLogsRequest pIamRoleARN_ =
  ConversationLogsRequest'
    { _clrLogSettings = mempty,
      _clrIamRoleARN = pIamRoleARN_
    }

-- | The settings for your conversation logs. You can log the conversation text, conversation audio, or both.
clrLogSettings :: Lens' ConversationLogsRequest [LogSettingsRequest]
clrLogSettings = lens _clrLogSettings (\s a -> s {_clrLogSettings = a}) . _Coerce

-- | The Amazon Resource Name (ARN) of an IAM role with permission to write to your CloudWatch Logs for text logs and your S3 bucket for audio logs. If audio encryption is enabled, this role also provides access permission for the AWS KMS key used for encrypting audio logs. For more information, see <https://docs.aws.amazon.com/lex/latest/dg/conversation-logs-role-and-policy.html Creating an IAM Role and Policy for Conversation Logs> .
clrIamRoleARN :: Lens' ConversationLogsRequest Text
clrIamRoleARN = lens _clrIamRoleARN (\s a -> s {_clrIamRoleARN = a})

instance Hashable ConversationLogsRequest

instance NFData ConversationLogsRequest

instance ToJSON ConversationLogsRequest where
  toJSON ConversationLogsRequest' {..} =
    object
      ( catMaybes
          [ Just ("logSettings" .= _clrLogSettings),
            Just ("iamRoleArn" .= _clrIamRoleARN)
          ]
      )
