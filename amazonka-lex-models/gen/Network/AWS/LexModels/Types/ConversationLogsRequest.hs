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
-- Module      : Network.AWS.LexModels.Types.ConversationLogsRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.ConversationLogsRequest where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types.LogSettingsRequest
import qualified Network.AWS.Prelude as Prelude

-- | Provides the settings needed for conversation logs.
--
-- /See:/ 'newConversationLogsRequest' smart constructor.
data ConversationLogsRequest = ConversationLogsRequest'
  { -- | The settings for your conversation logs. You can log the conversation
    -- text, conversation audio, or both.
    logSettings :: [LogSettingsRequest],
    -- | The Amazon Resource Name (ARN) of an IAM role with permission to write
    -- to your CloudWatch Logs for text logs and your S3 bucket for audio logs.
    -- If audio encryption is enabled, this role also provides access
    -- permission for the AWS KMS key used for encrypting audio logs. For more
    -- information, see
    -- <https://docs.aws.amazon.com/lex/latest/dg/conversation-logs-role-and-policy.html Creating an IAM Role and Policy for Conversation Logs>.
    iamRoleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ConversationLogsRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logSettings', 'conversationLogsRequest_logSettings' - The settings for your conversation logs. You can log the conversation
-- text, conversation audio, or both.
--
-- 'iamRoleArn', 'conversationLogsRequest_iamRoleArn' - The Amazon Resource Name (ARN) of an IAM role with permission to write
-- to your CloudWatch Logs for text logs and your S3 bucket for audio logs.
-- If audio encryption is enabled, this role also provides access
-- permission for the AWS KMS key used for encrypting audio logs. For more
-- information, see
-- <https://docs.aws.amazon.com/lex/latest/dg/conversation-logs-role-and-policy.html Creating an IAM Role and Policy for Conversation Logs>.
newConversationLogsRequest ::
  -- | 'iamRoleArn'
  Prelude.Text ->
  ConversationLogsRequest
newConversationLogsRequest pIamRoleArn_ =
  ConversationLogsRequest'
    { logSettings =
        Prelude.mempty,
      iamRoleArn = pIamRoleArn_
    }

-- | The settings for your conversation logs. You can log the conversation
-- text, conversation audio, or both.
conversationLogsRequest_logSettings :: Lens.Lens' ConversationLogsRequest [LogSettingsRequest]
conversationLogsRequest_logSettings = Lens.lens (\ConversationLogsRequest' {logSettings} -> logSettings) (\s@ConversationLogsRequest' {} a -> s {logSettings = a} :: ConversationLogsRequest) Prelude.. Prelude._Coerce

-- | The Amazon Resource Name (ARN) of an IAM role with permission to write
-- to your CloudWatch Logs for text logs and your S3 bucket for audio logs.
-- If audio encryption is enabled, this role also provides access
-- permission for the AWS KMS key used for encrypting audio logs. For more
-- information, see
-- <https://docs.aws.amazon.com/lex/latest/dg/conversation-logs-role-and-policy.html Creating an IAM Role and Policy for Conversation Logs>.
conversationLogsRequest_iamRoleArn :: Lens.Lens' ConversationLogsRequest Prelude.Text
conversationLogsRequest_iamRoleArn = Lens.lens (\ConversationLogsRequest' {iamRoleArn} -> iamRoleArn) (\s@ConversationLogsRequest' {} a -> s {iamRoleArn = a} :: ConversationLogsRequest)

instance Prelude.Hashable ConversationLogsRequest

instance Prelude.NFData ConversationLogsRequest

instance Prelude.ToJSON ConversationLogsRequest where
  toJSON ConversationLogsRequest' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("logSettings" Prelude..= logSettings),
            Prelude.Just ("iamRoleArn" Prelude..= iamRoleArn)
          ]
      )
