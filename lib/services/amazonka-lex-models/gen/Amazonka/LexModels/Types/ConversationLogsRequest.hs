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
-- Module      : Amazonka.LexModels.Types.ConversationLogsRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexModels.Types.ConversationLogsRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexModels.Types.LogSettingsRequest
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
conversationLogsRequest_logSettings = Lens.lens (\ConversationLogsRequest' {logSettings} -> logSettings) (\s@ConversationLogsRequest' {} a -> s {logSettings = a} :: ConversationLogsRequest) Prelude.. Lens.coerced

-- | The Amazon Resource Name (ARN) of an IAM role with permission to write
-- to your CloudWatch Logs for text logs and your S3 bucket for audio logs.
-- If audio encryption is enabled, this role also provides access
-- permission for the AWS KMS key used for encrypting audio logs. For more
-- information, see
-- <https://docs.aws.amazon.com/lex/latest/dg/conversation-logs-role-and-policy.html Creating an IAM Role and Policy for Conversation Logs>.
conversationLogsRequest_iamRoleArn :: Lens.Lens' ConversationLogsRequest Prelude.Text
conversationLogsRequest_iamRoleArn = Lens.lens (\ConversationLogsRequest' {iamRoleArn} -> iamRoleArn) (\s@ConversationLogsRequest' {} a -> s {iamRoleArn = a} :: ConversationLogsRequest)

instance Prelude.Hashable ConversationLogsRequest where
  hashWithSalt _salt ConversationLogsRequest' {..} =
    _salt
      `Prelude.hashWithSalt` logSettings
      `Prelude.hashWithSalt` iamRoleArn

instance Prelude.NFData ConversationLogsRequest where
  rnf ConversationLogsRequest' {..} =
    Prelude.rnf logSettings `Prelude.seq`
      Prelude.rnf iamRoleArn

instance Data.ToJSON ConversationLogsRequest where
  toJSON ConversationLogsRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("logSettings" Data..= logSettings),
            Prelude.Just ("iamRoleArn" Data..= iamRoleArn)
          ]
      )
