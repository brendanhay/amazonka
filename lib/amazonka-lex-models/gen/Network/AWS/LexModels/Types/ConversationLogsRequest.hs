{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.ConversationLogsRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.ConversationLogsRequest
  ( ConversationLogsRequest (..),

    -- * Smart constructor
    mkConversationLogsRequest,

    -- * Lenses
    clrIamRoleARN,
    clrLogSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types.LogSettingsRequest
import qualified Network.AWS.Prelude as Lude

-- | Provides the settings needed for conversation logs.
--
-- /See:/ 'mkConversationLogsRequest' smart constructor.
data ConversationLogsRequest = ConversationLogsRequest'
  { -- | The Amazon Resource Name (ARN) of an IAM role with permission to write to your CloudWatch Logs for text logs and your S3 bucket for audio logs. If audio encryption is enabled, this role also provides access permission for the AWS KMS key used for encrypting audio logs. For more information, see <https://docs.aws.amazon.com/lex/latest/dg/conversation-logs-role-and-policy.html Creating an IAM Role and Policy for Conversation Logs> .
    iamRoleARN :: Lude.Text,
    -- | The settings for your conversation logs. You can log the conversation text, conversation audio, or both.
    logSettings :: [LogSettingsRequest]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConversationLogsRequest' with the minimum fields required to make a request.
--
-- * 'iamRoleARN' - The Amazon Resource Name (ARN) of an IAM role with permission to write to your CloudWatch Logs for text logs and your S3 bucket for audio logs. If audio encryption is enabled, this role also provides access permission for the AWS KMS key used for encrypting audio logs. For more information, see <https://docs.aws.amazon.com/lex/latest/dg/conversation-logs-role-and-policy.html Creating an IAM Role and Policy for Conversation Logs> .
-- * 'logSettings' - The settings for your conversation logs. You can log the conversation text, conversation audio, or both.
mkConversationLogsRequest ::
  -- | 'iamRoleARN'
  Lude.Text ->
  ConversationLogsRequest
mkConversationLogsRequest pIamRoleARN_ =
  ConversationLogsRequest'
    { iamRoleARN = pIamRoleARN_,
      logSettings = Lude.mempty
    }

-- | The Amazon Resource Name (ARN) of an IAM role with permission to write to your CloudWatch Logs for text logs and your S3 bucket for audio logs. If audio encryption is enabled, this role also provides access permission for the AWS KMS key used for encrypting audio logs. For more information, see <https://docs.aws.amazon.com/lex/latest/dg/conversation-logs-role-and-policy.html Creating an IAM Role and Policy for Conversation Logs> .
--
-- /Note:/ Consider using 'iamRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clrIamRoleARN :: Lens.Lens' ConversationLogsRequest Lude.Text
clrIamRoleARN = Lens.lens (iamRoleARN :: ConversationLogsRequest -> Lude.Text) (\s a -> s {iamRoleARN = a} :: ConversationLogsRequest)
{-# DEPRECATED clrIamRoleARN "Use generic-lens or generic-optics with 'iamRoleARN' instead." #-}

-- | The settings for your conversation logs. You can log the conversation text, conversation audio, or both.
--
-- /Note:/ Consider using 'logSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clrLogSettings :: Lens.Lens' ConversationLogsRequest [LogSettingsRequest]
clrLogSettings = Lens.lens (logSettings :: ConversationLogsRequest -> [LogSettingsRequest]) (\s a -> s {logSettings = a} :: ConversationLogsRequest)
{-# DEPRECATED clrLogSettings "Use generic-lens or generic-optics with 'logSettings' instead." #-}

instance Lude.ToJSON ConversationLogsRequest where
  toJSON ConversationLogsRequest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("iamRoleArn" Lude..= iamRoleARN),
            Lude.Just ("logSettings" Lude..= logSettings)
          ]
      )
