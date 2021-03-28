{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.ConversationLogsRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexModels.Types.ConversationLogsRequest
  ( ConversationLogsRequest (..)
  -- * Smart constructor
  , mkConversationLogsRequest
  -- * Lenses
  , cLogSettings
  , cIamRoleArn
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types.IamRoleArn as Types
import qualified Network.AWS.LexModels.Types.LogSettingsRequest as Types
import qualified Network.AWS.Prelude as Core

-- | Provides the settings needed for conversation logs.
--
-- /See:/ 'mkConversationLogsRequest' smart constructor.
data ConversationLogsRequest = ConversationLogsRequest'
  { logSettings :: [Types.LogSettingsRequest]
    -- ^ The settings for your conversation logs. You can log the conversation text, conversation audio, or both.
  , iamRoleArn :: Types.IamRoleArn
    -- ^ The Amazon Resource Name (ARN) of an IAM role with permission to write to your CloudWatch Logs for text logs and your S3 bucket for audio logs. If audio encryption is enabled, this role also provides access permission for the AWS KMS key used for encrypting audio logs. For more information, see <https://docs.aws.amazon.com/lex/latest/dg/conversation-logs-role-and-policy.html Creating an IAM Role and Policy for Conversation Logs> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConversationLogsRequest' value with any optional fields omitted.
mkConversationLogsRequest
    :: Types.IamRoleArn -- ^ 'iamRoleArn'
    -> ConversationLogsRequest
mkConversationLogsRequest iamRoleArn
  = ConversationLogsRequest'{logSettings = Core.mempty, iamRoleArn}

-- | The settings for your conversation logs. You can log the conversation text, conversation audio, or both.
--
-- /Note:/ Consider using 'logSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cLogSettings :: Lens.Lens' ConversationLogsRequest [Types.LogSettingsRequest]
cLogSettings = Lens.field @"logSettings"
{-# INLINEABLE cLogSettings #-}
{-# DEPRECATED logSettings "Use generic-lens or generic-optics with 'logSettings' instead"  #-}

-- | The Amazon Resource Name (ARN) of an IAM role with permission to write to your CloudWatch Logs for text logs and your S3 bucket for audio logs. If audio encryption is enabled, this role also provides access permission for the AWS KMS key used for encrypting audio logs. For more information, see <https://docs.aws.amazon.com/lex/latest/dg/conversation-logs-role-and-policy.html Creating an IAM Role and Policy for Conversation Logs> .
--
-- /Note:/ Consider using 'iamRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cIamRoleArn :: Lens.Lens' ConversationLogsRequest Types.IamRoleArn
cIamRoleArn = Lens.field @"iamRoleArn"
{-# INLINEABLE cIamRoleArn #-}
{-# DEPRECATED iamRoleArn "Use generic-lens or generic-optics with 'iamRoleArn' instead"  #-}

instance Core.FromJSON ConversationLogsRequest where
        toJSON ConversationLogsRequest{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("logSettings" Core..= logSettings),
                  Core.Just ("iamRoleArn" Core..= iamRoleArn)])
