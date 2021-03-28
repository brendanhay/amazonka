{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.ConversationLogsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexModels.Types.ConversationLogsResponse
  ( ConversationLogsResponse (..)
  -- * Smart constructor
  , mkConversationLogsResponse
  -- * Lenses
  , clrIamRoleArn
  , clrLogSettings
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types.IamRoleArn as Types
import qualified Network.AWS.LexModels.Types.LogSettingsResponse as Types
import qualified Network.AWS.Prelude as Core

-- | Contains information about conversation log settings.
--
-- /See:/ 'mkConversationLogsResponse' smart constructor.
data ConversationLogsResponse = ConversationLogsResponse'
  { iamRoleArn :: Core.Maybe Types.IamRoleArn
    -- ^ The Amazon Resource Name (ARN) of the IAM role used to write your logs to CloudWatch Logs or an S3 bucket.
  , logSettings :: Core.Maybe [Types.LogSettingsResponse]
    -- ^ The settings for your conversation logs. You can log text, audio, or both.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConversationLogsResponse' value with any optional fields omitted.
mkConversationLogsResponse
    :: ConversationLogsResponse
mkConversationLogsResponse
  = ConversationLogsResponse'{iamRoleArn = Core.Nothing,
                              logSettings = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the IAM role used to write your logs to CloudWatch Logs or an S3 bucket.
--
-- /Note:/ Consider using 'iamRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clrIamRoleArn :: Lens.Lens' ConversationLogsResponse (Core.Maybe Types.IamRoleArn)
clrIamRoleArn = Lens.field @"iamRoleArn"
{-# INLINEABLE clrIamRoleArn #-}
{-# DEPRECATED iamRoleArn "Use generic-lens or generic-optics with 'iamRoleArn' instead"  #-}

-- | The settings for your conversation logs. You can log text, audio, or both.
--
-- /Note:/ Consider using 'logSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clrLogSettings :: Lens.Lens' ConversationLogsResponse (Core.Maybe [Types.LogSettingsResponse])
clrLogSettings = Lens.field @"logSettings"
{-# INLINEABLE clrLogSettings #-}
{-# DEPRECATED logSettings "Use generic-lens or generic-optics with 'logSettings' instead"  #-}

instance Core.FromJSON ConversationLogsResponse where
        parseJSON
          = Core.withObject "ConversationLogsResponse" Core.$
              \ x ->
                ConversationLogsResponse' Core.<$>
                  (x Core..:? "iamRoleArn") Core.<*> x Core..:? "logSettings"
