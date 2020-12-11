-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.ConversationLogsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.ConversationLogsResponse
  ( ConversationLogsResponse (..),

    -- * Smart constructor
    mkConversationLogsResponse,

    -- * Lenses
    clIamRoleARN,
    clLogSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types.LogSettingsResponse
import qualified Network.AWS.Prelude as Lude

-- | Contains information about conversation log settings.
--
-- /See:/ 'mkConversationLogsResponse' smart constructor.
data ConversationLogsResponse = ConversationLogsResponse'
  { iamRoleARN ::
      Lude.Maybe Lude.Text,
    logSettings ::
      Lude.Maybe [LogSettingsResponse]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConversationLogsResponse' with the minimum fields required to make a request.
--
-- * 'iamRoleARN' - The Amazon Resource Name (ARN) of the IAM role used to write your logs to CloudWatch Logs or an S3 bucket.
-- * 'logSettings' - The settings for your conversation logs. You can log text, audio, or both.
mkConversationLogsResponse ::
  ConversationLogsResponse
mkConversationLogsResponse =
  ConversationLogsResponse'
    { iamRoleARN = Lude.Nothing,
      logSettings = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the IAM role used to write your logs to CloudWatch Logs or an S3 bucket.
--
-- /Note:/ Consider using 'iamRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clIamRoleARN :: Lens.Lens' ConversationLogsResponse (Lude.Maybe Lude.Text)
clIamRoleARN = Lens.lens (iamRoleARN :: ConversationLogsResponse -> Lude.Maybe Lude.Text) (\s a -> s {iamRoleARN = a} :: ConversationLogsResponse)
{-# DEPRECATED clIamRoleARN "Use generic-lens or generic-optics with 'iamRoleARN' instead." #-}

-- | The settings for your conversation logs. You can log text, audio, or both.
--
-- /Note:/ Consider using 'logSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clLogSettings :: Lens.Lens' ConversationLogsResponse (Lude.Maybe [LogSettingsResponse])
clLogSettings = Lens.lens (logSettings :: ConversationLogsResponse -> Lude.Maybe [LogSettingsResponse]) (\s a -> s {logSettings = a} :: ConversationLogsResponse)
{-# DEPRECATED clLogSettings "Use generic-lens or generic-optics with 'logSettings' instead." #-}

instance Lude.FromJSON ConversationLogsResponse where
  parseJSON =
    Lude.withObject
      "ConversationLogsResponse"
      ( \x ->
          ConversationLogsResponse'
            Lude.<$> (x Lude..:? "iamRoleArn")
            Lude.<*> (x Lude..:? "logSettings" Lude..!= Lude.mempty)
      )
