{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOptionUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOptionUpdate
  ( CloudWatchLoggingOptionUpdate (..),

    -- * Smart constructor
    mkCloudWatchLoggingOptionUpdate,

    -- * Lenses
    cwlouRoleARNUpdate,
    cwlouLogStreamARNUpdate,
    cwlouCloudWatchLoggingOptionId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes CloudWatch logging option updates.
--
-- /See:/ 'mkCloudWatchLoggingOptionUpdate' smart constructor.
data CloudWatchLoggingOptionUpdate = CloudWatchLoggingOptionUpdate'
  { roleARNUpdate ::
      Lude.Maybe Lude.Text,
    logStreamARNUpdate ::
      Lude.Maybe Lude.Text,
    cloudWatchLoggingOptionId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CloudWatchLoggingOptionUpdate' with the minimum fields required to make a request.
--
-- * 'cloudWatchLoggingOptionId' - ID of the CloudWatch logging option to update
-- * 'logStreamARNUpdate' - ARN of the CloudWatch log to receive application messages.
-- * 'roleARNUpdate' - IAM ARN of the role to use to send application messages. Note: To write application messages to CloudWatch, the IAM role used must have the @PutLogEvents@ policy action enabled.
mkCloudWatchLoggingOptionUpdate ::
  -- | 'cloudWatchLoggingOptionId'
  Lude.Text ->
  CloudWatchLoggingOptionUpdate
mkCloudWatchLoggingOptionUpdate pCloudWatchLoggingOptionId_ =
  CloudWatchLoggingOptionUpdate'
    { roleARNUpdate = Lude.Nothing,
      logStreamARNUpdate = Lude.Nothing,
      cloudWatchLoggingOptionId = pCloudWatchLoggingOptionId_
    }

-- | IAM ARN of the role to use to send application messages. Note: To write application messages to CloudWatch, the IAM role used must have the @PutLogEvents@ policy action enabled.
--
-- /Note:/ Consider using 'roleARNUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwlouRoleARNUpdate :: Lens.Lens' CloudWatchLoggingOptionUpdate (Lude.Maybe Lude.Text)
cwlouRoleARNUpdate = Lens.lens (roleARNUpdate :: CloudWatchLoggingOptionUpdate -> Lude.Maybe Lude.Text) (\s a -> s {roleARNUpdate = a} :: CloudWatchLoggingOptionUpdate)
{-# DEPRECATED cwlouRoleARNUpdate "Use generic-lens or generic-optics with 'roleARNUpdate' instead." #-}

-- | ARN of the CloudWatch log to receive application messages.
--
-- /Note:/ Consider using 'logStreamARNUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwlouLogStreamARNUpdate :: Lens.Lens' CloudWatchLoggingOptionUpdate (Lude.Maybe Lude.Text)
cwlouLogStreamARNUpdate = Lens.lens (logStreamARNUpdate :: CloudWatchLoggingOptionUpdate -> Lude.Maybe Lude.Text) (\s a -> s {logStreamARNUpdate = a} :: CloudWatchLoggingOptionUpdate)
{-# DEPRECATED cwlouLogStreamARNUpdate "Use generic-lens or generic-optics with 'logStreamARNUpdate' instead." #-}

-- | ID of the CloudWatch logging option to update
--
-- /Note:/ Consider using 'cloudWatchLoggingOptionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwlouCloudWatchLoggingOptionId :: Lens.Lens' CloudWatchLoggingOptionUpdate Lude.Text
cwlouCloudWatchLoggingOptionId = Lens.lens (cloudWatchLoggingOptionId :: CloudWatchLoggingOptionUpdate -> Lude.Text) (\s a -> s {cloudWatchLoggingOptionId = a} :: CloudWatchLoggingOptionUpdate)
{-# DEPRECATED cwlouCloudWatchLoggingOptionId "Use generic-lens or generic-optics with 'cloudWatchLoggingOptionId' instead." #-}

instance Lude.ToJSON CloudWatchLoggingOptionUpdate where
  toJSON CloudWatchLoggingOptionUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RoleARNUpdate" Lude..=) Lude.<$> roleARNUpdate,
            ("LogStreamARNUpdate" Lude..=) Lude.<$> logStreamARNUpdate,
            Lude.Just
              ("CloudWatchLoggingOptionId" Lude..= cloudWatchLoggingOptionId)
          ]
      )
