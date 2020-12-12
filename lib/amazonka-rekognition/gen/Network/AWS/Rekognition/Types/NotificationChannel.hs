{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.NotificationChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.NotificationChannel
  ( NotificationChannel (..),

    -- * Smart constructor
    mkNotificationChannel,

    -- * Lenses
    ncSNSTopicARN,
    ncRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The Amazon Simple Notification Service topic to which Amazon Rekognition publishes the completion status of a video analysis operation. For more information, see 'api-video' .
--
-- /See:/ 'mkNotificationChannel' smart constructor.
data NotificationChannel = NotificationChannel'
  { snsTopicARN ::
      Lude.Text,
    roleARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NotificationChannel' with the minimum fields required to make a request.
--
-- * 'roleARN' - The ARN of an IAM role that gives Amazon Rekognition publishing permissions to the Amazon SNS topic.
-- * 'snsTopicARN' - The Amazon SNS topic to which Amazon Rekognition to posts the completion status.
mkNotificationChannel ::
  -- | 'snsTopicARN'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  NotificationChannel
mkNotificationChannel pSNSTopicARN_ pRoleARN_ =
  NotificationChannel'
    { snsTopicARN = pSNSTopicARN_,
      roleARN = pRoleARN_
    }

-- | The Amazon SNS topic to which Amazon Rekognition to posts the completion status.
--
-- /Note:/ Consider using 'snsTopicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncSNSTopicARN :: Lens.Lens' NotificationChannel Lude.Text
ncSNSTopicARN = Lens.lens (snsTopicARN :: NotificationChannel -> Lude.Text) (\s a -> s {snsTopicARN = a} :: NotificationChannel)
{-# DEPRECATED ncSNSTopicARN "Use generic-lens or generic-optics with 'snsTopicARN' instead." #-}

-- | The ARN of an IAM role that gives Amazon Rekognition publishing permissions to the Amazon SNS topic.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncRoleARN :: Lens.Lens' NotificationChannel Lude.Text
ncRoleARN = Lens.lens (roleARN :: NotificationChannel -> Lude.Text) (\s a -> s {roleARN = a} :: NotificationChannel)
{-# DEPRECATED ncRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.ToJSON NotificationChannel where
  toJSON NotificationChannel' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("SNSTopicArn" Lude..= snsTopicARN),
            Lude.Just ("RoleArn" Lude..= roleARN)
          ]
      )
