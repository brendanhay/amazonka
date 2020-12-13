{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types.NotificationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types.NotificationConfiguration
  ( NotificationConfiguration (..),

    -- * Smart constructor
    mkNotificationConfiguration,

    -- * Lenses
    ncTopicStatus,
    ncTopicARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a notification topic and its status. Notification topics are used for publishing DAX events to subscribers using Amazon Simple Notification Service (SNS).
--
-- /See:/ 'mkNotificationConfiguration' smart constructor.
data NotificationConfiguration = NotificationConfiguration'
  { -- | The current state of the topic.
    topicStatus :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) that identifies the topic.
    topicARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NotificationConfiguration' with the minimum fields required to make a request.
--
-- * 'topicStatus' - The current state of the topic.
-- * 'topicARN' - The Amazon Resource Name (ARN) that identifies the topic.
mkNotificationConfiguration ::
  NotificationConfiguration
mkNotificationConfiguration =
  NotificationConfiguration'
    { topicStatus = Lude.Nothing,
      topicARN = Lude.Nothing
    }

-- | The current state of the topic.
--
-- /Note:/ Consider using 'topicStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncTopicStatus :: Lens.Lens' NotificationConfiguration (Lude.Maybe Lude.Text)
ncTopicStatus = Lens.lens (topicStatus :: NotificationConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {topicStatus = a} :: NotificationConfiguration)
{-# DEPRECATED ncTopicStatus "Use generic-lens or generic-optics with 'topicStatus' instead." #-}

-- | The Amazon Resource Name (ARN) that identifies the topic.
--
-- /Note:/ Consider using 'topicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncTopicARN :: Lens.Lens' NotificationConfiguration (Lude.Maybe Lude.Text)
ncTopicARN = Lens.lens (topicARN :: NotificationConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {topicARN = a} :: NotificationConfiguration)
{-# DEPRECATED ncTopicARN "Use generic-lens or generic-optics with 'topicARN' instead." #-}

instance Lude.FromJSON NotificationConfiguration where
  parseJSON =
    Lude.withObject
      "NotificationConfiguration"
      ( \x ->
          NotificationConfiguration'
            Lude.<$> (x Lude..:? "TopicStatus") Lude.<*> (x Lude..:? "TopicArn")
      )
