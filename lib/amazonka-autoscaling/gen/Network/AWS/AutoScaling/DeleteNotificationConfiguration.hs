{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DeleteNotificationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified notification.
module Network.AWS.AutoScaling.DeleteNotificationConfiguration
  ( -- * Creating a request
    DeleteNotificationConfiguration (..),
    mkDeleteNotificationConfiguration,

    -- ** Request lenses
    dncAutoScalingGroupName,
    dncTopicARN,

    -- * Destructuring the response
    DeleteNotificationConfigurationResponse (..),
    mkDeleteNotificationConfigurationResponse,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteNotificationConfiguration' smart constructor.
data DeleteNotificationConfiguration = DeleteNotificationConfiguration'
  { autoScalingGroupName ::
      Lude.Text,
    topicARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteNotificationConfiguration' with the minimum fields required to make a request.
--
-- * 'autoScalingGroupName' - The name of the Auto Scaling group.
-- * 'topicARN' - The Amazon Resource Name (ARN) of the Amazon Simple Notification Service (Amazon SNS) topic.
mkDeleteNotificationConfiguration ::
  -- | 'autoScalingGroupName'
  Lude.Text ->
  -- | 'topicARN'
  Lude.Text ->
  DeleteNotificationConfiguration
mkDeleteNotificationConfiguration pAutoScalingGroupName_ pTopicARN_ =
  DeleteNotificationConfiguration'
    { autoScalingGroupName =
        pAutoScalingGroupName_,
      topicARN = pTopicARN_
    }

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncAutoScalingGroupName :: Lens.Lens' DeleteNotificationConfiguration Lude.Text
dncAutoScalingGroupName = Lens.lens (autoScalingGroupName :: DeleteNotificationConfiguration -> Lude.Text) (\s a -> s {autoScalingGroupName = a} :: DeleteNotificationConfiguration)
{-# DEPRECATED dncAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service (Amazon SNS) topic.
--
-- /Note:/ Consider using 'topicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncTopicARN :: Lens.Lens' DeleteNotificationConfiguration Lude.Text
dncTopicARN = Lens.lens (topicARN :: DeleteNotificationConfiguration -> Lude.Text) (\s a -> s {topicARN = a} :: DeleteNotificationConfiguration)
{-# DEPRECATED dncTopicARN "Use generic-lens or generic-optics with 'topicARN' instead." #-}

instance Lude.AWSRequest DeleteNotificationConfiguration where
  type
    Rs DeleteNotificationConfiguration =
      DeleteNotificationConfigurationResponse
  request = Req.postQuery autoScalingService
  response = Res.receiveNull DeleteNotificationConfigurationResponse'

instance Lude.ToHeaders DeleteNotificationConfiguration where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteNotificationConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteNotificationConfiguration where
  toQuery DeleteNotificationConfiguration' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DeleteNotificationConfiguration" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "AutoScalingGroupName" Lude.=: autoScalingGroupName,
        "TopicARN" Lude.=: topicARN
      ]

-- | /See:/ 'mkDeleteNotificationConfigurationResponse' smart constructor.
data DeleteNotificationConfigurationResponse = DeleteNotificationConfigurationResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteNotificationConfigurationResponse' with the minimum fields required to make a request.
mkDeleteNotificationConfigurationResponse ::
  DeleteNotificationConfigurationResponse
mkDeleteNotificationConfigurationResponse =
  DeleteNotificationConfigurationResponse'
