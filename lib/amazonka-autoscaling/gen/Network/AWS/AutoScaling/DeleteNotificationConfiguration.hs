{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteNotificationConfiguration' smart constructor.
data DeleteNotificationConfiguration = DeleteNotificationConfiguration'
  { -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Types.ResourceName,
    -- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service (Amazon SNS) topic.
    topicARN :: Types.ResourceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteNotificationConfiguration' value with any optional fields omitted.
mkDeleteNotificationConfiguration ::
  -- | 'autoScalingGroupName'
  Types.ResourceName ->
  -- | 'topicARN'
  Types.ResourceName ->
  DeleteNotificationConfiguration
mkDeleteNotificationConfiguration autoScalingGroupName topicARN =
  DeleteNotificationConfiguration' {autoScalingGroupName, topicARN}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncAutoScalingGroupName :: Lens.Lens' DeleteNotificationConfiguration Types.ResourceName
dncAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# DEPRECATED dncAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service (Amazon SNS) topic.
--
-- /Note:/ Consider using 'topicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncTopicARN :: Lens.Lens' DeleteNotificationConfiguration Types.ResourceName
dncTopicARN = Lens.field @"topicARN"
{-# DEPRECATED dncTopicARN "Use generic-lens or generic-optics with 'topicARN' instead." #-}

instance Core.AWSRequest DeleteNotificationConfiguration where
  type
    Rs DeleteNotificationConfiguration =
      DeleteNotificationConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DeleteNotificationConfiguration")
                Core.<> (Core.pure ("Version", "2011-01-01"))
                Core.<> (Core.toQueryValue "AutoScalingGroupName" autoScalingGroupName)
                Core.<> (Core.toQueryValue "TopicARN" topicARN)
            )
      }
  response =
    Response.receiveNull DeleteNotificationConfigurationResponse'

-- | /See:/ 'mkDeleteNotificationConfigurationResponse' smart constructor.
data DeleteNotificationConfigurationResponse = DeleteNotificationConfigurationResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteNotificationConfigurationResponse' value with any optional fields omitted.
mkDeleteNotificationConfigurationResponse ::
  DeleteNotificationConfigurationResponse
mkDeleteNotificationConfigurationResponse =
  DeleteNotificationConfigurationResponse'
