{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.PutNotificationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures an Auto Scaling group to send notifications when specified events take place. Subscribers to the specified topic can have messages delivered to an endpoint such as a web server or an email address.
--
-- This configuration overwrites any existing configuration.
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ASGettingNotifications.html Getting Amazon SNS notifications when your Auto Scaling group scales> in the /Amazon EC2 Auto Scaling User Guide/ .
-- If you exceed your maximum limit of SNS topics, which is 10 per Auto Scaling group, the call fails.
module Network.AWS.AutoScaling.PutNotificationConfiguration
  ( -- * Creating a request
    PutNotificationConfiguration (..),
    mkPutNotificationConfiguration,

    -- ** Request lenses
    pncAutoScalingGroupName,
    pncTopicARN,
    pncNotificationTypes,

    -- * Destructuring the response
    PutNotificationConfigurationResponse (..),
    mkPutNotificationConfigurationResponse,
  )
where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutNotificationConfiguration' smart constructor.
data PutNotificationConfiguration = PutNotificationConfiguration'
  { -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Types.ResourceName,
    -- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service (Amazon SNS) topic.
    topicARN :: Types.ResourceName,
    -- | The type of event that causes the notification to be sent. To query the notification types supported by Amazon EC2 Auto Scaling, call the 'DescribeAutoScalingNotificationTypes' API.
    notificationTypes :: [Types.XmlStringMaxLen255]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutNotificationConfiguration' value with any optional fields omitted.
mkPutNotificationConfiguration ::
  -- | 'autoScalingGroupName'
  Types.ResourceName ->
  -- | 'topicARN'
  Types.ResourceName ->
  PutNotificationConfiguration
mkPutNotificationConfiguration autoScalingGroupName topicARN =
  PutNotificationConfiguration'
    { autoScalingGroupName,
      topicARN,
      notificationTypes = Core.mempty
    }

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pncAutoScalingGroupName :: Lens.Lens' PutNotificationConfiguration Types.ResourceName
pncAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# DEPRECATED pncAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service (Amazon SNS) topic.
--
-- /Note:/ Consider using 'topicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pncTopicARN :: Lens.Lens' PutNotificationConfiguration Types.ResourceName
pncTopicARN = Lens.field @"topicARN"
{-# DEPRECATED pncTopicARN "Use generic-lens or generic-optics with 'topicARN' instead." #-}

-- | The type of event that causes the notification to be sent. To query the notification types supported by Amazon EC2 Auto Scaling, call the 'DescribeAutoScalingNotificationTypes' API.
--
-- /Note:/ Consider using 'notificationTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pncNotificationTypes :: Lens.Lens' PutNotificationConfiguration [Types.XmlStringMaxLen255]
pncNotificationTypes = Lens.field @"notificationTypes"
{-# DEPRECATED pncNotificationTypes "Use generic-lens or generic-optics with 'notificationTypes' instead." #-}

instance Core.AWSRequest PutNotificationConfiguration where
  type
    Rs PutNotificationConfiguration =
      PutNotificationConfigurationResponse
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
            ( Core.pure ("Action", "PutNotificationConfiguration")
                Core.<> (Core.pure ("Version", "2011-01-01"))
                Core.<> (Core.toQueryValue "AutoScalingGroupName" autoScalingGroupName)
                Core.<> (Core.toQueryValue "TopicARN" topicARN)
                Core.<> ( Core.toQueryValue
                            "NotificationTypes"
                            (Core.toQueryList "member" notificationTypes)
                        )
            )
      }
  response =
    Response.receiveNull PutNotificationConfigurationResponse'

-- | /See:/ 'mkPutNotificationConfigurationResponse' smart constructor.
data PutNotificationConfigurationResponse = PutNotificationConfigurationResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutNotificationConfigurationResponse' value with any optional fields omitted.
mkPutNotificationConfigurationResponse ::
  PutNotificationConfigurationResponse
mkPutNotificationConfigurationResponse =
  PutNotificationConfigurationResponse'
