{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      PutNotificationConfiguration (..)
    , mkPutNotificationConfiguration
    -- ** Request lenses
    , pncAutoScalingGroupName
    , pncTopicARN
    , pncNotificationTypes

    -- * Destructuring the response
    , PutNotificationConfigurationResponse (..)
    , mkPutNotificationConfigurationResponse
    ) where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutNotificationConfiguration' smart constructor.
data PutNotificationConfiguration = PutNotificationConfiguration'
  { autoScalingGroupName :: Types.ResourceName
    -- ^ The name of the Auto Scaling group.
  , topicARN :: Types.ResourceName
    -- ^ The Amazon Resource Name (ARN) of the Amazon Simple Notification Service (Amazon SNS) topic.
  , notificationTypes :: [Types.XmlStringMaxLen255]
    -- ^ The type of event that causes the notification to be sent. To query the notification types supported by Amazon EC2 Auto Scaling, call the 'DescribeAutoScalingNotificationTypes' API.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutNotificationConfiguration' value with any optional fields omitted.
mkPutNotificationConfiguration
    :: Types.ResourceName -- ^ 'autoScalingGroupName'
    -> Types.ResourceName -- ^ 'topicARN'
    -> PutNotificationConfiguration
mkPutNotificationConfiguration autoScalingGroupName topicARN
  = PutNotificationConfiguration'{autoScalingGroupName, topicARN,
                                  notificationTypes = Core.mempty}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pncAutoScalingGroupName :: Lens.Lens' PutNotificationConfiguration Types.ResourceName
pncAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# INLINEABLE pncAutoScalingGroupName #-}
{-# DEPRECATED autoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead"  #-}

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service (Amazon SNS) topic.
--
-- /Note:/ Consider using 'topicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pncTopicARN :: Lens.Lens' PutNotificationConfiguration Types.ResourceName
pncTopicARN = Lens.field @"topicARN"
{-# INLINEABLE pncTopicARN #-}
{-# DEPRECATED topicARN "Use generic-lens or generic-optics with 'topicARN' instead"  #-}

-- | The type of event that causes the notification to be sent. To query the notification types supported by Amazon EC2 Auto Scaling, call the 'DescribeAutoScalingNotificationTypes' API.
--
-- /Note:/ Consider using 'notificationTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pncNotificationTypes :: Lens.Lens' PutNotificationConfiguration [Types.XmlStringMaxLen255]
pncNotificationTypes = Lens.field @"notificationTypes"
{-# INLINEABLE pncNotificationTypes #-}
{-# DEPRECATED notificationTypes "Use generic-lens or generic-optics with 'notificationTypes' instead"  #-}

instance Core.ToQuery PutNotificationConfiguration where
        toQuery PutNotificationConfiguration{..}
          = Core.toQueryPair "Action"
              ("PutNotificationConfiguration" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2011-01-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "AutoScalingGroupName" autoScalingGroupName
              Core.<> Core.toQueryPair "TopicARN" topicARN
              Core.<>
              Core.toQueryPair "NotificationTypes"
                (Core.toQueryList "member" notificationTypes)

instance Core.ToHeaders PutNotificationConfiguration where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest PutNotificationConfiguration where
        type Rs PutNotificationConfiguration =
             PutNotificationConfigurationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull PutNotificationConfigurationResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutNotificationConfigurationResponse' smart constructor.
data PutNotificationConfigurationResponse = PutNotificationConfigurationResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutNotificationConfigurationResponse' value with any optional fields omitted.
mkPutNotificationConfigurationResponse
    :: PutNotificationConfigurationResponse
mkPutNotificationConfigurationResponse
  = PutNotificationConfigurationResponse'
