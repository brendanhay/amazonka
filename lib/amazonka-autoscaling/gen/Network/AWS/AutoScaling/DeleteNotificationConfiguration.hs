{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DeleteNotificationConfiguration (..)
    , mkDeleteNotificationConfiguration
    -- ** Request lenses
    , dncAutoScalingGroupName
    , dncTopicARN

    -- * Destructuring the response
    , DeleteNotificationConfigurationResponse (..)
    , mkDeleteNotificationConfigurationResponse
    ) where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteNotificationConfiguration' smart constructor.
data DeleteNotificationConfiguration = DeleteNotificationConfiguration'
  { autoScalingGroupName :: Types.ResourceName
    -- ^ The name of the Auto Scaling group.
  , topicARN :: Types.ResourceName
    -- ^ The Amazon Resource Name (ARN) of the Amazon Simple Notification Service (Amazon SNS) topic.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteNotificationConfiguration' value with any optional fields omitted.
mkDeleteNotificationConfiguration
    :: Types.ResourceName -- ^ 'autoScalingGroupName'
    -> Types.ResourceName -- ^ 'topicARN'
    -> DeleteNotificationConfiguration
mkDeleteNotificationConfiguration autoScalingGroupName topicARN
  = DeleteNotificationConfiguration'{autoScalingGroupName, topicARN}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncAutoScalingGroupName :: Lens.Lens' DeleteNotificationConfiguration Types.ResourceName
dncAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# INLINEABLE dncAutoScalingGroupName #-}
{-# DEPRECATED autoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead"  #-}

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service (Amazon SNS) topic.
--
-- /Note:/ Consider using 'topicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncTopicARN :: Lens.Lens' DeleteNotificationConfiguration Types.ResourceName
dncTopicARN = Lens.field @"topicARN"
{-# INLINEABLE dncTopicARN #-}
{-# DEPRECATED topicARN "Use generic-lens or generic-optics with 'topicARN' instead"  #-}

instance Core.ToQuery DeleteNotificationConfiguration where
        toQuery DeleteNotificationConfiguration{..}
          = Core.toQueryPair "Action"
              ("DeleteNotificationConfiguration" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2011-01-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "AutoScalingGroupName" autoScalingGroupName
              Core.<> Core.toQueryPair "TopicARN" topicARN

instance Core.ToHeaders DeleteNotificationConfiguration where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteNotificationConfiguration where
        type Rs DeleteNotificationConfiguration =
             DeleteNotificationConfigurationResponse
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
          = Response.receiveNull DeleteNotificationConfigurationResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteNotificationConfigurationResponse' smart constructor.
data DeleteNotificationConfigurationResponse = DeleteNotificationConfigurationResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteNotificationConfigurationResponse' value with any optional fields omitted.
mkDeleteNotificationConfigurationResponse
    :: DeleteNotificationConfigurationResponse
mkDeleteNotificationConfigurationResponse
  = DeleteNotificationConfigurationResponse'
