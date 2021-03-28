{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DetachInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more instances from the specified Auto Scaling group.
--
-- After the instances are detached, you can manage them independent of the Auto Scaling group.
-- If you do not specify the option to decrement the desired capacity, Amazon EC2 Auto Scaling launches instances to replace the ones that are detached.
-- If there is a Classic Load Balancer attached to the Auto Scaling group, the instances are deregistered from the load balancer. If there are target groups attached to the Auto Scaling group, the instances are deregistered from the target groups.
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/detach-instance-asg.html Detach EC2 instances from your Auto Scaling group> in the /Amazon EC2 Auto Scaling User Guide/ .
module Network.AWS.AutoScaling.DetachInstances
    (
    -- * Creating a request
      DetachInstances (..)
    , mkDetachInstances
    -- ** Request lenses
    , diAutoScalingGroupName
    , diShouldDecrementDesiredCapacity
    , diInstanceIds

    -- * Destructuring the response
    , DetachInstancesResponse (..)
    , mkDetachInstancesResponse
    -- ** Response lenses
    , dirrsActivities
    , dirrsResponseStatus
    ) where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDetachInstances' smart constructor.
data DetachInstances = DetachInstances'
  { autoScalingGroupName :: Types.AutoScalingGroupName
    -- ^ The name of the Auto Scaling group.
  , shouldDecrementDesiredCapacity :: Core.Bool
    -- ^ Indicates whether the Auto Scaling group decrements the desired capacity value by the number of instances detached.
  , instanceIds :: Core.Maybe [Types.XmlStringMaxLen19]
    -- ^ The IDs of the instances. You can specify up to 20 instances.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetachInstances' value with any optional fields omitted.
mkDetachInstances
    :: Types.AutoScalingGroupName -- ^ 'autoScalingGroupName'
    -> Core.Bool -- ^ 'shouldDecrementDesiredCapacity'
    -> DetachInstances
mkDetachInstances autoScalingGroupName
  shouldDecrementDesiredCapacity
  = DetachInstances'{autoScalingGroupName,
                     shouldDecrementDesiredCapacity, instanceIds = Core.Nothing}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diAutoScalingGroupName :: Lens.Lens' DetachInstances Types.AutoScalingGroupName
diAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# INLINEABLE diAutoScalingGroupName #-}
{-# DEPRECATED autoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead"  #-}

-- | Indicates whether the Auto Scaling group decrements the desired capacity value by the number of instances detached.
--
-- /Note:/ Consider using 'shouldDecrementDesiredCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diShouldDecrementDesiredCapacity :: Lens.Lens' DetachInstances Core.Bool
diShouldDecrementDesiredCapacity = Lens.field @"shouldDecrementDesiredCapacity"
{-# INLINEABLE diShouldDecrementDesiredCapacity #-}
{-# DEPRECATED shouldDecrementDesiredCapacity "Use generic-lens or generic-optics with 'shouldDecrementDesiredCapacity' instead"  #-}

-- | The IDs of the instances. You can specify up to 20 instances.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diInstanceIds :: Lens.Lens' DetachInstances (Core.Maybe [Types.XmlStringMaxLen19])
diInstanceIds = Lens.field @"instanceIds"
{-# INLINEABLE diInstanceIds #-}
{-# DEPRECATED instanceIds "Use generic-lens or generic-optics with 'instanceIds' instead"  #-}

instance Core.ToQuery DetachInstances where
        toQuery DetachInstances{..}
          = Core.toQueryPair "Action" ("DetachInstances" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2011-01-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "AutoScalingGroupName" autoScalingGroupName
              Core.<>
              Core.toQueryPair "ShouldDecrementDesiredCapacity"
                shouldDecrementDesiredCapacity
              Core.<>
              Core.toQueryPair "InstanceIds"
                (Core.maybe Core.mempty (Core.toQueryList "member") instanceIds)

instance Core.ToHeaders DetachInstances where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DetachInstances where
        type Rs DetachInstances = DetachInstancesResponse
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
          = Response.receiveXMLWrapper "DetachInstancesResult"
              (\ s h x ->
                 DetachInstancesResponse' Core.<$>
                   (x Core..@? "Activities" Core..<@> Core.parseXMLList "member")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDetachInstancesResponse' smart constructor.
data DetachInstancesResponse = DetachInstancesResponse'
  { activities :: Core.Maybe [Types.Activity]
    -- ^ The activities related to detaching the instances from the Auto Scaling group.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DetachInstancesResponse' value with any optional fields omitted.
mkDetachInstancesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DetachInstancesResponse
mkDetachInstancesResponse responseStatus
  = DetachInstancesResponse'{activities = Core.Nothing,
                             responseStatus}

-- | The activities related to detaching the instances from the Auto Scaling group.
--
-- /Note:/ Consider using 'activities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrsActivities :: Lens.Lens' DetachInstancesResponse (Core.Maybe [Types.Activity])
dirrsActivities = Lens.field @"activities"
{-# INLINEABLE dirrsActivities #-}
{-# DEPRECATED activities "Use generic-lens or generic-optics with 'activities' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrsResponseStatus :: Lens.Lens' DetachInstancesResponse Core.Int
dirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
