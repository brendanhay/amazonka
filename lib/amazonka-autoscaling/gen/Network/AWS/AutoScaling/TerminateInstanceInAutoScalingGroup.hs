{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.TerminateInstanceInAutoScalingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Terminates the specified instance and optionally adjusts the desired group size. 
--
-- This call simply makes a termination request. The instance is not terminated immediately. When an instance is terminated, the instance status changes to @terminated@ . You can't connect to or start an instance after you've terminated it.
-- If you do not specify the option to decrement the desired capacity, Amazon EC2 Auto Scaling launches instances to replace the ones that are terminated. 
-- By default, Amazon EC2 Auto Scaling balances instances across all Availability Zones. If you decrement the desired capacity, your Auto Scaling group can become unbalanced between Availability Zones. Amazon EC2 Auto Scaling tries to rebalance the group, and rebalancing might terminate instances in other zones. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/auto-scaling-benefits.html#AutoScalingBehavior.InstanceUsage Rebalancing activities> in the /Amazon EC2 Auto Scaling User Guide/ .
module Network.AWS.AutoScaling.TerminateInstanceInAutoScalingGroup
    (
    -- * Creating a request
      TerminateInstanceInAutoScalingGroup (..)
    , mkTerminateInstanceInAutoScalingGroup
    -- ** Request lenses
    , tiiasgInstanceId
    , tiiasgShouldDecrementDesiredCapacity

    -- * Destructuring the response
    , TerminateInstanceInAutoScalingGroupResponse (..)
    , mkTerminateInstanceInAutoScalingGroupResponse
    -- ** Response lenses
    , tiiasgrrsActivity
    , tiiasgrrsResponseStatus
    ) where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkTerminateInstanceInAutoScalingGroup' smart constructor.
data TerminateInstanceInAutoScalingGroup = TerminateInstanceInAutoScalingGroup'
  { instanceId :: Types.InstanceId
    -- ^ The ID of the instance.
  , shouldDecrementDesiredCapacity :: Core.Bool
    -- ^ Indicates whether terminating the instance also decrements the size of the Auto Scaling group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TerminateInstanceInAutoScalingGroup' value with any optional fields omitted.
mkTerminateInstanceInAutoScalingGroup
    :: Types.InstanceId -- ^ 'instanceId'
    -> Core.Bool -- ^ 'shouldDecrementDesiredCapacity'
    -> TerminateInstanceInAutoScalingGroup
mkTerminateInstanceInAutoScalingGroup instanceId
  shouldDecrementDesiredCapacity
  = TerminateInstanceInAutoScalingGroup'{instanceId,
                                         shouldDecrementDesiredCapacity}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiiasgInstanceId :: Lens.Lens' TerminateInstanceInAutoScalingGroup Types.InstanceId
tiiasgInstanceId = Lens.field @"instanceId"
{-# INLINEABLE tiiasgInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | Indicates whether terminating the instance also decrements the size of the Auto Scaling group.
--
-- /Note:/ Consider using 'shouldDecrementDesiredCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiiasgShouldDecrementDesiredCapacity :: Lens.Lens' TerminateInstanceInAutoScalingGroup Core.Bool
tiiasgShouldDecrementDesiredCapacity = Lens.field @"shouldDecrementDesiredCapacity"
{-# INLINEABLE tiiasgShouldDecrementDesiredCapacity #-}
{-# DEPRECATED shouldDecrementDesiredCapacity "Use generic-lens or generic-optics with 'shouldDecrementDesiredCapacity' instead"  #-}

instance Core.ToQuery TerminateInstanceInAutoScalingGroup where
        toQuery TerminateInstanceInAutoScalingGroup{..}
          = Core.toQueryPair "Action"
              ("TerminateInstanceInAutoScalingGroup" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2011-01-01" :: Core.Text)
              Core.<> Core.toQueryPair "InstanceId" instanceId
              Core.<>
              Core.toQueryPair "ShouldDecrementDesiredCapacity"
                shouldDecrementDesiredCapacity

instance Core.ToHeaders TerminateInstanceInAutoScalingGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest TerminateInstanceInAutoScalingGroup where
        type Rs TerminateInstanceInAutoScalingGroup =
             TerminateInstanceInAutoScalingGroupResponse
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
          = Response.receiveXMLWrapper
              "TerminateInstanceInAutoScalingGroupResult"
              (\ s h x ->
                 TerminateInstanceInAutoScalingGroupResponse' Core.<$>
                   (x Core..@? "Activity") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkTerminateInstanceInAutoScalingGroupResponse' smart constructor.
data TerminateInstanceInAutoScalingGroupResponse = TerminateInstanceInAutoScalingGroupResponse'
  { activity :: Core.Maybe Types.Activity
    -- ^ A scaling activity.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'TerminateInstanceInAutoScalingGroupResponse' value with any optional fields omitted.
mkTerminateInstanceInAutoScalingGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> TerminateInstanceInAutoScalingGroupResponse
mkTerminateInstanceInAutoScalingGroupResponse responseStatus
  = TerminateInstanceInAutoScalingGroupResponse'{activity =
                                                   Core.Nothing,
                                                 responseStatus}

-- | A scaling activity.
--
-- /Note:/ Consider using 'activity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiiasgrrsActivity :: Lens.Lens' TerminateInstanceInAutoScalingGroupResponse (Core.Maybe Types.Activity)
tiiasgrrsActivity = Lens.field @"activity"
{-# INLINEABLE tiiasgrrsActivity #-}
{-# DEPRECATED activity "Use generic-lens or generic-optics with 'activity' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiiasgrrsResponseStatus :: Lens.Lens' TerminateInstanceInAutoScalingGroupResponse Core.Int
tiiasgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE tiiasgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
