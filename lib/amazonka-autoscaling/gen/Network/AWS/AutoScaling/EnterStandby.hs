{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.EnterStandby
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Moves the specified instances into the standby state.
--
-- If you choose to decrement the desired capacity of the Auto Scaling group, the instances can enter standby as long as the desired capacity of the Auto Scaling group after the instances are placed into standby is equal to or greater than the minimum capacity of the group.
-- If you choose not to decrement the desired capacity of the Auto Scaling group, the Auto Scaling group launches new instances to replace the instances on standby.
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-enter-exit-standby.html Temporarily removing instances from your Auto Scaling group> in the /Amazon EC2 Auto Scaling User Guide/ .
module Network.AWS.AutoScaling.EnterStandby
    (
    -- * Creating a request
      EnterStandby (..)
    , mkEnterStandby
    -- ** Request lenses
    , esAutoScalingGroupName
    , esShouldDecrementDesiredCapacity
    , esInstanceIds

    -- * Destructuring the response
    , EnterStandbyResponse (..)
    , mkEnterStandbyResponse
    -- ** Response lenses
    , esrrsActivities
    , esrrsResponseStatus
    ) where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkEnterStandby' smart constructor.
data EnterStandby = EnterStandby'
  { autoScalingGroupName :: Types.AutoScalingGroupName
    -- ^ The name of the Auto Scaling group.
  , shouldDecrementDesiredCapacity :: Core.Bool
    -- ^ Indicates whether to decrement the desired capacity of the Auto Scaling group by the number of instances moved to @Standby@ mode.
  , instanceIds :: Core.Maybe [Types.XmlStringMaxLen19]
    -- ^ The IDs of the instances. You can specify up to 20 instances.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnterStandby' value with any optional fields omitted.
mkEnterStandby
    :: Types.AutoScalingGroupName -- ^ 'autoScalingGroupName'
    -> Core.Bool -- ^ 'shouldDecrementDesiredCapacity'
    -> EnterStandby
mkEnterStandby autoScalingGroupName shouldDecrementDesiredCapacity
  = EnterStandby'{autoScalingGroupName,
                  shouldDecrementDesiredCapacity, instanceIds = Core.Nothing}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esAutoScalingGroupName :: Lens.Lens' EnterStandby Types.AutoScalingGroupName
esAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# INLINEABLE esAutoScalingGroupName #-}
{-# DEPRECATED autoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead"  #-}

-- | Indicates whether to decrement the desired capacity of the Auto Scaling group by the number of instances moved to @Standby@ mode.
--
-- /Note:/ Consider using 'shouldDecrementDesiredCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esShouldDecrementDesiredCapacity :: Lens.Lens' EnterStandby Core.Bool
esShouldDecrementDesiredCapacity = Lens.field @"shouldDecrementDesiredCapacity"
{-# INLINEABLE esShouldDecrementDesiredCapacity #-}
{-# DEPRECATED shouldDecrementDesiredCapacity "Use generic-lens or generic-optics with 'shouldDecrementDesiredCapacity' instead"  #-}

-- | The IDs of the instances. You can specify up to 20 instances.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esInstanceIds :: Lens.Lens' EnterStandby (Core.Maybe [Types.XmlStringMaxLen19])
esInstanceIds = Lens.field @"instanceIds"
{-# INLINEABLE esInstanceIds #-}
{-# DEPRECATED instanceIds "Use generic-lens or generic-optics with 'instanceIds' instead"  #-}

instance Core.ToQuery EnterStandby where
        toQuery EnterStandby{..}
          = Core.toQueryPair "Action" ("EnterStandby" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2011-01-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "AutoScalingGroupName" autoScalingGroupName
              Core.<>
              Core.toQueryPair "ShouldDecrementDesiredCapacity"
                shouldDecrementDesiredCapacity
              Core.<>
              Core.toQueryPair "InstanceIds"
                (Core.maybe Core.mempty (Core.toQueryList "member") instanceIds)

instance Core.ToHeaders EnterStandby where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest EnterStandby where
        type Rs EnterStandby = EnterStandbyResponse
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
          = Response.receiveXMLWrapper "EnterStandbyResult"
              (\ s h x ->
                 EnterStandbyResponse' Core.<$>
                   (x Core..@? "Activities" Core..<@> Core.parseXMLList "member")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkEnterStandbyResponse' smart constructor.
data EnterStandbyResponse = EnterStandbyResponse'
  { activities :: Core.Maybe [Types.Activity]
    -- ^ The activities related to moving instances into @Standby@ mode.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'EnterStandbyResponse' value with any optional fields omitted.
mkEnterStandbyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> EnterStandbyResponse
mkEnterStandbyResponse responseStatus
  = EnterStandbyResponse'{activities = Core.Nothing, responseStatus}

-- | The activities related to moving instances into @Standby@ mode.
--
-- /Note:/ Consider using 'activities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esrrsActivities :: Lens.Lens' EnterStandbyResponse (Core.Maybe [Types.Activity])
esrrsActivities = Lens.field @"activities"
{-# INLINEABLE esrrsActivities #-}
{-# DEPRECATED activities "Use generic-lens or generic-optics with 'activities' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esrrsResponseStatus :: Lens.Lens' EnterStandbyResponse Core.Int
esrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE esrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
