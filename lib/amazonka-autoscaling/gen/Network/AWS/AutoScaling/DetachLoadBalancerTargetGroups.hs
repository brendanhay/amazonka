{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DetachLoadBalancerTargetGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches one or more target groups from the specified Auto Scaling group.
module Network.AWS.AutoScaling.DetachLoadBalancerTargetGroups
    (
    -- * Creating a request
      DetachLoadBalancerTargetGroups (..)
    , mkDetachLoadBalancerTargetGroups
    -- ** Request lenses
    , dlbtgAutoScalingGroupName
    , dlbtgTargetGroupARNs

    -- * Destructuring the response
    , DetachLoadBalancerTargetGroupsResponse (..)
    , mkDetachLoadBalancerTargetGroupsResponse
    -- ** Response lenses
    , drsResponseStatus
    ) where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDetachLoadBalancerTargetGroups' smart constructor.
data DetachLoadBalancerTargetGroups = DetachLoadBalancerTargetGroups'
  { autoScalingGroupName :: Types.ResourceName
    -- ^ The name of the Auto Scaling group.
  , targetGroupARNs :: [Types.XmlStringMaxLen511]
    -- ^ The Amazon Resource Names (ARN) of the target groups. You can specify up to 10 target groups.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetachLoadBalancerTargetGroups' value with any optional fields omitted.
mkDetachLoadBalancerTargetGroups
    :: Types.ResourceName -- ^ 'autoScalingGroupName'
    -> DetachLoadBalancerTargetGroups
mkDetachLoadBalancerTargetGroups autoScalingGroupName
  = DetachLoadBalancerTargetGroups'{autoScalingGroupName,
                                    targetGroupARNs = Core.mempty}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbtgAutoScalingGroupName :: Lens.Lens' DetachLoadBalancerTargetGroups Types.ResourceName
dlbtgAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# INLINEABLE dlbtgAutoScalingGroupName #-}
{-# DEPRECATED autoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead"  #-}

-- | The Amazon Resource Names (ARN) of the target groups. You can specify up to 10 target groups.
--
-- /Note:/ Consider using 'targetGroupARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbtgTargetGroupARNs :: Lens.Lens' DetachLoadBalancerTargetGroups [Types.XmlStringMaxLen511]
dlbtgTargetGroupARNs = Lens.field @"targetGroupARNs"
{-# INLINEABLE dlbtgTargetGroupARNs #-}
{-# DEPRECATED targetGroupARNs "Use generic-lens or generic-optics with 'targetGroupARNs' instead"  #-}

instance Core.ToQuery DetachLoadBalancerTargetGroups where
        toQuery DetachLoadBalancerTargetGroups{..}
          = Core.toQueryPair "Action"
              ("DetachLoadBalancerTargetGroups" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2011-01-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "AutoScalingGroupName" autoScalingGroupName
              Core.<>
              Core.toQueryPair "TargetGroupARNs"
                (Core.toQueryList "member" targetGroupARNs)

instance Core.ToHeaders DetachLoadBalancerTargetGroups where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DetachLoadBalancerTargetGroups where
        type Rs DetachLoadBalancerTargetGroups =
             DetachLoadBalancerTargetGroupsResponse
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
          = Response.receiveXMLWrapper "DetachLoadBalancerTargetGroupsResult"
              (\ s h x ->
                 DetachLoadBalancerTargetGroupsResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDetachLoadBalancerTargetGroupsResponse' smart constructor.
newtype DetachLoadBalancerTargetGroupsResponse = DetachLoadBalancerTargetGroupsResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DetachLoadBalancerTargetGroupsResponse' value with any optional fields omitted.
mkDetachLoadBalancerTargetGroupsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DetachLoadBalancerTargetGroupsResponse
mkDetachLoadBalancerTargetGroupsResponse responseStatus
  = DetachLoadBalancerTargetGroupsResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DetachLoadBalancerTargetGroupsResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
