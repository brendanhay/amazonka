{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DetachLoadBalancerTargetGroups (..),
    mkDetachLoadBalancerTargetGroups,

    -- ** Request lenses
    dlbtgAutoScalingGroupName,
    dlbtgTargetGroupARNs,

    -- * Destructuring the response
    DetachLoadBalancerTargetGroupsResponse (..),
    mkDetachLoadBalancerTargetGroupsResponse,

    -- ** Response lenses
    drsResponseStatus,
  )
where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDetachLoadBalancerTargetGroups' smart constructor.
data DetachLoadBalancerTargetGroups = DetachLoadBalancerTargetGroups'
  { -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Types.ResourceName,
    -- | The Amazon Resource Names (ARN) of the target groups. You can specify up to 10 target groups.
    targetGroupARNs :: [Types.XmlStringMaxLen511]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetachLoadBalancerTargetGroups' value with any optional fields omitted.
mkDetachLoadBalancerTargetGroups ::
  -- | 'autoScalingGroupName'
  Types.ResourceName ->
  DetachLoadBalancerTargetGroups
mkDetachLoadBalancerTargetGroups autoScalingGroupName =
  DetachLoadBalancerTargetGroups'
    { autoScalingGroupName,
      targetGroupARNs = Core.mempty
    }

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbtgAutoScalingGroupName :: Lens.Lens' DetachLoadBalancerTargetGroups Types.ResourceName
dlbtgAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# DEPRECATED dlbtgAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | The Amazon Resource Names (ARN) of the target groups. You can specify up to 10 target groups.
--
-- /Note:/ Consider using 'targetGroupARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbtgTargetGroupARNs :: Lens.Lens' DetachLoadBalancerTargetGroups [Types.XmlStringMaxLen511]
dlbtgTargetGroupARNs = Lens.field @"targetGroupARNs"
{-# DEPRECATED dlbtgTargetGroupARNs "Use generic-lens or generic-optics with 'targetGroupARNs' instead." #-}

instance Core.AWSRequest DetachLoadBalancerTargetGroups where
  type
    Rs DetachLoadBalancerTargetGroups =
      DetachLoadBalancerTargetGroupsResponse
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
            ( Core.pure ("Action", "DetachLoadBalancerTargetGroups")
                Core.<> (Core.pure ("Version", "2011-01-01"))
                Core.<> (Core.toQueryValue "AutoScalingGroupName" autoScalingGroupName)
                Core.<> ( Core.toQueryValue
                            "TargetGroupARNs"
                            (Core.toQueryList "member" targetGroupARNs)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "DetachLoadBalancerTargetGroupsResult"
      ( \s h x ->
          DetachLoadBalancerTargetGroupsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDetachLoadBalancerTargetGroupsResponse' smart constructor.
newtype DetachLoadBalancerTargetGroupsResponse = DetachLoadBalancerTargetGroupsResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DetachLoadBalancerTargetGroupsResponse' value with any optional fields omitted.
mkDetachLoadBalancerTargetGroupsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DetachLoadBalancerTargetGroupsResponse
mkDetachLoadBalancerTargetGroupsResponse responseStatus =
  DetachLoadBalancerTargetGroupsResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DetachLoadBalancerTargetGroupsResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
