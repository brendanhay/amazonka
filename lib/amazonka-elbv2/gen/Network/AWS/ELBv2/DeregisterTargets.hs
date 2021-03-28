{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.DeregisterTargets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters the specified targets from the specified target group. After the targets are deregistered, they no longer receive traffic from the load balancer.
module Network.AWS.ELBv2.DeregisterTargets
    (
    -- * Creating a request
      DeregisterTargets (..)
    , mkDeregisterTargets
    -- ** Request lenses
    , dtTargetGroupArn
    , dtTargets

    -- * Destructuring the response
    , DeregisterTargetsResponse (..)
    , mkDeregisterTargetsResponse
    -- ** Response lenses
    , dtrfrsResponseStatus
    ) where

import qualified Network.AWS.ELBv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeregisterTargets' smart constructor.
data DeregisterTargets = DeregisterTargets'
  { targetGroupArn :: Types.TargetGroupArn
    -- ^ The Amazon Resource Name (ARN) of the target group.
  , targets :: [Types.TargetDescription]
    -- ^ The targets. If you specified a port override when you registered a target, you must specify both the target ID and the port when you deregister it.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterTargets' value with any optional fields omitted.
mkDeregisterTargets
    :: Types.TargetGroupArn -- ^ 'targetGroupArn'
    -> DeregisterTargets
mkDeregisterTargets targetGroupArn
  = DeregisterTargets'{targetGroupArn, targets = Core.mempty}

-- | The Amazon Resource Name (ARN) of the target group.
--
-- /Note:/ Consider using 'targetGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtTargetGroupArn :: Lens.Lens' DeregisterTargets Types.TargetGroupArn
dtTargetGroupArn = Lens.field @"targetGroupArn"
{-# INLINEABLE dtTargetGroupArn #-}
{-# DEPRECATED targetGroupArn "Use generic-lens or generic-optics with 'targetGroupArn' instead"  #-}

-- | The targets. If you specified a port override when you registered a target, you must specify both the target ID and the port when you deregister it.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtTargets :: Lens.Lens' DeregisterTargets [Types.TargetDescription]
dtTargets = Lens.field @"targets"
{-# INLINEABLE dtTargets #-}
{-# DEPRECATED targets "Use generic-lens or generic-optics with 'targets' instead"  #-}

instance Core.ToQuery DeregisterTargets where
        toQuery DeregisterTargets{..}
          = Core.toQueryPair "Action" ("DeregisterTargets" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "TargetGroupArn" targetGroupArn
              Core.<>
              Core.toQueryPair "Targets" (Core.toQueryList "member" targets)

instance Core.ToHeaders DeregisterTargets where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeregisterTargets where
        type Rs DeregisterTargets = DeregisterTargetsResponse
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
          = Response.receiveXMLWrapper "DeregisterTargetsResult"
              (\ s h x ->
                 DeregisterTargetsResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeregisterTargetsResponse' smart constructor.
newtype DeregisterTargetsResponse = DeregisterTargetsResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterTargetsResponse' value with any optional fields omitted.
mkDeregisterTargetsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeregisterTargetsResponse
mkDeregisterTargetsResponse responseStatus
  = DeregisterTargetsResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrfrsResponseStatus :: Lens.Lens' DeregisterTargetsResponse Core.Int
dtrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
