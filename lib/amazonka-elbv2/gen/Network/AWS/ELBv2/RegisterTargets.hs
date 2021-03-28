{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.RegisterTargets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers the specified targets with the specified target group.
--
-- If the target is an EC2 instance, it must be in the @running@ state when you register it.
-- By default, the load balancer routes requests to registered targets using the protocol and port for the target group. Alternatively, you can override the port for a target when you register it. You can register each EC2 instance or IP address with the same target group multiple times using different ports.
-- With a Network Load Balancer, you cannot register instances by instance ID if they have the following instance types: C1, CC1, CC2, CG1, CG2, CR1, CS1, G1, G2, HI1, HS1, M1, M2, M3, and T1. You can register instances of these types by IP address.
module Network.AWS.ELBv2.RegisterTargets
    (
    -- * Creating a request
      RegisterTargets (..)
    , mkRegisterTargets
    -- ** Request lenses
    , rtTargetGroupArn
    , rtTargets

    -- * Destructuring the response
    , RegisterTargetsResponse (..)
    , mkRegisterTargetsResponse
    -- ** Response lenses
    , rrsResponseStatus
    ) where

import qualified Network.AWS.ELBv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRegisterTargets' smart constructor.
data RegisterTargets = RegisterTargets'
  { targetGroupArn :: Types.TargetGroupArn
    -- ^ The Amazon Resource Name (ARN) of the target group.
  , targets :: [Types.TargetDescription]
    -- ^ The targets.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterTargets' value with any optional fields omitted.
mkRegisterTargets
    :: Types.TargetGroupArn -- ^ 'targetGroupArn'
    -> RegisterTargets
mkRegisterTargets targetGroupArn
  = RegisterTargets'{targetGroupArn, targets = Core.mempty}

-- | The Amazon Resource Name (ARN) of the target group.
--
-- /Note:/ Consider using 'targetGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtTargetGroupArn :: Lens.Lens' RegisterTargets Types.TargetGroupArn
rtTargetGroupArn = Lens.field @"targetGroupArn"
{-# INLINEABLE rtTargetGroupArn #-}
{-# DEPRECATED targetGroupArn "Use generic-lens or generic-optics with 'targetGroupArn' instead"  #-}

-- | The targets.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtTargets :: Lens.Lens' RegisterTargets [Types.TargetDescription]
rtTargets = Lens.field @"targets"
{-# INLINEABLE rtTargets #-}
{-# DEPRECATED targets "Use generic-lens or generic-optics with 'targets' instead"  #-}

instance Core.ToQuery RegisterTargets where
        toQuery RegisterTargets{..}
          = Core.toQueryPair "Action" ("RegisterTargets" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "TargetGroupArn" targetGroupArn
              Core.<>
              Core.toQueryPair "Targets" (Core.toQueryList "member" targets)

instance Core.ToHeaders RegisterTargets where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest RegisterTargets where
        type Rs RegisterTargets = RegisterTargetsResponse
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
          = Response.receiveXMLWrapper "RegisterTargetsResult"
              (\ s h x ->
                 RegisterTargetsResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRegisterTargetsResponse' smart constructor.
newtype RegisterTargetsResponse = RegisterTargetsResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterTargetsResponse' value with any optional fields omitted.
mkRegisterTargetsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RegisterTargetsResponse
mkRegisterTargetsResponse responseStatus
  = RegisterTargetsResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsResponseStatus :: Lens.Lens' RegisterTargetsResponse Core.Int
rrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
