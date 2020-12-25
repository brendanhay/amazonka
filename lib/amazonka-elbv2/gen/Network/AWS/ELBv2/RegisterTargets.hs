{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    RegisterTargets (..),
    mkRegisterTargets,

    -- ** Request lenses
    rtTargetGroupArn,
    rtTargets,

    -- * Destructuring the response
    RegisterTargetsResponse (..),
    mkRegisterTargetsResponse,

    -- ** Response lenses
    rrsResponseStatus,
  )
where

import qualified Network.AWS.ELBv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRegisterTargets' smart constructor.
data RegisterTargets = RegisterTargets'
  { -- | The Amazon Resource Name (ARN) of the target group.
    targetGroupArn :: Types.TargetGroupArn,
    -- | The targets.
    targets :: [Types.TargetDescription]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterTargets' value with any optional fields omitted.
mkRegisterTargets ::
  -- | 'targetGroupArn'
  Types.TargetGroupArn ->
  RegisterTargets
mkRegisterTargets targetGroupArn =
  RegisterTargets' {targetGroupArn, targets = Core.mempty}

-- | The Amazon Resource Name (ARN) of the target group.
--
-- /Note:/ Consider using 'targetGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtTargetGroupArn :: Lens.Lens' RegisterTargets Types.TargetGroupArn
rtTargetGroupArn = Lens.field @"targetGroupArn"
{-# DEPRECATED rtTargetGroupArn "Use generic-lens or generic-optics with 'targetGroupArn' instead." #-}

-- | The targets.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtTargets :: Lens.Lens' RegisterTargets [Types.TargetDescription]
rtTargets = Lens.field @"targets"
{-# DEPRECATED rtTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

instance Core.AWSRequest RegisterTargets where
  type Rs RegisterTargets = RegisterTargetsResponse
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
            ( Core.pure ("Action", "RegisterTargets")
                Core.<> (Core.pure ("Version", "2015-12-01"))
                Core.<> (Core.toQueryValue "TargetGroupArn" targetGroupArn)
                Core.<> (Core.toQueryValue "Targets" (Core.toQueryList "member" targets))
            )
      }
  response =
    Response.receiveXMLWrapper
      "RegisterTargetsResult"
      ( \s h x ->
          RegisterTargetsResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRegisterTargetsResponse' smart constructor.
newtype RegisterTargetsResponse = RegisterTargetsResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterTargetsResponse' value with any optional fields omitted.
mkRegisterTargetsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RegisterTargetsResponse
mkRegisterTargetsResponse responseStatus =
  RegisterTargetsResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsResponseStatus :: Lens.Lens' RegisterTargetsResponse Core.Int
rrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
