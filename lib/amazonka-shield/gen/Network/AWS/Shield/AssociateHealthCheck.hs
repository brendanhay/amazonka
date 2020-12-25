{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.AssociateHealthCheck
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds health-based detection to the Shield Advanced protection for a resource. Shield Advanced health-based detection uses the health of your AWS resource to improve responsiveness and accuracy in attack detection and mitigation.
--
-- You define the health check in Route 53 and then associate it with your Shield Advanced protection. For more information, see <https://docs.aws.amazon.com/waf/latest/developerguide/ddos-overview.html#ddos-advanced-health-check-option Shield Advanced Health-Based Detection> in the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF and AWS Shield Developer Guide> .
module Network.AWS.Shield.AssociateHealthCheck
  ( -- * Creating a request
    AssociateHealthCheck (..),
    mkAssociateHealthCheck,

    -- ** Request lenses
    ahcProtectionId,
    ahcHealthCheckArn,

    -- * Destructuring the response
    AssociateHealthCheckResponse (..),
    mkAssociateHealthCheckResponse,

    -- ** Response lenses
    ahcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Shield.Types as Types

-- | /See:/ 'mkAssociateHealthCheck' smart constructor.
data AssociateHealthCheck = AssociateHealthCheck'
  { -- | The unique identifier (ID) for the 'Protection' object to add the health check association to.
    protectionId :: Types.ProtectionId,
    -- | The Amazon Resource Name (ARN) of the health check to associate with the protection.
    healthCheckArn :: Types.HealthCheckArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateHealthCheck' value with any optional fields omitted.
mkAssociateHealthCheck ::
  -- | 'protectionId'
  Types.ProtectionId ->
  -- | 'healthCheckArn'
  Types.HealthCheckArn ->
  AssociateHealthCheck
mkAssociateHealthCheck protectionId healthCheckArn =
  AssociateHealthCheck' {protectionId, healthCheckArn}

-- | The unique identifier (ID) for the 'Protection' object to add the health check association to.
--
-- /Note:/ Consider using 'protectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahcProtectionId :: Lens.Lens' AssociateHealthCheck Types.ProtectionId
ahcProtectionId = Lens.field @"protectionId"
{-# DEPRECATED ahcProtectionId "Use generic-lens or generic-optics with 'protectionId' instead." #-}

-- | The Amazon Resource Name (ARN) of the health check to associate with the protection.
--
-- /Note:/ Consider using 'healthCheckArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahcHealthCheckArn :: Lens.Lens' AssociateHealthCheck Types.HealthCheckArn
ahcHealthCheckArn = Lens.field @"healthCheckArn"
{-# DEPRECATED ahcHealthCheckArn "Use generic-lens or generic-optics with 'healthCheckArn' instead." #-}

instance Core.FromJSON AssociateHealthCheck where
  toJSON AssociateHealthCheck {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ProtectionId" Core..= protectionId),
            Core.Just ("HealthCheckArn" Core..= healthCheckArn)
          ]
      )

instance Core.AWSRequest AssociateHealthCheck where
  type Rs AssociateHealthCheck = AssociateHealthCheckResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSShield_20160616.AssociateHealthCheck")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateHealthCheckResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAssociateHealthCheckResponse' smart constructor.
newtype AssociateHealthCheckResponse = AssociateHealthCheckResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateHealthCheckResponse' value with any optional fields omitted.
mkAssociateHealthCheckResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AssociateHealthCheckResponse
mkAssociateHealthCheckResponse responseStatus =
  AssociateHealthCheckResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahcrrsResponseStatus :: Lens.Lens' AssociateHealthCheckResponse Core.Int
ahcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ahcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
