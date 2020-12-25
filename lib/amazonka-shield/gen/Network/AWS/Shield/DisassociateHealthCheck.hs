{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.DisassociateHealthCheck
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes health-based detection from the Shield Advanced protection for a resource. Shield Advanced health-based detection uses the health of your AWS resource to improve responsiveness and accuracy in attack detection and mitigation.
--
-- You define the health check in Route 53 and then associate or disassociate it with your Shield Advanced protection. For more information, see <https://docs.aws.amazon.com/waf/latest/developerguide/ddos-overview.html#ddos-advanced-health-check-option Shield Advanced Health-Based Detection> in the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF and AWS Shield Developer Guide> .
module Network.AWS.Shield.DisassociateHealthCheck
  ( -- * Creating a request
    DisassociateHealthCheck (..),
    mkDisassociateHealthCheck,

    -- ** Request lenses
    dhcProtectionId,
    dhcHealthCheckArn,

    -- * Destructuring the response
    DisassociateHealthCheckResponse (..),
    mkDisassociateHealthCheckResponse,

    -- ** Response lenses
    dhcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Shield.Types as Types

-- | /See:/ 'mkDisassociateHealthCheck' smart constructor.
data DisassociateHealthCheck = DisassociateHealthCheck'
  { -- | The unique identifier (ID) for the 'Protection' object to remove the health check association from.
    protectionId :: Types.ProtectionId,
    -- | The Amazon Resource Name (ARN) of the health check that is associated with the protection.
    healthCheckArn :: Types.HealthCheckArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateHealthCheck' value with any optional fields omitted.
mkDisassociateHealthCheck ::
  -- | 'protectionId'
  Types.ProtectionId ->
  -- | 'healthCheckArn'
  Types.HealthCheckArn ->
  DisassociateHealthCheck
mkDisassociateHealthCheck protectionId healthCheckArn =
  DisassociateHealthCheck' {protectionId, healthCheckArn}

-- | The unique identifier (ID) for the 'Protection' object to remove the health check association from.
--
-- /Note:/ Consider using 'protectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhcProtectionId :: Lens.Lens' DisassociateHealthCheck Types.ProtectionId
dhcProtectionId = Lens.field @"protectionId"
{-# DEPRECATED dhcProtectionId "Use generic-lens or generic-optics with 'protectionId' instead." #-}

-- | The Amazon Resource Name (ARN) of the health check that is associated with the protection.
--
-- /Note:/ Consider using 'healthCheckArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhcHealthCheckArn :: Lens.Lens' DisassociateHealthCheck Types.HealthCheckArn
dhcHealthCheckArn = Lens.field @"healthCheckArn"
{-# DEPRECATED dhcHealthCheckArn "Use generic-lens or generic-optics with 'healthCheckArn' instead." #-}

instance Core.FromJSON DisassociateHealthCheck where
  toJSON DisassociateHealthCheck {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ProtectionId" Core..= protectionId),
            Core.Just ("HealthCheckArn" Core..= healthCheckArn)
          ]
      )

instance Core.AWSRequest DisassociateHealthCheck where
  type Rs DisassociateHealthCheck = DisassociateHealthCheckResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSShield_20160616.DisassociateHealthCheck")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateHealthCheckResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDisassociateHealthCheckResponse' smart constructor.
newtype DisassociateHealthCheckResponse = DisassociateHealthCheckResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateHealthCheckResponse' value with any optional fields omitted.
mkDisassociateHealthCheckResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DisassociateHealthCheckResponse
mkDisassociateHealthCheckResponse responseStatus =
  DisassociateHealthCheckResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhcrrsResponseStatus :: Lens.Lens' DisassociateHealthCheckResponse Core.Int
dhcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dhcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
