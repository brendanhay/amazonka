{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DisassociateHealthCheck (..)
    , mkDisassociateHealthCheck
    -- ** Request lenses
    , dhcProtectionId
    , dhcHealthCheckArn

    -- * Destructuring the response
    , DisassociateHealthCheckResponse (..)
    , mkDisassociateHealthCheckResponse
    -- ** Response lenses
    , dhcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Shield.Types as Types

-- | /See:/ 'mkDisassociateHealthCheck' smart constructor.
data DisassociateHealthCheck = DisassociateHealthCheck'
  { protectionId :: Types.ProtectionId
    -- ^ The unique identifier (ID) for the 'Protection' object to remove the health check association from. 
  , healthCheckArn :: Types.HealthCheckArn
    -- ^ The Amazon Resource Name (ARN) of the health check that is associated with the protection.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateHealthCheck' value with any optional fields omitted.
mkDisassociateHealthCheck
    :: Types.ProtectionId -- ^ 'protectionId'
    -> Types.HealthCheckArn -- ^ 'healthCheckArn'
    -> DisassociateHealthCheck
mkDisassociateHealthCheck protectionId healthCheckArn
  = DisassociateHealthCheck'{protectionId, healthCheckArn}

-- | The unique identifier (ID) for the 'Protection' object to remove the health check association from. 
--
-- /Note:/ Consider using 'protectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhcProtectionId :: Lens.Lens' DisassociateHealthCheck Types.ProtectionId
dhcProtectionId = Lens.field @"protectionId"
{-# INLINEABLE dhcProtectionId #-}
{-# DEPRECATED protectionId "Use generic-lens or generic-optics with 'protectionId' instead"  #-}

-- | The Amazon Resource Name (ARN) of the health check that is associated with the protection.
--
-- /Note:/ Consider using 'healthCheckArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhcHealthCheckArn :: Lens.Lens' DisassociateHealthCheck Types.HealthCheckArn
dhcHealthCheckArn = Lens.field @"healthCheckArn"
{-# INLINEABLE dhcHealthCheckArn #-}
{-# DEPRECATED healthCheckArn "Use generic-lens or generic-optics with 'healthCheckArn' instead"  #-}

instance Core.ToQuery DisassociateHealthCheck where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DisassociateHealthCheck where
        toHeaders DisassociateHealthCheck{..}
          = Core.pure
              ("X-Amz-Target", "AWSShield_20160616.DisassociateHealthCheck")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DisassociateHealthCheck where
        toJSON DisassociateHealthCheck{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ProtectionId" Core..= protectionId),
                  Core.Just ("HealthCheckArn" Core..= healthCheckArn)])

instance Core.AWSRequest DisassociateHealthCheck where
        type Rs DisassociateHealthCheck = DisassociateHealthCheckResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DisassociateHealthCheckResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisassociateHealthCheckResponse' smart constructor.
newtype DisassociateHealthCheckResponse = DisassociateHealthCheckResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateHealthCheckResponse' value with any optional fields omitted.
mkDisassociateHealthCheckResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DisassociateHealthCheckResponse
mkDisassociateHealthCheckResponse responseStatus
  = DisassociateHealthCheckResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhcrrsResponseStatus :: Lens.Lens' DisassociateHealthCheckResponse Core.Int
dhcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dhcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
