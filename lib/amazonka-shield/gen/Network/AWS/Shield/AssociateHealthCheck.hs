{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      AssociateHealthCheck (..)
    , mkAssociateHealthCheck
    -- ** Request lenses
    , ahcProtectionId
    , ahcHealthCheckArn

    -- * Destructuring the response
    , AssociateHealthCheckResponse (..)
    , mkAssociateHealthCheckResponse
    -- ** Response lenses
    , ahcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Shield.Types as Types

-- | /See:/ 'mkAssociateHealthCheck' smart constructor.
data AssociateHealthCheck = AssociateHealthCheck'
  { protectionId :: Types.ProtectionId
    -- ^ The unique identifier (ID) for the 'Protection' object to add the health check association to. 
  , healthCheckArn :: Types.HealthCheckArn
    -- ^ The Amazon Resource Name (ARN) of the health check to associate with the protection.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateHealthCheck' value with any optional fields omitted.
mkAssociateHealthCheck
    :: Types.ProtectionId -- ^ 'protectionId'
    -> Types.HealthCheckArn -- ^ 'healthCheckArn'
    -> AssociateHealthCheck
mkAssociateHealthCheck protectionId healthCheckArn
  = AssociateHealthCheck'{protectionId, healthCheckArn}

-- | The unique identifier (ID) for the 'Protection' object to add the health check association to. 
--
-- /Note:/ Consider using 'protectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahcProtectionId :: Lens.Lens' AssociateHealthCheck Types.ProtectionId
ahcProtectionId = Lens.field @"protectionId"
{-# INLINEABLE ahcProtectionId #-}
{-# DEPRECATED protectionId "Use generic-lens or generic-optics with 'protectionId' instead"  #-}

-- | The Amazon Resource Name (ARN) of the health check to associate with the protection.
--
-- /Note:/ Consider using 'healthCheckArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahcHealthCheckArn :: Lens.Lens' AssociateHealthCheck Types.HealthCheckArn
ahcHealthCheckArn = Lens.field @"healthCheckArn"
{-# INLINEABLE ahcHealthCheckArn #-}
{-# DEPRECATED healthCheckArn "Use generic-lens or generic-optics with 'healthCheckArn' instead"  #-}

instance Core.ToQuery AssociateHealthCheck where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AssociateHealthCheck where
        toHeaders AssociateHealthCheck{..}
          = Core.pure
              ("X-Amz-Target", "AWSShield_20160616.AssociateHealthCheck")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AssociateHealthCheck where
        toJSON AssociateHealthCheck{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ProtectionId" Core..= protectionId),
                  Core.Just ("HealthCheckArn" Core..= healthCheckArn)])

instance Core.AWSRequest AssociateHealthCheck where
        type Rs AssociateHealthCheck = AssociateHealthCheckResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 AssociateHealthCheckResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAssociateHealthCheckResponse' smart constructor.
newtype AssociateHealthCheckResponse = AssociateHealthCheckResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateHealthCheckResponse' value with any optional fields omitted.
mkAssociateHealthCheckResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AssociateHealthCheckResponse
mkAssociateHealthCheckResponse responseStatus
  = AssociateHealthCheckResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahcrrsResponseStatus :: Lens.Lens' AssociateHealthCheckResponse Core.Int
ahcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ahcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
