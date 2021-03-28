{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.GetHealthCheck
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a specified health check.
module Network.AWS.Route53.GetHealthCheck
    (
    -- * Creating a request
      GetHealthCheck (..)
    , mkGetHealthCheck
    -- ** Request lenses
    , ghcHealthCheckId

    -- * Destructuring the response
    , GetHealthCheckResponse (..)
    , mkGetHealthCheckResponse
    -- ** Response lenses
    , ghcrrsHealthCheck
    , ghcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | A request to get information about a specified health check. 
--
-- /See:/ 'mkGetHealthCheck' smart constructor.
newtype GetHealthCheck = GetHealthCheck'
  { healthCheckId :: Types.HealthCheckId
    -- ^ The identifier that Amazon Route 53 assigned to the health check when you created it. When you add or update a resource record set, you use this value to specify which health check to use. The value can be up to 64 characters long.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetHealthCheck' value with any optional fields omitted.
mkGetHealthCheck
    :: Types.HealthCheckId -- ^ 'healthCheckId'
    -> GetHealthCheck
mkGetHealthCheck healthCheckId = GetHealthCheck'{healthCheckId}

-- | The identifier that Amazon Route 53 assigned to the health check when you created it. When you add or update a resource record set, you use this value to specify which health check to use. The value can be up to 64 characters long.
--
-- /Note:/ Consider using 'healthCheckId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghcHealthCheckId :: Lens.Lens' GetHealthCheck Types.HealthCheckId
ghcHealthCheckId = Lens.field @"healthCheckId"
{-# INLINEABLE ghcHealthCheckId #-}
{-# DEPRECATED healthCheckId "Use generic-lens or generic-optics with 'healthCheckId' instead"  #-}

instance Core.ToQuery GetHealthCheck where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetHealthCheck where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetHealthCheck where
        type Rs GetHealthCheck = GetHealthCheckResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/2013-04-01/healthcheck/" Core.<> Core.toText healthCheckId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 GetHealthCheckResponse' Core.<$>
                   (x Core..@ "HealthCheck") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | A complex type that contains the response to a @GetHealthCheck@ request.
--
-- /See:/ 'mkGetHealthCheckResponse' smart constructor.
data GetHealthCheckResponse = GetHealthCheckResponse'
  { healthCheck :: Types.HealthCheck
    -- ^ A complex type that contains information about one health check that is associated with the current AWS account.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetHealthCheckResponse' value with any optional fields omitted.
mkGetHealthCheckResponse
    :: Types.HealthCheck -- ^ 'healthCheck'
    -> Core.Int -- ^ 'responseStatus'
    -> GetHealthCheckResponse
mkGetHealthCheckResponse healthCheck responseStatus
  = GetHealthCheckResponse'{healthCheck, responseStatus}

-- | A complex type that contains information about one health check that is associated with the current AWS account.
--
-- /Note:/ Consider using 'healthCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghcrrsHealthCheck :: Lens.Lens' GetHealthCheckResponse Types.HealthCheck
ghcrrsHealthCheck = Lens.field @"healthCheck"
{-# INLINEABLE ghcrrsHealthCheck #-}
{-# DEPRECATED healthCheck "Use generic-lens or generic-optics with 'healthCheck' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghcrrsResponseStatus :: Lens.Lens' GetHealthCheckResponse Core.Int
ghcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ghcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
