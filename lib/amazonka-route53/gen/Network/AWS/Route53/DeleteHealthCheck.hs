{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.DeleteHealthCheck
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a health check.
--
-- /Important:/ Amazon Route 53 does not prevent you from deleting a health check even if the health check is associated with one or more resource record sets. If you delete a health check and you don't update the associated resource record sets, the future status of the health check can't be predicted and may change. This will affect the routing of DNS queries for your DNS failover configuration. For more information, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/health-checks-creating-deleting.html#health-checks-deleting.html Replacing and Deleting Health Checks> in the /Amazon Route 53 Developer Guide/ .
-- If you're using AWS Cloud Map and you configured Cloud Map to create a Route 53 health check when you register an instance, you can't use the Route 53 @DeleteHealthCheck@ command to delete the health check. The health check is deleted automatically when you deregister the instance; there can be a delay of several hours before the health check is deleted from Route 53. 
module Network.AWS.Route53.DeleteHealthCheck
    (
    -- * Creating a request
      DeleteHealthCheck (..)
    , mkDeleteHealthCheck
    -- ** Request lenses
    , dhcHealthCheckId

    -- * Destructuring the response
    , DeleteHealthCheckResponse (..)
    , mkDeleteHealthCheckResponse
    -- ** Response lenses
    , dhcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | This action deletes a health check.
--
-- /See:/ 'mkDeleteHealthCheck' smart constructor.
newtype DeleteHealthCheck = DeleteHealthCheck'
  { healthCheckId :: Types.HealthCheckId
    -- ^ The ID of the health check that you want to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteHealthCheck' value with any optional fields omitted.
mkDeleteHealthCheck
    :: Types.HealthCheckId -- ^ 'healthCheckId'
    -> DeleteHealthCheck
mkDeleteHealthCheck healthCheckId
  = DeleteHealthCheck'{healthCheckId}

-- | The ID of the health check that you want to delete.
--
-- /Note:/ Consider using 'healthCheckId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhcHealthCheckId :: Lens.Lens' DeleteHealthCheck Types.HealthCheckId
dhcHealthCheckId = Lens.field @"healthCheckId"
{-# INLINEABLE dhcHealthCheckId #-}
{-# DEPRECATED healthCheckId "Use generic-lens or generic-optics with 'healthCheckId' instead"  #-}

instance Core.ToQuery DeleteHealthCheck where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteHealthCheck where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteHealthCheck where
        type Rs DeleteHealthCheck = DeleteHealthCheckResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/2013-04-01/healthcheck/" Core.<> Core.toText healthCheckId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteHealthCheckResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | An empty element.
--
-- /See:/ 'mkDeleteHealthCheckResponse' smart constructor.
newtype DeleteHealthCheckResponse = DeleteHealthCheckResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteHealthCheckResponse' value with any optional fields omitted.
mkDeleteHealthCheckResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteHealthCheckResponse
mkDeleteHealthCheckResponse responseStatus
  = DeleteHealthCheckResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhcrrsResponseStatus :: Lens.Lens' DeleteHealthCheckResponse Core.Int
dhcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dhcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
