{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.UpdateService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Submits a request to perform the following operations:
--
--
--     * Update the TTL setting for existing @DnsRecords@ configurations
--
--
--     * Add, update, or delete @HealthCheckConfig@ for a specified service
--
--
-- For public and private DNS namespaces, note the following:
--
--     * If you omit any existing @DnsRecords@ or @HealthCheckConfig@ configurations from an @UpdateService@ request, the configurations are deleted from the service.
--
--
--     * If you omit an existing @HealthCheckCustomConfig@ configuration from an @UpdateService@ request, the configuration is not deleted from the service.
--
--
-- When you update settings for a service, AWS Cloud Map also updates the corresponding settings in all the records and health checks that were created by using the specified service.
module Network.AWS.Route53AutoNaming.UpdateService
    (
    -- * Creating a request
      UpdateService (..)
    , mkUpdateService
    -- ** Request lenses
    , usId
    , usService

    -- * Destructuring the response
    , UpdateServiceResponse (..)
    , mkUpdateServiceResponse
    -- ** Response lenses
    , usrrsOperationId
    , usrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53AutoNaming.Types as Types

-- | /See:/ 'mkUpdateService' smart constructor.
data UpdateService = UpdateService'
  { id :: Types.ResourceId
    -- ^ The ID of the service that you want to update.
  , service :: Types.ServiceChange
    -- ^ A complex type that contains the new settings for the service.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateService' value with any optional fields omitted.
mkUpdateService
    :: Types.ResourceId -- ^ 'id'
    -> Types.ServiceChange -- ^ 'service'
    -> UpdateService
mkUpdateService id service = UpdateService'{id, service}

-- | The ID of the service that you want to update.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usId :: Lens.Lens' UpdateService Types.ResourceId
usId = Lens.field @"id"
{-# INLINEABLE usId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | A complex type that contains the new settings for the service.
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usService :: Lens.Lens' UpdateService Types.ServiceChange
usService = Lens.field @"service"
{-# INLINEABLE usService #-}
{-# DEPRECATED service "Use generic-lens or generic-optics with 'service' instead"  #-}

instance Core.ToQuery UpdateService where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateService where
        toHeaders UpdateService{..}
          = Core.pure
              ("X-Amz-Target", "Route53AutoNaming_v20170314.UpdateService")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateService where
        toJSON UpdateService{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Id" Core..= id),
                  Core.Just ("Service" Core..= service)])

instance Core.AWSRequest UpdateService where
        type Rs UpdateService = UpdateServiceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateServiceResponse' Core.<$>
                   (x Core..:? "OperationId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateServiceResponse' smart constructor.
data UpdateServiceResponse = UpdateServiceResponse'
  { operationId :: Core.Maybe Types.OperationId
    -- ^ A value that you can use to determine whether the request completed successfully. To get the status of the operation, see <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation> .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateServiceResponse' value with any optional fields omitted.
mkUpdateServiceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateServiceResponse
mkUpdateServiceResponse responseStatus
  = UpdateServiceResponse'{operationId = Core.Nothing,
                           responseStatus}

-- | A value that you can use to determine whether the request completed successfully. To get the status of the operation, see <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation> .
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrrsOperationId :: Lens.Lens' UpdateServiceResponse (Core.Maybe Types.OperationId)
usrrsOperationId = Lens.field @"operationId"
{-# INLINEABLE usrrsOperationId #-}
{-# DEPRECATED operationId "Use generic-lens or generic-optics with 'operationId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrrsResponseStatus :: Lens.Lens' UpdateServiceResponse Core.Int
usrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE usrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
