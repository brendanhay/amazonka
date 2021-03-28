{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteVpcEndpointServiceConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more VPC endpoint service configurations in your account. Before you delete the endpoint service configuration, you must reject any @Available@ or @PendingAcceptance@ interface endpoint connections that are attached to the service.
module Network.AWS.EC2.DeleteVpcEndpointServiceConfigurations
    (
    -- * Creating a request
      DeleteVpcEndpointServiceConfigurations (..)
    , mkDeleteVpcEndpointServiceConfigurations
    -- ** Request lenses
    , dvescsServiceIds
    , dvescsDryRun

    -- * Destructuring the response
    , DeleteVpcEndpointServiceConfigurationsResponse (..)
    , mkDeleteVpcEndpointServiceConfigurationsResponse
    -- ** Response lenses
    , dvescrfrsUnsuccessful
    , dvescrfrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteVpcEndpointServiceConfigurations' smart constructor.
data DeleteVpcEndpointServiceConfigurations = DeleteVpcEndpointServiceConfigurations'
  { serviceIds :: [Types.VpcEndpointServiceId]
    -- ^ The IDs of one or more services.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVpcEndpointServiceConfigurations' value with any optional fields omitted.
mkDeleteVpcEndpointServiceConfigurations
    :: DeleteVpcEndpointServiceConfigurations
mkDeleteVpcEndpointServiceConfigurations
  = DeleteVpcEndpointServiceConfigurations'{serviceIds = Core.mempty,
                                            dryRun = Core.Nothing}

-- | The IDs of one or more services.
--
-- /Note:/ Consider using 'serviceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvescsServiceIds :: Lens.Lens' DeleteVpcEndpointServiceConfigurations [Types.VpcEndpointServiceId]
dvescsServiceIds = Lens.field @"serviceIds"
{-# INLINEABLE dvescsServiceIds #-}
{-# DEPRECATED serviceIds "Use generic-lens or generic-optics with 'serviceIds' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvescsDryRun :: Lens.Lens' DeleteVpcEndpointServiceConfigurations (Core.Maybe Core.Bool)
dvescsDryRun = Lens.field @"dryRun"
{-# INLINEABLE dvescsDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DeleteVpcEndpointServiceConfigurations where
        toQuery DeleteVpcEndpointServiceConfigurations{..}
          = Core.toQueryPair "Action"
              ("DeleteVpcEndpointServiceConfigurations" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryList "ServiceId" serviceIds
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DeleteVpcEndpointServiceConfigurations
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteVpcEndpointServiceConfigurations
         where
        type Rs DeleteVpcEndpointServiceConfigurations =
             DeleteVpcEndpointServiceConfigurationsResponse
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
          = Response.receiveXML
              (\ s h x ->
                 DeleteVpcEndpointServiceConfigurationsResponse' Core.<$>
                   (x Core..@? "unsuccessful" Core..<@> Core.parseXMLList "item")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteVpcEndpointServiceConfigurationsResponse' smart constructor.
data DeleteVpcEndpointServiceConfigurationsResponse = DeleteVpcEndpointServiceConfigurationsResponse'
  { unsuccessful :: Core.Maybe [Types.UnsuccessfulItem]
    -- ^ Information about the service configurations that were not deleted, if applicable.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVpcEndpointServiceConfigurationsResponse' value with any optional fields omitted.
mkDeleteVpcEndpointServiceConfigurationsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteVpcEndpointServiceConfigurationsResponse
mkDeleteVpcEndpointServiceConfigurationsResponse responseStatus
  = DeleteVpcEndpointServiceConfigurationsResponse'{unsuccessful =
                                                      Core.Nothing,
                                                    responseStatus}

-- | Information about the service configurations that were not deleted, if applicable.
--
-- /Note:/ Consider using 'unsuccessful' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvescrfrsUnsuccessful :: Lens.Lens' DeleteVpcEndpointServiceConfigurationsResponse (Core.Maybe [Types.UnsuccessfulItem])
dvescrfrsUnsuccessful = Lens.field @"unsuccessful"
{-# INLINEABLE dvescrfrsUnsuccessful #-}
{-# DEPRECATED unsuccessful "Use generic-lens or generic-optics with 'unsuccessful' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvescrfrsResponseStatus :: Lens.Lens' DeleteVpcEndpointServiceConfigurationsResponse Core.Int
dvescrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dvescrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
