{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DisassociateClientVpnTargetNetwork
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a target network from the specified Client VPN endpoint. When you disassociate the last target network from a Client VPN, the following happens:
--
--
--     * The route that was automatically added for the VPC is deleted
--
--
--     * All active client connections are terminated
--
--
--     * New client connections are disallowed
--
--
--     * The Client VPN endpoint's status changes to @pending-associate@ 
--
--
module Network.AWS.EC2.DisassociateClientVpnTargetNetwork
    (
    -- * Creating a request
      DisassociateClientVpnTargetNetwork (..)
    , mkDisassociateClientVpnTargetNetwork
    -- ** Request lenses
    , dcvtnfClientVpnEndpointId
    , dcvtnfAssociationId
    , dcvtnfDryRun

    -- * Destructuring the response
    , DisassociateClientVpnTargetNetworkResponse (..)
    , mkDisassociateClientVpnTargetNetworkResponse
    -- ** Response lenses
    , dcvtnrfrsAssociationId
    , dcvtnrfrsStatus
    , dcvtnrfrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateClientVpnTargetNetwork' smart constructor.
data DisassociateClientVpnTargetNetwork = DisassociateClientVpnTargetNetwork'
  { clientVpnEndpointId :: Types.ClientVpnEndpointId
    -- ^ The ID of the Client VPN endpoint from which to disassociate the target network.
  , associationId :: Types.AssociationId
    -- ^ The ID of the target network association.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateClientVpnTargetNetwork' value with any optional fields omitted.
mkDisassociateClientVpnTargetNetwork
    :: Types.ClientVpnEndpointId -- ^ 'clientVpnEndpointId'
    -> Types.AssociationId -- ^ 'associationId'
    -> DisassociateClientVpnTargetNetwork
mkDisassociateClientVpnTargetNetwork clientVpnEndpointId
  associationId
  = DisassociateClientVpnTargetNetwork'{clientVpnEndpointId,
                                        associationId, dryRun = Core.Nothing}

-- | The ID of the Client VPN endpoint from which to disassociate the target network.
--
-- /Note:/ Consider using 'clientVpnEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvtnfClientVpnEndpointId :: Lens.Lens' DisassociateClientVpnTargetNetwork Types.ClientVpnEndpointId
dcvtnfClientVpnEndpointId = Lens.field @"clientVpnEndpointId"
{-# INLINEABLE dcvtnfClientVpnEndpointId #-}
{-# DEPRECATED clientVpnEndpointId "Use generic-lens or generic-optics with 'clientVpnEndpointId' instead"  #-}

-- | The ID of the target network association.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvtnfAssociationId :: Lens.Lens' DisassociateClientVpnTargetNetwork Types.AssociationId
dcvtnfAssociationId = Lens.field @"associationId"
{-# INLINEABLE dcvtnfAssociationId #-}
{-# DEPRECATED associationId "Use generic-lens or generic-optics with 'associationId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvtnfDryRun :: Lens.Lens' DisassociateClientVpnTargetNetwork (Core.Maybe Core.Bool)
dcvtnfDryRun = Lens.field @"dryRun"
{-# INLINEABLE dcvtnfDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DisassociateClientVpnTargetNetwork where
        toQuery DisassociateClientVpnTargetNetwork{..}
          = Core.toQueryPair "Action"
              ("DisassociateClientVpnTargetNetwork" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "ClientVpnEndpointId" clientVpnEndpointId
              Core.<> Core.toQueryPair "AssociationId" associationId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DisassociateClientVpnTargetNetwork where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DisassociateClientVpnTargetNetwork where
        type Rs DisassociateClientVpnTargetNetwork =
             DisassociateClientVpnTargetNetworkResponse
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
                 DisassociateClientVpnTargetNetworkResponse' Core.<$>
                   (x Core..@? "associationId") Core.<*> x Core..@? "status" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisassociateClientVpnTargetNetworkResponse' smart constructor.
data DisassociateClientVpnTargetNetworkResponse = DisassociateClientVpnTargetNetworkResponse'
  { associationId :: Core.Maybe Core.Text
    -- ^ The ID of the target network association.
  , status :: Core.Maybe Types.AssociationStatus
    -- ^ The current state of the target network association.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateClientVpnTargetNetworkResponse' value with any optional fields omitted.
mkDisassociateClientVpnTargetNetworkResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DisassociateClientVpnTargetNetworkResponse
mkDisassociateClientVpnTargetNetworkResponse responseStatus
  = DisassociateClientVpnTargetNetworkResponse'{associationId =
                                                  Core.Nothing,
                                                status = Core.Nothing, responseStatus}

-- | The ID of the target network association.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvtnrfrsAssociationId :: Lens.Lens' DisassociateClientVpnTargetNetworkResponse (Core.Maybe Core.Text)
dcvtnrfrsAssociationId = Lens.field @"associationId"
{-# INLINEABLE dcvtnrfrsAssociationId #-}
{-# DEPRECATED associationId "Use generic-lens or generic-optics with 'associationId' instead"  #-}

-- | The current state of the target network association.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvtnrfrsStatus :: Lens.Lens' DisassociateClientVpnTargetNetworkResponse (Core.Maybe Types.AssociationStatus)
dcvtnrfrsStatus = Lens.field @"status"
{-# INLINEABLE dcvtnrfrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvtnrfrsResponseStatus :: Lens.Lens' DisassociateClientVpnTargetNetworkResponse Core.Int
dcvtnrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcvtnrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
