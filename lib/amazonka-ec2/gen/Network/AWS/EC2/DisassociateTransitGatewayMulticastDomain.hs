{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DisassociateTransitGatewayMulticastDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified subnets from the transit gateway multicast domain. 
module Network.AWS.EC2.DisassociateTransitGatewayMulticastDomain
    (
    -- * Creating a request
      DisassociateTransitGatewayMulticastDomain (..)
    , mkDisassociateTransitGatewayMulticastDomain
    -- ** Request lenses
    , dtgmdfDryRun
    , dtgmdfSubnetIds
    , dtgmdfTransitGatewayAttachmentId
    , dtgmdfTransitGatewayMulticastDomainId

    -- * Destructuring the response
    , DisassociateTransitGatewayMulticastDomainResponse (..)
    , mkDisassociateTransitGatewayMulticastDomainResponse
    -- ** Response lenses
    , dtgmdrfrsAssociations
    , dtgmdrfrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateTransitGatewayMulticastDomain' smart constructor.
data DisassociateTransitGatewayMulticastDomain = DisassociateTransitGatewayMulticastDomain'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , subnetIds :: Core.Maybe [Core.Text]
    -- ^ The IDs of the subnets;
  , transitGatewayAttachmentId :: Core.Maybe Types.TransitGatewayAttachmentId
    -- ^ The ID of the attachment.
  , transitGatewayMulticastDomainId :: Core.Maybe Types.TransitGatewayMulticastDomainId
    -- ^ The ID of the transit gateway multicast domain.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateTransitGatewayMulticastDomain' value with any optional fields omitted.
mkDisassociateTransitGatewayMulticastDomain
    :: DisassociateTransitGatewayMulticastDomain
mkDisassociateTransitGatewayMulticastDomain
  = DisassociateTransitGatewayMulticastDomain'{dryRun = Core.Nothing,
                                               subnetIds = Core.Nothing,
                                               transitGatewayAttachmentId = Core.Nothing,
                                               transitGatewayMulticastDomainId = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdfDryRun :: Lens.Lens' DisassociateTransitGatewayMulticastDomain (Core.Maybe Core.Bool)
dtgmdfDryRun = Lens.field @"dryRun"
{-# INLINEABLE dtgmdfDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The IDs of the subnets;
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdfSubnetIds :: Lens.Lens' DisassociateTransitGatewayMulticastDomain (Core.Maybe [Core.Text])
dtgmdfSubnetIds = Lens.field @"subnetIds"
{-# INLINEABLE dtgmdfSubnetIds #-}
{-# DEPRECATED subnetIds "Use generic-lens or generic-optics with 'subnetIds' instead"  #-}

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdfTransitGatewayAttachmentId :: Lens.Lens' DisassociateTransitGatewayMulticastDomain (Core.Maybe Types.TransitGatewayAttachmentId)
dtgmdfTransitGatewayAttachmentId = Lens.field @"transitGatewayAttachmentId"
{-# INLINEABLE dtgmdfTransitGatewayAttachmentId #-}
{-# DEPRECATED transitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead"  #-}

-- | The ID of the transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdfTransitGatewayMulticastDomainId :: Lens.Lens' DisassociateTransitGatewayMulticastDomain (Core.Maybe Types.TransitGatewayMulticastDomainId)
dtgmdfTransitGatewayMulticastDomainId = Lens.field @"transitGatewayMulticastDomainId"
{-# INLINEABLE dtgmdfTransitGatewayMulticastDomainId #-}
{-# DEPRECATED transitGatewayMulticastDomainId "Use generic-lens or generic-optics with 'transitGatewayMulticastDomainId' instead"  #-}

instance Core.ToQuery DisassociateTransitGatewayMulticastDomain
         where
        toQuery DisassociateTransitGatewayMulticastDomain{..}
          = Core.toQueryPair "Action"
              ("DisassociateTransitGatewayMulticastDomain" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "SubnetIds") subnetIds
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "TransitGatewayAttachmentId")
                transitGatewayAttachmentId
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "TransitGatewayMulticastDomainId")
                transitGatewayMulticastDomainId

instance Core.ToHeaders DisassociateTransitGatewayMulticastDomain
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DisassociateTransitGatewayMulticastDomain
         where
        type Rs DisassociateTransitGatewayMulticastDomain =
             DisassociateTransitGatewayMulticastDomainResponse
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
                 DisassociateTransitGatewayMulticastDomainResponse' Core.<$>
                   (x Core..@? "associations") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisassociateTransitGatewayMulticastDomainResponse' smart constructor.
data DisassociateTransitGatewayMulticastDomainResponse = DisassociateTransitGatewayMulticastDomainResponse'
  { associations :: Core.Maybe Types.TransitGatewayMulticastDomainAssociations
    -- ^ Information about the association.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateTransitGatewayMulticastDomainResponse' value with any optional fields omitted.
mkDisassociateTransitGatewayMulticastDomainResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DisassociateTransitGatewayMulticastDomainResponse
mkDisassociateTransitGatewayMulticastDomainResponse responseStatus
  = DisassociateTransitGatewayMulticastDomainResponse'{associations =
                                                         Core.Nothing,
                                                       responseStatus}

-- | Information about the association.
--
-- /Note:/ Consider using 'associations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdrfrsAssociations :: Lens.Lens' DisassociateTransitGatewayMulticastDomainResponse (Core.Maybe Types.TransitGatewayMulticastDomainAssociations)
dtgmdrfrsAssociations = Lens.field @"associations"
{-# INLINEABLE dtgmdrfrsAssociations #-}
{-# DEPRECATED associations "Use generic-lens or generic-optics with 'associations' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdrfrsResponseStatus :: Lens.Lens' DisassociateTransitGatewayMulticastDomainResponse Core.Int
dtgmdrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtgmdrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
