{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DisassociateTransitGatewayMulticastDomain (..),
    mkDisassociateTransitGatewayMulticastDomain,

    -- ** Request lenses
    dtgmdfDryRun,
    dtgmdfSubnetIds,
    dtgmdfTransitGatewayAttachmentId,
    dtgmdfTransitGatewayMulticastDomainId,

    -- * Destructuring the response
    DisassociateTransitGatewayMulticastDomainResponse (..),
    mkDisassociateTransitGatewayMulticastDomainResponse,

    -- ** Response lenses
    dtgmdrfrsAssociations,
    dtgmdrfrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateTransitGatewayMulticastDomain' smart constructor.
data DisassociateTransitGatewayMulticastDomain = DisassociateTransitGatewayMulticastDomain'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The IDs of the subnets;
    subnetIds :: Core.Maybe [Types.String],
    -- | The ID of the attachment.
    transitGatewayAttachmentId :: Core.Maybe Types.TransitGatewayAttachmentId,
    -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Core.Maybe Types.TransitGatewayMulticastDomainId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateTransitGatewayMulticastDomain' value with any optional fields omitted.
mkDisassociateTransitGatewayMulticastDomain ::
  DisassociateTransitGatewayMulticastDomain
mkDisassociateTransitGatewayMulticastDomain =
  DisassociateTransitGatewayMulticastDomain'
    { dryRun = Core.Nothing,
      subnetIds = Core.Nothing,
      transitGatewayAttachmentId = Core.Nothing,
      transitGatewayMulticastDomainId = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdfDryRun :: Lens.Lens' DisassociateTransitGatewayMulticastDomain (Core.Maybe Core.Bool)
dtgmdfDryRun = Lens.field @"dryRun"
{-# DEPRECATED dtgmdfDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The IDs of the subnets;
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdfSubnetIds :: Lens.Lens' DisassociateTransitGatewayMulticastDomain (Core.Maybe [Types.String])
dtgmdfSubnetIds = Lens.field @"subnetIds"
{-# DEPRECATED dtgmdfSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdfTransitGatewayAttachmentId :: Lens.Lens' DisassociateTransitGatewayMulticastDomain (Core.Maybe Types.TransitGatewayAttachmentId)
dtgmdfTransitGatewayAttachmentId = Lens.field @"transitGatewayAttachmentId"
{-# DEPRECATED dtgmdfTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

-- | The ID of the transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdfTransitGatewayMulticastDomainId :: Lens.Lens' DisassociateTransitGatewayMulticastDomain (Core.Maybe Types.TransitGatewayMulticastDomainId)
dtgmdfTransitGatewayMulticastDomainId = Lens.field @"transitGatewayMulticastDomainId"
{-# DEPRECATED dtgmdfTransitGatewayMulticastDomainId "Use generic-lens or generic-optics with 'transitGatewayMulticastDomainId' instead." #-}

instance Core.AWSRequest DisassociateTransitGatewayMulticastDomain where
  type
    Rs DisassociateTransitGatewayMulticastDomain =
      DisassociateTransitGatewayMulticastDomainResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DisassociateTransitGatewayMulticastDomain")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "SubnetIds" Core.<$> subnetIds)
                Core.<> ( Core.toQueryValue "TransitGatewayAttachmentId"
                            Core.<$> transitGatewayAttachmentId
                        )
                Core.<> ( Core.toQueryValue "TransitGatewayMulticastDomainId"
                            Core.<$> transitGatewayMulticastDomainId
                        )
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DisassociateTransitGatewayMulticastDomainResponse'
            Core.<$> (x Core..@? "associations") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDisassociateTransitGatewayMulticastDomainResponse' smart constructor.
data DisassociateTransitGatewayMulticastDomainResponse = DisassociateTransitGatewayMulticastDomainResponse'
  { -- | Information about the association.
    associations :: Core.Maybe Types.TransitGatewayMulticastDomainAssociations,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateTransitGatewayMulticastDomainResponse' value with any optional fields omitted.
mkDisassociateTransitGatewayMulticastDomainResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DisassociateTransitGatewayMulticastDomainResponse
mkDisassociateTransitGatewayMulticastDomainResponse responseStatus =
  DisassociateTransitGatewayMulticastDomainResponse'
    { associations =
        Core.Nothing,
      responseStatus
    }

-- | Information about the association.
--
-- /Note:/ Consider using 'associations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdrfrsAssociations :: Lens.Lens' DisassociateTransitGatewayMulticastDomainResponse (Core.Maybe Types.TransitGatewayMulticastDomainAssociations)
dtgmdrfrsAssociations = Lens.field @"associations"
{-# DEPRECATED dtgmdrfrsAssociations "Use generic-lens or generic-optics with 'associations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdrfrsResponseStatus :: Lens.Lens' DisassociateTransitGatewayMulticastDomainResponse Core.Int
dtgmdrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtgmdrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
