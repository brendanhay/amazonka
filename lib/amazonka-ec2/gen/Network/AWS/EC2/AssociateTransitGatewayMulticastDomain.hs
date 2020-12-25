{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AssociateTransitGatewayMulticastDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified subnets and transit gateway attachments with the specified transit gateway multicast domain.
--
-- The transit gateway attachment must be in the available state before you can add a resource. Use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeTransitGatewayAttachments.html DescribeTransitGatewayAttachments> to see the state of the attachment.
module Network.AWS.EC2.AssociateTransitGatewayMulticastDomain
  ( -- * Creating a request
    AssociateTransitGatewayMulticastDomain (..),
    mkAssociateTransitGatewayMulticastDomain,

    -- ** Request lenses
    atgmdDryRun,
    atgmdSubnetIds,
    atgmdTransitGatewayAttachmentId,
    atgmdTransitGatewayMulticastDomainId,

    -- * Destructuring the response
    AssociateTransitGatewayMulticastDomainResponse (..),
    mkAssociateTransitGatewayMulticastDomainResponse,

    -- ** Response lenses
    atgmdrrsAssociations,
    atgmdrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssociateTransitGatewayMulticastDomain' smart constructor.
data AssociateTransitGatewayMulticastDomain = AssociateTransitGatewayMulticastDomain'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The IDs of the subnets to associate with the transit gateway multicast domain.
    subnetIds :: Core.Maybe [Types.String],
    -- | The ID of the transit gateway attachment to associate with the transit gateway multicast domain.
    transitGatewayAttachmentId :: Core.Maybe Types.TransitGatewayAttachmentId,
    -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Core.Maybe Types.TransitGatewayMulticastDomainId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateTransitGatewayMulticastDomain' value with any optional fields omitted.
mkAssociateTransitGatewayMulticastDomain ::
  AssociateTransitGatewayMulticastDomain
mkAssociateTransitGatewayMulticastDomain =
  AssociateTransitGatewayMulticastDomain'
    { dryRun = Core.Nothing,
      subnetIds = Core.Nothing,
      transitGatewayAttachmentId = Core.Nothing,
      transitGatewayMulticastDomainId = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atgmdDryRun :: Lens.Lens' AssociateTransitGatewayMulticastDomain (Core.Maybe Core.Bool)
atgmdDryRun = Lens.field @"dryRun"
{-# DEPRECATED atgmdDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The IDs of the subnets to associate with the transit gateway multicast domain.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atgmdSubnetIds :: Lens.Lens' AssociateTransitGatewayMulticastDomain (Core.Maybe [Types.String])
atgmdSubnetIds = Lens.field @"subnetIds"
{-# DEPRECATED atgmdSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | The ID of the transit gateway attachment to associate with the transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atgmdTransitGatewayAttachmentId :: Lens.Lens' AssociateTransitGatewayMulticastDomain (Core.Maybe Types.TransitGatewayAttachmentId)
atgmdTransitGatewayAttachmentId = Lens.field @"transitGatewayAttachmentId"
{-# DEPRECATED atgmdTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

-- | The ID of the transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atgmdTransitGatewayMulticastDomainId :: Lens.Lens' AssociateTransitGatewayMulticastDomain (Core.Maybe Types.TransitGatewayMulticastDomainId)
atgmdTransitGatewayMulticastDomainId = Lens.field @"transitGatewayMulticastDomainId"
{-# DEPRECATED atgmdTransitGatewayMulticastDomainId "Use generic-lens or generic-optics with 'transitGatewayMulticastDomainId' instead." #-}

instance Core.AWSRequest AssociateTransitGatewayMulticastDomain where
  type
    Rs AssociateTransitGatewayMulticastDomain =
      AssociateTransitGatewayMulticastDomainResponse
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
            ( Core.pure ("Action", "AssociateTransitGatewayMulticastDomain")
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
          AssociateTransitGatewayMulticastDomainResponse'
            Core.<$> (x Core..@? "associations") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAssociateTransitGatewayMulticastDomainResponse' smart constructor.
data AssociateTransitGatewayMulticastDomainResponse = AssociateTransitGatewayMulticastDomainResponse'
  { -- | Information about the transit gateway multicast domain associations.
    associations :: Core.Maybe Types.TransitGatewayMulticastDomainAssociations,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateTransitGatewayMulticastDomainResponse' value with any optional fields omitted.
mkAssociateTransitGatewayMulticastDomainResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AssociateTransitGatewayMulticastDomainResponse
mkAssociateTransitGatewayMulticastDomainResponse responseStatus =
  AssociateTransitGatewayMulticastDomainResponse'
    { associations =
        Core.Nothing,
      responseStatus
    }

-- | Information about the transit gateway multicast domain associations.
--
-- /Note:/ Consider using 'associations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atgmdrrsAssociations :: Lens.Lens' AssociateTransitGatewayMulticastDomainResponse (Core.Maybe Types.TransitGatewayMulticastDomainAssociations)
atgmdrrsAssociations = Lens.field @"associations"
{-# DEPRECATED atgmdrrsAssociations "Use generic-lens or generic-optics with 'associations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atgmdrrsResponseStatus :: Lens.Lens' AssociateTransitGatewayMulticastDomainResponse Core.Int
atgmdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED atgmdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
