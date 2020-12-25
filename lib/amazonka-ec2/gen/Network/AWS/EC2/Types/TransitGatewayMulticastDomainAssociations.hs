{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayMulticastDomainAssociations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayMulticastDomainAssociations
  ( TransitGatewayMulticastDomainAssociations (..),

    -- * Smart constructor
    mkTransitGatewayMulticastDomainAssociations,

    -- * Lenses
    tgmdasResourceId,
    tgmdasResourceType,
    tgmdasSubnets,
    tgmdasTransitGatewayAttachmentId,
    tgmdasTransitGatewayMulticastDomainId,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.SubnetAssociation as Types
import qualified Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the multicast domain associations.
--
-- /See:/ 'mkTransitGatewayMulticastDomainAssociations' smart constructor.
data TransitGatewayMulticastDomainAssociations = TransitGatewayMulticastDomainAssociations'
  { -- | The ID of the resource.
    resourceId :: Core.Maybe Types.String,
    -- | The type of resource, for example a VPC attachment.
    resourceType :: Core.Maybe Types.TransitGatewayAttachmentResourceType,
    -- | The subnets associated with the multicast domain.
    subnets :: Core.Maybe [Types.SubnetAssociation],
    -- | The ID of the transit gateway attachment.
    transitGatewayAttachmentId :: Core.Maybe Types.String,
    -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TransitGatewayMulticastDomainAssociations' value with any optional fields omitted.
mkTransitGatewayMulticastDomainAssociations ::
  TransitGatewayMulticastDomainAssociations
mkTransitGatewayMulticastDomainAssociations =
  TransitGatewayMulticastDomainAssociations'
    { resourceId =
        Core.Nothing,
      resourceType = Core.Nothing,
      subnets = Core.Nothing,
      transitGatewayAttachmentId = Core.Nothing,
      transitGatewayMulticastDomainId = Core.Nothing
    }

-- | The ID of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmdasResourceId :: Lens.Lens' TransitGatewayMulticastDomainAssociations (Core.Maybe Types.String)
tgmdasResourceId = Lens.field @"resourceId"
{-# DEPRECATED tgmdasResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The type of resource, for example a VPC attachment.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmdasResourceType :: Lens.Lens' TransitGatewayMulticastDomainAssociations (Core.Maybe Types.TransitGatewayAttachmentResourceType)
tgmdasResourceType = Lens.field @"resourceType"
{-# DEPRECATED tgmdasResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The subnets associated with the multicast domain.
--
-- /Note:/ Consider using 'subnets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmdasSubnets :: Lens.Lens' TransitGatewayMulticastDomainAssociations (Core.Maybe [Types.SubnetAssociation])
tgmdasSubnets = Lens.field @"subnets"
{-# DEPRECATED tgmdasSubnets "Use generic-lens or generic-optics with 'subnets' instead." #-}

-- | The ID of the transit gateway attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmdasTransitGatewayAttachmentId :: Lens.Lens' TransitGatewayMulticastDomainAssociations (Core.Maybe Types.String)
tgmdasTransitGatewayAttachmentId = Lens.field @"transitGatewayAttachmentId"
{-# DEPRECATED tgmdasTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

-- | The ID of the transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmdasTransitGatewayMulticastDomainId :: Lens.Lens' TransitGatewayMulticastDomainAssociations (Core.Maybe Types.String)
tgmdasTransitGatewayMulticastDomainId = Lens.field @"transitGatewayMulticastDomainId"
{-# DEPRECATED tgmdasTransitGatewayMulticastDomainId "Use generic-lens or generic-optics with 'transitGatewayMulticastDomainId' instead." #-}

instance Core.FromXML TransitGatewayMulticastDomainAssociations where
  parseXML x =
    TransitGatewayMulticastDomainAssociations'
      Core.<$> (x Core..@? "resourceId")
      Core.<*> (x Core..@? "resourceType")
      Core.<*> (x Core..@? "subnets" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "transitGatewayAttachmentId")
      Core.<*> (x Core..@? "transitGatewayMulticastDomainId")
