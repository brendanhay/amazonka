{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayMulticastDomainAssociations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.TransitGatewayMulticastDomainAssociations
  ( TransitGatewayMulticastDomainAssociations (..)
  -- * Smart constructor
  , mkTransitGatewayMulticastDomainAssociations
  -- * Lenses
  , tgmdasResourceId
  , tgmdasResourceType
  , tgmdasSubnets
  , tgmdasTransitGatewayAttachmentId
  , tgmdasTransitGatewayMulticastDomainId
  ) where

import qualified Network.AWS.EC2.Types.SubnetAssociation as Types
import qualified Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the multicast domain associations.
--
-- /See:/ 'mkTransitGatewayMulticastDomainAssociations' smart constructor.
data TransitGatewayMulticastDomainAssociations = TransitGatewayMulticastDomainAssociations'
  { resourceId :: Core.Maybe Core.Text
    -- ^ The ID of the resource.
  , resourceType :: Core.Maybe Types.TransitGatewayAttachmentResourceType
    -- ^ The type of resource, for example a VPC attachment.
  , subnets :: Core.Maybe [Types.SubnetAssociation]
    -- ^ The subnets associated with the multicast domain.
  , transitGatewayAttachmentId :: Core.Maybe Core.Text
    -- ^ The ID of the transit gateway attachment.
  , transitGatewayMulticastDomainId :: Core.Maybe Core.Text
    -- ^ The ID of the transit gateway multicast domain.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TransitGatewayMulticastDomainAssociations' value with any optional fields omitted.
mkTransitGatewayMulticastDomainAssociations
    :: TransitGatewayMulticastDomainAssociations
mkTransitGatewayMulticastDomainAssociations
  = TransitGatewayMulticastDomainAssociations'{resourceId =
                                                 Core.Nothing,
                                               resourceType = Core.Nothing, subnets = Core.Nothing,
                                               transitGatewayAttachmentId = Core.Nothing,
                                               transitGatewayMulticastDomainId = Core.Nothing}

-- | The ID of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmdasResourceId :: Lens.Lens' TransitGatewayMulticastDomainAssociations (Core.Maybe Core.Text)
tgmdasResourceId = Lens.field @"resourceId"
{-# INLINEABLE tgmdasResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | The type of resource, for example a VPC attachment.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmdasResourceType :: Lens.Lens' TransitGatewayMulticastDomainAssociations (Core.Maybe Types.TransitGatewayAttachmentResourceType)
tgmdasResourceType = Lens.field @"resourceType"
{-# INLINEABLE tgmdasResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | The subnets associated with the multicast domain.
--
-- /Note:/ Consider using 'subnets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmdasSubnets :: Lens.Lens' TransitGatewayMulticastDomainAssociations (Core.Maybe [Types.SubnetAssociation])
tgmdasSubnets = Lens.field @"subnets"
{-# INLINEABLE tgmdasSubnets #-}
{-# DEPRECATED subnets "Use generic-lens or generic-optics with 'subnets' instead"  #-}

-- | The ID of the transit gateway attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmdasTransitGatewayAttachmentId :: Lens.Lens' TransitGatewayMulticastDomainAssociations (Core.Maybe Core.Text)
tgmdasTransitGatewayAttachmentId = Lens.field @"transitGatewayAttachmentId"
{-# INLINEABLE tgmdasTransitGatewayAttachmentId #-}
{-# DEPRECATED transitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead"  #-}

-- | The ID of the transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmdasTransitGatewayMulticastDomainId :: Lens.Lens' TransitGatewayMulticastDomainAssociations (Core.Maybe Core.Text)
tgmdasTransitGatewayMulticastDomainId = Lens.field @"transitGatewayMulticastDomainId"
{-# INLINEABLE tgmdasTransitGatewayMulticastDomainId #-}
{-# DEPRECATED transitGatewayMulticastDomainId "Use generic-lens or generic-optics with 'transitGatewayMulticastDomainId' instead"  #-}

instance Core.FromXML TransitGatewayMulticastDomainAssociations
         where
        parseXML x
          = TransitGatewayMulticastDomainAssociations' Core.<$>
              (x Core..@? "resourceId") Core.<*> x Core..@? "resourceType"
                Core.<*> x Core..@? "subnets" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "transitGatewayAttachmentId"
                Core.<*> x Core..@? "transitGatewayMulticastDomainId"
