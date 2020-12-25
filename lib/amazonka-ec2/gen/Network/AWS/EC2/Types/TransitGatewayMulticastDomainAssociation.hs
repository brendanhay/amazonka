{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayMulticastDomainAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayMulticastDomainAssociation
  ( TransitGatewayMulticastDomainAssociation (..),

    -- * Smart constructor
    mkTransitGatewayMulticastDomainAssociation,

    -- * Lenses
    tgmdaResourceId,
    tgmdaResourceType,
    tgmdaSubnet,
    tgmdaTransitGatewayAttachmentId,
  )
where

import qualified Network.AWS.EC2.Types.ResourceId as Types
import qualified Network.AWS.EC2.Types.SubnetAssociation as Types
import qualified Network.AWS.EC2.Types.TransitGatewayAttachmentId as Types
import qualified Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the resources associated with the transit gateway multicast domain.
--
-- /See:/ 'mkTransitGatewayMulticastDomainAssociation' smart constructor.
data TransitGatewayMulticastDomainAssociation = TransitGatewayMulticastDomainAssociation'
  { -- | The ID of the resource.
    resourceId :: Core.Maybe Types.ResourceId,
    -- | The type of resource, for example a VPC attachment.
    resourceType :: Core.Maybe Types.TransitGatewayAttachmentResourceType,
    -- | The subnet associated with the transit gateway multicast domain.
    subnet :: Core.Maybe Types.SubnetAssociation,
    -- | The ID of the transit gateway attachment.
    transitGatewayAttachmentId :: Core.Maybe Types.TransitGatewayAttachmentId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TransitGatewayMulticastDomainAssociation' value with any optional fields omitted.
mkTransitGatewayMulticastDomainAssociation ::
  TransitGatewayMulticastDomainAssociation
mkTransitGatewayMulticastDomainAssociation =
  TransitGatewayMulticastDomainAssociation'
    { resourceId =
        Core.Nothing,
      resourceType = Core.Nothing,
      subnet = Core.Nothing,
      transitGatewayAttachmentId = Core.Nothing
    }

-- | The ID of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmdaResourceId :: Lens.Lens' TransitGatewayMulticastDomainAssociation (Core.Maybe Types.ResourceId)
tgmdaResourceId = Lens.field @"resourceId"
{-# DEPRECATED tgmdaResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The type of resource, for example a VPC attachment.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmdaResourceType :: Lens.Lens' TransitGatewayMulticastDomainAssociation (Core.Maybe Types.TransitGatewayAttachmentResourceType)
tgmdaResourceType = Lens.field @"resourceType"
{-# DEPRECATED tgmdaResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The subnet associated with the transit gateway multicast domain.
--
-- /Note:/ Consider using 'subnet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmdaSubnet :: Lens.Lens' TransitGatewayMulticastDomainAssociation (Core.Maybe Types.SubnetAssociation)
tgmdaSubnet = Lens.field @"subnet"
{-# DEPRECATED tgmdaSubnet "Use generic-lens or generic-optics with 'subnet' instead." #-}

-- | The ID of the transit gateway attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmdaTransitGatewayAttachmentId :: Lens.Lens' TransitGatewayMulticastDomainAssociation (Core.Maybe Types.TransitGatewayAttachmentId)
tgmdaTransitGatewayAttachmentId = Lens.field @"transitGatewayAttachmentId"
{-# DEPRECATED tgmdaTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

instance Core.FromXML TransitGatewayMulticastDomainAssociation where
  parseXML x =
    TransitGatewayMulticastDomainAssociation'
      Core.<$> (x Core..@? "resourceId")
      Core.<*> (x Core..@? "resourceType")
      Core.<*> (x Core..@? "subnet")
      Core.<*> (x Core..@? "transitGatewayAttachmentId")
