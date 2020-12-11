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
    tResourceId,
    tResourceType,
    tSubnets,
    tTransitGatewayMulticastDomainId,
    tTransitGatewayAttachmentId,
  )
where

import Network.AWS.EC2.Types.SubnetAssociation
import Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the multicast domain associations.
--
-- /See:/ 'mkTransitGatewayMulticastDomainAssociations' smart constructor.
data TransitGatewayMulticastDomainAssociations = TransitGatewayMulticastDomainAssociations'
  { resourceId ::
      Lude.Maybe
        Lude.Text,
    resourceType ::
      Lude.Maybe
        TransitGatewayAttachmentResourceType,
    subnets ::
      Lude.Maybe
        [SubnetAssociation],
    transitGatewayMulticastDomainId ::
      Lude.Maybe
        Lude.Text,
    transitGatewayAttachmentId ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransitGatewayMulticastDomainAssociations' with the minimum fields required to make a request.
--
-- * 'resourceId' - The ID of the resource.
-- * 'resourceType' - The type of resource, for example a VPC attachment.
-- * 'subnets' - The subnets associated with the multicast domain.
-- * 'transitGatewayAttachmentId' - The ID of the transit gateway attachment.
-- * 'transitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
mkTransitGatewayMulticastDomainAssociations ::
  TransitGatewayMulticastDomainAssociations
mkTransitGatewayMulticastDomainAssociations =
  TransitGatewayMulticastDomainAssociations'
    { resourceId =
        Lude.Nothing,
      resourceType = Lude.Nothing,
      subnets = Lude.Nothing,
      transitGatewayMulticastDomainId = Lude.Nothing,
      transitGatewayAttachmentId = Lude.Nothing
    }

-- | The ID of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tResourceId :: Lens.Lens' TransitGatewayMulticastDomainAssociations (Lude.Maybe Lude.Text)
tResourceId = Lens.lens (resourceId :: TransitGatewayMulticastDomainAssociations -> Lude.Maybe Lude.Text) (\s a -> s {resourceId = a} :: TransitGatewayMulticastDomainAssociations)
{-# DEPRECATED tResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The type of resource, for example a VPC attachment.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tResourceType :: Lens.Lens' TransitGatewayMulticastDomainAssociations (Lude.Maybe TransitGatewayAttachmentResourceType)
tResourceType = Lens.lens (resourceType :: TransitGatewayMulticastDomainAssociations -> Lude.Maybe TransitGatewayAttachmentResourceType) (\s a -> s {resourceType = a} :: TransitGatewayMulticastDomainAssociations)
{-# DEPRECATED tResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The subnets associated with the multicast domain.
--
-- /Note:/ Consider using 'subnets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tSubnets :: Lens.Lens' TransitGatewayMulticastDomainAssociations (Lude.Maybe [SubnetAssociation])
tSubnets = Lens.lens (subnets :: TransitGatewayMulticastDomainAssociations -> Lude.Maybe [SubnetAssociation]) (\s a -> s {subnets = a} :: TransitGatewayMulticastDomainAssociations)
{-# DEPRECATED tSubnets "Use generic-lens or generic-optics with 'subnets' instead." #-}

-- | The ID of the transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTransitGatewayMulticastDomainId :: Lens.Lens' TransitGatewayMulticastDomainAssociations (Lude.Maybe Lude.Text)
tTransitGatewayMulticastDomainId = Lens.lens (transitGatewayMulticastDomainId :: TransitGatewayMulticastDomainAssociations -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayMulticastDomainId = a} :: TransitGatewayMulticastDomainAssociations)
{-# DEPRECATED tTransitGatewayMulticastDomainId "Use generic-lens or generic-optics with 'transitGatewayMulticastDomainId' instead." #-}

-- | The ID of the transit gateway attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTransitGatewayAttachmentId :: Lens.Lens' TransitGatewayMulticastDomainAssociations (Lude.Maybe Lude.Text)
tTransitGatewayAttachmentId = Lens.lens (transitGatewayAttachmentId :: TransitGatewayMulticastDomainAssociations -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayAttachmentId = a} :: TransitGatewayMulticastDomainAssociations)
{-# DEPRECATED tTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

instance Lude.FromXML TransitGatewayMulticastDomainAssociations where
  parseXML x =
    TransitGatewayMulticastDomainAssociations'
      Lude.<$> (x Lude..@? "resourceId")
      Lude.<*> (x Lude..@? "resourceType")
      Lude.<*> ( x Lude..@? "subnets" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "transitGatewayMulticastDomainId")
      Lude.<*> (x Lude..@? "transitGatewayAttachmentId")
