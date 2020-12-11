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

import Network.AWS.EC2.Types.SubnetAssociation
import Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the resources associated with the transit gateway multicast domain.
--
-- /See:/ 'mkTransitGatewayMulticastDomainAssociation' smart constructor.
data TransitGatewayMulticastDomainAssociation = TransitGatewayMulticastDomainAssociation'
  { resourceId ::
      Lude.Maybe
        Lude.Text,
    resourceType ::
      Lude.Maybe
        TransitGatewayAttachmentResourceType,
    subnet ::
      Lude.Maybe
        SubnetAssociation,
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

-- | Creates a value of 'TransitGatewayMulticastDomainAssociation' with the minimum fields required to make a request.
--
-- * 'resourceId' - The ID of the resource.
-- * 'resourceType' - The type of resource, for example a VPC attachment.
-- * 'subnet' - The subnet associated with the transit gateway multicast domain.
-- * 'transitGatewayAttachmentId' - The ID of the transit gateway attachment.
mkTransitGatewayMulticastDomainAssociation ::
  TransitGatewayMulticastDomainAssociation
mkTransitGatewayMulticastDomainAssociation =
  TransitGatewayMulticastDomainAssociation'
    { resourceId =
        Lude.Nothing,
      resourceType = Lude.Nothing,
      subnet = Lude.Nothing,
      transitGatewayAttachmentId = Lude.Nothing
    }

-- | The ID of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmdaResourceId :: Lens.Lens' TransitGatewayMulticastDomainAssociation (Lude.Maybe Lude.Text)
tgmdaResourceId = Lens.lens (resourceId :: TransitGatewayMulticastDomainAssociation -> Lude.Maybe Lude.Text) (\s a -> s {resourceId = a} :: TransitGatewayMulticastDomainAssociation)
{-# DEPRECATED tgmdaResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The type of resource, for example a VPC attachment.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmdaResourceType :: Lens.Lens' TransitGatewayMulticastDomainAssociation (Lude.Maybe TransitGatewayAttachmentResourceType)
tgmdaResourceType = Lens.lens (resourceType :: TransitGatewayMulticastDomainAssociation -> Lude.Maybe TransitGatewayAttachmentResourceType) (\s a -> s {resourceType = a} :: TransitGatewayMulticastDomainAssociation)
{-# DEPRECATED tgmdaResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The subnet associated with the transit gateway multicast domain.
--
-- /Note:/ Consider using 'subnet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmdaSubnet :: Lens.Lens' TransitGatewayMulticastDomainAssociation (Lude.Maybe SubnetAssociation)
tgmdaSubnet = Lens.lens (subnet :: TransitGatewayMulticastDomainAssociation -> Lude.Maybe SubnetAssociation) (\s a -> s {subnet = a} :: TransitGatewayMulticastDomainAssociation)
{-# DEPRECATED tgmdaSubnet "Use generic-lens or generic-optics with 'subnet' instead." #-}

-- | The ID of the transit gateway attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmdaTransitGatewayAttachmentId :: Lens.Lens' TransitGatewayMulticastDomainAssociation (Lude.Maybe Lude.Text)
tgmdaTransitGatewayAttachmentId = Lens.lens (transitGatewayAttachmentId :: TransitGatewayMulticastDomainAssociation -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayAttachmentId = a} :: TransitGatewayMulticastDomainAssociation)
{-# DEPRECATED tgmdaTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

instance Lude.FromXML TransitGatewayMulticastDomainAssociation where
  parseXML x =
    TransitGatewayMulticastDomainAssociation'
      Lude.<$> (x Lude..@? "resourceId")
      Lude.<*> (x Lude..@? "resourceType")
      Lude.<*> (x Lude..@? "subnet")
      Lude.<*> (x Lude..@? "transitGatewayAttachmentId")
