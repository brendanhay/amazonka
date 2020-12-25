{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayOptions
  ( TransitGatewayOptions (..),

    -- * Smart constructor
    mkTransitGatewayOptions,

    -- * Lenses
    tgoAmazonSideAsn,
    tgoAssociationDefaultRouteTableId,
    tgoAutoAcceptSharedAttachments,
    tgoDefaultRouteTableAssociation,
    tgoDefaultRouteTablePropagation,
    tgoDnsSupport,
    tgoMulticastSupport,
    tgoPropagationDefaultRouteTableId,
    tgoVpnEcmpSupport,
  )
where

import qualified Network.AWS.EC2.Types.AssociationDefaultRouteTableId as Types
import qualified Network.AWS.EC2.Types.AutoAcceptSharedAttachmentsValue as Types
import qualified Network.AWS.EC2.Types.DefaultRouteTableAssociationValue as Types
import qualified Network.AWS.EC2.Types.DefaultRouteTablePropagationValue as Types
import qualified Network.AWS.EC2.Types.DnsSupportValue as Types
import qualified Network.AWS.EC2.Types.MulticastSupportValue as Types
import qualified Network.AWS.EC2.Types.PropagationDefaultRouteTableId as Types
import qualified Network.AWS.EC2.Types.VpnEcmpSupportValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the options for a transit gateway.
--
-- /See:/ 'mkTransitGatewayOptions' smart constructor.
data TransitGatewayOptions = TransitGatewayOptions'
  { -- | A private Autonomous System Number (ASN) for the Amazon side of a BGP session. The range is 64512 to 65534 for 16-bit ASNs and 4200000000 to 4294967294 for 32-bit ASNs.
    amazonSideAsn :: Core.Maybe Core.Integer,
    -- | The ID of the default association route table.
    associationDefaultRouteTableId :: Core.Maybe Types.AssociationDefaultRouteTableId,
    -- | Indicates whether attachment requests are automatically accepted.
    autoAcceptSharedAttachments :: Core.Maybe Types.AutoAcceptSharedAttachmentsValue,
    -- | Indicates whether resource attachments are automatically associated with the default association route table.
    defaultRouteTableAssociation :: Core.Maybe Types.DefaultRouteTableAssociationValue,
    -- | Indicates whether resource attachments automatically propagate routes to the default propagation route table.
    defaultRouteTablePropagation :: Core.Maybe Types.DefaultRouteTablePropagationValue,
    -- | Indicates whether DNS support is enabled.
    dnsSupport :: Core.Maybe Types.DnsSupportValue,
    -- | Indicates whether multicast is enabled on the transit gateway
    multicastSupport :: Core.Maybe Types.MulticastSupportValue,
    -- | The ID of the default propagation route table.
    propagationDefaultRouteTableId :: Core.Maybe Types.PropagationDefaultRouteTableId,
    -- | Indicates whether Equal Cost Multipath Protocol support is enabled.
    vpnEcmpSupport :: Core.Maybe Types.VpnEcmpSupportValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TransitGatewayOptions' value with any optional fields omitted.
mkTransitGatewayOptions ::
  TransitGatewayOptions
mkTransitGatewayOptions =
  TransitGatewayOptions'
    { amazonSideAsn = Core.Nothing,
      associationDefaultRouteTableId = Core.Nothing,
      autoAcceptSharedAttachments = Core.Nothing,
      defaultRouteTableAssociation = Core.Nothing,
      defaultRouteTablePropagation = Core.Nothing,
      dnsSupport = Core.Nothing,
      multicastSupport = Core.Nothing,
      propagationDefaultRouteTableId = Core.Nothing,
      vpnEcmpSupport = Core.Nothing
    }

-- | A private Autonomous System Number (ASN) for the Amazon side of a BGP session. The range is 64512 to 65534 for 16-bit ASNs and 4200000000 to 4294967294 for 32-bit ASNs.
--
-- /Note:/ Consider using 'amazonSideAsn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgoAmazonSideAsn :: Lens.Lens' TransitGatewayOptions (Core.Maybe Core.Integer)
tgoAmazonSideAsn = Lens.field @"amazonSideAsn"
{-# DEPRECATED tgoAmazonSideAsn "Use generic-lens or generic-optics with 'amazonSideAsn' instead." #-}

-- | The ID of the default association route table.
--
-- /Note:/ Consider using 'associationDefaultRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgoAssociationDefaultRouteTableId :: Lens.Lens' TransitGatewayOptions (Core.Maybe Types.AssociationDefaultRouteTableId)
tgoAssociationDefaultRouteTableId = Lens.field @"associationDefaultRouteTableId"
{-# DEPRECATED tgoAssociationDefaultRouteTableId "Use generic-lens or generic-optics with 'associationDefaultRouteTableId' instead." #-}

-- | Indicates whether attachment requests are automatically accepted.
--
-- /Note:/ Consider using 'autoAcceptSharedAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgoAutoAcceptSharedAttachments :: Lens.Lens' TransitGatewayOptions (Core.Maybe Types.AutoAcceptSharedAttachmentsValue)
tgoAutoAcceptSharedAttachments = Lens.field @"autoAcceptSharedAttachments"
{-# DEPRECATED tgoAutoAcceptSharedAttachments "Use generic-lens or generic-optics with 'autoAcceptSharedAttachments' instead." #-}

-- | Indicates whether resource attachments are automatically associated with the default association route table.
--
-- /Note:/ Consider using 'defaultRouteTableAssociation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgoDefaultRouteTableAssociation :: Lens.Lens' TransitGatewayOptions (Core.Maybe Types.DefaultRouteTableAssociationValue)
tgoDefaultRouteTableAssociation = Lens.field @"defaultRouteTableAssociation"
{-# DEPRECATED tgoDefaultRouteTableAssociation "Use generic-lens or generic-optics with 'defaultRouteTableAssociation' instead." #-}

-- | Indicates whether resource attachments automatically propagate routes to the default propagation route table.
--
-- /Note:/ Consider using 'defaultRouteTablePropagation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgoDefaultRouteTablePropagation :: Lens.Lens' TransitGatewayOptions (Core.Maybe Types.DefaultRouteTablePropagationValue)
tgoDefaultRouteTablePropagation = Lens.field @"defaultRouteTablePropagation"
{-# DEPRECATED tgoDefaultRouteTablePropagation "Use generic-lens or generic-optics with 'defaultRouteTablePropagation' instead." #-}

-- | Indicates whether DNS support is enabled.
--
-- /Note:/ Consider using 'dnsSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgoDnsSupport :: Lens.Lens' TransitGatewayOptions (Core.Maybe Types.DnsSupportValue)
tgoDnsSupport = Lens.field @"dnsSupport"
{-# DEPRECATED tgoDnsSupport "Use generic-lens or generic-optics with 'dnsSupport' instead." #-}

-- | Indicates whether multicast is enabled on the transit gateway
--
-- /Note:/ Consider using 'multicastSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgoMulticastSupport :: Lens.Lens' TransitGatewayOptions (Core.Maybe Types.MulticastSupportValue)
tgoMulticastSupport = Lens.field @"multicastSupport"
{-# DEPRECATED tgoMulticastSupport "Use generic-lens or generic-optics with 'multicastSupport' instead." #-}

-- | The ID of the default propagation route table.
--
-- /Note:/ Consider using 'propagationDefaultRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgoPropagationDefaultRouteTableId :: Lens.Lens' TransitGatewayOptions (Core.Maybe Types.PropagationDefaultRouteTableId)
tgoPropagationDefaultRouteTableId = Lens.field @"propagationDefaultRouteTableId"
{-# DEPRECATED tgoPropagationDefaultRouteTableId "Use generic-lens or generic-optics with 'propagationDefaultRouteTableId' instead." #-}

-- | Indicates whether Equal Cost Multipath Protocol support is enabled.
--
-- /Note:/ Consider using 'vpnEcmpSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgoVpnEcmpSupport :: Lens.Lens' TransitGatewayOptions (Core.Maybe Types.VpnEcmpSupportValue)
tgoVpnEcmpSupport = Lens.field @"vpnEcmpSupport"
{-# DEPRECATED tgoVpnEcmpSupport "Use generic-lens or generic-optics with 'vpnEcmpSupport' instead." #-}

instance Core.FromXML TransitGatewayOptions where
  parseXML x =
    TransitGatewayOptions'
      Core.<$> (x Core..@? "amazonSideAsn")
      Core.<*> (x Core..@? "associationDefaultRouteTableId")
      Core.<*> (x Core..@? "autoAcceptSharedAttachments")
      Core.<*> (x Core..@? "defaultRouteTableAssociation")
      Core.<*> (x Core..@? "defaultRouteTablePropagation")
      Core.<*> (x Core..@? "dnsSupport")
      Core.<*> (x Core..@? "multicastSupport")
      Core.<*> (x Core..@? "propagationDefaultRouteTableId")
      Core.<*> (x Core..@? "vpnEcmpSupport")
