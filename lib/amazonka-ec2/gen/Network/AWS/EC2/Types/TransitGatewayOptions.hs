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
    tgoVPNEcmpSupport,
    tgoAutoAcceptSharedAttachments,
    tgoPropagationDefaultRouteTableId,
    tgoDefaultRouteTableAssociation,
    tgoAssociationDefaultRouteTableId,
    tgoAmazonSideASN,
    tgoDefaultRouteTablePropagation,
    tgoMulticastSupport,
    tgoDNSSupport,
  )
where

import Network.AWS.EC2.Types.AutoAcceptSharedAttachmentsValue
import Network.AWS.EC2.Types.DNSSupportValue
import Network.AWS.EC2.Types.DefaultRouteTableAssociationValue
import Network.AWS.EC2.Types.DefaultRouteTablePropagationValue
import Network.AWS.EC2.Types.MulticastSupportValue
import Network.AWS.EC2.Types.VPNEcmpSupportValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the options for a transit gateway.
--
-- /See:/ 'mkTransitGatewayOptions' smart constructor.
data TransitGatewayOptions = TransitGatewayOptions'
  { vpnEcmpSupport ::
      Lude.Maybe VPNEcmpSupportValue,
    autoAcceptSharedAttachments ::
      Lude.Maybe AutoAcceptSharedAttachmentsValue,
    propagationDefaultRouteTableId ::
      Lude.Maybe Lude.Text,
    defaultRouteTableAssociation ::
      Lude.Maybe DefaultRouteTableAssociationValue,
    associationDefaultRouteTableId ::
      Lude.Maybe Lude.Text,
    amazonSideASN :: Lude.Maybe Lude.Integer,
    defaultRouteTablePropagation ::
      Lude.Maybe DefaultRouteTablePropagationValue,
    multicastSupport ::
      Lude.Maybe MulticastSupportValue,
    dnsSupport :: Lude.Maybe DNSSupportValue
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransitGatewayOptions' with the minimum fields required to make a request.
--
-- * 'amazonSideASN' - A private Autonomous System Number (ASN) for the Amazon side of a BGP session. The range is 64512 to 65534 for 16-bit ASNs and 4200000000 to 4294967294 for 32-bit ASNs.
-- * 'associationDefaultRouteTableId' - The ID of the default association route table.
-- * 'autoAcceptSharedAttachments' - Indicates whether attachment requests are automatically accepted.
-- * 'defaultRouteTableAssociation' - Indicates whether resource attachments are automatically associated with the default association route table.
-- * 'defaultRouteTablePropagation' - Indicates whether resource attachments automatically propagate routes to the default propagation route table.
-- * 'dnsSupport' - Indicates whether DNS support is enabled.
-- * 'multicastSupport' - Indicates whether multicast is enabled on the transit gateway
-- * 'propagationDefaultRouteTableId' - The ID of the default propagation route table.
-- * 'vpnEcmpSupport' - Indicates whether Equal Cost Multipath Protocol support is enabled.
mkTransitGatewayOptions ::
  TransitGatewayOptions
mkTransitGatewayOptions =
  TransitGatewayOptions'
    { vpnEcmpSupport = Lude.Nothing,
      autoAcceptSharedAttachments = Lude.Nothing,
      propagationDefaultRouteTableId = Lude.Nothing,
      defaultRouteTableAssociation = Lude.Nothing,
      associationDefaultRouteTableId = Lude.Nothing,
      amazonSideASN = Lude.Nothing,
      defaultRouteTablePropagation = Lude.Nothing,
      multicastSupport = Lude.Nothing,
      dnsSupport = Lude.Nothing
    }

-- | Indicates whether Equal Cost Multipath Protocol support is enabled.
--
-- /Note:/ Consider using 'vpnEcmpSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgoVPNEcmpSupport :: Lens.Lens' TransitGatewayOptions (Lude.Maybe VPNEcmpSupportValue)
tgoVPNEcmpSupport = Lens.lens (vpnEcmpSupport :: TransitGatewayOptions -> Lude.Maybe VPNEcmpSupportValue) (\s a -> s {vpnEcmpSupport = a} :: TransitGatewayOptions)
{-# DEPRECATED tgoVPNEcmpSupport "Use generic-lens or generic-optics with 'vpnEcmpSupport' instead." #-}

-- | Indicates whether attachment requests are automatically accepted.
--
-- /Note:/ Consider using 'autoAcceptSharedAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgoAutoAcceptSharedAttachments :: Lens.Lens' TransitGatewayOptions (Lude.Maybe AutoAcceptSharedAttachmentsValue)
tgoAutoAcceptSharedAttachments = Lens.lens (autoAcceptSharedAttachments :: TransitGatewayOptions -> Lude.Maybe AutoAcceptSharedAttachmentsValue) (\s a -> s {autoAcceptSharedAttachments = a} :: TransitGatewayOptions)
{-# DEPRECATED tgoAutoAcceptSharedAttachments "Use generic-lens or generic-optics with 'autoAcceptSharedAttachments' instead." #-}

-- | The ID of the default propagation route table.
--
-- /Note:/ Consider using 'propagationDefaultRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgoPropagationDefaultRouteTableId :: Lens.Lens' TransitGatewayOptions (Lude.Maybe Lude.Text)
tgoPropagationDefaultRouteTableId = Lens.lens (propagationDefaultRouteTableId :: TransitGatewayOptions -> Lude.Maybe Lude.Text) (\s a -> s {propagationDefaultRouteTableId = a} :: TransitGatewayOptions)
{-# DEPRECATED tgoPropagationDefaultRouteTableId "Use generic-lens or generic-optics with 'propagationDefaultRouteTableId' instead." #-}

-- | Indicates whether resource attachments are automatically associated with the default association route table.
--
-- /Note:/ Consider using 'defaultRouteTableAssociation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgoDefaultRouteTableAssociation :: Lens.Lens' TransitGatewayOptions (Lude.Maybe DefaultRouteTableAssociationValue)
tgoDefaultRouteTableAssociation = Lens.lens (defaultRouteTableAssociation :: TransitGatewayOptions -> Lude.Maybe DefaultRouteTableAssociationValue) (\s a -> s {defaultRouteTableAssociation = a} :: TransitGatewayOptions)
{-# DEPRECATED tgoDefaultRouteTableAssociation "Use generic-lens or generic-optics with 'defaultRouteTableAssociation' instead." #-}

-- | The ID of the default association route table.
--
-- /Note:/ Consider using 'associationDefaultRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgoAssociationDefaultRouteTableId :: Lens.Lens' TransitGatewayOptions (Lude.Maybe Lude.Text)
tgoAssociationDefaultRouteTableId = Lens.lens (associationDefaultRouteTableId :: TransitGatewayOptions -> Lude.Maybe Lude.Text) (\s a -> s {associationDefaultRouteTableId = a} :: TransitGatewayOptions)
{-# DEPRECATED tgoAssociationDefaultRouteTableId "Use generic-lens or generic-optics with 'associationDefaultRouteTableId' instead." #-}

-- | A private Autonomous System Number (ASN) for the Amazon side of a BGP session. The range is 64512 to 65534 for 16-bit ASNs and 4200000000 to 4294967294 for 32-bit ASNs.
--
-- /Note:/ Consider using 'amazonSideASN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgoAmazonSideASN :: Lens.Lens' TransitGatewayOptions (Lude.Maybe Lude.Integer)
tgoAmazonSideASN = Lens.lens (amazonSideASN :: TransitGatewayOptions -> Lude.Maybe Lude.Integer) (\s a -> s {amazonSideASN = a} :: TransitGatewayOptions)
{-# DEPRECATED tgoAmazonSideASN "Use generic-lens or generic-optics with 'amazonSideASN' instead." #-}

-- | Indicates whether resource attachments automatically propagate routes to the default propagation route table.
--
-- /Note:/ Consider using 'defaultRouteTablePropagation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgoDefaultRouteTablePropagation :: Lens.Lens' TransitGatewayOptions (Lude.Maybe DefaultRouteTablePropagationValue)
tgoDefaultRouteTablePropagation = Lens.lens (defaultRouteTablePropagation :: TransitGatewayOptions -> Lude.Maybe DefaultRouteTablePropagationValue) (\s a -> s {defaultRouteTablePropagation = a} :: TransitGatewayOptions)
{-# DEPRECATED tgoDefaultRouteTablePropagation "Use generic-lens or generic-optics with 'defaultRouteTablePropagation' instead." #-}

-- | Indicates whether multicast is enabled on the transit gateway
--
-- /Note:/ Consider using 'multicastSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgoMulticastSupport :: Lens.Lens' TransitGatewayOptions (Lude.Maybe MulticastSupportValue)
tgoMulticastSupport = Lens.lens (multicastSupport :: TransitGatewayOptions -> Lude.Maybe MulticastSupportValue) (\s a -> s {multicastSupport = a} :: TransitGatewayOptions)
{-# DEPRECATED tgoMulticastSupport "Use generic-lens or generic-optics with 'multicastSupport' instead." #-}

-- | Indicates whether DNS support is enabled.
--
-- /Note:/ Consider using 'dnsSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgoDNSSupport :: Lens.Lens' TransitGatewayOptions (Lude.Maybe DNSSupportValue)
tgoDNSSupport = Lens.lens (dnsSupport :: TransitGatewayOptions -> Lude.Maybe DNSSupportValue) (\s a -> s {dnsSupport = a} :: TransitGatewayOptions)
{-# DEPRECATED tgoDNSSupport "Use generic-lens or generic-optics with 'dnsSupport' instead." #-}

instance Lude.FromXML TransitGatewayOptions where
  parseXML x =
    TransitGatewayOptions'
      Lude.<$> (x Lude..@? "vpnEcmpSupport")
      Lude.<*> (x Lude..@? "autoAcceptSharedAttachments")
      Lude.<*> (x Lude..@? "propagationDefaultRouteTableId")
      Lude.<*> (x Lude..@? "defaultRouteTableAssociation")
      Lude.<*> (x Lude..@? "associationDefaultRouteTableId")
      Lude.<*> (x Lude..@? "amazonSideAsn")
      Lude.<*> (x Lude..@? "defaultRouteTablePropagation")
      Lude.<*> (x Lude..@? "multicastSupport")
      Lude.<*> (x Lude..@? "dnsSupport")
