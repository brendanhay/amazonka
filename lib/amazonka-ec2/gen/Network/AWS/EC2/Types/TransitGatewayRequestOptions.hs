-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayRequestOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayRequestOptions
  ( TransitGatewayRequestOptions (..),

    -- * Smart constructor
    mkTransitGatewayRequestOptions,

    -- * Lenses
    tgroVPNEcmpSupport,
    tgroAutoAcceptSharedAttachments,
    tgroDefaultRouteTableAssociation,
    tgroAmazonSideASN,
    tgroDefaultRouteTablePropagation,
    tgroMulticastSupport,
    tgroDNSSupport,
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
-- /See:/ 'mkTransitGatewayRequestOptions' smart constructor.
data TransitGatewayRequestOptions = TransitGatewayRequestOptions'
  { vpnEcmpSupport ::
      Lude.Maybe VPNEcmpSupportValue,
    autoAcceptSharedAttachments ::
      Lude.Maybe
        AutoAcceptSharedAttachmentsValue,
    defaultRouteTableAssociation ::
      Lude.Maybe
        DefaultRouteTableAssociationValue,
    amazonSideASN ::
      Lude.Maybe Lude.Integer,
    defaultRouteTablePropagation ::
      Lude.Maybe
        DefaultRouteTablePropagationValue,
    multicastSupport ::
      Lude.Maybe MulticastSupportValue,
    dnsSupport ::
      Lude.Maybe DNSSupportValue
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransitGatewayRequestOptions' with the minimum fields required to make a request.
--
-- * 'amazonSideASN' - A private Autonomous System Number (ASN) for the Amazon side of a BGP session. The range is 64512 to 65534 for 16-bit ASNs and 4200000000 to 4294967294 for 32-bit ASNs. The default is @64512@ .
-- * 'autoAcceptSharedAttachments' - Enable or disable automatic acceptance of attachment requests. Disabled by default.
-- * 'defaultRouteTableAssociation' - Enable or disable automatic association with the default association route table. Enabled by default.
-- * 'defaultRouteTablePropagation' - Enable or disable automatic propagation of routes to the default propagation route table. Enabled by default.
-- * 'dnsSupport' - Enable or disable DNS support. Enabled by default.
-- * 'multicastSupport' - Indicates whether multicast is enabled on the transit gateway
-- * 'vpnEcmpSupport' - Enable or disable Equal Cost Multipath Protocol support. Enabled by default.
mkTransitGatewayRequestOptions ::
  TransitGatewayRequestOptions
mkTransitGatewayRequestOptions =
  TransitGatewayRequestOptions'
    { vpnEcmpSupport = Lude.Nothing,
      autoAcceptSharedAttachments = Lude.Nothing,
      defaultRouteTableAssociation = Lude.Nothing,
      amazonSideASN = Lude.Nothing,
      defaultRouteTablePropagation = Lude.Nothing,
      multicastSupport = Lude.Nothing,
      dnsSupport = Lude.Nothing
    }

-- | Enable or disable Equal Cost Multipath Protocol support. Enabled by default.
--
-- /Note:/ Consider using 'vpnEcmpSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgroVPNEcmpSupport :: Lens.Lens' TransitGatewayRequestOptions (Lude.Maybe VPNEcmpSupportValue)
tgroVPNEcmpSupport = Lens.lens (vpnEcmpSupport :: TransitGatewayRequestOptions -> Lude.Maybe VPNEcmpSupportValue) (\s a -> s {vpnEcmpSupport = a} :: TransitGatewayRequestOptions)
{-# DEPRECATED tgroVPNEcmpSupport "Use generic-lens or generic-optics with 'vpnEcmpSupport' instead." #-}

-- | Enable or disable automatic acceptance of attachment requests. Disabled by default.
--
-- /Note:/ Consider using 'autoAcceptSharedAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgroAutoAcceptSharedAttachments :: Lens.Lens' TransitGatewayRequestOptions (Lude.Maybe AutoAcceptSharedAttachmentsValue)
tgroAutoAcceptSharedAttachments = Lens.lens (autoAcceptSharedAttachments :: TransitGatewayRequestOptions -> Lude.Maybe AutoAcceptSharedAttachmentsValue) (\s a -> s {autoAcceptSharedAttachments = a} :: TransitGatewayRequestOptions)
{-# DEPRECATED tgroAutoAcceptSharedAttachments "Use generic-lens or generic-optics with 'autoAcceptSharedAttachments' instead." #-}

-- | Enable or disable automatic association with the default association route table. Enabled by default.
--
-- /Note:/ Consider using 'defaultRouteTableAssociation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgroDefaultRouteTableAssociation :: Lens.Lens' TransitGatewayRequestOptions (Lude.Maybe DefaultRouteTableAssociationValue)
tgroDefaultRouteTableAssociation = Lens.lens (defaultRouteTableAssociation :: TransitGatewayRequestOptions -> Lude.Maybe DefaultRouteTableAssociationValue) (\s a -> s {defaultRouteTableAssociation = a} :: TransitGatewayRequestOptions)
{-# DEPRECATED tgroDefaultRouteTableAssociation "Use generic-lens or generic-optics with 'defaultRouteTableAssociation' instead." #-}

-- | A private Autonomous System Number (ASN) for the Amazon side of a BGP session. The range is 64512 to 65534 for 16-bit ASNs and 4200000000 to 4294967294 for 32-bit ASNs. The default is @64512@ .
--
-- /Note:/ Consider using 'amazonSideASN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgroAmazonSideASN :: Lens.Lens' TransitGatewayRequestOptions (Lude.Maybe Lude.Integer)
tgroAmazonSideASN = Lens.lens (amazonSideASN :: TransitGatewayRequestOptions -> Lude.Maybe Lude.Integer) (\s a -> s {amazonSideASN = a} :: TransitGatewayRequestOptions)
{-# DEPRECATED tgroAmazonSideASN "Use generic-lens or generic-optics with 'amazonSideASN' instead." #-}

-- | Enable or disable automatic propagation of routes to the default propagation route table. Enabled by default.
--
-- /Note:/ Consider using 'defaultRouteTablePropagation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgroDefaultRouteTablePropagation :: Lens.Lens' TransitGatewayRequestOptions (Lude.Maybe DefaultRouteTablePropagationValue)
tgroDefaultRouteTablePropagation = Lens.lens (defaultRouteTablePropagation :: TransitGatewayRequestOptions -> Lude.Maybe DefaultRouteTablePropagationValue) (\s a -> s {defaultRouteTablePropagation = a} :: TransitGatewayRequestOptions)
{-# DEPRECATED tgroDefaultRouteTablePropagation "Use generic-lens or generic-optics with 'defaultRouteTablePropagation' instead." #-}

-- | Indicates whether multicast is enabled on the transit gateway
--
-- /Note:/ Consider using 'multicastSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgroMulticastSupport :: Lens.Lens' TransitGatewayRequestOptions (Lude.Maybe MulticastSupportValue)
tgroMulticastSupport = Lens.lens (multicastSupport :: TransitGatewayRequestOptions -> Lude.Maybe MulticastSupportValue) (\s a -> s {multicastSupport = a} :: TransitGatewayRequestOptions)
{-# DEPRECATED tgroMulticastSupport "Use generic-lens or generic-optics with 'multicastSupport' instead." #-}

-- | Enable or disable DNS support. Enabled by default.
--
-- /Note:/ Consider using 'dnsSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgroDNSSupport :: Lens.Lens' TransitGatewayRequestOptions (Lude.Maybe DNSSupportValue)
tgroDNSSupport = Lens.lens (dnsSupport :: TransitGatewayRequestOptions -> Lude.Maybe DNSSupportValue) (\s a -> s {dnsSupport = a} :: TransitGatewayRequestOptions)
{-# DEPRECATED tgroDNSSupport "Use generic-lens or generic-optics with 'dnsSupport' instead." #-}

instance Lude.ToQuery TransitGatewayRequestOptions where
  toQuery TransitGatewayRequestOptions' {..} =
    Lude.mconcat
      [ "VpnEcmpSupport" Lude.=: vpnEcmpSupport,
        "AutoAcceptSharedAttachments" Lude.=: autoAcceptSharedAttachments,
        "DefaultRouteTableAssociation"
          Lude.=: defaultRouteTableAssociation,
        "AmazonSideAsn" Lude.=: amazonSideASN,
        "DefaultRouteTablePropagation"
          Lude.=: defaultRouteTablePropagation,
        "MulticastSupport" Lude.=: multicastSupport,
        "DnsSupport" Lude.=: dnsSupport
      ]
