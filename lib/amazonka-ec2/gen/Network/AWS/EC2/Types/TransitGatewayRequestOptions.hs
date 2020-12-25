{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    tgroAmazonSideAsn,
    tgroAutoAcceptSharedAttachments,
    tgroDefaultRouteTableAssociation,
    tgroDefaultRouteTablePropagation,
    tgroDnsSupport,
    tgroMulticastSupport,
    tgroVpnEcmpSupport,
  )
where

import qualified Network.AWS.EC2.Types.AutoAcceptSharedAttachmentsValue as Types
import qualified Network.AWS.EC2.Types.DefaultRouteTableAssociationValue as Types
import qualified Network.AWS.EC2.Types.DefaultRouteTablePropagationValue as Types
import qualified Network.AWS.EC2.Types.DnsSupportValue as Types
import qualified Network.AWS.EC2.Types.MulticastSupportValue as Types
import qualified Network.AWS.EC2.Types.VpnEcmpSupportValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the options for a transit gateway.
--
-- /See:/ 'mkTransitGatewayRequestOptions' smart constructor.
data TransitGatewayRequestOptions = TransitGatewayRequestOptions'
  { -- | A private Autonomous System Number (ASN) for the Amazon side of a BGP session. The range is 64512 to 65534 for 16-bit ASNs and 4200000000 to 4294967294 for 32-bit ASNs. The default is @64512@ .
    amazonSideAsn :: Core.Maybe Core.Integer,
    -- | Enable or disable automatic acceptance of attachment requests. Disabled by default.
    autoAcceptSharedAttachments :: Core.Maybe Types.AutoAcceptSharedAttachmentsValue,
    -- | Enable or disable automatic association with the default association route table. Enabled by default.
    defaultRouteTableAssociation :: Core.Maybe Types.DefaultRouteTableAssociationValue,
    -- | Enable or disable automatic propagation of routes to the default propagation route table. Enabled by default.
    defaultRouteTablePropagation :: Core.Maybe Types.DefaultRouteTablePropagationValue,
    -- | Enable or disable DNS support. Enabled by default.
    dnsSupport :: Core.Maybe Types.DnsSupportValue,
    -- | Indicates whether multicast is enabled on the transit gateway
    multicastSupport :: Core.Maybe Types.MulticastSupportValue,
    -- | Enable or disable Equal Cost Multipath Protocol support. Enabled by default.
    vpnEcmpSupport :: Core.Maybe Types.VpnEcmpSupportValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TransitGatewayRequestOptions' value with any optional fields omitted.
mkTransitGatewayRequestOptions ::
  TransitGatewayRequestOptions
mkTransitGatewayRequestOptions =
  TransitGatewayRequestOptions'
    { amazonSideAsn = Core.Nothing,
      autoAcceptSharedAttachments = Core.Nothing,
      defaultRouteTableAssociation = Core.Nothing,
      defaultRouteTablePropagation = Core.Nothing,
      dnsSupport = Core.Nothing,
      multicastSupport = Core.Nothing,
      vpnEcmpSupport = Core.Nothing
    }

-- | A private Autonomous System Number (ASN) for the Amazon side of a BGP session. The range is 64512 to 65534 for 16-bit ASNs and 4200000000 to 4294967294 for 32-bit ASNs. The default is @64512@ .
--
-- /Note:/ Consider using 'amazonSideAsn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgroAmazonSideAsn :: Lens.Lens' TransitGatewayRequestOptions (Core.Maybe Core.Integer)
tgroAmazonSideAsn = Lens.field @"amazonSideAsn"
{-# DEPRECATED tgroAmazonSideAsn "Use generic-lens or generic-optics with 'amazonSideAsn' instead." #-}

-- | Enable or disable automatic acceptance of attachment requests. Disabled by default.
--
-- /Note:/ Consider using 'autoAcceptSharedAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgroAutoAcceptSharedAttachments :: Lens.Lens' TransitGatewayRequestOptions (Core.Maybe Types.AutoAcceptSharedAttachmentsValue)
tgroAutoAcceptSharedAttachments = Lens.field @"autoAcceptSharedAttachments"
{-# DEPRECATED tgroAutoAcceptSharedAttachments "Use generic-lens or generic-optics with 'autoAcceptSharedAttachments' instead." #-}

-- | Enable or disable automatic association with the default association route table. Enabled by default.
--
-- /Note:/ Consider using 'defaultRouteTableAssociation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgroDefaultRouteTableAssociation :: Lens.Lens' TransitGatewayRequestOptions (Core.Maybe Types.DefaultRouteTableAssociationValue)
tgroDefaultRouteTableAssociation = Lens.field @"defaultRouteTableAssociation"
{-# DEPRECATED tgroDefaultRouteTableAssociation "Use generic-lens or generic-optics with 'defaultRouteTableAssociation' instead." #-}

-- | Enable or disable automatic propagation of routes to the default propagation route table. Enabled by default.
--
-- /Note:/ Consider using 'defaultRouteTablePropagation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgroDefaultRouteTablePropagation :: Lens.Lens' TransitGatewayRequestOptions (Core.Maybe Types.DefaultRouteTablePropagationValue)
tgroDefaultRouteTablePropagation = Lens.field @"defaultRouteTablePropagation"
{-# DEPRECATED tgroDefaultRouteTablePropagation "Use generic-lens or generic-optics with 'defaultRouteTablePropagation' instead." #-}

-- | Enable or disable DNS support. Enabled by default.
--
-- /Note:/ Consider using 'dnsSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgroDnsSupport :: Lens.Lens' TransitGatewayRequestOptions (Core.Maybe Types.DnsSupportValue)
tgroDnsSupport = Lens.field @"dnsSupport"
{-# DEPRECATED tgroDnsSupport "Use generic-lens or generic-optics with 'dnsSupport' instead." #-}

-- | Indicates whether multicast is enabled on the transit gateway
--
-- /Note:/ Consider using 'multicastSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgroMulticastSupport :: Lens.Lens' TransitGatewayRequestOptions (Core.Maybe Types.MulticastSupportValue)
tgroMulticastSupport = Lens.field @"multicastSupport"
{-# DEPRECATED tgroMulticastSupport "Use generic-lens or generic-optics with 'multicastSupport' instead." #-}

-- | Enable or disable Equal Cost Multipath Protocol support. Enabled by default.
--
-- /Note:/ Consider using 'vpnEcmpSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgroVpnEcmpSupport :: Lens.Lens' TransitGatewayRequestOptions (Core.Maybe Types.VpnEcmpSupportValue)
tgroVpnEcmpSupport = Lens.field @"vpnEcmpSupport"
{-# DEPRECATED tgroVpnEcmpSupport "Use generic-lens or generic-optics with 'vpnEcmpSupport' instead." #-}
