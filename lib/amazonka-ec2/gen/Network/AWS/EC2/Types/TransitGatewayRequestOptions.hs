{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayRequestOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.TransitGatewayRequestOptions
  ( TransitGatewayRequestOptions (..)
  -- * Smart constructor
  , mkTransitGatewayRequestOptions
  -- * Lenses
  , tgroAmazonSideAsn
  , tgroAutoAcceptSharedAttachments
  , tgroDefaultRouteTableAssociation
  , tgroDefaultRouteTablePropagation
  , tgroDnsSupport
  , tgroMulticastSupport
  , tgroVpnEcmpSupport
  ) where

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
  { amazonSideAsn :: Core.Maybe Core.Integer
    -- ^ A private Autonomous System Number (ASN) for the Amazon side of a BGP session. The range is 64512 to 65534 for 16-bit ASNs and 4200000000 to 4294967294 for 32-bit ASNs. The default is @64512@ .
  , autoAcceptSharedAttachments :: Core.Maybe Types.AutoAcceptSharedAttachmentsValue
    -- ^ Enable or disable automatic acceptance of attachment requests. Disabled by default.
  , defaultRouteTableAssociation :: Core.Maybe Types.DefaultRouteTableAssociationValue
    -- ^ Enable or disable automatic association with the default association route table. Enabled by default.
  , defaultRouteTablePropagation :: Core.Maybe Types.DefaultRouteTablePropagationValue
    -- ^ Enable or disable automatic propagation of routes to the default propagation route table. Enabled by default.
  , dnsSupport :: Core.Maybe Types.DnsSupportValue
    -- ^ Enable or disable DNS support. Enabled by default.
  , multicastSupport :: Core.Maybe Types.MulticastSupportValue
    -- ^ Indicates whether multicast is enabled on the transit gateway
  , vpnEcmpSupport :: Core.Maybe Types.VpnEcmpSupportValue
    -- ^ Enable or disable Equal Cost Multipath Protocol support. Enabled by default.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TransitGatewayRequestOptions' value with any optional fields omitted.
mkTransitGatewayRequestOptions
    :: TransitGatewayRequestOptions
mkTransitGatewayRequestOptions
  = TransitGatewayRequestOptions'{amazonSideAsn = Core.Nothing,
                                  autoAcceptSharedAttachments = Core.Nothing,
                                  defaultRouteTableAssociation = Core.Nothing,
                                  defaultRouteTablePropagation = Core.Nothing,
                                  dnsSupport = Core.Nothing, multicastSupport = Core.Nothing,
                                  vpnEcmpSupport = Core.Nothing}

-- | A private Autonomous System Number (ASN) for the Amazon side of a BGP session. The range is 64512 to 65534 for 16-bit ASNs and 4200000000 to 4294967294 for 32-bit ASNs. The default is @64512@ .
--
-- /Note:/ Consider using 'amazonSideAsn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgroAmazonSideAsn :: Lens.Lens' TransitGatewayRequestOptions (Core.Maybe Core.Integer)
tgroAmazonSideAsn = Lens.field @"amazonSideAsn"
{-# INLINEABLE tgroAmazonSideAsn #-}
{-# DEPRECATED amazonSideAsn "Use generic-lens or generic-optics with 'amazonSideAsn' instead"  #-}

-- | Enable or disable automatic acceptance of attachment requests. Disabled by default.
--
-- /Note:/ Consider using 'autoAcceptSharedAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgroAutoAcceptSharedAttachments :: Lens.Lens' TransitGatewayRequestOptions (Core.Maybe Types.AutoAcceptSharedAttachmentsValue)
tgroAutoAcceptSharedAttachments = Lens.field @"autoAcceptSharedAttachments"
{-# INLINEABLE tgroAutoAcceptSharedAttachments #-}
{-# DEPRECATED autoAcceptSharedAttachments "Use generic-lens or generic-optics with 'autoAcceptSharedAttachments' instead"  #-}

-- | Enable or disable automatic association with the default association route table. Enabled by default.
--
-- /Note:/ Consider using 'defaultRouteTableAssociation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgroDefaultRouteTableAssociation :: Lens.Lens' TransitGatewayRequestOptions (Core.Maybe Types.DefaultRouteTableAssociationValue)
tgroDefaultRouteTableAssociation = Lens.field @"defaultRouteTableAssociation"
{-# INLINEABLE tgroDefaultRouteTableAssociation #-}
{-# DEPRECATED defaultRouteTableAssociation "Use generic-lens or generic-optics with 'defaultRouteTableAssociation' instead"  #-}

-- | Enable or disable automatic propagation of routes to the default propagation route table. Enabled by default.
--
-- /Note:/ Consider using 'defaultRouteTablePropagation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgroDefaultRouteTablePropagation :: Lens.Lens' TransitGatewayRequestOptions (Core.Maybe Types.DefaultRouteTablePropagationValue)
tgroDefaultRouteTablePropagation = Lens.field @"defaultRouteTablePropagation"
{-# INLINEABLE tgroDefaultRouteTablePropagation #-}
{-# DEPRECATED defaultRouteTablePropagation "Use generic-lens or generic-optics with 'defaultRouteTablePropagation' instead"  #-}

-- | Enable or disable DNS support. Enabled by default.
--
-- /Note:/ Consider using 'dnsSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgroDnsSupport :: Lens.Lens' TransitGatewayRequestOptions (Core.Maybe Types.DnsSupportValue)
tgroDnsSupport = Lens.field @"dnsSupport"
{-# INLINEABLE tgroDnsSupport #-}
{-# DEPRECATED dnsSupport "Use generic-lens or generic-optics with 'dnsSupport' instead"  #-}

-- | Indicates whether multicast is enabled on the transit gateway
--
-- /Note:/ Consider using 'multicastSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgroMulticastSupport :: Lens.Lens' TransitGatewayRequestOptions (Core.Maybe Types.MulticastSupportValue)
tgroMulticastSupport = Lens.field @"multicastSupport"
{-# INLINEABLE tgroMulticastSupport #-}
{-# DEPRECATED multicastSupport "Use generic-lens or generic-optics with 'multicastSupport' instead"  #-}

-- | Enable or disable Equal Cost Multipath Protocol support. Enabled by default.
--
-- /Note:/ Consider using 'vpnEcmpSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgroVpnEcmpSupport :: Lens.Lens' TransitGatewayRequestOptions (Core.Maybe Types.VpnEcmpSupportValue)
tgroVpnEcmpSupport = Lens.field @"vpnEcmpSupport"
{-# INLINEABLE tgroVpnEcmpSupport #-}
{-# DEPRECATED vpnEcmpSupport "Use generic-lens or generic-optics with 'vpnEcmpSupport' instead"  #-}

instance Core.ToQuery TransitGatewayRequestOptions where
        toQuery TransitGatewayRequestOptions{..}
          = Core.maybe Core.mempty (Core.toQueryPair "AmazonSideAsn")
              amazonSideAsn
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "AutoAcceptSharedAttachments")
                autoAcceptSharedAttachments
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "DefaultRouteTableAssociation")
                defaultRouteTableAssociation
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "DefaultRouteTablePropagation")
                defaultRouteTablePropagation
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DnsSupport") dnsSupport
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MulticastSupport")
                multicastSupport
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "VpnEcmpSupport")
                vpnEcmpSupport
