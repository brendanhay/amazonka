{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ModifyTransitGatewayVpcAttachmentRequestOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ModifyTransitGatewayVpcAttachmentRequestOptions
  ( ModifyTransitGatewayVpcAttachmentRequestOptions (..)
  -- * Smart constructor
  , mkModifyTransitGatewayVpcAttachmentRequestOptions
  -- * Lenses
  , mtgvaroApplianceModeSupport
  , mtgvaroDnsSupport
  , mtgvaroIpv6Support
  ) where

import qualified Network.AWS.EC2.Types.ApplianceModeSupportValue as Types
import qualified Network.AWS.EC2.Types.DnsSupportValue as Types
import qualified Network.AWS.EC2.Types.Ipv6SupportValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the options for a VPC attachment.
--
-- /See:/ 'mkModifyTransitGatewayVpcAttachmentRequestOptions' smart constructor.
data ModifyTransitGatewayVpcAttachmentRequestOptions = ModifyTransitGatewayVpcAttachmentRequestOptions'
  { applianceModeSupport :: Core.Maybe Types.ApplianceModeSupportValue
    -- ^ Enable or disable support for appliance mode. If enabled, a traffic flow between a source and destination uses the same Availability Zone for the VPC attachment for the lifetime of that flow. The default is @disable@ .
  , dnsSupport :: Core.Maybe Types.DnsSupportValue
    -- ^ Enable or disable DNS support. The default is @enable@ .
  , ipv6Support :: Core.Maybe Types.Ipv6SupportValue
    -- ^ Enable or disable IPv6 support. The default is @enable@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyTransitGatewayVpcAttachmentRequestOptions' value with any optional fields omitted.
mkModifyTransitGatewayVpcAttachmentRequestOptions
    :: ModifyTransitGatewayVpcAttachmentRequestOptions
mkModifyTransitGatewayVpcAttachmentRequestOptions
  = ModifyTransitGatewayVpcAttachmentRequestOptions'{applianceModeSupport
                                                       = Core.Nothing,
                                                     dnsSupport = Core.Nothing,
                                                     ipv6Support = Core.Nothing}

-- | Enable or disable support for appliance mode. If enabled, a traffic flow between a source and destination uses the same Availability Zone for the VPC attachment for the lifetime of that flow. The default is @disable@ .
--
-- /Note:/ Consider using 'applianceModeSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgvaroApplianceModeSupport :: Lens.Lens' ModifyTransitGatewayVpcAttachmentRequestOptions (Core.Maybe Types.ApplianceModeSupportValue)
mtgvaroApplianceModeSupport = Lens.field @"applianceModeSupport"
{-# INLINEABLE mtgvaroApplianceModeSupport #-}
{-# DEPRECATED applianceModeSupport "Use generic-lens or generic-optics with 'applianceModeSupport' instead"  #-}

-- | Enable or disable DNS support. The default is @enable@ .
--
-- /Note:/ Consider using 'dnsSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgvaroDnsSupport :: Lens.Lens' ModifyTransitGatewayVpcAttachmentRequestOptions (Core.Maybe Types.DnsSupportValue)
mtgvaroDnsSupport = Lens.field @"dnsSupport"
{-# INLINEABLE mtgvaroDnsSupport #-}
{-# DEPRECATED dnsSupport "Use generic-lens or generic-optics with 'dnsSupport' instead"  #-}

-- | Enable or disable IPv6 support. The default is @enable@ .
--
-- /Note:/ Consider using 'ipv6Support' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgvaroIpv6Support :: Lens.Lens' ModifyTransitGatewayVpcAttachmentRequestOptions (Core.Maybe Types.Ipv6SupportValue)
mtgvaroIpv6Support = Lens.field @"ipv6Support"
{-# INLINEABLE mtgvaroIpv6Support #-}
{-# DEPRECATED ipv6Support "Use generic-lens or generic-optics with 'ipv6Support' instead"  #-}

instance Core.ToQuery
           ModifyTransitGatewayVpcAttachmentRequestOptions
         where
        toQuery ModifyTransitGatewayVpcAttachmentRequestOptions{..}
          = Core.maybe Core.mempty (Core.toQueryPair "ApplianceModeSupport")
              applianceModeSupport
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DnsSupport") dnsSupport
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Ipv6Support") ipv6Support
