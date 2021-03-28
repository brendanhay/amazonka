{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayVpcAttachmentOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.TransitGatewayVpcAttachmentOptions
  ( TransitGatewayVpcAttachmentOptions (..)
  -- * Smart constructor
  , mkTransitGatewayVpcAttachmentOptions
  -- * Lenses
  , tgvaoApplianceModeSupport
  , tgvaoDnsSupport
  , tgvaoIpv6Support
  ) where

import qualified Network.AWS.EC2.Types.ApplianceModeSupportValue as Types
import qualified Network.AWS.EC2.Types.DnsSupportValue as Types
import qualified Network.AWS.EC2.Types.Ipv6SupportValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the VPC attachment options.
--
-- /See:/ 'mkTransitGatewayVpcAttachmentOptions' smart constructor.
data TransitGatewayVpcAttachmentOptions = TransitGatewayVpcAttachmentOptions'
  { applianceModeSupport :: Core.Maybe Types.ApplianceModeSupportValue
    -- ^ Indicates whether appliance mode support is enabled.
  , dnsSupport :: Core.Maybe Types.DnsSupportValue
    -- ^ Indicates whether DNS support is enabled.
  , ipv6Support :: Core.Maybe Types.Ipv6SupportValue
    -- ^ Indicates whether IPv6 support is disabled.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TransitGatewayVpcAttachmentOptions' value with any optional fields omitted.
mkTransitGatewayVpcAttachmentOptions
    :: TransitGatewayVpcAttachmentOptions
mkTransitGatewayVpcAttachmentOptions
  = TransitGatewayVpcAttachmentOptions'{applianceModeSupport =
                                          Core.Nothing,
                                        dnsSupport = Core.Nothing, ipv6Support = Core.Nothing}

-- | Indicates whether appliance mode support is enabled.
--
-- /Note:/ Consider using 'applianceModeSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgvaoApplianceModeSupport :: Lens.Lens' TransitGatewayVpcAttachmentOptions (Core.Maybe Types.ApplianceModeSupportValue)
tgvaoApplianceModeSupport = Lens.field @"applianceModeSupport"
{-# INLINEABLE tgvaoApplianceModeSupport #-}
{-# DEPRECATED applianceModeSupport "Use generic-lens or generic-optics with 'applianceModeSupport' instead"  #-}

-- | Indicates whether DNS support is enabled.
--
-- /Note:/ Consider using 'dnsSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgvaoDnsSupport :: Lens.Lens' TransitGatewayVpcAttachmentOptions (Core.Maybe Types.DnsSupportValue)
tgvaoDnsSupport = Lens.field @"dnsSupport"
{-# INLINEABLE tgvaoDnsSupport #-}
{-# DEPRECATED dnsSupport "Use generic-lens or generic-optics with 'dnsSupport' instead"  #-}

-- | Indicates whether IPv6 support is disabled.
--
-- /Note:/ Consider using 'ipv6Support' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgvaoIpv6Support :: Lens.Lens' TransitGatewayVpcAttachmentOptions (Core.Maybe Types.Ipv6SupportValue)
tgvaoIpv6Support = Lens.field @"ipv6Support"
{-# INLINEABLE tgvaoIpv6Support #-}
{-# DEPRECATED ipv6Support "Use generic-lens or generic-optics with 'ipv6Support' instead"  #-}

instance Core.FromXML TransitGatewayVpcAttachmentOptions where
        parseXML x
          = TransitGatewayVpcAttachmentOptions' Core.<$>
              (x Core..@? "applianceModeSupport") Core.<*>
                x Core..@? "dnsSupport"
                Core.<*> x Core..@? "ipv6Support"
