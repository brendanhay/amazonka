{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayVPCAttachmentOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayVPCAttachmentOptions
  ( TransitGatewayVPCAttachmentOptions (..),

    -- * Smart constructor
    mkTransitGatewayVPCAttachmentOptions,

    -- * Lenses
    tgvaoIPv6Support,
    tgvaoApplianceModeSupport,
    tgvaoDNSSupport,
  )
where

import Network.AWS.EC2.Types.ApplianceModeSupportValue
import Network.AWS.EC2.Types.DNSSupportValue
import Network.AWS.EC2.Types.IPv6SupportValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the VPC attachment options.
--
-- /See:/ 'mkTransitGatewayVPCAttachmentOptions' smart constructor.
data TransitGatewayVPCAttachmentOptions = TransitGatewayVPCAttachmentOptions'
  { ipv6Support ::
      Lude.Maybe
        IPv6SupportValue,
    applianceModeSupport ::
      Lude.Maybe
        ApplianceModeSupportValue,
    dnsSupport ::
      Lude.Maybe
        DNSSupportValue
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransitGatewayVPCAttachmentOptions' with the minimum fields required to make a request.
--
-- * 'applianceModeSupport' - Indicates whether appliance mode support is enabled.
-- * 'dnsSupport' - Indicates whether DNS support is enabled.
-- * 'ipv6Support' - Indicates whether IPv6 support is disabled.
mkTransitGatewayVPCAttachmentOptions ::
  TransitGatewayVPCAttachmentOptions
mkTransitGatewayVPCAttachmentOptions =
  TransitGatewayVPCAttachmentOptions'
    { ipv6Support = Lude.Nothing,
      applianceModeSupport = Lude.Nothing,
      dnsSupport = Lude.Nothing
    }

-- | Indicates whether IPv6 support is disabled.
--
-- /Note:/ Consider using 'ipv6Support' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgvaoIPv6Support :: Lens.Lens' TransitGatewayVPCAttachmentOptions (Lude.Maybe IPv6SupportValue)
tgvaoIPv6Support = Lens.lens (ipv6Support :: TransitGatewayVPCAttachmentOptions -> Lude.Maybe IPv6SupportValue) (\s a -> s {ipv6Support = a} :: TransitGatewayVPCAttachmentOptions)
{-# DEPRECATED tgvaoIPv6Support "Use generic-lens or generic-optics with 'ipv6Support' instead." #-}

-- | Indicates whether appliance mode support is enabled.
--
-- /Note:/ Consider using 'applianceModeSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgvaoApplianceModeSupport :: Lens.Lens' TransitGatewayVPCAttachmentOptions (Lude.Maybe ApplianceModeSupportValue)
tgvaoApplianceModeSupport = Lens.lens (applianceModeSupport :: TransitGatewayVPCAttachmentOptions -> Lude.Maybe ApplianceModeSupportValue) (\s a -> s {applianceModeSupport = a} :: TransitGatewayVPCAttachmentOptions)
{-# DEPRECATED tgvaoApplianceModeSupport "Use generic-lens or generic-optics with 'applianceModeSupport' instead." #-}

-- | Indicates whether DNS support is enabled.
--
-- /Note:/ Consider using 'dnsSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgvaoDNSSupport :: Lens.Lens' TransitGatewayVPCAttachmentOptions (Lude.Maybe DNSSupportValue)
tgvaoDNSSupport = Lens.lens (dnsSupport :: TransitGatewayVPCAttachmentOptions -> Lude.Maybe DNSSupportValue) (\s a -> s {dnsSupport = a} :: TransitGatewayVPCAttachmentOptions)
{-# DEPRECATED tgvaoDNSSupport "Use generic-lens or generic-optics with 'dnsSupport' instead." #-}

instance Lude.FromXML TransitGatewayVPCAttachmentOptions where
  parseXML x =
    TransitGatewayVPCAttachmentOptions'
      Lude.<$> (x Lude..@? "ipv6Support")
      Lude.<*> (x Lude..@? "applianceModeSupport")
      Lude.<*> (x Lude..@? "dnsSupport")
