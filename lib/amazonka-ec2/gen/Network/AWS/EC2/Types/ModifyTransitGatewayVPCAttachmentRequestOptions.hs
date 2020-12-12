{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ModifyTransitGatewayVPCAttachmentRequestOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ModifyTransitGatewayVPCAttachmentRequestOptions
  ( ModifyTransitGatewayVPCAttachmentRequestOptions (..),

    -- * Smart constructor
    mkModifyTransitGatewayVPCAttachmentRequestOptions,

    -- * Lenses
    mtgvaroIPv6Support,
    mtgvaroApplianceModeSupport,
    mtgvaroDNSSupport,
  )
where

import Network.AWS.EC2.Types.ApplianceModeSupportValue
import Network.AWS.EC2.Types.DNSSupportValue
import Network.AWS.EC2.Types.IPv6SupportValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the options for a VPC attachment.
--
-- /See:/ 'mkModifyTransitGatewayVPCAttachmentRequestOptions' smart constructor.
data ModifyTransitGatewayVPCAttachmentRequestOptions = ModifyTransitGatewayVPCAttachmentRequestOptions'
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
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'ModifyTransitGatewayVPCAttachmentRequestOptions' with the minimum fields required to make a request.
--
-- * 'applianceModeSupport' - Enable or disable support for appliance mode. If enabled, a traffic flow between a source and destination uses the same Availability Zone for the VPC attachment for the lifetime of that flow. The default is @disable@ .
-- * 'dnsSupport' - Enable or disable DNS support. The default is @enable@ .
-- * 'ipv6Support' - Enable or disable IPv6 support. The default is @enable@ .
mkModifyTransitGatewayVPCAttachmentRequestOptions ::
  ModifyTransitGatewayVPCAttachmentRequestOptions
mkModifyTransitGatewayVPCAttachmentRequestOptions =
  ModifyTransitGatewayVPCAttachmentRequestOptions'
    { ipv6Support =
        Lude.Nothing,
      applianceModeSupport = Lude.Nothing,
      dnsSupport = Lude.Nothing
    }

-- | Enable or disable IPv6 support. The default is @enable@ .
--
-- /Note:/ Consider using 'ipv6Support' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgvaroIPv6Support :: Lens.Lens' ModifyTransitGatewayVPCAttachmentRequestOptions (Lude.Maybe IPv6SupportValue)
mtgvaroIPv6Support = Lens.lens (ipv6Support :: ModifyTransitGatewayVPCAttachmentRequestOptions -> Lude.Maybe IPv6SupportValue) (\s a -> s {ipv6Support = a} :: ModifyTransitGatewayVPCAttachmentRequestOptions)
{-# DEPRECATED mtgvaroIPv6Support "Use generic-lens or generic-optics with 'ipv6Support' instead." #-}

-- | Enable or disable support for appliance mode. If enabled, a traffic flow between a source and destination uses the same Availability Zone for the VPC attachment for the lifetime of that flow. The default is @disable@ .
--
-- /Note:/ Consider using 'applianceModeSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgvaroApplianceModeSupport :: Lens.Lens' ModifyTransitGatewayVPCAttachmentRequestOptions (Lude.Maybe ApplianceModeSupportValue)
mtgvaroApplianceModeSupport = Lens.lens (applianceModeSupport :: ModifyTransitGatewayVPCAttachmentRequestOptions -> Lude.Maybe ApplianceModeSupportValue) (\s a -> s {applianceModeSupport = a} :: ModifyTransitGatewayVPCAttachmentRequestOptions)
{-# DEPRECATED mtgvaroApplianceModeSupport "Use generic-lens or generic-optics with 'applianceModeSupport' instead." #-}

-- | Enable or disable DNS support. The default is @enable@ .
--
-- /Note:/ Consider using 'dnsSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgvaroDNSSupport :: Lens.Lens' ModifyTransitGatewayVPCAttachmentRequestOptions (Lude.Maybe DNSSupportValue)
mtgvaroDNSSupport = Lens.lens (dnsSupport :: ModifyTransitGatewayVPCAttachmentRequestOptions -> Lude.Maybe DNSSupportValue) (\s a -> s {dnsSupport = a} :: ModifyTransitGatewayVPCAttachmentRequestOptions)
{-# DEPRECATED mtgvaroDNSSupport "Use generic-lens or generic-optics with 'dnsSupport' instead." #-}

instance
  Lude.ToQuery
    ModifyTransitGatewayVPCAttachmentRequestOptions
  where
  toQuery ModifyTransitGatewayVPCAttachmentRequestOptions' {..} =
    Lude.mconcat
      [ "Ipv6Support" Lude.=: ipv6Support,
        "ApplianceModeSupport" Lude.=: applianceModeSupport,
        "DnsSupport" Lude.=: dnsSupport
      ]
