-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CreateTransitGatewayVPCAttachmentRequestOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CreateTransitGatewayVPCAttachmentRequestOptions
  ( CreateTransitGatewayVPCAttachmentRequestOptions (..),

    -- * Smart constructor
    mkCreateTransitGatewayVPCAttachmentRequestOptions,

    -- * Lenses
    ctgvaroIPv6Support,
    ctgvaroApplianceModeSupport,
    ctgvaroDNSSupport,
  )
where

import Network.AWS.EC2.Types.ApplianceModeSupportValue
import Network.AWS.EC2.Types.DNSSupportValue
import Network.AWS.EC2.Types.IPv6SupportValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the options for a VPC attachment.
--
-- /See:/ 'mkCreateTransitGatewayVPCAttachmentRequestOptions' smart constructor.
data CreateTransitGatewayVPCAttachmentRequestOptions = CreateTransitGatewayVPCAttachmentRequestOptions'
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

-- | Creates a value of 'CreateTransitGatewayVPCAttachmentRequestOptions' with the minimum fields required to make a request.
--
-- * 'applianceModeSupport' - Enable or disable support for appliance mode. If enabled, a traffic flow between a source and destination uses the same Availability Zone for the VPC attachment for the lifetime of that flow. The default is @disable@ .
-- * 'dnsSupport' - Enable or disable DNS support. The default is @enable@ .
-- * 'ipv6Support' - Enable or disable IPv6 support.
mkCreateTransitGatewayVPCAttachmentRequestOptions ::
  CreateTransitGatewayVPCAttachmentRequestOptions
mkCreateTransitGatewayVPCAttachmentRequestOptions =
  CreateTransitGatewayVPCAttachmentRequestOptions'
    { ipv6Support =
        Lude.Nothing,
      applianceModeSupport = Lude.Nothing,
      dnsSupport = Lude.Nothing
    }

-- | Enable or disable IPv6 support.
--
-- /Note:/ Consider using 'ipv6Support' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgvaroIPv6Support :: Lens.Lens' CreateTransitGatewayVPCAttachmentRequestOptions (Lude.Maybe IPv6SupportValue)
ctgvaroIPv6Support = Lens.lens (ipv6Support :: CreateTransitGatewayVPCAttachmentRequestOptions -> Lude.Maybe IPv6SupportValue) (\s a -> s {ipv6Support = a} :: CreateTransitGatewayVPCAttachmentRequestOptions)
{-# DEPRECATED ctgvaroIPv6Support "Use generic-lens or generic-optics with 'ipv6Support' instead." #-}

-- | Enable or disable support for appliance mode. If enabled, a traffic flow between a source and destination uses the same Availability Zone for the VPC attachment for the lifetime of that flow. The default is @disable@ .
--
-- /Note:/ Consider using 'applianceModeSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgvaroApplianceModeSupport :: Lens.Lens' CreateTransitGatewayVPCAttachmentRequestOptions (Lude.Maybe ApplianceModeSupportValue)
ctgvaroApplianceModeSupport = Lens.lens (applianceModeSupport :: CreateTransitGatewayVPCAttachmentRequestOptions -> Lude.Maybe ApplianceModeSupportValue) (\s a -> s {applianceModeSupport = a} :: CreateTransitGatewayVPCAttachmentRequestOptions)
{-# DEPRECATED ctgvaroApplianceModeSupport "Use generic-lens or generic-optics with 'applianceModeSupport' instead." #-}

-- | Enable or disable DNS support. The default is @enable@ .
--
-- /Note:/ Consider using 'dnsSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgvaroDNSSupport :: Lens.Lens' CreateTransitGatewayVPCAttachmentRequestOptions (Lude.Maybe DNSSupportValue)
ctgvaroDNSSupport = Lens.lens (dnsSupport :: CreateTransitGatewayVPCAttachmentRequestOptions -> Lude.Maybe DNSSupportValue) (\s a -> s {dnsSupport = a} :: CreateTransitGatewayVPCAttachmentRequestOptions)
{-# DEPRECATED ctgvaroDNSSupport "Use generic-lens or generic-optics with 'dnsSupport' instead." #-}

instance
  Lude.ToQuery
    CreateTransitGatewayVPCAttachmentRequestOptions
  where
  toQuery CreateTransitGatewayVPCAttachmentRequestOptions' {..} =
    Lude.mconcat
      [ "Ipv6Support" Lude.=: ipv6Support,
        "ApplianceModeSupport" Lude.=: applianceModeSupport,
        "DnsSupport" Lude.=: dnsSupport
      ]
