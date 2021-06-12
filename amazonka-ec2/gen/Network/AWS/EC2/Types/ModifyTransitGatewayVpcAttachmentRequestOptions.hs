{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ModifyTransitGatewayVpcAttachmentRequestOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ModifyTransitGatewayVpcAttachmentRequestOptions where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ApplianceModeSupportValue
import Network.AWS.EC2.Types.DnsSupportValue
import Network.AWS.EC2.Types.Ipv6SupportValue
import qualified Network.AWS.Lens as Lens

-- | Describes the options for a VPC attachment.
--
-- /See:/ 'newModifyTransitGatewayVpcAttachmentRequestOptions' smart constructor.
data ModifyTransitGatewayVpcAttachmentRequestOptions = ModifyTransitGatewayVpcAttachmentRequestOptions'
  { -- | Enable or disable support for appliance mode. If enabled, a traffic flow
    -- between a source and destination uses the same Availability Zone for the
    -- VPC attachment for the lifetime of that flow. The default is @disable@.
    applianceModeSupport :: Core.Maybe ApplianceModeSupportValue,
    -- | Enable or disable DNS support. The default is @enable@.
    dnsSupport :: Core.Maybe DnsSupportValue,
    -- | Enable or disable IPv6 support. The default is @enable@.
    ipv6Support :: Core.Maybe Ipv6SupportValue
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyTransitGatewayVpcAttachmentRequestOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applianceModeSupport', 'modifyTransitGatewayVpcAttachmentRequestOptions_applianceModeSupport' - Enable or disable support for appliance mode. If enabled, a traffic flow
-- between a source and destination uses the same Availability Zone for the
-- VPC attachment for the lifetime of that flow. The default is @disable@.
--
-- 'dnsSupport', 'modifyTransitGatewayVpcAttachmentRequestOptions_dnsSupport' - Enable or disable DNS support. The default is @enable@.
--
-- 'ipv6Support', 'modifyTransitGatewayVpcAttachmentRequestOptions_ipv6Support' - Enable or disable IPv6 support. The default is @enable@.
newModifyTransitGatewayVpcAttachmentRequestOptions ::
  ModifyTransitGatewayVpcAttachmentRequestOptions
newModifyTransitGatewayVpcAttachmentRequestOptions =
  ModifyTransitGatewayVpcAttachmentRequestOptions'
    { applianceModeSupport =
        Core.Nothing,
      dnsSupport = Core.Nothing,
      ipv6Support = Core.Nothing
    }

-- | Enable or disable support for appliance mode. If enabled, a traffic flow
-- between a source and destination uses the same Availability Zone for the
-- VPC attachment for the lifetime of that flow. The default is @disable@.
modifyTransitGatewayVpcAttachmentRequestOptions_applianceModeSupport :: Lens.Lens' ModifyTransitGatewayVpcAttachmentRequestOptions (Core.Maybe ApplianceModeSupportValue)
modifyTransitGatewayVpcAttachmentRequestOptions_applianceModeSupport = Lens.lens (\ModifyTransitGatewayVpcAttachmentRequestOptions' {applianceModeSupport} -> applianceModeSupport) (\s@ModifyTransitGatewayVpcAttachmentRequestOptions' {} a -> s {applianceModeSupport = a} :: ModifyTransitGatewayVpcAttachmentRequestOptions)

-- | Enable or disable DNS support. The default is @enable@.
modifyTransitGatewayVpcAttachmentRequestOptions_dnsSupport :: Lens.Lens' ModifyTransitGatewayVpcAttachmentRequestOptions (Core.Maybe DnsSupportValue)
modifyTransitGatewayVpcAttachmentRequestOptions_dnsSupport = Lens.lens (\ModifyTransitGatewayVpcAttachmentRequestOptions' {dnsSupport} -> dnsSupport) (\s@ModifyTransitGatewayVpcAttachmentRequestOptions' {} a -> s {dnsSupport = a} :: ModifyTransitGatewayVpcAttachmentRequestOptions)

-- | Enable or disable IPv6 support. The default is @enable@.
modifyTransitGatewayVpcAttachmentRequestOptions_ipv6Support :: Lens.Lens' ModifyTransitGatewayVpcAttachmentRequestOptions (Core.Maybe Ipv6SupportValue)
modifyTransitGatewayVpcAttachmentRequestOptions_ipv6Support = Lens.lens (\ModifyTransitGatewayVpcAttachmentRequestOptions' {ipv6Support} -> ipv6Support) (\s@ModifyTransitGatewayVpcAttachmentRequestOptions' {} a -> s {ipv6Support = a} :: ModifyTransitGatewayVpcAttachmentRequestOptions)

instance
  Core.Hashable
    ModifyTransitGatewayVpcAttachmentRequestOptions

instance
  Core.NFData
    ModifyTransitGatewayVpcAttachmentRequestOptions

instance
  Core.ToQuery
    ModifyTransitGatewayVpcAttachmentRequestOptions
  where
  toQuery
    ModifyTransitGatewayVpcAttachmentRequestOptions' {..} =
      Core.mconcat
        [ "ApplianceModeSupport" Core.=: applianceModeSupport,
          "DnsSupport" Core.=: dnsSupport,
          "Ipv6Support" Core.=: ipv6Support
        ]
