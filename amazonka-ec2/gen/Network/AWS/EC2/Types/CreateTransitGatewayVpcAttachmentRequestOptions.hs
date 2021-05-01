{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.Types.CreateTransitGatewayVpcAttachmentRequestOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CreateTransitGatewayVpcAttachmentRequestOptions where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ApplianceModeSupportValue
import Network.AWS.EC2.Types.DnsSupportValue
import Network.AWS.EC2.Types.Ipv6SupportValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the options for a VPC attachment.
--
-- /See:/ 'newCreateTransitGatewayVpcAttachmentRequestOptions' smart constructor.
data CreateTransitGatewayVpcAttachmentRequestOptions = CreateTransitGatewayVpcAttachmentRequestOptions'
  { -- | Enable or disable support for appliance mode. If enabled, a traffic flow
    -- between a source and destination uses the same Availability Zone for the
    -- VPC attachment for the lifetime of that flow. The default is @disable@.
    applianceModeSupport :: Prelude.Maybe ApplianceModeSupportValue,
    -- | Enable or disable DNS support. The default is @enable@.
    dnsSupport :: Prelude.Maybe DnsSupportValue,
    -- | Enable or disable IPv6 support. The default is @disable@.
    ipv6Support :: Prelude.Maybe Ipv6SupportValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateTransitGatewayVpcAttachmentRequestOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applianceModeSupport', 'createTransitGatewayVpcAttachmentRequestOptions_applianceModeSupport' - Enable or disable support for appliance mode. If enabled, a traffic flow
-- between a source and destination uses the same Availability Zone for the
-- VPC attachment for the lifetime of that flow. The default is @disable@.
--
-- 'dnsSupport', 'createTransitGatewayVpcAttachmentRequestOptions_dnsSupport' - Enable or disable DNS support. The default is @enable@.
--
-- 'ipv6Support', 'createTransitGatewayVpcAttachmentRequestOptions_ipv6Support' - Enable or disable IPv6 support. The default is @disable@.
newCreateTransitGatewayVpcAttachmentRequestOptions ::
  CreateTransitGatewayVpcAttachmentRequestOptions
newCreateTransitGatewayVpcAttachmentRequestOptions =
  CreateTransitGatewayVpcAttachmentRequestOptions'
    { applianceModeSupport =
        Prelude.Nothing,
      dnsSupport =
        Prelude.Nothing,
      ipv6Support =
        Prelude.Nothing
    }

-- | Enable or disable support for appliance mode. If enabled, a traffic flow
-- between a source and destination uses the same Availability Zone for the
-- VPC attachment for the lifetime of that flow. The default is @disable@.
createTransitGatewayVpcAttachmentRequestOptions_applianceModeSupport :: Lens.Lens' CreateTransitGatewayVpcAttachmentRequestOptions (Prelude.Maybe ApplianceModeSupportValue)
createTransitGatewayVpcAttachmentRequestOptions_applianceModeSupport = Lens.lens (\CreateTransitGatewayVpcAttachmentRequestOptions' {applianceModeSupport} -> applianceModeSupport) (\s@CreateTransitGatewayVpcAttachmentRequestOptions' {} a -> s {applianceModeSupport = a} :: CreateTransitGatewayVpcAttachmentRequestOptions)

-- | Enable or disable DNS support. The default is @enable@.
createTransitGatewayVpcAttachmentRequestOptions_dnsSupport :: Lens.Lens' CreateTransitGatewayVpcAttachmentRequestOptions (Prelude.Maybe DnsSupportValue)
createTransitGatewayVpcAttachmentRequestOptions_dnsSupport = Lens.lens (\CreateTransitGatewayVpcAttachmentRequestOptions' {dnsSupport} -> dnsSupport) (\s@CreateTransitGatewayVpcAttachmentRequestOptions' {} a -> s {dnsSupport = a} :: CreateTransitGatewayVpcAttachmentRequestOptions)

-- | Enable or disable IPv6 support. The default is @disable@.
createTransitGatewayVpcAttachmentRequestOptions_ipv6Support :: Lens.Lens' CreateTransitGatewayVpcAttachmentRequestOptions (Prelude.Maybe Ipv6SupportValue)
createTransitGatewayVpcAttachmentRequestOptions_ipv6Support = Lens.lens (\CreateTransitGatewayVpcAttachmentRequestOptions' {ipv6Support} -> ipv6Support) (\s@CreateTransitGatewayVpcAttachmentRequestOptions' {} a -> s {ipv6Support = a} :: CreateTransitGatewayVpcAttachmentRequestOptions)

instance
  Prelude.Hashable
    CreateTransitGatewayVpcAttachmentRequestOptions

instance
  Prelude.NFData
    CreateTransitGatewayVpcAttachmentRequestOptions

instance
  Prelude.ToQuery
    CreateTransitGatewayVpcAttachmentRequestOptions
  where
  toQuery
    CreateTransitGatewayVpcAttachmentRequestOptions' {..} =
      Prelude.mconcat
        [ "ApplianceModeSupport"
            Prelude.=: applianceModeSupport,
          "DnsSupport" Prelude.=: dnsSupport,
          "Ipv6Support" Prelude.=: ipv6Support
        ]
