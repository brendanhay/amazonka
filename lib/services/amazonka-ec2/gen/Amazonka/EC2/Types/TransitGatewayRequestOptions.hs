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
-- Module      : Amazonka.EC2.Types.TransitGatewayRequestOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TransitGatewayRequestOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.AutoAcceptSharedAttachmentsValue
import Amazonka.EC2.Types.DefaultRouteTableAssociationValue
import Amazonka.EC2.Types.DefaultRouteTablePropagationValue
import Amazonka.EC2.Types.DnsSupportValue
import Amazonka.EC2.Types.MulticastSupportValue
import Amazonka.EC2.Types.VpnEcmpSupportValue
import qualified Amazonka.Prelude as Prelude

-- | Describes the options for a transit gateway.
--
-- /See:/ 'newTransitGatewayRequestOptions' smart constructor.
data TransitGatewayRequestOptions = TransitGatewayRequestOptions'
  { -- | Enable or disable DNS support. Enabled by default.
    dnsSupport :: Prelude.Maybe DnsSupportValue,
    -- | Enable or disable automatic association with the default association
    -- route table. Enabled by default.
    defaultRouteTableAssociation :: Prelude.Maybe DefaultRouteTableAssociationValue,
    -- | Enable or disable automatic acceptance of attachment requests. Disabled
    -- by default.
    autoAcceptSharedAttachments :: Prelude.Maybe AutoAcceptSharedAttachmentsValue,
    -- | Indicates whether multicast is enabled on the transit gateway
    multicastSupport :: Prelude.Maybe MulticastSupportValue,
    -- | A private Autonomous System Number (ASN) for the Amazon side of a BGP
    -- session. The range is 64512 to 65534 for 16-bit ASNs and 4200000000 to
    -- 4294967294 for 32-bit ASNs. The default is @64512@.
    amazonSideAsn :: Prelude.Maybe Prelude.Integer,
    -- | Enable or disable Equal Cost Multipath Protocol support. Enabled by
    -- default.
    vpnEcmpSupport :: Prelude.Maybe VpnEcmpSupportValue,
    -- | Enable or disable automatic propagation of routes to the default
    -- propagation route table. Enabled by default.
    defaultRouteTablePropagation :: Prelude.Maybe DefaultRouteTablePropagationValue,
    -- | One or more IPv4 or IPv6 CIDR blocks for the transit gateway. Must be a
    -- size \/24 CIDR block or larger for IPv4, or a size \/64 CIDR block or
    -- larger for IPv6.
    transitGatewayCidrBlocks :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransitGatewayRequestOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dnsSupport', 'transitGatewayRequestOptions_dnsSupport' - Enable or disable DNS support. Enabled by default.
--
-- 'defaultRouteTableAssociation', 'transitGatewayRequestOptions_defaultRouteTableAssociation' - Enable or disable automatic association with the default association
-- route table. Enabled by default.
--
-- 'autoAcceptSharedAttachments', 'transitGatewayRequestOptions_autoAcceptSharedAttachments' - Enable or disable automatic acceptance of attachment requests. Disabled
-- by default.
--
-- 'multicastSupport', 'transitGatewayRequestOptions_multicastSupport' - Indicates whether multicast is enabled on the transit gateway
--
-- 'amazonSideAsn', 'transitGatewayRequestOptions_amazonSideAsn' - A private Autonomous System Number (ASN) for the Amazon side of a BGP
-- session. The range is 64512 to 65534 for 16-bit ASNs and 4200000000 to
-- 4294967294 for 32-bit ASNs. The default is @64512@.
--
-- 'vpnEcmpSupport', 'transitGatewayRequestOptions_vpnEcmpSupport' - Enable or disable Equal Cost Multipath Protocol support. Enabled by
-- default.
--
-- 'defaultRouteTablePropagation', 'transitGatewayRequestOptions_defaultRouteTablePropagation' - Enable or disable automatic propagation of routes to the default
-- propagation route table. Enabled by default.
--
-- 'transitGatewayCidrBlocks', 'transitGatewayRequestOptions_transitGatewayCidrBlocks' - One or more IPv4 or IPv6 CIDR blocks for the transit gateway. Must be a
-- size \/24 CIDR block or larger for IPv4, or a size \/64 CIDR block or
-- larger for IPv6.
newTransitGatewayRequestOptions ::
  TransitGatewayRequestOptions
newTransitGatewayRequestOptions =
  TransitGatewayRequestOptions'
    { dnsSupport =
        Prelude.Nothing,
      defaultRouteTableAssociation =
        Prelude.Nothing,
      autoAcceptSharedAttachments = Prelude.Nothing,
      multicastSupport = Prelude.Nothing,
      amazonSideAsn = Prelude.Nothing,
      vpnEcmpSupport = Prelude.Nothing,
      defaultRouteTablePropagation =
        Prelude.Nothing,
      transitGatewayCidrBlocks = Prelude.Nothing
    }

-- | Enable or disable DNS support. Enabled by default.
transitGatewayRequestOptions_dnsSupport :: Lens.Lens' TransitGatewayRequestOptions (Prelude.Maybe DnsSupportValue)
transitGatewayRequestOptions_dnsSupport = Lens.lens (\TransitGatewayRequestOptions' {dnsSupport} -> dnsSupport) (\s@TransitGatewayRequestOptions' {} a -> s {dnsSupport = a} :: TransitGatewayRequestOptions)

-- | Enable or disable automatic association with the default association
-- route table. Enabled by default.
transitGatewayRequestOptions_defaultRouteTableAssociation :: Lens.Lens' TransitGatewayRequestOptions (Prelude.Maybe DefaultRouteTableAssociationValue)
transitGatewayRequestOptions_defaultRouteTableAssociation = Lens.lens (\TransitGatewayRequestOptions' {defaultRouteTableAssociation} -> defaultRouteTableAssociation) (\s@TransitGatewayRequestOptions' {} a -> s {defaultRouteTableAssociation = a} :: TransitGatewayRequestOptions)

-- | Enable or disable automatic acceptance of attachment requests. Disabled
-- by default.
transitGatewayRequestOptions_autoAcceptSharedAttachments :: Lens.Lens' TransitGatewayRequestOptions (Prelude.Maybe AutoAcceptSharedAttachmentsValue)
transitGatewayRequestOptions_autoAcceptSharedAttachments = Lens.lens (\TransitGatewayRequestOptions' {autoAcceptSharedAttachments} -> autoAcceptSharedAttachments) (\s@TransitGatewayRequestOptions' {} a -> s {autoAcceptSharedAttachments = a} :: TransitGatewayRequestOptions)

-- | Indicates whether multicast is enabled on the transit gateway
transitGatewayRequestOptions_multicastSupport :: Lens.Lens' TransitGatewayRequestOptions (Prelude.Maybe MulticastSupportValue)
transitGatewayRequestOptions_multicastSupport = Lens.lens (\TransitGatewayRequestOptions' {multicastSupport} -> multicastSupport) (\s@TransitGatewayRequestOptions' {} a -> s {multicastSupport = a} :: TransitGatewayRequestOptions)

-- | A private Autonomous System Number (ASN) for the Amazon side of a BGP
-- session. The range is 64512 to 65534 for 16-bit ASNs and 4200000000 to
-- 4294967294 for 32-bit ASNs. The default is @64512@.
transitGatewayRequestOptions_amazonSideAsn :: Lens.Lens' TransitGatewayRequestOptions (Prelude.Maybe Prelude.Integer)
transitGatewayRequestOptions_amazonSideAsn = Lens.lens (\TransitGatewayRequestOptions' {amazonSideAsn} -> amazonSideAsn) (\s@TransitGatewayRequestOptions' {} a -> s {amazonSideAsn = a} :: TransitGatewayRequestOptions)

-- | Enable or disable Equal Cost Multipath Protocol support. Enabled by
-- default.
transitGatewayRequestOptions_vpnEcmpSupport :: Lens.Lens' TransitGatewayRequestOptions (Prelude.Maybe VpnEcmpSupportValue)
transitGatewayRequestOptions_vpnEcmpSupport = Lens.lens (\TransitGatewayRequestOptions' {vpnEcmpSupport} -> vpnEcmpSupport) (\s@TransitGatewayRequestOptions' {} a -> s {vpnEcmpSupport = a} :: TransitGatewayRequestOptions)

-- | Enable or disable automatic propagation of routes to the default
-- propagation route table. Enabled by default.
transitGatewayRequestOptions_defaultRouteTablePropagation :: Lens.Lens' TransitGatewayRequestOptions (Prelude.Maybe DefaultRouteTablePropagationValue)
transitGatewayRequestOptions_defaultRouteTablePropagation = Lens.lens (\TransitGatewayRequestOptions' {defaultRouteTablePropagation} -> defaultRouteTablePropagation) (\s@TransitGatewayRequestOptions' {} a -> s {defaultRouteTablePropagation = a} :: TransitGatewayRequestOptions)

-- | One or more IPv4 or IPv6 CIDR blocks for the transit gateway. Must be a
-- size \/24 CIDR block or larger for IPv4, or a size \/64 CIDR block or
-- larger for IPv6.
transitGatewayRequestOptions_transitGatewayCidrBlocks :: Lens.Lens' TransitGatewayRequestOptions (Prelude.Maybe [Prelude.Text])
transitGatewayRequestOptions_transitGatewayCidrBlocks = Lens.lens (\TransitGatewayRequestOptions' {transitGatewayCidrBlocks} -> transitGatewayCidrBlocks) (\s@TransitGatewayRequestOptions' {} a -> s {transitGatewayCidrBlocks = a} :: TransitGatewayRequestOptions) Prelude.. Lens.mapping Lens.coerced

instance
  Prelude.Hashable
    TransitGatewayRequestOptions
  where
  hashWithSalt _salt TransitGatewayRequestOptions' {..} =
    _salt `Prelude.hashWithSalt` dnsSupport
      `Prelude.hashWithSalt` defaultRouteTableAssociation
      `Prelude.hashWithSalt` autoAcceptSharedAttachments
      `Prelude.hashWithSalt` multicastSupport
      `Prelude.hashWithSalt` amazonSideAsn
      `Prelude.hashWithSalt` vpnEcmpSupport
      `Prelude.hashWithSalt` defaultRouteTablePropagation
      `Prelude.hashWithSalt` transitGatewayCidrBlocks

instance Prelude.NFData TransitGatewayRequestOptions where
  rnf TransitGatewayRequestOptions' {..} =
    Prelude.rnf dnsSupport
      `Prelude.seq` Prelude.rnf defaultRouteTableAssociation
      `Prelude.seq` Prelude.rnf autoAcceptSharedAttachments
      `Prelude.seq` Prelude.rnf multicastSupport
      `Prelude.seq` Prelude.rnf amazonSideAsn
      `Prelude.seq` Prelude.rnf vpnEcmpSupport
      `Prelude.seq` Prelude.rnf defaultRouteTablePropagation
      `Prelude.seq` Prelude.rnf transitGatewayCidrBlocks

instance Data.ToQuery TransitGatewayRequestOptions where
  toQuery TransitGatewayRequestOptions' {..} =
    Prelude.mconcat
      [ "DnsSupport" Data.=: dnsSupport,
        "DefaultRouteTableAssociation"
          Data.=: defaultRouteTableAssociation,
        "AutoAcceptSharedAttachments"
          Data.=: autoAcceptSharedAttachments,
        "MulticastSupport" Data.=: multicastSupport,
        "AmazonSideAsn" Data.=: amazonSideAsn,
        "VpnEcmpSupport" Data.=: vpnEcmpSupport,
        "DefaultRouteTablePropagation"
          Data.=: defaultRouteTablePropagation,
        Data.toQuery
          ( Data.toQueryList "TransitGatewayCidrBlocks"
              Prelude.<$> transitGatewayCidrBlocks
          )
      ]
