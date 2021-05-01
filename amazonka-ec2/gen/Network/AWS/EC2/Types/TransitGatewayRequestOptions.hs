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
-- Module      : Network.AWS.EC2.Types.TransitGatewayRequestOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayRequestOptions where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.AutoAcceptSharedAttachmentsValue
import Network.AWS.EC2.Types.DefaultRouteTableAssociationValue
import Network.AWS.EC2.Types.DefaultRouteTablePropagationValue
import Network.AWS.EC2.Types.DnsSupportValue
import Network.AWS.EC2.Types.MulticastSupportValue
import Network.AWS.EC2.Types.VpnEcmpSupportValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the options for a transit gateway.
--
-- /See:/ 'newTransitGatewayRequestOptions' smart constructor.
data TransitGatewayRequestOptions = TransitGatewayRequestOptions'
  { -- | Enable or disable Equal Cost Multipath Protocol support. Enabled by
    -- default.
    vpnEcmpSupport :: Prelude.Maybe VpnEcmpSupportValue,
    -- | Enable or disable DNS support. Enabled by default.
    dnsSupport :: Prelude.Maybe DnsSupportValue,
    -- | Enable or disable automatic acceptance of attachment requests. Disabled
    -- by default.
    autoAcceptSharedAttachments :: Prelude.Maybe AutoAcceptSharedAttachmentsValue,
    -- | A private Autonomous System Number (ASN) for the Amazon side of a BGP
    -- session. The range is 64512 to 65534 for 16-bit ASNs and 4200000000 to
    -- 4294967294 for 32-bit ASNs. The default is @64512@.
    amazonSideAsn :: Prelude.Maybe Prelude.Integer,
    -- | One or more IPv4 or IPv6 CIDR blocks for the transit gateway. Must be a
    -- size \/24 CIDR block or larger for IPv4, or a size \/64 CIDR block or
    -- larger for IPv6.
    transitGatewayCidrBlocks :: Prelude.Maybe [Prelude.Text],
    -- | Indicates whether multicast is enabled on the transit gateway
    multicastSupport :: Prelude.Maybe MulticastSupportValue,
    -- | Enable or disable automatic association with the default association
    -- route table. Enabled by default.
    defaultRouteTableAssociation :: Prelude.Maybe DefaultRouteTableAssociationValue,
    -- | Enable or disable automatic propagation of routes to the default
    -- propagation route table. Enabled by default.
    defaultRouteTablePropagation :: Prelude.Maybe DefaultRouteTablePropagationValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TransitGatewayRequestOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpnEcmpSupport', 'transitGatewayRequestOptions_vpnEcmpSupport' - Enable or disable Equal Cost Multipath Protocol support. Enabled by
-- default.
--
-- 'dnsSupport', 'transitGatewayRequestOptions_dnsSupport' - Enable or disable DNS support. Enabled by default.
--
-- 'autoAcceptSharedAttachments', 'transitGatewayRequestOptions_autoAcceptSharedAttachments' - Enable or disable automatic acceptance of attachment requests. Disabled
-- by default.
--
-- 'amazonSideAsn', 'transitGatewayRequestOptions_amazonSideAsn' - A private Autonomous System Number (ASN) for the Amazon side of a BGP
-- session. The range is 64512 to 65534 for 16-bit ASNs and 4200000000 to
-- 4294967294 for 32-bit ASNs. The default is @64512@.
--
-- 'transitGatewayCidrBlocks', 'transitGatewayRequestOptions_transitGatewayCidrBlocks' - One or more IPv4 or IPv6 CIDR blocks for the transit gateway. Must be a
-- size \/24 CIDR block or larger for IPv4, or a size \/64 CIDR block or
-- larger for IPv6.
--
-- 'multicastSupport', 'transitGatewayRequestOptions_multicastSupport' - Indicates whether multicast is enabled on the transit gateway
--
-- 'defaultRouteTableAssociation', 'transitGatewayRequestOptions_defaultRouteTableAssociation' - Enable or disable automatic association with the default association
-- route table. Enabled by default.
--
-- 'defaultRouteTablePropagation', 'transitGatewayRequestOptions_defaultRouteTablePropagation' - Enable or disable automatic propagation of routes to the default
-- propagation route table. Enabled by default.
newTransitGatewayRequestOptions ::
  TransitGatewayRequestOptions
newTransitGatewayRequestOptions =
  TransitGatewayRequestOptions'
    { vpnEcmpSupport =
        Prelude.Nothing,
      dnsSupport = Prelude.Nothing,
      autoAcceptSharedAttachments = Prelude.Nothing,
      amazonSideAsn = Prelude.Nothing,
      transitGatewayCidrBlocks = Prelude.Nothing,
      multicastSupport = Prelude.Nothing,
      defaultRouteTableAssociation =
        Prelude.Nothing,
      defaultRouteTablePropagation =
        Prelude.Nothing
    }

-- | Enable or disable Equal Cost Multipath Protocol support. Enabled by
-- default.
transitGatewayRequestOptions_vpnEcmpSupport :: Lens.Lens' TransitGatewayRequestOptions (Prelude.Maybe VpnEcmpSupportValue)
transitGatewayRequestOptions_vpnEcmpSupport = Lens.lens (\TransitGatewayRequestOptions' {vpnEcmpSupport} -> vpnEcmpSupport) (\s@TransitGatewayRequestOptions' {} a -> s {vpnEcmpSupport = a} :: TransitGatewayRequestOptions)

-- | Enable or disable DNS support. Enabled by default.
transitGatewayRequestOptions_dnsSupport :: Lens.Lens' TransitGatewayRequestOptions (Prelude.Maybe DnsSupportValue)
transitGatewayRequestOptions_dnsSupport = Lens.lens (\TransitGatewayRequestOptions' {dnsSupport} -> dnsSupport) (\s@TransitGatewayRequestOptions' {} a -> s {dnsSupport = a} :: TransitGatewayRequestOptions)

-- | Enable or disable automatic acceptance of attachment requests. Disabled
-- by default.
transitGatewayRequestOptions_autoAcceptSharedAttachments :: Lens.Lens' TransitGatewayRequestOptions (Prelude.Maybe AutoAcceptSharedAttachmentsValue)
transitGatewayRequestOptions_autoAcceptSharedAttachments = Lens.lens (\TransitGatewayRequestOptions' {autoAcceptSharedAttachments} -> autoAcceptSharedAttachments) (\s@TransitGatewayRequestOptions' {} a -> s {autoAcceptSharedAttachments = a} :: TransitGatewayRequestOptions)

-- | A private Autonomous System Number (ASN) for the Amazon side of a BGP
-- session. The range is 64512 to 65534 for 16-bit ASNs and 4200000000 to
-- 4294967294 for 32-bit ASNs. The default is @64512@.
transitGatewayRequestOptions_amazonSideAsn :: Lens.Lens' TransitGatewayRequestOptions (Prelude.Maybe Prelude.Integer)
transitGatewayRequestOptions_amazonSideAsn = Lens.lens (\TransitGatewayRequestOptions' {amazonSideAsn} -> amazonSideAsn) (\s@TransitGatewayRequestOptions' {} a -> s {amazonSideAsn = a} :: TransitGatewayRequestOptions)

-- | One or more IPv4 or IPv6 CIDR blocks for the transit gateway. Must be a
-- size \/24 CIDR block or larger for IPv4, or a size \/64 CIDR block or
-- larger for IPv6.
transitGatewayRequestOptions_transitGatewayCidrBlocks :: Lens.Lens' TransitGatewayRequestOptions (Prelude.Maybe [Prelude.Text])
transitGatewayRequestOptions_transitGatewayCidrBlocks = Lens.lens (\TransitGatewayRequestOptions' {transitGatewayCidrBlocks} -> transitGatewayCidrBlocks) (\s@TransitGatewayRequestOptions' {} a -> s {transitGatewayCidrBlocks = a} :: TransitGatewayRequestOptions) Prelude.. Lens.mapping Prelude._Coerce

-- | Indicates whether multicast is enabled on the transit gateway
transitGatewayRequestOptions_multicastSupport :: Lens.Lens' TransitGatewayRequestOptions (Prelude.Maybe MulticastSupportValue)
transitGatewayRequestOptions_multicastSupport = Lens.lens (\TransitGatewayRequestOptions' {multicastSupport} -> multicastSupport) (\s@TransitGatewayRequestOptions' {} a -> s {multicastSupport = a} :: TransitGatewayRequestOptions)

-- | Enable or disable automatic association with the default association
-- route table. Enabled by default.
transitGatewayRequestOptions_defaultRouteTableAssociation :: Lens.Lens' TransitGatewayRequestOptions (Prelude.Maybe DefaultRouteTableAssociationValue)
transitGatewayRequestOptions_defaultRouteTableAssociation = Lens.lens (\TransitGatewayRequestOptions' {defaultRouteTableAssociation} -> defaultRouteTableAssociation) (\s@TransitGatewayRequestOptions' {} a -> s {defaultRouteTableAssociation = a} :: TransitGatewayRequestOptions)

-- | Enable or disable automatic propagation of routes to the default
-- propagation route table. Enabled by default.
transitGatewayRequestOptions_defaultRouteTablePropagation :: Lens.Lens' TransitGatewayRequestOptions (Prelude.Maybe DefaultRouteTablePropagationValue)
transitGatewayRequestOptions_defaultRouteTablePropagation = Lens.lens (\TransitGatewayRequestOptions' {defaultRouteTablePropagation} -> defaultRouteTablePropagation) (\s@TransitGatewayRequestOptions' {} a -> s {defaultRouteTablePropagation = a} :: TransitGatewayRequestOptions)

instance
  Prelude.Hashable
    TransitGatewayRequestOptions

instance Prelude.NFData TransitGatewayRequestOptions

instance Prelude.ToQuery TransitGatewayRequestOptions where
  toQuery TransitGatewayRequestOptions' {..} =
    Prelude.mconcat
      [ "VpnEcmpSupport" Prelude.=: vpnEcmpSupport,
        "DnsSupport" Prelude.=: dnsSupport,
        "AutoAcceptSharedAttachments"
          Prelude.=: autoAcceptSharedAttachments,
        "AmazonSideAsn" Prelude.=: amazonSideAsn,
        Prelude.toQuery
          ( Prelude.toQueryList "TransitGatewayCidrBlocks"
              Prelude.<$> transitGatewayCidrBlocks
          ),
        "MulticastSupport" Prelude.=: multicastSupport,
        "DefaultRouteTableAssociation"
          Prelude.=: defaultRouteTableAssociation,
        "DefaultRouteTablePropagation"
          Prelude.=: defaultRouteTablePropagation
      ]
