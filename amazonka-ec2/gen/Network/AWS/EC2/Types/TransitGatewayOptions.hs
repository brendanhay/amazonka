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
-- Module      : Network.AWS.EC2.Types.TransitGatewayOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayOptions where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.AutoAcceptSharedAttachmentsValue
import Network.AWS.EC2.Types.DefaultRouteTableAssociationValue
import Network.AWS.EC2.Types.DefaultRouteTablePropagationValue
import Network.AWS.EC2.Types.DnsSupportValue
import Network.AWS.EC2.Types.MulticastSupportValue
import Network.AWS.EC2.Types.VpnEcmpSupportValue
import qualified Network.AWS.Lens as Lens

-- | Describes the options for a transit gateway.
--
-- /See:/ 'newTransitGatewayOptions' smart constructor.
data TransitGatewayOptions = TransitGatewayOptions'
  { -- | The ID of the default propagation route table.
    propagationDefaultRouteTableId :: Core.Maybe Core.Text,
    -- | Indicates whether Equal Cost Multipath Protocol support is enabled.
    vpnEcmpSupport :: Core.Maybe VpnEcmpSupportValue,
    -- | Indicates whether DNS support is enabled.
    dnsSupport :: Core.Maybe DnsSupportValue,
    -- | The ID of the default association route table.
    associationDefaultRouteTableId :: Core.Maybe Core.Text,
    -- | Indicates whether attachment requests are automatically accepted.
    autoAcceptSharedAttachments :: Core.Maybe AutoAcceptSharedAttachmentsValue,
    -- | A private Autonomous System Number (ASN) for the Amazon side of a BGP
    -- session. The range is 64512 to 65534 for 16-bit ASNs and 4200000000 to
    -- 4294967294 for 32-bit ASNs.
    amazonSideAsn :: Core.Maybe Core.Integer,
    -- | The transit gateway CIDR blocks.
    transitGatewayCidrBlocks :: Core.Maybe [Core.Text],
    -- | Indicates whether multicast is enabled on the transit gateway
    multicastSupport :: Core.Maybe MulticastSupportValue,
    -- | Indicates whether resource attachments are automatically associated with
    -- the default association route table.
    defaultRouteTableAssociation :: Core.Maybe DefaultRouteTableAssociationValue,
    -- | Indicates whether resource attachments automatically propagate routes to
    -- the default propagation route table.
    defaultRouteTablePropagation :: Core.Maybe DefaultRouteTablePropagationValue
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TransitGatewayOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'propagationDefaultRouteTableId', 'transitGatewayOptions_propagationDefaultRouteTableId' - The ID of the default propagation route table.
--
-- 'vpnEcmpSupport', 'transitGatewayOptions_vpnEcmpSupport' - Indicates whether Equal Cost Multipath Protocol support is enabled.
--
-- 'dnsSupport', 'transitGatewayOptions_dnsSupport' - Indicates whether DNS support is enabled.
--
-- 'associationDefaultRouteTableId', 'transitGatewayOptions_associationDefaultRouteTableId' - The ID of the default association route table.
--
-- 'autoAcceptSharedAttachments', 'transitGatewayOptions_autoAcceptSharedAttachments' - Indicates whether attachment requests are automatically accepted.
--
-- 'amazonSideAsn', 'transitGatewayOptions_amazonSideAsn' - A private Autonomous System Number (ASN) for the Amazon side of a BGP
-- session. The range is 64512 to 65534 for 16-bit ASNs and 4200000000 to
-- 4294967294 for 32-bit ASNs.
--
-- 'transitGatewayCidrBlocks', 'transitGatewayOptions_transitGatewayCidrBlocks' - The transit gateway CIDR blocks.
--
-- 'multicastSupport', 'transitGatewayOptions_multicastSupport' - Indicates whether multicast is enabled on the transit gateway
--
-- 'defaultRouteTableAssociation', 'transitGatewayOptions_defaultRouteTableAssociation' - Indicates whether resource attachments are automatically associated with
-- the default association route table.
--
-- 'defaultRouteTablePropagation', 'transitGatewayOptions_defaultRouteTablePropagation' - Indicates whether resource attachments automatically propagate routes to
-- the default propagation route table.
newTransitGatewayOptions ::
  TransitGatewayOptions
newTransitGatewayOptions =
  TransitGatewayOptions'
    { propagationDefaultRouteTableId =
        Core.Nothing,
      vpnEcmpSupport = Core.Nothing,
      dnsSupport = Core.Nothing,
      associationDefaultRouteTableId = Core.Nothing,
      autoAcceptSharedAttachments = Core.Nothing,
      amazonSideAsn = Core.Nothing,
      transitGatewayCidrBlocks = Core.Nothing,
      multicastSupport = Core.Nothing,
      defaultRouteTableAssociation = Core.Nothing,
      defaultRouteTablePropagation = Core.Nothing
    }

-- | The ID of the default propagation route table.
transitGatewayOptions_propagationDefaultRouteTableId :: Lens.Lens' TransitGatewayOptions (Core.Maybe Core.Text)
transitGatewayOptions_propagationDefaultRouteTableId = Lens.lens (\TransitGatewayOptions' {propagationDefaultRouteTableId} -> propagationDefaultRouteTableId) (\s@TransitGatewayOptions' {} a -> s {propagationDefaultRouteTableId = a} :: TransitGatewayOptions)

-- | Indicates whether Equal Cost Multipath Protocol support is enabled.
transitGatewayOptions_vpnEcmpSupport :: Lens.Lens' TransitGatewayOptions (Core.Maybe VpnEcmpSupportValue)
transitGatewayOptions_vpnEcmpSupport = Lens.lens (\TransitGatewayOptions' {vpnEcmpSupport} -> vpnEcmpSupport) (\s@TransitGatewayOptions' {} a -> s {vpnEcmpSupport = a} :: TransitGatewayOptions)

-- | Indicates whether DNS support is enabled.
transitGatewayOptions_dnsSupport :: Lens.Lens' TransitGatewayOptions (Core.Maybe DnsSupportValue)
transitGatewayOptions_dnsSupport = Lens.lens (\TransitGatewayOptions' {dnsSupport} -> dnsSupport) (\s@TransitGatewayOptions' {} a -> s {dnsSupport = a} :: TransitGatewayOptions)

-- | The ID of the default association route table.
transitGatewayOptions_associationDefaultRouteTableId :: Lens.Lens' TransitGatewayOptions (Core.Maybe Core.Text)
transitGatewayOptions_associationDefaultRouteTableId = Lens.lens (\TransitGatewayOptions' {associationDefaultRouteTableId} -> associationDefaultRouteTableId) (\s@TransitGatewayOptions' {} a -> s {associationDefaultRouteTableId = a} :: TransitGatewayOptions)

-- | Indicates whether attachment requests are automatically accepted.
transitGatewayOptions_autoAcceptSharedAttachments :: Lens.Lens' TransitGatewayOptions (Core.Maybe AutoAcceptSharedAttachmentsValue)
transitGatewayOptions_autoAcceptSharedAttachments = Lens.lens (\TransitGatewayOptions' {autoAcceptSharedAttachments} -> autoAcceptSharedAttachments) (\s@TransitGatewayOptions' {} a -> s {autoAcceptSharedAttachments = a} :: TransitGatewayOptions)

-- | A private Autonomous System Number (ASN) for the Amazon side of a BGP
-- session. The range is 64512 to 65534 for 16-bit ASNs and 4200000000 to
-- 4294967294 for 32-bit ASNs.
transitGatewayOptions_amazonSideAsn :: Lens.Lens' TransitGatewayOptions (Core.Maybe Core.Integer)
transitGatewayOptions_amazonSideAsn = Lens.lens (\TransitGatewayOptions' {amazonSideAsn} -> amazonSideAsn) (\s@TransitGatewayOptions' {} a -> s {amazonSideAsn = a} :: TransitGatewayOptions)

-- | The transit gateway CIDR blocks.
transitGatewayOptions_transitGatewayCidrBlocks :: Lens.Lens' TransitGatewayOptions (Core.Maybe [Core.Text])
transitGatewayOptions_transitGatewayCidrBlocks = Lens.lens (\TransitGatewayOptions' {transitGatewayCidrBlocks} -> transitGatewayCidrBlocks) (\s@TransitGatewayOptions' {} a -> s {transitGatewayCidrBlocks = a} :: TransitGatewayOptions) Core.. Lens.mapping Lens._Coerce

-- | Indicates whether multicast is enabled on the transit gateway
transitGatewayOptions_multicastSupport :: Lens.Lens' TransitGatewayOptions (Core.Maybe MulticastSupportValue)
transitGatewayOptions_multicastSupport = Lens.lens (\TransitGatewayOptions' {multicastSupport} -> multicastSupport) (\s@TransitGatewayOptions' {} a -> s {multicastSupport = a} :: TransitGatewayOptions)

-- | Indicates whether resource attachments are automatically associated with
-- the default association route table.
transitGatewayOptions_defaultRouteTableAssociation :: Lens.Lens' TransitGatewayOptions (Core.Maybe DefaultRouteTableAssociationValue)
transitGatewayOptions_defaultRouteTableAssociation = Lens.lens (\TransitGatewayOptions' {defaultRouteTableAssociation} -> defaultRouteTableAssociation) (\s@TransitGatewayOptions' {} a -> s {defaultRouteTableAssociation = a} :: TransitGatewayOptions)

-- | Indicates whether resource attachments automatically propagate routes to
-- the default propagation route table.
transitGatewayOptions_defaultRouteTablePropagation :: Lens.Lens' TransitGatewayOptions (Core.Maybe DefaultRouteTablePropagationValue)
transitGatewayOptions_defaultRouteTablePropagation = Lens.lens (\TransitGatewayOptions' {defaultRouteTablePropagation} -> defaultRouteTablePropagation) (\s@TransitGatewayOptions' {} a -> s {defaultRouteTablePropagation = a} :: TransitGatewayOptions)

instance Core.FromXML TransitGatewayOptions where
  parseXML x =
    TransitGatewayOptions'
      Core.<$> (x Core..@? "propagationDefaultRouteTableId")
      Core.<*> (x Core..@? "vpnEcmpSupport")
      Core.<*> (x Core..@? "dnsSupport")
      Core.<*> (x Core..@? "associationDefaultRouteTableId")
      Core.<*> (x Core..@? "autoAcceptSharedAttachments")
      Core.<*> (x Core..@? "amazonSideAsn")
      Core.<*> ( x Core..@? "transitGatewayCidrBlocks"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "multicastSupport")
      Core.<*> (x Core..@? "defaultRouteTableAssociation")
      Core.<*> (x Core..@? "defaultRouteTablePropagation")

instance Core.Hashable TransitGatewayOptions

instance Core.NFData TransitGatewayOptions
