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
-- Module      : Amazonka.EC2.Types.TransitGatewayOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TransitGatewayOptions where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.AutoAcceptSharedAttachmentsValue
import Amazonka.EC2.Types.DefaultRouteTableAssociationValue
import Amazonka.EC2.Types.DefaultRouteTablePropagationValue
import Amazonka.EC2.Types.DnsSupportValue
import Amazonka.EC2.Types.MulticastSupportValue
import Amazonka.EC2.Types.VpnEcmpSupportValue
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the options for a transit gateway.
--
-- /See:/ 'newTransitGatewayOptions' smart constructor.
data TransitGatewayOptions = TransitGatewayOptions'
  { -- | Indicates whether Equal Cost Multipath Protocol support is enabled.
    vpnEcmpSupport :: Prelude.Maybe VpnEcmpSupportValue,
    -- | Indicates whether attachment requests are automatically accepted.
    autoAcceptSharedAttachments :: Prelude.Maybe AutoAcceptSharedAttachmentsValue,
    -- | The ID of the default propagation route table.
    propagationDefaultRouteTableId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether resource attachments are automatically associated with
    -- the default association route table.
    defaultRouteTableAssociation :: Prelude.Maybe DefaultRouteTableAssociationValue,
    -- | The ID of the default association route table.
    associationDefaultRouteTableId :: Prelude.Maybe Prelude.Text,
    -- | A private Autonomous System Number (ASN) for the Amazon side of a BGP
    -- session. The range is 64512 to 65534 for 16-bit ASNs and 4200000000 to
    -- 4294967294 for 32-bit ASNs.
    amazonSideAsn :: Prelude.Maybe Prelude.Integer,
    -- | Indicates whether resource attachments automatically propagate routes to
    -- the default propagation route table.
    defaultRouteTablePropagation :: Prelude.Maybe DefaultRouteTablePropagationValue,
    -- | Indicates whether multicast is enabled on the transit gateway
    multicastSupport :: Prelude.Maybe MulticastSupportValue,
    -- | Indicates whether DNS support is enabled.
    dnsSupport :: Prelude.Maybe DnsSupportValue,
    -- | The transit gateway CIDR blocks.
    transitGatewayCidrBlocks :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransitGatewayOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpnEcmpSupport', 'transitGatewayOptions_vpnEcmpSupport' - Indicates whether Equal Cost Multipath Protocol support is enabled.
--
-- 'autoAcceptSharedAttachments', 'transitGatewayOptions_autoAcceptSharedAttachments' - Indicates whether attachment requests are automatically accepted.
--
-- 'propagationDefaultRouteTableId', 'transitGatewayOptions_propagationDefaultRouteTableId' - The ID of the default propagation route table.
--
-- 'defaultRouteTableAssociation', 'transitGatewayOptions_defaultRouteTableAssociation' - Indicates whether resource attachments are automatically associated with
-- the default association route table.
--
-- 'associationDefaultRouteTableId', 'transitGatewayOptions_associationDefaultRouteTableId' - The ID of the default association route table.
--
-- 'amazonSideAsn', 'transitGatewayOptions_amazonSideAsn' - A private Autonomous System Number (ASN) for the Amazon side of a BGP
-- session. The range is 64512 to 65534 for 16-bit ASNs and 4200000000 to
-- 4294967294 for 32-bit ASNs.
--
-- 'defaultRouteTablePropagation', 'transitGatewayOptions_defaultRouteTablePropagation' - Indicates whether resource attachments automatically propagate routes to
-- the default propagation route table.
--
-- 'multicastSupport', 'transitGatewayOptions_multicastSupport' - Indicates whether multicast is enabled on the transit gateway
--
-- 'dnsSupport', 'transitGatewayOptions_dnsSupport' - Indicates whether DNS support is enabled.
--
-- 'transitGatewayCidrBlocks', 'transitGatewayOptions_transitGatewayCidrBlocks' - The transit gateway CIDR blocks.
newTransitGatewayOptions ::
  TransitGatewayOptions
newTransitGatewayOptions =
  TransitGatewayOptions'
    { vpnEcmpSupport =
        Prelude.Nothing,
      autoAcceptSharedAttachments = Prelude.Nothing,
      propagationDefaultRouteTableId = Prelude.Nothing,
      defaultRouteTableAssociation = Prelude.Nothing,
      associationDefaultRouteTableId = Prelude.Nothing,
      amazonSideAsn = Prelude.Nothing,
      defaultRouteTablePropagation = Prelude.Nothing,
      multicastSupport = Prelude.Nothing,
      dnsSupport = Prelude.Nothing,
      transitGatewayCidrBlocks = Prelude.Nothing
    }

-- | Indicates whether Equal Cost Multipath Protocol support is enabled.
transitGatewayOptions_vpnEcmpSupport :: Lens.Lens' TransitGatewayOptions (Prelude.Maybe VpnEcmpSupportValue)
transitGatewayOptions_vpnEcmpSupport = Lens.lens (\TransitGatewayOptions' {vpnEcmpSupport} -> vpnEcmpSupport) (\s@TransitGatewayOptions' {} a -> s {vpnEcmpSupport = a} :: TransitGatewayOptions)

-- | Indicates whether attachment requests are automatically accepted.
transitGatewayOptions_autoAcceptSharedAttachments :: Lens.Lens' TransitGatewayOptions (Prelude.Maybe AutoAcceptSharedAttachmentsValue)
transitGatewayOptions_autoAcceptSharedAttachments = Lens.lens (\TransitGatewayOptions' {autoAcceptSharedAttachments} -> autoAcceptSharedAttachments) (\s@TransitGatewayOptions' {} a -> s {autoAcceptSharedAttachments = a} :: TransitGatewayOptions)

-- | The ID of the default propagation route table.
transitGatewayOptions_propagationDefaultRouteTableId :: Lens.Lens' TransitGatewayOptions (Prelude.Maybe Prelude.Text)
transitGatewayOptions_propagationDefaultRouteTableId = Lens.lens (\TransitGatewayOptions' {propagationDefaultRouteTableId} -> propagationDefaultRouteTableId) (\s@TransitGatewayOptions' {} a -> s {propagationDefaultRouteTableId = a} :: TransitGatewayOptions)

-- | Indicates whether resource attachments are automatically associated with
-- the default association route table.
transitGatewayOptions_defaultRouteTableAssociation :: Lens.Lens' TransitGatewayOptions (Prelude.Maybe DefaultRouteTableAssociationValue)
transitGatewayOptions_defaultRouteTableAssociation = Lens.lens (\TransitGatewayOptions' {defaultRouteTableAssociation} -> defaultRouteTableAssociation) (\s@TransitGatewayOptions' {} a -> s {defaultRouteTableAssociation = a} :: TransitGatewayOptions)

-- | The ID of the default association route table.
transitGatewayOptions_associationDefaultRouteTableId :: Lens.Lens' TransitGatewayOptions (Prelude.Maybe Prelude.Text)
transitGatewayOptions_associationDefaultRouteTableId = Lens.lens (\TransitGatewayOptions' {associationDefaultRouteTableId} -> associationDefaultRouteTableId) (\s@TransitGatewayOptions' {} a -> s {associationDefaultRouteTableId = a} :: TransitGatewayOptions)

-- | A private Autonomous System Number (ASN) for the Amazon side of a BGP
-- session. The range is 64512 to 65534 for 16-bit ASNs and 4200000000 to
-- 4294967294 for 32-bit ASNs.
transitGatewayOptions_amazonSideAsn :: Lens.Lens' TransitGatewayOptions (Prelude.Maybe Prelude.Integer)
transitGatewayOptions_amazonSideAsn = Lens.lens (\TransitGatewayOptions' {amazonSideAsn} -> amazonSideAsn) (\s@TransitGatewayOptions' {} a -> s {amazonSideAsn = a} :: TransitGatewayOptions)

-- | Indicates whether resource attachments automatically propagate routes to
-- the default propagation route table.
transitGatewayOptions_defaultRouteTablePropagation :: Lens.Lens' TransitGatewayOptions (Prelude.Maybe DefaultRouteTablePropagationValue)
transitGatewayOptions_defaultRouteTablePropagation = Lens.lens (\TransitGatewayOptions' {defaultRouteTablePropagation} -> defaultRouteTablePropagation) (\s@TransitGatewayOptions' {} a -> s {defaultRouteTablePropagation = a} :: TransitGatewayOptions)

-- | Indicates whether multicast is enabled on the transit gateway
transitGatewayOptions_multicastSupport :: Lens.Lens' TransitGatewayOptions (Prelude.Maybe MulticastSupportValue)
transitGatewayOptions_multicastSupport = Lens.lens (\TransitGatewayOptions' {multicastSupport} -> multicastSupport) (\s@TransitGatewayOptions' {} a -> s {multicastSupport = a} :: TransitGatewayOptions)

-- | Indicates whether DNS support is enabled.
transitGatewayOptions_dnsSupport :: Lens.Lens' TransitGatewayOptions (Prelude.Maybe DnsSupportValue)
transitGatewayOptions_dnsSupport = Lens.lens (\TransitGatewayOptions' {dnsSupport} -> dnsSupport) (\s@TransitGatewayOptions' {} a -> s {dnsSupport = a} :: TransitGatewayOptions)

-- | The transit gateway CIDR blocks.
transitGatewayOptions_transitGatewayCidrBlocks :: Lens.Lens' TransitGatewayOptions (Prelude.Maybe [Prelude.Text])
transitGatewayOptions_transitGatewayCidrBlocks = Lens.lens (\TransitGatewayOptions' {transitGatewayCidrBlocks} -> transitGatewayCidrBlocks) (\s@TransitGatewayOptions' {} a -> s {transitGatewayCidrBlocks = a} :: TransitGatewayOptions) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML TransitGatewayOptions where
  parseXML x =
    TransitGatewayOptions'
      Prelude.<$> (x Core..@? "vpnEcmpSupport")
      Prelude.<*> (x Core..@? "autoAcceptSharedAttachments")
      Prelude.<*> (x Core..@? "propagationDefaultRouteTableId")
      Prelude.<*> (x Core..@? "defaultRouteTableAssociation")
      Prelude.<*> (x Core..@? "associationDefaultRouteTableId")
      Prelude.<*> (x Core..@? "amazonSideAsn")
      Prelude.<*> (x Core..@? "defaultRouteTablePropagation")
      Prelude.<*> (x Core..@? "multicastSupport")
      Prelude.<*> (x Core..@? "dnsSupport")
      Prelude.<*> ( x Core..@? "transitGatewayCidrBlocks"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )

instance Prelude.Hashable TransitGatewayOptions where
  hashWithSalt salt' TransitGatewayOptions' {..} =
    salt'
      `Prelude.hashWithSalt` transitGatewayCidrBlocks
      `Prelude.hashWithSalt` dnsSupport
      `Prelude.hashWithSalt` multicastSupport
      `Prelude.hashWithSalt` defaultRouteTablePropagation
      `Prelude.hashWithSalt` amazonSideAsn
      `Prelude.hashWithSalt` associationDefaultRouteTableId
      `Prelude.hashWithSalt` defaultRouteTableAssociation
      `Prelude.hashWithSalt` propagationDefaultRouteTableId
      `Prelude.hashWithSalt` autoAcceptSharedAttachments
      `Prelude.hashWithSalt` vpnEcmpSupport

instance Prelude.NFData TransitGatewayOptions where
  rnf TransitGatewayOptions' {..} =
    Prelude.rnf vpnEcmpSupport
      `Prelude.seq` Prelude.rnf transitGatewayCidrBlocks
      `Prelude.seq` Prelude.rnf dnsSupport
      `Prelude.seq` Prelude.rnf multicastSupport
      `Prelude.seq` Prelude.rnf defaultRouteTablePropagation
      `Prelude.seq` Prelude.rnf amazonSideAsn
      `Prelude.seq` Prelude.rnf associationDefaultRouteTableId
      `Prelude.seq` Prelude.rnf defaultRouteTableAssociation
      `Prelude.seq` Prelude.rnf propagationDefaultRouteTableId
      `Prelude.seq` Prelude.rnf autoAcceptSharedAttachments
