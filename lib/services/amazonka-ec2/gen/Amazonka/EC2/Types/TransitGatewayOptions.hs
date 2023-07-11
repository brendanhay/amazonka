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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TransitGatewayOptions where

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
-- /See:/ 'newTransitGatewayOptions' smart constructor.
data TransitGatewayOptions = TransitGatewayOptions'
  { -- | A private Autonomous System Number (ASN) for the Amazon side of a BGP
    -- session. The range is 64512 to 65534 for 16-bit ASNs and 4200000000 to
    -- 4294967294 for 32-bit ASNs.
    amazonSideAsn :: Prelude.Maybe Prelude.Integer,
    -- | The ID of the default association route table.
    associationDefaultRouteTableId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether attachment requests are automatically accepted.
    autoAcceptSharedAttachments :: Prelude.Maybe AutoAcceptSharedAttachmentsValue,
    -- | Indicates whether resource attachments are automatically associated with
    -- the default association route table.
    defaultRouteTableAssociation :: Prelude.Maybe DefaultRouteTableAssociationValue,
    -- | Indicates whether resource attachments automatically propagate routes to
    -- the default propagation route table.
    defaultRouteTablePropagation :: Prelude.Maybe DefaultRouteTablePropagationValue,
    -- | Indicates whether DNS support is enabled.
    dnsSupport :: Prelude.Maybe DnsSupportValue,
    -- | Indicates whether multicast is enabled on the transit gateway
    multicastSupport :: Prelude.Maybe MulticastSupportValue,
    -- | The ID of the default propagation route table.
    propagationDefaultRouteTableId :: Prelude.Maybe Prelude.Text,
    -- | The transit gateway CIDR blocks.
    transitGatewayCidrBlocks :: Prelude.Maybe [Prelude.Text],
    -- | Indicates whether Equal Cost Multipath Protocol support is enabled.
    vpnEcmpSupport :: Prelude.Maybe VpnEcmpSupportValue
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
-- 'amazonSideAsn', 'transitGatewayOptions_amazonSideAsn' - A private Autonomous System Number (ASN) for the Amazon side of a BGP
-- session. The range is 64512 to 65534 for 16-bit ASNs and 4200000000 to
-- 4294967294 for 32-bit ASNs.
--
-- 'associationDefaultRouteTableId', 'transitGatewayOptions_associationDefaultRouteTableId' - The ID of the default association route table.
--
-- 'autoAcceptSharedAttachments', 'transitGatewayOptions_autoAcceptSharedAttachments' - Indicates whether attachment requests are automatically accepted.
--
-- 'defaultRouteTableAssociation', 'transitGatewayOptions_defaultRouteTableAssociation' - Indicates whether resource attachments are automatically associated with
-- the default association route table.
--
-- 'defaultRouteTablePropagation', 'transitGatewayOptions_defaultRouteTablePropagation' - Indicates whether resource attachments automatically propagate routes to
-- the default propagation route table.
--
-- 'dnsSupport', 'transitGatewayOptions_dnsSupport' - Indicates whether DNS support is enabled.
--
-- 'multicastSupport', 'transitGatewayOptions_multicastSupport' - Indicates whether multicast is enabled on the transit gateway
--
-- 'propagationDefaultRouteTableId', 'transitGatewayOptions_propagationDefaultRouteTableId' - The ID of the default propagation route table.
--
-- 'transitGatewayCidrBlocks', 'transitGatewayOptions_transitGatewayCidrBlocks' - The transit gateway CIDR blocks.
--
-- 'vpnEcmpSupport', 'transitGatewayOptions_vpnEcmpSupport' - Indicates whether Equal Cost Multipath Protocol support is enabled.
newTransitGatewayOptions ::
  TransitGatewayOptions
newTransitGatewayOptions =
  TransitGatewayOptions'
    { amazonSideAsn =
        Prelude.Nothing,
      associationDefaultRouteTableId = Prelude.Nothing,
      autoAcceptSharedAttachments = Prelude.Nothing,
      defaultRouteTableAssociation = Prelude.Nothing,
      defaultRouteTablePropagation = Prelude.Nothing,
      dnsSupport = Prelude.Nothing,
      multicastSupport = Prelude.Nothing,
      propagationDefaultRouteTableId = Prelude.Nothing,
      transitGatewayCidrBlocks = Prelude.Nothing,
      vpnEcmpSupport = Prelude.Nothing
    }

-- | A private Autonomous System Number (ASN) for the Amazon side of a BGP
-- session. The range is 64512 to 65534 for 16-bit ASNs and 4200000000 to
-- 4294967294 for 32-bit ASNs.
transitGatewayOptions_amazonSideAsn :: Lens.Lens' TransitGatewayOptions (Prelude.Maybe Prelude.Integer)
transitGatewayOptions_amazonSideAsn = Lens.lens (\TransitGatewayOptions' {amazonSideAsn} -> amazonSideAsn) (\s@TransitGatewayOptions' {} a -> s {amazonSideAsn = a} :: TransitGatewayOptions)

-- | The ID of the default association route table.
transitGatewayOptions_associationDefaultRouteTableId :: Lens.Lens' TransitGatewayOptions (Prelude.Maybe Prelude.Text)
transitGatewayOptions_associationDefaultRouteTableId = Lens.lens (\TransitGatewayOptions' {associationDefaultRouteTableId} -> associationDefaultRouteTableId) (\s@TransitGatewayOptions' {} a -> s {associationDefaultRouteTableId = a} :: TransitGatewayOptions)

-- | Indicates whether attachment requests are automatically accepted.
transitGatewayOptions_autoAcceptSharedAttachments :: Lens.Lens' TransitGatewayOptions (Prelude.Maybe AutoAcceptSharedAttachmentsValue)
transitGatewayOptions_autoAcceptSharedAttachments = Lens.lens (\TransitGatewayOptions' {autoAcceptSharedAttachments} -> autoAcceptSharedAttachments) (\s@TransitGatewayOptions' {} a -> s {autoAcceptSharedAttachments = a} :: TransitGatewayOptions)

-- | Indicates whether resource attachments are automatically associated with
-- the default association route table.
transitGatewayOptions_defaultRouteTableAssociation :: Lens.Lens' TransitGatewayOptions (Prelude.Maybe DefaultRouteTableAssociationValue)
transitGatewayOptions_defaultRouteTableAssociation = Lens.lens (\TransitGatewayOptions' {defaultRouteTableAssociation} -> defaultRouteTableAssociation) (\s@TransitGatewayOptions' {} a -> s {defaultRouteTableAssociation = a} :: TransitGatewayOptions)

-- | Indicates whether resource attachments automatically propagate routes to
-- the default propagation route table.
transitGatewayOptions_defaultRouteTablePropagation :: Lens.Lens' TransitGatewayOptions (Prelude.Maybe DefaultRouteTablePropagationValue)
transitGatewayOptions_defaultRouteTablePropagation = Lens.lens (\TransitGatewayOptions' {defaultRouteTablePropagation} -> defaultRouteTablePropagation) (\s@TransitGatewayOptions' {} a -> s {defaultRouteTablePropagation = a} :: TransitGatewayOptions)

-- | Indicates whether DNS support is enabled.
transitGatewayOptions_dnsSupport :: Lens.Lens' TransitGatewayOptions (Prelude.Maybe DnsSupportValue)
transitGatewayOptions_dnsSupport = Lens.lens (\TransitGatewayOptions' {dnsSupport} -> dnsSupport) (\s@TransitGatewayOptions' {} a -> s {dnsSupport = a} :: TransitGatewayOptions)

-- | Indicates whether multicast is enabled on the transit gateway
transitGatewayOptions_multicastSupport :: Lens.Lens' TransitGatewayOptions (Prelude.Maybe MulticastSupportValue)
transitGatewayOptions_multicastSupport = Lens.lens (\TransitGatewayOptions' {multicastSupport} -> multicastSupport) (\s@TransitGatewayOptions' {} a -> s {multicastSupport = a} :: TransitGatewayOptions)

-- | The ID of the default propagation route table.
transitGatewayOptions_propagationDefaultRouteTableId :: Lens.Lens' TransitGatewayOptions (Prelude.Maybe Prelude.Text)
transitGatewayOptions_propagationDefaultRouteTableId = Lens.lens (\TransitGatewayOptions' {propagationDefaultRouteTableId} -> propagationDefaultRouteTableId) (\s@TransitGatewayOptions' {} a -> s {propagationDefaultRouteTableId = a} :: TransitGatewayOptions)

-- | The transit gateway CIDR blocks.
transitGatewayOptions_transitGatewayCidrBlocks :: Lens.Lens' TransitGatewayOptions (Prelude.Maybe [Prelude.Text])
transitGatewayOptions_transitGatewayCidrBlocks = Lens.lens (\TransitGatewayOptions' {transitGatewayCidrBlocks} -> transitGatewayCidrBlocks) (\s@TransitGatewayOptions' {} a -> s {transitGatewayCidrBlocks = a} :: TransitGatewayOptions) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether Equal Cost Multipath Protocol support is enabled.
transitGatewayOptions_vpnEcmpSupport :: Lens.Lens' TransitGatewayOptions (Prelude.Maybe VpnEcmpSupportValue)
transitGatewayOptions_vpnEcmpSupport = Lens.lens (\TransitGatewayOptions' {vpnEcmpSupport} -> vpnEcmpSupport) (\s@TransitGatewayOptions' {} a -> s {vpnEcmpSupport = a} :: TransitGatewayOptions)

instance Data.FromXML TransitGatewayOptions where
  parseXML x =
    TransitGatewayOptions'
      Prelude.<$> (x Data..@? "amazonSideAsn")
      Prelude.<*> (x Data..@? "associationDefaultRouteTableId")
      Prelude.<*> (x Data..@? "autoAcceptSharedAttachments")
      Prelude.<*> (x Data..@? "defaultRouteTableAssociation")
      Prelude.<*> (x Data..@? "defaultRouteTablePropagation")
      Prelude.<*> (x Data..@? "dnsSupport")
      Prelude.<*> (x Data..@? "multicastSupport")
      Prelude.<*> (x Data..@? "propagationDefaultRouteTableId")
      Prelude.<*> ( x
                      Data..@? "transitGatewayCidrBlocks"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "vpnEcmpSupport")

instance Prelude.Hashable TransitGatewayOptions where
  hashWithSalt _salt TransitGatewayOptions' {..} =
    _salt
      `Prelude.hashWithSalt` amazonSideAsn
      `Prelude.hashWithSalt` associationDefaultRouteTableId
      `Prelude.hashWithSalt` autoAcceptSharedAttachments
      `Prelude.hashWithSalt` defaultRouteTableAssociation
      `Prelude.hashWithSalt` defaultRouteTablePropagation
      `Prelude.hashWithSalt` dnsSupport
      `Prelude.hashWithSalt` multicastSupport
      `Prelude.hashWithSalt` propagationDefaultRouteTableId
      `Prelude.hashWithSalt` transitGatewayCidrBlocks
      `Prelude.hashWithSalt` vpnEcmpSupport

instance Prelude.NFData TransitGatewayOptions where
  rnf TransitGatewayOptions' {..} =
    Prelude.rnf amazonSideAsn
      `Prelude.seq` Prelude.rnf associationDefaultRouteTableId
      `Prelude.seq` Prelude.rnf autoAcceptSharedAttachments
      `Prelude.seq` Prelude.rnf defaultRouteTableAssociation
      `Prelude.seq` Prelude.rnf defaultRouteTablePropagation
      `Prelude.seq` Prelude.rnf dnsSupport
      `Prelude.seq` Prelude.rnf multicastSupport
      `Prelude.seq` Prelude.rnf propagationDefaultRouteTableId
      `Prelude.seq` Prelude.rnf transitGatewayCidrBlocks
      `Prelude.seq` Prelude.rnf vpnEcmpSupport
