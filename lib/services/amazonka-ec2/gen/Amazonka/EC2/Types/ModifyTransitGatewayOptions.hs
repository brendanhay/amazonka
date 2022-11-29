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
-- Module      : Amazonka.EC2.Types.ModifyTransitGatewayOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ModifyTransitGatewayOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.AutoAcceptSharedAttachmentsValue
import Amazonka.EC2.Types.DefaultRouteTableAssociationValue
import Amazonka.EC2.Types.DefaultRouteTablePropagationValue
import Amazonka.EC2.Types.DnsSupportValue
import Amazonka.EC2.Types.VpnEcmpSupportValue
import qualified Amazonka.Prelude as Prelude

-- | The transit gateway options.
--
-- /See:/ 'newModifyTransitGatewayOptions' smart constructor.
data ModifyTransitGatewayOptions = ModifyTransitGatewayOptions'
  { -- | The ID of the default association route table.
    associationDefaultRouteTableId :: Prelude.Maybe Prelude.Text,
    -- | Enable or disable DNS support.
    dnsSupport :: Prelude.Maybe DnsSupportValue,
    -- | Enable or disable automatic association with the default association
    -- route table.
    defaultRouteTableAssociation :: Prelude.Maybe DefaultRouteTableAssociationValue,
    -- | Removes CIDR blocks for the transit gateway.
    removeTransitGatewayCidrBlocks :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the default propagation route table.
    propagationDefaultRouteTableId :: Prelude.Maybe Prelude.Text,
    -- | Enable or disable automatic acceptance of attachment requests.
    autoAcceptSharedAttachments :: Prelude.Maybe AutoAcceptSharedAttachmentsValue,
    -- | A private Autonomous System Number (ASN) for the Amazon side of a BGP
    -- session. The range is 64512 to 65534 for 16-bit ASNs and 4200000000 to
    -- 4294967294 for 32-bit ASNs.
    --
    -- The modify ASN operation is not allowed on a transit gateway with active
    -- BGP sessions. You must first delete all transit gateway attachments that
    -- have BGP configured prior to modifying the ASN on the transit gateway.
    amazonSideAsn :: Prelude.Maybe Prelude.Integer,
    -- | Enable or disable Equal Cost Multipath Protocol support.
    vpnEcmpSupport :: Prelude.Maybe VpnEcmpSupportValue,
    -- | Enable or disable automatic propagation of routes to the default
    -- propagation route table.
    defaultRouteTablePropagation :: Prelude.Maybe DefaultRouteTablePropagationValue,
    -- | Adds IPv4 or IPv6 CIDR blocks for the transit gateway. Must be a size
    -- \/24 CIDR block or larger for IPv4, or a size \/64 CIDR block or larger
    -- for IPv6.
    addTransitGatewayCidrBlocks :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyTransitGatewayOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationDefaultRouteTableId', 'modifyTransitGatewayOptions_associationDefaultRouteTableId' - The ID of the default association route table.
--
-- 'dnsSupport', 'modifyTransitGatewayOptions_dnsSupport' - Enable or disable DNS support.
--
-- 'defaultRouteTableAssociation', 'modifyTransitGatewayOptions_defaultRouteTableAssociation' - Enable or disable automatic association with the default association
-- route table.
--
-- 'removeTransitGatewayCidrBlocks', 'modifyTransitGatewayOptions_removeTransitGatewayCidrBlocks' - Removes CIDR blocks for the transit gateway.
--
-- 'propagationDefaultRouteTableId', 'modifyTransitGatewayOptions_propagationDefaultRouteTableId' - The ID of the default propagation route table.
--
-- 'autoAcceptSharedAttachments', 'modifyTransitGatewayOptions_autoAcceptSharedAttachments' - Enable or disable automatic acceptance of attachment requests.
--
-- 'amazonSideAsn', 'modifyTransitGatewayOptions_amazonSideAsn' - A private Autonomous System Number (ASN) for the Amazon side of a BGP
-- session. The range is 64512 to 65534 for 16-bit ASNs and 4200000000 to
-- 4294967294 for 32-bit ASNs.
--
-- The modify ASN operation is not allowed on a transit gateway with active
-- BGP sessions. You must first delete all transit gateway attachments that
-- have BGP configured prior to modifying the ASN on the transit gateway.
--
-- 'vpnEcmpSupport', 'modifyTransitGatewayOptions_vpnEcmpSupport' - Enable or disable Equal Cost Multipath Protocol support.
--
-- 'defaultRouteTablePropagation', 'modifyTransitGatewayOptions_defaultRouteTablePropagation' - Enable or disable automatic propagation of routes to the default
-- propagation route table.
--
-- 'addTransitGatewayCidrBlocks', 'modifyTransitGatewayOptions_addTransitGatewayCidrBlocks' - Adds IPv4 or IPv6 CIDR blocks for the transit gateway. Must be a size
-- \/24 CIDR block or larger for IPv4, or a size \/64 CIDR block or larger
-- for IPv6.
newModifyTransitGatewayOptions ::
  ModifyTransitGatewayOptions
newModifyTransitGatewayOptions =
  ModifyTransitGatewayOptions'
    { associationDefaultRouteTableId =
        Prelude.Nothing,
      dnsSupport = Prelude.Nothing,
      defaultRouteTableAssociation = Prelude.Nothing,
      removeTransitGatewayCidrBlocks =
        Prelude.Nothing,
      propagationDefaultRouteTableId =
        Prelude.Nothing,
      autoAcceptSharedAttachments = Prelude.Nothing,
      amazonSideAsn = Prelude.Nothing,
      vpnEcmpSupport = Prelude.Nothing,
      defaultRouteTablePropagation = Prelude.Nothing,
      addTransitGatewayCidrBlocks = Prelude.Nothing
    }

-- | The ID of the default association route table.
modifyTransitGatewayOptions_associationDefaultRouteTableId :: Lens.Lens' ModifyTransitGatewayOptions (Prelude.Maybe Prelude.Text)
modifyTransitGatewayOptions_associationDefaultRouteTableId = Lens.lens (\ModifyTransitGatewayOptions' {associationDefaultRouteTableId} -> associationDefaultRouteTableId) (\s@ModifyTransitGatewayOptions' {} a -> s {associationDefaultRouteTableId = a} :: ModifyTransitGatewayOptions)

-- | Enable or disable DNS support.
modifyTransitGatewayOptions_dnsSupport :: Lens.Lens' ModifyTransitGatewayOptions (Prelude.Maybe DnsSupportValue)
modifyTransitGatewayOptions_dnsSupport = Lens.lens (\ModifyTransitGatewayOptions' {dnsSupport} -> dnsSupport) (\s@ModifyTransitGatewayOptions' {} a -> s {dnsSupport = a} :: ModifyTransitGatewayOptions)

-- | Enable or disable automatic association with the default association
-- route table.
modifyTransitGatewayOptions_defaultRouteTableAssociation :: Lens.Lens' ModifyTransitGatewayOptions (Prelude.Maybe DefaultRouteTableAssociationValue)
modifyTransitGatewayOptions_defaultRouteTableAssociation = Lens.lens (\ModifyTransitGatewayOptions' {defaultRouteTableAssociation} -> defaultRouteTableAssociation) (\s@ModifyTransitGatewayOptions' {} a -> s {defaultRouteTableAssociation = a} :: ModifyTransitGatewayOptions)

-- | Removes CIDR blocks for the transit gateway.
modifyTransitGatewayOptions_removeTransitGatewayCidrBlocks :: Lens.Lens' ModifyTransitGatewayOptions (Prelude.Maybe [Prelude.Text])
modifyTransitGatewayOptions_removeTransitGatewayCidrBlocks = Lens.lens (\ModifyTransitGatewayOptions' {removeTransitGatewayCidrBlocks} -> removeTransitGatewayCidrBlocks) (\s@ModifyTransitGatewayOptions' {} a -> s {removeTransitGatewayCidrBlocks = a} :: ModifyTransitGatewayOptions) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the default propagation route table.
modifyTransitGatewayOptions_propagationDefaultRouteTableId :: Lens.Lens' ModifyTransitGatewayOptions (Prelude.Maybe Prelude.Text)
modifyTransitGatewayOptions_propagationDefaultRouteTableId = Lens.lens (\ModifyTransitGatewayOptions' {propagationDefaultRouteTableId} -> propagationDefaultRouteTableId) (\s@ModifyTransitGatewayOptions' {} a -> s {propagationDefaultRouteTableId = a} :: ModifyTransitGatewayOptions)

-- | Enable or disable automatic acceptance of attachment requests.
modifyTransitGatewayOptions_autoAcceptSharedAttachments :: Lens.Lens' ModifyTransitGatewayOptions (Prelude.Maybe AutoAcceptSharedAttachmentsValue)
modifyTransitGatewayOptions_autoAcceptSharedAttachments = Lens.lens (\ModifyTransitGatewayOptions' {autoAcceptSharedAttachments} -> autoAcceptSharedAttachments) (\s@ModifyTransitGatewayOptions' {} a -> s {autoAcceptSharedAttachments = a} :: ModifyTransitGatewayOptions)

-- | A private Autonomous System Number (ASN) for the Amazon side of a BGP
-- session. The range is 64512 to 65534 for 16-bit ASNs and 4200000000 to
-- 4294967294 for 32-bit ASNs.
--
-- The modify ASN operation is not allowed on a transit gateway with active
-- BGP sessions. You must first delete all transit gateway attachments that
-- have BGP configured prior to modifying the ASN on the transit gateway.
modifyTransitGatewayOptions_amazonSideAsn :: Lens.Lens' ModifyTransitGatewayOptions (Prelude.Maybe Prelude.Integer)
modifyTransitGatewayOptions_amazonSideAsn = Lens.lens (\ModifyTransitGatewayOptions' {amazonSideAsn} -> amazonSideAsn) (\s@ModifyTransitGatewayOptions' {} a -> s {amazonSideAsn = a} :: ModifyTransitGatewayOptions)

-- | Enable or disable Equal Cost Multipath Protocol support.
modifyTransitGatewayOptions_vpnEcmpSupport :: Lens.Lens' ModifyTransitGatewayOptions (Prelude.Maybe VpnEcmpSupportValue)
modifyTransitGatewayOptions_vpnEcmpSupport = Lens.lens (\ModifyTransitGatewayOptions' {vpnEcmpSupport} -> vpnEcmpSupport) (\s@ModifyTransitGatewayOptions' {} a -> s {vpnEcmpSupport = a} :: ModifyTransitGatewayOptions)

-- | Enable or disable automatic propagation of routes to the default
-- propagation route table.
modifyTransitGatewayOptions_defaultRouteTablePropagation :: Lens.Lens' ModifyTransitGatewayOptions (Prelude.Maybe DefaultRouteTablePropagationValue)
modifyTransitGatewayOptions_defaultRouteTablePropagation = Lens.lens (\ModifyTransitGatewayOptions' {defaultRouteTablePropagation} -> defaultRouteTablePropagation) (\s@ModifyTransitGatewayOptions' {} a -> s {defaultRouteTablePropagation = a} :: ModifyTransitGatewayOptions)

-- | Adds IPv4 or IPv6 CIDR blocks for the transit gateway. Must be a size
-- \/24 CIDR block or larger for IPv4, or a size \/64 CIDR block or larger
-- for IPv6.
modifyTransitGatewayOptions_addTransitGatewayCidrBlocks :: Lens.Lens' ModifyTransitGatewayOptions (Prelude.Maybe [Prelude.Text])
modifyTransitGatewayOptions_addTransitGatewayCidrBlocks = Lens.lens (\ModifyTransitGatewayOptions' {addTransitGatewayCidrBlocks} -> addTransitGatewayCidrBlocks) (\s@ModifyTransitGatewayOptions' {} a -> s {addTransitGatewayCidrBlocks = a} :: ModifyTransitGatewayOptions) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable ModifyTransitGatewayOptions where
  hashWithSalt _salt ModifyTransitGatewayOptions' {..} =
    _salt
      `Prelude.hashWithSalt` associationDefaultRouteTableId
      `Prelude.hashWithSalt` dnsSupport
      `Prelude.hashWithSalt` defaultRouteTableAssociation
      `Prelude.hashWithSalt` removeTransitGatewayCidrBlocks
      `Prelude.hashWithSalt` propagationDefaultRouteTableId
      `Prelude.hashWithSalt` autoAcceptSharedAttachments
      `Prelude.hashWithSalt` amazonSideAsn
      `Prelude.hashWithSalt` vpnEcmpSupport
      `Prelude.hashWithSalt` defaultRouteTablePropagation
      `Prelude.hashWithSalt` addTransitGatewayCidrBlocks

instance Prelude.NFData ModifyTransitGatewayOptions where
  rnf ModifyTransitGatewayOptions' {..} =
    Prelude.rnf associationDefaultRouteTableId
      `Prelude.seq` Prelude.rnf dnsSupport
      `Prelude.seq` Prelude.rnf defaultRouteTableAssociation
      `Prelude.seq` Prelude.rnf removeTransitGatewayCidrBlocks
      `Prelude.seq` Prelude.rnf propagationDefaultRouteTableId
      `Prelude.seq` Prelude.rnf autoAcceptSharedAttachments
      `Prelude.seq` Prelude.rnf amazonSideAsn
      `Prelude.seq` Prelude.rnf vpnEcmpSupport
      `Prelude.seq` Prelude.rnf defaultRouteTablePropagation
      `Prelude.seq` Prelude.rnf addTransitGatewayCidrBlocks

instance Core.ToQuery ModifyTransitGatewayOptions where
  toQuery ModifyTransitGatewayOptions' {..} =
    Prelude.mconcat
      [ "AssociationDefaultRouteTableId"
          Core.=: associationDefaultRouteTableId,
        "DnsSupport" Core.=: dnsSupport,
        "DefaultRouteTableAssociation"
          Core.=: defaultRouteTableAssociation,
        Core.toQuery
          ( Core.toQueryList "RemoveTransitGatewayCidrBlocks"
              Prelude.<$> removeTransitGatewayCidrBlocks
          ),
        "PropagationDefaultRouteTableId"
          Core.=: propagationDefaultRouteTableId,
        "AutoAcceptSharedAttachments"
          Core.=: autoAcceptSharedAttachments,
        "AmazonSideAsn" Core.=: amazonSideAsn,
        "VpnEcmpSupport" Core.=: vpnEcmpSupport,
        "DefaultRouteTablePropagation"
          Core.=: defaultRouteTablePropagation,
        Core.toQuery
          ( Core.toQueryList "AddTransitGatewayCidrBlocks"
              Prelude.<$> addTransitGatewayCidrBlocks
          )
      ]
