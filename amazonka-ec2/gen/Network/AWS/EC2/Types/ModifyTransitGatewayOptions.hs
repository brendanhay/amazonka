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
-- Module      : Network.AWS.EC2.Types.ModifyTransitGatewayOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ModifyTransitGatewayOptions where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.AutoAcceptSharedAttachmentsValue
import Network.AWS.EC2.Types.DefaultRouteTableAssociationValue
import Network.AWS.EC2.Types.DefaultRouteTablePropagationValue
import Network.AWS.EC2.Types.DnsSupportValue
import Network.AWS.EC2.Types.VpnEcmpSupportValue
import qualified Network.AWS.Lens as Lens

-- | The transit gateway options.
--
-- /See:/ 'newModifyTransitGatewayOptions' smart constructor.
data ModifyTransitGatewayOptions = ModifyTransitGatewayOptions'
  { -- | Removes CIDR blocks for the transit gateway.
    removeTransitGatewayCidrBlocks :: Core.Maybe [Core.Text],
    -- | The ID of the default propagation route table.
    propagationDefaultRouteTableId :: Core.Maybe Core.Text,
    -- | Enable or disable Equal Cost Multipath Protocol support.
    vpnEcmpSupport :: Core.Maybe VpnEcmpSupportValue,
    -- | Enable or disable DNS support.
    dnsSupport :: Core.Maybe DnsSupportValue,
    -- | Adds IPv4 or IPv6 CIDR blocks for the transit gateway. Must be a size
    -- \/24 CIDR block or larger for IPv4, or a size \/64 CIDR block or larger
    -- for IPv6.
    addTransitGatewayCidrBlocks :: Core.Maybe [Core.Text],
    -- | The ID of the default association route table.
    associationDefaultRouteTableId :: Core.Maybe Core.Text,
    -- | Enable or disable automatic acceptance of attachment requests.
    autoAcceptSharedAttachments :: Core.Maybe AutoAcceptSharedAttachmentsValue,
    -- | Enable or disable automatic association with the default association
    -- route table.
    defaultRouteTableAssociation :: Core.Maybe DefaultRouteTableAssociationValue,
    -- | Enable or disable automatic propagation of routes to the default
    -- propagation route table.
    defaultRouteTablePropagation :: Core.Maybe DefaultRouteTablePropagationValue
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyTransitGatewayOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'removeTransitGatewayCidrBlocks', 'modifyTransitGatewayOptions_removeTransitGatewayCidrBlocks' - Removes CIDR blocks for the transit gateway.
--
-- 'propagationDefaultRouteTableId', 'modifyTransitGatewayOptions_propagationDefaultRouteTableId' - The ID of the default propagation route table.
--
-- 'vpnEcmpSupport', 'modifyTransitGatewayOptions_vpnEcmpSupport' - Enable or disable Equal Cost Multipath Protocol support.
--
-- 'dnsSupport', 'modifyTransitGatewayOptions_dnsSupport' - Enable or disable DNS support.
--
-- 'addTransitGatewayCidrBlocks', 'modifyTransitGatewayOptions_addTransitGatewayCidrBlocks' - Adds IPv4 or IPv6 CIDR blocks for the transit gateway. Must be a size
-- \/24 CIDR block or larger for IPv4, or a size \/64 CIDR block or larger
-- for IPv6.
--
-- 'associationDefaultRouteTableId', 'modifyTransitGatewayOptions_associationDefaultRouteTableId' - The ID of the default association route table.
--
-- 'autoAcceptSharedAttachments', 'modifyTransitGatewayOptions_autoAcceptSharedAttachments' - Enable or disable automatic acceptance of attachment requests.
--
-- 'defaultRouteTableAssociation', 'modifyTransitGatewayOptions_defaultRouteTableAssociation' - Enable or disable automatic association with the default association
-- route table.
--
-- 'defaultRouteTablePropagation', 'modifyTransitGatewayOptions_defaultRouteTablePropagation' - Enable or disable automatic propagation of routes to the default
-- propagation route table.
newModifyTransitGatewayOptions ::
  ModifyTransitGatewayOptions
newModifyTransitGatewayOptions =
  ModifyTransitGatewayOptions'
    { removeTransitGatewayCidrBlocks =
        Core.Nothing,
      propagationDefaultRouteTableId = Core.Nothing,
      vpnEcmpSupport = Core.Nothing,
      dnsSupport = Core.Nothing,
      addTransitGatewayCidrBlocks = Core.Nothing,
      associationDefaultRouteTableId = Core.Nothing,
      autoAcceptSharedAttachments = Core.Nothing,
      defaultRouteTableAssociation = Core.Nothing,
      defaultRouteTablePropagation = Core.Nothing
    }

-- | Removes CIDR blocks for the transit gateway.
modifyTransitGatewayOptions_removeTransitGatewayCidrBlocks :: Lens.Lens' ModifyTransitGatewayOptions (Core.Maybe [Core.Text])
modifyTransitGatewayOptions_removeTransitGatewayCidrBlocks = Lens.lens (\ModifyTransitGatewayOptions' {removeTransitGatewayCidrBlocks} -> removeTransitGatewayCidrBlocks) (\s@ModifyTransitGatewayOptions' {} a -> s {removeTransitGatewayCidrBlocks = a} :: ModifyTransitGatewayOptions) Core.. Lens.mapping Lens._Coerce

-- | The ID of the default propagation route table.
modifyTransitGatewayOptions_propagationDefaultRouteTableId :: Lens.Lens' ModifyTransitGatewayOptions (Core.Maybe Core.Text)
modifyTransitGatewayOptions_propagationDefaultRouteTableId = Lens.lens (\ModifyTransitGatewayOptions' {propagationDefaultRouteTableId} -> propagationDefaultRouteTableId) (\s@ModifyTransitGatewayOptions' {} a -> s {propagationDefaultRouteTableId = a} :: ModifyTransitGatewayOptions)

-- | Enable or disable Equal Cost Multipath Protocol support.
modifyTransitGatewayOptions_vpnEcmpSupport :: Lens.Lens' ModifyTransitGatewayOptions (Core.Maybe VpnEcmpSupportValue)
modifyTransitGatewayOptions_vpnEcmpSupport = Lens.lens (\ModifyTransitGatewayOptions' {vpnEcmpSupport} -> vpnEcmpSupport) (\s@ModifyTransitGatewayOptions' {} a -> s {vpnEcmpSupport = a} :: ModifyTransitGatewayOptions)

-- | Enable or disable DNS support.
modifyTransitGatewayOptions_dnsSupport :: Lens.Lens' ModifyTransitGatewayOptions (Core.Maybe DnsSupportValue)
modifyTransitGatewayOptions_dnsSupport = Lens.lens (\ModifyTransitGatewayOptions' {dnsSupport} -> dnsSupport) (\s@ModifyTransitGatewayOptions' {} a -> s {dnsSupport = a} :: ModifyTransitGatewayOptions)

-- | Adds IPv4 or IPv6 CIDR blocks for the transit gateway. Must be a size
-- \/24 CIDR block or larger for IPv4, or a size \/64 CIDR block or larger
-- for IPv6.
modifyTransitGatewayOptions_addTransitGatewayCidrBlocks :: Lens.Lens' ModifyTransitGatewayOptions (Core.Maybe [Core.Text])
modifyTransitGatewayOptions_addTransitGatewayCidrBlocks = Lens.lens (\ModifyTransitGatewayOptions' {addTransitGatewayCidrBlocks} -> addTransitGatewayCidrBlocks) (\s@ModifyTransitGatewayOptions' {} a -> s {addTransitGatewayCidrBlocks = a} :: ModifyTransitGatewayOptions) Core.. Lens.mapping Lens._Coerce

-- | The ID of the default association route table.
modifyTransitGatewayOptions_associationDefaultRouteTableId :: Lens.Lens' ModifyTransitGatewayOptions (Core.Maybe Core.Text)
modifyTransitGatewayOptions_associationDefaultRouteTableId = Lens.lens (\ModifyTransitGatewayOptions' {associationDefaultRouteTableId} -> associationDefaultRouteTableId) (\s@ModifyTransitGatewayOptions' {} a -> s {associationDefaultRouteTableId = a} :: ModifyTransitGatewayOptions)

-- | Enable or disable automatic acceptance of attachment requests.
modifyTransitGatewayOptions_autoAcceptSharedAttachments :: Lens.Lens' ModifyTransitGatewayOptions (Core.Maybe AutoAcceptSharedAttachmentsValue)
modifyTransitGatewayOptions_autoAcceptSharedAttachments = Lens.lens (\ModifyTransitGatewayOptions' {autoAcceptSharedAttachments} -> autoAcceptSharedAttachments) (\s@ModifyTransitGatewayOptions' {} a -> s {autoAcceptSharedAttachments = a} :: ModifyTransitGatewayOptions)

-- | Enable or disable automatic association with the default association
-- route table.
modifyTransitGatewayOptions_defaultRouteTableAssociation :: Lens.Lens' ModifyTransitGatewayOptions (Core.Maybe DefaultRouteTableAssociationValue)
modifyTransitGatewayOptions_defaultRouteTableAssociation = Lens.lens (\ModifyTransitGatewayOptions' {defaultRouteTableAssociation} -> defaultRouteTableAssociation) (\s@ModifyTransitGatewayOptions' {} a -> s {defaultRouteTableAssociation = a} :: ModifyTransitGatewayOptions)

-- | Enable or disable automatic propagation of routes to the default
-- propagation route table.
modifyTransitGatewayOptions_defaultRouteTablePropagation :: Lens.Lens' ModifyTransitGatewayOptions (Core.Maybe DefaultRouteTablePropagationValue)
modifyTransitGatewayOptions_defaultRouteTablePropagation = Lens.lens (\ModifyTransitGatewayOptions' {defaultRouteTablePropagation} -> defaultRouteTablePropagation) (\s@ModifyTransitGatewayOptions' {} a -> s {defaultRouteTablePropagation = a} :: ModifyTransitGatewayOptions)

instance Core.Hashable ModifyTransitGatewayOptions

instance Core.NFData ModifyTransitGatewayOptions

instance Core.ToQuery ModifyTransitGatewayOptions where
  toQuery ModifyTransitGatewayOptions' {..} =
    Core.mconcat
      [ Core.toQuery
          ( Core.toQueryList "RemoveTransitGatewayCidrBlocks"
              Core.<$> removeTransitGatewayCidrBlocks
          ),
        "PropagationDefaultRouteTableId"
          Core.=: propagationDefaultRouteTableId,
        "VpnEcmpSupport" Core.=: vpnEcmpSupport,
        "DnsSupport" Core.=: dnsSupport,
        Core.toQuery
          ( Core.toQueryList "AddTransitGatewayCidrBlocks"
              Core.<$> addTransitGatewayCidrBlocks
          ),
        "AssociationDefaultRouteTableId"
          Core.=: associationDefaultRouteTableId,
        "AutoAcceptSharedAttachments"
          Core.=: autoAcceptSharedAttachments,
        "DefaultRouteTableAssociation"
          Core.=: defaultRouteTableAssociation,
        "DefaultRouteTablePropagation"
          Core.=: defaultRouteTablePropagation
      ]
