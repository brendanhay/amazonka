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
-- Module      : Network.AWS.EC2.Types.LocalGatewayRouteTableVirtualInterfaceGroupAssociation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LocalGatewayRouteTableVirtualInterfaceGroupAssociation where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | Describes an association between a local gateway route table and a
-- virtual interface group.
--
-- /See:/ 'newLocalGatewayRouteTableVirtualInterfaceGroupAssociation' smart constructor.
data LocalGatewayRouteTableVirtualInterfaceGroupAssociation = LocalGatewayRouteTableVirtualInterfaceGroupAssociation'
  { -- | The AWS account ID that owns the local gateway virtual interface group
    -- association.
    ownerId :: Core.Maybe Core.Text,
    -- | The ID of the virtual interface group.
    localGatewayVirtualInterfaceGroupId :: Core.Maybe Core.Text,
    -- | The ID of the local gateway.
    localGatewayId :: Core.Maybe Core.Text,
    -- | The ID of the association.
    localGatewayRouteTableVirtualInterfaceGroupAssociationId :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the local gateway route table for the
    -- virtual interface group.
    localGatewayRouteTableArn :: Core.Maybe Core.Text,
    -- | The state of the association.
    state :: Core.Maybe Core.Text,
    -- | The ID of the local gateway route table.
    localGatewayRouteTableId :: Core.Maybe Core.Text,
    -- | The tags assigned to the association.
    tags :: Core.Maybe [Tag]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LocalGatewayRouteTableVirtualInterfaceGroupAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerId', 'localGatewayRouteTableVirtualInterfaceGroupAssociation_ownerId' - The AWS account ID that owns the local gateway virtual interface group
-- association.
--
-- 'localGatewayVirtualInterfaceGroupId', 'localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayVirtualInterfaceGroupId' - The ID of the virtual interface group.
--
-- 'localGatewayId', 'localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayId' - The ID of the local gateway.
--
-- 'localGatewayRouteTableVirtualInterfaceGroupAssociationId', 'localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableVirtualInterfaceGroupAssociationId' - The ID of the association.
--
-- 'localGatewayRouteTableArn', 'localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableArn' - The Amazon Resource Name (ARN) of the local gateway route table for the
-- virtual interface group.
--
-- 'state', 'localGatewayRouteTableVirtualInterfaceGroupAssociation_state' - The state of the association.
--
-- 'localGatewayRouteTableId', 'localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableId' - The ID of the local gateway route table.
--
-- 'tags', 'localGatewayRouteTableVirtualInterfaceGroupAssociation_tags' - The tags assigned to the association.
newLocalGatewayRouteTableVirtualInterfaceGroupAssociation ::
  LocalGatewayRouteTableVirtualInterfaceGroupAssociation
newLocalGatewayRouteTableVirtualInterfaceGroupAssociation =
  LocalGatewayRouteTableVirtualInterfaceGroupAssociation'
    { ownerId =
        Core.Nothing,
      localGatewayVirtualInterfaceGroupId =
        Core.Nothing,
      localGatewayId =
        Core.Nothing,
      localGatewayRouteTableVirtualInterfaceGroupAssociationId =
        Core.Nothing,
      localGatewayRouteTableArn =
        Core.Nothing,
      state =
        Core.Nothing,
      localGatewayRouteTableId =
        Core.Nothing,
      tags = Core.Nothing
    }

-- | The AWS account ID that owns the local gateway virtual interface group
-- association.
localGatewayRouteTableVirtualInterfaceGroupAssociation_ownerId :: Lens.Lens' LocalGatewayRouteTableVirtualInterfaceGroupAssociation (Core.Maybe Core.Text)
localGatewayRouteTableVirtualInterfaceGroupAssociation_ownerId = Lens.lens (\LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {ownerId} -> ownerId) (\s@LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {} a -> s {ownerId = a} :: LocalGatewayRouteTableVirtualInterfaceGroupAssociation)

-- | The ID of the virtual interface group.
localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayVirtualInterfaceGroupId :: Lens.Lens' LocalGatewayRouteTableVirtualInterfaceGroupAssociation (Core.Maybe Core.Text)
localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayVirtualInterfaceGroupId = Lens.lens (\LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {localGatewayVirtualInterfaceGroupId} -> localGatewayVirtualInterfaceGroupId) (\s@LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {} a -> s {localGatewayVirtualInterfaceGroupId = a} :: LocalGatewayRouteTableVirtualInterfaceGroupAssociation)

-- | The ID of the local gateway.
localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayId :: Lens.Lens' LocalGatewayRouteTableVirtualInterfaceGroupAssociation (Core.Maybe Core.Text)
localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayId = Lens.lens (\LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {localGatewayId} -> localGatewayId) (\s@LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {} a -> s {localGatewayId = a} :: LocalGatewayRouteTableVirtualInterfaceGroupAssociation)

-- | The ID of the association.
localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableVirtualInterfaceGroupAssociationId :: Lens.Lens' LocalGatewayRouteTableVirtualInterfaceGroupAssociation (Core.Maybe Core.Text)
localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableVirtualInterfaceGroupAssociationId = Lens.lens (\LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {localGatewayRouteTableVirtualInterfaceGroupAssociationId} -> localGatewayRouteTableVirtualInterfaceGroupAssociationId) (\s@LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {} a -> s {localGatewayRouteTableVirtualInterfaceGroupAssociationId = a} :: LocalGatewayRouteTableVirtualInterfaceGroupAssociation)

-- | The Amazon Resource Name (ARN) of the local gateway route table for the
-- virtual interface group.
localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableArn :: Lens.Lens' LocalGatewayRouteTableVirtualInterfaceGroupAssociation (Core.Maybe Core.Text)
localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableArn = Lens.lens (\LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {localGatewayRouteTableArn} -> localGatewayRouteTableArn) (\s@LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {} a -> s {localGatewayRouteTableArn = a} :: LocalGatewayRouteTableVirtualInterfaceGroupAssociation)

-- | The state of the association.
localGatewayRouteTableVirtualInterfaceGroupAssociation_state :: Lens.Lens' LocalGatewayRouteTableVirtualInterfaceGroupAssociation (Core.Maybe Core.Text)
localGatewayRouteTableVirtualInterfaceGroupAssociation_state = Lens.lens (\LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {state} -> state) (\s@LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {} a -> s {state = a} :: LocalGatewayRouteTableVirtualInterfaceGroupAssociation)

-- | The ID of the local gateway route table.
localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableId :: Lens.Lens' LocalGatewayRouteTableVirtualInterfaceGroupAssociation (Core.Maybe Core.Text)
localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableId = Lens.lens (\LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {localGatewayRouteTableId} -> localGatewayRouteTableId) (\s@LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {} a -> s {localGatewayRouteTableId = a} :: LocalGatewayRouteTableVirtualInterfaceGroupAssociation)

-- | The tags assigned to the association.
localGatewayRouteTableVirtualInterfaceGroupAssociation_tags :: Lens.Lens' LocalGatewayRouteTableVirtualInterfaceGroupAssociation (Core.Maybe [Tag])
localGatewayRouteTableVirtualInterfaceGroupAssociation_tags = Lens.lens (\LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {tags} -> tags) (\s@LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {} a -> s {tags = a} :: LocalGatewayRouteTableVirtualInterfaceGroupAssociation) Core.. Lens.mapping Lens._Coerce

instance
  Core.FromXML
    LocalGatewayRouteTableVirtualInterfaceGroupAssociation
  where
  parseXML x =
    LocalGatewayRouteTableVirtualInterfaceGroupAssociation'
      Core.<$> (x Core..@? "ownerId")
        Core.<*> (x Core..@? "localGatewayVirtualInterfaceGroupId")
        Core.<*> (x Core..@? "localGatewayId")
        Core.<*> ( x
                     Core..@? "localGatewayRouteTableVirtualInterfaceGroupAssociationId"
                 )
        Core.<*> (x Core..@? "localGatewayRouteTableArn")
        Core.<*> (x Core..@? "state")
        Core.<*> (x Core..@? "localGatewayRouteTableId")
        Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                     Core.>>= Core.may (Core.parseXMLList "item")
                 )

instance
  Core.Hashable
    LocalGatewayRouteTableVirtualInterfaceGroupAssociation

instance
  Core.NFData
    LocalGatewayRouteTableVirtualInterfaceGroupAssociation
