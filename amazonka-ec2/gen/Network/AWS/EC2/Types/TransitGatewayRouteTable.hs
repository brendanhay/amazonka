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
-- Module      : Network.AWS.EC2.Types.TransitGatewayRouteTable
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayRouteTable where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.TransitGatewayRouteTableState
import qualified Network.AWS.Lens as Lens

-- | Describes a transit gateway route table.
--
-- /See:/ 'newTransitGatewayRouteTable' smart constructor.
data TransitGatewayRouteTable = TransitGatewayRouteTable'
  { -- | The creation time.
    creationTime :: Core.Maybe Core.ISO8601,
    -- | Indicates whether this is the default association route table for the
    -- transit gateway.
    defaultAssociationRouteTable :: Core.Maybe Core.Bool,
    -- | Indicates whether this is the default propagation route table for the
    -- transit gateway.
    defaultPropagationRouteTable :: Core.Maybe Core.Bool,
    -- | The state of the transit gateway route table.
    state :: Core.Maybe TransitGatewayRouteTableState,
    -- | Any tags assigned to the route table.
    tags :: Core.Maybe [Tag],
    -- | The ID of the transit gateway route table.
    transitGatewayRouteTableId :: Core.Maybe Core.Text,
    -- | The ID of the transit gateway.
    transitGatewayId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TransitGatewayRouteTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'transitGatewayRouteTable_creationTime' - The creation time.
--
-- 'defaultAssociationRouteTable', 'transitGatewayRouteTable_defaultAssociationRouteTable' - Indicates whether this is the default association route table for the
-- transit gateway.
--
-- 'defaultPropagationRouteTable', 'transitGatewayRouteTable_defaultPropagationRouteTable' - Indicates whether this is the default propagation route table for the
-- transit gateway.
--
-- 'state', 'transitGatewayRouteTable_state' - The state of the transit gateway route table.
--
-- 'tags', 'transitGatewayRouteTable_tags' - Any tags assigned to the route table.
--
-- 'transitGatewayRouteTableId', 'transitGatewayRouteTable_transitGatewayRouteTableId' - The ID of the transit gateway route table.
--
-- 'transitGatewayId', 'transitGatewayRouteTable_transitGatewayId' - The ID of the transit gateway.
newTransitGatewayRouteTable ::
  TransitGatewayRouteTable
newTransitGatewayRouteTable =
  TransitGatewayRouteTable'
    { creationTime =
        Core.Nothing,
      defaultAssociationRouteTable = Core.Nothing,
      defaultPropagationRouteTable = Core.Nothing,
      state = Core.Nothing,
      tags = Core.Nothing,
      transitGatewayRouteTableId = Core.Nothing,
      transitGatewayId = Core.Nothing
    }

-- | The creation time.
transitGatewayRouteTable_creationTime :: Lens.Lens' TransitGatewayRouteTable (Core.Maybe Core.UTCTime)
transitGatewayRouteTable_creationTime = Lens.lens (\TransitGatewayRouteTable' {creationTime} -> creationTime) (\s@TransitGatewayRouteTable' {} a -> s {creationTime = a} :: TransitGatewayRouteTable) Core.. Lens.mapping Core._Time

-- | Indicates whether this is the default association route table for the
-- transit gateway.
transitGatewayRouteTable_defaultAssociationRouteTable :: Lens.Lens' TransitGatewayRouteTable (Core.Maybe Core.Bool)
transitGatewayRouteTable_defaultAssociationRouteTable = Lens.lens (\TransitGatewayRouteTable' {defaultAssociationRouteTable} -> defaultAssociationRouteTable) (\s@TransitGatewayRouteTable' {} a -> s {defaultAssociationRouteTable = a} :: TransitGatewayRouteTable)

-- | Indicates whether this is the default propagation route table for the
-- transit gateway.
transitGatewayRouteTable_defaultPropagationRouteTable :: Lens.Lens' TransitGatewayRouteTable (Core.Maybe Core.Bool)
transitGatewayRouteTable_defaultPropagationRouteTable = Lens.lens (\TransitGatewayRouteTable' {defaultPropagationRouteTable} -> defaultPropagationRouteTable) (\s@TransitGatewayRouteTable' {} a -> s {defaultPropagationRouteTable = a} :: TransitGatewayRouteTable)

-- | The state of the transit gateway route table.
transitGatewayRouteTable_state :: Lens.Lens' TransitGatewayRouteTable (Core.Maybe TransitGatewayRouteTableState)
transitGatewayRouteTable_state = Lens.lens (\TransitGatewayRouteTable' {state} -> state) (\s@TransitGatewayRouteTable' {} a -> s {state = a} :: TransitGatewayRouteTable)

-- | Any tags assigned to the route table.
transitGatewayRouteTable_tags :: Lens.Lens' TransitGatewayRouteTable (Core.Maybe [Tag])
transitGatewayRouteTable_tags = Lens.lens (\TransitGatewayRouteTable' {tags} -> tags) (\s@TransitGatewayRouteTable' {} a -> s {tags = a} :: TransitGatewayRouteTable) Core.. Lens.mapping Lens._Coerce

-- | The ID of the transit gateway route table.
transitGatewayRouteTable_transitGatewayRouteTableId :: Lens.Lens' TransitGatewayRouteTable (Core.Maybe Core.Text)
transitGatewayRouteTable_transitGatewayRouteTableId = Lens.lens (\TransitGatewayRouteTable' {transitGatewayRouteTableId} -> transitGatewayRouteTableId) (\s@TransitGatewayRouteTable' {} a -> s {transitGatewayRouteTableId = a} :: TransitGatewayRouteTable)

-- | The ID of the transit gateway.
transitGatewayRouteTable_transitGatewayId :: Lens.Lens' TransitGatewayRouteTable (Core.Maybe Core.Text)
transitGatewayRouteTable_transitGatewayId = Lens.lens (\TransitGatewayRouteTable' {transitGatewayId} -> transitGatewayId) (\s@TransitGatewayRouteTable' {} a -> s {transitGatewayId = a} :: TransitGatewayRouteTable)

instance Core.FromXML TransitGatewayRouteTable where
  parseXML x =
    TransitGatewayRouteTable'
      Core.<$> (x Core..@? "creationTime")
      Core.<*> (x Core..@? "defaultAssociationRouteTable")
      Core.<*> (x Core..@? "defaultPropagationRouteTable")
      Core.<*> (x Core..@? "state")
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "transitGatewayRouteTableId")
      Core.<*> (x Core..@? "transitGatewayId")

instance Core.Hashable TransitGatewayRouteTable

instance Core.NFData TransitGatewayRouteTable
