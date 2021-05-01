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
-- Module      : Network.AWS.EC2.Types.TransitGatewayRouteTable
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayRouteTable where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.TransitGatewayRouteTableState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a transit gateway route table.
--
-- /See:/ 'newTransitGatewayRouteTable' smart constructor.
data TransitGatewayRouteTable = TransitGatewayRouteTable'
  { -- | The creation time.
    creationTime :: Prelude.Maybe Prelude.ISO8601,
    -- | Indicates whether this is the default association route table for the
    -- transit gateway.
    defaultAssociationRouteTable :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether this is the default propagation route table for the
    -- transit gateway.
    defaultPropagationRouteTable :: Prelude.Maybe Prelude.Bool,
    -- | The state of the transit gateway route table.
    state :: Prelude.Maybe TransitGatewayRouteTableState,
    -- | Any tags assigned to the route table.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of the transit gateway route table.
    transitGatewayRouteTableId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the transit gateway.
    transitGatewayId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      defaultAssociationRouteTable = Prelude.Nothing,
      defaultPropagationRouteTable = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing,
      transitGatewayRouteTableId = Prelude.Nothing,
      transitGatewayId = Prelude.Nothing
    }

-- | The creation time.
transitGatewayRouteTable_creationTime :: Lens.Lens' TransitGatewayRouteTable (Prelude.Maybe Prelude.UTCTime)
transitGatewayRouteTable_creationTime = Lens.lens (\TransitGatewayRouteTable' {creationTime} -> creationTime) (\s@TransitGatewayRouteTable' {} a -> s {creationTime = a} :: TransitGatewayRouteTable) Prelude.. Lens.mapping Prelude._Time

-- | Indicates whether this is the default association route table for the
-- transit gateway.
transitGatewayRouteTable_defaultAssociationRouteTable :: Lens.Lens' TransitGatewayRouteTable (Prelude.Maybe Prelude.Bool)
transitGatewayRouteTable_defaultAssociationRouteTable = Lens.lens (\TransitGatewayRouteTable' {defaultAssociationRouteTable} -> defaultAssociationRouteTable) (\s@TransitGatewayRouteTable' {} a -> s {defaultAssociationRouteTable = a} :: TransitGatewayRouteTable)

-- | Indicates whether this is the default propagation route table for the
-- transit gateway.
transitGatewayRouteTable_defaultPropagationRouteTable :: Lens.Lens' TransitGatewayRouteTable (Prelude.Maybe Prelude.Bool)
transitGatewayRouteTable_defaultPropagationRouteTable = Lens.lens (\TransitGatewayRouteTable' {defaultPropagationRouteTable} -> defaultPropagationRouteTable) (\s@TransitGatewayRouteTable' {} a -> s {defaultPropagationRouteTable = a} :: TransitGatewayRouteTable)

-- | The state of the transit gateway route table.
transitGatewayRouteTable_state :: Lens.Lens' TransitGatewayRouteTable (Prelude.Maybe TransitGatewayRouteTableState)
transitGatewayRouteTable_state = Lens.lens (\TransitGatewayRouteTable' {state} -> state) (\s@TransitGatewayRouteTable' {} a -> s {state = a} :: TransitGatewayRouteTable)

-- | Any tags assigned to the route table.
transitGatewayRouteTable_tags :: Lens.Lens' TransitGatewayRouteTable (Prelude.Maybe [Tag])
transitGatewayRouteTable_tags = Lens.lens (\TransitGatewayRouteTable' {tags} -> tags) (\s@TransitGatewayRouteTable' {} a -> s {tags = a} :: TransitGatewayRouteTable) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of the transit gateway route table.
transitGatewayRouteTable_transitGatewayRouteTableId :: Lens.Lens' TransitGatewayRouteTable (Prelude.Maybe Prelude.Text)
transitGatewayRouteTable_transitGatewayRouteTableId = Lens.lens (\TransitGatewayRouteTable' {transitGatewayRouteTableId} -> transitGatewayRouteTableId) (\s@TransitGatewayRouteTable' {} a -> s {transitGatewayRouteTableId = a} :: TransitGatewayRouteTable)

-- | The ID of the transit gateway.
transitGatewayRouteTable_transitGatewayId :: Lens.Lens' TransitGatewayRouteTable (Prelude.Maybe Prelude.Text)
transitGatewayRouteTable_transitGatewayId = Lens.lens (\TransitGatewayRouteTable' {transitGatewayId} -> transitGatewayId) (\s@TransitGatewayRouteTable' {} a -> s {transitGatewayId = a} :: TransitGatewayRouteTable)

instance Prelude.FromXML TransitGatewayRouteTable where
  parseXML x =
    TransitGatewayRouteTable'
      Prelude.<$> (x Prelude..@? "creationTime")
      Prelude.<*> (x Prelude..@? "defaultAssociationRouteTable")
      Prelude.<*> (x Prelude..@? "defaultPropagationRouteTable")
      Prelude.<*> (x Prelude..@? "state")
      Prelude.<*> ( x Prelude..@? "tagSet" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "transitGatewayRouteTableId")
      Prelude.<*> (x Prelude..@? "transitGatewayId")

instance Prelude.Hashable TransitGatewayRouteTable

instance Prelude.NFData TransitGatewayRouteTable
