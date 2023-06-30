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
-- Module      : Amazonka.EC2.Types.TransitGatewayRouteTable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TransitGatewayRouteTable where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.Tag
import Amazonka.EC2.Types.TransitGatewayRouteTableState
import qualified Amazonka.Prelude as Prelude

-- | Describes a transit gateway route table.
--
-- /See:/ 'newTransitGatewayRouteTable' smart constructor.
data TransitGatewayRouteTable = TransitGatewayRouteTable'
  { -- | The creation time.
    creationTime :: Prelude.Maybe Data.ISO8601,
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
    -- | The ID of the transit gateway.
    transitGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the transit gateway route table.
    transitGatewayRouteTableId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'transitGatewayId', 'transitGatewayRouteTable_transitGatewayId' - The ID of the transit gateway.
--
-- 'transitGatewayRouteTableId', 'transitGatewayRouteTable_transitGatewayRouteTableId' - The ID of the transit gateway route table.
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
      transitGatewayId = Prelude.Nothing,
      transitGatewayRouteTableId = Prelude.Nothing
    }

-- | The creation time.
transitGatewayRouteTable_creationTime :: Lens.Lens' TransitGatewayRouteTable (Prelude.Maybe Prelude.UTCTime)
transitGatewayRouteTable_creationTime = Lens.lens (\TransitGatewayRouteTable' {creationTime} -> creationTime) (\s@TransitGatewayRouteTable' {} a -> s {creationTime = a} :: TransitGatewayRouteTable) Prelude.. Lens.mapping Data._Time

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
transitGatewayRouteTable_tags = Lens.lens (\TransitGatewayRouteTable' {tags} -> tags) (\s@TransitGatewayRouteTable' {} a -> s {tags = a} :: TransitGatewayRouteTable) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the transit gateway.
transitGatewayRouteTable_transitGatewayId :: Lens.Lens' TransitGatewayRouteTable (Prelude.Maybe Prelude.Text)
transitGatewayRouteTable_transitGatewayId = Lens.lens (\TransitGatewayRouteTable' {transitGatewayId} -> transitGatewayId) (\s@TransitGatewayRouteTable' {} a -> s {transitGatewayId = a} :: TransitGatewayRouteTable)

-- | The ID of the transit gateway route table.
transitGatewayRouteTable_transitGatewayRouteTableId :: Lens.Lens' TransitGatewayRouteTable (Prelude.Maybe Prelude.Text)
transitGatewayRouteTable_transitGatewayRouteTableId = Lens.lens (\TransitGatewayRouteTable' {transitGatewayRouteTableId} -> transitGatewayRouteTableId) (\s@TransitGatewayRouteTable' {} a -> s {transitGatewayRouteTableId = a} :: TransitGatewayRouteTable)

instance Data.FromXML TransitGatewayRouteTable where
  parseXML x =
    TransitGatewayRouteTable'
      Prelude.<$> (x Data..@? "creationTime")
      Prelude.<*> (x Data..@? "defaultAssociationRouteTable")
      Prelude.<*> (x Data..@? "defaultPropagationRouteTable")
      Prelude.<*> (x Data..@? "state")
      Prelude.<*> ( x
                      Data..@? "tagSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "transitGatewayId")
      Prelude.<*> (x Data..@? "transitGatewayRouteTableId")

instance Prelude.Hashable TransitGatewayRouteTable where
  hashWithSalt _salt TransitGatewayRouteTable' {..} =
    _salt
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` defaultAssociationRouteTable
      `Prelude.hashWithSalt` defaultPropagationRouteTable
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` transitGatewayId
      `Prelude.hashWithSalt` transitGatewayRouteTableId

instance Prelude.NFData TransitGatewayRouteTable where
  rnf TransitGatewayRouteTable' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf defaultAssociationRouteTable
      `Prelude.seq` Prelude.rnf defaultPropagationRouteTable
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf transitGatewayId
      `Prelude.seq` Prelude.rnf transitGatewayRouteTableId
