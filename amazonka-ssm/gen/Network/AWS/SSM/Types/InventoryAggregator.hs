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
-- Module      : Network.AWS.SSM.Types.InventoryAggregator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InventoryAggregator where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.InventoryGroup

-- | Specifies the inventory type and attribute for the aggregation
-- execution.
--
-- /See:/ 'newInventoryAggregator' smart constructor.
data InventoryAggregator = InventoryAggregator'
  { -- | A user-defined set of one or more filters on which to aggregate
    -- inventory data. Groups return a count of resources that match and don\'t
    -- match the specified criteria.
    groups :: Core.Maybe (Core.NonEmpty InventoryGroup),
    -- | Nested aggregators to further refine aggregation for an inventory type.
    aggregators :: Core.Maybe (Core.NonEmpty InventoryAggregator),
    -- | The inventory type and attribute name for aggregation.
    expression :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InventoryAggregator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groups', 'inventoryAggregator_groups' - A user-defined set of one or more filters on which to aggregate
-- inventory data. Groups return a count of resources that match and don\'t
-- match the specified criteria.
--
-- 'aggregators', 'inventoryAggregator_aggregators' - Nested aggregators to further refine aggregation for an inventory type.
--
-- 'expression', 'inventoryAggregator_expression' - The inventory type and attribute name for aggregation.
newInventoryAggregator ::
  InventoryAggregator
newInventoryAggregator =
  InventoryAggregator'
    { groups = Core.Nothing,
      aggregators = Core.Nothing,
      expression = Core.Nothing
    }

-- | A user-defined set of one or more filters on which to aggregate
-- inventory data. Groups return a count of resources that match and don\'t
-- match the specified criteria.
inventoryAggregator_groups :: Lens.Lens' InventoryAggregator (Core.Maybe (Core.NonEmpty InventoryGroup))
inventoryAggregator_groups = Lens.lens (\InventoryAggregator' {groups} -> groups) (\s@InventoryAggregator' {} a -> s {groups = a} :: InventoryAggregator) Core.. Lens.mapping Lens._Coerce

-- | Nested aggregators to further refine aggregation for an inventory type.
inventoryAggregator_aggregators :: Lens.Lens' InventoryAggregator (Core.Maybe (Core.NonEmpty InventoryAggregator))
inventoryAggregator_aggregators = Lens.lens (\InventoryAggregator' {aggregators} -> aggregators) (\s@InventoryAggregator' {} a -> s {aggregators = a} :: InventoryAggregator) Core.. Lens.mapping Lens._Coerce

-- | The inventory type and attribute name for aggregation.
inventoryAggregator_expression :: Lens.Lens' InventoryAggregator (Core.Maybe Core.Text)
inventoryAggregator_expression = Lens.lens (\InventoryAggregator' {expression} -> expression) (\s@InventoryAggregator' {} a -> s {expression = a} :: InventoryAggregator)

instance Core.Hashable InventoryAggregator

instance Core.NFData InventoryAggregator

instance Core.ToJSON InventoryAggregator where
  toJSON InventoryAggregator' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Groups" Core..=) Core.<$> groups,
            ("Aggregators" Core..=) Core.<$> aggregators,
            ("Expression" Core..=) Core.<$> expression
          ]
      )
