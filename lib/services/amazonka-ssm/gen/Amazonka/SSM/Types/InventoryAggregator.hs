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
-- Module      : Amazonka.SSM.Types.InventoryAggregator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.InventoryAggregator where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.InventoryGroup

-- | Specifies the inventory type and attribute for the aggregation
-- execution.
--
-- /See:/ 'newInventoryAggregator' smart constructor.
data InventoryAggregator = InventoryAggregator'
  { -- | A user-defined set of one or more filters on which to aggregate
    -- inventory data. Groups return a count of resources that match and don\'t
    -- match the specified criteria.
    groups :: Prelude.Maybe (Prelude.NonEmpty InventoryGroup),
    -- | Nested aggregators to further refine aggregation for an inventory type.
    aggregators :: Prelude.Maybe (Prelude.NonEmpty InventoryAggregator),
    -- | The inventory type and attribute name for aggregation.
    expression :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { groups = Prelude.Nothing,
      aggregators = Prelude.Nothing,
      expression = Prelude.Nothing
    }

-- | A user-defined set of one or more filters on which to aggregate
-- inventory data. Groups return a count of resources that match and don\'t
-- match the specified criteria.
inventoryAggregator_groups :: Lens.Lens' InventoryAggregator (Prelude.Maybe (Prelude.NonEmpty InventoryGroup))
inventoryAggregator_groups = Lens.lens (\InventoryAggregator' {groups} -> groups) (\s@InventoryAggregator' {} a -> s {groups = a} :: InventoryAggregator) Prelude.. Lens.mapping Lens.coerced

-- | Nested aggregators to further refine aggregation for an inventory type.
inventoryAggregator_aggregators :: Lens.Lens' InventoryAggregator (Prelude.Maybe (Prelude.NonEmpty InventoryAggregator))
inventoryAggregator_aggregators = Lens.lens (\InventoryAggregator' {aggregators} -> aggregators) (\s@InventoryAggregator' {} a -> s {aggregators = a} :: InventoryAggregator) Prelude.. Lens.mapping Lens.coerced

-- | The inventory type and attribute name for aggregation.
inventoryAggregator_expression :: Lens.Lens' InventoryAggregator (Prelude.Maybe Prelude.Text)
inventoryAggregator_expression = Lens.lens (\InventoryAggregator' {expression} -> expression) (\s@InventoryAggregator' {} a -> s {expression = a} :: InventoryAggregator)

instance Prelude.Hashable InventoryAggregator where
  hashWithSalt _salt InventoryAggregator' {..} =
    _salt `Prelude.hashWithSalt` groups
      `Prelude.hashWithSalt` aggregators
      `Prelude.hashWithSalt` expression

instance Prelude.NFData InventoryAggregator where
  rnf InventoryAggregator' {..} =
    Prelude.rnf groups
      `Prelude.seq` Prelude.rnf aggregators
      `Prelude.seq` Prelude.rnf expression

instance Core.ToJSON InventoryAggregator where
  toJSON InventoryAggregator' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Groups" Core..=) Prelude.<$> groups,
            ("Aggregators" Core..=) Prelude.<$> aggregators,
            ("Expression" Core..=) Prelude.<$> expression
          ]
      )
