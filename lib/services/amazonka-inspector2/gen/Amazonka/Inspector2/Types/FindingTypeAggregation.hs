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
-- Module      : Amazonka.Inspector2.Types.FindingTypeAggregation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.FindingTypeAggregation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Inspector2.Types.AggregationFindingType
import Amazonka.Inspector2.Types.AggregationResourceType
import Amazonka.Inspector2.Types.FindingTypeSortBy
import Amazonka.Inspector2.Types.SortOrder
import qualified Amazonka.Prelude as Prelude

-- | The details that define an aggregation based on finding type.
--
-- /See:/ 'newFindingTypeAggregation' smart constructor.
data FindingTypeAggregation = FindingTypeAggregation'
  { -- | The resource type to aggregate.
    resourceType :: Prelude.Maybe AggregationResourceType,
    -- | The order to sort results by.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | The value to sort results by.
    sortBy :: Prelude.Maybe FindingTypeSortBy,
    -- | The finding type to aggregate.
    findingType :: Prelude.Maybe AggregationFindingType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FindingTypeAggregation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'findingTypeAggregation_resourceType' - The resource type to aggregate.
--
-- 'sortOrder', 'findingTypeAggregation_sortOrder' - The order to sort results by.
--
-- 'sortBy', 'findingTypeAggregation_sortBy' - The value to sort results by.
--
-- 'findingType', 'findingTypeAggregation_findingType' - The finding type to aggregate.
newFindingTypeAggregation ::
  FindingTypeAggregation
newFindingTypeAggregation =
  FindingTypeAggregation'
    { resourceType =
        Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      findingType = Prelude.Nothing
    }

-- | The resource type to aggregate.
findingTypeAggregation_resourceType :: Lens.Lens' FindingTypeAggregation (Prelude.Maybe AggregationResourceType)
findingTypeAggregation_resourceType = Lens.lens (\FindingTypeAggregation' {resourceType} -> resourceType) (\s@FindingTypeAggregation' {} a -> s {resourceType = a} :: FindingTypeAggregation)

-- | The order to sort results by.
findingTypeAggregation_sortOrder :: Lens.Lens' FindingTypeAggregation (Prelude.Maybe SortOrder)
findingTypeAggregation_sortOrder = Lens.lens (\FindingTypeAggregation' {sortOrder} -> sortOrder) (\s@FindingTypeAggregation' {} a -> s {sortOrder = a} :: FindingTypeAggregation)

-- | The value to sort results by.
findingTypeAggregation_sortBy :: Lens.Lens' FindingTypeAggregation (Prelude.Maybe FindingTypeSortBy)
findingTypeAggregation_sortBy = Lens.lens (\FindingTypeAggregation' {sortBy} -> sortBy) (\s@FindingTypeAggregation' {} a -> s {sortBy = a} :: FindingTypeAggregation)

-- | The finding type to aggregate.
findingTypeAggregation_findingType :: Lens.Lens' FindingTypeAggregation (Prelude.Maybe AggregationFindingType)
findingTypeAggregation_findingType = Lens.lens (\FindingTypeAggregation' {findingType} -> findingType) (\s@FindingTypeAggregation' {} a -> s {findingType = a} :: FindingTypeAggregation)

instance Prelude.Hashable FindingTypeAggregation where
  hashWithSalt _salt FindingTypeAggregation' {..} =
    _salt `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` findingType

instance Prelude.NFData FindingTypeAggregation where
  rnf FindingTypeAggregation' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf findingType

instance Core.ToJSON FindingTypeAggregation where
  toJSON FindingTypeAggregation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("resourceType" Core..=) Prelude.<$> resourceType,
            ("sortOrder" Core..=) Prelude.<$> sortOrder,
            ("sortBy" Core..=) Prelude.<$> sortBy,
            ("findingType" Core..=) Prelude.<$> findingType
          ]
      )
