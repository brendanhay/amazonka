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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.FindingTypeAggregation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.AggregationFindingType
import Amazonka.Inspector2.Types.AggregationResourceType
import Amazonka.Inspector2.Types.FindingTypeSortBy
import Amazonka.Inspector2.Types.SortOrder
import qualified Amazonka.Prelude as Prelude

-- | The details that define an aggregation based on finding type.
--
-- /See:/ 'newFindingTypeAggregation' smart constructor.
data FindingTypeAggregation = FindingTypeAggregation'
  { -- | The finding type to aggregate.
    findingType :: Prelude.Maybe AggregationFindingType,
    -- | The resource type to aggregate.
    resourceType :: Prelude.Maybe AggregationResourceType,
    -- | The value to sort results by.
    sortBy :: Prelude.Maybe FindingTypeSortBy,
    -- | The order to sort results by.
    sortOrder :: Prelude.Maybe SortOrder
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
-- 'findingType', 'findingTypeAggregation_findingType' - The finding type to aggregate.
--
-- 'resourceType', 'findingTypeAggregation_resourceType' - The resource type to aggregate.
--
-- 'sortBy', 'findingTypeAggregation_sortBy' - The value to sort results by.
--
-- 'sortOrder', 'findingTypeAggregation_sortOrder' - The order to sort results by.
newFindingTypeAggregation ::
  FindingTypeAggregation
newFindingTypeAggregation =
  FindingTypeAggregation'
    { findingType =
        Prelude.Nothing,
      resourceType = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing
    }

-- | The finding type to aggregate.
findingTypeAggregation_findingType :: Lens.Lens' FindingTypeAggregation (Prelude.Maybe AggregationFindingType)
findingTypeAggregation_findingType = Lens.lens (\FindingTypeAggregation' {findingType} -> findingType) (\s@FindingTypeAggregation' {} a -> s {findingType = a} :: FindingTypeAggregation)

-- | The resource type to aggregate.
findingTypeAggregation_resourceType :: Lens.Lens' FindingTypeAggregation (Prelude.Maybe AggregationResourceType)
findingTypeAggregation_resourceType = Lens.lens (\FindingTypeAggregation' {resourceType} -> resourceType) (\s@FindingTypeAggregation' {} a -> s {resourceType = a} :: FindingTypeAggregation)

-- | The value to sort results by.
findingTypeAggregation_sortBy :: Lens.Lens' FindingTypeAggregation (Prelude.Maybe FindingTypeSortBy)
findingTypeAggregation_sortBy = Lens.lens (\FindingTypeAggregation' {sortBy} -> sortBy) (\s@FindingTypeAggregation' {} a -> s {sortBy = a} :: FindingTypeAggregation)

-- | The order to sort results by.
findingTypeAggregation_sortOrder :: Lens.Lens' FindingTypeAggregation (Prelude.Maybe SortOrder)
findingTypeAggregation_sortOrder = Lens.lens (\FindingTypeAggregation' {sortOrder} -> sortOrder) (\s@FindingTypeAggregation' {} a -> s {sortOrder = a} :: FindingTypeAggregation)

instance Prelude.Hashable FindingTypeAggregation where
  hashWithSalt _salt FindingTypeAggregation' {..} =
    _salt
      `Prelude.hashWithSalt` findingType
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` sortOrder

instance Prelude.NFData FindingTypeAggregation where
  rnf FindingTypeAggregation' {..} =
    Prelude.rnf findingType
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf sortOrder

instance Data.ToJSON FindingTypeAggregation where
  toJSON FindingTypeAggregation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("findingType" Data..=) Prelude.<$> findingType,
            ("resourceType" Data..=) Prelude.<$> resourceType,
            ("sortBy" Data..=) Prelude.<$> sortBy,
            ("sortOrder" Data..=) Prelude.<$> sortOrder
          ]
      )
