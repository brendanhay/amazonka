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
-- Module      : Amazonka.Inspector2.Types.AccountAggregation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.AccountAggregation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Inspector2.Types.AccountSortBy
import Amazonka.Inspector2.Types.AggregationFindingType
import Amazonka.Inspector2.Types.AggregationResourceType
import Amazonka.Inspector2.Types.SortOrder
import qualified Amazonka.Prelude as Prelude

-- | An object that contains details about an aggregation response based on
-- Amazon Web Services accounts.
--
-- /See:/ 'newAccountAggregation' smart constructor.
data AccountAggregation = AccountAggregation'
  { -- | The type of resource.
    resourceType :: Prelude.Maybe AggregationResourceType,
    -- | The sort order (ascending or descending).
    sortOrder :: Prelude.Maybe SortOrder,
    -- | The value to sort by.
    sortBy :: Prelude.Maybe AccountSortBy,
    -- | The type of finding.
    findingType :: Prelude.Maybe AggregationFindingType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccountAggregation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'accountAggregation_resourceType' - The type of resource.
--
-- 'sortOrder', 'accountAggregation_sortOrder' - The sort order (ascending or descending).
--
-- 'sortBy', 'accountAggregation_sortBy' - The value to sort by.
--
-- 'findingType', 'accountAggregation_findingType' - The type of finding.
newAccountAggregation ::
  AccountAggregation
newAccountAggregation =
  AccountAggregation'
    { resourceType = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      findingType = Prelude.Nothing
    }

-- | The type of resource.
accountAggregation_resourceType :: Lens.Lens' AccountAggregation (Prelude.Maybe AggregationResourceType)
accountAggregation_resourceType = Lens.lens (\AccountAggregation' {resourceType} -> resourceType) (\s@AccountAggregation' {} a -> s {resourceType = a} :: AccountAggregation)

-- | The sort order (ascending or descending).
accountAggregation_sortOrder :: Lens.Lens' AccountAggregation (Prelude.Maybe SortOrder)
accountAggregation_sortOrder = Lens.lens (\AccountAggregation' {sortOrder} -> sortOrder) (\s@AccountAggregation' {} a -> s {sortOrder = a} :: AccountAggregation)

-- | The value to sort by.
accountAggregation_sortBy :: Lens.Lens' AccountAggregation (Prelude.Maybe AccountSortBy)
accountAggregation_sortBy = Lens.lens (\AccountAggregation' {sortBy} -> sortBy) (\s@AccountAggregation' {} a -> s {sortBy = a} :: AccountAggregation)

-- | The type of finding.
accountAggregation_findingType :: Lens.Lens' AccountAggregation (Prelude.Maybe AggregationFindingType)
accountAggregation_findingType = Lens.lens (\AccountAggregation' {findingType} -> findingType) (\s@AccountAggregation' {} a -> s {findingType = a} :: AccountAggregation)

instance Prelude.Hashable AccountAggregation where
  hashWithSalt _salt AccountAggregation' {..} =
    _salt `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` findingType

instance Prelude.NFData AccountAggregation where
  rnf AccountAggregation' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf findingType

instance Core.ToJSON AccountAggregation where
  toJSON AccountAggregation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("resourceType" Core..=) Prelude.<$> resourceType,
            ("sortOrder" Core..=) Prelude.<$> sortOrder,
            ("sortBy" Core..=) Prelude.<$> sortBy,
            ("findingType" Core..=) Prelude.<$> findingType
          ]
      )
