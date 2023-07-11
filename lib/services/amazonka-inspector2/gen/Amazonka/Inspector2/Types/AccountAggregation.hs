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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.AccountAggregation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
  { -- | The type of finding.
    findingType :: Prelude.Maybe AggregationFindingType,
    -- | The type of resource.
    resourceType :: Prelude.Maybe AggregationResourceType,
    -- | The value to sort by.
    sortBy :: Prelude.Maybe AccountSortBy,
    -- | The sort order (ascending or descending).
    sortOrder :: Prelude.Maybe SortOrder
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
-- 'findingType', 'accountAggregation_findingType' - The type of finding.
--
-- 'resourceType', 'accountAggregation_resourceType' - The type of resource.
--
-- 'sortBy', 'accountAggregation_sortBy' - The value to sort by.
--
-- 'sortOrder', 'accountAggregation_sortOrder' - The sort order (ascending or descending).
newAccountAggregation ::
  AccountAggregation
newAccountAggregation =
  AccountAggregation'
    { findingType = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing
    }

-- | The type of finding.
accountAggregation_findingType :: Lens.Lens' AccountAggregation (Prelude.Maybe AggregationFindingType)
accountAggregation_findingType = Lens.lens (\AccountAggregation' {findingType} -> findingType) (\s@AccountAggregation' {} a -> s {findingType = a} :: AccountAggregation)

-- | The type of resource.
accountAggregation_resourceType :: Lens.Lens' AccountAggregation (Prelude.Maybe AggregationResourceType)
accountAggregation_resourceType = Lens.lens (\AccountAggregation' {resourceType} -> resourceType) (\s@AccountAggregation' {} a -> s {resourceType = a} :: AccountAggregation)

-- | The value to sort by.
accountAggregation_sortBy :: Lens.Lens' AccountAggregation (Prelude.Maybe AccountSortBy)
accountAggregation_sortBy = Lens.lens (\AccountAggregation' {sortBy} -> sortBy) (\s@AccountAggregation' {} a -> s {sortBy = a} :: AccountAggregation)

-- | The sort order (ascending or descending).
accountAggregation_sortOrder :: Lens.Lens' AccountAggregation (Prelude.Maybe SortOrder)
accountAggregation_sortOrder = Lens.lens (\AccountAggregation' {sortOrder} -> sortOrder) (\s@AccountAggregation' {} a -> s {sortOrder = a} :: AccountAggregation)

instance Prelude.Hashable AccountAggregation where
  hashWithSalt _salt AccountAggregation' {..} =
    _salt
      `Prelude.hashWithSalt` findingType
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` sortOrder

instance Prelude.NFData AccountAggregation where
  rnf AccountAggregation' {..} =
    Prelude.rnf findingType
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf sortOrder

instance Data.ToJSON AccountAggregation where
  toJSON AccountAggregation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("findingType" Data..=) Prelude.<$> findingType,
            ("resourceType" Data..=) Prelude.<$> resourceType,
            ("sortBy" Data..=) Prelude.<$> sortBy,
            ("sortOrder" Data..=) Prelude.<$> sortOrder
          ]
      )
