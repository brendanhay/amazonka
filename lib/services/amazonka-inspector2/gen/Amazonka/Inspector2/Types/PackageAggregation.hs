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
-- Module      : Amazonka.Inspector2.Types.PackageAggregation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.PackageAggregation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Inspector2.Types.PackageSortBy
import Amazonka.Inspector2.Types.SortOrder
import Amazonka.Inspector2.Types.StringFilter
import qualified Amazonka.Prelude as Prelude

-- | The details that define an aggregation based on operating system package
-- type.
--
-- /See:/ 'newPackageAggregation' smart constructor.
data PackageAggregation = PackageAggregation'
  { -- | The order to sort results by.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | The value to sort results by.
    sortBy :: Prelude.Maybe PackageSortBy,
    -- | The names of packages to aggregate findings on.
    packageNames :: Prelude.Maybe (Prelude.NonEmpty StringFilter)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PackageAggregation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'packageAggregation_sortOrder' - The order to sort results by.
--
-- 'sortBy', 'packageAggregation_sortBy' - The value to sort results by.
--
-- 'packageNames', 'packageAggregation_packageNames' - The names of packages to aggregate findings on.
newPackageAggregation ::
  PackageAggregation
newPackageAggregation =
  PackageAggregation'
    { sortOrder = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      packageNames = Prelude.Nothing
    }

-- | The order to sort results by.
packageAggregation_sortOrder :: Lens.Lens' PackageAggregation (Prelude.Maybe SortOrder)
packageAggregation_sortOrder = Lens.lens (\PackageAggregation' {sortOrder} -> sortOrder) (\s@PackageAggregation' {} a -> s {sortOrder = a} :: PackageAggregation)

-- | The value to sort results by.
packageAggregation_sortBy :: Lens.Lens' PackageAggregation (Prelude.Maybe PackageSortBy)
packageAggregation_sortBy = Lens.lens (\PackageAggregation' {sortBy} -> sortBy) (\s@PackageAggregation' {} a -> s {sortBy = a} :: PackageAggregation)

-- | The names of packages to aggregate findings on.
packageAggregation_packageNames :: Lens.Lens' PackageAggregation (Prelude.Maybe (Prelude.NonEmpty StringFilter))
packageAggregation_packageNames = Lens.lens (\PackageAggregation' {packageNames} -> packageNames) (\s@PackageAggregation' {} a -> s {packageNames = a} :: PackageAggregation) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable PackageAggregation where
  hashWithSalt _salt PackageAggregation' {..} =
    _salt `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` packageNames

instance Prelude.NFData PackageAggregation where
  rnf PackageAggregation' {..} =
    Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf packageNames

instance Core.ToJSON PackageAggregation where
  toJSON PackageAggregation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("sortOrder" Core..=) Prelude.<$> sortOrder,
            ("sortBy" Core..=) Prelude.<$> sortBy,
            ("packageNames" Core..=) Prelude.<$> packageNames
          ]
      )
