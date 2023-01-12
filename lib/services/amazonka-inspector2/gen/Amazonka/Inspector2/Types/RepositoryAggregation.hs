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
-- Module      : Amazonka.Inspector2.Types.RepositoryAggregation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.RepositoryAggregation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.RepositorySortBy
import Amazonka.Inspector2.Types.SortOrder
import Amazonka.Inspector2.Types.StringFilter
import qualified Amazonka.Prelude as Prelude

-- | The details that define an aggregation based on repository.
--
-- /See:/ 'newRepositoryAggregation' smart constructor.
data RepositoryAggregation = RepositoryAggregation'
  { -- | The names of repositories to aggregate findings on.
    repositories :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | The value to sort results by.
    sortBy :: Prelude.Maybe RepositorySortBy,
    -- | The order to sort results by.
    sortOrder :: Prelude.Maybe SortOrder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RepositoryAggregation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repositories', 'repositoryAggregation_repositories' - The names of repositories to aggregate findings on.
--
-- 'sortBy', 'repositoryAggregation_sortBy' - The value to sort results by.
--
-- 'sortOrder', 'repositoryAggregation_sortOrder' - The order to sort results by.
newRepositoryAggregation ::
  RepositoryAggregation
newRepositoryAggregation =
  RepositoryAggregation'
    { repositories =
        Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing
    }

-- | The names of repositories to aggregate findings on.
repositoryAggregation_repositories :: Lens.Lens' RepositoryAggregation (Prelude.Maybe (Prelude.NonEmpty StringFilter))
repositoryAggregation_repositories = Lens.lens (\RepositoryAggregation' {repositories} -> repositories) (\s@RepositoryAggregation' {} a -> s {repositories = a} :: RepositoryAggregation) Prelude.. Lens.mapping Lens.coerced

-- | The value to sort results by.
repositoryAggregation_sortBy :: Lens.Lens' RepositoryAggregation (Prelude.Maybe RepositorySortBy)
repositoryAggregation_sortBy = Lens.lens (\RepositoryAggregation' {sortBy} -> sortBy) (\s@RepositoryAggregation' {} a -> s {sortBy = a} :: RepositoryAggregation)

-- | The order to sort results by.
repositoryAggregation_sortOrder :: Lens.Lens' RepositoryAggregation (Prelude.Maybe SortOrder)
repositoryAggregation_sortOrder = Lens.lens (\RepositoryAggregation' {sortOrder} -> sortOrder) (\s@RepositoryAggregation' {} a -> s {sortOrder = a} :: RepositoryAggregation)

instance Prelude.Hashable RepositoryAggregation where
  hashWithSalt _salt RepositoryAggregation' {..} =
    _salt `Prelude.hashWithSalt` repositories
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` sortOrder

instance Prelude.NFData RepositoryAggregation where
  rnf RepositoryAggregation' {..} =
    Prelude.rnf repositories
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf sortOrder

instance Data.ToJSON RepositoryAggregation where
  toJSON RepositoryAggregation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("repositories" Data..=) Prelude.<$> repositories,
            ("sortBy" Data..=) Prelude.<$> sortBy,
            ("sortOrder" Data..=) Prelude.<$> sortOrder
          ]
      )
