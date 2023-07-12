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
-- Module      : Amazonka.Braket.Types.SearchJobsFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Braket.Types.SearchJobsFilter where

import Amazonka.Braket.Types.SearchJobsFilterOperator
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A filter used to search for Amazon Braket jobs.
--
-- /See:/ 'newSearchJobsFilter' smart constructor.
data SearchJobsFilter = SearchJobsFilter'
  { -- | The name to use for the jobs filter.
    name :: Prelude.Text,
    -- | An operator to use for the jobs filter.
    operator :: SearchJobsFilterOperator,
    -- | The values to use for the jobs filter.
    values :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchJobsFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'searchJobsFilter_name' - The name to use for the jobs filter.
--
-- 'operator', 'searchJobsFilter_operator' - An operator to use for the jobs filter.
--
-- 'values', 'searchJobsFilter_values' - The values to use for the jobs filter.
newSearchJobsFilter ::
  -- | 'name'
  Prelude.Text ->
  -- | 'operator'
  SearchJobsFilterOperator ->
  -- | 'values'
  Prelude.NonEmpty Prelude.Text ->
  SearchJobsFilter
newSearchJobsFilter pName_ pOperator_ pValues_ =
  SearchJobsFilter'
    { name = pName_,
      operator = pOperator_,
      values = Lens.coerced Lens.# pValues_
    }

-- | The name to use for the jobs filter.
searchJobsFilter_name :: Lens.Lens' SearchJobsFilter Prelude.Text
searchJobsFilter_name = Lens.lens (\SearchJobsFilter' {name} -> name) (\s@SearchJobsFilter' {} a -> s {name = a} :: SearchJobsFilter)

-- | An operator to use for the jobs filter.
searchJobsFilter_operator :: Lens.Lens' SearchJobsFilter SearchJobsFilterOperator
searchJobsFilter_operator = Lens.lens (\SearchJobsFilter' {operator} -> operator) (\s@SearchJobsFilter' {} a -> s {operator = a} :: SearchJobsFilter)

-- | The values to use for the jobs filter.
searchJobsFilter_values :: Lens.Lens' SearchJobsFilter (Prelude.NonEmpty Prelude.Text)
searchJobsFilter_values = Lens.lens (\SearchJobsFilter' {values} -> values) (\s@SearchJobsFilter' {} a -> s {values = a} :: SearchJobsFilter) Prelude.. Lens.coerced

instance Prelude.Hashable SearchJobsFilter where
  hashWithSalt _salt SearchJobsFilter' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` operator
      `Prelude.hashWithSalt` values

instance Prelude.NFData SearchJobsFilter where
  rnf SearchJobsFilter' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf operator
      `Prelude.seq` Prelude.rnf values

instance Data.ToJSON SearchJobsFilter where
  toJSON SearchJobsFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Data..= name),
            Prelude.Just ("operator" Data..= operator),
            Prelude.Just ("values" Data..= values)
          ]
      )
