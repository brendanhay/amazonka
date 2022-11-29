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
-- Module      : Amazonka.Braket.Types.SearchQuantumTasksFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Braket.Types.SearchQuantumTasksFilter where

import Amazonka.Braket.Types.SearchQuantumTasksFilterOperator
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A filter to use to search for tasks.
--
-- /See:/ 'newSearchQuantumTasksFilter' smart constructor.
data SearchQuantumTasksFilter = SearchQuantumTasksFilter'
  { -- | The name of the device used for the task.
    name :: Prelude.Text,
    -- | An operator to use in the filter.
    operator :: SearchQuantumTasksFilterOperator,
    -- | The values to use for the filter.
    values :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchQuantumTasksFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'searchQuantumTasksFilter_name' - The name of the device used for the task.
--
-- 'operator', 'searchQuantumTasksFilter_operator' - An operator to use in the filter.
--
-- 'values', 'searchQuantumTasksFilter_values' - The values to use for the filter.
newSearchQuantumTasksFilter ::
  -- | 'name'
  Prelude.Text ->
  -- | 'operator'
  SearchQuantumTasksFilterOperator ->
  -- | 'values'
  Prelude.NonEmpty Prelude.Text ->
  SearchQuantumTasksFilter
newSearchQuantumTasksFilter
  pName_
  pOperator_
  pValues_ =
    SearchQuantumTasksFilter'
      { name = pName_,
        operator = pOperator_,
        values = Lens.coerced Lens.# pValues_
      }

-- | The name of the device used for the task.
searchQuantumTasksFilter_name :: Lens.Lens' SearchQuantumTasksFilter Prelude.Text
searchQuantumTasksFilter_name = Lens.lens (\SearchQuantumTasksFilter' {name} -> name) (\s@SearchQuantumTasksFilter' {} a -> s {name = a} :: SearchQuantumTasksFilter)

-- | An operator to use in the filter.
searchQuantumTasksFilter_operator :: Lens.Lens' SearchQuantumTasksFilter SearchQuantumTasksFilterOperator
searchQuantumTasksFilter_operator = Lens.lens (\SearchQuantumTasksFilter' {operator} -> operator) (\s@SearchQuantumTasksFilter' {} a -> s {operator = a} :: SearchQuantumTasksFilter)

-- | The values to use for the filter.
searchQuantumTasksFilter_values :: Lens.Lens' SearchQuantumTasksFilter (Prelude.NonEmpty Prelude.Text)
searchQuantumTasksFilter_values = Lens.lens (\SearchQuantumTasksFilter' {values} -> values) (\s@SearchQuantumTasksFilter' {} a -> s {values = a} :: SearchQuantumTasksFilter) Prelude.. Lens.coerced

instance Prelude.Hashable SearchQuantumTasksFilter where
  hashWithSalt _salt SearchQuantumTasksFilter' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` operator
      `Prelude.hashWithSalt` values

instance Prelude.NFData SearchQuantumTasksFilter where
  rnf SearchQuantumTasksFilter' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf operator
      `Prelude.seq` Prelude.rnf values

instance Core.ToJSON SearchQuantumTasksFilter where
  toJSON SearchQuantumTasksFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Core..= name),
            Prelude.Just ("operator" Core..= operator),
            Prelude.Just ("values" Core..= values)
          ]
      )
