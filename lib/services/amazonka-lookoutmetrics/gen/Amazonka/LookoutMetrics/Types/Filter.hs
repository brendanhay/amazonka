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
-- Module      : Amazonka.LookoutMetrics.Types.Filter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.Filter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutMetrics.Types.FilterOperation
import qualified Amazonka.Prelude as Prelude

-- | Describes a filter for choosing a subset of dimension values. Each
-- filter consists of the dimension that you want to include and the
-- condition statement. The condition statement is specified in the
-- @FilterOperation@ object.
--
-- /See:/ 'newFilter' smart constructor.
data Filter = Filter'
  { -- | The value that you want to include in the filter.
    dimensionValue :: Prelude.Maybe Prelude.Text,
    -- | The condition to apply.
    filterOperation :: Prelude.Maybe FilterOperation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Filter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dimensionValue', 'filter_dimensionValue' - The value that you want to include in the filter.
--
-- 'filterOperation', 'filter_filterOperation' - The condition to apply.
newFilter ::
  Filter
newFilter =
  Filter'
    { dimensionValue = Prelude.Nothing,
      filterOperation = Prelude.Nothing
    }

-- | The value that you want to include in the filter.
filter_dimensionValue :: Lens.Lens' Filter (Prelude.Maybe Prelude.Text)
filter_dimensionValue = Lens.lens (\Filter' {dimensionValue} -> dimensionValue) (\s@Filter' {} a -> s {dimensionValue = a} :: Filter)

-- | The condition to apply.
filter_filterOperation :: Lens.Lens' Filter (Prelude.Maybe FilterOperation)
filter_filterOperation = Lens.lens (\Filter' {filterOperation} -> filterOperation) (\s@Filter' {} a -> s {filterOperation = a} :: Filter)

instance Data.FromJSON Filter where
  parseJSON =
    Data.withObject
      "Filter"
      ( \x ->
          Filter'
            Prelude.<$> (x Data..:? "DimensionValue")
            Prelude.<*> (x Data..:? "FilterOperation")
      )

instance Prelude.Hashable Filter where
  hashWithSalt _salt Filter' {..} =
    _salt
      `Prelude.hashWithSalt` dimensionValue
      `Prelude.hashWithSalt` filterOperation

instance Prelude.NFData Filter where
  rnf Filter' {..} =
    Prelude.rnf dimensionValue
      `Prelude.seq` Prelude.rnf filterOperation

instance Data.ToJSON Filter where
  toJSON Filter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DimensionValue" Data..=)
              Prelude.<$> dimensionValue,
            ("FilterOperation" Data..=)
              Prelude.<$> filterOperation
          ]
      )
