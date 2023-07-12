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
-- Module      : Amazonka.LicenseManagerLinuxSubscriptions.Types.Filter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManagerLinuxSubscriptions.Types.Filter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManagerLinuxSubscriptions.Types.Operator
import qualified Amazonka.Prelude as Prelude

-- | A filter object that is used to return more specific results from a
-- describe operation. Filters can be used to match a set of resources by
-- specific criteria.
--
-- /See:/ 'newFilter' smart constructor.
data Filter = Filter'
  { -- | The type of name to filter by.
    name :: Prelude.Maybe Prelude.Text,
    -- | An operator for filtering results.
    operator :: Prelude.Maybe Operator,
    -- | One or more values for the name to filter by.
    values :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
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
-- 'name', 'filter_name' - The type of name to filter by.
--
-- 'operator', 'filter_operator' - An operator for filtering results.
--
-- 'values', 'filter_values' - One or more values for the name to filter by.
newFilter ::
  Filter
newFilter =
  Filter'
    { name = Prelude.Nothing,
      operator = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The type of name to filter by.
filter_name :: Lens.Lens' Filter (Prelude.Maybe Prelude.Text)
filter_name = Lens.lens (\Filter' {name} -> name) (\s@Filter' {} a -> s {name = a} :: Filter)

-- | An operator for filtering results.
filter_operator :: Lens.Lens' Filter (Prelude.Maybe Operator)
filter_operator = Lens.lens (\Filter' {operator} -> operator) (\s@Filter' {} a -> s {operator = a} :: Filter)

-- | One or more values for the name to filter by.
filter_values :: Lens.Lens' Filter (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
filter_values = Lens.lens (\Filter' {values} -> values) (\s@Filter' {} a -> s {values = a} :: Filter) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable Filter where
  hashWithSalt _salt Filter' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` operator
      `Prelude.hashWithSalt` values

instance Prelude.NFData Filter where
  rnf Filter' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf operator
      `Prelude.seq` Prelude.rnf values

instance Data.ToJSON Filter where
  toJSON Filter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("Operator" Data..=) Prelude.<$> operator,
            ("Values" Data..=) Prelude.<$> values
          ]
      )
