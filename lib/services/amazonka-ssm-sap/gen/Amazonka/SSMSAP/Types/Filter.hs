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
-- Module      : Amazonka.SSMSAP.Types.Filter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMSAP.Types.Filter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMSAP.Types.FilterOperator

-- | A specific result obtained by specifying the name, value, and operator.
--
-- /See:/ 'newFilter' smart constructor.
data Filter = Filter'
  { -- | The name of the filter. Filter names are case-sensitive.
    name :: Prelude.Text,
    -- | The filter values. Filter values are case-sensitive. If you specify
    -- multiple values for a filter, the values are joined with an OR, and the
    -- request returns all results that match any of the specified values
    value :: Prelude.Text,
    -- | The operator for the filter.
    operator :: FilterOperator
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
-- 'name', 'filter_name' - The name of the filter. Filter names are case-sensitive.
--
-- 'value', 'filter_value' - The filter values. Filter values are case-sensitive. If you specify
-- multiple values for a filter, the values are joined with an OR, and the
-- request returns all results that match any of the specified values
--
-- 'operator', 'filter_operator' - The operator for the filter.
newFilter ::
  -- | 'name'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  -- | 'operator'
  FilterOperator ->
  Filter
newFilter pName_ pValue_ pOperator_ =
  Filter'
    { name = pName_,
      value = pValue_,
      operator = pOperator_
    }

-- | The name of the filter. Filter names are case-sensitive.
filter_name :: Lens.Lens' Filter Prelude.Text
filter_name = Lens.lens (\Filter' {name} -> name) (\s@Filter' {} a -> s {name = a} :: Filter)

-- | The filter values. Filter values are case-sensitive. If you specify
-- multiple values for a filter, the values are joined with an OR, and the
-- request returns all results that match any of the specified values
filter_value :: Lens.Lens' Filter Prelude.Text
filter_value = Lens.lens (\Filter' {value} -> value) (\s@Filter' {} a -> s {value = a} :: Filter)

-- | The operator for the filter.
filter_operator :: Lens.Lens' Filter FilterOperator
filter_operator = Lens.lens (\Filter' {operator} -> operator) (\s@Filter' {} a -> s {operator = a} :: Filter)

instance Prelude.Hashable Filter where
  hashWithSalt _salt Filter' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` operator

instance Prelude.NFData Filter where
  rnf Filter' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf value
      `Prelude.seq` Prelude.rnf operator

instance Data.ToJSON Filter where
  toJSON Filter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Value" Data..= value),
            Prelude.Just ("Operator" Data..= operator)
          ]
      )
