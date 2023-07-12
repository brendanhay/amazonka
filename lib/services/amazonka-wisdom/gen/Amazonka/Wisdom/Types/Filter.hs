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
-- Module      : Amazonka.Wisdom.Types.Filter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Wisdom.Types.Filter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Wisdom.Types.FilterField
import Amazonka.Wisdom.Types.FilterOperator

-- | A search filter.
--
-- /See:/ 'newFilter' smart constructor.
data Filter = Filter'
  { -- | The field on which to filter.
    field :: FilterField,
    -- | The operator to use for comparing the field’s value with the provided
    -- value.
    operator :: FilterOperator,
    -- | The desired field value on which to filter.
    value :: Prelude.Text
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
-- 'field', 'filter_field' - The field on which to filter.
--
-- 'operator', 'filter_operator' - The operator to use for comparing the field’s value with the provided
-- value.
--
-- 'value', 'filter_value' - The desired field value on which to filter.
newFilter ::
  -- | 'field'
  FilterField ->
  -- | 'operator'
  FilterOperator ->
  -- | 'value'
  Prelude.Text ->
  Filter
newFilter pField_ pOperator_ pValue_ =
  Filter'
    { field = pField_,
      operator = pOperator_,
      value = pValue_
    }

-- | The field on which to filter.
filter_field :: Lens.Lens' Filter FilterField
filter_field = Lens.lens (\Filter' {field} -> field) (\s@Filter' {} a -> s {field = a} :: Filter)

-- | The operator to use for comparing the field’s value with the provided
-- value.
filter_operator :: Lens.Lens' Filter FilterOperator
filter_operator = Lens.lens (\Filter' {operator} -> operator) (\s@Filter' {} a -> s {operator = a} :: Filter)

-- | The desired field value on which to filter.
filter_value :: Lens.Lens' Filter Prelude.Text
filter_value = Lens.lens (\Filter' {value} -> value) (\s@Filter' {} a -> s {value = a} :: Filter)

instance Prelude.Hashable Filter where
  hashWithSalt _salt Filter' {..} =
    _salt
      `Prelude.hashWithSalt` field
      `Prelude.hashWithSalt` operator
      `Prelude.hashWithSalt` value

instance Prelude.NFData Filter where
  rnf Filter' {..} =
    Prelude.rnf field
      `Prelude.seq` Prelude.rnf operator
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON Filter where
  toJSON Filter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("field" Data..= field),
            Prelude.Just ("operator" Data..= operator),
            Prelude.Just ("value" Data..= value)
          ]
      )
