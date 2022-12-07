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
-- Module      : Amazonka.LexV2Models.Types.IntentFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.IntentFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.IntentFilterName
import Amazonka.LexV2Models.Types.IntentFilterOperator
import qualified Amazonka.Prelude as Prelude

-- | Filters the response from the @ListIntents@ operation.
--
-- /See:/ 'newIntentFilter' smart constructor.
data IntentFilter = IntentFilter'
  { -- | The name of the field to use for the filter.
    name :: IntentFilterName,
    -- | The value to use for the filter.
    values :: Prelude.NonEmpty Prelude.Text,
    -- | The operator to use for the filter. Specify @EQ@ when the @ListIntents@
    -- operation should return only aliases that equal the specified value.
    -- Specify @CO@ when the @ListIntents@ operation should return aliases that
    -- contain the specified value.
    operator :: IntentFilterOperator
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IntentFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'intentFilter_name' - The name of the field to use for the filter.
--
-- 'values', 'intentFilter_values' - The value to use for the filter.
--
-- 'operator', 'intentFilter_operator' - The operator to use for the filter. Specify @EQ@ when the @ListIntents@
-- operation should return only aliases that equal the specified value.
-- Specify @CO@ when the @ListIntents@ operation should return aliases that
-- contain the specified value.
newIntentFilter ::
  -- | 'name'
  IntentFilterName ->
  -- | 'values'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'operator'
  IntentFilterOperator ->
  IntentFilter
newIntentFilter pName_ pValues_ pOperator_ =
  IntentFilter'
    { name = pName_,
      values = Lens.coerced Lens.# pValues_,
      operator = pOperator_
    }

-- | The name of the field to use for the filter.
intentFilter_name :: Lens.Lens' IntentFilter IntentFilterName
intentFilter_name = Lens.lens (\IntentFilter' {name} -> name) (\s@IntentFilter' {} a -> s {name = a} :: IntentFilter)

-- | The value to use for the filter.
intentFilter_values :: Lens.Lens' IntentFilter (Prelude.NonEmpty Prelude.Text)
intentFilter_values = Lens.lens (\IntentFilter' {values} -> values) (\s@IntentFilter' {} a -> s {values = a} :: IntentFilter) Prelude.. Lens.coerced

-- | The operator to use for the filter. Specify @EQ@ when the @ListIntents@
-- operation should return only aliases that equal the specified value.
-- Specify @CO@ when the @ListIntents@ operation should return aliases that
-- contain the specified value.
intentFilter_operator :: Lens.Lens' IntentFilter IntentFilterOperator
intentFilter_operator = Lens.lens (\IntentFilter' {operator} -> operator) (\s@IntentFilter' {} a -> s {operator = a} :: IntentFilter)

instance Prelude.Hashable IntentFilter where
  hashWithSalt _salt IntentFilter' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` values
      `Prelude.hashWithSalt` operator

instance Prelude.NFData IntentFilter where
  rnf IntentFilter' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf values
      `Prelude.seq` Prelude.rnf operator

instance Data.ToJSON IntentFilter where
  toJSON IntentFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Data..= name),
            Prelude.Just ("values" Data..= values),
            Prelude.Just ("operator" Data..= operator)
          ]
      )
