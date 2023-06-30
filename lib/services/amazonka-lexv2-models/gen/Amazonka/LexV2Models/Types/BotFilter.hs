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
-- Module      : Amazonka.LexV2Models.Types.BotFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.BotFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.BotFilterName
import Amazonka.LexV2Models.Types.BotFilterOperator
import qualified Amazonka.Prelude as Prelude

-- | Filters the responses returned by the @ListBots@ operation.
--
-- /See:/ 'newBotFilter' smart constructor.
data BotFilter = BotFilter'
  { -- | The name of the field to filter the list of bots.
    name :: BotFilterName,
    -- | The value to use for filtering the list of bots.
    values :: Prelude.NonEmpty Prelude.Text,
    -- | The operator to use for the filter. Specify @EQ@ when the @ListBots@
    -- operation should return only aliases that equal the specified value.
    -- Specify @CO@ when the @ListBots@ operation should return aliases that
    -- contain the specified value.
    operator :: BotFilterOperator
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BotFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'botFilter_name' - The name of the field to filter the list of bots.
--
-- 'values', 'botFilter_values' - The value to use for filtering the list of bots.
--
-- 'operator', 'botFilter_operator' - The operator to use for the filter. Specify @EQ@ when the @ListBots@
-- operation should return only aliases that equal the specified value.
-- Specify @CO@ when the @ListBots@ operation should return aliases that
-- contain the specified value.
newBotFilter ::
  -- | 'name'
  BotFilterName ->
  -- | 'values'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'operator'
  BotFilterOperator ->
  BotFilter
newBotFilter pName_ pValues_ pOperator_ =
  BotFilter'
    { name = pName_,
      values = Lens.coerced Lens.# pValues_,
      operator = pOperator_
    }

-- | The name of the field to filter the list of bots.
botFilter_name :: Lens.Lens' BotFilter BotFilterName
botFilter_name = Lens.lens (\BotFilter' {name} -> name) (\s@BotFilter' {} a -> s {name = a} :: BotFilter)

-- | The value to use for filtering the list of bots.
botFilter_values :: Lens.Lens' BotFilter (Prelude.NonEmpty Prelude.Text)
botFilter_values = Lens.lens (\BotFilter' {values} -> values) (\s@BotFilter' {} a -> s {values = a} :: BotFilter) Prelude.. Lens.coerced

-- | The operator to use for the filter. Specify @EQ@ when the @ListBots@
-- operation should return only aliases that equal the specified value.
-- Specify @CO@ when the @ListBots@ operation should return aliases that
-- contain the specified value.
botFilter_operator :: Lens.Lens' BotFilter BotFilterOperator
botFilter_operator = Lens.lens (\BotFilter' {operator} -> operator) (\s@BotFilter' {} a -> s {operator = a} :: BotFilter)

instance Prelude.Hashable BotFilter where
  hashWithSalt _salt BotFilter' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` values
      `Prelude.hashWithSalt` operator

instance Prelude.NFData BotFilter where
  rnf BotFilter' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf values
      `Prelude.seq` Prelude.rnf operator

instance Data.ToJSON BotFilter where
  toJSON BotFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Data..= name),
            Prelude.Just ("values" Data..= values),
            Prelude.Just ("operator" Data..= operator)
          ]
      )
