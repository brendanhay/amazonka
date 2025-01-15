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
-- Module      : Amazonka.LexV2Models.Types.BotLocaleFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.BotLocaleFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.BotLocaleFilterName
import Amazonka.LexV2Models.Types.BotLocaleFilterOperator
import qualified Amazonka.Prelude as Prelude

-- | Filters responses returned by the @ListBotLocales@ operation.
--
-- /See:/ 'newBotLocaleFilter' smart constructor.
data BotLocaleFilter = BotLocaleFilter'
  { -- | The name of the field to filter the list of bots.
    name :: BotLocaleFilterName,
    -- | The value to use for filtering the list of bots.
    values :: Prelude.NonEmpty Prelude.Text,
    -- | The operator to use for the filter. Specify @EQ@ when the
    -- @ListBotLocales@ operation should return only aliases that equal the
    -- specified value. Specify @CO@ when the @ListBotLocales@ operation should
    -- return aliases that contain the specified value.
    operator :: BotLocaleFilterOperator
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BotLocaleFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'botLocaleFilter_name' - The name of the field to filter the list of bots.
--
-- 'values', 'botLocaleFilter_values' - The value to use for filtering the list of bots.
--
-- 'operator', 'botLocaleFilter_operator' - The operator to use for the filter. Specify @EQ@ when the
-- @ListBotLocales@ operation should return only aliases that equal the
-- specified value. Specify @CO@ when the @ListBotLocales@ operation should
-- return aliases that contain the specified value.
newBotLocaleFilter ::
  -- | 'name'
  BotLocaleFilterName ->
  -- | 'values'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'operator'
  BotLocaleFilterOperator ->
  BotLocaleFilter
newBotLocaleFilter pName_ pValues_ pOperator_ =
  BotLocaleFilter'
    { name = pName_,
      values = Lens.coerced Lens.# pValues_,
      operator = pOperator_
    }

-- | The name of the field to filter the list of bots.
botLocaleFilter_name :: Lens.Lens' BotLocaleFilter BotLocaleFilterName
botLocaleFilter_name = Lens.lens (\BotLocaleFilter' {name} -> name) (\s@BotLocaleFilter' {} a -> s {name = a} :: BotLocaleFilter)

-- | The value to use for filtering the list of bots.
botLocaleFilter_values :: Lens.Lens' BotLocaleFilter (Prelude.NonEmpty Prelude.Text)
botLocaleFilter_values = Lens.lens (\BotLocaleFilter' {values} -> values) (\s@BotLocaleFilter' {} a -> s {values = a} :: BotLocaleFilter) Prelude.. Lens.coerced

-- | The operator to use for the filter. Specify @EQ@ when the
-- @ListBotLocales@ operation should return only aliases that equal the
-- specified value. Specify @CO@ when the @ListBotLocales@ operation should
-- return aliases that contain the specified value.
botLocaleFilter_operator :: Lens.Lens' BotLocaleFilter BotLocaleFilterOperator
botLocaleFilter_operator = Lens.lens (\BotLocaleFilter' {operator} -> operator) (\s@BotLocaleFilter' {} a -> s {operator = a} :: BotLocaleFilter)

instance Prelude.Hashable BotLocaleFilter where
  hashWithSalt _salt BotLocaleFilter' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` values
      `Prelude.hashWithSalt` operator

instance Prelude.NFData BotLocaleFilter where
  rnf BotLocaleFilter' {..} =
    Prelude.rnf name `Prelude.seq`
      Prelude.rnf values `Prelude.seq`
        Prelude.rnf operator

instance Data.ToJSON BotLocaleFilter where
  toJSON BotLocaleFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Data..= name),
            Prelude.Just ("values" Data..= values),
            Prelude.Just ("operator" Data..= operator)
          ]
      )
