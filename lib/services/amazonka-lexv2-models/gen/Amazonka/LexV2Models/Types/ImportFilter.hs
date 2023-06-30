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
-- Module      : Amazonka.LexV2Models.Types.ImportFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.ImportFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.ImportFilterName
import Amazonka.LexV2Models.Types.ImportFilterOperator
import qualified Amazonka.Prelude as Prelude

-- | Filters the response from the
-- <https://docs.aws.amazon.com/lexv2/latest/dg/API_ListImports.html ListImports>
-- operation.
--
-- /See:/ 'newImportFilter' smart constructor.
data ImportFilter = ImportFilter'
  { -- | The name of the field to use for filtering.
    name :: ImportFilterName,
    -- | The values to use to filter the response. The values must be @Bot@,
    -- @BotLocale@, or @CustomVocabulary@.
    values :: Prelude.NonEmpty Prelude.Text,
    -- | The operator to use for the filter. Specify EQ when the @ListImports@
    -- operation should return only resource types that equal the specified
    -- value. Specify CO when the @ListImports@ operation should return
    -- resource types that contain the specified value.
    operator :: ImportFilterOperator
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'importFilter_name' - The name of the field to use for filtering.
--
-- 'values', 'importFilter_values' - The values to use to filter the response. The values must be @Bot@,
-- @BotLocale@, or @CustomVocabulary@.
--
-- 'operator', 'importFilter_operator' - The operator to use for the filter. Specify EQ when the @ListImports@
-- operation should return only resource types that equal the specified
-- value. Specify CO when the @ListImports@ operation should return
-- resource types that contain the specified value.
newImportFilter ::
  -- | 'name'
  ImportFilterName ->
  -- | 'values'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'operator'
  ImportFilterOperator ->
  ImportFilter
newImportFilter pName_ pValues_ pOperator_ =
  ImportFilter'
    { name = pName_,
      values = Lens.coerced Lens.# pValues_,
      operator = pOperator_
    }

-- | The name of the field to use for filtering.
importFilter_name :: Lens.Lens' ImportFilter ImportFilterName
importFilter_name = Lens.lens (\ImportFilter' {name} -> name) (\s@ImportFilter' {} a -> s {name = a} :: ImportFilter)

-- | The values to use to filter the response. The values must be @Bot@,
-- @BotLocale@, or @CustomVocabulary@.
importFilter_values :: Lens.Lens' ImportFilter (Prelude.NonEmpty Prelude.Text)
importFilter_values = Lens.lens (\ImportFilter' {values} -> values) (\s@ImportFilter' {} a -> s {values = a} :: ImportFilter) Prelude.. Lens.coerced

-- | The operator to use for the filter. Specify EQ when the @ListImports@
-- operation should return only resource types that equal the specified
-- value. Specify CO when the @ListImports@ operation should return
-- resource types that contain the specified value.
importFilter_operator :: Lens.Lens' ImportFilter ImportFilterOperator
importFilter_operator = Lens.lens (\ImportFilter' {operator} -> operator) (\s@ImportFilter' {} a -> s {operator = a} :: ImportFilter)

instance Prelude.Hashable ImportFilter where
  hashWithSalt _salt ImportFilter' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` values
      `Prelude.hashWithSalt` operator

instance Prelude.NFData ImportFilter where
  rnf ImportFilter' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf values
      `Prelude.seq` Prelude.rnf operator

instance Data.ToJSON ImportFilter where
  toJSON ImportFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Data..= name),
            Prelude.Just ("values" Data..= values),
            Prelude.Just ("operator" Data..= operator)
          ]
      )
