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
-- Module      : Network.AWS.LexV2Models.Types.ImportFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Models.Types.ImportFilter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Models.Types.ImportFilterName
import Network.AWS.LexV2Models.Types.ImportFilterOperator
import qualified Network.AWS.Prelude as Prelude

-- | Filters the response from the operation.
--
-- /See:/ 'newImportFilter' smart constructor.
data ImportFilter = ImportFilter'
  { -- | The name of the field to use for filtering.
    name :: ImportFilterName,
    -- | The values to use to filter the response.
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
-- 'values', 'importFilter_values' - The values to use to filter the response.
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

-- | The values to use to filter the response.
importFilter_values :: Lens.Lens' ImportFilter (Prelude.NonEmpty Prelude.Text)
importFilter_values = Lens.lens (\ImportFilter' {values} -> values) (\s@ImportFilter' {} a -> s {values = a} :: ImportFilter) Prelude.. Lens.coerced

-- | The operator to use for the filter. Specify EQ when the @ListImports@
-- operation should return only resource types that equal the specified
-- value. Specify CO when the @ListImports@ operation should return
-- resource types that contain the specified value.
importFilter_operator :: Lens.Lens' ImportFilter ImportFilterOperator
importFilter_operator = Lens.lens (\ImportFilter' {operator} -> operator) (\s@ImportFilter' {} a -> s {operator = a} :: ImportFilter)

instance Prelude.Hashable ImportFilter

instance Prelude.NFData ImportFilter

instance Core.ToJSON ImportFilter where
  toJSON ImportFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Core..= name),
            Prelude.Just ("values" Core..= values),
            Prelude.Just ("operator" Core..= operator)
          ]
      )
