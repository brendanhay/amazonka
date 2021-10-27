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
-- Module      : Network.AWS.LexV2Models.Types.ExportFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Models.Types.ExportFilter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Models.Types.ExportFilterName
import Network.AWS.LexV2Models.Types.ExportFilterOperator
import qualified Network.AWS.Prelude as Prelude

-- | Filters the response form the operation
--
-- /See:/ 'newExportFilter' smart constructor.
data ExportFilter = ExportFilter'
  { -- | The name of the field to use for filtering.
    name :: ExportFilterName,
    -- | The values to use to filter the response.
    values :: Prelude.NonEmpty Prelude.Text,
    -- | The operator to use for the filter. Specify EQ when the @ListExports@
    -- operation should return only resource types that equal the specified
    -- value. Specify CO when the @ListExports@ operation should return
    -- resource types that contain the specified value.
    operator :: ExportFilterOperator
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'exportFilter_name' - The name of the field to use for filtering.
--
-- 'values', 'exportFilter_values' - The values to use to filter the response.
--
-- 'operator', 'exportFilter_operator' - The operator to use for the filter. Specify EQ when the @ListExports@
-- operation should return only resource types that equal the specified
-- value. Specify CO when the @ListExports@ operation should return
-- resource types that contain the specified value.
newExportFilter ::
  -- | 'name'
  ExportFilterName ->
  -- | 'values'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'operator'
  ExportFilterOperator ->
  ExportFilter
newExportFilter pName_ pValues_ pOperator_ =
  ExportFilter'
    { name = pName_,
      values = Lens.coerced Lens.# pValues_,
      operator = pOperator_
    }

-- | The name of the field to use for filtering.
exportFilter_name :: Lens.Lens' ExportFilter ExportFilterName
exportFilter_name = Lens.lens (\ExportFilter' {name} -> name) (\s@ExportFilter' {} a -> s {name = a} :: ExportFilter)

-- | The values to use to filter the response.
exportFilter_values :: Lens.Lens' ExportFilter (Prelude.NonEmpty Prelude.Text)
exportFilter_values = Lens.lens (\ExportFilter' {values} -> values) (\s@ExportFilter' {} a -> s {values = a} :: ExportFilter) Prelude.. Lens.coerced

-- | The operator to use for the filter. Specify EQ when the @ListExports@
-- operation should return only resource types that equal the specified
-- value. Specify CO when the @ListExports@ operation should return
-- resource types that contain the specified value.
exportFilter_operator :: Lens.Lens' ExportFilter ExportFilterOperator
exportFilter_operator = Lens.lens (\ExportFilter' {operator} -> operator) (\s@ExportFilter' {} a -> s {operator = a} :: ExportFilter)

instance Prelude.Hashable ExportFilter

instance Prelude.NFData ExportFilter

instance Core.ToJSON ExportFilter where
  toJSON ExportFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Core..= name),
            Prelude.Just ("values" Core..= values),
            Prelude.Just ("operator" Core..= operator)
          ]
      )
