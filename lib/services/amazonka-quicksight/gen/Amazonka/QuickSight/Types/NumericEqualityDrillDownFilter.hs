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
-- Module      : Amazonka.QuickSight.Types.NumericEqualityDrillDownFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.NumericEqualityDrillDownFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ColumnIdentifier

-- | The category drill down filter.
--
-- /See:/ 'newNumericEqualityDrillDownFilter' smart constructor.
data NumericEqualityDrillDownFilter = NumericEqualityDrillDownFilter'
  { -- | The column that the filter is applied to.
    column :: ColumnIdentifier,
    -- | The value of the double input numeric drill down filter.
    value :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NumericEqualityDrillDownFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'column', 'numericEqualityDrillDownFilter_column' - The column that the filter is applied to.
--
-- 'value', 'numericEqualityDrillDownFilter_value' - The value of the double input numeric drill down filter.
newNumericEqualityDrillDownFilter ::
  -- | 'column'
  ColumnIdentifier ->
  -- | 'value'
  Prelude.Double ->
  NumericEqualityDrillDownFilter
newNumericEqualityDrillDownFilter pColumn_ pValue_ =
  NumericEqualityDrillDownFilter'
    { column = pColumn_,
      value = pValue_
    }

-- | The column that the filter is applied to.
numericEqualityDrillDownFilter_column :: Lens.Lens' NumericEqualityDrillDownFilter ColumnIdentifier
numericEqualityDrillDownFilter_column = Lens.lens (\NumericEqualityDrillDownFilter' {column} -> column) (\s@NumericEqualityDrillDownFilter' {} a -> s {column = a} :: NumericEqualityDrillDownFilter)

-- | The value of the double input numeric drill down filter.
numericEqualityDrillDownFilter_value :: Lens.Lens' NumericEqualityDrillDownFilter Prelude.Double
numericEqualityDrillDownFilter_value = Lens.lens (\NumericEqualityDrillDownFilter' {value} -> value) (\s@NumericEqualityDrillDownFilter' {} a -> s {value = a} :: NumericEqualityDrillDownFilter)

instance Data.FromJSON NumericEqualityDrillDownFilter where
  parseJSON =
    Data.withObject
      "NumericEqualityDrillDownFilter"
      ( \x ->
          NumericEqualityDrillDownFilter'
            Prelude.<$> (x Data..: "Column")
            Prelude.<*> (x Data..: "Value")
      )

instance
  Prelude.Hashable
    NumericEqualityDrillDownFilter
  where
  hashWithSalt
    _salt
    NumericEqualityDrillDownFilter' {..} =
      _salt
        `Prelude.hashWithSalt` column
        `Prelude.hashWithSalt` value

instance
  Prelude.NFData
    NumericEqualityDrillDownFilter
  where
  rnf NumericEqualityDrillDownFilter' {..} =
    Prelude.rnf column `Prelude.seq` Prelude.rnf value

instance Data.ToJSON NumericEqualityDrillDownFilter where
  toJSON NumericEqualityDrillDownFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Column" Data..= column),
            Prelude.Just ("Value" Data..= value)
          ]
      )
