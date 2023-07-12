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
-- Module      : Amazonka.QuickSight.Types.NumericRangeFilterValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.NumericRangeFilterValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The value input pf the numeric range filter.
--
-- /See:/ 'newNumericRangeFilterValue' smart constructor.
data NumericRangeFilterValue = NumericRangeFilterValue'
  { -- | The parameter that is used in the numeric range.
    parameter :: Prelude.Maybe Prelude.Text,
    -- | The static value of the numeric range filter.
    staticValue :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NumericRangeFilterValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameter', 'numericRangeFilterValue_parameter' - The parameter that is used in the numeric range.
--
-- 'staticValue', 'numericRangeFilterValue_staticValue' - The static value of the numeric range filter.
newNumericRangeFilterValue ::
  NumericRangeFilterValue
newNumericRangeFilterValue =
  NumericRangeFilterValue'
    { parameter =
        Prelude.Nothing,
      staticValue = Prelude.Nothing
    }

-- | The parameter that is used in the numeric range.
numericRangeFilterValue_parameter :: Lens.Lens' NumericRangeFilterValue (Prelude.Maybe Prelude.Text)
numericRangeFilterValue_parameter = Lens.lens (\NumericRangeFilterValue' {parameter} -> parameter) (\s@NumericRangeFilterValue' {} a -> s {parameter = a} :: NumericRangeFilterValue)

-- | The static value of the numeric range filter.
numericRangeFilterValue_staticValue :: Lens.Lens' NumericRangeFilterValue (Prelude.Maybe Prelude.Double)
numericRangeFilterValue_staticValue = Lens.lens (\NumericRangeFilterValue' {staticValue} -> staticValue) (\s@NumericRangeFilterValue' {} a -> s {staticValue = a} :: NumericRangeFilterValue)

instance Data.FromJSON NumericRangeFilterValue where
  parseJSON =
    Data.withObject
      "NumericRangeFilterValue"
      ( \x ->
          NumericRangeFilterValue'
            Prelude.<$> (x Data..:? "Parameter")
            Prelude.<*> (x Data..:? "StaticValue")
      )

instance Prelude.Hashable NumericRangeFilterValue where
  hashWithSalt _salt NumericRangeFilterValue' {..} =
    _salt
      `Prelude.hashWithSalt` parameter
      `Prelude.hashWithSalt` staticValue

instance Prelude.NFData NumericRangeFilterValue where
  rnf NumericRangeFilterValue' {..} =
    Prelude.rnf parameter
      `Prelude.seq` Prelude.rnf staticValue

instance Data.ToJSON NumericRangeFilterValue where
  toJSON NumericRangeFilterValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Parameter" Data..=) Prelude.<$> parameter,
            ("StaticValue" Data..=) Prelude.<$> staticValue
          ]
      )
