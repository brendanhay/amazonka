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
-- Module      : Amazonka.Glue.Types.DecimalColumnStatisticsData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.DecimalColumnStatisticsData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.DecimalNumber
import qualified Amazonka.Prelude as Prelude

-- | Defines column statistics supported for fixed-point number data columns.
--
-- /See:/ 'newDecimalColumnStatisticsData' smart constructor.
data DecimalColumnStatisticsData = DecimalColumnStatisticsData'
  { -- | The highest value in the column.
    maximumValue :: Prelude.Maybe DecimalNumber,
    -- | The lowest value in the column.
    minimumValue :: Prelude.Maybe DecimalNumber,
    -- | The number of null values in the column.
    numberOfNulls :: Prelude.Natural,
    -- | The number of distinct values in a column.
    numberOfDistinctValues :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DecimalColumnStatisticsData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maximumValue', 'decimalColumnStatisticsData_maximumValue' - The highest value in the column.
--
-- 'minimumValue', 'decimalColumnStatisticsData_minimumValue' - The lowest value in the column.
--
-- 'numberOfNulls', 'decimalColumnStatisticsData_numberOfNulls' - The number of null values in the column.
--
-- 'numberOfDistinctValues', 'decimalColumnStatisticsData_numberOfDistinctValues' - The number of distinct values in a column.
newDecimalColumnStatisticsData ::
  -- | 'numberOfNulls'
  Prelude.Natural ->
  -- | 'numberOfDistinctValues'
  Prelude.Natural ->
  DecimalColumnStatisticsData
newDecimalColumnStatisticsData
  pNumberOfNulls_
  pNumberOfDistinctValues_ =
    DecimalColumnStatisticsData'
      { maximumValue =
          Prelude.Nothing,
        minimumValue = Prelude.Nothing,
        numberOfNulls = pNumberOfNulls_,
        numberOfDistinctValues =
          pNumberOfDistinctValues_
      }

-- | The highest value in the column.
decimalColumnStatisticsData_maximumValue :: Lens.Lens' DecimalColumnStatisticsData (Prelude.Maybe DecimalNumber)
decimalColumnStatisticsData_maximumValue = Lens.lens (\DecimalColumnStatisticsData' {maximumValue} -> maximumValue) (\s@DecimalColumnStatisticsData' {} a -> s {maximumValue = a} :: DecimalColumnStatisticsData)

-- | The lowest value in the column.
decimalColumnStatisticsData_minimumValue :: Lens.Lens' DecimalColumnStatisticsData (Prelude.Maybe DecimalNumber)
decimalColumnStatisticsData_minimumValue = Lens.lens (\DecimalColumnStatisticsData' {minimumValue} -> minimumValue) (\s@DecimalColumnStatisticsData' {} a -> s {minimumValue = a} :: DecimalColumnStatisticsData)

-- | The number of null values in the column.
decimalColumnStatisticsData_numberOfNulls :: Lens.Lens' DecimalColumnStatisticsData Prelude.Natural
decimalColumnStatisticsData_numberOfNulls = Lens.lens (\DecimalColumnStatisticsData' {numberOfNulls} -> numberOfNulls) (\s@DecimalColumnStatisticsData' {} a -> s {numberOfNulls = a} :: DecimalColumnStatisticsData)

-- | The number of distinct values in a column.
decimalColumnStatisticsData_numberOfDistinctValues :: Lens.Lens' DecimalColumnStatisticsData Prelude.Natural
decimalColumnStatisticsData_numberOfDistinctValues = Lens.lens (\DecimalColumnStatisticsData' {numberOfDistinctValues} -> numberOfDistinctValues) (\s@DecimalColumnStatisticsData' {} a -> s {numberOfDistinctValues = a} :: DecimalColumnStatisticsData)

instance Data.FromJSON DecimalColumnStatisticsData where
  parseJSON =
    Data.withObject
      "DecimalColumnStatisticsData"
      ( \x ->
          DecimalColumnStatisticsData'
            Prelude.<$> (x Data..:? "MaximumValue")
            Prelude.<*> (x Data..:? "MinimumValue")
            Prelude.<*> (x Data..: "NumberOfNulls")
            Prelude.<*> (x Data..: "NumberOfDistinctValues")
      )

instance Prelude.Hashable DecimalColumnStatisticsData where
  hashWithSalt _salt DecimalColumnStatisticsData' {..} =
    _salt `Prelude.hashWithSalt` maximumValue
      `Prelude.hashWithSalt` minimumValue
      `Prelude.hashWithSalt` numberOfNulls
      `Prelude.hashWithSalt` numberOfDistinctValues

instance Prelude.NFData DecimalColumnStatisticsData where
  rnf DecimalColumnStatisticsData' {..} =
    Prelude.rnf maximumValue
      `Prelude.seq` Prelude.rnf minimumValue
      `Prelude.seq` Prelude.rnf numberOfNulls
      `Prelude.seq` Prelude.rnf numberOfDistinctValues

instance Data.ToJSON DecimalColumnStatisticsData where
  toJSON DecimalColumnStatisticsData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaximumValue" Data..=) Prelude.<$> maximumValue,
            ("MinimumValue" Data..=) Prelude.<$> minimumValue,
            Prelude.Just ("NumberOfNulls" Data..= numberOfNulls),
            Prelude.Just
              ( "NumberOfDistinctValues"
                  Data..= numberOfDistinctValues
              )
          ]
      )
