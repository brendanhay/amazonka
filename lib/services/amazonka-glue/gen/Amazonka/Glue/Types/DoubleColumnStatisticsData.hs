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
-- Module      : Amazonka.Glue.Types.DoubleColumnStatisticsData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.DoubleColumnStatisticsData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines column statistics supported for floating-point number data
-- columns.
--
-- /See:/ 'newDoubleColumnStatisticsData' smart constructor.
data DoubleColumnStatisticsData = DoubleColumnStatisticsData'
  { -- | The highest value in the column.
    maximumValue :: Prelude.Maybe Prelude.Double,
    -- | The lowest value in the column.
    minimumValue :: Prelude.Maybe Prelude.Double,
    -- | The number of null values in the column.
    numberOfNulls :: Prelude.Natural,
    -- | The number of distinct values in a column.
    numberOfDistinctValues :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DoubleColumnStatisticsData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maximumValue', 'doubleColumnStatisticsData_maximumValue' - The highest value in the column.
--
-- 'minimumValue', 'doubleColumnStatisticsData_minimumValue' - The lowest value in the column.
--
-- 'numberOfNulls', 'doubleColumnStatisticsData_numberOfNulls' - The number of null values in the column.
--
-- 'numberOfDistinctValues', 'doubleColumnStatisticsData_numberOfDistinctValues' - The number of distinct values in a column.
newDoubleColumnStatisticsData ::
  -- | 'numberOfNulls'
  Prelude.Natural ->
  -- | 'numberOfDistinctValues'
  Prelude.Natural ->
  DoubleColumnStatisticsData
newDoubleColumnStatisticsData
  pNumberOfNulls_
  pNumberOfDistinctValues_ =
    DoubleColumnStatisticsData'
      { maximumValue =
          Prelude.Nothing,
        minimumValue = Prelude.Nothing,
        numberOfNulls = pNumberOfNulls_,
        numberOfDistinctValues =
          pNumberOfDistinctValues_
      }

-- | The highest value in the column.
doubleColumnStatisticsData_maximumValue :: Lens.Lens' DoubleColumnStatisticsData (Prelude.Maybe Prelude.Double)
doubleColumnStatisticsData_maximumValue = Lens.lens (\DoubleColumnStatisticsData' {maximumValue} -> maximumValue) (\s@DoubleColumnStatisticsData' {} a -> s {maximumValue = a} :: DoubleColumnStatisticsData)

-- | The lowest value in the column.
doubleColumnStatisticsData_minimumValue :: Lens.Lens' DoubleColumnStatisticsData (Prelude.Maybe Prelude.Double)
doubleColumnStatisticsData_minimumValue = Lens.lens (\DoubleColumnStatisticsData' {minimumValue} -> minimumValue) (\s@DoubleColumnStatisticsData' {} a -> s {minimumValue = a} :: DoubleColumnStatisticsData)

-- | The number of null values in the column.
doubleColumnStatisticsData_numberOfNulls :: Lens.Lens' DoubleColumnStatisticsData Prelude.Natural
doubleColumnStatisticsData_numberOfNulls = Lens.lens (\DoubleColumnStatisticsData' {numberOfNulls} -> numberOfNulls) (\s@DoubleColumnStatisticsData' {} a -> s {numberOfNulls = a} :: DoubleColumnStatisticsData)

-- | The number of distinct values in a column.
doubleColumnStatisticsData_numberOfDistinctValues :: Lens.Lens' DoubleColumnStatisticsData Prelude.Natural
doubleColumnStatisticsData_numberOfDistinctValues = Lens.lens (\DoubleColumnStatisticsData' {numberOfDistinctValues} -> numberOfDistinctValues) (\s@DoubleColumnStatisticsData' {} a -> s {numberOfDistinctValues = a} :: DoubleColumnStatisticsData)

instance Data.FromJSON DoubleColumnStatisticsData where
  parseJSON =
    Data.withObject
      "DoubleColumnStatisticsData"
      ( \x ->
          DoubleColumnStatisticsData'
            Prelude.<$> (x Data..:? "MaximumValue")
            Prelude.<*> (x Data..:? "MinimumValue")
            Prelude.<*> (x Data..: "NumberOfNulls")
            Prelude.<*> (x Data..: "NumberOfDistinctValues")
      )

instance Prelude.Hashable DoubleColumnStatisticsData where
  hashWithSalt _salt DoubleColumnStatisticsData' {..} =
    _salt
      `Prelude.hashWithSalt` maximumValue
      `Prelude.hashWithSalt` minimumValue
      `Prelude.hashWithSalt` numberOfNulls
      `Prelude.hashWithSalt` numberOfDistinctValues

instance Prelude.NFData DoubleColumnStatisticsData where
  rnf DoubleColumnStatisticsData' {..} =
    Prelude.rnf maximumValue
      `Prelude.seq` Prelude.rnf minimumValue
      `Prelude.seq` Prelude.rnf numberOfNulls
      `Prelude.seq` Prelude.rnf numberOfDistinctValues

instance Data.ToJSON DoubleColumnStatisticsData where
  toJSON DoubleColumnStatisticsData' {..} =
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
