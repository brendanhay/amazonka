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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.DoubleColumnStatisticsData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Defines column statistics supported for floating-point number data
-- columns.
--
-- /See:/ 'newDoubleColumnStatisticsData' smart constructor.
data DoubleColumnStatisticsData = DoubleColumnStatisticsData'
  { -- | The lowest value in the column.
    minimumValue :: Prelude.Maybe Prelude.Double,
    -- | The highest value in the column.
    maximumValue :: Prelude.Maybe Prelude.Double,
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
-- 'minimumValue', 'doubleColumnStatisticsData_minimumValue' - The lowest value in the column.
--
-- 'maximumValue', 'doubleColumnStatisticsData_maximumValue' - The highest value in the column.
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
      { minimumValue =
          Prelude.Nothing,
        maximumValue = Prelude.Nothing,
        numberOfNulls = pNumberOfNulls_,
        numberOfDistinctValues =
          pNumberOfDistinctValues_
      }

-- | The lowest value in the column.
doubleColumnStatisticsData_minimumValue :: Lens.Lens' DoubleColumnStatisticsData (Prelude.Maybe Prelude.Double)
doubleColumnStatisticsData_minimumValue = Lens.lens (\DoubleColumnStatisticsData' {minimumValue} -> minimumValue) (\s@DoubleColumnStatisticsData' {} a -> s {minimumValue = a} :: DoubleColumnStatisticsData)

-- | The highest value in the column.
doubleColumnStatisticsData_maximumValue :: Lens.Lens' DoubleColumnStatisticsData (Prelude.Maybe Prelude.Double)
doubleColumnStatisticsData_maximumValue = Lens.lens (\DoubleColumnStatisticsData' {maximumValue} -> maximumValue) (\s@DoubleColumnStatisticsData' {} a -> s {maximumValue = a} :: DoubleColumnStatisticsData)

-- | The number of null values in the column.
doubleColumnStatisticsData_numberOfNulls :: Lens.Lens' DoubleColumnStatisticsData Prelude.Natural
doubleColumnStatisticsData_numberOfNulls = Lens.lens (\DoubleColumnStatisticsData' {numberOfNulls} -> numberOfNulls) (\s@DoubleColumnStatisticsData' {} a -> s {numberOfNulls = a} :: DoubleColumnStatisticsData)

-- | The number of distinct values in a column.
doubleColumnStatisticsData_numberOfDistinctValues :: Lens.Lens' DoubleColumnStatisticsData Prelude.Natural
doubleColumnStatisticsData_numberOfDistinctValues = Lens.lens (\DoubleColumnStatisticsData' {numberOfDistinctValues} -> numberOfDistinctValues) (\s@DoubleColumnStatisticsData' {} a -> s {numberOfDistinctValues = a} :: DoubleColumnStatisticsData)

instance Core.FromJSON DoubleColumnStatisticsData where
  parseJSON =
    Core.withObject
      "DoubleColumnStatisticsData"
      ( \x ->
          DoubleColumnStatisticsData'
            Prelude.<$> (x Core..:? "MinimumValue")
            Prelude.<*> (x Core..:? "MaximumValue")
            Prelude.<*> (x Core..: "NumberOfNulls")
            Prelude.<*> (x Core..: "NumberOfDistinctValues")
      )

instance Prelude.Hashable DoubleColumnStatisticsData where
  hashWithSalt _salt DoubleColumnStatisticsData' {..} =
    _salt `Prelude.hashWithSalt` minimumValue
      `Prelude.hashWithSalt` maximumValue
      `Prelude.hashWithSalt` numberOfNulls
      `Prelude.hashWithSalt` numberOfDistinctValues

instance Prelude.NFData DoubleColumnStatisticsData where
  rnf DoubleColumnStatisticsData' {..} =
    Prelude.rnf minimumValue
      `Prelude.seq` Prelude.rnf maximumValue
      `Prelude.seq` Prelude.rnf numberOfNulls
      `Prelude.seq` Prelude.rnf numberOfDistinctValues

instance Core.ToJSON DoubleColumnStatisticsData where
  toJSON DoubleColumnStatisticsData' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MinimumValue" Core..=) Prelude.<$> minimumValue,
            ("MaximumValue" Core..=) Prelude.<$> maximumValue,
            Prelude.Just ("NumberOfNulls" Core..= numberOfNulls),
            Prelude.Just
              ( "NumberOfDistinctValues"
                  Core..= numberOfDistinctValues
              )
          ]
      )
