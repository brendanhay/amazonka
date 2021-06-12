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
-- Module      : Network.AWS.Glue.Types.DoubleColumnStatisticsData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.DoubleColumnStatisticsData where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Defines column statistics supported for floating-point number data
-- columns.
--
-- /See:/ 'newDoubleColumnStatisticsData' smart constructor.
data DoubleColumnStatisticsData = DoubleColumnStatisticsData'
  { -- | The highest value in the column.
    maximumValue :: Core.Maybe Core.Double,
    -- | The lowest value in the column.
    minimumValue :: Core.Maybe Core.Double,
    -- | The number of null values in the column.
    numberOfNulls :: Core.Natural,
    -- | The number of distinct values in a column.
    numberOfDistinctValues :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Natural ->
  -- | 'numberOfDistinctValues'
  Core.Natural ->
  DoubleColumnStatisticsData
newDoubleColumnStatisticsData
  pNumberOfNulls_
  pNumberOfDistinctValues_ =
    DoubleColumnStatisticsData'
      { maximumValue =
          Core.Nothing,
        minimumValue = Core.Nothing,
        numberOfNulls = pNumberOfNulls_,
        numberOfDistinctValues =
          pNumberOfDistinctValues_
      }

-- | The highest value in the column.
doubleColumnStatisticsData_maximumValue :: Lens.Lens' DoubleColumnStatisticsData (Core.Maybe Core.Double)
doubleColumnStatisticsData_maximumValue = Lens.lens (\DoubleColumnStatisticsData' {maximumValue} -> maximumValue) (\s@DoubleColumnStatisticsData' {} a -> s {maximumValue = a} :: DoubleColumnStatisticsData)

-- | The lowest value in the column.
doubleColumnStatisticsData_minimumValue :: Lens.Lens' DoubleColumnStatisticsData (Core.Maybe Core.Double)
doubleColumnStatisticsData_minimumValue = Lens.lens (\DoubleColumnStatisticsData' {minimumValue} -> minimumValue) (\s@DoubleColumnStatisticsData' {} a -> s {minimumValue = a} :: DoubleColumnStatisticsData)

-- | The number of null values in the column.
doubleColumnStatisticsData_numberOfNulls :: Lens.Lens' DoubleColumnStatisticsData Core.Natural
doubleColumnStatisticsData_numberOfNulls = Lens.lens (\DoubleColumnStatisticsData' {numberOfNulls} -> numberOfNulls) (\s@DoubleColumnStatisticsData' {} a -> s {numberOfNulls = a} :: DoubleColumnStatisticsData)

-- | The number of distinct values in a column.
doubleColumnStatisticsData_numberOfDistinctValues :: Lens.Lens' DoubleColumnStatisticsData Core.Natural
doubleColumnStatisticsData_numberOfDistinctValues = Lens.lens (\DoubleColumnStatisticsData' {numberOfDistinctValues} -> numberOfDistinctValues) (\s@DoubleColumnStatisticsData' {} a -> s {numberOfDistinctValues = a} :: DoubleColumnStatisticsData)

instance Core.FromJSON DoubleColumnStatisticsData where
  parseJSON =
    Core.withObject
      "DoubleColumnStatisticsData"
      ( \x ->
          DoubleColumnStatisticsData'
            Core.<$> (x Core..:? "MaximumValue")
            Core.<*> (x Core..:? "MinimumValue")
            Core.<*> (x Core..: "NumberOfNulls")
            Core.<*> (x Core..: "NumberOfDistinctValues")
      )

instance Core.Hashable DoubleColumnStatisticsData

instance Core.NFData DoubleColumnStatisticsData

instance Core.ToJSON DoubleColumnStatisticsData where
  toJSON DoubleColumnStatisticsData' {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaximumValue" Core..=) Core.<$> maximumValue,
            ("MinimumValue" Core..=) Core.<$> minimumValue,
            Core.Just ("NumberOfNulls" Core..= numberOfNulls),
            Core.Just
              ( "NumberOfDistinctValues"
                  Core..= numberOfDistinctValues
              )
          ]
      )
