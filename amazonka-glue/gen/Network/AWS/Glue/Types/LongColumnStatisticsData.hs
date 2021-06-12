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
-- Module      : Network.AWS.Glue.Types.LongColumnStatisticsData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.LongColumnStatisticsData where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Defines column statistics supported for integer data columns.
--
-- /See:/ 'newLongColumnStatisticsData' smart constructor.
data LongColumnStatisticsData = LongColumnStatisticsData'
  { -- | The highest value in the column.
    maximumValue :: Core.Maybe Core.Integer,
    -- | The lowest value in the column.
    minimumValue :: Core.Maybe Core.Integer,
    -- | The number of null values in the column.
    numberOfNulls :: Core.Natural,
    -- | The number of distinct values in a column.
    numberOfDistinctValues :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LongColumnStatisticsData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maximumValue', 'longColumnStatisticsData_maximumValue' - The highest value in the column.
--
-- 'minimumValue', 'longColumnStatisticsData_minimumValue' - The lowest value in the column.
--
-- 'numberOfNulls', 'longColumnStatisticsData_numberOfNulls' - The number of null values in the column.
--
-- 'numberOfDistinctValues', 'longColumnStatisticsData_numberOfDistinctValues' - The number of distinct values in a column.
newLongColumnStatisticsData ::
  -- | 'numberOfNulls'
  Core.Natural ->
  -- | 'numberOfDistinctValues'
  Core.Natural ->
  LongColumnStatisticsData
newLongColumnStatisticsData
  pNumberOfNulls_
  pNumberOfDistinctValues_ =
    LongColumnStatisticsData'
      { maximumValue =
          Core.Nothing,
        minimumValue = Core.Nothing,
        numberOfNulls = pNumberOfNulls_,
        numberOfDistinctValues = pNumberOfDistinctValues_
      }

-- | The highest value in the column.
longColumnStatisticsData_maximumValue :: Lens.Lens' LongColumnStatisticsData (Core.Maybe Core.Integer)
longColumnStatisticsData_maximumValue = Lens.lens (\LongColumnStatisticsData' {maximumValue} -> maximumValue) (\s@LongColumnStatisticsData' {} a -> s {maximumValue = a} :: LongColumnStatisticsData)

-- | The lowest value in the column.
longColumnStatisticsData_minimumValue :: Lens.Lens' LongColumnStatisticsData (Core.Maybe Core.Integer)
longColumnStatisticsData_minimumValue = Lens.lens (\LongColumnStatisticsData' {minimumValue} -> minimumValue) (\s@LongColumnStatisticsData' {} a -> s {minimumValue = a} :: LongColumnStatisticsData)

-- | The number of null values in the column.
longColumnStatisticsData_numberOfNulls :: Lens.Lens' LongColumnStatisticsData Core.Natural
longColumnStatisticsData_numberOfNulls = Lens.lens (\LongColumnStatisticsData' {numberOfNulls} -> numberOfNulls) (\s@LongColumnStatisticsData' {} a -> s {numberOfNulls = a} :: LongColumnStatisticsData)

-- | The number of distinct values in a column.
longColumnStatisticsData_numberOfDistinctValues :: Lens.Lens' LongColumnStatisticsData Core.Natural
longColumnStatisticsData_numberOfDistinctValues = Lens.lens (\LongColumnStatisticsData' {numberOfDistinctValues} -> numberOfDistinctValues) (\s@LongColumnStatisticsData' {} a -> s {numberOfDistinctValues = a} :: LongColumnStatisticsData)

instance Core.FromJSON LongColumnStatisticsData where
  parseJSON =
    Core.withObject
      "LongColumnStatisticsData"
      ( \x ->
          LongColumnStatisticsData'
            Core.<$> (x Core..:? "MaximumValue")
            Core.<*> (x Core..:? "MinimumValue")
            Core.<*> (x Core..: "NumberOfNulls")
            Core.<*> (x Core..: "NumberOfDistinctValues")
      )

instance Core.Hashable LongColumnStatisticsData

instance Core.NFData LongColumnStatisticsData

instance Core.ToJSON LongColumnStatisticsData where
  toJSON LongColumnStatisticsData' {..} =
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
