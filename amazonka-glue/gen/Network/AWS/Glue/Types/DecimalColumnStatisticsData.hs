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
-- Module      : Network.AWS.Glue.Types.DecimalColumnStatisticsData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.DecimalColumnStatisticsData where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.DecimalNumber
import qualified Network.AWS.Lens as Lens

-- | Defines column statistics supported for fixed-point number data columns.
--
-- /See:/ 'newDecimalColumnStatisticsData' smart constructor.
data DecimalColumnStatisticsData = DecimalColumnStatisticsData'
  { -- | The highest value in the column.
    maximumValue :: Core.Maybe DecimalNumber,
    -- | The lowest value in the column.
    minimumValue :: Core.Maybe DecimalNumber,
    -- | The number of null values in the column.
    numberOfNulls :: Core.Natural,
    -- | The number of distinct values in a column.
    numberOfDistinctValues :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Natural ->
  -- | 'numberOfDistinctValues'
  Core.Natural ->
  DecimalColumnStatisticsData
newDecimalColumnStatisticsData
  pNumberOfNulls_
  pNumberOfDistinctValues_ =
    DecimalColumnStatisticsData'
      { maximumValue =
          Core.Nothing,
        minimumValue = Core.Nothing,
        numberOfNulls = pNumberOfNulls_,
        numberOfDistinctValues =
          pNumberOfDistinctValues_
      }

-- | The highest value in the column.
decimalColumnStatisticsData_maximumValue :: Lens.Lens' DecimalColumnStatisticsData (Core.Maybe DecimalNumber)
decimalColumnStatisticsData_maximumValue = Lens.lens (\DecimalColumnStatisticsData' {maximumValue} -> maximumValue) (\s@DecimalColumnStatisticsData' {} a -> s {maximumValue = a} :: DecimalColumnStatisticsData)

-- | The lowest value in the column.
decimalColumnStatisticsData_minimumValue :: Lens.Lens' DecimalColumnStatisticsData (Core.Maybe DecimalNumber)
decimalColumnStatisticsData_minimumValue = Lens.lens (\DecimalColumnStatisticsData' {minimumValue} -> minimumValue) (\s@DecimalColumnStatisticsData' {} a -> s {minimumValue = a} :: DecimalColumnStatisticsData)

-- | The number of null values in the column.
decimalColumnStatisticsData_numberOfNulls :: Lens.Lens' DecimalColumnStatisticsData Core.Natural
decimalColumnStatisticsData_numberOfNulls = Lens.lens (\DecimalColumnStatisticsData' {numberOfNulls} -> numberOfNulls) (\s@DecimalColumnStatisticsData' {} a -> s {numberOfNulls = a} :: DecimalColumnStatisticsData)

-- | The number of distinct values in a column.
decimalColumnStatisticsData_numberOfDistinctValues :: Lens.Lens' DecimalColumnStatisticsData Core.Natural
decimalColumnStatisticsData_numberOfDistinctValues = Lens.lens (\DecimalColumnStatisticsData' {numberOfDistinctValues} -> numberOfDistinctValues) (\s@DecimalColumnStatisticsData' {} a -> s {numberOfDistinctValues = a} :: DecimalColumnStatisticsData)

instance Core.FromJSON DecimalColumnStatisticsData where
  parseJSON =
    Core.withObject
      "DecimalColumnStatisticsData"
      ( \x ->
          DecimalColumnStatisticsData'
            Core.<$> (x Core..:? "MaximumValue")
            Core.<*> (x Core..:? "MinimumValue")
            Core.<*> (x Core..: "NumberOfNulls")
            Core.<*> (x Core..: "NumberOfDistinctValues")
      )

instance Core.Hashable DecimalColumnStatisticsData

instance Core.NFData DecimalColumnStatisticsData

instance Core.ToJSON DecimalColumnStatisticsData where
  toJSON DecimalColumnStatisticsData' {..} =
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
