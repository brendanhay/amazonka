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
-- Module      : Network.AWS.Glue.Types.DateColumnStatisticsData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.DateColumnStatisticsData where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Defines column statistics supported for timestamp data columns.
--
-- /See:/ 'newDateColumnStatisticsData' smart constructor.
data DateColumnStatisticsData = DateColumnStatisticsData'
  { -- | The highest value in the column.
    maximumValue :: Core.Maybe Core.POSIX,
    -- | The lowest value in the column.
    minimumValue :: Core.Maybe Core.POSIX,
    -- | The number of null values in the column.
    numberOfNulls :: Core.Natural,
    -- | The number of distinct values in a column.
    numberOfDistinctValues :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DateColumnStatisticsData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maximumValue', 'dateColumnStatisticsData_maximumValue' - The highest value in the column.
--
-- 'minimumValue', 'dateColumnStatisticsData_minimumValue' - The lowest value in the column.
--
-- 'numberOfNulls', 'dateColumnStatisticsData_numberOfNulls' - The number of null values in the column.
--
-- 'numberOfDistinctValues', 'dateColumnStatisticsData_numberOfDistinctValues' - The number of distinct values in a column.
newDateColumnStatisticsData ::
  -- | 'numberOfNulls'
  Core.Natural ->
  -- | 'numberOfDistinctValues'
  Core.Natural ->
  DateColumnStatisticsData
newDateColumnStatisticsData
  pNumberOfNulls_
  pNumberOfDistinctValues_ =
    DateColumnStatisticsData'
      { maximumValue =
          Core.Nothing,
        minimumValue = Core.Nothing,
        numberOfNulls = pNumberOfNulls_,
        numberOfDistinctValues = pNumberOfDistinctValues_
      }

-- | The highest value in the column.
dateColumnStatisticsData_maximumValue :: Lens.Lens' DateColumnStatisticsData (Core.Maybe Core.UTCTime)
dateColumnStatisticsData_maximumValue = Lens.lens (\DateColumnStatisticsData' {maximumValue} -> maximumValue) (\s@DateColumnStatisticsData' {} a -> s {maximumValue = a} :: DateColumnStatisticsData) Core.. Lens.mapping Core._Time

-- | The lowest value in the column.
dateColumnStatisticsData_minimumValue :: Lens.Lens' DateColumnStatisticsData (Core.Maybe Core.UTCTime)
dateColumnStatisticsData_minimumValue = Lens.lens (\DateColumnStatisticsData' {minimumValue} -> minimumValue) (\s@DateColumnStatisticsData' {} a -> s {minimumValue = a} :: DateColumnStatisticsData) Core.. Lens.mapping Core._Time

-- | The number of null values in the column.
dateColumnStatisticsData_numberOfNulls :: Lens.Lens' DateColumnStatisticsData Core.Natural
dateColumnStatisticsData_numberOfNulls = Lens.lens (\DateColumnStatisticsData' {numberOfNulls} -> numberOfNulls) (\s@DateColumnStatisticsData' {} a -> s {numberOfNulls = a} :: DateColumnStatisticsData)

-- | The number of distinct values in a column.
dateColumnStatisticsData_numberOfDistinctValues :: Lens.Lens' DateColumnStatisticsData Core.Natural
dateColumnStatisticsData_numberOfDistinctValues = Lens.lens (\DateColumnStatisticsData' {numberOfDistinctValues} -> numberOfDistinctValues) (\s@DateColumnStatisticsData' {} a -> s {numberOfDistinctValues = a} :: DateColumnStatisticsData)

instance Core.FromJSON DateColumnStatisticsData where
  parseJSON =
    Core.withObject
      "DateColumnStatisticsData"
      ( \x ->
          DateColumnStatisticsData'
            Core.<$> (x Core..:? "MaximumValue")
            Core.<*> (x Core..:? "MinimumValue")
            Core.<*> (x Core..: "NumberOfNulls")
            Core.<*> (x Core..: "NumberOfDistinctValues")
      )

instance Core.Hashable DateColumnStatisticsData

instance Core.NFData DateColumnStatisticsData

instance Core.ToJSON DateColumnStatisticsData where
  toJSON DateColumnStatisticsData' {..} =
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
