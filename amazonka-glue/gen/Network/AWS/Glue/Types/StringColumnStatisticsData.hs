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
-- Module      : Network.AWS.Glue.Types.StringColumnStatisticsData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.StringColumnStatisticsData where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Defines column statistics supported for character sequence data values.
--
-- /See:/ 'newStringColumnStatisticsData' smart constructor.
data StringColumnStatisticsData = StringColumnStatisticsData'
  { -- | The size of the longest string in the column.
    maximumLength :: Core.Natural,
    -- | The average string length in the column.
    averageLength :: Core.Double,
    -- | The number of null values in the column.
    numberOfNulls :: Core.Natural,
    -- | The number of distinct values in a column.
    numberOfDistinctValues :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StringColumnStatisticsData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maximumLength', 'stringColumnStatisticsData_maximumLength' - The size of the longest string in the column.
--
-- 'averageLength', 'stringColumnStatisticsData_averageLength' - The average string length in the column.
--
-- 'numberOfNulls', 'stringColumnStatisticsData_numberOfNulls' - The number of null values in the column.
--
-- 'numberOfDistinctValues', 'stringColumnStatisticsData_numberOfDistinctValues' - The number of distinct values in a column.
newStringColumnStatisticsData ::
  -- | 'maximumLength'
  Core.Natural ->
  -- | 'averageLength'
  Core.Double ->
  -- | 'numberOfNulls'
  Core.Natural ->
  -- | 'numberOfDistinctValues'
  Core.Natural ->
  StringColumnStatisticsData
newStringColumnStatisticsData
  pMaximumLength_
  pAverageLength_
  pNumberOfNulls_
  pNumberOfDistinctValues_ =
    StringColumnStatisticsData'
      { maximumLength =
          pMaximumLength_,
        averageLength = pAverageLength_,
        numberOfNulls = pNumberOfNulls_,
        numberOfDistinctValues =
          pNumberOfDistinctValues_
      }

-- | The size of the longest string in the column.
stringColumnStatisticsData_maximumLength :: Lens.Lens' StringColumnStatisticsData Core.Natural
stringColumnStatisticsData_maximumLength = Lens.lens (\StringColumnStatisticsData' {maximumLength} -> maximumLength) (\s@StringColumnStatisticsData' {} a -> s {maximumLength = a} :: StringColumnStatisticsData)

-- | The average string length in the column.
stringColumnStatisticsData_averageLength :: Lens.Lens' StringColumnStatisticsData Core.Double
stringColumnStatisticsData_averageLength = Lens.lens (\StringColumnStatisticsData' {averageLength} -> averageLength) (\s@StringColumnStatisticsData' {} a -> s {averageLength = a} :: StringColumnStatisticsData)

-- | The number of null values in the column.
stringColumnStatisticsData_numberOfNulls :: Lens.Lens' StringColumnStatisticsData Core.Natural
stringColumnStatisticsData_numberOfNulls = Lens.lens (\StringColumnStatisticsData' {numberOfNulls} -> numberOfNulls) (\s@StringColumnStatisticsData' {} a -> s {numberOfNulls = a} :: StringColumnStatisticsData)

-- | The number of distinct values in a column.
stringColumnStatisticsData_numberOfDistinctValues :: Lens.Lens' StringColumnStatisticsData Core.Natural
stringColumnStatisticsData_numberOfDistinctValues = Lens.lens (\StringColumnStatisticsData' {numberOfDistinctValues} -> numberOfDistinctValues) (\s@StringColumnStatisticsData' {} a -> s {numberOfDistinctValues = a} :: StringColumnStatisticsData)

instance Core.FromJSON StringColumnStatisticsData where
  parseJSON =
    Core.withObject
      "StringColumnStatisticsData"
      ( \x ->
          StringColumnStatisticsData'
            Core.<$> (x Core..: "MaximumLength")
            Core.<*> (x Core..: "AverageLength")
            Core.<*> (x Core..: "NumberOfNulls")
            Core.<*> (x Core..: "NumberOfDistinctValues")
      )

instance Core.Hashable StringColumnStatisticsData

instance Core.NFData StringColumnStatisticsData

instance Core.ToJSON StringColumnStatisticsData where
  toJSON StringColumnStatisticsData' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("MaximumLength" Core..= maximumLength),
            Core.Just ("AverageLength" Core..= averageLength),
            Core.Just ("NumberOfNulls" Core..= numberOfNulls),
            Core.Just
              ( "NumberOfDistinctValues"
                  Core..= numberOfDistinctValues
              )
          ]
      )
