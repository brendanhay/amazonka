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
-- Module      : Network.AWS.Glue.Types.BinaryColumnStatisticsData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.BinaryColumnStatisticsData where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Defines column statistics supported for bit sequence data values.
--
-- /See:/ 'newBinaryColumnStatisticsData' smart constructor.
data BinaryColumnStatisticsData = BinaryColumnStatisticsData'
  { -- | The size of the longest bit sequence in the column.
    maximumLength :: Core.Natural,
    -- | The average bit sequence length in the column.
    averageLength :: Core.Double,
    -- | The number of null values in the column.
    numberOfNulls :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BinaryColumnStatisticsData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maximumLength', 'binaryColumnStatisticsData_maximumLength' - The size of the longest bit sequence in the column.
--
-- 'averageLength', 'binaryColumnStatisticsData_averageLength' - The average bit sequence length in the column.
--
-- 'numberOfNulls', 'binaryColumnStatisticsData_numberOfNulls' - The number of null values in the column.
newBinaryColumnStatisticsData ::
  -- | 'maximumLength'
  Core.Natural ->
  -- | 'averageLength'
  Core.Double ->
  -- | 'numberOfNulls'
  Core.Natural ->
  BinaryColumnStatisticsData
newBinaryColumnStatisticsData
  pMaximumLength_
  pAverageLength_
  pNumberOfNulls_ =
    BinaryColumnStatisticsData'
      { maximumLength =
          pMaximumLength_,
        averageLength = pAverageLength_,
        numberOfNulls = pNumberOfNulls_
      }

-- | The size of the longest bit sequence in the column.
binaryColumnStatisticsData_maximumLength :: Lens.Lens' BinaryColumnStatisticsData Core.Natural
binaryColumnStatisticsData_maximumLength = Lens.lens (\BinaryColumnStatisticsData' {maximumLength} -> maximumLength) (\s@BinaryColumnStatisticsData' {} a -> s {maximumLength = a} :: BinaryColumnStatisticsData)

-- | The average bit sequence length in the column.
binaryColumnStatisticsData_averageLength :: Lens.Lens' BinaryColumnStatisticsData Core.Double
binaryColumnStatisticsData_averageLength = Lens.lens (\BinaryColumnStatisticsData' {averageLength} -> averageLength) (\s@BinaryColumnStatisticsData' {} a -> s {averageLength = a} :: BinaryColumnStatisticsData)

-- | The number of null values in the column.
binaryColumnStatisticsData_numberOfNulls :: Lens.Lens' BinaryColumnStatisticsData Core.Natural
binaryColumnStatisticsData_numberOfNulls = Lens.lens (\BinaryColumnStatisticsData' {numberOfNulls} -> numberOfNulls) (\s@BinaryColumnStatisticsData' {} a -> s {numberOfNulls = a} :: BinaryColumnStatisticsData)

instance Core.FromJSON BinaryColumnStatisticsData where
  parseJSON =
    Core.withObject
      "BinaryColumnStatisticsData"
      ( \x ->
          BinaryColumnStatisticsData'
            Core.<$> (x Core..: "MaximumLength")
            Core.<*> (x Core..: "AverageLength")
            Core.<*> (x Core..: "NumberOfNulls")
      )

instance Core.Hashable BinaryColumnStatisticsData

instance Core.NFData BinaryColumnStatisticsData

instance Core.ToJSON BinaryColumnStatisticsData where
  toJSON BinaryColumnStatisticsData' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("MaximumLength" Core..= maximumLength),
            Core.Just ("AverageLength" Core..= averageLength),
            Core.Just ("NumberOfNulls" Core..= numberOfNulls)
          ]
      )
