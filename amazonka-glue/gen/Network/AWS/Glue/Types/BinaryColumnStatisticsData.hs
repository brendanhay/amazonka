{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Defines column statistics supported for bit sequence data values.
--
-- /See:/ 'newBinaryColumnStatisticsData' smart constructor.
data BinaryColumnStatisticsData = BinaryColumnStatisticsData'
  { -- | The size of the longest bit sequence in the column.
    maximumLength :: Prelude.Natural,
    -- | The average bit sequence length in the column.
    averageLength :: Prelude.Double,
    -- | The number of null values in the column.
    numberOfNulls :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Natural ->
  -- | 'averageLength'
  Prelude.Double ->
  -- | 'numberOfNulls'
  Prelude.Natural ->
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
binaryColumnStatisticsData_maximumLength :: Lens.Lens' BinaryColumnStatisticsData Prelude.Natural
binaryColumnStatisticsData_maximumLength = Lens.lens (\BinaryColumnStatisticsData' {maximumLength} -> maximumLength) (\s@BinaryColumnStatisticsData' {} a -> s {maximumLength = a} :: BinaryColumnStatisticsData)

-- | The average bit sequence length in the column.
binaryColumnStatisticsData_averageLength :: Lens.Lens' BinaryColumnStatisticsData Prelude.Double
binaryColumnStatisticsData_averageLength = Lens.lens (\BinaryColumnStatisticsData' {averageLength} -> averageLength) (\s@BinaryColumnStatisticsData' {} a -> s {averageLength = a} :: BinaryColumnStatisticsData)

-- | The number of null values in the column.
binaryColumnStatisticsData_numberOfNulls :: Lens.Lens' BinaryColumnStatisticsData Prelude.Natural
binaryColumnStatisticsData_numberOfNulls = Lens.lens (\BinaryColumnStatisticsData' {numberOfNulls} -> numberOfNulls) (\s@BinaryColumnStatisticsData' {} a -> s {numberOfNulls = a} :: BinaryColumnStatisticsData)

instance Prelude.FromJSON BinaryColumnStatisticsData where
  parseJSON =
    Prelude.withObject
      "BinaryColumnStatisticsData"
      ( \x ->
          BinaryColumnStatisticsData'
            Prelude.<$> (x Prelude..: "MaximumLength")
            Prelude.<*> (x Prelude..: "AverageLength")
            Prelude.<*> (x Prelude..: "NumberOfNulls")
      )

instance Prelude.Hashable BinaryColumnStatisticsData

instance Prelude.NFData BinaryColumnStatisticsData

instance Prelude.ToJSON BinaryColumnStatisticsData where
  toJSON BinaryColumnStatisticsData' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("MaximumLength" Prelude..= maximumLength),
            Prelude.Just
              ("AverageLength" Prelude..= averageLength),
            Prelude.Just
              ("NumberOfNulls" Prelude..= numberOfNulls)
          ]
      )
