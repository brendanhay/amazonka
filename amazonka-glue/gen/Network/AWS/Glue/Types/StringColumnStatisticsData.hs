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
-- Module      : Network.AWS.Glue.Types.StringColumnStatisticsData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.StringColumnStatisticsData where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Defines column statistics supported for character sequence data values.
--
-- /See:/ 'newStringColumnStatisticsData' smart constructor.
data StringColumnStatisticsData = StringColumnStatisticsData'
  { -- | The size of the longest string in the column.
    maximumLength :: Prelude.Natural,
    -- | The average string length in the column.
    averageLength :: Prelude.Double,
    -- | The number of null values in the column.
    numberOfNulls :: Prelude.Natural,
    -- | The number of distinct values in a column.
    numberOfDistinctValues :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Natural ->
  -- | 'averageLength'
  Prelude.Double ->
  -- | 'numberOfNulls'
  Prelude.Natural ->
  -- | 'numberOfDistinctValues'
  Prelude.Natural ->
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
stringColumnStatisticsData_maximumLength :: Lens.Lens' StringColumnStatisticsData Prelude.Natural
stringColumnStatisticsData_maximumLength = Lens.lens (\StringColumnStatisticsData' {maximumLength} -> maximumLength) (\s@StringColumnStatisticsData' {} a -> s {maximumLength = a} :: StringColumnStatisticsData)

-- | The average string length in the column.
stringColumnStatisticsData_averageLength :: Lens.Lens' StringColumnStatisticsData Prelude.Double
stringColumnStatisticsData_averageLength = Lens.lens (\StringColumnStatisticsData' {averageLength} -> averageLength) (\s@StringColumnStatisticsData' {} a -> s {averageLength = a} :: StringColumnStatisticsData)

-- | The number of null values in the column.
stringColumnStatisticsData_numberOfNulls :: Lens.Lens' StringColumnStatisticsData Prelude.Natural
stringColumnStatisticsData_numberOfNulls = Lens.lens (\StringColumnStatisticsData' {numberOfNulls} -> numberOfNulls) (\s@StringColumnStatisticsData' {} a -> s {numberOfNulls = a} :: StringColumnStatisticsData)

-- | The number of distinct values in a column.
stringColumnStatisticsData_numberOfDistinctValues :: Lens.Lens' StringColumnStatisticsData Prelude.Natural
stringColumnStatisticsData_numberOfDistinctValues = Lens.lens (\StringColumnStatisticsData' {numberOfDistinctValues} -> numberOfDistinctValues) (\s@StringColumnStatisticsData' {} a -> s {numberOfDistinctValues = a} :: StringColumnStatisticsData)

instance Prelude.FromJSON StringColumnStatisticsData where
  parseJSON =
    Prelude.withObject
      "StringColumnStatisticsData"
      ( \x ->
          StringColumnStatisticsData'
            Prelude.<$> (x Prelude..: "MaximumLength")
            Prelude.<*> (x Prelude..: "AverageLength")
            Prelude.<*> (x Prelude..: "NumberOfNulls")
            Prelude.<*> (x Prelude..: "NumberOfDistinctValues")
      )

instance Prelude.Hashable StringColumnStatisticsData

instance Prelude.NFData StringColumnStatisticsData

instance Prelude.ToJSON StringColumnStatisticsData where
  toJSON StringColumnStatisticsData' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("MaximumLength" Prelude..= maximumLength),
            Prelude.Just
              ("AverageLength" Prelude..= averageLength),
            Prelude.Just
              ("NumberOfNulls" Prelude..= numberOfNulls),
            Prelude.Just
              ( "NumberOfDistinctValues"
                  Prelude..= numberOfDistinctValues
              )
          ]
      )
