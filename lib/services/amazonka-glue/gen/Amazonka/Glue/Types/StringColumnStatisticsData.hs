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
-- Module      : Amazonka.Glue.Types.StringColumnStatisticsData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.StringColumnStatisticsData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON StringColumnStatisticsData where
  parseJSON =
    Data.withObject
      "StringColumnStatisticsData"
      ( \x ->
          StringColumnStatisticsData'
            Prelude.<$> (x Data..: "MaximumLength")
            Prelude.<*> (x Data..: "AverageLength")
            Prelude.<*> (x Data..: "NumberOfNulls")
            Prelude.<*> (x Data..: "NumberOfDistinctValues")
      )

instance Prelude.Hashable StringColumnStatisticsData where
  hashWithSalt _salt StringColumnStatisticsData' {..} =
    _salt `Prelude.hashWithSalt` maximumLength
      `Prelude.hashWithSalt` averageLength
      `Prelude.hashWithSalt` numberOfNulls
      `Prelude.hashWithSalt` numberOfDistinctValues

instance Prelude.NFData StringColumnStatisticsData where
  rnf StringColumnStatisticsData' {..} =
    Prelude.rnf maximumLength
      `Prelude.seq` Prelude.rnf averageLength
      `Prelude.seq` Prelude.rnf numberOfNulls
      `Prelude.seq` Prelude.rnf numberOfDistinctValues

instance Data.ToJSON StringColumnStatisticsData where
  toJSON StringColumnStatisticsData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("MaximumLength" Data..= maximumLength),
            Prelude.Just ("AverageLength" Data..= averageLength),
            Prelude.Just ("NumberOfNulls" Data..= numberOfNulls),
            Prelude.Just
              ( "NumberOfDistinctValues"
                  Data..= numberOfDistinctValues
              )
          ]
      )
