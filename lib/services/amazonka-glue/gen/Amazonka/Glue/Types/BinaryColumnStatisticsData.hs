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
-- Module      : Amazonka.Glue.Types.BinaryColumnStatisticsData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.BinaryColumnStatisticsData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON BinaryColumnStatisticsData where
  parseJSON =
    Data.withObject
      "BinaryColumnStatisticsData"
      ( \x ->
          BinaryColumnStatisticsData'
            Prelude.<$> (x Data..: "MaximumLength")
            Prelude.<*> (x Data..: "AverageLength")
            Prelude.<*> (x Data..: "NumberOfNulls")
      )

instance Prelude.Hashable BinaryColumnStatisticsData where
  hashWithSalt _salt BinaryColumnStatisticsData' {..} =
    _salt
      `Prelude.hashWithSalt` maximumLength
      `Prelude.hashWithSalt` averageLength
      `Prelude.hashWithSalt` numberOfNulls

instance Prelude.NFData BinaryColumnStatisticsData where
  rnf BinaryColumnStatisticsData' {..} =
    Prelude.rnf maximumLength `Prelude.seq`
      Prelude.rnf averageLength `Prelude.seq`
        Prelude.rnf numberOfNulls

instance Data.ToJSON BinaryColumnStatisticsData where
  toJSON BinaryColumnStatisticsData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("MaximumLength" Data..= maximumLength),
            Prelude.Just ("AverageLength" Data..= averageLength),
            Prelude.Just
              ("NumberOfNulls" Data..= numberOfNulls)
          ]
      )
