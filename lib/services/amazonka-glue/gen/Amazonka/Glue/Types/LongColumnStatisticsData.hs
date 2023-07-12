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
-- Module      : Amazonka.Glue.Types.LongColumnStatisticsData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.LongColumnStatisticsData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines column statistics supported for integer data columns.
--
-- /See:/ 'newLongColumnStatisticsData' smart constructor.
data LongColumnStatisticsData = LongColumnStatisticsData'
  { -- | The highest value in the column.
    maximumValue :: Prelude.Maybe Prelude.Integer,
    -- | The lowest value in the column.
    minimumValue :: Prelude.Maybe Prelude.Integer,
    -- | The number of null values in the column.
    numberOfNulls :: Prelude.Natural,
    -- | The number of distinct values in a column.
    numberOfDistinctValues :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Natural ->
  -- | 'numberOfDistinctValues'
  Prelude.Natural ->
  LongColumnStatisticsData
newLongColumnStatisticsData
  pNumberOfNulls_
  pNumberOfDistinctValues_ =
    LongColumnStatisticsData'
      { maximumValue =
          Prelude.Nothing,
        minimumValue = Prelude.Nothing,
        numberOfNulls = pNumberOfNulls_,
        numberOfDistinctValues = pNumberOfDistinctValues_
      }

-- | The highest value in the column.
longColumnStatisticsData_maximumValue :: Lens.Lens' LongColumnStatisticsData (Prelude.Maybe Prelude.Integer)
longColumnStatisticsData_maximumValue = Lens.lens (\LongColumnStatisticsData' {maximumValue} -> maximumValue) (\s@LongColumnStatisticsData' {} a -> s {maximumValue = a} :: LongColumnStatisticsData)

-- | The lowest value in the column.
longColumnStatisticsData_minimumValue :: Lens.Lens' LongColumnStatisticsData (Prelude.Maybe Prelude.Integer)
longColumnStatisticsData_minimumValue = Lens.lens (\LongColumnStatisticsData' {minimumValue} -> minimumValue) (\s@LongColumnStatisticsData' {} a -> s {minimumValue = a} :: LongColumnStatisticsData)

-- | The number of null values in the column.
longColumnStatisticsData_numberOfNulls :: Lens.Lens' LongColumnStatisticsData Prelude.Natural
longColumnStatisticsData_numberOfNulls = Lens.lens (\LongColumnStatisticsData' {numberOfNulls} -> numberOfNulls) (\s@LongColumnStatisticsData' {} a -> s {numberOfNulls = a} :: LongColumnStatisticsData)

-- | The number of distinct values in a column.
longColumnStatisticsData_numberOfDistinctValues :: Lens.Lens' LongColumnStatisticsData Prelude.Natural
longColumnStatisticsData_numberOfDistinctValues = Lens.lens (\LongColumnStatisticsData' {numberOfDistinctValues} -> numberOfDistinctValues) (\s@LongColumnStatisticsData' {} a -> s {numberOfDistinctValues = a} :: LongColumnStatisticsData)

instance Data.FromJSON LongColumnStatisticsData where
  parseJSON =
    Data.withObject
      "LongColumnStatisticsData"
      ( \x ->
          LongColumnStatisticsData'
            Prelude.<$> (x Data..:? "MaximumValue")
            Prelude.<*> (x Data..:? "MinimumValue")
            Prelude.<*> (x Data..: "NumberOfNulls")
            Prelude.<*> (x Data..: "NumberOfDistinctValues")
      )

instance Prelude.Hashable LongColumnStatisticsData where
  hashWithSalt _salt LongColumnStatisticsData' {..} =
    _salt
      `Prelude.hashWithSalt` maximumValue
      `Prelude.hashWithSalt` minimumValue
      `Prelude.hashWithSalt` numberOfNulls
      `Prelude.hashWithSalt` numberOfDistinctValues

instance Prelude.NFData LongColumnStatisticsData where
  rnf LongColumnStatisticsData' {..} =
    Prelude.rnf maximumValue
      `Prelude.seq` Prelude.rnf minimumValue
      `Prelude.seq` Prelude.rnf numberOfNulls
      `Prelude.seq` Prelude.rnf numberOfDistinctValues

instance Data.ToJSON LongColumnStatisticsData where
  toJSON LongColumnStatisticsData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaximumValue" Data..=) Prelude.<$> maximumValue,
            ("MinimumValue" Data..=) Prelude.<$> minimumValue,
            Prelude.Just ("NumberOfNulls" Data..= numberOfNulls),
            Prelude.Just
              ( "NumberOfDistinctValues"
                  Data..= numberOfDistinctValues
              )
          ]
      )
