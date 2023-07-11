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
-- Module      : Amazonka.Glue.Types.DateColumnStatisticsData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.DateColumnStatisticsData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines column statistics supported for timestamp data columns.
--
-- /See:/ 'newDateColumnStatisticsData' smart constructor.
data DateColumnStatisticsData = DateColumnStatisticsData'
  { -- | The highest value in the column.
    maximumValue :: Prelude.Maybe Data.POSIX,
    -- | The lowest value in the column.
    minimumValue :: Prelude.Maybe Data.POSIX,
    -- | The number of null values in the column.
    numberOfNulls :: Prelude.Natural,
    -- | The number of distinct values in a column.
    numberOfDistinctValues :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Natural ->
  -- | 'numberOfDistinctValues'
  Prelude.Natural ->
  DateColumnStatisticsData
newDateColumnStatisticsData
  pNumberOfNulls_
  pNumberOfDistinctValues_ =
    DateColumnStatisticsData'
      { maximumValue =
          Prelude.Nothing,
        minimumValue = Prelude.Nothing,
        numberOfNulls = pNumberOfNulls_,
        numberOfDistinctValues = pNumberOfDistinctValues_
      }

-- | The highest value in the column.
dateColumnStatisticsData_maximumValue :: Lens.Lens' DateColumnStatisticsData (Prelude.Maybe Prelude.UTCTime)
dateColumnStatisticsData_maximumValue = Lens.lens (\DateColumnStatisticsData' {maximumValue} -> maximumValue) (\s@DateColumnStatisticsData' {} a -> s {maximumValue = a} :: DateColumnStatisticsData) Prelude.. Lens.mapping Data._Time

-- | The lowest value in the column.
dateColumnStatisticsData_minimumValue :: Lens.Lens' DateColumnStatisticsData (Prelude.Maybe Prelude.UTCTime)
dateColumnStatisticsData_minimumValue = Lens.lens (\DateColumnStatisticsData' {minimumValue} -> minimumValue) (\s@DateColumnStatisticsData' {} a -> s {minimumValue = a} :: DateColumnStatisticsData) Prelude.. Lens.mapping Data._Time

-- | The number of null values in the column.
dateColumnStatisticsData_numberOfNulls :: Lens.Lens' DateColumnStatisticsData Prelude.Natural
dateColumnStatisticsData_numberOfNulls = Lens.lens (\DateColumnStatisticsData' {numberOfNulls} -> numberOfNulls) (\s@DateColumnStatisticsData' {} a -> s {numberOfNulls = a} :: DateColumnStatisticsData)

-- | The number of distinct values in a column.
dateColumnStatisticsData_numberOfDistinctValues :: Lens.Lens' DateColumnStatisticsData Prelude.Natural
dateColumnStatisticsData_numberOfDistinctValues = Lens.lens (\DateColumnStatisticsData' {numberOfDistinctValues} -> numberOfDistinctValues) (\s@DateColumnStatisticsData' {} a -> s {numberOfDistinctValues = a} :: DateColumnStatisticsData)

instance Data.FromJSON DateColumnStatisticsData where
  parseJSON =
    Data.withObject
      "DateColumnStatisticsData"
      ( \x ->
          DateColumnStatisticsData'
            Prelude.<$> (x Data..:? "MaximumValue")
            Prelude.<*> (x Data..:? "MinimumValue")
            Prelude.<*> (x Data..: "NumberOfNulls")
            Prelude.<*> (x Data..: "NumberOfDistinctValues")
      )

instance Prelude.Hashable DateColumnStatisticsData where
  hashWithSalt _salt DateColumnStatisticsData' {..} =
    _salt
      `Prelude.hashWithSalt` maximumValue
      `Prelude.hashWithSalt` minimumValue
      `Prelude.hashWithSalt` numberOfNulls
      `Prelude.hashWithSalt` numberOfDistinctValues

instance Prelude.NFData DateColumnStatisticsData where
  rnf DateColumnStatisticsData' {..} =
    Prelude.rnf maximumValue
      `Prelude.seq` Prelude.rnf minimumValue
      `Prelude.seq` Prelude.rnf numberOfNulls
      `Prelude.seq` Prelude.rnf numberOfDistinctValues

instance Data.ToJSON DateColumnStatisticsData where
  toJSON DateColumnStatisticsData' {..} =
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
