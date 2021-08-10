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
-- Module      : Network.AWS.Glue.Types.LongColumnStatisticsData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.LongColumnStatisticsData where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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

instance Core.FromJSON LongColumnStatisticsData where
  parseJSON =
    Core.withObject
      "LongColumnStatisticsData"
      ( \x ->
          LongColumnStatisticsData'
            Prelude.<$> (x Core..:? "MaximumValue")
            Prelude.<*> (x Core..:? "MinimumValue")
            Prelude.<*> (x Core..: "NumberOfNulls")
            Prelude.<*> (x Core..: "NumberOfDistinctValues")
      )

instance Prelude.Hashable LongColumnStatisticsData

instance Prelude.NFData LongColumnStatisticsData

instance Core.ToJSON LongColumnStatisticsData where
  toJSON LongColumnStatisticsData' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MaximumValue" Core..=) Prelude.<$> maximumValue,
            ("MinimumValue" Core..=) Prelude.<$> minimumValue,
            Prelude.Just ("NumberOfNulls" Core..= numberOfNulls),
            Prelude.Just
              ( "NumberOfDistinctValues"
                  Core..= numberOfDistinctValues
              )
          ]
      )
