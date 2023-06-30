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
-- Module      : Amazonka.Forecast.Types.Statistics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.Statistics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides statistics for each data field imported into to an Amazon
-- Forecast dataset with the
-- <https://docs.aws.amazon.com/forecast/latest/dg/API_CreateDatasetImportJob.html CreateDatasetImportJob>
-- operation.
--
-- /See:/ 'newStatistics' smart constructor.
data Statistics = Statistics'
  { -- | For a numeric field, the average value in the field.
    avg :: Prelude.Maybe Prelude.Double,
    -- | The number of values in the field. If the response value is -1, refer to
    -- @CountLong@.
    count :: Prelude.Maybe Prelude.Int,
    -- | The number of distinct values in the field. If the response value is -1,
    -- refer to @CountDistinctLong@.
    countDistinct :: Prelude.Maybe Prelude.Int,
    -- | The number of distinct values in the field. @CountDistinctLong@ is used
    -- instead of @CountDistinct@ if the value is greater than 2,147,483,647.
    countDistinctLong :: Prelude.Maybe Prelude.Integer,
    -- | The number of values in the field. @CountLong@ is used instead of
    -- @Count@ if the value is greater than 2,147,483,647.
    countLong :: Prelude.Maybe Prelude.Integer,
    -- | The number of NAN (not a number) values in the field. If the response
    -- value is -1, refer to @CountNanLong@.
    countNan :: Prelude.Maybe Prelude.Int,
    -- | The number of NAN (not a number) values in the field. @CountNanLong@ is
    -- used instead of @CountNan@ if the value is greater than 2,147,483,647.
    countNanLong :: Prelude.Maybe Prelude.Integer,
    -- | The number of null values in the field. If the response value is -1,
    -- refer to @CountNullLong@.
    countNull :: Prelude.Maybe Prelude.Int,
    -- | The number of null values in the field. @CountNullLong@ is used instead
    -- of @CountNull@ if the value is greater than 2,147,483,647.
    countNullLong :: Prelude.Maybe Prelude.Integer,
    -- | For a numeric field, the maximum value in the field.
    max :: Prelude.Maybe Prelude.Text,
    -- | For a numeric field, the minimum value in the field.
    min :: Prelude.Maybe Prelude.Text,
    -- | For a numeric field, the standard deviation.
    stddev :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Statistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'avg', 'statistics_avg' - For a numeric field, the average value in the field.
--
-- 'count', 'statistics_count' - The number of values in the field. If the response value is -1, refer to
-- @CountLong@.
--
-- 'countDistinct', 'statistics_countDistinct' - The number of distinct values in the field. If the response value is -1,
-- refer to @CountDistinctLong@.
--
-- 'countDistinctLong', 'statistics_countDistinctLong' - The number of distinct values in the field. @CountDistinctLong@ is used
-- instead of @CountDistinct@ if the value is greater than 2,147,483,647.
--
-- 'countLong', 'statistics_countLong' - The number of values in the field. @CountLong@ is used instead of
-- @Count@ if the value is greater than 2,147,483,647.
--
-- 'countNan', 'statistics_countNan' - The number of NAN (not a number) values in the field. If the response
-- value is -1, refer to @CountNanLong@.
--
-- 'countNanLong', 'statistics_countNanLong' - The number of NAN (not a number) values in the field. @CountNanLong@ is
-- used instead of @CountNan@ if the value is greater than 2,147,483,647.
--
-- 'countNull', 'statistics_countNull' - The number of null values in the field. If the response value is -1,
-- refer to @CountNullLong@.
--
-- 'countNullLong', 'statistics_countNullLong' - The number of null values in the field. @CountNullLong@ is used instead
-- of @CountNull@ if the value is greater than 2,147,483,647.
--
-- 'max', 'statistics_max' - For a numeric field, the maximum value in the field.
--
-- 'min', 'statistics_min' - For a numeric field, the minimum value in the field.
--
-- 'stddev', 'statistics_stddev' - For a numeric field, the standard deviation.
newStatistics ::
  Statistics
newStatistics =
  Statistics'
    { avg = Prelude.Nothing,
      count = Prelude.Nothing,
      countDistinct = Prelude.Nothing,
      countDistinctLong = Prelude.Nothing,
      countLong = Prelude.Nothing,
      countNan = Prelude.Nothing,
      countNanLong = Prelude.Nothing,
      countNull = Prelude.Nothing,
      countNullLong = Prelude.Nothing,
      max = Prelude.Nothing,
      min = Prelude.Nothing,
      stddev = Prelude.Nothing
    }

-- | For a numeric field, the average value in the field.
statistics_avg :: Lens.Lens' Statistics (Prelude.Maybe Prelude.Double)
statistics_avg = Lens.lens (\Statistics' {avg} -> avg) (\s@Statistics' {} a -> s {avg = a} :: Statistics)

-- | The number of values in the field. If the response value is -1, refer to
-- @CountLong@.
statistics_count :: Lens.Lens' Statistics (Prelude.Maybe Prelude.Int)
statistics_count = Lens.lens (\Statistics' {count} -> count) (\s@Statistics' {} a -> s {count = a} :: Statistics)

-- | The number of distinct values in the field. If the response value is -1,
-- refer to @CountDistinctLong@.
statistics_countDistinct :: Lens.Lens' Statistics (Prelude.Maybe Prelude.Int)
statistics_countDistinct = Lens.lens (\Statistics' {countDistinct} -> countDistinct) (\s@Statistics' {} a -> s {countDistinct = a} :: Statistics)

-- | The number of distinct values in the field. @CountDistinctLong@ is used
-- instead of @CountDistinct@ if the value is greater than 2,147,483,647.
statistics_countDistinctLong :: Lens.Lens' Statistics (Prelude.Maybe Prelude.Integer)
statistics_countDistinctLong = Lens.lens (\Statistics' {countDistinctLong} -> countDistinctLong) (\s@Statistics' {} a -> s {countDistinctLong = a} :: Statistics)

-- | The number of values in the field. @CountLong@ is used instead of
-- @Count@ if the value is greater than 2,147,483,647.
statistics_countLong :: Lens.Lens' Statistics (Prelude.Maybe Prelude.Integer)
statistics_countLong = Lens.lens (\Statistics' {countLong} -> countLong) (\s@Statistics' {} a -> s {countLong = a} :: Statistics)

-- | The number of NAN (not a number) values in the field. If the response
-- value is -1, refer to @CountNanLong@.
statistics_countNan :: Lens.Lens' Statistics (Prelude.Maybe Prelude.Int)
statistics_countNan = Lens.lens (\Statistics' {countNan} -> countNan) (\s@Statistics' {} a -> s {countNan = a} :: Statistics)

-- | The number of NAN (not a number) values in the field. @CountNanLong@ is
-- used instead of @CountNan@ if the value is greater than 2,147,483,647.
statistics_countNanLong :: Lens.Lens' Statistics (Prelude.Maybe Prelude.Integer)
statistics_countNanLong = Lens.lens (\Statistics' {countNanLong} -> countNanLong) (\s@Statistics' {} a -> s {countNanLong = a} :: Statistics)

-- | The number of null values in the field. If the response value is -1,
-- refer to @CountNullLong@.
statistics_countNull :: Lens.Lens' Statistics (Prelude.Maybe Prelude.Int)
statistics_countNull = Lens.lens (\Statistics' {countNull} -> countNull) (\s@Statistics' {} a -> s {countNull = a} :: Statistics)

-- | The number of null values in the field. @CountNullLong@ is used instead
-- of @CountNull@ if the value is greater than 2,147,483,647.
statistics_countNullLong :: Lens.Lens' Statistics (Prelude.Maybe Prelude.Integer)
statistics_countNullLong = Lens.lens (\Statistics' {countNullLong} -> countNullLong) (\s@Statistics' {} a -> s {countNullLong = a} :: Statistics)

-- | For a numeric field, the maximum value in the field.
statistics_max :: Lens.Lens' Statistics (Prelude.Maybe Prelude.Text)
statistics_max = Lens.lens (\Statistics' {max} -> max) (\s@Statistics' {} a -> s {max = a} :: Statistics)

-- | For a numeric field, the minimum value in the field.
statistics_min :: Lens.Lens' Statistics (Prelude.Maybe Prelude.Text)
statistics_min = Lens.lens (\Statistics' {min} -> min) (\s@Statistics' {} a -> s {min = a} :: Statistics)

-- | For a numeric field, the standard deviation.
statistics_stddev :: Lens.Lens' Statistics (Prelude.Maybe Prelude.Double)
statistics_stddev = Lens.lens (\Statistics' {stddev} -> stddev) (\s@Statistics' {} a -> s {stddev = a} :: Statistics)

instance Data.FromJSON Statistics where
  parseJSON =
    Data.withObject
      "Statistics"
      ( \x ->
          Statistics'
            Prelude.<$> (x Data..:? "Avg")
            Prelude.<*> (x Data..:? "Count")
            Prelude.<*> (x Data..:? "CountDistinct")
            Prelude.<*> (x Data..:? "CountDistinctLong")
            Prelude.<*> (x Data..:? "CountLong")
            Prelude.<*> (x Data..:? "CountNan")
            Prelude.<*> (x Data..:? "CountNanLong")
            Prelude.<*> (x Data..:? "CountNull")
            Prelude.<*> (x Data..:? "CountNullLong")
            Prelude.<*> (x Data..:? "Max")
            Prelude.<*> (x Data..:? "Min")
            Prelude.<*> (x Data..:? "Stddev")
      )

instance Prelude.Hashable Statistics where
  hashWithSalt _salt Statistics' {..} =
    _salt
      `Prelude.hashWithSalt` avg
      `Prelude.hashWithSalt` count
      `Prelude.hashWithSalt` countDistinct
      `Prelude.hashWithSalt` countDistinctLong
      `Prelude.hashWithSalt` countLong
      `Prelude.hashWithSalt` countNan
      `Prelude.hashWithSalt` countNanLong
      `Prelude.hashWithSalt` countNull
      `Prelude.hashWithSalt` countNullLong
      `Prelude.hashWithSalt` max
      `Prelude.hashWithSalt` min
      `Prelude.hashWithSalt` stddev

instance Prelude.NFData Statistics where
  rnf Statistics' {..} =
    Prelude.rnf avg
      `Prelude.seq` Prelude.rnf count
      `Prelude.seq` Prelude.rnf countDistinct
      `Prelude.seq` Prelude.rnf countDistinctLong
      `Prelude.seq` Prelude.rnf countLong
      `Prelude.seq` Prelude.rnf countNan
      `Prelude.seq` Prelude.rnf countNanLong
      `Prelude.seq` Prelude.rnf countNull
      `Prelude.seq` Prelude.rnf countNullLong
      `Prelude.seq` Prelude.rnf max
      `Prelude.seq` Prelude.rnf min
      `Prelude.seq` Prelude.rnf stddev
