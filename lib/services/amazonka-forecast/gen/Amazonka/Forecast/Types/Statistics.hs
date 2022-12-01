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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.Statistics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides statistics for each data field imported into to an Amazon
-- Forecast dataset with the
-- <https://docs.aws.amazon.com/forecast/latest/dg/API_CreateDatasetImportJob.html CreateDatasetImportJob>
-- operation.
--
-- /See:/ 'newStatistics' smart constructor.
data Statistics = Statistics'
  { -- | The number of NAN (not a number) values in the field. @CountNanLong@ is
    -- used instead of @CountNan@ if the value is greater than 2,147,483,647.
    countNanLong :: Prelude.Maybe Prelude.Integer,
    -- | The number of null values in the field. @CountNullLong@ is used instead
    -- of @CountNull@ if the value is greater than 2,147,483,647.
    countNullLong :: Prelude.Maybe Prelude.Integer,
    -- | The number of null values in the field. If the response value is -1,
    -- refer to @CountNullLong@.
    countNull :: Prelude.Maybe Prelude.Int,
    -- | For a numeric field, the maximum value in the field.
    max :: Prelude.Maybe Prelude.Text,
    -- | The number of values in the field. @CountLong@ is used instead of
    -- @Count@ if the value is greater than 2,147,483,647.
    countLong :: Prelude.Maybe Prelude.Integer,
    -- | The number of distinct values in the field. @CountDistinctLong@ is used
    -- instead of @CountDistinct@ if the value is greater than 2,147,483,647.
    countDistinctLong :: Prelude.Maybe Prelude.Integer,
    -- | For a numeric field, the average value in the field.
    avg :: Prelude.Maybe Prelude.Double,
    -- | The number of values in the field. If the response value is -1, refer to
    -- @CountLong@.
    count :: Prelude.Maybe Prelude.Int,
    -- | For a numeric field, the minimum value in the field.
    min :: Prelude.Maybe Prelude.Text,
    -- | The number of NAN (not a number) values in the field. If the response
    -- value is -1, refer to @CountNanLong@.
    countNan :: Prelude.Maybe Prelude.Int,
    -- | For a numeric field, the standard deviation.
    stddev :: Prelude.Maybe Prelude.Double,
    -- | The number of distinct values in the field. If the response value is -1,
    -- refer to @CountDistinctLong@.
    countDistinct :: Prelude.Maybe Prelude.Int
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
-- 'countNanLong', 'statistics_countNanLong' - The number of NAN (not a number) values in the field. @CountNanLong@ is
-- used instead of @CountNan@ if the value is greater than 2,147,483,647.
--
-- 'countNullLong', 'statistics_countNullLong' - The number of null values in the field. @CountNullLong@ is used instead
-- of @CountNull@ if the value is greater than 2,147,483,647.
--
-- 'countNull', 'statistics_countNull' - The number of null values in the field. If the response value is -1,
-- refer to @CountNullLong@.
--
-- 'max', 'statistics_max' - For a numeric field, the maximum value in the field.
--
-- 'countLong', 'statistics_countLong' - The number of values in the field. @CountLong@ is used instead of
-- @Count@ if the value is greater than 2,147,483,647.
--
-- 'countDistinctLong', 'statistics_countDistinctLong' - The number of distinct values in the field. @CountDistinctLong@ is used
-- instead of @CountDistinct@ if the value is greater than 2,147,483,647.
--
-- 'avg', 'statistics_avg' - For a numeric field, the average value in the field.
--
-- 'count', 'statistics_count' - The number of values in the field. If the response value is -1, refer to
-- @CountLong@.
--
-- 'min', 'statistics_min' - For a numeric field, the minimum value in the field.
--
-- 'countNan', 'statistics_countNan' - The number of NAN (not a number) values in the field. If the response
-- value is -1, refer to @CountNanLong@.
--
-- 'stddev', 'statistics_stddev' - For a numeric field, the standard deviation.
--
-- 'countDistinct', 'statistics_countDistinct' - The number of distinct values in the field. If the response value is -1,
-- refer to @CountDistinctLong@.
newStatistics ::
  Statistics
newStatistics =
  Statistics'
    { countNanLong = Prelude.Nothing,
      countNullLong = Prelude.Nothing,
      countNull = Prelude.Nothing,
      max = Prelude.Nothing,
      countLong = Prelude.Nothing,
      countDistinctLong = Prelude.Nothing,
      avg = Prelude.Nothing,
      count = Prelude.Nothing,
      min = Prelude.Nothing,
      countNan = Prelude.Nothing,
      stddev = Prelude.Nothing,
      countDistinct = Prelude.Nothing
    }

-- | The number of NAN (not a number) values in the field. @CountNanLong@ is
-- used instead of @CountNan@ if the value is greater than 2,147,483,647.
statistics_countNanLong :: Lens.Lens' Statistics (Prelude.Maybe Prelude.Integer)
statistics_countNanLong = Lens.lens (\Statistics' {countNanLong} -> countNanLong) (\s@Statistics' {} a -> s {countNanLong = a} :: Statistics)

-- | The number of null values in the field. @CountNullLong@ is used instead
-- of @CountNull@ if the value is greater than 2,147,483,647.
statistics_countNullLong :: Lens.Lens' Statistics (Prelude.Maybe Prelude.Integer)
statistics_countNullLong = Lens.lens (\Statistics' {countNullLong} -> countNullLong) (\s@Statistics' {} a -> s {countNullLong = a} :: Statistics)

-- | The number of null values in the field. If the response value is -1,
-- refer to @CountNullLong@.
statistics_countNull :: Lens.Lens' Statistics (Prelude.Maybe Prelude.Int)
statistics_countNull = Lens.lens (\Statistics' {countNull} -> countNull) (\s@Statistics' {} a -> s {countNull = a} :: Statistics)

-- | For a numeric field, the maximum value in the field.
statistics_max :: Lens.Lens' Statistics (Prelude.Maybe Prelude.Text)
statistics_max = Lens.lens (\Statistics' {max} -> max) (\s@Statistics' {} a -> s {max = a} :: Statistics)

-- | The number of values in the field. @CountLong@ is used instead of
-- @Count@ if the value is greater than 2,147,483,647.
statistics_countLong :: Lens.Lens' Statistics (Prelude.Maybe Prelude.Integer)
statistics_countLong = Lens.lens (\Statistics' {countLong} -> countLong) (\s@Statistics' {} a -> s {countLong = a} :: Statistics)

-- | The number of distinct values in the field. @CountDistinctLong@ is used
-- instead of @CountDistinct@ if the value is greater than 2,147,483,647.
statistics_countDistinctLong :: Lens.Lens' Statistics (Prelude.Maybe Prelude.Integer)
statistics_countDistinctLong = Lens.lens (\Statistics' {countDistinctLong} -> countDistinctLong) (\s@Statistics' {} a -> s {countDistinctLong = a} :: Statistics)

-- | For a numeric field, the average value in the field.
statistics_avg :: Lens.Lens' Statistics (Prelude.Maybe Prelude.Double)
statistics_avg = Lens.lens (\Statistics' {avg} -> avg) (\s@Statistics' {} a -> s {avg = a} :: Statistics)

-- | The number of values in the field. If the response value is -1, refer to
-- @CountLong@.
statistics_count :: Lens.Lens' Statistics (Prelude.Maybe Prelude.Int)
statistics_count = Lens.lens (\Statistics' {count} -> count) (\s@Statistics' {} a -> s {count = a} :: Statistics)

-- | For a numeric field, the minimum value in the field.
statistics_min :: Lens.Lens' Statistics (Prelude.Maybe Prelude.Text)
statistics_min = Lens.lens (\Statistics' {min} -> min) (\s@Statistics' {} a -> s {min = a} :: Statistics)

-- | The number of NAN (not a number) values in the field. If the response
-- value is -1, refer to @CountNanLong@.
statistics_countNan :: Lens.Lens' Statistics (Prelude.Maybe Prelude.Int)
statistics_countNan = Lens.lens (\Statistics' {countNan} -> countNan) (\s@Statistics' {} a -> s {countNan = a} :: Statistics)

-- | For a numeric field, the standard deviation.
statistics_stddev :: Lens.Lens' Statistics (Prelude.Maybe Prelude.Double)
statistics_stddev = Lens.lens (\Statistics' {stddev} -> stddev) (\s@Statistics' {} a -> s {stddev = a} :: Statistics)

-- | The number of distinct values in the field. If the response value is -1,
-- refer to @CountDistinctLong@.
statistics_countDistinct :: Lens.Lens' Statistics (Prelude.Maybe Prelude.Int)
statistics_countDistinct = Lens.lens (\Statistics' {countDistinct} -> countDistinct) (\s@Statistics' {} a -> s {countDistinct = a} :: Statistics)

instance Core.FromJSON Statistics where
  parseJSON =
    Core.withObject
      "Statistics"
      ( \x ->
          Statistics'
            Prelude.<$> (x Core..:? "CountNanLong")
            Prelude.<*> (x Core..:? "CountNullLong")
            Prelude.<*> (x Core..:? "CountNull")
            Prelude.<*> (x Core..:? "Max")
            Prelude.<*> (x Core..:? "CountLong")
            Prelude.<*> (x Core..:? "CountDistinctLong")
            Prelude.<*> (x Core..:? "Avg")
            Prelude.<*> (x Core..:? "Count")
            Prelude.<*> (x Core..:? "Min")
            Prelude.<*> (x Core..:? "CountNan")
            Prelude.<*> (x Core..:? "Stddev")
            Prelude.<*> (x Core..:? "CountDistinct")
      )

instance Prelude.Hashable Statistics where
  hashWithSalt _salt Statistics' {..} =
    _salt `Prelude.hashWithSalt` countNanLong
      `Prelude.hashWithSalt` countNullLong
      `Prelude.hashWithSalt` countNull
      `Prelude.hashWithSalt` max
      `Prelude.hashWithSalt` countLong
      `Prelude.hashWithSalt` countDistinctLong
      `Prelude.hashWithSalt` avg
      `Prelude.hashWithSalt` count
      `Prelude.hashWithSalt` min
      `Prelude.hashWithSalt` countNan
      `Prelude.hashWithSalt` stddev
      `Prelude.hashWithSalt` countDistinct

instance Prelude.NFData Statistics where
  rnf Statistics' {..} =
    Prelude.rnf countNanLong
      `Prelude.seq` Prelude.rnf countNullLong
      `Prelude.seq` Prelude.rnf countNull
      `Prelude.seq` Prelude.rnf max
      `Prelude.seq` Prelude.rnf countLong
      `Prelude.seq` Prelude.rnf countDistinctLong
      `Prelude.seq` Prelude.rnf avg
      `Prelude.seq` Prelude.rnf count
      `Prelude.seq` Prelude.rnf min
      `Prelude.seq` Prelude.rnf countNan
      `Prelude.seq` Prelude.rnf stddev
      `Prelude.seq` Prelude.rnf countDistinct
