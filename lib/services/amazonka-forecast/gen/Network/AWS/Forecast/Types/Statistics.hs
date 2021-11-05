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
-- Module      : Network.AWS.Forecast.Types.Statistics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Forecast.Types.Statistics where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides statistics for each data field imported into to an Amazon
-- Forecast dataset with the CreateDatasetImportJob operation.
--
-- /See:/ 'newStatistics' smart constructor.
data Statistics = Statistics'
  { -- | For a numeric field, the maximum value in the field.
    max :: Prelude.Maybe Prelude.Text,
    -- | The number of null values in the field. @CountNullLong@ is used instead
    -- of @CountNull@ if the value is greater than 2,147,483,647.
    countNullLong :: Prelude.Maybe Prelude.Integer,
    -- | The number of NAN (not a number) values in the field. If the response
    -- value is -1, refer to @CountNanLong@.
    countNan :: Prelude.Maybe Prelude.Int,
    -- | The number of NAN (not a number) values in the field. @CountNanLong@ is
    -- used instead of @CountNan@ if the value is greater than 2,147,483,647.
    countNanLong :: Prelude.Maybe Prelude.Integer,
    -- | For a numeric field, the average value in the field.
    avg :: Prelude.Maybe Prelude.Double,
    -- | The number of null values in the field. If the response value is -1,
    -- refer to @CountNullLong@.
    countNull :: Prelude.Maybe Prelude.Int,
    -- | The number of values in the field. If the response value is -1, refer to
    -- @CountLong@.
    count :: Prelude.Maybe Prelude.Int,
    -- | The number of values in the field. @CountLong@ is used instead of
    -- @Count@ if the value is greater than 2,147,483,647.
    countLong :: Prelude.Maybe Prelude.Integer,
    -- | For a numeric field, the standard deviation.
    stddev :: Prelude.Maybe Prelude.Double,
    -- | For a numeric field, the minimum value in the field.
    min :: Prelude.Maybe Prelude.Text,
    -- | The number of distinct values in the field. @CountDistinctLong@ is used
    -- instead of @CountDistinct@ if the value is greater than 2,147,483,647.
    countDistinctLong :: Prelude.Maybe Prelude.Integer,
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
-- 'max', 'statistics_max' - For a numeric field, the maximum value in the field.
--
-- 'countNullLong', 'statistics_countNullLong' - The number of null values in the field. @CountNullLong@ is used instead
-- of @CountNull@ if the value is greater than 2,147,483,647.
--
-- 'countNan', 'statistics_countNan' - The number of NAN (not a number) values in the field. If the response
-- value is -1, refer to @CountNanLong@.
--
-- 'countNanLong', 'statistics_countNanLong' - The number of NAN (not a number) values in the field. @CountNanLong@ is
-- used instead of @CountNan@ if the value is greater than 2,147,483,647.
--
-- 'avg', 'statistics_avg' - For a numeric field, the average value in the field.
--
-- 'countNull', 'statistics_countNull' - The number of null values in the field. If the response value is -1,
-- refer to @CountNullLong@.
--
-- 'count', 'statistics_count' - The number of values in the field. If the response value is -1, refer to
-- @CountLong@.
--
-- 'countLong', 'statistics_countLong' - The number of values in the field. @CountLong@ is used instead of
-- @Count@ if the value is greater than 2,147,483,647.
--
-- 'stddev', 'statistics_stddev' - For a numeric field, the standard deviation.
--
-- 'min', 'statistics_min' - For a numeric field, the minimum value in the field.
--
-- 'countDistinctLong', 'statistics_countDistinctLong' - The number of distinct values in the field. @CountDistinctLong@ is used
-- instead of @CountDistinct@ if the value is greater than 2,147,483,647.
--
-- 'countDistinct', 'statistics_countDistinct' - The number of distinct values in the field. If the response value is -1,
-- refer to @CountDistinctLong@.
newStatistics ::
  Statistics
newStatistics =
  Statistics'
    { max = Prelude.Nothing,
      countNullLong = Prelude.Nothing,
      countNan = Prelude.Nothing,
      countNanLong = Prelude.Nothing,
      avg = Prelude.Nothing,
      countNull = Prelude.Nothing,
      count = Prelude.Nothing,
      countLong = Prelude.Nothing,
      stddev = Prelude.Nothing,
      min = Prelude.Nothing,
      countDistinctLong = Prelude.Nothing,
      countDistinct = Prelude.Nothing
    }

-- | For a numeric field, the maximum value in the field.
statistics_max :: Lens.Lens' Statistics (Prelude.Maybe Prelude.Text)
statistics_max = Lens.lens (\Statistics' {max} -> max) (\s@Statistics' {} a -> s {max = a} :: Statistics)

-- | The number of null values in the field. @CountNullLong@ is used instead
-- of @CountNull@ if the value is greater than 2,147,483,647.
statistics_countNullLong :: Lens.Lens' Statistics (Prelude.Maybe Prelude.Integer)
statistics_countNullLong = Lens.lens (\Statistics' {countNullLong} -> countNullLong) (\s@Statistics' {} a -> s {countNullLong = a} :: Statistics)

-- | The number of NAN (not a number) values in the field. If the response
-- value is -1, refer to @CountNanLong@.
statistics_countNan :: Lens.Lens' Statistics (Prelude.Maybe Prelude.Int)
statistics_countNan = Lens.lens (\Statistics' {countNan} -> countNan) (\s@Statistics' {} a -> s {countNan = a} :: Statistics)

-- | The number of NAN (not a number) values in the field. @CountNanLong@ is
-- used instead of @CountNan@ if the value is greater than 2,147,483,647.
statistics_countNanLong :: Lens.Lens' Statistics (Prelude.Maybe Prelude.Integer)
statistics_countNanLong = Lens.lens (\Statistics' {countNanLong} -> countNanLong) (\s@Statistics' {} a -> s {countNanLong = a} :: Statistics)

-- | For a numeric field, the average value in the field.
statistics_avg :: Lens.Lens' Statistics (Prelude.Maybe Prelude.Double)
statistics_avg = Lens.lens (\Statistics' {avg} -> avg) (\s@Statistics' {} a -> s {avg = a} :: Statistics)

-- | The number of null values in the field. If the response value is -1,
-- refer to @CountNullLong@.
statistics_countNull :: Lens.Lens' Statistics (Prelude.Maybe Prelude.Int)
statistics_countNull = Lens.lens (\Statistics' {countNull} -> countNull) (\s@Statistics' {} a -> s {countNull = a} :: Statistics)

-- | The number of values in the field. If the response value is -1, refer to
-- @CountLong@.
statistics_count :: Lens.Lens' Statistics (Prelude.Maybe Prelude.Int)
statistics_count = Lens.lens (\Statistics' {count} -> count) (\s@Statistics' {} a -> s {count = a} :: Statistics)

-- | The number of values in the field. @CountLong@ is used instead of
-- @Count@ if the value is greater than 2,147,483,647.
statistics_countLong :: Lens.Lens' Statistics (Prelude.Maybe Prelude.Integer)
statistics_countLong = Lens.lens (\Statistics' {countLong} -> countLong) (\s@Statistics' {} a -> s {countLong = a} :: Statistics)

-- | For a numeric field, the standard deviation.
statistics_stddev :: Lens.Lens' Statistics (Prelude.Maybe Prelude.Double)
statistics_stddev = Lens.lens (\Statistics' {stddev} -> stddev) (\s@Statistics' {} a -> s {stddev = a} :: Statistics)

-- | For a numeric field, the minimum value in the field.
statistics_min :: Lens.Lens' Statistics (Prelude.Maybe Prelude.Text)
statistics_min = Lens.lens (\Statistics' {min} -> min) (\s@Statistics' {} a -> s {min = a} :: Statistics)

-- | The number of distinct values in the field. @CountDistinctLong@ is used
-- instead of @CountDistinct@ if the value is greater than 2,147,483,647.
statistics_countDistinctLong :: Lens.Lens' Statistics (Prelude.Maybe Prelude.Integer)
statistics_countDistinctLong = Lens.lens (\Statistics' {countDistinctLong} -> countDistinctLong) (\s@Statistics' {} a -> s {countDistinctLong = a} :: Statistics)

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
            Prelude.<$> (x Core..:? "Max")
            Prelude.<*> (x Core..:? "CountNullLong")
            Prelude.<*> (x Core..:? "CountNan")
            Prelude.<*> (x Core..:? "CountNanLong")
            Prelude.<*> (x Core..:? "Avg")
            Prelude.<*> (x Core..:? "CountNull")
            Prelude.<*> (x Core..:? "Count")
            Prelude.<*> (x Core..:? "CountLong")
            Prelude.<*> (x Core..:? "Stddev")
            Prelude.<*> (x Core..:? "Min")
            Prelude.<*> (x Core..:? "CountDistinctLong")
            Prelude.<*> (x Core..:? "CountDistinct")
      )

instance Prelude.Hashable Statistics

instance Prelude.NFData Statistics
