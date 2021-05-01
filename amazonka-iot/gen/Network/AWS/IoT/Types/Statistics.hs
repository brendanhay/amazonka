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
-- Module      : Network.AWS.IoT.Types.Statistics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.Statistics where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A map of key-value pairs for all supported statistics. Currently, only
-- count is supported.
--
-- /See:/ 'newStatistics' smart constructor.
data Statistics = Statistics'
  { -- | The minimum aggregated field value.
    minimum :: Prelude.Maybe Prelude.Double,
    -- | The sum of the aggregated field values.
    sum :: Prelude.Maybe Prelude.Double,
    -- | The standard deviation of the aggregated field values.
    stdDeviation :: Prelude.Maybe Prelude.Double,
    -- | The variance of the aggregated field values.
    variance :: Prelude.Maybe Prelude.Double,
    -- | The average of the aggregated field values.
    average :: Prelude.Maybe Prelude.Double,
    -- | The count of things that match the query.
    count :: Prelude.Maybe Prelude.Int,
    -- | The maximum aggregated field value.
    maximum :: Prelude.Maybe Prelude.Double,
    -- | The sum of the squares of the aggregated field values.
    sumOfSquares :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Statistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minimum', 'statistics_minimum' - The minimum aggregated field value.
--
-- 'sum', 'statistics_sum' - The sum of the aggregated field values.
--
-- 'stdDeviation', 'statistics_stdDeviation' - The standard deviation of the aggregated field values.
--
-- 'variance', 'statistics_variance' - The variance of the aggregated field values.
--
-- 'average', 'statistics_average' - The average of the aggregated field values.
--
-- 'count', 'statistics_count' - The count of things that match the query.
--
-- 'maximum', 'statistics_maximum' - The maximum aggregated field value.
--
-- 'sumOfSquares', 'statistics_sumOfSquares' - The sum of the squares of the aggregated field values.
newStatistics ::
  Statistics
newStatistics =
  Statistics'
    { minimum = Prelude.Nothing,
      sum = Prelude.Nothing,
      stdDeviation = Prelude.Nothing,
      variance = Prelude.Nothing,
      average = Prelude.Nothing,
      count = Prelude.Nothing,
      maximum = Prelude.Nothing,
      sumOfSquares = Prelude.Nothing
    }

-- | The minimum aggregated field value.
statistics_minimum :: Lens.Lens' Statistics (Prelude.Maybe Prelude.Double)
statistics_minimum = Lens.lens (\Statistics' {minimum} -> minimum) (\s@Statistics' {} a -> s {minimum = a} :: Statistics)

-- | The sum of the aggregated field values.
statistics_sum :: Lens.Lens' Statistics (Prelude.Maybe Prelude.Double)
statistics_sum = Lens.lens (\Statistics' {sum} -> sum) (\s@Statistics' {} a -> s {sum = a} :: Statistics)

-- | The standard deviation of the aggregated field values.
statistics_stdDeviation :: Lens.Lens' Statistics (Prelude.Maybe Prelude.Double)
statistics_stdDeviation = Lens.lens (\Statistics' {stdDeviation} -> stdDeviation) (\s@Statistics' {} a -> s {stdDeviation = a} :: Statistics)

-- | The variance of the aggregated field values.
statistics_variance :: Lens.Lens' Statistics (Prelude.Maybe Prelude.Double)
statistics_variance = Lens.lens (\Statistics' {variance} -> variance) (\s@Statistics' {} a -> s {variance = a} :: Statistics)

-- | The average of the aggregated field values.
statistics_average :: Lens.Lens' Statistics (Prelude.Maybe Prelude.Double)
statistics_average = Lens.lens (\Statistics' {average} -> average) (\s@Statistics' {} a -> s {average = a} :: Statistics)

-- | The count of things that match the query.
statistics_count :: Lens.Lens' Statistics (Prelude.Maybe Prelude.Int)
statistics_count = Lens.lens (\Statistics' {count} -> count) (\s@Statistics' {} a -> s {count = a} :: Statistics)

-- | The maximum aggregated field value.
statistics_maximum :: Lens.Lens' Statistics (Prelude.Maybe Prelude.Double)
statistics_maximum = Lens.lens (\Statistics' {maximum} -> maximum) (\s@Statistics' {} a -> s {maximum = a} :: Statistics)

-- | The sum of the squares of the aggregated field values.
statistics_sumOfSquares :: Lens.Lens' Statistics (Prelude.Maybe Prelude.Double)
statistics_sumOfSquares = Lens.lens (\Statistics' {sumOfSquares} -> sumOfSquares) (\s@Statistics' {} a -> s {sumOfSquares = a} :: Statistics)

instance Prelude.FromJSON Statistics where
  parseJSON =
    Prelude.withObject
      "Statistics"
      ( \x ->
          Statistics'
            Prelude.<$> (x Prelude..:? "minimum")
            Prelude.<*> (x Prelude..:? "sum")
            Prelude.<*> (x Prelude..:? "stdDeviation")
            Prelude.<*> (x Prelude..:? "variance")
            Prelude.<*> (x Prelude..:? "average")
            Prelude.<*> (x Prelude..:? "count")
            Prelude.<*> (x Prelude..:? "maximum")
            Prelude.<*> (x Prelude..:? "sumOfSquares")
      )

instance Prelude.Hashable Statistics

instance Prelude.NFData Statistics
