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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A map of key-value pairs for all supported statistics. Currently, only
-- count is supported.
--
-- /See:/ 'newStatistics' smart constructor.
data Statistics = Statistics'
  { -- | The minimum aggregated field value.
    minimum :: Core.Maybe Core.Double,
    -- | The sum of the aggregated field values.
    sum :: Core.Maybe Core.Double,
    -- | The standard deviation of the aggregated field values.
    stdDeviation :: Core.Maybe Core.Double,
    -- | The variance of the aggregated field values.
    variance :: Core.Maybe Core.Double,
    -- | The average of the aggregated field values.
    average :: Core.Maybe Core.Double,
    -- | The count of things that match the query.
    count :: Core.Maybe Core.Int,
    -- | The maximum aggregated field value.
    maximum :: Core.Maybe Core.Double,
    -- | The sum of the squares of the aggregated field values.
    sumOfSquares :: Core.Maybe Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { minimum = Core.Nothing,
      sum = Core.Nothing,
      stdDeviation = Core.Nothing,
      variance = Core.Nothing,
      average = Core.Nothing,
      count = Core.Nothing,
      maximum = Core.Nothing,
      sumOfSquares = Core.Nothing
    }

-- | The minimum aggregated field value.
statistics_minimum :: Lens.Lens' Statistics (Core.Maybe Core.Double)
statistics_minimum = Lens.lens (\Statistics' {minimum} -> minimum) (\s@Statistics' {} a -> s {minimum = a} :: Statistics)

-- | The sum of the aggregated field values.
statistics_sum :: Lens.Lens' Statistics (Core.Maybe Core.Double)
statistics_sum = Lens.lens (\Statistics' {sum} -> sum) (\s@Statistics' {} a -> s {sum = a} :: Statistics)

-- | The standard deviation of the aggregated field values.
statistics_stdDeviation :: Lens.Lens' Statistics (Core.Maybe Core.Double)
statistics_stdDeviation = Lens.lens (\Statistics' {stdDeviation} -> stdDeviation) (\s@Statistics' {} a -> s {stdDeviation = a} :: Statistics)

-- | The variance of the aggregated field values.
statistics_variance :: Lens.Lens' Statistics (Core.Maybe Core.Double)
statistics_variance = Lens.lens (\Statistics' {variance} -> variance) (\s@Statistics' {} a -> s {variance = a} :: Statistics)

-- | The average of the aggregated field values.
statistics_average :: Lens.Lens' Statistics (Core.Maybe Core.Double)
statistics_average = Lens.lens (\Statistics' {average} -> average) (\s@Statistics' {} a -> s {average = a} :: Statistics)

-- | The count of things that match the query.
statistics_count :: Lens.Lens' Statistics (Core.Maybe Core.Int)
statistics_count = Lens.lens (\Statistics' {count} -> count) (\s@Statistics' {} a -> s {count = a} :: Statistics)

-- | The maximum aggregated field value.
statistics_maximum :: Lens.Lens' Statistics (Core.Maybe Core.Double)
statistics_maximum = Lens.lens (\Statistics' {maximum} -> maximum) (\s@Statistics' {} a -> s {maximum = a} :: Statistics)

-- | The sum of the squares of the aggregated field values.
statistics_sumOfSquares :: Lens.Lens' Statistics (Core.Maybe Core.Double)
statistics_sumOfSquares = Lens.lens (\Statistics' {sumOfSquares} -> sumOfSquares) (\s@Statistics' {} a -> s {sumOfSquares = a} :: Statistics)

instance Core.FromJSON Statistics where
  parseJSON =
    Core.withObject
      "Statistics"
      ( \x ->
          Statistics'
            Core.<$> (x Core..:? "minimum")
            Core.<*> (x Core..:? "sum")
            Core.<*> (x Core..:? "stdDeviation")
            Core.<*> (x Core..:? "variance")
            Core.<*> (x Core..:? "average")
            Core.<*> (x Core..:? "count")
            Core.<*> (x Core..:? "maximum")
            Core.<*> (x Core..:? "sumOfSquares")
      )

instance Core.Hashable Statistics

instance Core.NFData Statistics
