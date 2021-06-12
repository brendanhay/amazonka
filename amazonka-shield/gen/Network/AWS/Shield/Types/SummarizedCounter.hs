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
-- Module      : Network.AWS.Shield.Types.SummarizedCounter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.SummarizedCounter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The counter that describes a DDoS attack.
--
-- /See:/ 'newSummarizedCounter' smart constructor.
data SummarizedCounter = SummarizedCounter'
  { -- | The unit of the counters.
    unit :: Core.Maybe Core.Text,
    -- | The number of counters for a specified time period.
    n :: Core.Maybe Core.Int,
    -- | The total of counter values for a specified time period.
    sum :: Core.Maybe Core.Double,
    -- | The counter name.
    name :: Core.Maybe Core.Text,
    -- | The maximum value of the counter for a specified time period.
    max :: Core.Maybe Core.Double,
    -- | The average value of the counter for a specified time period.
    average :: Core.Maybe Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SummarizedCounter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unit', 'summarizedCounter_unit' - The unit of the counters.
--
-- 'n', 'summarizedCounter_n' - The number of counters for a specified time period.
--
-- 'sum', 'summarizedCounter_sum' - The total of counter values for a specified time period.
--
-- 'name', 'summarizedCounter_name' - The counter name.
--
-- 'max', 'summarizedCounter_max' - The maximum value of the counter for a specified time period.
--
-- 'average', 'summarizedCounter_average' - The average value of the counter for a specified time period.
newSummarizedCounter ::
  SummarizedCounter
newSummarizedCounter =
  SummarizedCounter'
    { unit = Core.Nothing,
      n = Core.Nothing,
      sum = Core.Nothing,
      name = Core.Nothing,
      max = Core.Nothing,
      average = Core.Nothing
    }

-- | The unit of the counters.
summarizedCounter_unit :: Lens.Lens' SummarizedCounter (Core.Maybe Core.Text)
summarizedCounter_unit = Lens.lens (\SummarizedCounter' {unit} -> unit) (\s@SummarizedCounter' {} a -> s {unit = a} :: SummarizedCounter)

-- | The number of counters for a specified time period.
summarizedCounter_n :: Lens.Lens' SummarizedCounter (Core.Maybe Core.Int)
summarizedCounter_n = Lens.lens (\SummarizedCounter' {n} -> n) (\s@SummarizedCounter' {} a -> s {n = a} :: SummarizedCounter)

-- | The total of counter values for a specified time period.
summarizedCounter_sum :: Lens.Lens' SummarizedCounter (Core.Maybe Core.Double)
summarizedCounter_sum = Lens.lens (\SummarizedCounter' {sum} -> sum) (\s@SummarizedCounter' {} a -> s {sum = a} :: SummarizedCounter)

-- | The counter name.
summarizedCounter_name :: Lens.Lens' SummarizedCounter (Core.Maybe Core.Text)
summarizedCounter_name = Lens.lens (\SummarizedCounter' {name} -> name) (\s@SummarizedCounter' {} a -> s {name = a} :: SummarizedCounter)

-- | The maximum value of the counter for a specified time period.
summarizedCounter_max :: Lens.Lens' SummarizedCounter (Core.Maybe Core.Double)
summarizedCounter_max = Lens.lens (\SummarizedCounter' {max} -> max) (\s@SummarizedCounter' {} a -> s {max = a} :: SummarizedCounter)

-- | The average value of the counter for a specified time period.
summarizedCounter_average :: Lens.Lens' SummarizedCounter (Core.Maybe Core.Double)
summarizedCounter_average = Lens.lens (\SummarizedCounter' {average} -> average) (\s@SummarizedCounter' {} a -> s {average = a} :: SummarizedCounter)

instance Core.FromJSON SummarizedCounter where
  parseJSON =
    Core.withObject
      "SummarizedCounter"
      ( \x ->
          SummarizedCounter'
            Core.<$> (x Core..:? "Unit")
            Core.<*> (x Core..:? "N")
            Core.<*> (x Core..:? "Sum")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Max")
            Core.<*> (x Core..:? "Average")
      )

instance Core.Hashable SummarizedCounter

instance Core.NFData SummarizedCounter
