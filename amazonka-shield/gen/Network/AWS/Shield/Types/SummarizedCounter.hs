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
-- Module      : Network.AWS.Shield.Types.SummarizedCounter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.SummarizedCounter where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The counter that describes a DDoS attack.
--
-- /See:/ 'newSummarizedCounter' smart constructor.
data SummarizedCounter = SummarizedCounter'
  { -- | The unit of the counters.
    unit :: Prelude.Maybe Prelude.Text,
    -- | The number of counters for a specified time period.
    n :: Prelude.Maybe Prelude.Int,
    -- | The total of counter values for a specified time period.
    sum :: Prelude.Maybe Prelude.Double,
    -- | The counter name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The maximum value of the counter for a specified time period.
    max :: Prelude.Maybe Prelude.Double,
    -- | The average value of the counter for a specified time period.
    average :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { unit = Prelude.Nothing,
      n = Prelude.Nothing,
      sum = Prelude.Nothing,
      name = Prelude.Nothing,
      max = Prelude.Nothing,
      average = Prelude.Nothing
    }

-- | The unit of the counters.
summarizedCounter_unit :: Lens.Lens' SummarizedCounter (Prelude.Maybe Prelude.Text)
summarizedCounter_unit = Lens.lens (\SummarizedCounter' {unit} -> unit) (\s@SummarizedCounter' {} a -> s {unit = a} :: SummarizedCounter)

-- | The number of counters for a specified time period.
summarizedCounter_n :: Lens.Lens' SummarizedCounter (Prelude.Maybe Prelude.Int)
summarizedCounter_n = Lens.lens (\SummarizedCounter' {n} -> n) (\s@SummarizedCounter' {} a -> s {n = a} :: SummarizedCounter)

-- | The total of counter values for a specified time period.
summarizedCounter_sum :: Lens.Lens' SummarizedCounter (Prelude.Maybe Prelude.Double)
summarizedCounter_sum = Lens.lens (\SummarizedCounter' {sum} -> sum) (\s@SummarizedCounter' {} a -> s {sum = a} :: SummarizedCounter)

-- | The counter name.
summarizedCounter_name :: Lens.Lens' SummarizedCounter (Prelude.Maybe Prelude.Text)
summarizedCounter_name = Lens.lens (\SummarizedCounter' {name} -> name) (\s@SummarizedCounter' {} a -> s {name = a} :: SummarizedCounter)

-- | The maximum value of the counter for a specified time period.
summarizedCounter_max :: Lens.Lens' SummarizedCounter (Prelude.Maybe Prelude.Double)
summarizedCounter_max = Lens.lens (\SummarizedCounter' {max} -> max) (\s@SummarizedCounter' {} a -> s {max = a} :: SummarizedCounter)

-- | The average value of the counter for a specified time period.
summarizedCounter_average :: Lens.Lens' SummarizedCounter (Prelude.Maybe Prelude.Double)
summarizedCounter_average = Lens.lens (\SummarizedCounter' {average} -> average) (\s@SummarizedCounter' {} a -> s {average = a} :: SummarizedCounter)

instance Prelude.FromJSON SummarizedCounter where
  parseJSON =
    Prelude.withObject
      "SummarizedCounter"
      ( \x ->
          SummarizedCounter'
            Prelude.<$> (x Prelude..:? "Unit")
            Prelude.<*> (x Prelude..:? "N")
            Prelude.<*> (x Prelude..:? "Sum")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "Max")
            Prelude.<*> (x Prelude..:? "Average")
      )

instance Prelude.Hashable SummarizedCounter

instance Prelude.NFData SummarizedCounter
