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
-- Module      : Amazonka.Shield.Types.SummarizedCounter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Shield.Types.SummarizedCounter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The counter that describes a DDoS attack.
--
-- /See:/ 'newSummarizedCounter' smart constructor.
data SummarizedCounter = SummarizedCounter'
  { -- | The average value of the counter for a specified time period.
    average :: Prelude.Maybe Prelude.Double,
    -- | The maximum value of the counter for a specified time period.
    max :: Prelude.Maybe Prelude.Double,
    -- | The number of counters for a specified time period.
    n :: Prelude.Maybe Prelude.Int,
    -- | The counter name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The total of counter values for a specified time period.
    sum :: Prelude.Maybe Prelude.Double,
    -- | The unit of the counters.
    unit :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SummarizedCounter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'average', 'summarizedCounter_average' - The average value of the counter for a specified time period.
--
-- 'max', 'summarizedCounter_max' - The maximum value of the counter for a specified time period.
--
-- 'n', 'summarizedCounter_n' - The number of counters for a specified time period.
--
-- 'name', 'summarizedCounter_name' - The counter name.
--
-- 'sum', 'summarizedCounter_sum' - The total of counter values for a specified time period.
--
-- 'unit', 'summarizedCounter_unit' - The unit of the counters.
newSummarizedCounter ::
  SummarizedCounter
newSummarizedCounter =
  SummarizedCounter'
    { average = Prelude.Nothing,
      max = Prelude.Nothing,
      n = Prelude.Nothing,
      name = Prelude.Nothing,
      sum = Prelude.Nothing,
      unit = Prelude.Nothing
    }

-- | The average value of the counter for a specified time period.
summarizedCounter_average :: Lens.Lens' SummarizedCounter (Prelude.Maybe Prelude.Double)
summarizedCounter_average = Lens.lens (\SummarizedCounter' {average} -> average) (\s@SummarizedCounter' {} a -> s {average = a} :: SummarizedCounter)

-- | The maximum value of the counter for a specified time period.
summarizedCounter_max :: Lens.Lens' SummarizedCounter (Prelude.Maybe Prelude.Double)
summarizedCounter_max = Lens.lens (\SummarizedCounter' {max} -> max) (\s@SummarizedCounter' {} a -> s {max = a} :: SummarizedCounter)

-- | The number of counters for a specified time period.
summarizedCounter_n :: Lens.Lens' SummarizedCounter (Prelude.Maybe Prelude.Int)
summarizedCounter_n = Lens.lens (\SummarizedCounter' {n} -> n) (\s@SummarizedCounter' {} a -> s {n = a} :: SummarizedCounter)

-- | The counter name.
summarizedCounter_name :: Lens.Lens' SummarizedCounter (Prelude.Maybe Prelude.Text)
summarizedCounter_name = Lens.lens (\SummarizedCounter' {name} -> name) (\s@SummarizedCounter' {} a -> s {name = a} :: SummarizedCounter)

-- | The total of counter values for a specified time period.
summarizedCounter_sum :: Lens.Lens' SummarizedCounter (Prelude.Maybe Prelude.Double)
summarizedCounter_sum = Lens.lens (\SummarizedCounter' {sum} -> sum) (\s@SummarizedCounter' {} a -> s {sum = a} :: SummarizedCounter)

-- | The unit of the counters.
summarizedCounter_unit :: Lens.Lens' SummarizedCounter (Prelude.Maybe Prelude.Text)
summarizedCounter_unit = Lens.lens (\SummarizedCounter' {unit} -> unit) (\s@SummarizedCounter' {} a -> s {unit = a} :: SummarizedCounter)

instance Data.FromJSON SummarizedCounter where
  parseJSON =
    Data.withObject
      "SummarizedCounter"
      ( \x ->
          SummarizedCounter'
            Prelude.<$> (x Data..:? "Average")
            Prelude.<*> (x Data..:? "Max")
            Prelude.<*> (x Data..:? "N")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Sum")
            Prelude.<*> (x Data..:? "Unit")
      )

instance Prelude.Hashable SummarizedCounter where
  hashWithSalt _salt SummarizedCounter' {..} =
    _salt
      `Prelude.hashWithSalt` average
      `Prelude.hashWithSalt` max
      `Prelude.hashWithSalt` n
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` sum
      `Prelude.hashWithSalt` unit

instance Prelude.NFData SummarizedCounter where
  rnf SummarizedCounter' {..} =
    Prelude.rnf average
      `Prelude.seq` Prelude.rnf max
      `Prelude.seq` Prelude.rnf n
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf sum
      `Prelude.seq` Prelude.rnf unit
