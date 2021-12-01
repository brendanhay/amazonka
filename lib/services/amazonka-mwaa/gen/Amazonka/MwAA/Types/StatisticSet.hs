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
-- Module      : Amazonka.MwAA.Types.StatisticSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MwAA.Types.StatisticSet where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Internal only API.
--
-- /See:/ 'newStatisticSet' smart constructor.
data StatisticSet = StatisticSet'
  { -- | Internal only API.
    sampleCount :: Prelude.Maybe Prelude.Int,
    -- | Internal only API.
    maximum :: Prelude.Maybe Prelude.Double,
    -- | Internal only API.
    minimum :: Prelude.Maybe Prelude.Double,
    -- | Internal only API.
    sum :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StatisticSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sampleCount', 'statisticSet_sampleCount' - Internal only API.
--
-- 'maximum', 'statisticSet_maximum' - Internal only API.
--
-- 'minimum', 'statisticSet_minimum' - Internal only API.
--
-- 'sum', 'statisticSet_sum' - Internal only API.
newStatisticSet ::
  StatisticSet
newStatisticSet =
  StatisticSet'
    { sampleCount = Prelude.Nothing,
      maximum = Prelude.Nothing,
      minimum = Prelude.Nothing,
      sum = Prelude.Nothing
    }

-- | Internal only API.
statisticSet_sampleCount :: Lens.Lens' StatisticSet (Prelude.Maybe Prelude.Int)
statisticSet_sampleCount = Lens.lens (\StatisticSet' {sampleCount} -> sampleCount) (\s@StatisticSet' {} a -> s {sampleCount = a} :: StatisticSet)

-- | Internal only API.
statisticSet_maximum :: Lens.Lens' StatisticSet (Prelude.Maybe Prelude.Double)
statisticSet_maximum = Lens.lens (\StatisticSet' {maximum} -> maximum) (\s@StatisticSet' {} a -> s {maximum = a} :: StatisticSet)

-- | Internal only API.
statisticSet_minimum :: Lens.Lens' StatisticSet (Prelude.Maybe Prelude.Double)
statisticSet_minimum = Lens.lens (\StatisticSet' {minimum} -> minimum) (\s@StatisticSet' {} a -> s {minimum = a} :: StatisticSet)

-- | Internal only API.
statisticSet_sum :: Lens.Lens' StatisticSet (Prelude.Maybe Prelude.Double)
statisticSet_sum = Lens.lens (\StatisticSet' {sum} -> sum) (\s@StatisticSet' {} a -> s {sum = a} :: StatisticSet)

instance Prelude.Hashable StatisticSet where
  hashWithSalt salt' StatisticSet' {..} =
    salt' `Prelude.hashWithSalt` sum
      `Prelude.hashWithSalt` minimum
      `Prelude.hashWithSalt` maximum
      `Prelude.hashWithSalt` sampleCount

instance Prelude.NFData StatisticSet where
  rnf StatisticSet' {..} =
    Prelude.rnf sampleCount
      `Prelude.seq` Prelude.rnf sum
      `Prelude.seq` Prelude.rnf minimum
      `Prelude.seq` Prelude.rnf maximum

instance Core.ToJSON StatisticSet where
  toJSON StatisticSet' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SampleCount" Core..=) Prelude.<$> sampleCount,
            ("Maximum" Core..=) Prelude.<$> maximum,
            ("Minimum" Core..=) Prelude.<$> minimum,
            ("Sum" Core..=) Prelude.<$> sum
          ]
      )
