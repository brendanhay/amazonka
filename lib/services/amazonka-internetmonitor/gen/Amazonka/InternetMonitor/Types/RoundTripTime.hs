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
-- Module      : Amazonka.InternetMonitor.Types.RoundTripTime
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.InternetMonitor.Types.RoundTripTime where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Round-trip time (RTT) is how long it takes for a request from the user
-- to return a response to the user. Amazon CloudWatch Internet Monitor
-- calculates RTT at different percentiles: p50, p90, and p95.
--
-- /See:/ 'newRoundTripTime' smart constructor.
data RoundTripTime = RoundTripTime'
  { -- | RTT at the 50th percentile (p50).
    p50 :: Prelude.Maybe Prelude.Double,
    -- | RTT at the 90th percentile (p90).
    p90 :: Prelude.Maybe Prelude.Double,
    -- | RTT at the 95th percentile (p95).
    p95 :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RoundTripTime' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'p50', 'roundTripTime_p50' - RTT at the 50th percentile (p50).
--
-- 'p90', 'roundTripTime_p90' - RTT at the 90th percentile (p90).
--
-- 'p95', 'roundTripTime_p95' - RTT at the 95th percentile (p95).
newRoundTripTime ::
  RoundTripTime
newRoundTripTime =
  RoundTripTime'
    { p50 = Prelude.Nothing,
      p90 = Prelude.Nothing,
      p95 = Prelude.Nothing
    }

-- | RTT at the 50th percentile (p50).
roundTripTime_p50 :: Lens.Lens' RoundTripTime (Prelude.Maybe Prelude.Double)
roundTripTime_p50 = Lens.lens (\RoundTripTime' {p50} -> p50) (\s@RoundTripTime' {} a -> s {p50 = a} :: RoundTripTime)

-- | RTT at the 90th percentile (p90).
roundTripTime_p90 :: Lens.Lens' RoundTripTime (Prelude.Maybe Prelude.Double)
roundTripTime_p90 = Lens.lens (\RoundTripTime' {p90} -> p90) (\s@RoundTripTime' {} a -> s {p90 = a} :: RoundTripTime)

-- | RTT at the 95th percentile (p95).
roundTripTime_p95 :: Lens.Lens' RoundTripTime (Prelude.Maybe Prelude.Double)
roundTripTime_p95 = Lens.lens (\RoundTripTime' {p95} -> p95) (\s@RoundTripTime' {} a -> s {p95 = a} :: RoundTripTime)

instance Data.FromJSON RoundTripTime where
  parseJSON =
    Data.withObject
      "RoundTripTime"
      ( \x ->
          RoundTripTime'
            Prelude.<$> (x Data..:? "P50")
            Prelude.<*> (x Data..:? "P90")
            Prelude.<*> (x Data..:? "P95")
      )

instance Prelude.Hashable RoundTripTime where
  hashWithSalt _salt RoundTripTime' {..} =
    _salt
      `Prelude.hashWithSalt` p50
      `Prelude.hashWithSalt` p90
      `Prelude.hashWithSalt` p95

instance Prelude.NFData RoundTripTime where
  rnf RoundTripTime' {..} =
    Prelude.rnf p50
      `Prelude.seq` Prelude.rnf p90
      `Prelude.seq` Prelude.rnf p95
