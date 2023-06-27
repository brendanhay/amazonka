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
-- Module      : Amazonka.DataSync.Types.P95Metrics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.P95Metrics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types.IOPS
import Amazonka.DataSync.Types.Latency
import Amazonka.DataSync.Types.Throughput
import qualified Amazonka.Prelude as Prelude

-- | The types of performance data that DataSync Discovery collects about an
-- on-premises storage system resource.
--
-- /See:/ 'newP95Metrics' smart constructor.
data P95Metrics = P95Metrics'
  { -- | The IOPS peaks for an on-premises storage system resource. Each data
    -- point represents the 95th percentile peak value during a 1-hour
    -- interval.
    iops :: Prelude.Maybe IOPS,
    -- | The latency peaks for an on-premises storage system resource. Each data
    -- point represents the 95th percentile peak value during a 1-hour
    -- interval.
    latency :: Prelude.Maybe Latency,
    -- | The throughput peaks for an on-premises storage system resource. Each
    -- data point represents the 95th percentile peak value during a 1-hour
    -- interval.
    throughput :: Prelude.Maybe Throughput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'P95Metrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iops', 'p95Metrics_iops' - The IOPS peaks for an on-premises storage system resource. Each data
-- point represents the 95th percentile peak value during a 1-hour
-- interval.
--
-- 'latency', 'p95Metrics_latency' - The latency peaks for an on-premises storage system resource. Each data
-- point represents the 95th percentile peak value during a 1-hour
-- interval.
--
-- 'throughput', 'p95Metrics_throughput' - The throughput peaks for an on-premises storage system resource. Each
-- data point represents the 95th percentile peak value during a 1-hour
-- interval.
newP95Metrics ::
  P95Metrics
newP95Metrics =
  P95Metrics'
    { iops = Prelude.Nothing,
      latency = Prelude.Nothing,
      throughput = Prelude.Nothing
    }

-- | The IOPS peaks for an on-premises storage system resource. Each data
-- point represents the 95th percentile peak value during a 1-hour
-- interval.
p95Metrics_iops :: Lens.Lens' P95Metrics (Prelude.Maybe IOPS)
p95Metrics_iops = Lens.lens (\P95Metrics' {iops} -> iops) (\s@P95Metrics' {} a -> s {iops = a} :: P95Metrics)

-- | The latency peaks for an on-premises storage system resource. Each data
-- point represents the 95th percentile peak value during a 1-hour
-- interval.
p95Metrics_latency :: Lens.Lens' P95Metrics (Prelude.Maybe Latency)
p95Metrics_latency = Lens.lens (\P95Metrics' {latency} -> latency) (\s@P95Metrics' {} a -> s {latency = a} :: P95Metrics)

-- | The throughput peaks for an on-premises storage system resource. Each
-- data point represents the 95th percentile peak value during a 1-hour
-- interval.
p95Metrics_throughput :: Lens.Lens' P95Metrics (Prelude.Maybe Throughput)
p95Metrics_throughput = Lens.lens (\P95Metrics' {throughput} -> throughput) (\s@P95Metrics' {} a -> s {throughput = a} :: P95Metrics)

instance Data.FromJSON P95Metrics where
  parseJSON =
    Data.withObject
      "P95Metrics"
      ( \x ->
          P95Metrics'
            Prelude.<$> (x Data..:? "IOPS")
            Prelude.<*> (x Data..:? "Latency")
            Prelude.<*> (x Data..:? "Throughput")
      )

instance Prelude.Hashable P95Metrics where
  hashWithSalt _salt P95Metrics' {..} =
    _salt
      `Prelude.hashWithSalt` iops
      `Prelude.hashWithSalt` latency
      `Prelude.hashWithSalt` throughput

instance Prelude.NFData P95Metrics where
  rnf P95Metrics' {..} =
    Prelude.rnf iops
      `Prelude.seq` Prelude.rnf latency
      `Prelude.seq` Prelude.rnf throughput
