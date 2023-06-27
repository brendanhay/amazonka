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
-- Module      : Amazonka.DataSync.Types.MaxP95Performance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.MaxP95Performance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The performance data that DataSync Discovery collects about an
-- on-premises storage system resource.
--
-- /See:/ 'newMaxP95Performance' smart constructor.
data MaxP95Performance = MaxP95Performance'
  { -- | Peak IOPS unrelated to read and write operations.
    iopsOther :: Prelude.Maybe Prelude.Double,
    -- | Peak IOPS related to read operations.
    iopsRead :: Prelude.Maybe Prelude.Double,
    -- | Peak total IOPS on your on-premises storage system resource.
    iopsTotal :: Prelude.Maybe Prelude.Double,
    -- | Peak IOPS related to write operations.
    iopsWrite :: Prelude.Maybe Prelude.Double,
    -- | Peak latency for operations unrelated to read and write operations.
    latencyOther :: Prelude.Maybe Prelude.Double,
    -- | Peak latency for read operations.
    latencyRead :: Prelude.Maybe Prelude.Double,
    -- | Peak latency for write operations.
    latencyWrite :: Prelude.Maybe Prelude.Double,
    -- | Peak throughput unrelated to read and write operations.
    throughputOther :: Prelude.Maybe Prelude.Double,
    -- | Peak throughput related to read operations.
    throughputRead :: Prelude.Maybe Prelude.Double,
    -- | Peak total throughput on your on-premises storage system resource.
    throughputTotal :: Prelude.Maybe Prelude.Double,
    -- | Peak throughput related to write operations.
    throughputWrite :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MaxP95Performance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iopsOther', 'maxP95Performance_iopsOther' - Peak IOPS unrelated to read and write operations.
--
-- 'iopsRead', 'maxP95Performance_iopsRead' - Peak IOPS related to read operations.
--
-- 'iopsTotal', 'maxP95Performance_iopsTotal' - Peak total IOPS on your on-premises storage system resource.
--
-- 'iopsWrite', 'maxP95Performance_iopsWrite' - Peak IOPS related to write operations.
--
-- 'latencyOther', 'maxP95Performance_latencyOther' - Peak latency for operations unrelated to read and write operations.
--
-- 'latencyRead', 'maxP95Performance_latencyRead' - Peak latency for read operations.
--
-- 'latencyWrite', 'maxP95Performance_latencyWrite' - Peak latency for write operations.
--
-- 'throughputOther', 'maxP95Performance_throughputOther' - Peak throughput unrelated to read and write operations.
--
-- 'throughputRead', 'maxP95Performance_throughputRead' - Peak throughput related to read operations.
--
-- 'throughputTotal', 'maxP95Performance_throughputTotal' - Peak total throughput on your on-premises storage system resource.
--
-- 'throughputWrite', 'maxP95Performance_throughputWrite' - Peak throughput related to write operations.
newMaxP95Performance ::
  MaxP95Performance
newMaxP95Performance =
  MaxP95Performance'
    { iopsOther = Prelude.Nothing,
      iopsRead = Prelude.Nothing,
      iopsTotal = Prelude.Nothing,
      iopsWrite = Prelude.Nothing,
      latencyOther = Prelude.Nothing,
      latencyRead = Prelude.Nothing,
      latencyWrite = Prelude.Nothing,
      throughputOther = Prelude.Nothing,
      throughputRead = Prelude.Nothing,
      throughputTotal = Prelude.Nothing,
      throughputWrite = Prelude.Nothing
    }

-- | Peak IOPS unrelated to read and write operations.
maxP95Performance_iopsOther :: Lens.Lens' MaxP95Performance (Prelude.Maybe Prelude.Double)
maxP95Performance_iopsOther = Lens.lens (\MaxP95Performance' {iopsOther} -> iopsOther) (\s@MaxP95Performance' {} a -> s {iopsOther = a} :: MaxP95Performance)

-- | Peak IOPS related to read operations.
maxP95Performance_iopsRead :: Lens.Lens' MaxP95Performance (Prelude.Maybe Prelude.Double)
maxP95Performance_iopsRead = Lens.lens (\MaxP95Performance' {iopsRead} -> iopsRead) (\s@MaxP95Performance' {} a -> s {iopsRead = a} :: MaxP95Performance)

-- | Peak total IOPS on your on-premises storage system resource.
maxP95Performance_iopsTotal :: Lens.Lens' MaxP95Performance (Prelude.Maybe Prelude.Double)
maxP95Performance_iopsTotal = Lens.lens (\MaxP95Performance' {iopsTotal} -> iopsTotal) (\s@MaxP95Performance' {} a -> s {iopsTotal = a} :: MaxP95Performance)

-- | Peak IOPS related to write operations.
maxP95Performance_iopsWrite :: Lens.Lens' MaxP95Performance (Prelude.Maybe Prelude.Double)
maxP95Performance_iopsWrite = Lens.lens (\MaxP95Performance' {iopsWrite} -> iopsWrite) (\s@MaxP95Performance' {} a -> s {iopsWrite = a} :: MaxP95Performance)

-- | Peak latency for operations unrelated to read and write operations.
maxP95Performance_latencyOther :: Lens.Lens' MaxP95Performance (Prelude.Maybe Prelude.Double)
maxP95Performance_latencyOther = Lens.lens (\MaxP95Performance' {latencyOther} -> latencyOther) (\s@MaxP95Performance' {} a -> s {latencyOther = a} :: MaxP95Performance)

-- | Peak latency for read operations.
maxP95Performance_latencyRead :: Lens.Lens' MaxP95Performance (Prelude.Maybe Prelude.Double)
maxP95Performance_latencyRead = Lens.lens (\MaxP95Performance' {latencyRead} -> latencyRead) (\s@MaxP95Performance' {} a -> s {latencyRead = a} :: MaxP95Performance)

-- | Peak latency for write operations.
maxP95Performance_latencyWrite :: Lens.Lens' MaxP95Performance (Prelude.Maybe Prelude.Double)
maxP95Performance_latencyWrite = Lens.lens (\MaxP95Performance' {latencyWrite} -> latencyWrite) (\s@MaxP95Performance' {} a -> s {latencyWrite = a} :: MaxP95Performance)

-- | Peak throughput unrelated to read and write operations.
maxP95Performance_throughputOther :: Lens.Lens' MaxP95Performance (Prelude.Maybe Prelude.Double)
maxP95Performance_throughputOther = Lens.lens (\MaxP95Performance' {throughputOther} -> throughputOther) (\s@MaxP95Performance' {} a -> s {throughputOther = a} :: MaxP95Performance)

-- | Peak throughput related to read operations.
maxP95Performance_throughputRead :: Lens.Lens' MaxP95Performance (Prelude.Maybe Prelude.Double)
maxP95Performance_throughputRead = Lens.lens (\MaxP95Performance' {throughputRead} -> throughputRead) (\s@MaxP95Performance' {} a -> s {throughputRead = a} :: MaxP95Performance)

-- | Peak total throughput on your on-premises storage system resource.
maxP95Performance_throughputTotal :: Lens.Lens' MaxP95Performance (Prelude.Maybe Prelude.Double)
maxP95Performance_throughputTotal = Lens.lens (\MaxP95Performance' {throughputTotal} -> throughputTotal) (\s@MaxP95Performance' {} a -> s {throughputTotal = a} :: MaxP95Performance)

-- | Peak throughput related to write operations.
maxP95Performance_throughputWrite :: Lens.Lens' MaxP95Performance (Prelude.Maybe Prelude.Double)
maxP95Performance_throughputWrite = Lens.lens (\MaxP95Performance' {throughputWrite} -> throughputWrite) (\s@MaxP95Performance' {} a -> s {throughputWrite = a} :: MaxP95Performance)

instance Data.FromJSON MaxP95Performance where
  parseJSON =
    Data.withObject
      "MaxP95Performance"
      ( \x ->
          MaxP95Performance'
            Prelude.<$> (x Data..:? "IopsOther")
            Prelude.<*> (x Data..:? "IopsRead")
            Prelude.<*> (x Data..:? "IopsTotal")
            Prelude.<*> (x Data..:? "IopsWrite")
            Prelude.<*> (x Data..:? "LatencyOther")
            Prelude.<*> (x Data..:? "LatencyRead")
            Prelude.<*> (x Data..:? "LatencyWrite")
            Prelude.<*> (x Data..:? "ThroughputOther")
            Prelude.<*> (x Data..:? "ThroughputRead")
            Prelude.<*> (x Data..:? "ThroughputTotal")
            Prelude.<*> (x Data..:? "ThroughputWrite")
      )

instance Prelude.Hashable MaxP95Performance where
  hashWithSalt _salt MaxP95Performance' {..} =
    _salt
      `Prelude.hashWithSalt` iopsOther
      `Prelude.hashWithSalt` iopsRead
      `Prelude.hashWithSalt` iopsTotal
      `Prelude.hashWithSalt` iopsWrite
      `Prelude.hashWithSalt` latencyOther
      `Prelude.hashWithSalt` latencyRead
      `Prelude.hashWithSalt` latencyWrite
      `Prelude.hashWithSalt` throughputOther
      `Prelude.hashWithSalt` throughputRead
      `Prelude.hashWithSalt` throughputTotal
      `Prelude.hashWithSalt` throughputWrite

instance Prelude.NFData MaxP95Performance where
  rnf MaxP95Performance' {..} =
    Prelude.rnf iopsOther
      `Prelude.seq` Prelude.rnf iopsRead
      `Prelude.seq` Prelude.rnf iopsTotal
      `Prelude.seq` Prelude.rnf iopsWrite
      `Prelude.seq` Prelude.rnf latencyOther
      `Prelude.seq` Prelude.rnf latencyRead
      `Prelude.seq` Prelude.rnf latencyWrite
      `Prelude.seq` Prelude.rnf throughputOther
      `Prelude.seq` Prelude.rnf throughputRead
      `Prelude.seq` Prelude.rnf throughputTotal
      `Prelude.seq` Prelude.rnf throughputWrite
