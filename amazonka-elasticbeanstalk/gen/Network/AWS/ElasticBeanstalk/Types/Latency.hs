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
-- Module      : Network.AWS.ElasticBeanstalk.Types.Latency
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.Latency where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the average latency for the slowest X percent of requests
-- over the last 10 seconds.
--
-- /See:/ 'newLatency' smart constructor.
data Latency = Latency'
  { -- | The average latency for the slowest 5 percent of requests over the last
    -- 10 seconds.
    p95 :: Prelude.Maybe Prelude.Double,
    -- | The average latency for the slowest 90 percent of requests over the last
    -- 10 seconds.
    p10 :: Prelude.Maybe Prelude.Double,
    -- | The average latency for the slowest 0.1 percent of requests over the
    -- last 10 seconds.
    p999 :: Prelude.Maybe Prelude.Double,
    -- | The average latency for the slowest 1 percent of requests over the last
    -- 10 seconds.
    p99 :: Prelude.Maybe Prelude.Double,
    -- | The average latency for the slowest 15 percent of requests over the last
    -- 10 seconds.
    p85 :: Prelude.Maybe Prelude.Double,
    -- | The average latency for the slowest 50 percent of requests over the last
    -- 10 seconds.
    p50 :: Prelude.Maybe Prelude.Double,
    -- | The average latency for the slowest 10 percent of requests over the last
    -- 10 seconds.
    p90 :: Prelude.Maybe Prelude.Double,
    -- | The average latency for the slowest 25 percent of requests over the last
    -- 10 seconds.
    p75 :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Latency' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'p95', 'latency_p95' - The average latency for the slowest 5 percent of requests over the last
-- 10 seconds.
--
-- 'p10', 'latency_p10' - The average latency for the slowest 90 percent of requests over the last
-- 10 seconds.
--
-- 'p999', 'latency_p999' - The average latency for the slowest 0.1 percent of requests over the
-- last 10 seconds.
--
-- 'p99', 'latency_p99' - The average latency for the slowest 1 percent of requests over the last
-- 10 seconds.
--
-- 'p85', 'latency_p85' - The average latency for the slowest 15 percent of requests over the last
-- 10 seconds.
--
-- 'p50', 'latency_p50' - The average latency for the slowest 50 percent of requests over the last
-- 10 seconds.
--
-- 'p90', 'latency_p90' - The average latency for the slowest 10 percent of requests over the last
-- 10 seconds.
--
-- 'p75', 'latency_p75' - The average latency for the slowest 25 percent of requests over the last
-- 10 seconds.
newLatency ::
  Latency
newLatency =
  Latency'
    { p95 = Prelude.Nothing,
      p10 = Prelude.Nothing,
      p999 = Prelude.Nothing,
      p99 = Prelude.Nothing,
      p85 = Prelude.Nothing,
      p50 = Prelude.Nothing,
      p90 = Prelude.Nothing,
      p75 = Prelude.Nothing
    }

-- | The average latency for the slowest 5 percent of requests over the last
-- 10 seconds.
latency_p95 :: Lens.Lens' Latency (Prelude.Maybe Prelude.Double)
latency_p95 = Lens.lens (\Latency' {p95} -> p95) (\s@Latency' {} a -> s {p95 = a} :: Latency)

-- | The average latency for the slowest 90 percent of requests over the last
-- 10 seconds.
latency_p10 :: Lens.Lens' Latency (Prelude.Maybe Prelude.Double)
latency_p10 = Lens.lens (\Latency' {p10} -> p10) (\s@Latency' {} a -> s {p10 = a} :: Latency)

-- | The average latency for the slowest 0.1 percent of requests over the
-- last 10 seconds.
latency_p999 :: Lens.Lens' Latency (Prelude.Maybe Prelude.Double)
latency_p999 = Lens.lens (\Latency' {p999} -> p999) (\s@Latency' {} a -> s {p999 = a} :: Latency)

-- | The average latency for the slowest 1 percent of requests over the last
-- 10 seconds.
latency_p99 :: Lens.Lens' Latency (Prelude.Maybe Prelude.Double)
latency_p99 = Lens.lens (\Latency' {p99} -> p99) (\s@Latency' {} a -> s {p99 = a} :: Latency)

-- | The average latency for the slowest 15 percent of requests over the last
-- 10 seconds.
latency_p85 :: Lens.Lens' Latency (Prelude.Maybe Prelude.Double)
latency_p85 = Lens.lens (\Latency' {p85} -> p85) (\s@Latency' {} a -> s {p85 = a} :: Latency)

-- | The average latency for the slowest 50 percent of requests over the last
-- 10 seconds.
latency_p50 :: Lens.Lens' Latency (Prelude.Maybe Prelude.Double)
latency_p50 = Lens.lens (\Latency' {p50} -> p50) (\s@Latency' {} a -> s {p50 = a} :: Latency)

-- | The average latency for the slowest 10 percent of requests over the last
-- 10 seconds.
latency_p90 :: Lens.Lens' Latency (Prelude.Maybe Prelude.Double)
latency_p90 = Lens.lens (\Latency' {p90} -> p90) (\s@Latency' {} a -> s {p90 = a} :: Latency)

-- | The average latency for the slowest 25 percent of requests over the last
-- 10 seconds.
latency_p75 :: Lens.Lens' Latency (Prelude.Maybe Prelude.Double)
latency_p75 = Lens.lens (\Latency' {p75} -> p75) (\s@Latency' {} a -> s {p75 = a} :: Latency)

instance Prelude.FromXML Latency where
  parseXML x =
    Latency'
      Prelude.<$> (x Prelude..@? "P95")
      Prelude.<*> (x Prelude..@? "P10")
      Prelude.<*> (x Prelude..@? "P999")
      Prelude.<*> (x Prelude..@? "P99")
      Prelude.<*> (x Prelude..@? "P85")
      Prelude.<*> (x Prelude..@? "P50")
      Prelude.<*> (x Prelude..@? "P90")
      Prelude.<*> (x Prelude..@? "P75")

instance Prelude.Hashable Latency

instance Prelude.NFData Latency
