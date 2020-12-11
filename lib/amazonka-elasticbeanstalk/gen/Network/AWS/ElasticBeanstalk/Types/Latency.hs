-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.Latency
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.Latency
  ( Latency (..),

    -- * Smart constructor
    mkLatency,

    -- * Lenses
    lP75,
    lP50,
    lP85,
    lP999,
    lP90,
    lP95,
    lP99,
    lP10,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the average latency for the slowest X percent of requests over the last 10 seconds.
--
-- /See:/ 'mkLatency' smart constructor.
data Latency = Latency'
  { p75 :: Lude.Maybe Lude.Double,
    p50 :: Lude.Maybe Lude.Double,
    p85 :: Lude.Maybe Lude.Double,
    p999 :: Lude.Maybe Lude.Double,
    p90 :: Lude.Maybe Lude.Double,
    p95 :: Lude.Maybe Lude.Double,
    p99 :: Lude.Maybe Lude.Double,
    p10 :: Lude.Maybe Lude.Double
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Latency' with the minimum fields required to make a request.
--
-- * 'p10' - The average latency for the slowest 90 percent of requests over the last 10 seconds.
-- * 'p50' - The average latency for the slowest 50 percent of requests over the last 10 seconds.
-- * 'p75' - The average latency for the slowest 25 percent of requests over the last 10 seconds.
-- * 'p85' - The average latency for the slowest 15 percent of requests over the last 10 seconds.
-- * 'p90' - The average latency for the slowest 10 percent of requests over the last 10 seconds.
-- * 'p95' - The average latency for the slowest 5 percent of requests over the last 10 seconds.
-- * 'p99' - The average latency for the slowest 1 percent of requests over the last 10 seconds.
-- * 'p999' - The average latency for the slowest 0.1 percent of requests over the last 10 seconds.
mkLatency ::
  Latency
mkLatency =
  Latency'
    { p75 = Lude.Nothing,
      p50 = Lude.Nothing,
      p85 = Lude.Nothing,
      p999 = Lude.Nothing,
      p90 = Lude.Nothing,
      p95 = Lude.Nothing,
      p99 = Lude.Nothing,
      p10 = Lude.Nothing
    }

-- | The average latency for the slowest 25 percent of requests over the last 10 seconds.
--
-- /Note:/ Consider using 'p75' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lP75 :: Lens.Lens' Latency (Lude.Maybe Lude.Double)
lP75 = Lens.lens (p75 :: Latency -> Lude.Maybe Lude.Double) (\s a -> s {p75 = a} :: Latency)
{-# DEPRECATED lP75 "Use generic-lens or generic-optics with 'p75' instead." #-}

-- | The average latency for the slowest 50 percent of requests over the last 10 seconds.
--
-- /Note:/ Consider using 'p50' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lP50 :: Lens.Lens' Latency (Lude.Maybe Lude.Double)
lP50 = Lens.lens (p50 :: Latency -> Lude.Maybe Lude.Double) (\s a -> s {p50 = a} :: Latency)
{-# DEPRECATED lP50 "Use generic-lens or generic-optics with 'p50' instead." #-}

-- | The average latency for the slowest 15 percent of requests over the last 10 seconds.
--
-- /Note:/ Consider using 'p85' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lP85 :: Lens.Lens' Latency (Lude.Maybe Lude.Double)
lP85 = Lens.lens (p85 :: Latency -> Lude.Maybe Lude.Double) (\s a -> s {p85 = a} :: Latency)
{-# DEPRECATED lP85 "Use generic-lens or generic-optics with 'p85' instead." #-}

-- | The average latency for the slowest 0.1 percent of requests over the last 10 seconds.
--
-- /Note:/ Consider using 'p999' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lP999 :: Lens.Lens' Latency (Lude.Maybe Lude.Double)
lP999 = Lens.lens (p999 :: Latency -> Lude.Maybe Lude.Double) (\s a -> s {p999 = a} :: Latency)
{-# DEPRECATED lP999 "Use generic-lens or generic-optics with 'p999' instead." #-}

-- | The average latency for the slowest 10 percent of requests over the last 10 seconds.
--
-- /Note:/ Consider using 'p90' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lP90 :: Lens.Lens' Latency (Lude.Maybe Lude.Double)
lP90 = Lens.lens (p90 :: Latency -> Lude.Maybe Lude.Double) (\s a -> s {p90 = a} :: Latency)
{-# DEPRECATED lP90 "Use generic-lens or generic-optics with 'p90' instead." #-}

-- | The average latency for the slowest 5 percent of requests over the last 10 seconds.
--
-- /Note:/ Consider using 'p95' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lP95 :: Lens.Lens' Latency (Lude.Maybe Lude.Double)
lP95 = Lens.lens (p95 :: Latency -> Lude.Maybe Lude.Double) (\s a -> s {p95 = a} :: Latency)
{-# DEPRECATED lP95 "Use generic-lens or generic-optics with 'p95' instead." #-}

-- | The average latency for the slowest 1 percent of requests over the last 10 seconds.
--
-- /Note:/ Consider using 'p99' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lP99 :: Lens.Lens' Latency (Lude.Maybe Lude.Double)
lP99 = Lens.lens (p99 :: Latency -> Lude.Maybe Lude.Double) (\s a -> s {p99 = a} :: Latency)
{-# DEPRECATED lP99 "Use generic-lens or generic-optics with 'p99' instead." #-}

-- | The average latency for the slowest 90 percent of requests over the last 10 seconds.
--
-- /Note:/ Consider using 'p10' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lP10 :: Lens.Lens' Latency (Lude.Maybe Lude.Double)
lP10 = Lens.lens (p10 :: Latency -> Lude.Maybe Lude.Double) (\s a -> s {p10 = a} :: Latency)
{-# DEPRECATED lP10 "Use generic-lens or generic-optics with 'p10' instead." #-}

instance Lude.FromXML Latency where
  parseXML x =
    Latency'
      Lude.<$> (x Lude..@? "P75")
      Lude.<*> (x Lude..@? "P50")
      Lude.<*> (x Lude..@? "P85")
      Lude.<*> (x Lude..@? "P999")
      Lude.<*> (x Lude..@? "P90")
      Lude.<*> (x Lude..@? "P95")
      Lude.<*> (x Lude..@? "P99")
      Lude.<*> (x Lude..@? "P10")
