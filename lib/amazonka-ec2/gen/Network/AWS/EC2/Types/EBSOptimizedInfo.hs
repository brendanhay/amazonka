{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EBSOptimizedInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EBSOptimizedInfo
  ( EBSOptimizedInfo (..),

    -- * Smart constructor
    mkEBSOptimizedInfo,

    -- * Lenses
    eoiMaximumIOPS,
    eoiBaselineIOPS,
    eoiMaximumThroughputInMBps,
    eoiMaximumBandwidthInMbps,
    eoiBaselineBandwidthInMbps,
    eoiBaselineThroughputInMBps,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the optimized EBS performance for supported instance types.
--
-- /See:/ 'mkEBSOptimizedInfo' smart constructor.
data EBSOptimizedInfo = EBSOptimizedInfo'
  { maximumIOPS ::
      Lude.Maybe Lude.Int,
    baselineIOPS :: Lude.Maybe Lude.Int,
    maximumThroughputInMBps :: Lude.Maybe Lude.Double,
    maximumBandwidthInMbps :: Lude.Maybe Lude.Int,
    baselineBandwidthInMbps :: Lude.Maybe Lude.Int,
    baselineThroughputInMBps :: Lude.Maybe Lude.Double
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EBSOptimizedInfo' with the minimum fields required to make a request.
--
-- * 'baselineBandwidthInMbps' - The baseline bandwidth performance for an EBS-optimized instance type, in Mbps.
-- * 'baselineIOPS' - The baseline input/output storage operations per seconds for an EBS-optimized instance type.
-- * 'baselineThroughputInMBps' - The baseline throughput performance for an EBS-optimized instance type, in MB/s.
-- * 'maximumBandwidthInMbps' - The maximum bandwidth performance for an EBS-optimized instance type, in Mbps.
-- * 'maximumIOPS' - The maximum input/output storage operations per second for an EBS-optimized instance type.
-- * 'maximumThroughputInMBps' - The maximum throughput performance for an EBS-optimized instance type, in MB/s.
mkEBSOptimizedInfo ::
  EBSOptimizedInfo
mkEBSOptimizedInfo =
  EBSOptimizedInfo'
    { maximumIOPS = Lude.Nothing,
      baselineIOPS = Lude.Nothing,
      maximumThroughputInMBps = Lude.Nothing,
      maximumBandwidthInMbps = Lude.Nothing,
      baselineBandwidthInMbps = Lude.Nothing,
      baselineThroughputInMBps = Lude.Nothing
    }

-- | The maximum input/output storage operations per second for an EBS-optimized instance type.
--
-- /Note:/ Consider using 'maximumIOPS' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eoiMaximumIOPS :: Lens.Lens' EBSOptimizedInfo (Lude.Maybe Lude.Int)
eoiMaximumIOPS = Lens.lens (maximumIOPS :: EBSOptimizedInfo -> Lude.Maybe Lude.Int) (\s a -> s {maximumIOPS = a} :: EBSOptimizedInfo)
{-# DEPRECATED eoiMaximumIOPS "Use generic-lens or generic-optics with 'maximumIOPS' instead." #-}

-- | The baseline input/output storage operations per seconds for an EBS-optimized instance type.
--
-- /Note:/ Consider using 'baselineIOPS' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eoiBaselineIOPS :: Lens.Lens' EBSOptimizedInfo (Lude.Maybe Lude.Int)
eoiBaselineIOPS = Lens.lens (baselineIOPS :: EBSOptimizedInfo -> Lude.Maybe Lude.Int) (\s a -> s {baselineIOPS = a} :: EBSOptimizedInfo)
{-# DEPRECATED eoiBaselineIOPS "Use generic-lens or generic-optics with 'baselineIOPS' instead." #-}

-- | The maximum throughput performance for an EBS-optimized instance type, in MB/s.
--
-- /Note:/ Consider using 'maximumThroughputInMBps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eoiMaximumThroughputInMBps :: Lens.Lens' EBSOptimizedInfo (Lude.Maybe Lude.Double)
eoiMaximumThroughputInMBps = Lens.lens (maximumThroughputInMBps :: EBSOptimizedInfo -> Lude.Maybe Lude.Double) (\s a -> s {maximumThroughputInMBps = a} :: EBSOptimizedInfo)
{-# DEPRECATED eoiMaximumThroughputInMBps "Use generic-lens or generic-optics with 'maximumThroughputInMBps' instead." #-}

-- | The maximum bandwidth performance for an EBS-optimized instance type, in Mbps.
--
-- /Note:/ Consider using 'maximumBandwidthInMbps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eoiMaximumBandwidthInMbps :: Lens.Lens' EBSOptimizedInfo (Lude.Maybe Lude.Int)
eoiMaximumBandwidthInMbps = Lens.lens (maximumBandwidthInMbps :: EBSOptimizedInfo -> Lude.Maybe Lude.Int) (\s a -> s {maximumBandwidthInMbps = a} :: EBSOptimizedInfo)
{-# DEPRECATED eoiMaximumBandwidthInMbps "Use generic-lens or generic-optics with 'maximumBandwidthInMbps' instead." #-}

-- | The baseline bandwidth performance for an EBS-optimized instance type, in Mbps.
--
-- /Note:/ Consider using 'baselineBandwidthInMbps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eoiBaselineBandwidthInMbps :: Lens.Lens' EBSOptimizedInfo (Lude.Maybe Lude.Int)
eoiBaselineBandwidthInMbps = Lens.lens (baselineBandwidthInMbps :: EBSOptimizedInfo -> Lude.Maybe Lude.Int) (\s a -> s {baselineBandwidthInMbps = a} :: EBSOptimizedInfo)
{-# DEPRECATED eoiBaselineBandwidthInMbps "Use generic-lens or generic-optics with 'baselineBandwidthInMbps' instead." #-}

-- | The baseline throughput performance for an EBS-optimized instance type, in MB/s.
--
-- /Note:/ Consider using 'baselineThroughputInMBps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eoiBaselineThroughputInMBps :: Lens.Lens' EBSOptimizedInfo (Lude.Maybe Lude.Double)
eoiBaselineThroughputInMBps = Lens.lens (baselineThroughputInMBps :: EBSOptimizedInfo -> Lude.Maybe Lude.Double) (\s a -> s {baselineThroughputInMBps = a} :: EBSOptimizedInfo)
{-# DEPRECATED eoiBaselineThroughputInMBps "Use generic-lens or generic-optics with 'baselineThroughputInMBps' instead." #-}

instance Lude.FromXML EBSOptimizedInfo where
  parseXML x =
    EBSOptimizedInfo'
      Lude.<$> (x Lude..@? "maximumIops")
      Lude.<*> (x Lude..@? "baselineIops")
      Lude.<*> (x Lude..@? "maximumThroughputInMBps")
      Lude.<*> (x Lude..@? "maximumBandwidthInMbps")
      Lude.<*> (x Lude..@? "baselineBandwidthInMbps")
      Lude.<*> (x Lude..@? "baselineThroughputInMBps")
