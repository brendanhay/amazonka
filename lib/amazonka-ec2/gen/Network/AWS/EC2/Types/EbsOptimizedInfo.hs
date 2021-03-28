{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EbsOptimizedInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.EbsOptimizedInfo
  ( EbsOptimizedInfo (..)
  -- * Smart constructor
  , mkEbsOptimizedInfo
  -- * Lenses
  , eoiBaselineBandwidthInMbps
  , eoiBaselineIops
  , eoiBaselineThroughputInMBps
  , eoiMaximumBandwidthInMbps
  , eoiMaximumIops
  , eoiMaximumThroughputInMBps
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the optimized EBS performance for supported instance types.
--
-- /See:/ 'mkEbsOptimizedInfo' smart constructor.
data EbsOptimizedInfo = EbsOptimizedInfo'
  { baselineBandwidthInMbps :: Core.Maybe Core.Int
    -- ^ The baseline bandwidth performance for an EBS-optimized instance type, in Mbps.
  , baselineIops :: Core.Maybe Core.Int
    -- ^ The baseline input/output storage operations per seconds for an EBS-optimized instance type.
  , baselineThroughputInMBps :: Core.Maybe Core.Double
    -- ^ The baseline throughput performance for an EBS-optimized instance type, in MB/s.
  , maximumBandwidthInMbps :: Core.Maybe Core.Int
    -- ^ The maximum bandwidth performance for an EBS-optimized instance type, in Mbps.
  , maximumIops :: Core.Maybe Core.Int
    -- ^ The maximum input/output storage operations per second for an EBS-optimized instance type.
  , maximumThroughputInMBps :: Core.Maybe Core.Double
    -- ^ The maximum throughput performance for an EBS-optimized instance type, in MB/s.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EbsOptimizedInfo' value with any optional fields omitted.
mkEbsOptimizedInfo
    :: EbsOptimizedInfo
mkEbsOptimizedInfo
  = EbsOptimizedInfo'{baselineBandwidthInMbps = Core.Nothing,
                      baselineIops = Core.Nothing,
                      baselineThroughputInMBps = Core.Nothing,
                      maximumBandwidthInMbps = Core.Nothing, maximumIops = Core.Nothing,
                      maximumThroughputInMBps = Core.Nothing}

-- | The baseline bandwidth performance for an EBS-optimized instance type, in Mbps.
--
-- /Note:/ Consider using 'baselineBandwidthInMbps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eoiBaselineBandwidthInMbps :: Lens.Lens' EbsOptimizedInfo (Core.Maybe Core.Int)
eoiBaselineBandwidthInMbps = Lens.field @"baselineBandwidthInMbps"
{-# INLINEABLE eoiBaselineBandwidthInMbps #-}
{-# DEPRECATED baselineBandwidthInMbps "Use generic-lens or generic-optics with 'baselineBandwidthInMbps' instead"  #-}

-- | The baseline input/output storage operations per seconds for an EBS-optimized instance type.
--
-- /Note:/ Consider using 'baselineIops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eoiBaselineIops :: Lens.Lens' EbsOptimizedInfo (Core.Maybe Core.Int)
eoiBaselineIops = Lens.field @"baselineIops"
{-# INLINEABLE eoiBaselineIops #-}
{-# DEPRECATED baselineIops "Use generic-lens or generic-optics with 'baselineIops' instead"  #-}

-- | The baseline throughput performance for an EBS-optimized instance type, in MB/s.
--
-- /Note:/ Consider using 'baselineThroughputInMBps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eoiBaselineThroughputInMBps :: Lens.Lens' EbsOptimizedInfo (Core.Maybe Core.Double)
eoiBaselineThroughputInMBps = Lens.field @"baselineThroughputInMBps"
{-# INLINEABLE eoiBaselineThroughputInMBps #-}
{-# DEPRECATED baselineThroughputInMBps "Use generic-lens or generic-optics with 'baselineThroughputInMBps' instead"  #-}

-- | The maximum bandwidth performance for an EBS-optimized instance type, in Mbps.
--
-- /Note:/ Consider using 'maximumBandwidthInMbps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eoiMaximumBandwidthInMbps :: Lens.Lens' EbsOptimizedInfo (Core.Maybe Core.Int)
eoiMaximumBandwidthInMbps = Lens.field @"maximumBandwidthInMbps"
{-# INLINEABLE eoiMaximumBandwidthInMbps #-}
{-# DEPRECATED maximumBandwidthInMbps "Use generic-lens or generic-optics with 'maximumBandwidthInMbps' instead"  #-}

-- | The maximum input/output storage operations per second for an EBS-optimized instance type.
--
-- /Note:/ Consider using 'maximumIops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eoiMaximumIops :: Lens.Lens' EbsOptimizedInfo (Core.Maybe Core.Int)
eoiMaximumIops = Lens.field @"maximumIops"
{-# INLINEABLE eoiMaximumIops #-}
{-# DEPRECATED maximumIops "Use generic-lens or generic-optics with 'maximumIops' instead"  #-}

-- | The maximum throughput performance for an EBS-optimized instance type, in MB/s.
--
-- /Note:/ Consider using 'maximumThroughputInMBps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eoiMaximumThroughputInMBps :: Lens.Lens' EbsOptimizedInfo (Core.Maybe Core.Double)
eoiMaximumThroughputInMBps = Lens.field @"maximumThroughputInMBps"
{-# INLINEABLE eoiMaximumThroughputInMBps #-}
{-# DEPRECATED maximumThroughputInMBps "Use generic-lens or generic-optics with 'maximumThroughputInMBps' instead"  #-}

instance Core.FromXML EbsOptimizedInfo where
        parseXML x
          = EbsOptimizedInfo' Core.<$>
              (x Core..@? "baselineBandwidthInMbps") Core.<*>
                x Core..@? "baselineIops"
                Core.<*> x Core..@? "baselineThroughputInMBps"
                Core.<*> x Core..@? "maximumBandwidthInMbps"
                Core.<*> x Core..@? "maximumIops"
                Core.<*> x Core..@? "maximumThroughputInMBps"
