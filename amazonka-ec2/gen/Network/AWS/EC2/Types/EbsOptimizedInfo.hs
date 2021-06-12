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
-- Module      : Network.AWS.EC2.Types.EbsOptimizedInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EbsOptimizedInfo where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes the optimized EBS performance for supported instance types.
--
-- /See:/ 'newEbsOptimizedInfo' smart constructor.
data EbsOptimizedInfo = EbsOptimizedInfo'
  { -- | The baseline bandwidth performance for an EBS-optimized instance type,
    -- in Mbps.
    baselineBandwidthInMbps :: Core.Maybe Core.Int,
    -- | The maximum input\/output storage operations per second for an
    -- EBS-optimized instance type.
    maximumIops :: Core.Maybe Core.Int,
    -- | The maximum bandwidth performance for an EBS-optimized instance type, in
    -- Mbps.
    maximumBandwidthInMbps :: Core.Maybe Core.Int,
    -- | The maximum throughput performance for an EBS-optimized instance type,
    -- in MB\/s.
    maximumThroughputInMBps :: Core.Maybe Core.Double,
    -- | The baseline input\/output storage operations per seconds for an
    -- EBS-optimized instance type.
    baselineIops :: Core.Maybe Core.Int,
    -- | The baseline throughput performance for an EBS-optimized instance type,
    -- in MB\/s.
    baselineThroughputInMBps :: Core.Maybe Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EbsOptimizedInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baselineBandwidthInMbps', 'ebsOptimizedInfo_baselineBandwidthInMbps' - The baseline bandwidth performance for an EBS-optimized instance type,
-- in Mbps.
--
-- 'maximumIops', 'ebsOptimizedInfo_maximumIops' - The maximum input\/output storage operations per second for an
-- EBS-optimized instance type.
--
-- 'maximumBandwidthInMbps', 'ebsOptimizedInfo_maximumBandwidthInMbps' - The maximum bandwidth performance for an EBS-optimized instance type, in
-- Mbps.
--
-- 'maximumThroughputInMBps', 'ebsOptimizedInfo_maximumThroughputInMBps' - The maximum throughput performance for an EBS-optimized instance type,
-- in MB\/s.
--
-- 'baselineIops', 'ebsOptimizedInfo_baselineIops' - The baseline input\/output storage operations per seconds for an
-- EBS-optimized instance type.
--
-- 'baselineThroughputInMBps', 'ebsOptimizedInfo_baselineThroughputInMBps' - The baseline throughput performance for an EBS-optimized instance type,
-- in MB\/s.
newEbsOptimizedInfo ::
  EbsOptimizedInfo
newEbsOptimizedInfo =
  EbsOptimizedInfo'
    { baselineBandwidthInMbps =
        Core.Nothing,
      maximumIops = Core.Nothing,
      maximumBandwidthInMbps = Core.Nothing,
      maximumThroughputInMBps = Core.Nothing,
      baselineIops = Core.Nothing,
      baselineThroughputInMBps = Core.Nothing
    }

-- | The baseline bandwidth performance for an EBS-optimized instance type,
-- in Mbps.
ebsOptimizedInfo_baselineBandwidthInMbps :: Lens.Lens' EbsOptimizedInfo (Core.Maybe Core.Int)
ebsOptimizedInfo_baselineBandwidthInMbps = Lens.lens (\EbsOptimizedInfo' {baselineBandwidthInMbps} -> baselineBandwidthInMbps) (\s@EbsOptimizedInfo' {} a -> s {baselineBandwidthInMbps = a} :: EbsOptimizedInfo)

-- | The maximum input\/output storage operations per second for an
-- EBS-optimized instance type.
ebsOptimizedInfo_maximumIops :: Lens.Lens' EbsOptimizedInfo (Core.Maybe Core.Int)
ebsOptimizedInfo_maximumIops = Lens.lens (\EbsOptimizedInfo' {maximumIops} -> maximumIops) (\s@EbsOptimizedInfo' {} a -> s {maximumIops = a} :: EbsOptimizedInfo)

-- | The maximum bandwidth performance for an EBS-optimized instance type, in
-- Mbps.
ebsOptimizedInfo_maximumBandwidthInMbps :: Lens.Lens' EbsOptimizedInfo (Core.Maybe Core.Int)
ebsOptimizedInfo_maximumBandwidthInMbps = Lens.lens (\EbsOptimizedInfo' {maximumBandwidthInMbps} -> maximumBandwidthInMbps) (\s@EbsOptimizedInfo' {} a -> s {maximumBandwidthInMbps = a} :: EbsOptimizedInfo)

-- | The maximum throughput performance for an EBS-optimized instance type,
-- in MB\/s.
ebsOptimizedInfo_maximumThroughputInMBps :: Lens.Lens' EbsOptimizedInfo (Core.Maybe Core.Double)
ebsOptimizedInfo_maximumThroughputInMBps = Lens.lens (\EbsOptimizedInfo' {maximumThroughputInMBps} -> maximumThroughputInMBps) (\s@EbsOptimizedInfo' {} a -> s {maximumThroughputInMBps = a} :: EbsOptimizedInfo)

-- | The baseline input\/output storage operations per seconds for an
-- EBS-optimized instance type.
ebsOptimizedInfo_baselineIops :: Lens.Lens' EbsOptimizedInfo (Core.Maybe Core.Int)
ebsOptimizedInfo_baselineIops = Lens.lens (\EbsOptimizedInfo' {baselineIops} -> baselineIops) (\s@EbsOptimizedInfo' {} a -> s {baselineIops = a} :: EbsOptimizedInfo)

-- | The baseline throughput performance for an EBS-optimized instance type,
-- in MB\/s.
ebsOptimizedInfo_baselineThroughputInMBps :: Lens.Lens' EbsOptimizedInfo (Core.Maybe Core.Double)
ebsOptimizedInfo_baselineThroughputInMBps = Lens.lens (\EbsOptimizedInfo' {baselineThroughputInMBps} -> baselineThroughputInMBps) (\s@EbsOptimizedInfo' {} a -> s {baselineThroughputInMBps = a} :: EbsOptimizedInfo)

instance Core.FromXML EbsOptimizedInfo where
  parseXML x =
    EbsOptimizedInfo'
      Core.<$> (x Core..@? "baselineBandwidthInMbps")
      Core.<*> (x Core..@? "maximumIops")
      Core.<*> (x Core..@? "maximumBandwidthInMbps")
      Core.<*> (x Core..@? "maximumThroughputInMBps")
      Core.<*> (x Core..@? "baselineIops")
      Core.<*> (x Core..@? "baselineThroughputInMBps")

instance Core.Hashable EbsOptimizedInfo

instance Core.NFData EbsOptimizedInfo
