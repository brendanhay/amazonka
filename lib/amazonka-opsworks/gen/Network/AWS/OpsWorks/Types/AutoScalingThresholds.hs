{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.AutoScalingThresholds
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.AutoScalingThresholds
  ( AutoScalingThresholds (..),

    -- * Smart constructor
    mkAutoScalingThresholds,

    -- * Lenses
    astAlarms,
    astCpuThreshold,
    astIgnoreMetricsTime,
    astInstanceCount,
    astLoadThreshold,
    astMemoryThreshold,
    astThresholdsWaitTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types.String as Types
import qualified Network.AWS.Prelude as Core

-- | Describes a load-based auto scaling upscaling or downscaling threshold configuration, which specifies when AWS OpsWorks Stacks starts or stops load-based instances.
--
-- /See:/ 'mkAutoScalingThresholds' smart constructor.
data AutoScalingThresholds = AutoScalingThresholds'
  { -- | Custom Cloudwatch auto scaling alarms, to be used as thresholds. This parameter takes a list of up to five alarm names, which are case sensitive and must be in the same region as the stack.
    alarms :: Core.Maybe [Types.String],
    -- | The CPU utilization threshold, as a percent of the available CPU. A value of -1 disables the threshold.
    cpuThreshold :: Core.Maybe Core.Double,
    -- | The amount of time (in minutes) after a scaling event occurs that AWS OpsWorks Stacks should ignore metrics and suppress additional scaling events. For example, AWS OpsWorks Stacks adds new instances following an upscaling event but the instances won't start reducing the load until they have been booted and configured. There is no point in raising additional scaling events during that operation, which typically takes several minutes. @IgnoreMetricsTime@ allows you to direct AWS OpsWorks Stacks to suppress scaling events long enough to get the new instances online.
    ignoreMetricsTime :: Core.Maybe Core.Natural,
    -- | The number of instances to add or remove when the load exceeds a threshold.
    instanceCount :: Core.Maybe Core.Int,
    -- | The load threshold. A value of -1 disables the threshold. For more information about how load is computed, see <http://en.wikipedia.org/wiki/Load_%28computing%29 Load (computing)> .
    loadThreshold :: Core.Maybe Core.Double,
    -- | The memory utilization threshold, as a percent of the available memory. A value of -1 disables the threshold.
    memoryThreshold :: Core.Maybe Core.Double,
    -- | The amount of time, in minutes, that the load must exceed a threshold before more instances are added or removed.
    thresholdsWaitTime :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AutoScalingThresholds' value with any optional fields omitted.
mkAutoScalingThresholds ::
  AutoScalingThresholds
mkAutoScalingThresholds =
  AutoScalingThresholds'
    { alarms = Core.Nothing,
      cpuThreshold = Core.Nothing,
      ignoreMetricsTime = Core.Nothing,
      instanceCount = Core.Nothing,
      loadThreshold = Core.Nothing,
      memoryThreshold = Core.Nothing,
      thresholdsWaitTime = Core.Nothing
    }

-- | Custom Cloudwatch auto scaling alarms, to be used as thresholds. This parameter takes a list of up to five alarm names, which are case sensitive and must be in the same region as the stack.
--
-- /Note:/ Consider using 'alarms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
astAlarms :: Lens.Lens' AutoScalingThresholds (Core.Maybe [Types.String])
astAlarms = Lens.field @"alarms"
{-# DEPRECATED astAlarms "Use generic-lens or generic-optics with 'alarms' instead." #-}

-- | The CPU utilization threshold, as a percent of the available CPU. A value of -1 disables the threshold.
--
-- /Note:/ Consider using 'cpuThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
astCpuThreshold :: Lens.Lens' AutoScalingThresholds (Core.Maybe Core.Double)
astCpuThreshold = Lens.field @"cpuThreshold"
{-# DEPRECATED astCpuThreshold "Use generic-lens or generic-optics with 'cpuThreshold' instead." #-}

-- | The amount of time (in minutes) after a scaling event occurs that AWS OpsWorks Stacks should ignore metrics and suppress additional scaling events. For example, AWS OpsWorks Stacks adds new instances following an upscaling event but the instances won't start reducing the load until they have been booted and configured. There is no point in raising additional scaling events during that operation, which typically takes several minutes. @IgnoreMetricsTime@ allows you to direct AWS OpsWorks Stacks to suppress scaling events long enough to get the new instances online.
--
-- /Note:/ Consider using 'ignoreMetricsTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
astIgnoreMetricsTime :: Lens.Lens' AutoScalingThresholds (Core.Maybe Core.Natural)
astIgnoreMetricsTime = Lens.field @"ignoreMetricsTime"
{-# DEPRECATED astIgnoreMetricsTime "Use generic-lens or generic-optics with 'ignoreMetricsTime' instead." #-}

-- | The number of instances to add or remove when the load exceeds a threshold.
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
astInstanceCount :: Lens.Lens' AutoScalingThresholds (Core.Maybe Core.Int)
astInstanceCount = Lens.field @"instanceCount"
{-# DEPRECATED astInstanceCount "Use generic-lens or generic-optics with 'instanceCount' instead." #-}

-- | The load threshold. A value of -1 disables the threshold. For more information about how load is computed, see <http://en.wikipedia.org/wiki/Load_%28computing%29 Load (computing)> .
--
-- /Note:/ Consider using 'loadThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
astLoadThreshold :: Lens.Lens' AutoScalingThresholds (Core.Maybe Core.Double)
astLoadThreshold = Lens.field @"loadThreshold"
{-# DEPRECATED astLoadThreshold "Use generic-lens or generic-optics with 'loadThreshold' instead." #-}

-- | The memory utilization threshold, as a percent of the available memory. A value of -1 disables the threshold.
--
-- /Note:/ Consider using 'memoryThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
astMemoryThreshold :: Lens.Lens' AutoScalingThresholds (Core.Maybe Core.Double)
astMemoryThreshold = Lens.field @"memoryThreshold"
{-# DEPRECATED astMemoryThreshold "Use generic-lens or generic-optics with 'memoryThreshold' instead." #-}

-- | The amount of time, in minutes, that the load must exceed a threshold before more instances are added or removed.
--
-- /Note:/ Consider using 'thresholdsWaitTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
astThresholdsWaitTime :: Lens.Lens' AutoScalingThresholds (Core.Maybe Core.Natural)
astThresholdsWaitTime = Lens.field @"thresholdsWaitTime"
{-# DEPRECATED astThresholdsWaitTime "Use generic-lens or generic-optics with 'thresholdsWaitTime' instead." #-}

instance Core.FromJSON AutoScalingThresholds where
  toJSON AutoScalingThresholds {..} =
    Core.object
      ( Core.catMaybes
          [ ("Alarms" Core..=) Core.<$> alarms,
            ("CpuThreshold" Core..=) Core.<$> cpuThreshold,
            ("IgnoreMetricsTime" Core..=) Core.<$> ignoreMetricsTime,
            ("InstanceCount" Core..=) Core.<$> instanceCount,
            ("LoadThreshold" Core..=) Core.<$> loadThreshold,
            ("MemoryThreshold" Core..=) Core.<$> memoryThreshold,
            ("ThresholdsWaitTime" Core..=) Core.<$> thresholdsWaitTime
          ]
      )

instance Core.FromJSON AutoScalingThresholds where
  parseJSON =
    Core.withObject "AutoScalingThresholds" Core.$
      \x ->
        AutoScalingThresholds'
          Core.<$> (x Core..:? "Alarms")
          Core.<*> (x Core..:? "CpuThreshold")
          Core.<*> (x Core..:? "IgnoreMetricsTime")
          Core.<*> (x Core..:? "InstanceCount")
          Core.<*> (x Core..:? "LoadThreshold")
          Core.<*> (x Core..:? "MemoryThreshold")
          Core.<*> (x Core..:? "ThresholdsWaitTime")
