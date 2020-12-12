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
    astInstanceCount,
    astIgnoreMetricsTime,
    astLoadThreshold,
    astThresholdsWaitTime,
    astAlarms,
    astMemoryThreshold,
    astCPUThreshold,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a load-based auto scaling upscaling or downscaling threshold configuration, which specifies when AWS OpsWorks Stacks starts or stops load-based instances.
--
-- /See:/ 'mkAutoScalingThresholds' smart constructor.
data AutoScalingThresholds = AutoScalingThresholds'
  { instanceCount ::
      Lude.Maybe Lude.Int,
    ignoreMetricsTime :: Lude.Maybe Lude.Natural,
    loadThreshold :: Lude.Maybe Lude.Double,
    thresholdsWaitTime :: Lude.Maybe Lude.Natural,
    alarms :: Lude.Maybe [Lude.Text],
    memoryThreshold :: Lude.Maybe Lude.Double,
    cpuThreshold :: Lude.Maybe Lude.Double
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AutoScalingThresholds' with the minimum fields required to make a request.
--
-- * 'alarms' - Custom Cloudwatch auto scaling alarms, to be used as thresholds. This parameter takes a list of up to five alarm names, which are case sensitive and must be in the same region as the stack.
-- * 'cpuThreshold' - The CPU utilization threshold, as a percent of the available CPU. A value of -1 disables the threshold.
-- * 'ignoreMetricsTime' - The amount of time (in minutes) after a scaling event occurs that AWS OpsWorks Stacks should ignore metrics and suppress additional scaling events. For example, AWS OpsWorks Stacks adds new instances following an upscaling event but the instances won't start reducing the load until they have been booted and configured. There is no point in raising additional scaling events during that operation, which typically takes several minutes. @IgnoreMetricsTime@ allows you to direct AWS OpsWorks Stacks to suppress scaling events long enough to get the new instances online.
-- * 'instanceCount' - The number of instances to add or remove when the load exceeds a threshold.
-- * 'loadThreshold' - The load threshold. A value of -1 disables the threshold. For more information about how load is computed, see <http://en.wikipedia.org/wiki/Load_%28computing%29 Load (computing)> .
-- * 'memoryThreshold' - The memory utilization threshold, as a percent of the available memory. A value of -1 disables the threshold.
-- * 'thresholdsWaitTime' - The amount of time, in minutes, that the load must exceed a threshold before more instances are added or removed.
mkAutoScalingThresholds ::
  AutoScalingThresholds
mkAutoScalingThresholds =
  AutoScalingThresholds'
    { instanceCount = Lude.Nothing,
      ignoreMetricsTime = Lude.Nothing,
      loadThreshold = Lude.Nothing,
      thresholdsWaitTime = Lude.Nothing,
      alarms = Lude.Nothing,
      memoryThreshold = Lude.Nothing,
      cpuThreshold = Lude.Nothing
    }

-- | The number of instances to add or remove when the load exceeds a threshold.
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
astInstanceCount :: Lens.Lens' AutoScalingThresholds (Lude.Maybe Lude.Int)
astInstanceCount = Lens.lens (instanceCount :: AutoScalingThresholds -> Lude.Maybe Lude.Int) (\s a -> s {instanceCount = a} :: AutoScalingThresholds)
{-# DEPRECATED astInstanceCount "Use generic-lens or generic-optics with 'instanceCount' instead." #-}

-- | The amount of time (in minutes) after a scaling event occurs that AWS OpsWorks Stacks should ignore metrics and suppress additional scaling events. For example, AWS OpsWorks Stacks adds new instances following an upscaling event but the instances won't start reducing the load until they have been booted and configured. There is no point in raising additional scaling events during that operation, which typically takes several minutes. @IgnoreMetricsTime@ allows you to direct AWS OpsWorks Stacks to suppress scaling events long enough to get the new instances online.
--
-- /Note:/ Consider using 'ignoreMetricsTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
astIgnoreMetricsTime :: Lens.Lens' AutoScalingThresholds (Lude.Maybe Lude.Natural)
astIgnoreMetricsTime = Lens.lens (ignoreMetricsTime :: AutoScalingThresholds -> Lude.Maybe Lude.Natural) (\s a -> s {ignoreMetricsTime = a} :: AutoScalingThresholds)
{-# DEPRECATED astIgnoreMetricsTime "Use generic-lens or generic-optics with 'ignoreMetricsTime' instead." #-}

-- | The load threshold. A value of -1 disables the threshold. For more information about how load is computed, see <http://en.wikipedia.org/wiki/Load_%28computing%29 Load (computing)> .
--
-- /Note:/ Consider using 'loadThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
astLoadThreshold :: Lens.Lens' AutoScalingThresholds (Lude.Maybe Lude.Double)
astLoadThreshold = Lens.lens (loadThreshold :: AutoScalingThresholds -> Lude.Maybe Lude.Double) (\s a -> s {loadThreshold = a} :: AutoScalingThresholds)
{-# DEPRECATED astLoadThreshold "Use generic-lens or generic-optics with 'loadThreshold' instead." #-}

-- | The amount of time, in minutes, that the load must exceed a threshold before more instances are added or removed.
--
-- /Note:/ Consider using 'thresholdsWaitTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
astThresholdsWaitTime :: Lens.Lens' AutoScalingThresholds (Lude.Maybe Lude.Natural)
astThresholdsWaitTime = Lens.lens (thresholdsWaitTime :: AutoScalingThresholds -> Lude.Maybe Lude.Natural) (\s a -> s {thresholdsWaitTime = a} :: AutoScalingThresholds)
{-# DEPRECATED astThresholdsWaitTime "Use generic-lens or generic-optics with 'thresholdsWaitTime' instead." #-}

-- | Custom Cloudwatch auto scaling alarms, to be used as thresholds. This parameter takes a list of up to five alarm names, which are case sensitive and must be in the same region as the stack.
--
-- /Note:/ Consider using 'alarms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
astAlarms :: Lens.Lens' AutoScalingThresholds (Lude.Maybe [Lude.Text])
astAlarms = Lens.lens (alarms :: AutoScalingThresholds -> Lude.Maybe [Lude.Text]) (\s a -> s {alarms = a} :: AutoScalingThresholds)
{-# DEPRECATED astAlarms "Use generic-lens or generic-optics with 'alarms' instead." #-}

-- | The memory utilization threshold, as a percent of the available memory. A value of -1 disables the threshold.
--
-- /Note:/ Consider using 'memoryThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
astMemoryThreshold :: Lens.Lens' AutoScalingThresholds (Lude.Maybe Lude.Double)
astMemoryThreshold = Lens.lens (memoryThreshold :: AutoScalingThresholds -> Lude.Maybe Lude.Double) (\s a -> s {memoryThreshold = a} :: AutoScalingThresholds)
{-# DEPRECATED astMemoryThreshold "Use generic-lens or generic-optics with 'memoryThreshold' instead." #-}

-- | The CPU utilization threshold, as a percent of the available CPU. A value of -1 disables the threshold.
--
-- /Note:/ Consider using 'cpuThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
astCPUThreshold :: Lens.Lens' AutoScalingThresholds (Lude.Maybe Lude.Double)
astCPUThreshold = Lens.lens (cpuThreshold :: AutoScalingThresholds -> Lude.Maybe Lude.Double) (\s a -> s {cpuThreshold = a} :: AutoScalingThresholds)
{-# DEPRECATED astCPUThreshold "Use generic-lens or generic-optics with 'cpuThreshold' instead." #-}

instance Lude.FromJSON AutoScalingThresholds where
  parseJSON =
    Lude.withObject
      "AutoScalingThresholds"
      ( \x ->
          AutoScalingThresholds'
            Lude.<$> (x Lude..:? "InstanceCount")
            Lude.<*> (x Lude..:? "IgnoreMetricsTime")
            Lude.<*> (x Lude..:? "LoadThreshold")
            Lude.<*> (x Lude..:? "ThresholdsWaitTime")
            Lude.<*> (x Lude..:? "Alarms" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "MemoryThreshold")
            Lude.<*> (x Lude..:? "CpuThreshold")
      )

instance Lude.ToJSON AutoScalingThresholds where
  toJSON AutoScalingThresholds' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("InstanceCount" Lude..=) Lude.<$> instanceCount,
            ("IgnoreMetricsTime" Lude..=) Lude.<$> ignoreMetricsTime,
            ("LoadThreshold" Lude..=) Lude.<$> loadThreshold,
            ("ThresholdsWaitTime" Lude..=) Lude.<$> thresholdsWaitTime,
            ("Alarms" Lude..=) Lude.<$> alarms,
            ("MemoryThreshold" Lude..=) Lude.<$> memoryThreshold,
            ("CpuThreshold" Lude..=) Lude.<$> cpuThreshold
          ]
      )
