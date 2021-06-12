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
-- Module      : Network.AWS.OpsWorks.Types.AutoScalingThresholds
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.AutoScalingThresholds where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes a load-based auto scaling upscaling or downscaling threshold
-- configuration, which specifies when AWS OpsWorks Stacks starts or stops
-- load-based instances.
--
-- /See:/ 'newAutoScalingThresholds' smart constructor.
data AutoScalingThresholds = AutoScalingThresholds'
  { -- | The load threshold. A value of -1 disables the threshold. For more
    -- information about how load is computed, see
    -- <http://en.wikipedia.org/wiki/Load_%28computing%29 Load (computing)>.
    loadThreshold :: Core.Maybe Core.Double,
    -- | The CPU utilization threshold, as a percent of the available CPU. A
    -- value of -1 disables the threshold.
    cpuThreshold :: Core.Maybe Core.Double,
    -- | The memory utilization threshold, as a percent of the available memory.
    -- A value of -1 disables the threshold.
    memoryThreshold :: Core.Maybe Core.Double,
    -- | Custom Cloudwatch auto scaling alarms, to be used as thresholds. This
    -- parameter takes a list of up to five alarm names, which are case
    -- sensitive and must be in the same region as the stack.
    --
    -- To use custom alarms, you must update your service role to allow
    -- @cloudwatch:DescribeAlarms@. You can either have AWS OpsWorks Stacks
    -- update the role for you when you first use this feature or you can edit
    -- the role manually. For more information, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-servicerole.html Allowing AWS OpsWorks Stacks to Act on Your Behalf>.
    alarms :: Core.Maybe [Core.Text],
    -- | The amount of time (in minutes) after a scaling event occurs that AWS
    -- OpsWorks Stacks should ignore metrics and suppress additional scaling
    -- events. For example, AWS OpsWorks Stacks adds new instances following an
    -- upscaling event but the instances won\'t start reducing the load until
    -- they have been booted and configured. There is no point in raising
    -- additional scaling events during that operation, which typically takes
    -- several minutes. @IgnoreMetricsTime@ allows you to direct AWS OpsWorks
    -- Stacks to suppress scaling events long enough to get the new instances
    -- online.
    ignoreMetricsTime :: Core.Maybe Core.Natural,
    -- | The amount of time, in minutes, that the load must exceed a threshold
    -- before more instances are added or removed.
    thresholdsWaitTime :: Core.Maybe Core.Natural,
    -- | The number of instances to add or remove when the load exceeds a
    -- threshold.
    instanceCount :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AutoScalingThresholds' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadThreshold', 'autoScalingThresholds_loadThreshold' - The load threshold. A value of -1 disables the threshold. For more
-- information about how load is computed, see
-- <http://en.wikipedia.org/wiki/Load_%28computing%29 Load (computing)>.
--
-- 'cpuThreshold', 'autoScalingThresholds_cpuThreshold' - The CPU utilization threshold, as a percent of the available CPU. A
-- value of -1 disables the threshold.
--
-- 'memoryThreshold', 'autoScalingThresholds_memoryThreshold' - The memory utilization threshold, as a percent of the available memory.
-- A value of -1 disables the threshold.
--
-- 'alarms', 'autoScalingThresholds_alarms' - Custom Cloudwatch auto scaling alarms, to be used as thresholds. This
-- parameter takes a list of up to five alarm names, which are case
-- sensitive and must be in the same region as the stack.
--
-- To use custom alarms, you must update your service role to allow
-- @cloudwatch:DescribeAlarms@. You can either have AWS OpsWorks Stacks
-- update the role for you when you first use this feature or you can edit
-- the role manually. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-servicerole.html Allowing AWS OpsWorks Stacks to Act on Your Behalf>.
--
-- 'ignoreMetricsTime', 'autoScalingThresholds_ignoreMetricsTime' - The amount of time (in minutes) after a scaling event occurs that AWS
-- OpsWorks Stacks should ignore metrics and suppress additional scaling
-- events. For example, AWS OpsWorks Stacks adds new instances following an
-- upscaling event but the instances won\'t start reducing the load until
-- they have been booted and configured. There is no point in raising
-- additional scaling events during that operation, which typically takes
-- several minutes. @IgnoreMetricsTime@ allows you to direct AWS OpsWorks
-- Stacks to suppress scaling events long enough to get the new instances
-- online.
--
-- 'thresholdsWaitTime', 'autoScalingThresholds_thresholdsWaitTime' - The amount of time, in minutes, that the load must exceed a threshold
-- before more instances are added or removed.
--
-- 'instanceCount', 'autoScalingThresholds_instanceCount' - The number of instances to add or remove when the load exceeds a
-- threshold.
newAutoScalingThresholds ::
  AutoScalingThresholds
newAutoScalingThresholds =
  AutoScalingThresholds'
    { loadThreshold =
        Core.Nothing,
      cpuThreshold = Core.Nothing,
      memoryThreshold = Core.Nothing,
      alarms = Core.Nothing,
      ignoreMetricsTime = Core.Nothing,
      thresholdsWaitTime = Core.Nothing,
      instanceCount = Core.Nothing
    }

-- | The load threshold. A value of -1 disables the threshold. For more
-- information about how load is computed, see
-- <http://en.wikipedia.org/wiki/Load_%28computing%29 Load (computing)>.
autoScalingThresholds_loadThreshold :: Lens.Lens' AutoScalingThresholds (Core.Maybe Core.Double)
autoScalingThresholds_loadThreshold = Lens.lens (\AutoScalingThresholds' {loadThreshold} -> loadThreshold) (\s@AutoScalingThresholds' {} a -> s {loadThreshold = a} :: AutoScalingThresholds)

-- | The CPU utilization threshold, as a percent of the available CPU. A
-- value of -1 disables the threshold.
autoScalingThresholds_cpuThreshold :: Lens.Lens' AutoScalingThresholds (Core.Maybe Core.Double)
autoScalingThresholds_cpuThreshold = Lens.lens (\AutoScalingThresholds' {cpuThreshold} -> cpuThreshold) (\s@AutoScalingThresholds' {} a -> s {cpuThreshold = a} :: AutoScalingThresholds)

-- | The memory utilization threshold, as a percent of the available memory.
-- A value of -1 disables the threshold.
autoScalingThresholds_memoryThreshold :: Lens.Lens' AutoScalingThresholds (Core.Maybe Core.Double)
autoScalingThresholds_memoryThreshold = Lens.lens (\AutoScalingThresholds' {memoryThreshold} -> memoryThreshold) (\s@AutoScalingThresholds' {} a -> s {memoryThreshold = a} :: AutoScalingThresholds)

-- | Custom Cloudwatch auto scaling alarms, to be used as thresholds. This
-- parameter takes a list of up to five alarm names, which are case
-- sensitive and must be in the same region as the stack.
--
-- To use custom alarms, you must update your service role to allow
-- @cloudwatch:DescribeAlarms@. You can either have AWS OpsWorks Stacks
-- update the role for you when you first use this feature or you can edit
-- the role manually. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-servicerole.html Allowing AWS OpsWorks Stacks to Act on Your Behalf>.
autoScalingThresholds_alarms :: Lens.Lens' AutoScalingThresholds (Core.Maybe [Core.Text])
autoScalingThresholds_alarms = Lens.lens (\AutoScalingThresholds' {alarms} -> alarms) (\s@AutoScalingThresholds' {} a -> s {alarms = a} :: AutoScalingThresholds) Core.. Lens.mapping Lens._Coerce

-- | The amount of time (in minutes) after a scaling event occurs that AWS
-- OpsWorks Stacks should ignore metrics and suppress additional scaling
-- events. For example, AWS OpsWorks Stacks adds new instances following an
-- upscaling event but the instances won\'t start reducing the load until
-- they have been booted and configured. There is no point in raising
-- additional scaling events during that operation, which typically takes
-- several minutes. @IgnoreMetricsTime@ allows you to direct AWS OpsWorks
-- Stacks to suppress scaling events long enough to get the new instances
-- online.
autoScalingThresholds_ignoreMetricsTime :: Lens.Lens' AutoScalingThresholds (Core.Maybe Core.Natural)
autoScalingThresholds_ignoreMetricsTime = Lens.lens (\AutoScalingThresholds' {ignoreMetricsTime} -> ignoreMetricsTime) (\s@AutoScalingThresholds' {} a -> s {ignoreMetricsTime = a} :: AutoScalingThresholds)

-- | The amount of time, in minutes, that the load must exceed a threshold
-- before more instances are added or removed.
autoScalingThresholds_thresholdsWaitTime :: Lens.Lens' AutoScalingThresholds (Core.Maybe Core.Natural)
autoScalingThresholds_thresholdsWaitTime = Lens.lens (\AutoScalingThresholds' {thresholdsWaitTime} -> thresholdsWaitTime) (\s@AutoScalingThresholds' {} a -> s {thresholdsWaitTime = a} :: AutoScalingThresholds)

-- | The number of instances to add or remove when the load exceeds a
-- threshold.
autoScalingThresholds_instanceCount :: Lens.Lens' AutoScalingThresholds (Core.Maybe Core.Int)
autoScalingThresholds_instanceCount = Lens.lens (\AutoScalingThresholds' {instanceCount} -> instanceCount) (\s@AutoScalingThresholds' {} a -> s {instanceCount = a} :: AutoScalingThresholds)

instance Core.FromJSON AutoScalingThresholds where
  parseJSON =
    Core.withObject
      "AutoScalingThresholds"
      ( \x ->
          AutoScalingThresholds'
            Core.<$> (x Core..:? "LoadThreshold")
            Core.<*> (x Core..:? "CpuThreshold")
            Core.<*> (x Core..:? "MemoryThreshold")
            Core.<*> (x Core..:? "Alarms" Core..!= Core.mempty)
            Core.<*> (x Core..:? "IgnoreMetricsTime")
            Core.<*> (x Core..:? "ThresholdsWaitTime")
            Core.<*> (x Core..:? "InstanceCount")
      )

instance Core.Hashable AutoScalingThresholds

instance Core.NFData AutoScalingThresholds

instance Core.ToJSON AutoScalingThresholds where
  toJSON AutoScalingThresholds' {..} =
    Core.object
      ( Core.catMaybes
          [ ("LoadThreshold" Core..=) Core.<$> loadThreshold,
            ("CpuThreshold" Core..=) Core.<$> cpuThreshold,
            ("MemoryThreshold" Core..=) Core.<$> memoryThreshold,
            ("Alarms" Core..=) Core.<$> alarms,
            ("IgnoreMetricsTime" Core..=)
              Core.<$> ignoreMetricsTime,
            ("ThresholdsWaitTime" Core..=)
              Core.<$> thresholdsWaitTime,
            ("InstanceCount" Core..=) Core.<$> instanceCount
          ]
      )
