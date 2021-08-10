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
import qualified Network.AWS.Prelude as Prelude

-- | Describes a load-based auto scaling upscaling or downscaling threshold
-- configuration, which specifies when AWS OpsWorks Stacks starts or stops
-- load-based instances.
--
-- /See:/ 'newAutoScalingThresholds' smart constructor.
data AutoScalingThresholds = AutoScalingThresholds'
  { -- | The load threshold. A value of -1 disables the threshold. For more
    -- information about how load is computed, see
    -- <http://en.wikipedia.org/wiki/Load_%28computing%29 Load (computing)>.
    loadThreshold :: Prelude.Maybe Prelude.Double,
    -- | The CPU utilization threshold, as a percent of the available CPU. A
    -- value of -1 disables the threshold.
    cpuThreshold :: Prelude.Maybe Prelude.Double,
    -- | The memory utilization threshold, as a percent of the available memory.
    -- A value of -1 disables the threshold.
    memoryThreshold :: Prelude.Maybe Prelude.Double,
    -- | Custom Cloudwatch auto scaling alarms, to be used as thresholds. This
    -- parameter takes a list of up to five alarm names, which are case
    -- sensitive and must be in the same region as the stack.
    --
    -- To use custom alarms, you must update your service role to allow
    -- @cloudwatch:DescribeAlarms@. You can either have AWS OpsWorks Stacks
    -- update the role for you when you first use this feature or you can edit
    -- the role manually. For more information, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-servicerole.html Allowing AWS OpsWorks Stacks to Act on Your Behalf>.
    alarms :: Prelude.Maybe [Prelude.Text],
    -- | The amount of time (in minutes) after a scaling event occurs that AWS
    -- OpsWorks Stacks should ignore metrics and suppress additional scaling
    -- events. For example, AWS OpsWorks Stacks adds new instances following an
    -- upscaling event but the instances won\'t start reducing the load until
    -- they have been booted and configured. There is no point in raising
    -- additional scaling events during that operation, which typically takes
    -- several minutes. @IgnoreMetricsTime@ allows you to direct AWS OpsWorks
    -- Stacks to suppress scaling events long enough to get the new instances
    -- online.
    ignoreMetricsTime :: Prelude.Maybe Prelude.Natural,
    -- | The amount of time, in minutes, that the load must exceed a threshold
    -- before more instances are added or removed.
    thresholdsWaitTime :: Prelude.Maybe Prelude.Natural,
    -- | The number of instances to add or remove when the load exceeds a
    -- threshold.
    instanceCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      cpuThreshold = Prelude.Nothing,
      memoryThreshold = Prelude.Nothing,
      alarms = Prelude.Nothing,
      ignoreMetricsTime = Prelude.Nothing,
      thresholdsWaitTime = Prelude.Nothing,
      instanceCount = Prelude.Nothing
    }

-- | The load threshold. A value of -1 disables the threshold. For more
-- information about how load is computed, see
-- <http://en.wikipedia.org/wiki/Load_%28computing%29 Load (computing)>.
autoScalingThresholds_loadThreshold :: Lens.Lens' AutoScalingThresholds (Prelude.Maybe Prelude.Double)
autoScalingThresholds_loadThreshold = Lens.lens (\AutoScalingThresholds' {loadThreshold} -> loadThreshold) (\s@AutoScalingThresholds' {} a -> s {loadThreshold = a} :: AutoScalingThresholds)

-- | The CPU utilization threshold, as a percent of the available CPU. A
-- value of -1 disables the threshold.
autoScalingThresholds_cpuThreshold :: Lens.Lens' AutoScalingThresholds (Prelude.Maybe Prelude.Double)
autoScalingThresholds_cpuThreshold = Lens.lens (\AutoScalingThresholds' {cpuThreshold} -> cpuThreshold) (\s@AutoScalingThresholds' {} a -> s {cpuThreshold = a} :: AutoScalingThresholds)

-- | The memory utilization threshold, as a percent of the available memory.
-- A value of -1 disables the threshold.
autoScalingThresholds_memoryThreshold :: Lens.Lens' AutoScalingThresholds (Prelude.Maybe Prelude.Double)
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
autoScalingThresholds_alarms :: Lens.Lens' AutoScalingThresholds (Prelude.Maybe [Prelude.Text])
autoScalingThresholds_alarms = Lens.lens (\AutoScalingThresholds' {alarms} -> alarms) (\s@AutoScalingThresholds' {} a -> s {alarms = a} :: AutoScalingThresholds) Prelude.. Lens.mapping Lens._Coerce

-- | The amount of time (in minutes) after a scaling event occurs that AWS
-- OpsWorks Stacks should ignore metrics and suppress additional scaling
-- events. For example, AWS OpsWorks Stacks adds new instances following an
-- upscaling event but the instances won\'t start reducing the load until
-- they have been booted and configured. There is no point in raising
-- additional scaling events during that operation, which typically takes
-- several minutes. @IgnoreMetricsTime@ allows you to direct AWS OpsWorks
-- Stacks to suppress scaling events long enough to get the new instances
-- online.
autoScalingThresholds_ignoreMetricsTime :: Lens.Lens' AutoScalingThresholds (Prelude.Maybe Prelude.Natural)
autoScalingThresholds_ignoreMetricsTime = Lens.lens (\AutoScalingThresholds' {ignoreMetricsTime} -> ignoreMetricsTime) (\s@AutoScalingThresholds' {} a -> s {ignoreMetricsTime = a} :: AutoScalingThresholds)

-- | The amount of time, in minutes, that the load must exceed a threshold
-- before more instances are added or removed.
autoScalingThresholds_thresholdsWaitTime :: Lens.Lens' AutoScalingThresholds (Prelude.Maybe Prelude.Natural)
autoScalingThresholds_thresholdsWaitTime = Lens.lens (\AutoScalingThresholds' {thresholdsWaitTime} -> thresholdsWaitTime) (\s@AutoScalingThresholds' {} a -> s {thresholdsWaitTime = a} :: AutoScalingThresholds)

-- | The number of instances to add or remove when the load exceeds a
-- threshold.
autoScalingThresholds_instanceCount :: Lens.Lens' AutoScalingThresholds (Prelude.Maybe Prelude.Int)
autoScalingThresholds_instanceCount = Lens.lens (\AutoScalingThresholds' {instanceCount} -> instanceCount) (\s@AutoScalingThresholds' {} a -> s {instanceCount = a} :: AutoScalingThresholds)

instance Core.FromJSON AutoScalingThresholds where
  parseJSON =
    Core.withObject
      "AutoScalingThresholds"
      ( \x ->
          AutoScalingThresholds'
            Prelude.<$> (x Core..:? "LoadThreshold")
            Prelude.<*> (x Core..:? "CpuThreshold")
            Prelude.<*> (x Core..:? "MemoryThreshold")
            Prelude.<*> (x Core..:? "Alarms" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "IgnoreMetricsTime")
            Prelude.<*> (x Core..:? "ThresholdsWaitTime")
            Prelude.<*> (x Core..:? "InstanceCount")
      )

instance Prelude.Hashable AutoScalingThresholds

instance Prelude.NFData AutoScalingThresholds

instance Core.ToJSON AutoScalingThresholds where
  toJSON AutoScalingThresholds' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("LoadThreshold" Core..=) Prelude.<$> loadThreshold,
            ("CpuThreshold" Core..=) Prelude.<$> cpuThreshold,
            ("MemoryThreshold" Core..=)
              Prelude.<$> memoryThreshold,
            ("Alarms" Core..=) Prelude.<$> alarms,
            ("IgnoreMetricsTime" Core..=)
              Prelude.<$> ignoreMetricsTime,
            ("ThresholdsWaitTime" Core..=)
              Prelude.<$> thresholdsWaitTime,
            ("InstanceCount" Core..=) Prelude.<$> instanceCount
          ]
      )
