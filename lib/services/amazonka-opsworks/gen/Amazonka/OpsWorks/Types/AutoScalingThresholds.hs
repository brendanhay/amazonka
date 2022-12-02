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
-- Module      : Amazonka.OpsWorks.Types.AutoScalingThresholds
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types.AutoScalingThresholds where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a load-based auto scaling upscaling or downscaling threshold
-- configuration, which specifies when AWS OpsWorks Stacks starts or stops
-- load-based instances.
--
-- /See:/ 'newAutoScalingThresholds' smart constructor.
data AutoScalingThresholds = AutoScalingThresholds'
  { -- | The amount of time (in minutes) after a scaling event occurs that AWS
    -- OpsWorks Stacks should ignore metrics and suppress additional scaling
    -- events. For example, AWS OpsWorks Stacks adds new instances following an
    -- upscaling event but the instances won\'t start reducing the load until
    -- they have been booted and configured. There is no point in raising
    -- additional scaling events during that operation, which typically takes
    -- several minutes. @IgnoreMetricsTime@ allows you to direct AWS OpsWorks
    -- Stacks to suppress scaling events long enough to get the new instances
    -- online.
    ignoreMetricsTime :: Prelude.Maybe Prelude.Natural,
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
    -- | The memory utilization threshold, as a percent of the available memory.
    -- A value of -1 disables the threshold.
    memoryThreshold :: Prelude.Maybe Prelude.Double,
    -- | The number of instances to add or remove when the load exceeds a
    -- threshold.
    instanceCount :: Prelude.Maybe Prelude.Int,
    -- | The load threshold. A value of -1 disables the threshold. For more
    -- information about how load is computed, see
    -- <http://en.wikipedia.org/wiki/Load_%28computing%29 Load (computing)>.
    loadThreshold :: Prelude.Maybe Prelude.Double,
    -- | The amount of time, in minutes, that the load must exceed a threshold
    -- before more instances are added or removed.
    thresholdsWaitTime :: Prelude.Maybe Prelude.Natural,
    -- | The CPU utilization threshold, as a percent of the available CPU. A
    -- value of -1 disables the threshold.
    cpuThreshold :: Prelude.Maybe Prelude.Double
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
-- 'memoryThreshold', 'autoScalingThresholds_memoryThreshold' - The memory utilization threshold, as a percent of the available memory.
-- A value of -1 disables the threshold.
--
-- 'instanceCount', 'autoScalingThresholds_instanceCount' - The number of instances to add or remove when the load exceeds a
-- threshold.
--
-- 'loadThreshold', 'autoScalingThresholds_loadThreshold' - The load threshold. A value of -1 disables the threshold. For more
-- information about how load is computed, see
-- <http://en.wikipedia.org/wiki/Load_%28computing%29 Load (computing)>.
--
-- 'thresholdsWaitTime', 'autoScalingThresholds_thresholdsWaitTime' - The amount of time, in minutes, that the load must exceed a threshold
-- before more instances are added or removed.
--
-- 'cpuThreshold', 'autoScalingThresholds_cpuThreshold' - The CPU utilization threshold, as a percent of the available CPU. A
-- value of -1 disables the threshold.
newAutoScalingThresholds ::
  AutoScalingThresholds
newAutoScalingThresholds =
  AutoScalingThresholds'
    { ignoreMetricsTime =
        Prelude.Nothing,
      alarms = Prelude.Nothing,
      memoryThreshold = Prelude.Nothing,
      instanceCount = Prelude.Nothing,
      loadThreshold = Prelude.Nothing,
      thresholdsWaitTime = Prelude.Nothing,
      cpuThreshold = Prelude.Nothing
    }

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
autoScalingThresholds_alarms = Lens.lens (\AutoScalingThresholds' {alarms} -> alarms) (\s@AutoScalingThresholds' {} a -> s {alarms = a} :: AutoScalingThresholds) Prelude.. Lens.mapping Lens.coerced

-- | The memory utilization threshold, as a percent of the available memory.
-- A value of -1 disables the threshold.
autoScalingThresholds_memoryThreshold :: Lens.Lens' AutoScalingThresholds (Prelude.Maybe Prelude.Double)
autoScalingThresholds_memoryThreshold = Lens.lens (\AutoScalingThresholds' {memoryThreshold} -> memoryThreshold) (\s@AutoScalingThresholds' {} a -> s {memoryThreshold = a} :: AutoScalingThresholds)

-- | The number of instances to add or remove when the load exceeds a
-- threshold.
autoScalingThresholds_instanceCount :: Lens.Lens' AutoScalingThresholds (Prelude.Maybe Prelude.Int)
autoScalingThresholds_instanceCount = Lens.lens (\AutoScalingThresholds' {instanceCount} -> instanceCount) (\s@AutoScalingThresholds' {} a -> s {instanceCount = a} :: AutoScalingThresholds)

-- | The load threshold. A value of -1 disables the threshold. For more
-- information about how load is computed, see
-- <http://en.wikipedia.org/wiki/Load_%28computing%29 Load (computing)>.
autoScalingThresholds_loadThreshold :: Lens.Lens' AutoScalingThresholds (Prelude.Maybe Prelude.Double)
autoScalingThresholds_loadThreshold = Lens.lens (\AutoScalingThresholds' {loadThreshold} -> loadThreshold) (\s@AutoScalingThresholds' {} a -> s {loadThreshold = a} :: AutoScalingThresholds)

-- | The amount of time, in minutes, that the load must exceed a threshold
-- before more instances are added or removed.
autoScalingThresholds_thresholdsWaitTime :: Lens.Lens' AutoScalingThresholds (Prelude.Maybe Prelude.Natural)
autoScalingThresholds_thresholdsWaitTime = Lens.lens (\AutoScalingThresholds' {thresholdsWaitTime} -> thresholdsWaitTime) (\s@AutoScalingThresholds' {} a -> s {thresholdsWaitTime = a} :: AutoScalingThresholds)

-- | The CPU utilization threshold, as a percent of the available CPU. A
-- value of -1 disables the threshold.
autoScalingThresholds_cpuThreshold :: Lens.Lens' AutoScalingThresholds (Prelude.Maybe Prelude.Double)
autoScalingThresholds_cpuThreshold = Lens.lens (\AutoScalingThresholds' {cpuThreshold} -> cpuThreshold) (\s@AutoScalingThresholds' {} a -> s {cpuThreshold = a} :: AutoScalingThresholds)

instance Data.FromJSON AutoScalingThresholds where
  parseJSON =
    Data.withObject
      "AutoScalingThresholds"
      ( \x ->
          AutoScalingThresholds'
            Prelude.<$> (x Data..:? "IgnoreMetricsTime")
            Prelude.<*> (x Data..:? "Alarms" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "MemoryThreshold")
            Prelude.<*> (x Data..:? "InstanceCount")
            Prelude.<*> (x Data..:? "LoadThreshold")
            Prelude.<*> (x Data..:? "ThresholdsWaitTime")
            Prelude.<*> (x Data..:? "CpuThreshold")
      )

instance Prelude.Hashable AutoScalingThresholds where
  hashWithSalt _salt AutoScalingThresholds' {..} =
    _salt `Prelude.hashWithSalt` ignoreMetricsTime
      `Prelude.hashWithSalt` alarms
      `Prelude.hashWithSalt` memoryThreshold
      `Prelude.hashWithSalt` instanceCount
      `Prelude.hashWithSalt` loadThreshold
      `Prelude.hashWithSalt` thresholdsWaitTime
      `Prelude.hashWithSalt` cpuThreshold

instance Prelude.NFData AutoScalingThresholds where
  rnf AutoScalingThresholds' {..} =
    Prelude.rnf ignoreMetricsTime
      `Prelude.seq` Prelude.rnf alarms
      `Prelude.seq` Prelude.rnf memoryThreshold
      `Prelude.seq` Prelude.rnf instanceCount
      `Prelude.seq` Prelude.rnf loadThreshold
      `Prelude.seq` Prelude.rnf thresholdsWaitTime
      `Prelude.seq` Prelude.rnf cpuThreshold

instance Data.ToJSON AutoScalingThresholds where
  toJSON AutoScalingThresholds' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IgnoreMetricsTime" Data..=)
              Prelude.<$> ignoreMetricsTime,
            ("Alarms" Data..=) Prelude.<$> alarms,
            ("MemoryThreshold" Data..=)
              Prelude.<$> memoryThreshold,
            ("InstanceCount" Data..=) Prelude.<$> instanceCount,
            ("LoadThreshold" Data..=) Prelude.<$> loadThreshold,
            ("ThresholdsWaitTime" Data..=)
              Prelude.<$> thresholdsWaitTime,
            ("CpuThreshold" Data..=) Prelude.<$> cpuThreshold
          ]
      )
