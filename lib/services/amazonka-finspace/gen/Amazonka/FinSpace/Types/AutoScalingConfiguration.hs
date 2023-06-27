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
-- Module      : Amazonka.FinSpace.Types.AutoScalingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpace.Types.AutoScalingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpace.Types.AutoScalingMetric
import qualified Amazonka.Prelude as Prelude

-- | The configuration based on which FinSpace will scale in or scale out
-- nodes in your cluster.
--
-- /See:/ 'newAutoScalingConfiguration' smart constructor.
data AutoScalingConfiguration = AutoScalingConfiguration'
  { -- | The metric your cluster will track in order to scale in and out. For
    -- example, @CPU_UTILIZATION_PERCENTAGE@ is the average CPU usage across
    -- all the nodes in a cluster.
    autoScalingMetric :: Prelude.Maybe AutoScalingMetric,
    -- | The highest number of nodes to scale. This value cannot be greater than
    -- 5.
    maxNodeCount :: Prelude.Maybe Prelude.Natural,
    -- | The desired value of the chosen @autoScalingMetric@. When the metric
    -- drops below this value, the cluster will scale in. When the metric goes
    -- above this value, the cluster will scale out. You can set the target
    -- value between 1 and 100 percent.
    metricTarget :: Prelude.Maybe Prelude.Double,
    -- | The lowest number of nodes to scale. This value must be at least 1 and
    -- less than the @maxNodeCount@. If the nodes in a cluster belong to
    -- multiple availability zones, then @minNodeCount@ must be at least 3.
    minNodeCount :: Prelude.Maybe Prelude.Natural,
    -- | The duration in seconds that FinSpace will wait after a scale in event
    -- before initiating another scaling event.
    scaleInCooldownSeconds :: Prelude.Maybe Prelude.Double,
    -- | The duration in seconds that FinSpace will wait after a scale out event
    -- before initiating another scaling event.
    scaleOutCooldownSeconds :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoScalingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoScalingMetric', 'autoScalingConfiguration_autoScalingMetric' - The metric your cluster will track in order to scale in and out. For
-- example, @CPU_UTILIZATION_PERCENTAGE@ is the average CPU usage across
-- all the nodes in a cluster.
--
-- 'maxNodeCount', 'autoScalingConfiguration_maxNodeCount' - The highest number of nodes to scale. This value cannot be greater than
-- 5.
--
-- 'metricTarget', 'autoScalingConfiguration_metricTarget' - The desired value of the chosen @autoScalingMetric@. When the metric
-- drops below this value, the cluster will scale in. When the metric goes
-- above this value, the cluster will scale out. You can set the target
-- value between 1 and 100 percent.
--
-- 'minNodeCount', 'autoScalingConfiguration_minNodeCount' - The lowest number of nodes to scale. This value must be at least 1 and
-- less than the @maxNodeCount@. If the nodes in a cluster belong to
-- multiple availability zones, then @minNodeCount@ must be at least 3.
--
-- 'scaleInCooldownSeconds', 'autoScalingConfiguration_scaleInCooldownSeconds' - The duration in seconds that FinSpace will wait after a scale in event
-- before initiating another scaling event.
--
-- 'scaleOutCooldownSeconds', 'autoScalingConfiguration_scaleOutCooldownSeconds' - The duration in seconds that FinSpace will wait after a scale out event
-- before initiating another scaling event.
newAutoScalingConfiguration ::
  AutoScalingConfiguration
newAutoScalingConfiguration =
  AutoScalingConfiguration'
    { autoScalingMetric =
        Prelude.Nothing,
      maxNodeCount = Prelude.Nothing,
      metricTarget = Prelude.Nothing,
      minNodeCount = Prelude.Nothing,
      scaleInCooldownSeconds = Prelude.Nothing,
      scaleOutCooldownSeconds = Prelude.Nothing
    }

-- | The metric your cluster will track in order to scale in and out. For
-- example, @CPU_UTILIZATION_PERCENTAGE@ is the average CPU usage across
-- all the nodes in a cluster.
autoScalingConfiguration_autoScalingMetric :: Lens.Lens' AutoScalingConfiguration (Prelude.Maybe AutoScalingMetric)
autoScalingConfiguration_autoScalingMetric = Lens.lens (\AutoScalingConfiguration' {autoScalingMetric} -> autoScalingMetric) (\s@AutoScalingConfiguration' {} a -> s {autoScalingMetric = a} :: AutoScalingConfiguration)

-- | The highest number of nodes to scale. This value cannot be greater than
-- 5.
autoScalingConfiguration_maxNodeCount :: Lens.Lens' AutoScalingConfiguration (Prelude.Maybe Prelude.Natural)
autoScalingConfiguration_maxNodeCount = Lens.lens (\AutoScalingConfiguration' {maxNodeCount} -> maxNodeCount) (\s@AutoScalingConfiguration' {} a -> s {maxNodeCount = a} :: AutoScalingConfiguration)

-- | The desired value of the chosen @autoScalingMetric@. When the metric
-- drops below this value, the cluster will scale in. When the metric goes
-- above this value, the cluster will scale out. You can set the target
-- value between 1 and 100 percent.
autoScalingConfiguration_metricTarget :: Lens.Lens' AutoScalingConfiguration (Prelude.Maybe Prelude.Double)
autoScalingConfiguration_metricTarget = Lens.lens (\AutoScalingConfiguration' {metricTarget} -> metricTarget) (\s@AutoScalingConfiguration' {} a -> s {metricTarget = a} :: AutoScalingConfiguration)

-- | The lowest number of nodes to scale. This value must be at least 1 and
-- less than the @maxNodeCount@. If the nodes in a cluster belong to
-- multiple availability zones, then @minNodeCount@ must be at least 3.
autoScalingConfiguration_minNodeCount :: Lens.Lens' AutoScalingConfiguration (Prelude.Maybe Prelude.Natural)
autoScalingConfiguration_minNodeCount = Lens.lens (\AutoScalingConfiguration' {minNodeCount} -> minNodeCount) (\s@AutoScalingConfiguration' {} a -> s {minNodeCount = a} :: AutoScalingConfiguration)

-- | The duration in seconds that FinSpace will wait after a scale in event
-- before initiating another scaling event.
autoScalingConfiguration_scaleInCooldownSeconds :: Lens.Lens' AutoScalingConfiguration (Prelude.Maybe Prelude.Double)
autoScalingConfiguration_scaleInCooldownSeconds = Lens.lens (\AutoScalingConfiguration' {scaleInCooldownSeconds} -> scaleInCooldownSeconds) (\s@AutoScalingConfiguration' {} a -> s {scaleInCooldownSeconds = a} :: AutoScalingConfiguration)

-- | The duration in seconds that FinSpace will wait after a scale out event
-- before initiating another scaling event.
autoScalingConfiguration_scaleOutCooldownSeconds :: Lens.Lens' AutoScalingConfiguration (Prelude.Maybe Prelude.Double)
autoScalingConfiguration_scaleOutCooldownSeconds = Lens.lens (\AutoScalingConfiguration' {scaleOutCooldownSeconds} -> scaleOutCooldownSeconds) (\s@AutoScalingConfiguration' {} a -> s {scaleOutCooldownSeconds = a} :: AutoScalingConfiguration)

instance Data.FromJSON AutoScalingConfiguration where
  parseJSON =
    Data.withObject
      "AutoScalingConfiguration"
      ( \x ->
          AutoScalingConfiguration'
            Prelude.<$> (x Data..:? "autoScalingMetric")
            Prelude.<*> (x Data..:? "maxNodeCount")
            Prelude.<*> (x Data..:? "metricTarget")
            Prelude.<*> (x Data..:? "minNodeCount")
            Prelude.<*> (x Data..:? "scaleInCooldownSeconds")
            Prelude.<*> (x Data..:? "scaleOutCooldownSeconds")
      )

instance Prelude.Hashable AutoScalingConfiguration where
  hashWithSalt _salt AutoScalingConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` autoScalingMetric
      `Prelude.hashWithSalt` maxNodeCount
      `Prelude.hashWithSalt` metricTarget
      `Prelude.hashWithSalt` minNodeCount
      `Prelude.hashWithSalt` scaleInCooldownSeconds
      `Prelude.hashWithSalt` scaleOutCooldownSeconds

instance Prelude.NFData AutoScalingConfiguration where
  rnf AutoScalingConfiguration' {..} =
    Prelude.rnf autoScalingMetric
      `Prelude.seq` Prelude.rnf maxNodeCount
      `Prelude.seq` Prelude.rnf metricTarget
      `Prelude.seq` Prelude.rnf minNodeCount
      `Prelude.seq` Prelude.rnf scaleInCooldownSeconds
      `Prelude.seq` Prelude.rnf scaleOutCooldownSeconds

instance Data.ToJSON AutoScalingConfiguration where
  toJSON AutoScalingConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("autoScalingMetric" Data..=)
              Prelude.<$> autoScalingMetric,
            ("maxNodeCount" Data..=) Prelude.<$> maxNodeCount,
            ("metricTarget" Data..=) Prelude.<$> metricTarget,
            ("minNodeCount" Data..=) Prelude.<$> minNodeCount,
            ("scaleInCooldownSeconds" Data..=)
              Prelude.<$> scaleInCooldownSeconds,
            ("scaleOutCooldownSeconds" Data..=)
              Prelude.<$> scaleOutCooldownSeconds
          ]
      )
