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
-- Module      : Amazonka.IoTThingsGraph.Types.MetricsConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTThingsGraph.Types.MetricsConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that specifies whether cloud metrics are collected in a
-- deployment and, if so, what role is used to collect metrics.
--
-- /See:/ 'newMetricsConfiguration' smart constructor.
data MetricsConfiguration = MetricsConfiguration'
  { -- | A Boolean that specifies whether cloud metrics are collected.
    cloudMetricEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the role that is used to collect cloud metrics.
    metricRuleRoleArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricsConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudMetricEnabled', 'metricsConfiguration_cloudMetricEnabled' - A Boolean that specifies whether cloud metrics are collected.
--
-- 'metricRuleRoleArn', 'metricsConfiguration_metricRuleRoleArn' - The ARN of the role that is used to collect cloud metrics.
newMetricsConfiguration ::
  MetricsConfiguration
newMetricsConfiguration =
  MetricsConfiguration'
    { cloudMetricEnabled =
        Prelude.Nothing,
      metricRuleRoleArn = Prelude.Nothing
    }

-- | A Boolean that specifies whether cloud metrics are collected.
metricsConfiguration_cloudMetricEnabled :: Lens.Lens' MetricsConfiguration (Prelude.Maybe Prelude.Bool)
metricsConfiguration_cloudMetricEnabled = Lens.lens (\MetricsConfiguration' {cloudMetricEnabled} -> cloudMetricEnabled) (\s@MetricsConfiguration' {} a -> s {cloudMetricEnabled = a} :: MetricsConfiguration)

-- | The ARN of the role that is used to collect cloud metrics.
metricsConfiguration_metricRuleRoleArn :: Lens.Lens' MetricsConfiguration (Prelude.Maybe Prelude.Text)
metricsConfiguration_metricRuleRoleArn = Lens.lens (\MetricsConfiguration' {metricRuleRoleArn} -> metricRuleRoleArn) (\s@MetricsConfiguration' {} a -> s {metricRuleRoleArn = a} :: MetricsConfiguration)

instance Core.FromJSON MetricsConfiguration where
  parseJSON =
    Core.withObject
      "MetricsConfiguration"
      ( \x ->
          MetricsConfiguration'
            Prelude.<$> (x Core..:? "cloudMetricEnabled")
            Prelude.<*> (x Core..:? "metricRuleRoleArn")
      )

instance Prelude.Hashable MetricsConfiguration where
  hashWithSalt salt' MetricsConfiguration' {..} =
    salt' `Prelude.hashWithSalt` metricRuleRoleArn
      `Prelude.hashWithSalt` cloudMetricEnabled

instance Prelude.NFData MetricsConfiguration where
  rnf MetricsConfiguration' {..} =
    Prelude.rnf cloudMetricEnabled
      `Prelude.seq` Prelude.rnf metricRuleRoleArn

instance Core.ToJSON MetricsConfiguration where
  toJSON MetricsConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("cloudMetricEnabled" Core..=)
              Prelude.<$> cloudMetricEnabled,
            ("metricRuleRoleArn" Core..=)
              Prelude.<$> metricRuleRoleArn
          ]
      )
