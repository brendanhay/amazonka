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
-- Module      : Amazonka.Evidently.Types.MetricMonitorConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.MetricMonitorConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Evidently.Types.MetricDefinitionConfig
import qualified Amazonka.Prelude as Prelude

-- | A structure that defines a metric to be used to monitor performance of
-- the variations during a launch.
--
-- /See:/ 'newMetricMonitorConfig' smart constructor.
data MetricMonitorConfig = MetricMonitorConfig'
  { -- | A structure that defines the metric.
    metricDefinition :: MetricDefinitionConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricMonitorConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricDefinition', 'metricMonitorConfig_metricDefinition' - A structure that defines the metric.
newMetricMonitorConfig ::
  -- | 'metricDefinition'
  MetricDefinitionConfig ->
  MetricMonitorConfig
newMetricMonitorConfig pMetricDefinition_ =
  MetricMonitorConfig'
    { metricDefinition =
        pMetricDefinition_
    }

-- | A structure that defines the metric.
metricMonitorConfig_metricDefinition :: Lens.Lens' MetricMonitorConfig MetricDefinitionConfig
metricMonitorConfig_metricDefinition = Lens.lens (\MetricMonitorConfig' {metricDefinition} -> metricDefinition) (\s@MetricMonitorConfig' {} a -> s {metricDefinition = a} :: MetricMonitorConfig)

instance Prelude.Hashable MetricMonitorConfig where
  hashWithSalt _salt MetricMonitorConfig' {..} =
    _salt `Prelude.hashWithSalt` metricDefinition

instance Prelude.NFData MetricMonitorConfig where
  rnf MetricMonitorConfig' {..} =
    Prelude.rnf metricDefinition

instance Data.ToJSON MetricMonitorConfig where
  toJSON MetricMonitorConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("metricDefinition" Data..= metricDefinition)
          ]
      )
