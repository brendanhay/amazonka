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
-- Module      : Amazonka.ComputeOptimizer.Types.ProjectedMetric
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.ProjectedMetric where

import Amazonka.ComputeOptimizer.Types.MetricName
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a projected utilization metric of a recommendation option,
-- such as an Amazon EC2 instance. This represents the projected
-- utilization of a recommendation option had you used that resource during
-- the analyzed period.
--
-- Compare the utilization metric data of your resource against its
-- projected utilization metric data to determine the performance
-- difference between your current resource and the recommended option.
--
-- The @Cpu@ and @Memory@ metrics are the only projected utilization
-- metrics returned when you run the GetEC2RecommendationProjectedMetrics
-- action. Additionally, the @Memory@ metric is returned only for resources
-- that have the unified CloudWatch agent installed on them. For more
-- information, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/metrics.html#cw-agent Enabling Memory Utilization with the CloudWatch Agent>.
--
-- /See:/ 'newProjectedMetric' smart constructor.
data ProjectedMetric = ProjectedMetric'
  { -- | The name of the projected utilization metric.
    --
    -- The following projected utilization metrics are returned:
    --
    -- -   @Cpu@ - The projected percentage of allocated EC2 compute units that
    --     would be in use on the recommendation option had you used that
    --     resource during the analyzed period. This metric identifies the
    --     processing power required to run an application on the
    --     recommendation option.
    --
    --     Depending on the instance type, tools in your operating system can
    --     show a lower percentage than CloudWatch when the instance is not
    --     allocated a full processor core.
    --
    --     Units: Percent
    --
    -- -   @Memory@ - The percentage of memory that would be in use on the
    --     recommendation option had you used that resource during the analyzed
    --     period. This metric identifies the amount of memory required to run
    --     an application on the recommendation option.
    --
    --     Units: Percent
    --
    --     The @Memory@ metric is returned only for resources that have the
    --     unified CloudWatch agent installed on them. For more information,
    --     see
    --     <https://docs.aws.amazon.com/compute-optimizer/latest/ug/metrics.html#cw-agent Enabling Memory Utilization with the CloudWatch Agent>.
    name :: Prelude.Maybe MetricName,
    -- | The timestamps of the projected utilization metric.
    timestamps :: Prelude.Maybe [Data.POSIX],
    -- | The values of the projected utilization metrics.
    values :: Prelude.Maybe [Prelude.Double]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProjectedMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'projectedMetric_name' - The name of the projected utilization metric.
--
-- The following projected utilization metrics are returned:
--
-- -   @Cpu@ - The projected percentage of allocated EC2 compute units that
--     would be in use on the recommendation option had you used that
--     resource during the analyzed period. This metric identifies the
--     processing power required to run an application on the
--     recommendation option.
--
--     Depending on the instance type, tools in your operating system can
--     show a lower percentage than CloudWatch when the instance is not
--     allocated a full processor core.
--
--     Units: Percent
--
-- -   @Memory@ - The percentage of memory that would be in use on the
--     recommendation option had you used that resource during the analyzed
--     period. This metric identifies the amount of memory required to run
--     an application on the recommendation option.
--
--     Units: Percent
--
--     The @Memory@ metric is returned only for resources that have the
--     unified CloudWatch agent installed on them. For more information,
--     see
--     <https://docs.aws.amazon.com/compute-optimizer/latest/ug/metrics.html#cw-agent Enabling Memory Utilization with the CloudWatch Agent>.
--
-- 'timestamps', 'projectedMetric_timestamps' - The timestamps of the projected utilization metric.
--
-- 'values', 'projectedMetric_values' - The values of the projected utilization metrics.
newProjectedMetric ::
  ProjectedMetric
newProjectedMetric =
  ProjectedMetric'
    { name = Prelude.Nothing,
      timestamps = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The name of the projected utilization metric.
--
-- The following projected utilization metrics are returned:
--
-- -   @Cpu@ - The projected percentage of allocated EC2 compute units that
--     would be in use on the recommendation option had you used that
--     resource during the analyzed period. This metric identifies the
--     processing power required to run an application on the
--     recommendation option.
--
--     Depending on the instance type, tools in your operating system can
--     show a lower percentage than CloudWatch when the instance is not
--     allocated a full processor core.
--
--     Units: Percent
--
-- -   @Memory@ - The percentage of memory that would be in use on the
--     recommendation option had you used that resource during the analyzed
--     period. This metric identifies the amount of memory required to run
--     an application on the recommendation option.
--
--     Units: Percent
--
--     The @Memory@ metric is returned only for resources that have the
--     unified CloudWatch agent installed on them. For more information,
--     see
--     <https://docs.aws.amazon.com/compute-optimizer/latest/ug/metrics.html#cw-agent Enabling Memory Utilization with the CloudWatch Agent>.
projectedMetric_name :: Lens.Lens' ProjectedMetric (Prelude.Maybe MetricName)
projectedMetric_name = Lens.lens (\ProjectedMetric' {name} -> name) (\s@ProjectedMetric' {} a -> s {name = a} :: ProjectedMetric)

-- | The timestamps of the projected utilization metric.
projectedMetric_timestamps :: Lens.Lens' ProjectedMetric (Prelude.Maybe [Prelude.UTCTime])
projectedMetric_timestamps = Lens.lens (\ProjectedMetric' {timestamps} -> timestamps) (\s@ProjectedMetric' {} a -> s {timestamps = a} :: ProjectedMetric) Prelude.. Lens.mapping Lens.coerced

-- | The values of the projected utilization metrics.
projectedMetric_values :: Lens.Lens' ProjectedMetric (Prelude.Maybe [Prelude.Double])
projectedMetric_values = Lens.lens (\ProjectedMetric' {values} -> values) (\s@ProjectedMetric' {} a -> s {values = a} :: ProjectedMetric) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ProjectedMetric where
  parseJSON =
    Data.withObject
      "ProjectedMetric"
      ( \x ->
          ProjectedMetric'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "timestamps" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "values" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ProjectedMetric where
  hashWithSalt _salt ProjectedMetric' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` timestamps
      `Prelude.hashWithSalt` values

instance Prelude.NFData ProjectedMetric where
  rnf ProjectedMetric' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf timestamps
      `Prelude.seq` Prelude.rnf values
