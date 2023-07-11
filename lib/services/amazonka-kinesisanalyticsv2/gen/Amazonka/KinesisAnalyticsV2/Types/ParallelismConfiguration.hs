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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.ParallelismConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.ParallelismConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types.ConfigurationType
import qualified Amazonka.Prelude as Prelude

-- | Describes parameters for how a Flink-based Kinesis Data Analytics
-- application executes multiple tasks simultaneously. For more information
-- about parallelism, see
-- <https://ci.apache.org/projects/flink/flink-docs-release-1.8/dev/parallel.html Parallel Execution>
-- in the
-- <https://ci.apache.org/projects/flink/flink-docs-release-1.8/ Apache Flink Documentation>.
--
-- /See:/ 'newParallelismConfiguration' smart constructor.
data ParallelismConfiguration = ParallelismConfiguration'
  { -- | Describes whether the Kinesis Data Analytics service can increase the
    -- parallelism of the application in response to increased throughput.
    autoScalingEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Describes the initial number of parallel tasks that a Flink-based
    -- Kinesis Data Analytics application can perform. If @AutoScalingEnabled@
    -- is set to True, Kinesis Data Analytics increases the
    -- @CurrentParallelism@ value in response to application load. The service
    -- can increase the @CurrentParallelism@ value up to the maximum
    -- parallelism, which is @ParalellismPerKPU@ times the maximum KPUs for the
    -- application. The maximum KPUs for an application is 32 by default, and
    -- can be increased by requesting a limit increase. If application load is
    -- reduced, the service can reduce the @CurrentParallelism@ value down to
    -- the @Parallelism@ setting.
    parallelism :: Prelude.Maybe Prelude.Natural,
    -- | Describes the number of parallel tasks that a Flink-based Kinesis Data
    -- Analytics application can perform per Kinesis Processing Unit (KPU) used
    -- by the application. For more information about KPUs, see
    -- <http://aws.amazon.com/kinesis/data-analytics/pricing/ Amazon Kinesis Data Analytics Pricing>.
    parallelismPerKPU :: Prelude.Maybe Prelude.Natural,
    -- | Describes whether the application uses the default parallelism for the
    -- Kinesis Data Analytics service. You must set this property to @CUSTOM@
    -- in order to change your application\'s @AutoScalingEnabled@,
    -- @Parallelism@, or @ParallelismPerKPU@ properties.
    configurationType :: ConfigurationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParallelismConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoScalingEnabled', 'parallelismConfiguration_autoScalingEnabled' - Describes whether the Kinesis Data Analytics service can increase the
-- parallelism of the application in response to increased throughput.
--
-- 'parallelism', 'parallelismConfiguration_parallelism' - Describes the initial number of parallel tasks that a Flink-based
-- Kinesis Data Analytics application can perform. If @AutoScalingEnabled@
-- is set to True, Kinesis Data Analytics increases the
-- @CurrentParallelism@ value in response to application load. The service
-- can increase the @CurrentParallelism@ value up to the maximum
-- parallelism, which is @ParalellismPerKPU@ times the maximum KPUs for the
-- application. The maximum KPUs for an application is 32 by default, and
-- can be increased by requesting a limit increase. If application load is
-- reduced, the service can reduce the @CurrentParallelism@ value down to
-- the @Parallelism@ setting.
--
-- 'parallelismPerKPU', 'parallelismConfiguration_parallelismPerKPU' - Describes the number of parallel tasks that a Flink-based Kinesis Data
-- Analytics application can perform per Kinesis Processing Unit (KPU) used
-- by the application. For more information about KPUs, see
-- <http://aws.amazon.com/kinesis/data-analytics/pricing/ Amazon Kinesis Data Analytics Pricing>.
--
-- 'configurationType', 'parallelismConfiguration_configurationType' - Describes whether the application uses the default parallelism for the
-- Kinesis Data Analytics service. You must set this property to @CUSTOM@
-- in order to change your application\'s @AutoScalingEnabled@,
-- @Parallelism@, or @ParallelismPerKPU@ properties.
newParallelismConfiguration ::
  -- | 'configurationType'
  ConfigurationType ->
  ParallelismConfiguration
newParallelismConfiguration pConfigurationType_ =
  ParallelismConfiguration'
    { autoScalingEnabled =
        Prelude.Nothing,
      parallelism = Prelude.Nothing,
      parallelismPerKPU = Prelude.Nothing,
      configurationType = pConfigurationType_
    }

-- | Describes whether the Kinesis Data Analytics service can increase the
-- parallelism of the application in response to increased throughput.
parallelismConfiguration_autoScalingEnabled :: Lens.Lens' ParallelismConfiguration (Prelude.Maybe Prelude.Bool)
parallelismConfiguration_autoScalingEnabled = Lens.lens (\ParallelismConfiguration' {autoScalingEnabled} -> autoScalingEnabled) (\s@ParallelismConfiguration' {} a -> s {autoScalingEnabled = a} :: ParallelismConfiguration)

-- | Describes the initial number of parallel tasks that a Flink-based
-- Kinesis Data Analytics application can perform. If @AutoScalingEnabled@
-- is set to True, Kinesis Data Analytics increases the
-- @CurrentParallelism@ value in response to application load. The service
-- can increase the @CurrentParallelism@ value up to the maximum
-- parallelism, which is @ParalellismPerKPU@ times the maximum KPUs for the
-- application. The maximum KPUs for an application is 32 by default, and
-- can be increased by requesting a limit increase. If application load is
-- reduced, the service can reduce the @CurrentParallelism@ value down to
-- the @Parallelism@ setting.
parallelismConfiguration_parallelism :: Lens.Lens' ParallelismConfiguration (Prelude.Maybe Prelude.Natural)
parallelismConfiguration_parallelism = Lens.lens (\ParallelismConfiguration' {parallelism} -> parallelism) (\s@ParallelismConfiguration' {} a -> s {parallelism = a} :: ParallelismConfiguration)

-- | Describes the number of parallel tasks that a Flink-based Kinesis Data
-- Analytics application can perform per Kinesis Processing Unit (KPU) used
-- by the application. For more information about KPUs, see
-- <http://aws.amazon.com/kinesis/data-analytics/pricing/ Amazon Kinesis Data Analytics Pricing>.
parallelismConfiguration_parallelismPerKPU :: Lens.Lens' ParallelismConfiguration (Prelude.Maybe Prelude.Natural)
parallelismConfiguration_parallelismPerKPU = Lens.lens (\ParallelismConfiguration' {parallelismPerKPU} -> parallelismPerKPU) (\s@ParallelismConfiguration' {} a -> s {parallelismPerKPU = a} :: ParallelismConfiguration)

-- | Describes whether the application uses the default parallelism for the
-- Kinesis Data Analytics service. You must set this property to @CUSTOM@
-- in order to change your application\'s @AutoScalingEnabled@,
-- @Parallelism@, or @ParallelismPerKPU@ properties.
parallelismConfiguration_configurationType :: Lens.Lens' ParallelismConfiguration ConfigurationType
parallelismConfiguration_configurationType = Lens.lens (\ParallelismConfiguration' {configurationType} -> configurationType) (\s@ParallelismConfiguration' {} a -> s {configurationType = a} :: ParallelismConfiguration)

instance Prelude.Hashable ParallelismConfiguration where
  hashWithSalt _salt ParallelismConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` autoScalingEnabled
      `Prelude.hashWithSalt` parallelism
      `Prelude.hashWithSalt` parallelismPerKPU
      `Prelude.hashWithSalt` configurationType

instance Prelude.NFData ParallelismConfiguration where
  rnf ParallelismConfiguration' {..} =
    Prelude.rnf autoScalingEnabled
      `Prelude.seq` Prelude.rnf parallelism
      `Prelude.seq` Prelude.rnf parallelismPerKPU
      `Prelude.seq` Prelude.rnf configurationType

instance Data.ToJSON ParallelismConfiguration where
  toJSON ParallelismConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AutoScalingEnabled" Data..=)
              Prelude.<$> autoScalingEnabled,
            ("Parallelism" Data..=) Prelude.<$> parallelism,
            ("ParallelismPerKPU" Data..=)
              Prelude.<$> parallelismPerKPU,
            Prelude.Just
              ("ConfigurationType" Data..= configurationType)
          ]
      )
