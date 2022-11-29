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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.ParallelismConfigurationDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.ParallelismConfigurationDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KinesisAnalyticsV2.Types.ConfigurationType
import qualified Amazonka.Prelude as Prelude

-- | Describes parameters for how a Flink-based Kinesis Data Analytics
-- application executes multiple tasks simultaneously.
--
-- /See:/ 'newParallelismConfigurationDescription' smart constructor.
data ParallelismConfigurationDescription = ParallelismConfigurationDescription'
  { -- | Describes the number of parallel tasks that a Flink-based Kinesis Data
    -- Analytics application can perform per Kinesis Processing Unit (KPU) used
    -- by the application.
    parallelismPerKPU :: Prelude.Maybe Prelude.Natural,
    -- | Describes the current number of parallel tasks that a Flink-based
    -- Kinesis Data Analytics application can perform. If @AutoScalingEnabled@
    -- is set to True, Kinesis Data Analytics can increase this value in
    -- response to application load. The service can increase this value up to
    -- the maximum parallelism, which is @ParalellismPerKPU@ times the maximum
    -- KPUs for the application. The maximum KPUs for an application is 32 by
    -- default, and can be increased by requesting a limit increase. If
    -- application load is reduced, the service can reduce the
    -- @CurrentParallelism@ value down to the @Parallelism@ setting.
    currentParallelism :: Prelude.Maybe Prelude.Natural,
    -- | Describes whether the Kinesis Data Analytics service can increase the
    -- parallelism of the application in response to increased throughput.
    autoScalingEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Describes the initial number of parallel tasks that a Flink-based
    -- Kinesis Data Analytics application can perform. If @AutoScalingEnabled@
    -- is set to True, then Kinesis Data Analytics can increase the
    -- @CurrentParallelism@ value in response to application load. The service
    -- can increase @CurrentParallelism@ up to the maximum parallelism, which
    -- is @ParalellismPerKPU@ times the maximum KPUs for the application. The
    -- maximum KPUs for an application is 32 by default, and can be increased
    -- by requesting a limit increase. If application load is reduced, the
    -- service can reduce the @CurrentParallelism@ value down to the
    -- @Parallelism@ setting.
    parallelism :: Prelude.Maybe Prelude.Natural,
    -- | Describes whether the application uses the default parallelism for the
    -- Kinesis Data Analytics service.
    configurationType :: Prelude.Maybe ConfigurationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParallelismConfigurationDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parallelismPerKPU', 'parallelismConfigurationDescription_parallelismPerKPU' - Describes the number of parallel tasks that a Flink-based Kinesis Data
-- Analytics application can perform per Kinesis Processing Unit (KPU) used
-- by the application.
--
-- 'currentParallelism', 'parallelismConfigurationDescription_currentParallelism' - Describes the current number of parallel tasks that a Flink-based
-- Kinesis Data Analytics application can perform. If @AutoScalingEnabled@
-- is set to True, Kinesis Data Analytics can increase this value in
-- response to application load. The service can increase this value up to
-- the maximum parallelism, which is @ParalellismPerKPU@ times the maximum
-- KPUs for the application. The maximum KPUs for an application is 32 by
-- default, and can be increased by requesting a limit increase. If
-- application load is reduced, the service can reduce the
-- @CurrentParallelism@ value down to the @Parallelism@ setting.
--
-- 'autoScalingEnabled', 'parallelismConfigurationDescription_autoScalingEnabled' - Describes whether the Kinesis Data Analytics service can increase the
-- parallelism of the application in response to increased throughput.
--
-- 'parallelism', 'parallelismConfigurationDescription_parallelism' - Describes the initial number of parallel tasks that a Flink-based
-- Kinesis Data Analytics application can perform. If @AutoScalingEnabled@
-- is set to True, then Kinesis Data Analytics can increase the
-- @CurrentParallelism@ value in response to application load. The service
-- can increase @CurrentParallelism@ up to the maximum parallelism, which
-- is @ParalellismPerKPU@ times the maximum KPUs for the application. The
-- maximum KPUs for an application is 32 by default, and can be increased
-- by requesting a limit increase. If application load is reduced, the
-- service can reduce the @CurrentParallelism@ value down to the
-- @Parallelism@ setting.
--
-- 'configurationType', 'parallelismConfigurationDescription_configurationType' - Describes whether the application uses the default parallelism for the
-- Kinesis Data Analytics service.
newParallelismConfigurationDescription ::
  ParallelismConfigurationDescription
newParallelismConfigurationDescription =
  ParallelismConfigurationDescription'
    { parallelismPerKPU =
        Prelude.Nothing,
      currentParallelism = Prelude.Nothing,
      autoScalingEnabled = Prelude.Nothing,
      parallelism = Prelude.Nothing,
      configurationType = Prelude.Nothing
    }

-- | Describes the number of parallel tasks that a Flink-based Kinesis Data
-- Analytics application can perform per Kinesis Processing Unit (KPU) used
-- by the application.
parallelismConfigurationDescription_parallelismPerKPU :: Lens.Lens' ParallelismConfigurationDescription (Prelude.Maybe Prelude.Natural)
parallelismConfigurationDescription_parallelismPerKPU = Lens.lens (\ParallelismConfigurationDescription' {parallelismPerKPU} -> parallelismPerKPU) (\s@ParallelismConfigurationDescription' {} a -> s {parallelismPerKPU = a} :: ParallelismConfigurationDescription)

-- | Describes the current number of parallel tasks that a Flink-based
-- Kinesis Data Analytics application can perform. If @AutoScalingEnabled@
-- is set to True, Kinesis Data Analytics can increase this value in
-- response to application load. The service can increase this value up to
-- the maximum parallelism, which is @ParalellismPerKPU@ times the maximum
-- KPUs for the application. The maximum KPUs for an application is 32 by
-- default, and can be increased by requesting a limit increase. If
-- application load is reduced, the service can reduce the
-- @CurrentParallelism@ value down to the @Parallelism@ setting.
parallelismConfigurationDescription_currentParallelism :: Lens.Lens' ParallelismConfigurationDescription (Prelude.Maybe Prelude.Natural)
parallelismConfigurationDescription_currentParallelism = Lens.lens (\ParallelismConfigurationDescription' {currentParallelism} -> currentParallelism) (\s@ParallelismConfigurationDescription' {} a -> s {currentParallelism = a} :: ParallelismConfigurationDescription)

-- | Describes whether the Kinesis Data Analytics service can increase the
-- parallelism of the application in response to increased throughput.
parallelismConfigurationDescription_autoScalingEnabled :: Lens.Lens' ParallelismConfigurationDescription (Prelude.Maybe Prelude.Bool)
parallelismConfigurationDescription_autoScalingEnabled = Lens.lens (\ParallelismConfigurationDescription' {autoScalingEnabled} -> autoScalingEnabled) (\s@ParallelismConfigurationDescription' {} a -> s {autoScalingEnabled = a} :: ParallelismConfigurationDescription)

-- | Describes the initial number of parallel tasks that a Flink-based
-- Kinesis Data Analytics application can perform. If @AutoScalingEnabled@
-- is set to True, then Kinesis Data Analytics can increase the
-- @CurrentParallelism@ value in response to application load. The service
-- can increase @CurrentParallelism@ up to the maximum parallelism, which
-- is @ParalellismPerKPU@ times the maximum KPUs for the application. The
-- maximum KPUs for an application is 32 by default, and can be increased
-- by requesting a limit increase. If application load is reduced, the
-- service can reduce the @CurrentParallelism@ value down to the
-- @Parallelism@ setting.
parallelismConfigurationDescription_parallelism :: Lens.Lens' ParallelismConfigurationDescription (Prelude.Maybe Prelude.Natural)
parallelismConfigurationDescription_parallelism = Lens.lens (\ParallelismConfigurationDescription' {parallelism} -> parallelism) (\s@ParallelismConfigurationDescription' {} a -> s {parallelism = a} :: ParallelismConfigurationDescription)

-- | Describes whether the application uses the default parallelism for the
-- Kinesis Data Analytics service.
parallelismConfigurationDescription_configurationType :: Lens.Lens' ParallelismConfigurationDescription (Prelude.Maybe ConfigurationType)
parallelismConfigurationDescription_configurationType = Lens.lens (\ParallelismConfigurationDescription' {configurationType} -> configurationType) (\s@ParallelismConfigurationDescription' {} a -> s {configurationType = a} :: ParallelismConfigurationDescription)

instance
  Core.FromJSON
    ParallelismConfigurationDescription
  where
  parseJSON =
    Core.withObject
      "ParallelismConfigurationDescription"
      ( \x ->
          ParallelismConfigurationDescription'
            Prelude.<$> (x Core..:? "ParallelismPerKPU")
            Prelude.<*> (x Core..:? "CurrentParallelism")
            Prelude.<*> (x Core..:? "AutoScalingEnabled")
            Prelude.<*> (x Core..:? "Parallelism")
            Prelude.<*> (x Core..:? "ConfigurationType")
      )

instance
  Prelude.Hashable
    ParallelismConfigurationDescription
  where
  hashWithSalt
    _salt
    ParallelismConfigurationDescription' {..} =
      _salt `Prelude.hashWithSalt` parallelismPerKPU
        `Prelude.hashWithSalt` currentParallelism
        `Prelude.hashWithSalt` autoScalingEnabled
        `Prelude.hashWithSalt` parallelism
        `Prelude.hashWithSalt` configurationType

instance
  Prelude.NFData
    ParallelismConfigurationDescription
  where
  rnf ParallelismConfigurationDescription' {..} =
    Prelude.rnf parallelismPerKPU
      `Prelude.seq` Prelude.rnf currentParallelism
      `Prelude.seq` Prelude.rnf autoScalingEnabled
      `Prelude.seq` Prelude.rnf parallelism
      `Prelude.seq` Prelude.rnf configurationType
