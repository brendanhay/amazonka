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
-- Module      : Network.AWS.KinesisAnalyticsV2.Types.ParallelismConfigurationUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalyticsV2.Types.ParallelismConfigurationUpdate where

import qualified Network.AWS.Core as Core
import Network.AWS.KinesisAnalyticsV2.Types.ConfigurationType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes updates to parameters for how an application executes multiple
-- tasks simultaneously.
--
-- /See:/ 'newParallelismConfigurationUpdate' smart constructor.
data ParallelismConfigurationUpdate = ParallelismConfigurationUpdate'
  { -- | Describes updates to the initial number of parallel tasks an application
    -- can perform. If @AutoScalingEnabled@ is set to True, then Kinesis Data
    -- Analytics can increase the @CurrentParallelism@ value in response to
    -- application load. The service can increase @CurrentParallelism@ up to
    -- the maximum parallelism, which is @ParalellismPerKPU@ times the maximum
    -- KPUs for the application. The maximum KPUs for an application is 32 by
    -- default, and can be increased by requesting a limit increase. If
    -- application load is reduced, the service will reduce
    -- @CurrentParallelism@ down to the @Parallelism@ setting.
    parallelismUpdate :: Prelude.Maybe Prelude.Natural,
    -- | Describes updates to whether the Kinesis Data Analytics service can
    -- increase the parallelism of a Flink-based Kinesis Data Analytics
    -- application in response to increased throughput.
    autoScalingEnabledUpdate :: Prelude.Maybe Prelude.Bool,
    -- | Describes updates to the number of parallel tasks an application can
    -- perform per Kinesis Processing Unit (KPU) used by the application.
    parallelismPerKPUUpdate :: Prelude.Maybe Prelude.Natural,
    -- | Describes updates to whether the application uses the default
    -- parallelism for the Kinesis Data Analytics service, or if a custom
    -- parallelism is used. You must set this property to @CUSTOM@ in order to
    -- change your application\'s @AutoScalingEnabled@, @Parallelism@, or
    -- @ParallelismPerKPU@ properties.
    configurationTypeUpdate :: Prelude.Maybe ConfigurationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParallelismConfigurationUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parallelismUpdate', 'parallelismConfigurationUpdate_parallelismUpdate' - Describes updates to the initial number of parallel tasks an application
-- can perform. If @AutoScalingEnabled@ is set to True, then Kinesis Data
-- Analytics can increase the @CurrentParallelism@ value in response to
-- application load. The service can increase @CurrentParallelism@ up to
-- the maximum parallelism, which is @ParalellismPerKPU@ times the maximum
-- KPUs for the application. The maximum KPUs for an application is 32 by
-- default, and can be increased by requesting a limit increase. If
-- application load is reduced, the service will reduce
-- @CurrentParallelism@ down to the @Parallelism@ setting.
--
-- 'autoScalingEnabledUpdate', 'parallelismConfigurationUpdate_autoScalingEnabledUpdate' - Describes updates to whether the Kinesis Data Analytics service can
-- increase the parallelism of a Flink-based Kinesis Data Analytics
-- application in response to increased throughput.
--
-- 'parallelismPerKPUUpdate', 'parallelismConfigurationUpdate_parallelismPerKPUUpdate' - Describes updates to the number of parallel tasks an application can
-- perform per Kinesis Processing Unit (KPU) used by the application.
--
-- 'configurationTypeUpdate', 'parallelismConfigurationUpdate_configurationTypeUpdate' - Describes updates to whether the application uses the default
-- parallelism for the Kinesis Data Analytics service, or if a custom
-- parallelism is used. You must set this property to @CUSTOM@ in order to
-- change your application\'s @AutoScalingEnabled@, @Parallelism@, or
-- @ParallelismPerKPU@ properties.
newParallelismConfigurationUpdate ::
  ParallelismConfigurationUpdate
newParallelismConfigurationUpdate =
  ParallelismConfigurationUpdate'
    { parallelismUpdate =
        Prelude.Nothing,
      autoScalingEnabledUpdate = Prelude.Nothing,
      parallelismPerKPUUpdate = Prelude.Nothing,
      configurationTypeUpdate = Prelude.Nothing
    }

-- | Describes updates to the initial number of parallel tasks an application
-- can perform. If @AutoScalingEnabled@ is set to True, then Kinesis Data
-- Analytics can increase the @CurrentParallelism@ value in response to
-- application load. The service can increase @CurrentParallelism@ up to
-- the maximum parallelism, which is @ParalellismPerKPU@ times the maximum
-- KPUs for the application. The maximum KPUs for an application is 32 by
-- default, and can be increased by requesting a limit increase. If
-- application load is reduced, the service will reduce
-- @CurrentParallelism@ down to the @Parallelism@ setting.
parallelismConfigurationUpdate_parallelismUpdate :: Lens.Lens' ParallelismConfigurationUpdate (Prelude.Maybe Prelude.Natural)
parallelismConfigurationUpdate_parallelismUpdate = Lens.lens (\ParallelismConfigurationUpdate' {parallelismUpdate} -> parallelismUpdate) (\s@ParallelismConfigurationUpdate' {} a -> s {parallelismUpdate = a} :: ParallelismConfigurationUpdate)

-- | Describes updates to whether the Kinesis Data Analytics service can
-- increase the parallelism of a Flink-based Kinesis Data Analytics
-- application in response to increased throughput.
parallelismConfigurationUpdate_autoScalingEnabledUpdate :: Lens.Lens' ParallelismConfigurationUpdate (Prelude.Maybe Prelude.Bool)
parallelismConfigurationUpdate_autoScalingEnabledUpdate = Lens.lens (\ParallelismConfigurationUpdate' {autoScalingEnabledUpdate} -> autoScalingEnabledUpdate) (\s@ParallelismConfigurationUpdate' {} a -> s {autoScalingEnabledUpdate = a} :: ParallelismConfigurationUpdate)

-- | Describes updates to the number of parallel tasks an application can
-- perform per Kinesis Processing Unit (KPU) used by the application.
parallelismConfigurationUpdate_parallelismPerKPUUpdate :: Lens.Lens' ParallelismConfigurationUpdate (Prelude.Maybe Prelude.Natural)
parallelismConfigurationUpdate_parallelismPerKPUUpdate = Lens.lens (\ParallelismConfigurationUpdate' {parallelismPerKPUUpdate} -> parallelismPerKPUUpdate) (\s@ParallelismConfigurationUpdate' {} a -> s {parallelismPerKPUUpdate = a} :: ParallelismConfigurationUpdate)

-- | Describes updates to whether the application uses the default
-- parallelism for the Kinesis Data Analytics service, or if a custom
-- parallelism is used. You must set this property to @CUSTOM@ in order to
-- change your application\'s @AutoScalingEnabled@, @Parallelism@, or
-- @ParallelismPerKPU@ properties.
parallelismConfigurationUpdate_configurationTypeUpdate :: Lens.Lens' ParallelismConfigurationUpdate (Prelude.Maybe ConfigurationType)
parallelismConfigurationUpdate_configurationTypeUpdate = Lens.lens (\ParallelismConfigurationUpdate' {configurationTypeUpdate} -> configurationTypeUpdate) (\s@ParallelismConfigurationUpdate' {} a -> s {configurationTypeUpdate = a} :: ParallelismConfigurationUpdate)

instance
  Prelude.Hashable
    ParallelismConfigurationUpdate

instance
  Prelude.NFData
    ParallelismConfigurationUpdate

instance Core.ToJSON ParallelismConfigurationUpdate where
  toJSON ParallelismConfigurationUpdate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ParallelismUpdate" Core..=)
              Prelude.<$> parallelismUpdate,
            ("AutoScalingEnabledUpdate" Core..=)
              Prelude.<$> autoScalingEnabledUpdate,
            ("ParallelismPerKPUUpdate" Core..=)
              Prelude.<$> parallelismPerKPUUpdate,
            ("ConfigurationTypeUpdate" Core..=)
              Prelude.<$> configurationTypeUpdate
          ]
      )
