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
-- Module      : Network.AWS.SageMaker.Types.MonitoringScheduleConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringScheduleConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.MonitoringJobDefinition
import Network.AWS.SageMaker.Types.MonitoringType
import Network.AWS.SageMaker.Types.ScheduleConfig

-- | Configures the monitoring schedule and defines the monitoring job.
--
-- /See:/ 'newMonitoringScheduleConfig' smart constructor.
data MonitoringScheduleConfig = MonitoringScheduleConfig'
  { -- | Configures the monitoring schedule.
    scheduleConfig :: Core.Maybe ScheduleConfig,
    -- | The type of the monitoring job definition to schedule.
    monitoringType :: Core.Maybe MonitoringType,
    -- | The name of the monitoring job definition to schedule.
    monitoringJobDefinitionName :: Core.Maybe Core.Text,
    -- | Defines the monitoring job.
    monitoringJobDefinition :: Core.Maybe MonitoringJobDefinition
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MonitoringScheduleConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scheduleConfig', 'monitoringScheduleConfig_scheduleConfig' - Configures the monitoring schedule.
--
-- 'monitoringType', 'monitoringScheduleConfig_monitoringType' - The type of the monitoring job definition to schedule.
--
-- 'monitoringJobDefinitionName', 'monitoringScheduleConfig_monitoringJobDefinitionName' - The name of the monitoring job definition to schedule.
--
-- 'monitoringJobDefinition', 'monitoringScheduleConfig_monitoringJobDefinition' - Defines the monitoring job.
newMonitoringScheduleConfig ::
  MonitoringScheduleConfig
newMonitoringScheduleConfig =
  MonitoringScheduleConfig'
    { scheduleConfig =
        Core.Nothing,
      monitoringType = Core.Nothing,
      monitoringJobDefinitionName = Core.Nothing,
      monitoringJobDefinition = Core.Nothing
    }

-- | Configures the monitoring schedule.
monitoringScheduleConfig_scheduleConfig :: Lens.Lens' MonitoringScheduleConfig (Core.Maybe ScheduleConfig)
monitoringScheduleConfig_scheduleConfig = Lens.lens (\MonitoringScheduleConfig' {scheduleConfig} -> scheduleConfig) (\s@MonitoringScheduleConfig' {} a -> s {scheduleConfig = a} :: MonitoringScheduleConfig)

-- | The type of the monitoring job definition to schedule.
monitoringScheduleConfig_monitoringType :: Lens.Lens' MonitoringScheduleConfig (Core.Maybe MonitoringType)
monitoringScheduleConfig_monitoringType = Lens.lens (\MonitoringScheduleConfig' {monitoringType} -> monitoringType) (\s@MonitoringScheduleConfig' {} a -> s {monitoringType = a} :: MonitoringScheduleConfig)

-- | The name of the monitoring job definition to schedule.
monitoringScheduleConfig_monitoringJobDefinitionName :: Lens.Lens' MonitoringScheduleConfig (Core.Maybe Core.Text)
monitoringScheduleConfig_monitoringJobDefinitionName = Lens.lens (\MonitoringScheduleConfig' {monitoringJobDefinitionName} -> monitoringJobDefinitionName) (\s@MonitoringScheduleConfig' {} a -> s {monitoringJobDefinitionName = a} :: MonitoringScheduleConfig)

-- | Defines the monitoring job.
monitoringScheduleConfig_monitoringJobDefinition :: Lens.Lens' MonitoringScheduleConfig (Core.Maybe MonitoringJobDefinition)
monitoringScheduleConfig_monitoringJobDefinition = Lens.lens (\MonitoringScheduleConfig' {monitoringJobDefinition} -> monitoringJobDefinition) (\s@MonitoringScheduleConfig' {} a -> s {monitoringJobDefinition = a} :: MonitoringScheduleConfig)

instance Core.FromJSON MonitoringScheduleConfig where
  parseJSON =
    Core.withObject
      "MonitoringScheduleConfig"
      ( \x ->
          MonitoringScheduleConfig'
            Core.<$> (x Core..:? "ScheduleConfig")
            Core.<*> (x Core..:? "MonitoringType")
            Core.<*> (x Core..:? "MonitoringJobDefinitionName")
            Core.<*> (x Core..:? "MonitoringJobDefinition")
      )

instance Core.Hashable MonitoringScheduleConfig

instance Core.NFData MonitoringScheduleConfig

instance Core.ToJSON MonitoringScheduleConfig where
  toJSON MonitoringScheduleConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ScheduleConfig" Core..=) Core.<$> scheduleConfig,
            ("MonitoringType" Core..=) Core.<$> monitoringType,
            ("MonitoringJobDefinitionName" Core..=)
              Core.<$> monitoringJobDefinitionName,
            ("MonitoringJobDefinition" Core..=)
              Core.<$> monitoringJobDefinition
          ]
      )
