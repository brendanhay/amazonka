{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.MonitoringJobDefinition
import Network.AWS.SageMaker.Types.MonitoringType
import Network.AWS.SageMaker.Types.ScheduleConfig

-- | Configures the monitoring schedule and defines the monitoring job.
--
-- /See:/ 'newMonitoringScheduleConfig' smart constructor.
data MonitoringScheduleConfig = MonitoringScheduleConfig'
  { -- | Configures the monitoring schedule.
    scheduleConfig :: Prelude.Maybe ScheduleConfig,
    -- | The type of the monitoring job definition to schedule.
    monitoringType :: Prelude.Maybe MonitoringType,
    -- | The name of the monitoring job definition to schedule.
    monitoringJobDefinitionName :: Prelude.Maybe Prelude.Text,
    -- | Defines the monitoring job.
    monitoringJobDefinition :: Prelude.Maybe MonitoringJobDefinition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      monitoringType = Prelude.Nothing,
      monitoringJobDefinitionName = Prelude.Nothing,
      monitoringJobDefinition = Prelude.Nothing
    }

-- | Configures the monitoring schedule.
monitoringScheduleConfig_scheduleConfig :: Lens.Lens' MonitoringScheduleConfig (Prelude.Maybe ScheduleConfig)
monitoringScheduleConfig_scheduleConfig = Lens.lens (\MonitoringScheduleConfig' {scheduleConfig} -> scheduleConfig) (\s@MonitoringScheduleConfig' {} a -> s {scheduleConfig = a} :: MonitoringScheduleConfig)

-- | The type of the monitoring job definition to schedule.
monitoringScheduleConfig_monitoringType :: Lens.Lens' MonitoringScheduleConfig (Prelude.Maybe MonitoringType)
monitoringScheduleConfig_monitoringType = Lens.lens (\MonitoringScheduleConfig' {monitoringType} -> monitoringType) (\s@MonitoringScheduleConfig' {} a -> s {monitoringType = a} :: MonitoringScheduleConfig)

-- | The name of the monitoring job definition to schedule.
monitoringScheduleConfig_monitoringJobDefinitionName :: Lens.Lens' MonitoringScheduleConfig (Prelude.Maybe Prelude.Text)
monitoringScheduleConfig_monitoringJobDefinitionName = Lens.lens (\MonitoringScheduleConfig' {monitoringJobDefinitionName} -> monitoringJobDefinitionName) (\s@MonitoringScheduleConfig' {} a -> s {monitoringJobDefinitionName = a} :: MonitoringScheduleConfig)

-- | Defines the monitoring job.
monitoringScheduleConfig_monitoringJobDefinition :: Lens.Lens' MonitoringScheduleConfig (Prelude.Maybe MonitoringJobDefinition)
monitoringScheduleConfig_monitoringJobDefinition = Lens.lens (\MonitoringScheduleConfig' {monitoringJobDefinition} -> monitoringJobDefinition) (\s@MonitoringScheduleConfig' {} a -> s {monitoringJobDefinition = a} :: MonitoringScheduleConfig)

instance Prelude.FromJSON MonitoringScheduleConfig where
  parseJSON =
    Prelude.withObject
      "MonitoringScheduleConfig"
      ( \x ->
          MonitoringScheduleConfig'
            Prelude.<$> (x Prelude..:? "ScheduleConfig")
            Prelude.<*> (x Prelude..:? "MonitoringType")
            Prelude.<*> (x Prelude..:? "MonitoringJobDefinitionName")
            Prelude.<*> (x Prelude..:? "MonitoringJobDefinition")
      )

instance Prelude.Hashable MonitoringScheduleConfig

instance Prelude.NFData MonitoringScheduleConfig

instance Prelude.ToJSON MonitoringScheduleConfig where
  toJSON MonitoringScheduleConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ScheduleConfig" Prelude..=)
              Prelude.<$> scheduleConfig,
            ("MonitoringType" Prelude..=)
              Prelude.<$> monitoringType,
            ("MonitoringJobDefinitionName" Prelude..=)
              Prelude.<$> monitoringJobDefinitionName,
            ("MonitoringJobDefinition" Prelude..=)
              Prelude.<$> monitoringJobDefinition
          ]
      )
