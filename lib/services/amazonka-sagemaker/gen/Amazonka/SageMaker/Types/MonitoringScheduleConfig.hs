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
-- Module      : Amazonka.SageMaker.Types.MonitoringScheduleConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.MonitoringScheduleConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.MonitoringJobDefinition
import Amazonka.SageMaker.Types.MonitoringType
import Amazonka.SageMaker.Types.ScheduleConfig

-- | Configures the monitoring schedule and defines the monitoring job.
--
-- /See:/ 'newMonitoringScheduleConfig' smart constructor.
data MonitoringScheduleConfig = MonitoringScheduleConfig'
  { -- | Defines the monitoring job.
    monitoringJobDefinition :: Prelude.Maybe MonitoringJobDefinition,
    -- | The name of the monitoring job definition to schedule.
    monitoringJobDefinitionName :: Prelude.Maybe Prelude.Text,
    -- | The type of the monitoring job definition to schedule.
    monitoringType :: Prelude.Maybe MonitoringType,
    -- | Configures the monitoring schedule.
    scheduleConfig :: Prelude.Maybe ScheduleConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MonitoringScheduleConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'monitoringJobDefinition', 'monitoringScheduleConfig_monitoringJobDefinition' - Defines the monitoring job.
--
-- 'monitoringJobDefinitionName', 'monitoringScheduleConfig_monitoringJobDefinitionName' - The name of the monitoring job definition to schedule.
--
-- 'monitoringType', 'monitoringScheduleConfig_monitoringType' - The type of the monitoring job definition to schedule.
--
-- 'scheduleConfig', 'monitoringScheduleConfig_scheduleConfig' - Configures the monitoring schedule.
newMonitoringScheduleConfig ::
  MonitoringScheduleConfig
newMonitoringScheduleConfig =
  MonitoringScheduleConfig'
    { monitoringJobDefinition =
        Prelude.Nothing,
      monitoringJobDefinitionName = Prelude.Nothing,
      monitoringType = Prelude.Nothing,
      scheduleConfig = Prelude.Nothing
    }

-- | Defines the monitoring job.
monitoringScheduleConfig_monitoringJobDefinition :: Lens.Lens' MonitoringScheduleConfig (Prelude.Maybe MonitoringJobDefinition)
monitoringScheduleConfig_monitoringJobDefinition = Lens.lens (\MonitoringScheduleConfig' {monitoringJobDefinition} -> monitoringJobDefinition) (\s@MonitoringScheduleConfig' {} a -> s {monitoringJobDefinition = a} :: MonitoringScheduleConfig)

-- | The name of the monitoring job definition to schedule.
monitoringScheduleConfig_monitoringJobDefinitionName :: Lens.Lens' MonitoringScheduleConfig (Prelude.Maybe Prelude.Text)
monitoringScheduleConfig_monitoringJobDefinitionName = Lens.lens (\MonitoringScheduleConfig' {monitoringJobDefinitionName} -> monitoringJobDefinitionName) (\s@MonitoringScheduleConfig' {} a -> s {monitoringJobDefinitionName = a} :: MonitoringScheduleConfig)

-- | The type of the monitoring job definition to schedule.
monitoringScheduleConfig_monitoringType :: Lens.Lens' MonitoringScheduleConfig (Prelude.Maybe MonitoringType)
monitoringScheduleConfig_monitoringType = Lens.lens (\MonitoringScheduleConfig' {monitoringType} -> monitoringType) (\s@MonitoringScheduleConfig' {} a -> s {monitoringType = a} :: MonitoringScheduleConfig)

-- | Configures the monitoring schedule.
monitoringScheduleConfig_scheduleConfig :: Lens.Lens' MonitoringScheduleConfig (Prelude.Maybe ScheduleConfig)
monitoringScheduleConfig_scheduleConfig = Lens.lens (\MonitoringScheduleConfig' {scheduleConfig} -> scheduleConfig) (\s@MonitoringScheduleConfig' {} a -> s {scheduleConfig = a} :: MonitoringScheduleConfig)

instance Data.FromJSON MonitoringScheduleConfig where
  parseJSON =
    Data.withObject
      "MonitoringScheduleConfig"
      ( \x ->
          MonitoringScheduleConfig'
            Prelude.<$> (x Data..:? "MonitoringJobDefinition")
            Prelude.<*> (x Data..:? "MonitoringJobDefinitionName")
            Prelude.<*> (x Data..:? "MonitoringType")
            Prelude.<*> (x Data..:? "ScheduleConfig")
      )

instance Prelude.Hashable MonitoringScheduleConfig where
  hashWithSalt _salt MonitoringScheduleConfig' {..} =
    _salt
      `Prelude.hashWithSalt` monitoringJobDefinition
      `Prelude.hashWithSalt` monitoringJobDefinitionName
      `Prelude.hashWithSalt` monitoringType
      `Prelude.hashWithSalt` scheduleConfig

instance Prelude.NFData MonitoringScheduleConfig where
  rnf MonitoringScheduleConfig' {..} =
    Prelude.rnf monitoringJobDefinition
      `Prelude.seq` Prelude.rnf monitoringJobDefinitionName
      `Prelude.seq` Prelude.rnf monitoringType
      `Prelude.seq` Prelude.rnf scheduleConfig

instance Data.ToJSON MonitoringScheduleConfig where
  toJSON MonitoringScheduleConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MonitoringJobDefinition" Data..=)
              Prelude.<$> monitoringJobDefinition,
            ("MonitoringJobDefinitionName" Data..=)
              Prelude.<$> monitoringJobDefinitionName,
            ("MonitoringType" Data..=)
              Prelude.<$> monitoringType,
            ("ScheduleConfig" Data..=)
              Prelude.<$> scheduleConfig
          ]
      )
