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
-- Module      : Amazonka.SageMaker.Types.ModelDashboardMonitoringSchedule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelDashboardMonitoringSchedule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.MonitoringAlertSummary
import Amazonka.SageMaker.Types.MonitoringExecutionSummary
import Amazonka.SageMaker.Types.MonitoringScheduleConfig
import Amazonka.SageMaker.Types.MonitoringType
import Amazonka.SageMaker.Types.ScheduleStatus

-- | A monitoring schedule for a model displayed in the Amazon SageMaker
-- Model Dashboard.
--
-- /See:/ 'newModelDashboardMonitoringSchedule' smart constructor.
data ModelDashboardMonitoringSchedule = ModelDashboardMonitoringSchedule'
  { -- | A timestamp that indicates when the monitoring schedule was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The endpoint which is monitored.
    endpointName :: Prelude.Maybe Prelude.Text,
    -- | If a monitoring job failed, provides the reason.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | A timestamp that indicates when the monitoring schedule was last
    -- updated.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    lastMonitoringExecutionSummary :: Prelude.Maybe MonitoringExecutionSummary,
    -- | A JSON array where each element is a summary for a monitoring alert.
    monitoringAlertSummaries :: Prelude.Maybe (Prelude.NonEmpty MonitoringAlertSummary),
    -- | The Amazon Resource Name (ARN) of a monitoring schedule.
    monitoringScheduleArn :: Prelude.Maybe Prelude.Text,
    monitoringScheduleConfig :: Prelude.Maybe MonitoringScheduleConfig,
    -- | The name of a monitoring schedule.
    monitoringScheduleName :: Prelude.Maybe Prelude.Text,
    -- | The status of the monitoring schedule.
    monitoringScheduleStatus :: Prelude.Maybe ScheduleStatus,
    -- | The monitor type of a model monitor.
    monitoringType :: Prelude.Maybe MonitoringType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModelDashboardMonitoringSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'modelDashboardMonitoringSchedule_creationTime' - A timestamp that indicates when the monitoring schedule was created.
--
-- 'endpointName', 'modelDashboardMonitoringSchedule_endpointName' - The endpoint which is monitored.
--
-- 'failureReason', 'modelDashboardMonitoringSchedule_failureReason' - If a monitoring job failed, provides the reason.
--
-- 'lastModifiedTime', 'modelDashboardMonitoringSchedule_lastModifiedTime' - A timestamp that indicates when the monitoring schedule was last
-- updated.
--
-- 'lastMonitoringExecutionSummary', 'modelDashboardMonitoringSchedule_lastMonitoringExecutionSummary' - Undocumented member.
--
-- 'monitoringAlertSummaries', 'modelDashboardMonitoringSchedule_monitoringAlertSummaries' - A JSON array where each element is a summary for a monitoring alert.
--
-- 'monitoringScheduleArn', 'modelDashboardMonitoringSchedule_monitoringScheduleArn' - The Amazon Resource Name (ARN) of a monitoring schedule.
--
-- 'monitoringScheduleConfig', 'modelDashboardMonitoringSchedule_monitoringScheduleConfig' - Undocumented member.
--
-- 'monitoringScheduleName', 'modelDashboardMonitoringSchedule_monitoringScheduleName' - The name of a monitoring schedule.
--
-- 'monitoringScheduleStatus', 'modelDashboardMonitoringSchedule_monitoringScheduleStatus' - The status of the monitoring schedule.
--
-- 'monitoringType', 'modelDashboardMonitoringSchedule_monitoringType' - The monitor type of a model monitor.
newModelDashboardMonitoringSchedule ::
  ModelDashboardMonitoringSchedule
newModelDashboardMonitoringSchedule =
  ModelDashboardMonitoringSchedule'
    { creationTime =
        Prelude.Nothing,
      endpointName = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      lastMonitoringExecutionSummary =
        Prelude.Nothing,
      monitoringAlertSummaries =
        Prelude.Nothing,
      monitoringScheduleArn = Prelude.Nothing,
      monitoringScheduleConfig =
        Prelude.Nothing,
      monitoringScheduleName = Prelude.Nothing,
      monitoringScheduleStatus =
        Prelude.Nothing,
      monitoringType = Prelude.Nothing
    }

-- | A timestamp that indicates when the monitoring schedule was created.
modelDashboardMonitoringSchedule_creationTime :: Lens.Lens' ModelDashboardMonitoringSchedule (Prelude.Maybe Prelude.UTCTime)
modelDashboardMonitoringSchedule_creationTime = Lens.lens (\ModelDashboardMonitoringSchedule' {creationTime} -> creationTime) (\s@ModelDashboardMonitoringSchedule' {} a -> s {creationTime = a} :: ModelDashboardMonitoringSchedule) Prelude.. Lens.mapping Data._Time

-- | The endpoint which is monitored.
modelDashboardMonitoringSchedule_endpointName :: Lens.Lens' ModelDashboardMonitoringSchedule (Prelude.Maybe Prelude.Text)
modelDashboardMonitoringSchedule_endpointName = Lens.lens (\ModelDashboardMonitoringSchedule' {endpointName} -> endpointName) (\s@ModelDashboardMonitoringSchedule' {} a -> s {endpointName = a} :: ModelDashboardMonitoringSchedule)

-- | If a monitoring job failed, provides the reason.
modelDashboardMonitoringSchedule_failureReason :: Lens.Lens' ModelDashboardMonitoringSchedule (Prelude.Maybe Prelude.Text)
modelDashboardMonitoringSchedule_failureReason = Lens.lens (\ModelDashboardMonitoringSchedule' {failureReason} -> failureReason) (\s@ModelDashboardMonitoringSchedule' {} a -> s {failureReason = a} :: ModelDashboardMonitoringSchedule)

-- | A timestamp that indicates when the monitoring schedule was last
-- updated.
modelDashboardMonitoringSchedule_lastModifiedTime :: Lens.Lens' ModelDashboardMonitoringSchedule (Prelude.Maybe Prelude.UTCTime)
modelDashboardMonitoringSchedule_lastModifiedTime = Lens.lens (\ModelDashboardMonitoringSchedule' {lastModifiedTime} -> lastModifiedTime) (\s@ModelDashboardMonitoringSchedule' {} a -> s {lastModifiedTime = a} :: ModelDashboardMonitoringSchedule) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
modelDashboardMonitoringSchedule_lastMonitoringExecutionSummary :: Lens.Lens' ModelDashboardMonitoringSchedule (Prelude.Maybe MonitoringExecutionSummary)
modelDashboardMonitoringSchedule_lastMonitoringExecutionSummary = Lens.lens (\ModelDashboardMonitoringSchedule' {lastMonitoringExecutionSummary} -> lastMonitoringExecutionSummary) (\s@ModelDashboardMonitoringSchedule' {} a -> s {lastMonitoringExecutionSummary = a} :: ModelDashboardMonitoringSchedule)

-- | A JSON array where each element is a summary for a monitoring alert.
modelDashboardMonitoringSchedule_monitoringAlertSummaries :: Lens.Lens' ModelDashboardMonitoringSchedule (Prelude.Maybe (Prelude.NonEmpty MonitoringAlertSummary))
modelDashboardMonitoringSchedule_monitoringAlertSummaries = Lens.lens (\ModelDashboardMonitoringSchedule' {monitoringAlertSummaries} -> monitoringAlertSummaries) (\s@ModelDashboardMonitoringSchedule' {} a -> s {monitoringAlertSummaries = a} :: ModelDashboardMonitoringSchedule) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of a monitoring schedule.
modelDashboardMonitoringSchedule_monitoringScheduleArn :: Lens.Lens' ModelDashboardMonitoringSchedule (Prelude.Maybe Prelude.Text)
modelDashboardMonitoringSchedule_monitoringScheduleArn = Lens.lens (\ModelDashboardMonitoringSchedule' {monitoringScheduleArn} -> monitoringScheduleArn) (\s@ModelDashboardMonitoringSchedule' {} a -> s {monitoringScheduleArn = a} :: ModelDashboardMonitoringSchedule)

-- | Undocumented member.
modelDashboardMonitoringSchedule_monitoringScheduleConfig :: Lens.Lens' ModelDashboardMonitoringSchedule (Prelude.Maybe MonitoringScheduleConfig)
modelDashboardMonitoringSchedule_monitoringScheduleConfig = Lens.lens (\ModelDashboardMonitoringSchedule' {monitoringScheduleConfig} -> monitoringScheduleConfig) (\s@ModelDashboardMonitoringSchedule' {} a -> s {monitoringScheduleConfig = a} :: ModelDashboardMonitoringSchedule)

-- | The name of a monitoring schedule.
modelDashboardMonitoringSchedule_monitoringScheduleName :: Lens.Lens' ModelDashboardMonitoringSchedule (Prelude.Maybe Prelude.Text)
modelDashboardMonitoringSchedule_monitoringScheduleName = Lens.lens (\ModelDashboardMonitoringSchedule' {monitoringScheduleName} -> monitoringScheduleName) (\s@ModelDashboardMonitoringSchedule' {} a -> s {monitoringScheduleName = a} :: ModelDashboardMonitoringSchedule)

-- | The status of the monitoring schedule.
modelDashboardMonitoringSchedule_monitoringScheduleStatus :: Lens.Lens' ModelDashboardMonitoringSchedule (Prelude.Maybe ScheduleStatus)
modelDashboardMonitoringSchedule_monitoringScheduleStatus = Lens.lens (\ModelDashboardMonitoringSchedule' {monitoringScheduleStatus} -> monitoringScheduleStatus) (\s@ModelDashboardMonitoringSchedule' {} a -> s {monitoringScheduleStatus = a} :: ModelDashboardMonitoringSchedule)

-- | The monitor type of a model monitor.
modelDashboardMonitoringSchedule_monitoringType :: Lens.Lens' ModelDashboardMonitoringSchedule (Prelude.Maybe MonitoringType)
modelDashboardMonitoringSchedule_monitoringType = Lens.lens (\ModelDashboardMonitoringSchedule' {monitoringType} -> monitoringType) (\s@ModelDashboardMonitoringSchedule' {} a -> s {monitoringType = a} :: ModelDashboardMonitoringSchedule)

instance
  Data.FromJSON
    ModelDashboardMonitoringSchedule
  where
  parseJSON =
    Data.withObject
      "ModelDashboardMonitoringSchedule"
      ( \x ->
          ModelDashboardMonitoringSchedule'
            Prelude.<$> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "EndpointName")
            Prelude.<*> (x Data..:? "FailureReason")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "LastMonitoringExecutionSummary")
            Prelude.<*> (x Data..:? "MonitoringAlertSummaries")
            Prelude.<*> (x Data..:? "MonitoringScheduleArn")
            Prelude.<*> (x Data..:? "MonitoringScheduleConfig")
            Prelude.<*> (x Data..:? "MonitoringScheduleName")
            Prelude.<*> (x Data..:? "MonitoringScheduleStatus")
            Prelude.<*> (x Data..:? "MonitoringType")
      )

instance
  Prelude.Hashable
    ModelDashboardMonitoringSchedule
  where
  hashWithSalt
    _salt
    ModelDashboardMonitoringSchedule' {..} =
      _salt `Prelude.hashWithSalt` creationTime
        `Prelude.hashWithSalt` endpointName
        `Prelude.hashWithSalt` failureReason
        `Prelude.hashWithSalt` lastModifiedTime
        `Prelude.hashWithSalt` lastMonitoringExecutionSummary
        `Prelude.hashWithSalt` monitoringAlertSummaries
        `Prelude.hashWithSalt` monitoringScheduleArn
        `Prelude.hashWithSalt` monitoringScheduleConfig
        `Prelude.hashWithSalt` monitoringScheduleName
        `Prelude.hashWithSalt` monitoringScheduleStatus
        `Prelude.hashWithSalt` monitoringType

instance
  Prelude.NFData
    ModelDashboardMonitoringSchedule
  where
  rnf ModelDashboardMonitoringSchedule' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf endpointName
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf lastMonitoringExecutionSummary
      `Prelude.seq` Prelude.rnf monitoringAlertSummaries
      `Prelude.seq` Prelude.rnf monitoringScheduleArn
      `Prelude.seq` Prelude.rnf monitoringScheduleConfig
      `Prelude.seq` Prelude.rnf monitoringScheduleName
      `Prelude.seq` Prelude.rnf monitoringScheduleStatus
      `Prelude.seq` Prelude.rnf monitoringType
