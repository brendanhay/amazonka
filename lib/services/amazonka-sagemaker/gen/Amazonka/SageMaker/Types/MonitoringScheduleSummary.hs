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
-- Module      : Amazonka.SageMaker.Types.MonitoringScheduleSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.MonitoringScheduleSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.MonitoringType
import Amazonka.SageMaker.Types.ScheduleStatus

-- | Summarizes the monitoring schedule.
--
-- /See:/ 'newMonitoringScheduleSummary' smart constructor.
data MonitoringScheduleSummary = MonitoringScheduleSummary'
  { -- | The name of the endpoint using the monitoring schedule.
    endpointName :: Prelude.Maybe Prelude.Text,
    -- | The type of the monitoring job definition that the schedule is for.
    monitoringType :: Prelude.Maybe MonitoringType,
    -- | The name of the monitoring job definition that the schedule is for.
    monitoringJobDefinitionName :: Prelude.Maybe Prelude.Text,
    -- | The name of the monitoring schedule.
    monitoringScheduleName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the monitoring schedule.
    monitoringScheduleArn :: Prelude.Text,
    -- | The creation time of the monitoring schedule.
    creationTime :: Data.POSIX,
    -- | The last time the monitoring schedule was modified.
    lastModifiedTime :: Data.POSIX,
    -- | The status of the monitoring schedule.
    monitoringScheduleStatus :: ScheduleStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MonitoringScheduleSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointName', 'monitoringScheduleSummary_endpointName' - The name of the endpoint using the monitoring schedule.
--
-- 'monitoringType', 'monitoringScheduleSummary_monitoringType' - The type of the monitoring job definition that the schedule is for.
--
-- 'monitoringJobDefinitionName', 'monitoringScheduleSummary_monitoringJobDefinitionName' - The name of the monitoring job definition that the schedule is for.
--
-- 'monitoringScheduleName', 'monitoringScheduleSummary_monitoringScheduleName' - The name of the monitoring schedule.
--
-- 'monitoringScheduleArn', 'monitoringScheduleSummary_monitoringScheduleArn' - The Amazon Resource Name (ARN) of the monitoring schedule.
--
-- 'creationTime', 'monitoringScheduleSummary_creationTime' - The creation time of the monitoring schedule.
--
-- 'lastModifiedTime', 'monitoringScheduleSummary_lastModifiedTime' - The last time the monitoring schedule was modified.
--
-- 'monitoringScheduleStatus', 'monitoringScheduleSummary_monitoringScheduleStatus' - The status of the monitoring schedule.
newMonitoringScheduleSummary ::
  -- | 'monitoringScheduleName'
  Prelude.Text ->
  -- | 'monitoringScheduleArn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'lastModifiedTime'
  Prelude.UTCTime ->
  -- | 'monitoringScheduleStatus'
  ScheduleStatus ->
  MonitoringScheduleSummary
newMonitoringScheduleSummary
  pMonitoringScheduleName_
  pMonitoringScheduleArn_
  pCreationTime_
  pLastModifiedTime_
  pMonitoringScheduleStatus_ =
    MonitoringScheduleSummary'
      { endpointName =
          Prelude.Nothing,
        monitoringType = Prelude.Nothing,
        monitoringJobDefinitionName = Prelude.Nothing,
        monitoringScheduleName =
          pMonitoringScheduleName_,
        monitoringScheduleArn = pMonitoringScheduleArn_,
        creationTime = Data._Time Lens.# pCreationTime_,
        lastModifiedTime =
          Data._Time Lens.# pLastModifiedTime_,
        monitoringScheduleStatus =
          pMonitoringScheduleStatus_
      }

-- | The name of the endpoint using the monitoring schedule.
monitoringScheduleSummary_endpointName :: Lens.Lens' MonitoringScheduleSummary (Prelude.Maybe Prelude.Text)
monitoringScheduleSummary_endpointName = Lens.lens (\MonitoringScheduleSummary' {endpointName} -> endpointName) (\s@MonitoringScheduleSummary' {} a -> s {endpointName = a} :: MonitoringScheduleSummary)

-- | The type of the monitoring job definition that the schedule is for.
monitoringScheduleSummary_monitoringType :: Lens.Lens' MonitoringScheduleSummary (Prelude.Maybe MonitoringType)
monitoringScheduleSummary_monitoringType = Lens.lens (\MonitoringScheduleSummary' {monitoringType} -> monitoringType) (\s@MonitoringScheduleSummary' {} a -> s {monitoringType = a} :: MonitoringScheduleSummary)

-- | The name of the monitoring job definition that the schedule is for.
monitoringScheduleSummary_monitoringJobDefinitionName :: Lens.Lens' MonitoringScheduleSummary (Prelude.Maybe Prelude.Text)
monitoringScheduleSummary_monitoringJobDefinitionName = Lens.lens (\MonitoringScheduleSummary' {monitoringJobDefinitionName} -> monitoringJobDefinitionName) (\s@MonitoringScheduleSummary' {} a -> s {monitoringJobDefinitionName = a} :: MonitoringScheduleSummary)

-- | The name of the monitoring schedule.
monitoringScheduleSummary_monitoringScheduleName :: Lens.Lens' MonitoringScheduleSummary Prelude.Text
monitoringScheduleSummary_monitoringScheduleName = Lens.lens (\MonitoringScheduleSummary' {monitoringScheduleName} -> monitoringScheduleName) (\s@MonitoringScheduleSummary' {} a -> s {monitoringScheduleName = a} :: MonitoringScheduleSummary)

-- | The Amazon Resource Name (ARN) of the monitoring schedule.
monitoringScheduleSummary_monitoringScheduleArn :: Lens.Lens' MonitoringScheduleSummary Prelude.Text
monitoringScheduleSummary_monitoringScheduleArn = Lens.lens (\MonitoringScheduleSummary' {monitoringScheduleArn} -> monitoringScheduleArn) (\s@MonitoringScheduleSummary' {} a -> s {monitoringScheduleArn = a} :: MonitoringScheduleSummary)

-- | The creation time of the monitoring schedule.
monitoringScheduleSummary_creationTime :: Lens.Lens' MonitoringScheduleSummary Prelude.UTCTime
monitoringScheduleSummary_creationTime = Lens.lens (\MonitoringScheduleSummary' {creationTime} -> creationTime) (\s@MonitoringScheduleSummary' {} a -> s {creationTime = a} :: MonitoringScheduleSummary) Prelude.. Data._Time

-- | The last time the monitoring schedule was modified.
monitoringScheduleSummary_lastModifiedTime :: Lens.Lens' MonitoringScheduleSummary Prelude.UTCTime
monitoringScheduleSummary_lastModifiedTime = Lens.lens (\MonitoringScheduleSummary' {lastModifiedTime} -> lastModifiedTime) (\s@MonitoringScheduleSummary' {} a -> s {lastModifiedTime = a} :: MonitoringScheduleSummary) Prelude.. Data._Time

-- | The status of the monitoring schedule.
monitoringScheduleSummary_monitoringScheduleStatus :: Lens.Lens' MonitoringScheduleSummary ScheduleStatus
monitoringScheduleSummary_monitoringScheduleStatus = Lens.lens (\MonitoringScheduleSummary' {monitoringScheduleStatus} -> monitoringScheduleStatus) (\s@MonitoringScheduleSummary' {} a -> s {monitoringScheduleStatus = a} :: MonitoringScheduleSummary)

instance Data.FromJSON MonitoringScheduleSummary where
  parseJSON =
    Data.withObject
      "MonitoringScheduleSummary"
      ( \x ->
          MonitoringScheduleSummary'
            Prelude.<$> (x Data..:? "EndpointName")
            Prelude.<*> (x Data..:? "MonitoringType")
            Prelude.<*> (x Data..:? "MonitoringJobDefinitionName")
            Prelude.<*> (x Data..: "MonitoringScheduleName")
            Prelude.<*> (x Data..: "MonitoringScheduleArn")
            Prelude.<*> (x Data..: "CreationTime")
            Prelude.<*> (x Data..: "LastModifiedTime")
            Prelude.<*> (x Data..: "MonitoringScheduleStatus")
      )

instance Prelude.Hashable MonitoringScheduleSummary where
  hashWithSalt _salt MonitoringScheduleSummary' {..} =
    _salt `Prelude.hashWithSalt` endpointName
      `Prelude.hashWithSalt` monitoringType
      `Prelude.hashWithSalt` monitoringJobDefinitionName
      `Prelude.hashWithSalt` monitoringScheduleName
      `Prelude.hashWithSalt` monitoringScheduleArn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` monitoringScheduleStatus

instance Prelude.NFData MonitoringScheduleSummary where
  rnf MonitoringScheduleSummary' {..} =
    Prelude.rnf endpointName
      `Prelude.seq` Prelude.rnf monitoringType
      `Prelude.seq` Prelude.rnf monitoringJobDefinitionName
      `Prelude.seq` Prelude.rnf monitoringScheduleName
      `Prelude.seq` Prelude.rnf monitoringScheduleArn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf monitoringScheduleStatus
