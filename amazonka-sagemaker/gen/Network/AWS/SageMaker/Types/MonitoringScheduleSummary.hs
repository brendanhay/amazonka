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
-- Module      : Network.AWS.SageMaker.Types.MonitoringScheduleSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringScheduleSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.MonitoringType
import Network.AWS.SageMaker.Types.ScheduleStatus

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
    creationTime :: Prelude.POSIX,
    -- | The last time the monitoring schedule was modified.
    lastModifiedTime :: Prelude.POSIX,
    -- | The status of the monitoring schedule.
    monitoringScheduleStatus :: ScheduleStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        creationTime =
          Prelude._Time Lens.# pCreationTime_,
        lastModifiedTime =
          Prelude._Time Lens.# pLastModifiedTime_,
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
monitoringScheduleSummary_creationTime = Lens.lens (\MonitoringScheduleSummary' {creationTime} -> creationTime) (\s@MonitoringScheduleSummary' {} a -> s {creationTime = a} :: MonitoringScheduleSummary) Prelude.. Prelude._Time

-- | The last time the monitoring schedule was modified.
monitoringScheduleSummary_lastModifiedTime :: Lens.Lens' MonitoringScheduleSummary Prelude.UTCTime
monitoringScheduleSummary_lastModifiedTime = Lens.lens (\MonitoringScheduleSummary' {lastModifiedTime} -> lastModifiedTime) (\s@MonitoringScheduleSummary' {} a -> s {lastModifiedTime = a} :: MonitoringScheduleSummary) Prelude.. Prelude._Time

-- | The status of the monitoring schedule.
monitoringScheduleSummary_monitoringScheduleStatus :: Lens.Lens' MonitoringScheduleSummary ScheduleStatus
monitoringScheduleSummary_monitoringScheduleStatus = Lens.lens (\MonitoringScheduleSummary' {monitoringScheduleStatus} -> monitoringScheduleStatus) (\s@MonitoringScheduleSummary' {} a -> s {monitoringScheduleStatus = a} :: MonitoringScheduleSummary)

instance Prelude.FromJSON MonitoringScheduleSummary where
  parseJSON =
    Prelude.withObject
      "MonitoringScheduleSummary"
      ( \x ->
          MonitoringScheduleSummary'
            Prelude.<$> (x Prelude..:? "EndpointName")
            Prelude.<*> (x Prelude..:? "MonitoringType")
            Prelude.<*> (x Prelude..:? "MonitoringJobDefinitionName")
            Prelude.<*> (x Prelude..: "MonitoringScheduleName")
            Prelude.<*> (x Prelude..: "MonitoringScheduleArn")
            Prelude.<*> (x Prelude..: "CreationTime")
            Prelude.<*> (x Prelude..: "LastModifiedTime")
            Prelude.<*> (x Prelude..: "MonitoringScheduleStatus")
      )

instance Prelude.Hashable MonitoringScheduleSummary

instance Prelude.NFData MonitoringScheduleSummary
