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
-- Module      : Amazonka.SageMaker.Types.MonitoringExecutionSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.MonitoringExecutionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ExecutionStatus
import Amazonka.SageMaker.Types.MonitoringType

-- | Summary of information about the last monitoring job to run.
--
-- /See:/ 'newMonitoringExecutionSummary' smart constructor.
data MonitoringExecutionSummary = MonitoringExecutionSummary'
  { -- | The name of the endpoint used to run the monitoring job.
    endpointName :: Prelude.Maybe Prelude.Text,
    -- | Contains the reason a monitoring job failed, if it failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The name of the monitoring job.
    monitoringJobDefinitionName :: Prelude.Maybe Prelude.Text,
    -- | The type of the monitoring job.
    monitoringType :: Prelude.Maybe MonitoringType,
    -- | The Amazon Resource Name (ARN) of the monitoring job.
    processingJobArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the monitoring schedule.
    monitoringScheduleName :: Prelude.Text,
    -- | The time the monitoring job was scheduled.
    scheduledTime :: Data.POSIX,
    -- | The time at which the monitoring job was created.
    creationTime :: Data.POSIX,
    -- | A timestamp that indicates the last time the monitoring job was
    -- modified.
    lastModifiedTime :: Data.POSIX,
    -- | The status of the monitoring job.
    monitoringExecutionStatus :: ExecutionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MonitoringExecutionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointName', 'monitoringExecutionSummary_endpointName' - The name of the endpoint used to run the monitoring job.
--
-- 'failureReason', 'monitoringExecutionSummary_failureReason' - Contains the reason a monitoring job failed, if it failed.
--
-- 'monitoringJobDefinitionName', 'monitoringExecutionSummary_monitoringJobDefinitionName' - The name of the monitoring job.
--
-- 'monitoringType', 'monitoringExecutionSummary_monitoringType' - The type of the monitoring job.
--
-- 'processingJobArn', 'monitoringExecutionSummary_processingJobArn' - The Amazon Resource Name (ARN) of the monitoring job.
--
-- 'monitoringScheduleName', 'monitoringExecutionSummary_monitoringScheduleName' - The name of the monitoring schedule.
--
-- 'scheduledTime', 'monitoringExecutionSummary_scheduledTime' - The time the monitoring job was scheduled.
--
-- 'creationTime', 'monitoringExecutionSummary_creationTime' - The time at which the monitoring job was created.
--
-- 'lastModifiedTime', 'monitoringExecutionSummary_lastModifiedTime' - A timestamp that indicates the last time the monitoring job was
-- modified.
--
-- 'monitoringExecutionStatus', 'monitoringExecutionSummary_monitoringExecutionStatus' - The status of the monitoring job.
newMonitoringExecutionSummary ::
  -- | 'monitoringScheduleName'
  Prelude.Text ->
  -- | 'scheduledTime'
  Prelude.UTCTime ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'lastModifiedTime'
  Prelude.UTCTime ->
  -- | 'monitoringExecutionStatus'
  ExecutionStatus ->
  MonitoringExecutionSummary
newMonitoringExecutionSummary
  pMonitoringScheduleName_
  pScheduledTime_
  pCreationTime_
  pLastModifiedTime_
  pMonitoringExecutionStatus_ =
    MonitoringExecutionSummary'
      { endpointName =
          Prelude.Nothing,
        failureReason = Prelude.Nothing,
        monitoringJobDefinitionName = Prelude.Nothing,
        monitoringType = Prelude.Nothing,
        processingJobArn = Prelude.Nothing,
        monitoringScheduleName =
          pMonitoringScheduleName_,
        scheduledTime =
          Data._Time Lens.# pScheduledTime_,
        creationTime = Data._Time Lens.# pCreationTime_,
        lastModifiedTime =
          Data._Time Lens.# pLastModifiedTime_,
        monitoringExecutionStatus =
          pMonitoringExecutionStatus_
      }

-- | The name of the endpoint used to run the monitoring job.
monitoringExecutionSummary_endpointName :: Lens.Lens' MonitoringExecutionSummary (Prelude.Maybe Prelude.Text)
monitoringExecutionSummary_endpointName = Lens.lens (\MonitoringExecutionSummary' {endpointName} -> endpointName) (\s@MonitoringExecutionSummary' {} a -> s {endpointName = a} :: MonitoringExecutionSummary)

-- | Contains the reason a monitoring job failed, if it failed.
monitoringExecutionSummary_failureReason :: Lens.Lens' MonitoringExecutionSummary (Prelude.Maybe Prelude.Text)
monitoringExecutionSummary_failureReason = Lens.lens (\MonitoringExecutionSummary' {failureReason} -> failureReason) (\s@MonitoringExecutionSummary' {} a -> s {failureReason = a} :: MonitoringExecutionSummary)

-- | The name of the monitoring job.
monitoringExecutionSummary_monitoringJobDefinitionName :: Lens.Lens' MonitoringExecutionSummary (Prelude.Maybe Prelude.Text)
monitoringExecutionSummary_monitoringJobDefinitionName = Lens.lens (\MonitoringExecutionSummary' {monitoringJobDefinitionName} -> monitoringJobDefinitionName) (\s@MonitoringExecutionSummary' {} a -> s {monitoringJobDefinitionName = a} :: MonitoringExecutionSummary)

-- | The type of the monitoring job.
monitoringExecutionSummary_monitoringType :: Lens.Lens' MonitoringExecutionSummary (Prelude.Maybe MonitoringType)
monitoringExecutionSummary_monitoringType = Lens.lens (\MonitoringExecutionSummary' {monitoringType} -> monitoringType) (\s@MonitoringExecutionSummary' {} a -> s {monitoringType = a} :: MonitoringExecutionSummary)

-- | The Amazon Resource Name (ARN) of the monitoring job.
monitoringExecutionSummary_processingJobArn :: Lens.Lens' MonitoringExecutionSummary (Prelude.Maybe Prelude.Text)
monitoringExecutionSummary_processingJobArn = Lens.lens (\MonitoringExecutionSummary' {processingJobArn} -> processingJobArn) (\s@MonitoringExecutionSummary' {} a -> s {processingJobArn = a} :: MonitoringExecutionSummary)

-- | The name of the monitoring schedule.
monitoringExecutionSummary_monitoringScheduleName :: Lens.Lens' MonitoringExecutionSummary Prelude.Text
monitoringExecutionSummary_monitoringScheduleName = Lens.lens (\MonitoringExecutionSummary' {monitoringScheduleName} -> monitoringScheduleName) (\s@MonitoringExecutionSummary' {} a -> s {monitoringScheduleName = a} :: MonitoringExecutionSummary)

-- | The time the monitoring job was scheduled.
monitoringExecutionSummary_scheduledTime :: Lens.Lens' MonitoringExecutionSummary Prelude.UTCTime
monitoringExecutionSummary_scheduledTime = Lens.lens (\MonitoringExecutionSummary' {scheduledTime} -> scheduledTime) (\s@MonitoringExecutionSummary' {} a -> s {scheduledTime = a} :: MonitoringExecutionSummary) Prelude.. Data._Time

-- | The time at which the monitoring job was created.
monitoringExecutionSummary_creationTime :: Lens.Lens' MonitoringExecutionSummary Prelude.UTCTime
monitoringExecutionSummary_creationTime = Lens.lens (\MonitoringExecutionSummary' {creationTime} -> creationTime) (\s@MonitoringExecutionSummary' {} a -> s {creationTime = a} :: MonitoringExecutionSummary) Prelude.. Data._Time

-- | A timestamp that indicates the last time the monitoring job was
-- modified.
monitoringExecutionSummary_lastModifiedTime :: Lens.Lens' MonitoringExecutionSummary Prelude.UTCTime
monitoringExecutionSummary_lastModifiedTime = Lens.lens (\MonitoringExecutionSummary' {lastModifiedTime} -> lastModifiedTime) (\s@MonitoringExecutionSummary' {} a -> s {lastModifiedTime = a} :: MonitoringExecutionSummary) Prelude.. Data._Time

-- | The status of the monitoring job.
monitoringExecutionSummary_monitoringExecutionStatus :: Lens.Lens' MonitoringExecutionSummary ExecutionStatus
monitoringExecutionSummary_monitoringExecutionStatus = Lens.lens (\MonitoringExecutionSummary' {monitoringExecutionStatus} -> monitoringExecutionStatus) (\s@MonitoringExecutionSummary' {} a -> s {monitoringExecutionStatus = a} :: MonitoringExecutionSummary)

instance Data.FromJSON MonitoringExecutionSummary where
  parseJSON =
    Data.withObject
      "MonitoringExecutionSummary"
      ( \x ->
          MonitoringExecutionSummary'
            Prelude.<$> (x Data..:? "EndpointName")
            Prelude.<*> (x Data..:? "FailureReason")
            Prelude.<*> (x Data..:? "MonitoringJobDefinitionName")
            Prelude.<*> (x Data..:? "MonitoringType")
            Prelude.<*> (x Data..:? "ProcessingJobArn")
            Prelude.<*> (x Data..: "MonitoringScheduleName")
            Prelude.<*> (x Data..: "ScheduledTime")
            Prelude.<*> (x Data..: "CreationTime")
            Prelude.<*> (x Data..: "LastModifiedTime")
            Prelude.<*> (x Data..: "MonitoringExecutionStatus")
      )

instance Prelude.Hashable MonitoringExecutionSummary where
  hashWithSalt _salt MonitoringExecutionSummary' {..} =
    _salt
      `Prelude.hashWithSalt` endpointName
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` monitoringJobDefinitionName
      `Prelude.hashWithSalt` monitoringType
      `Prelude.hashWithSalt` processingJobArn
      `Prelude.hashWithSalt` monitoringScheduleName
      `Prelude.hashWithSalt` scheduledTime
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` monitoringExecutionStatus

instance Prelude.NFData MonitoringExecutionSummary where
  rnf MonitoringExecutionSummary' {..} =
    Prelude.rnf endpointName
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf monitoringJobDefinitionName
      `Prelude.seq` Prelude.rnf monitoringType
      `Prelude.seq` Prelude.rnf processingJobArn
      `Prelude.seq` Prelude.rnf monitoringScheduleName
      `Prelude.seq` Prelude.rnf scheduledTime
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf monitoringExecutionStatus
