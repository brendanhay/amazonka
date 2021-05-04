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
-- Module      : Network.AWS.SageMaker.Types.MonitoringSchedule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringSchedule where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.MonitoringExecutionSummary
import Network.AWS.SageMaker.Types.MonitoringScheduleConfig
import Network.AWS.SageMaker.Types.MonitoringType
import Network.AWS.SageMaker.Types.ScheduleStatus
import Network.AWS.SageMaker.Types.Tag

-- | A schedule for a model monitoring job. For information about model
-- monitor, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-monitor.html Amazon SageMaker Model Monitor>.
--
-- /See:/ 'newMonitoringSchedule' smart constructor.
data MonitoringSchedule = MonitoringSchedule'
  { -- | The endpoint that hosts the model being monitored.
    endpointName :: Prelude.Maybe Prelude.Text,
    -- | The time that the monitoring schedule was created.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | The type of the monitoring job definition to schedule.
    monitoringType :: Prelude.Maybe MonitoringType,
    -- | The name of the monitoring schedule.
    monitoringScheduleName :: Prelude.Maybe Prelude.Text,
    -- | The status of the monitoring schedule. This can be one of the following
    -- values.
    --
    -- -   @PENDING@ - The schedule is pending being created.
    --
    -- -   @FAILED@ - The schedule failed.
    --
    -- -   @SCHEDULED@ - The schedule was successfully created.
    --
    -- -   @STOPPED@ - The schedule was stopped.
    monitoringScheduleStatus :: Prelude.Maybe ScheduleStatus,
    -- | If the monitoring schedule failed, the reason it failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the monitoring schedule.
    monitoringScheduleArn :: Prelude.Maybe Prelude.Text,
    -- | A list of the tags associated with the monitoring schedlue. For more
    -- information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources>
    -- in the /AWS General Reference Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | The last time the monitoring schedule was changed.
    lastModifiedTime :: Prelude.Maybe Prelude.POSIX,
    monitoringScheduleConfig :: Prelude.Maybe MonitoringScheduleConfig,
    lastMonitoringExecutionSummary :: Prelude.Maybe MonitoringExecutionSummary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MonitoringSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointName', 'monitoringSchedule_endpointName' - The endpoint that hosts the model being monitored.
--
-- 'creationTime', 'monitoringSchedule_creationTime' - The time that the monitoring schedule was created.
--
-- 'monitoringType', 'monitoringSchedule_monitoringType' - The type of the monitoring job definition to schedule.
--
-- 'monitoringScheduleName', 'monitoringSchedule_monitoringScheduleName' - The name of the monitoring schedule.
--
-- 'monitoringScheduleStatus', 'monitoringSchedule_monitoringScheduleStatus' - The status of the monitoring schedule. This can be one of the following
-- values.
--
-- -   @PENDING@ - The schedule is pending being created.
--
-- -   @FAILED@ - The schedule failed.
--
-- -   @SCHEDULED@ - The schedule was successfully created.
--
-- -   @STOPPED@ - The schedule was stopped.
--
-- 'failureReason', 'monitoringSchedule_failureReason' - If the monitoring schedule failed, the reason it failed.
--
-- 'monitoringScheduleArn', 'monitoringSchedule_monitoringScheduleArn' - The Amazon Resource Name (ARN) of the monitoring schedule.
--
-- 'tags', 'monitoringSchedule_tags' - A list of the tags associated with the monitoring schedlue. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources>
-- in the /AWS General Reference Guide/.
--
-- 'lastModifiedTime', 'monitoringSchedule_lastModifiedTime' - The last time the monitoring schedule was changed.
--
-- 'monitoringScheduleConfig', 'monitoringSchedule_monitoringScheduleConfig' - Undocumented member.
--
-- 'lastMonitoringExecutionSummary', 'monitoringSchedule_lastMonitoringExecutionSummary' - Undocumented member.
newMonitoringSchedule ::
  MonitoringSchedule
newMonitoringSchedule =
  MonitoringSchedule'
    { endpointName = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      monitoringType = Prelude.Nothing,
      monitoringScheduleName = Prelude.Nothing,
      monitoringScheduleStatus = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      monitoringScheduleArn = Prelude.Nothing,
      tags = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      monitoringScheduleConfig = Prelude.Nothing,
      lastMonitoringExecutionSummary = Prelude.Nothing
    }

-- | The endpoint that hosts the model being monitored.
monitoringSchedule_endpointName :: Lens.Lens' MonitoringSchedule (Prelude.Maybe Prelude.Text)
monitoringSchedule_endpointName = Lens.lens (\MonitoringSchedule' {endpointName} -> endpointName) (\s@MonitoringSchedule' {} a -> s {endpointName = a} :: MonitoringSchedule)

-- | The time that the monitoring schedule was created.
monitoringSchedule_creationTime :: Lens.Lens' MonitoringSchedule (Prelude.Maybe Prelude.UTCTime)
monitoringSchedule_creationTime = Lens.lens (\MonitoringSchedule' {creationTime} -> creationTime) (\s@MonitoringSchedule' {} a -> s {creationTime = a} :: MonitoringSchedule) Prelude.. Lens.mapping Prelude._Time

-- | The type of the monitoring job definition to schedule.
monitoringSchedule_monitoringType :: Lens.Lens' MonitoringSchedule (Prelude.Maybe MonitoringType)
monitoringSchedule_monitoringType = Lens.lens (\MonitoringSchedule' {monitoringType} -> monitoringType) (\s@MonitoringSchedule' {} a -> s {monitoringType = a} :: MonitoringSchedule)

-- | The name of the monitoring schedule.
monitoringSchedule_monitoringScheduleName :: Lens.Lens' MonitoringSchedule (Prelude.Maybe Prelude.Text)
monitoringSchedule_monitoringScheduleName = Lens.lens (\MonitoringSchedule' {monitoringScheduleName} -> monitoringScheduleName) (\s@MonitoringSchedule' {} a -> s {monitoringScheduleName = a} :: MonitoringSchedule)

-- | The status of the monitoring schedule. This can be one of the following
-- values.
--
-- -   @PENDING@ - The schedule is pending being created.
--
-- -   @FAILED@ - The schedule failed.
--
-- -   @SCHEDULED@ - The schedule was successfully created.
--
-- -   @STOPPED@ - The schedule was stopped.
monitoringSchedule_monitoringScheduleStatus :: Lens.Lens' MonitoringSchedule (Prelude.Maybe ScheduleStatus)
monitoringSchedule_monitoringScheduleStatus = Lens.lens (\MonitoringSchedule' {monitoringScheduleStatus} -> monitoringScheduleStatus) (\s@MonitoringSchedule' {} a -> s {monitoringScheduleStatus = a} :: MonitoringSchedule)

-- | If the monitoring schedule failed, the reason it failed.
monitoringSchedule_failureReason :: Lens.Lens' MonitoringSchedule (Prelude.Maybe Prelude.Text)
monitoringSchedule_failureReason = Lens.lens (\MonitoringSchedule' {failureReason} -> failureReason) (\s@MonitoringSchedule' {} a -> s {failureReason = a} :: MonitoringSchedule)

-- | The Amazon Resource Name (ARN) of the monitoring schedule.
monitoringSchedule_monitoringScheduleArn :: Lens.Lens' MonitoringSchedule (Prelude.Maybe Prelude.Text)
monitoringSchedule_monitoringScheduleArn = Lens.lens (\MonitoringSchedule' {monitoringScheduleArn} -> monitoringScheduleArn) (\s@MonitoringSchedule' {} a -> s {monitoringScheduleArn = a} :: MonitoringSchedule)

-- | A list of the tags associated with the monitoring schedlue. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources>
-- in the /AWS General Reference Guide/.
monitoringSchedule_tags :: Lens.Lens' MonitoringSchedule (Prelude.Maybe [Tag])
monitoringSchedule_tags = Lens.lens (\MonitoringSchedule' {tags} -> tags) (\s@MonitoringSchedule' {} a -> s {tags = a} :: MonitoringSchedule) Prelude.. Lens.mapping Prelude._Coerce

-- | The last time the monitoring schedule was changed.
monitoringSchedule_lastModifiedTime :: Lens.Lens' MonitoringSchedule (Prelude.Maybe Prelude.UTCTime)
monitoringSchedule_lastModifiedTime = Lens.lens (\MonitoringSchedule' {lastModifiedTime} -> lastModifiedTime) (\s@MonitoringSchedule' {} a -> s {lastModifiedTime = a} :: MonitoringSchedule) Prelude.. Lens.mapping Prelude._Time

-- | Undocumented member.
monitoringSchedule_monitoringScheduleConfig :: Lens.Lens' MonitoringSchedule (Prelude.Maybe MonitoringScheduleConfig)
monitoringSchedule_monitoringScheduleConfig = Lens.lens (\MonitoringSchedule' {monitoringScheduleConfig} -> monitoringScheduleConfig) (\s@MonitoringSchedule' {} a -> s {monitoringScheduleConfig = a} :: MonitoringSchedule)

-- | Undocumented member.
monitoringSchedule_lastMonitoringExecutionSummary :: Lens.Lens' MonitoringSchedule (Prelude.Maybe MonitoringExecutionSummary)
monitoringSchedule_lastMonitoringExecutionSummary = Lens.lens (\MonitoringSchedule' {lastMonitoringExecutionSummary} -> lastMonitoringExecutionSummary) (\s@MonitoringSchedule' {} a -> s {lastMonitoringExecutionSummary = a} :: MonitoringSchedule)

instance Prelude.FromJSON MonitoringSchedule where
  parseJSON =
    Prelude.withObject
      "MonitoringSchedule"
      ( \x ->
          MonitoringSchedule'
            Prelude.<$> (x Prelude..:? "EndpointName")
            Prelude.<*> (x Prelude..:? "CreationTime")
            Prelude.<*> (x Prelude..:? "MonitoringType")
            Prelude.<*> (x Prelude..:? "MonitoringScheduleName")
            Prelude.<*> (x Prelude..:? "MonitoringScheduleStatus")
            Prelude.<*> (x Prelude..:? "FailureReason")
            Prelude.<*> (x Prelude..:? "MonitoringScheduleArn")
            Prelude.<*> (x Prelude..:? "Tags" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "LastModifiedTime")
            Prelude.<*> (x Prelude..:? "MonitoringScheduleConfig")
            Prelude.<*> (x Prelude..:? "LastMonitoringExecutionSummary")
      )

instance Prelude.Hashable MonitoringSchedule

instance Prelude.NFData MonitoringSchedule
