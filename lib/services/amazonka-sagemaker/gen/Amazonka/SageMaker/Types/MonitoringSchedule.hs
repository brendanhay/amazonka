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
-- Module      : Amazonka.SageMaker.Types.MonitoringSchedule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.MonitoringSchedule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.MonitoringExecutionSummary
import Amazonka.SageMaker.Types.MonitoringScheduleConfig
import Amazonka.SageMaker.Types.MonitoringType
import Amazonka.SageMaker.Types.ScheduleStatus
import Amazonka.SageMaker.Types.Tag

-- | A schedule for a model monitoring job. For information about model
-- monitor, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-monitor.html Amazon SageMaker Model Monitor>.
--
-- /See:/ 'newMonitoringSchedule' smart constructor.
data MonitoringSchedule = MonitoringSchedule'
  { -- | The time that the monitoring schedule was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The endpoint that hosts the model being monitored.
    endpointName :: Prelude.Maybe Prelude.Text,
    -- | If the monitoring schedule failed, the reason it failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The last time the monitoring schedule was changed.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    lastMonitoringExecutionSummary :: Prelude.Maybe MonitoringExecutionSummary,
    -- | The Amazon Resource Name (ARN) of the monitoring schedule.
    monitoringScheduleArn :: Prelude.Maybe Prelude.Text,
    monitoringScheduleConfig :: Prelude.Maybe MonitoringScheduleConfig,
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
    -- | The type of the monitoring job definition to schedule.
    monitoringType :: Prelude.Maybe MonitoringType,
    -- | A list of the tags associated with the monitoring schedlue. For more
    -- information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
    -- in the /Amazon Web Services General Reference Guide/.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MonitoringSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'monitoringSchedule_creationTime' - The time that the monitoring schedule was created.
--
-- 'endpointName', 'monitoringSchedule_endpointName' - The endpoint that hosts the model being monitored.
--
-- 'failureReason', 'monitoringSchedule_failureReason' - If the monitoring schedule failed, the reason it failed.
--
-- 'lastModifiedTime', 'monitoringSchedule_lastModifiedTime' - The last time the monitoring schedule was changed.
--
-- 'lastMonitoringExecutionSummary', 'monitoringSchedule_lastMonitoringExecutionSummary' - Undocumented member.
--
-- 'monitoringScheduleArn', 'monitoringSchedule_monitoringScheduleArn' - The Amazon Resource Name (ARN) of the monitoring schedule.
--
-- 'monitoringScheduleConfig', 'monitoringSchedule_monitoringScheduleConfig' - Undocumented member.
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
-- 'monitoringType', 'monitoringSchedule_monitoringType' - The type of the monitoring job definition to schedule.
--
-- 'tags', 'monitoringSchedule_tags' - A list of the tags associated with the monitoring schedlue. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
-- in the /Amazon Web Services General Reference Guide/.
newMonitoringSchedule ::
  MonitoringSchedule
newMonitoringSchedule =
  MonitoringSchedule'
    { creationTime = Prelude.Nothing,
      endpointName = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      lastMonitoringExecutionSummary = Prelude.Nothing,
      monitoringScheduleArn = Prelude.Nothing,
      monitoringScheduleConfig = Prelude.Nothing,
      monitoringScheduleName = Prelude.Nothing,
      monitoringScheduleStatus = Prelude.Nothing,
      monitoringType = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The time that the monitoring schedule was created.
monitoringSchedule_creationTime :: Lens.Lens' MonitoringSchedule (Prelude.Maybe Prelude.UTCTime)
monitoringSchedule_creationTime = Lens.lens (\MonitoringSchedule' {creationTime} -> creationTime) (\s@MonitoringSchedule' {} a -> s {creationTime = a} :: MonitoringSchedule) Prelude.. Lens.mapping Data._Time

-- | The endpoint that hosts the model being monitored.
monitoringSchedule_endpointName :: Lens.Lens' MonitoringSchedule (Prelude.Maybe Prelude.Text)
monitoringSchedule_endpointName = Lens.lens (\MonitoringSchedule' {endpointName} -> endpointName) (\s@MonitoringSchedule' {} a -> s {endpointName = a} :: MonitoringSchedule)

-- | If the monitoring schedule failed, the reason it failed.
monitoringSchedule_failureReason :: Lens.Lens' MonitoringSchedule (Prelude.Maybe Prelude.Text)
monitoringSchedule_failureReason = Lens.lens (\MonitoringSchedule' {failureReason} -> failureReason) (\s@MonitoringSchedule' {} a -> s {failureReason = a} :: MonitoringSchedule)

-- | The last time the monitoring schedule was changed.
monitoringSchedule_lastModifiedTime :: Lens.Lens' MonitoringSchedule (Prelude.Maybe Prelude.UTCTime)
monitoringSchedule_lastModifiedTime = Lens.lens (\MonitoringSchedule' {lastModifiedTime} -> lastModifiedTime) (\s@MonitoringSchedule' {} a -> s {lastModifiedTime = a} :: MonitoringSchedule) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
monitoringSchedule_lastMonitoringExecutionSummary :: Lens.Lens' MonitoringSchedule (Prelude.Maybe MonitoringExecutionSummary)
monitoringSchedule_lastMonitoringExecutionSummary = Lens.lens (\MonitoringSchedule' {lastMonitoringExecutionSummary} -> lastMonitoringExecutionSummary) (\s@MonitoringSchedule' {} a -> s {lastMonitoringExecutionSummary = a} :: MonitoringSchedule)

-- | The Amazon Resource Name (ARN) of the monitoring schedule.
monitoringSchedule_monitoringScheduleArn :: Lens.Lens' MonitoringSchedule (Prelude.Maybe Prelude.Text)
monitoringSchedule_monitoringScheduleArn = Lens.lens (\MonitoringSchedule' {monitoringScheduleArn} -> monitoringScheduleArn) (\s@MonitoringSchedule' {} a -> s {monitoringScheduleArn = a} :: MonitoringSchedule)

-- | Undocumented member.
monitoringSchedule_monitoringScheduleConfig :: Lens.Lens' MonitoringSchedule (Prelude.Maybe MonitoringScheduleConfig)
monitoringSchedule_monitoringScheduleConfig = Lens.lens (\MonitoringSchedule' {monitoringScheduleConfig} -> monitoringScheduleConfig) (\s@MonitoringSchedule' {} a -> s {monitoringScheduleConfig = a} :: MonitoringSchedule)

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

-- | The type of the monitoring job definition to schedule.
monitoringSchedule_monitoringType :: Lens.Lens' MonitoringSchedule (Prelude.Maybe MonitoringType)
monitoringSchedule_monitoringType = Lens.lens (\MonitoringSchedule' {monitoringType} -> monitoringType) (\s@MonitoringSchedule' {} a -> s {monitoringType = a} :: MonitoringSchedule)

-- | A list of the tags associated with the monitoring schedlue. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
-- in the /Amazon Web Services General Reference Guide/.
monitoringSchedule_tags :: Lens.Lens' MonitoringSchedule (Prelude.Maybe [Tag])
monitoringSchedule_tags = Lens.lens (\MonitoringSchedule' {tags} -> tags) (\s@MonitoringSchedule' {} a -> s {tags = a} :: MonitoringSchedule) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON MonitoringSchedule where
  parseJSON =
    Data.withObject
      "MonitoringSchedule"
      ( \x ->
          MonitoringSchedule'
            Prelude.<$> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "EndpointName")
            Prelude.<*> (x Data..:? "FailureReason")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "LastMonitoringExecutionSummary")
            Prelude.<*> (x Data..:? "MonitoringScheduleArn")
            Prelude.<*> (x Data..:? "MonitoringScheduleConfig")
            Prelude.<*> (x Data..:? "MonitoringScheduleName")
            Prelude.<*> (x Data..:? "MonitoringScheduleStatus")
            Prelude.<*> (x Data..:? "MonitoringType")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable MonitoringSchedule where
  hashWithSalt _salt MonitoringSchedule' {..} =
    _salt
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` endpointName
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` lastMonitoringExecutionSummary
      `Prelude.hashWithSalt` monitoringScheduleArn
      `Prelude.hashWithSalt` monitoringScheduleConfig
      `Prelude.hashWithSalt` monitoringScheduleName
      `Prelude.hashWithSalt` monitoringScheduleStatus
      `Prelude.hashWithSalt` monitoringType
      `Prelude.hashWithSalt` tags

instance Prelude.NFData MonitoringSchedule where
  rnf MonitoringSchedule' {..} =
    Prelude.rnf creationTime `Prelude.seq`
      Prelude.rnf endpointName `Prelude.seq`
        Prelude.rnf failureReason `Prelude.seq`
          Prelude.rnf lastModifiedTime `Prelude.seq`
            Prelude.rnf lastMonitoringExecutionSummary `Prelude.seq`
              Prelude.rnf monitoringScheduleArn `Prelude.seq`
                Prelude.rnf monitoringScheduleConfig `Prelude.seq`
                  Prelude.rnf monitoringScheduleName `Prelude.seq`
                    Prelude.rnf monitoringScheduleStatus `Prelude.seq`
                      Prelude.rnf monitoringType `Prelude.seq`
                        Prelude.rnf tags
