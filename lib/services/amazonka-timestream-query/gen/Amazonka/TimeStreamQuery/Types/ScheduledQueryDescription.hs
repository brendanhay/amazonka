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
-- Module      : Amazonka.TimeStreamQuery.Types.ScheduledQueryDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamQuery.Types.ScheduledQueryDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TimeStreamQuery.Types.ErrorReportConfiguration
import Amazonka.TimeStreamQuery.Types.NotificationConfiguration
import Amazonka.TimeStreamQuery.Types.ScheduleConfiguration
import Amazonka.TimeStreamQuery.Types.ScheduledQueryRunSummary
import Amazonka.TimeStreamQuery.Types.ScheduledQueryState
import Amazonka.TimeStreamQuery.Types.TargetConfiguration

-- | Structure that describes scheduled query.
--
-- /See:/ 'newScheduledQueryDescription' smart constructor.
data ScheduledQueryDescription = ScheduledQueryDescription'
  { -- | Runtime summary for the last five failed scheduled query runs.
    recentlyFailedRuns :: Prelude.Maybe [ScheduledQueryRunSummary],
    -- | Runtime summary for the last scheduled query run.
    lastRunSummary :: Prelude.Maybe ScheduledQueryRunSummary,
    -- | Last time the query was run.
    previousInvocationTime :: Prelude.Maybe Data.POSIX,
    -- | Error-reporting configuration for the scheduled query.
    errorReportConfiguration :: Prelude.Maybe ErrorReportConfiguration,
    -- | IAM role that Timestream uses to run the schedule query.
    scheduledQueryExecutionRoleArn :: Prelude.Maybe Prelude.Text,
    -- | A customer provided KMS key used to encrypt the scheduled query
    -- resource.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Scheduled query target store configuration.
    targetConfiguration :: Prelude.Maybe TargetConfiguration,
    -- | The next time the scheduled query is scheduled to run.
    nextInvocationTime :: Prelude.Maybe Data.POSIX,
    -- | Creation time of the scheduled query.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | Scheduled query ARN.
    arn :: Prelude.Text,
    -- | Name of the scheduled query.
    name :: Prelude.Text,
    -- | The query to be run.
    queryString :: Data.Sensitive Prelude.Text,
    -- | State of the scheduled query.
    state :: ScheduledQueryState,
    -- | Schedule configuration.
    scheduleConfiguration :: ScheduleConfiguration,
    -- | Notification configuration.
    notificationConfiguration :: NotificationConfiguration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScheduledQueryDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recentlyFailedRuns', 'scheduledQueryDescription_recentlyFailedRuns' - Runtime summary for the last five failed scheduled query runs.
--
-- 'lastRunSummary', 'scheduledQueryDescription_lastRunSummary' - Runtime summary for the last scheduled query run.
--
-- 'previousInvocationTime', 'scheduledQueryDescription_previousInvocationTime' - Last time the query was run.
--
-- 'errorReportConfiguration', 'scheduledQueryDescription_errorReportConfiguration' - Error-reporting configuration for the scheduled query.
--
-- 'scheduledQueryExecutionRoleArn', 'scheduledQueryDescription_scheduledQueryExecutionRoleArn' - IAM role that Timestream uses to run the schedule query.
--
-- 'kmsKeyId', 'scheduledQueryDescription_kmsKeyId' - A customer provided KMS key used to encrypt the scheduled query
-- resource.
--
-- 'targetConfiguration', 'scheduledQueryDescription_targetConfiguration' - Scheduled query target store configuration.
--
-- 'nextInvocationTime', 'scheduledQueryDescription_nextInvocationTime' - The next time the scheduled query is scheduled to run.
--
-- 'creationTime', 'scheduledQueryDescription_creationTime' - Creation time of the scheduled query.
--
-- 'arn', 'scheduledQueryDescription_arn' - Scheduled query ARN.
--
-- 'name', 'scheduledQueryDescription_name' - Name of the scheduled query.
--
-- 'queryString', 'scheduledQueryDescription_queryString' - The query to be run.
--
-- 'state', 'scheduledQueryDescription_state' - State of the scheduled query.
--
-- 'scheduleConfiguration', 'scheduledQueryDescription_scheduleConfiguration' - Schedule configuration.
--
-- 'notificationConfiguration', 'scheduledQueryDescription_notificationConfiguration' - Notification configuration.
newScheduledQueryDescription ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'queryString'
  Prelude.Text ->
  -- | 'state'
  ScheduledQueryState ->
  -- | 'scheduleConfiguration'
  ScheduleConfiguration ->
  -- | 'notificationConfiguration'
  NotificationConfiguration ->
  ScheduledQueryDescription
newScheduledQueryDescription
  pArn_
  pName_
  pQueryString_
  pState_
  pScheduleConfiguration_
  pNotificationConfiguration_ =
    ScheduledQueryDescription'
      { recentlyFailedRuns =
          Prelude.Nothing,
        lastRunSummary = Prelude.Nothing,
        previousInvocationTime = Prelude.Nothing,
        errorReportConfiguration = Prelude.Nothing,
        scheduledQueryExecutionRoleArn = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        targetConfiguration = Prelude.Nothing,
        nextInvocationTime = Prelude.Nothing,
        creationTime = Prelude.Nothing,
        arn = pArn_,
        name = pName_,
        queryString =
          Data._Sensitive Lens.# pQueryString_,
        state = pState_,
        scheduleConfiguration = pScheduleConfiguration_,
        notificationConfiguration =
          pNotificationConfiguration_
      }

-- | Runtime summary for the last five failed scheduled query runs.
scheduledQueryDescription_recentlyFailedRuns :: Lens.Lens' ScheduledQueryDescription (Prelude.Maybe [ScheduledQueryRunSummary])
scheduledQueryDescription_recentlyFailedRuns = Lens.lens (\ScheduledQueryDescription' {recentlyFailedRuns} -> recentlyFailedRuns) (\s@ScheduledQueryDescription' {} a -> s {recentlyFailedRuns = a} :: ScheduledQueryDescription) Prelude.. Lens.mapping Lens.coerced

-- | Runtime summary for the last scheduled query run.
scheduledQueryDescription_lastRunSummary :: Lens.Lens' ScheduledQueryDescription (Prelude.Maybe ScheduledQueryRunSummary)
scheduledQueryDescription_lastRunSummary = Lens.lens (\ScheduledQueryDescription' {lastRunSummary} -> lastRunSummary) (\s@ScheduledQueryDescription' {} a -> s {lastRunSummary = a} :: ScheduledQueryDescription)

-- | Last time the query was run.
scheduledQueryDescription_previousInvocationTime :: Lens.Lens' ScheduledQueryDescription (Prelude.Maybe Prelude.UTCTime)
scheduledQueryDescription_previousInvocationTime = Lens.lens (\ScheduledQueryDescription' {previousInvocationTime} -> previousInvocationTime) (\s@ScheduledQueryDescription' {} a -> s {previousInvocationTime = a} :: ScheduledQueryDescription) Prelude.. Lens.mapping Data._Time

-- | Error-reporting configuration for the scheduled query.
scheduledQueryDescription_errorReportConfiguration :: Lens.Lens' ScheduledQueryDescription (Prelude.Maybe ErrorReportConfiguration)
scheduledQueryDescription_errorReportConfiguration = Lens.lens (\ScheduledQueryDescription' {errorReportConfiguration} -> errorReportConfiguration) (\s@ScheduledQueryDescription' {} a -> s {errorReportConfiguration = a} :: ScheduledQueryDescription)

-- | IAM role that Timestream uses to run the schedule query.
scheduledQueryDescription_scheduledQueryExecutionRoleArn :: Lens.Lens' ScheduledQueryDescription (Prelude.Maybe Prelude.Text)
scheduledQueryDescription_scheduledQueryExecutionRoleArn = Lens.lens (\ScheduledQueryDescription' {scheduledQueryExecutionRoleArn} -> scheduledQueryExecutionRoleArn) (\s@ScheduledQueryDescription' {} a -> s {scheduledQueryExecutionRoleArn = a} :: ScheduledQueryDescription)

-- | A customer provided KMS key used to encrypt the scheduled query
-- resource.
scheduledQueryDescription_kmsKeyId :: Lens.Lens' ScheduledQueryDescription (Prelude.Maybe Prelude.Text)
scheduledQueryDescription_kmsKeyId = Lens.lens (\ScheduledQueryDescription' {kmsKeyId} -> kmsKeyId) (\s@ScheduledQueryDescription' {} a -> s {kmsKeyId = a} :: ScheduledQueryDescription)

-- | Scheduled query target store configuration.
scheduledQueryDescription_targetConfiguration :: Lens.Lens' ScheduledQueryDescription (Prelude.Maybe TargetConfiguration)
scheduledQueryDescription_targetConfiguration = Lens.lens (\ScheduledQueryDescription' {targetConfiguration} -> targetConfiguration) (\s@ScheduledQueryDescription' {} a -> s {targetConfiguration = a} :: ScheduledQueryDescription)

-- | The next time the scheduled query is scheduled to run.
scheduledQueryDescription_nextInvocationTime :: Lens.Lens' ScheduledQueryDescription (Prelude.Maybe Prelude.UTCTime)
scheduledQueryDescription_nextInvocationTime = Lens.lens (\ScheduledQueryDescription' {nextInvocationTime} -> nextInvocationTime) (\s@ScheduledQueryDescription' {} a -> s {nextInvocationTime = a} :: ScheduledQueryDescription) Prelude.. Lens.mapping Data._Time

-- | Creation time of the scheduled query.
scheduledQueryDescription_creationTime :: Lens.Lens' ScheduledQueryDescription (Prelude.Maybe Prelude.UTCTime)
scheduledQueryDescription_creationTime = Lens.lens (\ScheduledQueryDescription' {creationTime} -> creationTime) (\s@ScheduledQueryDescription' {} a -> s {creationTime = a} :: ScheduledQueryDescription) Prelude.. Lens.mapping Data._Time

-- | Scheduled query ARN.
scheduledQueryDescription_arn :: Lens.Lens' ScheduledQueryDescription Prelude.Text
scheduledQueryDescription_arn = Lens.lens (\ScheduledQueryDescription' {arn} -> arn) (\s@ScheduledQueryDescription' {} a -> s {arn = a} :: ScheduledQueryDescription)

-- | Name of the scheduled query.
scheduledQueryDescription_name :: Lens.Lens' ScheduledQueryDescription Prelude.Text
scheduledQueryDescription_name = Lens.lens (\ScheduledQueryDescription' {name} -> name) (\s@ScheduledQueryDescription' {} a -> s {name = a} :: ScheduledQueryDescription)

-- | The query to be run.
scheduledQueryDescription_queryString :: Lens.Lens' ScheduledQueryDescription Prelude.Text
scheduledQueryDescription_queryString = Lens.lens (\ScheduledQueryDescription' {queryString} -> queryString) (\s@ScheduledQueryDescription' {} a -> s {queryString = a} :: ScheduledQueryDescription) Prelude.. Data._Sensitive

-- | State of the scheduled query.
scheduledQueryDescription_state :: Lens.Lens' ScheduledQueryDescription ScheduledQueryState
scheduledQueryDescription_state = Lens.lens (\ScheduledQueryDescription' {state} -> state) (\s@ScheduledQueryDescription' {} a -> s {state = a} :: ScheduledQueryDescription)

-- | Schedule configuration.
scheduledQueryDescription_scheduleConfiguration :: Lens.Lens' ScheduledQueryDescription ScheduleConfiguration
scheduledQueryDescription_scheduleConfiguration = Lens.lens (\ScheduledQueryDescription' {scheduleConfiguration} -> scheduleConfiguration) (\s@ScheduledQueryDescription' {} a -> s {scheduleConfiguration = a} :: ScheduledQueryDescription)

-- | Notification configuration.
scheduledQueryDescription_notificationConfiguration :: Lens.Lens' ScheduledQueryDescription NotificationConfiguration
scheduledQueryDescription_notificationConfiguration = Lens.lens (\ScheduledQueryDescription' {notificationConfiguration} -> notificationConfiguration) (\s@ScheduledQueryDescription' {} a -> s {notificationConfiguration = a} :: ScheduledQueryDescription)

instance Data.FromJSON ScheduledQueryDescription where
  parseJSON =
    Data.withObject
      "ScheduledQueryDescription"
      ( \x ->
          ScheduledQueryDescription'
            Prelude.<$> ( x Data..:? "RecentlyFailedRuns"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "LastRunSummary")
            Prelude.<*> (x Data..:? "PreviousInvocationTime")
            Prelude.<*> (x Data..:? "ErrorReportConfiguration")
            Prelude.<*> (x Data..:? "ScheduledQueryExecutionRoleArn")
            Prelude.<*> (x Data..:? "KmsKeyId")
            Prelude.<*> (x Data..:? "TargetConfiguration")
            Prelude.<*> (x Data..:? "NextInvocationTime")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..: "Arn")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "QueryString")
            Prelude.<*> (x Data..: "State")
            Prelude.<*> (x Data..: "ScheduleConfiguration")
            Prelude.<*> (x Data..: "NotificationConfiguration")
      )

instance Prelude.Hashable ScheduledQueryDescription where
  hashWithSalt _salt ScheduledQueryDescription' {..} =
    _salt `Prelude.hashWithSalt` recentlyFailedRuns
      `Prelude.hashWithSalt` lastRunSummary
      `Prelude.hashWithSalt` previousInvocationTime
      `Prelude.hashWithSalt` errorReportConfiguration
      `Prelude.hashWithSalt` scheduledQueryExecutionRoleArn
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` targetConfiguration
      `Prelude.hashWithSalt` nextInvocationTime
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` queryString
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` scheduleConfiguration
      `Prelude.hashWithSalt` notificationConfiguration

instance Prelude.NFData ScheduledQueryDescription where
  rnf ScheduledQueryDescription' {..} =
    Prelude.rnf recentlyFailedRuns
      `Prelude.seq` Prelude.rnf lastRunSummary
      `Prelude.seq` Prelude.rnf previousInvocationTime
      `Prelude.seq` Prelude.rnf errorReportConfiguration
      `Prelude.seq` Prelude.rnf scheduledQueryExecutionRoleArn
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf targetConfiguration
      `Prelude.seq` Prelude.rnf nextInvocationTime
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf queryString
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf scheduleConfiguration
      `Prelude.seq` Prelude.rnf notificationConfiguration
