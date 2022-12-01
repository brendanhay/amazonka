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
-- Module      : Amazonka.Backup.Types.ReportJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Types.ReportJob where

import Amazonka.Backup.Types.ReportDestination
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains detailed information about a report job. A report job compiles
-- a report based on a report plan and publishes it to Amazon S3.
--
-- /See:/ 'newReportJob' smart constructor.
data ReportJob = ReportJob'
  { -- | An Amazon Resource Name (ARN) that uniquely identifies a resource. The
    -- format of the ARN depends on the resource type.
    reportPlanArn :: Prelude.Maybe Prelude.Text,
    -- | Identifies the report template for the report. Reports are built using a
    -- report template. The report templates are:
    --
    -- @RESOURCE_COMPLIANCE_REPORT | CONTROL_COMPLIANCE_REPORT | BACKUP_JOB_REPORT | COPY_JOB_REPORT | RESTORE_JOB_REPORT@
    reportTemplate :: Prelude.Maybe Prelude.Text,
    -- | The identifier for a report job. A unique, randomly generated, Unicode,
    -- UTF-8 encoded string that is at most 1,024 bytes long. Report job IDs
    -- cannot be edited.
    reportJobId :: Prelude.Maybe Prelude.Text,
    -- | The S3 bucket name and S3 keys for the destination where the report job
    -- publishes the report.
    reportDestination :: Prelude.Maybe ReportDestination,
    -- | The status of a report job. The statuses are:
    --
    -- @CREATED | RUNNING | COMPLETED | FAILED@
    --
    -- @COMPLETED@ means that the report is available for your review at your
    -- designated destination. If the status is @FAILED@, review the
    -- @StatusMessage@ for the reason.
    status :: Prelude.Maybe Prelude.Text,
    -- | The date and time that a report job is completed, in Unix format and
    -- Coordinated Universal Time (UTC). The value of @CompletionTime@ is
    -- accurate to milliseconds. For example, the value 1516925490.087
    -- represents Friday, January 26, 2018 12:11:30.087 AM.
    completionTime :: Prelude.Maybe Core.POSIX,
    -- | The date and time that a report job is created, in Unix format and
    -- Coordinated Universal Time (UTC). The value of @CreationTime@ is
    -- accurate to milliseconds. For example, the value 1516925490.087
    -- represents Friday, January 26, 2018 12:11:30.087 AM.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | A message explaining the status of the report job.
    statusMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reportPlanArn', 'reportJob_reportPlanArn' - An Amazon Resource Name (ARN) that uniquely identifies a resource. The
-- format of the ARN depends on the resource type.
--
-- 'reportTemplate', 'reportJob_reportTemplate' - Identifies the report template for the report. Reports are built using a
-- report template. The report templates are:
--
-- @RESOURCE_COMPLIANCE_REPORT | CONTROL_COMPLIANCE_REPORT | BACKUP_JOB_REPORT | COPY_JOB_REPORT | RESTORE_JOB_REPORT@
--
-- 'reportJobId', 'reportJob_reportJobId' - The identifier for a report job. A unique, randomly generated, Unicode,
-- UTF-8 encoded string that is at most 1,024 bytes long. Report job IDs
-- cannot be edited.
--
-- 'reportDestination', 'reportJob_reportDestination' - The S3 bucket name and S3 keys for the destination where the report job
-- publishes the report.
--
-- 'status', 'reportJob_status' - The status of a report job. The statuses are:
--
-- @CREATED | RUNNING | COMPLETED | FAILED@
--
-- @COMPLETED@ means that the report is available for your review at your
-- designated destination. If the status is @FAILED@, review the
-- @StatusMessage@ for the reason.
--
-- 'completionTime', 'reportJob_completionTime' - The date and time that a report job is completed, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CompletionTime@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'creationTime', 'reportJob_creationTime' - The date and time that a report job is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationTime@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'statusMessage', 'reportJob_statusMessage' - A message explaining the status of the report job.
newReportJob ::
  ReportJob
newReportJob =
  ReportJob'
    { reportPlanArn = Prelude.Nothing,
      reportTemplate = Prelude.Nothing,
      reportJobId = Prelude.Nothing,
      reportDestination = Prelude.Nothing,
      status = Prelude.Nothing,
      completionTime = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      statusMessage = Prelude.Nothing
    }

-- | An Amazon Resource Name (ARN) that uniquely identifies a resource. The
-- format of the ARN depends on the resource type.
reportJob_reportPlanArn :: Lens.Lens' ReportJob (Prelude.Maybe Prelude.Text)
reportJob_reportPlanArn = Lens.lens (\ReportJob' {reportPlanArn} -> reportPlanArn) (\s@ReportJob' {} a -> s {reportPlanArn = a} :: ReportJob)

-- | Identifies the report template for the report. Reports are built using a
-- report template. The report templates are:
--
-- @RESOURCE_COMPLIANCE_REPORT | CONTROL_COMPLIANCE_REPORT | BACKUP_JOB_REPORT | COPY_JOB_REPORT | RESTORE_JOB_REPORT@
reportJob_reportTemplate :: Lens.Lens' ReportJob (Prelude.Maybe Prelude.Text)
reportJob_reportTemplate = Lens.lens (\ReportJob' {reportTemplate} -> reportTemplate) (\s@ReportJob' {} a -> s {reportTemplate = a} :: ReportJob)

-- | The identifier for a report job. A unique, randomly generated, Unicode,
-- UTF-8 encoded string that is at most 1,024 bytes long. Report job IDs
-- cannot be edited.
reportJob_reportJobId :: Lens.Lens' ReportJob (Prelude.Maybe Prelude.Text)
reportJob_reportJobId = Lens.lens (\ReportJob' {reportJobId} -> reportJobId) (\s@ReportJob' {} a -> s {reportJobId = a} :: ReportJob)

-- | The S3 bucket name and S3 keys for the destination where the report job
-- publishes the report.
reportJob_reportDestination :: Lens.Lens' ReportJob (Prelude.Maybe ReportDestination)
reportJob_reportDestination = Lens.lens (\ReportJob' {reportDestination} -> reportDestination) (\s@ReportJob' {} a -> s {reportDestination = a} :: ReportJob)

-- | The status of a report job. The statuses are:
--
-- @CREATED | RUNNING | COMPLETED | FAILED@
--
-- @COMPLETED@ means that the report is available for your review at your
-- designated destination. If the status is @FAILED@, review the
-- @StatusMessage@ for the reason.
reportJob_status :: Lens.Lens' ReportJob (Prelude.Maybe Prelude.Text)
reportJob_status = Lens.lens (\ReportJob' {status} -> status) (\s@ReportJob' {} a -> s {status = a} :: ReportJob)

-- | The date and time that a report job is completed, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CompletionTime@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
reportJob_completionTime :: Lens.Lens' ReportJob (Prelude.Maybe Prelude.UTCTime)
reportJob_completionTime = Lens.lens (\ReportJob' {completionTime} -> completionTime) (\s@ReportJob' {} a -> s {completionTime = a} :: ReportJob) Prelude.. Lens.mapping Core._Time

-- | The date and time that a report job is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationTime@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
reportJob_creationTime :: Lens.Lens' ReportJob (Prelude.Maybe Prelude.UTCTime)
reportJob_creationTime = Lens.lens (\ReportJob' {creationTime} -> creationTime) (\s@ReportJob' {} a -> s {creationTime = a} :: ReportJob) Prelude.. Lens.mapping Core._Time

-- | A message explaining the status of the report job.
reportJob_statusMessage :: Lens.Lens' ReportJob (Prelude.Maybe Prelude.Text)
reportJob_statusMessage = Lens.lens (\ReportJob' {statusMessage} -> statusMessage) (\s@ReportJob' {} a -> s {statusMessage = a} :: ReportJob)

instance Core.FromJSON ReportJob where
  parseJSON =
    Core.withObject
      "ReportJob"
      ( \x ->
          ReportJob'
            Prelude.<$> (x Core..:? "ReportPlanArn")
            Prelude.<*> (x Core..:? "ReportTemplate")
            Prelude.<*> (x Core..:? "ReportJobId")
            Prelude.<*> (x Core..:? "ReportDestination")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "CompletionTime")
            Prelude.<*> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "StatusMessage")
      )

instance Prelude.Hashable ReportJob where
  hashWithSalt _salt ReportJob' {..} =
    _salt `Prelude.hashWithSalt` reportPlanArn
      `Prelude.hashWithSalt` reportTemplate
      `Prelude.hashWithSalt` reportJobId
      `Prelude.hashWithSalt` reportDestination
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` completionTime
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` statusMessage

instance Prelude.NFData ReportJob where
  rnf ReportJob' {..} =
    Prelude.rnf reportPlanArn
      `Prelude.seq` Prelude.rnf reportTemplate
      `Prelude.seq` Prelude.rnf reportJobId
      `Prelude.seq` Prelude.rnf reportDestination
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf completionTime
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf statusMessage
