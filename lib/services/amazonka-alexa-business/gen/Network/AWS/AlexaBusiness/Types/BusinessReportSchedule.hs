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
-- Module      : Network.AWS.AlexaBusiness.Types.BusinessReportSchedule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.BusinessReportSchedule where

import Network.AWS.AlexaBusiness.Types.BusinessReport
import Network.AWS.AlexaBusiness.Types.BusinessReportContentRange
import Network.AWS.AlexaBusiness.Types.BusinessReportFormat
import Network.AWS.AlexaBusiness.Types.BusinessReportRecurrence
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The schedule of the usage report.
--
-- /See:/ 'newBusinessReportSchedule' smart constructor.
data BusinessReportSchedule = BusinessReportSchedule'
  { -- | The S3 key where the report is delivered.
    s3KeyPrefix :: Prelude.Maybe Prelude.Text,
    -- | The details of the last business report delivery for a specified time
    -- interval.
    lastBusinessReport :: Prelude.Maybe BusinessReport,
    -- | The format of the generated report (individual CSV files or zipped files
    -- of individual files).
    format :: Prelude.Maybe BusinessReportFormat,
    -- | The recurrence of the reports.
    recurrence :: Prelude.Maybe BusinessReportRecurrence,
    -- | The name identifier of the schedule.
    scheduleName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the business report schedule.
    scheduleArn :: Prelude.Maybe Prelude.Text,
    -- | The content range of the reports.
    contentRange :: Prelude.Maybe BusinessReportContentRange,
    -- | The S3 bucket name of the output reports.
    s3BucketName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BusinessReportSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3KeyPrefix', 'businessReportSchedule_s3KeyPrefix' - The S3 key where the report is delivered.
--
-- 'lastBusinessReport', 'businessReportSchedule_lastBusinessReport' - The details of the last business report delivery for a specified time
-- interval.
--
-- 'format', 'businessReportSchedule_format' - The format of the generated report (individual CSV files or zipped files
-- of individual files).
--
-- 'recurrence', 'businessReportSchedule_recurrence' - The recurrence of the reports.
--
-- 'scheduleName', 'businessReportSchedule_scheduleName' - The name identifier of the schedule.
--
-- 'scheduleArn', 'businessReportSchedule_scheduleArn' - The ARN of the business report schedule.
--
-- 'contentRange', 'businessReportSchedule_contentRange' - The content range of the reports.
--
-- 's3BucketName', 'businessReportSchedule_s3BucketName' - The S3 bucket name of the output reports.
newBusinessReportSchedule ::
  BusinessReportSchedule
newBusinessReportSchedule =
  BusinessReportSchedule'
    { s3KeyPrefix =
        Prelude.Nothing,
      lastBusinessReport = Prelude.Nothing,
      format = Prelude.Nothing,
      recurrence = Prelude.Nothing,
      scheduleName = Prelude.Nothing,
      scheduleArn = Prelude.Nothing,
      contentRange = Prelude.Nothing,
      s3BucketName = Prelude.Nothing
    }

-- | The S3 key where the report is delivered.
businessReportSchedule_s3KeyPrefix :: Lens.Lens' BusinessReportSchedule (Prelude.Maybe Prelude.Text)
businessReportSchedule_s3KeyPrefix = Lens.lens (\BusinessReportSchedule' {s3KeyPrefix} -> s3KeyPrefix) (\s@BusinessReportSchedule' {} a -> s {s3KeyPrefix = a} :: BusinessReportSchedule)

-- | The details of the last business report delivery for a specified time
-- interval.
businessReportSchedule_lastBusinessReport :: Lens.Lens' BusinessReportSchedule (Prelude.Maybe BusinessReport)
businessReportSchedule_lastBusinessReport = Lens.lens (\BusinessReportSchedule' {lastBusinessReport} -> lastBusinessReport) (\s@BusinessReportSchedule' {} a -> s {lastBusinessReport = a} :: BusinessReportSchedule)

-- | The format of the generated report (individual CSV files or zipped files
-- of individual files).
businessReportSchedule_format :: Lens.Lens' BusinessReportSchedule (Prelude.Maybe BusinessReportFormat)
businessReportSchedule_format = Lens.lens (\BusinessReportSchedule' {format} -> format) (\s@BusinessReportSchedule' {} a -> s {format = a} :: BusinessReportSchedule)

-- | The recurrence of the reports.
businessReportSchedule_recurrence :: Lens.Lens' BusinessReportSchedule (Prelude.Maybe BusinessReportRecurrence)
businessReportSchedule_recurrence = Lens.lens (\BusinessReportSchedule' {recurrence} -> recurrence) (\s@BusinessReportSchedule' {} a -> s {recurrence = a} :: BusinessReportSchedule)

-- | The name identifier of the schedule.
businessReportSchedule_scheduleName :: Lens.Lens' BusinessReportSchedule (Prelude.Maybe Prelude.Text)
businessReportSchedule_scheduleName = Lens.lens (\BusinessReportSchedule' {scheduleName} -> scheduleName) (\s@BusinessReportSchedule' {} a -> s {scheduleName = a} :: BusinessReportSchedule)

-- | The ARN of the business report schedule.
businessReportSchedule_scheduleArn :: Lens.Lens' BusinessReportSchedule (Prelude.Maybe Prelude.Text)
businessReportSchedule_scheduleArn = Lens.lens (\BusinessReportSchedule' {scheduleArn} -> scheduleArn) (\s@BusinessReportSchedule' {} a -> s {scheduleArn = a} :: BusinessReportSchedule)

-- | The content range of the reports.
businessReportSchedule_contentRange :: Lens.Lens' BusinessReportSchedule (Prelude.Maybe BusinessReportContentRange)
businessReportSchedule_contentRange = Lens.lens (\BusinessReportSchedule' {contentRange} -> contentRange) (\s@BusinessReportSchedule' {} a -> s {contentRange = a} :: BusinessReportSchedule)

-- | The S3 bucket name of the output reports.
businessReportSchedule_s3BucketName :: Lens.Lens' BusinessReportSchedule (Prelude.Maybe Prelude.Text)
businessReportSchedule_s3BucketName = Lens.lens (\BusinessReportSchedule' {s3BucketName} -> s3BucketName) (\s@BusinessReportSchedule' {} a -> s {s3BucketName = a} :: BusinessReportSchedule)

instance Core.FromJSON BusinessReportSchedule where
  parseJSON =
    Core.withObject
      "BusinessReportSchedule"
      ( \x ->
          BusinessReportSchedule'
            Prelude.<$> (x Core..:? "S3KeyPrefix")
            Prelude.<*> (x Core..:? "LastBusinessReport")
            Prelude.<*> (x Core..:? "Format")
            Prelude.<*> (x Core..:? "Recurrence")
            Prelude.<*> (x Core..:? "ScheduleName")
            Prelude.<*> (x Core..:? "ScheduleArn")
            Prelude.<*> (x Core..:? "ContentRange")
            Prelude.<*> (x Core..:? "S3BucketName")
      )

instance Prelude.Hashable BusinessReportSchedule

instance Prelude.NFData BusinessReportSchedule
