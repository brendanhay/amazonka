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
-- Module      : Amazonka.Snowball.Types.JobLogs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Snowball.Types.JobLogs where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains job logs. Whenever a Snow device is used to import data into or
-- export data out of Amazon S3, you\'ll have the option of downloading a
-- PDF job report. Job logs are returned as a part of the response syntax
-- of the @DescribeJob@ action in the @JobMetadata@ data type. The job logs
-- can be accessed for up to 60 minutes after this request has been made.
-- To access any of the job logs after 60 minutes have passed, you\'ll have
-- to make another call to the @DescribeJob@ action.
--
-- For import jobs, the PDF job report becomes available at the end of the
-- import process. For export jobs, your job report typically becomes
-- available while the Snow device for your job part is being delivered to
-- you.
--
-- The job report provides you insight into the state of your Amazon S3
-- data transfer. The report includes details about your job or job part
-- for your records.
--
-- For deeper visibility into the status of your transferred objects, you
-- can look at the two associated logs: a success log and a failure log.
-- The logs are saved in comma-separated value (CSV) format, and the name
-- of each log includes the ID of the job or job part that the log
-- describes.
--
-- /See:/ 'newJobLogs' smart constructor.
data JobLogs = JobLogs'
  { -- | A link to an Amazon S3 presigned URL where the job completion report is
    -- located.
    jobCompletionReportURI :: Prelude.Maybe Prelude.Text,
    -- | A link to an Amazon S3 presigned URL where the job failure log is
    -- located.
    jobFailureLogURI :: Prelude.Maybe Prelude.Text,
    -- | A link to an Amazon S3 presigned URL where the job success log is
    -- located.
    jobSuccessLogURI :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobLogs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobCompletionReportURI', 'jobLogs_jobCompletionReportURI' - A link to an Amazon S3 presigned URL where the job completion report is
-- located.
--
-- 'jobFailureLogURI', 'jobLogs_jobFailureLogURI' - A link to an Amazon S3 presigned URL where the job failure log is
-- located.
--
-- 'jobSuccessLogURI', 'jobLogs_jobSuccessLogURI' - A link to an Amazon S3 presigned URL where the job success log is
-- located.
newJobLogs ::
  JobLogs
newJobLogs =
  JobLogs'
    { jobCompletionReportURI = Prelude.Nothing,
      jobFailureLogURI = Prelude.Nothing,
      jobSuccessLogURI = Prelude.Nothing
    }

-- | A link to an Amazon S3 presigned URL where the job completion report is
-- located.
jobLogs_jobCompletionReportURI :: Lens.Lens' JobLogs (Prelude.Maybe Prelude.Text)
jobLogs_jobCompletionReportURI = Lens.lens (\JobLogs' {jobCompletionReportURI} -> jobCompletionReportURI) (\s@JobLogs' {} a -> s {jobCompletionReportURI = a} :: JobLogs)

-- | A link to an Amazon S3 presigned URL where the job failure log is
-- located.
jobLogs_jobFailureLogURI :: Lens.Lens' JobLogs (Prelude.Maybe Prelude.Text)
jobLogs_jobFailureLogURI = Lens.lens (\JobLogs' {jobFailureLogURI} -> jobFailureLogURI) (\s@JobLogs' {} a -> s {jobFailureLogURI = a} :: JobLogs)

-- | A link to an Amazon S3 presigned URL where the job success log is
-- located.
jobLogs_jobSuccessLogURI :: Lens.Lens' JobLogs (Prelude.Maybe Prelude.Text)
jobLogs_jobSuccessLogURI = Lens.lens (\JobLogs' {jobSuccessLogURI} -> jobSuccessLogURI) (\s@JobLogs' {} a -> s {jobSuccessLogURI = a} :: JobLogs)

instance Data.FromJSON JobLogs where
  parseJSON =
    Data.withObject
      "JobLogs"
      ( \x ->
          JobLogs'
            Prelude.<$> (x Data..:? "JobCompletionReportURI")
            Prelude.<*> (x Data..:? "JobFailureLogURI")
            Prelude.<*> (x Data..:? "JobSuccessLogURI")
      )

instance Prelude.Hashable JobLogs where
  hashWithSalt _salt JobLogs' {..} =
    _salt `Prelude.hashWithSalt` jobCompletionReportURI
      `Prelude.hashWithSalt` jobFailureLogURI
      `Prelude.hashWithSalt` jobSuccessLogURI

instance Prelude.NFData JobLogs where
  rnf JobLogs' {..} =
    Prelude.rnf jobCompletionReportURI
      `Prelude.seq` Prelude.rnf jobFailureLogURI
      `Prelude.seq` Prelude.rnf jobSuccessLogURI
