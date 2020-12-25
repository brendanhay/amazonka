{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.JobLogs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.JobLogs
  ( JobLogs (..),

    -- * Smart constructor
    mkJobLogs,

    -- * Lenses
    jlJobCompletionReportURI,
    jlJobFailureLogURI,
    jlJobSuccessLogURI,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Snowball.Types.String as Types

-- | Contains job logs. Whenever a Snow device is used to import data into or export data out of Amazon S3, you'll have the option of downloading a PDF job report. Job logs are returned as a part of the response syntax of the @DescribeJob@ action in the @JobMetadata@ data type. The job logs can be accessed for up to 60 minutes after this request has been made. To access any of the job logs after 60 minutes have passed, you'll have to make another call to the @DescribeJob@ action.
--
-- For import jobs, the PDF job report becomes available at the end of the import process. For export jobs, your job report typically becomes available while the Snow device for your job part is being delivered to you.
-- The job report provides you insight into the state of your Amazon S3 data transfer. The report includes details about your job or job part for your records.
-- For deeper visibility into the status of your transferred objects, you can look at the two associated logs: a success log and a failure log. The logs are saved in comma-separated value (CSV) format, and the name of each log includes the ID of the job or job part that the log describes.
--
-- /See:/ 'mkJobLogs' smart constructor.
data JobLogs = JobLogs'
  { -- | A link to an Amazon S3 presigned URL where the job completion report is located.
    jobCompletionReportURI :: Core.Maybe Types.String,
    -- | A link to an Amazon S3 presigned URL where the job failure log is located.
    jobFailureLogURI :: Core.Maybe Types.String,
    -- | A link to an Amazon S3 presigned URL where the job success log is located.
    jobSuccessLogURI :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'JobLogs' value with any optional fields omitted.
mkJobLogs ::
  JobLogs
mkJobLogs =
  JobLogs'
    { jobCompletionReportURI = Core.Nothing,
      jobFailureLogURI = Core.Nothing,
      jobSuccessLogURI = Core.Nothing
    }

-- | A link to an Amazon S3 presigned URL where the job completion report is located.
--
-- /Note:/ Consider using 'jobCompletionReportURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jlJobCompletionReportURI :: Lens.Lens' JobLogs (Core.Maybe Types.String)
jlJobCompletionReportURI = Lens.field @"jobCompletionReportURI"
{-# DEPRECATED jlJobCompletionReportURI "Use generic-lens or generic-optics with 'jobCompletionReportURI' instead." #-}

-- | A link to an Amazon S3 presigned URL where the job failure log is located.
--
-- /Note:/ Consider using 'jobFailureLogURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jlJobFailureLogURI :: Lens.Lens' JobLogs (Core.Maybe Types.String)
jlJobFailureLogURI = Lens.field @"jobFailureLogURI"
{-# DEPRECATED jlJobFailureLogURI "Use generic-lens or generic-optics with 'jobFailureLogURI' instead." #-}

-- | A link to an Amazon S3 presigned URL where the job success log is located.
--
-- /Note:/ Consider using 'jobSuccessLogURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jlJobSuccessLogURI :: Lens.Lens' JobLogs (Core.Maybe Types.String)
jlJobSuccessLogURI = Lens.field @"jobSuccessLogURI"
{-# DEPRECATED jlJobSuccessLogURI "Use generic-lens or generic-optics with 'jobSuccessLogURI' instead." #-}

instance Core.FromJSON JobLogs where
  parseJSON =
    Core.withObject "JobLogs" Core.$
      \x ->
        JobLogs'
          Core.<$> (x Core..:? "JobCompletionReportURI")
          Core.<*> (x Core..:? "JobFailureLogURI")
          Core.<*> (x Core..:? "JobSuccessLogURI")
