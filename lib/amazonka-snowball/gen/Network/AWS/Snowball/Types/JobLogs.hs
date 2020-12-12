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
    jlJobFailureLogURI,
    jlJobCompletionReportURI,
    jlJobSuccessLogURI,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains job logs. Whenever a Snow device is used to import data into or export data out of Amazon S3, you'll have the option of downloading a PDF job report. Job logs are returned as a part of the response syntax of the @DescribeJob@ action in the @JobMetadata@ data type. The job logs can be accessed for up to 60 minutes after this request has been made. To access any of the job logs after 60 minutes have passed, you'll have to make another call to the @DescribeJob@ action.
--
-- For import jobs, the PDF job report becomes available at the end of the import process. For export jobs, your job report typically becomes available while the Snow device for your job part is being delivered to you.
-- The job report provides you insight into the state of your Amazon S3 data transfer. The report includes details about your job or job part for your records.
-- For deeper visibility into the status of your transferred objects, you can look at the two associated logs: a success log and a failure log. The logs are saved in comma-separated value (CSV) format, and the name of each log includes the ID of the job or job part that the log describes.
--
-- /See:/ 'mkJobLogs' smart constructor.
data JobLogs = JobLogs'
  { jobFailureLogURI :: Lude.Maybe Lude.Text,
    jobCompletionReportURI :: Lude.Maybe Lude.Text,
    jobSuccessLogURI :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JobLogs' with the minimum fields required to make a request.
--
-- * 'jobCompletionReportURI' - A link to an Amazon S3 presigned URL where the job completion report is located.
-- * 'jobFailureLogURI' - A link to an Amazon S3 presigned URL where the job failure log is located.
-- * 'jobSuccessLogURI' - A link to an Amazon S3 presigned URL where the job success log is located.
mkJobLogs ::
  JobLogs
mkJobLogs =
  JobLogs'
    { jobFailureLogURI = Lude.Nothing,
      jobCompletionReportURI = Lude.Nothing,
      jobSuccessLogURI = Lude.Nothing
    }

-- | A link to an Amazon S3 presigned URL where the job failure log is located.
--
-- /Note:/ Consider using 'jobFailureLogURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jlJobFailureLogURI :: Lens.Lens' JobLogs (Lude.Maybe Lude.Text)
jlJobFailureLogURI = Lens.lens (jobFailureLogURI :: JobLogs -> Lude.Maybe Lude.Text) (\s a -> s {jobFailureLogURI = a} :: JobLogs)
{-# DEPRECATED jlJobFailureLogURI "Use generic-lens or generic-optics with 'jobFailureLogURI' instead." #-}

-- | A link to an Amazon S3 presigned URL where the job completion report is located.
--
-- /Note:/ Consider using 'jobCompletionReportURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jlJobCompletionReportURI :: Lens.Lens' JobLogs (Lude.Maybe Lude.Text)
jlJobCompletionReportURI = Lens.lens (jobCompletionReportURI :: JobLogs -> Lude.Maybe Lude.Text) (\s a -> s {jobCompletionReportURI = a} :: JobLogs)
{-# DEPRECATED jlJobCompletionReportURI "Use generic-lens or generic-optics with 'jobCompletionReportURI' instead." #-}

-- | A link to an Amazon S3 presigned URL where the job success log is located.
--
-- /Note:/ Consider using 'jobSuccessLogURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jlJobSuccessLogURI :: Lens.Lens' JobLogs (Lude.Maybe Lude.Text)
jlJobSuccessLogURI = Lens.lens (jobSuccessLogURI :: JobLogs -> Lude.Maybe Lude.Text) (\s a -> s {jobSuccessLogURI = a} :: JobLogs)
{-# DEPRECATED jlJobSuccessLogURI "Use generic-lens or generic-optics with 'jobSuccessLogURI' instead." #-}

instance Lude.FromJSON JobLogs where
  parseJSON =
    Lude.withObject
      "JobLogs"
      ( \x ->
          JobLogs'
            Lude.<$> (x Lude..:? "JobFailureLogURI")
            Lude.<*> (x Lude..:? "JobCompletionReportURI")
            Lude.<*> (x Lude..:? "JobSuccessLogURI")
      )
