{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.BusinessReportSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.BusinessReportSchedule
  ( BusinessReportSchedule (..),

    -- * Smart constructor
    mkBusinessReportSchedule,

    -- * Lenses
    brsS3KeyPrefix,
    brsLastBusinessReport,
    brsFormat,
    brsRecurrence,
    brsScheduleName,
    brsScheduleARN,
    brsContentRange,
    brsS3BucketName,
  )
where

import Network.AWS.AlexaBusiness.Types.BusinessReport
import Network.AWS.AlexaBusiness.Types.BusinessReportContentRange
import Network.AWS.AlexaBusiness.Types.BusinessReportFormat
import Network.AWS.AlexaBusiness.Types.BusinessReportRecurrence
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The schedule of the usage report.
--
-- /See:/ 'mkBusinessReportSchedule' smart constructor.
data BusinessReportSchedule = BusinessReportSchedule'
  { -- | The S3 key where the report is delivered.
    s3KeyPrefix :: Lude.Maybe Lude.Text,
    -- | The details of the last business report delivery for a specified time interval.
    lastBusinessReport :: Lude.Maybe BusinessReport,
    -- | The format of the generated report (individual CSV files or zipped files of individual files).
    format :: Lude.Maybe BusinessReportFormat,
    -- | The recurrence of the reports.
    recurrence :: Lude.Maybe BusinessReportRecurrence,
    -- | The name identifier of the schedule.
    scheduleName :: Lude.Maybe Lude.Text,
    -- | The ARN of the business report schedule.
    scheduleARN :: Lude.Maybe Lude.Text,
    -- | The content range of the reports.
    contentRange :: Lude.Maybe BusinessReportContentRange,
    -- | The S3 bucket name of the output reports.
    s3BucketName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BusinessReportSchedule' with the minimum fields required to make a request.
--
-- * 's3KeyPrefix' - The S3 key where the report is delivered.
-- * 'lastBusinessReport' - The details of the last business report delivery for a specified time interval.
-- * 'format' - The format of the generated report (individual CSV files or zipped files of individual files).
-- * 'recurrence' - The recurrence of the reports.
-- * 'scheduleName' - The name identifier of the schedule.
-- * 'scheduleARN' - The ARN of the business report schedule.
-- * 'contentRange' - The content range of the reports.
-- * 's3BucketName' - The S3 bucket name of the output reports.
mkBusinessReportSchedule ::
  BusinessReportSchedule
mkBusinessReportSchedule =
  BusinessReportSchedule'
    { s3KeyPrefix = Lude.Nothing,
      lastBusinessReport = Lude.Nothing,
      format = Lude.Nothing,
      recurrence = Lude.Nothing,
      scheduleName = Lude.Nothing,
      scheduleARN = Lude.Nothing,
      contentRange = Lude.Nothing,
      s3BucketName = Lude.Nothing
    }

-- | The S3 key where the report is delivered.
--
-- /Note:/ Consider using 's3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsS3KeyPrefix :: Lens.Lens' BusinessReportSchedule (Lude.Maybe Lude.Text)
brsS3KeyPrefix = Lens.lens (s3KeyPrefix :: BusinessReportSchedule -> Lude.Maybe Lude.Text) (\s a -> s {s3KeyPrefix = a} :: BusinessReportSchedule)
{-# DEPRECATED brsS3KeyPrefix "Use generic-lens or generic-optics with 's3KeyPrefix' instead." #-}

-- | The details of the last business report delivery for a specified time interval.
--
-- /Note:/ Consider using 'lastBusinessReport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsLastBusinessReport :: Lens.Lens' BusinessReportSchedule (Lude.Maybe BusinessReport)
brsLastBusinessReport = Lens.lens (lastBusinessReport :: BusinessReportSchedule -> Lude.Maybe BusinessReport) (\s a -> s {lastBusinessReport = a} :: BusinessReportSchedule)
{-# DEPRECATED brsLastBusinessReport "Use generic-lens or generic-optics with 'lastBusinessReport' instead." #-}

-- | The format of the generated report (individual CSV files or zipped files of individual files).
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsFormat :: Lens.Lens' BusinessReportSchedule (Lude.Maybe BusinessReportFormat)
brsFormat = Lens.lens (format :: BusinessReportSchedule -> Lude.Maybe BusinessReportFormat) (\s a -> s {format = a} :: BusinessReportSchedule)
{-# DEPRECATED brsFormat "Use generic-lens or generic-optics with 'format' instead." #-}

-- | The recurrence of the reports.
--
-- /Note:/ Consider using 'recurrence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsRecurrence :: Lens.Lens' BusinessReportSchedule (Lude.Maybe BusinessReportRecurrence)
brsRecurrence = Lens.lens (recurrence :: BusinessReportSchedule -> Lude.Maybe BusinessReportRecurrence) (\s a -> s {recurrence = a} :: BusinessReportSchedule)
{-# DEPRECATED brsRecurrence "Use generic-lens or generic-optics with 'recurrence' instead." #-}

-- | The name identifier of the schedule.
--
-- /Note:/ Consider using 'scheduleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsScheduleName :: Lens.Lens' BusinessReportSchedule (Lude.Maybe Lude.Text)
brsScheduleName = Lens.lens (scheduleName :: BusinessReportSchedule -> Lude.Maybe Lude.Text) (\s a -> s {scheduleName = a} :: BusinessReportSchedule)
{-# DEPRECATED brsScheduleName "Use generic-lens or generic-optics with 'scheduleName' instead." #-}

-- | The ARN of the business report schedule.
--
-- /Note:/ Consider using 'scheduleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsScheduleARN :: Lens.Lens' BusinessReportSchedule (Lude.Maybe Lude.Text)
brsScheduleARN = Lens.lens (scheduleARN :: BusinessReportSchedule -> Lude.Maybe Lude.Text) (\s a -> s {scheduleARN = a} :: BusinessReportSchedule)
{-# DEPRECATED brsScheduleARN "Use generic-lens or generic-optics with 'scheduleARN' instead." #-}

-- | The content range of the reports.
--
-- /Note:/ Consider using 'contentRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsContentRange :: Lens.Lens' BusinessReportSchedule (Lude.Maybe BusinessReportContentRange)
brsContentRange = Lens.lens (contentRange :: BusinessReportSchedule -> Lude.Maybe BusinessReportContentRange) (\s a -> s {contentRange = a} :: BusinessReportSchedule)
{-# DEPRECATED brsContentRange "Use generic-lens or generic-optics with 'contentRange' instead." #-}

-- | The S3 bucket name of the output reports.
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsS3BucketName :: Lens.Lens' BusinessReportSchedule (Lude.Maybe Lude.Text)
brsS3BucketName = Lens.lens (s3BucketName :: BusinessReportSchedule -> Lude.Maybe Lude.Text) (\s a -> s {s3BucketName = a} :: BusinessReportSchedule)
{-# DEPRECATED brsS3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead." #-}

instance Lude.FromJSON BusinessReportSchedule where
  parseJSON =
    Lude.withObject
      "BusinessReportSchedule"
      ( \x ->
          BusinessReportSchedule'
            Lude.<$> (x Lude..:? "S3KeyPrefix")
            Lude.<*> (x Lude..:? "LastBusinessReport")
            Lude.<*> (x Lude..:? "Format")
            Lude.<*> (x Lude..:? "Recurrence")
            Lude.<*> (x Lude..:? "ScheduleName")
            Lude.<*> (x Lude..:? "ScheduleArn")
            Lude.<*> (x Lude..:? "ContentRange")
            Lude.<*> (x Lude..:? "S3BucketName")
      )
