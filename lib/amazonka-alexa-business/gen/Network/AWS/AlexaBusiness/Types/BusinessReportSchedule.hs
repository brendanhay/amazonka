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
    brsContentRange,
    brsFormat,
    brsLastBusinessReport,
    brsRecurrence,
    brsS3BucketName,
    brsS3KeyPrefix,
    brsScheduleArn,
    brsScheduleName,
  )
where

import qualified Network.AWS.AlexaBusiness.Types.Arn as Types
import qualified Network.AWS.AlexaBusiness.Types.BusinessReport as Types
import qualified Network.AWS.AlexaBusiness.Types.BusinessReportContentRange as Types
import qualified Network.AWS.AlexaBusiness.Types.BusinessReportFormat as Types
import qualified Network.AWS.AlexaBusiness.Types.BusinessReportRecurrence as Types
import qualified Network.AWS.AlexaBusiness.Types.BusinessReportScheduleName as Types
import qualified Network.AWS.AlexaBusiness.Types.CustomerS3BucketName as Types
import qualified Network.AWS.AlexaBusiness.Types.S3KeyPrefix as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The schedule of the usage report.
--
-- /See:/ 'mkBusinessReportSchedule' smart constructor.
data BusinessReportSchedule = BusinessReportSchedule'
  { -- | The content range of the reports.
    contentRange :: Core.Maybe Types.BusinessReportContentRange,
    -- | The format of the generated report (individual CSV files or zipped files of individual files).
    format :: Core.Maybe Types.BusinessReportFormat,
    -- | The details of the last business report delivery for a specified time interval.
    lastBusinessReport :: Core.Maybe Types.BusinessReport,
    -- | The recurrence of the reports.
    recurrence :: Core.Maybe Types.BusinessReportRecurrence,
    -- | The S3 bucket name of the output reports.
    s3BucketName :: Core.Maybe Types.CustomerS3BucketName,
    -- | The S3 key where the report is delivered.
    s3KeyPrefix :: Core.Maybe Types.S3KeyPrefix,
    -- | The ARN of the business report schedule.
    scheduleArn :: Core.Maybe Types.Arn,
    -- | The name identifier of the schedule.
    scheduleName :: Core.Maybe Types.BusinessReportScheduleName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BusinessReportSchedule' value with any optional fields omitted.
mkBusinessReportSchedule ::
  BusinessReportSchedule
mkBusinessReportSchedule =
  BusinessReportSchedule'
    { contentRange = Core.Nothing,
      format = Core.Nothing,
      lastBusinessReport = Core.Nothing,
      recurrence = Core.Nothing,
      s3BucketName = Core.Nothing,
      s3KeyPrefix = Core.Nothing,
      scheduleArn = Core.Nothing,
      scheduleName = Core.Nothing
    }

-- | The content range of the reports.
--
-- /Note:/ Consider using 'contentRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsContentRange :: Lens.Lens' BusinessReportSchedule (Core.Maybe Types.BusinessReportContentRange)
brsContentRange = Lens.field @"contentRange"
{-# DEPRECATED brsContentRange "Use generic-lens or generic-optics with 'contentRange' instead." #-}

-- | The format of the generated report (individual CSV files or zipped files of individual files).
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsFormat :: Lens.Lens' BusinessReportSchedule (Core.Maybe Types.BusinessReportFormat)
brsFormat = Lens.field @"format"
{-# DEPRECATED brsFormat "Use generic-lens or generic-optics with 'format' instead." #-}

-- | The details of the last business report delivery for a specified time interval.
--
-- /Note:/ Consider using 'lastBusinessReport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsLastBusinessReport :: Lens.Lens' BusinessReportSchedule (Core.Maybe Types.BusinessReport)
brsLastBusinessReport = Lens.field @"lastBusinessReport"
{-# DEPRECATED brsLastBusinessReport "Use generic-lens or generic-optics with 'lastBusinessReport' instead." #-}

-- | The recurrence of the reports.
--
-- /Note:/ Consider using 'recurrence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsRecurrence :: Lens.Lens' BusinessReportSchedule (Core.Maybe Types.BusinessReportRecurrence)
brsRecurrence = Lens.field @"recurrence"
{-# DEPRECATED brsRecurrence "Use generic-lens or generic-optics with 'recurrence' instead." #-}

-- | The S3 bucket name of the output reports.
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsS3BucketName :: Lens.Lens' BusinessReportSchedule (Core.Maybe Types.CustomerS3BucketName)
brsS3BucketName = Lens.field @"s3BucketName"
{-# DEPRECATED brsS3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead." #-}

-- | The S3 key where the report is delivered.
--
-- /Note:/ Consider using 's3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsS3KeyPrefix :: Lens.Lens' BusinessReportSchedule (Core.Maybe Types.S3KeyPrefix)
brsS3KeyPrefix = Lens.field @"s3KeyPrefix"
{-# DEPRECATED brsS3KeyPrefix "Use generic-lens or generic-optics with 's3KeyPrefix' instead." #-}

-- | The ARN of the business report schedule.
--
-- /Note:/ Consider using 'scheduleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsScheduleArn :: Lens.Lens' BusinessReportSchedule (Core.Maybe Types.Arn)
brsScheduleArn = Lens.field @"scheduleArn"
{-# DEPRECATED brsScheduleArn "Use generic-lens or generic-optics with 'scheduleArn' instead." #-}

-- | The name identifier of the schedule.
--
-- /Note:/ Consider using 'scheduleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsScheduleName :: Lens.Lens' BusinessReportSchedule (Core.Maybe Types.BusinessReportScheduleName)
brsScheduleName = Lens.field @"scheduleName"
{-# DEPRECATED brsScheduleName "Use generic-lens or generic-optics with 'scheduleName' instead." #-}

instance Core.FromJSON BusinessReportSchedule where
  parseJSON =
    Core.withObject "BusinessReportSchedule" Core.$
      \x ->
        BusinessReportSchedule'
          Core.<$> (x Core..:? "ContentRange")
          Core.<*> (x Core..:? "Format")
          Core.<*> (x Core..:? "LastBusinessReport")
          Core.<*> (x Core..:? "Recurrence")
          Core.<*> (x Core..:? "S3BucketName")
          Core.<*> (x Core..:? "S3KeyPrefix")
          Core.<*> (x Core..:? "ScheduleArn")
          Core.<*> (x Core..:? "ScheduleName")
