{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.UpdateBusinessReportSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration of the report delivery schedule with the specified schedule ARN.
module Network.AWS.AlexaBusiness.UpdateBusinessReportSchedule
    (
    -- * Creating a request
      UpdateBusinessReportSchedule (..)
    , mkUpdateBusinessReportSchedule
    -- ** Request lenses
    , ubrsScheduleArn
    , ubrsFormat
    , ubrsRecurrence
    , ubrsS3BucketName
    , ubrsS3KeyPrefix
    , ubrsScheduleName

    -- * Destructuring the response
    , UpdateBusinessReportScheduleResponse (..)
    , mkUpdateBusinessReportScheduleResponse
    -- ** Response lenses
    , ubrsrrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateBusinessReportSchedule' smart constructor.
data UpdateBusinessReportSchedule = UpdateBusinessReportSchedule'
  { scheduleArn :: Types.Arn
    -- ^ The ARN of the business report schedule.
  , format :: Core.Maybe Types.BusinessReportFormat
    -- ^ The format of the generated report (individual CSV files or zipped files of individual files).
  , recurrence :: Core.Maybe Types.BusinessReportRecurrence
    -- ^ The recurrence of the reports.
  , s3BucketName :: Core.Maybe Types.CustomerS3BucketName
    -- ^ The S3 location of the output reports.
  , s3KeyPrefix :: Core.Maybe Types.S3KeyPrefix
    -- ^ The S3 key where the report is delivered.
  , scheduleName :: Core.Maybe Types.BusinessReportScheduleName
    -- ^ The name identifier of the schedule.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateBusinessReportSchedule' value with any optional fields omitted.
mkUpdateBusinessReportSchedule
    :: Types.Arn -- ^ 'scheduleArn'
    -> UpdateBusinessReportSchedule
mkUpdateBusinessReportSchedule scheduleArn
  = UpdateBusinessReportSchedule'{scheduleArn, format = Core.Nothing,
                                  recurrence = Core.Nothing, s3BucketName = Core.Nothing,
                                  s3KeyPrefix = Core.Nothing, scheduleName = Core.Nothing}

-- | The ARN of the business report schedule.
--
-- /Note:/ Consider using 'scheduleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrsScheduleArn :: Lens.Lens' UpdateBusinessReportSchedule Types.Arn
ubrsScheduleArn = Lens.field @"scheduleArn"
{-# INLINEABLE ubrsScheduleArn #-}
{-# DEPRECATED scheduleArn "Use generic-lens or generic-optics with 'scheduleArn' instead"  #-}

-- | The format of the generated report (individual CSV files or zipped files of individual files).
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrsFormat :: Lens.Lens' UpdateBusinessReportSchedule (Core.Maybe Types.BusinessReportFormat)
ubrsFormat = Lens.field @"format"
{-# INLINEABLE ubrsFormat #-}
{-# DEPRECATED format "Use generic-lens or generic-optics with 'format' instead"  #-}

-- | The recurrence of the reports.
--
-- /Note:/ Consider using 'recurrence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrsRecurrence :: Lens.Lens' UpdateBusinessReportSchedule (Core.Maybe Types.BusinessReportRecurrence)
ubrsRecurrence = Lens.field @"recurrence"
{-# INLINEABLE ubrsRecurrence #-}
{-# DEPRECATED recurrence "Use generic-lens or generic-optics with 'recurrence' instead"  #-}

-- | The S3 location of the output reports.
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrsS3BucketName :: Lens.Lens' UpdateBusinessReportSchedule (Core.Maybe Types.CustomerS3BucketName)
ubrsS3BucketName = Lens.field @"s3BucketName"
{-# INLINEABLE ubrsS3BucketName #-}
{-# DEPRECATED s3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead"  #-}

-- | The S3 key where the report is delivered.
--
-- /Note:/ Consider using 's3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrsS3KeyPrefix :: Lens.Lens' UpdateBusinessReportSchedule (Core.Maybe Types.S3KeyPrefix)
ubrsS3KeyPrefix = Lens.field @"s3KeyPrefix"
{-# INLINEABLE ubrsS3KeyPrefix #-}
{-# DEPRECATED s3KeyPrefix "Use generic-lens or generic-optics with 's3KeyPrefix' instead"  #-}

-- | The name identifier of the schedule.
--
-- /Note:/ Consider using 'scheduleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrsScheduleName :: Lens.Lens' UpdateBusinessReportSchedule (Core.Maybe Types.BusinessReportScheduleName)
ubrsScheduleName = Lens.field @"scheduleName"
{-# INLINEABLE ubrsScheduleName #-}
{-# DEPRECATED scheduleName "Use generic-lens or generic-optics with 'scheduleName' instead"  #-}

instance Core.ToQuery UpdateBusinessReportSchedule where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateBusinessReportSchedule where
        toHeaders UpdateBusinessReportSchedule{..}
          = Core.pure
              ("X-Amz-Target", "AlexaForBusiness.UpdateBusinessReportSchedule")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateBusinessReportSchedule where
        toJSON UpdateBusinessReportSchedule{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ScheduleArn" Core..= scheduleArn),
                  ("Format" Core..=) Core.<$> format,
                  ("Recurrence" Core..=) Core.<$> recurrence,
                  ("S3BucketName" Core..=) Core.<$> s3BucketName,
                  ("S3KeyPrefix" Core..=) Core.<$> s3KeyPrefix,
                  ("ScheduleName" Core..=) Core.<$> scheduleName])

instance Core.AWSRequest UpdateBusinessReportSchedule where
        type Rs UpdateBusinessReportSchedule =
             UpdateBusinessReportScheduleResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateBusinessReportScheduleResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateBusinessReportScheduleResponse' smart constructor.
newtype UpdateBusinessReportScheduleResponse = UpdateBusinessReportScheduleResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateBusinessReportScheduleResponse' value with any optional fields omitted.
mkUpdateBusinessReportScheduleResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateBusinessReportScheduleResponse
mkUpdateBusinessReportScheduleResponse responseStatus
  = UpdateBusinessReportScheduleResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrsrrsResponseStatus :: Lens.Lens' UpdateBusinessReportScheduleResponse Core.Int
ubrsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ubrsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
