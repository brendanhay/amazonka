{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.CreateBusinessReportSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a recurring schedule for usage reports to deliver to the specified S3 location with a specified daily or weekly interval.
module Network.AWS.AlexaBusiness.CreateBusinessReportSchedule
    (
    -- * Creating a request
      CreateBusinessReportSchedule (..)
    , mkCreateBusinessReportSchedule
    -- ** Request lenses
    , cbrsFormat
    , cbrsContentRange
    , cbrsClientRequestToken
    , cbrsRecurrence
    , cbrsS3BucketName
    , cbrsS3KeyPrefix
    , cbrsScheduleName
    , cbrsTags

    -- * Destructuring the response
    , CreateBusinessReportScheduleResponse (..)
    , mkCreateBusinessReportScheduleResponse
    -- ** Response lenses
    , cbrsrrsScheduleArn
    , cbrsrrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateBusinessReportSchedule' smart constructor.
data CreateBusinessReportSchedule = CreateBusinessReportSchedule'
  { format :: Types.BusinessReportFormat
    -- ^ The format of the generated report (individual CSV files or zipped files of individual files).
  , contentRange :: Types.BusinessReportContentRange
    -- ^ The content range of the reports.
  , clientRequestToken :: Core.Maybe Types.ClientRequestToken
    -- ^ The client request token.
  , recurrence :: Core.Maybe Types.BusinessReportRecurrence
    -- ^ The recurrence of the reports. If this isn't specified, the report will only be delivered one time when the API is called. 
  , s3BucketName :: Core.Maybe Types.CustomerS3BucketName
    -- ^ The S3 bucket name of the output reports. If this isn't specified, the report can be retrieved from a download link by calling ListBusinessReportSchedule. 
  , s3KeyPrefix :: Core.Maybe Types.S3KeyPrefix
    -- ^ The S3 key where the report is delivered.
  , scheduleName :: Core.Maybe Types.BusinessReportScheduleName
    -- ^ The name identifier of the schedule.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags for the business report schedule.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateBusinessReportSchedule' value with any optional fields omitted.
mkCreateBusinessReportSchedule
    :: Types.BusinessReportFormat -- ^ 'format'
    -> Types.BusinessReportContentRange -- ^ 'contentRange'
    -> CreateBusinessReportSchedule
mkCreateBusinessReportSchedule format contentRange
  = CreateBusinessReportSchedule'{format, contentRange,
                                  clientRequestToken = Core.Nothing, recurrence = Core.Nothing,
                                  s3BucketName = Core.Nothing, s3KeyPrefix = Core.Nothing,
                                  scheduleName = Core.Nothing, tags = Core.Nothing}

-- | The format of the generated report (individual CSV files or zipped files of individual files).
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrsFormat :: Lens.Lens' CreateBusinessReportSchedule Types.BusinessReportFormat
cbrsFormat = Lens.field @"format"
{-# INLINEABLE cbrsFormat #-}
{-# DEPRECATED format "Use generic-lens or generic-optics with 'format' instead"  #-}

-- | The content range of the reports.
--
-- /Note:/ Consider using 'contentRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrsContentRange :: Lens.Lens' CreateBusinessReportSchedule Types.BusinessReportContentRange
cbrsContentRange = Lens.field @"contentRange"
{-# INLINEABLE cbrsContentRange #-}
{-# DEPRECATED contentRange "Use generic-lens or generic-optics with 'contentRange' instead"  #-}

-- | The client request token.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrsClientRequestToken :: Lens.Lens' CreateBusinessReportSchedule (Core.Maybe Types.ClientRequestToken)
cbrsClientRequestToken = Lens.field @"clientRequestToken"
{-# INLINEABLE cbrsClientRequestToken #-}
{-# DEPRECATED clientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead"  #-}

-- | The recurrence of the reports. If this isn't specified, the report will only be delivered one time when the API is called. 
--
-- /Note:/ Consider using 'recurrence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrsRecurrence :: Lens.Lens' CreateBusinessReportSchedule (Core.Maybe Types.BusinessReportRecurrence)
cbrsRecurrence = Lens.field @"recurrence"
{-# INLINEABLE cbrsRecurrence #-}
{-# DEPRECATED recurrence "Use generic-lens or generic-optics with 'recurrence' instead"  #-}

-- | The S3 bucket name of the output reports. If this isn't specified, the report can be retrieved from a download link by calling ListBusinessReportSchedule. 
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrsS3BucketName :: Lens.Lens' CreateBusinessReportSchedule (Core.Maybe Types.CustomerS3BucketName)
cbrsS3BucketName = Lens.field @"s3BucketName"
{-# INLINEABLE cbrsS3BucketName #-}
{-# DEPRECATED s3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead"  #-}

-- | The S3 key where the report is delivered.
--
-- /Note:/ Consider using 's3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrsS3KeyPrefix :: Lens.Lens' CreateBusinessReportSchedule (Core.Maybe Types.S3KeyPrefix)
cbrsS3KeyPrefix = Lens.field @"s3KeyPrefix"
{-# INLINEABLE cbrsS3KeyPrefix #-}
{-# DEPRECATED s3KeyPrefix "Use generic-lens or generic-optics with 's3KeyPrefix' instead"  #-}

-- | The name identifier of the schedule.
--
-- /Note:/ Consider using 'scheduleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrsScheduleName :: Lens.Lens' CreateBusinessReportSchedule (Core.Maybe Types.BusinessReportScheduleName)
cbrsScheduleName = Lens.field @"scheduleName"
{-# INLINEABLE cbrsScheduleName #-}
{-# DEPRECATED scheduleName "Use generic-lens or generic-optics with 'scheduleName' instead"  #-}

-- | The tags for the business report schedule.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrsTags :: Lens.Lens' CreateBusinessReportSchedule (Core.Maybe [Types.Tag])
cbrsTags = Lens.field @"tags"
{-# INLINEABLE cbrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateBusinessReportSchedule where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateBusinessReportSchedule where
        toHeaders CreateBusinessReportSchedule{..}
          = Core.pure
              ("X-Amz-Target", "AlexaForBusiness.CreateBusinessReportSchedule")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateBusinessReportSchedule where
        toJSON CreateBusinessReportSchedule{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Format" Core..= format),
                  Core.Just ("ContentRange" Core..= contentRange),
                  ("ClientRequestToken" Core..=) Core.<$> clientRequestToken,
                  ("Recurrence" Core..=) Core.<$> recurrence,
                  ("S3BucketName" Core..=) Core.<$> s3BucketName,
                  ("S3KeyPrefix" Core..=) Core.<$> s3KeyPrefix,
                  ("ScheduleName" Core..=) Core.<$> scheduleName,
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateBusinessReportSchedule where
        type Rs CreateBusinessReportSchedule =
             CreateBusinessReportScheduleResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateBusinessReportScheduleResponse' Core.<$>
                   (x Core..:? "ScheduleArn") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateBusinessReportScheduleResponse' smart constructor.
data CreateBusinessReportScheduleResponse = CreateBusinessReportScheduleResponse'
  { scheduleArn :: Core.Maybe Types.ScheduleArn
    -- ^ The ARN of the business report schedule.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateBusinessReportScheduleResponse' value with any optional fields omitted.
mkCreateBusinessReportScheduleResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateBusinessReportScheduleResponse
mkCreateBusinessReportScheduleResponse responseStatus
  = CreateBusinessReportScheduleResponse'{scheduleArn = Core.Nothing,
                                          responseStatus}

-- | The ARN of the business report schedule.
--
-- /Note:/ Consider using 'scheduleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrsrrsScheduleArn :: Lens.Lens' CreateBusinessReportScheduleResponse (Core.Maybe Types.ScheduleArn)
cbrsrrsScheduleArn = Lens.field @"scheduleArn"
{-# INLINEABLE cbrsrrsScheduleArn #-}
{-# DEPRECATED scheduleArn "Use generic-lens or generic-optics with 'scheduleArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrsrrsResponseStatus :: Lens.Lens' CreateBusinessReportScheduleResponse Core.Int
cbrsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cbrsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
