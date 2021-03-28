{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeThingRegistrationTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a bulk thing provisioning task.
module Network.AWS.IoT.DescribeThingRegistrationTask
    (
    -- * Creating a request
      DescribeThingRegistrationTask (..)
    , mkDescribeThingRegistrationTask
    -- ** Request lenses
    , dtrtTaskId

    -- * Destructuring the response
    , DescribeThingRegistrationTaskResponse (..)
    , mkDescribeThingRegistrationTaskResponse
    -- ** Response lenses
    , dtrtrrsCreationDate
    , dtrtrrsFailureCount
    , dtrtrrsInputFileBucket
    , dtrtrrsInputFileKey
    , dtrtrrsLastModifiedDate
    , dtrtrrsMessage
    , dtrtrrsPercentageProgress
    , dtrtrrsRoleArn
    , dtrtrrsStatus
    , dtrtrrsSuccessCount
    , dtrtrrsTaskId
    , dtrtrrsTemplateBody
    , dtrtrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeThingRegistrationTask' smart constructor.
newtype DescribeThingRegistrationTask = DescribeThingRegistrationTask'
  { taskId :: Types.TaskId
    -- ^ The task ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeThingRegistrationTask' value with any optional fields omitted.
mkDescribeThingRegistrationTask
    :: Types.TaskId -- ^ 'taskId'
    -> DescribeThingRegistrationTask
mkDescribeThingRegistrationTask taskId
  = DescribeThingRegistrationTask'{taskId}

-- | The task ID.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrtTaskId :: Lens.Lens' DescribeThingRegistrationTask Types.TaskId
dtrtTaskId = Lens.field @"taskId"
{-# INLINEABLE dtrtTaskId #-}
{-# DEPRECATED taskId "Use generic-lens or generic-optics with 'taskId' instead"  #-}

instance Core.ToQuery DescribeThingRegistrationTask where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeThingRegistrationTask where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeThingRegistrationTask where
        type Rs DescribeThingRegistrationTask =
             DescribeThingRegistrationTaskResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/thing-registration-tasks/" Core.<> Core.toText taskId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeThingRegistrationTaskResponse' Core.<$>
                   (x Core..:? "creationDate") Core.<*> x Core..:? "failureCount"
                     Core.<*> x Core..:? "inputFileBucket"
                     Core.<*> x Core..:? "inputFileKey"
                     Core.<*> x Core..:? "lastModifiedDate"
                     Core.<*> x Core..:? "message"
                     Core.<*> x Core..:? "percentageProgress"
                     Core.<*> x Core..:? "roleArn"
                     Core.<*> x Core..:? "status"
                     Core.<*> x Core..:? "successCount"
                     Core.<*> x Core..:? "taskId"
                     Core.<*> x Core..:? "templateBody"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeThingRegistrationTaskResponse' smart constructor.
data DescribeThingRegistrationTaskResponse = DescribeThingRegistrationTaskResponse'
  { creationDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The task creation date.
  , failureCount :: Core.Maybe Core.Int
    -- ^ The number of things that failed to be provisioned.
  , inputFileBucket :: Core.Maybe Types.RegistryS3BucketName
    -- ^ The S3 bucket that contains the input file.
  , inputFileKey :: Core.Maybe Types.RegistryS3KeyName
    -- ^ The input file key.
  , lastModifiedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date when the task was last modified.
  , message :: Core.Maybe Types.ErrorMessage
    -- ^ The message.
  , percentageProgress :: Core.Maybe Core.Natural
    -- ^ The progress of the bulk provisioning task expressed as a percentage.
  , roleArn :: Core.Maybe Types.RoleArn
    -- ^ The role ARN that grants access to the input file bucket.
  , status :: Core.Maybe Types.TaskStatus
    -- ^ The status of the bulk thing provisioning task.
  , successCount :: Core.Maybe Core.Int
    -- ^ The number of things successfully provisioned.
  , taskId :: Core.Maybe Types.TaskId
    -- ^ The task ID.
  , templateBody :: Core.Maybe Types.TemplateBody
    -- ^ The task's template.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeThingRegistrationTaskResponse' value with any optional fields omitted.
mkDescribeThingRegistrationTaskResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeThingRegistrationTaskResponse
mkDescribeThingRegistrationTaskResponse responseStatus
  = DescribeThingRegistrationTaskResponse'{creationDate =
                                             Core.Nothing,
                                           failureCount = Core.Nothing,
                                           inputFileBucket = Core.Nothing,
                                           inputFileKey = Core.Nothing,
                                           lastModifiedDate = Core.Nothing, message = Core.Nothing,
                                           percentageProgress = Core.Nothing,
                                           roleArn = Core.Nothing, status = Core.Nothing,
                                           successCount = Core.Nothing, taskId = Core.Nothing,
                                           templateBody = Core.Nothing, responseStatus}

-- | The task creation date.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrtrrsCreationDate :: Lens.Lens' DescribeThingRegistrationTaskResponse (Core.Maybe Core.NominalDiffTime)
dtrtrrsCreationDate = Lens.field @"creationDate"
{-# INLINEABLE dtrtrrsCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

-- | The number of things that failed to be provisioned.
--
-- /Note:/ Consider using 'failureCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrtrrsFailureCount :: Lens.Lens' DescribeThingRegistrationTaskResponse (Core.Maybe Core.Int)
dtrtrrsFailureCount = Lens.field @"failureCount"
{-# INLINEABLE dtrtrrsFailureCount #-}
{-# DEPRECATED failureCount "Use generic-lens or generic-optics with 'failureCount' instead"  #-}

-- | The S3 bucket that contains the input file.
--
-- /Note:/ Consider using 'inputFileBucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrtrrsInputFileBucket :: Lens.Lens' DescribeThingRegistrationTaskResponse (Core.Maybe Types.RegistryS3BucketName)
dtrtrrsInputFileBucket = Lens.field @"inputFileBucket"
{-# INLINEABLE dtrtrrsInputFileBucket #-}
{-# DEPRECATED inputFileBucket "Use generic-lens or generic-optics with 'inputFileBucket' instead"  #-}

-- | The input file key.
--
-- /Note:/ Consider using 'inputFileKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrtrrsInputFileKey :: Lens.Lens' DescribeThingRegistrationTaskResponse (Core.Maybe Types.RegistryS3KeyName)
dtrtrrsInputFileKey = Lens.field @"inputFileKey"
{-# INLINEABLE dtrtrrsInputFileKey #-}
{-# DEPRECATED inputFileKey "Use generic-lens or generic-optics with 'inputFileKey' instead"  #-}

-- | The date when the task was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrtrrsLastModifiedDate :: Lens.Lens' DescribeThingRegistrationTaskResponse (Core.Maybe Core.NominalDiffTime)
dtrtrrsLastModifiedDate = Lens.field @"lastModifiedDate"
{-# INLINEABLE dtrtrrsLastModifiedDate #-}
{-# DEPRECATED lastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead"  #-}

-- | The message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrtrrsMessage :: Lens.Lens' DescribeThingRegistrationTaskResponse (Core.Maybe Types.ErrorMessage)
dtrtrrsMessage = Lens.field @"message"
{-# INLINEABLE dtrtrrsMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

-- | The progress of the bulk provisioning task expressed as a percentage.
--
-- /Note:/ Consider using 'percentageProgress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrtrrsPercentageProgress :: Lens.Lens' DescribeThingRegistrationTaskResponse (Core.Maybe Core.Natural)
dtrtrrsPercentageProgress = Lens.field @"percentageProgress"
{-# INLINEABLE dtrtrrsPercentageProgress #-}
{-# DEPRECATED percentageProgress "Use generic-lens or generic-optics with 'percentageProgress' instead"  #-}

-- | The role ARN that grants access to the input file bucket.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrtrrsRoleArn :: Lens.Lens' DescribeThingRegistrationTaskResponse (Core.Maybe Types.RoleArn)
dtrtrrsRoleArn = Lens.field @"roleArn"
{-# INLINEABLE dtrtrrsRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | The status of the bulk thing provisioning task.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrtrrsStatus :: Lens.Lens' DescribeThingRegistrationTaskResponse (Core.Maybe Types.TaskStatus)
dtrtrrsStatus = Lens.field @"status"
{-# INLINEABLE dtrtrrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The number of things successfully provisioned.
--
-- /Note:/ Consider using 'successCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrtrrsSuccessCount :: Lens.Lens' DescribeThingRegistrationTaskResponse (Core.Maybe Core.Int)
dtrtrrsSuccessCount = Lens.field @"successCount"
{-# INLINEABLE dtrtrrsSuccessCount #-}
{-# DEPRECATED successCount "Use generic-lens or generic-optics with 'successCount' instead"  #-}

-- | The task ID.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrtrrsTaskId :: Lens.Lens' DescribeThingRegistrationTaskResponse (Core.Maybe Types.TaskId)
dtrtrrsTaskId = Lens.field @"taskId"
{-# INLINEABLE dtrtrrsTaskId #-}
{-# DEPRECATED taskId "Use generic-lens or generic-optics with 'taskId' instead"  #-}

-- | The task's template.
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrtrrsTemplateBody :: Lens.Lens' DescribeThingRegistrationTaskResponse (Core.Maybe Types.TemplateBody)
dtrtrrsTemplateBody = Lens.field @"templateBody"
{-# INLINEABLE dtrtrrsTemplateBody #-}
{-# DEPRECATED templateBody "Use generic-lens or generic-optics with 'templateBody' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrtrrsResponseStatus :: Lens.Lens' DescribeThingRegistrationTaskResponse Core.Int
dtrtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtrtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
