{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeThingRegistrationTask (..),
    mkDescribeThingRegistrationTask,

    -- ** Request lenses
    dtrtTaskId,

    -- * Destructuring the response
    DescribeThingRegistrationTaskResponse (..),
    mkDescribeThingRegistrationTaskResponse,

    -- ** Response lenses
    dtrtrrsCreationDate,
    dtrtrrsFailureCount,
    dtrtrrsInputFileBucket,
    dtrtrrsInputFileKey,
    dtrtrrsLastModifiedDate,
    dtrtrrsMessage,
    dtrtrrsPercentageProgress,
    dtrtrrsRoleArn,
    dtrtrrsStatus,
    dtrtrrsSuccessCount,
    dtrtrrsTaskId,
    dtrtrrsTemplateBody,
    dtrtrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeThingRegistrationTask' smart constructor.
newtype DescribeThingRegistrationTask = DescribeThingRegistrationTask'
  { -- | The task ID.
    taskId :: Types.TaskId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeThingRegistrationTask' value with any optional fields omitted.
mkDescribeThingRegistrationTask ::
  -- | 'taskId'
  Types.TaskId ->
  DescribeThingRegistrationTask
mkDescribeThingRegistrationTask taskId =
  DescribeThingRegistrationTask' {taskId}

-- | The task ID.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrtTaskId :: Lens.Lens' DescribeThingRegistrationTask Types.TaskId
dtrtTaskId = Lens.field @"taskId"
{-# DEPRECATED dtrtTaskId "Use generic-lens or generic-optics with 'taskId' instead." #-}

instance Core.AWSRequest DescribeThingRegistrationTask where
  type
    Rs DescribeThingRegistrationTask =
      DescribeThingRegistrationTaskResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ("/thing-registration-tasks/" Core.<> (Core.toText taskId)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeThingRegistrationTaskResponse'
            Core.<$> (x Core..:? "creationDate")
            Core.<*> (x Core..:? "failureCount")
            Core.<*> (x Core..:? "inputFileBucket")
            Core.<*> (x Core..:? "inputFileKey")
            Core.<*> (x Core..:? "lastModifiedDate")
            Core.<*> (x Core..:? "message")
            Core.<*> (x Core..:? "percentageProgress")
            Core.<*> (x Core..:? "roleArn")
            Core.<*> (x Core..:? "status")
            Core.<*> (x Core..:? "successCount")
            Core.<*> (x Core..:? "taskId")
            Core.<*> (x Core..:? "templateBody")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeThingRegistrationTaskResponse' smart constructor.
data DescribeThingRegistrationTaskResponse = DescribeThingRegistrationTaskResponse'
  { -- | The task creation date.
    creationDate :: Core.Maybe Core.NominalDiffTime,
    -- | The number of things that failed to be provisioned.
    failureCount :: Core.Maybe Core.Int,
    -- | The S3 bucket that contains the input file.
    inputFileBucket :: Core.Maybe Types.RegistryS3BucketName,
    -- | The input file key.
    inputFileKey :: Core.Maybe Types.RegistryS3KeyName,
    -- | The date when the task was last modified.
    lastModifiedDate :: Core.Maybe Core.NominalDiffTime,
    -- | The message.
    message :: Core.Maybe Types.ErrorMessage,
    -- | The progress of the bulk provisioning task expressed as a percentage.
    percentageProgress :: Core.Maybe Core.Natural,
    -- | The role ARN that grants access to the input file bucket.
    roleArn :: Core.Maybe Types.RoleArn,
    -- | The status of the bulk thing provisioning task.
    status :: Core.Maybe Types.TaskStatus,
    -- | The number of things successfully provisioned.
    successCount :: Core.Maybe Core.Int,
    -- | The task ID.
    taskId :: Core.Maybe Types.TaskId,
    -- | The task's template.
    templateBody :: Core.Maybe Types.TemplateBody,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeThingRegistrationTaskResponse' value with any optional fields omitted.
mkDescribeThingRegistrationTaskResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeThingRegistrationTaskResponse
mkDescribeThingRegistrationTaskResponse responseStatus =
  DescribeThingRegistrationTaskResponse'
    { creationDate =
        Core.Nothing,
      failureCount = Core.Nothing,
      inputFileBucket = Core.Nothing,
      inputFileKey = Core.Nothing,
      lastModifiedDate = Core.Nothing,
      message = Core.Nothing,
      percentageProgress = Core.Nothing,
      roleArn = Core.Nothing,
      status = Core.Nothing,
      successCount = Core.Nothing,
      taskId = Core.Nothing,
      templateBody = Core.Nothing,
      responseStatus
    }

-- | The task creation date.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrtrrsCreationDate :: Lens.Lens' DescribeThingRegistrationTaskResponse (Core.Maybe Core.NominalDiffTime)
dtrtrrsCreationDate = Lens.field @"creationDate"
{-# DEPRECATED dtrtrrsCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The number of things that failed to be provisioned.
--
-- /Note:/ Consider using 'failureCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrtrrsFailureCount :: Lens.Lens' DescribeThingRegistrationTaskResponse (Core.Maybe Core.Int)
dtrtrrsFailureCount = Lens.field @"failureCount"
{-# DEPRECATED dtrtrrsFailureCount "Use generic-lens or generic-optics with 'failureCount' instead." #-}

-- | The S3 bucket that contains the input file.
--
-- /Note:/ Consider using 'inputFileBucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrtrrsInputFileBucket :: Lens.Lens' DescribeThingRegistrationTaskResponse (Core.Maybe Types.RegistryS3BucketName)
dtrtrrsInputFileBucket = Lens.field @"inputFileBucket"
{-# DEPRECATED dtrtrrsInputFileBucket "Use generic-lens or generic-optics with 'inputFileBucket' instead." #-}

-- | The input file key.
--
-- /Note:/ Consider using 'inputFileKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrtrrsInputFileKey :: Lens.Lens' DescribeThingRegistrationTaskResponse (Core.Maybe Types.RegistryS3KeyName)
dtrtrrsInputFileKey = Lens.field @"inputFileKey"
{-# DEPRECATED dtrtrrsInputFileKey "Use generic-lens or generic-optics with 'inputFileKey' instead." #-}

-- | The date when the task was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrtrrsLastModifiedDate :: Lens.Lens' DescribeThingRegistrationTaskResponse (Core.Maybe Core.NominalDiffTime)
dtrtrrsLastModifiedDate = Lens.field @"lastModifiedDate"
{-# DEPRECATED dtrtrrsLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrtrrsMessage :: Lens.Lens' DescribeThingRegistrationTaskResponse (Core.Maybe Types.ErrorMessage)
dtrtrrsMessage = Lens.field @"message"
{-# DEPRECATED dtrtrrsMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The progress of the bulk provisioning task expressed as a percentage.
--
-- /Note:/ Consider using 'percentageProgress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrtrrsPercentageProgress :: Lens.Lens' DescribeThingRegistrationTaskResponse (Core.Maybe Core.Natural)
dtrtrrsPercentageProgress = Lens.field @"percentageProgress"
{-# DEPRECATED dtrtrrsPercentageProgress "Use generic-lens or generic-optics with 'percentageProgress' instead." #-}

-- | The role ARN that grants access to the input file bucket.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrtrrsRoleArn :: Lens.Lens' DescribeThingRegistrationTaskResponse (Core.Maybe Types.RoleArn)
dtrtrrsRoleArn = Lens.field @"roleArn"
{-# DEPRECATED dtrtrrsRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | The status of the bulk thing provisioning task.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrtrrsStatus :: Lens.Lens' DescribeThingRegistrationTaskResponse (Core.Maybe Types.TaskStatus)
dtrtrrsStatus = Lens.field @"status"
{-# DEPRECATED dtrtrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The number of things successfully provisioned.
--
-- /Note:/ Consider using 'successCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrtrrsSuccessCount :: Lens.Lens' DescribeThingRegistrationTaskResponse (Core.Maybe Core.Int)
dtrtrrsSuccessCount = Lens.field @"successCount"
{-# DEPRECATED dtrtrrsSuccessCount "Use generic-lens or generic-optics with 'successCount' instead." #-}

-- | The task ID.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrtrrsTaskId :: Lens.Lens' DescribeThingRegistrationTaskResponse (Core.Maybe Types.TaskId)
dtrtrrsTaskId = Lens.field @"taskId"
{-# DEPRECATED dtrtrrsTaskId "Use generic-lens or generic-optics with 'taskId' instead." #-}

-- | The task's template.
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrtrrsTemplateBody :: Lens.Lens' DescribeThingRegistrationTaskResponse (Core.Maybe Types.TemplateBody)
dtrtrrsTemplateBody = Lens.field @"templateBody"
{-# DEPRECATED dtrtrrsTemplateBody "Use generic-lens or generic-optics with 'templateBody' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrtrrsResponseStatus :: Lens.Lens' DescribeThingRegistrationTaskResponse Core.Int
dtrtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtrtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
