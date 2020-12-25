{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a job.
module Network.AWS.IoT.CreateJob
  ( -- * Creating a request
    CreateJob (..),
    mkCreateJob,

    -- ** Request lenses
    cjJobId,
    cjTargets,
    cjAbortConfig,
    cjDescription,
    cjDocument,
    cjDocumentSource,
    cjJobExecutionsRolloutConfig,
    cjNamespaceId,
    cjPresignedUrlConfig,
    cjTags,
    cjTargetSelection,
    cjTimeoutConfig,

    -- * Destructuring the response
    CreateJobResponse (..),
    mkCreateJobResponse,

    -- ** Response lenses
    cjrrsDescription,
    cjrrsJobArn,
    cjrrsJobId,
    cjrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateJob' smart constructor.
data CreateJob = CreateJob'
  { -- | A job identifier which must be unique for your AWS account. We recommend using a UUID. Alpha-numeric characters, "-" and "_" are valid for use here.
    jobId :: Types.JobId,
    -- | A list of things and thing groups to which the job should be sent.
    targets :: Core.NonEmpty Types.TargetArn,
    -- | Allows you to create criteria to abort a job.
    abortConfig :: Core.Maybe Types.AbortConfig,
    -- | A short text description of the job.
    description :: Core.Maybe Types.Description,
    -- | The job document.
    document :: Core.Maybe Types.Document,
    -- | An S3 link to the job document.
    documentSource :: Core.Maybe Types.DocumentSource,
    -- | Allows you to create a staged rollout of the job.
    jobExecutionsRolloutConfig :: Core.Maybe Types.JobExecutionsRolloutConfig,
    -- | The namespace used to indicate that a job is a customer-managed job.
    --
    -- When you specify a value for this parameter, AWS IoT Core sends jobs notifications to MQTT topics that contain the value in the following format.
    -- @> aws/things//THING_NAME/ /jobs//JOB_ID/ /notify-namespace-/NAMESPACE_ID/ /@
    namespaceId :: Core.Maybe Types.NamespaceId,
    -- | Configuration information for pre-signed S3 URLs.
    presignedUrlConfig :: Core.Maybe Types.PresignedUrlConfig,
    -- | Metadata which can be used to manage the job.
    tags :: Core.Maybe [Types.Tag],
    -- | Specifies whether the job will continue to run (CONTINUOUS), or will be complete after all those things specified as targets have completed the job (SNAPSHOT). If continuous, the job may also be run on a thing when a change is detected in a target. For example, a job will run on a thing when the thing is added to a target group, even after the job was completed by all things originally in the group.
    targetSelection :: Core.Maybe Types.TargetSelection,
    -- | Specifies the amount of time each device has to finish its execution of the job. The timer is started when the job execution status is set to @IN_PROGRESS@ . If the job execution status is not set to another terminal state before the time expires, it will be automatically set to @TIMED_OUT@ .
    timeoutConfig :: Core.Maybe Types.TimeoutConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateJob' value with any optional fields omitted.
mkCreateJob ::
  -- | 'jobId'
  Types.JobId ->
  -- | 'targets'
  Core.NonEmpty Types.TargetArn ->
  CreateJob
mkCreateJob jobId targets =
  CreateJob'
    { jobId,
      targets,
      abortConfig = Core.Nothing,
      description = Core.Nothing,
      document = Core.Nothing,
      documentSource = Core.Nothing,
      jobExecutionsRolloutConfig = Core.Nothing,
      namespaceId = Core.Nothing,
      presignedUrlConfig = Core.Nothing,
      tags = Core.Nothing,
      targetSelection = Core.Nothing,
      timeoutConfig = Core.Nothing
    }

-- | A job identifier which must be unique for your AWS account. We recommend using a UUID. Alpha-numeric characters, "-" and "_" are valid for use here.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjJobId :: Lens.Lens' CreateJob Types.JobId
cjJobId = Lens.field @"jobId"
{-# DEPRECATED cjJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | A list of things and thing groups to which the job should be sent.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjTargets :: Lens.Lens' CreateJob (Core.NonEmpty Types.TargetArn)
cjTargets = Lens.field @"targets"
{-# DEPRECATED cjTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | Allows you to create criteria to abort a job.
--
-- /Note:/ Consider using 'abortConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjAbortConfig :: Lens.Lens' CreateJob (Core.Maybe Types.AbortConfig)
cjAbortConfig = Lens.field @"abortConfig"
{-# DEPRECATED cjAbortConfig "Use generic-lens or generic-optics with 'abortConfig' instead." #-}

-- | A short text description of the job.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjDescription :: Lens.Lens' CreateJob (Core.Maybe Types.Description)
cjDescription = Lens.field @"description"
{-# DEPRECATED cjDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The job document.
--
-- /Note:/ Consider using 'document' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjDocument :: Lens.Lens' CreateJob (Core.Maybe Types.Document)
cjDocument = Lens.field @"document"
{-# DEPRECATED cjDocument "Use generic-lens or generic-optics with 'document' instead." #-}

-- | An S3 link to the job document.
--
-- /Note:/ Consider using 'documentSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjDocumentSource :: Lens.Lens' CreateJob (Core.Maybe Types.DocumentSource)
cjDocumentSource = Lens.field @"documentSource"
{-# DEPRECATED cjDocumentSource "Use generic-lens or generic-optics with 'documentSource' instead." #-}

-- | Allows you to create a staged rollout of the job.
--
-- /Note:/ Consider using 'jobExecutionsRolloutConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjJobExecutionsRolloutConfig :: Lens.Lens' CreateJob (Core.Maybe Types.JobExecutionsRolloutConfig)
cjJobExecutionsRolloutConfig = Lens.field @"jobExecutionsRolloutConfig"
{-# DEPRECATED cjJobExecutionsRolloutConfig "Use generic-lens or generic-optics with 'jobExecutionsRolloutConfig' instead." #-}

-- | The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, AWS IoT Core sends jobs notifications to MQTT topics that contain the value in the following format.
-- @> aws/things//THING_NAME/ /jobs//JOB_ID/ /notify-namespace-/NAMESPACE_ID/ /@
--
-- /Note:/ Consider using 'namespaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjNamespaceId :: Lens.Lens' CreateJob (Core.Maybe Types.NamespaceId)
cjNamespaceId = Lens.field @"namespaceId"
{-# DEPRECATED cjNamespaceId "Use generic-lens or generic-optics with 'namespaceId' instead." #-}

-- | Configuration information for pre-signed S3 URLs.
--
-- /Note:/ Consider using 'presignedUrlConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjPresignedUrlConfig :: Lens.Lens' CreateJob (Core.Maybe Types.PresignedUrlConfig)
cjPresignedUrlConfig = Lens.field @"presignedUrlConfig"
{-# DEPRECATED cjPresignedUrlConfig "Use generic-lens or generic-optics with 'presignedUrlConfig' instead." #-}

-- | Metadata which can be used to manage the job.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjTags :: Lens.Lens' CreateJob (Core.Maybe [Types.Tag])
cjTags = Lens.field @"tags"
{-# DEPRECATED cjTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Specifies whether the job will continue to run (CONTINUOUS), or will be complete after all those things specified as targets have completed the job (SNAPSHOT). If continuous, the job may also be run on a thing when a change is detected in a target. For example, a job will run on a thing when the thing is added to a target group, even after the job was completed by all things originally in the group.
--
-- /Note:/ Consider using 'targetSelection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjTargetSelection :: Lens.Lens' CreateJob (Core.Maybe Types.TargetSelection)
cjTargetSelection = Lens.field @"targetSelection"
{-# DEPRECATED cjTargetSelection "Use generic-lens or generic-optics with 'targetSelection' instead." #-}

-- | Specifies the amount of time each device has to finish its execution of the job. The timer is started when the job execution status is set to @IN_PROGRESS@ . If the job execution status is not set to another terminal state before the time expires, it will be automatically set to @TIMED_OUT@ .
--
-- /Note:/ Consider using 'timeoutConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjTimeoutConfig :: Lens.Lens' CreateJob (Core.Maybe Types.TimeoutConfig)
cjTimeoutConfig = Lens.field @"timeoutConfig"
{-# DEPRECATED cjTimeoutConfig "Use generic-lens or generic-optics with 'timeoutConfig' instead." #-}

instance Core.FromJSON CreateJob where
  toJSON CreateJob {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("targets" Core..= targets),
            ("abortConfig" Core..=) Core.<$> abortConfig,
            ("description" Core..=) Core.<$> description,
            ("document" Core..=) Core.<$> document,
            ("documentSource" Core..=) Core.<$> documentSource,
            ("jobExecutionsRolloutConfig" Core..=)
              Core.<$> jobExecutionsRolloutConfig,
            ("namespaceId" Core..=) Core.<$> namespaceId,
            ("presignedUrlConfig" Core..=) Core.<$> presignedUrlConfig,
            ("tags" Core..=) Core.<$> tags,
            ("targetSelection" Core..=) Core.<$> targetSelection,
            ("timeoutConfig" Core..=) Core.<$> timeoutConfig
          ]
      )

instance Core.AWSRequest CreateJob where
  type Rs CreateJob = CreateJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath = Core.rawPath ("/jobs/" Core.<> (Core.toText jobId)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateJobResponse'
            Core.<$> (x Core..:? "description")
            Core.<*> (x Core..:? "jobArn")
            Core.<*> (x Core..:? "jobId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateJobResponse' smart constructor.
data CreateJobResponse = CreateJobResponse'
  { -- | The job description.
    description :: Core.Maybe Types.Description,
    -- | The job ARN.
    jobArn :: Core.Maybe Types.JobArn,
    -- | The unique identifier you assigned to this job.
    jobId :: Core.Maybe Types.JobId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateJobResponse' value with any optional fields omitted.
mkCreateJobResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateJobResponse
mkCreateJobResponse responseStatus =
  CreateJobResponse'
    { description = Core.Nothing,
      jobArn = Core.Nothing,
      jobId = Core.Nothing,
      responseStatus
    }

-- | The job description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrrsDescription :: Lens.Lens' CreateJobResponse (Core.Maybe Types.Description)
cjrrsDescription = Lens.field @"description"
{-# DEPRECATED cjrrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The job ARN.
--
-- /Note:/ Consider using 'jobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrrsJobArn :: Lens.Lens' CreateJobResponse (Core.Maybe Types.JobArn)
cjrrsJobArn = Lens.field @"jobArn"
{-# DEPRECATED cjrrsJobArn "Use generic-lens or generic-optics with 'jobArn' instead." #-}

-- | The unique identifier you assigned to this job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrrsJobId :: Lens.Lens' CreateJobResponse (Core.Maybe Types.JobId)
cjrrsJobId = Lens.field @"jobId"
{-# DEPRECATED cjrrsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrrsResponseStatus :: Lens.Lens' CreateJobResponse Core.Int
cjrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
