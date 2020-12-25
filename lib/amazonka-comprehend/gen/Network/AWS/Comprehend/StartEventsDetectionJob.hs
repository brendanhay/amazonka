{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.StartEventsDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an asynchronous event detection job for a collection of documents.
module Network.AWS.Comprehend.StartEventsDetectionJob
  ( -- * Creating a request
    StartEventsDetectionJob (..),
    mkStartEventsDetectionJob,

    -- ** Request lenses
    sInputDataConfig,
    sOutputDataConfig,
    sDataAccessRoleArn,
    sLanguageCode,
    sTargetEventTypes,
    sClientRequestToken,
    sJobName,

    -- * Destructuring the response
    StartEventsDetectionJobResponse (..),
    mkStartEventsDetectionJobResponse,

    -- ** Response lenses
    sedjrfrsJobId,
    sedjrfrsJobStatus,
    sedjrfrsResponseStatus,
  )
where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartEventsDetectionJob' smart constructor.
data StartEventsDetectionJob = StartEventsDetectionJob'
  { -- | Specifies the format and location of the input data for the job.
    inputDataConfig :: Types.InputDataConfig,
    -- | Specifies where to send the output files.
    outputDataConfig :: Types.OutputDataConfig,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data.
    dataAccessRoleArn :: Types.IamRoleArn,
    -- | The language code of the input documents.
    languageCode :: Types.LanguageCode,
    -- | The types of events to detect in the input documents.
    targetEventTypes :: Core.NonEmpty Types.EventTypeString,
    -- | An unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
    clientRequestToken :: Core.Maybe Types.ClientRequestToken,
    -- | The identifier of the events detection job.
    jobName :: Core.Maybe Types.JobName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartEventsDetectionJob' value with any optional fields omitted.
mkStartEventsDetectionJob ::
  -- | 'inputDataConfig'
  Types.InputDataConfig ->
  -- | 'outputDataConfig'
  Types.OutputDataConfig ->
  -- | 'dataAccessRoleArn'
  Types.IamRoleArn ->
  -- | 'languageCode'
  Types.LanguageCode ->
  -- | 'targetEventTypes'
  Core.NonEmpty Types.EventTypeString ->
  StartEventsDetectionJob
mkStartEventsDetectionJob
  inputDataConfig
  outputDataConfig
  dataAccessRoleArn
  languageCode
  targetEventTypes =
    StartEventsDetectionJob'
      { inputDataConfig,
        outputDataConfig,
        dataAccessRoleArn,
        languageCode,
        targetEventTypes,
        clientRequestToken = Core.Nothing,
        jobName = Core.Nothing
      }

-- | Specifies the format and location of the input data for the job.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sInputDataConfig :: Lens.Lens' StartEventsDetectionJob Types.InputDataConfig
sInputDataConfig = Lens.field @"inputDataConfig"
{-# DEPRECATED sInputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead." #-}

-- | Specifies where to send the output files.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sOutputDataConfig :: Lens.Lens' StartEventsDetectionJob Types.OutputDataConfig
sOutputDataConfig = Lens.field @"outputDataConfig"
{-# DEPRECATED sOutputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data.
--
-- /Note:/ Consider using 'dataAccessRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDataAccessRoleArn :: Lens.Lens' StartEventsDetectionJob Types.IamRoleArn
sDataAccessRoleArn = Lens.field @"dataAccessRoleArn"
{-# DEPRECATED sDataAccessRoleArn "Use generic-lens or generic-optics with 'dataAccessRoleArn' instead." #-}

-- | The language code of the input documents.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sLanguageCode :: Lens.Lens' StartEventsDetectionJob Types.LanguageCode
sLanguageCode = Lens.field @"languageCode"
{-# DEPRECATED sLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | The types of events to detect in the input documents.
--
-- /Note:/ Consider using 'targetEventTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTargetEventTypes :: Lens.Lens' StartEventsDetectionJob (Core.NonEmpty Types.EventTypeString)
sTargetEventTypes = Lens.field @"targetEventTypes"
{-# DEPRECATED sTargetEventTypes "Use generic-lens or generic-optics with 'targetEventTypes' instead." #-}

-- | An unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sClientRequestToken :: Lens.Lens' StartEventsDetectionJob (Core.Maybe Types.ClientRequestToken)
sClientRequestToken = Lens.field @"clientRequestToken"
{-# DEPRECATED sClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | The identifier of the events detection job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sJobName :: Lens.Lens' StartEventsDetectionJob (Core.Maybe Types.JobName)
sJobName = Lens.field @"jobName"
{-# DEPRECATED sJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

instance Core.FromJSON StartEventsDetectionJob where
  toJSON StartEventsDetectionJob {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("InputDataConfig" Core..= inputDataConfig),
            Core.Just ("OutputDataConfig" Core..= outputDataConfig),
            Core.Just ("DataAccessRoleArn" Core..= dataAccessRoleArn),
            Core.Just ("LanguageCode" Core..= languageCode),
            Core.Just ("TargetEventTypes" Core..= targetEventTypes),
            ("ClientRequestToken" Core..=) Core.<$> clientRequestToken,
            ("JobName" Core..=) Core.<$> jobName
          ]
      )

instance Core.AWSRequest StartEventsDetectionJob where
  type Rs StartEventsDetectionJob = StartEventsDetectionJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Comprehend_20171127.StartEventsDetectionJob")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartEventsDetectionJobResponse'
            Core.<$> (x Core..:? "JobId")
            Core.<*> (x Core..:? "JobStatus")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartEventsDetectionJobResponse' smart constructor.
data StartEventsDetectionJobResponse = StartEventsDetectionJobResponse'
  { -- | An unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
    jobId :: Core.Maybe Types.JobId,
    -- | The status of the events detection job.
    jobStatus :: Core.Maybe Types.JobStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartEventsDetectionJobResponse' value with any optional fields omitted.
mkStartEventsDetectionJobResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartEventsDetectionJobResponse
mkStartEventsDetectionJobResponse responseStatus =
  StartEventsDetectionJobResponse'
    { jobId = Core.Nothing,
      jobStatus = Core.Nothing,
      responseStatus
    }

-- | An unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sedjrfrsJobId :: Lens.Lens' StartEventsDetectionJobResponse (Core.Maybe Types.JobId)
sedjrfrsJobId = Lens.field @"jobId"
{-# DEPRECATED sedjrfrsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The status of the events detection job.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sedjrfrsJobStatus :: Lens.Lens' StartEventsDetectionJobResponse (Core.Maybe Types.JobStatus)
sedjrfrsJobStatus = Lens.field @"jobStatus"
{-# DEPRECATED sedjrfrsJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sedjrfrsResponseStatus :: Lens.Lens' StartEventsDetectionJobResponse Core.Int
sedjrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sedjrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
