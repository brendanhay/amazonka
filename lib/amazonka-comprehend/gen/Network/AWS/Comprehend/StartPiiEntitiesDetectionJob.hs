{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.StartPiiEntitiesDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an asynchronous PII entity detection job for a collection of documents.
module Network.AWS.Comprehend.StartPiiEntitiesDetectionJob
  ( -- * Creating a request
    StartPiiEntitiesDetectionJob (..),
    mkStartPiiEntitiesDetectionJob,

    -- ** Request lenses
    spedjInputDataConfig,
    spedjOutputDataConfig,
    spedjMode,
    spedjDataAccessRoleArn,
    spedjLanguageCode,
    spedjClientRequestToken,
    spedjJobName,
    spedjRedactionConfig,

    -- * Destructuring the response
    StartPiiEntitiesDetectionJobResponse (..),
    mkStartPiiEntitiesDetectionJobResponse,

    -- ** Response lenses
    spedjrrsJobId,
    spedjrrsJobStatus,
    spedjrrsResponseStatus,
  )
where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartPiiEntitiesDetectionJob' smart constructor.
data StartPiiEntitiesDetectionJob = StartPiiEntitiesDetectionJob'
  { -- | The input properties for a PII entities detection job.
    inputDataConfig :: Types.InputDataConfig,
    -- | Provides conﬁguration parameters for the output of PII entity detection jobs.
    outputDataConfig :: Types.OutputDataConfig,
    -- | Specifies whether the output provides the locations (offsets) of PII entities or a file in which PII entities are redacted.
    mode :: Types.PiiEntitiesDetectionMode,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data.
    dataAccessRoleArn :: Types.IamRoleArn,
    -- | The language of the input documents.
    languageCode :: Types.LanguageCode,
    -- | A unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
    clientRequestToken :: Core.Maybe Types.ClientRequestTokenString,
    -- | The identifier of the job.
    jobName :: Core.Maybe Types.JobName,
    -- | Provides configuration parameters for PII entity redaction.
    --
    -- This parameter is required if you set the @Mode@ parameter to @ONLY_REDACTION@ . In that case, you must provide a @RedactionConfig@ definition that includes the @PiiEntityTypes@ parameter.
    redactionConfig :: Core.Maybe Types.RedactionConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartPiiEntitiesDetectionJob' value with any optional fields omitted.
mkStartPiiEntitiesDetectionJob ::
  -- | 'inputDataConfig'
  Types.InputDataConfig ->
  -- | 'outputDataConfig'
  Types.OutputDataConfig ->
  -- | 'mode'
  Types.PiiEntitiesDetectionMode ->
  -- | 'dataAccessRoleArn'
  Types.IamRoleArn ->
  -- | 'languageCode'
  Types.LanguageCode ->
  StartPiiEntitiesDetectionJob
mkStartPiiEntitiesDetectionJob
  inputDataConfig
  outputDataConfig
  mode
  dataAccessRoleArn
  languageCode =
    StartPiiEntitiesDetectionJob'
      { inputDataConfig,
        outputDataConfig,
        mode,
        dataAccessRoleArn,
        languageCode,
        clientRequestToken = Core.Nothing,
        jobName = Core.Nothing,
        redactionConfig = Core.Nothing
      }

-- | The input properties for a PII entities detection job.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spedjInputDataConfig :: Lens.Lens' StartPiiEntitiesDetectionJob Types.InputDataConfig
spedjInputDataConfig = Lens.field @"inputDataConfig"
{-# DEPRECATED spedjInputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead." #-}

-- | Provides conﬁguration parameters for the output of PII entity detection jobs.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spedjOutputDataConfig :: Lens.Lens' StartPiiEntitiesDetectionJob Types.OutputDataConfig
spedjOutputDataConfig = Lens.field @"outputDataConfig"
{-# DEPRECATED spedjOutputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead." #-}

-- | Specifies whether the output provides the locations (offsets) of PII entities or a file in which PII entities are redacted.
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spedjMode :: Lens.Lens' StartPiiEntitiesDetectionJob Types.PiiEntitiesDetectionMode
spedjMode = Lens.field @"mode"
{-# DEPRECATED spedjMode "Use generic-lens or generic-optics with 'mode' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data.
--
-- /Note:/ Consider using 'dataAccessRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spedjDataAccessRoleArn :: Lens.Lens' StartPiiEntitiesDetectionJob Types.IamRoleArn
spedjDataAccessRoleArn = Lens.field @"dataAccessRoleArn"
{-# DEPRECATED spedjDataAccessRoleArn "Use generic-lens or generic-optics with 'dataAccessRoleArn' instead." #-}

-- | The language of the input documents.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spedjLanguageCode :: Lens.Lens' StartPiiEntitiesDetectionJob Types.LanguageCode
spedjLanguageCode = Lens.field @"languageCode"
{-# DEPRECATED spedjLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | A unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spedjClientRequestToken :: Lens.Lens' StartPiiEntitiesDetectionJob (Core.Maybe Types.ClientRequestTokenString)
spedjClientRequestToken = Lens.field @"clientRequestToken"
{-# DEPRECATED spedjClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | The identifier of the job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spedjJobName :: Lens.Lens' StartPiiEntitiesDetectionJob (Core.Maybe Types.JobName)
spedjJobName = Lens.field @"jobName"
{-# DEPRECATED spedjJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | Provides configuration parameters for PII entity redaction.
--
-- This parameter is required if you set the @Mode@ parameter to @ONLY_REDACTION@ . In that case, you must provide a @RedactionConfig@ definition that includes the @PiiEntityTypes@ parameter.
--
-- /Note:/ Consider using 'redactionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spedjRedactionConfig :: Lens.Lens' StartPiiEntitiesDetectionJob (Core.Maybe Types.RedactionConfig)
spedjRedactionConfig = Lens.field @"redactionConfig"
{-# DEPRECATED spedjRedactionConfig "Use generic-lens or generic-optics with 'redactionConfig' instead." #-}

instance Core.FromJSON StartPiiEntitiesDetectionJob where
  toJSON StartPiiEntitiesDetectionJob {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("InputDataConfig" Core..= inputDataConfig),
            Core.Just ("OutputDataConfig" Core..= outputDataConfig),
            Core.Just ("Mode" Core..= mode),
            Core.Just ("DataAccessRoleArn" Core..= dataAccessRoleArn),
            Core.Just ("LanguageCode" Core..= languageCode),
            ("ClientRequestToken" Core..=) Core.<$> clientRequestToken,
            ("JobName" Core..=) Core.<$> jobName,
            ("RedactionConfig" Core..=) Core.<$> redactionConfig
          ]
      )

instance Core.AWSRequest StartPiiEntitiesDetectionJob where
  type
    Rs StartPiiEntitiesDetectionJob =
      StartPiiEntitiesDetectionJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "Comprehend_20171127.StartPiiEntitiesDetectionJob"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartPiiEntitiesDetectionJobResponse'
            Core.<$> (x Core..:? "JobId")
            Core.<*> (x Core..:? "JobStatus")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartPiiEntitiesDetectionJobResponse' smart constructor.
data StartPiiEntitiesDetectionJobResponse = StartPiiEntitiesDetectionJobResponse'
  { -- | The identifier generated for the job.
    jobId :: Core.Maybe Types.JobId,
    -- | The status of the job.
    jobStatus :: Core.Maybe Types.JobStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartPiiEntitiesDetectionJobResponse' value with any optional fields omitted.
mkStartPiiEntitiesDetectionJobResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartPiiEntitiesDetectionJobResponse
mkStartPiiEntitiesDetectionJobResponse responseStatus =
  StartPiiEntitiesDetectionJobResponse'
    { jobId = Core.Nothing,
      jobStatus = Core.Nothing,
      responseStatus
    }

-- | The identifier generated for the job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spedjrrsJobId :: Lens.Lens' StartPiiEntitiesDetectionJobResponse (Core.Maybe Types.JobId)
spedjrrsJobId = Lens.field @"jobId"
{-# DEPRECATED spedjrrsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The status of the job.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spedjrrsJobStatus :: Lens.Lens' StartPiiEntitiesDetectionJobResponse (Core.Maybe Types.JobStatus)
spedjrrsJobStatus = Lens.field @"jobStatus"
{-# DEPRECATED spedjrrsJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spedjrrsResponseStatus :: Lens.Lens' StartPiiEntitiesDetectionJobResponse Core.Int
spedjrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED spedjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
