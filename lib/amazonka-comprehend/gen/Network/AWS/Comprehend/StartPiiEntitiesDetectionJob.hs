{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      StartPiiEntitiesDetectionJob (..)
    , mkStartPiiEntitiesDetectionJob
    -- ** Request lenses
    , spedjInputDataConfig
    , spedjOutputDataConfig
    , spedjMode
    , spedjDataAccessRoleArn
    , spedjLanguageCode
    , spedjClientRequestToken
    , spedjJobName
    , spedjRedactionConfig

    -- * Destructuring the response
    , StartPiiEntitiesDetectionJobResponse (..)
    , mkStartPiiEntitiesDetectionJobResponse
    -- ** Response lenses
    , spedjrrsJobId
    , spedjrrsJobStatus
    , spedjrrsResponseStatus
    ) where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartPiiEntitiesDetectionJob' smart constructor.
data StartPiiEntitiesDetectionJob = StartPiiEntitiesDetectionJob'
  { inputDataConfig :: Types.InputDataConfig
    -- ^ The input properties for a PII entities detection job.
  , outputDataConfig :: Types.OutputDataConfig
    -- ^ Provides conﬁguration parameters for the output of PII entity detection jobs.
  , mode :: Types.PiiEntitiesDetectionMode
    -- ^ Specifies whether the output provides the locations (offsets) of PII entities or a file in which PII entities are redacted.
  , dataAccessRoleArn :: Types.IamRoleArn
    -- ^ The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data.
  , languageCode :: Types.LanguageCode
    -- ^ The language of the input documents.
  , clientRequestToken :: Core.Maybe Types.ClientRequestTokenString
    -- ^ A unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
  , jobName :: Core.Maybe Types.JobName
    -- ^ The identifier of the job.
  , redactionConfig :: Core.Maybe Types.RedactionConfig
    -- ^ Provides configuration parameters for PII entity redaction.
--
-- This parameter is required if you set the @Mode@ parameter to @ONLY_REDACTION@ . In that case, you must provide a @RedactionConfig@ definition that includes the @PiiEntityTypes@ parameter.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartPiiEntitiesDetectionJob' value with any optional fields omitted.
mkStartPiiEntitiesDetectionJob
    :: Types.InputDataConfig -- ^ 'inputDataConfig'
    -> Types.OutputDataConfig -- ^ 'outputDataConfig'
    -> Types.PiiEntitiesDetectionMode -- ^ 'mode'
    -> Types.IamRoleArn -- ^ 'dataAccessRoleArn'
    -> Types.LanguageCode -- ^ 'languageCode'
    -> StartPiiEntitiesDetectionJob
mkStartPiiEntitiesDetectionJob inputDataConfig outputDataConfig
  mode dataAccessRoleArn languageCode
  = StartPiiEntitiesDetectionJob'{inputDataConfig, outputDataConfig,
                                  mode, dataAccessRoleArn, languageCode,
                                  clientRequestToken = Core.Nothing, jobName = Core.Nothing,
                                  redactionConfig = Core.Nothing}

-- | The input properties for a PII entities detection job.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spedjInputDataConfig :: Lens.Lens' StartPiiEntitiesDetectionJob Types.InputDataConfig
spedjInputDataConfig = Lens.field @"inputDataConfig"
{-# INLINEABLE spedjInputDataConfig #-}
{-# DEPRECATED inputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead"  #-}

-- | Provides conﬁguration parameters for the output of PII entity detection jobs.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spedjOutputDataConfig :: Lens.Lens' StartPiiEntitiesDetectionJob Types.OutputDataConfig
spedjOutputDataConfig = Lens.field @"outputDataConfig"
{-# INLINEABLE spedjOutputDataConfig #-}
{-# DEPRECATED outputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead"  #-}

-- | Specifies whether the output provides the locations (offsets) of PII entities or a file in which PII entities are redacted.
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spedjMode :: Lens.Lens' StartPiiEntitiesDetectionJob Types.PiiEntitiesDetectionMode
spedjMode = Lens.field @"mode"
{-# INLINEABLE spedjMode #-}
{-# DEPRECATED mode "Use generic-lens or generic-optics with 'mode' instead"  #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data.
--
-- /Note:/ Consider using 'dataAccessRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spedjDataAccessRoleArn :: Lens.Lens' StartPiiEntitiesDetectionJob Types.IamRoleArn
spedjDataAccessRoleArn = Lens.field @"dataAccessRoleArn"
{-# INLINEABLE spedjDataAccessRoleArn #-}
{-# DEPRECATED dataAccessRoleArn "Use generic-lens or generic-optics with 'dataAccessRoleArn' instead"  #-}

-- | The language of the input documents.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spedjLanguageCode :: Lens.Lens' StartPiiEntitiesDetectionJob Types.LanguageCode
spedjLanguageCode = Lens.field @"languageCode"
{-# INLINEABLE spedjLanguageCode #-}
{-# DEPRECATED languageCode "Use generic-lens or generic-optics with 'languageCode' instead"  #-}

-- | A unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spedjClientRequestToken :: Lens.Lens' StartPiiEntitiesDetectionJob (Core.Maybe Types.ClientRequestTokenString)
spedjClientRequestToken = Lens.field @"clientRequestToken"
{-# INLINEABLE spedjClientRequestToken #-}
{-# DEPRECATED clientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead"  #-}

-- | The identifier of the job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spedjJobName :: Lens.Lens' StartPiiEntitiesDetectionJob (Core.Maybe Types.JobName)
spedjJobName = Lens.field @"jobName"
{-# INLINEABLE spedjJobName #-}
{-# DEPRECATED jobName "Use generic-lens or generic-optics with 'jobName' instead"  #-}

-- | Provides configuration parameters for PII entity redaction.
--
-- This parameter is required if you set the @Mode@ parameter to @ONLY_REDACTION@ . In that case, you must provide a @RedactionConfig@ definition that includes the @PiiEntityTypes@ parameter.
--
-- /Note:/ Consider using 'redactionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spedjRedactionConfig :: Lens.Lens' StartPiiEntitiesDetectionJob (Core.Maybe Types.RedactionConfig)
spedjRedactionConfig = Lens.field @"redactionConfig"
{-# INLINEABLE spedjRedactionConfig #-}
{-# DEPRECATED redactionConfig "Use generic-lens or generic-optics with 'redactionConfig' instead"  #-}

instance Core.ToQuery StartPiiEntitiesDetectionJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartPiiEntitiesDetectionJob where
        toHeaders StartPiiEntitiesDetectionJob{..}
          = Core.pure
              ("X-Amz-Target",
               "Comprehend_20171127.StartPiiEntitiesDetectionJob")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StartPiiEntitiesDetectionJob where
        toJSON StartPiiEntitiesDetectionJob{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("InputDataConfig" Core..= inputDataConfig),
                  Core.Just ("OutputDataConfig" Core..= outputDataConfig),
                  Core.Just ("Mode" Core..= mode),
                  Core.Just ("DataAccessRoleArn" Core..= dataAccessRoleArn),
                  Core.Just ("LanguageCode" Core..= languageCode),
                  ("ClientRequestToken" Core..=) Core.<$> clientRequestToken,
                  ("JobName" Core..=) Core.<$> jobName,
                  ("RedactionConfig" Core..=) Core.<$> redactionConfig])

instance Core.AWSRequest StartPiiEntitiesDetectionJob where
        type Rs StartPiiEntitiesDetectionJob =
             StartPiiEntitiesDetectionJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StartPiiEntitiesDetectionJobResponse' Core.<$>
                   (x Core..:? "JobId") Core.<*> x Core..:? "JobStatus" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartPiiEntitiesDetectionJobResponse' smart constructor.
data StartPiiEntitiesDetectionJobResponse = StartPiiEntitiesDetectionJobResponse'
  { jobId :: Core.Maybe Types.JobId
    -- ^ The identifier generated for the job.
  , jobStatus :: Core.Maybe Types.JobStatus
    -- ^ The status of the job.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartPiiEntitiesDetectionJobResponse' value with any optional fields omitted.
mkStartPiiEntitiesDetectionJobResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartPiiEntitiesDetectionJobResponse
mkStartPiiEntitiesDetectionJobResponse responseStatus
  = StartPiiEntitiesDetectionJobResponse'{jobId = Core.Nothing,
                                          jobStatus = Core.Nothing, responseStatus}

-- | The identifier generated for the job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spedjrrsJobId :: Lens.Lens' StartPiiEntitiesDetectionJobResponse (Core.Maybe Types.JobId)
spedjrrsJobId = Lens.field @"jobId"
{-# INLINEABLE spedjrrsJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

-- | The status of the job.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spedjrrsJobStatus :: Lens.Lens' StartPiiEntitiesDetectionJobResponse (Core.Maybe Types.JobStatus)
spedjrrsJobStatus = Lens.field @"jobStatus"
{-# INLINEABLE spedjrrsJobStatus #-}
{-# DEPRECATED jobStatus "Use generic-lens or generic-optics with 'jobStatus' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spedjrrsResponseStatus :: Lens.Lens' StartPiiEntitiesDetectionJobResponse Core.Int
spedjrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE spedjrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
