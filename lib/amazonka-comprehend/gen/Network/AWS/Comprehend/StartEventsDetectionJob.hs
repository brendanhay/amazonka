{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      StartEventsDetectionJob (..)
    , mkStartEventsDetectionJob
    -- ** Request lenses
    , sInputDataConfig
    , sOutputDataConfig
    , sDataAccessRoleArn
    , sLanguageCode
    , sTargetEventTypes
    , sClientRequestToken
    , sJobName

    -- * Destructuring the response
    , StartEventsDetectionJobResponse (..)
    , mkStartEventsDetectionJobResponse
    -- ** Response lenses
    , sedjrfrsJobId
    , sedjrfrsJobStatus
    , sedjrfrsResponseStatus
    ) where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartEventsDetectionJob' smart constructor.
data StartEventsDetectionJob = StartEventsDetectionJob'
  { inputDataConfig :: Types.InputDataConfig
    -- ^ Specifies the format and location of the input data for the job.
  , outputDataConfig :: Types.OutputDataConfig
    -- ^ Specifies where to send the output files.
  , dataAccessRoleArn :: Types.IamRoleArn
    -- ^ The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data.
  , languageCode :: Types.LanguageCode
    -- ^ The language code of the input documents.
  , targetEventTypes :: Core.NonEmpty Types.EventTypeString
    -- ^ The types of events to detect in the input documents.
  , clientRequestToken :: Core.Maybe Types.ClientRequestToken
    -- ^ An unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
  , jobName :: Core.Maybe Types.JobName
    -- ^ The identifier of the events detection job.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartEventsDetectionJob' value with any optional fields omitted.
mkStartEventsDetectionJob
    :: Types.InputDataConfig -- ^ 'inputDataConfig'
    -> Types.OutputDataConfig -- ^ 'outputDataConfig'
    -> Types.IamRoleArn -- ^ 'dataAccessRoleArn'
    -> Types.LanguageCode -- ^ 'languageCode'
    -> Core.NonEmpty Types.EventTypeString -- ^ 'targetEventTypes'
    -> StartEventsDetectionJob
mkStartEventsDetectionJob inputDataConfig outputDataConfig
  dataAccessRoleArn languageCode targetEventTypes
  = StartEventsDetectionJob'{inputDataConfig, outputDataConfig,
                             dataAccessRoleArn, languageCode, targetEventTypes,
                             clientRequestToken = Core.Nothing, jobName = Core.Nothing}

-- | Specifies the format and location of the input data for the job.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sInputDataConfig :: Lens.Lens' StartEventsDetectionJob Types.InputDataConfig
sInputDataConfig = Lens.field @"inputDataConfig"
{-# INLINEABLE sInputDataConfig #-}
{-# DEPRECATED inputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead"  #-}

-- | Specifies where to send the output files.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sOutputDataConfig :: Lens.Lens' StartEventsDetectionJob Types.OutputDataConfig
sOutputDataConfig = Lens.field @"outputDataConfig"
{-# INLINEABLE sOutputDataConfig #-}
{-# DEPRECATED outputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead"  #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data.
--
-- /Note:/ Consider using 'dataAccessRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDataAccessRoleArn :: Lens.Lens' StartEventsDetectionJob Types.IamRoleArn
sDataAccessRoleArn = Lens.field @"dataAccessRoleArn"
{-# INLINEABLE sDataAccessRoleArn #-}
{-# DEPRECATED dataAccessRoleArn "Use generic-lens or generic-optics with 'dataAccessRoleArn' instead"  #-}

-- | The language code of the input documents.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sLanguageCode :: Lens.Lens' StartEventsDetectionJob Types.LanguageCode
sLanguageCode = Lens.field @"languageCode"
{-# INLINEABLE sLanguageCode #-}
{-# DEPRECATED languageCode "Use generic-lens or generic-optics with 'languageCode' instead"  #-}

-- | The types of events to detect in the input documents.
--
-- /Note:/ Consider using 'targetEventTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTargetEventTypes :: Lens.Lens' StartEventsDetectionJob (Core.NonEmpty Types.EventTypeString)
sTargetEventTypes = Lens.field @"targetEventTypes"
{-# INLINEABLE sTargetEventTypes #-}
{-# DEPRECATED targetEventTypes "Use generic-lens or generic-optics with 'targetEventTypes' instead"  #-}

-- | An unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sClientRequestToken :: Lens.Lens' StartEventsDetectionJob (Core.Maybe Types.ClientRequestToken)
sClientRequestToken = Lens.field @"clientRequestToken"
{-# INLINEABLE sClientRequestToken #-}
{-# DEPRECATED clientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead"  #-}

-- | The identifier of the events detection job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sJobName :: Lens.Lens' StartEventsDetectionJob (Core.Maybe Types.JobName)
sJobName = Lens.field @"jobName"
{-# INLINEABLE sJobName #-}
{-# DEPRECATED jobName "Use generic-lens or generic-optics with 'jobName' instead"  #-}

instance Core.ToQuery StartEventsDetectionJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartEventsDetectionJob where
        toHeaders StartEventsDetectionJob{..}
          = Core.pure
              ("X-Amz-Target", "Comprehend_20171127.StartEventsDetectionJob")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StartEventsDetectionJob where
        toJSON StartEventsDetectionJob{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("InputDataConfig" Core..= inputDataConfig),
                  Core.Just ("OutputDataConfig" Core..= outputDataConfig),
                  Core.Just ("DataAccessRoleArn" Core..= dataAccessRoleArn),
                  Core.Just ("LanguageCode" Core..= languageCode),
                  Core.Just ("TargetEventTypes" Core..= targetEventTypes),
                  ("ClientRequestToken" Core..=) Core.<$> clientRequestToken,
                  ("JobName" Core..=) Core.<$> jobName])

instance Core.AWSRequest StartEventsDetectionJob where
        type Rs StartEventsDetectionJob = StartEventsDetectionJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StartEventsDetectionJobResponse' Core.<$>
                   (x Core..:? "JobId") Core.<*> x Core..:? "JobStatus" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartEventsDetectionJobResponse' smart constructor.
data StartEventsDetectionJobResponse = StartEventsDetectionJobResponse'
  { jobId :: Core.Maybe Types.JobId
    -- ^ An unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
  , jobStatus :: Core.Maybe Types.JobStatus
    -- ^ The status of the events detection job.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartEventsDetectionJobResponse' value with any optional fields omitted.
mkStartEventsDetectionJobResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartEventsDetectionJobResponse
mkStartEventsDetectionJobResponse responseStatus
  = StartEventsDetectionJobResponse'{jobId = Core.Nothing,
                                     jobStatus = Core.Nothing, responseStatus}

-- | An unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sedjrfrsJobId :: Lens.Lens' StartEventsDetectionJobResponse (Core.Maybe Types.JobId)
sedjrfrsJobId = Lens.field @"jobId"
{-# INLINEABLE sedjrfrsJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

-- | The status of the events detection job.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sedjrfrsJobStatus :: Lens.Lens' StartEventsDetectionJobResponse (Core.Maybe Types.JobStatus)
sedjrfrsJobStatus = Lens.field @"jobStatus"
{-# INLINEABLE sedjrfrsJobStatus #-}
{-# DEPRECATED jobStatus "Use generic-lens or generic-optics with 'jobStatus' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sedjrfrsResponseStatus :: Lens.Lens' StartEventsDetectionJobResponse Core.Int
sedjrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE sedjrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
