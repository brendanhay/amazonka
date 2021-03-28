{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.StartEntitiesDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an asynchronous entity detection job for a collection of documents. Use the operation to track the status of a job.
--
-- This API can be used for either standard entity detection or custom entity recognition. In order to be used for custom entity recognition, the optional @EntityRecognizerArn@ must be used in order to provide access to the recognizer being used to detect the custom entity.
module Network.AWS.Comprehend.StartEntitiesDetectionJob
    (
    -- * Creating a request
      StartEntitiesDetectionJob (..)
    , mkStartEntitiesDetectionJob
    -- ** Request lenses
    , sedjInputDataConfig
    , sedjOutputDataConfig
    , sedjDataAccessRoleArn
    , sedjLanguageCode
    , sedjClientRequestToken
    , sedjEntityRecognizerArn
    , sedjJobName
    , sedjVolumeKmsKeyId
    , sedjVpcConfig

    -- * Destructuring the response
    , StartEntitiesDetectionJobResponse (..)
    , mkStartEntitiesDetectionJobResponse
    -- ** Response lenses
    , sedjrgrsJobId
    , sedjrgrsJobStatus
    , sedjrgrsResponseStatus
    ) where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartEntitiesDetectionJob' smart constructor.
data StartEntitiesDetectionJob = StartEntitiesDetectionJob'
  { inputDataConfig :: Types.InputDataConfig
    -- ^ Specifies the format and location of the input data for the job.
  , outputDataConfig :: Types.OutputDataConfig
    -- ^ Specifies where to send the output files.
  , dataAccessRoleArn :: Types.DataAccessRoleArn
    -- ^ The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data. For more information, see <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions> .
  , languageCode :: Types.LanguageCode
    -- ^ The language of the input documents. All documents must be in the same language. You can specify any of the languages supported by Amazon Comprehend. If custom entities recognition is used, this parameter is ignored and the language used for training the model is used instead.
  , clientRequestToken :: Core.Maybe Types.ClientRequestToken
    -- ^ A unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
  , entityRecognizerArn :: Core.Maybe Types.EntityRecognizerArn
    -- ^ The Amazon Resource Name (ARN) that identifies the specific entity recognizer to be used by the @StartEntitiesDetectionJob@ . This ARN is optional and is only used for a custom entity recognition job.
  , jobName :: Core.Maybe Types.JobName
    -- ^ The identifier of the job.
  , volumeKmsKeyId :: Core.Maybe Types.VolumeKmsKeyId
    -- ^ ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:
--
--
--     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@ 
--
--
--     * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@ 
--
--
  , vpcConfig :: Core.Maybe Types.VpcConfig
    -- ^ Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for your entity detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartEntitiesDetectionJob' value with any optional fields omitted.
mkStartEntitiesDetectionJob
    :: Types.InputDataConfig -- ^ 'inputDataConfig'
    -> Types.OutputDataConfig -- ^ 'outputDataConfig'
    -> Types.DataAccessRoleArn -- ^ 'dataAccessRoleArn'
    -> Types.LanguageCode -- ^ 'languageCode'
    -> StartEntitiesDetectionJob
mkStartEntitiesDetectionJob inputDataConfig outputDataConfig
  dataAccessRoleArn languageCode
  = StartEntitiesDetectionJob'{inputDataConfig, outputDataConfig,
                               dataAccessRoleArn, languageCode, clientRequestToken = Core.Nothing,
                               entityRecognizerArn = Core.Nothing, jobName = Core.Nothing,
                               volumeKmsKeyId = Core.Nothing, vpcConfig = Core.Nothing}

-- | Specifies the format and location of the input data for the job.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sedjInputDataConfig :: Lens.Lens' StartEntitiesDetectionJob Types.InputDataConfig
sedjInputDataConfig = Lens.field @"inputDataConfig"
{-# INLINEABLE sedjInputDataConfig #-}
{-# DEPRECATED inputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead"  #-}

-- | Specifies where to send the output files.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sedjOutputDataConfig :: Lens.Lens' StartEntitiesDetectionJob Types.OutputDataConfig
sedjOutputDataConfig = Lens.field @"outputDataConfig"
{-# INLINEABLE sedjOutputDataConfig #-}
{-# DEPRECATED outputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead"  #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data. For more information, see <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions> .
--
-- /Note:/ Consider using 'dataAccessRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sedjDataAccessRoleArn :: Lens.Lens' StartEntitiesDetectionJob Types.DataAccessRoleArn
sedjDataAccessRoleArn = Lens.field @"dataAccessRoleArn"
{-# INLINEABLE sedjDataAccessRoleArn #-}
{-# DEPRECATED dataAccessRoleArn "Use generic-lens or generic-optics with 'dataAccessRoleArn' instead"  #-}

-- | The language of the input documents. All documents must be in the same language. You can specify any of the languages supported by Amazon Comprehend. If custom entities recognition is used, this parameter is ignored and the language used for training the model is used instead.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sedjLanguageCode :: Lens.Lens' StartEntitiesDetectionJob Types.LanguageCode
sedjLanguageCode = Lens.field @"languageCode"
{-# INLINEABLE sedjLanguageCode #-}
{-# DEPRECATED languageCode "Use generic-lens or generic-optics with 'languageCode' instead"  #-}

-- | A unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sedjClientRequestToken :: Lens.Lens' StartEntitiesDetectionJob (Core.Maybe Types.ClientRequestToken)
sedjClientRequestToken = Lens.field @"clientRequestToken"
{-# INLINEABLE sedjClientRequestToken #-}
{-# DEPRECATED clientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead"  #-}

-- | The Amazon Resource Name (ARN) that identifies the specific entity recognizer to be used by the @StartEntitiesDetectionJob@ . This ARN is optional and is only used for a custom entity recognition job.
--
-- /Note:/ Consider using 'entityRecognizerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sedjEntityRecognizerArn :: Lens.Lens' StartEntitiesDetectionJob (Core.Maybe Types.EntityRecognizerArn)
sedjEntityRecognizerArn = Lens.field @"entityRecognizerArn"
{-# INLINEABLE sedjEntityRecognizerArn #-}
{-# DEPRECATED entityRecognizerArn "Use generic-lens or generic-optics with 'entityRecognizerArn' instead"  #-}

-- | The identifier of the job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sedjJobName :: Lens.Lens' StartEntitiesDetectionJob (Core.Maybe Types.JobName)
sedjJobName = Lens.field @"jobName"
{-# INLINEABLE sedjJobName #-}
{-# DEPRECATED jobName "Use generic-lens or generic-optics with 'jobName' instead"  #-}

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:
--
--
--     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@ 
--
--
--     * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@ 
--
--
--
-- /Note:/ Consider using 'volumeKmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sedjVolumeKmsKeyId :: Lens.Lens' StartEntitiesDetectionJob (Core.Maybe Types.VolumeKmsKeyId)
sedjVolumeKmsKeyId = Lens.field @"volumeKmsKeyId"
{-# INLINEABLE sedjVolumeKmsKeyId #-}
{-# DEPRECATED volumeKmsKeyId "Use generic-lens or generic-optics with 'volumeKmsKeyId' instead"  #-}

-- | Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for your entity detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> . 
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sedjVpcConfig :: Lens.Lens' StartEntitiesDetectionJob (Core.Maybe Types.VpcConfig)
sedjVpcConfig = Lens.field @"vpcConfig"
{-# INLINEABLE sedjVpcConfig #-}
{-# DEPRECATED vpcConfig "Use generic-lens or generic-optics with 'vpcConfig' instead"  #-}

instance Core.ToQuery StartEntitiesDetectionJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartEntitiesDetectionJob where
        toHeaders StartEntitiesDetectionJob{..}
          = Core.pure
              ("X-Amz-Target", "Comprehend_20171127.StartEntitiesDetectionJob")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StartEntitiesDetectionJob where
        toJSON StartEntitiesDetectionJob{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("InputDataConfig" Core..= inputDataConfig),
                  Core.Just ("OutputDataConfig" Core..= outputDataConfig),
                  Core.Just ("DataAccessRoleArn" Core..= dataAccessRoleArn),
                  Core.Just ("LanguageCode" Core..= languageCode),
                  ("ClientRequestToken" Core..=) Core.<$> clientRequestToken,
                  ("EntityRecognizerArn" Core..=) Core.<$> entityRecognizerArn,
                  ("JobName" Core..=) Core.<$> jobName,
                  ("VolumeKmsKeyId" Core..=) Core.<$> volumeKmsKeyId,
                  ("VpcConfig" Core..=) Core.<$> vpcConfig])

instance Core.AWSRequest StartEntitiesDetectionJob where
        type Rs StartEntitiesDetectionJob =
             StartEntitiesDetectionJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StartEntitiesDetectionJobResponse' Core.<$>
                   (x Core..:? "JobId") Core.<*> x Core..:? "JobStatus" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartEntitiesDetectionJobResponse' smart constructor.
data StartEntitiesDetectionJobResponse = StartEntitiesDetectionJobResponse'
  { jobId :: Core.Maybe Types.JobId
    -- ^ The identifier generated for the job. To get the status of job, use this identifier with the operation.
  , jobStatus :: Core.Maybe Types.JobStatus
    -- ^ The status of the job. 
--
--
--     * SUBMITTED - The job has been received and is queued for processing.
--
--
--     * IN_PROGRESS - Amazon Comprehend is processing the job.
--
--
--     * COMPLETED - The job was successfully completed and the output is available.
--
--
--     * FAILED - The job did not complete. To get details, use the operation.
--
--
--     * STOP_REQUESTED - Amazon Comprehend has received a stop request for the job and is processing the request.
--
--
--     * STOPPED - The job was successfully stopped without completing.
--
--
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartEntitiesDetectionJobResponse' value with any optional fields omitted.
mkStartEntitiesDetectionJobResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartEntitiesDetectionJobResponse
mkStartEntitiesDetectionJobResponse responseStatus
  = StartEntitiesDetectionJobResponse'{jobId = Core.Nothing,
                                       jobStatus = Core.Nothing, responseStatus}

-- | The identifier generated for the job. To get the status of job, use this identifier with the operation.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sedjrgrsJobId :: Lens.Lens' StartEntitiesDetectionJobResponse (Core.Maybe Types.JobId)
sedjrgrsJobId = Lens.field @"jobId"
{-# INLINEABLE sedjrgrsJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

-- | The status of the job. 
--
--
--     * SUBMITTED - The job has been received and is queued for processing.
--
--
--     * IN_PROGRESS - Amazon Comprehend is processing the job.
--
--
--     * COMPLETED - The job was successfully completed and the output is available.
--
--
--     * FAILED - The job did not complete. To get details, use the operation.
--
--
--     * STOP_REQUESTED - Amazon Comprehend has received a stop request for the job and is processing the request.
--
--
--     * STOPPED - The job was successfully stopped without completing.
--
--
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sedjrgrsJobStatus :: Lens.Lens' StartEntitiesDetectionJobResponse (Core.Maybe Types.JobStatus)
sedjrgrsJobStatus = Lens.field @"jobStatus"
{-# INLINEABLE sedjrgrsJobStatus #-}
{-# DEPRECATED jobStatus "Use generic-lens or generic-optics with 'jobStatus' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sedjrgrsResponseStatus :: Lens.Lens' StartEntitiesDetectionJobResponse Core.Int
sedjrgrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE sedjrgrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
