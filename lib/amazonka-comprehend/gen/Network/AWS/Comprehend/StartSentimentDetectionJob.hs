{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.StartSentimentDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an asynchronous sentiment detection job for a collection of documents. use the operation to track the status of a job.
module Network.AWS.Comprehend.StartSentimentDetectionJob
    (
    -- * Creating a request
      StartSentimentDetectionJob (..)
    , mkStartSentimentDetectionJob
    -- ** Request lenses
    , ssdjInputDataConfig
    , ssdjOutputDataConfig
    , ssdjDataAccessRoleArn
    , ssdjLanguageCode
    , ssdjClientRequestToken
    , ssdjJobName
    , ssdjVolumeKmsKeyId
    , ssdjVpcConfig

    -- * Destructuring the response
    , StartSentimentDetectionJobResponse (..)
    , mkStartSentimentDetectionJobResponse
    -- ** Response lenses
    , ssdjrrsJobId
    , ssdjrrsJobStatus
    , ssdjrrsResponseStatus
    ) where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartSentimentDetectionJob' smart constructor.
data StartSentimentDetectionJob = StartSentimentDetectionJob'
  { inputDataConfig :: Types.InputDataConfig
    -- ^ Specifies the format and location of the input data for the job.
  , outputDataConfig :: Types.OutputDataConfig
    -- ^ Specifies where to send the output files. 
  , dataAccessRoleArn :: Types.IamRoleArn
    -- ^ The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data. For more information, see <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions> .
  , languageCode :: Types.LanguageCode
    -- ^ The language of the input documents. You can specify any of the primary languages supported by Amazon Comprehend. All documents must be in the same language.
  , clientRequestToken :: Core.Maybe Types.ClientRequestTokenString
    -- ^ A unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
  , jobName :: Core.Maybe Types.JobName
    -- ^ The identifier of the job.
  , volumeKmsKeyId :: Core.Maybe Types.KmsKeyId
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
    -- ^ Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for your sentiment detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartSentimentDetectionJob' value with any optional fields omitted.
mkStartSentimentDetectionJob
    :: Types.InputDataConfig -- ^ 'inputDataConfig'
    -> Types.OutputDataConfig -- ^ 'outputDataConfig'
    -> Types.IamRoleArn -- ^ 'dataAccessRoleArn'
    -> Types.LanguageCode -- ^ 'languageCode'
    -> StartSentimentDetectionJob
mkStartSentimentDetectionJob inputDataConfig outputDataConfig
  dataAccessRoleArn languageCode
  = StartSentimentDetectionJob'{inputDataConfig, outputDataConfig,
                                dataAccessRoleArn, languageCode, clientRequestToken = Core.Nothing,
                                jobName = Core.Nothing, volumeKmsKeyId = Core.Nothing,
                                vpcConfig = Core.Nothing}

-- | Specifies the format and location of the input data for the job.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdjInputDataConfig :: Lens.Lens' StartSentimentDetectionJob Types.InputDataConfig
ssdjInputDataConfig = Lens.field @"inputDataConfig"
{-# INLINEABLE ssdjInputDataConfig #-}
{-# DEPRECATED inputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead"  #-}

-- | Specifies where to send the output files. 
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdjOutputDataConfig :: Lens.Lens' StartSentimentDetectionJob Types.OutputDataConfig
ssdjOutputDataConfig = Lens.field @"outputDataConfig"
{-# INLINEABLE ssdjOutputDataConfig #-}
{-# DEPRECATED outputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead"  #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data. For more information, see <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions> .
--
-- /Note:/ Consider using 'dataAccessRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdjDataAccessRoleArn :: Lens.Lens' StartSentimentDetectionJob Types.IamRoleArn
ssdjDataAccessRoleArn = Lens.field @"dataAccessRoleArn"
{-# INLINEABLE ssdjDataAccessRoleArn #-}
{-# DEPRECATED dataAccessRoleArn "Use generic-lens or generic-optics with 'dataAccessRoleArn' instead"  #-}

-- | The language of the input documents. You can specify any of the primary languages supported by Amazon Comprehend. All documents must be in the same language.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdjLanguageCode :: Lens.Lens' StartSentimentDetectionJob Types.LanguageCode
ssdjLanguageCode = Lens.field @"languageCode"
{-# INLINEABLE ssdjLanguageCode #-}
{-# DEPRECATED languageCode "Use generic-lens or generic-optics with 'languageCode' instead"  #-}

-- | A unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdjClientRequestToken :: Lens.Lens' StartSentimentDetectionJob (Core.Maybe Types.ClientRequestTokenString)
ssdjClientRequestToken = Lens.field @"clientRequestToken"
{-# INLINEABLE ssdjClientRequestToken #-}
{-# DEPRECATED clientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead"  #-}

-- | The identifier of the job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdjJobName :: Lens.Lens' StartSentimentDetectionJob (Core.Maybe Types.JobName)
ssdjJobName = Lens.field @"jobName"
{-# INLINEABLE ssdjJobName #-}
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
ssdjVolumeKmsKeyId :: Lens.Lens' StartSentimentDetectionJob (Core.Maybe Types.KmsKeyId)
ssdjVolumeKmsKeyId = Lens.field @"volumeKmsKeyId"
{-# INLINEABLE ssdjVolumeKmsKeyId #-}
{-# DEPRECATED volumeKmsKeyId "Use generic-lens or generic-optics with 'volumeKmsKeyId' instead"  #-}

-- | Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for your sentiment detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> . 
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdjVpcConfig :: Lens.Lens' StartSentimentDetectionJob (Core.Maybe Types.VpcConfig)
ssdjVpcConfig = Lens.field @"vpcConfig"
{-# INLINEABLE ssdjVpcConfig #-}
{-# DEPRECATED vpcConfig "Use generic-lens or generic-optics with 'vpcConfig' instead"  #-}

instance Core.ToQuery StartSentimentDetectionJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartSentimentDetectionJob where
        toHeaders StartSentimentDetectionJob{..}
          = Core.pure
              ("X-Amz-Target", "Comprehend_20171127.StartSentimentDetectionJob")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StartSentimentDetectionJob where
        toJSON StartSentimentDetectionJob{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("InputDataConfig" Core..= inputDataConfig),
                  Core.Just ("OutputDataConfig" Core..= outputDataConfig),
                  Core.Just ("DataAccessRoleArn" Core..= dataAccessRoleArn),
                  Core.Just ("LanguageCode" Core..= languageCode),
                  ("ClientRequestToken" Core..=) Core.<$> clientRequestToken,
                  ("JobName" Core..=) Core.<$> jobName,
                  ("VolumeKmsKeyId" Core..=) Core.<$> volumeKmsKeyId,
                  ("VpcConfig" Core..=) Core.<$> vpcConfig])

instance Core.AWSRequest StartSentimentDetectionJob where
        type Rs StartSentimentDetectionJob =
             StartSentimentDetectionJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StartSentimentDetectionJobResponse' Core.<$>
                   (x Core..:? "JobId") Core.<*> x Core..:? "JobStatus" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartSentimentDetectionJobResponse' smart constructor.
data StartSentimentDetectionJobResponse = StartSentimentDetectionJobResponse'
  { jobId :: Core.Maybe Types.JobId
    -- ^ The identifier generated for the job. To get the status of a job, use this identifier with the operation.
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
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartSentimentDetectionJobResponse' value with any optional fields omitted.
mkStartSentimentDetectionJobResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartSentimentDetectionJobResponse
mkStartSentimentDetectionJobResponse responseStatus
  = StartSentimentDetectionJobResponse'{jobId = Core.Nothing,
                                        jobStatus = Core.Nothing, responseStatus}

-- | The identifier generated for the job. To get the status of a job, use this identifier with the operation.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdjrrsJobId :: Lens.Lens' StartSentimentDetectionJobResponse (Core.Maybe Types.JobId)
ssdjrrsJobId = Lens.field @"jobId"
{-# INLINEABLE ssdjrrsJobId #-}
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
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdjrrsJobStatus :: Lens.Lens' StartSentimentDetectionJobResponse (Core.Maybe Types.JobStatus)
ssdjrrsJobStatus = Lens.field @"jobStatus"
{-# INLINEABLE ssdjrrsJobStatus #-}
{-# DEPRECATED jobStatus "Use generic-lens or generic-optics with 'jobStatus' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdjrrsResponseStatus :: Lens.Lens' StartSentimentDetectionJobResponse Core.Int
ssdjrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ssdjrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
