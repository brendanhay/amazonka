{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.StartKeyPhrasesDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an asynchronous key phrase detection job for a collection of documents. Use the operation to track the status of a job.
module Network.AWS.Comprehend.StartKeyPhrasesDetectionJob
    (
    -- * Creating a request
      StartKeyPhrasesDetectionJob (..)
    , mkStartKeyPhrasesDetectionJob
    -- ** Request lenses
    , skpdjInputDataConfig
    , skpdjOutputDataConfig
    , skpdjDataAccessRoleArn
    , skpdjLanguageCode
    , skpdjClientRequestToken
    , skpdjJobName
    , skpdjVolumeKmsKeyId
    , skpdjVpcConfig

    -- * Destructuring the response
    , StartKeyPhrasesDetectionJobResponse (..)
    , mkStartKeyPhrasesDetectionJobResponse
    -- ** Response lenses
    , skpdjrfrsJobId
    , skpdjrfrsJobStatus
    , skpdjrfrsResponseStatus
    ) where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartKeyPhrasesDetectionJob' smart constructor.
data StartKeyPhrasesDetectionJob = StartKeyPhrasesDetectionJob'
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
    -- ^ Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for your key phrases detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartKeyPhrasesDetectionJob' value with any optional fields omitted.
mkStartKeyPhrasesDetectionJob
    :: Types.InputDataConfig -- ^ 'inputDataConfig'
    -> Types.OutputDataConfig -- ^ 'outputDataConfig'
    -> Types.IamRoleArn -- ^ 'dataAccessRoleArn'
    -> Types.LanguageCode -- ^ 'languageCode'
    -> StartKeyPhrasesDetectionJob
mkStartKeyPhrasesDetectionJob inputDataConfig outputDataConfig
  dataAccessRoleArn languageCode
  = StartKeyPhrasesDetectionJob'{inputDataConfig, outputDataConfig,
                                 dataAccessRoleArn, languageCode, clientRequestToken = Core.Nothing,
                                 jobName = Core.Nothing, volumeKmsKeyId = Core.Nothing,
                                 vpcConfig = Core.Nothing}

-- | Specifies the format and location of the input data for the job.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpdjInputDataConfig :: Lens.Lens' StartKeyPhrasesDetectionJob Types.InputDataConfig
skpdjInputDataConfig = Lens.field @"inputDataConfig"
{-# INLINEABLE skpdjInputDataConfig #-}
{-# DEPRECATED inputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead"  #-}

-- | Specifies where to send the output files.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpdjOutputDataConfig :: Lens.Lens' StartKeyPhrasesDetectionJob Types.OutputDataConfig
skpdjOutputDataConfig = Lens.field @"outputDataConfig"
{-# INLINEABLE skpdjOutputDataConfig #-}
{-# DEPRECATED outputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead"  #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data. For more information, see <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions> .
--
-- /Note:/ Consider using 'dataAccessRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpdjDataAccessRoleArn :: Lens.Lens' StartKeyPhrasesDetectionJob Types.IamRoleArn
skpdjDataAccessRoleArn = Lens.field @"dataAccessRoleArn"
{-# INLINEABLE skpdjDataAccessRoleArn #-}
{-# DEPRECATED dataAccessRoleArn "Use generic-lens or generic-optics with 'dataAccessRoleArn' instead"  #-}

-- | The language of the input documents. You can specify any of the primary languages supported by Amazon Comprehend. All documents must be in the same language.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpdjLanguageCode :: Lens.Lens' StartKeyPhrasesDetectionJob Types.LanguageCode
skpdjLanguageCode = Lens.field @"languageCode"
{-# INLINEABLE skpdjLanguageCode #-}
{-# DEPRECATED languageCode "Use generic-lens or generic-optics with 'languageCode' instead"  #-}

-- | A unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpdjClientRequestToken :: Lens.Lens' StartKeyPhrasesDetectionJob (Core.Maybe Types.ClientRequestTokenString)
skpdjClientRequestToken = Lens.field @"clientRequestToken"
{-# INLINEABLE skpdjClientRequestToken #-}
{-# DEPRECATED clientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead"  #-}

-- | The identifier of the job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpdjJobName :: Lens.Lens' StartKeyPhrasesDetectionJob (Core.Maybe Types.JobName)
skpdjJobName = Lens.field @"jobName"
{-# INLINEABLE skpdjJobName #-}
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
skpdjVolumeKmsKeyId :: Lens.Lens' StartKeyPhrasesDetectionJob (Core.Maybe Types.KmsKeyId)
skpdjVolumeKmsKeyId = Lens.field @"volumeKmsKeyId"
{-# INLINEABLE skpdjVolumeKmsKeyId #-}
{-# DEPRECATED volumeKmsKeyId "Use generic-lens or generic-optics with 'volumeKmsKeyId' instead"  #-}

-- | Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for your key phrases detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> . 
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpdjVpcConfig :: Lens.Lens' StartKeyPhrasesDetectionJob (Core.Maybe Types.VpcConfig)
skpdjVpcConfig = Lens.field @"vpcConfig"
{-# INLINEABLE skpdjVpcConfig #-}
{-# DEPRECATED vpcConfig "Use generic-lens or generic-optics with 'vpcConfig' instead"  #-}

instance Core.ToQuery StartKeyPhrasesDetectionJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartKeyPhrasesDetectionJob where
        toHeaders StartKeyPhrasesDetectionJob{..}
          = Core.pure
              ("X-Amz-Target", "Comprehend_20171127.StartKeyPhrasesDetectionJob")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StartKeyPhrasesDetectionJob where
        toJSON StartKeyPhrasesDetectionJob{..}
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

instance Core.AWSRequest StartKeyPhrasesDetectionJob where
        type Rs StartKeyPhrasesDetectionJob =
             StartKeyPhrasesDetectionJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StartKeyPhrasesDetectionJobResponse' Core.<$>
                   (x Core..:? "JobId") Core.<*> x Core..:? "JobStatus" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartKeyPhrasesDetectionJobResponse' smart constructor.
data StartKeyPhrasesDetectionJobResponse = StartKeyPhrasesDetectionJobResponse'
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

-- | Creates a 'StartKeyPhrasesDetectionJobResponse' value with any optional fields omitted.
mkStartKeyPhrasesDetectionJobResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartKeyPhrasesDetectionJobResponse
mkStartKeyPhrasesDetectionJobResponse responseStatus
  = StartKeyPhrasesDetectionJobResponse'{jobId = Core.Nothing,
                                         jobStatus = Core.Nothing, responseStatus}

-- | The identifier generated for the job. To get the status of a job, use this identifier with the operation.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpdjrfrsJobId :: Lens.Lens' StartKeyPhrasesDetectionJobResponse (Core.Maybe Types.JobId)
skpdjrfrsJobId = Lens.field @"jobId"
{-# INLINEABLE skpdjrfrsJobId #-}
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
skpdjrfrsJobStatus :: Lens.Lens' StartKeyPhrasesDetectionJobResponse (Core.Maybe Types.JobStatus)
skpdjrfrsJobStatus = Lens.field @"jobStatus"
{-# INLINEABLE skpdjrfrsJobStatus #-}
{-# DEPRECATED jobStatus "Use generic-lens or generic-optics with 'jobStatus' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpdjrfrsResponseStatus :: Lens.Lens' StartKeyPhrasesDetectionJobResponse Core.Int
skpdjrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE skpdjrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
