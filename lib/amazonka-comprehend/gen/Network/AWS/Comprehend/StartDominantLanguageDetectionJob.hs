{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.StartDominantLanguageDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an asynchronous dominant language detection job for a collection of documents. Use the operation to track the status of a job.
module Network.AWS.Comprehend.StartDominantLanguageDetectionJob
    (
    -- * Creating a request
      StartDominantLanguageDetectionJob (..)
    , mkStartDominantLanguageDetectionJob
    -- ** Request lenses
    , sdldjInputDataConfig
    , sdldjOutputDataConfig
    , sdldjDataAccessRoleArn
    , sdldjClientRequestToken
    , sdldjJobName
    , sdldjVolumeKmsKeyId
    , sdldjVpcConfig

    -- * Destructuring the response
    , StartDominantLanguageDetectionJobResponse (..)
    , mkStartDominantLanguageDetectionJobResponse
    -- ** Response lenses
    , sdldjrrsJobId
    , sdldjrrsJobStatus
    , sdldjrrsResponseStatus
    ) where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartDominantLanguageDetectionJob' smart constructor.
data StartDominantLanguageDetectionJob = StartDominantLanguageDetectionJob'
  { inputDataConfig :: Types.InputDataConfig
    -- ^ Specifies the format and location of the input data for the job.
  , outputDataConfig :: Types.OutputDataConfig
    -- ^ Specifies where to send the output files.
  , dataAccessRoleArn :: Types.IamRoleArn
    -- ^ The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data. For more information, see <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions> .
  , clientRequestToken :: Core.Maybe Types.ClientRequestTokenString
    -- ^ A unique identifier for the request. If you do not set the client request token, Amazon Comprehend generates one.
  , jobName :: Core.Maybe Types.JobName
    -- ^ An identifier for the job.
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
    -- ^ Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for your dominant language detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartDominantLanguageDetectionJob' value with any optional fields omitted.
mkStartDominantLanguageDetectionJob
    :: Types.InputDataConfig -- ^ 'inputDataConfig'
    -> Types.OutputDataConfig -- ^ 'outputDataConfig'
    -> Types.IamRoleArn -- ^ 'dataAccessRoleArn'
    -> StartDominantLanguageDetectionJob
mkStartDominantLanguageDetectionJob inputDataConfig
  outputDataConfig dataAccessRoleArn
  = StartDominantLanguageDetectionJob'{inputDataConfig,
                                       outputDataConfig, dataAccessRoleArn,
                                       clientRequestToken = Core.Nothing, jobName = Core.Nothing,
                                       volumeKmsKeyId = Core.Nothing, vpcConfig = Core.Nothing}

-- | Specifies the format and location of the input data for the job.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdldjInputDataConfig :: Lens.Lens' StartDominantLanguageDetectionJob Types.InputDataConfig
sdldjInputDataConfig = Lens.field @"inputDataConfig"
{-# INLINEABLE sdldjInputDataConfig #-}
{-# DEPRECATED inputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead"  #-}

-- | Specifies where to send the output files.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdldjOutputDataConfig :: Lens.Lens' StartDominantLanguageDetectionJob Types.OutputDataConfig
sdldjOutputDataConfig = Lens.field @"outputDataConfig"
{-# INLINEABLE sdldjOutputDataConfig #-}
{-# DEPRECATED outputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead"  #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data. For more information, see <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions> .
--
-- /Note:/ Consider using 'dataAccessRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdldjDataAccessRoleArn :: Lens.Lens' StartDominantLanguageDetectionJob Types.IamRoleArn
sdldjDataAccessRoleArn = Lens.field @"dataAccessRoleArn"
{-# INLINEABLE sdldjDataAccessRoleArn #-}
{-# DEPRECATED dataAccessRoleArn "Use generic-lens or generic-optics with 'dataAccessRoleArn' instead"  #-}

-- | A unique identifier for the request. If you do not set the client request token, Amazon Comprehend generates one.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdldjClientRequestToken :: Lens.Lens' StartDominantLanguageDetectionJob (Core.Maybe Types.ClientRequestTokenString)
sdldjClientRequestToken = Lens.field @"clientRequestToken"
{-# INLINEABLE sdldjClientRequestToken #-}
{-# DEPRECATED clientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead"  #-}

-- | An identifier for the job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdldjJobName :: Lens.Lens' StartDominantLanguageDetectionJob (Core.Maybe Types.JobName)
sdldjJobName = Lens.field @"jobName"
{-# INLINEABLE sdldjJobName #-}
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
sdldjVolumeKmsKeyId :: Lens.Lens' StartDominantLanguageDetectionJob (Core.Maybe Types.KmsKeyId)
sdldjVolumeKmsKeyId = Lens.field @"volumeKmsKeyId"
{-# INLINEABLE sdldjVolumeKmsKeyId #-}
{-# DEPRECATED volumeKmsKeyId "Use generic-lens or generic-optics with 'volumeKmsKeyId' instead"  #-}

-- | Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for your dominant language detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> . 
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdldjVpcConfig :: Lens.Lens' StartDominantLanguageDetectionJob (Core.Maybe Types.VpcConfig)
sdldjVpcConfig = Lens.field @"vpcConfig"
{-# INLINEABLE sdldjVpcConfig #-}
{-# DEPRECATED vpcConfig "Use generic-lens or generic-optics with 'vpcConfig' instead"  #-}

instance Core.ToQuery StartDominantLanguageDetectionJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartDominantLanguageDetectionJob where
        toHeaders StartDominantLanguageDetectionJob{..}
          = Core.pure
              ("X-Amz-Target",
               "Comprehend_20171127.StartDominantLanguageDetectionJob")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StartDominantLanguageDetectionJob where
        toJSON StartDominantLanguageDetectionJob{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("InputDataConfig" Core..= inputDataConfig),
                  Core.Just ("OutputDataConfig" Core..= outputDataConfig),
                  Core.Just ("DataAccessRoleArn" Core..= dataAccessRoleArn),
                  ("ClientRequestToken" Core..=) Core.<$> clientRequestToken,
                  ("JobName" Core..=) Core.<$> jobName,
                  ("VolumeKmsKeyId" Core..=) Core.<$> volumeKmsKeyId,
                  ("VpcConfig" Core..=) Core.<$> vpcConfig])

instance Core.AWSRequest StartDominantLanguageDetectionJob where
        type Rs StartDominantLanguageDetectionJob =
             StartDominantLanguageDetectionJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StartDominantLanguageDetectionJobResponse' Core.<$>
                   (x Core..:? "JobId") Core.<*> x Core..:? "JobStatus" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartDominantLanguageDetectionJobResponse' smart constructor.
data StartDominantLanguageDetectionJobResponse = StartDominantLanguageDetectionJobResponse'
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

-- | Creates a 'StartDominantLanguageDetectionJobResponse' value with any optional fields omitted.
mkStartDominantLanguageDetectionJobResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartDominantLanguageDetectionJobResponse
mkStartDominantLanguageDetectionJobResponse responseStatus
  = StartDominantLanguageDetectionJobResponse'{jobId = Core.Nothing,
                                               jobStatus = Core.Nothing, responseStatus}

-- | The identifier generated for the job. To get the status of a job, use this identifier with the operation.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdldjrrsJobId :: Lens.Lens' StartDominantLanguageDetectionJobResponse (Core.Maybe Types.JobId)
sdldjrrsJobId = Lens.field @"jobId"
{-# INLINEABLE sdldjrrsJobId #-}
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
sdldjrrsJobStatus :: Lens.Lens' StartDominantLanguageDetectionJobResponse (Core.Maybe Types.JobStatus)
sdldjrrsJobStatus = Lens.field @"jobStatus"
{-# INLINEABLE sdldjrrsJobStatus #-}
{-# DEPRECATED jobStatus "Use generic-lens or generic-optics with 'jobStatus' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdldjrrsResponseStatus :: Lens.Lens' StartDominantLanguageDetectionJobResponse Core.Int
sdldjrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE sdldjrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
