{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.StartTopicsDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an asynchronous topic detection job. Use the @DescribeTopicDetectionJob@ operation to track the status of a job.
module Network.AWS.Comprehend.StartTopicsDetectionJob
    (
    -- * Creating a request
      StartTopicsDetectionJob (..)
    , mkStartTopicsDetectionJob
    -- ** Request lenses
    , stdjInputDataConfig
    , stdjOutputDataConfig
    , stdjDataAccessRoleArn
    , stdjClientRequestToken
    , stdjJobName
    , stdjNumberOfTopics
    , stdjVolumeKmsKeyId
    , stdjVpcConfig

    -- * Destructuring the response
    , StartTopicsDetectionJobResponse (..)
    , mkStartTopicsDetectionJobResponse
    -- ** Response lenses
    , stdjrrsJobId
    , stdjrrsJobStatus
    , stdjrrsResponseStatus
    ) where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartTopicsDetectionJob' smart constructor.
data StartTopicsDetectionJob = StartTopicsDetectionJob'
  { inputDataConfig :: Types.InputDataConfig
    -- ^ Specifies the format and location of the input data for the job.
  , outputDataConfig :: Types.OutputDataConfig
    -- ^ Specifies where to send the output files. The output is a compressed archive with two files, @topic-terms.csv@ that lists the terms associated with each topic, and @doc-topics.csv@ that lists the documents associated with each topic
  , dataAccessRoleArn :: Types.IamRoleArn
    -- ^ The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data. For more information, see <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions> .
  , clientRequestToken :: Core.Maybe Types.ClientRequestTokenString
    -- ^ A unique identifier for the request. If you do not set the client request token, Amazon Comprehend generates one.
  , jobName :: Core.Maybe Types.JobName
    -- ^ The identifier of the job.
  , numberOfTopics :: Core.Maybe Core.Natural
    -- ^ The number of topics to detect.
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
    -- ^ Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for your topic detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartTopicsDetectionJob' value with any optional fields omitted.
mkStartTopicsDetectionJob
    :: Types.InputDataConfig -- ^ 'inputDataConfig'
    -> Types.OutputDataConfig -- ^ 'outputDataConfig'
    -> Types.IamRoleArn -- ^ 'dataAccessRoleArn'
    -> StartTopicsDetectionJob
mkStartTopicsDetectionJob inputDataConfig outputDataConfig
  dataAccessRoleArn
  = StartTopicsDetectionJob'{inputDataConfig, outputDataConfig,
                             dataAccessRoleArn, clientRequestToken = Core.Nothing,
                             jobName = Core.Nothing, numberOfTopics = Core.Nothing,
                             volumeKmsKeyId = Core.Nothing, vpcConfig = Core.Nothing}

-- | Specifies the format and location of the input data for the job.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdjInputDataConfig :: Lens.Lens' StartTopicsDetectionJob Types.InputDataConfig
stdjInputDataConfig = Lens.field @"inputDataConfig"
{-# INLINEABLE stdjInputDataConfig #-}
{-# DEPRECATED inputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead"  #-}

-- | Specifies where to send the output files. The output is a compressed archive with two files, @topic-terms.csv@ that lists the terms associated with each topic, and @doc-topics.csv@ that lists the documents associated with each topic
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdjOutputDataConfig :: Lens.Lens' StartTopicsDetectionJob Types.OutputDataConfig
stdjOutputDataConfig = Lens.field @"outputDataConfig"
{-# INLINEABLE stdjOutputDataConfig #-}
{-# DEPRECATED outputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead"  #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data. For more information, see <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions> .
--
-- /Note:/ Consider using 'dataAccessRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdjDataAccessRoleArn :: Lens.Lens' StartTopicsDetectionJob Types.IamRoleArn
stdjDataAccessRoleArn = Lens.field @"dataAccessRoleArn"
{-# INLINEABLE stdjDataAccessRoleArn #-}
{-# DEPRECATED dataAccessRoleArn "Use generic-lens or generic-optics with 'dataAccessRoleArn' instead"  #-}

-- | A unique identifier for the request. If you do not set the client request token, Amazon Comprehend generates one.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdjClientRequestToken :: Lens.Lens' StartTopicsDetectionJob (Core.Maybe Types.ClientRequestTokenString)
stdjClientRequestToken = Lens.field @"clientRequestToken"
{-# INLINEABLE stdjClientRequestToken #-}
{-# DEPRECATED clientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead"  #-}

-- | The identifier of the job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdjJobName :: Lens.Lens' StartTopicsDetectionJob (Core.Maybe Types.JobName)
stdjJobName = Lens.field @"jobName"
{-# INLINEABLE stdjJobName #-}
{-# DEPRECATED jobName "Use generic-lens or generic-optics with 'jobName' instead"  #-}

-- | The number of topics to detect.
--
-- /Note:/ Consider using 'numberOfTopics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdjNumberOfTopics :: Lens.Lens' StartTopicsDetectionJob (Core.Maybe Core.Natural)
stdjNumberOfTopics = Lens.field @"numberOfTopics"
{-# INLINEABLE stdjNumberOfTopics #-}
{-# DEPRECATED numberOfTopics "Use generic-lens or generic-optics with 'numberOfTopics' instead"  #-}

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
stdjVolumeKmsKeyId :: Lens.Lens' StartTopicsDetectionJob (Core.Maybe Types.KmsKeyId)
stdjVolumeKmsKeyId = Lens.field @"volumeKmsKeyId"
{-# INLINEABLE stdjVolumeKmsKeyId #-}
{-# DEPRECATED volumeKmsKeyId "Use generic-lens or generic-optics with 'volumeKmsKeyId' instead"  #-}

-- | Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for your topic detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> . 
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdjVpcConfig :: Lens.Lens' StartTopicsDetectionJob (Core.Maybe Types.VpcConfig)
stdjVpcConfig = Lens.field @"vpcConfig"
{-# INLINEABLE stdjVpcConfig #-}
{-# DEPRECATED vpcConfig "Use generic-lens or generic-optics with 'vpcConfig' instead"  #-}

instance Core.ToQuery StartTopicsDetectionJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartTopicsDetectionJob where
        toHeaders StartTopicsDetectionJob{..}
          = Core.pure
              ("X-Amz-Target", "Comprehend_20171127.StartTopicsDetectionJob")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StartTopicsDetectionJob where
        toJSON StartTopicsDetectionJob{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("InputDataConfig" Core..= inputDataConfig),
                  Core.Just ("OutputDataConfig" Core..= outputDataConfig),
                  Core.Just ("DataAccessRoleArn" Core..= dataAccessRoleArn),
                  ("ClientRequestToken" Core..=) Core.<$> clientRequestToken,
                  ("JobName" Core..=) Core.<$> jobName,
                  ("NumberOfTopics" Core..=) Core.<$> numberOfTopics,
                  ("VolumeKmsKeyId" Core..=) Core.<$> volumeKmsKeyId,
                  ("VpcConfig" Core..=) Core.<$> vpcConfig])

instance Core.AWSRequest StartTopicsDetectionJob where
        type Rs StartTopicsDetectionJob = StartTopicsDetectionJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StartTopicsDetectionJobResponse' Core.<$>
                   (x Core..:? "JobId") Core.<*> x Core..:? "JobStatus" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartTopicsDetectionJobResponse' smart constructor.
data StartTopicsDetectionJobResponse = StartTopicsDetectionJobResponse'
  { jobId :: Core.Maybe Types.JobId
    -- ^ The identifier generated for the job. To get the status of the job, use this identifier with the @DescribeTopicDetectionJob@ operation.
  , jobStatus :: Core.Maybe Types.JobStatus
    -- ^ The status of the job: 
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
--     * FAILED - The job did not complete. To get details, use the @DescribeTopicDetectionJob@ operation.
--
--
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartTopicsDetectionJobResponse' value with any optional fields omitted.
mkStartTopicsDetectionJobResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartTopicsDetectionJobResponse
mkStartTopicsDetectionJobResponse responseStatus
  = StartTopicsDetectionJobResponse'{jobId = Core.Nothing,
                                     jobStatus = Core.Nothing, responseStatus}

-- | The identifier generated for the job. To get the status of the job, use this identifier with the @DescribeTopicDetectionJob@ operation.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdjrrsJobId :: Lens.Lens' StartTopicsDetectionJobResponse (Core.Maybe Types.JobId)
stdjrrsJobId = Lens.field @"jobId"
{-# INLINEABLE stdjrrsJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

-- | The status of the job: 
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
--     * FAILED - The job did not complete. To get details, use the @DescribeTopicDetectionJob@ operation.
--
--
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdjrrsJobStatus :: Lens.Lens' StartTopicsDetectionJobResponse (Core.Maybe Types.JobStatus)
stdjrrsJobStatus = Lens.field @"jobStatus"
{-# INLINEABLE stdjrrsJobStatus #-}
{-# DEPRECATED jobStatus "Use generic-lens or generic-optics with 'jobStatus' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdjrrsResponseStatus :: Lens.Lens' StartTopicsDetectionJobResponse Core.Int
stdjrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE stdjrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
