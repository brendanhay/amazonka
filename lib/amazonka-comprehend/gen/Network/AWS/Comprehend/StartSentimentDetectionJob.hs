{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    StartSentimentDetectionJob (..),
    mkStartSentimentDetectionJob,

    -- ** Request lenses
    ssdjInputDataConfig,
    ssdjOutputDataConfig,
    ssdjDataAccessRoleArn,
    ssdjLanguageCode,
    ssdjClientRequestToken,
    ssdjJobName,
    ssdjVolumeKmsKeyId,
    ssdjVpcConfig,

    -- * Destructuring the response
    StartSentimentDetectionJobResponse (..),
    mkStartSentimentDetectionJobResponse,

    -- ** Response lenses
    ssdjrrsJobId,
    ssdjrrsJobStatus,
    ssdjrrsResponseStatus,
  )
where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartSentimentDetectionJob' smart constructor.
data StartSentimentDetectionJob = StartSentimentDetectionJob'
  { -- | Specifies the format and location of the input data for the job.
    inputDataConfig :: Types.InputDataConfig,
    -- | Specifies where to send the output files.
    outputDataConfig :: Types.OutputDataConfig,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data. For more information, see <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions> .
    dataAccessRoleArn :: Types.IamRoleArn,
    -- | The language of the input documents. You can specify any of the primary languages supported by Amazon Comprehend. All documents must be in the same language.
    languageCode :: Types.LanguageCode,
    -- | A unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
    clientRequestToken :: Core.Maybe Types.ClientRequestTokenString,
    -- | The identifier of the job.
    jobName :: Core.Maybe Types.JobName,
    -- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:
    --
    --
    --     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@
    --
    --
    --     * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
    volumeKmsKeyId :: Core.Maybe Types.KmsKeyId,
    -- | Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for your sentiment detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
    vpcConfig :: Core.Maybe Types.VpcConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartSentimentDetectionJob' value with any optional fields omitted.
mkStartSentimentDetectionJob ::
  -- | 'inputDataConfig'
  Types.InputDataConfig ->
  -- | 'outputDataConfig'
  Types.OutputDataConfig ->
  -- | 'dataAccessRoleArn'
  Types.IamRoleArn ->
  -- | 'languageCode'
  Types.LanguageCode ->
  StartSentimentDetectionJob
mkStartSentimentDetectionJob
  inputDataConfig
  outputDataConfig
  dataAccessRoleArn
  languageCode =
    StartSentimentDetectionJob'
      { inputDataConfig,
        outputDataConfig,
        dataAccessRoleArn,
        languageCode,
        clientRequestToken = Core.Nothing,
        jobName = Core.Nothing,
        volumeKmsKeyId = Core.Nothing,
        vpcConfig = Core.Nothing
      }

-- | Specifies the format and location of the input data for the job.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdjInputDataConfig :: Lens.Lens' StartSentimentDetectionJob Types.InputDataConfig
ssdjInputDataConfig = Lens.field @"inputDataConfig"
{-# DEPRECATED ssdjInputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead." #-}

-- | Specifies where to send the output files.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdjOutputDataConfig :: Lens.Lens' StartSentimentDetectionJob Types.OutputDataConfig
ssdjOutputDataConfig = Lens.field @"outputDataConfig"
{-# DEPRECATED ssdjOutputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data. For more information, see <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions> .
--
-- /Note:/ Consider using 'dataAccessRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdjDataAccessRoleArn :: Lens.Lens' StartSentimentDetectionJob Types.IamRoleArn
ssdjDataAccessRoleArn = Lens.field @"dataAccessRoleArn"
{-# DEPRECATED ssdjDataAccessRoleArn "Use generic-lens or generic-optics with 'dataAccessRoleArn' instead." #-}

-- | The language of the input documents. You can specify any of the primary languages supported by Amazon Comprehend. All documents must be in the same language.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdjLanguageCode :: Lens.Lens' StartSentimentDetectionJob Types.LanguageCode
ssdjLanguageCode = Lens.field @"languageCode"
{-# DEPRECATED ssdjLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | A unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdjClientRequestToken :: Lens.Lens' StartSentimentDetectionJob (Core.Maybe Types.ClientRequestTokenString)
ssdjClientRequestToken = Lens.field @"clientRequestToken"
{-# DEPRECATED ssdjClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | The identifier of the job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdjJobName :: Lens.Lens' StartSentimentDetectionJob (Core.Maybe Types.JobName)
ssdjJobName = Lens.field @"jobName"
{-# DEPRECATED ssdjJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

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
{-# DEPRECATED ssdjVolumeKmsKeyId "Use generic-lens or generic-optics with 'volumeKmsKeyId' instead." #-}

-- | Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for your sentiment detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdjVpcConfig :: Lens.Lens' StartSentimentDetectionJob (Core.Maybe Types.VpcConfig)
ssdjVpcConfig = Lens.field @"vpcConfig"
{-# DEPRECATED ssdjVpcConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

instance Core.FromJSON StartSentimentDetectionJob where
  toJSON StartSentimentDetectionJob {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("InputDataConfig" Core..= inputDataConfig),
            Core.Just ("OutputDataConfig" Core..= outputDataConfig),
            Core.Just ("DataAccessRoleArn" Core..= dataAccessRoleArn),
            Core.Just ("LanguageCode" Core..= languageCode),
            ("ClientRequestToken" Core..=) Core.<$> clientRequestToken,
            ("JobName" Core..=) Core.<$> jobName,
            ("VolumeKmsKeyId" Core..=) Core.<$> volumeKmsKeyId,
            ("VpcConfig" Core..=) Core.<$> vpcConfig
          ]
      )

instance Core.AWSRequest StartSentimentDetectionJob where
  type
    Rs StartSentimentDetectionJob =
      StartSentimentDetectionJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Comprehend_20171127.StartSentimentDetectionJob")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartSentimentDetectionJobResponse'
            Core.<$> (x Core..:? "JobId")
            Core.<*> (x Core..:? "JobStatus")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartSentimentDetectionJobResponse' smart constructor.
data StartSentimentDetectionJobResponse = StartSentimentDetectionJobResponse'
  { -- | The identifier generated for the job. To get the status of a job, use this identifier with the operation.
    jobId :: Core.Maybe Types.JobId,
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
    jobStatus :: Core.Maybe Types.JobStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartSentimentDetectionJobResponse' value with any optional fields omitted.
mkStartSentimentDetectionJobResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartSentimentDetectionJobResponse
mkStartSentimentDetectionJobResponse responseStatus =
  StartSentimentDetectionJobResponse'
    { jobId = Core.Nothing,
      jobStatus = Core.Nothing,
      responseStatus
    }

-- | The identifier generated for the job. To get the status of a job, use this identifier with the operation.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdjrrsJobId :: Lens.Lens' StartSentimentDetectionJobResponse (Core.Maybe Types.JobId)
ssdjrrsJobId = Lens.field @"jobId"
{-# DEPRECATED ssdjrrsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

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
{-# DEPRECATED ssdjrrsJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdjrrsResponseStatus :: Lens.Lens' StartSentimentDetectionJobResponse Core.Int
ssdjrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ssdjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
