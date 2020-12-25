{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.StartDocumentClassificationJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an asynchronous document classification job. Use the operation to track the progress of the job.
module Network.AWS.Comprehend.StartDocumentClassificationJob
  ( -- * Creating a request
    StartDocumentClassificationJob (..),
    mkStartDocumentClassificationJob,

    -- ** Request lenses
    sdcjDocumentClassifierArn,
    sdcjInputDataConfig,
    sdcjOutputDataConfig,
    sdcjDataAccessRoleArn,
    sdcjClientRequestToken,
    sdcjJobName,
    sdcjVolumeKmsKeyId,
    sdcjVpcConfig,

    -- * Destructuring the response
    StartDocumentClassificationJobResponse (..),
    mkStartDocumentClassificationJobResponse,

    -- ** Response lenses
    sdcjrrsJobId,
    sdcjrrsJobStatus,
    sdcjrrsResponseStatus,
  )
where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartDocumentClassificationJob' smart constructor.
data StartDocumentClassificationJob = StartDocumentClassificationJob'
  { -- | The Amazon Resource Name (ARN) of the document classifier to use to process the job.
    documentClassifierArn :: Types.DocumentClassifierArn,
    -- | Specifies the format and location of the input data for the job.
    inputDataConfig :: Types.InputDataConfig,
    -- | Specifies where to send the output files.
    outputDataConfig :: Types.OutputDataConfig,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data.
    dataAccessRoleArn :: Types.IamRoleArn,
    -- | A unique identifier for the request. If you do not set the client request token, Amazon Comprehend generates one.
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
    -- | Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for your document classification job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
    vpcConfig :: Core.Maybe Types.VpcConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartDocumentClassificationJob' value with any optional fields omitted.
mkStartDocumentClassificationJob ::
  -- | 'documentClassifierArn'
  Types.DocumentClassifierArn ->
  -- | 'inputDataConfig'
  Types.InputDataConfig ->
  -- | 'outputDataConfig'
  Types.OutputDataConfig ->
  -- | 'dataAccessRoleArn'
  Types.IamRoleArn ->
  StartDocumentClassificationJob
mkStartDocumentClassificationJob
  documentClassifierArn
  inputDataConfig
  outputDataConfig
  dataAccessRoleArn =
    StartDocumentClassificationJob'
      { documentClassifierArn,
        inputDataConfig,
        outputDataConfig,
        dataAccessRoleArn,
        clientRequestToken = Core.Nothing,
        jobName = Core.Nothing,
        volumeKmsKeyId = Core.Nothing,
        vpcConfig = Core.Nothing
      }

-- | The Amazon Resource Name (ARN) of the document classifier to use to process the job.
--
-- /Note:/ Consider using 'documentClassifierArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcjDocumentClassifierArn :: Lens.Lens' StartDocumentClassificationJob Types.DocumentClassifierArn
sdcjDocumentClassifierArn = Lens.field @"documentClassifierArn"
{-# DEPRECATED sdcjDocumentClassifierArn "Use generic-lens or generic-optics with 'documentClassifierArn' instead." #-}

-- | Specifies the format and location of the input data for the job.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcjInputDataConfig :: Lens.Lens' StartDocumentClassificationJob Types.InputDataConfig
sdcjInputDataConfig = Lens.field @"inputDataConfig"
{-# DEPRECATED sdcjInputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead." #-}

-- | Specifies where to send the output files.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcjOutputDataConfig :: Lens.Lens' StartDocumentClassificationJob Types.OutputDataConfig
sdcjOutputDataConfig = Lens.field @"outputDataConfig"
{-# DEPRECATED sdcjOutputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data.
--
-- /Note:/ Consider using 'dataAccessRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcjDataAccessRoleArn :: Lens.Lens' StartDocumentClassificationJob Types.IamRoleArn
sdcjDataAccessRoleArn = Lens.field @"dataAccessRoleArn"
{-# DEPRECATED sdcjDataAccessRoleArn "Use generic-lens or generic-optics with 'dataAccessRoleArn' instead." #-}

-- | A unique identifier for the request. If you do not set the client request token, Amazon Comprehend generates one.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcjClientRequestToken :: Lens.Lens' StartDocumentClassificationJob (Core.Maybe Types.ClientRequestTokenString)
sdcjClientRequestToken = Lens.field @"clientRequestToken"
{-# DEPRECATED sdcjClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | The identifier of the job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcjJobName :: Lens.Lens' StartDocumentClassificationJob (Core.Maybe Types.JobName)
sdcjJobName = Lens.field @"jobName"
{-# DEPRECATED sdcjJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

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
sdcjVolumeKmsKeyId :: Lens.Lens' StartDocumentClassificationJob (Core.Maybe Types.KmsKeyId)
sdcjVolumeKmsKeyId = Lens.field @"volumeKmsKeyId"
{-# DEPRECATED sdcjVolumeKmsKeyId "Use generic-lens or generic-optics with 'volumeKmsKeyId' instead." #-}

-- | Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for your document classification job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcjVpcConfig :: Lens.Lens' StartDocumentClassificationJob (Core.Maybe Types.VpcConfig)
sdcjVpcConfig = Lens.field @"vpcConfig"
{-# DEPRECATED sdcjVpcConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

instance Core.FromJSON StartDocumentClassificationJob where
  toJSON StartDocumentClassificationJob {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DocumentClassifierArn" Core..= documentClassifierArn),
            Core.Just ("InputDataConfig" Core..= inputDataConfig),
            Core.Just ("OutputDataConfig" Core..= outputDataConfig),
            Core.Just ("DataAccessRoleArn" Core..= dataAccessRoleArn),
            ("ClientRequestToken" Core..=) Core.<$> clientRequestToken,
            ("JobName" Core..=) Core.<$> jobName,
            ("VolumeKmsKeyId" Core..=) Core.<$> volumeKmsKeyId,
            ("VpcConfig" Core..=) Core.<$> vpcConfig
          ]
      )

instance Core.AWSRequest StartDocumentClassificationJob where
  type
    Rs StartDocumentClassificationJob =
      StartDocumentClassificationJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "Comprehend_20171127.StartDocumentClassificationJob"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartDocumentClassificationJobResponse'
            Core.<$> (x Core..:? "JobId")
            Core.<*> (x Core..:? "JobStatus")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartDocumentClassificationJobResponse' smart constructor.
data StartDocumentClassificationJobResponse = StartDocumentClassificationJobResponse'
  { -- | The identifier generated for the job. To get the status of the job, use this identifier with the operation.
    jobId :: Core.Maybe Types.JobId,
    -- | The status of the job:
    --
    --
    --     * SUBMITTED - The job has been received and queued for processing.
    --
    --
    --     * IN_PROGRESS - Amazon Comprehend is processing the job.
    --
    --
    --     * COMPLETED - The job was successfully completed and the output is available.
    --
    --
    --     * FAILED - The job did not complete. For details, use the operation.
    --
    --
    --     * STOP_REQUESTED - Amazon Comprehend has received a stop request for the job and is processing the request.
    --
    --
    --     * STOPPED - The job was successfully stopped without completing.
    jobStatus :: Core.Maybe Types.JobStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartDocumentClassificationJobResponse' value with any optional fields omitted.
mkStartDocumentClassificationJobResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartDocumentClassificationJobResponse
mkStartDocumentClassificationJobResponse responseStatus =
  StartDocumentClassificationJobResponse'
    { jobId = Core.Nothing,
      jobStatus = Core.Nothing,
      responseStatus
    }

-- | The identifier generated for the job. To get the status of the job, use this identifier with the operation.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcjrrsJobId :: Lens.Lens' StartDocumentClassificationJobResponse (Core.Maybe Types.JobId)
sdcjrrsJobId = Lens.field @"jobId"
{-# DEPRECATED sdcjrrsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The status of the job:
--
--
--     * SUBMITTED - The job has been received and queued for processing.
--
--
--     * IN_PROGRESS - Amazon Comprehend is processing the job.
--
--
--     * COMPLETED - The job was successfully completed and the output is available.
--
--
--     * FAILED - The job did not complete. For details, use the operation.
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
sdcjrrsJobStatus :: Lens.Lens' StartDocumentClassificationJobResponse (Core.Maybe Types.JobStatus)
sdcjrrsJobStatus = Lens.field @"jobStatus"
{-# DEPRECATED sdcjrrsJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcjrrsResponseStatus :: Lens.Lens' StartDocumentClassificationJobResponse Core.Int
sdcjrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sdcjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
