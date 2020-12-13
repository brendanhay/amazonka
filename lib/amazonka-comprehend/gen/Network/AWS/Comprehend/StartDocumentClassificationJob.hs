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
    sdcjDocumentClassifierARN,
    sdcjJobName,
    sdcjInputDataConfig,
    sdcjVPCConfig,
    sdcjVolumeKMSKeyId,
    sdcjOutputDataConfig,
    sdcjDataAccessRoleARN,
    sdcjClientRequestToken,

    -- * Destructuring the response
    StartDocumentClassificationJobResponse (..),
    mkStartDocumentClassificationJobResponse,

    -- ** Response lenses
    sdcjrsJobId,
    sdcjrsJobStatus,
    sdcjrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartDocumentClassificationJob' smart constructor.
data StartDocumentClassificationJob = StartDocumentClassificationJob'
  { -- | The Amazon Resource Name (ARN) of the document classifier to use to process the job.
    documentClassifierARN :: Lude.Text,
    -- | The identifier of the job.
    jobName :: Lude.Maybe Lude.Text,
    -- | Specifies the format and location of the input data for the job.
    inputDataConfig :: InputDataConfig,
    -- | Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for your document classification job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
    vpcConfig :: Lude.Maybe VPCConfig,
    -- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:
    --
    --
    --     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@
    --
    --
    --     * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
    volumeKMSKeyId :: Lude.Maybe Lude.Text,
    -- | Specifies where to send the output files.
    outputDataConfig :: OutputDataConfig,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data.
    dataAccessRoleARN :: Lude.Text,
    -- | A unique identifier for the request. If you do not set the client request token, Amazon Comprehend generates one.
    clientRequestToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartDocumentClassificationJob' with the minimum fields required to make a request.
--
-- * 'documentClassifierARN' - The Amazon Resource Name (ARN) of the document classifier to use to process the job.
-- * 'jobName' - The identifier of the job.
-- * 'inputDataConfig' - Specifies the format and location of the input data for the job.
-- * 'vpcConfig' - Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for your document classification job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
-- * 'volumeKMSKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:
--
--
--     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@
--
--
--     * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
--
--
-- * 'outputDataConfig' - Specifies where to send the output files.
-- * 'dataAccessRoleARN' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data.
-- * 'clientRequestToken' - A unique identifier for the request. If you do not set the client request token, Amazon Comprehend generates one.
mkStartDocumentClassificationJob ::
  -- | 'documentClassifierARN'
  Lude.Text ->
  -- | 'inputDataConfig'
  InputDataConfig ->
  -- | 'outputDataConfig'
  OutputDataConfig ->
  -- | 'dataAccessRoleARN'
  Lude.Text ->
  StartDocumentClassificationJob
mkStartDocumentClassificationJob
  pDocumentClassifierARN_
  pInputDataConfig_
  pOutputDataConfig_
  pDataAccessRoleARN_ =
    StartDocumentClassificationJob'
      { documentClassifierARN =
          pDocumentClassifierARN_,
        jobName = Lude.Nothing,
        inputDataConfig = pInputDataConfig_,
        vpcConfig = Lude.Nothing,
        volumeKMSKeyId = Lude.Nothing,
        outputDataConfig = pOutputDataConfig_,
        dataAccessRoleARN = pDataAccessRoleARN_,
        clientRequestToken = Lude.Nothing
      }

-- | The Amazon Resource Name (ARN) of the document classifier to use to process the job.
--
-- /Note:/ Consider using 'documentClassifierARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcjDocumentClassifierARN :: Lens.Lens' StartDocumentClassificationJob Lude.Text
sdcjDocumentClassifierARN = Lens.lens (documentClassifierARN :: StartDocumentClassificationJob -> Lude.Text) (\s a -> s {documentClassifierARN = a} :: StartDocumentClassificationJob)
{-# DEPRECATED sdcjDocumentClassifierARN "Use generic-lens or generic-optics with 'documentClassifierARN' instead." #-}

-- | The identifier of the job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcjJobName :: Lens.Lens' StartDocumentClassificationJob (Lude.Maybe Lude.Text)
sdcjJobName = Lens.lens (jobName :: StartDocumentClassificationJob -> Lude.Maybe Lude.Text) (\s a -> s {jobName = a} :: StartDocumentClassificationJob)
{-# DEPRECATED sdcjJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | Specifies the format and location of the input data for the job.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcjInputDataConfig :: Lens.Lens' StartDocumentClassificationJob InputDataConfig
sdcjInputDataConfig = Lens.lens (inputDataConfig :: StartDocumentClassificationJob -> InputDataConfig) (\s a -> s {inputDataConfig = a} :: StartDocumentClassificationJob)
{-# DEPRECATED sdcjInputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead." #-}

-- | Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for your document classification job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcjVPCConfig :: Lens.Lens' StartDocumentClassificationJob (Lude.Maybe VPCConfig)
sdcjVPCConfig = Lens.lens (vpcConfig :: StartDocumentClassificationJob -> Lude.Maybe VPCConfig) (\s a -> s {vpcConfig = a} :: StartDocumentClassificationJob)
{-# DEPRECATED sdcjVPCConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

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
-- /Note:/ Consider using 'volumeKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcjVolumeKMSKeyId :: Lens.Lens' StartDocumentClassificationJob (Lude.Maybe Lude.Text)
sdcjVolumeKMSKeyId = Lens.lens (volumeKMSKeyId :: StartDocumentClassificationJob -> Lude.Maybe Lude.Text) (\s a -> s {volumeKMSKeyId = a} :: StartDocumentClassificationJob)
{-# DEPRECATED sdcjVolumeKMSKeyId "Use generic-lens or generic-optics with 'volumeKMSKeyId' instead." #-}

-- | Specifies where to send the output files.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcjOutputDataConfig :: Lens.Lens' StartDocumentClassificationJob OutputDataConfig
sdcjOutputDataConfig = Lens.lens (outputDataConfig :: StartDocumentClassificationJob -> OutputDataConfig) (\s a -> s {outputDataConfig = a} :: StartDocumentClassificationJob)
{-# DEPRECATED sdcjOutputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data.
--
-- /Note:/ Consider using 'dataAccessRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcjDataAccessRoleARN :: Lens.Lens' StartDocumentClassificationJob Lude.Text
sdcjDataAccessRoleARN = Lens.lens (dataAccessRoleARN :: StartDocumentClassificationJob -> Lude.Text) (\s a -> s {dataAccessRoleARN = a} :: StartDocumentClassificationJob)
{-# DEPRECATED sdcjDataAccessRoleARN "Use generic-lens or generic-optics with 'dataAccessRoleARN' instead." #-}

-- | A unique identifier for the request. If you do not set the client request token, Amazon Comprehend generates one.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcjClientRequestToken :: Lens.Lens' StartDocumentClassificationJob (Lude.Maybe Lude.Text)
sdcjClientRequestToken = Lens.lens (clientRequestToken :: StartDocumentClassificationJob -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: StartDocumentClassificationJob)
{-# DEPRECATED sdcjClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

instance Lude.AWSRequest StartDocumentClassificationJob where
  type
    Rs StartDocumentClassificationJob =
      StartDocumentClassificationJobResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartDocumentClassificationJobResponse'
            Lude.<$> (x Lude..?> "JobId")
            Lude.<*> (x Lude..?> "JobStatus")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartDocumentClassificationJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Comprehend_20171127.StartDocumentClassificationJob" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartDocumentClassificationJob where
  toJSON StartDocumentClassificationJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DocumentClassifierArn" Lude..= documentClassifierARN),
            ("JobName" Lude..=) Lude.<$> jobName,
            Lude.Just ("InputDataConfig" Lude..= inputDataConfig),
            ("VpcConfig" Lude..=) Lude.<$> vpcConfig,
            ("VolumeKmsKeyId" Lude..=) Lude.<$> volumeKMSKeyId,
            Lude.Just ("OutputDataConfig" Lude..= outputDataConfig),
            Lude.Just ("DataAccessRoleArn" Lude..= dataAccessRoleARN),
            ("ClientRequestToken" Lude..=) Lude.<$> clientRequestToken
          ]
      )

instance Lude.ToPath StartDocumentClassificationJob where
  toPath = Lude.const "/"

instance Lude.ToQuery StartDocumentClassificationJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartDocumentClassificationJobResponse' smart constructor.
data StartDocumentClassificationJobResponse = StartDocumentClassificationJobResponse'
  { -- | The identifier generated for the job. To get the status of the job, use this identifier with the operation.
    jobId :: Lude.Maybe Lude.Text,
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
    jobStatus :: Lude.Maybe JobStatus,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartDocumentClassificationJobResponse' with the minimum fields required to make a request.
--
-- * 'jobId' - The identifier generated for the job. To get the status of the job, use this identifier with the operation.
-- * 'jobStatus' - The status of the job:
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
-- * 'responseStatus' - The response status code.
mkStartDocumentClassificationJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartDocumentClassificationJobResponse
mkStartDocumentClassificationJobResponse pResponseStatus_ =
  StartDocumentClassificationJobResponse'
    { jobId = Lude.Nothing,
      jobStatus = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier generated for the job. To get the status of the job, use this identifier with the operation.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcjrsJobId :: Lens.Lens' StartDocumentClassificationJobResponse (Lude.Maybe Lude.Text)
sdcjrsJobId = Lens.lens (jobId :: StartDocumentClassificationJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: StartDocumentClassificationJobResponse)
{-# DEPRECATED sdcjrsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

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
sdcjrsJobStatus :: Lens.Lens' StartDocumentClassificationJobResponse (Lude.Maybe JobStatus)
sdcjrsJobStatus = Lens.lens (jobStatus :: StartDocumentClassificationJobResponse -> Lude.Maybe JobStatus) (\s a -> s {jobStatus = a} :: StartDocumentClassificationJobResponse)
{-# DEPRECATED sdcjrsJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcjrsResponseStatus :: Lens.Lens' StartDocumentClassificationJobResponse Lude.Int
sdcjrsResponseStatus = Lens.lens (responseStatus :: StartDocumentClassificationJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartDocumentClassificationJobResponse)
{-# DEPRECATED sdcjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
