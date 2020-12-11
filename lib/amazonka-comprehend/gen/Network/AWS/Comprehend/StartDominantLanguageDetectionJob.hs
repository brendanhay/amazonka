{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    StartDominantLanguageDetectionJob (..),
    mkStartDominantLanguageDetectionJob,

    -- ** Request lenses
    sdldjJobName,
    sdldjVPCConfig,
    sdldjVolumeKMSKeyId,
    sdldjClientRequestToken,
    sdldjInputDataConfig,
    sdldjOutputDataConfig,
    sdldjDataAccessRoleARN,

    -- * Destructuring the response
    StartDominantLanguageDetectionJobResponse (..),
    mkStartDominantLanguageDetectionJobResponse,

    -- ** Response lenses
    sdldjrsJobId,
    sdldjrsJobStatus,
    sdldjrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartDominantLanguageDetectionJob' smart constructor.
data StartDominantLanguageDetectionJob = StartDominantLanguageDetectionJob'
  { jobName ::
      Lude.Maybe Lude.Text,
    vpcConfig ::
      Lude.Maybe VPCConfig,
    volumeKMSKeyId ::
      Lude.Maybe Lude.Text,
    clientRequestToken ::
      Lude.Maybe Lude.Text,
    inputDataConfig ::
      InputDataConfig,
    outputDataConfig ::
      OutputDataConfig,
    dataAccessRoleARN ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartDominantLanguageDetectionJob' with the minimum fields required to make a request.
--
-- * 'clientRequestToken' - A unique identifier for the request. If you do not set the client request token, Amazon Comprehend generates one.
-- * 'dataAccessRoleARN' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data. For more information, see <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions> .
-- * 'inputDataConfig' - Specifies the format and location of the input data for the job.
-- * 'jobName' - An identifier for the job.
-- * 'outputDataConfig' - Specifies where to send the output files.
-- * 'volumeKMSKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:
--
--
--     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@
--
--
--     * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
--
--
-- * 'vpcConfig' - Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for your dominant language detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
mkStartDominantLanguageDetectionJob ::
  -- | 'inputDataConfig'
  InputDataConfig ->
  -- | 'outputDataConfig'
  OutputDataConfig ->
  -- | 'dataAccessRoleARN'
  Lude.Text ->
  StartDominantLanguageDetectionJob
mkStartDominantLanguageDetectionJob
  pInputDataConfig_
  pOutputDataConfig_
  pDataAccessRoleARN_ =
    StartDominantLanguageDetectionJob'
      { jobName = Lude.Nothing,
        vpcConfig = Lude.Nothing,
        volumeKMSKeyId = Lude.Nothing,
        clientRequestToken = Lude.Nothing,
        inputDataConfig = pInputDataConfig_,
        outputDataConfig = pOutputDataConfig_,
        dataAccessRoleARN = pDataAccessRoleARN_
      }

-- | An identifier for the job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdldjJobName :: Lens.Lens' StartDominantLanguageDetectionJob (Lude.Maybe Lude.Text)
sdldjJobName = Lens.lens (jobName :: StartDominantLanguageDetectionJob -> Lude.Maybe Lude.Text) (\s a -> s {jobName = a} :: StartDominantLanguageDetectionJob)
{-# DEPRECATED sdldjJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for your dominant language detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdldjVPCConfig :: Lens.Lens' StartDominantLanguageDetectionJob (Lude.Maybe VPCConfig)
sdldjVPCConfig = Lens.lens (vpcConfig :: StartDominantLanguageDetectionJob -> Lude.Maybe VPCConfig) (\s a -> s {vpcConfig = a} :: StartDominantLanguageDetectionJob)
{-# DEPRECATED sdldjVPCConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

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
sdldjVolumeKMSKeyId :: Lens.Lens' StartDominantLanguageDetectionJob (Lude.Maybe Lude.Text)
sdldjVolumeKMSKeyId = Lens.lens (volumeKMSKeyId :: StartDominantLanguageDetectionJob -> Lude.Maybe Lude.Text) (\s a -> s {volumeKMSKeyId = a} :: StartDominantLanguageDetectionJob)
{-# DEPRECATED sdldjVolumeKMSKeyId "Use generic-lens or generic-optics with 'volumeKMSKeyId' instead." #-}

-- | A unique identifier for the request. If you do not set the client request token, Amazon Comprehend generates one.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdldjClientRequestToken :: Lens.Lens' StartDominantLanguageDetectionJob (Lude.Maybe Lude.Text)
sdldjClientRequestToken = Lens.lens (clientRequestToken :: StartDominantLanguageDetectionJob -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: StartDominantLanguageDetectionJob)
{-# DEPRECATED sdldjClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | Specifies the format and location of the input data for the job.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdldjInputDataConfig :: Lens.Lens' StartDominantLanguageDetectionJob InputDataConfig
sdldjInputDataConfig = Lens.lens (inputDataConfig :: StartDominantLanguageDetectionJob -> InputDataConfig) (\s a -> s {inputDataConfig = a} :: StartDominantLanguageDetectionJob)
{-# DEPRECATED sdldjInputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead." #-}

-- | Specifies where to send the output files.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdldjOutputDataConfig :: Lens.Lens' StartDominantLanguageDetectionJob OutputDataConfig
sdldjOutputDataConfig = Lens.lens (outputDataConfig :: StartDominantLanguageDetectionJob -> OutputDataConfig) (\s a -> s {outputDataConfig = a} :: StartDominantLanguageDetectionJob)
{-# DEPRECATED sdldjOutputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data. For more information, see <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions> .
--
-- /Note:/ Consider using 'dataAccessRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdldjDataAccessRoleARN :: Lens.Lens' StartDominantLanguageDetectionJob Lude.Text
sdldjDataAccessRoleARN = Lens.lens (dataAccessRoleARN :: StartDominantLanguageDetectionJob -> Lude.Text) (\s a -> s {dataAccessRoleARN = a} :: StartDominantLanguageDetectionJob)
{-# DEPRECATED sdldjDataAccessRoleARN "Use generic-lens or generic-optics with 'dataAccessRoleARN' instead." #-}

instance Lude.AWSRequest StartDominantLanguageDetectionJob where
  type
    Rs StartDominantLanguageDetectionJob =
      StartDominantLanguageDetectionJobResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartDominantLanguageDetectionJobResponse'
            Lude.<$> (x Lude..?> "JobId")
            Lude.<*> (x Lude..?> "JobStatus")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartDominantLanguageDetectionJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Comprehend_20171127.StartDominantLanguageDetectionJob" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartDominantLanguageDetectionJob where
  toJSON StartDominantLanguageDetectionJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("JobName" Lude..=) Lude.<$> jobName,
            ("VpcConfig" Lude..=) Lude.<$> vpcConfig,
            ("VolumeKmsKeyId" Lude..=) Lude.<$> volumeKMSKeyId,
            ("ClientRequestToken" Lude..=) Lude.<$> clientRequestToken,
            Lude.Just ("InputDataConfig" Lude..= inputDataConfig),
            Lude.Just ("OutputDataConfig" Lude..= outputDataConfig),
            Lude.Just ("DataAccessRoleArn" Lude..= dataAccessRoleARN)
          ]
      )

instance Lude.ToPath StartDominantLanguageDetectionJob where
  toPath = Lude.const "/"

instance Lude.ToQuery StartDominantLanguageDetectionJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartDominantLanguageDetectionJobResponse' smart constructor.
data StartDominantLanguageDetectionJobResponse = StartDominantLanguageDetectionJobResponse'
  { jobId ::
      Lude.Maybe
        Lude.Text,
    jobStatus ::
      Lude.Maybe
        JobStatus,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartDominantLanguageDetectionJobResponse' with the minimum fields required to make a request.
--
-- * 'jobId' - The identifier generated for the job. To get the status of a job, use this identifier with the operation.
-- * 'jobStatus' - The status of the job.
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
-- * 'responseStatus' - The response status code.
mkStartDominantLanguageDetectionJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartDominantLanguageDetectionJobResponse
mkStartDominantLanguageDetectionJobResponse pResponseStatus_ =
  StartDominantLanguageDetectionJobResponse'
    { jobId = Lude.Nothing,
      jobStatus = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier generated for the job. To get the status of a job, use this identifier with the operation.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdldjrsJobId :: Lens.Lens' StartDominantLanguageDetectionJobResponse (Lude.Maybe Lude.Text)
sdldjrsJobId = Lens.lens (jobId :: StartDominantLanguageDetectionJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: StartDominantLanguageDetectionJobResponse)
{-# DEPRECATED sdldjrsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

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
sdldjrsJobStatus :: Lens.Lens' StartDominantLanguageDetectionJobResponse (Lude.Maybe JobStatus)
sdldjrsJobStatus = Lens.lens (jobStatus :: StartDominantLanguageDetectionJobResponse -> Lude.Maybe JobStatus) (\s a -> s {jobStatus = a} :: StartDominantLanguageDetectionJobResponse)
{-# DEPRECATED sdldjrsJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdldjrsResponseStatus :: Lens.Lens' StartDominantLanguageDetectionJobResponse Lude.Int
sdldjrsResponseStatus = Lens.lens (responseStatus :: StartDominantLanguageDetectionJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartDominantLanguageDetectionJobResponse)
{-# DEPRECATED sdldjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
