{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    StartKeyPhrasesDetectionJob (..),
    mkStartKeyPhrasesDetectionJob,

    -- ** Request lenses
    skpdjJobName,
    skpdjVPCConfig,
    skpdjVolumeKMSKeyId,
    skpdjClientRequestToken,
    skpdjInputDataConfig,
    skpdjOutputDataConfig,
    skpdjDataAccessRoleARN,
    skpdjLanguageCode,

    -- * Destructuring the response
    StartKeyPhrasesDetectionJobResponse (..),
    mkStartKeyPhrasesDetectionJobResponse,

    -- ** Response lenses
    skpdjkrsJobId,
    skpdjkrsJobStatus,
    skpdjkrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartKeyPhrasesDetectionJob' smart constructor.
data StartKeyPhrasesDetectionJob = StartKeyPhrasesDetectionJob'
  { jobName ::
      Lude.Maybe Lude.Text,
    vpcConfig :: Lude.Maybe VPCConfig,
    volumeKMSKeyId ::
      Lude.Maybe Lude.Text,
    clientRequestToken ::
      Lude.Maybe Lude.Text,
    inputDataConfig :: InputDataConfig,
    outputDataConfig ::
      OutputDataConfig,
    dataAccessRoleARN :: Lude.Text,
    languageCode :: LanguageCode
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartKeyPhrasesDetectionJob' with the minimum fields required to make a request.
--
-- * 'clientRequestToken' - A unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
-- * 'dataAccessRoleARN' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data. For more information, see <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions> .
-- * 'inputDataConfig' - Specifies the format and location of the input data for the job.
-- * 'jobName' - The identifier of the job.
-- * 'languageCode' - The language of the input documents. You can specify any of the primary languages supported by Amazon Comprehend. All documents must be in the same language.
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
-- * 'vpcConfig' - Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for your key phrases detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
mkStartKeyPhrasesDetectionJob ::
  -- | 'inputDataConfig'
  InputDataConfig ->
  -- | 'outputDataConfig'
  OutputDataConfig ->
  -- | 'dataAccessRoleARN'
  Lude.Text ->
  -- | 'languageCode'
  LanguageCode ->
  StartKeyPhrasesDetectionJob
mkStartKeyPhrasesDetectionJob
  pInputDataConfig_
  pOutputDataConfig_
  pDataAccessRoleARN_
  pLanguageCode_ =
    StartKeyPhrasesDetectionJob'
      { jobName = Lude.Nothing,
        vpcConfig = Lude.Nothing,
        volumeKMSKeyId = Lude.Nothing,
        clientRequestToken = Lude.Nothing,
        inputDataConfig = pInputDataConfig_,
        outputDataConfig = pOutputDataConfig_,
        dataAccessRoleARN = pDataAccessRoleARN_,
        languageCode = pLanguageCode_
      }

-- | The identifier of the job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpdjJobName :: Lens.Lens' StartKeyPhrasesDetectionJob (Lude.Maybe Lude.Text)
skpdjJobName = Lens.lens (jobName :: StartKeyPhrasesDetectionJob -> Lude.Maybe Lude.Text) (\s a -> s {jobName = a} :: StartKeyPhrasesDetectionJob)
{-# DEPRECATED skpdjJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for your key phrases detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpdjVPCConfig :: Lens.Lens' StartKeyPhrasesDetectionJob (Lude.Maybe VPCConfig)
skpdjVPCConfig = Lens.lens (vpcConfig :: StartKeyPhrasesDetectionJob -> Lude.Maybe VPCConfig) (\s a -> s {vpcConfig = a} :: StartKeyPhrasesDetectionJob)
{-# DEPRECATED skpdjVPCConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

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
skpdjVolumeKMSKeyId :: Lens.Lens' StartKeyPhrasesDetectionJob (Lude.Maybe Lude.Text)
skpdjVolumeKMSKeyId = Lens.lens (volumeKMSKeyId :: StartKeyPhrasesDetectionJob -> Lude.Maybe Lude.Text) (\s a -> s {volumeKMSKeyId = a} :: StartKeyPhrasesDetectionJob)
{-# DEPRECATED skpdjVolumeKMSKeyId "Use generic-lens or generic-optics with 'volumeKMSKeyId' instead." #-}

-- | A unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpdjClientRequestToken :: Lens.Lens' StartKeyPhrasesDetectionJob (Lude.Maybe Lude.Text)
skpdjClientRequestToken = Lens.lens (clientRequestToken :: StartKeyPhrasesDetectionJob -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: StartKeyPhrasesDetectionJob)
{-# DEPRECATED skpdjClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | Specifies the format and location of the input data for the job.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpdjInputDataConfig :: Lens.Lens' StartKeyPhrasesDetectionJob InputDataConfig
skpdjInputDataConfig = Lens.lens (inputDataConfig :: StartKeyPhrasesDetectionJob -> InputDataConfig) (\s a -> s {inputDataConfig = a} :: StartKeyPhrasesDetectionJob)
{-# DEPRECATED skpdjInputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead." #-}

-- | Specifies where to send the output files.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpdjOutputDataConfig :: Lens.Lens' StartKeyPhrasesDetectionJob OutputDataConfig
skpdjOutputDataConfig = Lens.lens (outputDataConfig :: StartKeyPhrasesDetectionJob -> OutputDataConfig) (\s a -> s {outputDataConfig = a} :: StartKeyPhrasesDetectionJob)
{-# DEPRECATED skpdjOutputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data. For more information, see <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions> .
--
-- /Note:/ Consider using 'dataAccessRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpdjDataAccessRoleARN :: Lens.Lens' StartKeyPhrasesDetectionJob Lude.Text
skpdjDataAccessRoleARN = Lens.lens (dataAccessRoleARN :: StartKeyPhrasesDetectionJob -> Lude.Text) (\s a -> s {dataAccessRoleARN = a} :: StartKeyPhrasesDetectionJob)
{-# DEPRECATED skpdjDataAccessRoleARN "Use generic-lens or generic-optics with 'dataAccessRoleARN' instead." #-}

-- | The language of the input documents. You can specify any of the primary languages supported by Amazon Comprehend. All documents must be in the same language.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpdjLanguageCode :: Lens.Lens' StartKeyPhrasesDetectionJob LanguageCode
skpdjLanguageCode = Lens.lens (languageCode :: StartKeyPhrasesDetectionJob -> LanguageCode) (\s a -> s {languageCode = a} :: StartKeyPhrasesDetectionJob)
{-# DEPRECATED skpdjLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

instance Lude.AWSRequest StartKeyPhrasesDetectionJob where
  type
    Rs StartKeyPhrasesDetectionJob =
      StartKeyPhrasesDetectionJobResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartKeyPhrasesDetectionJobResponse'
            Lude.<$> (x Lude..?> "JobId")
            Lude.<*> (x Lude..?> "JobStatus")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartKeyPhrasesDetectionJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Comprehend_20171127.StartKeyPhrasesDetectionJob" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartKeyPhrasesDetectionJob where
  toJSON StartKeyPhrasesDetectionJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("JobName" Lude..=) Lude.<$> jobName,
            ("VpcConfig" Lude..=) Lude.<$> vpcConfig,
            ("VolumeKmsKeyId" Lude..=) Lude.<$> volumeKMSKeyId,
            ("ClientRequestToken" Lude..=) Lude.<$> clientRequestToken,
            Lude.Just ("InputDataConfig" Lude..= inputDataConfig),
            Lude.Just ("OutputDataConfig" Lude..= outputDataConfig),
            Lude.Just ("DataAccessRoleArn" Lude..= dataAccessRoleARN),
            Lude.Just ("LanguageCode" Lude..= languageCode)
          ]
      )

instance Lude.ToPath StartKeyPhrasesDetectionJob where
  toPath = Lude.const "/"

instance Lude.ToQuery StartKeyPhrasesDetectionJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartKeyPhrasesDetectionJobResponse' smart constructor.
data StartKeyPhrasesDetectionJobResponse = StartKeyPhrasesDetectionJobResponse'
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

-- | Creates a value of 'StartKeyPhrasesDetectionJobResponse' with the minimum fields required to make a request.
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
mkStartKeyPhrasesDetectionJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartKeyPhrasesDetectionJobResponse
mkStartKeyPhrasesDetectionJobResponse pResponseStatus_ =
  StartKeyPhrasesDetectionJobResponse'
    { jobId = Lude.Nothing,
      jobStatus = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier generated for the job. To get the status of a job, use this identifier with the operation.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpdjkrsJobId :: Lens.Lens' StartKeyPhrasesDetectionJobResponse (Lude.Maybe Lude.Text)
skpdjkrsJobId = Lens.lens (jobId :: StartKeyPhrasesDetectionJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: StartKeyPhrasesDetectionJobResponse)
{-# DEPRECATED skpdjkrsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

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
skpdjkrsJobStatus :: Lens.Lens' StartKeyPhrasesDetectionJobResponse (Lude.Maybe JobStatus)
skpdjkrsJobStatus = Lens.lens (jobStatus :: StartKeyPhrasesDetectionJobResponse -> Lude.Maybe JobStatus) (\s a -> s {jobStatus = a} :: StartKeyPhrasesDetectionJobResponse)
{-# DEPRECATED skpdjkrsJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpdjkrsResponseStatus :: Lens.Lens' StartKeyPhrasesDetectionJobResponse Lude.Int
skpdjkrsResponseStatus = Lens.lens (responseStatus :: StartKeyPhrasesDetectionJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartKeyPhrasesDetectionJobResponse)
{-# DEPRECATED skpdjkrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
