{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    StartEntitiesDetectionJob (..),
    mkStartEntitiesDetectionJob,

    -- ** Request lenses
    sedjLanguageCode,
    sedjEntityRecognizerARN,
    sedjJobName,
    sedjInputDataConfig,
    sedjVPCConfig,
    sedjVolumeKMSKeyId,
    sedjOutputDataConfig,
    sedjDataAccessRoleARN,
    sedjClientRequestToken,

    -- * Destructuring the response
    StartEntitiesDetectionJobResponse (..),
    mkStartEntitiesDetectionJobResponse,

    -- ** Response lenses
    sedjgrsJobId,
    sedjgrsJobStatus,
    sedjgrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartEntitiesDetectionJob' smart constructor.
data StartEntitiesDetectionJob = StartEntitiesDetectionJob'
  { -- | The language of the input documents. All documents must be in the same language. You can specify any of the languages supported by Amazon Comprehend. If custom entities recognition is used, this parameter is ignored and the language used for training the model is used instead.
    languageCode :: LanguageCode,
    -- | The Amazon Resource Name (ARN) that identifies the specific entity recognizer to be used by the @StartEntitiesDetectionJob@ . This ARN is optional and is only used for a custom entity recognition job.
    entityRecognizerARN :: Lude.Maybe Lude.Text,
    -- | The identifier of the job.
    jobName :: Lude.Maybe Lude.Text,
    -- | Specifies the format and location of the input data for the job.
    inputDataConfig :: InputDataConfig,
    -- | Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for your entity detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
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
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data. For more information, see <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions> .
    dataAccessRoleARN :: Lude.Text,
    -- | A unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
    clientRequestToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartEntitiesDetectionJob' with the minimum fields required to make a request.
--
-- * 'languageCode' - The language of the input documents. All documents must be in the same language. You can specify any of the languages supported by Amazon Comprehend. If custom entities recognition is used, this parameter is ignored and the language used for training the model is used instead.
-- * 'entityRecognizerARN' - The Amazon Resource Name (ARN) that identifies the specific entity recognizer to be used by the @StartEntitiesDetectionJob@ . This ARN is optional and is only used for a custom entity recognition job.
-- * 'jobName' - The identifier of the job.
-- * 'inputDataConfig' - Specifies the format and location of the input data for the job.
-- * 'vpcConfig' - Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for your entity detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
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
-- * 'dataAccessRoleARN' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data. For more information, see <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions> .
-- * 'clientRequestToken' - A unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
mkStartEntitiesDetectionJob ::
  -- | 'languageCode'
  LanguageCode ->
  -- | 'inputDataConfig'
  InputDataConfig ->
  -- | 'outputDataConfig'
  OutputDataConfig ->
  -- | 'dataAccessRoleARN'
  Lude.Text ->
  StartEntitiesDetectionJob
mkStartEntitiesDetectionJob
  pLanguageCode_
  pInputDataConfig_
  pOutputDataConfig_
  pDataAccessRoleARN_ =
    StartEntitiesDetectionJob'
      { languageCode = pLanguageCode_,
        entityRecognizerARN = Lude.Nothing,
        jobName = Lude.Nothing,
        inputDataConfig = pInputDataConfig_,
        vpcConfig = Lude.Nothing,
        volumeKMSKeyId = Lude.Nothing,
        outputDataConfig = pOutputDataConfig_,
        dataAccessRoleARN = pDataAccessRoleARN_,
        clientRequestToken = Lude.Nothing
      }

-- | The language of the input documents. All documents must be in the same language. You can specify any of the languages supported by Amazon Comprehend. If custom entities recognition is used, this parameter is ignored and the language used for training the model is used instead.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sedjLanguageCode :: Lens.Lens' StartEntitiesDetectionJob LanguageCode
sedjLanguageCode = Lens.lens (languageCode :: StartEntitiesDetectionJob -> LanguageCode) (\s a -> s {languageCode = a} :: StartEntitiesDetectionJob)
{-# DEPRECATED sedjLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | The Amazon Resource Name (ARN) that identifies the specific entity recognizer to be used by the @StartEntitiesDetectionJob@ . This ARN is optional and is only used for a custom entity recognition job.
--
-- /Note:/ Consider using 'entityRecognizerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sedjEntityRecognizerARN :: Lens.Lens' StartEntitiesDetectionJob (Lude.Maybe Lude.Text)
sedjEntityRecognizerARN = Lens.lens (entityRecognizerARN :: StartEntitiesDetectionJob -> Lude.Maybe Lude.Text) (\s a -> s {entityRecognizerARN = a} :: StartEntitiesDetectionJob)
{-# DEPRECATED sedjEntityRecognizerARN "Use generic-lens or generic-optics with 'entityRecognizerARN' instead." #-}

-- | The identifier of the job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sedjJobName :: Lens.Lens' StartEntitiesDetectionJob (Lude.Maybe Lude.Text)
sedjJobName = Lens.lens (jobName :: StartEntitiesDetectionJob -> Lude.Maybe Lude.Text) (\s a -> s {jobName = a} :: StartEntitiesDetectionJob)
{-# DEPRECATED sedjJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | Specifies the format and location of the input data for the job.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sedjInputDataConfig :: Lens.Lens' StartEntitiesDetectionJob InputDataConfig
sedjInputDataConfig = Lens.lens (inputDataConfig :: StartEntitiesDetectionJob -> InputDataConfig) (\s a -> s {inputDataConfig = a} :: StartEntitiesDetectionJob)
{-# DEPRECATED sedjInputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead." #-}

-- | Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for your entity detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sedjVPCConfig :: Lens.Lens' StartEntitiesDetectionJob (Lude.Maybe VPCConfig)
sedjVPCConfig = Lens.lens (vpcConfig :: StartEntitiesDetectionJob -> Lude.Maybe VPCConfig) (\s a -> s {vpcConfig = a} :: StartEntitiesDetectionJob)
{-# DEPRECATED sedjVPCConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

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
sedjVolumeKMSKeyId :: Lens.Lens' StartEntitiesDetectionJob (Lude.Maybe Lude.Text)
sedjVolumeKMSKeyId = Lens.lens (volumeKMSKeyId :: StartEntitiesDetectionJob -> Lude.Maybe Lude.Text) (\s a -> s {volumeKMSKeyId = a} :: StartEntitiesDetectionJob)
{-# DEPRECATED sedjVolumeKMSKeyId "Use generic-lens or generic-optics with 'volumeKMSKeyId' instead." #-}

-- | Specifies where to send the output files.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sedjOutputDataConfig :: Lens.Lens' StartEntitiesDetectionJob OutputDataConfig
sedjOutputDataConfig = Lens.lens (outputDataConfig :: StartEntitiesDetectionJob -> OutputDataConfig) (\s a -> s {outputDataConfig = a} :: StartEntitiesDetectionJob)
{-# DEPRECATED sedjOutputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data. For more information, see <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions> .
--
-- /Note:/ Consider using 'dataAccessRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sedjDataAccessRoleARN :: Lens.Lens' StartEntitiesDetectionJob Lude.Text
sedjDataAccessRoleARN = Lens.lens (dataAccessRoleARN :: StartEntitiesDetectionJob -> Lude.Text) (\s a -> s {dataAccessRoleARN = a} :: StartEntitiesDetectionJob)
{-# DEPRECATED sedjDataAccessRoleARN "Use generic-lens or generic-optics with 'dataAccessRoleARN' instead." #-}

-- | A unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sedjClientRequestToken :: Lens.Lens' StartEntitiesDetectionJob (Lude.Maybe Lude.Text)
sedjClientRequestToken = Lens.lens (clientRequestToken :: StartEntitiesDetectionJob -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: StartEntitiesDetectionJob)
{-# DEPRECATED sedjClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

instance Lude.AWSRequest StartEntitiesDetectionJob where
  type
    Rs StartEntitiesDetectionJob =
      StartEntitiesDetectionJobResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartEntitiesDetectionJobResponse'
            Lude.<$> (x Lude..?> "JobId")
            Lude.<*> (x Lude..?> "JobStatus")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartEntitiesDetectionJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Comprehend_20171127.StartEntitiesDetectionJob" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartEntitiesDetectionJob where
  toJSON StartEntitiesDetectionJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("LanguageCode" Lude..= languageCode),
            ("EntityRecognizerArn" Lude..=) Lude.<$> entityRecognizerARN,
            ("JobName" Lude..=) Lude.<$> jobName,
            Lude.Just ("InputDataConfig" Lude..= inputDataConfig),
            ("VpcConfig" Lude..=) Lude.<$> vpcConfig,
            ("VolumeKmsKeyId" Lude..=) Lude.<$> volumeKMSKeyId,
            Lude.Just ("OutputDataConfig" Lude..= outputDataConfig),
            Lude.Just ("DataAccessRoleArn" Lude..= dataAccessRoleARN),
            ("ClientRequestToken" Lude..=) Lude.<$> clientRequestToken
          ]
      )

instance Lude.ToPath StartEntitiesDetectionJob where
  toPath = Lude.const "/"

instance Lude.ToQuery StartEntitiesDetectionJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartEntitiesDetectionJobResponse' smart constructor.
data StartEntitiesDetectionJobResponse = StartEntitiesDetectionJobResponse'
  { -- | The identifier generated for the job. To get the status of job, use this identifier with the operation.
    jobId :: Lude.Maybe Lude.Text,
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
    jobStatus :: Lude.Maybe JobStatus,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartEntitiesDetectionJobResponse' with the minimum fields required to make a request.
--
-- * 'jobId' - The identifier generated for the job. To get the status of job, use this identifier with the operation.
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
--     * STOP_REQUESTED - Amazon Comprehend has received a stop request for the job and is processing the request.
--
--
--     * STOPPED - The job was successfully stopped without completing.
--
--
-- * 'responseStatus' - The response status code.
mkStartEntitiesDetectionJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartEntitiesDetectionJobResponse
mkStartEntitiesDetectionJobResponse pResponseStatus_ =
  StartEntitiesDetectionJobResponse'
    { jobId = Lude.Nothing,
      jobStatus = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier generated for the job. To get the status of job, use this identifier with the operation.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sedjgrsJobId :: Lens.Lens' StartEntitiesDetectionJobResponse (Lude.Maybe Lude.Text)
sedjgrsJobId = Lens.lens (jobId :: StartEntitiesDetectionJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: StartEntitiesDetectionJobResponse)
{-# DEPRECATED sedjgrsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

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
sedjgrsJobStatus :: Lens.Lens' StartEntitiesDetectionJobResponse (Lude.Maybe JobStatus)
sedjgrsJobStatus = Lens.lens (jobStatus :: StartEntitiesDetectionJobResponse -> Lude.Maybe JobStatus) (\s a -> s {jobStatus = a} :: StartEntitiesDetectionJobResponse)
{-# DEPRECATED sedjgrsJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sedjgrsResponseStatus :: Lens.Lens' StartEntitiesDetectionJobResponse Lude.Int
sedjgrsResponseStatus = Lens.lens (responseStatus :: StartEntitiesDetectionJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartEntitiesDetectionJobResponse)
{-# DEPRECATED sedjgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
