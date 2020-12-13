{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    StartTopicsDetectionJob (..),
    mkStartTopicsDetectionJob,

    -- ** Request lenses
    stdjJobName,
    stdjInputDataConfig,
    stdjVPCConfig,
    stdjVolumeKMSKeyId,
    stdjOutputDataConfig,
    stdjDataAccessRoleARN,
    stdjNumberOfTopics,
    stdjClientRequestToken,

    -- * Destructuring the response
    StartTopicsDetectionJobResponse (..),
    mkStartTopicsDetectionJobResponse,

    -- ** Response lenses
    stdjrsJobId,
    stdjrsJobStatus,
    stdjrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartTopicsDetectionJob' smart constructor.
data StartTopicsDetectionJob = StartTopicsDetectionJob'
  { -- | The identifier of the job.
    jobName :: Lude.Maybe Lude.Text,
    -- | Specifies the format and location of the input data for the job.
    inputDataConfig :: InputDataConfig,
    -- | Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for your topic detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
    vpcConfig :: Lude.Maybe VPCConfig,
    -- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:
    --
    --
    --     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@
    --
    --
    --     * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
    volumeKMSKeyId :: Lude.Maybe Lude.Text,
    -- | Specifies where to send the output files. The output is a compressed archive with two files, @topic-terms.csv@ that lists the terms associated with each topic, and @doc-topics.csv@ that lists the documents associated with each topic
    outputDataConfig :: OutputDataConfig,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data. For more information, see <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions> .
    dataAccessRoleARN :: Lude.Text,
    -- | The number of topics to detect.
    numberOfTopics :: Lude.Maybe Lude.Natural,
    -- | A unique identifier for the request. If you do not set the client request token, Amazon Comprehend generates one.
    clientRequestToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartTopicsDetectionJob' with the minimum fields required to make a request.
--
-- * 'jobName' - The identifier of the job.
-- * 'inputDataConfig' - Specifies the format and location of the input data for the job.
-- * 'vpcConfig' - Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for your topic detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
-- * 'volumeKMSKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:
--
--
--     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@
--
--
--     * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
--
--
-- * 'outputDataConfig' - Specifies where to send the output files. The output is a compressed archive with two files, @topic-terms.csv@ that lists the terms associated with each topic, and @doc-topics.csv@ that lists the documents associated with each topic
-- * 'dataAccessRoleARN' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data. For more information, see <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions> .
-- * 'numberOfTopics' - The number of topics to detect.
-- * 'clientRequestToken' - A unique identifier for the request. If you do not set the client request token, Amazon Comprehend generates one.
mkStartTopicsDetectionJob ::
  -- | 'inputDataConfig'
  InputDataConfig ->
  -- | 'outputDataConfig'
  OutputDataConfig ->
  -- | 'dataAccessRoleARN'
  Lude.Text ->
  StartTopicsDetectionJob
mkStartTopicsDetectionJob
  pInputDataConfig_
  pOutputDataConfig_
  pDataAccessRoleARN_ =
    StartTopicsDetectionJob'
      { jobName = Lude.Nothing,
        inputDataConfig = pInputDataConfig_,
        vpcConfig = Lude.Nothing,
        volumeKMSKeyId = Lude.Nothing,
        outputDataConfig = pOutputDataConfig_,
        dataAccessRoleARN = pDataAccessRoleARN_,
        numberOfTopics = Lude.Nothing,
        clientRequestToken = Lude.Nothing
      }

-- | The identifier of the job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdjJobName :: Lens.Lens' StartTopicsDetectionJob (Lude.Maybe Lude.Text)
stdjJobName = Lens.lens (jobName :: StartTopicsDetectionJob -> Lude.Maybe Lude.Text) (\s a -> s {jobName = a} :: StartTopicsDetectionJob)
{-# DEPRECATED stdjJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | Specifies the format and location of the input data for the job.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdjInputDataConfig :: Lens.Lens' StartTopicsDetectionJob InputDataConfig
stdjInputDataConfig = Lens.lens (inputDataConfig :: StartTopicsDetectionJob -> InputDataConfig) (\s a -> s {inputDataConfig = a} :: StartTopicsDetectionJob)
{-# DEPRECATED stdjInputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead." #-}

-- | Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for your topic detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdjVPCConfig :: Lens.Lens' StartTopicsDetectionJob (Lude.Maybe VPCConfig)
stdjVPCConfig = Lens.lens (vpcConfig :: StartTopicsDetectionJob -> Lude.Maybe VPCConfig) (\s a -> s {vpcConfig = a} :: StartTopicsDetectionJob)
{-# DEPRECATED stdjVPCConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

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
stdjVolumeKMSKeyId :: Lens.Lens' StartTopicsDetectionJob (Lude.Maybe Lude.Text)
stdjVolumeKMSKeyId = Lens.lens (volumeKMSKeyId :: StartTopicsDetectionJob -> Lude.Maybe Lude.Text) (\s a -> s {volumeKMSKeyId = a} :: StartTopicsDetectionJob)
{-# DEPRECATED stdjVolumeKMSKeyId "Use generic-lens or generic-optics with 'volumeKMSKeyId' instead." #-}

-- | Specifies where to send the output files. The output is a compressed archive with two files, @topic-terms.csv@ that lists the terms associated with each topic, and @doc-topics.csv@ that lists the documents associated with each topic
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdjOutputDataConfig :: Lens.Lens' StartTopicsDetectionJob OutputDataConfig
stdjOutputDataConfig = Lens.lens (outputDataConfig :: StartTopicsDetectionJob -> OutputDataConfig) (\s a -> s {outputDataConfig = a} :: StartTopicsDetectionJob)
{-# DEPRECATED stdjOutputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data. For more information, see <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions> .
--
-- /Note:/ Consider using 'dataAccessRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdjDataAccessRoleARN :: Lens.Lens' StartTopicsDetectionJob Lude.Text
stdjDataAccessRoleARN = Lens.lens (dataAccessRoleARN :: StartTopicsDetectionJob -> Lude.Text) (\s a -> s {dataAccessRoleARN = a} :: StartTopicsDetectionJob)
{-# DEPRECATED stdjDataAccessRoleARN "Use generic-lens or generic-optics with 'dataAccessRoleARN' instead." #-}

-- | The number of topics to detect.
--
-- /Note:/ Consider using 'numberOfTopics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdjNumberOfTopics :: Lens.Lens' StartTopicsDetectionJob (Lude.Maybe Lude.Natural)
stdjNumberOfTopics = Lens.lens (numberOfTopics :: StartTopicsDetectionJob -> Lude.Maybe Lude.Natural) (\s a -> s {numberOfTopics = a} :: StartTopicsDetectionJob)
{-# DEPRECATED stdjNumberOfTopics "Use generic-lens or generic-optics with 'numberOfTopics' instead." #-}

-- | A unique identifier for the request. If you do not set the client request token, Amazon Comprehend generates one.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdjClientRequestToken :: Lens.Lens' StartTopicsDetectionJob (Lude.Maybe Lude.Text)
stdjClientRequestToken = Lens.lens (clientRequestToken :: StartTopicsDetectionJob -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: StartTopicsDetectionJob)
{-# DEPRECATED stdjClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

instance Lude.AWSRequest StartTopicsDetectionJob where
  type Rs StartTopicsDetectionJob = StartTopicsDetectionJobResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartTopicsDetectionJobResponse'
            Lude.<$> (x Lude..?> "JobId")
            Lude.<*> (x Lude..?> "JobStatus")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartTopicsDetectionJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Comprehend_20171127.StartTopicsDetectionJob" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartTopicsDetectionJob where
  toJSON StartTopicsDetectionJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("JobName" Lude..=) Lude.<$> jobName,
            Lude.Just ("InputDataConfig" Lude..= inputDataConfig),
            ("VpcConfig" Lude..=) Lude.<$> vpcConfig,
            ("VolumeKmsKeyId" Lude..=) Lude.<$> volumeKMSKeyId,
            Lude.Just ("OutputDataConfig" Lude..= outputDataConfig),
            Lude.Just ("DataAccessRoleArn" Lude..= dataAccessRoleARN),
            ("NumberOfTopics" Lude..=) Lude.<$> numberOfTopics,
            ("ClientRequestToken" Lude..=) Lude.<$> clientRequestToken
          ]
      )

instance Lude.ToPath StartTopicsDetectionJob where
  toPath = Lude.const "/"

instance Lude.ToQuery StartTopicsDetectionJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartTopicsDetectionJobResponse' smart constructor.
data StartTopicsDetectionJobResponse = StartTopicsDetectionJobResponse'
  { -- | The identifier generated for the job. To get the status of the job, use this identifier with the @DescribeTopicDetectionJob@ operation.
    jobId :: Lude.Maybe Lude.Text,
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
    jobStatus :: Lude.Maybe JobStatus,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartTopicsDetectionJobResponse' with the minimum fields required to make a request.
--
-- * 'jobId' - The identifier generated for the job. To get the status of the job, use this identifier with the @DescribeTopicDetectionJob@ operation.
-- * 'jobStatus' - The status of the job:
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
-- * 'responseStatus' - The response status code.
mkStartTopicsDetectionJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartTopicsDetectionJobResponse
mkStartTopicsDetectionJobResponse pResponseStatus_ =
  StartTopicsDetectionJobResponse'
    { jobId = Lude.Nothing,
      jobStatus = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier generated for the job. To get the status of the job, use this identifier with the @DescribeTopicDetectionJob@ operation.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdjrsJobId :: Lens.Lens' StartTopicsDetectionJobResponse (Lude.Maybe Lude.Text)
stdjrsJobId = Lens.lens (jobId :: StartTopicsDetectionJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: StartTopicsDetectionJobResponse)
{-# DEPRECATED stdjrsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

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
stdjrsJobStatus :: Lens.Lens' StartTopicsDetectionJobResponse (Lude.Maybe JobStatus)
stdjrsJobStatus = Lens.lens (jobStatus :: StartTopicsDetectionJobResponse -> Lude.Maybe JobStatus) (\s a -> s {jobStatus = a} :: StartTopicsDetectionJobResponse)
{-# DEPRECATED stdjrsJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdjrsResponseStatus :: Lens.Lens' StartTopicsDetectionJobResponse Lude.Int
stdjrsResponseStatus = Lens.lens (responseStatus :: StartTopicsDetectionJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartTopicsDetectionJobResponse)
{-# DEPRECATED stdjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
