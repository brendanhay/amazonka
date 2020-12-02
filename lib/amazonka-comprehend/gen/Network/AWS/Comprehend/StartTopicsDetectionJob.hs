{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
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
  ( -- * Creating a Request
    startTopicsDetectionJob,
    StartTopicsDetectionJob,

    -- * Request Lenses
    stdjJobName,
    stdjVPCConfig,
    stdjVolumeKMSKeyId,
    stdjNumberOfTopics,
    stdjClientRequestToken,
    stdjInputDataConfig,
    stdjOutputDataConfig,
    stdjDataAccessRoleARN,

    -- * Destructuring the Response
    startTopicsDetectionJobResponse,
    StartTopicsDetectionJobResponse,

    -- * Response Lenses
    stdjrsJobId,
    stdjrsJobStatus,
    stdjrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startTopicsDetectionJob' smart constructor.
data StartTopicsDetectionJob = StartTopicsDetectionJob'
  { _stdjJobName ::
      !(Maybe Text),
    _stdjVPCConfig :: !(Maybe VPCConfig),
    _stdjVolumeKMSKeyId :: !(Maybe Text),
    _stdjNumberOfTopics :: !(Maybe Nat),
    _stdjClientRequestToken :: !(Maybe Text),
    _stdjInputDataConfig :: !InputDataConfig,
    _stdjOutputDataConfig :: !OutputDataConfig,
    _stdjDataAccessRoleARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartTopicsDetectionJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stdjJobName' - The identifier of the job.
--
-- * 'stdjVPCConfig' - Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for your topic detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
--
-- * 'stdjVolumeKMSKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@      * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
--
-- * 'stdjNumberOfTopics' - The number of topics to detect.
--
-- * 'stdjClientRequestToken' - A unique identifier for the request. If you do not set the client request token, Amazon Comprehend generates one.
--
-- * 'stdjInputDataConfig' - Specifies the format and location of the input data for the job.
--
-- * 'stdjOutputDataConfig' - Specifies where to send the output files. The output is a compressed archive with two files, @topic-terms.csv@ that lists the terms associated with each topic, and @doc-topics.csv@ that lists the documents associated with each topic
--
-- * 'stdjDataAccessRoleARN' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data. For more information, see <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions> .
startTopicsDetectionJob ::
  -- | 'stdjInputDataConfig'
  InputDataConfig ->
  -- | 'stdjOutputDataConfig'
  OutputDataConfig ->
  -- | 'stdjDataAccessRoleARN'
  Text ->
  StartTopicsDetectionJob
startTopicsDetectionJob
  pInputDataConfig_
  pOutputDataConfig_
  pDataAccessRoleARN_ =
    StartTopicsDetectionJob'
      { _stdjJobName = Nothing,
        _stdjVPCConfig = Nothing,
        _stdjVolumeKMSKeyId = Nothing,
        _stdjNumberOfTopics = Nothing,
        _stdjClientRequestToken = Nothing,
        _stdjInputDataConfig = pInputDataConfig_,
        _stdjOutputDataConfig = pOutputDataConfig_,
        _stdjDataAccessRoleARN = pDataAccessRoleARN_
      }

-- | The identifier of the job.
stdjJobName :: Lens' StartTopicsDetectionJob (Maybe Text)
stdjJobName = lens _stdjJobName (\s a -> s {_stdjJobName = a})

-- | Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for your topic detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
stdjVPCConfig :: Lens' StartTopicsDetectionJob (Maybe VPCConfig)
stdjVPCConfig = lens _stdjVPCConfig (\s a -> s {_stdjVPCConfig = a})

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@      * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
stdjVolumeKMSKeyId :: Lens' StartTopicsDetectionJob (Maybe Text)
stdjVolumeKMSKeyId = lens _stdjVolumeKMSKeyId (\s a -> s {_stdjVolumeKMSKeyId = a})

-- | The number of topics to detect.
stdjNumberOfTopics :: Lens' StartTopicsDetectionJob (Maybe Natural)
stdjNumberOfTopics = lens _stdjNumberOfTopics (\s a -> s {_stdjNumberOfTopics = a}) . mapping _Nat

-- | A unique identifier for the request. If you do not set the client request token, Amazon Comprehend generates one.
stdjClientRequestToken :: Lens' StartTopicsDetectionJob (Maybe Text)
stdjClientRequestToken = lens _stdjClientRequestToken (\s a -> s {_stdjClientRequestToken = a})

-- | Specifies the format and location of the input data for the job.
stdjInputDataConfig :: Lens' StartTopicsDetectionJob InputDataConfig
stdjInputDataConfig = lens _stdjInputDataConfig (\s a -> s {_stdjInputDataConfig = a})

-- | Specifies where to send the output files. The output is a compressed archive with two files, @topic-terms.csv@ that lists the terms associated with each topic, and @doc-topics.csv@ that lists the documents associated with each topic
stdjOutputDataConfig :: Lens' StartTopicsDetectionJob OutputDataConfig
stdjOutputDataConfig = lens _stdjOutputDataConfig (\s a -> s {_stdjOutputDataConfig = a})

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data. For more information, see <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions> .
stdjDataAccessRoleARN :: Lens' StartTopicsDetectionJob Text
stdjDataAccessRoleARN = lens _stdjDataAccessRoleARN (\s a -> s {_stdjDataAccessRoleARN = a})

instance AWSRequest StartTopicsDetectionJob where
  type Rs StartTopicsDetectionJob = StartTopicsDetectionJobResponse
  request = postJSON comprehend
  response =
    receiveJSON
      ( \s h x ->
          StartTopicsDetectionJobResponse'
            <$> (x .?> "JobId") <*> (x .?> "JobStatus") <*> (pure (fromEnum s))
      )

instance Hashable StartTopicsDetectionJob

instance NFData StartTopicsDetectionJob

instance ToHeaders StartTopicsDetectionJob where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Comprehend_20171127.StartTopicsDetectionJob" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON StartTopicsDetectionJob where
  toJSON StartTopicsDetectionJob' {..} =
    object
      ( catMaybes
          [ ("JobName" .=) <$> _stdjJobName,
            ("VpcConfig" .=) <$> _stdjVPCConfig,
            ("VolumeKmsKeyId" .=) <$> _stdjVolumeKMSKeyId,
            ("NumberOfTopics" .=) <$> _stdjNumberOfTopics,
            ("ClientRequestToken" .=) <$> _stdjClientRequestToken,
            Just ("InputDataConfig" .= _stdjInputDataConfig),
            Just ("OutputDataConfig" .= _stdjOutputDataConfig),
            Just ("DataAccessRoleArn" .= _stdjDataAccessRoleARN)
          ]
      )

instance ToPath StartTopicsDetectionJob where
  toPath = const "/"

instance ToQuery StartTopicsDetectionJob where
  toQuery = const mempty

-- | /See:/ 'startTopicsDetectionJobResponse' smart constructor.
data StartTopicsDetectionJobResponse = StartTopicsDetectionJobResponse'
  { _stdjrsJobId ::
      !(Maybe Text),
    _stdjrsJobStatus ::
      !(Maybe JobStatus),
    _stdjrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartTopicsDetectionJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stdjrsJobId' - The identifier generated for the job. To get the status of the job, use this identifier with the @DescribeTopicDetectionJob@ operation.
--
-- * 'stdjrsJobStatus' - The status of the job:      * SUBMITTED - The job has been received and is queued for processing.     * IN_PROGRESS - Amazon Comprehend is processing the job.     * COMPLETED - The job was successfully completed and the output is available.     * FAILED - The job did not complete. To get details, use the @DescribeTopicDetectionJob@ operation.
--
-- * 'stdjrsResponseStatus' - -- | The response status code.
startTopicsDetectionJobResponse ::
  -- | 'stdjrsResponseStatus'
  Int ->
  StartTopicsDetectionJobResponse
startTopicsDetectionJobResponse pResponseStatus_ =
  StartTopicsDetectionJobResponse'
    { _stdjrsJobId = Nothing,
      _stdjrsJobStatus = Nothing,
      _stdjrsResponseStatus = pResponseStatus_
    }

-- | The identifier generated for the job. To get the status of the job, use this identifier with the @DescribeTopicDetectionJob@ operation.
stdjrsJobId :: Lens' StartTopicsDetectionJobResponse (Maybe Text)
stdjrsJobId = lens _stdjrsJobId (\s a -> s {_stdjrsJobId = a})

-- | The status of the job:      * SUBMITTED - The job has been received and is queued for processing.     * IN_PROGRESS - Amazon Comprehend is processing the job.     * COMPLETED - The job was successfully completed and the output is available.     * FAILED - The job did not complete. To get details, use the @DescribeTopicDetectionJob@ operation.
stdjrsJobStatus :: Lens' StartTopicsDetectionJobResponse (Maybe JobStatus)
stdjrsJobStatus = lens _stdjrsJobStatus (\s a -> s {_stdjrsJobStatus = a})

-- | -- | The response status code.
stdjrsResponseStatus :: Lens' StartTopicsDetectionJobResponse Int
stdjrsResponseStatus = lens _stdjrsResponseStatus (\s a -> s {_stdjrsResponseStatus = a})

instance NFData StartTopicsDetectionJobResponse
