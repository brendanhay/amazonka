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
-- Module      : Network.AWS.Comprehend.StartSentimentDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an asynchronous sentiment detection job for a collection of documents. use the operation to track the status of a job.
module Network.AWS.Comprehend.StartSentimentDetectionJob
  ( -- * Creating a Request
    startSentimentDetectionJob,
    StartSentimentDetectionJob,

    -- * Request Lenses
    ssdjJobName,
    ssdjVPCConfig,
    ssdjVolumeKMSKeyId,
    ssdjClientRequestToken,
    ssdjInputDataConfig,
    ssdjOutputDataConfig,
    ssdjDataAccessRoleARN,
    ssdjLanguageCode,

    -- * Destructuring the Response
    startSentimentDetectionJobResponse,
    StartSentimentDetectionJobResponse,

    -- * Response Lenses
    ssdjrsJobId,
    ssdjrsJobStatus,
    ssdjrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startSentimentDetectionJob' smart constructor.
data StartSentimentDetectionJob = StartSentimentDetectionJob'
  { _ssdjJobName ::
      !(Maybe Text),
    _ssdjVPCConfig :: !(Maybe VPCConfig),
    _ssdjVolumeKMSKeyId :: !(Maybe Text),
    _ssdjClientRequestToken ::
      !(Maybe Text),
    _ssdjInputDataConfig ::
      !InputDataConfig,
    _ssdjOutputDataConfig ::
      !OutputDataConfig,
    _ssdjDataAccessRoleARN :: !Text,
    _ssdjLanguageCode :: !LanguageCode
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartSentimentDetectionJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssdjJobName' - The identifier of the job.
--
-- * 'ssdjVPCConfig' - Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for your sentiment detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
--
-- * 'ssdjVolumeKMSKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@      * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
--
-- * 'ssdjClientRequestToken' - A unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
--
-- * 'ssdjInputDataConfig' - Specifies the format and location of the input data for the job.
--
-- * 'ssdjOutputDataConfig' - Specifies where to send the output files.
--
-- * 'ssdjDataAccessRoleARN' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data. For more information, see <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions> .
--
-- * 'ssdjLanguageCode' - The language of the input documents. You can specify any of the primary languages supported by Amazon Comprehend. All documents must be in the same language.
startSentimentDetectionJob ::
  -- | 'ssdjInputDataConfig'
  InputDataConfig ->
  -- | 'ssdjOutputDataConfig'
  OutputDataConfig ->
  -- | 'ssdjDataAccessRoleARN'
  Text ->
  -- | 'ssdjLanguageCode'
  LanguageCode ->
  StartSentimentDetectionJob
startSentimentDetectionJob
  pInputDataConfig_
  pOutputDataConfig_
  pDataAccessRoleARN_
  pLanguageCode_ =
    StartSentimentDetectionJob'
      { _ssdjJobName = Nothing,
        _ssdjVPCConfig = Nothing,
        _ssdjVolumeKMSKeyId = Nothing,
        _ssdjClientRequestToken = Nothing,
        _ssdjInputDataConfig = pInputDataConfig_,
        _ssdjOutputDataConfig = pOutputDataConfig_,
        _ssdjDataAccessRoleARN = pDataAccessRoleARN_,
        _ssdjLanguageCode = pLanguageCode_
      }

-- | The identifier of the job.
ssdjJobName :: Lens' StartSentimentDetectionJob (Maybe Text)
ssdjJobName = lens _ssdjJobName (\s a -> s {_ssdjJobName = a})

-- | Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for your sentiment detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
ssdjVPCConfig :: Lens' StartSentimentDetectionJob (Maybe VPCConfig)
ssdjVPCConfig = lens _ssdjVPCConfig (\s a -> s {_ssdjVPCConfig = a})

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@      * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
ssdjVolumeKMSKeyId :: Lens' StartSentimentDetectionJob (Maybe Text)
ssdjVolumeKMSKeyId = lens _ssdjVolumeKMSKeyId (\s a -> s {_ssdjVolumeKMSKeyId = a})

-- | A unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
ssdjClientRequestToken :: Lens' StartSentimentDetectionJob (Maybe Text)
ssdjClientRequestToken = lens _ssdjClientRequestToken (\s a -> s {_ssdjClientRequestToken = a})

-- | Specifies the format and location of the input data for the job.
ssdjInputDataConfig :: Lens' StartSentimentDetectionJob InputDataConfig
ssdjInputDataConfig = lens _ssdjInputDataConfig (\s a -> s {_ssdjInputDataConfig = a})

-- | Specifies where to send the output files.
ssdjOutputDataConfig :: Lens' StartSentimentDetectionJob OutputDataConfig
ssdjOutputDataConfig = lens _ssdjOutputDataConfig (\s a -> s {_ssdjOutputDataConfig = a})

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data. For more information, see <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions> .
ssdjDataAccessRoleARN :: Lens' StartSentimentDetectionJob Text
ssdjDataAccessRoleARN = lens _ssdjDataAccessRoleARN (\s a -> s {_ssdjDataAccessRoleARN = a})

-- | The language of the input documents. You can specify any of the primary languages supported by Amazon Comprehend. All documents must be in the same language.
ssdjLanguageCode :: Lens' StartSentimentDetectionJob LanguageCode
ssdjLanguageCode = lens _ssdjLanguageCode (\s a -> s {_ssdjLanguageCode = a})

instance AWSRequest StartSentimentDetectionJob where
  type
    Rs StartSentimentDetectionJob =
      StartSentimentDetectionJobResponse
  request = postJSON comprehend
  response =
    receiveJSON
      ( \s h x ->
          StartSentimentDetectionJobResponse'
            <$> (x .?> "JobId") <*> (x .?> "JobStatus") <*> (pure (fromEnum s))
      )

instance Hashable StartSentimentDetectionJob

instance NFData StartSentimentDetectionJob

instance ToHeaders StartSentimentDetectionJob where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Comprehend_20171127.StartSentimentDetectionJob" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON StartSentimentDetectionJob where
  toJSON StartSentimentDetectionJob' {..} =
    object
      ( catMaybes
          [ ("JobName" .=) <$> _ssdjJobName,
            ("VpcConfig" .=) <$> _ssdjVPCConfig,
            ("VolumeKmsKeyId" .=) <$> _ssdjVolumeKMSKeyId,
            ("ClientRequestToken" .=) <$> _ssdjClientRequestToken,
            Just ("InputDataConfig" .= _ssdjInputDataConfig),
            Just ("OutputDataConfig" .= _ssdjOutputDataConfig),
            Just ("DataAccessRoleArn" .= _ssdjDataAccessRoleARN),
            Just ("LanguageCode" .= _ssdjLanguageCode)
          ]
      )

instance ToPath StartSentimentDetectionJob where
  toPath = const "/"

instance ToQuery StartSentimentDetectionJob where
  toQuery = const mempty

-- | /See:/ 'startSentimentDetectionJobResponse' smart constructor.
data StartSentimentDetectionJobResponse = StartSentimentDetectionJobResponse'
  { _ssdjrsJobId ::
      !(Maybe Text),
    _ssdjrsJobStatus ::
      !(Maybe JobStatus),
    _ssdjrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartSentimentDetectionJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssdjrsJobId' - The identifier generated for the job. To get the status of a job, use this identifier with the operation.
--
-- * 'ssdjrsJobStatus' - The status of the job.      * SUBMITTED - The job has been received and is queued for processing.     * IN_PROGRESS - Amazon Comprehend is processing the job.     * COMPLETED - The job was successfully completed and the output is available.     * FAILED - The job did not complete. To get details, use the operation.
--
-- * 'ssdjrsResponseStatus' - -- | The response status code.
startSentimentDetectionJobResponse ::
  -- | 'ssdjrsResponseStatus'
  Int ->
  StartSentimentDetectionJobResponse
startSentimentDetectionJobResponse pResponseStatus_ =
  StartSentimentDetectionJobResponse'
    { _ssdjrsJobId = Nothing,
      _ssdjrsJobStatus = Nothing,
      _ssdjrsResponseStatus = pResponseStatus_
    }

-- | The identifier generated for the job. To get the status of a job, use this identifier with the operation.
ssdjrsJobId :: Lens' StartSentimentDetectionJobResponse (Maybe Text)
ssdjrsJobId = lens _ssdjrsJobId (\s a -> s {_ssdjrsJobId = a})

-- | The status of the job.      * SUBMITTED - The job has been received and is queued for processing.     * IN_PROGRESS - Amazon Comprehend is processing the job.     * COMPLETED - The job was successfully completed and the output is available.     * FAILED - The job did not complete. To get details, use the operation.
ssdjrsJobStatus :: Lens' StartSentimentDetectionJobResponse (Maybe JobStatus)
ssdjrsJobStatus = lens _ssdjrsJobStatus (\s a -> s {_ssdjrsJobStatus = a})

-- | -- | The response status code.
ssdjrsResponseStatus :: Lens' StartSentimentDetectionJobResponse Int
ssdjrsResponseStatus = lens _ssdjrsResponseStatus (\s a -> s {_ssdjrsResponseStatus = a})

instance NFData StartSentimentDetectionJobResponse
