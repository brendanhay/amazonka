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
-- Module      : Network.AWS.Comprehend.StartDominantLanguageDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an asynchronous dominant language detection job for a collection of documents. Use the operation to track the status of a job.
module Network.AWS.Comprehend.StartDominantLanguageDetectionJob
  ( -- * Creating a Request
    startDominantLanguageDetectionJob,
    StartDominantLanguageDetectionJob,

    -- * Request Lenses
    sdldjJobName,
    sdldjVPCConfig,
    sdldjVolumeKMSKeyId,
    sdldjClientRequestToken,
    sdldjInputDataConfig,
    sdldjOutputDataConfig,
    sdldjDataAccessRoleARN,

    -- * Destructuring the Response
    startDominantLanguageDetectionJobResponse,
    StartDominantLanguageDetectionJobResponse,

    -- * Response Lenses
    sdldjrsJobId,
    sdldjrsJobStatus,
    sdldjrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startDominantLanguageDetectionJob' smart constructor.
data StartDominantLanguageDetectionJob = StartDominantLanguageDetectionJob'
  { _sdldjJobName ::
      !(Maybe Text),
    _sdldjVPCConfig ::
      !(Maybe VPCConfig),
    _sdldjVolumeKMSKeyId ::
      !(Maybe Text),
    _sdldjClientRequestToken ::
      !(Maybe Text),
    _sdldjInputDataConfig ::
      !InputDataConfig,
    _sdldjOutputDataConfig ::
      !OutputDataConfig,
    _sdldjDataAccessRoleARN ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartDominantLanguageDetectionJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdldjJobName' - An identifier for the job.
--
-- * 'sdldjVPCConfig' - Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for your dominant language detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
--
-- * 'sdldjVolumeKMSKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@      * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
--
-- * 'sdldjClientRequestToken' - A unique identifier for the request. If you do not set the client request token, Amazon Comprehend generates one.
--
-- * 'sdldjInputDataConfig' - Specifies the format and location of the input data for the job.
--
-- * 'sdldjOutputDataConfig' - Specifies where to send the output files.
--
-- * 'sdldjDataAccessRoleARN' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data. For more information, see <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions> .
startDominantLanguageDetectionJob ::
  -- | 'sdldjInputDataConfig'
  InputDataConfig ->
  -- | 'sdldjOutputDataConfig'
  OutputDataConfig ->
  -- | 'sdldjDataAccessRoleARN'
  Text ->
  StartDominantLanguageDetectionJob
startDominantLanguageDetectionJob
  pInputDataConfig_
  pOutputDataConfig_
  pDataAccessRoleARN_ =
    StartDominantLanguageDetectionJob'
      { _sdldjJobName = Nothing,
        _sdldjVPCConfig = Nothing,
        _sdldjVolumeKMSKeyId = Nothing,
        _sdldjClientRequestToken = Nothing,
        _sdldjInputDataConfig = pInputDataConfig_,
        _sdldjOutputDataConfig = pOutputDataConfig_,
        _sdldjDataAccessRoleARN = pDataAccessRoleARN_
      }

-- | An identifier for the job.
sdldjJobName :: Lens' StartDominantLanguageDetectionJob (Maybe Text)
sdldjJobName = lens _sdldjJobName (\s a -> s {_sdldjJobName = a})

-- | Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for your dominant language detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
sdldjVPCConfig :: Lens' StartDominantLanguageDetectionJob (Maybe VPCConfig)
sdldjVPCConfig = lens _sdldjVPCConfig (\s a -> s {_sdldjVPCConfig = a})

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@      * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
sdldjVolumeKMSKeyId :: Lens' StartDominantLanguageDetectionJob (Maybe Text)
sdldjVolumeKMSKeyId = lens _sdldjVolumeKMSKeyId (\s a -> s {_sdldjVolumeKMSKeyId = a})

-- | A unique identifier for the request. If you do not set the client request token, Amazon Comprehend generates one.
sdldjClientRequestToken :: Lens' StartDominantLanguageDetectionJob (Maybe Text)
sdldjClientRequestToken = lens _sdldjClientRequestToken (\s a -> s {_sdldjClientRequestToken = a})

-- | Specifies the format and location of the input data for the job.
sdldjInputDataConfig :: Lens' StartDominantLanguageDetectionJob InputDataConfig
sdldjInputDataConfig = lens _sdldjInputDataConfig (\s a -> s {_sdldjInputDataConfig = a})

-- | Specifies where to send the output files.
sdldjOutputDataConfig :: Lens' StartDominantLanguageDetectionJob OutputDataConfig
sdldjOutputDataConfig = lens _sdldjOutputDataConfig (\s a -> s {_sdldjOutputDataConfig = a})

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data. For more information, see <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions> .
sdldjDataAccessRoleARN :: Lens' StartDominantLanguageDetectionJob Text
sdldjDataAccessRoleARN = lens _sdldjDataAccessRoleARN (\s a -> s {_sdldjDataAccessRoleARN = a})

instance AWSRequest StartDominantLanguageDetectionJob where
  type
    Rs StartDominantLanguageDetectionJob =
      StartDominantLanguageDetectionJobResponse
  request = postJSON comprehend
  response =
    receiveJSON
      ( \s h x ->
          StartDominantLanguageDetectionJobResponse'
            <$> (x .?> "JobId") <*> (x .?> "JobStatus") <*> (pure (fromEnum s))
      )

instance Hashable StartDominantLanguageDetectionJob

instance NFData StartDominantLanguageDetectionJob

instance ToHeaders StartDominantLanguageDetectionJob where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "Comprehend_20171127.StartDominantLanguageDetectionJob" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON StartDominantLanguageDetectionJob where
  toJSON StartDominantLanguageDetectionJob' {..} =
    object
      ( catMaybes
          [ ("JobName" .=) <$> _sdldjJobName,
            ("VpcConfig" .=) <$> _sdldjVPCConfig,
            ("VolumeKmsKeyId" .=) <$> _sdldjVolumeKMSKeyId,
            ("ClientRequestToken" .=) <$> _sdldjClientRequestToken,
            Just ("InputDataConfig" .= _sdldjInputDataConfig),
            Just ("OutputDataConfig" .= _sdldjOutputDataConfig),
            Just ("DataAccessRoleArn" .= _sdldjDataAccessRoleARN)
          ]
      )

instance ToPath StartDominantLanguageDetectionJob where
  toPath = const "/"

instance ToQuery StartDominantLanguageDetectionJob where
  toQuery = const mempty

-- | /See:/ 'startDominantLanguageDetectionJobResponse' smart constructor.
data StartDominantLanguageDetectionJobResponse = StartDominantLanguageDetectionJobResponse'
  { _sdldjrsJobId ::
      !( Maybe
           Text
       ),
    _sdldjrsJobStatus ::
      !( Maybe
           JobStatus
       ),
    _sdldjrsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'StartDominantLanguageDetectionJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdldjrsJobId' - The identifier generated for the job. To get the status of a job, use this identifier with the operation.
--
-- * 'sdldjrsJobStatus' - The status of the job.      * SUBMITTED - The job has been received and is queued for processing.     * IN_PROGRESS - Amazon Comprehend is processing the job.     * COMPLETED - The job was successfully completed and the output is available.     * FAILED - The job did not complete. To get details, use the operation.
--
-- * 'sdldjrsResponseStatus' - -- | The response status code.
startDominantLanguageDetectionJobResponse ::
  -- | 'sdldjrsResponseStatus'
  Int ->
  StartDominantLanguageDetectionJobResponse
startDominantLanguageDetectionJobResponse pResponseStatus_ =
  StartDominantLanguageDetectionJobResponse'
    { _sdldjrsJobId =
        Nothing,
      _sdldjrsJobStatus = Nothing,
      _sdldjrsResponseStatus = pResponseStatus_
    }

-- | The identifier generated for the job. To get the status of a job, use this identifier with the operation.
sdldjrsJobId :: Lens' StartDominantLanguageDetectionJobResponse (Maybe Text)
sdldjrsJobId = lens _sdldjrsJobId (\s a -> s {_sdldjrsJobId = a})

-- | The status of the job.      * SUBMITTED - The job has been received and is queued for processing.     * IN_PROGRESS - Amazon Comprehend is processing the job.     * COMPLETED - The job was successfully completed and the output is available.     * FAILED - The job did not complete. To get details, use the operation.
sdldjrsJobStatus :: Lens' StartDominantLanguageDetectionJobResponse (Maybe JobStatus)
sdldjrsJobStatus = lens _sdldjrsJobStatus (\s a -> s {_sdldjrsJobStatus = a})

-- | -- | The response status code.
sdldjrsResponseStatus :: Lens' StartDominantLanguageDetectionJobResponse Int
sdldjrsResponseStatus = lens _sdldjrsResponseStatus (\s a -> s {_sdldjrsResponseStatus = a})

instance NFData StartDominantLanguageDetectionJobResponse
