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
-- Module      : Network.AWS.Comprehend.StartEntitiesDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an asynchronous entity detection job for a collection of documents. Use the operation to track the status of a job.
--
--
-- This API can be used for either standard entity detection or custom entity recognition. In order to be used for custom entity recognition, the optional @EntityRecognizerArn@ must be used in order to provide access to the recognizer being used to detect the custom entity.
module Network.AWS.Comprehend.StartEntitiesDetectionJob
  ( -- * Creating a Request
    startEntitiesDetectionJob,
    StartEntitiesDetectionJob,

    -- * Request Lenses
    sedjEntityRecognizerARN,
    sedjJobName,
    sedjVPCConfig,
    sedjVolumeKMSKeyId,
    sedjClientRequestToken,
    sedjInputDataConfig,
    sedjOutputDataConfig,
    sedjDataAccessRoleARN,
    sedjLanguageCode,

    -- * Destructuring the Response
    startEntitiesDetectionJobResponse,
    StartEntitiesDetectionJobResponse,

    -- * Response Lenses
    sedjersJobId,
    sedjersJobStatus,
    sedjersResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startEntitiesDetectionJob' smart constructor.
data StartEntitiesDetectionJob = StartEntitiesDetectionJob'
  { _sedjEntityRecognizerARN ::
      !(Maybe Text),
    _sedjJobName :: !(Maybe Text),
    _sedjVPCConfig :: !(Maybe VPCConfig),
    _sedjVolumeKMSKeyId :: !(Maybe Text),
    _sedjClientRequestToken ::
      !(Maybe Text),
    _sedjInputDataConfig ::
      !InputDataConfig,
    _sedjOutputDataConfig ::
      !OutputDataConfig,
    _sedjDataAccessRoleARN :: !Text,
    _sedjLanguageCode :: !LanguageCode
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartEntitiesDetectionJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sedjEntityRecognizerARN' - The Amazon Resource Name (ARN) that identifies the specific entity recognizer to be used by the @StartEntitiesDetectionJob@ . This ARN is optional and is only used for a custom entity recognition job.
--
-- * 'sedjJobName' - The identifier of the job.
--
-- * 'sedjVPCConfig' - Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for your entity detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
--
-- * 'sedjVolumeKMSKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@      * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
--
-- * 'sedjClientRequestToken' - A unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
--
-- * 'sedjInputDataConfig' - Specifies the format and location of the input data for the job.
--
-- * 'sedjOutputDataConfig' - Specifies where to send the output files.
--
-- * 'sedjDataAccessRoleARN' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data. For more information, see <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions> .
--
-- * 'sedjLanguageCode' - The language of the input documents. All documents must be in the same language. You can specify any of the languages supported by Amazon Comprehend. If custom entities recognition is used, this parameter is ignored and the language used for training the model is used instead.
startEntitiesDetectionJob ::
  -- | 'sedjInputDataConfig'
  InputDataConfig ->
  -- | 'sedjOutputDataConfig'
  OutputDataConfig ->
  -- | 'sedjDataAccessRoleARN'
  Text ->
  -- | 'sedjLanguageCode'
  LanguageCode ->
  StartEntitiesDetectionJob
startEntitiesDetectionJob
  pInputDataConfig_
  pOutputDataConfig_
  pDataAccessRoleARN_
  pLanguageCode_ =
    StartEntitiesDetectionJob'
      { _sedjEntityRecognizerARN = Nothing,
        _sedjJobName = Nothing,
        _sedjVPCConfig = Nothing,
        _sedjVolumeKMSKeyId = Nothing,
        _sedjClientRequestToken = Nothing,
        _sedjInputDataConfig = pInputDataConfig_,
        _sedjOutputDataConfig = pOutputDataConfig_,
        _sedjDataAccessRoleARN = pDataAccessRoleARN_,
        _sedjLanguageCode = pLanguageCode_
      }

-- | The Amazon Resource Name (ARN) that identifies the specific entity recognizer to be used by the @StartEntitiesDetectionJob@ . This ARN is optional and is only used for a custom entity recognition job.
sedjEntityRecognizerARN :: Lens' StartEntitiesDetectionJob (Maybe Text)
sedjEntityRecognizerARN = lens _sedjEntityRecognizerARN (\s a -> s {_sedjEntityRecognizerARN = a})

-- | The identifier of the job.
sedjJobName :: Lens' StartEntitiesDetectionJob (Maybe Text)
sedjJobName = lens _sedjJobName (\s a -> s {_sedjJobName = a})

-- | Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for your entity detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
sedjVPCConfig :: Lens' StartEntitiesDetectionJob (Maybe VPCConfig)
sedjVPCConfig = lens _sedjVPCConfig (\s a -> s {_sedjVPCConfig = a})

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@      * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
sedjVolumeKMSKeyId :: Lens' StartEntitiesDetectionJob (Maybe Text)
sedjVolumeKMSKeyId = lens _sedjVolumeKMSKeyId (\s a -> s {_sedjVolumeKMSKeyId = a})

-- | A unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
sedjClientRequestToken :: Lens' StartEntitiesDetectionJob (Maybe Text)
sedjClientRequestToken = lens _sedjClientRequestToken (\s a -> s {_sedjClientRequestToken = a})

-- | Specifies the format and location of the input data for the job.
sedjInputDataConfig :: Lens' StartEntitiesDetectionJob InputDataConfig
sedjInputDataConfig = lens _sedjInputDataConfig (\s a -> s {_sedjInputDataConfig = a})

-- | Specifies where to send the output files.
sedjOutputDataConfig :: Lens' StartEntitiesDetectionJob OutputDataConfig
sedjOutputDataConfig = lens _sedjOutputDataConfig (\s a -> s {_sedjOutputDataConfig = a})

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data. For more information, see <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions> .
sedjDataAccessRoleARN :: Lens' StartEntitiesDetectionJob Text
sedjDataAccessRoleARN = lens _sedjDataAccessRoleARN (\s a -> s {_sedjDataAccessRoleARN = a})

-- | The language of the input documents. All documents must be in the same language. You can specify any of the languages supported by Amazon Comprehend. If custom entities recognition is used, this parameter is ignored and the language used for training the model is used instead.
sedjLanguageCode :: Lens' StartEntitiesDetectionJob LanguageCode
sedjLanguageCode = lens _sedjLanguageCode (\s a -> s {_sedjLanguageCode = a})

instance AWSRequest StartEntitiesDetectionJob where
  type
    Rs StartEntitiesDetectionJob =
      StartEntitiesDetectionJobResponse
  request = postJSON comprehend
  response =
    receiveJSON
      ( \s h x ->
          StartEntitiesDetectionJobResponse'
            <$> (x .?> "JobId") <*> (x .?> "JobStatus") <*> (pure (fromEnum s))
      )

instance Hashable StartEntitiesDetectionJob

instance NFData StartEntitiesDetectionJob

instance ToHeaders StartEntitiesDetectionJob where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Comprehend_20171127.StartEntitiesDetectionJob" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON StartEntitiesDetectionJob where
  toJSON StartEntitiesDetectionJob' {..} =
    object
      ( catMaybes
          [ ("EntityRecognizerArn" .=) <$> _sedjEntityRecognizerARN,
            ("JobName" .=) <$> _sedjJobName,
            ("VpcConfig" .=) <$> _sedjVPCConfig,
            ("VolumeKmsKeyId" .=) <$> _sedjVolumeKMSKeyId,
            ("ClientRequestToken" .=) <$> _sedjClientRequestToken,
            Just ("InputDataConfig" .= _sedjInputDataConfig),
            Just ("OutputDataConfig" .= _sedjOutputDataConfig),
            Just ("DataAccessRoleArn" .= _sedjDataAccessRoleARN),
            Just ("LanguageCode" .= _sedjLanguageCode)
          ]
      )

instance ToPath StartEntitiesDetectionJob where
  toPath = const "/"

instance ToQuery StartEntitiesDetectionJob where
  toQuery = const mempty

-- | /See:/ 'startEntitiesDetectionJobResponse' smart constructor.
data StartEntitiesDetectionJobResponse = StartEntitiesDetectionJobResponse'
  { _sedjersJobId ::
      !(Maybe Text),
    _sedjersJobStatus ::
      !(Maybe JobStatus),
    _sedjersResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartEntitiesDetectionJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sedjersJobId' - The identifier generated for the job. To get the status of job, use this identifier with the operation.
--
-- * 'sedjersJobStatus' - The status of the job.      * SUBMITTED - The job has been received and is queued for processing.     * IN_PROGRESS - Amazon Comprehend is processing the job.     * COMPLETED - The job was successfully completed and the output is available.     * FAILED - The job did not complete. To get details, use the operation.     * STOP_REQUESTED - Amazon Comprehend has received a stop request for the job and is processing the request.     * STOPPED - The job was successfully stopped without completing.
--
-- * 'sedjersResponseStatus' - -- | The response status code.
startEntitiesDetectionJobResponse ::
  -- | 'sedjersResponseStatus'
  Int ->
  StartEntitiesDetectionJobResponse
startEntitiesDetectionJobResponse pResponseStatus_ =
  StartEntitiesDetectionJobResponse'
    { _sedjersJobId = Nothing,
      _sedjersJobStatus = Nothing,
      _sedjersResponseStatus = pResponseStatus_
    }

-- | The identifier generated for the job. To get the status of job, use this identifier with the operation.
sedjersJobId :: Lens' StartEntitiesDetectionJobResponse (Maybe Text)
sedjersJobId = lens _sedjersJobId (\s a -> s {_sedjersJobId = a})

-- | The status of the job.      * SUBMITTED - The job has been received and is queued for processing.     * IN_PROGRESS - Amazon Comprehend is processing the job.     * COMPLETED - The job was successfully completed and the output is available.     * FAILED - The job did not complete. To get details, use the operation.     * STOP_REQUESTED - Amazon Comprehend has received a stop request for the job and is processing the request.     * STOPPED - The job was successfully stopped without completing.
sedjersJobStatus :: Lens' StartEntitiesDetectionJobResponse (Maybe JobStatus)
sedjersJobStatus = lens _sedjersJobStatus (\s a -> s {_sedjersJobStatus = a})

-- | -- | The response status code.
sedjersResponseStatus :: Lens' StartEntitiesDetectionJobResponse Int
sedjersResponseStatus = lens _sedjersResponseStatus (\s a -> s {_sedjersResponseStatus = a})

instance NFData StartEntitiesDetectionJobResponse
