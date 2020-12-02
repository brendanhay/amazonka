{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.SentimentDetectionJobProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.SentimentDetectionJobProperties where

import Network.AWS.Comprehend.Types.InputDataConfig
import Network.AWS.Comprehend.Types.JobStatus
import Network.AWS.Comprehend.Types.LanguageCode
import Network.AWS.Comprehend.Types.OutputDataConfig
import Network.AWS.Comprehend.Types.VPCConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information about a sentiment detection job.
--
--
--
-- /See:/ 'sentimentDetectionJobProperties' smart constructor.
data SentimentDetectionJobProperties = SentimentDetectionJobProperties'
  { _sdjpLanguageCode ::
      !(Maybe LanguageCode),
    _sdjpJobId :: !(Maybe Text),
    _sdjpJobName ::
      !(Maybe Text),
    _sdjpInputDataConfig ::
      !(Maybe InputDataConfig),
    _sdjpVPCConfig ::
      !(Maybe VPCConfig),
    _sdjpVolumeKMSKeyId ::
      !(Maybe Text),
    _sdjpEndTime ::
      !(Maybe POSIX),
    _sdjpOutputDataConfig ::
      !(Maybe OutputDataConfig),
    _sdjpDataAccessRoleARN ::
      !(Maybe Text),
    _sdjpJobStatus ::
      !(Maybe JobStatus),
    _sdjpMessage ::
      !(Maybe Text),
    _sdjpSubmitTime ::
      !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SentimentDetectionJobProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdjpLanguageCode' - The language code of the input documents.
--
-- * 'sdjpJobId' - The identifier assigned to the sentiment detection job.
--
-- * 'sdjpJobName' - The name that you assigned to the sentiment detection job
--
-- * 'sdjpInputDataConfig' - The input data configuration that you supplied when you created the sentiment detection job.
--
-- * 'sdjpVPCConfig' - Configuration parameters for a private Virtual Private Cloud (VPC) containing the resources you are using for your sentiment detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
--
-- * 'sdjpVolumeKMSKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@      * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
--
-- * 'sdjpEndTime' - The time that the sentiment detection job ended.
--
-- * 'sdjpOutputDataConfig' - The output data configuration that you supplied when you created the sentiment detection job.
--
-- * 'sdjpDataAccessRoleARN' - The Amazon Resource Name (ARN) that gives Amazon Comprehend read access to your input data.
--
-- * 'sdjpJobStatus' - The current status of the sentiment detection job. If the status is @FAILED@ , the @Messages@ field shows the reason for the failure.
--
-- * 'sdjpMessage' - A description of the status of a job.
--
-- * 'sdjpSubmitTime' - The time that the sentiment detection job was submitted for processing.
sentimentDetectionJobProperties ::
  SentimentDetectionJobProperties
sentimentDetectionJobProperties =
  SentimentDetectionJobProperties'
    { _sdjpLanguageCode = Nothing,
      _sdjpJobId = Nothing,
      _sdjpJobName = Nothing,
      _sdjpInputDataConfig = Nothing,
      _sdjpVPCConfig = Nothing,
      _sdjpVolumeKMSKeyId = Nothing,
      _sdjpEndTime = Nothing,
      _sdjpOutputDataConfig = Nothing,
      _sdjpDataAccessRoleARN = Nothing,
      _sdjpJobStatus = Nothing,
      _sdjpMessage = Nothing,
      _sdjpSubmitTime = Nothing
    }

-- | The language code of the input documents.
sdjpLanguageCode :: Lens' SentimentDetectionJobProperties (Maybe LanguageCode)
sdjpLanguageCode = lens _sdjpLanguageCode (\s a -> s {_sdjpLanguageCode = a})

-- | The identifier assigned to the sentiment detection job.
sdjpJobId :: Lens' SentimentDetectionJobProperties (Maybe Text)
sdjpJobId = lens _sdjpJobId (\s a -> s {_sdjpJobId = a})

-- | The name that you assigned to the sentiment detection job
sdjpJobName :: Lens' SentimentDetectionJobProperties (Maybe Text)
sdjpJobName = lens _sdjpJobName (\s a -> s {_sdjpJobName = a})

-- | The input data configuration that you supplied when you created the sentiment detection job.
sdjpInputDataConfig :: Lens' SentimentDetectionJobProperties (Maybe InputDataConfig)
sdjpInputDataConfig = lens _sdjpInputDataConfig (\s a -> s {_sdjpInputDataConfig = a})

-- | Configuration parameters for a private Virtual Private Cloud (VPC) containing the resources you are using for your sentiment detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
sdjpVPCConfig :: Lens' SentimentDetectionJobProperties (Maybe VPCConfig)
sdjpVPCConfig = lens _sdjpVPCConfig (\s a -> s {_sdjpVPCConfig = a})

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@      * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
sdjpVolumeKMSKeyId :: Lens' SentimentDetectionJobProperties (Maybe Text)
sdjpVolumeKMSKeyId = lens _sdjpVolumeKMSKeyId (\s a -> s {_sdjpVolumeKMSKeyId = a})

-- | The time that the sentiment detection job ended.
sdjpEndTime :: Lens' SentimentDetectionJobProperties (Maybe UTCTime)
sdjpEndTime = lens _sdjpEndTime (\s a -> s {_sdjpEndTime = a}) . mapping _Time

-- | The output data configuration that you supplied when you created the sentiment detection job.
sdjpOutputDataConfig :: Lens' SentimentDetectionJobProperties (Maybe OutputDataConfig)
sdjpOutputDataConfig = lens _sdjpOutputDataConfig (\s a -> s {_sdjpOutputDataConfig = a})

-- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access to your input data.
sdjpDataAccessRoleARN :: Lens' SentimentDetectionJobProperties (Maybe Text)
sdjpDataAccessRoleARN = lens _sdjpDataAccessRoleARN (\s a -> s {_sdjpDataAccessRoleARN = a})

-- | The current status of the sentiment detection job. If the status is @FAILED@ , the @Messages@ field shows the reason for the failure.
sdjpJobStatus :: Lens' SentimentDetectionJobProperties (Maybe JobStatus)
sdjpJobStatus = lens _sdjpJobStatus (\s a -> s {_sdjpJobStatus = a})

-- | A description of the status of a job.
sdjpMessage :: Lens' SentimentDetectionJobProperties (Maybe Text)
sdjpMessage = lens _sdjpMessage (\s a -> s {_sdjpMessage = a})

-- | The time that the sentiment detection job was submitted for processing.
sdjpSubmitTime :: Lens' SentimentDetectionJobProperties (Maybe UTCTime)
sdjpSubmitTime = lens _sdjpSubmitTime (\s a -> s {_sdjpSubmitTime = a}) . mapping _Time

instance FromJSON SentimentDetectionJobProperties where
  parseJSON =
    withObject
      "SentimentDetectionJobProperties"
      ( \x ->
          SentimentDetectionJobProperties'
            <$> (x .:? "LanguageCode")
            <*> (x .:? "JobId")
            <*> (x .:? "JobName")
            <*> (x .:? "InputDataConfig")
            <*> (x .:? "VpcConfig")
            <*> (x .:? "VolumeKmsKeyId")
            <*> (x .:? "EndTime")
            <*> (x .:? "OutputDataConfig")
            <*> (x .:? "DataAccessRoleArn")
            <*> (x .:? "JobStatus")
            <*> (x .:? "Message")
            <*> (x .:? "SubmitTime")
      )

instance Hashable SentimentDetectionJobProperties

instance NFData SentimentDetectionJobProperties
