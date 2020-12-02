{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.TopicsDetectionJobProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.TopicsDetectionJobProperties where

import Network.AWS.Comprehend.Types.InputDataConfig
import Network.AWS.Comprehend.Types.JobStatus
import Network.AWS.Comprehend.Types.OutputDataConfig
import Network.AWS.Comprehend.Types.VPCConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information about a topic detection job.
--
--
--
-- /See:/ 'topicsDetectionJobProperties' smart constructor.
data TopicsDetectionJobProperties = TopicsDetectionJobProperties'
  { _tdjpJobId ::
      !(Maybe Text),
    _tdjpJobName :: !(Maybe Text),
    _tdjpInputDataConfig ::
      !(Maybe InputDataConfig),
    _tdjpVPCConfig ::
      !(Maybe VPCConfig),
    _tdjpVolumeKMSKeyId ::
      !(Maybe Text),
    _tdjpEndTime :: !(Maybe POSIX),
    _tdjpOutputDataConfig ::
      !(Maybe OutputDataConfig),
    _tdjpDataAccessRoleARN ::
      !(Maybe Text),
    _tdjpNumberOfTopics ::
      !(Maybe Int),
    _tdjpJobStatus ::
      !(Maybe JobStatus),
    _tdjpMessage :: !(Maybe Text),
    _tdjpSubmitTime :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TopicsDetectionJobProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdjpJobId' - The identifier assigned to the topic detection job.
--
-- * 'tdjpJobName' - The name of the topic detection job.
--
-- * 'tdjpInputDataConfig' - The input data configuration supplied when you created the topic detection job.
--
-- * 'tdjpVPCConfig' - Configuration parameters for a private Virtual Private Cloud (VPC) containing the resources you are using for your topic detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
--
-- * 'tdjpVolumeKMSKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@      * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
--
-- * 'tdjpEndTime' - The time that the topic detection job was completed.
--
-- * 'tdjpOutputDataConfig' - The output data configuration supplied when you created the topic detection job.
--
-- * 'tdjpDataAccessRoleARN' - The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM) role that grants Amazon Comprehend read access to your job data.
--
-- * 'tdjpNumberOfTopics' - The number of topics to detect supplied when you created the topic detection job. The default is 10.
--
-- * 'tdjpJobStatus' - The current status of the topic detection job. If the status is @Failed@ , the reason for the failure is shown in the @Message@ field.
--
-- * 'tdjpMessage' - A description for the status of a job.
--
-- * 'tdjpSubmitTime' - The time that the topic detection job was submitted for processing.
topicsDetectionJobProperties ::
  TopicsDetectionJobProperties
topicsDetectionJobProperties =
  TopicsDetectionJobProperties'
    { _tdjpJobId = Nothing,
      _tdjpJobName = Nothing,
      _tdjpInputDataConfig = Nothing,
      _tdjpVPCConfig = Nothing,
      _tdjpVolumeKMSKeyId = Nothing,
      _tdjpEndTime = Nothing,
      _tdjpOutputDataConfig = Nothing,
      _tdjpDataAccessRoleARN = Nothing,
      _tdjpNumberOfTopics = Nothing,
      _tdjpJobStatus = Nothing,
      _tdjpMessage = Nothing,
      _tdjpSubmitTime = Nothing
    }

-- | The identifier assigned to the topic detection job.
tdjpJobId :: Lens' TopicsDetectionJobProperties (Maybe Text)
tdjpJobId = lens _tdjpJobId (\s a -> s {_tdjpJobId = a})

-- | The name of the topic detection job.
tdjpJobName :: Lens' TopicsDetectionJobProperties (Maybe Text)
tdjpJobName = lens _tdjpJobName (\s a -> s {_tdjpJobName = a})

-- | The input data configuration supplied when you created the topic detection job.
tdjpInputDataConfig :: Lens' TopicsDetectionJobProperties (Maybe InputDataConfig)
tdjpInputDataConfig = lens _tdjpInputDataConfig (\s a -> s {_tdjpInputDataConfig = a})

-- | Configuration parameters for a private Virtual Private Cloud (VPC) containing the resources you are using for your topic detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
tdjpVPCConfig :: Lens' TopicsDetectionJobProperties (Maybe VPCConfig)
tdjpVPCConfig = lens _tdjpVPCConfig (\s a -> s {_tdjpVPCConfig = a})

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@      * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
tdjpVolumeKMSKeyId :: Lens' TopicsDetectionJobProperties (Maybe Text)
tdjpVolumeKMSKeyId = lens _tdjpVolumeKMSKeyId (\s a -> s {_tdjpVolumeKMSKeyId = a})

-- | The time that the topic detection job was completed.
tdjpEndTime :: Lens' TopicsDetectionJobProperties (Maybe UTCTime)
tdjpEndTime = lens _tdjpEndTime (\s a -> s {_tdjpEndTime = a}) . mapping _Time

-- | The output data configuration supplied when you created the topic detection job.
tdjpOutputDataConfig :: Lens' TopicsDetectionJobProperties (Maybe OutputDataConfig)
tdjpOutputDataConfig = lens _tdjpOutputDataConfig (\s a -> s {_tdjpOutputDataConfig = a})

-- | The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM) role that grants Amazon Comprehend read access to your job data.
tdjpDataAccessRoleARN :: Lens' TopicsDetectionJobProperties (Maybe Text)
tdjpDataAccessRoleARN = lens _tdjpDataAccessRoleARN (\s a -> s {_tdjpDataAccessRoleARN = a})

-- | The number of topics to detect supplied when you created the topic detection job. The default is 10.
tdjpNumberOfTopics :: Lens' TopicsDetectionJobProperties (Maybe Int)
tdjpNumberOfTopics = lens _tdjpNumberOfTopics (\s a -> s {_tdjpNumberOfTopics = a})

-- | The current status of the topic detection job. If the status is @Failed@ , the reason for the failure is shown in the @Message@ field.
tdjpJobStatus :: Lens' TopicsDetectionJobProperties (Maybe JobStatus)
tdjpJobStatus = lens _tdjpJobStatus (\s a -> s {_tdjpJobStatus = a})

-- | A description for the status of a job.
tdjpMessage :: Lens' TopicsDetectionJobProperties (Maybe Text)
tdjpMessage = lens _tdjpMessage (\s a -> s {_tdjpMessage = a})

-- | The time that the topic detection job was submitted for processing.
tdjpSubmitTime :: Lens' TopicsDetectionJobProperties (Maybe UTCTime)
tdjpSubmitTime = lens _tdjpSubmitTime (\s a -> s {_tdjpSubmitTime = a}) . mapping _Time

instance FromJSON TopicsDetectionJobProperties where
  parseJSON =
    withObject
      "TopicsDetectionJobProperties"
      ( \x ->
          TopicsDetectionJobProperties'
            <$> (x .:? "JobId")
            <*> (x .:? "JobName")
            <*> (x .:? "InputDataConfig")
            <*> (x .:? "VpcConfig")
            <*> (x .:? "VolumeKmsKeyId")
            <*> (x .:? "EndTime")
            <*> (x .:? "OutputDataConfig")
            <*> (x .:? "DataAccessRoleArn")
            <*> (x .:? "NumberOfTopics")
            <*> (x .:? "JobStatus")
            <*> (x .:? "Message")
            <*> (x .:? "SubmitTime")
      )

instance Hashable TopicsDetectionJobProperties

instance NFData TopicsDetectionJobProperties
