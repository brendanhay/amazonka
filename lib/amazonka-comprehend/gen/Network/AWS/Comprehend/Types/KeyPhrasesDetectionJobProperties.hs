{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.KeyPhrasesDetectionJobProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.KeyPhrasesDetectionJobProperties where

import Network.AWS.Comprehend.Types.InputDataConfig
import Network.AWS.Comprehend.Types.JobStatus
import Network.AWS.Comprehend.Types.LanguageCode
import Network.AWS.Comprehend.Types.OutputDataConfig
import Network.AWS.Comprehend.Types.VPCConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information about a key phrases detection job.
--
--
--
-- /See:/ 'keyPhrasesDetectionJobProperties' smart constructor.
data KeyPhrasesDetectionJobProperties = KeyPhrasesDetectionJobProperties'
  { _kpdjpLanguageCode ::
      !(Maybe LanguageCode),
    _kpdjpJobId ::
      !(Maybe Text),
    _kpdjpJobName ::
      !(Maybe Text),
    _kpdjpInputDataConfig ::
      !(Maybe InputDataConfig),
    _kpdjpVPCConfig ::
      !(Maybe VPCConfig),
    _kpdjpVolumeKMSKeyId ::
      !(Maybe Text),
    _kpdjpEndTime ::
      !(Maybe POSIX),
    _kpdjpOutputDataConfig ::
      !(Maybe OutputDataConfig),
    _kpdjpDataAccessRoleARN ::
      !(Maybe Text),
    _kpdjpJobStatus ::
      !(Maybe JobStatus),
    _kpdjpMessage ::
      !(Maybe Text),
    _kpdjpSubmitTime ::
      !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KeyPhrasesDetectionJobProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kpdjpLanguageCode' - The language code of the input documents.
--
-- * 'kpdjpJobId' - The identifier assigned to the key phrases detection job.
--
-- * 'kpdjpJobName' - The name that you assigned the key phrases detection job.
--
-- * 'kpdjpInputDataConfig' - The input data configuration that you supplied when you created the key phrases detection job.
--
-- * 'kpdjpVPCConfig' - Configuration parameters for a private Virtual Private Cloud (VPC) containing the resources you are using for your key phrases detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
--
-- * 'kpdjpVolumeKMSKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@      * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
--
-- * 'kpdjpEndTime' - The time that the key phrases detection job completed.
--
-- * 'kpdjpOutputDataConfig' - The output data configuration that you supplied when you created the key phrases detection job.
--
-- * 'kpdjpDataAccessRoleARN' - The Amazon Resource Name (ARN) that gives Amazon Comprehend read access to your input data.
--
-- * 'kpdjpJobStatus' - The current status of the key phrases detection job. If the status is @FAILED@ , the @Message@ field shows the reason for the failure.
--
-- * 'kpdjpMessage' - A description of the status of a job.
--
-- * 'kpdjpSubmitTime' - The time that the key phrases detection job was submitted for processing.
keyPhrasesDetectionJobProperties ::
  KeyPhrasesDetectionJobProperties
keyPhrasesDetectionJobProperties =
  KeyPhrasesDetectionJobProperties'
    { _kpdjpLanguageCode = Nothing,
      _kpdjpJobId = Nothing,
      _kpdjpJobName = Nothing,
      _kpdjpInputDataConfig = Nothing,
      _kpdjpVPCConfig = Nothing,
      _kpdjpVolumeKMSKeyId = Nothing,
      _kpdjpEndTime = Nothing,
      _kpdjpOutputDataConfig = Nothing,
      _kpdjpDataAccessRoleARN = Nothing,
      _kpdjpJobStatus = Nothing,
      _kpdjpMessage = Nothing,
      _kpdjpSubmitTime = Nothing
    }

-- | The language code of the input documents.
kpdjpLanguageCode :: Lens' KeyPhrasesDetectionJobProperties (Maybe LanguageCode)
kpdjpLanguageCode = lens _kpdjpLanguageCode (\s a -> s {_kpdjpLanguageCode = a})

-- | The identifier assigned to the key phrases detection job.
kpdjpJobId :: Lens' KeyPhrasesDetectionJobProperties (Maybe Text)
kpdjpJobId = lens _kpdjpJobId (\s a -> s {_kpdjpJobId = a})

-- | The name that you assigned the key phrases detection job.
kpdjpJobName :: Lens' KeyPhrasesDetectionJobProperties (Maybe Text)
kpdjpJobName = lens _kpdjpJobName (\s a -> s {_kpdjpJobName = a})

-- | The input data configuration that you supplied when you created the key phrases detection job.
kpdjpInputDataConfig :: Lens' KeyPhrasesDetectionJobProperties (Maybe InputDataConfig)
kpdjpInputDataConfig = lens _kpdjpInputDataConfig (\s a -> s {_kpdjpInputDataConfig = a})

-- | Configuration parameters for a private Virtual Private Cloud (VPC) containing the resources you are using for your key phrases detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
kpdjpVPCConfig :: Lens' KeyPhrasesDetectionJobProperties (Maybe VPCConfig)
kpdjpVPCConfig = lens _kpdjpVPCConfig (\s a -> s {_kpdjpVPCConfig = a})

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@      * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
kpdjpVolumeKMSKeyId :: Lens' KeyPhrasesDetectionJobProperties (Maybe Text)
kpdjpVolumeKMSKeyId = lens _kpdjpVolumeKMSKeyId (\s a -> s {_kpdjpVolumeKMSKeyId = a})

-- | The time that the key phrases detection job completed.
kpdjpEndTime :: Lens' KeyPhrasesDetectionJobProperties (Maybe UTCTime)
kpdjpEndTime = lens _kpdjpEndTime (\s a -> s {_kpdjpEndTime = a}) . mapping _Time

-- | The output data configuration that you supplied when you created the key phrases detection job.
kpdjpOutputDataConfig :: Lens' KeyPhrasesDetectionJobProperties (Maybe OutputDataConfig)
kpdjpOutputDataConfig = lens _kpdjpOutputDataConfig (\s a -> s {_kpdjpOutputDataConfig = a})

-- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access to your input data.
kpdjpDataAccessRoleARN :: Lens' KeyPhrasesDetectionJobProperties (Maybe Text)
kpdjpDataAccessRoleARN = lens _kpdjpDataAccessRoleARN (\s a -> s {_kpdjpDataAccessRoleARN = a})

-- | The current status of the key phrases detection job. If the status is @FAILED@ , the @Message@ field shows the reason for the failure.
kpdjpJobStatus :: Lens' KeyPhrasesDetectionJobProperties (Maybe JobStatus)
kpdjpJobStatus = lens _kpdjpJobStatus (\s a -> s {_kpdjpJobStatus = a})

-- | A description of the status of a job.
kpdjpMessage :: Lens' KeyPhrasesDetectionJobProperties (Maybe Text)
kpdjpMessage = lens _kpdjpMessage (\s a -> s {_kpdjpMessage = a})

-- | The time that the key phrases detection job was submitted for processing.
kpdjpSubmitTime :: Lens' KeyPhrasesDetectionJobProperties (Maybe UTCTime)
kpdjpSubmitTime = lens _kpdjpSubmitTime (\s a -> s {_kpdjpSubmitTime = a}) . mapping _Time

instance FromJSON KeyPhrasesDetectionJobProperties where
  parseJSON =
    withObject
      "KeyPhrasesDetectionJobProperties"
      ( \x ->
          KeyPhrasesDetectionJobProperties'
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

instance Hashable KeyPhrasesDetectionJobProperties

instance NFData KeyPhrasesDetectionJobProperties
