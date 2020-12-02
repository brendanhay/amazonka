{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.DocumentClassificationJobProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.DocumentClassificationJobProperties where

import Network.AWS.Comprehend.Types.InputDataConfig
import Network.AWS.Comprehend.Types.JobStatus
import Network.AWS.Comprehend.Types.OutputDataConfig
import Network.AWS.Comprehend.Types.VPCConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information about a document classification job.
--
--
--
-- /See:/ 'documentClassificationJobProperties' smart constructor.
data DocumentClassificationJobProperties = DocumentClassificationJobProperties'
  { _dcjpJobId ::
      !(Maybe Text),
    _dcjpDocumentClassifierARN ::
      !(Maybe Text),
    _dcjpJobName ::
      !(Maybe Text),
    _dcjpInputDataConfig ::
      !( Maybe
           InputDataConfig
       ),
    _dcjpVPCConfig ::
      !(Maybe VPCConfig),
    _dcjpVolumeKMSKeyId ::
      !(Maybe Text),
    _dcjpEndTime ::
      !(Maybe POSIX),
    _dcjpOutputDataConfig ::
      !( Maybe
           OutputDataConfig
       ),
    _dcjpDataAccessRoleARN ::
      !(Maybe Text),
    _dcjpJobStatus ::
      !(Maybe JobStatus),
    _dcjpMessage ::
      !(Maybe Text),
    _dcjpSubmitTime ::
      !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DocumentClassificationJobProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcjpJobId' - The identifier assigned to the document classification job.
--
-- * 'dcjpDocumentClassifierARN' - The Amazon Resource Name (ARN) that identifies the document classifier.
--
-- * 'dcjpJobName' - The name that you assigned to the document classification job.
--
-- * 'dcjpInputDataConfig' - The input data configuration that you supplied when you created the document classification job.
--
-- * 'dcjpVPCConfig' - Configuration parameters for a private Virtual Private Cloud (VPC) containing the resources you are using for your document classification job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
--
-- * 'dcjpVolumeKMSKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@      * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
--
-- * 'dcjpEndTime' - The time that the document classification job completed.
--
-- * 'dcjpOutputDataConfig' - The output data configuration that you supplied when you created the document classification job.
--
-- * 'dcjpDataAccessRoleARN' - The Amazon Resource Name (ARN) of the AWS identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data.
--
-- * 'dcjpJobStatus' - The current status of the document classification job. If the status is @FAILED@ , the @Message@ field shows the reason for the failure.
--
-- * 'dcjpMessage' - A description of the status of the job.
--
-- * 'dcjpSubmitTime' - The time that the document classification job was submitted for processing.
documentClassificationJobProperties ::
  DocumentClassificationJobProperties
documentClassificationJobProperties =
  DocumentClassificationJobProperties'
    { _dcjpJobId = Nothing,
      _dcjpDocumentClassifierARN = Nothing,
      _dcjpJobName = Nothing,
      _dcjpInputDataConfig = Nothing,
      _dcjpVPCConfig = Nothing,
      _dcjpVolumeKMSKeyId = Nothing,
      _dcjpEndTime = Nothing,
      _dcjpOutputDataConfig = Nothing,
      _dcjpDataAccessRoleARN = Nothing,
      _dcjpJobStatus = Nothing,
      _dcjpMessage = Nothing,
      _dcjpSubmitTime = Nothing
    }

-- | The identifier assigned to the document classification job.
dcjpJobId :: Lens' DocumentClassificationJobProperties (Maybe Text)
dcjpJobId = lens _dcjpJobId (\s a -> s {_dcjpJobId = a})

-- | The Amazon Resource Name (ARN) that identifies the document classifier.
dcjpDocumentClassifierARN :: Lens' DocumentClassificationJobProperties (Maybe Text)
dcjpDocumentClassifierARN = lens _dcjpDocumentClassifierARN (\s a -> s {_dcjpDocumentClassifierARN = a})

-- | The name that you assigned to the document classification job.
dcjpJobName :: Lens' DocumentClassificationJobProperties (Maybe Text)
dcjpJobName = lens _dcjpJobName (\s a -> s {_dcjpJobName = a})

-- | The input data configuration that you supplied when you created the document classification job.
dcjpInputDataConfig :: Lens' DocumentClassificationJobProperties (Maybe InputDataConfig)
dcjpInputDataConfig = lens _dcjpInputDataConfig (\s a -> s {_dcjpInputDataConfig = a})

-- | Configuration parameters for a private Virtual Private Cloud (VPC) containing the resources you are using for your document classification job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
dcjpVPCConfig :: Lens' DocumentClassificationJobProperties (Maybe VPCConfig)
dcjpVPCConfig = lens _dcjpVPCConfig (\s a -> s {_dcjpVPCConfig = a})

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@      * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
dcjpVolumeKMSKeyId :: Lens' DocumentClassificationJobProperties (Maybe Text)
dcjpVolumeKMSKeyId = lens _dcjpVolumeKMSKeyId (\s a -> s {_dcjpVolumeKMSKeyId = a})

-- | The time that the document classification job completed.
dcjpEndTime :: Lens' DocumentClassificationJobProperties (Maybe UTCTime)
dcjpEndTime = lens _dcjpEndTime (\s a -> s {_dcjpEndTime = a}) . mapping _Time

-- | The output data configuration that you supplied when you created the document classification job.
dcjpOutputDataConfig :: Lens' DocumentClassificationJobProperties (Maybe OutputDataConfig)
dcjpOutputDataConfig = lens _dcjpOutputDataConfig (\s a -> s {_dcjpOutputDataConfig = a})

-- | The Amazon Resource Name (ARN) of the AWS identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data.
dcjpDataAccessRoleARN :: Lens' DocumentClassificationJobProperties (Maybe Text)
dcjpDataAccessRoleARN = lens _dcjpDataAccessRoleARN (\s a -> s {_dcjpDataAccessRoleARN = a})

-- | The current status of the document classification job. If the status is @FAILED@ , the @Message@ field shows the reason for the failure.
dcjpJobStatus :: Lens' DocumentClassificationJobProperties (Maybe JobStatus)
dcjpJobStatus = lens _dcjpJobStatus (\s a -> s {_dcjpJobStatus = a})

-- | A description of the status of the job.
dcjpMessage :: Lens' DocumentClassificationJobProperties (Maybe Text)
dcjpMessage = lens _dcjpMessage (\s a -> s {_dcjpMessage = a})

-- | The time that the document classification job was submitted for processing.
dcjpSubmitTime :: Lens' DocumentClassificationJobProperties (Maybe UTCTime)
dcjpSubmitTime = lens _dcjpSubmitTime (\s a -> s {_dcjpSubmitTime = a}) . mapping _Time

instance FromJSON DocumentClassificationJobProperties where
  parseJSON =
    withObject
      "DocumentClassificationJobProperties"
      ( \x ->
          DocumentClassificationJobProperties'
            <$> (x .:? "JobId")
            <*> (x .:? "DocumentClassifierArn")
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

instance Hashable DocumentClassificationJobProperties

instance NFData DocumentClassificationJobProperties
