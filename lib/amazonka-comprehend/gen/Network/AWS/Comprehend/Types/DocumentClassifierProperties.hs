{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.DocumentClassifierProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.DocumentClassifierProperties where

import Network.AWS.Comprehend.Types.ClassifierMetadata
import Network.AWS.Comprehend.Types.DocumentClassifierInputDataConfig
import Network.AWS.Comprehend.Types.DocumentClassifierMode
import Network.AWS.Comprehend.Types.DocumentClassifierOutputDataConfig
import Network.AWS.Comprehend.Types.LanguageCode
import Network.AWS.Comprehend.Types.ModelStatus
import Network.AWS.Comprehend.Types.VPCConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information about a document classifier.
--
--
--
-- /See:/ 'documentClassifierProperties' smart constructor.
data DocumentClassifierProperties = DocumentClassifierProperties'
  { _dcpStatus ::
      !(Maybe ModelStatus),
    _dcpLanguageCode ::
      !(Maybe LanguageCode),
    _dcpClassifierMetadata ::
      !( Maybe
           ( Sensitive
               ClassifierMetadata
           )
       ),
    _dcpTrainingEndTime ::
      !(Maybe POSIX),
    _dcpDocumentClassifierARN ::
      !(Maybe Text),
    _dcpMode ::
      !(Maybe DocumentClassifierMode),
    _dcpInputDataConfig ::
      !( Maybe
           DocumentClassifierInputDataConfig
       ),
    _dcpVPCConfig ::
      !(Maybe VPCConfig),
    _dcpVolumeKMSKeyId ::
      !(Maybe Text),
    _dcpEndTime :: !(Maybe POSIX),
    _dcpOutputDataConfig ::
      !( Maybe
           DocumentClassifierOutputDataConfig
       ),
    _dcpTrainingStartTime ::
      !(Maybe POSIX),
    _dcpDataAccessRoleARN ::
      !(Maybe Text),
    _dcpMessage :: !(Maybe Text),
    _dcpSubmitTime :: !(Maybe POSIX)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'DocumentClassifierProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcpStatus' - The status of the document classifier. If the status is @TRAINED@ the classifier is ready to use. If the status is @FAILED@ you can see additional information about why the classifier wasn't trained in the @Message@ field.
--
-- * 'dcpLanguageCode' - The language code for the language of the documents that the classifier was trained on.
--
-- * 'dcpClassifierMetadata' - Information about the document classifier, including the number of documents used for training the classifier, the number of documents used for test the classifier, and an accuracy rating.
--
-- * 'dcpTrainingEndTime' - The time that training of the document classifier was completed. Indicates the time when the training completes on documentation classifiers. You are billed for the time interval between this time and the value of TrainingStartTime.
--
-- * 'dcpDocumentClassifierARN' - The Amazon Resource Name (ARN) that identifies the document classifier.
--
-- * 'dcpMode' - Indicates the mode in which the specific classifier was trained. This also indicates the format of input documents and the format of the confusion matrix. Each classifier can only be trained in one mode and this cannot be changed once the classifier is trained.
--
-- * 'dcpInputDataConfig' - The input data configuration that you supplied when you created the document classifier for training.
--
-- * 'dcpVPCConfig' - Configuration parameters for a private Virtual Private Cloud (VPC) containing the resources you are using for your custom classifier. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
--
-- * 'dcpVolumeKMSKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@      * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
--
-- * 'dcpEndTime' - The time that training the document classifier completed.
--
-- * 'dcpOutputDataConfig' - Provides output results configuration parameters for custom classifier jobs.
--
-- * 'dcpTrainingStartTime' - Indicates the time when the training starts on documentation classifiers. You are billed for the time interval between this time and the value of TrainingEndTime.
--
-- * 'dcpDataAccessRoleARN' - The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM) role that grants Amazon Comprehend read access to your input data.
--
-- * 'dcpMessage' - Additional information about the status of the classifier.
--
-- * 'dcpSubmitTime' - The time that the document classifier was submitted for training.
documentClassifierProperties ::
  DocumentClassifierProperties
documentClassifierProperties =
  DocumentClassifierProperties'
    { _dcpStatus = Nothing,
      _dcpLanguageCode = Nothing,
      _dcpClassifierMetadata = Nothing,
      _dcpTrainingEndTime = Nothing,
      _dcpDocumentClassifierARN = Nothing,
      _dcpMode = Nothing,
      _dcpInputDataConfig = Nothing,
      _dcpVPCConfig = Nothing,
      _dcpVolumeKMSKeyId = Nothing,
      _dcpEndTime = Nothing,
      _dcpOutputDataConfig = Nothing,
      _dcpTrainingStartTime = Nothing,
      _dcpDataAccessRoleARN = Nothing,
      _dcpMessage = Nothing,
      _dcpSubmitTime = Nothing
    }

-- | The status of the document classifier. If the status is @TRAINED@ the classifier is ready to use. If the status is @FAILED@ you can see additional information about why the classifier wasn't trained in the @Message@ field.
dcpStatus :: Lens' DocumentClassifierProperties (Maybe ModelStatus)
dcpStatus = lens _dcpStatus (\s a -> s {_dcpStatus = a})

-- | The language code for the language of the documents that the classifier was trained on.
dcpLanguageCode :: Lens' DocumentClassifierProperties (Maybe LanguageCode)
dcpLanguageCode = lens _dcpLanguageCode (\s a -> s {_dcpLanguageCode = a})

-- | Information about the document classifier, including the number of documents used for training the classifier, the number of documents used for test the classifier, and an accuracy rating.
dcpClassifierMetadata :: Lens' DocumentClassifierProperties (Maybe ClassifierMetadata)
dcpClassifierMetadata = lens _dcpClassifierMetadata (\s a -> s {_dcpClassifierMetadata = a}) . mapping _Sensitive

-- | The time that training of the document classifier was completed. Indicates the time when the training completes on documentation classifiers. You are billed for the time interval between this time and the value of TrainingStartTime.
dcpTrainingEndTime :: Lens' DocumentClassifierProperties (Maybe UTCTime)
dcpTrainingEndTime = lens _dcpTrainingEndTime (\s a -> s {_dcpTrainingEndTime = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) that identifies the document classifier.
dcpDocumentClassifierARN :: Lens' DocumentClassifierProperties (Maybe Text)
dcpDocumentClassifierARN = lens _dcpDocumentClassifierARN (\s a -> s {_dcpDocumentClassifierARN = a})

-- | Indicates the mode in which the specific classifier was trained. This also indicates the format of input documents and the format of the confusion matrix. Each classifier can only be trained in one mode and this cannot be changed once the classifier is trained.
dcpMode :: Lens' DocumentClassifierProperties (Maybe DocumentClassifierMode)
dcpMode = lens _dcpMode (\s a -> s {_dcpMode = a})

-- | The input data configuration that you supplied when you created the document classifier for training.
dcpInputDataConfig :: Lens' DocumentClassifierProperties (Maybe DocumentClassifierInputDataConfig)
dcpInputDataConfig = lens _dcpInputDataConfig (\s a -> s {_dcpInputDataConfig = a})

-- | Configuration parameters for a private Virtual Private Cloud (VPC) containing the resources you are using for your custom classifier. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
dcpVPCConfig :: Lens' DocumentClassifierProperties (Maybe VPCConfig)
dcpVPCConfig = lens _dcpVPCConfig (\s a -> s {_dcpVPCConfig = a})

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@      * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
dcpVolumeKMSKeyId :: Lens' DocumentClassifierProperties (Maybe Text)
dcpVolumeKMSKeyId = lens _dcpVolumeKMSKeyId (\s a -> s {_dcpVolumeKMSKeyId = a})

-- | The time that training the document classifier completed.
dcpEndTime :: Lens' DocumentClassifierProperties (Maybe UTCTime)
dcpEndTime = lens _dcpEndTime (\s a -> s {_dcpEndTime = a}) . mapping _Time

-- | Provides output results configuration parameters for custom classifier jobs.
dcpOutputDataConfig :: Lens' DocumentClassifierProperties (Maybe DocumentClassifierOutputDataConfig)
dcpOutputDataConfig = lens _dcpOutputDataConfig (\s a -> s {_dcpOutputDataConfig = a})

-- | Indicates the time when the training starts on documentation classifiers. You are billed for the time interval between this time and the value of TrainingEndTime.
dcpTrainingStartTime :: Lens' DocumentClassifierProperties (Maybe UTCTime)
dcpTrainingStartTime = lens _dcpTrainingStartTime (\s a -> s {_dcpTrainingStartTime = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM) role that grants Amazon Comprehend read access to your input data.
dcpDataAccessRoleARN :: Lens' DocumentClassifierProperties (Maybe Text)
dcpDataAccessRoleARN = lens _dcpDataAccessRoleARN (\s a -> s {_dcpDataAccessRoleARN = a})

-- | Additional information about the status of the classifier.
dcpMessage :: Lens' DocumentClassifierProperties (Maybe Text)
dcpMessage = lens _dcpMessage (\s a -> s {_dcpMessage = a})

-- | The time that the document classifier was submitted for training.
dcpSubmitTime :: Lens' DocumentClassifierProperties (Maybe UTCTime)
dcpSubmitTime = lens _dcpSubmitTime (\s a -> s {_dcpSubmitTime = a}) . mapping _Time

instance FromJSON DocumentClassifierProperties where
  parseJSON =
    withObject
      "DocumentClassifierProperties"
      ( \x ->
          DocumentClassifierProperties'
            <$> (x .:? "Status")
            <*> (x .:? "LanguageCode")
            <*> (x .:? "ClassifierMetadata")
            <*> (x .:? "TrainingEndTime")
            <*> (x .:? "DocumentClassifierArn")
            <*> (x .:? "Mode")
            <*> (x .:? "InputDataConfig")
            <*> (x .:? "VpcConfig")
            <*> (x .:? "VolumeKmsKeyId")
            <*> (x .:? "EndTime")
            <*> (x .:? "OutputDataConfig")
            <*> (x .:? "TrainingStartTime")
            <*> (x .:? "DataAccessRoleArn")
            <*> (x .:? "Message")
            <*> (x .:? "SubmitTime")
      )

instance Hashable DocumentClassifierProperties

instance NFData DocumentClassifierProperties
