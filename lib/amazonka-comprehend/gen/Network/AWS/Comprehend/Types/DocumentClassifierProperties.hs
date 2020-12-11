-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.DocumentClassifierProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.DocumentClassifierProperties
  ( DocumentClassifierProperties (..),

    -- * Smart constructor
    mkDocumentClassifierProperties,

    -- * Lenses
    dcpStatus,
    dcpLanguageCode,
    dcpClassifierMetadata,
    dcpTrainingEndTime,
    dcpDocumentClassifierARN,
    dcpMode,
    dcpInputDataConfig,
    dcpVPCConfig,
    dcpVolumeKMSKeyId,
    dcpEndTime,
    dcpOutputDataConfig,
    dcpTrainingStartTime,
    dcpDataAccessRoleARN,
    dcpMessage,
    dcpSubmitTime,
  )
where

import Network.AWS.Comprehend.Types.ClassifierMetadata
import Network.AWS.Comprehend.Types.DocumentClassifierInputDataConfig
import Network.AWS.Comprehend.Types.DocumentClassifierMode
import Network.AWS.Comprehend.Types.DocumentClassifierOutputDataConfig
import Network.AWS.Comprehend.Types.LanguageCode
import Network.AWS.Comprehend.Types.ModelStatus
import Network.AWS.Comprehend.Types.VPCConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about a document classifier.
--
-- /See:/ 'mkDocumentClassifierProperties' smart constructor.
data DocumentClassifierProperties = DocumentClassifierProperties'
  { status ::
      Lude.Maybe ModelStatus,
    languageCode ::
      Lude.Maybe LanguageCode,
    classifierMetadata ::
      Lude.Maybe ClassifierMetadata,
    trainingEndTime ::
      Lude.Maybe Lude.Timestamp,
    documentClassifierARN ::
      Lude.Maybe Lude.Text,
    mode ::
      Lude.Maybe DocumentClassifierMode,
    inputDataConfig ::
      Lude.Maybe
        DocumentClassifierInputDataConfig,
    vpcConfig :: Lude.Maybe VPCConfig,
    volumeKMSKeyId ::
      Lude.Maybe Lude.Text,
    endTime ::
      Lude.Maybe Lude.Timestamp,
    outputDataConfig ::
      Lude.Maybe
        DocumentClassifierOutputDataConfig,
    trainingStartTime ::
      Lude.Maybe Lude.Timestamp,
    dataAccessRoleARN ::
      Lude.Maybe Lude.Text,
    message :: Lude.Maybe Lude.Text,
    submitTime ::
      Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DocumentClassifierProperties' with the minimum fields required to make a request.
--
-- * 'classifierMetadata' - Information about the document classifier, including the number of documents used for training the classifier, the number of documents used for test the classifier, and an accuracy rating.
-- * 'dataAccessRoleARN' - The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM) role that grants Amazon Comprehend read access to your input data.
-- * 'documentClassifierARN' - The Amazon Resource Name (ARN) that identifies the document classifier.
-- * 'endTime' - The time that training the document classifier completed.
-- * 'inputDataConfig' - The input data configuration that you supplied when you created the document classifier for training.
-- * 'languageCode' - The language code for the language of the documents that the classifier was trained on.
-- * 'message' - Additional information about the status of the classifier.
-- * 'mode' - Indicates the mode in which the specific classifier was trained. This also indicates the format of input documents and the format of the confusion matrix. Each classifier can only be trained in one mode and this cannot be changed once the classifier is trained.
-- * 'outputDataConfig' - Provides output results configuration parameters for custom classifier jobs.
-- * 'status' - The status of the document classifier. If the status is @TRAINED@ the classifier is ready to use. If the status is @FAILED@ you can see additional information about why the classifier wasn't trained in the @Message@ field.
-- * 'submitTime' - The time that the document classifier was submitted for training.
-- * 'trainingEndTime' - The time that training of the document classifier was completed. Indicates the time when the training completes on documentation classifiers. You are billed for the time interval between this time and the value of TrainingStartTime.
-- * 'trainingStartTime' - Indicates the time when the training starts on documentation classifiers. You are billed for the time interval between this time and the value of TrainingEndTime.
-- * 'volumeKMSKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:
--
--
--     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@
--
--
--     * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
--
--
-- * 'vpcConfig' - Configuration parameters for a private Virtual Private Cloud (VPC) containing the resources you are using for your custom classifier. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
mkDocumentClassifierProperties ::
  DocumentClassifierProperties
mkDocumentClassifierProperties =
  DocumentClassifierProperties'
    { status = Lude.Nothing,
      languageCode = Lude.Nothing,
      classifierMetadata = Lude.Nothing,
      trainingEndTime = Lude.Nothing,
      documentClassifierARN = Lude.Nothing,
      mode = Lude.Nothing,
      inputDataConfig = Lude.Nothing,
      vpcConfig = Lude.Nothing,
      volumeKMSKeyId = Lude.Nothing,
      endTime = Lude.Nothing,
      outputDataConfig = Lude.Nothing,
      trainingStartTime = Lude.Nothing,
      dataAccessRoleARN = Lude.Nothing,
      message = Lude.Nothing,
      submitTime = Lude.Nothing
    }

-- | The status of the document classifier. If the status is @TRAINED@ the classifier is ready to use. If the status is @FAILED@ you can see additional information about why the classifier wasn't trained in the @Message@ field.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpStatus :: Lens.Lens' DocumentClassifierProperties (Lude.Maybe ModelStatus)
dcpStatus = Lens.lens (status :: DocumentClassifierProperties -> Lude.Maybe ModelStatus) (\s a -> s {status = a} :: DocumentClassifierProperties)
{-# DEPRECATED dcpStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The language code for the language of the documents that the classifier was trained on.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpLanguageCode :: Lens.Lens' DocumentClassifierProperties (Lude.Maybe LanguageCode)
dcpLanguageCode = Lens.lens (languageCode :: DocumentClassifierProperties -> Lude.Maybe LanguageCode) (\s a -> s {languageCode = a} :: DocumentClassifierProperties)
{-# DEPRECATED dcpLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | Information about the document classifier, including the number of documents used for training the classifier, the number of documents used for test the classifier, and an accuracy rating.
--
-- /Note:/ Consider using 'classifierMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpClassifierMetadata :: Lens.Lens' DocumentClassifierProperties (Lude.Maybe ClassifierMetadata)
dcpClassifierMetadata = Lens.lens (classifierMetadata :: DocumentClassifierProperties -> Lude.Maybe ClassifierMetadata) (\s a -> s {classifierMetadata = a} :: DocumentClassifierProperties)
{-# DEPRECATED dcpClassifierMetadata "Use generic-lens or generic-optics with 'classifierMetadata' instead." #-}

-- | The time that training of the document classifier was completed. Indicates the time when the training completes on documentation classifiers. You are billed for the time interval between this time and the value of TrainingStartTime.
--
-- /Note:/ Consider using 'trainingEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpTrainingEndTime :: Lens.Lens' DocumentClassifierProperties (Lude.Maybe Lude.Timestamp)
dcpTrainingEndTime = Lens.lens (trainingEndTime :: DocumentClassifierProperties -> Lude.Maybe Lude.Timestamp) (\s a -> s {trainingEndTime = a} :: DocumentClassifierProperties)
{-# DEPRECATED dcpTrainingEndTime "Use generic-lens or generic-optics with 'trainingEndTime' instead." #-}

-- | The Amazon Resource Name (ARN) that identifies the document classifier.
--
-- /Note:/ Consider using 'documentClassifierARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpDocumentClassifierARN :: Lens.Lens' DocumentClassifierProperties (Lude.Maybe Lude.Text)
dcpDocumentClassifierARN = Lens.lens (documentClassifierARN :: DocumentClassifierProperties -> Lude.Maybe Lude.Text) (\s a -> s {documentClassifierARN = a} :: DocumentClassifierProperties)
{-# DEPRECATED dcpDocumentClassifierARN "Use generic-lens or generic-optics with 'documentClassifierARN' instead." #-}

-- | Indicates the mode in which the specific classifier was trained. This also indicates the format of input documents and the format of the confusion matrix. Each classifier can only be trained in one mode and this cannot be changed once the classifier is trained.
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpMode :: Lens.Lens' DocumentClassifierProperties (Lude.Maybe DocumentClassifierMode)
dcpMode = Lens.lens (mode :: DocumentClassifierProperties -> Lude.Maybe DocumentClassifierMode) (\s a -> s {mode = a} :: DocumentClassifierProperties)
{-# DEPRECATED dcpMode "Use generic-lens or generic-optics with 'mode' instead." #-}

-- | The input data configuration that you supplied when you created the document classifier for training.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpInputDataConfig :: Lens.Lens' DocumentClassifierProperties (Lude.Maybe DocumentClassifierInputDataConfig)
dcpInputDataConfig = Lens.lens (inputDataConfig :: DocumentClassifierProperties -> Lude.Maybe DocumentClassifierInputDataConfig) (\s a -> s {inputDataConfig = a} :: DocumentClassifierProperties)
{-# DEPRECATED dcpInputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead." #-}

-- | Configuration parameters for a private Virtual Private Cloud (VPC) containing the resources you are using for your custom classifier. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpVPCConfig :: Lens.Lens' DocumentClassifierProperties (Lude.Maybe VPCConfig)
dcpVPCConfig = Lens.lens (vpcConfig :: DocumentClassifierProperties -> Lude.Maybe VPCConfig) (\s a -> s {vpcConfig = a} :: DocumentClassifierProperties)
{-# DEPRECATED dcpVPCConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

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
dcpVolumeKMSKeyId :: Lens.Lens' DocumentClassifierProperties (Lude.Maybe Lude.Text)
dcpVolumeKMSKeyId = Lens.lens (volumeKMSKeyId :: DocumentClassifierProperties -> Lude.Maybe Lude.Text) (\s a -> s {volumeKMSKeyId = a} :: DocumentClassifierProperties)
{-# DEPRECATED dcpVolumeKMSKeyId "Use generic-lens or generic-optics with 'volumeKMSKeyId' instead." #-}

-- | The time that training the document classifier completed.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpEndTime :: Lens.Lens' DocumentClassifierProperties (Lude.Maybe Lude.Timestamp)
dcpEndTime = Lens.lens (endTime :: DocumentClassifierProperties -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: DocumentClassifierProperties)
{-# DEPRECATED dcpEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | Provides output results configuration parameters for custom classifier jobs.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpOutputDataConfig :: Lens.Lens' DocumentClassifierProperties (Lude.Maybe DocumentClassifierOutputDataConfig)
dcpOutputDataConfig = Lens.lens (outputDataConfig :: DocumentClassifierProperties -> Lude.Maybe DocumentClassifierOutputDataConfig) (\s a -> s {outputDataConfig = a} :: DocumentClassifierProperties)
{-# DEPRECATED dcpOutputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead." #-}

-- | Indicates the time when the training starts on documentation classifiers. You are billed for the time interval between this time and the value of TrainingEndTime.
--
-- /Note:/ Consider using 'trainingStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpTrainingStartTime :: Lens.Lens' DocumentClassifierProperties (Lude.Maybe Lude.Timestamp)
dcpTrainingStartTime = Lens.lens (trainingStartTime :: DocumentClassifierProperties -> Lude.Maybe Lude.Timestamp) (\s a -> s {trainingStartTime = a} :: DocumentClassifierProperties)
{-# DEPRECATED dcpTrainingStartTime "Use generic-lens or generic-optics with 'trainingStartTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM) role that grants Amazon Comprehend read access to your input data.
--
-- /Note:/ Consider using 'dataAccessRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpDataAccessRoleARN :: Lens.Lens' DocumentClassifierProperties (Lude.Maybe Lude.Text)
dcpDataAccessRoleARN = Lens.lens (dataAccessRoleARN :: DocumentClassifierProperties -> Lude.Maybe Lude.Text) (\s a -> s {dataAccessRoleARN = a} :: DocumentClassifierProperties)
{-# DEPRECATED dcpDataAccessRoleARN "Use generic-lens or generic-optics with 'dataAccessRoleARN' instead." #-}

-- | Additional information about the status of the classifier.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpMessage :: Lens.Lens' DocumentClassifierProperties (Lude.Maybe Lude.Text)
dcpMessage = Lens.lens (message :: DocumentClassifierProperties -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: DocumentClassifierProperties)
{-# DEPRECATED dcpMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The time that the document classifier was submitted for training.
--
-- /Note:/ Consider using 'submitTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpSubmitTime :: Lens.Lens' DocumentClassifierProperties (Lude.Maybe Lude.Timestamp)
dcpSubmitTime = Lens.lens (submitTime :: DocumentClassifierProperties -> Lude.Maybe Lude.Timestamp) (\s a -> s {submitTime = a} :: DocumentClassifierProperties)
{-# DEPRECATED dcpSubmitTime "Use generic-lens or generic-optics with 'submitTime' instead." #-}

instance Lude.FromJSON DocumentClassifierProperties where
  parseJSON =
    Lude.withObject
      "DocumentClassifierProperties"
      ( \x ->
          DocumentClassifierProperties'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "LanguageCode")
            Lude.<*> (x Lude..:? "ClassifierMetadata")
            Lude.<*> (x Lude..:? "TrainingEndTime")
            Lude.<*> (x Lude..:? "DocumentClassifierArn")
            Lude.<*> (x Lude..:? "Mode")
            Lude.<*> (x Lude..:? "InputDataConfig")
            Lude.<*> (x Lude..:? "VpcConfig")
            Lude.<*> (x Lude..:? "VolumeKmsKeyId")
            Lude.<*> (x Lude..:? "EndTime")
            Lude.<*> (x Lude..:? "OutputDataConfig")
            Lude.<*> (x Lude..:? "TrainingStartTime")
            Lude.<*> (x Lude..:? "DataAccessRoleArn")
            Lude.<*> (x Lude..:? "Message")
            Lude.<*> (x Lude..:? "SubmitTime")
      )
