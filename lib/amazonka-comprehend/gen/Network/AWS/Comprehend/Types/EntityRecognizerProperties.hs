-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EntityRecognizerProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EntityRecognizerProperties
  ( EntityRecognizerProperties (..),

    -- * Smart constructor
    mkEntityRecognizerProperties,

    -- * Lenses
    erpStatus,
    erpLanguageCode,
    erpTrainingEndTime,
    erpEntityRecognizerARN,
    erpInputDataConfig,
    erpVPCConfig,
    erpVolumeKMSKeyId,
    erpEndTime,
    erpTrainingStartTime,
    erpDataAccessRoleARN,
    erpRecognizerMetadata,
    erpMessage,
    erpSubmitTime,
  )
where

import Network.AWS.Comprehend.Types.EntityRecognizerInputDataConfig
import Network.AWS.Comprehend.Types.EntityRecognizerMetadata
import Network.AWS.Comprehend.Types.LanguageCode
import Network.AWS.Comprehend.Types.ModelStatus
import Network.AWS.Comprehend.Types.VPCConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes information about an entity recognizer.
--
-- /See:/ 'mkEntityRecognizerProperties' smart constructor.
data EntityRecognizerProperties = EntityRecognizerProperties'
  { status ::
      Lude.Maybe ModelStatus,
    languageCode ::
      Lude.Maybe LanguageCode,
    trainingEndTime ::
      Lude.Maybe Lude.Timestamp,
    entityRecognizerARN ::
      Lude.Maybe Lude.Text,
    inputDataConfig ::
      Lude.Maybe
        EntityRecognizerInputDataConfig,
    vpcConfig :: Lude.Maybe VPCConfig,
    volumeKMSKeyId ::
      Lude.Maybe Lude.Text,
    endTime :: Lude.Maybe Lude.Timestamp,
    trainingStartTime ::
      Lude.Maybe Lude.Timestamp,
    dataAccessRoleARN ::
      Lude.Maybe Lude.Text,
    recognizerMetadata ::
      Lude.Maybe EntityRecognizerMetadata,
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

-- | Creates a value of 'EntityRecognizerProperties' with the minimum fields required to make a request.
--
-- * 'dataAccessRoleARN' - The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM) role that grants Amazon Comprehend read access to your input data.
-- * 'endTime' - The time that the recognizer creation completed.
-- * 'entityRecognizerARN' - The Amazon Resource Name (ARN) that identifies the entity recognizer.
-- * 'inputDataConfig' - The input data properties of an entity recognizer.
-- * 'languageCode' - The language of the input documents. All documents must be in the same language. Only English ("en") is currently supported.
-- * 'message' - A description of the status of the recognizer.
-- * 'recognizerMetadata' - Provides information about an entity recognizer.
-- * 'status' - Provides the status of the entity recognizer.
-- * 'submitTime' - The time that the recognizer was submitted for processing.
-- * 'trainingEndTime' - The time that training of the entity recognizer was completed.
-- * 'trainingStartTime' - The time that training of the entity recognizer started.
-- * 'volumeKMSKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:
--
--
--     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@
--
--
--     * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
--
--
-- * 'vpcConfig' - Configuration parameters for a private Virtual Private Cloud (VPC) containing the resources you are using for your custom entity recognizer. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
mkEntityRecognizerProperties ::
  EntityRecognizerProperties
mkEntityRecognizerProperties =
  EntityRecognizerProperties'
    { status = Lude.Nothing,
      languageCode = Lude.Nothing,
      trainingEndTime = Lude.Nothing,
      entityRecognizerARN = Lude.Nothing,
      inputDataConfig = Lude.Nothing,
      vpcConfig = Lude.Nothing,
      volumeKMSKeyId = Lude.Nothing,
      endTime = Lude.Nothing,
      trainingStartTime = Lude.Nothing,
      dataAccessRoleARN = Lude.Nothing,
      recognizerMetadata = Lude.Nothing,
      message = Lude.Nothing,
      submitTime = Lude.Nothing
    }

-- | Provides the status of the entity recognizer.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erpStatus :: Lens.Lens' EntityRecognizerProperties (Lude.Maybe ModelStatus)
erpStatus = Lens.lens (status :: EntityRecognizerProperties -> Lude.Maybe ModelStatus) (\s a -> s {status = a} :: EntityRecognizerProperties)
{-# DEPRECATED erpStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The language of the input documents. All documents must be in the same language. Only English ("en") is currently supported.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erpLanguageCode :: Lens.Lens' EntityRecognizerProperties (Lude.Maybe LanguageCode)
erpLanguageCode = Lens.lens (languageCode :: EntityRecognizerProperties -> Lude.Maybe LanguageCode) (\s a -> s {languageCode = a} :: EntityRecognizerProperties)
{-# DEPRECATED erpLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | The time that training of the entity recognizer was completed.
--
-- /Note:/ Consider using 'trainingEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erpTrainingEndTime :: Lens.Lens' EntityRecognizerProperties (Lude.Maybe Lude.Timestamp)
erpTrainingEndTime = Lens.lens (trainingEndTime :: EntityRecognizerProperties -> Lude.Maybe Lude.Timestamp) (\s a -> s {trainingEndTime = a} :: EntityRecognizerProperties)
{-# DEPRECATED erpTrainingEndTime "Use generic-lens or generic-optics with 'trainingEndTime' instead." #-}

-- | The Amazon Resource Name (ARN) that identifies the entity recognizer.
--
-- /Note:/ Consider using 'entityRecognizerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erpEntityRecognizerARN :: Lens.Lens' EntityRecognizerProperties (Lude.Maybe Lude.Text)
erpEntityRecognizerARN = Lens.lens (entityRecognizerARN :: EntityRecognizerProperties -> Lude.Maybe Lude.Text) (\s a -> s {entityRecognizerARN = a} :: EntityRecognizerProperties)
{-# DEPRECATED erpEntityRecognizerARN "Use generic-lens or generic-optics with 'entityRecognizerARN' instead." #-}

-- | The input data properties of an entity recognizer.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erpInputDataConfig :: Lens.Lens' EntityRecognizerProperties (Lude.Maybe EntityRecognizerInputDataConfig)
erpInputDataConfig = Lens.lens (inputDataConfig :: EntityRecognizerProperties -> Lude.Maybe EntityRecognizerInputDataConfig) (\s a -> s {inputDataConfig = a} :: EntityRecognizerProperties)
{-# DEPRECATED erpInputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead." #-}

-- | Configuration parameters for a private Virtual Private Cloud (VPC) containing the resources you are using for your custom entity recognizer. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erpVPCConfig :: Lens.Lens' EntityRecognizerProperties (Lude.Maybe VPCConfig)
erpVPCConfig = Lens.lens (vpcConfig :: EntityRecognizerProperties -> Lude.Maybe VPCConfig) (\s a -> s {vpcConfig = a} :: EntityRecognizerProperties)
{-# DEPRECATED erpVPCConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

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
erpVolumeKMSKeyId :: Lens.Lens' EntityRecognizerProperties (Lude.Maybe Lude.Text)
erpVolumeKMSKeyId = Lens.lens (volumeKMSKeyId :: EntityRecognizerProperties -> Lude.Maybe Lude.Text) (\s a -> s {volumeKMSKeyId = a} :: EntityRecognizerProperties)
{-# DEPRECATED erpVolumeKMSKeyId "Use generic-lens or generic-optics with 'volumeKMSKeyId' instead." #-}

-- | The time that the recognizer creation completed.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erpEndTime :: Lens.Lens' EntityRecognizerProperties (Lude.Maybe Lude.Timestamp)
erpEndTime = Lens.lens (endTime :: EntityRecognizerProperties -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: EntityRecognizerProperties)
{-# DEPRECATED erpEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The time that training of the entity recognizer started.
--
-- /Note:/ Consider using 'trainingStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erpTrainingStartTime :: Lens.Lens' EntityRecognizerProperties (Lude.Maybe Lude.Timestamp)
erpTrainingStartTime = Lens.lens (trainingStartTime :: EntityRecognizerProperties -> Lude.Maybe Lude.Timestamp) (\s a -> s {trainingStartTime = a} :: EntityRecognizerProperties)
{-# DEPRECATED erpTrainingStartTime "Use generic-lens or generic-optics with 'trainingStartTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM) role that grants Amazon Comprehend read access to your input data.
--
-- /Note:/ Consider using 'dataAccessRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erpDataAccessRoleARN :: Lens.Lens' EntityRecognizerProperties (Lude.Maybe Lude.Text)
erpDataAccessRoleARN = Lens.lens (dataAccessRoleARN :: EntityRecognizerProperties -> Lude.Maybe Lude.Text) (\s a -> s {dataAccessRoleARN = a} :: EntityRecognizerProperties)
{-# DEPRECATED erpDataAccessRoleARN "Use generic-lens or generic-optics with 'dataAccessRoleARN' instead." #-}

-- | Provides information about an entity recognizer.
--
-- /Note:/ Consider using 'recognizerMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erpRecognizerMetadata :: Lens.Lens' EntityRecognizerProperties (Lude.Maybe EntityRecognizerMetadata)
erpRecognizerMetadata = Lens.lens (recognizerMetadata :: EntityRecognizerProperties -> Lude.Maybe EntityRecognizerMetadata) (\s a -> s {recognizerMetadata = a} :: EntityRecognizerProperties)
{-# DEPRECATED erpRecognizerMetadata "Use generic-lens or generic-optics with 'recognizerMetadata' instead." #-}

-- | A description of the status of the recognizer.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erpMessage :: Lens.Lens' EntityRecognizerProperties (Lude.Maybe Lude.Text)
erpMessage = Lens.lens (message :: EntityRecognizerProperties -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: EntityRecognizerProperties)
{-# DEPRECATED erpMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The time that the recognizer was submitted for processing.
--
-- /Note:/ Consider using 'submitTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erpSubmitTime :: Lens.Lens' EntityRecognizerProperties (Lude.Maybe Lude.Timestamp)
erpSubmitTime = Lens.lens (submitTime :: EntityRecognizerProperties -> Lude.Maybe Lude.Timestamp) (\s a -> s {submitTime = a} :: EntityRecognizerProperties)
{-# DEPRECATED erpSubmitTime "Use generic-lens or generic-optics with 'submitTime' instead." #-}

instance Lude.FromJSON EntityRecognizerProperties where
  parseJSON =
    Lude.withObject
      "EntityRecognizerProperties"
      ( \x ->
          EntityRecognizerProperties'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "LanguageCode")
            Lude.<*> (x Lude..:? "TrainingEndTime")
            Lude.<*> (x Lude..:? "EntityRecognizerArn")
            Lude.<*> (x Lude..:? "InputDataConfig")
            Lude.<*> (x Lude..:? "VpcConfig")
            Lude.<*> (x Lude..:? "VolumeKmsKeyId")
            Lude.<*> (x Lude..:? "EndTime")
            Lude.<*> (x Lude..:? "TrainingStartTime")
            Lude.<*> (x Lude..:? "DataAccessRoleArn")
            Lude.<*> (x Lude..:? "RecognizerMetadata")
            Lude.<*> (x Lude..:? "Message")
            Lude.<*> (x Lude..:? "SubmitTime")
      )
