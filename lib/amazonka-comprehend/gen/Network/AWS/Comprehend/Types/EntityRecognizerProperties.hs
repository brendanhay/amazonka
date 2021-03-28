{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EntityRecognizerProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Comprehend.Types.EntityRecognizerProperties
  ( EntityRecognizerProperties (..)
  -- * Smart constructor
  , mkEntityRecognizerProperties
  -- * Lenses
  , erpDataAccessRoleArn
  , erpEndTime
  , erpEntityRecognizerArn
  , erpInputDataConfig
  , erpLanguageCode
  , erpMessage
  , erpRecognizerMetadata
  , erpStatus
  , erpSubmitTime
  , erpTrainingEndTime
  , erpTrainingStartTime
  , erpVolumeKmsKeyId
  , erpVpcConfig
  ) where

import qualified Network.AWS.Comprehend.Types.EntityRecognizerArn as Types
import qualified Network.AWS.Comprehend.Types.EntityRecognizerInputDataConfig as Types
import qualified Network.AWS.Comprehend.Types.EntityRecognizerMetadata as Types
import qualified Network.AWS.Comprehend.Types.IamRoleArn as Types
import qualified Network.AWS.Comprehend.Types.LanguageCode as Types
import qualified Network.AWS.Comprehend.Types.Message as Types
import qualified Network.AWS.Comprehend.Types.ModelStatus as Types
import qualified Network.AWS.Comprehend.Types.VolumeKmsKeyId as Types
import qualified Network.AWS.Comprehend.Types.VpcConfig as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes information about an entity recognizer.
--
-- /See:/ 'mkEntityRecognizerProperties' smart constructor.
data EntityRecognizerProperties = EntityRecognizerProperties'
  { dataAccessRoleArn :: Core.Maybe Types.IamRoleArn
    -- ^ The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM) role that grants Amazon Comprehend read access to your input data.
  , endTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time that the recognizer creation completed.
  , entityRecognizerArn :: Core.Maybe Types.EntityRecognizerArn
    -- ^ The Amazon Resource Name (ARN) that identifies the entity recognizer.
  , inputDataConfig :: Core.Maybe Types.EntityRecognizerInputDataConfig
    -- ^ The input data properties of an entity recognizer.
  , languageCode :: Core.Maybe Types.LanguageCode
    -- ^ The language of the input documents. All documents must be in the same language. Only English ("en") is currently supported.
  , message :: Core.Maybe Types.Message
    -- ^ A description of the status of the recognizer.
  , recognizerMetadata :: Core.Maybe Types.EntityRecognizerMetadata
    -- ^ Provides information about an entity recognizer.
  , status :: Core.Maybe Types.ModelStatus
    -- ^ Provides the status of the entity recognizer.
  , submitTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time that the recognizer was submitted for processing.
  , trainingEndTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time that training of the entity recognizer was completed.
  , trainingStartTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time that training of the entity recognizer started.
  , volumeKmsKeyId :: Core.Maybe Types.VolumeKmsKeyId
    -- ^ ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:
--
--
--     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@ 
--
--
--     * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@ 
--
--
  , vpcConfig :: Core.Maybe Types.VpcConfig
    -- ^ Configuration parameters for a private Virtual Private Cloud (VPC) containing the resources you are using for your custom entity recognizer. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'EntityRecognizerProperties' value with any optional fields omitted.
mkEntityRecognizerProperties
    :: EntityRecognizerProperties
mkEntityRecognizerProperties
  = EntityRecognizerProperties'{dataAccessRoleArn = Core.Nothing,
                                endTime = Core.Nothing, entityRecognizerArn = Core.Nothing,
                                inputDataConfig = Core.Nothing, languageCode = Core.Nothing,
                                message = Core.Nothing, recognizerMetadata = Core.Nothing,
                                status = Core.Nothing, submitTime = Core.Nothing,
                                trainingEndTime = Core.Nothing, trainingStartTime = Core.Nothing,
                                volumeKmsKeyId = Core.Nothing, vpcConfig = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM) role that grants Amazon Comprehend read access to your input data.
--
-- /Note:/ Consider using 'dataAccessRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erpDataAccessRoleArn :: Lens.Lens' EntityRecognizerProperties (Core.Maybe Types.IamRoleArn)
erpDataAccessRoleArn = Lens.field @"dataAccessRoleArn"
{-# INLINEABLE erpDataAccessRoleArn #-}
{-# DEPRECATED dataAccessRoleArn "Use generic-lens or generic-optics with 'dataAccessRoleArn' instead"  #-}

-- | The time that the recognizer creation completed.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erpEndTime :: Lens.Lens' EntityRecognizerProperties (Core.Maybe Core.NominalDiffTime)
erpEndTime = Lens.field @"endTime"
{-# INLINEABLE erpEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | The Amazon Resource Name (ARN) that identifies the entity recognizer.
--
-- /Note:/ Consider using 'entityRecognizerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erpEntityRecognizerArn :: Lens.Lens' EntityRecognizerProperties (Core.Maybe Types.EntityRecognizerArn)
erpEntityRecognizerArn = Lens.field @"entityRecognizerArn"
{-# INLINEABLE erpEntityRecognizerArn #-}
{-# DEPRECATED entityRecognizerArn "Use generic-lens or generic-optics with 'entityRecognizerArn' instead"  #-}

-- | The input data properties of an entity recognizer.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erpInputDataConfig :: Lens.Lens' EntityRecognizerProperties (Core.Maybe Types.EntityRecognizerInputDataConfig)
erpInputDataConfig = Lens.field @"inputDataConfig"
{-# INLINEABLE erpInputDataConfig #-}
{-# DEPRECATED inputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead"  #-}

-- | The language of the input documents. All documents must be in the same language. Only English ("en") is currently supported.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erpLanguageCode :: Lens.Lens' EntityRecognizerProperties (Core.Maybe Types.LanguageCode)
erpLanguageCode = Lens.field @"languageCode"
{-# INLINEABLE erpLanguageCode #-}
{-# DEPRECATED languageCode "Use generic-lens or generic-optics with 'languageCode' instead"  #-}

-- | A description of the status of the recognizer.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erpMessage :: Lens.Lens' EntityRecognizerProperties (Core.Maybe Types.Message)
erpMessage = Lens.field @"message"
{-# INLINEABLE erpMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

-- | Provides information about an entity recognizer.
--
-- /Note:/ Consider using 'recognizerMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erpRecognizerMetadata :: Lens.Lens' EntityRecognizerProperties (Core.Maybe Types.EntityRecognizerMetadata)
erpRecognizerMetadata = Lens.field @"recognizerMetadata"
{-# INLINEABLE erpRecognizerMetadata #-}
{-# DEPRECATED recognizerMetadata "Use generic-lens or generic-optics with 'recognizerMetadata' instead"  #-}

-- | Provides the status of the entity recognizer.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erpStatus :: Lens.Lens' EntityRecognizerProperties (Core.Maybe Types.ModelStatus)
erpStatus = Lens.field @"status"
{-# INLINEABLE erpStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The time that the recognizer was submitted for processing.
--
-- /Note:/ Consider using 'submitTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erpSubmitTime :: Lens.Lens' EntityRecognizerProperties (Core.Maybe Core.NominalDiffTime)
erpSubmitTime = Lens.field @"submitTime"
{-# INLINEABLE erpSubmitTime #-}
{-# DEPRECATED submitTime "Use generic-lens or generic-optics with 'submitTime' instead"  #-}

-- | The time that training of the entity recognizer was completed.
--
-- /Note:/ Consider using 'trainingEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erpTrainingEndTime :: Lens.Lens' EntityRecognizerProperties (Core.Maybe Core.NominalDiffTime)
erpTrainingEndTime = Lens.field @"trainingEndTime"
{-# INLINEABLE erpTrainingEndTime #-}
{-# DEPRECATED trainingEndTime "Use generic-lens or generic-optics with 'trainingEndTime' instead"  #-}

-- | The time that training of the entity recognizer started.
--
-- /Note:/ Consider using 'trainingStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erpTrainingStartTime :: Lens.Lens' EntityRecognizerProperties (Core.Maybe Core.NominalDiffTime)
erpTrainingStartTime = Lens.field @"trainingStartTime"
{-# INLINEABLE erpTrainingStartTime #-}
{-# DEPRECATED trainingStartTime "Use generic-lens or generic-optics with 'trainingStartTime' instead"  #-}

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
-- /Note:/ Consider using 'volumeKmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erpVolumeKmsKeyId :: Lens.Lens' EntityRecognizerProperties (Core.Maybe Types.VolumeKmsKeyId)
erpVolumeKmsKeyId = Lens.field @"volumeKmsKeyId"
{-# INLINEABLE erpVolumeKmsKeyId #-}
{-# DEPRECATED volumeKmsKeyId "Use generic-lens or generic-optics with 'volumeKmsKeyId' instead"  #-}

-- | Configuration parameters for a private Virtual Private Cloud (VPC) containing the resources you are using for your custom entity recognizer. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> . 
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erpVpcConfig :: Lens.Lens' EntityRecognizerProperties (Core.Maybe Types.VpcConfig)
erpVpcConfig = Lens.field @"vpcConfig"
{-# INLINEABLE erpVpcConfig #-}
{-# DEPRECATED vpcConfig "Use generic-lens or generic-optics with 'vpcConfig' instead"  #-}

instance Core.FromJSON EntityRecognizerProperties where
        parseJSON
          = Core.withObject "EntityRecognizerProperties" Core.$
              \ x ->
                EntityRecognizerProperties' Core.<$>
                  (x Core..:? "DataAccessRoleArn") Core.<*> x Core..:? "EndTime"
                    Core.<*> x Core..:? "EntityRecognizerArn"
                    Core.<*> x Core..:? "InputDataConfig"
                    Core.<*> x Core..:? "LanguageCode"
                    Core.<*> x Core..:? "Message"
                    Core.<*> x Core..:? "RecognizerMetadata"
                    Core.<*> x Core..:? "Status"
                    Core.<*> x Core..:? "SubmitTime"
                    Core.<*> x Core..:? "TrainingEndTime"
                    Core.<*> x Core..:? "TrainingStartTime"
                    Core.<*> x Core..:? "VolumeKmsKeyId"
                    Core.<*> x Core..:? "VpcConfig"
