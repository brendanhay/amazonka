{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.DocumentClassifierProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Comprehend.Types.DocumentClassifierProperties
  ( DocumentClassifierProperties (..)
  -- * Smart constructor
  , mkDocumentClassifierProperties
  -- * Lenses
  , dcpClassifierMetadata
  , dcpDataAccessRoleArn
  , dcpDocumentClassifierArn
  , dcpEndTime
  , dcpInputDataConfig
  , dcpLanguageCode
  , dcpMessage
  , dcpMode
  , dcpOutputDataConfig
  , dcpStatus
  , dcpSubmitTime
  , dcpTrainingEndTime
  , dcpTrainingStartTime
  , dcpVolumeKmsKeyId
  , dcpVpcConfig
  ) where

import qualified Network.AWS.Comprehend.Types.AnyLengthString as Types
import qualified Network.AWS.Comprehend.Types.ClassifierMetadata as Types
import qualified Network.AWS.Comprehend.Types.DocumentClassifierArn as Types
import qualified Network.AWS.Comprehend.Types.DocumentClassifierInputDataConfig as Types
import qualified Network.AWS.Comprehend.Types.DocumentClassifierMode as Types
import qualified Network.AWS.Comprehend.Types.DocumentClassifierOutputDataConfig as Types
import qualified Network.AWS.Comprehend.Types.IamRoleArn as Types
import qualified Network.AWS.Comprehend.Types.KmsKeyId as Types
import qualified Network.AWS.Comprehend.Types.LanguageCode as Types
import qualified Network.AWS.Comprehend.Types.ModelStatus as Types
import qualified Network.AWS.Comprehend.Types.VpcConfig as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information about a document classifier.
--
-- /See:/ 'mkDocumentClassifierProperties' smart constructor.
data DocumentClassifierProperties = DocumentClassifierProperties'
  { classifierMetadata :: Core.Maybe Types.ClassifierMetadata
    -- ^ Information about the document classifier, including the number of documents used for training the classifier, the number of documents used for test the classifier, and an accuracy rating.
  , dataAccessRoleArn :: Core.Maybe Types.IamRoleArn
    -- ^ The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM) role that grants Amazon Comprehend read access to your input data.
  , documentClassifierArn :: Core.Maybe Types.DocumentClassifierArn
    -- ^ The Amazon Resource Name (ARN) that identifies the document classifier.
  , endTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time that training the document classifier completed.
  , inputDataConfig :: Core.Maybe Types.DocumentClassifierInputDataConfig
    -- ^ The input data configuration that you supplied when you created the document classifier for training.
  , languageCode :: Core.Maybe Types.LanguageCode
    -- ^ The language code for the language of the documents that the classifier was trained on.
  , message :: Core.Maybe Types.AnyLengthString
    -- ^ Additional information about the status of the classifier.
  , mode :: Core.Maybe Types.DocumentClassifierMode
    -- ^ Indicates the mode in which the specific classifier was trained. This also indicates the format of input documents and the format of the confusion matrix. Each classifier can only be trained in one mode and this cannot be changed once the classifier is trained.
  , outputDataConfig :: Core.Maybe Types.DocumentClassifierOutputDataConfig
    -- ^ Provides output results configuration parameters for custom classifier jobs.
  , status :: Core.Maybe Types.ModelStatus
    -- ^ The status of the document classifier. If the status is @TRAINED@ the classifier is ready to use. If the status is @FAILED@ you can see additional information about why the classifier wasn't trained in the @Message@ field.
  , submitTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time that the document classifier was submitted for training.
  , trainingEndTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time that training of the document classifier was completed. Indicates the time when the training completes on documentation classifiers. You are billed for the time interval between this time and the value of TrainingStartTime.
  , trainingStartTime :: Core.Maybe Core.NominalDiffTime
    -- ^ Indicates the time when the training starts on documentation classifiers. You are billed for the time interval between this time and the value of TrainingEndTime. 
  , volumeKmsKeyId :: Core.Maybe Types.KmsKeyId
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
    -- ^ Configuration parameters for a private Virtual Private Cloud (VPC) containing the resources you are using for your custom classifier. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DocumentClassifierProperties' value with any optional fields omitted.
mkDocumentClassifierProperties
    :: DocumentClassifierProperties
mkDocumentClassifierProperties
  = DocumentClassifierProperties'{classifierMetadata = Core.Nothing,
                                  dataAccessRoleArn = Core.Nothing,
                                  documentClassifierArn = Core.Nothing, endTime = Core.Nothing,
                                  inputDataConfig = Core.Nothing, languageCode = Core.Nothing,
                                  message = Core.Nothing, mode = Core.Nothing,
                                  outputDataConfig = Core.Nothing, status = Core.Nothing,
                                  submitTime = Core.Nothing, trainingEndTime = Core.Nothing,
                                  trainingStartTime = Core.Nothing, volumeKmsKeyId = Core.Nothing,
                                  vpcConfig = Core.Nothing}

-- | Information about the document classifier, including the number of documents used for training the classifier, the number of documents used for test the classifier, and an accuracy rating.
--
-- /Note:/ Consider using 'classifierMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpClassifierMetadata :: Lens.Lens' DocumentClassifierProperties (Core.Maybe Types.ClassifierMetadata)
dcpClassifierMetadata = Lens.field @"classifierMetadata"
{-# INLINEABLE dcpClassifierMetadata #-}
{-# DEPRECATED classifierMetadata "Use generic-lens or generic-optics with 'classifierMetadata' instead"  #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM) role that grants Amazon Comprehend read access to your input data.
--
-- /Note:/ Consider using 'dataAccessRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpDataAccessRoleArn :: Lens.Lens' DocumentClassifierProperties (Core.Maybe Types.IamRoleArn)
dcpDataAccessRoleArn = Lens.field @"dataAccessRoleArn"
{-# INLINEABLE dcpDataAccessRoleArn #-}
{-# DEPRECATED dataAccessRoleArn "Use generic-lens or generic-optics with 'dataAccessRoleArn' instead"  #-}

-- | The Amazon Resource Name (ARN) that identifies the document classifier.
--
-- /Note:/ Consider using 'documentClassifierArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpDocumentClassifierArn :: Lens.Lens' DocumentClassifierProperties (Core.Maybe Types.DocumentClassifierArn)
dcpDocumentClassifierArn = Lens.field @"documentClassifierArn"
{-# INLINEABLE dcpDocumentClassifierArn #-}
{-# DEPRECATED documentClassifierArn "Use generic-lens or generic-optics with 'documentClassifierArn' instead"  #-}

-- | The time that training the document classifier completed.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpEndTime :: Lens.Lens' DocumentClassifierProperties (Core.Maybe Core.NominalDiffTime)
dcpEndTime = Lens.field @"endTime"
{-# INLINEABLE dcpEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | The input data configuration that you supplied when you created the document classifier for training.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpInputDataConfig :: Lens.Lens' DocumentClassifierProperties (Core.Maybe Types.DocumentClassifierInputDataConfig)
dcpInputDataConfig = Lens.field @"inputDataConfig"
{-# INLINEABLE dcpInputDataConfig #-}
{-# DEPRECATED inputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead"  #-}

-- | The language code for the language of the documents that the classifier was trained on.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpLanguageCode :: Lens.Lens' DocumentClassifierProperties (Core.Maybe Types.LanguageCode)
dcpLanguageCode = Lens.field @"languageCode"
{-# INLINEABLE dcpLanguageCode #-}
{-# DEPRECATED languageCode "Use generic-lens or generic-optics with 'languageCode' instead"  #-}

-- | Additional information about the status of the classifier.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpMessage :: Lens.Lens' DocumentClassifierProperties (Core.Maybe Types.AnyLengthString)
dcpMessage = Lens.field @"message"
{-# INLINEABLE dcpMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

-- | Indicates the mode in which the specific classifier was trained. This also indicates the format of input documents and the format of the confusion matrix. Each classifier can only be trained in one mode and this cannot be changed once the classifier is trained.
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpMode :: Lens.Lens' DocumentClassifierProperties (Core.Maybe Types.DocumentClassifierMode)
dcpMode = Lens.field @"mode"
{-# INLINEABLE dcpMode #-}
{-# DEPRECATED mode "Use generic-lens or generic-optics with 'mode' instead"  #-}

-- | Provides output results configuration parameters for custom classifier jobs.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpOutputDataConfig :: Lens.Lens' DocumentClassifierProperties (Core.Maybe Types.DocumentClassifierOutputDataConfig)
dcpOutputDataConfig = Lens.field @"outputDataConfig"
{-# INLINEABLE dcpOutputDataConfig #-}
{-# DEPRECATED outputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead"  #-}

-- | The status of the document classifier. If the status is @TRAINED@ the classifier is ready to use. If the status is @FAILED@ you can see additional information about why the classifier wasn't trained in the @Message@ field.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpStatus :: Lens.Lens' DocumentClassifierProperties (Core.Maybe Types.ModelStatus)
dcpStatus = Lens.field @"status"
{-# INLINEABLE dcpStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The time that the document classifier was submitted for training.
--
-- /Note:/ Consider using 'submitTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpSubmitTime :: Lens.Lens' DocumentClassifierProperties (Core.Maybe Core.NominalDiffTime)
dcpSubmitTime = Lens.field @"submitTime"
{-# INLINEABLE dcpSubmitTime #-}
{-# DEPRECATED submitTime "Use generic-lens or generic-optics with 'submitTime' instead"  #-}

-- | The time that training of the document classifier was completed. Indicates the time when the training completes on documentation classifiers. You are billed for the time interval between this time and the value of TrainingStartTime.
--
-- /Note:/ Consider using 'trainingEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpTrainingEndTime :: Lens.Lens' DocumentClassifierProperties (Core.Maybe Core.NominalDiffTime)
dcpTrainingEndTime = Lens.field @"trainingEndTime"
{-# INLINEABLE dcpTrainingEndTime #-}
{-# DEPRECATED trainingEndTime "Use generic-lens or generic-optics with 'trainingEndTime' instead"  #-}

-- | Indicates the time when the training starts on documentation classifiers. You are billed for the time interval between this time and the value of TrainingEndTime. 
--
-- /Note:/ Consider using 'trainingStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpTrainingStartTime :: Lens.Lens' DocumentClassifierProperties (Core.Maybe Core.NominalDiffTime)
dcpTrainingStartTime = Lens.field @"trainingStartTime"
{-# INLINEABLE dcpTrainingStartTime #-}
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
dcpVolumeKmsKeyId :: Lens.Lens' DocumentClassifierProperties (Core.Maybe Types.KmsKeyId)
dcpVolumeKmsKeyId = Lens.field @"volumeKmsKeyId"
{-# INLINEABLE dcpVolumeKmsKeyId #-}
{-# DEPRECATED volumeKmsKeyId "Use generic-lens or generic-optics with 'volumeKmsKeyId' instead"  #-}

-- | Configuration parameters for a private Virtual Private Cloud (VPC) containing the resources you are using for your custom classifier. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> . 
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpVpcConfig :: Lens.Lens' DocumentClassifierProperties (Core.Maybe Types.VpcConfig)
dcpVpcConfig = Lens.field @"vpcConfig"
{-# INLINEABLE dcpVpcConfig #-}
{-# DEPRECATED vpcConfig "Use generic-lens or generic-optics with 'vpcConfig' instead"  #-}

instance Core.FromJSON DocumentClassifierProperties where
        parseJSON
          = Core.withObject "DocumentClassifierProperties" Core.$
              \ x ->
                DocumentClassifierProperties' Core.<$>
                  (x Core..:? "ClassifierMetadata") Core.<*>
                    x Core..:? "DataAccessRoleArn"
                    Core.<*> x Core..:? "DocumentClassifierArn"
                    Core.<*> x Core..:? "EndTime"
                    Core.<*> x Core..:? "InputDataConfig"
                    Core.<*> x Core..:? "LanguageCode"
                    Core.<*> x Core..:? "Message"
                    Core.<*> x Core..:? "Mode"
                    Core.<*> x Core..:? "OutputDataConfig"
                    Core.<*> x Core..:? "Status"
                    Core.<*> x Core..:? "SubmitTime"
                    Core.<*> x Core..:? "TrainingEndTime"
                    Core.<*> x Core..:? "TrainingStartTime"
                    Core.<*> x Core..:? "VolumeKmsKeyId"
                    Core.<*> x Core..:? "VpcConfig"
