{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EntitiesDetectionJobProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EntitiesDetectionJobProperties
  ( EntitiesDetectionJobProperties (..),

    -- * Smart constructor
    mkEntitiesDetectionJobProperties,

    -- * Lenses
    eLanguageCode,
    eJobId,
    eEntityRecognizerARN,
    eJobName,
    eInputDataConfig,
    eVPCConfig,
    eVolumeKMSKeyId,
    eEndTime,
    eOutputDataConfig,
    eDataAccessRoleARN,
    eJobStatus,
    eMessage,
    eSubmitTime,
  )
where

import Network.AWS.Comprehend.Types.InputDataConfig
import Network.AWS.Comprehend.Types.JobStatus
import Network.AWS.Comprehend.Types.LanguageCode
import Network.AWS.Comprehend.Types.OutputDataConfig
import Network.AWS.Comprehend.Types.VPCConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about an entities detection job.
--
-- /See:/ 'mkEntitiesDetectionJobProperties' smart constructor.
data EntitiesDetectionJobProperties = EntitiesDetectionJobProperties'
  { -- | The language code of the input documents.
    languageCode :: Lude.Maybe LanguageCode,
    -- | The identifier assigned to the entities detection job.
    jobId :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) that identifies the entity recognizer.
    entityRecognizerARN :: Lude.Maybe Lude.Text,
    -- | The name that you assigned the entities detection job.
    jobName :: Lude.Maybe Lude.Text,
    -- | The input data configuration that you supplied when you created the entities detection job.
    inputDataConfig :: Lude.Maybe InputDataConfig,
    -- | Configuration parameters for a private Virtual Private Cloud (VPC) containing the resources you are using for your entity detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
    vpcConfig :: Lude.Maybe VPCConfig,
    -- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:
    --
    --
    --     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@
    --
    --
    --     * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
    volumeKMSKeyId :: Lude.Maybe Lude.Text,
    -- | The time that the entities detection job completed
    endTime :: Lude.Maybe Lude.Timestamp,
    -- | The output data configuration that you supplied when you created the entities detection job.
    outputDataConfig :: Lude.Maybe OutputDataConfig,
    -- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access to your input data.
    dataAccessRoleARN :: Lude.Maybe Lude.Text,
    -- | The current status of the entities detection job. If the status is @FAILED@ , the @Message@ field shows the reason for the failure.
    jobStatus :: Lude.Maybe JobStatus,
    -- | A description of the status of a job.
    message :: Lude.Maybe Lude.Text,
    -- | The time that the entities detection job was submitted for processing.
    submitTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EntitiesDetectionJobProperties' with the minimum fields required to make a request.
--
-- * 'languageCode' - The language code of the input documents.
-- * 'jobId' - The identifier assigned to the entities detection job.
-- * 'entityRecognizerARN' - The Amazon Resource Name (ARN) that identifies the entity recognizer.
-- * 'jobName' - The name that you assigned the entities detection job.
-- * 'inputDataConfig' - The input data configuration that you supplied when you created the entities detection job.
-- * 'vpcConfig' - Configuration parameters for a private Virtual Private Cloud (VPC) containing the resources you are using for your entity detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
-- * 'volumeKMSKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:
--
--
--     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@
--
--
--     * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
--
--
-- * 'endTime' - The time that the entities detection job completed
-- * 'outputDataConfig' - The output data configuration that you supplied when you created the entities detection job.
-- * 'dataAccessRoleARN' - The Amazon Resource Name (ARN) that gives Amazon Comprehend read access to your input data.
-- * 'jobStatus' - The current status of the entities detection job. If the status is @FAILED@ , the @Message@ field shows the reason for the failure.
-- * 'message' - A description of the status of a job.
-- * 'submitTime' - The time that the entities detection job was submitted for processing.
mkEntitiesDetectionJobProperties ::
  EntitiesDetectionJobProperties
mkEntitiesDetectionJobProperties =
  EntitiesDetectionJobProperties'
    { languageCode = Lude.Nothing,
      jobId = Lude.Nothing,
      entityRecognizerARN = Lude.Nothing,
      jobName = Lude.Nothing,
      inputDataConfig = Lude.Nothing,
      vpcConfig = Lude.Nothing,
      volumeKMSKeyId = Lude.Nothing,
      endTime = Lude.Nothing,
      outputDataConfig = Lude.Nothing,
      dataAccessRoleARN = Lude.Nothing,
      jobStatus = Lude.Nothing,
      message = Lude.Nothing,
      submitTime = Lude.Nothing
    }

-- | The language code of the input documents.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eLanguageCode :: Lens.Lens' EntitiesDetectionJobProperties (Lude.Maybe LanguageCode)
eLanguageCode = Lens.lens (languageCode :: EntitiesDetectionJobProperties -> Lude.Maybe LanguageCode) (\s a -> s {languageCode = a} :: EntitiesDetectionJobProperties)
{-# DEPRECATED eLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | The identifier assigned to the entities detection job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eJobId :: Lens.Lens' EntitiesDetectionJobProperties (Lude.Maybe Lude.Text)
eJobId = Lens.lens (jobId :: EntitiesDetectionJobProperties -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: EntitiesDetectionJobProperties)
{-# DEPRECATED eJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The Amazon Resource Name (ARN) that identifies the entity recognizer.
--
-- /Note:/ Consider using 'entityRecognizerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEntityRecognizerARN :: Lens.Lens' EntitiesDetectionJobProperties (Lude.Maybe Lude.Text)
eEntityRecognizerARN = Lens.lens (entityRecognizerARN :: EntitiesDetectionJobProperties -> Lude.Maybe Lude.Text) (\s a -> s {entityRecognizerARN = a} :: EntitiesDetectionJobProperties)
{-# DEPRECATED eEntityRecognizerARN "Use generic-lens or generic-optics with 'entityRecognizerARN' instead." #-}

-- | The name that you assigned the entities detection job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eJobName :: Lens.Lens' EntitiesDetectionJobProperties (Lude.Maybe Lude.Text)
eJobName = Lens.lens (jobName :: EntitiesDetectionJobProperties -> Lude.Maybe Lude.Text) (\s a -> s {jobName = a} :: EntitiesDetectionJobProperties)
{-# DEPRECATED eJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The input data configuration that you supplied when you created the entities detection job.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eInputDataConfig :: Lens.Lens' EntitiesDetectionJobProperties (Lude.Maybe InputDataConfig)
eInputDataConfig = Lens.lens (inputDataConfig :: EntitiesDetectionJobProperties -> Lude.Maybe InputDataConfig) (\s a -> s {inputDataConfig = a} :: EntitiesDetectionJobProperties)
{-# DEPRECATED eInputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead." #-}

-- | Configuration parameters for a private Virtual Private Cloud (VPC) containing the resources you are using for your entity detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eVPCConfig :: Lens.Lens' EntitiesDetectionJobProperties (Lude.Maybe VPCConfig)
eVPCConfig = Lens.lens (vpcConfig :: EntitiesDetectionJobProperties -> Lude.Maybe VPCConfig) (\s a -> s {vpcConfig = a} :: EntitiesDetectionJobProperties)
{-# DEPRECATED eVPCConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

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
eVolumeKMSKeyId :: Lens.Lens' EntitiesDetectionJobProperties (Lude.Maybe Lude.Text)
eVolumeKMSKeyId = Lens.lens (volumeKMSKeyId :: EntitiesDetectionJobProperties -> Lude.Maybe Lude.Text) (\s a -> s {volumeKMSKeyId = a} :: EntitiesDetectionJobProperties)
{-# DEPRECATED eVolumeKMSKeyId "Use generic-lens or generic-optics with 'volumeKMSKeyId' instead." #-}

-- | The time that the entities detection job completed
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEndTime :: Lens.Lens' EntitiesDetectionJobProperties (Lude.Maybe Lude.Timestamp)
eEndTime = Lens.lens (endTime :: EntitiesDetectionJobProperties -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: EntitiesDetectionJobProperties)
{-# DEPRECATED eEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The output data configuration that you supplied when you created the entities detection job.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eOutputDataConfig :: Lens.Lens' EntitiesDetectionJobProperties (Lude.Maybe OutputDataConfig)
eOutputDataConfig = Lens.lens (outputDataConfig :: EntitiesDetectionJobProperties -> Lude.Maybe OutputDataConfig) (\s a -> s {outputDataConfig = a} :: EntitiesDetectionJobProperties)
{-# DEPRECATED eOutputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead." #-}

-- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access to your input data.
--
-- /Note:/ Consider using 'dataAccessRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eDataAccessRoleARN :: Lens.Lens' EntitiesDetectionJobProperties (Lude.Maybe Lude.Text)
eDataAccessRoleARN = Lens.lens (dataAccessRoleARN :: EntitiesDetectionJobProperties -> Lude.Maybe Lude.Text) (\s a -> s {dataAccessRoleARN = a} :: EntitiesDetectionJobProperties)
{-# DEPRECATED eDataAccessRoleARN "Use generic-lens or generic-optics with 'dataAccessRoleARN' instead." #-}

-- | The current status of the entities detection job. If the status is @FAILED@ , the @Message@ field shows the reason for the failure.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eJobStatus :: Lens.Lens' EntitiesDetectionJobProperties (Lude.Maybe JobStatus)
eJobStatus = Lens.lens (jobStatus :: EntitiesDetectionJobProperties -> Lude.Maybe JobStatus) (\s a -> s {jobStatus = a} :: EntitiesDetectionJobProperties)
{-# DEPRECATED eJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | A description of the status of a job.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eMessage :: Lens.Lens' EntitiesDetectionJobProperties (Lude.Maybe Lude.Text)
eMessage = Lens.lens (message :: EntitiesDetectionJobProperties -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: EntitiesDetectionJobProperties)
{-# DEPRECATED eMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The time that the entities detection job was submitted for processing.
--
-- /Note:/ Consider using 'submitTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSubmitTime :: Lens.Lens' EntitiesDetectionJobProperties (Lude.Maybe Lude.Timestamp)
eSubmitTime = Lens.lens (submitTime :: EntitiesDetectionJobProperties -> Lude.Maybe Lude.Timestamp) (\s a -> s {submitTime = a} :: EntitiesDetectionJobProperties)
{-# DEPRECATED eSubmitTime "Use generic-lens or generic-optics with 'submitTime' instead." #-}

instance Lude.FromJSON EntitiesDetectionJobProperties where
  parseJSON =
    Lude.withObject
      "EntitiesDetectionJobProperties"
      ( \x ->
          EntitiesDetectionJobProperties'
            Lude.<$> (x Lude..:? "LanguageCode")
            Lude.<*> (x Lude..:? "JobId")
            Lude.<*> (x Lude..:? "EntityRecognizerArn")
            Lude.<*> (x Lude..:? "JobName")
            Lude.<*> (x Lude..:? "InputDataConfig")
            Lude.<*> (x Lude..:? "VpcConfig")
            Lude.<*> (x Lude..:? "VolumeKmsKeyId")
            Lude.<*> (x Lude..:? "EndTime")
            Lude.<*> (x Lude..:? "OutputDataConfig")
            Lude.<*> (x Lude..:? "DataAccessRoleArn")
            Lude.<*> (x Lude..:? "JobStatus")
            Lude.<*> (x Lude..:? "Message")
            Lude.<*> (x Lude..:? "SubmitTime")
      )
