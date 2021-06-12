{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EntitiesDetectionJobProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EntitiesDetectionJobProperties where

import Network.AWS.Comprehend.Types.InputDataConfig
import Network.AWS.Comprehend.Types.JobStatus
import Network.AWS.Comprehend.Types.LanguageCode
import Network.AWS.Comprehend.Types.OutputDataConfig
import Network.AWS.Comprehend.Types.VpcConfig
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides information about an entities detection job.
--
-- /See:/ 'newEntitiesDetectionJobProperties' smart constructor.
data EntitiesDetectionJobProperties = EntitiesDetectionJobProperties'
  { -- | Configuration parameters for a private Virtual Private Cloud (VPC)
    -- containing the resources you are using for your entity detection job.
    -- For more information, see
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
    vpcConfig :: Core.Maybe VpcConfig,
    -- | The language code of the input documents.
    languageCode :: Core.Maybe LanguageCode,
    -- | The input data configuration that you supplied when you created the
    -- entities detection job.
    inputDataConfig :: Core.Maybe InputDataConfig,
    -- | A description of the status of a job.
    message :: Core.Maybe Core.Text,
    -- | The current status of the entities detection job. If the status is
    -- @FAILED@, the @Message@ field shows the reason for the failure.
    jobStatus :: Core.Maybe JobStatus,
    -- | The output data configuration that you supplied when you created the
    -- entities detection job.
    outputDataConfig :: Core.Maybe OutputDataConfig,
    -- | The time that the entities detection job completed
    endTime :: Core.Maybe Core.POSIX,
    -- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
    -- uses to encrypt data on the storage volume attached to the ML compute
    -- instance(s) that process the analysis job. The VolumeKmsKeyId can be
    -- either of the following formats:
    --
    -- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
    --
    -- -   Amazon Resource Name (ARN) of a KMS Key:
    --     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
    volumeKmsKeyId :: Core.Maybe Core.Text,
    -- | The time that the entities detection job was submitted for processing.
    submitTime :: Core.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) that identifies the entity recognizer.
    entityRecognizerArn :: Core.Maybe Core.Text,
    -- | The name that you assigned the entities detection job.
    jobName :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access
    -- to your input data.
    dataAccessRoleArn :: Core.Maybe Core.Text,
    -- | The identifier assigned to the entities detection job.
    jobId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EntitiesDetectionJobProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcConfig', 'entitiesDetectionJobProperties_vpcConfig' - Configuration parameters for a private Virtual Private Cloud (VPC)
-- containing the resources you are using for your entity detection job.
-- For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
--
-- 'languageCode', 'entitiesDetectionJobProperties_languageCode' - The language code of the input documents.
--
-- 'inputDataConfig', 'entitiesDetectionJobProperties_inputDataConfig' - The input data configuration that you supplied when you created the
-- entities detection job.
--
-- 'message', 'entitiesDetectionJobProperties_message' - A description of the status of a job.
--
-- 'jobStatus', 'entitiesDetectionJobProperties_jobStatus' - The current status of the entities detection job. If the status is
-- @FAILED@, the @Message@ field shows the reason for the failure.
--
-- 'outputDataConfig', 'entitiesDetectionJobProperties_outputDataConfig' - The output data configuration that you supplied when you created the
-- entities detection job.
--
-- 'endTime', 'entitiesDetectionJobProperties_endTime' - The time that the entities detection job completed
--
-- 'volumeKmsKeyId', 'entitiesDetectionJobProperties_volumeKmsKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt data on the storage volume attached to the ML compute
-- instance(s) that process the analysis job. The VolumeKmsKeyId can be
-- either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- 'submitTime', 'entitiesDetectionJobProperties_submitTime' - The time that the entities detection job was submitted for processing.
--
-- 'entityRecognizerArn', 'entitiesDetectionJobProperties_entityRecognizerArn' - The Amazon Resource Name (ARN) that identifies the entity recognizer.
--
-- 'jobName', 'entitiesDetectionJobProperties_jobName' - The name that you assigned the entities detection job.
--
-- 'dataAccessRoleArn', 'entitiesDetectionJobProperties_dataAccessRoleArn' - The Amazon Resource Name (ARN) that gives Amazon Comprehend read access
-- to your input data.
--
-- 'jobId', 'entitiesDetectionJobProperties_jobId' - The identifier assigned to the entities detection job.
newEntitiesDetectionJobProperties ::
  EntitiesDetectionJobProperties
newEntitiesDetectionJobProperties =
  EntitiesDetectionJobProperties'
    { vpcConfig =
        Core.Nothing,
      languageCode = Core.Nothing,
      inputDataConfig = Core.Nothing,
      message = Core.Nothing,
      jobStatus = Core.Nothing,
      outputDataConfig = Core.Nothing,
      endTime = Core.Nothing,
      volumeKmsKeyId = Core.Nothing,
      submitTime = Core.Nothing,
      entityRecognizerArn = Core.Nothing,
      jobName = Core.Nothing,
      dataAccessRoleArn = Core.Nothing,
      jobId = Core.Nothing
    }

-- | Configuration parameters for a private Virtual Private Cloud (VPC)
-- containing the resources you are using for your entity detection job.
-- For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
entitiesDetectionJobProperties_vpcConfig :: Lens.Lens' EntitiesDetectionJobProperties (Core.Maybe VpcConfig)
entitiesDetectionJobProperties_vpcConfig = Lens.lens (\EntitiesDetectionJobProperties' {vpcConfig} -> vpcConfig) (\s@EntitiesDetectionJobProperties' {} a -> s {vpcConfig = a} :: EntitiesDetectionJobProperties)

-- | The language code of the input documents.
entitiesDetectionJobProperties_languageCode :: Lens.Lens' EntitiesDetectionJobProperties (Core.Maybe LanguageCode)
entitiesDetectionJobProperties_languageCode = Lens.lens (\EntitiesDetectionJobProperties' {languageCode} -> languageCode) (\s@EntitiesDetectionJobProperties' {} a -> s {languageCode = a} :: EntitiesDetectionJobProperties)

-- | The input data configuration that you supplied when you created the
-- entities detection job.
entitiesDetectionJobProperties_inputDataConfig :: Lens.Lens' EntitiesDetectionJobProperties (Core.Maybe InputDataConfig)
entitiesDetectionJobProperties_inputDataConfig = Lens.lens (\EntitiesDetectionJobProperties' {inputDataConfig} -> inputDataConfig) (\s@EntitiesDetectionJobProperties' {} a -> s {inputDataConfig = a} :: EntitiesDetectionJobProperties)

-- | A description of the status of a job.
entitiesDetectionJobProperties_message :: Lens.Lens' EntitiesDetectionJobProperties (Core.Maybe Core.Text)
entitiesDetectionJobProperties_message = Lens.lens (\EntitiesDetectionJobProperties' {message} -> message) (\s@EntitiesDetectionJobProperties' {} a -> s {message = a} :: EntitiesDetectionJobProperties)

-- | The current status of the entities detection job. If the status is
-- @FAILED@, the @Message@ field shows the reason for the failure.
entitiesDetectionJobProperties_jobStatus :: Lens.Lens' EntitiesDetectionJobProperties (Core.Maybe JobStatus)
entitiesDetectionJobProperties_jobStatus = Lens.lens (\EntitiesDetectionJobProperties' {jobStatus} -> jobStatus) (\s@EntitiesDetectionJobProperties' {} a -> s {jobStatus = a} :: EntitiesDetectionJobProperties)

-- | The output data configuration that you supplied when you created the
-- entities detection job.
entitiesDetectionJobProperties_outputDataConfig :: Lens.Lens' EntitiesDetectionJobProperties (Core.Maybe OutputDataConfig)
entitiesDetectionJobProperties_outputDataConfig = Lens.lens (\EntitiesDetectionJobProperties' {outputDataConfig} -> outputDataConfig) (\s@EntitiesDetectionJobProperties' {} a -> s {outputDataConfig = a} :: EntitiesDetectionJobProperties)

-- | The time that the entities detection job completed
entitiesDetectionJobProperties_endTime :: Lens.Lens' EntitiesDetectionJobProperties (Core.Maybe Core.UTCTime)
entitiesDetectionJobProperties_endTime = Lens.lens (\EntitiesDetectionJobProperties' {endTime} -> endTime) (\s@EntitiesDetectionJobProperties' {} a -> s {endTime = a} :: EntitiesDetectionJobProperties) Core.. Lens.mapping Core._Time

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt data on the storage volume attached to the ML compute
-- instance(s) that process the analysis job. The VolumeKmsKeyId can be
-- either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
entitiesDetectionJobProperties_volumeKmsKeyId :: Lens.Lens' EntitiesDetectionJobProperties (Core.Maybe Core.Text)
entitiesDetectionJobProperties_volumeKmsKeyId = Lens.lens (\EntitiesDetectionJobProperties' {volumeKmsKeyId} -> volumeKmsKeyId) (\s@EntitiesDetectionJobProperties' {} a -> s {volumeKmsKeyId = a} :: EntitiesDetectionJobProperties)

-- | The time that the entities detection job was submitted for processing.
entitiesDetectionJobProperties_submitTime :: Lens.Lens' EntitiesDetectionJobProperties (Core.Maybe Core.UTCTime)
entitiesDetectionJobProperties_submitTime = Lens.lens (\EntitiesDetectionJobProperties' {submitTime} -> submitTime) (\s@EntitiesDetectionJobProperties' {} a -> s {submitTime = a} :: EntitiesDetectionJobProperties) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) that identifies the entity recognizer.
entitiesDetectionJobProperties_entityRecognizerArn :: Lens.Lens' EntitiesDetectionJobProperties (Core.Maybe Core.Text)
entitiesDetectionJobProperties_entityRecognizerArn = Lens.lens (\EntitiesDetectionJobProperties' {entityRecognizerArn} -> entityRecognizerArn) (\s@EntitiesDetectionJobProperties' {} a -> s {entityRecognizerArn = a} :: EntitiesDetectionJobProperties)

-- | The name that you assigned the entities detection job.
entitiesDetectionJobProperties_jobName :: Lens.Lens' EntitiesDetectionJobProperties (Core.Maybe Core.Text)
entitiesDetectionJobProperties_jobName = Lens.lens (\EntitiesDetectionJobProperties' {jobName} -> jobName) (\s@EntitiesDetectionJobProperties' {} a -> s {jobName = a} :: EntitiesDetectionJobProperties)

-- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access
-- to your input data.
entitiesDetectionJobProperties_dataAccessRoleArn :: Lens.Lens' EntitiesDetectionJobProperties (Core.Maybe Core.Text)
entitiesDetectionJobProperties_dataAccessRoleArn = Lens.lens (\EntitiesDetectionJobProperties' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@EntitiesDetectionJobProperties' {} a -> s {dataAccessRoleArn = a} :: EntitiesDetectionJobProperties)

-- | The identifier assigned to the entities detection job.
entitiesDetectionJobProperties_jobId :: Lens.Lens' EntitiesDetectionJobProperties (Core.Maybe Core.Text)
entitiesDetectionJobProperties_jobId = Lens.lens (\EntitiesDetectionJobProperties' {jobId} -> jobId) (\s@EntitiesDetectionJobProperties' {} a -> s {jobId = a} :: EntitiesDetectionJobProperties)

instance Core.FromJSON EntitiesDetectionJobProperties where
  parseJSON =
    Core.withObject
      "EntitiesDetectionJobProperties"
      ( \x ->
          EntitiesDetectionJobProperties'
            Core.<$> (x Core..:? "VpcConfig")
            Core.<*> (x Core..:? "LanguageCode")
            Core.<*> (x Core..:? "InputDataConfig")
            Core.<*> (x Core..:? "Message")
            Core.<*> (x Core..:? "JobStatus")
            Core.<*> (x Core..:? "OutputDataConfig")
            Core.<*> (x Core..:? "EndTime")
            Core.<*> (x Core..:? "VolumeKmsKeyId")
            Core.<*> (x Core..:? "SubmitTime")
            Core.<*> (x Core..:? "EntityRecognizerArn")
            Core.<*> (x Core..:? "JobName")
            Core.<*> (x Core..:? "DataAccessRoleArn")
            Core.<*> (x Core..:? "JobId")
      )

instance Core.Hashable EntitiesDetectionJobProperties

instance Core.NFData EntitiesDetectionJobProperties
