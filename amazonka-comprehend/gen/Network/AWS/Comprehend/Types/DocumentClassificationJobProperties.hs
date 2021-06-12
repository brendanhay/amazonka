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
-- Module      : Network.AWS.Comprehend.Types.DocumentClassificationJobProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.DocumentClassificationJobProperties where

import Network.AWS.Comprehend.Types.InputDataConfig
import Network.AWS.Comprehend.Types.JobStatus
import Network.AWS.Comprehend.Types.OutputDataConfig
import Network.AWS.Comprehend.Types.VpcConfig
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides information about a document classification job.
--
-- /See:/ 'newDocumentClassificationJobProperties' smart constructor.
data DocumentClassificationJobProperties = DocumentClassificationJobProperties'
  { -- | Configuration parameters for a private Virtual Private Cloud (VPC)
    -- containing the resources you are using for your document classification
    -- job. For more information, see
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
    vpcConfig :: Core.Maybe VpcConfig,
    -- | The input data configuration that you supplied when you created the
    -- document classification job.
    inputDataConfig :: Core.Maybe InputDataConfig,
    -- | A description of the status of the job.
    message :: Core.Maybe Core.Text,
    -- | The current status of the document classification job. If the status is
    -- @FAILED@, the @Message@ field shows the reason for the failure.
    jobStatus :: Core.Maybe JobStatus,
    -- | The output data configuration that you supplied when you created the
    -- document classification job.
    outputDataConfig :: Core.Maybe OutputDataConfig,
    -- | The Amazon Resource Name (ARN) that identifies the document classifier.
    documentClassifierArn :: Core.Maybe Core.Text,
    -- | The time that the document classification job completed.
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
    -- | The time that the document classification job was submitted for
    -- processing.
    submitTime :: Core.Maybe Core.POSIX,
    -- | The name that you assigned to the document classification job.
    jobName :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the AWS identity and Access Management
    -- (IAM) role that grants Amazon Comprehend read access to your input data.
    dataAccessRoleArn :: Core.Maybe Core.Text,
    -- | The identifier assigned to the document classification job.
    jobId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DocumentClassificationJobProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcConfig', 'documentClassificationJobProperties_vpcConfig' - Configuration parameters for a private Virtual Private Cloud (VPC)
-- containing the resources you are using for your document classification
-- job. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
--
-- 'inputDataConfig', 'documentClassificationJobProperties_inputDataConfig' - The input data configuration that you supplied when you created the
-- document classification job.
--
-- 'message', 'documentClassificationJobProperties_message' - A description of the status of the job.
--
-- 'jobStatus', 'documentClassificationJobProperties_jobStatus' - The current status of the document classification job. If the status is
-- @FAILED@, the @Message@ field shows the reason for the failure.
--
-- 'outputDataConfig', 'documentClassificationJobProperties_outputDataConfig' - The output data configuration that you supplied when you created the
-- document classification job.
--
-- 'documentClassifierArn', 'documentClassificationJobProperties_documentClassifierArn' - The Amazon Resource Name (ARN) that identifies the document classifier.
--
-- 'endTime', 'documentClassificationJobProperties_endTime' - The time that the document classification job completed.
--
-- 'volumeKmsKeyId', 'documentClassificationJobProperties_volumeKmsKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt data on the storage volume attached to the ML compute
-- instance(s) that process the analysis job. The VolumeKmsKeyId can be
-- either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- 'submitTime', 'documentClassificationJobProperties_submitTime' - The time that the document classification job was submitted for
-- processing.
--
-- 'jobName', 'documentClassificationJobProperties_jobName' - The name that you assigned to the document classification job.
--
-- 'dataAccessRoleArn', 'documentClassificationJobProperties_dataAccessRoleArn' - The Amazon Resource Name (ARN) of the AWS identity and Access Management
-- (IAM) role that grants Amazon Comprehend read access to your input data.
--
-- 'jobId', 'documentClassificationJobProperties_jobId' - The identifier assigned to the document classification job.
newDocumentClassificationJobProperties ::
  DocumentClassificationJobProperties
newDocumentClassificationJobProperties =
  DocumentClassificationJobProperties'
    { vpcConfig =
        Core.Nothing,
      inputDataConfig = Core.Nothing,
      message = Core.Nothing,
      jobStatus = Core.Nothing,
      outputDataConfig = Core.Nothing,
      documentClassifierArn = Core.Nothing,
      endTime = Core.Nothing,
      volumeKmsKeyId = Core.Nothing,
      submitTime = Core.Nothing,
      jobName = Core.Nothing,
      dataAccessRoleArn = Core.Nothing,
      jobId = Core.Nothing
    }

-- | Configuration parameters for a private Virtual Private Cloud (VPC)
-- containing the resources you are using for your document classification
-- job. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
documentClassificationJobProperties_vpcConfig :: Lens.Lens' DocumentClassificationJobProperties (Core.Maybe VpcConfig)
documentClassificationJobProperties_vpcConfig = Lens.lens (\DocumentClassificationJobProperties' {vpcConfig} -> vpcConfig) (\s@DocumentClassificationJobProperties' {} a -> s {vpcConfig = a} :: DocumentClassificationJobProperties)

-- | The input data configuration that you supplied when you created the
-- document classification job.
documentClassificationJobProperties_inputDataConfig :: Lens.Lens' DocumentClassificationJobProperties (Core.Maybe InputDataConfig)
documentClassificationJobProperties_inputDataConfig = Lens.lens (\DocumentClassificationJobProperties' {inputDataConfig} -> inputDataConfig) (\s@DocumentClassificationJobProperties' {} a -> s {inputDataConfig = a} :: DocumentClassificationJobProperties)

-- | A description of the status of the job.
documentClassificationJobProperties_message :: Lens.Lens' DocumentClassificationJobProperties (Core.Maybe Core.Text)
documentClassificationJobProperties_message = Lens.lens (\DocumentClassificationJobProperties' {message} -> message) (\s@DocumentClassificationJobProperties' {} a -> s {message = a} :: DocumentClassificationJobProperties)

-- | The current status of the document classification job. If the status is
-- @FAILED@, the @Message@ field shows the reason for the failure.
documentClassificationJobProperties_jobStatus :: Lens.Lens' DocumentClassificationJobProperties (Core.Maybe JobStatus)
documentClassificationJobProperties_jobStatus = Lens.lens (\DocumentClassificationJobProperties' {jobStatus} -> jobStatus) (\s@DocumentClassificationJobProperties' {} a -> s {jobStatus = a} :: DocumentClassificationJobProperties)

-- | The output data configuration that you supplied when you created the
-- document classification job.
documentClassificationJobProperties_outputDataConfig :: Lens.Lens' DocumentClassificationJobProperties (Core.Maybe OutputDataConfig)
documentClassificationJobProperties_outputDataConfig = Lens.lens (\DocumentClassificationJobProperties' {outputDataConfig} -> outputDataConfig) (\s@DocumentClassificationJobProperties' {} a -> s {outputDataConfig = a} :: DocumentClassificationJobProperties)

-- | The Amazon Resource Name (ARN) that identifies the document classifier.
documentClassificationJobProperties_documentClassifierArn :: Lens.Lens' DocumentClassificationJobProperties (Core.Maybe Core.Text)
documentClassificationJobProperties_documentClassifierArn = Lens.lens (\DocumentClassificationJobProperties' {documentClassifierArn} -> documentClassifierArn) (\s@DocumentClassificationJobProperties' {} a -> s {documentClassifierArn = a} :: DocumentClassificationJobProperties)

-- | The time that the document classification job completed.
documentClassificationJobProperties_endTime :: Lens.Lens' DocumentClassificationJobProperties (Core.Maybe Core.UTCTime)
documentClassificationJobProperties_endTime = Lens.lens (\DocumentClassificationJobProperties' {endTime} -> endTime) (\s@DocumentClassificationJobProperties' {} a -> s {endTime = a} :: DocumentClassificationJobProperties) Core.. Lens.mapping Core._Time

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt data on the storage volume attached to the ML compute
-- instance(s) that process the analysis job. The VolumeKmsKeyId can be
-- either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
documentClassificationJobProperties_volumeKmsKeyId :: Lens.Lens' DocumentClassificationJobProperties (Core.Maybe Core.Text)
documentClassificationJobProperties_volumeKmsKeyId = Lens.lens (\DocumentClassificationJobProperties' {volumeKmsKeyId} -> volumeKmsKeyId) (\s@DocumentClassificationJobProperties' {} a -> s {volumeKmsKeyId = a} :: DocumentClassificationJobProperties)

-- | The time that the document classification job was submitted for
-- processing.
documentClassificationJobProperties_submitTime :: Lens.Lens' DocumentClassificationJobProperties (Core.Maybe Core.UTCTime)
documentClassificationJobProperties_submitTime = Lens.lens (\DocumentClassificationJobProperties' {submitTime} -> submitTime) (\s@DocumentClassificationJobProperties' {} a -> s {submitTime = a} :: DocumentClassificationJobProperties) Core.. Lens.mapping Core._Time

-- | The name that you assigned to the document classification job.
documentClassificationJobProperties_jobName :: Lens.Lens' DocumentClassificationJobProperties (Core.Maybe Core.Text)
documentClassificationJobProperties_jobName = Lens.lens (\DocumentClassificationJobProperties' {jobName} -> jobName) (\s@DocumentClassificationJobProperties' {} a -> s {jobName = a} :: DocumentClassificationJobProperties)

-- | The Amazon Resource Name (ARN) of the AWS identity and Access Management
-- (IAM) role that grants Amazon Comprehend read access to your input data.
documentClassificationJobProperties_dataAccessRoleArn :: Lens.Lens' DocumentClassificationJobProperties (Core.Maybe Core.Text)
documentClassificationJobProperties_dataAccessRoleArn = Lens.lens (\DocumentClassificationJobProperties' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@DocumentClassificationJobProperties' {} a -> s {dataAccessRoleArn = a} :: DocumentClassificationJobProperties)

-- | The identifier assigned to the document classification job.
documentClassificationJobProperties_jobId :: Lens.Lens' DocumentClassificationJobProperties (Core.Maybe Core.Text)
documentClassificationJobProperties_jobId = Lens.lens (\DocumentClassificationJobProperties' {jobId} -> jobId) (\s@DocumentClassificationJobProperties' {} a -> s {jobId = a} :: DocumentClassificationJobProperties)

instance
  Core.FromJSON
    DocumentClassificationJobProperties
  where
  parseJSON =
    Core.withObject
      "DocumentClassificationJobProperties"
      ( \x ->
          DocumentClassificationJobProperties'
            Core.<$> (x Core..:? "VpcConfig")
            Core.<*> (x Core..:? "InputDataConfig")
            Core.<*> (x Core..:? "Message")
            Core.<*> (x Core..:? "JobStatus")
            Core.<*> (x Core..:? "OutputDataConfig")
            Core.<*> (x Core..:? "DocumentClassifierArn")
            Core.<*> (x Core..:? "EndTime")
            Core.<*> (x Core..:? "VolumeKmsKeyId")
            Core.<*> (x Core..:? "SubmitTime")
            Core.<*> (x Core..:? "JobName")
            Core.<*> (x Core..:? "DataAccessRoleArn")
            Core.<*> (x Core..:? "JobId")
      )

instance
  Core.Hashable
    DocumentClassificationJobProperties

instance
  Core.NFData
    DocumentClassificationJobProperties
