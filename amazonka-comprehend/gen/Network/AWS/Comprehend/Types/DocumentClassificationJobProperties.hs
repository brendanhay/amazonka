{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about a document classification job.
--
-- /See:/ 'newDocumentClassificationJobProperties' smart constructor.
data DocumentClassificationJobProperties = DocumentClassificationJobProperties'
  { -- | Configuration parameters for a private Virtual Private Cloud (VPC)
    -- containing the resources you are using for your document classification
    -- job. For more information, see
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
    vpcConfig :: Prelude.Maybe VpcConfig,
    -- | The input data configuration that you supplied when you created the
    -- document classification job.
    inputDataConfig :: Prelude.Maybe InputDataConfig,
    -- | A description of the status of the job.
    message :: Prelude.Maybe Prelude.Text,
    -- | The current status of the document classification job. If the status is
    -- @FAILED@, the @Message@ field shows the reason for the failure.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | The output data configuration that you supplied when you created the
    -- document classification job.
    outputDataConfig :: Prelude.Maybe OutputDataConfig,
    -- | The Amazon Resource Name (ARN) that identifies the document classifier.
    documentClassifierArn :: Prelude.Maybe Prelude.Text,
    -- | The time that the document classification job completed.
    endTime :: Prelude.Maybe Prelude.POSIX,
    -- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
    -- uses to encrypt data on the storage volume attached to the ML compute
    -- instance(s) that process the analysis job. The VolumeKmsKeyId can be
    -- either of the following formats:
    --
    -- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
    --
    -- -   Amazon Resource Name (ARN) of a KMS Key:
    --     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
    volumeKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The time that the document classification job was submitted for
    -- processing.
    submitTime :: Prelude.Maybe Prelude.POSIX,
    -- | The name that you assigned to the document classification job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the AWS identity and Access Management
    -- (IAM) role that grants Amazon Comprehend read access to your input data.
    dataAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier assigned to the document classification job.
    jobId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      inputDataConfig = Prelude.Nothing,
      message = Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      outputDataConfig = Prelude.Nothing,
      documentClassifierArn =
        Prelude.Nothing,
      endTime = Prelude.Nothing,
      volumeKmsKeyId = Prelude.Nothing,
      submitTime = Prelude.Nothing,
      jobName = Prelude.Nothing,
      dataAccessRoleArn = Prelude.Nothing,
      jobId = Prelude.Nothing
    }

-- | Configuration parameters for a private Virtual Private Cloud (VPC)
-- containing the resources you are using for your document classification
-- job. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
documentClassificationJobProperties_vpcConfig :: Lens.Lens' DocumentClassificationJobProperties (Prelude.Maybe VpcConfig)
documentClassificationJobProperties_vpcConfig = Lens.lens (\DocumentClassificationJobProperties' {vpcConfig} -> vpcConfig) (\s@DocumentClassificationJobProperties' {} a -> s {vpcConfig = a} :: DocumentClassificationJobProperties)

-- | The input data configuration that you supplied when you created the
-- document classification job.
documentClassificationJobProperties_inputDataConfig :: Lens.Lens' DocumentClassificationJobProperties (Prelude.Maybe InputDataConfig)
documentClassificationJobProperties_inputDataConfig = Lens.lens (\DocumentClassificationJobProperties' {inputDataConfig} -> inputDataConfig) (\s@DocumentClassificationJobProperties' {} a -> s {inputDataConfig = a} :: DocumentClassificationJobProperties)

-- | A description of the status of the job.
documentClassificationJobProperties_message :: Lens.Lens' DocumentClassificationJobProperties (Prelude.Maybe Prelude.Text)
documentClassificationJobProperties_message = Lens.lens (\DocumentClassificationJobProperties' {message} -> message) (\s@DocumentClassificationJobProperties' {} a -> s {message = a} :: DocumentClassificationJobProperties)

-- | The current status of the document classification job. If the status is
-- @FAILED@, the @Message@ field shows the reason for the failure.
documentClassificationJobProperties_jobStatus :: Lens.Lens' DocumentClassificationJobProperties (Prelude.Maybe JobStatus)
documentClassificationJobProperties_jobStatus = Lens.lens (\DocumentClassificationJobProperties' {jobStatus} -> jobStatus) (\s@DocumentClassificationJobProperties' {} a -> s {jobStatus = a} :: DocumentClassificationJobProperties)

-- | The output data configuration that you supplied when you created the
-- document classification job.
documentClassificationJobProperties_outputDataConfig :: Lens.Lens' DocumentClassificationJobProperties (Prelude.Maybe OutputDataConfig)
documentClassificationJobProperties_outputDataConfig = Lens.lens (\DocumentClassificationJobProperties' {outputDataConfig} -> outputDataConfig) (\s@DocumentClassificationJobProperties' {} a -> s {outputDataConfig = a} :: DocumentClassificationJobProperties)

-- | The Amazon Resource Name (ARN) that identifies the document classifier.
documentClassificationJobProperties_documentClassifierArn :: Lens.Lens' DocumentClassificationJobProperties (Prelude.Maybe Prelude.Text)
documentClassificationJobProperties_documentClassifierArn = Lens.lens (\DocumentClassificationJobProperties' {documentClassifierArn} -> documentClassifierArn) (\s@DocumentClassificationJobProperties' {} a -> s {documentClassifierArn = a} :: DocumentClassificationJobProperties)

-- | The time that the document classification job completed.
documentClassificationJobProperties_endTime :: Lens.Lens' DocumentClassificationJobProperties (Prelude.Maybe Prelude.UTCTime)
documentClassificationJobProperties_endTime = Lens.lens (\DocumentClassificationJobProperties' {endTime} -> endTime) (\s@DocumentClassificationJobProperties' {} a -> s {endTime = a} :: DocumentClassificationJobProperties) Prelude.. Lens.mapping Prelude._Time

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt data on the storage volume attached to the ML compute
-- instance(s) that process the analysis job. The VolumeKmsKeyId can be
-- either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
documentClassificationJobProperties_volumeKmsKeyId :: Lens.Lens' DocumentClassificationJobProperties (Prelude.Maybe Prelude.Text)
documentClassificationJobProperties_volumeKmsKeyId = Lens.lens (\DocumentClassificationJobProperties' {volumeKmsKeyId} -> volumeKmsKeyId) (\s@DocumentClassificationJobProperties' {} a -> s {volumeKmsKeyId = a} :: DocumentClassificationJobProperties)

-- | The time that the document classification job was submitted for
-- processing.
documentClassificationJobProperties_submitTime :: Lens.Lens' DocumentClassificationJobProperties (Prelude.Maybe Prelude.UTCTime)
documentClassificationJobProperties_submitTime = Lens.lens (\DocumentClassificationJobProperties' {submitTime} -> submitTime) (\s@DocumentClassificationJobProperties' {} a -> s {submitTime = a} :: DocumentClassificationJobProperties) Prelude.. Lens.mapping Prelude._Time

-- | The name that you assigned to the document classification job.
documentClassificationJobProperties_jobName :: Lens.Lens' DocumentClassificationJobProperties (Prelude.Maybe Prelude.Text)
documentClassificationJobProperties_jobName = Lens.lens (\DocumentClassificationJobProperties' {jobName} -> jobName) (\s@DocumentClassificationJobProperties' {} a -> s {jobName = a} :: DocumentClassificationJobProperties)

-- | The Amazon Resource Name (ARN) of the AWS identity and Access Management
-- (IAM) role that grants Amazon Comprehend read access to your input data.
documentClassificationJobProperties_dataAccessRoleArn :: Lens.Lens' DocumentClassificationJobProperties (Prelude.Maybe Prelude.Text)
documentClassificationJobProperties_dataAccessRoleArn = Lens.lens (\DocumentClassificationJobProperties' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@DocumentClassificationJobProperties' {} a -> s {dataAccessRoleArn = a} :: DocumentClassificationJobProperties)

-- | The identifier assigned to the document classification job.
documentClassificationJobProperties_jobId :: Lens.Lens' DocumentClassificationJobProperties (Prelude.Maybe Prelude.Text)
documentClassificationJobProperties_jobId = Lens.lens (\DocumentClassificationJobProperties' {jobId} -> jobId) (\s@DocumentClassificationJobProperties' {} a -> s {jobId = a} :: DocumentClassificationJobProperties)

instance
  Prelude.FromJSON
    DocumentClassificationJobProperties
  where
  parseJSON =
    Prelude.withObject
      "DocumentClassificationJobProperties"
      ( \x ->
          DocumentClassificationJobProperties'
            Prelude.<$> (x Prelude..:? "VpcConfig")
            Prelude.<*> (x Prelude..:? "InputDataConfig")
            Prelude.<*> (x Prelude..:? "Message")
            Prelude.<*> (x Prelude..:? "JobStatus")
            Prelude.<*> (x Prelude..:? "OutputDataConfig")
            Prelude.<*> (x Prelude..:? "DocumentClassifierArn")
            Prelude.<*> (x Prelude..:? "EndTime")
            Prelude.<*> (x Prelude..:? "VolumeKmsKeyId")
            Prelude.<*> (x Prelude..:? "SubmitTime")
            Prelude.<*> (x Prelude..:? "JobName")
            Prelude.<*> (x Prelude..:? "DataAccessRoleArn")
            Prelude.<*> (x Prelude..:? "JobId")
      )

instance
  Prelude.Hashable
    DocumentClassificationJobProperties

instance
  Prelude.NFData
    DocumentClassificationJobProperties
