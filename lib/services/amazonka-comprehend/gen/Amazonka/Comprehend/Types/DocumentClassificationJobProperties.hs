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
-- Module      : Amazonka.Comprehend.Types.DocumentClassificationJobProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.DocumentClassificationJobProperties where

import Amazonka.Comprehend.Types.InputDataConfig
import Amazonka.Comprehend.Types.JobStatus
import Amazonka.Comprehend.Types.OutputDataConfig
import Amazonka.Comprehend.Types.VpcConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about a document classification job.
--
-- /See:/ 'newDocumentClassificationJobProperties' smart constructor.
data DocumentClassificationJobProperties = DocumentClassificationJobProperties'
  { -- | The Amazon Resource Name (ARN) of the AWS identity and Access Management
    -- (IAM) role that grants Amazon Comprehend read access to your input data.
    dataAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) that identifies the document classifier.
    documentClassifierArn :: Prelude.Maybe Prelude.Text,
    -- | The time that the document classification job completed.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The input data configuration that you supplied when you created the
    -- document classification job.
    inputDataConfig :: Prelude.Maybe InputDataConfig,
    -- | The Amazon Resource Name (ARN) of the document classification job. It is
    -- a unique, fully qualified identifier for the job. It includes the AWS
    -- account, Region, and the job ID. The format of the ARN is as follows:
    --
    -- @arn:\<partition>:comprehend:\<region>:\<account-id>:document-classification-job\/\<job-id>@
    --
    -- The following is an example job ARN:
    --
    -- @arn:aws:comprehend:us-west-2:111122223333:document-classification-job\/1234abcd12ab34cd56ef1234567890ab@
    jobArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier assigned to the document classification job.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The name that you assigned to the document classification job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The current status of the document classification job. If the status is
    -- @FAILED@, the @Message@ field shows the reason for the failure.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | A description of the status of the job.
    message :: Prelude.Maybe Prelude.Text,
    -- | The output data configuration that you supplied when you created the
    -- document classification job.
    outputDataConfig :: Prelude.Maybe OutputDataConfig,
    -- | The time that the document classification job was submitted for
    -- processing.
    submitTime :: Prelude.Maybe Data.POSIX,
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
    -- | Configuration parameters for a private Virtual Private Cloud (VPC)
    -- containing the resources you are using for your document classification
    -- job. For more information, see
    -- <https://docs.aws.amazon.com/vppc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
    vpcConfig :: Prelude.Maybe VpcConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DocumentClassificationJobProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataAccessRoleArn', 'documentClassificationJobProperties_dataAccessRoleArn' - The Amazon Resource Name (ARN) of the AWS identity and Access Management
-- (IAM) role that grants Amazon Comprehend read access to your input data.
--
-- 'documentClassifierArn', 'documentClassificationJobProperties_documentClassifierArn' - The Amazon Resource Name (ARN) that identifies the document classifier.
--
-- 'endTime', 'documentClassificationJobProperties_endTime' - The time that the document classification job completed.
--
-- 'inputDataConfig', 'documentClassificationJobProperties_inputDataConfig' - The input data configuration that you supplied when you created the
-- document classification job.
--
-- 'jobArn', 'documentClassificationJobProperties_jobArn' - The Amazon Resource Name (ARN) of the document classification job. It is
-- a unique, fully qualified identifier for the job. It includes the AWS
-- account, Region, and the job ID. The format of the ARN is as follows:
--
-- @arn:\<partition>:comprehend:\<region>:\<account-id>:document-classification-job\/\<job-id>@
--
-- The following is an example job ARN:
--
-- @arn:aws:comprehend:us-west-2:111122223333:document-classification-job\/1234abcd12ab34cd56ef1234567890ab@
--
-- 'jobId', 'documentClassificationJobProperties_jobId' - The identifier assigned to the document classification job.
--
-- 'jobName', 'documentClassificationJobProperties_jobName' - The name that you assigned to the document classification job.
--
-- 'jobStatus', 'documentClassificationJobProperties_jobStatus' - The current status of the document classification job. If the status is
-- @FAILED@, the @Message@ field shows the reason for the failure.
--
-- 'message', 'documentClassificationJobProperties_message' - A description of the status of the job.
--
-- 'outputDataConfig', 'documentClassificationJobProperties_outputDataConfig' - The output data configuration that you supplied when you created the
-- document classification job.
--
-- 'submitTime', 'documentClassificationJobProperties_submitTime' - The time that the document classification job was submitted for
-- processing.
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
-- 'vpcConfig', 'documentClassificationJobProperties_vpcConfig' - Configuration parameters for a private Virtual Private Cloud (VPC)
-- containing the resources you are using for your document classification
-- job. For more information, see
-- <https://docs.aws.amazon.com/vppc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
newDocumentClassificationJobProperties ::
  DocumentClassificationJobProperties
newDocumentClassificationJobProperties =
  DocumentClassificationJobProperties'
    { dataAccessRoleArn =
        Prelude.Nothing,
      documentClassifierArn =
        Prelude.Nothing,
      endTime = Prelude.Nothing,
      inputDataConfig = Prelude.Nothing,
      jobArn = Prelude.Nothing,
      jobId = Prelude.Nothing,
      jobName = Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      message = Prelude.Nothing,
      outputDataConfig = Prelude.Nothing,
      submitTime = Prelude.Nothing,
      volumeKmsKeyId = Prelude.Nothing,
      vpcConfig = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the AWS identity and Access Management
-- (IAM) role that grants Amazon Comprehend read access to your input data.
documentClassificationJobProperties_dataAccessRoleArn :: Lens.Lens' DocumentClassificationJobProperties (Prelude.Maybe Prelude.Text)
documentClassificationJobProperties_dataAccessRoleArn = Lens.lens (\DocumentClassificationJobProperties' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@DocumentClassificationJobProperties' {} a -> s {dataAccessRoleArn = a} :: DocumentClassificationJobProperties)

-- | The Amazon Resource Name (ARN) that identifies the document classifier.
documentClassificationJobProperties_documentClassifierArn :: Lens.Lens' DocumentClassificationJobProperties (Prelude.Maybe Prelude.Text)
documentClassificationJobProperties_documentClassifierArn = Lens.lens (\DocumentClassificationJobProperties' {documentClassifierArn} -> documentClassifierArn) (\s@DocumentClassificationJobProperties' {} a -> s {documentClassifierArn = a} :: DocumentClassificationJobProperties)

-- | The time that the document classification job completed.
documentClassificationJobProperties_endTime :: Lens.Lens' DocumentClassificationJobProperties (Prelude.Maybe Prelude.UTCTime)
documentClassificationJobProperties_endTime = Lens.lens (\DocumentClassificationJobProperties' {endTime} -> endTime) (\s@DocumentClassificationJobProperties' {} a -> s {endTime = a} :: DocumentClassificationJobProperties) Prelude.. Lens.mapping Data._Time

-- | The input data configuration that you supplied when you created the
-- document classification job.
documentClassificationJobProperties_inputDataConfig :: Lens.Lens' DocumentClassificationJobProperties (Prelude.Maybe InputDataConfig)
documentClassificationJobProperties_inputDataConfig = Lens.lens (\DocumentClassificationJobProperties' {inputDataConfig} -> inputDataConfig) (\s@DocumentClassificationJobProperties' {} a -> s {inputDataConfig = a} :: DocumentClassificationJobProperties)

-- | The Amazon Resource Name (ARN) of the document classification job. It is
-- a unique, fully qualified identifier for the job. It includes the AWS
-- account, Region, and the job ID. The format of the ARN is as follows:
--
-- @arn:\<partition>:comprehend:\<region>:\<account-id>:document-classification-job\/\<job-id>@
--
-- The following is an example job ARN:
--
-- @arn:aws:comprehend:us-west-2:111122223333:document-classification-job\/1234abcd12ab34cd56ef1234567890ab@
documentClassificationJobProperties_jobArn :: Lens.Lens' DocumentClassificationJobProperties (Prelude.Maybe Prelude.Text)
documentClassificationJobProperties_jobArn = Lens.lens (\DocumentClassificationJobProperties' {jobArn} -> jobArn) (\s@DocumentClassificationJobProperties' {} a -> s {jobArn = a} :: DocumentClassificationJobProperties)

-- | The identifier assigned to the document classification job.
documentClassificationJobProperties_jobId :: Lens.Lens' DocumentClassificationJobProperties (Prelude.Maybe Prelude.Text)
documentClassificationJobProperties_jobId = Lens.lens (\DocumentClassificationJobProperties' {jobId} -> jobId) (\s@DocumentClassificationJobProperties' {} a -> s {jobId = a} :: DocumentClassificationJobProperties)

-- | The name that you assigned to the document classification job.
documentClassificationJobProperties_jobName :: Lens.Lens' DocumentClassificationJobProperties (Prelude.Maybe Prelude.Text)
documentClassificationJobProperties_jobName = Lens.lens (\DocumentClassificationJobProperties' {jobName} -> jobName) (\s@DocumentClassificationJobProperties' {} a -> s {jobName = a} :: DocumentClassificationJobProperties)

-- | The current status of the document classification job. If the status is
-- @FAILED@, the @Message@ field shows the reason for the failure.
documentClassificationJobProperties_jobStatus :: Lens.Lens' DocumentClassificationJobProperties (Prelude.Maybe JobStatus)
documentClassificationJobProperties_jobStatus = Lens.lens (\DocumentClassificationJobProperties' {jobStatus} -> jobStatus) (\s@DocumentClassificationJobProperties' {} a -> s {jobStatus = a} :: DocumentClassificationJobProperties)

-- | A description of the status of the job.
documentClassificationJobProperties_message :: Lens.Lens' DocumentClassificationJobProperties (Prelude.Maybe Prelude.Text)
documentClassificationJobProperties_message = Lens.lens (\DocumentClassificationJobProperties' {message} -> message) (\s@DocumentClassificationJobProperties' {} a -> s {message = a} :: DocumentClassificationJobProperties)

-- | The output data configuration that you supplied when you created the
-- document classification job.
documentClassificationJobProperties_outputDataConfig :: Lens.Lens' DocumentClassificationJobProperties (Prelude.Maybe OutputDataConfig)
documentClassificationJobProperties_outputDataConfig = Lens.lens (\DocumentClassificationJobProperties' {outputDataConfig} -> outputDataConfig) (\s@DocumentClassificationJobProperties' {} a -> s {outputDataConfig = a} :: DocumentClassificationJobProperties)

-- | The time that the document classification job was submitted for
-- processing.
documentClassificationJobProperties_submitTime :: Lens.Lens' DocumentClassificationJobProperties (Prelude.Maybe Prelude.UTCTime)
documentClassificationJobProperties_submitTime = Lens.lens (\DocumentClassificationJobProperties' {submitTime} -> submitTime) (\s@DocumentClassificationJobProperties' {} a -> s {submitTime = a} :: DocumentClassificationJobProperties) Prelude.. Lens.mapping Data._Time

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

-- | Configuration parameters for a private Virtual Private Cloud (VPC)
-- containing the resources you are using for your document classification
-- job. For more information, see
-- <https://docs.aws.amazon.com/vppc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
documentClassificationJobProperties_vpcConfig :: Lens.Lens' DocumentClassificationJobProperties (Prelude.Maybe VpcConfig)
documentClassificationJobProperties_vpcConfig = Lens.lens (\DocumentClassificationJobProperties' {vpcConfig} -> vpcConfig) (\s@DocumentClassificationJobProperties' {} a -> s {vpcConfig = a} :: DocumentClassificationJobProperties)

instance
  Data.FromJSON
    DocumentClassificationJobProperties
  where
  parseJSON =
    Data.withObject
      "DocumentClassificationJobProperties"
      ( \x ->
          DocumentClassificationJobProperties'
            Prelude.<$> (x Data..:? "DataAccessRoleArn")
            Prelude.<*> (x Data..:? "DocumentClassifierArn")
            Prelude.<*> (x Data..:? "EndTime")
            Prelude.<*> (x Data..:? "InputDataConfig")
            Prelude.<*> (x Data..:? "JobArn")
            Prelude.<*> (x Data..:? "JobId")
            Prelude.<*> (x Data..:? "JobName")
            Prelude.<*> (x Data..:? "JobStatus")
            Prelude.<*> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "OutputDataConfig")
            Prelude.<*> (x Data..:? "SubmitTime")
            Prelude.<*> (x Data..:? "VolumeKmsKeyId")
            Prelude.<*> (x Data..:? "VpcConfig")
      )

instance
  Prelude.Hashable
    DocumentClassificationJobProperties
  where
  hashWithSalt
    _salt
    DocumentClassificationJobProperties' {..} =
      _salt
        `Prelude.hashWithSalt` dataAccessRoleArn
        `Prelude.hashWithSalt` documentClassifierArn
        `Prelude.hashWithSalt` endTime
        `Prelude.hashWithSalt` inputDataConfig
        `Prelude.hashWithSalt` jobArn
        `Prelude.hashWithSalt` jobId
        `Prelude.hashWithSalt` jobName
        `Prelude.hashWithSalt` jobStatus
        `Prelude.hashWithSalt` message
        `Prelude.hashWithSalt` outputDataConfig
        `Prelude.hashWithSalt` submitTime
        `Prelude.hashWithSalt` volumeKmsKeyId
        `Prelude.hashWithSalt` vpcConfig

instance
  Prelude.NFData
    DocumentClassificationJobProperties
  where
  rnf DocumentClassificationJobProperties' {..} =
    Prelude.rnf dataAccessRoleArn
      `Prelude.seq` Prelude.rnf documentClassifierArn
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf inputDataConfig
      `Prelude.seq` Prelude.rnf jobArn
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf outputDataConfig
      `Prelude.seq` Prelude.rnf submitTime
      `Prelude.seq` Prelude.rnf volumeKmsKeyId
      `Prelude.seq` Prelude.rnf vpcConfig
