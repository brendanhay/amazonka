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
-- Module      : Amazonka.Comprehend.Types.EntitiesDetectionJobProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.EntitiesDetectionJobProperties where

import Amazonka.Comprehend.Types.InputDataConfig
import Amazonka.Comprehend.Types.JobStatus
import Amazonka.Comprehend.Types.LanguageCode
import Amazonka.Comprehend.Types.OutputDataConfig
import Amazonka.Comprehend.Types.VpcConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about an entities detection job.
--
-- /See:/ 'newEntitiesDetectionJobProperties' smart constructor.
data EntitiesDetectionJobProperties = EntitiesDetectionJobProperties'
  { -- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access
    -- to your input data.
    dataAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The time that the entities detection job completed
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) that identifies the entity recognizer.
    entityRecognizerArn :: Prelude.Maybe Prelude.Text,
    -- | The input data configuration that you supplied when you created the
    -- entities detection job.
    inputDataConfig :: Prelude.Maybe InputDataConfig,
    -- | The Amazon Resource Name (ARN) of the entities detection job. It is a
    -- unique, fully qualified identifier for the job. It includes the AWS
    -- account, Region, and the job ID. The format of the ARN is as follows:
    --
    -- @arn:\<partition>:comprehend:\<region>:\<account-id>:entities-detection-job\/\<job-id>@
    --
    -- The following is an example job ARN:
    --
    -- @arn:aws:comprehend:us-west-2:111122223333:entities-detection-job\/1234abcd12ab34cd56ef1234567890ab@
    jobArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier assigned to the entities detection job.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The name that you assigned the entities detection job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The current status of the entities detection job. If the status is
    -- @FAILED@, the @Message@ field shows the reason for the failure.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | The language code of the input documents.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | A description of the status of a job.
    message :: Prelude.Maybe Prelude.Text,
    -- | The output data configuration that you supplied when you created the
    -- entities detection job.
    outputDataConfig :: Prelude.Maybe OutputDataConfig,
    -- | The time that the entities detection job was submitted for processing.
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
    -- containing the resources you are using for your entity detection job.
    -- For more information, see
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
    vpcConfig :: Prelude.Maybe VpcConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EntitiesDetectionJobProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataAccessRoleArn', 'entitiesDetectionJobProperties_dataAccessRoleArn' - The Amazon Resource Name (ARN) that gives Amazon Comprehend read access
-- to your input data.
--
-- 'endTime', 'entitiesDetectionJobProperties_endTime' - The time that the entities detection job completed
--
-- 'entityRecognizerArn', 'entitiesDetectionJobProperties_entityRecognizerArn' - The Amazon Resource Name (ARN) that identifies the entity recognizer.
--
-- 'inputDataConfig', 'entitiesDetectionJobProperties_inputDataConfig' - The input data configuration that you supplied when you created the
-- entities detection job.
--
-- 'jobArn', 'entitiesDetectionJobProperties_jobArn' - The Amazon Resource Name (ARN) of the entities detection job. It is a
-- unique, fully qualified identifier for the job. It includes the AWS
-- account, Region, and the job ID. The format of the ARN is as follows:
--
-- @arn:\<partition>:comprehend:\<region>:\<account-id>:entities-detection-job\/\<job-id>@
--
-- The following is an example job ARN:
--
-- @arn:aws:comprehend:us-west-2:111122223333:entities-detection-job\/1234abcd12ab34cd56ef1234567890ab@
--
-- 'jobId', 'entitiesDetectionJobProperties_jobId' - The identifier assigned to the entities detection job.
--
-- 'jobName', 'entitiesDetectionJobProperties_jobName' - The name that you assigned the entities detection job.
--
-- 'jobStatus', 'entitiesDetectionJobProperties_jobStatus' - The current status of the entities detection job. If the status is
-- @FAILED@, the @Message@ field shows the reason for the failure.
--
-- 'languageCode', 'entitiesDetectionJobProperties_languageCode' - The language code of the input documents.
--
-- 'message', 'entitiesDetectionJobProperties_message' - A description of the status of a job.
--
-- 'outputDataConfig', 'entitiesDetectionJobProperties_outputDataConfig' - The output data configuration that you supplied when you created the
-- entities detection job.
--
-- 'submitTime', 'entitiesDetectionJobProperties_submitTime' - The time that the entities detection job was submitted for processing.
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
-- 'vpcConfig', 'entitiesDetectionJobProperties_vpcConfig' - Configuration parameters for a private Virtual Private Cloud (VPC)
-- containing the resources you are using for your entity detection job.
-- For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
newEntitiesDetectionJobProperties ::
  EntitiesDetectionJobProperties
newEntitiesDetectionJobProperties =
  EntitiesDetectionJobProperties'
    { dataAccessRoleArn =
        Prelude.Nothing,
      endTime = Prelude.Nothing,
      entityRecognizerArn = Prelude.Nothing,
      inputDataConfig = Prelude.Nothing,
      jobArn = Prelude.Nothing,
      jobId = Prelude.Nothing,
      jobName = Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      languageCode = Prelude.Nothing,
      message = Prelude.Nothing,
      outputDataConfig = Prelude.Nothing,
      submitTime = Prelude.Nothing,
      volumeKmsKeyId = Prelude.Nothing,
      vpcConfig = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access
-- to your input data.
entitiesDetectionJobProperties_dataAccessRoleArn :: Lens.Lens' EntitiesDetectionJobProperties (Prelude.Maybe Prelude.Text)
entitiesDetectionJobProperties_dataAccessRoleArn = Lens.lens (\EntitiesDetectionJobProperties' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@EntitiesDetectionJobProperties' {} a -> s {dataAccessRoleArn = a} :: EntitiesDetectionJobProperties)

-- | The time that the entities detection job completed
entitiesDetectionJobProperties_endTime :: Lens.Lens' EntitiesDetectionJobProperties (Prelude.Maybe Prelude.UTCTime)
entitiesDetectionJobProperties_endTime = Lens.lens (\EntitiesDetectionJobProperties' {endTime} -> endTime) (\s@EntitiesDetectionJobProperties' {} a -> s {endTime = a} :: EntitiesDetectionJobProperties) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) that identifies the entity recognizer.
entitiesDetectionJobProperties_entityRecognizerArn :: Lens.Lens' EntitiesDetectionJobProperties (Prelude.Maybe Prelude.Text)
entitiesDetectionJobProperties_entityRecognizerArn = Lens.lens (\EntitiesDetectionJobProperties' {entityRecognizerArn} -> entityRecognizerArn) (\s@EntitiesDetectionJobProperties' {} a -> s {entityRecognizerArn = a} :: EntitiesDetectionJobProperties)

-- | The input data configuration that you supplied when you created the
-- entities detection job.
entitiesDetectionJobProperties_inputDataConfig :: Lens.Lens' EntitiesDetectionJobProperties (Prelude.Maybe InputDataConfig)
entitiesDetectionJobProperties_inputDataConfig = Lens.lens (\EntitiesDetectionJobProperties' {inputDataConfig} -> inputDataConfig) (\s@EntitiesDetectionJobProperties' {} a -> s {inputDataConfig = a} :: EntitiesDetectionJobProperties)

-- | The Amazon Resource Name (ARN) of the entities detection job. It is a
-- unique, fully qualified identifier for the job. It includes the AWS
-- account, Region, and the job ID. The format of the ARN is as follows:
--
-- @arn:\<partition>:comprehend:\<region>:\<account-id>:entities-detection-job\/\<job-id>@
--
-- The following is an example job ARN:
--
-- @arn:aws:comprehend:us-west-2:111122223333:entities-detection-job\/1234abcd12ab34cd56ef1234567890ab@
entitiesDetectionJobProperties_jobArn :: Lens.Lens' EntitiesDetectionJobProperties (Prelude.Maybe Prelude.Text)
entitiesDetectionJobProperties_jobArn = Lens.lens (\EntitiesDetectionJobProperties' {jobArn} -> jobArn) (\s@EntitiesDetectionJobProperties' {} a -> s {jobArn = a} :: EntitiesDetectionJobProperties)

-- | The identifier assigned to the entities detection job.
entitiesDetectionJobProperties_jobId :: Lens.Lens' EntitiesDetectionJobProperties (Prelude.Maybe Prelude.Text)
entitiesDetectionJobProperties_jobId = Lens.lens (\EntitiesDetectionJobProperties' {jobId} -> jobId) (\s@EntitiesDetectionJobProperties' {} a -> s {jobId = a} :: EntitiesDetectionJobProperties)

-- | The name that you assigned the entities detection job.
entitiesDetectionJobProperties_jobName :: Lens.Lens' EntitiesDetectionJobProperties (Prelude.Maybe Prelude.Text)
entitiesDetectionJobProperties_jobName = Lens.lens (\EntitiesDetectionJobProperties' {jobName} -> jobName) (\s@EntitiesDetectionJobProperties' {} a -> s {jobName = a} :: EntitiesDetectionJobProperties)

-- | The current status of the entities detection job. If the status is
-- @FAILED@, the @Message@ field shows the reason for the failure.
entitiesDetectionJobProperties_jobStatus :: Lens.Lens' EntitiesDetectionJobProperties (Prelude.Maybe JobStatus)
entitiesDetectionJobProperties_jobStatus = Lens.lens (\EntitiesDetectionJobProperties' {jobStatus} -> jobStatus) (\s@EntitiesDetectionJobProperties' {} a -> s {jobStatus = a} :: EntitiesDetectionJobProperties)

-- | The language code of the input documents.
entitiesDetectionJobProperties_languageCode :: Lens.Lens' EntitiesDetectionJobProperties (Prelude.Maybe LanguageCode)
entitiesDetectionJobProperties_languageCode = Lens.lens (\EntitiesDetectionJobProperties' {languageCode} -> languageCode) (\s@EntitiesDetectionJobProperties' {} a -> s {languageCode = a} :: EntitiesDetectionJobProperties)

-- | A description of the status of a job.
entitiesDetectionJobProperties_message :: Lens.Lens' EntitiesDetectionJobProperties (Prelude.Maybe Prelude.Text)
entitiesDetectionJobProperties_message = Lens.lens (\EntitiesDetectionJobProperties' {message} -> message) (\s@EntitiesDetectionJobProperties' {} a -> s {message = a} :: EntitiesDetectionJobProperties)

-- | The output data configuration that you supplied when you created the
-- entities detection job.
entitiesDetectionJobProperties_outputDataConfig :: Lens.Lens' EntitiesDetectionJobProperties (Prelude.Maybe OutputDataConfig)
entitiesDetectionJobProperties_outputDataConfig = Lens.lens (\EntitiesDetectionJobProperties' {outputDataConfig} -> outputDataConfig) (\s@EntitiesDetectionJobProperties' {} a -> s {outputDataConfig = a} :: EntitiesDetectionJobProperties)

-- | The time that the entities detection job was submitted for processing.
entitiesDetectionJobProperties_submitTime :: Lens.Lens' EntitiesDetectionJobProperties (Prelude.Maybe Prelude.UTCTime)
entitiesDetectionJobProperties_submitTime = Lens.lens (\EntitiesDetectionJobProperties' {submitTime} -> submitTime) (\s@EntitiesDetectionJobProperties' {} a -> s {submitTime = a} :: EntitiesDetectionJobProperties) Prelude.. Lens.mapping Data._Time

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt data on the storage volume attached to the ML compute
-- instance(s) that process the analysis job. The VolumeKmsKeyId can be
-- either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
entitiesDetectionJobProperties_volumeKmsKeyId :: Lens.Lens' EntitiesDetectionJobProperties (Prelude.Maybe Prelude.Text)
entitiesDetectionJobProperties_volumeKmsKeyId = Lens.lens (\EntitiesDetectionJobProperties' {volumeKmsKeyId} -> volumeKmsKeyId) (\s@EntitiesDetectionJobProperties' {} a -> s {volumeKmsKeyId = a} :: EntitiesDetectionJobProperties)

-- | Configuration parameters for a private Virtual Private Cloud (VPC)
-- containing the resources you are using for your entity detection job.
-- For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
entitiesDetectionJobProperties_vpcConfig :: Lens.Lens' EntitiesDetectionJobProperties (Prelude.Maybe VpcConfig)
entitiesDetectionJobProperties_vpcConfig = Lens.lens (\EntitiesDetectionJobProperties' {vpcConfig} -> vpcConfig) (\s@EntitiesDetectionJobProperties' {} a -> s {vpcConfig = a} :: EntitiesDetectionJobProperties)

instance Data.FromJSON EntitiesDetectionJobProperties where
  parseJSON =
    Data.withObject
      "EntitiesDetectionJobProperties"
      ( \x ->
          EntitiesDetectionJobProperties'
            Prelude.<$> (x Data..:? "DataAccessRoleArn")
            Prelude.<*> (x Data..:? "EndTime")
            Prelude.<*> (x Data..:? "EntityRecognizerArn")
            Prelude.<*> (x Data..:? "InputDataConfig")
            Prelude.<*> (x Data..:? "JobArn")
            Prelude.<*> (x Data..:? "JobId")
            Prelude.<*> (x Data..:? "JobName")
            Prelude.<*> (x Data..:? "JobStatus")
            Prelude.<*> (x Data..:? "LanguageCode")
            Prelude.<*> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "OutputDataConfig")
            Prelude.<*> (x Data..:? "SubmitTime")
            Prelude.<*> (x Data..:? "VolumeKmsKeyId")
            Prelude.<*> (x Data..:? "VpcConfig")
      )

instance
  Prelude.Hashable
    EntitiesDetectionJobProperties
  where
  hashWithSalt
    _salt
    EntitiesDetectionJobProperties' {..} =
      _salt
        `Prelude.hashWithSalt` dataAccessRoleArn
        `Prelude.hashWithSalt` endTime
        `Prelude.hashWithSalt` entityRecognizerArn
        `Prelude.hashWithSalt` inputDataConfig
        `Prelude.hashWithSalt` jobArn
        `Prelude.hashWithSalt` jobId
        `Prelude.hashWithSalt` jobName
        `Prelude.hashWithSalt` jobStatus
        `Prelude.hashWithSalt` languageCode
        `Prelude.hashWithSalt` message
        `Prelude.hashWithSalt` outputDataConfig
        `Prelude.hashWithSalt` submitTime
        `Prelude.hashWithSalt` volumeKmsKeyId
        `Prelude.hashWithSalt` vpcConfig

instance
  Prelude.NFData
    EntitiesDetectionJobProperties
  where
  rnf EntitiesDetectionJobProperties' {..} =
    Prelude.rnf dataAccessRoleArn
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf entityRecognizerArn
      `Prelude.seq` Prelude.rnf inputDataConfig
      `Prelude.seq` Prelude.rnf jobArn
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf outputDataConfig
      `Prelude.seq` Prelude.rnf submitTime
      `Prelude.seq` Prelude.rnf volumeKmsKeyId
      `Prelude.seq` Prelude.rnf vpcConfig
