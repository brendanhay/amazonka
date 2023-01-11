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
-- Module      : Amazonka.ComprehendMedical.Types.ComprehendMedicalAsyncJobProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComprehendMedical.Types.ComprehendMedicalAsyncJobProperties where

import Amazonka.ComprehendMedical.Types.InputDataConfig
import Amazonka.ComprehendMedical.Types.JobStatus
import Amazonka.ComprehendMedical.Types.LanguageCode
import Amazonka.ComprehendMedical.Types.OutputDataConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about a detection job.
--
-- /See:/ 'newComprehendMedicalAsyncJobProperties' smart constructor.
data ComprehendMedicalAsyncJobProperties = ComprehendMedicalAsyncJobProperties'
  { -- | The Amazon Resource Name (ARN) that gives Comprehend Medical; read
    -- access to your input data.
    dataAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The time that the detection job completed.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The date and time that job metadata is deleted from the server. Output
    -- files in your S3 bucket will not be deleted. After the metadata is
    -- deleted, the job will no longer appear in the results of the
    -- @ListEntitiesDetectionV2Job@ or the @ListPHIDetectionJobs@ operation.
    expirationTime :: Prelude.Maybe Data.POSIX,
    -- | The input data configuration that you supplied when you created the
    -- detection job.
    inputDataConfig :: Prelude.Maybe InputDataConfig,
    -- | The identifier assigned to the detection job.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The name that you assigned to the detection job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The current status of the detection job. If the status is @FAILED@, the
    -- @Message@ field shows the reason for the failure.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | The AWS Key Management Service key, if any, used to encrypt the output
    -- files.
    kmsKey :: Prelude.Maybe Prelude.Text,
    -- | The language code of the input documents.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | The path to the file that describes the results of a batch job.
    manifestFilePath :: Prelude.Maybe Prelude.Text,
    -- | A description of the status of a job.
    message :: Prelude.Maybe Prelude.Text,
    -- | The version of the model used to analyze the documents. The version
    -- number looks like X.X.X. You can use this information to track the model
    -- used for a particular batch of documents.
    modelVersion :: Prelude.Maybe Prelude.Text,
    -- | The output data configuration that you supplied when you created the
    -- detection job.
    outputDataConfig :: Prelude.Maybe OutputDataConfig,
    -- | The time that the detection job was submitted for processing.
    submitTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComprehendMedicalAsyncJobProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataAccessRoleArn', 'comprehendMedicalAsyncJobProperties_dataAccessRoleArn' - The Amazon Resource Name (ARN) that gives Comprehend Medical; read
-- access to your input data.
--
-- 'endTime', 'comprehendMedicalAsyncJobProperties_endTime' - The time that the detection job completed.
--
-- 'expirationTime', 'comprehendMedicalAsyncJobProperties_expirationTime' - The date and time that job metadata is deleted from the server. Output
-- files in your S3 bucket will not be deleted. After the metadata is
-- deleted, the job will no longer appear in the results of the
-- @ListEntitiesDetectionV2Job@ or the @ListPHIDetectionJobs@ operation.
--
-- 'inputDataConfig', 'comprehendMedicalAsyncJobProperties_inputDataConfig' - The input data configuration that you supplied when you created the
-- detection job.
--
-- 'jobId', 'comprehendMedicalAsyncJobProperties_jobId' - The identifier assigned to the detection job.
--
-- 'jobName', 'comprehendMedicalAsyncJobProperties_jobName' - The name that you assigned to the detection job.
--
-- 'jobStatus', 'comprehendMedicalAsyncJobProperties_jobStatus' - The current status of the detection job. If the status is @FAILED@, the
-- @Message@ field shows the reason for the failure.
--
-- 'kmsKey', 'comprehendMedicalAsyncJobProperties_kmsKey' - The AWS Key Management Service key, if any, used to encrypt the output
-- files.
--
-- 'languageCode', 'comprehendMedicalAsyncJobProperties_languageCode' - The language code of the input documents.
--
-- 'manifestFilePath', 'comprehendMedicalAsyncJobProperties_manifestFilePath' - The path to the file that describes the results of a batch job.
--
-- 'message', 'comprehendMedicalAsyncJobProperties_message' - A description of the status of a job.
--
-- 'modelVersion', 'comprehendMedicalAsyncJobProperties_modelVersion' - The version of the model used to analyze the documents. The version
-- number looks like X.X.X. You can use this information to track the model
-- used for a particular batch of documents.
--
-- 'outputDataConfig', 'comprehendMedicalAsyncJobProperties_outputDataConfig' - The output data configuration that you supplied when you created the
-- detection job.
--
-- 'submitTime', 'comprehendMedicalAsyncJobProperties_submitTime' - The time that the detection job was submitted for processing.
newComprehendMedicalAsyncJobProperties ::
  ComprehendMedicalAsyncJobProperties
newComprehendMedicalAsyncJobProperties =
  ComprehendMedicalAsyncJobProperties'
    { dataAccessRoleArn =
        Prelude.Nothing,
      endTime = Prelude.Nothing,
      expirationTime = Prelude.Nothing,
      inputDataConfig = Prelude.Nothing,
      jobId = Prelude.Nothing,
      jobName = Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      kmsKey = Prelude.Nothing,
      languageCode = Prelude.Nothing,
      manifestFilePath = Prelude.Nothing,
      message = Prelude.Nothing,
      modelVersion = Prelude.Nothing,
      outputDataConfig = Prelude.Nothing,
      submitTime = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) that gives Comprehend Medical; read
-- access to your input data.
comprehendMedicalAsyncJobProperties_dataAccessRoleArn :: Lens.Lens' ComprehendMedicalAsyncJobProperties (Prelude.Maybe Prelude.Text)
comprehendMedicalAsyncJobProperties_dataAccessRoleArn = Lens.lens (\ComprehendMedicalAsyncJobProperties' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@ComprehendMedicalAsyncJobProperties' {} a -> s {dataAccessRoleArn = a} :: ComprehendMedicalAsyncJobProperties)

-- | The time that the detection job completed.
comprehendMedicalAsyncJobProperties_endTime :: Lens.Lens' ComprehendMedicalAsyncJobProperties (Prelude.Maybe Prelude.UTCTime)
comprehendMedicalAsyncJobProperties_endTime = Lens.lens (\ComprehendMedicalAsyncJobProperties' {endTime} -> endTime) (\s@ComprehendMedicalAsyncJobProperties' {} a -> s {endTime = a} :: ComprehendMedicalAsyncJobProperties) Prelude.. Lens.mapping Data._Time

-- | The date and time that job metadata is deleted from the server. Output
-- files in your S3 bucket will not be deleted. After the metadata is
-- deleted, the job will no longer appear in the results of the
-- @ListEntitiesDetectionV2Job@ or the @ListPHIDetectionJobs@ operation.
comprehendMedicalAsyncJobProperties_expirationTime :: Lens.Lens' ComprehendMedicalAsyncJobProperties (Prelude.Maybe Prelude.UTCTime)
comprehendMedicalAsyncJobProperties_expirationTime = Lens.lens (\ComprehendMedicalAsyncJobProperties' {expirationTime} -> expirationTime) (\s@ComprehendMedicalAsyncJobProperties' {} a -> s {expirationTime = a} :: ComprehendMedicalAsyncJobProperties) Prelude.. Lens.mapping Data._Time

-- | The input data configuration that you supplied when you created the
-- detection job.
comprehendMedicalAsyncJobProperties_inputDataConfig :: Lens.Lens' ComprehendMedicalAsyncJobProperties (Prelude.Maybe InputDataConfig)
comprehendMedicalAsyncJobProperties_inputDataConfig = Lens.lens (\ComprehendMedicalAsyncJobProperties' {inputDataConfig} -> inputDataConfig) (\s@ComprehendMedicalAsyncJobProperties' {} a -> s {inputDataConfig = a} :: ComprehendMedicalAsyncJobProperties)

-- | The identifier assigned to the detection job.
comprehendMedicalAsyncJobProperties_jobId :: Lens.Lens' ComprehendMedicalAsyncJobProperties (Prelude.Maybe Prelude.Text)
comprehendMedicalAsyncJobProperties_jobId = Lens.lens (\ComprehendMedicalAsyncJobProperties' {jobId} -> jobId) (\s@ComprehendMedicalAsyncJobProperties' {} a -> s {jobId = a} :: ComprehendMedicalAsyncJobProperties)

-- | The name that you assigned to the detection job.
comprehendMedicalAsyncJobProperties_jobName :: Lens.Lens' ComprehendMedicalAsyncJobProperties (Prelude.Maybe Prelude.Text)
comprehendMedicalAsyncJobProperties_jobName = Lens.lens (\ComprehendMedicalAsyncJobProperties' {jobName} -> jobName) (\s@ComprehendMedicalAsyncJobProperties' {} a -> s {jobName = a} :: ComprehendMedicalAsyncJobProperties)

-- | The current status of the detection job. If the status is @FAILED@, the
-- @Message@ field shows the reason for the failure.
comprehendMedicalAsyncJobProperties_jobStatus :: Lens.Lens' ComprehendMedicalAsyncJobProperties (Prelude.Maybe JobStatus)
comprehendMedicalAsyncJobProperties_jobStatus = Lens.lens (\ComprehendMedicalAsyncJobProperties' {jobStatus} -> jobStatus) (\s@ComprehendMedicalAsyncJobProperties' {} a -> s {jobStatus = a} :: ComprehendMedicalAsyncJobProperties)

-- | The AWS Key Management Service key, if any, used to encrypt the output
-- files.
comprehendMedicalAsyncJobProperties_kmsKey :: Lens.Lens' ComprehendMedicalAsyncJobProperties (Prelude.Maybe Prelude.Text)
comprehendMedicalAsyncJobProperties_kmsKey = Lens.lens (\ComprehendMedicalAsyncJobProperties' {kmsKey} -> kmsKey) (\s@ComprehendMedicalAsyncJobProperties' {} a -> s {kmsKey = a} :: ComprehendMedicalAsyncJobProperties)

-- | The language code of the input documents.
comprehendMedicalAsyncJobProperties_languageCode :: Lens.Lens' ComprehendMedicalAsyncJobProperties (Prelude.Maybe LanguageCode)
comprehendMedicalAsyncJobProperties_languageCode = Lens.lens (\ComprehendMedicalAsyncJobProperties' {languageCode} -> languageCode) (\s@ComprehendMedicalAsyncJobProperties' {} a -> s {languageCode = a} :: ComprehendMedicalAsyncJobProperties)

-- | The path to the file that describes the results of a batch job.
comprehendMedicalAsyncJobProperties_manifestFilePath :: Lens.Lens' ComprehendMedicalAsyncJobProperties (Prelude.Maybe Prelude.Text)
comprehendMedicalAsyncJobProperties_manifestFilePath = Lens.lens (\ComprehendMedicalAsyncJobProperties' {manifestFilePath} -> manifestFilePath) (\s@ComprehendMedicalAsyncJobProperties' {} a -> s {manifestFilePath = a} :: ComprehendMedicalAsyncJobProperties)

-- | A description of the status of a job.
comprehendMedicalAsyncJobProperties_message :: Lens.Lens' ComprehendMedicalAsyncJobProperties (Prelude.Maybe Prelude.Text)
comprehendMedicalAsyncJobProperties_message = Lens.lens (\ComprehendMedicalAsyncJobProperties' {message} -> message) (\s@ComprehendMedicalAsyncJobProperties' {} a -> s {message = a} :: ComprehendMedicalAsyncJobProperties)

-- | The version of the model used to analyze the documents. The version
-- number looks like X.X.X. You can use this information to track the model
-- used for a particular batch of documents.
comprehendMedicalAsyncJobProperties_modelVersion :: Lens.Lens' ComprehendMedicalAsyncJobProperties (Prelude.Maybe Prelude.Text)
comprehendMedicalAsyncJobProperties_modelVersion = Lens.lens (\ComprehendMedicalAsyncJobProperties' {modelVersion} -> modelVersion) (\s@ComprehendMedicalAsyncJobProperties' {} a -> s {modelVersion = a} :: ComprehendMedicalAsyncJobProperties)

-- | The output data configuration that you supplied when you created the
-- detection job.
comprehendMedicalAsyncJobProperties_outputDataConfig :: Lens.Lens' ComprehendMedicalAsyncJobProperties (Prelude.Maybe OutputDataConfig)
comprehendMedicalAsyncJobProperties_outputDataConfig = Lens.lens (\ComprehendMedicalAsyncJobProperties' {outputDataConfig} -> outputDataConfig) (\s@ComprehendMedicalAsyncJobProperties' {} a -> s {outputDataConfig = a} :: ComprehendMedicalAsyncJobProperties)

-- | The time that the detection job was submitted for processing.
comprehendMedicalAsyncJobProperties_submitTime :: Lens.Lens' ComprehendMedicalAsyncJobProperties (Prelude.Maybe Prelude.UTCTime)
comprehendMedicalAsyncJobProperties_submitTime = Lens.lens (\ComprehendMedicalAsyncJobProperties' {submitTime} -> submitTime) (\s@ComprehendMedicalAsyncJobProperties' {} a -> s {submitTime = a} :: ComprehendMedicalAsyncJobProperties) Prelude.. Lens.mapping Data._Time

instance
  Data.FromJSON
    ComprehendMedicalAsyncJobProperties
  where
  parseJSON =
    Data.withObject
      "ComprehendMedicalAsyncJobProperties"
      ( \x ->
          ComprehendMedicalAsyncJobProperties'
            Prelude.<$> (x Data..:? "DataAccessRoleArn")
            Prelude.<*> (x Data..:? "EndTime")
            Prelude.<*> (x Data..:? "ExpirationTime")
            Prelude.<*> (x Data..:? "InputDataConfig")
            Prelude.<*> (x Data..:? "JobId")
            Prelude.<*> (x Data..:? "JobName")
            Prelude.<*> (x Data..:? "JobStatus")
            Prelude.<*> (x Data..:? "KMSKey")
            Prelude.<*> (x Data..:? "LanguageCode")
            Prelude.<*> (x Data..:? "ManifestFilePath")
            Prelude.<*> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "ModelVersion")
            Prelude.<*> (x Data..:? "OutputDataConfig")
            Prelude.<*> (x Data..:? "SubmitTime")
      )

instance
  Prelude.Hashable
    ComprehendMedicalAsyncJobProperties
  where
  hashWithSalt
    _salt
    ComprehendMedicalAsyncJobProperties' {..} =
      _salt `Prelude.hashWithSalt` dataAccessRoleArn
        `Prelude.hashWithSalt` endTime
        `Prelude.hashWithSalt` expirationTime
        `Prelude.hashWithSalt` inputDataConfig
        `Prelude.hashWithSalt` jobId
        `Prelude.hashWithSalt` jobName
        `Prelude.hashWithSalt` jobStatus
        `Prelude.hashWithSalt` kmsKey
        `Prelude.hashWithSalt` languageCode
        `Prelude.hashWithSalt` manifestFilePath
        `Prelude.hashWithSalt` message
        `Prelude.hashWithSalt` modelVersion
        `Prelude.hashWithSalt` outputDataConfig
        `Prelude.hashWithSalt` submitTime

instance
  Prelude.NFData
    ComprehendMedicalAsyncJobProperties
  where
  rnf ComprehendMedicalAsyncJobProperties' {..} =
    Prelude.rnf dataAccessRoleArn
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf expirationTime
      `Prelude.seq` Prelude.rnf inputDataConfig
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf kmsKey
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf manifestFilePath
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf modelVersion
      `Prelude.seq` Prelude.rnf outputDataConfig
      `Prelude.seq` Prelude.rnf submitTime
