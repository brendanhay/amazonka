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
-- Module      : Amazonka.Comprehend.Types.PiiEntitiesDetectionJobProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.PiiEntitiesDetectionJobProperties where

import Amazonka.Comprehend.Types.InputDataConfig
import Amazonka.Comprehend.Types.JobStatus
import Amazonka.Comprehend.Types.LanguageCode
import Amazonka.Comprehend.Types.PiiEntitiesDetectionMode
import Amazonka.Comprehend.Types.PiiOutputDataConfig
import Amazonka.Comprehend.Types.RedactionConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about a PII entities detection job.
--
-- /See:/ 'newPiiEntitiesDetectionJobProperties' smart constructor.
data PiiEntitiesDetectionJobProperties = PiiEntitiesDetectionJobProperties'
  { -- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access
    -- to your input data.
    dataAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The time that the PII entities detection job completed.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The input properties for a PII entities detection job.
    inputDataConfig :: Prelude.Maybe InputDataConfig,
    -- | The Amazon Resource Name (ARN) of the PII entities detection job. It is
    -- a unique, fully qualified identifier for the job. It includes the AWS
    -- account, Region, and the job ID. The format of the ARN is as follows:
    --
    -- @arn:\<partition>:comprehend:\<region>:\<account-id>:pii-entities-detection-job\/\<job-id>@
    --
    -- The following is an example job ARN:
    --
    -- @arn:aws:comprehend:us-west-2:111122223333:pii-entities-detection-job\/1234abcd12ab34cd56ef1234567890ab@
    jobArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier assigned to the PII entities detection job.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The name that you assigned the PII entities detection job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The current status of the PII entities detection job. If the status is
    -- @FAILED@, the @Message@ field shows the reason for the failure.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | The language code of the input documents
    languageCode :: Prelude.Maybe LanguageCode,
    -- | A description of the status of a job.
    message :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the output provides the locations (offsets) of PII
    -- entities or a file in which PII entities are redacted.
    mode :: Prelude.Maybe PiiEntitiesDetectionMode,
    -- | The output data configuration that you supplied when you created the PII
    -- entities detection job.
    outputDataConfig :: Prelude.Maybe PiiOutputDataConfig,
    -- | Provides configuration parameters for PII entity redaction.
    --
    -- This parameter is required if you set the @Mode@ parameter to
    -- @ONLY_REDACTION@. In that case, you must provide a @RedactionConfig@
    -- definition that includes the @PiiEntityTypes@ parameter.
    redactionConfig :: Prelude.Maybe RedactionConfig,
    -- | The time that the PII entities detection job was submitted for
    -- processing.
    submitTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PiiEntitiesDetectionJobProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataAccessRoleArn', 'piiEntitiesDetectionJobProperties_dataAccessRoleArn' - The Amazon Resource Name (ARN) that gives Amazon Comprehend read access
-- to your input data.
--
-- 'endTime', 'piiEntitiesDetectionJobProperties_endTime' - The time that the PII entities detection job completed.
--
-- 'inputDataConfig', 'piiEntitiesDetectionJobProperties_inputDataConfig' - The input properties for a PII entities detection job.
--
-- 'jobArn', 'piiEntitiesDetectionJobProperties_jobArn' - The Amazon Resource Name (ARN) of the PII entities detection job. It is
-- a unique, fully qualified identifier for the job. It includes the AWS
-- account, Region, and the job ID. The format of the ARN is as follows:
--
-- @arn:\<partition>:comprehend:\<region>:\<account-id>:pii-entities-detection-job\/\<job-id>@
--
-- The following is an example job ARN:
--
-- @arn:aws:comprehend:us-west-2:111122223333:pii-entities-detection-job\/1234abcd12ab34cd56ef1234567890ab@
--
-- 'jobId', 'piiEntitiesDetectionJobProperties_jobId' - The identifier assigned to the PII entities detection job.
--
-- 'jobName', 'piiEntitiesDetectionJobProperties_jobName' - The name that you assigned the PII entities detection job.
--
-- 'jobStatus', 'piiEntitiesDetectionJobProperties_jobStatus' - The current status of the PII entities detection job. If the status is
-- @FAILED@, the @Message@ field shows the reason for the failure.
--
-- 'languageCode', 'piiEntitiesDetectionJobProperties_languageCode' - The language code of the input documents
--
-- 'message', 'piiEntitiesDetectionJobProperties_message' - A description of the status of a job.
--
-- 'mode', 'piiEntitiesDetectionJobProperties_mode' - Specifies whether the output provides the locations (offsets) of PII
-- entities or a file in which PII entities are redacted.
--
-- 'outputDataConfig', 'piiEntitiesDetectionJobProperties_outputDataConfig' - The output data configuration that you supplied when you created the PII
-- entities detection job.
--
-- 'redactionConfig', 'piiEntitiesDetectionJobProperties_redactionConfig' - Provides configuration parameters for PII entity redaction.
--
-- This parameter is required if you set the @Mode@ parameter to
-- @ONLY_REDACTION@. In that case, you must provide a @RedactionConfig@
-- definition that includes the @PiiEntityTypes@ parameter.
--
-- 'submitTime', 'piiEntitiesDetectionJobProperties_submitTime' - The time that the PII entities detection job was submitted for
-- processing.
newPiiEntitiesDetectionJobProperties ::
  PiiEntitiesDetectionJobProperties
newPiiEntitiesDetectionJobProperties =
  PiiEntitiesDetectionJobProperties'
    { dataAccessRoleArn =
        Prelude.Nothing,
      endTime = Prelude.Nothing,
      inputDataConfig = Prelude.Nothing,
      jobArn = Prelude.Nothing,
      jobId = Prelude.Nothing,
      jobName = Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      languageCode = Prelude.Nothing,
      message = Prelude.Nothing,
      mode = Prelude.Nothing,
      outputDataConfig = Prelude.Nothing,
      redactionConfig = Prelude.Nothing,
      submitTime = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access
-- to your input data.
piiEntitiesDetectionJobProperties_dataAccessRoleArn :: Lens.Lens' PiiEntitiesDetectionJobProperties (Prelude.Maybe Prelude.Text)
piiEntitiesDetectionJobProperties_dataAccessRoleArn = Lens.lens (\PiiEntitiesDetectionJobProperties' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {dataAccessRoleArn = a} :: PiiEntitiesDetectionJobProperties)

-- | The time that the PII entities detection job completed.
piiEntitiesDetectionJobProperties_endTime :: Lens.Lens' PiiEntitiesDetectionJobProperties (Prelude.Maybe Prelude.UTCTime)
piiEntitiesDetectionJobProperties_endTime = Lens.lens (\PiiEntitiesDetectionJobProperties' {endTime} -> endTime) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {endTime = a} :: PiiEntitiesDetectionJobProperties) Prelude.. Lens.mapping Data._Time

-- | The input properties for a PII entities detection job.
piiEntitiesDetectionJobProperties_inputDataConfig :: Lens.Lens' PiiEntitiesDetectionJobProperties (Prelude.Maybe InputDataConfig)
piiEntitiesDetectionJobProperties_inputDataConfig = Lens.lens (\PiiEntitiesDetectionJobProperties' {inputDataConfig} -> inputDataConfig) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {inputDataConfig = a} :: PiiEntitiesDetectionJobProperties)

-- | The Amazon Resource Name (ARN) of the PII entities detection job. It is
-- a unique, fully qualified identifier for the job. It includes the AWS
-- account, Region, and the job ID. The format of the ARN is as follows:
--
-- @arn:\<partition>:comprehend:\<region>:\<account-id>:pii-entities-detection-job\/\<job-id>@
--
-- The following is an example job ARN:
--
-- @arn:aws:comprehend:us-west-2:111122223333:pii-entities-detection-job\/1234abcd12ab34cd56ef1234567890ab@
piiEntitiesDetectionJobProperties_jobArn :: Lens.Lens' PiiEntitiesDetectionJobProperties (Prelude.Maybe Prelude.Text)
piiEntitiesDetectionJobProperties_jobArn = Lens.lens (\PiiEntitiesDetectionJobProperties' {jobArn} -> jobArn) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {jobArn = a} :: PiiEntitiesDetectionJobProperties)

-- | The identifier assigned to the PII entities detection job.
piiEntitiesDetectionJobProperties_jobId :: Lens.Lens' PiiEntitiesDetectionJobProperties (Prelude.Maybe Prelude.Text)
piiEntitiesDetectionJobProperties_jobId = Lens.lens (\PiiEntitiesDetectionJobProperties' {jobId} -> jobId) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {jobId = a} :: PiiEntitiesDetectionJobProperties)

-- | The name that you assigned the PII entities detection job.
piiEntitiesDetectionJobProperties_jobName :: Lens.Lens' PiiEntitiesDetectionJobProperties (Prelude.Maybe Prelude.Text)
piiEntitiesDetectionJobProperties_jobName = Lens.lens (\PiiEntitiesDetectionJobProperties' {jobName} -> jobName) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {jobName = a} :: PiiEntitiesDetectionJobProperties)

-- | The current status of the PII entities detection job. If the status is
-- @FAILED@, the @Message@ field shows the reason for the failure.
piiEntitiesDetectionJobProperties_jobStatus :: Lens.Lens' PiiEntitiesDetectionJobProperties (Prelude.Maybe JobStatus)
piiEntitiesDetectionJobProperties_jobStatus = Lens.lens (\PiiEntitiesDetectionJobProperties' {jobStatus} -> jobStatus) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {jobStatus = a} :: PiiEntitiesDetectionJobProperties)

-- | The language code of the input documents
piiEntitiesDetectionJobProperties_languageCode :: Lens.Lens' PiiEntitiesDetectionJobProperties (Prelude.Maybe LanguageCode)
piiEntitiesDetectionJobProperties_languageCode = Lens.lens (\PiiEntitiesDetectionJobProperties' {languageCode} -> languageCode) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {languageCode = a} :: PiiEntitiesDetectionJobProperties)

-- | A description of the status of a job.
piiEntitiesDetectionJobProperties_message :: Lens.Lens' PiiEntitiesDetectionJobProperties (Prelude.Maybe Prelude.Text)
piiEntitiesDetectionJobProperties_message = Lens.lens (\PiiEntitiesDetectionJobProperties' {message} -> message) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {message = a} :: PiiEntitiesDetectionJobProperties)

-- | Specifies whether the output provides the locations (offsets) of PII
-- entities or a file in which PII entities are redacted.
piiEntitiesDetectionJobProperties_mode :: Lens.Lens' PiiEntitiesDetectionJobProperties (Prelude.Maybe PiiEntitiesDetectionMode)
piiEntitiesDetectionJobProperties_mode = Lens.lens (\PiiEntitiesDetectionJobProperties' {mode} -> mode) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {mode = a} :: PiiEntitiesDetectionJobProperties)

-- | The output data configuration that you supplied when you created the PII
-- entities detection job.
piiEntitiesDetectionJobProperties_outputDataConfig :: Lens.Lens' PiiEntitiesDetectionJobProperties (Prelude.Maybe PiiOutputDataConfig)
piiEntitiesDetectionJobProperties_outputDataConfig = Lens.lens (\PiiEntitiesDetectionJobProperties' {outputDataConfig} -> outputDataConfig) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {outputDataConfig = a} :: PiiEntitiesDetectionJobProperties)

-- | Provides configuration parameters for PII entity redaction.
--
-- This parameter is required if you set the @Mode@ parameter to
-- @ONLY_REDACTION@. In that case, you must provide a @RedactionConfig@
-- definition that includes the @PiiEntityTypes@ parameter.
piiEntitiesDetectionJobProperties_redactionConfig :: Lens.Lens' PiiEntitiesDetectionJobProperties (Prelude.Maybe RedactionConfig)
piiEntitiesDetectionJobProperties_redactionConfig = Lens.lens (\PiiEntitiesDetectionJobProperties' {redactionConfig} -> redactionConfig) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {redactionConfig = a} :: PiiEntitiesDetectionJobProperties)

-- | The time that the PII entities detection job was submitted for
-- processing.
piiEntitiesDetectionJobProperties_submitTime :: Lens.Lens' PiiEntitiesDetectionJobProperties (Prelude.Maybe Prelude.UTCTime)
piiEntitiesDetectionJobProperties_submitTime = Lens.lens (\PiiEntitiesDetectionJobProperties' {submitTime} -> submitTime) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {submitTime = a} :: PiiEntitiesDetectionJobProperties) Prelude.. Lens.mapping Data._Time

instance
  Data.FromJSON
    PiiEntitiesDetectionJobProperties
  where
  parseJSON =
    Data.withObject
      "PiiEntitiesDetectionJobProperties"
      ( \x ->
          PiiEntitiesDetectionJobProperties'
            Prelude.<$> (x Data..:? "DataAccessRoleArn")
            Prelude.<*> (x Data..:? "EndTime")
            Prelude.<*> (x Data..:? "InputDataConfig")
            Prelude.<*> (x Data..:? "JobArn")
            Prelude.<*> (x Data..:? "JobId")
            Prelude.<*> (x Data..:? "JobName")
            Prelude.<*> (x Data..:? "JobStatus")
            Prelude.<*> (x Data..:? "LanguageCode")
            Prelude.<*> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "Mode")
            Prelude.<*> (x Data..:? "OutputDataConfig")
            Prelude.<*> (x Data..:? "RedactionConfig")
            Prelude.<*> (x Data..:? "SubmitTime")
      )

instance
  Prelude.Hashable
    PiiEntitiesDetectionJobProperties
  where
  hashWithSalt
    _salt
    PiiEntitiesDetectionJobProperties' {..} =
      _salt
        `Prelude.hashWithSalt` dataAccessRoleArn
        `Prelude.hashWithSalt` endTime
        `Prelude.hashWithSalt` inputDataConfig
        `Prelude.hashWithSalt` jobArn
        `Prelude.hashWithSalt` jobId
        `Prelude.hashWithSalt` jobName
        `Prelude.hashWithSalt` jobStatus
        `Prelude.hashWithSalt` languageCode
        `Prelude.hashWithSalt` message
        `Prelude.hashWithSalt` mode
        `Prelude.hashWithSalt` outputDataConfig
        `Prelude.hashWithSalt` redactionConfig
        `Prelude.hashWithSalt` submitTime

instance
  Prelude.NFData
    PiiEntitiesDetectionJobProperties
  where
  rnf PiiEntitiesDetectionJobProperties' {..} =
    Prelude.rnf dataAccessRoleArn `Prelude.seq`
      Prelude.rnf endTime `Prelude.seq`
        Prelude.rnf inputDataConfig `Prelude.seq`
          Prelude.rnf jobArn `Prelude.seq`
            Prelude.rnf jobId `Prelude.seq`
              Prelude.rnf jobName `Prelude.seq`
                Prelude.rnf jobStatus `Prelude.seq`
                  Prelude.rnf languageCode `Prelude.seq`
                    Prelude.rnf message `Prelude.seq`
                      Prelude.rnf mode `Prelude.seq`
                        Prelude.rnf outputDataConfig `Prelude.seq`
                          Prelude.rnf redactionConfig `Prelude.seq`
                            Prelude.rnf submitTime
