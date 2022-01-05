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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about a PII entities detection job.
--
-- /See:/ 'newPiiEntitiesDetectionJobProperties' smart constructor.
data PiiEntitiesDetectionJobProperties = PiiEntitiesDetectionJobProperties'
  { -- | The language code of the input documents
    languageCode :: Prelude.Maybe LanguageCode,
    -- | The identifier assigned to the PII entities detection job.
    jobId :: Prelude.Maybe Prelude.Text,
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
    -- | The name that you assigned the PII entities detection job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the output provides the locations (offsets) of PII
    -- entities or a file in which PII entities are redacted.
    mode :: Prelude.Maybe PiiEntitiesDetectionMode,
    -- | The input properties for a PII entities detection job.
    inputDataConfig :: Prelude.Maybe InputDataConfig,
    -- | Provides configuration parameters for PII entity redaction.
    --
    -- This parameter is required if you set the @Mode@ parameter to
    -- @ONLY_REDACTION@. In that case, you must provide a @RedactionConfig@
    -- definition that includes the @PiiEntityTypes@ parameter.
    redactionConfig :: Prelude.Maybe RedactionConfig,
    -- | The time that the PII entities detection job completed.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | The output data configuration that you supplied when you created the PII
    -- entities detection job.
    outputDataConfig :: Prelude.Maybe PiiOutputDataConfig,
    -- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access
    -- to your input data.
    dataAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The current status of the PII entities detection job. If the status is
    -- @FAILED@, the @Message@ field shows the reason for the failure.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | A description of the status of a job.
    message :: Prelude.Maybe Prelude.Text,
    -- | The time that the PII entities detection job was submitted for
    -- processing.
    submitTime :: Prelude.Maybe Core.POSIX
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
-- 'languageCode', 'piiEntitiesDetectionJobProperties_languageCode' - The language code of the input documents
--
-- 'jobId', 'piiEntitiesDetectionJobProperties_jobId' - The identifier assigned to the PII entities detection job.
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
-- 'jobName', 'piiEntitiesDetectionJobProperties_jobName' - The name that you assigned the PII entities detection job.
--
-- 'mode', 'piiEntitiesDetectionJobProperties_mode' - Specifies whether the output provides the locations (offsets) of PII
-- entities or a file in which PII entities are redacted.
--
-- 'inputDataConfig', 'piiEntitiesDetectionJobProperties_inputDataConfig' - The input properties for a PII entities detection job.
--
-- 'redactionConfig', 'piiEntitiesDetectionJobProperties_redactionConfig' - Provides configuration parameters for PII entity redaction.
--
-- This parameter is required if you set the @Mode@ parameter to
-- @ONLY_REDACTION@. In that case, you must provide a @RedactionConfig@
-- definition that includes the @PiiEntityTypes@ parameter.
--
-- 'endTime', 'piiEntitiesDetectionJobProperties_endTime' - The time that the PII entities detection job completed.
--
-- 'outputDataConfig', 'piiEntitiesDetectionJobProperties_outputDataConfig' - The output data configuration that you supplied when you created the PII
-- entities detection job.
--
-- 'dataAccessRoleArn', 'piiEntitiesDetectionJobProperties_dataAccessRoleArn' - The Amazon Resource Name (ARN) that gives Amazon Comprehend read access
-- to your input data.
--
-- 'jobStatus', 'piiEntitiesDetectionJobProperties_jobStatus' - The current status of the PII entities detection job. If the status is
-- @FAILED@, the @Message@ field shows the reason for the failure.
--
-- 'message', 'piiEntitiesDetectionJobProperties_message' - A description of the status of a job.
--
-- 'submitTime', 'piiEntitiesDetectionJobProperties_submitTime' - The time that the PII entities detection job was submitted for
-- processing.
newPiiEntitiesDetectionJobProperties ::
  PiiEntitiesDetectionJobProperties
newPiiEntitiesDetectionJobProperties =
  PiiEntitiesDetectionJobProperties'
    { languageCode =
        Prelude.Nothing,
      jobId = Prelude.Nothing,
      jobArn = Prelude.Nothing,
      jobName = Prelude.Nothing,
      mode = Prelude.Nothing,
      inputDataConfig = Prelude.Nothing,
      redactionConfig = Prelude.Nothing,
      endTime = Prelude.Nothing,
      outputDataConfig = Prelude.Nothing,
      dataAccessRoleArn = Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      message = Prelude.Nothing,
      submitTime = Prelude.Nothing
    }

-- | The language code of the input documents
piiEntitiesDetectionJobProperties_languageCode :: Lens.Lens' PiiEntitiesDetectionJobProperties (Prelude.Maybe LanguageCode)
piiEntitiesDetectionJobProperties_languageCode = Lens.lens (\PiiEntitiesDetectionJobProperties' {languageCode} -> languageCode) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {languageCode = a} :: PiiEntitiesDetectionJobProperties)

-- | The identifier assigned to the PII entities detection job.
piiEntitiesDetectionJobProperties_jobId :: Lens.Lens' PiiEntitiesDetectionJobProperties (Prelude.Maybe Prelude.Text)
piiEntitiesDetectionJobProperties_jobId = Lens.lens (\PiiEntitiesDetectionJobProperties' {jobId} -> jobId) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {jobId = a} :: PiiEntitiesDetectionJobProperties)

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

-- | The name that you assigned the PII entities detection job.
piiEntitiesDetectionJobProperties_jobName :: Lens.Lens' PiiEntitiesDetectionJobProperties (Prelude.Maybe Prelude.Text)
piiEntitiesDetectionJobProperties_jobName = Lens.lens (\PiiEntitiesDetectionJobProperties' {jobName} -> jobName) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {jobName = a} :: PiiEntitiesDetectionJobProperties)

-- | Specifies whether the output provides the locations (offsets) of PII
-- entities or a file in which PII entities are redacted.
piiEntitiesDetectionJobProperties_mode :: Lens.Lens' PiiEntitiesDetectionJobProperties (Prelude.Maybe PiiEntitiesDetectionMode)
piiEntitiesDetectionJobProperties_mode = Lens.lens (\PiiEntitiesDetectionJobProperties' {mode} -> mode) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {mode = a} :: PiiEntitiesDetectionJobProperties)

-- | The input properties for a PII entities detection job.
piiEntitiesDetectionJobProperties_inputDataConfig :: Lens.Lens' PiiEntitiesDetectionJobProperties (Prelude.Maybe InputDataConfig)
piiEntitiesDetectionJobProperties_inputDataConfig = Lens.lens (\PiiEntitiesDetectionJobProperties' {inputDataConfig} -> inputDataConfig) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {inputDataConfig = a} :: PiiEntitiesDetectionJobProperties)

-- | Provides configuration parameters for PII entity redaction.
--
-- This parameter is required if you set the @Mode@ parameter to
-- @ONLY_REDACTION@. In that case, you must provide a @RedactionConfig@
-- definition that includes the @PiiEntityTypes@ parameter.
piiEntitiesDetectionJobProperties_redactionConfig :: Lens.Lens' PiiEntitiesDetectionJobProperties (Prelude.Maybe RedactionConfig)
piiEntitiesDetectionJobProperties_redactionConfig = Lens.lens (\PiiEntitiesDetectionJobProperties' {redactionConfig} -> redactionConfig) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {redactionConfig = a} :: PiiEntitiesDetectionJobProperties)

-- | The time that the PII entities detection job completed.
piiEntitiesDetectionJobProperties_endTime :: Lens.Lens' PiiEntitiesDetectionJobProperties (Prelude.Maybe Prelude.UTCTime)
piiEntitiesDetectionJobProperties_endTime = Lens.lens (\PiiEntitiesDetectionJobProperties' {endTime} -> endTime) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {endTime = a} :: PiiEntitiesDetectionJobProperties) Prelude.. Lens.mapping Core._Time

-- | The output data configuration that you supplied when you created the PII
-- entities detection job.
piiEntitiesDetectionJobProperties_outputDataConfig :: Lens.Lens' PiiEntitiesDetectionJobProperties (Prelude.Maybe PiiOutputDataConfig)
piiEntitiesDetectionJobProperties_outputDataConfig = Lens.lens (\PiiEntitiesDetectionJobProperties' {outputDataConfig} -> outputDataConfig) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {outputDataConfig = a} :: PiiEntitiesDetectionJobProperties)

-- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access
-- to your input data.
piiEntitiesDetectionJobProperties_dataAccessRoleArn :: Lens.Lens' PiiEntitiesDetectionJobProperties (Prelude.Maybe Prelude.Text)
piiEntitiesDetectionJobProperties_dataAccessRoleArn = Lens.lens (\PiiEntitiesDetectionJobProperties' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {dataAccessRoleArn = a} :: PiiEntitiesDetectionJobProperties)

-- | The current status of the PII entities detection job. If the status is
-- @FAILED@, the @Message@ field shows the reason for the failure.
piiEntitiesDetectionJobProperties_jobStatus :: Lens.Lens' PiiEntitiesDetectionJobProperties (Prelude.Maybe JobStatus)
piiEntitiesDetectionJobProperties_jobStatus = Lens.lens (\PiiEntitiesDetectionJobProperties' {jobStatus} -> jobStatus) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {jobStatus = a} :: PiiEntitiesDetectionJobProperties)

-- | A description of the status of a job.
piiEntitiesDetectionJobProperties_message :: Lens.Lens' PiiEntitiesDetectionJobProperties (Prelude.Maybe Prelude.Text)
piiEntitiesDetectionJobProperties_message = Lens.lens (\PiiEntitiesDetectionJobProperties' {message} -> message) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {message = a} :: PiiEntitiesDetectionJobProperties)

-- | The time that the PII entities detection job was submitted for
-- processing.
piiEntitiesDetectionJobProperties_submitTime :: Lens.Lens' PiiEntitiesDetectionJobProperties (Prelude.Maybe Prelude.UTCTime)
piiEntitiesDetectionJobProperties_submitTime = Lens.lens (\PiiEntitiesDetectionJobProperties' {submitTime} -> submitTime) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {submitTime = a} :: PiiEntitiesDetectionJobProperties) Prelude.. Lens.mapping Core._Time

instance
  Core.FromJSON
    PiiEntitiesDetectionJobProperties
  where
  parseJSON =
    Core.withObject
      "PiiEntitiesDetectionJobProperties"
      ( \x ->
          PiiEntitiesDetectionJobProperties'
            Prelude.<$> (x Core..:? "LanguageCode")
            Prelude.<*> (x Core..:? "JobId")
            Prelude.<*> (x Core..:? "JobArn")
            Prelude.<*> (x Core..:? "JobName")
            Prelude.<*> (x Core..:? "Mode")
            Prelude.<*> (x Core..:? "InputDataConfig")
            Prelude.<*> (x Core..:? "RedactionConfig")
            Prelude.<*> (x Core..:? "EndTime")
            Prelude.<*> (x Core..:? "OutputDataConfig")
            Prelude.<*> (x Core..:? "DataAccessRoleArn")
            Prelude.<*> (x Core..:? "JobStatus")
            Prelude.<*> (x Core..:? "Message")
            Prelude.<*> (x Core..:? "SubmitTime")
      )

instance
  Prelude.Hashable
    PiiEntitiesDetectionJobProperties
  where
  hashWithSalt
    _salt
    PiiEntitiesDetectionJobProperties' {..} =
      _salt `Prelude.hashWithSalt` languageCode
        `Prelude.hashWithSalt` jobId
        `Prelude.hashWithSalt` jobArn
        `Prelude.hashWithSalt` jobName
        `Prelude.hashWithSalt` mode
        `Prelude.hashWithSalt` inputDataConfig
        `Prelude.hashWithSalt` redactionConfig
        `Prelude.hashWithSalt` endTime
        `Prelude.hashWithSalt` outputDataConfig
        `Prelude.hashWithSalt` dataAccessRoleArn
        `Prelude.hashWithSalt` jobStatus
        `Prelude.hashWithSalt` message
        `Prelude.hashWithSalt` submitTime

instance
  Prelude.NFData
    PiiEntitiesDetectionJobProperties
  where
  rnf PiiEntitiesDetectionJobProperties' {..} =
    Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf jobArn
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf mode
      `Prelude.seq` Prelude.rnf inputDataConfig
      `Prelude.seq` Prelude.rnf redactionConfig
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf outputDataConfig
      `Prelude.seq` Prelude.rnf dataAccessRoleArn
      `Prelude.seq` Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf submitTime
