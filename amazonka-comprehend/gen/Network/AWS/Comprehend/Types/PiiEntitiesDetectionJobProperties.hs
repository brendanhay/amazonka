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
-- Module      : Network.AWS.Comprehend.Types.PiiEntitiesDetectionJobProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.PiiEntitiesDetectionJobProperties where

import Network.AWS.Comprehend.Types.InputDataConfig
import Network.AWS.Comprehend.Types.JobStatus
import Network.AWS.Comprehend.Types.LanguageCode
import Network.AWS.Comprehend.Types.PiiEntitiesDetectionMode
import Network.AWS.Comprehend.Types.PiiOutputDataConfig
import Network.AWS.Comprehend.Types.RedactionConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about a PII entities detection job.
--
-- /See:/ 'newPiiEntitiesDetectionJobProperties' smart constructor.
data PiiEntitiesDetectionJobProperties = PiiEntitiesDetectionJobProperties'
  { -- | Provides configuration parameters for PII entity redaction.
    --
    -- This parameter is required if you set the @Mode@ parameter to
    -- @ONLY_REDACTION@. In that case, you must provide a @RedactionConfig@
    -- definition that includes the @PiiEntityTypes@ parameter.
    redactionConfig :: Prelude.Maybe RedactionConfig,
    -- | The language code of the input documents
    languageCode :: Prelude.Maybe LanguageCode,
    -- | The input properties for a PII entities detection job.
    inputDataConfig :: Prelude.Maybe InputDataConfig,
    -- | Specifies whether the output provides the locations (offsets) of PII
    -- entities or a file in which PII entities are redacted.
    mode :: Prelude.Maybe PiiEntitiesDetectionMode,
    -- | A description of the status of a job.
    message :: Prelude.Maybe Prelude.Text,
    -- | The current status of the PII entities detection job. If the status is
    -- @FAILED@, the @Message@ field shows the reason for the failure.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | The output data configuration that you supplied when you created the PII
    -- entities detection job.
    outputDataConfig :: Prelude.Maybe PiiOutputDataConfig,
    -- | The time that the PII entities detection job completed.
    endTime :: Prelude.Maybe Prelude.POSIX,
    -- | The time that the PII entities detection job was submitted for
    -- processing.
    submitTime :: Prelude.Maybe Prelude.POSIX,
    -- | The name that you assigned the PII entities detection job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access
    -- to your input data.
    dataAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier assigned to the PII entities detection job.
    jobId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PiiEntitiesDetectionJobProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'redactionConfig', 'piiEntitiesDetectionJobProperties_redactionConfig' - Provides configuration parameters for PII entity redaction.
--
-- This parameter is required if you set the @Mode@ parameter to
-- @ONLY_REDACTION@. In that case, you must provide a @RedactionConfig@
-- definition that includes the @PiiEntityTypes@ parameter.
--
-- 'languageCode', 'piiEntitiesDetectionJobProperties_languageCode' - The language code of the input documents
--
-- 'inputDataConfig', 'piiEntitiesDetectionJobProperties_inputDataConfig' - The input properties for a PII entities detection job.
--
-- 'mode', 'piiEntitiesDetectionJobProperties_mode' - Specifies whether the output provides the locations (offsets) of PII
-- entities or a file in which PII entities are redacted.
--
-- 'message', 'piiEntitiesDetectionJobProperties_message' - A description of the status of a job.
--
-- 'jobStatus', 'piiEntitiesDetectionJobProperties_jobStatus' - The current status of the PII entities detection job. If the status is
-- @FAILED@, the @Message@ field shows the reason for the failure.
--
-- 'outputDataConfig', 'piiEntitiesDetectionJobProperties_outputDataConfig' - The output data configuration that you supplied when you created the PII
-- entities detection job.
--
-- 'endTime', 'piiEntitiesDetectionJobProperties_endTime' - The time that the PII entities detection job completed.
--
-- 'submitTime', 'piiEntitiesDetectionJobProperties_submitTime' - The time that the PII entities detection job was submitted for
-- processing.
--
-- 'jobName', 'piiEntitiesDetectionJobProperties_jobName' - The name that you assigned the PII entities detection job.
--
-- 'dataAccessRoleArn', 'piiEntitiesDetectionJobProperties_dataAccessRoleArn' - The Amazon Resource Name (ARN) that gives Amazon Comprehend read access
-- to your input data.
--
-- 'jobId', 'piiEntitiesDetectionJobProperties_jobId' - The identifier assigned to the PII entities detection job.
newPiiEntitiesDetectionJobProperties ::
  PiiEntitiesDetectionJobProperties
newPiiEntitiesDetectionJobProperties =
  PiiEntitiesDetectionJobProperties'
    { redactionConfig =
        Prelude.Nothing,
      languageCode = Prelude.Nothing,
      inputDataConfig = Prelude.Nothing,
      mode = Prelude.Nothing,
      message = Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      outputDataConfig = Prelude.Nothing,
      endTime = Prelude.Nothing,
      submitTime = Prelude.Nothing,
      jobName = Prelude.Nothing,
      dataAccessRoleArn = Prelude.Nothing,
      jobId = Prelude.Nothing
    }

-- | Provides configuration parameters for PII entity redaction.
--
-- This parameter is required if you set the @Mode@ parameter to
-- @ONLY_REDACTION@. In that case, you must provide a @RedactionConfig@
-- definition that includes the @PiiEntityTypes@ parameter.
piiEntitiesDetectionJobProperties_redactionConfig :: Lens.Lens' PiiEntitiesDetectionJobProperties (Prelude.Maybe RedactionConfig)
piiEntitiesDetectionJobProperties_redactionConfig = Lens.lens (\PiiEntitiesDetectionJobProperties' {redactionConfig} -> redactionConfig) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {redactionConfig = a} :: PiiEntitiesDetectionJobProperties)

-- | The language code of the input documents
piiEntitiesDetectionJobProperties_languageCode :: Lens.Lens' PiiEntitiesDetectionJobProperties (Prelude.Maybe LanguageCode)
piiEntitiesDetectionJobProperties_languageCode = Lens.lens (\PiiEntitiesDetectionJobProperties' {languageCode} -> languageCode) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {languageCode = a} :: PiiEntitiesDetectionJobProperties)

-- | The input properties for a PII entities detection job.
piiEntitiesDetectionJobProperties_inputDataConfig :: Lens.Lens' PiiEntitiesDetectionJobProperties (Prelude.Maybe InputDataConfig)
piiEntitiesDetectionJobProperties_inputDataConfig = Lens.lens (\PiiEntitiesDetectionJobProperties' {inputDataConfig} -> inputDataConfig) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {inputDataConfig = a} :: PiiEntitiesDetectionJobProperties)

-- | Specifies whether the output provides the locations (offsets) of PII
-- entities or a file in which PII entities are redacted.
piiEntitiesDetectionJobProperties_mode :: Lens.Lens' PiiEntitiesDetectionJobProperties (Prelude.Maybe PiiEntitiesDetectionMode)
piiEntitiesDetectionJobProperties_mode = Lens.lens (\PiiEntitiesDetectionJobProperties' {mode} -> mode) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {mode = a} :: PiiEntitiesDetectionJobProperties)

-- | A description of the status of a job.
piiEntitiesDetectionJobProperties_message :: Lens.Lens' PiiEntitiesDetectionJobProperties (Prelude.Maybe Prelude.Text)
piiEntitiesDetectionJobProperties_message = Lens.lens (\PiiEntitiesDetectionJobProperties' {message} -> message) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {message = a} :: PiiEntitiesDetectionJobProperties)

-- | The current status of the PII entities detection job. If the status is
-- @FAILED@, the @Message@ field shows the reason for the failure.
piiEntitiesDetectionJobProperties_jobStatus :: Lens.Lens' PiiEntitiesDetectionJobProperties (Prelude.Maybe JobStatus)
piiEntitiesDetectionJobProperties_jobStatus = Lens.lens (\PiiEntitiesDetectionJobProperties' {jobStatus} -> jobStatus) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {jobStatus = a} :: PiiEntitiesDetectionJobProperties)

-- | The output data configuration that you supplied when you created the PII
-- entities detection job.
piiEntitiesDetectionJobProperties_outputDataConfig :: Lens.Lens' PiiEntitiesDetectionJobProperties (Prelude.Maybe PiiOutputDataConfig)
piiEntitiesDetectionJobProperties_outputDataConfig = Lens.lens (\PiiEntitiesDetectionJobProperties' {outputDataConfig} -> outputDataConfig) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {outputDataConfig = a} :: PiiEntitiesDetectionJobProperties)

-- | The time that the PII entities detection job completed.
piiEntitiesDetectionJobProperties_endTime :: Lens.Lens' PiiEntitiesDetectionJobProperties (Prelude.Maybe Prelude.UTCTime)
piiEntitiesDetectionJobProperties_endTime = Lens.lens (\PiiEntitiesDetectionJobProperties' {endTime} -> endTime) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {endTime = a} :: PiiEntitiesDetectionJobProperties) Prelude.. Lens.mapping Prelude._Time

-- | The time that the PII entities detection job was submitted for
-- processing.
piiEntitiesDetectionJobProperties_submitTime :: Lens.Lens' PiiEntitiesDetectionJobProperties (Prelude.Maybe Prelude.UTCTime)
piiEntitiesDetectionJobProperties_submitTime = Lens.lens (\PiiEntitiesDetectionJobProperties' {submitTime} -> submitTime) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {submitTime = a} :: PiiEntitiesDetectionJobProperties) Prelude.. Lens.mapping Prelude._Time

-- | The name that you assigned the PII entities detection job.
piiEntitiesDetectionJobProperties_jobName :: Lens.Lens' PiiEntitiesDetectionJobProperties (Prelude.Maybe Prelude.Text)
piiEntitiesDetectionJobProperties_jobName = Lens.lens (\PiiEntitiesDetectionJobProperties' {jobName} -> jobName) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {jobName = a} :: PiiEntitiesDetectionJobProperties)

-- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access
-- to your input data.
piiEntitiesDetectionJobProperties_dataAccessRoleArn :: Lens.Lens' PiiEntitiesDetectionJobProperties (Prelude.Maybe Prelude.Text)
piiEntitiesDetectionJobProperties_dataAccessRoleArn = Lens.lens (\PiiEntitiesDetectionJobProperties' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {dataAccessRoleArn = a} :: PiiEntitiesDetectionJobProperties)

-- | The identifier assigned to the PII entities detection job.
piiEntitiesDetectionJobProperties_jobId :: Lens.Lens' PiiEntitiesDetectionJobProperties (Prelude.Maybe Prelude.Text)
piiEntitiesDetectionJobProperties_jobId = Lens.lens (\PiiEntitiesDetectionJobProperties' {jobId} -> jobId) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {jobId = a} :: PiiEntitiesDetectionJobProperties)

instance
  Prelude.FromJSON
    PiiEntitiesDetectionJobProperties
  where
  parseJSON =
    Prelude.withObject
      "PiiEntitiesDetectionJobProperties"
      ( \x ->
          PiiEntitiesDetectionJobProperties'
            Prelude.<$> (x Prelude..:? "RedactionConfig")
            Prelude.<*> (x Prelude..:? "LanguageCode")
            Prelude.<*> (x Prelude..:? "InputDataConfig")
            Prelude.<*> (x Prelude..:? "Mode")
            Prelude.<*> (x Prelude..:? "Message")
            Prelude.<*> (x Prelude..:? "JobStatus")
            Prelude.<*> (x Prelude..:? "OutputDataConfig")
            Prelude.<*> (x Prelude..:? "EndTime")
            Prelude.<*> (x Prelude..:? "SubmitTime")
            Prelude.<*> (x Prelude..:? "JobName")
            Prelude.<*> (x Prelude..:? "DataAccessRoleArn")
            Prelude.<*> (x Prelude..:? "JobId")
      )

instance
  Prelude.Hashable
    PiiEntitiesDetectionJobProperties

instance
  Prelude.NFData
    PiiEntitiesDetectionJobProperties
