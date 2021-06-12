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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides information about a PII entities detection job.
--
-- /See:/ 'newPiiEntitiesDetectionJobProperties' smart constructor.
data PiiEntitiesDetectionJobProperties = PiiEntitiesDetectionJobProperties'
  { -- | Provides configuration parameters for PII entity redaction.
    --
    -- This parameter is required if you set the @Mode@ parameter to
    -- @ONLY_REDACTION@. In that case, you must provide a @RedactionConfig@
    -- definition that includes the @PiiEntityTypes@ parameter.
    redactionConfig :: Core.Maybe RedactionConfig,
    -- | The language code of the input documents
    languageCode :: Core.Maybe LanguageCode,
    -- | The input properties for a PII entities detection job.
    inputDataConfig :: Core.Maybe InputDataConfig,
    -- | Specifies whether the output provides the locations (offsets) of PII
    -- entities or a file in which PII entities are redacted.
    mode :: Core.Maybe PiiEntitiesDetectionMode,
    -- | A description of the status of a job.
    message :: Core.Maybe Core.Text,
    -- | The current status of the PII entities detection job. If the status is
    -- @FAILED@, the @Message@ field shows the reason for the failure.
    jobStatus :: Core.Maybe JobStatus,
    -- | The output data configuration that you supplied when you created the PII
    -- entities detection job.
    outputDataConfig :: Core.Maybe PiiOutputDataConfig,
    -- | The time that the PII entities detection job completed.
    endTime :: Core.Maybe Core.POSIX,
    -- | The time that the PII entities detection job was submitted for
    -- processing.
    submitTime :: Core.Maybe Core.POSIX,
    -- | The name that you assigned the PII entities detection job.
    jobName :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access
    -- to your input data.
    dataAccessRoleArn :: Core.Maybe Core.Text,
    -- | The identifier assigned to the PII entities detection job.
    jobId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      languageCode = Core.Nothing,
      inputDataConfig = Core.Nothing,
      mode = Core.Nothing,
      message = Core.Nothing,
      jobStatus = Core.Nothing,
      outputDataConfig = Core.Nothing,
      endTime = Core.Nothing,
      submitTime = Core.Nothing,
      jobName = Core.Nothing,
      dataAccessRoleArn = Core.Nothing,
      jobId = Core.Nothing
    }

-- | Provides configuration parameters for PII entity redaction.
--
-- This parameter is required if you set the @Mode@ parameter to
-- @ONLY_REDACTION@. In that case, you must provide a @RedactionConfig@
-- definition that includes the @PiiEntityTypes@ parameter.
piiEntitiesDetectionJobProperties_redactionConfig :: Lens.Lens' PiiEntitiesDetectionJobProperties (Core.Maybe RedactionConfig)
piiEntitiesDetectionJobProperties_redactionConfig = Lens.lens (\PiiEntitiesDetectionJobProperties' {redactionConfig} -> redactionConfig) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {redactionConfig = a} :: PiiEntitiesDetectionJobProperties)

-- | The language code of the input documents
piiEntitiesDetectionJobProperties_languageCode :: Lens.Lens' PiiEntitiesDetectionJobProperties (Core.Maybe LanguageCode)
piiEntitiesDetectionJobProperties_languageCode = Lens.lens (\PiiEntitiesDetectionJobProperties' {languageCode} -> languageCode) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {languageCode = a} :: PiiEntitiesDetectionJobProperties)

-- | The input properties for a PII entities detection job.
piiEntitiesDetectionJobProperties_inputDataConfig :: Lens.Lens' PiiEntitiesDetectionJobProperties (Core.Maybe InputDataConfig)
piiEntitiesDetectionJobProperties_inputDataConfig = Lens.lens (\PiiEntitiesDetectionJobProperties' {inputDataConfig} -> inputDataConfig) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {inputDataConfig = a} :: PiiEntitiesDetectionJobProperties)

-- | Specifies whether the output provides the locations (offsets) of PII
-- entities or a file in which PII entities are redacted.
piiEntitiesDetectionJobProperties_mode :: Lens.Lens' PiiEntitiesDetectionJobProperties (Core.Maybe PiiEntitiesDetectionMode)
piiEntitiesDetectionJobProperties_mode = Lens.lens (\PiiEntitiesDetectionJobProperties' {mode} -> mode) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {mode = a} :: PiiEntitiesDetectionJobProperties)

-- | A description of the status of a job.
piiEntitiesDetectionJobProperties_message :: Lens.Lens' PiiEntitiesDetectionJobProperties (Core.Maybe Core.Text)
piiEntitiesDetectionJobProperties_message = Lens.lens (\PiiEntitiesDetectionJobProperties' {message} -> message) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {message = a} :: PiiEntitiesDetectionJobProperties)

-- | The current status of the PII entities detection job. If the status is
-- @FAILED@, the @Message@ field shows the reason for the failure.
piiEntitiesDetectionJobProperties_jobStatus :: Lens.Lens' PiiEntitiesDetectionJobProperties (Core.Maybe JobStatus)
piiEntitiesDetectionJobProperties_jobStatus = Lens.lens (\PiiEntitiesDetectionJobProperties' {jobStatus} -> jobStatus) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {jobStatus = a} :: PiiEntitiesDetectionJobProperties)

-- | The output data configuration that you supplied when you created the PII
-- entities detection job.
piiEntitiesDetectionJobProperties_outputDataConfig :: Lens.Lens' PiiEntitiesDetectionJobProperties (Core.Maybe PiiOutputDataConfig)
piiEntitiesDetectionJobProperties_outputDataConfig = Lens.lens (\PiiEntitiesDetectionJobProperties' {outputDataConfig} -> outputDataConfig) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {outputDataConfig = a} :: PiiEntitiesDetectionJobProperties)

-- | The time that the PII entities detection job completed.
piiEntitiesDetectionJobProperties_endTime :: Lens.Lens' PiiEntitiesDetectionJobProperties (Core.Maybe Core.UTCTime)
piiEntitiesDetectionJobProperties_endTime = Lens.lens (\PiiEntitiesDetectionJobProperties' {endTime} -> endTime) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {endTime = a} :: PiiEntitiesDetectionJobProperties) Core.. Lens.mapping Core._Time

-- | The time that the PII entities detection job was submitted for
-- processing.
piiEntitiesDetectionJobProperties_submitTime :: Lens.Lens' PiiEntitiesDetectionJobProperties (Core.Maybe Core.UTCTime)
piiEntitiesDetectionJobProperties_submitTime = Lens.lens (\PiiEntitiesDetectionJobProperties' {submitTime} -> submitTime) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {submitTime = a} :: PiiEntitiesDetectionJobProperties) Core.. Lens.mapping Core._Time

-- | The name that you assigned the PII entities detection job.
piiEntitiesDetectionJobProperties_jobName :: Lens.Lens' PiiEntitiesDetectionJobProperties (Core.Maybe Core.Text)
piiEntitiesDetectionJobProperties_jobName = Lens.lens (\PiiEntitiesDetectionJobProperties' {jobName} -> jobName) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {jobName = a} :: PiiEntitiesDetectionJobProperties)

-- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access
-- to your input data.
piiEntitiesDetectionJobProperties_dataAccessRoleArn :: Lens.Lens' PiiEntitiesDetectionJobProperties (Core.Maybe Core.Text)
piiEntitiesDetectionJobProperties_dataAccessRoleArn = Lens.lens (\PiiEntitiesDetectionJobProperties' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {dataAccessRoleArn = a} :: PiiEntitiesDetectionJobProperties)

-- | The identifier assigned to the PII entities detection job.
piiEntitiesDetectionJobProperties_jobId :: Lens.Lens' PiiEntitiesDetectionJobProperties (Core.Maybe Core.Text)
piiEntitiesDetectionJobProperties_jobId = Lens.lens (\PiiEntitiesDetectionJobProperties' {jobId} -> jobId) (\s@PiiEntitiesDetectionJobProperties' {} a -> s {jobId = a} :: PiiEntitiesDetectionJobProperties)

instance
  Core.FromJSON
    PiiEntitiesDetectionJobProperties
  where
  parseJSON =
    Core.withObject
      "PiiEntitiesDetectionJobProperties"
      ( \x ->
          PiiEntitiesDetectionJobProperties'
            Core.<$> (x Core..:? "RedactionConfig")
            Core.<*> (x Core..:? "LanguageCode")
            Core.<*> (x Core..:? "InputDataConfig")
            Core.<*> (x Core..:? "Mode")
            Core.<*> (x Core..:? "Message")
            Core.<*> (x Core..:? "JobStatus")
            Core.<*> (x Core..:? "OutputDataConfig")
            Core.<*> (x Core..:? "EndTime")
            Core.<*> (x Core..:? "SubmitTime")
            Core.<*> (x Core..:? "JobName")
            Core.<*> (x Core..:? "DataAccessRoleArn")
            Core.<*> (x Core..:? "JobId")
      )

instance
  Core.Hashable
    PiiEntitiesDetectionJobProperties

instance
  Core.NFData
    PiiEntitiesDetectionJobProperties
