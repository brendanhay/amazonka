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
-- Module      : Amazonka.Translate.Types.TextTranslationJobProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Translate.Types.TextTranslationJobProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Translate.Types.InputDataConfig
import Amazonka.Translate.Types.JobDetails
import Amazonka.Translate.Types.JobStatus
import Amazonka.Translate.Types.OutputDataConfig

-- | Provides information about a translation job.
--
-- /See:/ 'newTextTranslationJobProperties' smart constructor.
data TextTranslationJobProperties = TextTranslationJobProperties'
  { -- | The ID of the translation job.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The language code of the language of the target text. The language must
    -- be a language supported by Amazon Translate.
    targetLanguageCodes :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The user-defined name of the translation job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The time at which the translation job was submitted.
    submittedTime :: Prelude.Maybe Core.POSIX,
    -- | The input configuration properties that were specified when the job was
    -- requested.
    inputDataConfig :: Prelude.Maybe InputDataConfig,
    -- | A list containing the names of the parallel data resources applied to
    -- the translation job.
    parallelDataNames :: Prelude.Maybe [Prelude.Text],
    -- | A list containing the names of the terminologies applied to a
    -- translation job. Only one terminology can be applied per
    -- StartTextTranslationJob request at this time.
    terminologyNames :: Prelude.Maybe [Prelude.Text],
    -- | The language code of the language of the source text. The language must
    -- be a language supported by Amazon Translate.
    sourceLanguageCode :: Prelude.Maybe Prelude.Text,
    -- | The time at which the translation job ended.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | The output configuration properties that were specified when the job was
    -- requested.
    outputDataConfig :: Prelude.Maybe OutputDataConfig,
    -- | The number of documents successfully and unsuccessfully processed during
    -- the translation job.
    jobDetails :: Prelude.Maybe JobDetails,
    -- | The Amazon Resource Name (ARN) of an AWS Identity Access and Management
    -- (IAM) role that granted Amazon Translate read access to the job\'s input
    -- data.
    dataAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the translation job.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | An explanation of any errors that may have occured during the
    -- translation job.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TextTranslationJobProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'textTranslationJobProperties_jobId' - The ID of the translation job.
--
-- 'targetLanguageCodes', 'textTranslationJobProperties_targetLanguageCodes' - The language code of the language of the target text. The language must
-- be a language supported by Amazon Translate.
--
-- 'jobName', 'textTranslationJobProperties_jobName' - The user-defined name of the translation job.
--
-- 'submittedTime', 'textTranslationJobProperties_submittedTime' - The time at which the translation job was submitted.
--
-- 'inputDataConfig', 'textTranslationJobProperties_inputDataConfig' - The input configuration properties that were specified when the job was
-- requested.
--
-- 'parallelDataNames', 'textTranslationJobProperties_parallelDataNames' - A list containing the names of the parallel data resources applied to
-- the translation job.
--
-- 'terminologyNames', 'textTranslationJobProperties_terminologyNames' - A list containing the names of the terminologies applied to a
-- translation job. Only one terminology can be applied per
-- StartTextTranslationJob request at this time.
--
-- 'sourceLanguageCode', 'textTranslationJobProperties_sourceLanguageCode' - The language code of the language of the source text. The language must
-- be a language supported by Amazon Translate.
--
-- 'endTime', 'textTranslationJobProperties_endTime' - The time at which the translation job ended.
--
-- 'outputDataConfig', 'textTranslationJobProperties_outputDataConfig' - The output configuration properties that were specified when the job was
-- requested.
--
-- 'jobDetails', 'textTranslationJobProperties_jobDetails' - The number of documents successfully and unsuccessfully processed during
-- the translation job.
--
-- 'dataAccessRoleArn', 'textTranslationJobProperties_dataAccessRoleArn' - The Amazon Resource Name (ARN) of an AWS Identity Access and Management
-- (IAM) role that granted Amazon Translate read access to the job\'s input
-- data.
--
-- 'jobStatus', 'textTranslationJobProperties_jobStatus' - The status of the translation job.
--
-- 'message', 'textTranslationJobProperties_message' - An explanation of any errors that may have occured during the
-- translation job.
newTextTranslationJobProperties ::
  TextTranslationJobProperties
newTextTranslationJobProperties =
  TextTranslationJobProperties'
    { jobId =
        Prelude.Nothing,
      targetLanguageCodes = Prelude.Nothing,
      jobName = Prelude.Nothing,
      submittedTime = Prelude.Nothing,
      inputDataConfig = Prelude.Nothing,
      parallelDataNames = Prelude.Nothing,
      terminologyNames = Prelude.Nothing,
      sourceLanguageCode = Prelude.Nothing,
      endTime = Prelude.Nothing,
      outputDataConfig = Prelude.Nothing,
      jobDetails = Prelude.Nothing,
      dataAccessRoleArn = Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The ID of the translation job.
textTranslationJobProperties_jobId :: Lens.Lens' TextTranslationJobProperties (Prelude.Maybe Prelude.Text)
textTranslationJobProperties_jobId = Lens.lens (\TextTranslationJobProperties' {jobId} -> jobId) (\s@TextTranslationJobProperties' {} a -> s {jobId = a} :: TextTranslationJobProperties)

-- | The language code of the language of the target text. The language must
-- be a language supported by Amazon Translate.
textTranslationJobProperties_targetLanguageCodes :: Lens.Lens' TextTranslationJobProperties (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
textTranslationJobProperties_targetLanguageCodes = Lens.lens (\TextTranslationJobProperties' {targetLanguageCodes} -> targetLanguageCodes) (\s@TextTranslationJobProperties' {} a -> s {targetLanguageCodes = a} :: TextTranslationJobProperties) Prelude.. Lens.mapping Lens.coerced

-- | The user-defined name of the translation job.
textTranslationJobProperties_jobName :: Lens.Lens' TextTranslationJobProperties (Prelude.Maybe Prelude.Text)
textTranslationJobProperties_jobName = Lens.lens (\TextTranslationJobProperties' {jobName} -> jobName) (\s@TextTranslationJobProperties' {} a -> s {jobName = a} :: TextTranslationJobProperties)

-- | The time at which the translation job was submitted.
textTranslationJobProperties_submittedTime :: Lens.Lens' TextTranslationJobProperties (Prelude.Maybe Prelude.UTCTime)
textTranslationJobProperties_submittedTime = Lens.lens (\TextTranslationJobProperties' {submittedTime} -> submittedTime) (\s@TextTranslationJobProperties' {} a -> s {submittedTime = a} :: TextTranslationJobProperties) Prelude.. Lens.mapping Core._Time

-- | The input configuration properties that were specified when the job was
-- requested.
textTranslationJobProperties_inputDataConfig :: Lens.Lens' TextTranslationJobProperties (Prelude.Maybe InputDataConfig)
textTranslationJobProperties_inputDataConfig = Lens.lens (\TextTranslationJobProperties' {inputDataConfig} -> inputDataConfig) (\s@TextTranslationJobProperties' {} a -> s {inputDataConfig = a} :: TextTranslationJobProperties)

-- | A list containing the names of the parallel data resources applied to
-- the translation job.
textTranslationJobProperties_parallelDataNames :: Lens.Lens' TextTranslationJobProperties (Prelude.Maybe [Prelude.Text])
textTranslationJobProperties_parallelDataNames = Lens.lens (\TextTranslationJobProperties' {parallelDataNames} -> parallelDataNames) (\s@TextTranslationJobProperties' {} a -> s {parallelDataNames = a} :: TextTranslationJobProperties) Prelude.. Lens.mapping Lens.coerced

-- | A list containing the names of the terminologies applied to a
-- translation job. Only one terminology can be applied per
-- StartTextTranslationJob request at this time.
textTranslationJobProperties_terminologyNames :: Lens.Lens' TextTranslationJobProperties (Prelude.Maybe [Prelude.Text])
textTranslationJobProperties_terminologyNames = Lens.lens (\TextTranslationJobProperties' {terminologyNames} -> terminologyNames) (\s@TextTranslationJobProperties' {} a -> s {terminologyNames = a} :: TextTranslationJobProperties) Prelude.. Lens.mapping Lens.coerced

-- | The language code of the language of the source text. The language must
-- be a language supported by Amazon Translate.
textTranslationJobProperties_sourceLanguageCode :: Lens.Lens' TextTranslationJobProperties (Prelude.Maybe Prelude.Text)
textTranslationJobProperties_sourceLanguageCode = Lens.lens (\TextTranslationJobProperties' {sourceLanguageCode} -> sourceLanguageCode) (\s@TextTranslationJobProperties' {} a -> s {sourceLanguageCode = a} :: TextTranslationJobProperties)

-- | The time at which the translation job ended.
textTranslationJobProperties_endTime :: Lens.Lens' TextTranslationJobProperties (Prelude.Maybe Prelude.UTCTime)
textTranslationJobProperties_endTime = Lens.lens (\TextTranslationJobProperties' {endTime} -> endTime) (\s@TextTranslationJobProperties' {} a -> s {endTime = a} :: TextTranslationJobProperties) Prelude.. Lens.mapping Core._Time

-- | The output configuration properties that were specified when the job was
-- requested.
textTranslationJobProperties_outputDataConfig :: Lens.Lens' TextTranslationJobProperties (Prelude.Maybe OutputDataConfig)
textTranslationJobProperties_outputDataConfig = Lens.lens (\TextTranslationJobProperties' {outputDataConfig} -> outputDataConfig) (\s@TextTranslationJobProperties' {} a -> s {outputDataConfig = a} :: TextTranslationJobProperties)

-- | The number of documents successfully and unsuccessfully processed during
-- the translation job.
textTranslationJobProperties_jobDetails :: Lens.Lens' TextTranslationJobProperties (Prelude.Maybe JobDetails)
textTranslationJobProperties_jobDetails = Lens.lens (\TextTranslationJobProperties' {jobDetails} -> jobDetails) (\s@TextTranslationJobProperties' {} a -> s {jobDetails = a} :: TextTranslationJobProperties)

-- | The Amazon Resource Name (ARN) of an AWS Identity Access and Management
-- (IAM) role that granted Amazon Translate read access to the job\'s input
-- data.
textTranslationJobProperties_dataAccessRoleArn :: Lens.Lens' TextTranslationJobProperties (Prelude.Maybe Prelude.Text)
textTranslationJobProperties_dataAccessRoleArn = Lens.lens (\TextTranslationJobProperties' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@TextTranslationJobProperties' {} a -> s {dataAccessRoleArn = a} :: TextTranslationJobProperties)

-- | The status of the translation job.
textTranslationJobProperties_jobStatus :: Lens.Lens' TextTranslationJobProperties (Prelude.Maybe JobStatus)
textTranslationJobProperties_jobStatus = Lens.lens (\TextTranslationJobProperties' {jobStatus} -> jobStatus) (\s@TextTranslationJobProperties' {} a -> s {jobStatus = a} :: TextTranslationJobProperties)

-- | An explanation of any errors that may have occured during the
-- translation job.
textTranslationJobProperties_message :: Lens.Lens' TextTranslationJobProperties (Prelude.Maybe Prelude.Text)
textTranslationJobProperties_message = Lens.lens (\TextTranslationJobProperties' {message} -> message) (\s@TextTranslationJobProperties' {} a -> s {message = a} :: TextTranslationJobProperties)

instance Core.FromJSON TextTranslationJobProperties where
  parseJSON =
    Core.withObject
      "TextTranslationJobProperties"
      ( \x ->
          TextTranslationJobProperties'
            Prelude.<$> (x Core..:? "JobId")
            Prelude.<*> (x Core..:? "TargetLanguageCodes")
            Prelude.<*> (x Core..:? "JobName")
            Prelude.<*> (x Core..:? "SubmittedTime")
            Prelude.<*> (x Core..:? "InputDataConfig")
            Prelude.<*> ( x Core..:? "ParallelDataNames"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "TerminologyNames"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "SourceLanguageCode")
            Prelude.<*> (x Core..:? "EndTime")
            Prelude.<*> (x Core..:? "OutputDataConfig")
            Prelude.<*> (x Core..:? "JobDetails")
            Prelude.<*> (x Core..:? "DataAccessRoleArn")
            Prelude.<*> (x Core..:? "JobStatus")
            Prelude.<*> (x Core..:? "Message")
      )

instance
  Prelude.Hashable
    TextTranslationJobProperties
  where
  hashWithSalt salt' TextTranslationJobProperties' {..} =
    salt' `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` jobStatus
      `Prelude.hashWithSalt` dataAccessRoleArn
      `Prelude.hashWithSalt` jobDetails
      `Prelude.hashWithSalt` outputDataConfig
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` sourceLanguageCode
      `Prelude.hashWithSalt` terminologyNames
      `Prelude.hashWithSalt` parallelDataNames
      `Prelude.hashWithSalt` inputDataConfig
      `Prelude.hashWithSalt` submittedTime
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` targetLanguageCodes
      `Prelude.hashWithSalt` jobId

instance Prelude.NFData TextTranslationJobProperties where
  rnf TextTranslationJobProperties' {..} =
    Prelude.rnf jobId `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf dataAccessRoleArn
      `Prelude.seq` Prelude.rnf jobDetails
      `Prelude.seq` Prelude.rnf outputDataConfig
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf sourceLanguageCode
      `Prelude.seq` Prelude.rnf terminologyNames
      `Prelude.seq` Prelude.rnf parallelDataNames
      `Prelude.seq` Prelude.rnf inputDataConfig
      `Prelude.seq` Prelude.rnf submittedTime
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf targetLanguageCodes
