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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Translate.Types.TextTranslationJobProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Translate.Types.InputDataConfig
import Amazonka.Translate.Types.JobDetails
import Amazonka.Translate.Types.JobStatus
import Amazonka.Translate.Types.OutputDataConfig
import Amazonka.Translate.Types.TranslationSettings

-- | Provides information about a translation job.
--
-- /See:/ 'newTextTranslationJobProperties' smart constructor.
data TextTranslationJobProperties = TextTranslationJobProperties'
  { -- | The Amazon Resource Name (ARN) of an AWS Identity Access and Management
    -- (IAM) role that granted Amazon Translate read access to the job\'s input
    -- data.
    dataAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The time at which the translation job ended.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The input configuration properties that were specified when the job was
    -- requested.
    inputDataConfig :: Prelude.Maybe InputDataConfig,
    -- | The number of documents successfully and unsuccessfully processed during
    -- the translation job.
    jobDetails :: Prelude.Maybe JobDetails,
    -- | The ID of the translation job.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The user-defined name of the translation job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The status of the translation job.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | An explanation of any errors that may have occurred during the
    -- translation job.
    message :: Prelude.Maybe Prelude.Text,
    -- | The output configuration properties that were specified when the job was
    -- requested.
    outputDataConfig :: Prelude.Maybe OutputDataConfig,
    -- | A list containing the names of the parallel data resources applied to
    -- the translation job.
    parallelDataNames :: Prelude.Maybe [Prelude.Text],
    -- | Settings that modify the translation output.
    settings :: Prelude.Maybe TranslationSettings,
    -- | The language code of the language of the source text. The language must
    -- be a language supported by Amazon Translate.
    sourceLanguageCode :: Prelude.Maybe Prelude.Text,
    -- | The time at which the translation job was submitted.
    submittedTime :: Prelude.Maybe Data.POSIX,
    -- | The language code of the language of the target text. The language must
    -- be a language supported by Amazon Translate.
    targetLanguageCodes :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A list containing the names of the terminologies applied to a
    -- translation job. Only one terminology can be applied per
    -- StartTextTranslationJob request at this time.
    terminologyNames :: Prelude.Maybe [Prelude.Text]
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
-- 'dataAccessRoleArn', 'textTranslationJobProperties_dataAccessRoleArn' - The Amazon Resource Name (ARN) of an AWS Identity Access and Management
-- (IAM) role that granted Amazon Translate read access to the job\'s input
-- data.
--
-- 'endTime', 'textTranslationJobProperties_endTime' - The time at which the translation job ended.
--
-- 'inputDataConfig', 'textTranslationJobProperties_inputDataConfig' - The input configuration properties that were specified when the job was
-- requested.
--
-- 'jobDetails', 'textTranslationJobProperties_jobDetails' - The number of documents successfully and unsuccessfully processed during
-- the translation job.
--
-- 'jobId', 'textTranslationJobProperties_jobId' - The ID of the translation job.
--
-- 'jobName', 'textTranslationJobProperties_jobName' - The user-defined name of the translation job.
--
-- 'jobStatus', 'textTranslationJobProperties_jobStatus' - The status of the translation job.
--
-- 'message', 'textTranslationJobProperties_message' - An explanation of any errors that may have occurred during the
-- translation job.
--
-- 'outputDataConfig', 'textTranslationJobProperties_outputDataConfig' - The output configuration properties that were specified when the job was
-- requested.
--
-- 'parallelDataNames', 'textTranslationJobProperties_parallelDataNames' - A list containing the names of the parallel data resources applied to
-- the translation job.
--
-- 'settings', 'textTranslationJobProperties_settings' - Settings that modify the translation output.
--
-- 'sourceLanguageCode', 'textTranslationJobProperties_sourceLanguageCode' - The language code of the language of the source text. The language must
-- be a language supported by Amazon Translate.
--
-- 'submittedTime', 'textTranslationJobProperties_submittedTime' - The time at which the translation job was submitted.
--
-- 'targetLanguageCodes', 'textTranslationJobProperties_targetLanguageCodes' - The language code of the language of the target text. The language must
-- be a language supported by Amazon Translate.
--
-- 'terminologyNames', 'textTranslationJobProperties_terminologyNames' - A list containing the names of the terminologies applied to a
-- translation job. Only one terminology can be applied per
-- StartTextTranslationJob request at this time.
newTextTranslationJobProperties ::
  TextTranslationJobProperties
newTextTranslationJobProperties =
  TextTranslationJobProperties'
    { dataAccessRoleArn =
        Prelude.Nothing,
      endTime = Prelude.Nothing,
      inputDataConfig = Prelude.Nothing,
      jobDetails = Prelude.Nothing,
      jobId = Prelude.Nothing,
      jobName = Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      message = Prelude.Nothing,
      outputDataConfig = Prelude.Nothing,
      parallelDataNames = Prelude.Nothing,
      settings = Prelude.Nothing,
      sourceLanguageCode = Prelude.Nothing,
      submittedTime = Prelude.Nothing,
      targetLanguageCodes = Prelude.Nothing,
      terminologyNames = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of an AWS Identity Access and Management
-- (IAM) role that granted Amazon Translate read access to the job\'s input
-- data.
textTranslationJobProperties_dataAccessRoleArn :: Lens.Lens' TextTranslationJobProperties (Prelude.Maybe Prelude.Text)
textTranslationJobProperties_dataAccessRoleArn = Lens.lens (\TextTranslationJobProperties' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@TextTranslationJobProperties' {} a -> s {dataAccessRoleArn = a} :: TextTranslationJobProperties)

-- | The time at which the translation job ended.
textTranslationJobProperties_endTime :: Lens.Lens' TextTranslationJobProperties (Prelude.Maybe Prelude.UTCTime)
textTranslationJobProperties_endTime = Lens.lens (\TextTranslationJobProperties' {endTime} -> endTime) (\s@TextTranslationJobProperties' {} a -> s {endTime = a} :: TextTranslationJobProperties) Prelude.. Lens.mapping Data._Time

-- | The input configuration properties that were specified when the job was
-- requested.
textTranslationJobProperties_inputDataConfig :: Lens.Lens' TextTranslationJobProperties (Prelude.Maybe InputDataConfig)
textTranslationJobProperties_inputDataConfig = Lens.lens (\TextTranslationJobProperties' {inputDataConfig} -> inputDataConfig) (\s@TextTranslationJobProperties' {} a -> s {inputDataConfig = a} :: TextTranslationJobProperties)

-- | The number of documents successfully and unsuccessfully processed during
-- the translation job.
textTranslationJobProperties_jobDetails :: Lens.Lens' TextTranslationJobProperties (Prelude.Maybe JobDetails)
textTranslationJobProperties_jobDetails = Lens.lens (\TextTranslationJobProperties' {jobDetails} -> jobDetails) (\s@TextTranslationJobProperties' {} a -> s {jobDetails = a} :: TextTranslationJobProperties)

-- | The ID of the translation job.
textTranslationJobProperties_jobId :: Lens.Lens' TextTranslationJobProperties (Prelude.Maybe Prelude.Text)
textTranslationJobProperties_jobId = Lens.lens (\TextTranslationJobProperties' {jobId} -> jobId) (\s@TextTranslationJobProperties' {} a -> s {jobId = a} :: TextTranslationJobProperties)

-- | The user-defined name of the translation job.
textTranslationJobProperties_jobName :: Lens.Lens' TextTranslationJobProperties (Prelude.Maybe Prelude.Text)
textTranslationJobProperties_jobName = Lens.lens (\TextTranslationJobProperties' {jobName} -> jobName) (\s@TextTranslationJobProperties' {} a -> s {jobName = a} :: TextTranslationJobProperties)

-- | The status of the translation job.
textTranslationJobProperties_jobStatus :: Lens.Lens' TextTranslationJobProperties (Prelude.Maybe JobStatus)
textTranslationJobProperties_jobStatus = Lens.lens (\TextTranslationJobProperties' {jobStatus} -> jobStatus) (\s@TextTranslationJobProperties' {} a -> s {jobStatus = a} :: TextTranslationJobProperties)

-- | An explanation of any errors that may have occurred during the
-- translation job.
textTranslationJobProperties_message :: Lens.Lens' TextTranslationJobProperties (Prelude.Maybe Prelude.Text)
textTranslationJobProperties_message = Lens.lens (\TextTranslationJobProperties' {message} -> message) (\s@TextTranslationJobProperties' {} a -> s {message = a} :: TextTranslationJobProperties)

-- | The output configuration properties that were specified when the job was
-- requested.
textTranslationJobProperties_outputDataConfig :: Lens.Lens' TextTranslationJobProperties (Prelude.Maybe OutputDataConfig)
textTranslationJobProperties_outputDataConfig = Lens.lens (\TextTranslationJobProperties' {outputDataConfig} -> outputDataConfig) (\s@TextTranslationJobProperties' {} a -> s {outputDataConfig = a} :: TextTranslationJobProperties)

-- | A list containing the names of the parallel data resources applied to
-- the translation job.
textTranslationJobProperties_parallelDataNames :: Lens.Lens' TextTranslationJobProperties (Prelude.Maybe [Prelude.Text])
textTranslationJobProperties_parallelDataNames = Lens.lens (\TextTranslationJobProperties' {parallelDataNames} -> parallelDataNames) (\s@TextTranslationJobProperties' {} a -> s {parallelDataNames = a} :: TextTranslationJobProperties) Prelude.. Lens.mapping Lens.coerced

-- | Settings that modify the translation output.
textTranslationJobProperties_settings :: Lens.Lens' TextTranslationJobProperties (Prelude.Maybe TranslationSettings)
textTranslationJobProperties_settings = Lens.lens (\TextTranslationJobProperties' {settings} -> settings) (\s@TextTranslationJobProperties' {} a -> s {settings = a} :: TextTranslationJobProperties)

-- | The language code of the language of the source text. The language must
-- be a language supported by Amazon Translate.
textTranslationJobProperties_sourceLanguageCode :: Lens.Lens' TextTranslationJobProperties (Prelude.Maybe Prelude.Text)
textTranslationJobProperties_sourceLanguageCode = Lens.lens (\TextTranslationJobProperties' {sourceLanguageCode} -> sourceLanguageCode) (\s@TextTranslationJobProperties' {} a -> s {sourceLanguageCode = a} :: TextTranslationJobProperties)

-- | The time at which the translation job was submitted.
textTranslationJobProperties_submittedTime :: Lens.Lens' TextTranslationJobProperties (Prelude.Maybe Prelude.UTCTime)
textTranslationJobProperties_submittedTime = Lens.lens (\TextTranslationJobProperties' {submittedTime} -> submittedTime) (\s@TextTranslationJobProperties' {} a -> s {submittedTime = a} :: TextTranslationJobProperties) Prelude.. Lens.mapping Data._Time

-- | The language code of the language of the target text. The language must
-- be a language supported by Amazon Translate.
textTranslationJobProperties_targetLanguageCodes :: Lens.Lens' TextTranslationJobProperties (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
textTranslationJobProperties_targetLanguageCodes = Lens.lens (\TextTranslationJobProperties' {targetLanguageCodes} -> targetLanguageCodes) (\s@TextTranslationJobProperties' {} a -> s {targetLanguageCodes = a} :: TextTranslationJobProperties) Prelude.. Lens.mapping Lens.coerced

-- | A list containing the names of the terminologies applied to a
-- translation job. Only one terminology can be applied per
-- StartTextTranslationJob request at this time.
textTranslationJobProperties_terminologyNames :: Lens.Lens' TextTranslationJobProperties (Prelude.Maybe [Prelude.Text])
textTranslationJobProperties_terminologyNames = Lens.lens (\TextTranslationJobProperties' {terminologyNames} -> terminologyNames) (\s@TextTranslationJobProperties' {} a -> s {terminologyNames = a} :: TextTranslationJobProperties) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON TextTranslationJobProperties where
  parseJSON =
    Data.withObject
      "TextTranslationJobProperties"
      ( \x ->
          TextTranslationJobProperties'
            Prelude.<$> (x Data..:? "DataAccessRoleArn")
            Prelude.<*> (x Data..:? "EndTime")
            Prelude.<*> (x Data..:? "InputDataConfig")
            Prelude.<*> (x Data..:? "JobDetails")
            Prelude.<*> (x Data..:? "JobId")
            Prelude.<*> (x Data..:? "JobName")
            Prelude.<*> (x Data..:? "JobStatus")
            Prelude.<*> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "OutputDataConfig")
            Prelude.<*> ( x
                            Data..:? "ParallelDataNames"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Settings")
            Prelude.<*> (x Data..:? "SourceLanguageCode")
            Prelude.<*> (x Data..:? "SubmittedTime")
            Prelude.<*> (x Data..:? "TargetLanguageCodes")
            Prelude.<*> ( x
                            Data..:? "TerminologyNames"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    TextTranslationJobProperties
  where
  hashWithSalt _salt TextTranslationJobProperties' {..} =
    _salt
      `Prelude.hashWithSalt` dataAccessRoleArn
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` inputDataConfig
      `Prelude.hashWithSalt` jobDetails
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` jobStatus
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` outputDataConfig
      `Prelude.hashWithSalt` parallelDataNames
      `Prelude.hashWithSalt` settings
      `Prelude.hashWithSalt` sourceLanguageCode
      `Prelude.hashWithSalt` submittedTime
      `Prelude.hashWithSalt` targetLanguageCodes
      `Prelude.hashWithSalt` terminologyNames

instance Prelude.NFData TextTranslationJobProperties where
  rnf TextTranslationJobProperties' {..} =
    Prelude.rnf dataAccessRoleArn
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf inputDataConfig
      `Prelude.seq` Prelude.rnf jobDetails
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf outputDataConfig
      `Prelude.seq` Prelude.rnf parallelDataNames
      `Prelude.seq` Prelude.rnf settings
      `Prelude.seq` Prelude.rnf sourceLanguageCode
      `Prelude.seq` Prelude.rnf submittedTime
      `Prelude.seq` Prelude.rnf targetLanguageCodes
      `Prelude.seq` Prelude.rnf terminologyNames
