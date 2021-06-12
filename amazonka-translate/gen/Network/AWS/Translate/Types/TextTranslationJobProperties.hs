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
-- Module      : Network.AWS.Translate.Types.TextTranslationJobProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.TextTranslationJobProperties where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Translate.Types.InputDataConfig
import Network.AWS.Translate.Types.JobDetails
import Network.AWS.Translate.Types.JobStatus
import Network.AWS.Translate.Types.OutputDataConfig

-- | Provides information about a translation job.
--
-- /See:/ 'newTextTranslationJobProperties' smart constructor.
data TextTranslationJobProperties = TextTranslationJobProperties'
  { -- | A list containing the names of the parallel data resources applied to
    -- the translation job.
    parallelDataNames :: Core.Maybe [Core.Text],
    -- | The input configuration properties that were specified when the job was
    -- requested.
    inputDataConfig :: Core.Maybe InputDataConfig,
    -- | The time at which the translation job was submitted.
    submittedTime :: Core.Maybe Core.POSIX,
    -- | An explanation of any errors that may have occured during the
    -- translation job.
    message :: Core.Maybe Core.Text,
    -- | The status of the translation job.
    jobStatus :: Core.Maybe JobStatus,
    -- | The number of documents successfully and unsuccessfully processed during
    -- the translation job.
    jobDetails :: Core.Maybe JobDetails,
    -- | The output configuration properties that were specified when the job was
    -- requested.
    outputDataConfig :: Core.Maybe OutputDataConfig,
    -- | The language code of the language of the target text. The language must
    -- be a language supported by Amazon Translate.
    targetLanguageCodes :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The time at which the translation job ended.
    endTime :: Core.Maybe Core.POSIX,
    -- | A list containing the names of the terminologies applied to a
    -- translation job. Only one terminology can be applied per
    -- StartTextTranslationJob request at this time.
    terminologyNames :: Core.Maybe [Core.Text],
    -- | The user-defined name of the translation job.
    jobName :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of an AWS Identity Access and Management
    -- (IAM) role that granted Amazon Translate read access to the job\'s input
    -- data.
    dataAccessRoleArn :: Core.Maybe Core.Text,
    -- | The ID of the translation job.
    jobId :: Core.Maybe Core.Text,
    -- | The language code of the language of the source text. The language must
    -- be a language supported by Amazon Translate.
    sourceLanguageCode :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TextTranslationJobProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parallelDataNames', 'textTranslationJobProperties_parallelDataNames' - A list containing the names of the parallel data resources applied to
-- the translation job.
--
-- 'inputDataConfig', 'textTranslationJobProperties_inputDataConfig' - The input configuration properties that were specified when the job was
-- requested.
--
-- 'submittedTime', 'textTranslationJobProperties_submittedTime' - The time at which the translation job was submitted.
--
-- 'message', 'textTranslationJobProperties_message' - An explanation of any errors that may have occured during the
-- translation job.
--
-- 'jobStatus', 'textTranslationJobProperties_jobStatus' - The status of the translation job.
--
-- 'jobDetails', 'textTranslationJobProperties_jobDetails' - The number of documents successfully and unsuccessfully processed during
-- the translation job.
--
-- 'outputDataConfig', 'textTranslationJobProperties_outputDataConfig' - The output configuration properties that were specified when the job was
-- requested.
--
-- 'targetLanguageCodes', 'textTranslationJobProperties_targetLanguageCodes' - The language code of the language of the target text. The language must
-- be a language supported by Amazon Translate.
--
-- 'endTime', 'textTranslationJobProperties_endTime' - The time at which the translation job ended.
--
-- 'terminologyNames', 'textTranslationJobProperties_terminologyNames' - A list containing the names of the terminologies applied to a
-- translation job. Only one terminology can be applied per
-- StartTextTranslationJob request at this time.
--
-- 'jobName', 'textTranslationJobProperties_jobName' - The user-defined name of the translation job.
--
-- 'dataAccessRoleArn', 'textTranslationJobProperties_dataAccessRoleArn' - The Amazon Resource Name (ARN) of an AWS Identity Access and Management
-- (IAM) role that granted Amazon Translate read access to the job\'s input
-- data.
--
-- 'jobId', 'textTranslationJobProperties_jobId' - The ID of the translation job.
--
-- 'sourceLanguageCode', 'textTranslationJobProperties_sourceLanguageCode' - The language code of the language of the source text. The language must
-- be a language supported by Amazon Translate.
newTextTranslationJobProperties ::
  TextTranslationJobProperties
newTextTranslationJobProperties =
  TextTranslationJobProperties'
    { parallelDataNames =
        Core.Nothing,
      inputDataConfig = Core.Nothing,
      submittedTime = Core.Nothing,
      message = Core.Nothing,
      jobStatus = Core.Nothing,
      jobDetails = Core.Nothing,
      outputDataConfig = Core.Nothing,
      targetLanguageCodes = Core.Nothing,
      endTime = Core.Nothing,
      terminologyNames = Core.Nothing,
      jobName = Core.Nothing,
      dataAccessRoleArn = Core.Nothing,
      jobId = Core.Nothing,
      sourceLanguageCode = Core.Nothing
    }

-- | A list containing the names of the parallel data resources applied to
-- the translation job.
textTranslationJobProperties_parallelDataNames :: Lens.Lens' TextTranslationJobProperties (Core.Maybe [Core.Text])
textTranslationJobProperties_parallelDataNames = Lens.lens (\TextTranslationJobProperties' {parallelDataNames} -> parallelDataNames) (\s@TextTranslationJobProperties' {} a -> s {parallelDataNames = a} :: TextTranslationJobProperties) Core.. Lens.mapping Lens._Coerce

-- | The input configuration properties that were specified when the job was
-- requested.
textTranslationJobProperties_inputDataConfig :: Lens.Lens' TextTranslationJobProperties (Core.Maybe InputDataConfig)
textTranslationJobProperties_inputDataConfig = Lens.lens (\TextTranslationJobProperties' {inputDataConfig} -> inputDataConfig) (\s@TextTranslationJobProperties' {} a -> s {inputDataConfig = a} :: TextTranslationJobProperties)

-- | The time at which the translation job was submitted.
textTranslationJobProperties_submittedTime :: Lens.Lens' TextTranslationJobProperties (Core.Maybe Core.UTCTime)
textTranslationJobProperties_submittedTime = Lens.lens (\TextTranslationJobProperties' {submittedTime} -> submittedTime) (\s@TextTranslationJobProperties' {} a -> s {submittedTime = a} :: TextTranslationJobProperties) Core.. Lens.mapping Core._Time

-- | An explanation of any errors that may have occured during the
-- translation job.
textTranslationJobProperties_message :: Lens.Lens' TextTranslationJobProperties (Core.Maybe Core.Text)
textTranslationJobProperties_message = Lens.lens (\TextTranslationJobProperties' {message} -> message) (\s@TextTranslationJobProperties' {} a -> s {message = a} :: TextTranslationJobProperties)

-- | The status of the translation job.
textTranslationJobProperties_jobStatus :: Lens.Lens' TextTranslationJobProperties (Core.Maybe JobStatus)
textTranslationJobProperties_jobStatus = Lens.lens (\TextTranslationJobProperties' {jobStatus} -> jobStatus) (\s@TextTranslationJobProperties' {} a -> s {jobStatus = a} :: TextTranslationJobProperties)

-- | The number of documents successfully and unsuccessfully processed during
-- the translation job.
textTranslationJobProperties_jobDetails :: Lens.Lens' TextTranslationJobProperties (Core.Maybe JobDetails)
textTranslationJobProperties_jobDetails = Lens.lens (\TextTranslationJobProperties' {jobDetails} -> jobDetails) (\s@TextTranslationJobProperties' {} a -> s {jobDetails = a} :: TextTranslationJobProperties)

-- | The output configuration properties that were specified when the job was
-- requested.
textTranslationJobProperties_outputDataConfig :: Lens.Lens' TextTranslationJobProperties (Core.Maybe OutputDataConfig)
textTranslationJobProperties_outputDataConfig = Lens.lens (\TextTranslationJobProperties' {outputDataConfig} -> outputDataConfig) (\s@TextTranslationJobProperties' {} a -> s {outputDataConfig = a} :: TextTranslationJobProperties)

-- | The language code of the language of the target text. The language must
-- be a language supported by Amazon Translate.
textTranslationJobProperties_targetLanguageCodes :: Lens.Lens' TextTranslationJobProperties (Core.Maybe (Core.NonEmpty Core.Text))
textTranslationJobProperties_targetLanguageCodes = Lens.lens (\TextTranslationJobProperties' {targetLanguageCodes} -> targetLanguageCodes) (\s@TextTranslationJobProperties' {} a -> s {targetLanguageCodes = a} :: TextTranslationJobProperties) Core.. Lens.mapping Lens._Coerce

-- | The time at which the translation job ended.
textTranslationJobProperties_endTime :: Lens.Lens' TextTranslationJobProperties (Core.Maybe Core.UTCTime)
textTranslationJobProperties_endTime = Lens.lens (\TextTranslationJobProperties' {endTime} -> endTime) (\s@TextTranslationJobProperties' {} a -> s {endTime = a} :: TextTranslationJobProperties) Core.. Lens.mapping Core._Time

-- | A list containing the names of the terminologies applied to a
-- translation job. Only one terminology can be applied per
-- StartTextTranslationJob request at this time.
textTranslationJobProperties_terminologyNames :: Lens.Lens' TextTranslationJobProperties (Core.Maybe [Core.Text])
textTranslationJobProperties_terminologyNames = Lens.lens (\TextTranslationJobProperties' {terminologyNames} -> terminologyNames) (\s@TextTranslationJobProperties' {} a -> s {terminologyNames = a} :: TextTranslationJobProperties) Core.. Lens.mapping Lens._Coerce

-- | The user-defined name of the translation job.
textTranslationJobProperties_jobName :: Lens.Lens' TextTranslationJobProperties (Core.Maybe Core.Text)
textTranslationJobProperties_jobName = Lens.lens (\TextTranslationJobProperties' {jobName} -> jobName) (\s@TextTranslationJobProperties' {} a -> s {jobName = a} :: TextTranslationJobProperties)

-- | The Amazon Resource Name (ARN) of an AWS Identity Access and Management
-- (IAM) role that granted Amazon Translate read access to the job\'s input
-- data.
textTranslationJobProperties_dataAccessRoleArn :: Lens.Lens' TextTranslationJobProperties (Core.Maybe Core.Text)
textTranslationJobProperties_dataAccessRoleArn = Lens.lens (\TextTranslationJobProperties' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@TextTranslationJobProperties' {} a -> s {dataAccessRoleArn = a} :: TextTranslationJobProperties)

-- | The ID of the translation job.
textTranslationJobProperties_jobId :: Lens.Lens' TextTranslationJobProperties (Core.Maybe Core.Text)
textTranslationJobProperties_jobId = Lens.lens (\TextTranslationJobProperties' {jobId} -> jobId) (\s@TextTranslationJobProperties' {} a -> s {jobId = a} :: TextTranslationJobProperties)

-- | The language code of the language of the source text. The language must
-- be a language supported by Amazon Translate.
textTranslationJobProperties_sourceLanguageCode :: Lens.Lens' TextTranslationJobProperties (Core.Maybe Core.Text)
textTranslationJobProperties_sourceLanguageCode = Lens.lens (\TextTranslationJobProperties' {sourceLanguageCode} -> sourceLanguageCode) (\s@TextTranslationJobProperties' {} a -> s {sourceLanguageCode = a} :: TextTranslationJobProperties)

instance Core.FromJSON TextTranslationJobProperties where
  parseJSON =
    Core.withObject
      "TextTranslationJobProperties"
      ( \x ->
          TextTranslationJobProperties'
            Core.<$> (x Core..:? "ParallelDataNames" Core..!= Core.mempty)
            Core.<*> (x Core..:? "InputDataConfig")
            Core.<*> (x Core..:? "SubmittedTime")
            Core.<*> (x Core..:? "Message")
            Core.<*> (x Core..:? "JobStatus")
            Core.<*> (x Core..:? "JobDetails")
            Core.<*> (x Core..:? "OutputDataConfig")
            Core.<*> (x Core..:? "TargetLanguageCodes")
            Core.<*> (x Core..:? "EndTime")
            Core.<*> (x Core..:? "TerminologyNames" Core..!= Core.mempty)
            Core.<*> (x Core..:? "JobName")
            Core.<*> (x Core..:? "DataAccessRoleArn")
            Core.<*> (x Core..:? "JobId")
            Core.<*> (x Core..:? "SourceLanguageCode")
      )

instance Core.Hashable TextTranslationJobProperties

instance Core.NFData TextTranslationJobProperties
