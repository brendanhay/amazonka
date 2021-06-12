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
-- Module      : Network.AWS.Transcribe.Types.TranscriptionJobSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.TranscriptionJobSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Transcribe.Types.ContentRedaction
import Network.AWS.Transcribe.Types.LanguageCode
import Network.AWS.Transcribe.Types.ModelSettings
import Network.AWS.Transcribe.Types.OutputLocationType
import Network.AWS.Transcribe.Types.TranscriptionJobStatus

-- | Provides a summary of information about a transcription job.
--
-- /See:/ 'newTranscriptionJobSummary' smart constructor.
data TranscriptionJobSummary = TranscriptionJobSummary'
  { -- | The language code for the input speech.
    languageCode :: Core.Maybe LanguageCode,
    -- | The content redaction settings of the transcription job.
    contentRedaction :: Core.Maybe ContentRedaction,
    -- | A timestamp that shows when the job was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | A timestamp that shows when the job was completed.
    completionTime :: Core.Maybe Core.POSIX,
    -- | The name of the transcription job.
    transcriptionJobName :: Core.Maybe Core.Text,
    -- | Whether automatic language identification was enabled for a
    -- transcription job.
    identifyLanguage :: Core.Maybe Core.Bool,
    -- | A timestamp that shows when the job started processing.
    startTime :: Core.Maybe Core.POSIX,
    -- | The status of the transcription job. When the status is @COMPLETED@, use
    -- the @GetTranscriptionJob@ operation to get the results of the
    -- transcription.
    transcriptionJobStatus :: Core.Maybe TranscriptionJobStatus,
    modelSettings :: Core.Maybe ModelSettings,
    -- | Indicates the location of the output of the transcription job.
    --
    -- If the value is @CUSTOMER_BUCKET@ then the location is the S3 bucket
    -- specified in the @outputBucketName@ field when the transcription job was
    -- started with the @StartTranscriptionJob@ operation.
    --
    -- If the value is @SERVICE_BUCKET@ then the output is stored by Amazon
    -- Transcribe and can be retrieved using the URI in the
    -- @GetTranscriptionJob@ response\'s @TranscriptFileUri@ field.
    outputLocationType :: Core.Maybe OutputLocationType,
    -- | A value between zero and one that Amazon Transcribe assigned to the
    -- language it identified in the source audio. A higher score indicates
    -- that Amazon Transcribe is more confident in the language it identified.
    identifiedLanguageScore :: Core.Maybe Core.Double,
    -- | If the @TranscriptionJobStatus@ field is @FAILED@, a description of the
    -- error.
    failureReason :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TranscriptionJobSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languageCode', 'transcriptionJobSummary_languageCode' - The language code for the input speech.
--
-- 'contentRedaction', 'transcriptionJobSummary_contentRedaction' - The content redaction settings of the transcription job.
--
-- 'creationTime', 'transcriptionJobSummary_creationTime' - A timestamp that shows when the job was created.
--
-- 'completionTime', 'transcriptionJobSummary_completionTime' - A timestamp that shows when the job was completed.
--
-- 'transcriptionJobName', 'transcriptionJobSummary_transcriptionJobName' - The name of the transcription job.
--
-- 'identifyLanguage', 'transcriptionJobSummary_identifyLanguage' - Whether automatic language identification was enabled for a
-- transcription job.
--
-- 'startTime', 'transcriptionJobSummary_startTime' - A timestamp that shows when the job started processing.
--
-- 'transcriptionJobStatus', 'transcriptionJobSummary_transcriptionJobStatus' - The status of the transcription job. When the status is @COMPLETED@, use
-- the @GetTranscriptionJob@ operation to get the results of the
-- transcription.
--
-- 'modelSettings', 'transcriptionJobSummary_modelSettings' - Undocumented member.
--
-- 'outputLocationType', 'transcriptionJobSummary_outputLocationType' - Indicates the location of the output of the transcription job.
--
-- If the value is @CUSTOMER_BUCKET@ then the location is the S3 bucket
-- specified in the @outputBucketName@ field when the transcription job was
-- started with the @StartTranscriptionJob@ operation.
--
-- If the value is @SERVICE_BUCKET@ then the output is stored by Amazon
-- Transcribe and can be retrieved using the URI in the
-- @GetTranscriptionJob@ response\'s @TranscriptFileUri@ field.
--
-- 'identifiedLanguageScore', 'transcriptionJobSummary_identifiedLanguageScore' - A value between zero and one that Amazon Transcribe assigned to the
-- language it identified in the source audio. A higher score indicates
-- that Amazon Transcribe is more confident in the language it identified.
--
-- 'failureReason', 'transcriptionJobSummary_failureReason' - If the @TranscriptionJobStatus@ field is @FAILED@, a description of the
-- error.
newTranscriptionJobSummary ::
  TranscriptionJobSummary
newTranscriptionJobSummary =
  TranscriptionJobSummary'
    { languageCode =
        Core.Nothing,
      contentRedaction = Core.Nothing,
      creationTime = Core.Nothing,
      completionTime = Core.Nothing,
      transcriptionJobName = Core.Nothing,
      identifyLanguage = Core.Nothing,
      startTime = Core.Nothing,
      transcriptionJobStatus = Core.Nothing,
      modelSettings = Core.Nothing,
      outputLocationType = Core.Nothing,
      identifiedLanguageScore = Core.Nothing,
      failureReason = Core.Nothing
    }

-- | The language code for the input speech.
transcriptionJobSummary_languageCode :: Lens.Lens' TranscriptionJobSummary (Core.Maybe LanguageCode)
transcriptionJobSummary_languageCode = Lens.lens (\TranscriptionJobSummary' {languageCode} -> languageCode) (\s@TranscriptionJobSummary' {} a -> s {languageCode = a} :: TranscriptionJobSummary)

-- | The content redaction settings of the transcription job.
transcriptionJobSummary_contentRedaction :: Lens.Lens' TranscriptionJobSummary (Core.Maybe ContentRedaction)
transcriptionJobSummary_contentRedaction = Lens.lens (\TranscriptionJobSummary' {contentRedaction} -> contentRedaction) (\s@TranscriptionJobSummary' {} a -> s {contentRedaction = a} :: TranscriptionJobSummary)

-- | A timestamp that shows when the job was created.
transcriptionJobSummary_creationTime :: Lens.Lens' TranscriptionJobSummary (Core.Maybe Core.UTCTime)
transcriptionJobSummary_creationTime = Lens.lens (\TranscriptionJobSummary' {creationTime} -> creationTime) (\s@TranscriptionJobSummary' {} a -> s {creationTime = a} :: TranscriptionJobSummary) Core.. Lens.mapping Core._Time

-- | A timestamp that shows when the job was completed.
transcriptionJobSummary_completionTime :: Lens.Lens' TranscriptionJobSummary (Core.Maybe Core.UTCTime)
transcriptionJobSummary_completionTime = Lens.lens (\TranscriptionJobSummary' {completionTime} -> completionTime) (\s@TranscriptionJobSummary' {} a -> s {completionTime = a} :: TranscriptionJobSummary) Core.. Lens.mapping Core._Time

-- | The name of the transcription job.
transcriptionJobSummary_transcriptionJobName :: Lens.Lens' TranscriptionJobSummary (Core.Maybe Core.Text)
transcriptionJobSummary_transcriptionJobName = Lens.lens (\TranscriptionJobSummary' {transcriptionJobName} -> transcriptionJobName) (\s@TranscriptionJobSummary' {} a -> s {transcriptionJobName = a} :: TranscriptionJobSummary)

-- | Whether automatic language identification was enabled for a
-- transcription job.
transcriptionJobSummary_identifyLanguage :: Lens.Lens' TranscriptionJobSummary (Core.Maybe Core.Bool)
transcriptionJobSummary_identifyLanguage = Lens.lens (\TranscriptionJobSummary' {identifyLanguage} -> identifyLanguage) (\s@TranscriptionJobSummary' {} a -> s {identifyLanguage = a} :: TranscriptionJobSummary)

-- | A timestamp that shows when the job started processing.
transcriptionJobSummary_startTime :: Lens.Lens' TranscriptionJobSummary (Core.Maybe Core.UTCTime)
transcriptionJobSummary_startTime = Lens.lens (\TranscriptionJobSummary' {startTime} -> startTime) (\s@TranscriptionJobSummary' {} a -> s {startTime = a} :: TranscriptionJobSummary) Core.. Lens.mapping Core._Time

-- | The status of the transcription job. When the status is @COMPLETED@, use
-- the @GetTranscriptionJob@ operation to get the results of the
-- transcription.
transcriptionJobSummary_transcriptionJobStatus :: Lens.Lens' TranscriptionJobSummary (Core.Maybe TranscriptionJobStatus)
transcriptionJobSummary_transcriptionJobStatus = Lens.lens (\TranscriptionJobSummary' {transcriptionJobStatus} -> transcriptionJobStatus) (\s@TranscriptionJobSummary' {} a -> s {transcriptionJobStatus = a} :: TranscriptionJobSummary)

-- | Undocumented member.
transcriptionJobSummary_modelSettings :: Lens.Lens' TranscriptionJobSummary (Core.Maybe ModelSettings)
transcriptionJobSummary_modelSettings = Lens.lens (\TranscriptionJobSummary' {modelSettings} -> modelSettings) (\s@TranscriptionJobSummary' {} a -> s {modelSettings = a} :: TranscriptionJobSummary)

-- | Indicates the location of the output of the transcription job.
--
-- If the value is @CUSTOMER_BUCKET@ then the location is the S3 bucket
-- specified in the @outputBucketName@ field when the transcription job was
-- started with the @StartTranscriptionJob@ operation.
--
-- If the value is @SERVICE_BUCKET@ then the output is stored by Amazon
-- Transcribe and can be retrieved using the URI in the
-- @GetTranscriptionJob@ response\'s @TranscriptFileUri@ field.
transcriptionJobSummary_outputLocationType :: Lens.Lens' TranscriptionJobSummary (Core.Maybe OutputLocationType)
transcriptionJobSummary_outputLocationType = Lens.lens (\TranscriptionJobSummary' {outputLocationType} -> outputLocationType) (\s@TranscriptionJobSummary' {} a -> s {outputLocationType = a} :: TranscriptionJobSummary)

-- | A value between zero and one that Amazon Transcribe assigned to the
-- language it identified in the source audio. A higher score indicates
-- that Amazon Transcribe is more confident in the language it identified.
transcriptionJobSummary_identifiedLanguageScore :: Lens.Lens' TranscriptionJobSummary (Core.Maybe Core.Double)
transcriptionJobSummary_identifiedLanguageScore = Lens.lens (\TranscriptionJobSummary' {identifiedLanguageScore} -> identifiedLanguageScore) (\s@TranscriptionJobSummary' {} a -> s {identifiedLanguageScore = a} :: TranscriptionJobSummary)

-- | If the @TranscriptionJobStatus@ field is @FAILED@, a description of the
-- error.
transcriptionJobSummary_failureReason :: Lens.Lens' TranscriptionJobSummary (Core.Maybe Core.Text)
transcriptionJobSummary_failureReason = Lens.lens (\TranscriptionJobSummary' {failureReason} -> failureReason) (\s@TranscriptionJobSummary' {} a -> s {failureReason = a} :: TranscriptionJobSummary)

instance Core.FromJSON TranscriptionJobSummary where
  parseJSON =
    Core.withObject
      "TranscriptionJobSummary"
      ( \x ->
          TranscriptionJobSummary'
            Core.<$> (x Core..:? "LanguageCode")
            Core.<*> (x Core..:? "ContentRedaction")
            Core.<*> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "CompletionTime")
            Core.<*> (x Core..:? "TranscriptionJobName")
            Core.<*> (x Core..:? "IdentifyLanguage")
            Core.<*> (x Core..:? "StartTime")
            Core.<*> (x Core..:? "TranscriptionJobStatus")
            Core.<*> (x Core..:? "ModelSettings")
            Core.<*> (x Core..:? "OutputLocationType")
            Core.<*> (x Core..:? "IdentifiedLanguageScore")
            Core.<*> (x Core..:? "FailureReason")
      )

instance Core.Hashable TranscriptionJobSummary

instance Core.NFData TranscriptionJobSummary
