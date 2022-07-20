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
-- Module      : Amazonka.Transcribe.Types.TranscriptionJobSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.TranscriptionJobSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transcribe.Types.ContentRedaction
import Amazonka.Transcribe.Types.LanguageCode
import Amazonka.Transcribe.Types.ModelSettings
import Amazonka.Transcribe.Types.OutputLocationType
import Amazonka.Transcribe.Types.TranscriptionJobStatus

-- | Provides a summary of information about a transcription job.
--
-- /See:/ 'newTranscriptionJobSummary' smart constructor.
data TranscriptionJobSummary = TranscriptionJobSummary'
  { -- | Whether automatic language identification was enabled for a
    -- transcription job.
    identifyLanguage :: Prelude.Maybe Prelude.Bool,
    -- | The content redaction settings of the transcription job.
    contentRedaction :: Prelude.Maybe ContentRedaction,
    -- | The name of the transcription job.
    transcriptionJobName :: Prelude.Maybe Prelude.Text,
    -- | A timestamp that shows when the job was completed.
    completionTime :: Prelude.Maybe Core.POSIX,
    -- | Indicates the location of the output of the transcription job.
    --
    -- If the value is @CUSTOMER_BUCKET@ then the location is the S3 bucket
    -- specified in the @outputBucketName@ field when the transcription job was
    -- started with the @StartTranscriptionJob@ operation.
    --
    -- If the value is @SERVICE_BUCKET@ then the output is stored by Amazon
    -- Transcribe and can be retrieved using the URI in the
    -- @GetTranscriptionJob@ response\'s @TranscriptFileUri@ field.
    outputLocationType :: Prelude.Maybe OutputLocationType,
    -- | The language code for the input speech.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | The status of the transcription job. When the status is @COMPLETED@, use
    -- the @GetTranscriptionJob@ operation to get the results of the
    -- transcription.
    transcriptionJobStatus :: Prelude.Maybe TranscriptionJobStatus,
    -- | A timestamp that shows when the job was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    modelSettings :: Prelude.Maybe ModelSettings,
    -- | A value between zero and one that Amazon Transcribe assigned to the
    -- language it identified in the source audio. A higher score indicates
    -- that Amazon Transcribe is more confident in the language it identified.
    identifiedLanguageScore :: Prelude.Maybe Prelude.Double,
    -- | A timestamp that shows when the job started processing.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | If the @TranscriptionJobStatus@ field is @FAILED@, a description of the
    -- error.
    failureReason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TranscriptionJobSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identifyLanguage', 'transcriptionJobSummary_identifyLanguage' - Whether automatic language identification was enabled for a
-- transcription job.
--
-- 'contentRedaction', 'transcriptionJobSummary_contentRedaction' - The content redaction settings of the transcription job.
--
-- 'transcriptionJobName', 'transcriptionJobSummary_transcriptionJobName' - The name of the transcription job.
--
-- 'completionTime', 'transcriptionJobSummary_completionTime' - A timestamp that shows when the job was completed.
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
-- 'languageCode', 'transcriptionJobSummary_languageCode' - The language code for the input speech.
--
-- 'transcriptionJobStatus', 'transcriptionJobSummary_transcriptionJobStatus' - The status of the transcription job. When the status is @COMPLETED@, use
-- the @GetTranscriptionJob@ operation to get the results of the
-- transcription.
--
-- 'creationTime', 'transcriptionJobSummary_creationTime' - A timestamp that shows when the job was created.
--
-- 'modelSettings', 'transcriptionJobSummary_modelSettings' - Undocumented member.
--
-- 'identifiedLanguageScore', 'transcriptionJobSummary_identifiedLanguageScore' - A value between zero and one that Amazon Transcribe assigned to the
-- language it identified in the source audio. A higher score indicates
-- that Amazon Transcribe is more confident in the language it identified.
--
-- 'startTime', 'transcriptionJobSummary_startTime' - A timestamp that shows when the job started processing.
--
-- 'failureReason', 'transcriptionJobSummary_failureReason' - If the @TranscriptionJobStatus@ field is @FAILED@, a description of the
-- error.
newTranscriptionJobSummary ::
  TranscriptionJobSummary
newTranscriptionJobSummary =
  TranscriptionJobSummary'
    { identifyLanguage =
        Prelude.Nothing,
      contentRedaction = Prelude.Nothing,
      transcriptionJobName = Prelude.Nothing,
      completionTime = Prelude.Nothing,
      outputLocationType = Prelude.Nothing,
      languageCode = Prelude.Nothing,
      transcriptionJobStatus = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      modelSettings = Prelude.Nothing,
      identifiedLanguageScore = Prelude.Nothing,
      startTime = Prelude.Nothing,
      failureReason = Prelude.Nothing
    }

-- | Whether automatic language identification was enabled for a
-- transcription job.
transcriptionJobSummary_identifyLanguage :: Lens.Lens' TranscriptionJobSummary (Prelude.Maybe Prelude.Bool)
transcriptionJobSummary_identifyLanguage = Lens.lens (\TranscriptionJobSummary' {identifyLanguage} -> identifyLanguage) (\s@TranscriptionJobSummary' {} a -> s {identifyLanguage = a} :: TranscriptionJobSummary)

-- | The content redaction settings of the transcription job.
transcriptionJobSummary_contentRedaction :: Lens.Lens' TranscriptionJobSummary (Prelude.Maybe ContentRedaction)
transcriptionJobSummary_contentRedaction = Lens.lens (\TranscriptionJobSummary' {contentRedaction} -> contentRedaction) (\s@TranscriptionJobSummary' {} a -> s {contentRedaction = a} :: TranscriptionJobSummary)

-- | The name of the transcription job.
transcriptionJobSummary_transcriptionJobName :: Lens.Lens' TranscriptionJobSummary (Prelude.Maybe Prelude.Text)
transcriptionJobSummary_transcriptionJobName = Lens.lens (\TranscriptionJobSummary' {transcriptionJobName} -> transcriptionJobName) (\s@TranscriptionJobSummary' {} a -> s {transcriptionJobName = a} :: TranscriptionJobSummary)

-- | A timestamp that shows when the job was completed.
transcriptionJobSummary_completionTime :: Lens.Lens' TranscriptionJobSummary (Prelude.Maybe Prelude.UTCTime)
transcriptionJobSummary_completionTime = Lens.lens (\TranscriptionJobSummary' {completionTime} -> completionTime) (\s@TranscriptionJobSummary' {} a -> s {completionTime = a} :: TranscriptionJobSummary) Prelude.. Lens.mapping Core._Time

-- | Indicates the location of the output of the transcription job.
--
-- If the value is @CUSTOMER_BUCKET@ then the location is the S3 bucket
-- specified in the @outputBucketName@ field when the transcription job was
-- started with the @StartTranscriptionJob@ operation.
--
-- If the value is @SERVICE_BUCKET@ then the output is stored by Amazon
-- Transcribe and can be retrieved using the URI in the
-- @GetTranscriptionJob@ response\'s @TranscriptFileUri@ field.
transcriptionJobSummary_outputLocationType :: Lens.Lens' TranscriptionJobSummary (Prelude.Maybe OutputLocationType)
transcriptionJobSummary_outputLocationType = Lens.lens (\TranscriptionJobSummary' {outputLocationType} -> outputLocationType) (\s@TranscriptionJobSummary' {} a -> s {outputLocationType = a} :: TranscriptionJobSummary)

-- | The language code for the input speech.
transcriptionJobSummary_languageCode :: Lens.Lens' TranscriptionJobSummary (Prelude.Maybe LanguageCode)
transcriptionJobSummary_languageCode = Lens.lens (\TranscriptionJobSummary' {languageCode} -> languageCode) (\s@TranscriptionJobSummary' {} a -> s {languageCode = a} :: TranscriptionJobSummary)

-- | The status of the transcription job. When the status is @COMPLETED@, use
-- the @GetTranscriptionJob@ operation to get the results of the
-- transcription.
transcriptionJobSummary_transcriptionJobStatus :: Lens.Lens' TranscriptionJobSummary (Prelude.Maybe TranscriptionJobStatus)
transcriptionJobSummary_transcriptionJobStatus = Lens.lens (\TranscriptionJobSummary' {transcriptionJobStatus} -> transcriptionJobStatus) (\s@TranscriptionJobSummary' {} a -> s {transcriptionJobStatus = a} :: TranscriptionJobSummary)

-- | A timestamp that shows when the job was created.
transcriptionJobSummary_creationTime :: Lens.Lens' TranscriptionJobSummary (Prelude.Maybe Prelude.UTCTime)
transcriptionJobSummary_creationTime = Lens.lens (\TranscriptionJobSummary' {creationTime} -> creationTime) (\s@TranscriptionJobSummary' {} a -> s {creationTime = a} :: TranscriptionJobSummary) Prelude.. Lens.mapping Core._Time

-- | Undocumented member.
transcriptionJobSummary_modelSettings :: Lens.Lens' TranscriptionJobSummary (Prelude.Maybe ModelSettings)
transcriptionJobSummary_modelSettings = Lens.lens (\TranscriptionJobSummary' {modelSettings} -> modelSettings) (\s@TranscriptionJobSummary' {} a -> s {modelSettings = a} :: TranscriptionJobSummary)

-- | A value between zero and one that Amazon Transcribe assigned to the
-- language it identified in the source audio. A higher score indicates
-- that Amazon Transcribe is more confident in the language it identified.
transcriptionJobSummary_identifiedLanguageScore :: Lens.Lens' TranscriptionJobSummary (Prelude.Maybe Prelude.Double)
transcriptionJobSummary_identifiedLanguageScore = Lens.lens (\TranscriptionJobSummary' {identifiedLanguageScore} -> identifiedLanguageScore) (\s@TranscriptionJobSummary' {} a -> s {identifiedLanguageScore = a} :: TranscriptionJobSummary)

-- | A timestamp that shows when the job started processing.
transcriptionJobSummary_startTime :: Lens.Lens' TranscriptionJobSummary (Prelude.Maybe Prelude.UTCTime)
transcriptionJobSummary_startTime = Lens.lens (\TranscriptionJobSummary' {startTime} -> startTime) (\s@TranscriptionJobSummary' {} a -> s {startTime = a} :: TranscriptionJobSummary) Prelude.. Lens.mapping Core._Time

-- | If the @TranscriptionJobStatus@ field is @FAILED@, a description of the
-- error.
transcriptionJobSummary_failureReason :: Lens.Lens' TranscriptionJobSummary (Prelude.Maybe Prelude.Text)
transcriptionJobSummary_failureReason = Lens.lens (\TranscriptionJobSummary' {failureReason} -> failureReason) (\s@TranscriptionJobSummary' {} a -> s {failureReason = a} :: TranscriptionJobSummary)

instance Core.FromJSON TranscriptionJobSummary where
  parseJSON =
    Core.withObject
      "TranscriptionJobSummary"
      ( \x ->
          TranscriptionJobSummary'
            Prelude.<$> (x Core..:? "IdentifyLanguage")
            Prelude.<*> (x Core..:? "ContentRedaction")
            Prelude.<*> (x Core..:? "TranscriptionJobName")
            Prelude.<*> (x Core..:? "CompletionTime")
            Prelude.<*> (x Core..:? "OutputLocationType")
            Prelude.<*> (x Core..:? "LanguageCode")
            Prelude.<*> (x Core..:? "TranscriptionJobStatus")
            Prelude.<*> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "ModelSettings")
            Prelude.<*> (x Core..:? "IdentifiedLanguageScore")
            Prelude.<*> (x Core..:? "StartTime")
            Prelude.<*> (x Core..:? "FailureReason")
      )

instance Prelude.Hashable TranscriptionJobSummary where
  hashWithSalt _salt TranscriptionJobSummary' {..} =
    _salt `Prelude.hashWithSalt` identifyLanguage
      `Prelude.hashWithSalt` contentRedaction
      `Prelude.hashWithSalt` transcriptionJobName
      `Prelude.hashWithSalt` completionTime
      `Prelude.hashWithSalt` outputLocationType
      `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` transcriptionJobStatus
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` modelSettings
      `Prelude.hashWithSalt` identifiedLanguageScore
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` failureReason

instance Prelude.NFData TranscriptionJobSummary where
  rnf TranscriptionJobSummary' {..} =
    Prelude.rnf identifyLanguage
      `Prelude.seq` Prelude.rnf contentRedaction
      `Prelude.seq` Prelude.rnf transcriptionJobName
      `Prelude.seq` Prelude.rnf completionTime
      `Prelude.seq` Prelude.rnf outputLocationType
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf transcriptionJobStatus
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf modelSettings
      `Prelude.seq` Prelude.rnf identifiedLanguageScore
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf failureReason
