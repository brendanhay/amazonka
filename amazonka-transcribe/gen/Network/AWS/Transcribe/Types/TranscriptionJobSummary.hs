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
-- Module      : Network.AWS.Transcribe.Types.TranscriptionJobSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.TranscriptionJobSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    languageCode :: Prelude.Maybe LanguageCode,
    -- | The content redaction settings of the transcription job.
    contentRedaction :: Prelude.Maybe ContentRedaction,
    -- | A timestamp that shows when the job was created.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | A timestamp that shows when the job was completed.
    completionTime :: Prelude.Maybe Prelude.POSIX,
    -- | The name of the transcription job.
    transcriptionJobName :: Prelude.Maybe Prelude.Text,
    -- | Whether automatic language identification was enabled for a
    -- transcription job.
    identifyLanguage :: Prelude.Maybe Prelude.Bool,
    -- | A timestamp that shows when the job started processing.
    startTime :: Prelude.Maybe Prelude.POSIX,
    -- | The status of the transcription job. When the status is @COMPLETED@, use
    -- the @GetTranscriptionJob@ operation to get the results of the
    -- transcription.
    transcriptionJobStatus :: Prelude.Maybe TranscriptionJobStatus,
    modelSettings :: Prelude.Maybe ModelSettings,
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
    -- | A value between zero and one that Amazon Transcribe assigned to the
    -- language it identified in the source audio. A higher score indicates
    -- that Amazon Transcribe is more confident in the language it identified.
    identifiedLanguageScore :: Prelude.Maybe Prelude.Double,
    -- | If the @TranscriptionJobStatus@ field is @FAILED@, a description of the
    -- error.
    failureReason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      contentRedaction = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      completionTime = Prelude.Nothing,
      transcriptionJobName = Prelude.Nothing,
      identifyLanguage = Prelude.Nothing,
      startTime = Prelude.Nothing,
      transcriptionJobStatus = Prelude.Nothing,
      modelSettings = Prelude.Nothing,
      outputLocationType = Prelude.Nothing,
      identifiedLanguageScore = Prelude.Nothing,
      failureReason = Prelude.Nothing
    }

-- | The language code for the input speech.
transcriptionJobSummary_languageCode :: Lens.Lens' TranscriptionJobSummary (Prelude.Maybe LanguageCode)
transcriptionJobSummary_languageCode = Lens.lens (\TranscriptionJobSummary' {languageCode} -> languageCode) (\s@TranscriptionJobSummary' {} a -> s {languageCode = a} :: TranscriptionJobSummary)

-- | The content redaction settings of the transcription job.
transcriptionJobSummary_contentRedaction :: Lens.Lens' TranscriptionJobSummary (Prelude.Maybe ContentRedaction)
transcriptionJobSummary_contentRedaction = Lens.lens (\TranscriptionJobSummary' {contentRedaction} -> contentRedaction) (\s@TranscriptionJobSummary' {} a -> s {contentRedaction = a} :: TranscriptionJobSummary)

-- | A timestamp that shows when the job was created.
transcriptionJobSummary_creationTime :: Lens.Lens' TranscriptionJobSummary (Prelude.Maybe Prelude.UTCTime)
transcriptionJobSummary_creationTime = Lens.lens (\TranscriptionJobSummary' {creationTime} -> creationTime) (\s@TranscriptionJobSummary' {} a -> s {creationTime = a} :: TranscriptionJobSummary) Prelude.. Lens.mapping Prelude._Time

-- | A timestamp that shows when the job was completed.
transcriptionJobSummary_completionTime :: Lens.Lens' TranscriptionJobSummary (Prelude.Maybe Prelude.UTCTime)
transcriptionJobSummary_completionTime = Lens.lens (\TranscriptionJobSummary' {completionTime} -> completionTime) (\s@TranscriptionJobSummary' {} a -> s {completionTime = a} :: TranscriptionJobSummary) Prelude.. Lens.mapping Prelude._Time

-- | The name of the transcription job.
transcriptionJobSummary_transcriptionJobName :: Lens.Lens' TranscriptionJobSummary (Prelude.Maybe Prelude.Text)
transcriptionJobSummary_transcriptionJobName = Lens.lens (\TranscriptionJobSummary' {transcriptionJobName} -> transcriptionJobName) (\s@TranscriptionJobSummary' {} a -> s {transcriptionJobName = a} :: TranscriptionJobSummary)

-- | Whether automatic language identification was enabled for a
-- transcription job.
transcriptionJobSummary_identifyLanguage :: Lens.Lens' TranscriptionJobSummary (Prelude.Maybe Prelude.Bool)
transcriptionJobSummary_identifyLanguage = Lens.lens (\TranscriptionJobSummary' {identifyLanguage} -> identifyLanguage) (\s@TranscriptionJobSummary' {} a -> s {identifyLanguage = a} :: TranscriptionJobSummary)

-- | A timestamp that shows when the job started processing.
transcriptionJobSummary_startTime :: Lens.Lens' TranscriptionJobSummary (Prelude.Maybe Prelude.UTCTime)
transcriptionJobSummary_startTime = Lens.lens (\TranscriptionJobSummary' {startTime} -> startTime) (\s@TranscriptionJobSummary' {} a -> s {startTime = a} :: TranscriptionJobSummary) Prelude.. Lens.mapping Prelude._Time

-- | The status of the transcription job. When the status is @COMPLETED@, use
-- the @GetTranscriptionJob@ operation to get the results of the
-- transcription.
transcriptionJobSummary_transcriptionJobStatus :: Lens.Lens' TranscriptionJobSummary (Prelude.Maybe TranscriptionJobStatus)
transcriptionJobSummary_transcriptionJobStatus = Lens.lens (\TranscriptionJobSummary' {transcriptionJobStatus} -> transcriptionJobStatus) (\s@TranscriptionJobSummary' {} a -> s {transcriptionJobStatus = a} :: TranscriptionJobSummary)

-- | Undocumented member.
transcriptionJobSummary_modelSettings :: Lens.Lens' TranscriptionJobSummary (Prelude.Maybe ModelSettings)
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
transcriptionJobSummary_outputLocationType :: Lens.Lens' TranscriptionJobSummary (Prelude.Maybe OutputLocationType)
transcriptionJobSummary_outputLocationType = Lens.lens (\TranscriptionJobSummary' {outputLocationType} -> outputLocationType) (\s@TranscriptionJobSummary' {} a -> s {outputLocationType = a} :: TranscriptionJobSummary)

-- | A value between zero and one that Amazon Transcribe assigned to the
-- language it identified in the source audio. A higher score indicates
-- that Amazon Transcribe is more confident in the language it identified.
transcriptionJobSummary_identifiedLanguageScore :: Lens.Lens' TranscriptionJobSummary (Prelude.Maybe Prelude.Double)
transcriptionJobSummary_identifiedLanguageScore = Lens.lens (\TranscriptionJobSummary' {identifiedLanguageScore} -> identifiedLanguageScore) (\s@TranscriptionJobSummary' {} a -> s {identifiedLanguageScore = a} :: TranscriptionJobSummary)

-- | If the @TranscriptionJobStatus@ field is @FAILED@, a description of the
-- error.
transcriptionJobSummary_failureReason :: Lens.Lens' TranscriptionJobSummary (Prelude.Maybe Prelude.Text)
transcriptionJobSummary_failureReason = Lens.lens (\TranscriptionJobSummary' {failureReason} -> failureReason) (\s@TranscriptionJobSummary' {} a -> s {failureReason = a} :: TranscriptionJobSummary)

instance Prelude.FromJSON TranscriptionJobSummary where
  parseJSON =
    Prelude.withObject
      "TranscriptionJobSummary"
      ( \x ->
          TranscriptionJobSummary'
            Prelude.<$> (x Prelude..:? "LanguageCode")
            Prelude.<*> (x Prelude..:? "ContentRedaction")
            Prelude.<*> (x Prelude..:? "CreationTime")
            Prelude.<*> (x Prelude..:? "CompletionTime")
            Prelude.<*> (x Prelude..:? "TranscriptionJobName")
            Prelude.<*> (x Prelude..:? "IdentifyLanguage")
            Prelude.<*> (x Prelude..:? "StartTime")
            Prelude.<*> (x Prelude..:? "TranscriptionJobStatus")
            Prelude.<*> (x Prelude..:? "ModelSettings")
            Prelude.<*> (x Prelude..:? "OutputLocationType")
            Prelude.<*> (x Prelude..:? "IdentifiedLanguageScore")
            Prelude.<*> (x Prelude..:? "FailureReason")
      )

instance Prelude.Hashable TranscriptionJobSummary

instance Prelude.NFData TranscriptionJobSummary
