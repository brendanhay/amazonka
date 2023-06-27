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
-- Module      : Amazonka.Transcribe.Types.CallAnalyticsJobSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.CallAnalyticsJobSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transcribe.Types.CallAnalyticsJobStatus
import Amazonka.Transcribe.Types.LanguageCode

-- | Provides detailed information about a specific Call Analytics job.
--
-- /See:/ 'newCallAnalyticsJobSummary' smart constructor.
data CallAnalyticsJobSummary = CallAnalyticsJobSummary'
  { -- | The name of the Call Analytics job. Job names are case sensitive and
    -- must be unique within an Amazon Web Services account.
    callAnalyticsJobName :: Prelude.Maybe Prelude.Text,
    -- | Provides the status of your Call Analytics job.
    --
    -- If the status is @COMPLETED@, the job is finished and you can find the
    -- results at the location specified in @TranscriptFileUri@ (or
    -- @RedactedTranscriptFileUri@, if you requested transcript redaction). If
    -- the status is @FAILED@, @FailureReason@ provides details on why your
    -- transcription job failed.
    callAnalyticsJobStatus :: Prelude.Maybe CallAnalyticsJobStatus,
    -- | The date and time the specified Call Analytics job finished processing.
    --
    -- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
    -- example, @2022-05-04T12:33:13.922000-07:00@ represents a transcription
    -- job that started processing at 12:33 PM UTC-7 on May 4, 2022.
    completionTime :: Prelude.Maybe Data.POSIX,
    -- | The date and time the specified Call Analytics job request was made.
    --
    -- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
    -- example, @2022-05-04T12:32:58.761000-07:00@ represents a transcription
    -- job that started processing at 12:32 PM UTC-7 on May 4, 2022.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | If @CallAnalyticsJobStatus@ is @FAILED@, @FailureReason@ contains
    -- information about why the Call Analytics job failed. See also:
    -- <https://docs.aws.amazon.com/transcribe/latest/APIReference/CommonErrors.html Common Errors>.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The language code used to create your Call Analytics transcription.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | The date and time your Call Analytics job began processing.
    --
    -- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
    -- example, @2022-05-04T12:32:58.789000-07:00@ represents a transcription
    -- job that started processing at 12:32 PM UTC-7 on May 4, 2022.
    startTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CallAnalyticsJobSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'callAnalyticsJobName', 'callAnalyticsJobSummary_callAnalyticsJobName' - The name of the Call Analytics job. Job names are case sensitive and
-- must be unique within an Amazon Web Services account.
--
-- 'callAnalyticsJobStatus', 'callAnalyticsJobSummary_callAnalyticsJobStatus' - Provides the status of your Call Analytics job.
--
-- If the status is @COMPLETED@, the job is finished and you can find the
-- results at the location specified in @TranscriptFileUri@ (or
-- @RedactedTranscriptFileUri@, if you requested transcript redaction). If
-- the status is @FAILED@, @FailureReason@ provides details on why your
-- transcription job failed.
--
-- 'completionTime', 'callAnalyticsJobSummary_completionTime' - The date and time the specified Call Analytics job finished processing.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:33:13.922000-07:00@ represents a transcription
-- job that started processing at 12:33 PM UTC-7 on May 4, 2022.
--
-- 'creationTime', 'callAnalyticsJobSummary_creationTime' - The date and time the specified Call Analytics job request was made.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.761000-07:00@ represents a transcription
-- job that started processing at 12:32 PM UTC-7 on May 4, 2022.
--
-- 'failureReason', 'callAnalyticsJobSummary_failureReason' - If @CallAnalyticsJobStatus@ is @FAILED@, @FailureReason@ contains
-- information about why the Call Analytics job failed. See also:
-- <https://docs.aws.amazon.com/transcribe/latest/APIReference/CommonErrors.html Common Errors>.
--
-- 'languageCode', 'callAnalyticsJobSummary_languageCode' - The language code used to create your Call Analytics transcription.
--
-- 'startTime', 'callAnalyticsJobSummary_startTime' - The date and time your Call Analytics job began processing.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.789000-07:00@ represents a transcription
-- job that started processing at 12:32 PM UTC-7 on May 4, 2022.
newCallAnalyticsJobSummary ::
  CallAnalyticsJobSummary
newCallAnalyticsJobSummary =
  CallAnalyticsJobSummary'
    { callAnalyticsJobName =
        Prelude.Nothing,
      callAnalyticsJobStatus = Prelude.Nothing,
      completionTime = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      languageCode = Prelude.Nothing,
      startTime = Prelude.Nothing
    }

-- | The name of the Call Analytics job. Job names are case sensitive and
-- must be unique within an Amazon Web Services account.
callAnalyticsJobSummary_callAnalyticsJobName :: Lens.Lens' CallAnalyticsJobSummary (Prelude.Maybe Prelude.Text)
callAnalyticsJobSummary_callAnalyticsJobName = Lens.lens (\CallAnalyticsJobSummary' {callAnalyticsJobName} -> callAnalyticsJobName) (\s@CallAnalyticsJobSummary' {} a -> s {callAnalyticsJobName = a} :: CallAnalyticsJobSummary)

-- | Provides the status of your Call Analytics job.
--
-- If the status is @COMPLETED@, the job is finished and you can find the
-- results at the location specified in @TranscriptFileUri@ (or
-- @RedactedTranscriptFileUri@, if you requested transcript redaction). If
-- the status is @FAILED@, @FailureReason@ provides details on why your
-- transcription job failed.
callAnalyticsJobSummary_callAnalyticsJobStatus :: Lens.Lens' CallAnalyticsJobSummary (Prelude.Maybe CallAnalyticsJobStatus)
callAnalyticsJobSummary_callAnalyticsJobStatus = Lens.lens (\CallAnalyticsJobSummary' {callAnalyticsJobStatus} -> callAnalyticsJobStatus) (\s@CallAnalyticsJobSummary' {} a -> s {callAnalyticsJobStatus = a} :: CallAnalyticsJobSummary)

-- | The date and time the specified Call Analytics job finished processing.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:33:13.922000-07:00@ represents a transcription
-- job that started processing at 12:33 PM UTC-7 on May 4, 2022.
callAnalyticsJobSummary_completionTime :: Lens.Lens' CallAnalyticsJobSummary (Prelude.Maybe Prelude.UTCTime)
callAnalyticsJobSummary_completionTime = Lens.lens (\CallAnalyticsJobSummary' {completionTime} -> completionTime) (\s@CallAnalyticsJobSummary' {} a -> s {completionTime = a} :: CallAnalyticsJobSummary) Prelude.. Lens.mapping Data._Time

-- | The date and time the specified Call Analytics job request was made.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.761000-07:00@ represents a transcription
-- job that started processing at 12:32 PM UTC-7 on May 4, 2022.
callAnalyticsJobSummary_creationTime :: Lens.Lens' CallAnalyticsJobSummary (Prelude.Maybe Prelude.UTCTime)
callAnalyticsJobSummary_creationTime = Lens.lens (\CallAnalyticsJobSummary' {creationTime} -> creationTime) (\s@CallAnalyticsJobSummary' {} a -> s {creationTime = a} :: CallAnalyticsJobSummary) Prelude.. Lens.mapping Data._Time

-- | If @CallAnalyticsJobStatus@ is @FAILED@, @FailureReason@ contains
-- information about why the Call Analytics job failed. See also:
-- <https://docs.aws.amazon.com/transcribe/latest/APIReference/CommonErrors.html Common Errors>.
callAnalyticsJobSummary_failureReason :: Lens.Lens' CallAnalyticsJobSummary (Prelude.Maybe Prelude.Text)
callAnalyticsJobSummary_failureReason = Lens.lens (\CallAnalyticsJobSummary' {failureReason} -> failureReason) (\s@CallAnalyticsJobSummary' {} a -> s {failureReason = a} :: CallAnalyticsJobSummary)

-- | The language code used to create your Call Analytics transcription.
callAnalyticsJobSummary_languageCode :: Lens.Lens' CallAnalyticsJobSummary (Prelude.Maybe LanguageCode)
callAnalyticsJobSummary_languageCode = Lens.lens (\CallAnalyticsJobSummary' {languageCode} -> languageCode) (\s@CallAnalyticsJobSummary' {} a -> s {languageCode = a} :: CallAnalyticsJobSummary)

-- | The date and time your Call Analytics job began processing.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.789000-07:00@ represents a transcription
-- job that started processing at 12:32 PM UTC-7 on May 4, 2022.
callAnalyticsJobSummary_startTime :: Lens.Lens' CallAnalyticsJobSummary (Prelude.Maybe Prelude.UTCTime)
callAnalyticsJobSummary_startTime = Lens.lens (\CallAnalyticsJobSummary' {startTime} -> startTime) (\s@CallAnalyticsJobSummary' {} a -> s {startTime = a} :: CallAnalyticsJobSummary) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON CallAnalyticsJobSummary where
  parseJSON =
    Data.withObject
      "CallAnalyticsJobSummary"
      ( \x ->
          CallAnalyticsJobSummary'
            Prelude.<$> (x Data..:? "CallAnalyticsJobName")
            Prelude.<*> (x Data..:? "CallAnalyticsJobStatus")
            Prelude.<*> (x Data..:? "CompletionTime")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "FailureReason")
            Prelude.<*> (x Data..:? "LanguageCode")
            Prelude.<*> (x Data..:? "StartTime")
      )

instance Prelude.Hashable CallAnalyticsJobSummary where
  hashWithSalt _salt CallAnalyticsJobSummary' {..} =
    _salt
      `Prelude.hashWithSalt` callAnalyticsJobName
      `Prelude.hashWithSalt` callAnalyticsJobStatus
      `Prelude.hashWithSalt` completionTime
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData CallAnalyticsJobSummary where
  rnf CallAnalyticsJobSummary' {..} =
    Prelude.rnf callAnalyticsJobName
      `Prelude.seq` Prelude.rnf callAnalyticsJobStatus
      `Prelude.seq` Prelude.rnf completionTime
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf startTime
