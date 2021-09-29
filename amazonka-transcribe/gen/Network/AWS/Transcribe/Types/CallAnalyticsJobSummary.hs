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
-- Module      : Network.AWS.Transcribe.Types.CallAnalyticsJobSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.CallAnalyticsJobSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Transcribe.Types.CallAnalyticsJobStatus
import Network.AWS.Transcribe.Types.LanguageCode

-- | Provides summary information about a call analytics job.
--
-- /See:/ 'newCallAnalyticsJobSummary' smart constructor.
data CallAnalyticsJobSummary = CallAnalyticsJobSummary'
  { -- | The language of the transcript in the source audio file.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | The status of the call analytics job.
    callAnalyticsJobStatus :: Prelude.Maybe CallAnalyticsJobStatus,
    -- | A timestamp that shows when the call analytics job was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | A timestamp that shows when the job was completed.
    completionTime :: Prelude.Maybe Core.POSIX,
    -- | A timestamp that shows when the job began processing.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the call analytics job.
    callAnalyticsJobName :: Prelude.Maybe Prelude.Text,
    -- | If the @CallAnalyticsJobStatus@ is @FAILED@, a description of the error.
    failureReason :: Prelude.Maybe Prelude.Text
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
-- 'languageCode', 'callAnalyticsJobSummary_languageCode' - The language of the transcript in the source audio file.
--
-- 'callAnalyticsJobStatus', 'callAnalyticsJobSummary_callAnalyticsJobStatus' - The status of the call analytics job.
--
-- 'creationTime', 'callAnalyticsJobSummary_creationTime' - A timestamp that shows when the call analytics job was created.
--
-- 'completionTime', 'callAnalyticsJobSummary_completionTime' - A timestamp that shows when the job was completed.
--
-- 'startTime', 'callAnalyticsJobSummary_startTime' - A timestamp that shows when the job began processing.
--
-- 'callAnalyticsJobName', 'callAnalyticsJobSummary_callAnalyticsJobName' - The name of the call analytics job.
--
-- 'failureReason', 'callAnalyticsJobSummary_failureReason' - If the @CallAnalyticsJobStatus@ is @FAILED@, a description of the error.
newCallAnalyticsJobSummary ::
  CallAnalyticsJobSummary
newCallAnalyticsJobSummary =
  CallAnalyticsJobSummary'
    { languageCode =
        Prelude.Nothing,
      callAnalyticsJobStatus = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      completionTime = Prelude.Nothing,
      startTime = Prelude.Nothing,
      callAnalyticsJobName = Prelude.Nothing,
      failureReason = Prelude.Nothing
    }

-- | The language of the transcript in the source audio file.
callAnalyticsJobSummary_languageCode :: Lens.Lens' CallAnalyticsJobSummary (Prelude.Maybe LanguageCode)
callAnalyticsJobSummary_languageCode = Lens.lens (\CallAnalyticsJobSummary' {languageCode} -> languageCode) (\s@CallAnalyticsJobSummary' {} a -> s {languageCode = a} :: CallAnalyticsJobSummary)

-- | The status of the call analytics job.
callAnalyticsJobSummary_callAnalyticsJobStatus :: Lens.Lens' CallAnalyticsJobSummary (Prelude.Maybe CallAnalyticsJobStatus)
callAnalyticsJobSummary_callAnalyticsJobStatus = Lens.lens (\CallAnalyticsJobSummary' {callAnalyticsJobStatus} -> callAnalyticsJobStatus) (\s@CallAnalyticsJobSummary' {} a -> s {callAnalyticsJobStatus = a} :: CallAnalyticsJobSummary)

-- | A timestamp that shows when the call analytics job was created.
callAnalyticsJobSummary_creationTime :: Lens.Lens' CallAnalyticsJobSummary (Prelude.Maybe Prelude.UTCTime)
callAnalyticsJobSummary_creationTime = Lens.lens (\CallAnalyticsJobSummary' {creationTime} -> creationTime) (\s@CallAnalyticsJobSummary' {} a -> s {creationTime = a} :: CallAnalyticsJobSummary) Prelude.. Lens.mapping Core._Time

-- | A timestamp that shows when the job was completed.
callAnalyticsJobSummary_completionTime :: Lens.Lens' CallAnalyticsJobSummary (Prelude.Maybe Prelude.UTCTime)
callAnalyticsJobSummary_completionTime = Lens.lens (\CallAnalyticsJobSummary' {completionTime} -> completionTime) (\s@CallAnalyticsJobSummary' {} a -> s {completionTime = a} :: CallAnalyticsJobSummary) Prelude.. Lens.mapping Core._Time

-- | A timestamp that shows when the job began processing.
callAnalyticsJobSummary_startTime :: Lens.Lens' CallAnalyticsJobSummary (Prelude.Maybe Prelude.UTCTime)
callAnalyticsJobSummary_startTime = Lens.lens (\CallAnalyticsJobSummary' {startTime} -> startTime) (\s@CallAnalyticsJobSummary' {} a -> s {startTime = a} :: CallAnalyticsJobSummary) Prelude.. Lens.mapping Core._Time

-- | The name of the call analytics job.
callAnalyticsJobSummary_callAnalyticsJobName :: Lens.Lens' CallAnalyticsJobSummary (Prelude.Maybe Prelude.Text)
callAnalyticsJobSummary_callAnalyticsJobName = Lens.lens (\CallAnalyticsJobSummary' {callAnalyticsJobName} -> callAnalyticsJobName) (\s@CallAnalyticsJobSummary' {} a -> s {callAnalyticsJobName = a} :: CallAnalyticsJobSummary)

-- | If the @CallAnalyticsJobStatus@ is @FAILED@, a description of the error.
callAnalyticsJobSummary_failureReason :: Lens.Lens' CallAnalyticsJobSummary (Prelude.Maybe Prelude.Text)
callAnalyticsJobSummary_failureReason = Lens.lens (\CallAnalyticsJobSummary' {failureReason} -> failureReason) (\s@CallAnalyticsJobSummary' {} a -> s {failureReason = a} :: CallAnalyticsJobSummary)

instance Core.FromJSON CallAnalyticsJobSummary where
  parseJSON =
    Core.withObject
      "CallAnalyticsJobSummary"
      ( \x ->
          CallAnalyticsJobSummary'
            Prelude.<$> (x Core..:? "LanguageCode")
            Prelude.<*> (x Core..:? "CallAnalyticsJobStatus")
            Prelude.<*> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "CompletionTime")
            Prelude.<*> (x Core..:? "StartTime")
            Prelude.<*> (x Core..:? "CallAnalyticsJobName")
            Prelude.<*> (x Core..:? "FailureReason")
      )

instance Prelude.Hashable CallAnalyticsJobSummary

instance Prelude.NFData CallAnalyticsJobSummary
