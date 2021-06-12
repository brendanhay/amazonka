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
-- Module      : Network.AWS.Transcribe.Types.Transcript
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.Transcript where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Identifies the location of a transcription.
--
-- /See:/ 'newTranscript' smart constructor.
data Transcript = Transcript'
  { -- | The S3 object location of the transcript.
    --
    -- Use this URI to access the transcript. If you specified an S3 bucket in
    -- the @OutputBucketName@ field when you created the job, this is the URI
    -- of that bucket. If you chose to store the transcript in Amazon
    -- Transcribe, this is a shareable URL that provides secure access to that
    -- location.
    transcriptFileUri :: Core.Maybe Core.Text,
    -- | The S3 object location of the redacted transcript.
    --
    -- Use this URI to access the redacted transcript. If you specified an S3
    -- bucket in the @OutputBucketName@ field when you created the job, this is
    -- the URI of that bucket. If you chose to store the transcript in Amazon
    -- Transcribe, this is a shareable URL that provides secure access to that
    -- location.
    redactedTranscriptFileUri :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Transcript' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transcriptFileUri', 'transcript_transcriptFileUri' - The S3 object location of the transcript.
--
-- Use this URI to access the transcript. If you specified an S3 bucket in
-- the @OutputBucketName@ field when you created the job, this is the URI
-- of that bucket. If you chose to store the transcript in Amazon
-- Transcribe, this is a shareable URL that provides secure access to that
-- location.
--
-- 'redactedTranscriptFileUri', 'transcript_redactedTranscriptFileUri' - The S3 object location of the redacted transcript.
--
-- Use this URI to access the redacted transcript. If you specified an S3
-- bucket in the @OutputBucketName@ field when you created the job, this is
-- the URI of that bucket. If you chose to store the transcript in Amazon
-- Transcribe, this is a shareable URL that provides secure access to that
-- location.
newTranscript ::
  Transcript
newTranscript =
  Transcript'
    { transcriptFileUri = Core.Nothing,
      redactedTranscriptFileUri = Core.Nothing
    }

-- | The S3 object location of the transcript.
--
-- Use this URI to access the transcript. If you specified an S3 bucket in
-- the @OutputBucketName@ field when you created the job, this is the URI
-- of that bucket. If you chose to store the transcript in Amazon
-- Transcribe, this is a shareable URL that provides secure access to that
-- location.
transcript_transcriptFileUri :: Lens.Lens' Transcript (Core.Maybe Core.Text)
transcript_transcriptFileUri = Lens.lens (\Transcript' {transcriptFileUri} -> transcriptFileUri) (\s@Transcript' {} a -> s {transcriptFileUri = a} :: Transcript)

-- | The S3 object location of the redacted transcript.
--
-- Use this URI to access the redacted transcript. If you specified an S3
-- bucket in the @OutputBucketName@ field when you created the job, this is
-- the URI of that bucket. If you chose to store the transcript in Amazon
-- Transcribe, this is a shareable URL that provides secure access to that
-- location.
transcript_redactedTranscriptFileUri :: Lens.Lens' Transcript (Core.Maybe Core.Text)
transcript_redactedTranscriptFileUri = Lens.lens (\Transcript' {redactedTranscriptFileUri} -> redactedTranscriptFileUri) (\s@Transcript' {} a -> s {redactedTranscriptFileUri = a} :: Transcript)

instance Core.FromJSON Transcript where
  parseJSON =
    Core.withObject
      "Transcript"
      ( \x ->
          Transcript'
            Core.<$> (x Core..:? "TranscriptFileUri")
            Core.<*> (x Core..:? "RedactedTranscriptFileUri")
      )

instance Core.Hashable Transcript

instance Core.NFData Transcript
