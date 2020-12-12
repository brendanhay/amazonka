{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.Transcript
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.Transcript
  ( Transcript (..),

    -- * Smart constructor
    mkTranscript,

    -- * Lenses
    tRedactedTranscriptFileURI,
    tTranscriptFileURI,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Identifies the location of a transcription.
--
-- /See:/ 'mkTranscript' smart constructor.
data Transcript = Transcript'
  { redactedTranscriptFileURI ::
      Lude.Maybe Lude.Text,
    transcriptFileURI :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Transcript' with the minimum fields required to make a request.
--
-- * 'redactedTranscriptFileURI' - The S3 object location of the redacted transcript.
--
-- Use this URI to access the redacted transcript. If you specified an S3 bucket in the @OutputBucketName@ field when you created the job, this is the URI of that bucket. If you chose to store the transcript in Amazon Transcribe, this is a shareable URL that provides secure access to that location.
-- * 'transcriptFileURI' - The S3 object location of the transcript.
--
-- Use this URI to access the transcript. If you specified an S3 bucket in the @OutputBucketName@ field when you created the job, this is the URI of that bucket. If you chose to store the transcript in Amazon Transcribe, this is a shareable URL that provides secure access to that location.
mkTranscript ::
  Transcript
mkTranscript =
  Transcript'
    { redactedTranscriptFileURI = Lude.Nothing,
      transcriptFileURI = Lude.Nothing
    }

-- | The S3 object location of the redacted transcript.
--
-- Use this URI to access the redacted transcript. If you specified an S3 bucket in the @OutputBucketName@ field when you created the job, this is the URI of that bucket. If you chose to store the transcript in Amazon Transcribe, this is a shareable URL that provides secure access to that location.
--
-- /Note:/ Consider using 'redactedTranscriptFileURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tRedactedTranscriptFileURI :: Lens.Lens' Transcript (Lude.Maybe Lude.Text)
tRedactedTranscriptFileURI = Lens.lens (redactedTranscriptFileURI :: Transcript -> Lude.Maybe Lude.Text) (\s a -> s {redactedTranscriptFileURI = a} :: Transcript)
{-# DEPRECATED tRedactedTranscriptFileURI "Use generic-lens or generic-optics with 'redactedTranscriptFileURI' instead." #-}

-- | The S3 object location of the transcript.
--
-- Use this URI to access the transcript. If you specified an S3 bucket in the @OutputBucketName@ field when you created the job, this is the URI of that bucket. If you chose to store the transcript in Amazon Transcribe, this is a shareable URL that provides secure access to that location.
--
-- /Note:/ Consider using 'transcriptFileURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTranscriptFileURI :: Lens.Lens' Transcript (Lude.Maybe Lude.Text)
tTranscriptFileURI = Lens.lens (transcriptFileURI :: Transcript -> Lude.Maybe Lude.Text) (\s a -> s {transcriptFileURI = a} :: Transcript)
{-# DEPRECATED tTranscriptFileURI "Use generic-lens or generic-optics with 'transcriptFileURI' instead." #-}

instance Lude.FromJSON Transcript where
  parseJSON =
    Lude.withObject
      "Transcript"
      ( \x ->
          Transcript'
            Lude.<$> (x Lude..:? "RedactedTranscriptFileUri")
            Lude.<*> (x Lude..:? "TranscriptFileUri")
      )
