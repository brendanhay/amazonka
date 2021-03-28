{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.Transcript
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Transcribe.Types.Transcript
  ( Transcript (..)
  -- * Smart constructor
  , mkTranscript
  -- * Lenses
  , tRedactedTranscriptFileUri
  , tTranscriptFileUri
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Transcribe.Types.Uri as Types

-- | Identifies the location of a transcription.
--
-- /See:/ 'mkTranscript' smart constructor.
data Transcript = Transcript'
  { redactedTranscriptFileUri :: Core.Maybe Types.Uri
    -- ^ The S3 object location of the redacted transcript.
--
-- Use this URI to access the redacted transcript. If you specified an S3 bucket in the @OutputBucketName@ field when you created the job, this is the URI of that bucket. If you chose to store the transcript in Amazon Transcribe, this is a shareable URL that provides secure access to that location.
  , transcriptFileUri :: Core.Maybe Types.Uri
    -- ^ The S3 object location of the transcript.
--
-- Use this URI to access the transcript. If you specified an S3 bucket in the @OutputBucketName@ field when you created the job, this is the URI of that bucket. If you chose to store the transcript in Amazon Transcribe, this is a shareable URL that provides secure access to that location.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Transcript' value with any optional fields omitted.
mkTranscript
    :: Transcript
mkTranscript
  = Transcript'{redactedTranscriptFileUri = Core.Nothing,
                transcriptFileUri = Core.Nothing}

-- | The S3 object location of the redacted transcript.
--
-- Use this URI to access the redacted transcript. If you specified an S3 bucket in the @OutputBucketName@ field when you created the job, this is the URI of that bucket. If you chose to store the transcript in Amazon Transcribe, this is a shareable URL that provides secure access to that location.
--
-- /Note:/ Consider using 'redactedTranscriptFileUri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tRedactedTranscriptFileUri :: Lens.Lens' Transcript (Core.Maybe Types.Uri)
tRedactedTranscriptFileUri = Lens.field @"redactedTranscriptFileUri"
{-# INLINEABLE tRedactedTranscriptFileUri #-}
{-# DEPRECATED redactedTranscriptFileUri "Use generic-lens or generic-optics with 'redactedTranscriptFileUri' instead"  #-}

-- | The S3 object location of the transcript.
--
-- Use this URI to access the transcript. If you specified an S3 bucket in the @OutputBucketName@ field when you created the job, this is the URI of that bucket. If you chose to store the transcript in Amazon Transcribe, this is a shareable URL that provides secure access to that location.
--
-- /Note:/ Consider using 'transcriptFileUri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTranscriptFileUri :: Lens.Lens' Transcript (Core.Maybe Types.Uri)
tTranscriptFileUri = Lens.field @"transcriptFileUri"
{-# INLINEABLE tTranscriptFileUri #-}
{-# DEPRECATED transcriptFileUri "Use generic-lens or generic-optics with 'transcriptFileUri' instead"  #-}

instance Core.FromJSON Transcript where
        parseJSON
          = Core.withObject "Transcript" Core.$
              \ x ->
                Transcript' Core.<$>
                  (x Core..:? "RedactedTranscriptFileUri") Core.<*>
                    x Core..:? "TranscriptFileUri"
