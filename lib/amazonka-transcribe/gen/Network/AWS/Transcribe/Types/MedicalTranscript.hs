{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.MedicalTranscript
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.MedicalTranscript
  ( MedicalTranscript (..),

    -- * Smart constructor
    mkMedicalTranscript,

    -- * Lenses
    mtTranscriptFileUri,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Transcribe.Types.Uri as Types

-- | Identifies the location of a medical transcript.
--
-- /See:/ 'mkMedicalTranscript' smart constructor.
newtype MedicalTranscript = MedicalTranscript'
  { -- | The S3 object location of the medical transcript.
    --
    -- Use this URI to access the medical transcript. This URI points to the S3 bucket you created to store the medical transcript.
    transcriptFileUri :: Core.Maybe Types.Uri
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'MedicalTranscript' value with any optional fields omitted.
mkMedicalTranscript ::
  MedicalTranscript
mkMedicalTranscript =
  MedicalTranscript' {transcriptFileUri = Core.Nothing}

-- | The S3 object location of the medical transcript.
--
-- Use this URI to access the medical transcript. This URI points to the S3 bucket you created to store the medical transcript.
--
-- /Note:/ Consider using 'transcriptFileUri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtTranscriptFileUri :: Lens.Lens' MedicalTranscript (Core.Maybe Types.Uri)
mtTranscriptFileUri = Lens.field @"transcriptFileUri"
{-# DEPRECATED mtTranscriptFileUri "Use generic-lens or generic-optics with 'transcriptFileUri' instead." #-}

instance Core.FromJSON MedicalTranscript where
  parseJSON =
    Core.withObject "MedicalTranscript" Core.$
      \x -> MedicalTranscript' Core.<$> (x Core..:? "TranscriptFileUri")
