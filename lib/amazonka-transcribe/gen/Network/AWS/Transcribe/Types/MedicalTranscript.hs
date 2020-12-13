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
    mtTranscriptFileURI,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Identifies the location of a medical transcript.
--
-- /See:/ 'mkMedicalTranscript' smart constructor.
newtype MedicalTranscript = MedicalTranscript'
  { -- | The S3 object location of the medical transcript.
    --
    -- Use this URI to access the medical transcript. This URI points to the S3 bucket you created to store the medical transcript.
    transcriptFileURI :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MedicalTranscript' with the minimum fields required to make a request.
--
-- * 'transcriptFileURI' - The S3 object location of the medical transcript.
--
-- Use this URI to access the medical transcript. This URI points to the S3 bucket you created to store the medical transcript.
mkMedicalTranscript ::
  MedicalTranscript
mkMedicalTranscript =
  MedicalTranscript' {transcriptFileURI = Lude.Nothing}

-- | The S3 object location of the medical transcript.
--
-- Use this URI to access the medical transcript. This URI points to the S3 bucket you created to store the medical transcript.
--
-- /Note:/ Consider using 'transcriptFileURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtTranscriptFileURI :: Lens.Lens' MedicalTranscript (Lude.Maybe Lude.Text)
mtTranscriptFileURI = Lens.lens (transcriptFileURI :: MedicalTranscript -> Lude.Maybe Lude.Text) (\s a -> s {transcriptFileURI = a} :: MedicalTranscript)
{-# DEPRECATED mtTranscriptFileURI "Use generic-lens or generic-optics with 'transcriptFileURI' instead." #-}

instance Lude.FromJSON MedicalTranscript where
  parseJSON =
    Lude.withObject
      "MedicalTranscript"
      ( \x ->
          MedicalTranscript' Lude.<$> (x Lude..:? "TranscriptFileUri")
      )
