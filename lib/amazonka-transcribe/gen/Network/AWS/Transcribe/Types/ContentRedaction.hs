{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.ContentRedaction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.ContentRedaction
  ( ContentRedaction (..),

    -- * Smart constructor
    mkContentRedaction,

    -- * Lenses
    crRedactionType,
    crRedactionOutput,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Transcribe.Types.RedactionOutput
import Network.AWS.Transcribe.Types.RedactionType

-- | Settings for content redaction within a transcription job.
--
-- /See:/ 'mkContentRedaction' smart constructor.
data ContentRedaction = ContentRedaction'
  { redactionType ::
      RedactionType,
    redactionOutput :: RedactionOutput
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ContentRedaction' with the minimum fields required to make a request.
--
-- * 'redactionOutput' - The output transcript file stored in either the default S3 bucket or in a bucket you specify.
--
-- When you choose @redacted@ Amazon Transcribe outputs only the redacted transcript.
-- When you choose @redacted_and_unredacted@ Amazon Transcribe outputs both the redacted and unredacted transcripts.
-- * 'redactionType' - Request parameter that defines the entities to be redacted. The only accepted value is @PII@ .
mkContentRedaction ::
  -- | 'redactionType'
  RedactionType ->
  -- | 'redactionOutput'
  RedactionOutput ->
  ContentRedaction
mkContentRedaction pRedactionType_ pRedactionOutput_ =
  ContentRedaction'
    { redactionType = pRedactionType_,
      redactionOutput = pRedactionOutput_
    }

-- | Request parameter that defines the entities to be redacted. The only accepted value is @PII@ .
--
-- /Note:/ Consider using 'redactionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crRedactionType :: Lens.Lens' ContentRedaction RedactionType
crRedactionType = Lens.lens (redactionType :: ContentRedaction -> RedactionType) (\s a -> s {redactionType = a} :: ContentRedaction)
{-# DEPRECATED crRedactionType "Use generic-lens or generic-optics with 'redactionType' instead." #-}

-- | The output transcript file stored in either the default S3 bucket or in a bucket you specify.
--
-- When you choose @redacted@ Amazon Transcribe outputs only the redacted transcript.
-- When you choose @redacted_and_unredacted@ Amazon Transcribe outputs both the redacted and unredacted transcripts.
--
-- /Note:/ Consider using 'redactionOutput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crRedactionOutput :: Lens.Lens' ContentRedaction RedactionOutput
crRedactionOutput = Lens.lens (redactionOutput :: ContentRedaction -> RedactionOutput) (\s a -> s {redactionOutput = a} :: ContentRedaction)
{-# DEPRECATED crRedactionOutput "Use generic-lens or generic-optics with 'redactionOutput' instead." #-}

instance Lude.FromJSON ContentRedaction where
  parseJSON =
    Lude.withObject
      "ContentRedaction"
      ( \x ->
          ContentRedaction'
            Lude.<$> (x Lude..: "RedactionType") Lude.<*> (x Lude..: "RedactionOutput")
      )

instance Lude.ToJSON ContentRedaction where
  toJSON ContentRedaction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("RedactionType" Lude..= redactionType),
            Lude.Just ("RedactionOutput" Lude..= redactionOutput)
          ]
      )
