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
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Transcribe.Types.RedactionOutput as Types
import qualified Network.AWS.Transcribe.Types.RedactionType as Types

-- | Settings for content redaction within a transcription job.
--
-- /See:/ 'mkContentRedaction' smart constructor.
data ContentRedaction = ContentRedaction'
  { -- | Request parameter that defines the entities to be redacted. The only accepted value is @PII@ .
    redactionType :: Types.RedactionType,
    -- | The output transcript file stored in either the default S3 bucket or in a bucket you specify.
    --
    -- When you choose @redacted@ Amazon Transcribe outputs only the redacted transcript.
    -- When you choose @redacted_and_unredacted@ Amazon Transcribe outputs both the redacted and unredacted transcripts.
    redactionOutput :: Types.RedactionOutput
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ContentRedaction' value with any optional fields omitted.
mkContentRedaction ::
  -- | 'redactionType'
  Types.RedactionType ->
  -- | 'redactionOutput'
  Types.RedactionOutput ->
  ContentRedaction
mkContentRedaction redactionType redactionOutput =
  ContentRedaction' {redactionType, redactionOutput}

-- | Request parameter that defines the entities to be redacted. The only accepted value is @PII@ .
--
-- /Note:/ Consider using 'redactionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crRedactionType :: Lens.Lens' ContentRedaction Types.RedactionType
crRedactionType = Lens.field @"redactionType"
{-# DEPRECATED crRedactionType "Use generic-lens or generic-optics with 'redactionType' instead." #-}

-- | The output transcript file stored in either the default S3 bucket or in a bucket you specify.
--
-- When you choose @redacted@ Amazon Transcribe outputs only the redacted transcript.
-- When you choose @redacted_and_unredacted@ Amazon Transcribe outputs both the redacted and unredacted transcripts.
--
-- /Note:/ Consider using 'redactionOutput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crRedactionOutput :: Lens.Lens' ContentRedaction Types.RedactionOutput
crRedactionOutput = Lens.field @"redactionOutput"
{-# DEPRECATED crRedactionOutput "Use generic-lens or generic-optics with 'redactionOutput' instead." #-}

instance Core.FromJSON ContentRedaction where
  toJSON ContentRedaction {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("RedactionType" Core..= redactionType),
            Core.Just ("RedactionOutput" Core..= redactionOutput)
          ]
      )

instance Core.FromJSON ContentRedaction where
  parseJSON =
    Core.withObject "ContentRedaction" Core.$
      \x ->
        ContentRedaction'
          Core.<$> (x Core..: "RedactionType") Core.<*> (x Core..: "RedactionOutput")
