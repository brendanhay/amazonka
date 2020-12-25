{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.PartOfSpeechTag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.PartOfSpeechTag
  ( PartOfSpeechTag (..),

    -- * Smart constructor
    mkPartOfSpeechTag,

    -- * Lenses
    postScore,
    postTag,
  )
where

import qualified Network.AWS.Comprehend.Types.PartOfSpeechTagType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Identifies the part of speech represented by the token and gives the confidence that Amazon Comprehend has that the part of speech was correctly identified. For more information about the parts of speech that Amazon Comprehend can identify, see 'how-syntax' .
--
-- /See:/ 'mkPartOfSpeechTag' smart constructor.
data PartOfSpeechTag = PartOfSpeechTag'
  { -- | The confidence that Amazon Comprehend has that the part of speech was correctly identified.
    score :: Core.Maybe Core.Double,
    -- | Identifies the part of speech that the token represents.
    tag :: Core.Maybe Types.PartOfSpeechTagType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PartOfSpeechTag' value with any optional fields omitted.
mkPartOfSpeechTag ::
  PartOfSpeechTag
mkPartOfSpeechTag =
  PartOfSpeechTag' {score = Core.Nothing, tag = Core.Nothing}

-- | The confidence that Amazon Comprehend has that the part of speech was correctly identified.
--
-- /Note:/ Consider using 'score' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
postScore :: Lens.Lens' PartOfSpeechTag (Core.Maybe Core.Double)
postScore = Lens.field @"score"
{-# DEPRECATED postScore "Use generic-lens or generic-optics with 'score' instead." #-}

-- | Identifies the part of speech that the token represents.
--
-- /Note:/ Consider using 'tag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
postTag :: Lens.Lens' PartOfSpeechTag (Core.Maybe Types.PartOfSpeechTagType)
postTag = Lens.field @"tag"
{-# DEPRECATED postTag "Use generic-lens or generic-optics with 'tag' instead." #-}

instance Core.FromJSON PartOfSpeechTag where
  parseJSON =
    Core.withObject "PartOfSpeechTag" Core.$
      \x ->
        PartOfSpeechTag'
          Core.<$> (x Core..:? "Score") Core.<*> (x Core..:? "Tag")
