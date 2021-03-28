{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.PartOfSpeechTag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Comprehend.Types.PartOfSpeechTag
  ( PartOfSpeechTag (..)
  -- * Smart constructor
  , mkPartOfSpeechTag
  -- * Lenses
  , postScore
  , postTag
  ) where

import qualified Network.AWS.Comprehend.Types.PartOfSpeechTagType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Identifies the part of speech represented by the token and gives the confidence that Amazon Comprehend has that the part of speech was correctly identified. For more information about the parts of speech that Amazon Comprehend can identify, see 'how-syntax' .
--
-- /See:/ 'mkPartOfSpeechTag' smart constructor.
data PartOfSpeechTag = PartOfSpeechTag'
  { score :: Core.Maybe Core.Double
    -- ^ The confidence that Amazon Comprehend has that the part of speech was correctly identified.
  , tag :: Core.Maybe Types.PartOfSpeechTagType
    -- ^ Identifies the part of speech that the token represents.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PartOfSpeechTag' value with any optional fields omitted.
mkPartOfSpeechTag
    :: PartOfSpeechTag
mkPartOfSpeechTag
  = PartOfSpeechTag'{score = Core.Nothing, tag = Core.Nothing}

-- | The confidence that Amazon Comprehend has that the part of speech was correctly identified.
--
-- /Note:/ Consider using 'score' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
postScore :: Lens.Lens' PartOfSpeechTag (Core.Maybe Core.Double)
postScore = Lens.field @"score"
{-# INLINEABLE postScore #-}
{-# DEPRECATED score "Use generic-lens or generic-optics with 'score' instead"  #-}

-- | Identifies the part of speech that the token represents.
--
-- /Note:/ Consider using 'tag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
postTag :: Lens.Lens' PartOfSpeechTag (Core.Maybe Types.PartOfSpeechTagType)
postTag = Lens.field @"tag"
{-# INLINEABLE postTag #-}
{-# DEPRECATED tag "Use generic-lens or generic-optics with 'tag' instead"  #-}

instance Core.FromJSON PartOfSpeechTag where
        parseJSON
          = Core.withObject "PartOfSpeechTag" Core.$
              \ x ->
                PartOfSpeechTag' Core.<$>
                  (x Core..:? "Score") Core.<*> x Core..:? "Tag"
