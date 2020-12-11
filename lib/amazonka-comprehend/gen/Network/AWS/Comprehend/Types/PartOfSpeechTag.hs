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
    postTag,
    postScore,
  )
where

import Network.AWS.Comprehend.Types.PartOfSpeechTagType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Identifies the part of speech represented by the token and gives the confidence that Amazon Comprehend has that the part of speech was correctly identified. For more information about the parts of speech that Amazon Comprehend can identify, see 'how-syntax' .
--
-- /See:/ 'mkPartOfSpeechTag' smart constructor.
data PartOfSpeechTag = PartOfSpeechTag'
  { tag ::
      Lude.Maybe PartOfSpeechTagType,
    score :: Lude.Maybe Lude.Double
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PartOfSpeechTag' with the minimum fields required to make a request.
--
-- * 'score' - The confidence that Amazon Comprehend has that the part of speech was correctly identified.
-- * 'tag' - Identifies the part of speech that the token represents.
mkPartOfSpeechTag ::
  PartOfSpeechTag
mkPartOfSpeechTag =
  PartOfSpeechTag' {tag = Lude.Nothing, score = Lude.Nothing}

-- | Identifies the part of speech that the token represents.
--
-- /Note:/ Consider using 'tag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
postTag :: Lens.Lens' PartOfSpeechTag (Lude.Maybe PartOfSpeechTagType)
postTag = Lens.lens (tag :: PartOfSpeechTag -> Lude.Maybe PartOfSpeechTagType) (\s a -> s {tag = a} :: PartOfSpeechTag)
{-# DEPRECATED postTag "Use generic-lens or generic-optics with 'tag' instead." #-}

-- | The confidence that Amazon Comprehend has that the part of speech was correctly identified.
--
-- /Note:/ Consider using 'score' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
postScore :: Lens.Lens' PartOfSpeechTag (Lude.Maybe Lude.Double)
postScore = Lens.lens (score :: PartOfSpeechTag -> Lude.Maybe Lude.Double) (\s a -> s {score = a} :: PartOfSpeechTag)
{-# DEPRECATED postScore "Use generic-lens or generic-optics with 'score' instead." #-}

instance Lude.FromJSON PartOfSpeechTag where
  parseJSON =
    Lude.withObject
      "PartOfSpeechTag"
      ( \x ->
          PartOfSpeechTag'
            Lude.<$> (x Lude..:? "Tag") Lude.<*> (x Lude..:? "Score")
      )
