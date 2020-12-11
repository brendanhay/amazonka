-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.Entity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.Entity
  ( Entity (..),

    -- * Smart constructor
    mkEntity,

    -- * Lenses
    eBeginOffset,
    eText,
    eScore,
    eEndOffset,
    eType,
  )
where

import Network.AWS.Comprehend.Types.EntityType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about an entity.
--
--
--
-- /See:/ 'mkEntity' smart constructor.
data Entity = Entity'
  { beginOffset :: Lude.Maybe Lude.Int,
    text :: Lude.Maybe Lude.Text,
    score :: Lude.Maybe Lude.Double,
    endOffset :: Lude.Maybe Lude.Int,
    type' :: Lude.Maybe EntityType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Entity' with the minimum fields required to make a request.
--
-- * 'beginOffset' - A character offset in the input text that shows where the entity begins (the first character is at position 0). The offset returns the position of each UTF-8 code point in the string. A /code point/ is the abstract character from a particular graphical representation. For example, a multi-byte UTF-8 character maps to a single code point.
-- * 'endOffset' - A character offset in the input text that shows where the entity ends. The offset returns the position of each UTF-8 code point in the string. A /code point/ is the abstract character from a particular graphical representation. For example, a multi-byte UTF-8 character maps to a single code point.
-- * 'score' - The level of confidence that Amazon Comprehend has in the accuracy of the detection.
-- * 'text' - The text of the entity.
-- * 'type'' - The entity's type.
mkEntity ::
  Entity
mkEntity =
  Entity'
    { beginOffset = Lude.Nothing,
      text = Lude.Nothing,
      score = Lude.Nothing,
      endOffset = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | A character offset in the input text that shows where the entity begins (the first character is at position 0). The offset returns the position of each UTF-8 code point in the string. A /code point/ is the abstract character from a particular graphical representation. For example, a multi-byte UTF-8 character maps to a single code point.
--
-- /Note:/ Consider using 'beginOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eBeginOffset :: Lens.Lens' Entity (Lude.Maybe Lude.Int)
eBeginOffset = Lens.lens (beginOffset :: Entity -> Lude.Maybe Lude.Int) (\s a -> s {beginOffset = a} :: Entity)
{-# DEPRECATED eBeginOffset "Use generic-lens or generic-optics with 'beginOffset' instead." #-}

-- | The text of the entity.
--
-- /Note:/ Consider using 'text' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eText :: Lens.Lens' Entity (Lude.Maybe Lude.Text)
eText = Lens.lens (text :: Entity -> Lude.Maybe Lude.Text) (\s a -> s {text = a} :: Entity)
{-# DEPRECATED eText "Use generic-lens or generic-optics with 'text' instead." #-}

-- | The level of confidence that Amazon Comprehend has in the accuracy of the detection.
--
-- /Note:/ Consider using 'score' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eScore :: Lens.Lens' Entity (Lude.Maybe Lude.Double)
eScore = Lens.lens (score :: Entity -> Lude.Maybe Lude.Double) (\s a -> s {score = a} :: Entity)
{-# DEPRECATED eScore "Use generic-lens or generic-optics with 'score' instead." #-}

-- | A character offset in the input text that shows where the entity ends. The offset returns the position of each UTF-8 code point in the string. A /code point/ is the abstract character from a particular graphical representation. For example, a multi-byte UTF-8 character maps to a single code point.
--
-- /Note:/ Consider using 'endOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEndOffset :: Lens.Lens' Entity (Lude.Maybe Lude.Int)
eEndOffset = Lens.lens (endOffset :: Entity -> Lude.Maybe Lude.Int) (\s a -> s {endOffset = a} :: Entity)
{-# DEPRECATED eEndOffset "Use generic-lens or generic-optics with 'endOffset' instead." #-}

-- | The entity's type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eType :: Lens.Lens' Entity (Lude.Maybe EntityType)
eType = Lens.lens (type' :: Entity -> Lude.Maybe EntityType) (\s a -> s {type' = a} :: Entity)
{-# DEPRECATED eType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON Entity where
  parseJSON =
    Lude.withObject
      "Entity"
      ( \x ->
          Entity'
            Lude.<$> (x Lude..:? "BeginOffset")
            Lude.<*> (x Lude..:? "Text")
            Lude.<*> (x Lude..:? "Score")
            Lude.<*> (x Lude..:? "EndOffset")
            Lude.<*> (x Lude..:? "Type")
      )
