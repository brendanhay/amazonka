-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.PiiEntity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.PiiEntity
  ( PiiEntity (..),

    -- * Smart constructor
    mkPiiEntity,

    -- * Lenses
    peBeginOffset,
    peScore,
    peEndOffset,
    peType,
  )
where

import Network.AWS.Comprehend.Types.PiiEntityType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about a PII entity.
--
-- /See:/ 'mkPiiEntity' smart constructor.
data PiiEntity = PiiEntity'
  { beginOffset :: Lude.Maybe Lude.Int,
    score :: Lude.Maybe Lude.Double,
    endOffset :: Lude.Maybe Lude.Int,
    type' :: Lude.Maybe PiiEntityType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PiiEntity' with the minimum fields required to make a request.
--
-- * 'beginOffset' - A character offset in the input text that shows where the PII entity begins (the first character is at position 0). The offset returns the position of each UTF-8 code point in the string. A /code point/ is the abstract character from a particular graphical representation. For example, a multi-byte UTF-8 character maps to a single code point.
-- * 'endOffset' - A character offset in the input text that shows where the PII entity ends. The offset returns the position of each UTF-8 code point in the string. A /code point/ is the abstract character from a particular graphical representation. For example, a multi-byte UTF-8 character maps to a single code point.
-- * 'score' - The level of confidence that Amazon Comprehend has in the accuracy of the detection.
-- * 'type'' - The entity's type.
mkPiiEntity ::
  PiiEntity
mkPiiEntity =
  PiiEntity'
    { beginOffset = Lude.Nothing,
      score = Lude.Nothing,
      endOffset = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | A character offset in the input text that shows where the PII entity begins (the first character is at position 0). The offset returns the position of each UTF-8 code point in the string. A /code point/ is the abstract character from a particular graphical representation. For example, a multi-byte UTF-8 character maps to a single code point.
--
-- /Note:/ Consider using 'beginOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peBeginOffset :: Lens.Lens' PiiEntity (Lude.Maybe Lude.Int)
peBeginOffset = Lens.lens (beginOffset :: PiiEntity -> Lude.Maybe Lude.Int) (\s a -> s {beginOffset = a} :: PiiEntity)
{-# DEPRECATED peBeginOffset "Use generic-lens or generic-optics with 'beginOffset' instead." #-}

-- | The level of confidence that Amazon Comprehend has in the accuracy of the detection.
--
-- /Note:/ Consider using 'score' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peScore :: Lens.Lens' PiiEntity (Lude.Maybe Lude.Double)
peScore = Lens.lens (score :: PiiEntity -> Lude.Maybe Lude.Double) (\s a -> s {score = a} :: PiiEntity)
{-# DEPRECATED peScore "Use generic-lens or generic-optics with 'score' instead." #-}

-- | A character offset in the input text that shows where the PII entity ends. The offset returns the position of each UTF-8 code point in the string. A /code point/ is the abstract character from a particular graphical representation. For example, a multi-byte UTF-8 character maps to a single code point.
--
-- /Note:/ Consider using 'endOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peEndOffset :: Lens.Lens' PiiEntity (Lude.Maybe Lude.Int)
peEndOffset = Lens.lens (endOffset :: PiiEntity -> Lude.Maybe Lude.Int) (\s a -> s {endOffset = a} :: PiiEntity)
{-# DEPRECATED peEndOffset "Use generic-lens or generic-optics with 'endOffset' instead." #-}

-- | The entity's type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peType :: Lens.Lens' PiiEntity (Lude.Maybe PiiEntityType)
peType = Lens.lens (type' :: PiiEntity -> Lude.Maybe PiiEntityType) (\s a -> s {type' = a} :: PiiEntity)
{-# DEPRECATED peType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON PiiEntity where
  parseJSON =
    Lude.withObject
      "PiiEntity"
      ( \x ->
          PiiEntity'
            Lude.<$> (x Lude..:? "BeginOffset")
            Lude.<*> (x Lude..:? "Score")
            Lude.<*> (x Lude..:? "EndOffset")
            Lude.<*> (x Lude..:? "Type")
      )
