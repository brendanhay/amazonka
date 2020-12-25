{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    peEndOffset,
    peScore,
    peType,
  )
where

import qualified Network.AWS.Comprehend.Types.PiiEntityType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information about a PII entity.
--
-- /See:/ 'mkPiiEntity' smart constructor.
data PiiEntity = PiiEntity'
  { -- | A character offset in the input text that shows where the PII entity begins (the first character is at position 0). The offset returns the position of each UTF-8 code point in the string. A /code point/ is the abstract character from a particular graphical representation. For example, a multi-byte UTF-8 character maps to a single code point.
    beginOffset :: Core.Maybe Core.Int,
    -- | A character offset in the input text that shows where the PII entity ends. The offset returns the position of each UTF-8 code point in the string. A /code point/ is the abstract character from a particular graphical representation. For example, a multi-byte UTF-8 character maps to a single code point.
    endOffset :: Core.Maybe Core.Int,
    -- | The level of confidence that Amazon Comprehend has in the accuracy of the detection.
    score :: Core.Maybe Core.Double,
    -- | The entity's type.
    type' :: Core.Maybe Types.PiiEntityType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PiiEntity' value with any optional fields omitted.
mkPiiEntity ::
  PiiEntity
mkPiiEntity =
  PiiEntity'
    { beginOffset = Core.Nothing,
      endOffset = Core.Nothing,
      score = Core.Nothing,
      type' = Core.Nothing
    }

-- | A character offset in the input text that shows where the PII entity begins (the first character is at position 0). The offset returns the position of each UTF-8 code point in the string. A /code point/ is the abstract character from a particular graphical representation. For example, a multi-byte UTF-8 character maps to a single code point.
--
-- /Note:/ Consider using 'beginOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peBeginOffset :: Lens.Lens' PiiEntity (Core.Maybe Core.Int)
peBeginOffset = Lens.field @"beginOffset"
{-# DEPRECATED peBeginOffset "Use generic-lens or generic-optics with 'beginOffset' instead." #-}

-- | A character offset in the input text that shows where the PII entity ends. The offset returns the position of each UTF-8 code point in the string. A /code point/ is the abstract character from a particular graphical representation. For example, a multi-byte UTF-8 character maps to a single code point.
--
-- /Note:/ Consider using 'endOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peEndOffset :: Lens.Lens' PiiEntity (Core.Maybe Core.Int)
peEndOffset = Lens.field @"endOffset"
{-# DEPRECATED peEndOffset "Use generic-lens or generic-optics with 'endOffset' instead." #-}

-- | The level of confidence that Amazon Comprehend has in the accuracy of the detection.
--
-- /Note:/ Consider using 'score' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peScore :: Lens.Lens' PiiEntity (Core.Maybe Core.Double)
peScore = Lens.field @"score"
{-# DEPRECATED peScore "Use generic-lens or generic-optics with 'score' instead." #-}

-- | The entity's type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peType :: Lens.Lens' PiiEntity (Core.Maybe Types.PiiEntityType)
peType = Lens.field @"type'"
{-# DEPRECATED peType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON PiiEntity where
  parseJSON =
    Core.withObject "PiiEntity" Core.$
      \x ->
        PiiEntity'
          Core.<$> (x Core..:? "BeginOffset")
          Core.<*> (x Core..:? "EndOffset")
          Core.<*> (x Core..:? "Score")
          Core.<*> (x Core..:? "Type")
