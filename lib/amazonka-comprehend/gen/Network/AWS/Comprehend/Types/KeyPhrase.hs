{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.KeyPhrase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.KeyPhrase
  ( KeyPhrase (..),

    -- * Smart constructor
    mkKeyPhrase,

    -- * Lenses
    kpBeginOffset,
    kpEndOffset,
    kpScore,
    kpText,
  )
where

import qualified Network.AWS.Comprehend.Types.Text as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a key noun phrase.
--
-- /See:/ 'mkKeyPhrase' smart constructor.
data KeyPhrase = KeyPhrase'
  { -- | A character offset in the input text that shows where the key phrase begins (the first character is at position 0). The offset returns the position of each UTF-8 code point in the string. A /code point/ is the abstract character from a particular graphical representation. For example, a multi-byte UTF-8 character maps to a single code point.
    beginOffset :: Core.Maybe Core.Int,
    -- | A character offset in the input text where the key phrase ends. The offset returns the position of each UTF-8 code point in the string. A @code point@ is the abstract character from a particular graphical representation. For example, a multi-byte UTF-8 character maps to a single code point.
    endOffset :: Core.Maybe Core.Int,
    -- | The level of confidence that Amazon Comprehend has in the accuracy of the detection.
    score :: Core.Maybe Core.Double,
    -- | The text of a key noun phrase.
    text :: Core.Maybe Types.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'KeyPhrase' value with any optional fields omitted.
mkKeyPhrase ::
  KeyPhrase
mkKeyPhrase =
  KeyPhrase'
    { beginOffset = Core.Nothing,
      endOffset = Core.Nothing,
      score = Core.Nothing,
      text = Core.Nothing
    }

-- | A character offset in the input text that shows where the key phrase begins (the first character is at position 0). The offset returns the position of each UTF-8 code point in the string. A /code point/ is the abstract character from a particular graphical representation. For example, a multi-byte UTF-8 character maps to a single code point.
--
-- /Note:/ Consider using 'beginOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpBeginOffset :: Lens.Lens' KeyPhrase (Core.Maybe Core.Int)
kpBeginOffset = Lens.field @"beginOffset"
{-# DEPRECATED kpBeginOffset "Use generic-lens or generic-optics with 'beginOffset' instead." #-}

-- | A character offset in the input text where the key phrase ends. The offset returns the position of each UTF-8 code point in the string. A @code point@ is the abstract character from a particular graphical representation. For example, a multi-byte UTF-8 character maps to a single code point.
--
-- /Note:/ Consider using 'endOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpEndOffset :: Lens.Lens' KeyPhrase (Core.Maybe Core.Int)
kpEndOffset = Lens.field @"endOffset"
{-# DEPRECATED kpEndOffset "Use generic-lens or generic-optics with 'endOffset' instead." #-}

-- | The level of confidence that Amazon Comprehend has in the accuracy of the detection.
--
-- /Note:/ Consider using 'score' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpScore :: Lens.Lens' KeyPhrase (Core.Maybe Core.Double)
kpScore = Lens.field @"score"
{-# DEPRECATED kpScore "Use generic-lens or generic-optics with 'score' instead." #-}

-- | The text of a key noun phrase.
--
-- /Note:/ Consider using 'text' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpText :: Lens.Lens' KeyPhrase (Core.Maybe Types.Text)
kpText = Lens.field @"text"
{-# DEPRECATED kpText "Use generic-lens or generic-optics with 'text' instead." #-}

instance Core.FromJSON KeyPhrase where
  parseJSON =
    Core.withObject "KeyPhrase" Core.$
      \x ->
        KeyPhrase'
          Core.<$> (x Core..:? "BeginOffset")
          Core.<*> (x Core..:? "EndOffset")
          Core.<*> (x Core..:? "Score")
          Core.<*> (x Core..:? "Text")
