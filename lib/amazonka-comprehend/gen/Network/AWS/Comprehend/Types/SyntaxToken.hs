{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.SyntaxToken
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Comprehend.Types.SyntaxToken
  ( SyntaxToken (..)
  -- * Smart constructor
  , mkSyntaxToken
  -- * Lenses
  , stBeginOffset
  , stEndOffset
  , stPartOfSpeech
  , stText
  , stTokenId
  ) where

import qualified Network.AWS.Comprehend.Types.PartOfSpeechTag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a work in the input text that was recognized and assigned a part of speech. There is one syntax token record for each word in the source text.
--
-- /See:/ 'mkSyntaxToken' smart constructor.
data SyntaxToken = SyntaxToken'
  { beginOffset :: Core.Maybe Core.Int
    -- ^ The zero-based offset from the beginning of the source text to the first character in the word.
  , endOffset :: Core.Maybe Core.Int
    -- ^ The zero-based offset from the beginning of the source text to the last character in the word.
  , partOfSpeech :: Core.Maybe Types.PartOfSpeechTag
    -- ^ Provides the part of speech label and the confidence level that Amazon Comprehend has that the part of speech was correctly identified. For more information, see 'how-syntax' .
  , text :: Core.Maybe Core.Text
    -- ^ The word that was recognized in the source text.
  , tokenId :: Core.Maybe Core.Int
    -- ^ A unique identifier for a token.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SyntaxToken' value with any optional fields omitted.
mkSyntaxToken
    :: SyntaxToken
mkSyntaxToken
  = SyntaxToken'{beginOffset = Core.Nothing,
                 endOffset = Core.Nothing, partOfSpeech = Core.Nothing,
                 text = Core.Nothing, tokenId = Core.Nothing}

-- | The zero-based offset from the beginning of the source text to the first character in the word.
--
-- /Note:/ Consider using 'beginOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stBeginOffset :: Lens.Lens' SyntaxToken (Core.Maybe Core.Int)
stBeginOffset = Lens.field @"beginOffset"
{-# INLINEABLE stBeginOffset #-}
{-# DEPRECATED beginOffset "Use generic-lens or generic-optics with 'beginOffset' instead"  #-}

-- | The zero-based offset from the beginning of the source text to the last character in the word.
--
-- /Note:/ Consider using 'endOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stEndOffset :: Lens.Lens' SyntaxToken (Core.Maybe Core.Int)
stEndOffset = Lens.field @"endOffset"
{-# INLINEABLE stEndOffset #-}
{-# DEPRECATED endOffset "Use generic-lens or generic-optics with 'endOffset' instead"  #-}

-- | Provides the part of speech label and the confidence level that Amazon Comprehend has that the part of speech was correctly identified. For more information, see 'how-syntax' .
--
-- /Note:/ Consider using 'partOfSpeech' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stPartOfSpeech :: Lens.Lens' SyntaxToken (Core.Maybe Types.PartOfSpeechTag)
stPartOfSpeech = Lens.field @"partOfSpeech"
{-# INLINEABLE stPartOfSpeech #-}
{-# DEPRECATED partOfSpeech "Use generic-lens or generic-optics with 'partOfSpeech' instead"  #-}

-- | The word that was recognized in the source text.
--
-- /Note:/ Consider using 'text' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stText :: Lens.Lens' SyntaxToken (Core.Maybe Core.Text)
stText = Lens.field @"text"
{-# INLINEABLE stText #-}
{-# DEPRECATED text "Use generic-lens or generic-optics with 'text' instead"  #-}

-- | A unique identifier for a token.
--
-- /Note:/ Consider using 'tokenId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stTokenId :: Lens.Lens' SyntaxToken (Core.Maybe Core.Int)
stTokenId = Lens.field @"tokenId"
{-# INLINEABLE stTokenId #-}
{-# DEPRECATED tokenId "Use generic-lens or generic-optics with 'tokenId' instead"  #-}

instance Core.FromJSON SyntaxToken where
        parseJSON
          = Core.withObject "SyntaxToken" Core.$
              \ x ->
                SyntaxToken' Core.<$>
                  (x Core..:? "BeginOffset") Core.<*> x Core..:? "EndOffset" Core.<*>
                    x Core..:? "PartOfSpeech"
                    Core.<*> x Core..:? "Text"
                    Core.<*> x Core..:? "TokenId"
