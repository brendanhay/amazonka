-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.SyntaxToken
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.SyntaxToken
  ( SyntaxToken (..),

    -- * Smart constructor
    mkSyntaxToken,

    -- * Lenses
    stBeginOffset,
    stText,
    stTokenId,
    stEndOffset,
    stPartOfSpeech,
  )
where

import Network.AWS.Comprehend.Types.PartOfSpeechTag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a work in the input text that was recognized and assigned a part of speech. There is one syntax token record for each word in the source text.
--
-- /See:/ 'mkSyntaxToken' smart constructor.
data SyntaxToken = SyntaxToken'
  { beginOffset :: Lude.Maybe Lude.Int,
    text :: Lude.Maybe Lude.Text,
    tokenId :: Lude.Maybe Lude.Int,
    endOffset :: Lude.Maybe Lude.Int,
    partOfSpeech :: Lude.Maybe PartOfSpeechTag
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SyntaxToken' with the minimum fields required to make a request.
--
-- * 'beginOffset' - The zero-based offset from the beginning of the source text to the first character in the word.
-- * 'endOffset' - The zero-based offset from the beginning of the source text to the last character in the word.
-- * 'partOfSpeech' - Provides the part of speech label and the confidence level that Amazon Comprehend has that the part of speech was correctly identified. For more information, see 'how-syntax' .
-- * 'text' - The word that was recognized in the source text.
-- * 'tokenId' - A unique identifier for a token.
mkSyntaxToken ::
  SyntaxToken
mkSyntaxToken =
  SyntaxToken'
    { beginOffset = Lude.Nothing,
      text = Lude.Nothing,
      tokenId = Lude.Nothing,
      endOffset = Lude.Nothing,
      partOfSpeech = Lude.Nothing
    }

-- | The zero-based offset from the beginning of the source text to the first character in the word.
--
-- /Note:/ Consider using 'beginOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stBeginOffset :: Lens.Lens' SyntaxToken (Lude.Maybe Lude.Int)
stBeginOffset = Lens.lens (beginOffset :: SyntaxToken -> Lude.Maybe Lude.Int) (\s a -> s {beginOffset = a} :: SyntaxToken)
{-# DEPRECATED stBeginOffset "Use generic-lens or generic-optics with 'beginOffset' instead." #-}

-- | The word that was recognized in the source text.
--
-- /Note:/ Consider using 'text' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stText :: Lens.Lens' SyntaxToken (Lude.Maybe Lude.Text)
stText = Lens.lens (text :: SyntaxToken -> Lude.Maybe Lude.Text) (\s a -> s {text = a} :: SyntaxToken)
{-# DEPRECATED stText "Use generic-lens or generic-optics with 'text' instead." #-}

-- | A unique identifier for a token.
--
-- /Note:/ Consider using 'tokenId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stTokenId :: Lens.Lens' SyntaxToken (Lude.Maybe Lude.Int)
stTokenId = Lens.lens (tokenId :: SyntaxToken -> Lude.Maybe Lude.Int) (\s a -> s {tokenId = a} :: SyntaxToken)
{-# DEPRECATED stTokenId "Use generic-lens or generic-optics with 'tokenId' instead." #-}

-- | The zero-based offset from the beginning of the source text to the last character in the word.
--
-- /Note:/ Consider using 'endOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stEndOffset :: Lens.Lens' SyntaxToken (Lude.Maybe Lude.Int)
stEndOffset = Lens.lens (endOffset :: SyntaxToken -> Lude.Maybe Lude.Int) (\s a -> s {endOffset = a} :: SyntaxToken)
{-# DEPRECATED stEndOffset "Use generic-lens or generic-optics with 'endOffset' instead." #-}

-- | Provides the part of speech label and the confidence level that Amazon Comprehend has that the part of speech was correctly identified. For more information, see 'how-syntax' .
--
-- /Note:/ Consider using 'partOfSpeech' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stPartOfSpeech :: Lens.Lens' SyntaxToken (Lude.Maybe PartOfSpeechTag)
stPartOfSpeech = Lens.lens (partOfSpeech :: SyntaxToken -> Lude.Maybe PartOfSpeechTag) (\s a -> s {partOfSpeech = a} :: SyntaxToken)
{-# DEPRECATED stPartOfSpeech "Use generic-lens or generic-optics with 'partOfSpeech' instead." #-}

instance Lude.FromJSON SyntaxToken where
  parseJSON =
    Lude.withObject
      "SyntaxToken"
      ( \x ->
          SyntaxToken'
            Lude.<$> (x Lude..:? "BeginOffset")
            Lude.<*> (x Lude..:? "Text")
            Lude.<*> (x Lude..:? "TokenId")
            Lude.<*> (x Lude..:? "EndOffset")
            Lude.<*> (x Lude..:? "PartOfSpeech")
      )
