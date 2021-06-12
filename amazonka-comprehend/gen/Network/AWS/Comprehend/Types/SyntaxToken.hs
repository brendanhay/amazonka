{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.SyntaxToken
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.SyntaxToken where

import Network.AWS.Comprehend.Types.PartOfSpeechTag
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents a work in the input text that was recognized and assigned a
-- part of speech. There is one syntax token record for each word in the
-- source text.
--
-- /See:/ 'newSyntaxToken' smart constructor.
data SyntaxToken = SyntaxToken'
  { -- | A unique identifier for a token.
    tokenId :: Core.Maybe Core.Int,
    -- | Provides the part of speech label and the confidence level that Amazon
    -- Comprehend has that the part of speech was correctly identified. For
    -- more information, see how-syntax.
    partOfSpeech :: Core.Maybe PartOfSpeechTag,
    -- | The zero-based offset from the beginning of the source text to the last
    -- character in the word.
    endOffset :: Core.Maybe Core.Int,
    -- | The word that was recognized in the source text.
    text :: Core.Maybe Core.Text,
    -- | The zero-based offset from the beginning of the source text to the first
    -- character in the word.
    beginOffset :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SyntaxToken' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tokenId', 'syntaxToken_tokenId' - A unique identifier for a token.
--
-- 'partOfSpeech', 'syntaxToken_partOfSpeech' - Provides the part of speech label and the confidence level that Amazon
-- Comprehend has that the part of speech was correctly identified. For
-- more information, see how-syntax.
--
-- 'endOffset', 'syntaxToken_endOffset' - The zero-based offset from the beginning of the source text to the last
-- character in the word.
--
-- 'text', 'syntaxToken_text' - The word that was recognized in the source text.
--
-- 'beginOffset', 'syntaxToken_beginOffset' - The zero-based offset from the beginning of the source text to the first
-- character in the word.
newSyntaxToken ::
  SyntaxToken
newSyntaxToken =
  SyntaxToken'
    { tokenId = Core.Nothing,
      partOfSpeech = Core.Nothing,
      endOffset = Core.Nothing,
      text = Core.Nothing,
      beginOffset = Core.Nothing
    }

-- | A unique identifier for a token.
syntaxToken_tokenId :: Lens.Lens' SyntaxToken (Core.Maybe Core.Int)
syntaxToken_tokenId = Lens.lens (\SyntaxToken' {tokenId} -> tokenId) (\s@SyntaxToken' {} a -> s {tokenId = a} :: SyntaxToken)

-- | Provides the part of speech label and the confidence level that Amazon
-- Comprehend has that the part of speech was correctly identified. For
-- more information, see how-syntax.
syntaxToken_partOfSpeech :: Lens.Lens' SyntaxToken (Core.Maybe PartOfSpeechTag)
syntaxToken_partOfSpeech = Lens.lens (\SyntaxToken' {partOfSpeech} -> partOfSpeech) (\s@SyntaxToken' {} a -> s {partOfSpeech = a} :: SyntaxToken)

-- | The zero-based offset from the beginning of the source text to the last
-- character in the word.
syntaxToken_endOffset :: Lens.Lens' SyntaxToken (Core.Maybe Core.Int)
syntaxToken_endOffset = Lens.lens (\SyntaxToken' {endOffset} -> endOffset) (\s@SyntaxToken' {} a -> s {endOffset = a} :: SyntaxToken)

-- | The word that was recognized in the source text.
syntaxToken_text :: Lens.Lens' SyntaxToken (Core.Maybe Core.Text)
syntaxToken_text = Lens.lens (\SyntaxToken' {text} -> text) (\s@SyntaxToken' {} a -> s {text = a} :: SyntaxToken)

-- | The zero-based offset from the beginning of the source text to the first
-- character in the word.
syntaxToken_beginOffset :: Lens.Lens' SyntaxToken (Core.Maybe Core.Int)
syntaxToken_beginOffset = Lens.lens (\SyntaxToken' {beginOffset} -> beginOffset) (\s@SyntaxToken' {} a -> s {beginOffset = a} :: SyntaxToken)

instance Core.FromJSON SyntaxToken where
  parseJSON =
    Core.withObject
      "SyntaxToken"
      ( \x ->
          SyntaxToken'
            Core.<$> (x Core..:? "TokenId")
            Core.<*> (x Core..:? "PartOfSpeech")
            Core.<*> (x Core..:? "EndOffset")
            Core.<*> (x Core..:? "Text")
            Core.<*> (x Core..:? "BeginOffset")
      )

instance Core.Hashable SyntaxToken

instance Core.NFData SyntaxToken
