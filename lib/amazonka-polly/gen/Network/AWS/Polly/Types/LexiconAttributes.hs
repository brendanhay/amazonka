{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.Types.LexiconAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Polly.Types.LexiconAttributes
  ( LexiconAttributes (..),

    -- * Smart constructor
    mkLexiconAttributes,

    -- * Lenses
    laAlphabet,
    laLanguageCode,
    laLastModified,
    laLexemesCount,
    laLexiconArn,
    laSize,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Polly.Types.Alphabet as Types
import qualified Network.AWS.Polly.Types.LanguageCode as Types
import qualified Network.AWS.Polly.Types.LexiconArn as Types
import qualified Network.AWS.Prelude as Core

-- | Contains metadata describing the lexicon such as the number of lexemes, language code, and so on. For more information, see <https://docs.aws.amazon.com/polly/latest/dg/managing-lexicons.html Managing Lexicons> .
--
-- /See:/ 'mkLexiconAttributes' smart constructor.
data LexiconAttributes = LexiconAttributes'
  { -- | Phonetic alphabet used in the lexicon. Valid values are @ipa@ and @x-sampa@ .
    alphabet :: Core.Maybe Types.Alphabet,
    -- | Language code that the lexicon applies to. A lexicon with a language code such as "en" would be applied to all English languages (en-GB, en-US, en-AUS, en-WLS, and so on.
    languageCode :: Core.Maybe Types.LanguageCode,
    -- | Date lexicon was last modified (a timestamp value).
    lastModified :: Core.Maybe Core.NominalDiffTime,
    -- | Number of lexemes in the lexicon.
    lexemesCount :: Core.Maybe Core.Int,
    -- | Amazon Resource Name (ARN) of the lexicon.
    lexiconArn :: Core.Maybe Types.LexiconArn,
    -- | Total size of the lexicon, in characters.
    size :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'LexiconAttributes' value with any optional fields omitted.
mkLexiconAttributes ::
  LexiconAttributes
mkLexiconAttributes =
  LexiconAttributes'
    { alphabet = Core.Nothing,
      languageCode = Core.Nothing,
      lastModified = Core.Nothing,
      lexemesCount = Core.Nothing,
      lexiconArn = Core.Nothing,
      size = Core.Nothing
    }

-- | Phonetic alphabet used in the lexicon. Valid values are @ipa@ and @x-sampa@ .
--
-- /Note:/ Consider using 'alphabet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laAlphabet :: Lens.Lens' LexiconAttributes (Core.Maybe Types.Alphabet)
laAlphabet = Lens.field @"alphabet"
{-# DEPRECATED laAlphabet "Use generic-lens or generic-optics with 'alphabet' instead." #-}

-- | Language code that the lexicon applies to. A lexicon with a language code such as "en" would be applied to all English languages (en-GB, en-US, en-AUS, en-WLS, and so on.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laLanguageCode :: Lens.Lens' LexiconAttributes (Core.Maybe Types.LanguageCode)
laLanguageCode = Lens.field @"languageCode"
{-# DEPRECATED laLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | Date lexicon was last modified (a timestamp value).
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laLastModified :: Lens.Lens' LexiconAttributes (Core.Maybe Core.NominalDiffTime)
laLastModified = Lens.field @"lastModified"
{-# DEPRECATED laLastModified "Use generic-lens or generic-optics with 'lastModified' instead." #-}

-- | Number of lexemes in the lexicon.
--
-- /Note:/ Consider using 'lexemesCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laLexemesCount :: Lens.Lens' LexiconAttributes (Core.Maybe Core.Int)
laLexemesCount = Lens.field @"lexemesCount"
{-# DEPRECATED laLexemesCount "Use generic-lens or generic-optics with 'lexemesCount' instead." #-}

-- | Amazon Resource Name (ARN) of the lexicon.
--
-- /Note:/ Consider using 'lexiconArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laLexiconArn :: Lens.Lens' LexiconAttributes (Core.Maybe Types.LexiconArn)
laLexiconArn = Lens.field @"lexiconArn"
{-# DEPRECATED laLexiconArn "Use generic-lens or generic-optics with 'lexiconArn' instead." #-}

-- | Total size of the lexicon, in characters.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laSize :: Lens.Lens' LexiconAttributes (Core.Maybe Core.Int)
laSize = Lens.field @"size"
{-# DEPRECATED laSize "Use generic-lens or generic-optics with 'size' instead." #-}

instance Core.FromJSON LexiconAttributes where
  parseJSON =
    Core.withObject "LexiconAttributes" Core.$
      \x ->
        LexiconAttributes'
          Core.<$> (x Core..:? "Alphabet")
          Core.<*> (x Core..:? "LanguageCode")
          Core.<*> (x Core..:? "LastModified")
          Core.<*> (x Core..:? "LexemesCount")
          Core.<*> (x Core..:? "LexiconArn")
          Core.<*> (x Core..:? "Size")
