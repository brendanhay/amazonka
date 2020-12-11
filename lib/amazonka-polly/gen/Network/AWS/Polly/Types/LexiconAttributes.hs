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
    laLanguageCode,
    laSize,
    laLexemesCount,
    laLexiconARN,
    laAlphabet,
    laLastModified,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Polly.Types.LanguageCode
import qualified Network.AWS.Prelude as Lude

-- | Contains metadata describing the lexicon such as the number of lexemes, language code, and so on. For more information, see <https://docs.aws.amazon.com/polly/latest/dg/managing-lexicons.html Managing Lexicons> .
--
-- /See:/ 'mkLexiconAttributes' smart constructor.
data LexiconAttributes = LexiconAttributes'
  { languageCode ::
      Lude.Maybe LanguageCode,
    size :: Lude.Maybe Lude.Int,
    lexemesCount :: Lude.Maybe Lude.Int,
    lexiconARN :: Lude.Maybe Lude.Text,
    alphabet :: Lude.Maybe Lude.Text,
    lastModified :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LexiconAttributes' with the minimum fields required to make a request.
--
-- * 'alphabet' - Phonetic alphabet used in the lexicon. Valid values are @ipa@ and @x-sampa@ .
-- * 'languageCode' - Language code that the lexicon applies to. A lexicon with a language code such as "en" would be applied to all English languages (en-GB, en-US, en-AUS, en-WLS, and so on.
-- * 'lastModified' - Date lexicon was last modified (a timestamp value).
-- * 'lexemesCount' - Number of lexemes in the lexicon.
-- * 'lexiconARN' - Amazon Resource Name (ARN) of the lexicon.
-- * 'size' - Total size of the lexicon, in characters.
mkLexiconAttributes ::
  LexiconAttributes
mkLexiconAttributes =
  LexiconAttributes'
    { languageCode = Lude.Nothing,
      size = Lude.Nothing,
      lexemesCount = Lude.Nothing,
      lexiconARN = Lude.Nothing,
      alphabet = Lude.Nothing,
      lastModified = Lude.Nothing
    }

-- | Language code that the lexicon applies to. A lexicon with a language code such as "en" would be applied to all English languages (en-GB, en-US, en-AUS, en-WLS, and so on.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laLanguageCode :: Lens.Lens' LexiconAttributes (Lude.Maybe LanguageCode)
laLanguageCode = Lens.lens (languageCode :: LexiconAttributes -> Lude.Maybe LanguageCode) (\s a -> s {languageCode = a} :: LexiconAttributes)
{-# DEPRECATED laLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | Total size of the lexicon, in characters.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laSize :: Lens.Lens' LexiconAttributes (Lude.Maybe Lude.Int)
laSize = Lens.lens (size :: LexiconAttributes -> Lude.Maybe Lude.Int) (\s a -> s {size = a} :: LexiconAttributes)
{-# DEPRECATED laSize "Use generic-lens or generic-optics with 'size' instead." #-}

-- | Number of lexemes in the lexicon.
--
-- /Note:/ Consider using 'lexemesCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laLexemesCount :: Lens.Lens' LexiconAttributes (Lude.Maybe Lude.Int)
laLexemesCount = Lens.lens (lexemesCount :: LexiconAttributes -> Lude.Maybe Lude.Int) (\s a -> s {lexemesCount = a} :: LexiconAttributes)
{-# DEPRECATED laLexemesCount "Use generic-lens or generic-optics with 'lexemesCount' instead." #-}

-- | Amazon Resource Name (ARN) of the lexicon.
--
-- /Note:/ Consider using 'lexiconARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laLexiconARN :: Lens.Lens' LexiconAttributes (Lude.Maybe Lude.Text)
laLexiconARN = Lens.lens (lexiconARN :: LexiconAttributes -> Lude.Maybe Lude.Text) (\s a -> s {lexiconARN = a} :: LexiconAttributes)
{-# DEPRECATED laLexiconARN "Use generic-lens or generic-optics with 'lexiconARN' instead." #-}

-- | Phonetic alphabet used in the lexicon. Valid values are @ipa@ and @x-sampa@ .
--
-- /Note:/ Consider using 'alphabet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laAlphabet :: Lens.Lens' LexiconAttributes (Lude.Maybe Lude.Text)
laAlphabet = Lens.lens (alphabet :: LexiconAttributes -> Lude.Maybe Lude.Text) (\s a -> s {alphabet = a} :: LexiconAttributes)
{-# DEPRECATED laAlphabet "Use generic-lens or generic-optics with 'alphabet' instead." #-}

-- | Date lexicon was last modified (a timestamp value).
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laLastModified :: Lens.Lens' LexiconAttributes (Lude.Maybe Lude.Timestamp)
laLastModified = Lens.lens (lastModified :: LexiconAttributes -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModified = a} :: LexiconAttributes)
{-# DEPRECATED laLastModified "Use generic-lens or generic-optics with 'lastModified' instead." #-}

instance Lude.FromJSON LexiconAttributes where
  parseJSON =
    Lude.withObject
      "LexiconAttributes"
      ( \x ->
          LexiconAttributes'
            Lude.<$> (x Lude..:? "LanguageCode")
            Lude.<*> (x Lude..:? "Size")
            Lude.<*> (x Lude..:? "LexemesCount")
            Lude.<*> (x Lude..:? "LexiconArn")
            Lude.<*> (x Lude..:? "Alphabet")
            Lude.<*> (x Lude..:? "LastModified")
      )
