{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Polly.Types.LexiconAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Polly.Types.LexiconAttributes where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Polly.Types.LanguageCode
import qualified Network.AWS.Prelude as Prelude

-- | Contains metadata describing the lexicon such as the number of lexemes,
-- language code, and so on. For more information, see
-- <https://docs.aws.amazon.com/polly/latest/dg/managing-lexicons.html Managing Lexicons>.
--
-- /See:/ 'newLexiconAttributes' smart constructor.
data LexiconAttributes = LexiconAttributes'
  { -- | Language code that the lexicon applies to. A lexicon with a language
    -- code such as \"en\" would be applied to all English languages (en-GB,
    -- en-US, en-AUS, en-WLS, and so on.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | Amazon Resource Name (ARN) of the lexicon.
    lexiconArn :: Prelude.Maybe Prelude.Text,
    -- | Phonetic alphabet used in the lexicon. Valid values are @ipa@ and
    -- @x-sampa@.
    alphabet :: Prelude.Maybe Prelude.Text,
    -- | Number of lexemes in the lexicon.
    lexemesCount :: Prelude.Maybe Prelude.Int,
    -- | Date lexicon was last modified (a timestamp value).
    lastModified :: Prelude.Maybe Prelude.POSIX,
    -- | Total size of the lexicon, in characters.
    size :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LexiconAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languageCode', 'lexiconAttributes_languageCode' - Language code that the lexicon applies to. A lexicon with a language
-- code such as \"en\" would be applied to all English languages (en-GB,
-- en-US, en-AUS, en-WLS, and so on.
--
-- 'lexiconArn', 'lexiconAttributes_lexiconArn' - Amazon Resource Name (ARN) of the lexicon.
--
-- 'alphabet', 'lexiconAttributes_alphabet' - Phonetic alphabet used in the lexicon. Valid values are @ipa@ and
-- @x-sampa@.
--
-- 'lexemesCount', 'lexiconAttributes_lexemesCount' - Number of lexemes in the lexicon.
--
-- 'lastModified', 'lexiconAttributes_lastModified' - Date lexicon was last modified (a timestamp value).
--
-- 'size', 'lexiconAttributes_size' - Total size of the lexicon, in characters.
newLexiconAttributes ::
  LexiconAttributes
newLexiconAttributes =
  LexiconAttributes'
    { languageCode = Prelude.Nothing,
      lexiconArn = Prelude.Nothing,
      alphabet = Prelude.Nothing,
      lexemesCount = Prelude.Nothing,
      lastModified = Prelude.Nothing,
      size = Prelude.Nothing
    }

-- | Language code that the lexicon applies to. A lexicon with a language
-- code such as \"en\" would be applied to all English languages (en-GB,
-- en-US, en-AUS, en-WLS, and so on.
lexiconAttributes_languageCode :: Lens.Lens' LexiconAttributes (Prelude.Maybe LanguageCode)
lexiconAttributes_languageCode = Lens.lens (\LexiconAttributes' {languageCode} -> languageCode) (\s@LexiconAttributes' {} a -> s {languageCode = a} :: LexiconAttributes)

-- | Amazon Resource Name (ARN) of the lexicon.
lexiconAttributes_lexiconArn :: Lens.Lens' LexiconAttributes (Prelude.Maybe Prelude.Text)
lexiconAttributes_lexiconArn = Lens.lens (\LexiconAttributes' {lexiconArn} -> lexiconArn) (\s@LexiconAttributes' {} a -> s {lexiconArn = a} :: LexiconAttributes)

-- | Phonetic alphabet used in the lexicon. Valid values are @ipa@ and
-- @x-sampa@.
lexiconAttributes_alphabet :: Lens.Lens' LexiconAttributes (Prelude.Maybe Prelude.Text)
lexiconAttributes_alphabet = Lens.lens (\LexiconAttributes' {alphabet} -> alphabet) (\s@LexiconAttributes' {} a -> s {alphabet = a} :: LexiconAttributes)

-- | Number of lexemes in the lexicon.
lexiconAttributes_lexemesCount :: Lens.Lens' LexiconAttributes (Prelude.Maybe Prelude.Int)
lexiconAttributes_lexemesCount = Lens.lens (\LexiconAttributes' {lexemesCount} -> lexemesCount) (\s@LexiconAttributes' {} a -> s {lexemesCount = a} :: LexiconAttributes)

-- | Date lexicon was last modified (a timestamp value).
lexiconAttributes_lastModified :: Lens.Lens' LexiconAttributes (Prelude.Maybe Prelude.UTCTime)
lexiconAttributes_lastModified = Lens.lens (\LexiconAttributes' {lastModified} -> lastModified) (\s@LexiconAttributes' {} a -> s {lastModified = a} :: LexiconAttributes) Prelude.. Lens.mapping Prelude._Time

-- | Total size of the lexicon, in characters.
lexiconAttributes_size :: Lens.Lens' LexiconAttributes (Prelude.Maybe Prelude.Int)
lexiconAttributes_size = Lens.lens (\LexiconAttributes' {size} -> size) (\s@LexiconAttributes' {} a -> s {size = a} :: LexiconAttributes)

instance Prelude.FromJSON LexiconAttributes where
  parseJSON =
    Prelude.withObject
      "LexiconAttributes"
      ( \x ->
          LexiconAttributes'
            Prelude.<$> (x Prelude..:? "LanguageCode")
            Prelude.<*> (x Prelude..:? "LexiconArn")
            Prelude.<*> (x Prelude..:? "Alphabet")
            Prelude.<*> (x Prelude..:? "LexemesCount")
            Prelude.<*> (x Prelude..:? "LastModified")
            Prelude.<*> (x Prelude..:? "Size")
      )

instance Prelude.Hashable LexiconAttributes

instance Prelude.NFData LexiconAttributes
