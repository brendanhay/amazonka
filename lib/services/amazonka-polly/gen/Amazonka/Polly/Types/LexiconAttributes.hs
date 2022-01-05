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
-- Module      : Amazonka.Polly.Types.LexiconAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Polly.Types.LexiconAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Polly.Types.LanguageCode
import qualified Amazonka.Prelude as Prelude

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
    -- | Total size of the lexicon, in characters.
    size :: Prelude.Maybe Prelude.Int,
    -- | Number of lexemes in the lexicon.
    lexemesCount :: Prelude.Maybe Prelude.Int,
    -- | Amazon Resource Name (ARN) of the lexicon.
    lexiconArn :: Prelude.Maybe Prelude.Text,
    -- | Phonetic alphabet used in the lexicon. Valid values are @ipa@ and
    -- @x-sampa@.
    alphabet :: Prelude.Maybe Prelude.Text,
    -- | Date lexicon was last modified (a timestamp value).
    lastModified :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'size', 'lexiconAttributes_size' - Total size of the lexicon, in characters.
--
-- 'lexemesCount', 'lexiconAttributes_lexemesCount' - Number of lexemes in the lexicon.
--
-- 'lexiconArn', 'lexiconAttributes_lexiconArn' - Amazon Resource Name (ARN) of the lexicon.
--
-- 'alphabet', 'lexiconAttributes_alphabet' - Phonetic alphabet used in the lexicon. Valid values are @ipa@ and
-- @x-sampa@.
--
-- 'lastModified', 'lexiconAttributes_lastModified' - Date lexicon was last modified (a timestamp value).
newLexiconAttributes ::
  LexiconAttributes
newLexiconAttributes =
  LexiconAttributes'
    { languageCode = Prelude.Nothing,
      size = Prelude.Nothing,
      lexemesCount = Prelude.Nothing,
      lexiconArn = Prelude.Nothing,
      alphabet = Prelude.Nothing,
      lastModified = Prelude.Nothing
    }

-- | Language code that the lexicon applies to. A lexicon with a language
-- code such as \"en\" would be applied to all English languages (en-GB,
-- en-US, en-AUS, en-WLS, and so on.
lexiconAttributes_languageCode :: Lens.Lens' LexiconAttributes (Prelude.Maybe LanguageCode)
lexiconAttributes_languageCode = Lens.lens (\LexiconAttributes' {languageCode} -> languageCode) (\s@LexiconAttributes' {} a -> s {languageCode = a} :: LexiconAttributes)

-- | Total size of the lexicon, in characters.
lexiconAttributes_size :: Lens.Lens' LexiconAttributes (Prelude.Maybe Prelude.Int)
lexiconAttributes_size = Lens.lens (\LexiconAttributes' {size} -> size) (\s@LexiconAttributes' {} a -> s {size = a} :: LexiconAttributes)

-- | Number of lexemes in the lexicon.
lexiconAttributes_lexemesCount :: Lens.Lens' LexiconAttributes (Prelude.Maybe Prelude.Int)
lexiconAttributes_lexemesCount = Lens.lens (\LexiconAttributes' {lexemesCount} -> lexemesCount) (\s@LexiconAttributes' {} a -> s {lexemesCount = a} :: LexiconAttributes)

-- | Amazon Resource Name (ARN) of the lexicon.
lexiconAttributes_lexiconArn :: Lens.Lens' LexiconAttributes (Prelude.Maybe Prelude.Text)
lexiconAttributes_lexiconArn = Lens.lens (\LexiconAttributes' {lexiconArn} -> lexiconArn) (\s@LexiconAttributes' {} a -> s {lexiconArn = a} :: LexiconAttributes)

-- | Phonetic alphabet used in the lexicon. Valid values are @ipa@ and
-- @x-sampa@.
lexiconAttributes_alphabet :: Lens.Lens' LexiconAttributes (Prelude.Maybe Prelude.Text)
lexiconAttributes_alphabet = Lens.lens (\LexiconAttributes' {alphabet} -> alphabet) (\s@LexiconAttributes' {} a -> s {alphabet = a} :: LexiconAttributes)

-- | Date lexicon was last modified (a timestamp value).
lexiconAttributes_lastModified :: Lens.Lens' LexiconAttributes (Prelude.Maybe Prelude.UTCTime)
lexiconAttributes_lastModified = Lens.lens (\LexiconAttributes' {lastModified} -> lastModified) (\s@LexiconAttributes' {} a -> s {lastModified = a} :: LexiconAttributes) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON LexiconAttributes where
  parseJSON =
    Core.withObject
      "LexiconAttributes"
      ( \x ->
          LexiconAttributes'
            Prelude.<$> (x Core..:? "LanguageCode")
            Prelude.<*> (x Core..:? "Size")
            Prelude.<*> (x Core..:? "LexemesCount")
            Prelude.<*> (x Core..:? "LexiconArn")
            Prelude.<*> (x Core..:? "Alphabet")
            Prelude.<*> (x Core..:? "LastModified")
      )

instance Prelude.Hashable LexiconAttributes where
  hashWithSalt _salt LexiconAttributes' {..} =
    _salt `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` size
      `Prelude.hashWithSalt` lexemesCount
      `Prelude.hashWithSalt` lexiconArn
      `Prelude.hashWithSalt` alphabet
      `Prelude.hashWithSalt` lastModified

instance Prelude.NFData LexiconAttributes where
  rnf LexiconAttributes' {..} =
    Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf size
      `Prelude.seq` Prelude.rnf lexemesCount
      `Prelude.seq` Prelude.rnf lexiconArn
      `Prelude.seq` Prelude.rnf alphabet
      `Prelude.seq` Prelude.rnf lastModified
