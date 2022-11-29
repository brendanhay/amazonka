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
-- Module      : Amazonka.Translate.Types.Language
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Translate.Types.Language where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A supported language.
--
-- /See:/ 'newLanguage' smart constructor.
data Language = Language'
  { -- | Language name of the supported language.
    languageName :: Prelude.Text,
    -- | Language code for the supported language.
    languageCode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Language' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languageName', 'language_languageName' - Language name of the supported language.
--
-- 'languageCode', 'language_languageCode' - Language code for the supported language.
newLanguage ::
  -- | 'languageName'
  Prelude.Text ->
  -- | 'languageCode'
  Prelude.Text ->
  Language
newLanguage pLanguageName_ pLanguageCode_ =
  Language'
    { languageName = pLanguageName_,
      languageCode = pLanguageCode_
    }

-- | Language name of the supported language.
language_languageName :: Lens.Lens' Language Prelude.Text
language_languageName = Lens.lens (\Language' {languageName} -> languageName) (\s@Language' {} a -> s {languageName = a} :: Language)

-- | Language code for the supported language.
language_languageCode :: Lens.Lens' Language Prelude.Text
language_languageCode = Lens.lens (\Language' {languageCode} -> languageCode) (\s@Language' {} a -> s {languageCode = a} :: Language)

instance Core.FromJSON Language where
  parseJSON =
    Core.withObject
      "Language"
      ( \x ->
          Language'
            Prelude.<$> (x Core..: "LanguageName")
            Prelude.<*> (x Core..: "LanguageCode")
      )

instance Prelude.Hashable Language where
  hashWithSalt _salt Language' {..} =
    _salt `Prelude.hashWithSalt` languageName
      `Prelude.hashWithSalt` languageCode

instance Prelude.NFData Language where
  rnf Language' {..} =
    Prelude.rnf languageName
      `Prelude.seq` Prelude.rnf languageCode
