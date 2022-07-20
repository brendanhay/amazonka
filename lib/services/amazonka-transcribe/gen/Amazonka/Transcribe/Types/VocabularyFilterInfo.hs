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
-- Module      : Amazonka.Transcribe.Types.VocabularyFilterInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.VocabularyFilterInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transcribe.Types.LanguageCode

-- | Provides information about a vocabulary filter.
--
-- /See:/ 'newVocabularyFilterInfo' smart constructor.
data VocabularyFilterInfo = VocabularyFilterInfo'
  { -- | The date and time that the vocabulary was last updated.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The language code of the words in the vocabulary filter.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | The name of the vocabulary filter. The name must be unique in the
    -- account that holds the filter.
    vocabularyFilterName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VocabularyFilterInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedTime', 'vocabularyFilterInfo_lastModifiedTime' - The date and time that the vocabulary was last updated.
--
-- 'languageCode', 'vocabularyFilterInfo_languageCode' - The language code of the words in the vocabulary filter.
--
-- 'vocabularyFilterName', 'vocabularyFilterInfo_vocabularyFilterName' - The name of the vocabulary filter. The name must be unique in the
-- account that holds the filter.
newVocabularyFilterInfo ::
  VocabularyFilterInfo
newVocabularyFilterInfo =
  VocabularyFilterInfo'
    { lastModifiedTime =
        Prelude.Nothing,
      languageCode = Prelude.Nothing,
      vocabularyFilterName = Prelude.Nothing
    }

-- | The date and time that the vocabulary was last updated.
vocabularyFilterInfo_lastModifiedTime :: Lens.Lens' VocabularyFilterInfo (Prelude.Maybe Prelude.UTCTime)
vocabularyFilterInfo_lastModifiedTime = Lens.lens (\VocabularyFilterInfo' {lastModifiedTime} -> lastModifiedTime) (\s@VocabularyFilterInfo' {} a -> s {lastModifiedTime = a} :: VocabularyFilterInfo) Prelude.. Lens.mapping Core._Time

-- | The language code of the words in the vocabulary filter.
vocabularyFilterInfo_languageCode :: Lens.Lens' VocabularyFilterInfo (Prelude.Maybe LanguageCode)
vocabularyFilterInfo_languageCode = Lens.lens (\VocabularyFilterInfo' {languageCode} -> languageCode) (\s@VocabularyFilterInfo' {} a -> s {languageCode = a} :: VocabularyFilterInfo)

-- | The name of the vocabulary filter. The name must be unique in the
-- account that holds the filter.
vocabularyFilterInfo_vocabularyFilterName :: Lens.Lens' VocabularyFilterInfo (Prelude.Maybe Prelude.Text)
vocabularyFilterInfo_vocabularyFilterName = Lens.lens (\VocabularyFilterInfo' {vocabularyFilterName} -> vocabularyFilterName) (\s@VocabularyFilterInfo' {} a -> s {vocabularyFilterName = a} :: VocabularyFilterInfo)

instance Core.FromJSON VocabularyFilterInfo where
  parseJSON =
    Core.withObject
      "VocabularyFilterInfo"
      ( \x ->
          VocabularyFilterInfo'
            Prelude.<$> (x Core..:? "LastModifiedTime")
            Prelude.<*> (x Core..:? "LanguageCode")
            Prelude.<*> (x Core..:? "VocabularyFilterName")
      )

instance Prelude.Hashable VocabularyFilterInfo where
  hashWithSalt _salt VocabularyFilterInfo' {..} =
    _salt `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` vocabularyFilterName

instance Prelude.NFData VocabularyFilterInfo where
  rnf VocabularyFilterInfo' {..} =
    Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf vocabularyFilterName
