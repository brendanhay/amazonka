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
-- Module      : Network.AWS.Transcribe.Types.VocabularyFilterInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.VocabularyFilterInfo where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Transcribe.Types.LanguageCode

-- | Provides information about a vocabulary filter.
--
-- /See:/ 'newVocabularyFilterInfo' smart constructor.
data VocabularyFilterInfo = VocabularyFilterInfo'
  { -- | The language code of the words in the vocabulary filter.
    languageCode :: Core.Maybe LanguageCode,
    -- | The name of the vocabulary filter. The name must be unique in the
    -- account that holds the filter.
    vocabularyFilterName :: Core.Maybe Core.Text,
    -- | The date and time that the vocabulary was last updated.
    lastModifiedTime :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'VocabularyFilterInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languageCode', 'vocabularyFilterInfo_languageCode' - The language code of the words in the vocabulary filter.
--
-- 'vocabularyFilterName', 'vocabularyFilterInfo_vocabularyFilterName' - The name of the vocabulary filter. The name must be unique in the
-- account that holds the filter.
--
-- 'lastModifiedTime', 'vocabularyFilterInfo_lastModifiedTime' - The date and time that the vocabulary was last updated.
newVocabularyFilterInfo ::
  VocabularyFilterInfo
newVocabularyFilterInfo =
  VocabularyFilterInfo'
    { languageCode = Core.Nothing,
      vocabularyFilterName = Core.Nothing,
      lastModifiedTime = Core.Nothing
    }

-- | The language code of the words in the vocabulary filter.
vocabularyFilterInfo_languageCode :: Lens.Lens' VocabularyFilterInfo (Core.Maybe LanguageCode)
vocabularyFilterInfo_languageCode = Lens.lens (\VocabularyFilterInfo' {languageCode} -> languageCode) (\s@VocabularyFilterInfo' {} a -> s {languageCode = a} :: VocabularyFilterInfo)

-- | The name of the vocabulary filter. The name must be unique in the
-- account that holds the filter.
vocabularyFilterInfo_vocabularyFilterName :: Lens.Lens' VocabularyFilterInfo (Core.Maybe Core.Text)
vocabularyFilterInfo_vocabularyFilterName = Lens.lens (\VocabularyFilterInfo' {vocabularyFilterName} -> vocabularyFilterName) (\s@VocabularyFilterInfo' {} a -> s {vocabularyFilterName = a} :: VocabularyFilterInfo)

-- | The date and time that the vocabulary was last updated.
vocabularyFilterInfo_lastModifiedTime :: Lens.Lens' VocabularyFilterInfo (Core.Maybe Core.UTCTime)
vocabularyFilterInfo_lastModifiedTime = Lens.lens (\VocabularyFilterInfo' {lastModifiedTime} -> lastModifiedTime) (\s@VocabularyFilterInfo' {} a -> s {lastModifiedTime = a} :: VocabularyFilterInfo) Core.. Lens.mapping Core._Time

instance Core.FromJSON VocabularyFilterInfo where
  parseJSON =
    Core.withObject
      "VocabularyFilterInfo"
      ( \x ->
          VocabularyFilterInfo'
            Core.<$> (x Core..:? "LanguageCode")
            Core.<*> (x Core..:? "VocabularyFilterName")
            Core.<*> (x Core..:? "LastModifiedTime")
      )

instance Core.Hashable VocabularyFilterInfo

instance Core.NFData VocabularyFilterInfo
