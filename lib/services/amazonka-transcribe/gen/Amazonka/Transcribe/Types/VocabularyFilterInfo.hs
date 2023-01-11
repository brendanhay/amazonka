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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.VocabularyFilterInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transcribe.Types.LanguageCode

-- | Provides information about a custom vocabulary filter, including the
-- language of the filter, when it was last modified, and its name.
--
-- /See:/ 'newVocabularyFilterInfo' smart constructor.
data VocabularyFilterInfo = VocabularyFilterInfo'
  { -- | The language code that represents the language of the entries in your
    -- vocabulary filter. Each custom vocabulary filter must contain terms in
    -- only one language.
    --
    -- A custom vocabulary filter can only be used to transcribe files in the
    -- same language as the filter. For example, if you create a custom
    -- vocabulary filter using US English (@en-US@), you can only apply this
    -- filter to files that contain English audio.
    --
    -- For a list of supported languages and their associated language codes,
    -- refer to the
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/supported-languages.html Supported languages>
    -- table.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | The date and time the specified custom vocabulary filter was last
    -- modified.
    --
    -- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
    -- example, @2022-05-04T12:32:58.761000-07:00@ represents 12:32 PM UTC-7 on
    -- May 4, 2022.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | A unique name, chosen by you, for your custom vocabulary filter. This
    -- name is case sensitive, cannot contain spaces, and must be unique within
    -- an Amazon Web Services account.
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
-- 'languageCode', 'vocabularyFilterInfo_languageCode' - The language code that represents the language of the entries in your
-- vocabulary filter. Each custom vocabulary filter must contain terms in
-- only one language.
--
-- A custom vocabulary filter can only be used to transcribe files in the
-- same language as the filter. For example, if you create a custom
-- vocabulary filter using US English (@en-US@), you can only apply this
-- filter to files that contain English audio.
--
-- For a list of supported languages and their associated language codes,
-- refer to the
-- <https://docs.aws.amazon.com/transcribe/latest/dg/supported-languages.html Supported languages>
-- table.
--
-- 'lastModifiedTime', 'vocabularyFilterInfo_lastModifiedTime' - The date and time the specified custom vocabulary filter was last
-- modified.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.761000-07:00@ represents 12:32 PM UTC-7 on
-- May 4, 2022.
--
-- 'vocabularyFilterName', 'vocabularyFilterInfo_vocabularyFilterName' - A unique name, chosen by you, for your custom vocabulary filter. This
-- name is case sensitive, cannot contain spaces, and must be unique within
-- an Amazon Web Services account.
newVocabularyFilterInfo ::
  VocabularyFilterInfo
newVocabularyFilterInfo =
  VocabularyFilterInfo'
    { languageCode =
        Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      vocabularyFilterName = Prelude.Nothing
    }

-- | The language code that represents the language of the entries in your
-- vocabulary filter. Each custom vocabulary filter must contain terms in
-- only one language.
--
-- A custom vocabulary filter can only be used to transcribe files in the
-- same language as the filter. For example, if you create a custom
-- vocabulary filter using US English (@en-US@), you can only apply this
-- filter to files that contain English audio.
--
-- For a list of supported languages and their associated language codes,
-- refer to the
-- <https://docs.aws.amazon.com/transcribe/latest/dg/supported-languages.html Supported languages>
-- table.
vocabularyFilterInfo_languageCode :: Lens.Lens' VocabularyFilterInfo (Prelude.Maybe LanguageCode)
vocabularyFilterInfo_languageCode = Lens.lens (\VocabularyFilterInfo' {languageCode} -> languageCode) (\s@VocabularyFilterInfo' {} a -> s {languageCode = a} :: VocabularyFilterInfo)

-- | The date and time the specified custom vocabulary filter was last
-- modified.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.761000-07:00@ represents 12:32 PM UTC-7 on
-- May 4, 2022.
vocabularyFilterInfo_lastModifiedTime :: Lens.Lens' VocabularyFilterInfo (Prelude.Maybe Prelude.UTCTime)
vocabularyFilterInfo_lastModifiedTime = Lens.lens (\VocabularyFilterInfo' {lastModifiedTime} -> lastModifiedTime) (\s@VocabularyFilterInfo' {} a -> s {lastModifiedTime = a} :: VocabularyFilterInfo) Prelude.. Lens.mapping Data._Time

-- | A unique name, chosen by you, for your custom vocabulary filter. This
-- name is case sensitive, cannot contain spaces, and must be unique within
-- an Amazon Web Services account.
vocabularyFilterInfo_vocabularyFilterName :: Lens.Lens' VocabularyFilterInfo (Prelude.Maybe Prelude.Text)
vocabularyFilterInfo_vocabularyFilterName = Lens.lens (\VocabularyFilterInfo' {vocabularyFilterName} -> vocabularyFilterName) (\s@VocabularyFilterInfo' {} a -> s {vocabularyFilterName = a} :: VocabularyFilterInfo)

instance Data.FromJSON VocabularyFilterInfo where
  parseJSON =
    Data.withObject
      "VocabularyFilterInfo"
      ( \x ->
          VocabularyFilterInfo'
            Prelude.<$> (x Data..:? "LanguageCode")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "VocabularyFilterName")
      )

instance Prelude.Hashable VocabularyFilterInfo where
  hashWithSalt _salt VocabularyFilterInfo' {..} =
    _salt `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` vocabularyFilterName

instance Prelude.NFData VocabularyFilterInfo where
  rnf VocabularyFilterInfo' {..} =
    Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf vocabularyFilterName
