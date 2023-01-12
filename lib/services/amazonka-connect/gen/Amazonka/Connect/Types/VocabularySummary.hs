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
-- Module      : Amazonka.Connect.Types.VocabularySummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.VocabularySummary where

import Amazonka.Connect.Types.VocabularyLanguageCode
import Amazonka.Connect.Types.VocabularyState
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains summary information about the custom vocabulary.
--
-- /See:/ 'newVocabularySummary' smart constructor.
data VocabularySummary = VocabularySummary'
  { -- | The reason why the custom vocabulary was not created.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | A unique name of the custom vocabulary.
    name :: Prelude.Text,
    -- | The identifier of the custom vocabulary.
    id :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the custom vocabulary.
    arn :: Prelude.Text,
    -- | The language code of the vocabulary entries. For a list of languages and
    -- their corresponding language codes, see
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/transcribe-whatis.html What is Amazon Transcribe?>
    languageCode :: VocabularyLanguageCode,
    -- | The current state of the custom vocabulary.
    state :: VocabularyState,
    -- | The timestamp when the custom vocabulary was last modified.
    lastModifiedTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VocabularySummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureReason', 'vocabularySummary_failureReason' - The reason why the custom vocabulary was not created.
--
-- 'name', 'vocabularySummary_name' - A unique name of the custom vocabulary.
--
-- 'id', 'vocabularySummary_id' - The identifier of the custom vocabulary.
--
-- 'arn', 'vocabularySummary_arn' - The Amazon Resource Name (ARN) of the custom vocabulary.
--
-- 'languageCode', 'vocabularySummary_languageCode' - The language code of the vocabulary entries. For a list of languages and
-- their corresponding language codes, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/transcribe-whatis.html What is Amazon Transcribe?>
--
-- 'state', 'vocabularySummary_state' - The current state of the custom vocabulary.
--
-- 'lastModifiedTime', 'vocabularySummary_lastModifiedTime' - The timestamp when the custom vocabulary was last modified.
newVocabularySummary ::
  -- | 'name'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'languageCode'
  VocabularyLanguageCode ->
  -- | 'state'
  VocabularyState ->
  -- | 'lastModifiedTime'
  Prelude.UTCTime ->
  VocabularySummary
newVocabularySummary
  pName_
  pId_
  pArn_
  pLanguageCode_
  pState_
  pLastModifiedTime_ =
    VocabularySummary'
      { failureReason = Prelude.Nothing,
        name = pName_,
        id = pId_,
        arn = pArn_,
        languageCode = pLanguageCode_,
        state = pState_,
        lastModifiedTime =
          Data._Time Lens.# pLastModifiedTime_
      }

-- | The reason why the custom vocabulary was not created.
vocabularySummary_failureReason :: Lens.Lens' VocabularySummary (Prelude.Maybe Prelude.Text)
vocabularySummary_failureReason = Lens.lens (\VocabularySummary' {failureReason} -> failureReason) (\s@VocabularySummary' {} a -> s {failureReason = a} :: VocabularySummary)

-- | A unique name of the custom vocabulary.
vocabularySummary_name :: Lens.Lens' VocabularySummary Prelude.Text
vocabularySummary_name = Lens.lens (\VocabularySummary' {name} -> name) (\s@VocabularySummary' {} a -> s {name = a} :: VocabularySummary)

-- | The identifier of the custom vocabulary.
vocabularySummary_id :: Lens.Lens' VocabularySummary Prelude.Text
vocabularySummary_id = Lens.lens (\VocabularySummary' {id} -> id) (\s@VocabularySummary' {} a -> s {id = a} :: VocabularySummary)

-- | The Amazon Resource Name (ARN) of the custom vocabulary.
vocabularySummary_arn :: Lens.Lens' VocabularySummary Prelude.Text
vocabularySummary_arn = Lens.lens (\VocabularySummary' {arn} -> arn) (\s@VocabularySummary' {} a -> s {arn = a} :: VocabularySummary)

-- | The language code of the vocabulary entries. For a list of languages and
-- their corresponding language codes, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/transcribe-whatis.html What is Amazon Transcribe?>
vocabularySummary_languageCode :: Lens.Lens' VocabularySummary VocabularyLanguageCode
vocabularySummary_languageCode = Lens.lens (\VocabularySummary' {languageCode} -> languageCode) (\s@VocabularySummary' {} a -> s {languageCode = a} :: VocabularySummary)

-- | The current state of the custom vocabulary.
vocabularySummary_state :: Lens.Lens' VocabularySummary VocabularyState
vocabularySummary_state = Lens.lens (\VocabularySummary' {state} -> state) (\s@VocabularySummary' {} a -> s {state = a} :: VocabularySummary)

-- | The timestamp when the custom vocabulary was last modified.
vocabularySummary_lastModifiedTime :: Lens.Lens' VocabularySummary Prelude.UTCTime
vocabularySummary_lastModifiedTime = Lens.lens (\VocabularySummary' {lastModifiedTime} -> lastModifiedTime) (\s@VocabularySummary' {} a -> s {lastModifiedTime = a} :: VocabularySummary) Prelude.. Data._Time

instance Data.FromJSON VocabularySummary where
  parseJSON =
    Data.withObject
      "VocabularySummary"
      ( \x ->
          VocabularySummary'
            Prelude.<$> (x Data..:? "FailureReason")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Id")
            Prelude.<*> (x Data..: "Arn")
            Prelude.<*> (x Data..: "LanguageCode")
            Prelude.<*> (x Data..: "State")
            Prelude.<*> (x Data..: "LastModifiedTime")
      )

instance Prelude.Hashable VocabularySummary where
  hashWithSalt _salt VocabularySummary' {..} =
    _salt `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` lastModifiedTime

instance Prelude.NFData VocabularySummary where
  rnf VocabularySummary' {..} =
    Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf lastModifiedTime
