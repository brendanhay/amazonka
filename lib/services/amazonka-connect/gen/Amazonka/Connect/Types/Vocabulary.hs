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
-- Module      : Amazonka.Connect.Types.Vocabulary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.Vocabulary where

import Amazonka.Connect.Types.VocabularyLanguageCode
import Amazonka.Connect.Types.VocabularyState
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a custom vocabulary.
--
-- /See:/ 'newVocabulary' smart constructor.
data Vocabulary = Vocabulary'
  { -- | The content of the custom vocabulary in plain-text format with a table
    -- of values. Each row in the table represents a word or a phrase,
    -- described with @Phrase@, @IPA@, @SoundsLike@, and @DisplayAs@ fields.
    -- Separate the fields with TAB characters. For more information, see
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/custom-vocabulary.html#create-vocabulary-table Create a custom vocabulary using a table>.
    content :: Prelude.Maybe Prelude.Text,
    -- | The reason why the custom vocabulary was not created.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The tags used to organize, track, or control access for this resource.
    -- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
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
-- Create a value of 'Vocabulary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'content', 'vocabulary_content' - The content of the custom vocabulary in plain-text format with a table
-- of values. Each row in the table represents a word or a phrase,
-- described with @Phrase@, @IPA@, @SoundsLike@, and @DisplayAs@ fields.
-- Separate the fields with TAB characters. For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/custom-vocabulary.html#create-vocabulary-table Create a custom vocabulary using a table>.
--
-- 'failureReason', 'vocabulary_failureReason' - The reason why the custom vocabulary was not created.
--
-- 'tags', 'vocabulary_tags' - The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
--
-- 'name', 'vocabulary_name' - A unique name of the custom vocabulary.
--
-- 'id', 'vocabulary_id' - The identifier of the custom vocabulary.
--
-- 'arn', 'vocabulary_arn' - The Amazon Resource Name (ARN) of the custom vocabulary.
--
-- 'languageCode', 'vocabulary_languageCode' - The language code of the vocabulary entries. For a list of languages and
-- their corresponding language codes, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/transcribe-whatis.html What is Amazon Transcribe?>
--
-- 'state', 'vocabulary_state' - The current state of the custom vocabulary.
--
-- 'lastModifiedTime', 'vocabulary_lastModifiedTime' - The timestamp when the custom vocabulary was last modified.
newVocabulary ::
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
  Vocabulary
newVocabulary
  pName_
  pId_
  pArn_
  pLanguageCode_
  pState_
  pLastModifiedTime_ =
    Vocabulary'
      { content = Prelude.Nothing,
        failureReason = Prelude.Nothing,
        tags = Prelude.Nothing,
        name = pName_,
        id = pId_,
        arn = pArn_,
        languageCode = pLanguageCode_,
        state = pState_,
        lastModifiedTime =
          Data._Time Lens.# pLastModifiedTime_
      }

-- | The content of the custom vocabulary in plain-text format with a table
-- of values. Each row in the table represents a word or a phrase,
-- described with @Phrase@, @IPA@, @SoundsLike@, and @DisplayAs@ fields.
-- Separate the fields with TAB characters. For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/custom-vocabulary.html#create-vocabulary-table Create a custom vocabulary using a table>.
vocabulary_content :: Lens.Lens' Vocabulary (Prelude.Maybe Prelude.Text)
vocabulary_content = Lens.lens (\Vocabulary' {content} -> content) (\s@Vocabulary' {} a -> s {content = a} :: Vocabulary)

-- | The reason why the custom vocabulary was not created.
vocabulary_failureReason :: Lens.Lens' Vocabulary (Prelude.Maybe Prelude.Text)
vocabulary_failureReason = Lens.lens (\Vocabulary' {failureReason} -> failureReason) (\s@Vocabulary' {} a -> s {failureReason = a} :: Vocabulary)

-- | The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
vocabulary_tags :: Lens.Lens' Vocabulary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
vocabulary_tags = Lens.lens (\Vocabulary' {tags} -> tags) (\s@Vocabulary' {} a -> s {tags = a} :: Vocabulary) Prelude.. Lens.mapping Lens.coerced

-- | A unique name of the custom vocabulary.
vocabulary_name :: Lens.Lens' Vocabulary Prelude.Text
vocabulary_name = Lens.lens (\Vocabulary' {name} -> name) (\s@Vocabulary' {} a -> s {name = a} :: Vocabulary)

-- | The identifier of the custom vocabulary.
vocabulary_id :: Lens.Lens' Vocabulary Prelude.Text
vocabulary_id = Lens.lens (\Vocabulary' {id} -> id) (\s@Vocabulary' {} a -> s {id = a} :: Vocabulary)

-- | The Amazon Resource Name (ARN) of the custom vocabulary.
vocabulary_arn :: Lens.Lens' Vocabulary Prelude.Text
vocabulary_arn = Lens.lens (\Vocabulary' {arn} -> arn) (\s@Vocabulary' {} a -> s {arn = a} :: Vocabulary)

-- | The language code of the vocabulary entries. For a list of languages and
-- their corresponding language codes, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/transcribe-whatis.html What is Amazon Transcribe?>
vocabulary_languageCode :: Lens.Lens' Vocabulary VocabularyLanguageCode
vocabulary_languageCode = Lens.lens (\Vocabulary' {languageCode} -> languageCode) (\s@Vocabulary' {} a -> s {languageCode = a} :: Vocabulary)

-- | The current state of the custom vocabulary.
vocabulary_state :: Lens.Lens' Vocabulary VocabularyState
vocabulary_state = Lens.lens (\Vocabulary' {state} -> state) (\s@Vocabulary' {} a -> s {state = a} :: Vocabulary)

-- | The timestamp when the custom vocabulary was last modified.
vocabulary_lastModifiedTime :: Lens.Lens' Vocabulary Prelude.UTCTime
vocabulary_lastModifiedTime = Lens.lens (\Vocabulary' {lastModifiedTime} -> lastModifiedTime) (\s@Vocabulary' {} a -> s {lastModifiedTime = a} :: Vocabulary) Prelude.. Data._Time

instance Data.FromJSON Vocabulary where
  parseJSON =
    Data.withObject
      "Vocabulary"
      ( \x ->
          Vocabulary'
            Prelude.<$> (x Data..:? "Content")
            Prelude.<*> (x Data..:? "FailureReason")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Id")
            Prelude.<*> (x Data..: "Arn")
            Prelude.<*> (x Data..: "LanguageCode")
            Prelude.<*> (x Data..: "State")
            Prelude.<*> (x Data..: "LastModifiedTime")
      )

instance Prelude.Hashable Vocabulary where
  hashWithSalt _salt Vocabulary' {..} =
    _salt
      `Prelude.hashWithSalt` content
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` lastModifiedTime

instance Prelude.NFData Vocabulary where
  rnf Vocabulary' {..} =
    Prelude.rnf content
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf lastModifiedTime
