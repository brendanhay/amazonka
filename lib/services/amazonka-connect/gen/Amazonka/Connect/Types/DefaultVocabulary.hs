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
-- Module      : Amazonka.Connect.Types.DefaultVocabulary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.DefaultVocabulary where

import Amazonka.Connect.Types.VocabularyLanguageCode
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a default vocabulary.
--
-- /See:/ 'newDefaultVocabulary' smart constructor.
data DefaultVocabulary = DefaultVocabulary'
  { -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The language code of the vocabulary entries. For a list of languages and
    -- their corresponding language codes, see
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/transcribe-whatis.html What is Amazon Transcribe?>
    languageCode :: VocabularyLanguageCode,
    -- | The identifier of the custom vocabulary.
    vocabularyId :: Prelude.Text,
    -- | A unique name of the custom vocabulary.
    vocabularyName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DefaultVocabulary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'defaultVocabulary_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'languageCode', 'defaultVocabulary_languageCode' - The language code of the vocabulary entries. For a list of languages and
-- their corresponding language codes, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/transcribe-whatis.html What is Amazon Transcribe?>
--
-- 'vocabularyId', 'defaultVocabulary_vocabularyId' - The identifier of the custom vocabulary.
--
-- 'vocabularyName', 'defaultVocabulary_vocabularyName' - A unique name of the custom vocabulary.
newDefaultVocabulary ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'languageCode'
  VocabularyLanguageCode ->
  -- | 'vocabularyId'
  Prelude.Text ->
  -- | 'vocabularyName'
  Prelude.Text ->
  DefaultVocabulary
newDefaultVocabulary
  pInstanceId_
  pLanguageCode_
  pVocabularyId_
  pVocabularyName_ =
    DefaultVocabulary'
      { instanceId = pInstanceId_,
        languageCode = pLanguageCode_,
        vocabularyId = pVocabularyId_,
        vocabularyName = pVocabularyName_
      }

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
defaultVocabulary_instanceId :: Lens.Lens' DefaultVocabulary Prelude.Text
defaultVocabulary_instanceId = Lens.lens (\DefaultVocabulary' {instanceId} -> instanceId) (\s@DefaultVocabulary' {} a -> s {instanceId = a} :: DefaultVocabulary)

-- | The language code of the vocabulary entries. For a list of languages and
-- their corresponding language codes, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/transcribe-whatis.html What is Amazon Transcribe?>
defaultVocabulary_languageCode :: Lens.Lens' DefaultVocabulary VocabularyLanguageCode
defaultVocabulary_languageCode = Lens.lens (\DefaultVocabulary' {languageCode} -> languageCode) (\s@DefaultVocabulary' {} a -> s {languageCode = a} :: DefaultVocabulary)

-- | The identifier of the custom vocabulary.
defaultVocabulary_vocabularyId :: Lens.Lens' DefaultVocabulary Prelude.Text
defaultVocabulary_vocabularyId = Lens.lens (\DefaultVocabulary' {vocabularyId} -> vocabularyId) (\s@DefaultVocabulary' {} a -> s {vocabularyId = a} :: DefaultVocabulary)

-- | A unique name of the custom vocabulary.
defaultVocabulary_vocabularyName :: Lens.Lens' DefaultVocabulary Prelude.Text
defaultVocabulary_vocabularyName = Lens.lens (\DefaultVocabulary' {vocabularyName} -> vocabularyName) (\s@DefaultVocabulary' {} a -> s {vocabularyName = a} :: DefaultVocabulary)

instance Data.FromJSON DefaultVocabulary where
  parseJSON =
    Data.withObject
      "DefaultVocabulary"
      ( \x ->
          DefaultVocabulary'
            Prelude.<$> (x Data..: "InstanceId")
            Prelude.<*> (x Data..: "LanguageCode")
            Prelude.<*> (x Data..: "VocabularyId")
            Prelude.<*> (x Data..: "VocabularyName")
      )

instance Prelude.Hashable DefaultVocabulary where
  hashWithSalt _salt DefaultVocabulary' {..} =
    _salt `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` vocabularyId
      `Prelude.hashWithSalt` vocabularyName

instance Prelude.NFData DefaultVocabulary where
  rnf DefaultVocabulary' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf vocabularyId
      `Prelude.seq` Prelude.rnf vocabularyName
