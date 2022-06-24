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
-- Module      : Amazonka.Chime.Types.EngineTranscribeSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.EngineTranscribeSettings where

import Amazonka.Chime.Types.TranscribeLanguageCode
import Amazonka.Chime.Types.TranscribeRegion
import Amazonka.Chime.Types.TranscribeVocabularyFilterMethod
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Settings specific to the Amazon Transcribe engine.
--
-- /See:/ 'newEngineTranscribeSettings' smart constructor.
data EngineTranscribeSettings = EngineTranscribeSettings'
  { -- | The filtering method passed to Amazon Transcribe.
    vocabularyFilterMethod :: Prelude.Maybe TranscribeVocabularyFilterMethod,
    -- | The name of the vocabulary passed to Amazon Transcribe.
    vocabularyName :: Prelude.Maybe Prelude.Text,
    -- | The AWS Region passed to Amazon Transcribe. If you don\'t specify a
    -- Region, Amazon Chime uses the meeting\'s Region.
    region :: Prelude.Maybe TranscribeRegion,
    -- | The name of the vocabulary filter passed to Amazon Transcribe.
    vocabularyFilterName :: Prelude.Maybe Prelude.Text,
    -- | The language code specified for the Amazon Transcribe engine.
    languageCode :: TranscribeLanguageCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EngineTranscribeSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vocabularyFilterMethod', 'engineTranscribeSettings_vocabularyFilterMethod' - The filtering method passed to Amazon Transcribe.
--
-- 'vocabularyName', 'engineTranscribeSettings_vocabularyName' - The name of the vocabulary passed to Amazon Transcribe.
--
-- 'region', 'engineTranscribeSettings_region' - The AWS Region passed to Amazon Transcribe. If you don\'t specify a
-- Region, Amazon Chime uses the meeting\'s Region.
--
-- 'vocabularyFilterName', 'engineTranscribeSettings_vocabularyFilterName' - The name of the vocabulary filter passed to Amazon Transcribe.
--
-- 'languageCode', 'engineTranscribeSettings_languageCode' - The language code specified for the Amazon Transcribe engine.
newEngineTranscribeSettings ::
  -- | 'languageCode'
  TranscribeLanguageCode ->
  EngineTranscribeSettings
newEngineTranscribeSettings pLanguageCode_ =
  EngineTranscribeSettings'
    { vocabularyFilterMethod =
        Prelude.Nothing,
      vocabularyName = Prelude.Nothing,
      region = Prelude.Nothing,
      vocabularyFilterName = Prelude.Nothing,
      languageCode = pLanguageCode_
    }

-- | The filtering method passed to Amazon Transcribe.
engineTranscribeSettings_vocabularyFilterMethod :: Lens.Lens' EngineTranscribeSettings (Prelude.Maybe TranscribeVocabularyFilterMethod)
engineTranscribeSettings_vocabularyFilterMethod = Lens.lens (\EngineTranscribeSettings' {vocabularyFilterMethod} -> vocabularyFilterMethod) (\s@EngineTranscribeSettings' {} a -> s {vocabularyFilterMethod = a} :: EngineTranscribeSettings)

-- | The name of the vocabulary passed to Amazon Transcribe.
engineTranscribeSettings_vocabularyName :: Lens.Lens' EngineTranscribeSettings (Prelude.Maybe Prelude.Text)
engineTranscribeSettings_vocabularyName = Lens.lens (\EngineTranscribeSettings' {vocabularyName} -> vocabularyName) (\s@EngineTranscribeSettings' {} a -> s {vocabularyName = a} :: EngineTranscribeSettings)

-- | The AWS Region passed to Amazon Transcribe. If you don\'t specify a
-- Region, Amazon Chime uses the meeting\'s Region.
engineTranscribeSettings_region :: Lens.Lens' EngineTranscribeSettings (Prelude.Maybe TranscribeRegion)
engineTranscribeSettings_region = Lens.lens (\EngineTranscribeSettings' {region} -> region) (\s@EngineTranscribeSettings' {} a -> s {region = a} :: EngineTranscribeSettings)

-- | The name of the vocabulary filter passed to Amazon Transcribe.
engineTranscribeSettings_vocabularyFilterName :: Lens.Lens' EngineTranscribeSettings (Prelude.Maybe Prelude.Text)
engineTranscribeSettings_vocabularyFilterName = Lens.lens (\EngineTranscribeSettings' {vocabularyFilterName} -> vocabularyFilterName) (\s@EngineTranscribeSettings' {} a -> s {vocabularyFilterName = a} :: EngineTranscribeSettings)

-- | The language code specified for the Amazon Transcribe engine.
engineTranscribeSettings_languageCode :: Lens.Lens' EngineTranscribeSettings TranscribeLanguageCode
engineTranscribeSettings_languageCode = Lens.lens (\EngineTranscribeSettings' {languageCode} -> languageCode) (\s@EngineTranscribeSettings' {} a -> s {languageCode = a} :: EngineTranscribeSettings)

instance Prelude.Hashable EngineTranscribeSettings where
  hashWithSalt _salt EngineTranscribeSettings' {..} =
    _salt `Prelude.hashWithSalt` vocabularyFilterMethod
      `Prelude.hashWithSalt` vocabularyName
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` vocabularyFilterName
      `Prelude.hashWithSalt` languageCode

instance Prelude.NFData EngineTranscribeSettings where
  rnf EngineTranscribeSettings' {..} =
    Prelude.rnf vocabularyFilterMethod
      `Prelude.seq` Prelude.rnf vocabularyName
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf vocabularyFilterName
      `Prelude.seq` Prelude.rnf languageCode

instance Core.ToJSON EngineTranscribeSettings where
  toJSON EngineTranscribeSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("VocabularyFilterMethod" Core..=)
              Prelude.<$> vocabularyFilterMethod,
            ("VocabularyName" Core..=)
              Prelude.<$> vocabularyName,
            ("Region" Core..=) Prelude.<$> region,
            ("VocabularyFilterName" Core..=)
              Prelude.<$> vocabularyFilterName,
            Prelude.Just ("LanguageCode" Core..= languageCode)
          ]
      )
