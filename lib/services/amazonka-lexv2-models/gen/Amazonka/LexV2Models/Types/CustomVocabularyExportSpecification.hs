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
-- Module      : Amazonka.LexV2Models.Types.CustomVocabularyExportSpecification
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.CustomVocabularyExportSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides the parameters required for exporting a custom vocabulary.
--
-- /See:/ 'newCustomVocabularyExportSpecification' smart constructor.
data CustomVocabularyExportSpecification = CustomVocabularyExportSpecification'
  { -- | The identifier of the bot that contains the custom vocabulary to export.
    botId :: Prelude.Text,
    -- | The version of the bot that contains the custom vocabulary to export.
    botVersion :: Prelude.Text,
    -- | The locale of the bot that contains the custom vocabulary to export.
    localeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomVocabularyExportSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botId', 'customVocabularyExportSpecification_botId' - The identifier of the bot that contains the custom vocabulary to export.
--
-- 'botVersion', 'customVocabularyExportSpecification_botVersion' - The version of the bot that contains the custom vocabulary to export.
--
-- 'localeId', 'customVocabularyExportSpecification_localeId' - The locale of the bot that contains the custom vocabulary to export.
newCustomVocabularyExportSpecification ::
  -- | 'botId'
  Prelude.Text ->
  -- | 'botVersion'
  Prelude.Text ->
  -- | 'localeId'
  Prelude.Text ->
  CustomVocabularyExportSpecification
newCustomVocabularyExportSpecification
  pBotId_
  pBotVersion_
  pLocaleId_ =
    CustomVocabularyExportSpecification'
      { botId =
          pBotId_,
        botVersion = pBotVersion_,
        localeId = pLocaleId_
      }

-- | The identifier of the bot that contains the custom vocabulary to export.
customVocabularyExportSpecification_botId :: Lens.Lens' CustomVocabularyExportSpecification Prelude.Text
customVocabularyExportSpecification_botId = Lens.lens (\CustomVocabularyExportSpecification' {botId} -> botId) (\s@CustomVocabularyExportSpecification' {} a -> s {botId = a} :: CustomVocabularyExportSpecification)

-- | The version of the bot that contains the custom vocabulary to export.
customVocabularyExportSpecification_botVersion :: Lens.Lens' CustomVocabularyExportSpecification Prelude.Text
customVocabularyExportSpecification_botVersion = Lens.lens (\CustomVocabularyExportSpecification' {botVersion} -> botVersion) (\s@CustomVocabularyExportSpecification' {} a -> s {botVersion = a} :: CustomVocabularyExportSpecification)

-- | The locale of the bot that contains the custom vocabulary to export.
customVocabularyExportSpecification_localeId :: Lens.Lens' CustomVocabularyExportSpecification Prelude.Text
customVocabularyExportSpecification_localeId = Lens.lens (\CustomVocabularyExportSpecification' {localeId} -> localeId) (\s@CustomVocabularyExportSpecification' {} a -> s {localeId = a} :: CustomVocabularyExportSpecification)

instance
  Data.FromJSON
    CustomVocabularyExportSpecification
  where
  parseJSON =
    Data.withObject
      "CustomVocabularyExportSpecification"
      ( \x ->
          CustomVocabularyExportSpecification'
            Prelude.<$> (x Data..: "botId")
            Prelude.<*> (x Data..: "botVersion")
            Prelude.<*> (x Data..: "localeId")
      )

instance
  Prelude.Hashable
    CustomVocabularyExportSpecification
  where
  hashWithSalt
    _salt
    CustomVocabularyExportSpecification' {..} =
      _salt `Prelude.hashWithSalt` botId
        `Prelude.hashWithSalt` botVersion
        `Prelude.hashWithSalt` localeId

instance
  Prelude.NFData
    CustomVocabularyExportSpecification
  where
  rnf CustomVocabularyExportSpecification' {..} =
    Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf localeId

instance
  Data.ToJSON
    CustomVocabularyExportSpecification
  where
  toJSON CustomVocabularyExportSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("botId" Data..= botId),
            Prelude.Just ("botVersion" Data..= botVersion),
            Prelude.Just ("localeId" Data..= localeId)
          ]
      )
