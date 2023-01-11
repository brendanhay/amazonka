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
-- Module      : Amazonka.LexV2Models.Types.CustomVocabularyImportSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.CustomVocabularyImportSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides the parameters required for importing a custom vocabulary.
--
-- /See:/ 'newCustomVocabularyImportSpecification' smart constructor.
data CustomVocabularyImportSpecification = CustomVocabularyImportSpecification'
  { -- | The identifier of the bot to import the custom vocabulary to.
    botId :: Prelude.Text,
    -- | The version of the bot to import the custom vocabulary to.
    botVersion :: Prelude.Text,
    -- | The identifier of the local to import the custom vocabulary to. The
    -- value must be @en_GB@.
    localeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomVocabularyImportSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botId', 'customVocabularyImportSpecification_botId' - The identifier of the bot to import the custom vocabulary to.
--
-- 'botVersion', 'customVocabularyImportSpecification_botVersion' - The version of the bot to import the custom vocabulary to.
--
-- 'localeId', 'customVocabularyImportSpecification_localeId' - The identifier of the local to import the custom vocabulary to. The
-- value must be @en_GB@.
newCustomVocabularyImportSpecification ::
  -- | 'botId'
  Prelude.Text ->
  -- | 'botVersion'
  Prelude.Text ->
  -- | 'localeId'
  Prelude.Text ->
  CustomVocabularyImportSpecification
newCustomVocabularyImportSpecification
  pBotId_
  pBotVersion_
  pLocaleId_ =
    CustomVocabularyImportSpecification'
      { botId =
          pBotId_,
        botVersion = pBotVersion_,
        localeId = pLocaleId_
      }

-- | The identifier of the bot to import the custom vocabulary to.
customVocabularyImportSpecification_botId :: Lens.Lens' CustomVocabularyImportSpecification Prelude.Text
customVocabularyImportSpecification_botId = Lens.lens (\CustomVocabularyImportSpecification' {botId} -> botId) (\s@CustomVocabularyImportSpecification' {} a -> s {botId = a} :: CustomVocabularyImportSpecification)

-- | The version of the bot to import the custom vocabulary to.
customVocabularyImportSpecification_botVersion :: Lens.Lens' CustomVocabularyImportSpecification Prelude.Text
customVocabularyImportSpecification_botVersion = Lens.lens (\CustomVocabularyImportSpecification' {botVersion} -> botVersion) (\s@CustomVocabularyImportSpecification' {} a -> s {botVersion = a} :: CustomVocabularyImportSpecification)

-- | The identifier of the local to import the custom vocabulary to. The
-- value must be @en_GB@.
customVocabularyImportSpecification_localeId :: Lens.Lens' CustomVocabularyImportSpecification Prelude.Text
customVocabularyImportSpecification_localeId = Lens.lens (\CustomVocabularyImportSpecification' {localeId} -> localeId) (\s@CustomVocabularyImportSpecification' {} a -> s {localeId = a} :: CustomVocabularyImportSpecification)

instance
  Data.FromJSON
    CustomVocabularyImportSpecification
  where
  parseJSON =
    Data.withObject
      "CustomVocabularyImportSpecification"
      ( \x ->
          CustomVocabularyImportSpecification'
            Prelude.<$> (x Data..: "botId")
            Prelude.<*> (x Data..: "botVersion")
            Prelude.<*> (x Data..: "localeId")
      )

instance
  Prelude.Hashable
    CustomVocabularyImportSpecification
  where
  hashWithSalt
    _salt
    CustomVocabularyImportSpecification' {..} =
      _salt `Prelude.hashWithSalt` botId
        `Prelude.hashWithSalt` botVersion
        `Prelude.hashWithSalt` localeId

instance
  Prelude.NFData
    CustomVocabularyImportSpecification
  where
  rnf CustomVocabularyImportSpecification' {..} =
    Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf localeId

instance
  Data.ToJSON
    CustomVocabularyImportSpecification
  where
  toJSON CustomVocabularyImportSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("botId" Data..= botId),
            Prelude.Just ("botVersion" Data..= botVersion),
            Prelude.Just ("localeId" Data..= localeId)
          ]
      )
