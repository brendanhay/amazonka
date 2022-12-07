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
-- Module      : Amazonka.LexV2Models.Types.ExportResourceSpecification
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.ExportResourceSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.BotExportSpecification
import Amazonka.LexV2Models.Types.BotLocaleExportSpecification
import Amazonka.LexV2Models.Types.CustomVocabularyExportSpecification
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the bot or bot locale that you want to
-- export. You can specify the @botExportSpecification@ or the
-- @botLocaleExportSpecification@, but not both.
--
-- /See:/ 'newExportResourceSpecification' smart constructor.
data ExportResourceSpecification = ExportResourceSpecification'
  { -- | Parameters for exporting a bot locale.
    botLocaleExportSpecification :: Prelude.Maybe BotLocaleExportSpecification,
    -- | Parameters for exporting a bot.
    botExportSpecification :: Prelude.Maybe BotExportSpecification,
    -- | The parameters required to export a custom vocabulary.
    customVocabularyExportSpecification :: Prelude.Maybe CustomVocabularyExportSpecification
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportResourceSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botLocaleExportSpecification', 'exportResourceSpecification_botLocaleExportSpecification' - Parameters for exporting a bot locale.
--
-- 'botExportSpecification', 'exportResourceSpecification_botExportSpecification' - Parameters for exporting a bot.
--
-- 'customVocabularyExportSpecification', 'exportResourceSpecification_customVocabularyExportSpecification' - The parameters required to export a custom vocabulary.
newExportResourceSpecification ::
  ExportResourceSpecification
newExportResourceSpecification =
  ExportResourceSpecification'
    { botLocaleExportSpecification =
        Prelude.Nothing,
      botExportSpecification = Prelude.Nothing,
      customVocabularyExportSpecification =
        Prelude.Nothing
    }

-- | Parameters for exporting a bot locale.
exportResourceSpecification_botLocaleExportSpecification :: Lens.Lens' ExportResourceSpecification (Prelude.Maybe BotLocaleExportSpecification)
exportResourceSpecification_botLocaleExportSpecification = Lens.lens (\ExportResourceSpecification' {botLocaleExportSpecification} -> botLocaleExportSpecification) (\s@ExportResourceSpecification' {} a -> s {botLocaleExportSpecification = a} :: ExportResourceSpecification)

-- | Parameters for exporting a bot.
exportResourceSpecification_botExportSpecification :: Lens.Lens' ExportResourceSpecification (Prelude.Maybe BotExportSpecification)
exportResourceSpecification_botExportSpecification = Lens.lens (\ExportResourceSpecification' {botExportSpecification} -> botExportSpecification) (\s@ExportResourceSpecification' {} a -> s {botExportSpecification = a} :: ExportResourceSpecification)

-- | The parameters required to export a custom vocabulary.
exportResourceSpecification_customVocabularyExportSpecification :: Lens.Lens' ExportResourceSpecification (Prelude.Maybe CustomVocabularyExportSpecification)
exportResourceSpecification_customVocabularyExportSpecification = Lens.lens (\ExportResourceSpecification' {customVocabularyExportSpecification} -> customVocabularyExportSpecification) (\s@ExportResourceSpecification' {} a -> s {customVocabularyExportSpecification = a} :: ExportResourceSpecification)

instance Data.FromJSON ExportResourceSpecification where
  parseJSON =
    Data.withObject
      "ExportResourceSpecification"
      ( \x ->
          ExportResourceSpecification'
            Prelude.<$> (x Data..:? "botLocaleExportSpecification")
            Prelude.<*> (x Data..:? "botExportSpecification")
            Prelude.<*> (x Data..:? "customVocabularyExportSpecification")
      )

instance Prelude.Hashable ExportResourceSpecification where
  hashWithSalt _salt ExportResourceSpecification' {..} =
    _salt
      `Prelude.hashWithSalt` botLocaleExportSpecification
      `Prelude.hashWithSalt` botExportSpecification
      `Prelude.hashWithSalt` customVocabularyExportSpecification

instance Prelude.NFData ExportResourceSpecification where
  rnf ExportResourceSpecification' {..} =
    Prelude.rnf botLocaleExportSpecification
      `Prelude.seq` Prelude.rnf botExportSpecification
      `Prelude.seq` Prelude.rnf customVocabularyExportSpecification

instance Data.ToJSON ExportResourceSpecification where
  toJSON ExportResourceSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("botLocaleExportSpecification" Data..=)
              Prelude.<$> botLocaleExportSpecification,
            ("botExportSpecification" Data..=)
              Prelude.<$> botExportSpecification,
            ("customVocabularyExportSpecification" Data..=)
              Prelude.<$> customVocabularyExportSpecification
          ]
      )
