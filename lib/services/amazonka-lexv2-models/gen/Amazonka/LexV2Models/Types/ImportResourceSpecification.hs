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
-- Module      : Amazonka.LexV2Models.Types.ImportResourceSpecification
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.ImportResourceSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LexV2Models.Types.BotImportSpecification
import Amazonka.LexV2Models.Types.BotLocaleImportSpecification
import Amazonka.LexV2Models.Types.CustomVocabularyImportSpecification
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the bot or bot locale that you want to
-- import. You can specify the @botImportSpecification@ or the
-- @botLocaleImportSpecification@, but not both.
--
-- /See:/ 'newImportResourceSpecification' smart constructor.
data ImportResourceSpecification = ImportResourceSpecification'
  { -- | Parameters for importing a bot locale.
    botLocaleImportSpecification :: Prelude.Maybe BotLocaleImportSpecification,
    -- | Parameters for importing a bot.
    botImportSpecification :: Prelude.Maybe BotImportSpecification,
    customVocabularyImportSpecification :: Prelude.Maybe CustomVocabularyImportSpecification
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportResourceSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botLocaleImportSpecification', 'importResourceSpecification_botLocaleImportSpecification' - Parameters for importing a bot locale.
--
-- 'botImportSpecification', 'importResourceSpecification_botImportSpecification' - Parameters for importing a bot.
--
-- 'customVocabularyImportSpecification', 'importResourceSpecification_customVocabularyImportSpecification' - Undocumented member.
newImportResourceSpecification ::
  ImportResourceSpecification
newImportResourceSpecification =
  ImportResourceSpecification'
    { botLocaleImportSpecification =
        Prelude.Nothing,
      botImportSpecification = Prelude.Nothing,
      customVocabularyImportSpecification =
        Prelude.Nothing
    }

-- | Parameters for importing a bot locale.
importResourceSpecification_botLocaleImportSpecification :: Lens.Lens' ImportResourceSpecification (Prelude.Maybe BotLocaleImportSpecification)
importResourceSpecification_botLocaleImportSpecification = Lens.lens (\ImportResourceSpecification' {botLocaleImportSpecification} -> botLocaleImportSpecification) (\s@ImportResourceSpecification' {} a -> s {botLocaleImportSpecification = a} :: ImportResourceSpecification)

-- | Parameters for importing a bot.
importResourceSpecification_botImportSpecification :: Lens.Lens' ImportResourceSpecification (Prelude.Maybe BotImportSpecification)
importResourceSpecification_botImportSpecification = Lens.lens (\ImportResourceSpecification' {botImportSpecification} -> botImportSpecification) (\s@ImportResourceSpecification' {} a -> s {botImportSpecification = a} :: ImportResourceSpecification)

-- | Undocumented member.
importResourceSpecification_customVocabularyImportSpecification :: Lens.Lens' ImportResourceSpecification (Prelude.Maybe CustomVocabularyImportSpecification)
importResourceSpecification_customVocabularyImportSpecification = Lens.lens (\ImportResourceSpecification' {customVocabularyImportSpecification} -> customVocabularyImportSpecification) (\s@ImportResourceSpecification' {} a -> s {customVocabularyImportSpecification = a} :: ImportResourceSpecification)

instance Core.FromJSON ImportResourceSpecification where
  parseJSON =
    Core.withObject
      "ImportResourceSpecification"
      ( \x ->
          ImportResourceSpecification'
            Prelude.<$> (x Core..:? "botLocaleImportSpecification")
            Prelude.<*> (x Core..:? "botImportSpecification")
            Prelude.<*> (x Core..:? "customVocabularyImportSpecification")
      )

instance Prelude.Hashable ImportResourceSpecification where
  hashWithSalt _salt ImportResourceSpecification' {..} =
    _salt
      `Prelude.hashWithSalt` botLocaleImportSpecification
      `Prelude.hashWithSalt` botImportSpecification
      `Prelude.hashWithSalt` customVocabularyImportSpecification

instance Prelude.NFData ImportResourceSpecification where
  rnf ImportResourceSpecification' {..} =
    Prelude.rnf botLocaleImportSpecification
      `Prelude.seq` Prelude.rnf botImportSpecification
      `Prelude.seq` Prelude.rnf customVocabularyImportSpecification

instance Core.ToJSON ImportResourceSpecification where
  toJSON ImportResourceSpecification' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("botLocaleImportSpecification" Core..=)
              Prelude.<$> botLocaleImportSpecification,
            ("botImportSpecification" Core..=)
              Prelude.<$> botImportSpecification,
            ("customVocabularyImportSpecification" Core..=)
              Prelude.<$> customVocabularyImportSpecification
          ]
      )
