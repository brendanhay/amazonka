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
-- Module      : Amazonka.LexV2Models.Types.BotLocaleExportSpecification
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.BotLocaleExportSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides the bot locale parameters required for exporting a bot locale.
--
-- /See:/ 'newBotLocaleExportSpecification' smart constructor.
data BotLocaleExportSpecification = BotLocaleExportSpecification'
  { -- | The identifier of the bot to create the locale for.
    botId :: Prelude.Text,
    -- | The version of the bot to export.
    botVersion :: Prelude.Text,
    -- | The identifier of the language and locale to export. The string must
    -- match one of the locales in the bot.
    localeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BotLocaleExportSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botId', 'botLocaleExportSpecification_botId' - The identifier of the bot to create the locale for.
--
-- 'botVersion', 'botLocaleExportSpecification_botVersion' - The version of the bot to export.
--
-- 'localeId', 'botLocaleExportSpecification_localeId' - The identifier of the language and locale to export. The string must
-- match one of the locales in the bot.
newBotLocaleExportSpecification ::
  -- | 'botId'
  Prelude.Text ->
  -- | 'botVersion'
  Prelude.Text ->
  -- | 'localeId'
  Prelude.Text ->
  BotLocaleExportSpecification
newBotLocaleExportSpecification
  pBotId_
  pBotVersion_
  pLocaleId_ =
    BotLocaleExportSpecification'
      { botId = pBotId_,
        botVersion = pBotVersion_,
        localeId = pLocaleId_
      }

-- | The identifier of the bot to create the locale for.
botLocaleExportSpecification_botId :: Lens.Lens' BotLocaleExportSpecification Prelude.Text
botLocaleExportSpecification_botId = Lens.lens (\BotLocaleExportSpecification' {botId} -> botId) (\s@BotLocaleExportSpecification' {} a -> s {botId = a} :: BotLocaleExportSpecification)

-- | The version of the bot to export.
botLocaleExportSpecification_botVersion :: Lens.Lens' BotLocaleExportSpecification Prelude.Text
botLocaleExportSpecification_botVersion = Lens.lens (\BotLocaleExportSpecification' {botVersion} -> botVersion) (\s@BotLocaleExportSpecification' {} a -> s {botVersion = a} :: BotLocaleExportSpecification)

-- | The identifier of the language and locale to export. The string must
-- match one of the locales in the bot.
botLocaleExportSpecification_localeId :: Lens.Lens' BotLocaleExportSpecification Prelude.Text
botLocaleExportSpecification_localeId = Lens.lens (\BotLocaleExportSpecification' {localeId} -> localeId) (\s@BotLocaleExportSpecification' {} a -> s {localeId = a} :: BotLocaleExportSpecification)

instance Data.FromJSON BotLocaleExportSpecification where
  parseJSON =
    Data.withObject
      "BotLocaleExportSpecification"
      ( \x ->
          BotLocaleExportSpecification'
            Prelude.<$> (x Data..: "botId")
            Prelude.<*> (x Data..: "botVersion")
            Prelude.<*> (x Data..: "localeId")
      )

instance
  Prelude.Hashable
    BotLocaleExportSpecification
  where
  hashWithSalt _salt BotLocaleExportSpecification' {..} =
    _salt `Prelude.hashWithSalt` botId
      `Prelude.hashWithSalt` botVersion
      `Prelude.hashWithSalt` localeId

instance Prelude.NFData BotLocaleExportSpecification where
  rnf BotLocaleExportSpecification' {..} =
    Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf localeId

instance Data.ToJSON BotLocaleExportSpecification where
  toJSON BotLocaleExportSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("botId" Data..= botId),
            Prelude.Just ("botVersion" Data..= botVersion),
            Prelude.Just ("localeId" Data..= localeId)
          ]
      )
