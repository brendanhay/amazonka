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
-- Module      : Network.AWS.LexV2Models.Types.ExportResourceSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Models.Types.ExportResourceSpecification where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Models.Types.BotExportSpecification
import Network.AWS.LexV2Models.Types.BotLocaleExportSpecification
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about the bot or bot locale that you want to
-- export. You can specify the @botExportSpecification@ or the
-- @botLocaleExportSpecification@, but not both.
--
-- /See:/ 'newExportResourceSpecification' smart constructor.
data ExportResourceSpecification = ExportResourceSpecification'
  { -- | Parameters for exporting a bot.
    botExportSpecification :: Prelude.Maybe BotExportSpecification,
    -- | Parameters for exporting a bot locale.
    botLocaleExportSpecification :: Prelude.Maybe BotLocaleExportSpecification
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
-- 'botExportSpecification', 'exportResourceSpecification_botExportSpecification' - Parameters for exporting a bot.
--
-- 'botLocaleExportSpecification', 'exportResourceSpecification_botLocaleExportSpecification' - Parameters for exporting a bot locale.
newExportResourceSpecification ::
  ExportResourceSpecification
newExportResourceSpecification =
  ExportResourceSpecification'
    { botExportSpecification =
        Prelude.Nothing,
      botLocaleExportSpecification = Prelude.Nothing
    }

-- | Parameters for exporting a bot.
exportResourceSpecification_botExportSpecification :: Lens.Lens' ExportResourceSpecification (Prelude.Maybe BotExportSpecification)
exportResourceSpecification_botExportSpecification = Lens.lens (\ExportResourceSpecification' {botExportSpecification} -> botExportSpecification) (\s@ExportResourceSpecification' {} a -> s {botExportSpecification = a} :: ExportResourceSpecification)

-- | Parameters for exporting a bot locale.
exportResourceSpecification_botLocaleExportSpecification :: Lens.Lens' ExportResourceSpecification (Prelude.Maybe BotLocaleExportSpecification)
exportResourceSpecification_botLocaleExportSpecification = Lens.lens (\ExportResourceSpecification' {botLocaleExportSpecification} -> botLocaleExportSpecification) (\s@ExportResourceSpecification' {} a -> s {botLocaleExportSpecification = a} :: ExportResourceSpecification)

instance Core.FromJSON ExportResourceSpecification where
  parseJSON =
    Core.withObject
      "ExportResourceSpecification"
      ( \x ->
          ExportResourceSpecification'
            Prelude.<$> (x Core..:? "botExportSpecification")
            Prelude.<*> (x Core..:? "botLocaleExportSpecification")
      )

instance Prelude.Hashable ExportResourceSpecification

instance Prelude.NFData ExportResourceSpecification

instance Core.ToJSON ExportResourceSpecification where
  toJSON ExportResourceSpecification' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("botExportSpecification" Core..=)
              Prelude.<$> botExportSpecification,
            ("botLocaleExportSpecification" Core..=)
              Prelude.<$> botLocaleExportSpecification
          ]
      )
