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
-- Module      : Network.AWS.LexV2Models.Types.ImportResourceSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Models.Types.ImportResourceSpecification where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Models.Types.BotImportSpecification
import Network.AWS.LexV2Models.Types.BotLocaleImportSpecification
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about the bot or bot locale that you want to
-- import. You can specify the @botImportSpecification@ or the
-- @botLocaleImportSpecification@, but not both.
--
-- /See:/ 'newImportResourceSpecification' smart constructor.
data ImportResourceSpecification = ImportResourceSpecification'
  { -- | Parameters for importing a bot.
    botImportSpecification :: Prelude.Maybe BotImportSpecification,
    -- | Parameters for importing a bot locale.
    botLocaleImportSpecification :: Prelude.Maybe BotLocaleImportSpecification
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
-- 'botImportSpecification', 'importResourceSpecification_botImportSpecification' - Parameters for importing a bot.
--
-- 'botLocaleImportSpecification', 'importResourceSpecification_botLocaleImportSpecification' - Parameters for importing a bot locale.
newImportResourceSpecification ::
  ImportResourceSpecification
newImportResourceSpecification =
  ImportResourceSpecification'
    { botImportSpecification =
        Prelude.Nothing,
      botLocaleImportSpecification = Prelude.Nothing
    }

-- | Parameters for importing a bot.
importResourceSpecification_botImportSpecification :: Lens.Lens' ImportResourceSpecification (Prelude.Maybe BotImportSpecification)
importResourceSpecification_botImportSpecification = Lens.lens (\ImportResourceSpecification' {botImportSpecification} -> botImportSpecification) (\s@ImportResourceSpecification' {} a -> s {botImportSpecification = a} :: ImportResourceSpecification)

-- | Parameters for importing a bot locale.
importResourceSpecification_botLocaleImportSpecification :: Lens.Lens' ImportResourceSpecification (Prelude.Maybe BotLocaleImportSpecification)
importResourceSpecification_botLocaleImportSpecification = Lens.lens (\ImportResourceSpecification' {botLocaleImportSpecification} -> botLocaleImportSpecification) (\s@ImportResourceSpecification' {} a -> s {botLocaleImportSpecification = a} :: ImportResourceSpecification)

instance Core.FromJSON ImportResourceSpecification where
  parseJSON =
    Core.withObject
      "ImportResourceSpecification"
      ( \x ->
          ImportResourceSpecification'
            Prelude.<$> (x Core..:? "botImportSpecification")
            Prelude.<*> (x Core..:? "botLocaleImportSpecification")
      )

instance Prelude.Hashable ImportResourceSpecification

instance Prelude.NFData ImportResourceSpecification

instance Core.ToJSON ImportResourceSpecification where
  toJSON ImportResourceSpecification' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("botImportSpecification" Core..=)
              Prelude.<$> botImportSpecification,
            ("botLocaleImportSpecification" Core..=)
              Prelude.<$> botLocaleImportSpecification
          ]
      )
