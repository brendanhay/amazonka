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
-- Module      : Amazonka.LexV2Models.Types.BotExportSpecification
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.BotExportSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides the identity of a the bot that was exported.
--
-- /See:/ 'newBotExportSpecification' smart constructor.
data BotExportSpecification = BotExportSpecification'
  { -- | The identifier of the bot assigned by Amazon Lex.
    botId :: Prelude.Text,
    -- | The version of the bot that was exported. This will be either @DRAFT@ or
    -- the version number.
    botVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BotExportSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botId', 'botExportSpecification_botId' - The identifier of the bot assigned by Amazon Lex.
--
-- 'botVersion', 'botExportSpecification_botVersion' - The version of the bot that was exported. This will be either @DRAFT@ or
-- the version number.
newBotExportSpecification ::
  -- | 'botId'
  Prelude.Text ->
  -- | 'botVersion'
  Prelude.Text ->
  BotExportSpecification
newBotExportSpecification pBotId_ pBotVersion_ =
  BotExportSpecification'
    { botId = pBotId_,
      botVersion = pBotVersion_
    }

-- | The identifier of the bot assigned by Amazon Lex.
botExportSpecification_botId :: Lens.Lens' BotExportSpecification Prelude.Text
botExportSpecification_botId = Lens.lens (\BotExportSpecification' {botId} -> botId) (\s@BotExportSpecification' {} a -> s {botId = a} :: BotExportSpecification)

-- | The version of the bot that was exported. This will be either @DRAFT@ or
-- the version number.
botExportSpecification_botVersion :: Lens.Lens' BotExportSpecification Prelude.Text
botExportSpecification_botVersion = Lens.lens (\BotExportSpecification' {botVersion} -> botVersion) (\s@BotExportSpecification' {} a -> s {botVersion = a} :: BotExportSpecification)

instance Core.FromJSON BotExportSpecification where
  parseJSON =
    Core.withObject
      "BotExportSpecification"
      ( \x ->
          BotExportSpecification'
            Prelude.<$> (x Core..: "botId")
            Prelude.<*> (x Core..: "botVersion")
      )

instance Prelude.Hashable BotExportSpecification where
  hashWithSalt _salt BotExportSpecification' {..} =
    _salt `Prelude.hashWithSalt` botId
      `Prelude.hashWithSalt` botVersion

instance Prelude.NFData BotExportSpecification where
  rnf BotExportSpecification' {..} =
    Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion

instance Core.ToJSON BotExportSpecification where
  toJSON BotExportSpecification' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("botId" Core..= botId),
            Prelude.Just ("botVersion" Core..= botVersion)
          ]
      )
