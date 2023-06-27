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
-- Module      : Amazonka.LexV2Models.Types.BotAliasTestExecutionTarget
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.BotAliasTestExecutionTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The target Amazon S3 location for the test set execution using a bot
-- alias.
--
-- /See:/ 'newBotAliasTestExecutionTarget' smart constructor.
data BotAliasTestExecutionTarget = BotAliasTestExecutionTarget'
  { -- | The bot Id of the bot alias used in the test set execution.
    botId :: Prelude.Text,
    -- | The bot alias Id of the bot alias used in the test set execution.
    botAliasId :: Prelude.Text,
    -- | The locale Id of the bot alias used in the test set execution.
    localeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BotAliasTestExecutionTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botId', 'botAliasTestExecutionTarget_botId' - The bot Id of the bot alias used in the test set execution.
--
-- 'botAliasId', 'botAliasTestExecutionTarget_botAliasId' - The bot alias Id of the bot alias used in the test set execution.
--
-- 'localeId', 'botAliasTestExecutionTarget_localeId' - The locale Id of the bot alias used in the test set execution.
newBotAliasTestExecutionTarget ::
  -- | 'botId'
  Prelude.Text ->
  -- | 'botAliasId'
  Prelude.Text ->
  -- | 'localeId'
  Prelude.Text ->
  BotAliasTestExecutionTarget
newBotAliasTestExecutionTarget
  pBotId_
  pBotAliasId_
  pLocaleId_ =
    BotAliasTestExecutionTarget'
      { botId = pBotId_,
        botAliasId = pBotAliasId_,
        localeId = pLocaleId_
      }

-- | The bot Id of the bot alias used in the test set execution.
botAliasTestExecutionTarget_botId :: Lens.Lens' BotAliasTestExecutionTarget Prelude.Text
botAliasTestExecutionTarget_botId = Lens.lens (\BotAliasTestExecutionTarget' {botId} -> botId) (\s@BotAliasTestExecutionTarget' {} a -> s {botId = a} :: BotAliasTestExecutionTarget)

-- | The bot alias Id of the bot alias used in the test set execution.
botAliasTestExecutionTarget_botAliasId :: Lens.Lens' BotAliasTestExecutionTarget Prelude.Text
botAliasTestExecutionTarget_botAliasId = Lens.lens (\BotAliasTestExecutionTarget' {botAliasId} -> botAliasId) (\s@BotAliasTestExecutionTarget' {} a -> s {botAliasId = a} :: BotAliasTestExecutionTarget)

-- | The locale Id of the bot alias used in the test set execution.
botAliasTestExecutionTarget_localeId :: Lens.Lens' BotAliasTestExecutionTarget Prelude.Text
botAliasTestExecutionTarget_localeId = Lens.lens (\BotAliasTestExecutionTarget' {localeId} -> localeId) (\s@BotAliasTestExecutionTarget' {} a -> s {localeId = a} :: BotAliasTestExecutionTarget)

instance Data.FromJSON BotAliasTestExecutionTarget where
  parseJSON =
    Data.withObject
      "BotAliasTestExecutionTarget"
      ( \x ->
          BotAliasTestExecutionTarget'
            Prelude.<$> (x Data..: "botId")
            Prelude.<*> (x Data..: "botAliasId")
            Prelude.<*> (x Data..: "localeId")
      )

instance Prelude.Hashable BotAliasTestExecutionTarget where
  hashWithSalt _salt BotAliasTestExecutionTarget' {..} =
    _salt
      `Prelude.hashWithSalt` botId
      `Prelude.hashWithSalt` botAliasId
      `Prelude.hashWithSalt` localeId

instance Prelude.NFData BotAliasTestExecutionTarget where
  rnf BotAliasTestExecutionTarget' {..} =
    Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botAliasId
      `Prelude.seq` Prelude.rnf localeId

instance Data.ToJSON BotAliasTestExecutionTarget where
  toJSON BotAliasTestExecutionTarget' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("botId" Data..= botId),
            Prelude.Just ("botAliasId" Data..= botAliasId),
            Prelude.Just ("localeId" Data..= localeId)
          ]
      )
