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
-- Module      : Amazonka.Connect.Types.LexBotConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.LexBotConfig where

import Amazonka.Connect.Types.LexBot
import Amazonka.Connect.Types.LexV2Bot
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configuration information of an Amazon Lex or Amazon Lex V2 bot.
--
-- /See:/ 'newLexBotConfig' smart constructor.
data LexBotConfig = LexBotConfig'
  { lexBot :: Prelude.Maybe LexBot,
    -- | Configuration information of an Amazon Lex V2 bot.
    lexV2Bot :: Prelude.Maybe LexV2Bot
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LexBotConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lexBot', 'lexBotConfig_lexBot' - Undocumented member.
--
-- 'lexV2Bot', 'lexBotConfig_lexV2Bot' - Configuration information of an Amazon Lex V2 bot.
newLexBotConfig ::
  LexBotConfig
newLexBotConfig =
  LexBotConfig'
    { lexBot = Prelude.Nothing,
      lexV2Bot = Prelude.Nothing
    }

-- | Undocumented member.
lexBotConfig_lexBot :: Lens.Lens' LexBotConfig (Prelude.Maybe LexBot)
lexBotConfig_lexBot = Lens.lens (\LexBotConfig' {lexBot} -> lexBot) (\s@LexBotConfig' {} a -> s {lexBot = a} :: LexBotConfig)

-- | Configuration information of an Amazon Lex V2 bot.
lexBotConfig_lexV2Bot :: Lens.Lens' LexBotConfig (Prelude.Maybe LexV2Bot)
lexBotConfig_lexV2Bot = Lens.lens (\LexBotConfig' {lexV2Bot} -> lexV2Bot) (\s@LexBotConfig' {} a -> s {lexV2Bot = a} :: LexBotConfig)

instance Data.FromJSON LexBotConfig where
  parseJSON =
    Data.withObject
      "LexBotConfig"
      ( \x ->
          LexBotConfig'
            Prelude.<$> (x Data..:? "LexBot")
            Prelude.<*> (x Data..:? "LexV2Bot")
      )

instance Prelude.Hashable LexBotConfig where
  hashWithSalt _salt LexBotConfig' {..} =
    _salt `Prelude.hashWithSalt` lexBot
      `Prelude.hashWithSalt` lexV2Bot

instance Prelude.NFData LexBotConfig where
  rnf LexBotConfig' {..} =
    Prelude.rnf lexBot
      `Prelude.seq` Prelude.rnf lexV2Bot
