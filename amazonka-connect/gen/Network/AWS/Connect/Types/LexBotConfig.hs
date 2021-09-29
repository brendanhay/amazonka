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
-- Module      : Network.AWS.Connect.Types.LexBotConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.LexBotConfig where

import Network.AWS.Connect.Types.LexBot
import Network.AWS.Connect.Types.LexV2Bot
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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

instance Core.FromJSON LexBotConfig where
  parseJSON =
    Core.withObject
      "LexBotConfig"
      ( \x ->
          LexBotConfig'
            Prelude.<$> (x Core..:? "LexBot")
            Prelude.<*> (x Core..:? "LexV2Bot")
      )

instance Prelude.Hashable LexBotConfig

instance Prelude.NFData LexBotConfig
