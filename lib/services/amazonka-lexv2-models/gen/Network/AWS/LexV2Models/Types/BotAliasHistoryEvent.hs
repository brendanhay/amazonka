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
-- Module      : Network.AWS.LexV2Models.Types.BotAliasHistoryEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Models.Types.BotAliasHistoryEvent where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides a record of an event that affects a bot alias. For example,
-- when the version of a bot that the alias points to changes.
--
-- /See:/ 'newBotAliasHistoryEvent' smart constructor.
data BotAliasHistoryEvent = BotAliasHistoryEvent'
  { -- | The date and time that the event ended.
    endDate :: Prelude.Maybe Core.POSIX,
    -- | The version of the bot that was used in the event.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the event started.
    startDate :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BotAliasHistoryEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endDate', 'botAliasHistoryEvent_endDate' - The date and time that the event ended.
--
-- 'botVersion', 'botAliasHistoryEvent_botVersion' - The version of the bot that was used in the event.
--
-- 'startDate', 'botAliasHistoryEvent_startDate' - The date and time that the event started.
newBotAliasHistoryEvent ::
  BotAliasHistoryEvent
newBotAliasHistoryEvent =
  BotAliasHistoryEvent'
    { endDate = Prelude.Nothing,
      botVersion = Prelude.Nothing,
      startDate = Prelude.Nothing
    }

-- | The date and time that the event ended.
botAliasHistoryEvent_endDate :: Lens.Lens' BotAliasHistoryEvent (Prelude.Maybe Prelude.UTCTime)
botAliasHistoryEvent_endDate = Lens.lens (\BotAliasHistoryEvent' {endDate} -> endDate) (\s@BotAliasHistoryEvent' {} a -> s {endDate = a} :: BotAliasHistoryEvent) Prelude.. Lens.mapping Core._Time

-- | The version of the bot that was used in the event.
botAliasHistoryEvent_botVersion :: Lens.Lens' BotAliasHistoryEvent (Prelude.Maybe Prelude.Text)
botAliasHistoryEvent_botVersion = Lens.lens (\BotAliasHistoryEvent' {botVersion} -> botVersion) (\s@BotAliasHistoryEvent' {} a -> s {botVersion = a} :: BotAliasHistoryEvent)

-- | The date and time that the event started.
botAliasHistoryEvent_startDate :: Lens.Lens' BotAliasHistoryEvent (Prelude.Maybe Prelude.UTCTime)
botAliasHistoryEvent_startDate = Lens.lens (\BotAliasHistoryEvent' {startDate} -> startDate) (\s@BotAliasHistoryEvent' {} a -> s {startDate = a} :: BotAliasHistoryEvent) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON BotAliasHistoryEvent where
  parseJSON =
    Core.withObject
      "BotAliasHistoryEvent"
      ( \x ->
          BotAliasHistoryEvent'
            Prelude.<$> (x Core..:? "endDate")
            Prelude.<*> (x Core..:? "botVersion")
            Prelude.<*> (x Core..:? "startDate")
      )

instance Prelude.Hashable BotAliasHistoryEvent

instance Prelude.NFData BotAliasHistoryEvent
