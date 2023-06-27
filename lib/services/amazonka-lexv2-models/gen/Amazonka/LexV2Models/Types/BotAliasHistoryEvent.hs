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
-- Module      : Amazonka.LexV2Models.Types.BotAliasHistoryEvent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.BotAliasHistoryEvent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides a record of an event that affects a bot alias. For example,
-- when the version of a bot that the alias points to changes.
--
-- /See:/ 'newBotAliasHistoryEvent' smart constructor.
data BotAliasHistoryEvent = BotAliasHistoryEvent'
  { -- | The version of the bot that was used in the event.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the event ended.
    endDate :: Prelude.Maybe Data.POSIX,
    -- | The date and time that the event started.
    startDate :: Prelude.Maybe Data.POSIX
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
-- 'botVersion', 'botAliasHistoryEvent_botVersion' - The version of the bot that was used in the event.
--
-- 'endDate', 'botAliasHistoryEvent_endDate' - The date and time that the event ended.
--
-- 'startDate', 'botAliasHistoryEvent_startDate' - The date and time that the event started.
newBotAliasHistoryEvent ::
  BotAliasHistoryEvent
newBotAliasHistoryEvent =
  BotAliasHistoryEvent'
    { botVersion = Prelude.Nothing,
      endDate = Prelude.Nothing,
      startDate = Prelude.Nothing
    }

-- | The version of the bot that was used in the event.
botAliasHistoryEvent_botVersion :: Lens.Lens' BotAliasHistoryEvent (Prelude.Maybe Prelude.Text)
botAliasHistoryEvent_botVersion = Lens.lens (\BotAliasHistoryEvent' {botVersion} -> botVersion) (\s@BotAliasHistoryEvent' {} a -> s {botVersion = a} :: BotAliasHistoryEvent)

-- | The date and time that the event ended.
botAliasHistoryEvent_endDate :: Lens.Lens' BotAliasHistoryEvent (Prelude.Maybe Prelude.UTCTime)
botAliasHistoryEvent_endDate = Lens.lens (\BotAliasHistoryEvent' {endDate} -> endDate) (\s@BotAliasHistoryEvent' {} a -> s {endDate = a} :: BotAliasHistoryEvent) Prelude.. Lens.mapping Data._Time

-- | The date and time that the event started.
botAliasHistoryEvent_startDate :: Lens.Lens' BotAliasHistoryEvent (Prelude.Maybe Prelude.UTCTime)
botAliasHistoryEvent_startDate = Lens.lens (\BotAliasHistoryEvent' {startDate} -> startDate) (\s@BotAliasHistoryEvent' {} a -> s {startDate = a} :: BotAliasHistoryEvent) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON BotAliasHistoryEvent where
  parseJSON =
    Data.withObject
      "BotAliasHistoryEvent"
      ( \x ->
          BotAliasHistoryEvent'
            Prelude.<$> (x Data..:? "botVersion")
            Prelude.<*> (x Data..:? "endDate")
            Prelude.<*> (x Data..:? "startDate")
      )

instance Prelude.Hashable BotAliasHistoryEvent where
  hashWithSalt _salt BotAliasHistoryEvent' {..} =
    _salt
      `Prelude.hashWithSalt` botVersion
      `Prelude.hashWithSalt` endDate
      `Prelude.hashWithSalt` startDate

instance Prelude.NFData BotAliasHistoryEvent where
  rnf BotAliasHistoryEvent' {..} =
    Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf endDate
      `Prelude.seq` Prelude.rnf startDate
