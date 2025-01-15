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
-- Module      : Amazonka.LexV2Models.Types.BotLocaleHistoryEvent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.BotLocaleHistoryEvent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about an event that occurred affecting the bot
-- locale.
--
-- /See:/ 'newBotLocaleHistoryEvent' smart constructor.
data BotLocaleHistoryEvent = BotLocaleHistoryEvent'
  { -- | A description of the event that occurred.
    event :: Prelude.Text,
    -- | A timestamp of the date and time that the event occurred.
    eventDate :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BotLocaleHistoryEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'event', 'botLocaleHistoryEvent_event' - A description of the event that occurred.
--
-- 'eventDate', 'botLocaleHistoryEvent_eventDate' - A timestamp of the date and time that the event occurred.
newBotLocaleHistoryEvent ::
  -- | 'event'
  Prelude.Text ->
  -- | 'eventDate'
  Prelude.UTCTime ->
  BotLocaleHistoryEvent
newBotLocaleHistoryEvent pEvent_ pEventDate_ =
  BotLocaleHistoryEvent'
    { event = pEvent_,
      eventDate = Data._Time Lens.# pEventDate_
    }

-- | A description of the event that occurred.
botLocaleHistoryEvent_event :: Lens.Lens' BotLocaleHistoryEvent Prelude.Text
botLocaleHistoryEvent_event = Lens.lens (\BotLocaleHistoryEvent' {event} -> event) (\s@BotLocaleHistoryEvent' {} a -> s {event = a} :: BotLocaleHistoryEvent)

-- | A timestamp of the date and time that the event occurred.
botLocaleHistoryEvent_eventDate :: Lens.Lens' BotLocaleHistoryEvent Prelude.UTCTime
botLocaleHistoryEvent_eventDate = Lens.lens (\BotLocaleHistoryEvent' {eventDate} -> eventDate) (\s@BotLocaleHistoryEvent' {} a -> s {eventDate = a} :: BotLocaleHistoryEvent) Prelude.. Data._Time

instance Data.FromJSON BotLocaleHistoryEvent where
  parseJSON =
    Data.withObject
      "BotLocaleHistoryEvent"
      ( \x ->
          BotLocaleHistoryEvent'
            Prelude.<$> (x Data..: "event")
            Prelude.<*> (x Data..: "eventDate")
      )

instance Prelude.Hashable BotLocaleHistoryEvent where
  hashWithSalt _salt BotLocaleHistoryEvent' {..} =
    _salt
      `Prelude.hashWithSalt` event
      `Prelude.hashWithSalt` eventDate

instance Prelude.NFData BotLocaleHistoryEvent where
  rnf BotLocaleHistoryEvent' {..} =
    Prelude.rnf event `Prelude.seq`
      Prelude.rnf eventDate
