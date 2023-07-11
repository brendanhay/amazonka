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
-- Module      : Amazonka.DAX.Types.Event
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DAX.Types.Event where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DAX.Types.SourceType
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a single occurrence of something interesting within the
-- system. Some examples of events are creating a DAX cluster, adding or
-- removing a node, or rebooting a node.
--
-- /See:/ 'newEvent' smart constructor.
data Event = Event'
  { -- | The date and time when the event occurred.
    date :: Prelude.Maybe Data.POSIX,
    -- | A user-defined message associated with the event.
    message :: Prelude.Maybe Prelude.Text,
    -- | The source of the event. For example, if the event occurred at the node
    -- level, the source would be the node ID.
    sourceName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the origin of this event - a cluster, a parameter group, a
    -- node ID, etc.
    sourceType :: Prelude.Maybe SourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Event' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'date', 'event_date' - The date and time when the event occurred.
--
-- 'message', 'event_message' - A user-defined message associated with the event.
--
-- 'sourceName', 'event_sourceName' - The source of the event. For example, if the event occurred at the node
-- level, the source would be the node ID.
--
-- 'sourceType', 'event_sourceType' - Specifies the origin of this event - a cluster, a parameter group, a
-- node ID, etc.
newEvent ::
  Event
newEvent =
  Event'
    { date = Prelude.Nothing,
      message = Prelude.Nothing,
      sourceName = Prelude.Nothing,
      sourceType = Prelude.Nothing
    }

-- | The date and time when the event occurred.
event_date :: Lens.Lens' Event (Prelude.Maybe Prelude.UTCTime)
event_date = Lens.lens (\Event' {date} -> date) (\s@Event' {} a -> s {date = a} :: Event) Prelude.. Lens.mapping Data._Time

-- | A user-defined message associated with the event.
event_message :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_message = Lens.lens (\Event' {message} -> message) (\s@Event' {} a -> s {message = a} :: Event)

-- | The source of the event. For example, if the event occurred at the node
-- level, the source would be the node ID.
event_sourceName :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_sourceName = Lens.lens (\Event' {sourceName} -> sourceName) (\s@Event' {} a -> s {sourceName = a} :: Event)

-- | Specifies the origin of this event - a cluster, a parameter group, a
-- node ID, etc.
event_sourceType :: Lens.Lens' Event (Prelude.Maybe SourceType)
event_sourceType = Lens.lens (\Event' {sourceType} -> sourceType) (\s@Event' {} a -> s {sourceType = a} :: Event)

instance Data.FromJSON Event where
  parseJSON =
    Data.withObject
      "Event"
      ( \x ->
          Event'
            Prelude.<$> (x Data..:? "Date")
            Prelude.<*> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "SourceName")
            Prelude.<*> (x Data..:? "SourceType")
      )

instance Prelude.Hashable Event where
  hashWithSalt _salt Event' {..} =
    _salt
      `Prelude.hashWithSalt` date
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` sourceName
      `Prelude.hashWithSalt` sourceType

instance Prelude.NFData Event where
  rnf Event' {..} =
    Prelude.rnf date
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf sourceName
      `Prelude.seq` Prelude.rnf sourceType
