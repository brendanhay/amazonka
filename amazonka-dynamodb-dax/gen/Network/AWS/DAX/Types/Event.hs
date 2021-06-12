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
-- Module      : Network.AWS.DAX.Types.Event
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types.Event where

import qualified Network.AWS.Core as Core
import Network.AWS.DAX.Types.SourceType
import qualified Network.AWS.Lens as Lens

-- | Represents a single occurrence of something interesting within the
-- system. Some examples of events are creating a DAX cluster, adding or
-- removing a node, or rebooting a node.
--
-- /See:/ 'newEvent' smart constructor.
data Event = Event'
  { -- | A user-defined message associated with the event.
    message :: Core.Maybe Core.Text,
    -- | The source of the event. For example, if the event occurred at the node
    -- level, the source would be the node ID.
    sourceName :: Core.Maybe Core.Text,
    -- | The date and time when the event occurred.
    date :: Core.Maybe Core.POSIX,
    -- | Specifies the origin of this event - a cluster, a parameter group, a
    -- node ID, etc.
    sourceType :: Core.Maybe SourceType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Event' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'event_message' - A user-defined message associated with the event.
--
-- 'sourceName', 'event_sourceName' - The source of the event. For example, if the event occurred at the node
-- level, the source would be the node ID.
--
-- 'date', 'event_date' - The date and time when the event occurred.
--
-- 'sourceType', 'event_sourceType' - Specifies the origin of this event - a cluster, a parameter group, a
-- node ID, etc.
newEvent ::
  Event
newEvent =
  Event'
    { message = Core.Nothing,
      sourceName = Core.Nothing,
      date = Core.Nothing,
      sourceType = Core.Nothing
    }

-- | A user-defined message associated with the event.
event_message :: Lens.Lens' Event (Core.Maybe Core.Text)
event_message = Lens.lens (\Event' {message} -> message) (\s@Event' {} a -> s {message = a} :: Event)

-- | The source of the event. For example, if the event occurred at the node
-- level, the source would be the node ID.
event_sourceName :: Lens.Lens' Event (Core.Maybe Core.Text)
event_sourceName = Lens.lens (\Event' {sourceName} -> sourceName) (\s@Event' {} a -> s {sourceName = a} :: Event)

-- | The date and time when the event occurred.
event_date :: Lens.Lens' Event (Core.Maybe Core.UTCTime)
event_date = Lens.lens (\Event' {date} -> date) (\s@Event' {} a -> s {date = a} :: Event) Core.. Lens.mapping Core._Time

-- | Specifies the origin of this event - a cluster, a parameter group, a
-- node ID, etc.
event_sourceType :: Lens.Lens' Event (Core.Maybe SourceType)
event_sourceType = Lens.lens (\Event' {sourceType} -> sourceType) (\s@Event' {} a -> s {sourceType = a} :: Event)

instance Core.FromJSON Event where
  parseJSON =
    Core.withObject
      "Event"
      ( \x ->
          Event'
            Core.<$> (x Core..:? "Message")
            Core.<*> (x Core..:? "SourceName")
            Core.<*> (x Core..:? "Date")
            Core.<*> (x Core..:? "SourceType")
      )

instance Core.Hashable Event

instance Core.NFData Event
