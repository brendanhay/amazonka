{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ElastiCache.Types.Event
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.Event where

import Network.AWS.ElastiCache.Types.SourceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents a single occurrence of something interesting within the
-- system. Some examples of events are creating a cluster, adding or
-- removing a cache node, or rebooting a node.
--
-- /See:/ 'newEvent' smart constructor.
data Event = Event'
  { -- | The text of the event.
    message :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the event occurred.
    date :: Prelude.Maybe Prelude.ISO8601,
    -- | The identifier for the source of the event. For example, if the event
    -- occurred at the cluster level, the identifier would be the name of the
    -- cluster.
    sourceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Specifies the origin of this event - a cluster, a parameter group, a
    -- security group, etc.
    sourceType :: Prelude.Maybe SourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Event' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'event_message' - The text of the event.
--
-- 'date', 'event_date' - The date and time when the event occurred.
--
-- 'sourceIdentifier', 'event_sourceIdentifier' - The identifier for the source of the event. For example, if the event
-- occurred at the cluster level, the identifier would be the name of the
-- cluster.
--
-- 'sourceType', 'event_sourceType' - Specifies the origin of this event - a cluster, a parameter group, a
-- security group, etc.
newEvent ::
  Event
newEvent =
  Event'
    { message = Prelude.Nothing,
      date = Prelude.Nothing,
      sourceIdentifier = Prelude.Nothing,
      sourceType = Prelude.Nothing
    }

-- | The text of the event.
event_message :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_message = Lens.lens (\Event' {message} -> message) (\s@Event' {} a -> s {message = a} :: Event)

-- | The date and time when the event occurred.
event_date :: Lens.Lens' Event (Prelude.Maybe Prelude.UTCTime)
event_date = Lens.lens (\Event' {date} -> date) (\s@Event' {} a -> s {date = a} :: Event) Prelude.. Lens.mapping Prelude._Time

-- | The identifier for the source of the event. For example, if the event
-- occurred at the cluster level, the identifier would be the name of the
-- cluster.
event_sourceIdentifier :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_sourceIdentifier = Lens.lens (\Event' {sourceIdentifier} -> sourceIdentifier) (\s@Event' {} a -> s {sourceIdentifier = a} :: Event)

-- | Specifies the origin of this event - a cluster, a parameter group, a
-- security group, etc.
event_sourceType :: Lens.Lens' Event (Prelude.Maybe SourceType)
event_sourceType = Lens.lens (\Event' {sourceType} -> sourceType) (\s@Event' {} a -> s {sourceType = a} :: Event)

instance Prelude.FromXML Event where
  parseXML x =
    Event'
      Prelude.<$> (x Prelude..@? "Message")
      Prelude.<*> (x Prelude..@? "Date")
      Prelude.<*> (x Prelude..@? "SourceIdentifier")
      Prelude.<*> (x Prelude..@? "SourceType")

instance Prelude.Hashable Event

instance Prelude.NFData Event
