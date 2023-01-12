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
-- Module      : Amazonka.Proton.Types.ResourceSyncEvent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.ResourceSyncEvent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Detail data for a resource sync event.
--
-- /See:/ 'newResourceSyncEvent' smart constructor.
data ResourceSyncEvent = ResourceSyncEvent'
  { -- | The external ID for the event.
    externalId :: Prelude.Maybe Prelude.Text,
    -- | A resource sync event.
    event :: Prelude.Text,
    -- | The time when the event occurred.
    time :: Data.POSIX,
    -- | The type of event.
    type' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceSyncEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'externalId', 'resourceSyncEvent_externalId' - The external ID for the event.
--
-- 'event', 'resourceSyncEvent_event' - A resource sync event.
--
-- 'time', 'resourceSyncEvent_time' - The time when the event occurred.
--
-- 'type'', 'resourceSyncEvent_type' - The type of event.
newResourceSyncEvent ::
  -- | 'event'
  Prelude.Text ->
  -- | 'time'
  Prelude.UTCTime ->
  -- | 'type''
  Prelude.Text ->
  ResourceSyncEvent
newResourceSyncEvent pEvent_ pTime_ pType_ =
  ResourceSyncEvent'
    { externalId = Prelude.Nothing,
      event = pEvent_,
      time = Data._Time Lens.# pTime_,
      type' = pType_
    }

-- | The external ID for the event.
resourceSyncEvent_externalId :: Lens.Lens' ResourceSyncEvent (Prelude.Maybe Prelude.Text)
resourceSyncEvent_externalId = Lens.lens (\ResourceSyncEvent' {externalId} -> externalId) (\s@ResourceSyncEvent' {} a -> s {externalId = a} :: ResourceSyncEvent)

-- | A resource sync event.
resourceSyncEvent_event :: Lens.Lens' ResourceSyncEvent Prelude.Text
resourceSyncEvent_event = Lens.lens (\ResourceSyncEvent' {event} -> event) (\s@ResourceSyncEvent' {} a -> s {event = a} :: ResourceSyncEvent)

-- | The time when the event occurred.
resourceSyncEvent_time :: Lens.Lens' ResourceSyncEvent Prelude.UTCTime
resourceSyncEvent_time = Lens.lens (\ResourceSyncEvent' {time} -> time) (\s@ResourceSyncEvent' {} a -> s {time = a} :: ResourceSyncEvent) Prelude.. Data._Time

-- | The type of event.
resourceSyncEvent_type :: Lens.Lens' ResourceSyncEvent Prelude.Text
resourceSyncEvent_type = Lens.lens (\ResourceSyncEvent' {type'} -> type') (\s@ResourceSyncEvent' {} a -> s {type' = a} :: ResourceSyncEvent)

instance Data.FromJSON ResourceSyncEvent where
  parseJSON =
    Data.withObject
      "ResourceSyncEvent"
      ( \x ->
          ResourceSyncEvent'
            Prelude.<$> (x Data..:? "externalId")
            Prelude.<*> (x Data..: "event")
            Prelude.<*> (x Data..: "time")
            Prelude.<*> (x Data..: "type")
      )

instance Prelude.Hashable ResourceSyncEvent where
  hashWithSalt _salt ResourceSyncEvent' {..} =
    _salt `Prelude.hashWithSalt` externalId
      `Prelude.hashWithSalt` event
      `Prelude.hashWithSalt` time
      `Prelude.hashWithSalt` type'

instance Prelude.NFData ResourceSyncEvent where
  rnf ResourceSyncEvent' {..} =
    Prelude.rnf externalId
      `Prelude.seq` Prelude.rnf event
      `Prelude.seq` Prelude.rnf time
      `Prelude.seq` Prelude.rnf type'
