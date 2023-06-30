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
-- Module      : Amazonka.Proton.Types.RepositorySyncEvent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.RepositorySyncEvent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Repository sync event detail data for a sync attempt.
--
-- /See:/ 'newRepositorySyncEvent' smart constructor.
data RepositorySyncEvent = RepositorySyncEvent'
  { -- | The external ID of the sync event.
    externalId :: Prelude.Maybe Prelude.Text,
    -- | Event detail for a repository sync attempt.
    event :: Prelude.Text,
    -- | The time that the sync event occurred.
    time :: Data.POSIX,
    -- | The type of event.
    type' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RepositorySyncEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'externalId', 'repositorySyncEvent_externalId' - The external ID of the sync event.
--
-- 'event', 'repositorySyncEvent_event' - Event detail for a repository sync attempt.
--
-- 'time', 'repositorySyncEvent_time' - The time that the sync event occurred.
--
-- 'type'', 'repositorySyncEvent_type' - The type of event.
newRepositorySyncEvent ::
  -- | 'event'
  Prelude.Text ->
  -- | 'time'
  Prelude.UTCTime ->
  -- | 'type''
  Prelude.Text ->
  RepositorySyncEvent
newRepositorySyncEvent pEvent_ pTime_ pType_ =
  RepositorySyncEvent'
    { externalId = Prelude.Nothing,
      event = pEvent_,
      time = Data._Time Lens.# pTime_,
      type' = pType_
    }

-- | The external ID of the sync event.
repositorySyncEvent_externalId :: Lens.Lens' RepositorySyncEvent (Prelude.Maybe Prelude.Text)
repositorySyncEvent_externalId = Lens.lens (\RepositorySyncEvent' {externalId} -> externalId) (\s@RepositorySyncEvent' {} a -> s {externalId = a} :: RepositorySyncEvent)

-- | Event detail for a repository sync attempt.
repositorySyncEvent_event :: Lens.Lens' RepositorySyncEvent Prelude.Text
repositorySyncEvent_event = Lens.lens (\RepositorySyncEvent' {event} -> event) (\s@RepositorySyncEvent' {} a -> s {event = a} :: RepositorySyncEvent)

-- | The time that the sync event occurred.
repositorySyncEvent_time :: Lens.Lens' RepositorySyncEvent Prelude.UTCTime
repositorySyncEvent_time = Lens.lens (\RepositorySyncEvent' {time} -> time) (\s@RepositorySyncEvent' {} a -> s {time = a} :: RepositorySyncEvent) Prelude.. Data._Time

-- | The type of event.
repositorySyncEvent_type :: Lens.Lens' RepositorySyncEvent Prelude.Text
repositorySyncEvent_type = Lens.lens (\RepositorySyncEvent' {type'} -> type') (\s@RepositorySyncEvent' {} a -> s {type' = a} :: RepositorySyncEvent)

instance Data.FromJSON RepositorySyncEvent where
  parseJSON =
    Data.withObject
      "RepositorySyncEvent"
      ( \x ->
          RepositorySyncEvent'
            Prelude.<$> (x Data..:? "externalId")
            Prelude.<*> (x Data..: "event")
            Prelude.<*> (x Data..: "time")
            Prelude.<*> (x Data..: "type")
      )

instance Prelude.Hashable RepositorySyncEvent where
  hashWithSalt _salt RepositorySyncEvent' {..} =
    _salt
      `Prelude.hashWithSalt` externalId
      `Prelude.hashWithSalt` event
      `Prelude.hashWithSalt` time
      `Prelude.hashWithSalt` type'

instance Prelude.NFData RepositorySyncEvent where
  rnf RepositorySyncEvent' {..} =
    Prelude.rnf externalId
      `Prelude.seq` Prelude.rnf event
      `Prelude.seq` Prelude.rnf time
      `Prelude.seq` Prelude.rnf type'
