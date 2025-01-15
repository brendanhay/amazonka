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
-- Module      : Amazonka.DevOpsGuru.Types.Event
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.Event where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types.EventClass
import Amazonka.DevOpsGuru.Types.EventDataSource
import Amazonka.DevOpsGuru.Types.EventResource
import Amazonka.DevOpsGuru.Types.ResourceCollection
import qualified Amazonka.Prelude as Prelude

-- | An Amazon Web Services resource event. Amazon Web Services resource
-- events and metrics are analyzed by DevOps Guru to find anomalous
-- behavior and provide recommendations to improve your operational
-- solutions.
--
-- /See:/ 'newEvent' smart constructor.
data Event = Event'
  { -- | The source, @AWS_CLOUD_TRAIL@ or @AWS_CODE_DEPLOY@, where DevOps Guru
    -- analysis found the event.
    dataSource :: Prelude.Maybe EventDataSource,
    -- | The class of the event. The class specifies what the event is related
    -- to, such as an infrastructure change, a deployment, or a schema change.
    eventClass :: Prelude.Maybe EventClass,
    -- | The Amazon Web Services source that emitted the event.
    eventSource :: Prelude.Maybe Prelude.Text,
    -- | The ID of the event.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the event.
    name :: Prelude.Maybe Prelude.Text,
    resourceCollection :: Prelude.Maybe ResourceCollection,
    -- | An @EventResource@ object that contains information about the resource
    -- that emitted the event.
    resources :: Prelude.Maybe [EventResource],
    -- | A @Timestamp@ that specifies the time the event occurred.
    time :: Prelude.Maybe Data.POSIX
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
-- 'dataSource', 'event_dataSource' - The source, @AWS_CLOUD_TRAIL@ or @AWS_CODE_DEPLOY@, where DevOps Guru
-- analysis found the event.
--
-- 'eventClass', 'event_eventClass' - The class of the event. The class specifies what the event is related
-- to, such as an infrastructure change, a deployment, or a schema change.
--
-- 'eventSource', 'event_eventSource' - The Amazon Web Services source that emitted the event.
--
-- 'id', 'event_id' - The ID of the event.
--
-- 'name', 'event_name' - The name of the event.
--
-- 'resourceCollection', 'event_resourceCollection' - Undocumented member.
--
-- 'resources', 'event_resources' - An @EventResource@ object that contains information about the resource
-- that emitted the event.
--
-- 'time', 'event_time' - A @Timestamp@ that specifies the time the event occurred.
newEvent ::
  Event
newEvent =
  Event'
    { dataSource = Prelude.Nothing,
      eventClass = Prelude.Nothing,
      eventSource = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      resourceCollection = Prelude.Nothing,
      resources = Prelude.Nothing,
      time = Prelude.Nothing
    }

-- | The source, @AWS_CLOUD_TRAIL@ or @AWS_CODE_DEPLOY@, where DevOps Guru
-- analysis found the event.
event_dataSource :: Lens.Lens' Event (Prelude.Maybe EventDataSource)
event_dataSource = Lens.lens (\Event' {dataSource} -> dataSource) (\s@Event' {} a -> s {dataSource = a} :: Event)

-- | The class of the event. The class specifies what the event is related
-- to, such as an infrastructure change, a deployment, or a schema change.
event_eventClass :: Lens.Lens' Event (Prelude.Maybe EventClass)
event_eventClass = Lens.lens (\Event' {eventClass} -> eventClass) (\s@Event' {} a -> s {eventClass = a} :: Event)

-- | The Amazon Web Services source that emitted the event.
event_eventSource :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_eventSource = Lens.lens (\Event' {eventSource} -> eventSource) (\s@Event' {} a -> s {eventSource = a} :: Event)

-- | The ID of the event.
event_id :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_id = Lens.lens (\Event' {id} -> id) (\s@Event' {} a -> s {id = a} :: Event)

-- | The name of the event.
event_name :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_name = Lens.lens (\Event' {name} -> name) (\s@Event' {} a -> s {name = a} :: Event)

-- | Undocumented member.
event_resourceCollection :: Lens.Lens' Event (Prelude.Maybe ResourceCollection)
event_resourceCollection = Lens.lens (\Event' {resourceCollection} -> resourceCollection) (\s@Event' {} a -> s {resourceCollection = a} :: Event)

-- | An @EventResource@ object that contains information about the resource
-- that emitted the event.
event_resources :: Lens.Lens' Event (Prelude.Maybe [EventResource])
event_resources = Lens.lens (\Event' {resources} -> resources) (\s@Event' {} a -> s {resources = a} :: Event) Prelude.. Lens.mapping Lens.coerced

-- | A @Timestamp@ that specifies the time the event occurred.
event_time :: Lens.Lens' Event (Prelude.Maybe Prelude.UTCTime)
event_time = Lens.lens (\Event' {time} -> time) (\s@Event' {} a -> s {time = a} :: Event) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON Event where
  parseJSON =
    Data.withObject
      "Event"
      ( \x ->
          Event'
            Prelude.<$> (x Data..:? "DataSource")
            Prelude.<*> (x Data..:? "EventClass")
            Prelude.<*> (x Data..:? "EventSource")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "ResourceCollection")
            Prelude.<*> (x Data..:? "Resources" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Time")
      )

instance Prelude.Hashable Event where
  hashWithSalt _salt Event' {..} =
    _salt
      `Prelude.hashWithSalt` dataSource
      `Prelude.hashWithSalt` eventClass
      `Prelude.hashWithSalt` eventSource
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` resourceCollection
      `Prelude.hashWithSalt` resources
      `Prelude.hashWithSalt` time

instance Prelude.NFData Event where
  rnf Event' {..} =
    Prelude.rnf dataSource `Prelude.seq`
      Prelude.rnf eventClass `Prelude.seq`
        Prelude.rnf eventSource `Prelude.seq`
          Prelude.rnf id `Prelude.seq`
            Prelude.rnf name `Prelude.seq`
              Prelude.rnf resourceCollection `Prelude.seq`
                Prelude.rnf resources `Prelude.seq`
                  Prelude.rnf time
