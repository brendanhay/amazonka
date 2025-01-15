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
-- Module      : Amazonka.DevOpsGuru.Types.ListEventsFilters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.ListEventsFilters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types.EventClass
import Amazonka.DevOpsGuru.Types.EventDataSource
import Amazonka.DevOpsGuru.Types.EventTimeRange
import Amazonka.DevOpsGuru.Types.ResourceCollection
import qualified Amazonka.Prelude as Prelude

-- | Filters you can use to specify which events are returned when
-- @ListEvents@ is called.
--
-- /See:/ 'newListEventsFilters' smart constructor.
data ListEventsFilters = ListEventsFilters'
  { -- | The source, @AWS_CLOUD_TRAIL@ or @AWS_CODE_DEPLOY@, of the events you
    -- want returned.
    dataSource :: Prelude.Maybe EventDataSource,
    -- | The class of the events you want to filter for, such as an
    -- infrastructure change, a deployment, or a schema change.
    eventClass :: Prelude.Maybe EventClass,
    -- | The Amazon Web Services source that emitted the events you want to
    -- filter for.
    eventSource :: Prelude.Maybe Prelude.Text,
    -- | A time range during which you want the filtered events to have occurred.
    eventTimeRange :: Prelude.Maybe EventTimeRange,
    -- | An ID of an insight that is related to the events you want to filter
    -- for.
    insightId :: Prelude.Maybe Prelude.Text,
    resourceCollection :: Prelude.Maybe ResourceCollection
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEventsFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSource', 'listEventsFilters_dataSource' - The source, @AWS_CLOUD_TRAIL@ or @AWS_CODE_DEPLOY@, of the events you
-- want returned.
--
-- 'eventClass', 'listEventsFilters_eventClass' - The class of the events you want to filter for, such as an
-- infrastructure change, a deployment, or a schema change.
--
-- 'eventSource', 'listEventsFilters_eventSource' - The Amazon Web Services source that emitted the events you want to
-- filter for.
--
-- 'eventTimeRange', 'listEventsFilters_eventTimeRange' - A time range during which you want the filtered events to have occurred.
--
-- 'insightId', 'listEventsFilters_insightId' - An ID of an insight that is related to the events you want to filter
-- for.
--
-- 'resourceCollection', 'listEventsFilters_resourceCollection' - Undocumented member.
newListEventsFilters ::
  ListEventsFilters
newListEventsFilters =
  ListEventsFilters'
    { dataSource = Prelude.Nothing,
      eventClass = Prelude.Nothing,
      eventSource = Prelude.Nothing,
      eventTimeRange = Prelude.Nothing,
      insightId = Prelude.Nothing,
      resourceCollection = Prelude.Nothing
    }

-- | The source, @AWS_CLOUD_TRAIL@ or @AWS_CODE_DEPLOY@, of the events you
-- want returned.
listEventsFilters_dataSource :: Lens.Lens' ListEventsFilters (Prelude.Maybe EventDataSource)
listEventsFilters_dataSource = Lens.lens (\ListEventsFilters' {dataSource} -> dataSource) (\s@ListEventsFilters' {} a -> s {dataSource = a} :: ListEventsFilters)

-- | The class of the events you want to filter for, such as an
-- infrastructure change, a deployment, or a schema change.
listEventsFilters_eventClass :: Lens.Lens' ListEventsFilters (Prelude.Maybe EventClass)
listEventsFilters_eventClass = Lens.lens (\ListEventsFilters' {eventClass} -> eventClass) (\s@ListEventsFilters' {} a -> s {eventClass = a} :: ListEventsFilters)

-- | The Amazon Web Services source that emitted the events you want to
-- filter for.
listEventsFilters_eventSource :: Lens.Lens' ListEventsFilters (Prelude.Maybe Prelude.Text)
listEventsFilters_eventSource = Lens.lens (\ListEventsFilters' {eventSource} -> eventSource) (\s@ListEventsFilters' {} a -> s {eventSource = a} :: ListEventsFilters)

-- | A time range during which you want the filtered events to have occurred.
listEventsFilters_eventTimeRange :: Lens.Lens' ListEventsFilters (Prelude.Maybe EventTimeRange)
listEventsFilters_eventTimeRange = Lens.lens (\ListEventsFilters' {eventTimeRange} -> eventTimeRange) (\s@ListEventsFilters' {} a -> s {eventTimeRange = a} :: ListEventsFilters)

-- | An ID of an insight that is related to the events you want to filter
-- for.
listEventsFilters_insightId :: Lens.Lens' ListEventsFilters (Prelude.Maybe Prelude.Text)
listEventsFilters_insightId = Lens.lens (\ListEventsFilters' {insightId} -> insightId) (\s@ListEventsFilters' {} a -> s {insightId = a} :: ListEventsFilters)

-- | Undocumented member.
listEventsFilters_resourceCollection :: Lens.Lens' ListEventsFilters (Prelude.Maybe ResourceCollection)
listEventsFilters_resourceCollection = Lens.lens (\ListEventsFilters' {resourceCollection} -> resourceCollection) (\s@ListEventsFilters' {} a -> s {resourceCollection = a} :: ListEventsFilters)

instance Prelude.Hashable ListEventsFilters where
  hashWithSalt _salt ListEventsFilters' {..} =
    _salt
      `Prelude.hashWithSalt` dataSource
      `Prelude.hashWithSalt` eventClass
      `Prelude.hashWithSalt` eventSource
      `Prelude.hashWithSalt` eventTimeRange
      `Prelude.hashWithSalt` insightId
      `Prelude.hashWithSalt` resourceCollection

instance Prelude.NFData ListEventsFilters where
  rnf ListEventsFilters' {..} =
    Prelude.rnf dataSource `Prelude.seq`
      Prelude.rnf eventClass `Prelude.seq`
        Prelude.rnf eventSource `Prelude.seq`
          Prelude.rnf eventTimeRange `Prelude.seq`
            Prelude.rnf insightId `Prelude.seq`
              Prelude.rnf resourceCollection

instance Data.ToJSON ListEventsFilters where
  toJSON ListEventsFilters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataSource" Data..=) Prelude.<$> dataSource,
            ("EventClass" Data..=) Prelude.<$> eventClass,
            ("EventSource" Data..=) Prelude.<$> eventSource,
            ("EventTimeRange" Data..=)
              Prelude.<$> eventTimeRange,
            ("InsightId" Data..=) Prelude.<$> insightId,
            ("ResourceCollection" Data..=)
              Prelude.<$> resourceCollection
          ]
      )
