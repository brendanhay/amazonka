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
-- Module      : Amazonka.Redshift.Types.EventCategoriesMap
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.EventCategoriesMap where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal
import Amazonka.Redshift.Types.EventInfoMap

-- | Describes event categories.
--
-- /See:/ 'newEventCategoriesMap' smart constructor.
data EventCategoriesMap = EventCategoriesMap'
  { -- | The events in the event category.
    events :: Prelude.Maybe [EventInfoMap],
    -- | The source type, such as cluster or cluster-snapshot, that the returned
    -- categories belong to.
    sourceType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventCategoriesMap' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'events', 'eventCategoriesMap_events' - The events in the event category.
--
-- 'sourceType', 'eventCategoriesMap_sourceType' - The source type, such as cluster or cluster-snapshot, that the returned
-- categories belong to.
newEventCategoriesMap ::
  EventCategoriesMap
newEventCategoriesMap =
  EventCategoriesMap'
    { events = Prelude.Nothing,
      sourceType = Prelude.Nothing
    }

-- | The events in the event category.
eventCategoriesMap_events :: Lens.Lens' EventCategoriesMap (Prelude.Maybe [EventInfoMap])
eventCategoriesMap_events = Lens.lens (\EventCategoriesMap' {events} -> events) (\s@EventCategoriesMap' {} a -> s {events = a} :: EventCategoriesMap) Prelude.. Lens.mapping Lens.coerced

-- | The source type, such as cluster or cluster-snapshot, that the returned
-- categories belong to.
eventCategoriesMap_sourceType :: Lens.Lens' EventCategoriesMap (Prelude.Maybe Prelude.Text)
eventCategoriesMap_sourceType = Lens.lens (\EventCategoriesMap' {sourceType} -> sourceType) (\s@EventCategoriesMap' {} a -> s {sourceType = a} :: EventCategoriesMap)

instance Data.FromXML EventCategoriesMap where
  parseXML x =
    EventCategoriesMap'
      Prelude.<$> ( x
                      Data..@? "Events"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "EventInfoMap")
                  )
      Prelude.<*> (x Data..@? "SourceType")

instance Prelude.Hashable EventCategoriesMap where
  hashWithSalt _salt EventCategoriesMap' {..} =
    _salt
      `Prelude.hashWithSalt` events
      `Prelude.hashWithSalt` sourceType

instance Prelude.NFData EventCategoriesMap where
  rnf EventCategoriesMap' {..} =
    Prelude.rnf events
      `Prelude.seq` Prelude.rnf sourceType
