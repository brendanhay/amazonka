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
-- Module      : Network.AWS.Redshift.Types.EventCategoriesMap
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.EventCategoriesMap where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.EventInfoMap

-- | Describes event categories.
--
-- /See:/ 'newEventCategoriesMap' smart constructor.
data EventCategoriesMap = EventCategoriesMap'
  { -- | The events in the event category.
    events :: Core.Maybe [EventInfoMap],
    -- | The source type, such as cluster or cluster-snapshot, that the returned
    -- categories belong to.
    sourceType :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { events = Core.Nothing,
      sourceType = Core.Nothing
    }

-- | The events in the event category.
eventCategoriesMap_events :: Lens.Lens' EventCategoriesMap (Core.Maybe [EventInfoMap])
eventCategoriesMap_events = Lens.lens (\EventCategoriesMap' {events} -> events) (\s@EventCategoriesMap' {} a -> s {events = a} :: EventCategoriesMap) Core.. Lens.mapping Lens._Coerce

-- | The source type, such as cluster or cluster-snapshot, that the returned
-- categories belong to.
eventCategoriesMap_sourceType :: Lens.Lens' EventCategoriesMap (Core.Maybe Core.Text)
eventCategoriesMap_sourceType = Lens.lens (\EventCategoriesMap' {sourceType} -> sourceType) (\s@EventCategoriesMap' {} a -> s {sourceType = a} :: EventCategoriesMap)

instance Core.FromXML EventCategoriesMap where
  parseXML x =
    EventCategoriesMap'
      Core.<$> ( x Core..@? "Events" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "EventInfoMap")
               )
      Core.<*> (x Core..@? "SourceType")

instance Core.Hashable EventCategoriesMap

instance Core.NFData EventCategoriesMap
