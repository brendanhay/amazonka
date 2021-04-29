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
-- Module      : Network.AWS.Redshift.Types.EventCategoriesMap
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.EventCategoriesMap where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.EventInfoMap

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
eventCategoriesMap_events = Lens.lens (\EventCategoriesMap' {events} -> events) (\s@EventCategoriesMap' {} a -> s {events = a} :: EventCategoriesMap) Prelude.. Lens.mapping Prelude._Coerce

-- | The source type, such as cluster or cluster-snapshot, that the returned
-- categories belong to.
eventCategoriesMap_sourceType :: Lens.Lens' EventCategoriesMap (Prelude.Maybe Prelude.Text)
eventCategoriesMap_sourceType = Lens.lens (\EventCategoriesMap' {sourceType} -> sourceType) (\s@EventCategoriesMap' {} a -> s {sourceType = a} :: EventCategoriesMap)

instance Prelude.FromXML EventCategoriesMap where
  parseXML x =
    EventCategoriesMap'
      Prelude.<$> ( x Prelude..@? "Events" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "EventInfoMap")
                  )
      Prelude.<*> (x Prelude..@? "SourceType")

instance Prelude.Hashable EventCategoriesMap

instance Prelude.NFData EventCategoriesMap
