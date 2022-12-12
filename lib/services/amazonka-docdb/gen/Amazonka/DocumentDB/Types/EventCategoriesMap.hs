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
-- Module      : Amazonka.DocumentDB.Types.EventCategoriesMap
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DocumentDB.Types.EventCategoriesMap where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An event source type, accompanied by one or more event category names.
--
-- /See:/ 'newEventCategoriesMap' smart constructor.
data EventCategoriesMap = EventCategoriesMap'
  { -- | The event categories for the specified source type.
    eventCategories :: Prelude.Maybe [Prelude.Text],
    -- | The source type that the returned categories belong to.
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
-- 'eventCategories', 'eventCategoriesMap_eventCategories' - The event categories for the specified source type.
--
-- 'sourceType', 'eventCategoriesMap_sourceType' - The source type that the returned categories belong to.
newEventCategoriesMap ::
  EventCategoriesMap
newEventCategoriesMap =
  EventCategoriesMap'
    { eventCategories =
        Prelude.Nothing,
      sourceType = Prelude.Nothing
    }

-- | The event categories for the specified source type.
eventCategoriesMap_eventCategories :: Lens.Lens' EventCategoriesMap (Prelude.Maybe [Prelude.Text])
eventCategoriesMap_eventCategories = Lens.lens (\EventCategoriesMap' {eventCategories} -> eventCategories) (\s@EventCategoriesMap' {} a -> s {eventCategories = a} :: EventCategoriesMap) Prelude.. Lens.mapping Lens.coerced

-- | The source type that the returned categories belong to.
eventCategoriesMap_sourceType :: Lens.Lens' EventCategoriesMap (Prelude.Maybe Prelude.Text)
eventCategoriesMap_sourceType = Lens.lens (\EventCategoriesMap' {sourceType} -> sourceType) (\s@EventCategoriesMap' {} a -> s {sourceType = a} :: EventCategoriesMap)

instance Data.FromXML EventCategoriesMap where
  parseXML x =
    EventCategoriesMap'
      Prelude.<$> ( x Data..@? "EventCategories" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "EventCategory")
                  )
      Prelude.<*> (x Data..@? "SourceType")

instance Prelude.Hashable EventCategoriesMap where
  hashWithSalt _salt EventCategoriesMap' {..} =
    _salt `Prelude.hashWithSalt` eventCategories
      `Prelude.hashWithSalt` sourceType

instance Prelude.NFData EventCategoriesMap where
  rnf EventCategoriesMap' {..} =
    Prelude.rnf eventCategories
      `Prelude.seq` Prelude.rnf sourceType
