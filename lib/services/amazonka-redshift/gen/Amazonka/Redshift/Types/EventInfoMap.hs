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
-- Module      : Amazonka.Redshift.Types.EventInfoMap
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.EventInfoMap where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal

-- | Describes event information.
--
-- /See:/ 'newEventInfoMap' smart constructor.
data EventInfoMap = EventInfoMap'
  { -- | The category of an Amazon Redshift event.
    eventCategories :: Prelude.Maybe [Prelude.Text],
    -- | The description of an Amazon Redshift event.
    eventDescription :: Prelude.Maybe Prelude.Text,
    -- | The identifier of an Amazon Redshift event.
    eventId :: Prelude.Maybe Prelude.Text,
    -- | The severity of the event.
    --
    -- Values: ERROR, INFO
    severity :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventInfoMap' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventCategories', 'eventInfoMap_eventCategories' - The category of an Amazon Redshift event.
--
-- 'eventDescription', 'eventInfoMap_eventDescription' - The description of an Amazon Redshift event.
--
-- 'eventId', 'eventInfoMap_eventId' - The identifier of an Amazon Redshift event.
--
-- 'severity', 'eventInfoMap_severity' - The severity of the event.
--
-- Values: ERROR, INFO
newEventInfoMap ::
  EventInfoMap
newEventInfoMap =
  EventInfoMap'
    { eventCategories = Prelude.Nothing,
      eventDescription = Prelude.Nothing,
      eventId = Prelude.Nothing,
      severity = Prelude.Nothing
    }

-- | The category of an Amazon Redshift event.
eventInfoMap_eventCategories :: Lens.Lens' EventInfoMap (Prelude.Maybe [Prelude.Text])
eventInfoMap_eventCategories = Lens.lens (\EventInfoMap' {eventCategories} -> eventCategories) (\s@EventInfoMap' {} a -> s {eventCategories = a} :: EventInfoMap) Prelude.. Lens.mapping Lens.coerced

-- | The description of an Amazon Redshift event.
eventInfoMap_eventDescription :: Lens.Lens' EventInfoMap (Prelude.Maybe Prelude.Text)
eventInfoMap_eventDescription = Lens.lens (\EventInfoMap' {eventDescription} -> eventDescription) (\s@EventInfoMap' {} a -> s {eventDescription = a} :: EventInfoMap)

-- | The identifier of an Amazon Redshift event.
eventInfoMap_eventId :: Lens.Lens' EventInfoMap (Prelude.Maybe Prelude.Text)
eventInfoMap_eventId = Lens.lens (\EventInfoMap' {eventId} -> eventId) (\s@EventInfoMap' {} a -> s {eventId = a} :: EventInfoMap)

-- | The severity of the event.
--
-- Values: ERROR, INFO
eventInfoMap_severity :: Lens.Lens' EventInfoMap (Prelude.Maybe Prelude.Text)
eventInfoMap_severity = Lens.lens (\EventInfoMap' {severity} -> severity) (\s@EventInfoMap' {} a -> s {severity = a} :: EventInfoMap)

instance Data.FromXML EventInfoMap where
  parseXML x =
    EventInfoMap'
      Prelude.<$> ( x Data..@? "EventCategories" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "EventCategory")
                  )
      Prelude.<*> (x Data..@? "EventDescription")
      Prelude.<*> (x Data..@? "EventId")
      Prelude.<*> (x Data..@? "Severity")

instance Prelude.Hashable EventInfoMap where
  hashWithSalt _salt EventInfoMap' {..} =
    _salt `Prelude.hashWithSalt` eventCategories
      `Prelude.hashWithSalt` eventDescription
      `Prelude.hashWithSalt` eventId
      `Prelude.hashWithSalt` severity

instance Prelude.NFData EventInfoMap where
  rnf EventInfoMap' {..} =
    Prelude.rnf eventCategories
      `Prelude.seq` Prelude.rnf eventDescription
      `Prelude.seq` Prelude.rnf eventId
      `Prelude.seq` Prelude.rnf severity
