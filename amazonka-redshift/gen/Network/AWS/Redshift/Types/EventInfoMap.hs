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
-- Module      : Network.AWS.Redshift.Types.EventInfoMap
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.EventInfoMap where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Internal

-- | Describes event information.
--
-- /See:/ 'newEventInfoMap' smart constructor.
data EventInfoMap = EventInfoMap'
  { -- | The identifier of an Amazon Redshift event.
    eventId :: Prelude.Maybe Prelude.Text,
    -- | The severity of the event.
    --
    -- Values: ERROR, INFO
    severity :: Prelude.Maybe Prelude.Text,
    -- | The description of an Amazon Redshift event.
    eventDescription :: Prelude.Maybe Prelude.Text,
    -- | The category of an Amazon Redshift event.
    eventCategories :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EventInfoMap' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventId', 'eventInfoMap_eventId' - The identifier of an Amazon Redshift event.
--
-- 'severity', 'eventInfoMap_severity' - The severity of the event.
--
-- Values: ERROR, INFO
--
-- 'eventDescription', 'eventInfoMap_eventDescription' - The description of an Amazon Redshift event.
--
-- 'eventCategories', 'eventInfoMap_eventCategories' - The category of an Amazon Redshift event.
newEventInfoMap ::
  EventInfoMap
newEventInfoMap =
  EventInfoMap'
    { eventId = Prelude.Nothing,
      severity = Prelude.Nothing,
      eventDescription = Prelude.Nothing,
      eventCategories = Prelude.Nothing
    }

-- | The identifier of an Amazon Redshift event.
eventInfoMap_eventId :: Lens.Lens' EventInfoMap (Prelude.Maybe Prelude.Text)
eventInfoMap_eventId = Lens.lens (\EventInfoMap' {eventId} -> eventId) (\s@EventInfoMap' {} a -> s {eventId = a} :: EventInfoMap)

-- | The severity of the event.
--
-- Values: ERROR, INFO
eventInfoMap_severity :: Lens.Lens' EventInfoMap (Prelude.Maybe Prelude.Text)
eventInfoMap_severity = Lens.lens (\EventInfoMap' {severity} -> severity) (\s@EventInfoMap' {} a -> s {severity = a} :: EventInfoMap)

-- | The description of an Amazon Redshift event.
eventInfoMap_eventDescription :: Lens.Lens' EventInfoMap (Prelude.Maybe Prelude.Text)
eventInfoMap_eventDescription = Lens.lens (\EventInfoMap' {eventDescription} -> eventDescription) (\s@EventInfoMap' {} a -> s {eventDescription = a} :: EventInfoMap)

-- | The category of an Amazon Redshift event.
eventInfoMap_eventCategories :: Lens.Lens' EventInfoMap (Prelude.Maybe [Prelude.Text])
eventInfoMap_eventCategories = Lens.lens (\EventInfoMap' {eventCategories} -> eventCategories) (\s@EventInfoMap' {} a -> s {eventCategories = a} :: EventInfoMap) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromXML EventInfoMap where
  parseXML x =
    EventInfoMap'
      Prelude.<$> (x Prelude..@? "EventId")
      Prelude.<*> (x Prelude..@? "Severity")
      Prelude.<*> (x Prelude..@? "EventDescription")
      Prelude.<*> ( x Prelude..@? "EventCategories"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "EventCategory")
                  )

instance Prelude.Hashable EventInfoMap

instance Prelude.NFData EventInfoMap
