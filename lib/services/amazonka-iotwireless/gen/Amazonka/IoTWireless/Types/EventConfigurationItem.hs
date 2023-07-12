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
-- Module      : Amazonka.IoTWireless.Types.EventConfigurationItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.EventConfigurationItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types.EventNotificationItemConfigurations
import Amazonka.IoTWireless.Types.EventNotificationPartnerType
import Amazonka.IoTWireless.Types.IdentifierType
import qualified Amazonka.Prelude as Prelude

-- | Event configuration object for a single resource.
--
-- /See:/ 'newEventConfigurationItem' smart constructor.
data EventConfigurationItem = EventConfigurationItem'
  { events :: Prelude.Maybe EventNotificationItemConfigurations,
    -- | Resource identifier opted in for event messaging.
    identifier :: Prelude.Maybe Prelude.Text,
    -- | Identifier type of the particular resource identifier for event
    -- configuration.
    identifierType :: Prelude.Maybe IdentifierType,
    -- | Partner type of the resource if the identifier type is PartnerAccountId.
    partnerType :: Prelude.Maybe EventNotificationPartnerType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventConfigurationItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'events', 'eventConfigurationItem_events' - Undocumented member.
--
-- 'identifier', 'eventConfigurationItem_identifier' - Resource identifier opted in for event messaging.
--
-- 'identifierType', 'eventConfigurationItem_identifierType' - Identifier type of the particular resource identifier for event
-- configuration.
--
-- 'partnerType', 'eventConfigurationItem_partnerType' - Partner type of the resource if the identifier type is PartnerAccountId.
newEventConfigurationItem ::
  EventConfigurationItem
newEventConfigurationItem =
  EventConfigurationItem'
    { events = Prelude.Nothing,
      identifier = Prelude.Nothing,
      identifierType = Prelude.Nothing,
      partnerType = Prelude.Nothing
    }

-- | Undocumented member.
eventConfigurationItem_events :: Lens.Lens' EventConfigurationItem (Prelude.Maybe EventNotificationItemConfigurations)
eventConfigurationItem_events = Lens.lens (\EventConfigurationItem' {events} -> events) (\s@EventConfigurationItem' {} a -> s {events = a} :: EventConfigurationItem)

-- | Resource identifier opted in for event messaging.
eventConfigurationItem_identifier :: Lens.Lens' EventConfigurationItem (Prelude.Maybe Prelude.Text)
eventConfigurationItem_identifier = Lens.lens (\EventConfigurationItem' {identifier} -> identifier) (\s@EventConfigurationItem' {} a -> s {identifier = a} :: EventConfigurationItem)

-- | Identifier type of the particular resource identifier for event
-- configuration.
eventConfigurationItem_identifierType :: Lens.Lens' EventConfigurationItem (Prelude.Maybe IdentifierType)
eventConfigurationItem_identifierType = Lens.lens (\EventConfigurationItem' {identifierType} -> identifierType) (\s@EventConfigurationItem' {} a -> s {identifierType = a} :: EventConfigurationItem)

-- | Partner type of the resource if the identifier type is PartnerAccountId.
eventConfigurationItem_partnerType :: Lens.Lens' EventConfigurationItem (Prelude.Maybe EventNotificationPartnerType)
eventConfigurationItem_partnerType = Lens.lens (\EventConfigurationItem' {partnerType} -> partnerType) (\s@EventConfigurationItem' {} a -> s {partnerType = a} :: EventConfigurationItem)

instance Data.FromJSON EventConfigurationItem where
  parseJSON =
    Data.withObject
      "EventConfigurationItem"
      ( \x ->
          EventConfigurationItem'
            Prelude.<$> (x Data..:? "Events")
            Prelude.<*> (x Data..:? "Identifier")
            Prelude.<*> (x Data..:? "IdentifierType")
            Prelude.<*> (x Data..:? "PartnerType")
      )

instance Prelude.Hashable EventConfigurationItem where
  hashWithSalt _salt EventConfigurationItem' {..} =
    _salt
      `Prelude.hashWithSalt` events
      `Prelude.hashWithSalt` identifier
      `Prelude.hashWithSalt` identifierType
      `Prelude.hashWithSalt` partnerType

instance Prelude.NFData EventConfigurationItem where
  rnf EventConfigurationItem' {..} =
    Prelude.rnf events
      `Prelude.seq` Prelude.rnf identifier
      `Prelude.seq` Prelude.rnf identifierType
      `Prelude.seq` Prelude.rnf partnerType
