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
-- Module      : Amazonka.NetworkManager.Types.CoreNetworkChangeEvent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.CoreNetworkChangeEvent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types.ChangeAction
import Amazonka.NetworkManager.Types.ChangeStatus
import Amazonka.NetworkManager.Types.ChangeType
import Amazonka.NetworkManager.Types.CoreNetworkChangeEventValues
import qualified Amazonka.Prelude as Prelude

-- | Describes a core network change event. This can be a change to a
-- segment, attachment, route, etc.
--
-- /See:/ 'newCoreNetworkChangeEvent' smart constructor.
data CoreNetworkChangeEvent = CoreNetworkChangeEvent'
  { -- | The action taken for the change event.
    action :: Prelude.Maybe ChangeAction,
    -- | The timestamp for an event change in status.
    eventTime :: Prelude.Maybe Data.POSIX,
    -- | Uniquely identifies the path for a change within the changeset. For
    -- example, the @IdentifierPath@ for a core network segment change might be
    -- @\"CORE_NETWORK_SEGMENT\/us-east-1\/devsegment\"@.
    identifierPath :: Prelude.Maybe Prelude.Text,
    -- | The status of the core network change event.
    status :: Prelude.Maybe ChangeStatus,
    -- | Describes the type of change event.
    type' :: Prelude.Maybe ChangeType,
    -- | Details of the change event.
    values :: Prelude.Maybe CoreNetworkChangeEventValues
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CoreNetworkChangeEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'coreNetworkChangeEvent_action' - The action taken for the change event.
--
-- 'eventTime', 'coreNetworkChangeEvent_eventTime' - The timestamp for an event change in status.
--
-- 'identifierPath', 'coreNetworkChangeEvent_identifierPath' - Uniquely identifies the path for a change within the changeset. For
-- example, the @IdentifierPath@ for a core network segment change might be
-- @\"CORE_NETWORK_SEGMENT\/us-east-1\/devsegment\"@.
--
-- 'status', 'coreNetworkChangeEvent_status' - The status of the core network change event.
--
-- 'type'', 'coreNetworkChangeEvent_type' - Describes the type of change event.
--
-- 'values', 'coreNetworkChangeEvent_values' - Details of the change event.
newCoreNetworkChangeEvent ::
  CoreNetworkChangeEvent
newCoreNetworkChangeEvent =
  CoreNetworkChangeEvent'
    { action = Prelude.Nothing,
      eventTime = Prelude.Nothing,
      identifierPath = Prelude.Nothing,
      status = Prelude.Nothing,
      type' = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The action taken for the change event.
coreNetworkChangeEvent_action :: Lens.Lens' CoreNetworkChangeEvent (Prelude.Maybe ChangeAction)
coreNetworkChangeEvent_action = Lens.lens (\CoreNetworkChangeEvent' {action} -> action) (\s@CoreNetworkChangeEvent' {} a -> s {action = a} :: CoreNetworkChangeEvent)

-- | The timestamp for an event change in status.
coreNetworkChangeEvent_eventTime :: Lens.Lens' CoreNetworkChangeEvent (Prelude.Maybe Prelude.UTCTime)
coreNetworkChangeEvent_eventTime = Lens.lens (\CoreNetworkChangeEvent' {eventTime} -> eventTime) (\s@CoreNetworkChangeEvent' {} a -> s {eventTime = a} :: CoreNetworkChangeEvent) Prelude.. Lens.mapping Data._Time

-- | Uniquely identifies the path for a change within the changeset. For
-- example, the @IdentifierPath@ for a core network segment change might be
-- @\"CORE_NETWORK_SEGMENT\/us-east-1\/devsegment\"@.
coreNetworkChangeEvent_identifierPath :: Lens.Lens' CoreNetworkChangeEvent (Prelude.Maybe Prelude.Text)
coreNetworkChangeEvent_identifierPath = Lens.lens (\CoreNetworkChangeEvent' {identifierPath} -> identifierPath) (\s@CoreNetworkChangeEvent' {} a -> s {identifierPath = a} :: CoreNetworkChangeEvent)

-- | The status of the core network change event.
coreNetworkChangeEvent_status :: Lens.Lens' CoreNetworkChangeEvent (Prelude.Maybe ChangeStatus)
coreNetworkChangeEvent_status = Lens.lens (\CoreNetworkChangeEvent' {status} -> status) (\s@CoreNetworkChangeEvent' {} a -> s {status = a} :: CoreNetworkChangeEvent)

-- | Describes the type of change event.
coreNetworkChangeEvent_type :: Lens.Lens' CoreNetworkChangeEvent (Prelude.Maybe ChangeType)
coreNetworkChangeEvent_type = Lens.lens (\CoreNetworkChangeEvent' {type'} -> type') (\s@CoreNetworkChangeEvent' {} a -> s {type' = a} :: CoreNetworkChangeEvent)

-- | Details of the change event.
coreNetworkChangeEvent_values :: Lens.Lens' CoreNetworkChangeEvent (Prelude.Maybe CoreNetworkChangeEventValues)
coreNetworkChangeEvent_values = Lens.lens (\CoreNetworkChangeEvent' {values} -> values) (\s@CoreNetworkChangeEvent' {} a -> s {values = a} :: CoreNetworkChangeEvent)

instance Data.FromJSON CoreNetworkChangeEvent where
  parseJSON =
    Data.withObject
      "CoreNetworkChangeEvent"
      ( \x ->
          CoreNetworkChangeEvent'
            Prelude.<$> (x Data..:? "Action")
            Prelude.<*> (x Data..:? "EventTime")
            Prelude.<*> (x Data..:? "IdentifierPath")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "Values")
      )

instance Prelude.Hashable CoreNetworkChangeEvent where
  hashWithSalt _salt CoreNetworkChangeEvent' {..} =
    _salt `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` eventTime
      `Prelude.hashWithSalt` identifierPath
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` values

instance Prelude.NFData CoreNetworkChangeEvent where
  rnf CoreNetworkChangeEvent' {..} =
    Prelude.rnf action
      `Prelude.seq` Prelude.rnf eventTime
      `Prelude.seq` Prelude.rnf identifierPath
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf values
