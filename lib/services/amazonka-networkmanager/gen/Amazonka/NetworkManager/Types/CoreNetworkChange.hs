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
-- Module      : Amazonka.NetworkManager.Types.CoreNetworkChange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.CoreNetworkChange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types.ChangeAction
import Amazonka.NetworkManager.Types.ChangeType
import Amazonka.NetworkManager.Types.CoreNetworkChangeValues
import qualified Amazonka.Prelude as Prelude

-- | Details describing a core network change.
--
-- /See:/ 'newCoreNetworkChange' smart constructor.
data CoreNetworkChange = CoreNetworkChange'
  { -- | The action to take for a core network.
    action :: Prelude.Maybe ChangeAction,
    -- | The resource identifier.
    identifier :: Prelude.Maybe Prelude.Text,
    -- | Uniquely identifies the path for a change within the changeset. For
    -- example, the @IdentifierPath@ for a core network segment change might be
    -- @\"CORE_NETWORK_SEGMENT\/us-east-1\/devsegment\"@.
    identifierPath :: Prelude.Maybe Prelude.Text,
    -- | The new value for a core network
    newValues' :: Prelude.Maybe CoreNetworkChangeValues,
    -- | The previous values for a core network.
    previousValues :: Prelude.Maybe CoreNetworkChangeValues,
    -- | The type of change.
    type' :: Prelude.Maybe ChangeType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CoreNetworkChange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'coreNetworkChange_action' - The action to take for a core network.
--
-- 'identifier', 'coreNetworkChange_identifier' - The resource identifier.
--
-- 'identifierPath', 'coreNetworkChange_identifierPath' - Uniquely identifies the path for a change within the changeset. For
-- example, the @IdentifierPath@ for a core network segment change might be
-- @\"CORE_NETWORK_SEGMENT\/us-east-1\/devsegment\"@.
--
-- 'newValues'', 'coreNetworkChange_newValues' - The new value for a core network
--
-- 'previousValues', 'coreNetworkChange_previousValues' - The previous values for a core network.
--
-- 'type'', 'coreNetworkChange_type' - The type of change.
newCoreNetworkChange ::
  CoreNetworkChange
newCoreNetworkChange =
  CoreNetworkChange'
    { action = Prelude.Nothing,
      identifier = Prelude.Nothing,
      identifierPath = Prelude.Nothing,
      newValues' = Prelude.Nothing,
      previousValues = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The action to take for a core network.
coreNetworkChange_action :: Lens.Lens' CoreNetworkChange (Prelude.Maybe ChangeAction)
coreNetworkChange_action = Lens.lens (\CoreNetworkChange' {action} -> action) (\s@CoreNetworkChange' {} a -> s {action = a} :: CoreNetworkChange)

-- | The resource identifier.
coreNetworkChange_identifier :: Lens.Lens' CoreNetworkChange (Prelude.Maybe Prelude.Text)
coreNetworkChange_identifier = Lens.lens (\CoreNetworkChange' {identifier} -> identifier) (\s@CoreNetworkChange' {} a -> s {identifier = a} :: CoreNetworkChange)

-- | Uniquely identifies the path for a change within the changeset. For
-- example, the @IdentifierPath@ for a core network segment change might be
-- @\"CORE_NETWORK_SEGMENT\/us-east-1\/devsegment\"@.
coreNetworkChange_identifierPath :: Lens.Lens' CoreNetworkChange (Prelude.Maybe Prelude.Text)
coreNetworkChange_identifierPath = Lens.lens (\CoreNetworkChange' {identifierPath} -> identifierPath) (\s@CoreNetworkChange' {} a -> s {identifierPath = a} :: CoreNetworkChange)

-- | The new value for a core network
coreNetworkChange_newValues :: Lens.Lens' CoreNetworkChange (Prelude.Maybe CoreNetworkChangeValues)
coreNetworkChange_newValues = Lens.lens (\CoreNetworkChange' {newValues'} -> newValues') (\s@CoreNetworkChange' {} a -> s {newValues' = a} :: CoreNetworkChange)

-- | The previous values for a core network.
coreNetworkChange_previousValues :: Lens.Lens' CoreNetworkChange (Prelude.Maybe CoreNetworkChangeValues)
coreNetworkChange_previousValues = Lens.lens (\CoreNetworkChange' {previousValues} -> previousValues) (\s@CoreNetworkChange' {} a -> s {previousValues = a} :: CoreNetworkChange)

-- | The type of change.
coreNetworkChange_type :: Lens.Lens' CoreNetworkChange (Prelude.Maybe ChangeType)
coreNetworkChange_type = Lens.lens (\CoreNetworkChange' {type'} -> type') (\s@CoreNetworkChange' {} a -> s {type' = a} :: CoreNetworkChange)

instance Data.FromJSON CoreNetworkChange where
  parseJSON =
    Data.withObject
      "CoreNetworkChange"
      ( \x ->
          CoreNetworkChange'
            Prelude.<$> (x Data..:? "Action")
            Prelude.<*> (x Data..:? "Identifier")
            Prelude.<*> (x Data..:? "IdentifierPath")
            Prelude.<*> (x Data..:? "NewValues")
            Prelude.<*> (x Data..:? "PreviousValues")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable CoreNetworkChange where
  hashWithSalt _salt CoreNetworkChange' {..} =
    _salt
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` identifier
      `Prelude.hashWithSalt` identifierPath
      `Prelude.hashWithSalt` newValues'
      `Prelude.hashWithSalt` previousValues
      `Prelude.hashWithSalt` type'

instance Prelude.NFData CoreNetworkChange where
  rnf CoreNetworkChange' {..} =
    Prelude.rnf action
      `Prelude.seq` Prelude.rnf identifier
      `Prelude.seq` Prelude.rnf identifierPath
      `Prelude.seq` Prelude.rnf newValues'
      `Prelude.seq` Prelude.rnf previousValues
      `Prelude.seq` Prelude.rnf type'
