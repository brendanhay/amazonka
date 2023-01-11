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
-- Module      : Amazonka.ECS.Types.ManagedAgentStateChange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.ManagedAgentStateChange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types.ManagedAgentName
import qualified Amazonka.Prelude as Prelude

-- | An object representing a change in state for a managed agent.
--
-- /See:/ 'newManagedAgentStateChange' smart constructor.
data ManagedAgentStateChange = ManagedAgentStateChange'
  { -- | The reason for the status of the managed agent.
    reason :: Prelude.Maybe Prelude.Text,
    -- | The name of the container that\'s associated with the managed agent.
    containerName :: Prelude.Text,
    -- | The name of the managed agent.
    managedAgentName :: ManagedAgentName,
    -- | The status of the managed agent.
    status :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ManagedAgentStateChange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reason', 'managedAgentStateChange_reason' - The reason for the status of the managed agent.
--
-- 'containerName', 'managedAgentStateChange_containerName' - The name of the container that\'s associated with the managed agent.
--
-- 'managedAgentName', 'managedAgentStateChange_managedAgentName' - The name of the managed agent.
--
-- 'status', 'managedAgentStateChange_status' - The status of the managed agent.
newManagedAgentStateChange ::
  -- | 'containerName'
  Prelude.Text ->
  -- | 'managedAgentName'
  ManagedAgentName ->
  -- | 'status'
  Prelude.Text ->
  ManagedAgentStateChange
newManagedAgentStateChange
  pContainerName_
  pManagedAgentName_
  pStatus_ =
    ManagedAgentStateChange'
      { reason = Prelude.Nothing,
        containerName = pContainerName_,
        managedAgentName = pManagedAgentName_,
        status = pStatus_
      }

-- | The reason for the status of the managed agent.
managedAgentStateChange_reason :: Lens.Lens' ManagedAgentStateChange (Prelude.Maybe Prelude.Text)
managedAgentStateChange_reason = Lens.lens (\ManagedAgentStateChange' {reason} -> reason) (\s@ManagedAgentStateChange' {} a -> s {reason = a} :: ManagedAgentStateChange)

-- | The name of the container that\'s associated with the managed agent.
managedAgentStateChange_containerName :: Lens.Lens' ManagedAgentStateChange Prelude.Text
managedAgentStateChange_containerName = Lens.lens (\ManagedAgentStateChange' {containerName} -> containerName) (\s@ManagedAgentStateChange' {} a -> s {containerName = a} :: ManagedAgentStateChange)

-- | The name of the managed agent.
managedAgentStateChange_managedAgentName :: Lens.Lens' ManagedAgentStateChange ManagedAgentName
managedAgentStateChange_managedAgentName = Lens.lens (\ManagedAgentStateChange' {managedAgentName} -> managedAgentName) (\s@ManagedAgentStateChange' {} a -> s {managedAgentName = a} :: ManagedAgentStateChange)

-- | The status of the managed agent.
managedAgentStateChange_status :: Lens.Lens' ManagedAgentStateChange Prelude.Text
managedAgentStateChange_status = Lens.lens (\ManagedAgentStateChange' {status} -> status) (\s@ManagedAgentStateChange' {} a -> s {status = a} :: ManagedAgentStateChange)

instance Prelude.Hashable ManagedAgentStateChange where
  hashWithSalt _salt ManagedAgentStateChange' {..} =
    _salt `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` containerName
      `Prelude.hashWithSalt` managedAgentName
      `Prelude.hashWithSalt` status

instance Prelude.NFData ManagedAgentStateChange where
  rnf ManagedAgentStateChange' {..} =
    Prelude.rnf reason
      `Prelude.seq` Prelude.rnf containerName
      `Prelude.seq` Prelude.rnf managedAgentName
      `Prelude.seq` Prelude.rnf status

instance Data.ToJSON ManagedAgentStateChange where
  toJSON ManagedAgentStateChange' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("reason" Data..=) Prelude.<$> reason,
            Prelude.Just ("containerName" Data..= containerName),
            Prelude.Just
              ("managedAgentName" Data..= managedAgentName),
            Prelude.Just ("status" Data..= status)
          ]
      )
