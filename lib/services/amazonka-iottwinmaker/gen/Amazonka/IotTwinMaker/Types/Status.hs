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
-- Module      : Amazonka.IotTwinMaker.Types.Status
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.Status where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types.ErrorDetails
import Amazonka.IotTwinMaker.Types.State
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the status of an entity, component, component
-- type, or workspace.
--
-- /See:/ 'newStatus' smart constructor.
data Status = Status'
  { -- | The error message.
    error :: Prelude.Maybe ErrorDetails,
    -- | The current state of the entity, component, component type, or
    -- workspace.
    state :: Prelude.Maybe State
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Status' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'error', 'status_error' - The error message.
--
-- 'state', 'status_state' - The current state of the entity, component, component type, or
-- workspace.
newStatus ::
  Status
newStatus =
  Status'
    { error = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | The error message.
status_error :: Lens.Lens' Status (Prelude.Maybe ErrorDetails)
status_error = Lens.lens (\Status' {error} -> error) (\s@Status' {} a -> s {error = a} :: Status)

-- | The current state of the entity, component, component type, or
-- workspace.
status_state :: Lens.Lens' Status (Prelude.Maybe State)
status_state = Lens.lens (\Status' {state} -> state) (\s@Status' {} a -> s {state = a} :: Status)

instance Data.FromJSON Status where
  parseJSON =
    Data.withObject
      "Status"
      ( \x ->
          Status'
            Prelude.<$> (x Data..:? "error")
            Prelude.<*> (x Data..:? "state")
      )

instance Prelude.Hashable Status where
  hashWithSalt _salt Status' {..} =
    _salt
      `Prelude.hashWithSalt` error
      `Prelude.hashWithSalt` state

instance Prelude.NFData Status where
  rnf Status' {..} =
    Prelude.rnf error `Prelude.seq` Prelude.rnf state
