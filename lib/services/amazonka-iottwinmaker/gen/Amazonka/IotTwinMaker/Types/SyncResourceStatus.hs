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
-- Module      : Amazonka.IotTwinMaker.Types.SyncResourceStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.SyncResourceStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types.ErrorDetails
import Amazonka.IotTwinMaker.Types.SyncResourceState
import qualified Amazonka.Prelude as Prelude

-- | The sync resource status.
--
-- /See:/ 'newSyncResourceStatus' smart constructor.
data SyncResourceStatus = SyncResourceStatus'
  { -- | The status error.
    error :: Prelude.Maybe ErrorDetails,
    -- | The sync resource status state.
    state :: Prelude.Maybe SyncResourceState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SyncResourceStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'error', 'syncResourceStatus_error' - The status error.
--
-- 'state', 'syncResourceStatus_state' - The sync resource status state.
newSyncResourceStatus ::
  SyncResourceStatus
newSyncResourceStatus =
  SyncResourceStatus'
    { error = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | The status error.
syncResourceStatus_error :: Lens.Lens' SyncResourceStatus (Prelude.Maybe ErrorDetails)
syncResourceStatus_error = Lens.lens (\SyncResourceStatus' {error} -> error) (\s@SyncResourceStatus' {} a -> s {error = a} :: SyncResourceStatus)

-- | The sync resource status state.
syncResourceStatus_state :: Lens.Lens' SyncResourceStatus (Prelude.Maybe SyncResourceState)
syncResourceStatus_state = Lens.lens (\SyncResourceStatus' {state} -> state) (\s@SyncResourceStatus' {} a -> s {state = a} :: SyncResourceStatus)

instance Data.FromJSON SyncResourceStatus where
  parseJSON =
    Data.withObject
      "SyncResourceStatus"
      ( \x ->
          SyncResourceStatus'
            Prelude.<$> (x Data..:? "error")
            Prelude.<*> (x Data..:? "state")
      )

instance Prelude.Hashable SyncResourceStatus where
  hashWithSalt _salt SyncResourceStatus' {..} =
    _salt `Prelude.hashWithSalt` error
      `Prelude.hashWithSalt` state

instance Prelude.NFData SyncResourceStatus where
  rnf SyncResourceStatus' {..} =
    Prelude.rnf error `Prelude.seq` Prelude.rnf state
