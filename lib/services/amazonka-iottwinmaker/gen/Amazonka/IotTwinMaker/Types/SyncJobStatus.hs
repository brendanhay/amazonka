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
-- Module      : Amazonka.IotTwinMaker.Types.SyncJobStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.SyncJobStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types.ErrorDetails
import Amazonka.IotTwinMaker.Types.SyncJobState
import qualified Amazonka.Prelude as Prelude

-- | The SyncJob status.
--
-- /See:/ 'newSyncJobStatus' smart constructor.
data SyncJobStatus = SyncJobStatus'
  { -- | The SyncJob error.
    error :: Prelude.Maybe ErrorDetails,
    -- | The SyncJob status state.
    state :: Prelude.Maybe SyncJobState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SyncJobStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'error', 'syncJobStatus_error' - The SyncJob error.
--
-- 'state', 'syncJobStatus_state' - The SyncJob status state.
newSyncJobStatus ::
  SyncJobStatus
newSyncJobStatus =
  SyncJobStatus'
    { error = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | The SyncJob error.
syncJobStatus_error :: Lens.Lens' SyncJobStatus (Prelude.Maybe ErrorDetails)
syncJobStatus_error = Lens.lens (\SyncJobStatus' {error} -> error) (\s@SyncJobStatus' {} a -> s {error = a} :: SyncJobStatus)

-- | The SyncJob status state.
syncJobStatus_state :: Lens.Lens' SyncJobStatus (Prelude.Maybe SyncJobState)
syncJobStatus_state = Lens.lens (\SyncJobStatus' {state} -> state) (\s@SyncJobStatus' {} a -> s {state = a} :: SyncJobStatus)

instance Data.FromJSON SyncJobStatus where
  parseJSON =
    Data.withObject
      "SyncJobStatus"
      ( \x ->
          SyncJobStatus'
            Prelude.<$> (x Data..:? "error")
            Prelude.<*> (x Data..:? "state")
      )

instance Prelude.Hashable SyncJobStatus where
  hashWithSalt _salt SyncJobStatus' {..} =
    _salt
      `Prelude.hashWithSalt` error
      `Prelude.hashWithSalt` state

instance Prelude.NFData SyncJobStatus where
  rnf SyncJobStatus' {..} =
    Prelude.rnf error `Prelude.seq` Prelude.rnf state
