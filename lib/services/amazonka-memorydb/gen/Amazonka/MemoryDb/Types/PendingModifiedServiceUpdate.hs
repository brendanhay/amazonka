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
-- Module      : Amazonka.MemoryDb.Types.PendingModifiedServiceUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MemoryDb.Types.PendingModifiedServiceUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MemoryDb.Types.ServiceUpdateStatus
import qualified Amazonka.Prelude as Prelude

-- | Update action that has yet to be processed for the corresponding
-- apply\/stop request
--
-- /See:/ 'newPendingModifiedServiceUpdate' smart constructor.
data PendingModifiedServiceUpdate = PendingModifiedServiceUpdate'
  { -- | The unique ID of the service update
    serviceUpdateName :: Prelude.Maybe Prelude.Text,
    -- | The status of the service update
    status :: Prelude.Maybe ServiceUpdateStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PendingModifiedServiceUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceUpdateName', 'pendingModifiedServiceUpdate_serviceUpdateName' - The unique ID of the service update
--
-- 'status', 'pendingModifiedServiceUpdate_status' - The status of the service update
newPendingModifiedServiceUpdate ::
  PendingModifiedServiceUpdate
newPendingModifiedServiceUpdate =
  PendingModifiedServiceUpdate'
    { serviceUpdateName =
        Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The unique ID of the service update
pendingModifiedServiceUpdate_serviceUpdateName :: Lens.Lens' PendingModifiedServiceUpdate (Prelude.Maybe Prelude.Text)
pendingModifiedServiceUpdate_serviceUpdateName = Lens.lens (\PendingModifiedServiceUpdate' {serviceUpdateName} -> serviceUpdateName) (\s@PendingModifiedServiceUpdate' {} a -> s {serviceUpdateName = a} :: PendingModifiedServiceUpdate)

-- | The status of the service update
pendingModifiedServiceUpdate_status :: Lens.Lens' PendingModifiedServiceUpdate (Prelude.Maybe ServiceUpdateStatus)
pendingModifiedServiceUpdate_status = Lens.lens (\PendingModifiedServiceUpdate' {status} -> status) (\s@PendingModifiedServiceUpdate' {} a -> s {status = a} :: PendingModifiedServiceUpdate)

instance Data.FromJSON PendingModifiedServiceUpdate where
  parseJSON =
    Data.withObject
      "PendingModifiedServiceUpdate"
      ( \x ->
          PendingModifiedServiceUpdate'
            Prelude.<$> (x Data..:? "ServiceUpdateName")
            Prelude.<*> (x Data..:? "Status")
      )

instance
  Prelude.Hashable
    PendingModifiedServiceUpdate
  where
  hashWithSalt _salt PendingModifiedServiceUpdate' {..} =
    _salt
      `Prelude.hashWithSalt` serviceUpdateName
      `Prelude.hashWithSalt` status

instance Prelude.NFData PendingModifiedServiceUpdate where
  rnf PendingModifiedServiceUpdate' {..} =
    Prelude.rnf serviceUpdateName
      `Prelude.seq` Prelude.rnf status
