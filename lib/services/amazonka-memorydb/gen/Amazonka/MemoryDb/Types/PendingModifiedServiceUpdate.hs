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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MemoryDb.Types.PendingModifiedServiceUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MemoryDb.Types.ServiceUpdateStatus
import qualified Amazonka.Prelude as Prelude

-- | Update action that has yet to be processed for the corresponding
-- apply\/stop request
--
-- /See:/ 'newPendingModifiedServiceUpdate' smart constructor.
data PendingModifiedServiceUpdate = PendingModifiedServiceUpdate'
  { -- | The status of the service update
    status :: Prelude.Maybe ServiceUpdateStatus,
    -- | The unique ID of the service update
    serviceUpdateName :: Prelude.Maybe Prelude.Text
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
-- 'status', 'pendingModifiedServiceUpdate_status' - The status of the service update
--
-- 'serviceUpdateName', 'pendingModifiedServiceUpdate_serviceUpdateName' - The unique ID of the service update
newPendingModifiedServiceUpdate ::
  PendingModifiedServiceUpdate
newPendingModifiedServiceUpdate =
  PendingModifiedServiceUpdate'
    { status =
        Prelude.Nothing,
      serviceUpdateName = Prelude.Nothing
    }

-- | The status of the service update
pendingModifiedServiceUpdate_status :: Lens.Lens' PendingModifiedServiceUpdate (Prelude.Maybe ServiceUpdateStatus)
pendingModifiedServiceUpdate_status = Lens.lens (\PendingModifiedServiceUpdate' {status} -> status) (\s@PendingModifiedServiceUpdate' {} a -> s {status = a} :: PendingModifiedServiceUpdate)

-- | The unique ID of the service update
pendingModifiedServiceUpdate_serviceUpdateName :: Lens.Lens' PendingModifiedServiceUpdate (Prelude.Maybe Prelude.Text)
pendingModifiedServiceUpdate_serviceUpdateName = Lens.lens (\PendingModifiedServiceUpdate' {serviceUpdateName} -> serviceUpdateName) (\s@PendingModifiedServiceUpdate' {} a -> s {serviceUpdateName = a} :: PendingModifiedServiceUpdate)

instance Core.FromJSON PendingModifiedServiceUpdate where
  parseJSON =
    Core.withObject
      "PendingModifiedServiceUpdate"
      ( \x ->
          PendingModifiedServiceUpdate'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "ServiceUpdateName")
      )

instance
  Prelude.Hashable
    PendingModifiedServiceUpdate
  where
  hashWithSalt _salt PendingModifiedServiceUpdate' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` serviceUpdateName

instance Prelude.NFData PendingModifiedServiceUpdate where
  rnf PendingModifiedServiceUpdate' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf serviceUpdateName
