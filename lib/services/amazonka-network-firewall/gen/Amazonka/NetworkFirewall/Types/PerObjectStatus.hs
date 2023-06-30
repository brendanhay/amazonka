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
-- Module      : Amazonka.NetworkFirewall.Types.PerObjectStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.PerObjectStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types.PerObjectSyncStatus
import qualified Amazonka.Prelude as Prelude

-- | Provides configuration status for a single policy or rule group that is
-- used for a firewall endpoint. Network Firewall provides each endpoint
-- with the rules that are configured in the firewall policy. Each time you
-- add a subnet or modify the associated firewall policy, Network Firewall
-- synchronizes the rules in the endpoint, so it can properly filter
-- network traffic. This is part of a SyncState for a firewall.
--
-- /See:/ 'newPerObjectStatus' smart constructor.
data PerObjectStatus = PerObjectStatus'
  { -- | Indicates whether this object is in sync with the version indicated in
    -- the update token.
    syncStatus :: Prelude.Maybe PerObjectSyncStatus,
    -- | The current version of the object that is either in sync or pending
    -- synchronization.
    updateToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PerObjectStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'syncStatus', 'perObjectStatus_syncStatus' - Indicates whether this object is in sync with the version indicated in
-- the update token.
--
-- 'updateToken', 'perObjectStatus_updateToken' - The current version of the object that is either in sync or pending
-- synchronization.
newPerObjectStatus ::
  PerObjectStatus
newPerObjectStatus =
  PerObjectStatus'
    { syncStatus = Prelude.Nothing,
      updateToken = Prelude.Nothing
    }

-- | Indicates whether this object is in sync with the version indicated in
-- the update token.
perObjectStatus_syncStatus :: Lens.Lens' PerObjectStatus (Prelude.Maybe PerObjectSyncStatus)
perObjectStatus_syncStatus = Lens.lens (\PerObjectStatus' {syncStatus} -> syncStatus) (\s@PerObjectStatus' {} a -> s {syncStatus = a} :: PerObjectStatus)

-- | The current version of the object that is either in sync or pending
-- synchronization.
perObjectStatus_updateToken :: Lens.Lens' PerObjectStatus (Prelude.Maybe Prelude.Text)
perObjectStatus_updateToken = Lens.lens (\PerObjectStatus' {updateToken} -> updateToken) (\s@PerObjectStatus' {} a -> s {updateToken = a} :: PerObjectStatus)

instance Data.FromJSON PerObjectStatus where
  parseJSON =
    Data.withObject
      "PerObjectStatus"
      ( \x ->
          PerObjectStatus'
            Prelude.<$> (x Data..:? "SyncStatus")
            Prelude.<*> (x Data..:? "UpdateToken")
      )

instance Prelude.Hashable PerObjectStatus where
  hashWithSalt _salt PerObjectStatus' {..} =
    _salt
      `Prelude.hashWithSalt` syncStatus
      `Prelude.hashWithSalt` updateToken

instance Prelude.NFData PerObjectStatus where
  rnf PerObjectStatus' {..} =
    Prelude.rnf syncStatus
      `Prelude.seq` Prelude.rnf updateToken
