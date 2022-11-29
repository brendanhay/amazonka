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
-- Module      : Amazonka.NetworkFirewall.Types.SyncState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.SyncState where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.NetworkFirewall.Types.Attachment
import Amazonka.NetworkFirewall.Types.PerObjectStatus
import qualified Amazonka.Prelude as Prelude

-- | The status of the firewall endpoint and firewall policy configuration
-- for a single VPC subnet.
--
-- For each VPC subnet that you associate with a firewall, Network Firewall
-- does the following:
--
-- -   Instantiates a firewall endpoint in the subnet, ready to take
--     traffic.
--
-- -   Configures the endpoint with the current firewall policy settings,
--     to provide the filtering behavior for the endpoint.
--
-- When you update a firewall, for example to add a subnet association or
-- change a rule group in the firewall policy, the affected sync states
-- reflect out-of-sync or not ready status until the changes are complete.
--
-- /See:/ 'newSyncState' smart constructor.
data SyncState = SyncState'
  { -- | The attachment status of the firewall\'s association with a single VPC
    -- subnet. For each configured subnet, Network Firewall creates the
    -- attachment by instantiating the firewall endpoint in the subnet so that
    -- it\'s ready to take traffic. This is part of the FirewallStatus.
    attachment :: Prelude.Maybe Attachment,
    -- | The configuration status of the firewall endpoint in a single VPC
    -- subnet. Network Firewall provides each endpoint with the rules that are
    -- configured in the firewall policy. Each time you add a subnet or modify
    -- the associated firewall policy, Network Firewall synchronizes the rules
    -- in the endpoint, so it can properly filter network traffic. This is part
    -- of the FirewallStatus.
    config :: Prelude.Maybe (Prelude.HashMap Prelude.Text PerObjectStatus)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SyncState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachment', 'syncState_attachment' - The attachment status of the firewall\'s association with a single VPC
-- subnet. For each configured subnet, Network Firewall creates the
-- attachment by instantiating the firewall endpoint in the subnet so that
-- it\'s ready to take traffic. This is part of the FirewallStatus.
--
-- 'config', 'syncState_config' - The configuration status of the firewall endpoint in a single VPC
-- subnet. Network Firewall provides each endpoint with the rules that are
-- configured in the firewall policy. Each time you add a subnet or modify
-- the associated firewall policy, Network Firewall synchronizes the rules
-- in the endpoint, so it can properly filter network traffic. This is part
-- of the FirewallStatus.
newSyncState ::
  SyncState
newSyncState =
  SyncState'
    { attachment = Prelude.Nothing,
      config = Prelude.Nothing
    }

-- | The attachment status of the firewall\'s association with a single VPC
-- subnet. For each configured subnet, Network Firewall creates the
-- attachment by instantiating the firewall endpoint in the subnet so that
-- it\'s ready to take traffic. This is part of the FirewallStatus.
syncState_attachment :: Lens.Lens' SyncState (Prelude.Maybe Attachment)
syncState_attachment = Lens.lens (\SyncState' {attachment} -> attachment) (\s@SyncState' {} a -> s {attachment = a} :: SyncState)

-- | The configuration status of the firewall endpoint in a single VPC
-- subnet. Network Firewall provides each endpoint with the rules that are
-- configured in the firewall policy. Each time you add a subnet or modify
-- the associated firewall policy, Network Firewall synchronizes the rules
-- in the endpoint, so it can properly filter network traffic. This is part
-- of the FirewallStatus.
syncState_config :: Lens.Lens' SyncState (Prelude.Maybe (Prelude.HashMap Prelude.Text PerObjectStatus))
syncState_config = Lens.lens (\SyncState' {config} -> config) (\s@SyncState' {} a -> s {config = a} :: SyncState) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON SyncState where
  parseJSON =
    Core.withObject
      "SyncState"
      ( \x ->
          SyncState'
            Prelude.<$> (x Core..:? "Attachment")
            Prelude.<*> (x Core..:? "Config" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable SyncState where
  hashWithSalt _salt SyncState' {..} =
    _salt `Prelude.hashWithSalt` attachment
      `Prelude.hashWithSalt` config

instance Prelude.NFData SyncState where
  rnf SyncState' {..} =
    Prelude.rnf attachment
      `Prelude.seq` Prelude.rnf config
