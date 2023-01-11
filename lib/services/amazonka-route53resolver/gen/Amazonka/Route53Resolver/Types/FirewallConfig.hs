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
-- Module      : Amazonka.Route53Resolver.Types.FirewallConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Resolver.Types.FirewallConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53Resolver.Types.FirewallFailOpenStatus

-- | Configuration of the firewall behavior provided by DNS Firewall for a
-- single VPC from Amazon Virtual Private Cloud (Amazon VPC).
--
-- /See:/ 'newFirewallConfig' smart constructor.
data FirewallConfig = FirewallConfig'
  { -- | Determines how DNS Firewall operates during failures, for example when
    -- all traffic that is sent to DNS Firewall fails to receive a reply.
    --
    -- -   By default, fail open is disabled, which means the failure mode is
    --     closed. This approach favors security over availability. DNS
    --     Firewall returns a failure error when it is unable to properly
    --     evaluate a query.
    --
    -- -   If you enable this option, the failure mode is open. This approach
    --     favors availability over security. DNS Firewall allows queries to
    --     proceed if it is unable to properly evaluate them.
    --
    -- This behavior is only enforced for VPCs that have at least one DNS
    -- Firewall rule group association.
    firewallFailOpen :: Prelude.Maybe FirewallFailOpenStatus,
    -- | The ID of the firewall configuration.
    id :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID of the owner of the VPC that this
    -- firewall configuration applies to.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPC that this firewall configuration applies to.
    resourceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FirewallConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firewallFailOpen', 'firewallConfig_firewallFailOpen' - Determines how DNS Firewall operates during failures, for example when
-- all traffic that is sent to DNS Firewall fails to receive a reply.
--
-- -   By default, fail open is disabled, which means the failure mode is
--     closed. This approach favors security over availability. DNS
--     Firewall returns a failure error when it is unable to properly
--     evaluate a query.
--
-- -   If you enable this option, the failure mode is open. This approach
--     favors availability over security. DNS Firewall allows queries to
--     proceed if it is unable to properly evaluate them.
--
-- This behavior is only enforced for VPCs that have at least one DNS
-- Firewall rule group association.
--
-- 'id', 'firewallConfig_id' - The ID of the firewall configuration.
--
-- 'ownerId', 'firewallConfig_ownerId' - The Amazon Web Services account ID of the owner of the VPC that this
-- firewall configuration applies to.
--
-- 'resourceId', 'firewallConfig_resourceId' - The ID of the VPC that this firewall configuration applies to.
newFirewallConfig ::
  FirewallConfig
newFirewallConfig =
  FirewallConfig'
    { firewallFailOpen = Prelude.Nothing,
      id = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      resourceId = Prelude.Nothing
    }

-- | Determines how DNS Firewall operates during failures, for example when
-- all traffic that is sent to DNS Firewall fails to receive a reply.
--
-- -   By default, fail open is disabled, which means the failure mode is
--     closed. This approach favors security over availability. DNS
--     Firewall returns a failure error when it is unable to properly
--     evaluate a query.
--
-- -   If you enable this option, the failure mode is open. This approach
--     favors availability over security. DNS Firewall allows queries to
--     proceed if it is unable to properly evaluate them.
--
-- This behavior is only enforced for VPCs that have at least one DNS
-- Firewall rule group association.
firewallConfig_firewallFailOpen :: Lens.Lens' FirewallConfig (Prelude.Maybe FirewallFailOpenStatus)
firewallConfig_firewallFailOpen = Lens.lens (\FirewallConfig' {firewallFailOpen} -> firewallFailOpen) (\s@FirewallConfig' {} a -> s {firewallFailOpen = a} :: FirewallConfig)

-- | The ID of the firewall configuration.
firewallConfig_id :: Lens.Lens' FirewallConfig (Prelude.Maybe Prelude.Text)
firewallConfig_id = Lens.lens (\FirewallConfig' {id} -> id) (\s@FirewallConfig' {} a -> s {id = a} :: FirewallConfig)

-- | The Amazon Web Services account ID of the owner of the VPC that this
-- firewall configuration applies to.
firewallConfig_ownerId :: Lens.Lens' FirewallConfig (Prelude.Maybe Prelude.Text)
firewallConfig_ownerId = Lens.lens (\FirewallConfig' {ownerId} -> ownerId) (\s@FirewallConfig' {} a -> s {ownerId = a} :: FirewallConfig)

-- | The ID of the VPC that this firewall configuration applies to.
firewallConfig_resourceId :: Lens.Lens' FirewallConfig (Prelude.Maybe Prelude.Text)
firewallConfig_resourceId = Lens.lens (\FirewallConfig' {resourceId} -> resourceId) (\s@FirewallConfig' {} a -> s {resourceId = a} :: FirewallConfig)

instance Data.FromJSON FirewallConfig where
  parseJSON =
    Data.withObject
      "FirewallConfig"
      ( \x ->
          FirewallConfig'
            Prelude.<$> (x Data..:? "FirewallFailOpen")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "OwnerId")
            Prelude.<*> (x Data..:? "ResourceId")
      )

instance Prelude.Hashable FirewallConfig where
  hashWithSalt _salt FirewallConfig' {..} =
    _salt `Prelude.hashWithSalt` firewallFailOpen
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` resourceId

instance Prelude.NFData FirewallConfig where
  rnf FirewallConfig' {..} =
    Prelude.rnf firewallFailOpen
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf resourceId
