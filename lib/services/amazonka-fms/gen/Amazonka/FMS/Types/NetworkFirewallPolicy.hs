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
-- Module      : Amazonka.FMS.Types.NetworkFirewallPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.NetworkFirewallPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FMS.Types.FirewallDeploymentModel
import qualified Amazonka.Prelude as Prelude

-- | Configures the firewall policy deployment model of Network Firewall. For
-- information about Network Firewall deployment models, see
-- <https://docs.aws.amazon.com/network-firewall/latest/developerguide/architectures.html Network Firewall example architectures with routing>
-- in the /Network Firewall Developer Guide/.
--
-- /See:/ 'newNetworkFirewallPolicy' smart constructor.
data NetworkFirewallPolicy = NetworkFirewallPolicy'
  { -- | Defines the deployment model to use for the firewall policy. To use a
    -- distributed model, set
    -- <https://docs.aws.amazon.com/fms/2018-01-01/APIReference/API_PolicyOption.html PolicyOption>
    -- to @NULL@.
    firewallDeploymentModel :: Prelude.Maybe FirewallDeploymentModel
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkFirewallPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firewallDeploymentModel', 'networkFirewallPolicy_firewallDeploymentModel' - Defines the deployment model to use for the firewall policy. To use a
-- distributed model, set
-- <https://docs.aws.amazon.com/fms/2018-01-01/APIReference/API_PolicyOption.html PolicyOption>
-- to @NULL@.
newNetworkFirewallPolicy ::
  NetworkFirewallPolicy
newNetworkFirewallPolicy =
  NetworkFirewallPolicy'
    { firewallDeploymentModel =
        Prelude.Nothing
    }

-- | Defines the deployment model to use for the firewall policy. To use a
-- distributed model, set
-- <https://docs.aws.amazon.com/fms/2018-01-01/APIReference/API_PolicyOption.html PolicyOption>
-- to @NULL@.
networkFirewallPolicy_firewallDeploymentModel :: Lens.Lens' NetworkFirewallPolicy (Prelude.Maybe FirewallDeploymentModel)
networkFirewallPolicy_firewallDeploymentModel = Lens.lens (\NetworkFirewallPolicy' {firewallDeploymentModel} -> firewallDeploymentModel) (\s@NetworkFirewallPolicy' {} a -> s {firewallDeploymentModel = a} :: NetworkFirewallPolicy)

instance Core.FromJSON NetworkFirewallPolicy where
  parseJSON =
    Core.withObject
      "NetworkFirewallPolicy"
      ( \x ->
          NetworkFirewallPolicy'
            Prelude.<$> (x Core..:? "FirewallDeploymentModel")
      )

instance Prelude.Hashable NetworkFirewallPolicy where
  hashWithSalt _salt NetworkFirewallPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` firewallDeploymentModel

instance Prelude.NFData NetworkFirewallPolicy where
  rnf NetworkFirewallPolicy' {..} =
    Prelude.rnf firewallDeploymentModel

instance Core.ToJSON NetworkFirewallPolicy where
  toJSON NetworkFirewallPolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("FirewallDeploymentModel" Core..=)
              Prelude.<$> firewallDeploymentModel
          ]
      )
