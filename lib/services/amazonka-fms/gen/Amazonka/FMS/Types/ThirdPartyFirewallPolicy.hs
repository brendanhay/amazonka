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
-- Module      : Amazonka.FMS.Types.ThirdPartyFirewallPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.ThirdPartyFirewallPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types.FirewallDeploymentModel
import qualified Amazonka.Prelude as Prelude

-- | Configures the deployment model for the third-party firewall.
--
-- /See:/ 'newThirdPartyFirewallPolicy' smart constructor.
data ThirdPartyFirewallPolicy = ThirdPartyFirewallPolicy'
  { -- | Defines the deployment model to use for the third-party firewall policy.
    firewallDeploymentModel :: Prelude.Maybe FirewallDeploymentModel
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ThirdPartyFirewallPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firewallDeploymentModel', 'thirdPartyFirewallPolicy_firewallDeploymentModel' - Defines the deployment model to use for the third-party firewall policy.
newThirdPartyFirewallPolicy ::
  ThirdPartyFirewallPolicy
newThirdPartyFirewallPolicy =
  ThirdPartyFirewallPolicy'
    { firewallDeploymentModel =
        Prelude.Nothing
    }

-- | Defines the deployment model to use for the third-party firewall policy.
thirdPartyFirewallPolicy_firewallDeploymentModel :: Lens.Lens' ThirdPartyFirewallPolicy (Prelude.Maybe FirewallDeploymentModel)
thirdPartyFirewallPolicy_firewallDeploymentModel = Lens.lens (\ThirdPartyFirewallPolicy' {firewallDeploymentModel} -> firewallDeploymentModel) (\s@ThirdPartyFirewallPolicy' {} a -> s {firewallDeploymentModel = a} :: ThirdPartyFirewallPolicy)

instance Data.FromJSON ThirdPartyFirewallPolicy where
  parseJSON =
    Data.withObject
      "ThirdPartyFirewallPolicy"
      ( \x ->
          ThirdPartyFirewallPolicy'
            Prelude.<$> (x Data..:? "FirewallDeploymentModel")
      )

instance Prelude.Hashable ThirdPartyFirewallPolicy where
  hashWithSalt _salt ThirdPartyFirewallPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` firewallDeploymentModel

instance Prelude.NFData ThirdPartyFirewallPolicy where
  rnf ThirdPartyFirewallPolicy' {..} =
    Prelude.rnf firewallDeploymentModel

instance Data.ToJSON ThirdPartyFirewallPolicy where
  toJSON ThirdPartyFirewallPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FirewallDeploymentModel" Data..=)
              Prelude.<$> firewallDeploymentModel
          ]
      )
