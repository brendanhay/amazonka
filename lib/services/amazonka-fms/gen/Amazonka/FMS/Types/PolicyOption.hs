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
-- Module      : Amazonka.FMS.Types.PolicyOption
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.PolicyOption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types.NetworkFirewallPolicy
import Amazonka.FMS.Types.ThirdPartyFirewallPolicy
import qualified Amazonka.Prelude as Prelude

-- | Contains the Network Firewall firewall policy options to configure the
-- policy\'s deployment model and third-party firewall policy settings.
--
-- /See:/ 'newPolicyOption' smart constructor.
data PolicyOption = PolicyOption'
  { -- | Defines the policy options for a third-party firewall policy.
    thirdPartyFirewallPolicy :: Prelude.Maybe ThirdPartyFirewallPolicy,
    -- | Defines the deployment model to use for the firewall policy.
    networkFirewallPolicy :: Prelude.Maybe NetworkFirewallPolicy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PolicyOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'thirdPartyFirewallPolicy', 'policyOption_thirdPartyFirewallPolicy' - Defines the policy options for a third-party firewall policy.
--
-- 'networkFirewallPolicy', 'policyOption_networkFirewallPolicy' - Defines the deployment model to use for the firewall policy.
newPolicyOption ::
  PolicyOption
newPolicyOption =
  PolicyOption'
    { thirdPartyFirewallPolicy =
        Prelude.Nothing,
      networkFirewallPolicy = Prelude.Nothing
    }

-- | Defines the policy options for a third-party firewall policy.
policyOption_thirdPartyFirewallPolicy :: Lens.Lens' PolicyOption (Prelude.Maybe ThirdPartyFirewallPolicy)
policyOption_thirdPartyFirewallPolicy = Lens.lens (\PolicyOption' {thirdPartyFirewallPolicy} -> thirdPartyFirewallPolicy) (\s@PolicyOption' {} a -> s {thirdPartyFirewallPolicy = a} :: PolicyOption)

-- | Defines the deployment model to use for the firewall policy.
policyOption_networkFirewallPolicy :: Lens.Lens' PolicyOption (Prelude.Maybe NetworkFirewallPolicy)
policyOption_networkFirewallPolicy = Lens.lens (\PolicyOption' {networkFirewallPolicy} -> networkFirewallPolicy) (\s@PolicyOption' {} a -> s {networkFirewallPolicy = a} :: PolicyOption)

instance Data.FromJSON PolicyOption where
  parseJSON =
    Data.withObject
      "PolicyOption"
      ( \x ->
          PolicyOption'
            Prelude.<$> (x Data..:? "ThirdPartyFirewallPolicy")
            Prelude.<*> (x Data..:? "NetworkFirewallPolicy")
      )

instance Prelude.Hashable PolicyOption where
  hashWithSalt _salt PolicyOption' {..} =
    _salt
      `Prelude.hashWithSalt` thirdPartyFirewallPolicy
      `Prelude.hashWithSalt` networkFirewallPolicy

instance Prelude.NFData PolicyOption where
  rnf PolicyOption' {..} =
    Prelude.rnf thirdPartyFirewallPolicy
      `Prelude.seq` Prelude.rnf networkFirewallPolicy

instance Data.ToJSON PolicyOption where
  toJSON PolicyOption' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ThirdPartyFirewallPolicy" Data..=)
              Prelude.<$> thirdPartyFirewallPolicy,
            ("NetworkFirewallPolicy" Data..=)
              Prelude.<$> networkFirewallPolicy
          ]
      )
