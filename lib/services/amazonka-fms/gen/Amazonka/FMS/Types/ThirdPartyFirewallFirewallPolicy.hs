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
-- Module      : Amazonka.FMS.Types.ThirdPartyFirewallFirewallPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.ThirdPartyFirewallFirewallPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configures the third-party firewall\'s firewall policy.
--
-- /See:/ 'newThirdPartyFirewallFirewallPolicy' smart constructor.
data ThirdPartyFirewallFirewallPolicy = ThirdPartyFirewallFirewallPolicy'
  { -- | The ID of the specified firewall policy.
    firewallPolicyId :: Prelude.Maybe Prelude.Text,
    -- | The name of the specified firewall policy.
    firewallPolicyName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ThirdPartyFirewallFirewallPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firewallPolicyId', 'thirdPartyFirewallFirewallPolicy_firewallPolicyId' - The ID of the specified firewall policy.
--
-- 'firewallPolicyName', 'thirdPartyFirewallFirewallPolicy_firewallPolicyName' - The name of the specified firewall policy.
newThirdPartyFirewallFirewallPolicy ::
  ThirdPartyFirewallFirewallPolicy
newThirdPartyFirewallFirewallPolicy =
  ThirdPartyFirewallFirewallPolicy'
    { firewallPolicyId =
        Prelude.Nothing,
      firewallPolicyName = Prelude.Nothing
    }

-- | The ID of the specified firewall policy.
thirdPartyFirewallFirewallPolicy_firewallPolicyId :: Lens.Lens' ThirdPartyFirewallFirewallPolicy (Prelude.Maybe Prelude.Text)
thirdPartyFirewallFirewallPolicy_firewallPolicyId = Lens.lens (\ThirdPartyFirewallFirewallPolicy' {firewallPolicyId} -> firewallPolicyId) (\s@ThirdPartyFirewallFirewallPolicy' {} a -> s {firewallPolicyId = a} :: ThirdPartyFirewallFirewallPolicy)

-- | The name of the specified firewall policy.
thirdPartyFirewallFirewallPolicy_firewallPolicyName :: Lens.Lens' ThirdPartyFirewallFirewallPolicy (Prelude.Maybe Prelude.Text)
thirdPartyFirewallFirewallPolicy_firewallPolicyName = Lens.lens (\ThirdPartyFirewallFirewallPolicy' {firewallPolicyName} -> firewallPolicyName) (\s@ThirdPartyFirewallFirewallPolicy' {} a -> s {firewallPolicyName = a} :: ThirdPartyFirewallFirewallPolicy)

instance
  Data.FromJSON
    ThirdPartyFirewallFirewallPolicy
  where
  parseJSON =
    Data.withObject
      "ThirdPartyFirewallFirewallPolicy"
      ( \x ->
          ThirdPartyFirewallFirewallPolicy'
            Prelude.<$> (x Data..:? "FirewallPolicyId")
            Prelude.<*> (x Data..:? "FirewallPolicyName")
      )

instance
  Prelude.Hashable
    ThirdPartyFirewallFirewallPolicy
  where
  hashWithSalt
    _salt
    ThirdPartyFirewallFirewallPolicy' {..} =
      _salt `Prelude.hashWithSalt` firewallPolicyId
        `Prelude.hashWithSalt` firewallPolicyName

instance
  Prelude.NFData
    ThirdPartyFirewallFirewallPolicy
  where
  rnf ThirdPartyFirewallFirewallPolicy' {..} =
    Prelude.rnf firewallPolicyId
      `Prelude.seq` Prelude.rnf firewallPolicyName
