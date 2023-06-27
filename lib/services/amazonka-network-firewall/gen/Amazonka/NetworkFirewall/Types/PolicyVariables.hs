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
-- Module      : Amazonka.NetworkFirewall.Types.PolicyVariables
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.PolicyVariables where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types.IPSet
import qualified Amazonka.Prelude as Prelude

-- | Contains variables that you can use to override default Suricata
-- settings in your firewall policy.
--
-- /See:/ 'newPolicyVariables' smart constructor.
data PolicyVariables = PolicyVariables'
  { -- | The IPv4 or IPv6 addresses in CIDR notation to use for the Suricata
    -- @HOME_NET@ variable. If your firewall uses an inspection VPC, you might
    -- want to override the @HOME_NET@ variable with the CIDRs of your home
    -- networks. If you don\'t override @HOME_NET@ with your own CIDRs, Network
    -- Firewall by default uses the CIDR of your inspection VPC.
    ruleVariables :: Prelude.Maybe (Prelude.HashMap Prelude.Text IPSet)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PolicyVariables' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleVariables', 'policyVariables_ruleVariables' - The IPv4 or IPv6 addresses in CIDR notation to use for the Suricata
-- @HOME_NET@ variable. If your firewall uses an inspection VPC, you might
-- want to override the @HOME_NET@ variable with the CIDRs of your home
-- networks. If you don\'t override @HOME_NET@ with your own CIDRs, Network
-- Firewall by default uses the CIDR of your inspection VPC.
newPolicyVariables ::
  PolicyVariables
newPolicyVariables =
  PolicyVariables' {ruleVariables = Prelude.Nothing}

-- | The IPv4 or IPv6 addresses in CIDR notation to use for the Suricata
-- @HOME_NET@ variable. If your firewall uses an inspection VPC, you might
-- want to override the @HOME_NET@ variable with the CIDRs of your home
-- networks. If you don\'t override @HOME_NET@ with your own CIDRs, Network
-- Firewall by default uses the CIDR of your inspection VPC.
policyVariables_ruleVariables :: Lens.Lens' PolicyVariables (Prelude.Maybe (Prelude.HashMap Prelude.Text IPSet))
policyVariables_ruleVariables = Lens.lens (\PolicyVariables' {ruleVariables} -> ruleVariables) (\s@PolicyVariables' {} a -> s {ruleVariables = a} :: PolicyVariables) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON PolicyVariables where
  parseJSON =
    Data.withObject
      "PolicyVariables"
      ( \x ->
          PolicyVariables'
            Prelude.<$> (x Data..:? "RuleVariables" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable PolicyVariables where
  hashWithSalt _salt PolicyVariables' {..} =
    _salt `Prelude.hashWithSalt` ruleVariables

instance Prelude.NFData PolicyVariables where
  rnf PolicyVariables' {..} = Prelude.rnf ruleVariables

instance Data.ToJSON PolicyVariables where
  toJSON PolicyVariables' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RuleVariables" Data..=)
              Prelude.<$> ruleVariables
          ]
      )
