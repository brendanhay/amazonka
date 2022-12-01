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
-- Module      : Amazonka.FMS.Types.NetworkFirewallStatefulRuleGroupOverride
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.NetworkFirewallStatefulRuleGroupOverride where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FMS.Types.NetworkFirewallOverrideAction
import qualified Amazonka.Prelude as Prelude

-- | The setting that allows the policy owner to change the behavior of the
-- rule group within a policy.
--
-- /See:/ 'newNetworkFirewallStatefulRuleGroupOverride' smart constructor.
data NetworkFirewallStatefulRuleGroupOverride = NetworkFirewallStatefulRuleGroupOverride'
  { -- | The action that changes the rule group from @DROP@ to @ALERT@. This only
    -- applies to managed rule groups.
    action :: Prelude.Maybe NetworkFirewallOverrideAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkFirewallStatefulRuleGroupOverride' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'networkFirewallStatefulRuleGroupOverride_action' - The action that changes the rule group from @DROP@ to @ALERT@. This only
-- applies to managed rule groups.
newNetworkFirewallStatefulRuleGroupOverride ::
  NetworkFirewallStatefulRuleGroupOverride
newNetworkFirewallStatefulRuleGroupOverride =
  NetworkFirewallStatefulRuleGroupOverride'
    { action =
        Prelude.Nothing
    }

-- | The action that changes the rule group from @DROP@ to @ALERT@. This only
-- applies to managed rule groups.
networkFirewallStatefulRuleGroupOverride_action :: Lens.Lens' NetworkFirewallStatefulRuleGroupOverride (Prelude.Maybe NetworkFirewallOverrideAction)
networkFirewallStatefulRuleGroupOverride_action = Lens.lens (\NetworkFirewallStatefulRuleGroupOverride' {action} -> action) (\s@NetworkFirewallStatefulRuleGroupOverride' {} a -> s {action = a} :: NetworkFirewallStatefulRuleGroupOverride)

instance
  Core.FromJSON
    NetworkFirewallStatefulRuleGroupOverride
  where
  parseJSON =
    Core.withObject
      "NetworkFirewallStatefulRuleGroupOverride"
      ( \x ->
          NetworkFirewallStatefulRuleGroupOverride'
            Prelude.<$> (x Core..:? "Action")
      )

instance
  Prelude.Hashable
    NetworkFirewallStatefulRuleGroupOverride
  where
  hashWithSalt
    _salt
    NetworkFirewallStatefulRuleGroupOverride' {..} =
      _salt `Prelude.hashWithSalt` action

instance
  Prelude.NFData
    NetworkFirewallStatefulRuleGroupOverride
  where
  rnf NetworkFirewallStatefulRuleGroupOverride' {..} =
    Prelude.rnf action
