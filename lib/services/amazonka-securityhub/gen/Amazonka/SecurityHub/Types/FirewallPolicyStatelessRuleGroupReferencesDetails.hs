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
-- Module      : Amazonka.SecurityHub.Types.FirewallPolicyStatelessRuleGroupReferencesDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.FirewallPolicyStatelessRuleGroupReferencesDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A stateless rule group that is used by the firewall policy.
--
-- /See:/ 'newFirewallPolicyStatelessRuleGroupReferencesDetails' smart constructor.
data FirewallPolicyStatelessRuleGroupReferencesDetails = FirewallPolicyStatelessRuleGroupReferencesDetails'
  { -- | The order in which to run the stateless rule group.
    priority :: Prelude.Maybe Prelude.Int,
    -- | The ARN of the stateless rule group.
    resourceArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FirewallPolicyStatelessRuleGroupReferencesDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'priority', 'firewallPolicyStatelessRuleGroupReferencesDetails_priority' - The order in which to run the stateless rule group.
--
-- 'resourceArn', 'firewallPolicyStatelessRuleGroupReferencesDetails_resourceArn' - The ARN of the stateless rule group.
newFirewallPolicyStatelessRuleGroupReferencesDetails ::
  FirewallPolicyStatelessRuleGroupReferencesDetails
newFirewallPolicyStatelessRuleGroupReferencesDetails =
  FirewallPolicyStatelessRuleGroupReferencesDetails'
    { priority =
        Prelude.Nothing,
      resourceArn =
        Prelude.Nothing
    }

-- | The order in which to run the stateless rule group.
firewallPolicyStatelessRuleGroupReferencesDetails_priority :: Lens.Lens' FirewallPolicyStatelessRuleGroupReferencesDetails (Prelude.Maybe Prelude.Int)
firewallPolicyStatelessRuleGroupReferencesDetails_priority = Lens.lens (\FirewallPolicyStatelessRuleGroupReferencesDetails' {priority} -> priority) (\s@FirewallPolicyStatelessRuleGroupReferencesDetails' {} a -> s {priority = a} :: FirewallPolicyStatelessRuleGroupReferencesDetails)

-- | The ARN of the stateless rule group.
firewallPolicyStatelessRuleGroupReferencesDetails_resourceArn :: Lens.Lens' FirewallPolicyStatelessRuleGroupReferencesDetails (Prelude.Maybe Prelude.Text)
firewallPolicyStatelessRuleGroupReferencesDetails_resourceArn = Lens.lens (\FirewallPolicyStatelessRuleGroupReferencesDetails' {resourceArn} -> resourceArn) (\s@FirewallPolicyStatelessRuleGroupReferencesDetails' {} a -> s {resourceArn = a} :: FirewallPolicyStatelessRuleGroupReferencesDetails)

instance
  Data.FromJSON
    FirewallPolicyStatelessRuleGroupReferencesDetails
  where
  parseJSON =
    Data.withObject
      "FirewallPolicyStatelessRuleGroupReferencesDetails"
      ( \x ->
          FirewallPolicyStatelessRuleGroupReferencesDetails'
            Prelude.<$> (x Data..:? "Priority")
              Prelude.<*> (x Data..:? "ResourceArn")
      )

instance
  Prelude.Hashable
    FirewallPolicyStatelessRuleGroupReferencesDetails
  where
  hashWithSalt
    _salt
    FirewallPolicyStatelessRuleGroupReferencesDetails' {..} =
      _salt `Prelude.hashWithSalt` priority
        `Prelude.hashWithSalt` resourceArn

instance
  Prelude.NFData
    FirewallPolicyStatelessRuleGroupReferencesDetails
  where
  rnf
    FirewallPolicyStatelessRuleGroupReferencesDetails' {..} =
      Prelude.rnf priority
        `Prelude.seq` Prelude.rnf resourceArn

instance
  Data.ToJSON
    FirewallPolicyStatelessRuleGroupReferencesDetails
  where
  toJSON
    FirewallPolicyStatelessRuleGroupReferencesDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Priority" Data..=) Prelude.<$> priority,
              ("ResourceArn" Data..=) Prelude.<$> resourceArn
            ]
        )
