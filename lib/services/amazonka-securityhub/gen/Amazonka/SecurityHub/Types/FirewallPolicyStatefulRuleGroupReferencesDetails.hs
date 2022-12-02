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
-- Module      : Amazonka.SecurityHub.Types.FirewallPolicyStatefulRuleGroupReferencesDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.FirewallPolicyStatefulRuleGroupReferencesDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A stateful rule group that is used by the firewall policy.
--
-- /See:/ 'newFirewallPolicyStatefulRuleGroupReferencesDetails' smart constructor.
data FirewallPolicyStatefulRuleGroupReferencesDetails = FirewallPolicyStatefulRuleGroupReferencesDetails'
  { -- | The ARN of the stateful rule group.
    resourceArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FirewallPolicyStatefulRuleGroupReferencesDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'firewallPolicyStatefulRuleGroupReferencesDetails_resourceArn' - The ARN of the stateful rule group.
newFirewallPolicyStatefulRuleGroupReferencesDetails ::
  FirewallPolicyStatefulRuleGroupReferencesDetails
newFirewallPolicyStatefulRuleGroupReferencesDetails =
  FirewallPolicyStatefulRuleGroupReferencesDetails'
    { resourceArn =
        Prelude.Nothing
    }

-- | The ARN of the stateful rule group.
firewallPolicyStatefulRuleGroupReferencesDetails_resourceArn :: Lens.Lens' FirewallPolicyStatefulRuleGroupReferencesDetails (Prelude.Maybe Prelude.Text)
firewallPolicyStatefulRuleGroupReferencesDetails_resourceArn = Lens.lens (\FirewallPolicyStatefulRuleGroupReferencesDetails' {resourceArn} -> resourceArn) (\s@FirewallPolicyStatefulRuleGroupReferencesDetails' {} a -> s {resourceArn = a} :: FirewallPolicyStatefulRuleGroupReferencesDetails)

instance
  Data.FromJSON
    FirewallPolicyStatefulRuleGroupReferencesDetails
  where
  parseJSON =
    Data.withObject
      "FirewallPolicyStatefulRuleGroupReferencesDetails"
      ( \x ->
          FirewallPolicyStatefulRuleGroupReferencesDetails'
            Prelude.<$> (x Data..:? "ResourceArn")
      )

instance
  Prelude.Hashable
    FirewallPolicyStatefulRuleGroupReferencesDetails
  where
  hashWithSalt
    _salt
    FirewallPolicyStatefulRuleGroupReferencesDetails' {..} =
      _salt `Prelude.hashWithSalt` resourceArn

instance
  Prelude.NFData
    FirewallPolicyStatefulRuleGroupReferencesDetails
  where
  rnf
    FirewallPolicyStatefulRuleGroupReferencesDetails' {..} =
      Prelude.rnf resourceArn

instance
  Data.ToJSON
    FirewallPolicyStatefulRuleGroupReferencesDetails
  where
  toJSON
    FirewallPolicyStatefulRuleGroupReferencesDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [("ResourceArn" Data..=) Prelude.<$> resourceArn]
        )
