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
-- Module      : Amazonka.NetworkFirewall.Types.StatefulRuleGroupReference
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.StatefulRuleGroupReference where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types.StatefulRuleGroupOverride
import qualified Amazonka.Prelude as Prelude

-- | Identifier for a single stateful rule group, used in a firewall policy
-- to refer to a rule group.
--
-- /See:/ 'newStatefulRuleGroupReference' smart constructor.
data StatefulRuleGroupReference = StatefulRuleGroupReference'
  { -- | The action that allows the policy owner to override the behavior of the
    -- rule group within a policy.
    override :: Prelude.Maybe StatefulRuleGroupOverride,
    -- | An integer setting that indicates the order in which to run the stateful
    -- rule groups in a single FirewallPolicy. This setting only applies to
    -- firewall policies that specify the @STRICT_ORDER@ rule order in the
    -- stateful engine options settings.
    --
    -- Network Firewall evalutes each stateful rule group against a packet
    -- starting with the group that has the lowest priority setting. You must
    -- ensure that the priority settings are unique within each policy.
    --
    -- You can change the priority settings of your rule groups at any time. To
    -- make it easier to insert rule groups later, number them so there\'s a
    -- wide range in between, for example use 100, 200, and so on.
    priority :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the stateful rule group.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StatefulRuleGroupReference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'override', 'statefulRuleGroupReference_override' - The action that allows the policy owner to override the behavior of the
-- rule group within a policy.
--
-- 'priority', 'statefulRuleGroupReference_priority' - An integer setting that indicates the order in which to run the stateful
-- rule groups in a single FirewallPolicy. This setting only applies to
-- firewall policies that specify the @STRICT_ORDER@ rule order in the
-- stateful engine options settings.
--
-- Network Firewall evalutes each stateful rule group against a packet
-- starting with the group that has the lowest priority setting. You must
-- ensure that the priority settings are unique within each policy.
--
-- You can change the priority settings of your rule groups at any time. To
-- make it easier to insert rule groups later, number them so there\'s a
-- wide range in between, for example use 100, 200, and so on.
--
-- 'resourceArn', 'statefulRuleGroupReference_resourceArn' - The Amazon Resource Name (ARN) of the stateful rule group.
newStatefulRuleGroupReference ::
  -- | 'resourceArn'
  Prelude.Text ->
  StatefulRuleGroupReference
newStatefulRuleGroupReference pResourceArn_ =
  StatefulRuleGroupReference'
    { override =
        Prelude.Nothing,
      priority = Prelude.Nothing,
      resourceArn = pResourceArn_
    }

-- | The action that allows the policy owner to override the behavior of the
-- rule group within a policy.
statefulRuleGroupReference_override :: Lens.Lens' StatefulRuleGroupReference (Prelude.Maybe StatefulRuleGroupOverride)
statefulRuleGroupReference_override = Lens.lens (\StatefulRuleGroupReference' {override} -> override) (\s@StatefulRuleGroupReference' {} a -> s {override = a} :: StatefulRuleGroupReference)

-- | An integer setting that indicates the order in which to run the stateful
-- rule groups in a single FirewallPolicy. This setting only applies to
-- firewall policies that specify the @STRICT_ORDER@ rule order in the
-- stateful engine options settings.
--
-- Network Firewall evalutes each stateful rule group against a packet
-- starting with the group that has the lowest priority setting. You must
-- ensure that the priority settings are unique within each policy.
--
-- You can change the priority settings of your rule groups at any time. To
-- make it easier to insert rule groups later, number them so there\'s a
-- wide range in between, for example use 100, 200, and so on.
statefulRuleGroupReference_priority :: Lens.Lens' StatefulRuleGroupReference (Prelude.Maybe Prelude.Natural)
statefulRuleGroupReference_priority = Lens.lens (\StatefulRuleGroupReference' {priority} -> priority) (\s@StatefulRuleGroupReference' {} a -> s {priority = a} :: StatefulRuleGroupReference)

-- | The Amazon Resource Name (ARN) of the stateful rule group.
statefulRuleGroupReference_resourceArn :: Lens.Lens' StatefulRuleGroupReference Prelude.Text
statefulRuleGroupReference_resourceArn = Lens.lens (\StatefulRuleGroupReference' {resourceArn} -> resourceArn) (\s@StatefulRuleGroupReference' {} a -> s {resourceArn = a} :: StatefulRuleGroupReference)

instance Data.FromJSON StatefulRuleGroupReference where
  parseJSON =
    Data.withObject
      "StatefulRuleGroupReference"
      ( \x ->
          StatefulRuleGroupReference'
            Prelude.<$> (x Data..:? "Override")
            Prelude.<*> (x Data..:? "Priority")
            Prelude.<*> (x Data..: "ResourceArn")
      )

instance Prelude.Hashable StatefulRuleGroupReference where
  hashWithSalt _salt StatefulRuleGroupReference' {..} =
    _salt `Prelude.hashWithSalt` override
      `Prelude.hashWithSalt` priority
      `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData StatefulRuleGroupReference where
  rnf StatefulRuleGroupReference' {..} =
    Prelude.rnf override
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf resourceArn

instance Data.ToJSON StatefulRuleGroupReference where
  toJSON StatefulRuleGroupReference' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Override" Data..=) Prelude.<$> override,
            ("Priority" Data..=) Prelude.<$> priority,
            Prelude.Just ("ResourceArn" Data..= resourceArn)
          ]
      )
