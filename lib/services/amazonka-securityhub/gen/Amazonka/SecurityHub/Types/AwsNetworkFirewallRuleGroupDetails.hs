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
-- Module      : Amazonka.SecurityHub.Types.AwsNetworkFirewallRuleGroupDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsNetworkFirewallRuleGroupDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.RuleGroupDetails

-- | Details about an Network Firewall rule group. Rule groups are used to
-- inspect and control network traffic. Stateless rule groups apply to
-- individual packets. Stateful rule groups apply to packets in the context
-- of their traffic flow.
--
-- Rule groups are referenced in firewall policies.
--
-- /See:/ 'newAwsNetworkFirewallRuleGroupDetails' smart constructor.
data AwsNetworkFirewallRuleGroupDetails = AwsNetworkFirewallRuleGroupDetails'
  { -- | The descriptive name of the rule group.
    ruleGroupName :: Prelude.Maybe Prelude.Text,
    -- | Details about the rule group.
    ruleGroup :: Prelude.Maybe RuleGroupDetails,
    -- | The type of rule group. A rule group can be stateful or stateless.
    type' :: Prelude.Maybe Prelude.Text,
    -- | A description of the rule group.
    description :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the rule group.
    ruleGroupId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the rule group.
    ruleGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of operating resources that this rule group can use.
    capacity :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsNetworkFirewallRuleGroupDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleGroupName', 'awsNetworkFirewallRuleGroupDetails_ruleGroupName' - The descriptive name of the rule group.
--
-- 'ruleGroup', 'awsNetworkFirewallRuleGroupDetails_ruleGroup' - Details about the rule group.
--
-- 'type'', 'awsNetworkFirewallRuleGroupDetails_type' - The type of rule group. A rule group can be stateful or stateless.
--
-- 'description', 'awsNetworkFirewallRuleGroupDetails_description' - A description of the rule group.
--
-- 'ruleGroupId', 'awsNetworkFirewallRuleGroupDetails_ruleGroupId' - The identifier of the rule group.
--
-- 'ruleGroupArn', 'awsNetworkFirewallRuleGroupDetails_ruleGroupArn' - The ARN of the rule group.
--
-- 'capacity', 'awsNetworkFirewallRuleGroupDetails_capacity' - The maximum number of operating resources that this rule group can use.
newAwsNetworkFirewallRuleGroupDetails ::
  AwsNetworkFirewallRuleGroupDetails
newAwsNetworkFirewallRuleGroupDetails =
  AwsNetworkFirewallRuleGroupDetails'
    { ruleGroupName =
        Prelude.Nothing,
      ruleGroup = Prelude.Nothing,
      type' = Prelude.Nothing,
      description = Prelude.Nothing,
      ruleGroupId = Prelude.Nothing,
      ruleGroupArn = Prelude.Nothing,
      capacity = Prelude.Nothing
    }

-- | The descriptive name of the rule group.
awsNetworkFirewallRuleGroupDetails_ruleGroupName :: Lens.Lens' AwsNetworkFirewallRuleGroupDetails (Prelude.Maybe Prelude.Text)
awsNetworkFirewallRuleGroupDetails_ruleGroupName = Lens.lens (\AwsNetworkFirewallRuleGroupDetails' {ruleGroupName} -> ruleGroupName) (\s@AwsNetworkFirewallRuleGroupDetails' {} a -> s {ruleGroupName = a} :: AwsNetworkFirewallRuleGroupDetails)

-- | Details about the rule group.
awsNetworkFirewallRuleGroupDetails_ruleGroup :: Lens.Lens' AwsNetworkFirewallRuleGroupDetails (Prelude.Maybe RuleGroupDetails)
awsNetworkFirewallRuleGroupDetails_ruleGroup = Lens.lens (\AwsNetworkFirewallRuleGroupDetails' {ruleGroup} -> ruleGroup) (\s@AwsNetworkFirewallRuleGroupDetails' {} a -> s {ruleGroup = a} :: AwsNetworkFirewallRuleGroupDetails)

-- | The type of rule group. A rule group can be stateful or stateless.
awsNetworkFirewallRuleGroupDetails_type :: Lens.Lens' AwsNetworkFirewallRuleGroupDetails (Prelude.Maybe Prelude.Text)
awsNetworkFirewallRuleGroupDetails_type = Lens.lens (\AwsNetworkFirewallRuleGroupDetails' {type'} -> type') (\s@AwsNetworkFirewallRuleGroupDetails' {} a -> s {type' = a} :: AwsNetworkFirewallRuleGroupDetails)

-- | A description of the rule group.
awsNetworkFirewallRuleGroupDetails_description :: Lens.Lens' AwsNetworkFirewallRuleGroupDetails (Prelude.Maybe Prelude.Text)
awsNetworkFirewallRuleGroupDetails_description = Lens.lens (\AwsNetworkFirewallRuleGroupDetails' {description} -> description) (\s@AwsNetworkFirewallRuleGroupDetails' {} a -> s {description = a} :: AwsNetworkFirewallRuleGroupDetails)

-- | The identifier of the rule group.
awsNetworkFirewallRuleGroupDetails_ruleGroupId :: Lens.Lens' AwsNetworkFirewallRuleGroupDetails (Prelude.Maybe Prelude.Text)
awsNetworkFirewallRuleGroupDetails_ruleGroupId = Lens.lens (\AwsNetworkFirewallRuleGroupDetails' {ruleGroupId} -> ruleGroupId) (\s@AwsNetworkFirewallRuleGroupDetails' {} a -> s {ruleGroupId = a} :: AwsNetworkFirewallRuleGroupDetails)

-- | The ARN of the rule group.
awsNetworkFirewallRuleGroupDetails_ruleGroupArn :: Lens.Lens' AwsNetworkFirewallRuleGroupDetails (Prelude.Maybe Prelude.Text)
awsNetworkFirewallRuleGroupDetails_ruleGroupArn = Lens.lens (\AwsNetworkFirewallRuleGroupDetails' {ruleGroupArn} -> ruleGroupArn) (\s@AwsNetworkFirewallRuleGroupDetails' {} a -> s {ruleGroupArn = a} :: AwsNetworkFirewallRuleGroupDetails)

-- | The maximum number of operating resources that this rule group can use.
awsNetworkFirewallRuleGroupDetails_capacity :: Lens.Lens' AwsNetworkFirewallRuleGroupDetails (Prelude.Maybe Prelude.Int)
awsNetworkFirewallRuleGroupDetails_capacity = Lens.lens (\AwsNetworkFirewallRuleGroupDetails' {capacity} -> capacity) (\s@AwsNetworkFirewallRuleGroupDetails' {} a -> s {capacity = a} :: AwsNetworkFirewallRuleGroupDetails)

instance
  Core.FromJSON
    AwsNetworkFirewallRuleGroupDetails
  where
  parseJSON =
    Core.withObject
      "AwsNetworkFirewallRuleGroupDetails"
      ( \x ->
          AwsNetworkFirewallRuleGroupDetails'
            Prelude.<$> (x Core..:? "RuleGroupName")
            Prelude.<*> (x Core..:? "RuleGroup")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "RuleGroupId")
            Prelude.<*> (x Core..:? "RuleGroupArn")
            Prelude.<*> (x Core..:? "Capacity")
      )

instance
  Prelude.Hashable
    AwsNetworkFirewallRuleGroupDetails
  where
  hashWithSalt
    _salt
    AwsNetworkFirewallRuleGroupDetails' {..} =
      _salt `Prelude.hashWithSalt` ruleGroupName
        `Prelude.hashWithSalt` ruleGroup
        `Prelude.hashWithSalt` type'
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` ruleGroupId
        `Prelude.hashWithSalt` ruleGroupArn
        `Prelude.hashWithSalt` capacity

instance
  Prelude.NFData
    AwsNetworkFirewallRuleGroupDetails
  where
  rnf AwsNetworkFirewallRuleGroupDetails' {..} =
    Prelude.rnf ruleGroupName
      `Prelude.seq` Prelude.rnf ruleGroup
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf ruleGroupId
      `Prelude.seq` Prelude.rnf ruleGroupArn
      `Prelude.seq` Prelude.rnf capacity

instance
  Core.ToJSON
    AwsNetworkFirewallRuleGroupDetails
  where
  toJSON AwsNetworkFirewallRuleGroupDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RuleGroupName" Core..=) Prelude.<$> ruleGroupName,
            ("RuleGroup" Core..=) Prelude.<$> ruleGroup,
            ("Type" Core..=) Prelude.<$> type',
            ("Description" Core..=) Prelude.<$> description,
            ("RuleGroupId" Core..=) Prelude.<$> ruleGroupId,
            ("RuleGroupArn" Core..=) Prelude.<$> ruleGroupArn,
            ("Capacity" Core..=) Prelude.<$> capacity
          ]
      )
