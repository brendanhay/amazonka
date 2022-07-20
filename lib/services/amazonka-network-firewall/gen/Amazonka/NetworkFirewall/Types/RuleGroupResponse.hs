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
-- Module      : Amazonka.NetworkFirewall.Types.RuleGroupResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.RuleGroupResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.NetworkFirewall.Types.ResourceStatus
import Amazonka.NetworkFirewall.Types.RuleGroupType
import Amazonka.NetworkFirewall.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | The high-level properties of a rule group. This, along with the
-- RuleGroup, define the rule group. You can retrieve all objects for a
-- rule group by calling DescribeRuleGroup.
--
-- /See:/ 'newRuleGroupResponse' smart constructor.
data RuleGroupResponse = RuleGroupResponse'
  { -- | The key:value pairs to associate with the resource.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | Indicates whether the rule group is stateless or stateful. If the rule
    -- group is stateless, it contains stateless rules. If it is stateful, it
    -- contains stateful rules.
    type' :: Prelude.Maybe RuleGroupType,
    -- | A description of the rule group.
    description :: Prelude.Maybe Prelude.Text,
    -- | The number of capacity units currently consumed by the rule group rules.
    consumedCapacity :: Prelude.Maybe Prelude.Int,
    -- | The number of firewall policies that use this rule group.
    numberOfAssociations :: Prelude.Maybe Prelude.Int,
    -- | The maximum operating resources that this rule group can use. Rule group
    -- capacity is fixed at creation. When you update a rule group, you are
    -- limited to this capacity. When you reference a rule group from a
    -- firewall policy, Network Firewall reserves this capacity for the rule
    -- group.
    --
    -- You can retrieve the capacity that would be required for a rule group
    -- before you create the rule group by calling CreateRuleGroup with
    -- @DryRun@ set to @TRUE@.
    capacity :: Prelude.Maybe Prelude.Int,
    -- | Detailed information about the current status of a rule group.
    ruleGroupStatus :: Prelude.Maybe ResourceStatus,
    -- | The Amazon Resource Name (ARN) of the rule group.
    --
    -- If this response is for a create request that had @DryRun@ set to
    -- @TRUE@, then this ARN is a placeholder that isn\'t attached to a valid
    -- resource.
    ruleGroupArn :: Prelude.Text,
    -- | The descriptive name of the rule group. You can\'t change the name of a
    -- rule group after you create it.
    ruleGroupName :: Prelude.Text,
    -- | The unique identifier for the rule group.
    ruleGroupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'ruleGroupResponse_tags' - The key:value pairs to associate with the resource.
--
-- 'type'', 'ruleGroupResponse_type' - Indicates whether the rule group is stateless or stateful. If the rule
-- group is stateless, it contains stateless rules. If it is stateful, it
-- contains stateful rules.
--
-- 'description', 'ruleGroupResponse_description' - A description of the rule group.
--
-- 'consumedCapacity', 'ruleGroupResponse_consumedCapacity' - The number of capacity units currently consumed by the rule group rules.
--
-- 'numberOfAssociations', 'ruleGroupResponse_numberOfAssociations' - The number of firewall policies that use this rule group.
--
-- 'capacity', 'ruleGroupResponse_capacity' - The maximum operating resources that this rule group can use. Rule group
-- capacity is fixed at creation. When you update a rule group, you are
-- limited to this capacity. When you reference a rule group from a
-- firewall policy, Network Firewall reserves this capacity for the rule
-- group.
--
-- You can retrieve the capacity that would be required for a rule group
-- before you create the rule group by calling CreateRuleGroup with
-- @DryRun@ set to @TRUE@.
--
-- 'ruleGroupStatus', 'ruleGroupResponse_ruleGroupStatus' - Detailed information about the current status of a rule group.
--
-- 'ruleGroupArn', 'ruleGroupResponse_ruleGroupArn' - The Amazon Resource Name (ARN) of the rule group.
--
-- If this response is for a create request that had @DryRun@ set to
-- @TRUE@, then this ARN is a placeholder that isn\'t attached to a valid
-- resource.
--
-- 'ruleGroupName', 'ruleGroupResponse_ruleGroupName' - The descriptive name of the rule group. You can\'t change the name of a
-- rule group after you create it.
--
-- 'ruleGroupId', 'ruleGroupResponse_ruleGroupId' - The unique identifier for the rule group.
newRuleGroupResponse ::
  -- | 'ruleGroupArn'
  Prelude.Text ->
  -- | 'ruleGroupName'
  Prelude.Text ->
  -- | 'ruleGroupId'
  Prelude.Text ->
  RuleGroupResponse
newRuleGroupResponse
  pRuleGroupArn_
  pRuleGroupName_
  pRuleGroupId_ =
    RuleGroupResponse'
      { tags = Prelude.Nothing,
        type' = Prelude.Nothing,
        description = Prelude.Nothing,
        consumedCapacity = Prelude.Nothing,
        numberOfAssociations = Prelude.Nothing,
        capacity = Prelude.Nothing,
        ruleGroupStatus = Prelude.Nothing,
        ruleGroupArn = pRuleGroupArn_,
        ruleGroupName = pRuleGroupName_,
        ruleGroupId = pRuleGroupId_
      }

-- | The key:value pairs to associate with the resource.
ruleGroupResponse_tags :: Lens.Lens' RuleGroupResponse (Prelude.Maybe (Prelude.NonEmpty Tag))
ruleGroupResponse_tags = Lens.lens (\RuleGroupResponse' {tags} -> tags) (\s@RuleGroupResponse' {} a -> s {tags = a} :: RuleGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether the rule group is stateless or stateful. If the rule
-- group is stateless, it contains stateless rules. If it is stateful, it
-- contains stateful rules.
ruleGroupResponse_type :: Lens.Lens' RuleGroupResponse (Prelude.Maybe RuleGroupType)
ruleGroupResponse_type = Lens.lens (\RuleGroupResponse' {type'} -> type') (\s@RuleGroupResponse' {} a -> s {type' = a} :: RuleGroupResponse)

-- | A description of the rule group.
ruleGroupResponse_description :: Lens.Lens' RuleGroupResponse (Prelude.Maybe Prelude.Text)
ruleGroupResponse_description = Lens.lens (\RuleGroupResponse' {description} -> description) (\s@RuleGroupResponse' {} a -> s {description = a} :: RuleGroupResponse)

-- | The number of capacity units currently consumed by the rule group rules.
ruleGroupResponse_consumedCapacity :: Lens.Lens' RuleGroupResponse (Prelude.Maybe Prelude.Int)
ruleGroupResponse_consumedCapacity = Lens.lens (\RuleGroupResponse' {consumedCapacity} -> consumedCapacity) (\s@RuleGroupResponse' {} a -> s {consumedCapacity = a} :: RuleGroupResponse)

-- | The number of firewall policies that use this rule group.
ruleGroupResponse_numberOfAssociations :: Lens.Lens' RuleGroupResponse (Prelude.Maybe Prelude.Int)
ruleGroupResponse_numberOfAssociations = Lens.lens (\RuleGroupResponse' {numberOfAssociations} -> numberOfAssociations) (\s@RuleGroupResponse' {} a -> s {numberOfAssociations = a} :: RuleGroupResponse)

-- | The maximum operating resources that this rule group can use. Rule group
-- capacity is fixed at creation. When you update a rule group, you are
-- limited to this capacity. When you reference a rule group from a
-- firewall policy, Network Firewall reserves this capacity for the rule
-- group.
--
-- You can retrieve the capacity that would be required for a rule group
-- before you create the rule group by calling CreateRuleGroup with
-- @DryRun@ set to @TRUE@.
ruleGroupResponse_capacity :: Lens.Lens' RuleGroupResponse (Prelude.Maybe Prelude.Int)
ruleGroupResponse_capacity = Lens.lens (\RuleGroupResponse' {capacity} -> capacity) (\s@RuleGroupResponse' {} a -> s {capacity = a} :: RuleGroupResponse)

-- | Detailed information about the current status of a rule group.
ruleGroupResponse_ruleGroupStatus :: Lens.Lens' RuleGroupResponse (Prelude.Maybe ResourceStatus)
ruleGroupResponse_ruleGroupStatus = Lens.lens (\RuleGroupResponse' {ruleGroupStatus} -> ruleGroupStatus) (\s@RuleGroupResponse' {} a -> s {ruleGroupStatus = a} :: RuleGroupResponse)

-- | The Amazon Resource Name (ARN) of the rule group.
--
-- If this response is for a create request that had @DryRun@ set to
-- @TRUE@, then this ARN is a placeholder that isn\'t attached to a valid
-- resource.
ruleGroupResponse_ruleGroupArn :: Lens.Lens' RuleGroupResponse Prelude.Text
ruleGroupResponse_ruleGroupArn = Lens.lens (\RuleGroupResponse' {ruleGroupArn} -> ruleGroupArn) (\s@RuleGroupResponse' {} a -> s {ruleGroupArn = a} :: RuleGroupResponse)

-- | The descriptive name of the rule group. You can\'t change the name of a
-- rule group after you create it.
ruleGroupResponse_ruleGroupName :: Lens.Lens' RuleGroupResponse Prelude.Text
ruleGroupResponse_ruleGroupName = Lens.lens (\RuleGroupResponse' {ruleGroupName} -> ruleGroupName) (\s@RuleGroupResponse' {} a -> s {ruleGroupName = a} :: RuleGroupResponse)

-- | The unique identifier for the rule group.
ruleGroupResponse_ruleGroupId :: Lens.Lens' RuleGroupResponse Prelude.Text
ruleGroupResponse_ruleGroupId = Lens.lens (\RuleGroupResponse' {ruleGroupId} -> ruleGroupId) (\s@RuleGroupResponse' {} a -> s {ruleGroupId = a} :: RuleGroupResponse)

instance Core.FromJSON RuleGroupResponse where
  parseJSON =
    Core.withObject
      "RuleGroupResponse"
      ( \x ->
          RuleGroupResponse'
            Prelude.<$> (x Core..:? "Tags")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "ConsumedCapacity")
            Prelude.<*> (x Core..:? "NumberOfAssociations")
            Prelude.<*> (x Core..:? "Capacity")
            Prelude.<*> (x Core..:? "RuleGroupStatus")
            Prelude.<*> (x Core..: "RuleGroupArn")
            Prelude.<*> (x Core..: "RuleGroupName")
            Prelude.<*> (x Core..: "RuleGroupId")
      )

instance Prelude.Hashable RuleGroupResponse where
  hashWithSalt _salt RuleGroupResponse' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` consumedCapacity
      `Prelude.hashWithSalt` numberOfAssociations
      `Prelude.hashWithSalt` capacity
      `Prelude.hashWithSalt` ruleGroupStatus
      `Prelude.hashWithSalt` ruleGroupArn
      `Prelude.hashWithSalt` ruleGroupName
      `Prelude.hashWithSalt` ruleGroupId

instance Prelude.NFData RuleGroupResponse where
  rnf RuleGroupResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf consumedCapacity
      `Prelude.seq` Prelude.rnf numberOfAssociations
      `Prelude.seq` Prelude.rnf capacity
      `Prelude.seq` Prelude.rnf ruleGroupStatus
      `Prelude.seq` Prelude.rnf ruleGroupArn
      `Prelude.seq` Prelude.rnf ruleGroupName
      `Prelude.seq` Prelude.rnf ruleGroupId
