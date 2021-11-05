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
-- Module      : Network.AWS.NetworkFirewall.Types.FirewallPolicyResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.NetworkFirewall.Types.FirewallPolicyResponse where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.NetworkFirewall.Types.ResourceStatus
import Network.AWS.NetworkFirewall.Types.Tag
import qualified Network.AWS.Prelude as Prelude

-- | The high-level properties of a firewall policy. This, along with the
-- FirewallPolicy, define the policy. You can retrieve all objects for a
-- firewall policy by calling DescribeFirewallPolicy.
--
-- /See:/ 'newFirewallPolicyResponse' smart constructor.
data FirewallPolicyResponse = FirewallPolicyResponse'
  { -- | The number of capacity units currently consumed by the policy\'s
    -- stateless rules.
    consumedStatelessRuleCapacity :: Prelude.Maybe Prelude.Int,
    -- | The number of firewalls that are associated with this firewall policy.
    numberOfAssociations :: Prelude.Maybe Prelude.Int,
    -- | The current status of the firewall policy. You can retrieve this for a
    -- firewall policy by calling DescribeFirewallPolicy and providing the
    -- firewall policy\'s name or ARN.
    firewallPolicyStatus :: Prelude.Maybe ResourceStatus,
    -- | The number of capacity units currently consumed by the policy\'s
    -- stateful rules.
    consumedStatefulRuleCapacity :: Prelude.Maybe Prelude.Int,
    -- | A description of the firewall policy.
    description :: Prelude.Maybe Prelude.Text,
    -- | The key:value pairs to associate with the resource.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The descriptive name of the firewall policy. You can\'t change the name
    -- of a firewall policy after you create it.
    firewallPolicyName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the firewall policy.
    --
    -- If this response is for a create request that had @DryRun@ set to
    -- @TRUE@, then this ARN is a placeholder that isn\'t attached to a valid
    -- resource.
    firewallPolicyArn :: Prelude.Text,
    -- | The unique identifier for the firewall policy.
    firewallPolicyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FirewallPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'consumedStatelessRuleCapacity', 'firewallPolicyResponse_consumedStatelessRuleCapacity' - The number of capacity units currently consumed by the policy\'s
-- stateless rules.
--
-- 'numberOfAssociations', 'firewallPolicyResponse_numberOfAssociations' - The number of firewalls that are associated with this firewall policy.
--
-- 'firewallPolicyStatus', 'firewallPolicyResponse_firewallPolicyStatus' - The current status of the firewall policy. You can retrieve this for a
-- firewall policy by calling DescribeFirewallPolicy and providing the
-- firewall policy\'s name or ARN.
--
-- 'consumedStatefulRuleCapacity', 'firewallPolicyResponse_consumedStatefulRuleCapacity' - The number of capacity units currently consumed by the policy\'s
-- stateful rules.
--
-- 'description', 'firewallPolicyResponse_description' - A description of the firewall policy.
--
-- 'tags', 'firewallPolicyResponse_tags' - The key:value pairs to associate with the resource.
--
-- 'firewallPolicyName', 'firewallPolicyResponse_firewallPolicyName' - The descriptive name of the firewall policy. You can\'t change the name
-- of a firewall policy after you create it.
--
-- 'firewallPolicyArn', 'firewallPolicyResponse_firewallPolicyArn' - The Amazon Resource Name (ARN) of the firewall policy.
--
-- If this response is for a create request that had @DryRun@ set to
-- @TRUE@, then this ARN is a placeholder that isn\'t attached to a valid
-- resource.
--
-- 'firewallPolicyId', 'firewallPolicyResponse_firewallPolicyId' - The unique identifier for the firewall policy.
newFirewallPolicyResponse ::
  -- | 'firewallPolicyName'
  Prelude.Text ->
  -- | 'firewallPolicyArn'
  Prelude.Text ->
  -- | 'firewallPolicyId'
  Prelude.Text ->
  FirewallPolicyResponse
newFirewallPolicyResponse
  pFirewallPolicyName_
  pFirewallPolicyArn_
  pFirewallPolicyId_ =
    FirewallPolicyResponse'
      { consumedStatelessRuleCapacity =
          Prelude.Nothing,
        numberOfAssociations = Prelude.Nothing,
        firewallPolicyStatus = Prelude.Nothing,
        consumedStatefulRuleCapacity = Prelude.Nothing,
        description = Prelude.Nothing,
        tags = Prelude.Nothing,
        firewallPolicyName = pFirewallPolicyName_,
        firewallPolicyArn = pFirewallPolicyArn_,
        firewallPolicyId = pFirewallPolicyId_
      }

-- | The number of capacity units currently consumed by the policy\'s
-- stateless rules.
firewallPolicyResponse_consumedStatelessRuleCapacity :: Lens.Lens' FirewallPolicyResponse (Prelude.Maybe Prelude.Int)
firewallPolicyResponse_consumedStatelessRuleCapacity = Lens.lens (\FirewallPolicyResponse' {consumedStatelessRuleCapacity} -> consumedStatelessRuleCapacity) (\s@FirewallPolicyResponse' {} a -> s {consumedStatelessRuleCapacity = a} :: FirewallPolicyResponse)

-- | The number of firewalls that are associated with this firewall policy.
firewallPolicyResponse_numberOfAssociations :: Lens.Lens' FirewallPolicyResponse (Prelude.Maybe Prelude.Int)
firewallPolicyResponse_numberOfAssociations = Lens.lens (\FirewallPolicyResponse' {numberOfAssociations} -> numberOfAssociations) (\s@FirewallPolicyResponse' {} a -> s {numberOfAssociations = a} :: FirewallPolicyResponse)

-- | The current status of the firewall policy. You can retrieve this for a
-- firewall policy by calling DescribeFirewallPolicy and providing the
-- firewall policy\'s name or ARN.
firewallPolicyResponse_firewallPolicyStatus :: Lens.Lens' FirewallPolicyResponse (Prelude.Maybe ResourceStatus)
firewallPolicyResponse_firewallPolicyStatus = Lens.lens (\FirewallPolicyResponse' {firewallPolicyStatus} -> firewallPolicyStatus) (\s@FirewallPolicyResponse' {} a -> s {firewallPolicyStatus = a} :: FirewallPolicyResponse)

-- | The number of capacity units currently consumed by the policy\'s
-- stateful rules.
firewallPolicyResponse_consumedStatefulRuleCapacity :: Lens.Lens' FirewallPolicyResponse (Prelude.Maybe Prelude.Int)
firewallPolicyResponse_consumedStatefulRuleCapacity = Lens.lens (\FirewallPolicyResponse' {consumedStatefulRuleCapacity} -> consumedStatefulRuleCapacity) (\s@FirewallPolicyResponse' {} a -> s {consumedStatefulRuleCapacity = a} :: FirewallPolicyResponse)

-- | A description of the firewall policy.
firewallPolicyResponse_description :: Lens.Lens' FirewallPolicyResponse (Prelude.Maybe Prelude.Text)
firewallPolicyResponse_description = Lens.lens (\FirewallPolicyResponse' {description} -> description) (\s@FirewallPolicyResponse' {} a -> s {description = a} :: FirewallPolicyResponse)

-- | The key:value pairs to associate with the resource.
firewallPolicyResponse_tags :: Lens.Lens' FirewallPolicyResponse (Prelude.Maybe (Prelude.NonEmpty Tag))
firewallPolicyResponse_tags = Lens.lens (\FirewallPolicyResponse' {tags} -> tags) (\s@FirewallPolicyResponse' {} a -> s {tags = a} :: FirewallPolicyResponse) Prelude.. Lens.mapping Lens.coerced

-- | The descriptive name of the firewall policy. You can\'t change the name
-- of a firewall policy after you create it.
firewallPolicyResponse_firewallPolicyName :: Lens.Lens' FirewallPolicyResponse Prelude.Text
firewallPolicyResponse_firewallPolicyName = Lens.lens (\FirewallPolicyResponse' {firewallPolicyName} -> firewallPolicyName) (\s@FirewallPolicyResponse' {} a -> s {firewallPolicyName = a} :: FirewallPolicyResponse)

-- | The Amazon Resource Name (ARN) of the firewall policy.
--
-- If this response is for a create request that had @DryRun@ set to
-- @TRUE@, then this ARN is a placeholder that isn\'t attached to a valid
-- resource.
firewallPolicyResponse_firewallPolicyArn :: Lens.Lens' FirewallPolicyResponse Prelude.Text
firewallPolicyResponse_firewallPolicyArn = Lens.lens (\FirewallPolicyResponse' {firewallPolicyArn} -> firewallPolicyArn) (\s@FirewallPolicyResponse' {} a -> s {firewallPolicyArn = a} :: FirewallPolicyResponse)

-- | The unique identifier for the firewall policy.
firewallPolicyResponse_firewallPolicyId :: Lens.Lens' FirewallPolicyResponse Prelude.Text
firewallPolicyResponse_firewallPolicyId = Lens.lens (\FirewallPolicyResponse' {firewallPolicyId} -> firewallPolicyId) (\s@FirewallPolicyResponse' {} a -> s {firewallPolicyId = a} :: FirewallPolicyResponse)

instance Core.FromJSON FirewallPolicyResponse where
  parseJSON =
    Core.withObject
      "FirewallPolicyResponse"
      ( \x ->
          FirewallPolicyResponse'
            Prelude.<$> (x Core..:? "ConsumedStatelessRuleCapacity")
            Prelude.<*> (x Core..:? "NumberOfAssociations")
            Prelude.<*> (x Core..:? "FirewallPolicyStatus")
            Prelude.<*> (x Core..:? "ConsumedStatefulRuleCapacity")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "Tags")
            Prelude.<*> (x Core..: "FirewallPolicyName")
            Prelude.<*> (x Core..: "FirewallPolicyArn")
            Prelude.<*> (x Core..: "FirewallPolicyId")
      )

instance Prelude.Hashable FirewallPolicyResponse

instance Prelude.NFData FirewallPolicyResponse
