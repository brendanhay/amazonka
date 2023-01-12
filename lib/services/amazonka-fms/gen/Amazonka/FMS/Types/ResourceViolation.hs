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
-- Module      : Amazonka.FMS.Types.ResourceViolation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.ResourceViolation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types.AwsEc2InstanceViolation
import Amazonka.FMS.Types.AwsEc2NetworkInterfaceViolation
import Amazonka.FMS.Types.AwsVPCSecurityGroupViolation
import Amazonka.FMS.Types.DnsDuplicateRuleGroupViolation
import Amazonka.FMS.Types.DnsRuleGroupLimitExceededViolation
import Amazonka.FMS.Types.DnsRuleGroupPriorityConflictViolation
import Amazonka.FMS.Types.FirewallSubnetIsOutOfScopeViolation
import Amazonka.FMS.Types.FirewallSubnetMissingVPCEndpointViolation
import Amazonka.FMS.Types.NetworkFirewallBlackHoleRouteDetectedViolation
import Amazonka.FMS.Types.NetworkFirewallInternetTrafficNotInspectedViolation
import Amazonka.FMS.Types.NetworkFirewallInvalidRouteConfigurationViolation
import Amazonka.FMS.Types.NetworkFirewallMissingExpectedRTViolation
import Amazonka.FMS.Types.NetworkFirewallMissingExpectedRoutesViolation
import Amazonka.FMS.Types.NetworkFirewallMissingFirewallViolation
import Amazonka.FMS.Types.NetworkFirewallMissingSubnetViolation
import Amazonka.FMS.Types.NetworkFirewallPolicyModifiedViolation
import Amazonka.FMS.Types.NetworkFirewallUnexpectedFirewallRoutesViolation
import Amazonka.FMS.Types.NetworkFirewallUnexpectedGatewayRoutesViolation
import Amazonka.FMS.Types.PossibleRemediationActions
import Amazonka.FMS.Types.RouteHasOutOfScopeEndpointViolation
import Amazonka.FMS.Types.ThirdPartyFirewallMissingExpectedRouteTableViolation
import Amazonka.FMS.Types.ThirdPartyFirewallMissingFirewallViolation
import Amazonka.FMS.Types.ThirdPartyFirewallMissingSubnetViolation
import qualified Amazonka.Prelude as Prelude

-- | Violation detail based on resource type.
--
-- /See:/ 'newResourceViolation' smart constructor.
data ResourceViolation = ResourceViolation'
  { -- | Violation detail for an EC2 instance.
    awsEc2InstanceViolation :: Prelude.Maybe AwsEc2InstanceViolation,
    -- | Violation detail for a network interface.
    awsEc2NetworkInterfaceViolation :: Prelude.Maybe AwsEc2NetworkInterfaceViolation,
    -- | Violation detail for security groups.
    awsVPCSecurityGroupViolation :: Prelude.Maybe AwsVPCSecurityGroupViolation,
    -- | Violation detail for a DNS Firewall policy that indicates that a rule
    -- group that Firewall Manager tried to associate with a VPC is already
    -- associated with the VPC and can\'t be associated again.
    dnsDuplicateRuleGroupViolation :: Prelude.Maybe DnsDuplicateRuleGroupViolation,
    -- | Violation detail for a DNS Firewall policy that indicates that the VPC
    -- reached the limit for associated DNS Firewall rule groups. Firewall
    -- Manager tried to associate another rule group with the VPC and failed.
    dnsRuleGroupLimitExceededViolation :: Prelude.Maybe DnsRuleGroupLimitExceededViolation,
    -- | Violation detail for a DNS Firewall policy that indicates that a rule
    -- group that Firewall Manager tried to associate with a VPC has the same
    -- priority as a rule group that\'s already associated.
    dnsRuleGroupPriorityConflictViolation :: Prelude.Maybe DnsRuleGroupPriorityConflictViolation,
    -- | Contains details about the firewall subnet that violates the policy
    -- scope.
    firewallSubnetIsOutOfScopeViolation :: Prelude.Maybe FirewallSubnetIsOutOfScopeViolation,
    -- | The violation details for a third-party firewall\'s VPC endpoint subnet
    -- that was deleted.
    firewallSubnetMissingVPCEndpointViolation :: Prelude.Maybe FirewallSubnetMissingVPCEndpointViolation,
    networkFirewallBlackHoleRouteDetectedViolation :: Prelude.Maybe NetworkFirewallBlackHoleRouteDetectedViolation,
    -- | Violation detail for the subnet for which internet traffic hasn\'t been
    -- inspected.
    networkFirewallInternetTrafficNotInspectedViolation :: Prelude.Maybe NetworkFirewallInternetTrafficNotInspectedViolation,
    -- | The route configuration is invalid.
    networkFirewallInvalidRouteConfigurationViolation :: Prelude.Maybe NetworkFirewallInvalidRouteConfigurationViolation,
    -- | Violation detail for an Network Firewall policy that indicates that a
    -- subnet is not associated with the expected Firewall Manager managed
    -- route table.
    networkFirewallMissingExpectedRTViolation :: Prelude.Maybe NetworkFirewallMissingExpectedRTViolation,
    -- | Expected routes are missing from Network Firewall.
    networkFirewallMissingExpectedRoutesViolation :: Prelude.Maybe NetworkFirewallMissingExpectedRoutesViolation,
    -- | Violation detail for an Network Firewall policy that indicates that a
    -- subnet has no Firewall Manager managed firewall in its VPC.
    networkFirewallMissingFirewallViolation :: Prelude.Maybe NetworkFirewallMissingFirewallViolation,
    -- | Violation detail for an Network Firewall policy that indicates that an
    -- Availability Zone is missing the expected Firewall Manager managed
    -- subnet.
    networkFirewallMissingSubnetViolation :: Prelude.Maybe NetworkFirewallMissingSubnetViolation,
    -- | Violation detail for an Network Firewall policy that indicates that a
    -- firewall policy in an individual account has been modified in a way that
    -- makes it noncompliant. For example, the individual account owner might
    -- have deleted a rule group, changed the priority of a stateless rule
    -- group, or changed a policy default action.
    networkFirewallPolicyModifiedViolation :: Prelude.Maybe NetworkFirewallPolicyModifiedViolation,
    -- | There\'s an unexpected firewall route.
    networkFirewallUnexpectedFirewallRoutesViolation :: Prelude.Maybe NetworkFirewallUnexpectedFirewallRoutesViolation,
    -- | There\'s an unexpected gateway route.
    networkFirewallUnexpectedGatewayRoutesViolation :: Prelude.Maybe NetworkFirewallUnexpectedGatewayRoutesViolation,
    -- | A list of possible remediation action lists. Each individual possible
    -- remediation action is a list of individual remediation actions.
    possibleRemediationActions :: Prelude.Maybe PossibleRemediationActions,
    -- | Contains details about the route endpoint that violates the policy
    -- scope.
    routeHasOutOfScopeEndpointViolation :: Prelude.Maybe RouteHasOutOfScopeEndpointViolation,
    -- | The violation details for a third-party firewall that has the Firewall
    -- Manager managed route table that was associated with the third-party
    -- firewall has been deleted.
    thirdPartyFirewallMissingExpectedRouteTableViolation :: Prelude.Maybe ThirdPartyFirewallMissingExpectedRouteTableViolation,
    -- | The violation details for a third-party firewall that\'s been deleted.
    thirdPartyFirewallMissingFirewallViolation :: Prelude.Maybe ThirdPartyFirewallMissingFirewallViolation,
    -- | The violation details for a third-party firewall\'s subnet that\'s been
    -- deleted.
    thirdPartyFirewallMissingSubnetViolation :: Prelude.Maybe ThirdPartyFirewallMissingSubnetViolation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceViolation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsEc2InstanceViolation', 'resourceViolation_awsEc2InstanceViolation' - Violation detail for an EC2 instance.
--
-- 'awsEc2NetworkInterfaceViolation', 'resourceViolation_awsEc2NetworkInterfaceViolation' - Violation detail for a network interface.
--
-- 'awsVPCSecurityGroupViolation', 'resourceViolation_awsVPCSecurityGroupViolation' - Violation detail for security groups.
--
-- 'dnsDuplicateRuleGroupViolation', 'resourceViolation_dnsDuplicateRuleGroupViolation' - Violation detail for a DNS Firewall policy that indicates that a rule
-- group that Firewall Manager tried to associate with a VPC is already
-- associated with the VPC and can\'t be associated again.
--
-- 'dnsRuleGroupLimitExceededViolation', 'resourceViolation_dnsRuleGroupLimitExceededViolation' - Violation detail for a DNS Firewall policy that indicates that the VPC
-- reached the limit for associated DNS Firewall rule groups. Firewall
-- Manager tried to associate another rule group with the VPC and failed.
--
-- 'dnsRuleGroupPriorityConflictViolation', 'resourceViolation_dnsRuleGroupPriorityConflictViolation' - Violation detail for a DNS Firewall policy that indicates that a rule
-- group that Firewall Manager tried to associate with a VPC has the same
-- priority as a rule group that\'s already associated.
--
-- 'firewallSubnetIsOutOfScopeViolation', 'resourceViolation_firewallSubnetIsOutOfScopeViolation' - Contains details about the firewall subnet that violates the policy
-- scope.
--
-- 'firewallSubnetMissingVPCEndpointViolation', 'resourceViolation_firewallSubnetMissingVPCEndpointViolation' - The violation details for a third-party firewall\'s VPC endpoint subnet
-- that was deleted.
--
-- 'networkFirewallBlackHoleRouteDetectedViolation', 'resourceViolation_networkFirewallBlackHoleRouteDetectedViolation' - Undocumented member.
--
-- 'networkFirewallInternetTrafficNotInspectedViolation', 'resourceViolation_networkFirewallInternetTrafficNotInspectedViolation' - Violation detail for the subnet for which internet traffic hasn\'t been
-- inspected.
--
-- 'networkFirewallInvalidRouteConfigurationViolation', 'resourceViolation_networkFirewallInvalidRouteConfigurationViolation' - The route configuration is invalid.
--
-- 'networkFirewallMissingExpectedRTViolation', 'resourceViolation_networkFirewallMissingExpectedRTViolation' - Violation detail for an Network Firewall policy that indicates that a
-- subnet is not associated with the expected Firewall Manager managed
-- route table.
--
-- 'networkFirewallMissingExpectedRoutesViolation', 'resourceViolation_networkFirewallMissingExpectedRoutesViolation' - Expected routes are missing from Network Firewall.
--
-- 'networkFirewallMissingFirewallViolation', 'resourceViolation_networkFirewallMissingFirewallViolation' - Violation detail for an Network Firewall policy that indicates that a
-- subnet has no Firewall Manager managed firewall in its VPC.
--
-- 'networkFirewallMissingSubnetViolation', 'resourceViolation_networkFirewallMissingSubnetViolation' - Violation detail for an Network Firewall policy that indicates that an
-- Availability Zone is missing the expected Firewall Manager managed
-- subnet.
--
-- 'networkFirewallPolicyModifiedViolation', 'resourceViolation_networkFirewallPolicyModifiedViolation' - Violation detail for an Network Firewall policy that indicates that a
-- firewall policy in an individual account has been modified in a way that
-- makes it noncompliant. For example, the individual account owner might
-- have deleted a rule group, changed the priority of a stateless rule
-- group, or changed a policy default action.
--
-- 'networkFirewallUnexpectedFirewallRoutesViolation', 'resourceViolation_networkFirewallUnexpectedFirewallRoutesViolation' - There\'s an unexpected firewall route.
--
-- 'networkFirewallUnexpectedGatewayRoutesViolation', 'resourceViolation_networkFirewallUnexpectedGatewayRoutesViolation' - There\'s an unexpected gateway route.
--
-- 'possibleRemediationActions', 'resourceViolation_possibleRemediationActions' - A list of possible remediation action lists. Each individual possible
-- remediation action is a list of individual remediation actions.
--
-- 'routeHasOutOfScopeEndpointViolation', 'resourceViolation_routeHasOutOfScopeEndpointViolation' - Contains details about the route endpoint that violates the policy
-- scope.
--
-- 'thirdPartyFirewallMissingExpectedRouteTableViolation', 'resourceViolation_thirdPartyFirewallMissingExpectedRouteTableViolation' - The violation details for a third-party firewall that has the Firewall
-- Manager managed route table that was associated with the third-party
-- firewall has been deleted.
--
-- 'thirdPartyFirewallMissingFirewallViolation', 'resourceViolation_thirdPartyFirewallMissingFirewallViolation' - The violation details for a third-party firewall that\'s been deleted.
--
-- 'thirdPartyFirewallMissingSubnetViolation', 'resourceViolation_thirdPartyFirewallMissingSubnetViolation' - The violation details for a third-party firewall\'s subnet that\'s been
-- deleted.
newResourceViolation ::
  ResourceViolation
newResourceViolation =
  ResourceViolation'
    { awsEc2InstanceViolation =
        Prelude.Nothing,
      awsEc2NetworkInterfaceViolation = Prelude.Nothing,
      awsVPCSecurityGroupViolation = Prelude.Nothing,
      dnsDuplicateRuleGroupViolation = Prelude.Nothing,
      dnsRuleGroupLimitExceededViolation = Prelude.Nothing,
      dnsRuleGroupPriorityConflictViolation =
        Prelude.Nothing,
      firewallSubnetIsOutOfScopeViolation =
        Prelude.Nothing,
      firewallSubnetMissingVPCEndpointViolation =
        Prelude.Nothing,
      networkFirewallBlackHoleRouteDetectedViolation =
        Prelude.Nothing,
      networkFirewallInternetTrafficNotInspectedViolation =
        Prelude.Nothing,
      networkFirewallInvalidRouteConfigurationViolation =
        Prelude.Nothing,
      networkFirewallMissingExpectedRTViolation =
        Prelude.Nothing,
      networkFirewallMissingExpectedRoutesViolation =
        Prelude.Nothing,
      networkFirewallMissingFirewallViolation =
        Prelude.Nothing,
      networkFirewallMissingSubnetViolation =
        Prelude.Nothing,
      networkFirewallPolicyModifiedViolation =
        Prelude.Nothing,
      networkFirewallUnexpectedFirewallRoutesViolation =
        Prelude.Nothing,
      networkFirewallUnexpectedGatewayRoutesViolation =
        Prelude.Nothing,
      possibleRemediationActions = Prelude.Nothing,
      routeHasOutOfScopeEndpointViolation =
        Prelude.Nothing,
      thirdPartyFirewallMissingExpectedRouteTableViolation =
        Prelude.Nothing,
      thirdPartyFirewallMissingFirewallViolation =
        Prelude.Nothing,
      thirdPartyFirewallMissingSubnetViolation =
        Prelude.Nothing
    }

-- | Violation detail for an EC2 instance.
resourceViolation_awsEc2InstanceViolation :: Lens.Lens' ResourceViolation (Prelude.Maybe AwsEc2InstanceViolation)
resourceViolation_awsEc2InstanceViolation = Lens.lens (\ResourceViolation' {awsEc2InstanceViolation} -> awsEc2InstanceViolation) (\s@ResourceViolation' {} a -> s {awsEc2InstanceViolation = a} :: ResourceViolation)

-- | Violation detail for a network interface.
resourceViolation_awsEc2NetworkInterfaceViolation :: Lens.Lens' ResourceViolation (Prelude.Maybe AwsEc2NetworkInterfaceViolation)
resourceViolation_awsEc2NetworkInterfaceViolation = Lens.lens (\ResourceViolation' {awsEc2NetworkInterfaceViolation} -> awsEc2NetworkInterfaceViolation) (\s@ResourceViolation' {} a -> s {awsEc2NetworkInterfaceViolation = a} :: ResourceViolation)

-- | Violation detail for security groups.
resourceViolation_awsVPCSecurityGroupViolation :: Lens.Lens' ResourceViolation (Prelude.Maybe AwsVPCSecurityGroupViolation)
resourceViolation_awsVPCSecurityGroupViolation = Lens.lens (\ResourceViolation' {awsVPCSecurityGroupViolation} -> awsVPCSecurityGroupViolation) (\s@ResourceViolation' {} a -> s {awsVPCSecurityGroupViolation = a} :: ResourceViolation)

-- | Violation detail for a DNS Firewall policy that indicates that a rule
-- group that Firewall Manager tried to associate with a VPC is already
-- associated with the VPC and can\'t be associated again.
resourceViolation_dnsDuplicateRuleGroupViolation :: Lens.Lens' ResourceViolation (Prelude.Maybe DnsDuplicateRuleGroupViolation)
resourceViolation_dnsDuplicateRuleGroupViolation = Lens.lens (\ResourceViolation' {dnsDuplicateRuleGroupViolation} -> dnsDuplicateRuleGroupViolation) (\s@ResourceViolation' {} a -> s {dnsDuplicateRuleGroupViolation = a} :: ResourceViolation)

-- | Violation detail for a DNS Firewall policy that indicates that the VPC
-- reached the limit for associated DNS Firewall rule groups. Firewall
-- Manager tried to associate another rule group with the VPC and failed.
resourceViolation_dnsRuleGroupLimitExceededViolation :: Lens.Lens' ResourceViolation (Prelude.Maybe DnsRuleGroupLimitExceededViolation)
resourceViolation_dnsRuleGroupLimitExceededViolation = Lens.lens (\ResourceViolation' {dnsRuleGroupLimitExceededViolation} -> dnsRuleGroupLimitExceededViolation) (\s@ResourceViolation' {} a -> s {dnsRuleGroupLimitExceededViolation = a} :: ResourceViolation)

-- | Violation detail for a DNS Firewall policy that indicates that a rule
-- group that Firewall Manager tried to associate with a VPC has the same
-- priority as a rule group that\'s already associated.
resourceViolation_dnsRuleGroupPriorityConflictViolation :: Lens.Lens' ResourceViolation (Prelude.Maybe DnsRuleGroupPriorityConflictViolation)
resourceViolation_dnsRuleGroupPriorityConflictViolation = Lens.lens (\ResourceViolation' {dnsRuleGroupPriorityConflictViolation} -> dnsRuleGroupPriorityConflictViolation) (\s@ResourceViolation' {} a -> s {dnsRuleGroupPriorityConflictViolation = a} :: ResourceViolation)

-- | Contains details about the firewall subnet that violates the policy
-- scope.
resourceViolation_firewallSubnetIsOutOfScopeViolation :: Lens.Lens' ResourceViolation (Prelude.Maybe FirewallSubnetIsOutOfScopeViolation)
resourceViolation_firewallSubnetIsOutOfScopeViolation = Lens.lens (\ResourceViolation' {firewallSubnetIsOutOfScopeViolation} -> firewallSubnetIsOutOfScopeViolation) (\s@ResourceViolation' {} a -> s {firewallSubnetIsOutOfScopeViolation = a} :: ResourceViolation)

-- | The violation details for a third-party firewall\'s VPC endpoint subnet
-- that was deleted.
resourceViolation_firewallSubnetMissingVPCEndpointViolation :: Lens.Lens' ResourceViolation (Prelude.Maybe FirewallSubnetMissingVPCEndpointViolation)
resourceViolation_firewallSubnetMissingVPCEndpointViolation = Lens.lens (\ResourceViolation' {firewallSubnetMissingVPCEndpointViolation} -> firewallSubnetMissingVPCEndpointViolation) (\s@ResourceViolation' {} a -> s {firewallSubnetMissingVPCEndpointViolation = a} :: ResourceViolation)

-- | Undocumented member.
resourceViolation_networkFirewallBlackHoleRouteDetectedViolation :: Lens.Lens' ResourceViolation (Prelude.Maybe NetworkFirewallBlackHoleRouteDetectedViolation)
resourceViolation_networkFirewallBlackHoleRouteDetectedViolation = Lens.lens (\ResourceViolation' {networkFirewallBlackHoleRouteDetectedViolation} -> networkFirewallBlackHoleRouteDetectedViolation) (\s@ResourceViolation' {} a -> s {networkFirewallBlackHoleRouteDetectedViolation = a} :: ResourceViolation)

-- | Violation detail for the subnet for which internet traffic hasn\'t been
-- inspected.
resourceViolation_networkFirewallInternetTrafficNotInspectedViolation :: Lens.Lens' ResourceViolation (Prelude.Maybe NetworkFirewallInternetTrafficNotInspectedViolation)
resourceViolation_networkFirewallInternetTrafficNotInspectedViolation = Lens.lens (\ResourceViolation' {networkFirewallInternetTrafficNotInspectedViolation} -> networkFirewallInternetTrafficNotInspectedViolation) (\s@ResourceViolation' {} a -> s {networkFirewallInternetTrafficNotInspectedViolation = a} :: ResourceViolation)

-- | The route configuration is invalid.
resourceViolation_networkFirewallInvalidRouteConfigurationViolation :: Lens.Lens' ResourceViolation (Prelude.Maybe NetworkFirewallInvalidRouteConfigurationViolation)
resourceViolation_networkFirewallInvalidRouteConfigurationViolation = Lens.lens (\ResourceViolation' {networkFirewallInvalidRouteConfigurationViolation} -> networkFirewallInvalidRouteConfigurationViolation) (\s@ResourceViolation' {} a -> s {networkFirewallInvalidRouteConfigurationViolation = a} :: ResourceViolation)

-- | Violation detail for an Network Firewall policy that indicates that a
-- subnet is not associated with the expected Firewall Manager managed
-- route table.
resourceViolation_networkFirewallMissingExpectedRTViolation :: Lens.Lens' ResourceViolation (Prelude.Maybe NetworkFirewallMissingExpectedRTViolation)
resourceViolation_networkFirewallMissingExpectedRTViolation = Lens.lens (\ResourceViolation' {networkFirewallMissingExpectedRTViolation} -> networkFirewallMissingExpectedRTViolation) (\s@ResourceViolation' {} a -> s {networkFirewallMissingExpectedRTViolation = a} :: ResourceViolation)

-- | Expected routes are missing from Network Firewall.
resourceViolation_networkFirewallMissingExpectedRoutesViolation :: Lens.Lens' ResourceViolation (Prelude.Maybe NetworkFirewallMissingExpectedRoutesViolation)
resourceViolation_networkFirewallMissingExpectedRoutesViolation = Lens.lens (\ResourceViolation' {networkFirewallMissingExpectedRoutesViolation} -> networkFirewallMissingExpectedRoutesViolation) (\s@ResourceViolation' {} a -> s {networkFirewallMissingExpectedRoutesViolation = a} :: ResourceViolation)

-- | Violation detail for an Network Firewall policy that indicates that a
-- subnet has no Firewall Manager managed firewall in its VPC.
resourceViolation_networkFirewallMissingFirewallViolation :: Lens.Lens' ResourceViolation (Prelude.Maybe NetworkFirewallMissingFirewallViolation)
resourceViolation_networkFirewallMissingFirewallViolation = Lens.lens (\ResourceViolation' {networkFirewallMissingFirewallViolation} -> networkFirewallMissingFirewallViolation) (\s@ResourceViolation' {} a -> s {networkFirewallMissingFirewallViolation = a} :: ResourceViolation)

-- | Violation detail for an Network Firewall policy that indicates that an
-- Availability Zone is missing the expected Firewall Manager managed
-- subnet.
resourceViolation_networkFirewallMissingSubnetViolation :: Lens.Lens' ResourceViolation (Prelude.Maybe NetworkFirewallMissingSubnetViolation)
resourceViolation_networkFirewallMissingSubnetViolation = Lens.lens (\ResourceViolation' {networkFirewallMissingSubnetViolation} -> networkFirewallMissingSubnetViolation) (\s@ResourceViolation' {} a -> s {networkFirewallMissingSubnetViolation = a} :: ResourceViolation)

-- | Violation detail for an Network Firewall policy that indicates that a
-- firewall policy in an individual account has been modified in a way that
-- makes it noncompliant. For example, the individual account owner might
-- have deleted a rule group, changed the priority of a stateless rule
-- group, or changed a policy default action.
resourceViolation_networkFirewallPolicyModifiedViolation :: Lens.Lens' ResourceViolation (Prelude.Maybe NetworkFirewallPolicyModifiedViolation)
resourceViolation_networkFirewallPolicyModifiedViolation = Lens.lens (\ResourceViolation' {networkFirewallPolicyModifiedViolation} -> networkFirewallPolicyModifiedViolation) (\s@ResourceViolation' {} a -> s {networkFirewallPolicyModifiedViolation = a} :: ResourceViolation)

-- | There\'s an unexpected firewall route.
resourceViolation_networkFirewallUnexpectedFirewallRoutesViolation :: Lens.Lens' ResourceViolation (Prelude.Maybe NetworkFirewallUnexpectedFirewallRoutesViolation)
resourceViolation_networkFirewallUnexpectedFirewallRoutesViolation = Lens.lens (\ResourceViolation' {networkFirewallUnexpectedFirewallRoutesViolation} -> networkFirewallUnexpectedFirewallRoutesViolation) (\s@ResourceViolation' {} a -> s {networkFirewallUnexpectedFirewallRoutesViolation = a} :: ResourceViolation)

-- | There\'s an unexpected gateway route.
resourceViolation_networkFirewallUnexpectedGatewayRoutesViolation :: Lens.Lens' ResourceViolation (Prelude.Maybe NetworkFirewallUnexpectedGatewayRoutesViolation)
resourceViolation_networkFirewallUnexpectedGatewayRoutesViolation = Lens.lens (\ResourceViolation' {networkFirewallUnexpectedGatewayRoutesViolation} -> networkFirewallUnexpectedGatewayRoutesViolation) (\s@ResourceViolation' {} a -> s {networkFirewallUnexpectedGatewayRoutesViolation = a} :: ResourceViolation)

-- | A list of possible remediation action lists. Each individual possible
-- remediation action is a list of individual remediation actions.
resourceViolation_possibleRemediationActions :: Lens.Lens' ResourceViolation (Prelude.Maybe PossibleRemediationActions)
resourceViolation_possibleRemediationActions = Lens.lens (\ResourceViolation' {possibleRemediationActions} -> possibleRemediationActions) (\s@ResourceViolation' {} a -> s {possibleRemediationActions = a} :: ResourceViolation)

-- | Contains details about the route endpoint that violates the policy
-- scope.
resourceViolation_routeHasOutOfScopeEndpointViolation :: Lens.Lens' ResourceViolation (Prelude.Maybe RouteHasOutOfScopeEndpointViolation)
resourceViolation_routeHasOutOfScopeEndpointViolation = Lens.lens (\ResourceViolation' {routeHasOutOfScopeEndpointViolation} -> routeHasOutOfScopeEndpointViolation) (\s@ResourceViolation' {} a -> s {routeHasOutOfScopeEndpointViolation = a} :: ResourceViolation)

-- | The violation details for a third-party firewall that has the Firewall
-- Manager managed route table that was associated with the third-party
-- firewall has been deleted.
resourceViolation_thirdPartyFirewallMissingExpectedRouteTableViolation :: Lens.Lens' ResourceViolation (Prelude.Maybe ThirdPartyFirewallMissingExpectedRouteTableViolation)
resourceViolation_thirdPartyFirewallMissingExpectedRouteTableViolation = Lens.lens (\ResourceViolation' {thirdPartyFirewallMissingExpectedRouteTableViolation} -> thirdPartyFirewallMissingExpectedRouteTableViolation) (\s@ResourceViolation' {} a -> s {thirdPartyFirewallMissingExpectedRouteTableViolation = a} :: ResourceViolation)

-- | The violation details for a third-party firewall that\'s been deleted.
resourceViolation_thirdPartyFirewallMissingFirewallViolation :: Lens.Lens' ResourceViolation (Prelude.Maybe ThirdPartyFirewallMissingFirewallViolation)
resourceViolation_thirdPartyFirewallMissingFirewallViolation = Lens.lens (\ResourceViolation' {thirdPartyFirewallMissingFirewallViolation} -> thirdPartyFirewallMissingFirewallViolation) (\s@ResourceViolation' {} a -> s {thirdPartyFirewallMissingFirewallViolation = a} :: ResourceViolation)

-- | The violation details for a third-party firewall\'s subnet that\'s been
-- deleted.
resourceViolation_thirdPartyFirewallMissingSubnetViolation :: Lens.Lens' ResourceViolation (Prelude.Maybe ThirdPartyFirewallMissingSubnetViolation)
resourceViolation_thirdPartyFirewallMissingSubnetViolation = Lens.lens (\ResourceViolation' {thirdPartyFirewallMissingSubnetViolation} -> thirdPartyFirewallMissingSubnetViolation) (\s@ResourceViolation' {} a -> s {thirdPartyFirewallMissingSubnetViolation = a} :: ResourceViolation)

instance Data.FromJSON ResourceViolation where
  parseJSON =
    Data.withObject
      "ResourceViolation"
      ( \x ->
          ResourceViolation'
            Prelude.<$> (x Data..:? "AwsEc2InstanceViolation")
            Prelude.<*> (x Data..:? "AwsEc2NetworkInterfaceViolation")
            Prelude.<*> (x Data..:? "AwsVPCSecurityGroupViolation")
            Prelude.<*> (x Data..:? "DnsDuplicateRuleGroupViolation")
            Prelude.<*> (x Data..:? "DnsRuleGroupLimitExceededViolation")
            Prelude.<*> (x Data..:? "DnsRuleGroupPriorityConflictViolation")
            Prelude.<*> (x Data..:? "FirewallSubnetIsOutOfScopeViolation")
            Prelude.<*> ( x
                            Data..:? "FirewallSubnetMissingVPCEndpointViolation"
                        )
            Prelude.<*> ( x
                            Data..:? "NetworkFirewallBlackHoleRouteDetectedViolation"
                        )
            Prelude.<*> ( x
                            Data..:? "NetworkFirewallInternetTrafficNotInspectedViolation"
                        )
            Prelude.<*> ( x
                            Data..:? "NetworkFirewallInvalidRouteConfigurationViolation"
                        )
            Prelude.<*> ( x
                            Data..:? "NetworkFirewallMissingExpectedRTViolation"
                        )
            Prelude.<*> ( x
                            Data..:? "NetworkFirewallMissingExpectedRoutesViolation"
                        )
            Prelude.<*> ( x
                            Data..:? "NetworkFirewallMissingFirewallViolation"
                        )
            Prelude.<*> (x Data..:? "NetworkFirewallMissingSubnetViolation")
            Prelude.<*> (x Data..:? "NetworkFirewallPolicyModifiedViolation")
            Prelude.<*> ( x
                            Data..:? "NetworkFirewallUnexpectedFirewallRoutesViolation"
                        )
            Prelude.<*> ( x
                            Data..:? "NetworkFirewallUnexpectedGatewayRoutesViolation"
                        )
            Prelude.<*> (x Data..:? "PossibleRemediationActions")
            Prelude.<*> (x Data..:? "RouteHasOutOfScopeEndpointViolation")
            Prelude.<*> ( x
                            Data..:? "ThirdPartyFirewallMissingExpectedRouteTableViolation"
                        )
            Prelude.<*> ( x
                            Data..:? "ThirdPartyFirewallMissingFirewallViolation"
                        )
            Prelude.<*> ( x
                            Data..:? "ThirdPartyFirewallMissingSubnetViolation"
                        )
      )

instance Prelude.Hashable ResourceViolation where
  hashWithSalt _salt ResourceViolation' {..} =
    _salt
      `Prelude.hashWithSalt` awsEc2InstanceViolation
      `Prelude.hashWithSalt` awsEc2NetworkInterfaceViolation
      `Prelude.hashWithSalt` awsVPCSecurityGroupViolation
      `Prelude.hashWithSalt` dnsDuplicateRuleGroupViolation
      `Prelude.hashWithSalt` dnsRuleGroupLimitExceededViolation
      `Prelude.hashWithSalt` dnsRuleGroupPriorityConflictViolation
      `Prelude.hashWithSalt` firewallSubnetIsOutOfScopeViolation
      `Prelude.hashWithSalt` firewallSubnetMissingVPCEndpointViolation
      `Prelude.hashWithSalt` networkFirewallBlackHoleRouteDetectedViolation
      `Prelude.hashWithSalt` networkFirewallInternetTrafficNotInspectedViolation
      `Prelude.hashWithSalt` networkFirewallInvalidRouteConfigurationViolation
      `Prelude.hashWithSalt` networkFirewallMissingExpectedRTViolation
      `Prelude.hashWithSalt` networkFirewallMissingExpectedRoutesViolation
      `Prelude.hashWithSalt` networkFirewallMissingFirewallViolation
      `Prelude.hashWithSalt` networkFirewallMissingSubnetViolation
      `Prelude.hashWithSalt` networkFirewallPolicyModifiedViolation
      `Prelude.hashWithSalt` networkFirewallUnexpectedFirewallRoutesViolation
      `Prelude.hashWithSalt` networkFirewallUnexpectedGatewayRoutesViolation
      `Prelude.hashWithSalt` possibleRemediationActions
      `Prelude.hashWithSalt` routeHasOutOfScopeEndpointViolation
      `Prelude.hashWithSalt` thirdPartyFirewallMissingExpectedRouteTableViolation
      `Prelude.hashWithSalt` thirdPartyFirewallMissingFirewallViolation
      `Prelude.hashWithSalt` thirdPartyFirewallMissingSubnetViolation

instance Prelude.NFData ResourceViolation where
  rnf ResourceViolation' {..} =
    Prelude.rnf awsEc2InstanceViolation
      `Prelude.seq` Prelude.rnf awsEc2NetworkInterfaceViolation
      `Prelude.seq` Prelude.rnf awsVPCSecurityGroupViolation
      `Prelude.seq` Prelude.rnf dnsDuplicateRuleGroupViolation
      `Prelude.seq` Prelude.rnf dnsRuleGroupLimitExceededViolation
      `Prelude.seq` Prelude.rnf dnsRuleGroupPriorityConflictViolation
      `Prelude.seq` Prelude.rnf firewallSubnetIsOutOfScopeViolation
      `Prelude.seq` Prelude.rnf firewallSubnetMissingVPCEndpointViolation
      `Prelude.seq` Prelude.rnf
        networkFirewallBlackHoleRouteDetectedViolation
      `Prelude.seq` Prelude.rnf
        networkFirewallInternetTrafficNotInspectedViolation
      `Prelude.seq` Prelude.rnf
        networkFirewallInvalidRouteConfigurationViolation
      `Prelude.seq` Prelude.rnf
        networkFirewallMissingExpectedRTViolation
      `Prelude.seq` Prelude.rnf
        networkFirewallMissingExpectedRoutesViolation
      `Prelude.seq` Prelude.rnf
        networkFirewallMissingFirewallViolation
      `Prelude.seq` Prelude.rnf
        networkFirewallMissingSubnetViolation
      `Prelude.seq` Prelude.rnf
        networkFirewallPolicyModifiedViolation
      `Prelude.seq` Prelude.rnf
        networkFirewallUnexpectedFirewallRoutesViolation
      `Prelude.seq` Prelude.rnf
        networkFirewallUnexpectedGatewayRoutesViolation
      `Prelude.seq` Prelude.rnf
        possibleRemediationActions
      `Prelude.seq` Prelude.rnf
        routeHasOutOfScopeEndpointViolation
      `Prelude.seq` Prelude.rnf
        thirdPartyFirewallMissingExpectedRouteTableViolation
      `Prelude.seq` Prelude.rnf
        thirdPartyFirewallMissingFirewallViolation
      `Prelude.seq` Prelude.rnf
        thirdPartyFirewallMissingSubnetViolation
