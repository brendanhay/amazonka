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
-- Module      : Network.AWS.FMS.Types.ResourceViolation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.ResourceViolation where

import qualified Network.AWS.Core as Core
import Network.AWS.FMS.Types.AwsEc2InstanceViolation
import Network.AWS.FMS.Types.AwsEc2NetworkInterfaceViolation
import Network.AWS.FMS.Types.AwsVPCSecurityGroupViolation
import Network.AWS.FMS.Types.DnsDuplicateRuleGroupViolation
import Network.AWS.FMS.Types.DnsRuleGroupLimitExceededViolation
import Network.AWS.FMS.Types.DnsRuleGroupPriorityConflictViolation
import Network.AWS.FMS.Types.NetworkFirewallBlackHoleRouteDetectedViolation
import Network.AWS.FMS.Types.NetworkFirewallInternetTrafficNotInspectedViolation
import Network.AWS.FMS.Types.NetworkFirewallInvalidRouteConfigurationViolation
import Network.AWS.FMS.Types.NetworkFirewallMissingExpectedRTViolation
import Network.AWS.FMS.Types.NetworkFirewallMissingExpectedRoutesViolation
import Network.AWS.FMS.Types.NetworkFirewallMissingFirewallViolation
import Network.AWS.FMS.Types.NetworkFirewallMissingSubnetViolation
import Network.AWS.FMS.Types.NetworkFirewallPolicyModifiedViolation
import Network.AWS.FMS.Types.NetworkFirewallUnexpectedFirewallRoutesViolation
import Network.AWS.FMS.Types.NetworkFirewallUnexpectedGatewayRoutesViolation
import Network.AWS.FMS.Types.PossibleRemediationActions
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Violation detail based on resource type.
--
-- /See:/ 'newResourceViolation' smart constructor.
data ResourceViolation = ResourceViolation'
  { -- | A list of possible remediation action lists. Each individual possible
    -- remediation action is a list of individual remediation actions.
    possibleRemediationActions :: Prelude.Maybe PossibleRemediationActions,
    networkFirewallBlackHoleRouteDetectedViolation :: Prelude.Maybe NetworkFirewallBlackHoleRouteDetectedViolation,
    -- | Violation detail for a DNS Firewall policy that indicates that the VPC
    -- reached the limit for associated DNS Firewall rule groups. Firewall
    -- Manager tried to associate another rule group with the VPC and failed.
    dnsRuleGroupLimitExceededViolation :: Prelude.Maybe DnsRuleGroupLimitExceededViolation,
    -- | Violation detail for an Network Firewall policy that indicates that a
    -- subnet is not associated with the expected Firewall Manager managed
    -- route table.
    networkFirewallMissingExpectedRTViolation :: Prelude.Maybe NetworkFirewallMissingExpectedRTViolation,
    -- | Violation detail for the subnet for which internet traffic hasn\'t been
    -- inspected.
    networkFirewallInternetTrafficNotInspectedViolation :: Prelude.Maybe NetworkFirewallInternetTrafficNotInspectedViolation,
    -- | Violation detail for an Network Firewall policy that indicates that a
    -- subnet has no Firewall Manager managed firewall in its VPC.
    networkFirewallMissingFirewallViolation :: Prelude.Maybe NetworkFirewallMissingFirewallViolation,
    -- | Violation detail for an Network Firewall policy that indicates that an
    -- Availability Zone is missing the expected Firewall Manager managed
    -- subnet.
    networkFirewallMissingSubnetViolation :: Prelude.Maybe NetworkFirewallMissingSubnetViolation,
    -- | Violation detail for an EC2 instance.
    awsEc2InstanceViolation :: Prelude.Maybe AwsEc2InstanceViolation,
    -- | Expected routes are missing from Network Firewall.
    networkFirewallMissingExpectedRoutesViolation :: Prelude.Maybe NetworkFirewallMissingExpectedRoutesViolation,
    -- | Violation detail for a DNS Firewall policy that indicates that a rule
    -- group that Firewall Manager tried to associate with a VPC has the same
    -- priority as a rule group that\'s already associated.
    dnsRuleGroupPriorityConflictViolation :: Prelude.Maybe DnsRuleGroupPriorityConflictViolation,
    -- | Violation detail for security groups.
    awsVPCSecurityGroupViolation :: Prelude.Maybe AwsVPCSecurityGroupViolation,
    -- | Violation detail for an Network Firewall policy that indicates that a
    -- firewall policy in an individual account has been modified in a way that
    -- makes it noncompliant. For example, the individual account owner might
    -- have deleted a rule group, changed the priority of a stateless rule
    -- group, or changed a policy default action.
    networkFirewallPolicyModifiedViolation :: Prelude.Maybe NetworkFirewallPolicyModifiedViolation,
    -- | There\'s an unexpected firewall route.
    networkFirewallUnexpectedFirewallRoutesViolation :: Prelude.Maybe NetworkFirewallUnexpectedFirewallRoutesViolation,
    -- | Violation detail for a network interface.
    awsEc2NetworkInterfaceViolation :: Prelude.Maybe AwsEc2NetworkInterfaceViolation,
    -- | There\'s an unexpected gateway route.
    networkFirewallUnexpectedGatewayRoutesViolation :: Prelude.Maybe NetworkFirewallUnexpectedGatewayRoutesViolation,
    -- | Violation detail for a DNS Firewall policy that indicates that a rule
    -- group that Firewall Manager tried to associate with a VPC is already
    -- associated with the VPC and can\'t be associated again.
    dnsDuplicateRuleGroupViolation :: Prelude.Maybe DnsDuplicateRuleGroupViolation,
    -- | The route configuration is invalid.
    networkFirewallInvalidRouteConfigurationViolation :: Prelude.Maybe NetworkFirewallInvalidRouteConfigurationViolation
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
-- 'possibleRemediationActions', 'resourceViolation_possibleRemediationActions' - A list of possible remediation action lists. Each individual possible
-- remediation action is a list of individual remediation actions.
--
-- 'networkFirewallBlackHoleRouteDetectedViolation', 'resourceViolation_networkFirewallBlackHoleRouteDetectedViolation' - Undocumented member.
--
-- 'dnsRuleGroupLimitExceededViolation', 'resourceViolation_dnsRuleGroupLimitExceededViolation' - Violation detail for a DNS Firewall policy that indicates that the VPC
-- reached the limit for associated DNS Firewall rule groups. Firewall
-- Manager tried to associate another rule group with the VPC and failed.
--
-- 'networkFirewallMissingExpectedRTViolation', 'resourceViolation_networkFirewallMissingExpectedRTViolation' - Violation detail for an Network Firewall policy that indicates that a
-- subnet is not associated with the expected Firewall Manager managed
-- route table.
--
-- 'networkFirewallInternetTrafficNotInspectedViolation', 'resourceViolation_networkFirewallInternetTrafficNotInspectedViolation' - Violation detail for the subnet for which internet traffic hasn\'t been
-- inspected.
--
-- 'networkFirewallMissingFirewallViolation', 'resourceViolation_networkFirewallMissingFirewallViolation' - Violation detail for an Network Firewall policy that indicates that a
-- subnet has no Firewall Manager managed firewall in its VPC.
--
-- 'networkFirewallMissingSubnetViolation', 'resourceViolation_networkFirewallMissingSubnetViolation' - Violation detail for an Network Firewall policy that indicates that an
-- Availability Zone is missing the expected Firewall Manager managed
-- subnet.
--
-- 'awsEc2InstanceViolation', 'resourceViolation_awsEc2InstanceViolation' - Violation detail for an EC2 instance.
--
-- 'networkFirewallMissingExpectedRoutesViolation', 'resourceViolation_networkFirewallMissingExpectedRoutesViolation' - Expected routes are missing from Network Firewall.
--
-- 'dnsRuleGroupPriorityConflictViolation', 'resourceViolation_dnsRuleGroupPriorityConflictViolation' - Violation detail for a DNS Firewall policy that indicates that a rule
-- group that Firewall Manager tried to associate with a VPC has the same
-- priority as a rule group that\'s already associated.
--
-- 'awsVPCSecurityGroupViolation', 'resourceViolation_awsVPCSecurityGroupViolation' - Violation detail for security groups.
--
-- 'networkFirewallPolicyModifiedViolation', 'resourceViolation_networkFirewallPolicyModifiedViolation' - Violation detail for an Network Firewall policy that indicates that a
-- firewall policy in an individual account has been modified in a way that
-- makes it noncompliant. For example, the individual account owner might
-- have deleted a rule group, changed the priority of a stateless rule
-- group, or changed a policy default action.
--
-- 'networkFirewallUnexpectedFirewallRoutesViolation', 'resourceViolation_networkFirewallUnexpectedFirewallRoutesViolation' - There\'s an unexpected firewall route.
--
-- 'awsEc2NetworkInterfaceViolation', 'resourceViolation_awsEc2NetworkInterfaceViolation' - Violation detail for a network interface.
--
-- 'networkFirewallUnexpectedGatewayRoutesViolation', 'resourceViolation_networkFirewallUnexpectedGatewayRoutesViolation' - There\'s an unexpected gateway route.
--
-- 'dnsDuplicateRuleGroupViolation', 'resourceViolation_dnsDuplicateRuleGroupViolation' - Violation detail for a DNS Firewall policy that indicates that a rule
-- group that Firewall Manager tried to associate with a VPC is already
-- associated with the VPC and can\'t be associated again.
--
-- 'networkFirewallInvalidRouteConfigurationViolation', 'resourceViolation_networkFirewallInvalidRouteConfigurationViolation' - The route configuration is invalid.
newResourceViolation ::
  ResourceViolation
newResourceViolation =
  ResourceViolation'
    { possibleRemediationActions =
        Prelude.Nothing,
      networkFirewallBlackHoleRouteDetectedViolation =
        Prelude.Nothing,
      dnsRuleGroupLimitExceededViolation = Prelude.Nothing,
      networkFirewallMissingExpectedRTViolation =
        Prelude.Nothing,
      networkFirewallInternetTrafficNotInspectedViolation =
        Prelude.Nothing,
      networkFirewallMissingFirewallViolation =
        Prelude.Nothing,
      networkFirewallMissingSubnetViolation =
        Prelude.Nothing,
      awsEc2InstanceViolation = Prelude.Nothing,
      networkFirewallMissingExpectedRoutesViolation =
        Prelude.Nothing,
      dnsRuleGroupPriorityConflictViolation =
        Prelude.Nothing,
      awsVPCSecurityGroupViolation = Prelude.Nothing,
      networkFirewallPolicyModifiedViolation =
        Prelude.Nothing,
      networkFirewallUnexpectedFirewallRoutesViolation =
        Prelude.Nothing,
      awsEc2NetworkInterfaceViolation = Prelude.Nothing,
      networkFirewallUnexpectedGatewayRoutesViolation =
        Prelude.Nothing,
      dnsDuplicateRuleGroupViolation = Prelude.Nothing,
      networkFirewallInvalidRouteConfigurationViolation =
        Prelude.Nothing
    }

-- | A list of possible remediation action lists. Each individual possible
-- remediation action is a list of individual remediation actions.
resourceViolation_possibleRemediationActions :: Lens.Lens' ResourceViolation (Prelude.Maybe PossibleRemediationActions)
resourceViolation_possibleRemediationActions = Lens.lens (\ResourceViolation' {possibleRemediationActions} -> possibleRemediationActions) (\s@ResourceViolation' {} a -> s {possibleRemediationActions = a} :: ResourceViolation)

-- | Undocumented member.
resourceViolation_networkFirewallBlackHoleRouteDetectedViolation :: Lens.Lens' ResourceViolation (Prelude.Maybe NetworkFirewallBlackHoleRouteDetectedViolation)
resourceViolation_networkFirewallBlackHoleRouteDetectedViolation = Lens.lens (\ResourceViolation' {networkFirewallBlackHoleRouteDetectedViolation} -> networkFirewallBlackHoleRouteDetectedViolation) (\s@ResourceViolation' {} a -> s {networkFirewallBlackHoleRouteDetectedViolation = a} :: ResourceViolation)

-- | Violation detail for a DNS Firewall policy that indicates that the VPC
-- reached the limit for associated DNS Firewall rule groups. Firewall
-- Manager tried to associate another rule group with the VPC and failed.
resourceViolation_dnsRuleGroupLimitExceededViolation :: Lens.Lens' ResourceViolation (Prelude.Maybe DnsRuleGroupLimitExceededViolation)
resourceViolation_dnsRuleGroupLimitExceededViolation = Lens.lens (\ResourceViolation' {dnsRuleGroupLimitExceededViolation} -> dnsRuleGroupLimitExceededViolation) (\s@ResourceViolation' {} a -> s {dnsRuleGroupLimitExceededViolation = a} :: ResourceViolation)

-- | Violation detail for an Network Firewall policy that indicates that a
-- subnet is not associated with the expected Firewall Manager managed
-- route table.
resourceViolation_networkFirewallMissingExpectedRTViolation :: Lens.Lens' ResourceViolation (Prelude.Maybe NetworkFirewallMissingExpectedRTViolation)
resourceViolation_networkFirewallMissingExpectedRTViolation = Lens.lens (\ResourceViolation' {networkFirewallMissingExpectedRTViolation} -> networkFirewallMissingExpectedRTViolation) (\s@ResourceViolation' {} a -> s {networkFirewallMissingExpectedRTViolation = a} :: ResourceViolation)

-- | Violation detail for the subnet for which internet traffic hasn\'t been
-- inspected.
resourceViolation_networkFirewallInternetTrafficNotInspectedViolation :: Lens.Lens' ResourceViolation (Prelude.Maybe NetworkFirewallInternetTrafficNotInspectedViolation)
resourceViolation_networkFirewallInternetTrafficNotInspectedViolation = Lens.lens (\ResourceViolation' {networkFirewallInternetTrafficNotInspectedViolation} -> networkFirewallInternetTrafficNotInspectedViolation) (\s@ResourceViolation' {} a -> s {networkFirewallInternetTrafficNotInspectedViolation = a} :: ResourceViolation)

-- | Violation detail for an Network Firewall policy that indicates that a
-- subnet has no Firewall Manager managed firewall in its VPC.
resourceViolation_networkFirewallMissingFirewallViolation :: Lens.Lens' ResourceViolation (Prelude.Maybe NetworkFirewallMissingFirewallViolation)
resourceViolation_networkFirewallMissingFirewallViolation = Lens.lens (\ResourceViolation' {networkFirewallMissingFirewallViolation} -> networkFirewallMissingFirewallViolation) (\s@ResourceViolation' {} a -> s {networkFirewallMissingFirewallViolation = a} :: ResourceViolation)

-- | Violation detail for an Network Firewall policy that indicates that an
-- Availability Zone is missing the expected Firewall Manager managed
-- subnet.
resourceViolation_networkFirewallMissingSubnetViolation :: Lens.Lens' ResourceViolation (Prelude.Maybe NetworkFirewallMissingSubnetViolation)
resourceViolation_networkFirewallMissingSubnetViolation = Lens.lens (\ResourceViolation' {networkFirewallMissingSubnetViolation} -> networkFirewallMissingSubnetViolation) (\s@ResourceViolation' {} a -> s {networkFirewallMissingSubnetViolation = a} :: ResourceViolation)

-- | Violation detail for an EC2 instance.
resourceViolation_awsEc2InstanceViolation :: Lens.Lens' ResourceViolation (Prelude.Maybe AwsEc2InstanceViolation)
resourceViolation_awsEc2InstanceViolation = Lens.lens (\ResourceViolation' {awsEc2InstanceViolation} -> awsEc2InstanceViolation) (\s@ResourceViolation' {} a -> s {awsEc2InstanceViolation = a} :: ResourceViolation)

-- | Expected routes are missing from Network Firewall.
resourceViolation_networkFirewallMissingExpectedRoutesViolation :: Lens.Lens' ResourceViolation (Prelude.Maybe NetworkFirewallMissingExpectedRoutesViolation)
resourceViolation_networkFirewallMissingExpectedRoutesViolation = Lens.lens (\ResourceViolation' {networkFirewallMissingExpectedRoutesViolation} -> networkFirewallMissingExpectedRoutesViolation) (\s@ResourceViolation' {} a -> s {networkFirewallMissingExpectedRoutesViolation = a} :: ResourceViolation)

-- | Violation detail for a DNS Firewall policy that indicates that a rule
-- group that Firewall Manager tried to associate with a VPC has the same
-- priority as a rule group that\'s already associated.
resourceViolation_dnsRuleGroupPriorityConflictViolation :: Lens.Lens' ResourceViolation (Prelude.Maybe DnsRuleGroupPriorityConflictViolation)
resourceViolation_dnsRuleGroupPriorityConflictViolation = Lens.lens (\ResourceViolation' {dnsRuleGroupPriorityConflictViolation} -> dnsRuleGroupPriorityConflictViolation) (\s@ResourceViolation' {} a -> s {dnsRuleGroupPriorityConflictViolation = a} :: ResourceViolation)

-- | Violation detail for security groups.
resourceViolation_awsVPCSecurityGroupViolation :: Lens.Lens' ResourceViolation (Prelude.Maybe AwsVPCSecurityGroupViolation)
resourceViolation_awsVPCSecurityGroupViolation = Lens.lens (\ResourceViolation' {awsVPCSecurityGroupViolation} -> awsVPCSecurityGroupViolation) (\s@ResourceViolation' {} a -> s {awsVPCSecurityGroupViolation = a} :: ResourceViolation)

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

-- | Violation detail for a network interface.
resourceViolation_awsEc2NetworkInterfaceViolation :: Lens.Lens' ResourceViolation (Prelude.Maybe AwsEc2NetworkInterfaceViolation)
resourceViolation_awsEc2NetworkInterfaceViolation = Lens.lens (\ResourceViolation' {awsEc2NetworkInterfaceViolation} -> awsEc2NetworkInterfaceViolation) (\s@ResourceViolation' {} a -> s {awsEc2NetworkInterfaceViolation = a} :: ResourceViolation)

-- | There\'s an unexpected gateway route.
resourceViolation_networkFirewallUnexpectedGatewayRoutesViolation :: Lens.Lens' ResourceViolation (Prelude.Maybe NetworkFirewallUnexpectedGatewayRoutesViolation)
resourceViolation_networkFirewallUnexpectedGatewayRoutesViolation = Lens.lens (\ResourceViolation' {networkFirewallUnexpectedGatewayRoutesViolation} -> networkFirewallUnexpectedGatewayRoutesViolation) (\s@ResourceViolation' {} a -> s {networkFirewallUnexpectedGatewayRoutesViolation = a} :: ResourceViolation)

-- | Violation detail for a DNS Firewall policy that indicates that a rule
-- group that Firewall Manager tried to associate with a VPC is already
-- associated with the VPC and can\'t be associated again.
resourceViolation_dnsDuplicateRuleGroupViolation :: Lens.Lens' ResourceViolation (Prelude.Maybe DnsDuplicateRuleGroupViolation)
resourceViolation_dnsDuplicateRuleGroupViolation = Lens.lens (\ResourceViolation' {dnsDuplicateRuleGroupViolation} -> dnsDuplicateRuleGroupViolation) (\s@ResourceViolation' {} a -> s {dnsDuplicateRuleGroupViolation = a} :: ResourceViolation)

-- | The route configuration is invalid.
resourceViolation_networkFirewallInvalidRouteConfigurationViolation :: Lens.Lens' ResourceViolation (Prelude.Maybe NetworkFirewallInvalidRouteConfigurationViolation)
resourceViolation_networkFirewallInvalidRouteConfigurationViolation = Lens.lens (\ResourceViolation' {networkFirewallInvalidRouteConfigurationViolation} -> networkFirewallInvalidRouteConfigurationViolation) (\s@ResourceViolation' {} a -> s {networkFirewallInvalidRouteConfigurationViolation = a} :: ResourceViolation)

instance Core.FromJSON ResourceViolation where
  parseJSON =
    Core.withObject
      "ResourceViolation"
      ( \x ->
          ResourceViolation'
            Prelude.<$> (x Core..:? "PossibleRemediationActions")
            Prelude.<*> ( x
                            Core..:? "NetworkFirewallBlackHoleRouteDetectedViolation"
                        )
            Prelude.<*> (x Core..:? "DnsRuleGroupLimitExceededViolation")
            Prelude.<*> ( x
                            Core..:? "NetworkFirewallMissingExpectedRTViolation"
                        )
            Prelude.<*> ( x
                            Core..:? "NetworkFirewallInternetTrafficNotInspectedViolation"
                        )
            Prelude.<*> ( x
                            Core..:? "NetworkFirewallMissingFirewallViolation"
                        )
            Prelude.<*> (x Core..:? "NetworkFirewallMissingSubnetViolation")
            Prelude.<*> (x Core..:? "AwsEc2InstanceViolation")
            Prelude.<*> ( x
                            Core..:? "NetworkFirewallMissingExpectedRoutesViolation"
                        )
            Prelude.<*> (x Core..:? "DnsRuleGroupPriorityConflictViolation")
            Prelude.<*> (x Core..:? "AwsVPCSecurityGroupViolation")
            Prelude.<*> (x Core..:? "NetworkFirewallPolicyModifiedViolation")
            Prelude.<*> ( x
                            Core..:? "NetworkFirewallUnexpectedFirewallRoutesViolation"
                        )
            Prelude.<*> (x Core..:? "AwsEc2NetworkInterfaceViolation")
            Prelude.<*> ( x
                            Core..:? "NetworkFirewallUnexpectedGatewayRoutesViolation"
                        )
            Prelude.<*> (x Core..:? "DnsDuplicateRuleGroupViolation")
            Prelude.<*> ( x
                            Core..:? "NetworkFirewallInvalidRouteConfigurationViolation"
                        )
      )

instance Prelude.Hashable ResourceViolation

instance Prelude.NFData ResourceViolation
