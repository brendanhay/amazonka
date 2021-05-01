{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.FMS.Types.AwsEc2InstanceViolation
import Network.AWS.FMS.Types.AwsEc2NetworkInterfaceViolation
import Network.AWS.FMS.Types.AwsVPCSecurityGroupViolation
import Network.AWS.FMS.Types.NetworkFirewallMissingExpectedRTViolation
import Network.AWS.FMS.Types.NetworkFirewallMissingFirewallViolation
import Network.AWS.FMS.Types.NetworkFirewallMissingSubnetViolation
import Network.AWS.FMS.Types.NetworkFirewallPolicyModifiedViolation
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Violation detail based on resource type.
--
-- /See:/ 'newResourceViolation' smart constructor.
data ResourceViolation = ResourceViolation'
  { -- | Violation detail for an Network Firewall policy that indicates that an
    -- Availability Zone is missing the expected Firewall Manager managed
    -- subnet.
    networkFirewallMissingSubnetViolation :: Prelude.Maybe NetworkFirewallMissingSubnetViolation,
    -- | Violation details for network interface.
    awsEc2NetworkInterfaceViolation :: Prelude.Maybe AwsEc2NetworkInterfaceViolation,
    -- | Violation detail for an Network Firewall policy that indicates that a
    -- subnet is not associated with the expected Firewall Manager managed
    -- route table.
    networkFirewallMissingExpectedRTViolation :: Prelude.Maybe NetworkFirewallMissingExpectedRTViolation,
    -- | Violation details for an EC2 instance.
    awsEc2InstanceViolation :: Prelude.Maybe AwsEc2InstanceViolation,
    -- | Violation detail for an Network Firewall policy that indicates that a
    -- subnet has no Firewall Manager managed firewall in its VPC.
    networkFirewallMissingFirewallViolation :: Prelude.Maybe NetworkFirewallMissingFirewallViolation,
    -- | Violation detail for an Network Firewall policy that indicates that a
    -- firewall policy in an individual account has been modified in a way that
    -- makes it noncompliant. For example, the individual account owner might
    -- have deleted a rule group, changed the priority of a stateless rule
    -- group, or changed a policy default action.
    networkFirewallPolicyModifiedViolation :: Prelude.Maybe NetworkFirewallPolicyModifiedViolation,
    -- | Violation details for security groups.
    awsVPCSecurityGroupViolation :: Prelude.Maybe AwsVPCSecurityGroupViolation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResourceViolation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkFirewallMissingSubnetViolation', 'resourceViolation_networkFirewallMissingSubnetViolation' - Violation detail for an Network Firewall policy that indicates that an
-- Availability Zone is missing the expected Firewall Manager managed
-- subnet.
--
-- 'awsEc2NetworkInterfaceViolation', 'resourceViolation_awsEc2NetworkInterfaceViolation' - Violation details for network interface.
--
-- 'networkFirewallMissingExpectedRTViolation', 'resourceViolation_networkFirewallMissingExpectedRTViolation' - Violation detail for an Network Firewall policy that indicates that a
-- subnet is not associated with the expected Firewall Manager managed
-- route table.
--
-- 'awsEc2InstanceViolation', 'resourceViolation_awsEc2InstanceViolation' - Violation details for an EC2 instance.
--
-- 'networkFirewallMissingFirewallViolation', 'resourceViolation_networkFirewallMissingFirewallViolation' - Violation detail for an Network Firewall policy that indicates that a
-- subnet has no Firewall Manager managed firewall in its VPC.
--
-- 'networkFirewallPolicyModifiedViolation', 'resourceViolation_networkFirewallPolicyModifiedViolation' - Violation detail for an Network Firewall policy that indicates that a
-- firewall policy in an individual account has been modified in a way that
-- makes it noncompliant. For example, the individual account owner might
-- have deleted a rule group, changed the priority of a stateless rule
-- group, or changed a policy default action.
--
-- 'awsVPCSecurityGroupViolation', 'resourceViolation_awsVPCSecurityGroupViolation' - Violation details for security groups.
newResourceViolation ::
  ResourceViolation
newResourceViolation =
  ResourceViolation'
    { networkFirewallMissingSubnetViolation =
        Prelude.Nothing,
      awsEc2NetworkInterfaceViolation = Prelude.Nothing,
      networkFirewallMissingExpectedRTViolation =
        Prelude.Nothing,
      awsEc2InstanceViolation = Prelude.Nothing,
      networkFirewallMissingFirewallViolation =
        Prelude.Nothing,
      networkFirewallPolicyModifiedViolation =
        Prelude.Nothing,
      awsVPCSecurityGroupViolation = Prelude.Nothing
    }

-- | Violation detail for an Network Firewall policy that indicates that an
-- Availability Zone is missing the expected Firewall Manager managed
-- subnet.
resourceViolation_networkFirewallMissingSubnetViolation :: Lens.Lens' ResourceViolation (Prelude.Maybe NetworkFirewallMissingSubnetViolation)
resourceViolation_networkFirewallMissingSubnetViolation = Lens.lens (\ResourceViolation' {networkFirewallMissingSubnetViolation} -> networkFirewallMissingSubnetViolation) (\s@ResourceViolation' {} a -> s {networkFirewallMissingSubnetViolation = a} :: ResourceViolation)

-- | Violation details for network interface.
resourceViolation_awsEc2NetworkInterfaceViolation :: Lens.Lens' ResourceViolation (Prelude.Maybe AwsEc2NetworkInterfaceViolation)
resourceViolation_awsEc2NetworkInterfaceViolation = Lens.lens (\ResourceViolation' {awsEc2NetworkInterfaceViolation} -> awsEc2NetworkInterfaceViolation) (\s@ResourceViolation' {} a -> s {awsEc2NetworkInterfaceViolation = a} :: ResourceViolation)

-- | Violation detail for an Network Firewall policy that indicates that a
-- subnet is not associated with the expected Firewall Manager managed
-- route table.
resourceViolation_networkFirewallMissingExpectedRTViolation :: Lens.Lens' ResourceViolation (Prelude.Maybe NetworkFirewallMissingExpectedRTViolation)
resourceViolation_networkFirewallMissingExpectedRTViolation = Lens.lens (\ResourceViolation' {networkFirewallMissingExpectedRTViolation} -> networkFirewallMissingExpectedRTViolation) (\s@ResourceViolation' {} a -> s {networkFirewallMissingExpectedRTViolation = a} :: ResourceViolation)

-- | Violation details for an EC2 instance.
resourceViolation_awsEc2InstanceViolation :: Lens.Lens' ResourceViolation (Prelude.Maybe AwsEc2InstanceViolation)
resourceViolation_awsEc2InstanceViolation = Lens.lens (\ResourceViolation' {awsEc2InstanceViolation} -> awsEc2InstanceViolation) (\s@ResourceViolation' {} a -> s {awsEc2InstanceViolation = a} :: ResourceViolation)

-- | Violation detail for an Network Firewall policy that indicates that a
-- subnet has no Firewall Manager managed firewall in its VPC.
resourceViolation_networkFirewallMissingFirewallViolation :: Lens.Lens' ResourceViolation (Prelude.Maybe NetworkFirewallMissingFirewallViolation)
resourceViolation_networkFirewallMissingFirewallViolation = Lens.lens (\ResourceViolation' {networkFirewallMissingFirewallViolation} -> networkFirewallMissingFirewallViolation) (\s@ResourceViolation' {} a -> s {networkFirewallMissingFirewallViolation = a} :: ResourceViolation)

-- | Violation detail for an Network Firewall policy that indicates that a
-- firewall policy in an individual account has been modified in a way that
-- makes it noncompliant. For example, the individual account owner might
-- have deleted a rule group, changed the priority of a stateless rule
-- group, or changed a policy default action.
resourceViolation_networkFirewallPolicyModifiedViolation :: Lens.Lens' ResourceViolation (Prelude.Maybe NetworkFirewallPolicyModifiedViolation)
resourceViolation_networkFirewallPolicyModifiedViolation = Lens.lens (\ResourceViolation' {networkFirewallPolicyModifiedViolation} -> networkFirewallPolicyModifiedViolation) (\s@ResourceViolation' {} a -> s {networkFirewallPolicyModifiedViolation = a} :: ResourceViolation)

-- | Violation details for security groups.
resourceViolation_awsVPCSecurityGroupViolation :: Lens.Lens' ResourceViolation (Prelude.Maybe AwsVPCSecurityGroupViolation)
resourceViolation_awsVPCSecurityGroupViolation = Lens.lens (\ResourceViolation' {awsVPCSecurityGroupViolation} -> awsVPCSecurityGroupViolation) (\s@ResourceViolation' {} a -> s {awsVPCSecurityGroupViolation = a} :: ResourceViolation)

instance Prelude.FromJSON ResourceViolation where
  parseJSON =
    Prelude.withObject
      "ResourceViolation"
      ( \x ->
          ResourceViolation'
            Prelude.<$> ( x
                            Prelude..:? "NetworkFirewallMissingSubnetViolation"
                        )
            Prelude.<*> (x Prelude..:? "AwsEc2NetworkInterfaceViolation")
            Prelude.<*> ( x
                            Prelude..:? "NetworkFirewallMissingExpectedRTViolation"
                        )
            Prelude.<*> (x Prelude..:? "AwsEc2InstanceViolation")
            Prelude.<*> ( x
                            Prelude..:? "NetworkFirewallMissingFirewallViolation"
                        )
            Prelude.<*> ( x
                            Prelude..:? "NetworkFirewallPolicyModifiedViolation"
                        )
            Prelude.<*> (x Prelude..:? "AwsVPCSecurityGroupViolation")
      )

instance Prelude.Hashable ResourceViolation

instance Prelude.NFData ResourceViolation
