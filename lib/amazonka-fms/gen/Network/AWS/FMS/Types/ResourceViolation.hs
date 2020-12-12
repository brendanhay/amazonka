{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.ResourceViolation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.ResourceViolation
  ( ResourceViolation (..),

    -- * Smart constructor
    mkResourceViolation,

    -- * Lenses
    rvNetworkFirewallMissingExpectedRTViolation,
    rvNetworkFirewallMissingFirewallViolation,
    rvNetworkFirewallMissingSubnetViolation,
    rvAWSEC2InstanceViolation,
    rvAWSVPCSecurityGroupViolation,
    rvNetworkFirewallPolicyModifiedViolation,
    rvAWSEC2NetworkInterfaceViolation,
  )
where

import Network.AWS.FMS.Types.AWSEC2InstanceViolation
import Network.AWS.FMS.Types.AWSEC2NetworkInterfaceViolation
import Network.AWS.FMS.Types.AWSVPCSecurityGroupViolation
import Network.AWS.FMS.Types.NetworkFirewallMissingExpectedRTViolation
import Network.AWS.FMS.Types.NetworkFirewallMissingFirewallViolation
import Network.AWS.FMS.Types.NetworkFirewallMissingSubnetViolation
import Network.AWS.FMS.Types.NetworkFirewallPolicyModifiedViolation
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Violation detail based on resource type.
--
-- /See:/ 'mkResourceViolation' smart constructor.
data ResourceViolation = ResourceViolation'
  { networkFirewallMissingExpectedRTViolation ::
      Lude.Maybe NetworkFirewallMissingExpectedRTViolation,
    networkFirewallMissingFirewallViolation ::
      Lude.Maybe NetworkFirewallMissingFirewallViolation,
    networkFirewallMissingSubnetViolation ::
      Lude.Maybe NetworkFirewallMissingSubnetViolation,
    awsEC2InstanceViolation ::
      Lude.Maybe AWSEC2InstanceViolation,
    awsVPCSecurityGroupViolation ::
      Lude.Maybe AWSVPCSecurityGroupViolation,
    networkFirewallPolicyModifiedViolation ::
      Lude.Maybe NetworkFirewallPolicyModifiedViolation,
    awsEC2NetworkInterfaceViolation ::
      Lude.Maybe AWSEC2NetworkInterfaceViolation
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceViolation' with the minimum fields required to make a request.
--
-- * 'awsEC2InstanceViolation' - Violation details for an EC2 instance.
-- * 'awsEC2NetworkInterfaceViolation' - Violation details for network interface.
-- * 'awsVPCSecurityGroupViolation' - Violation details for security groups.
-- * 'networkFirewallMissingExpectedRTViolation' - Violation detail for an Network Firewall policy that indicates that a subnet is not associated with the expected Firewall Manager managed route table.
-- * 'networkFirewallMissingFirewallViolation' - Violation detail for an Network Firewall policy that indicates that a subnet has no Firewall Manager managed firewall in its VPC.
-- * 'networkFirewallMissingSubnetViolation' - Violation detail for an Network Firewall policy that indicates that an Availability Zone is missing the expected Firewall Manager managed subnet.
-- * 'networkFirewallPolicyModifiedViolation' - Violation detail for an Network Firewall policy that indicates that a firewall policy in an individual account has been modified in a way that makes it noncompliant. For example, the individual account owner might have deleted a rule group, changed the priority of a stateless rule group, or changed a policy default action.
mkResourceViolation ::
  ResourceViolation
mkResourceViolation =
  ResourceViolation'
    { networkFirewallMissingExpectedRTViolation =
        Lude.Nothing,
      networkFirewallMissingFirewallViolation = Lude.Nothing,
      networkFirewallMissingSubnetViolation = Lude.Nothing,
      awsEC2InstanceViolation = Lude.Nothing,
      awsVPCSecurityGroupViolation = Lude.Nothing,
      networkFirewallPolicyModifiedViolation = Lude.Nothing,
      awsEC2NetworkInterfaceViolation = Lude.Nothing
    }

-- | Violation detail for an Network Firewall policy that indicates that a subnet is not associated with the expected Firewall Manager managed route table.
--
-- /Note:/ Consider using 'networkFirewallMissingExpectedRTViolation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvNetworkFirewallMissingExpectedRTViolation :: Lens.Lens' ResourceViolation (Lude.Maybe NetworkFirewallMissingExpectedRTViolation)
rvNetworkFirewallMissingExpectedRTViolation = Lens.lens (networkFirewallMissingExpectedRTViolation :: ResourceViolation -> Lude.Maybe NetworkFirewallMissingExpectedRTViolation) (\s a -> s {networkFirewallMissingExpectedRTViolation = a} :: ResourceViolation)
{-# DEPRECATED rvNetworkFirewallMissingExpectedRTViolation "Use generic-lens or generic-optics with 'networkFirewallMissingExpectedRTViolation' instead." #-}

-- | Violation detail for an Network Firewall policy that indicates that a subnet has no Firewall Manager managed firewall in its VPC.
--
-- /Note:/ Consider using 'networkFirewallMissingFirewallViolation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvNetworkFirewallMissingFirewallViolation :: Lens.Lens' ResourceViolation (Lude.Maybe NetworkFirewallMissingFirewallViolation)
rvNetworkFirewallMissingFirewallViolation = Lens.lens (networkFirewallMissingFirewallViolation :: ResourceViolation -> Lude.Maybe NetworkFirewallMissingFirewallViolation) (\s a -> s {networkFirewallMissingFirewallViolation = a} :: ResourceViolation)
{-# DEPRECATED rvNetworkFirewallMissingFirewallViolation "Use generic-lens or generic-optics with 'networkFirewallMissingFirewallViolation' instead." #-}

-- | Violation detail for an Network Firewall policy that indicates that an Availability Zone is missing the expected Firewall Manager managed subnet.
--
-- /Note:/ Consider using 'networkFirewallMissingSubnetViolation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvNetworkFirewallMissingSubnetViolation :: Lens.Lens' ResourceViolation (Lude.Maybe NetworkFirewallMissingSubnetViolation)
rvNetworkFirewallMissingSubnetViolation = Lens.lens (networkFirewallMissingSubnetViolation :: ResourceViolation -> Lude.Maybe NetworkFirewallMissingSubnetViolation) (\s a -> s {networkFirewallMissingSubnetViolation = a} :: ResourceViolation)
{-# DEPRECATED rvNetworkFirewallMissingSubnetViolation "Use generic-lens or generic-optics with 'networkFirewallMissingSubnetViolation' instead." #-}

-- | Violation details for an EC2 instance.
--
-- /Note:/ Consider using 'awsEC2InstanceViolation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvAWSEC2InstanceViolation :: Lens.Lens' ResourceViolation (Lude.Maybe AWSEC2InstanceViolation)
rvAWSEC2InstanceViolation = Lens.lens (awsEC2InstanceViolation :: ResourceViolation -> Lude.Maybe AWSEC2InstanceViolation) (\s a -> s {awsEC2InstanceViolation = a} :: ResourceViolation)
{-# DEPRECATED rvAWSEC2InstanceViolation "Use generic-lens or generic-optics with 'awsEC2InstanceViolation' instead." #-}

-- | Violation details for security groups.
--
-- /Note:/ Consider using 'awsVPCSecurityGroupViolation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvAWSVPCSecurityGroupViolation :: Lens.Lens' ResourceViolation (Lude.Maybe AWSVPCSecurityGroupViolation)
rvAWSVPCSecurityGroupViolation = Lens.lens (awsVPCSecurityGroupViolation :: ResourceViolation -> Lude.Maybe AWSVPCSecurityGroupViolation) (\s a -> s {awsVPCSecurityGroupViolation = a} :: ResourceViolation)
{-# DEPRECATED rvAWSVPCSecurityGroupViolation "Use generic-lens or generic-optics with 'awsVPCSecurityGroupViolation' instead." #-}

-- | Violation detail for an Network Firewall policy that indicates that a firewall policy in an individual account has been modified in a way that makes it noncompliant. For example, the individual account owner might have deleted a rule group, changed the priority of a stateless rule group, or changed a policy default action.
--
-- /Note:/ Consider using 'networkFirewallPolicyModifiedViolation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvNetworkFirewallPolicyModifiedViolation :: Lens.Lens' ResourceViolation (Lude.Maybe NetworkFirewallPolicyModifiedViolation)
rvNetworkFirewallPolicyModifiedViolation = Lens.lens (networkFirewallPolicyModifiedViolation :: ResourceViolation -> Lude.Maybe NetworkFirewallPolicyModifiedViolation) (\s a -> s {networkFirewallPolicyModifiedViolation = a} :: ResourceViolation)
{-# DEPRECATED rvNetworkFirewallPolicyModifiedViolation "Use generic-lens or generic-optics with 'networkFirewallPolicyModifiedViolation' instead." #-}

-- | Violation details for network interface.
--
-- /Note:/ Consider using 'awsEC2NetworkInterfaceViolation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvAWSEC2NetworkInterfaceViolation :: Lens.Lens' ResourceViolation (Lude.Maybe AWSEC2NetworkInterfaceViolation)
rvAWSEC2NetworkInterfaceViolation = Lens.lens (awsEC2NetworkInterfaceViolation :: ResourceViolation -> Lude.Maybe AWSEC2NetworkInterfaceViolation) (\s a -> s {awsEC2NetworkInterfaceViolation = a} :: ResourceViolation)
{-# DEPRECATED rvAWSEC2NetworkInterfaceViolation "Use generic-lens or generic-optics with 'awsEC2NetworkInterfaceViolation' instead." #-}

instance Lude.FromJSON ResourceViolation where
  parseJSON =
    Lude.withObject
      "ResourceViolation"
      ( \x ->
          ResourceViolation'
            Lude.<$> (x Lude..:? "NetworkFirewallMissingExpectedRTViolation")
            Lude.<*> (x Lude..:? "NetworkFirewallMissingFirewallViolation")
            Lude.<*> (x Lude..:? "NetworkFirewallMissingSubnetViolation")
            Lude.<*> (x Lude..:? "AwsEc2InstanceViolation")
            Lude.<*> (x Lude..:? "AwsVPCSecurityGroupViolation")
            Lude.<*> (x Lude..:? "NetworkFirewallPolicyModifiedViolation")
            Lude.<*> (x Lude..:? "AwsEc2NetworkInterfaceViolation")
      )
