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
    rvAwsEc2InstanceViolation,
    rvAwsEc2NetworkInterfaceViolation,
    rvAwsVPCSecurityGroupViolation,
    rvNetworkFirewallMissingExpectedRTViolation,
    rvNetworkFirewallMissingFirewallViolation,
    rvNetworkFirewallMissingSubnetViolation,
    rvNetworkFirewallPolicyModifiedViolation,
  )
where

import qualified Network.AWS.FMS.Types.AwsEc2InstanceViolation as Types
import qualified Network.AWS.FMS.Types.AwsEc2NetworkInterfaceViolation as Types
import qualified Network.AWS.FMS.Types.AwsVPCSecurityGroupViolation as Types
import qualified Network.AWS.FMS.Types.NetworkFirewallMissingExpectedRTViolation as Types
import qualified Network.AWS.FMS.Types.NetworkFirewallMissingFirewallViolation as Types
import qualified Network.AWS.FMS.Types.NetworkFirewallMissingSubnetViolation as Types
import qualified Network.AWS.FMS.Types.NetworkFirewallPolicyModifiedViolation as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Violation detail based on resource type.
--
-- /See:/ 'mkResourceViolation' smart constructor.
data ResourceViolation = ResourceViolation'
  { -- | Violation details for an EC2 instance.
    awsEc2InstanceViolation :: Core.Maybe Types.AwsEc2InstanceViolation,
    -- | Violation details for network interface.
    awsEc2NetworkInterfaceViolation :: Core.Maybe Types.AwsEc2NetworkInterfaceViolation,
    -- | Violation details for security groups.
    awsVPCSecurityGroupViolation :: Core.Maybe Types.AwsVPCSecurityGroupViolation,
    -- | Violation detail for an Network Firewall policy that indicates that a subnet is not associated with the expected Firewall Manager managed route table.
    networkFirewallMissingExpectedRTViolation :: Core.Maybe Types.NetworkFirewallMissingExpectedRTViolation,
    -- | Violation detail for an Network Firewall policy that indicates that a subnet has no Firewall Manager managed firewall in its VPC.
    networkFirewallMissingFirewallViolation :: Core.Maybe Types.NetworkFirewallMissingFirewallViolation,
    -- | Violation detail for an Network Firewall policy that indicates that an Availability Zone is missing the expected Firewall Manager managed subnet.
    networkFirewallMissingSubnetViolation :: Core.Maybe Types.NetworkFirewallMissingSubnetViolation,
    -- | Violation detail for an Network Firewall policy that indicates that a firewall policy in an individual account has been modified in a way that makes it noncompliant. For example, the individual account owner might have deleted a rule group, changed the priority of a stateless rule group, or changed a policy default action.
    networkFirewallPolicyModifiedViolation :: Core.Maybe Types.NetworkFirewallPolicyModifiedViolation
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceViolation' value with any optional fields omitted.
mkResourceViolation ::
  ResourceViolation
mkResourceViolation =
  ResourceViolation'
    { awsEc2InstanceViolation = Core.Nothing,
      awsEc2NetworkInterfaceViolation = Core.Nothing,
      awsVPCSecurityGroupViolation = Core.Nothing,
      networkFirewallMissingExpectedRTViolation = Core.Nothing,
      networkFirewallMissingFirewallViolation = Core.Nothing,
      networkFirewallMissingSubnetViolation = Core.Nothing,
      networkFirewallPolicyModifiedViolation = Core.Nothing
    }

-- | Violation details for an EC2 instance.
--
-- /Note:/ Consider using 'awsEc2InstanceViolation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvAwsEc2InstanceViolation :: Lens.Lens' ResourceViolation (Core.Maybe Types.AwsEc2InstanceViolation)
rvAwsEc2InstanceViolation = Lens.field @"awsEc2InstanceViolation"
{-# DEPRECATED rvAwsEc2InstanceViolation "Use generic-lens or generic-optics with 'awsEc2InstanceViolation' instead." #-}

-- | Violation details for network interface.
--
-- /Note:/ Consider using 'awsEc2NetworkInterfaceViolation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvAwsEc2NetworkInterfaceViolation :: Lens.Lens' ResourceViolation (Core.Maybe Types.AwsEc2NetworkInterfaceViolation)
rvAwsEc2NetworkInterfaceViolation = Lens.field @"awsEc2NetworkInterfaceViolation"
{-# DEPRECATED rvAwsEc2NetworkInterfaceViolation "Use generic-lens or generic-optics with 'awsEc2NetworkInterfaceViolation' instead." #-}

-- | Violation details for security groups.
--
-- /Note:/ Consider using 'awsVPCSecurityGroupViolation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvAwsVPCSecurityGroupViolation :: Lens.Lens' ResourceViolation (Core.Maybe Types.AwsVPCSecurityGroupViolation)
rvAwsVPCSecurityGroupViolation = Lens.field @"awsVPCSecurityGroupViolation"
{-# DEPRECATED rvAwsVPCSecurityGroupViolation "Use generic-lens or generic-optics with 'awsVPCSecurityGroupViolation' instead." #-}

-- | Violation detail for an Network Firewall policy that indicates that a subnet is not associated with the expected Firewall Manager managed route table.
--
-- /Note:/ Consider using 'networkFirewallMissingExpectedRTViolation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvNetworkFirewallMissingExpectedRTViolation :: Lens.Lens' ResourceViolation (Core.Maybe Types.NetworkFirewallMissingExpectedRTViolation)
rvNetworkFirewallMissingExpectedRTViolation = Lens.field @"networkFirewallMissingExpectedRTViolation"
{-# DEPRECATED rvNetworkFirewallMissingExpectedRTViolation "Use generic-lens or generic-optics with 'networkFirewallMissingExpectedRTViolation' instead." #-}

-- | Violation detail for an Network Firewall policy that indicates that a subnet has no Firewall Manager managed firewall in its VPC.
--
-- /Note:/ Consider using 'networkFirewallMissingFirewallViolation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvNetworkFirewallMissingFirewallViolation :: Lens.Lens' ResourceViolation (Core.Maybe Types.NetworkFirewallMissingFirewallViolation)
rvNetworkFirewallMissingFirewallViolation = Lens.field @"networkFirewallMissingFirewallViolation"
{-# DEPRECATED rvNetworkFirewallMissingFirewallViolation "Use generic-lens or generic-optics with 'networkFirewallMissingFirewallViolation' instead." #-}

-- | Violation detail for an Network Firewall policy that indicates that an Availability Zone is missing the expected Firewall Manager managed subnet.
--
-- /Note:/ Consider using 'networkFirewallMissingSubnetViolation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvNetworkFirewallMissingSubnetViolation :: Lens.Lens' ResourceViolation (Core.Maybe Types.NetworkFirewallMissingSubnetViolation)
rvNetworkFirewallMissingSubnetViolation = Lens.field @"networkFirewallMissingSubnetViolation"
{-# DEPRECATED rvNetworkFirewallMissingSubnetViolation "Use generic-lens or generic-optics with 'networkFirewallMissingSubnetViolation' instead." #-}

-- | Violation detail for an Network Firewall policy that indicates that a firewall policy in an individual account has been modified in a way that makes it noncompliant. For example, the individual account owner might have deleted a rule group, changed the priority of a stateless rule group, or changed a policy default action.
--
-- /Note:/ Consider using 'networkFirewallPolicyModifiedViolation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvNetworkFirewallPolicyModifiedViolation :: Lens.Lens' ResourceViolation (Core.Maybe Types.NetworkFirewallPolicyModifiedViolation)
rvNetworkFirewallPolicyModifiedViolation = Lens.field @"networkFirewallPolicyModifiedViolation"
{-# DEPRECATED rvNetworkFirewallPolicyModifiedViolation "Use generic-lens or generic-optics with 'networkFirewallPolicyModifiedViolation' instead." #-}

instance Core.FromJSON ResourceViolation where
  parseJSON =
    Core.withObject "ResourceViolation" Core.$
      \x ->
        ResourceViolation'
          Core.<$> (x Core..:? "AwsEc2InstanceViolation")
          Core.<*> (x Core..:? "AwsEc2NetworkInterfaceViolation")
          Core.<*> (x Core..:? "AwsVPCSecurityGroupViolation")
          Core.<*> (x Core..:? "NetworkFirewallMissingExpectedRTViolation")
          Core.<*> (x Core..:? "NetworkFirewallMissingFirewallViolation")
          Core.<*> (x Core..:? "NetworkFirewallMissingSubnetViolation")
          Core.<*> (x Core..:? "NetworkFirewallPolicyModifiedViolation")
