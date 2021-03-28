{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.Ec2InstanceAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.Ec2InstanceAttributes
  ( Ec2InstanceAttributes (..)
  -- * Smart constructor
  , mkEc2InstanceAttributes
  -- * Lenses
  , eiaAdditionalMasterSecurityGroups
  , eiaAdditionalSlaveSecurityGroups
  , eiaEc2AvailabilityZone
  , eiaEc2KeyName
  , eiaEc2SubnetId
  , eiaEmrManagedMasterSecurityGroup
  , eiaEmrManagedSlaveSecurityGroup
  , eiaIamInstanceProfile
  , eiaRequestedEc2AvailabilityZones
  , eiaRequestedEc2SubnetIds
  , eiaServiceAccessSecurityGroup
  ) where

import qualified Network.AWS.EMR.Types.XmlStringMaxLen256 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information about the EC2 instances in a cluster grouped by category. For example, key name, subnet ID, IAM instance profile, and so on.
--
-- /See:/ 'mkEc2InstanceAttributes' smart constructor.
data Ec2InstanceAttributes = Ec2InstanceAttributes'
  { additionalMasterSecurityGroups :: Core.Maybe [Core.Text]
    -- ^ A list of additional Amazon EC2 security group IDs for the master node.
  , additionalSlaveSecurityGroups :: Core.Maybe [Core.Text]
    -- ^ A list of additional Amazon EC2 security group IDs for the core and task nodes.
  , ec2AvailabilityZone :: Core.Maybe Core.Text
    -- ^ The Availability Zone in which the cluster will run. 
  , ec2KeyName :: Core.Maybe Core.Text
    -- ^ The name of the Amazon EC2 key pair to use when connecting with SSH into the master node as a user named "hadoop".
  , ec2SubnetId :: Core.Maybe Core.Text
    -- ^ Set this parameter to the identifier of the Amazon VPC subnet where you want the cluster to launch. If you do not specify this value, and your account supports EC2-Classic, the cluster launches in EC2-Classic.
  , emrManagedMasterSecurityGroup :: Core.Maybe Core.Text
    -- ^ The identifier of the Amazon EC2 security group for the master node.
  , emrManagedSlaveSecurityGroup :: Core.Maybe Core.Text
    -- ^ The identifier of the Amazon EC2 security group for the core and task nodes.
  , iamInstanceProfile :: Core.Maybe Core.Text
    -- ^ The IAM role that was specified when the cluster was launched. The EC2 instances of the cluster assume this role.
  , requestedEc2AvailabilityZones :: Core.Maybe [Types.XmlStringMaxLen256]
    -- ^ Applies to clusters configured with the instance fleets option. Specifies one or more Availability Zones in which to launch EC2 cluster instances when the EC2-Classic network configuration is supported. Amazon EMR chooses the Availability Zone with the best fit from among the list of @RequestedEc2AvailabilityZones@ , and then launches all cluster instances within that Availability Zone. If you do not specify this value, Amazon EMR chooses the Availability Zone for you. @RequestedEc2SubnetIDs@ and @RequestedEc2AvailabilityZones@ cannot be specified together.
  , requestedEc2SubnetIds :: Core.Maybe [Types.XmlStringMaxLen256]
    -- ^ Applies to clusters configured with the instance fleets option. Specifies the unique identifier of one or more Amazon EC2 subnets in which to launch EC2 cluster instances. Subnets must exist within the same VPC. Amazon EMR chooses the EC2 subnet with the best fit from among the list of @RequestedEc2SubnetIds@ , and then launches all cluster instances within that Subnet. If this value is not specified, and the account and Region support EC2-Classic networks, the cluster launches instances in the EC2-Classic network and uses @RequestedEc2AvailabilityZones@ instead of this setting. If EC2-Classic is not supported, and no Subnet is specified, Amazon EMR chooses the subnet for you. @RequestedEc2SubnetIDs@ and @RequestedEc2AvailabilityZones@ cannot be specified together.
  , serviceAccessSecurityGroup :: Core.Maybe Core.Text
    -- ^ The identifier of the Amazon EC2 security group for the Amazon EMR service to access clusters in VPC private subnets.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Ec2InstanceAttributes' value with any optional fields omitted.
mkEc2InstanceAttributes
    :: Ec2InstanceAttributes
mkEc2InstanceAttributes
  = Ec2InstanceAttributes'{additionalMasterSecurityGroups =
                             Core.Nothing,
                           additionalSlaveSecurityGroups = Core.Nothing,
                           ec2AvailabilityZone = Core.Nothing, ec2KeyName = Core.Nothing,
                           ec2SubnetId = Core.Nothing,
                           emrManagedMasterSecurityGroup = Core.Nothing,
                           emrManagedSlaveSecurityGroup = Core.Nothing,
                           iamInstanceProfile = Core.Nothing,
                           requestedEc2AvailabilityZones = Core.Nothing,
                           requestedEc2SubnetIds = Core.Nothing,
                           serviceAccessSecurityGroup = Core.Nothing}

-- | A list of additional Amazon EC2 security group IDs for the master node.
--
-- /Note:/ Consider using 'additionalMasterSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiaAdditionalMasterSecurityGroups :: Lens.Lens' Ec2InstanceAttributes (Core.Maybe [Core.Text])
eiaAdditionalMasterSecurityGroups = Lens.field @"additionalMasterSecurityGroups"
{-# INLINEABLE eiaAdditionalMasterSecurityGroups #-}
{-# DEPRECATED additionalMasterSecurityGroups "Use generic-lens or generic-optics with 'additionalMasterSecurityGroups' instead"  #-}

-- | A list of additional Amazon EC2 security group IDs for the core and task nodes.
--
-- /Note:/ Consider using 'additionalSlaveSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiaAdditionalSlaveSecurityGroups :: Lens.Lens' Ec2InstanceAttributes (Core.Maybe [Core.Text])
eiaAdditionalSlaveSecurityGroups = Lens.field @"additionalSlaveSecurityGroups"
{-# INLINEABLE eiaAdditionalSlaveSecurityGroups #-}
{-# DEPRECATED additionalSlaveSecurityGroups "Use generic-lens or generic-optics with 'additionalSlaveSecurityGroups' instead"  #-}

-- | The Availability Zone in which the cluster will run. 
--
-- /Note:/ Consider using 'ec2AvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiaEc2AvailabilityZone :: Lens.Lens' Ec2InstanceAttributes (Core.Maybe Core.Text)
eiaEc2AvailabilityZone = Lens.field @"ec2AvailabilityZone"
{-# INLINEABLE eiaEc2AvailabilityZone #-}
{-# DEPRECATED ec2AvailabilityZone "Use generic-lens or generic-optics with 'ec2AvailabilityZone' instead"  #-}

-- | The name of the Amazon EC2 key pair to use when connecting with SSH into the master node as a user named "hadoop".
--
-- /Note:/ Consider using 'ec2KeyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiaEc2KeyName :: Lens.Lens' Ec2InstanceAttributes (Core.Maybe Core.Text)
eiaEc2KeyName = Lens.field @"ec2KeyName"
{-# INLINEABLE eiaEc2KeyName #-}
{-# DEPRECATED ec2KeyName "Use generic-lens or generic-optics with 'ec2KeyName' instead"  #-}

-- | Set this parameter to the identifier of the Amazon VPC subnet where you want the cluster to launch. If you do not specify this value, and your account supports EC2-Classic, the cluster launches in EC2-Classic.
--
-- /Note:/ Consider using 'ec2SubnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiaEc2SubnetId :: Lens.Lens' Ec2InstanceAttributes (Core.Maybe Core.Text)
eiaEc2SubnetId = Lens.field @"ec2SubnetId"
{-# INLINEABLE eiaEc2SubnetId #-}
{-# DEPRECATED ec2SubnetId "Use generic-lens or generic-optics with 'ec2SubnetId' instead"  #-}

-- | The identifier of the Amazon EC2 security group for the master node.
--
-- /Note:/ Consider using 'emrManagedMasterSecurityGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiaEmrManagedMasterSecurityGroup :: Lens.Lens' Ec2InstanceAttributes (Core.Maybe Core.Text)
eiaEmrManagedMasterSecurityGroup = Lens.field @"emrManagedMasterSecurityGroup"
{-# INLINEABLE eiaEmrManagedMasterSecurityGroup #-}
{-# DEPRECATED emrManagedMasterSecurityGroup "Use generic-lens or generic-optics with 'emrManagedMasterSecurityGroup' instead"  #-}

-- | The identifier of the Amazon EC2 security group for the core and task nodes.
--
-- /Note:/ Consider using 'emrManagedSlaveSecurityGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiaEmrManagedSlaveSecurityGroup :: Lens.Lens' Ec2InstanceAttributes (Core.Maybe Core.Text)
eiaEmrManagedSlaveSecurityGroup = Lens.field @"emrManagedSlaveSecurityGroup"
{-# INLINEABLE eiaEmrManagedSlaveSecurityGroup #-}
{-# DEPRECATED emrManagedSlaveSecurityGroup "Use generic-lens or generic-optics with 'emrManagedSlaveSecurityGroup' instead"  #-}

-- | The IAM role that was specified when the cluster was launched. The EC2 instances of the cluster assume this role.
--
-- /Note:/ Consider using 'iamInstanceProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiaIamInstanceProfile :: Lens.Lens' Ec2InstanceAttributes (Core.Maybe Core.Text)
eiaIamInstanceProfile = Lens.field @"iamInstanceProfile"
{-# INLINEABLE eiaIamInstanceProfile #-}
{-# DEPRECATED iamInstanceProfile "Use generic-lens or generic-optics with 'iamInstanceProfile' instead"  #-}

-- | Applies to clusters configured with the instance fleets option. Specifies one or more Availability Zones in which to launch EC2 cluster instances when the EC2-Classic network configuration is supported. Amazon EMR chooses the Availability Zone with the best fit from among the list of @RequestedEc2AvailabilityZones@ , and then launches all cluster instances within that Availability Zone. If you do not specify this value, Amazon EMR chooses the Availability Zone for you. @RequestedEc2SubnetIDs@ and @RequestedEc2AvailabilityZones@ cannot be specified together.
--
-- /Note:/ Consider using 'requestedEc2AvailabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiaRequestedEc2AvailabilityZones :: Lens.Lens' Ec2InstanceAttributes (Core.Maybe [Types.XmlStringMaxLen256])
eiaRequestedEc2AvailabilityZones = Lens.field @"requestedEc2AvailabilityZones"
{-# INLINEABLE eiaRequestedEc2AvailabilityZones #-}
{-# DEPRECATED requestedEc2AvailabilityZones "Use generic-lens or generic-optics with 'requestedEc2AvailabilityZones' instead"  #-}

-- | Applies to clusters configured with the instance fleets option. Specifies the unique identifier of one or more Amazon EC2 subnets in which to launch EC2 cluster instances. Subnets must exist within the same VPC. Amazon EMR chooses the EC2 subnet with the best fit from among the list of @RequestedEc2SubnetIds@ , and then launches all cluster instances within that Subnet. If this value is not specified, and the account and Region support EC2-Classic networks, the cluster launches instances in the EC2-Classic network and uses @RequestedEc2AvailabilityZones@ instead of this setting. If EC2-Classic is not supported, and no Subnet is specified, Amazon EMR chooses the subnet for you. @RequestedEc2SubnetIDs@ and @RequestedEc2AvailabilityZones@ cannot be specified together.
--
-- /Note:/ Consider using 'requestedEc2SubnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiaRequestedEc2SubnetIds :: Lens.Lens' Ec2InstanceAttributes (Core.Maybe [Types.XmlStringMaxLen256])
eiaRequestedEc2SubnetIds = Lens.field @"requestedEc2SubnetIds"
{-# INLINEABLE eiaRequestedEc2SubnetIds #-}
{-# DEPRECATED requestedEc2SubnetIds "Use generic-lens or generic-optics with 'requestedEc2SubnetIds' instead"  #-}

-- | The identifier of the Amazon EC2 security group for the Amazon EMR service to access clusters in VPC private subnets.
--
-- /Note:/ Consider using 'serviceAccessSecurityGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiaServiceAccessSecurityGroup :: Lens.Lens' Ec2InstanceAttributes (Core.Maybe Core.Text)
eiaServiceAccessSecurityGroup = Lens.field @"serviceAccessSecurityGroup"
{-# INLINEABLE eiaServiceAccessSecurityGroup #-}
{-# DEPRECATED serviceAccessSecurityGroup "Use generic-lens or generic-optics with 'serviceAccessSecurityGroup' instead"  #-}

instance Core.FromJSON Ec2InstanceAttributes where
        parseJSON
          = Core.withObject "Ec2InstanceAttributes" Core.$
              \ x ->
                Ec2InstanceAttributes' Core.<$>
                  (x Core..:? "AdditionalMasterSecurityGroups") Core.<*>
                    x Core..:? "AdditionalSlaveSecurityGroups"
                    Core.<*> x Core..:? "Ec2AvailabilityZone"
                    Core.<*> x Core..:? "Ec2KeyName"
                    Core.<*> x Core..:? "Ec2SubnetId"
                    Core.<*> x Core..:? "EmrManagedMasterSecurityGroup"
                    Core.<*> x Core..:? "EmrManagedSlaveSecurityGroup"
                    Core.<*> x Core..:? "IamInstanceProfile"
                    Core.<*> x Core..:? "RequestedEc2AvailabilityZones"
                    Core.<*> x Core..:? "RequestedEc2SubnetIds"
                    Core.<*> x Core..:? "ServiceAccessSecurityGroup"
