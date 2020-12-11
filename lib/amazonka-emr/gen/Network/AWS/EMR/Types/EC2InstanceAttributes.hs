-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.EC2InstanceAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.EC2InstanceAttributes
  ( EC2InstanceAttributes (..),

    -- * Smart constructor
    mkEC2InstanceAttributes,

    -- * Lenses
    eiaEC2KeyName,
    eiaEmrManagedSlaveSecurityGroup,
    eiaAdditionalSlaveSecurityGroups,
    eiaRequestedEC2SubnetIds,
    eiaAdditionalMasterSecurityGroups,
    eiaIAMInstanceProfile,
    eiaEmrManagedMasterSecurityGroup,
    eiaEC2SubnetId,
    eiaRequestedEC2AvailabilityZones,
    eiaServiceAccessSecurityGroup,
    eiaEC2AvailabilityZone,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about the EC2 instances in a cluster grouped by category. For example, key name, subnet ID, IAM instance profile, and so on.
--
-- /See:/ 'mkEC2InstanceAttributes' smart constructor.
data EC2InstanceAttributes = EC2InstanceAttributes'
  { ec2KeyName ::
      Lude.Maybe Lude.Text,
    emrManagedSlaveSecurityGroup ::
      Lude.Maybe Lude.Text,
    additionalSlaveSecurityGroups ::
      Lude.Maybe [Lude.Text],
    requestedEC2SubnetIds :: Lude.Maybe [Lude.Text],
    additionalMasterSecurityGroups ::
      Lude.Maybe [Lude.Text],
    iamInstanceProfile :: Lude.Maybe Lude.Text,
    emrManagedMasterSecurityGroup ::
      Lude.Maybe Lude.Text,
    ec2SubnetId :: Lude.Maybe Lude.Text,
    requestedEC2AvailabilityZones ::
      Lude.Maybe [Lude.Text],
    serviceAccessSecurityGroup ::
      Lude.Maybe Lude.Text,
    ec2AvailabilityZone :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EC2InstanceAttributes' with the minimum fields required to make a request.
--
-- * 'additionalMasterSecurityGroups' - A list of additional Amazon EC2 security group IDs for the master node.
-- * 'additionalSlaveSecurityGroups' - A list of additional Amazon EC2 security group IDs for the core and task nodes.
-- * 'ec2AvailabilityZone' - The Availability Zone in which the cluster will run.
-- * 'ec2KeyName' - The name of the Amazon EC2 key pair to use when connecting with SSH into the master node as a user named "hadoop".
-- * 'ec2SubnetId' - Set this parameter to the identifier of the Amazon VPC subnet where you want the cluster to launch. If you do not specify this value, and your account supports EC2-Classic, the cluster launches in EC2-Classic.
-- * 'emrManagedMasterSecurityGroup' - The identifier of the Amazon EC2 security group for the master node.
-- * 'emrManagedSlaveSecurityGroup' - The identifier of the Amazon EC2 security group for the core and task nodes.
-- * 'iamInstanceProfile' - The IAM role that was specified when the cluster was launched. The EC2 instances of the cluster assume this role.
-- * 'requestedEC2AvailabilityZones' - Applies to clusters configured with the instance fleets option. Specifies one or more Availability Zones in which to launch EC2 cluster instances when the EC2-Classic network configuration is supported. Amazon EMR chooses the Availability Zone with the best fit from among the list of @RequestedEc2AvailabilityZones@ , and then launches all cluster instances within that Availability Zone. If you do not specify this value, Amazon EMR chooses the Availability Zone for you. @RequestedEc2SubnetIDs@ and @RequestedEc2AvailabilityZones@ cannot be specified together.
-- * 'requestedEC2SubnetIds' - Applies to clusters configured with the instance fleets option. Specifies the unique identifier of one or more Amazon EC2 subnets in which to launch EC2 cluster instances. Subnets must exist within the same VPC. Amazon EMR chooses the EC2 subnet with the best fit from among the list of @RequestedEc2SubnetIds@ , and then launches all cluster instances within that Subnet. If this value is not specified, and the account and Region support EC2-Classic networks, the cluster launches instances in the EC2-Classic network and uses @RequestedEc2AvailabilityZones@ instead of this setting. If EC2-Classic is not supported, and no Subnet is specified, Amazon EMR chooses the subnet for you. @RequestedEc2SubnetIDs@ and @RequestedEc2AvailabilityZones@ cannot be specified together.
-- * 'serviceAccessSecurityGroup' - The identifier of the Amazon EC2 security group for the Amazon EMR service to access clusters in VPC private subnets.
mkEC2InstanceAttributes ::
  EC2InstanceAttributes
mkEC2InstanceAttributes =
  EC2InstanceAttributes'
    { ec2KeyName = Lude.Nothing,
      emrManagedSlaveSecurityGroup = Lude.Nothing,
      additionalSlaveSecurityGroups = Lude.Nothing,
      requestedEC2SubnetIds = Lude.Nothing,
      additionalMasterSecurityGroups = Lude.Nothing,
      iamInstanceProfile = Lude.Nothing,
      emrManagedMasterSecurityGroup = Lude.Nothing,
      ec2SubnetId = Lude.Nothing,
      requestedEC2AvailabilityZones = Lude.Nothing,
      serviceAccessSecurityGroup = Lude.Nothing,
      ec2AvailabilityZone = Lude.Nothing
    }

-- | The name of the Amazon EC2 key pair to use when connecting with SSH into the master node as a user named "hadoop".
--
-- /Note:/ Consider using 'ec2KeyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiaEC2KeyName :: Lens.Lens' EC2InstanceAttributes (Lude.Maybe Lude.Text)
eiaEC2KeyName = Lens.lens (ec2KeyName :: EC2InstanceAttributes -> Lude.Maybe Lude.Text) (\s a -> s {ec2KeyName = a} :: EC2InstanceAttributes)
{-# DEPRECATED eiaEC2KeyName "Use generic-lens or generic-optics with 'ec2KeyName' instead." #-}

-- | The identifier of the Amazon EC2 security group for the core and task nodes.
--
-- /Note:/ Consider using 'emrManagedSlaveSecurityGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiaEmrManagedSlaveSecurityGroup :: Lens.Lens' EC2InstanceAttributes (Lude.Maybe Lude.Text)
eiaEmrManagedSlaveSecurityGroup = Lens.lens (emrManagedSlaveSecurityGroup :: EC2InstanceAttributes -> Lude.Maybe Lude.Text) (\s a -> s {emrManagedSlaveSecurityGroup = a} :: EC2InstanceAttributes)
{-# DEPRECATED eiaEmrManagedSlaveSecurityGroup "Use generic-lens or generic-optics with 'emrManagedSlaveSecurityGroup' instead." #-}

-- | A list of additional Amazon EC2 security group IDs for the core and task nodes.
--
-- /Note:/ Consider using 'additionalSlaveSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiaAdditionalSlaveSecurityGroups :: Lens.Lens' EC2InstanceAttributes (Lude.Maybe [Lude.Text])
eiaAdditionalSlaveSecurityGroups = Lens.lens (additionalSlaveSecurityGroups :: EC2InstanceAttributes -> Lude.Maybe [Lude.Text]) (\s a -> s {additionalSlaveSecurityGroups = a} :: EC2InstanceAttributes)
{-# DEPRECATED eiaAdditionalSlaveSecurityGroups "Use generic-lens or generic-optics with 'additionalSlaveSecurityGroups' instead." #-}

-- | Applies to clusters configured with the instance fleets option. Specifies the unique identifier of one or more Amazon EC2 subnets in which to launch EC2 cluster instances. Subnets must exist within the same VPC. Amazon EMR chooses the EC2 subnet with the best fit from among the list of @RequestedEc2SubnetIds@ , and then launches all cluster instances within that Subnet. If this value is not specified, and the account and Region support EC2-Classic networks, the cluster launches instances in the EC2-Classic network and uses @RequestedEc2AvailabilityZones@ instead of this setting. If EC2-Classic is not supported, and no Subnet is specified, Amazon EMR chooses the subnet for you. @RequestedEc2SubnetIDs@ and @RequestedEc2AvailabilityZones@ cannot be specified together.
--
-- /Note:/ Consider using 'requestedEC2SubnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiaRequestedEC2SubnetIds :: Lens.Lens' EC2InstanceAttributes (Lude.Maybe [Lude.Text])
eiaRequestedEC2SubnetIds = Lens.lens (requestedEC2SubnetIds :: EC2InstanceAttributes -> Lude.Maybe [Lude.Text]) (\s a -> s {requestedEC2SubnetIds = a} :: EC2InstanceAttributes)
{-# DEPRECATED eiaRequestedEC2SubnetIds "Use generic-lens or generic-optics with 'requestedEC2SubnetIds' instead." #-}

-- | A list of additional Amazon EC2 security group IDs for the master node.
--
-- /Note:/ Consider using 'additionalMasterSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiaAdditionalMasterSecurityGroups :: Lens.Lens' EC2InstanceAttributes (Lude.Maybe [Lude.Text])
eiaAdditionalMasterSecurityGroups = Lens.lens (additionalMasterSecurityGroups :: EC2InstanceAttributes -> Lude.Maybe [Lude.Text]) (\s a -> s {additionalMasterSecurityGroups = a} :: EC2InstanceAttributes)
{-# DEPRECATED eiaAdditionalMasterSecurityGroups "Use generic-lens or generic-optics with 'additionalMasterSecurityGroups' instead." #-}

-- | The IAM role that was specified when the cluster was launched. The EC2 instances of the cluster assume this role.
--
-- /Note:/ Consider using 'iamInstanceProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiaIAMInstanceProfile :: Lens.Lens' EC2InstanceAttributes (Lude.Maybe Lude.Text)
eiaIAMInstanceProfile = Lens.lens (iamInstanceProfile :: EC2InstanceAttributes -> Lude.Maybe Lude.Text) (\s a -> s {iamInstanceProfile = a} :: EC2InstanceAttributes)
{-# DEPRECATED eiaIAMInstanceProfile "Use generic-lens or generic-optics with 'iamInstanceProfile' instead." #-}

-- | The identifier of the Amazon EC2 security group for the master node.
--
-- /Note:/ Consider using 'emrManagedMasterSecurityGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiaEmrManagedMasterSecurityGroup :: Lens.Lens' EC2InstanceAttributes (Lude.Maybe Lude.Text)
eiaEmrManagedMasterSecurityGroup = Lens.lens (emrManagedMasterSecurityGroup :: EC2InstanceAttributes -> Lude.Maybe Lude.Text) (\s a -> s {emrManagedMasterSecurityGroup = a} :: EC2InstanceAttributes)
{-# DEPRECATED eiaEmrManagedMasterSecurityGroup "Use generic-lens or generic-optics with 'emrManagedMasterSecurityGroup' instead." #-}

-- | Set this parameter to the identifier of the Amazon VPC subnet where you want the cluster to launch. If you do not specify this value, and your account supports EC2-Classic, the cluster launches in EC2-Classic.
--
-- /Note:/ Consider using 'ec2SubnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiaEC2SubnetId :: Lens.Lens' EC2InstanceAttributes (Lude.Maybe Lude.Text)
eiaEC2SubnetId = Lens.lens (ec2SubnetId :: EC2InstanceAttributes -> Lude.Maybe Lude.Text) (\s a -> s {ec2SubnetId = a} :: EC2InstanceAttributes)
{-# DEPRECATED eiaEC2SubnetId "Use generic-lens or generic-optics with 'ec2SubnetId' instead." #-}

-- | Applies to clusters configured with the instance fleets option. Specifies one or more Availability Zones in which to launch EC2 cluster instances when the EC2-Classic network configuration is supported. Amazon EMR chooses the Availability Zone with the best fit from among the list of @RequestedEc2AvailabilityZones@ , and then launches all cluster instances within that Availability Zone. If you do not specify this value, Amazon EMR chooses the Availability Zone for you. @RequestedEc2SubnetIDs@ and @RequestedEc2AvailabilityZones@ cannot be specified together.
--
-- /Note:/ Consider using 'requestedEC2AvailabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiaRequestedEC2AvailabilityZones :: Lens.Lens' EC2InstanceAttributes (Lude.Maybe [Lude.Text])
eiaRequestedEC2AvailabilityZones = Lens.lens (requestedEC2AvailabilityZones :: EC2InstanceAttributes -> Lude.Maybe [Lude.Text]) (\s a -> s {requestedEC2AvailabilityZones = a} :: EC2InstanceAttributes)
{-# DEPRECATED eiaRequestedEC2AvailabilityZones "Use generic-lens or generic-optics with 'requestedEC2AvailabilityZones' instead." #-}

-- | The identifier of the Amazon EC2 security group for the Amazon EMR service to access clusters in VPC private subnets.
--
-- /Note:/ Consider using 'serviceAccessSecurityGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiaServiceAccessSecurityGroup :: Lens.Lens' EC2InstanceAttributes (Lude.Maybe Lude.Text)
eiaServiceAccessSecurityGroup = Lens.lens (serviceAccessSecurityGroup :: EC2InstanceAttributes -> Lude.Maybe Lude.Text) (\s a -> s {serviceAccessSecurityGroup = a} :: EC2InstanceAttributes)
{-# DEPRECATED eiaServiceAccessSecurityGroup "Use generic-lens or generic-optics with 'serviceAccessSecurityGroup' instead." #-}

-- | The Availability Zone in which the cluster will run.
--
-- /Note:/ Consider using 'ec2AvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiaEC2AvailabilityZone :: Lens.Lens' EC2InstanceAttributes (Lude.Maybe Lude.Text)
eiaEC2AvailabilityZone = Lens.lens (ec2AvailabilityZone :: EC2InstanceAttributes -> Lude.Maybe Lude.Text) (\s a -> s {ec2AvailabilityZone = a} :: EC2InstanceAttributes)
{-# DEPRECATED eiaEC2AvailabilityZone "Use generic-lens or generic-optics with 'ec2AvailabilityZone' instead." #-}

instance Lude.FromJSON EC2InstanceAttributes where
  parseJSON =
    Lude.withObject
      "EC2InstanceAttributes"
      ( \x ->
          EC2InstanceAttributes'
            Lude.<$> (x Lude..:? "Ec2KeyName")
            Lude.<*> (x Lude..:? "EmrManagedSlaveSecurityGroup")
            Lude.<*> (x Lude..:? "AdditionalSlaveSecurityGroups" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "RequestedEc2SubnetIds" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "AdditionalMasterSecurityGroups" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "IamInstanceProfile")
            Lude.<*> (x Lude..:? "EmrManagedMasterSecurityGroup")
            Lude.<*> (x Lude..:? "Ec2SubnetId")
            Lude.<*> (x Lude..:? "RequestedEc2AvailabilityZones" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ServiceAccessSecurityGroup")
            Lude.<*> (x Lude..:? "Ec2AvailabilityZone")
      )
