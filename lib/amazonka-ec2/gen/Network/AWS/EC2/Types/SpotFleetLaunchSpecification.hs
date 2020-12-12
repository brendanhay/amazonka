{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotFleetLaunchSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotFleetLaunchSpecification
  ( SpotFleetLaunchSpecification (..),

    -- * Smart constructor
    mkSpotFleetLaunchSpecification,

    -- * Lenses
    sflsSecurityGroups,
    sflsSpotPrice,
    sflsWeightedCapacity,
    sflsKeyName,
    sflsNetworkInterfaces,
    sflsRAMDiskId,
    sflsSubnetId,
    sflsKernelId,
    sflsInstanceType,
    sflsEBSOptimized,
    sflsUserData,
    sflsMonitoring,
    sflsTagSpecifications,
    sflsIAMInstanceProfile,
    sflsImageId,
    sflsAddressingType,
    sflsBlockDeviceMappings,
    sflsPlacement,
  )
where

import Network.AWS.EC2.Types.BlockDeviceMapping
import Network.AWS.EC2.Types.GroupIdentifier
import Network.AWS.EC2.Types.IAMInstanceProfileSpecification
import Network.AWS.EC2.Types.InstanceNetworkInterfaceSpecification
import Network.AWS.EC2.Types.InstanceType
import Network.AWS.EC2.Types.SpotFleetMonitoring
import Network.AWS.EC2.Types.SpotFleetTagSpecification
import Network.AWS.EC2.Types.SpotPlacement
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the launch specification for one or more Spot Instances. If you include On-Demand capacity in your fleet request or want to specify an EFA network device, you can't use @SpotFleetLaunchSpecification@ ; you must use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_LaunchTemplateConfig.html LaunchTemplateConfig> .
--
-- /See:/ 'mkSpotFleetLaunchSpecification' smart constructor.
data SpotFleetLaunchSpecification = SpotFleetLaunchSpecification'
  { securityGroups ::
      Lude.Maybe [GroupIdentifier],
    spotPrice :: Lude.Maybe Lude.Text,
    weightedCapacity ::
      Lude.Maybe Lude.Double,
    keyName :: Lude.Maybe Lude.Text,
    networkInterfaces ::
      Lude.Maybe
        [InstanceNetworkInterfaceSpecification],
    ramdiskId :: Lude.Maybe Lude.Text,
    subnetId :: Lude.Maybe Lude.Text,
    kernelId :: Lude.Maybe Lude.Text,
    instanceType ::
      Lude.Maybe InstanceType,
    ebsOptimized ::
      Lude.Maybe Lude.Bool,
    userData :: Lude.Maybe Lude.Text,
    monitoring ::
      Lude.Maybe SpotFleetMonitoring,
    tagSpecifications ::
      Lude.Maybe
        [SpotFleetTagSpecification],
    iamInstanceProfile ::
      Lude.Maybe
        IAMInstanceProfileSpecification,
    imageId :: Lude.Maybe Lude.Text,
    addressingType ::
      Lude.Maybe Lude.Text,
    blockDeviceMappings ::
      Lude.Maybe [BlockDeviceMapping],
    placement ::
      Lude.Maybe SpotPlacement
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SpotFleetLaunchSpecification' with the minimum fields required to make a request.
--
-- * 'addressingType' - Deprecated.
-- * 'blockDeviceMappings' - One or more block devices that are mapped to the Spot Instances. You can't specify both a snapshot ID and an encryption value. This is because only blank volumes can be encrypted on creation. If a snapshot is the basis for a volume, it is not blank and its encryption status is used for the volume encryption status.
-- * 'ebsOptimized' - Indicates whether the instances are optimized for EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS Optimized instance.
--
-- Default: @false@
-- * 'iamInstanceProfile' - The IAM instance profile.
-- * 'imageId' - The ID of the AMI.
-- * 'instanceType' - The instance type.
-- * 'kernelId' - The ID of the kernel.
-- * 'keyName' - The name of the key pair.
-- * 'monitoring' - Enable or disable monitoring for the instances.
-- * 'networkInterfaces' - One or more network interfaces. If you specify a network interface, you must specify subnet IDs and security group IDs using the network interface.
-- * 'placement' - The placement information.
-- * 'ramdiskId' - The ID of the RAM disk. Some kernels require additional drivers at launch. Check the kernel requirements for information about whether you need to specify a RAM disk. To find kernel requirements, refer to the AWS Resource Center and search for the kernel ID.
-- * 'securityGroups' - One or more security groups. When requesting instances in a VPC, you must specify the IDs of the security groups. When requesting instances in EC2-Classic, you can specify the names or the IDs of the security groups.
-- * 'spotPrice' - The maximum price per unit hour that you are willing to pay for a Spot Instance. If this value is not specified, the default is the Spot price specified for the fleet. To determine the Spot price per unit hour, divide the Spot price by the value of @WeightedCapacity@ .
-- * 'subnetId' - The IDs of the subnets in which to launch the instances. To specify multiple subnets, separate them using commas; for example, "subnet-1234abcdeexample1, subnet-0987cdef6example2".
-- * 'tagSpecifications' - The tags to apply during creation.
-- * 'userData' - The Base64-encoded user data that instances use when starting up.
-- * 'weightedCapacity' - The number of units provided by the specified instance type. These are the same units that you chose to set the target capacity in terms of instances, or a performance characteristic such as vCPUs, memory, or I/O.
--
-- If the target capacity divided by this value is not a whole number, Amazon EC2 rounds the number of instances to the next whole number. If this value is not specified, the default is 1.
mkSpotFleetLaunchSpecification ::
  SpotFleetLaunchSpecification
mkSpotFleetLaunchSpecification =
  SpotFleetLaunchSpecification'
    { securityGroups = Lude.Nothing,
      spotPrice = Lude.Nothing,
      weightedCapacity = Lude.Nothing,
      keyName = Lude.Nothing,
      networkInterfaces = Lude.Nothing,
      ramdiskId = Lude.Nothing,
      subnetId = Lude.Nothing,
      kernelId = Lude.Nothing,
      instanceType = Lude.Nothing,
      ebsOptimized = Lude.Nothing,
      userData = Lude.Nothing,
      monitoring = Lude.Nothing,
      tagSpecifications = Lude.Nothing,
      iamInstanceProfile = Lude.Nothing,
      imageId = Lude.Nothing,
      addressingType = Lude.Nothing,
      blockDeviceMappings = Lude.Nothing,
      placement = Lude.Nothing
    }

-- | One or more security groups. When requesting instances in a VPC, you must specify the IDs of the security groups. When requesting instances in EC2-Classic, you can specify the names or the IDs of the security groups.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsSecurityGroups :: Lens.Lens' SpotFleetLaunchSpecification (Lude.Maybe [GroupIdentifier])
sflsSecurityGroups = Lens.lens (securityGroups :: SpotFleetLaunchSpecification -> Lude.Maybe [GroupIdentifier]) (\s a -> s {securityGroups = a} :: SpotFleetLaunchSpecification)
{-# DEPRECATED sflsSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | The maximum price per unit hour that you are willing to pay for a Spot Instance. If this value is not specified, the default is the Spot price specified for the fleet. To determine the Spot price per unit hour, divide the Spot price by the value of @WeightedCapacity@ .
--
-- /Note:/ Consider using 'spotPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsSpotPrice :: Lens.Lens' SpotFleetLaunchSpecification (Lude.Maybe Lude.Text)
sflsSpotPrice = Lens.lens (spotPrice :: SpotFleetLaunchSpecification -> Lude.Maybe Lude.Text) (\s a -> s {spotPrice = a} :: SpotFleetLaunchSpecification)
{-# DEPRECATED sflsSpotPrice "Use generic-lens or generic-optics with 'spotPrice' instead." #-}

-- | The number of units provided by the specified instance type. These are the same units that you chose to set the target capacity in terms of instances, or a performance characteristic such as vCPUs, memory, or I/O.
--
-- If the target capacity divided by this value is not a whole number, Amazon EC2 rounds the number of instances to the next whole number. If this value is not specified, the default is 1.
--
-- /Note:/ Consider using 'weightedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsWeightedCapacity :: Lens.Lens' SpotFleetLaunchSpecification (Lude.Maybe Lude.Double)
sflsWeightedCapacity = Lens.lens (weightedCapacity :: SpotFleetLaunchSpecification -> Lude.Maybe Lude.Double) (\s a -> s {weightedCapacity = a} :: SpotFleetLaunchSpecification)
{-# DEPRECATED sflsWeightedCapacity "Use generic-lens or generic-optics with 'weightedCapacity' instead." #-}

-- | The name of the key pair.
--
-- /Note:/ Consider using 'keyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsKeyName :: Lens.Lens' SpotFleetLaunchSpecification (Lude.Maybe Lude.Text)
sflsKeyName = Lens.lens (keyName :: SpotFleetLaunchSpecification -> Lude.Maybe Lude.Text) (\s a -> s {keyName = a} :: SpotFleetLaunchSpecification)
{-# DEPRECATED sflsKeyName "Use generic-lens or generic-optics with 'keyName' instead." #-}

-- | One or more network interfaces. If you specify a network interface, you must specify subnet IDs and security group IDs using the network interface.
--
-- /Note:/ Consider using 'networkInterfaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsNetworkInterfaces :: Lens.Lens' SpotFleetLaunchSpecification (Lude.Maybe [InstanceNetworkInterfaceSpecification])
sflsNetworkInterfaces = Lens.lens (networkInterfaces :: SpotFleetLaunchSpecification -> Lude.Maybe [InstanceNetworkInterfaceSpecification]) (\s a -> s {networkInterfaces = a} :: SpotFleetLaunchSpecification)
{-# DEPRECATED sflsNetworkInterfaces "Use generic-lens or generic-optics with 'networkInterfaces' instead." #-}

-- | The ID of the RAM disk. Some kernels require additional drivers at launch. Check the kernel requirements for information about whether you need to specify a RAM disk. To find kernel requirements, refer to the AWS Resource Center and search for the kernel ID.
--
-- /Note:/ Consider using 'ramdiskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsRAMDiskId :: Lens.Lens' SpotFleetLaunchSpecification (Lude.Maybe Lude.Text)
sflsRAMDiskId = Lens.lens (ramdiskId :: SpotFleetLaunchSpecification -> Lude.Maybe Lude.Text) (\s a -> s {ramdiskId = a} :: SpotFleetLaunchSpecification)
{-# DEPRECATED sflsRAMDiskId "Use generic-lens or generic-optics with 'ramdiskId' instead." #-}

-- | The IDs of the subnets in which to launch the instances. To specify multiple subnets, separate them using commas; for example, "subnet-1234abcdeexample1, subnet-0987cdef6example2".
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsSubnetId :: Lens.Lens' SpotFleetLaunchSpecification (Lude.Maybe Lude.Text)
sflsSubnetId = Lens.lens (subnetId :: SpotFleetLaunchSpecification -> Lude.Maybe Lude.Text) (\s a -> s {subnetId = a} :: SpotFleetLaunchSpecification)
{-# DEPRECATED sflsSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The ID of the kernel.
--
-- /Note:/ Consider using 'kernelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsKernelId :: Lens.Lens' SpotFleetLaunchSpecification (Lude.Maybe Lude.Text)
sflsKernelId = Lens.lens (kernelId :: SpotFleetLaunchSpecification -> Lude.Maybe Lude.Text) (\s a -> s {kernelId = a} :: SpotFleetLaunchSpecification)
{-# DEPRECATED sflsKernelId "Use generic-lens or generic-optics with 'kernelId' instead." #-}

-- | The instance type.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsInstanceType :: Lens.Lens' SpotFleetLaunchSpecification (Lude.Maybe InstanceType)
sflsInstanceType = Lens.lens (instanceType :: SpotFleetLaunchSpecification -> Lude.Maybe InstanceType) (\s a -> s {instanceType = a} :: SpotFleetLaunchSpecification)
{-# DEPRECATED sflsInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | Indicates whether the instances are optimized for EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS Optimized instance.
--
-- Default: @false@
--
-- /Note:/ Consider using 'ebsOptimized' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsEBSOptimized :: Lens.Lens' SpotFleetLaunchSpecification (Lude.Maybe Lude.Bool)
sflsEBSOptimized = Lens.lens (ebsOptimized :: SpotFleetLaunchSpecification -> Lude.Maybe Lude.Bool) (\s a -> s {ebsOptimized = a} :: SpotFleetLaunchSpecification)
{-# DEPRECATED sflsEBSOptimized "Use generic-lens or generic-optics with 'ebsOptimized' instead." #-}

-- | The Base64-encoded user data that instances use when starting up.
--
-- /Note:/ Consider using 'userData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsUserData :: Lens.Lens' SpotFleetLaunchSpecification (Lude.Maybe Lude.Text)
sflsUserData = Lens.lens (userData :: SpotFleetLaunchSpecification -> Lude.Maybe Lude.Text) (\s a -> s {userData = a} :: SpotFleetLaunchSpecification)
{-# DEPRECATED sflsUserData "Use generic-lens or generic-optics with 'userData' instead." #-}

-- | Enable or disable monitoring for the instances.
--
-- /Note:/ Consider using 'monitoring' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsMonitoring :: Lens.Lens' SpotFleetLaunchSpecification (Lude.Maybe SpotFleetMonitoring)
sflsMonitoring = Lens.lens (monitoring :: SpotFleetLaunchSpecification -> Lude.Maybe SpotFleetMonitoring) (\s a -> s {monitoring = a} :: SpotFleetLaunchSpecification)
{-# DEPRECATED sflsMonitoring "Use generic-lens or generic-optics with 'monitoring' instead." #-}

-- | The tags to apply during creation.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsTagSpecifications :: Lens.Lens' SpotFleetLaunchSpecification (Lude.Maybe [SpotFleetTagSpecification])
sflsTagSpecifications = Lens.lens (tagSpecifications :: SpotFleetLaunchSpecification -> Lude.Maybe [SpotFleetTagSpecification]) (\s a -> s {tagSpecifications = a} :: SpotFleetLaunchSpecification)
{-# DEPRECATED sflsTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | The IAM instance profile.
--
-- /Note:/ Consider using 'iamInstanceProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsIAMInstanceProfile :: Lens.Lens' SpotFleetLaunchSpecification (Lude.Maybe IAMInstanceProfileSpecification)
sflsIAMInstanceProfile = Lens.lens (iamInstanceProfile :: SpotFleetLaunchSpecification -> Lude.Maybe IAMInstanceProfileSpecification) (\s a -> s {iamInstanceProfile = a} :: SpotFleetLaunchSpecification)
{-# DEPRECATED sflsIAMInstanceProfile "Use generic-lens or generic-optics with 'iamInstanceProfile' instead." #-}

-- | The ID of the AMI.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsImageId :: Lens.Lens' SpotFleetLaunchSpecification (Lude.Maybe Lude.Text)
sflsImageId = Lens.lens (imageId :: SpotFleetLaunchSpecification -> Lude.Maybe Lude.Text) (\s a -> s {imageId = a} :: SpotFleetLaunchSpecification)
{-# DEPRECATED sflsImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | Deprecated.
--
-- /Note:/ Consider using 'addressingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsAddressingType :: Lens.Lens' SpotFleetLaunchSpecification (Lude.Maybe Lude.Text)
sflsAddressingType = Lens.lens (addressingType :: SpotFleetLaunchSpecification -> Lude.Maybe Lude.Text) (\s a -> s {addressingType = a} :: SpotFleetLaunchSpecification)
{-# DEPRECATED sflsAddressingType "Use generic-lens or generic-optics with 'addressingType' instead." #-}

-- | One or more block devices that are mapped to the Spot Instances. You can't specify both a snapshot ID and an encryption value. This is because only blank volumes can be encrypted on creation. If a snapshot is the basis for a volume, it is not blank and its encryption status is used for the volume encryption status.
--
-- /Note:/ Consider using 'blockDeviceMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsBlockDeviceMappings :: Lens.Lens' SpotFleetLaunchSpecification (Lude.Maybe [BlockDeviceMapping])
sflsBlockDeviceMappings = Lens.lens (blockDeviceMappings :: SpotFleetLaunchSpecification -> Lude.Maybe [BlockDeviceMapping]) (\s a -> s {blockDeviceMappings = a} :: SpotFleetLaunchSpecification)
{-# DEPRECATED sflsBlockDeviceMappings "Use generic-lens or generic-optics with 'blockDeviceMappings' instead." #-}

-- | The placement information.
--
-- /Note:/ Consider using 'placement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sflsPlacement :: Lens.Lens' SpotFleetLaunchSpecification (Lude.Maybe SpotPlacement)
sflsPlacement = Lens.lens (placement :: SpotFleetLaunchSpecification -> Lude.Maybe SpotPlacement) (\s a -> s {placement = a} :: SpotFleetLaunchSpecification)
{-# DEPRECATED sflsPlacement "Use generic-lens or generic-optics with 'placement' instead." #-}

instance Lude.FromXML SpotFleetLaunchSpecification where
  parseXML x =
    SpotFleetLaunchSpecification'
      Lude.<$> ( x Lude..@? "groupSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "spotPrice")
      Lude.<*> (x Lude..@? "weightedCapacity")
      Lude.<*> (x Lude..@? "keyName")
      Lude.<*> ( x Lude..@? "networkInterfaceSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "ramdiskId")
      Lude.<*> (x Lude..@? "subnetId")
      Lude.<*> (x Lude..@? "kernelId")
      Lude.<*> (x Lude..@? "instanceType")
      Lude.<*> (x Lude..@? "ebsOptimized")
      Lude.<*> (x Lude..@? "userData")
      Lude.<*> (x Lude..@? "monitoring")
      Lude.<*> ( x Lude..@? "tagSpecificationSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "iamInstanceProfile")
      Lude.<*> (x Lude..@? "imageId")
      Lude.<*> (x Lude..@? "addressingType")
      Lude.<*> ( x Lude..@? "blockDeviceMapping" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "placement")

instance Lude.ToQuery SpotFleetLaunchSpecification where
  toQuery SpotFleetLaunchSpecification' {..} =
    Lude.mconcat
      [ Lude.toQuery
          (Lude.toQueryList "GroupSet" Lude.<$> securityGroups),
        "SpotPrice" Lude.=: spotPrice,
        "WeightedCapacity" Lude.=: weightedCapacity,
        "KeyName" Lude.=: keyName,
        Lude.toQuery
          ( Lude.toQueryList "NetworkInterfaceSet"
              Lude.<$> networkInterfaces
          ),
        "RamdiskId" Lude.=: ramdiskId,
        "SubnetId" Lude.=: subnetId,
        "KernelId" Lude.=: kernelId,
        "InstanceType" Lude.=: instanceType,
        "EbsOptimized" Lude.=: ebsOptimized,
        "UserData" Lude.=: userData,
        "Monitoring" Lude.=: monitoring,
        Lude.toQuery
          ( Lude.toQueryList "TagSpecificationSet"
              Lude.<$> tagSpecifications
          ),
        "IamInstanceProfile" Lude.=: iamInstanceProfile,
        "ImageId" Lude.=: imageId,
        "AddressingType" Lude.=: addressingType,
        Lude.toQuery
          ( Lude.toQueryList "BlockDeviceMapping"
              Lude.<$> blockDeviceMappings
          ),
        "Placement" Lude.=: placement
      ]
