{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.RequestSpotLaunchSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.RequestSpotLaunchSpecification
  ( RequestSpotLaunchSpecification (..)
  -- * Smart constructor
  , mkRequestSpotLaunchSpecification
  -- * Lenses
  , rslsAddressingType
  , rslsBlockDeviceMappings
  , rslsEbsOptimized
  , rslsIamInstanceProfile
  , rslsImageId
  , rslsInstanceType
  , rslsKernelId
  , rslsKeyName
  , rslsMonitoring
  , rslsNetworkInterfaces
  , rslsPlacement
  , rslsRamdiskId
  , rslsSecurityGroupIds
  , rslsSecurityGroups
  , rslsSubnetId
  , rslsUserData
  ) where

import qualified Network.AWS.EC2.Types.BlockDeviceMapping as Types
import qualified Network.AWS.EC2.Types.IamInstanceProfileSpecification as Types
import qualified Network.AWS.EC2.Types.ImageId as Types
import qualified Network.AWS.EC2.Types.InstanceNetworkInterfaceSpecification as Types
import qualified Network.AWS.EC2.Types.InstanceType as Types
import qualified Network.AWS.EC2.Types.KernelId as Types
import qualified Network.AWS.EC2.Types.KeyPairName as Types
import qualified Network.AWS.EC2.Types.RamdiskId as Types
import qualified Network.AWS.EC2.Types.RunInstancesMonitoringEnabled as Types
import qualified Network.AWS.EC2.Types.SecurityGroupId as Types
import qualified Network.AWS.EC2.Types.SecurityGroupName as Types
import qualified Network.AWS.EC2.Types.SpotPlacement as Types
import qualified Network.AWS.EC2.Types.SubnetId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the launch specification for an instance.
--
-- /See:/ 'mkRequestSpotLaunchSpecification' smart constructor.
data RequestSpotLaunchSpecification = RequestSpotLaunchSpecification'
  { addressingType :: Core.Maybe Core.Text
    -- ^ Deprecated.
  , blockDeviceMappings :: Core.Maybe [Types.BlockDeviceMapping]
    -- ^ One or more block device mapping entries. You can't specify both a snapshot ID and an encryption value. This is because only blank volumes can be encrypted on creation. If a snapshot is the basis for a volume, it is not blank and its encryption status is used for the volume encryption status.
  , ebsOptimized :: Core.Maybe Core.Bool
    -- ^ Indicates whether the instance is optimized for EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS Optimized instance.
--
-- Default: @false@ 
  , iamInstanceProfile :: Core.Maybe Types.IamInstanceProfileSpecification
    -- ^ The IAM instance profile.
  , imageId :: Core.Maybe Types.ImageId
    -- ^ The ID of the AMI.
  , instanceType :: Core.Maybe Types.InstanceType
    -- ^ The instance type.
  , kernelId :: Core.Maybe Types.KernelId
    -- ^ The ID of the kernel.
  , keyName :: Core.Maybe Types.KeyPairName
    -- ^ The name of the key pair.
  , monitoring :: Core.Maybe Types.RunInstancesMonitoringEnabled
    -- ^ Indicates whether basic or detailed monitoring is enabled for the instance.
--
-- Default: Disabled
  , networkInterfaces :: Core.Maybe [Types.InstanceNetworkInterfaceSpecification]
    -- ^ One or more network interfaces. If you specify a network interface, you must specify subnet IDs and security group IDs using the network interface.
  , placement :: Core.Maybe Types.SpotPlacement
    -- ^ The placement information for the instance.
  , ramdiskId :: Core.Maybe Types.RamdiskId
    -- ^ The ID of the RAM disk.
  , securityGroupIds :: Core.Maybe [Types.SecurityGroupId]
    -- ^ One or more security group IDs.
  , securityGroups :: Core.Maybe [Types.SecurityGroupName]
    -- ^ One or more security groups. When requesting instances in a VPC, you must specify the IDs of the security groups. When requesting instances in EC2-Classic, you can specify the names or the IDs of the security groups.
  , subnetId :: Core.Maybe Types.SubnetId
    -- ^ The IDs of the subnets in which to launch the instance. To specify multiple subnets, separate them using commas; for example, "subnet-1234abcdeexample1, subnet-0987cdef6example2".
  , userData :: Core.Maybe Core.Text
    -- ^ The Base64-encoded user data for the instance. User data is limited to 16 KB.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RequestSpotLaunchSpecification' value with any optional fields omitted.
mkRequestSpotLaunchSpecification
    :: RequestSpotLaunchSpecification
mkRequestSpotLaunchSpecification
  = RequestSpotLaunchSpecification'{addressingType = Core.Nothing,
                                    blockDeviceMappings = Core.Nothing, ebsOptimized = Core.Nothing,
                                    iamInstanceProfile = Core.Nothing, imageId = Core.Nothing,
                                    instanceType = Core.Nothing, kernelId = Core.Nothing,
                                    keyName = Core.Nothing, monitoring = Core.Nothing,
                                    networkInterfaces = Core.Nothing, placement = Core.Nothing,
                                    ramdiskId = Core.Nothing, securityGroupIds = Core.Nothing,
                                    securityGroups = Core.Nothing, subnetId = Core.Nothing,
                                    userData = Core.Nothing}

-- | Deprecated.
--
-- /Note:/ Consider using 'addressingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rslsAddressingType :: Lens.Lens' RequestSpotLaunchSpecification (Core.Maybe Core.Text)
rslsAddressingType = Lens.field @"addressingType"
{-# INLINEABLE rslsAddressingType #-}
{-# DEPRECATED addressingType "Use generic-lens or generic-optics with 'addressingType' instead"  #-}

-- | One or more block device mapping entries. You can't specify both a snapshot ID and an encryption value. This is because only blank volumes can be encrypted on creation. If a snapshot is the basis for a volume, it is not blank and its encryption status is used for the volume encryption status.
--
-- /Note:/ Consider using 'blockDeviceMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rslsBlockDeviceMappings :: Lens.Lens' RequestSpotLaunchSpecification (Core.Maybe [Types.BlockDeviceMapping])
rslsBlockDeviceMappings = Lens.field @"blockDeviceMappings"
{-# INLINEABLE rslsBlockDeviceMappings #-}
{-# DEPRECATED blockDeviceMappings "Use generic-lens or generic-optics with 'blockDeviceMappings' instead"  #-}

-- | Indicates whether the instance is optimized for EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS Optimized instance.
--
-- Default: @false@ 
--
-- /Note:/ Consider using 'ebsOptimized' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rslsEbsOptimized :: Lens.Lens' RequestSpotLaunchSpecification (Core.Maybe Core.Bool)
rslsEbsOptimized = Lens.field @"ebsOptimized"
{-# INLINEABLE rslsEbsOptimized #-}
{-# DEPRECATED ebsOptimized "Use generic-lens or generic-optics with 'ebsOptimized' instead"  #-}

-- | The IAM instance profile.
--
-- /Note:/ Consider using 'iamInstanceProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rslsIamInstanceProfile :: Lens.Lens' RequestSpotLaunchSpecification (Core.Maybe Types.IamInstanceProfileSpecification)
rslsIamInstanceProfile = Lens.field @"iamInstanceProfile"
{-# INLINEABLE rslsIamInstanceProfile #-}
{-# DEPRECATED iamInstanceProfile "Use generic-lens or generic-optics with 'iamInstanceProfile' instead"  #-}

-- | The ID of the AMI.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rslsImageId :: Lens.Lens' RequestSpotLaunchSpecification (Core.Maybe Types.ImageId)
rslsImageId = Lens.field @"imageId"
{-# INLINEABLE rslsImageId #-}
{-# DEPRECATED imageId "Use generic-lens or generic-optics with 'imageId' instead"  #-}

-- | The instance type.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rslsInstanceType :: Lens.Lens' RequestSpotLaunchSpecification (Core.Maybe Types.InstanceType)
rslsInstanceType = Lens.field @"instanceType"
{-# INLINEABLE rslsInstanceType #-}
{-# DEPRECATED instanceType "Use generic-lens or generic-optics with 'instanceType' instead"  #-}

-- | The ID of the kernel.
--
-- /Note:/ Consider using 'kernelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rslsKernelId :: Lens.Lens' RequestSpotLaunchSpecification (Core.Maybe Types.KernelId)
rslsKernelId = Lens.field @"kernelId"
{-# INLINEABLE rslsKernelId #-}
{-# DEPRECATED kernelId "Use generic-lens or generic-optics with 'kernelId' instead"  #-}

-- | The name of the key pair.
--
-- /Note:/ Consider using 'keyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rslsKeyName :: Lens.Lens' RequestSpotLaunchSpecification (Core.Maybe Types.KeyPairName)
rslsKeyName = Lens.field @"keyName"
{-# INLINEABLE rslsKeyName #-}
{-# DEPRECATED keyName "Use generic-lens or generic-optics with 'keyName' instead"  #-}

-- | Indicates whether basic or detailed monitoring is enabled for the instance.
--
-- Default: Disabled
--
-- /Note:/ Consider using 'monitoring' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rslsMonitoring :: Lens.Lens' RequestSpotLaunchSpecification (Core.Maybe Types.RunInstancesMonitoringEnabled)
rslsMonitoring = Lens.field @"monitoring"
{-# INLINEABLE rslsMonitoring #-}
{-# DEPRECATED monitoring "Use generic-lens or generic-optics with 'monitoring' instead"  #-}

-- | One or more network interfaces. If you specify a network interface, you must specify subnet IDs and security group IDs using the network interface.
--
-- /Note:/ Consider using 'networkInterfaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rslsNetworkInterfaces :: Lens.Lens' RequestSpotLaunchSpecification (Core.Maybe [Types.InstanceNetworkInterfaceSpecification])
rslsNetworkInterfaces = Lens.field @"networkInterfaces"
{-# INLINEABLE rslsNetworkInterfaces #-}
{-# DEPRECATED networkInterfaces "Use generic-lens or generic-optics with 'networkInterfaces' instead"  #-}

-- | The placement information for the instance.
--
-- /Note:/ Consider using 'placement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rslsPlacement :: Lens.Lens' RequestSpotLaunchSpecification (Core.Maybe Types.SpotPlacement)
rslsPlacement = Lens.field @"placement"
{-# INLINEABLE rslsPlacement #-}
{-# DEPRECATED placement "Use generic-lens or generic-optics with 'placement' instead"  #-}

-- | The ID of the RAM disk.
--
-- /Note:/ Consider using 'ramdiskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rslsRamdiskId :: Lens.Lens' RequestSpotLaunchSpecification (Core.Maybe Types.RamdiskId)
rslsRamdiskId = Lens.field @"ramdiskId"
{-# INLINEABLE rslsRamdiskId #-}
{-# DEPRECATED ramdiskId "Use generic-lens or generic-optics with 'ramdiskId' instead"  #-}

-- | One or more security group IDs.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rslsSecurityGroupIds :: Lens.Lens' RequestSpotLaunchSpecification (Core.Maybe [Types.SecurityGroupId])
rslsSecurityGroupIds = Lens.field @"securityGroupIds"
{-# INLINEABLE rslsSecurityGroupIds #-}
{-# DEPRECATED securityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead"  #-}

-- | One or more security groups. When requesting instances in a VPC, you must specify the IDs of the security groups. When requesting instances in EC2-Classic, you can specify the names or the IDs of the security groups.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rslsSecurityGroups :: Lens.Lens' RequestSpotLaunchSpecification (Core.Maybe [Types.SecurityGroupName])
rslsSecurityGroups = Lens.field @"securityGroups"
{-# INLINEABLE rslsSecurityGroups #-}
{-# DEPRECATED securityGroups "Use generic-lens or generic-optics with 'securityGroups' instead"  #-}

-- | The IDs of the subnets in which to launch the instance. To specify multiple subnets, separate them using commas; for example, "subnet-1234abcdeexample1, subnet-0987cdef6example2".
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rslsSubnetId :: Lens.Lens' RequestSpotLaunchSpecification (Core.Maybe Types.SubnetId)
rslsSubnetId = Lens.field @"subnetId"
{-# INLINEABLE rslsSubnetId #-}
{-# DEPRECATED subnetId "Use generic-lens or generic-optics with 'subnetId' instead"  #-}

-- | The Base64-encoded user data for the instance. User data is limited to 16 KB.
--
-- /Note:/ Consider using 'userData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rslsUserData :: Lens.Lens' RequestSpotLaunchSpecification (Core.Maybe Core.Text)
rslsUserData = Lens.field @"userData"
{-# INLINEABLE rslsUserData #-}
{-# DEPRECATED userData "Use generic-lens or generic-optics with 'userData' instead"  #-}

instance Core.ToQuery RequestSpotLaunchSpecification where
        toQuery RequestSpotLaunchSpecification{..}
          = Core.maybe Core.mempty (Core.toQueryPair "AddressingType")
              addressingType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "BlockDeviceMapping")
                blockDeviceMappings
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EbsOptimized")
                ebsOptimized
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "IamInstanceProfile")
                iamInstanceProfile
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "ImageId") imageId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "InstanceType")
                instanceType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "KernelId") kernelId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "KeyName") keyName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Monitoring") monitoring
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "NetworkInterface")
                networkInterfaces
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Placement") placement
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "RamdiskId") ramdiskId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "SecurityGroupId")
                securityGroupIds
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "SecurityGroup")
                securityGroups
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SubnetId") subnetId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "UserData") userData
