{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.CreateLaunchConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a launch configuration.
--
-- If you exceed your maximum limit of launch configurations, the call fails. To query this limit, call the 'DescribeAccountLimits' API. For information about updating this limit, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-account-limits.html Amazon EC2 Auto Scaling service quotas> in the /Amazon EC2 Auto Scaling User Guide/ .
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/LaunchConfiguration.html Launch configurations> in the /Amazon EC2 Auto Scaling User Guide/ .
module Network.AWS.AutoScaling.CreateLaunchConfiguration
    (
    -- * Creating a request
      CreateLaunchConfiguration (..)
    , mkCreateLaunchConfiguration
    -- ** Request lenses
    , clcLaunchConfigurationName
    , clcAssociatePublicIpAddress
    , clcBlockDeviceMappings
    , clcClassicLinkVPCId
    , clcClassicLinkVPCSecurityGroups
    , clcEbsOptimized
    , clcIamInstanceProfile
    , clcImageId
    , clcInstanceId
    , clcInstanceMonitoring
    , clcInstanceType
    , clcKernelId
    , clcKeyName
    , clcMetadataOptions
    , clcPlacementTenancy
    , clcRamdiskId
    , clcSecurityGroups
    , clcSpotPrice
    , clcUserData

    -- * Destructuring the response
    , CreateLaunchConfigurationResponse (..)
    , mkCreateLaunchConfigurationResponse
    ) where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateLaunchConfiguration' smart constructor.
data CreateLaunchConfiguration = CreateLaunchConfiguration'
  { launchConfigurationName :: Types.XmlStringMaxLen255
    -- ^ The name of the launch configuration. This name must be unique per Region per account.
  , associatePublicIpAddress :: Core.Maybe Core.Bool
    -- ^ For Auto Scaling groups that are running in a virtual private cloud (VPC), specifies whether to assign a public IP address to the group's instances. If you specify @true@ , each instance in the Auto Scaling group receives a unique public IP address. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html Launching Auto Scaling instances in a VPC> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- If you specify this parameter, you must specify at least one subnet for @VPCZoneIdentifier@ when you create your group.
  , blockDeviceMappings :: Core.Maybe [Types.BlockDeviceMapping]
    -- ^ A block device mapping, which specifies the block devices for the instance. You can specify virtual devices and EBS volumes. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html Block Device Mapping> in the /Amazon EC2 User Guide for Linux Instances/ .
  , classicLinkVPCId :: Core.Maybe Types.XmlStringMaxLen255
    -- ^ The ID of a ClassicLink-enabled VPC to link your EC2-Classic instances to. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink> in the /Amazon EC2 User Guide for Linux Instances/ and <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html#as-ClassicLink Linking EC2-Classic instances to a VPC> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- This parameter can only be used if you are launching EC2-Classic instances.
  , classicLinkVPCSecurityGroups :: Core.Maybe [Types.XmlStringMaxLen255]
    -- ^ The IDs of one or more security groups for the specified ClassicLink-enabled VPC. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink> in the /Amazon EC2 User Guide for Linux Instances/ and <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html#as-ClassicLink Linking EC2-Classic instances to a VPC> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- If you specify the @ClassicLinkVPCId@ parameter, you must specify this parameter.
  , ebsOptimized :: Core.Maybe Core.Bool
    -- ^ Specifies whether the launch configuration is optimized for EBS I/O (@true@ ) or not (@false@ ). The optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal I/O performance. This optimization is not available with all instance types. Additional fees are incurred when you enable EBS optimization for an instance type that is not EBS-optimized by default. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSOptimized.html Amazon EBS-Optimized Instances> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- The default value is @false@ .
  , iamInstanceProfile :: Core.Maybe Types.XmlStringMaxLen1600
    -- ^ The name or the Amazon Resource Name (ARN) of the instance profile associated with the IAM role for the instance. The instance profile contains the IAM role.
--
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/us-iam-role.html IAM role for applications that run on Amazon EC2 instances> in the /Amazon EC2 Auto Scaling User Guide/ .
  , imageId :: Core.Maybe Types.XmlStringMaxLen255
    -- ^ The ID of the Amazon Machine Image (AMI) that was assigned during registration. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/finding-an-ami.html Finding an AMI> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- If you do not specify @InstanceId@ , you must specify @ImageId@ .
  , instanceId :: Core.Maybe Types.InstanceId
    -- ^ The ID of the instance to use to create the launch configuration. The new launch configuration derives attributes from the instance, except for the block device mapping.
--
-- To create a launch configuration with a block device mapping or override any other instance attributes, specify them as part of the same request.
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-lc-with-instanceID.html Creating a launch configuration using an EC2 instance> in the /Amazon EC2 Auto Scaling User Guide/ .
-- If you do not specify @InstanceId@ , you must specify both @ImageId@ and @InstanceType@ .
  , instanceMonitoring :: Core.Maybe Types.InstanceMonitoring
    -- ^ Controls whether instances in this group are launched with detailed (@true@ ) or basic (@false@ ) monitoring.
--
-- The default value is @true@ (enabled).
-- /Important:/ When detailed monitoring is enabled, Amazon CloudWatch generates metrics every minute and your account is charged a fee. When you disable detailed monitoring, CloudWatch generates metrics every 5 minutes. For more information, see <https://docs.aws.amazon.com/autoscaling/latest/userguide/enable-as-instance-metrics.html Configure Monitoring for Auto Scaling Instances> in the /Amazon EC2 Auto Scaling User Guide/ .
  , instanceType :: Core.Maybe Types.XmlStringMaxLen255
    -- ^ Specifies the instance type of the EC2 instance.
--
-- For information about available instance types, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#AvailableInstanceTypes Available Instance Types> in the /Amazon EC2 User Guide for Linux Instances./ 
-- If you do not specify @InstanceId@ , you must specify @InstanceType@ .
  , kernelId :: Core.Maybe Types.XmlStringMaxLen255
    -- ^ The ID of the kernel associated with the AMI.
  , keyName :: Core.Maybe Types.XmlStringMaxLen255
    -- ^ The name of the key pair. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Amazon EC2 Key Pairs> in the /Amazon EC2 User Guide for Linux Instances/ .
  , metadataOptions :: Core.Maybe Types.InstanceMetadataOptions
    -- ^ The metadata options for the instances. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-launch-config.html#launch-configurations-imds Configuring the Instance Metadata Options> in the /Amazon EC2 Auto Scaling User Guide/ .
  , placementTenancy :: Core.Maybe Types.XmlStringMaxLen64
    -- ^ The tenancy of the instance. An instance with @dedicated@ tenancy runs on isolated, single-tenant hardware and can only be launched into a VPC.
--
-- To launch dedicated instances into a shared tenancy VPC (a VPC with the instance placement tenancy attribute set to @default@ ), you must set the value of this parameter to @dedicated@ .
-- If you specify @PlacementTenancy@ , you must specify at least one subnet for @VPCZoneIdentifier@ when you create your group.
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/auto-scaling-dedicated-instances.html Configuring instance tenancy with Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
-- Valid Values: @default@ | @dedicated@ 
  , ramdiskId :: Core.Maybe Types.XmlStringMaxLen255
    -- ^ The ID of the RAM disk to select.
  , securityGroups :: Core.Maybe [Types.XmlString]
    -- ^ A list that contains the security groups to assign to the instances in the Auto Scaling group.
--
-- [EC2-VPC] Specify the security group IDs. For more information, see <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_SecurityGroups.html Security Groups for Your VPC> in the /Amazon Virtual Private Cloud User Guide/ .
-- [EC2-Classic] Specify either the security group names or the security group IDs. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-network-security.html Amazon EC2 Security Groups> in the /Amazon EC2 User Guide for Linux Instances/ .
  , spotPrice :: Core.Maybe Types.SpotPrice
    -- ^ The maximum hourly price to be paid for any Spot Instance launched to fulfill the request. Spot Instances are launched when the price you specify exceeds the current Spot price. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-launch-spot-instances.html Requesting Spot Instances> in the /Amazon EC2 Auto Scaling User Guide/ .
  , userData :: Core.Maybe Types.XmlStringUserData
    -- ^ The Base64-encoded user data to make available to the launched EC2 instances. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance metadata and user data> in the /Amazon EC2 User Guide for Linux Instances/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateLaunchConfiguration' value with any optional fields omitted.
mkCreateLaunchConfiguration
    :: Types.XmlStringMaxLen255 -- ^ 'launchConfigurationName'
    -> CreateLaunchConfiguration
mkCreateLaunchConfiguration launchConfigurationName
  = CreateLaunchConfiguration'{launchConfigurationName,
                               associatePublicIpAddress = Core.Nothing,
                               blockDeviceMappings = Core.Nothing,
                               classicLinkVPCId = Core.Nothing,
                               classicLinkVPCSecurityGroups = Core.Nothing,
                               ebsOptimized = Core.Nothing, iamInstanceProfile = Core.Nothing,
                               imageId = Core.Nothing, instanceId = Core.Nothing,
                               instanceMonitoring = Core.Nothing, instanceType = Core.Nothing,
                               kernelId = Core.Nothing, keyName = Core.Nothing,
                               metadataOptions = Core.Nothing, placementTenancy = Core.Nothing,
                               ramdiskId = Core.Nothing, securityGroups = Core.Nothing,
                               spotPrice = Core.Nothing, userData = Core.Nothing}

-- | The name of the launch configuration. This name must be unique per Region per account.
--
-- /Note:/ Consider using 'launchConfigurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcLaunchConfigurationName :: Lens.Lens' CreateLaunchConfiguration Types.XmlStringMaxLen255
clcLaunchConfigurationName = Lens.field @"launchConfigurationName"
{-# INLINEABLE clcLaunchConfigurationName #-}
{-# DEPRECATED launchConfigurationName "Use generic-lens or generic-optics with 'launchConfigurationName' instead"  #-}

-- | For Auto Scaling groups that are running in a virtual private cloud (VPC), specifies whether to assign a public IP address to the group's instances. If you specify @true@ , each instance in the Auto Scaling group receives a unique public IP address. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html Launching Auto Scaling instances in a VPC> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- If you specify this parameter, you must specify at least one subnet for @VPCZoneIdentifier@ when you create your group.
--
-- /Note:/ Consider using 'associatePublicIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcAssociatePublicIpAddress :: Lens.Lens' CreateLaunchConfiguration (Core.Maybe Core.Bool)
clcAssociatePublicIpAddress = Lens.field @"associatePublicIpAddress"
{-# INLINEABLE clcAssociatePublicIpAddress #-}
{-# DEPRECATED associatePublicIpAddress "Use generic-lens or generic-optics with 'associatePublicIpAddress' instead"  #-}

-- | A block device mapping, which specifies the block devices for the instance. You can specify virtual devices and EBS volumes. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html Block Device Mapping> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- /Note:/ Consider using 'blockDeviceMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcBlockDeviceMappings :: Lens.Lens' CreateLaunchConfiguration (Core.Maybe [Types.BlockDeviceMapping])
clcBlockDeviceMappings = Lens.field @"blockDeviceMappings"
{-# INLINEABLE clcBlockDeviceMappings #-}
{-# DEPRECATED blockDeviceMappings "Use generic-lens or generic-optics with 'blockDeviceMappings' instead"  #-}

-- | The ID of a ClassicLink-enabled VPC to link your EC2-Classic instances to. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink> in the /Amazon EC2 User Guide for Linux Instances/ and <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html#as-ClassicLink Linking EC2-Classic instances to a VPC> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- This parameter can only be used if you are launching EC2-Classic instances.
--
-- /Note:/ Consider using 'classicLinkVPCId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcClassicLinkVPCId :: Lens.Lens' CreateLaunchConfiguration (Core.Maybe Types.XmlStringMaxLen255)
clcClassicLinkVPCId = Lens.field @"classicLinkVPCId"
{-# INLINEABLE clcClassicLinkVPCId #-}
{-# DEPRECATED classicLinkVPCId "Use generic-lens or generic-optics with 'classicLinkVPCId' instead"  #-}

-- | The IDs of one or more security groups for the specified ClassicLink-enabled VPC. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink> in the /Amazon EC2 User Guide for Linux Instances/ and <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html#as-ClassicLink Linking EC2-Classic instances to a VPC> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- If you specify the @ClassicLinkVPCId@ parameter, you must specify this parameter.
--
-- /Note:/ Consider using 'classicLinkVPCSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcClassicLinkVPCSecurityGroups :: Lens.Lens' CreateLaunchConfiguration (Core.Maybe [Types.XmlStringMaxLen255])
clcClassicLinkVPCSecurityGroups = Lens.field @"classicLinkVPCSecurityGroups"
{-# INLINEABLE clcClassicLinkVPCSecurityGroups #-}
{-# DEPRECATED classicLinkVPCSecurityGroups "Use generic-lens or generic-optics with 'classicLinkVPCSecurityGroups' instead"  #-}

-- | Specifies whether the launch configuration is optimized for EBS I/O (@true@ ) or not (@false@ ). The optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal I/O performance. This optimization is not available with all instance types. Additional fees are incurred when you enable EBS optimization for an instance type that is not EBS-optimized by default. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSOptimized.html Amazon EBS-Optimized Instances> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- The default value is @false@ .
--
-- /Note:/ Consider using 'ebsOptimized' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcEbsOptimized :: Lens.Lens' CreateLaunchConfiguration (Core.Maybe Core.Bool)
clcEbsOptimized = Lens.field @"ebsOptimized"
{-# INLINEABLE clcEbsOptimized #-}
{-# DEPRECATED ebsOptimized "Use generic-lens or generic-optics with 'ebsOptimized' instead"  #-}

-- | The name or the Amazon Resource Name (ARN) of the instance profile associated with the IAM role for the instance. The instance profile contains the IAM role.
--
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/us-iam-role.html IAM role for applications that run on Amazon EC2 instances> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'iamInstanceProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcIamInstanceProfile :: Lens.Lens' CreateLaunchConfiguration (Core.Maybe Types.XmlStringMaxLen1600)
clcIamInstanceProfile = Lens.field @"iamInstanceProfile"
{-# INLINEABLE clcIamInstanceProfile #-}
{-# DEPRECATED iamInstanceProfile "Use generic-lens or generic-optics with 'iamInstanceProfile' instead"  #-}

-- | The ID of the Amazon Machine Image (AMI) that was assigned during registration. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/finding-an-ami.html Finding an AMI> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- If you do not specify @InstanceId@ , you must specify @ImageId@ .
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcImageId :: Lens.Lens' CreateLaunchConfiguration (Core.Maybe Types.XmlStringMaxLen255)
clcImageId = Lens.field @"imageId"
{-# INLINEABLE clcImageId #-}
{-# DEPRECATED imageId "Use generic-lens or generic-optics with 'imageId' instead"  #-}

-- | The ID of the instance to use to create the launch configuration. The new launch configuration derives attributes from the instance, except for the block device mapping.
--
-- To create a launch configuration with a block device mapping or override any other instance attributes, specify them as part of the same request.
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-lc-with-instanceID.html Creating a launch configuration using an EC2 instance> in the /Amazon EC2 Auto Scaling User Guide/ .
-- If you do not specify @InstanceId@ , you must specify both @ImageId@ and @InstanceType@ .
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcInstanceId :: Lens.Lens' CreateLaunchConfiguration (Core.Maybe Types.InstanceId)
clcInstanceId = Lens.field @"instanceId"
{-# INLINEABLE clcInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | Controls whether instances in this group are launched with detailed (@true@ ) or basic (@false@ ) monitoring.
--
-- The default value is @true@ (enabled).
-- /Important:/ When detailed monitoring is enabled, Amazon CloudWatch generates metrics every minute and your account is charged a fee. When you disable detailed monitoring, CloudWatch generates metrics every 5 minutes. For more information, see <https://docs.aws.amazon.com/autoscaling/latest/userguide/enable-as-instance-metrics.html Configure Monitoring for Auto Scaling Instances> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'instanceMonitoring' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcInstanceMonitoring :: Lens.Lens' CreateLaunchConfiguration (Core.Maybe Types.InstanceMonitoring)
clcInstanceMonitoring = Lens.field @"instanceMonitoring"
{-# INLINEABLE clcInstanceMonitoring #-}
{-# DEPRECATED instanceMonitoring "Use generic-lens or generic-optics with 'instanceMonitoring' instead"  #-}

-- | Specifies the instance type of the EC2 instance.
--
-- For information about available instance types, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#AvailableInstanceTypes Available Instance Types> in the /Amazon EC2 User Guide for Linux Instances./ 
-- If you do not specify @InstanceId@ , you must specify @InstanceType@ .
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcInstanceType :: Lens.Lens' CreateLaunchConfiguration (Core.Maybe Types.XmlStringMaxLen255)
clcInstanceType = Lens.field @"instanceType"
{-# INLINEABLE clcInstanceType #-}
{-# DEPRECATED instanceType "Use generic-lens or generic-optics with 'instanceType' instead"  #-}

-- | The ID of the kernel associated with the AMI.
--
-- /Note:/ Consider using 'kernelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcKernelId :: Lens.Lens' CreateLaunchConfiguration (Core.Maybe Types.XmlStringMaxLen255)
clcKernelId = Lens.field @"kernelId"
{-# INLINEABLE clcKernelId #-}
{-# DEPRECATED kernelId "Use generic-lens or generic-optics with 'kernelId' instead"  #-}

-- | The name of the key pair. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Amazon EC2 Key Pairs> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- /Note:/ Consider using 'keyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcKeyName :: Lens.Lens' CreateLaunchConfiguration (Core.Maybe Types.XmlStringMaxLen255)
clcKeyName = Lens.field @"keyName"
{-# INLINEABLE clcKeyName #-}
{-# DEPRECATED keyName "Use generic-lens or generic-optics with 'keyName' instead"  #-}

-- | The metadata options for the instances. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-launch-config.html#launch-configurations-imds Configuring the Instance Metadata Options> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'metadataOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcMetadataOptions :: Lens.Lens' CreateLaunchConfiguration (Core.Maybe Types.InstanceMetadataOptions)
clcMetadataOptions = Lens.field @"metadataOptions"
{-# INLINEABLE clcMetadataOptions #-}
{-# DEPRECATED metadataOptions "Use generic-lens or generic-optics with 'metadataOptions' instead"  #-}

-- | The tenancy of the instance. An instance with @dedicated@ tenancy runs on isolated, single-tenant hardware and can only be launched into a VPC.
--
-- To launch dedicated instances into a shared tenancy VPC (a VPC with the instance placement tenancy attribute set to @default@ ), you must set the value of this parameter to @dedicated@ .
-- If you specify @PlacementTenancy@ , you must specify at least one subnet for @VPCZoneIdentifier@ when you create your group.
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/auto-scaling-dedicated-instances.html Configuring instance tenancy with Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
-- Valid Values: @default@ | @dedicated@ 
--
-- /Note:/ Consider using 'placementTenancy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcPlacementTenancy :: Lens.Lens' CreateLaunchConfiguration (Core.Maybe Types.XmlStringMaxLen64)
clcPlacementTenancy = Lens.field @"placementTenancy"
{-# INLINEABLE clcPlacementTenancy #-}
{-# DEPRECATED placementTenancy "Use generic-lens or generic-optics with 'placementTenancy' instead"  #-}

-- | The ID of the RAM disk to select.
--
-- /Note:/ Consider using 'ramdiskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcRamdiskId :: Lens.Lens' CreateLaunchConfiguration (Core.Maybe Types.XmlStringMaxLen255)
clcRamdiskId = Lens.field @"ramdiskId"
{-# INLINEABLE clcRamdiskId #-}
{-# DEPRECATED ramdiskId "Use generic-lens or generic-optics with 'ramdiskId' instead"  #-}

-- | A list that contains the security groups to assign to the instances in the Auto Scaling group.
--
-- [EC2-VPC] Specify the security group IDs. For more information, see <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_SecurityGroups.html Security Groups for Your VPC> in the /Amazon Virtual Private Cloud User Guide/ .
-- [EC2-Classic] Specify either the security group names or the security group IDs. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-network-security.html Amazon EC2 Security Groups> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcSecurityGroups :: Lens.Lens' CreateLaunchConfiguration (Core.Maybe [Types.XmlString])
clcSecurityGroups = Lens.field @"securityGroups"
{-# INLINEABLE clcSecurityGroups #-}
{-# DEPRECATED securityGroups "Use generic-lens or generic-optics with 'securityGroups' instead"  #-}

-- | The maximum hourly price to be paid for any Spot Instance launched to fulfill the request. Spot Instances are launched when the price you specify exceeds the current Spot price. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-launch-spot-instances.html Requesting Spot Instances> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'spotPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcSpotPrice :: Lens.Lens' CreateLaunchConfiguration (Core.Maybe Types.SpotPrice)
clcSpotPrice = Lens.field @"spotPrice"
{-# INLINEABLE clcSpotPrice #-}
{-# DEPRECATED spotPrice "Use generic-lens or generic-optics with 'spotPrice' instead"  #-}

-- | The Base64-encoded user data to make available to the launched EC2 instances. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance metadata and user data> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- /Note:/ Consider using 'userData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcUserData :: Lens.Lens' CreateLaunchConfiguration (Core.Maybe Types.XmlStringUserData)
clcUserData = Lens.field @"userData"
{-# INLINEABLE clcUserData #-}
{-# DEPRECATED userData "Use generic-lens or generic-optics with 'userData' instead"  #-}

instance Core.ToQuery CreateLaunchConfiguration where
        toQuery CreateLaunchConfiguration{..}
          = Core.toQueryPair "Action"
              ("CreateLaunchConfiguration" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2011-01-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "LaunchConfigurationName" launchConfigurationName
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "AssociatePublicIpAddress")
                associatePublicIpAddress
              Core.<>
              Core.toQueryPair "BlockDeviceMappings"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   blockDeviceMappings)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClassicLinkVPCId")
                classicLinkVPCId
              Core.<>
              Core.toQueryPair "ClassicLinkVPCSecurityGroups"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   classicLinkVPCSecurityGroups)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EbsOptimized")
                ebsOptimized
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "IamInstanceProfile")
                iamInstanceProfile
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "ImageId") imageId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "InstanceId") instanceId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "InstanceMonitoring")
                instanceMonitoring
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "InstanceType")
                instanceType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "KernelId") kernelId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "KeyName") keyName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MetadataOptions")
                metadataOptions
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PlacementTenancy")
                placementTenancy
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "RamdiskId") ramdiskId
              Core.<>
              Core.toQueryPair "SecurityGroups"
                (Core.maybe Core.mempty (Core.toQueryList "member") securityGroups)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SpotPrice") spotPrice
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "UserData") userData

instance Core.ToHeaders CreateLaunchConfiguration where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateLaunchConfiguration where
        type Rs CreateLaunchConfiguration =
             CreateLaunchConfigurationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull CreateLaunchConfigurationResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateLaunchConfigurationResponse' smart constructor.
data CreateLaunchConfigurationResponse = CreateLaunchConfigurationResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateLaunchConfigurationResponse' value with any optional fields omitted.
mkCreateLaunchConfigurationResponse
    :: CreateLaunchConfigurationResponse
mkCreateLaunchConfigurationResponse
  = CreateLaunchConfigurationResponse'
