{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateLaunchConfiguration (..),
    mkCreateLaunchConfiguration,

    -- ** Request lenses
    clcLaunchConfigurationName,
    clcAssociatePublicIpAddress,
    clcBlockDeviceMappings,
    clcClassicLinkVPCId,
    clcClassicLinkVPCSecurityGroups,
    clcEbsOptimized,
    clcIamInstanceProfile,
    clcImageId,
    clcInstanceId,
    clcInstanceMonitoring,
    clcInstanceType,
    clcKernelId,
    clcKeyName,
    clcMetadataOptions,
    clcPlacementTenancy,
    clcRamdiskId,
    clcSecurityGroups,
    clcSpotPrice,
    clcUserData,

    -- * Destructuring the response
    CreateLaunchConfigurationResponse (..),
    mkCreateLaunchConfigurationResponse,
  )
where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateLaunchConfiguration' smart constructor.
data CreateLaunchConfiguration = CreateLaunchConfiguration'
  { -- | The name of the launch configuration. This name must be unique per Region per account.
    launchConfigurationName :: Types.XmlStringMaxLen255,
    -- | For Auto Scaling groups that are running in a virtual private cloud (VPC), specifies whether to assign a public IP address to the group's instances. If you specify @true@ , each instance in the Auto Scaling group receives a unique public IP address. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html Launching Auto Scaling instances in a VPC> in the /Amazon EC2 Auto Scaling User Guide/ .
    --
    -- If you specify this parameter, you must specify at least one subnet for @VPCZoneIdentifier@ when you create your group.
    associatePublicIpAddress :: Core.Maybe Core.Bool,
    -- | A block device mapping, which specifies the block devices for the instance. You can specify virtual devices and EBS volumes. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html Block Device Mapping> in the /Amazon EC2 User Guide for Linux Instances/ .
    blockDeviceMappings :: Core.Maybe [Types.BlockDeviceMapping],
    -- | The ID of a ClassicLink-enabled VPC to link your EC2-Classic instances to. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink> in the /Amazon EC2 User Guide for Linux Instances/ and <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html#as-ClassicLink Linking EC2-Classic instances to a VPC> in the /Amazon EC2 Auto Scaling User Guide/ .
    --
    -- This parameter can only be used if you are launching EC2-Classic instances.
    classicLinkVPCId :: Core.Maybe Types.XmlStringMaxLen255,
    -- | The IDs of one or more security groups for the specified ClassicLink-enabled VPC. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink> in the /Amazon EC2 User Guide for Linux Instances/ and <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html#as-ClassicLink Linking EC2-Classic instances to a VPC> in the /Amazon EC2 Auto Scaling User Guide/ .
    --
    -- If you specify the @ClassicLinkVPCId@ parameter, you must specify this parameter.
    classicLinkVPCSecurityGroups :: Core.Maybe [Types.XmlStringMaxLen255],
    -- | Specifies whether the launch configuration is optimized for EBS I/O (@true@ ) or not (@false@ ). The optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal I/O performance. This optimization is not available with all instance types. Additional fees are incurred when you enable EBS optimization for an instance type that is not EBS-optimized by default. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSOptimized.html Amazon EBS-Optimized Instances> in the /Amazon EC2 User Guide for Linux Instances/ .
    --
    -- The default value is @false@ .
    ebsOptimized :: Core.Maybe Core.Bool,
    -- | The name or the Amazon Resource Name (ARN) of the instance profile associated with the IAM role for the instance. The instance profile contains the IAM role.
    --
    -- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/us-iam-role.html IAM role for applications that run on Amazon EC2 instances> in the /Amazon EC2 Auto Scaling User Guide/ .
    iamInstanceProfile :: Core.Maybe Types.XmlStringMaxLen1600,
    -- | The ID of the Amazon Machine Image (AMI) that was assigned during registration. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/finding-an-ami.html Finding an AMI> in the /Amazon EC2 User Guide for Linux Instances/ .
    --
    -- If you do not specify @InstanceId@ , you must specify @ImageId@ .
    imageId :: Core.Maybe Types.XmlStringMaxLen255,
    -- | The ID of the instance to use to create the launch configuration. The new launch configuration derives attributes from the instance, except for the block device mapping.
    --
    -- To create a launch configuration with a block device mapping or override any other instance attributes, specify them as part of the same request.
    -- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-lc-with-instanceID.html Creating a launch configuration using an EC2 instance> in the /Amazon EC2 Auto Scaling User Guide/ .
    -- If you do not specify @InstanceId@ , you must specify both @ImageId@ and @InstanceType@ .
    instanceId :: Core.Maybe Types.InstanceId,
    -- | Controls whether instances in this group are launched with detailed (@true@ ) or basic (@false@ ) monitoring.
    --
    -- The default value is @true@ (enabled).
    -- /Important:/ When detailed monitoring is enabled, Amazon CloudWatch generates metrics every minute and your account is charged a fee. When you disable detailed monitoring, CloudWatch generates metrics every 5 minutes. For more information, see <https://docs.aws.amazon.com/autoscaling/latest/userguide/enable-as-instance-metrics.html Configure Monitoring for Auto Scaling Instances> in the /Amazon EC2 Auto Scaling User Guide/ .
    instanceMonitoring :: Core.Maybe Types.InstanceMonitoring,
    -- | Specifies the instance type of the EC2 instance.
    --
    -- For information about available instance types, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#AvailableInstanceTypes Available Instance Types> in the /Amazon EC2 User Guide for Linux Instances./
    -- If you do not specify @InstanceId@ , you must specify @InstanceType@ .
    instanceType :: Core.Maybe Types.XmlStringMaxLen255,
    -- | The ID of the kernel associated with the AMI.
    kernelId :: Core.Maybe Types.XmlStringMaxLen255,
    -- | The name of the key pair. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Amazon EC2 Key Pairs> in the /Amazon EC2 User Guide for Linux Instances/ .
    keyName :: Core.Maybe Types.XmlStringMaxLen255,
    -- | The metadata options for the instances. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-launch-config.html#launch-configurations-imds Configuring the Instance Metadata Options> in the /Amazon EC2 Auto Scaling User Guide/ .
    metadataOptions :: Core.Maybe Types.InstanceMetadataOptions,
    -- | The tenancy of the instance. An instance with @dedicated@ tenancy runs on isolated, single-tenant hardware and can only be launched into a VPC.
    --
    -- To launch dedicated instances into a shared tenancy VPC (a VPC with the instance placement tenancy attribute set to @default@ ), you must set the value of this parameter to @dedicated@ .
    -- If you specify @PlacementTenancy@ , you must specify at least one subnet for @VPCZoneIdentifier@ when you create your group.
    -- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/auto-scaling-dedicated-instances.html Configuring instance tenancy with Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
    -- Valid Values: @default@ | @dedicated@
    placementTenancy :: Core.Maybe Types.XmlStringMaxLen64,
    -- | The ID of the RAM disk to select.
    ramdiskId :: Core.Maybe Types.XmlStringMaxLen255,
    -- | A list that contains the security groups to assign to the instances in the Auto Scaling group.
    --
    -- [EC2-VPC] Specify the security group IDs. For more information, see <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_SecurityGroups.html Security Groups for Your VPC> in the /Amazon Virtual Private Cloud User Guide/ .
    -- [EC2-Classic] Specify either the security group names or the security group IDs. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-network-security.html Amazon EC2 Security Groups> in the /Amazon EC2 User Guide for Linux Instances/ .
    securityGroups :: Core.Maybe [Types.XmlString],
    -- | The maximum hourly price to be paid for any Spot Instance launched to fulfill the request. Spot Instances are launched when the price you specify exceeds the current Spot price. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-launch-spot-instances.html Requesting Spot Instances> in the /Amazon EC2 Auto Scaling User Guide/ .
    spotPrice :: Core.Maybe Types.SpotPrice,
    -- | The Base64-encoded user data to make available to the launched EC2 instances. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance metadata and user data> in the /Amazon EC2 User Guide for Linux Instances/ .
    userData :: Core.Maybe Types.XmlStringUserData
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateLaunchConfiguration' value with any optional fields omitted.
mkCreateLaunchConfiguration ::
  -- | 'launchConfigurationName'
  Types.XmlStringMaxLen255 ->
  CreateLaunchConfiguration
mkCreateLaunchConfiguration launchConfigurationName =
  CreateLaunchConfiguration'
    { launchConfigurationName,
      associatePublicIpAddress = Core.Nothing,
      blockDeviceMappings = Core.Nothing,
      classicLinkVPCId = Core.Nothing,
      classicLinkVPCSecurityGroups = Core.Nothing,
      ebsOptimized = Core.Nothing,
      iamInstanceProfile = Core.Nothing,
      imageId = Core.Nothing,
      instanceId = Core.Nothing,
      instanceMonitoring = Core.Nothing,
      instanceType = Core.Nothing,
      kernelId = Core.Nothing,
      keyName = Core.Nothing,
      metadataOptions = Core.Nothing,
      placementTenancy = Core.Nothing,
      ramdiskId = Core.Nothing,
      securityGroups = Core.Nothing,
      spotPrice = Core.Nothing,
      userData = Core.Nothing
    }

-- | The name of the launch configuration. This name must be unique per Region per account.
--
-- /Note:/ Consider using 'launchConfigurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcLaunchConfigurationName :: Lens.Lens' CreateLaunchConfiguration Types.XmlStringMaxLen255
clcLaunchConfigurationName = Lens.field @"launchConfigurationName"
{-# DEPRECATED clcLaunchConfigurationName "Use generic-lens or generic-optics with 'launchConfigurationName' instead." #-}

-- | For Auto Scaling groups that are running in a virtual private cloud (VPC), specifies whether to assign a public IP address to the group's instances. If you specify @true@ , each instance in the Auto Scaling group receives a unique public IP address. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html Launching Auto Scaling instances in a VPC> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- If you specify this parameter, you must specify at least one subnet for @VPCZoneIdentifier@ when you create your group.
--
-- /Note:/ Consider using 'associatePublicIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcAssociatePublicIpAddress :: Lens.Lens' CreateLaunchConfiguration (Core.Maybe Core.Bool)
clcAssociatePublicIpAddress = Lens.field @"associatePublicIpAddress"
{-# DEPRECATED clcAssociatePublicIpAddress "Use generic-lens or generic-optics with 'associatePublicIpAddress' instead." #-}

-- | A block device mapping, which specifies the block devices for the instance. You can specify virtual devices and EBS volumes. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html Block Device Mapping> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- /Note:/ Consider using 'blockDeviceMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcBlockDeviceMappings :: Lens.Lens' CreateLaunchConfiguration (Core.Maybe [Types.BlockDeviceMapping])
clcBlockDeviceMappings = Lens.field @"blockDeviceMappings"
{-# DEPRECATED clcBlockDeviceMappings "Use generic-lens or generic-optics with 'blockDeviceMappings' instead." #-}

-- | The ID of a ClassicLink-enabled VPC to link your EC2-Classic instances to. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink> in the /Amazon EC2 User Guide for Linux Instances/ and <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html#as-ClassicLink Linking EC2-Classic instances to a VPC> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- This parameter can only be used if you are launching EC2-Classic instances.
--
-- /Note:/ Consider using 'classicLinkVPCId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcClassicLinkVPCId :: Lens.Lens' CreateLaunchConfiguration (Core.Maybe Types.XmlStringMaxLen255)
clcClassicLinkVPCId = Lens.field @"classicLinkVPCId"
{-# DEPRECATED clcClassicLinkVPCId "Use generic-lens or generic-optics with 'classicLinkVPCId' instead." #-}

-- | The IDs of one or more security groups for the specified ClassicLink-enabled VPC. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink> in the /Amazon EC2 User Guide for Linux Instances/ and <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html#as-ClassicLink Linking EC2-Classic instances to a VPC> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- If you specify the @ClassicLinkVPCId@ parameter, you must specify this parameter.
--
-- /Note:/ Consider using 'classicLinkVPCSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcClassicLinkVPCSecurityGroups :: Lens.Lens' CreateLaunchConfiguration (Core.Maybe [Types.XmlStringMaxLen255])
clcClassicLinkVPCSecurityGroups = Lens.field @"classicLinkVPCSecurityGroups"
{-# DEPRECATED clcClassicLinkVPCSecurityGroups "Use generic-lens or generic-optics with 'classicLinkVPCSecurityGroups' instead." #-}

-- | Specifies whether the launch configuration is optimized for EBS I/O (@true@ ) or not (@false@ ). The optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal I/O performance. This optimization is not available with all instance types. Additional fees are incurred when you enable EBS optimization for an instance type that is not EBS-optimized by default. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSOptimized.html Amazon EBS-Optimized Instances> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- The default value is @false@ .
--
-- /Note:/ Consider using 'ebsOptimized' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcEbsOptimized :: Lens.Lens' CreateLaunchConfiguration (Core.Maybe Core.Bool)
clcEbsOptimized = Lens.field @"ebsOptimized"
{-# DEPRECATED clcEbsOptimized "Use generic-lens or generic-optics with 'ebsOptimized' instead." #-}

-- | The name or the Amazon Resource Name (ARN) of the instance profile associated with the IAM role for the instance. The instance profile contains the IAM role.
--
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/us-iam-role.html IAM role for applications that run on Amazon EC2 instances> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'iamInstanceProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcIamInstanceProfile :: Lens.Lens' CreateLaunchConfiguration (Core.Maybe Types.XmlStringMaxLen1600)
clcIamInstanceProfile = Lens.field @"iamInstanceProfile"
{-# DEPRECATED clcIamInstanceProfile "Use generic-lens or generic-optics with 'iamInstanceProfile' instead." #-}

-- | The ID of the Amazon Machine Image (AMI) that was assigned during registration. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/finding-an-ami.html Finding an AMI> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- If you do not specify @InstanceId@ , you must specify @ImageId@ .
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcImageId :: Lens.Lens' CreateLaunchConfiguration (Core.Maybe Types.XmlStringMaxLen255)
clcImageId = Lens.field @"imageId"
{-# DEPRECATED clcImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | The ID of the instance to use to create the launch configuration. The new launch configuration derives attributes from the instance, except for the block device mapping.
--
-- To create a launch configuration with a block device mapping or override any other instance attributes, specify them as part of the same request.
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-lc-with-instanceID.html Creating a launch configuration using an EC2 instance> in the /Amazon EC2 Auto Scaling User Guide/ .
-- If you do not specify @InstanceId@ , you must specify both @ImageId@ and @InstanceType@ .
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcInstanceId :: Lens.Lens' CreateLaunchConfiguration (Core.Maybe Types.InstanceId)
clcInstanceId = Lens.field @"instanceId"
{-# DEPRECATED clcInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | Controls whether instances in this group are launched with detailed (@true@ ) or basic (@false@ ) monitoring.
--
-- The default value is @true@ (enabled).
-- /Important:/ When detailed monitoring is enabled, Amazon CloudWatch generates metrics every minute and your account is charged a fee. When you disable detailed monitoring, CloudWatch generates metrics every 5 minutes. For more information, see <https://docs.aws.amazon.com/autoscaling/latest/userguide/enable-as-instance-metrics.html Configure Monitoring for Auto Scaling Instances> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'instanceMonitoring' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcInstanceMonitoring :: Lens.Lens' CreateLaunchConfiguration (Core.Maybe Types.InstanceMonitoring)
clcInstanceMonitoring = Lens.field @"instanceMonitoring"
{-# DEPRECATED clcInstanceMonitoring "Use generic-lens or generic-optics with 'instanceMonitoring' instead." #-}

-- | Specifies the instance type of the EC2 instance.
--
-- For information about available instance types, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#AvailableInstanceTypes Available Instance Types> in the /Amazon EC2 User Guide for Linux Instances./
-- If you do not specify @InstanceId@ , you must specify @InstanceType@ .
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcInstanceType :: Lens.Lens' CreateLaunchConfiguration (Core.Maybe Types.XmlStringMaxLen255)
clcInstanceType = Lens.field @"instanceType"
{-# DEPRECATED clcInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The ID of the kernel associated with the AMI.
--
-- /Note:/ Consider using 'kernelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcKernelId :: Lens.Lens' CreateLaunchConfiguration (Core.Maybe Types.XmlStringMaxLen255)
clcKernelId = Lens.field @"kernelId"
{-# DEPRECATED clcKernelId "Use generic-lens or generic-optics with 'kernelId' instead." #-}

-- | The name of the key pair. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Amazon EC2 Key Pairs> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- /Note:/ Consider using 'keyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcKeyName :: Lens.Lens' CreateLaunchConfiguration (Core.Maybe Types.XmlStringMaxLen255)
clcKeyName = Lens.field @"keyName"
{-# DEPRECATED clcKeyName "Use generic-lens or generic-optics with 'keyName' instead." #-}

-- | The metadata options for the instances. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-launch-config.html#launch-configurations-imds Configuring the Instance Metadata Options> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'metadataOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcMetadataOptions :: Lens.Lens' CreateLaunchConfiguration (Core.Maybe Types.InstanceMetadataOptions)
clcMetadataOptions = Lens.field @"metadataOptions"
{-# DEPRECATED clcMetadataOptions "Use generic-lens or generic-optics with 'metadataOptions' instead." #-}

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
{-# DEPRECATED clcPlacementTenancy "Use generic-lens or generic-optics with 'placementTenancy' instead." #-}

-- | The ID of the RAM disk to select.
--
-- /Note:/ Consider using 'ramdiskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcRamdiskId :: Lens.Lens' CreateLaunchConfiguration (Core.Maybe Types.XmlStringMaxLen255)
clcRamdiskId = Lens.field @"ramdiskId"
{-# DEPRECATED clcRamdiskId "Use generic-lens or generic-optics with 'ramdiskId' instead." #-}

-- | A list that contains the security groups to assign to the instances in the Auto Scaling group.
--
-- [EC2-VPC] Specify the security group IDs. For more information, see <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_SecurityGroups.html Security Groups for Your VPC> in the /Amazon Virtual Private Cloud User Guide/ .
-- [EC2-Classic] Specify either the security group names or the security group IDs. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-network-security.html Amazon EC2 Security Groups> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcSecurityGroups :: Lens.Lens' CreateLaunchConfiguration (Core.Maybe [Types.XmlString])
clcSecurityGroups = Lens.field @"securityGroups"
{-# DEPRECATED clcSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | The maximum hourly price to be paid for any Spot Instance launched to fulfill the request. Spot Instances are launched when the price you specify exceeds the current Spot price. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-launch-spot-instances.html Requesting Spot Instances> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'spotPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcSpotPrice :: Lens.Lens' CreateLaunchConfiguration (Core.Maybe Types.SpotPrice)
clcSpotPrice = Lens.field @"spotPrice"
{-# DEPRECATED clcSpotPrice "Use generic-lens or generic-optics with 'spotPrice' instead." #-}

-- | The Base64-encoded user data to make available to the launched EC2 instances. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance metadata and user data> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- /Note:/ Consider using 'userData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcUserData :: Lens.Lens' CreateLaunchConfiguration (Core.Maybe Types.XmlStringUserData)
clcUserData = Lens.field @"userData"
{-# DEPRECATED clcUserData "Use generic-lens or generic-optics with 'userData' instead." #-}

instance Core.AWSRequest CreateLaunchConfiguration where
  type
    Rs CreateLaunchConfiguration =
      CreateLaunchConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "CreateLaunchConfiguration")
                Core.<> (Core.pure ("Version", "2011-01-01"))
                Core.<> ( Core.toQueryValue
                            "LaunchConfigurationName"
                            launchConfigurationName
                        )
                Core.<> ( Core.toQueryValue "AssociatePublicIpAddress"
                            Core.<$> associatePublicIpAddress
                        )
                Core.<> ( Core.toQueryValue
                            "BlockDeviceMappings"
                            (Core.toQueryList "member" Core.<$> blockDeviceMappings)
                        )
                Core.<> (Core.toQueryValue "ClassicLinkVPCId" Core.<$> classicLinkVPCId)
                Core.<> ( Core.toQueryValue
                            "ClassicLinkVPCSecurityGroups"
                            ( Core.toQueryList "member"
                                Core.<$> classicLinkVPCSecurityGroups
                            )
                        )
                Core.<> (Core.toQueryValue "EbsOptimized" Core.<$> ebsOptimized)
                Core.<> ( Core.toQueryValue "IamInstanceProfile"
                            Core.<$> iamInstanceProfile
                        )
                Core.<> (Core.toQueryValue "ImageId" Core.<$> imageId)
                Core.<> (Core.toQueryValue "InstanceId" Core.<$> instanceId)
                Core.<> ( Core.toQueryValue "InstanceMonitoring"
                            Core.<$> instanceMonitoring
                        )
                Core.<> (Core.toQueryValue "InstanceType" Core.<$> instanceType)
                Core.<> (Core.toQueryValue "KernelId" Core.<$> kernelId)
                Core.<> (Core.toQueryValue "KeyName" Core.<$> keyName)
                Core.<> (Core.toQueryValue "MetadataOptions" Core.<$> metadataOptions)
                Core.<> (Core.toQueryValue "PlacementTenancy" Core.<$> placementTenancy)
                Core.<> (Core.toQueryValue "RamdiskId" Core.<$> ramdiskId)
                Core.<> ( Core.toQueryValue
                            "SecurityGroups"
                            (Core.toQueryList "member" Core.<$> securityGroups)
                        )
                Core.<> (Core.toQueryValue "SpotPrice" Core.<$> spotPrice)
                Core.<> (Core.toQueryValue "UserData" Core.<$> userData)
            )
      }
  response = Response.receiveNull CreateLaunchConfigurationResponse'

-- | /See:/ 'mkCreateLaunchConfigurationResponse' smart constructor.
data CreateLaunchConfigurationResponse = CreateLaunchConfigurationResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateLaunchConfigurationResponse' value with any optional fields omitted.
mkCreateLaunchConfigurationResponse ::
  CreateLaunchConfigurationResponse
mkCreateLaunchConfigurationResponse =
  CreateLaunchConfigurationResponse'
