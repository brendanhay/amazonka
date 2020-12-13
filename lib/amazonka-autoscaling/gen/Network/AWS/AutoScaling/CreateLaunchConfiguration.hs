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
    clcInstanceId,
    clcAssociatePublicIPAddress,
    clcSecurityGroups,
    clcSpotPrice,
    clcInstanceMonitoring,
    clcKeyName,
    clcClassicLinkVPCSecurityGroups,
    clcRAMDiskId,
    clcKernelId,
    clcInstanceType,
    clcEBSOptimized,
    clcUserData,
    clcClassicLinkVPCId,
    clcIAMInstanceProfile,
    clcImageId,
    clcLaunchConfigurationName,
    clcMetadataOptions,
    clcPlacementTenancy,
    clcBlockDeviceMappings,

    -- * Destructuring the response
    CreateLaunchConfigurationResponse (..),
    mkCreateLaunchConfigurationResponse,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateLaunchConfiguration' smart constructor.
data CreateLaunchConfiguration = CreateLaunchConfiguration'
  { -- | The ID of the instance to use to create the launch configuration. The new launch configuration derives attributes from the instance, except for the block device mapping.
    --
    -- To create a launch configuration with a block device mapping or override any other instance attributes, specify them as part of the same request.
    -- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-lc-with-instanceID.html Creating a launch configuration using an EC2 instance> in the /Amazon EC2 Auto Scaling User Guide/ .
    -- If you do not specify @InstanceId@ , you must specify both @ImageId@ and @InstanceType@ .
    instanceId :: Lude.Maybe Lude.Text,
    -- | For Auto Scaling groups that are running in a virtual private cloud (VPC), specifies whether to assign a public IP address to the group's instances. If you specify @true@ , each instance in the Auto Scaling group receives a unique public IP address. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html Launching Auto Scaling instances in a VPC> in the /Amazon EC2 Auto Scaling User Guide/ .
    --
    -- If you specify this parameter, you must specify at least one subnet for @VPCZoneIdentifier@ when you create your group.
    associatePublicIPAddress :: Lude.Maybe Lude.Bool,
    -- | A list that contains the security groups to assign to the instances in the Auto Scaling group.
    --
    -- [EC2-VPC] Specify the security group IDs. For more information, see <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_SecurityGroups.html Security Groups for Your VPC> in the /Amazon Virtual Private Cloud User Guide/ .
    -- [EC2-Classic] Specify either the security group names or the security group IDs. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-network-security.html Amazon EC2 Security Groups> in the /Amazon EC2 User Guide for Linux Instances/ .
    securityGroups :: Lude.Maybe [Lude.Text],
    -- | The maximum hourly price to be paid for any Spot Instance launched to fulfill the request. Spot Instances are launched when the price you specify exceeds the current Spot price. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-launch-spot-instances.html Requesting Spot Instances> in the /Amazon EC2 Auto Scaling User Guide/ .
    spotPrice :: Lude.Maybe Lude.Text,
    -- | Controls whether instances in this group are launched with detailed (@true@ ) or basic (@false@ ) monitoring.
    --
    -- The default value is @true@ (enabled).
    -- /Important:/ When detailed monitoring is enabled, Amazon CloudWatch generates metrics every minute and your account is charged a fee. When you disable detailed monitoring, CloudWatch generates metrics every 5 minutes. For more information, see <https://docs.aws.amazon.com/autoscaling/latest/userguide/enable-as-instance-metrics.html Configure Monitoring for Auto Scaling Instances> in the /Amazon EC2 Auto Scaling User Guide/ .
    instanceMonitoring :: Lude.Maybe InstanceMonitoring,
    -- | The name of the key pair. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Amazon EC2 Key Pairs> in the /Amazon EC2 User Guide for Linux Instances/ .
    keyName :: Lude.Maybe Lude.Text,
    -- | The IDs of one or more security groups for the specified ClassicLink-enabled VPC. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink> in the /Amazon EC2 User Guide for Linux Instances/ and <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html#as-ClassicLink Linking EC2-Classic instances to a VPC> in the /Amazon EC2 Auto Scaling User Guide/ .
    --
    -- If you specify the @ClassicLinkVPCId@ parameter, you must specify this parameter.
    classicLinkVPCSecurityGroups :: Lude.Maybe [Lude.Text],
    -- | The ID of the RAM disk to select.
    ramdiskId :: Lude.Maybe Lude.Text,
    -- | The ID of the kernel associated with the AMI.
    kernelId :: Lude.Maybe Lude.Text,
    -- | Specifies the instance type of the EC2 instance.
    --
    -- For information about available instance types, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#AvailableInstanceTypes Available Instance Types> in the /Amazon EC2 User Guide for Linux Instances./
    -- If you do not specify @InstanceId@ , you must specify @InstanceType@ .
    instanceType :: Lude.Maybe Lude.Text,
    -- | Specifies whether the launch configuration is optimized for EBS I/O (@true@ ) or not (@false@ ). The optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal I/O performance. This optimization is not available with all instance types. Additional fees are incurred when you enable EBS optimization for an instance type that is not EBS-optimized by default. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSOptimized.html Amazon EBS-Optimized Instances> in the /Amazon EC2 User Guide for Linux Instances/ .
    --
    -- The default value is @false@ .
    ebsOptimized :: Lude.Maybe Lude.Bool,
    -- | The Base64-encoded user data to make available to the launched EC2 instances. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance metadata and user data> in the /Amazon EC2 User Guide for Linux Instances/ .
    userData :: Lude.Maybe Lude.Text,
    -- | The ID of a ClassicLink-enabled VPC to link your EC2-Classic instances to. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink> in the /Amazon EC2 User Guide for Linux Instances/ and <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html#as-ClassicLink Linking EC2-Classic instances to a VPC> in the /Amazon EC2 Auto Scaling User Guide/ .
    --
    -- This parameter can only be used if you are launching EC2-Classic instances.
    classicLinkVPCId :: Lude.Maybe Lude.Text,
    -- | The name or the Amazon Resource Name (ARN) of the instance profile associated with the IAM role for the instance. The instance profile contains the IAM role.
    --
    -- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/us-iam-role.html IAM role for applications that run on Amazon EC2 instances> in the /Amazon EC2 Auto Scaling User Guide/ .
    iamInstanceProfile :: Lude.Maybe Lude.Text,
    -- | The ID of the Amazon Machine Image (AMI) that was assigned during registration. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/finding-an-ami.html Finding an AMI> in the /Amazon EC2 User Guide for Linux Instances/ .
    --
    -- If you do not specify @InstanceId@ , you must specify @ImageId@ .
    imageId :: Lude.Maybe Lude.Text,
    -- | The name of the launch configuration. This name must be unique per Region per account.
    launchConfigurationName :: Lude.Text,
    -- | The metadata options for the instances. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-launch-config.html#launch-configurations-imds Configuring the Instance Metadata Options> in the /Amazon EC2 Auto Scaling User Guide/ .
    metadataOptions :: Lude.Maybe InstanceMetadataOptions,
    -- | The tenancy of the instance. An instance with @dedicated@ tenancy runs on isolated, single-tenant hardware and can only be launched into a VPC.
    --
    -- To launch dedicated instances into a shared tenancy VPC (a VPC with the instance placement tenancy attribute set to @default@ ), you must set the value of this parameter to @dedicated@ .
    -- If you specify @PlacementTenancy@ , you must specify at least one subnet for @VPCZoneIdentifier@ when you create your group.
    -- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/auto-scaling-dedicated-instances.html Configuring instance tenancy with Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
    -- Valid Values: @default@ | @dedicated@
    placementTenancy :: Lude.Maybe Lude.Text,
    -- | A block device mapping, which specifies the block devices for the instance. You can specify virtual devices and EBS volumes. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html Block Device Mapping> in the /Amazon EC2 User Guide for Linux Instances/ .
    blockDeviceMappings :: Lude.Maybe [BlockDeviceMapping]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateLaunchConfiguration' with the minimum fields required to make a request.
--
-- * 'instanceId' - The ID of the instance to use to create the launch configuration. The new launch configuration derives attributes from the instance, except for the block device mapping.
--
-- To create a launch configuration with a block device mapping or override any other instance attributes, specify them as part of the same request.
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-lc-with-instanceID.html Creating a launch configuration using an EC2 instance> in the /Amazon EC2 Auto Scaling User Guide/ .
-- If you do not specify @InstanceId@ , you must specify both @ImageId@ and @InstanceType@ .
-- * 'associatePublicIPAddress' - For Auto Scaling groups that are running in a virtual private cloud (VPC), specifies whether to assign a public IP address to the group's instances. If you specify @true@ , each instance in the Auto Scaling group receives a unique public IP address. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html Launching Auto Scaling instances in a VPC> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- If you specify this parameter, you must specify at least one subnet for @VPCZoneIdentifier@ when you create your group.
-- * 'securityGroups' - A list that contains the security groups to assign to the instances in the Auto Scaling group.
--
-- [EC2-VPC] Specify the security group IDs. For more information, see <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_SecurityGroups.html Security Groups for Your VPC> in the /Amazon Virtual Private Cloud User Guide/ .
-- [EC2-Classic] Specify either the security group names or the security group IDs. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-network-security.html Amazon EC2 Security Groups> in the /Amazon EC2 User Guide for Linux Instances/ .
-- * 'spotPrice' - The maximum hourly price to be paid for any Spot Instance launched to fulfill the request. Spot Instances are launched when the price you specify exceeds the current Spot price. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-launch-spot-instances.html Requesting Spot Instances> in the /Amazon EC2 Auto Scaling User Guide/ .
-- * 'instanceMonitoring' - Controls whether instances in this group are launched with detailed (@true@ ) or basic (@false@ ) monitoring.
--
-- The default value is @true@ (enabled).
-- /Important:/ When detailed monitoring is enabled, Amazon CloudWatch generates metrics every minute and your account is charged a fee. When you disable detailed monitoring, CloudWatch generates metrics every 5 minutes. For more information, see <https://docs.aws.amazon.com/autoscaling/latest/userguide/enable-as-instance-metrics.html Configure Monitoring for Auto Scaling Instances> in the /Amazon EC2 Auto Scaling User Guide/ .
-- * 'keyName' - The name of the key pair. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Amazon EC2 Key Pairs> in the /Amazon EC2 User Guide for Linux Instances/ .
-- * 'classicLinkVPCSecurityGroups' - The IDs of one or more security groups for the specified ClassicLink-enabled VPC. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink> in the /Amazon EC2 User Guide for Linux Instances/ and <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html#as-ClassicLink Linking EC2-Classic instances to a VPC> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- If you specify the @ClassicLinkVPCId@ parameter, you must specify this parameter.
-- * 'ramdiskId' - The ID of the RAM disk to select.
-- * 'kernelId' - The ID of the kernel associated with the AMI.
-- * 'instanceType' - Specifies the instance type of the EC2 instance.
--
-- For information about available instance types, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#AvailableInstanceTypes Available Instance Types> in the /Amazon EC2 User Guide for Linux Instances./
-- If you do not specify @InstanceId@ , you must specify @InstanceType@ .
-- * 'ebsOptimized' - Specifies whether the launch configuration is optimized for EBS I/O (@true@ ) or not (@false@ ). The optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal I/O performance. This optimization is not available with all instance types. Additional fees are incurred when you enable EBS optimization for an instance type that is not EBS-optimized by default. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSOptimized.html Amazon EBS-Optimized Instances> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- The default value is @false@ .
-- * 'userData' - The Base64-encoded user data to make available to the launched EC2 instances. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance metadata and user data> in the /Amazon EC2 User Guide for Linux Instances/ .
-- * 'classicLinkVPCId' - The ID of a ClassicLink-enabled VPC to link your EC2-Classic instances to. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink> in the /Amazon EC2 User Guide for Linux Instances/ and <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html#as-ClassicLink Linking EC2-Classic instances to a VPC> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- This parameter can only be used if you are launching EC2-Classic instances.
-- * 'iamInstanceProfile' - The name or the Amazon Resource Name (ARN) of the instance profile associated with the IAM role for the instance. The instance profile contains the IAM role.
--
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/us-iam-role.html IAM role for applications that run on Amazon EC2 instances> in the /Amazon EC2 Auto Scaling User Guide/ .
-- * 'imageId' - The ID of the Amazon Machine Image (AMI) that was assigned during registration. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/finding-an-ami.html Finding an AMI> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- If you do not specify @InstanceId@ , you must specify @ImageId@ .
-- * 'launchConfigurationName' - The name of the launch configuration. This name must be unique per Region per account.
-- * 'metadataOptions' - The metadata options for the instances. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-launch-config.html#launch-configurations-imds Configuring the Instance Metadata Options> in the /Amazon EC2 Auto Scaling User Guide/ .
-- * 'placementTenancy' - The tenancy of the instance. An instance with @dedicated@ tenancy runs on isolated, single-tenant hardware and can only be launched into a VPC.
--
-- To launch dedicated instances into a shared tenancy VPC (a VPC with the instance placement tenancy attribute set to @default@ ), you must set the value of this parameter to @dedicated@ .
-- If you specify @PlacementTenancy@ , you must specify at least one subnet for @VPCZoneIdentifier@ when you create your group.
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/auto-scaling-dedicated-instances.html Configuring instance tenancy with Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
-- Valid Values: @default@ | @dedicated@
-- * 'blockDeviceMappings' - A block device mapping, which specifies the block devices for the instance. You can specify virtual devices and EBS volumes. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html Block Device Mapping> in the /Amazon EC2 User Guide for Linux Instances/ .
mkCreateLaunchConfiguration ::
  -- | 'launchConfigurationName'
  Lude.Text ->
  CreateLaunchConfiguration
mkCreateLaunchConfiguration pLaunchConfigurationName_ =
  CreateLaunchConfiguration'
    { instanceId = Lude.Nothing,
      associatePublicIPAddress = Lude.Nothing,
      securityGroups = Lude.Nothing,
      spotPrice = Lude.Nothing,
      instanceMonitoring = Lude.Nothing,
      keyName = Lude.Nothing,
      classicLinkVPCSecurityGroups = Lude.Nothing,
      ramdiskId = Lude.Nothing,
      kernelId = Lude.Nothing,
      instanceType = Lude.Nothing,
      ebsOptimized = Lude.Nothing,
      userData = Lude.Nothing,
      classicLinkVPCId = Lude.Nothing,
      iamInstanceProfile = Lude.Nothing,
      imageId = Lude.Nothing,
      launchConfigurationName = pLaunchConfigurationName_,
      metadataOptions = Lude.Nothing,
      placementTenancy = Lude.Nothing,
      blockDeviceMappings = Lude.Nothing
    }

-- | The ID of the instance to use to create the launch configuration. The new launch configuration derives attributes from the instance, except for the block device mapping.
--
-- To create a launch configuration with a block device mapping or override any other instance attributes, specify them as part of the same request.
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-lc-with-instanceID.html Creating a launch configuration using an EC2 instance> in the /Amazon EC2 Auto Scaling User Guide/ .
-- If you do not specify @InstanceId@ , you must specify both @ImageId@ and @InstanceType@ .
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcInstanceId :: Lens.Lens' CreateLaunchConfiguration (Lude.Maybe Lude.Text)
clcInstanceId = Lens.lens (instanceId :: CreateLaunchConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: CreateLaunchConfiguration)
{-# DEPRECATED clcInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | For Auto Scaling groups that are running in a virtual private cloud (VPC), specifies whether to assign a public IP address to the group's instances. If you specify @true@ , each instance in the Auto Scaling group receives a unique public IP address. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html Launching Auto Scaling instances in a VPC> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- If you specify this parameter, you must specify at least one subnet for @VPCZoneIdentifier@ when you create your group.
--
-- /Note:/ Consider using 'associatePublicIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcAssociatePublicIPAddress :: Lens.Lens' CreateLaunchConfiguration (Lude.Maybe Lude.Bool)
clcAssociatePublicIPAddress = Lens.lens (associatePublicIPAddress :: CreateLaunchConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {associatePublicIPAddress = a} :: CreateLaunchConfiguration)
{-# DEPRECATED clcAssociatePublicIPAddress "Use generic-lens or generic-optics with 'associatePublicIPAddress' instead." #-}

-- | A list that contains the security groups to assign to the instances in the Auto Scaling group.
--
-- [EC2-VPC] Specify the security group IDs. For more information, see <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_SecurityGroups.html Security Groups for Your VPC> in the /Amazon Virtual Private Cloud User Guide/ .
-- [EC2-Classic] Specify either the security group names or the security group IDs. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-network-security.html Amazon EC2 Security Groups> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcSecurityGroups :: Lens.Lens' CreateLaunchConfiguration (Lude.Maybe [Lude.Text])
clcSecurityGroups = Lens.lens (securityGroups :: CreateLaunchConfiguration -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroups = a} :: CreateLaunchConfiguration)
{-# DEPRECATED clcSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | The maximum hourly price to be paid for any Spot Instance launched to fulfill the request. Spot Instances are launched when the price you specify exceeds the current Spot price. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-launch-spot-instances.html Requesting Spot Instances> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'spotPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcSpotPrice :: Lens.Lens' CreateLaunchConfiguration (Lude.Maybe Lude.Text)
clcSpotPrice = Lens.lens (spotPrice :: CreateLaunchConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {spotPrice = a} :: CreateLaunchConfiguration)
{-# DEPRECATED clcSpotPrice "Use generic-lens or generic-optics with 'spotPrice' instead." #-}

-- | Controls whether instances in this group are launched with detailed (@true@ ) or basic (@false@ ) monitoring.
--
-- The default value is @true@ (enabled).
-- /Important:/ When detailed monitoring is enabled, Amazon CloudWatch generates metrics every minute and your account is charged a fee. When you disable detailed monitoring, CloudWatch generates metrics every 5 minutes. For more information, see <https://docs.aws.amazon.com/autoscaling/latest/userguide/enable-as-instance-metrics.html Configure Monitoring for Auto Scaling Instances> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'instanceMonitoring' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcInstanceMonitoring :: Lens.Lens' CreateLaunchConfiguration (Lude.Maybe InstanceMonitoring)
clcInstanceMonitoring = Lens.lens (instanceMonitoring :: CreateLaunchConfiguration -> Lude.Maybe InstanceMonitoring) (\s a -> s {instanceMonitoring = a} :: CreateLaunchConfiguration)
{-# DEPRECATED clcInstanceMonitoring "Use generic-lens or generic-optics with 'instanceMonitoring' instead." #-}

-- | The name of the key pair. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Amazon EC2 Key Pairs> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- /Note:/ Consider using 'keyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcKeyName :: Lens.Lens' CreateLaunchConfiguration (Lude.Maybe Lude.Text)
clcKeyName = Lens.lens (keyName :: CreateLaunchConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {keyName = a} :: CreateLaunchConfiguration)
{-# DEPRECATED clcKeyName "Use generic-lens or generic-optics with 'keyName' instead." #-}

-- | The IDs of one or more security groups for the specified ClassicLink-enabled VPC. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink> in the /Amazon EC2 User Guide for Linux Instances/ and <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html#as-ClassicLink Linking EC2-Classic instances to a VPC> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- If you specify the @ClassicLinkVPCId@ parameter, you must specify this parameter.
--
-- /Note:/ Consider using 'classicLinkVPCSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcClassicLinkVPCSecurityGroups :: Lens.Lens' CreateLaunchConfiguration (Lude.Maybe [Lude.Text])
clcClassicLinkVPCSecurityGroups = Lens.lens (classicLinkVPCSecurityGroups :: CreateLaunchConfiguration -> Lude.Maybe [Lude.Text]) (\s a -> s {classicLinkVPCSecurityGroups = a} :: CreateLaunchConfiguration)
{-# DEPRECATED clcClassicLinkVPCSecurityGroups "Use generic-lens or generic-optics with 'classicLinkVPCSecurityGroups' instead." #-}

-- | The ID of the RAM disk to select.
--
-- /Note:/ Consider using 'ramdiskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcRAMDiskId :: Lens.Lens' CreateLaunchConfiguration (Lude.Maybe Lude.Text)
clcRAMDiskId = Lens.lens (ramdiskId :: CreateLaunchConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {ramdiskId = a} :: CreateLaunchConfiguration)
{-# DEPRECATED clcRAMDiskId "Use generic-lens or generic-optics with 'ramdiskId' instead." #-}

-- | The ID of the kernel associated with the AMI.
--
-- /Note:/ Consider using 'kernelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcKernelId :: Lens.Lens' CreateLaunchConfiguration (Lude.Maybe Lude.Text)
clcKernelId = Lens.lens (kernelId :: CreateLaunchConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {kernelId = a} :: CreateLaunchConfiguration)
{-# DEPRECATED clcKernelId "Use generic-lens or generic-optics with 'kernelId' instead." #-}

-- | Specifies the instance type of the EC2 instance.
--
-- For information about available instance types, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#AvailableInstanceTypes Available Instance Types> in the /Amazon EC2 User Guide for Linux Instances./
-- If you do not specify @InstanceId@ , you must specify @InstanceType@ .
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcInstanceType :: Lens.Lens' CreateLaunchConfiguration (Lude.Maybe Lude.Text)
clcInstanceType = Lens.lens (instanceType :: CreateLaunchConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {instanceType = a} :: CreateLaunchConfiguration)
{-# DEPRECATED clcInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | Specifies whether the launch configuration is optimized for EBS I/O (@true@ ) or not (@false@ ). The optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal I/O performance. This optimization is not available with all instance types. Additional fees are incurred when you enable EBS optimization for an instance type that is not EBS-optimized by default. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSOptimized.html Amazon EBS-Optimized Instances> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- The default value is @false@ .
--
-- /Note:/ Consider using 'ebsOptimized' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcEBSOptimized :: Lens.Lens' CreateLaunchConfiguration (Lude.Maybe Lude.Bool)
clcEBSOptimized = Lens.lens (ebsOptimized :: CreateLaunchConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {ebsOptimized = a} :: CreateLaunchConfiguration)
{-# DEPRECATED clcEBSOptimized "Use generic-lens or generic-optics with 'ebsOptimized' instead." #-}

-- | The Base64-encoded user data to make available to the launched EC2 instances. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance metadata and user data> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- /Note:/ Consider using 'userData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcUserData :: Lens.Lens' CreateLaunchConfiguration (Lude.Maybe Lude.Text)
clcUserData = Lens.lens (userData :: CreateLaunchConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {userData = a} :: CreateLaunchConfiguration)
{-# DEPRECATED clcUserData "Use generic-lens or generic-optics with 'userData' instead." #-}

-- | The ID of a ClassicLink-enabled VPC to link your EC2-Classic instances to. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink> in the /Amazon EC2 User Guide for Linux Instances/ and <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html#as-ClassicLink Linking EC2-Classic instances to a VPC> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- This parameter can only be used if you are launching EC2-Classic instances.
--
-- /Note:/ Consider using 'classicLinkVPCId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcClassicLinkVPCId :: Lens.Lens' CreateLaunchConfiguration (Lude.Maybe Lude.Text)
clcClassicLinkVPCId = Lens.lens (classicLinkVPCId :: CreateLaunchConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {classicLinkVPCId = a} :: CreateLaunchConfiguration)
{-# DEPRECATED clcClassicLinkVPCId "Use generic-lens or generic-optics with 'classicLinkVPCId' instead." #-}

-- | The name or the Amazon Resource Name (ARN) of the instance profile associated with the IAM role for the instance. The instance profile contains the IAM role.
--
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/us-iam-role.html IAM role for applications that run on Amazon EC2 instances> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'iamInstanceProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcIAMInstanceProfile :: Lens.Lens' CreateLaunchConfiguration (Lude.Maybe Lude.Text)
clcIAMInstanceProfile = Lens.lens (iamInstanceProfile :: CreateLaunchConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {iamInstanceProfile = a} :: CreateLaunchConfiguration)
{-# DEPRECATED clcIAMInstanceProfile "Use generic-lens or generic-optics with 'iamInstanceProfile' instead." #-}

-- | The ID of the Amazon Machine Image (AMI) that was assigned during registration. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/finding-an-ami.html Finding an AMI> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- If you do not specify @InstanceId@ , you must specify @ImageId@ .
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcImageId :: Lens.Lens' CreateLaunchConfiguration (Lude.Maybe Lude.Text)
clcImageId = Lens.lens (imageId :: CreateLaunchConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {imageId = a} :: CreateLaunchConfiguration)
{-# DEPRECATED clcImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | The name of the launch configuration. This name must be unique per Region per account.
--
-- /Note:/ Consider using 'launchConfigurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcLaunchConfigurationName :: Lens.Lens' CreateLaunchConfiguration Lude.Text
clcLaunchConfigurationName = Lens.lens (launchConfigurationName :: CreateLaunchConfiguration -> Lude.Text) (\s a -> s {launchConfigurationName = a} :: CreateLaunchConfiguration)
{-# DEPRECATED clcLaunchConfigurationName "Use generic-lens or generic-optics with 'launchConfigurationName' instead." #-}

-- | The metadata options for the instances. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-launch-config.html#launch-configurations-imds Configuring the Instance Metadata Options> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'metadataOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcMetadataOptions :: Lens.Lens' CreateLaunchConfiguration (Lude.Maybe InstanceMetadataOptions)
clcMetadataOptions = Lens.lens (metadataOptions :: CreateLaunchConfiguration -> Lude.Maybe InstanceMetadataOptions) (\s a -> s {metadataOptions = a} :: CreateLaunchConfiguration)
{-# DEPRECATED clcMetadataOptions "Use generic-lens or generic-optics with 'metadataOptions' instead." #-}

-- | The tenancy of the instance. An instance with @dedicated@ tenancy runs on isolated, single-tenant hardware and can only be launched into a VPC.
--
-- To launch dedicated instances into a shared tenancy VPC (a VPC with the instance placement tenancy attribute set to @default@ ), you must set the value of this parameter to @dedicated@ .
-- If you specify @PlacementTenancy@ , you must specify at least one subnet for @VPCZoneIdentifier@ when you create your group.
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/auto-scaling-dedicated-instances.html Configuring instance tenancy with Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
-- Valid Values: @default@ | @dedicated@
--
-- /Note:/ Consider using 'placementTenancy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcPlacementTenancy :: Lens.Lens' CreateLaunchConfiguration (Lude.Maybe Lude.Text)
clcPlacementTenancy = Lens.lens (placementTenancy :: CreateLaunchConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {placementTenancy = a} :: CreateLaunchConfiguration)
{-# DEPRECATED clcPlacementTenancy "Use generic-lens or generic-optics with 'placementTenancy' instead." #-}

-- | A block device mapping, which specifies the block devices for the instance. You can specify virtual devices and EBS volumes. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html Block Device Mapping> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- /Note:/ Consider using 'blockDeviceMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcBlockDeviceMappings :: Lens.Lens' CreateLaunchConfiguration (Lude.Maybe [BlockDeviceMapping])
clcBlockDeviceMappings = Lens.lens (blockDeviceMappings :: CreateLaunchConfiguration -> Lude.Maybe [BlockDeviceMapping]) (\s a -> s {blockDeviceMappings = a} :: CreateLaunchConfiguration)
{-# DEPRECATED clcBlockDeviceMappings "Use generic-lens or generic-optics with 'blockDeviceMappings' instead." #-}

instance Lude.AWSRequest CreateLaunchConfiguration where
  type
    Rs CreateLaunchConfiguration =
      CreateLaunchConfigurationResponse
  request = Req.postQuery autoScalingService
  response = Res.receiveNull CreateLaunchConfigurationResponse'

instance Lude.ToHeaders CreateLaunchConfiguration where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateLaunchConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateLaunchConfiguration where
  toQuery CreateLaunchConfiguration' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateLaunchConfiguration" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "InstanceId" Lude.=: instanceId,
        "AssociatePublicIpAddress" Lude.=: associatePublicIPAddress,
        "SecurityGroups"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> securityGroups),
        "SpotPrice" Lude.=: spotPrice,
        "InstanceMonitoring" Lude.=: instanceMonitoring,
        "KeyName" Lude.=: keyName,
        "ClassicLinkVPCSecurityGroups"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> classicLinkVPCSecurityGroups),
        "RamdiskId" Lude.=: ramdiskId,
        "KernelId" Lude.=: kernelId,
        "InstanceType" Lude.=: instanceType,
        "EbsOptimized" Lude.=: ebsOptimized,
        "UserData" Lude.=: userData,
        "ClassicLinkVPCId" Lude.=: classicLinkVPCId,
        "IamInstanceProfile" Lude.=: iamInstanceProfile,
        "ImageId" Lude.=: imageId,
        "LaunchConfigurationName" Lude.=: launchConfigurationName,
        "MetadataOptions" Lude.=: metadataOptions,
        "PlacementTenancy" Lude.=: placementTenancy,
        "BlockDeviceMappings"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> blockDeviceMappings)
      ]

-- | /See:/ 'mkCreateLaunchConfigurationResponse' smart constructor.
data CreateLaunchConfigurationResponse = CreateLaunchConfigurationResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateLaunchConfigurationResponse' with the minimum fields required to make a request.
mkCreateLaunchConfigurationResponse ::
  CreateLaunchConfigurationResponse
mkCreateLaunchConfigurationResponse =
  CreateLaunchConfigurationResponse'
