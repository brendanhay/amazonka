{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.LaunchConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.LaunchConfiguration
  ( LaunchConfiguration (..),

    -- * Smart constructor
    mkLaunchConfiguration,

    -- * Lenses
    lcAssociatePublicIPAddress,
    lcSecurityGroups,
    lcSpotPrice,
    lcCreatedTime,
    lcInstanceMonitoring,
    lcKeyName,
    lcClassicLinkVPCSecurityGroups,
    lcRAMDiskId,
    lcKernelId,
    lcInstanceType,
    lcEBSOptimized,
    lcUserData,
    lcClassicLinkVPCId,
    lcIAMInstanceProfile,
    lcImageId,
    lcLaunchConfigurationName,
    lcMetadataOptions,
    lcLaunchConfigurationARN,
    lcPlacementTenancy,
    lcBlockDeviceMappings,
  )
where

import Network.AWS.AutoScaling.Types.BlockDeviceMapping
import Network.AWS.AutoScaling.Types.InstanceMetadataOptions
import Network.AWS.AutoScaling.Types.InstanceMonitoring
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a launch configuration.
--
-- /See:/ 'mkLaunchConfiguration' smart constructor.
data LaunchConfiguration = LaunchConfiguration'
  { -- | For Auto Scaling groups that are running in a VPC, specifies whether to assign a public IP address to the group's instances. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html Launching Auto Scaling instances in a VPC> in the /Amazon EC2 Auto Scaling User Guide/ .
    associatePublicIPAddress :: Lude.Maybe Lude.Bool,
    -- | A list that contains the security groups to assign to the instances in the Auto Scaling group. For more information, see <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_SecurityGroups.html Security Groups for Your VPC> in the /Amazon Virtual Private Cloud User Guide/ .
    securityGroups :: Lude.Maybe [Lude.Text],
    -- | The maximum hourly price to be paid for any Spot Instance launched to fulfill the request. Spot Instances are launched when the price you specify exceeds the current Spot price. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-launch-spot-instances.html Requesting Spot Instances> in the /Amazon EC2 Auto Scaling User Guide/ .
    spotPrice :: Lude.Maybe Lude.Text,
    -- | The creation date and time for the launch configuration.
    createdTime :: Lude.DateTime,
    -- | Controls whether instances in this group are launched with detailed (@true@ ) or basic (@false@ ) monitoring.
    --
    -- For more information, see <https://docs.aws.amazon.com/autoscaling/latest/userguide/enable-as-instance-metrics.html Configure Monitoring for Auto Scaling Instances> in the /Amazon EC2 Auto Scaling User Guide/ .
    instanceMonitoring :: Lude.Maybe InstanceMonitoring,
    -- | The name of the key pair.
    --
    -- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Amazon EC2 Key Pairs> in the /Amazon EC2 User Guide for Linux Instances/ .
    keyName :: Lude.Maybe Lude.Text,
    -- | The IDs of one or more security groups for the VPC specified in @ClassicLinkVPCId@ .
    --
    -- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink> in the /Amazon EC2 User Guide for Linux Instances/ and <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html#as-ClassicLink Linking EC2-Classic instances to a VPC> in the /Amazon EC2 Auto Scaling User Guide/ .
    classicLinkVPCSecurityGroups :: Lude.Maybe [Lude.Text],
    -- | The ID of the RAM disk associated with the AMI.
    ramdiskId :: Lude.Maybe Lude.Text,
    -- | The ID of the kernel associated with the AMI.
    kernelId :: Lude.Maybe Lude.Text,
    -- | The instance type for the instances.
    --
    -- For information about available instance types, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#AvailableInstanceTypes Available Instance Types> in the /Amazon EC2 User Guide for Linux Instances./
    instanceType :: Lude.Text,
    -- | Specifies whether the launch configuration is optimized for EBS I/O (@true@ ) or not (@false@ ). For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSOptimized.html Amazon EBS-Optimized Instances> in the /Amazon EC2 User Guide for Linux Instances/ .
    ebsOptimized :: Lude.Maybe Lude.Bool,
    -- | The Base64-encoded user data to make available to the launched EC2 instances. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance metadata and user data> in the /Amazon EC2 User Guide for Linux Instances/ .
    userData :: Lude.Maybe Lude.Text,
    -- | The ID of a ClassicLink-enabled VPC to link your EC2-Classic instances to. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink> in the /Amazon EC2 User Guide for Linux Instances/ and <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html#as-ClassicLink Linking EC2-Classic instances to a VPC> in the /Amazon EC2 Auto Scaling User Guide/ .
    classicLinkVPCId :: Lude.Maybe Lude.Text,
    -- | The name or the Amazon Resource Name (ARN) of the instance profile associated with the IAM role for the instance. The instance profile contains the IAM role. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/us-iam-role.html IAM role for applications that run on Amazon EC2 instances> in the /Amazon EC2 Auto Scaling User Guide/ .
    iamInstanceProfile :: Lude.Maybe Lude.Text,
    -- | The ID of the Amazon Machine Image (AMI) to use to launch your EC2 instances. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/finding-an-ami.html Finding an AMI> in the /Amazon EC2 User Guide for Linux Instances/ .
    imageId :: Lude.Text,
    -- | The name of the launch configuration.
    launchConfigurationName :: Lude.Text,
    -- | The metadata options for the instances. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-launch-config.html#launch-configurations-imds Configuring the Instance Metadata Options> in the /Amazon EC2 Auto Scaling User Guide/ .
    metadataOptions :: Lude.Maybe InstanceMetadataOptions,
    -- | The Amazon Resource Name (ARN) of the launch configuration.
    launchConfigurationARN :: Lude.Maybe Lude.Text,
    -- | The tenancy of the instance, either @default@ or @dedicated@ . An instance with @dedicated@ tenancy runs on isolated, single-tenant hardware and can only be launched into a VPC.
    --
    -- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/auto-scaling-dedicated-instances.html Configuring instance tenancy with Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
    placementTenancy :: Lude.Maybe Lude.Text,
    -- | A block device mapping, which specifies the block devices for the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html Block Device Mapping> in the /Amazon EC2 User Guide for Linux Instances/ .
    blockDeviceMappings :: Lude.Maybe [BlockDeviceMapping]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LaunchConfiguration' with the minimum fields required to make a request.
--
-- * 'associatePublicIPAddress' - For Auto Scaling groups that are running in a VPC, specifies whether to assign a public IP address to the group's instances. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html Launching Auto Scaling instances in a VPC> in the /Amazon EC2 Auto Scaling User Guide/ .
-- * 'securityGroups' - A list that contains the security groups to assign to the instances in the Auto Scaling group. For more information, see <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_SecurityGroups.html Security Groups for Your VPC> in the /Amazon Virtual Private Cloud User Guide/ .
-- * 'spotPrice' - The maximum hourly price to be paid for any Spot Instance launched to fulfill the request. Spot Instances are launched when the price you specify exceeds the current Spot price. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-launch-spot-instances.html Requesting Spot Instances> in the /Amazon EC2 Auto Scaling User Guide/ .
-- * 'createdTime' - The creation date and time for the launch configuration.
-- * 'instanceMonitoring' - Controls whether instances in this group are launched with detailed (@true@ ) or basic (@false@ ) monitoring.
--
-- For more information, see <https://docs.aws.amazon.com/autoscaling/latest/userguide/enable-as-instance-metrics.html Configure Monitoring for Auto Scaling Instances> in the /Amazon EC2 Auto Scaling User Guide/ .
-- * 'keyName' - The name of the key pair.
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Amazon EC2 Key Pairs> in the /Amazon EC2 User Guide for Linux Instances/ .
-- * 'classicLinkVPCSecurityGroups' - The IDs of one or more security groups for the VPC specified in @ClassicLinkVPCId@ .
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink> in the /Amazon EC2 User Guide for Linux Instances/ and <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html#as-ClassicLink Linking EC2-Classic instances to a VPC> in the /Amazon EC2 Auto Scaling User Guide/ .
-- * 'ramdiskId' - The ID of the RAM disk associated with the AMI.
-- * 'kernelId' - The ID of the kernel associated with the AMI.
-- * 'instanceType' - The instance type for the instances.
--
-- For information about available instance types, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#AvailableInstanceTypes Available Instance Types> in the /Amazon EC2 User Guide for Linux Instances./
-- * 'ebsOptimized' - Specifies whether the launch configuration is optimized for EBS I/O (@true@ ) or not (@false@ ). For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSOptimized.html Amazon EBS-Optimized Instances> in the /Amazon EC2 User Guide for Linux Instances/ .
-- * 'userData' - The Base64-encoded user data to make available to the launched EC2 instances. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance metadata and user data> in the /Amazon EC2 User Guide for Linux Instances/ .
-- * 'classicLinkVPCId' - The ID of a ClassicLink-enabled VPC to link your EC2-Classic instances to. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink> in the /Amazon EC2 User Guide for Linux Instances/ and <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html#as-ClassicLink Linking EC2-Classic instances to a VPC> in the /Amazon EC2 Auto Scaling User Guide/ .
-- * 'iamInstanceProfile' - The name or the Amazon Resource Name (ARN) of the instance profile associated with the IAM role for the instance. The instance profile contains the IAM role. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/us-iam-role.html IAM role for applications that run on Amazon EC2 instances> in the /Amazon EC2 Auto Scaling User Guide/ .
-- * 'imageId' - The ID of the Amazon Machine Image (AMI) to use to launch your EC2 instances. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/finding-an-ami.html Finding an AMI> in the /Amazon EC2 User Guide for Linux Instances/ .
-- * 'launchConfigurationName' - The name of the launch configuration.
-- * 'metadataOptions' - The metadata options for the instances. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-launch-config.html#launch-configurations-imds Configuring the Instance Metadata Options> in the /Amazon EC2 Auto Scaling User Guide/ .
-- * 'launchConfigurationARN' - The Amazon Resource Name (ARN) of the launch configuration.
-- * 'placementTenancy' - The tenancy of the instance, either @default@ or @dedicated@ . An instance with @dedicated@ tenancy runs on isolated, single-tenant hardware and can only be launched into a VPC.
--
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/auto-scaling-dedicated-instances.html Configuring instance tenancy with Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
-- * 'blockDeviceMappings' - A block device mapping, which specifies the block devices for the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html Block Device Mapping> in the /Amazon EC2 User Guide for Linux Instances/ .
mkLaunchConfiguration ::
  -- | 'createdTime'
  Lude.DateTime ->
  -- | 'instanceType'
  Lude.Text ->
  -- | 'imageId'
  Lude.Text ->
  -- | 'launchConfigurationName'
  Lude.Text ->
  LaunchConfiguration
mkLaunchConfiguration
  pCreatedTime_
  pInstanceType_
  pImageId_
  pLaunchConfigurationName_ =
    LaunchConfiguration'
      { associatePublicIPAddress = Lude.Nothing,
        securityGroups = Lude.Nothing,
        spotPrice = Lude.Nothing,
        createdTime = pCreatedTime_,
        instanceMonitoring = Lude.Nothing,
        keyName = Lude.Nothing,
        classicLinkVPCSecurityGroups = Lude.Nothing,
        ramdiskId = Lude.Nothing,
        kernelId = Lude.Nothing,
        instanceType = pInstanceType_,
        ebsOptimized = Lude.Nothing,
        userData = Lude.Nothing,
        classicLinkVPCId = Lude.Nothing,
        iamInstanceProfile = Lude.Nothing,
        imageId = pImageId_,
        launchConfigurationName = pLaunchConfigurationName_,
        metadataOptions = Lude.Nothing,
        launchConfigurationARN = Lude.Nothing,
        placementTenancy = Lude.Nothing,
        blockDeviceMappings = Lude.Nothing
      }

-- | For Auto Scaling groups that are running in a VPC, specifies whether to assign a public IP address to the group's instances. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html Launching Auto Scaling instances in a VPC> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'associatePublicIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcAssociatePublicIPAddress :: Lens.Lens' LaunchConfiguration (Lude.Maybe Lude.Bool)
lcAssociatePublicIPAddress = Lens.lens (associatePublicIPAddress :: LaunchConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {associatePublicIPAddress = a} :: LaunchConfiguration)
{-# DEPRECATED lcAssociatePublicIPAddress "Use generic-lens or generic-optics with 'associatePublicIPAddress' instead." #-}

-- | A list that contains the security groups to assign to the instances in the Auto Scaling group. For more information, see <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_SecurityGroups.html Security Groups for Your VPC> in the /Amazon Virtual Private Cloud User Guide/ .
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcSecurityGroups :: Lens.Lens' LaunchConfiguration (Lude.Maybe [Lude.Text])
lcSecurityGroups = Lens.lens (securityGroups :: LaunchConfiguration -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroups = a} :: LaunchConfiguration)
{-# DEPRECATED lcSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | The maximum hourly price to be paid for any Spot Instance launched to fulfill the request. Spot Instances are launched when the price you specify exceeds the current Spot price. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-launch-spot-instances.html Requesting Spot Instances> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'spotPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcSpotPrice :: Lens.Lens' LaunchConfiguration (Lude.Maybe Lude.Text)
lcSpotPrice = Lens.lens (spotPrice :: LaunchConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {spotPrice = a} :: LaunchConfiguration)
{-# DEPRECATED lcSpotPrice "Use generic-lens or generic-optics with 'spotPrice' instead." #-}

-- | The creation date and time for the launch configuration.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcCreatedTime :: Lens.Lens' LaunchConfiguration Lude.DateTime
lcCreatedTime = Lens.lens (createdTime :: LaunchConfiguration -> Lude.DateTime) (\s a -> s {createdTime = a} :: LaunchConfiguration)
{-# DEPRECATED lcCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | Controls whether instances in this group are launched with detailed (@true@ ) or basic (@false@ ) monitoring.
--
-- For more information, see <https://docs.aws.amazon.com/autoscaling/latest/userguide/enable-as-instance-metrics.html Configure Monitoring for Auto Scaling Instances> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'instanceMonitoring' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcInstanceMonitoring :: Lens.Lens' LaunchConfiguration (Lude.Maybe InstanceMonitoring)
lcInstanceMonitoring = Lens.lens (instanceMonitoring :: LaunchConfiguration -> Lude.Maybe InstanceMonitoring) (\s a -> s {instanceMonitoring = a} :: LaunchConfiguration)
{-# DEPRECATED lcInstanceMonitoring "Use generic-lens or generic-optics with 'instanceMonitoring' instead." #-}

-- | The name of the key pair.
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Amazon EC2 Key Pairs> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- /Note:/ Consider using 'keyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcKeyName :: Lens.Lens' LaunchConfiguration (Lude.Maybe Lude.Text)
lcKeyName = Lens.lens (keyName :: LaunchConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {keyName = a} :: LaunchConfiguration)
{-# DEPRECATED lcKeyName "Use generic-lens or generic-optics with 'keyName' instead." #-}

-- | The IDs of one or more security groups for the VPC specified in @ClassicLinkVPCId@ .
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink> in the /Amazon EC2 User Guide for Linux Instances/ and <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html#as-ClassicLink Linking EC2-Classic instances to a VPC> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'classicLinkVPCSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcClassicLinkVPCSecurityGroups :: Lens.Lens' LaunchConfiguration (Lude.Maybe [Lude.Text])
lcClassicLinkVPCSecurityGroups = Lens.lens (classicLinkVPCSecurityGroups :: LaunchConfiguration -> Lude.Maybe [Lude.Text]) (\s a -> s {classicLinkVPCSecurityGroups = a} :: LaunchConfiguration)
{-# DEPRECATED lcClassicLinkVPCSecurityGroups "Use generic-lens or generic-optics with 'classicLinkVPCSecurityGroups' instead." #-}

-- | The ID of the RAM disk associated with the AMI.
--
-- /Note:/ Consider using 'ramdiskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcRAMDiskId :: Lens.Lens' LaunchConfiguration (Lude.Maybe Lude.Text)
lcRAMDiskId = Lens.lens (ramdiskId :: LaunchConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {ramdiskId = a} :: LaunchConfiguration)
{-# DEPRECATED lcRAMDiskId "Use generic-lens or generic-optics with 'ramdiskId' instead." #-}

-- | The ID of the kernel associated with the AMI.
--
-- /Note:/ Consider using 'kernelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcKernelId :: Lens.Lens' LaunchConfiguration (Lude.Maybe Lude.Text)
lcKernelId = Lens.lens (kernelId :: LaunchConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {kernelId = a} :: LaunchConfiguration)
{-# DEPRECATED lcKernelId "Use generic-lens or generic-optics with 'kernelId' instead." #-}

-- | The instance type for the instances.
--
-- For information about available instance types, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#AvailableInstanceTypes Available Instance Types> in the /Amazon EC2 User Guide for Linux Instances./
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcInstanceType :: Lens.Lens' LaunchConfiguration Lude.Text
lcInstanceType = Lens.lens (instanceType :: LaunchConfiguration -> Lude.Text) (\s a -> s {instanceType = a} :: LaunchConfiguration)
{-# DEPRECATED lcInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | Specifies whether the launch configuration is optimized for EBS I/O (@true@ ) or not (@false@ ). For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSOptimized.html Amazon EBS-Optimized Instances> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- /Note:/ Consider using 'ebsOptimized' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcEBSOptimized :: Lens.Lens' LaunchConfiguration (Lude.Maybe Lude.Bool)
lcEBSOptimized = Lens.lens (ebsOptimized :: LaunchConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {ebsOptimized = a} :: LaunchConfiguration)
{-# DEPRECATED lcEBSOptimized "Use generic-lens or generic-optics with 'ebsOptimized' instead." #-}

-- | The Base64-encoded user data to make available to the launched EC2 instances. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance metadata and user data> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- /Note:/ Consider using 'userData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcUserData :: Lens.Lens' LaunchConfiguration (Lude.Maybe Lude.Text)
lcUserData = Lens.lens (userData :: LaunchConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {userData = a} :: LaunchConfiguration)
{-# DEPRECATED lcUserData "Use generic-lens or generic-optics with 'userData' instead." #-}

-- | The ID of a ClassicLink-enabled VPC to link your EC2-Classic instances to. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink> in the /Amazon EC2 User Guide for Linux Instances/ and <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html#as-ClassicLink Linking EC2-Classic instances to a VPC> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'classicLinkVPCId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcClassicLinkVPCId :: Lens.Lens' LaunchConfiguration (Lude.Maybe Lude.Text)
lcClassicLinkVPCId = Lens.lens (classicLinkVPCId :: LaunchConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {classicLinkVPCId = a} :: LaunchConfiguration)
{-# DEPRECATED lcClassicLinkVPCId "Use generic-lens or generic-optics with 'classicLinkVPCId' instead." #-}

-- | The name or the Amazon Resource Name (ARN) of the instance profile associated with the IAM role for the instance. The instance profile contains the IAM role. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/us-iam-role.html IAM role for applications that run on Amazon EC2 instances> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'iamInstanceProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcIAMInstanceProfile :: Lens.Lens' LaunchConfiguration (Lude.Maybe Lude.Text)
lcIAMInstanceProfile = Lens.lens (iamInstanceProfile :: LaunchConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {iamInstanceProfile = a} :: LaunchConfiguration)
{-# DEPRECATED lcIAMInstanceProfile "Use generic-lens or generic-optics with 'iamInstanceProfile' instead." #-}

-- | The ID of the Amazon Machine Image (AMI) to use to launch your EC2 instances. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/finding-an-ami.html Finding an AMI> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcImageId :: Lens.Lens' LaunchConfiguration Lude.Text
lcImageId = Lens.lens (imageId :: LaunchConfiguration -> Lude.Text) (\s a -> s {imageId = a} :: LaunchConfiguration)
{-# DEPRECATED lcImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | The name of the launch configuration.
--
-- /Note:/ Consider using 'launchConfigurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcLaunchConfigurationName :: Lens.Lens' LaunchConfiguration Lude.Text
lcLaunchConfigurationName = Lens.lens (launchConfigurationName :: LaunchConfiguration -> Lude.Text) (\s a -> s {launchConfigurationName = a} :: LaunchConfiguration)
{-# DEPRECATED lcLaunchConfigurationName "Use generic-lens or generic-optics with 'launchConfigurationName' instead." #-}

-- | The metadata options for the instances. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-launch-config.html#launch-configurations-imds Configuring the Instance Metadata Options> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'metadataOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcMetadataOptions :: Lens.Lens' LaunchConfiguration (Lude.Maybe InstanceMetadataOptions)
lcMetadataOptions = Lens.lens (metadataOptions :: LaunchConfiguration -> Lude.Maybe InstanceMetadataOptions) (\s a -> s {metadataOptions = a} :: LaunchConfiguration)
{-# DEPRECATED lcMetadataOptions "Use generic-lens or generic-optics with 'metadataOptions' instead." #-}

-- | The Amazon Resource Name (ARN) of the launch configuration.
--
-- /Note:/ Consider using 'launchConfigurationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcLaunchConfigurationARN :: Lens.Lens' LaunchConfiguration (Lude.Maybe Lude.Text)
lcLaunchConfigurationARN = Lens.lens (launchConfigurationARN :: LaunchConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {launchConfigurationARN = a} :: LaunchConfiguration)
{-# DEPRECATED lcLaunchConfigurationARN "Use generic-lens or generic-optics with 'launchConfigurationARN' instead." #-}

-- | The tenancy of the instance, either @default@ or @dedicated@ . An instance with @dedicated@ tenancy runs on isolated, single-tenant hardware and can only be launched into a VPC.
--
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/auto-scaling-dedicated-instances.html Configuring instance tenancy with Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'placementTenancy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcPlacementTenancy :: Lens.Lens' LaunchConfiguration (Lude.Maybe Lude.Text)
lcPlacementTenancy = Lens.lens (placementTenancy :: LaunchConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {placementTenancy = a} :: LaunchConfiguration)
{-# DEPRECATED lcPlacementTenancy "Use generic-lens or generic-optics with 'placementTenancy' instead." #-}

-- | A block device mapping, which specifies the block devices for the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html Block Device Mapping> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- /Note:/ Consider using 'blockDeviceMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcBlockDeviceMappings :: Lens.Lens' LaunchConfiguration (Lude.Maybe [BlockDeviceMapping])
lcBlockDeviceMappings = Lens.lens (blockDeviceMappings :: LaunchConfiguration -> Lude.Maybe [BlockDeviceMapping]) (\s a -> s {blockDeviceMappings = a} :: LaunchConfiguration)
{-# DEPRECATED lcBlockDeviceMappings "Use generic-lens or generic-optics with 'blockDeviceMappings' instead." #-}

instance Lude.FromXML LaunchConfiguration where
  parseXML x =
    LaunchConfiguration'
      Lude.<$> (x Lude..@? "AssociatePublicIpAddress")
      Lude.<*> ( x Lude..@? "SecurityGroups" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "SpotPrice")
      Lude.<*> (x Lude..@ "CreatedTime")
      Lude.<*> (x Lude..@? "InstanceMonitoring")
      Lude.<*> (x Lude..@? "KeyName")
      Lude.<*> ( x Lude..@? "ClassicLinkVPCSecurityGroups" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "RamdiskId")
      Lude.<*> (x Lude..@? "KernelId")
      Lude.<*> (x Lude..@ "InstanceType")
      Lude.<*> (x Lude..@? "EbsOptimized")
      Lude.<*> (x Lude..@? "UserData")
      Lude.<*> (x Lude..@? "ClassicLinkVPCId")
      Lude.<*> (x Lude..@? "IamInstanceProfile")
      Lude.<*> (x Lude..@ "ImageId")
      Lude.<*> (x Lude..@ "LaunchConfigurationName")
      Lude.<*> (x Lude..@? "MetadataOptions")
      Lude.<*> (x Lude..@? "LaunchConfigurationARN")
      Lude.<*> (x Lude..@? "PlacementTenancy")
      Lude.<*> ( x Lude..@? "BlockDeviceMappings" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
