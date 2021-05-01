{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.CreateLaunchConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a launch configuration.
--
-- If you exceed your maximum limit of launch configurations, the call
-- fails. To query this limit, call the DescribeAccountLimits API. For
-- information about updating this limit, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-account-limits.html Amazon EC2 Auto Scaling service quotas>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/LaunchConfiguration.html Launch configurations>
-- in the /Amazon EC2 Auto Scaling User Guide/.
module Network.AWS.AutoScaling.CreateLaunchConfiguration
  ( -- * Creating a Request
    CreateLaunchConfiguration (..),
    newCreateLaunchConfiguration,

    -- * Request Lenses
    createLaunchConfiguration_instanceId,
    createLaunchConfiguration_instanceType,
    createLaunchConfiguration_ebsOptimized,
    createLaunchConfiguration_userData,
    createLaunchConfiguration_ramdiskId,
    createLaunchConfiguration_classicLinkVPCSecurityGroups,
    createLaunchConfiguration_spotPrice,
    createLaunchConfiguration_imageId,
    createLaunchConfiguration_associatePublicIpAddress,
    createLaunchConfiguration_securityGroups,
    createLaunchConfiguration_iamInstanceProfile,
    createLaunchConfiguration_classicLinkVPCId,
    createLaunchConfiguration_blockDeviceMappings,
    createLaunchConfiguration_kernelId,
    createLaunchConfiguration_placementTenancy,
    createLaunchConfiguration_keyName,
    createLaunchConfiguration_instanceMonitoring,
    createLaunchConfiguration_metadataOptions,
    createLaunchConfiguration_launchConfigurationName,

    -- * Destructuring the Response
    CreateLaunchConfigurationResponse (..),
    newCreateLaunchConfigurationResponse,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateLaunchConfiguration' smart constructor.
data CreateLaunchConfiguration = CreateLaunchConfiguration'
  { -- | The ID of the instance to use to create the launch configuration. The
    -- new launch configuration derives attributes from the instance, except
    -- for the block device mapping.
    --
    -- To create a launch configuration with a block device mapping or override
    -- any other instance attributes, specify them as part of the same request.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-lc-with-instanceID.html Creating a launch configuration using an EC2 instance>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    --
    -- If you do not specify @InstanceId@, you must specify both @ImageId@ and
    -- @InstanceType@.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the instance type of the EC2 instance.
    --
    -- For information about available instance types, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#AvailableInstanceTypes Available Instance Types>
    -- in the /Amazon EC2 User Guide for Linux Instances/.
    --
    -- If you do not specify @InstanceId@, you must specify @InstanceType@.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the launch configuration is optimized for EBS I\/O
    -- (@true@) or not (@false@). The optimization provides dedicated
    -- throughput to Amazon EBS and an optimized configuration stack to provide
    -- optimal I\/O performance. This optimization is not available with all
    -- instance types. Additional fees are incurred when you enable EBS
    -- optimization for an instance type that is not EBS-optimized by default.
    -- For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSOptimized.html Amazon EBS-Optimized Instances>
    -- in the /Amazon EC2 User Guide for Linux Instances/.
    --
    -- The default value is @false@.
    ebsOptimized :: Prelude.Maybe Prelude.Bool,
    -- | The user data to make available to the launched EC2 instances. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance metadata and user data>
    -- (Linux) and
    -- <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ec2-instance-metadata.html Instance metadata and user data>
    -- (Windows). If you are using a command line tool, base64-encoding is
    -- performed for you, and you can load the text from a file. Otherwise, you
    -- must provide base64-encoded text. User data is limited to 16 KB.
    userData :: Prelude.Maybe Prelude.Text,
    -- | The ID of the RAM disk to select.
    ramdiskId :: Prelude.Maybe Prelude.Text,
    -- | The IDs of one or more security groups for the specified
    -- ClassicLink-enabled VPC. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink>
    -- in the /Amazon EC2 User Guide for Linux Instances/ and
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html#as-ClassicLink Linking EC2-Classic instances to a VPC>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    --
    -- If you specify the @ClassicLinkVPCId@ parameter, you must specify this
    -- parameter.
    classicLinkVPCSecurityGroups :: Prelude.Maybe [Prelude.Text],
    -- | The maximum hourly price to be paid for any Spot Instance launched to
    -- fulfill the request. Spot Instances are launched when the price you
    -- specify exceeds the current Spot price. For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-launch-spot-instances.html Requesting Spot Instances>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    --
    -- When you change your maximum price by creating a new launch
    -- configuration, running instances will continue to run as long as the
    -- maximum price for those running instances is higher than the current
    -- Spot price.
    spotPrice :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Machine Image (AMI) that was assigned during
    -- registration. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/finding-an-ami.html Finding an AMI>
    -- in the /Amazon EC2 User Guide for Linux Instances/.
    --
    -- If you do not specify @InstanceId@, you must specify @ImageId@.
    imageId :: Prelude.Maybe Prelude.Text,
    -- | For Auto Scaling groups that are running in a virtual private cloud
    -- (VPC), specifies whether to assign a public IP address to the group\'s
    -- instances. If you specify @true@, each instance in the Auto Scaling
    -- group receives a unique public IP address. For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html Launching Auto Scaling instances in a VPC>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    --
    -- If you specify this parameter, you must specify at least one subnet for
    -- @VPCZoneIdentifier@ when you create your group.
    --
    -- If the instance is launched into a default subnet, the default is to
    -- assign a public IP address, unless you disabled the option to assign a
    -- public IP address on the subnet. If the instance is launched into a
    -- nondefault subnet, the default is not to assign a public IP address,
    -- unless you enabled the option to assign a public IP address on the
    -- subnet.
    associatePublicIpAddress :: Prelude.Maybe Prelude.Bool,
    -- | A list that contains the security groups to assign to the instances in
    -- the Auto Scaling group.
    --
    -- [EC2-VPC] Specify the security group IDs. For more information, see
    -- <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_SecurityGroups.html Security Groups for Your VPC>
    -- in the /Amazon Virtual Private Cloud User Guide/.
    --
    -- [EC2-Classic] Specify either the security group names or the security
    -- group IDs. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-network-security.html Amazon EC2 Security Groups>
    -- in the /Amazon EC2 User Guide for Linux Instances/.
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | The name or the Amazon Resource Name (ARN) of the instance profile
    -- associated with the IAM role for the instance. The instance profile
    -- contains the IAM role.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/us-iam-role.html IAM role for applications that run on Amazon EC2 instances>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    iamInstanceProfile :: Prelude.Maybe Prelude.Text,
    -- | The ID of a ClassicLink-enabled VPC to link your EC2-Classic instances
    -- to. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink>
    -- in the /Amazon EC2 User Guide for Linux Instances/ and
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html#as-ClassicLink Linking EC2-Classic instances to a VPC>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    --
    -- This parameter can only be used if you are launching EC2-Classic
    -- instances.
    classicLinkVPCId :: Prelude.Maybe Prelude.Text,
    -- | A block device mapping, which specifies the block devices for the
    -- instance. You can specify virtual devices and EBS volumes. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html Block Device Mapping>
    -- in the /Amazon EC2 User Guide for Linux Instances/.
    blockDeviceMappings :: Prelude.Maybe [BlockDeviceMapping],
    -- | The ID of the kernel associated with the AMI.
    kernelId :: Prelude.Maybe Prelude.Text,
    -- | The tenancy of the instance. An instance with @dedicated@ tenancy runs
    -- on isolated, single-tenant hardware and can only be launched into a VPC.
    --
    -- To launch dedicated instances into a shared tenancy VPC (a VPC with the
    -- instance placement tenancy attribute set to @default@), you must set the
    -- value of this parameter to @dedicated@.
    --
    -- If you specify @PlacementTenancy@, you must specify at least one subnet
    -- for @VPCZoneIdentifier@ when you create your group.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/auto-scaling-dedicated-instances.html Configuring instance tenancy with Amazon EC2 Auto Scaling>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    --
    -- Valid Values: @default@ | @dedicated@
    placementTenancy :: Prelude.Maybe Prelude.Text,
    -- | The name of the key pair. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Amazon EC2 Key Pairs>
    -- in the /Amazon EC2 User Guide for Linux Instances/.
    keyName :: Prelude.Maybe Prelude.Text,
    -- | Controls whether instances in this group are launched with detailed
    -- (@true@) or basic (@false@) monitoring.
    --
    -- The default value is @true@ (enabled).
    --
    -- When detailed monitoring is enabled, Amazon CloudWatch generates metrics
    -- every minute and your account is charged a fee. When you disable
    -- detailed monitoring, CloudWatch generates metrics every 5 minutes. For
    -- more information, see
    -- <https://docs.aws.amazon.com/autoscaling/latest/userguide/enable-as-instance-metrics.html Configure Monitoring for Auto Scaling Instances>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    instanceMonitoring :: Prelude.Maybe InstanceMonitoring,
    -- | The metadata options for the instances. For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-launch-config.html#launch-configurations-imds Configuring the Instance Metadata Options>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    metadataOptions :: Prelude.Maybe InstanceMetadataOptions,
    -- | The name of the launch configuration. This name must be unique per
    -- Region per account.
    launchConfigurationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateLaunchConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'createLaunchConfiguration_instanceId' - The ID of the instance to use to create the launch configuration. The
-- new launch configuration derives attributes from the instance, except
-- for the block device mapping.
--
-- To create a launch configuration with a block device mapping or override
-- any other instance attributes, specify them as part of the same request.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-lc-with-instanceID.html Creating a launch configuration using an EC2 instance>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- If you do not specify @InstanceId@, you must specify both @ImageId@ and
-- @InstanceType@.
--
-- 'instanceType', 'createLaunchConfiguration_instanceType' - Specifies the instance type of the EC2 instance.
--
-- For information about available instance types, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#AvailableInstanceTypes Available Instance Types>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- If you do not specify @InstanceId@, you must specify @InstanceType@.
--
-- 'ebsOptimized', 'createLaunchConfiguration_ebsOptimized' - Specifies whether the launch configuration is optimized for EBS I\/O
-- (@true@) or not (@false@). The optimization provides dedicated
-- throughput to Amazon EBS and an optimized configuration stack to provide
-- optimal I\/O performance. This optimization is not available with all
-- instance types. Additional fees are incurred when you enable EBS
-- optimization for an instance type that is not EBS-optimized by default.
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSOptimized.html Amazon EBS-Optimized Instances>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- The default value is @false@.
--
-- 'userData', 'createLaunchConfiguration_userData' - The user data to make available to the launched EC2 instances. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance metadata and user data>
-- (Linux) and
-- <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ec2-instance-metadata.html Instance metadata and user data>
-- (Windows). If you are using a command line tool, base64-encoding is
-- performed for you, and you can load the text from a file. Otherwise, you
-- must provide base64-encoded text. User data is limited to 16 KB.
--
-- 'ramdiskId', 'createLaunchConfiguration_ramdiskId' - The ID of the RAM disk to select.
--
-- 'classicLinkVPCSecurityGroups', 'createLaunchConfiguration_classicLinkVPCSecurityGroups' - The IDs of one or more security groups for the specified
-- ClassicLink-enabled VPC. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink>
-- in the /Amazon EC2 User Guide for Linux Instances/ and
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html#as-ClassicLink Linking EC2-Classic instances to a VPC>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- If you specify the @ClassicLinkVPCId@ parameter, you must specify this
-- parameter.
--
-- 'spotPrice', 'createLaunchConfiguration_spotPrice' - The maximum hourly price to be paid for any Spot Instance launched to
-- fulfill the request. Spot Instances are launched when the price you
-- specify exceeds the current Spot price. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-launch-spot-instances.html Requesting Spot Instances>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- When you change your maximum price by creating a new launch
-- configuration, running instances will continue to run as long as the
-- maximum price for those running instances is higher than the current
-- Spot price.
--
-- 'imageId', 'createLaunchConfiguration_imageId' - The ID of the Amazon Machine Image (AMI) that was assigned during
-- registration. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/finding-an-ami.html Finding an AMI>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- If you do not specify @InstanceId@, you must specify @ImageId@.
--
-- 'associatePublicIpAddress', 'createLaunchConfiguration_associatePublicIpAddress' - For Auto Scaling groups that are running in a virtual private cloud
-- (VPC), specifies whether to assign a public IP address to the group\'s
-- instances. If you specify @true@, each instance in the Auto Scaling
-- group receives a unique public IP address. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html Launching Auto Scaling instances in a VPC>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- If you specify this parameter, you must specify at least one subnet for
-- @VPCZoneIdentifier@ when you create your group.
--
-- If the instance is launched into a default subnet, the default is to
-- assign a public IP address, unless you disabled the option to assign a
-- public IP address on the subnet. If the instance is launched into a
-- nondefault subnet, the default is not to assign a public IP address,
-- unless you enabled the option to assign a public IP address on the
-- subnet.
--
-- 'securityGroups', 'createLaunchConfiguration_securityGroups' - A list that contains the security groups to assign to the instances in
-- the Auto Scaling group.
--
-- [EC2-VPC] Specify the security group IDs. For more information, see
-- <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_SecurityGroups.html Security Groups for Your VPC>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- [EC2-Classic] Specify either the security group names or the security
-- group IDs. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-network-security.html Amazon EC2 Security Groups>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- 'iamInstanceProfile', 'createLaunchConfiguration_iamInstanceProfile' - The name or the Amazon Resource Name (ARN) of the instance profile
-- associated with the IAM role for the instance. The instance profile
-- contains the IAM role.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/us-iam-role.html IAM role for applications that run on Amazon EC2 instances>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'classicLinkVPCId', 'createLaunchConfiguration_classicLinkVPCId' - The ID of a ClassicLink-enabled VPC to link your EC2-Classic instances
-- to. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink>
-- in the /Amazon EC2 User Guide for Linux Instances/ and
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html#as-ClassicLink Linking EC2-Classic instances to a VPC>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- This parameter can only be used if you are launching EC2-Classic
-- instances.
--
-- 'blockDeviceMappings', 'createLaunchConfiguration_blockDeviceMappings' - A block device mapping, which specifies the block devices for the
-- instance. You can specify virtual devices and EBS volumes. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html Block Device Mapping>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- 'kernelId', 'createLaunchConfiguration_kernelId' - The ID of the kernel associated with the AMI.
--
-- 'placementTenancy', 'createLaunchConfiguration_placementTenancy' - The tenancy of the instance. An instance with @dedicated@ tenancy runs
-- on isolated, single-tenant hardware and can only be launched into a VPC.
--
-- To launch dedicated instances into a shared tenancy VPC (a VPC with the
-- instance placement tenancy attribute set to @default@), you must set the
-- value of this parameter to @dedicated@.
--
-- If you specify @PlacementTenancy@, you must specify at least one subnet
-- for @VPCZoneIdentifier@ when you create your group.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/auto-scaling-dedicated-instances.html Configuring instance tenancy with Amazon EC2 Auto Scaling>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- Valid Values: @default@ | @dedicated@
--
-- 'keyName', 'createLaunchConfiguration_keyName' - The name of the key pair. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Amazon EC2 Key Pairs>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- 'instanceMonitoring', 'createLaunchConfiguration_instanceMonitoring' - Controls whether instances in this group are launched with detailed
-- (@true@) or basic (@false@) monitoring.
--
-- The default value is @true@ (enabled).
--
-- When detailed monitoring is enabled, Amazon CloudWatch generates metrics
-- every minute and your account is charged a fee. When you disable
-- detailed monitoring, CloudWatch generates metrics every 5 minutes. For
-- more information, see
-- <https://docs.aws.amazon.com/autoscaling/latest/userguide/enable-as-instance-metrics.html Configure Monitoring for Auto Scaling Instances>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'metadataOptions', 'createLaunchConfiguration_metadataOptions' - The metadata options for the instances. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-launch-config.html#launch-configurations-imds Configuring the Instance Metadata Options>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'launchConfigurationName', 'createLaunchConfiguration_launchConfigurationName' - The name of the launch configuration. This name must be unique per
-- Region per account.
newCreateLaunchConfiguration ::
  -- | 'launchConfigurationName'
  Prelude.Text ->
  CreateLaunchConfiguration
newCreateLaunchConfiguration
  pLaunchConfigurationName_ =
    CreateLaunchConfiguration'
      { instanceId =
          Prelude.Nothing,
        instanceType = Prelude.Nothing,
        ebsOptimized = Prelude.Nothing,
        userData = Prelude.Nothing,
        ramdiskId = Prelude.Nothing,
        classicLinkVPCSecurityGroups = Prelude.Nothing,
        spotPrice = Prelude.Nothing,
        imageId = Prelude.Nothing,
        associatePublicIpAddress = Prelude.Nothing,
        securityGroups = Prelude.Nothing,
        iamInstanceProfile = Prelude.Nothing,
        classicLinkVPCId = Prelude.Nothing,
        blockDeviceMappings = Prelude.Nothing,
        kernelId = Prelude.Nothing,
        placementTenancy = Prelude.Nothing,
        keyName = Prelude.Nothing,
        instanceMonitoring = Prelude.Nothing,
        metadataOptions = Prelude.Nothing,
        launchConfigurationName =
          pLaunchConfigurationName_
      }

-- | The ID of the instance to use to create the launch configuration. The
-- new launch configuration derives attributes from the instance, except
-- for the block device mapping.
--
-- To create a launch configuration with a block device mapping or override
-- any other instance attributes, specify them as part of the same request.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-lc-with-instanceID.html Creating a launch configuration using an EC2 instance>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- If you do not specify @InstanceId@, you must specify both @ImageId@ and
-- @InstanceType@.
createLaunchConfiguration_instanceId :: Lens.Lens' CreateLaunchConfiguration (Prelude.Maybe Prelude.Text)
createLaunchConfiguration_instanceId = Lens.lens (\CreateLaunchConfiguration' {instanceId} -> instanceId) (\s@CreateLaunchConfiguration' {} a -> s {instanceId = a} :: CreateLaunchConfiguration)

-- | Specifies the instance type of the EC2 instance.
--
-- For information about available instance types, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#AvailableInstanceTypes Available Instance Types>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- If you do not specify @InstanceId@, you must specify @InstanceType@.
createLaunchConfiguration_instanceType :: Lens.Lens' CreateLaunchConfiguration (Prelude.Maybe Prelude.Text)
createLaunchConfiguration_instanceType = Lens.lens (\CreateLaunchConfiguration' {instanceType} -> instanceType) (\s@CreateLaunchConfiguration' {} a -> s {instanceType = a} :: CreateLaunchConfiguration)

-- | Specifies whether the launch configuration is optimized for EBS I\/O
-- (@true@) or not (@false@). The optimization provides dedicated
-- throughput to Amazon EBS and an optimized configuration stack to provide
-- optimal I\/O performance. This optimization is not available with all
-- instance types. Additional fees are incurred when you enable EBS
-- optimization for an instance type that is not EBS-optimized by default.
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSOptimized.html Amazon EBS-Optimized Instances>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- The default value is @false@.
createLaunchConfiguration_ebsOptimized :: Lens.Lens' CreateLaunchConfiguration (Prelude.Maybe Prelude.Bool)
createLaunchConfiguration_ebsOptimized = Lens.lens (\CreateLaunchConfiguration' {ebsOptimized} -> ebsOptimized) (\s@CreateLaunchConfiguration' {} a -> s {ebsOptimized = a} :: CreateLaunchConfiguration)

-- | The user data to make available to the launched EC2 instances. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance metadata and user data>
-- (Linux) and
-- <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ec2-instance-metadata.html Instance metadata and user data>
-- (Windows). If you are using a command line tool, base64-encoding is
-- performed for you, and you can load the text from a file. Otherwise, you
-- must provide base64-encoded text. User data is limited to 16 KB.
createLaunchConfiguration_userData :: Lens.Lens' CreateLaunchConfiguration (Prelude.Maybe Prelude.Text)
createLaunchConfiguration_userData = Lens.lens (\CreateLaunchConfiguration' {userData} -> userData) (\s@CreateLaunchConfiguration' {} a -> s {userData = a} :: CreateLaunchConfiguration)

-- | The ID of the RAM disk to select.
createLaunchConfiguration_ramdiskId :: Lens.Lens' CreateLaunchConfiguration (Prelude.Maybe Prelude.Text)
createLaunchConfiguration_ramdiskId = Lens.lens (\CreateLaunchConfiguration' {ramdiskId} -> ramdiskId) (\s@CreateLaunchConfiguration' {} a -> s {ramdiskId = a} :: CreateLaunchConfiguration)

-- | The IDs of one or more security groups for the specified
-- ClassicLink-enabled VPC. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink>
-- in the /Amazon EC2 User Guide for Linux Instances/ and
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html#as-ClassicLink Linking EC2-Classic instances to a VPC>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- If you specify the @ClassicLinkVPCId@ parameter, you must specify this
-- parameter.
createLaunchConfiguration_classicLinkVPCSecurityGroups :: Lens.Lens' CreateLaunchConfiguration (Prelude.Maybe [Prelude.Text])
createLaunchConfiguration_classicLinkVPCSecurityGroups = Lens.lens (\CreateLaunchConfiguration' {classicLinkVPCSecurityGroups} -> classicLinkVPCSecurityGroups) (\s@CreateLaunchConfiguration' {} a -> s {classicLinkVPCSecurityGroups = a} :: CreateLaunchConfiguration) Prelude.. Lens.mapping Prelude._Coerce

-- | The maximum hourly price to be paid for any Spot Instance launched to
-- fulfill the request. Spot Instances are launched when the price you
-- specify exceeds the current Spot price. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-launch-spot-instances.html Requesting Spot Instances>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- When you change your maximum price by creating a new launch
-- configuration, running instances will continue to run as long as the
-- maximum price for those running instances is higher than the current
-- Spot price.
createLaunchConfiguration_spotPrice :: Lens.Lens' CreateLaunchConfiguration (Prelude.Maybe Prelude.Text)
createLaunchConfiguration_spotPrice = Lens.lens (\CreateLaunchConfiguration' {spotPrice} -> spotPrice) (\s@CreateLaunchConfiguration' {} a -> s {spotPrice = a} :: CreateLaunchConfiguration)

-- | The ID of the Amazon Machine Image (AMI) that was assigned during
-- registration. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/finding-an-ami.html Finding an AMI>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- If you do not specify @InstanceId@, you must specify @ImageId@.
createLaunchConfiguration_imageId :: Lens.Lens' CreateLaunchConfiguration (Prelude.Maybe Prelude.Text)
createLaunchConfiguration_imageId = Lens.lens (\CreateLaunchConfiguration' {imageId} -> imageId) (\s@CreateLaunchConfiguration' {} a -> s {imageId = a} :: CreateLaunchConfiguration)

-- | For Auto Scaling groups that are running in a virtual private cloud
-- (VPC), specifies whether to assign a public IP address to the group\'s
-- instances. If you specify @true@, each instance in the Auto Scaling
-- group receives a unique public IP address. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html Launching Auto Scaling instances in a VPC>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- If you specify this parameter, you must specify at least one subnet for
-- @VPCZoneIdentifier@ when you create your group.
--
-- If the instance is launched into a default subnet, the default is to
-- assign a public IP address, unless you disabled the option to assign a
-- public IP address on the subnet. If the instance is launched into a
-- nondefault subnet, the default is not to assign a public IP address,
-- unless you enabled the option to assign a public IP address on the
-- subnet.
createLaunchConfiguration_associatePublicIpAddress :: Lens.Lens' CreateLaunchConfiguration (Prelude.Maybe Prelude.Bool)
createLaunchConfiguration_associatePublicIpAddress = Lens.lens (\CreateLaunchConfiguration' {associatePublicIpAddress} -> associatePublicIpAddress) (\s@CreateLaunchConfiguration' {} a -> s {associatePublicIpAddress = a} :: CreateLaunchConfiguration)

-- | A list that contains the security groups to assign to the instances in
-- the Auto Scaling group.
--
-- [EC2-VPC] Specify the security group IDs. For more information, see
-- <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_SecurityGroups.html Security Groups for Your VPC>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- [EC2-Classic] Specify either the security group names or the security
-- group IDs. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-network-security.html Amazon EC2 Security Groups>
-- in the /Amazon EC2 User Guide for Linux Instances/.
createLaunchConfiguration_securityGroups :: Lens.Lens' CreateLaunchConfiguration (Prelude.Maybe [Prelude.Text])
createLaunchConfiguration_securityGroups = Lens.lens (\CreateLaunchConfiguration' {securityGroups} -> securityGroups) (\s@CreateLaunchConfiguration' {} a -> s {securityGroups = a} :: CreateLaunchConfiguration) Prelude.. Lens.mapping Prelude._Coerce

-- | The name or the Amazon Resource Name (ARN) of the instance profile
-- associated with the IAM role for the instance. The instance profile
-- contains the IAM role.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/us-iam-role.html IAM role for applications that run on Amazon EC2 instances>
-- in the /Amazon EC2 Auto Scaling User Guide/.
createLaunchConfiguration_iamInstanceProfile :: Lens.Lens' CreateLaunchConfiguration (Prelude.Maybe Prelude.Text)
createLaunchConfiguration_iamInstanceProfile = Lens.lens (\CreateLaunchConfiguration' {iamInstanceProfile} -> iamInstanceProfile) (\s@CreateLaunchConfiguration' {} a -> s {iamInstanceProfile = a} :: CreateLaunchConfiguration)

-- | The ID of a ClassicLink-enabled VPC to link your EC2-Classic instances
-- to. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink>
-- in the /Amazon EC2 User Guide for Linux Instances/ and
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html#as-ClassicLink Linking EC2-Classic instances to a VPC>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- This parameter can only be used if you are launching EC2-Classic
-- instances.
createLaunchConfiguration_classicLinkVPCId :: Lens.Lens' CreateLaunchConfiguration (Prelude.Maybe Prelude.Text)
createLaunchConfiguration_classicLinkVPCId = Lens.lens (\CreateLaunchConfiguration' {classicLinkVPCId} -> classicLinkVPCId) (\s@CreateLaunchConfiguration' {} a -> s {classicLinkVPCId = a} :: CreateLaunchConfiguration)

-- | A block device mapping, which specifies the block devices for the
-- instance. You can specify virtual devices and EBS volumes. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html Block Device Mapping>
-- in the /Amazon EC2 User Guide for Linux Instances/.
createLaunchConfiguration_blockDeviceMappings :: Lens.Lens' CreateLaunchConfiguration (Prelude.Maybe [BlockDeviceMapping])
createLaunchConfiguration_blockDeviceMappings = Lens.lens (\CreateLaunchConfiguration' {blockDeviceMappings} -> blockDeviceMappings) (\s@CreateLaunchConfiguration' {} a -> s {blockDeviceMappings = a} :: CreateLaunchConfiguration) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of the kernel associated with the AMI.
createLaunchConfiguration_kernelId :: Lens.Lens' CreateLaunchConfiguration (Prelude.Maybe Prelude.Text)
createLaunchConfiguration_kernelId = Lens.lens (\CreateLaunchConfiguration' {kernelId} -> kernelId) (\s@CreateLaunchConfiguration' {} a -> s {kernelId = a} :: CreateLaunchConfiguration)

-- | The tenancy of the instance. An instance with @dedicated@ tenancy runs
-- on isolated, single-tenant hardware and can only be launched into a VPC.
--
-- To launch dedicated instances into a shared tenancy VPC (a VPC with the
-- instance placement tenancy attribute set to @default@), you must set the
-- value of this parameter to @dedicated@.
--
-- If you specify @PlacementTenancy@, you must specify at least one subnet
-- for @VPCZoneIdentifier@ when you create your group.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/auto-scaling-dedicated-instances.html Configuring instance tenancy with Amazon EC2 Auto Scaling>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- Valid Values: @default@ | @dedicated@
createLaunchConfiguration_placementTenancy :: Lens.Lens' CreateLaunchConfiguration (Prelude.Maybe Prelude.Text)
createLaunchConfiguration_placementTenancy = Lens.lens (\CreateLaunchConfiguration' {placementTenancy} -> placementTenancy) (\s@CreateLaunchConfiguration' {} a -> s {placementTenancy = a} :: CreateLaunchConfiguration)

-- | The name of the key pair. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Amazon EC2 Key Pairs>
-- in the /Amazon EC2 User Guide for Linux Instances/.
createLaunchConfiguration_keyName :: Lens.Lens' CreateLaunchConfiguration (Prelude.Maybe Prelude.Text)
createLaunchConfiguration_keyName = Lens.lens (\CreateLaunchConfiguration' {keyName} -> keyName) (\s@CreateLaunchConfiguration' {} a -> s {keyName = a} :: CreateLaunchConfiguration)

-- | Controls whether instances in this group are launched with detailed
-- (@true@) or basic (@false@) monitoring.
--
-- The default value is @true@ (enabled).
--
-- When detailed monitoring is enabled, Amazon CloudWatch generates metrics
-- every minute and your account is charged a fee. When you disable
-- detailed monitoring, CloudWatch generates metrics every 5 minutes. For
-- more information, see
-- <https://docs.aws.amazon.com/autoscaling/latest/userguide/enable-as-instance-metrics.html Configure Monitoring for Auto Scaling Instances>
-- in the /Amazon EC2 Auto Scaling User Guide/.
createLaunchConfiguration_instanceMonitoring :: Lens.Lens' CreateLaunchConfiguration (Prelude.Maybe InstanceMonitoring)
createLaunchConfiguration_instanceMonitoring = Lens.lens (\CreateLaunchConfiguration' {instanceMonitoring} -> instanceMonitoring) (\s@CreateLaunchConfiguration' {} a -> s {instanceMonitoring = a} :: CreateLaunchConfiguration)

-- | The metadata options for the instances. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-launch-config.html#launch-configurations-imds Configuring the Instance Metadata Options>
-- in the /Amazon EC2 Auto Scaling User Guide/.
createLaunchConfiguration_metadataOptions :: Lens.Lens' CreateLaunchConfiguration (Prelude.Maybe InstanceMetadataOptions)
createLaunchConfiguration_metadataOptions = Lens.lens (\CreateLaunchConfiguration' {metadataOptions} -> metadataOptions) (\s@CreateLaunchConfiguration' {} a -> s {metadataOptions = a} :: CreateLaunchConfiguration)

-- | The name of the launch configuration. This name must be unique per
-- Region per account.
createLaunchConfiguration_launchConfigurationName :: Lens.Lens' CreateLaunchConfiguration Prelude.Text
createLaunchConfiguration_launchConfigurationName = Lens.lens (\CreateLaunchConfiguration' {launchConfigurationName} -> launchConfigurationName) (\s@CreateLaunchConfiguration' {} a -> s {launchConfigurationName = a} :: CreateLaunchConfiguration)

instance Prelude.AWSRequest CreateLaunchConfiguration where
  type
    Rs CreateLaunchConfiguration =
      CreateLaunchConfigurationResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      CreateLaunchConfigurationResponse'

instance Prelude.Hashable CreateLaunchConfiguration

instance Prelude.NFData CreateLaunchConfiguration

instance Prelude.ToHeaders CreateLaunchConfiguration where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath CreateLaunchConfiguration where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateLaunchConfiguration where
  toQuery CreateLaunchConfiguration' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("CreateLaunchConfiguration" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2011-01-01" :: Prelude.ByteString),
        "InstanceId" Prelude.=: instanceId,
        "InstanceType" Prelude.=: instanceType,
        "EbsOptimized" Prelude.=: ebsOptimized,
        "UserData" Prelude.=: userData,
        "RamdiskId" Prelude.=: ramdiskId,
        "ClassicLinkVPCSecurityGroups"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "member"
                Prelude.<$> classicLinkVPCSecurityGroups
            ),
        "SpotPrice" Prelude.=: spotPrice,
        "ImageId" Prelude.=: imageId,
        "AssociatePublicIpAddress"
          Prelude.=: associatePublicIpAddress,
        "SecurityGroups"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "member"
                Prelude.<$> securityGroups
            ),
        "IamInstanceProfile" Prelude.=: iamInstanceProfile,
        "ClassicLinkVPCId" Prelude.=: classicLinkVPCId,
        "BlockDeviceMappings"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "member"
                Prelude.<$> blockDeviceMappings
            ),
        "KernelId" Prelude.=: kernelId,
        "PlacementTenancy" Prelude.=: placementTenancy,
        "KeyName" Prelude.=: keyName,
        "InstanceMonitoring" Prelude.=: instanceMonitoring,
        "MetadataOptions" Prelude.=: metadataOptions,
        "LaunchConfigurationName"
          Prelude.=: launchConfigurationName
      ]

-- | /See:/ 'newCreateLaunchConfigurationResponse' smart constructor.
data CreateLaunchConfigurationResponse = CreateLaunchConfigurationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateLaunchConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCreateLaunchConfigurationResponse ::
  CreateLaunchConfigurationResponse
newCreateLaunchConfigurationResponse =
  CreateLaunchConfigurationResponse'

instance
  Prelude.NFData
    CreateLaunchConfigurationResponse
