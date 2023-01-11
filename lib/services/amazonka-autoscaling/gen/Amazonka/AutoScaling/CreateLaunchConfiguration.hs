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
-- Module      : Amazonka.AutoScaling.CreateLaunchConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
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
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-quotas.html Quotas for Amazon EC2 Auto Scaling>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/LaunchConfiguration.html Launch configurations>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- Amazon EC2 Auto Scaling configures instances launched as part of an Auto
-- Scaling group using either a launch template or a launch configuration.
-- We strongly recommend that you do not use launch configurations. They do
-- not provide full functionality for Amazon EC2 Auto Scaling or Amazon
-- EC2. For information about using launch templates, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/launch-templates.html Launch templates>
-- in the /Amazon EC2 Auto Scaling User Guide/.
module Amazonka.AutoScaling.CreateLaunchConfiguration
  ( -- * Creating a Request
    CreateLaunchConfiguration (..),
    newCreateLaunchConfiguration,

    -- * Request Lenses
    createLaunchConfiguration_associatePublicIpAddress,
    createLaunchConfiguration_blockDeviceMappings,
    createLaunchConfiguration_classicLinkVPCId,
    createLaunchConfiguration_classicLinkVPCSecurityGroups,
    createLaunchConfiguration_ebsOptimized,
    createLaunchConfiguration_iamInstanceProfile,
    createLaunchConfiguration_imageId,
    createLaunchConfiguration_instanceId,
    createLaunchConfiguration_instanceMonitoring,
    createLaunchConfiguration_instanceType,
    createLaunchConfiguration_kernelId,
    createLaunchConfiguration_keyName,
    createLaunchConfiguration_metadataOptions,
    createLaunchConfiguration_placementTenancy,
    createLaunchConfiguration_ramdiskId,
    createLaunchConfiguration_securityGroups,
    createLaunchConfiguration_spotPrice,
    createLaunchConfiguration_userData,
    createLaunchConfiguration_launchConfigurationName,

    -- * Destructuring the Response
    CreateLaunchConfigurationResponse (..),
    newCreateLaunchConfigurationResponse,
  )
where

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateLaunchConfiguration' smart constructor.
data CreateLaunchConfiguration = CreateLaunchConfiguration'
  { -- | Specifies whether to assign a public IPv4 address to the group\'s
    -- instances. If the instance is launched into a default subnet, the
    -- default is to assign a public IPv4 address, unless you disabled the
    -- option to assign a public IPv4 address on the subnet. If the instance is
    -- launched into a nondefault subnet, the default is not to assign a public
    -- IPv4 address, unless you enabled the option to assign a public IPv4
    -- address on the subnet.
    --
    -- If you specify @true@, each instance in the Auto Scaling group receives
    -- a unique public IPv4 address. For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html Launching Auto Scaling instances in a VPC>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    --
    -- If you specify this property, you must specify at least one subnet for
    -- @VPCZoneIdentifier@ when you create your group.
    associatePublicIpAddress :: Prelude.Maybe Prelude.Bool,
    -- | The block device mapping entries that define the block devices to attach
    -- to the instances at launch. By default, the block devices specified in
    -- the block device mapping for the AMI are used. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html Block device mappings>
    -- in the /Amazon EC2 User Guide for Linux Instances/.
    blockDeviceMappings :: Prelude.Maybe [BlockDeviceMapping],
    -- | Available for backward compatibility.
    classicLinkVPCId :: Prelude.Maybe Prelude.Text,
    -- | Available for backward compatibility.
    classicLinkVPCSecurityGroups :: Prelude.Maybe [Prelude.Text],
    -- | Specifies whether the launch configuration is optimized for EBS I\/O
    -- (@true@) or not (@false@). The optimization provides dedicated
    -- throughput to Amazon EBS and an optimized configuration stack to provide
    -- optimal I\/O performance. This optimization is not available with all
    -- instance types. Additional fees are incurred when you enable EBS
    -- optimization for an instance type that is not EBS-optimized by default.
    -- For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSOptimized.html Amazon EBS-optimized instances>
    -- in the /Amazon EC2 User Guide for Linux Instances/.
    --
    -- The default value is @false@.
    ebsOptimized :: Prelude.Maybe Prelude.Bool,
    -- | The name or the Amazon Resource Name (ARN) of the instance profile
    -- associated with the IAM role for the instance. The instance profile
    -- contains the IAM role. For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/us-iam-role.html IAM role for applications that run on Amazon EC2 instances>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    iamInstanceProfile :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Machine Image (AMI) that was assigned during
    -- registration. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/finding-an-ami.html Finding a Linux AMI>
    -- in the /Amazon EC2 User Guide for Linux Instances/.
    --
    -- If you specify @InstanceId@, an @ImageId@ is not required.
    imageId :: Prelude.Maybe Prelude.Text,
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
    instanceId :: Prelude.Maybe Prelude.Text,
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
    -- | Specifies the instance type of the EC2 instance. For information about
    -- available instance types, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#AvailableInstanceTypes Available instance types>
    -- in the /Amazon EC2 User Guide for Linux Instances/.
    --
    -- If you specify @InstanceId@, an @InstanceType@ is not required.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | The ID of the kernel associated with the AMI.
    --
    -- We recommend that you use PV-GRUB instead of kernels and RAM disks. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedKernels.html User provided kernels>
    -- in the /Amazon EC2 User Guide for Linux Instances/.
    kernelId :: Prelude.Maybe Prelude.Text,
    -- | The name of the key pair. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Amazon EC2 key pairs and Linux instances>
    -- in the /Amazon EC2 User Guide for Linux Instances/.
    keyName :: Prelude.Maybe Prelude.Text,
    -- | The metadata options for the instances. For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-launch-config.html#launch-configurations-imds Configuring the Instance Metadata Options>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    metadataOptions :: Prelude.Maybe InstanceMetadataOptions,
    -- | The tenancy of the instance, either @default@ or @dedicated@. An
    -- instance with @dedicated@ tenancy runs on isolated, single-tenant
    -- hardware and can only be launched into a VPC. To launch dedicated
    -- instances into a shared tenancy VPC (a VPC with the instance placement
    -- tenancy attribute set to @default@), you must set the value of this
    -- property to @dedicated@. For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/auto-scaling-dedicated-instances.html Configuring instance tenancy with Amazon EC2 Auto Scaling>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    --
    -- If you specify @PlacementTenancy@, you must specify at least one subnet
    -- for @VPCZoneIdentifier@ when you create your group.
    --
    -- Valid values: @default@ | @dedicated@
    placementTenancy :: Prelude.Maybe Prelude.Text,
    -- | The ID of the RAM disk to select.
    --
    -- We recommend that you use PV-GRUB instead of kernels and RAM disks. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedKernels.html User provided kernels>
    -- in the /Amazon EC2 User Guide for Linux Instances/.
    ramdiskId :: Prelude.Maybe Prelude.Text,
    -- | A list that contains the security group IDs to assign to the instances
    -- in the Auto Scaling group. For more information, see
    -- <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_SecurityGroups.html Control traffic to resources using security groups>
    -- in the /Amazon Virtual Private Cloud User Guide/.
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | The maximum hourly price to be paid for any Spot Instance launched to
    -- fulfill the request. Spot Instances are launched when the price you
    -- specify exceeds the current Spot price. For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/launch-template-spot-instances.html Request Spot Instances for fault-tolerant and flexible applications>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    --
    -- Valid Range: Minimum value of 0.001
    --
    -- When you change your maximum price by creating a new launch
    -- configuration, running instances will continue to run as long as the
    -- maximum price for those running instances is higher than the current
    -- Spot price.
    spotPrice :: Prelude.Maybe Prelude.Text,
    -- | The user data to make available to the launched EC2 instances. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance metadata and user data>
    -- (Linux) and
    -- <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ec2-instance-metadata.html Instance metadata and user data>
    -- (Windows). If you are using a command line tool, base64-encoding is
    -- performed for you, and you can load the text from a file. Otherwise, you
    -- must provide base64-encoded text. User data is limited to 16 KB.
    userData :: Prelude.Maybe Prelude.Text,
    -- | The name of the launch configuration. This name must be unique per
    -- Region per account.
    launchConfigurationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLaunchConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associatePublicIpAddress', 'createLaunchConfiguration_associatePublicIpAddress' - Specifies whether to assign a public IPv4 address to the group\'s
-- instances. If the instance is launched into a default subnet, the
-- default is to assign a public IPv4 address, unless you disabled the
-- option to assign a public IPv4 address on the subnet. If the instance is
-- launched into a nondefault subnet, the default is not to assign a public
-- IPv4 address, unless you enabled the option to assign a public IPv4
-- address on the subnet.
--
-- If you specify @true@, each instance in the Auto Scaling group receives
-- a unique public IPv4 address. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html Launching Auto Scaling instances in a VPC>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- If you specify this property, you must specify at least one subnet for
-- @VPCZoneIdentifier@ when you create your group.
--
-- 'blockDeviceMappings', 'createLaunchConfiguration_blockDeviceMappings' - The block device mapping entries that define the block devices to attach
-- to the instances at launch. By default, the block devices specified in
-- the block device mapping for the AMI are used. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html Block device mappings>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- 'classicLinkVPCId', 'createLaunchConfiguration_classicLinkVPCId' - Available for backward compatibility.
--
-- 'classicLinkVPCSecurityGroups', 'createLaunchConfiguration_classicLinkVPCSecurityGroups' - Available for backward compatibility.
--
-- 'ebsOptimized', 'createLaunchConfiguration_ebsOptimized' - Specifies whether the launch configuration is optimized for EBS I\/O
-- (@true@) or not (@false@). The optimization provides dedicated
-- throughput to Amazon EBS and an optimized configuration stack to provide
-- optimal I\/O performance. This optimization is not available with all
-- instance types. Additional fees are incurred when you enable EBS
-- optimization for an instance type that is not EBS-optimized by default.
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSOptimized.html Amazon EBS-optimized instances>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- The default value is @false@.
--
-- 'iamInstanceProfile', 'createLaunchConfiguration_iamInstanceProfile' - The name or the Amazon Resource Name (ARN) of the instance profile
-- associated with the IAM role for the instance. The instance profile
-- contains the IAM role. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/us-iam-role.html IAM role for applications that run on Amazon EC2 instances>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'imageId', 'createLaunchConfiguration_imageId' - The ID of the Amazon Machine Image (AMI) that was assigned during
-- registration. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/finding-an-ami.html Finding a Linux AMI>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- If you specify @InstanceId@, an @ImageId@ is not required.
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
-- 'instanceType', 'createLaunchConfiguration_instanceType' - Specifies the instance type of the EC2 instance. For information about
-- available instance types, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#AvailableInstanceTypes Available instance types>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- If you specify @InstanceId@, an @InstanceType@ is not required.
--
-- 'kernelId', 'createLaunchConfiguration_kernelId' - The ID of the kernel associated with the AMI.
--
-- We recommend that you use PV-GRUB instead of kernels and RAM disks. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedKernels.html User provided kernels>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- 'keyName', 'createLaunchConfiguration_keyName' - The name of the key pair. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Amazon EC2 key pairs and Linux instances>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- 'metadataOptions', 'createLaunchConfiguration_metadataOptions' - The metadata options for the instances. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-launch-config.html#launch-configurations-imds Configuring the Instance Metadata Options>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'placementTenancy', 'createLaunchConfiguration_placementTenancy' - The tenancy of the instance, either @default@ or @dedicated@. An
-- instance with @dedicated@ tenancy runs on isolated, single-tenant
-- hardware and can only be launched into a VPC. To launch dedicated
-- instances into a shared tenancy VPC (a VPC with the instance placement
-- tenancy attribute set to @default@), you must set the value of this
-- property to @dedicated@. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/auto-scaling-dedicated-instances.html Configuring instance tenancy with Amazon EC2 Auto Scaling>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- If you specify @PlacementTenancy@, you must specify at least one subnet
-- for @VPCZoneIdentifier@ when you create your group.
--
-- Valid values: @default@ | @dedicated@
--
-- 'ramdiskId', 'createLaunchConfiguration_ramdiskId' - The ID of the RAM disk to select.
--
-- We recommend that you use PV-GRUB instead of kernels and RAM disks. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedKernels.html User provided kernels>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- 'securityGroups', 'createLaunchConfiguration_securityGroups' - A list that contains the security group IDs to assign to the instances
-- in the Auto Scaling group. For more information, see
-- <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_SecurityGroups.html Control traffic to resources using security groups>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- 'spotPrice', 'createLaunchConfiguration_spotPrice' - The maximum hourly price to be paid for any Spot Instance launched to
-- fulfill the request. Spot Instances are launched when the price you
-- specify exceeds the current Spot price. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/launch-template-spot-instances.html Request Spot Instances for fault-tolerant and flexible applications>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- Valid Range: Minimum value of 0.001
--
-- When you change your maximum price by creating a new launch
-- configuration, running instances will continue to run as long as the
-- maximum price for those running instances is higher than the current
-- Spot price.
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
-- 'launchConfigurationName', 'createLaunchConfiguration_launchConfigurationName' - The name of the launch configuration. This name must be unique per
-- Region per account.
newCreateLaunchConfiguration ::
  -- | 'launchConfigurationName'
  Prelude.Text ->
  CreateLaunchConfiguration
newCreateLaunchConfiguration
  pLaunchConfigurationName_ =
    CreateLaunchConfiguration'
      { associatePublicIpAddress =
          Prelude.Nothing,
        blockDeviceMappings = Prelude.Nothing,
        classicLinkVPCId = Prelude.Nothing,
        classicLinkVPCSecurityGroups = Prelude.Nothing,
        ebsOptimized = Prelude.Nothing,
        iamInstanceProfile = Prelude.Nothing,
        imageId = Prelude.Nothing,
        instanceId = Prelude.Nothing,
        instanceMonitoring = Prelude.Nothing,
        instanceType = Prelude.Nothing,
        kernelId = Prelude.Nothing,
        keyName = Prelude.Nothing,
        metadataOptions = Prelude.Nothing,
        placementTenancy = Prelude.Nothing,
        ramdiskId = Prelude.Nothing,
        securityGroups = Prelude.Nothing,
        spotPrice = Prelude.Nothing,
        userData = Prelude.Nothing,
        launchConfigurationName =
          pLaunchConfigurationName_
      }

-- | Specifies whether to assign a public IPv4 address to the group\'s
-- instances. If the instance is launched into a default subnet, the
-- default is to assign a public IPv4 address, unless you disabled the
-- option to assign a public IPv4 address on the subnet. If the instance is
-- launched into a nondefault subnet, the default is not to assign a public
-- IPv4 address, unless you enabled the option to assign a public IPv4
-- address on the subnet.
--
-- If you specify @true@, each instance in the Auto Scaling group receives
-- a unique public IPv4 address. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html Launching Auto Scaling instances in a VPC>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- If you specify this property, you must specify at least one subnet for
-- @VPCZoneIdentifier@ when you create your group.
createLaunchConfiguration_associatePublicIpAddress :: Lens.Lens' CreateLaunchConfiguration (Prelude.Maybe Prelude.Bool)
createLaunchConfiguration_associatePublicIpAddress = Lens.lens (\CreateLaunchConfiguration' {associatePublicIpAddress} -> associatePublicIpAddress) (\s@CreateLaunchConfiguration' {} a -> s {associatePublicIpAddress = a} :: CreateLaunchConfiguration)

-- | The block device mapping entries that define the block devices to attach
-- to the instances at launch. By default, the block devices specified in
-- the block device mapping for the AMI are used. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html Block device mappings>
-- in the /Amazon EC2 User Guide for Linux Instances/.
createLaunchConfiguration_blockDeviceMappings :: Lens.Lens' CreateLaunchConfiguration (Prelude.Maybe [BlockDeviceMapping])
createLaunchConfiguration_blockDeviceMappings = Lens.lens (\CreateLaunchConfiguration' {blockDeviceMappings} -> blockDeviceMappings) (\s@CreateLaunchConfiguration' {} a -> s {blockDeviceMappings = a} :: CreateLaunchConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Available for backward compatibility.
createLaunchConfiguration_classicLinkVPCId :: Lens.Lens' CreateLaunchConfiguration (Prelude.Maybe Prelude.Text)
createLaunchConfiguration_classicLinkVPCId = Lens.lens (\CreateLaunchConfiguration' {classicLinkVPCId} -> classicLinkVPCId) (\s@CreateLaunchConfiguration' {} a -> s {classicLinkVPCId = a} :: CreateLaunchConfiguration)

-- | Available for backward compatibility.
createLaunchConfiguration_classicLinkVPCSecurityGroups :: Lens.Lens' CreateLaunchConfiguration (Prelude.Maybe [Prelude.Text])
createLaunchConfiguration_classicLinkVPCSecurityGroups = Lens.lens (\CreateLaunchConfiguration' {classicLinkVPCSecurityGroups} -> classicLinkVPCSecurityGroups) (\s@CreateLaunchConfiguration' {} a -> s {classicLinkVPCSecurityGroups = a} :: CreateLaunchConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether the launch configuration is optimized for EBS I\/O
-- (@true@) or not (@false@). The optimization provides dedicated
-- throughput to Amazon EBS and an optimized configuration stack to provide
-- optimal I\/O performance. This optimization is not available with all
-- instance types. Additional fees are incurred when you enable EBS
-- optimization for an instance type that is not EBS-optimized by default.
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSOptimized.html Amazon EBS-optimized instances>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- The default value is @false@.
createLaunchConfiguration_ebsOptimized :: Lens.Lens' CreateLaunchConfiguration (Prelude.Maybe Prelude.Bool)
createLaunchConfiguration_ebsOptimized = Lens.lens (\CreateLaunchConfiguration' {ebsOptimized} -> ebsOptimized) (\s@CreateLaunchConfiguration' {} a -> s {ebsOptimized = a} :: CreateLaunchConfiguration)

-- | The name or the Amazon Resource Name (ARN) of the instance profile
-- associated with the IAM role for the instance. The instance profile
-- contains the IAM role. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/us-iam-role.html IAM role for applications that run on Amazon EC2 instances>
-- in the /Amazon EC2 Auto Scaling User Guide/.
createLaunchConfiguration_iamInstanceProfile :: Lens.Lens' CreateLaunchConfiguration (Prelude.Maybe Prelude.Text)
createLaunchConfiguration_iamInstanceProfile = Lens.lens (\CreateLaunchConfiguration' {iamInstanceProfile} -> iamInstanceProfile) (\s@CreateLaunchConfiguration' {} a -> s {iamInstanceProfile = a} :: CreateLaunchConfiguration)

-- | The ID of the Amazon Machine Image (AMI) that was assigned during
-- registration. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/finding-an-ami.html Finding a Linux AMI>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- If you specify @InstanceId@, an @ImageId@ is not required.
createLaunchConfiguration_imageId :: Lens.Lens' CreateLaunchConfiguration (Prelude.Maybe Prelude.Text)
createLaunchConfiguration_imageId = Lens.lens (\CreateLaunchConfiguration' {imageId} -> imageId) (\s@CreateLaunchConfiguration' {} a -> s {imageId = a} :: CreateLaunchConfiguration)

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
createLaunchConfiguration_instanceId :: Lens.Lens' CreateLaunchConfiguration (Prelude.Maybe Prelude.Text)
createLaunchConfiguration_instanceId = Lens.lens (\CreateLaunchConfiguration' {instanceId} -> instanceId) (\s@CreateLaunchConfiguration' {} a -> s {instanceId = a} :: CreateLaunchConfiguration)

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

-- | Specifies the instance type of the EC2 instance. For information about
-- available instance types, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#AvailableInstanceTypes Available instance types>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- If you specify @InstanceId@, an @InstanceType@ is not required.
createLaunchConfiguration_instanceType :: Lens.Lens' CreateLaunchConfiguration (Prelude.Maybe Prelude.Text)
createLaunchConfiguration_instanceType = Lens.lens (\CreateLaunchConfiguration' {instanceType} -> instanceType) (\s@CreateLaunchConfiguration' {} a -> s {instanceType = a} :: CreateLaunchConfiguration)

-- | The ID of the kernel associated with the AMI.
--
-- We recommend that you use PV-GRUB instead of kernels and RAM disks. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedKernels.html User provided kernels>
-- in the /Amazon EC2 User Guide for Linux Instances/.
createLaunchConfiguration_kernelId :: Lens.Lens' CreateLaunchConfiguration (Prelude.Maybe Prelude.Text)
createLaunchConfiguration_kernelId = Lens.lens (\CreateLaunchConfiguration' {kernelId} -> kernelId) (\s@CreateLaunchConfiguration' {} a -> s {kernelId = a} :: CreateLaunchConfiguration)

-- | The name of the key pair. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Amazon EC2 key pairs and Linux instances>
-- in the /Amazon EC2 User Guide for Linux Instances/.
createLaunchConfiguration_keyName :: Lens.Lens' CreateLaunchConfiguration (Prelude.Maybe Prelude.Text)
createLaunchConfiguration_keyName = Lens.lens (\CreateLaunchConfiguration' {keyName} -> keyName) (\s@CreateLaunchConfiguration' {} a -> s {keyName = a} :: CreateLaunchConfiguration)

-- | The metadata options for the instances. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-launch-config.html#launch-configurations-imds Configuring the Instance Metadata Options>
-- in the /Amazon EC2 Auto Scaling User Guide/.
createLaunchConfiguration_metadataOptions :: Lens.Lens' CreateLaunchConfiguration (Prelude.Maybe InstanceMetadataOptions)
createLaunchConfiguration_metadataOptions = Lens.lens (\CreateLaunchConfiguration' {metadataOptions} -> metadataOptions) (\s@CreateLaunchConfiguration' {} a -> s {metadataOptions = a} :: CreateLaunchConfiguration)

-- | The tenancy of the instance, either @default@ or @dedicated@. An
-- instance with @dedicated@ tenancy runs on isolated, single-tenant
-- hardware and can only be launched into a VPC. To launch dedicated
-- instances into a shared tenancy VPC (a VPC with the instance placement
-- tenancy attribute set to @default@), you must set the value of this
-- property to @dedicated@. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/auto-scaling-dedicated-instances.html Configuring instance tenancy with Amazon EC2 Auto Scaling>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- If you specify @PlacementTenancy@, you must specify at least one subnet
-- for @VPCZoneIdentifier@ when you create your group.
--
-- Valid values: @default@ | @dedicated@
createLaunchConfiguration_placementTenancy :: Lens.Lens' CreateLaunchConfiguration (Prelude.Maybe Prelude.Text)
createLaunchConfiguration_placementTenancy = Lens.lens (\CreateLaunchConfiguration' {placementTenancy} -> placementTenancy) (\s@CreateLaunchConfiguration' {} a -> s {placementTenancy = a} :: CreateLaunchConfiguration)

-- | The ID of the RAM disk to select.
--
-- We recommend that you use PV-GRUB instead of kernels and RAM disks. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedKernels.html User provided kernels>
-- in the /Amazon EC2 User Guide for Linux Instances/.
createLaunchConfiguration_ramdiskId :: Lens.Lens' CreateLaunchConfiguration (Prelude.Maybe Prelude.Text)
createLaunchConfiguration_ramdiskId = Lens.lens (\CreateLaunchConfiguration' {ramdiskId} -> ramdiskId) (\s@CreateLaunchConfiguration' {} a -> s {ramdiskId = a} :: CreateLaunchConfiguration)

-- | A list that contains the security group IDs to assign to the instances
-- in the Auto Scaling group. For more information, see
-- <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_SecurityGroups.html Control traffic to resources using security groups>
-- in the /Amazon Virtual Private Cloud User Guide/.
createLaunchConfiguration_securityGroups :: Lens.Lens' CreateLaunchConfiguration (Prelude.Maybe [Prelude.Text])
createLaunchConfiguration_securityGroups = Lens.lens (\CreateLaunchConfiguration' {securityGroups} -> securityGroups) (\s@CreateLaunchConfiguration' {} a -> s {securityGroups = a} :: CreateLaunchConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The maximum hourly price to be paid for any Spot Instance launched to
-- fulfill the request. Spot Instances are launched when the price you
-- specify exceeds the current Spot price. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/launch-template-spot-instances.html Request Spot Instances for fault-tolerant and flexible applications>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- Valid Range: Minimum value of 0.001
--
-- When you change your maximum price by creating a new launch
-- configuration, running instances will continue to run as long as the
-- maximum price for those running instances is higher than the current
-- Spot price.
createLaunchConfiguration_spotPrice :: Lens.Lens' CreateLaunchConfiguration (Prelude.Maybe Prelude.Text)
createLaunchConfiguration_spotPrice = Lens.lens (\CreateLaunchConfiguration' {spotPrice} -> spotPrice) (\s@CreateLaunchConfiguration' {} a -> s {spotPrice = a} :: CreateLaunchConfiguration)

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

-- | The name of the launch configuration. This name must be unique per
-- Region per account.
createLaunchConfiguration_launchConfigurationName :: Lens.Lens' CreateLaunchConfiguration Prelude.Text
createLaunchConfiguration_launchConfigurationName = Lens.lens (\CreateLaunchConfiguration' {launchConfigurationName} -> launchConfigurationName) (\s@CreateLaunchConfiguration' {} a -> s {launchConfigurationName = a} :: CreateLaunchConfiguration)

instance Core.AWSRequest CreateLaunchConfiguration where
  type
    AWSResponse CreateLaunchConfiguration =
      CreateLaunchConfigurationResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      CreateLaunchConfigurationResponse'

instance Prelude.Hashable CreateLaunchConfiguration where
  hashWithSalt _salt CreateLaunchConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` associatePublicIpAddress
      `Prelude.hashWithSalt` blockDeviceMappings
      `Prelude.hashWithSalt` classicLinkVPCId
      `Prelude.hashWithSalt` classicLinkVPCSecurityGroups
      `Prelude.hashWithSalt` ebsOptimized
      `Prelude.hashWithSalt` iamInstanceProfile
      `Prelude.hashWithSalt` imageId
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` instanceMonitoring
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` kernelId
      `Prelude.hashWithSalt` keyName
      `Prelude.hashWithSalt` metadataOptions
      `Prelude.hashWithSalt` placementTenancy
      `Prelude.hashWithSalt` ramdiskId
      `Prelude.hashWithSalt` securityGroups
      `Prelude.hashWithSalt` spotPrice
      `Prelude.hashWithSalt` userData
      `Prelude.hashWithSalt` launchConfigurationName

instance Prelude.NFData CreateLaunchConfiguration where
  rnf CreateLaunchConfiguration' {..} =
    Prelude.rnf associatePublicIpAddress
      `Prelude.seq` Prelude.rnf blockDeviceMappings
      `Prelude.seq` Prelude.rnf classicLinkVPCId
      `Prelude.seq` Prelude.rnf classicLinkVPCSecurityGroups
      `Prelude.seq` Prelude.rnf ebsOptimized
      `Prelude.seq` Prelude.rnf iamInstanceProfile
      `Prelude.seq` Prelude.rnf imageId
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf instanceMonitoring
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf kernelId
      `Prelude.seq` Prelude.rnf keyName
      `Prelude.seq` Prelude.rnf metadataOptions
      `Prelude.seq` Prelude.rnf placementTenancy
      `Prelude.seq` Prelude.rnf ramdiskId
      `Prelude.seq` Prelude.rnf securityGroups
      `Prelude.seq` Prelude.rnf spotPrice
      `Prelude.seq` Prelude.rnf userData
      `Prelude.seq` Prelude.rnf
        launchConfigurationName

instance Data.ToHeaders CreateLaunchConfiguration where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateLaunchConfiguration where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateLaunchConfiguration where
  toQuery CreateLaunchConfiguration' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateLaunchConfiguration" :: Prelude.ByteString),
        "Version"
          Data.=: ("2011-01-01" :: Prelude.ByteString),
        "AssociatePublicIpAddress"
          Data.=: associatePublicIpAddress,
        "BlockDeviceMappings"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> blockDeviceMappings
            ),
        "ClassicLinkVPCId" Data.=: classicLinkVPCId,
        "ClassicLinkVPCSecurityGroups"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> classicLinkVPCSecurityGroups
            ),
        "EbsOptimized" Data.=: ebsOptimized,
        "IamInstanceProfile" Data.=: iamInstanceProfile,
        "ImageId" Data.=: imageId,
        "InstanceId" Data.=: instanceId,
        "InstanceMonitoring" Data.=: instanceMonitoring,
        "InstanceType" Data.=: instanceType,
        "KernelId" Data.=: kernelId,
        "KeyName" Data.=: keyName,
        "MetadataOptions" Data.=: metadataOptions,
        "PlacementTenancy" Data.=: placementTenancy,
        "RamdiskId" Data.=: ramdiskId,
        "SecurityGroups"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> securityGroups
            ),
        "SpotPrice" Data.=: spotPrice,
        "UserData" Data.=: userData,
        "LaunchConfigurationName"
          Data.=: launchConfigurationName
      ]

-- | /See:/ 'newCreateLaunchConfigurationResponse' smart constructor.
data CreateLaunchConfigurationResponse = CreateLaunchConfigurationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf _ = ()
