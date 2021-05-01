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
-- Module      : Network.AWS.EC2.RunInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Launches the specified number of instances using an AMI for which you
-- have permissions.
--
-- You can specify a number of options, or leave the default options. The
-- following rules apply:
--
-- -   [EC2-VPC] If you don\'t specify a subnet ID, we choose a default
--     subnet from your default VPC for you. If you don\'t have a default
--     VPC, you must specify a subnet ID in the request.
--
-- -   [EC2-Classic] If don\'t specify an Availability Zone, we choose one
--     for you.
--
-- -   Some instance types must be launched into a VPC. If you do not have
--     a default VPC, or if you do not specify a subnet ID, the request
--     fails. For more information, see
--     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-vpc.html#vpc-only-instance-types Instance types available only in a VPC>.
--
-- -   [EC2-VPC] All instances have a network interface with a primary
--     private IPv4 address. If you don\'t specify this address, we choose
--     one from the IPv4 range of your subnet.
--
-- -   Not all instance types support IPv6 addresses. For more information,
--     see
--     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>.
--
-- -   If you don\'t specify a security group ID, we use the default
--     security group. For more information, see
--     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-network-security.html Security groups>.
--
-- -   If any of the AMIs have a product code attached for which the user
--     has not subscribed, the request fails.
--
-- You can create a
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html launch template>,
-- which is a resource that contains the parameters to launch an instance.
-- When you launch an instance using RunInstances, you can specify the
-- launch template instead of specifying the launch parameters.
--
-- To ensure faster instance launches, break up large requests into smaller
-- batches. For example, create five separate launch requests for 100
-- instances each instead of one launch request for 500 instances.
--
-- An instance is ready for you to use when it\'s in the @running@ state.
-- You can check the state of your instance using DescribeInstances. You
-- can tag instances and EBS volumes during launch, after launch, or both.
-- For more information, see CreateTags and
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html Tagging your Amazon EC2 resources>.
--
-- Linux instances have access to the public key of the key pair at boot.
-- You can use this key to provide secure access to the instance. Amazon
-- EC2 public images use this feature to provide secure access without
-- passwords. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Key pairs>.
--
-- For troubleshooting, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_InstanceStraightToTerminated.html What to do if an instance immediately terminates>,
-- and
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/TroubleshootingInstancesConnecting.html Troubleshooting connecting to your instance>.
module Network.AWS.EC2.RunInstances
  ( -- * Creating a Request
    RunInstances (..),
    newRunInstances,

    -- * Request Lenses
    runInstances_securityGroupIds,
    runInstances_additionalInfo,
    runInstances_tagSpecifications,
    runInstances_instanceType,
    runInstances_capacityReservationSpecification,
    runInstances_ebsOptimized,
    runInstances_userData,
    runInstances_placement,
    runInstances_ipv6Addresses,
    runInstances_dryRun,
    runInstances_ramdiskId,
    runInstances_creditSpecification,
    runInstances_instanceMarketOptions,
    runInstances_launchTemplate,
    runInstances_licenseSpecifications,
    runInstances_instanceInitiatedShutdownBehavior,
    runInstances_imageId,
    runInstances_securityGroups,
    runInstances_elasticGpuSpecification,
    runInstances_elasticInferenceAccelerators,
    runInstances_iamInstanceProfile,
    runInstances_hibernationOptions,
    runInstances_ipv6AddressCount,
    runInstances_monitoring,
    runInstances_blockDeviceMappings,
    runInstances_subnetId,
    runInstances_enclaveOptions,
    runInstances_kernelId,
    runInstances_cpuOptions,
    runInstances_keyName,
    runInstances_networkInterfaces,
    runInstances_disableApiTermination,
    runInstances_metadataOptions,
    runInstances_clientToken,
    runInstances_privateIpAddress,
    runInstances_maxCount,
    runInstances_minCount,

    -- * Destructuring the Response
    Reservation (..),
    newReservation,

    -- * Response Lenses
    reservation_groups,
    reservation_requesterId,
    reservation_instances,
    reservation_reservationId,
    reservation_ownerId,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRunInstances' smart constructor.
data RunInstances = RunInstances'
  { -- | The IDs of the security groups. You can create a security group using
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateSecurityGroup.html CreateSecurityGroup>.
    --
    -- If you specify a network interface, you must specify any security groups
    -- as part of the network interface.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | Reserved.
    additionalInfo :: Prelude.Maybe Prelude.Text,
    -- | The tags to apply to the resources during launch. You can only tag
    -- instances and volumes on launch. The specified tags are applied to all
    -- instances or volumes that are created during launch. To tag a resource
    -- after it has been created, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags>.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | The instance type. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
    -- in the /Amazon EC2 User Guide/.
    --
    -- Default: @m1.small@
    instanceType :: Prelude.Maybe InstanceType,
    -- | Information about the Capacity Reservation targeting option. If you do
    -- not specify this parameter, the instance\'s Capacity Reservation
    -- preference defaults to @open@, which enables it to run in any open
    -- Capacity Reservation that has matching attributes (instance type,
    -- platform, Availability Zone).
    capacityReservationSpecification :: Prelude.Maybe CapacityReservationSpecification,
    -- | Indicates whether the instance is optimized for Amazon EBS I\/O. This
    -- optimization provides dedicated throughput to Amazon EBS and an
    -- optimized configuration stack to provide optimal Amazon EBS I\/O
    -- performance. This optimization isn\'t available with all instance types.
    -- Additional usage charges apply when using an EBS-optimized instance.
    --
    -- Default: @false@
    ebsOptimized :: Prelude.Maybe Prelude.Bool,
    -- | The user data to make available to the instance. For more information,
    -- see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/user-data.html Running commands on your Linux instance at launch>
    -- (Linux) and
    -- <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ec2-instance-metadata.html#instancedata-add-user-data Adding User Data>
    -- (Windows). If you are using a command line tool, base64-encoding is
    -- performed for you, and you can load the text from a file. Otherwise, you
    -- must provide base64-encoded text. User data is limited to 16 KB.
    userData :: Prelude.Maybe Prelude.Text,
    -- | The placement for the instance.
    placement :: Prelude.Maybe Placement,
    -- | [EC2-VPC] The IPv6 addresses from the range of the subnet to associate
    -- with the primary network interface. You cannot specify this option and
    -- the option to assign a number of IPv6 addresses in the same request. You
    -- cannot specify this option if you\'ve specified a minimum number of
    -- instances to launch.
    --
    -- You cannot specify this option and the network interfaces option in the
    -- same request.
    ipv6Addresses :: Prelude.Maybe [InstanceIpv6Address],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the RAM disk to select. Some kernels require additional
    -- drivers at launch. Check the kernel requirements for information about
    -- whether you need to specify a RAM disk. To find kernel requirements, go
    -- to the AWS Resource Center and search for the kernel ID.
    --
    -- We recommend that you use PV-GRUB instead of kernels and RAM disks. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html PV-GRUB>
    -- in the /Amazon EC2 User Guide/.
    ramdiskId :: Prelude.Maybe Prelude.Text,
    -- | The credit option for CPU usage of the burstable performance instance.
    -- Valid values are @standard@ and @unlimited@. To change this attribute
    -- after launch, use
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ModifyInstanceCreditSpecification.html ModifyInstanceCreditSpecification>.
    -- For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/burstable-performance-instances.html Burstable performance instances>
    -- in the /Amazon EC2 User Guide/.
    --
    -- Default: @standard@ (T2 instances) or @unlimited@ (T3\/T3a instances)
    creditSpecification :: Prelude.Maybe CreditSpecificationRequest,
    -- | The market (purchasing) option for the instances.
    --
    -- For RunInstances, persistent Spot Instance requests are only supported
    -- when __InstanceInterruptionBehavior__ is set to either @hibernate@ or
    -- @stop@.
    instanceMarketOptions :: Prelude.Maybe InstanceMarketOptionsRequest,
    -- | The launch template to use to launch the instances. Any parameters that
    -- you specify in RunInstances override the same parameters in the launch
    -- template. You can specify either the name or ID of a launch template,
    -- but not both.
    launchTemplate :: Prelude.Maybe LaunchTemplateSpecification,
    -- | The license configurations.
    licenseSpecifications :: Prelude.Maybe [LicenseConfigurationRequest],
    -- | Indicates whether an instance stops or terminates when you initiate
    -- shutdown from the instance (using the operating system command for
    -- system shutdown).
    --
    -- Default: @stop@
    instanceInitiatedShutdownBehavior :: Prelude.Maybe ShutdownBehavior,
    -- | The ID of the AMI. An AMI ID is required to launch an instance and must
    -- be specified here or in a launch template.
    imageId :: Prelude.Maybe Prelude.Text,
    -- | [EC2-Classic, default VPC] The names of the security groups. For a
    -- nondefault VPC, you must use security group IDs instead.
    --
    -- If you specify a network interface, you must specify any security groups
    -- as part of the network interface.
    --
    -- Default: Amazon EC2 uses the default security group.
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | An elastic GPU to associate with the instance. An Elastic GPU is a GPU
    -- resource that you can attach to your Windows instance to accelerate the
    -- graphics performance of your applications. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/elastic-graphics.html Amazon EC2 Elastic GPUs>
    -- in the /Amazon EC2 User Guide/.
    elasticGpuSpecification :: Prelude.Maybe [ElasticGpuSpecification],
    -- | An elastic inference accelerator to associate with the instance. Elastic
    -- inference accelerators are a resource you can attach to your Amazon EC2
    -- instances to accelerate your Deep Learning (DL) inference workloads.
    --
    -- You cannot specify accelerators from different generations in the same
    -- request.
    elasticInferenceAccelerators :: Prelude.Maybe [ElasticInferenceAccelerator],
    -- | The name or Amazon Resource Name (ARN) of an IAM instance profile.
    iamInstanceProfile :: Prelude.Maybe IamInstanceProfileSpecification,
    -- | Indicates whether an instance is enabled for hibernation. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate your instance>
    -- in the /Amazon EC2 User Guide/.
    --
    -- You can\'t enable hibernation and AWS Nitro Enclaves on the same
    -- instance.
    hibernationOptions :: Prelude.Maybe HibernationOptionsRequest,
    -- | [EC2-VPC] The number of IPv6 addresses to associate with the primary
    -- network interface. Amazon EC2 chooses the IPv6 addresses from the range
    -- of your subnet. You cannot specify this option and the option to assign
    -- specific IPv6 addresses in the same request. You can specify this option
    -- if you\'ve specified a minimum number of instances to launch.
    --
    -- You cannot specify this option and the network interfaces option in the
    -- same request.
    ipv6AddressCount :: Prelude.Maybe Prelude.Int,
    -- | Specifies whether detailed monitoring is enabled for the instance.
    monitoring :: Prelude.Maybe RunInstancesMonitoringEnabled,
    -- | The block device mapping entries.
    blockDeviceMappings :: Prelude.Maybe [BlockDeviceMapping],
    -- | [EC2-VPC] The ID of the subnet to launch the instance into.
    --
    -- If you specify a network interface, you must specify any subnets as part
    -- of the network interface.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the instance is enabled for AWS Nitro Enclaves. For
    -- more information, see
    -- <https://docs.aws.amazon.com/enclaves/latest/user/nitro-enclave.html What is AWS Nitro Enclaves?>
    -- in the /AWS Nitro Enclaves User Guide/.
    --
    -- You can\'t enable AWS Nitro Enclaves and hibernation on the same
    -- instance.
    enclaveOptions :: Prelude.Maybe EnclaveOptionsRequest,
    -- | The ID of the kernel.
    --
    -- We recommend that you use PV-GRUB instead of kernels and RAM disks. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html PV-GRUB>
    -- in the /Amazon EC2 User Guide/.
    kernelId :: Prelude.Maybe Prelude.Text,
    -- | The CPU options for the instance. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-optimize-cpu.html Optimizing CPU options>
    -- in the /Amazon EC2 User Guide/.
    cpuOptions :: Prelude.Maybe CpuOptionsRequest,
    -- | The name of the key pair. You can create a key pair using
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateKeyPair.html CreateKeyPair>
    -- or
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportKeyPair.html ImportKeyPair>.
    --
    -- If you do not specify a key pair, you can\'t connect to the instance
    -- unless you choose an AMI that is configured to allow users another way
    -- to log in.
    keyName :: Prelude.Maybe Prelude.Text,
    -- | The network interfaces to associate with the instance. If you specify a
    -- network interface, you must specify any security groups and subnets as
    -- part of the network interface.
    networkInterfaces :: Prelude.Maybe [InstanceNetworkInterfaceSpecification],
    -- | If you set this parameter to @true@, you can\'t terminate the instance
    -- using the Amazon EC2 console, CLI, or API; otherwise, you can. To change
    -- this attribute after launch, use
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ModifyInstanceAttribute.html ModifyInstanceAttribute>.
    -- Alternatively, if you set @InstanceInitiatedShutdownBehavior@ to
    -- @terminate@, you can terminate the instance by running the shutdown
    -- command from the instance.
    --
    -- Default: @false@
    disableApiTermination :: Prelude.Maybe Prelude.Bool,
    -- | The metadata options for the instance. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance metadata and user data>.
    metadataOptions :: Prelude.Maybe InstanceMetadataOptionsRequest,
    -- | Unique, case-sensitive identifier you provide to ensure the idempotency
    -- of the request. If you do not specify a client token, a randomly
    -- generated token is used for the request to ensure idempotency.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
    --
    -- Constraints: Maximum 64 ASCII characters
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | [EC2-VPC] The primary IPv4 address. You must specify a value from the
    -- IPv4 address range of the subnet.
    --
    -- Only one private IP address can be designated as primary. You can\'t
    -- specify this option if you\'ve specified the option to designate a
    -- private IP address as the primary IP address in a network interface
    -- specification. You cannot specify this option if you\'re launching more
    -- than one instance in the request.
    --
    -- You cannot specify this option and the network interfaces option in the
    -- same request.
    privateIpAddress :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of instances to launch. If you specify more instances
    -- than Amazon EC2 can launch in the target Availability Zone, Amazon EC2
    -- launches the largest possible number of instances above @MinCount@.
    --
    -- Constraints: Between 1 and the maximum number you\'re allowed for the
    -- specified instance type. For more information about the default limits,
    -- and how to request an increase, see
    -- <http://aws.amazon.com/ec2/faqs/#How_many_instances_can_I_run_in_Amazon_EC2 How many instances can I run in Amazon EC2>
    -- in the Amazon EC2 FAQ.
    maxCount :: Prelude.Int,
    -- | The minimum number of instances to launch. If you specify a minimum that
    -- is more instances than Amazon EC2 can launch in the target Availability
    -- Zone, Amazon EC2 launches no instances.
    --
    -- Constraints: Between 1 and the maximum number you\'re allowed for the
    -- specified instance type. For more information about the default limits,
    -- and how to request an increase, see
    -- <http://aws.amazon.com/ec2/faqs/#How_many_instances_can_I_run_in_Amazon_EC2 How many instances can I run in Amazon EC2>
    -- in the Amazon EC2 General FAQ.
    minCount :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RunInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'runInstances_securityGroupIds' - The IDs of the security groups. You can create a security group using
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateSecurityGroup.html CreateSecurityGroup>.
--
-- If you specify a network interface, you must specify any security groups
-- as part of the network interface.
--
-- 'additionalInfo', 'runInstances_additionalInfo' - Reserved.
--
-- 'tagSpecifications', 'runInstances_tagSpecifications' - The tags to apply to the resources during launch. You can only tag
-- instances and volumes on launch. The specified tags are applied to all
-- instances or volumes that are created during launch. To tag a resource
-- after it has been created, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags>.
--
-- 'instanceType', 'runInstances_instanceType' - The instance type. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
-- in the /Amazon EC2 User Guide/.
--
-- Default: @m1.small@
--
-- 'capacityReservationSpecification', 'runInstances_capacityReservationSpecification' - Information about the Capacity Reservation targeting option. If you do
-- not specify this parameter, the instance\'s Capacity Reservation
-- preference defaults to @open@, which enables it to run in any open
-- Capacity Reservation that has matching attributes (instance type,
-- platform, Availability Zone).
--
-- 'ebsOptimized', 'runInstances_ebsOptimized' - Indicates whether the instance is optimized for Amazon EBS I\/O. This
-- optimization provides dedicated throughput to Amazon EBS and an
-- optimized configuration stack to provide optimal Amazon EBS I\/O
-- performance. This optimization isn\'t available with all instance types.
-- Additional usage charges apply when using an EBS-optimized instance.
--
-- Default: @false@
--
-- 'userData', 'runInstances_userData' - The user data to make available to the instance. For more information,
-- see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/user-data.html Running commands on your Linux instance at launch>
-- (Linux) and
-- <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ec2-instance-metadata.html#instancedata-add-user-data Adding User Data>
-- (Windows). If you are using a command line tool, base64-encoding is
-- performed for you, and you can load the text from a file. Otherwise, you
-- must provide base64-encoded text. User data is limited to 16 KB.
--
-- 'placement', 'runInstances_placement' - The placement for the instance.
--
-- 'ipv6Addresses', 'runInstances_ipv6Addresses' - [EC2-VPC] The IPv6 addresses from the range of the subnet to associate
-- with the primary network interface. You cannot specify this option and
-- the option to assign a number of IPv6 addresses in the same request. You
-- cannot specify this option if you\'ve specified a minimum number of
-- instances to launch.
--
-- You cannot specify this option and the network interfaces option in the
-- same request.
--
-- 'dryRun', 'runInstances_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'ramdiskId', 'runInstances_ramdiskId' - The ID of the RAM disk to select. Some kernels require additional
-- drivers at launch. Check the kernel requirements for information about
-- whether you need to specify a RAM disk. To find kernel requirements, go
-- to the AWS Resource Center and search for the kernel ID.
--
-- We recommend that you use PV-GRUB instead of kernels and RAM disks. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html PV-GRUB>
-- in the /Amazon EC2 User Guide/.
--
-- 'creditSpecification', 'runInstances_creditSpecification' - The credit option for CPU usage of the burstable performance instance.
-- Valid values are @standard@ and @unlimited@. To change this attribute
-- after launch, use
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ModifyInstanceCreditSpecification.html ModifyInstanceCreditSpecification>.
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/burstable-performance-instances.html Burstable performance instances>
-- in the /Amazon EC2 User Guide/.
--
-- Default: @standard@ (T2 instances) or @unlimited@ (T3\/T3a instances)
--
-- 'instanceMarketOptions', 'runInstances_instanceMarketOptions' - The market (purchasing) option for the instances.
--
-- For RunInstances, persistent Spot Instance requests are only supported
-- when __InstanceInterruptionBehavior__ is set to either @hibernate@ or
-- @stop@.
--
-- 'launchTemplate', 'runInstances_launchTemplate' - The launch template to use to launch the instances. Any parameters that
-- you specify in RunInstances override the same parameters in the launch
-- template. You can specify either the name or ID of a launch template,
-- but not both.
--
-- 'licenseSpecifications', 'runInstances_licenseSpecifications' - The license configurations.
--
-- 'instanceInitiatedShutdownBehavior', 'runInstances_instanceInitiatedShutdownBehavior' - Indicates whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for
-- system shutdown).
--
-- Default: @stop@
--
-- 'imageId', 'runInstances_imageId' - The ID of the AMI. An AMI ID is required to launch an instance and must
-- be specified here or in a launch template.
--
-- 'securityGroups', 'runInstances_securityGroups' - [EC2-Classic, default VPC] The names of the security groups. For a
-- nondefault VPC, you must use security group IDs instead.
--
-- If you specify a network interface, you must specify any security groups
-- as part of the network interface.
--
-- Default: Amazon EC2 uses the default security group.
--
-- 'elasticGpuSpecification', 'runInstances_elasticGpuSpecification' - An elastic GPU to associate with the instance. An Elastic GPU is a GPU
-- resource that you can attach to your Windows instance to accelerate the
-- graphics performance of your applications. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/elastic-graphics.html Amazon EC2 Elastic GPUs>
-- in the /Amazon EC2 User Guide/.
--
-- 'elasticInferenceAccelerators', 'runInstances_elasticInferenceAccelerators' - An elastic inference accelerator to associate with the instance. Elastic
-- inference accelerators are a resource you can attach to your Amazon EC2
-- instances to accelerate your Deep Learning (DL) inference workloads.
--
-- You cannot specify accelerators from different generations in the same
-- request.
--
-- 'iamInstanceProfile', 'runInstances_iamInstanceProfile' - The name or Amazon Resource Name (ARN) of an IAM instance profile.
--
-- 'hibernationOptions', 'runInstances_hibernationOptions' - Indicates whether an instance is enabled for hibernation. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate your instance>
-- in the /Amazon EC2 User Guide/.
--
-- You can\'t enable hibernation and AWS Nitro Enclaves on the same
-- instance.
--
-- 'ipv6AddressCount', 'runInstances_ipv6AddressCount' - [EC2-VPC] The number of IPv6 addresses to associate with the primary
-- network interface. Amazon EC2 chooses the IPv6 addresses from the range
-- of your subnet. You cannot specify this option and the option to assign
-- specific IPv6 addresses in the same request. You can specify this option
-- if you\'ve specified a minimum number of instances to launch.
--
-- You cannot specify this option and the network interfaces option in the
-- same request.
--
-- 'monitoring', 'runInstances_monitoring' - Specifies whether detailed monitoring is enabled for the instance.
--
-- 'blockDeviceMappings', 'runInstances_blockDeviceMappings' - The block device mapping entries.
--
-- 'subnetId', 'runInstances_subnetId' - [EC2-VPC] The ID of the subnet to launch the instance into.
--
-- If you specify a network interface, you must specify any subnets as part
-- of the network interface.
--
-- 'enclaveOptions', 'runInstances_enclaveOptions' - Indicates whether the instance is enabled for AWS Nitro Enclaves. For
-- more information, see
-- <https://docs.aws.amazon.com/enclaves/latest/user/nitro-enclave.html What is AWS Nitro Enclaves?>
-- in the /AWS Nitro Enclaves User Guide/.
--
-- You can\'t enable AWS Nitro Enclaves and hibernation on the same
-- instance.
--
-- 'kernelId', 'runInstances_kernelId' - The ID of the kernel.
--
-- We recommend that you use PV-GRUB instead of kernels and RAM disks. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html PV-GRUB>
-- in the /Amazon EC2 User Guide/.
--
-- 'cpuOptions', 'runInstances_cpuOptions' - The CPU options for the instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-optimize-cpu.html Optimizing CPU options>
-- in the /Amazon EC2 User Guide/.
--
-- 'keyName', 'runInstances_keyName' - The name of the key pair. You can create a key pair using
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateKeyPair.html CreateKeyPair>
-- or
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportKeyPair.html ImportKeyPair>.
--
-- If you do not specify a key pair, you can\'t connect to the instance
-- unless you choose an AMI that is configured to allow users another way
-- to log in.
--
-- 'networkInterfaces', 'runInstances_networkInterfaces' - The network interfaces to associate with the instance. If you specify a
-- network interface, you must specify any security groups and subnets as
-- part of the network interface.
--
-- 'disableApiTermination', 'runInstances_disableApiTermination' - If you set this parameter to @true@, you can\'t terminate the instance
-- using the Amazon EC2 console, CLI, or API; otherwise, you can. To change
-- this attribute after launch, use
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ModifyInstanceAttribute.html ModifyInstanceAttribute>.
-- Alternatively, if you set @InstanceInitiatedShutdownBehavior@ to
-- @terminate@, you can terminate the instance by running the shutdown
-- command from the instance.
--
-- Default: @false@
--
-- 'metadataOptions', 'runInstances_metadataOptions' - The metadata options for the instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance metadata and user data>.
--
-- 'clientToken', 'runInstances_clientToken' - Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request. If you do not specify a client token, a randomly
-- generated token is used for the request to ensure idempotency.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- Constraints: Maximum 64 ASCII characters
--
-- 'privateIpAddress', 'runInstances_privateIpAddress' - [EC2-VPC] The primary IPv4 address. You must specify a value from the
-- IPv4 address range of the subnet.
--
-- Only one private IP address can be designated as primary. You can\'t
-- specify this option if you\'ve specified the option to designate a
-- private IP address as the primary IP address in a network interface
-- specification. You cannot specify this option if you\'re launching more
-- than one instance in the request.
--
-- You cannot specify this option and the network interfaces option in the
-- same request.
--
-- 'maxCount', 'runInstances_maxCount' - The maximum number of instances to launch. If you specify more instances
-- than Amazon EC2 can launch in the target Availability Zone, Amazon EC2
-- launches the largest possible number of instances above @MinCount@.
--
-- Constraints: Between 1 and the maximum number you\'re allowed for the
-- specified instance type. For more information about the default limits,
-- and how to request an increase, see
-- <http://aws.amazon.com/ec2/faqs/#How_many_instances_can_I_run_in_Amazon_EC2 How many instances can I run in Amazon EC2>
-- in the Amazon EC2 FAQ.
--
-- 'minCount', 'runInstances_minCount' - The minimum number of instances to launch. If you specify a minimum that
-- is more instances than Amazon EC2 can launch in the target Availability
-- Zone, Amazon EC2 launches no instances.
--
-- Constraints: Between 1 and the maximum number you\'re allowed for the
-- specified instance type. For more information about the default limits,
-- and how to request an increase, see
-- <http://aws.amazon.com/ec2/faqs/#How_many_instances_can_I_run_in_Amazon_EC2 How many instances can I run in Amazon EC2>
-- in the Amazon EC2 General FAQ.
newRunInstances ::
  -- | 'maxCount'
  Prelude.Int ->
  -- | 'minCount'
  Prelude.Int ->
  RunInstances
newRunInstances pMaxCount_ pMinCount_ =
  RunInstances'
    { securityGroupIds = Prelude.Nothing,
      additionalInfo = Prelude.Nothing,
      tagSpecifications = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      capacityReservationSpecification = Prelude.Nothing,
      ebsOptimized = Prelude.Nothing,
      userData = Prelude.Nothing,
      placement = Prelude.Nothing,
      ipv6Addresses = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      ramdiskId = Prelude.Nothing,
      creditSpecification = Prelude.Nothing,
      instanceMarketOptions = Prelude.Nothing,
      launchTemplate = Prelude.Nothing,
      licenseSpecifications = Prelude.Nothing,
      instanceInitiatedShutdownBehavior = Prelude.Nothing,
      imageId = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      elasticGpuSpecification = Prelude.Nothing,
      elasticInferenceAccelerators = Prelude.Nothing,
      iamInstanceProfile = Prelude.Nothing,
      hibernationOptions = Prelude.Nothing,
      ipv6AddressCount = Prelude.Nothing,
      monitoring = Prelude.Nothing,
      blockDeviceMappings = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      enclaveOptions = Prelude.Nothing,
      kernelId = Prelude.Nothing,
      cpuOptions = Prelude.Nothing,
      keyName = Prelude.Nothing,
      networkInterfaces = Prelude.Nothing,
      disableApiTermination = Prelude.Nothing,
      metadataOptions = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      privateIpAddress = Prelude.Nothing,
      maxCount = pMaxCount_,
      minCount = pMinCount_
    }

-- | The IDs of the security groups. You can create a security group using
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateSecurityGroup.html CreateSecurityGroup>.
--
-- If you specify a network interface, you must specify any security groups
-- as part of the network interface.
runInstances_securityGroupIds :: Lens.Lens' RunInstances (Prelude.Maybe [Prelude.Text])
runInstances_securityGroupIds = Lens.lens (\RunInstances' {securityGroupIds} -> securityGroupIds) (\s@RunInstances' {} a -> s {securityGroupIds = a} :: RunInstances) Prelude.. Lens.mapping Prelude._Coerce

-- | Reserved.
runInstances_additionalInfo :: Lens.Lens' RunInstances (Prelude.Maybe Prelude.Text)
runInstances_additionalInfo = Lens.lens (\RunInstances' {additionalInfo} -> additionalInfo) (\s@RunInstances' {} a -> s {additionalInfo = a} :: RunInstances)

-- | The tags to apply to the resources during launch. You can only tag
-- instances and volumes on launch. The specified tags are applied to all
-- instances or volumes that are created during launch. To tag a resource
-- after it has been created, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags>.
runInstances_tagSpecifications :: Lens.Lens' RunInstances (Prelude.Maybe [TagSpecification])
runInstances_tagSpecifications = Lens.lens (\RunInstances' {tagSpecifications} -> tagSpecifications) (\s@RunInstances' {} a -> s {tagSpecifications = a} :: RunInstances) Prelude.. Lens.mapping Prelude._Coerce

-- | The instance type. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
-- in the /Amazon EC2 User Guide/.
--
-- Default: @m1.small@
runInstances_instanceType :: Lens.Lens' RunInstances (Prelude.Maybe InstanceType)
runInstances_instanceType = Lens.lens (\RunInstances' {instanceType} -> instanceType) (\s@RunInstances' {} a -> s {instanceType = a} :: RunInstances)

-- | Information about the Capacity Reservation targeting option. If you do
-- not specify this parameter, the instance\'s Capacity Reservation
-- preference defaults to @open@, which enables it to run in any open
-- Capacity Reservation that has matching attributes (instance type,
-- platform, Availability Zone).
runInstances_capacityReservationSpecification :: Lens.Lens' RunInstances (Prelude.Maybe CapacityReservationSpecification)
runInstances_capacityReservationSpecification = Lens.lens (\RunInstances' {capacityReservationSpecification} -> capacityReservationSpecification) (\s@RunInstances' {} a -> s {capacityReservationSpecification = a} :: RunInstances)

-- | Indicates whether the instance is optimized for Amazon EBS I\/O. This
-- optimization provides dedicated throughput to Amazon EBS and an
-- optimized configuration stack to provide optimal Amazon EBS I\/O
-- performance. This optimization isn\'t available with all instance types.
-- Additional usage charges apply when using an EBS-optimized instance.
--
-- Default: @false@
runInstances_ebsOptimized :: Lens.Lens' RunInstances (Prelude.Maybe Prelude.Bool)
runInstances_ebsOptimized = Lens.lens (\RunInstances' {ebsOptimized} -> ebsOptimized) (\s@RunInstances' {} a -> s {ebsOptimized = a} :: RunInstances)

-- | The user data to make available to the instance. For more information,
-- see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/user-data.html Running commands on your Linux instance at launch>
-- (Linux) and
-- <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ec2-instance-metadata.html#instancedata-add-user-data Adding User Data>
-- (Windows). If you are using a command line tool, base64-encoding is
-- performed for you, and you can load the text from a file. Otherwise, you
-- must provide base64-encoded text. User data is limited to 16 KB.
runInstances_userData :: Lens.Lens' RunInstances (Prelude.Maybe Prelude.Text)
runInstances_userData = Lens.lens (\RunInstances' {userData} -> userData) (\s@RunInstances' {} a -> s {userData = a} :: RunInstances)

-- | The placement for the instance.
runInstances_placement :: Lens.Lens' RunInstances (Prelude.Maybe Placement)
runInstances_placement = Lens.lens (\RunInstances' {placement} -> placement) (\s@RunInstances' {} a -> s {placement = a} :: RunInstances)

-- | [EC2-VPC] The IPv6 addresses from the range of the subnet to associate
-- with the primary network interface. You cannot specify this option and
-- the option to assign a number of IPv6 addresses in the same request. You
-- cannot specify this option if you\'ve specified a minimum number of
-- instances to launch.
--
-- You cannot specify this option and the network interfaces option in the
-- same request.
runInstances_ipv6Addresses :: Lens.Lens' RunInstances (Prelude.Maybe [InstanceIpv6Address])
runInstances_ipv6Addresses = Lens.lens (\RunInstances' {ipv6Addresses} -> ipv6Addresses) (\s@RunInstances' {} a -> s {ipv6Addresses = a} :: RunInstances) Prelude.. Lens.mapping Prelude._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
runInstances_dryRun :: Lens.Lens' RunInstances (Prelude.Maybe Prelude.Bool)
runInstances_dryRun = Lens.lens (\RunInstances' {dryRun} -> dryRun) (\s@RunInstances' {} a -> s {dryRun = a} :: RunInstances)

-- | The ID of the RAM disk to select. Some kernels require additional
-- drivers at launch. Check the kernel requirements for information about
-- whether you need to specify a RAM disk. To find kernel requirements, go
-- to the AWS Resource Center and search for the kernel ID.
--
-- We recommend that you use PV-GRUB instead of kernels and RAM disks. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html PV-GRUB>
-- in the /Amazon EC2 User Guide/.
runInstances_ramdiskId :: Lens.Lens' RunInstances (Prelude.Maybe Prelude.Text)
runInstances_ramdiskId = Lens.lens (\RunInstances' {ramdiskId} -> ramdiskId) (\s@RunInstances' {} a -> s {ramdiskId = a} :: RunInstances)

-- | The credit option for CPU usage of the burstable performance instance.
-- Valid values are @standard@ and @unlimited@. To change this attribute
-- after launch, use
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ModifyInstanceCreditSpecification.html ModifyInstanceCreditSpecification>.
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/burstable-performance-instances.html Burstable performance instances>
-- in the /Amazon EC2 User Guide/.
--
-- Default: @standard@ (T2 instances) or @unlimited@ (T3\/T3a instances)
runInstances_creditSpecification :: Lens.Lens' RunInstances (Prelude.Maybe CreditSpecificationRequest)
runInstances_creditSpecification = Lens.lens (\RunInstances' {creditSpecification} -> creditSpecification) (\s@RunInstances' {} a -> s {creditSpecification = a} :: RunInstances)

-- | The market (purchasing) option for the instances.
--
-- For RunInstances, persistent Spot Instance requests are only supported
-- when __InstanceInterruptionBehavior__ is set to either @hibernate@ or
-- @stop@.
runInstances_instanceMarketOptions :: Lens.Lens' RunInstances (Prelude.Maybe InstanceMarketOptionsRequest)
runInstances_instanceMarketOptions = Lens.lens (\RunInstances' {instanceMarketOptions} -> instanceMarketOptions) (\s@RunInstances' {} a -> s {instanceMarketOptions = a} :: RunInstances)

-- | The launch template to use to launch the instances. Any parameters that
-- you specify in RunInstances override the same parameters in the launch
-- template. You can specify either the name or ID of a launch template,
-- but not both.
runInstances_launchTemplate :: Lens.Lens' RunInstances (Prelude.Maybe LaunchTemplateSpecification)
runInstances_launchTemplate = Lens.lens (\RunInstances' {launchTemplate} -> launchTemplate) (\s@RunInstances' {} a -> s {launchTemplate = a} :: RunInstances)

-- | The license configurations.
runInstances_licenseSpecifications :: Lens.Lens' RunInstances (Prelude.Maybe [LicenseConfigurationRequest])
runInstances_licenseSpecifications = Lens.lens (\RunInstances' {licenseSpecifications} -> licenseSpecifications) (\s@RunInstances' {} a -> s {licenseSpecifications = a} :: RunInstances) Prelude.. Lens.mapping Prelude._Coerce

-- | Indicates whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for
-- system shutdown).
--
-- Default: @stop@
runInstances_instanceInitiatedShutdownBehavior :: Lens.Lens' RunInstances (Prelude.Maybe ShutdownBehavior)
runInstances_instanceInitiatedShutdownBehavior = Lens.lens (\RunInstances' {instanceInitiatedShutdownBehavior} -> instanceInitiatedShutdownBehavior) (\s@RunInstances' {} a -> s {instanceInitiatedShutdownBehavior = a} :: RunInstances)

-- | The ID of the AMI. An AMI ID is required to launch an instance and must
-- be specified here or in a launch template.
runInstances_imageId :: Lens.Lens' RunInstances (Prelude.Maybe Prelude.Text)
runInstances_imageId = Lens.lens (\RunInstances' {imageId} -> imageId) (\s@RunInstances' {} a -> s {imageId = a} :: RunInstances)

-- | [EC2-Classic, default VPC] The names of the security groups. For a
-- nondefault VPC, you must use security group IDs instead.
--
-- If you specify a network interface, you must specify any security groups
-- as part of the network interface.
--
-- Default: Amazon EC2 uses the default security group.
runInstances_securityGroups :: Lens.Lens' RunInstances (Prelude.Maybe [Prelude.Text])
runInstances_securityGroups = Lens.lens (\RunInstances' {securityGroups} -> securityGroups) (\s@RunInstances' {} a -> s {securityGroups = a} :: RunInstances) Prelude.. Lens.mapping Prelude._Coerce

-- | An elastic GPU to associate with the instance. An Elastic GPU is a GPU
-- resource that you can attach to your Windows instance to accelerate the
-- graphics performance of your applications. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/elastic-graphics.html Amazon EC2 Elastic GPUs>
-- in the /Amazon EC2 User Guide/.
runInstances_elasticGpuSpecification :: Lens.Lens' RunInstances (Prelude.Maybe [ElasticGpuSpecification])
runInstances_elasticGpuSpecification = Lens.lens (\RunInstances' {elasticGpuSpecification} -> elasticGpuSpecification) (\s@RunInstances' {} a -> s {elasticGpuSpecification = a} :: RunInstances) Prelude.. Lens.mapping Prelude._Coerce

-- | An elastic inference accelerator to associate with the instance. Elastic
-- inference accelerators are a resource you can attach to your Amazon EC2
-- instances to accelerate your Deep Learning (DL) inference workloads.
--
-- You cannot specify accelerators from different generations in the same
-- request.
runInstances_elasticInferenceAccelerators :: Lens.Lens' RunInstances (Prelude.Maybe [ElasticInferenceAccelerator])
runInstances_elasticInferenceAccelerators = Lens.lens (\RunInstances' {elasticInferenceAccelerators} -> elasticInferenceAccelerators) (\s@RunInstances' {} a -> s {elasticInferenceAccelerators = a} :: RunInstances) Prelude.. Lens.mapping Prelude._Coerce

-- | The name or Amazon Resource Name (ARN) of an IAM instance profile.
runInstances_iamInstanceProfile :: Lens.Lens' RunInstances (Prelude.Maybe IamInstanceProfileSpecification)
runInstances_iamInstanceProfile = Lens.lens (\RunInstances' {iamInstanceProfile} -> iamInstanceProfile) (\s@RunInstances' {} a -> s {iamInstanceProfile = a} :: RunInstances)

-- | Indicates whether an instance is enabled for hibernation. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate your instance>
-- in the /Amazon EC2 User Guide/.
--
-- You can\'t enable hibernation and AWS Nitro Enclaves on the same
-- instance.
runInstances_hibernationOptions :: Lens.Lens' RunInstances (Prelude.Maybe HibernationOptionsRequest)
runInstances_hibernationOptions = Lens.lens (\RunInstances' {hibernationOptions} -> hibernationOptions) (\s@RunInstances' {} a -> s {hibernationOptions = a} :: RunInstances)

-- | [EC2-VPC] The number of IPv6 addresses to associate with the primary
-- network interface. Amazon EC2 chooses the IPv6 addresses from the range
-- of your subnet. You cannot specify this option and the option to assign
-- specific IPv6 addresses in the same request. You can specify this option
-- if you\'ve specified a minimum number of instances to launch.
--
-- You cannot specify this option and the network interfaces option in the
-- same request.
runInstances_ipv6AddressCount :: Lens.Lens' RunInstances (Prelude.Maybe Prelude.Int)
runInstances_ipv6AddressCount = Lens.lens (\RunInstances' {ipv6AddressCount} -> ipv6AddressCount) (\s@RunInstances' {} a -> s {ipv6AddressCount = a} :: RunInstances)

-- | Specifies whether detailed monitoring is enabled for the instance.
runInstances_monitoring :: Lens.Lens' RunInstances (Prelude.Maybe RunInstancesMonitoringEnabled)
runInstances_monitoring = Lens.lens (\RunInstances' {monitoring} -> monitoring) (\s@RunInstances' {} a -> s {monitoring = a} :: RunInstances)

-- | The block device mapping entries.
runInstances_blockDeviceMappings :: Lens.Lens' RunInstances (Prelude.Maybe [BlockDeviceMapping])
runInstances_blockDeviceMappings = Lens.lens (\RunInstances' {blockDeviceMappings} -> blockDeviceMappings) (\s@RunInstances' {} a -> s {blockDeviceMappings = a} :: RunInstances) Prelude.. Lens.mapping Prelude._Coerce

-- | [EC2-VPC] The ID of the subnet to launch the instance into.
--
-- If you specify a network interface, you must specify any subnets as part
-- of the network interface.
runInstances_subnetId :: Lens.Lens' RunInstances (Prelude.Maybe Prelude.Text)
runInstances_subnetId = Lens.lens (\RunInstances' {subnetId} -> subnetId) (\s@RunInstances' {} a -> s {subnetId = a} :: RunInstances)

-- | Indicates whether the instance is enabled for AWS Nitro Enclaves. For
-- more information, see
-- <https://docs.aws.amazon.com/enclaves/latest/user/nitro-enclave.html What is AWS Nitro Enclaves?>
-- in the /AWS Nitro Enclaves User Guide/.
--
-- You can\'t enable AWS Nitro Enclaves and hibernation on the same
-- instance.
runInstances_enclaveOptions :: Lens.Lens' RunInstances (Prelude.Maybe EnclaveOptionsRequest)
runInstances_enclaveOptions = Lens.lens (\RunInstances' {enclaveOptions} -> enclaveOptions) (\s@RunInstances' {} a -> s {enclaveOptions = a} :: RunInstances)

-- | The ID of the kernel.
--
-- We recommend that you use PV-GRUB instead of kernels and RAM disks. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html PV-GRUB>
-- in the /Amazon EC2 User Guide/.
runInstances_kernelId :: Lens.Lens' RunInstances (Prelude.Maybe Prelude.Text)
runInstances_kernelId = Lens.lens (\RunInstances' {kernelId} -> kernelId) (\s@RunInstances' {} a -> s {kernelId = a} :: RunInstances)

-- | The CPU options for the instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-optimize-cpu.html Optimizing CPU options>
-- in the /Amazon EC2 User Guide/.
runInstances_cpuOptions :: Lens.Lens' RunInstances (Prelude.Maybe CpuOptionsRequest)
runInstances_cpuOptions = Lens.lens (\RunInstances' {cpuOptions} -> cpuOptions) (\s@RunInstances' {} a -> s {cpuOptions = a} :: RunInstances)

-- | The name of the key pair. You can create a key pair using
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateKeyPair.html CreateKeyPair>
-- or
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportKeyPair.html ImportKeyPair>.
--
-- If you do not specify a key pair, you can\'t connect to the instance
-- unless you choose an AMI that is configured to allow users another way
-- to log in.
runInstances_keyName :: Lens.Lens' RunInstances (Prelude.Maybe Prelude.Text)
runInstances_keyName = Lens.lens (\RunInstances' {keyName} -> keyName) (\s@RunInstances' {} a -> s {keyName = a} :: RunInstances)

-- | The network interfaces to associate with the instance. If you specify a
-- network interface, you must specify any security groups and subnets as
-- part of the network interface.
runInstances_networkInterfaces :: Lens.Lens' RunInstances (Prelude.Maybe [InstanceNetworkInterfaceSpecification])
runInstances_networkInterfaces = Lens.lens (\RunInstances' {networkInterfaces} -> networkInterfaces) (\s@RunInstances' {} a -> s {networkInterfaces = a} :: RunInstances) Prelude.. Lens.mapping Prelude._Coerce

-- | If you set this parameter to @true@, you can\'t terminate the instance
-- using the Amazon EC2 console, CLI, or API; otherwise, you can. To change
-- this attribute after launch, use
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ModifyInstanceAttribute.html ModifyInstanceAttribute>.
-- Alternatively, if you set @InstanceInitiatedShutdownBehavior@ to
-- @terminate@, you can terminate the instance by running the shutdown
-- command from the instance.
--
-- Default: @false@
runInstances_disableApiTermination :: Lens.Lens' RunInstances (Prelude.Maybe Prelude.Bool)
runInstances_disableApiTermination = Lens.lens (\RunInstances' {disableApiTermination} -> disableApiTermination) (\s@RunInstances' {} a -> s {disableApiTermination = a} :: RunInstances)

-- | The metadata options for the instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance metadata and user data>.
runInstances_metadataOptions :: Lens.Lens' RunInstances (Prelude.Maybe InstanceMetadataOptionsRequest)
runInstances_metadataOptions = Lens.lens (\RunInstances' {metadataOptions} -> metadataOptions) (\s@RunInstances' {} a -> s {metadataOptions = a} :: RunInstances)

-- | Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request. If you do not specify a client token, a randomly
-- generated token is used for the request to ensure idempotency.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- Constraints: Maximum 64 ASCII characters
runInstances_clientToken :: Lens.Lens' RunInstances (Prelude.Maybe Prelude.Text)
runInstances_clientToken = Lens.lens (\RunInstances' {clientToken} -> clientToken) (\s@RunInstances' {} a -> s {clientToken = a} :: RunInstances)

-- | [EC2-VPC] The primary IPv4 address. You must specify a value from the
-- IPv4 address range of the subnet.
--
-- Only one private IP address can be designated as primary. You can\'t
-- specify this option if you\'ve specified the option to designate a
-- private IP address as the primary IP address in a network interface
-- specification. You cannot specify this option if you\'re launching more
-- than one instance in the request.
--
-- You cannot specify this option and the network interfaces option in the
-- same request.
runInstances_privateIpAddress :: Lens.Lens' RunInstances (Prelude.Maybe Prelude.Text)
runInstances_privateIpAddress = Lens.lens (\RunInstances' {privateIpAddress} -> privateIpAddress) (\s@RunInstances' {} a -> s {privateIpAddress = a} :: RunInstances)

-- | The maximum number of instances to launch. If you specify more instances
-- than Amazon EC2 can launch in the target Availability Zone, Amazon EC2
-- launches the largest possible number of instances above @MinCount@.
--
-- Constraints: Between 1 and the maximum number you\'re allowed for the
-- specified instance type. For more information about the default limits,
-- and how to request an increase, see
-- <http://aws.amazon.com/ec2/faqs/#How_many_instances_can_I_run_in_Amazon_EC2 How many instances can I run in Amazon EC2>
-- in the Amazon EC2 FAQ.
runInstances_maxCount :: Lens.Lens' RunInstances Prelude.Int
runInstances_maxCount = Lens.lens (\RunInstances' {maxCount} -> maxCount) (\s@RunInstances' {} a -> s {maxCount = a} :: RunInstances)

-- | The minimum number of instances to launch. If you specify a minimum that
-- is more instances than Amazon EC2 can launch in the target Availability
-- Zone, Amazon EC2 launches no instances.
--
-- Constraints: Between 1 and the maximum number you\'re allowed for the
-- specified instance type. For more information about the default limits,
-- and how to request an increase, see
-- <http://aws.amazon.com/ec2/faqs/#How_many_instances_can_I_run_in_Amazon_EC2 How many instances can I run in Amazon EC2>
-- in the Amazon EC2 General FAQ.
runInstances_minCount :: Lens.Lens' RunInstances Prelude.Int
runInstances_minCount = Lens.lens (\RunInstances' {minCount} -> minCount) (\s@RunInstances' {} a -> s {minCount = a} :: RunInstances)

instance Prelude.AWSRequest RunInstances where
  type Rs RunInstances = Reservation
  request = Request.postQuery defaultService
  response =
    Response.receiveXML (\s h x -> Prelude.parseXML x)

instance Prelude.Hashable RunInstances

instance Prelude.NFData RunInstances

instance Prelude.ToHeaders RunInstances where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath RunInstances where
  toPath = Prelude.const "/"

instance Prelude.ToQuery RunInstances where
  toQuery RunInstances' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("RunInstances" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        Prelude.toQuery
          ( Prelude.toQueryList "SecurityGroupId"
              Prelude.<$> securityGroupIds
          ),
        "AdditionalInfo" Prelude.=: additionalInfo,
        Prelude.toQuery
          ( Prelude.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "InstanceType" Prelude.=: instanceType,
        "CapacityReservationSpecification"
          Prelude.=: capacityReservationSpecification,
        "EbsOptimized" Prelude.=: ebsOptimized,
        "UserData" Prelude.=: userData,
        "Placement" Prelude.=: placement,
        Prelude.toQuery
          ( Prelude.toQueryList "Ipv6Address"
              Prelude.<$> ipv6Addresses
          ),
        "DryRun" Prelude.=: dryRun,
        "RamdiskId" Prelude.=: ramdiskId,
        "CreditSpecification" Prelude.=: creditSpecification,
        "InstanceMarketOptions"
          Prelude.=: instanceMarketOptions,
        "LaunchTemplate" Prelude.=: launchTemplate,
        Prelude.toQuery
          ( Prelude.toQueryList "LicenseSpecification"
              Prelude.<$> licenseSpecifications
          ),
        "InstanceInitiatedShutdownBehavior"
          Prelude.=: instanceInitiatedShutdownBehavior,
        "ImageId" Prelude.=: imageId,
        Prelude.toQuery
          ( Prelude.toQueryList "SecurityGroup"
              Prelude.<$> securityGroups
          ),
        Prelude.toQuery
          ( Prelude.toQueryList "ElasticGpuSpecification"
              Prelude.<$> elasticGpuSpecification
          ),
        Prelude.toQuery
          ( Prelude.toQueryList "ElasticInferenceAccelerator"
              Prelude.<$> elasticInferenceAccelerators
          ),
        "IamInstanceProfile" Prelude.=: iamInstanceProfile,
        "HibernationOptions" Prelude.=: hibernationOptions,
        "Ipv6AddressCount" Prelude.=: ipv6AddressCount,
        "Monitoring" Prelude.=: monitoring,
        Prelude.toQuery
          ( Prelude.toQueryList "BlockDeviceMapping"
              Prelude.<$> blockDeviceMappings
          ),
        "SubnetId" Prelude.=: subnetId,
        "EnclaveOptions" Prelude.=: enclaveOptions,
        "KernelId" Prelude.=: kernelId,
        "CpuOptions" Prelude.=: cpuOptions,
        "KeyName" Prelude.=: keyName,
        Prelude.toQuery
          ( Prelude.toQueryList "NetworkInterface"
              Prelude.<$> networkInterfaces
          ),
        "DisableApiTermination"
          Prelude.=: disableApiTermination,
        "MetadataOptions" Prelude.=: metadataOptions,
        "ClientToken" Prelude.=: clientToken,
        "PrivateIpAddress" Prelude.=: privateIpAddress,
        "MaxCount" Prelude.=: maxCount,
        "MinCount" Prelude.=: minCount
      ]
