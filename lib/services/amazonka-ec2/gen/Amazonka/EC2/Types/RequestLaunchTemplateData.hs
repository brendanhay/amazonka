{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EC2.Types.RequestLaunchTemplateData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.RequestLaunchTemplateData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.CreditSpecificationRequest
import Amazonka.EC2.Types.ElasticGpuSpecification
import Amazonka.EC2.Types.InstanceRequirementsRequest
import Amazonka.EC2.Types.InstanceType
import Amazonka.EC2.Types.LaunchTemplateBlockDeviceMappingRequest
import Amazonka.EC2.Types.LaunchTemplateCapacityReservationSpecificationRequest
import Amazonka.EC2.Types.LaunchTemplateCpuOptionsRequest
import Amazonka.EC2.Types.LaunchTemplateElasticInferenceAccelerator
import Amazonka.EC2.Types.LaunchTemplateEnclaveOptionsRequest
import Amazonka.EC2.Types.LaunchTemplateHibernationOptionsRequest
import Amazonka.EC2.Types.LaunchTemplateIamInstanceProfileSpecificationRequest
import Amazonka.EC2.Types.LaunchTemplateInstanceMaintenanceOptionsRequest
import Amazonka.EC2.Types.LaunchTemplateInstanceMarketOptionsRequest
import Amazonka.EC2.Types.LaunchTemplateInstanceMetadataOptionsRequest
import Amazonka.EC2.Types.LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
import Amazonka.EC2.Types.LaunchTemplateLicenseConfigurationRequest
import Amazonka.EC2.Types.LaunchTemplatePlacementRequest
import Amazonka.EC2.Types.LaunchTemplatePrivateDnsNameOptionsRequest
import Amazonka.EC2.Types.LaunchTemplateTagSpecificationRequest
import Amazonka.EC2.Types.LaunchTemplatesMonitoringRequest
import Amazonka.EC2.Types.ShutdownBehavior
import qualified Amazonka.Prelude as Prelude

-- | The information to include in the launch template.
--
-- You must specify at least one parameter for the launch template data.
--
-- /See:/ 'newRequestLaunchTemplateData' smart constructor.
data RequestLaunchTemplateData = RequestLaunchTemplateData'
  { -- | Indicates whether the instance is optimized for Amazon EBS I\/O. This
    -- optimization provides dedicated throughput to Amazon EBS and an
    -- optimized configuration stack to provide optimal Amazon EBS I\/O
    -- performance. This optimization isn\'t available with all instance types.
    -- Additional usage charges apply when using an EBS-optimized instance.
    ebsOptimized :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether an instance is enabled for hibernation. This parameter
    -- is valid only if the instance meets the
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/hibernating-prerequisites.html hibernation prerequisites>.
    -- For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate your instance>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    hibernationOptions :: Prelude.Maybe LaunchTemplateHibernationOptionsRequest,
    -- | The name or Amazon Resource Name (ARN) of an IAM instance profile.
    iamInstanceProfile :: Prelude.Maybe LaunchTemplateIamInstanceProfileSpecificationRequest,
    -- | The elastic inference accelerator for the instance.
    elasticInferenceAccelerators :: Prelude.Maybe [LaunchTemplateElasticInferenceAccelerator],
    -- | The placement for the instance.
    placement :: Prelude.Maybe LaunchTemplatePlacementRequest,
    -- | The user data to make available to the instance. You must provide
    -- base64-encoded text. User data is limited to 16 KB. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/user-data.html Run commands on your Linux instance at launch>
    -- (Linux) or
    -- <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/instancedata-add-user-data.html Work with instance user data>
    -- (Windows) in the /Amazon Elastic Compute Cloud User Guide/.
    --
    -- If you are creating the launch template for use with Batch, the user
    -- data must be provided in the
    -- <https://cloudinit.readthedocs.io/en/latest/topics/format.html#mime-multi-part-archive MIME multi-part archive format>.
    -- For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/launch-templates.html Amazon EC2 user data in launch templates>
    -- in the /Batch User Guide/.
    userData :: Prelude.Maybe Prelude.Text,
    -- | The attributes for the instance types. When you specify instance
    -- attributes, Amazon EC2 will identify instance types with these
    -- attributes.
    --
    -- If you specify @InstanceRequirements@, you can\'t specify
    -- @InstanceType@.
    instanceRequirements :: Prelude.Maybe InstanceRequirementsRequest,
    -- | The credit option for CPU usage of the instance. Valid only for T
    -- instances.
    creditSpecification :: Prelude.Maybe CreditSpecificationRequest,
    -- | The block device mapping.
    blockDeviceMappings :: Prelude.Maybe [LaunchTemplateBlockDeviceMappingRequest],
    -- | One or more security group IDs. You can create a security group using
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateSecurityGroup.html CreateSecurityGroup>.
    -- You cannot specify both a security group ID and security name in the
    -- same request.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | Indicates whether an instance stops or terminates when you initiate
    -- shutdown from the instance (using the operating system command for
    -- system shutdown).
    --
    -- Default: @stop@
    instanceInitiatedShutdownBehavior :: Prelude.Maybe ShutdownBehavior,
    -- | The monitoring for the instance.
    monitoring :: Prelude.Maybe LaunchTemplatesMonitoringRequest,
    -- | The market (purchasing) option for the instances.
    instanceMarketOptions :: Prelude.Maybe LaunchTemplateInstanceMarketOptionsRequest,
    -- | The Capacity Reservation targeting option. If you do not specify this
    -- parameter, the instance\'s Capacity Reservation preference defaults to
    -- @open@, which enables it to run in any open Capacity Reservation that
    -- has matching attributes (instance type, platform, Availability Zone).
    capacityReservationSpecification :: Prelude.Maybe LaunchTemplateCapacityReservationSpecificationRequest,
    -- | The instance type. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    --
    -- If you specify @InstanceType@, you can\'t specify
    -- @InstanceRequirements@.
    instanceType :: Prelude.Maybe InstanceType,
    -- | One or more security group names. For a nondefault VPC, you must use
    -- security group IDs instead. You cannot specify both a security group ID
    -- and security name in the same request.
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | An elastic GPU to associate with the instance.
    elasticGpuSpecifications :: Prelude.Maybe [ElasticGpuSpecification],
    -- | The ID of the RAM disk.
    --
    -- We recommend that you use PV-GRUB instead of kernels and RAM disks. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html User provided kernels>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    ramDiskId :: Prelude.Maybe Prelude.Text,
    -- | The maintenance options for the instance.
    maintenanceOptions :: Prelude.Maybe LaunchTemplateInstanceMaintenanceOptionsRequest,
    -- | The options for the instance hostname. The default values are inherited
    -- from the subnet.
    privateDnsNameOptions :: Prelude.Maybe LaunchTemplatePrivateDnsNameOptionsRequest,
    -- | The name of the key pair. You can create a key pair using
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateKeyPair.html CreateKeyPair>
    -- or
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportKeyPair.html ImportKeyPair>.
    --
    -- If you do not specify a key pair, you can\'t connect to the instance
    -- unless you choose an AMI that is configured to allow users another way
    -- to log in.
    keyName :: Prelude.Maybe Prelude.Text,
    -- | The license configurations.
    licenseSpecifications :: Prelude.Maybe [LaunchTemplateLicenseConfigurationRequest],
    -- | The ID of the kernel.
    --
    -- We recommend that you use PV-GRUB instead of kernels and RAM disks. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html User provided kernels>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    kernelId :: Prelude.Maybe Prelude.Text,
    -- | If you set this parameter to @true@, you can\'t terminate the instance
    -- using the Amazon EC2 console, CLI, or API; otherwise, you can. To change
    -- this attribute after launch, use
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ModifyInstanceAttribute.html ModifyInstanceAttribute>.
    -- Alternatively, if you set @InstanceInitiatedShutdownBehavior@ to
    -- @terminate@, you can terminate the instance by running the shutdown
    -- command from the instance.
    disableApiTermination :: Prelude.Maybe Prelude.Bool,
    -- | The tags to apply to the resources that are created during instance
    -- launch.
    --
    -- You can specify tags for the following resources only:
    --
    -- -   Instances
    --
    -- -   Volumes
    --
    -- -   Elastic graphics
    --
    -- -   Spot Instance requests
    --
    -- -   Network interfaces
    --
    -- To tag a resource after it has been created, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags>.
    --
    -- To tag the launch template itself, you must use the
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateLaunchTemplate.html TagSpecification>
    -- parameter.
    tagSpecifications :: Prelude.Maybe [LaunchTemplateTagSpecificationRequest],
    -- | The CPU options for the instance. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-optimize-cpu.html Optimizing CPU Options>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    cpuOptions :: Prelude.Maybe LaunchTemplateCpuOptionsRequest,
    -- | Indicates whether to enable the instance for stop protection. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Stop_Start.html#Using_StopProtection Stop Protection>.
    disableApiStop :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the AMI.
    imageId :: Prelude.Maybe Prelude.Text,
    -- | One or more network interfaces. If you specify a network interface, you
    -- must specify any security groups and subnets as part of the network
    -- interface.
    networkInterfaces :: Prelude.Maybe [LaunchTemplateInstanceNetworkInterfaceSpecificationRequest],
    -- | Indicates whether the instance is enabled for Amazon Web Services Nitro
    -- Enclaves. For more information, see
    -- <https://docs.aws.amazon.com/enclaves/latest/user/nitro-enclave.html What is Amazon Web Services Nitro Enclaves?>
    -- in the /Amazon Web Services Nitro Enclaves User Guide/.
    --
    -- You can\'t enable Amazon Web Services Nitro Enclaves and hibernation on
    -- the same instance.
    enclaveOptions :: Prelude.Maybe LaunchTemplateEnclaveOptionsRequest,
    -- | The metadata options for the instance. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance metadata and user data>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    metadataOptions :: Prelude.Maybe LaunchTemplateInstanceMetadataOptionsRequest
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RequestLaunchTemplateData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ebsOptimized', 'requestLaunchTemplateData_ebsOptimized' - Indicates whether the instance is optimized for Amazon EBS I\/O. This
-- optimization provides dedicated throughput to Amazon EBS and an
-- optimized configuration stack to provide optimal Amazon EBS I\/O
-- performance. This optimization isn\'t available with all instance types.
-- Additional usage charges apply when using an EBS-optimized instance.
--
-- 'hibernationOptions', 'requestLaunchTemplateData_hibernationOptions' - Indicates whether an instance is enabled for hibernation. This parameter
-- is valid only if the instance meets the
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/hibernating-prerequisites.html hibernation prerequisites>.
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate your instance>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'iamInstanceProfile', 'requestLaunchTemplateData_iamInstanceProfile' - The name or Amazon Resource Name (ARN) of an IAM instance profile.
--
-- 'elasticInferenceAccelerators', 'requestLaunchTemplateData_elasticInferenceAccelerators' - The elastic inference accelerator for the instance.
--
-- 'placement', 'requestLaunchTemplateData_placement' - The placement for the instance.
--
-- 'userData', 'requestLaunchTemplateData_userData' - The user data to make available to the instance. You must provide
-- base64-encoded text. User data is limited to 16 KB. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/user-data.html Run commands on your Linux instance at launch>
-- (Linux) or
-- <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/instancedata-add-user-data.html Work with instance user data>
-- (Windows) in the /Amazon Elastic Compute Cloud User Guide/.
--
-- If you are creating the launch template for use with Batch, the user
-- data must be provided in the
-- <https://cloudinit.readthedocs.io/en/latest/topics/format.html#mime-multi-part-archive MIME multi-part archive format>.
-- For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/launch-templates.html Amazon EC2 user data in launch templates>
-- in the /Batch User Guide/.
--
-- 'instanceRequirements', 'requestLaunchTemplateData_instanceRequirements' - The attributes for the instance types. When you specify instance
-- attributes, Amazon EC2 will identify instance types with these
-- attributes.
--
-- If you specify @InstanceRequirements@, you can\'t specify
-- @InstanceType@.
--
-- 'creditSpecification', 'requestLaunchTemplateData_creditSpecification' - The credit option for CPU usage of the instance. Valid only for T
-- instances.
--
-- 'blockDeviceMappings', 'requestLaunchTemplateData_blockDeviceMappings' - The block device mapping.
--
-- 'securityGroupIds', 'requestLaunchTemplateData_securityGroupIds' - One or more security group IDs. You can create a security group using
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateSecurityGroup.html CreateSecurityGroup>.
-- You cannot specify both a security group ID and security name in the
-- same request.
--
-- 'instanceInitiatedShutdownBehavior', 'requestLaunchTemplateData_instanceInitiatedShutdownBehavior' - Indicates whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for
-- system shutdown).
--
-- Default: @stop@
--
-- 'monitoring', 'requestLaunchTemplateData_monitoring' - The monitoring for the instance.
--
-- 'instanceMarketOptions', 'requestLaunchTemplateData_instanceMarketOptions' - The market (purchasing) option for the instances.
--
-- 'capacityReservationSpecification', 'requestLaunchTemplateData_capacityReservationSpecification' - The Capacity Reservation targeting option. If you do not specify this
-- parameter, the instance\'s Capacity Reservation preference defaults to
-- @open@, which enables it to run in any open Capacity Reservation that
-- has matching attributes (instance type, platform, Availability Zone).
--
-- 'instanceType', 'requestLaunchTemplateData_instanceType' - The instance type. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- If you specify @InstanceType@, you can\'t specify
-- @InstanceRequirements@.
--
-- 'securityGroups', 'requestLaunchTemplateData_securityGroups' - One or more security group names. For a nondefault VPC, you must use
-- security group IDs instead. You cannot specify both a security group ID
-- and security name in the same request.
--
-- 'elasticGpuSpecifications', 'requestLaunchTemplateData_elasticGpuSpecifications' - An elastic GPU to associate with the instance.
--
-- 'ramDiskId', 'requestLaunchTemplateData_ramDiskId' - The ID of the RAM disk.
--
-- We recommend that you use PV-GRUB instead of kernels and RAM disks. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html User provided kernels>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'maintenanceOptions', 'requestLaunchTemplateData_maintenanceOptions' - The maintenance options for the instance.
--
-- 'privateDnsNameOptions', 'requestLaunchTemplateData_privateDnsNameOptions' - The options for the instance hostname. The default values are inherited
-- from the subnet.
--
-- 'keyName', 'requestLaunchTemplateData_keyName' - The name of the key pair. You can create a key pair using
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateKeyPair.html CreateKeyPair>
-- or
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportKeyPair.html ImportKeyPair>.
--
-- If you do not specify a key pair, you can\'t connect to the instance
-- unless you choose an AMI that is configured to allow users another way
-- to log in.
--
-- 'licenseSpecifications', 'requestLaunchTemplateData_licenseSpecifications' - The license configurations.
--
-- 'kernelId', 'requestLaunchTemplateData_kernelId' - The ID of the kernel.
--
-- We recommend that you use PV-GRUB instead of kernels and RAM disks. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html User provided kernels>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'disableApiTermination', 'requestLaunchTemplateData_disableApiTermination' - If you set this parameter to @true@, you can\'t terminate the instance
-- using the Amazon EC2 console, CLI, or API; otherwise, you can. To change
-- this attribute after launch, use
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ModifyInstanceAttribute.html ModifyInstanceAttribute>.
-- Alternatively, if you set @InstanceInitiatedShutdownBehavior@ to
-- @terminate@, you can terminate the instance by running the shutdown
-- command from the instance.
--
-- 'tagSpecifications', 'requestLaunchTemplateData_tagSpecifications' - The tags to apply to the resources that are created during instance
-- launch.
--
-- You can specify tags for the following resources only:
--
-- -   Instances
--
-- -   Volumes
--
-- -   Elastic graphics
--
-- -   Spot Instance requests
--
-- -   Network interfaces
--
-- To tag a resource after it has been created, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags>.
--
-- To tag the launch template itself, you must use the
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateLaunchTemplate.html TagSpecification>
-- parameter.
--
-- 'cpuOptions', 'requestLaunchTemplateData_cpuOptions' - The CPU options for the instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-optimize-cpu.html Optimizing CPU Options>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'disableApiStop', 'requestLaunchTemplateData_disableApiStop' - Indicates whether to enable the instance for stop protection. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Stop_Start.html#Using_StopProtection Stop Protection>.
--
-- 'imageId', 'requestLaunchTemplateData_imageId' - The ID of the AMI.
--
-- 'networkInterfaces', 'requestLaunchTemplateData_networkInterfaces' - One or more network interfaces. If you specify a network interface, you
-- must specify any security groups and subnets as part of the network
-- interface.
--
-- 'enclaveOptions', 'requestLaunchTemplateData_enclaveOptions' - Indicates whether the instance is enabled for Amazon Web Services Nitro
-- Enclaves. For more information, see
-- <https://docs.aws.amazon.com/enclaves/latest/user/nitro-enclave.html What is Amazon Web Services Nitro Enclaves?>
-- in the /Amazon Web Services Nitro Enclaves User Guide/.
--
-- You can\'t enable Amazon Web Services Nitro Enclaves and hibernation on
-- the same instance.
--
-- 'metadataOptions', 'requestLaunchTemplateData_metadataOptions' - The metadata options for the instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance metadata and user data>
-- in the /Amazon Elastic Compute Cloud User Guide/.
newRequestLaunchTemplateData ::
  RequestLaunchTemplateData
newRequestLaunchTemplateData =
  RequestLaunchTemplateData'
    { ebsOptimized =
        Prelude.Nothing,
      hibernationOptions = Prelude.Nothing,
      iamInstanceProfile = Prelude.Nothing,
      elasticInferenceAccelerators = Prelude.Nothing,
      placement = Prelude.Nothing,
      userData = Prelude.Nothing,
      instanceRequirements = Prelude.Nothing,
      creditSpecification = Prelude.Nothing,
      blockDeviceMappings = Prelude.Nothing,
      securityGroupIds = Prelude.Nothing,
      instanceInitiatedShutdownBehavior =
        Prelude.Nothing,
      monitoring = Prelude.Nothing,
      instanceMarketOptions = Prelude.Nothing,
      capacityReservationSpecification =
        Prelude.Nothing,
      instanceType = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      elasticGpuSpecifications = Prelude.Nothing,
      ramDiskId = Prelude.Nothing,
      maintenanceOptions = Prelude.Nothing,
      privateDnsNameOptions = Prelude.Nothing,
      keyName = Prelude.Nothing,
      licenseSpecifications = Prelude.Nothing,
      kernelId = Prelude.Nothing,
      disableApiTermination = Prelude.Nothing,
      tagSpecifications = Prelude.Nothing,
      cpuOptions = Prelude.Nothing,
      disableApiStop = Prelude.Nothing,
      imageId = Prelude.Nothing,
      networkInterfaces = Prelude.Nothing,
      enclaveOptions = Prelude.Nothing,
      metadataOptions = Prelude.Nothing
    }

-- | Indicates whether the instance is optimized for Amazon EBS I\/O. This
-- optimization provides dedicated throughput to Amazon EBS and an
-- optimized configuration stack to provide optimal Amazon EBS I\/O
-- performance. This optimization isn\'t available with all instance types.
-- Additional usage charges apply when using an EBS-optimized instance.
requestLaunchTemplateData_ebsOptimized :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe Prelude.Bool)
requestLaunchTemplateData_ebsOptimized = Lens.lens (\RequestLaunchTemplateData' {ebsOptimized} -> ebsOptimized) (\s@RequestLaunchTemplateData' {} a -> s {ebsOptimized = a} :: RequestLaunchTemplateData)

-- | Indicates whether an instance is enabled for hibernation. This parameter
-- is valid only if the instance meets the
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/hibernating-prerequisites.html hibernation prerequisites>.
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate your instance>
-- in the /Amazon Elastic Compute Cloud User Guide/.
requestLaunchTemplateData_hibernationOptions :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe LaunchTemplateHibernationOptionsRequest)
requestLaunchTemplateData_hibernationOptions = Lens.lens (\RequestLaunchTemplateData' {hibernationOptions} -> hibernationOptions) (\s@RequestLaunchTemplateData' {} a -> s {hibernationOptions = a} :: RequestLaunchTemplateData)

-- | The name or Amazon Resource Name (ARN) of an IAM instance profile.
requestLaunchTemplateData_iamInstanceProfile :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe LaunchTemplateIamInstanceProfileSpecificationRequest)
requestLaunchTemplateData_iamInstanceProfile = Lens.lens (\RequestLaunchTemplateData' {iamInstanceProfile} -> iamInstanceProfile) (\s@RequestLaunchTemplateData' {} a -> s {iamInstanceProfile = a} :: RequestLaunchTemplateData)

-- | The elastic inference accelerator for the instance.
requestLaunchTemplateData_elasticInferenceAccelerators :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe [LaunchTemplateElasticInferenceAccelerator])
requestLaunchTemplateData_elasticInferenceAccelerators = Lens.lens (\RequestLaunchTemplateData' {elasticInferenceAccelerators} -> elasticInferenceAccelerators) (\s@RequestLaunchTemplateData' {} a -> s {elasticInferenceAccelerators = a} :: RequestLaunchTemplateData) Prelude.. Lens.mapping Lens.coerced

-- | The placement for the instance.
requestLaunchTemplateData_placement :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe LaunchTemplatePlacementRequest)
requestLaunchTemplateData_placement = Lens.lens (\RequestLaunchTemplateData' {placement} -> placement) (\s@RequestLaunchTemplateData' {} a -> s {placement = a} :: RequestLaunchTemplateData)

-- | The user data to make available to the instance. You must provide
-- base64-encoded text. User data is limited to 16 KB. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/user-data.html Run commands on your Linux instance at launch>
-- (Linux) or
-- <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/instancedata-add-user-data.html Work with instance user data>
-- (Windows) in the /Amazon Elastic Compute Cloud User Guide/.
--
-- If you are creating the launch template for use with Batch, the user
-- data must be provided in the
-- <https://cloudinit.readthedocs.io/en/latest/topics/format.html#mime-multi-part-archive MIME multi-part archive format>.
-- For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/launch-templates.html Amazon EC2 user data in launch templates>
-- in the /Batch User Guide/.
requestLaunchTemplateData_userData :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe Prelude.Text)
requestLaunchTemplateData_userData = Lens.lens (\RequestLaunchTemplateData' {userData} -> userData) (\s@RequestLaunchTemplateData' {} a -> s {userData = a} :: RequestLaunchTemplateData)

-- | The attributes for the instance types. When you specify instance
-- attributes, Amazon EC2 will identify instance types with these
-- attributes.
--
-- If you specify @InstanceRequirements@, you can\'t specify
-- @InstanceType@.
requestLaunchTemplateData_instanceRequirements :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe InstanceRequirementsRequest)
requestLaunchTemplateData_instanceRequirements = Lens.lens (\RequestLaunchTemplateData' {instanceRequirements} -> instanceRequirements) (\s@RequestLaunchTemplateData' {} a -> s {instanceRequirements = a} :: RequestLaunchTemplateData)

-- | The credit option for CPU usage of the instance. Valid only for T
-- instances.
requestLaunchTemplateData_creditSpecification :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe CreditSpecificationRequest)
requestLaunchTemplateData_creditSpecification = Lens.lens (\RequestLaunchTemplateData' {creditSpecification} -> creditSpecification) (\s@RequestLaunchTemplateData' {} a -> s {creditSpecification = a} :: RequestLaunchTemplateData)

-- | The block device mapping.
requestLaunchTemplateData_blockDeviceMappings :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe [LaunchTemplateBlockDeviceMappingRequest])
requestLaunchTemplateData_blockDeviceMappings = Lens.lens (\RequestLaunchTemplateData' {blockDeviceMappings} -> blockDeviceMappings) (\s@RequestLaunchTemplateData' {} a -> s {blockDeviceMappings = a} :: RequestLaunchTemplateData) Prelude.. Lens.mapping Lens.coerced

-- | One or more security group IDs. You can create a security group using
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateSecurityGroup.html CreateSecurityGroup>.
-- You cannot specify both a security group ID and security name in the
-- same request.
requestLaunchTemplateData_securityGroupIds :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe [Prelude.Text])
requestLaunchTemplateData_securityGroupIds = Lens.lens (\RequestLaunchTemplateData' {securityGroupIds} -> securityGroupIds) (\s@RequestLaunchTemplateData' {} a -> s {securityGroupIds = a} :: RequestLaunchTemplateData) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for
-- system shutdown).
--
-- Default: @stop@
requestLaunchTemplateData_instanceInitiatedShutdownBehavior :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe ShutdownBehavior)
requestLaunchTemplateData_instanceInitiatedShutdownBehavior = Lens.lens (\RequestLaunchTemplateData' {instanceInitiatedShutdownBehavior} -> instanceInitiatedShutdownBehavior) (\s@RequestLaunchTemplateData' {} a -> s {instanceInitiatedShutdownBehavior = a} :: RequestLaunchTemplateData)

-- | The monitoring for the instance.
requestLaunchTemplateData_monitoring :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe LaunchTemplatesMonitoringRequest)
requestLaunchTemplateData_monitoring = Lens.lens (\RequestLaunchTemplateData' {monitoring} -> monitoring) (\s@RequestLaunchTemplateData' {} a -> s {monitoring = a} :: RequestLaunchTemplateData)

-- | The market (purchasing) option for the instances.
requestLaunchTemplateData_instanceMarketOptions :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe LaunchTemplateInstanceMarketOptionsRequest)
requestLaunchTemplateData_instanceMarketOptions = Lens.lens (\RequestLaunchTemplateData' {instanceMarketOptions} -> instanceMarketOptions) (\s@RequestLaunchTemplateData' {} a -> s {instanceMarketOptions = a} :: RequestLaunchTemplateData)

-- | The Capacity Reservation targeting option. If you do not specify this
-- parameter, the instance\'s Capacity Reservation preference defaults to
-- @open@, which enables it to run in any open Capacity Reservation that
-- has matching attributes (instance type, platform, Availability Zone).
requestLaunchTemplateData_capacityReservationSpecification :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe LaunchTemplateCapacityReservationSpecificationRequest)
requestLaunchTemplateData_capacityReservationSpecification = Lens.lens (\RequestLaunchTemplateData' {capacityReservationSpecification} -> capacityReservationSpecification) (\s@RequestLaunchTemplateData' {} a -> s {capacityReservationSpecification = a} :: RequestLaunchTemplateData)

-- | The instance type. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- If you specify @InstanceType@, you can\'t specify
-- @InstanceRequirements@.
requestLaunchTemplateData_instanceType :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe InstanceType)
requestLaunchTemplateData_instanceType = Lens.lens (\RequestLaunchTemplateData' {instanceType} -> instanceType) (\s@RequestLaunchTemplateData' {} a -> s {instanceType = a} :: RequestLaunchTemplateData)

-- | One or more security group names. For a nondefault VPC, you must use
-- security group IDs instead. You cannot specify both a security group ID
-- and security name in the same request.
requestLaunchTemplateData_securityGroups :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe [Prelude.Text])
requestLaunchTemplateData_securityGroups = Lens.lens (\RequestLaunchTemplateData' {securityGroups} -> securityGroups) (\s@RequestLaunchTemplateData' {} a -> s {securityGroups = a} :: RequestLaunchTemplateData) Prelude.. Lens.mapping Lens.coerced

-- | An elastic GPU to associate with the instance.
requestLaunchTemplateData_elasticGpuSpecifications :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe [ElasticGpuSpecification])
requestLaunchTemplateData_elasticGpuSpecifications = Lens.lens (\RequestLaunchTemplateData' {elasticGpuSpecifications} -> elasticGpuSpecifications) (\s@RequestLaunchTemplateData' {} a -> s {elasticGpuSpecifications = a} :: RequestLaunchTemplateData) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the RAM disk.
--
-- We recommend that you use PV-GRUB instead of kernels and RAM disks. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html User provided kernels>
-- in the /Amazon Elastic Compute Cloud User Guide/.
requestLaunchTemplateData_ramDiskId :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe Prelude.Text)
requestLaunchTemplateData_ramDiskId = Lens.lens (\RequestLaunchTemplateData' {ramDiskId} -> ramDiskId) (\s@RequestLaunchTemplateData' {} a -> s {ramDiskId = a} :: RequestLaunchTemplateData)

-- | The maintenance options for the instance.
requestLaunchTemplateData_maintenanceOptions :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe LaunchTemplateInstanceMaintenanceOptionsRequest)
requestLaunchTemplateData_maintenanceOptions = Lens.lens (\RequestLaunchTemplateData' {maintenanceOptions} -> maintenanceOptions) (\s@RequestLaunchTemplateData' {} a -> s {maintenanceOptions = a} :: RequestLaunchTemplateData)

-- | The options for the instance hostname. The default values are inherited
-- from the subnet.
requestLaunchTemplateData_privateDnsNameOptions :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe LaunchTemplatePrivateDnsNameOptionsRequest)
requestLaunchTemplateData_privateDnsNameOptions = Lens.lens (\RequestLaunchTemplateData' {privateDnsNameOptions} -> privateDnsNameOptions) (\s@RequestLaunchTemplateData' {} a -> s {privateDnsNameOptions = a} :: RequestLaunchTemplateData)

-- | The name of the key pair. You can create a key pair using
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateKeyPair.html CreateKeyPair>
-- or
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportKeyPair.html ImportKeyPair>.
--
-- If you do not specify a key pair, you can\'t connect to the instance
-- unless you choose an AMI that is configured to allow users another way
-- to log in.
requestLaunchTemplateData_keyName :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe Prelude.Text)
requestLaunchTemplateData_keyName = Lens.lens (\RequestLaunchTemplateData' {keyName} -> keyName) (\s@RequestLaunchTemplateData' {} a -> s {keyName = a} :: RequestLaunchTemplateData)

-- | The license configurations.
requestLaunchTemplateData_licenseSpecifications :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe [LaunchTemplateLicenseConfigurationRequest])
requestLaunchTemplateData_licenseSpecifications = Lens.lens (\RequestLaunchTemplateData' {licenseSpecifications} -> licenseSpecifications) (\s@RequestLaunchTemplateData' {} a -> s {licenseSpecifications = a} :: RequestLaunchTemplateData) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the kernel.
--
-- We recommend that you use PV-GRUB instead of kernels and RAM disks. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html User provided kernels>
-- in the /Amazon Elastic Compute Cloud User Guide/.
requestLaunchTemplateData_kernelId :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe Prelude.Text)
requestLaunchTemplateData_kernelId = Lens.lens (\RequestLaunchTemplateData' {kernelId} -> kernelId) (\s@RequestLaunchTemplateData' {} a -> s {kernelId = a} :: RequestLaunchTemplateData)

-- | If you set this parameter to @true@, you can\'t terminate the instance
-- using the Amazon EC2 console, CLI, or API; otherwise, you can. To change
-- this attribute after launch, use
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ModifyInstanceAttribute.html ModifyInstanceAttribute>.
-- Alternatively, if you set @InstanceInitiatedShutdownBehavior@ to
-- @terminate@, you can terminate the instance by running the shutdown
-- command from the instance.
requestLaunchTemplateData_disableApiTermination :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe Prelude.Bool)
requestLaunchTemplateData_disableApiTermination = Lens.lens (\RequestLaunchTemplateData' {disableApiTermination} -> disableApiTermination) (\s@RequestLaunchTemplateData' {} a -> s {disableApiTermination = a} :: RequestLaunchTemplateData)

-- | The tags to apply to the resources that are created during instance
-- launch.
--
-- You can specify tags for the following resources only:
--
-- -   Instances
--
-- -   Volumes
--
-- -   Elastic graphics
--
-- -   Spot Instance requests
--
-- -   Network interfaces
--
-- To tag a resource after it has been created, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags>.
--
-- To tag the launch template itself, you must use the
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateLaunchTemplate.html TagSpecification>
-- parameter.
requestLaunchTemplateData_tagSpecifications :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe [LaunchTemplateTagSpecificationRequest])
requestLaunchTemplateData_tagSpecifications = Lens.lens (\RequestLaunchTemplateData' {tagSpecifications} -> tagSpecifications) (\s@RequestLaunchTemplateData' {} a -> s {tagSpecifications = a} :: RequestLaunchTemplateData) Prelude.. Lens.mapping Lens.coerced

-- | The CPU options for the instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-optimize-cpu.html Optimizing CPU Options>
-- in the /Amazon Elastic Compute Cloud User Guide/.
requestLaunchTemplateData_cpuOptions :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe LaunchTemplateCpuOptionsRequest)
requestLaunchTemplateData_cpuOptions = Lens.lens (\RequestLaunchTemplateData' {cpuOptions} -> cpuOptions) (\s@RequestLaunchTemplateData' {} a -> s {cpuOptions = a} :: RequestLaunchTemplateData)

-- | Indicates whether to enable the instance for stop protection. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Stop_Start.html#Using_StopProtection Stop Protection>.
requestLaunchTemplateData_disableApiStop :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe Prelude.Bool)
requestLaunchTemplateData_disableApiStop = Lens.lens (\RequestLaunchTemplateData' {disableApiStop} -> disableApiStop) (\s@RequestLaunchTemplateData' {} a -> s {disableApiStop = a} :: RequestLaunchTemplateData)

-- | The ID of the AMI.
requestLaunchTemplateData_imageId :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe Prelude.Text)
requestLaunchTemplateData_imageId = Lens.lens (\RequestLaunchTemplateData' {imageId} -> imageId) (\s@RequestLaunchTemplateData' {} a -> s {imageId = a} :: RequestLaunchTemplateData)

-- | One or more network interfaces. If you specify a network interface, you
-- must specify any security groups and subnets as part of the network
-- interface.
requestLaunchTemplateData_networkInterfaces :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe [LaunchTemplateInstanceNetworkInterfaceSpecificationRequest])
requestLaunchTemplateData_networkInterfaces = Lens.lens (\RequestLaunchTemplateData' {networkInterfaces} -> networkInterfaces) (\s@RequestLaunchTemplateData' {} a -> s {networkInterfaces = a} :: RequestLaunchTemplateData) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether the instance is enabled for Amazon Web Services Nitro
-- Enclaves. For more information, see
-- <https://docs.aws.amazon.com/enclaves/latest/user/nitro-enclave.html What is Amazon Web Services Nitro Enclaves?>
-- in the /Amazon Web Services Nitro Enclaves User Guide/.
--
-- You can\'t enable Amazon Web Services Nitro Enclaves and hibernation on
-- the same instance.
requestLaunchTemplateData_enclaveOptions :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe LaunchTemplateEnclaveOptionsRequest)
requestLaunchTemplateData_enclaveOptions = Lens.lens (\RequestLaunchTemplateData' {enclaveOptions} -> enclaveOptions) (\s@RequestLaunchTemplateData' {} a -> s {enclaveOptions = a} :: RequestLaunchTemplateData)

-- | The metadata options for the instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance metadata and user data>
-- in the /Amazon Elastic Compute Cloud User Guide/.
requestLaunchTemplateData_metadataOptions :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe LaunchTemplateInstanceMetadataOptionsRequest)
requestLaunchTemplateData_metadataOptions = Lens.lens (\RequestLaunchTemplateData' {metadataOptions} -> metadataOptions) (\s@RequestLaunchTemplateData' {} a -> s {metadataOptions = a} :: RequestLaunchTemplateData)

instance Prelude.Hashable RequestLaunchTemplateData where
  hashWithSalt _salt RequestLaunchTemplateData' {..} =
    _salt `Prelude.hashWithSalt` ebsOptimized
      `Prelude.hashWithSalt` hibernationOptions
      `Prelude.hashWithSalt` iamInstanceProfile
      `Prelude.hashWithSalt` elasticInferenceAccelerators
      `Prelude.hashWithSalt` placement
      `Prelude.hashWithSalt` userData
      `Prelude.hashWithSalt` instanceRequirements
      `Prelude.hashWithSalt` creditSpecification
      `Prelude.hashWithSalt` blockDeviceMappings
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` instanceInitiatedShutdownBehavior
      `Prelude.hashWithSalt` monitoring
      `Prelude.hashWithSalt` instanceMarketOptions
      `Prelude.hashWithSalt` capacityReservationSpecification
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` securityGroups
      `Prelude.hashWithSalt` elasticGpuSpecifications
      `Prelude.hashWithSalt` ramDiskId
      `Prelude.hashWithSalt` maintenanceOptions
      `Prelude.hashWithSalt` privateDnsNameOptions
      `Prelude.hashWithSalt` keyName
      `Prelude.hashWithSalt` licenseSpecifications
      `Prelude.hashWithSalt` kernelId
      `Prelude.hashWithSalt` disableApiTermination
      `Prelude.hashWithSalt` tagSpecifications
      `Prelude.hashWithSalt` cpuOptions
      `Prelude.hashWithSalt` disableApiStop
      `Prelude.hashWithSalt` imageId
      `Prelude.hashWithSalt` networkInterfaces
      `Prelude.hashWithSalt` enclaveOptions
      `Prelude.hashWithSalt` metadataOptions

instance Prelude.NFData RequestLaunchTemplateData where
  rnf RequestLaunchTemplateData' {..} =
    Prelude.rnf ebsOptimized
      `Prelude.seq` Prelude.rnf hibernationOptions
      `Prelude.seq` Prelude.rnf iamInstanceProfile
      `Prelude.seq` Prelude.rnf elasticInferenceAccelerators
      `Prelude.seq` Prelude.rnf placement
      `Prelude.seq` Prelude.rnf userData
      `Prelude.seq` Prelude.rnf instanceRequirements
      `Prelude.seq` Prelude.rnf creditSpecification
      `Prelude.seq` Prelude.rnf blockDeviceMappings
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf instanceInitiatedShutdownBehavior
      `Prelude.seq` Prelude.rnf monitoring
      `Prelude.seq` Prelude.rnf instanceMarketOptions
      `Prelude.seq` Prelude.rnf
        capacityReservationSpecification
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf securityGroups
      `Prelude.seq` Prelude.rnf elasticGpuSpecifications
      `Prelude.seq` Prelude.rnf ramDiskId
      `Prelude.seq` Prelude.rnf maintenanceOptions
      `Prelude.seq` Prelude.rnf
        privateDnsNameOptions
      `Prelude.seq` Prelude.rnf keyName
      `Prelude.seq` Prelude.rnf
        licenseSpecifications
      `Prelude.seq` Prelude.rnf kernelId
      `Prelude.seq` Prelude.rnf
        disableApiTermination
      `Prelude.seq` Prelude.rnf
        tagSpecifications
      `Prelude.seq` Prelude.rnf
        cpuOptions
      `Prelude.seq` Prelude.rnf
        disableApiStop
      `Prelude.seq` Prelude.rnf
        imageId
      `Prelude.seq` Prelude.rnf
        networkInterfaces
      `Prelude.seq` Prelude.rnf
        enclaveOptions
      `Prelude.seq` Prelude.rnf
        metadataOptions

instance Core.ToQuery RequestLaunchTemplateData where
  toQuery RequestLaunchTemplateData' {..} =
    Prelude.mconcat
      [ "EbsOptimized" Core.=: ebsOptimized,
        "HibernationOptions" Core.=: hibernationOptions,
        "IamInstanceProfile" Core.=: iamInstanceProfile,
        Core.toQuery
          ( Core.toQueryList "ElasticInferenceAccelerator"
              Prelude.<$> elasticInferenceAccelerators
          ),
        "Placement" Core.=: placement,
        "UserData" Core.=: userData,
        "InstanceRequirements" Core.=: instanceRequirements,
        "CreditSpecification" Core.=: creditSpecification,
        Core.toQuery
          ( Core.toQueryList "BlockDeviceMapping"
              Prelude.<$> blockDeviceMappings
          ),
        Core.toQuery
          ( Core.toQueryList "SecurityGroupId"
              Prelude.<$> securityGroupIds
          ),
        "InstanceInitiatedShutdownBehavior"
          Core.=: instanceInitiatedShutdownBehavior,
        "Monitoring" Core.=: monitoring,
        "InstanceMarketOptions"
          Core.=: instanceMarketOptions,
        "CapacityReservationSpecification"
          Core.=: capacityReservationSpecification,
        "InstanceType" Core.=: instanceType,
        Core.toQuery
          ( Core.toQueryList "SecurityGroup"
              Prelude.<$> securityGroups
          ),
        Core.toQuery
          ( Core.toQueryList "ElasticGpuSpecification"
              Prelude.<$> elasticGpuSpecifications
          ),
        "RamDiskId" Core.=: ramDiskId,
        "MaintenanceOptions" Core.=: maintenanceOptions,
        "PrivateDnsNameOptions"
          Core.=: privateDnsNameOptions,
        "KeyName" Core.=: keyName,
        Core.toQuery
          ( Core.toQueryList "LicenseSpecification"
              Prelude.<$> licenseSpecifications
          ),
        "KernelId" Core.=: kernelId,
        "DisableApiTermination"
          Core.=: disableApiTermination,
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "CpuOptions" Core.=: cpuOptions,
        "DisableApiStop" Core.=: disableApiStop,
        "ImageId" Core.=: imageId,
        Core.toQuery
          ( Core.toQueryList "NetworkInterface"
              Prelude.<$> networkInterfaces
          ),
        "EnclaveOptions" Core.=: enclaveOptions,
        "MetadataOptions" Core.=: metadataOptions
      ]
