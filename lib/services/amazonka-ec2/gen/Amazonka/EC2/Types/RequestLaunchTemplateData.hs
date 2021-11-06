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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.RequestLaunchTemplateData where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.CreditSpecificationRequest
import Amazonka.EC2.Types.ElasticGpuSpecification
import Amazonka.EC2.Types.InstanceType
import Amazonka.EC2.Types.LaunchTemplateBlockDeviceMappingRequest
import Amazonka.EC2.Types.LaunchTemplateCapacityReservationSpecificationRequest
import Amazonka.EC2.Types.LaunchTemplateCpuOptionsRequest
import Amazonka.EC2.Types.LaunchTemplateElasticInferenceAccelerator
import Amazonka.EC2.Types.LaunchTemplateEnclaveOptionsRequest
import Amazonka.EC2.Types.LaunchTemplateHibernationOptionsRequest
import Amazonka.EC2.Types.LaunchTemplateIamInstanceProfileSpecificationRequest
import Amazonka.EC2.Types.LaunchTemplateInstanceMarketOptionsRequest
import Amazonka.EC2.Types.LaunchTemplateInstanceMetadataOptionsRequest
import Amazonka.EC2.Types.LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
import Amazonka.EC2.Types.LaunchTemplateLicenseConfigurationRequest
import Amazonka.EC2.Types.LaunchTemplatePlacementRequest
import Amazonka.EC2.Types.LaunchTemplateTagSpecificationRequest
import Amazonka.EC2.Types.LaunchTemplatesMonitoringRequest
import Amazonka.EC2.Types.ShutdownBehavior
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The information to include in the launch template.
--
-- /See:/ 'newRequestLaunchTemplateData' smart constructor.
data RequestLaunchTemplateData = RequestLaunchTemplateData'
  { -- | One or more security group IDs. You can create a security group using
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateSecurityGroup.html CreateSecurityGroup>.
    -- You cannot specify both a security group ID and security name in the
    -- same request.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | [EC2-Classic, default VPC] One or more security group names. For a
    -- nondefault VPC, you must use security group IDs instead. You cannot
    -- specify both a security group ID and security name in the same request.
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | The elastic inference accelerator for the instance.
    elasticInferenceAccelerators :: Prelude.Maybe [LaunchTemplateElasticInferenceAccelerator],
    -- | The market (purchasing) option for the instances.
    instanceMarketOptions :: Prelude.Maybe LaunchTemplateInstanceMarketOptionsRequest,
    -- | The license configurations.
    licenseSpecifications :: Prelude.Maybe [LaunchTemplateLicenseConfigurationRequest],
    -- | If you set this parameter to @true@, you can\'t terminate the instance
    -- using the Amazon EC2 console, CLI, or API; otherwise, you can. To change
    -- this attribute after launch, use
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ModifyInstanceAttribute.html ModifyInstanceAttribute>.
    -- Alternatively, if you set @InstanceInitiatedShutdownBehavior@ to
    -- @terminate@, you can terminate the instance by running the shutdown
    -- command from the instance.
    disableApiTermination :: Prelude.Maybe Prelude.Bool,
    -- | The name of the key pair. You can create a key pair using
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateKeyPair.html CreateKeyPair>
    -- or
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportKeyPair.html ImportKeyPair>.
    --
    -- If you do not specify a key pair, you can\'t connect to the instance
    -- unless you choose an AMI that is configured to allow users another way
    -- to log in.
    keyName :: Prelude.Maybe Prelude.Text,
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
    -- | The CPU options for the instance. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-optimize-cpu.html Optimizing CPU Options>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    cpuOptions :: Prelude.Maybe LaunchTemplateCpuOptionsRequest,
    -- | The ID of the RAM disk.
    --
    -- We recommend that you use PV-GRUB instead of kernels and RAM disks. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html User Provided Kernels>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    ramDiskId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the kernel.
    --
    -- We recommend that you use PV-GRUB instead of kernels and RAM disks. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html User Provided Kernels>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    kernelId :: Prelude.Maybe Prelude.Text,
    -- | An elastic GPU to associate with the instance.
    elasticGpuSpecifications :: Prelude.Maybe [ElasticGpuSpecification],
    -- | The instance type. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    instanceType :: Prelude.Maybe InstanceType,
    -- | The Capacity Reservation targeting option. If you do not specify this
    -- parameter, the instance\'s Capacity Reservation preference defaults to
    -- @open@, which enables it to run in any open Capacity Reservation that
    -- has matching attributes (instance type, platform, Availability Zone).
    capacityReservationSpecification :: Prelude.Maybe LaunchTemplateCapacityReservationSpecificationRequest,
    -- | Indicates whether the instance is optimized for Amazon EBS I\/O. This
    -- optimization provides dedicated throughput to Amazon EBS and an
    -- optimized configuration stack to provide optimal Amazon EBS I\/O
    -- performance. This optimization isn\'t available with all instance types.
    -- Additional usage charges apply when using an EBS-optimized instance.
    ebsOptimized :: Prelude.Maybe Prelude.Bool,
    -- | The user data to make available to the instance. You must provide
    -- base64-encoded text. User data is limited to 16 KB. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/user-data.html Running Commands on Your Linux Instance at Launch>
    -- (Linux) or
    -- <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ec2-instance-metadata.html#instancedata-add-user-data Adding User Data>
    -- (Windows).
    --
    -- If you are creating the launch template for use with Batch, the user
    -- data must be provided in the
    -- <https://cloudinit.readthedocs.io/en/latest/topics/format.html#mime-multi-part-archive MIME multi-part archive format>.
    -- For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/launch-templates.html Amazon EC2 user data in launch templates>
    -- in the /Batch User Guide/.
    userData :: Prelude.Maybe Prelude.Text,
    -- | The monitoring for the instance.
    monitoring :: Prelude.Maybe LaunchTemplatesMonitoringRequest,
    -- | The tags to apply to the resources during launch. You can only tag
    -- instances and volumes on launch. The specified tags are applied to all
    -- instances or volumes that are created during launch. To tag a resource
    -- after it has been created, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags>.
    tagSpecifications :: Prelude.Maybe [LaunchTemplateTagSpecificationRequest],
    -- | Indicates whether an instance is enabled for hibernation. This parameter
    -- is valid only if the instance meets the
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html#hibernating-prerequisites hibernation prerequisites>.
    -- For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate your instance>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    hibernationOptions :: Prelude.Maybe LaunchTemplateHibernationOptionsRequest,
    -- | The name or Amazon Resource Name (ARN) of an IAM instance profile.
    iamInstanceProfile :: Prelude.Maybe LaunchTemplateIamInstanceProfileSpecificationRequest,
    -- | The ID of the AMI.
    imageId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether an instance stops or terminates when you initiate
    -- shutdown from the instance (using the operating system command for
    -- system shutdown).
    --
    -- Default: @stop@
    instanceInitiatedShutdownBehavior :: Prelude.Maybe ShutdownBehavior,
    -- | The metadata options for the instance. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance metadata and user data>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    metadataOptions :: Prelude.Maybe LaunchTemplateInstanceMetadataOptionsRequest,
    -- | The credit option for CPU usage of the instance. Valid for T2, T3, or
    -- T3a instances only.
    creditSpecification :: Prelude.Maybe CreditSpecificationRequest,
    -- | The block device mapping.
    blockDeviceMappings :: Prelude.Maybe [LaunchTemplateBlockDeviceMappingRequest],
    -- | The placement for the instance.
    placement :: Prelude.Maybe LaunchTemplatePlacementRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RequestLaunchTemplateData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'requestLaunchTemplateData_securityGroupIds' - One or more security group IDs. You can create a security group using
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateSecurityGroup.html CreateSecurityGroup>.
-- You cannot specify both a security group ID and security name in the
-- same request.
--
-- 'securityGroups', 'requestLaunchTemplateData_securityGroups' - [EC2-Classic, default VPC] One or more security group names. For a
-- nondefault VPC, you must use security group IDs instead. You cannot
-- specify both a security group ID and security name in the same request.
--
-- 'elasticInferenceAccelerators', 'requestLaunchTemplateData_elasticInferenceAccelerators' - The elastic inference accelerator for the instance.
--
-- 'instanceMarketOptions', 'requestLaunchTemplateData_instanceMarketOptions' - The market (purchasing) option for the instances.
--
-- 'licenseSpecifications', 'requestLaunchTemplateData_licenseSpecifications' - The license configurations.
--
-- 'disableApiTermination', 'requestLaunchTemplateData_disableApiTermination' - If you set this parameter to @true@, you can\'t terminate the instance
-- using the Amazon EC2 console, CLI, or API; otherwise, you can. To change
-- this attribute after launch, use
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ModifyInstanceAttribute.html ModifyInstanceAttribute>.
-- Alternatively, if you set @InstanceInitiatedShutdownBehavior@ to
-- @terminate@, you can terminate the instance by running the shutdown
-- command from the instance.
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
-- 'cpuOptions', 'requestLaunchTemplateData_cpuOptions' - The CPU options for the instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-optimize-cpu.html Optimizing CPU Options>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'ramDiskId', 'requestLaunchTemplateData_ramDiskId' - The ID of the RAM disk.
--
-- We recommend that you use PV-GRUB instead of kernels and RAM disks. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html User Provided Kernels>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'kernelId', 'requestLaunchTemplateData_kernelId' - The ID of the kernel.
--
-- We recommend that you use PV-GRUB instead of kernels and RAM disks. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html User Provided Kernels>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'elasticGpuSpecifications', 'requestLaunchTemplateData_elasticGpuSpecifications' - An elastic GPU to associate with the instance.
--
-- 'instanceType', 'requestLaunchTemplateData_instanceType' - The instance type. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'capacityReservationSpecification', 'requestLaunchTemplateData_capacityReservationSpecification' - The Capacity Reservation targeting option. If you do not specify this
-- parameter, the instance\'s Capacity Reservation preference defaults to
-- @open@, which enables it to run in any open Capacity Reservation that
-- has matching attributes (instance type, platform, Availability Zone).
--
-- 'ebsOptimized', 'requestLaunchTemplateData_ebsOptimized' - Indicates whether the instance is optimized for Amazon EBS I\/O. This
-- optimization provides dedicated throughput to Amazon EBS and an
-- optimized configuration stack to provide optimal Amazon EBS I\/O
-- performance. This optimization isn\'t available with all instance types.
-- Additional usage charges apply when using an EBS-optimized instance.
--
-- 'userData', 'requestLaunchTemplateData_userData' - The user data to make available to the instance. You must provide
-- base64-encoded text. User data is limited to 16 KB. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/user-data.html Running Commands on Your Linux Instance at Launch>
-- (Linux) or
-- <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ec2-instance-metadata.html#instancedata-add-user-data Adding User Data>
-- (Windows).
--
-- If you are creating the launch template for use with Batch, the user
-- data must be provided in the
-- <https://cloudinit.readthedocs.io/en/latest/topics/format.html#mime-multi-part-archive MIME multi-part archive format>.
-- For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/launch-templates.html Amazon EC2 user data in launch templates>
-- in the /Batch User Guide/.
--
-- 'monitoring', 'requestLaunchTemplateData_monitoring' - The monitoring for the instance.
--
-- 'tagSpecifications', 'requestLaunchTemplateData_tagSpecifications' - The tags to apply to the resources during launch. You can only tag
-- instances and volumes on launch. The specified tags are applied to all
-- instances or volumes that are created during launch. To tag a resource
-- after it has been created, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags>.
--
-- 'hibernationOptions', 'requestLaunchTemplateData_hibernationOptions' - Indicates whether an instance is enabled for hibernation. This parameter
-- is valid only if the instance meets the
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html#hibernating-prerequisites hibernation prerequisites>.
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate your instance>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'iamInstanceProfile', 'requestLaunchTemplateData_iamInstanceProfile' - The name or Amazon Resource Name (ARN) of an IAM instance profile.
--
-- 'imageId', 'requestLaunchTemplateData_imageId' - The ID of the AMI.
--
-- 'instanceInitiatedShutdownBehavior', 'requestLaunchTemplateData_instanceInitiatedShutdownBehavior' - Indicates whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for
-- system shutdown).
--
-- Default: @stop@
--
-- 'metadataOptions', 'requestLaunchTemplateData_metadataOptions' - The metadata options for the instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance metadata and user data>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'creditSpecification', 'requestLaunchTemplateData_creditSpecification' - The credit option for CPU usage of the instance. Valid for T2, T3, or
-- T3a instances only.
--
-- 'blockDeviceMappings', 'requestLaunchTemplateData_blockDeviceMappings' - The block device mapping.
--
-- 'placement', 'requestLaunchTemplateData_placement' - The placement for the instance.
newRequestLaunchTemplateData ::
  RequestLaunchTemplateData
newRequestLaunchTemplateData =
  RequestLaunchTemplateData'
    { securityGroupIds =
        Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      elasticInferenceAccelerators = Prelude.Nothing,
      instanceMarketOptions = Prelude.Nothing,
      licenseSpecifications = Prelude.Nothing,
      disableApiTermination = Prelude.Nothing,
      keyName = Prelude.Nothing,
      networkInterfaces = Prelude.Nothing,
      enclaveOptions = Prelude.Nothing,
      cpuOptions = Prelude.Nothing,
      ramDiskId = Prelude.Nothing,
      kernelId = Prelude.Nothing,
      elasticGpuSpecifications = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      capacityReservationSpecification =
        Prelude.Nothing,
      ebsOptimized = Prelude.Nothing,
      userData = Prelude.Nothing,
      monitoring = Prelude.Nothing,
      tagSpecifications = Prelude.Nothing,
      hibernationOptions = Prelude.Nothing,
      iamInstanceProfile = Prelude.Nothing,
      imageId = Prelude.Nothing,
      instanceInitiatedShutdownBehavior =
        Prelude.Nothing,
      metadataOptions = Prelude.Nothing,
      creditSpecification = Prelude.Nothing,
      blockDeviceMappings = Prelude.Nothing,
      placement = Prelude.Nothing
    }

-- | One or more security group IDs. You can create a security group using
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateSecurityGroup.html CreateSecurityGroup>.
-- You cannot specify both a security group ID and security name in the
-- same request.
requestLaunchTemplateData_securityGroupIds :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe [Prelude.Text])
requestLaunchTemplateData_securityGroupIds = Lens.lens (\RequestLaunchTemplateData' {securityGroupIds} -> securityGroupIds) (\s@RequestLaunchTemplateData' {} a -> s {securityGroupIds = a} :: RequestLaunchTemplateData) Prelude.. Lens.mapping Lens.coerced

-- | [EC2-Classic, default VPC] One or more security group names. For a
-- nondefault VPC, you must use security group IDs instead. You cannot
-- specify both a security group ID and security name in the same request.
requestLaunchTemplateData_securityGroups :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe [Prelude.Text])
requestLaunchTemplateData_securityGroups = Lens.lens (\RequestLaunchTemplateData' {securityGroups} -> securityGroups) (\s@RequestLaunchTemplateData' {} a -> s {securityGroups = a} :: RequestLaunchTemplateData) Prelude.. Lens.mapping Lens.coerced

-- | The elastic inference accelerator for the instance.
requestLaunchTemplateData_elasticInferenceAccelerators :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe [LaunchTemplateElasticInferenceAccelerator])
requestLaunchTemplateData_elasticInferenceAccelerators = Lens.lens (\RequestLaunchTemplateData' {elasticInferenceAccelerators} -> elasticInferenceAccelerators) (\s@RequestLaunchTemplateData' {} a -> s {elasticInferenceAccelerators = a} :: RequestLaunchTemplateData) Prelude.. Lens.mapping Lens.coerced

-- | The market (purchasing) option for the instances.
requestLaunchTemplateData_instanceMarketOptions :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe LaunchTemplateInstanceMarketOptionsRequest)
requestLaunchTemplateData_instanceMarketOptions = Lens.lens (\RequestLaunchTemplateData' {instanceMarketOptions} -> instanceMarketOptions) (\s@RequestLaunchTemplateData' {} a -> s {instanceMarketOptions = a} :: RequestLaunchTemplateData)

-- | The license configurations.
requestLaunchTemplateData_licenseSpecifications :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe [LaunchTemplateLicenseConfigurationRequest])
requestLaunchTemplateData_licenseSpecifications = Lens.lens (\RequestLaunchTemplateData' {licenseSpecifications} -> licenseSpecifications) (\s@RequestLaunchTemplateData' {} a -> s {licenseSpecifications = a} :: RequestLaunchTemplateData) Prelude.. Lens.mapping Lens.coerced

-- | If you set this parameter to @true@, you can\'t terminate the instance
-- using the Amazon EC2 console, CLI, or API; otherwise, you can. To change
-- this attribute after launch, use
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ModifyInstanceAttribute.html ModifyInstanceAttribute>.
-- Alternatively, if you set @InstanceInitiatedShutdownBehavior@ to
-- @terminate@, you can terminate the instance by running the shutdown
-- command from the instance.
requestLaunchTemplateData_disableApiTermination :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe Prelude.Bool)
requestLaunchTemplateData_disableApiTermination = Lens.lens (\RequestLaunchTemplateData' {disableApiTermination} -> disableApiTermination) (\s@RequestLaunchTemplateData' {} a -> s {disableApiTermination = a} :: RequestLaunchTemplateData)

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

-- | The CPU options for the instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-optimize-cpu.html Optimizing CPU Options>
-- in the /Amazon Elastic Compute Cloud User Guide/.
requestLaunchTemplateData_cpuOptions :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe LaunchTemplateCpuOptionsRequest)
requestLaunchTemplateData_cpuOptions = Lens.lens (\RequestLaunchTemplateData' {cpuOptions} -> cpuOptions) (\s@RequestLaunchTemplateData' {} a -> s {cpuOptions = a} :: RequestLaunchTemplateData)

-- | The ID of the RAM disk.
--
-- We recommend that you use PV-GRUB instead of kernels and RAM disks. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html User Provided Kernels>
-- in the /Amazon Elastic Compute Cloud User Guide/.
requestLaunchTemplateData_ramDiskId :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe Prelude.Text)
requestLaunchTemplateData_ramDiskId = Lens.lens (\RequestLaunchTemplateData' {ramDiskId} -> ramDiskId) (\s@RequestLaunchTemplateData' {} a -> s {ramDiskId = a} :: RequestLaunchTemplateData)

-- | The ID of the kernel.
--
-- We recommend that you use PV-GRUB instead of kernels and RAM disks. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html User Provided Kernels>
-- in the /Amazon Elastic Compute Cloud User Guide/.
requestLaunchTemplateData_kernelId :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe Prelude.Text)
requestLaunchTemplateData_kernelId = Lens.lens (\RequestLaunchTemplateData' {kernelId} -> kernelId) (\s@RequestLaunchTemplateData' {} a -> s {kernelId = a} :: RequestLaunchTemplateData)

-- | An elastic GPU to associate with the instance.
requestLaunchTemplateData_elasticGpuSpecifications :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe [ElasticGpuSpecification])
requestLaunchTemplateData_elasticGpuSpecifications = Lens.lens (\RequestLaunchTemplateData' {elasticGpuSpecifications} -> elasticGpuSpecifications) (\s@RequestLaunchTemplateData' {} a -> s {elasticGpuSpecifications = a} :: RequestLaunchTemplateData) Prelude.. Lens.mapping Lens.coerced

-- | The instance type. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types>
-- in the /Amazon Elastic Compute Cloud User Guide/.
requestLaunchTemplateData_instanceType :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe InstanceType)
requestLaunchTemplateData_instanceType = Lens.lens (\RequestLaunchTemplateData' {instanceType} -> instanceType) (\s@RequestLaunchTemplateData' {} a -> s {instanceType = a} :: RequestLaunchTemplateData)

-- | The Capacity Reservation targeting option. If you do not specify this
-- parameter, the instance\'s Capacity Reservation preference defaults to
-- @open@, which enables it to run in any open Capacity Reservation that
-- has matching attributes (instance type, platform, Availability Zone).
requestLaunchTemplateData_capacityReservationSpecification :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe LaunchTemplateCapacityReservationSpecificationRequest)
requestLaunchTemplateData_capacityReservationSpecification = Lens.lens (\RequestLaunchTemplateData' {capacityReservationSpecification} -> capacityReservationSpecification) (\s@RequestLaunchTemplateData' {} a -> s {capacityReservationSpecification = a} :: RequestLaunchTemplateData)

-- | Indicates whether the instance is optimized for Amazon EBS I\/O. This
-- optimization provides dedicated throughput to Amazon EBS and an
-- optimized configuration stack to provide optimal Amazon EBS I\/O
-- performance. This optimization isn\'t available with all instance types.
-- Additional usage charges apply when using an EBS-optimized instance.
requestLaunchTemplateData_ebsOptimized :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe Prelude.Bool)
requestLaunchTemplateData_ebsOptimized = Lens.lens (\RequestLaunchTemplateData' {ebsOptimized} -> ebsOptimized) (\s@RequestLaunchTemplateData' {} a -> s {ebsOptimized = a} :: RequestLaunchTemplateData)

-- | The user data to make available to the instance. You must provide
-- base64-encoded text. User data is limited to 16 KB. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/user-data.html Running Commands on Your Linux Instance at Launch>
-- (Linux) or
-- <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ec2-instance-metadata.html#instancedata-add-user-data Adding User Data>
-- (Windows).
--
-- If you are creating the launch template for use with Batch, the user
-- data must be provided in the
-- <https://cloudinit.readthedocs.io/en/latest/topics/format.html#mime-multi-part-archive MIME multi-part archive format>.
-- For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/launch-templates.html Amazon EC2 user data in launch templates>
-- in the /Batch User Guide/.
requestLaunchTemplateData_userData :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe Prelude.Text)
requestLaunchTemplateData_userData = Lens.lens (\RequestLaunchTemplateData' {userData} -> userData) (\s@RequestLaunchTemplateData' {} a -> s {userData = a} :: RequestLaunchTemplateData)

-- | The monitoring for the instance.
requestLaunchTemplateData_monitoring :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe LaunchTemplatesMonitoringRequest)
requestLaunchTemplateData_monitoring = Lens.lens (\RequestLaunchTemplateData' {monitoring} -> monitoring) (\s@RequestLaunchTemplateData' {} a -> s {monitoring = a} :: RequestLaunchTemplateData)

-- | The tags to apply to the resources during launch. You can only tag
-- instances and volumes on launch. The specified tags are applied to all
-- instances or volumes that are created during launch. To tag a resource
-- after it has been created, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags>.
requestLaunchTemplateData_tagSpecifications :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe [LaunchTemplateTagSpecificationRequest])
requestLaunchTemplateData_tagSpecifications = Lens.lens (\RequestLaunchTemplateData' {tagSpecifications} -> tagSpecifications) (\s@RequestLaunchTemplateData' {} a -> s {tagSpecifications = a} :: RequestLaunchTemplateData) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether an instance is enabled for hibernation. This parameter
-- is valid only if the instance meets the
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html#hibernating-prerequisites hibernation prerequisites>.
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate your instance>
-- in the /Amazon Elastic Compute Cloud User Guide/.
requestLaunchTemplateData_hibernationOptions :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe LaunchTemplateHibernationOptionsRequest)
requestLaunchTemplateData_hibernationOptions = Lens.lens (\RequestLaunchTemplateData' {hibernationOptions} -> hibernationOptions) (\s@RequestLaunchTemplateData' {} a -> s {hibernationOptions = a} :: RequestLaunchTemplateData)

-- | The name or Amazon Resource Name (ARN) of an IAM instance profile.
requestLaunchTemplateData_iamInstanceProfile :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe LaunchTemplateIamInstanceProfileSpecificationRequest)
requestLaunchTemplateData_iamInstanceProfile = Lens.lens (\RequestLaunchTemplateData' {iamInstanceProfile} -> iamInstanceProfile) (\s@RequestLaunchTemplateData' {} a -> s {iamInstanceProfile = a} :: RequestLaunchTemplateData)

-- | The ID of the AMI.
requestLaunchTemplateData_imageId :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe Prelude.Text)
requestLaunchTemplateData_imageId = Lens.lens (\RequestLaunchTemplateData' {imageId} -> imageId) (\s@RequestLaunchTemplateData' {} a -> s {imageId = a} :: RequestLaunchTemplateData)

-- | Indicates whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for
-- system shutdown).
--
-- Default: @stop@
requestLaunchTemplateData_instanceInitiatedShutdownBehavior :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe ShutdownBehavior)
requestLaunchTemplateData_instanceInitiatedShutdownBehavior = Lens.lens (\RequestLaunchTemplateData' {instanceInitiatedShutdownBehavior} -> instanceInitiatedShutdownBehavior) (\s@RequestLaunchTemplateData' {} a -> s {instanceInitiatedShutdownBehavior = a} :: RequestLaunchTemplateData)

-- | The metadata options for the instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance metadata and user data>
-- in the /Amazon Elastic Compute Cloud User Guide/.
requestLaunchTemplateData_metadataOptions :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe LaunchTemplateInstanceMetadataOptionsRequest)
requestLaunchTemplateData_metadataOptions = Lens.lens (\RequestLaunchTemplateData' {metadataOptions} -> metadataOptions) (\s@RequestLaunchTemplateData' {} a -> s {metadataOptions = a} :: RequestLaunchTemplateData)

-- | The credit option for CPU usage of the instance. Valid for T2, T3, or
-- T3a instances only.
requestLaunchTemplateData_creditSpecification :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe CreditSpecificationRequest)
requestLaunchTemplateData_creditSpecification = Lens.lens (\RequestLaunchTemplateData' {creditSpecification} -> creditSpecification) (\s@RequestLaunchTemplateData' {} a -> s {creditSpecification = a} :: RequestLaunchTemplateData)

-- | The block device mapping.
requestLaunchTemplateData_blockDeviceMappings :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe [LaunchTemplateBlockDeviceMappingRequest])
requestLaunchTemplateData_blockDeviceMappings = Lens.lens (\RequestLaunchTemplateData' {blockDeviceMappings} -> blockDeviceMappings) (\s@RequestLaunchTemplateData' {} a -> s {blockDeviceMappings = a} :: RequestLaunchTemplateData) Prelude.. Lens.mapping Lens.coerced

-- | The placement for the instance.
requestLaunchTemplateData_placement :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe LaunchTemplatePlacementRequest)
requestLaunchTemplateData_placement = Lens.lens (\RequestLaunchTemplateData' {placement} -> placement) (\s@RequestLaunchTemplateData' {} a -> s {placement = a} :: RequestLaunchTemplateData)

instance Prelude.Hashable RequestLaunchTemplateData

instance Prelude.NFData RequestLaunchTemplateData

instance Core.ToQuery RequestLaunchTemplateData where
  toQuery RequestLaunchTemplateData' {..} =
    Prelude.mconcat
      [ Core.toQuery
          ( Core.toQueryList "SecurityGroupId"
              Prelude.<$> securityGroupIds
          ),
        Core.toQuery
          ( Core.toQueryList "SecurityGroup"
              Prelude.<$> securityGroups
          ),
        Core.toQuery
          ( Core.toQueryList "ElasticInferenceAccelerator"
              Prelude.<$> elasticInferenceAccelerators
          ),
        "InstanceMarketOptions"
          Core.=: instanceMarketOptions,
        Core.toQuery
          ( Core.toQueryList "LicenseSpecification"
              Prelude.<$> licenseSpecifications
          ),
        "DisableApiTermination"
          Core.=: disableApiTermination,
        "KeyName" Core.=: keyName,
        Core.toQuery
          ( Core.toQueryList "NetworkInterface"
              Prelude.<$> networkInterfaces
          ),
        "EnclaveOptions" Core.=: enclaveOptions,
        "CpuOptions" Core.=: cpuOptions,
        "RamDiskId" Core.=: ramDiskId,
        "KernelId" Core.=: kernelId,
        Core.toQuery
          ( Core.toQueryList "ElasticGpuSpecification"
              Prelude.<$> elasticGpuSpecifications
          ),
        "InstanceType" Core.=: instanceType,
        "CapacityReservationSpecification"
          Core.=: capacityReservationSpecification,
        "EbsOptimized" Core.=: ebsOptimized,
        "UserData" Core.=: userData,
        "Monitoring" Core.=: monitoring,
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "HibernationOptions" Core.=: hibernationOptions,
        "IamInstanceProfile" Core.=: iamInstanceProfile,
        "ImageId" Core.=: imageId,
        "InstanceInitiatedShutdownBehavior"
          Core.=: instanceInitiatedShutdownBehavior,
        "MetadataOptions" Core.=: metadataOptions,
        "CreditSpecification" Core.=: creditSpecification,
        Core.toQuery
          ( Core.toQueryList "BlockDeviceMapping"
              Prelude.<$> blockDeviceMappings
          ),
        "Placement" Core.=: placement
      ]
