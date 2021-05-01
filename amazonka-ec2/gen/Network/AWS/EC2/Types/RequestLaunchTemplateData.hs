{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.Types.RequestLaunchTemplateData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.RequestLaunchTemplateData where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.CreditSpecificationRequest
import Network.AWS.EC2.Types.ElasticGpuSpecification
import Network.AWS.EC2.Types.InstanceType
import Network.AWS.EC2.Types.LaunchTemplateBlockDeviceMappingRequest
import Network.AWS.EC2.Types.LaunchTemplateCapacityReservationSpecificationRequest
import Network.AWS.EC2.Types.LaunchTemplateCpuOptionsRequest
import Network.AWS.EC2.Types.LaunchTemplateElasticInferenceAccelerator
import Network.AWS.EC2.Types.LaunchTemplateEnclaveOptionsRequest
import Network.AWS.EC2.Types.LaunchTemplateHibernationOptionsRequest
import Network.AWS.EC2.Types.LaunchTemplateIamInstanceProfileSpecificationRequest
import Network.AWS.EC2.Types.LaunchTemplateInstanceMarketOptionsRequest
import Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataOptionsRequest
import Network.AWS.EC2.Types.LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
import Network.AWS.EC2.Types.LaunchTemplateLicenseConfigurationRequest
import Network.AWS.EC2.Types.LaunchTemplatePlacementRequest
import Network.AWS.EC2.Types.LaunchTemplateTagSpecificationRequest
import Network.AWS.EC2.Types.LaunchTemplatesMonitoringRequest
import Network.AWS.EC2.Types.ShutdownBehavior
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The information to include in the launch template.
--
-- /See:/ 'newRequestLaunchTemplateData' smart constructor.
data RequestLaunchTemplateData = RequestLaunchTemplateData'
  { -- | One or more security group IDs. You can create a security group using
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateSecurityGroup.html CreateSecurityGroup>.
    -- You cannot specify both a security group ID and security name in the
    -- same request.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The tags to apply to the resources during launch. You can only tag
    -- instances and volumes on launch. The specified tags are applied to all
    -- instances or volumes that are created during launch. To tag a resource
    -- after it has been created, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags>.
    tagSpecifications :: Prelude.Maybe [LaunchTemplateTagSpecificationRequest],
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
    -- | The Base64-encoded user data to make available to the instance. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/user-data.html Running Commands on Your Linux Instance at Launch>
    -- (Linux) and
    -- <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ec2-instance-metadata.html#instancedata-add-user-data Adding User Data>
    -- (Windows).
    userData :: Prelude.Maybe Prelude.Text,
    -- | The placement for the instance.
    placement :: Prelude.Maybe LaunchTemplatePlacementRequest,
    -- | The ID of the RAM disk.
    --
    -- We recommend that you use PV-GRUB instead of kernels and RAM disks. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html User Provided Kernels>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    ramDiskId :: Prelude.Maybe Prelude.Text,
    -- | The credit option for CPU usage of the instance. Valid for T2, T3, or
    -- T3a instances only.
    creditSpecification :: Prelude.Maybe CreditSpecificationRequest,
    -- | The market (purchasing) option for the instances.
    instanceMarketOptions :: Prelude.Maybe LaunchTemplateInstanceMarketOptionsRequest,
    -- | The license configurations.
    licenseSpecifications :: Prelude.Maybe [LaunchTemplateLicenseConfigurationRequest],
    -- | Indicates whether an instance stops or terminates when you initiate
    -- shutdown from the instance (using the operating system command for
    -- system shutdown).
    --
    -- Default: @stop@
    instanceInitiatedShutdownBehavior :: Prelude.Maybe ShutdownBehavior,
    -- | The ID of the AMI.
    imageId :: Prelude.Maybe Prelude.Text,
    -- | [EC2-Classic, default VPC] One or more security group names. For a
    -- nondefault VPC, you must use security group IDs instead. You cannot
    -- specify both a security group ID and security name in the same request.
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | The elastic inference accelerator for the instance.
    elasticInferenceAccelerators :: Prelude.Maybe [LaunchTemplateElasticInferenceAccelerator],
    -- | The name or Amazon Resource Name (ARN) of an IAM instance profile.
    iamInstanceProfile :: Prelude.Maybe LaunchTemplateIamInstanceProfileSpecificationRequest,
    -- | Indicates whether an instance is enabled for hibernation. This parameter
    -- is valid only if the instance meets the
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html#hibernating-prerequisites hibernation prerequisites>.
    -- For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate Your Instance>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    hibernationOptions :: Prelude.Maybe LaunchTemplateHibernationOptionsRequest,
    -- | The monitoring for the instance.
    monitoring :: Prelude.Maybe LaunchTemplatesMonitoringRequest,
    -- | The block device mapping.
    blockDeviceMappings :: Prelude.Maybe [LaunchTemplateBlockDeviceMappingRequest],
    -- | Indicates whether the instance is enabled for AWS Nitro Enclaves. For
    -- more information, see
    -- <https://docs.aws.amazon.com/enclaves/latest/user/nitro-enclave.html What is AWS Nitro Enclaves?>
    -- in the /AWS Nitro Enclaves User Guide/.
    --
    -- You can\'t enable AWS Nitro Enclaves and hibernation on the same
    -- instance.
    enclaveOptions :: Prelude.Maybe LaunchTemplateEnclaveOptionsRequest,
    -- | The ID of the kernel.
    --
    -- We recommend that you use PV-GRUB instead of kernels and RAM disks. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html User Provided Kernels>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    kernelId :: Prelude.Maybe Prelude.Text,
    -- | The CPU options for the instance. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-optimize-cpu.html Optimizing CPU Options>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    cpuOptions :: Prelude.Maybe LaunchTemplateCpuOptionsRequest,
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
    -- | If you set this parameter to @true@, you can\'t terminate the instance
    -- using the Amazon EC2 console, CLI, or API; otherwise, you can. To change
    -- this attribute after launch, use
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ModifyInstanceAttribute.html ModifyInstanceAttribute>.
    -- Alternatively, if you set @InstanceInitiatedShutdownBehavior@ to
    -- @terminate@, you can terminate the instance by running the shutdown
    -- command from the instance.
    disableApiTermination :: Prelude.Maybe Prelude.Bool,
    -- | The metadata options for the instance. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance Metadata and User Data>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    metadataOptions :: Prelude.Maybe LaunchTemplateInstanceMetadataOptionsRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'tagSpecifications', 'requestLaunchTemplateData_tagSpecifications' - The tags to apply to the resources during launch. You can only tag
-- instances and volumes on launch. The specified tags are applied to all
-- instances or volumes that are created during launch. To tag a resource
-- after it has been created, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags>.
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
-- 'userData', 'requestLaunchTemplateData_userData' - The Base64-encoded user data to make available to the instance. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/user-data.html Running Commands on Your Linux Instance at Launch>
-- (Linux) and
-- <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ec2-instance-metadata.html#instancedata-add-user-data Adding User Data>
-- (Windows).
--
-- 'placement', 'requestLaunchTemplateData_placement' - The placement for the instance.
--
-- 'ramDiskId', 'requestLaunchTemplateData_ramDiskId' - The ID of the RAM disk.
--
-- We recommend that you use PV-GRUB instead of kernels and RAM disks. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html User Provided Kernels>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'creditSpecification', 'requestLaunchTemplateData_creditSpecification' - The credit option for CPU usage of the instance. Valid for T2, T3, or
-- T3a instances only.
--
-- 'instanceMarketOptions', 'requestLaunchTemplateData_instanceMarketOptions' - The market (purchasing) option for the instances.
--
-- 'licenseSpecifications', 'requestLaunchTemplateData_licenseSpecifications' - The license configurations.
--
-- 'instanceInitiatedShutdownBehavior', 'requestLaunchTemplateData_instanceInitiatedShutdownBehavior' - Indicates whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for
-- system shutdown).
--
-- Default: @stop@
--
-- 'imageId', 'requestLaunchTemplateData_imageId' - The ID of the AMI.
--
-- 'securityGroups', 'requestLaunchTemplateData_securityGroups' - [EC2-Classic, default VPC] One or more security group names. For a
-- nondefault VPC, you must use security group IDs instead. You cannot
-- specify both a security group ID and security name in the same request.
--
-- 'elasticInferenceAccelerators', 'requestLaunchTemplateData_elasticInferenceAccelerators' - The elastic inference accelerator for the instance.
--
-- 'iamInstanceProfile', 'requestLaunchTemplateData_iamInstanceProfile' - The name or Amazon Resource Name (ARN) of an IAM instance profile.
--
-- 'hibernationOptions', 'requestLaunchTemplateData_hibernationOptions' - Indicates whether an instance is enabled for hibernation. This parameter
-- is valid only if the instance meets the
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html#hibernating-prerequisites hibernation prerequisites>.
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate Your Instance>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'monitoring', 'requestLaunchTemplateData_monitoring' - The monitoring for the instance.
--
-- 'blockDeviceMappings', 'requestLaunchTemplateData_blockDeviceMappings' - The block device mapping.
--
-- 'enclaveOptions', 'requestLaunchTemplateData_enclaveOptions' - Indicates whether the instance is enabled for AWS Nitro Enclaves. For
-- more information, see
-- <https://docs.aws.amazon.com/enclaves/latest/user/nitro-enclave.html What is AWS Nitro Enclaves?>
-- in the /AWS Nitro Enclaves User Guide/.
--
-- You can\'t enable AWS Nitro Enclaves and hibernation on the same
-- instance.
--
-- 'kernelId', 'requestLaunchTemplateData_kernelId' - The ID of the kernel.
--
-- We recommend that you use PV-GRUB instead of kernels and RAM disks. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html User Provided Kernels>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'cpuOptions', 'requestLaunchTemplateData_cpuOptions' - The CPU options for the instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-optimize-cpu.html Optimizing CPU Options>
-- in the /Amazon Elastic Compute Cloud User Guide/.
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
-- 'disableApiTermination', 'requestLaunchTemplateData_disableApiTermination' - If you set this parameter to @true@, you can\'t terminate the instance
-- using the Amazon EC2 console, CLI, or API; otherwise, you can. To change
-- this attribute after launch, use
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ModifyInstanceAttribute.html ModifyInstanceAttribute>.
-- Alternatively, if you set @InstanceInitiatedShutdownBehavior@ to
-- @terminate@, you can terminate the instance by running the shutdown
-- command from the instance.
--
-- 'metadataOptions', 'requestLaunchTemplateData_metadataOptions' - The metadata options for the instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance Metadata and User Data>
-- in the /Amazon Elastic Compute Cloud User Guide/.
newRequestLaunchTemplateData ::
  RequestLaunchTemplateData
newRequestLaunchTemplateData =
  RequestLaunchTemplateData'
    { securityGroupIds =
        Prelude.Nothing,
      tagSpecifications = Prelude.Nothing,
      elasticGpuSpecifications = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      capacityReservationSpecification =
        Prelude.Nothing,
      ebsOptimized = Prelude.Nothing,
      userData = Prelude.Nothing,
      placement = Prelude.Nothing,
      ramDiskId = Prelude.Nothing,
      creditSpecification = Prelude.Nothing,
      instanceMarketOptions = Prelude.Nothing,
      licenseSpecifications = Prelude.Nothing,
      instanceInitiatedShutdownBehavior =
        Prelude.Nothing,
      imageId = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      elasticInferenceAccelerators = Prelude.Nothing,
      iamInstanceProfile = Prelude.Nothing,
      hibernationOptions = Prelude.Nothing,
      monitoring = Prelude.Nothing,
      blockDeviceMappings = Prelude.Nothing,
      enclaveOptions = Prelude.Nothing,
      kernelId = Prelude.Nothing,
      cpuOptions = Prelude.Nothing,
      keyName = Prelude.Nothing,
      networkInterfaces = Prelude.Nothing,
      disableApiTermination = Prelude.Nothing,
      metadataOptions = Prelude.Nothing
    }

-- | One or more security group IDs. You can create a security group using
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateSecurityGroup.html CreateSecurityGroup>.
-- You cannot specify both a security group ID and security name in the
-- same request.
requestLaunchTemplateData_securityGroupIds :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe [Prelude.Text])
requestLaunchTemplateData_securityGroupIds = Lens.lens (\RequestLaunchTemplateData' {securityGroupIds} -> securityGroupIds) (\s@RequestLaunchTemplateData' {} a -> s {securityGroupIds = a} :: RequestLaunchTemplateData) Prelude.. Lens.mapping Prelude._Coerce

-- | The tags to apply to the resources during launch. You can only tag
-- instances and volumes on launch. The specified tags are applied to all
-- instances or volumes that are created during launch. To tag a resource
-- after it has been created, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags>.
requestLaunchTemplateData_tagSpecifications :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe [LaunchTemplateTagSpecificationRequest])
requestLaunchTemplateData_tagSpecifications = Lens.lens (\RequestLaunchTemplateData' {tagSpecifications} -> tagSpecifications) (\s@RequestLaunchTemplateData' {} a -> s {tagSpecifications = a} :: RequestLaunchTemplateData) Prelude.. Lens.mapping Prelude._Coerce

-- | An elastic GPU to associate with the instance.
requestLaunchTemplateData_elasticGpuSpecifications :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe [ElasticGpuSpecification])
requestLaunchTemplateData_elasticGpuSpecifications = Lens.lens (\RequestLaunchTemplateData' {elasticGpuSpecifications} -> elasticGpuSpecifications) (\s@RequestLaunchTemplateData' {} a -> s {elasticGpuSpecifications = a} :: RequestLaunchTemplateData) Prelude.. Lens.mapping Prelude._Coerce

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

-- | The Base64-encoded user data to make available to the instance. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/user-data.html Running Commands on Your Linux Instance at Launch>
-- (Linux) and
-- <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ec2-instance-metadata.html#instancedata-add-user-data Adding User Data>
-- (Windows).
requestLaunchTemplateData_userData :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe Prelude.Text)
requestLaunchTemplateData_userData = Lens.lens (\RequestLaunchTemplateData' {userData} -> userData) (\s@RequestLaunchTemplateData' {} a -> s {userData = a} :: RequestLaunchTemplateData)

-- | The placement for the instance.
requestLaunchTemplateData_placement :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe LaunchTemplatePlacementRequest)
requestLaunchTemplateData_placement = Lens.lens (\RequestLaunchTemplateData' {placement} -> placement) (\s@RequestLaunchTemplateData' {} a -> s {placement = a} :: RequestLaunchTemplateData)

-- | The ID of the RAM disk.
--
-- We recommend that you use PV-GRUB instead of kernels and RAM disks. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html User Provided Kernels>
-- in the /Amazon Elastic Compute Cloud User Guide/.
requestLaunchTemplateData_ramDiskId :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe Prelude.Text)
requestLaunchTemplateData_ramDiskId = Lens.lens (\RequestLaunchTemplateData' {ramDiskId} -> ramDiskId) (\s@RequestLaunchTemplateData' {} a -> s {ramDiskId = a} :: RequestLaunchTemplateData)

-- | The credit option for CPU usage of the instance. Valid for T2, T3, or
-- T3a instances only.
requestLaunchTemplateData_creditSpecification :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe CreditSpecificationRequest)
requestLaunchTemplateData_creditSpecification = Lens.lens (\RequestLaunchTemplateData' {creditSpecification} -> creditSpecification) (\s@RequestLaunchTemplateData' {} a -> s {creditSpecification = a} :: RequestLaunchTemplateData)

-- | The market (purchasing) option for the instances.
requestLaunchTemplateData_instanceMarketOptions :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe LaunchTemplateInstanceMarketOptionsRequest)
requestLaunchTemplateData_instanceMarketOptions = Lens.lens (\RequestLaunchTemplateData' {instanceMarketOptions} -> instanceMarketOptions) (\s@RequestLaunchTemplateData' {} a -> s {instanceMarketOptions = a} :: RequestLaunchTemplateData)

-- | The license configurations.
requestLaunchTemplateData_licenseSpecifications :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe [LaunchTemplateLicenseConfigurationRequest])
requestLaunchTemplateData_licenseSpecifications = Lens.lens (\RequestLaunchTemplateData' {licenseSpecifications} -> licenseSpecifications) (\s@RequestLaunchTemplateData' {} a -> s {licenseSpecifications = a} :: RequestLaunchTemplateData) Prelude.. Lens.mapping Prelude._Coerce

-- | Indicates whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for
-- system shutdown).
--
-- Default: @stop@
requestLaunchTemplateData_instanceInitiatedShutdownBehavior :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe ShutdownBehavior)
requestLaunchTemplateData_instanceInitiatedShutdownBehavior = Lens.lens (\RequestLaunchTemplateData' {instanceInitiatedShutdownBehavior} -> instanceInitiatedShutdownBehavior) (\s@RequestLaunchTemplateData' {} a -> s {instanceInitiatedShutdownBehavior = a} :: RequestLaunchTemplateData)

-- | The ID of the AMI.
requestLaunchTemplateData_imageId :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe Prelude.Text)
requestLaunchTemplateData_imageId = Lens.lens (\RequestLaunchTemplateData' {imageId} -> imageId) (\s@RequestLaunchTemplateData' {} a -> s {imageId = a} :: RequestLaunchTemplateData)

-- | [EC2-Classic, default VPC] One or more security group names. For a
-- nondefault VPC, you must use security group IDs instead. You cannot
-- specify both a security group ID and security name in the same request.
requestLaunchTemplateData_securityGroups :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe [Prelude.Text])
requestLaunchTemplateData_securityGroups = Lens.lens (\RequestLaunchTemplateData' {securityGroups} -> securityGroups) (\s@RequestLaunchTemplateData' {} a -> s {securityGroups = a} :: RequestLaunchTemplateData) Prelude.. Lens.mapping Prelude._Coerce

-- | The elastic inference accelerator for the instance.
requestLaunchTemplateData_elasticInferenceAccelerators :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe [LaunchTemplateElasticInferenceAccelerator])
requestLaunchTemplateData_elasticInferenceAccelerators = Lens.lens (\RequestLaunchTemplateData' {elasticInferenceAccelerators} -> elasticInferenceAccelerators) (\s@RequestLaunchTemplateData' {} a -> s {elasticInferenceAccelerators = a} :: RequestLaunchTemplateData) Prelude.. Lens.mapping Prelude._Coerce

-- | The name or Amazon Resource Name (ARN) of an IAM instance profile.
requestLaunchTemplateData_iamInstanceProfile :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe LaunchTemplateIamInstanceProfileSpecificationRequest)
requestLaunchTemplateData_iamInstanceProfile = Lens.lens (\RequestLaunchTemplateData' {iamInstanceProfile} -> iamInstanceProfile) (\s@RequestLaunchTemplateData' {} a -> s {iamInstanceProfile = a} :: RequestLaunchTemplateData)

-- | Indicates whether an instance is enabled for hibernation. This parameter
-- is valid only if the instance meets the
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html#hibernating-prerequisites hibernation prerequisites>.
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate Your Instance>
-- in the /Amazon Elastic Compute Cloud User Guide/.
requestLaunchTemplateData_hibernationOptions :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe LaunchTemplateHibernationOptionsRequest)
requestLaunchTemplateData_hibernationOptions = Lens.lens (\RequestLaunchTemplateData' {hibernationOptions} -> hibernationOptions) (\s@RequestLaunchTemplateData' {} a -> s {hibernationOptions = a} :: RequestLaunchTemplateData)

-- | The monitoring for the instance.
requestLaunchTemplateData_monitoring :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe LaunchTemplatesMonitoringRequest)
requestLaunchTemplateData_monitoring = Lens.lens (\RequestLaunchTemplateData' {monitoring} -> monitoring) (\s@RequestLaunchTemplateData' {} a -> s {monitoring = a} :: RequestLaunchTemplateData)

-- | The block device mapping.
requestLaunchTemplateData_blockDeviceMappings :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe [LaunchTemplateBlockDeviceMappingRequest])
requestLaunchTemplateData_blockDeviceMappings = Lens.lens (\RequestLaunchTemplateData' {blockDeviceMappings} -> blockDeviceMappings) (\s@RequestLaunchTemplateData' {} a -> s {blockDeviceMappings = a} :: RequestLaunchTemplateData) Prelude.. Lens.mapping Prelude._Coerce

-- | Indicates whether the instance is enabled for AWS Nitro Enclaves. For
-- more information, see
-- <https://docs.aws.amazon.com/enclaves/latest/user/nitro-enclave.html What is AWS Nitro Enclaves?>
-- in the /AWS Nitro Enclaves User Guide/.
--
-- You can\'t enable AWS Nitro Enclaves and hibernation on the same
-- instance.
requestLaunchTemplateData_enclaveOptions :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe LaunchTemplateEnclaveOptionsRequest)
requestLaunchTemplateData_enclaveOptions = Lens.lens (\RequestLaunchTemplateData' {enclaveOptions} -> enclaveOptions) (\s@RequestLaunchTemplateData' {} a -> s {enclaveOptions = a} :: RequestLaunchTemplateData)

-- | The ID of the kernel.
--
-- We recommend that you use PV-GRUB instead of kernels and RAM disks. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html User Provided Kernels>
-- in the /Amazon Elastic Compute Cloud User Guide/.
requestLaunchTemplateData_kernelId :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe Prelude.Text)
requestLaunchTemplateData_kernelId = Lens.lens (\RequestLaunchTemplateData' {kernelId} -> kernelId) (\s@RequestLaunchTemplateData' {} a -> s {kernelId = a} :: RequestLaunchTemplateData)

-- | The CPU options for the instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-optimize-cpu.html Optimizing CPU Options>
-- in the /Amazon Elastic Compute Cloud User Guide/.
requestLaunchTemplateData_cpuOptions :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe LaunchTemplateCpuOptionsRequest)
requestLaunchTemplateData_cpuOptions = Lens.lens (\RequestLaunchTemplateData' {cpuOptions} -> cpuOptions) (\s@RequestLaunchTemplateData' {} a -> s {cpuOptions = a} :: RequestLaunchTemplateData)

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
requestLaunchTemplateData_networkInterfaces = Lens.lens (\RequestLaunchTemplateData' {networkInterfaces} -> networkInterfaces) (\s@RequestLaunchTemplateData' {} a -> s {networkInterfaces = a} :: RequestLaunchTemplateData) Prelude.. Lens.mapping Prelude._Coerce

-- | If you set this parameter to @true@, you can\'t terminate the instance
-- using the Amazon EC2 console, CLI, or API; otherwise, you can. To change
-- this attribute after launch, use
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ModifyInstanceAttribute.html ModifyInstanceAttribute>.
-- Alternatively, if you set @InstanceInitiatedShutdownBehavior@ to
-- @terminate@, you can terminate the instance by running the shutdown
-- command from the instance.
requestLaunchTemplateData_disableApiTermination :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe Prelude.Bool)
requestLaunchTemplateData_disableApiTermination = Lens.lens (\RequestLaunchTemplateData' {disableApiTermination} -> disableApiTermination) (\s@RequestLaunchTemplateData' {} a -> s {disableApiTermination = a} :: RequestLaunchTemplateData)

-- | The metadata options for the instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance Metadata and User Data>
-- in the /Amazon Elastic Compute Cloud User Guide/.
requestLaunchTemplateData_metadataOptions :: Lens.Lens' RequestLaunchTemplateData (Prelude.Maybe LaunchTemplateInstanceMetadataOptionsRequest)
requestLaunchTemplateData_metadataOptions = Lens.lens (\RequestLaunchTemplateData' {metadataOptions} -> metadataOptions) (\s@RequestLaunchTemplateData' {} a -> s {metadataOptions = a} :: RequestLaunchTemplateData)

instance Prelude.Hashable RequestLaunchTemplateData

instance Prelude.NFData RequestLaunchTemplateData

instance Prelude.ToQuery RequestLaunchTemplateData where
  toQuery RequestLaunchTemplateData' {..} =
    Prelude.mconcat
      [ Prelude.toQuery
          ( Prelude.toQueryList "SecurityGroupId"
              Prelude.<$> securityGroupIds
          ),
        Prelude.toQuery
          ( Prelude.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        Prelude.toQuery
          ( Prelude.toQueryList "ElasticGpuSpecification"
              Prelude.<$> elasticGpuSpecifications
          ),
        "InstanceType" Prelude.=: instanceType,
        "CapacityReservationSpecification"
          Prelude.=: capacityReservationSpecification,
        "EbsOptimized" Prelude.=: ebsOptimized,
        "UserData" Prelude.=: userData,
        "Placement" Prelude.=: placement,
        "RamDiskId" Prelude.=: ramDiskId,
        "CreditSpecification" Prelude.=: creditSpecification,
        "InstanceMarketOptions"
          Prelude.=: instanceMarketOptions,
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
          ( Prelude.toQueryList "ElasticInferenceAccelerator"
              Prelude.<$> elasticInferenceAccelerators
          ),
        "IamInstanceProfile" Prelude.=: iamInstanceProfile,
        "HibernationOptions" Prelude.=: hibernationOptions,
        "Monitoring" Prelude.=: monitoring,
        Prelude.toQuery
          ( Prelude.toQueryList "BlockDeviceMapping"
              Prelude.<$> blockDeviceMappings
          ),
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
        "MetadataOptions" Prelude.=: metadataOptions
      ]
