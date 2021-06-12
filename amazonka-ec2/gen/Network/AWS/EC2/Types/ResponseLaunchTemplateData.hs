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
-- Module      : Network.AWS.EC2.Types.ResponseLaunchTemplateData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ResponseLaunchTemplateData where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.CreditSpecification
import Network.AWS.EC2.Types.ElasticGpuSpecificationResponse
import Network.AWS.EC2.Types.InstanceType
import Network.AWS.EC2.Types.LaunchTemplateBlockDeviceMapping
import Network.AWS.EC2.Types.LaunchTemplateCapacityReservationSpecificationResponse
import Network.AWS.EC2.Types.LaunchTemplateCpuOptions
import Network.AWS.EC2.Types.LaunchTemplateElasticInferenceAcceleratorResponse
import Network.AWS.EC2.Types.LaunchTemplateEnclaveOptions
import Network.AWS.EC2.Types.LaunchTemplateHibernationOptions
import Network.AWS.EC2.Types.LaunchTemplateIamInstanceProfileSpecification
import Network.AWS.EC2.Types.LaunchTemplateInstanceMarketOptions
import Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataOptions
import Network.AWS.EC2.Types.LaunchTemplateInstanceNetworkInterfaceSpecification
import Network.AWS.EC2.Types.LaunchTemplateLicenseConfiguration
import Network.AWS.EC2.Types.LaunchTemplatePlacement
import Network.AWS.EC2.Types.LaunchTemplateTagSpecification
import Network.AWS.EC2.Types.LaunchTemplatesMonitoring
import Network.AWS.EC2.Types.ShutdownBehavior
import qualified Network.AWS.Lens as Lens

-- | The information for a launch template.
--
-- /See:/ 'newResponseLaunchTemplateData' smart constructor.
data ResponseLaunchTemplateData = ResponseLaunchTemplateData'
  { -- | The security group IDs.
    securityGroupIds :: Core.Maybe [Core.Text],
    -- | The tags.
    tagSpecifications :: Core.Maybe [LaunchTemplateTagSpecification],
    -- | The elastic GPU specification.
    elasticGpuSpecifications :: Core.Maybe [ElasticGpuSpecificationResponse],
    -- | The instance type.
    instanceType :: Core.Maybe InstanceType,
    -- | Information about the Capacity Reservation targeting option.
    capacityReservationSpecification :: Core.Maybe LaunchTemplateCapacityReservationSpecificationResponse,
    -- | Indicates whether the instance is optimized for Amazon EBS I\/O.
    ebsOptimized :: Core.Maybe Core.Bool,
    -- | The user data for the instance.
    userData :: Core.Maybe Core.Text,
    -- | The placement of the instance.
    placement :: Core.Maybe LaunchTemplatePlacement,
    -- | The ID of the RAM disk, if applicable.
    ramDiskId :: Core.Maybe Core.Text,
    -- | The credit option for CPU usage of the instance.
    creditSpecification :: Core.Maybe CreditSpecification,
    -- | The market (purchasing) option for the instances.
    instanceMarketOptions :: Core.Maybe LaunchTemplateInstanceMarketOptions,
    -- | The license configurations.
    licenseSpecifications :: Core.Maybe [LaunchTemplateLicenseConfiguration],
    -- | Indicates whether an instance stops or terminates when you initiate
    -- shutdown from the instance (using the operating system command for
    -- system shutdown).
    instanceInitiatedShutdownBehavior :: Core.Maybe ShutdownBehavior,
    -- | The ID of the AMI that was used to launch the instance.
    imageId :: Core.Maybe Core.Text,
    -- | The security group names.
    securityGroups :: Core.Maybe [Core.Text],
    -- | The elastic inference accelerator for the instance.
    elasticInferenceAccelerators :: Core.Maybe [LaunchTemplateElasticInferenceAcceleratorResponse],
    -- | The IAM instance profile.
    iamInstanceProfile :: Core.Maybe LaunchTemplateIamInstanceProfileSpecification,
    -- | Indicates whether an instance is configured for hibernation. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate Your Instance>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    hibernationOptions :: Core.Maybe LaunchTemplateHibernationOptions,
    -- | The monitoring for the instance.
    monitoring :: Core.Maybe LaunchTemplatesMonitoring,
    -- | The block device mappings.
    blockDeviceMappings :: Core.Maybe [LaunchTemplateBlockDeviceMapping],
    -- | Indicates whether the instance is enabled for AWS Nitro Enclaves.
    enclaveOptions :: Core.Maybe LaunchTemplateEnclaveOptions,
    -- | The ID of the kernel, if applicable.
    kernelId :: Core.Maybe Core.Text,
    -- | The CPU options for the instance. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-optimize-cpu.html Optimizing CPU Options>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    cpuOptions :: Core.Maybe LaunchTemplateCpuOptions,
    -- | The name of the key pair.
    keyName :: Core.Maybe Core.Text,
    -- | The network interfaces.
    networkInterfaces :: Core.Maybe [LaunchTemplateInstanceNetworkInterfaceSpecification],
    -- | If set to @true@, indicates that the instance cannot be terminated using
    -- the Amazon EC2 console, command line tool, or API.
    disableApiTermination :: Core.Maybe Core.Bool,
    -- | The metadata options for the instance. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance Metadata and User Data>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    metadataOptions :: Core.Maybe LaunchTemplateInstanceMetadataOptions
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ResponseLaunchTemplateData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'responseLaunchTemplateData_securityGroupIds' - The security group IDs.
--
-- 'tagSpecifications', 'responseLaunchTemplateData_tagSpecifications' - The tags.
--
-- 'elasticGpuSpecifications', 'responseLaunchTemplateData_elasticGpuSpecifications' - The elastic GPU specification.
--
-- 'instanceType', 'responseLaunchTemplateData_instanceType' - The instance type.
--
-- 'capacityReservationSpecification', 'responseLaunchTemplateData_capacityReservationSpecification' - Information about the Capacity Reservation targeting option.
--
-- 'ebsOptimized', 'responseLaunchTemplateData_ebsOptimized' - Indicates whether the instance is optimized for Amazon EBS I\/O.
--
-- 'userData', 'responseLaunchTemplateData_userData' - The user data for the instance.
--
-- 'placement', 'responseLaunchTemplateData_placement' - The placement of the instance.
--
-- 'ramDiskId', 'responseLaunchTemplateData_ramDiskId' - The ID of the RAM disk, if applicable.
--
-- 'creditSpecification', 'responseLaunchTemplateData_creditSpecification' - The credit option for CPU usage of the instance.
--
-- 'instanceMarketOptions', 'responseLaunchTemplateData_instanceMarketOptions' - The market (purchasing) option for the instances.
--
-- 'licenseSpecifications', 'responseLaunchTemplateData_licenseSpecifications' - The license configurations.
--
-- 'instanceInitiatedShutdownBehavior', 'responseLaunchTemplateData_instanceInitiatedShutdownBehavior' - Indicates whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for
-- system shutdown).
--
-- 'imageId', 'responseLaunchTemplateData_imageId' - The ID of the AMI that was used to launch the instance.
--
-- 'securityGroups', 'responseLaunchTemplateData_securityGroups' - The security group names.
--
-- 'elasticInferenceAccelerators', 'responseLaunchTemplateData_elasticInferenceAccelerators' - The elastic inference accelerator for the instance.
--
-- 'iamInstanceProfile', 'responseLaunchTemplateData_iamInstanceProfile' - The IAM instance profile.
--
-- 'hibernationOptions', 'responseLaunchTemplateData_hibernationOptions' - Indicates whether an instance is configured for hibernation. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate Your Instance>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'monitoring', 'responseLaunchTemplateData_monitoring' - The monitoring for the instance.
--
-- 'blockDeviceMappings', 'responseLaunchTemplateData_blockDeviceMappings' - The block device mappings.
--
-- 'enclaveOptions', 'responseLaunchTemplateData_enclaveOptions' - Indicates whether the instance is enabled for AWS Nitro Enclaves.
--
-- 'kernelId', 'responseLaunchTemplateData_kernelId' - The ID of the kernel, if applicable.
--
-- 'cpuOptions', 'responseLaunchTemplateData_cpuOptions' - The CPU options for the instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-optimize-cpu.html Optimizing CPU Options>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'keyName', 'responseLaunchTemplateData_keyName' - The name of the key pair.
--
-- 'networkInterfaces', 'responseLaunchTemplateData_networkInterfaces' - The network interfaces.
--
-- 'disableApiTermination', 'responseLaunchTemplateData_disableApiTermination' - If set to @true@, indicates that the instance cannot be terminated using
-- the Amazon EC2 console, command line tool, or API.
--
-- 'metadataOptions', 'responseLaunchTemplateData_metadataOptions' - The metadata options for the instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance Metadata and User Data>
-- in the /Amazon Elastic Compute Cloud User Guide/.
newResponseLaunchTemplateData ::
  ResponseLaunchTemplateData
newResponseLaunchTemplateData =
  ResponseLaunchTemplateData'
    { securityGroupIds =
        Core.Nothing,
      tagSpecifications = Core.Nothing,
      elasticGpuSpecifications = Core.Nothing,
      instanceType = Core.Nothing,
      capacityReservationSpecification = Core.Nothing,
      ebsOptimized = Core.Nothing,
      userData = Core.Nothing,
      placement = Core.Nothing,
      ramDiskId = Core.Nothing,
      creditSpecification = Core.Nothing,
      instanceMarketOptions = Core.Nothing,
      licenseSpecifications = Core.Nothing,
      instanceInitiatedShutdownBehavior =
        Core.Nothing,
      imageId = Core.Nothing,
      securityGroups = Core.Nothing,
      elasticInferenceAccelerators = Core.Nothing,
      iamInstanceProfile = Core.Nothing,
      hibernationOptions = Core.Nothing,
      monitoring = Core.Nothing,
      blockDeviceMappings = Core.Nothing,
      enclaveOptions = Core.Nothing,
      kernelId = Core.Nothing,
      cpuOptions = Core.Nothing,
      keyName = Core.Nothing,
      networkInterfaces = Core.Nothing,
      disableApiTermination = Core.Nothing,
      metadataOptions = Core.Nothing
    }

-- | The security group IDs.
responseLaunchTemplateData_securityGroupIds :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe [Core.Text])
responseLaunchTemplateData_securityGroupIds = Lens.lens (\ResponseLaunchTemplateData' {securityGroupIds} -> securityGroupIds) (\s@ResponseLaunchTemplateData' {} a -> s {securityGroupIds = a} :: ResponseLaunchTemplateData) Core.. Lens.mapping Lens._Coerce

-- | The tags.
responseLaunchTemplateData_tagSpecifications :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe [LaunchTemplateTagSpecification])
responseLaunchTemplateData_tagSpecifications = Lens.lens (\ResponseLaunchTemplateData' {tagSpecifications} -> tagSpecifications) (\s@ResponseLaunchTemplateData' {} a -> s {tagSpecifications = a} :: ResponseLaunchTemplateData) Core.. Lens.mapping Lens._Coerce

-- | The elastic GPU specification.
responseLaunchTemplateData_elasticGpuSpecifications :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe [ElasticGpuSpecificationResponse])
responseLaunchTemplateData_elasticGpuSpecifications = Lens.lens (\ResponseLaunchTemplateData' {elasticGpuSpecifications} -> elasticGpuSpecifications) (\s@ResponseLaunchTemplateData' {} a -> s {elasticGpuSpecifications = a} :: ResponseLaunchTemplateData) Core.. Lens.mapping Lens._Coerce

-- | The instance type.
responseLaunchTemplateData_instanceType :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe InstanceType)
responseLaunchTemplateData_instanceType = Lens.lens (\ResponseLaunchTemplateData' {instanceType} -> instanceType) (\s@ResponseLaunchTemplateData' {} a -> s {instanceType = a} :: ResponseLaunchTemplateData)

-- | Information about the Capacity Reservation targeting option.
responseLaunchTemplateData_capacityReservationSpecification :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe LaunchTemplateCapacityReservationSpecificationResponse)
responseLaunchTemplateData_capacityReservationSpecification = Lens.lens (\ResponseLaunchTemplateData' {capacityReservationSpecification} -> capacityReservationSpecification) (\s@ResponseLaunchTemplateData' {} a -> s {capacityReservationSpecification = a} :: ResponseLaunchTemplateData)

-- | Indicates whether the instance is optimized for Amazon EBS I\/O.
responseLaunchTemplateData_ebsOptimized :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe Core.Bool)
responseLaunchTemplateData_ebsOptimized = Lens.lens (\ResponseLaunchTemplateData' {ebsOptimized} -> ebsOptimized) (\s@ResponseLaunchTemplateData' {} a -> s {ebsOptimized = a} :: ResponseLaunchTemplateData)

-- | The user data for the instance.
responseLaunchTemplateData_userData :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe Core.Text)
responseLaunchTemplateData_userData = Lens.lens (\ResponseLaunchTemplateData' {userData} -> userData) (\s@ResponseLaunchTemplateData' {} a -> s {userData = a} :: ResponseLaunchTemplateData)

-- | The placement of the instance.
responseLaunchTemplateData_placement :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe LaunchTemplatePlacement)
responseLaunchTemplateData_placement = Lens.lens (\ResponseLaunchTemplateData' {placement} -> placement) (\s@ResponseLaunchTemplateData' {} a -> s {placement = a} :: ResponseLaunchTemplateData)

-- | The ID of the RAM disk, if applicable.
responseLaunchTemplateData_ramDiskId :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe Core.Text)
responseLaunchTemplateData_ramDiskId = Lens.lens (\ResponseLaunchTemplateData' {ramDiskId} -> ramDiskId) (\s@ResponseLaunchTemplateData' {} a -> s {ramDiskId = a} :: ResponseLaunchTemplateData)

-- | The credit option for CPU usage of the instance.
responseLaunchTemplateData_creditSpecification :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe CreditSpecification)
responseLaunchTemplateData_creditSpecification = Lens.lens (\ResponseLaunchTemplateData' {creditSpecification} -> creditSpecification) (\s@ResponseLaunchTemplateData' {} a -> s {creditSpecification = a} :: ResponseLaunchTemplateData)

-- | The market (purchasing) option for the instances.
responseLaunchTemplateData_instanceMarketOptions :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe LaunchTemplateInstanceMarketOptions)
responseLaunchTemplateData_instanceMarketOptions = Lens.lens (\ResponseLaunchTemplateData' {instanceMarketOptions} -> instanceMarketOptions) (\s@ResponseLaunchTemplateData' {} a -> s {instanceMarketOptions = a} :: ResponseLaunchTemplateData)

-- | The license configurations.
responseLaunchTemplateData_licenseSpecifications :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe [LaunchTemplateLicenseConfiguration])
responseLaunchTemplateData_licenseSpecifications = Lens.lens (\ResponseLaunchTemplateData' {licenseSpecifications} -> licenseSpecifications) (\s@ResponseLaunchTemplateData' {} a -> s {licenseSpecifications = a} :: ResponseLaunchTemplateData) Core.. Lens.mapping Lens._Coerce

-- | Indicates whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for
-- system shutdown).
responseLaunchTemplateData_instanceInitiatedShutdownBehavior :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe ShutdownBehavior)
responseLaunchTemplateData_instanceInitiatedShutdownBehavior = Lens.lens (\ResponseLaunchTemplateData' {instanceInitiatedShutdownBehavior} -> instanceInitiatedShutdownBehavior) (\s@ResponseLaunchTemplateData' {} a -> s {instanceInitiatedShutdownBehavior = a} :: ResponseLaunchTemplateData)

-- | The ID of the AMI that was used to launch the instance.
responseLaunchTemplateData_imageId :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe Core.Text)
responseLaunchTemplateData_imageId = Lens.lens (\ResponseLaunchTemplateData' {imageId} -> imageId) (\s@ResponseLaunchTemplateData' {} a -> s {imageId = a} :: ResponseLaunchTemplateData)

-- | The security group names.
responseLaunchTemplateData_securityGroups :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe [Core.Text])
responseLaunchTemplateData_securityGroups = Lens.lens (\ResponseLaunchTemplateData' {securityGroups} -> securityGroups) (\s@ResponseLaunchTemplateData' {} a -> s {securityGroups = a} :: ResponseLaunchTemplateData) Core.. Lens.mapping Lens._Coerce

-- | The elastic inference accelerator for the instance.
responseLaunchTemplateData_elasticInferenceAccelerators :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe [LaunchTemplateElasticInferenceAcceleratorResponse])
responseLaunchTemplateData_elasticInferenceAccelerators = Lens.lens (\ResponseLaunchTemplateData' {elasticInferenceAccelerators} -> elasticInferenceAccelerators) (\s@ResponseLaunchTemplateData' {} a -> s {elasticInferenceAccelerators = a} :: ResponseLaunchTemplateData) Core.. Lens.mapping Lens._Coerce

-- | The IAM instance profile.
responseLaunchTemplateData_iamInstanceProfile :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe LaunchTemplateIamInstanceProfileSpecification)
responseLaunchTemplateData_iamInstanceProfile = Lens.lens (\ResponseLaunchTemplateData' {iamInstanceProfile} -> iamInstanceProfile) (\s@ResponseLaunchTemplateData' {} a -> s {iamInstanceProfile = a} :: ResponseLaunchTemplateData)

-- | Indicates whether an instance is configured for hibernation. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate Your Instance>
-- in the /Amazon Elastic Compute Cloud User Guide/.
responseLaunchTemplateData_hibernationOptions :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe LaunchTemplateHibernationOptions)
responseLaunchTemplateData_hibernationOptions = Lens.lens (\ResponseLaunchTemplateData' {hibernationOptions} -> hibernationOptions) (\s@ResponseLaunchTemplateData' {} a -> s {hibernationOptions = a} :: ResponseLaunchTemplateData)

-- | The monitoring for the instance.
responseLaunchTemplateData_monitoring :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe LaunchTemplatesMonitoring)
responseLaunchTemplateData_monitoring = Lens.lens (\ResponseLaunchTemplateData' {monitoring} -> monitoring) (\s@ResponseLaunchTemplateData' {} a -> s {monitoring = a} :: ResponseLaunchTemplateData)

-- | The block device mappings.
responseLaunchTemplateData_blockDeviceMappings :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe [LaunchTemplateBlockDeviceMapping])
responseLaunchTemplateData_blockDeviceMappings = Lens.lens (\ResponseLaunchTemplateData' {blockDeviceMappings} -> blockDeviceMappings) (\s@ResponseLaunchTemplateData' {} a -> s {blockDeviceMappings = a} :: ResponseLaunchTemplateData) Core.. Lens.mapping Lens._Coerce

-- | Indicates whether the instance is enabled for AWS Nitro Enclaves.
responseLaunchTemplateData_enclaveOptions :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe LaunchTemplateEnclaveOptions)
responseLaunchTemplateData_enclaveOptions = Lens.lens (\ResponseLaunchTemplateData' {enclaveOptions} -> enclaveOptions) (\s@ResponseLaunchTemplateData' {} a -> s {enclaveOptions = a} :: ResponseLaunchTemplateData)

-- | The ID of the kernel, if applicable.
responseLaunchTemplateData_kernelId :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe Core.Text)
responseLaunchTemplateData_kernelId = Lens.lens (\ResponseLaunchTemplateData' {kernelId} -> kernelId) (\s@ResponseLaunchTemplateData' {} a -> s {kernelId = a} :: ResponseLaunchTemplateData)

-- | The CPU options for the instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-optimize-cpu.html Optimizing CPU Options>
-- in the /Amazon Elastic Compute Cloud User Guide/.
responseLaunchTemplateData_cpuOptions :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe LaunchTemplateCpuOptions)
responseLaunchTemplateData_cpuOptions = Lens.lens (\ResponseLaunchTemplateData' {cpuOptions} -> cpuOptions) (\s@ResponseLaunchTemplateData' {} a -> s {cpuOptions = a} :: ResponseLaunchTemplateData)

-- | The name of the key pair.
responseLaunchTemplateData_keyName :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe Core.Text)
responseLaunchTemplateData_keyName = Lens.lens (\ResponseLaunchTemplateData' {keyName} -> keyName) (\s@ResponseLaunchTemplateData' {} a -> s {keyName = a} :: ResponseLaunchTemplateData)

-- | The network interfaces.
responseLaunchTemplateData_networkInterfaces :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe [LaunchTemplateInstanceNetworkInterfaceSpecification])
responseLaunchTemplateData_networkInterfaces = Lens.lens (\ResponseLaunchTemplateData' {networkInterfaces} -> networkInterfaces) (\s@ResponseLaunchTemplateData' {} a -> s {networkInterfaces = a} :: ResponseLaunchTemplateData) Core.. Lens.mapping Lens._Coerce

-- | If set to @true@, indicates that the instance cannot be terminated using
-- the Amazon EC2 console, command line tool, or API.
responseLaunchTemplateData_disableApiTermination :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe Core.Bool)
responseLaunchTemplateData_disableApiTermination = Lens.lens (\ResponseLaunchTemplateData' {disableApiTermination} -> disableApiTermination) (\s@ResponseLaunchTemplateData' {} a -> s {disableApiTermination = a} :: ResponseLaunchTemplateData)

-- | The metadata options for the instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance Metadata and User Data>
-- in the /Amazon Elastic Compute Cloud User Guide/.
responseLaunchTemplateData_metadataOptions :: Lens.Lens' ResponseLaunchTemplateData (Core.Maybe LaunchTemplateInstanceMetadataOptions)
responseLaunchTemplateData_metadataOptions = Lens.lens (\ResponseLaunchTemplateData' {metadataOptions} -> metadataOptions) (\s@ResponseLaunchTemplateData' {} a -> s {metadataOptions = a} :: ResponseLaunchTemplateData)

instance Core.FromXML ResponseLaunchTemplateData where
  parseXML x =
    ResponseLaunchTemplateData'
      Core.<$> ( x Core..@? "securityGroupIdSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> ( x Core..@? "tagSpecificationSet"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> ( x Core..@? "elasticGpuSpecificationSet"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "instanceType")
      Core.<*> (x Core..@? "capacityReservationSpecification")
      Core.<*> (x Core..@? "ebsOptimized")
      Core.<*> (x Core..@? "userData")
      Core.<*> (x Core..@? "placement")
      Core.<*> (x Core..@? "ramDiskId")
      Core.<*> (x Core..@? "creditSpecification")
      Core.<*> (x Core..@? "instanceMarketOptions")
      Core.<*> ( x Core..@? "licenseSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "instanceInitiatedShutdownBehavior")
      Core.<*> (x Core..@? "imageId")
      Core.<*> ( x Core..@? "securityGroupSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> ( x Core..@? "elasticInferenceAcceleratorSet"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "iamInstanceProfile")
      Core.<*> (x Core..@? "hibernationOptions")
      Core.<*> (x Core..@? "monitoring")
      Core.<*> ( x Core..@? "blockDeviceMappingSet"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "enclaveOptions")
      Core.<*> (x Core..@? "kernelId")
      Core.<*> (x Core..@? "cpuOptions")
      Core.<*> (x Core..@? "keyName")
      Core.<*> ( x Core..@? "networkInterfaceSet"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "disableApiTermination")
      Core.<*> (x Core..@? "metadataOptions")

instance Core.Hashable ResponseLaunchTemplateData

instance Core.NFData ResponseLaunchTemplateData
