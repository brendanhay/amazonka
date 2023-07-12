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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataCapacityReservationSpecificationDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataCpuOptionsDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataCreditSpecificationDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataElasticGpuSpecificationSetDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataEnclaveOptionsDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataHibernationOptionsDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataIamInstanceProfileDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceMarketOptionsDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceRequirementsDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataLicenseSetDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataMaintenanceOptionsDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataMetadataOptionsDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataMonitoringDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataPlacementDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails

-- | The information to include in an Amazon Elastic Compute Cloud (Amazon
-- EC2) launch template.
--
-- /See:/ 'newAwsEc2LaunchTemplateDataDetails' smart constructor.
data AwsEc2LaunchTemplateDataDetails = AwsEc2LaunchTemplateDataDetails'
  { -- | Information about a block device mapping for an Amazon EC2 launch
    -- template.
    blockDeviceMappingSet :: Prelude.Maybe [AwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails],
    -- | Specifies an instance\'s Capacity Reservation targeting option. You can
    -- specify only one option at a time.
    capacityReservationSpecification :: Prelude.Maybe AwsEc2LaunchTemplateDataCapacityReservationSpecificationDetails,
    -- | Specifies the CPU options for an instance. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-optimize-cpu.html Optimize CPU options>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    cpuOptions :: Prelude.Maybe AwsEc2LaunchTemplateDataCpuOptionsDetails,
    -- | Specifies the credit option for CPU usage of a T2, T3, or T3a instance.
    creditSpecification :: Prelude.Maybe AwsEc2LaunchTemplateDataCreditSpecificationDetails,
    -- | Indicates whether to enable the instance for stop protection. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Stop_Start.html#Using_StopProtection Enable stop protection>
    -- in the /Amazon EC2 User Guide/.
    disableApiStop :: Prelude.Maybe Prelude.Bool,
    -- | If you set this parameter to @true@, you can\'t terminate the instance
    -- using the Amazon EC2 console, CLI, or API. If set to @true@, you can.
    disableApiTermination :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether the instance is optimized for Amazon EBS I\/O.
    ebsOptimized :: Prelude.Maybe Prelude.Bool,
    -- | Provides details about Elastic Graphics accelerators to associate with
    -- the instance.
    elasticGpuSpecificationSet :: Prelude.Maybe [AwsEc2LaunchTemplateDataElasticGpuSpecificationSetDetails],
    -- | The Amazon Elastic Inference accelerator for the instance.
    elasticInferenceAcceleratorSet :: Prelude.Maybe [AwsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails],
    -- | Indicates whether the Amazon EC2 instance is enabled for Amazon Web
    -- Services Nitro Enclaves.
    enclaveOptions :: Prelude.Maybe AwsEc2LaunchTemplateDataEnclaveOptionsDetails,
    -- | Specifies whether your Amazon EC2 instance is configured for
    -- hibernation.
    hibernationOptions :: Prelude.Maybe AwsEc2LaunchTemplateDataHibernationOptionsDetails,
    -- | The name or Amazon Resource Name (ARN) of an IAM instance profile.
    iamInstanceProfile :: Prelude.Maybe AwsEc2LaunchTemplateDataIamInstanceProfileDetails,
    -- | The ID of the Amazon Machine Image (AMI).
    imageId :: Prelude.Maybe Prelude.Text,
    -- | Provides the options for specifying the instance initiated shutdown
    -- behavior.
    instanceInitiatedShutdownBehavior :: Prelude.Maybe Prelude.Text,
    -- | Specifies the market (purchasing) option for an instance.
    instanceMarketOptions :: Prelude.Maybe AwsEc2LaunchTemplateDataInstanceMarketOptionsDetails,
    -- | The attributes for the instance types. When you specify instance
    -- attributes, Amazon EC2 will identify instance types with these
    -- attributes. If you specify @InstanceRequirements@, you can\'t specify
    -- @InstanceType@.
    instanceRequirements :: Prelude.Maybe AwsEc2LaunchTemplateDataInstanceRequirementsDetails,
    -- | The instance type. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
    -- in the /Amazon EC2 User Guide/. If you specify @InstanceType@, you
    -- can\'t specify @InstanceRequirements@.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | The ID of the kernel.
    kernelId :: Prelude.Maybe Prelude.Text,
    -- | The name of the key pair that allows users to connect to the instance.
    keyName :: Prelude.Maybe Prelude.Text,
    -- | Specifies a license configuration for an instance.
    licenseSet :: Prelude.Maybe [AwsEc2LaunchTemplateDataLicenseSetDetails],
    -- | The maintenance options of your instance.
    maintenanceOptions :: Prelude.Maybe AwsEc2LaunchTemplateDataMaintenanceOptionsDetails,
    -- | The metadata options for the instance. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance metadata and user data>
    -- in the /Amazon EC2 User Guide/.
    metadataOptions :: Prelude.Maybe AwsEc2LaunchTemplateDataMetadataOptionsDetails,
    -- | The monitoring for the instance.
    monitoring :: Prelude.Maybe AwsEc2LaunchTemplateDataMonitoringDetails,
    -- | Specifies the parameters for a network interface that is attached to the
    -- instance.
    networkInterfaceSet :: Prelude.Maybe [AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails],
    -- | Specifies the placement of an instance.
    placement :: Prelude.Maybe AwsEc2LaunchTemplateDataPlacementDetails,
    -- | The options for the instance hostname.
    privateDnsNameOptions :: Prelude.Maybe AwsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails,
    -- | The ID of the RAM disk.
    ramDiskId :: Prelude.Maybe Prelude.Text,
    -- | One or more security group IDs.
    securityGroupIdSet :: Prelude.Maybe [Prelude.Text],
    -- | One or more security group names. For a nondefault VPC, you must use
    -- security group IDs instead. You cannot specify both a security group ID
    -- and security name in the same request.
    securityGroupSet :: Prelude.Maybe [Prelude.Text],
    -- | The user data to make available to the instance.
    userData :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2LaunchTemplateDataDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blockDeviceMappingSet', 'awsEc2LaunchTemplateDataDetails_blockDeviceMappingSet' - Information about a block device mapping for an Amazon EC2 launch
-- template.
--
-- 'capacityReservationSpecification', 'awsEc2LaunchTemplateDataDetails_capacityReservationSpecification' - Specifies an instance\'s Capacity Reservation targeting option. You can
-- specify only one option at a time.
--
-- 'cpuOptions', 'awsEc2LaunchTemplateDataDetails_cpuOptions' - Specifies the CPU options for an instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-optimize-cpu.html Optimize CPU options>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'creditSpecification', 'awsEc2LaunchTemplateDataDetails_creditSpecification' - Specifies the credit option for CPU usage of a T2, T3, or T3a instance.
--
-- 'disableApiStop', 'awsEc2LaunchTemplateDataDetails_disableApiStop' - Indicates whether to enable the instance for stop protection. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Stop_Start.html#Using_StopProtection Enable stop protection>
-- in the /Amazon EC2 User Guide/.
--
-- 'disableApiTermination', 'awsEc2LaunchTemplateDataDetails_disableApiTermination' - If you set this parameter to @true@, you can\'t terminate the instance
-- using the Amazon EC2 console, CLI, or API. If set to @true@, you can.
--
-- 'ebsOptimized', 'awsEc2LaunchTemplateDataDetails_ebsOptimized' - Indicates whether the instance is optimized for Amazon EBS I\/O.
--
-- 'elasticGpuSpecificationSet', 'awsEc2LaunchTemplateDataDetails_elasticGpuSpecificationSet' - Provides details about Elastic Graphics accelerators to associate with
-- the instance.
--
-- 'elasticInferenceAcceleratorSet', 'awsEc2LaunchTemplateDataDetails_elasticInferenceAcceleratorSet' - The Amazon Elastic Inference accelerator for the instance.
--
-- 'enclaveOptions', 'awsEc2LaunchTemplateDataDetails_enclaveOptions' - Indicates whether the Amazon EC2 instance is enabled for Amazon Web
-- Services Nitro Enclaves.
--
-- 'hibernationOptions', 'awsEc2LaunchTemplateDataDetails_hibernationOptions' - Specifies whether your Amazon EC2 instance is configured for
-- hibernation.
--
-- 'iamInstanceProfile', 'awsEc2LaunchTemplateDataDetails_iamInstanceProfile' - The name or Amazon Resource Name (ARN) of an IAM instance profile.
--
-- 'imageId', 'awsEc2LaunchTemplateDataDetails_imageId' - The ID of the Amazon Machine Image (AMI).
--
-- 'instanceInitiatedShutdownBehavior', 'awsEc2LaunchTemplateDataDetails_instanceInitiatedShutdownBehavior' - Provides the options for specifying the instance initiated shutdown
-- behavior.
--
-- 'instanceMarketOptions', 'awsEc2LaunchTemplateDataDetails_instanceMarketOptions' - Specifies the market (purchasing) option for an instance.
--
-- 'instanceRequirements', 'awsEc2LaunchTemplateDataDetails_instanceRequirements' - The attributes for the instance types. When you specify instance
-- attributes, Amazon EC2 will identify instance types with these
-- attributes. If you specify @InstanceRequirements@, you can\'t specify
-- @InstanceType@.
--
-- 'instanceType', 'awsEc2LaunchTemplateDataDetails_instanceType' - The instance type. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
-- in the /Amazon EC2 User Guide/. If you specify @InstanceType@, you
-- can\'t specify @InstanceRequirements@.
--
-- 'kernelId', 'awsEc2LaunchTemplateDataDetails_kernelId' - The ID of the kernel.
--
-- 'keyName', 'awsEc2LaunchTemplateDataDetails_keyName' - The name of the key pair that allows users to connect to the instance.
--
-- 'licenseSet', 'awsEc2LaunchTemplateDataDetails_licenseSet' - Specifies a license configuration for an instance.
--
-- 'maintenanceOptions', 'awsEc2LaunchTemplateDataDetails_maintenanceOptions' - The maintenance options of your instance.
--
-- 'metadataOptions', 'awsEc2LaunchTemplateDataDetails_metadataOptions' - The metadata options for the instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance metadata and user data>
-- in the /Amazon EC2 User Guide/.
--
-- 'monitoring', 'awsEc2LaunchTemplateDataDetails_monitoring' - The monitoring for the instance.
--
-- 'networkInterfaceSet', 'awsEc2LaunchTemplateDataDetails_networkInterfaceSet' - Specifies the parameters for a network interface that is attached to the
-- instance.
--
-- 'placement', 'awsEc2LaunchTemplateDataDetails_placement' - Specifies the placement of an instance.
--
-- 'privateDnsNameOptions', 'awsEc2LaunchTemplateDataDetails_privateDnsNameOptions' - The options for the instance hostname.
--
-- 'ramDiskId', 'awsEc2LaunchTemplateDataDetails_ramDiskId' - The ID of the RAM disk.
--
-- 'securityGroupIdSet', 'awsEc2LaunchTemplateDataDetails_securityGroupIdSet' - One or more security group IDs.
--
-- 'securityGroupSet', 'awsEc2LaunchTemplateDataDetails_securityGroupSet' - One or more security group names. For a nondefault VPC, you must use
-- security group IDs instead. You cannot specify both a security group ID
-- and security name in the same request.
--
-- 'userData', 'awsEc2LaunchTemplateDataDetails_userData' - The user data to make available to the instance.
newAwsEc2LaunchTemplateDataDetails ::
  AwsEc2LaunchTemplateDataDetails
newAwsEc2LaunchTemplateDataDetails =
  AwsEc2LaunchTemplateDataDetails'
    { blockDeviceMappingSet =
        Prelude.Nothing,
      capacityReservationSpecification =
        Prelude.Nothing,
      cpuOptions = Prelude.Nothing,
      creditSpecification = Prelude.Nothing,
      disableApiStop = Prelude.Nothing,
      disableApiTermination = Prelude.Nothing,
      ebsOptimized = Prelude.Nothing,
      elasticGpuSpecificationSet =
        Prelude.Nothing,
      elasticInferenceAcceleratorSet =
        Prelude.Nothing,
      enclaveOptions = Prelude.Nothing,
      hibernationOptions = Prelude.Nothing,
      iamInstanceProfile = Prelude.Nothing,
      imageId = Prelude.Nothing,
      instanceInitiatedShutdownBehavior =
        Prelude.Nothing,
      instanceMarketOptions = Prelude.Nothing,
      instanceRequirements = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      kernelId = Prelude.Nothing,
      keyName = Prelude.Nothing,
      licenseSet = Prelude.Nothing,
      maintenanceOptions = Prelude.Nothing,
      metadataOptions = Prelude.Nothing,
      monitoring = Prelude.Nothing,
      networkInterfaceSet = Prelude.Nothing,
      placement = Prelude.Nothing,
      privateDnsNameOptions = Prelude.Nothing,
      ramDiskId = Prelude.Nothing,
      securityGroupIdSet = Prelude.Nothing,
      securityGroupSet = Prelude.Nothing,
      userData = Prelude.Nothing
    }

-- | Information about a block device mapping for an Amazon EC2 launch
-- template.
awsEc2LaunchTemplateDataDetails_blockDeviceMappingSet :: Lens.Lens' AwsEc2LaunchTemplateDataDetails (Prelude.Maybe [AwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails])
awsEc2LaunchTemplateDataDetails_blockDeviceMappingSet = Lens.lens (\AwsEc2LaunchTemplateDataDetails' {blockDeviceMappingSet} -> blockDeviceMappingSet) (\s@AwsEc2LaunchTemplateDataDetails' {} a -> s {blockDeviceMappingSet = a} :: AwsEc2LaunchTemplateDataDetails) Prelude.. Lens.mapping Lens.coerced

-- | Specifies an instance\'s Capacity Reservation targeting option. You can
-- specify only one option at a time.
awsEc2LaunchTemplateDataDetails_capacityReservationSpecification :: Lens.Lens' AwsEc2LaunchTemplateDataDetails (Prelude.Maybe AwsEc2LaunchTemplateDataCapacityReservationSpecificationDetails)
awsEc2LaunchTemplateDataDetails_capacityReservationSpecification = Lens.lens (\AwsEc2LaunchTemplateDataDetails' {capacityReservationSpecification} -> capacityReservationSpecification) (\s@AwsEc2LaunchTemplateDataDetails' {} a -> s {capacityReservationSpecification = a} :: AwsEc2LaunchTemplateDataDetails)

-- | Specifies the CPU options for an instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-optimize-cpu.html Optimize CPU options>
-- in the /Amazon Elastic Compute Cloud User Guide/.
awsEc2LaunchTemplateDataDetails_cpuOptions :: Lens.Lens' AwsEc2LaunchTemplateDataDetails (Prelude.Maybe AwsEc2LaunchTemplateDataCpuOptionsDetails)
awsEc2LaunchTemplateDataDetails_cpuOptions = Lens.lens (\AwsEc2LaunchTemplateDataDetails' {cpuOptions} -> cpuOptions) (\s@AwsEc2LaunchTemplateDataDetails' {} a -> s {cpuOptions = a} :: AwsEc2LaunchTemplateDataDetails)

-- | Specifies the credit option for CPU usage of a T2, T3, or T3a instance.
awsEc2LaunchTemplateDataDetails_creditSpecification :: Lens.Lens' AwsEc2LaunchTemplateDataDetails (Prelude.Maybe AwsEc2LaunchTemplateDataCreditSpecificationDetails)
awsEc2LaunchTemplateDataDetails_creditSpecification = Lens.lens (\AwsEc2LaunchTemplateDataDetails' {creditSpecification} -> creditSpecification) (\s@AwsEc2LaunchTemplateDataDetails' {} a -> s {creditSpecification = a} :: AwsEc2LaunchTemplateDataDetails)

-- | Indicates whether to enable the instance for stop protection. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Stop_Start.html#Using_StopProtection Enable stop protection>
-- in the /Amazon EC2 User Guide/.
awsEc2LaunchTemplateDataDetails_disableApiStop :: Lens.Lens' AwsEc2LaunchTemplateDataDetails (Prelude.Maybe Prelude.Bool)
awsEc2LaunchTemplateDataDetails_disableApiStop = Lens.lens (\AwsEc2LaunchTemplateDataDetails' {disableApiStop} -> disableApiStop) (\s@AwsEc2LaunchTemplateDataDetails' {} a -> s {disableApiStop = a} :: AwsEc2LaunchTemplateDataDetails)

-- | If you set this parameter to @true@, you can\'t terminate the instance
-- using the Amazon EC2 console, CLI, or API. If set to @true@, you can.
awsEc2LaunchTemplateDataDetails_disableApiTermination :: Lens.Lens' AwsEc2LaunchTemplateDataDetails (Prelude.Maybe Prelude.Bool)
awsEc2LaunchTemplateDataDetails_disableApiTermination = Lens.lens (\AwsEc2LaunchTemplateDataDetails' {disableApiTermination} -> disableApiTermination) (\s@AwsEc2LaunchTemplateDataDetails' {} a -> s {disableApiTermination = a} :: AwsEc2LaunchTemplateDataDetails)

-- | Indicates whether the instance is optimized for Amazon EBS I\/O.
awsEc2LaunchTemplateDataDetails_ebsOptimized :: Lens.Lens' AwsEc2LaunchTemplateDataDetails (Prelude.Maybe Prelude.Bool)
awsEc2LaunchTemplateDataDetails_ebsOptimized = Lens.lens (\AwsEc2LaunchTemplateDataDetails' {ebsOptimized} -> ebsOptimized) (\s@AwsEc2LaunchTemplateDataDetails' {} a -> s {ebsOptimized = a} :: AwsEc2LaunchTemplateDataDetails)

-- | Provides details about Elastic Graphics accelerators to associate with
-- the instance.
awsEc2LaunchTemplateDataDetails_elasticGpuSpecificationSet :: Lens.Lens' AwsEc2LaunchTemplateDataDetails (Prelude.Maybe [AwsEc2LaunchTemplateDataElasticGpuSpecificationSetDetails])
awsEc2LaunchTemplateDataDetails_elasticGpuSpecificationSet = Lens.lens (\AwsEc2LaunchTemplateDataDetails' {elasticGpuSpecificationSet} -> elasticGpuSpecificationSet) (\s@AwsEc2LaunchTemplateDataDetails' {} a -> s {elasticGpuSpecificationSet = a} :: AwsEc2LaunchTemplateDataDetails) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Elastic Inference accelerator for the instance.
awsEc2LaunchTemplateDataDetails_elasticInferenceAcceleratorSet :: Lens.Lens' AwsEc2LaunchTemplateDataDetails (Prelude.Maybe [AwsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails])
awsEc2LaunchTemplateDataDetails_elasticInferenceAcceleratorSet = Lens.lens (\AwsEc2LaunchTemplateDataDetails' {elasticInferenceAcceleratorSet} -> elasticInferenceAcceleratorSet) (\s@AwsEc2LaunchTemplateDataDetails' {} a -> s {elasticInferenceAcceleratorSet = a} :: AwsEc2LaunchTemplateDataDetails) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether the Amazon EC2 instance is enabled for Amazon Web
-- Services Nitro Enclaves.
awsEc2LaunchTemplateDataDetails_enclaveOptions :: Lens.Lens' AwsEc2LaunchTemplateDataDetails (Prelude.Maybe AwsEc2LaunchTemplateDataEnclaveOptionsDetails)
awsEc2LaunchTemplateDataDetails_enclaveOptions = Lens.lens (\AwsEc2LaunchTemplateDataDetails' {enclaveOptions} -> enclaveOptions) (\s@AwsEc2LaunchTemplateDataDetails' {} a -> s {enclaveOptions = a} :: AwsEc2LaunchTemplateDataDetails)

-- | Specifies whether your Amazon EC2 instance is configured for
-- hibernation.
awsEc2LaunchTemplateDataDetails_hibernationOptions :: Lens.Lens' AwsEc2LaunchTemplateDataDetails (Prelude.Maybe AwsEc2LaunchTemplateDataHibernationOptionsDetails)
awsEc2LaunchTemplateDataDetails_hibernationOptions = Lens.lens (\AwsEc2LaunchTemplateDataDetails' {hibernationOptions} -> hibernationOptions) (\s@AwsEc2LaunchTemplateDataDetails' {} a -> s {hibernationOptions = a} :: AwsEc2LaunchTemplateDataDetails)

-- | The name or Amazon Resource Name (ARN) of an IAM instance profile.
awsEc2LaunchTemplateDataDetails_iamInstanceProfile :: Lens.Lens' AwsEc2LaunchTemplateDataDetails (Prelude.Maybe AwsEc2LaunchTemplateDataIamInstanceProfileDetails)
awsEc2LaunchTemplateDataDetails_iamInstanceProfile = Lens.lens (\AwsEc2LaunchTemplateDataDetails' {iamInstanceProfile} -> iamInstanceProfile) (\s@AwsEc2LaunchTemplateDataDetails' {} a -> s {iamInstanceProfile = a} :: AwsEc2LaunchTemplateDataDetails)

-- | The ID of the Amazon Machine Image (AMI).
awsEc2LaunchTemplateDataDetails_imageId :: Lens.Lens' AwsEc2LaunchTemplateDataDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataDetails_imageId = Lens.lens (\AwsEc2LaunchTemplateDataDetails' {imageId} -> imageId) (\s@AwsEc2LaunchTemplateDataDetails' {} a -> s {imageId = a} :: AwsEc2LaunchTemplateDataDetails)

-- | Provides the options for specifying the instance initiated shutdown
-- behavior.
awsEc2LaunchTemplateDataDetails_instanceInitiatedShutdownBehavior :: Lens.Lens' AwsEc2LaunchTemplateDataDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataDetails_instanceInitiatedShutdownBehavior = Lens.lens (\AwsEc2LaunchTemplateDataDetails' {instanceInitiatedShutdownBehavior} -> instanceInitiatedShutdownBehavior) (\s@AwsEc2LaunchTemplateDataDetails' {} a -> s {instanceInitiatedShutdownBehavior = a} :: AwsEc2LaunchTemplateDataDetails)

-- | Specifies the market (purchasing) option for an instance.
awsEc2LaunchTemplateDataDetails_instanceMarketOptions :: Lens.Lens' AwsEc2LaunchTemplateDataDetails (Prelude.Maybe AwsEc2LaunchTemplateDataInstanceMarketOptionsDetails)
awsEc2LaunchTemplateDataDetails_instanceMarketOptions = Lens.lens (\AwsEc2LaunchTemplateDataDetails' {instanceMarketOptions} -> instanceMarketOptions) (\s@AwsEc2LaunchTemplateDataDetails' {} a -> s {instanceMarketOptions = a} :: AwsEc2LaunchTemplateDataDetails)

-- | The attributes for the instance types. When you specify instance
-- attributes, Amazon EC2 will identify instance types with these
-- attributes. If you specify @InstanceRequirements@, you can\'t specify
-- @InstanceType@.
awsEc2LaunchTemplateDataDetails_instanceRequirements :: Lens.Lens' AwsEc2LaunchTemplateDataDetails (Prelude.Maybe AwsEc2LaunchTemplateDataInstanceRequirementsDetails)
awsEc2LaunchTemplateDataDetails_instanceRequirements = Lens.lens (\AwsEc2LaunchTemplateDataDetails' {instanceRequirements} -> instanceRequirements) (\s@AwsEc2LaunchTemplateDataDetails' {} a -> s {instanceRequirements = a} :: AwsEc2LaunchTemplateDataDetails)

-- | The instance type. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
-- in the /Amazon EC2 User Guide/. If you specify @InstanceType@, you
-- can\'t specify @InstanceRequirements@.
awsEc2LaunchTemplateDataDetails_instanceType :: Lens.Lens' AwsEc2LaunchTemplateDataDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataDetails_instanceType = Lens.lens (\AwsEc2LaunchTemplateDataDetails' {instanceType} -> instanceType) (\s@AwsEc2LaunchTemplateDataDetails' {} a -> s {instanceType = a} :: AwsEc2LaunchTemplateDataDetails)

-- | The ID of the kernel.
awsEc2LaunchTemplateDataDetails_kernelId :: Lens.Lens' AwsEc2LaunchTemplateDataDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataDetails_kernelId = Lens.lens (\AwsEc2LaunchTemplateDataDetails' {kernelId} -> kernelId) (\s@AwsEc2LaunchTemplateDataDetails' {} a -> s {kernelId = a} :: AwsEc2LaunchTemplateDataDetails)

-- | The name of the key pair that allows users to connect to the instance.
awsEc2LaunchTemplateDataDetails_keyName :: Lens.Lens' AwsEc2LaunchTemplateDataDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataDetails_keyName = Lens.lens (\AwsEc2LaunchTemplateDataDetails' {keyName} -> keyName) (\s@AwsEc2LaunchTemplateDataDetails' {} a -> s {keyName = a} :: AwsEc2LaunchTemplateDataDetails)

-- | Specifies a license configuration for an instance.
awsEc2LaunchTemplateDataDetails_licenseSet :: Lens.Lens' AwsEc2LaunchTemplateDataDetails (Prelude.Maybe [AwsEc2LaunchTemplateDataLicenseSetDetails])
awsEc2LaunchTemplateDataDetails_licenseSet = Lens.lens (\AwsEc2LaunchTemplateDataDetails' {licenseSet} -> licenseSet) (\s@AwsEc2LaunchTemplateDataDetails' {} a -> s {licenseSet = a} :: AwsEc2LaunchTemplateDataDetails) Prelude.. Lens.mapping Lens.coerced

-- | The maintenance options of your instance.
awsEc2LaunchTemplateDataDetails_maintenanceOptions :: Lens.Lens' AwsEc2LaunchTemplateDataDetails (Prelude.Maybe AwsEc2LaunchTemplateDataMaintenanceOptionsDetails)
awsEc2LaunchTemplateDataDetails_maintenanceOptions = Lens.lens (\AwsEc2LaunchTemplateDataDetails' {maintenanceOptions} -> maintenanceOptions) (\s@AwsEc2LaunchTemplateDataDetails' {} a -> s {maintenanceOptions = a} :: AwsEc2LaunchTemplateDataDetails)

-- | The metadata options for the instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance metadata and user data>
-- in the /Amazon EC2 User Guide/.
awsEc2LaunchTemplateDataDetails_metadataOptions :: Lens.Lens' AwsEc2LaunchTemplateDataDetails (Prelude.Maybe AwsEc2LaunchTemplateDataMetadataOptionsDetails)
awsEc2LaunchTemplateDataDetails_metadataOptions = Lens.lens (\AwsEc2LaunchTemplateDataDetails' {metadataOptions} -> metadataOptions) (\s@AwsEc2LaunchTemplateDataDetails' {} a -> s {metadataOptions = a} :: AwsEc2LaunchTemplateDataDetails)

-- | The monitoring for the instance.
awsEc2LaunchTemplateDataDetails_monitoring :: Lens.Lens' AwsEc2LaunchTemplateDataDetails (Prelude.Maybe AwsEc2LaunchTemplateDataMonitoringDetails)
awsEc2LaunchTemplateDataDetails_monitoring = Lens.lens (\AwsEc2LaunchTemplateDataDetails' {monitoring} -> monitoring) (\s@AwsEc2LaunchTemplateDataDetails' {} a -> s {monitoring = a} :: AwsEc2LaunchTemplateDataDetails)

-- | Specifies the parameters for a network interface that is attached to the
-- instance.
awsEc2LaunchTemplateDataDetails_networkInterfaceSet :: Lens.Lens' AwsEc2LaunchTemplateDataDetails (Prelude.Maybe [AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails])
awsEc2LaunchTemplateDataDetails_networkInterfaceSet = Lens.lens (\AwsEc2LaunchTemplateDataDetails' {networkInterfaceSet} -> networkInterfaceSet) (\s@AwsEc2LaunchTemplateDataDetails' {} a -> s {networkInterfaceSet = a} :: AwsEc2LaunchTemplateDataDetails) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the placement of an instance.
awsEc2LaunchTemplateDataDetails_placement :: Lens.Lens' AwsEc2LaunchTemplateDataDetails (Prelude.Maybe AwsEc2LaunchTemplateDataPlacementDetails)
awsEc2LaunchTemplateDataDetails_placement = Lens.lens (\AwsEc2LaunchTemplateDataDetails' {placement} -> placement) (\s@AwsEc2LaunchTemplateDataDetails' {} a -> s {placement = a} :: AwsEc2LaunchTemplateDataDetails)

-- | The options for the instance hostname.
awsEc2LaunchTemplateDataDetails_privateDnsNameOptions :: Lens.Lens' AwsEc2LaunchTemplateDataDetails (Prelude.Maybe AwsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails)
awsEc2LaunchTemplateDataDetails_privateDnsNameOptions = Lens.lens (\AwsEc2LaunchTemplateDataDetails' {privateDnsNameOptions} -> privateDnsNameOptions) (\s@AwsEc2LaunchTemplateDataDetails' {} a -> s {privateDnsNameOptions = a} :: AwsEc2LaunchTemplateDataDetails)

-- | The ID of the RAM disk.
awsEc2LaunchTemplateDataDetails_ramDiskId :: Lens.Lens' AwsEc2LaunchTemplateDataDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataDetails_ramDiskId = Lens.lens (\AwsEc2LaunchTemplateDataDetails' {ramDiskId} -> ramDiskId) (\s@AwsEc2LaunchTemplateDataDetails' {} a -> s {ramDiskId = a} :: AwsEc2LaunchTemplateDataDetails)

-- | One or more security group IDs.
awsEc2LaunchTemplateDataDetails_securityGroupIdSet :: Lens.Lens' AwsEc2LaunchTemplateDataDetails (Prelude.Maybe [Prelude.Text])
awsEc2LaunchTemplateDataDetails_securityGroupIdSet = Lens.lens (\AwsEc2LaunchTemplateDataDetails' {securityGroupIdSet} -> securityGroupIdSet) (\s@AwsEc2LaunchTemplateDataDetails' {} a -> s {securityGroupIdSet = a} :: AwsEc2LaunchTemplateDataDetails) Prelude.. Lens.mapping Lens.coerced

-- | One or more security group names. For a nondefault VPC, you must use
-- security group IDs instead. You cannot specify both a security group ID
-- and security name in the same request.
awsEc2LaunchTemplateDataDetails_securityGroupSet :: Lens.Lens' AwsEc2LaunchTemplateDataDetails (Prelude.Maybe [Prelude.Text])
awsEc2LaunchTemplateDataDetails_securityGroupSet = Lens.lens (\AwsEc2LaunchTemplateDataDetails' {securityGroupSet} -> securityGroupSet) (\s@AwsEc2LaunchTemplateDataDetails' {} a -> s {securityGroupSet = a} :: AwsEc2LaunchTemplateDataDetails) Prelude.. Lens.mapping Lens.coerced

-- | The user data to make available to the instance.
awsEc2LaunchTemplateDataDetails_userData :: Lens.Lens' AwsEc2LaunchTemplateDataDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataDetails_userData = Lens.lens (\AwsEc2LaunchTemplateDataDetails' {userData} -> userData) (\s@AwsEc2LaunchTemplateDataDetails' {} a -> s {userData = a} :: AwsEc2LaunchTemplateDataDetails)

instance
  Data.FromJSON
    AwsEc2LaunchTemplateDataDetails
  where
  parseJSON =
    Data.withObject
      "AwsEc2LaunchTemplateDataDetails"
      ( \x ->
          AwsEc2LaunchTemplateDataDetails'
            Prelude.<$> ( x
                            Data..:? "BlockDeviceMappingSet"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "CapacityReservationSpecification")
            Prelude.<*> (x Data..:? "CpuOptions")
            Prelude.<*> (x Data..:? "CreditSpecification")
            Prelude.<*> (x Data..:? "DisableApiStop")
            Prelude.<*> (x Data..:? "DisableApiTermination")
            Prelude.<*> (x Data..:? "EbsOptimized")
            Prelude.<*> ( x
                            Data..:? "ElasticGpuSpecificationSet"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "ElasticInferenceAcceleratorSet"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "EnclaveOptions")
            Prelude.<*> (x Data..:? "HibernationOptions")
            Prelude.<*> (x Data..:? "IamInstanceProfile")
            Prelude.<*> (x Data..:? "ImageId")
            Prelude.<*> (x Data..:? "InstanceInitiatedShutdownBehavior")
            Prelude.<*> (x Data..:? "InstanceMarketOptions")
            Prelude.<*> (x Data..:? "InstanceRequirements")
            Prelude.<*> (x Data..:? "InstanceType")
            Prelude.<*> (x Data..:? "KernelId")
            Prelude.<*> (x Data..:? "KeyName")
            Prelude.<*> (x Data..:? "LicenseSet" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "MaintenanceOptions")
            Prelude.<*> (x Data..:? "MetadataOptions")
            Prelude.<*> (x Data..:? "Monitoring")
            Prelude.<*> ( x
                            Data..:? "NetworkInterfaceSet"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Placement")
            Prelude.<*> (x Data..:? "PrivateDnsNameOptions")
            Prelude.<*> (x Data..:? "RamDiskId")
            Prelude.<*> ( x
                            Data..:? "SecurityGroupIdSet"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "SecurityGroupSet"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "UserData")
      )

instance
  Prelude.Hashable
    AwsEc2LaunchTemplateDataDetails
  where
  hashWithSalt
    _salt
    AwsEc2LaunchTemplateDataDetails' {..} =
      _salt
        `Prelude.hashWithSalt` blockDeviceMappingSet
        `Prelude.hashWithSalt` capacityReservationSpecification
        `Prelude.hashWithSalt` cpuOptions
        `Prelude.hashWithSalt` creditSpecification
        `Prelude.hashWithSalt` disableApiStop
        `Prelude.hashWithSalt` disableApiTermination
        `Prelude.hashWithSalt` ebsOptimized
        `Prelude.hashWithSalt` elasticGpuSpecificationSet
        `Prelude.hashWithSalt` elasticInferenceAcceleratorSet
        `Prelude.hashWithSalt` enclaveOptions
        `Prelude.hashWithSalt` hibernationOptions
        `Prelude.hashWithSalt` iamInstanceProfile
        `Prelude.hashWithSalt` imageId
        `Prelude.hashWithSalt` instanceInitiatedShutdownBehavior
        `Prelude.hashWithSalt` instanceMarketOptions
        `Prelude.hashWithSalt` instanceRequirements
        `Prelude.hashWithSalt` instanceType
        `Prelude.hashWithSalt` kernelId
        `Prelude.hashWithSalt` keyName
        `Prelude.hashWithSalt` licenseSet
        `Prelude.hashWithSalt` maintenanceOptions
        `Prelude.hashWithSalt` metadataOptions
        `Prelude.hashWithSalt` monitoring
        `Prelude.hashWithSalt` networkInterfaceSet
        `Prelude.hashWithSalt` placement
        `Prelude.hashWithSalt` privateDnsNameOptions
        `Prelude.hashWithSalt` ramDiskId
        `Prelude.hashWithSalt` securityGroupIdSet
        `Prelude.hashWithSalt` securityGroupSet
        `Prelude.hashWithSalt` userData

instance
  Prelude.NFData
    AwsEc2LaunchTemplateDataDetails
  where
  rnf AwsEc2LaunchTemplateDataDetails' {..} =
    Prelude.rnf blockDeviceMappingSet
      `Prelude.seq` Prelude.rnf capacityReservationSpecification
      `Prelude.seq` Prelude.rnf cpuOptions
      `Prelude.seq` Prelude.rnf creditSpecification
      `Prelude.seq` Prelude.rnf disableApiStop
      `Prelude.seq` Prelude.rnf disableApiTermination
      `Prelude.seq` Prelude.rnf ebsOptimized
      `Prelude.seq` Prelude.rnf elasticGpuSpecificationSet
      `Prelude.seq` Prelude.rnf elasticInferenceAcceleratorSet
      `Prelude.seq` Prelude.rnf enclaveOptions
      `Prelude.seq` Prelude.rnf hibernationOptions
      `Prelude.seq` Prelude.rnf iamInstanceProfile
      `Prelude.seq` Prelude.rnf imageId
      `Prelude.seq` Prelude.rnf
        instanceInitiatedShutdownBehavior
      `Prelude.seq` Prelude.rnf instanceMarketOptions
      `Prelude.seq` Prelude.rnf instanceRequirements
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf kernelId
      `Prelude.seq` Prelude.rnf keyName
      `Prelude.seq` Prelude.rnf licenseSet
      `Prelude.seq` Prelude.rnf
        maintenanceOptions
      `Prelude.seq` Prelude.rnf
        metadataOptions
      `Prelude.seq` Prelude.rnf monitoring
      `Prelude.seq` Prelude.rnf
        networkInterfaceSet
      `Prelude.seq` Prelude.rnf
        placement
      `Prelude.seq` Prelude.rnf
        privateDnsNameOptions
      `Prelude.seq` Prelude.rnf
        ramDiskId
      `Prelude.seq` Prelude.rnf
        securityGroupIdSet
      `Prelude.seq` Prelude.rnf
        securityGroupSet
      `Prelude.seq` Prelude.rnf
        userData

instance Data.ToJSON AwsEc2LaunchTemplateDataDetails where
  toJSON AwsEc2LaunchTemplateDataDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BlockDeviceMappingSet" Data..=)
              Prelude.<$> blockDeviceMappingSet,
            ("CapacityReservationSpecification" Data..=)
              Prelude.<$> capacityReservationSpecification,
            ("CpuOptions" Data..=) Prelude.<$> cpuOptions,
            ("CreditSpecification" Data..=)
              Prelude.<$> creditSpecification,
            ("DisableApiStop" Data..=)
              Prelude.<$> disableApiStop,
            ("DisableApiTermination" Data..=)
              Prelude.<$> disableApiTermination,
            ("EbsOptimized" Data..=) Prelude.<$> ebsOptimized,
            ("ElasticGpuSpecificationSet" Data..=)
              Prelude.<$> elasticGpuSpecificationSet,
            ("ElasticInferenceAcceleratorSet" Data..=)
              Prelude.<$> elasticInferenceAcceleratorSet,
            ("EnclaveOptions" Data..=)
              Prelude.<$> enclaveOptions,
            ("HibernationOptions" Data..=)
              Prelude.<$> hibernationOptions,
            ("IamInstanceProfile" Data..=)
              Prelude.<$> iamInstanceProfile,
            ("ImageId" Data..=) Prelude.<$> imageId,
            ("InstanceInitiatedShutdownBehavior" Data..=)
              Prelude.<$> instanceInitiatedShutdownBehavior,
            ("InstanceMarketOptions" Data..=)
              Prelude.<$> instanceMarketOptions,
            ("InstanceRequirements" Data..=)
              Prelude.<$> instanceRequirements,
            ("InstanceType" Data..=) Prelude.<$> instanceType,
            ("KernelId" Data..=) Prelude.<$> kernelId,
            ("KeyName" Data..=) Prelude.<$> keyName,
            ("LicenseSet" Data..=) Prelude.<$> licenseSet,
            ("MaintenanceOptions" Data..=)
              Prelude.<$> maintenanceOptions,
            ("MetadataOptions" Data..=)
              Prelude.<$> metadataOptions,
            ("Monitoring" Data..=) Prelude.<$> monitoring,
            ("NetworkInterfaceSet" Data..=)
              Prelude.<$> networkInterfaceSet,
            ("Placement" Data..=) Prelude.<$> placement,
            ("PrivateDnsNameOptions" Data..=)
              Prelude.<$> privateDnsNameOptions,
            ("RamDiskId" Data..=) Prelude.<$> ramDiskId,
            ("SecurityGroupIdSet" Data..=)
              Prelude.<$> securityGroupIdSet,
            ("SecurityGroupSet" Data..=)
              Prelude.<$> securityGroupSet,
            ("UserData" Data..=) Prelude.<$> userData
          ]
      )
