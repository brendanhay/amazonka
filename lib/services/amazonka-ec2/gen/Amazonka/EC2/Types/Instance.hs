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
-- Module      : Amazonka.EC2.Types.Instance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.Instance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ArchitectureValues
import Amazonka.EC2.Types.BootModeValues
import Amazonka.EC2.Types.CapacityReservationSpecificationResponse
import Amazonka.EC2.Types.CpuOptions
import Amazonka.EC2.Types.DeviceType
import Amazonka.EC2.Types.ElasticGpuAssociation
import Amazonka.EC2.Types.ElasticInferenceAcceleratorAssociation
import Amazonka.EC2.Types.EnclaveOptions
import Amazonka.EC2.Types.GroupIdentifier
import Amazonka.EC2.Types.HibernationOptions
import Amazonka.EC2.Types.HypervisorType
import Amazonka.EC2.Types.IamInstanceProfile
import Amazonka.EC2.Types.InstanceBlockDeviceMapping
import Amazonka.EC2.Types.InstanceBootModeValues
import Amazonka.EC2.Types.InstanceLifecycleType
import Amazonka.EC2.Types.InstanceMaintenanceOptions
import Amazonka.EC2.Types.InstanceMetadataOptionsResponse
import Amazonka.EC2.Types.InstanceNetworkInterface
import Amazonka.EC2.Types.InstanceState
import Amazonka.EC2.Types.InstanceType
import Amazonka.EC2.Types.LicenseConfiguration
import Amazonka.EC2.Types.Monitoring
import Amazonka.EC2.Types.Placement
import Amazonka.EC2.Types.PlatformValues
import Amazonka.EC2.Types.PrivateDnsNameOptionsResponse
import Amazonka.EC2.Types.ProductCode
import Amazonka.EC2.Types.StateReason
import Amazonka.EC2.Types.Tag
import Amazonka.EC2.Types.VirtualizationType
import qualified Amazonka.Prelude as Prelude

-- | Describes an instance.
--
-- /See:/ 'newInstance' smart constructor.
data Instance = Instance'
  { -- | Any block device mapping entries for the instance.
    blockDeviceMappings :: Prelude.Maybe [InstanceBlockDeviceMapping],
    -- | The boot mode that was specified by the AMI. If the value is
    -- @uefi-preferred@, the AMI supports both UEFI and Legacy BIOS. The
    -- @currentInstanceBootMode@ parameter is the boot mode that is used to
    -- boot the instance at launch or start.
    --
    -- The operating system contained in the AMI must be configured to support
    -- the specified boot mode.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-boot.html Boot modes>
    -- in the /Amazon EC2 User Guide/.
    bootMode :: Prelude.Maybe BootModeValues,
    -- | The ID of the Capacity Reservation.
    capacityReservationId :: Prelude.Maybe Prelude.Text,
    -- | Information about the Capacity Reservation targeting option.
    capacityReservationSpecification :: Prelude.Maybe CapacityReservationSpecificationResponse,
    -- | The idempotency token you provided when you launched the instance, if
    -- applicable.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The CPU options for the instance.
    cpuOptions :: Prelude.Maybe CpuOptions,
    -- | The boot mode that is used to boot the instance at launch or start. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-boot.html Boot modes>
    -- in the /Amazon EC2 User Guide/.
    currentInstanceBootMode :: Prelude.Maybe InstanceBootModeValues,
    -- | Indicates whether the instance is optimized for Amazon EBS I\/O. This
    -- optimization provides dedicated throughput to Amazon EBS and an
    -- optimized configuration stack to provide optimal I\/O performance. This
    -- optimization isn\'t available with all instance types. Additional usage
    -- charges apply when using an EBS Optimized instance.
    ebsOptimized :: Prelude.Maybe Prelude.Bool,
    -- | The Elastic GPU associated with the instance.
    elasticGpuAssociations :: Prelude.Maybe [ElasticGpuAssociation],
    -- | The elastic inference accelerator associated with the instance.
    elasticInferenceAcceleratorAssociations :: Prelude.Maybe [ElasticInferenceAcceleratorAssociation],
    -- | Specifies whether enhanced networking with ENA is enabled.
    enaSupport :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether the instance is enabled for Amazon Web Services Nitro
    -- Enclaves.
    enclaveOptions :: Prelude.Maybe EnclaveOptions,
    -- | Indicates whether the instance is enabled for hibernation.
    hibernationOptions :: Prelude.Maybe HibernationOptions,
    -- | The IAM instance profile associated with the instance, if applicable.
    iamInstanceProfile :: Prelude.Maybe IamInstanceProfile,
    -- | Indicates whether this is a Spot Instance or a Scheduled Instance.
    instanceLifecycle :: Prelude.Maybe InstanceLifecycleType,
    -- | The IPv6 address assigned to the instance.
    ipv6Address :: Prelude.Maybe Prelude.Text,
    -- | The kernel associated with this instance, if applicable.
    kernelId :: Prelude.Maybe Prelude.Text,
    -- | The name of the key pair, if this instance was launched with an
    -- associated key pair.
    keyName :: Prelude.Maybe Prelude.Text,
    -- | The license configurations for the instance.
    licenses :: Prelude.Maybe [LicenseConfiguration],
    -- | Provides information on the recovery and maintenance options of your
    -- instance.
    maintenanceOptions :: Prelude.Maybe InstanceMaintenanceOptions,
    -- | The metadata options for the instance.
    metadataOptions :: Prelude.Maybe InstanceMetadataOptionsResponse,
    -- | The network interfaces for the instance.
    networkInterfaces :: Prelude.Maybe [InstanceNetworkInterface],
    -- | The Amazon Resource Name (ARN) of the Outpost.
    outpostArn :: Prelude.Maybe Prelude.Text,
    -- | The value is @Windows@ for Windows instances; otherwise blank.
    platform :: Prelude.Maybe PlatformValues,
    -- | The platform details value for the instance. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/billing-info-fields.html AMI billing information fields>
    -- in the /Amazon EC2 User Guide/.
    platformDetails :: Prelude.Maybe Prelude.Text,
    -- | [IPv4 only] The private DNS hostname name assigned to the instance. This
    -- DNS hostname can only be used inside the Amazon EC2 network. This name
    -- is not available until the instance enters the @running@ state.
    --
    -- The Amazon-provided DNS server resolves Amazon-provided private DNS
    -- hostnames if you\'ve enabled DNS resolution and DNS hostnames in your
    -- VPC. If you are not using the Amazon-provided DNS server in your VPC,
    -- your custom domain name servers must resolve the hostname as
    -- appropriate.
    privateDnsName :: Prelude.Maybe Prelude.Text,
    -- | The options for the instance hostname.
    privateDnsNameOptions :: Prelude.Maybe PrivateDnsNameOptionsResponse,
    -- | The private IPv4 address assigned to the instance.
    privateIpAddress :: Prelude.Maybe Prelude.Text,
    -- | The product codes attached to this instance, if applicable.
    productCodes :: Prelude.Maybe [ProductCode],
    -- | [IPv4 only] The public DNS name assigned to the instance. This name is
    -- not available until the instance enters the @running@ state. This name
    -- is only available if you\'ve enabled DNS hostnames for your VPC.
    publicDnsName :: Prelude.Maybe Prelude.Text,
    -- | The public IPv4 address, or the Carrier IP address assigned to the
    -- instance, if applicable.
    --
    -- A Carrier IP address only applies to an instance launched in a subnet
    -- associated with a Wavelength Zone.
    publicIpAddress :: Prelude.Maybe Prelude.Text,
    -- | The RAM disk associated with this instance, if applicable.
    ramdiskId :: Prelude.Maybe Prelude.Text,
    -- | The device name of the root device volume (for example, @\/dev\/sda1@).
    rootDeviceName :: Prelude.Maybe Prelude.Text,
    -- | The security groups for the instance.
    securityGroups :: Prelude.Maybe [GroupIdentifier],
    -- | Indicates whether source\/destination checking is enabled.
    sourceDestCheck :: Prelude.Maybe Prelude.Bool,
    -- | If the request is a Spot Instance request, the ID of the request.
    spotInstanceRequestId :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether enhanced networking with the Intel 82599 Virtual
    -- Function interface is enabled.
    sriovNetSupport :: Prelude.Maybe Prelude.Text,
    -- | The reason for the most recent state transition.
    stateReason :: Prelude.Maybe StateReason,
    -- | The reason for the most recent state transition. This might be an empty
    -- string.
    stateTransitionReason :: Prelude.Maybe Prelude.Text,
    -- | The ID of the subnet in which the instance is running.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | Any tags assigned to the instance.
    tags :: Prelude.Maybe [Tag],
    -- | If the instance is configured for NitroTPM support, the value is @v2.0@.
    -- For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/nitrotpm.html NitroTPM>
    -- in the /Amazon EC2 User Guide/.
    tpmSupport :: Prelude.Maybe Prelude.Text,
    -- | The usage operation value for the instance. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/billing-info-fields.html AMI billing information fields>
    -- in the /Amazon EC2 User Guide/.
    usageOperation :: Prelude.Maybe Prelude.Text,
    -- | The time that the usage operation was last updated.
    usageOperationUpdateTime :: Prelude.Maybe Data.ISO8601,
    -- | The ID of the VPC in which the instance is running.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the instance.
    instanceId :: Prelude.Text,
    -- | The ID of the AMI used to launch the instance.
    imageId :: Prelude.Text,
    -- | The AMI launch index, which can be used to find this instance in the
    -- launch group.
    amiLaunchIndex :: Prelude.Int,
    -- | The instance type.
    instanceType :: InstanceType,
    -- | The time the instance was launched.
    launchTime :: Data.ISO8601,
    -- | The location where the instance launched, if applicable.
    placement :: Placement,
    -- | The monitoring for the instance.
    monitoring :: Monitoring,
    -- | The architecture of the image.
    architecture :: ArchitectureValues,
    -- | The root device type used by the AMI. The AMI can use an EBS volume or
    -- an instance store volume.
    rootDeviceType :: DeviceType,
    -- | The virtualization type of the instance.
    virtualizationType :: VirtualizationType,
    -- | The hypervisor type of the instance. The value @xen@ is used for both
    -- Xen and Nitro hypervisors.
    hypervisor :: HypervisorType,
    -- | The current state of the instance.
    state :: InstanceState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Instance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blockDeviceMappings', 'instance_blockDeviceMappings' - Any block device mapping entries for the instance.
--
-- 'bootMode', 'instance_bootMode' - The boot mode that was specified by the AMI. If the value is
-- @uefi-preferred@, the AMI supports both UEFI and Legacy BIOS. The
-- @currentInstanceBootMode@ parameter is the boot mode that is used to
-- boot the instance at launch or start.
--
-- The operating system contained in the AMI must be configured to support
-- the specified boot mode.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-boot.html Boot modes>
-- in the /Amazon EC2 User Guide/.
--
-- 'capacityReservationId', 'instance_capacityReservationId' - The ID of the Capacity Reservation.
--
-- 'capacityReservationSpecification', 'instance_capacityReservationSpecification' - Information about the Capacity Reservation targeting option.
--
-- 'clientToken', 'instance_clientToken' - The idempotency token you provided when you launched the instance, if
-- applicable.
--
-- 'cpuOptions', 'instance_cpuOptions' - The CPU options for the instance.
--
-- 'currentInstanceBootMode', 'instance_currentInstanceBootMode' - The boot mode that is used to boot the instance at launch or start. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-boot.html Boot modes>
-- in the /Amazon EC2 User Guide/.
--
-- 'ebsOptimized', 'instance_ebsOptimized' - Indicates whether the instance is optimized for Amazon EBS I\/O. This
-- optimization provides dedicated throughput to Amazon EBS and an
-- optimized configuration stack to provide optimal I\/O performance. This
-- optimization isn\'t available with all instance types. Additional usage
-- charges apply when using an EBS Optimized instance.
--
-- 'elasticGpuAssociations', 'instance_elasticGpuAssociations' - The Elastic GPU associated with the instance.
--
-- 'elasticInferenceAcceleratorAssociations', 'instance_elasticInferenceAcceleratorAssociations' - The elastic inference accelerator associated with the instance.
--
-- 'enaSupport', 'instance_enaSupport' - Specifies whether enhanced networking with ENA is enabled.
--
-- 'enclaveOptions', 'instance_enclaveOptions' - Indicates whether the instance is enabled for Amazon Web Services Nitro
-- Enclaves.
--
-- 'hibernationOptions', 'instance_hibernationOptions' - Indicates whether the instance is enabled for hibernation.
--
-- 'iamInstanceProfile', 'instance_iamInstanceProfile' - The IAM instance profile associated with the instance, if applicable.
--
-- 'instanceLifecycle', 'instance_instanceLifecycle' - Indicates whether this is a Spot Instance or a Scheduled Instance.
--
-- 'ipv6Address', 'instance_ipv6Address' - The IPv6 address assigned to the instance.
--
-- 'kernelId', 'instance_kernelId' - The kernel associated with this instance, if applicable.
--
-- 'keyName', 'instance_keyName' - The name of the key pair, if this instance was launched with an
-- associated key pair.
--
-- 'licenses', 'instance_licenses' - The license configurations for the instance.
--
-- 'maintenanceOptions', 'instance_maintenanceOptions' - Provides information on the recovery and maintenance options of your
-- instance.
--
-- 'metadataOptions', 'instance_metadataOptions' - The metadata options for the instance.
--
-- 'networkInterfaces', 'instance_networkInterfaces' - The network interfaces for the instance.
--
-- 'outpostArn', 'instance_outpostArn' - The Amazon Resource Name (ARN) of the Outpost.
--
-- 'platform', 'instance_platform' - The value is @Windows@ for Windows instances; otherwise blank.
--
-- 'platformDetails', 'instance_platformDetails' - The platform details value for the instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/billing-info-fields.html AMI billing information fields>
-- in the /Amazon EC2 User Guide/.
--
-- 'privateDnsName', 'instance_privateDnsName' - [IPv4 only] The private DNS hostname name assigned to the instance. This
-- DNS hostname can only be used inside the Amazon EC2 network. This name
-- is not available until the instance enters the @running@ state.
--
-- The Amazon-provided DNS server resolves Amazon-provided private DNS
-- hostnames if you\'ve enabled DNS resolution and DNS hostnames in your
-- VPC. If you are not using the Amazon-provided DNS server in your VPC,
-- your custom domain name servers must resolve the hostname as
-- appropriate.
--
-- 'privateDnsNameOptions', 'instance_privateDnsNameOptions' - The options for the instance hostname.
--
-- 'privateIpAddress', 'instance_privateIpAddress' - The private IPv4 address assigned to the instance.
--
-- 'productCodes', 'instance_productCodes' - The product codes attached to this instance, if applicable.
--
-- 'publicDnsName', 'instance_publicDnsName' - [IPv4 only] The public DNS name assigned to the instance. This name is
-- not available until the instance enters the @running@ state. This name
-- is only available if you\'ve enabled DNS hostnames for your VPC.
--
-- 'publicIpAddress', 'instance_publicIpAddress' - The public IPv4 address, or the Carrier IP address assigned to the
-- instance, if applicable.
--
-- A Carrier IP address only applies to an instance launched in a subnet
-- associated with a Wavelength Zone.
--
-- 'ramdiskId', 'instance_ramdiskId' - The RAM disk associated with this instance, if applicable.
--
-- 'rootDeviceName', 'instance_rootDeviceName' - The device name of the root device volume (for example, @\/dev\/sda1@).
--
-- 'securityGroups', 'instance_securityGroups' - The security groups for the instance.
--
-- 'sourceDestCheck', 'instance_sourceDestCheck' - Indicates whether source\/destination checking is enabled.
--
-- 'spotInstanceRequestId', 'instance_spotInstanceRequestId' - If the request is a Spot Instance request, the ID of the request.
--
-- 'sriovNetSupport', 'instance_sriovNetSupport' - Specifies whether enhanced networking with the Intel 82599 Virtual
-- Function interface is enabled.
--
-- 'stateReason', 'instance_stateReason' - The reason for the most recent state transition.
--
-- 'stateTransitionReason', 'instance_stateTransitionReason' - The reason for the most recent state transition. This might be an empty
-- string.
--
-- 'subnetId', 'instance_subnetId' - The ID of the subnet in which the instance is running.
--
-- 'tags', 'instance_tags' - Any tags assigned to the instance.
--
-- 'tpmSupport', 'instance_tpmSupport' - If the instance is configured for NitroTPM support, the value is @v2.0@.
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/nitrotpm.html NitroTPM>
-- in the /Amazon EC2 User Guide/.
--
-- 'usageOperation', 'instance_usageOperation' - The usage operation value for the instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/billing-info-fields.html AMI billing information fields>
-- in the /Amazon EC2 User Guide/.
--
-- 'usageOperationUpdateTime', 'instance_usageOperationUpdateTime' - The time that the usage operation was last updated.
--
-- 'vpcId', 'instance_vpcId' - The ID of the VPC in which the instance is running.
--
-- 'instanceId', 'instance_instanceId' - The ID of the instance.
--
-- 'imageId', 'instance_imageId' - The ID of the AMI used to launch the instance.
--
-- 'amiLaunchIndex', 'instance_amiLaunchIndex' - The AMI launch index, which can be used to find this instance in the
-- launch group.
--
-- 'instanceType', 'instance_instanceType' - The instance type.
--
-- 'launchTime', 'instance_launchTime' - The time the instance was launched.
--
-- 'placement', 'instance_placement' - The location where the instance launched, if applicable.
--
-- 'monitoring', 'instance_monitoring' - The monitoring for the instance.
--
-- 'architecture', 'instance_architecture' - The architecture of the image.
--
-- 'rootDeviceType', 'instance_rootDeviceType' - The root device type used by the AMI. The AMI can use an EBS volume or
-- an instance store volume.
--
-- 'virtualizationType', 'instance_virtualizationType' - The virtualization type of the instance.
--
-- 'hypervisor', 'instance_hypervisor' - The hypervisor type of the instance. The value @xen@ is used for both
-- Xen and Nitro hypervisors.
--
-- 'state', 'instance_state' - The current state of the instance.
newInstance ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'imageId'
  Prelude.Text ->
  -- | 'amiLaunchIndex'
  Prelude.Int ->
  -- | 'instanceType'
  InstanceType ->
  -- | 'launchTime'
  Prelude.UTCTime ->
  -- | 'placement'
  Placement ->
  -- | 'monitoring'
  Monitoring ->
  -- | 'architecture'
  ArchitectureValues ->
  -- | 'rootDeviceType'
  DeviceType ->
  -- | 'virtualizationType'
  VirtualizationType ->
  -- | 'hypervisor'
  HypervisorType ->
  -- | 'state'
  InstanceState ->
  Instance
newInstance
  pInstanceId_
  pImageId_
  pAmiLaunchIndex_
  pInstanceType_
  pLaunchTime_
  pPlacement_
  pMonitoring_
  pArchitecture_
  pRootDeviceType_
  pVirtualizationType_
  pHypervisor_
  pState_ =
    Instance'
      { blockDeviceMappings = Prelude.Nothing,
        bootMode = Prelude.Nothing,
        capacityReservationId = Prelude.Nothing,
        capacityReservationSpecification = Prelude.Nothing,
        clientToken = Prelude.Nothing,
        cpuOptions = Prelude.Nothing,
        currentInstanceBootMode = Prelude.Nothing,
        ebsOptimized = Prelude.Nothing,
        elasticGpuAssociations = Prelude.Nothing,
        elasticInferenceAcceleratorAssociations =
          Prelude.Nothing,
        enaSupport = Prelude.Nothing,
        enclaveOptions = Prelude.Nothing,
        hibernationOptions = Prelude.Nothing,
        iamInstanceProfile = Prelude.Nothing,
        instanceLifecycle = Prelude.Nothing,
        ipv6Address = Prelude.Nothing,
        kernelId = Prelude.Nothing,
        keyName = Prelude.Nothing,
        licenses = Prelude.Nothing,
        maintenanceOptions = Prelude.Nothing,
        metadataOptions = Prelude.Nothing,
        networkInterfaces = Prelude.Nothing,
        outpostArn = Prelude.Nothing,
        platform = Prelude.Nothing,
        platformDetails = Prelude.Nothing,
        privateDnsName = Prelude.Nothing,
        privateDnsNameOptions = Prelude.Nothing,
        privateIpAddress = Prelude.Nothing,
        productCodes = Prelude.Nothing,
        publicDnsName = Prelude.Nothing,
        publicIpAddress = Prelude.Nothing,
        ramdiskId = Prelude.Nothing,
        rootDeviceName = Prelude.Nothing,
        securityGroups = Prelude.Nothing,
        sourceDestCheck = Prelude.Nothing,
        spotInstanceRequestId = Prelude.Nothing,
        sriovNetSupport = Prelude.Nothing,
        stateReason = Prelude.Nothing,
        stateTransitionReason = Prelude.Nothing,
        subnetId = Prelude.Nothing,
        tags = Prelude.Nothing,
        tpmSupport = Prelude.Nothing,
        usageOperation = Prelude.Nothing,
        usageOperationUpdateTime = Prelude.Nothing,
        vpcId = Prelude.Nothing,
        instanceId = pInstanceId_,
        imageId = pImageId_,
        amiLaunchIndex = pAmiLaunchIndex_,
        instanceType = pInstanceType_,
        launchTime = Data._Time Lens.# pLaunchTime_,
        placement = pPlacement_,
        monitoring = pMonitoring_,
        architecture = pArchitecture_,
        rootDeviceType = pRootDeviceType_,
        virtualizationType = pVirtualizationType_,
        hypervisor = pHypervisor_,
        state = pState_
      }

-- | Any block device mapping entries for the instance.
instance_blockDeviceMappings :: Lens.Lens' Instance (Prelude.Maybe [InstanceBlockDeviceMapping])
instance_blockDeviceMappings = Lens.lens (\Instance' {blockDeviceMappings} -> blockDeviceMappings) (\s@Instance' {} a -> s {blockDeviceMappings = a} :: Instance) Prelude.. Lens.mapping Lens.coerced

-- | The boot mode that was specified by the AMI. If the value is
-- @uefi-preferred@, the AMI supports both UEFI and Legacy BIOS. The
-- @currentInstanceBootMode@ parameter is the boot mode that is used to
-- boot the instance at launch or start.
--
-- The operating system contained in the AMI must be configured to support
-- the specified boot mode.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-boot.html Boot modes>
-- in the /Amazon EC2 User Guide/.
instance_bootMode :: Lens.Lens' Instance (Prelude.Maybe BootModeValues)
instance_bootMode = Lens.lens (\Instance' {bootMode} -> bootMode) (\s@Instance' {} a -> s {bootMode = a} :: Instance)

-- | The ID of the Capacity Reservation.
instance_capacityReservationId :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_capacityReservationId = Lens.lens (\Instance' {capacityReservationId} -> capacityReservationId) (\s@Instance' {} a -> s {capacityReservationId = a} :: Instance)

-- | Information about the Capacity Reservation targeting option.
instance_capacityReservationSpecification :: Lens.Lens' Instance (Prelude.Maybe CapacityReservationSpecificationResponse)
instance_capacityReservationSpecification = Lens.lens (\Instance' {capacityReservationSpecification} -> capacityReservationSpecification) (\s@Instance' {} a -> s {capacityReservationSpecification = a} :: Instance)

-- | The idempotency token you provided when you launched the instance, if
-- applicable.
instance_clientToken :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_clientToken = Lens.lens (\Instance' {clientToken} -> clientToken) (\s@Instance' {} a -> s {clientToken = a} :: Instance)

-- | The CPU options for the instance.
instance_cpuOptions :: Lens.Lens' Instance (Prelude.Maybe CpuOptions)
instance_cpuOptions = Lens.lens (\Instance' {cpuOptions} -> cpuOptions) (\s@Instance' {} a -> s {cpuOptions = a} :: Instance)

-- | The boot mode that is used to boot the instance at launch or start. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-boot.html Boot modes>
-- in the /Amazon EC2 User Guide/.
instance_currentInstanceBootMode :: Lens.Lens' Instance (Prelude.Maybe InstanceBootModeValues)
instance_currentInstanceBootMode = Lens.lens (\Instance' {currentInstanceBootMode} -> currentInstanceBootMode) (\s@Instance' {} a -> s {currentInstanceBootMode = a} :: Instance)

-- | Indicates whether the instance is optimized for Amazon EBS I\/O. This
-- optimization provides dedicated throughput to Amazon EBS and an
-- optimized configuration stack to provide optimal I\/O performance. This
-- optimization isn\'t available with all instance types. Additional usage
-- charges apply when using an EBS Optimized instance.
instance_ebsOptimized :: Lens.Lens' Instance (Prelude.Maybe Prelude.Bool)
instance_ebsOptimized = Lens.lens (\Instance' {ebsOptimized} -> ebsOptimized) (\s@Instance' {} a -> s {ebsOptimized = a} :: Instance)

-- | The Elastic GPU associated with the instance.
instance_elasticGpuAssociations :: Lens.Lens' Instance (Prelude.Maybe [ElasticGpuAssociation])
instance_elasticGpuAssociations = Lens.lens (\Instance' {elasticGpuAssociations} -> elasticGpuAssociations) (\s@Instance' {} a -> s {elasticGpuAssociations = a} :: Instance) Prelude.. Lens.mapping Lens.coerced

-- | The elastic inference accelerator associated with the instance.
instance_elasticInferenceAcceleratorAssociations :: Lens.Lens' Instance (Prelude.Maybe [ElasticInferenceAcceleratorAssociation])
instance_elasticInferenceAcceleratorAssociations = Lens.lens (\Instance' {elasticInferenceAcceleratorAssociations} -> elasticInferenceAcceleratorAssociations) (\s@Instance' {} a -> s {elasticInferenceAcceleratorAssociations = a} :: Instance) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether enhanced networking with ENA is enabled.
instance_enaSupport :: Lens.Lens' Instance (Prelude.Maybe Prelude.Bool)
instance_enaSupport = Lens.lens (\Instance' {enaSupport} -> enaSupport) (\s@Instance' {} a -> s {enaSupport = a} :: Instance)

-- | Indicates whether the instance is enabled for Amazon Web Services Nitro
-- Enclaves.
instance_enclaveOptions :: Lens.Lens' Instance (Prelude.Maybe EnclaveOptions)
instance_enclaveOptions = Lens.lens (\Instance' {enclaveOptions} -> enclaveOptions) (\s@Instance' {} a -> s {enclaveOptions = a} :: Instance)

-- | Indicates whether the instance is enabled for hibernation.
instance_hibernationOptions :: Lens.Lens' Instance (Prelude.Maybe HibernationOptions)
instance_hibernationOptions = Lens.lens (\Instance' {hibernationOptions} -> hibernationOptions) (\s@Instance' {} a -> s {hibernationOptions = a} :: Instance)

-- | The IAM instance profile associated with the instance, if applicable.
instance_iamInstanceProfile :: Lens.Lens' Instance (Prelude.Maybe IamInstanceProfile)
instance_iamInstanceProfile = Lens.lens (\Instance' {iamInstanceProfile} -> iamInstanceProfile) (\s@Instance' {} a -> s {iamInstanceProfile = a} :: Instance)

-- | Indicates whether this is a Spot Instance or a Scheduled Instance.
instance_instanceLifecycle :: Lens.Lens' Instance (Prelude.Maybe InstanceLifecycleType)
instance_instanceLifecycle = Lens.lens (\Instance' {instanceLifecycle} -> instanceLifecycle) (\s@Instance' {} a -> s {instanceLifecycle = a} :: Instance)

-- | The IPv6 address assigned to the instance.
instance_ipv6Address :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_ipv6Address = Lens.lens (\Instance' {ipv6Address} -> ipv6Address) (\s@Instance' {} a -> s {ipv6Address = a} :: Instance)

-- | The kernel associated with this instance, if applicable.
instance_kernelId :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_kernelId = Lens.lens (\Instance' {kernelId} -> kernelId) (\s@Instance' {} a -> s {kernelId = a} :: Instance)

-- | The name of the key pair, if this instance was launched with an
-- associated key pair.
instance_keyName :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_keyName = Lens.lens (\Instance' {keyName} -> keyName) (\s@Instance' {} a -> s {keyName = a} :: Instance)

-- | The license configurations for the instance.
instance_licenses :: Lens.Lens' Instance (Prelude.Maybe [LicenseConfiguration])
instance_licenses = Lens.lens (\Instance' {licenses} -> licenses) (\s@Instance' {} a -> s {licenses = a} :: Instance) Prelude.. Lens.mapping Lens.coerced

-- | Provides information on the recovery and maintenance options of your
-- instance.
instance_maintenanceOptions :: Lens.Lens' Instance (Prelude.Maybe InstanceMaintenanceOptions)
instance_maintenanceOptions = Lens.lens (\Instance' {maintenanceOptions} -> maintenanceOptions) (\s@Instance' {} a -> s {maintenanceOptions = a} :: Instance)

-- | The metadata options for the instance.
instance_metadataOptions :: Lens.Lens' Instance (Prelude.Maybe InstanceMetadataOptionsResponse)
instance_metadataOptions = Lens.lens (\Instance' {metadataOptions} -> metadataOptions) (\s@Instance' {} a -> s {metadataOptions = a} :: Instance)

-- | The network interfaces for the instance.
instance_networkInterfaces :: Lens.Lens' Instance (Prelude.Maybe [InstanceNetworkInterface])
instance_networkInterfaces = Lens.lens (\Instance' {networkInterfaces} -> networkInterfaces) (\s@Instance' {} a -> s {networkInterfaces = a} :: Instance) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the Outpost.
instance_outpostArn :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_outpostArn = Lens.lens (\Instance' {outpostArn} -> outpostArn) (\s@Instance' {} a -> s {outpostArn = a} :: Instance)

-- | The value is @Windows@ for Windows instances; otherwise blank.
instance_platform :: Lens.Lens' Instance (Prelude.Maybe PlatformValues)
instance_platform = Lens.lens (\Instance' {platform} -> platform) (\s@Instance' {} a -> s {platform = a} :: Instance)

-- | The platform details value for the instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/billing-info-fields.html AMI billing information fields>
-- in the /Amazon EC2 User Guide/.
instance_platformDetails :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_platformDetails = Lens.lens (\Instance' {platformDetails} -> platformDetails) (\s@Instance' {} a -> s {platformDetails = a} :: Instance)

-- | [IPv4 only] The private DNS hostname name assigned to the instance. This
-- DNS hostname can only be used inside the Amazon EC2 network. This name
-- is not available until the instance enters the @running@ state.
--
-- The Amazon-provided DNS server resolves Amazon-provided private DNS
-- hostnames if you\'ve enabled DNS resolution and DNS hostnames in your
-- VPC. If you are not using the Amazon-provided DNS server in your VPC,
-- your custom domain name servers must resolve the hostname as
-- appropriate.
instance_privateDnsName :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_privateDnsName = Lens.lens (\Instance' {privateDnsName} -> privateDnsName) (\s@Instance' {} a -> s {privateDnsName = a} :: Instance)

-- | The options for the instance hostname.
instance_privateDnsNameOptions :: Lens.Lens' Instance (Prelude.Maybe PrivateDnsNameOptionsResponse)
instance_privateDnsNameOptions = Lens.lens (\Instance' {privateDnsNameOptions} -> privateDnsNameOptions) (\s@Instance' {} a -> s {privateDnsNameOptions = a} :: Instance)

-- | The private IPv4 address assigned to the instance.
instance_privateIpAddress :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_privateIpAddress = Lens.lens (\Instance' {privateIpAddress} -> privateIpAddress) (\s@Instance' {} a -> s {privateIpAddress = a} :: Instance)

-- | The product codes attached to this instance, if applicable.
instance_productCodes :: Lens.Lens' Instance (Prelude.Maybe [ProductCode])
instance_productCodes = Lens.lens (\Instance' {productCodes} -> productCodes) (\s@Instance' {} a -> s {productCodes = a} :: Instance) Prelude.. Lens.mapping Lens.coerced

-- | [IPv4 only] The public DNS name assigned to the instance. This name is
-- not available until the instance enters the @running@ state. This name
-- is only available if you\'ve enabled DNS hostnames for your VPC.
instance_publicDnsName :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_publicDnsName = Lens.lens (\Instance' {publicDnsName} -> publicDnsName) (\s@Instance' {} a -> s {publicDnsName = a} :: Instance)

-- | The public IPv4 address, or the Carrier IP address assigned to the
-- instance, if applicable.
--
-- A Carrier IP address only applies to an instance launched in a subnet
-- associated with a Wavelength Zone.
instance_publicIpAddress :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_publicIpAddress = Lens.lens (\Instance' {publicIpAddress} -> publicIpAddress) (\s@Instance' {} a -> s {publicIpAddress = a} :: Instance)

-- | The RAM disk associated with this instance, if applicable.
instance_ramdiskId :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_ramdiskId = Lens.lens (\Instance' {ramdiskId} -> ramdiskId) (\s@Instance' {} a -> s {ramdiskId = a} :: Instance)

-- | The device name of the root device volume (for example, @\/dev\/sda1@).
instance_rootDeviceName :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_rootDeviceName = Lens.lens (\Instance' {rootDeviceName} -> rootDeviceName) (\s@Instance' {} a -> s {rootDeviceName = a} :: Instance)

-- | The security groups for the instance.
instance_securityGroups :: Lens.Lens' Instance (Prelude.Maybe [GroupIdentifier])
instance_securityGroups = Lens.lens (\Instance' {securityGroups} -> securityGroups) (\s@Instance' {} a -> s {securityGroups = a} :: Instance) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether source\/destination checking is enabled.
instance_sourceDestCheck :: Lens.Lens' Instance (Prelude.Maybe Prelude.Bool)
instance_sourceDestCheck = Lens.lens (\Instance' {sourceDestCheck} -> sourceDestCheck) (\s@Instance' {} a -> s {sourceDestCheck = a} :: Instance)

-- | If the request is a Spot Instance request, the ID of the request.
instance_spotInstanceRequestId :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_spotInstanceRequestId = Lens.lens (\Instance' {spotInstanceRequestId} -> spotInstanceRequestId) (\s@Instance' {} a -> s {spotInstanceRequestId = a} :: Instance)

-- | Specifies whether enhanced networking with the Intel 82599 Virtual
-- Function interface is enabled.
instance_sriovNetSupport :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_sriovNetSupport = Lens.lens (\Instance' {sriovNetSupport} -> sriovNetSupport) (\s@Instance' {} a -> s {sriovNetSupport = a} :: Instance)

-- | The reason for the most recent state transition.
instance_stateReason :: Lens.Lens' Instance (Prelude.Maybe StateReason)
instance_stateReason = Lens.lens (\Instance' {stateReason} -> stateReason) (\s@Instance' {} a -> s {stateReason = a} :: Instance)

-- | The reason for the most recent state transition. This might be an empty
-- string.
instance_stateTransitionReason :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_stateTransitionReason = Lens.lens (\Instance' {stateTransitionReason} -> stateTransitionReason) (\s@Instance' {} a -> s {stateTransitionReason = a} :: Instance)

-- | The ID of the subnet in which the instance is running.
instance_subnetId :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_subnetId = Lens.lens (\Instance' {subnetId} -> subnetId) (\s@Instance' {} a -> s {subnetId = a} :: Instance)

-- | Any tags assigned to the instance.
instance_tags :: Lens.Lens' Instance (Prelude.Maybe [Tag])
instance_tags = Lens.lens (\Instance' {tags} -> tags) (\s@Instance' {} a -> s {tags = a} :: Instance) Prelude.. Lens.mapping Lens.coerced

-- | If the instance is configured for NitroTPM support, the value is @v2.0@.
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/nitrotpm.html NitroTPM>
-- in the /Amazon EC2 User Guide/.
instance_tpmSupport :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_tpmSupport = Lens.lens (\Instance' {tpmSupport} -> tpmSupport) (\s@Instance' {} a -> s {tpmSupport = a} :: Instance)

-- | The usage operation value for the instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/billing-info-fields.html AMI billing information fields>
-- in the /Amazon EC2 User Guide/.
instance_usageOperation :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_usageOperation = Lens.lens (\Instance' {usageOperation} -> usageOperation) (\s@Instance' {} a -> s {usageOperation = a} :: Instance)

-- | The time that the usage operation was last updated.
instance_usageOperationUpdateTime :: Lens.Lens' Instance (Prelude.Maybe Prelude.UTCTime)
instance_usageOperationUpdateTime = Lens.lens (\Instance' {usageOperationUpdateTime} -> usageOperationUpdateTime) (\s@Instance' {} a -> s {usageOperationUpdateTime = a} :: Instance) Prelude.. Lens.mapping Data._Time

-- | The ID of the VPC in which the instance is running.
instance_vpcId :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_vpcId = Lens.lens (\Instance' {vpcId} -> vpcId) (\s@Instance' {} a -> s {vpcId = a} :: Instance)

-- | The ID of the instance.
instance_instanceId :: Lens.Lens' Instance Prelude.Text
instance_instanceId = Lens.lens (\Instance' {instanceId} -> instanceId) (\s@Instance' {} a -> s {instanceId = a} :: Instance)

-- | The ID of the AMI used to launch the instance.
instance_imageId :: Lens.Lens' Instance Prelude.Text
instance_imageId = Lens.lens (\Instance' {imageId} -> imageId) (\s@Instance' {} a -> s {imageId = a} :: Instance)

-- | The AMI launch index, which can be used to find this instance in the
-- launch group.
instance_amiLaunchIndex :: Lens.Lens' Instance Prelude.Int
instance_amiLaunchIndex = Lens.lens (\Instance' {amiLaunchIndex} -> amiLaunchIndex) (\s@Instance' {} a -> s {amiLaunchIndex = a} :: Instance)

-- | The instance type.
instance_instanceType :: Lens.Lens' Instance InstanceType
instance_instanceType = Lens.lens (\Instance' {instanceType} -> instanceType) (\s@Instance' {} a -> s {instanceType = a} :: Instance)

-- | The time the instance was launched.
instance_launchTime :: Lens.Lens' Instance Prelude.UTCTime
instance_launchTime = Lens.lens (\Instance' {launchTime} -> launchTime) (\s@Instance' {} a -> s {launchTime = a} :: Instance) Prelude.. Data._Time

-- | The location where the instance launched, if applicable.
instance_placement :: Lens.Lens' Instance Placement
instance_placement = Lens.lens (\Instance' {placement} -> placement) (\s@Instance' {} a -> s {placement = a} :: Instance)

-- | The monitoring for the instance.
instance_monitoring :: Lens.Lens' Instance Monitoring
instance_monitoring = Lens.lens (\Instance' {monitoring} -> monitoring) (\s@Instance' {} a -> s {monitoring = a} :: Instance)

-- | The architecture of the image.
instance_architecture :: Lens.Lens' Instance ArchitectureValues
instance_architecture = Lens.lens (\Instance' {architecture} -> architecture) (\s@Instance' {} a -> s {architecture = a} :: Instance)

-- | The root device type used by the AMI. The AMI can use an EBS volume or
-- an instance store volume.
instance_rootDeviceType :: Lens.Lens' Instance DeviceType
instance_rootDeviceType = Lens.lens (\Instance' {rootDeviceType} -> rootDeviceType) (\s@Instance' {} a -> s {rootDeviceType = a} :: Instance)

-- | The virtualization type of the instance.
instance_virtualizationType :: Lens.Lens' Instance VirtualizationType
instance_virtualizationType = Lens.lens (\Instance' {virtualizationType} -> virtualizationType) (\s@Instance' {} a -> s {virtualizationType = a} :: Instance)

-- | The hypervisor type of the instance. The value @xen@ is used for both
-- Xen and Nitro hypervisors.
instance_hypervisor :: Lens.Lens' Instance HypervisorType
instance_hypervisor = Lens.lens (\Instance' {hypervisor} -> hypervisor) (\s@Instance' {} a -> s {hypervisor = a} :: Instance)

-- | The current state of the instance.
instance_state :: Lens.Lens' Instance InstanceState
instance_state = Lens.lens (\Instance' {state} -> state) (\s@Instance' {} a -> s {state = a} :: Instance)

instance Data.FromXML Instance where
  parseXML x =
    Instance'
      Prelude.<$> ( x
                      Data..@? "blockDeviceMapping"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "bootMode")
      Prelude.<*> (x Data..@? "capacityReservationId")
      Prelude.<*> (x Data..@? "capacityReservationSpecification")
      Prelude.<*> (x Data..@? "clientToken")
      Prelude.<*> (x Data..@? "cpuOptions")
      Prelude.<*> (x Data..@? "currentInstanceBootMode")
      Prelude.<*> (x Data..@? "ebsOptimized")
      Prelude.<*> ( x
                      Data..@? "elasticGpuAssociationSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x
                      Data..@? "elasticInferenceAcceleratorAssociationSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "enaSupport")
      Prelude.<*> (x Data..@? "enclaveOptions")
      Prelude.<*> (x Data..@? "hibernationOptions")
      Prelude.<*> (x Data..@? "iamInstanceProfile")
      Prelude.<*> (x Data..@? "instanceLifecycle")
      Prelude.<*> (x Data..@? "ipv6Address")
      Prelude.<*> (x Data..@? "kernelId")
      Prelude.<*> (x Data..@? "keyName")
      Prelude.<*> ( x
                      Data..@? "licenseSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "maintenanceOptions")
      Prelude.<*> (x Data..@? "metadataOptions")
      Prelude.<*> ( x
                      Data..@? "networkInterfaceSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "outpostArn")
      Prelude.<*> (x Data..@? "platform")
      Prelude.<*> (x Data..@? "platformDetails")
      Prelude.<*> (x Data..@? "privateDnsName")
      Prelude.<*> (x Data..@? "privateDnsNameOptions")
      Prelude.<*> (x Data..@? "privateIpAddress")
      Prelude.<*> ( x
                      Data..@? "productCodes"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "dnsName")
      Prelude.<*> (x Data..@? "ipAddress")
      Prelude.<*> (x Data..@? "ramdiskId")
      Prelude.<*> (x Data..@? "rootDeviceName")
      Prelude.<*> ( x
                      Data..@? "groupSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "sourceDestCheck")
      Prelude.<*> (x Data..@? "spotInstanceRequestId")
      Prelude.<*> (x Data..@? "sriovNetSupport")
      Prelude.<*> (x Data..@? "stateReason")
      Prelude.<*> (x Data..@? "reason")
      Prelude.<*> (x Data..@? "subnetId")
      Prelude.<*> ( x
                      Data..@? "tagSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "tpmSupport")
      Prelude.<*> (x Data..@? "usageOperation")
      Prelude.<*> (x Data..@? "usageOperationUpdateTime")
      Prelude.<*> (x Data..@? "vpcId")
      Prelude.<*> (x Data..@ "instanceId")
      Prelude.<*> (x Data..@ "imageId")
      Prelude.<*> (x Data..@ "amiLaunchIndex")
      Prelude.<*> (x Data..@ "instanceType")
      Prelude.<*> (x Data..@ "launchTime")
      Prelude.<*> (x Data..@ "placement")
      Prelude.<*> (x Data..@ "monitoring")
      Prelude.<*> (x Data..@ "architecture")
      Prelude.<*> (x Data..@ "rootDeviceType")
      Prelude.<*> (x Data..@ "virtualizationType")
      Prelude.<*> (x Data..@ "hypervisor")
      Prelude.<*> (x Data..@ "instanceState")

instance Prelude.Hashable Instance where
  hashWithSalt _salt Instance' {..} =
    _salt
      `Prelude.hashWithSalt` blockDeviceMappings
      `Prelude.hashWithSalt` bootMode
      `Prelude.hashWithSalt` capacityReservationId
      `Prelude.hashWithSalt` capacityReservationSpecification
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` cpuOptions
      `Prelude.hashWithSalt` currentInstanceBootMode
      `Prelude.hashWithSalt` ebsOptimized
      `Prelude.hashWithSalt` elasticGpuAssociations
      `Prelude.hashWithSalt` elasticInferenceAcceleratorAssociations
      `Prelude.hashWithSalt` enaSupport
      `Prelude.hashWithSalt` enclaveOptions
      `Prelude.hashWithSalt` hibernationOptions
      `Prelude.hashWithSalt` iamInstanceProfile
      `Prelude.hashWithSalt` instanceLifecycle
      `Prelude.hashWithSalt` ipv6Address
      `Prelude.hashWithSalt` kernelId
      `Prelude.hashWithSalt` keyName
      `Prelude.hashWithSalt` licenses
      `Prelude.hashWithSalt` maintenanceOptions
      `Prelude.hashWithSalt` metadataOptions
      `Prelude.hashWithSalt` networkInterfaces
      `Prelude.hashWithSalt` outpostArn
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` platformDetails
      `Prelude.hashWithSalt` privateDnsName
      `Prelude.hashWithSalt` privateDnsNameOptions
      `Prelude.hashWithSalt` privateIpAddress
      `Prelude.hashWithSalt` productCodes
      `Prelude.hashWithSalt` publicDnsName
      `Prelude.hashWithSalt` publicIpAddress
      `Prelude.hashWithSalt` ramdiskId
      `Prelude.hashWithSalt` rootDeviceName
      `Prelude.hashWithSalt` securityGroups
      `Prelude.hashWithSalt` sourceDestCheck
      `Prelude.hashWithSalt` spotInstanceRequestId
      `Prelude.hashWithSalt` sriovNetSupport
      `Prelude.hashWithSalt` stateReason
      `Prelude.hashWithSalt` stateTransitionReason
      `Prelude.hashWithSalt` subnetId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` tpmSupport
      `Prelude.hashWithSalt` usageOperation
      `Prelude.hashWithSalt` usageOperationUpdateTime
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` imageId
      `Prelude.hashWithSalt` amiLaunchIndex
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` launchTime
      `Prelude.hashWithSalt` placement
      `Prelude.hashWithSalt` monitoring
      `Prelude.hashWithSalt` architecture
      `Prelude.hashWithSalt` rootDeviceType
      `Prelude.hashWithSalt` virtualizationType
      `Prelude.hashWithSalt` hypervisor
      `Prelude.hashWithSalt` state

instance Prelude.NFData Instance where
  rnf Instance' {..} =
    Prelude.rnf blockDeviceMappings
      `Prelude.seq` Prelude.rnf bootMode
      `Prelude.seq` Prelude.rnf capacityReservationId
      `Prelude.seq` Prelude.rnf capacityReservationSpecification
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf cpuOptions
      `Prelude.seq` Prelude.rnf currentInstanceBootMode
      `Prelude.seq` Prelude.rnf ebsOptimized
      `Prelude.seq` Prelude.rnf elasticGpuAssociations
      `Prelude.seq` Prelude.rnf
        elasticInferenceAcceleratorAssociations
      `Prelude.seq` Prelude.rnf enaSupport
      `Prelude.seq` Prelude.rnf enclaveOptions
      `Prelude.seq` Prelude.rnf hibernationOptions
      `Prelude.seq` Prelude.rnf iamInstanceProfile
      `Prelude.seq` Prelude.rnf instanceLifecycle
      `Prelude.seq` Prelude.rnf ipv6Address
      `Prelude.seq` Prelude.rnf kernelId
      `Prelude.seq` Prelude.rnf keyName
      `Prelude.seq` Prelude.rnf licenses
      `Prelude.seq` Prelude.rnf maintenanceOptions
      `Prelude.seq` Prelude.rnf metadataOptions
      `Prelude.seq` Prelude.rnf
        networkInterfaces
      `Prelude.seq` Prelude.rnf outpostArn
      `Prelude.seq` Prelude.rnf platform
      `Prelude.seq` Prelude.rnf
        platformDetails
      `Prelude.seq` Prelude.rnf
        privateDnsName
      `Prelude.seq` Prelude.rnf
        privateDnsNameOptions
      `Prelude.seq` Prelude.rnf
        privateIpAddress
      `Prelude.seq` Prelude.rnf
        productCodes
      `Prelude.seq` Prelude.rnf
        publicDnsName
      `Prelude.seq` Prelude.rnf
        publicIpAddress
      `Prelude.seq` Prelude.rnf
        ramdiskId
      `Prelude.seq` Prelude.rnf
        rootDeviceName
      `Prelude.seq` Prelude.rnf
        securityGroups
      `Prelude.seq` Prelude.rnf
        sourceDestCheck
      `Prelude.seq` Prelude.rnf
        spotInstanceRequestId
      `Prelude.seq` Prelude.rnf
        sriovNetSupport
      `Prelude.seq` Prelude.rnf
        stateReason
      `Prelude.seq` Prelude.rnf
        stateTransitionReason
      `Prelude.seq` Prelude.rnf
        subnetId
      `Prelude.seq` Prelude.rnf
        tags
      `Prelude.seq` Prelude.rnf
        tpmSupport
      `Prelude.seq` Prelude.rnf
        usageOperation
      `Prelude.seq` Prelude.rnf
        usageOperationUpdateTime
      `Prelude.seq` Prelude.rnf
        vpcId
      `Prelude.seq` Prelude.rnf
        instanceId
      `Prelude.seq` Prelude.rnf
        imageId
      `Prelude.seq` Prelude.rnf
        amiLaunchIndex
      `Prelude.seq` Prelude.rnf
        instanceType
      `Prelude.seq` Prelude.rnf
        launchTime
      `Prelude.seq` Prelude.rnf
        placement
      `Prelude.seq` Prelude.rnf
        monitoring
      `Prelude.seq` Prelude.rnf
        architecture
      `Prelude.seq` Prelude.rnf
        rootDeviceType
      `Prelude.seq` Prelude.rnf
        virtualizationType
      `Prelude.seq` Prelude.rnf
        hypervisor
      `Prelude.seq` Prelude.rnf
        state
