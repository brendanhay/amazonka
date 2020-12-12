{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Instance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Instance
  ( Instance (..),

    -- * Smart constructor
    mkInstance,

    -- * Lenses
    insPublicDNSName,
    insPlatform,
    insSecurityGroups,
    insClientToken,
    insEnaSupport,
    insSourceDestCheck,
    insElasticGpuAssociations,
    insVPCId,
    insKeyName,
    insNetworkInterfaces,
    insOutpostARN,
    insEnclaveOptions,
    insRAMDiskId,
    insCPUOptions,
    insSubnetId,
    insKernelId,
    insRootDeviceName,
    insCapacityReservationId,
    insCapacityReservationSpecification,
    insSRIOVNetSupport,
    insEBSOptimized,
    insStateTransitionReason,
    insHibernationOptions,
    insInstanceLifecycle,
    insIAMInstanceProfile,
    insPrivateIPAddress,
    insMetadataOptions,
    insProductCodes,
    insSpotInstanceRequestId,
    insLicenses,
    insElasticInferenceAcceleratorAssociations,
    insPrivateDNSName,
    insStateReason,
    insBlockDeviceMappings,
    insPublicIPAddress,
    insTags,
    insInstanceId,
    insImageId,
    insAMILaunchIndex,
    insInstanceType,
    insLaunchTime,
    insPlacement,
    insMonitoring,
    insArchitecture,
    insRootDeviceType,
    insVirtualizationType,
    insHypervisor,
    insState,
  )
where

import Network.AWS.EC2.Types.ArchitectureValues
import Network.AWS.EC2.Types.CPUOptions
import Network.AWS.EC2.Types.CapacityReservationSpecificationResponse
import Network.AWS.EC2.Types.DeviceType
import Network.AWS.EC2.Types.ElasticGpuAssociation
import Network.AWS.EC2.Types.ElasticInferenceAcceleratorAssociation
import Network.AWS.EC2.Types.EnclaveOptions
import Network.AWS.EC2.Types.GroupIdentifier
import Network.AWS.EC2.Types.HibernationOptions
import Network.AWS.EC2.Types.HypervisorType
import Network.AWS.EC2.Types.IAMInstanceProfile
import Network.AWS.EC2.Types.InstanceBlockDeviceMapping
import Network.AWS.EC2.Types.InstanceLifecycleType
import Network.AWS.EC2.Types.InstanceMetadataOptionsResponse
import Network.AWS.EC2.Types.InstanceNetworkInterface
import Network.AWS.EC2.Types.InstanceState
import Network.AWS.EC2.Types.InstanceType
import Network.AWS.EC2.Types.LicenseConfiguration
import Network.AWS.EC2.Types.Monitoring
import Network.AWS.EC2.Types.Placement
import Network.AWS.EC2.Types.PlatformValues
import Network.AWS.EC2.Types.ProductCode
import Network.AWS.EC2.Types.StateReason
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.VirtualizationType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an instance.
--
-- /See:/ 'mkInstance' smart constructor.
data Instance = Instance'
  { publicDNSName :: Lude.Maybe Lude.Text,
    platform :: Lude.Maybe PlatformValues,
    securityGroups :: Lude.Maybe [GroupIdentifier],
    clientToken :: Lude.Maybe Lude.Text,
    enaSupport :: Lude.Maybe Lude.Bool,
    sourceDestCheck :: Lude.Maybe Lude.Bool,
    elasticGpuAssociations :: Lude.Maybe [ElasticGpuAssociation],
    vpcId :: Lude.Maybe Lude.Text,
    keyName :: Lude.Maybe Lude.Text,
    networkInterfaces :: Lude.Maybe [InstanceNetworkInterface],
    outpostARN :: Lude.Maybe Lude.Text,
    enclaveOptions :: Lude.Maybe EnclaveOptions,
    ramdiskId :: Lude.Maybe Lude.Text,
    cpuOptions :: Lude.Maybe CPUOptions,
    subnetId :: Lude.Maybe Lude.Text,
    kernelId :: Lude.Maybe Lude.Text,
    rootDeviceName :: Lude.Maybe Lude.Text,
    capacityReservationId :: Lude.Maybe Lude.Text,
    capacityReservationSpecification ::
      Lude.Maybe CapacityReservationSpecificationResponse,
    sriovNetSupport :: Lude.Maybe Lude.Text,
    ebsOptimized :: Lude.Maybe Lude.Bool,
    stateTransitionReason :: Lude.Maybe Lude.Text,
    hibernationOptions :: Lude.Maybe HibernationOptions,
    instanceLifecycle :: Lude.Maybe InstanceLifecycleType,
    iamInstanceProfile :: Lude.Maybe IAMInstanceProfile,
    privateIPAddress :: Lude.Maybe Lude.Text,
    metadataOptions :: Lude.Maybe InstanceMetadataOptionsResponse,
    productCodes :: Lude.Maybe [ProductCode],
    spotInstanceRequestId :: Lude.Maybe Lude.Text,
    licenses :: Lude.Maybe [LicenseConfiguration],
    elasticInferenceAcceleratorAssociations ::
      Lude.Maybe [ElasticInferenceAcceleratorAssociation],
    privateDNSName :: Lude.Maybe Lude.Text,
    stateReason :: Lude.Maybe StateReason,
    blockDeviceMappings :: Lude.Maybe [InstanceBlockDeviceMapping],
    publicIPAddress :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    instanceId :: Lude.Text,
    imageId :: Lude.Text,
    amiLaunchIndex :: Lude.Int,
    instanceType :: InstanceType,
    launchTime :: Lude.DateTime,
    placement :: Placement,
    monitoring :: Monitoring,
    architecture :: ArchitectureValues,
    rootDeviceType :: DeviceType,
    virtualizationType :: VirtualizationType,
    hypervisor :: HypervisorType,
    state :: InstanceState
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Instance' with the minimum fields required to make a request.
--
-- * 'amiLaunchIndex' - The AMI launch index, which can be used to find this instance in the launch group.
-- * 'architecture' - The architecture of the image.
-- * 'blockDeviceMappings' - Any block device mapping entries for the instance.
-- * 'capacityReservationId' - The ID of the Capacity Reservation.
-- * 'capacityReservationSpecification' - Information about the Capacity Reservation targeting option.
-- * 'clientToken' - The idempotency token you provided when you launched the instance, if applicable.
-- * 'cpuOptions' - The CPU options for the instance.
-- * 'ebsOptimized' - Indicates whether the instance is optimized for Amazon EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS Optimized instance.
-- * 'elasticGpuAssociations' - The Elastic GPU associated with the instance.
-- * 'elasticInferenceAcceleratorAssociations' - The elastic inference accelerator associated with the instance.
-- * 'enaSupport' - Specifies whether enhanced networking with ENA is enabled.
-- * 'enclaveOptions' - Indicates whether the instance is enabled for AWS Nitro Enclaves.
-- * 'hibernationOptions' - Indicates whether the instance is enabled for hibernation.
-- * 'hypervisor' - The hypervisor type of the instance. The value @xen@ is used for both Xen and Nitro hypervisors.
-- * 'iamInstanceProfile' - The IAM instance profile associated with the instance, if applicable.
-- * 'imageId' - The ID of the AMI used to launch the instance.
-- * 'instanceId' - The ID of the instance.
-- * 'instanceLifecycle' - Indicates whether this is a Spot Instance or a Scheduled Instance.
-- * 'instanceType' - The instance type.
-- * 'kernelId' - The kernel associated with this instance, if applicable.
-- * 'keyName' - The name of the key pair, if this instance was launched with an associated key pair.
-- * 'launchTime' - The time the instance was launched.
-- * 'licenses' - The license configurations.
-- * 'metadataOptions' - The metadata options for the instance.
-- * 'monitoring' - The monitoring for the instance.
-- * 'networkInterfaces' - [EC2-VPC] The network interfaces for the instance.
-- * 'outpostARN' - The Amazon Resource Name (ARN) of the Outpost.
-- * 'placement' - The location where the instance launched, if applicable.
-- * 'platform' - The value is @Windows@ for Windows instances; otherwise blank.
-- * 'privateDNSName' - (IPv4 only) The private DNS hostname name assigned to the instance. This DNS hostname can only be used inside the Amazon EC2 network. This name is not available until the instance enters the @running@ state.
--
-- [EC2-VPC] The Amazon-provided DNS server resolves Amazon-provided private DNS hostnames if you've enabled DNS resolution and DNS hostnames in your VPC. If you are not using the Amazon-provided DNS server in your VPC, your custom domain name servers must resolve the hostname as appropriate.
-- * 'privateIPAddress' - The private IPv4 address assigned to the instance.
-- * 'productCodes' - The product codes attached to this instance, if applicable.
-- * 'publicDNSName' - (IPv4 only) The public DNS name assigned to the instance. This name is not available until the instance enters the @running@ state. For EC2-VPC, this name is only available if you've enabled DNS hostnames for your VPC.
-- * 'publicIPAddress' - The public IPv4 address, or the Carrier IP address assigned to the instance, if applicable.
--
-- A Carrier IP address only applies to an instance launched in a subnet associated with a Wavelength Zone.
-- * 'ramdiskId' - The RAM disk associated with this instance, if applicable.
-- * 'rootDeviceName' - The device name of the root device volume (for example, @/dev/sda1@ ).
-- * 'rootDeviceType' - The root device type used by the AMI. The AMI can use an EBS volume or an instance store volume.
-- * 'securityGroups' - The security groups for the instance.
-- * 'sourceDestCheck' - Specifies whether to enable an instance launched in a VPC to perform NAT. This controls whether source/destination checking is enabled on the instance. A value of @true@ means that checking is enabled, and @false@ means that checking is disabled. The value must be @false@ for the instance to perform NAT. For more information, see <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_NAT_Instance.html NAT Instances> in the /Amazon Virtual Private Cloud User Guide/ .
-- * 'spotInstanceRequestId' - If the request is a Spot Instance request, the ID of the request.
-- * 'sriovNetSupport' - Specifies whether enhanced networking with the Intel 82599 Virtual Function interface is enabled.
-- * 'state' - The current state of the instance.
-- * 'stateReason' - The reason for the most recent state transition.
-- * 'stateTransitionReason' - The reason for the most recent state transition. This might be an empty string.
-- * 'subnetId' - [EC2-VPC] The ID of the subnet in which the instance is running.
-- * 'tags' - Any tags assigned to the instance.
-- * 'virtualizationType' - The virtualization type of the instance.
-- * 'vpcId' - [EC2-VPC] The ID of the VPC in which the instance is running.
mkInstance ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'imageId'
  Lude.Text ->
  -- | 'amiLaunchIndex'
  Lude.Int ->
  -- | 'instanceType'
  InstanceType ->
  -- | 'launchTime'
  Lude.DateTime ->
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
mkInstance
  pInstanceId_
  pImageId_
  pAMILaunchIndex_
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
      { publicDNSName = Lude.Nothing,
        platform = Lude.Nothing,
        securityGroups = Lude.Nothing,
        clientToken = Lude.Nothing,
        enaSupport = Lude.Nothing,
        sourceDestCheck = Lude.Nothing,
        elasticGpuAssociations = Lude.Nothing,
        vpcId = Lude.Nothing,
        keyName = Lude.Nothing,
        networkInterfaces = Lude.Nothing,
        outpostARN = Lude.Nothing,
        enclaveOptions = Lude.Nothing,
        ramdiskId = Lude.Nothing,
        cpuOptions = Lude.Nothing,
        subnetId = Lude.Nothing,
        kernelId = Lude.Nothing,
        rootDeviceName = Lude.Nothing,
        capacityReservationId = Lude.Nothing,
        capacityReservationSpecification = Lude.Nothing,
        sriovNetSupport = Lude.Nothing,
        ebsOptimized = Lude.Nothing,
        stateTransitionReason = Lude.Nothing,
        hibernationOptions = Lude.Nothing,
        instanceLifecycle = Lude.Nothing,
        iamInstanceProfile = Lude.Nothing,
        privateIPAddress = Lude.Nothing,
        metadataOptions = Lude.Nothing,
        productCodes = Lude.Nothing,
        spotInstanceRequestId = Lude.Nothing,
        licenses = Lude.Nothing,
        elasticInferenceAcceleratorAssociations = Lude.Nothing,
        privateDNSName = Lude.Nothing,
        stateReason = Lude.Nothing,
        blockDeviceMappings = Lude.Nothing,
        publicIPAddress = Lude.Nothing,
        tags = Lude.Nothing,
        instanceId = pInstanceId_,
        imageId = pImageId_,
        amiLaunchIndex = pAMILaunchIndex_,
        instanceType = pInstanceType_,
        launchTime = pLaunchTime_,
        placement = pPlacement_,
        monitoring = pMonitoring_,
        architecture = pArchitecture_,
        rootDeviceType = pRootDeviceType_,
        virtualizationType = pVirtualizationType_,
        hypervisor = pHypervisor_,
        state = pState_
      }

-- | (IPv4 only) The public DNS name assigned to the instance. This name is not available until the instance enters the @running@ state. For EC2-VPC, this name is only available if you've enabled DNS hostnames for your VPC.
--
-- /Note:/ Consider using 'publicDNSName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insPublicDNSName :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
insPublicDNSName = Lens.lens (publicDNSName :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {publicDNSName = a} :: Instance)
{-# DEPRECATED insPublicDNSName "Use generic-lens or generic-optics with 'publicDNSName' instead." #-}

-- | The value is @Windows@ for Windows instances; otherwise blank.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insPlatform :: Lens.Lens' Instance (Lude.Maybe PlatformValues)
insPlatform = Lens.lens (platform :: Instance -> Lude.Maybe PlatformValues) (\s a -> s {platform = a} :: Instance)
{-# DEPRECATED insPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | The security groups for the instance.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insSecurityGroups :: Lens.Lens' Instance (Lude.Maybe [GroupIdentifier])
insSecurityGroups = Lens.lens (securityGroups :: Instance -> Lude.Maybe [GroupIdentifier]) (\s a -> s {securityGroups = a} :: Instance)
{-# DEPRECATED insSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | The idempotency token you provided when you launched the instance, if applicable.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insClientToken :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
insClientToken = Lens.lens (clientToken :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: Instance)
{-# DEPRECATED insClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | Specifies whether enhanced networking with ENA is enabled.
--
-- /Note:/ Consider using 'enaSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insEnaSupport :: Lens.Lens' Instance (Lude.Maybe Lude.Bool)
insEnaSupport = Lens.lens (enaSupport :: Instance -> Lude.Maybe Lude.Bool) (\s a -> s {enaSupport = a} :: Instance)
{-# DEPRECATED insEnaSupport "Use generic-lens or generic-optics with 'enaSupport' instead." #-}

-- | Specifies whether to enable an instance launched in a VPC to perform NAT. This controls whether source/destination checking is enabled on the instance. A value of @true@ means that checking is enabled, and @false@ means that checking is disabled. The value must be @false@ for the instance to perform NAT. For more information, see <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_NAT_Instance.html NAT Instances> in the /Amazon Virtual Private Cloud User Guide/ .
--
-- /Note:/ Consider using 'sourceDestCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insSourceDestCheck :: Lens.Lens' Instance (Lude.Maybe Lude.Bool)
insSourceDestCheck = Lens.lens (sourceDestCheck :: Instance -> Lude.Maybe Lude.Bool) (\s a -> s {sourceDestCheck = a} :: Instance)
{-# DEPRECATED insSourceDestCheck "Use generic-lens or generic-optics with 'sourceDestCheck' instead." #-}

-- | The Elastic GPU associated with the instance.
--
-- /Note:/ Consider using 'elasticGpuAssociations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insElasticGpuAssociations :: Lens.Lens' Instance (Lude.Maybe [ElasticGpuAssociation])
insElasticGpuAssociations = Lens.lens (elasticGpuAssociations :: Instance -> Lude.Maybe [ElasticGpuAssociation]) (\s a -> s {elasticGpuAssociations = a} :: Instance)
{-# DEPRECATED insElasticGpuAssociations "Use generic-lens or generic-optics with 'elasticGpuAssociations' instead." #-}

-- | [EC2-VPC] The ID of the VPC in which the instance is running.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insVPCId :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
insVPCId = Lens.lens (vpcId :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: Instance)
{-# DEPRECATED insVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The name of the key pair, if this instance was launched with an associated key pair.
--
-- /Note:/ Consider using 'keyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insKeyName :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
insKeyName = Lens.lens (keyName :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {keyName = a} :: Instance)
{-# DEPRECATED insKeyName "Use generic-lens or generic-optics with 'keyName' instead." #-}

-- | [EC2-VPC] The network interfaces for the instance.
--
-- /Note:/ Consider using 'networkInterfaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insNetworkInterfaces :: Lens.Lens' Instance (Lude.Maybe [InstanceNetworkInterface])
insNetworkInterfaces = Lens.lens (networkInterfaces :: Instance -> Lude.Maybe [InstanceNetworkInterface]) (\s a -> s {networkInterfaces = a} :: Instance)
{-# DEPRECATED insNetworkInterfaces "Use generic-lens or generic-optics with 'networkInterfaces' instead." #-}

-- | The Amazon Resource Name (ARN) of the Outpost.
--
-- /Note:/ Consider using 'outpostARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insOutpostARN :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
insOutpostARN = Lens.lens (outpostARN :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {outpostARN = a} :: Instance)
{-# DEPRECATED insOutpostARN "Use generic-lens or generic-optics with 'outpostARN' instead." #-}

-- | Indicates whether the instance is enabled for AWS Nitro Enclaves.
--
-- /Note:/ Consider using 'enclaveOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insEnclaveOptions :: Lens.Lens' Instance (Lude.Maybe EnclaveOptions)
insEnclaveOptions = Lens.lens (enclaveOptions :: Instance -> Lude.Maybe EnclaveOptions) (\s a -> s {enclaveOptions = a} :: Instance)
{-# DEPRECATED insEnclaveOptions "Use generic-lens or generic-optics with 'enclaveOptions' instead." #-}

-- | The RAM disk associated with this instance, if applicable.
--
-- /Note:/ Consider using 'ramdiskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insRAMDiskId :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
insRAMDiskId = Lens.lens (ramdiskId :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {ramdiskId = a} :: Instance)
{-# DEPRECATED insRAMDiskId "Use generic-lens or generic-optics with 'ramdiskId' instead." #-}

-- | The CPU options for the instance.
--
-- /Note:/ Consider using 'cpuOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insCPUOptions :: Lens.Lens' Instance (Lude.Maybe CPUOptions)
insCPUOptions = Lens.lens (cpuOptions :: Instance -> Lude.Maybe CPUOptions) (\s a -> s {cpuOptions = a} :: Instance)
{-# DEPRECATED insCPUOptions "Use generic-lens or generic-optics with 'cpuOptions' instead." #-}

-- | [EC2-VPC] The ID of the subnet in which the instance is running.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insSubnetId :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
insSubnetId = Lens.lens (subnetId :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {subnetId = a} :: Instance)
{-# DEPRECATED insSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The kernel associated with this instance, if applicable.
--
-- /Note:/ Consider using 'kernelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insKernelId :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
insKernelId = Lens.lens (kernelId :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {kernelId = a} :: Instance)
{-# DEPRECATED insKernelId "Use generic-lens or generic-optics with 'kernelId' instead." #-}

-- | The device name of the root device volume (for example, @/dev/sda1@ ).
--
-- /Note:/ Consider using 'rootDeviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insRootDeviceName :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
insRootDeviceName = Lens.lens (rootDeviceName :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {rootDeviceName = a} :: Instance)
{-# DEPRECATED insRootDeviceName "Use generic-lens or generic-optics with 'rootDeviceName' instead." #-}

-- | The ID of the Capacity Reservation.
--
-- /Note:/ Consider using 'capacityReservationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insCapacityReservationId :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
insCapacityReservationId = Lens.lens (capacityReservationId :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {capacityReservationId = a} :: Instance)
{-# DEPRECATED insCapacityReservationId "Use generic-lens or generic-optics with 'capacityReservationId' instead." #-}

-- | Information about the Capacity Reservation targeting option.
--
-- /Note:/ Consider using 'capacityReservationSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insCapacityReservationSpecification :: Lens.Lens' Instance (Lude.Maybe CapacityReservationSpecificationResponse)
insCapacityReservationSpecification = Lens.lens (capacityReservationSpecification :: Instance -> Lude.Maybe CapacityReservationSpecificationResponse) (\s a -> s {capacityReservationSpecification = a} :: Instance)
{-# DEPRECATED insCapacityReservationSpecification "Use generic-lens or generic-optics with 'capacityReservationSpecification' instead." #-}

-- | Specifies whether enhanced networking with the Intel 82599 Virtual Function interface is enabled.
--
-- /Note:/ Consider using 'sriovNetSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insSRIOVNetSupport :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
insSRIOVNetSupport = Lens.lens (sriovNetSupport :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {sriovNetSupport = a} :: Instance)
{-# DEPRECATED insSRIOVNetSupport "Use generic-lens or generic-optics with 'sriovNetSupport' instead." #-}

-- | Indicates whether the instance is optimized for Amazon EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS Optimized instance.
--
-- /Note:/ Consider using 'ebsOptimized' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insEBSOptimized :: Lens.Lens' Instance (Lude.Maybe Lude.Bool)
insEBSOptimized = Lens.lens (ebsOptimized :: Instance -> Lude.Maybe Lude.Bool) (\s a -> s {ebsOptimized = a} :: Instance)
{-# DEPRECATED insEBSOptimized "Use generic-lens or generic-optics with 'ebsOptimized' instead." #-}

-- | The reason for the most recent state transition. This might be an empty string.
--
-- /Note:/ Consider using 'stateTransitionReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insStateTransitionReason :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
insStateTransitionReason = Lens.lens (stateTransitionReason :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {stateTransitionReason = a} :: Instance)
{-# DEPRECATED insStateTransitionReason "Use generic-lens or generic-optics with 'stateTransitionReason' instead." #-}

-- | Indicates whether the instance is enabled for hibernation.
--
-- /Note:/ Consider using 'hibernationOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insHibernationOptions :: Lens.Lens' Instance (Lude.Maybe HibernationOptions)
insHibernationOptions = Lens.lens (hibernationOptions :: Instance -> Lude.Maybe HibernationOptions) (\s a -> s {hibernationOptions = a} :: Instance)
{-# DEPRECATED insHibernationOptions "Use generic-lens or generic-optics with 'hibernationOptions' instead." #-}

-- | Indicates whether this is a Spot Instance or a Scheduled Instance.
--
-- /Note:/ Consider using 'instanceLifecycle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insInstanceLifecycle :: Lens.Lens' Instance (Lude.Maybe InstanceLifecycleType)
insInstanceLifecycle = Lens.lens (instanceLifecycle :: Instance -> Lude.Maybe InstanceLifecycleType) (\s a -> s {instanceLifecycle = a} :: Instance)
{-# DEPRECATED insInstanceLifecycle "Use generic-lens or generic-optics with 'instanceLifecycle' instead." #-}

-- | The IAM instance profile associated with the instance, if applicable.
--
-- /Note:/ Consider using 'iamInstanceProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insIAMInstanceProfile :: Lens.Lens' Instance (Lude.Maybe IAMInstanceProfile)
insIAMInstanceProfile = Lens.lens (iamInstanceProfile :: Instance -> Lude.Maybe IAMInstanceProfile) (\s a -> s {iamInstanceProfile = a} :: Instance)
{-# DEPRECATED insIAMInstanceProfile "Use generic-lens or generic-optics with 'iamInstanceProfile' instead." #-}

-- | The private IPv4 address assigned to the instance.
--
-- /Note:/ Consider using 'privateIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insPrivateIPAddress :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
insPrivateIPAddress = Lens.lens (privateIPAddress :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {privateIPAddress = a} :: Instance)
{-# DEPRECATED insPrivateIPAddress "Use generic-lens or generic-optics with 'privateIPAddress' instead." #-}

-- | The metadata options for the instance.
--
-- /Note:/ Consider using 'metadataOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insMetadataOptions :: Lens.Lens' Instance (Lude.Maybe InstanceMetadataOptionsResponse)
insMetadataOptions = Lens.lens (metadataOptions :: Instance -> Lude.Maybe InstanceMetadataOptionsResponse) (\s a -> s {metadataOptions = a} :: Instance)
{-# DEPRECATED insMetadataOptions "Use generic-lens or generic-optics with 'metadataOptions' instead." #-}

-- | The product codes attached to this instance, if applicable.
--
-- /Note:/ Consider using 'productCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insProductCodes :: Lens.Lens' Instance (Lude.Maybe [ProductCode])
insProductCodes = Lens.lens (productCodes :: Instance -> Lude.Maybe [ProductCode]) (\s a -> s {productCodes = a} :: Instance)
{-# DEPRECATED insProductCodes "Use generic-lens or generic-optics with 'productCodes' instead." #-}

-- | If the request is a Spot Instance request, the ID of the request.
--
-- /Note:/ Consider using 'spotInstanceRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insSpotInstanceRequestId :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
insSpotInstanceRequestId = Lens.lens (spotInstanceRequestId :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {spotInstanceRequestId = a} :: Instance)
{-# DEPRECATED insSpotInstanceRequestId "Use generic-lens or generic-optics with 'spotInstanceRequestId' instead." #-}

-- | The license configurations.
--
-- /Note:/ Consider using 'licenses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insLicenses :: Lens.Lens' Instance (Lude.Maybe [LicenseConfiguration])
insLicenses = Lens.lens (licenses :: Instance -> Lude.Maybe [LicenseConfiguration]) (\s a -> s {licenses = a} :: Instance)
{-# DEPRECATED insLicenses "Use generic-lens or generic-optics with 'licenses' instead." #-}

-- | The elastic inference accelerator associated with the instance.
--
-- /Note:/ Consider using 'elasticInferenceAcceleratorAssociations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insElasticInferenceAcceleratorAssociations :: Lens.Lens' Instance (Lude.Maybe [ElasticInferenceAcceleratorAssociation])
insElasticInferenceAcceleratorAssociations = Lens.lens (elasticInferenceAcceleratorAssociations :: Instance -> Lude.Maybe [ElasticInferenceAcceleratorAssociation]) (\s a -> s {elasticInferenceAcceleratorAssociations = a} :: Instance)
{-# DEPRECATED insElasticInferenceAcceleratorAssociations "Use generic-lens or generic-optics with 'elasticInferenceAcceleratorAssociations' instead." #-}

-- | (IPv4 only) The private DNS hostname name assigned to the instance. This DNS hostname can only be used inside the Amazon EC2 network. This name is not available until the instance enters the @running@ state.
--
-- [EC2-VPC] The Amazon-provided DNS server resolves Amazon-provided private DNS hostnames if you've enabled DNS resolution and DNS hostnames in your VPC. If you are not using the Amazon-provided DNS server in your VPC, your custom domain name servers must resolve the hostname as appropriate.
--
-- /Note:/ Consider using 'privateDNSName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insPrivateDNSName :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
insPrivateDNSName = Lens.lens (privateDNSName :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {privateDNSName = a} :: Instance)
{-# DEPRECATED insPrivateDNSName "Use generic-lens or generic-optics with 'privateDNSName' instead." #-}

-- | The reason for the most recent state transition.
--
-- /Note:/ Consider using 'stateReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insStateReason :: Lens.Lens' Instance (Lude.Maybe StateReason)
insStateReason = Lens.lens (stateReason :: Instance -> Lude.Maybe StateReason) (\s a -> s {stateReason = a} :: Instance)
{-# DEPRECATED insStateReason "Use generic-lens or generic-optics with 'stateReason' instead." #-}

-- | Any block device mapping entries for the instance.
--
-- /Note:/ Consider using 'blockDeviceMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insBlockDeviceMappings :: Lens.Lens' Instance (Lude.Maybe [InstanceBlockDeviceMapping])
insBlockDeviceMappings = Lens.lens (blockDeviceMappings :: Instance -> Lude.Maybe [InstanceBlockDeviceMapping]) (\s a -> s {blockDeviceMappings = a} :: Instance)
{-# DEPRECATED insBlockDeviceMappings "Use generic-lens or generic-optics with 'blockDeviceMappings' instead." #-}

-- | The public IPv4 address, or the Carrier IP address assigned to the instance, if applicable.
--
-- A Carrier IP address only applies to an instance launched in a subnet associated with a Wavelength Zone.
--
-- /Note:/ Consider using 'publicIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insPublicIPAddress :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
insPublicIPAddress = Lens.lens (publicIPAddress :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {publicIPAddress = a} :: Instance)
{-# DEPRECATED insPublicIPAddress "Use generic-lens or generic-optics with 'publicIPAddress' instead." #-}

-- | Any tags assigned to the instance.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insTags :: Lens.Lens' Instance (Lude.Maybe [Tag])
insTags = Lens.lens (tags :: Instance -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: Instance)
{-# DEPRECATED insTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insInstanceId :: Lens.Lens' Instance Lude.Text
insInstanceId = Lens.lens (instanceId :: Instance -> Lude.Text) (\s a -> s {instanceId = a} :: Instance)
{-# DEPRECATED insInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The ID of the AMI used to launch the instance.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insImageId :: Lens.Lens' Instance Lude.Text
insImageId = Lens.lens (imageId :: Instance -> Lude.Text) (\s a -> s {imageId = a} :: Instance)
{-# DEPRECATED insImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | The AMI launch index, which can be used to find this instance in the launch group.
--
-- /Note:/ Consider using 'amiLaunchIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insAMILaunchIndex :: Lens.Lens' Instance Lude.Int
insAMILaunchIndex = Lens.lens (amiLaunchIndex :: Instance -> Lude.Int) (\s a -> s {amiLaunchIndex = a} :: Instance)
{-# DEPRECATED insAMILaunchIndex "Use generic-lens or generic-optics with 'amiLaunchIndex' instead." #-}

-- | The instance type.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insInstanceType :: Lens.Lens' Instance InstanceType
insInstanceType = Lens.lens (instanceType :: Instance -> InstanceType) (\s a -> s {instanceType = a} :: Instance)
{-# DEPRECATED insInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The time the instance was launched.
--
-- /Note:/ Consider using 'launchTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insLaunchTime :: Lens.Lens' Instance Lude.DateTime
insLaunchTime = Lens.lens (launchTime :: Instance -> Lude.DateTime) (\s a -> s {launchTime = a} :: Instance)
{-# DEPRECATED insLaunchTime "Use generic-lens or generic-optics with 'launchTime' instead." #-}

-- | The location where the instance launched, if applicable.
--
-- /Note:/ Consider using 'placement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insPlacement :: Lens.Lens' Instance Placement
insPlacement = Lens.lens (placement :: Instance -> Placement) (\s a -> s {placement = a} :: Instance)
{-# DEPRECATED insPlacement "Use generic-lens or generic-optics with 'placement' instead." #-}

-- | The monitoring for the instance.
--
-- /Note:/ Consider using 'monitoring' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insMonitoring :: Lens.Lens' Instance Monitoring
insMonitoring = Lens.lens (monitoring :: Instance -> Monitoring) (\s a -> s {monitoring = a} :: Instance)
{-# DEPRECATED insMonitoring "Use generic-lens or generic-optics with 'monitoring' instead." #-}

-- | The architecture of the image.
--
-- /Note:/ Consider using 'architecture' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insArchitecture :: Lens.Lens' Instance ArchitectureValues
insArchitecture = Lens.lens (architecture :: Instance -> ArchitectureValues) (\s a -> s {architecture = a} :: Instance)
{-# DEPRECATED insArchitecture "Use generic-lens or generic-optics with 'architecture' instead." #-}

-- | The root device type used by the AMI. The AMI can use an EBS volume or an instance store volume.
--
-- /Note:/ Consider using 'rootDeviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insRootDeviceType :: Lens.Lens' Instance DeviceType
insRootDeviceType = Lens.lens (rootDeviceType :: Instance -> DeviceType) (\s a -> s {rootDeviceType = a} :: Instance)
{-# DEPRECATED insRootDeviceType "Use generic-lens or generic-optics with 'rootDeviceType' instead." #-}

-- | The virtualization type of the instance.
--
-- /Note:/ Consider using 'virtualizationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insVirtualizationType :: Lens.Lens' Instance VirtualizationType
insVirtualizationType = Lens.lens (virtualizationType :: Instance -> VirtualizationType) (\s a -> s {virtualizationType = a} :: Instance)
{-# DEPRECATED insVirtualizationType "Use generic-lens or generic-optics with 'virtualizationType' instead." #-}

-- | The hypervisor type of the instance. The value @xen@ is used for both Xen and Nitro hypervisors.
--
-- /Note:/ Consider using 'hypervisor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insHypervisor :: Lens.Lens' Instance HypervisorType
insHypervisor = Lens.lens (hypervisor :: Instance -> HypervisorType) (\s a -> s {hypervisor = a} :: Instance)
{-# DEPRECATED insHypervisor "Use generic-lens or generic-optics with 'hypervisor' instead." #-}

-- | The current state of the instance.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insState :: Lens.Lens' Instance InstanceState
insState = Lens.lens (state :: Instance -> InstanceState) (\s a -> s {state = a} :: Instance)
{-# DEPRECATED insState "Use generic-lens or generic-optics with 'state' instead." #-}

instance Lude.FromXML Instance where
  parseXML x =
    Instance'
      Lude.<$> (x Lude..@? "dnsName")
      Lude.<*> (x Lude..@? "platform")
      Lude.<*> ( x Lude..@? "groupSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "clientToken")
      Lude.<*> (x Lude..@? "enaSupport")
      Lude.<*> (x Lude..@? "sourceDestCheck")
      Lude.<*> ( x Lude..@? "elasticGpuAssociationSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "vpcId")
      Lude.<*> (x Lude..@? "keyName")
      Lude.<*> ( x Lude..@? "networkInterfaceSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "outpostArn")
      Lude.<*> (x Lude..@? "enclaveOptions")
      Lude.<*> (x Lude..@? "ramdiskId")
      Lude.<*> (x Lude..@? "cpuOptions")
      Lude.<*> (x Lude..@? "subnetId")
      Lude.<*> (x Lude..@? "kernelId")
      Lude.<*> (x Lude..@? "rootDeviceName")
      Lude.<*> (x Lude..@? "capacityReservationId")
      Lude.<*> (x Lude..@? "capacityReservationSpecification")
      Lude.<*> (x Lude..@? "sriovNetSupport")
      Lude.<*> (x Lude..@? "ebsOptimized")
      Lude.<*> (x Lude..@? "reason")
      Lude.<*> (x Lude..@? "hibernationOptions")
      Lude.<*> (x Lude..@? "instanceLifecycle")
      Lude.<*> (x Lude..@? "iamInstanceProfile")
      Lude.<*> (x Lude..@? "privateIpAddress")
      Lude.<*> (x Lude..@? "metadataOptions")
      Lude.<*> ( x Lude..@? "productCodes" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "spotInstanceRequestId")
      Lude.<*> ( x Lude..@? "licenseSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> ( x Lude..@? "elasticInferenceAcceleratorAssociationSet"
                   Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "privateDnsName")
      Lude.<*> (x Lude..@? "stateReason")
      Lude.<*> ( x Lude..@? "blockDeviceMapping" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "ipAddress")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@ "instanceId")
      Lude.<*> (x Lude..@ "imageId")
      Lude.<*> (x Lude..@ "amiLaunchIndex")
      Lude.<*> (x Lude..@ "instanceType")
      Lude.<*> (x Lude..@ "launchTime")
      Lude.<*> (x Lude..@ "placement")
      Lude.<*> (x Lude..@ "monitoring")
      Lude.<*> (x Lude..@ "architecture")
      Lude.<*> (x Lude..@ "rootDeviceType")
      Lude.<*> (x Lude..@ "virtualizationType")
      Lude.<*> (x Lude..@ "hypervisor")
      Lude.<*> (x Lude..@ "instanceState")
