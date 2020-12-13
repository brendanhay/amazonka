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
    ifInstanceId,
    ifState,
    ifVirtualizationType,
    ifPublicDNSName,
    ifHypervisor,
    ifPlatform,
    ifSecurityGroups,
    ifClientToken,
    ifEnaSupport,
    ifSourceDestCheck,
    ifElasticGpuAssociations,
    ifVPCId,
    ifKeyName,
    ifLaunchTime,
    ifNetworkInterfaces,
    ifOutpostARN,
    ifEnclaveOptions,
    ifRAMDiskId,
    ifCPUOptions,
    ifSubnetId,
    ifKernelId,
    ifRootDeviceName,
    ifCapacityReservationId,
    ifInstanceType,
    ifCapacityReservationSpecification,
    ifSRIOVNetSupport,
    ifEBSOptimized,
    ifMonitoring,
    ifStateTransitionReason,
    ifHibernationOptions,
    ifInstanceLifecycle,
    ifIAMInstanceProfile,
    ifImageId,
    ifPrivateIPAddress,
    ifMetadataOptions,
    ifArchitecture,
    ifProductCodes,
    ifSpotInstanceRequestId,
    ifLicenses,
    ifElasticInferenceAcceleratorAssociations,
    ifPrivateDNSName,
    ifStateReason,
    ifRootDeviceType,
    ifBlockDeviceMappings,
    ifAMILaunchIndex,
    ifPublicIPAddress,
    ifPlacement,
    ifTags,
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
  { -- | The ID of the instance.
    instanceId :: Lude.Text,
    -- | The current state of the instance.
    state :: InstanceState,
    -- | The virtualization type of the instance.
    virtualizationType :: VirtualizationType,
    -- | (IPv4 only) The public DNS name assigned to the instance. This name is not available until the instance enters the @running@ state. For EC2-VPC, this name is only available if you've enabled DNS hostnames for your VPC.
    publicDNSName :: Lude.Maybe Lude.Text,
    -- | The hypervisor type of the instance. The value @xen@ is used for both Xen and Nitro hypervisors.
    hypervisor :: HypervisorType,
    -- | The value is @Windows@ for Windows instances; otherwise blank.
    platform :: Lude.Maybe PlatformValues,
    -- | The security groups for the instance.
    securityGroups :: Lude.Maybe [GroupIdentifier],
    -- | The idempotency token you provided when you launched the instance, if applicable.
    clientToken :: Lude.Maybe Lude.Text,
    -- | Specifies whether enhanced networking with ENA is enabled.
    enaSupport :: Lude.Maybe Lude.Bool,
    -- | Specifies whether to enable an instance launched in a VPC to perform NAT. This controls whether source/destination checking is enabled on the instance. A value of @true@ means that checking is enabled, and @false@ means that checking is disabled. The value must be @false@ for the instance to perform NAT. For more information, see <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_NAT_Instance.html NAT Instances> in the /Amazon Virtual Private Cloud User Guide/ .
    sourceDestCheck :: Lude.Maybe Lude.Bool,
    -- | The Elastic GPU associated with the instance.
    elasticGpuAssociations :: Lude.Maybe [ElasticGpuAssociation],
    -- | [EC2-VPC] The ID of the VPC in which the instance is running.
    vpcId :: Lude.Maybe Lude.Text,
    -- | The name of the key pair, if this instance was launched with an associated key pair.
    keyName :: Lude.Maybe Lude.Text,
    -- | The time the instance was launched.
    launchTime :: Lude.DateTime,
    -- | [EC2-VPC] The network interfaces for the instance.
    networkInterfaces :: Lude.Maybe [InstanceNetworkInterface],
    -- | The Amazon Resource Name (ARN) of the Outpost.
    outpostARN :: Lude.Maybe Lude.Text,
    -- | Indicates whether the instance is enabled for AWS Nitro Enclaves.
    enclaveOptions :: Lude.Maybe EnclaveOptions,
    -- | The RAM disk associated with this instance, if applicable.
    ramdiskId :: Lude.Maybe Lude.Text,
    -- | The CPU options for the instance.
    cpuOptions :: Lude.Maybe CPUOptions,
    -- | [EC2-VPC] The ID of the subnet in which the instance is running.
    subnetId :: Lude.Maybe Lude.Text,
    -- | The kernel associated with this instance, if applicable.
    kernelId :: Lude.Maybe Lude.Text,
    -- | The device name of the root device volume (for example, @/dev/sda1@ ).
    rootDeviceName :: Lude.Maybe Lude.Text,
    -- | The ID of the Capacity Reservation.
    capacityReservationId :: Lude.Maybe Lude.Text,
    -- | The instance type.
    instanceType :: InstanceType,
    -- | Information about the Capacity Reservation targeting option.
    capacityReservationSpecification :: Lude.Maybe CapacityReservationSpecificationResponse,
    -- | Specifies whether enhanced networking with the Intel 82599 Virtual Function interface is enabled.
    sriovNetSupport :: Lude.Maybe Lude.Text,
    -- | Indicates whether the instance is optimized for Amazon EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS Optimized instance.
    ebsOptimized :: Lude.Maybe Lude.Bool,
    -- | The monitoring for the instance.
    monitoring :: Monitoring,
    -- | The reason for the most recent state transition. This might be an empty string.
    stateTransitionReason :: Lude.Maybe Lude.Text,
    -- | Indicates whether the instance is enabled for hibernation.
    hibernationOptions :: Lude.Maybe HibernationOptions,
    -- | Indicates whether this is a Spot Instance or a Scheduled Instance.
    instanceLifecycle :: Lude.Maybe InstanceLifecycleType,
    -- | The IAM instance profile associated with the instance, if applicable.
    iamInstanceProfile :: Lude.Maybe IAMInstanceProfile,
    -- | The ID of the AMI used to launch the instance.
    imageId :: Lude.Text,
    -- | The private IPv4 address assigned to the instance.
    privateIPAddress :: Lude.Maybe Lude.Text,
    -- | The metadata options for the instance.
    metadataOptions :: Lude.Maybe InstanceMetadataOptionsResponse,
    -- | The architecture of the image.
    architecture :: ArchitectureValues,
    -- | The product codes attached to this instance, if applicable.
    productCodes :: Lude.Maybe [ProductCode],
    -- | If the request is a Spot Instance request, the ID of the request.
    spotInstanceRequestId :: Lude.Maybe Lude.Text,
    -- | The license configurations.
    licenses :: Lude.Maybe [LicenseConfiguration],
    -- | The elastic inference accelerator associated with the instance.
    elasticInferenceAcceleratorAssociations :: Lude.Maybe [ElasticInferenceAcceleratorAssociation],
    -- | (IPv4 only) The private DNS hostname name assigned to the instance. This DNS hostname can only be used inside the Amazon EC2 network. This name is not available until the instance enters the @running@ state.
    --
    -- [EC2-VPC] The Amazon-provided DNS server resolves Amazon-provided private DNS hostnames if you've enabled DNS resolution and DNS hostnames in your VPC. If you are not using the Amazon-provided DNS server in your VPC, your custom domain name servers must resolve the hostname as appropriate.
    privateDNSName :: Lude.Maybe Lude.Text,
    -- | The reason for the most recent state transition.
    stateReason :: Lude.Maybe StateReason,
    -- | The root device type used by the AMI. The AMI can use an EBS volume or an instance store volume.
    rootDeviceType :: DeviceType,
    -- | Any block device mapping entries for the instance.
    blockDeviceMappings :: Lude.Maybe [InstanceBlockDeviceMapping],
    -- | The AMI launch index, which can be used to find this instance in the launch group.
    amiLaunchIndex :: Lude.Int,
    -- | The public IPv4 address, or the Carrier IP address assigned to the instance, if applicable.
    --
    -- A Carrier IP address only applies to an instance launched in a subnet associated with a Wavelength Zone.
    publicIPAddress :: Lude.Maybe Lude.Text,
    -- | The location where the instance launched, if applicable.
    placement :: Placement,
    -- | Any tags assigned to the instance.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Instance' with the minimum fields required to make a request.
--
-- * 'instanceId' - The ID of the instance.
-- * 'state' - The current state of the instance.
-- * 'virtualizationType' - The virtualization type of the instance.
-- * 'publicDNSName' - (IPv4 only) The public DNS name assigned to the instance. This name is not available until the instance enters the @running@ state. For EC2-VPC, this name is only available if you've enabled DNS hostnames for your VPC.
-- * 'hypervisor' - The hypervisor type of the instance. The value @xen@ is used for both Xen and Nitro hypervisors.
-- * 'platform' - The value is @Windows@ for Windows instances; otherwise blank.
-- * 'securityGroups' - The security groups for the instance.
-- * 'clientToken' - The idempotency token you provided when you launched the instance, if applicable.
-- * 'enaSupport' - Specifies whether enhanced networking with ENA is enabled.
-- * 'sourceDestCheck' - Specifies whether to enable an instance launched in a VPC to perform NAT. This controls whether source/destination checking is enabled on the instance. A value of @true@ means that checking is enabled, and @false@ means that checking is disabled. The value must be @false@ for the instance to perform NAT. For more information, see <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_NAT_Instance.html NAT Instances> in the /Amazon Virtual Private Cloud User Guide/ .
-- * 'elasticGpuAssociations' - The Elastic GPU associated with the instance.
-- * 'vpcId' - [EC2-VPC] The ID of the VPC in which the instance is running.
-- * 'keyName' - The name of the key pair, if this instance was launched with an associated key pair.
-- * 'launchTime' - The time the instance was launched.
-- * 'networkInterfaces' - [EC2-VPC] The network interfaces for the instance.
-- * 'outpostARN' - The Amazon Resource Name (ARN) of the Outpost.
-- * 'enclaveOptions' - Indicates whether the instance is enabled for AWS Nitro Enclaves.
-- * 'ramdiskId' - The RAM disk associated with this instance, if applicable.
-- * 'cpuOptions' - The CPU options for the instance.
-- * 'subnetId' - [EC2-VPC] The ID of the subnet in which the instance is running.
-- * 'kernelId' - The kernel associated with this instance, if applicable.
-- * 'rootDeviceName' - The device name of the root device volume (for example, @/dev/sda1@ ).
-- * 'capacityReservationId' - The ID of the Capacity Reservation.
-- * 'instanceType' - The instance type.
-- * 'capacityReservationSpecification' - Information about the Capacity Reservation targeting option.
-- * 'sriovNetSupport' - Specifies whether enhanced networking with the Intel 82599 Virtual Function interface is enabled.
-- * 'ebsOptimized' - Indicates whether the instance is optimized for Amazon EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS Optimized instance.
-- * 'monitoring' - The monitoring for the instance.
-- * 'stateTransitionReason' - The reason for the most recent state transition. This might be an empty string.
-- * 'hibernationOptions' - Indicates whether the instance is enabled for hibernation.
-- * 'instanceLifecycle' - Indicates whether this is a Spot Instance or a Scheduled Instance.
-- * 'iamInstanceProfile' - The IAM instance profile associated with the instance, if applicable.
-- * 'imageId' - The ID of the AMI used to launch the instance.
-- * 'privateIPAddress' - The private IPv4 address assigned to the instance.
-- * 'metadataOptions' - The metadata options for the instance.
-- * 'architecture' - The architecture of the image.
-- * 'productCodes' - The product codes attached to this instance, if applicable.
-- * 'spotInstanceRequestId' - If the request is a Spot Instance request, the ID of the request.
-- * 'licenses' - The license configurations.
-- * 'elasticInferenceAcceleratorAssociations' - The elastic inference accelerator associated with the instance.
-- * 'privateDNSName' - (IPv4 only) The private DNS hostname name assigned to the instance. This DNS hostname can only be used inside the Amazon EC2 network. This name is not available until the instance enters the @running@ state.
--
-- [EC2-VPC] The Amazon-provided DNS server resolves Amazon-provided private DNS hostnames if you've enabled DNS resolution and DNS hostnames in your VPC. If you are not using the Amazon-provided DNS server in your VPC, your custom domain name servers must resolve the hostname as appropriate.
-- * 'stateReason' - The reason for the most recent state transition.
-- * 'rootDeviceType' - The root device type used by the AMI. The AMI can use an EBS volume or an instance store volume.
-- * 'blockDeviceMappings' - Any block device mapping entries for the instance.
-- * 'amiLaunchIndex' - The AMI launch index, which can be used to find this instance in the launch group.
-- * 'publicIPAddress' - The public IPv4 address, or the Carrier IP address assigned to the instance, if applicable.
--
-- A Carrier IP address only applies to an instance launched in a subnet associated with a Wavelength Zone.
-- * 'placement' - The location where the instance launched, if applicable.
-- * 'tags' - Any tags assigned to the instance.
mkInstance ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'state'
  InstanceState ->
  -- | 'virtualizationType'
  VirtualizationType ->
  -- | 'hypervisor'
  HypervisorType ->
  -- | 'launchTime'
  Lude.DateTime ->
  -- | 'instanceType'
  InstanceType ->
  -- | 'monitoring'
  Monitoring ->
  -- | 'imageId'
  Lude.Text ->
  -- | 'architecture'
  ArchitectureValues ->
  -- | 'rootDeviceType'
  DeviceType ->
  -- | 'amiLaunchIndex'
  Lude.Int ->
  -- | 'placement'
  Placement ->
  Instance
mkInstance
  pInstanceId_
  pState_
  pVirtualizationType_
  pHypervisor_
  pLaunchTime_
  pInstanceType_
  pMonitoring_
  pImageId_
  pArchitecture_
  pRootDeviceType_
  pAMILaunchIndex_
  pPlacement_ =
    Instance'
      { instanceId = pInstanceId_,
        state = pState_,
        virtualizationType = pVirtualizationType_,
        publicDNSName = Lude.Nothing,
        hypervisor = pHypervisor_,
        platform = Lude.Nothing,
        securityGroups = Lude.Nothing,
        clientToken = Lude.Nothing,
        enaSupport = Lude.Nothing,
        sourceDestCheck = Lude.Nothing,
        elasticGpuAssociations = Lude.Nothing,
        vpcId = Lude.Nothing,
        keyName = Lude.Nothing,
        launchTime = pLaunchTime_,
        networkInterfaces = Lude.Nothing,
        outpostARN = Lude.Nothing,
        enclaveOptions = Lude.Nothing,
        ramdiskId = Lude.Nothing,
        cpuOptions = Lude.Nothing,
        subnetId = Lude.Nothing,
        kernelId = Lude.Nothing,
        rootDeviceName = Lude.Nothing,
        capacityReservationId = Lude.Nothing,
        instanceType = pInstanceType_,
        capacityReservationSpecification = Lude.Nothing,
        sriovNetSupport = Lude.Nothing,
        ebsOptimized = Lude.Nothing,
        monitoring = pMonitoring_,
        stateTransitionReason = Lude.Nothing,
        hibernationOptions = Lude.Nothing,
        instanceLifecycle = Lude.Nothing,
        iamInstanceProfile = Lude.Nothing,
        imageId = pImageId_,
        privateIPAddress = Lude.Nothing,
        metadataOptions = Lude.Nothing,
        architecture = pArchitecture_,
        productCodes = Lude.Nothing,
        spotInstanceRequestId = Lude.Nothing,
        licenses = Lude.Nothing,
        elasticInferenceAcceleratorAssociations = Lude.Nothing,
        privateDNSName = Lude.Nothing,
        stateReason = Lude.Nothing,
        rootDeviceType = pRootDeviceType_,
        blockDeviceMappings = Lude.Nothing,
        amiLaunchIndex = pAMILaunchIndex_,
        publicIPAddress = Lude.Nothing,
        placement = pPlacement_,
        tags = Lude.Nothing
      }

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifInstanceId :: Lens.Lens' Instance Lude.Text
ifInstanceId = Lens.lens (instanceId :: Instance -> Lude.Text) (\s a -> s {instanceId = a} :: Instance)
{-# DEPRECATED ifInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The current state of the instance.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifState :: Lens.Lens' Instance InstanceState
ifState = Lens.lens (state :: Instance -> InstanceState) (\s a -> s {state = a} :: Instance)
{-# DEPRECATED ifState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The virtualization type of the instance.
--
-- /Note:/ Consider using 'virtualizationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifVirtualizationType :: Lens.Lens' Instance VirtualizationType
ifVirtualizationType = Lens.lens (virtualizationType :: Instance -> VirtualizationType) (\s a -> s {virtualizationType = a} :: Instance)
{-# DEPRECATED ifVirtualizationType "Use generic-lens or generic-optics with 'virtualizationType' instead." #-}

-- | (IPv4 only) The public DNS name assigned to the instance. This name is not available until the instance enters the @running@ state. For EC2-VPC, this name is only available if you've enabled DNS hostnames for your VPC.
--
-- /Note:/ Consider using 'publicDNSName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifPublicDNSName :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
ifPublicDNSName = Lens.lens (publicDNSName :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {publicDNSName = a} :: Instance)
{-# DEPRECATED ifPublicDNSName "Use generic-lens or generic-optics with 'publicDNSName' instead." #-}

-- | The hypervisor type of the instance. The value @xen@ is used for both Xen and Nitro hypervisors.
--
-- /Note:/ Consider using 'hypervisor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifHypervisor :: Lens.Lens' Instance HypervisorType
ifHypervisor = Lens.lens (hypervisor :: Instance -> HypervisorType) (\s a -> s {hypervisor = a} :: Instance)
{-# DEPRECATED ifHypervisor "Use generic-lens or generic-optics with 'hypervisor' instead." #-}

-- | The value is @Windows@ for Windows instances; otherwise blank.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifPlatform :: Lens.Lens' Instance (Lude.Maybe PlatformValues)
ifPlatform = Lens.lens (platform :: Instance -> Lude.Maybe PlatformValues) (\s a -> s {platform = a} :: Instance)
{-# DEPRECATED ifPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | The security groups for the instance.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifSecurityGroups :: Lens.Lens' Instance (Lude.Maybe [GroupIdentifier])
ifSecurityGroups = Lens.lens (securityGroups :: Instance -> Lude.Maybe [GroupIdentifier]) (\s a -> s {securityGroups = a} :: Instance)
{-# DEPRECATED ifSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | The idempotency token you provided when you launched the instance, if applicable.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifClientToken :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
ifClientToken = Lens.lens (clientToken :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: Instance)
{-# DEPRECATED ifClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | Specifies whether enhanced networking with ENA is enabled.
--
-- /Note:/ Consider using 'enaSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifEnaSupport :: Lens.Lens' Instance (Lude.Maybe Lude.Bool)
ifEnaSupport = Lens.lens (enaSupport :: Instance -> Lude.Maybe Lude.Bool) (\s a -> s {enaSupport = a} :: Instance)
{-# DEPRECATED ifEnaSupport "Use generic-lens or generic-optics with 'enaSupport' instead." #-}

-- | Specifies whether to enable an instance launched in a VPC to perform NAT. This controls whether source/destination checking is enabled on the instance. A value of @true@ means that checking is enabled, and @false@ means that checking is disabled. The value must be @false@ for the instance to perform NAT. For more information, see <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_NAT_Instance.html NAT Instances> in the /Amazon Virtual Private Cloud User Guide/ .
--
-- /Note:/ Consider using 'sourceDestCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifSourceDestCheck :: Lens.Lens' Instance (Lude.Maybe Lude.Bool)
ifSourceDestCheck = Lens.lens (sourceDestCheck :: Instance -> Lude.Maybe Lude.Bool) (\s a -> s {sourceDestCheck = a} :: Instance)
{-# DEPRECATED ifSourceDestCheck "Use generic-lens or generic-optics with 'sourceDestCheck' instead." #-}

-- | The Elastic GPU associated with the instance.
--
-- /Note:/ Consider using 'elasticGpuAssociations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifElasticGpuAssociations :: Lens.Lens' Instance (Lude.Maybe [ElasticGpuAssociation])
ifElasticGpuAssociations = Lens.lens (elasticGpuAssociations :: Instance -> Lude.Maybe [ElasticGpuAssociation]) (\s a -> s {elasticGpuAssociations = a} :: Instance)
{-# DEPRECATED ifElasticGpuAssociations "Use generic-lens or generic-optics with 'elasticGpuAssociations' instead." #-}

-- | [EC2-VPC] The ID of the VPC in which the instance is running.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifVPCId :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
ifVPCId = Lens.lens (vpcId :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: Instance)
{-# DEPRECATED ifVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The name of the key pair, if this instance was launched with an associated key pair.
--
-- /Note:/ Consider using 'keyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifKeyName :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
ifKeyName = Lens.lens (keyName :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {keyName = a} :: Instance)
{-# DEPRECATED ifKeyName "Use generic-lens or generic-optics with 'keyName' instead." #-}

-- | The time the instance was launched.
--
-- /Note:/ Consider using 'launchTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifLaunchTime :: Lens.Lens' Instance Lude.DateTime
ifLaunchTime = Lens.lens (launchTime :: Instance -> Lude.DateTime) (\s a -> s {launchTime = a} :: Instance)
{-# DEPRECATED ifLaunchTime "Use generic-lens or generic-optics with 'launchTime' instead." #-}

-- | [EC2-VPC] The network interfaces for the instance.
--
-- /Note:/ Consider using 'networkInterfaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifNetworkInterfaces :: Lens.Lens' Instance (Lude.Maybe [InstanceNetworkInterface])
ifNetworkInterfaces = Lens.lens (networkInterfaces :: Instance -> Lude.Maybe [InstanceNetworkInterface]) (\s a -> s {networkInterfaces = a} :: Instance)
{-# DEPRECATED ifNetworkInterfaces "Use generic-lens or generic-optics with 'networkInterfaces' instead." #-}

-- | The Amazon Resource Name (ARN) of the Outpost.
--
-- /Note:/ Consider using 'outpostARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifOutpostARN :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
ifOutpostARN = Lens.lens (outpostARN :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {outpostARN = a} :: Instance)
{-# DEPRECATED ifOutpostARN "Use generic-lens or generic-optics with 'outpostARN' instead." #-}

-- | Indicates whether the instance is enabled for AWS Nitro Enclaves.
--
-- /Note:/ Consider using 'enclaveOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifEnclaveOptions :: Lens.Lens' Instance (Lude.Maybe EnclaveOptions)
ifEnclaveOptions = Lens.lens (enclaveOptions :: Instance -> Lude.Maybe EnclaveOptions) (\s a -> s {enclaveOptions = a} :: Instance)
{-# DEPRECATED ifEnclaveOptions "Use generic-lens or generic-optics with 'enclaveOptions' instead." #-}

-- | The RAM disk associated with this instance, if applicable.
--
-- /Note:/ Consider using 'ramdiskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifRAMDiskId :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
ifRAMDiskId = Lens.lens (ramdiskId :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {ramdiskId = a} :: Instance)
{-# DEPRECATED ifRAMDiskId "Use generic-lens or generic-optics with 'ramdiskId' instead." #-}

-- | The CPU options for the instance.
--
-- /Note:/ Consider using 'cpuOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifCPUOptions :: Lens.Lens' Instance (Lude.Maybe CPUOptions)
ifCPUOptions = Lens.lens (cpuOptions :: Instance -> Lude.Maybe CPUOptions) (\s a -> s {cpuOptions = a} :: Instance)
{-# DEPRECATED ifCPUOptions "Use generic-lens or generic-optics with 'cpuOptions' instead." #-}

-- | [EC2-VPC] The ID of the subnet in which the instance is running.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifSubnetId :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
ifSubnetId = Lens.lens (subnetId :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {subnetId = a} :: Instance)
{-# DEPRECATED ifSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The kernel associated with this instance, if applicable.
--
-- /Note:/ Consider using 'kernelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifKernelId :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
ifKernelId = Lens.lens (kernelId :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {kernelId = a} :: Instance)
{-# DEPRECATED ifKernelId "Use generic-lens or generic-optics with 'kernelId' instead." #-}

-- | The device name of the root device volume (for example, @/dev/sda1@ ).
--
-- /Note:/ Consider using 'rootDeviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifRootDeviceName :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
ifRootDeviceName = Lens.lens (rootDeviceName :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {rootDeviceName = a} :: Instance)
{-# DEPRECATED ifRootDeviceName "Use generic-lens or generic-optics with 'rootDeviceName' instead." #-}

-- | The ID of the Capacity Reservation.
--
-- /Note:/ Consider using 'capacityReservationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifCapacityReservationId :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
ifCapacityReservationId = Lens.lens (capacityReservationId :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {capacityReservationId = a} :: Instance)
{-# DEPRECATED ifCapacityReservationId "Use generic-lens or generic-optics with 'capacityReservationId' instead." #-}

-- | The instance type.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifInstanceType :: Lens.Lens' Instance InstanceType
ifInstanceType = Lens.lens (instanceType :: Instance -> InstanceType) (\s a -> s {instanceType = a} :: Instance)
{-# DEPRECATED ifInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | Information about the Capacity Reservation targeting option.
--
-- /Note:/ Consider using 'capacityReservationSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifCapacityReservationSpecification :: Lens.Lens' Instance (Lude.Maybe CapacityReservationSpecificationResponse)
ifCapacityReservationSpecification = Lens.lens (capacityReservationSpecification :: Instance -> Lude.Maybe CapacityReservationSpecificationResponse) (\s a -> s {capacityReservationSpecification = a} :: Instance)
{-# DEPRECATED ifCapacityReservationSpecification "Use generic-lens or generic-optics with 'capacityReservationSpecification' instead." #-}

-- | Specifies whether enhanced networking with the Intel 82599 Virtual Function interface is enabled.
--
-- /Note:/ Consider using 'sriovNetSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifSRIOVNetSupport :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
ifSRIOVNetSupport = Lens.lens (sriovNetSupport :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {sriovNetSupport = a} :: Instance)
{-# DEPRECATED ifSRIOVNetSupport "Use generic-lens or generic-optics with 'sriovNetSupport' instead." #-}

-- | Indicates whether the instance is optimized for Amazon EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS Optimized instance.
--
-- /Note:/ Consider using 'ebsOptimized' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifEBSOptimized :: Lens.Lens' Instance (Lude.Maybe Lude.Bool)
ifEBSOptimized = Lens.lens (ebsOptimized :: Instance -> Lude.Maybe Lude.Bool) (\s a -> s {ebsOptimized = a} :: Instance)
{-# DEPRECATED ifEBSOptimized "Use generic-lens or generic-optics with 'ebsOptimized' instead." #-}

-- | The monitoring for the instance.
--
-- /Note:/ Consider using 'monitoring' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifMonitoring :: Lens.Lens' Instance Monitoring
ifMonitoring = Lens.lens (monitoring :: Instance -> Monitoring) (\s a -> s {monitoring = a} :: Instance)
{-# DEPRECATED ifMonitoring "Use generic-lens or generic-optics with 'monitoring' instead." #-}

-- | The reason for the most recent state transition. This might be an empty string.
--
-- /Note:/ Consider using 'stateTransitionReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifStateTransitionReason :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
ifStateTransitionReason = Lens.lens (stateTransitionReason :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {stateTransitionReason = a} :: Instance)
{-# DEPRECATED ifStateTransitionReason "Use generic-lens or generic-optics with 'stateTransitionReason' instead." #-}

-- | Indicates whether the instance is enabled for hibernation.
--
-- /Note:/ Consider using 'hibernationOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifHibernationOptions :: Lens.Lens' Instance (Lude.Maybe HibernationOptions)
ifHibernationOptions = Lens.lens (hibernationOptions :: Instance -> Lude.Maybe HibernationOptions) (\s a -> s {hibernationOptions = a} :: Instance)
{-# DEPRECATED ifHibernationOptions "Use generic-lens or generic-optics with 'hibernationOptions' instead." #-}

-- | Indicates whether this is a Spot Instance or a Scheduled Instance.
--
-- /Note:/ Consider using 'instanceLifecycle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifInstanceLifecycle :: Lens.Lens' Instance (Lude.Maybe InstanceLifecycleType)
ifInstanceLifecycle = Lens.lens (instanceLifecycle :: Instance -> Lude.Maybe InstanceLifecycleType) (\s a -> s {instanceLifecycle = a} :: Instance)
{-# DEPRECATED ifInstanceLifecycle "Use generic-lens or generic-optics with 'instanceLifecycle' instead." #-}

-- | The IAM instance profile associated with the instance, if applicable.
--
-- /Note:/ Consider using 'iamInstanceProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifIAMInstanceProfile :: Lens.Lens' Instance (Lude.Maybe IAMInstanceProfile)
ifIAMInstanceProfile = Lens.lens (iamInstanceProfile :: Instance -> Lude.Maybe IAMInstanceProfile) (\s a -> s {iamInstanceProfile = a} :: Instance)
{-# DEPRECATED ifIAMInstanceProfile "Use generic-lens or generic-optics with 'iamInstanceProfile' instead." #-}

-- | The ID of the AMI used to launch the instance.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifImageId :: Lens.Lens' Instance Lude.Text
ifImageId = Lens.lens (imageId :: Instance -> Lude.Text) (\s a -> s {imageId = a} :: Instance)
{-# DEPRECATED ifImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | The private IPv4 address assigned to the instance.
--
-- /Note:/ Consider using 'privateIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifPrivateIPAddress :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
ifPrivateIPAddress = Lens.lens (privateIPAddress :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {privateIPAddress = a} :: Instance)
{-# DEPRECATED ifPrivateIPAddress "Use generic-lens or generic-optics with 'privateIPAddress' instead." #-}

-- | The metadata options for the instance.
--
-- /Note:/ Consider using 'metadataOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifMetadataOptions :: Lens.Lens' Instance (Lude.Maybe InstanceMetadataOptionsResponse)
ifMetadataOptions = Lens.lens (metadataOptions :: Instance -> Lude.Maybe InstanceMetadataOptionsResponse) (\s a -> s {metadataOptions = a} :: Instance)
{-# DEPRECATED ifMetadataOptions "Use generic-lens or generic-optics with 'metadataOptions' instead." #-}

-- | The architecture of the image.
--
-- /Note:/ Consider using 'architecture' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifArchitecture :: Lens.Lens' Instance ArchitectureValues
ifArchitecture = Lens.lens (architecture :: Instance -> ArchitectureValues) (\s a -> s {architecture = a} :: Instance)
{-# DEPRECATED ifArchitecture "Use generic-lens or generic-optics with 'architecture' instead." #-}

-- | The product codes attached to this instance, if applicable.
--
-- /Note:/ Consider using 'productCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifProductCodes :: Lens.Lens' Instance (Lude.Maybe [ProductCode])
ifProductCodes = Lens.lens (productCodes :: Instance -> Lude.Maybe [ProductCode]) (\s a -> s {productCodes = a} :: Instance)
{-# DEPRECATED ifProductCodes "Use generic-lens or generic-optics with 'productCodes' instead." #-}

-- | If the request is a Spot Instance request, the ID of the request.
--
-- /Note:/ Consider using 'spotInstanceRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifSpotInstanceRequestId :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
ifSpotInstanceRequestId = Lens.lens (spotInstanceRequestId :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {spotInstanceRequestId = a} :: Instance)
{-# DEPRECATED ifSpotInstanceRequestId "Use generic-lens or generic-optics with 'spotInstanceRequestId' instead." #-}

-- | The license configurations.
--
-- /Note:/ Consider using 'licenses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifLicenses :: Lens.Lens' Instance (Lude.Maybe [LicenseConfiguration])
ifLicenses = Lens.lens (licenses :: Instance -> Lude.Maybe [LicenseConfiguration]) (\s a -> s {licenses = a} :: Instance)
{-# DEPRECATED ifLicenses "Use generic-lens or generic-optics with 'licenses' instead." #-}

-- | The elastic inference accelerator associated with the instance.
--
-- /Note:/ Consider using 'elasticInferenceAcceleratorAssociations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifElasticInferenceAcceleratorAssociations :: Lens.Lens' Instance (Lude.Maybe [ElasticInferenceAcceleratorAssociation])
ifElasticInferenceAcceleratorAssociations = Lens.lens (elasticInferenceAcceleratorAssociations :: Instance -> Lude.Maybe [ElasticInferenceAcceleratorAssociation]) (\s a -> s {elasticInferenceAcceleratorAssociations = a} :: Instance)
{-# DEPRECATED ifElasticInferenceAcceleratorAssociations "Use generic-lens or generic-optics with 'elasticInferenceAcceleratorAssociations' instead." #-}

-- | (IPv4 only) The private DNS hostname name assigned to the instance. This DNS hostname can only be used inside the Amazon EC2 network. This name is not available until the instance enters the @running@ state.
--
-- [EC2-VPC] The Amazon-provided DNS server resolves Amazon-provided private DNS hostnames if you've enabled DNS resolution and DNS hostnames in your VPC. If you are not using the Amazon-provided DNS server in your VPC, your custom domain name servers must resolve the hostname as appropriate.
--
-- /Note:/ Consider using 'privateDNSName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifPrivateDNSName :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
ifPrivateDNSName = Lens.lens (privateDNSName :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {privateDNSName = a} :: Instance)
{-# DEPRECATED ifPrivateDNSName "Use generic-lens or generic-optics with 'privateDNSName' instead." #-}

-- | The reason for the most recent state transition.
--
-- /Note:/ Consider using 'stateReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifStateReason :: Lens.Lens' Instance (Lude.Maybe StateReason)
ifStateReason = Lens.lens (stateReason :: Instance -> Lude.Maybe StateReason) (\s a -> s {stateReason = a} :: Instance)
{-# DEPRECATED ifStateReason "Use generic-lens or generic-optics with 'stateReason' instead." #-}

-- | The root device type used by the AMI. The AMI can use an EBS volume or an instance store volume.
--
-- /Note:/ Consider using 'rootDeviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifRootDeviceType :: Lens.Lens' Instance DeviceType
ifRootDeviceType = Lens.lens (rootDeviceType :: Instance -> DeviceType) (\s a -> s {rootDeviceType = a} :: Instance)
{-# DEPRECATED ifRootDeviceType "Use generic-lens or generic-optics with 'rootDeviceType' instead." #-}

-- | Any block device mapping entries for the instance.
--
-- /Note:/ Consider using 'blockDeviceMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifBlockDeviceMappings :: Lens.Lens' Instance (Lude.Maybe [InstanceBlockDeviceMapping])
ifBlockDeviceMappings = Lens.lens (blockDeviceMappings :: Instance -> Lude.Maybe [InstanceBlockDeviceMapping]) (\s a -> s {blockDeviceMappings = a} :: Instance)
{-# DEPRECATED ifBlockDeviceMappings "Use generic-lens or generic-optics with 'blockDeviceMappings' instead." #-}

-- | The AMI launch index, which can be used to find this instance in the launch group.
--
-- /Note:/ Consider using 'amiLaunchIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifAMILaunchIndex :: Lens.Lens' Instance Lude.Int
ifAMILaunchIndex = Lens.lens (amiLaunchIndex :: Instance -> Lude.Int) (\s a -> s {amiLaunchIndex = a} :: Instance)
{-# DEPRECATED ifAMILaunchIndex "Use generic-lens or generic-optics with 'amiLaunchIndex' instead." #-}

-- | The public IPv4 address, or the Carrier IP address assigned to the instance, if applicable.
--
-- A Carrier IP address only applies to an instance launched in a subnet associated with a Wavelength Zone.
--
-- /Note:/ Consider using 'publicIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifPublicIPAddress :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
ifPublicIPAddress = Lens.lens (publicIPAddress :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {publicIPAddress = a} :: Instance)
{-# DEPRECATED ifPublicIPAddress "Use generic-lens or generic-optics with 'publicIPAddress' instead." #-}

-- | The location where the instance launched, if applicable.
--
-- /Note:/ Consider using 'placement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifPlacement :: Lens.Lens' Instance Placement
ifPlacement = Lens.lens (placement :: Instance -> Placement) (\s a -> s {placement = a} :: Instance)
{-# DEPRECATED ifPlacement "Use generic-lens or generic-optics with 'placement' instead." #-}

-- | Any tags assigned to the instance.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifTags :: Lens.Lens' Instance (Lude.Maybe [Tag])
ifTags = Lens.lens (tags :: Instance -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: Instance)
{-# DEPRECATED ifTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML Instance where
  parseXML x =
    Instance'
      Lude.<$> (x Lude..@ "instanceId")
      Lude.<*> (x Lude..@ "instanceState")
      Lude.<*> (x Lude..@ "virtualizationType")
      Lude.<*> (x Lude..@? "dnsName")
      Lude.<*> (x Lude..@ "hypervisor")
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
      Lude.<*> (x Lude..@ "launchTime")
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
      Lude.<*> (x Lude..@ "instanceType")
      Lude.<*> (x Lude..@? "capacityReservationSpecification")
      Lude.<*> (x Lude..@? "sriovNetSupport")
      Lude.<*> (x Lude..@? "ebsOptimized")
      Lude.<*> (x Lude..@ "monitoring")
      Lude.<*> (x Lude..@? "reason")
      Lude.<*> (x Lude..@? "hibernationOptions")
      Lude.<*> (x Lude..@? "instanceLifecycle")
      Lude.<*> (x Lude..@? "iamInstanceProfile")
      Lude.<*> (x Lude..@ "imageId")
      Lude.<*> (x Lude..@? "privateIpAddress")
      Lude.<*> (x Lude..@? "metadataOptions")
      Lude.<*> (x Lude..@ "architecture")
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
      Lude.<*> (x Lude..@ "rootDeviceType")
      Lude.<*> ( x Lude..@? "blockDeviceMapping" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@ "amiLaunchIndex")
      Lude.<*> (x Lude..@? "ipAddress")
      Lude.<*> (x Lude..@ "placement")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
