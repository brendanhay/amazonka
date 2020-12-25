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
    iAmiLaunchIndex,
    iArchitecture,
    iBlockDeviceMappings,
    iCapacityReservationId,
    iCapacityReservationSpecification,
    iClientToken,
    iCpuOptions,
    iEbsOptimized,
    iElasticGpuAssociations,
    iElasticInferenceAcceleratorAssociations,
    iEnaSupport,
    iEnclaveOptions,
    iHibernationOptions,
    iHypervisor,
    iIamInstanceProfile,
    iImageId,
    iInstanceId,
    iInstanceLifecycle,
    iInstanceType,
    iKernelId,
    iKeyName,
    iLaunchTime,
    iLicenses,
    iMetadataOptions,
    iMonitoring,
    iNetworkInterfaces,
    iOutpostArn,
    iPlacement,
    iPlatform,
    iPrivateDnsName,
    iPrivateIpAddress,
    iProductCodes,
    iPublicDnsName,
    iPublicIpAddress,
    iRamdiskId,
    iRootDeviceName,
    iRootDeviceType,
    iSecurityGroups,
    iSourceDestCheck,
    iSpotInstanceRequestId,
    iSriovNetSupport,
    iState,
    iStateReason,
    iStateTransitionReason,
    iSubnetId,
    iTags,
    iVirtualizationType,
    iVpcId,
  )
where

import qualified Network.AWS.EC2.Types.ArchitectureValues as Types
import qualified Network.AWS.EC2.Types.CapacityReservationSpecificationResponse as Types
import qualified Network.AWS.EC2.Types.CpuOptions as Types
import qualified Network.AWS.EC2.Types.DeviceType as Types
import qualified Network.AWS.EC2.Types.ElasticGpuAssociation as Types
import qualified Network.AWS.EC2.Types.ElasticInferenceAcceleratorAssociation as Types
import qualified Network.AWS.EC2.Types.EnclaveOptions as Types
import qualified Network.AWS.EC2.Types.GroupIdentifier as Types
import qualified Network.AWS.EC2.Types.HibernationOptions as Types
import qualified Network.AWS.EC2.Types.HypervisorType as Types
import qualified Network.AWS.EC2.Types.IamInstanceProfile as Types
import qualified Network.AWS.EC2.Types.InstanceBlockDeviceMapping as Types
import qualified Network.AWS.EC2.Types.InstanceLifecycleType as Types
import qualified Network.AWS.EC2.Types.InstanceMetadataOptionsResponse as Types
import qualified Network.AWS.EC2.Types.InstanceNetworkInterface as Types
import qualified Network.AWS.EC2.Types.InstanceState as Types
import qualified Network.AWS.EC2.Types.InstanceType as Types
import qualified Network.AWS.EC2.Types.LicenseConfiguration as Types
import qualified Network.AWS.EC2.Types.Monitoring as Types
import qualified Network.AWS.EC2.Types.Placement as Types
import qualified Network.AWS.EC2.Types.PlatformValues as Types
import qualified Network.AWS.EC2.Types.ProductCode as Types
import qualified Network.AWS.EC2.Types.StateReason as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.EC2.Types.VirtualizationType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an instance.
--
-- /See:/ 'mkInstance' smart constructor.
data Instance = Instance'
  { -- | The AMI launch index, which can be used to find this instance in the launch group.
    amiLaunchIndex :: Core.Int,
    -- | The architecture of the image.
    architecture :: Types.ArchitectureValues,
    -- | Any block device mapping entries for the instance.
    blockDeviceMappings :: Core.Maybe [Types.InstanceBlockDeviceMapping],
    -- | The ID of the Capacity Reservation.
    capacityReservationId :: Core.Maybe Types.String,
    -- | Information about the Capacity Reservation targeting option.
    capacityReservationSpecification :: Core.Maybe Types.CapacityReservationSpecificationResponse,
    -- | The idempotency token you provided when you launched the instance, if applicable.
    clientToken :: Core.Maybe Types.String,
    -- | The CPU options for the instance.
    cpuOptions :: Core.Maybe Types.CpuOptions,
    -- | Indicates whether the instance is optimized for Amazon EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS Optimized instance.
    ebsOptimized :: Core.Maybe Core.Bool,
    -- | The Elastic GPU associated with the instance.
    elasticGpuAssociations :: Core.Maybe [Types.ElasticGpuAssociation],
    -- | The elastic inference accelerator associated with the instance.
    elasticInferenceAcceleratorAssociations :: Core.Maybe [Types.ElasticInferenceAcceleratorAssociation],
    -- | Specifies whether enhanced networking with ENA is enabled.
    enaSupport :: Core.Maybe Core.Bool,
    -- | Indicates whether the instance is enabled for AWS Nitro Enclaves.
    enclaveOptions :: Core.Maybe Types.EnclaveOptions,
    -- | Indicates whether the instance is enabled for hibernation.
    hibernationOptions :: Core.Maybe Types.HibernationOptions,
    -- | The hypervisor type of the instance. The value @xen@ is used for both Xen and Nitro hypervisors.
    hypervisor :: Types.HypervisorType,
    -- | The IAM instance profile associated with the instance, if applicable.
    iamInstanceProfile :: Core.Maybe Types.IamInstanceProfile,
    -- | The ID of the AMI used to launch the instance.
    imageId :: Types.String,
    -- | The ID of the instance.
    instanceId :: Types.String,
    -- | Indicates whether this is a Spot Instance or a Scheduled Instance.
    instanceLifecycle :: Core.Maybe Types.InstanceLifecycleType,
    -- | The instance type.
    instanceType :: Types.InstanceType,
    -- | The kernel associated with this instance, if applicable.
    kernelId :: Core.Maybe Types.String,
    -- | The name of the key pair, if this instance was launched with an associated key pair.
    keyName :: Core.Maybe Types.String,
    -- | The time the instance was launched.
    launchTime :: Core.UTCTime,
    -- | The license configurations.
    licenses :: Core.Maybe [Types.LicenseConfiguration],
    -- | The metadata options for the instance.
    metadataOptions :: Core.Maybe Types.InstanceMetadataOptionsResponse,
    -- | The monitoring for the instance.
    monitoring :: Types.Monitoring,
    -- | [EC2-VPC] The network interfaces for the instance.
    networkInterfaces :: Core.Maybe [Types.InstanceNetworkInterface],
    -- | The Amazon Resource Name (ARN) of the Outpost.
    outpostArn :: Core.Maybe Types.String,
    -- | The location where the instance launched, if applicable.
    placement :: Types.Placement,
    -- | The value is @Windows@ for Windows instances; otherwise blank.
    platform :: Core.Maybe Types.PlatformValues,
    -- | (IPv4 only) The private DNS hostname name assigned to the instance. This DNS hostname can only be used inside the Amazon EC2 network. This name is not available until the instance enters the @running@ state.
    --
    -- [EC2-VPC] The Amazon-provided DNS server resolves Amazon-provided private DNS hostnames if you've enabled DNS resolution and DNS hostnames in your VPC. If you are not using the Amazon-provided DNS server in your VPC, your custom domain name servers must resolve the hostname as appropriate.
    privateDnsName :: Core.Maybe Types.String,
    -- | The private IPv4 address assigned to the instance.
    privateIpAddress :: Core.Maybe Types.String,
    -- | The product codes attached to this instance, if applicable.
    productCodes :: Core.Maybe [Types.ProductCode],
    -- | (IPv4 only) The public DNS name assigned to the instance. This name is not available until the instance enters the @running@ state. For EC2-VPC, this name is only available if you've enabled DNS hostnames for your VPC.
    publicDnsName :: Core.Maybe Types.String,
    -- | The public IPv4 address, or the Carrier IP address assigned to the instance, if applicable.
    --
    -- A Carrier IP address only applies to an instance launched in a subnet associated with a Wavelength Zone.
    publicIpAddress :: Core.Maybe Types.String,
    -- | The RAM disk associated with this instance, if applicable.
    ramdiskId :: Core.Maybe Types.String,
    -- | The device name of the root device volume (for example, @/dev/sda1@ ).
    rootDeviceName :: Core.Maybe Types.String,
    -- | The root device type used by the AMI. The AMI can use an EBS volume or an instance store volume.
    rootDeviceType :: Types.DeviceType,
    -- | The security groups for the instance.
    securityGroups :: Core.Maybe [Types.GroupIdentifier],
    -- | Specifies whether to enable an instance launched in a VPC to perform NAT. This controls whether source/destination checking is enabled on the instance. A value of @true@ means that checking is enabled, and @false@ means that checking is disabled. The value must be @false@ for the instance to perform NAT. For more information, see <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_NAT_Instance.html NAT Instances> in the /Amazon Virtual Private Cloud User Guide/ .
    sourceDestCheck :: Core.Maybe Core.Bool,
    -- | If the request is a Spot Instance request, the ID of the request.
    spotInstanceRequestId :: Core.Maybe Types.String,
    -- | Specifies whether enhanced networking with the Intel 82599 Virtual Function interface is enabled.
    sriovNetSupport :: Core.Maybe Types.String,
    -- | The current state of the instance.
    state :: Types.InstanceState,
    -- | The reason for the most recent state transition.
    stateReason :: Core.Maybe Types.StateReason,
    -- | The reason for the most recent state transition. This might be an empty string.
    stateTransitionReason :: Core.Maybe Types.String,
    -- | [EC2-VPC] The ID of the subnet in which the instance is running.
    subnetId :: Core.Maybe Types.String,
    -- | Any tags assigned to the instance.
    tags :: Core.Maybe [Types.Tag],
    -- | The virtualization type of the instance.
    virtualizationType :: Types.VirtualizationType,
    -- | [EC2-VPC] The ID of the VPC in which the instance is running.
    vpcId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Instance' value with any optional fields omitted.
mkInstance ::
  -- | 'amiLaunchIndex'
  Core.Int ->
  -- | 'architecture'
  Types.ArchitectureValues ->
  -- | 'hypervisor'
  Types.HypervisorType ->
  -- | 'imageId'
  Types.String ->
  -- | 'instanceId'
  Types.String ->
  -- | 'instanceType'
  Types.InstanceType ->
  -- | 'launchTime'
  Core.UTCTime ->
  -- | 'monitoring'
  Types.Monitoring ->
  -- | 'placement'
  Types.Placement ->
  -- | 'rootDeviceType'
  Types.DeviceType ->
  -- | 'state'
  Types.InstanceState ->
  -- | 'virtualizationType'
  Types.VirtualizationType ->
  Instance
mkInstance
  amiLaunchIndex
  architecture
  hypervisor
  imageId
  instanceId
  instanceType
  launchTime
  monitoring
  placement
  rootDeviceType
  state
  virtualizationType =
    Instance'
      { amiLaunchIndex,
        architecture,
        blockDeviceMappings = Core.Nothing,
        capacityReservationId = Core.Nothing,
        capacityReservationSpecification = Core.Nothing,
        clientToken = Core.Nothing,
        cpuOptions = Core.Nothing,
        ebsOptimized = Core.Nothing,
        elasticGpuAssociations = Core.Nothing,
        elasticInferenceAcceleratorAssociations = Core.Nothing,
        enaSupport = Core.Nothing,
        enclaveOptions = Core.Nothing,
        hibernationOptions = Core.Nothing,
        hypervisor,
        iamInstanceProfile = Core.Nothing,
        imageId,
        instanceId,
        instanceLifecycle = Core.Nothing,
        instanceType,
        kernelId = Core.Nothing,
        keyName = Core.Nothing,
        launchTime,
        licenses = Core.Nothing,
        metadataOptions = Core.Nothing,
        monitoring,
        networkInterfaces = Core.Nothing,
        outpostArn = Core.Nothing,
        placement,
        platform = Core.Nothing,
        privateDnsName = Core.Nothing,
        privateIpAddress = Core.Nothing,
        productCodes = Core.Nothing,
        publicDnsName = Core.Nothing,
        publicIpAddress = Core.Nothing,
        ramdiskId = Core.Nothing,
        rootDeviceName = Core.Nothing,
        rootDeviceType,
        securityGroups = Core.Nothing,
        sourceDestCheck = Core.Nothing,
        spotInstanceRequestId = Core.Nothing,
        sriovNetSupport = Core.Nothing,
        state,
        stateReason = Core.Nothing,
        stateTransitionReason = Core.Nothing,
        subnetId = Core.Nothing,
        tags = Core.Nothing,
        virtualizationType,
        vpcId = Core.Nothing
      }

-- | The AMI launch index, which can be used to find this instance in the launch group.
--
-- /Note:/ Consider using 'amiLaunchIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iAmiLaunchIndex :: Lens.Lens' Instance Core.Int
iAmiLaunchIndex = Lens.field @"amiLaunchIndex"
{-# DEPRECATED iAmiLaunchIndex "Use generic-lens or generic-optics with 'amiLaunchIndex' instead." #-}

-- | The architecture of the image.
--
-- /Note:/ Consider using 'architecture' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iArchitecture :: Lens.Lens' Instance Types.ArchitectureValues
iArchitecture = Lens.field @"architecture"
{-# DEPRECATED iArchitecture "Use generic-lens or generic-optics with 'architecture' instead." #-}

-- | Any block device mapping entries for the instance.
--
-- /Note:/ Consider using 'blockDeviceMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iBlockDeviceMappings :: Lens.Lens' Instance (Core.Maybe [Types.InstanceBlockDeviceMapping])
iBlockDeviceMappings = Lens.field @"blockDeviceMappings"
{-# DEPRECATED iBlockDeviceMappings "Use generic-lens or generic-optics with 'blockDeviceMappings' instead." #-}

-- | The ID of the Capacity Reservation.
--
-- /Note:/ Consider using 'capacityReservationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iCapacityReservationId :: Lens.Lens' Instance (Core.Maybe Types.String)
iCapacityReservationId = Lens.field @"capacityReservationId"
{-# DEPRECATED iCapacityReservationId "Use generic-lens or generic-optics with 'capacityReservationId' instead." #-}

-- | Information about the Capacity Reservation targeting option.
--
-- /Note:/ Consider using 'capacityReservationSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iCapacityReservationSpecification :: Lens.Lens' Instance (Core.Maybe Types.CapacityReservationSpecificationResponse)
iCapacityReservationSpecification = Lens.field @"capacityReservationSpecification"
{-# DEPRECATED iCapacityReservationSpecification "Use generic-lens or generic-optics with 'capacityReservationSpecification' instead." #-}

-- | The idempotency token you provided when you launched the instance, if applicable.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iClientToken :: Lens.Lens' Instance (Core.Maybe Types.String)
iClientToken = Lens.field @"clientToken"
{-# DEPRECATED iClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The CPU options for the instance.
--
-- /Note:/ Consider using 'cpuOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iCpuOptions :: Lens.Lens' Instance (Core.Maybe Types.CpuOptions)
iCpuOptions = Lens.field @"cpuOptions"
{-# DEPRECATED iCpuOptions "Use generic-lens or generic-optics with 'cpuOptions' instead." #-}

-- | Indicates whether the instance is optimized for Amazon EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS Optimized instance.
--
-- /Note:/ Consider using 'ebsOptimized' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iEbsOptimized :: Lens.Lens' Instance (Core.Maybe Core.Bool)
iEbsOptimized = Lens.field @"ebsOptimized"
{-# DEPRECATED iEbsOptimized "Use generic-lens or generic-optics with 'ebsOptimized' instead." #-}

-- | The Elastic GPU associated with the instance.
--
-- /Note:/ Consider using 'elasticGpuAssociations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iElasticGpuAssociations :: Lens.Lens' Instance (Core.Maybe [Types.ElasticGpuAssociation])
iElasticGpuAssociations = Lens.field @"elasticGpuAssociations"
{-# DEPRECATED iElasticGpuAssociations "Use generic-lens or generic-optics with 'elasticGpuAssociations' instead." #-}

-- | The elastic inference accelerator associated with the instance.
--
-- /Note:/ Consider using 'elasticInferenceAcceleratorAssociations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iElasticInferenceAcceleratorAssociations :: Lens.Lens' Instance (Core.Maybe [Types.ElasticInferenceAcceleratorAssociation])
iElasticInferenceAcceleratorAssociations = Lens.field @"elasticInferenceAcceleratorAssociations"
{-# DEPRECATED iElasticInferenceAcceleratorAssociations "Use generic-lens or generic-optics with 'elasticInferenceAcceleratorAssociations' instead." #-}

-- | Specifies whether enhanced networking with ENA is enabled.
--
-- /Note:/ Consider using 'enaSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iEnaSupport :: Lens.Lens' Instance (Core.Maybe Core.Bool)
iEnaSupport = Lens.field @"enaSupport"
{-# DEPRECATED iEnaSupport "Use generic-lens or generic-optics with 'enaSupport' instead." #-}

-- | Indicates whether the instance is enabled for AWS Nitro Enclaves.
--
-- /Note:/ Consider using 'enclaveOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iEnclaveOptions :: Lens.Lens' Instance (Core.Maybe Types.EnclaveOptions)
iEnclaveOptions = Lens.field @"enclaveOptions"
{-# DEPRECATED iEnclaveOptions "Use generic-lens or generic-optics with 'enclaveOptions' instead." #-}

-- | Indicates whether the instance is enabled for hibernation.
--
-- /Note:/ Consider using 'hibernationOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iHibernationOptions :: Lens.Lens' Instance (Core.Maybe Types.HibernationOptions)
iHibernationOptions = Lens.field @"hibernationOptions"
{-# DEPRECATED iHibernationOptions "Use generic-lens or generic-optics with 'hibernationOptions' instead." #-}

-- | The hypervisor type of the instance. The value @xen@ is used for both Xen and Nitro hypervisors.
--
-- /Note:/ Consider using 'hypervisor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iHypervisor :: Lens.Lens' Instance Types.HypervisorType
iHypervisor = Lens.field @"hypervisor"
{-# DEPRECATED iHypervisor "Use generic-lens or generic-optics with 'hypervisor' instead." #-}

-- | The IAM instance profile associated with the instance, if applicable.
--
-- /Note:/ Consider using 'iamInstanceProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iIamInstanceProfile :: Lens.Lens' Instance (Core.Maybe Types.IamInstanceProfile)
iIamInstanceProfile = Lens.field @"iamInstanceProfile"
{-# DEPRECATED iIamInstanceProfile "Use generic-lens or generic-optics with 'iamInstanceProfile' instead." #-}

-- | The ID of the AMI used to launch the instance.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iImageId :: Lens.Lens' Instance Types.String
iImageId = Lens.field @"imageId"
{-# DEPRECATED iImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInstanceId :: Lens.Lens' Instance Types.String
iInstanceId = Lens.field @"instanceId"
{-# DEPRECATED iInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | Indicates whether this is a Spot Instance or a Scheduled Instance.
--
-- /Note:/ Consider using 'instanceLifecycle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInstanceLifecycle :: Lens.Lens' Instance (Core.Maybe Types.InstanceLifecycleType)
iInstanceLifecycle = Lens.field @"instanceLifecycle"
{-# DEPRECATED iInstanceLifecycle "Use generic-lens or generic-optics with 'instanceLifecycle' instead." #-}

-- | The instance type.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInstanceType :: Lens.Lens' Instance Types.InstanceType
iInstanceType = Lens.field @"instanceType"
{-# DEPRECATED iInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The kernel associated with this instance, if applicable.
--
-- /Note:/ Consider using 'kernelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iKernelId :: Lens.Lens' Instance (Core.Maybe Types.String)
iKernelId = Lens.field @"kernelId"
{-# DEPRECATED iKernelId "Use generic-lens or generic-optics with 'kernelId' instead." #-}

-- | The name of the key pair, if this instance was launched with an associated key pair.
--
-- /Note:/ Consider using 'keyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iKeyName :: Lens.Lens' Instance (Core.Maybe Types.String)
iKeyName = Lens.field @"keyName"
{-# DEPRECATED iKeyName "Use generic-lens or generic-optics with 'keyName' instead." #-}

-- | The time the instance was launched.
--
-- /Note:/ Consider using 'launchTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iLaunchTime :: Lens.Lens' Instance Core.UTCTime
iLaunchTime = Lens.field @"launchTime"
{-# DEPRECATED iLaunchTime "Use generic-lens or generic-optics with 'launchTime' instead." #-}

-- | The license configurations.
--
-- /Note:/ Consider using 'licenses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iLicenses :: Lens.Lens' Instance (Core.Maybe [Types.LicenseConfiguration])
iLicenses = Lens.field @"licenses"
{-# DEPRECATED iLicenses "Use generic-lens or generic-optics with 'licenses' instead." #-}

-- | The metadata options for the instance.
--
-- /Note:/ Consider using 'metadataOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iMetadataOptions :: Lens.Lens' Instance (Core.Maybe Types.InstanceMetadataOptionsResponse)
iMetadataOptions = Lens.field @"metadataOptions"
{-# DEPRECATED iMetadataOptions "Use generic-lens or generic-optics with 'metadataOptions' instead." #-}

-- | The monitoring for the instance.
--
-- /Note:/ Consider using 'monitoring' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iMonitoring :: Lens.Lens' Instance Types.Monitoring
iMonitoring = Lens.field @"monitoring"
{-# DEPRECATED iMonitoring "Use generic-lens or generic-optics with 'monitoring' instead." #-}

-- | [EC2-VPC] The network interfaces for the instance.
--
-- /Note:/ Consider using 'networkInterfaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iNetworkInterfaces :: Lens.Lens' Instance (Core.Maybe [Types.InstanceNetworkInterface])
iNetworkInterfaces = Lens.field @"networkInterfaces"
{-# DEPRECATED iNetworkInterfaces "Use generic-lens or generic-optics with 'networkInterfaces' instead." #-}

-- | The Amazon Resource Name (ARN) of the Outpost.
--
-- /Note:/ Consider using 'outpostArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iOutpostArn :: Lens.Lens' Instance (Core.Maybe Types.String)
iOutpostArn = Lens.field @"outpostArn"
{-# DEPRECATED iOutpostArn "Use generic-lens or generic-optics with 'outpostArn' instead." #-}

-- | The location where the instance launched, if applicable.
--
-- /Note:/ Consider using 'placement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPlacement :: Lens.Lens' Instance Types.Placement
iPlacement = Lens.field @"placement"
{-# DEPRECATED iPlacement "Use generic-lens or generic-optics with 'placement' instead." #-}

-- | The value is @Windows@ for Windows instances; otherwise blank.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPlatform :: Lens.Lens' Instance (Core.Maybe Types.PlatformValues)
iPlatform = Lens.field @"platform"
{-# DEPRECATED iPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | (IPv4 only) The private DNS hostname name assigned to the instance. This DNS hostname can only be used inside the Amazon EC2 network. This name is not available until the instance enters the @running@ state.
--
-- [EC2-VPC] The Amazon-provided DNS server resolves Amazon-provided private DNS hostnames if you've enabled DNS resolution and DNS hostnames in your VPC. If you are not using the Amazon-provided DNS server in your VPC, your custom domain name servers must resolve the hostname as appropriate.
--
-- /Note:/ Consider using 'privateDnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPrivateDnsName :: Lens.Lens' Instance (Core.Maybe Types.String)
iPrivateDnsName = Lens.field @"privateDnsName"
{-# DEPRECATED iPrivateDnsName "Use generic-lens or generic-optics with 'privateDnsName' instead." #-}

-- | The private IPv4 address assigned to the instance.
--
-- /Note:/ Consider using 'privateIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPrivateIpAddress :: Lens.Lens' Instance (Core.Maybe Types.String)
iPrivateIpAddress = Lens.field @"privateIpAddress"
{-# DEPRECATED iPrivateIpAddress "Use generic-lens or generic-optics with 'privateIpAddress' instead." #-}

-- | The product codes attached to this instance, if applicable.
--
-- /Note:/ Consider using 'productCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iProductCodes :: Lens.Lens' Instance (Core.Maybe [Types.ProductCode])
iProductCodes = Lens.field @"productCodes"
{-# DEPRECATED iProductCodes "Use generic-lens or generic-optics with 'productCodes' instead." #-}

-- | (IPv4 only) The public DNS name assigned to the instance. This name is not available until the instance enters the @running@ state. For EC2-VPC, this name is only available if you've enabled DNS hostnames for your VPC.
--
-- /Note:/ Consider using 'publicDnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPublicDnsName :: Lens.Lens' Instance (Core.Maybe Types.String)
iPublicDnsName = Lens.field @"publicDnsName"
{-# DEPRECATED iPublicDnsName "Use generic-lens or generic-optics with 'publicDnsName' instead." #-}

-- | The public IPv4 address, or the Carrier IP address assigned to the instance, if applicable.
--
-- A Carrier IP address only applies to an instance launched in a subnet associated with a Wavelength Zone.
--
-- /Note:/ Consider using 'publicIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPublicIpAddress :: Lens.Lens' Instance (Core.Maybe Types.String)
iPublicIpAddress = Lens.field @"publicIpAddress"
{-# DEPRECATED iPublicIpAddress "Use generic-lens or generic-optics with 'publicIpAddress' instead." #-}

-- | The RAM disk associated with this instance, if applicable.
--
-- /Note:/ Consider using 'ramdiskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iRamdiskId :: Lens.Lens' Instance (Core.Maybe Types.String)
iRamdiskId = Lens.field @"ramdiskId"
{-# DEPRECATED iRamdiskId "Use generic-lens or generic-optics with 'ramdiskId' instead." #-}

-- | The device name of the root device volume (for example, @/dev/sda1@ ).
--
-- /Note:/ Consider using 'rootDeviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iRootDeviceName :: Lens.Lens' Instance (Core.Maybe Types.String)
iRootDeviceName = Lens.field @"rootDeviceName"
{-# DEPRECATED iRootDeviceName "Use generic-lens or generic-optics with 'rootDeviceName' instead." #-}

-- | The root device type used by the AMI. The AMI can use an EBS volume or an instance store volume.
--
-- /Note:/ Consider using 'rootDeviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iRootDeviceType :: Lens.Lens' Instance Types.DeviceType
iRootDeviceType = Lens.field @"rootDeviceType"
{-# DEPRECATED iRootDeviceType "Use generic-lens or generic-optics with 'rootDeviceType' instead." #-}

-- | The security groups for the instance.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iSecurityGroups :: Lens.Lens' Instance (Core.Maybe [Types.GroupIdentifier])
iSecurityGroups = Lens.field @"securityGroups"
{-# DEPRECATED iSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | Specifies whether to enable an instance launched in a VPC to perform NAT. This controls whether source/destination checking is enabled on the instance. A value of @true@ means that checking is enabled, and @false@ means that checking is disabled. The value must be @false@ for the instance to perform NAT. For more information, see <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_NAT_Instance.html NAT Instances> in the /Amazon Virtual Private Cloud User Guide/ .
--
-- /Note:/ Consider using 'sourceDestCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iSourceDestCheck :: Lens.Lens' Instance (Core.Maybe Core.Bool)
iSourceDestCheck = Lens.field @"sourceDestCheck"
{-# DEPRECATED iSourceDestCheck "Use generic-lens or generic-optics with 'sourceDestCheck' instead." #-}

-- | If the request is a Spot Instance request, the ID of the request.
--
-- /Note:/ Consider using 'spotInstanceRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iSpotInstanceRequestId :: Lens.Lens' Instance (Core.Maybe Types.String)
iSpotInstanceRequestId = Lens.field @"spotInstanceRequestId"
{-# DEPRECATED iSpotInstanceRequestId "Use generic-lens or generic-optics with 'spotInstanceRequestId' instead." #-}

-- | Specifies whether enhanced networking with the Intel 82599 Virtual Function interface is enabled.
--
-- /Note:/ Consider using 'sriovNetSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iSriovNetSupport :: Lens.Lens' Instance (Core.Maybe Types.String)
iSriovNetSupport = Lens.field @"sriovNetSupport"
{-# DEPRECATED iSriovNetSupport "Use generic-lens or generic-optics with 'sriovNetSupport' instead." #-}

-- | The current state of the instance.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iState :: Lens.Lens' Instance Types.InstanceState
iState = Lens.field @"state"
{-# DEPRECATED iState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The reason for the most recent state transition.
--
-- /Note:/ Consider using 'stateReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iStateReason :: Lens.Lens' Instance (Core.Maybe Types.StateReason)
iStateReason = Lens.field @"stateReason"
{-# DEPRECATED iStateReason "Use generic-lens or generic-optics with 'stateReason' instead." #-}

-- | The reason for the most recent state transition. This might be an empty string.
--
-- /Note:/ Consider using 'stateTransitionReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iStateTransitionReason :: Lens.Lens' Instance (Core.Maybe Types.String)
iStateTransitionReason = Lens.field @"stateTransitionReason"
{-# DEPRECATED iStateTransitionReason "Use generic-lens or generic-optics with 'stateTransitionReason' instead." #-}

-- | [EC2-VPC] The ID of the subnet in which the instance is running.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iSubnetId :: Lens.Lens' Instance (Core.Maybe Types.String)
iSubnetId = Lens.field @"subnetId"
{-# DEPRECATED iSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | Any tags assigned to the instance.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iTags :: Lens.Lens' Instance (Core.Maybe [Types.Tag])
iTags = Lens.field @"tags"
{-# DEPRECATED iTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The virtualization type of the instance.
--
-- /Note:/ Consider using 'virtualizationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iVirtualizationType :: Lens.Lens' Instance Types.VirtualizationType
iVirtualizationType = Lens.field @"virtualizationType"
{-# DEPRECATED iVirtualizationType "Use generic-lens or generic-optics with 'virtualizationType' instead." #-}

-- | [EC2-VPC] The ID of the VPC in which the instance is running.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iVpcId :: Lens.Lens' Instance (Core.Maybe Types.String)
iVpcId = Lens.field @"vpcId"
{-# DEPRECATED iVpcId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

instance Core.FromXML Instance where
  parseXML x =
    Instance'
      Core.<$> (x Core..@ "amiLaunchIndex")
      Core.<*> (x Core..@ "architecture")
      Core.<*> ( x Core..@? "blockDeviceMapping"
                   Core..<@> Core.parseXMLList "item"
               )
      Core.<*> (x Core..@? "capacityReservationId")
      Core.<*> (x Core..@? "capacityReservationSpecification")
      Core.<*> (x Core..@? "clientToken")
      Core.<*> (x Core..@? "cpuOptions")
      Core.<*> (x Core..@? "ebsOptimized")
      Core.<*> ( x Core..@? "elasticGpuAssociationSet"
                   Core..<@> Core.parseXMLList "item"
               )
      Core.<*> ( x Core..@? "elasticInferenceAcceleratorAssociationSet"
                   Core..<@> Core.parseXMLList "item"
               )
      Core.<*> (x Core..@? "enaSupport")
      Core.<*> (x Core..@? "enclaveOptions")
      Core.<*> (x Core..@? "hibernationOptions")
      Core.<*> (x Core..@ "hypervisor")
      Core.<*> (x Core..@? "iamInstanceProfile")
      Core.<*> (x Core..@ "imageId")
      Core.<*> (x Core..@ "instanceId")
      Core.<*> (x Core..@? "instanceLifecycle")
      Core.<*> (x Core..@ "instanceType")
      Core.<*> (x Core..@? "kernelId")
      Core.<*> (x Core..@? "keyName")
      Core.<*> (x Core..@ "launchTime")
      Core.<*> (x Core..@? "licenseSet" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "metadataOptions")
      Core.<*> (x Core..@ "monitoring")
      Core.<*> ( x Core..@? "networkInterfaceSet"
                   Core..<@> Core.parseXMLList "item"
               )
      Core.<*> (x Core..@? "outpostArn")
      Core.<*> (x Core..@ "placement")
      Core.<*> (x Core..@? "platform")
      Core.<*> (x Core..@? "privateDnsName")
      Core.<*> (x Core..@? "privateIpAddress")
      Core.<*> (x Core..@? "productCodes" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "dnsName")
      Core.<*> (x Core..@? "ipAddress")
      Core.<*> (x Core..@? "ramdiskId")
      Core.<*> (x Core..@? "rootDeviceName")
      Core.<*> (x Core..@ "rootDeviceType")
      Core.<*> (x Core..@? "groupSet" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "sourceDestCheck")
      Core.<*> (x Core..@? "spotInstanceRequestId")
      Core.<*> (x Core..@? "sriovNetSupport")
      Core.<*> (x Core..@ "instanceState")
      Core.<*> (x Core..@? "stateReason")
      Core.<*> (x Core..@? "reason")
      Core.<*> (x Core..@? "subnetId")
      Core.<*> (x Core..@? "tagSet" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@ "virtualizationType")
      Core.<*> (x Core..@? "vpcId")
