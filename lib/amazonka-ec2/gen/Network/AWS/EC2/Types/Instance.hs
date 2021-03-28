{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Instance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.Instance
  ( Instance (..)
  -- * Smart constructor
  , mkInstance
  -- * Lenses
  , iAmiLaunchIndex
  , iArchitecture
  , iBlockDeviceMappings
  , iCapacityReservationId
  , iCapacityReservationSpecification
  , iClientToken
  , iCpuOptions
  , iEbsOptimized
  , iElasticGpuAssociations
  , iElasticInferenceAcceleratorAssociations
  , iEnaSupport
  , iEnclaveOptions
  , iHibernationOptions
  , iHypervisor
  , iIamInstanceProfile
  , iImageId
  , iInstanceId
  , iInstanceLifecycle
  , iInstanceType
  , iKernelId
  , iKeyName
  , iLaunchTime
  , iLicenses
  , iMetadataOptions
  , iMonitoring
  , iNetworkInterfaces
  , iOutpostArn
  , iPlacement
  , iPlatform
  , iPrivateDnsName
  , iPrivateIpAddress
  , iProductCodes
  , iPublicDnsName
  , iPublicIpAddress
  , iRamdiskId
  , iRootDeviceName
  , iRootDeviceType
  , iSecurityGroups
  , iSourceDestCheck
  , iSpotInstanceRequestId
  , iSriovNetSupport
  , iState
  , iStateReason
  , iStateTransitionReason
  , iSubnetId
  , iTags
  , iVirtualizationType
  , iVpcId
  ) where

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
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.EC2.Types.VirtualizationType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an instance.
--
-- /See:/ 'mkInstance' smart constructor.
data Instance = Instance'
  { amiLaunchIndex :: Core.Int
    -- ^ The AMI launch index, which can be used to find this instance in the launch group.
  , architecture :: Types.ArchitectureValues
    -- ^ The architecture of the image.
  , blockDeviceMappings :: Core.Maybe [Types.InstanceBlockDeviceMapping]
    -- ^ Any block device mapping entries for the instance.
  , capacityReservationId :: Core.Maybe Core.Text
    -- ^ The ID of the Capacity Reservation.
  , capacityReservationSpecification :: Core.Maybe Types.CapacityReservationSpecificationResponse
    -- ^ Information about the Capacity Reservation targeting option.
  , clientToken :: Core.Maybe Core.Text
    -- ^ The idempotency token you provided when you launched the instance, if applicable.
  , cpuOptions :: Core.Maybe Types.CpuOptions
    -- ^ The CPU options for the instance.
  , ebsOptimized :: Core.Maybe Core.Bool
    -- ^ Indicates whether the instance is optimized for Amazon EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS Optimized instance.
  , elasticGpuAssociations :: Core.Maybe [Types.ElasticGpuAssociation]
    -- ^ The Elastic GPU associated with the instance.
  , elasticInferenceAcceleratorAssociations :: Core.Maybe [Types.ElasticInferenceAcceleratorAssociation]
    -- ^ The elastic inference accelerator associated with the instance.
  , enaSupport :: Core.Maybe Core.Bool
    -- ^ Specifies whether enhanced networking with ENA is enabled.
  , enclaveOptions :: Core.Maybe Types.EnclaveOptions
    -- ^ Indicates whether the instance is enabled for AWS Nitro Enclaves.
  , hibernationOptions :: Core.Maybe Types.HibernationOptions
    -- ^ Indicates whether the instance is enabled for hibernation.
  , hypervisor :: Types.HypervisorType
    -- ^ The hypervisor type of the instance. The value @xen@ is used for both Xen and Nitro hypervisors.
  , iamInstanceProfile :: Core.Maybe Types.IamInstanceProfile
    -- ^ The IAM instance profile associated with the instance, if applicable.
  , imageId :: Core.Text
    -- ^ The ID of the AMI used to launch the instance.
  , instanceId :: Core.Text
    -- ^ The ID of the instance.
  , instanceLifecycle :: Core.Maybe Types.InstanceLifecycleType
    -- ^ Indicates whether this is a Spot Instance or a Scheduled Instance.
  , instanceType :: Types.InstanceType
    -- ^ The instance type.
  , kernelId :: Core.Maybe Core.Text
    -- ^ The kernel associated with this instance, if applicable.
  , keyName :: Core.Maybe Core.Text
    -- ^ The name of the key pair, if this instance was launched with an associated key pair.
  , launchTime :: Core.UTCTime
    -- ^ The time the instance was launched.
  , licenses :: Core.Maybe [Types.LicenseConfiguration]
    -- ^ The license configurations.
  , metadataOptions :: Core.Maybe Types.InstanceMetadataOptionsResponse
    -- ^ The metadata options for the instance.
  , monitoring :: Types.Monitoring
    -- ^ The monitoring for the instance.
  , networkInterfaces :: Core.Maybe [Types.InstanceNetworkInterface]
    -- ^ [EC2-VPC] The network interfaces for the instance.
  , outpostArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the Outpost.
  , placement :: Types.Placement
    -- ^ The location where the instance launched, if applicable.
  , platform :: Core.Maybe Types.PlatformValues
    -- ^ The value is @Windows@ for Windows instances; otherwise blank.
  , privateDnsName :: Core.Maybe Core.Text
    -- ^ (IPv4 only) The private DNS hostname name assigned to the instance. This DNS hostname can only be used inside the Amazon EC2 network. This name is not available until the instance enters the @running@ state. 
--
-- [EC2-VPC] The Amazon-provided DNS server resolves Amazon-provided private DNS hostnames if you've enabled DNS resolution and DNS hostnames in your VPC. If you are not using the Amazon-provided DNS server in your VPC, your custom domain name servers must resolve the hostname as appropriate.
  , privateIpAddress :: Core.Maybe Core.Text
    -- ^ The private IPv4 address assigned to the instance.
  , productCodes :: Core.Maybe [Types.ProductCode]
    -- ^ The product codes attached to this instance, if applicable.
  , publicDnsName :: Core.Maybe Core.Text
    -- ^ (IPv4 only) The public DNS name assigned to the instance. This name is not available until the instance enters the @running@ state. For EC2-VPC, this name is only available if you've enabled DNS hostnames for your VPC.
  , publicIpAddress :: Core.Maybe Core.Text
    -- ^ The public IPv4 address, or the Carrier IP address assigned to the instance, if applicable.
--
-- A Carrier IP address only applies to an instance launched in a subnet associated with a Wavelength Zone.
  , ramdiskId :: Core.Maybe Core.Text
    -- ^ The RAM disk associated with this instance, if applicable.
  , rootDeviceName :: Core.Maybe Core.Text
    -- ^ The device name of the root device volume (for example, @/dev/sda1@ ).
  , rootDeviceType :: Types.DeviceType
    -- ^ The root device type used by the AMI. The AMI can use an EBS volume or an instance store volume.
  , securityGroups :: Core.Maybe [Types.GroupIdentifier]
    -- ^ The security groups for the instance.
  , sourceDestCheck :: Core.Maybe Core.Bool
    -- ^ Specifies whether to enable an instance launched in a VPC to perform NAT. This controls whether source/destination checking is enabled on the instance. A value of @true@ means that checking is enabled, and @false@ means that checking is disabled. The value must be @false@ for the instance to perform NAT. For more information, see <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_NAT_Instance.html NAT Instances> in the /Amazon Virtual Private Cloud User Guide/ .
  , spotInstanceRequestId :: Core.Maybe Core.Text
    -- ^ If the request is a Spot Instance request, the ID of the request.
  , sriovNetSupport :: Core.Maybe Core.Text
    -- ^ Specifies whether enhanced networking with the Intel 82599 Virtual Function interface is enabled.
  , state :: Types.InstanceState
    -- ^ The current state of the instance.
  , stateReason :: Core.Maybe Types.StateReason
    -- ^ The reason for the most recent state transition.
  , stateTransitionReason :: Core.Maybe Core.Text
    -- ^ The reason for the most recent state transition. This might be an empty string.
  , subnetId :: Core.Maybe Core.Text
    -- ^ [EC2-VPC] The ID of the subnet in which the instance is running.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Any tags assigned to the instance.
  , virtualizationType :: Types.VirtualizationType
    -- ^ The virtualization type of the instance.
  , vpcId :: Core.Maybe Core.Text
    -- ^ [EC2-VPC] The ID of the VPC in which the instance is running.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Instance' value with any optional fields omitted.
mkInstance
    :: Core.Int -- ^ 'amiLaunchIndex'
    -> Types.ArchitectureValues -- ^ 'architecture'
    -> Types.HypervisorType -- ^ 'hypervisor'
    -> Core.Text -- ^ 'imageId'
    -> Core.Text -- ^ 'instanceId'
    -> Types.InstanceType -- ^ 'instanceType'
    -> Core.UTCTime -- ^ 'launchTime'
    -> Types.Monitoring -- ^ 'monitoring'
    -> Types.Placement -- ^ 'placement'
    -> Types.DeviceType -- ^ 'rootDeviceType'
    -> Types.InstanceState -- ^ 'state'
    -> Types.VirtualizationType -- ^ 'virtualizationType'
    -> Instance
mkInstance amiLaunchIndex architecture hypervisor imageId
  instanceId instanceType launchTime monitoring placement
  rootDeviceType state virtualizationType
  = Instance'{amiLaunchIndex, architecture,
              blockDeviceMappings = Core.Nothing,
              capacityReservationId = Core.Nothing,
              capacityReservationSpecification = Core.Nothing,
              clientToken = Core.Nothing, cpuOptions = Core.Nothing,
              ebsOptimized = Core.Nothing, elasticGpuAssociations = Core.Nothing,
              elasticInferenceAcceleratorAssociations = Core.Nothing,
              enaSupport = Core.Nothing, enclaveOptions = Core.Nothing,
              hibernationOptions = Core.Nothing, hypervisor,
              iamInstanceProfile = Core.Nothing, imageId, instanceId,
              instanceLifecycle = Core.Nothing, instanceType,
              kernelId = Core.Nothing, keyName = Core.Nothing, launchTime,
              licenses = Core.Nothing, metadataOptions = Core.Nothing,
              monitoring, networkInterfaces = Core.Nothing,
              outpostArn = Core.Nothing, placement, platform = Core.Nothing,
              privateDnsName = Core.Nothing, privateIpAddress = Core.Nothing,
              productCodes = Core.Nothing, publicDnsName = Core.Nothing,
              publicIpAddress = Core.Nothing, ramdiskId = Core.Nothing,
              rootDeviceName = Core.Nothing, rootDeviceType,
              securityGroups = Core.Nothing, sourceDestCheck = Core.Nothing,
              spotInstanceRequestId = Core.Nothing,
              sriovNetSupport = Core.Nothing, state, stateReason = Core.Nothing,
              stateTransitionReason = Core.Nothing, subnetId = Core.Nothing,
              tags = Core.Nothing, virtualizationType, vpcId = Core.Nothing}

-- | The AMI launch index, which can be used to find this instance in the launch group.
--
-- /Note:/ Consider using 'amiLaunchIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iAmiLaunchIndex :: Lens.Lens' Instance Core.Int
iAmiLaunchIndex = Lens.field @"amiLaunchIndex"
{-# INLINEABLE iAmiLaunchIndex #-}
{-# DEPRECATED amiLaunchIndex "Use generic-lens or generic-optics with 'amiLaunchIndex' instead"  #-}

-- | The architecture of the image.
--
-- /Note:/ Consider using 'architecture' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iArchitecture :: Lens.Lens' Instance Types.ArchitectureValues
iArchitecture = Lens.field @"architecture"
{-# INLINEABLE iArchitecture #-}
{-# DEPRECATED architecture "Use generic-lens or generic-optics with 'architecture' instead"  #-}

-- | Any block device mapping entries for the instance.
--
-- /Note:/ Consider using 'blockDeviceMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iBlockDeviceMappings :: Lens.Lens' Instance (Core.Maybe [Types.InstanceBlockDeviceMapping])
iBlockDeviceMappings = Lens.field @"blockDeviceMappings"
{-# INLINEABLE iBlockDeviceMappings #-}
{-# DEPRECATED blockDeviceMappings "Use generic-lens or generic-optics with 'blockDeviceMappings' instead"  #-}

-- | The ID of the Capacity Reservation.
--
-- /Note:/ Consider using 'capacityReservationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iCapacityReservationId :: Lens.Lens' Instance (Core.Maybe Core.Text)
iCapacityReservationId = Lens.field @"capacityReservationId"
{-# INLINEABLE iCapacityReservationId #-}
{-# DEPRECATED capacityReservationId "Use generic-lens or generic-optics with 'capacityReservationId' instead"  #-}

-- | Information about the Capacity Reservation targeting option.
--
-- /Note:/ Consider using 'capacityReservationSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iCapacityReservationSpecification :: Lens.Lens' Instance (Core.Maybe Types.CapacityReservationSpecificationResponse)
iCapacityReservationSpecification = Lens.field @"capacityReservationSpecification"
{-# INLINEABLE iCapacityReservationSpecification #-}
{-# DEPRECATED capacityReservationSpecification "Use generic-lens or generic-optics with 'capacityReservationSpecification' instead"  #-}

-- | The idempotency token you provided when you launched the instance, if applicable.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iClientToken :: Lens.Lens' Instance (Core.Maybe Core.Text)
iClientToken = Lens.field @"clientToken"
{-# INLINEABLE iClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | The CPU options for the instance.
--
-- /Note:/ Consider using 'cpuOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iCpuOptions :: Lens.Lens' Instance (Core.Maybe Types.CpuOptions)
iCpuOptions = Lens.field @"cpuOptions"
{-# INLINEABLE iCpuOptions #-}
{-# DEPRECATED cpuOptions "Use generic-lens or generic-optics with 'cpuOptions' instead"  #-}

-- | Indicates whether the instance is optimized for Amazon EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS Optimized instance.
--
-- /Note:/ Consider using 'ebsOptimized' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iEbsOptimized :: Lens.Lens' Instance (Core.Maybe Core.Bool)
iEbsOptimized = Lens.field @"ebsOptimized"
{-# INLINEABLE iEbsOptimized #-}
{-# DEPRECATED ebsOptimized "Use generic-lens or generic-optics with 'ebsOptimized' instead"  #-}

-- | The Elastic GPU associated with the instance.
--
-- /Note:/ Consider using 'elasticGpuAssociations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iElasticGpuAssociations :: Lens.Lens' Instance (Core.Maybe [Types.ElasticGpuAssociation])
iElasticGpuAssociations = Lens.field @"elasticGpuAssociations"
{-# INLINEABLE iElasticGpuAssociations #-}
{-# DEPRECATED elasticGpuAssociations "Use generic-lens or generic-optics with 'elasticGpuAssociations' instead"  #-}

-- | The elastic inference accelerator associated with the instance.
--
-- /Note:/ Consider using 'elasticInferenceAcceleratorAssociations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iElasticInferenceAcceleratorAssociations :: Lens.Lens' Instance (Core.Maybe [Types.ElasticInferenceAcceleratorAssociation])
iElasticInferenceAcceleratorAssociations = Lens.field @"elasticInferenceAcceleratorAssociations"
{-# INLINEABLE iElasticInferenceAcceleratorAssociations #-}
{-# DEPRECATED elasticInferenceAcceleratorAssociations "Use generic-lens or generic-optics with 'elasticInferenceAcceleratorAssociations' instead"  #-}

-- | Specifies whether enhanced networking with ENA is enabled.
--
-- /Note:/ Consider using 'enaSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iEnaSupport :: Lens.Lens' Instance (Core.Maybe Core.Bool)
iEnaSupport = Lens.field @"enaSupport"
{-# INLINEABLE iEnaSupport #-}
{-# DEPRECATED enaSupport "Use generic-lens or generic-optics with 'enaSupport' instead"  #-}

-- | Indicates whether the instance is enabled for AWS Nitro Enclaves.
--
-- /Note:/ Consider using 'enclaveOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iEnclaveOptions :: Lens.Lens' Instance (Core.Maybe Types.EnclaveOptions)
iEnclaveOptions = Lens.field @"enclaveOptions"
{-# INLINEABLE iEnclaveOptions #-}
{-# DEPRECATED enclaveOptions "Use generic-lens or generic-optics with 'enclaveOptions' instead"  #-}

-- | Indicates whether the instance is enabled for hibernation.
--
-- /Note:/ Consider using 'hibernationOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iHibernationOptions :: Lens.Lens' Instance (Core.Maybe Types.HibernationOptions)
iHibernationOptions = Lens.field @"hibernationOptions"
{-# INLINEABLE iHibernationOptions #-}
{-# DEPRECATED hibernationOptions "Use generic-lens or generic-optics with 'hibernationOptions' instead"  #-}

-- | The hypervisor type of the instance. The value @xen@ is used for both Xen and Nitro hypervisors.
--
-- /Note:/ Consider using 'hypervisor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iHypervisor :: Lens.Lens' Instance Types.HypervisorType
iHypervisor = Lens.field @"hypervisor"
{-# INLINEABLE iHypervisor #-}
{-# DEPRECATED hypervisor "Use generic-lens or generic-optics with 'hypervisor' instead"  #-}

-- | The IAM instance profile associated with the instance, if applicable.
--
-- /Note:/ Consider using 'iamInstanceProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iIamInstanceProfile :: Lens.Lens' Instance (Core.Maybe Types.IamInstanceProfile)
iIamInstanceProfile = Lens.field @"iamInstanceProfile"
{-# INLINEABLE iIamInstanceProfile #-}
{-# DEPRECATED iamInstanceProfile "Use generic-lens or generic-optics with 'iamInstanceProfile' instead"  #-}

-- | The ID of the AMI used to launch the instance.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iImageId :: Lens.Lens' Instance Core.Text
iImageId = Lens.field @"imageId"
{-# INLINEABLE iImageId #-}
{-# DEPRECATED imageId "Use generic-lens or generic-optics with 'imageId' instead"  #-}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInstanceId :: Lens.Lens' Instance Core.Text
iInstanceId = Lens.field @"instanceId"
{-# INLINEABLE iInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | Indicates whether this is a Spot Instance or a Scheduled Instance.
--
-- /Note:/ Consider using 'instanceLifecycle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInstanceLifecycle :: Lens.Lens' Instance (Core.Maybe Types.InstanceLifecycleType)
iInstanceLifecycle = Lens.field @"instanceLifecycle"
{-# INLINEABLE iInstanceLifecycle #-}
{-# DEPRECATED instanceLifecycle "Use generic-lens or generic-optics with 'instanceLifecycle' instead"  #-}

-- | The instance type.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInstanceType :: Lens.Lens' Instance Types.InstanceType
iInstanceType = Lens.field @"instanceType"
{-# INLINEABLE iInstanceType #-}
{-# DEPRECATED instanceType "Use generic-lens or generic-optics with 'instanceType' instead"  #-}

-- | The kernel associated with this instance, if applicable.
--
-- /Note:/ Consider using 'kernelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iKernelId :: Lens.Lens' Instance (Core.Maybe Core.Text)
iKernelId = Lens.field @"kernelId"
{-# INLINEABLE iKernelId #-}
{-# DEPRECATED kernelId "Use generic-lens or generic-optics with 'kernelId' instead"  #-}

-- | The name of the key pair, if this instance was launched with an associated key pair.
--
-- /Note:/ Consider using 'keyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iKeyName :: Lens.Lens' Instance (Core.Maybe Core.Text)
iKeyName = Lens.field @"keyName"
{-# INLINEABLE iKeyName #-}
{-# DEPRECATED keyName "Use generic-lens or generic-optics with 'keyName' instead"  #-}

-- | The time the instance was launched.
--
-- /Note:/ Consider using 'launchTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iLaunchTime :: Lens.Lens' Instance Core.UTCTime
iLaunchTime = Lens.field @"launchTime"
{-# INLINEABLE iLaunchTime #-}
{-# DEPRECATED launchTime "Use generic-lens or generic-optics with 'launchTime' instead"  #-}

-- | The license configurations.
--
-- /Note:/ Consider using 'licenses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iLicenses :: Lens.Lens' Instance (Core.Maybe [Types.LicenseConfiguration])
iLicenses = Lens.field @"licenses"
{-# INLINEABLE iLicenses #-}
{-# DEPRECATED licenses "Use generic-lens or generic-optics with 'licenses' instead"  #-}

-- | The metadata options for the instance.
--
-- /Note:/ Consider using 'metadataOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iMetadataOptions :: Lens.Lens' Instance (Core.Maybe Types.InstanceMetadataOptionsResponse)
iMetadataOptions = Lens.field @"metadataOptions"
{-# INLINEABLE iMetadataOptions #-}
{-# DEPRECATED metadataOptions "Use generic-lens or generic-optics with 'metadataOptions' instead"  #-}

-- | The monitoring for the instance.
--
-- /Note:/ Consider using 'monitoring' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iMonitoring :: Lens.Lens' Instance Types.Monitoring
iMonitoring = Lens.field @"monitoring"
{-# INLINEABLE iMonitoring #-}
{-# DEPRECATED monitoring "Use generic-lens or generic-optics with 'monitoring' instead"  #-}

-- | [EC2-VPC] The network interfaces for the instance.
--
-- /Note:/ Consider using 'networkInterfaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iNetworkInterfaces :: Lens.Lens' Instance (Core.Maybe [Types.InstanceNetworkInterface])
iNetworkInterfaces = Lens.field @"networkInterfaces"
{-# INLINEABLE iNetworkInterfaces #-}
{-# DEPRECATED networkInterfaces "Use generic-lens or generic-optics with 'networkInterfaces' instead"  #-}

-- | The Amazon Resource Name (ARN) of the Outpost.
--
-- /Note:/ Consider using 'outpostArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iOutpostArn :: Lens.Lens' Instance (Core.Maybe Core.Text)
iOutpostArn = Lens.field @"outpostArn"
{-# INLINEABLE iOutpostArn #-}
{-# DEPRECATED outpostArn "Use generic-lens or generic-optics with 'outpostArn' instead"  #-}

-- | The location where the instance launched, if applicable.
--
-- /Note:/ Consider using 'placement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPlacement :: Lens.Lens' Instance Types.Placement
iPlacement = Lens.field @"placement"
{-# INLINEABLE iPlacement #-}
{-# DEPRECATED placement "Use generic-lens or generic-optics with 'placement' instead"  #-}

-- | The value is @Windows@ for Windows instances; otherwise blank.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPlatform :: Lens.Lens' Instance (Core.Maybe Types.PlatformValues)
iPlatform = Lens.field @"platform"
{-# INLINEABLE iPlatform #-}
{-# DEPRECATED platform "Use generic-lens or generic-optics with 'platform' instead"  #-}

-- | (IPv4 only) The private DNS hostname name assigned to the instance. This DNS hostname can only be used inside the Amazon EC2 network. This name is not available until the instance enters the @running@ state. 
--
-- [EC2-VPC] The Amazon-provided DNS server resolves Amazon-provided private DNS hostnames if you've enabled DNS resolution and DNS hostnames in your VPC. If you are not using the Amazon-provided DNS server in your VPC, your custom domain name servers must resolve the hostname as appropriate.
--
-- /Note:/ Consider using 'privateDnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPrivateDnsName :: Lens.Lens' Instance (Core.Maybe Core.Text)
iPrivateDnsName = Lens.field @"privateDnsName"
{-# INLINEABLE iPrivateDnsName #-}
{-# DEPRECATED privateDnsName "Use generic-lens or generic-optics with 'privateDnsName' instead"  #-}

-- | The private IPv4 address assigned to the instance.
--
-- /Note:/ Consider using 'privateIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPrivateIpAddress :: Lens.Lens' Instance (Core.Maybe Core.Text)
iPrivateIpAddress = Lens.field @"privateIpAddress"
{-# INLINEABLE iPrivateIpAddress #-}
{-# DEPRECATED privateIpAddress "Use generic-lens or generic-optics with 'privateIpAddress' instead"  #-}

-- | The product codes attached to this instance, if applicable.
--
-- /Note:/ Consider using 'productCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iProductCodes :: Lens.Lens' Instance (Core.Maybe [Types.ProductCode])
iProductCodes = Lens.field @"productCodes"
{-# INLINEABLE iProductCodes #-}
{-# DEPRECATED productCodes "Use generic-lens or generic-optics with 'productCodes' instead"  #-}

-- | (IPv4 only) The public DNS name assigned to the instance. This name is not available until the instance enters the @running@ state. For EC2-VPC, this name is only available if you've enabled DNS hostnames for your VPC.
--
-- /Note:/ Consider using 'publicDnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPublicDnsName :: Lens.Lens' Instance (Core.Maybe Core.Text)
iPublicDnsName = Lens.field @"publicDnsName"
{-# INLINEABLE iPublicDnsName #-}
{-# DEPRECATED publicDnsName "Use generic-lens or generic-optics with 'publicDnsName' instead"  #-}

-- | The public IPv4 address, or the Carrier IP address assigned to the instance, if applicable.
--
-- A Carrier IP address only applies to an instance launched in a subnet associated with a Wavelength Zone.
--
-- /Note:/ Consider using 'publicIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPublicIpAddress :: Lens.Lens' Instance (Core.Maybe Core.Text)
iPublicIpAddress = Lens.field @"publicIpAddress"
{-# INLINEABLE iPublicIpAddress #-}
{-# DEPRECATED publicIpAddress "Use generic-lens or generic-optics with 'publicIpAddress' instead"  #-}

-- | The RAM disk associated with this instance, if applicable.
--
-- /Note:/ Consider using 'ramdiskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iRamdiskId :: Lens.Lens' Instance (Core.Maybe Core.Text)
iRamdiskId = Lens.field @"ramdiskId"
{-# INLINEABLE iRamdiskId #-}
{-# DEPRECATED ramdiskId "Use generic-lens or generic-optics with 'ramdiskId' instead"  #-}

-- | The device name of the root device volume (for example, @/dev/sda1@ ).
--
-- /Note:/ Consider using 'rootDeviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iRootDeviceName :: Lens.Lens' Instance (Core.Maybe Core.Text)
iRootDeviceName = Lens.field @"rootDeviceName"
{-# INLINEABLE iRootDeviceName #-}
{-# DEPRECATED rootDeviceName "Use generic-lens or generic-optics with 'rootDeviceName' instead"  #-}

-- | The root device type used by the AMI. The AMI can use an EBS volume or an instance store volume.
--
-- /Note:/ Consider using 'rootDeviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iRootDeviceType :: Lens.Lens' Instance Types.DeviceType
iRootDeviceType = Lens.field @"rootDeviceType"
{-# INLINEABLE iRootDeviceType #-}
{-# DEPRECATED rootDeviceType "Use generic-lens or generic-optics with 'rootDeviceType' instead"  #-}

-- | The security groups for the instance.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iSecurityGroups :: Lens.Lens' Instance (Core.Maybe [Types.GroupIdentifier])
iSecurityGroups = Lens.field @"securityGroups"
{-# INLINEABLE iSecurityGroups #-}
{-# DEPRECATED securityGroups "Use generic-lens or generic-optics with 'securityGroups' instead"  #-}

-- | Specifies whether to enable an instance launched in a VPC to perform NAT. This controls whether source/destination checking is enabled on the instance. A value of @true@ means that checking is enabled, and @false@ means that checking is disabled. The value must be @false@ for the instance to perform NAT. For more information, see <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_NAT_Instance.html NAT Instances> in the /Amazon Virtual Private Cloud User Guide/ .
--
-- /Note:/ Consider using 'sourceDestCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iSourceDestCheck :: Lens.Lens' Instance (Core.Maybe Core.Bool)
iSourceDestCheck = Lens.field @"sourceDestCheck"
{-# INLINEABLE iSourceDestCheck #-}
{-# DEPRECATED sourceDestCheck "Use generic-lens or generic-optics with 'sourceDestCheck' instead"  #-}

-- | If the request is a Spot Instance request, the ID of the request.
--
-- /Note:/ Consider using 'spotInstanceRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iSpotInstanceRequestId :: Lens.Lens' Instance (Core.Maybe Core.Text)
iSpotInstanceRequestId = Lens.field @"spotInstanceRequestId"
{-# INLINEABLE iSpotInstanceRequestId #-}
{-# DEPRECATED spotInstanceRequestId "Use generic-lens or generic-optics with 'spotInstanceRequestId' instead"  #-}

-- | Specifies whether enhanced networking with the Intel 82599 Virtual Function interface is enabled.
--
-- /Note:/ Consider using 'sriovNetSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iSriovNetSupport :: Lens.Lens' Instance (Core.Maybe Core.Text)
iSriovNetSupport = Lens.field @"sriovNetSupport"
{-# INLINEABLE iSriovNetSupport #-}
{-# DEPRECATED sriovNetSupport "Use generic-lens or generic-optics with 'sriovNetSupport' instead"  #-}

-- | The current state of the instance.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iState :: Lens.Lens' Instance Types.InstanceState
iState = Lens.field @"state"
{-# INLINEABLE iState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The reason for the most recent state transition.
--
-- /Note:/ Consider using 'stateReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iStateReason :: Lens.Lens' Instance (Core.Maybe Types.StateReason)
iStateReason = Lens.field @"stateReason"
{-# INLINEABLE iStateReason #-}
{-# DEPRECATED stateReason "Use generic-lens or generic-optics with 'stateReason' instead"  #-}

-- | The reason for the most recent state transition. This might be an empty string.
--
-- /Note:/ Consider using 'stateTransitionReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iStateTransitionReason :: Lens.Lens' Instance (Core.Maybe Core.Text)
iStateTransitionReason = Lens.field @"stateTransitionReason"
{-# INLINEABLE iStateTransitionReason #-}
{-# DEPRECATED stateTransitionReason "Use generic-lens or generic-optics with 'stateTransitionReason' instead"  #-}

-- | [EC2-VPC] The ID of the subnet in which the instance is running.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iSubnetId :: Lens.Lens' Instance (Core.Maybe Core.Text)
iSubnetId = Lens.field @"subnetId"
{-# INLINEABLE iSubnetId #-}
{-# DEPRECATED subnetId "Use generic-lens or generic-optics with 'subnetId' instead"  #-}

-- | Any tags assigned to the instance.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iTags :: Lens.Lens' Instance (Core.Maybe [Types.Tag])
iTags = Lens.field @"tags"
{-# INLINEABLE iTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The virtualization type of the instance.
--
-- /Note:/ Consider using 'virtualizationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iVirtualizationType :: Lens.Lens' Instance Types.VirtualizationType
iVirtualizationType = Lens.field @"virtualizationType"
{-# INLINEABLE iVirtualizationType #-}
{-# DEPRECATED virtualizationType "Use generic-lens or generic-optics with 'virtualizationType' instead"  #-}

-- | [EC2-VPC] The ID of the VPC in which the instance is running.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iVpcId :: Lens.Lens' Instance (Core.Maybe Core.Text)
iVpcId = Lens.field @"vpcId"
{-# INLINEABLE iVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

instance Core.FromXML Instance where
        parseXML x
          = Instance' Core.<$>
              (x Core..@ "amiLaunchIndex") Core.<*> x Core..@ "architecture"
                Core.<*>
                x Core..@? "blockDeviceMapping" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "capacityReservationId"
                Core.<*> x Core..@? "capacityReservationSpecification"
                Core.<*> x Core..@? "clientToken"
                Core.<*> x Core..@? "cpuOptions"
                Core.<*> x Core..@? "ebsOptimized"
                Core.<*>
                x Core..@? "elasticGpuAssociationSet" Core..<@>
                  Core.parseXMLList "item"
                Core.<*>
                x Core..@? "elasticInferenceAcceleratorAssociationSet" Core..<@>
                  Core.parseXMLList "item"
                Core.<*> x Core..@? "enaSupport"
                Core.<*> x Core..@? "enclaveOptions"
                Core.<*> x Core..@? "hibernationOptions"
                Core.<*> x Core..@ "hypervisor"
                Core.<*> x Core..@? "iamInstanceProfile"
                Core.<*> x Core..@ "imageId"
                Core.<*> x Core..@ "instanceId"
                Core.<*> x Core..@? "instanceLifecycle"
                Core.<*> x Core..@ "instanceType"
                Core.<*> x Core..@? "kernelId"
                Core.<*> x Core..@? "keyName"
                Core.<*> x Core..@ "launchTime"
                Core.<*> x Core..@? "licenseSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "metadataOptions"
                Core.<*> x Core..@ "monitoring"
                Core.<*>
                x Core..@? "networkInterfaceSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "outpostArn"
                Core.<*> x Core..@ "placement"
                Core.<*> x Core..@? "platform"
                Core.<*> x Core..@? "privateDnsName"
                Core.<*> x Core..@? "privateIpAddress"
                Core.<*>
                x Core..@? "productCodes" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "dnsName"
                Core.<*> x Core..@? "ipAddress"
                Core.<*> x Core..@? "ramdiskId"
                Core.<*> x Core..@? "rootDeviceName"
                Core.<*> x Core..@ "rootDeviceType"
                Core.<*> x Core..@? "groupSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "sourceDestCheck"
                Core.<*> x Core..@? "spotInstanceRequestId"
                Core.<*> x Core..@? "sriovNetSupport"
                Core.<*> x Core..@ "instanceState"
                Core.<*> x Core..@? "stateReason"
                Core.<*> x Core..@? "reason"
                Core.<*> x Core..@? "subnetId"
                Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@ "virtualizationType"
                Core.<*> x Core..@? "vpcId"
