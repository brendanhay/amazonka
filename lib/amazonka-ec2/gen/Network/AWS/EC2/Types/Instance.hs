{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Instance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Instance where

import Network.AWS.EC2.Internal
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
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an instance.
--
--
--
-- /See:/ 'instance'' smart constructor.
data Instance = Instance'
  { _insPublicDNSName :: !(Maybe Text),
    _insPlatform :: !(Maybe PlatformValues),
    _insSecurityGroups :: !(Maybe [GroupIdentifier]),
    _insClientToken :: !(Maybe Text),
    _insEnaSupport :: !(Maybe Bool),
    _insSourceDestCheck :: !(Maybe Bool),
    _insElasticGpuAssociations :: !(Maybe [ElasticGpuAssociation]),
    _insVPCId :: !(Maybe Text),
    _insKeyName :: !(Maybe Text),
    _insNetworkInterfaces :: !(Maybe [InstanceNetworkInterface]),
    _insOutpostARN :: !(Maybe Text),
    _insEnclaveOptions :: !(Maybe EnclaveOptions),
    _insRAMDiskId :: !(Maybe Text),
    _insCPUOptions :: !(Maybe CPUOptions),
    _insSubnetId :: !(Maybe Text),
    _insKernelId :: !(Maybe Text),
    _insRootDeviceName :: !(Maybe Text),
    _insCapacityReservationId :: !(Maybe Text),
    _insCapacityReservationSpecification ::
      !(Maybe CapacityReservationSpecificationResponse),
    _insSRIOVNetSupport :: !(Maybe Text),
    _insEBSOptimized :: !(Maybe Bool),
    _insStateTransitionReason :: !(Maybe Text),
    _insHibernationOptions :: !(Maybe HibernationOptions),
    _insInstanceLifecycle :: !(Maybe InstanceLifecycleType),
    _insIAMInstanceProfile :: !(Maybe IAMInstanceProfile),
    _insPrivateIPAddress :: !(Maybe Text),
    _insMetadataOptions :: !(Maybe InstanceMetadataOptionsResponse),
    _insProductCodes :: !(Maybe [ProductCode]),
    _insSpotInstanceRequestId :: !(Maybe Text),
    _insLicenses :: !(Maybe [LicenseConfiguration]),
    _insElasticInferenceAcceleratorAssociations ::
      !(Maybe [ElasticInferenceAcceleratorAssociation]),
    _insPrivateDNSName :: !(Maybe Text),
    _insStateReason :: !(Maybe StateReason),
    _insBlockDeviceMappings :: !(Maybe [InstanceBlockDeviceMapping]),
    _insPublicIPAddress :: !(Maybe Text),
    _insTags :: !(Maybe [Tag]),
    _insInstanceId :: !Text,
    _insImageId :: !Text,
    _insAMILaunchIndex :: !Int,
    _insInstanceType :: !InstanceType,
    _insLaunchTime :: !ISO8601,
    _insPlacement :: !Placement,
    _insMonitoring :: !Monitoring,
    _insArchitecture :: !ArchitectureValues,
    _insRootDeviceType :: !DeviceType,
    _insVirtualizationType :: !VirtualizationType,
    _insHypervisor :: !HypervisorType,
    _insState :: !InstanceState
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Instance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'insPublicDNSName' - (IPv4 only) The public DNS name assigned to the instance. This name is not available until the instance enters the @running@ state. For EC2-VPC, this name is only available if you've enabled DNS hostnames for your VPC.
--
-- * 'insPlatform' - The value is @Windows@ for Windows instances; otherwise blank.
--
-- * 'insSecurityGroups' - The security groups for the instance.
--
-- * 'insClientToken' - The idempotency token you provided when you launched the instance, if applicable.
--
-- * 'insEnaSupport' - Specifies whether enhanced networking with ENA is enabled.
--
-- * 'insSourceDestCheck' - Specifies whether to enable an instance launched in a VPC to perform NAT. This controls whether source/destination checking is enabled on the instance. A value of @true@ means that checking is enabled, and @false@ means that checking is disabled. The value must be @false@ for the instance to perform NAT. For more information, see <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_NAT_Instance.html NAT Instances> in the /Amazon Virtual Private Cloud User Guide/ .
--
-- * 'insElasticGpuAssociations' - The Elastic GPU associated with the instance.
--
-- * 'insVPCId' - [EC2-VPC] The ID of the VPC in which the instance is running.
--
-- * 'insKeyName' - The name of the key pair, if this instance was launched with an associated key pair.
--
-- * 'insNetworkInterfaces' - [EC2-VPC] The network interfaces for the instance.
--
-- * 'insOutpostARN' - The Amazon Resource Name (ARN) of the Outpost.
--
-- * 'insEnclaveOptions' - Indicates whether the instance is enabled for AWS Nitro Enclaves.
--
-- * 'insRAMDiskId' - The RAM disk associated with this instance, if applicable.
--
-- * 'insCPUOptions' - The CPU options for the instance.
--
-- * 'insSubnetId' - [EC2-VPC] The ID of the subnet in which the instance is running.
--
-- * 'insKernelId' - The kernel associated with this instance, if applicable.
--
-- * 'insRootDeviceName' - The device name of the root device volume (for example, @/dev/sda1@ ).
--
-- * 'insCapacityReservationId' - The ID of the Capacity Reservation.
--
-- * 'insCapacityReservationSpecification' - Information about the Capacity Reservation targeting option.
--
-- * 'insSRIOVNetSupport' - Specifies whether enhanced networking with the Intel 82599 Virtual Function interface is enabled.
--
-- * 'insEBSOptimized' - Indicates whether the instance is optimized for Amazon EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS Optimized instance.
--
-- * 'insStateTransitionReason' - The reason for the most recent state transition. This might be an empty string.
--
-- * 'insHibernationOptions' - Indicates whether the instance is enabled for hibernation.
--
-- * 'insInstanceLifecycle' - Indicates whether this is a Spot Instance or a Scheduled Instance.
--
-- * 'insIAMInstanceProfile' - The IAM instance profile associated with the instance, if applicable.
--
-- * 'insPrivateIPAddress' - The private IPv4 address assigned to the instance.
--
-- * 'insMetadataOptions' - The metadata options for the instance.
--
-- * 'insProductCodes' - The product codes attached to this instance, if applicable.
--
-- * 'insSpotInstanceRequestId' - If the request is a Spot Instance request, the ID of the request.
--
-- * 'insLicenses' - The license configurations.
--
-- * 'insElasticInferenceAcceleratorAssociations' - The elastic inference accelerator associated with the instance.
--
-- * 'insPrivateDNSName' - (IPv4 only) The private DNS hostname name assigned to the instance. This DNS hostname can only be used inside the Amazon EC2 network. This name is not available until the instance enters the @running@ state.  [EC2-VPC] The Amazon-provided DNS server resolves Amazon-provided private DNS hostnames if you've enabled DNS resolution and DNS hostnames in your VPC. If you are not using the Amazon-provided DNS server in your VPC, your custom domain name servers must resolve the hostname as appropriate.
--
-- * 'insStateReason' - The reason for the most recent state transition.
--
-- * 'insBlockDeviceMappings' - Any block device mapping entries for the instance.
--
-- * 'insPublicIPAddress' - The public IPv4 address, or the Carrier IP address assigned to the instance, if applicable. A Carrier IP address only applies to an instance launched in a subnet associated with a Wavelength Zone.
--
-- * 'insTags' - Any tags assigned to the instance.
--
-- * 'insInstanceId' - The ID of the instance.
--
-- * 'insImageId' - The ID of the AMI used to launch the instance.
--
-- * 'insAMILaunchIndex' - The AMI launch index, which can be used to find this instance in the launch group.
--
-- * 'insInstanceType' - The instance type.
--
-- * 'insLaunchTime' - The time the instance was launched.
--
-- * 'insPlacement' - The location where the instance launched, if applicable.
--
-- * 'insMonitoring' - The monitoring for the instance.
--
-- * 'insArchitecture' - The architecture of the image.
--
-- * 'insRootDeviceType' - The root device type used by the AMI. The AMI can use an EBS volume or an instance store volume.
--
-- * 'insVirtualizationType' - The virtualization type of the instance.
--
-- * 'insHypervisor' - The hypervisor type of the instance. The value @xen@ is used for both Xen and Nitro hypervisors.
--
-- * 'insState' - The current state of the instance.
instance' ::
  -- | 'insInstanceId'
  Text ->
  -- | 'insImageId'
  Text ->
  -- | 'insAMILaunchIndex'
  Int ->
  -- | 'insInstanceType'
  InstanceType ->
  -- | 'insLaunchTime'
  UTCTime ->
  -- | 'insPlacement'
  Placement ->
  -- | 'insMonitoring'
  Monitoring ->
  -- | 'insArchitecture'
  ArchitectureValues ->
  -- | 'insRootDeviceType'
  DeviceType ->
  -- | 'insVirtualizationType'
  VirtualizationType ->
  -- | 'insHypervisor'
  HypervisorType ->
  -- | 'insState'
  InstanceState ->
  Instance
instance'
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
      { _insPublicDNSName = Nothing,
        _insPlatform = Nothing,
        _insSecurityGroups = Nothing,
        _insClientToken = Nothing,
        _insEnaSupport = Nothing,
        _insSourceDestCheck = Nothing,
        _insElasticGpuAssociations = Nothing,
        _insVPCId = Nothing,
        _insKeyName = Nothing,
        _insNetworkInterfaces = Nothing,
        _insOutpostARN = Nothing,
        _insEnclaveOptions = Nothing,
        _insRAMDiskId = Nothing,
        _insCPUOptions = Nothing,
        _insSubnetId = Nothing,
        _insKernelId = Nothing,
        _insRootDeviceName = Nothing,
        _insCapacityReservationId = Nothing,
        _insCapacityReservationSpecification = Nothing,
        _insSRIOVNetSupport = Nothing,
        _insEBSOptimized = Nothing,
        _insStateTransitionReason = Nothing,
        _insHibernationOptions = Nothing,
        _insInstanceLifecycle = Nothing,
        _insIAMInstanceProfile = Nothing,
        _insPrivateIPAddress = Nothing,
        _insMetadataOptions = Nothing,
        _insProductCodes = Nothing,
        _insSpotInstanceRequestId = Nothing,
        _insLicenses = Nothing,
        _insElasticInferenceAcceleratorAssociations = Nothing,
        _insPrivateDNSName = Nothing,
        _insStateReason = Nothing,
        _insBlockDeviceMappings = Nothing,
        _insPublicIPAddress = Nothing,
        _insTags = Nothing,
        _insInstanceId = pInstanceId_,
        _insImageId = pImageId_,
        _insAMILaunchIndex = pAMILaunchIndex_,
        _insInstanceType = pInstanceType_,
        _insLaunchTime = _Time # pLaunchTime_,
        _insPlacement = pPlacement_,
        _insMonitoring = pMonitoring_,
        _insArchitecture = pArchitecture_,
        _insRootDeviceType = pRootDeviceType_,
        _insVirtualizationType = pVirtualizationType_,
        _insHypervisor = pHypervisor_,
        _insState = pState_
      }

-- | (IPv4 only) The public DNS name assigned to the instance. This name is not available until the instance enters the @running@ state. For EC2-VPC, this name is only available if you've enabled DNS hostnames for your VPC.
insPublicDNSName :: Lens' Instance (Maybe Text)
insPublicDNSName = lens _insPublicDNSName (\s a -> s {_insPublicDNSName = a})

-- | The value is @Windows@ for Windows instances; otherwise blank.
insPlatform :: Lens' Instance (Maybe PlatformValues)
insPlatform = lens _insPlatform (\s a -> s {_insPlatform = a})

-- | The security groups for the instance.
insSecurityGroups :: Lens' Instance [GroupIdentifier]
insSecurityGroups = lens _insSecurityGroups (\s a -> s {_insSecurityGroups = a}) . _Default . _Coerce

-- | The idempotency token you provided when you launched the instance, if applicable.
insClientToken :: Lens' Instance (Maybe Text)
insClientToken = lens _insClientToken (\s a -> s {_insClientToken = a})

-- | Specifies whether enhanced networking with ENA is enabled.
insEnaSupport :: Lens' Instance (Maybe Bool)
insEnaSupport = lens _insEnaSupport (\s a -> s {_insEnaSupport = a})

-- | Specifies whether to enable an instance launched in a VPC to perform NAT. This controls whether source/destination checking is enabled on the instance. A value of @true@ means that checking is enabled, and @false@ means that checking is disabled. The value must be @false@ for the instance to perform NAT. For more information, see <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_NAT_Instance.html NAT Instances> in the /Amazon Virtual Private Cloud User Guide/ .
insSourceDestCheck :: Lens' Instance (Maybe Bool)
insSourceDestCheck = lens _insSourceDestCheck (\s a -> s {_insSourceDestCheck = a})

-- | The Elastic GPU associated with the instance.
insElasticGpuAssociations :: Lens' Instance [ElasticGpuAssociation]
insElasticGpuAssociations = lens _insElasticGpuAssociations (\s a -> s {_insElasticGpuAssociations = a}) . _Default . _Coerce

-- | [EC2-VPC] The ID of the VPC in which the instance is running.
insVPCId :: Lens' Instance (Maybe Text)
insVPCId = lens _insVPCId (\s a -> s {_insVPCId = a})

-- | The name of the key pair, if this instance was launched with an associated key pair.
insKeyName :: Lens' Instance (Maybe Text)
insKeyName = lens _insKeyName (\s a -> s {_insKeyName = a})

-- | [EC2-VPC] The network interfaces for the instance.
insNetworkInterfaces :: Lens' Instance [InstanceNetworkInterface]
insNetworkInterfaces = lens _insNetworkInterfaces (\s a -> s {_insNetworkInterfaces = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the Outpost.
insOutpostARN :: Lens' Instance (Maybe Text)
insOutpostARN = lens _insOutpostARN (\s a -> s {_insOutpostARN = a})

-- | Indicates whether the instance is enabled for AWS Nitro Enclaves.
insEnclaveOptions :: Lens' Instance (Maybe EnclaveOptions)
insEnclaveOptions = lens _insEnclaveOptions (\s a -> s {_insEnclaveOptions = a})

-- | The RAM disk associated with this instance, if applicable.
insRAMDiskId :: Lens' Instance (Maybe Text)
insRAMDiskId = lens _insRAMDiskId (\s a -> s {_insRAMDiskId = a})

-- | The CPU options for the instance.
insCPUOptions :: Lens' Instance (Maybe CPUOptions)
insCPUOptions = lens _insCPUOptions (\s a -> s {_insCPUOptions = a})

-- | [EC2-VPC] The ID of the subnet in which the instance is running.
insSubnetId :: Lens' Instance (Maybe Text)
insSubnetId = lens _insSubnetId (\s a -> s {_insSubnetId = a})

-- | The kernel associated with this instance, if applicable.
insKernelId :: Lens' Instance (Maybe Text)
insKernelId = lens _insKernelId (\s a -> s {_insKernelId = a})

-- | The device name of the root device volume (for example, @/dev/sda1@ ).
insRootDeviceName :: Lens' Instance (Maybe Text)
insRootDeviceName = lens _insRootDeviceName (\s a -> s {_insRootDeviceName = a})

-- | The ID of the Capacity Reservation.
insCapacityReservationId :: Lens' Instance (Maybe Text)
insCapacityReservationId = lens _insCapacityReservationId (\s a -> s {_insCapacityReservationId = a})

-- | Information about the Capacity Reservation targeting option.
insCapacityReservationSpecification :: Lens' Instance (Maybe CapacityReservationSpecificationResponse)
insCapacityReservationSpecification = lens _insCapacityReservationSpecification (\s a -> s {_insCapacityReservationSpecification = a})

-- | Specifies whether enhanced networking with the Intel 82599 Virtual Function interface is enabled.
insSRIOVNetSupport :: Lens' Instance (Maybe Text)
insSRIOVNetSupport = lens _insSRIOVNetSupport (\s a -> s {_insSRIOVNetSupport = a})

-- | Indicates whether the instance is optimized for Amazon EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS Optimized instance.
insEBSOptimized :: Lens' Instance (Maybe Bool)
insEBSOptimized = lens _insEBSOptimized (\s a -> s {_insEBSOptimized = a})

-- | The reason for the most recent state transition. This might be an empty string.
insStateTransitionReason :: Lens' Instance (Maybe Text)
insStateTransitionReason = lens _insStateTransitionReason (\s a -> s {_insStateTransitionReason = a})

-- | Indicates whether the instance is enabled for hibernation.
insHibernationOptions :: Lens' Instance (Maybe HibernationOptions)
insHibernationOptions = lens _insHibernationOptions (\s a -> s {_insHibernationOptions = a})

-- | Indicates whether this is a Spot Instance or a Scheduled Instance.
insInstanceLifecycle :: Lens' Instance (Maybe InstanceLifecycleType)
insInstanceLifecycle = lens _insInstanceLifecycle (\s a -> s {_insInstanceLifecycle = a})

-- | The IAM instance profile associated with the instance, if applicable.
insIAMInstanceProfile :: Lens' Instance (Maybe IAMInstanceProfile)
insIAMInstanceProfile = lens _insIAMInstanceProfile (\s a -> s {_insIAMInstanceProfile = a})

-- | The private IPv4 address assigned to the instance.
insPrivateIPAddress :: Lens' Instance (Maybe Text)
insPrivateIPAddress = lens _insPrivateIPAddress (\s a -> s {_insPrivateIPAddress = a})

-- | The metadata options for the instance.
insMetadataOptions :: Lens' Instance (Maybe InstanceMetadataOptionsResponse)
insMetadataOptions = lens _insMetadataOptions (\s a -> s {_insMetadataOptions = a})

-- | The product codes attached to this instance, if applicable.
insProductCodes :: Lens' Instance [ProductCode]
insProductCodes = lens _insProductCodes (\s a -> s {_insProductCodes = a}) . _Default . _Coerce

-- | If the request is a Spot Instance request, the ID of the request.
insSpotInstanceRequestId :: Lens' Instance (Maybe Text)
insSpotInstanceRequestId = lens _insSpotInstanceRequestId (\s a -> s {_insSpotInstanceRequestId = a})

-- | The license configurations.
insLicenses :: Lens' Instance [LicenseConfiguration]
insLicenses = lens _insLicenses (\s a -> s {_insLicenses = a}) . _Default . _Coerce

-- | The elastic inference accelerator associated with the instance.
insElasticInferenceAcceleratorAssociations :: Lens' Instance [ElasticInferenceAcceleratorAssociation]
insElasticInferenceAcceleratorAssociations = lens _insElasticInferenceAcceleratorAssociations (\s a -> s {_insElasticInferenceAcceleratorAssociations = a}) . _Default . _Coerce

-- | (IPv4 only) The private DNS hostname name assigned to the instance. This DNS hostname can only be used inside the Amazon EC2 network. This name is not available until the instance enters the @running@ state.  [EC2-VPC] The Amazon-provided DNS server resolves Amazon-provided private DNS hostnames if you've enabled DNS resolution and DNS hostnames in your VPC. If you are not using the Amazon-provided DNS server in your VPC, your custom domain name servers must resolve the hostname as appropriate.
insPrivateDNSName :: Lens' Instance (Maybe Text)
insPrivateDNSName = lens _insPrivateDNSName (\s a -> s {_insPrivateDNSName = a})

-- | The reason for the most recent state transition.
insStateReason :: Lens' Instance (Maybe StateReason)
insStateReason = lens _insStateReason (\s a -> s {_insStateReason = a})

-- | Any block device mapping entries for the instance.
insBlockDeviceMappings :: Lens' Instance [InstanceBlockDeviceMapping]
insBlockDeviceMappings = lens _insBlockDeviceMappings (\s a -> s {_insBlockDeviceMappings = a}) . _Default . _Coerce

-- | The public IPv4 address, or the Carrier IP address assigned to the instance, if applicable. A Carrier IP address only applies to an instance launched in a subnet associated with a Wavelength Zone.
insPublicIPAddress :: Lens' Instance (Maybe Text)
insPublicIPAddress = lens _insPublicIPAddress (\s a -> s {_insPublicIPAddress = a})

-- | Any tags assigned to the instance.
insTags :: Lens' Instance [Tag]
insTags = lens _insTags (\s a -> s {_insTags = a}) . _Default . _Coerce

-- | The ID of the instance.
insInstanceId :: Lens' Instance Text
insInstanceId = lens _insInstanceId (\s a -> s {_insInstanceId = a})

-- | The ID of the AMI used to launch the instance.
insImageId :: Lens' Instance Text
insImageId = lens _insImageId (\s a -> s {_insImageId = a})

-- | The AMI launch index, which can be used to find this instance in the launch group.
insAMILaunchIndex :: Lens' Instance Int
insAMILaunchIndex = lens _insAMILaunchIndex (\s a -> s {_insAMILaunchIndex = a})

-- | The instance type.
insInstanceType :: Lens' Instance InstanceType
insInstanceType = lens _insInstanceType (\s a -> s {_insInstanceType = a})

-- | The time the instance was launched.
insLaunchTime :: Lens' Instance UTCTime
insLaunchTime = lens _insLaunchTime (\s a -> s {_insLaunchTime = a}) . _Time

-- | The location where the instance launched, if applicable.
insPlacement :: Lens' Instance Placement
insPlacement = lens _insPlacement (\s a -> s {_insPlacement = a})

-- | The monitoring for the instance.
insMonitoring :: Lens' Instance Monitoring
insMonitoring = lens _insMonitoring (\s a -> s {_insMonitoring = a})

-- | The architecture of the image.
insArchitecture :: Lens' Instance ArchitectureValues
insArchitecture = lens _insArchitecture (\s a -> s {_insArchitecture = a})

-- | The root device type used by the AMI. The AMI can use an EBS volume or an instance store volume.
insRootDeviceType :: Lens' Instance DeviceType
insRootDeviceType = lens _insRootDeviceType (\s a -> s {_insRootDeviceType = a})

-- | The virtualization type of the instance.
insVirtualizationType :: Lens' Instance VirtualizationType
insVirtualizationType = lens _insVirtualizationType (\s a -> s {_insVirtualizationType = a})

-- | The hypervisor type of the instance. The value @xen@ is used for both Xen and Nitro hypervisors.
insHypervisor :: Lens' Instance HypervisorType
insHypervisor = lens _insHypervisor (\s a -> s {_insHypervisor = a})

-- | The current state of the instance.
insState :: Lens' Instance InstanceState
insState = lens _insState (\s a -> s {_insState = a})

instance FromXML Instance where
  parseXML x =
    Instance'
      <$> (x .@? "dnsName")
      <*> (x .@? "platform")
      <*> (x .@? "groupSet" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "clientToken")
      <*> (x .@? "enaSupport")
      <*> (x .@? "sourceDestCheck")
      <*> ( x .@? "elasticGpuAssociationSet" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> (x .@? "vpcId")
      <*> (x .@? "keyName")
      <*> ( x .@? "networkInterfaceSet" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> (x .@? "outpostArn")
      <*> (x .@? "enclaveOptions")
      <*> (x .@? "ramdiskId")
      <*> (x .@? "cpuOptions")
      <*> (x .@? "subnetId")
      <*> (x .@? "kernelId")
      <*> (x .@? "rootDeviceName")
      <*> (x .@? "capacityReservationId")
      <*> (x .@? "capacityReservationSpecification")
      <*> (x .@? "sriovNetSupport")
      <*> (x .@? "ebsOptimized")
      <*> (x .@? "reason")
      <*> (x .@? "hibernationOptions")
      <*> (x .@? "instanceLifecycle")
      <*> (x .@? "iamInstanceProfile")
      <*> (x .@? "privateIpAddress")
      <*> (x .@? "metadataOptions")
      <*> (x .@? "productCodes" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "spotInstanceRequestId")
      <*> (x .@? "licenseSet" .!@ mempty >>= may (parseXMLList "item"))
      <*> ( x .@? "elasticInferenceAcceleratorAssociationSet" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> (x .@? "privateDnsName")
      <*> (x .@? "stateReason")
      <*> ( x .@? "blockDeviceMapping" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> (x .@? "ipAddress")
      <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@ "instanceId")
      <*> (x .@ "imageId")
      <*> (x .@ "amiLaunchIndex")
      <*> (x .@ "instanceType")
      <*> (x .@ "launchTime")
      <*> (x .@ "placement")
      <*> (x .@ "monitoring")
      <*> (x .@ "architecture")
      <*> (x .@ "rootDeviceType")
      <*> (x .@ "virtualizationType")
      <*> (x .@ "hypervisor")
      <*> (x .@ "instanceState")

instance Hashable Instance

instance NFData Instance
