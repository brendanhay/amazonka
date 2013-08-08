-- |
-- Module : Network.AWS.EC2.Types
-- Copyright : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.EC2.Types where

import Data.ByteString      (ByteString)
import Data.Monoid
import Network.AWS.Internal

data CreateImage = CreateImage
    { InstanceId :: !String
    , Name :: !String
    , Description :: !(Maybe String)
    , NoReboot :: !(Maybe Boolean)
    , BlockDeviceMapping :: !(Maybe BlockDeviceMapping)
    } deriving (Show)

data CreateImageResponse = CreateImageResponse
    { RequestId :: !String
    , ImageId :: !String
    } deriving (Show)

data RegisterImage = RegisterImage
    { ImageLocation :: !(Maybe String)
    , Name :: !String
    , Description :: !(Maybe String)
    , Architecture :: !(Maybe String)
    , KernelId :: !(Maybe String)
    , RamdiskId :: !(Maybe String)
    , RootDeviceName :: !(Maybe String)
    , BlockDeviceMapping :: !(Maybe BlockDeviceMapping)
    } deriving (Show)

data RegisterImageResponse = RegisterImageResponse
    { RequestId :: !String
    , ImageId :: !String
    } deriving (Show)

data DeregisterImage = DeregisterImage
    { ImageId :: !String
    } deriving (Show)

data DeregisterImageResponse = DeregisterImageResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data CreateKeyPair = CreateKeyPair
    { KeyName :: !String
    } deriving (Show)

data CreateKeyPairResponse = CreateKeyPairResponse
    { RequestId :: !String
    , KeyName :: !String
    , KeyFingerprint :: !String
    , KeyMaterial :: !String
    } deriving (Show)

data ImportKeyPair = ImportKeyPair
    { KeyName :: !String
    , PublicKeyMaterial :: !String
    } deriving (Show)

data ImportKeyPairResponse = ImportKeyPairResponse
    { RequestId :: !String
    , KeyName :: !String
    , KeyFingerprint :: !String
    } deriving (Show)

data DeleteKeyPair = DeleteKeyPair
    { KeyName :: !String
    } deriving (Show)

data DeleteKeyPairResponse = DeleteKeyPairResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data DescribeKeyPairs = DescribeKeyPairs
    { KeySet :: !DescribeKeyPairsInfo
    , FilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data DescribeKeyPairsInfo = DescribeKeyPairsInfo
    { Item :: ![DescribeKeyPairsItem]
    } deriving (Show)

data DescribeKeyPairsItem = DescribeKeyPairsItem
    { KeyName :: !String
    } deriving (Show)

data DescribeKeyPairsResponse = DescribeKeyPairsResponse
    { RequestId :: !String
    , KeySet :: !DescribeKeyPairsResponseInfo
    } deriving (Show)

data DescribeKeyPairsResponseInfo = DescribeKeyPairsResponseInfo
    { Item :: ![DescribeKeyPairsResponseItem]
    } deriving (Show)

data DescribeKeyPairsResponseItem = DescribeKeyPairsResponseItem
    { KeyName :: !String
    , KeyFingerprint :: !String
    } deriving (Show)

data RunInstances = RunInstances
    { ImageId :: !String
    , MinCount :: !Int
    , MaxCount :: !Int
    , KeyName :: !(Maybe String)
    , GroupSet :: !GroupSet
    , UserData :: !(Maybe UserData)
    , Instance :: !String
    , Placement :: !(Maybe PlacementRequest)
    , KernelId :: !(Maybe String)
    , RamdiskId :: !(Maybe String)
    , BlockDeviceMapping :: !(Maybe BlockDeviceMapping)
    , Monitoring :: !(Maybe MonitoringInstance)
    , SubnetId :: !(Maybe String)
    , DisableApiTermination :: !(Maybe Boolean)
    , InstanceInitiatedShutdownBehavior :: !(Maybe String)
    , License :: !(Maybe InstanceLicenseRequest)
    , PrivateIpAddress :: !(Maybe String)
    , ClientToken :: !(Maybe String)
    , NetworkInterfaceSet :: !(Maybe InstanceNetworkInterfaceSetRequest)
    , IamInstanceProfile :: !(Maybe IamInstanceProfileRequest)
    , EbsOptimized :: !(Maybe Boolean)
    } deriving (Show)

data IamInstanceProfileRequest = IamInstanceProfileRequest
    { Arn :: !(Maybe String)
    , Name :: !(Maybe String)
    } deriving (Show)

data InstanceNetworkInterfaceSetRequest = InstanceNetworkInterfaceSetRequest
    { Item :: ![InstanceNetworkInterfaceSetItemRequest]
    } deriving (Show)

data InstanceNetworkInterfaceSetItemRequest = InstanceNetworkInterfaceSetItemRequest
    { NetworkInterfaceId :: !(Maybe String)
    , DeviceIndex :: !Int
    , SubnetId :: !(Maybe String)
    , Description :: !(Maybe String)
    , PrivateIpAddress :: !(Maybe String)
    , GroupSet :: !(Maybe SecurityGroupIdSet)
    , DeleteOnTermination :: !(Maybe Boolean)
    , PrivateIpAddressesSet :: !(Maybe PrivateIpAddressesSetRequest)
    , SecondaryPrivateIpAddressCount :: !(Maybe Int)
    } deriving (Show)

data PrivateIpAddressesSetRequest = PrivateIpAddressesSetRequest
    { Item :: ![PrivateIpAddressesSetItemRequest]
    } deriving (Show)

data PrivateIpAddressesSetItemRequest = PrivateIpAddressesSetItemRequest
    { PrivateIpAddress :: !String
    , Primary :: !(Maybe Boolean)
    } deriving (Show)

data ImportInstanceGroupSet = ImportInstanceGroupSet
    { Item :: ![ImportInstanceGroupItem]
    } deriving (Show)

data ImportInstanceGroupItem = ImportInstanceGroupItem
    { GroupId :: !(Maybe String)
    , GroupName :: !(Maybe String)
    } deriving (Show)

data GroupSet = GroupSet
    { Item :: ![GroupItem]
    } deriving (Show)

data GroupItem = GroupItem
    { GroupId :: !(Maybe String)
    , GroupName :: !(Maybe String)
    } deriving (Show)

data UserData = UserData
    { Data :: !(Maybe String)
    , Version :: !String
    , Encoding :: !String
    } deriving (Show)

data BlockDeviceMapping = BlockDeviceMapping
    { Item :: ![BlockDeviceMappingItem]
    } deriving (Show)

data BlockDeviceMappingItem = BlockDeviceMappingItem
    { DeviceName :: !String
  -- <xs:choice>
    , VirtualName :: !(Maybe String)
    , Ebs :: !(Maybe EbsBlockDevice)
   , NoDevice :: !(Maybe EmptyElement)
  -- </xs:choice>
    } deriving (Show)

data EbsBlockDevice = EbsBlockDevice
    { SnapshotId :: !(Maybe String)
    , VolumeSize :: !(Maybe Int)
    , DeleteOnTermination :: !(Maybe Boolean)
    , Volume :: !(Maybe String)
    , Iops :: !(Maybe Int)
    } deriving (Show)

data PlacementRequest = PlacementRequest
    { AvailabilityZone :: !(Maybe String)
    , GroupName :: !(Maybe String)
    , Tenancy :: !(Maybe String)
    } deriving (Show)

data SpotPlacementRequest = SpotPlacementRequest
    { AvailabilityZone :: !(Maybe String)
    , GroupName :: !(Maybe String)
    } deriving (Show)

data InstancePlacement = InstancePlacement
    { AvailabilityZone :: !(Maybe String)
    , GroupName :: !(Maybe String)
    } deriving (Show)

data MonitoringInstance = MonitoringInstance
    { Enabled :: !(Maybe Boolean)
    } deriving (Show)

data InstanceLicenseRequest = InstanceLicenseRequest
    { Pool :: !String
    } deriving (Show)

data RunInstancesResponse = RunInstancesResponse
    { RequestId :: !String
    , ReservationId :: !String
    , OwnerId :: !String
    , GroupSet :: !GroupSet
    , InstancesSet :: !RunningInstancesSet
    , RequesterId :: !(Maybe String)
    } deriving (Show)

data ReservationInfo = ReservationInfo
    { ReservationId :: !String
    , OwnerId :: !String
    , GroupSet :: !GroupSet
    , InstancesSet :: !RunningInstancesSet
    , RequesterId :: !(Maybe String)
    } deriving (Show)

data RunningInstancesSet = RunningInstancesSet
    { Item :: ![RunningInstancesItem]
    } deriving (Show)

data RunningInstancesItem = RunningInstancesItem
    { InstanceId :: !String
    , ImageId :: !(Maybe String)
    , InstanceState :: !InstanceState
    , PrivateDnsName :: !String
    , DnsName :: !(Maybe String)
    , Reason :: !(Maybe String)
    , KeyName :: !(Maybe String)
    , AmiLaunchIndex :: !(Maybe String)
    , ProductCodes :: !(Maybe ProductCodesSet)
    , Instance :: !String
    , LaunchTime :: !DateTime
    , Placement :: !(Maybe PlacementResponse)
    , KernelId :: !(Maybe String)
    , RamdiskId :: !(Maybe String)
    , Platform :: !(Maybe String)
    , Monitoring :: !(Maybe InstanceMonitoringState)
    , SubnetId :: !(Maybe String)
    , VpcId :: !(Maybe String)
    , PrivateIpAddress :: !(Maybe String)
    , IpAddress :: !(Maybe String)
    , SourceDestCheck :: !(Maybe Boolean)
    , GroupSet :: !GroupSet
    , StateReason :: !(Maybe StateReason)
    , Architecture :: !(Maybe String)
    , RootDevice :: !(Maybe String)
    , RootDeviceName :: !(Maybe String)
    , BlockDeviceMapping :: !(Maybe InstanceBlockDeviceMappingResponse)
    , InstanceLifecycle :: !(Maybe String)
    , SpotInstanceRequestId :: !(Maybe String)
    , License :: !(Maybe InstanceLicenseResponse)
    , Virtualization :: !(Maybe String)
    , ClientToken :: !(Maybe String)
    , TagSet :: !(Maybe ResourceTagSet)
    , Hypervisor :: !(Maybe String)
    , NetworkInterfaceSet :: !(Maybe InstanceNetworkInterfaceSet)
    , IamInstanceProfile :: !(Maybe IamInstanceProfileResponse)
    , EbsOptimized :: !(Maybe Boolean)
    } deriving (Show)

data IamInstanceProfileResponse = IamInstanceProfileResponse
    { Arn :: !String
    , Id :: !String
    } deriving (Show)

data InstanceNetworkInterfaceSet = InstanceNetworkInterfaceSet
    { Item :: ![InstanceNetworkInterfaceSetItem]
    } deriving (Show)

data InstanceNetworkInterfaceSetItem = InstanceNetworkInterfaceSetItem
    { NetworkInterfaceId :: !String
    , SubnetId :: !(Maybe String)
    , VpcId :: !(Maybe String)
    , Description :: !(Maybe String)
    , OwnerId :: !String
    , Status :: !String
    , MacAddress :: !(Maybe String)
    , PrivateIpAddress :: !(Maybe String)
    , PrivateDnsName :: !(Maybe String)
    , SourceDestCheck :: !(Maybe Boolean)
    , GroupSet :: !(Maybe GroupSet)
    , Attachment :: !InstanceNetworkInterfaceAttachment
    , Association :: !(Maybe InstanceNetworkInterfaceAssociation)
    , PrivateIpAddressesSet :: !(Maybe InstancePrivateIpAddressesSet)
    } deriving (Show)

data InstancePrivateIpAddressesSet = InstancePrivateIpAddressesSet
    { Item :: ![InstancePrivateIpAddressesSetItem]
    } deriving (Show)

data InstancePrivateIpAddressesSetItem = InstancePrivateIpAddressesSetItem
    { PrivateIpAddress :: !(Maybe String)
    , PrivateDnsName :: !(Maybe String)
    , Primary :: !(Maybe Boolean)
    , Association :: !(Maybe InstanceNetworkInterfaceAssociation)
    } deriving (Show)

data InstanceNetworkInterfaceAttachment = InstanceNetworkInterfaceAttachment
    { AttachmentId :: !String
    , DeviceIndex :: !Int
    , Status :: !String
    , AttachTime :: !DateTime
    , DeleteOnTermination :: !Boolean
    } deriving (Show)

data InstanceNetworkInterfaceAssociation = InstanceNetworkInterfaceAssociation
    { PublicIp :: !String
    , PublicDnsName :: !(Maybe String)
    , IpOwnerId :: !(Maybe String)
    } deriving (Show)

data PlacementResponse = PlacementResponse
    { AvailabilityZone :: !String
    , GroupName :: !(Maybe String)
    , Tenancy :: !(Maybe String)
    } deriving (Show)

data StateReason = StateReason
    { Code :: !String
    , Message :: !String
    } deriving (Show)

data InstanceBlockDeviceMappingResponse = InstanceBlockDeviceMappingResponse
    { Item :: ![InstanceBlockDeviceMappingResponseItem]
    } deriving (Show)

data InstanceBlockDeviceMappingResponseItem = InstanceBlockDeviceMappingResponseItem
    { DeviceName :: !String
  -- <xs:choice>
    , Ebs :: !EbsInstanceBlockDeviceMappingResponse
  -- </xs:choice>
    } deriving (Show)

data EbsInstanceBlockDeviceMappingResponse = EbsInstanceBlockDeviceMappingResponse
    { VolumeId :: !String
    , Status :: !String
    , AttachTime :: !DateTime
    , DeleteOnTermination :: !(Maybe Boolean)
    } deriving (Show)

data InstanceLicenseResponse = InstanceLicenseResponse
    { Pool :: !String
    } deriving (Show)

data DescribeAccountAttributes = DescribeAccountAttributes
    { AccountAttributeNameSet :: !(Maybe AccountAttributeNameSet)
    , FilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data DescribeAccountAttributesResponse = DescribeAccountAttributesResponse
    { RequestId :: !String
    , AccountAttributeSet :: !(Maybe AccountAttributeSet)
    } deriving (Show)

data AccountAttributeNameSet = AccountAttributeNameSet
    { Item :: ![AccountAttributeNameSetItem]
    } deriving (Show)

data AccountAttributeNameSetItem = AccountAttributeNameSetItem
    { AttributeName :: !String
    } deriving (Show)

data AccountAttributeSet = AccountAttributeSet
    { Item :: ![AccountAttributeSetItem]
    } deriving (Show)

data AccountAttributeSetItem = AccountAttributeSetItem
    { AttributeName :: !String
    , AttributeValueSet :: !AccountAttributeValueSet
    } deriving (Show)

data AccountAttributeValueSet = AccountAttributeValueSet
    { Item :: ![AccountAttributeValueSetItem]
    } deriving (Show)

data AccountAttributeValueSetItem = AccountAttributeValueSetItem
    { AttributeValue :: !String
    } deriving (Show)

data DescribeVpcAttribute = DescribeVpcAttribute
    { VpcId :: !String
    , DescribeVpcAttributesGroup :: !DescribeVpcAttributesGroup
    } deriving (Show)

data DescribeVpcAttributesGroup = DescribeVpcAttributesGroup
    { EnableDnsSupport :: !EmptyElement
    , EnableDnsHostnames :: !EmptyElement
    } deriving (Show)
data DescribeVpcAttributeResponse = DescribeVpcAttributeResponse
    { RequestId :: !String
    , VpcId :: !String
  -- <xs:choice>
    , EnableDnsSupport :: !(Maybe AttributeBooleanValue)
    , EnableDnsHostnames :: !(Maybe AttributeBooleanValue)
  -- </xs:choice>
    } deriving (Show)

data ModifyVpcAttribute = ModifyVpcAttribute
    { VpcId :: !String
  -- <xs:choice>
    , EnableDnsSupport :: !(Maybe AttributeBooleanValue)
    , EnableDnsHostnames :: !(Maybe AttributeBooleanValue)
  -- </xs:choice>
    } deriving (Show)

data ModifyVpcAttributeResponse = ModifyVpcAttributeResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data GetConsoleOutput = GetConsoleOutput
    { InstanceId :: !String
    } deriving (Show)

data GetConsoleOutputResponse = GetConsoleOutputResponse
    { RequestId :: !String
    , InstanceId :: !String
    , Timestamp :: !DateTime
    , Output :: !String
    } deriving (Show)

data GetPasswordData = GetPasswordData
    { InstanceId :: !String
    } deriving (Show)

data GetPasswordDataResponse = GetPasswordDataResponse
    { RequestId :: !String
    , InstanceId :: !String
    , Timestamp :: !DateTime
    , PasswordData :: !String
    } deriving (Show)

data InstanceId = InstanceId
    { InstanceId :: !String
    } deriving (Show)

data InstanceIdSet = InstanceIdSet
    { Item :: ![InstanceId]
    } deriving (Show)

data InstanceStateChange = InstanceStateChange
    { InstanceId :: !String
    , CurrentState :: !InstanceState
    , PreviousState :: !InstanceState
    } deriving (Show)

data InstanceStateChangeSet = InstanceStateChangeSet
    { Item :: ![InstanceStateChange]
    } deriving (Show)

data TerminateInstances = TerminateInstances
    { InstancesSet :: !InstanceIdSet
    } deriving (Show)

data TerminateInstancesResponse = TerminateInstancesResponse
    { RequestId :: !String
    , InstancesSet :: !InstanceStateChangeSet
    } deriving (Show)

data InstanceBlockDeviceMapping = InstanceBlockDeviceMapping
    { Item :: ![InstanceBlockDeviceMappingItem]
    } deriving (Show)

data InstanceBlockDeviceMappingItem = InstanceBlockDeviceMappingItem
    { DeviceName :: !String
  -- <xs:choice>
    , VirtualName :: !(Maybe String)
    , Ebs :: !(Maybe InstanceEbsBlockDevice)
    , NoDevice :: !(Maybe EmptyElement)
  -- </xs:choice>
    } deriving (Show)

data InstanceEbsBlockDevice = InstanceEbsBlockDevice
    { VolumeId :: !String
    , DeleteOnTermination :: !(Maybe Boolean)
    } deriving (Show)

data StopInstances = StopInstances
    { InstancesSet :: !InstanceIdSet
    , Force :: !(Maybe Boolean)
    } deriving (Show)

data StopInstancesResponse = StopInstancesResponse
    { RequestId :: !String
    , InstancesSet :: !InstanceStateChangeSet
    } deriving (Show)

data StartInstances = StartInstances
    { InstancesSet :: !InstanceIdSet
    } deriving (Show)

data StartInstancesResponse = StartInstancesResponse
    { RequestId :: !String
    , InstancesSet :: !InstanceStateChangeSet
    } deriving (Show)

data RebootInstances = RebootInstances
    { InstancesSet :: !RebootInstancesInfo
    } deriving (Show)

data RebootInstancesInfo = RebootInstancesInfo
    { Item :: !(NonEmpty RebootInstancesItem)
    } deriving (Show)

data RebootInstancesItem = RebootInstancesItem
    { InstanceId :: !String
    } deriving (Show)

data RebootInstancesResponse = RebootInstancesResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data DescribeInstances = DescribeInstances
    { InstancesSet :: !DescribeInstancesInfo
    , FilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data DescribeInstancesInfo = DescribeInstancesInfo
    { Item :: ![DescribeInstancesItem]
    } deriving (Show)

data DescribeInstancesItem = DescribeInstancesItem
    { InstanceId :: !String
    } deriving (Show)

data DescribeInstancesResponse = DescribeInstancesResponse
    { RequestId :: !String
    , ReservationSet :: !ReservationSet
    } deriving (Show)

data ReservationSet = ReservationSet
    { Item :: ![ReservationInfo]
    } deriving (Show)

data UnavailableResultSet = UnavailableResultSet
    { Item :: ![UnavailableResult]
    } deriving (Show)

data UnavailableResult = UnavailableResult
    { AvailabilityZone :: !String
    } deriving (Show)

data DescribeImages = DescribeImages
    { ExecutableBySet :: !(Maybe DescribeImagesExecutableBySet)
    , ImagesSet :: !DescribeImagesInfo
    , OwnersSet :: !(Maybe DescribeImagesOwners)
    , FilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data DescribeImagesInfo = DescribeImagesInfo
    { Item :: ![DescribeImagesItem]
    } deriving (Show)

data DescribeImagesItem = DescribeImagesItem
    { ImageId :: !String
    } deriving (Show)

data DescribeImagesOwners = DescribeImagesOwners
    { Item :: ![DescribeImagesOwner]
    } deriving (Show)

data DescribeImagesOwner = DescribeImagesOwner
    { Owner :: !String
    } deriving (Show)

data DescribeImagesExecutableBySet = DescribeImagesExecutableBySet
    { Item :: ![DescribeImagesExecutableBy]
    } deriving (Show)

data DescribeImagesExecutableBy = DescribeImagesExecutableBy
    { User :: !String
    } deriving (Show)

data DescribeImagesResponse = DescribeImagesResponse
    { RequestId :: !String
    , ImagesSet :: !DescribeImagesResponseInfo
    } deriving (Show)

data DescribeImagesResponseInfo = DescribeImagesResponseInfo
    { Item :: ![DescribeImagesResponseItem]
    } deriving (Show)

data DescribeImagesResponseItem = DescribeImagesResponseItem
    { ImageId :: !String
    , ImageLocation :: !(Maybe String)
    , ImageState :: !String
    , ImageOwnerId :: !String
    , IsPublic :: !Boolean
    , ProductCodes :: !(Maybe ProductCodesSet)
    , Architecture :: !(Maybe String)
    , Image :: !(Maybe String)
    , KernelId :: !(Maybe String)
    , RamdiskId :: !(Maybe String)
    , Platform :: !(Maybe String)
    , StateReason :: !(Maybe StateReason)
    , ImageOwnerAlias :: !(Maybe String)
    , Name :: !(Maybe String)
    , Description :: !(Maybe String)
    , RootDevice :: !(Maybe String)
    , RootDeviceName :: !(Maybe String)
    , BlockDeviceMapping :: !(Maybe BlockDeviceMapping)
    , Virtualization :: !(Maybe String)
    , TagSet :: !(Maybe ResourceTagSet)
    , Hypervisor :: !(Maybe String)
    } deriving (Show)

data CreateSecurityGroup = CreateSecurityGroup
    { GroupName :: !String
    , GroupDescription :: !String
    , VpcId :: !(Maybe String)
    } deriving (Show)

data CreateSecurityGroupResponse = CreateSecurityGroupResponse
    { RequestId :: !String
    , Return :: !Boolean
    , GroupId :: !String
    } deriving (Show)

data DeleteSecurityGroup = DeleteSecurityGroup
    {
-- <xs:choice>
      GroupId :: !(Maybe String)
    , GroupName :: !(Maybe String)
-- </xs:choice>
    } deriving (Show)

data DeleteSecurityGroupResponse = DeleteSecurityGroupResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data DescribeSecurityGroups = DescribeSecurityGroups
    { SecurityGroupSet :: !DescribeSecurityGroupsSet
    , SecurityGroupIdSet :: !(Maybe DescribeSecurityGroupsIdSet)
    , FilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data DescribeSecurityGroupsSet = DescribeSecurityGroupsSet
    { Item :: ![DescribeSecurityGroupsSetItem]
    } deriving (Show)

data DescribeSecurityGroupsSetItem = DescribeSecurityGroupsSetItem
    { GroupName :: !String
    } deriving (Show)

data DescribeSecurityGroupsIdSet = DescribeSecurityGroupsIdSet
    { Item :: ![DescribeSecurityGroupsIdSetItem]
    } deriving (Show)

data DescribeSecurityGroupsIdSetItem = DescribeSecurityGroupsIdSetItem
    { GroupId :: !String
    } deriving (Show)

data DescribeSecurityGroupsResponse = DescribeSecurityGroupsResponse
    { RequestId :: !String
    , SecurityGroupInfo :: !SecurityGroupSet
    } deriving (Show)

data IpPermissionSet = IpPermissionSet
    { Item :: ![IpPermission]
    } deriving (Show)

data IpPermission = IpPermission
    { IpProtocol :: !String
    , FromPort :: !(Maybe Int)
    , ToPort :: !(Maybe Int)
    , Groups :: !UserIdGroupPairSet
    , IpRanges :: !IpRangeSet
    } deriving (Show)

data IpRangeSet = IpRangeSet
    { Item :: ![IpRangeItem]
    } deriving (Show)

data IpRangeItem = IpRangeItem
    { CidrIp :: !String
    } deriving (Show)

data UserIdGroupPairSet = UserIdGroupPairSet
    { Item :: ![UserIdGroupPair]
    } deriving (Show)

data UserIdGroupPair = UserIdGroupPair
    { UserId :: !(Maybe String)
    , GroupId :: !(Maybe String)
    , GroupName :: !(Maybe String)
    } deriving (Show)

data SecurityGroupSet = SecurityGroupSet
    { Item :: ![SecurityGroupItem]
    } deriving (Show)

data SecurityGroupItem = SecurityGroupItem
    { OwnerId :: !String
    , GroupId :: !String
    , GroupName :: !String
    , GroupDescription :: !String
    , VpcId :: !(Maybe String)
    , IpPermissions :: !IpPermissionSet
    , IpPermissionsEgress :: !(Maybe IpPermissionSet)
    , TagSet :: !(Maybe ResourceTagSet)
    } deriving (Show)

data AuthorizeSecurityGroupIngress = AuthorizeSecurityGroupIngress
    { UserId :: !(Maybe String)
  -- <xs:choice>
    , GroupId :: !(Maybe String)
    , GroupName :: !(Maybe String)
  -- </xs:choice>
    , IpPermissions :: !IpPermissionSet
    } deriving (Show)

data AuthorizeSecurityGroupIngressResponse = AuthorizeSecurityGroupIngressResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data RevokeSecurityGroupIngress = RevokeSecurityGroupIngress
    { UserId :: !(Maybe String)
  -- <xs:choice>
    , GroupId :: !(Maybe String)
    , GroupName :: !(Maybe String)
  -- </xs:choice>
    , IpPermissions :: !IpPermissionSet
    } deriving (Show)

data RevokeSecurityGroupIngressResponse = RevokeSecurityGroupIngressResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data AuthorizeSecurityGroupEgress = AuthorizeSecurityGroupEgress
    { GroupId :: !String
    , IpPermissions :: !IpPermissionSet
    } deriving (Show)

data AuthorizeSecurityGroupEgressResponse = AuthorizeSecurityGroupEgressResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data RevokeSecurityGroupEgress = RevokeSecurityGroupEgress
    { GroupId :: !String
    , IpPermissions :: !IpPermissionSet
    } deriving (Show)

data RevokeSecurityGroupEgressResponse = RevokeSecurityGroupEgressResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data InstanceState = InstanceState
    { Code :: !Int
    , Name :: !String
    } deriving (Show)

data ModifyInstanceAttribute = ModifyInstanceAttribute
    { InstanceId :: !String
  -- <xs:choice>
    , Instance :: !(Maybe AttributeValue)
    , Kernel :: !(Maybe AttributeValue)
    , Ramdisk :: !(Maybe AttributeValue)
    , UserData :: !(Maybe AttributeValue)
    , DisableApiTermination :: !(Maybe AttributeBooleanValue)
    , InstanceInitiatedShutdownBehavior :: !(Maybe AttributeValue)
    , BlockDeviceMapping :: !(Maybe InstanceBlockDeviceMapping)
    , SourceDestCheck :: !(Maybe AttributeBooleanValue)
    , GroupSet :: !(Maybe SecurityGroupIdSet)
    , EbsOptimized :: !(Maybe AttributeBooleanValue)
  -- </xs:choice>
    } deriving (Show)

data SecurityGroupIdSet = SecurityGroupIdSet
    { Item :: ![SecurityGroupIdSetItem]
    } deriving (Show)

data SecurityGroupIdSetItem = SecurityGroupIdSetItem
    { GroupId :: !String
    } deriving (Show)

data ModifyInstanceAttributeResponse = ModifyInstanceAttributeResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data ResetInstanceAttribute = ResetInstanceAttribute
    { InstanceId :: !String
    , ResetInstanceAttributesGroup :: !ResetInstanceAttributesGroup
    } deriving (Show)

data ResetInstanceAttributesGroup = ResetInstanceAttributesGroup
    { Kernel :: !EmptyElement
    , Ramdisk :: !EmptyElement
    , SourceDestCheck :: !EmptyElement
    } deriving (Show)
data ResetInstanceAttributeResponse = ResetInstanceAttributeResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data DescribeInstanceAttribute = DescribeInstanceAttribute
    { InstanceId :: !String
    , DescribeInstanceAttributesGroup :: !DescribeInstanceAttributesGroup
    } deriving (Show)

data DescribeInstanceAttributesGroup = DescribeInstanceAttributesGroup
    { Instance :: !EmptyElement
    , Kernel :: !EmptyElement
    , Ramdisk :: !EmptyElement
    , UserData :: !EmptyElement
    , DisableApiTermination :: !EmptyElement
    , InstanceInitiatedShutdownBehavior :: !EmptyElement
    , RootDeviceName :: !EmptyElement
    , BlockDeviceMapping :: !EmptyElement
    , SourceDestCheck :: !EmptyElement
    , GroupSet :: !EmptyElement
    , ProductCodes :: !EmptyElement
    , EbsOptimized :: !EmptyElement
    } deriving (Show)
data DescribeInstanceAttributeResponse = DescribeInstanceAttributeResponse
    { RequestId :: !String
    , InstanceId :: !String
  -- <xs:choice>
    , Instance :: !(Maybe NullableAttributeValue)
    , Kernel :: !(Maybe NullableAttributeValue)
    , Ramdisk :: !(Maybe NullableAttributeValue)
    , UserData :: !(Maybe NullableAttributeValue)
    , DisableApiTermination :: !(Maybe NullableAttributeBooleanValue)
    , InstanceInitiatedShutdownBehavior :: !(Maybe NullableAttributeValue)
    , RootDeviceName :: !(Maybe NullableAttributeValue)
    , BlockDeviceMapping :: !(Maybe InstanceBlockDeviceMappingResponse)
    , SourceDestCheck :: !(Maybe NullableAttributeBooleanValue)
    , GroupSet :: !(Maybe GroupSet)
    , ProductCodes :: !(Maybe ProductCodesSet)
    , EbsOptimized :: !(Maybe NullableAttributeBooleanValue)
  -- </xs:choice>
    } deriving (Show)

data ModifyImageAttribute = ModifyImageAttribute
    { ImageId :: !String
  -- <xs:choice>
    , LaunchPermission :: !(Maybe LaunchPermissionOperation)
    , ProductCodes :: !(Maybe ProductCodeList)
    , Description :: !(Maybe AttributeValue)
  -- </xs:choice>
    } deriving (Show)

data LaunchPermissionOperation = LaunchPermissionOperation
    {
-- <xs:choice>
    , Add :: !(Maybe LaunchPermissionList)
    , Remove :: !(Maybe LaunchPermissionList)
-- </xs:choice>
    } deriving (Show)

data LaunchPermissionList = LaunchPermissionList
    { Item :: ![LaunchPermissionItem]
    } deriving (Show)

data LaunchPermissionItem = LaunchPermissionItem
    {
-- <xs:choice>
    , UserId :: !(Maybe String)
    , Group :: !(Maybe String)
-- </xs:choice>
    } deriving (Show)

data ProductCodeList = ProductCodeList
    { Item :: ![ProductCodeItem]
    } deriving (Show)

data ProductCodeItem = ProductCodeItem
    { ProductCode :: !String
    } deriving (Show)

data ModifyImageAttributeResponse = ModifyImageAttributeResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data ResetImageAttribute = ResetImageAttribute
    { ImageId :: !String
    , ResetImageAttributesGroup :: !ResetImageAttributesGroup
    } deriving (Show)

data ResetImageAttributesGroup = ResetImageAttributesGroup
    { LaunchPermission :: !EmptyElement
    } deriving (Show)

data EmptyElement = EmptyElement

data ResetImageAttributeResponse = ResetImageAttributeResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data DescribeImageAttribute = DescribeImageAttribute
    { ImageId :: !String
    , DescribeImageAttributesGroup :: !DescribeImageAttributesGroup
    } deriving (Show)

data DescribeImageAttributesGroup = DescribeImageAttributesGroup
    { LaunchPermission :: !EmptyElement
    , ProductCodes :: !EmptyElement
    , Kernel :: !EmptyElement
    , Ramdisk :: !EmptyElement
    , BlockDeviceMapping :: !EmptyElement
    , Description :: !EmptyElement
    , InstanceCategory :: !EmptyElement
    } deriving (Show)
data DescribeImageAttributeResponse = DescribeImageAttributeResponse
    { RequestId :: !String
    , ImageId :: !String
  -- <xs:choice>
    , LaunchPermission :: !(Maybe LaunchPermissionList)
    , ProductCodes :: !(Maybe ProductCodesSet)
    , Kernel :: !(Maybe NullableAttributeValue)
    , Ramdisk :: !(Maybe NullableAttributeValue)
    , Description :: !(Maybe NullableAttributeValue)
    , BlockDeviceMapping :: !(Maybe BlockDeviceMapping)
  -- </xs:choice>
    } deriving (Show)

data NullableAttributeValue = NullableAttributeValue
    { Value :: !(Maybe String)
    } deriving (Show)

data NullableAttributeBooleanValue = NullableAttributeBooleanValue
    { Value :: !(Maybe Boolean)
    } deriving (Show)

data AttributeValue = AttributeValue
    { Value :: !String
    } deriving (Show)

data AttributeBooleanValue = AttributeBooleanValue
    { Value :: !Boolean
    } deriving (Show)

data ConfirmProductInstance = ConfirmProductInstance
    { ProductCode :: !String
    , InstanceId :: !String
    } deriving (Show)

data ProductCodesSet = ProductCodesSet
    { Item :: ![ProductCodesSetItem]
    } deriving (Show)

data ProductCodesSetItem = ProductCodesSetItem
    { ProductCode :: !String
    , Type :: !String
    } deriving (Show)

data ConfirmProductInstanceResponse = ConfirmProductInstanceResponse
    { RequestId :: !String
    , Return :: !Boolean
    , OwnerId :: !(Maybe String)
    } deriving (Show)

data DescribeAvailabilityZones = DescribeAvailabilityZones
    { AvailabilityZoneSet :: !DescribeAvailabilityZonesSet
    , FilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data DescribeAvailabilityZonesSet = DescribeAvailabilityZonesSet
    { Item :: ![DescribeAvailabilityZonesSetItem]
    } deriving (Show)

data DescribeAvailabilityZonesSetItem = DescribeAvailabilityZonesSetItem
    { ZoneName :: !String
    } deriving (Show)

data DescribeAvailabilityZonesResponse = DescribeAvailabilityZonesResponse
    { RequestId :: !String
    , AvailabilityZoneInfo :: !AvailabilityZoneSet
    } deriving (Show)

data AvailabilityZoneSet = AvailabilityZoneSet
    { Item :: ![AvailabilityZoneItem]
    } deriving (Show)

data AvailabilityZoneMessage = AvailabilityZoneMessage
    { Message :: !String
    } deriving (Show)

data AvailabilityZoneMessageSet = AvailabilityZoneMessageSet
    { Item :: ![AvailabilityZoneMessage]
    } deriving (Show)

data AvailabilityZoneItem = AvailabilityZoneItem
    { ZoneName :: !String
    , ZoneState :: !String
    , RegionName :: !String
    , MessageSet :: !AvailabilityZoneMessageSet
    } deriving (Show)

data AllocateAddress = AllocateAddress
    { Domain :: !(Maybe String)
    } deriving (Show)

data AllocateAddressResponse = AllocateAddressResponse
    { RequestId :: !String
    , PublicIp :: !String
    , Domain :: !String
    , AllocationId :: !(Maybe String)
    } deriving (Show)

data ReleaseAddress = ReleaseAddress
    {
  -- <xs:choice>
    , PublicIp :: !(Maybe String)
    , AllocationId :: !(Maybe String)
  -- </xs:choice>
    } deriving (Show)

data ReleaseAddressResponse = ReleaseAddressResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data DescribeAddresses = DescribeAddresses
    { PublicIpsSet :: !DescribeAddressesInfo
    , AllocationIdsSet :: !AllocationIdSet
    , FilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data AllocationIdSet = AllocationIdSet
    { Item :: ![AllocationIdSetItem]
    } deriving (Show)

data AllocationIdSetItem = AllocationIdSetItem
    { AllocationId :: !String
    } deriving (Show)

data DescribeAddressesInfo = DescribeAddressesInfo
    { Item :: ![DescribeAddressesItem]
    } deriving (Show)

data DescribeAddressesItem = DescribeAddressesItem
    { PublicIp :: !String
    } deriving (Show)

data DescribeAddressesResponse = DescribeAddressesResponse
    { RequestId :: !String
    , AddressesSet :: !DescribeAddressesResponseInfo
    } deriving (Show)

data DescribeAddressesResponseInfo = DescribeAddressesResponseInfo
    { Item :: ![DescribeAddressesResponseItem]
    } deriving (Show)

data DescribeAddressesResponseItem = DescribeAddressesResponseItem
    { PublicIp :: !String
    , AllocationId :: !(Maybe String)
    , Domain :: !String
    , InstanceId :: !(Maybe String)
    , AssociationId :: !(Maybe String)
    , NetworkInterfaceId :: !(Maybe String)
    , NetworkInterfaceOwnerId :: !(Maybe String)
    , PrivateIpAddress :: !(Maybe String)
    } deriving (Show)

data AssociateAddress = AssociateAddress
    {
  -- <xs:choice>
    , PublicIp :: !(Maybe String)
    , AllocationId :: !(Maybe String)
  -- </xs:choice>
  -- <xs:choice>
    , NetworkInterfaceId :: !(Maybe String)
    , InstanceId :: !(Maybe String)
  -- </xs:choice>
    , PrivateIpAddress :: !(Maybe String)
    , AllowReassociation :: !(Maybe Boolean)
    } deriving (Show)

data AssociateAddressResponse = AssociateAddressResponse
    { RequestId :: !String
    , Return :: !Boolean
    , AssociationId :: !(Maybe String)
    } deriving (Show)

data DisassociateAddress = DisassociateAddress
    {
-- <xs:choice>
    , PublicIp :: !(Maybe String)
    , AssociationId :: !(Maybe String)
-- </xs:choice>
     } deriving (Show)

data DisassociateAddressResponse = DisassociateAddressResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data CreateVolume = CreateVolume
    { Size :: !(Maybe String)
    , SnapshotId :: !(Maybe String)
    , AvailabilityZone :: !String
    , Volume :: !(Maybe String)
    , Iops :: !(Maybe Int)
    } deriving (Show)

data CreateVolumeResponse = CreateVolumeResponse
    { RequestId :: !String
    , VolumeId :: !String
    , Size :: !String
    , SnapshotId :: !String
    , AvailabilityZone :: !String
    , Status :: !String
    , CreateTime :: !DateTime
    , Volume :: !String
    , Iops :: !(Maybe Int)
    } deriving (Show)

data DeleteVolume = DeleteVolume
    { VolumeId :: !String
    } deriving (Show)

data DeleteVolumeResponse = DeleteVolumeResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data DescribeVolumes = DescribeVolumes
    { VolumeSet :: !DescribeVolumesSet
    , FilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data DescribeVolumesSet = DescribeVolumesSet
    { Item :: ![DescribeVolumesSetItem]
    } deriving (Show)

data DescribeVolumesSetItem = DescribeVolumesSetItem
    { VolumeId :: !String
    } deriving (Show)

data DescribeVolumesResponse = DescribeVolumesResponse
    { RequestId :: !String
    , VolumeSet :: !DescribeVolumesSetResponse
    } deriving (Show)

data DescribeVolumesSetResponse = DescribeVolumesSetResponse
    { Item :: ![DescribeVolumesSetItemResponse]
    } deriving (Show)

data DescribeVolumesSetItemResponse = DescribeVolumesSetItemResponse
    { VolumeId :: !String
    , Size :: !String
    , SnapshotId :: !String
    , AvailabilityZone :: !String
    , Status :: !String
    , CreateTime :: !DateTime
    , AttachmentSet :: !AttachmentSetResponse
    , TagSet :: !(Maybe ResourceTagSet)
    , Volume :: !String
    , Iops :: !(Maybe Int)
    } deriving (Show)

data AttachmentSetResponse = AttachmentSetResponse
    { Item :: ![AttachmentSetItemResponse]
    } deriving (Show)

data AttachmentSetItemResponse = AttachmentSetItemResponse
    { VolumeId :: !String
    , InstanceId :: !String
    , Device :: !String
    , Status :: !String
    , AttachTime :: !DateTime
    , DeleteOnTermination :: !Boolean
    } deriving (Show)

data AttachVolume = AttachVolume
    { VolumeId :: !String
    , InstanceId :: !String
    , Device :: !String
    } deriving (Show)

data AttachVolumeResponse = AttachVolumeResponse
    { RequestId :: !String
    , VolumeId :: !String
    , InstanceId :: !String
    , Device :: !String
    , Status :: !String
    , AttachTime :: !DateTime
    } deriving (Show)

data DetachVolume = DetachVolume
    { VolumeId :: !String
    , InstanceId :: !(Maybe String)
    , Device :: !(Maybe String)
    , Force :: !(Maybe Boolean)
    } deriving (Show)

data DetachVolumeResponse = DetachVolumeResponse
    { RequestId :: !String
    , VolumeId :: !String
    , InstanceId :: !String
    , Device :: !String
    , Status :: !String
    , AttachTime :: !DateTime
    } deriving (Show)

data CreateSnapshot = CreateSnapshot
    { VolumeId :: !String
    , Description :: !(Maybe String)
    } deriving (Show)

data CreateSnapshotResponse = CreateSnapshotResponse
    { RequestId :: !String
    , SnapshotId :: !String
    , VolumeId :: !String
    , Status :: !String
    , StartTime :: !DateTime
    , Progress :: !String
    , OwnerId :: !String
    , VolumeSize :: !String
    , Description :: !(Maybe String)
    } deriving (Show)

data CopySnapshot = CopySnapshot
    { SourceRegion :: !String
    , SourceSnapshotId :: !String
    , Description :: !(Maybe String)
    } deriving (Show)

data CopySnapshotResponse = CopySnapshotResponse
    { RequestId :: !String
    , SnapshotId :: !String
    } deriving (Show)

data DeleteSnapshot = DeleteSnapshot
    { SnapshotId :: !String
    } deriving (Show)

data DeleteSnapshotResponse = DeleteSnapshotResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data DescribeSnapshots = DescribeSnapshots
    { SnapshotSet :: !DescribeSnapshotsSet
    , OwnersSet :: !(Maybe DescribeSnapshotsOwners)
    , RestorableBySet :: !(Maybe DescribeSnapshotsRestorableBySet)
    , FilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data DescribeSnapshotsSet = DescribeSnapshotsSet
    { Item :: ![DescribeSnapshotsSetItem]
    } deriving (Show)

data DescribeSnapshotsSetItem = DescribeSnapshotsSetItem
    { SnapshotId :: !String
    } deriving (Show)

data DescribeSnapshotsOwners = DescribeSnapshotsOwners
    { Item :: ![DescribeSnapshotsOwner]
    } deriving (Show)

data DescribeSnapshotsOwner = DescribeSnapshotsOwner
    { Owner :: !String
    } deriving (Show)

data DescribeSnapshotsRestorableBySet = DescribeSnapshotsRestorableBySet
    { Item :: ![DescribeSnapshotsRestorableBy]
    } deriving (Show)

data DescribeSnapshotsRestorableBy = DescribeSnapshotsRestorableBy
    { User :: !String
    } deriving (Show)

data DescribeSnapshotsResponse = DescribeSnapshotsResponse
    { RequestId :: !String
    , SnapshotSet :: !DescribeSnapshotsSetResponse
    } deriving (Show)

data DescribeSnapshotsSetResponse = DescribeSnapshotsSetResponse
    { Item :: ![DescribeSnapshotsSetItemResponse]
    } deriving (Show)

data DescribeSnapshotsSetItemResponse = DescribeSnapshotsSetItemResponse
    { SnapshotId :: !String
    , VolumeId :: !String
    , Status :: !String
    , StartTime :: !DateTime
    , Progress :: !String
    , OwnerId :: !String
    , VolumeSize :: !String
    , Description :: !(Maybe String)
    , OwnerAlias :: !(Maybe String)
    , TagSet :: !(Maybe ResourceTagSet)
    } deriving (Show)

data ModifySnapshotAttribute = ModifySnapshotAttribute
    { SnapshotId :: !String
    , CreateVolumePermission :: !CreateVolumePermissionOperation
    } deriving (Show)

data CreateVolumePermissionOperation = CreateVolumePermissionOperation
    {
-- <xs:choice>
    , Add :: !(Maybe CreateVolumePermissionList)
    , Remove :: !(Maybe CreateVolumePermissionList)
-- </xs:choice>
    } deriving (Show)

data CreateVolumePermissionList = CreateVolumePermissionList
    { Item :: ![CreateVolumePermissionItem]
    } deriving (Show)

data CreateVolumePermissionItem = CreateVolumePermissionItem
    {
-- <xs:choice>
    , UserId :: !(Maybe String)
    , Group :: !(Maybe String)
-- </xs:choice>
    } deriving (Show)

data ModifySnapshotAttributeResponse = ModifySnapshotAttributeResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data ResetSnapshotAttribute = ResetSnapshotAttribute
    { SnapshotId :: !String
    , ResetSnapshotAttributesGroup :: !ResetSnapshotAttributesGroup
    } deriving (Show)

data ResetSnapshotAttributesGroup = ResetSnapshotAttributesGroup
    { CreateVolumePermission :: !EmptyElement
    } deriving (Show)
data ResetSnapshotAttributeResponse = ResetSnapshotAttributeResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data DescribeSnapshotAttribute = DescribeSnapshotAttribute
    { SnapshotId :: !String
    , DescribeSnapshotAttributesGroup :: !DescribeSnapshotAttributesGroup
    } deriving (Show)

data DescribeSnapshotAttributesGroup = DescribeSnapshotAttributesGroup
    { CreateVolumePermission :: !EmptyElement
    , ProductCodes :: !EmptyElement
    } deriving (Show)
data DescribeSnapshotAttributeResponse = DescribeSnapshotAttributeResponse
    { RequestId :: !String
    , SnapshotId :: !String
  -- <xs:choice>
    , CreateVolumePermission :: !(Maybe CreateVolumePermissionList)
    , ProductCodes :: !(Maybe ProductCodesSet)
  -- </xs:choice>
    } deriving (Show)

data BundleInstance = BundleInstance
    { InstanceId :: !String
    , Storage :: !BundleInstanceTaskStorage
    } deriving (Show)

data BundleInstanceTaskStorage = BundleInstanceTaskStorage
    { S3 :: !BundleInstanceS3Storage
    } deriving (Show)

data BundleInstanceS3Storage = BundleInstanceS3Storage
    { Bucket :: !String
    , Prefix :: !String
    , AwsAccessKeyId :: !(Maybe String)
    , UploadPolicy :: !(Maybe String)
    , UploadPolicySignature :: !(Maybe String)
    } deriving (Show)

data BundleInstanceResponse = BundleInstanceResponse
    { RequestId :: !String
    , BundleInstanceTask :: !BundleInstanceTask
    } deriving (Show)

data BundleInstanceTask = BundleInstanceTask
    { InstanceId :: !String
    , BundleId :: !String
    , State :: !String
    , StartTime :: !DateTime
    , UpdateTime :: !DateTime
    , Storage :: !BundleInstanceTaskStorage
    , Progress :: !(Maybe String)
    , Error :: !(Maybe BundleInstanceTaskError)
    } deriving (Show)

data BundleInstanceTaskError = BundleInstanceTaskError
    { Code :: !String
    , Message :: !String
    } deriving (Show)

data DescribeBundleTasks = DescribeBundleTasks
    { BundlesSet :: !DescribeBundleTasksInfo
    , FilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data DescribeBundleTasksInfo = DescribeBundleTasksInfo
    { Item :: ![DescribeBundleTasksItem]
    } deriving (Show)

data DescribeBundleTasksItem = DescribeBundleTasksItem
    { BundleId :: !String
    } deriving (Show)

data DescribeBundleTasksResponse = DescribeBundleTasksResponse
    { RequestId :: !String
    , BundleInstanceTasksSet :: !BundleInstanceTasksSet
    } deriving (Show)

data BundleInstanceTasksSet = BundleInstanceTasksSet
    { Item :: ![BundleInstanceTask]
    } deriving (Show)

data CancelBundleTask = CancelBundleTask
    { BundleId :: !String
    } deriving (Show)

data CancelBundleTaskResponse = CancelBundleTaskResponse
    { RequestId :: !String
    , BundleInstanceTask :: !BundleInstanceTask
    } deriving (Show)

data CopyImage = CopyImage
    { SourceRegion :: !String
    , SourceImageId :: !String
    , Name :: !String
    , Description :: !(Maybe String)
    , ClientToken :: !(Maybe String)
    } deriving (Show)

data CopyImageResponse = CopyImageResponse
    { RequestId :: !String
    , ImageId :: !String
    } deriving (Show)

data DescribeRegions = DescribeRegions
    { RegionSet :: !DescribeRegionsSet
    , FilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data DescribeRegionsSet = DescribeRegionsSet
    { Item :: ![DescribeRegionsSetItem]
    } deriving (Show)

data DescribeRegionsSetItem = DescribeRegionsSetItem
    { RegionName :: !String
    } deriving (Show)

data DescribeRegionsResponse = DescribeRegionsResponse
    { RequestId :: !String
    , RegionInfo :: !RegionSet
    } deriving (Show)

data RegionSet = RegionSet
    { Item :: ![RegionItem]
    } deriving (Show)

data RegionItem = RegionItem
    { RegionName :: !String
    , RegionEndpoint :: !String
    } deriving (Show)

data DescribeReservedInstancesOfferings = DescribeReservedInstancesOfferings
    { ReservedInstancesOfferingsSet :: !(Maybe DescribeReservedInstancesOfferingsSet)
    , Instance :: !(Maybe String)
    , AvailabilityZone :: !(Maybe String)
    , ProductDescription :: !(Maybe String)
    , FilterSet :: !(Maybe FilterSet)
    , InstanceTenancy :: !(Maybe String)
    , Offering :: !(Maybe String)
    , IncludeMarketplace :: !(Maybe Boolean)
    , MinDuration :: !(Maybe Long)
    , MaxDuration :: !(Maybe Long)
    , MaxInstanceCount :: !(Maybe Int)
    , NextToken :: !(Maybe String)
    , MaxResults :: !(Maybe Int)
    } deriving (Show)

data DescribeReservedInstancesOfferingsSet = DescribeReservedInstancesOfferingsSet
    { Item :: ![DescribeReservedInstancesOfferingsSetItem]
    } deriving (Show)

data DescribeReservedInstancesOfferingsSetItem = DescribeReservedInstancesOfferingsSetItem
    { ReservedInstancesOfferingId :: !String
    } deriving (Show)

data DescribeReservedInstancesOfferingsResponse = DescribeReservedInstancesOfferingsResponse
    { RequestId :: !String
    , ReservedInstancesOfferingsSet :: !DescribeReservedInstancesOfferingsResponseSet
    , NextToken :: !(Maybe String)
    } deriving (Show)

data DescribeReservedInstancesOfferingsResponseSet = DescribeReservedInstancesOfferingsResponseSet
    { Item :: ![DescribeReservedInstancesOfferingsResponseSetItem]
    } deriving (Show)

data DescribeReservedInstancesOfferingsResponseSetItem = DescribeReservedInstancesOfferingsResponseSetItem
    { ReservedInstancesOfferingId :: !String
    , Instance :: !String
    , AvailabilityZone :: !String
    , Duration :: !Long
    , FixedPrice :: !Double
    , UsagePrice :: !Double
    , ProductDescription :: !String
    , InstanceTenancy :: !String
    , CurrencyCode :: !String
    , Offering :: !String
    , RecurringCharges :: !RecurringChargesSet
    , Marketplace :: !(Maybe Boolean)
    , PricingDetailsSet :: !(Maybe PricingDetailsSet)
    } deriving (Show)

data RecurringChargesSet = RecurringChargesSet
    { Item :: ![RecurringChargesSetItem]
    } deriving (Show)

data RecurringChargesSetItem = RecurringChargesSetItem
    { Frequency :: !String
    , Amount :: !Double
    } deriving (Show)

data PricingDetailsSet = PricingDetailsSet
    { Item :: ![PricingDetailsSetItem]
    } deriving (Show)

data PricingDetailsSetItem = PricingDetailsSetItem
    { Price :: !Double
    , Count :: !Int
    } deriving (Show)

data PurchaseReservedInstancesOffering = PurchaseReservedInstancesOffering
    { ReservedInstancesOfferingId :: !String
    , InstanceCount :: !Int
    , LimitPrice :: !(Maybe ReservedInstanceLimitPrice)
    } deriving (Show)

data ReservedInstanceLimitPrice = ReservedInstanceLimitPrice
    { Amount :: !Double
    , CurrencyCode :: !(Maybe String)
    } deriving (Show)

data PurchaseReservedInstancesOfferingResponse = PurchaseReservedInstancesOfferingResponse
    { RequestId :: !String
    , ReservedInstancesId :: !String
    } deriving (Show)

data DescribeReservedInstances = DescribeReservedInstances
    { ReservedInstancesSet :: !(Maybe DescribeReservedInstancesSet)
    , FilterSet :: !(Maybe FilterSet)
    , Offering :: !(Maybe String)
    } deriving (Show)

data DescribeReservedInstancesSet = DescribeReservedInstancesSet
    { Item :: ![DescribeReservedInstancesSetItem]
    } deriving (Show)

data DescribeReservedInstancesSetItem = DescribeReservedInstancesSetItem
    { ReservedInstancesId :: !String
    } deriving (Show)

data DescribeReservedInstancesResponse = DescribeReservedInstancesResponse
    { RequestId :: !String
    , ReservedInstancesSet :: !DescribeReservedInstancesResponseSet
    } deriving (Show)

data DescribeReservedInstancesResponseSet = DescribeReservedInstancesResponseSet
    { Item :: ![DescribeReservedInstancesResponseSetItem]
    } deriving (Show)

data DescribeReservedInstancesResponseSetItem = DescribeReservedInstancesResponseSetItem
    { ReservedInstancesId :: !String
    , Instance :: !String
    , AvailabilityZone :: !String
    , Start :: !DateTime
    , Duration :: !Long
    , FixedPrice :: !Double
    , UsagePrice :: !Double
    , InstanceCount :: !Integer
    , ProductDescription :: !String
    , State :: !String
    , TagSet :: !(Maybe ResourceTagSet)
    , InstanceTenancy :: !String
    , CurrencyCode :: !String
    , Offering :: !String
    , RecurringCharges :: !(Maybe RecurringChargesSet)
    } deriving (Show)

data CreateReservedInstancesListing = CreateReservedInstancesListing
    { ReservedInstancesId :: !String
    , InstanceCount :: !(Maybe Int)
    , PriceSchedules :: !PriceScheduleRequestSet
    , ClientToken :: !String
    } deriving (Show)

data PriceScheduleRequestSet = PriceScheduleRequestSet
    { Item :: ![PriceScheduleRequestSetItem]
    } deriving (Show)

data PriceScheduleRequestSetItem = PriceScheduleRequestSetItem
    { Term :: !Long
    , Price :: !Double
    , CurrencyCode :: !(Maybe String)
    } deriving (Show)

data CreateReservedInstancesListingResponse = CreateReservedInstancesListingResponse
    { RequestId :: !String
    , ReservedInstancesListingsSet :: !DescribeReservedInstancesListingsResponseSet
    } deriving (Show)

data CancelReservedInstancesListing = CancelReservedInstancesListing
    { ReservedInstancesListingId :: !String
    } deriving (Show)

data CancelReservedInstancesListingResponse = CancelReservedInstancesListingResponse
    { RequestId :: !String
    , ReservedInstancesListingsSet :: !DescribeReservedInstancesListingsResponseSet
    } deriving (Show)

data DescribeReservedInstancesListings = DescribeReservedInstancesListings
    { ReservedInstancesListingSet :: !(Maybe DescribeReservedInstancesListingSet)
    , ReservedInstancesSet :: !(Maybe DescribeReservedInstancesSet)
    , FilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data DescribeReservedInstancesListingSet = DescribeReservedInstancesListingSet
    { Item :: ![DescribeReservedInstancesListingSetItem]
    } deriving (Show)

data DescribeReservedInstancesListingSetItem = DescribeReservedInstancesListingSetItem
    { ReservedInstancesListingId :: !String
    } deriving (Show)

data DescribeReservedInstancesListingsResponse = DescribeReservedInstancesListingsResponse
    { RequestId :: !String
    , ReservedInstancesListingsSet :: !DescribeReservedInstancesListingsResponseSet
    } deriving (Show)

data DescribeReservedInstancesListingsResponseSet = DescribeReservedInstancesListingsResponseSet
    { Item :: ![DescribeReservedInstancesListingsResponseSetItem]
    } deriving (Show)

data DescribeReservedInstancesListingsResponseSetItem = DescribeReservedInstancesListingsResponseSetItem
    { ReservedInstancesListingId :: !String
    , ReservedInstancesId :: !String
    , CreateDate :: !DateTime
    , UpdateDate :: !DateTime
    , Status :: !String
    , StatusMessage :: !String
    , InstanceCounts :: !InstanceCountsSet
    , PriceSchedules :: !PriceScheduleSet
    , TagSet :: !(Maybe ResourceTagSet)
    , ClientToken :: !(Maybe String)
    } deriving (Show)

data InstanceCountsSet = InstanceCountsSet
    { Item :: ![InstanceCountsSetItem]
    } deriving (Show)

data InstanceCountsSetItem = InstanceCountsSetItem
    { State :: !String
    , InstanceCount :: !Int
    } deriving (Show)

data PriceScheduleSet = PriceScheduleSet
    { Item :: ![PriceScheduleSetItem]
    } deriving (Show)

data PriceScheduleSetItem = PriceScheduleSetItem
    { Term :: !Long
    , Price :: !Double
    , CurrencyCode :: !(Maybe String)
    , Active :: !Boolean
    } deriving (Show)

data MonitorInstances = MonitorInstances
    { InstancesSet :: !MonitorInstancesSet
    } deriving (Show)

data MonitorInstancesSet = MonitorInstancesSet
    { Item :: !(NonEmpty MonitorInstancesSetItem)
    } deriving (Show)

data MonitorInstancesSetItem = MonitorInstancesSetItem
    { InstanceId :: !String
    } deriving (Show)

data MonitorInstancesResponse = MonitorInstancesResponse
    { RequestId :: !String
    , InstancesSet :: !MonitorInstancesResponseSet
    } deriving (Show)

data MonitorInstancesResponseSet = MonitorInstancesResponseSet
    { Item :: !(NonEmpty MonitorInstancesResponseSetItem)
    } deriving (Show)

data MonitorInstancesResponseSetItem = MonitorInstancesResponseSetItem
    { InstanceId :: !String
    , Monitoring :: !InstanceMonitoringState
    } deriving (Show)

data InstanceMonitoringState = InstanceMonitoringState
    { State :: !String
    } deriving (Show)

data Attachment = Attachment
    { VpcId :: !String
    , State :: !String
    } deriving (Show)

data AttachmentSet = AttachmentSet
    { Item :: ![Attachment]
    } deriving (Show)

data VpnGateway = VpnGateway
    { VpnGatewayId :: !String
    , State :: !String
    , Type :: !String
    , AvailabilityZone :: !(Maybe String)
    , Attachments :: !AttachmentSet
    , TagSet :: !(Maybe ResourceTagSet)
    } deriving (Show)

data CustomerGateway = CustomerGateway
    { CustomerGatewayId :: !String
    , State :: !String
    , Type :: !String
    , IpAddress :: !String
    , BgpAsn :: !(Maybe Int)
    , TagSet :: !(Maybe ResourceTagSet)
    } deriving (Show)

data VpnConnection = VpnConnection
    { VpnConnectionId :: !String
    , State :: !String
    , CustomerGatewayConfiguration :: !(Maybe String)
    , Type :: !(Maybe String)
    , CustomerGatewayId :: !String
    , VpnGatewayId :: !String
    , TagSet :: !(Maybe ResourceTagSet)
    , VgwTelemetry :: !(Maybe VgwTelemetry)
    , Options :: !(Maybe VpnConnectionOptionsResponse)
    , Routes :: !(Maybe VpnStaticRoutesSet)
    } deriving (Show)

data VpnConnectionOptionsResponse = VpnConnectionOptionsResponse
    { StaticRoutesOnly :: !(Maybe Boolean)
    } deriving (Show)

data VpnStaticRoutesSet = VpnStaticRoutesSet
    { Item :: ![VpnStaticRoute]
    } deriving (Show)

data VpnStaticRoute = VpnStaticRoute
    { DestinationCidrBlock :: !String
    , Source :: !String
    , State :: !String
    } deriving (Show)

data VgwTelemetry = VgwTelemetry
    { Item :: ![VpnTunnelTelemetry]
    } deriving (Show)

data VpnTunnelTelemetry = VpnTunnelTelemetry
    { OutsideIpAddress :: !String
    , Status :: !String
    , LastStatusChange :: !DateTime
    , StatusMessage :: !(Maybe String)
    , AcceptedRouteCount :: !Int
    } deriving (Show)

data Vpc = Vpc
    { VpcId :: !String
    , State :: !(Maybe String)
    , CidrBlock :: !(Maybe String)
    , DhcpOptionsId :: !(Maybe String)
    , TagSet :: !(Maybe ResourceTagSet)
    , InstanceTenancy :: !(Maybe String)
    , IsDefault :: !(Maybe Boolean)
    } deriving (Show)

data Subnet = Subnet
    { SubnetId :: !String
    , State :: !(Maybe String)
    , VpcId :: !(Maybe String)
    , CidrBlock :: !(Maybe String)
    , AvailableIpAddressCount :: !(Maybe Int)
    , AvailabilityZone :: !(Maybe String)
    , DefaultForAz :: !(Maybe Boolean)
    , MapPublicIpOnLaunch :: !(Maybe Boolean)
    , TagSet :: !(Maybe ResourceTagSet)
    } deriving (Show)

data CustomerGatewaySet = CustomerGatewaySet
    { Item :: ![CustomerGateway]
    } deriving (Show)

data VpnGatewaySet = VpnGatewaySet
    { Item :: ![VpnGateway]
    } deriving (Show)

data VpnConnectionSet = VpnConnectionSet
    { Item :: ![VpnConnection]
    } deriving (Show)

data VpcSet = VpcSet
    { Item :: ![Vpc]
    } deriving (Show)

data SubnetSet = SubnetSet
    { Item :: ![Subnet]
    } deriving (Show)

data CustomerGatewayIdSetItem = CustomerGatewayIdSetItem
    { CustomerGatewayId :: !String
    } deriving (Show)

data CustomerGatewayIdSet = CustomerGatewayIdSet
    { Item :: ![CustomerGatewayIdSetItem]
    } deriving (Show)

data VpnGatewayIdSetItem = VpnGatewayIdSetItem
    { VpnGatewayId :: !String
    } deriving (Show)

data VpnGatewayIdSet = VpnGatewayIdSet
    { Item :: ![VpnGatewayIdSetItem]
    } deriving (Show)

data VpnConnectionIdSetItem = VpnConnectionIdSetItem
    { VpnConnectionId :: !String
    } deriving (Show)

data VpnConnectionIdSet = VpnConnectionIdSet
    { Item :: ![VpnConnectionIdSetItem]
    } deriving (Show)

data VpcIdSetItem = VpcIdSetItem
    { VpcId :: !String
    } deriving (Show)

data VpcIdSet = VpcIdSet
    { Item :: ![VpcIdSetItem]
    } deriving (Show)

data SubnetIdSetItem = SubnetIdSetItem
    { SubnetId :: !String
    } deriving (Show)

data SubnetIdSet = SubnetIdSet
    { Item :: ![SubnetIdSetItem]
    } deriving (Show)

data DhcpOptionsIdSetItem = DhcpOptionsIdSetItem
    { DhcpOptionsId :: !String
    } deriving (Show)

data DhcpOptionsIdSet = DhcpOptionsIdSet
    { Item :: ![DhcpOptionsIdSetItem]
    } deriving (Show)

data DhcpConfigurationItemSet = DhcpConfigurationItemSet
    { Item :: ![DhcpConfigurationItem]
    } deriving (Show)

data DhcpOptionsSet = DhcpOptionsSet
    { Item :: ![DhcpOptions]
    } deriving (Show)

data DhcpConfigurationItem = DhcpConfigurationItem
    { Key :: !String
    , ValueSet :: !DhcpValueSet
    } deriving (Show)

data DhcpOptions = DhcpOptions
    { DhcpOptionsId :: !String
    , DhcpConfigurationSet :: !DhcpConfigurationItemSet
    , TagSet :: !(Maybe ResourceTagSet)
    } deriving (Show)

data DhcpValue = DhcpValue
    { Value :: !String
    } deriving (Show)

data DhcpValueSet = DhcpValueSet
    { Item :: ![DhcpValue]
    } deriving (Show)

data Filter = Filter
    { Name :: !String
    , ValueSet :: !ValueSet
    } deriving (Show)

data FilterSet = FilterSet
    { Item :: ![Filter]
    } deriving (Show)

data Value = Value
    { Value :: !String
    } deriving (Show)

data ValueSet = ValueSet
    { Item :: ![Value]
    } deriving (Show)

data CreateCustomerGateway = CreateCustomerGateway
    { Type :: !String
    , IpAddress :: !String
    , BgpAsn :: !(Maybe Int)
    } deriving (Show)

data CreateCustomerGatewayResponse = CreateCustomerGatewayResponse
    { RequestId :: !String
    , CustomerGateway :: !CustomerGateway
    } deriving (Show)

data DeleteCustomerGateway = DeleteCustomerGateway
    { CustomerGatewayId :: !String
    } deriving (Show)

data DeleteCustomerGatewayResponse = DeleteCustomerGatewayResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data DescribeCustomerGateways = DescribeCustomerGateways
    { CustomerGatewaySet :: !(Maybe CustomerGatewayIdSet)
    , FilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data DescribeCustomerGatewaysResponse = DescribeCustomerGatewaysResponse
    { RequestId :: !String
    , CustomerGatewaySet :: !CustomerGatewaySet
    } deriving (Show)

data CreateVpnGateway = CreateVpnGateway
    { Type :: !String
    , AvailabilityZone :: !(Maybe String)
    } deriving (Show)

data CreateVpnGatewayResponse = CreateVpnGatewayResponse
    { RequestId :: !String
    , VpnGateway :: !VpnGateway
    } deriving (Show)

data DeleteVpnGateway = DeleteVpnGateway
    { VpnGatewayId :: !String
    } deriving (Show)

data DeleteVpnGatewayResponse = DeleteVpnGatewayResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data DescribeVpnGateways = DescribeVpnGateways
    { VpnGatewaySet :: !(Maybe VpnGatewayIdSet)
    , FilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data DescribeVpnGatewaysResponse = DescribeVpnGatewaysResponse
    { RequestId :: !String
    , VpnGatewaySet :: !VpnGatewaySet
    } deriving (Show)

data CreateVpnConnection = CreateVpnConnection
    { Type :: !String
    , CustomerGatewayId :: !String
    , VpnGatewayId :: !String
    , Options :: !(Maybe VpnConnectionOptionsRequest)
    } deriving (Show)

data VpnConnectionOptionsRequest = VpnConnectionOptionsRequest
    { StaticRoutesOnly :: !(Maybe Boolean)
    } deriving (Show)

data CreateVpnConnectionResponse = CreateVpnConnectionResponse
    { RequestId :: !String
    , VpnConnection :: !VpnConnection
    } deriving (Show)

data CreateVpnConnectionRoute = CreateVpnConnectionRoute
    { VpnConnectionId :: !String
    , DestinationCidrBlock :: !String
    } deriving (Show)

data CreateVpnConnectionRouteResponse = CreateVpnConnectionRouteResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data DeleteVpnConnectionRoute = DeleteVpnConnectionRoute
    { VpnConnectionId :: !String
    , DestinationCidrBlock :: !String
    } deriving (Show)

data DeleteVpnConnectionRouteResponse = DeleteVpnConnectionRouteResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data DeleteVpnConnection = DeleteVpnConnection
    { VpnConnectionId :: !String
    } deriving (Show)

data DeleteVpnConnectionResponse = DeleteVpnConnectionResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data DescribeVpnConnections = DescribeVpnConnections
    { VpnConnectionSet :: !(Maybe VpnConnectionIdSet)
    , FilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data DescribeVpnConnectionsResponse = DescribeVpnConnectionsResponse
    { RequestId :: !String
    , VpnConnectionSet :: !VpnConnectionSet
    } deriving (Show)

data AttachVpnGateway = AttachVpnGateway
    { VpnGatewayId :: !String
    , VpcId :: !String
    } deriving (Show)

data AttachVpnGatewayResponse = AttachVpnGatewayResponse
    { RequestId :: !String
    , Attachment :: !Attachment
    } deriving (Show)

data DetachVpnGateway = DetachVpnGateway
    { VpnGatewayId :: !String
    , VpcId :: !String
    } deriving (Show)

data DetachVpnGatewayResponse = DetachVpnGatewayResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data CreateVpc = CreateVpc
    { CidrBlock :: !String
    , InstanceTenancy :: !(Maybe String)
    } deriving (Show)

data CreateVpcResponse = CreateVpcResponse
    { RequestId :: !String
    , Vpc :: !Vpc
    } deriving (Show)

data DescribeVpcs = DescribeVpcs
    { VpcSet :: !(Maybe VpcIdSet)
    , FilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data DescribeVpcsResponse = DescribeVpcsResponse
    { RequestId :: !String
    , VpcSet :: !VpcSet
    } deriving (Show)

data DeleteVpc = DeleteVpc
    { VpcId :: !String
    } deriving (Show)

data DeleteVpcResponse = DeleteVpcResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data CreateSubnet = CreateSubnet
    { VpcId :: !String
    , CidrBlock :: !String
    , AvailabilityZone :: !(Maybe String)
    } deriving (Show)

data CreateSubnetResponse = CreateSubnetResponse
    { RequestId :: !String
    , Subnet :: !Subnet
    } deriving (Show)

data DescribeSubnets = DescribeSubnets
    { SubnetSet :: !(Maybe SubnetIdSet)
    , FilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data DescribeSubnetsResponse = DescribeSubnetsResponse
    { RequestId :: !String
    , SubnetSet :: !SubnetSet
    } deriving (Show)

data DeleteSubnet = DeleteSubnet
    { SubnetId :: !String
    } deriving (Show)

data DeleteSubnetResponse = DeleteSubnetResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data DeleteDhcpOptions = DeleteDhcpOptions
    { DhcpOptionsId :: !String
    } deriving (Show)

data DeleteDhcpOptionsResponse = DeleteDhcpOptionsResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data DescribeDhcpOptions = DescribeDhcpOptions
    { DhcpOptionsSet :: !(Maybe DhcpOptionsIdSet)
    , FilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data DescribeDhcpOptionsResponse = DescribeDhcpOptionsResponse
    { RequestId :: !String
    , DhcpOptionsSet :: !DhcpOptionsSet
    } deriving (Show)

data CreateDhcpOptions = CreateDhcpOptions
    { DhcpConfigurationSet :: !DhcpConfigurationItemSet
    } deriving (Show)

data CreateDhcpOptionsResponse = CreateDhcpOptionsResponse
    { RequestId :: !String
    , DhcpOptions :: !DhcpOptions
    } deriving (Show)

data AssociateDhcpOptions = AssociateDhcpOptions
    { DhcpOptionsId :: !String
    , VpcId :: !String
    } deriving (Show)

data AssociateDhcpOptionsResponse = AssociateDhcpOptionsResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data RequestSpotInstances = RequestSpotInstances
    { SpotPrice :: !String
    , InstanceCount :: !(Maybe Integer)
    , Type :: !(Maybe String)
    , ValidFrom :: !(Maybe DateTime)
    , ValidUntil :: !(Maybe DateTime)
    , LaunchGroup :: !(Maybe String)
    , AvailabilityZoneGroup :: !(Maybe String)
    , LaunchSpecification :: !LaunchSpecificationRequest
    } deriving (Show)

data LaunchSpecificationRequest = LaunchSpecificationRequest
    { ImageId :: !String
    , KeyName :: !(Maybe String)
    , GroupSet :: !GroupSet
    , UserData :: !(Maybe UserData)
    , Addressing :: !(Maybe String)
    , Instance :: !String
    , Placement :: !(Maybe SpotPlacementRequest)
    , KernelId :: !(Maybe String)
    , RamdiskId :: !(Maybe String)
    , BlockDeviceMapping :: !(Maybe BlockDeviceMapping)
    , Monitoring :: !(Maybe MonitoringInstance)
    , SubnetId :: !(Maybe String)
    , NetworkInterfaceSet :: !(Maybe InstanceNetworkInterfaceSetRequest)
    , IamInstanceProfile :: !(Maybe IamInstanceProfileRequest)
    , EbsOptimized :: !(Maybe Boolean)
    } deriving (Show)

data LaunchSpecificationResponse = LaunchSpecificationResponse
    { ImageId :: !String
    , KeyName :: !(Maybe String)
    , GroupSet :: !GroupSet
    , Addressing :: !(Maybe String)
    , Instance :: !String
    , Placement :: !(Maybe SpotPlacementRequest)
    , KernelId :: !(Maybe String)
    , RamdiskId :: !(Maybe String)
    , BlockDeviceMapping :: !(Maybe BlockDeviceMapping)
    , Monitoring :: !(Maybe MonitoringInstance)
    , SubnetId :: !(Maybe String)
    , NetworkInterfaceSet :: !(Maybe InstanceNetworkInterfaceSetRequest)
    , IamInstanceProfile :: !(Maybe IamInstanceProfileRequest)
    , EbsOptimized :: !(Maybe Boolean)
    } deriving (Show)

data SpotInstanceRequestSetItem = SpotInstanceRequestSetItem
    { SpotInstanceRequestId :: !String
    , SpotPrice :: !String
    , Type :: !String
    , State :: !String
    , Fault :: !(Maybe SpotInstanceStateFault)
    , Status :: !(Maybe SpotInstanceStatusMessage)
    , ValidFrom :: !(Maybe DateTime)
    , ValidUntil :: !(Maybe DateTime)
    , LaunchGroup :: !(Maybe String)
    , AvailabilityZoneGroup :: !(Maybe String)
    , LaunchSpecification :: !(Maybe LaunchSpecificationResponse)
    , InstanceId :: !(Maybe String)
    , CreateTime :: !(Maybe DateTime)
    , ProductDescription :: !(Maybe String)
    , TagSet :: !(Maybe ResourceTagSet)
    , LaunchedAvailabilityZone :: !(Maybe String)
    } deriving (Show)

data SpotInstanceStateFault = SpotInstanceStateFault
    { Code :: !String
    , Message :: !String
    } deriving (Show)

data SpotInstanceStatusMessage = SpotInstanceStatusMessage
    { Code :: !(Maybe String)
    , UpdateTime :: !(Maybe DateTime)
    , Message :: !(Maybe String)
    } deriving (Show)

data SpotInstanceRequestSet = SpotInstanceRequestSet
    { Item :: ![SpotInstanceRequestSetItem]
    } deriving (Show)

data RequestSpotInstancesResponse = RequestSpotInstancesResponse
    { RequestId :: !String
    , SpotInstanceRequestSet :: !SpotInstanceRequestSet
    } deriving (Show)

data DescribeSpotInstanceRequests = DescribeSpotInstanceRequests
    { SpotInstanceRequestIdSet :: !SpotInstanceRequestIdSet
    , FilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data SpotInstanceRequestIdSet = SpotInstanceRequestIdSet
    { Item :: ![SpotInstanceRequestIdSetItem]
    } deriving (Show)

data SpotInstanceRequestIdSetItem = SpotInstanceRequestIdSetItem
    { SpotInstanceRequestId :: !String
    } deriving (Show)

data DescribeSpotInstanceRequestsResponse = DescribeSpotInstanceRequestsResponse
    { RequestId :: !String
    , SpotInstanceRequestSet :: !SpotInstanceRequestSet
    } deriving (Show)

data CancelSpotInstanceRequests = CancelSpotInstanceRequests
    { SpotInstanceRequestIdSet :: !SpotInstanceRequestIdSet
    } deriving (Show)

data CancelSpotInstanceRequestsResponse = CancelSpotInstanceRequestsResponse
    { RequestId :: !String
    , SpotInstanceRequestSet :: !CancelSpotInstanceRequestsResponseSet
    } deriving (Show)

data CancelSpotInstanceRequestsResponseSet = CancelSpotInstanceRequestsResponseSet
    { Item :: !(NonEmpty CancelSpotInstanceRequestsResponseSetItem)
    } deriving (Show)

data CancelSpotInstanceRequestsResponseSetItem = CancelSpotInstanceRequestsResponseSetItem
    { SpotInstanceRequestId :: !String
    , State :: !String
    } deriving (Show)

data DescribeSpotPriceHistory = DescribeSpotPriceHistory
    { StartTime :: !(Maybe DateTime)
    , EndTime :: !(Maybe DateTime)
    , InstanceSet :: !(Maybe InstanceSet)
    , ProductDescriptionSet :: !(Maybe ProductDescriptionSet)
    , FilterSet :: !(Maybe FilterSet)
    , AvailabilityZone :: !(Maybe String)
    , MaxResults :: !(Maybe Integer)
    , NextToken :: !(Maybe String)
    } deriving (Show)

data InstanceSet = InstanceSet
    { Item :: !(NonEmpty InstanceSetItem)
    } deriving (Show)

data InstanceSetItem = InstanceSetItem
    { Instance :: !String
    } deriving (Show)

data ProductDescriptionSet = ProductDescriptionSet
    { Item :: !(NonEmpty ProductDescriptionSetItem)
    } deriving (Show)

data ProductDescriptionSetItem = ProductDescriptionSetItem
    { ProductDescription :: !String
    } deriving (Show)

data DescribeSpotPriceHistoryResponse = DescribeSpotPriceHistoryResponse
    { RequestId :: !String
    , SpotPriceHistorySet :: !SpotPriceHistorySet
    , NextToken :: !(Maybe String)
    } deriving (Show)

data SpotPriceHistorySet = SpotPriceHistorySet
    { Item :: ![SpotPriceHistorySetItem]
    } deriving (Show)

data SpotPriceHistorySetItem = SpotPriceHistorySetItem
    { Instance :: !String
    , ProductDescription :: !String
    , SpotPrice :: !String
    , Timestamp :: !DateTime
    , AvailabilityZone :: !(Maybe String)
    } deriving (Show)

data SpotDatafeedSubscription = SpotDatafeedSubscription
    { OwnerId :: !String
    , Bucket :: !String
    , Prefix :: !String
    , State :: !String
    , Fault :: !(Maybe SpotInstanceStateFault)
    } deriving (Show)

data CreateSpotDatafeedSubscription = CreateSpotDatafeedSubscription
    { Bucket :: !String
    , Prefix :: !String
    } deriving (Show)

data CreateSpotDatafeedSubscriptionResponse = CreateSpotDatafeedSubscriptionResponse
    { RequestId :: !String
    , SpotDatafeedSubscription :: !SpotDatafeedSubscription
    } deriving (Show)

data DescribeSpotDatafeedSubscription = DescribeSpotDatafeedSubscription

data DescribeSpotDatafeedSubscriptionResponse = DescribeSpotDatafeedSubscriptionResponse
    { RequestId :: !String
    , SpotDatafeedSubscription :: !SpotDatafeedSubscription
    } deriving (Show)

data DescribeSpotDatafeedSubscription = DescribeSpotDatafeedSubscription

data DeleteSpotDatafeedSubscriptionResponse = DeleteSpotDatafeedSubscriptionResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data DescribeLicenses = DescribeLicenses
    { LicenseIdSet :: !(Maybe LicenseIdSet)
    , FilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data LicenseIdSet = LicenseIdSet
    { Item :: ![LicenseIdSetItem]
    } deriving (Show)

data LicenseIdSetItem = LicenseIdSetItem
    { LicenseId :: !String
    } deriving (Show)

data DescribeLicensesResponse = DescribeLicensesResponse
    { RequestId :: !String
    , LicenseSet :: !LicenseSet
    } deriving (Show)

data LicenseSet = LicenseSet
    { Item :: ![LicenseSetItem]
    } deriving (Show)

data LicenseSetItem = LicenseSetItem
    { LicenseId :: !String
    , Type :: !String
    , Pool :: !String
    , CapacitySet :: !LicenseCapacitySet
    , TagSet :: !(Maybe ResourceTagSet)
    } deriving (Show)

data LicenseCapacitySet = LicenseCapacitySet
    { Item :: ![LicenseCapacitySetItem]
    } deriving (Show)

data LicenseCapacitySetItem = LicenseCapacitySetItem
    { Capacity :: !Int
    , InstanceCapacity :: !Int
    , State :: !String
    , EarliestAllowedDeactivationTime :: !(Maybe DateTime)
    } deriving (Show)

data ActivateLicense = ActivateLicense
    { LicenseId :: !String
    , Capacity :: !Int
    } deriving (Show)

data ActivateLicenseResponse = ActivateLicenseResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data DeactivateLicense = DeactivateLicense
    { LicenseId :: !String
    , Capacity :: !Int
    } deriving (Show)

data DeactivateLicenseResponse = DeactivateLicenseResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data CreatePlacementGroup = CreatePlacementGroup
    { GroupName :: !String
    , Strategy :: !String
    } deriving (Show)

data CreatePlacementGroupResponse = CreatePlacementGroupResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data DeletePlacementGroup = DeletePlacementGroup
    { GroupName :: !String
    } deriving (Show)

data DeletePlacementGroupResponse = DeletePlacementGroupResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data DescribePlacementGroupItem = DescribePlacementGroupItem
    { GroupName :: !String
    } deriving (Show)

data DescribePlacementGroupsInfo = DescribePlacementGroupsInfo
    { Item :: ![DescribePlacementGroupItem]
    } deriving (Show)

data DescribePlacementGroups = DescribePlacementGroups
    { PlacementGroupSet :: !DescribePlacementGroupsInfo
    , FilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data PlacementGroupInfo = PlacementGroupInfo
    { GroupName :: !String
    , Strategy :: !String
    , State :: !String
    } deriving (Show)

data PlacementGroupSet = PlacementGroupSet
    { Item :: ![PlacementGroupInfo]
    } deriving (Show)

data DescribePlacementGroupsResponse = DescribePlacementGroupsResponse
    { RequestId :: !String
    , PlacementGroupSet :: !PlacementGroupSet
    } deriving (Show)

data ResourceIdSet = ResourceIdSet
    { Item :: ![ResourceIdSetItem]
    } deriving (Show)

data ResourceIdSetItem = ResourceIdSetItem
    { ResourceId :: !String
    } deriving (Show)

data ResourceTagSetItem = ResourceTagSetItem
    { Key :: !String
    , Value :: !String
    } deriving (Show)

data ResourceTagSet = ResourceTagSet
    { Item :: ![ResourceTagSetItem]
    } deriving (Show)

data CreateTags = CreateTags
    { ResourcesSet :: !ResourceIdSet
    , TagSet :: !ResourceTagSet
    } deriving (Show)

data CreateTagsResponse = CreateTagsResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data TagSetItem = TagSetItem
    { ResourceId :: !(Maybe String)
    , Resource :: !(Maybe String)
    , Key :: !(Maybe String)
    , Value :: !(Maybe String)
    } deriving (Show)

data TagSet = TagSet
    { Item :: ![TagSetItem]
    } deriving (Show)

data DescribeTags = DescribeTags
    { FilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data DescribeTagsResponse = DescribeTagsResponse
    { RequestId :: !String
    , TagSet :: !TagSet
    } deriving (Show)

data DeleteTagsSetItem = DeleteTagsSetItem
    { Key :: !(Maybe String)
    , Value :: !(Maybe String)
    } deriving (Show)

data DeleteTagsSet = DeleteTagsSet
    { Item :: ![DeleteTagsSetItem]
    } deriving (Show)

data DeleteTags = DeleteTags
    { ResourcesSet :: !ResourceIdSet
    , TagSet :: !DeleteTagsSet
    } deriving (Show)

data DeleteTagsResponse = DeleteTagsResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data ImportInstance = ImportInstance
    { Description :: !(Maybe String)
    , LaunchSpecification :: !ImportInstanceLaunchSpecification
    , DiskImageSet :: !DiskImageSet
    , KeepPartialImports :: !(Maybe Boolean)
    , Platform :: !String
    } deriving (Show)

data ImportInstanceResponse = ImportInstanceResponse
    { RequestId :: !String
    , ConversionTask :: !ConversionTask
    } deriving (Show)

data ImportInstanceLaunchSpecification = ImportInstanceLaunchSpecification
    { Architecture :: !String
    , GroupSet :: !(Maybe ImportInstanceGroupSet)
    , UserData :: !(Maybe UserData)
    , Instance :: !String
    , Placement :: !(Maybe InstancePlacement)
    , Monitoring :: !(Maybe MonitoringInstance)
    , SubnetId :: !(Maybe String)
    , InstanceInitiatedShutdownBehavior :: !(Maybe String)
    , PrivateIpAddress :: !(Maybe String)
    } deriving (Show)

data DiskImageSet = DiskImageSet
    { Item :: ![DiskImage]
    } deriving (Show)

data DiskImage = DiskImage
    { Image :: !DiskImageDetail
    , Description :: !(Maybe String)
    , Volume :: !DiskImageVolume
    } deriving (Show)

data DiskImageDetail = DiskImageDetail
    { Format :: !String
    , Bytes :: !Long
    , ImportManifestUrl :: !String
    } deriving (Show)

data DiskImageVolume = DiskImageVolume
    { Size :: !Integer
    } deriving (Show)

data ConversionTask = ConversionTask
    { ConversionTaskId :: !String
    , ExpirationTime :: !(Maybe String)
  -- <xs:choice>
    , ImportVolume :: !(Maybe ImportVolumeTaskDetails)
    , ImportInstance :: !(Maybe ImportInstanceTaskDetails)
  -- </xs:choice>
    , State :: !String
    , StatusMessage :: !(Maybe String)
    , TagSet :: !(Maybe ResourceTagSet)
    } deriving (Show)

data ImportInstanceTaskDetails = ImportInstanceTaskDetails
    { Volumes :: !ImportInstanceVolumeDetailSet
    , InstanceId :: !(Maybe String)
    , Platform :: !(Maybe String)
    , Description :: !(Maybe String)
    } deriving (Show)

data ImportVolumeTaskDetails = ImportVolumeTaskDetails
    { BytesConverted :: !Long
    , AvailabilityZone :: !String
    , Description :: !(Maybe String)
    , Image :: !DiskImageDescription
    , Volume :: !DiskImageVolumeDescription
    } deriving (Show)

data ImportInstanceVolumeDetailSet = ImportInstanceVolumeDetailSet
    { Item :: ![ImportInstanceVolumeDetailItem]
    } deriving (Show)

data ImportInstanceVolumeDetailItem = ImportInstanceVolumeDetailItem
    { BytesConverted :: !Long
    , AvailabilityZone :: !String
    , Image :: !DiskImageDescription
    , Description :: !(Maybe String)
    , Volume :: !DiskImageVolumeDescription
    , Status :: !String
    , StatusMessage :: !(Maybe String)
    } deriving (Show)

data DiskImageVolumeDescription = DiskImageVolumeDescription
    { Size :: !Integer
    , Id :: !String
    } deriving (Show)

data DiskImageDescription = DiskImageDescription
    { Format :: !String
    , Size :: !Long
    , ImportManifestUrl :: !String
    , Checksum :: !(Maybe String)
    } deriving (Show)

data ImportVolume = ImportVolume
    { AvailabilityZone :: !String
    , Image :: !DiskImageDetail
    , Description :: !(Maybe String)
    , Volume :: !DiskImageVolume
    } deriving (Show)

data ImportVolumeResponse = ImportVolumeResponse
    { RequestId :: !String
    , ConversionTask :: !ConversionTask
    } deriving (Show)

data DescribeConversionTasks = DescribeConversionTasks
    { ConversionTaskIdSet :: !ConversionTaskIdSet
    } deriving (Show)

data DescribeConversionTasksResponse = DescribeConversionTasksResponse
    { RequestId :: !String
    , ConversionTasks :: !ConversionTaskSet
    } deriving (Show)

data ConversionTaskIdSet = ConversionTaskIdSet
    { Item :: ![ConversionTaskIdItem]
    } deriving (Show)

data ConversionTaskIdItem = ConversionTaskIdItem
    { ConversionTaskId :: !String
    } deriving (Show)

data ConversionTaskSet = ConversionTaskSet
    { Item :: ![ConversionTask]
    } deriving (Show)

data CancelConversionTask = CancelConversionTask
    { ConversionTaskId :: !String
    } deriving (Show)

data CancelConversionTaskResponse = CancelConversionTaskResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data CreateInstanceExportTask = CreateInstanceExportTask
    { Description :: !(Maybe String)
    , InstanceId :: !String
    , TargetEnvironment :: !String
  -- <xs:choice>
    , ExportToS3 :: !(Maybe ExportToS3Task)
  -- </xs:choice>
    } deriving (Show)

data ExportToS3Task = ExportToS3Task
    { DiskImageFormat :: !(Maybe String)
    , ContainerFormat :: !(Maybe String)
    , S3Bucket :: !String
    , S3Prefix :: !String
    } deriving (Show)

data CreateInstanceExportTaskResponse = CreateInstanceExportTaskResponse
    { RequestId :: !String
    , ExportTask :: !ExportTaskResponse
    } deriving (Show)

data DescribeExportTasks = DescribeExportTasks
    { ExportTaskIdSet :: !ExportTaskIdSet
    } deriving (Show)

data ExportTaskIdSet = ExportTaskIdSet
    { Item :: ![ExportTaskId]
    } deriving (Show)

data ExportTaskId = ExportTaskId
    { ExportTaskId :: !String
    } deriving (Show)

data DescribeExportTasksResponse = DescribeExportTasksResponse
    { RequestId :: !String
    , ExportTaskSet :: !ExportTaskSetResponse
    } deriving (Show)

data ExportTaskSetResponse = ExportTaskSetResponse
    { Item :: ![ExportTaskResponse]
    } deriving (Show)

data ExportTaskResponse = ExportTaskResponse
    { ExportTaskId :: !String
    , Description :: !(Maybe String)
    , State :: !String
    , StatusMessage :: !(Maybe String)
  -- <xs:choice>
    , InstanceExport :: !(Maybe InstanceExportTaskResponse)
  -- </xs:choice>
  -- <xs:choice>
    , ExportToS3 :: !(Maybe ExportToS3TaskResponse)
  -- </xs:choice>
    } deriving (Show)

data InstanceExportTaskResponse = InstanceExportTaskResponse
    { InstanceId :: !String
    , TargetEnvironment :: !(Maybe String)
    } deriving (Show)

data ExportToS3TaskResponse = ExportToS3TaskResponse
    { DiskImageFormat :: !String
    , ContainerFormat :: !(Maybe String)
    , S3Bucket :: !String
    , S3Key :: !String
    } deriving (Show)

data CancelExportTask = CancelExportTask
    { ExportTaskId :: !String
    } deriving (Show)

data CancelExportTaskResponse = CancelExportTaskResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data CreateInternetGateway = CreateInternetGateway

data InternetGatewayAttachmentSet = InternetGatewayAttachmentSet
    { Item :: ![InternetGatewayAttachment]
    } deriving (Show)

data InternetGatewayAttachment = InternetGatewayAttachment
    { VpcId :: !String
    , State :: !String
    } deriving (Show)

data InternetGateway = InternetGateway
    { InternetGatewayId :: !String
    , AttachmentSet :: !InternetGatewayAttachmentSet
    , TagSet :: !(Maybe ResourceTagSet)
    } deriving (Show)

data CreateInternetGatewayResponse = CreateInternetGatewayResponse
    { RequestId :: !String
    , InternetGateway :: !InternetGateway
    } deriving (Show)

data InternetGatewayIdSet = InternetGatewayIdSet
    { Item :: ![InternetGatewayIdSetItem]
    } deriving (Show)

data InternetGatewayIdSetItem = InternetGatewayIdSetItem
    { InternetGatewayId :: !String
    } deriving (Show)

data DescribeInternetGateways = DescribeInternetGateways
    { InternetGatewayIdSet :: !InternetGatewayIdSet
    , FilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data InternetGatewaySet = InternetGatewaySet
    { Item :: ![InternetGateway]
    } deriving (Show)

data DescribeInternetGatewaysResponse = DescribeInternetGatewaysResponse
    { RequestId :: !String
    , InternetGatewaySet :: !InternetGatewaySet
    } deriving (Show)

data DeleteInternetGateway = DeleteInternetGateway
    { InternetGatewayId :: !String
    } deriving (Show)

data DeleteInternetGatewayResponse = DeleteInternetGatewayResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data AttachInternetGateway = AttachInternetGateway
    { InternetGatewayId :: !String
    , VpcId :: !String
    } deriving (Show)

data AttachInternetGatewayResponse = AttachInternetGatewayResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data DetachInternetGateway = DetachInternetGateway
    { InternetGatewayId :: !String
    , VpcId :: !String
    } deriving (Show)

data DetachInternetGatewayResponse = DetachInternetGatewayResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data CreateRouteTable = CreateRouteTable
    { VpcId :: !String
    } deriving (Show)

data RouteSet = RouteSet
    { Item :: ![Route]
    } deriving (Show)

data Route = Route
    { DestinationCidrBlock :: !String
    , GatewayId :: !(Maybe String)
    , InstanceId :: !(Maybe String)
    , InstanceOwnerId :: !(Maybe String)
    , NetworkInterfaceId :: !(Maybe String)
    , State :: !String
    , Origin :: !String
    } deriving (Show)

data RouteTableAssociationSet = RouteTableAssociationSet
    { Item :: ![RouteTableAssociation]
    } deriving (Show)

data RouteTableAssociation = RouteTableAssociation
    { RouteTableAssociationId :: !String
    , RouteTableId :: !String
  -- <xs:choice>
    , SubnetId :: !(Maybe String)
    , Main :: !(Maybe Boolean)
  -- </xs:choice>
    } deriving (Show)

data PropagatingVgwSet = PropagatingVgwSet
    { Item :: ![PropagatingVgw]
    } deriving (Show)

data PropagatingVgw = PropagatingVgw
    { GatewayId :: !String
    } deriving (Show)

data RouteTable = RouteTable
    { RouteTableId :: !String
    , VpcId :: !String
    , RouteSet :: !RouteSet
    , AssociationSet :: !RouteTableAssociationSet
    , PropagatingVgwSet :: !PropagatingVgwSet
    , TagSet :: !(Maybe ResourceTagSet)
    } deriving (Show)

data CreateRouteTableResponse = CreateRouteTableResponse
    { RequestId :: !String
    , RouteTable :: !RouteTable
    } deriving (Show)

data RouteTableIdSet = RouteTableIdSet
    { Item :: ![RouteTableIdSetItem]
    } deriving (Show)

data RouteTableIdSetItem = RouteTableIdSetItem
    { RouteTableId :: !String
    } deriving (Show)

data DescribeRouteTables = DescribeRouteTables
    { RouteTableIdSet :: !RouteTableIdSet
    , FilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data RouteTableSet = RouteTableSet
    { Item :: ![RouteTable]
    } deriving (Show)

data DescribeRouteTablesResponse = DescribeRouteTablesResponse
    { RequestId :: !String
    , RouteTableSet :: !RouteTableSet
    } deriving (Show)

data EnableVgwRoutePropagationRequest = EnableVgwRoutePropagationRequest
    { RouteTableId :: !String
    , GatewayId :: !String
    } deriving (Show)

data EnableVgwRoutePropagationResponse = EnableVgwRoutePropagationResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data DisableVgwRoutePropagationRequest = DisableVgwRoutePropagationRequest
    { RouteTableId :: !String
    , GatewayId :: !String
    } deriving (Show)

data DisableVgwRoutePropagationResponse = DisableVgwRoutePropagationResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data DeleteRouteTable = DeleteRouteTable
    { RouteTableId :: !String
    } deriving (Show)

data DeleteRouteTableResponse = DeleteRouteTableResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data AssociateRouteTable = AssociateRouteTable
    { RouteTableId :: !String
    , SubnetId :: !String
    } deriving (Show)

data AssociateRouteTableResponse = AssociateRouteTableResponse
    { RequestId :: !String
    , AssociationId :: !String
    } deriving (Show)

data ReplaceRouteTableAssociation = ReplaceRouteTableAssociation
    { AssociationId :: !String
    , RouteTableId :: !String
    } deriving (Show)

data ReplaceRouteTableAssociationResponse = ReplaceRouteTableAssociationResponse
    { RequestId :: !String
    , NewAssociationId :: !String
    } deriving (Show)

data DisassociateRouteTable = DisassociateRouteTable
    { AssociationId :: !String
    } deriving (Show)

data DisassociateRouteTableResponse = DisassociateRouteTableResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data CreateRoute = CreateRoute
    { RouteTableId :: !String
    , DestinationCidrBlock :: !String
  -- <xs:choice>
    , GatewayId :: !(Maybe String)
    , InstanceId :: !(Maybe String)
    , NetworkInterfaceId :: !(Maybe String)
  -- </xs:choice>
    } deriving (Show)

data CreateRouteResponse = CreateRouteResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data ReplaceRoute = ReplaceRoute
    { RouteTableId :: !String
    , DestinationCidrBlock :: !String
  -- <xs:choice>
    , GatewayId :: !(Maybe String)
    , InstanceId :: !(Maybe String)
    , NetworkInterfaceId :: !(Maybe String)
  -- </xs:choice>
    } deriving (Show)

data ReplaceRouteResponse = ReplaceRouteResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data DeleteRoute = DeleteRoute
    { RouteTableId :: !String
    , DestinationCidrBlock :: !String
    } deriving (Show)

data DeleteRouteResponse = DeleteRouteResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data CreateNetworkAcl = CreateNetworkAcl
    { VpcId :: !String
    } deriving (Show)

data NetworkAclEntrySet = NetworkAclEntrySet
    { Item :: ![NetworkAclEntry]
    } deriving (Show)

data IcmpCode = IcmpCode
    { Code :: !Int
    , Type :: !Int
    } deriving (Show)

data PortRange = PortRange
    { From :: !Int
    , To :: !Int
    } deriving (Show)

data NetworkAclEntry = NetworkAclEntry
    { RuleNumber :: !Int
    , Protocol :: !String
    , RuleAction :: !String
    , Egress :: !Boolean
    , CidrBlock :: !String
    , IcmpCode :: !(Maybe IcmpCode)
    , PortRange :: !(Maybe PortRange)
    } deriving (Show)

data NetworkAclAssociationSet = NetworkAclAssociationSet
    { Item :: ![NetworkAclAssociation]
    } deriving (Show)

data NetworkAclAssociation = NetworkAclAssociation
    { NetworkAclAssociationId :: !String
    , NetworkAclId :: !String
    , SubnetId :: !String
    } deriving (Show)

data NetworkAcl = NetworkAcl
    { NetworkAclId :: !String
    , VpcId :: !String
    , Default :: !Boolean
    , EntrySet :: !NetworkAclEntrySet
    , AssociationSet :: !NetworkAclAssociationSet
    , TagSet :: !(Maybe ResourceTagSet)
    } deriving (Show)

data CreateNetworkAclResponse = CreateNetworkAclResponse
    { RequestId :: !String
    , NetworkAcl :: !NetworkAcl
    } deriving (Show)

data NetworkAclIdSet = NetworkAclIdSet
    { Item :: ![NetworkAclIdSetItem]
    } deriving (Show)

data NetworkAclIdSetItem = NetworkAclIdSetItem
    { NetworkAclId :: !String
    } deriving (Show)

data DescribeNetworkAcls = DescribeNetworkAcls
    { NetworkAclIdSet :: !NetworkAclIdSet
    , FilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data NetworkAclSet = NetworkAclSet
    { Item :: ![NetworkAcl]
    } deriving (Show)

data DescribeNetworkAclsResponse = DescribeNetworkAclsResponse
    { RequestId :: !String
    , NetworkAclSet :: !NetworkAclSet
    } deriving (Show)

data DeleteNetworkAcl = DeleteNetworkAcl
    { NetworkAclId :: !String
    } deriving (Show)

data DeleteNetworkAclResponse = DeleteNetworkAclResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data ReplaceNetworkAclAssociation = ReplaceNetworkAclAssociation
    { AssociationId :: !String
    , NetworkAclId :: !String
    } deriving (Show)

data ReplaceNetworkAclAssociationResponse = ReplaceNetworkAclAssociationResponse
    { RequestId :: !String
    , NewAssociationId :: !String
    } deriving (Show)

data CreateNetworkAclEntry = CreateNetworkAclEntry
    { NetworkAclId :: !String
    , RuleNumber :: !Int
    , Protocol :: !String
    , RuleAction :: !String
    , Egress :: !Boolean
    , CidrBlock :: !String
    , IcmpCode :: !(Maybe IcmpCode)
    , PortRange :: !(Maybe PortRange)
    } deriving (Show)

data CreateNetworkAclEntryResponse = CreateNetworkAclEntryResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data ReplaceNetworkAclEntry = ReplaceNetworkAclEntry
    { NetworkAclId :: !String
    , RuleNumber :: !Int
    , Protocol :: !String
    , RuleAction :: !String
    , Egress :: !Boolean
    , CidrBlock :: !String
    , IcmpCode :: !(Maybe IcmpCode)
    , PortRange :: !(Maybe PortRange)
    } deriving (Show)

data ReplaceNetworkAclEntryResponse = ReplaceNetworkAclEntryResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data DeleteNetworkAclEntry = DeleteNetworkAclEntry
    { NetworkAclId :: !String
    , RuleNumber :: !Int
    , Egress :: !Boolean
    } deriving (Show)

data DeleteNetworkAclEntryResponse = DeleteNetworkAclEntryResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data DescribeInstanceStatus = DescribeInstanceStatus
    { InstancesSet :: !InstanceIdSet
    , FilterSet :: !(Maybe FilterSet)
    , NextToken :: !(Maybe String)
    , MaxResults :: !(Maybe Int)
    , IncludeAllInstances :: !(Maybe Boolean)
    } deriving (Show)

data DescribeInstanceStatusResponse = DescribeInstanceStatusResponse
    { RequestId :: !String
    , InstanceStatusSet :: !InstanceStatusSet
    , NextToken :: !(Maybe String)
    } deriving (Show)

data InstanceStatusSet = InstanceStatusSet
    { Item :: ![InstanceStatusItem]
    } deriving (Show)

data InstanceStatus = InstanceStatus
    { Status :: !String
    , Details :: !(Maybe InstanceStatusDetailsSet)
    } deriving (Show)

data InstanceStatusDetailsSet = InstanceStatusDetailsSet
    { Item :: ![InstanceStatusDetailsSetItem]
    } deriving (Show)

data InstanceStatusDetailsSetItem = InstanceStatusDetailsSetItem
    { Name :: !String
    , Status :: !String
    , ImpairedSince :: !(Maybe DateTime)
    } deriving (Show)

data InstanceStatusEvent = InstanceStatusEvent
    { Code :: !String
    , Description :: !String
    , NotBefore :: !DateTime
    , NotAfter :: !(Maybe DateTime)
    } deriving (Show)

data InstanceStatusEventsSet = InstanceStatusEventsSet
    { Item :: ![InstanceStatusEvent]
    } deriving (Show)

data InstanceStatusItem = InstanceStatusItem
    { InstanceId :: !String
    , AvailabilityZone :: !String
    , EventsSet :: !(Maybe InstanceStatusEventsSet)
    , InstanceState :: !InstanceState
    , SystemStatus :: !InstanceStatus
    , InstanceStatus :: !InstanceStatus
    } deriving (Show)

data ReportInstanceStatus = ReportInstanceStatus
    { InstancesSet :: !InstanceIdSet
    , Status :: !String
    , StartTime :: !(Maybe DateTime)
    , EndTime :: !(Maybe DateTime)
    , ReasonCodesSet :: !ReportInstanceStatusReasonCodesSet
    , Description :: !(Maybe String)
    } deriving (Show)

data ReportInstanceStatusReasonCodesSet = ReportInstanceStatusReasonCodesSet
    { Item :: !(NonEmpty ReportInstanceStatusReasonCodeSetItem)
    } deriving (Show)

data ReportInstanceStatusReasonCodeSetItem = ReportInstanceStatusReasonCodeSetItem
    { ReasonCode :: !String
    } deriving (Show)

data ReportInstanceStatusResponse = ReportInstanceStatusResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data CreateNetworkInterface = CreateNetworkInterface
    { SubnetId :: !String
    , Description :: !(Maybe String)
    , PrivateIpAddress :: !(Maybe String)
    , GroupSet :: !(Maybe SecurityGroupIdSet)
    , PrivateIpAddressesSet :: !(Maybe PrivateIpAddressesSetRequest)
    , SecondaryPrivateIpAddressCount :: !(Maybe Int)
    } deriving (Show)

data CreateNetworkInterfaceResponse = CreateNetworkInterfaceResponse
    { RequestId :: !String
    , NetworkInterface :: !NetworkInterface
    } deriving (Show)

data NetworkInterfaceIdSet = NetworkInterfaceIdSet
    { Item :: ![NetworkInterfaceIdSetItem]
    } deriving (Show)

data NetworkInterfaceIdSetItem = NetworkInterfaceIdSetItem
    { NetworkInterfaceId :: !String
    } deriving (Show)

data DescribeNetworkInterfaces = DescribeNetworkInterfaces
    { NetworkInterfaceIdSet :: !(Maybe NetworkInterfaceIdSet)
    , FilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data NetworkInterface = NetworkInterface
    { NetworkInterfaceId :: !String
    , SubnetId :: !(Maybe String)
    , VpcId :: !(Maybe String)
    , AvailabilityZone :: !(Maybe String)
    , Description :: !(Maybe String)
    , OwnerId :: !String
    , RequesterId :: !(Maybe String)
    , RequesterManaged :: !(Maybe Boolean)
    , Status :: !String
    , MacAddress :: !String
    , PrivateIpAddress :: !String
    , PrivateDnsName :: !(Maybe String)
    , SourceDestCheck :: !Boolean
    , GroupSet :: !GroupSet
    , Attachment :: !(Maybe NetworkInterfaceAttachment)
    , Association :: !(Maybe NetworkInterfaceAssociation)
    , TagSet :: !(Maybe ResourceTagSet)
    , PrivateIpAddressesSet :: !(Maybe NetworkInterfacePrivateIpAddressesSet)
    } deriving (Show)

data NetworkInterfacePrivateIpAddressesSet = NetworkInterfacePrivateIpAddressesSet
    { Item :: ![NetworkInterfacePrivateIpAddressesSetItem]
    } deriving (Show)

data NetworkInterfacePrivateIpAddressesSetItem = NetworkInterfacePrivateIpAddressesSetItem
    { PrivateIpAddress :: !String
    , PrivateDnsName :: !(Maybe String)
    , Primary :: !Boolean
    , Association :: !(Maybe NetworkInterfaceAssociation)
    } deriving (Show)

data NetworkInterfaceAttachment = NetworkInterfaceAttachment
    { AttachmentId :: !String
    , InstanceId :: !(Maybe String)
    , InstanceOwnerId :: !(Maybe String)
    , DeviceIndex :: !Int
    , Status :: !String
    , AttachTime :: !DateTime
    , DeleteOnTermination :: !Boolean
    } deriving (Show)

data NetworkInterfaceAssociation = NetworkInterfaceAssociation
    { PublicIp :: !String
    , PublicDnsName :: !(Maybe String)
    , IpOwnerId :: !(Maybe String)
    , AllocationId :: !(Maybe String)
    , AssociationId :: !(Maybe String)
    } deriving (Show)

data NetworkInterfaceSet = NetworkInterfaceSet
    { Item :: ![NetworkInterface]
    } deriving (Show)

data DescribeNetworkInterfacesResponse = DescribeNetworkInterfacesResponse
    { RequestId :: !String
    , NetworkInterfaceSet :: !NetworkInterfaceSet
    } deriving (Show)

data DeleteNetworkInterface = DeleteNetworkInterface
    { NetworkInterfaceId :: !String
    } deriving (Show)

data DeleteNetworkInterfaceResponse = DeleteNetworkInterfaceResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data AttachNetworkInterface = AttachNetworkInterface
    { NetworkInterfaceId :: !String
    , InstanceId :: !String
    , DeviceIndex :: !Int
    } deriving (Show)

data AttachNetworkInterfaceResponse = AttachNetworkInterfaceResponse
    { RequestId :: !String
    , AttachmentId :: !String
    } deriving (Show)

data DetachNetworkInterface = DetachNetworkInterface
    { AttachmentId :: !String
    , Force :: !(Maybe Boolean)
    } deriving (Show)

data DetachNetworkInterfaceResponse = DetachNetworkInterfaceResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data DescribeNetworkInterfaceAttribute = DescribeNetworkInterfaceAttribute
    { NetworkInterfaceId :: !String
    , DescribeNetworkInterfaceAttributesGroup :: !DescribeNetworkInterfaceAttributesGroup
    } deriving (Show)

data DescribeNetworkInterfaceAttributesGroup = DescribeNetworkInterfaceAttributesGroup
    { Description :: !EmptyElement
    , SourceDestCheck :: !EmptyElement
    , GroupSet :: !EmptyElement
    , Attachment :: !EmptyElement
    } deriving (Show)
data DescribeNetworkInterfaceAttributeResponse = DescribeNetworkInterfaceAttributeResponse
    { RequestId :: !String
    , NetworkInterfaceId :: !String
  -- <xs:choice>
    , Description :: !(Maybe NullableAttributeValue)
    , SourceDestCheck :: !(Maybe AttributeBooleanValue)
    , GroupSet :: !(Maybe GroupSet)
    , Attachment :: !(Maybe NetworkInterfaceAttachment)
  -- </xs:choice>
    } deriving (Show)

data ModifyNetworkInterfaceAttribute = ModifyNetworkInterfaceAttribute
    { NetworkInterfaceId :: !String
  -- <xs:choice>
    , Description :: !(Maybe NullableAttributeValue)
    , SourceDestCheck :: !(Maybe AttributeBooleanValue)
    , GroupSet :: !(Maybe SecurityGroupIdSet)
    , Attachment :: !(Maybe ModifyNetworkInterfaceAttachment)
  -- </xs:choice>
    } deriving (Show)

data ModifyNetworkInterfaceAttachment = ModifyNetworkInterfaceAttachment
    { AttachmentId :: !String
    , DeleteOnTermination :: !Boolean
    } deriving (Show)

data ModifyNetworkInterfaceAttributeResponse = ModifyNetworkInterfaceAttributeResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data ResetNetworkInterfaceAttribute = ResetNetworkInterfaceAttribute
    { NetworkInterfaceId :: !String
    , ResetNetworkInterfaceAttributesGroup :: !ResetNetworkInterfaceAttributesGroup
    } deriving (Show)

data ResetNetworkInterfaceAttributesGroup = ResetNetworkInterfaceAttributesGroup
    { SourceDestCheck :: !EmptyElement
    } deriving (Show)
data ResetNetworkInterfaceAttributeResponse = ResetNetworkInterfaceAttributeResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data AssignPrivateIpAddresses = AssignPrivateIpAddresses
    { NetworkInterfaceId :: !String
    , PrivateIpAddressesSet :: !(Maybe AssignPrivateIpAddressesSetRequest)
    , SecondaryPrivateIpAddressCount :: !(Maybe Int)
    , AllowReassignment :: !(Maybe Boolean)
    } deriving (Show)

data AssignPrivateIpAddressesResponse = AssignPrivateIpAddressesResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data UnassignPrivateIpAddresses = UnassignPrivateIpAddresses
    { NetworkInterfaceId :: !String
    , PrivateIpAddressesSet :: !AssignPrivateIpAddressesSetRequest
    } deriving (Show)

data UnassignPrivateIpAddressesResponse = UnassignPrivateIpAddressesResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data AssignPrivateIpAddressesSetRequest = AssignPrivateIpAddressesSetRequest
    { Item :: !(NonEmpty AssignPrivateIpAddressesSetItemRequest)
    } deriving (Show)

data AssignPrivateIpAddressesSetItemRequest = AssignPrivateIpAddressesSetItemRequest
    { PrivateIpAddress :: !String
    } deriving (Show)

data DescribeVolumeStatus = DescribeVolumeStatus
    { VolumeSet :: !DescribeVolumesSet
    , FilterSet :: !(Maybe FilterSet)
    , MaxResults :: !(Maybe Integer)
    , NextToken :: !(Maybe String)
    } deriving (Show)

data DescribeVolumeStatusResponse = DescribeVolumeStatusResponse
    { RequestId :: !String
    , VolumeStatusSet :: !VolumeStatusSet
    , NextToken :: !(Maybe String)
    } deriving (Show)

data VolumeStatusSet = VolumeStatusSet
    { Item :: ![VolumeStatusItem]
    } deriving (Show)

data VolumeStatusItem = VolumeStatusItem
    { VolumeId :: !String
    , AvailabilityZone :: !String
    , VolumeStatus :: !VolumeStatusInfo
    , EventsSet :: !VolumeStatusEventsSet
    , ActionsSet :: !VolumeStatusActionsSet
    } deriving (Show)

data VolumeStatusInfo = VolumeStatusInfo
    { Status :: !String
    , Details :: !VolumeStatusDetailsSet
    } deriving (Show)

data VolumeStatusDetailsSet = VolumeStatusDetailsSet
    { Item :: ![VolumeStatusDetailsItem]
    } deriving (Show)

data VolumeStatusDetailsItem = VolumeStatusDetailsItem
    { Name :: !String
    , Status :: !String
    } deriving (Show)

data VolumeStatusEventsSet = VolumeStatusEventsSet
    { Item :: ![VolumeStatusEventItem]
    } deriving (Show)

data VolumeStatusEventItem = VolumeStatusEventItem
    { Description :: !String
    , NotBefore :: !DateTime
    , NotAfter :: !DateTime
    , EventId :: !String
    , Event :: !String
    } deriving (Show)

data VolumeStatusActionsSet = VolumeStatusActionsSet
    { Item :: ![VolumeStatusActionItem]
    } deriving (Show)

data VolumeStatusActionItem = VolumeStatusActionItem
    { Description :: !String
    , Code :: !String
    , EventId :: !String
    , Event :: !String
    } deriving (Show)

data EnableVolumeIO = EnableVolumeIO
    { VolumeId :: !String
    } deriving (Show)

data EnableVolumeIOResponse = EnableVolumeIOResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data ModifyVolumeAttribute = ModifyVolumeAttribute
    { VolumeId :: !String
  -- <xs:choice>
    , AutoEnableIO :: !AttributeBooleanValue
  -- </xs:choice>
    } deriving (Show)

data ModifyVolumeAttributeResponse = ModifyVolumeAttributeResponse
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

data DescribeVolumeAttribute = DescribeVolumeAttribute
    { VolumeId :: !String
    , DescribeVolumeAttributesGroup :: !DescribeVolumeAttributesGroup
    } deriving (Show)

data DescribeVolumeAttributesGroup = DescribeVolumeAttributesGroup
    { AutoEnableIO :: !EmptyElement
    , ProductCodes :: !EmptyElement
    } deriving (Show)

data DescribeVolumeAttributeResponse = DescribeVolumeAttributeResponse
    { RequestId :: !String
    , VolumeId :: !String
  -- <xs:choice>
    , AutoEnableIO :: !(Maybe NullableAttributeBooleanValue)
    , ProductCodes :: !(Maybe ProductCodesSet)
  -- </xs:choice>
    } deriving (Show)
