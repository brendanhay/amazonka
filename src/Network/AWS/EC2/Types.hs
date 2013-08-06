{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.AWS.EC2.Types
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.EC2.Types where

import Data.ByteString      (ByteString)
import Data.Monoid
import Network.AWS.Internal

type CreateImage = CreateImageType

data CreateImageType = CreateImageType
    { InstanceId         :: !String
    , Name               :: !String
    , Description        :: !(Maybe String)
    , NoReboot           :: !(Maybe Boolean)
    , BlockDeviceMapping :: !(Maybe BlockDeviceMappingType)
    } deriving (Show)

type CreateImageResponse = CreateImageResponseType

data CreateImageResponseType = CreateImageResponseType
    { RequestId :: !String
    , ImageId   :: !String
    } deriving (Show)

type RegisterImage = RegisterImageType

type RegisterImageResponse = RegisterImageResponseType

data RegisterImageType = RegisterImageType
    { ImageLocation      :: !(Maybe String)
    , Name               :: !String
    , Description        :: !(Maybe String)
    , Architecture       :: !(Maybe String)
    , KernelId           :: !(Maybe String)
    , RamdiskId          :: !(Maybe String)
    , RootDeviceName     :: !(Maybe String)
    , BlockDeviceMapping :: !(Maybe BlockDeviceMappingType)
    } deriving (Show)

data RegisterImageResponseType = RegisterImageResponseType
    { RequestId :: !String
    , ImageId   :: !String
    } deriving (Show)

type DeregisterImage = DeregisterImageType

data DeregisterImageType = DeregisterImageType
    { ImageId :: !String
    } deriving (Show)

type DeregisterImageResponse = DeregisterImageResponseType

data DeregisterImageResponseType = DeregisterImageResponseType
    { RequestId :: !String
    , Return :: !Boolean
    } deriving (Show)

type CreateKeyPair = CreateKeyPairType

data CreateKeyPairType = CreateKeyPairType
    { KeyName :: !String
    } deriving (Show)

type CreateKeyPairResponse = CreateKeyPairResponseType

data CreateKeyPairResponseType = CreateKeyPairResponseType
    { RequestId      :: !String
    , KeyName        :: !String
    , KeyFingerprint :: !String
    , KeyMaterial    :: !String
    } deriving (Show)

type ImportKeyPair = ImportKeyPairType

type ImportKeyPairResponse = ImportKeyPairResponseType

data ImportKeyPairType = ImportKeyPairType
    { KeyName           :: !String
    , PublicKeyMaterial :: !String
    } deriving (Show)

data ImportKeyPairResponseType = ImportKeyPairResponseType
    { RequestId      :: !String
    , KeyName        :: !String
    , KeyFingerprint :: !String
    } deriving (Show)

type DeleteKeyPair = DeleteKeyPairType

data DeleteKeyPairType = DeleteKeyPairType
    { KeyName :: !String
    } deriving (Show)

type DeleteKeyPairResponse = DeleteKeyPairResponseType

data DeleteKeyPairResponseType = DeleteKeyPairResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

type DescribeKeyPairs = DescribeKeyPairsType

data DescribeKeyPairsType = DescribeKeyPairsType
    { KeySet    :: !DescribeKeyPairsInfoType
    , FilterSet :: !(Maybe FilterSetType)
    } deriving (Show)

data DescribeKeyPairsInfoType = DescribeKeyPairsInfoType
    { Item :: ![DescribeKeyPairsItemType]
    } deriving (Show)

data DescribeKeyPairsItemType = DescribeKeyPairsItemType
    { KeyName :: !String
    } deriving (Show)

type DescribeKeyPairsResponse = DescribeKeyPairsResponseType

data DescribeKeyPairsResponseType = DescribeKeyPairsResponseType
    { RequestId :: !String
    , KeySet    :: !DescribeKeyPairsResponseInfoType
    } deriving (Show)

data DescribeKeyPairsResponseInfoType = DescribeKeyPairsResponseInfoType
    { Item :: ![DescribeKeyPairsResponseItemType]
    } deriving (Show)

data DescribeKeyPairsResponseItemType = DescribeKeyPairsResponseItemType
    { KeyName        :: !String
    , KeyFingerprint :: !String
    } deriving (Show)

type RunInstances = RunInstancesType

data RunInstancesType = RunInstancesType
    { ImageId                           :: !String
    , MinCount                          :: !Int
    , MaxCount                          :: !Int
    , KeyName                           :: !(Maybe String)
    , GroupSet                          :: !GroupSetType
    , UserData                          :: !(Maybe UserDataType)
    , InstanceType                      :: !String
    , Placement                         :: !(Maybe PlacementRequestType)
    , KernelId                          :: !(Maybe String)
    , RamdiskId                         :: !(Maybe String)
    , BlockDeviceMapping                :: !(Maybe BlockDeviceMappingType)
    , Monitoring                        :: !(Maybe MonitoringInstanceType)
    , SubnetId                          :: !(Maybe String)
    , DisableApiTermination             :: !(Maybe Boolean)
    , InstanceInitiatedShutdownBehavior :: !(Maybe String)
    , License                           :: !(Maybe InstanceLicenseRequestType)
    , PrivateIpAddress                  :: !(Maybe String)
    , ClientToken                       :: !(Maybe String)
    , NetworkInterfaceSet               :: !(Maybe InstanceNetworkInterfaceSetRequestType)
    , IamInstanceProfile                :: !(Maybe IamInstanceProfileRequestType)
    , EbsOptimized                      :: !(Maybe Boolean)
    } deriving (Show)

data IamInstanceProfileRequestType = IamInstanceProfileRequestType
    { Arn  :: !(Maybe String)
    , Name :: !(Maybe String)
    } deriving (Show)

data InstanceNetworkInterfaceSetRequestType = InstanceNetworkInterfaceSetRequestType
    { Item :: ![InstanceNetworkInterfaceSetItemRequestType]
    } deriving (Show)

data InstanceNetworkInterfaceSetItemRequestType = InstanceNetworkInterfaceSetItemRequestType
    { NetworkInterfaceId             :: !(Maybe String)
    , DeviceIndex                    :: !Int
    , SubnetId                       :: !(Maybe String)
    , Description                    :: !(Maybe String)
    , PrivateIpAddress               :: !(Maybe String)
    , GroupSet                       :: !(Maybe SecurityGroupIdSetType)
    , DeleteOnTermination            :: !(Maybe Boolean)
    , PrivateIpAddressesSet          :: !(Maybe PrivateIpAddressesSetRequestType)
    , SecondaryPrivateIpAddressCount :: !(Maybe Int)
    } deriving (Show)

data PrivateIpAddressesSetRequestType = PrivateIpAddressesSetRequestType
    { Item :: ![PrivateIpAddressesSetItemRequestType]
    } deriving (Show)

data PrivateIpAddressesSetItemRequestType = PrivateIpAddressesSetItemRequestType
    { PrivateIpAddress :: !String
    , Primary          :: !(Maybe Boolean)
    } deriving (Show)

data ImportInstanceGroupSetType = ImportInstanceGroupSetType
    { Item :: ![ImportInstanceGroupItemType]
    } deriving (Show)

data ImportInstanceGroupItemType = ImportInstanceGroupItemType
    { GroupId   :: !(Maybe String)
    , GroupName :: !(Maybe String)
    } deriving (Show)

data GroupSetType = GroupSetType
    { Item :: ![GroupItemType]
    } deriving (Show)

data GroupItemType = GroupItemType
    { GroupId   :: !(Maybe String)
    , GroupName :: !(Maybe String)
    } deriving (Show)

data UserDataType = UserDataType
    { Data     :: !(Maybe String)
    , Version  :: !String -- fixed: 1.0
    , Encoding :: !String -- fixed: base64
    } deriving (Show)

data BlockDeviceMappingType = BlockDeviceMappingType
    { Item :: ![BlockDeviceMappingItemType]
    } deriving (Show)

data BlockDeviceMappingItemType = BlockDeviceMappingItemType
    { DeviceName  :: !String
  -- <xs:choice>
    , VirtualName :: !(Maybe String)
    , Ebs         :: !(Maybe EbsBlockDeviceType)
   , NoDevice     :: !(Maybe EmptyElementType)
  -- </xs:choice>
    } deriving (Show)

data EbsBlockDeviceType = EbsBlockDeviceType
    { SnapshotId          :: !(Maybe String)
    , VolumeSize          :: !(Maybe Int)
    , DeleteOnTermination :: !(Maybe Boolean)
    , VolumeType          :: !(Maybe String)
    , Iops                :: !(Maybe Int)
    } deriving (Show)

data PlacementRequestType = PlacementRequestType
    { AvailabilityZone :: !(Maybe String)
    , GroupName        :: !(Maybe String)
    , Tenancy          :: !(Maybe String)
    } deriving (Show)

data SpotPlacementRequestType = SpotPlacementRequestType
    { AvailabilityZone :: !(Maybe String)
    , GroupName        :: !(Maybe String)
    } deriving (Show)

data InstancePlacementType = InstancePlacementType
    { AvailabilityZone :: !(Maybe String)
    , GroupName        :: !(Maybe String)
    } deriving (Show)

data MonitoringInstanceType = MonitoringInstanceType
    { Enabled :: !(Maybe Boolean)
    } deriving (Show)

data InstanceLicenseRequestType = InstanceLicenseRequestType
    { Pool :: !String
    } deriving (Show)

type RunInstancesResponse = RunInstancesResponseType

data RunInstancesResponseType = RunInstancesResponseType
    { RequestId     :: !String
    , ReservationId :: !String
    , OwnerId       :: !String
    , GroupSet      :: !GroupSetType
    , InstancesSet  :: !RunningInstancesSetType
    , RequesterId   :: !(Maybe String)
    } deriving (Show)

data ReservationInfoType = ReservationInfoType
    { ReservationId :: !String
    , OwnerId       :: !String
    , GroupSet      :: !GroupSetType
    , InstancesSet  :: !RunningInstancesSetType
    , RequesterId   :: !(Maybe String)
    } deriving (Show)

data RunningInstancesSetType = RunningInstancesSetType
    { Item :: ![RunningInstancesItemType]
    } deriving (Show)

data RunningInstancesItemType = RunningInstancesItemType
    { InstanceId            :: !String
    , ImageId               :: !(Maybe String)
    , InstanceState         :: !InstanceStateType
    , PrivateDnsName        :: !String
    , DnsName               :: !(Maybe String)
    , Reason                :: !(Maybe String)
    , KeyName               :: !(Maybe String)
    , AmiLaunchIndex        :: !(Maybe String)
    , ProductCodes          :: !(Maybe ProductCodesSetType)
    , InstanceType          :: !String
    , LaunchTime            :: !DateTime
    , Placement             :: !(Maybe PlacementResponseType)
    , KernelId              :: !(Maybe String)
    , RamdiskId             :: !(Maybe String)
    , Platform              :: !(Maybe String)
    , Monitoring            :: !(Maybe InstanceMonitoringStateType)
    , SubnetId              :: !(Maybe String)
    , VpcId                 :: !(Maybe String)
    , PrivateIpAddress      :: !(Maybe String)
    , IpAddress             :: !(Maybe String)
    , SourceDestCheck       :: !(Maybe Boolean)
    , GroupSet              :: !GroupSetType
    , StateReason           :: !(Maybe StateReasonType)
    , Architecture          :: !(Maybe String)
    , RootDeviceType        :: !(Maybe String)
    , RootDeviceName        :: !(Maybe String)
    , BlockDeviceMapping    :: !(Maybe InstanceBlockDeviceMappingResponseType)
    , InstanceLifecycle     :: !(Maybe String)
    , SpotInstanceRequestId :: !(Maybe String)
    , License               :: !(Maybe InstanceLicenseResponseType)
    , VirtualizationType    :: !(Maybe String)
    , ClientToken           :: !(Maybe String)
    , TagSet                :: !(Maybe ResourceTagSetType)
    , Hypervisor            :: !(Maybe String)
    , NetworkInterfaceSet   :: !(Maybe InstanceNetworkInterfaceSetType)
    , IamInstanceProfile    :: !(Maybe IamInstanceProfileResponseType)
    , EbsOptimized          :: !(Maybe Boolean)
    } deriving (Show)

data IamInstanceProfileResponseType = IamInstanceProfileResponseType
    { Arn :: !String
    , Id  :: !String
    } deriving (Show)

data InstanceNetworkInterfaceSetType = InstanceNetworkInterfaceSetType
    { Item :: ![InstanceNetworkInterfaceSetItemType]
    } deriving (Show)

data InstanceNetworkInterfaceSetItemType = InstanceNetworkInterfaceSetItemType
    { NetworkInterfaceId    :: !String
    , SubnetId              :: !(Maybe String)
    , VpcId                 :: !(Maybe String)
    , Description           :: !(Maybe String)
    , OwnerId               :: !String
    , Status                :: !String
    , MacAddress            :: !(Maybe String)
    , PrivateIpAddress      :: !(Maybe String)
    , PrivateDnsName        :: !(Maybe String)
    , SourceDestCheck       :: !(Maybe Boolean)
    , GroupSet              :: !(Maybe GroupSetType)
    , Attachment            :: !InstanceNetworkInterfaceAttachmentType
    , Association           :: !(Maybe InstanceNetworkInterfaceAssociationType)
    , PrivateIpAddressesSet :: !(Maybe InstancePrivateIpAddressesSetType)
    } deriving (Show)

data InstancePrivateIpAddressesSetType = InstancePrivateIpAddressesSetType
    { Item :: ![InstancePrivateIpAddressesSetItemType]
    } deriving (Show)

data InstancePrivateIpAddressesSetItemType = InstancePrivateIpAddressesSetItemType
    { PrivateIpAddress :: !(Maybe String)
    , PrivateDnsName   :: !(Maybe String)
    , Primary          :: !(Maybe Boolean)
    , Association      :: !(Maybe InstanceNetworkInterfaceAssociationType)
    } deriving (Show)

data InstanceNetworkInterfaceAttachmentType = InstanceNetworkInterfaceAttachmentType
    { AttachmentId        :: !String
    , DeviceIndex         :: !Int
    , Status              :: !String
    , AttachTime          :: !DateTime
    , DeleteOnTermination :: !Boolean
    } deriving (Show)

data InstanceNetworkInterfaceAssociationType = InstanceNetworkInterfaceAssociationType
    { PublicIp      :: !String
    , PublicDnsName :: !(Maybe String)
    , IpOwnerId     :: !(Maybe String)
    } deriving (Show)

data PlacementResponseType = PlacementResponseType
    { AvailabilityZone :: !String
    , GroupName        :: !(Maybe String)
    , Tenancy          :: !(Maybe String)
    } deriving (Show)

data StateReasonType = StateReasonType
    { Code    :: !String
    , Message :: !String
    } deriving (Show)

data InstanceBlockDeviceMappingResponseType = InstanceBlockDeviceMappingResponseType
    { Item :: ![InstanceBlockDeviceMappingResponseItemType]
    } deriving (Show)

data InstanceBlockDeviceMappingResponseItemType = InstanceBlockDeviceMappingResponseItemType
    { DeviceName :: !String
  -- <xs:choice>
    , Ebs        :: !EbsInstanceBlockDeviceMappingResponseType
  -- </xs:choice>
    } deriving (Show)

data EbsInstanceBlockDeviceMappingResponseType = EbsInstanceBlockDeviceMappingResponseType
    { VolumeId            :: !String
    , Status              :: !String
    , AttachTime          :: !DateTime
    , DeleteOnTermination :: !(Maybe Boolean)
    } deriving (Show)

data InstanceLicenseResponseType = InstanceLicenseResponseType
    { Pool :: !String
    } deriving (Show)

type DescribeAccountAttributes = DescribeAccountAttributesType

data DescribeAccountAttributesType = DescribeAccountAttributesType
    { AccountAttributeNameSet :: !(Maybe AccountAttributeNameSetType)
    , FilterSet               :: !(Maybe FilterSetType)
    } deriving (Show)

type DescribeAccountAttributesResponse = DescribeAccountAttributesResponseType

data DescribeAccountAttributesResponseType = DescribeAccountAttributesResponseType
    { RequestId           :: !String
    , AccountAttributeSet :: !(Maybe AccountAttributeSetType)
    } deriving (Show)

data AccountAttributeNameSetType = AccountAttributeNameSetType
    { Item :: ![AccountAttributeNameSetItemType]
    } deriving (Show)

data AccountAttributeNameSetItemType = AccountAttributeNameSetItemType
    { AttributeName :: !String
    } deriving (Show)

data AccountAttributeSetType = AccountAttributeSetType
    { Item :: ![AccountAttributeSetItemType]
    } deriving (Show)

data AccountAttributeSetItemType = AccountAttributeSetItemType
    { AttributeName     :: !String
    , AttributeValueSet :: !AccountAttributeValueSetType
    } deriving (Show)

data AccountAttributeValueSetType = AccountAttributeValueSetType
    { Item :: ![AccountAttributeValueSetItemType]
    } deriving (Show)

data AccountAttributeValueSetItemType = AccountAttributeValueSetItemType
    { AttributeValue :: !String
    } deriving (Show)

type DescribeVpcAttribute = DescribeVpcAttributeType

type DescribeVpcAttributeResponse = DescribeVpcAttributeResponseType

data DescribeVpcAttributeType = DescribeVpcAttributeType
    { VpcId                      :: !String
    , DescribeVpcAttributesGroup :: !DescribeVpcAttributesGroup
    } deriving (Show)

data DescribeVpcAttributesGroup = DescribeVpcAttributesGroup
    { EnableDnsSupport   :: !EmptyElementType
    , EnableDnsHostnames :: !EmptyElementType
    } deriving (Show)
data DescribeVpcAttributeResponseType = DescribeVpcAttributeResponseType
    { RequestId          :: !String
    , VpcId              :: !String
  -- <xs:choice>
    , EnableDnsSupport   :: !(Maybe AttributeBooleanValueType)
    , EnableDnsHostnames :: !(Maybe AttributeBooleanValueType)
  -- </xs:choice>
    } deriving (Show)

type ModifyVpcAttribute = ModifyVpcAttributeType

type ModifyVpcAttributeResponse = ModifyVpcAttributeResponseType

data ModifyVpcAttributeType = ModifyVpcAttributeType
    { VpcId              :: !String
  -- <xs:choice>
    , EnableDnsSupport   :: !(Maybe AttributeBooleanValueType)
    , EnableDnsHostnames :: !(Maybe AttributeBooleanValueType)
  -- </xs:choice>
    } deriving (Show)

data ModifyVpcAttributeResponseType = ModifyVpcAttributeResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

type GetConsoleOutput = GetConsoleOutputType

data GetConsoleOutputType = GetConsoleOutputType
    { InstanceId :: !String
    } deriving (Show)

type GetConsoleOutputResponse = GetConsoleOutputResponseType

data GetConsoleOutputResponseType = GetConsoleOutputResponseType
    { RequestId  :: !String
    , InstanceId :: !String
    , Timestamp  :: !DateTime
    , Output     :: !String
    } deriving (Show)

type GetPasswordData = GetPasswordDataType

data GetPasswordDataType = GetPasswordDataType
    { InstanceId :: !String
    } deriving (Show)

type GetPasswordDataResponse = GetPasswordDataResponseType

data GetPasswordDataResponseType = GetPasswordDataResponseType
    { RequestId    :: !String
    , InstanceId   :: !String
    , Timestamp    :: !DateTime
    , PasswordData :: !String
    } deriving (Show)

data InstanceIdType = InstanceIdType
    { InstanceId :: !String
    } deriving (Show)

data InstanceIdSetType = InstanceIdSetType
    { Item :: ![InstanceIdType]
    } deriving (Show)

data InstanceStateChangeType = InstanceStateChangeType
    { InstanceId    :: !String
    , CurrentState  :: !InstanceStateType
    , PreviousState :: !InstanceStateType
    } deriving (Show)

data InstanceStateChangeSetType = InstanceStateChangeSetType
    { Item :: ![InstanceStateChangeType]
    } deriving (Show)

type TerminateInstances = TerminateInstancesType

type TerminateInstancesResponse = TerminateInstancesResponseType

data TerminateInstancesType = TerminateInstancesType
    { InstancesSet :: !InstanceIdSetType
    } deriving (Show)

data TerminateInstancesResponseType = TerminateInstancesResponseType
    { RequestId    :: !String
    , InstancesSet :: !InstanceStateChangeSetType
    } deriving (Show)

data InstanceBlockDeviceMappingType = InstanceBlockDeviceMappingType
    { Item :: ![InstanceBlockDeviceMappingItemType]
    } deriving (Show)

data InstanceBlockDeviceMappingItemType = InstanceBlockDeviceMappingItemType
    { DeviceName  :: !String
  -- <xs:choice>
    , VirtualName :: !(Maybe String)
    , Ebs         :: !(Maybe InstanceEbsBlockDeviceType)
    , NoDevice    :: !(Maybe EmptyElementType)
  -- </xs:choice>
    } deriving (Show)

data InstanceEbsBlockDeviceType = InstanceEbsBlockDeviceType
    { VolumeId            :: !String
    , DeleteOnTermination :: !(Maybe Boolean)
    } deriving (Show)

type StopInstances = StopInstancesType

type StopInstancesResponse = StopInstancesResponseType

data StopInstancesType = StopInstancesType
    { InstancesSet :: !InstanceIdSetType
    , Force        :: !(Maybe Boolean)
    } deriving (Show)

data StopInstancesResponseType = StopInstancesResponseType
    { RequestId    :: !String
    , InstancesSet :: !InstanceStateChangeSetType
    } deriving (Show)

type StartInstances = StartInstancesType

type StartInstancesResponse = StartInstancesResponseType

data StartInstancesType = StartInstancesType
    { InstancesSet :: !InstanceIdSetType
    } deriving (Show)

data StartInstancesResponseType = StartInstancesResponseType
    { RequestId    :: !String
    , InstancesSet :: !InstanceStateChangeSetType
    } deriving (Show)

type RebootInstances = RebootInstancesType

data RebootInstancesType = RebootInstancesType
    { InstancesSet :: !RebootInstancesInfoType
    } deriving (Show)

data RebootInstancesInfoType = RebootInstancesInfoType
    {
  Item :: !(NonEmpty RebootInstancesItemType)
    } deriving (Show)

data RebootInstancesItemType = RebootInstancesItemType
    { InstanceId :: !String
    } deriving (Show)

type RebootInstancesResponse = RebootInstancesResponseType

data RebootInstancesResponseType = RebootInstancesResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

type DescribeInstances = DescribeInstancesType

data DescribeInstancesType = DescribeInstancesType
    { InstancesSet :: !DescribeInstancesInfoType
    , FilterSet    :: !(Maybe FilterSetType)
    } deriving (Show)

data DescribeInstancesInfoType = DescribeInstancesInfoType
    { Item :: ![DescribeInstancesItemType]
    } deriving (Show)

data DescribeInstancesItemType = DescribeInstancesItemType
    { InstanceId :: !String
    } deriving (Show)

type DescribeInstancesResponse = DescribeInstancesResponseType

data DescribeInstancesResponseType = DescribeInstancesResponseType
    { RequestId      :: !String
    , ReservationSet :: !ReservationSetType
    } deriving (Show)

data ReservationSetType = ReservationSetType
    { Item :: ![ReservationInfoType]
    } deriving (Show)

data UnavailableResultSetType = UnavailableResultSetType
    { Item :: ![UnavailableResultType]
    } deriving (Show)

data UnavailableResultType = UnavailableResultType
    { AvailabilityZone :: !String
    } deriving (Show)

type DescribeImages = DescribeImagesType

data DescribeImagesType = DescribeImagesType
    { ExecutableBySet :: !(Maybe DescribeImagesExecutableBySetType)
    , ImagesSet       :: !DescribeImagesInfoType
    , OwnersSet       :: !(Maybe DescribeImagesOwnersType)
    , FilterSet       :: !(Maybe FilterSetType)
    } deriving (Show)

data DescribeImagesInfoType = DescribeImagesInfoType
    { Item :: ![DescribeImagesItemType]
    } deriving (Show)

data DescribeImagesItemType = DescribeImagesItemType
    { ImageId :: !String
    } deriving (Show)

data DescribeImagesOwnersType = DescribeImagesOwnersType
    { Item :: ![DescribeImagesOwnerType]
    } deriving (Show)

data DescribeImagesOwnerType = DescribeImagesOwnerType
    { Owner :: !String
    } deriving (Show)

data DescribeImagesExecutableBySetType = DescribeImagesExecutableBySetType
    { Item :: ![DescribeImagesExecutableByType]
    } deriving (Show)

data DescribeImagesExecutableByType = DescribeImagesExecutableByType
    { User :: !String
    } deriving (Show)

type DescribeImagesResponse = DescribeImagesResponseType

data DescribeImagesResponseType = DescribeImagesResponseType
    { RequestId :: !String
    , ImagesSet :: !DescribeImagesResponseInfoType
    } deriving (Show)

data DescribeImagesResponseInfoType = DescribeImagesResponseInfoType
    { Item :: ![DescribeImagesResponseItemType]
    } deriving (Show)

data DescribeImagesResponseItemType = DescribeImagesResponseItemType
    { ImageId            :: !String
    , ImageLocation      :: !(Maybe String)
    , ImageState         :: !String
    , ImageOwnerId       :: !String
    , IsPublic           :: !Boolean
    , ProductCodes       :: !(Maybe ProductCodesSetType)
    , Architecture       :: !(Maybe String)
    , ImageType          :: !(Maybe String)
    , KernelId           :: !(Maybe String)
    , RamdiskId          :: !(Maybe String)
    , Platform           :: !(Maybe String)
    , StateReason        :: !(Maybe StateReasonType)
    , ImageOwnerAlias    :: !(Maybe String)
    , Name               :: !(Maybe String)
    , Description        :: !(Maybe String)
    , RootDeviceType     :: !(Maybe String)
    , RootDeviceName     :: !(Maybe String)
    , BlockDeviceMapping :: !(Maybe BlockDeviceMappingType)
    , VirtualizationType :: !(Maybe String)
    , TagSet             :: !(Maybe ResourceTagSetType)
    , Hypervisor         :: !(Maybe String)
    } deriving (Show)

type CreateSecurityGroup = CreateSecurityGroupType

data CreateSecurityGroupType = CreateSecurityGroupType
    { GroupName        :: !String
    , GroupDescription :: !String
    , VpcId            :: !(Maybe String)
    } deriving (Show)

type CreateSecurityGroupResponse = CreateSecurityGroupResponseType

data CreateSecurityGroupResponseType = CreateSecurityGroupResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    , GroupId   :: !String
    } deriving (Show)

type DeleteSecurityGroup = DeleteSecurityGroupType

data DeleteSecurityGroupType = DeleteSecurityGroupType
-- <xs:choice>
    , GroupId   :: !(Maybe String)
    , GroupName :: !(Maybe String)
-- </xs:choice>

type DeleteSecurityGroupResponse = DeleteSecurityGroupResponseType

data DeleteSecurityGroupResponseType = DeleteSecurityGroupResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

type DescribeSecurityGroups = DescribeSecurityGroupsType

data DescribeSecurityGroupsType = DescribeSecurityGroupsType
    { SecurityGroupSet   :: !DescribeSecurityGroupsSetType
    , SecurityGroupIdSet :: !(Maybe DescribeSecurityGroupsIdSetType)
    , FilterSet          :: !(Maybe FilterSetType)
    } deriving (Show)

data DescribeSecurityGroupsSetType = DescribeSecurityGroupsSetType
    { Item :: ![DescribeSecurityGroupsSetItemType]
    } deriving (Show)

data DescribeSecurityGroupsSetItemType = DescribeSecurityGroupsSetItemType
    { GroupName :: !String
    } deriving (Show)

data DescribeSecurityGroupsIdSetType = DescribeSecurityGroupsIdSetType
    { Item :: ![DescribeSecurityGroupsIdSetItemType]
    } deriving (Show)

data DescribeSecurityGroupsIdSetItemType = DescribeSecurityGroupsIdSetItemType
    { GroupId :: !String
    } deriving (Show)

type DescribeSecurityGroupsResponse = DescribeSecurityGroupsResponseType

data DescribeSecurityGroupsResponseType = DescribeSecurityGroupsResponseType
    { RequestId         :: !String
    , SecurityGroupInfo :: !SecurityGroupSetType
    } deriving (Show)

data IpPermissionSetType = IpPermissionSetType
    { Item :: ![IpPermissionType]
    } deriving (Show)

data IpPermissionType = IpPermissionType
    { IpProtocol :: !String
    , FromPort   :: !(Maybe Int)
    , ToPort     :: !(Maybe Int)
    , Groups     :: !UserIdGroupPairSetType
    , IpRanges   :: !IpRangeSetType
    } deriving (Show)

data IpRangeSetType = IpRangeSetType
    { Item :: ![IpRangeItemType]
    } deriving (Show)

data IpRangeItemType = IpRangeItemType
    { CidrIp :: !String
    } deriving (Show)

data UserIdGroupPairSetType = UserIdGroupPairSetType
    { Item :: ![UserIdGroupPairType]
    } deriving (Show)

data UserIdGroupPairType = UserIdGroupPairType
    { UserId    :: !(Maybe String)
    , GroupId   :: !(Maybe String)
    , GroupName :: !(Maybe String)
    } deriving (Show)

data SecurityGroupSetType = SecurityGroupSetType
    { Item :: ![SecurityGroupItemType]
    } deriving (Show)

data SecurityGroupItemType = SecurityGroupItemType
    { OwnerId             :: !String
    , GroupId             :: !String
    , GroupName           :: !String
    , GroupDescription    :: !String
    , VpcId               :: !(Maybe String)
    , IpPermissions       :: !IpPermissionSetType
    , IpPermissionsEgress :: !(Maybe IpPermissionSetType)
    , TagSet              :: !(Maybe ResourceTagSetType)
    } deriving (Show)

type AuthorizeSecurityGroupIngress = AuthorizeSecurityGroupIngressType

data AuthorizeSecurityGroupIngressType = AuthorizeSecurityGroupIngressType
    { UserId        :: !(Maybe String)
  -- <xs:choice>
    , GroupId       :: !(Maybe String)
    , GroupName     :: !(Maybe String)
  -- </xs:choice>
    , IpPermissions :: !IpPermissionSetType
    } deriving (Show)

type AuthorizeSecurityGroupIngressResponse = AuthorizeSecurityGroupIngressResponseType

data AuthorizeSecurityGroupIngressResponseType = AuthorizeSecurityGroupIngressResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

type RevokeSecurityGroupIngress = RevokeSecurityGroupIngressType

data RevokeSecurityGroupIngressType = RevokeSecurityGroupIngressType
    { UserId        :: !(Maybe String)
  -- <xs:choice>
    , GroupId       :: !(Maybe String)
    , GroupName     :: !(Maybe String)
  -- </xs:choice>
    , IpPermissions :: !IpPermissionSetType
    } deriving (Show)

type RevokeSecurityGroupIngressResponse = RevokeSecurityGroupIngressResponseType

data RevokeSecurityGroupIngressResponseType = RevokeSecurityGroupIngressResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

type AuthorizeSecurityGroupEgress = AuthorizeSecurityGroupEgressType

data AuthorizeSecurityGroupEgressType = AuthorizeSecurityGroupEgressType
    { GroupId       :: !String
    , IpPermissions :: !IpPermissionSetType
    } deriving (Show)

type AuthorizeSecurityGroupEgressResponse = AuthorizeSecurityGroupEgressResponseType

data AuthorizeSecurityGroupEgressResponseType = AuthorizeSecurityGroupEgressResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

type RevokeSecurityGroupEgress = RevokeSecurityGroupEgressType

data RevokeSecurityGroupEgressType = RevokeSecurityGroupEgressType
    { GroupId       :: !String
    , IpPermissions :: !IpPermissionSetType
    } deriving (Show)

type RevokeSecurityGroupEgressResponse = RevokeSecurityGroupEgressResponseType

data RevokeSecurityGroupEgressResponseType = RevokeSecurityGroupEgressResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

data InstanceStateType = InstanceStateType
    { Code :: !Int
    , Name :: !String
    } deriving (Show)

type ModifyInstanceAttribute = ModifyInstanceAttributeType

data ModifyInstanceAttributeType = ModifyInstanceAttributeType
    { InstanceId                        :: !String
  -- <xs:choice>
    , InstanceType                      :: !(Maybe AttributeValueType)
    , Kernel                            :: !(Maybe AttributeValueType)
    , Ramdisk                           :: !(Maybe AttributeValueType)
    , UserData                          :: !(Maybe AttributeValueType)
    , DisableApiTermination             :: !(Maybe AttributeBooleanValueType)
    , InstanceInitiatedShutdownBehavior :: !(Maybe AttributeValueType)
    , BlockDeviceMapping                :: !(Maybe InstanceBlockDeviceMappingType)
    , SourceDestCheck                   :: !(Maybe AttributeBooleanValueType)
    , GroupSet                          :: !(Maybe SecurityGroupIdSetType)
    , EbsOptimized                      :: !(Maybe AttributeBooleanValueType)
  -- </xs:choice>
    } deriving (Show)

data SecurityGroupIdSetType = SecurityGroupIdSetType
    { Item :: ![SecurityGroupIdSetItemType]
    } deriving (Show)

data SecurityGroupIdSetItemType = SecurityGroupIdSetItemType
    { GroupId :: !String
    } deriving (Show)

type ModifyInstanceAttributeResponse = ModifyInstanceAttributeResponseType

data ModifyInstanceAttributeResponseType = ModifyInstanceAttributeResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

type ResetInstanceAttribute = ResetInstanceAttributeType

data ResetInstanceAttributeType = ResetInstanceAttributeType
    { InstanceId                   :: !String
    , ResetInstanceAttributesGroup :: !ResetInstanceAttributesGroup
    } deriving (Show)

data ResetInstanceAttributesGroup = ResetInstanceAttributesGroup
    { Kernel          :: !EmptyElementType
    , Ramdisk         :: !EmptyElementType
    , SourceDestCheck :: !EmptyElementType
    } deriving (Show)
type ResetInstanceAttributeResponse = ResetInstanceAttributeResponseType

data ResetInstanceAttributeResponseType = ResetInstanceAttributeResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

type DescribeInstanceAttribute = DescribeInstanceAttributeType

data DescribeInstanceAttributeType = DescribeInstanceAttributeType
    { InstanceId                      :: !String
    , DescribeInstanceAttributesGroup :: !DescribeInstanceAttributesGroup
    } deriving (Show)

data DescribeInstanceAttributesGroup = DescribeInstanceAttributesGroup
    { InstanceType                      :: !EmptyElementType
    , Kernel                            :: !EmptyElementType
    , Ramdisk                           :: !EmptyElementType
    , UserData                          :: !EmptyElementType
    , DisableApiTermination             :: !EmptyElementType
    , InstanceInitiatedShutdownBehavior :: !EmptyElementType
    , RootDeviceName                    :: !EmptyElementType
    , BlockDeviceMapping                :: !EmptyElementType
    , SourceDestCheck                   :: !EmptyElementType
    , GroupSet                          :: !EmptyElementType
    , ProductCodes                      :: !EmptyElementType
    , EbsOptimized                      :: !EmptyElementType
    } deriving (Show)
type DescribeInstanceAttributeResponse = DescribeInstanceAttributeResponseType

data DescribeInstanceAttributeResponseType = DescribeInstanceAttributeResponseType
    { RequestId                         :: !String
    , InstanceId                        :: !String
  -- <xs:choice>
    , InstanceType                      :: !(Maybe NullableAttributeValueType)
    , Kernel                            :: !(Maybe NullableAttributeValueType)
    , Ramdisk                           :: !(Maybe NullableAttributeValueType)
    , UserData                          :: !(Maybe NullableAttributeValueType)
    , DisableApiTermination             :: !(Maybe NullableAttributeBooleanValueType)
    , InstanceInitiatedShutdownBehavior :: !(Maybe NullableAttributeValueType)
    , RootDeviceName                    :: !(Maybe NullableAttributeValueType)
    , BlockDeviceMapping                :: !(Maybe InstanceBlockDeviceMappingResponseType)
    , SourceDestCheck                   :: !(Maybe NullableAttributeBooleanValueType)
    , GroupSet                          :: !(Maybe GroupSetType)
    , ProductCodes                      :: !(Maybe ProductCodesSetType)
    , EbsOptimized                      :: !(Maybe NullableAttributeBooleanValueType)
  -- </xs:choice>
    } deriving (Show)

type ModifyImageAttribute = ModifyImageAttributeType

data ModifyImageAttributeType = ModifyImageAttributeType
    { ImageId          :: !String
  -- <xs:choice>
    , LaunchPermission :: !(Maybe LaunchPermissionOperationType)
    , ProductCodes     :: !(Maybe ProductCodeListType)
    , Description      :: !(Maybe AttributeValueType)
  -- </xs:choice>
    } deriving (Show)

data LaunchPermissionOperationType = LaunchPermissionOperationType
    {
-- <xs:choice>
    , Add    :: !(Maybe LaunchPermissionListType)
    , Remove :: !(Maybe LaunchPermissionListType)
-- </xs:choice>
    } deriving (Show)

data LaunchPermissionListType = LaunchPermissionListType
    { Item :: ![LaunchPermissionItemType]
    } deriving (Show)

data LaunchPermissionItemType = LaunchPermissionItemType
    {
-- <xs:choice>
    , UserId :: !(Maybe String)
    , Group  :: !(Maybe String)
-- </xs:choice>
    } deriving (Show)

data ProductCodeListType = ProductCodeListType
    { Item :: ![ProductCodeItemType]
    } deriving (Show)

data ProductCodeItemType = ProductCodeItemType
    { ProductCode :: !String
    } deriving (Show)

type ModifyImageAttributeResponse = ModifyImageAttributeResponseType

data ModifyImageAttributeResponseType = ModifyImageAttributeResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

type ResetImageAttribute = ResetImageAttributeType

data ResetImageAttributeType = ResetImageAttributeType
    { ImageId                   :: !String
    , ResetImageAttributesGroup :: !ResetImageAttributesGroup
    } deriving (Show)

data ResetImageAttributesGroup = ResetImageAttributesGroup
    { LaunchPermission :: !EmptyElementType
    } deriving (Show)

data EmptyElementType = EmptyElementType

type ResetImageAttributeResponse = ResetImageAttributeResponseType

data ResetImageAttributeResponseType = ResetImageAttributeResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

type DescribeImageAttribute = DescribeImageAttributeType

data DescribeImageAttributeType = DescribeImageAttributeType
    { ImageId                      :: !String
    , DescribeImageAttributesGroup :: !DescribeImageAttributesGroup
    } deriving (Show)

data DescribeImageAttributesGroup = DescribeImageAttributesGroup
    { LaunchPermission     :: !EmptyElementType
    , ProductCodes         :: !EmptyElementType
    , Kernel               :: !EmptyElementType
    , Ramdisk              :: !EmptyElementType
    , BlockDeviceMapping   :: !EmptyElementType
    , Description          :: !EmptyElementType
    , InstanceTypeCategory :: !EmptyElementType
    } deriving (Show)
type DescribeImageAttributeResponse = DescribeImageAttributeResponseType

data DescribeImageAttributeResponseType = DescribeImageAttributeResponseType
    { RequestId          :: !String
    , ImageId            :: !String
  -- <xs:choice>
    , LaunchPermission   :: !(Maybe LaunchPermissionListType)
    , ProductCodes       :: !(Maybe ProductCodesSetType)
    , Kernel             :: !(Maybe NullableAttributeValueType)
    , Ramdisk            :: !(Maybe NullableAttributeValueType)
    , Description        :: !(Maybe NullableAttributeValueType)
    , BlockDeviceMapping :: !(Maybe BlockDeviceMappingType)
  -- </xs:choice>
    } deriving (Show)

data NullableAttributeValueType = NullableAttributeValueType
    { Value :: !(Maybe String)
    } deriving (Show)

data NullableAttributeBooleanValueType = NullableAttributeBooleanValueType
    { Value :: !(Maybe Boolean)
    } deriving (Show)

data AttributeValueType = AttributeValueType
    { Value :: !String
    } deriving (Show)

data AttributeBooleanValueType = AttributeBooleanValueType
    { Value :: !Boolean
    } deriving (Show)

type ConfirmProductInstance = ConfirmProductInstanceType

data ConfirmProductInstanceType = ConfirmProductInstanceType
    { ProductCode :: !String
    , InstanceId  :: !String
    } deriving (Show)

data ProductCodesSetType = ProductCodesSetType
    { Item :: ![ProductCodesSetItemType]
    } deriving (Show)

data ProductCodesSetItemType = ProductCodesSetItemType
    { ProductCode :: !String
    , Type        :: !String
    } deriving (Show)

type ConfirmProductInstanceResponse = ConfirmProductInstanceResponseType

data ConfirmProductInstanceResponseType = ConfirmProductInstanceResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    , OwnerId   :: !(Maybe String)
    } deriving (Show)

type DescribeAvailabilityZones = DescribeAvailabilityZonesType

data DescribeAvailabilityZonesType = DescribeAvailabilityZonesType
    { AvailabilityZoneSet :: !DescribeAvailabilityZonesSetType
    , FilterSet           :: !(Maybe FilterSetType)
    } deriving (Show)

data DescribeAvailabilityZonesSetType = DescribeAvailabilityZonesSetType
    { Item :: ![DescribeAvailabilityZonesSetItemType]
    } deriving (Show)

data DescribeAvailabilityZonesSetItemType = DescribeAvailabilityZonesSetItemType
    { ZoneName :: !String
    } deriving (Show)

type DescribeAvailabilityZonesResponse = DescribeAvailabilityZonesResponseType

data DescribeAvailabilityZonesResponseType = DescribeAvailabilityZonesResponseType
    { RequestId            :: !String
    , AvailabilityZoneInfo :: !AvailabilityZoneSetType
    } deriving (Show)

data AvailabilityZoneSetType = AvailabilityZoneSetType
    { Item :: ![AvailabilityZoneItemType]
    } deriving (Show)

data AvailabilityZoneMessageType = AvailabilityZoneMessageType
    { Message :: !String
    } deriving (Show)

data AvailabilityZoneMessageSetType = AvailabilityZoneMessageSetType
    { Item :: ![AvailabilityZoneMessageType]
    } deriving (Show)

data AvailabilityZoneItemType = AvailabilityZoneItemType
    { ZoneName   :: !String
    , ZoneState  :: !String
    , RegionName :: !String
    , MessageSet :: !AvailabilityZoneMessageSetType
    } deriving (Show)

type AllocateAddress = AllocateAddressType

data AllocateAddressType = AllocateAddressType
    { Domain :: !(Maybe String)
    } deriving (Show)

type AllocateAddressResponse = AllocateAddressResponseType

data AllocateAddressResponseType = AllocateAddressResponseType
    { RequestId    :: !String
    , PublicIp     :: !String
    , Domain       :: !String
    , AllocationId :: !(Maybe String)
    } deriving (Show)

type ReleaseAddress = ReleaseAddressType

data ReleaseAddressType = ReleaseAddressType
    {
  -- <xs:choice>
    , PublicIp     :: !(Maybe String)
    , AllocationId :: !(Maybe String)
  -- </xs:choice>
    } deriving (Show)

type ReleaseAddressResponse = ReleaseAddressResponseType

data ReleaseAddressResponseType = ReleaseAddressResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

type DescribeAddresses = DescribeAddressesType

data DescribeAddressesType = DescribeAddressesType
    { PublicIpsSet     :: !DescribeAddressesInfoType
    , AllocationIdsSet :: !AllocationIdSetType
    , FilterSet        :: !(Maybe FilterSetType)
    } deriving (Show)

data AllocationIdSetType = AllocationIdSetType
    { Item :: ![AllocationIdSetItemType]
    } deriving (Show)

data AllocationIdSetItemType = AllocationIdSetItemType
    { AllocationId :: !String
    } deriving (Show)

data DescribeAddressesInfoType = DescribeAddressesInfoType
    { Item :: ![DescribeAddressesItemType]
    } deriving (Show)

data DescribeAddressesItemType = DescribeAddressesItemType
    { PublicIp :: !String
    } deriving (Show)

type DescribeAddressesResponse = DescribeAddressesResponseType

data DescribeAddressesResponseType = DescribeAddressesResponseType
    { RequestId    :: !String
    , AddressesSet :: !DescribeAddressesResponseInfoType
    } deriving (Show)

data DescribeAddressesResponseInfoType = DescribeAddressesResponseInfoType
    { Item :: ![DescribeAddressesResponseItemType]
    } deriving (Show)

data DescribeAddressesResponseItemType = DescribeAddressesResponseItemType
    { PublicIp                :: !String
    , AllocationId            :: !(Maybe String)
    , Domain                  :: !String
    , InstanceId              :: !(Maybe String)
    , AssociationId           :: !(Maybe String)
    , NetworkInterfaceId      :: !(Maybe String)
    , NetworkInterfaceOwnerId :: !(Maybe String)
    , PrivateIpAddress        :: !(Maybe String)
    } deriving (Show)

type AssociateAddress = AssociateAddressType

data AssociateAddressType = AssociateAddressType
    {
  -- <xs:choice>
    , PublicIp           :: !(Maybe String)
    , AllocationId       :: !(Maybe String)
  -- </xs:choice>
  -- <xs:choice>
    , NetworkInterfaceId :: !(Maybe String)
    , InstanceId         :: !(Maybe String)
  -- </xs:choice>
    , PrivateIpAddress   :: !(Maybe String)
    , AllowReassociation :: !(Maybe Boolean)
    } deriving (Show)

type AssociateAddressResponse = AssociateAddressResponseType

data AssociateAddressResponseType = AssociateAddressResponseType
    { RequestId     :: !String
    , Return        :: !Boolean
    , AssociationId :: !(Maybe String)
    } deriving (Show)

type DisassociateAddress = DisassociateAddressType

data DisassociateAddressType = DisassociateAddressType
    {
-- <xs:choice>
    , PublicIp      :: !(Maybe String)
    , AssociationId :: !(Maybe String)
-- </xs:choice>
     } deriving (Show)

type DisassociateAddressResponse = DisassociateAddressResponseType

data DisassociateAddressResponseType = DisassociateAddressResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

type CreateVolume = CreateVolumeType

data CreateVolumeType = CreateVolumeType
    { Size             :: !(Maybe String)
    , SnapshotId       :: !(Maybe String)
    , AvailabilityZone :: !String
    , VolumeType       :: !(Maybe String)
    , Iops             :: !(Maybe Int)
    } deriving (Show)

type CreateVolumeResponse = CreateVolumeResponseType

data CreateVolumeResponseType = CreateVolumeResponseType
    { RequestId        :: !String
    , VolumeId         :: !String
    , Size             :: !String
    , SnapshotId       :: !String
    , AvailabilityZone :: !String
    , Status           :: !String
    , CreateTime       :: !DateTime
    , VolumeType       :: !String
    , Iops             :: !(Maybe Int)
    } deriving (Show)

type DeleteVolume = DeleteVolumeType

data DeleteVolumeType = DeleteVolumeType
    { VolumeId :: !String
    } deriving (Show)

type DeleteVolumeResponse = DeleteVolumeResponseType

data DeleteVolumeResponseType = DeleteVolumeResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

type DescribeVolumes = DescribeVolumesType

data DescribeVolumesType = DescribeVolumesType
    { VolumeSet :: !DescribeVolumesSetType
    , FilterSet :: !(Maybe FilterSetType)
    } deriving (Show)

data DescribeVolumesSetType = DescribeVolumesSetType
    { Item :: ![DescribeVolumesSetItemType]
    } deriving (Show)

data DescribeVolumesSetItemType = DescribeVolumesSetItemType
    { VolumeId :: !String
    } deriving (Show)

type DescribeVolumesResponse = DescribeVolumesResponseType

data DescribeVolumesResponseType = DescribeVolumesResponseType
    { RequestId :: !String
    , VolumeSet :: !DescribeVolumesSetResponseType
    } deriving (Show)

data DescribeVolumesSetResponseType = DescribeVolumesSetResponseType
    { Item :: ![DescribeVolumesSetItemResponseType]
    } deriving (Show)

data DescribeVolumesSetItemResponseType = DescribeVolumesSetItemResponseType
    { VolumeId         :: !String
    , Size             :: !String
    , SnapshotId       :: !String
    , AvailabilityZone :: !String
    , Status           :: !String
    , CreateTime       :: !DateTime
    , AttachmentSet    :: !AttachmentSetResponseType
    , TagSet           :: !(Maybe ResourceTagSetType)
    , VolumeType       :: !String
    , Iops             :: !(Maybe Int)
    } deriving (Show)

data AttachmentSetResponseType = AttachmentSetResponseType
    { Item :: ![AttachmentSetItemResponseType]
    } deriving (Show)

data AttachmentSetItemResponseType = AttachmentSetItemResponseType
    { VolumeId            :: !String
    , InstanceId          :: !String
    , Device              :: !String
    , Status              :: !String
    , AttachTime          :: !DateTime
    , DeleteOnTermination :: !Boolean
    } deriving (Show)

type AttachVolume = AttachVolumeType

data AttachVolumeType = AttachVolumeType
    { VolumeId   :: !String
    , InstanceId :: !String
    , Device     :: !String
    } deriving (Show)

type AttachVolumeResponse = AttachVolumeResponseType

data AttachVolumeResponseType = AttachVolumeResponseType
    { RequestId  :: !String
    , VolumeId   :: !String
    , InstanceId :: !String
    , Device     :: !String
    , Status     :: !String
    , AttachTime :: !DateTime
    } deriving (Show)

type DetachVolume = DetachVolumeType

data DetachVolumeType = DetachVolumeType
    { VolumeId   :: !String
    , InstanceId :: !(Maybe String)
    , Device     :: !(Maybe String)
    , Force      :: !(Maybe Boolean)
    } deriving (Show)

type DetachVolumeResponse = DetachVolumeResponseType

data DetachVolumeResponseType = DetachVolumeResponseType
    { RequestId  :: !String
    , VolumeId   :: !String
    , InstanceId :: !String
    , Device     :: !String
    , Status     :: !String
    , AttachTime :: !DateTime
    } deriving (Show)

type CreateSnapshot = CreateSnapshotType

data CreateSnapshotType = CreateSnapshotType
    { VolumeId    :: !String
    , Description :: !(Maybe String)
    } deriving (Show)

type CreateSnapshotResponse = CreateSnapshotResponseType

data CreateSnapshotResponseType = CreateSnapshotResponseType
    { RequestId   :: !String
    , SnapshotId  :: !String
    , VolumeId    :: !String
    , Status      :: !String
    , StartTime   :: !DateTime
    , Progress    :: !String
    , OwnerId     :: !String
    , VolumeSize  :: !String
    , Description :: !(Maybe String)
    } deriving (Show)

type CopySnapshot = CopySnapshotType

data CopySnapshotType = CopySnapshotType
    { SourceRegion     :: !String
    , SourceSnapshotId :: !String
    , Description      :: !(Maybe String)
    } deriving (Show)

type CopySnapshotResponse = CopySnapshotResponseType

data CopySnapshotResponseType = CopySnapshotResponseType
    { RequestId  :: !String
    , SnapshotId :: !String
    } deriving (Show)

type DeleteSnapshot = DeleteSnapshotType

data DeleteSnapshotType = DeleteSnapshotType
    { SnapshotId :: !String
    } deriving (Show)

type DeleteSnapshotResponse = DeleteSnapshotResponseType

data DeleteSnapshotResponseType = DeleteSnapshotResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

type DescribeSnapshots = DescribeSnapshotsType

data DescribeSnapshotsType = DescribeSnapshotsType
    { SnapshotSet     :: !DescribeSnapshotsSetType
    , OwnersSet       :: !(Maybe DescribeSnapshotsOwnersType)
    , RestorableBySet :: !(Maybe DescribeSnapshotsRestorableBySetType)
    , FilterSet       :: !(Maybe FilterSetType)
    } deriving (Show)

data DescribeSnapshotsSetType = DescribeSnapshotsSetType
    { Item :: ![DescribeSnapshotsSetItemType]
    } deriving (Show)

data DescribeSnapshotsSetItemType = DescribeSnapshotsSetItemType
    { SnapshotId :: !String
    } deriving (Show)

data DescribeSnapshotsOwnersType = DescribeSnapshotsOwnersType
    { Item :: ![DescribeSnapshotsOwnerType]
    } deriving (Show)

data DescribeSnapshotsOwnerType = DescribeSnapshotsOwnerType
    { Owner :: !String
    } deriving (Show)

data DescribeSnapshotsRestorableBySetType = DescribeSnapshotsRestorableBySetType
    { Item :: ![DescribeSnapshotsRestorableByType]
    } deriving (Show)

data DescribeSnapshotsRestorableByType = DescribeSnapshotsRestorableByType
    { User :: !String
    } deriving (Show)

type DescribeSnapshotsResponse = DescribeSnapshotsResponseType

data DescribeSnapshotsResponseType = DescribeSnapshotsResponseType
    { RequestId   :: !String
    , SnapshotSet :: !DescribeSnapshotsSetResponseType
    } deriving (Show)

data DescribeSnapshotsSetResponseType = DescribeSnapshotsSetResponseType
    { Item :: ![DescribeSnapshotsSetItemResponseType]
    } deriving (Show)

data DescribeSnapshotsSetItemResponseType = DescribeSnapshotsSetItemResponseType
    { SnapshotId  :: !String
    , VolumeId    :: !String
    , Status      :: !String
    , StartTime   :: !DateTime
    , Progress    :: !String
    , OwnerId     :: !String
    , VolumeSize  :: !String
    , Description :: !(Maybe String)
    , OwnerAlias  :: !(Maybe String)
    , TagSet      :: !(Maybe ResourceTagSetType)
    } deriving (Show)

type ModifySnapshotAttribute = ModifySnapshotAttributeType

data ModifySnapshotAttributeType = ModifySnapshotAttributeType
    { SnapshotId             :: !String
    , CreateVolumePermission :: !CreateVolumePermissionOperationType
    } deriving (Show)

data CreateVolumePermissionOperationType = CreateVolumePermissionOperationType
    {
-- <xs:choice>
    , Add    :: !(Maybe CreateVolumePermissionListType)
    , Remove :: !(Maybe CreateVolumePermissionListType)
-- </xs:choice>
    } deriving (Show)

data CreateVolumePermissionListType = CreateVolumePermissionListType
    { Item :: ![CreateVolumePermissionItemType]
    } deriving (Show)

data CreateVolumePermissionItemType = CreateVolumePermissionItemType
    {
-- <xs:choice>
    , UserId :: !(Maybe String)
    , Group  :: !(Maybe String)
-- </xs:choice>
    } deriving (Show)

type ModifySnapshotAttributeResponse = ModifySnapshotAttributeResponseType

data ModifySnapshotAttributeResponseType = ModifySnapshotAttributeResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

type ResetSnapshotAttribute = ResetSnapshotAttributeType

data ResetSnapshotAttributeType = ResetSnapshotAttributeType
    { SnapshotId                   :: !String
    , ResetSnapshotAttributesGroup :: !ResetSnapshotAttributesGroup
    } deriving (Show)

data ResetSnapshotAttributesGroup = ResetSnapshotAttributesGroup
    { CreateVolumePermission :: !EmptyElementType
    } deriving (Show)
type ResetSnapshotAttributeResponse = ResetSnapshotAttributeResponseType

data ResetSnapshotAttributeResponseType = ResetSnapshotAttributeResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

type DescribeSnapshotAttribute = DescribeSnapshotAttributeType

data DescribeSnapshotAttributeType = DescribeSnapshotAttributeType
    { SnapshotId                      :: !String
    , DescribeSnapshotAttributesGroup :: !DescribeSnapshotAttributesGroup
    } deriving (Show)

data DescribeSnapshotAttributesGroup = DescribeSnapshotAttributesGroup
    { CreateVolumePermission :: !EmptyElementType
    , ProductCodes           :: !EmptyElementType
    } deriving (Show)
type DescribeSnapshotAttributeResponse = DescribeSnapshotAttributeResponseType

data DescribeSnapshotAttributeResponseType = DescribeSnapshotAttributeResponseType
    { RequestId              :: !String
    , SnapshotId             :: !String
  -- <xs:choice>
    , CreateVolumePermission :: !(Maybe CreateVolumePermissionListType)
    , ProductCodes           :: !(Maybe ProductCodesSetType)
  -- </xs:choice>
    } deriving (Show)

type BundleInstance = BundleInstanceType

data BundleInstanceType = BundleInstanceType
    { InstanceId :: !String
    , Storage    :: !BundleInstanceTaskStorageType
    } deriving (Show)

data BundleInstanceTaskStorageType = BundleInstanceTaskStorageType
    { S3 :: !BundleInstanceS3StorageType
    } deriving (Show)

data BundleInstanceS3StorageType = BundleInstanceS3StorageType
    { Bucket                :: !String
    , Prefix                :: !String
    , AwsAccessKeyId        :: !(Maybe String)
    , UploadPolicy          :: !(Maybe String)
    , UploadPolicySignature :: !(Maybe String)
    } deriving (Show)

type BundleInstanceResponse = BundleInstanceResponseType

data BundleInstanceResponseType = BundleInstanceResponseType
    { RequestId          :: !String
    , BundleInstanceTask :: !BundleInstanceTaskType
    } deriving (Show)

data BundleInstanceTaskType = BundleInstanceTaskType
    { InstanceId :: !String
    , BundleId   :: !String
    , State      :: !String
    , StartTime  :: !DateTime
    , UpdateTime :: !DateTime
    , Storage    :: !BundleInstanceTaskStorageType
    , Progress   :: !(Maybe String)
    , Error      :: !(Maybe BundleInstanceTaskErrorType)
    } deriving (Show)

data BundleInstanceTaskErrorType = BundleInstanceTaskErrorType
    { Code    :: !String
    , Message :: !String
    } deriving (Show)

type DescribeBundleTasks = DescribeBundleTasksType

data DescribeBundleTasksType = DescribeBundleTasksType
    { BundlesSet :: !DescribeBundleTasksInfoType
    , FilterSet  :: !(Maybe FilterSetType)
    } deriving (Show)

data DescribeBundleTasksInfoType = DescribeBundleTasksInfoType
    { Item :: ![DescribeBundleTasksItemType]
    } deriving (Show)

data DescribeBundleTasksItemType = DescribeBundleTasksItemType
    { BundleId :: !String
    } deriving (Show)

type DescribeBundleTasksResponse = DescribeBundleTasksResponseType

data DescribeBundleTasksResponseType = DescribeBundleTasksResponseType
    { RequestId              :: !String
    , BundleInstanceTasksSet :: !BundleInstanceTasksSetType
    } deriving (Show)

data BundleInstanceTasksSetType = BundleInstanceTasksSetType
    { Item :: ![BundleInstanceTaskType]
    } deriving (Show)

type CancelBundleTask = CancelBundleTaskType

data CancelBundleTaskType = CancelBundleTaskType
    { BundleId :: !String
    } deriving (Show)

type CancelBundleTaskResponse = CancelBundleTaskResponseType

data CancelBundleTaskResponseType = CancelBundleTaskResponseType
    { RequestId          :: !String
    , BundleInstanceTask :: !BundleInstanceTaskType
    } deriving (Show)

type CopyImage = CopyImageType

data CopyImageType = CopyImageType
    { SourceRegion  :: !String
    , SourceImageId :: !String
    , Name          :: !String
    , Description   :: !(Maybe String)
    , ClientToken   :: !(Maybe String)
    } deriving (Show)

type CopyImageResponse = CopyImageResponseType

data CopyImageResponseType = CopyImageResponseType
    { RequestId :: !String
    , ImageId   :: !String
    } deriving (Show)

type DescribeRegions = DescribeRegionsType

data DescribeRegionsType = DescribeRegionsType
    { RegionSet :: !DescribeRegionsSetType
    , FilterSet :: !(Maybe FilterSetType)
    } deriving (Show)

data DescribeRegionsSetType = DescribeRegionsSetType
    { Item :: ![DescribeRegionsSetItemType]
    } deriving (Show)

data DescribeRegionsSetItemType = DescribeRegionsSetItemType
    { RegionName :: !String
    } deriving (Show)

type DescribeRegionsResponse = DescribeRegionsResponseType

data DescribeRegionsResponseType = DescribeRegionsResponseType
    { RequestId  :: !String
    , RegionInfo :: !RegionSetType
    } deriving (Show)

data RegionSetType = RegionSetType
    { Item :: ![RegionItemType]
    } deriving (Show)

data RegionItemType = RegionItemType
    { RegionName     :: !String
    , RegionEndpoint :: !String
    } deriving (Show)

type DescribeReservedInstancesOfferings = DescribeReservedInstancesOfferingsType

data DescribeReservedInstancesOfferingsType = DescribeReservedInstancesOfferingsType
    { ReservedInstancesOfferingsSet :: !(Maybe DescribeReservedInstancesOfferingsSetType)
    , InstanceType                  :: !(Maybe String)
    , AvailabilityZone              :: !(Maybe String)
    , ProductDescription            :: !(Maybe String)
    , FilterSet                     :: !(Maybe FilterSetType)
    , InstanceTenancy               :: !(Maybe String)
    , OfferingType                  :: !(Maybe String)
    , IncludeMarketplace            :: !(Maybe Boolean)
    , MinDuration                   :: !(Maybe Long)
    , MaxDuration                   :: !(Maybe Long)
    , MaxInstanceCount              :: !(Maybe Int)
    , NextToken                     :: !(Maybe String)
    , MaxResults                    :: !(Maybe Int)
    } deriving (Show)

data DescribeReservedInstancesOfferingsSetType = DescribeReservedInstancesOfferingsSetType
    { Item :: ![DescribeReservedInstancesOfferingsSetItemType]
    } deriving (Show)

data DescribeReservedInstancesOfferingsSetItemType = DescribeReservedInstancesOfferingsSetItemType
    { ReservedInstancesOfferingId :: !String
    } deriving (Show)

type DescribeReservedInstancesOfferingsResponse = DescribeReservedInstancesOfferingsResponseType

data DescribeReservedInstancesOfferingsResponseType = DescribeReservedInstancesOfferingsResponseType
    { RequestId                     :: !String
    , ReservedInstancesOfferingsSet :: !DescribeReservedInstancesOfferingsResponseSetType
    , NextToken                     :: !(Maybe String)
    } deriving (Show)

data DescribeReservedInstancesOfferingsResponseSetType = DescribeReservedInstancesOfferingsResponseSetType
    { Item :: ![DescribeReservedInstancesOfferingsResponseSetItemType]
    } deriving (Show)

data DescribeReservedInstancesOfferingsResponseSetItemType = DescribeReservedInstancesOfferingsResponseSetItemType
    { ReservedInstancesOfferingId :: !String
    , InstanceType                :: !String
    , AvailabilityZone            :: !String
    , Duration                    :: !Long
    , FixedPrice                  :: !Double
    , UsagePrice                  :: !Double
    , ProductDescription          :: !String
    , InstanceTenancy             :: !String
    , CurrencyCode                :: !String
    , OfferingType                :: !String
    , RecurringCharges            :: !RecurringChargesSetType
    , Marketplace                 :: !(Maybe Boolean)
    , PricingDetailsSet           :: !(Maybe PricingDetailsSetType)
    } deriving (Show)

data RecurringChargesSetType = RecurringChargesSetType
    { Item :: ![RecurringChargesSetItemType]
    } deriving (Show)

data RecurringChargesSetItemType = RecurringChargesSetItemType
    { Frequency :: !String
    , Amount    :: !Double
    } deriving (Show)

data PricingDetailsSetType = PricingDetailsSetType
    { Item :: ![PricingDetailsSetItemType]
    } deriving (Show)

data PricingDetailsSetItemType = PricingDetailsSetItemType
    { Price :: !Double
    , Count :: !Int
    } deriving (Show)

type PurchaseReservedInstancesOffering = PurchaseReservedInstancesOfferingType

data PurchaseReservedInstancesOfferingType = PurchaseReservedInstancesOfferingType
    { ReservedInstancesOfferingId :: !String
    , InstanceCount               :: !Int
    , LimitPrice                  :: !(Maybe ReservedInstanceLimitPriceType)
    } deriving (Show)

data ReservedInstanceLimitPriceType = ReservedInstanceLimitPriceType
    { Amount       :: !Double
    , CurrencyCode :: !(Maybe String)
    } deriving (Show)

type PurchaseReservedInstancesOfferingResponse = PurchaseReservedInstancesOfferingResponseType

data PurchaseReservedInstancesOfferingResponseType = PurchaseReservedInstancesOfferingResponseType
    { RequestId           :: !String
    , ReservedInstancesId :: !String
    } deriving (Show)

type DescribeReservedInstances = DescribeReservedInstancesType

data DescribeReservedInstancesType = DescribeReservedInstancesType
    { ReservedInstancesSet :: !(Maybe DescribeReservedInstancesSetType)
    , FilterSet            :: !(Maybe FilterSetType)
    , OfferingType         :: !(Maybe String)
    } deriving (Show)

data DescribeReservedInstancesSetType = DescribeReservedInstancesSetType
    { Item :: ![DescribeReservedInstancesSetItemType]
    } deriving (Show)

data DescribeReservedInstancesSetItemType = DescribeReservedInstancesSetItemType
    { ReservedInstancesId :: !String
    } deriving (Show)

type DescribeReservedInstancesResponse = DescribeReservedInstancesResponseType

data DescribeReservedInstancesResponseType = DescribeReservedInstancesResponseType
    { RequestId            :: !String
    , ReservedInstancesSet :: !DescribeReservedInstancesResponseSetType
    } deriving (Show)

data DescribeReservedInstancesResponseSetType = DescribeReservedInstancesResponseSetType
    { Item :: ![DescribeReservedInstancesResponseSetItemType]
    } deriving (Show)

data DescribeReservedInstancesResponseSetItemType = DescribeReservedInstancesResponseSetItemType
    { ReservedInstancesId :: !String
    , InstanceType        :: !String
    , AvailabilityZone    :: !String
    , Start               :: !DateTime
    , Duration            :: !Long
    , FixedPrice          :: !Double
    , UsagePrice          :: !Double
    , InstanceCount       :: !Integer
    , ProductDescription  :: !String
    , State               :: !String
    , TagSet              :: !(Maybe ResourceTagSetType)
    , InstanceTenancy     :: !String
    , CurrencyCode        :: !String
    , OfferingType        :: !String
    , RecurringCharges    :: !(Maybe RecurringChargesSetType)
    } deriving (Show)

type CreateReservedInstancesListing = CreateReservedInstancesListingType

data CreateReservedInstancesListingType = CreateReservedInstancesListingType
    { ReservedInstancesId :: !String
    , InstanceCount       :: !(Maybe Int)
    , PriceSchedules      :: !PriceScheduleRequestSetType
    , ClientToken         :: !String
    } deriving (Show)

data PriceScheduleRequestSetType = PriceScheduleRequestSetType
    { Item :: ![PriceScheduleRequestSetItemType]
    } deriving (Show)

data PriceScheduleRequestSetItemType = PriceScheduleRequestSetItemType
    { Term         :: !Long
    , Price        :: !Double
    , CurrencyCode :: !(Maybe String)
    } deriving (Show)

type CreateReservedInstancesListingResponse = CreateReservedInstancesListingResponseType

data CreateReservedInstancesListingResponseType = CreateReservedInstancesListingResponseType
    { RequestId                    :: !String
    , ReservedInstancesListingsSet :: !DescribeReservedInstancesListingsResponseSetType
    } deriving (Show)

type CancelReservedInstancesListing = CancelReservedInstancesListingType

data CancelReservedInstancesListingType = CancelReservedInstancesListingType
    { ReservedInstancesListingId :: !String
    } deriving (Show)

type CancelReservedInstancesListingResponse = CancelReservedInstancesListingResponseType

data CancelReservedInstancesListingResponseType = CancelReservedInstancesListingResponseType
    { RequestId                    :: !String
    , ReservedInstancesListingsSet :: !DescribeReservedInstancesListingsResponseSetType
    } deriving (Show)

type DescribeReservedInstancesListings = DescribeReservedInstancesListingsType

data DescribeReservedInstancesListingsType = DescribeReservedInstancesListingsType
    { ReservedInstancesListingSet :: !(Maybe DescribeReservedInstancesListingSetType)
    , ReservedInstancesSet        :: !(Maybe DescribeReservedInstancesSetType)
    , FilterSet                   :: !(Maybe FilterSetType)
    } deriving (Show)

data DescribeReservedInstancesListingSetType = DescribeReservedInstancesListingSetType
    { Item :: ![DescribeReservedInstancesListingSetItemType]
    } deriving (Show)

data DescribeReservedInstancesListingSetItemType = DescribeReservedInstancesListingSetItemType
    { ReservedInstancesListingId :: !String
    } deriving (Show)

type DescribeReservedInstancesListingsResponse = DescribeReservedInstancesListingsResponseType

data DescribeReservedInstancesListingsResponseType = DescribeReservedInstancesListingsResponseType
    { RequestId                    :: !String
    , ReservedInstancesListingsSet :: !DescribeReservedInstancesListingsResponseSetType
    } deriving (Show)

data DescribeReservedInstancesListingsResponseSetType = DescribeReservedInstancesListingsResponseSetType
    { Item :: ![DescribeReservedInstancesListingsResponseSetItemType]
    } deriving (Show)

data DescribeReservedInstancesListingsResponseSetItemType = DescribeReservedInstancesListingsResponseSetItemType
    { ReservedInstancesListingId :: !String
    , ReservedInstancesId        :: !String
    , CreateDate                 :: !DateTime
    , UpdateDate                 :: !DateTime
    , Status                     :: !String
    , StatusMessage              :: !String
    , InstanceCounts             :: !InstanceCountsSetType
    , PriceSchedules             :: !PriceScheduleSetType
    , TagSet                     :: !(Maybe ResourceTagSetType)
    , ClientToken                :: !(Maybe String)
    } deriving (Show)

data InstanceCountsSetType = InstanceCountsSetType
    { Item :: ![InstanceCountsSetItemType]
    } deriving (Show)

data InstanceCountsSetItemType = InstanceCountsSetItemType
    { State         :: !String
    , InstanceCount :: !Int
    } deriving (Show)

data PriceScheduleSetType = PriceScheduleSetType
    { Item :: ![PriceScheduleSetItemType]
    } deriving (Show)

data PriceScheduleSetItemType = PriceScheduleSetItemType
    { Term         :: !Long
    , Price        :: !Double
    , CurrencyCode :: !(Maybe String)
    , Active       :: !Boolean
    } deriving (Show)

type MonitorInstances = MonitorInstancesType

type UnmonitorInstances = MonitorInstancesType

data MonitorInstancesType = MonitorInstancesType
    { InstancesSet :: !MonitorInstancesSetType
    } deriving (Show)

data MonitorInstancesSetType = MonitorInstancesSetType
    {
  Item :: !(NonEmpty MonitorInstancesSetItemType)
    } deriving (Show)

data MonitorInstancesSetItemType = MonitorInstancesSetItemType
    { InstanceId :: !String
    } deriving (Show)

type MonitorInstancesResponse = MonitorInstancesResponseType

type UnmonitorInstancesResponse = MonitorInstancesResponseType

data MonitorInstancesResponseType = MonitorInstancesResponseType
    { RequestId    :: !String
    , InstancesSet :: !MonitorInstancesResponseSetType
    } deriving (Show)

data MonitorInstancesResponseSetType = MonitorInstancesResponseSetType
    {
  Item :: !(NonEmpty MonitorInstancesResponseSetItemType)
    } deriving (Show)

data MonitorInstancesResponseSetItemType = MonitorInstancesResponseSetItemType
    { InstanceId :: !String
    , Monitoring :: !InstanceMonitoringStateType
    } deriving (Show)

data InstanceMonitoringStateType = InstanceMonitoringStateType
    { State :: !String
    } deriving (Show)

data AttachmentType = AttachmentType
    { VpcId :: !String
    , State :: !String
    } deriving (Show)

data AttachmentSetType = AttachmentSetType
    { Item :: ![AttachmentType]
    } deriving (Show)

data VpnGatewayType = VpnGatewayType
    { VpnGatewayId     :: !String
    , State            :: !String
    , Type             :: !String
    , AvailabilityZone :: !(Maybe String)
    , Attachments      :: !AttachmentSetType
    , TagSet           :: !(Maybe ResourceTagSetType)
    } deriving (Show)

data CustomerGatewayType = CustomerGatewayType
    { CustomerGatewayId :: !String
    , State             :: !String
    , Type              :: !String
    , IpAddress         :: !String
    , BgpAsn            :: !(Maybe Int)
    , TagSet            :: !(Maybe ResourceTagSetType)
    } deriving (Show)

data VpnConnectionType = VpnConnectionType
    { VpnConnectionId              :: !String
    , State                        :: !String
    , CustomerGatewayConfiguration :: !(Maybe String)
    , Type                         :: !(Maybe String)
    , CustomerGatewayId            :: !String
    , VpnGatewayId                 :: !String
    , TagSet                       :: !(Maybe ResourceTagSetType)
    , VgwTelemetry                 :: !(Maybe VgwTelemetryType)
    , Options                      :: !(Maybe VpnConnectionOptionsResponseType)
    , Routes                       :: !(Maybe VpnStaticRoutesSetType)
    } deriving (Show)

data VpnConnectionOptionsResponseType = VpnConnectionOptionsResponseType
    { StaticRoutesOnly :: !(Maybe Boolean)
    } deriving (Show)

data VpnStaticRoutesSetType = VpnStaticRoutesSetType
    { Item :: ![VpnStaticRouteType]
    } deriving (Show)

data VpnStaticRouteType = VpnStaticRouteType
    { DestinationCidrBlock :: !String
    , Source               :: !String
    , State                :: !String
    } deriving (Show)

data VgwTelemetryType = VgwTelemetryType
    { Item :: ![VpnTunnelTelemetryType]
    } deriving (Show)

data VpnTunnelTelemetryType = VpnTunnelTelemetryType
    { OutsideIpAddress   :: !String
    , Status             :: !String
    , LastStatusChange   :: !DateTime
    , StatusMessage      :: !(Maybe String)
    , AcceptedRouteCount :: !Int
    } deriving (Show)

data VpcType = VpcType
    { VpcId           :: !String
    , State           :: !(Maybe String)
    , CidrBlock       :: !(Maybe String)
    , DhcpOptionsId   :: !(Maybe String)
    , TagSet          :: !(Maybe ResourceTagSetType)
    , InstanceTenancy :: !(Maybe String)
    , IsDefault       :: !(Maybe Boolean)
    } deriving (Show)

data SubnetType = SubnetType
    { SubnetId                :: !String
    , State                   :: !(Maybe String)
    , VpcId                   :: !(Maybe String)
    , CidrBlock               :: !(Maybe String)
    , AvailableIpAddressCount :: !(Maybe Int)
    , AvailabilityZone        :: !(Maybe String)
    , DefaultForAz            :: !(Maybe Boolean)
    , MapPublicIpOnLaunch     :: !(Maybe Boolean)
    , TagSet                  :: !(Maybe ResourceTagSetType)
    } deriving (Show)

data CustomerGatewaySetType = CustomerGatewaySetType
    { Item :: ![CustomerGatewayType]
    } deriving (Show)

data VpnGatewaySetType = VpnGatewaySetType
    { Item :: ![VpnGatewayType]
    } deriving (Show)

data VpnConnectionSetType = VpnConnectionSetType
    { Item :: ![VpnConnectionType]
    } deriving (Show)

data VpcSetType = VpcSetType
    { Item :: ![VpcType]
    } deriving (Show)

data SubnetSetType = SubnetSetType
    { Item :: ![SubnetType]
    } deriving (Show)

data CustomerGatewayIdSetItemType = CustomerGatewayIdSetItemType
    { CustomerGatewayId :: !String
    } deriving (Show)

data CustomerGatewayIdSetType = CustomerGatewayIdSetType
    { Item :: ![CustomerGatewayIdSetItemType]
    } deriving (Show)

data VpnGatewayIdSetItemType = VpnGatewayIdSetItemType
    { VpnGatewayId :: !String
    } deriving (Show)

data VpnGatewayIdSetType = VpnGatewayIdSetType
    { Item :: ![VpnGatewayIdSetItemType]
    } deriving (Show)

data VpnConnectionIdSetItemType = VpnConnectionIdSetItemType
    { VpnConnectionId :: !String
    } deriving (Show)

data VpnConnectionIdSetType = VpnConnectionIdSetType
    { Item :: ![VpnConnectionIdSetItemType]
    } deriving (Show)

data VpcIdSetItemType = VpcIdSetItemType
    { VpcId :: !String
    } deriving (Show)

data VpcIdSetType = VpcIdSetType
    { Item :: ![VpcIdSetItemType]
    } deriving (Show)

data SubnetIdSetItemType = SubnetIdSetItemType
    { SubnetId :: !String
    } deriving (Show)

data SubnetIdSetType = SubnetIdSetType
    { Item :: ![SubnetIdSetItemType]
    } deriving (Show)

data DhcpOptionsIdSetItemType = DhcpOptionsIdSetItemType
    { DhcpOptionsId :: !String
    } deriving (Show)

data DhcpOptionsIdSetType = DhcpOptionsIdSetType
    { Item :: ![DhcpOptionsIdSetItemType]
    } deriving (Show)

data DhcpConfigurationItemSetType = DhcpConfigurationItemSetType
    { Item :: ![DhcpConfigurationItemType]
    } deriving (Show)

data DhcpOptionsSetType = DhcpOptionsSetType
    { Item :: ![DhcpOptionsType]
    } deriving (Show)

data DhcpConfigurationItemType = DhcpConfigurationItemType
    { Key      :: !String
    , ValueSet :: !DhcpValueSetType
    } deriving (Show)

data DhcpOptionsType = DhcpOptionsType
    { DhcpOptionsId        :: !String
    , DhcpConfigurationSet :: !DhcpConfigurationItemSetType
    , TagSet               :: !(Maybe ResourceTagSetType)
    } deriving (Show)

data DhcpValueType = DhcpValueType
    { Value :: !String
    } deriving (Show)

data DhcpValueSetType = DhcpValueSetType
    { Item :: ![DhcpValueType]
    } deriving (Show)

data FilterType = FilterType
    { Name     :: !String
    , ValueSet :: !ValueSetType
    } deriving (Show)

data FilterSetType = FilterSetType
    { Item :: ![FilterType]
    } deriving (Show)

data ValueType = ValueType
    { Value :: !String
    } deriving (Show)

data ValueSetType = ValueSetType
    { Item :: ![ValueType]
    } deriving (Show)

type CreateCustomerGateway = CreateCustomerGatewayType

type CreateCustomerGatewayResponse = CreateCustomerGatewayResponseType

type DeleteCustomerGateway = DeleteCustomerGatewayType

type DeleteCustomerGatewayResponse = DeleteCustomerGatewayResponseType

type DescribeCustomerGateways = DescribeCustomerGatewaysType

type DescribeCustomerGatewaysResponse = DescribeCustomerGatewaysResponseType

type CreateVpnGateway = CreateVpnGatewayType

type CreateVpnGatewayResponse = CreateVpnGatewayResponseType

type DeleteVpnGateway = DeleteVpnGatewayType

type DeleteVpnGatewayResponse = DeleteVpnGatewayResponseType

type DescribeVpnGateways = DescribeVpnGatewaysType

type DescribeVpnGatewaysResponse = DescribeVpnGatewaysResponseType

type CreateVpnConnection = CreateVpnConnectionType

type CreateVpnConnectionResponse = CreateVpnConnectionResponseType

type CreateVpnConnectionRoute = CreateVpnConnectionRouteType

type CreateVpnConnectionRouteResponse = CreateVpnConnectionRouteResponseType

type DeleteVpnConnectionRoute = DeleteVpnConnectionRouteType

type DeleteVpnConnectionRouteResponse = DeleteVpnConnectionRouteResponseType

type DeleteVpnConnection = DeleteVpnConnectionType

type DeleteVpnConnectionResponse = DeleteVpnConnectionResponseType

type DescribeVpnConnections = DescribeVpnConnectionsType

type DescribeVpnConnectionsResponse = DescribeVpnConnectionsResponseType

type AttachVpnGateway = AttachVpnGatewayType

type AttachVpnGatewayResponse = AttachVpnGatewayResponseType

type DetachVpnGateway = DetachVpnGatewayType

type DetachVpnGatewayResponse = DetachVpnGatewayResponseType

type CreateVpc = CreateVpcType

type CreateVpcResponse = CreateVpcResponseType

type DescribeVpcs = DescribeVpcsType

type DescribeVpcsResponse = DescribeVpcsResponseType

type DeleteVpc = DeleteVpcType

type DeleteVpcResponse = DeleteVpcResponseType

type CreateSubnet = CreateSubnetType

type CreateSubnetResponse = CreateSubnetResponseType

type DescribeSubnets = DescribeSubnetsType

type DescribeSubnetsResponse = DescribeSubnetsResponseType

type DeleteSubnet = DeleteSubnetType

type DeleteSubnetResponse = DeleteSubnetResponseType

type DeleteDhcpOptions = DeleteDhcpOptionsType

type DeleteDhcpOptionsResponse = DeleteDhcpOptionsResponseType

type DescribeDhcpOptions = DescribeDhcpOptionsType

type DescribeDhcpOptionsResponse = DescribeDhcpOptionsResponseType

type CreateDhcpOptions = CreateDhcpOptionsType

type CreateDhcpOptionsResponse = CreateDhcpOptionsResponseType

type AssociateDhcpOptions = AssociateDhcpOptionsType

type AssociateDhcpOptionsResponse = AssociateDhcpOptionsResponseType

data CreateCustomerGatewayType = CreateCustomerGatewayType
    { Type      :: !String
    , IpAddress :: !String
    , BgpAsn    :: !(Maybe Int)
    } deriving (Show)

data CreateCustomerGatewayResponseType = CreateCustomerGatewayResponseType
    { RequestId       :: !String
    , CustomerGateway :: !CustomerGatewayType
    } deriving (Show)

data DeleteCustomerGatewayType = DeleteCustomerGatewayType
    { CustomerGatewayId :: !String
    } deriving (Show)

data DeleteCustomerGatewayResponseType = DeleteCustomerGatewayResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

data DescribeCustomerGatewaysType = DescribeCustomerGatewaysType
    { CustomerGatewaySet :: !(Maybe CustomerGatewayIdSetType)
    , FilterSet          :: !(Maybe FilterSetType)
    } deriving (Show)

data DescribeCustomerGatewaysResponseType = DescribeCustomerGatewaysResponseType
    { RequestId          :: !String
    , CustomerGatewaySet :: !CustomerGatewaySetType
    } deriving (Show)

data CreateVpnGatewayType = CreateVpnGatewayType
    { Type             :: !String
    , AvailabilityZone :: !(Maybe String)
    } deriving (Show)

data CreateVpnGatewayResponseType = CreateVpnGatewayResponseType
    { RequestId  :: !String
    , VpnGateway :: !VpnGatewayType
    } deriving (Show)

data DeleteVpnGatewayType = DeleteVpnGatewayType
    { VpnGatewayId :: !String
    } deriving (Show)

data DeleteVpnGatewayResponseType = DeleteVpnGatewayResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

data DescribeVpnGatewaysType = DescribeVpnGatewaysType
    { VpnGatewaySet :: !(Maybe VpnGatewayIdSetType)
    , FilterSet     :: !(Maybe FilterSetType)
    } deriving (Show)

data DescribeVpnGatewaysResponseType = DescribeVpnGatewaysResponseType
    { RequestId     :: !String
    , VpnGatewaySet :: !VpnGatewaySetType
    } deriving (Show)

data CreateVpnConnectionType = CreateVpnConnectionType
    { Type              :: !String
    , CustomerGatewayId :: !String
    , VpnGatewayId      :: !String
    , Options           :: !(Maybe VpnConnectionOptionsRequestType)
    } deriving (Show)

data VpnConnectionOptionsRequestType = VpnConnectionOptionsRequestType
    { StaticRoutesOnly :: !(Maybe Boolean)
    } deriving (Show)

data CreateVpnConnectionResponseType = CreateVpnConnectionResponseType
    { RequestId     :: !String
    , VpnConnection :: !VpnConnectionType
    } deriving (Show)

data CreateVpnConnectionRouteType = CreateVpnConnectionRouteType
    { VpnConnectionId      :: !String
    , DestinationCidrBlock :: !String
    } deriving (Show)

data CreateVpnConnectionRouteResponseType = CreateVpnConnectionRouteResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

data DeleteVpnConnectionRouteType = DeleteVpnConnectionRouteType
    { VpnConnectionId      :: !String
    , DestinationCidrBlock :: !String
    } deriving (Show)

data DeleteVpnConnectionRouteResponseType = DeleteVpnConnectionRouteResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

data DeleteVpnConnectionType = DeleteVpnConnectionType
    { VpnConnectionId :: !String
    } deriving (Show)

data DeleteVpnConnectionResponseType = DeleteVpnConnectionResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

data DescribeVpnConnectionsType = DescribeVpnConnectionsType
    { VpnConnectionSet :: !(Maybe VpnConnectionIdSetType)
    , FilterSet        :: !(Maybe FilterSetType)
    } deriving (Show)

data DescribeVpnConnectionsResponseType = DescribeVpnConnectionsResponseType
    { RequestId        :: !String
    , VpnConnectionSet :: !VpnConnectionSetType
    } deriving (Show)

data AttachVpnGatewayType = AttachVpnGatewayType
    { VpnGatewayId :: !String
    , VpcId        :: !String
    } deriving (Show)

data AttachVpnGatewayResponseType = AttachVpnGatewayResponseType
    { RequestId  :: !String
    , Attachment :: !AttachmentType
    } deriving (Show)

data DetachVpnGatewayType = DetachVpnGatewayType
    { VpnGatewayId :: !String
    , VpcId        :: !String
    } deriving (Show)

data DetachVpnGatewayResponseType = DetachVpnGatewayResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

data CreateVpcType = CreateVpcType
    { CidrBlock       :: !String
    , InstanceTenancy :: !(Maybe String)
    } deriving (Show)

data CreateVpcResponseType = CreateVpcResponseType
    { RequestId :: !String
    , Vpc       :: !VpcType
    } deriving (Show)

data DescribeVpcsType = DescribeVpcsType
    { VpcSet    :: !(Maybe VpcIdSetType)
    , FilterSet :: !(Maybe FilterSetType)
    } deriving (Show)

data DescribeVpcsResponseType = DescribeVpcsResponseType
    { RequestId :: !String
    , VpcSet    :: !VpcSetType
    } deriving (Show)

data DeleteVpcType = DeleteVpcType
    { VpcId :: !String
    } deriving (Show)

data DeleteVpcResponseType = DeleteVpcResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

data CreateSubnetType = CreateSubnetType
    { VpcId            :: !String
    , CidrBlock        :: !String
    , AvailabilityZone :: !(Maybe String)
    } deriving (Show)

data CreateSubnetResponseType = CreateSubnetResponseType
    { RequestId :: !String
    , Subnet    :: !SubnetType
    } deriving (Show)

data DescribeSubnetsType = DescribeSubnetsType
    { SubnetSet :: !(Maybe SubnetIdSetType)
    , FilterSet :: !(Maybe FilterSetType)
    } deriving (Show)

data DescribeSubnetsResponseType = DescribeSubnetsResponseType
    { RequestId :: !String
    , SubnetSet :: !SubnetSetType
    } deriving (Show)

data DeleteSubnetType = DeleteSubnetType
    { SubnetId :: !String
    } deriving (Show)

data DeleteSubnetResponseType = DeleteSubnetResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

data DeleteDhcpOptionsType = DeleteDhcpOptionsType
    { DhcpOptionsId :: !String
    } deriving (Show)

data DeleteDhcpOptionsResponseType = DeleteDhcpOptionsResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

data DescribeDhcpOptionsType = DescribeDhcpOptionsType
    { DhcpOptionsSet :: !(Maybe DhcpOptionsIdSetType)
    , FilterSet      :: !(Maybe FilterSetType)
    } deriving (Show)

data DescribeDhcpOptionsResponseType = DescribeDhcpOptionsResponseType
    { RequestId      :: !String
    , DhcpOptionsSet :: !DhcpOptionsSetType
    } deriving (Show)

data CreateDhcpOptionsType = CreateDhcpOptionsType
    { DhcpConfigurationSet :: !DhcpConfigurationItemSetType
    } deriving (Show)

data CreateDhcpOptionsResponseType = CreateDhcpOptionsResponseType
    { RequestId   :: !String
    , DhcpOptions :: !DhcpOptionsType
    } deriving (Show)

data AssociateDhcpOptionsType = AssociateDhcpOptionsType
    { DhcpOptionsId :: !String
    , VpcId         :: !String
    } deriving (Show)

data AssociateDhcpOptionsResponseType = AssociateDhcpOptionsResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

type RequestSpotInstances = RequestSpotInstancesType

type RequestSpotInstancesResponse = RequestSpotInstancesResponseType

type DescribeSpotInstanceRequests = DescribeSpotInstanceRequestsType

type DescribeSpotInstanceRequestsResponse = DescribeSpotInstanceRequestsResponseType

type CancelSpotInstanceRequests = CancelSpotInstanceRequestsType

type CancelSpotInstanceRequestsResponse = CancelSpotInstanceRequestsResponseType

type DescribeSpotPriceHistory = DescribeSpotPriceHistoryType

type DescribeSpotPriceHistoryResponse = DescribeSpotPriceHistoryResponseType

type CreateSpotDatafeedSubscription = CreateSpotDatafeedSubscriptionType

type CreateSpotDatafeedSubscriptionResponse = CreateSpotDatafeedSubscriptionResponseType

type DescribeSpotDatafeedSubscription = DescribeSpotDatafeedSubscriptionType

type DescribeSpotDatafeedSubscriptionResponse = DescribeSpotDatafeedSubscriptionResponseType

type DeleteSpotDatafeedSubscription = DeleteSpotDatafeedSubscriptionType

type DeleteSpotDatafeedSubscriptionResponse = DeleteSpotDatafeedSubscriptionResponseType

data RequestSpotInstancesType = RequestSpotInstancesType
    { SpotPrice             :: !String
    , InstanceCount         :: !(Maybe Integer)
    , Type                  :: !(Maybe String)
    , ValidFrom             :: !(Maybe DateTime)
    , ValidUntil            :: !(Maybe DateTime)
    , LaunchGroup           :: !(Maybe String)
    , AvailabilityZoneGroup :: !(Maybe String)
    , LaunchSpecification   :: !LaunchSpecificationRequestType
    } deriving (Show)

data LaunchSpecificationRequestType = LaunchSpecificationRequestType
    { ImageId             :: !String
    , KeyName             :: !(Maybe String)
    , GroupSet            :: !GroupSetType
    , UserData            :: !(Maybe UserDataType)
    , AddressingType      :: !(Maybe String)
    , InstanceType        :: !String
    , Placement           :: !(Maybe SpotPlacementRequestType)
    , KernelId            :: !(Maybe String)
    , RamdiskId           :: !(Maybe String)
    , BlockDeviceMapping  :: !(Maybe BlockDeviceMappingType)
    , Monitoring          :: !(Maybe MonitoringInstanceType)
    , SubnetId            :: !(Maybe String)
    , NetworkInterfaceSet :: !(Maybe InstanceNetworkInterfaceSetRequestType)
    , IamInstanceProfile  :: !(Maybe IamInstanceProfileRequestType)
    , EbsOptimized        :: !(Maybe Boolean)
    } deriving (Show)

data LaunchSpecificationResponseType = LaunchSpecificationResponseType
    { ImageId             :: !String
    , KeyName             :: !(Maybe String)
    , GroupSet            :: !GroupSetType
    , AddressingType      :: !(Maybe String)
    , InstanceType        :: !String
    , Placement           :: !(Maybe SpotPlacementRequestType)
    , KernelId            :: !(Maybe String)
    , RamdiskId           :: !(Maybe String)
    , BlockDeviceMapping  :: !(Maybe BlockDeviceMappingType)
    , Monitoring          :: !(Maybe MonitoringInstanceType)
    , SubnetId            :: !(Maybe String)
    , NetworkInterfaceSet :: !(Maybe InstanceNetworkInterfaceSetRequestType)
    , IamInstanceProfile  :: !(Maybe IamInstanceProfileRequestType)
    , EbsOptimized        :: !(Maybe Boolean)
    } deriving (Show)

data SpotInstanceRequestSetItemType = SpotInstanceRequestSetItemType
    { SpotInstanceRequestId    :: !String
    , SpotPrice                :: !String
    , Type                     :: !String
    , State                    :: !String
    , Fault                    :: !(Maybe SpotInstanceStateFaultType)
    , Status                   :: !(Maybe SpotInstanceStatusMessageType)
    , ValidFrom                :: !(Maybe DateTime)
    , ValidUntil               :: !(Maybe DateTime)
    , LaunchGroup              :: !(Maybe String)
    , AvailabilityZoneGroup    :: !(Maybe String)
    , LaunchSpecification      :: !(Maybe LaunchSpecificationResponseType)
    , InstanceId               :: !(Maybe String)
    , CreateTime               :: !(Maybe DateTime)
    , ProductDescription       :: !(Maybe String)
    , TagSet                   :: !(Maybe ResourceTagSetType)
    , LaunchedAvailabilityZone :: !(Maybe String)
    } deriving (Show)

data SpotInstanceStateFaultType = SpotInstanceStateFaultType
    { Code    :: !String
    , Message :: !String
    } deriving (Show)

data SpotInstanceStatusMessageType = SpotInstanceStatusMessageType
    { Code       :: !(Maybe String)
    , UpdateTime :: !(Maybe DateTime)
    , Message    :: !(Maybe String)
    } deriving (Show)

data SpotInstanceRequestSetType = SpotInstanceRequestSetType
    { Item :: ![SpotInstanceRequestSetItemType]
    } deriving (Show)

data RequestSpotInstancesResponseType = RequestSpotInstancesResponseType
    { RequestId              :: !String
    , SpotInstanceRequestSet :: !SpotInstanceRequestSetType
    } deriving (Show)

data DescribeSpotInstanceRequestsType = DescribeSpotInstanceRequestsType
    { SpotInstanceRequestIdSet :: !SpotInstanceRequestIdSetType
    , FilterSet                :: !(Maybe FilterSetType)
    } deriving (Show)

data SpotInstanceRequestIdSetType = SpotInstanceRequestIdSetType
    { Item :: ![SpotInstanceRequestIdSetItemType]
    } deriving (Show)

data SpotInstanceRequestIdSetItemType = SpotInstanceRequestIdSetItemType
    { SpotInstanceRequestId :: !String
    } deriving (Show)

data DescribeSpotInstanceRequestsResponseType = DescribeSpotInstanceRequestsResponseType
    { RequestId              :: !String
    , SpotInstanceRequestSet :: !SpotInstanceRequestSetType
    } deriving (Show)

data CancelSpotInstanceRequestsType = CancelSpotInstanceRequestsType
    { SpotInstanceRequestIdSet :: !SpotInstanceRequestIdSetType
    } deriving (Show)

data CancelSpotInstanceRequestsResponseType = CancelSpotInstanceRequestsResponseType
    { RequestId              :: !String
    , SpotInstanceRequestSet :: !CancelSpotInstanceRequestsResponseSetType
    } deriving (Show)

data CancelSpotInstanceRequestsResponseSetType = CancelSpotInstanceRequestsResponseSetType
    {
  Item :: !(NonEmpty CancelSpotInstanceRequestsResponseSetItemType)
    } deriving (Show)

data CancelSpotInstanceRequestsResponseSetItemType = CancelSpotInstanceRequestsResponseSetItemType
    { SpotInstanceRequestId :: !String
    , State                 :: !String
    } deriving (Show)

data DescribeSpotPriceHistoryType = DescribeSpotPriceHistoryType
    { StartTime             :: !(Maybe DateTime)
    , EndTime               :: !(Maybe DateTime)
    , InstanceTypeSet       :: !(Maybe InstanceTypeSetType)
    , ProductDescriptionSet :: !(Maybe ProductDescriptionSetType)
    , FilterSet             :: !(Maybe FilterSetType)
    , AvailabilityZone      :: !(Maybe String)
    , MaxResults            :: !(Maybe Integer)
    , NextToken             :: !(Maybe String)
    } deriving (Show)

data InstanceTypeSetType = InstanceTypeSetType
    {
  Item :: !(NonEmpty InstanceTypeSetItemType)
    } deriving (Show)

data InstanceTypeSetItemType = InstanceTypeSetItemType
    { InstanceType :: !String
    } deriving (Show)

data ProductDescriptionSetType = ProductDescriptionSetType
    {
  Item :: !(NonEmpty ProductDescriptionSetItemType)
    } deriving (Show)

data ProductDescriptionSetItemType = ProductDescriptionSetItemType
    { ProductDescription :: !String
    } deriving (Show)

data DescribeSpotPriceHistoryResponseType = DescribeSpotPriceHistoryResponseType
    { RequestId           :: !String
    , SpotPriceHistorySet :: !SpotPriceHistorySetType
    , NextToken           :: !(Maybe String)
    } deriving (Show)

data SpotPriceHistorySetType = SpotPriceHistorySetType
    { Item :: ![SpotPriceHistorySetItemType]
    } deriving (Show)

data SpotPriceHistorySetItemType = SpotPriceHistorySetItemType
    { InstanceType       :: !String
    , ProductDescription :: !String
    , SpotPrice          :: !String
    , Timestamp          :: !DateTime
    , AvailabilityZone   :: !(Maybe String)
    } deriving (Show)

data SpotDatafeedSubscriptionType = SpotDatafeedSubscriptionType
    { OwnerId :: !String
    , Bucket  :: !String
    , Prefix  :: !String
    , State   :: !String
    , Fault   :: !(Maybe SpotInstanceStateFaultType)
    } deriving (Show)

data CreateSpotDatafeedSubscriptionType = CreateSpotDatafeedSubscriptionType
    { Bucket :: !String
    , Prefix :: !String
    } deriving (Show)

data CreateSpotDatafeedSubscriptionResponseType = CreateSpotDatafeedSubscriptionResponseType
    { RequestId                :: !String
    , SpotDatafeedSubscription :: !SpotDatafeedSubscriptionType
    } deriving (Show)

data DescribeSpotDatafeedSubscriptionType = DescribeSpotDatafeedSubscriptionType

data DescribeSpotDatafeedSubscriptionResponseType = DescribeSpotDatafeedSubscriptionResponseType
    { RequestId                :: !String
    , SpotDatafeedSubscription :: !SpotDatafeedSubscriptionType
    } deriving (Show)

data DescribeSpotDatafeedSubscriptionType = DescribeSpotDatafeedSubscriptionType

data DeleteSpotDatafeedSubscriptionResponseType = DeleteSpotDatafeedSubscriptionResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

type DescribeLicenses = DescribeLicensesType

type DescribeLicensesResponse = DescribeLicensesResponseType

type ActivateLicense = ActivateLicenseType

type ActivateLicenseResponse = ActivateLicenseResponseType

type DeactivateLicense = DeactivateLicenseType

type DeactivateLicenseResponse = DeactivateLicenseResponseType

data DescribeLicensesType = DescribeLicensesType
    { LicenseIdSet :: !(Maybe LicenseIdSetType)
    , FilterSet    :: !(Maybe FilterSetType)
    } deriving (Show)

data LicenseIdSetType = LicenseIdSetType
    { Item :: ![LicenseIdSetItemType]
    } deriving (Show)

data LicenseIdSetItemType = LicenseIdSetItemType
    { LicenseId :: !String
    } deriving (Show)

data DescribeLicensesResponseType = DescribeLicensesResponseType
    { RequestId  :: !String
    , LicenseSet :: !LicenseSetType
    } deriving (Show)

data LicenseSetType = LicenseSetType
    { Item :: ![LicenseSetItemType]
    } deriving (Show)

data LicenseSetItemType = LicenseSetItemType
    { LicenseId   :: !String
    , Type        :: !String
    , Pool        :: !String
    , CapacitySet :: !LicenseCapacitySetType
    , TagSet      :: !(Maybe ResourceTagSetType)
    } deriving (Show)

data LicenseCapacitySetType = LicenseCapacitySetType
    { Item :: ![LicenseCapacitySetItemType]
    } deriving (Show)

data LicenseCapacitySetItemType = LicenseCapacitySetItemType
    { Capacity                        :: !Int
    , InstanceCapacity                :: !Int
    , State                           :: !String
    , EarliestAllowedDeactivationTime :: !(Maybe DateTime)
    } deriving (Show)

data ActivateLicenseType = ActivateLicenseType
    { LicenseId :: !String
    , Capacity  :: !Int
    } deriving (Show)

data ActivateLicenseResponseType = ActivateLicenseResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

data DeactivateLicenseType = DeactivateLicenseType
    { LicenseId :: !String
    , Capacity  :: !Int
    } deriving (Show)

data DeactivateLicenseResponseType = DeactivateLicenseResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

type CreatePlacementGroup = CreatePlacementGroupType

type CreatePlacementGroupResponse = CreatePlacementGroupResponseType

data CreatePlacementGroupType = CreatePlacementGroupType
    { GroupName :: !String
    , Strategy  :: !String
    } deriving (Show)

data CreatePlacementGroupResponseType = CreatePlacementGroupResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

type DeletePlacementGroup = DeletePlacementGroupType

type DeletePlacementGroupResponse = DeletePlacementGroupResponseType

data DeletePlacementGroupType = DeletePlacementGroupType
    { GroupName :: !String
    } deriving (Show)

data DeletePlacementGroupResponseType = DeletePlacementGroupResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

type DescribePlacementGroups = DescribePlacementGroupsType

type DescribePlacementGroupsResponse = DescribePlacementGroupsResponseType

data DescribePlacementGroupItemType = DescribePlacementGroupItemType
    { GroupName :: !String
    } deriving (Show)

data DescribePlacementGroupsInfoType = DescribePlacementGroupsInfoType
    { Item :: ![DescribePlacementGroupItemType]
    } deriving (Show)

data DescribePlacementGroupsType = DescribePlacementGroupsType
    { PlacementGroupSet :: !DescribePlacementGroupsInfoType
    , FilterSet         :: !(Maybe FilterSetType)
    } deriving (Show)

data PlacementGroupInfoType = PlacementGroupInfoType
    { GroupName :: !String
    , Strategy  :: !String
    , State     :: !String
    } deriving (Show)

data PlacementGroupSetType = PlacementGroupSetType
    { Item :: ![PlacementGroupInfoType]
    } deriving (Show)

data DescribePlacementGroupsResponseType = DescribePlacementGroupsResponseType
    { RequestId         :: !String
    , PlacementGroupSet :: !PlacementGroupSetType
    } deriving (Show)

type CreateTags = CreateTagsType

type CreateTagsResponse = CreateTagsResponseType

type DescribeTags = DescribeTagsType

type DescribeTagsResponse = DescribeTagsResponseType

type DeleteTags = DeleteTagsType

type DeleteTagsResponse = DeleteTagsResponseType

data ResourceIdSetType = ResourceIdSetType
    { Item :: ![ResourceIdSetItemType]
    } deriving (Show)

data ResourceIdSetItemType = ResourceIdSetItemType
    { ResourceId :: !String
    } deriving (Show)

data ResourceTagSetItemType = ResourceTagSetItemType
    { Key   :: !String
    , Value :: !String
    } deriving (Show)

data ResourceTagSetType = ResourceTagSetType
    { Item :: ![ResourceTagSetItemType]
    } deriving (Show)

data CreateTagsType = CreateTagsType
    { ResourcesSet :: !ResourceIdSetType
    , TagSet       :: !ResourceTagSetType
    } deriving (Show)

data CreateTagsResponseType = CreateTagsResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

data TagSetItemType = TagSetItemType
    { ResourceId   :: !(Maybe String)
    , ResourceType :: !(Maybe String)
    , Key          :: !(Maybe String)
    , Value        :: !(Maybe String)
    } deriving (Show)

data TagSetType = TagSetType
    { Item :: ![TagSetItemType]
    } deriving (Show)

data DescribeTagsType = DescribeTagsType
    { FilterSet :: !(Maybe FilterSetType)
    } deriving (Show)

data DescribeTagsResponseType = DescribeTagsResponseType
    { RequestId :: !String
    , TagSet    :: !TagSetType
    } deriving (Show)

data DeleteTagsSetItemType = DeleteTagsSetItemType
    { Key   :: !(Maybe String)
    , Value :: !(Maybe String)
    } deriving (Show)

data DeleteTagsSetType = DeleteTagsSetType
    { Item :: ![DeleteTagsSetItemType]
    } deriving (Show)

data DeleteTagsType = DeleteTagsType
    { ResourcesSet :: !ResourceIdSetType
    , TagSet       :: !DeleteTagsSetType
    } deriving (Show)

data DeleteTagsResponseType = DeleteTagsResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

type ImportInstance = ImportInstanceType

type ImportInstanceResponse = ImportInstanceResponseType

data ImportInstanceType = ImportInstanceType
    { Description         :: !(Maybe String)
    , LaunchSpecification :: !ImportInstanceLaunchSpecificationType
    , DiskImageSet        :: !DiskImageSetType
    , KeepPartialImports  :: !(Maybe Boolean)
    , Platform            :: !String
    } deriving (Show)

data ImportInstanceResponseType = ImportInstanceResponseType
    { RequestId      :: !String
    , ConversionTask :: !ConversionTaskType
    } deriving (Show)

data ImportInstanceLaunchSpecificationType = ImportInstanceLaunchSpecificationType
    { Architecture                      :: !String
    , GroupSet                          :: !(Maybe ImportInstanceGroupSetType)
    , UserData                          :: !(Maybe UserDataType)
    , InstanceType                      :: !String
    , Placement                         :: !(Maybe InstancePlacementType)
    , Monitoring                        :: !(Maybe MonitoringInstanceType)
    , SubnetId                          :: !(Maybe String)
    , InstanceInitiatedShutdownBehavior :: !(Maybe String)
    , PrivateIpAddress                  :: !(Maybe String)
    } deriving (Show)

data DiskImageSetType = DiskImageSetType
    { Item :: ![DiskImageType]
    } deriving (Show)

data DiskImageType = DiskImageType
    { Image       :: !DiskImageDetailType
    , Description :: !(Maybe String)
    , Volume      :: !DiskImageVolumeType
    } deriving (Show)

data DiskImageDetailType = DiskImageDetailType
    { Format            :: !String
    , Bytes             :: !Long
    , ImportManifestUrl :: !String
    } deriving (Show)

data DiskImageVolumeType = DiskImageVolumeType
    { Size :: !Integer
    } deriving (Show)

data ConversionTaskType = ConversionTaskType
    { ConversionTaskId :: !String
    , ExpirationTime   :: !(Maybe String)
  -- <xs:choice>
    , ImportVolume     :: !(Maybe ImportVolumeTaskDetailsType)
    , ImportInstance   :: !(Maybe ImportInstanceTaskDetailsType)
  -- </xs:choice>
    , State            :: !String
    , StatusMessage    :: !(Maybe String)
    , TagSet           :: !(Maybe ResourceTagSetType)
    } deriving (Show)

data ImportInstanceTaskDetailsType = ImportInstanceTaskDetailsType
    { Volumes     :: !ImportInstanceVolumeDetailSetType
    , InstanceId  :: !(Maybe String)
    , Platform    :: !(Maybe String)
    , Description :: !(Maybe String)
    } deriving (Show)

data ImportVolumeTaskDetailsType = ImportVolumeTaskDetailsType
    { BytesConverted   :: !Long
    , AvailabilityZone :: !String
    , Description      :: !(Maybe String)
    , Image            :: !DiskImageDescriptionType
    , Volume           :: !DiskImageVolumeDescriptionType
    } deriving (Show)

data ImportInstanceVolumeDetailSetType = ImportInstanceVolumeDetailSetType
    { Item :: ![ImportInstanceVolumeDetailItemType]
    } deriving (Show)

data ImportInstanceVolumeDetailItemType = ImportInstanceVolumeDetailItemType
    { BytesConverted   :: !Long
    , AvailabilityZone :: !String
    , Image            :: !DiskImageDescriptionType
    , Description      :: !(Maybe String)
    , Volume           :: !DiskImageVolumeDescriptionType
    , Status           :: !String
    , StatusMessage    :: !(Maybe String)
    } deriving (Show)

data DiskImageVolumeDescriptionType = DiskImageVolumeDescriptionType
    { Size :: !Integer
    , Id   :: !String
    } deriving (Show)

data DiskImageDescriptionType = DiskImageDescriptionType
    { Format            :: !String
    , Size              :: !Long
    , ImportManifestUrl :: !String
    , Checksum          :: !(Maybe String)
    } deriving (Show)

type ImportVolume = ImportVolumeType

type ImportVolumeResponse = ImportVolumeResponseType

data ImportVolumeType = ImportVolumeType
    { AvailabilityZone :: !String
    , Image            :: !DiskImageDetailType
    , Description      :: !(Maybe String)
    , Volume           :: !DiskImageVolumeType
    } deriving (Show)

data ImportVolumeResponseType = ImportVolumeResponseType
    { RequestId      :: !String
    , ConversionTask :: !ConversionTaskType
    } deriving (Show)

type DescribeConversionTasks = DescribeConversionTasksType

type DescribeConversionTasksResponse = DescribeConversionTasksResponseType

data DescribeConversionTasksType = DescribeConversionTasksType
    { ConversionTaskIdSet :: !ConversionTaskIdSetType
    } deriving (Show)

data DescribeConversionTasksResponseType = DescribeConversionTasksResponseType
    { RequestId       :: !String
    , ConversionTasks :: !ConversionTaskSetType
    } deriving (Show)

data ConversionTaskIdSetType = ConversionTaskIdSetType
    { Item :: ![ConversionTaskIdItemType]
    } deriving (Show)

data ConversionTaskIdItemType = ConversionTaskIdItemType
    { ConversionTaskId :: !String
    } deriving (Show)

data ConversionTaskSetType = ConversionTaskSetType
    { Item :: ![ConversionTaskType]
    } deriving (Show)

type CancelConversionTask = CancelConversionTaskType

type CancelConversionTaskResponse = CancelConversionTaskResponseType

data CancelConversionTaskType = CancelConversionTaskType
    { ConversionTaskId :: !String
    } deriving (Show)

data CancelConversionTaskResponseType = CancelConversionTaskResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

type CreateInstanceExportTask = CreateInstanceExportTaskType

type CreateInstanceExportTaskResponse = CreateInstanceExportTaskResponseType

data CreateInstanceExportTaskType = CreateInstanceExportTaskType
    { Description       :: !(Maybe String)
    , InstanceId        :: !String
    , TargetEnvironment :: !String
  -- <xs:choice>
    , ExportToS3        :: !(Maybe ExportToS3TaskType)
  -- </xs:choice>
    } deriving (Show)

data ExportToS3TaskType = ExportToS3TaskType
    { DiskImageFormat :: !(Maybe String)
    , ContainerFormat :: !(Maybe String)
    , S3Bucket        :: !String
    , S3Prefix        :: !String
    } deriving (Show)

data CreateInstanceExportTaskResponseType = CreateInstanceExportTaskResponseType
    { RequestId  :: !String
    , ExportTask :: !ExportTaskResponseType
    } deriving (Show)

type DescribeExportTasks = DescribeExportTasksType

type DescribeExportTasksResponse = DescribeExportTasksResponseType

data DescribeExportTasksType = DescribeExportTasksType
    { ExportTaskIdSet :: !ExportTaskIdSetType
    } deriving (Show)

data ExportTaskIdSetType = ExportTaskIdSetType
    { Item :: ![ExportTaskIdType]
    } deriving (Show)

data ExportTaskIdType = ExportTaskIdType
    { ExportTaskId :: !String
    } deriving (Show)

data DescribeExportTasksResponseType = DescribeExportTasksResponseType
    { RequestId     :: !String
    , ExportTaskSet :: !ExportTaskSetResponseType
    } deriving (Show)

data ExportTaskSetResponseType = ExportTaskSetResponseType
    { Item :: ![ExportTaskResponseType]
    } deriving (Show)

data ExportTaskResponseType = ExportTaskResponseType
    { ExportTaskId   :: !String
    , Description    :: !(Maybe String)
    , State          :: !String
    , StatusMessage  :: !(Maybe String)
  -- <xs:choice>
    , InstanceExport :: !(Maybe InstanceExportTaskResponseType)
  -- </xs:choice>
  -- <xs:choice>
    , ExportToS3     :: !(Maybe ExportToS3TaskResponseType)
  -- </xs:choice>
    } deriving (Show)

data InstanceExportTaskResponseType = InstanceExportTaskResponseType
    { InstanceId        :: !String
    , TargetEnvironment :: !(Maybe String)
    } deriving (Show)

data ExportToS3TaskResponseType = ExportToS3TaskResponseType
    { DiskImageFormat :: !String
    , ContainerFormat :: !(Maybe String)
    , S3Bucket        :: !String
    , S3Key           :: !String
    } deriving (Show)

type CancelExportTask = CancelExportTaskType

type CancelExportTaskResponse = CancelExportTaskResponseType

data CancelExportTaskType = CancelExportTaskType
    { ExportTaskId :: !String
    } deriving (Show)

data CancelExportTaskResponseType = CancelExportTaskResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

type CreateInternetGateway = CreateInternetGatewayType

type CreateInternetGatewayResponse = CreateInternetGatewayResponseType

data CreateInternetGatewayType = CreateInternetGatewayType

data InternetGatewayAttachmentSetType = InternetGatewayAttachmentSetType
    { Item :: ![InternetGatewayAttachmentType]
    } deriving (Show)

data InternetGatewayAttachmentType = InternetGatewayAttachmentType
    { VpcId :: !String
    , State :: !String
    } deriving (Show)

data InternetGatewayType = InternetGatewayType
    { InternetGatewayId :: !String
    , AttachmentSet     :: !InternetGatewayAttachmentSetType
    , TagSet            :: !(Maybe ResourceTagSetType)
    } deriving (Show)

data CreateInternetGatewayResponseType = CreateInternetGatewayResponseType
    { RequestId       :: !String
    , InternetGateway :: !InternetGatewayType
    } deriving (Show)

type DescribeInternetGateways = DescribeInternetGatewaysType

type DescribeInternetGatewaysResponse = DescribeInternetGatewaysResponseType

data InternetGatewayIdSetType = InternetGatewayIdSetType
    { Item :: ![InternetGatewayIdSetItemType]
    } deriving (Show)

data InternetGatewayIdSetItemType = InternetGatewayIdSetItemType
    { InternetGatewayId :: !String
    } deriving (Show)

data DescribeInternetGatewaysType = DescribeInternetGatewaysType
    { InternetGatewayIdSet :: !InternetGatewayIdSetType
    , FilterSet            :: !(Maybe FilterSetType)
    } deriving (Show)

data InternetGatewaySetType = InternetGatewaySetType
    { Item :: ![InternetGatewayType]
    } deriving (Show)

data DescribeInternetGatewaysResponseType = DescribeInternetGatewaysResponseType
    { RequestId          :: !String
    , InternetGatewaySet :: !InternetGatewaySetType
    } deriving (Show)

type DeleteInternetGateway = DeleteInternetGatewayType

type DeleteInternetGatewayResponse = DeleteInternetGatewayResponseType

data DeleteInternetGatewayType = DeleteInternetGatewayType
    { InternetGatewayId :: !String
    } deriving (Show)

data DeleteInternetGatewayResponseType = DeleteInternetGatewayResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

type AttachInternetGateway = AttachInternetGatewayType

type AttachInternetGatewayResponse = AttachInternetGatewayResponseType

data AttachInternetGatewayType = AttachInternetGatewayType
    { InternetGatewayId :: !String
    , VpcId             :: !String
    } deriving (Show)

data AttachInternetGatewayResponseType = AttachInternetGatewayResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

type DetachInternetGateway = DetachInternetGatewayType

type DetachInternetGatewayResponse = DetachInternetGatewayResponseType

data DetachInternetGatewayType = DetachInternetGatewayType
    { InternetGatewayId :: !String
    , VpcId             :: !String
    } deriving (Show)

data DetachInternetGatewayResponseType = DetachInternetGatewayResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

type CreateRouteTable = CreateRouteTableType

type CreateRouteTableResponse = CreateRouteTableResponseType

data CreateRouteTableType = CreateRouteTableType
    { VpcId :: !String
    } deriving (Show)

data RouteSetType = RouteSetType
    { Item :: ![RouteType]
    } deriving (Show)

data RouteType = RouteType
    { DestinationCidrBlock :: !String
    , GatewayId            :: !(Maybe String)
    , InstanceId           :: !(Maybe String)
    , InstanceOwnerId      :: !(Maybe String)
    , NetworkInterfaceId   :: !(Maybe String)
    , State                :: !String
    , Origin               :: !String
    } deriving (Show)

data RouteTableAssociationSetType = RouteTableAssociationSetType
    { Item :: ![RouteTableAssociationType]
    } deriving (Show)

data RouteTableAssociationType = RouteTableAssociationType
    { RouteTableAssociationId :: !String
    , RouteTableId            :: !String
  -- <xs:choice>
    , SubnetId                :: !(Maybe String)
    , Main                    :: !(Maybe Boolean)
  -- </xs:choice>
    } deriving (Show)

data PropagatingVgwSetType = PropagatingVgwSetType
    { Item :: ![PropagatingVgwType]
    } deriving (Show)

data PropagatingVgwType = PropagatingVgwType
    { GatewayId :: !String
    } deriving (Show)

data RouteTableType = RouteTableType
    { RouteTableId      :: !String
    , VpcId             :: !String
    , RouteSet          :: !RouteSetType
    , AssociationSet    :: !RouteTableAssociationSetType
    , PropagatingVgwSet :: !PropagatingVgwSetType
    , TagSet            :: !(Maybe ResourceTagSetType)
    } deriving (Show)

data CreateRouteTableResponseType = CreateRouteTableResponseType
    { RequestId  :: !String
    , RouteTable :: !RouteTableType
    } deriving (Show)

type DescribeRouteTables = DescribeRouteTablesType

type DescribeRouteTablesResponse = DescribeRouteTablesResponseType

data RouteTableIdSetType = RouteTableIdSetType
    { Item :: ![RouteTableIdSetItemType]
    } deriving (Show)

data RouteTableIdSetItemType = RouteTableIdSetItemType
    { RouteTableId :: !String
    } deriving (Show)

data DescribeRouteTablesType = DescribeRouteTablesType
    { RouteTableIdSet :: !RouteTableIdSetType
    , FilterSet       :: !(Maybe FilterSetType)
    } deriving (Show)

data RouteTableSetType = RouteTableSetType
    { Item :: ![RouteTableType]
    } deriving (Show)

data DescribeRouteTablesResponseType = DescribeRouteTablesResponseType
    { RequestId     :: !String
    , RouteTableSet :: !RouteTableSetType
    } deriving (Show)

type EnableVgwRoutePropagation = EnableVgwRoutePropagationRequestType

type EnableVgwRoutePropagationResponse = EnableVgwRoutePropagationResponseType

data EnableVgwRoutePropagationRequestType = EnableVgwRoutePropagationRequestType
    { RouteTableId :: !String
    , GatewayId    :: !String
    } deriving (Show)

data EnableVgwRoutePropagationResponseType = EnableVgwRoutePropagationResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

type DisableVgwRoutePropagation = DisableVgwRoutePropagationRequestType

type DisableVgwRoutePropagationResponse = DisableVgwRoutePropagationResponseType

data DisableVgwRoutePropagationRequestType = DisableVgwRoutePropagationRequestType
    { RouteTableId :: !String
    , GatewayId    :: !String
    } deriving (Show)

data DisableVgwRoutePropagationResponseType = DisableVgwRoutePropagationResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

type DeleteRouteTable = DeleteRouteTableType

type DeleteRouteTableResponse = DeleteRouteTableResponseType

data DeleteRouteTableType = DeleteRouteTableType
    { RouteTableId :: !String
    } deriving (Show)

data DeleteRouteTableResponseType = DeleteRouteTableResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

type AssociateRouteTable = AssociateRouteTableType

type AssociateRouteTableResponse = AssociateRouteTableResponseType

data AssociateRouteTableType = AssociateRouteTableType
    { RouteTableId :: !String
    , SubnetId     :: !String
    } deriving (Show)

data AssociateRouteTableResponseType = AssociateRouteTableResponseType
    { RequestId     :: !String
    , AssociationId :: !String
    } deriving (Show)

type ReplaceRouteTableAssociation = ReplaceRouteTableAssociationType

type ReplaceRouteTableAssociationResponse = ReplaceRouteTableAssociationResponseType

data ReplaceRouteTableAssociationType = ReplaceRouteTableAssociationType
    { AssociationId :: !String
    , RouteTableId  :: !String
    } deriving (Show)

data ReplaceRouteTableAssociationResponseType = ReplaceRouteTableAssociationResponseType
    { RequestId        :: !String
    , NewAssociationId :: !String
    } deriving (Show)

type DisassociateRouteTable = DisassociateRouteTableType

type DisassociateRouteTableResponse = DisassociateRouteTableResponseType

data DisassociateRouteTableType = DisassociateRouteTableType
    { AssociationId :: !String
    } deriving (Show)

data DisassociateRouteTableResponseType = DisassociateRouteTableResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

type CreateRoute = CreateRouteType

type CreateRouteResponse = CreateRouteResponseType

data CreateRouteType = CreateRouteType
    { RouteTableId         :: !String
    , DestinationCidrBlock :: !String
  -- <xs:choice>
    , GatewayId            :: !(Maybe String)
    , InstanceId           :: !(Maybe String)
    , NetworkInterfaceId   :: !(Maybe String)
  -- </xs:choice>
    } deriving (Show)

data CreateRouteResponseType = CreateRouteResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

type ReplaceRoute = ReplaceRouteType

type ReplaceRouteResponse = ReplaceRouteResponseType

data ReplaceRouteType = ReplaceRouteType
    { RouteTableId         :: !String
    , DestinationCidrBlock :: !String
  -- <xs:choice>
    , GatewayId            :: !(Maybe String)
    , InstanceId           :: !(Maybe String)
    , NetworkInterfaceId   :: !(Maybe String)
  -- </xs:choice>
    } deriving (Show)

data ReplaceRouteResponseType = ReplaceRouteResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

type DeleteRoute = DeleteRouteType

type DeleteRouteResponse = DeleteRouteResponseType

data DeleteRouteType = DeleteRouteType
    { RouteTableId         :: !String
    , DestinationCidrBlock :: !String
    } deriving (Show)

data DeleteRouteResponseType = DeleteRouteResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

type CreateNetworkAcl = CreateNetworkAclType

type CreateNetworkAclResponse = CreateNetworkAclResponseType

data CreateNetworkAclType = CreateNetworkAclType
    { VpcId :: !String
    } deriving (Show)

data NetworkAclEntrySetType = NetworkAclEntrySetType
    { Item :: ![NetworkAclEntryType]
    } deriving (Show)

data IcmpTypeCodeType = IcmpTypeCodeType
    { Code :: !Int
    , Type :: !Int
    } deriving (Show)

data PortRangeType = PortRangeType
    { From :: !Int
    , To   :: !Int
    } deriving (Show)

data NetworkAclEntryType = NetworkAclEntryType
    { RuleNumber   :: !Int
    , Protocol     :: !String
    , RuleAction   :: !String
    , Egress       :: !Boolean
    , CidrBlock    :: !String
    , IcmpTypeCode :: !(Maybe IcmpTypeCodeType)
    , PortRange    :: !(Maybe PortRangeType)
    } deriving (Show)

data NetworkAclAssociationSetType = NetworkAclAssociationSetType
    { Item :: ![NetworkAclAssociationType]
    } deriving (Show)

data NetworkAclAssociationType = NetworkAclAssociationType
    { NetworkAclAssociationId :: !String
    , NetworkAclId            :: !String
    , SubnetId                :: !String
    } deriving (Show)

data NetworkAclType = NetworkAclType
    { NetworkAclId   :: !String
    , VpcId          :: !String
    , Default        :: !Boolean
    , EntrySet       :: !NetworkAclEntrySetType
    , AssociationSet :: !NetworkAclAssociationSetType
    , TagSet         :: !(Maybe ResourceTagSetType)
    } deriving (Show)

data CreateNetworkAclResponseType = CreateNetworkAclResponseType
    { RequestId  :: !String
    , NetworkAcl :: !NetworkAclType
    } deriving (Show)

type DescribeNetworkAcls = DescribeNetworkAclsType

type DescribeNetworkAclsResponse = DescribeNetworkAclsResponseType

data NetworkAclIdSetType = NetworkAclIdSetType
    { Item :: ![NetworkAclIdSetItemType]
    } deriving (Show)

data NetworkAclIdSetItemType = NetworkAclIdSetItemType
    { NetworkAclId :: !String
    } deriving (Show)

data DescribeNetworkAclsType = DescribeNetworkAclsType
    { NetworkAclIdSet :: !NetworkAclIdSetType
    , FilterSet       :: !(Maybe FilterSetType)
    } deriving (Show)

data NetworkAclSetType = NetworkAclSetType
    { Item :: ![NetworkAclType]
    } deriving (Show)

data DescribeNetworkAclsResponseType = DescribeNetworkAclsResponseType
    { RequestId     :: !String
    , NetworkAclSet :: !NetworkAclSetType
    } deriving (Show)

type DeleteNetworkAcl = DeleteNetworkAclType

type DeleteNetworkAclResponse = DeleteNetworkAclResponseType

data DeleteNetworkAclType = DeleteNetworkAclType
    { NetworkAclId :: !String
    } deriving (Show)

data DeleteNetworkAclResponseType = DeleteNetworkAclResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

type ReplaceNetworkAclAssociation = ReplaceNetworkAclAssociationType

type ReplaceNetworkAclAssociationResponse = ReplaceNetworkAclAssociationResponseType

data ReplaceNetworkAclAssociationType = ReplaceNetworkAclAssociationType
    { AssociationId :: !String
    , NetworkAclId  :: !String
    } deriving (Show)

data ReplaceNetworkAclAssociationResponseType = ReplaceNetworkAclAssociationResponseType
    { RequestId        :: !String
    , NewAssociationId :: !String
    } deriving (Show)

type CreateNetworkAclEntry = CreateNetworkAclEntryType

type CreateNetworkAclEntryResponse = CreateNetworkAclEntryResponseType

data CreateNetworkAclEntryType = CreateNetworkAclEntryType
    { NetworkAclId :: !String
    , RuleNumber   :: !Int
    , Protocol     :: !String
    , RuleAction   :: !String
    , Egress       :: !Boolean
    , CidrBlock    :: !String
    , IcmpTypeCode :: !(Maybe IcmpTypeCodeType)
    , PortRange    :: !(Maybe PortRangeType)
    } deriving (Show)

data CreateNetworkAclEntryResponseType = CreateNetworkAclEntryResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

type ReplaceNetworkAclEntry = ReplaceNetworkAclEntryType

type ReplaceNetworkAclEntryResponse = ReplaceNetworkAclEntryResponseType

data ReplaceNetworkAclEntryType = ReplaceNetworkAclEntryType
    { NetworkAclId :: !String
    , RuleNumber   :: !Int
    , Protocol     :: !String
    , RuleAction   :: !String
    , Egress       :: !Boolean
    , CidrBlock    :: !String
    , IcmpTypeCode :: !(Maybe IcmpTypeCodeType)
    , PortRange    :: !(Maybe PortRangeType)
    } deriving (Show)

data ReplaceNetworkAclEntryResponseType = ReplaceNetworkAclEntryResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

type DeleteNetworkAclEntry = DeleteNetworkAclEntryType

type DeleteNetworkAclEntryResponse = DeleteNetworkAclEntryResponseType

data DeleteNetworkAclEntryType = DeleteNetworkAclEntryType
    { NetworkAclId :: !String
    , RuleNumber   :: !Int
    , Egress       :: !Boolean
    } deriving (Show)

data DeleteNetworkAclEntryResponseType = DeleteNetworkAclEntryResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

type DescribeInstanceStatus = DescribeInstanceStatusType

data DescribeInstanceStatusType = DescribeInstanceStatusType
    { InstancesSet        :: !InstanceIdSetType
    , FilterSet           :: !(Maybe FilterSetType)
    , NextToken           :: !(Maybe String)
    , MaxResults          :: !(Maybe Int)
    , IncludeAllInstances :: !(Maybe Boolean)
    } deriving (Show)

type DescribeInstanceStatusResponse = DescribeInstanceStatusResponseType

data DescribeInstanceStatusResponseType = DescribeInstanceStatusResponseType
    { RequestId         :: !String
    , InstanceStatusSet :: !InstanceStatusSetType
    , NextToken         :: !(Maybe String)
    } deriving (Show)

data InstanceStatusSetType = InstanceStatusSetType
    { Item :: ![InstanceStatusItemType]
    } deriving (Show)

data InstanceStatusType = InstanceStatusType
    { Status  :: !String
    , Details :: !(Maybe InstanceStatusDetailsSetType)
    } deriving (Show)

data InstanceStatusDetailsSetType = InstanceStatusDetailsSetType
    { Item :: ![InstanceStatusDetailsSetItemType]
    } deriving (Show)

data InstanceStatusDetailsSetItemType = InstanceStatusDetailsSetItemType
    { Name          :: !String
    , Status        :: !String
    , ImpairedSince :: !(Maybe DateTime)
    } deriving (Show)

data InstanceStatusEventType = InstanceStatusEventType
    { Code        :: !String
    , Description :: !String
    , NotBefore   :: !DateTime
    , NotAfter    :: !(Maybe DateTime)
    } deriving (Show)

data InstanceStatusEventsSetType = InstanceStatusEventsSetType
    { Item :: ![InstanceStatusEventType]
    } deriving (Show)

data InstanceStatusItemType = InstanceStatusItemType
    { InstanceId       :: !String
    , AvailabilityZone :: !String
    , EventsSet        :: !(Maybe InstanceStatusEventsSetType)
    , InstanceState    :: !InstanceStateType
    , SystemStatus     :: !InstanceStatusType
    , InstanceStatus   :: !InstanceStatusType
    } deriving (Show)

type ReportInstanceStatus = ReportInstanceStatusType

data ReportInstanceStatusType = ReportInstanceStatusType
    { InstancesSet   :: !InstanceIdSetType
    , Status         :: !String
    , StartTime      :: !(Maybe DateTime)
    , EndTime        :: !(Maybe DateTime)
    , ReasonCodesSet :: !ReportInstanceStatusReasonCodesSetType
    , Description    :: !(Maybe String)
    } deriving (Show)

data ReportInstanceStatusReasonCodesSetType = ReportInstanceStatusReasonCodesSetType
    {
  Item :: !(NonEmpty ReportInstanceStatusReasonCodeSetItemType)
    } deriving (Show)

data ReportInstanceStatusReasonCodeSetItemType = ReportInstanceStatusReasonCodeSetItemType
    { ReasonCode :: !String
    } deriving (Show)

type ReportInstanceStatusResponse = ReportInstanceStatusResponseType

data ReportInstanceStatusResponseType = ReportInstanceStatusResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

type CreateNetworkInterface = CreateNetworkInterfaceType

type CreateNetworkInterfaceResponse = CreateNetworkInterfaceResponseType

data CreateNetworkInterfaceType = CreateNetworkInterfaceType
    { SubnetId                       :: !String
    , Description                    :: !(Maybe String)
    , PrivateIpAddress               :: !(Maybe String)
    , GroupSet                       :: !(Maybe SecurityGroupIdSetType)
    , PrivateIpAddressesSet          :: !(Maybe PrivateIpAddressesSetRequestType)
    , SecondaryPrivateIpAddressCount :: !(Maybe Int)
    } deriving (Show)

data CreateNetworkInterfaceResponseType = CreateNetworkInterfaceResponseType
    { RequestId        :: !String
    , NetworkInterface :: !NetworkInterfaceType
    } deriving (Show)

type DescribeNetworkInterfaces = DescribeNetworkInterfacesType

type DescribeNetworkInterfacesResponse = DescribeNetworkInterfacesResponseType

data NetworkInterfaceIdSetType = NetworkInterfaceIdSetType
    { Item :: ![NetworkInterfaceIdSetItemType]
    } deriving (Show)

data NetworkInterfaceIdSetItemType = NetworkInterfaceIdSetItemType
    { NetworkInterfaceId :: !String
    } deriving (Show)

data DescribeNetworkInterfacesType = DescribeNetworkInterfacesType
    { NetworkInterfaceIdSet :: !(Maybe NetworkInterfaceIdSetType)
    , FilterSet             :: !(Maybe FilterSetType)
    } deriving (Show)

data NetworkInterfaceType = NetworkInterfaceType
    { NetworkInterfaceId    :: !String
    , SubnetId              :: !(Maybe String)
    , VpcId                 :: !(Maybe String)
    , AvailabilityZone      :: !(Maybe String)
    , Description           :: !(Maybe String)
    , OwnerId               :: !String
    , RequesterId           :: !(Maybe String)
    , RequesterManaged      :: !(Maybe Boolean)
    , Status                :: !String
    , MacAddress            :: !String
    , PrivateIpAddress      :: !String
    , PrivateDnsName        :: !(Maybe String)
    , SourceDestCheck       :: !Boolean
    , GroupSet              :: !GroupSetType
    , Attachment            :: !(Maybe NetworkInterfaceAttachmentType)
    , Association           :: !(Maybe NetworkInterfaceAssociationType)
    , TagSet                :: !(Maybe ResourceTagSetType)
    , PrivateIpAddressesSet :: !(Maybe NetworkInterfacePrivateIpAddressesSetType)
    } deriving (Show)

data NetworkInterfacePrivateIpAddressesSetType = NetworkInterfacePrivateIpAddressesSetType
    { Item :: ![NetworkInterfacePrivateIpAddressesSetItemType]
    } deriving (Show)

data NetworkInterfacePrivateIpAddressesSetItemType = NetworkInterfacePrivateIpAddressesSetItemType
    { PrivateIpAddress :: !String
    , PrivateDnsName   :: !(Maybe String)
    , Primary          :: !Boolean
    , Association      :: !(Maybe NetworkInterfaceAssociationType)
    } deriving (Show)

data NetworkInterfaceAttachmentType = NetworkInterfaceAttachmentType
    { AttachmentId        :: !String
    , InstanceId          :: !(Maybe String)
    , InstanceOwnerId     :: !(Maybe String)
    , DeviceIndex         :: !Int
    , Status              :: !String
    , AttachTime          :: !DateTime
    , DeleteOnTermination :: !Boolean
    } deriving (Show)

data NetworkInterfaceAssociationType = NetworkInterfaceAssociationType
    { PublicIp      :: !String
    , PublicDnsName :: !(Maybe String)
    , IpOwnerId     :: !(Maybe String)
    , AllocationId  :: !(Maybe String)
    , AssociationId :: !(Maybe String)
    } deriving (Show)

data NetworkInterfaceSetType = NetworkInterfaceSetType
    { Item :: ![NetworkInterfaceType]
    } deriving (Show)

data DescribeNetworkInterfacesResponseType = DescribeNetworkInterfacesResponseType
    { RequestId           :: !String
    , NetworkInterfaceSet :: !NetworkInterfaceSetType
    } deriving (Show)

type DeleteNetworkInterface = DeleteNetworkInterfaceType

type DeleteNetworkInterfaceResponse = DeleteNetworkInterfaceResponseType

data DeleteNetworkInterfaceType = DeleteNetworkInterfaceType
    { NetworkInterfaceId :: !String
    } deriving (Show)

data DeleteNetworkInterfaceResponseType = DeleteNetworkInterfaceResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

type AttachNetworkInterface = AttachNetworkInterfaceType

type AttachNetworkInterfaceResponse = AttachNetworkInterfaceResponseType

data AttachNetworkInterfaceType = AttachNetworkInterfaceType
    { NetworkInterfaceId :: !String
    , InstanceId         :: !String
    , DeviceIndex        :: !Int
    } deriving (Show)

data AttachNetworkInterfaceResponseType = AttachNetworkInterfaceResponseType
    { RequestId    :: !String
    , AttachmentId :: !String
    } deriving (Show)

type DetachNetworkInterface = DetachNetworkInterfaceType

type DetachNetworkInterfaceResponse = DetachNetworkInterfaceResponseType

data DetachNetworkInterfaceType = DetachNetworkInterfaceType
    { AttachmentId :: !String
    , Force        :: !(Maybe Boolean)
    } deriving (Show)

data DetachNetworkInterfaceResponseType = DetachNetworkInterfaceResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

type DescribeNetworkInterfaceAttribute = DescribeNetworkInterfaceAttributeType

type DescribeNetworkInterfaceAttributeResponse = DescribeNetworkInterfaceAttributeResponseType

data DescribeNetworkInterfaceAttributeType = DescribeNetworkInterfaceAttributeType
    { NetworkInterfaceId                      :: !String
    , DescribeNetworkInterfaceAttributesGroup :: !DescribeNetworkInterfaceAttributesGroup
    } deriving (Show)

data DescribeNetworkInterfaceAttributesGroup = DescribeNetworkInterfaceAttributesGroup
    { Description     :: !EmptyElementType
    , SourceDestCheck :: !EmptyElementType
    , GroupSet        :: !EmptyElementType
    , Attachment      :: !EmptyElementType
    } deriving (Show)
data DescribeNetworkInterfaceAttributeResponseType = DescribeNetworkInterfaceAttributeResponseType
    { RequestId          :: !String
    , NetworkInterfaceId :: !String
  -- <xs:choice>
    , Description        :: !(Maybe NullableAttributeValueType)
    , SourceDestCheck    :: !(Maybe AttributeBooleanValueType)
    , GroupSet           :: !(Maybe GroupSetType)
    , Attachment         :: !(Maybe NetworkInterfaceAttachmentType)
  -- </xs:choice>
    } deriving (Show)

type ModifyNetworkInterfaceAttribute = ModifyNetworkInterfaceAttributeType

type ModifyNetworkInterfaceAttributeResponse = ModifyNetworkInterfaceAttributeResponseType

data ModifyNetworkInterfaceAttributeType = ModifyNetworkInterfaceAttributeType
    { NetworkInterfaceId :: !String
  -- <xs:choice>
    , Description        :: !(Maybe NullableAttributeValueType)
    , SourceDestCheck    :: !(Maybe AttributeBooleanValueType)
    , GroupSet           :: !(Maybe SecurityGroupIdSetType)
    , Attachment         :: !(Maybe ModifyNetworkInterfaceAttachmentType)
  -- </xs:choice>
    } deriving (Show)

data ModifyNetworkInterfaceAttachmentType = ModifyNetworkInterfaceAttachmentType
    { AttachmentId        :: !String
    , DeleteOnTermination :: !Boolean
    } deriving (Show)

data ModifyNetworkInterfaceAttributeResponseType = ModifyNetworkInterfaceAttributeResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

type ResetNetworkInterfaceAttribute = ResetNetworkInterfaceAttributeType

type ResetNetworkInterfaceAttributeResponse = ResetNetworkInterfaceAttributeResponseType

data ResetNetworkInterfaceAttributeType = ResetNetworkInterfaceAttributeType
    { NetworkInterfaceId                   :: !String
    , ResetNetworkInterfaceAttributesGroup :: !ResetNetworkInterfaceAttributesGroup
    } deriving (Show)

data ResetNetworkInterfaceAttributesGroup = ResetNetworkInterfaceAttributesGroup
    { SourceDestCheck :: !EmptyElementType
    } deriving (Show)
data ResetNetworkInterfaceAttributeResponseType = ResetNetworkInterfaceAttributeResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

type AssignPrivateIpAddresses = AssignPrivateIpAddressesType

data AssignPrivateIpAddressesType = AssignPrivateIpAddressesType
    { NetworkInterfaceId             :: !String
    , PrivateIpAddressesSet          :: !(Maybe AssignPrivateIpAddressesSetRequestType)
    , SecondaryPrivateIpAddressCount :: !(Maybe Int)
    , AllowReassignment              :: !(Maybe Boolean)
    } deriving (Show)

type AssignPrivateIpAddressesResponse = AssignPrivateIpAddressesResponseType

data AssignPrivateIpAddressesResponseType = AssignPrivateIpAddressesResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

type UnassignPrivateIpAddresses = UnassignPrivateIpAddressesType

data UnassignPrivateIpAddressesType = UnassignPrivateIpAddressesType
    { NetworkInterfaceId    :: !String
    , PrivateIpAddressesSet :: !AssignPrivateIpAddressesSetRequestType
    } deriving (Show)

type UnassignPrivateIpAddressesResponse = UnassignPrivateIpAddressesResponseType

data UnassignPrivateIpAddressesResponseType = UnassignPrivateIpAddressesResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

data AssignPrivateIpAddressesSetRequestType = AssignPrivateIpAddressesSetRequestType
    {
  Item :: !(NonEmpty AssignPrivateIpAddressesSetItemRequestType)
    } deriving (Show)

data AssignPrivateIpAddressesSetItemRequestType = AssignPrivateIpAddressesSetItemRequestType
    { PrivateIpAddress :: !String
    } deriving (Show)

type DescribeVolumeStatus = DescribeVolumeStatusType

data DescribeVolumeStatusType = DescribeVolumeStatusType
    { VolumeSet  :: !DescribeVolumesSetType
    , FilterSet  :: !(Maybe FilterSetType)
    , MaxResults :: !(Maybe Integer)
    , NextToken  :: !(Maybe String)
    } deriving (Show)

type DescribeVolumeStatusResponse = DescribeVolumeStatusResponseType

data DescribeVolumeStatusResponseType = DescribeVolumeStatusResponseType
    { RequestId       :: !String
    , VolumeStatusSet :: !VolumeStatusSetType
    , NextToken       :: !(Maybe String)
    } deriving (Show)

data VolumeStatusSetType = VolumeStatusSetType
    { Item :: ![VolumeStatusItemType]
    } deriving (Show)

data VolumeStatusItemType = VolumeStatusItemType
    { VolumeId         :: !String
    , AvailabilityZone :: !String
    , VolumeStatus     :: !VolumeStatusInfoType
    , EventsSet        :: !VolumeStatusEventsSetType
    , ActionsSet       :: !VolumeStatusActionsSetType
    } deriving (Show)

data VolumeStatusInfoType = VolumeStatusInfoType
    { Status  :: !String
    , Details :: !VolumeStatusDetailsSetType
    } deriving (Show)

data VolumeStatusDetailsSetType = VolumeStatusDetailsSetType
    { Item :: ![VolumeStatusDetailsItemType]
    } deriving (Show)

data VolumeStatusDetailsItemType = VolumeStatusDetailsItemType
    { Name   :: !String
    , Status :: !String
    } deriving (Show)

data VolumeStatusEventsSetType = VolumeStatusEventsSetType
    { Item :: ![VolumeStatusEventItemType]
    } deriving (Show)

data VolumeStatusEventItemType = VolumeStatusEventItemType
    { Description :: !String
    , NotBefore   :: !DateTime
    , NotAfter    :: !DateTime
    , EventId     :: !String
    , EventType   :: !String
    } deriving (Show)

data VolumeStatusActionsSetType = VolumeStatusActionsSetType
    { Item :: ![VolumeStatusActionItemType]
    } deriving (Show)

data VolumeStatusActionItemType = VolumeStatusActionItemType
    { Description :: !String
    , Code        :: !String
    , EventId     :: !String
    , EventType   :: !String
    } deriving (Show)

type EnableVolumeIO = EnableVolumeIOType

data EnableVolumeIOType = EnableVolumeIOType
    { VolumeId :: !String
    } deriving (Show)

type EnableVolumeIOResponse = EnableVolumeIOResponseType

data EnableVolumeIOResponseType = EnableVolumeIOResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

type ModifyVolumeAttribute = ModifyVolumeAttributeType

data ModifyVolumeAttributeType = ModifyVolumeAttributeType
    { VolumeId     :: !String
  -- <xs:choice>
    , AutoEnableIO :: !AttributeBooleanValueType
  -- </xs:choice>
    } deriving (Show)

type ModifyVolumeAttributeResponse = ModifyVolumeAttributeResponseType

data ModifyVolumeAttributeResponseType = ModifyVolumeAttributeResponseType
    { RequestId :: !String
    , Return    :: !Boolean
    } deriving (Show)

type DescribeVolumeAttribute = DescribeVolumeAttributeType

type DescribeVolumeAttributeResponse = DescribeVolumeAttributeResponseType

data DescribeVolumeAttributeType = DescribeVolumeAttributeType
    { VolumeId                      :: !String
    , DescribeVolumeAttributesGroup :: !DescribeVolumeAttributesGroup
    } deriving (Show)

data DescribeVolumeAttributesGroup = DescribeVolumeAttributesGroup
    { AutoEnableIO :: !EmptyElementType
    , ProductCodes :: !EmptyElementType
    } deriving (Show)

data DescribeVolumeAttributeResponseType = DescribeVolumeAttributeResponseType
    { RequestId    :: !String
    , VolumeId     :: !String
  -- <xs:choice>
    , AutoEnableIO :: !(Maybe NullableAttributeBooleanValueType)
    , ProductCodes :: !(Maybe ProductCodesSetType)
  -- </xs:choice>
    } deriving (Show)
