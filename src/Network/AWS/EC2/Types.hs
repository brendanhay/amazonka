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

import Data.Text
import Network.AWS.Internal

data CreateImage = CreateImage
    { ciInstanceId :: !Text
    , ciName :: !Text
    , ciDescription :: !(Maybe Text)
    , ciNoReboot :: !(Maybe Boolean)
    , ciBlockDeviceMapping :: !(Maybe BlockDeviceMapping)
    } deriving (Show)

data CreateImageResponse = CreateImageResponse
    { cirRequestId :: !Text
    , cirImageId :: !Text
    } deriving (Show)

data RegisterImage = RegisterImage
    { riImageLocation :: !(Maybe Text)
    , riName :: !Text
    , riDescription :: !(Maybe Text)
    , riArchitecture :: !(Maybe Text)
    , riKernelId :: !(Maybe Text)
    , riRamdiskId :: !(Maybe Text)
    , riRootDeviceName :: !(Maybe Text)
    , riBlockDeviceMapping :: !(Maybe BlockDeviceMapping)
    } deriving (Show)

data RegisterImageResponse = RegisterImageResponse
    { rirRequestId :: !Text
    , rirImageId :: !Text
    } deriving (Show)

data DeregisterImage = DeregisterImage
    { diImageId :: !Text
    } deriving (Show)

data DeregisterImageResponse = DeregisterImageResponse
    { dirRequestId :: !Text
    , dirReturn :: !Boolean
    } deriving (Show)

data CreateKeyPair = CreateKeyPair
    { ckpKeyName :: !Text
    } deriving (Show)

data CreateKeyPairResponse = CreateKeyPairResponse
    { ckprRequestId :: !Text
    , ckprKeyName :: !Text
    , ckprKeyFingerprint :: !Text
    , ckprKeyMaterial :: !Text
    } deriving (Show)

data ImportKeyPair = ImportKeyPair
    { ikpKeyName :: !Text
    , ikpPublicKeyMaterial :: !Text
    } deriving (Show)

data ImportKeyPairResponse = ImportKeyPairResponse
    { ikprRequestId :: !Text
    , ikprKeyName :: !Text
    , ikprKeyFingerprint :: !Text
    } deriving (Show)

data DeleteKeyPair = DeleteKeyPair
    { dkpKeyName :: !Text
    } deriving (Show)

data DeleteKeyPairResponse = DeleteKeyPairResponse
    { dkprRequestId :: !Text
    , dkprReturn :: !Boolean
    } deriving (Show)

data DescribeKeyPairs = DescribeKeyPairs
    { dkpKeySet :: !DescribeKeyPairsInfo
    , dkpFilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data DescribeKeyPairsInfo = DescribeKeyPairsInfo
    { dkpiItem :: ![DescribeKeyPairsItem]
    } deriving (Show)

data DescribeKeyPairsItem = DescribeKeyPairsItem
    { dkpiKeyName :: !Text
    } deriving (Show)

data DescribeKeyPairsResponse = DescribeKeyPairsResponse
    { dkprRequestId :: !Text
    , dkprKeySet :: !DescribeKeyPairsResponseInfo
    } deriving (Show)

data DescribeKeyPairsResponseInfo = DescribeKeyPairsResponseInfo
    { dkpriItem :: ![DescribeKeyPairsResponseItem]
    } deriving (Show)

data DescribeKeyPairsResponseItem = DescribeKeyPairsResponseItem
    { dkpriKeyName :: !Text
    , dkpriKeyFingerprint :: !Text
    } deriving (Show)

data RunInstances = RunInstances
    { riImageId :: !Text
    , riMinCount :: !Int
    , riMaxCount :: !Int
    , riKeyName :: !(Maybe Text)
    , riGroupSet :: !GroupSet
    , riUserData :: !(Maybe UserData)
    , riInstance :: !Text
    , riPlacement :: !(Maybe PlacementRequest)
    , riKernelId :: !(Maybe Text)
    , riRamdiskId :: !(Maybe Text)
    , riBlockDeviceMapping :: !(Maybe BlockDeviceMapping)
    , riMonitoring :: !(Maybe MonitoringInstance)
    , riSubnetId :: !(Maybe Text)
    , riDisableApiTermination :: !(Maybe Boolean)
    , riInstanceInitiatedShutdownBehavior :: !(Maybe Text)
    , riLicense :: !(Maybe InstanceLicenseRequest)
    , riPrivateIpAddress :: !(Maybe Text)
    , riClientToken :: !(Maybe Text)
    , riNetworkInterfaceSet :: !(Maybe InstanceNetworkInterfaceSetRequest)
    , riIamInstanceProfile :: !(Maybe IamInstanceProfileRequest)
    , riEbsOptimized :: !(Maybe Boolean)
    } deriving (Show)

data IamInstanceProfileRequest = IamInstanceProfileRequest
    { iiprArn :: !(Maybe Text)
    , iiprName :: !(Maybe Text)
    } deriving (Show)

data InstanceNetworkInterfaceSetRequest = InstanceNetworkInterfaceSetRequest
    { inisrItem :: ![InstanceNetworkInterfaceSetItemRequest]
    } deriving (Show)

data InstanceNetworkInterfaceSetItemRequest = InstanceNetworkInterfaceSetItemRequest
    { inisirNetworkInterfaceId :: !(Maybe Text)
    , inisirDeviceIndex :: !Int
    , inisirSubnetId :: !(Maybe Text)
    , inisirDescription :: !(Maybe Text)
    , inisirPrivateIpAddress :: !(Maybe Text)
    , inisirGroupSet :: !(Maybe SecurityGroupIdSet)
    , inisirDeleteOnTermination :: !(Maybe Boolean)
    , inisirPrivateIpAddressesSet :: !(Maybe PrivateIpAddressesSetRequest)
    , inisirSecondaryPrivateIpAddressCount :: !(Maybe Int)
    } deriving (Show)

data PrivateIpAddressesSetRequest = PrivateIpAddressesSetRequest
    { piasrItem :: ![PrivateIpAddressesSetItemRequest]
    } deriving (Show)

data PrivateIpAddressesSetItemRequest = PrivateIpAddressesSetItemRequest
    { piasirPrivateIpAddress :: !Text
    , piasirPrimary :: !(Maybe Boolean)
    } deriving (Show)

data ImportInstanceGroupSet = ImportInstanceGroupSet
    { iigsItem :: ![ImportInstanceGroupItem]
    } deriving (Show)

data ImportInstanceGroupItem = ImportInstanceGroupItem
    { iigiGroupId :: !(Maybe Text)
    , iigiGroupName :: !(Maybe Text)
    } deriving (Show)

data GroupSet = GroupSet
    { gsItem :: ![GroupItem]
    } deriving (Show)

data GroupItem = GroupItem
    { giGroupId :: !(Maybe Text)
    , giGroupName :: !(Maybe Text)
    } deriving (Show)

data UserData = UserData
    { udData :: !(Maybe Text)
    , udVersion :: !Text
    , udEncoding :: !Text
    } deriving (Show)

data BlockDeviceMapping = BlockDeviceMapping
    { bdmItem :: ![BlockDeviceMappingItem]
    } deriving (Show)

data BlockDeviceMappingItem = BlockDeviceMappingItem
    { bdmiDeviceName :: !Text
  -- <xs:choice>
    , bdmiVirtualName :: !(Maybe Text)
    , bdmiEbs :: !(Maybe EbsBlockDevice)
    , bdmiNoDevice :: !(Maybe EmptyElement)
  -- </xs:choice>
    } deriving (Show)

data EbsBlockDevice = EbsBlockDevice
    { ebdSnapshotId :: !(Maybe Text)
    , ebdVolumeSize :: !(Maybe Int)
    , ebdDeleteOnTermination :: !(Maybe Boolean)
    , ebdVolume :: !(Maybe Text)
    , ebdIops :: !(Maybe Int)
    } deriving (Show)

data PlacementRequest = PlacementRequest
    { prAvailabilityZone :: !(Maybe Text)
    , prGroupName :: !(Maybe Text)
    , prTenancy :: !(Maybe Text)
    } deriving (Show)

data SpotPlacementRequest = SpotPlacementRequest
    { sprAvailabilityZone :: !(Maybe Text)
    , sprGroupName :: !(Maybe Text)
    } deriving (Show)

data InstancePlacement = InstancePlacement
    { ipAvailabilityZone :: !(Maybe Text)
    , ipGroupName :: !(Maybe Text)
    } deriving (Show)

data MonitoringInstance = MonitoringInstance
    { miEnabled :: !(Maybe Boolean)
    } deriving (Show)

data InstanceLicenseRequest = InstanceLicenseRequest
    { ilrPool :: !Text
    } deriving (Show)

data RunInstancesResponse = RunInstancesResponse
    { rirRequestId :: !Text
    , rirReservationId :: !Text
    , rirOwnerId :: !Text
    , rirGroupSet :: !GroupSet
    , rirInstancesSet :: !RunningInstancesSet
    , rirRequesterId :: !(Maybe Text)
    } deriving (Show)

data ReservationInfo = ReservationInfo
    { riReservationId :: !Text
    , riOwnerId :: !Text
    , riGroupSet :: !GroupSet
    , riInstancesSet :: !RunningInstancesSet
    , riRequesterId :: !(Maybe Text)
    } deriving (Show)

data RunningInstancesSet = RunningInstancesSet
    { risItem :: ![RunningInstancesItem]
    } deriving (Show)

data RunningInstancesItem = RunningInstancesItem
    { riiInstanceId :: !Text
    , riiImageId :: !(Maybe Text)
    , riiInstanceState :: !InstanceState
    , riiPrivateDnsName :: !Text
    , riiDnsName :: !(Maybe Text)
    , riiReason :: !(Maybe Text)
    , riiKeyName :: !(Maybe Text)
    , riiAmiLaunchIndex :: !(Maybe Text)
    , riiProductCodes :: !(Maybe ProductCodesSet)
    , riiInstance :: !Text
    , riiLaunchTime :: !DateTime
    , riiPlacement :: !(Maybe PlacementResponse)
    , riiKernelId :: !(Maybe Text)
    , riiRamdiskId :: !(Maybe Text)
    , riiPlatform :: !(Maybe Text)
    , riiMonitoring :: !(Maybe InstanceMonitoringState)
    , riiSubnetId :: !(Maybe Text)
    , riiVpcId :: !(Maybe Text)
    , riiPrivateIpAddress :: !(Maybe Text)
    , riiIpAddress :: !(Maybe Text)
    , riiSourceDestCheck :: !(Maybe Boolean)
    , riiGroupSet :: !GroupSet
    , riiStateReason :: !(Maybe StateReason)
    , riiArchitecture :: !(Maybe Text)
    , riiRootDevice :: !(Maybe Text)
    , riiRootDeviceName :: !(Maybe Text)
    , riiBlockDeviceMapping :: !(Maybe InstanceBlockDeviceMappingResponse)
    , riiInstanceLifecycle :: !(Maybe Text)
    , riiSpotInstanceRequestId :: !(Maybe Text)
    , riiLicense :: !(Maybe InstanceLicenseResponse)
    , riiVirtualization :: !(Maybe Text)
    , riiClientToken :: !(Maybe Text)
    , riiTagSet :: !(Maybe ResourceTagSet)
    , riiHypervisor :: !(Maybe Text)
    , riiNetworkInterfaceSet :: !(Maybe InstanceNetworkInterfaceSet)
    , riiIamInstanceProfile :: !(Maybe IamInstanceProfileResponse)
    , riiEbsOptimized :: !(Maybe Boolean)
    } deriving (Show)

data IamInstanceProfileResponse = IamInstanceProfileResponse
    { iiprArn :: !Text
    , iiprId :: !Text
    } deriving (Show)

data InstanceNetworkInterfaceSet = InstanceNetworkInterfaceSet
    { inisItem :: ![InstanceNetworkInterfaceSetItem]
    } deriving (Show)

data InstanceNetworkInterfaceSetItem = InstanceNetworkInterfaceSetItem
    { inisiNetworkInterfaceId :: !Text
    , inisiSubnetId :: !(Maybe Text)
    , inisiVpcId :: !(Maybe Text)
    , inisiDescription :: !(Maybe Text)
    , inisiOwnerId :: !Text
    , inisiStatus :: !Text
    , inisiMacAddress :: !(Maybe Text)
    , inisiPrivateIpAddress :: !(Maybe Text)
    , inisiPrivateDnsName :: !(Maybe Text)
    , inisiSourceDestCheck :: !(Maybe Boolean)
    , inisiGroupSet :: !(Maybe GroupSet)
    , inisiAttachment :: !InstanceNetworkInterfaceAttachment
    , inisiAssociation :: !(Maybe InstanceNetworkInterfaceAssociation)
    , inisiPrivateIpAddressesSet :: !(Maybe InstancePrivateIpAddressesSet)
    } deriving (Show)

data InstancePrivateIpAddressesSet = InstancePrivateIpAddressesSet
    { ipiasItem :: ![InstancePrivateIpAddressesSetItem]
    } deriving (Show)

data InstancePrivateIpAddressesSetItem = InstancePrivateIpAddressesSetItem
    { ipiasiPrivateIpAddress :: !(Maybe Text)
    , ipiasiPrivateDnsName :: !(Maybe Text)
    , ipiasiPrimary :: !(Maybe Boolean)
    , ipiasiAssociation :: !(Maybe InstanceNetworkInterfaceAssociation)
    } deriving (Show)

data InstanceNetworkInterfaceAttachment = InstanceNetworkInterfaceAttachment
    { iniaAttachmentId :: !Text
    , iniaDeviceIndex :: !Int
    , iniaStatus :: !Text
    , iniaAttachTime :: !DateTime
    , iniaDeleteOnTermination :: !Boolean
    } deriving (Show)

data InstanceNetworkInterfaceAssociation = InstanceNetworkInterfaceAssociation
    { iniaPublicIp :: !Text
    , iniaPublicDnsName :: !(Maybe Text)
    , iniaIpOwnerId :: !(Maybe Text)
    } deriving (Show)

data PlacementResponse = PlacementResponse
    { prAvailabilityZone :: !Text
    , prGroupName :: !(Maybe Text)
    , prTenancy :: !(Maybe Text)
    } deriving (Show)

data StateReason = StateReason
    { srCode :: !Text
    , srMessage :: !Text
    } deriving (Show)

data InstanceBlockDeviceMappingResponse = InstanceBlockDeviceMappingResponse
    { ibdmrItem :: ![InstanceBlockDeviceMappingResponseItem]
    } deriving (Show)

data InstanceBlockDeviceMappingResponseItem = InstanceBlockDeviceMappingResponseItem
    { ibdmriDeviceName :: !Text
  -- <xs:choice>
    , ibdmriEbs :: !EbsInstanceBlockDeviceMappingResponse
  -- </xs:choice>
    } deriving (Show)

data EbsInstanceBlockDeviceMappingResponse = EbsInstanceBlockDeviceMappingResponse
    { eibdmrVolumeId :: !Text
    , eibdmrStatus :: !Text
    , eibdmrAttachTime :: !DateTime
    , eibdmrDeleteOnTermination :: !(Maybe Boolean)
    } deriving (Show)

data InstanceLicenseResponse = InstanceLicenseResponse
    { ilrPool :: !Text
    } deriving (Show)

data DescribeAccountAttributes = DescribeAccountAttributes
    { daaAccountAttributeNameSet :: !(Maybe AccountAttributeNameSet)
    , daaFilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data DescribeAccountAttributesResponse = DescribeAccountAttributesResponse
    { daarRequestId :: !Text
    , daarAccountAttributeSet :: !(Maybe AccountAttributeSet)
    } deriving (Show)

data AccountAttributeNameSet = AccountAttributeNameSet
    { aansItem :: ![AccountAttributeNameSetItem]
    } deriving (Show)

data AccountAttributeNameSetItem = AccountAttributeNameSetItem
    { aansiAttributeName :: !Text
    } deriving (Show)

data AccountAttributeSet = AccountAttributeSet
    { aasItem :: ![AccountAttributeSetItem]
    } deriving (Show)

data AccountAttributeSetItem = AccountAttributeSetItem
    { aasiAttributeName :: !Text
    , aasiAttributeValueSet :: !AccountAttributeValueSet
    } deriving (Show)

data AccountAttributeValueSet = AccountAttributeValueSet
    { aavsItem :: ![AccountAttributeValueSetItem]
    } deriving (Show)

data AccountAttributeValueSetItem = AccountAttributeValueSetItem
    { aavsiAttributeValue :: !Text
    } deriving (Show)

data DescribeVpcAttribute = DescribeVpcAttribute
    { dvaVpcId :: !Text
    , dvaDescribeVpcAttributesGroup :: !DescribeVpcAttributesGroup
    } deriving (Show)

data DescribeVpcAttributesGroup = DescribeVpcAttributesGroup
    { dvagEnableDnsSupport :: !EmptyElement
    , dvagEnableDnsHostnames :: !EmptyElement
    } deriving (Show)
data DescribeVpcAttributeResponse = DescribeVpcAttributeResponse
    { dvarRequestId :: !Text
    , dvarVpcId :: !Text
  -- <xs:choice>
    , dvarEnableDnsSupport :: !(Maybe AttributeBooleanValue)
    , dvarEnableDnsHostnames :: !(Maybe AttributeBooleanValue)
  -- </xs:choice>
    } deriving (Show)

data ModifyVpcAttribute = ModifyVpcAttribute
    { mvaVpcId :: !Text
  -- <xs:choice>
    , mvaEnableDnsSupport :: !(Maybe AttributeBooleanValue)
    , mvaEnableDnsHostnames :: !(Maybe AttributeBooleanValue)
  -- </xs:choice>
    } deriving (Show)

data ModifyVpcAttributeResponse = ModifyVpcAttributeResponse
    { mvarRequestId :: !Text
    , mvarReturn :: !Boolean
    } deriving (Show)

data GetConsoleOutput = GetConsoleOutput
    { gcoInstanceId :: !Text
    } deriving (Show)

data GetConsoleOutputResponse = GetConsoleOutputResponse
    { gcorRequestId :: !Text
    , gcorInstanceId :: !Text
    , gcorTimestamp :: !DateTime
    , gcorOutput :: !Text
    } deriving (Show)

data GetPasswordData = GetPasswordData
    { gpdInstanceId :: !Text
    } deriving (Show)

data GetPasswordDataResponse = GetPasswordDataResponse
    { gpdrRequestId :: !Text
    , gpdrInstanceId :: !Text
    , gpdrTimestamp :: !DateTime
    , gpdrPasswordData :: !Text
    } deriving (Show)

data InstanceId = InstanceId
    { iiInstanceId :: !Text
    } deriving (Show)

data InstanceIdSet = InstanceIdSet
    { iisItem :: ![InstanceId]
    } deriving (Show)

data InstanceStateChange = InstanceStateChange
    { iscInstanceId :: !Text
    , iscCurrentState :: !InstanceState
    , iscPreviousState :: !InstanceState
    } deriving (Show)

data InstanceStateChangeSet = InstanceStateChangeSet
    { iscsItem :: ![InstanceStateChange]
    } deriving (Show)

data TerminateInstances = TerminateInstances
    { tiInstancesSet :: !InstanceIdSet
    } deriving (Show)

data TerminateInstancesResponse = TerminateInstancesResponse
    { tirRequestId :: !Text
    , tirInstancesSet :: !InstanceStateChangeSet
    } deriving (Show)

data InstanceBlockDeviceMapping = InstanceBlockDeviceMapping
    { ibdmItem :: ![InstanceBlockDeviceMappingItem]
    } deriving (Show)

data InstanceBlockDeviceMappingItem = InstanceBlockDeviceMappingItem
    { ibdmiDeviceName :: !Text
  -- <xs:choice>
    , ibdmiVirtualName :: !(Maybe Text)
    , ibdmiEbs :: !(Maybe InstanceEbsBlockDevice)
    , ibdmiNoDevice :: !(Maybe EmptyElement)
  -- </xs:choice>
    } deriving (Show)

data InstanceEbsBlockDevice = InstanceEbsBlockDevice
    { iebdVolumeId :: !Text
    , iebdDeleteOnTermination :: !(Maybe Boolean)
    } deriving (Show)

data StopInstances = StopInstances
    { siInstancesSet :: !InstanceIdSet
    , siForce :: !(Maybe Boolean)
    } deriving (Show)

data StopInstancesResponse = StopInstancesResponse
    { sirRequestId :: !Text
    , sirInstancesSet :: !InstanceStateChangeSet
    } deriving (Show)

data StartInstances = StartInstances
    { siInstancesSet :: !InstanceIdSet
    } deriving (Show)

data StartInstancesResponse = StartInstancesResponse
    { sirRequestId :: !Text
    , sirInstancesSet :: !InstanceStateChangeSet
    } deriving (Show)

data RebootInstances = RebootInstances
    { riInstancesSet :: !RebootInstancesInfo
    } deriving (Show)

data RebootInstancesInfo = RebootInstancesInfo
    { riiItem :: !(NonEmpty RebootInstancesItem)
    } deriving (Show)

data RebootInstancesItem = RebootInstancesItem
    { riiInstanceId :: !Text
    } deriving (Show)

data RebootInstancesResponse = RebootInstancesResponse
    { rirRequestId :: !Text
    , rirReturn :: !Boolean
    } deriving (Show)

data DescribeInstances = DescribeInstances
    { diInstancesSet :: !DescribeInstancesInfo
    , diFilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data DescribeInstancesInfo = DescribeInstancesInfo
    { diiItem :: ![DescribeInstancesItem]
    } deriving (Show)

data DescribeInstancesItem = DescribeInstancesItem
    { diiInstanceId :: !Text
    } deriving (Show)

data DescribeInstancesResponse = DescribeInstancesResponse
    { dirRequestId :: !Text
    , dirReservationSet :: !ReservationSet
    } deriving (Show)

data ReservationSet = ReservationSet
    { rsItem :: ![ReservationInfo]
    } deriving (Show)

data UnavailableResultSet = UnavailableResultSet
    { ursItem :: ![UnavailableResult]
    } deriving (Show)

data UnavailableResult = UnavailableResult
    { urAvailabilityZone :: !Text
    } deriving (Show)

data DescribeImages = DescribeImages
    { diExecutableBySet :: !(Maybe DescribeImagesExecutableBySet)
    , diImagesSet :: !DescribeImagesInfo
    , diOwnersSet :: !(Maybe DescribeImagesOwners)
    , diFilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data DescribeImagesInfo = DescribeImagesInfo
    { diiItem :: ![DescribeImagesItem]
    } deriving (Show)

data DescribeImagesItem = DescribeImagesItem
    { diiImageId :: !Text
    } deriving (Show)

data DescribeImagesOwners = DescribeImagesOwners
    { dioItem :: ![DescribeImagesOwner]
    } deriving (Show)

data DescribeImagesOwner = DescribeImagesOwner
    { dioOwner :: !Text
    } deriving (Show)

data DescribeImagesExecutableBySet = DescribeImagesExecutableBySet
    { diebsItem :: ![DescribeImagesExecutableBy]
    } deriving (Show)

data DescribeImagesExecutableBy = DescribeImagesExecutableBy
    { diebUser :: !Text
    } deriving (Show)

data DescribeImagesResponse = DescribeImagesResponse
    { dirRequestId :: !Text
    , dirImagesSet :: !DescribeImagesResponseInfo
    } deriving (Show)

data DescribeImagesResponseInfo = DescribeImagesResponseInfo
    { diriItem :: ![DescribeImagesResponseItem]
    } deriving (Show)

data DescribeImagesResponseItem = DescribeImagesResponseItem
    { diriImageId :: !Text
    , diriImageLocation :: !(Maybe Text)
    , diriImageState :: !Text
    , diriImageOwnerId :: !Text
    , diriIsPublic :: !Boolean
    , diriProductCodes :: !(Maybe ProductCodesSet)
    , diriArchitecture :: !(Maybe Text)
    , diriImage :: !(Maybe Text)
    , diriKernelId :: !(Maybe Text)
    , diriRamdiskId :: !(Maybe Text)
    , diriPlatform :: !(Maybe Text)
    , diriStateReason :: !(Maybe StateReason)
    , diriImageOwnerAlias :: !(Maybe Text)
    , diriName :: !(Maybe Text)
    , diriDescription :: !(Maybe Text)
    , diriRootDevice :: !(Maybe Text)
    , diriRootDeviceName :: !(Maybe Text)
    , diriBlockDeviceMapping :: !(Maybe BlockDeviceMapping)
    , diriVirtualization :: !(Maybe Text)
    , diriTagSet :: !(Maybe ResourceTagSet)
    , diriHypervisor :: !(Maybe Text)
    } deriving (Show)

data CreateSecurityGroup = CreateSecurityGroup
    { csgGroupName :: !Text
    , csgGroupDescription :: !Text
    , csgVpcId :: !(Maybe Text)
    } deriving (Show)

data CreateSecurityGroupResponse = CreateSecurityGroupResponse
    { csgrRequestId :: !Text
    , csgrReturn :: !Boolean
    , csgrGroupId :: !Text
    } deriving (Show)

data DeleteSecurityGroup = DeleteSecurityGroup
    {
-- <xs:choice>
      dsgGroupId :: !(Maybe Text)
    , dsgGroupName :: !(Maybe Text)
-- </xs:choice>
    } deriving (Show)

data DeleteSecurityGroupResponse = DeleteSecurityGroupResponse
    { dsgrRequestId :: !Text
    , dsgrReturn :: !Boolean
    } deriving (Show)

data DescribeSecurityGroups = DescribeSecurityGroups
    { dsgSecurityGroupSet :: !DescribeSecurityGroupsSet
    , dsgSecurityGroupIdSet :: !(Maybe DescribeSecurityGroupsIdSet)
    , dsgFilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data DescribeSecurityGroupsSet = DescribeSecurityGroupsSet
    { dsgsItem :: ![DescribeSecurityGroupsSetItem]
    } deriving (Show)

data DescribeSecurityGroupsSetItem = DescribeSecurityGroupsSetItem
    { dsgsiGroupName :: !Text
    } deriving (Show)

data DescribeSecurityGroupsIdSet = DescribeSecurityGroupsIdSet
    { dsgisItem :: ![DescribeSecurityGroupsIdSetItem]
    } deriving (Show)

data DescribeSecurityGroupsIdSetItem = DescribeSecurityGroupsIdSetItem
    { dsgisiGroupId :: !Text
    } deriving (Show)

data DescribeSecurityGroupsResponse = DescribeSecurityGroupsResponse
    { dsgrRequestId :: !Text
    , dsgrSecurityGroupInfo :: !SecurityGroupSet
    } deriving (Show)

data IpPermissionSet = IpPermissionSet
    { ipsItem :: ![IpPermission]
    } deriving (Show)

data IpPermission = IpPermission
    { ipIpProtocol :: !Text
    , ipFromPort :: !(Maybe Int)
    , ipToPort :: !(Maybe Int)
    , ipGroups :: !UserIdGroupPairSet
    , ipIpRanges :: !IpRangeSet
    } deriving (Show)

data IpRangeSet = IpRangeSet
    { irsItem :: ![IpRangeItem]
    } deriving (Show)

data IpRangeItem = IpRangeItem
    { iriCidrIp :: !Text
    } deriving (Show)

data UserIdGroupPairSet = UserIdGroupPairSet
    { uigpsItem :: ![UserIdGroupPair]
    } deriving (Show)

data UserIdGroupPair = UserIdGroupPair
    { uigpUserId :: !(Maybe Text)
    , uigpGroupId :: !(Maybe Text)
    , uigpGroupName :: !(Maybe Text)
    } deriving (Show)

data SecurityGroupSet = SecurityGroupSet
    { sgsItem :: ![SecurityGroupItem]
    } deriving (Show)

data SecurityGroupItem = SecurityGroupItem
    { sgiOwnerId :: !Text
    , sgiGroupId :: !Text
    , sgiGroupName :: !Text
    , sgiGroupDescription :: !Text
    , sgiVpcId :: !(Maybe Text)
    , sgiIpPermissions :: !IpPermissionSet
    , sgiIpPermissionsEgress :: !(Maybe IpPermissionSet)
    , sgiTagSet :: !(Maybe ResourceTagSet)
    } deriving (Show)

data AuthorizeSecurityGroupIngress = AuthorizeSecurityGroupIngress
    { asgiUserId :: !(Maybe Text)
  -- <xs:choice>
    , asgiGroupId :: !(Maybe Text)
    , asgiGroupName :: !(Maybe Text)
  -- </xs:choice>
    , asgiIpPermissions :: !IpPermissionSet
    } deriving (Show)

data AuthorizeSecurityGroupIngressResponse = AuthorizeSecurityGroupIngressResponse
    { asgirRequestId :: !Text
    , asgirReturn :: !Boolean
    } deriving (Show)

data RevokeSecurityGroupIngress = RevokeSecurityGroupIngress
    { rsgiUserId :: !(Maybe Text)
  -- <xs:choice>
    , rsgiGroupId :: !(Maybe Text)
    , rsgiGroupName :: !(Maybe Text)
  -- </xs:choice>
    , rsgiIpPermissions :: !IpPermissionSet
    } deriving (Show)

data RevokeSecurityGroupIngressResponse = RevokeSecurityGroupIngressResponse
    { rsgirRequestId :: !Text
    , rsgirReturn :: !Boolean
    } deriving (Show)

data AuthorizeSecurityGroupEgress = AuthorizeSecurityGroupEgress
    { asgeGroupId :: !Text
    , asgeIpPermissions :: !IpPermissionSet
    } deriving (Show)

data AuthorizeSecurityGroupEgressResponse = AuthorizeSecurityGroupEgressResponse
    { asgerRequestId :: !Text
    , asgerReturn :: !Boolean
    } deriving (Show)

data RevokeSecurityGroupEgress = RevokeSecurityGroupEgress
    { rsgeGroupId :: !Text
    , rsgeIpPermissions :: !IpPermissionSet
    } deriving (Show)

data RevokeSecurityGroupEgressResponse = RevokeSecurityGroupEgressResponse
    { rsgerRequestId :: !Text
    , rsgerReturn :: !Boolean
    } deriving (Show)

data InstanceState = InstanceState
    { isCode :: !Int
    , isName :: !Text
    } deriving (Show)

data ModifyInstanceAttribute = ModifyInstanceAttribute
    { miaInstanceId :: !Text
  -- <xs:choice>
    , miaInstance :: !(Maybe AttributeValue)
    , miaKernel :: !(Maybe AttributeValue)
    , miaRamdisk :: !(Maybe AttributeValue)
    , miaUserData :: !(Maybe AttributeValue)
    , miaDisableApiTermination :: !(Maybe AttributeBooleanValue)
    , miaInstanceInitiatedShutdownBehavior :: !(Maybe AttributeValue)
    , miaBlockDeviceMapping :: !(Maybe InstanceBlockDeviceMapping)
    , miaSourceDestCheck :: !(Maybe AttributeBooleanValue)
    , miaGroupSet :: !(Maybe SecurityGroupIdSet)
    , miaEbsOptimized :: !(Maybe AttributeBooleanValue)
  -- </xs:choice>
    } deriving (Show)

data SecurityGroupIdSet = SecurityGroupIdSet
    { sgisItem :: ![SecurityGroupIdSetItem]
    } deriving (Show)

data SecurityGroupIdSetItem = SecurityGroupIdSetItem
    { sgisiGroupId :: !Text
    } deriving (Show)

data ModifyInstanceAttributeResponse = ModifyInstanceAttributeResponse
    { miarRequestId :: !Text
    , miarReturn :: !Boolean
    } deriving (Show)

data ResetInstanceAttribute = ResetInstanceAttribute
    { riaInstanceId :: !Text
    , riaResetInstanceAttributesGroup :: !ResetInstanceAttributesGroup
    } deriving (Show)

data ResetInstanceAttributesGroup = ResetInstanceAttributesGroup
    { riagKernel :: !EmptyElement
    , riagRamdisk :: !EmptyElement
    , riagSourceDestCheck :: !EmptyElement
    } deriving (Show)
data ResetInstanceAttributeResponse = ResetInstanceAttributeResponse
    { riarRequestId :: !Text
    , riarReturn :: !Boolean
    } deriving (Show)

data DescribeInstanceAttribute = DescribeInstanceAttribute
    { diaInstanceId :: !Text
    , diaDescribeInstanceAttributesGroup :: !DescribeInstanceAttributesGroup
    } deriving (Show)

data DescribeInstanceAttributesGroup = DescribeInstanceAttributesGroup
    { diagInstance :: !EmptyElement
    , diagKernel :: !EmptyElement
    , diagRamdisk :: !EmptyElement
    , diagUserData :: !EmptyElement
    , diagDisableApiTermination :: !EmptyElement
    , diagInstanceInitiatedShutdownBehavior :: !EmptyElement
    , diagRootDeviceName :: !EmptyElement
    , diagBlockDeviceMapping :: !EmptyElement
    , diagSourceDestCheck :: !EmptyElement
    , diagGroupSet :: !EmptyElement
    , diagProductCodes :: !EmptyElement
    , diagEbsOptimized :: !EmptyElement
    } deriving (Show)
data DescribeInstanceAttributeResponse = DescribeInstanceAttributeResponse
    { diarRequestId :: !Text
    , diarInstanceId :: !Text
  -- <xs:choice>
    , diarInstance :: !(Maybe NullableAttributeValue)
    , diarKernel :: !(Maybe NullableAttributeValue)
    , diarRamdisk :: !(Maybe NullableAttributeValue)
    , diarUserData :: !(Maybe NullableAttributeValue)
    , diarDisableApiTermination :: !(Maybe NullableAttributeBooleanValue)
    , diarInstanceInitiatedShutdownBehavior :: !(Maybe NullableAttributeValue)
    , diarRootDeviceName :: !(Maybe NullableAttributeValue)
    , diarBlockDeviceMapping :: !(Maybe InstanceBlockDeviceMappingResponse)
    , diarSourceDestCheck :: !(Maybe NullableAttributeBooleanValue)
    , diarGroupSet :: !(Maybe GroupSet)
    , diarProductCodes :: !(Maybe ProductCodesSet)
    , diarEbsOptimized :: !(Maybe NullableAttributeBooleanValue)
  -- </xs:choice>
    } deriving (Show)

data ModifyImageAttribute = ModifyImageAttribute
    { miaImageId :: !Text
  -- <xs:choice>
    , miaLaunchPermission :: !(Maybe LaunchPermissionOperation)
    , miaProductCodes :: !(Maybe ProductCodeList)
    , miaDescription :: !(Maybe AttributeValue)
  -- </xs:choice>
    } deriving (Show)

data LaunchPermissionOperation = LaunchPermissionOperation
    { lpo 
-- <xs:choice>
    , lpoAdd :: !(Maybe LaunchPermissionList)
    , lpoRemove :: !(Maybe LaunchPermissionList)
-- </xs:choice>
    } deriving (Show)

data LaunchPermissionList = LaunchPermissionList
    { lplItem :: ![LaunchPermissionItem]
    } deriving (Show)

data LaunchPermissionItem = LaunchPermissionItem
    { lpi 
-- <xs:choice>
    , lpiUserId :: !(Maybe Text)
    , lpiGroup :: !(Maybe Text)
-- </xs:choice>
    } deriving (Show)

data ProductCodeList = ProductCodeList
    { pclItem :: ![ProductCodeItem]
    } deriving (Show)

data ProductCodeItem = ProductCodeItem
    { pciProductCode :: !Text
    } deriving (Show)

data ModifyImageAttributeResponse = ModifyImageAttributeResponse
    { miarRequestId :: !Text
    , miarReturn :: !Boolean
    } deriving (Show)

data ResetImageAttribute = ResetImageAttribute
    { riaImageId :: !Text
    , riaResetImageAttributesGroup :: !ResetImageAttributesGroup
    } deriving (Show)

data ResetImageAttributesGroup = ResetImageAttributesGroup
    { riagLaunchPermission :: !EmptyElement
    } deriving (Show)

data EmptyElement = EmptyElement

data ResetImageAttributeResponse = ResetImageAttributeResponse
    { riarRequestId :: !Text
    , riarReturn :: !Boolean
    } deriving (Show)

data DescribeImageAttribute = DescribeImageAttribute
    { diaImageId :: !Text
    , diaDescribeImageAttributesGroup :: !DescribeImageAttributesGroup
    } deriving (Show)

data DescribeImageAttributesGroup = DescribeImageAttributesGroup
    { diagLaunchPermission :: !EmptyElement
    , diagProductCodes :: !EmptyElement
    , diagKernel :: !EmptyElement
    , diagRamdisk :: !EmptyElement
    , diagBlockDeviceMapping :: !EmptyElement
    , diagDescription :: !EmptyElement
    , diagInstanceCategory :: !EmptyElement
    } deriving (Show)
data DescribeImageAttributeResponse = DescribeImageAttributeResponse
    { diarRequestId :: !Text
    , diarImageId :: !Text
  -- <xs:choice>
    , diarLaunchPermission :: !(Maybe LaunchPermissionList)
    , diarProductCodes :: !(Maybe ProductCodesSet)
    , diarKernel :: !(Maybe NullableAttributeValue)
    , diarRamdisk :: !(Maybe NullableAttributeValue)
    , diarDescription :: !(Maybe NullableAttributeValue)
    , diarBlockDeviceMapping :: !(Maybe BlockDeviceMapping)
  -- </xs:choice>
    } deriving (Show)

data NullableAttributeValue = NullableAttributeValue
    { navValue :: !(Maybe Text)
    } deriving (Show)

data NullableAttributeBooleanValue = NullableAttributeBooleanValue
    { nabvValue :: !(Maybe Boolean)
    } deriving (Show)

data AttributeValue = AttributeValue
    { avValue :: !Text
    } deriving (Show)

data AttributeBooleanValue = AttributeBooleanValue
    { abvValue :: !Boolean
    } deriving (Show)

data ConfirmProductInstance = ConfirmProductInstance
    { cpiProductCode :: !Text
    , cpiInstanceId :: !Text
    } deriving (Show)

data ProductCodesSet = ProductCodesSet
    { pcsItem :: ![ProductCodesSetItem]
    } deriving (Show)

data ProductCodesSetItem = ProductCodesSetItem
    { pcsiProductCode :: !Text
    , pcsiType :: !Text
    } deriving (Show)

data ConfirmProductInstanceResponse = ConfirmProductInstanceResponse
    { cpirRequestId :: !Text
    , cpirReturn :: !Boolean
    , cpirOwnerId :: !(Maybe Text)
    } deriving (Show)

data DescribeAvailabilityZones = DescribeAvailabilityZones
    { dazAvailabilityZoneSet :: !DescribeAvailabilityZonesSet
    , dazFilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data DescribeAvailabilityZonesSet = DescribeAvailabilityZonesSet
    { dazsItem :: ![DescribeAvailabilityZonesSetItem]
    } deriving (Show)

data DescribeAvailabilityZonesSetItem = DescribeAvailabilityZonesSetItem
    { dazsiZoneName :: !Text
    } deriving (Show)

data DescribeAvailabilityZonesResponse = DescribeAvailabilityZonesResponse
    { dazrRequestId :: !Text
    , dazrAvailabilityZoneInfo :: !AvailabilityZoneSet
    } deriving (Show)

data AvailabilityZoneSet = AvailabilityZoneSet
    { azsItem :: ![AvailabilityZoneItem]
    } deriving (Show)

data AvailabilityZoneMessage = AvailabilityZoneMessage
    { azmMessage :: !Text
    } deriving (Show)

data AvailabilityZoneMessageSet = AvailabilityZoneMessageSet
    { azmsItem :: ![AvailabilityZoneMessage]
    } deriving (Show)

data AvailabilityZoneItem = AvailabilityZoneItem
    { aziZoneName :: !Text
    , aziZoneState :: !Text
    , aziRegionName :: !Text
    , aziMessageSet :: !AvailabilityZoneMessageSet
    } deriving (Show)

data AllocateAddress = AllocateAddress
    { aaDomain :: !(Maybe Text)
    } deriving (Show)

data AllocateAddressResponse = AllocateAddressResponse
    { aarRequestId :: !Text
    , aarPublicIp :: !Text
    , aarDomain :: !Text
    , aarAllocationId :: !(Maybe Text)
    } deriving (Show)

data ReleaseAddress = ReleaseAddress
    { ra 
  -- <xs:choice>
    , raPublicIp :: !(Maybe Text)
    , raAllocationId :: !(Maybe Text)
  -- </xs:choice>
    } deriving (Show)

data ReleaseAddressResponse = ReleaseAddressResponse
    { rarRequestId :: !Text
    , rarReturn :: !Boolean
    } deriving (Show)

data DescribeAddresses = DescribeAddresses
    { daPublicIpsSet :: !DescribeAddressesInfo
    , daAllocationIdsSet :: !AllocationIdSet
    , daFilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data AllocationIdSet = AllocationIdSet
    { aisItem :: ![AllocationIdSetItem]
    } deriving (Show)

data AllocationIdSetItem = AllocationIdSetItem
    { aisiAllocationId :: !Text
    } deriving (Show)

data DescribeAddressesInfo = DescribeAddressesInfo
    { daiItem :: ![DescribeAddressesItem]
    } deriving (Show)

data DescribeAddressesItem = DescribeAddressesItem
    { daiPublicIp :: !Text
    } deriving (Show)

data DescribeAddressesResponse = DescribeAddressesResponse
    { darRequestId :: !Text
    , darAddressesSet :: !DescribeAddressesResponseInfo
    } deriving (Show)

data DescribeAddressesResponseInfo = DescribeAddressesResponseInfo
    { dariItem :: ![DescribeAddressesResponseItem]
    } deriving (Show)

data DescribeAddressesResponseItem = DescribeAddressesResponseItem
    { dariPublicIp :: !Text
    , dariAllocationId :: !(Maybe Text)
    , dariDomain :: !Text
    , dariInstanceId :: !(Maybe Text)
    , dariAssociationId :: !(Maybe Text)
    , dariNetworkInterfaceId :: !(Maybe Text)
    , dariNetworkInterfaceOwnerId :: !(Maybe Text)
    , dariPrivateIpAddress :: !(Maybe Text)
    } deriving (Show)

data AssociateAddress = AssociateAddress
    { aa 
  -- <xs:choice>
    , aaPublicIp :: !(Maybe Text)
    , aaAllocationId :: !(Maybe Text)
  -- </xs:choice>
  -- <xs:choice>
    , aaNetworkInterfaceId :: !(Maybe Text)
    , aaInstanceId :: !(Maybe Text)
  -- </xs:choice>
    , aaPrivateIpAddress :: !(Maybe Text)
    , aaAllowReassociation :: !(Maybe Boolean)
    } deriving (Show)

data AssociateAddressResponse = AssociateAddressResponse
    { aarRequestId :: !Text
    , aarReturn :: !Boolean
    , aarAssociationId :: !(Maybe Text)
    } deriving (Show)

data DisassociateAddress = DisassociateAddress
    { da 
-- <xs:choice>
    , daPublicIp :: !(Maybe Text)
    , daAssociationId :: !(Maybe Text)
-- </xs:choice>
     } deriving (Show)

data DisassociateAddressResponse = DisassociateAddressResponse
    { darRequestId :: !Text
    , darReturn :: !Boolean
    } deriving (Show)

data CreateVolume = CreateVolume
    { cvSize :: !(Maybe Text)
    , cvSnapshotId :: !(Maybe Text)
    , cvAvailabilityZone :: !Text
    , cvVolume :: !(Maybe Text)
    , cvIops :: !(Maybe Int)
    } deriving (Show)

data CreateVolumeResponse = CreateVolumeResponse
    { cvrRequestId :: !Text
    , cvrVolumeId :: !Text
    , cvrSize :: !Text
    , cvrSnapshotId :: !Text
    , cvrAvailabilityZone :: !Text
    , cvrStatus :: !Text
    , cvrCreateTime :: !DateTime
    , cvrVolume :: !Text
    , cvrIops :: !(Maybe Int)
    } deriving (Show)

data DeleteVolume = DeleteVolume
    { dvVolumeId :: !Text
    } deriving (Show)

data DeleteVolumeResponse = DeleteVolumeResponse
    { dvrRequestId :: !Text
    , dvrReturn :: !Boolean
    } deriving (Show)

data DescribeVolumes = DescribeVolumes
    { dvVolumeSet :: !DescribeVolumesSet
    , dvFilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data DescribeVolumesSet = DescribeVolumesSet
    { dvsItem :: ![DescribeVolumesSetItem]
    } deriving (Show)

data DescribeVolumesSetItem = DescribeVolumesSetItem
    { dvsiVolumeId :: !Text
    } deriving (Show)

data DescribeVolumesResponse = DescribeVolumesResponse
    { dvrRequestId :: !Text
    , dvrVolumeSet :: !DescribeVolumesSetResponse
    } deriving (Show)

data DescribeVolumesSetResponse = DescribeVolumesSetResponse
    { dvsrItem :: ![DescribeVolumesSetItemResponse]
    } deriving (Show)

data DescribeVolumesSetItemResponse = DescribeVolumesSetItemResponse
    { dvsirVolumeId :: !Text
    , dvsirSize :: !Text
    , dvsirSnapshotId :: !Text
    , dvsirAvailabilityZone :: !Text
    , dvsirStatus :: !Text
    , dvsirCreateTime :: !DateTime
    , dvsirAttachmentSet :: !AttachmentSetResponse
    , dvsirTagSet :: !(Maybe ResourceTagSet)
    , dvsirVolume :: !Text
    , dvsirIops :: !(Maybe Int)
    } deriving (Show)

data AttachmentSetResponse = AttachmentSetResponse
    { asrItem :: ![AttachmentSetItemResponse]
    } deriving (Show)

data AttachmentSetItemResponse = AttachmentSetItemResponse
    { asirVolumeId :: !Text
    , asirInstanceId :: !Text
    , asirDevice :: !Text
    , asirStatus :: !Text
    , asirAttachTime :: !DateTime
    , asirDeleteOnTermination :: !Boolean
    } deriving (Show)

data AttachVolume = AttachVolume
    { avVolumeId :: !Text
    , avInstanceId :: !Text
    , avDevice :: !Text
    } deriving (Show)

data AttachVolumeResponse = AttachVolumeResponse
    { avrRequestId :: !Text
    , avrVolumeId :: !Text
    , avrInstanceId :: !Text
    , avrDevice :: !Text
    , avrStatus :: !Text
    , avrAttachTime :: !DateTime
    } deriving (Show)

data DetachVolume = DetachVolume
    { dvVolumeId :: !Text
    , dvInstanceId :: !(Maybe Text)
    , dvDevice :: !(Maybe Text)
    , dvForce :: !(Maybe Boolean)
    } deriving (Show)

data DetachVolumeResponse = DetachVolumeResponse
    { dvrRequestId :: !Text
    , dvrVolumeId :: !Text
    , dvrInstanceId :: !Text
    , dvrDevice :: !Text
    , dvrStatus :: !Text
    , dvrAttachTime :: !DateTime
    } deriving (Show)

data CreateSnapshot = CreateSnapshot
    { csVolumeId :: !Text
    , csDescription :: !(Maybe Text)
    } deriving (Show)

data CreateSnapshotResponse = CreateSnapshotResponse
    { csrRequestId :: !Text
    , csrSnapshotId :: !Text
    , csrVolumeId :: !Text
    , csrStatus :: !Text
    , csrStartTime :: !DateTime
    , csrProgress :: !Text
    , csrOwnerId :: !Text
    , csrVolumeSize :: !Text
    , csrDescription :: !(Maybe Text)
    } deriving (Show)

data CopySnapshot = CopySnapshot
    { csSourceRegion :: !Text
    , csSourceSnapshotId :: !Text
    , csDescription :: !(Maybe Text)
    } deriving (Show)

data CopySnapshotResponse = CopySnapshotResponse
    { csrRequestId :: !Text
    , csrSnapshotId :: !Text
    } deriving (Show)

data DeleteSnapshot = DeleteSnapshot
    { dsSnapshotId :: !Text
    } deriving (Show)

data DeleteSnapshotResponse = DeleteSnapshotResponse
    { dsrRequestId :: !Text
    , dsrReturn :: !Boolean
    } deriving (Show)

data DescribeSnapshots = DescribeSnapshots
    { dsSnapshotSet :: !DescribeSnapshotsSet
    , dsOwnersSet :: !(Maybe DescribeSnapshotsOwners)
    , dsRestorableBySet :: !(Maybe DescribeSnapshotsRestorableBySet)
    , dsFilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data DescribeSnapshotsSet = DescribeSnapshotsSet
    { dssItem :: ![DescribeSnapshotsSetItem]
    } deriving (Show)

data DescribeSnapshotsSetItem = DescribeSnapshotsSetItem
    { dssiSnapshotId :: !Text
    } deriving (Show)

data DescribeSnapshotsOwners = DescribeSnapshotsOwners
    { dsoItem :: ![DescribeSnapshotsOwner]
    } deriving (Show)

data DescribeSnapshotsOwner = DescribeSnapshotsOwner
    { dsoOwner :: !Text
    } deriving (Show)

data DescribeSnapshotsRestorableBySet = DescribeSnapshotsRestorableBySet
    { dsrbsItem :: ![DescribeSnapshotsRestorableBy]
    } deriving (Show)

data DescribeSnapshotsRestorableBy = DescribeSnapshotsRestorableBy
    { dsrbUser :: !Text
    } deriving (Show)

data DescribeSnapshotsResponse = DescribeSnapshotsResponse
    { dsrRequestId :: !Text
    , dsrSnapshotSet :: !DescribeSnapshotsSetResponse
    } deriving (Show)

data DescribeSnapshotsSetResponse = DescribeSnapshotsSetResponse
    { dssrItem :: ![DescribeSnapshotsSetItemResponse]
    } deriving (Show)

data DescribeSnapshotsSetItemResponse = DescribeSnapshotsSetItemResponse
    { dssirSnapshotId :: !Text
    , dssirVolumeId :: !Text
    , dssirStatus :: !Text
    , dssirStartTime :: !DateTime
    , dssirProgress :: !Text
    , dssirOwnerId :: !Text
    , dssirVolumeSize :: !Text
    , dssirDescription :: !(Maybe Text)
    , dssirOwnerAlias :: !(Maybe Text)
    , dssirTagSet :: !(Maybe ResourceTagSet)
    } deriving (Show)

data ModifySnapshotAttribute = ModifySnapshotAttribute
    { msaSnapshotId :: !Text
    , msaCreateVolumePermission :: !CreateVolumePermissionOperation
    } deriving (Show)

data CreateVolumePermissionOperation = CreateVolumePermissionOperation
    { cvpo 
-- <xs:choice>
    , cvpoAdd :: !(Maybe CreateVolumePermissionList)
    , cvpoRemove :: !(Maybe CreateVolumePermissionList)
-- </xs:choice>
    } deriving (Show)

data CreateVolumePermissionList = CreateVolumePermissionList
    { cvplItem :: ![CreateVolumePermissionItem]
    } deriving (Show)

data CreateVolumePermissionItem = CreateVolumePermissionItem
    { cvpi 
-- <xs:choice>
    , cvpiUserId :: !(Maybe Text)
    , cvpiGroup :: !(Maybe Text)
-- </xs:choice>
    } deriving (Show)

data ModifySnapshotAttributeResponse = ModifySnapshotAttributeResponse
    { msarRequestId :: !Text
    , msarReturn :: !Boolean
    } deriving (Show)

data ResetSnapshotAttribute = ResetSnapshotAttribute
    { rsaSnapshotId :: !Text
    , rsaResetSnapshotAttributesGroup :: !ResetSnapshotAttributesGroup
    } deriving (Show)

data ResetSnapshotAttributesGroup = ResetSnapshotAttributesGroup
    { rsagCreateVolumePermission :: !EmptyElement
    } deriving (Show)
data ResetSnapshotAttributeResponse = ResetSnapshotAttributeResponse
    { rsarRequestId :: !Text
    , rsarReturn :: !Boolean
    } deriving (Show)

data DescribeSnapshotAttribute = DescribeSnapshotAttribute
    { dsaSnapshotId :: !Text
    , dsaDescribeSnapshotAttributesGroup :: !DescribeSnapshotAttributesGroup
    } deriving (Show)

data DescribeSnapshotAttributesGroup = DescribeSnapshotAttributesGroup
    { dsagCreateVolumePermission :: !EmptyElement
    , dsagProductCodes :: !EmptyElement
    } deriving (Show)
data DescribeSnapshotAttributeResponse = DescribeSnapshotAttributeResponse
    { dsarRequestId :: !Text
    , dsarSnapshotId :: !Text
  -- <xs:choice>
    , dsarCreateVolumePermission :: !(Maybe CreateVolumePermissionList)
    , dsarProductCodes :: !(Maybe ProductCodesSet)
  -- </xs:choice>
    } deriving (Show)

data BundleInstance = BundleInstance
    { biInstanceId :: !Text
    , biStorage :: !BundleInstanceTaskStorage
    } deriving (Show)

data BundleInstanceTaskStorage = BundleInstanceTaskStorage
    { bitsS3 :: !BundleInstanceS3Storage
    } deriving (Show)

data BundleInstanceS3Storage = BundleInstanceS3Storage
    { bis3sBucket :: !Text
    , bis3sPrefix :: !Text
    , bis3sAwsAccessKeyId :: !(Maybe Text)
    , bis3sUploadPolicy :: !(Maybe Text)
    , bis3sUploadPolicySignature :: !(Maybe Text)
    } deriving (Show)

data BundleInstanceResponse = BundleInstanceResponse
    { birRequestId :: !Text
    , birBundleInstanceTask :: !BundleInstanceTask
    } deriving (Show)

data BundleInstanceTask = BundleInstanceTask
    { bitInstanceId :: !Text
    , bitBundleId :: !Text
    , bitState :: !Text
    , bitStartTime :: !DateTime
    , bitUpdateTime :: !DateTime
    , bitStorage :: !BundleInstanceTaskStorage
    , bitProgress :: !(Maybe Text)
    , bitError :: !(Maybe BundleInstanceTaskError)
    } deriving (Show)

data BundleInstanceTaskError = BundleInstanceTaskError
    { biteCode :: !Text
    , biteMessage :: !Text
    } deriving (Show)

data DescribeBundleTasks = DescribeBundleTasks
    { dbtBundlesSet :: !DescribeBundleTasksInfo
    , dbtFilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data DescribeBundleTasksInfo = DescribeBundleTasksInfo
    { dbtiItem :: ![DescribeBundleTasksItem]
    } deriving (Show)

data DescribeBundleTasksItem = DescribeBundleTasksItem
    { dbtiBundleId :: !Text
    } deriving (Show)

data DescribeBundleTasksResponse = DescribeBundleTasksResponse
    { dbtrRequestId :: !Text
    , dbtrBundleInstanceTasksSet :: !BundleInstanceTasksSet
    } deriving (Show)

data BundleInstanceTasksSet = BundleInstanceTasksSet
    { bitsItem :: ![BundleInstanceTask]
    } deriving (Show)

data CancelBundleTask = CancelBundleTask
    { cbtBundleId :: !Text
    } deriving (Show)

data CancelBundleTaskResponse = CancelBundleTaskResponse
    { cbtrRequestId :: !Text
    , cbtrBundleInstanceTask :: !BundleInstanceTask
    } deriving (Show)

data CopyImage = CopyImage
    { ciSourceRegion :: !Text
    , ciSourceImageId :: !Text
    , ciName :: !Text
    , ciDescription :: !(Maybe Text)
    , ciClientToken :: !(Maybe Text)
    } deriving (Show)

data CopyImageResponse = CopyImageResponse
    { cirRequestId :: !Text
    , cirImageId :: !Text
    } deriving (Show)

data DescribeRegions = DescribeRegions
    { drRegionSet :: !DescribeRegionsSet
    , drFilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data DescribeRegionsSet = DescribeRegionsSet
    { drsItem :: ![DescribeRegionsSetItem]
    } deriving (Show)

data DescribeRegionsSetItem = DescribeRegionsSetItem
    { drsiRegionName :: !Text
    } deriving (Show)

data DescribeRegionsResponse = DescribeRegionsResponse
    { drrRequestId :: !Text
    , drrRegionInfo :: !RegionSet
    } deriving (Show)

data RegionSet = RegionSet
    { rsItem :: ![RegionItem]
    } deriving (Show)

data RegionItem = RegionItem
    { riRegionName :: !Text
    , riRegionEndpoint :: !Text
    } deriving (Show)

data DescribeReservedInstancesOfferings = DescribeReservedInstancesOfferings
    { drioReservedInstancesOfferingsSet :: !(Maybe DescribeReservedInstancesOfferingsSet)
    , drioInstance :: !(Maybe Text)
    , drioAvailabilityZone :: !(Maybe Text)
    , drioProductDescription :: !(Maybe Text)
    , drioFilterSet :: !(Maybe FilterSet)
    , drioInstanceTenancy :: !(Maybe Text)
    , drioOffering :: !(Maybe Text)
    , drioIncludeMarketplace :: !(Maybe Boolean)
    , drioMinDuration :: !(Maybe Long)
    , drioMaxDuration :: !(Maybe Long)
    , drioMaxInstanceCount :: !(Maybe Int)
    , drioNextToken :: !(Maybe Text)
    , drioMaxResults :: !(Maybe Int)
    } deriving (Show)

data DescribeReservedInstancesOfferingsSet = DescribeReservedInstancesOfferingsSet
    { driosItem :: ![DescribeReservedInstancesOfferingsSetItem]
    } deriving (Show)

data DescribeReservedInstancesOfferingsSetItem = DescribeReservedInstancesOfferingsSetItem
    { driosiReservedInstancesOfferingId :: !Text
    } deriving (Show)

data DescribeReservedInstancesOfferingsResponse = DescribeReservedInstancesOfferingsResponse
    { driorRequestId :: !Text
    , driorReservedInstancesOfferingsSet :: !DescribeReservedInstancesOfferingsResponseSet
    , driorNextToken :: !(Maybe Text)
    } deriving (Show)

data DescribeReservedInstancesOfferingsResponseSet = DescribeReservedInstancesOfferingsResponseSet
    { driorsItem :: ![DescribeReservedInstancesOfferingsResponseSetItem]
    } deriving (Show)

data DescribeReservedInstancesOfferingsResponseSetItem = DescribeReservedInstancesOfferingsResponseSetItem
    { driorsiReservedInstancesOfferingId :: !Text
    , driorsiInstance :: !Text
    , driorsiAvailabilityZone :: !Text
    , driorsiDuration :: !Long
    , driorsiFixedPrice :: !Double
    , driorsiUsagePrice :: !Double
    , driorsiProductDescription :: !Text
    , driorsiInstanceTenancy :: !Text
    , driorsiCurrencyCode :: !Text
    , driorsiOffering :: !Text
    , driorsiRecurringCharges :: !RecurringChargesSet
    , driorsiMarketplace :: !(Maybe Boolean)
    , driorsiPricingDetailsSet :: !(Maybe PricingDetailsSet)
    } deriving (Show)

data RecurringChargesSet = RecurringChargesSet
    { rcsItem :: ![RecurringChargesSetItem]
    } deriving (Show)

data RecurringChargesSetItem = RecurringChargesSetItem
    { rcsiFrequency :: !Text
    , rcsiAmount :: !Double
    } deriving (Show)

data PricingDetailsSet = PricingDetailsSet
    { pdsItem :: ![PricingDetailsSetItem]
    } deriving (Show)

data PricingDetailsSetItem = PricingDetailsSetItem
    { pdsiPrice :: !Double
    , pdsiCount :: !Int
    } deriving (Show)

data PurchaseReservedInstancesOffering = PurchaseReservedInstancesOffering
    { prioReservedInstancesOfferingId :: !Text
    , prioInstanceCount :: !Int
    , prioLimitPrice :: !(Maybe ReservedInstanceLimitPrice)
    } deriving (Show)

data ReservedInstanceLimitPrice = ReservedInstanceLimitPrice
    { rilpAmount :: !Double
    , rilpCurrencyCode :: !(Maybe Text)
    } deriving (Show)

data PurchaseReservedInstancesOfferingResponse = PurchaseReservedInstancesOfferingResponse
    { priorRequestId :: !Text
    , priorReservedInstancesId :: !Text
    } deriving (Show)

data DescribeReservedInstances = DescribeReservedInstances
    { driReservedInstancesSet :: !(Maybe DescribeReservedInstancesSet)
    , driFilterSet :: !(Maybe FilterSet)
    , driOffering :: !(Maybe Text)
    } deriving (Show)

data DescribeReservedInstancesSet = DescribeReservedInstancesSet
    { drisItem :: ![DescribeReservedInstancesSetItem]
    } deriving (Show)

data DescribeReservedInstancesSetItem = DescribeReservedInstancesSetItem
    { drisiReservedInstancesId :: !Text
    } deriving (Show)

data DescribeReservedInstancesResponse = DescribeReservedInstancesResponse
    { drirRequestId :: !Text
    , drirReservedInstancesSet :: !DescribeReservedInstancesResponseSet
    } deriving (Show)

data DescribeReservedInstancesResponseSet = DescribeReservedInstancesResponseSet
    { drirsItem :: ![DescribeReservedInstancesResponseSetItem]
    } deriving (Show)

data DescribeReservedInstancesResponseSetItem = DescribeReservedInstancesResponseSetItem
    { drirsiReservedInstancesId :: !Text
    , drirsiInstance :: !Text
    , drirsiAvailabilityZone :: !Text
    , drirsiStart :: !DateTime
    , drirsiDuration :: !Long
    , drirsiFixedPrice :: !Double
    , drirsiUsagePrice :: !Double
    , drirsiInstanceCount :: !Integer
    , drirsiProductDescription :: !Text
    , drirsiState :: !Text
    , drirsiTagSet :: !(Maybe ResourceTagSet)
    , drirsiInstanceTenancy :: !Text
    , drirsiCurrencyCode :: !Text
    , drirsiOffering :: !Text
    , drirsiRecurringCharges :: !(Maybe RecurringChargesSet)
    } deriving (Show)

data CreateReservedInstancesListing = CreateReservedInstancesListing
    { crilReservedInstancesId :: !Text
    , crilInstanceCount :: !(Maybe Int)
    , crilPriceSchedules :: !PriceScheduleRequestSet
    , crilClientToken :: !Text
    } deriving (Show)

data PriceScheduleRequestSet = PriceScheduleRequestSet
    { psrsItem :: ![PriceScheduleRequestSetItem]
    } deriving (Show)

data PriceScheduleRequestSetItem = PriceScheduleRequestSetItem
    { psrsiTerm :: !Long
    , psrsiPrice :: !Double
    , psrsiCurrencyCode :: !(Maybe Text)
    } deriving (Show)

data CreateReservedInstancesListingResponse = CreateReservedInstancesListingResponse
    { crilrRequestId :: !Text
    , crilrReservedInstancesListingsSet :: !DescribeReservedInstancesListingsResponseSet
    } deriving (Show)

data CancelReservedInstancesListing = CancelReservedInstancesListing
    { crilReservedInstancesListingId :: !Text
    } deriving (Show)

data CancelReservedInstancesListingResponse = CancelReservedInstancesListingResponse
    { crilrRequestId :: !Text
    , crilrReservedInstancesListingsSet :: !DescribeReservedInstancesListingsResponseSet
    } deriving (Show)

data DescribeReservedInstancesListings = DescribeReservedInstancesListings
    { drilReservedInstancesListingSet :: !(Maybe DescribeReservedInstancesListingSet)
    , drilReservedInstancesSet :: !(Maybe DescribeReservedInstancesSet)
    , drilFilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data DescribeReservedInstancesListingSet = DescribeReservedInstancesListingSet
    { drilsItem :: ![DescribeReservedInstancesListingSetItem]
    } deriving (Show)

data DescribeReservedInstancesListingSetItem = DescribeReservedInstancesListingSetItem
    { drilsiReservedInstancesListingId :: !Text
    } deriving (Show)

data DescribeReservedInstancesListingsResponse = DescribeReservedInstancesListingsResponse
    { drilrRequestId :: !Text
    , drilrReservedInstancesListingsSet :: !DescribeReservedInstancesListingsResponseSet
    } deriving (Show)

data DescribeReservedInstancesListingsResponseSet = DescribeReservedInstancesListingsResponseSet
    { drilrsItem :: ![DescribeReservedInstancesListingsResponseSetItem]
    } deriving (Show)

data DescribeReservedInstancesListingsResponseSetItem = DescribeReservedInstancesListingsResponseSetItem
    { drilrsiReservedInstancesListingId :: !Text
    , drilrsiReservedInstancesId :: !Text
    , drilrsiCreateDate :: !DateTime
    , drilrsiUpdateDate :: !DateTime
    , drilrsiStatus :: !Text
    , drilrsiStatusMessage :: !Text
    , drilrsiInstanceCounts :: !InstanceCountsSet
    , drilrsiPriceSchedules :: !PriceScheduleSet
    , drilrsiTagSet :: !(Maybe ResourceTagSet)
    , drilrsiClientToken :: !(Maybe Text)
    } deriving (Show)

data InstanceCountsSet = InstanceCountsSet
    { icsItem :: ![InstanceCountsSetItem]
    } deriving (Show)

data InstanceCountsSetItem = InstanceCountsSetItem
    { icsiState :: !Text
    , icsiInstanceCount :: !Int
    } deriving (Show)

data PriceScheduleSet = PriceScheduleSet
    { pssItem :: ![PriceScheduleSetItem]
    } deriving (Show)

data PriceScheduleSetItem = PriceScheduleSetItem
    { pssiTerm :: !Long
    , pssiPrice :: !Double
    , pssiCurrencyCode :: !(Maybe Text)
    , pssiActive :: !Boolean
    } deriving (Show)

data MonitorInstances = MonitorInstances
    { miInstancesSet :: !MonitorInstancesSet
    } deriving (Show)

data MonitorInstancesSet = MonitorInstancesSet
    { misItem :: !(NonEmpty MonitorInstancesSetItem)
    } deriving (Show)

data MonitorInstancesSetItem = MonitorInstancesSetItem
    { misiInstanceId :: !Text
    } deriving (Show)

data MonitorInstancesResponse = MonitorInstancesResponse
    { mirRequestId :: !Text
    , mirInstancesSet :: !MonitorInstancesResponseSet
    } deriving (Show)

data MonitorInstancesResponseSet = MonitorInstancesResponseSet
    { mirsItem :: !(NonEmpty MonitorInstancesResponseSetItem)
    } deriving (Show)

data MonitorInstancesResponseSetItem = MonitorInstancesResponseSetItem
    { mirsiInstanceId :: !Text
    , mirsiMonitoring :: !InstanceMonitoringState
    } deriving (Show)

data InstanceMonitoringState = InstanceMonitoringState
    { imsState :: !Text
    } deriving (Show)

data Attachment = Attachment
    { aVpcId :: !Text
    , aState :: !Text
    } deriving (Show)

data AttachmentSet = AttachmentSet
    { asItem :: ![Attachment]
    } deriving (Show)

data VpnGateway = VpnGateway
    { vgVpnGatewayId :: !Text
    , vgState :: !Text
    , vgType :: !Text
    , vgAvailabilityZone :: !(Maybe Text)
    , vgAttachments :: !AttachmentSet
    , vgTagSet :: !(Maybe ResourceTagSet)
    } deriving (Show)

data CustomerGateway = CustomerGateway
    { cgCustomerGatewayId :: !Text
    , cgState :: !Text
    , cgType :: !Text
    , cgIpAddress :: !Text
    , cgBgpAsn :: !(Maybe Int)
    , cgTagSet :: !(Maybe ResourceTagSet)
    } deriving (Show)

data VpnConnection = VpnConnection
    { vcVpnConnectionId :: !Text
    , vcState :: !Text
    , vcCustomerGatewayConfiguration :: !(Maybe Text)
    , vcType :: !(Maybe Text)
    , vcCustomerGatewayId :: !Text
    , vcVpnGatewayId :: !Text
    , vcTagSet :: !(Maybe ResourceTagSet)
    , vcVgwTelemetry :: !(Maybe VgwTelemetry)
    , vcOptions :: !(Maybe VpnConnectionOptionsResponse)
    , vcRoutes :: !(Maybe VpnStaticRoutesSet)
    } deriving (Show)

data VpnConnectionOptionsResponse = VpnConnectionOptionsResponse
    { vcorStaticRoutesOnly :: !(Maybe Boolean)
    } deriving (Show)

data VpnStaticRoutesSet = VpnStaticRoutesSet
    { vsrsItem :: ![VpnStaticRoute]
    } deriving (Show)

data VpnStaticRoute = VpnStaticRoute
    { vsrDestinationCidrBlock :: !Text
    , vsrSource :: !Text
    , vsrState :: !Text
    } deriving (Show)

data VgwTelemetry = VgwTelemetry
    { vtItem :: ![VpnTunnelTelemetry]
    } deriving (Show)

data VpnTunnelTelemetry = VpnTunnelTelemetry
    { vttOutsideIpAddress :: !Text
    , vttStatus :: !Text
    , vttLastStatusChange :: !DateTime
    , vttStatusMessage :: !(Maybe Text)
    , vttAcceptedRouteCount :: !Int
    } deriving (Show)

data Vpc = Vpc
    { vVpcId :: !Text
    , vState :: !(Maybe Text)
    , vCidrBlock :: !(Maybe Text)
    , vDhcpOptionsId :: !(Maybe Text)
    , vTagSet :: !(Maybe ResourceTagSet)
    , vInstanceTenancy :: !(Maybe Text)
    , vIsDefault :: !(Maybe Boolean)
    } deriving (Show)

data Subnet = Subnet
    { sSubnetId :: !Text
    , sState :: !(Maybe Text)
    , sVpcId :: !(Maybe Text)
    , sCidrBlock :: !(Maybe Text)
    , sAvailableIpAddressCount :: !(Maybe Int)
    , sAvailabilityZone :: !(Maybe Text)
    , sDefaultForAz :: !(Maybe Boolean)
    , sMapPublicIpOnLaunch :: !(Maybe Boolean)
    , sTagSet :: !(Maybe ResourceTagSet)
    } deriving (Show)

data CustomerGatewaySet = CustomerGatewaySet
    { cgsItem :: ![CustomerGateway]
    } deriving (Show)

data VpnGatewaySet = VpnGatewaySet
    { vgsItem :: ![VpnGateway]
    } deriving (Show)

data VpnConnectionSet = VpnConnectionSet
    { vcsItem :: ![VpnConnection]
    } deriving (Show)

data VpcSet = VpcSet
    { vsItem :: ![Vpc]
    } deriving (Show)

data SubnetSet = SubnetSet
    { ssItem :: ![Subnet]
    } deriving (Show)

data CustomerGatewayIdSetItem = CustomerGatewayIdSetItem
    { cgisiCustomerGatewayId :: !Text
    } deriving (Show)

data CustomerGatewayIdSet = CustomerGatewayIdSet
    { cgisItem :: ![CustomerGatewayIdSetItem]
    } deriving (Show)

data VpnGatewayIdSetItem = VpnGatewayIdSetItem
    { vgisiVpnGatewayId :: !Text
    } deriving (Show)

data VpnGatewayIdSet = VpnGatewayIdSet
    { vgisItem :: ![VpnGatewayIdSetItem]
    } deriving (Show)

data VpnConnectionIdSetItem = VpnConnectionIdSetItem
    { vcisiVpnConnectionId :: !Text
    } deriving (Show)

data VpnConnectionIdSet = VpnConnectionIdSet
    { vcisItem :: ![VpnConnectionIdSetItem]
    } deriving (Show)

data VpcIdSetItem = VpcIdSetItem
    { visiVpcId :: !Text
    } deriving (Show)

data VpcIdSet = VpcIdSet
    { visItem :: ![VpcIdSetItem]
    } deriving (Show)

data SubnetIdSetItem = SubnetIdSetItem
    { sisiSubnetId :: !Text
    } deriving (Show)

data SubnetIdSet = SubnetIdSet
    { sisItem :: ![SubnetIdSetItem]
    } deriving (Show)

data DhcpOptionsIdSetItem = DhcpOptionsIdSetItem
    { doisiDhcpOptionsId :: !Text
    } deriving (Show)

data DhcpOptionsIdSet = DhcpOptionsIdSet
    { doisItem :: ![DhcpOptionsIdSetItem]
    } deriving (Show)

data DhcpConfigurationItemSet = DhcpConfigurationItemSet
    { dcisItem :: ![DhcpConfigurationItem]
    } deriving (Show)

data DhcpOptionsSet = DhcpOptionsSet
    { dosItem :: ![DhcpOptions]
    } deriving (Show)

data DhcpConfigurationItem = DhcpConfigurationItem
    { dciKey :: !Text
    , dciValueSet :: !DhcpValueSet
    } deriving (Show)

data DhcpOptions = DhcpOptions
    { doDhcpOptionsId :: !Text
    , doDhcpConfigurationSet :: !DhcpConfigurationItemSet
    , doTagSet :: !(Maybe ResourceTagSet)
    } deriving (Show)

data DhcpValue = DhcpValue
    { dvValue :: !Text
    } deriving (Show)

data DhcpValueSet = DhcpValueSet
    { dvsItem :: ![DhcpValue]
    } deriving (Show)

data Filter = Filter
    { fName :: !Text
    , fValueSet :: !ValueSet
    } deriving (Show)

data FilterSet = FilterSet
    { fsItem :: ![Filter]
    } deriving (Show)

data Value = Value
    { vValue :: !Text
    } deriving (Show)

data ValueSet = ValueSet
    { vsItem :: ![Value]
    } deriving (Show)

data CreateCustomerGateway = CreateCustomerGateway
    { ccgType :: !Text
    , ccgIpAddress :: !Text
    , ccgBgpAsn :: !(Maybe Int)
    } deriving (Show)

data CreateCustomerGatewayResponse = CreateCustomerGatewayResponse
    { ccgrRequestId :: !Text
    , ccgrCustomerGateway :: !CustomerGateway
    } deriving (Show)

data DeleteCustomerGateway = DeleteCustomerGateway
    { dcgCustomerGatewayId :: !Text
    } deriving (Show)

data DeleteCustomerGatewayResponse = DeleteCustomerGatewayResponse
    { dcgrRequestId :: !Text
    , dcgrReturn :: !Boolean
    } deriving (Show)

data DescribeCustomerGateways = DescribeCustomerGateways
    { dcgCustomerGatewaySet :: !(Maybe CustomerGatewayIdSet)
    , dcgFilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data DescribeCustomerGatewaysResponse = DescribeCustomerGatewaysResponse
    { dcgrRequestId :: !Text
    , dcgrCustomerGatewaySet :: !CustomerGatewaySet
    } deriving (Show)

data CreateVpnGateway = CreateVpnGateway
    { cvgType :: !Text
    , cvgAvailabilityZone :: !(Maybe Text)
    } deriving (Show)

data CreateVpnGatewayResponse = CreateVpnGatewayResponse
    { cvgrRequestId :: !Text
    , cvgrVpnGateway :: !VpnGateway
    } deriving (Show)

data DeleteVpnGateway = DeleteVpnGateway
    { dvgVpnGatewayId :: !Text
    } deriving (Show)

data DeleteVpnGatewayResponse = DeleteVpnGatewayResponse
    { dvgrRequestId :: !Text
    , dvgrReturn :: !Boolean
    } deriving (Show)

data DescribeVpnGateways = DescribeVpnGateways
    { dvgVpnGatewaySet :: !(Maybe VpnGatewayIdSet)
    , dvgFilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data DescribeVpnGatewaysResponse = DescribeVpnGatewaysResponse
    { dvgrRequestId :: !Text
    , dvgrVpnGatewaySet :: !VpnGatewaySet
    } deriving (Show)

data CreateVpnConnection = CreateVpnConnection
    { cvcType :: !Text
    , cvcCustomerGatewayId :: !Text
    , cvcVpnGatewayId :: !Text
    , cvcOptions :: !(Maybe VpnConnectionOptionsRequest)
    } deriving (Show)

data VpnConnectionOptionsRequest = VpnConnectionOptionsRequest
    { vcorStaticRoutesOnly :: !(Maybe Boolean)
    } deriving (Show)

data CreateVpnConnectionResponse = CreateVpnConnectionResponse
    { cvcrRequestId :: !Text
    , cvcrVpnConnection :: !VpnConnection
    } deriving (Show)

data CreateVpnConnectionRoute = CreateVpnConnectionRoute
    { cvcrVpnConnectionId :: !Text
    , cvcrDestinationCidrBlock :: !Text
    } deriving (Show)

data CreateVpnConnectionRouteResponse = CreateVpnConnectionRouteResponse
    { cvcrrRequestId :: !Text
    , cvcrrReturn :: !Boolean
    } deriving (Show)

data DeleteVpnConnectionRoute = DeleteVpnConnectionRoute
    { dvcrVpnConnectionId :: !Text
    , dvcrDestinationCidrBlock :: !Text
    } deriving (Show)

data DeleteVpnConnectionRouteResponse = DeleteVpnConnectionRouteResponse
    { dvcrrRequestId :: !Text
    , dvcrrReturn :: !Boolean
    } deriving (Show)

data DeleteVpnConnection = DeleteVpnConnection
    { dvcVpnConnectionId :: !Text
    } deriving (Show)

data DeleteVpnConnectionResponse = DeleteVpnConnectionResponse
    { dvcrRequestId :: !Text
    , dvcrReturn :: !Boolean
    } deriving (Show)

data DescribeVpnConnections = DescribeVpnConnections
    { dvcVpnConnectionSet :: !(Maybe VpnConnectionIdSet)
    , dvcFilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data DescribeVpnConnectionsResponse = DescribeVpnConnectionsResponse
    { dvcrRequestId :: !Text
    , dvcrVpnConnectionSet :: !VpnConnectionSet
    } deriving (Show)

data AttachVpnGateway = AttachVpnGateway
    { avgVpnGatewayId :: !Text
    , avgVpcId :: !Text
    } deriving (Show)

data AttachVpnGatewayResponse = AttachVpnGatewayResponse
    { avgrRequestId :: !Text
    , avgrAttachment :: !Attachment
    } deriving (Show)

data DetachVpnGateway = DetachVpnGateway
    { dvgVpnGatewayId :: !Text
    , dvgVpcId :: !Text
    } deriving (Show)

data DetachVpnGatewayResponse = DetachVpnGatewayResponse
    { dvgrRequestId :: !Text
    , dvgrReturn :: !Boolean
    } deriving (Show)

data CreateVpc = CreateVpc
    { cvCidrBlock :: !Text
    , cvInstanceTenancy :: !(Maybe Text)
    } deriving (Show)

data CreateVpcResponse = CreateVpcResponse
    { cvrRequestId :: !Text
    , cvrVpc :: !Vpc
    } deriving (Show)

data DescribeVpcs = DescribeVpcs
    { dvVpcSet :: !(Maybe VpcIdSet)
    , dvFilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data DescribeVpcsResponse = DescribeVpcsResponse
    { dvrRequestId :: !Text
    , dvrVpcSet :: !VpcSet
    } deriving (Show)

data DeleteVpc = DeleteVpc
    { dvVpcId :: !Text
    } deriving (Show)

data DeleteVpcResponse = DeleteVpcResponse
    { dvrRequestId :: !Text
    , dvrReturn :: !Boolean
    } deriving (Show)

data CreateSubnet = CreateSubnet
    { csVpcId :: !Text
    , csCidrBlock :: !Text
    , csAvailabilityZone :: !(Maybe Text)
    } deriving (Show)

data CreateSubnetResponse = CreateSubnetResponse
    { csrRequestId :: !Text
    , csrSubnet :: !Subnet
    } deriving (Show)

data DescribeSubnets = DescribeSubnets
    { dsSubnetSet :: !(Maybe SubnetIdSet)
    , dsFilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data DescribeSubnetsResponse = DescribeSubnetsResponse
    { dsrRequestId :: !Text
    , dsrSubnetSet :: !SubnetSet
    } deriving (Show)

data DeleteSubnet = DeleteSubnet
    { dsSubnetId :: !Text
    } deriving (Show)

data DeleteSubnetResponse = DeleteSubnetResponse
    { dsrRequestId :: !Text
    , dsrReturn :: !Boolean
    } deriving (Show)

data DeleteDhcpOptions = DeleteDhcpOptions
    { ddoDhcpOptionsId :: !Text
    } deriving (Show)

data DeleteDhcpOptionsResponse = DeleteDhcpOptionsResponse
    { ddorRequestId :: !Text
    , ddorReturn :: !Boolean
    } deriving (Show)

data DescribeDhcpOptions = DescribeDhcpOptions
    { ddoDhcpOptionsSet :: !(Maybe DhcpOptionsIdSet)
    , ddoFilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data DescribeDhcpOptionsResponse = DescribeDhcpOptionsResponse
    { ddorRequestId :: !Text
    , ddorDhcpOptionsSet :: !DhcpOptionsSet
    } deriving (Show)

data CreateDhcpOptions = CreateDhcpOptions
    { cdoDhcpConfigurationSet :: !DhcpConfigurationItemSet
    } deriving (Show)

data CreateDhcpOptionsResponse = CreateDhcpOptionsResponse
    { cdorRequestId :: !Text
    , cdorDhcpOptions :: !DhcpOptions
    } deriving (Show)

data AssociateDhcpOptions = AssociateDhcpOptions
    { adoDhcpOptionsId :: !Text
    , adoVpcId :: !Text
    } deriving (Show)

data AssociateDhcpOptionsResponse = AssociateDhcpOptionsResponse
    { adorRequestId :: !Text
    , adorReturn :: !Boolean
    } deriving (Show)

data RequestSpotInstances = RequestSpotInstances
    { rsiSpotPrice :: !Text
    , rsiInstanceCount :: !(Maybe Integer)
    , rsiType :: !(Maybe Text)
    , rsiValidFrom :: !(Maybe DateTime)
    , rsiValidUntil :: !(Maybe DateTime)
    , rsiLaunchGroup :: !(Maybe Text)
    , rsiAvailabilityZoneGroup :: !(Maybe Text)
    , rsiLaunchSpecification :: !LaunchSpecificationRequest
    } deriving (Show)

data LaunchSpecificationRequest = LaunchSpecificationRequest
    { lsrImageId :: !Text
    , lsrKeyName :: !(Maybe Text)
    , lsrGroupSet :: !GroupSet
    , lsrUserData :: !(Maybe UserData)
    , lsrAddressing :: !(Maybe Text)
    , lsrInstance :: !Text
    , lsrPlacement :: !(Maybe SpotPlacementRequest)
    , lsrKernelId :: !(Maybe Text)
    , lsrRamdiskId :: !(Maybe Text)
    , lsrBlockDeviceMapping :: !(Maybe BlockDeviceMapping)
    , lsrMonitoring :: !(Maybe MonitoringInstance)
    , lsrSubnetId :: !(Maybe Text)
    , lsrNetworkInterfaceSet :: !(Maybe InstanceNetworkInterfaceSetRequest)
    , lsrIamInstanceProfile :: !(Maybe IamInstanceProfileRequest)
    , lsrEbsOptimized :: !(Maybe Boolean)
    } deriving (Show)

data LaunchSpecificationResponse = LaunchSpecificationResponse
    { lsrImageId :: !Text
    , lsrKeyName :: !(Maybe Text)
    , lsrGroupSet :: !GroupSet
    , lsrAddressing :: !(Maybe Text)
    , lsrInstance :: !Text
    , lsrPlacement :: !(Maybe SpotPlacementRequest)
    , lsrKernelId :: !(Maybe Text)
    , lsrRamdiskId :: !(Maybe Text)
    , lsrBlockDeviceMapping :: !(Maybe BlockDeviceMapping)
    , lsrMonitoring :: !(Maybe MonitoringInstance)
    , lsrSubnetId :: !(Maybe Text)
    , lsrNetworkInterfaceSet :: !(Maybe InstanceNetworkInterfaceSetRequest)
    , lsrIamInstanceProfile :: !(Maybe IamInstanceProfileRequest)
    , lsrEbsOptimized :: !(Maybe Boolean)
    } deriving (Show)

data SpotInstanceRequestSetItem = SpotInstanceRequestSetItem
    { sirsiSpotInstanceRequestId :: !Text
    , sirsiSpotPrice :: !Text
    , sirsiType :: !Text
    , sirsiState :: !Text
    , sirsiFault :: !(Maybe SpotInstanceStateFault)
    , sirsiStatus :: !(Maybe SpotInstanceStatusMessage)
    , sirsiValidFrom :: !(Maybe DateTime)
    , sirsiValidUntil :: !(Maybe DateTime)
    , sirsiLaunchGroup :: !(Maybe Text)
    , sirsiAvailabilityZoneGroup :: !(Maybe Text)
    , sirsiLaunchSpecification :: !(Maybe LaunchSpecificationResponse)
    , sirsiInstanceId :: !(Maybe Text)
    , sirsiCreateTime :: !(Maybe DateTime)
    , sirsiProductDescription :: !(Maybe Text)
    , sirsiTagSet :: !(Maybe ResourceTagSet)
    , sirsiLaunchedAvailabilityZone :: !(Maybe Text)
    } deriving (Show)

data SpotInstanceStateFault = SpotInstanceStateFault
    { sisfCode :: !Text
    , sisfMessage :: !Text
    } deriving (Show)

data SpotInstanceStatusMessage = SpotInstanceStatusMessage
    { sismCode :: !(Maybe Text)
    , sismUpdateTime :: !(Maybe DateTime)
    , sismMessage :: !(Maybe Text)
    } deriving (Show)

data SpotInstanceRequestSet = SpotInstanceRequestSet
    { sirsItem :: ![SpotInstanceRequestSetItem]
    } deriving (Show)

data RequestSpotInstancesResponse = RequestSpotInstancesResponse
    { rsirRequestId :: !Text
    , rsirSpotInstanceRequestSet :: !SpotInstanceRequestSet
    } deriving (Show)

data DescribeSpotInstanceRequests = DescribeSpotInstanceRequests
    { dsirSpotInstanceRequestIdSet :: !SpotInstanceRequestIdSet
    , dsirFilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data SpotInstanceRequestIdSet = SpotInstanceRequestIdSet
    { sirisItem :: ![SpotInstanceRequestIdSetItem]
    } deriving (Show)

data SpotInstanceRequestIdSetItem = SpotInstanceRequestIdSetItem
    { sirisiSpotInstanceRequestId :: !Text
    } deriving (Show)

data DescribeSpotInstanceRequestsResponse = DescribeSpotInstanceRequestsResponse
    { dsirrRequestId :: !Text
    , dsirrSpotInstanceRequestSet :: !SpotInstanceRequestSet
    } deriving (Show)

data CancelSpotInstanceRequests = CancelSpotInstanceRequests
    { csirSpotInstanceRequestIdSet :: !SpotInstanceRequestIdSet
    } deriving (Show)

data CancelSpotInstanceRequestsResponse = CancelSpotInstanceRequestsResponse
    { csirrRequestId :: !Text
    , csirrSpotInstanceRequestSet :: !CancelSpotInstanceRequestsResponseSet
    } deriving (Show)

data CancelSpotInstanceRequestsResponseSet = CancelSpotInstanceRequestsResponseSet
    { csirrsItem :: !(NonEmpty CancelSpotInstanceRequestsResponseSetItem)
    } deriving (Show)

data CancelSpotInstanceRequestsResponseSetItem = CancelSpotInstanceRequestsResponseSetItem
    { csirrsiSpotInstanceRequestId :: !Text
    , csirrsiState :: !Text
    } deriving (Show)

data DescribeSpotPriceHistory = DescribeSpotPriceHistory
    { dsphStartTime :: !(Maybe DateTime)
    , dsphEndTime :: !(Maybe DateTime)
    , dsphInstanceSet :: !(Maybe InstanceSet)
    , dsphProductDescriptionSet :: !(Maybe ProductDescriptionSet)
    , dsphFilterSet :: !(Maybe FilterSet)
    , dsphAvailabilityZone :: !(Maybe Text)
    , dsphMaxResults :: !(Maybe Integer)
    , dsphNextToken :: !(Maybe Text)
    } deriving (Show)

data InstanceSet = InstanceSet
    { isItem :: !(NonEmpty InstanceSetItem)
    } deriving (Show)

data InstanceSetItem = InstanceSetItem
    { isiInstance :: !Text
    } deriving (Show)

data ProductDescriptionSet = ProductDescriptionSet
    { pdsItem :: !(NonEmpty ProductDescriptionSetItem)
    } deriving (Show)

data ProductDescriptionSetItem = ProductDescriptionSetItem
    { pdsiProductDescription :: !Text
    } deriving (Show)

data DescribeSpotPriceHistoryResponse = DescribeSpotPriceHistoryResponse
    { dsphrRequestId :: !Text
    , dsphrSpotPriceHistorySet :: !SpotPriceHistorySet
    , dsphrNextToken :: !(Maybe Text)
    } deriving (Show)

data SpotPriceHistorySet = SpotPriceHistorySet
    { sphsItem :: ![SpotPriceHistorySetItem]
    } deriving (Show)

data SpotPriceHistorySetItem = SpotPriceHistorySetItem
    { sphsiInstance :: !Text
    , sphsiProductDescription :: !Text
    , sphsiSpotPrice :: !Text
    , sphsiTimestamp :: !DateTime
    , sphsiAvailabilityZone :: !(Maybe Text)
    } deriving (Show)

data SpotDatafeedSubscription = SpotDatafeedSubscription
    { sdsOwnerId :: !Text
    , sdsBucket :: !Text
    , sdsPrefix :: !Text
    , sdsState :: !Text
    , sdsFault :: !(Maybe SpotInstanceStateFault)
    } deriving (Show)

data CreateSpotDatafeedSubscription = CreateSpotDatafeedSubscription
    { csdsBucket :: !Text
    , csdsPrefix :: !Text
    } deriving (Show)

data CreateSpotDatafeedSubscriptionResponse = CreateSpotDatafeedSubscriptionResponse
    { csdsrRequestId :: !Text
    , csdsrSpotDatafeedSubscription :: !SpotDatafeedSubscription
    } deriving (Show)

data DescribeSpotDatafeedSubscription = DescribeSpotDatafeedSubscription

data DescribeSpotDatafeedSubscriptionResponse = DescribeSpotDatafeedSubscriptionResponse
    { dsdsrRequestId :: !Text
    , dsdsrSpotDatafeedSubscription :: !SpotDatafeedSubscription
    } deriving (Show)

data DescribeSpotDatafeedSubscription = DescribeSpotDatafeedSubscription

data DeleteSpotDatafeedSubscriptionResponse = DeleteSpotDatafeedSubscriptionResponse
    { dsdsrRequestId :: !Text
    , dsdsrReturn :: !Boolean
    } deriving (Show)

data DescribeLicenses = DescribeLicenses
    { dlLicenseIdSet :: !(Maybe LicenseIdSet)
    , dlFilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data LicenseIdSet = LicenseIdSet
    { lisItem :: ![LicenseIdSetItem]
    } deriving (Show)

data LicenseIdSetItem = LicenseIdSetItem
    { lisiLicenseId :: !Text
    } deriving (Show)

data DescribeLicensesResponse = DescribeLicensesResponse
    { dlrRequestId :: !Text
    , dlrLicenseSet :: !LicenseSet
    } deriving (Show)

data LicenseSet = LicenseSet
    { lsItem :: ![LicenseSetItem]
    } deriving (Show)

data LicenseSetItem = LicenseSetItem
    { lsiLicenseId :: !Text
    , lsiType :: !Text
    , lsiPool :: !Text
    , lsiCapacitySet :: !LicenseCapacitySet
    , lsiTagSet :: !(Maybe ResourceTagSet)
    } deriving (Show)

data LicenseCapacitySet = LicenseCapacitySet
    { lcsItem :: ![LicenseCapacitySetItem]
    } deriving (Show)

data LicenseCapacitySetItem = LicenseCapacitySetItem
    { lcsiCapacity :: !Int
    , lcsiInstanceCapacity :: !Int
    , lcsiState :: !Text
    , lcsiEarliestAllowedDeactivationTime :: !(Maybe DateTime)
    } deriving (Show)

data ActivateLicense = ActivateLicense
    { alLicenseId :: !Text
    , alCapacity :: !Int
    } deriving (Show)

data ActivateLicenseResponse = ActivateLicenseResponse
    { alrRequestId :: !Text
    , alrReturn :: !Boolean
    } deriving (Show)

data DeactivateLicense = DeactivateLicense
    { dlLicenseId :: !Text
    , dlCapacity :: !Int
    } deriving (Show)

data DeactivateLicenseResponse = DeactivateLicenseResponse
    { dlrRequestId :: !Text
    , dlrReturn :: !Boolean
    } deriving (Show)

data CreatePlacementGroup = CreatePlacementGroup
    { cpgGroupName :: !Text
    , cpgStrategy :: !Text
    } deriving (Show)

data CreatePlacementGroupResponse = CreatePlacementGroupResponse
    { cpgrRequestId :: !Text
    , cpgrReturn :: !Boolean
    } deriving (Show)

data DeletePlacementGroup = DeletePlacementGroup
    { dpgGroupName :: !Text
    } deriving (Show)

data DeletePlacementGroupResponse = DeletePlacementGroupResponse
    { dpgrRequestId :: !Text
    , dpgrReturn :: !Boolean
    } deriving (Show)

data DescribePlacementGroupItem = DescribePlacementGroupItem
    { dpgiGroupName :: !Text
    } deriving (Show)

data DescribePlacementGroupsInfo = DescribePlacementGroupsInfo
    { dpgiItem :: ![DescribePlacementGroupItem]
    } deriving (Show)

data DescribePlacementGroups = DescribePlacementGroups
    { dpgPlacementGroupSet :: !DescribePlacementGroupsInfo
    , dpgFilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data PlacementGroupInfo = PlacementGroupInfo
    { pgiGroupName :: !Text
    , pgiStrategy :: !Text
    , pgiState :: !Text
    } deriving (Show)

data PlacementGroupSet = PlacementGroupSet
    { pgsItem :: ![PlacementGroupInfo]
    } deriving (Show)

data DescribePlacementGroupsResponse = DescribePlacementGroupsResponse
    { dpgrRequestId :: !Text
    , dpgrPlacementGroupSet :: !PlacementGroupSet
    } deriving (Show)

data ResourceIdSet = ResourceIdSet
    { risItem :: ![ResourceIdSetItem]
    } deriving (Show)

data ResourceIdSetItem = ResourceIdSetItem
    { risiResourceId :: !Text
    } deriving (Show)

data ResourceTagSetItem = ResourceTagSetItem
    { rtsiKey :: !Text
    , rtsiValue :: !Text
    } deriving (Show)

data ResourceTagSet = ResourceTagSet
    { rtsItem :: ![ResourceTagSetItem]
    } deriving (Show)

data CreateTags = CreateTags
    { ctResourcesSet :: !ResourceIdSet
    , ctTagSet :: !ResourceTagSet
    } deriving (Show)

data CreateTagsResponse = CreateTagsResponse
    { ctrRequestId :: !Text
    , ctrReturn :: !Boolean
    } deriving (Show)

data TagSetItem = TagSetItem
    { tsiResourceId :: !(Maybe Text)
    , tsiResource :: !(Maybe Text)
    , tsiKey :: !(Maybe Text)
    , tsiValue :: !(Maybe Text)
    } deriving (Show)

data TagSet = TagSet
    { tsItem :: ![TagSetItem]
    } deriving (Show)

data DescribeTags = DescribeTags
    { dtFilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data DescribeTagsResponse = DescribeTagsResponse
    { dtrRequestId :: !Text
    , dtrTagSet :: !TagSet
    } deriving (Show)

data DeleteTagsSetItem = DeleteTagsSetItem
    { dtsiKey :: !(Maybe Text)
    , dtsiValue :: !(Maybe Text)
    } deriving (Show)

data DeleteTagsSet = DeleteTagsSet
    { dtsItem :: ![DeleteTagsSetItem]
    } deriving (Show)

data DeleteTags = DeleteTags
    { dtResourcesSet :: !ResourceIdSet
    , dtTagSet :: !DeleteTagsSet
    } deriving (Show)

data DeleteTagsResponse = DeleteTagsResponse
    { dtrRequestId :: !Text
    , dtrReturn :: !Boolean
    } deriving (Show)

data ImportInstance = ImportInstance
    { iiDescription :: !(Maybe Text)
    , iiLaunchSpecification :: !ImportInstanceLaunchSpecification
    , iiDiskImageSet :: !DiskImageSet
    , iiKeepPartialImports :: !(Maybe Boolean)
    , iiPlatform :: !Text
    } deriving (Show)

data ImportInstanceResponse = ImportInstanceResponse
    { iirRequestId :: !Text
    , iirConversionTask :: !ConversionTask
    } deriving (Show)

data ImportInstanceLaunchSpecification = ImportInstanceLaunchSpecification
    { iilsArchitecture :: !Text
    , iilsGroupSet :: !(Maybe ImportInstanceGroupSet)
    , iilsUserData :: !(Maybe UserData)
    , iilsInstance :: !Text
    , iilsPlacement :: !(Maybe InstancePlacement)
    , iilsMonitoring :: !(Maybe MonitoringInstance)
    , iilsSubnetId :: !(Maybe Text)
    , iilsInstanceInitiatedShutdownBehavior :: !(Maybe Text)
    , iilsPrivateIpAddress :: !(Maybe Text)
    } deriving (Show)

data DiskImageSet = DiskImageSet
    { disItem :: ![DiskImage]
    } deriving (Show)

data DiskImage = DiskImage
    { diImage :: !DiskImageDetail
    , diDescription :: !(Maybe Text)
    , diVolume :: !DiskImageVolume
    } deriving (Show)

data DiskImageDetail = DiskImageDetail
    { didFormat :: !Text
    , didBytes :: !Long
    , didImportManifestUrl :: !Text
    } deriving (Show)

data DiskImageVolume = DiskImageVolume
    { divSize :: !Integer
    } deriving (Show)

data ConversionTask = ConversionTask
    { ctConversionTaskId :: !Text
    , ctExpirationTime :: !(Maybe Text)
  -- <xs:choice>
    , ctImportVolume :: !(Maybe ImportVolumeTaskDetails)
    , ctImportInstance :: !(Maybe ImportInstanceTaskDetails)
  -- </xs:choice>
    , ctState :: !Text
    , ctStatusMessage :: !(Maybe Text)
    , ctTagSet :: !(Maybe ResourceTagSet)
    } deriving (Show)

data ImportInstanceTaskDetails = ImportInstanceTaskDetails
    { iitdVolumes :: !ImportInstanceVolumeDetailSet
    , iitdInstanceId :: !(Maybe Text)
    , iitdPlatform :: !(Maybe Text)
    , iitdDescription :: !(Maybe Text)
    } deriving (Show)

data ImportVolumeTaskDetails = ImportVolumeTaskDetails
    { ivtdBytesConverted :: !Long
    , ivtdAvailabilityZone :: !Text
    , ivtdDescription :: !(Maybe Text)
    , ivtdImage :: !DiskImageDescription
    , ivtdVolume :: !DiskImageVolumeDescription
    } deriving (Show)

data ImportInstanceVolumeDetailSet = ImportInstanceVolumeDetailSet
    { iivdsItem :: ![ImportInstanceVolumeDetailItem]
    } deriving (Show)

data ImportInstanceVolumeDetailItem = ImportInstanceVolumeDetailItem
    { iivdiBytesConverted :: !Long
    , iivdiAvailabilityZone :: !Text
    , iivdiImage :: !DiskImageDescription
    , iivdiDescription :: !(Maybe Text)
    , iivdiVolume :: !DiskImageVolumeDescription
    , iivdiStatus :: !Text
    , iivdiStatusMessage :: !(Maybe Text)
    } deriving (Show)

data DiskImageVolumeDescription = DiskImageVolumeDescription
    { divdSize :: !Integer
    , divdId :: !Text
    } deriving (Show)

data DiskImageDescription = DiskImageDescription
    { didFormat :: !Text
    , didSize :: !Long
    , didImportManifestUrl :: !Text
    , didChecksum :: !(Maybe Text)
    } deriving (Show)

data ImportVolume = ImportVolume
    { ivAvailabilityZone :: !Text
    , ivImage :: !DiskImageDetail
    , ivDescription :: !(Maybe Text)
    , ivVolume :: !DiskImageVolume
    } deriving (Show)

data ImportVolumeResponse = ImportVolumeResponse
    { ivrRequestId :: !Text
    , ivrConversionTask :: !ConversionTask
    } deriving (Show)

data DescribeConversionTasks = DescribeConversionTasks
    { dctConversionTaskIdSet :: !ConversionTaskIdSet
    } deriving (Show)

data DescribeConversionTasksResponse = DescribeConversionTasksResponse
    { dctrRequestId :: !Text
    , dctrConversionTasks :: !ConversionTaskSet
    } deriving (Show)

data ConversionTaskIdSet = ConversionTaskIdSet
    { ctisItem :: ![ConversionTaskIdItem]
    } deriving (Show)

data ConversionTaskIdItem = ConversionTaskIdItem
    { ctiiConversionTaskId :: !Text
    } deriving (Show)

data ConversionTaskSet = ConversionTaskSet
    { ctsItem :: ![ConversionTask]
    } deriving (Show)

data CancelConversionTask = CancelConversionTask
    { cctConversionTaskId :: !Text
    } deriving (Show)

data CancelConversionTaskResponse = CancelConversionTaskResponse
    { cctrRequestId :: !Text
    , cctrReturn :: !Boolean
    } deriving (Show)

data CreateInstanceExportTask = CreateInstanceExportTask
    { cietDescription :: !(Maybe Text)
    , cietInstanceId :: !Text
    , cietTargetEnvironment :: !Text
  -- <xs:choice>
    , cietExportToS3 :: !(Maybe ExportToS3Task)
  -- </xs:choice>
    } deriving (Show)

data ExportToS3Task = ExportToS3Task
    { ets3tDiskImageFormat :: !(Maybe Text)
    , ets3tContainerFormat :: !(Maybe Text)
    , ets3tS3Bucket :: !Text
    , ets3tS3Prefix :: !Text
    } deriving (Show)

data CreateInstanceExportTaskResponse = CreateInstanceExportTaskResponse
    { cietrRequestId :: !Text
    , cietrExportTask :: !ExportTaskResponse
    } deriving (Show)

data DescribeExportTasks = DescribeExportTasks
    { detExportTaskIdSet :: !ExportTaskIdSet
    } deriving (Show)

data ExportTaskIdSet = ExportTaskIdSet
    { etisItem :: ![ExportTaskId]
    } deriving (Show)

data ExportTaskId = ExportTaskId
    { etiExportTaskId :: !Text
    } deriving (Show)

data DescribeExportTasksResponse = DescribeExportTasksResponse
    { detrRequestId :: !Text
    , detrExportTaskSet :: !ExportTaskSetResponse
    } deriving (Show)

data ExportTaskSetResponse = ExportTaskSetResponse
    { etsrItem :: ![ExportTaskResponse]
    } deriving (Show)

data ExportTaskResponse = ExportTaskResponse
    { etrExportTaskId :: !Text
    , etrDescription :: !(Maybe Text)
    , etrState :: !Text
    , etrStatusMessage :: !(Maybe Text)
  -- <xs:choice>
    , etrInstanceExport :: !(Maybe InstanceExportTaskResponse)
  -- </xs:choice>
  -- <xs:choice>
    , etrExportToS3 :: !(Maybe ExportToS3TaskResponse)
  -- </xs:choice>
    } deriving (Show)

data InstanceExportTaskResponse = InstanceExportTaskResponse
    { ietrInstanceId :: !Text
    , ietrTargetEnvironment :: !(Maybe Text)
    } deriving (Show)

data ExportToS3TaskResponse = ExportToS3TaskResponse
    { ets3trDiskImageFormat :: !Text
    , ets3trContainerFormat :: !(Maybe Text)
    , ets3trS3Bucket :: !Text
    , ets3trS3Key :: !Text
    } deriving (Show)

data CancelExportTask = CancelExportTask
    { cetExportTaskId :: !Text
    } deriving (Show)

data CancelExportTaskResponse = CancelExportTaskResponse
    { cetrRequestId :: !Text
    , cetrReturn :: !Boolean
    } deriving (Show)

data CreateInternetGateway = CreateInternetGateway

data InternetGatewayAttachmentSet = InternetGatewayAttachmentSet
    { igasItem :: ![InternetGatewayAttachment]
    } deriving (Show)

data InternetGatewayAttachment = InternetGatewayAttachment
    { igaVpcId :: !Text
    , igaState :: !Text
    } deriving (Show)

data InternetGateway = InternetGateway
    { igInternetGatewayId :: !Text
    , igAttachmentSet :: !InternetGatewayAttachmentSet
    , igTagSet :: !(Maybe ResourceTagSet)
    } deriving (Show)

data CreateInternetGatewayResponse = CreateInternetGatewayResponse
    { cigrRequestId :: !Text
    , cigrInternetGateway :: !InternetGateway
    } deriving (Show)

data InternetGatewayIdSet = InternetGatewayIdSet
    { igisItem :: ![InternetGatewayIdSetItem]
    } deriving (Show)

data InternetGatewayIdSetItem = InternetGatewayIdSetItem
    { igisiInternetGatewayId :: !Text
    } deriving (Show)

data DescribeInternetGateways = DescribeInternetGateways
    { digInternetGatewayIdSet :: !InternetGatewayIdSet
    , digFilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data InternetGatewaySet = InternetGatewaySet
    { igsItem :: ![InternetGateway]
    } deriving (Show)

data DescribeInternetGatewaysResponse = DescribeInternetGatewaysResponse
    { digrRequestId :: !Text
    , digrInternetGatewaySet :: !InternetGatewaySet
    } deriving (Show)

data DeleteInternetGateway = DeleteInternetGateway
    { digInternetGatewayId :: !Text
    } deriving (Show)

data DeleteInternetGatewayResponse = DeleteInternetGatewayResponse
    { digrRequestId :: !Text
    , digrReturn :: !Boolean
    } deriving (Show)

data AttachInternetGateway = AttachInternetGateway
    { aigInternetGatewayId :: !Text
    , aigVpcId :: !Text
    } deriving (Show)

data AttachInternetGatewayResponse = AttachInternetGatewayResponse
    { aigrRequestId :: !Text
    , aigrReturn :: !Boolean
    } deriving (Show)

data DetachInternetGateway = DetachInternetGateway
    { digInternetGatewayId :: !Text
    , digVpcId :: !Text
    } deriving (Show)

data DetachInternetGatewayResponse = DetachInternetGatewayResponse
    { digrRequestId :: !Text
    , digrReturn :: !Boolean
    } deriving (Show)

data CreateRouteTable = CreateRouteTable
    { crtVpcId :: !Text
    } deriving (Show)

data RouteSet = RouteSet
    { rsItem :: ![Route]
    } deriving (Show)

data Route = Route
    { rDestinationCidrBlock :: !Text
    , rGatewayId :: !(Maybe Text)
    , rInstanceId :: !(Maybe Text)
    , rInstanceOwnerId :: !(Maybe Text)
    , rNetworkInterfaceId :: !(Maybe Text)
    , rState :: !Text
    , rOrigin :: !Text
    } deriving (Show)

data RouteTableAssociationSet = RouteTableAssociationSet
    { rtasItem :: ![RouteTableAssociation]
    } deriving (Show)

data RouteTableAssociation = RouteTableAssociation
    { rtaRouteTableAssociationId :: !Text
    , rtaRouteTableId :: !Text
  -- <xs:choice>
    , rtaSubnetId :: !(Maybe Text)
    , rtaMain :: !(Maybe Boolean)
  -- </xs:choice>
    } deriving (Show)

data PropagatingVgwSet = PropagatingVgwSet
    { pvsItem :: ![PropagatingVgw]
    } deriving (Show)

data PropagatingVgw = PropagatingVgw
    { pvGatewayId :: !Text
    } deriving (Show)

data RouteTable = RouteTable
    { rtRouteTableId :: !Text
    , rtVpcId :: !Text
    , rtRouteSet :: !RouteSet
    , rtAssociationSet :: !RouteTableAssociationSet
    , rtPropagatingVgwSet :: !PropagatingVgwSet
    , rtTagSet :: !(Maybe ResourceTagSet)
    } deriving (Show)

data CreateRouteTableResponse = CreateRouteTableResponse
    { crtrRequestId :: !Text
    , crtrRouteTable :: !RouteTable
    } deriving (Show)

data RouteTableIdSet = RouteTableIdSet
    { rtisItem :: ![RouteTableIdSetItem]
    } deriving (Show)

data RouteTableIdSetItem = RouteTableIdSetItem
    { rtisiRouteTableId :: !Text
    } deriving (Show)

data DescribeRouteTables = DescribeRouteTables
    { drtRouteTableIdSet :: !RouteTableIdSet
    , drtFilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data RouteTableSet = RouteTableSet
    { rtsItem :: ![RouteTable]
    } deriving (Show)

data DescribeRouteTablesResponse = DescribeRouteTablesResponse
    { drtrRequestId :: !Text
    , drtrRouteTableSet :: !RouteTableSet
    } deriving (Show)

data EnableVgwRoutePropagationRequest = EnableVgwRoutePropagationRequest
    { evrprRouteTableId :: !Text
    , evrprGatewayId :: !Text
    } deriving (Show)

data EnableVgwRoutePropagationResponse = EnableVgwRoutePropagationResponse
    { evrprRequestId :: !Text
    , evrprReturn :: !Boolean
    } deriving (Show)

data DisableVgwRoutePropagationRequest = DisableVgwRoutePropagationRequest
    { dvrprRouteTableId :: !Text
    , dvrprGatewayId :: !Text
    } deriving (Show)

data DisableVgwRoutePropagationResponse = DisableVgwRoutePropagationResponse
    { dvrprRequestId :: !Text
    , dvrprReturn :: !Boolean
    } deriving (Show)

data DeleteRouteTable = DeleteRouteTable
    { drtRouteTableId :: !Text
    } deriving (Show)

data DeleteRouteTableResponse = DeleteRouteTableResponse
    { drtrRequestId :: !Text
    , drtrReturn :: !Boolean
    } deriving (Show)

data AssociateRouteTable = AssociateRouteTable
    { artRouteTableId :: !Text
    , artSubnetId :: !Text
    } deriving (Show)

data AssociateRouteTableResponse = AssociateRouteTableResponse
    { artrRequestId :: !Text
    , artrAssociationId :: !Text
    } deriving (Show)

data ReplaceRouteTableAssociation = ReplaceRouteTableAssociation
    { rrtaAssociationId :: !Text
    , rrtaRouteTableId :: !Text
    } deriving (Show)

data ReplaceRouteTableAssociationResponse = ReplaceRouteTableAssociationResponse
    { rrtarRequestId :: !Text
    , rrtarNewAssociationId :: !Text
    } deriving (Show)

data DisassociateRouteTable = DisassociateRouteTable
    { drtAssociationId :: !Text
    } deriving (Show)

data DisassociateRouteTableResponse = DisassociateRouteTableResponse
    { drtrRequestId :: !Text
    , drtrReturn :: !Boolean
    } deriving (Show)

data CreateRoute = CreateRoute
    { crRouteTableId :: !Text
    , crDestinationCidrBlock :: !Text
  -- <xs:choice>
    , crGatewayId :: !(Maybe Text)
    , crInstanceId :: !(Maybe Text)
    , crNetworkInterfaceId :: !(Maybe Text)
  -- </xs:choice>
    } deriving (Show)

data CreateRouteResponse = CreateRouteResponse
    { crrRequestId :: !Text
    , crrReturn :: !Boolean
    } deriving (Show)

data ReplaceRoute = ReplaceRoute
    { rrRouteTableId :: !Text
    , rrDestinationCidrBlock :: !Text
  -- <xs:choice>
    , rrGatewayId :: !(Maybe Text)
    , rrInstanceId :: !(Maybe Text)
    , rrNetworkInterfaceId :: !(Maybe Text)
  -- </xs:choice>
    } deriving (Show)

data ReplaceRouteResponse = ReplaceRouteResponse
    { rrrRequestId :: !Text
    , rrrReturn :: !Boolean
    } deriving (Show)

data DeleteRoute = DeleteRoute
    { drRouteTableId :: !Text
    , drDestinationCidrBlock :: !Text
    } deriving (Show)

data DeleteRouteResponse = DeleteRouteResponse
    { drrRequestId :: !Text
    , drrReturn :: !Boolean
    } deriving (Show)

data CreateNetworkAcl = CreateNetworkAcl
    { cnaVpcId :: !Text
    } deriving (Show)

data NetworkAclEntrySet = NetworkAclEntrySet
    { naesItem :: ![NetworkAclEntry]
    } deriving (Show)

data IcmpCode = IcmpCode
    { icCode :: !Int
    , icType :: !Int
    } deriving (Show)

data PortRange = PortRange
    { prFrom :: !Int
    , prTo :: !Int
    } deriving (Show)

data NetworkAclEntry = NetworkAclEntry
    { naeRuleNumber :: !Int
    , naeProtocol :: !Text
    , naeRuleAction :: !Text
    , naeEgress :: !Boolean
    , naeCidrBlock :: !Text
    , naeIcmpCode :: !(Maybe IcmpCode)
    , naePortRange :: !(Maybe PortRange)
    } deriving (Show)

data NetworkAclAssociationSet = NetworkAclAssociationSet
    { naasItem :: ![NetworkAclAssociation]
    } deriving (Show)

data NetworkAclAssociation = NetworkAclAssociation
    { naaNetworkAclAssociationId :: !Text
    , naaNetworkAclId :: !Text
    , naaSubnetId :: !Text
    } deriving (Show)

data NetworkAcl = NetworkAcl
    { naNetworkAclId :: !Text
    , naVpcId :: !Text
    , naDefault :: !Boolean
    , naEntrySet :: !NetworkAclEntrySet
    , naAssociationSet :: !NetworkAclAssociationSet
    , naTagSet :: !(Maybe ResourceTagSet)
    } deriving (Show)

data CreateNetworkAclResponse = CreateNetworkAclResponse
    { cnarRequestId :: !Text
    , cnarNetworkAcl :: !NetworkAcl
    } deriving (Show)

data NetworkAclIdSet = NetworkAclIdSet
    { naisItem :: ![NetworkAclIdSetItem]
    } deriving (Show)

data NetworkAclIdSetItem = NetworkAclIdSetItem
    { naisiNetworkAclId :: !Text
    } deriving (Show)

data DescribeNetworkAcls = DescribeNetworkAcls
    { dnaNetworkAclIdSet :: !NetworkAclIdSet
    , dnaFilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data NetworkAclSet = NetworkAclSet
    { nasItem :: ![NetworkAcl]
    } deriving (Show)

data DescribeNetworkAclsResponse = DescribeNetworkAclsResponse
    { dnarRequestId :: !Text
    , dnarNetworkAclSet :: !NetworkAclSet
    } deriving (Show)

data DeleteNetworkAcl = DeleteNetworkAcl
    { dnaNetworkAclId :: !Text
    } deriving (Show)

data DeleteNetworkAclResponse = DeleteNetworkAclResponse
    { dnarRequestId :: !Text
    , dnarReturn :: !Boolean
    } deriving (Show)

data ReplaceNetworkAclAssociation = ReplaceNetworkAclAssociation
    { rnaaAssociationId :: !Text
    , rnaaNetworkAclId :: !Text
    } deriving (Show)

data ReplaceNetworkAclAssociationResponse = ReplaceNetworkAclAssociationResponse
    { rnaarRequestId :: !Text
    , rnaarNewAssociationId :: !Text
    } deriving (Show)

data CreateNetworkAclEntry = CreateNetworkAclEntry
    { cnaeNetworkAclId :: !Text
    , cnaeRuleNumber :: !Int
    , cnaeProtocol :: !Text
    , cnaeRuleAction :: !Text
    , cnaeEgress :: !Boolean
    , cnaeCidrBlock :: !Text
    , cnaeIcmpCode :: !(Maybe IcmpCode)
    , cnaePortRange :: !(Maybe PortRange)
    } deriving (Show)

data CreateNetworkAclEntryResponse = CreateNetworkAclEntryResponse
    { cnaerRequestId :: !Text
    , cnaerReturn :: !Boolean
    } deriving (Show)

data ReplaceNetworkAclEntry = ReplaceNetworkAclEntry
    { rnaeNetworkAclId :: !Text
    , rnaeRuleNumber :: !Int
    , rnaeProtocol :: !Text
    , rnaeRuleAction :: !Text
    , rnaeEgress :: !Boolean
    , rnaeCidrBlock :: !Text
    , rnaeIcmpCode :: !(Maybe IcmpCode)
    , rnaePortRange :: !(Maybe PortRange)
    } deriving (Show)

data ReplaceNetworkAclEntryResponse = ReplaceNetworkAclEntryResponse
    { rnaerRequestId :: !Text
    , rnaerReturn :: !Boolean
    } deriving (Show)

data DeleteNetworkAclEntry = DeleteNetworkAclEntry
    { dnaeNetworkAclId :: !Text
    , dnaeRuleNumber :: !Int
    , dnaeEgress :: !Boolean
    } deriving (Show)

data DeleteNetworkAclEntryResponse = DeleteNetworkAclEntryResponse
    { dnaerRequestId :: !Text
    , dnaerReturn :: !Boolean
    } deriving (Show)

data DescribeInstanceStatus = DescribeInstanceStatus
    { disInstancesSet :: !InstanceIdSet
    , disFilterSet :: !(Maybe FilterSet)
    , disNextToken :: !(Maybe Text)
    , disMaxResults :: !(Maybe Int)
    , disIncludeAllInstances :: !(Maybe Boolean)
    } deriving (Show)

data DescribeInstanceStatusResponse = DescribeInstanceStatusResponse
    { disrRequestId :: !Text
    , disrInstanceStatusSet :: !InstanceStatusSet
    , disrNextToken :: !(Maybe Text)
    } deriving (Show)

data InstanceStatusSet = InstanceStatusSet
    { issItem :: ![InstanceStatusItem]
    } deriving (Show)

data InstanceStatus = InstanceStatus
    { isStatus :: !Text
    , isDetails :: !(Maybe InstanceStatusDetailsSet)
    } deriving (Show)

data InstanceStatusDetailsSet = InstanceStatusDetailsSet
    { isdsItem :: ![InstanceStatusDetailsSetItem]
    } deriving (Show)

data InstanceStatusDetailsSetItem = InstanceStatusDetailsSetItem
    { isdsiName :: !Text
    , isdsiStatus :: !Text
    , isdsiImpairedSince :: !(Maybe DateTime)
    } deriving (Show)

data InstanceStatusEvent = InstanceStatusEvent
    { iseCode :: !Text
    , iseDescription :: !Text
    , iseNotBefore :: !DateTime
    , iseNotAfter :: !(Maybe DateTime)
    } deriving (Show)

data InstanceStatusEventsSet = InstanceStatusEventsSet
    { isesItem :: ![InstanceStatusEvent]
    } deriving (Show)

data InstanceStatusItem = InstanceStatusItem
    { isiInstanceId :: !Text
    , isiAvailabilityZone :: !Text
    , isiEventsSet :: !(Maybe InstanceStatusEventsSet)
    , isiInstanceState :: !InstanceState
    , isiSystemStatus :: !InstanceStatus
    , isiInstanceStatus :: !InstanceStatus
    } deriving (Show)

data ReportInstanceStatus = ReportInstanceStatus
    { risInstancesSet :: !InstanceIdSet
    , risStatus :: !Text
    , risStartTime :: !(Maybe DateTime)
    , risEndTime :: !(Maybe DateTime)
    , risReasonCodesSet :: !ReportInstanceStatusReasonCodesSet
    , risDescription :: !(Maybe Text)
    } deriving (Show)

data ReportInstanceStatusReasonCodesSet = ReportInstanceStatusReasonCodesSet
    { risrcsItem :: !(NonEmpty ReportInstanceStatusReasonCodeSetItem)
    } deriving (Show)

data ReportInstanceStatusReasonCodeSetItem = ReportInstanceStatusReasonCodeSetItem
    { risrcsiReasonCode :: !Text
    } deriving (Show)

data ReportInstanceStatusResponse = ReportInstanceStatusResponse
    { risrRequestId :: !Text
    , risrReturn :: !Boolean
    } deriving (Show)

data CreateNetworkInterface = CreateNetworkInterface
    { cniSubnetId :: !Text
    , cniDescription :: !(Maybe Text)
    , cniPrivateIpAddress :: !(Maybe Text)
    , cniGroupSet :: !(Maybe SecurityGroupIdSet)
    , cniPrivateIpAddressesSet :: !(Maybe PrivateIpAddressesSetRequest)
    , cniSecondaryPrivateIpAddressCount :: !(Maybe Int)
    } deriving (Show)

data CreateNetworkInterfaceResponse = CreateNetworkInterfaceResponse
    { cnirRequestId :: !Text
    , cnirNetworkInterface :: !NetworkInterface
    } deriving (Show)

data NetworkInterfaceIdSet = NetworkInterfaceIdSet
    { niisItem :: ![NetworkInterfaceIdSetItem]
    } deriving (Show)

data NetworkInterfaceIdSetItem = NetworkInterfaceIdSetItem
    { niisiNetworkInterfaceId :: !Text
    } deriving (Show)

data DescribeNetworkInterfaces = DescribeNetworkInterfaces
    { dniNetworkInterfaceIdSet :: !(Maybe NetworkInterfaceIdSet)
    , dniFilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data NetworkInterface = NetworkInterface
    { niNetworkInterfaceId :: !Text
    , niSubnetId :: !(Maybe Text)
    , niVpcId :: !(Maybe Text)
    , niAvailabilityZone :: !(Maybe Text)
    , niDescription :: !(Maybe Text)
    , niOwnerId :: !Text
    , niRequesterId :: !(Maybe Text)
    , niRequesterManaged :: !(Maybe Boolean)
    , niStatus :: !Text
    , niMacAddress :: !Text
    , niPrivateIpAddress :: !Text
    , niPrivateDnsName :: !(Maybe Text)
    , niSourceDestCheck :: !Boolean
    , niGroupSet :: !GroupSet
    , niAttachment :: !(Maybe NetworkInterfaceAttachment)
    , niAssociation :: !(Maybe NetworkInterfaceAssociation)
    , niTagSet :: !(Maybe ResourceTagSet)
    , niPrivateIpAddressesSet :: !(Maybe NetworkInterfacePrivateIpAddressesSet)
    } deriving (Show)

data NetworkInterfacePrivateIpAddressesSet = NetworkInterfacePrivateIpAddressesSet
    { nipiasItem :: ![NetworkInterfacePrivateIpAddressesSetItem]
    } deriving (Show)

data NetworkInterfacePrivateIpAddressesSetItem = NetworkInterfacePrivateIpAddressesSetItem
    { nipiasiPrivateIpAddress :: !Text
    , nipiasiPrivateDnsName :: !(Maybe Text)
    , nipiasiPrimary :: !Boolean
    , nipiasiAssociation :: !(Maybe NetworkInterfaceAssociation)
    } deriving (Show)

data NetworkInterfaceAttachment = NetworkInterfaceAttachment
    { niaAttachmentId :: !Text
    , niaInstanceId :: !(Maybe Text)
    , niaInstanceOwnerId :: !(Maybe Text)
    , niaDeviceIndex :: !Int
    , niaStatus :: !Text
    , niaAttachTime :: !DateTime
    , niaDeleteOnTermination :: !Boolean
    } deriving (Show)

data NetworkInterfaceAssociation = NetworkInterfaceAssociation
    { niaPublicIp :: !Text
    , niaPublicDnsName :: !(Maybe Text)
    , niaIpOwnerId :: !(Maybe Text)
    , niaAllocationId :: !(Maybe Text)
    , niaAssociationId :: !(Maybe Text)
    } deriving (Show)

data NetworkInterfaceSet = NetworkInterfaceSet
    { nisItem :: ![NetworkInterface]
    } deriving (Show)

data DescribeNetworkInterfacesResponse = DescribeNetworkInterfacesResponse
    { dnirRequestId :: !Text
    , dnirNetworkInterfaceSet :: !NetworkInterfaceSet
    } deriving (Show)

data DeleteNetworkInterface = DeleteNetworkInterface
    { dniNetworkInterfaceId :: !Text
    } deriving (Show)

data DeleteNetworkInterfaceResponse = DeleteNetworkInterfaceResponse
    { dnirRequestId :: !Text
    , dnirReturn :: !Boolean
    } deriving (Show)

data AttachNetworkInterface = AttachNetworkInterface
    { aniNetworkInterfaceId :: !Text
    , aniInstanceId :: !Text
    , aniDeviceIndex :: !Int
    } deriving (Show)

data AttachNetworkInterfaceResponse = AttachNetworkInterfaceResponse
    { anirRequestId :: !Text
    , anirAttachmentId :: !Text
    } deriving (Show)

data DetachNetworkInterface = DetachNetworkInterface
    { dniAttachmentId :: !Text
    , dniForce :: !(Maybe Boolean)
    } deriving (Show)

data DetachNetworkInterfaceResponse = DetachNetworkInterfaceResponse
    { dnirRequestId :: !Text
    , dnirReturn :: !Boolean
    } deriving (Show)

data DescribeNetworkInterfaceAttribute = DescribeNetworkInterfaceAttribute
    { dniaNetworkInterfaceId :: !Text
    , dniaDescribeNetworkInterfaceAttributesGroup :: !DescribeNetworkInterfaceAttributesGroup
    } deriving (Show)

data DescribeNetworkInterfaceAttributesGroup = DescribeNetworkInterfaceAttributesGroup
    { dniagDescription :: !EmptyElement
    , dniagSourceDestCheck :: !EmptyElement
    , dniagGroupSet :: !EmptyElement
    , dniagAttachment :: !EmptyElement
    } deriving (Show)
data DescribeNetworkInterfaceAttributeResponse = DescribeNetworkInterfaceAttributeResponse
    { dniarRequestId :: !Text
    , dniarNetworkInterfaceId :: !Text
  -- <xs:choice>
    , dniarDescription :: !(Maybe NullableAttributeValue)
    , dniarSourceDestCheck :: !(Maybe AttributeBooleanValue)
    , dniarGroupSet :: !(Maybe GroupSet)
    , dniarAttachment :: !(Maybe NetworkInterfaceAttachment)
  -- </xs:choice>
    } deriving (Show)

data ModifyNetworkInterfaceAttribute = ModifyNetworkInterfaceAttribute
    { mniaNetworkInterfaceId :: !Text
  -- <xs:choice>
    , mniaDescription :: !(Maybe NullableAttributeValue)
    , mniaSourceDestCheck :: !(Maybe AttributeBooleanValue)
    , mniaGroupSet :: !(Maybe SecurityGroupIdSet)
    , mniaAttachment :: !(Maybe ModifyNetworkInterfaceAttachment)
  -- </xs:choice>
    } deriving (Show)

data ModifyNetworkInterfaceAttachment = ModifyNetworkInterfaceAttachment
    { mniaAttachmentId :: !Text
    , mniaDeleteOnTermination :: !Boolean
    } deriving (Show)

data ModifyNetworkInterfaceAttributeResponse = ModifyNetworkInterfaceAttributeResponse
    { mniarRequestId :: !Text
    , mniarReturn :: !Boolean
    } deriving (Show)

data ResetNetworkInterfaceAttribute = ResetNetworkInterfaceAttribute
    { rniaNetworkInterfaceId :: !Text
    , rniaResetNetworkInterfaceAttributesGroup :: !ResetNetworkInterfaceAttributesGroup
    } deriving (Show)

data ResetNetworkInterfaceAttributesGroup = ResetNetworkInterfaceAttributesGroup
    { rniagSourceDestCheck :: !EmptyElement
    } deriving (Show)
data ResetNetworkInterfaceAttributeResponse = ResetNetworkInterfaceAttributeResponse
    { rniarRequestId :: !Text
    , rniarReturn :: !Boolean
    } deriving (Show)

data AssignPrivateIpAddresses = AssignPrivateIpAddresses
    { apiaNetworkInterfaceId :: !Text
    , apiaPrivateIpAddressesSet :: !(Maybe AssignPrivateIpAddressesSetRequest)
    , apiaSecondaryPrivateIpAddressCount :: !(Maybe Int)
    , apiaAllowReassignment :: !(Maybe Boolean)
    } deriving (Show)

data AssignPrivateIpAddressesResponse = AssignPrivateIpAddressesResponse
    { apiarRequestId :: !Text
    , apiarReturn :: !Boolean
    } deriving (Show)

data UnassignPrivateIpAddresses = UnassignPrivateIpAddresses
    { upiaNetworkInterfaceId :: !Text
    , upiaPrivateIpAddressesSet :: !AssignPrivateIpAddressesSetRequest
    } deriving (Show)

data UnassignPrivateIpAddressesResponse = UnassignPrivateIpAddressesResponse
    { upiarRequestId :: !Text
    , upiarReturn :: !Boolean
    } deriving (Show)

data AssignPrivateIpAddressesSetRequest = AssignPrivateIpAddressesSetRequest
    { apiasrItem :: !(NonEmpty AssignPrivateIpAddressesSetItemRequest)
    } deriving (Show)

data AssignPrivateIpAddressesSetItemRequest = AssignPrivateIpAddressesSetItemRequest
    { apiasirPrivateIpAddress :: !Text
    } deriving (Show)

data DescribeVolumeStatus = DescribeVolumeStatus
    { dvsVolumeSet :: !DescribeVolumesSet
    , dvsFilterSet :: !(Maybe FilterSet)
    , dvsMaxResults :: !(Maybe Integer)
    , dvsNextToken :: !(Maybe Text)
    } deriving (Show)

data DescribeVolumeStatusResponse = DescribeVolumeStatusResponse
    { dvsrRequestId :: !Text
    , dvsrVolumeStatusSet :: !VolumeStatusSet
    , dvsrNextToken :: !(Maybe Text)
    } deriving (Show)

data VolumeStatusSet = VolumeStatusSet
    { vssItem :: ![VolumeStatusItem]
    } deriving (Show)

data VolumeStatusItem = VolumeStatusItem
    { vsiVolumeId :: !Text
    , vsiAvailabilityZone :: !Text
    , vsiVolumeStatus :: !VolumeStatusInfo
    , vsiEventsSet :: !VolumeStatusEventsSet
    , vsiActionsSet :: !VolumeStatusActionsSet
    } deriving (Show)

data VolumeStatusInfo = VolumeStatusInfo
    { vsiStatus :: !Text
    , vsiDetails :: !VolumeStatusDetailsSet
    } deriving (Show)

data VolumeStatusDetailsSet = VolumeStatusDetailsSet
    { vsdsItem :: ![VolumeStatusDetailsItem]
    } deriving (Show)

data VolumeStatusDetailsItem = VolumeStatusDetailsItem
    { vsdiName :: !Text
    , vsdiStatus :: !Text
    } deriving (Show)

data VolumeStatusEventsSet = VolumeStatusEventsSet
    { vsesItem :: ![VolumeStatusEventItem]
    } deriving (Show)

data VolumeStatusEventItem = VolumeStatusEventItem
    { vseiDescription :: !Text
    , vseiNotBefore :: !DateTime
    , vseiNotAfter :: !DateTime
    , vseiEventId :: !Text
    , vseiEvent :: !Text
    } deriving (Show)

data VolumeStatusActionsSet = VolumeStatusActionsSet
    { vsasItem :: ![VolumeStatusActionItem]
    } deriving (Show)

data VolumeStatusActionItem = VolumeStatusActionItem
    { vsaiDescription :: !Text
    , vsaiCode :: !Text
    , vsaiEventId :: !Text
    , vsaiEvent :: !Text
    } deriving (Show)

data EnableVolumeIO = EnableVolumeIO
    { evioVolumeId :: !Text
    } deriving (Show)

data EnableVolumeIOResponse = EnableVolumeIOResponse
    { eviorRequestId :: !Text
    , eviorReturn :: !Boolean
    } deriving (Show)

data ModifyVolumeAttribute = ModifyVolumeAttribute
    { mvaVolumeId :: !Text
  -- <xs:choice>
    , mvaAutoEnableIO :: !AttributeBooleanValue
  -- </xs:choice>
    } deriving (Show)

data ModifyVolumeAttributeResponse = ModifyVolumeAttributeResponse
    { mvarRequestId :: !Text
    , mvarReturn :: !Boolean
    } deriving (Show)

data DescribeVolumeAttribute = DescribeVolumeAttribute
    { dvaVolumeId :: !Text
    , dvaDescribeVolumeAttributesGroup :: !DescribeVolumeAttributesGroup
    } deriving (Show)

data DescribeVolumeAttributesGroup = DescribeVolumeAttributesGroup
    { dvagAutoEnableIO :: !EmptyElement
    , dvagProductCodes :: !EmptyElement
    } deriving (Show)

data DescribeVolumeAttributeResponse = DescribeVolumeAttributeResponse
    { dvarRequestId :: !Text
    , dvarVolumeId :: !Text
  -- <xs:choice>
    , dvarAutoEnableIO :: !(Maybe NullableAttributeBooleanValue)
    , dvarProductCodes :: !(Maybe ProductCodesSet)
  -- </xs:choice>
    } deriving (Show)
