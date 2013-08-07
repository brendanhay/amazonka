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
import Data.Text
import Network.AWS.Internal

data CreateImage = CreateImage
    { ciInstanceId :: !String
    , ciName :: !String
    , ciDescription :: !(Maybe String)
    , ciNoReboot :: !(Maybe Boolean)
    , ciBlockDeviceMapping :: !(Maybe BlockDeviceMapping)
    } deriving (Show)

data CreateImageResponse = CreateImageResponse
    { cirRequestId :: !String
    , cirImageId :: !String
    } deriving (Show)

data RegisterImage = RegisterImage
    { riImageLocation :: !(Maybe String)
    , riName :: !String
    , riDescription :: !(Maybe String)
    , riArchitecture :: !(Maybe String)
    , riKernelId :: !(Maybe String)
    , riRamdiskId :: !(Maybe String)
    , riRootDeviceName :: !(Maybe String)
    , riBlockDeviceMapping :: !(Maybe BlockDeviceMapping)
    } deriving (Show)

data RegisterImageResponse = RegisterImageResponse
    { rirRequestId :: !String
    , rirImageId :: !String
    } deriving (Show)

data DeregisterImage = DeregisterImage
    { diImageId :: !String
    } deriving (Show)

data DeregisterImageResponse = DeregisterImageResponse
    { dirRequestId :: !String
    , dirReturn :: !Boolean
    } deriving (Show)

data CreateKeyPair = CreateKeyPair
    { ckpKeyName :: !String
    } deriving (Show)

data CreateKeyPairResponse = CreateKeyPairResponse
    { ckprRequestId :: !String
    , ckprKeyName :: !String
    , ckprKeyFingerprint :: !String
    , ckprKeyMaterial :: !String
    } deriving (Show)

data ImportKeyPair = ImportKeyPair
    { ikpKeyName :: !String
    , ikpPublicKeyMaterial :: !String
    } deriving (Show)

data ImportKeyPairResponse = ImportKeyPairResponse
    { ikprRequestId :: !String
    , ikprKeyName :: !String
    , ikprKeyFingerprint :: !String
    } deriving (Show)

data DeleteKeyPair = DeleteKeyPair
    { dkpKeyName :: !String
    } deriving (Show)

data DeleteKeyPairResponse = DeleteKeyPairResponse
    { dkprRequestId :: !String
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
    { dkpiKeyName :: !String
    } deriving (Show)

data DescribeKeyPairsResponse = DescribeKeyPairsResponse
    { dkprRequestId :: !String
    , dkprKeySet :: !DescribeKeyPairsResponseInfo
    } deriving (Show)

data DescribeKeyPairsResponseInfo = DescribeKeyPairsResponseInfo
    { dkpriItem :: ![DescribeKeyPairsResponseItem]
    } deriving (Show)

data DescribeKeyPairsResponseItem = DescribeKeyPairsResponseItem
    { dkpriKeyName :: !String
    , dkpriKeyFingerprint :: !String
    } deriving (Show)

data RunInstances = RunInstances
    { riImageId :: !String
    , riMinCount :: !Int
    , riMaxCount :: !Int
    , riKeyName :: !(Maybe String)
    , riGroupSet :: !GroupSet
    , riUserData :: !(Maybe UserData)
    , riInstance :: !String
    , riPlacement :: !(Maybe PlacementRequest)
    , riKernelId :: !(Maybe String)
    , riRamdiskId :: !(Maybe String)
    , riBlockDeviceMapping :: !(Maybe BlockDeviceMapping)
    , riMonitoring :: !(Maybe MonitoringInstance)
    , riSubnetId :: !(Maybe String)
    , riDisableApiTermination :: !(Maybe Boolean)
    , riInstanceInitiatedShutdownBehavior :: !(Maybe String)
    , riLicense :: !(Maybe InstanceLicenseRequest)
    , riPrivateIpAddress :: !(Maybe String)
    , riClientToken :: !(Maybe String)
    , riNetworkInterfaceSet :: !(Maybe InstanceNetworkInterfaceSetRequest)
    , riIamInstanceProfile :: !(Maybe IamInstanceProfileRequest)
    , riEbsOptimized :: !(Maybe Boolean)
    } deriving (Show)

data IamInstanceProfileRequest = IamInstanceProfileRequest
    { iiprArn :: !(Maybe String)
    , iiprName :: !(Maybe String)
    } deriving (Show)

data InstanceNetworkInterfaceSetRequest = InstanceNetworkInterfaceSetRequest
    { inisrItem :: ![InstanceNetworkInterfaceSetItemRequest]
    } deriving (Show)

data InstanceNetworkInterfaceSetItemRequest = InstanceNetworkInterfaceSetItemRequest
    { inisirNetworkInterfaceId :: !(Maybe String)
    , inisirDeviceIndex :: !Int
    , inisirSubnetId :: !(Maybe String)
    , inisirDescription :: !(Maybe String)
    , inisirPrivateIpAddress :: !(Maybe String)
    , inisirGroupSet :: !(Maybe SecurityGroupIdSet)
    , inisirDeleteOnTermination :: !(Maybe Boolean)
    , inisirPrivateIpAddressesSet :: !(Maybe PrivateIpAddressesSetRequest)
    , inisirSecondaryPrivateIpAddressCount :: !(Maybe Int)
    } deriving (Show)

data PrivateIpAddressesSetRequest = PrivateIpAddressesSetRequest
    { piasrItem :: ![PrivateIpAddressesSetItemRequest]
    } deriving (Show)

data PrivateIpAddressesSetItemRequest = PrivateIpAddressesSetItemRequest
    { piasirPrivateIpAddress :: !String
    , piasirPrimary :: !(Maybe Boolean)
    } deriving (Show)

data ImportInstanceGroupSet = ImportInstanceGroupSet
    { iigsItem :: ![ImportInstanceGroupItem]
    } deriving (Show)

data ImportInstanceGroupItem = ImportInstanceGroupItem
    { iigiGroupId :: !(Maybe String)
    , iigiGroupName :: !(Maybe String)
    } deriving (Show)

data GroupSet = GroupSet
    { gsItem :: ![GroupItem]
    } deriving (Show)

data GroupItem = GroupItem
    { giGroupId :: !(Maybe String)
    , giGroupName :: !(Maybe String)
    } deriving (Show)

data UserData = UserData
    { udData :: !(Maybe String)
    , udVersion :: !String
    , udEncoding :: !String
    } deriving (Show)

data BlockDeviceMapping = BlockDeviceMapping
    { bdmItem :: ![BlockDeviceMappingItem]
    } deriving (Show)

data BlockDeviceMappingItem = BlockDeviceMappingItem
    { bdmiDeviceName :: !String
  -- <xs:choice>
    , bdmiVirtualName :: !(Maybe String)
    , bdmiEbs :: !(Maybe EbsBlockDevice)
    , bdmiNoDevice :: !(Maybe EmptyElement)
  -- </xs:choice>
    } deriving (Show)

data EbsBlockDevice = EbsBlockDevice
    { ebdSnapshotId :: !(Maybe String)
    , ebdVolumeSize :: !(Maybe Int)
    , ebdDeleteOnTermination :: !(Maybe Boolean)
    , ebdVolume :: !(Maybe String)
    , ebdIops :: !(Maybe Int)
    } deriving (Show)

data PlacementRequest = PlacementRequest
    { prAvailabilityZone :: !(Maybe String)
    , prGroupName :: !(Maybe String)
    , prTenancy :: !(Maybe String)
    } deriving (Show)

data SpotPlacementRequest = SpotPlacementRequest
    { sprAvailabilityZone :: !(Maybe String)
    , sprGroupName :: !(Maybe String)
    } deriving (Show)

data InstancePlacement = InstancePlacement
    { ipAvailabilityZone :: !(Maybe String)
    , ipGroupName :: !(Maybe String)
    } deriving (Show)

data MonitoringInstance = MonitoringInstance
    { miEnabled :: !(Maybe Boolean)
    } deriving (Show)

data InstanceLicenseRequest = InstanceLicenseRequest
    { ilrPool :: !String
    } deriving (Show)

data RunInstancesResponse = RunInstancesResponse
    { rirRequestId :: !String
    , rirReservationId :: !String
    , rirOwnerId :: !String
    , rirGroupSet :: !GroupSet
    , rirInstancesSet :: !RunningInstancesSet
    , rirRequesterId :: !(Maybe String)
    } deriving (Show)

data ReservationInfo = ReservationInfo
    { riReservationId :: !String
    , riOwnerId :: !String
    , riGroupSet :: !GroupSet
    , riInstancesSet :: !RunningInstancesSet
    , riRequesterId :: !(Maybe String)
    } deriving (Show)

data RunningInstancesSet = RunningInstancesSet
    { risItem :: ![RunningInstancesItem]
    } deriving (Show)

data RunningInstancesItem = RunningInstancesItem
    { riiInstanceId :: !String
    , riiImageId :: !(Maybe String)
    , riiInstanceState :: !InstanceState
    , riiPrivateDnsName :: !String
    , riiDnsName :: !(Maybe String)
    , riiReason :: !(Maybe String)
    , riiKeyName :: !(Maybe String)
    , riiAmiLaunchIndex :: !(Maybe String)
    , riiProductCodes :: !(Maybe ProductCodesSet)
    , riiInstance :: !String
    , riiLaunchTime :: !DateTime
    , riiPlacement :: !(Maybe PlacementResponse)
    , riiKernelId :: !(Maybe String)
    , riiRamdiskId :: !(Maybe String)
    , riiPlatform :: !(Maybe String)
    , riiMonitoring :: !(Maybe InstanceMonitoringState)
    , riiSubnetId :: !(Maybe String)
    , riiVpcId :: !(Maybe String)
    , riiPrivateIpAddress :: !(Maybe String)
    , riiIpAddress :: !(Maybe String)
    , riiSourceDestCheck :: !(Maybe Boolean)
    , riiGroupSet :: !GroupSet
    , riiStateReason :: !(Maybe StateReason)
    , riiArchitecture :: !(Maybe String)
    , riiRootDevice :: !(Maybe String)
    , riiRootDeviceName :: !(Maybe String)
    , riiBlockDeviceMapping :: !(Maybe InstanceBlockDeviceMappingResponse)
    , riiInstanceLifecycle :: !(Maybe String)
    , riiSpotInstanceRequestId :: !(Maybe String)
    , riiLicense :: !(Maybe InstanceLicenseResponse)
    , riiVirtualization :: !(Maybe String)
    , riiClientToken :: !(Maybe String)
    , riiTagSet :: !(Maybe ResourceTagSet)
    , riiHypervisor :: !(Maybe String)
    , riiNetworkInterfaceSet :: !(Maybe InstanceNetworkInterfaceSet)
    , riiIamInstanceProfile :: !(Maybe IamInstanceProfileResponse)
    , riiEbsOptimized :: !(Maybe Boolean)
    } deriving (Show)

data IamInstanceProfileResponse = IamInstanceProfileResponse
    { iiprArn :: !String
    , iiprId :: !String
    } deriving (Show)

data InstanceNetworkInterfaceSet = InstanceNetworkInterfaceSet
    { inisItem :: ![InstanceNetworkInterfaceSetItem]
    } deriving (Show)

data InstanceNetworkInterfaceSetItem = InstanceNetworkInterfaceSetItem
    { inisiNetworkInterfaceId :: !String
    , inisiSubnetId :: !(Maybe String)
    , inisiVpcId :: !(Maybe String)
    , inisiDescription :: !(Maybe String)
    , inisiOwnerId :: !String
    , inisiStatus :: !String
    , inisiMacAddress :: !(Maybe String)
    , inisiPrivateIpAddress :: !(Maybe String)
    , inisiPrivateDnsName :: !(Maybe String)
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
    { ipiasiPrivateIpAddress :: !(Maybe String)
    , ipiasiPrivateDnsName :: !(Maybe String)
    , ipiasiPrimary :: !(Maybe Boolean)
    , ipiasiAssociation :: !(Maybe InstanceNetworkInterfaceAssociation)
    } deriving (Show)

data InstanceNetworkInterfaceAttachment = InstanceNetworkInterfaceAttachment
    { iniaAttachmentId :: !String
    , iniaDeviceIndex :: !Int
    , iniaStatus :: !String
    , iniaAttachTime :: !DateTime
    , iniaDeleteOnTermination :: !Boolean
    } deriving (Show)

data InstanceNetworkInterfaceAssociation = InstanceNetworkInterfaceAssociation
    { iniaPublicIp :: !String
    , iniaPublicDnsName :: !(Maybe String)
    , iniaIpOwnerId :: !(Maybe String)
    } deriving (Show)

data PlacementResponse = PlacementResponse
    { prAvailabilityZone :: !String
    , prGroupName :: !(Maybe String)
    , prTenancy :: !(Maybe String)
    } deriving (Show)

data StateReason = StateReason
    { srCode :: !String
    , srMessage :: !String
    } deriving (Show)

data InstanceBlockDeviceMappingResponse = InstanceBlockDeviceMappingResponse
    { ibdmrItem :: ![InstanceBlockDeviceMappingResponseItem]
    } deriving (Show)

data InstanceBlockDeviceMappingResponseItem = InstanceBlockDeviceMappingResponseItem
    { ibdmriDeviceName :: !String
  -- <xs:choice>
    , ibdmriEbs :: !EbsInstanceBlockDeviceMappingResponse
  -- </xs:choice>
    } deriving (Show)

data EbsInstanceBlockDeviceMappingResponse = EbsInstanceBlockDeviceMappingResponse
    { eibdmrVolumeId :: !String
    , eibdmrStatus :: !String
    , eibdmrAttachTime :: !DateTime
    , eibdmrDeleteOnTermination :: !(Maybe Boolean)
    } deriving (Show)

data InstanceLicenseResponse = InstanceLicenseResponse
    { ilrPool :: !String
    } deriving (Show)

data DescribeAccountAttributes = DescribeAccountAttributes
    { daaAccountAttributeNameSet :: !(Maybe AccountAttributeNameSet)
    , daaFilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data DescribeAccountAttributesResponse = DescribeAccountAttributesResponse
    { daarRequestId :: !String
    , daarAccountAttributeSet :: !(Maybe AccountAttributeSet)
    } deriving (Show)

data AccountAttributeNameSet = AccountAttributeNameSet
    { aansItem :: ![AccountAttributeNameSetItem]
    } deriving (Show)

data AccountAttributeNameSetItem = AccountAttributeNameSetItem
    { aansiAttributeName :: !String
    } deriving (Show)

data AccountAttributeSet = AccountAttributeSet
    { aasItem :: ![AccountAttributeSetItem]
    } deriving (Show)

data AccountAttributeSetItem = AccountAttributeSetItem
    { aasiAttributeName :: !String
    , aasiAttributeValueSet :: !AccountAttributeValueSet
    } deriving (Show)

data AccountAttributeValueSet = AccountAttributeValueSet
    { aavsItem :: ![AccountAttributeValueSetItem]
    } deriving (Show)

data AccountAttributeValueSetItem = AccountAttributeValueSetItem
    { aavsiAttributeValue :: !String
    } deriving (Show)

data DescribeVpcAttribute = DescribeVpcAttribute
    { dvaVpcId :: !String
    , dvaDescribeVpcAttributesGroup :: !DescribeVpcAttributesGroup
    } deriving (Show)

data DescribeVpcAttributesGroup = DescribeVpcAttributesGroup
    { dvagEnableDnsSupport :: !EmptyElement
    , dvagEnableDnsHostnames :: !EmptyElement
    } deriving (Show)
data DescribeVpcAttributeResponse = DescribeVpcAttributeResponse
    { dvarRequestId :: !String
    , dvarVpcId :: !String
  -- <xs:choice>
    , dvarEnableDnsSupport :: !(Maybe AttributeBooleanValue)
    , dvarEnableDnsHostnames :: !(Maybe AttributeBooleanValue)
  -- </xs:choice>
    } deriving (Show)

data ModifyVpcAttribute = ModifyVpcAttribute
    { mvaVpcId :: !String
  -- <xs:choice>
    , mvaEnableDnsSupport :: !(Maybe AttributeBooleanValue)
    , mvaEnableDnsHostnames :: !(Maybe AttributeBooleanValue)
  -- </xs:choice>
    } deriving (Show)

data ModifyVpcAttributeResponse = ModifyVpcAttributeResponse
    { mvarRequestId :: !String
    , mvarReturn :: !Boolean
    } deriving (Show)

data GetConsoleOutput = GetConsoleOutput
    { gcoInstanceId :: !String
    } deriving (Show)

data GetConsoleOutputResponse = GetConsoleOutputResponse
    { gcorRequestId :: !String
    , gcorInstanceId :: !String
    , gcorTimestamp :: !DateTime
    , gcorOutput :: !String
    } deriving (Show)

data GetPasswordData = GetPasswordData
    { gpdInstanceId :: !String
    } deriving (Show)

data GetPasswordDataResponse = GetPasswordDataResponse
    { gpdrRequestId :: !String
    , gpdrInstanceId :: !String
    , gpdrTimestamp :: !DateTime
    , gpdrPasswordData :: !String
    } deriving (Show)

data InstanceId = InstanceId
    { iiInstanceId :: !String
    } deriving (Show)

data InstanceIdSet = InstanceIdSet
    { iisItem :: ![InstanceId]
    } deriving (Show)

data InstanceStateChange = InstanceStateChange
    { iscInstanceId :: !String
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
    { tirRequestId :: !String
    , tirInstancesSet :: !InstanceStateChangeSet
    } deriving (Show)

data InstanceBlockDeviceMapping = InstanceBlockDeviceMapping
    { ibdmItem :: ![InstanceBlockDeviceMappingItem]
    } deriving (Show)

data InstanceBlockDeviceMappingItem = InstanceBlockDeviceMappingItem
    { ibdmiDeviceName :: !String
  -- <xs:choice>
    , ibdmiVirtualName :: !(Maybe String)
    , ibdmiEbs :: !(Maybe InstanceEbsBlockDevice)
    , ibdmiNoDevice :: !(Maybe EmptyElement)
  -- </xs:choice>
    } deriving (Show)

data InstanceEbsBlockDevice = InstanceEbsBlockDevice
    { iebdVolumeId :: !String
    , iebdDeleteOnTermination :: !(Maybe Boolean)
    } deriving (Show)

data StopInstances = StopInstances
    { siInstancesSet :: !InstanceIdSet
    , siForce :: !(Maybe Boolean)
    } deriving (Show)

data StopInstancesResponse = StopInstancesResponse
    { sirRequestId :: !String
    , sirInstancesSet :: !InstanceStateChangeSet
    } deriving (Show)

data StartInstances = StartInstances
    { siInstancesSet :: !InstanceIdSet
    } deriving (Show)

data StartInstancesResponse = StartInstancesResponse
    { sirRequestId :: !String
    , sirInstancesSet :: !InstanceStateChangeSet
    } deriving (Show)

data RebootInstances = RebootInstances
    { riInstancesSet :: !RebootInstancesInfo
    } deriving (Show)

data RebootInstancesInfo = RebootInstancesInfo
    { riiItem :: !(NonEmpty RebootInstancesItem)
    } deriving (Show)

data RebootInstancesItem = RebootInstancesItem
    { riiInstanceId :: !String
    } deriving (Show)

data RebootInstancesResponse = RebootInstancesResponse
    { rirRequestId :: !String
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
    { diiInstanceId :: !String
    } deriving (Show)

data DescribeInstancesResponse = DescribeInstancesResponse
    { dirRequestId :: !String
    , dirReservationSet :: !ReservationSet
    } deriving (Show)

data ReservationSet = ReservationSet
    { rsItem :: ![ReservationInfo]
    } deriving (Show)

data UnavailableResultSet = UnavailableResultSet
    { ursItem :: ![UnavailableResult]
    } deriving (Show)

data UnavailableResult = UnavailableResult
    { urAvailabilityZone :: !String
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
    { diiImageId :: !String
    } deriving (Show)

data DescribeImagesOwners = DescribeImagesOwners
    { dioItem :: ![DescribeImagesOwner]
    } deriving (Show)

data DescribeImagesOwner = DescribeImagesOwner
    { dioOwner :: !String
    } deriving (Show)

data DescribeImagesExecutableBySet = DescribeImagesExecutableBySet
    { diebsItem :: ![DescribeImagesExecutableBy]
    } deriving (Show)

data DescribeImagesExecutableBy = DescribeImagesExecutableBy
    { diebUser :: !String
    } deriving (Show)

data DescribeImagesResponse = DescribeImagesResponse
    { dirRequestId :: !String
    , dirImagesSet :: !DescribeImagesResponseInfo
    } deriving (Show)

data DescribeImagesResponseInfo = DescribeImagesResponseInfo
    { diriItem :: ![DescribeImagesResponseItem]
    } deriving (Show)

data DescribeImagesResponseItem = DescribeImagesResponseItem
    { diriImageId :: !String
    , diriImageLocation :: !(Maybe String)
    , diriImageState :: !String
    , diriImageOwnerId :: !String
    , diriIsPublic :: !Boolean
    , diriProductCodes :: !(Maybe ProductCodesSet)
    , diriArchitecture :: !(Maybe String)
    , diriImage :: !(Maybe String)
    , diriKernelId :: !(Maybe String)
    , diriRamdiskId :: !(Maybe String)
    , diriPlatform :: !(Maybe String)
    , diriStateReason :: !(Maybe StateReason)
    , diriImageOwnerAlias :: !(Maybe String)
    , diriName :: !(Maybe String)
    , diriDescription :: !(Maybe String)
    , diriRootDevice :: !(Maybe String)
    , diriRootDeviceName :: !(Maybe String)
    , diriBlockDeviceMapping :: !(Maybe BlockDeviceMapping)
    , diriVirtualization :: !(Maybe String)
    , diriTagSet :: !(Maybe ResourceTagSet)
    , diriHypervisor :: !(Maybe String)
    } deriving (Show)

data CreateSecurityGroup = CreateSecurityGroup
    { csgGroupName :: !String
    , csgGroupDescription :: !String
    , csgVpcId :: !(Maybe String)
    } deriving (Show)

data CreateSecurityGroupResponse = CreateSecurityGroupResponse
    { csgrRequestId :: !String
    , csgrReturn :: !Boolean
    , csgrGroupId :: !String
    } deriving (Show)

data DeleteSecurityGroup = DeleteSecurityGroup
    {
-- <xs:choice>
      dsgGroupId :: !(Maybe String)
    , dsgGroupName :: !(Maybe String)
-- </xs:choice>
    } deriving (Show)

data DeleteSecurityGroupResponse = DeleteSecurityGroupResponse
    { dsgrRequestId :: !String
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
    { dsgsiGroupName :: !String
    } deriving (Show)

data DescribeSecurityGroupsIdSet = DescribeSecurityGroupsIdSet
    { dsgisItem :: ![DescribeSecurityGroupsIdSetItem]
    } deriving (Show)

data DescribeSecurityGroupsIdSetItem = DescribeSecurityGroupsIdSetItem
    { dsgisiGroupId :: !String
    } deriving (Show)

data DescribeSecurityGroupsResponse = DescribeSecurityGroupsResponse
    { dsgrRequestId :: !String
    , dsgrSecurityGroupInfo :: !SecurityGroupSet
    } deriving (Show)

data IpPermissionSet = IpPermissionSet
    { ipsItem :: ![IpPermission]
    } deriving (Show)

data IpPermission = IpPermission
    { ipIpProtocol :: !String
    , ipFromPort :: !(Maybe Int)
    , ipToPort :: !(Maybe Int)
    , ipGroups :: !UserIdGroupPairSet
    , ipIpRanges :: !IpRangeSet
    } deriving (Show)

data IpRangeSet = IpRangeSet
    { irsItem :: ![IpRangeItem]
    } deriving (Show)

data IpRangeItem = IpRangeItem
    { iriCidrIp :: !String
    } deriving (Show)

data UserIdGroupPairSet = UserIdGroupPairSet
    { uigpsItem :: ![UserIdGroupPair]
    } deriving (Show)

data UserIdGroupPair = UserIdGroupPair
    { uigpUserId :: !(Maybe String)
    , uigpGroupId :: !(Maybe String)
    , uigpGroupName :: !(Maybe String)
    } deriving (Show)

data SecurityGroupSet = SecurityGroupSet
    { sgsItem :: ![SecurityGroupItem]
    } deriving (Show)

data SecurityGroupItem = SecurityGroupItem
    { sgiOwnerId :: !String
    , sgiGroupId :: !String
    , sgiGroupName :: !String
    , sgiGroupDescription :: !String
    , sgiVpcId :: !(Maybe String)
    , sgiIpPermissions :: !IpPermissionSet
    , sgiIpPermissionsEgress :: !(Maybe IpPermissionSet)
    , sgiTagSet :: !(Maybe ResourceTagSet)
    } deriving (Show)

data AuthorizeSecurityGroupIngress = AuthorizeSecurityGroupIngress
    { asgiUserId :: !(Maybe String)
  -- <xs:choice>
    , asgiGroupId :: !(Maybe String)
    , asgiGroupName :: !(Maybe String)
  -- </xs:choice>
    , asgiIpPermissions :: !IpPermissionSet
    } deriving (Show)

data AuthorizeSecurityGroupIngressResponse = AuthorizeSecurityGroupIngressResponse
    { asgirRequestId :: !String
    , asgirReturn :: !Boolean
    } deriving (Show)

data RevokeSecurityGroupIngress = RevokeSecurityGroupIngress
    { rsgiUserId :: !(Maybe String)
  -- <xs:choice>
    , rsgiGroupId :: !(Maybe String)
    , rsgiGroupName :: !(Maybe String)
  -- </xs:choice>
    , rsgiIpPermissions :: !IpPermissionSet
    } deriving (Show)

data RevokeSecurityGroupIngressResponse = RevokeSecurityGroupIngressResponse
    { rsgirRequestId :: !String
    , rsgirReturn :: !Boolean
    } deriving (Show)

data AuthorizeSecurityGroupEgress = AuthorizeSecurityGroupEgress
    { asgeGroupId :: !String
    , asgeIpPermissions :: !IpPermissionSet
    } deriving (Show)

data AuthorizeSecurityGroupEgressResponse = AuthorizeSecurityGroupEgressResponse
    { asgerRequestId :: !String
    , asgerReturn :: !Boolean
    } deriving (Show)

data RevokeSecurityGroupEgress = RevokeSecurityGroupEgress
    { rsgeGroupId :: !String
    , rsgeIpPermissions :: !IpPermissionSet
    } deriving (Show)

data RevokeSecurityGroupEgressResponse = RevokeSecurityGroupEgressResponse
    { rsgerRequestId :: !String
    , rsgerReturn :: !Boolean
    } deriving (Show)

data InstanceState = InstanceState
    { isCode :: !Int
    , isName :: !String
    } deriving (Show)

data ModifyInstanceAttribute = ModifyInstanceAttribute
    { miaInstanceId :: !String
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
    { sgisiGroupId :: !String
    } deriving (Show)

data ModifyInstanceAttributeResponse = ModifyInstanceAttributeResponse
    { miarRequestId :: !String
    , miarReturn :: !Boolean
    } deriving (Show)

data ResetInstanceAttribute = ResetInstanceAttribute
    { riaInstanceId :: !String
    , riaResetInstanceAttributesGroup :: !ResetInstanceAttributesGroup
    } deriving (Show)

data ResetInstanceAttributesGroup = ResetInstanceAttributesGroup
    { riagKernel :: !EmptyElement
    , riagRamdisk :: !EmptyElement
    , riagSourceDestCheck :: !EmptyElement
    } deriving (Show)
data ResetInstanceAttributeResponse = ResetInstanceAttributeResponse
    { riarRequestId :: !String
    , riarReturn :: !Boolean
    } deriving (Show)

data DescribeInstanceAttribute = DescribeInstanceAttribute
    { diaInstanceId :: !String
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
    { diarRequestId :: !String
    , diarInstanceId :: !String
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
    { miaImageId :: !String
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
    , lpiUserId :: !(Maybe String)
    , lpiGroup :: !(Maybe String)
-- </xs:choice>
    } deriving (Show)

data ProductCodeList = ProductCodeList
    { pclItem :: ![ProductCodeItem]
    } deriving (Show)

data ProductCodeItem = ProductCodeItem
    { pciProductCode :: !String
    } deriving (Show)

data ModifyImageAttributeResponse = ModifyImageAttributeResponse
    { miarRequestId :: !String
    , miarReturn :: !Boolean
    } deriving (Show)

data ResetImageAttribute = ResetImageAttribute
    { riaImageId :: !String
    , riaResetImageAttributesGroup :: !ResetImageAttributesGroup
    } deriving (Show)

data ResetImageAttributesGroup = ResetImageAttributesGroup
    { riagLaunchPermission :: !EmptyElement
    } deriving (Show)

data EmptyElement = EmptyElement

data ResetImageAttributeResponse = ResetImageAttributeResponse
    { riarRequestId :: !String
    , riarReturn :: !Boolean
    } deriving (Show)

data DescribeImageAttribute = DescribeImageAttribute
    { diaImageId :: !String
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
    { diarRequestId :: !String
    , diarImageId :: !String
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
    { navValue :: !(Maybe String)
    } deriving (Show)

data NullableAttributeBooleanValue = NullableAttributeBooleanValue
    { nabvValue :: !(Maybe Boolean)
    } deriving (Show)

data AttributeValue = AttributeValue
    { avValue :: !String
    } deriving (Show)

data AttributeBooleanValue = AttributeBooleanValue
    { abvValue :: !Boolean
    } deriving (Show)

data ConfirmProductInstance = ConfirmProductInstance
    { cpiProductCode :: !String
    , cpiInstanceId :: !String
    } deriving (Show)

data ProductCodesSet = ProductCodesSet
    { pcsItem :: ![ProductCodesSetItem]
    } deriving (Show)

data ProductCodesSetItem = ProductCodesSetItem
    { pcsiProductCode :: !String
    , pcsiType :: !String
    } deriving (Show)

data ConfirmProductInstanceResponse = ConfirmProductInstanceResponse
    { cpirRequestId :: !String
    , cpirReturn :: !Boolean
    , cpirOwnerId :: !(Maybe String)
    } deriving (Show)

data DescribeAvailabilityZones = DescribeAvailabilityZones
    { dazAvailabilityZoneSet :: !DescribeAvailabilityZonesSet
    , dazFilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data DescribeAvailabilityZonesSet = DescribeAvailabilityZonesSet
    { dazsItem :: ![DescribeAvailabilityZonesSetItem]
    } deriving (Show)

data DescribeAvailabilityZonesSetItem = DescribeAvailabilityZonesSetItem
    { dazsiZoneName :: !String
    } deriving (Show)

data DescribeAvailabilityZonesResponse = DescribeAvailabilityZonesResponse
    { dazrRequestId :: !String
    , dazrAvailabilityZoneInfo :: !AvailabilityZoneSet
    } deriving (Show)

data AvailabilityZoneSet = AvailabilityZoneSet
    { azsItem :: ![AvailabilityZoneItem]
    } deriving (Show)

data AvailabilityZoneMessage = AvailabilityZoneMessage
    { azmMessage :: !String
    } deriving (Show)

data AvailabilityZoneMessageSet = AvailabilityZoneMessageSet
    { azmsItem :: ![AvailabilityZoneMessage]
    } deriving (Show)

data AvailabilityZoneItem = AvailabilityZoneItem
    { aziZoneName :: !String
    , aziZoneState :: !String
    , aziRegionName :: !String
    , aziMessageSet :: !AvailabilityZoneMessageSet
    } deriving (Show)

data AllocateAddress = AllocateAddress
    { aaDomain :: !(Maybe String)
    } deriving (Show)

data AllocateAddressResponse = AllocateAddressResponse
    { aarRequestId :: !String
    , aarPublicIp :: !String
    , aarDomain :: !String
    , aarAllocationId :: !(Maybe String)
    } deriving (Show)

data ReleaseAddress = ReleaseAddress
    { ra 
  -- <xs:choice>
    , raPublicIp :: !(Maybe String)
    , raAllocationId :: !(Maybe String)
  -- </xs:choice>
    } deriving (Show)

data ReleaseAddressResponse = ReleaseAddressResponse
    { rarRequestId :: !String
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
    { aisiAllocationId :: !String
    } deriving (Show)

data DescribeAddressesInfo = DescribeAddressesInfo
    { daiItem :: ![DescribeAddressesItem]
    } deriving (Show)

data DescribeAddressesItem = DescribeAddressesItem
    { daiPublicIp :: !String
    } deriving (Show)

data DescribeAddressesResponse = DescribeAddressesResponse
    { darRequestId :: !String
    , darAddressesSet :: !DescribeAddressesResponseInfo
    } deriving (Show)

data DescribeAddressesResponseInfo = DescribeAddressesResponseInfo
    { dariItem :: ![DescribeAddressesResponseItem]
    } deriving (Show)

data DescribeAddressesResponseItem = DescribeAddressesResponseItem
    { dariPublicIp :: !String
    , dariAllocationId :: !(Maybe String)
    , dariDomain :: !String
    , dariInstanceId :: !(Maybe String)
    , dariAssociationId :: !(Maybe String)
    , dariNetworkInterfaceId :: !(Maybe String)
    , dariNetworkInterfaceOwnerId :: !(Maybe String)
    , dariPrivateIpAddress :: !(Maybe String)
    } deriving (Show)

data AssociateAddress = AssociateAddress
    { aa 
  -- <xs:choice>
    , aaPublicIp :: !(Maybe String)
    , aaAllocationId :: !(Maybe String)
  -- </xs:choice>
  -- <xs:choice>
    , aaNetworkInterfaceId :: !(Maybe String)
    , aaInstanceId :: !(Maybe String)
  -- </xs:choice>
    , aaPrivateIpAddress :: !(Maybe String)
    , aaAllowReassociation :: !(Maybe Boolean)
    } deriving (Show)

data AssociateAddressResponse = AssociateAddressResponse
    { aarRequestId :: !String
    , aarReturn :: !Boolean
    , aarAssociationId :: !(Maybe String)
    } deriving (Show)

data DisassociateAddress = DisassociateAddress
    { da 
-- <xs:choice>
    , daPublicIp :: !(Maybe String)
    , daAssociationId :: !(Maybe String)
-- </xs:choice>
     } deriving (Show)

data DisassociateAddressResponse = DisassociateAddressResponse
    { darRequestId :: !String
    , darReturn :: !Boolean
    } deriving (Show)

data CreateVolume = CreateVolume
    { cvSize :: !(Maybe String)
    , cvSnapshotId :: !(Maybe String)
    , cvAvailabilityZone :: !String
    , cvVolume :: !(Maybe String)
    , cvIops :: !(Maybe Int)
    } deriving (Show)

data CreateVolumeResponse = CreateVolumeResponse
    { cvrRequestId :: !String
    , cvrVolumeId :: !String
    , cvrSize :: !String
    , cvrSnapshotId :: !String
    , cvrAvailabilityZone :: !String
    , cvrStatus :: !String
    , cvrCreateTime :: !DateTime
    , cvrVolume :: !String
    , cvrIops :: !(Maybe Int)
    } deriving (Show)

data DeleteVolume = DeleteVolume
    { dvVolumeId :: !String
    } deriving (Show)

data DeleteVolumeResponse = DeleteVolumeResponse
    { dvrRequestId :: !String
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
    { dvsiVolumeId :: !String
    } deriving (Show)

data DescribeVolumesResponse = DescribeVolumesResponse
    { dvrRequestId :: !String
    , dvrVolumeSet :: !DescribeVolumesSetResponse
    } deriving (Show)

data DescribeVolumesSetResponse = DescribeVolumesSetResponse
    { dvsrItem :: ![DescribeVolumesSetItemResponse]
    } deriving (Show)

data DescribeVolumesSetItemResponse = DescribeVolumesSetItemResponse
    { dvsirVolumeId :: !String
    , dvsirSize :: !String
    , dvsirSnapshotId :: !String
    , dvsirAvailabilityZone :: !String
    , dvsirStatus :: !String
    , dvsirCreateTime :: !DateTime
    , dvsirAttachmentSet :: !AttachmentSetResponse
    , dvsirTagSet :: !(Maybe ResourceTagSet)
    , dvsirVolume :: !String
    , dvsirIops :: !(Maybe Int)
    } deriving (Show)

data AttachmentSetResponse = AttachmentSetResponse
    { asrItem :: ![AttachmentSetItemResponse]
    } deriving (Show)

data AttachmentSetItemResponse = AttachmentSetItemResponse
    { asirVolumeId :: !String
    , asirInstanceId :: !String
    , asirDevice :: !String
    , asirStatus :: !String
    , asirAttachTime :: !DateTime
    , asirDeleteOnTermination :: !Boolean
    } deriving (Show)

data AttachVolume = AttachVolume
    { avVolumeId :: !String
    , avInstanceId :: !String
    , avDevice :: !String
    } deriving (Show)

data AttachVolumeResponse = AttachVolumeResponse
    { avrRequestId :: !String
    , avrVolumeId :: !String
    , avrInstanceId :: !String
    , avrDevice :: !String
    , avrStatus :: !String
    , avrAttachTime :: !DateTime
    } deriving (Show)

data DetachVolume = DetachVolume
    { dvVolumeId :: !String
    , dvInstanceId :: !(Maybe String)
    , dvDevice :: !(Maybe String)
    , dvForce :: !(Maybe Boolean)
    } deriving (Show)

data DetachVolumeResponse = DetachVolumeResponse
    { dvrRequestId :: !String
    , dvrVolumeId :: !String
    , dvrInstanceId :: !String
    , dvrDevice :: !String
    , dvrStatus :: !String
    , dvrAttachTime :: !DateTime
    } deriving (Show)

data CreateSnapshot = CreateSnapshot
    { csVolumeId :: !String
    , csDescription :: !(Maybe String)
    } deriving (Show)

data CreateSnapshotResponse = CreateSnapshotResponse
    { csrRequestId :: !String
    , csrSnapshotId :: !String
    , csrVolumeId :: !String
    , csrStatus :: !String
    , csrStartTime :: !DateTime
    , csrProgress :: !String
    , csrOwnerId :: !String
    , csrVolumeSize :: !String
    , csrDescription :: !(Maybe String)
    } deriving (Show)

data CopySnapshot = CopySnapshot
    { csSourceRegion :: !String
    , csSourceSnapshotId :: !String
    , csDescription :: !(Maybe String)
    } deriving (Show)

data CopySnapshotResponse = CopySnapshotResponse
    { csrRequestId :: !String
    , csrSnapshotId :: !String
    } deriving (Show)

data DeleteSnapshot = DeleteSnapshot
    { dsSnapshotId :: !String
    } deriving (Show)

data DeleteSnapshotResponse = DeleteSnapshotResponse
    { dsrRequestId :: !String
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
    { dssiSnapshotId :: !String
    } deriving (Show)

data DescribeSnapshotsOwners = DescribeSnapshotsOwners
    { dsoItem :: ![DescribeSnapshotsOwner]
    } deriving (Show)

data DescribeSnapshotsOwner = DescribeSnapshotsOwner
    { dsoOwner :: !String
    } deriving (Show)

data DescribeSnapshotsRestorableBySet = DescribeSnapshotsRestorableBySet
    { dsrbsItem :: ![DescribeSnapshotsRestorableBy]
    } deriving (Show)

data DescribeSnapshotsRestorableBy = DescribeSnapshotsRestorableBy
    { dsrbUser :: !String
    } deriving (Show)

data DescribeSnapshotsResponse = DescribeSnapshotsResponse
    { dsrRequestId :: !String
    , dsrSnapshotSet :: !DescribeSnapshotsSetResponse
    } deriving (Show)

data DescribeSnapshotsSetResponse = DescribeSnapshotsSetResponse
    { dssrItem :: ![DescribeSnapshotsSetItemResponse]
    } deriving (Show)

data DescribeSnapshotsSetItemResponse = DescribeSnapshotsSetItemResponse
    { dssirSnapshotId :: !String
    , dssirVolumeId :: !String
    , dssirStatus :: !String
    , dssirStartTime :: !DateTime
    , dssirProgress :: !String
    , dssirOwnerId :: !String
    , dssirVolumeSize :: !String
    , dssirDescription :: !(Maybe String)
    , dssirOwnerAlias :: !(Maybe String)
    , dssirTagSet :: !(Maybe ResourceTagSet)
    } deriving (Show)

data ModifySnapshotAttribute = ModifySnapshotAttribute
    { msaSnapshotId :: !String
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
    , cvpiUserId :: !(Maybe String)
    , cvpiGroup :: !(Maybe String)
-- </xs:choice>
    } deriving (Show)

data ModifySnapshotAttributeResponse = ModifySnapshotAttributeResponse
    { msarRequestId :: !String
    , msarReturn :: !Boolean
    } deriving (Show)

data ResetSnapshotAttribute = ResetSnapshotAttribute
    { rsaSnapshotId :: !String
    , rsaResetSnapshotAttributesGroup :: !ResetSnapshotAttributesGroup
    } deriving (Show)

data ResetSnapshotAttributesGroup = ResetSnapshotAttributesGroup
    { rsagCreateVolumePermission :: !EmptyElement
    } deriving (Show)
data ResetSnapshotAttributeResponse = ResetSnapshotAttributeResponse
    { rsarRequestId :: !String
    , rsarReturn :: !Boolean
    } deriving (Show)

data DescribeSnapshotAttribute = DescribeSnapshotAttribute
    { dsaSnapshotId :: !String
    , dsaDescribeSnapshotAttributesGroup :: !DescribeSnapshotAttributesGroup
    } deriving (Show)

data DescribeSnapshotAttributesGroup = DescribeSnapshotAttributesGroup
    { dsagCreateVolumePermission :: !EmptyElement
    , dsagProductCodes :: !EmptyElement
    } deriving (Show)
data DescribeSnapshotAttributeResponse = DescribeSnapshotAttributeResponse
    { dsarRequestId :: !String
    , dsarSnapshotId :: !String
  -- <xs:choice>
    , dsarCreateVolumePermission :: !(Maybe CreateVolumePermissionList)
    , dsarProductCodes :: !(Maybe ProductCodesSet)
  -- </xs:choice>
    } deriving (Show)

data BundleInstance = BundleInstance
    { biInstanceId :: !String
    , biStorage :: !BundleInstanceTaskStorage
    } deriving (Show)

data BundleInstanceTaskStorage = BundleInstanceTaskStorage
    { bitsS3 :: !BundleInstanceS3Storage
    } deriving (Show)

data BundleInstanceS3Storage = BundleInstanceS3Storage
    { bis3sBucket :: !String
    , bis3sPrefix :: !String
    , bis3sAwsAccessKeyId :: !(Maybe String)
    , bis3sUploadPolicy :: !(Maybe String)
    , bis3sUploadPolicySignature :: !(Maybe String)
    } deriving (Show)

data BundleInstanceResponse = BundleInstanceResponse
    { birRequestId :: !String
    , birBundleInstanceTask :: !BundleInstanceTask
    } deriving (Show)

data BundleInstanceTask = BundleInstanceTask
    { bitInstanceId :: !String
    , bitBundleId :: !String
    , bitState :: !String
    , bitStartTime :: !DateTime
    , bitUpdateTime :: !DateTime
    , bitStorage :: !BundleInstanceTaskStorage
    , bitProgress :: !(Maybe String)
    , bitError :: !(Maybe BundleInstanceTaskError)
    } deriving (Show)

data BundleInstanceTaskError = BundleInstanceTaskError
    { biteCode :: !String
    , biteMessage :: !String
    } deriving (Show)

data DescribeBundleTasks = DescribeBundleTasks
    { dbtBundlesSet :: !DescribeBundleTasksInfo
    , dbtFilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data DescribeBundleTasksInfo = DescribeBundleTasksInfo
    { dbtiItem :: ![DescribeBundleTasksItem]
    } deriving (Show)

data DescribeBundleTasksItem = DescribeBundleTasksItem
    { dbtiBundleId :: !String
    } deriving (Show)

data DescribeBundleTasksResponse = DescribeBundleTasksResponse
    { dbtrRequestId :: !String
    , dbtrBundleInstanceTasksSet :: !BundleInstanceTasksSet
    } deriving (Show)

data BundleInstanceTasksSet = BundleInstanceTasksSet
    { bitsItem :: ![BundleInstanceTask]
    } deriving (Show)

data CancelBundleTask = CancelBundleTask
    { cbtBundleId :: !String
    } deriving (Show)

data CancelBundleTaskResponse = CancelBundleTaskResponse
    { cbtrRequestId :: !String
    , cbtrBundleInstanceTask :: !BundleInstanceTask
    } deriving (Show)

data CopyImage = CopyImage
    { ciSourceRegion :: !String
    , ciSourceImageId :: !String
    , ciName :: !String
    , ciDescription :: !(Maybe String)
    , ciClientToken :: !(Maybe String)
    } deriving (Show)

data CopyImageResponse = CopyImageResponse
    { cirRequestId :: !String
    , cirImageId :: !String
    } deriving (Show)

data DescribeRegions = DescribeRegions
    { drRegionSet :: !DescribeRegionsSet
    , drFilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data DescribeRegionsSet = DescribeRegionsSet
    { drsItem :: ![DescribeRegionsSetItem]
    } deriving (Show)

data DescribeRegionsSetItem = DescribeRegionsSetItem
    { drsiRegionName :: !String
    } deriving (Show)

data DescribeRegionsResponse = DescribeRegionsResponse
    { drrRequestId :: !String
    , drrRegionInfo :: !RegionSet
    } deriving (Show)

data RegionSet = RegionSet
    { rsItem :: ![RegionItem]
    } deriving (Show)

data RegionItem = RegionItem
    { riRegionName :: !String
    , riRegionEndpoint :: !String
    } deriving (Show)

data DescribeReservedInstancesOfferings = DescribeReservedInstancesOfferings
    { drioReservedInstancesOfferingsSet :: !(Maybe DescribeReservedInstancesOfferingsSet)
    , drioInstance :: !(Maybe String)
    , drioAvailabilityZone :: !(Maybe String)
    , drioProductDescription :: !(Maybe String)
    , drioFilterSet :: !(Maybe FilterSet)
    , drioInstanceTenancy :: !(Maybe String)
    , drioOffering :: !(Maybe String)
    , drioIncludeMarketplace :: !(Maybe Boolean)
    , drioMinDuration :: !(Maybe Long)
    , drioMaxDuration :: !(Maybe Long)
    , drioMaxInstanceCount :: !(Maybe Int)
    , drioNextToken :: !(Maybe String)
    , drioMaxResults :: !(Maybe Int)
    } deriving (Show)

data DescribeReservedInstancesOfferingsSet = DescribeReservedInstancesOfferingsSet
    { driosItem :: ![DescribeReservedInstancesOfferingsSetItem]
    } deriving (Show)

data DescribeReservedInstancesOfferingsSetItem = DescribeReservedInstancesOfferingsSetItem
    { driosiReservedInstancesOfferingId :: !String
    } deriving (Show)

data DescribeReservedInstancesOfferingsResponse = DescribeReservedInstancesOfferingsResponse
    { driorRequestId :: !String
    , driorReservedInstancesOfferingsSet :: !DescribeReservedInstancesOfferingsResponseSet
    , driorNextToken :: !(Maybe String)
    } deriving (Show)

data DescribeReservedInstancesOfferingsResponseSet = DescribeReservedInstancesOfferingsResponseSet
    { driorsItem :: ![DescribeReservedInstancesOfferingsResponseSetItem]
    } deriving (Show)

data DescribeReservedInstancesOfferingsResponseSetItem = DescribeReservedInstancesOfferingsResponseSetItem
    { driorsiReservedInstancesOfferingId :: !String
    , driorsiInstance :: !String
    , driorsiAvailabilityZone :: !String
    , driorsiDuration :: !Long
    , driorsiFixedPrice :: !Double
    , driorsiUsagePrice :: !Double
    , driorsiProductDescription :: !String
    , driorsiInstanceTenancy :: !String
    , driorsiCurrencyCode :: !String
    , driorsiOffering :: !String
    , driorsiRecurringCharges :: !RecurringChargesSet
    , driorsiMarketplace :: !(Maybe Boolean)
    , driorsiPricingDetailsSet :: !(Maybe PricingDetailsSet)
    } deriving (Show)

data RecurringChargesSet = RecurringChargesSet
    { rcsItem :: ![RecurringChargesSetItem]
    } deriving (Show)

data RecurringChargesSetItem = RecurringChargesSetItem
    { rcsiFrequency :: !String
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
    { prioReservedInstancesOfferingId :: !String
    , prioInstanceCount :: !Int
    , prioLimitPrice :: !(Maybe ReservedInstanceLimitPrice)
    } deriving (Show)

data ReservedInstanceLimitPrice = ReservedInstanceLimitPrice
    { rilpAmount :: !Double
    , rilpCurrencyCode :: !(Maybe String)
    } deriving (Show)

data PurchaseReservedInstancesOfferingResponse = PurchaseReservedInstancesOfferingResponse
    { priorRequestId :: !String
    , priorReservedInstancesId :: !String
    } deriving (Show)

data DescribeReservedInstances = DescribeReservedInstances
    { driReservedInstancesSet :: !(Maybe DescribeReservedInstancesSet)
    , driFilterSet :: !(Maybe FilterSet)
    , driOffering :: !(Maybe String)
    } deriving (Show)

data DescribeReservedInstancesSet = DescribeReservedInstancesSet
    { drisItem :: ![DescribeReservedInstancesSetItem]
    } deriving (Show)

data DescribeReservedInstancesSetItem = DescribeReservedInstancesSetItem
    { drisiReservedInstancesId :: !String
    } deriving (Show)

data DescribeReservedInstancesResponse = DescribeReservedInstancesResponse
    { drirRequestId :: !String
    , drirReservedInstancesSet :: !DescribeReservedInstancesResponseSet
    } deriving (Show)

data DescribeReservedInstancesResponseSet = DescribeReservedInstancesResponseSet
    { drirsItem :: ![DescribeReservedInstancesResponseSetItem]
    } deriving (Show)

data DescribeReservedInstancesResponseSetItem = DescribeReservedInstancesResponseSetItem
    { drirsiReservedInstancesId :: !String
    , drirsiInstance :: !String
    , drirsiAvailabilityZone :: !String
    , drirsiStart :: !DateTime
    , drirsiDuration :: !Long
    , drirsiFixedPrice :: !Double
    , drirsiUsagePrice :: !Double
    , drirsiInstanceCount :: !Integer
    , drirsiProductDescription :: !String
    , drirsiState :: !String
    , drirsiTagSet :: !(Maybe ResourceTagSet)
    , drirsiInstanceTenancy :: !String
    , drirsiCurrencyCode :: !String
    , drirsiOffering :: !String
    , drirsiRecurringCharges :: !(Maybe RecurringChargesSet)
    } deriving (Show)

data CreateReservedInstancesListing = CreateReservedInstancesListing
    { crilReservedInstancesId :: !String
    , crilInstanceCount :: !(Maybe Int)
    , crilPriceSchedules :: !PriceScheduleRequestSet
    , crilClientToken :: !String
    } deriving (Show)

data PriceScheduleRequestSet = PriceScheduleRequestSet
    { psrsItem :: ![PriceScheduleRequestSetItem]
    } deriving (Show)

data PriceScheduleRequestSetItem = PriceScheduleRequestSetItem
    { psrsiTerm :: !Long
    , psrsiPrice :: !Double
    , psrsiCurrencyCode :: !(Maybe String)
    } deriving (Show)

data CreateReservedInstancesListingResponse = CreateReservedInstancesListingResponse
    { crilrRequestId :: !String
    , crilrReservedInstancesListingsSet :: !DescribeReservedInstancesListingsResponseSet
    } deriving (Show)

data CancelReservedInstancesListing = CancelReservedInstancesListing
    { crilReservedInstancesListingId :: !String
    } deriving (Show)

data CancelReservedInstancesListingResponse = CancelReservedInstancesListingResponse
    { crilrRequestId :: !String
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
    { drilsiReservedInstancesListingId :: !String
    } deriving (Show)

data DescribeReservedInstancesListingsResponse = DescribeReservedInstancesListingsResponse
    { drilrRequestId :: !String
    , drilrReservedInstancesListingsSet :: !DescribeReservedInstancesListingsResponseSet
    } deriving (Show)

data DescribeReservedInstancesListingsResponseSet = DescribeReservedInstancesListingsResponseSet
    { drilrsItem :: ![DescribeReservedInstancesListingsResponseSetItem]
    } deriving (Show)

data DescribeReservedInstancesListingsResponseSetItem = DescribeReservedInstancesListingsResponseSetItem
    { drilrsiReservedInstancesListingId :: !String
    , drilrsiReservedInstancesId :: !String
    , drilrsiCreateDate :: !DateTime
    , drilrsiUpdateDate :: !DateTime
    , drilrsiStatus :: !String
    , drilrsiStatusMessage :: !String
    , drilrsiInstanceCounts :: !InstanceCountsSet
    , drilrsiPriceSchedules :: !PriceScheduleSet
    , drilrsiTagSet :: !(Maybe ResourceTagSet)
    , drilrsiClientToken :: !(Maybe String)
    } deriving (Show)

data InstanceCountsSet = InstanceCountsSet
    { icsItem :: ![InstanceCountsSetItem]
    } deriving (Show)

data InstanceCountsSetItem = InstanceCountsSetItem
    { icsiState :: !String
    , icsiInstanceCount :: !Int
    } deriving (Show)

data PriceScheduleSet = PriceScheduleSet
    { pssItem :: ![PriceScheduleSetItem]
    } deriving (Show)

data PriceScheduleSetItem = PriceScheduleSetItem
    { pssiTerm :: !Long
    , pssiPrice :: !Double
    , pssiCurrencyCode :: !(Maybe String)
    , pssiActive :: !Boolean
    } deriving (Show)

data MonitorInstances = MonitorInstances
    { miInstancesSet :: !MonitorInstancesSet
    } deriving (Show)

data MonitorInstancesSet = MonitorInstancesSet
    { misItem :: !(NonEmpty MonitorInstancesSetItem)
    } deriving (Show)

data MonitorInstancesSetItem = MonitorInstancesSetItem
    { misiInstanceId :: !String
    } deriving (Show)

data MonitorInstancesResponse = MonitorInstancesResponse
    { mirRequestId :: !String
    , mirInstancesSet :: !MonitorInstancesResponseSet
    } deriving (Show)

data MonitorInstancesResponseSet = MonitorInstancesResponseSet
    { mirsItem :: !(NonEmpty MonitorInstancesResponseSetItem)
    } deriving (Show)

data MonitorInstancesResponseSetItem = MonitorInstancesResponseSetItem
    { mirsiInstanceId :: !String
    , mirsiMonitoring :: !InstanceMonitoringState
    } deriving (Show)

data InstanceMonitoringState = InstanceMonitoringState
    { imsState :: !String
    } deriving (Show)

data Attachment = Attachment
    { aVpcId :: !String
    , aState :: !String
    } deriving (Show)

data AttachmentSet = AttachmentSet
    { asItem :: ![Attachment]
    } deriving (Show)

data VpnGateway = VpnGateway
    { vgVpnGatewayId :: !String
    , vgState :: !String
    , vgType :: !String
    , vgAvailabilityZone :: !(Maybe String)
    , vgAttachments :: !AttachmentSet
    , vgTagSet :: !(Maybe ResourceTagSet)
    } deriving (Show)

data CustomerGateway = CustomerGateway
    { cgCustomerGatewayId :: !String
    , cgState :: !String
    , cgType :: !String
    , cgIpAddress :: !String
    , cgBgpAsn :: !(Maybe Int)
    , cgTagSet :: !(Maybe ResourceTagSet)
    } deriving (Show)

data VpnConnection = VpnConnection
    { vcVpnConnectionId :: !String
    , vcState :: !String
    , vcCustomerGatewayConfiguration :: !(Maybe String)
    , vcType :: !(Maybe String)
    , vcCustomerGatewayId :: !String
    , vcVpnGatewayId :: !String
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
    { vsrDestinationCidrBlock :: !String
    , vsrSource :: !String
    , vsrState :: !String
    } deriving (Show)

data VgwTelemetry = VgwTelemetry
    { vtItem :: ![VpnTunnelTelemetry]
    } deriving (Show)

data VpnTunnelTelemetry = VpnTunnelTelemetry
    { vttOutsideIpAddress :: !String
    , vttStatus :: !String
    , vttLastStatusChange :: !DateTime
    , vttStatusMessage :: !(Maybe String)
    , vttAcceptedRouteCount :: !Int
    } deriving (Show)

data Vpc = Vpc
    { vVpcId :: !String
    , vState :: !(Maybe String)
    , vCidrBlock :: !(Maybe String)
    , vDhcpOptionsId :: !(Maybe String)
    , vTagSet :: !(Maybe ResourceTagSet)
    , vInstanceTenancy :: !(Maybe String)
    , vIsDefault :: !(Maybe Boolean)
    } deriving (Show)

data Subnet = Subnet
    { sSubnetId :: !String
    , sState :: !(Maybe String)
    , sVpcId :: !(Maybe String)
    , sCidrBlock :: !(Maybe String)
    , sAvailableIpAddressCount :: !(Maybe Int)
    , sAvailabilityZone :: !(Maybe String)
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
    { cgisiCustomerGatewayId :: !String
    } deriving (Show)

data CustomerGatewayIdSet = CustomerGatewayIdSet
    { cgisItem :: ![CustomerGatewayIdSetItem]
    } deriving (Show)

data VpnGatewayIdSetItem = VpnGatewayIdSetItem
    { vgisiVpnGatewayId :: !String
    } deriving (Show)

data VpnGatewayIdSet = VpnGatewayIdSet
    { vgisItem :: ![VpnGatewayIdSetItem]
    } deriving (Show)

data VpnConnectionIdSetItem = VpnConnectionIdSetItem
    { vcisiVpnConnectionId :: !String
    } deriving (Show)

data VpnConnectionIdSet = VpnConnectionIdSet
    { vcisItem :: ![VpnConnectionIdSetItem]
    } deriving (Show)

data VpcIdSetItem = VpcIdSetItem
    { visiVpcId :: !String
    } deriving (Show)

data VpcIdSet = VpcIdSet
    { visItem :: ![VpcIdSetItem]
    } deriving (Show)

data SubnetIdSetItem = SubnetIdSetItem
    { sisiSubnetId :: !String
    } deriving (Show)

data SubnetIdSet = SubnetIdSet
    { sisItem :: ![SubnetIdSetItem]
    } deriving (Show)

data DhcpOptionsIdSetItem = DhcpOptionsIdSetItem
    { doisiDhcpOptionsId :: !String
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
    { dciKey :: !String
    , dciValueSet :: !DhcpValueSet
    } deriving (Show)

data DhcpOptions = DhcpOptions
    { doDhcpOptionsId :: !String
    , doDhcpConfigurationSet :: !DhcpConfigurationItemSet
    , doTagSet :: !(Maybe ResourceTagSet)
    } deriving (Show)

data DhcpValue = DhcpValue
    { dvValue :: !String
    } deriving (Show)

data DhcpValueSet = DhcpValueSet
    { dvsItem :: ![DhcpValue]
    } deriving (Show)

data Filter = Filter
    { fName :: !String
    , fValueSet :: !ValueSet
    } deriving (Show)

data FilterSet = FilterSet
    { fsItem :: ![Filter]
    } deriving (Show)

data Value = Value
    { vValue :: !String
    } deriving (Show)

data ValueSet = ValueSet
    { vsItem :: ![Value]
    } deriving (Show)

data CreateCustomerGateway = CreateCustomerGateway
    { ccgType :: !String
    , ccgIpAddress :: !String
    , ccgBgpAsn :: !(Maybe Int)
    } deriving (Show)

data CreateCustomerGatewayResponse = CreateCustomerGatewayResponse
    { ccgrRequestId :: !String
    , ccgrCustomerGateway :: !CustomerGateway
    } deriving (Show)

data DeleteCustomerGateway = DeleteCustomerGateway
    { dcgCustomerGatewayId :: !String
    } deriving (Show)

data DeleteCustomerGatewayResponse = DeleteCustomerGatewayResponse
    { dcgrRequestId :: !String
    , dcgrReturn :: !Boolean
    } deriving (Show)

data DescribeCustomerGateways = DescribeCustomerGateways
    { dcgCustomerGatewaySet :: !(Maybe CustomerGatewayIdSet)
    , dcgFilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data DescribeCustomerGatewaysResponse = DescribeCustomerGatewaysResponse
    { dcgrRequestId :: !String
    , dcgrCustomerGatewaySet :: !CustomerGatewaySet
    } deriving (Show)

data CreateVpnGateway = CreateVpnGateway
    { cvgType :: !String
    , cvgAvailabilityZone :: !(Maybe String)
    } deriving (Show)

data CreateVpnGatewayResponse = CreateVpnGatewayResponse
    { cvgrRequestId :: !String
    , cvgrVpnGateway :: !VpnGateway
    } deriving (Show)

data DeleteVpnGateway = DeleteVpnGateway
    { dvgVpnGatewayId :: !String
    } deriving (Show)

data DeleteVpnGatewayResponse = DeleteVpnGatewayResponse
    { dvgrRequestId :: !String
    , dvgrReturn :: !Boolean
    } deriving (Show)

data DescribeVpnGateways = DescribeVpnGateways
    { dvgVpnGatewaySet :: !(Maybe VpnGatewayIdSet)
    , dvgFilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data DescribeVpnGatewaysResponse = DescribeVpnGatewaysResponse
    { dvgrRequestId :: !String
    , dvgrVpnGatewaySet :: !VpnGatewaySet
    } deriving (Show)

data CreateVpnConnection = CreateVpnConnection
    { cvcType :: !String
    , cvcCustomerGatewayId :: !String
    , cvcVpnGatewayId :: !String
    , cvcOptions :: !(Maybe VpnConnectionOptionsRequest)
    } deriving (Show)

data VpnConnectionOptionsRequest = VpnConnectionOptionsRequest
    { vcorStaticRoutesOnly :: !(Maybe Boolean)
    } deriving (Show)

data CreateVpnConnectionResponse = CreateVpnConnectionResponse
    { cvcrRequestId :: !String
    , cvcrVpnConnection :: !VpnConnection
    } deriving (Show)

data CreateVpnConnectionRoute = CreateVpnConnectionRoute
    { cvcrVpnConnectionId :: !String
    , cvcrDestinationCidrBlock :: !String
    } deriving (Show)

data CreateVpnConnectionRouteResponse = CreateVpnConnectionRouteResponse
    { cvcrrRequestId :: !String
    , cvcrrReturn :: !Boolean
    } deriving (Show)

data DeleteVpnConnectionRoute = DeleteVpnConnectionRoute
    { dvcrVpnConnectionId :: !String
    , dvcrDestinationCidrBlock :: !String
    } deriving (Show)

data DeleteVpnConnectionRouteResponse = DeleteVpnConnectionRouteResponse
    { dvcrrRequestId :: !String
    , dvcrrReturn :: !Boolean
    } deriving (Show)

data DeleteVpnConnection = DeleteVpnConnection
    { dvcVpnConnectionId :: !String
    } deriving (Show)

data DeleteVpnConnectionResponse = DeleteVpnConnectionResponse
    { dvcrRequestId :: !String
    , dvcrReturn :: !Boolean
    } deriving (Show)

data DescribeVpnConnections = DescribeVpnConnections
    { dvcVpnConnectionSet :: !(Maybe VpnConnectionIdSet)
    , dvcFilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data DescribeVpnConnectionsResponse = DescribeVpnConnectionsResponse
    { dvcrRequestId :: !String
    , dvcrVpnConnectionSet :: !VpnConnectionSet
    } deriving (Show)

data AttachVpnGateway = AttachVpnGateway
    { avgVpnGatewayId :: !String
    , avgVpcId :: !String
    } deriving (Show)

data AttachVpnGatewayResponse = AttachVpnGatewayResponse
    { avgrRequestId :: !String
    , avgrAttachment :: !Attachment
    } deriving (Show)

data DetachVpnGateway = DetachVpnGateway
    { dvgVpnGatewayId :: !String
    , dvgVpcId :: !String
    } deriving (Show)

data DetachVpnGatewayResponse = DetachVpnGatewayResponse
    { dvgrRequestId :: !String
    , dvgrReturn :: !Boolean
    } deriving (Show)

data CreateVpc = CreateVpc
    { cvCidrBlock :: !String
    , cvInstanceTenancy :: !(Maybe String)
    } deriving (Show)

data CreateVpcResponse = CreateVpcResponse
    { cvrRequestId :: !String
    , cvrVpc :: !Vpc
    } deriving (Show)

data DescribeVpcs = DescribeVpcs
    { dvVpcSet :: !(Maybe VpcIdSet)
    , dvFilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data DescribeVpcsResponse = DescribeVpcsResponse
    { dvrRequestId :: !String
    , dvrVpcSet :: !VpcSet
    } deriving (Show)

data DeleteVpc = DeleteVpc
    { dvVpcId :: !String
    } deriving (Show)

data DeleteVpcResponse = DeleteVpcResponse
    { dvrRequestId :: !String
    , dvrReturn :: !Boolean
    } deriving (Show)

data CreateSubnet = CreateSubnet
    { csVpcId :: !String
    , csCidrBlock :: !String
    , csAvailabilityZone :: !(Maybe String)
    } deriving (Show)

data CreateSubnetResponse = CreateSubnetResponse
    { csrRequestId :: !String
    , csrSubnet :: !Subnet
    } deriving (Show)

data DescribeSubnets = DescribeSubnets
    { dsSubnetSet :: !(Maybe SubnetIdSet)
    , dsFilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data DescribeSubnetsResponse = DescribeSubnetsResponse
    { dsrRequestId :: !String
    , dsrSubnetSet :: !SubnetSet
    } deriving (Show)

data DeleteSubnet = DeleteSubnet
    { dsSubnetId :: !String
    } deriving (Show)

data DeleteSubnetResponse = DeleteSubnetResponse
    { dsrRequestId :: !String
    , dsrReturn :: !Boolean
    } deriving (Show)

data DeleteDhcpOptions = DeleteDhcpOptions
    { ddoDhcpOptionsId :: !String
    } deriving (Show)

data DeleteDhcpOptionsResponse = DeleteDhcpOptionsResponse
    { ddorRequestId :: !String
    , ddorReturn :: !Boolean
    } deriving (Show)

data DescribeDhcpOptions = DescribeDhcpOptions
    { ddoDhcpOptionsSet :: !(Maybe DhcpOptionsIdSet)
    , ddoFilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data DescribeDhcpOptionsResponse = DescribeDhcpOptionsResponse
    { ddorRequestId :: !String
    , ddorDhcpOptionsSet :: !DhcpOptionsSet
    } deriving (Show)

data CreateDhcpOptions = CreateDhcpOptions
    { cdoDhcpConfigurationSet :: !DhcpConfigurationItemSet
    } deriving (Show)

data CreateDhcpOptionsResponse = CreateDhcpOptionsResponse
    { cdorRequestId :: !String
    , cdorDhcpOptions :: !DhcpOptions
    } deriving (Show)

data AssociateDhcpOptions = AssociateDhcpOptions
    { adoDhcpOptionsId :: !String
    , adoVpcId :: !String
    } deriving (Show)

data AssociateDhcpOptionsResponse = AssociateDhcpOptionsResponse
    { adorRequestId :: !String
    , adorReturn :: !Boolean
    } deriving (Show)

data RequestSpotInstances = RequestSpotInstances
    { rsiSpotPrice :: !String
    , rsiInstanceCount :: !(Maybe Integer)
    , rsiType :: !(Maybe String)
    , rsiValidFrom :: !(Maybe DateTime)
    , rsiValidUntil :: !(Maybe DateTime)
    , rsiLaunchGroup :: !(Maybe String)
    , rsiAvailabilityZoneGroup :: !(Maybe String)
    , rsiLaunchSpecification :: !LaunchSpecificationRequest
    } deriving (Show)

data LaunchSpecificationRequest = LaunchSpecificationRequest
    { lsrImageId :: !String
    , lsrKeyName :: !(Maybe String)
    , lsrGroupSet :: !GroupSet
    , lsrUserData :: !(Maybe UserData)
    , lsrAddressing :: !(Maybe String)
    , lsrInstance :: !String
    , lsrPlacement :: !(Maybe SpotPlacementRequest)
    , lsrKernelId :: !(Maybe String)
    , lsrRamdiskId :: !(Maybe String)
    , lsrBlockDeviceMapping :: !(Maybe BlockDeviceMapping)
    , lsrMonitoring :: !(Maybe MonitoringInstance)
    , lsrSubnetId :: !(Maybe String)
    , lsrNetworkInterfaceSet :: !(Maybe InstanceNetworkInterfaceSetRequest)
    , lsrIamInstanceProfile :: !(Maybe IamInstanceProfileRequest)
    , lsrEbsOptimized :: !(Maybe Boolean)
    } deriving (Show)

data LaunchSpecificationResponse = LaunchSpecificationResponse
    { lsrImageId :: !String
    , lsrKeyName :: !(Maybe String)
    , lsrGroupSet :: !GroupSet
    , lsrAddressing :: !(Maybe String)
    , lsrInstance :: !String
    , lsrPlacement :: !(Maybe SpotPlacementRequest)
    , lsrKernelId :: !(Maybe String)
    , lsrRamdiskId :: !(Maybe String)
    , lsrBlockDeviceMapping :: !(Maybe BlockDeviceMapping)
    , lsrMonitoring :: !(Maybe MonitoringInstance)
    , lsrSubnetId :: !(Maybe String)
    , lsrNetworkInterfaceSet :: !(Maybe InstanceNetworkInterfaceSetRequest)
    , lsrIamInstanceProfile :: !(Maybe IamInstanceProfileRequest)
    , lsrEbsOptimized :: !(Maybe Boolean)
    } deriving (Show)

data SpotInstanceRequestSetItem = SpotInstanceRequestSetItem
    { sirsiSpotInstanceRequestId :: !String
    , sirsiSpotPrice :: !String
    , sirsiType :: !String
    , sirsiState :: !String
    , sirsiFault :: !(Maybe SpotInstanceStateFault)
    , sirsiStatus :: !(Maybe SpotInstanceStatusMessage)
    , sirsiValidFrom :: !(Maybe DateTime)
    , sirsiValidUntil :: !(Maybe DateTime)
    , sirsiLaunchGroup :: !(Maybe String)
    , sirsiAvailabilityZoneGroup :: !(Maybe String)
    , sirsiLaunchSpecification :: !(Maybe LaunchSpecificationResponse)
    , sirsiInstanceId :: !(Maybe String)
    , sirsiCreateTime :: !(Maybe DateTime)
    , sirsiProductDescription :: !(Maybe String)
    , sirsiTagSet :: !(Maybe ResourceTagSet)
    , sirsiLaunchedAvailabilityZone :: !(Maybe String)
    } deriving (Show)

data SpotInstanceStateFault = SpotInstanceStateFault
    { sisfCode :: !String
    , sisfMessage :: !String
    } deriving (Show)

data SpotInstanceStatusMessage = SpotInstanceStatusMessage
    { sismCode :: !(Maybe String)
    , sismUpdateTime :: !(Maybe DateTime)
    , sismMessage :: !(Maybe String)
    } deriving (Show)

data SpotInstanceRequestSet = SpotInstanceRequestSet
    { sirsItem :: ![SpotInstanceRequestSetItem]
    } deriving (Show)

data RequestSpotInstancesResponse = RequestSpotInstancesResponse
    { rsirRequestId :: !String
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
    { sirisiSpotInstanceRequestId :: !String
    } deriving (Show)

data DescribeSpotInstanceRequestsResponse = DescribeSpotInstanceRequestsResponse
    { dsirrRequestId :: !String
    , dsirrSpotInstanceRequestSet :: !SpotInstanceRequestSet
    } deriving (Show)

data CancelSpotInstanceRequests = CancelSpotInstanceRequests
    { csirSpotInstanceRequestIdSet :: !SpotInstanceRequestIdSet
    } deriving (Show)

data CancelSpotInstanceRequestsResponse = CancelSpotInstanceRequestsResponse
    { csirrRequestId :: !String
    , csirrSpotInstanceRequestSet :: !CancelSpotInstanceRequestsResponseSet
    } deriving (Show)

data CancelSpotInstanceRequestsResponseSet = CancelSpotInstanceRequestsResponseSet
    { csirrsItem :: !(NonEmpty CancelSpotInstanceRequestsResponseSetItem)
    } deriving (Show)

data CancelSpotInstanceRequestsResponseSetItem = CancelSpotInstanceRequestsResponseSetItem
    { csirrsiSpotInstanceRequestId :: !String
    , csirrsiState :: !String
    } deriving (Show)

data DescribeSpotPriceHistory = DescribeSpotPriceHistory
    { dsphStartTime :: !(Maybe DateTime)
    , dsphEndTime :: !(Maybe DateTime)
    , dsphInstanceSet :: !(Maybe InstanceSet)
    , dsphProductDescriptionSet :: !(Maybe ProductDescriptionSet)
    , dsphFilterSet :: !(Maybe FilterSet)
    , dsphAvailabilityZone :: !(Maybe String)
    , dsphMaxResults :: !(Maybe Integer)
    , dsphNextToken :: !(Maybe String)
    } deriving (Show)

data InstanceSet = InstanceSet
    { isItem :: !(NonEmpty InstanceSetItem)
    } deriving (Show)

data InstanceSetItem = InstanceSetItem
    { isiInstance :: !String
    } deriving (Show)

data ProductDescriptionSet = ProductDescriptionSet
    { pdsItem :: !(NonEmpty ProductDescriptionSetItem)
    } deriving (Show)

data ProductDescriptionSetItem = ProductDescriptionSetItem
    { pdsiProductDescription :: !String
    } deriving (Show)

data DescribeSpotPriceHistoryResponse = DescribeSpotPriceHistoryResponse
    { dsphrRequestId :: !String
    , dsphrSpotPriceHistorySet :: !SpotPriceHistorySet
    , dsphrNextToken :: !(Maybe String)
    } deriving (Show)

data SpotPriceHistorySet = SpotPriceHistorySet
    { sphsItem :: ![SpotPriceHistorySetItem]
    } deriving (Show)

data SpotPriceHistorySetItem = SpotPriceHistorySetItem
    { sphsiInstance :: !String
    , sphsiProductDescription :: !String
    , sphsiSpotPrice :: !String
    , sphsiTimestamp :: !DateTime
    , sphsiAvailabilityZone :: !(Maybe String)
    } deriving (Show)

data SpotDatafeedSubscription = SpotDatafeedSubscription
    { sdsOwnerId :: !String
    , sdsBucket :: !String
    , sdsPrefix :: !String
    , sdsState :: !String
    , sdsFault :: !(Maybe SpotInstanceStateFault)
    } deriving (Show)

data CreateSpotDatafeedSubscription = CreateSpotDatafeedSubscription
    { csdsBucket :: !String
    , csdsPrefix :: !String
    } deriving (Show)

data CreateSpotDatafeedSubscriptionResponse = CreateSpotDatafeedSubscriptionResponse
    { csdsrRequestId :: !String
    , csdsrSpotDatafeedSubscription :: !SpotDatafeedSubscription
    } deriving (Show)

data DescribeSpotDatafeedSubscription = DescribeSpotDatafeedSubscription

data DescribeSpotDatafeedSubscriptionResponse = DescribeSpotDatafeedSubscriptionResponse
    { dsdsrRequestId :: !String
    , dsdsrSpotDatafeedSubscription :: !SpotDatafeedSubscription
    } deriving (Show)

data DescribeSpotDatafeedSubscription = DescribeSpotDatafeedSubscription

data DeleteSpotDatafeedSubscriptionResponse = DeleteSpotDatafeedSubscriptionResponse
    { dsdsrRequestId :: !String
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
    { lisiLicenseId :: !String
    } deriving (Show)

data DescribeLicensesResponse = DescribeLicensesResponse
    { dlrRequestId :: !String
    , dlrLicenseSet :: !LicenseSet
    } deriving (Show)

data LicenseSet = LicenseSet
    { lsItem :: ![LicenseSetItem]
    } deriving (Show)

data LicenseSetItem = LicenseSetItem
    { lsiLicenseId :: !String
    , lsiType :: !String
    , lsiPool :: !String
    , lsiCapacitySet :: !LicenseCapacitySet
    , lsiTagSet :: !(Maybe ResourceTagSet)
    } deriving (Show)

data LicenseCapacitySet = LicenseCapacitySet
    { lcsItem :: ![LicenseCapacitySetItem]
    } deriving (Show)

data LicenseCapacitySetItem = LicenseCapacitySetItem
    { lcsiCapacity :: !Int
    , lcsiInstanceCapacity :: !Int
    , lcsiState :: !String
    , lcsiEarliestAllowedDeactivationTime :: !(Maybe DateTime)
    } deriving (Show)

data ActivateLicense = ActivateLicense
    { alLicenseId :: !String
    , alCapacity :: !Int
    } deriving (Show)

data ActivateLicenseResponse = ActivateLicenseResponse
    { alrRequestId :: !String
    , alrReturn :: !Boolean
    } deriving (Show)

data DeactivateLicense = DeactivateLicense
    { dlLicenseId :: !String
    , dlCapacity :: !Int
    } deriving (Show)

data DeactivateLicenseResponse = DeactivateLicenseResponse
    { dlrRequestId :: !String
    , dlrReturn :: !Boolean
    } deriving (Show)

data CreatePlacementGroup = CreatePlacementGroup
    { cpgGroupName :: !String
    , cpgStrategy :: !String
    } deriving (Show)

data CreatePlacementGroupResponse = CreatePlacementGroupResponse
    { cpgrRequestId :: !String
    , cpgrReturn :: !Boolean
    } deriving (Show)

data DeletePlacementGroup = DeletePlacementGroup
    { dpgGroupName :: !String
    } deriving (Show)

data DeletePlacementGroupResponse = DeletePlacementGroupResponse
    { dpgrRequestId :: !String
    , dpgrReturn :: !Boolean
    } deriving (Show)

data DescribePlacementGroupItem = DescribePlacementGroupItem
    { dpgiGroupName :: !String
    } deriving (Show)

data DescribePlacementGroupsInfo = DescribePlacementGroupsInfo
    { dpgiItem :: ![DescribePlacementGroupItem]
    } deriving (Show)

data DescribePlacementGroups = DescribePlacementGroups
    { dpgPlacementGroupSet :: !DescribePlacementGroupsInfo
    , dpgFilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data PlacementGroupInfo = PlacementGroupInfo
    { pgiGroupName :: !String
    , pgiStrategy :: !String
    , pgiState :: !String
    } deriving (Show)

data PlacementGroupSet = PlacementGroupSet
    { pgsItem :: ![PlacementGroupInfo]
    } deriving (Show)

data DescribePlacementGroupsResponse = DescribePlacementGroupsResponse
    { dpgrRequestId :: !String
    , dpgrPlacementGroupSet :: !PlacementGroupSet
    } deriving (Show)

data ResourceIdSet = ResourceIdSet
    { risItem :: ![ResourceIdSetItem]
    } deriving (Show)

data ResourceIdSetItem = ResourceIdSetItem
    { risiResourceId :: !String
    } deriving (Show)

data ResourceTagSetItem = ResourceTagSetItem
    { rtsiKey :: !String
    , rtsiValue :: !String
    } deriving (Show)

data ResourceTagSet = ResourceTagSet
    { rtsItem :: ![ResourceTagSetItem]
    } deriving (Show)

data CreateTags = CreateTags
    { ctResourcesSet :: !ResourceIdSet
    , ctTagSet :: !ResourceTagSet
    } deriving (Show)

data CreateTagsResponse = CreateTagsResponse
    { ctrRequestId :: !String
    , ctrReturn :: !Boolean
    } deriving (Show)

data TagSetItem = TagSetItem
    { tsiResourceId :: !(Maybe String)
    , tsiResource :: !(Maybe String)
    , tsiKey :: !(Maybe String)
    , tsiValue :: !(Maybe String)
    } deriving (Show)

data TagSet = TagSet
    { tsItem :: ![TagSetItem]
    } deriving (Show)

data DescribeTags = DescribeTags
    { dtFilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data DescribeTagsResponse = DescribeTagsResponse
    { dtrRequestId :: !String
    , dtrTagSet :: !TagSet
    } deriving (Show)

data DeleteTagsSetItem = DeleteTagsSetItem
    { dtsiKey :: !(Maybe String)
    , dtsiValue :: !(Maybe String)
    } deriving (Show)

data DeleteTagsSet = DeleteTagsSet
    { dtsItem :: ![DeleteTagsSetItem]
    } deriving (Show)

data DeleteTags = DeleteTags
    { dtResourcesSet :: !ResourceIdSet
    , dtTagSet :: !DeleteTagsSet
    } deriving (Show)

data DeleteTagsResponse = DeleteTagsResponse
    { dtrRequestId :: !String
    , dtrReturn :: !Boolean
    } deriving (Show)

data ImportInstance = ImportInstance
    { iiDescription :: !(Maybe String)
    , iiLaunchSpecification :: !ImportInstanceLaunchSpecification
    , iiDiskImageSet :: !DiskImageSet
    , iiKeepPartialImports :: !(Maybe Boolean)
    , iiPlatform :: !String
    } deriving (Show)

data ImportInstanceResponse = ImportInstanceResponse
    { iirRequestId :: !String
    , iirConversionTask :: !ConversionTask
    } deriving (Show)

data ImportInstanceLaunchSpecification = ImportInstanceLaunchSpecification
    { iilsArchitecture :: !String
    , iilsGroupSet :: !(Maybe ImportInstanceGroupSet)
    , iilsUserData :: !(Maybe UserData)
    , iilsInstance :: !String
    , iilsPlacement :: !(Maybe InstancePlacement)
    , iilsMonitoring :: !(Maybe MonitoringInstance)
    , iilsSubnetId :: !(Maybe String)
    , iilsInstanceInitiatedShutdownBehavior :: !(Maybe String)
    , iilsPrivateIpAddress :: !(Maybe String)
    } deriving (Show)

data DiskImageSet = DiskImageSet
    { disItem :: ![DiskImage]
    } deriving (Show)

data DiskImage = DiskImage
    { diImage :: !DiskImageDetail
    , diDescription :: !(Maybe String)
    , diVolume :: !DiskImageVolume
    } deriving (Show)

data DiskImageDetail = DiskImageDetail
    { didFormat :: !String
    , didBytes :: !Long
    , didImportManifestUrl :: !String
    } deriving (Show)

data DiskImageVolume = DiskImageVolume
    { divSize :: !Integer
    } deriving (Show)

data ConversionTask = ConversionTask
    { ctConversionTaskId :: !String
    , ctExpirationTime :: !(Maybe String)
  -- <xs:choice>
    , ctImportVolume :: !(Maybe ImportVolumeTaskDetails)
    , ctImportInstance :: !(Maybe ImportInstanceTaskDetails)
  -- </xs:choice>
    , ctState :: !String
    , ctStatusMessage :: !(Maybe String)
    , ctTagSet :: !(Maybe ResourceTagSet)
    } deriving (Show)

data ImportInstanceTaskDetails = ImportInstanceTaskDetails
    { iitdVolumes :: !ImportInstanceVolumeDetailSet
    , iitdInstanceId :: !(Maybe String)
    , iitdPlatform :: !(Maybe String)
    , iitdDescription :: !(Maybe String)
    } deriving (Show)

data ImportVolumeTaskDetails = ImportVolumeTaskDetails
    { ivtdBytesConverted :: !Long
    , ivtdAvailabilityZone :: !String
    , ivtdDescription :: !(Maybe String)
    , ivtdImage :: !DiskImageDescription
    , ivtdVolume :: !DiskImageVolumeDescription
    } deriving (Show)

data ImportInstanceVolumeDetailSet = ImportInstanceVolumeDetailSet
    { iivdsItem :: ![ImportInstanceVolumeDetailItem]
    } deriving (Show)

data ImportInstanceVolumeDetailItem = ImportInstanceVolumeDetailItem
    { iivdiBytesConverted :: !Long
    , iivdiAvailabilityZone :: !String
    , iivdiImage :: !DiskImageDescription
    , iivdiDescription :: !(Maybe String)
    , iivdiVolume :: !DiskImageVolumeDescription
    , iivdiStatus :: !String
    , iivdiStatusMessage :: !(Maybe String)
    } deriving (Show)

data DiskImageVolumeDescription = DiskImageVolumeDescription
    { divdSize :: !Integer
    , divdId :: !String
    } deriving (Show)

data DiskImageDescription = DiskImageDescription
    { didFormat :: !String
    , didSize :: !Long
    , didImportManifestUrl :: !String
    , didChecksum :: !(Maybe String)
    } deriving (Show)

data ImportVolume = ImportVolume
    { ivAvailabilityZone :: !String
    , ivImage :: !DiskImageDetail
    , ivDescription :: !(Maybe String)
    , ivVolume :: !DiskImageVolume
    } deriving (Show)

data ImportVolumeResponse = ImportVolumeResponse
    { ivrRequestId :: !String
    , ivrConversionTask :: !ConversionTask
    } deriving (Show)

data DescribeConversionTasks = DescribeConversionTasks
    { dctConversionTaskIdSet :: !ConversionTaskIdSet
    } deriving (Show)

data DescribeConversionTasksResponse = DescribeConversionTasksResponse
    { dctrRequestId :: !String
    , dctrConversionTasks :: !ConversionTaskSet
    } deriving (Show)

data ConversionTaskIdSet = ConversionTaskIdSet
    { ctisItem :: ![ConversionTaskIdItem]
    } deriving (Show)

data ConversionTaskIdItem = ConversionTaskIdItem
    { ctiiConversionTaskId :: !String
    } deriving (Show)

data ConversionTaskSet = ConversionTaskSet
    { ctsItem :: ![ConversionTask]
    } deriving (Show)

data CancelConversionTask = CancelConversionTask
    { cctConversionTaskId :: !String
    } deriving (Show)

data CancelConversionTaskResponse = CancelConversionTaskResponse
    { cctrRequestId :: !String
    , cctrReturn :: !Boolean
    } deriving (Show)

data CreateInstanceExportTask = CreateInstanceExportTask
    { cietDescription :: !(Maybe String)
    , cietInstanceId :: !String
    , cietTargetEnvironment :: !String
  -- <xs:choice>
    , cietExportToS3 :: !(Maybe ExportToS3Task)
  -- </xs:choice>
    } deriving (Show)

data ExportToS3Task = ExportToS3Task
    { ets3tDiskImageFormat :: !(Maybe String)
    , ets3tContainerFormat :: !(Maybe String)
    , ets3tS3Bucket :: !String
    , ets3tS3Prefix :: !String
    } deriving (Show)

data CreateInstanceExportTaskResponse = CreateInstanceExportTaskResponse
    { cietrRequestId :: !String
    , cietrExportTask :: !ExportTaskResponse
    } deriving (Show)

data DescribeExportTasks = DescribeExportTasks
    { detExportTaskIdSet :: !ExportTaskIdSet
    } deriving (Show)

data ExportTaskIdSet = ExportTaskIdSet
    { etisItem :: ![ExportTaskId]
    } deriving (Show)

data ExportTaskId = ExportTaskId
    { etiExportTaskId :: !String
    } deriving (Show)

data DescribeExportTasksResponse = DescribeExportTasksResponse
    { detrRequestId :: !String
    , detrExportTaskSet :: !ExportTaskSetResponse
    } deriving (Show)

data ExportTaskSetResponse = ExportTaskSetResponse
    { etsrItem :: ![ExportTaskResponse]
    } deriving (Show)

data ExportTaskResponse = ExportTaskResponse
    { etrExportTaskId :: !String
    , etrDescription :: !(Maybe String)
    , etrState :: !String
    , etrStatusMessage :: !(Maybe String)
  -- <xs:choice>
    , etrInstanceExport :: !(Maybe InstanceExportTaskResponse)
  -- </xs:choice>
  -- <xs:choice>
    , etrExportToS3 :: !(Maybe ExportToS3TaskResponse)
  -- </xs:choice>
    } deriving (Show)

data InstanceExportTaskResponse = InstanceExportTaskResponse
    { ietrInstanceId :: !String
    , ietrTargetEnvironment :: !(Maybe String)
    } deriving (Show)

data ExportToS3TaskResponse = ExportToS3TaskResponse
    { ets3trDiskImageFormat :: !String
    , ets3trContainerFormat :: !(Maybe String)
    , ets3trS3Bucket :: !String
    , ets3trS3Key :: !String
    } deriving (Show)

data CancelExportTask = CancelExportTask
    { cetExportTaskId :: !String
    } deriving (Show)

data CancelExportTaskResponse = CancelExportTaskResponse
    { cetrRequestId :: !String
    , cetrReturn :: !Boolean
    } deriving (Show)

data CreateInternetGateway = CreateInternetGateway

data InternetGatewayAttachmentSet = InternetGatewayAttachmentSet
    { igasItem :: ![InternetGatewayAttachment]
    } deriving (Show)

data InternetGatewayAttachment = InternetGatewayAttachment
    { igaVpcId :: !String
    , igaState :: !String
    } deriving (Show)

data InternetGateway = InternetGateway
    { igInternetGatewayId :: !String
    , igAttachmentSet :: !InternetGatewayAttachmentSet
    , igTagSet :: !(Maybe ResourceTagSet)
    } deriving (Show)

data CreateInternetGatewayResponse = CreateInternetGatewayResponse
    { cigrRequestId :: !String
    , cigrInternetGateway :: !InternetGateway
    } deriving (Show)

data InternetGatewayIdSet = InternetGatewayIdSet
    { igisItem :: ![InternetGatewayIdSetItem]
    } deriving (Show)

data InternetGatewayIdSetItem = InternetGatewayIdSetItem
    { igisiInternetGatewayId :: !String
    } deriving (Show)

data DescribeInternetGateways = DescribeInternetGateways
    { digInternetGatewayIdSet :: !InternetGatewayIdSet
    , digFilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data InternetGatewaySet = InternetGatewaySet
    { igsItem :: ![InternetGateway]
    } deriving (Show)

data DescribeInternetGatewaysResponse = DescribeInternetGatewaysResponse
    { digrRequestId :: !String
    , digrInternetGatewaySet :: !InternetGatewaySet
    } deriving (Show)

data DeleteInternetGateway = DeleteInternetGateway
    { digInternetGatewayId :: !String
    } deriving (Show)

data DeleteInternetGatewayResponse = DeleteInternetGatewayResponse
    { digrRequestId :: !String
    , digrReturn :: !Boolean
    } deriving (Show)

data AttachInternetGateway = AttachInternetGateway
    { aigInternetGatewayId :: !String
    , aigVpcId :: !String
    } deriving (Show)

data AttachInternetGatewayResponse = AttachInternetGatewayResponse
    { aigrRequestId :: !String
    , aigrReturn :: !Boolean
    } deriving (Show)

data DetachInternetGateway = DetachInternetGateway
    { digInternetGatewayId :: !String
    , digVpcId :: !String
    } deriving (Show)

data DetachInternetGatewayResponse = DetachInternetGatewayResponse
    { digrRequestId :: !String
    , digrReturn :: !Boolean
    } deriving (Show)

data CreateRouteTable = CreateRouteTable
    { crtVpcId :: !String
    } deriving (Show)

data RouteSet = RouteSet
    { rsItem :: ![Route]
    } deriving (Show)

data Route = Route
    { rDestinationCidrBlock :: !String
    , rGatewayId :: !(Maybe String)
    , rInstanceId :: !(Maybe String)
    , rInstanceOwnerId :: !(Maybe String)
    , rNetworkInterfaceId :: !(Maybe String)
    , rState :: !String
    , rOrigin :: !String
    } deriving (Show)

data RouteTableAssociationSet = RouteTableAssociationSet
    { rtasItem :: ![RouteTableAssociation]
    } deriving (Show)

data RouteTableAssociation = RouteTableAssociation
    { rtaRouteTableAssociationId :: !String
    , rtaRouteTableId :: !String
  -- <xs:choice>
    , rtaSubnetId :: !(Maybe String)
    , rtaMain :: !(Maybe Boolean)
  -- </xs:choice>
    } deriving (Show)

data PropagatingVgwSet = PropagatingVgwSet
    { pvsItem :: ![PropagatingVgw]
    } deriving (Show)

data PropagatingVgw = PropagatingVgw
    { pvGatewayId :: !String
    } deriving (Show)

data RouteTable = RouteTable
    { rtRouteTableId :: !String
    , rtVpcId :: !String
    , rtRouteSet :: !RouteSet
    , rtAssociationSet :: !RouteTableAssociationSet
    , rtPropagatingVgwSet :: !PropagatingVgwSet
    , rtTagSet :: !(Maybe ResourceTagSet)
    } deriving (Show)

data CreateRouteTableResponse = CreateRouteTableResponse
    { crtrRequestId :: !String
    , crtrRouteTable :: !RouteTable
    } deriving (Show)

data RouteTableIdSet = RouteTableIdSet
    { rtisItem :: ![RouteTableIdSetItem]
    } deriving (Show)

data RouteTableIdSetItem = RouteTableIdSetItem
    { rtisiRouteTableId :: !String
    } deriving (Show)

data DescribeRouteTables = DescribeRouteTables
    { drtRouteTableIdSet :: !RouteTableIdSet
    , drtFilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data RouteTableSet = RouteTableSet
    { rtsItem :: ![RouteTable]
    } deriving (Show)

data DescribeRouteTablesResponse = DescribeRouteTablesResponse
    { drtrRequestId :: !String
    , drtrRouteTableSet :: !RouteTableSet
    } deriving (Show)

data EnableVgwRoutePropagationRequest = EnableVgwRoutePropagationRequest
    { evrprRouteTableId :: !String
    , evrprGatewayId :: !String
    } deriving (Show)

data EnableVgwRoutePropagationResponse = EnableVgwRoutePropagationResponse
    { evrprRequestId :: !String
    , evrprReturn :: !Boolean
    } deriving (Show)

data DisableVgwRoutePropagationRequest = DisableVgwRoutePropagationRequest
    { dvrprRouteTableId :: !String
    , dvrprGatewayId :: !String
    } deriving (Show)

data DisableVgwRoutePropagationResponse = DisableVgwRoutePropagationResponse
    { dvrprRequestId :: !String
    , dvrprReturn :: !Boolean
    } deriving (Show)

data DeleteRouteTable = DeleteRouteTable
    { drtRouteTableId :: !String
    } deriving (Show)

data DeleteRouteTableResponse = DeleteRouteTableResponse
    { drtrRequestId :: !String
    , drtrReturn :: !Boolean
    } deriving (Show)

data AssociateRouteTable = AssociateRouteTable
    { artRouteTableId :: !String
    , artSubnetId :: !String
    } deriving (Show)

data AssociateRouteTableResponse = AssociateRouteTableResponse
    { artrRequestId :: !String
    , artrAssociationId :: !String
    } deriving (Show)

data ReplaceRouteTableAssociation = ReplaceRouteTableAssociation
    { rrtaAssociationId :: !String
    , rrtaRouteTableId :: !String
    } deriving (Show)

data ReplaceRouteTableAssociationResponse = ReplaceRouteTableAssociationResponse
    { rrtarRequestId :: !String
    , rrtarNewAssociationId :: !String
    } deriving (Show)

data DisassociateRouteTable = DisassociateRouteTable
    { drtAssociationId :: !String
    } deriving (Show)

data DisassociateRouteTableResponse = DisassociateRouteTableResponse
    { drtrRequestId :: !String
    , drtrReturn :: !Boolean
    } deriving (Show)

data CreateRoute = CreateRoute
    { crRouteTableId :: !String
    , crDestinationCidrBlock :: !String
  -- <xs:choice>
    , crGatewayId :: !(Maybe String)
    , crInstanceId :: !(Maybe String)
    , crNetworkInterfaceId :: !(Maybe String)
  -- </xs:choice>
    } deriving (Show)

data CreateRouteResponse = CreateRouteResponse
    { crrRequestId :: !String
    , crrReturn :: !Boolean
    } deriving (Show)

data ReplaceRoute = ReplaceRoute
    { rrRouteTableId :: !String
    , rrDestinationCidrBlock :: !String
  -- <xs:choice>
    , rrGatewayId :: !(Maybe String)
    , rrInstanceId :: !(Maybe String)
    , rrNetworkInterfaceId :: !(Maybe String)
  -- </xs:choice>
    } deriving (Show)

data ReplaceRouteResponse = ReplaceRouteResponse
    { rrrRequestId :: !String
    , rrrReturn :: !Boolean
    } deriving (Show)

data DeleteRoute = DeleteRoute
    { drRouteTableId :: !String
    , drDestinationCidrBlock :: !String
    } deriving (Show)

data DeleteRouteResponse = DeleteRouteResponse
    { drrRequestId :: !String
    , drrReturn :: !Boolean
    } deriving (Show)

data CreateNetworkAcl = CreateNetworkAcl
    { cnaVpcId :: !String
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
    , naeProtocol :: !String
    , naeRuleAction :: !String
    , naeEgress :: !Boolean
    , naeCidrBlock :: !String
    , naeIcmpCode :: !(Maybe IcmpCode)
    , naePortRange :: !(Maybe PortRange)
    } deriving (Show)

data NetworkAclAssociationSet = NetworkAclAssociationSet
    { naasItem :: ![NetworkAclAssociation]
    } deriving (Show)

data NetworkAclAssociation = NetworkAclAssociation
    { naaNetworkAclAssociationId :: !String
    , naaNetworkAclId :: !String
    , naaSubnetId :: !String
    } deriving (Show)

data NetworkAcl = NetworkAcl
    { naNetworkAclId :: !String
    , naVpcId :: !String
    , naDefault :: !Boolean
    , naEntrySet :: !NetworkAclEntrySet
    , naAssociationSet :: !NetworkAclAssociationSet
    , naTagSet :: !(Maybe ResourceTagSet)
    } deriving (Show)

data CreateNetworkAclResponse = CreateNetworkAclResponse
    { cnarRequestId :: !String
    , cnarNetworkAcl :: !NetworkAcl
    } deriving (Show)

data NetworkAclIdSet = NetworkAclIdSet
    { naisItem :: ![NetworkAclIdSetItem]
    } deriving (Show)

data NetworkAclIdSetItem = NetworkAclIdSetItem
    { naisiNetworkAclId :: !String
    } deriving (Show)

data DescribeNetworkAcls = DescribeNetworkAcls
    { dnaNetworkAclIdSet :: !NetworkAclIdSet
    , dnaFilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data NetworkAclSet = NetworkAclSet
    { nasItem :: ![NetworkAcl]
    } deriving (Show)

data DescribeNetworkAclsResponse = DescribeNetworkAclsResponse
    { dnarRequestId :: !String
    , dnarNetworkAclSet :: !NetworkAclSet
    } deriving (Show)

data DeleteNetworkAcl = DeleteNetworkAcl
    { dnaNetworkAclId :: !String
    } deriving (Show)

data DeleteNetworkAclResponse = DeleteNetworkAclResponse
    { dnarRequestId :: !String
    , dnarReturn :: !Boolean
    } deriving (Show)

data ReplaceNetworkAclAssociation = ReplaceNetworkAclAssociation
    { rnaaAssociationId :: !String
    , rnaaNetworkAclId :: !String
    } deriving (Show)

data ReplaceNetworkAclAssociationResponse = ReplaceNetworkAclAssociationResponse
    { rnaarRequestId :: !String
    , rnaarNewAssociationId :: !String
    } deriving (Show)

data CreateNetworkAclEntry = CreateNetworkAclEntry
    { cnaeNetworkAclId :: !String
    , cnaeRuleNumber :: !Int
    , cnaeProtocol :: !String
    , cnaeRuleAction :: !String
    , cnaeEgress :: !Boolean
    , cnaeCidrBlock :: !String
    , cnaeIcmpCode :: !(Maybe IcmpCode)
    , cnaePortRange :: !(Maybe PortRange)
    } deriving (Show)

data CreateNetworkAclEntryResponse = CreateNetworkAclEntryResponse
    { cnaerRequestId :: !String
    , cnaerReturn :: !Boolean
    } deriving (Show)

data ReplaceNetworkAclEntry = ReplaceNetworkAclEntry
    { rnaeNetworkAclId :: !String
    , rnaeRuleNumber :: !Int
    , rnaeProtocol :: !String
    , rnaeRuleAction :: !String
    , rnaeEgress :: !Boolean
    , rnaeCidrBlock :: !String
    , rnaeIcmpCode :: !(Maybe IcmpCode)
    , rnaePortRange :: !(Maybe PortRange)
    } deriving (Show)

data ReplaceNetworkAclEntryResponse = ReplaceNetworkAclEntryResponse
    { rnaerRequestId :: !String
    , rnaerReturn :: !Boolean
    } deriving (Show)

data DeleteNetworkAclEntry = DeleteNetworkAclEntry
    { dnaeNetworkAclId :: !String
    , dnaeRuleNumber :: !Int
    , dnaeEgress :: !Boolean
    } deriving (Show)

data DeleteNetworkAclEntryResponse = DeleteNetworkAclEntryResponse
    { dnaerRequestId :: !String
    , dnaerReturn :: !Boolean
    } deriving (Show)

data DescribeInstanceStatus = DescribeInstanceStatus
    { disInstancesSet :: !InstanceIdSet
    , disFilterSet :: !(Maybe FilterSet)
    , disNextToken :: !(Maybe String)
    , disMaxResults :: !(Maybe Int)
    , disIncludeAllInstances :: !(Maybe Boolean)
    } deriving (Show)

data DescribeInstanceStatusResponse = DescribeInstanceStatusResponse
    { disrRequestId :: !String
    , disrInstanceStatusSet :: !InstanceStatusSet
    , disrNextToken :: !(Maybe String)
    } deriving (Show)

data InstanceStatusSet = InstanceStatusSet
    { issItem :: ![InstanceStatusItem]
    } deriving (Show)

data InstanceStatus = InstanceStatus
    { isStatus :: !String
    , isDetails :: !(Maybe InstanceStatusDetailsSet)
    } deriving (Show)

data InstanceStatusDetailsSet = InstanceStatusDetailsSet
    { isdsItem :: ![InstanceStatusDetailsSetItem]
    } deriving (Show)

data InstanceStatusDetailsSetItem = InstanceStatusDetailsSetItem
    { isdsiName :: !String
    , isdsiStatus :: !String
    , isdsiImpairedSince :: !(Maybe DateTime)
    } deriving (Show)

data InstanceStatusEvent = InstanceStatusEvent
    { iseCode :: !String
    , iseDescription :: !String
    , iseNotBefore :: !DateTime
    , iseNotAfter :: !(Maybe DateTime)
    } deriving (Show)

data InstanceStatusEventsSet = InstanceStatusEventsSet
    { isesItem :: ![InstanceStatusEvent]
    } deriving (Show)

data InstanceStatusItem = InstanceStatusItem
    { isiInstanceId :: !String
    , isiAvailabilityZone :: !String
    , isiEventsSet :: !(Maybe InstanceStatusEventsSet)
    , isiInstanceState :: !InstanceState
    , isiSystemStatus :: !InstanceStatus
    , isiInstanceStatus :: !InstanceStatus
    } deriving (Show)

data ReportInstanceStatus = ReportInstanceStatus
    { risInstancesSet :: !InstanceIdSet
    , risStatus :: !String
    , risStartTime :: !(Maybe DateTime)
    , risEndTime :: !(Maybe DateTime)
    , risReasonCodesSet :: !ReportInstanceStatusReasonCodesSet
    , risDescription :: !(Maybe String)
    } deriving (Show)

data ReportInstanceStatusReasonCodesSet = ReportInstanceStatusReasonCodesSet
    { risrcsItem :: !(NonEmpty ReportInstanceStatusReasonCodeSetItem)
    } deriving (Show)

data ReportInstanceStatusReasonCodeSetItem = ReportInstanceStatusReasonCodeSetItem
    { risrcsiReasonCode :: !String
    } deriving (Show)

data ReportInstanceStatusResponse = ReportInstanceStatusResponse
    { risrRequestId :: !String
    , risrReturn :: !Boolean
    } deriving (Show)

data CreateNetworkInterface = CreateNetworkInterface
    { cniSubnetId :: !String
    , cniDescription :: !(Maybe String)
    , cniPrivateIpAddress :: !(Maybe String)
    , cniGroupSet :: !(Maybe SecurityGroupIdSet)
    , cniPrivateIpAddressesSet :: !(Maybe PrivateIpAddressesSetRequest)
    , cniSecondaryPrivateIpAddressCount :: !(Maybe Int)
    } deriving (Show)

data CreateNetworkInterfaceResponse = CreateNetworkInterfaceResponse
    { cnirRequestId :: !String
    , cnirNetworkInterface :: !NetworkInterface
    } deriving (Show)

data NetworkInterfaceIdSet = NetworkInterfaceIdSet
    { niisItem :: ![NetworkInterfaceIdSetItem]
    } deriving (Show)

data NetworkInterfaceIdSetItem = NetworkInterfaceIdSetItem
    { niisiNetworkInterfaceId :: !String
    } deriving (Show)

data DescribeNetworkInterfaces = DescribeNetworkInterfaces
    { dniNetworkInterfaceIdSet :: !(Maybe NetworkInterfaceIdSet)
    , dniFilterSet :: !(Maybe FilterSet)
    } deriving (Show)

data NetworkInterface = NetworkInterface
    { niNetworkInterfaceId :: !String
    , niSubnetId :: !(Maybe String)
    , niVpcId :: !(Maybe String)
    , niAvailabilityZone :: !(Maybe String)
    , niDescription :: !(Maybe String)
    , niOwnerId :: !String
    , niRequesterId :: !(Maybe String)
    , niRequesterManaged :: !(Maybe Boolean)
    , niStatus :: !String
    , niMacAddress :: !String
    , niPrivateIpAddress :: !String
    , niPrivateDnsName :: !(Maybe String)
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
    { nipiasiPrivateIpAddress :: !String
    , nipiasiPrivateDnsName :: !(Maybe String)
    , nipiasiPrimary :: !Boolean
    , nipiasiAssociation :: !(Maybe NetworkInterfaceAssociation)
    } deriving (Show)

data NetworkInterfaceAttachment = NetworkInterfaceAttachment
    { niaAttachmentId :: !String
    , niaInstanceId :: !(Maybe String)
    , niaInstanceOwnerId :: !(Maybe String)
    , niaDeviceIndex :: !Int
    , niaStatus :: !String
    , niaAttachTime :: !DateTime
    , niaDeleteOnTermination :: !Boolean
    } deriving (Show)

data NetworkInterfaceAssociation = NetworkInterfaceAssociation
    { niaPublicIp :: !String
    , niaPublicDnsName :: !(Maybe String)
    , niaIpOwnerId :: !(Maybe String)
    , niaAllocationId :: !(Maybe String)
    , niaAssociationId :: !(Maybe String)
    } deriving (Show)

data NetworkInterfaceSet = NetworkInterfaceSet
    { nisItem :: ![NetworkInterface]
    } deriving (Show)

data DescribeNetworkInterfacesResponse = DescribeNetworkInterfacesResponse
    { dnirRequestId :: !String
    , dnirNetworkInterfaceSet :: !NetworkInterfaceSet
    } deriving (Show)

data DeleteNetworkInterface = DeleteNetworkInterface
    { dniNetworkInterfaceId :: !String
    } deriving (Show)

data DeleteNetworkInterfaceResponse = DeleteNetworkInterfaceResponse
    { dnirRequestId :: !String
    , dnirReturn :: !Boolean
    } deriving (Show)

data AttachNetworkInterface = AttachNetworkInterface
    { aniNetworkInterfaceId :: !String
    , aniInstanceId :: !String
    , aniDeviceIndex :: !Int
    } deriving (Show)

data AttachNetworkInterfaceResponse = AttachNetworkInterfaceResponse
    { anirRequestId :: !String
    , anirAttachmentId :: !String
    } deriving (Show)

data DetachNetworkInterface = DetachNetworkInterface
    { dniAttachmentId :: !String
    , dniForce :: !(Maybe Boolean)
    } deriving (Show)

data DetachNetworkInterfaceResponse = DetachNetworkInterfaceResponse
    { dnirRequestId :: !String
    , dnirReturn :: !Boolean
    } deriving (Show)

data DescribeNetworkInterfaceAttribute = DescribeNetworkInterfaceAttribute
    { dniaNetworkInterfaceId :: !String
    , dniaDescribeNetworkInterfaceAttributesGroup :: !DescribeNetworkInterfaceAttributesGroup
    } deriving (Show)

data DescribeNetworkInterfaceAttributesGroup = DescribeNetworkInterfaceAttributesGroup
    { dniagDescription :: !EmptyElement
    , dniagSourceDestCheck :: !EmptyElement
    , dniagGroupSet :: !EmptyElement
    , dniagAttachment :: !EmptyElement
    } deriving (Show)
data DescribeNetworkInterfaceAttributeResponse = DescribeNetworkInterfaceAttributeResponse
    { dniarRequestId :: !String
    , dniarNetworkInterfaceId :: !String
  -- <xs:choice>
    , dniarDescription :: !(Maybe NullableAttributeValue)
    , dniarSourceDestCheck :: !(Maybe AttributeBooleanValue)
    , dniarGroupSet :: !(Maybe GroupSet)
    , dniarAttachment :: !(Maybe NetworkInterfaceAttachment)
  -- </xs:choice>
    } deriving (Show)

data ModifyNetworkInterfaceAttribute = ModifyNetworkInterfaceAttribute
    { mniaNetworkInterfaceId :: !String
  -- <xs:choice>
    , mniaDescription :: !(Maybe NullableAttributeValue)
    , mniaSourceDestCheck :: !(Maybe AttributeBooleanValue)
    , mniaGroupSet :: !(Maybe SecurityGroupIdSet)
    , mniaAttachment :: !(Maybe ModifyNetworkInterfaceAttachment)
  -- </xs:choice>
    } deriving (Show)

data ModifyNetworkInterfaceAttachment = ModifyNetworkInterfaceAttachment
    { mniaAttachmentId :: !String
    , mniaDeleteOnTermination :: !Boolean
    } deriving (Show)

data ModifyNetworkInterfaceAttributeResponse = ModifyNetworkInterfaceAttributeResponse
    { mniarRequestId :: !String
    , mniarReturn :: !Boolean
    } deriving (Show)

data ResetNetworkInterfaceAttribute = ResetNetworkInterfaceAttribute
    { rniaNetworkInterfaceId :: !String
    , rniaResetNetworkInterfaceAttributesGroup :: !ResetNetworkInterfaceAttributesGroup
    } deriving (Show)

data ResetNetworkInterfaceAttributesGroup = ResetNetworkInterfaceAttributesGroup
    { rniagSourceDestCheck :: !EmptyElement
    } deriving (Show)
data ResetNetworkInterfaceAttributeResponse = ResetNetworkInterfaceAttributeResponse
    { rniarRequestId :: !String
    , rniarReturn :: !Boolean
    } deriving (Show)

data AssignPrivateIpAddresses = AssignPrivateIpAddresses
    { apiaNetworkInterfaceId :: !String
    , apiaPrivateIpAddressesSet :: !(Maybe AssignPrivateIpAddressesSetRequest)
    , apiaSecondaryPrivateIpAddressCount :: !(Maybe Int)
    , apiaAllowReassignment :: !(Maybe Boolean)
    } deriving (Show)

data AssignPrivateIpAddressesResponse = AssignPrivateIpAddressesResponse
    { apiarRequestId :: !String
    , apiarReturn :: !Boolean
    } deriving (Show)

data UnassignPrivateIpAddresses = UnassignPrivateIpAddresses
    { upiaNetworkInterfaceId :: !String
    , upiaPrivateIpAddressesSet :: !AssignPrivateIpAddressesSetRequest
    } deriving (Show)

data UnassignPrivateIpAddressesResponse = UnassignPrivateIpAddressesResponse
    { upiarRequestId :: !String
    , upiarReturn :: !Boolean
    } deriving (Show)

data AssignPrivateIpAddressesSetRequest = AssignPrivateIpAddressesSetRequest
    { apiasrItem :: !(NonEmpty AssignPrivateIpAddressesSetItemRequest)
    } deriving (Show)

data AssignPrivateIpAddressesSetItemRequest = AssignPrivateIpAddressesSetItemRequest
    { apiasirPrivateIpAddress :: !String
    } deriving (Show)

data DescribeVolumeStatus = DescribeVolumeStatus
    { dvsVolumeSet :: !DescribeVolumesSet
    , dvsFilterSet :: !(Maybe FilterSet)
    , dvsMaxResults :: !(Maybe Integer)
    , dvsNextToken :: !(Maybe String)
    } deriving (Show)

data DescribeVolumeStatusResponse = DescribeVolumeStatusResponse
    { dvsrRequestId :: !String
    , dvsrVolumeStatusSet :: !VolumeStatusSet
    , dvsrNextToken :: !(Maybe String)
    } deriving (Show)

data VolumeStatusSet = VolumeStatusSet
    { vssItem :: ![VolumeStatusItem]
    } deriving (Show)

data VolumeStatusItem = VolumeStatusItem
    { vsiVolumeId :: !String
    , vsiAvailabilityZone :: !String
    , vsiVolumeStatus :: !VolumeStatusInfo
    , vsiEventsSet :: !VolumeStatusEventsSet
    , vsiActionsSet :: !VolumeStatusActionsSet
    } deriving (Show)

data VolumeStatusInfo = VolumeStatusInfo
    { vsiStatus :: !String
    , vsiDetails :: !VolumeStatusDetailsSet
    } deriving (Show)

data VolumeStatusDetailsSet = VolumeStatusDetailsSet
    { vsdsItem :: ![VolumeStatusDetailsItem]
    } deriving (Show)

data VolumeStatusDetailsItem = VolumeStatusDetailsItem
    { vsdiName :: !String
    , vsdiStatus :: !String
    } deriving (Show)

data VolumeStatusEventsSet = VolumeStatusEventsSet
    { vsesItem :: ![VolumeStatusEventItem]
    } deriving (Show)

data VolumeStatusEventItem = VolumeStatusEventItem
    { vseiDescription :: !String
    , vseiNotBefore :: !DateTime
    , vseiNotAfter :: !DateTime
    , vseiEventId :: !String
    , vseiEvent :: !String
    } deriving (Show)

data VolumeStatusActionsSet = VolumeStatusActionsSet
    { vsasItem :: ![VolumeStatusActionItem]
    } deriving (Show)

data VolumeStatusActionItem = VolumeStatusActionItem
    { vsaiDescription :: !String
    , vsaiCode :: !String
    , vsaiEventId :: !String
    , vsaiEvent :: !String
    } deriving (Show)

data EnableVolumeIO = EnableVolumeIO
    { evioVolumeId :: !String
    } deriving (Show)

data EnableVolumeIOResponse = EnableVolumeIOResponse
    { eviorRequestId :: !String
    , eviorReturn :: !Boolean
    } deriving (Show)

data ModifyVolumeAttribute = ModifyVolumeAttribute
    { mvaVolumeId :: !String
  -- <xs:choice>
    , mvaAutoEnableIO :: !AttributeBooleanValue
  -- </xs:choice>
    } deriving (Show)

data ModifyVolumeAttributeResponse = ModifyVolumeAttributeResponse
    { mvarRequestId :: !String
    , mvarReturn :: !Boolean
    } deriving (Show)

data DescribeVolumeAttribute = DescribeVolumeAttribute
    { dvaVolumeId :: !String
    , dvaDescribeVolumeAttributesGroup :: !DescribeVolumeAttributesGroup
    } deriving (Show)

data DescribeVolumeAttributesGroup = DescribeVolumeAttributesGroup
    { dvagAutoEnableIO :: !EmptyElement
    , dvagProductCodes :: !EmptyElement
    } deriving (Show)

data DescribeVolumeAttributeResponse = DescribeVolumeAttributeResponse
    { dvarRequestId :: !String
    , dvarVolumeId :: !String
  -- <xs:choice>
    , dvarAutoEnableIO :: !(Maybe NullableAttributeBooleanValue)
    , dvarProductCodes :: !(Maybe ProductCodesSet)
  -- </xs:choice>
    } deriving (Show)
