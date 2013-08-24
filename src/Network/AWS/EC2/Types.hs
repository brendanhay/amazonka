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

import Network.AWS.Internal
import Text.ParserCombinators.ReadP (string)
import Text.Read

data Domain = Standard | VPC
    deriving (Eq)

instance Show Domain where
    show Standard = "standard"
    show VPC      = "vpc"

instance Read Domain where
    readPrec = choice $ map (\(x, y) -> lift $ string x >> return y)
         [ ("standard", Standard)
         , ("vpc",      VPC)
         ]

instance IsQuery Domain where
    queryPickler = qpPrim

-- data CreateImage = CreateImage
--     { ciInstanceId :: !ByteString
--     , ciName :: !ByteString
--     , ciDescription :: !(Maybe ByteString)
--     , ciNoReboot :: !(Maybe Boolean)
--     , ciBlockDeviceMapping :: !(Maybe BlockDeviceMapping)
--     } deriving (Show)

-- data CreateImageResponse = CreateImageResponse
--     { cirRequestId :: !ByteString
--     , cirImageId :: !ByteString
--     } deriving (Show)

-- data RegisterImage = RegisterImage
--     { riImageLocation :: !(Maybe ByteString)
--     , riName :: !ByteString
--     , riDescription :: !(Maybe ByteString)
--     , riArchitecture :: !(Maybe ByteString)
--     , riKernelId :: !(Maybe ByteString)
--     , riRamdiskId :: !(Maybe ByteString)
--     , riRootDeviceName :: !(Maybe ByteString)
--     , riBlockDeviceMapping :: !(Maybe BlockDeviceMapping)
--     } deriving (Show)

-- data RegisterImageResponse = RegisterImageResponse
--     { rirRequestId :: !ByteString
--     , rirImageId :: !ByteString
--     } deriving (Show)

-- data DeregisterImage = DeregisterImage
--     { diImageId :: !ByteString
--     } deriving (Show)

-- data DeregisterImageResponse = DeregisterImageResponse
--     { dirRequestId :: !ByteString
--     , dirReturn :: !Boolean
--     } deriving (Show)

-- data CreateKeyPair = CreateKeyPair
--     { ckpKeyName :: !ByteString
--     } deriving (Show)

-- data CreateKeyPairResponse = CreateKeyPairResponse
--     { ckprRequestId :: !ByteString
--     , ckprKeyName :: !ByteString
--     , ckprKeyFingerprint :: !ByteString
--     , ckprKeyMaterial :: !ByteString
--     } deriving (Show)

-- data ImportKeyPair = ImportKeyPair
--     { ikpKeyName :: !ByteString
--     , ikpPublicKeyMaterial :: !ByteString
--     } deriving (Show)

-- data ImportKeyPairResponse = ImportKeyPairResponse
--     { ikprRequestId :: !ByteString
--     , ikprKeyName :: !ByteString
--     , ikprKeyFingerprint :: !ByteString
--     } deriving (Show)

-- data DeleteKeyPair = DeleteKeyPair
--     { dkpKeyName :: !ByteString
--     } deriving (Show)

-- data DeleteKeyPairResponse = DeleteKeyPairResponse
--     { dkprRequestId :: !ByteString
--     , dkprReturn :: !Boolean
--     } deriving (Show)

-- data DescribeKeyPairs = DescribeKeyPairs
--     { dkpKeySet :: !DescribeKeyPairsInfo
--     , dkpFilterSet :: !(Maybe FilterSet)
--     } deriving (Show)

-- data DescribeKeyPairsInfo = DescribeKeyPairsInfo
--     { dkpiItem :: ![DescribeKeyPairsItem]
--     } deriving (Show)

-- data DescribeKeyPairsItem = DescribeKeyPairsItem
--     { dkpiKeyName :: !ByteString
--     } deriving (Show)

-- data DescribeKeyPairsResponse = DescribeKeyPairsResponse
--     { dkprRequestId :: !ByteString
--     , dkprKeySet :: !DescribeKeyPairsResponseInfo
--     } deriving (Show)

-- data DescribeKeyPairsResponseInfo = DescribeKeyPairsResponseInfo
--     { dkpriItem :: ![DescribeKeyPairsResponseItem]
--     } deriving (Show)

-- data DescribeKeyPairsResponseItem = DescribeKeyPairsResponseItem
--     { dkpriKeyName :: !ByteString
--     , dkpriKeyFingerprint :: !ByteString
--     } deriving (Show)

-- data RunInstances = RunInstances
--     { riImageId :: !ByteString
--     , riMinCount :: !Int
--     , riMaxCount :: !Int
--     , riKeyName :: !(Maybe ByteString)
--     , riGroupSet :: !GroupSet
--     , riUserData :: !(Maybe UserData)
--     , riInstance :: !ByteString
--     , riPlacement :: !(Maybe PlacementRequest)
--     , riKernelId :: !(Maybe ByteString)
--     , riRamdiskId :: !(Maybe ByteString)
--     , riBlockDeviceMapping :: !(Maybe BlockDeviceMapping)
--     , riMonitoring :: !(Maybe MonitoringInstance)
--     , riSubnetId :: !(Maybe ByteString)
--     , riDisableApiTermination :: !(Maybe Boolean)
--     , riInstanceInitiatedShutdownBehavior :: !(Maybe ByteString)
--     , riLicense :: !(Maybe InstanceLicenseRequest)
--     , riPrivateIpAddress :: !(Maybe ByteString)
--     , riClientToken :: !(Maybe ByteString)
--     , riNetworkInterfaceSet :: !(Maybe InstanceNetworkInterfaceSetRequest)
--     , riIamInstanceProfile :: !(Maybe IamInstanceProfileRequest)
--     , riEbsOptimized :: !(Maybe Boolean)
--     } deriving (Show)

-- data IamInstanceProfileRequest = IamInstanceProfileRequest
--     { iiprArn :: !(Maybe ByteString)
--     , iiprName :: !(Maybe ByteString)
--     } deriving (Show)

-- data InstanceNetworkInterfaceSetRequest = InstanceNetworkInterfaceSetRequest
--     { inisrItem :: ![InstanceNetworkInterfaceSetItemRequest]
--     } deriving (Show)

-- data InstanceNetworkInterfaceSetItemRequest = InstanceNetworkInterfaceSetItemRequest
--     { inisirNetworkInterfaceId :: !(Maybe ByteString)
--     , inisirDeviceIndex :: !Int
--     , inisirSubnetId :: !(Maybe ByteString)
--     , inisirDescription :: !(Maybe ByteString)
--     , inisirPrivateIpAddress :: !(Maybe ByteString)
--     , inisirGroupSet :: !(Maybe SecurityGroupIdSet)
--     , inisirDeleteOnTermination :: !(Maybe Boolean)
--     , inisirPrivateIpAddressesSet :: !(Maybe PrivateIpAddressesSetRequest)
--     , inisirSecondaryPrivateIpAddressCount :: !(Maybe Int)
--     } deriving (Show)

-- data PrivateIpAddressesSetRequest = PrivateIpAddressesSetRequest
--     { piasrItem :: ![PrivateIpAddressesSetItemRequest]
--     } deriving (Show)

-- data PrivateIpAddressesSetItemRequest = PrivateIpAddressesSetItemRequest
--     { piasirPrivateIpAddress :: !ByteString
--     , piasirPrimary :: !(Maybe Boolean)
--     } deriving (Show)

-- data ImportInstanceGroupSet = ImportInstanceGroupSet
--     { iigsItem :: ![ImportInstanceGroupItem]
--     } deriving (Show)

-- data ImportInstanceGroupItem = ImportInstanceGroupItem
--     { iigiGroupId :: !(Maybe ByteString)
--     , iigiGroupName :: !(Maybe ByteString)
--     } deriving (Show)

-- data GroupSet = GroupSet
--     { gsItem :: ![GroupItem]
--     } deriving (Show)

-- data GroupItem = GroupItem
--     { giGroupId :: !(Maybe ByteString)
--     , giGroupName :: !(Maybe ByteString)
--     } deriving (Show)

-- data UserData = UserData
--     { udData :: !(Maybe ByteString)
--     , udVersion :: !ByteString
--     , udEncoding :: !ByteString
--     } deriving (Show)

-- data BlockDeviceMapping = BlockDeviceMapping
--     { bdmItem :: ![BlockDeviceMappingItem]
--     } deriving (Show)

-- data BlockDeviceMappingItem = BlockDeviceMappingItem
--     { bdmiDeviceName :: !ByteString
--   -- <xs:choice>
--     , bdmiVirtualName :: !(Maybe ByteString)
--     , bdmiEbs :: !(Maybe EbsBlockDevice)
--     , bdmiNoDevice :: !(Maybe EmptyElement)
--   -- </xs:choice>
--     } deriving (Show)

-- data EbsBlockDevice = EbsBlockDevice
--     { ebdSnapshotId :: !(Maybe ByteString)
--     , ebdVolumeSize :: !(Maybe Int)
--     , ebdDeleteOnTermination :: !(Maybe Boolean)
--     , ebdVolume :: !(Maybe ByteString)
--     , ebdIops :: !(Maybe Int)
--     } deriving (Show)

-- data PlacementRequest = PlacementRequest
--     { prAvailabilityZone :: !(Maybe ByteString)
--     , prGroupName :: !(Maybe ByteString)
--     , prTenancy :: !(Maybe ByteString)
--     } deriving (Show)

-- data SpotPlacementRequest = SpotPlacementRequest
--     { sprAvailabilityZone :: !(Maybe ByteString)
--     , sprGroupName :: !(Maybe ByteString)
--     } deriving (Show)

-- data InstancePlacement = InstancePlacement
--     { ipAvailabilityZone :: !(Maybe ByteString)
--     , ipGroupName :: !(Maybe ByteString)
--     } deriving (Show)

-- data MonitoringInstance = MonitoringInstance
--     { miEnabled :: !(Maybe Boolean)
--     } deriving (Show)

-- data InstanceLicenseRequest = InstanceLicenseRequest
--     { ilrPool :: !ByteString
--     } deriving (Show)

-- data RunInstancesResponse = RunInstancesResponse
--     { rirRequestId :: !ByteString
--     , rirReservationId :: !ByteString
--     , rirOwnerId :: !ByteString
--     , rirGroupSet :: !GroupSet
--     , rirInstancesSet :: !RunningInstancesSet
--     , rirRequesterId :: !(Maybe ByteString)
--     } deriving (Show)

-- data ReservationInfo = ReservationInfo
--     { riReservationId :: !ByteString
--     , riOwnerId :: !ByteString
--     , riGroupSet :: !GroupSet
--     , riInstancesSet :: !RunningInstancesSet
--     , riRequesterId :: !(Maybe ByteString)
--     } deriving (Show)

-- data RunningInstancesSet = RunningInstancesSet
--     { risItem :: ![RunningInstancesItem]
--     } deriving (Show)

-- data RunningInstancesItem = RunningInstancesItem
--     { riiInstanceId :: !ByteString
--     , riiImageId :: !(Maybe ByteString)
--     , riiInstanceState :: !InstanceState
--     , riiPrivateDnsName :: !ByteString
--     , riiDnsName :: !(Maybe ByteString)
--     , riiReason :: !(Maybe ByteString)
--     , riiKeyName :: !(Maybe ByteString)
--     , riiAmiLaunchIndex :: !(Maybe ByteString)
--     , riiProductCodes :: !(Maybe ProductCodesSet)
--     , riiInstance :: !ByteString
--     , riiLaunchTime :: !DateTime
--     , riiPlacement :: !(Maybe PlacementResponse)
--     , riiKernelId :: !(Maybe ByteString)
--     , riiRamdiskId :: !(Maybe ByteString)
--     , riiPlatform :: !(Maybe ByteString)
--     , riiMonitoring :: !(Maybe InstanceMonitoringState)
--     , riiSubnetId :: !(Maybe ByteString)
--     , riiVpcId :: !(Maybe ByteString)
--     , riiPrivateIpAddress :: !(Maybe ByteString)
--     , riiIpAddress :: !(Maybe ByteString)
--     , riiSourceDestCheck :: !(Maybe Boolean)
--     , riiGroupSet :: !GroupSet
--     , riiStateReason :: !(Maybe StateReason)
--     , riiArchitecture :: !(Maybe ByteString)
--     , riiRootDevice :: !(Maybe ByteString)
--     , riiRootDeviceName :: !(Maybe ByteString)
--     , riiBlockDeviceMapping :: !(Maybe InstanceBlockDeviceMappingResponse)
--     , riiInstanceLifecycle :: !(Maybe ByteString)
--     , riiSpotInstanceRequestId :: !(Maybe ByteString)
--     , riiLicense :: !(Maybe InstanceLicenseResponse)
--     , riiVirtualization :: !(Maybe ByteString)
--     , riiClientToken :: !(Maybe ByteString)
--     , riiTagSet :: !(Maybe ResourceTagSet)
--     , riiHypervisor :: !(Maybe ByteString)
--     , riiNetworkInterfaceSet :: !(Maybe InstanceNetworkInterfaceSet)
--     , riiIamInstanceProfile :: !(Maybe IamInstanceProfileResponse)
--     , riiEbsOptimized :: !(Maybe Boolean)
--     } deriving (Show)

-- data IamInstanceProfileResponse = IamInstanceProfileResponse
--     { iiprArn :: !ByteString
--     , iiprId :: !ByteString
--     } deriving (Show)

-- data InstanceNetworkInterfaceSet = InstanceNetworkInterfaceSet
--     { inisItem :: ![InstanceNetworkInterfaceSetItem]
--     } deriving (Show)

-- data InstanceNetworkInterfaceSetItem = InstanceNetworkInterfaceSetItem
--     { inisiNetworkInterfaceId :: !ByteString
--     , inisiSubnetId :: !(Maybe ByteString)
--     , inisiVpcId :: !(Maybe ByteString)
--     , inisiDescription :: !(Maybe ByteString)
--     , inisiOwnerId :: !ByteString
--     , inisiStatus :: !ByteString
--     , inisiMacAddress :: !(Maybe ByteString)
--     , inisiPrivateIpAddress :: !(Maybe ByteString)
--     , inisiPrivateDnsName :: !(Maybe ByteString)
--     , inisiSourceDestCheck :: !(Maybe Boolean)
--     , inisiGroupSet :: !(Maybe GroupSet)
--     , inisiAttachment :: !InstanceNetworkInterfaceAttachment
--     , inisiAssociation :: !(Maybe InstanceNetworkInterfaceAssociation)
--     , inisiPrivateIpAddressesSet :: !(Maybe InstancePrivateIpAddressesSet)
--     } deriving (Show)

-- data InstancePrivateIpAddressesSet = InstancePrivateIpAddressesSet
--     { ipiasItem :: ![InstancePrivateIpAddressesSetItem]
--     } deriving (Show)

-- data InstancePrivateIpAddressesSetItem = InstancePrivateIpAddressesSetItem
--     { ipiasiPrivateIpAddress :: !(Maybe ByteString)
--     , ipiasiPrivateDnsName :: !(Maybe ByteString)
--     , ipiasiPrimary :: !(Maybe Boolean)
--     , ipiasiAssociation :: !(Maybe InstanceNetworkInterfaceAssociation)
--     } deriving (Show)

-- data InstanceNetworkInterfaceAttachment = InstanceNetworkInterfaceAttachment
--     { iniaAttachmentId :: !ByteString
--     , iniaDeviceIndex :: !Int
--     , iniaStatus :: !ByteString
--     , iniaAttachTime :: !DateTime
--     , iniaDeleteOnTermination :: !Boolean
--     } deriving (Show)

-- data InstanceNetworkInterfaceAssociation = InstanceNetworkInterfaceAssociation
--     { iniaPublicIp :: !ByteString
--     , iniaPublicDnsName :: !(Maybe ByteString)
--     , iniaIpOwnerId :: !(Maybe ByteString)
--     } deriving (Show)

-- data PlacementResponse = PlacementResponse
--     { prAvailabilityZone :: !ByteString
--     , prGroupName :: !(Maybe ByteString)
--     , prTenancy :: !(Maybe ByteString)
--     } deriving (Show)

-- data StateReason = StateReason
--     { srCode :: !ByteString
--     , srMessage :: !ByteString
--     } deriving (Show)

-- data InstanceBlockDeviceMappingResponse = InstanceBlockDeviceMappingResponse
--     { ibdmrItem :: ![InstanceBlockDeviceMappingResponseItem]
--     } deriving (Show)

-- data InstanceBlockDeviceMappingResponseItem = InstanceBlockDeviceMappingResponseItem
--     { ibdmriDeviceName :: !ByteString
--   -- <xs:choice>
--     , ibdmriEbs :: !EbsInstanceBlockDeviceMappingResponse
--   -- </xs:choice>
--     } deriving (Show)

-- data EbsInstanceBlockDeviceMappingResponse = EbsInstanceBlockDeviceMappingResponse
--     { eibdmrVolumeId :: !ByteString
--     , eibdmrStatus :: !ByteString
--     , eibdmrAttachTime :: !DateTime
--     , eibdmrDeleteOnTermination :: !(Maybe Boolean)
--     } deriving (Show)

-- data InstanceLicenseResponse = InstanceLicenseResponse
--     { ilrPool :: !ByteString
--     } deriving (Show)

-- data DescribeAccountAttributes = DescribeAccountAttributes
--     { daaAccountAttributeNameSet :: !(Maybe AccountAttributeNameSet)
--     , daaFilterSet :: !(Maybe FilterSet)
--     } deriving (Show)

-- data DescribeAccountAttributesResponse = DescribeAccountAttributesResponse
--     { daarRequestId :: !ByteString
--     , daarAccountAttributeSet :: !(Maybe AccountAttributeSet)
--     } deriving (Show)

-- data AccountAttributeNameSet = AccountAttributeNameSet
--     { aansItem :: ![AccountAttributeNameSetItem]
--     } deriving (Show)

-- data AccountAttributeNameSetItem = AccountAttributeNameSetItem
--     { aansiAttributeName :: !ByteString
--     } deriving (Show)

-- data AccountAttributeSet = AccountAttributeSet
--     { aasItem :: ![AccountAttributeSetItem]
--     } deriving (Show)

-- data AccountAttributeSetItem = AccountAttributeSetItem
--     { aasiAttributeName :: !ByteString
--     , aasiAttributeValueSet :: !AccountAttributeValueSet
--     } deriving (Show)

-- data AccountAttributeValueSet = AccountAttributeValueSet
--     { aavsItem :: ![AccountAttributeValueSetItem]
--     } deriving (Show)

-- data AccountAttributeValueSetItem = AccountAttributeValueSetItem
--     { aavsiAttributeValue :: !ByteString
--     } deriving (Show)

-- data DescribeVpcAttribute = DescribeVpcAttribute
--     { dvaVpcId :: !ByteString
--     , dvaDescribeVpcAttributesGroup :: !DescribeVpcAttributesGroup
--     } deriving (Show)

-- data DescribeVpcAttributesGroup = DescribeVpcAttributesGroup
--     { dvagEnableDnsSupport :: !EmptyElement
--     , dvagEnableDnsHostnames :: !EmptyElement
--     } deriving (Show)
-- data DescribeVpcAttributeResponse = DescribeVpcAttributeResponse
--     { dvarRequestId :: !ByteString
--     , dvarVpcId :: !ByteString
--   -- <xs:choice>
--     , dvarEnableDnsSupport :: !(Maybe AttributeBooleanValue)
--     , dvarEnableDnsHostnames :: !(Maybe AttributeBooleanValue)
--   -- </xs:choice>
--     } deriving (Show)

-- data ModifyVpcAttribute = ModifyVpcAttribute
--     { mvaVpcId :: !ByteString
--   -- <xs:choice>
--     , mvaEnableDnsSupport :: !(Maybe AttributeBooleanValue)
--     , mvaEnableDnsHostnames :: !(Maybe AttributeBooleanValue)
--   -- </xs:choice>
--     } deriving (Show)

-- data ModifyVpcAttributeResponse = ModifyVpcAttributeResponse
--     { mvarRequestId :: !ByteString
--     , mvarReturn :: !Boolean
--     } deriving (Show)

-- data GetConsoleOutput = GetConsoleOutput
--     { gcoInstanceId :: !ByteString
--     } deriving (Show)

-- data GetConsoleOutputResponse = GetConsoleOutputResponse
--     { gcorRequestId :: !ByteString
--     , gcorInstanceId :: !ByteString
--     , gcorTimestamp :: !DateTime
--     , gcorOutput :: !ByteString
--     } deriving (Show)

-- data GetPasswordData = GetPasswordData
--     { gpdInstanceId :: !ByteString
--     } deriving (Show)

-- data GetPasswordDataResponse = GetPasswordDataResponse
--     { gpdrRequestId :: !ByteString
--     , gpdrInstanceId :: !ByteString
--     , gpdrTimestamp :: !DateTime
--     , gpdrPasswordData :: !ByteString
--     } deriving (Show)

-- data InstanceId = InstanceId
--     { iiInstanceId :: !ByteString
--     } deriving (Show)

-- data InstanceIdSet = InstanceIdSet
--     { iisItem :: ![InstanceId]
--     } deriving (Show)

-- data InstanceStateChange = InstanceStateChange
--     { iscInstanceId :: !ByteString
--     , iscCurrentState :: !InstanceState
--     , iscPreviousState :: !InstanceState
--     } deriving (Show)

-- data InstanceStateChangeSet = InstanceStateChangeSet
--     { iscsItem :: ![InstanceStateChange]
--     } deriving (Show)

-- data TerminateInstances = TerminateInstances
--     { tiInstancesSet :: !InstanceIdSet
--     } deriving (Show)

-- data TerminateInstancesResponse = TerminateInstancesResponse
--     { tirRequestId :: !ByteString
--     , tirInstancesSet :: !InstanceStateChangeSet
--     } deriving (Show)

-- data InstanceBlockDeviceMapping = InstanceBlockDeviceMapping
--     { ibdmItem :: ![InstanceBlockDeviceMappingItem]
--     } deriving (Show)

-- data InstanceBlockDeviceMappingItem = InstanceBlockDeviceMappingItem
--     { ibdmiDeviceName :: !ByteString
--   -- <xs:choice>
--     , ibdmiVirtualName :: !(Maybe ByteString)
--     , ibdmiEbs :: !(Maybe InstanceEbsBlockDevice)
--     , ibdmiNoDevice :: !(Maybe EmptyElement)
--   -- </xs:choice>
--     } deriving (Show)

-- data InstanceEbsBlockDevice = InstanceEbsBlockDevice
--     { iebdVolumeId :: !ByteString
--     , iebdDeleteOnTermination :: !(Maybe Boolean)
--     } deriving (Show)

-- data StopInstances = StopInstances
--     { siInstancesSet :: !InstanceIdSet
--     , siForce :: !(Maybe Boolean)
--     } deriving (Show)

-- data StopInstancesResponse = StopInstancesResponse
--     { sirRequestId :: !ByteString
--     , sirInstancesSet :: !InstanceStateChangeSet
--     } deriving (Show)

-- data StartInstances = StartInstances
--     { siInstancesSet :: !InstanceIdSet
--     } deriving (Show)

-- data StartInstancesResponse = StartInstancesResponse
--     { sirRequestId :: !ByteString
--     , sirInstancesSet :: !InstanceStateChangeSet
--     } deriving (Show)

-- data RebootInstances = RebootInstances
--     { riInstancesSet :: !RebootInstancesInfo
--     } deriving (Show)

-- data RebootInstancesInfo = RebootInstancesInfo
--     { riiItem :: !(NonEmpty RebootInstancesItem)
--     } deriving (Show)

-- data RebootInstancesItem = RebootInstancesItem
--     { riiInstanceId :: !ByteString
--     } deriving (Show)

-- data RebootInstancesResponse = RebootInstancesResponse
--     { rirRequestId :: !ByteString
--     , rirReturn :: !Boolean
--     } deriving (Show)

-- data DescribeInstances = DescribeInstances
--     { diInstancesSet :: !DescribeInstancesInfo
--     , diFilterSet :: !(Maybe FilterSet)
--     } deriving (Show)

-- data DescribeInstancesInfo = DescribeInstancesInfo
--     { diiItem :: ![DescribeInstancesItem]
--     } deriving (Show)

-- data DescribeInstancesItem = DescribeInstancesItem
--     { diiInstanceId :: !ByteString
--     } deriving (Show)

-- data DescribeInstancesResponse = DescribeInstancesResponse
--     { dirRequestId :: !ByteString
--     , dirReservationSet :: !ReservationSet
--     } deriving (Show)

-- data ReservationSet = ReservationSet
--     { rsItem :: ![ReservationInfo]
--     } deriving (Show)

-- data UnavailableResultSet = UnavailableResultSet
--     { ursItem :: ![UnavailableResult]
--     } deriving (Show)

-- data UnavailableResult = UnavailableResult
--     { urAvailabilityZone :: !ByteString
--     } deriving (Show)

-- data DescribeImages = DescribeImages
--     { diExecutableBySet :: !(Maybe DescribeImagesExecutableBySet)
--     , diImagesSet :: !DescribeImagesInfo
--     , diOwnersSet :: !(Maybe DescribeImagesOwners)
--     , diFilterSet :: !(Maybe FilterSet)
--     } deriving (Show)

-- data DescribeImagesInfo = DescribeImagesInfo
--     { diiItem :: ![DescribeImagesItem]
--     } deriving (Show)

-- data DescribeImagesItem = DescribeImagesItem
--     { diiImageId :: !ByteString
--     } deriving (Show)

-- data DescribeImagesOwners = DescribeImagesOwners
--     { dioItem :: ![DescribeImagesOwner]
--     } deriving (Show)

-- data DescribeImagesOwner = DescribeImagesOwner
--     { dioOwner :: !ByteString
--     } deriving (Show)

-- data DescribeImagesExecutableBySet = DescribeImagesExecutableBySet
--     { diebsItem :: ![DescribeImagesExecutableBy]
--     } deriving (Show)

-- data DescribeImagesExecutableBy = DescribeImagesExecutableBy
--     { diebUser :: !ByteString
--     } deriving (Show)

-- data DescribeImagesResponse = DescribeImagesResponse
--     { dirRequestId :: !ByteString
--     , dirImagesSet :: !DescribeImagesResponseInfo
--     } deriving (Show)

-- data DescribeImagesResponseInfo = DescribeImagesResponseInfo
--     { diriItem :: ![DescribeImagesResponseItem]
--     } deriving (Show)

-- data DescribeImagesResponseItem = DescribeImagesResponseItem
--     { diriImageId :: !ByteString
--     , diriImageLocation :: !(Maybe ByteString)
--     , diriImageState :: !ByteString
--     , diriImageOwnerId :: !ByteString
--     , diriIsPublic :: !Boolean
--     , diriProductCodes :: !(Maybe ProductCodesSet)
--     , diriArchitecture :: !(Maybe ByteString)
--     , diriImage :: !(Maybe ByteString)
--     , diriKernelId :: !(Maybe ByteString)
--     , diriRamdiskId :: !(Maybe ByteString)
--     , diriPlatform :: !(Maybe ByteString)
--     , diriStateReason :: !(Maybe StateReason)
--     , diriImageOwnerAlias :: !(Maybe ByteString)
--     , diriName :: !(Maybe ByteString)
--     , diriDescription :: !(Maybe ByteString)
--     , diriRootDevice :: !(Maybe ByteString)
--     , diriRootDeviceName :: !(Maybe ByteString)
--     , diriBlockDeviceMapping :: !(Maybe BlockDeviceMapping)
--     , diriVirtualization :: !(Maybe ByteString)
--     , diriTagSet :: !(Maybe ResourceTagSet)
--     , diriHypervisor :: !(Maybe ByteString)
--     } deriving (Show)

-- data CreateSecurityGroup = CreateSecurityGroup
--     { csgGroupName :: !ByteString
--     , csgGroupDescription :: !ByteString
--     , csgVpcId :: !(Maybe ByteString)
--     } deriving (Show)

-- data CreateSecurityGroupResponse = CreateSecurityGroupResponse
--     { csgrRequestId :: !ByteString
--     , csgrReturn :: !Boolean
--     , csgrGroupId :: !ByteString
--     } deriving (Show)

-- data DeleteSecurityGroup = DeleteSecurityGroup
--     {
-- -- <xs:choice>
--       dsgGroupId :: !(Maybe ByteString)
--     , dsgGroupName :: !(Maybe ByteString)
-- -- </xs:choice>
--     } deriving (Show)

-- data DeleteSecurityGroupResponse = DeleteSecurityGroupResponse
--     { dsgrRequestId :: !ByteString
--     , dsgrReturn :: !Boolean
--     } deriving (Show)

-- data DescribeSecurityGroups = DescribeSecurityGroups
--     { dsgSecurityGroupSet :: !DescribeSecurityGroupsSet
--     , dsgSecurityGroupIdSet :: !(Maybe DescribeSecurityGroupsIdSet)
--     , dsgFilterSet :: !(Maybe FilterSet)
--     } deriving (Show)

-- data DescribeSecurityGroupsSet = DescribeSecurityGroupsSet
--     { dsgsItem :: ![DescribeSecurityGroupsSetItem]
--     } deriving (Show)

-- data DescribeSecurityGroupsSetItem = DescribeSecurityGroupsSetItem
--     { dsgsiGroupName :: !ByteString
--     } deriving (Show)

-- data DescribeSecurityGroupsIdSet = DescribeSecurityGroupsIdSet
--     { dsgisItem :: ![DescribeSecurityGroupsIdSetItem]
--     } deriving (Show)

-- data DescribeSecurityGroupsIdSetItem = DescribeSecurityGroupsIdSetItem
--     { dsgisiGroupId :: !ByteString
--     } deriving (Show)

-- data DescribeSecurityGroupsResponse = DescribeSecurityGroupsResponse
--     { dsgrRequestId :: !ByteString
--     , dsgrSecurityGroupInfo :: !SecurityGroupSet
--     } deriving (Show)

-- data IpPermissionSet = IpPermissionSet
--     { ipsItem :: ![IpPermission]
--     } deriving (Show)

-- data IpPermission = IpPermission
--     { ipIpProtocol :: !ByteString
--     , ipFromPort :: !(Maybe Int)
--     , ipToPort :: !(Maybe Int)
--     , ipGroups :: !UserIdGroupPairSet
--     , ipIpRanges :: !IpRangeSet
--     } deriving (Show)

-- data IpRangeSet = IpRangeSet
--     { irsItem :: ![IpRangeItem]
--     } deriving (Show)

-- data IpRangeItem = IpRangeItem
--     { iriCidrIp :: !ByteString
--     } deriving (Show)

-- data UserIdGroupPairSet = UserIdGroupPairSet
--     { uigpsItem :: ![UserIdGroupPair]
--     } deriving (Show)

-- data UserIdGroupPair = UserIdGroupPair
--     { uigpUserId :: !(Maybe ByteString)
--     , uigpGroupId :: !(Maybe ByteString)
--     , uigpGroupName :: !(Maybe ByteString)
--     } deriving (Show)

-- data SecurityGroupSet = SecurityGroupSet
--     { sgsItem :: ![SecurityGroupItem]
--     } deriving (Show)

-- data SecurityGroupItem = SecurityGroupItem
--     { sgiOwnerId :: !ByteString
--     , sgiGroupId :: !ByteString
--     , sgiGroupName :: !ByteString
--     , sgiGroupDescription :: !ByteString
--     , sgiVpcId :: !(Maybe ByteString)
--     , sgiIpPermissions :: !IpPermissionSet
--     , sgiIpPermissionsEgress :: !(Maybe IpPermissionSet)
--     , sgiTagSet :: !(Maybe ResourceTagSet)
--     } deriving (Show)

-- data AuthorizeSecurityGroupIngress = AuthorizeSecurityGroupIngress
--     { asgiUserId :: !(Maybe ByteString)
--   -- <xs:choice>
--     , asgiGroupId :: !(Maybe ByteString)
--     , asgiGroupName :: !(Maybe ByteString)
--   -- </xs:choice>
--     , asgiIpPermissions :: !IpPermissionSet
--     } deriving (Show)

-- data AuthorizeSecurityGroupIngressResponse = AuthorizeSecurityGroupIngressResponse
--     { asgirRequestId :: !ByteString
--     , asgirReturn :: !Boolean
--     } deriving (Show)

-- data RevokeSecurityGroupIngress = RevokeSecurityGroupIngress
--     { rsgiUserId :: !(Maybe ByteString)
--   -- <xs:choice>
--     , rsgiGroupId :: !(Maybe ByteString)
--     , rsgiGroupName :: !(Maybe ByteString)
--   -- </xs:choice>
--     , rsgiIpPermissions :: !IpPermissionSet
--     } deriving (Show)

-- data RevokeSecurityGroupIngressResponse = RevokeSecurityGroupIngressResponse
--     { rsgirRequestId :: !ByteString
--     , rsgirReturn :: !Boolean
--     } deriving (Show)

-- data AuthorizeSecurityGroupEgress = AuthorizeSecurityGroupEgress
--     { asgeGroupId :: !ByteString
--     , asgeIpPermissions :: !IpPermissionSet
--     } deriving (Show)

-- data AuthorizeSecurityGroupEgressResponse = AuthorizeSecurityGroupEgressResponse
--     { asgerRequestId :: !ByteString
--     , asgerReturn :: !Boolean
--     } deriving (Show)

-- data RevokeSecurityGroupEgress = RevokeSecurityGroupEgress
--     { rsgeGroupId :: !ByteString
--     , rsgeIpPermissions :: !IpPermissionSet
--     } deriving (Show)

-- data RevokeSecurityGroupEgressResponse = RevokeSecurityGroupEgressResponse
--     { rsgerRequestId :: !ByteString
--     , rsgerReturn :: !Boolean
--     } deriving (Show)

-- data InstanceState = InstanceState
--     { isCode :: !Int
--     , isName :: !ByteString
--     } deriving (Show)

-- data ModifyInstanceAttribute = ModifyInstanceAttribute
--     { miaInstanceId :: !ByteString
--   -- <xs:choice>
--     , miaInstance :: !(Maybe AttributeValue)
--     , miaKernel :: !(Maybe AttributeValue)
--     , miaRamdisk :: !(Maybe AttributeValue)
--     , miaUserData :: !(Maybe AttributeValue)
--     , miaDisableApiTermination :: !(Maybe AttributeBooleanValue)
--     , miaInstanceInitiatedShutdownBehavior :: !(Maybe AttributeValue)
--     , miaBlockDeviceMapping :: !(Maybe InstanceBlockDeviceMapping)
--     , miaSourceDestCheck :: !(Maybe AttributeBooleanValue)
--     , miaGroupSet :: !(Maybe SecurityGroupIdSet)
--     , miaEbsOptimized :: !(Maybe AttributeBooleanValue)
--   -- </xs:choice>
--     } deriving (Show)

-- data SecurityGroupIdSet = SecurityGroupIdSet
--     { sgisItem :: ![SecurityGroupIdSetItem]
--     } deriving (Show)

-- data SecurityGroupIdSetItem = SecurityGroupIdSetItem
--     { sgisiGroupId :: !ByteString
--     } deriving (Show)

-- data ModifyInstanceAttributeResponse = ModifyInstanceAttributeResponse
--     { miarRequestId :: !ByteString
--     , miarReturn :: !Boolean
--     } deriving (Show)

-- data ResetInstanceAttribute = ResetInstanceAttribute
--     { riaInstanceId :: !ByteString
--     , riaResetInstanceAttributesGroup :: !ResetInstanceAttributesGroup
--     } deriving (Show)

-- data ResetInstanceAttributesGroup = ResetInstanceAttributesGroup
--     { riagKernel :: !EmptyElement
--     , riagRamdisk :: !EmptyElement
--     , riagSourceDestCheck :: !EmptyElement
--     } deriving (Show)
-- data ResetInstanceAttributeResponse = ResetInstanceAttributeResponse
--     { riarRequestId :: !ByteString
--     , riarReturn :: !Boolean
--     } deriving (Show)

-- data DescribeInstanceAttribute = DescribeInstanceAttribute
--     { diaInstanceId :: !ByteString
--     , diaDescribeInstanceAttributesGroup :: !DescribeInstanceAttributesGroup
--     } deriving (Show)

-- data DescribeInstanceAttributesGroup = DescribeInstanceAttributesGroup
--     { diagInstance :: !EmptyElement
--     , diagKernel :: !EmptyElement
--     , diagRamdisk :: !EmptyElement
--     , diagUserData :: !EmptyElement
--     , diagDisableApiTermination :: !EmptyElement
--     , diagInstanceInitiatedShutdownBehavior :: !EmptyElement
--     , diagRootDeviceName :: !EmptyElement
--     , diagBlockDeviceMapping :: !EmptyElement
--     , diagSourceDestCheck :: !EmptyElement
--     , diagGroupSet :: !EmptyElement
--     , diagProductCodes :: !EmptyElement
--     , diagEbsOptimized :: !EmptyElement
--     } deriving (Show)
-- data DescribeInstanceAttributeResponse = DescribeInstanceAttributeResponse
--     { diarRequestId :: !ByteString
--     , diarInstanceId :: !ByteString
--   -- <xs:choice>
--     , diarInstance :: !(Maybe NullableAttributeValue)
--     , diarKernel :: !(Maybe NullableAttributeValue)
--     , diarRamdisk :: !(Maybe NullableAttributeValue)
--     , diarUserData :: !(Maybe NullableAttributeValue)
--     , diarDisableApiTermination :: !(Maybe NullableAttributeBooleanValue)
--     , diarInstanceInitiatedShutdownBehavior :: !(Maybe NullableAttributeValue)
--     , diarRootDeviceName :: !(Maybe NullableAttributeValue)
--     , diarBlockDeviceMapping :: !(Maybe InstanceBlockDeviceMappingResponse)
--     , diarSourceDestCheck :: !(Maybe NullableAttributeBooleanValue)
--     , diarGroupSet :: !(Maybe GroupSet)
--     , diarProductCodes :: !(Maybe ProductCodesSet)
--     , diarEbsOptimized :: !(Maybe NullableAttributeBooleanValue)
--   -- </xs:choice>
--     } deriving (Show)

-- data ModifyImageAttribute = ModifyImageAttribute
--     { miaImageId :: !ByteString
--   -- <xs:choice>
--     , miaLaunchPermission :: !(Maybe LaunchPermissionOperation)
--     , miaProductCodes :: !(Maybe ProductCodeList)
--     , miaDescription :: !(Maybe AttributeValue)
--   -- </xs:choice>
--     } deriving (Show)

-- data LaunchPermissionOperation = LaunchPermissionOperation
--     { lpo 
-- -- <xs:choice>
--     , lpoAdd :: !(Maybe LaunchPermissionList)
--     , lpoRemove :: !(Maybe LaunchPermissionList)
-- -- </xs:choice>
--     } deriving (Show)

-- data LaunchPermissionList = LaunchPermissionList
--     { lplItem :: ![LaunchPermissionItem]
--     } deriving (Show)

-- data LaunchPermissionItem = LaunchPermissionItem
--     { lpi 
-- -- <xs:choice>
--     , lpiUserId :: !(Maybe ByteString)
--     , lpiGroup :: !(Maybe ByteString)
-- -- </xs:choice>
--     } deriving (Show)

-- data ProductCodeList = ProductCodeList
--     { pclItem :: ![ProductCodeItem]
--     } deriving (Show)

-- data ProductCodeItem = ProductCodeItem
--     { pciProductCode :: !ByteString
--     } deriving (Show)

-- data ModifyImageAttributeResponse = ModifyImageAttributeResponse
--     { miarRequestId :: !ByteString
--     , miarReturn :: !Boolean
--     } deriving (Show)

-- data ResetImageAttribute = ResetImageAttribute
--     { riaImageId :: !ByteString
--     , riaResetImageAttributesGroup :: !ResetImageAttributesGroup
--     } deriving (Show)

-- data ResetImageAttributesGroup = ResetImageAttributesGroup
--     { riagLaunchPermission :: !EmptyElement
--     } deriving (Show)

-- data EmptyElement = EmptyElement

-- data ResetImageAttributeResponse = ResetImageAttributeResponse
--     { riarRequestId :: !ByteString
--     , riarReturn :: !Boolean
--     } deriving (Show)

-- data DescribeImageAttribute = DescribeImageAttribute
--     { diaImageId :: !ByteString
--     , diaDescribeImageAttributesGroup :: !DescribeImageAttributesGroup
--     } deriving (Show)

-- data DescribeImageAttributesGroup = DescribeImageAttributesGroup
--     { diagLaunchPermission :: !EmptyElement
--     , diagProductCodes :: !EmptyElement
--     , diagKernel :: !EmptyElement
--     , diagRamdisk :: !EmptyElement
--     , diagBlockDeviceMapping :: !EmptyElement
--     , diagDescription :: !EmptyElement
--     , diagInstanceCategory :: !EmptyElement
--     } deriving (Show)
-- data DescribeImageAttributeResponse = DescribeImageAttributeResponse
--     { diarRequestId :: !ByteString
--     , diarImageId :: !ByteString
--   -- <xs:choice>
--     , diarLaunchPermission :: !(Maybe LaunchPermissionList)
--     , diarProductCodes :: !(Maybe ProductCodesSet)
--     , diarKernel :: !(Maybe NullableAttributeValue)
--     , diarRamdisk :: !(Maybe NullableAttributeValue)
--     , diarDescription :: !(Maybe NullableAttributeValue)
--     , diarBlockDeviceMapping :: !(Maybe BlockDeviceMapping)
--   -- </xs:choice>
--     } deriving (Show)

-- data NullableAttributeValue = NullableAttributeValue
--     { navValue :: !(Maybe ByteString)
--     } deriving (Show)

-- data NullableAttributeBooleanValue = NullableAttributeBooleanValue
--     { nabvValue :: !(Maybe Boolean)
--     } deriving (Show)

-- data AttributeValue = AttributeValue
--     { avValue :: !ByteString
--     } deriving (Show)

-- data AttributeBooleanValue = AttributeBooleanValue
--     { abvValue :: !Boolean
--     } deriving (Show)

-- data ConfirmProductInstance = ConfirmProductInstance
--     { cpiProductCode :: !ByteString
--     , cpiInstanceId :: !ByteString
--     } deriving (Show)

-- data ProductCodesSet = ProductCodesSet
--     { pcsItem :: ![ProductCodesSetItem]
--     } deriving (Show)

-- data ProductCodesSetItem = ProductCodesSetItem
--     { pcsiProductCode :: !ByteString
--     , pcsiType :: !ByteString
--     } deriving (Show)

-- data ConfirmProductInstanceResponse = ConfirmProductInstanceResponse
--     { cpirRequestId :: !ByteString
--     , cpirReturn :: !Boolean
--     , cpirOwnerId :: !(Maybe ByteString)
--     } deriving (Show)

-- data DescribeAvailabilityZones = DescribeAvailabilityZones
--     { dazAvailabilityZoneSet :: !DescribeAvailabilityZonesSet
--     , dazFilterSet :: !(Maybe FilterSet)
--     } deriving (Show)

-- data DescribeAvailabilityZonesSet = DescribeAvailabilityZonesSet
--     { dazsItem :: ![DescribeAvailabilityZonesSetItem]
--     } deriving (Show)

-- data DescribeAvailabilityZonesSetItem = DescribeAvailabilityZonesSetItem
--     { dazsiZoneName :: !ByteString
--     } deriving (Show)

-- data DescribeAvailabilityZonesResponse = DescribeAvailabilityZonesResponse
--     { dazrRequestId :: !ByteString
--     , dazrAvailabilityZoneInfo :: !AvailabilityZoneSet
--     } deriving (Show)

-- data AvailabilityZoneSet = AvailabilityZoneSet
--     { azsItem :: ![AvailabilityZoneItem]
--     } deriving (Show)

-- data AvailabilityZoneMessage = AvailabilityZoneMessage
--     { azmMessage :: !ByteString
--     } deriving (Show)

-- data AvailabilityZoneMessageSet = AvailabilityZoneMessageSet
--     { azmsItem :: ![AvailabilityZoneMessage]
--     } deriving (Show)

-- data AvailabilityZoneItem = AvailabilityZoneItem
--     { aziZoneName :: !ByteString
--     , aziZoneState :: !ByteString
--     , aziRegionName :: !ByteString
--     , aziMessageSet :: !AvailabilityZoneMessageSet
--     } deriving (Show)

-- data ReleaseAddress = ReleaseAddress
--     { ra 
--   -- <xs:choice>
--     , raPublicIp :: !(Maybe ByteString)
--     , raAllocationId :: !(Maybe ByteString)
--   -- </xs:choice>
--     } deriving (Show)

-- data ReleaseAddressResponse = ReleaseAddressResponse
--     { rarRequestId :: !ByteString
--     , rarReturn :: !Boolean
--     } deriving (Show)

-- data DescribeAddresses = DescribeAddresses
--     { daPublicIpsSet :: !DescribeAddressesInfo
--     , daAllocationIdsSet :: !AllocationIdSet
--     , daFilterSet :: !(Maybe FilterSet)
--     } deriving (Show)

-- data AllocationIdSet = AllocationIdSet
--     { aisItem :: ![AllocationIdSetItem]
--     } deriving (Show)

-- data AllocationIdSetItem = AllocationIdSetItem
--     { aisiAllocationId :: !ByteString
--     } deriving (Show)

-- data DescribeAddressesInfo = DescribeAddressesInfo
--     { daiItem :: ![DescribeAddressesItem]
--     } deriving (Show)

-- data DescribeAddressesItem = DescribeAddressesItem
--     { daiPublicIp :: !ByteString
--     } deriving (Show)

-- data DescribeAddressesResponse = DescribeAddressesResponse
--     { darRequestId :: !ByteString
--     , darAddressesSet :: !DescribeAddressesResponseInfo
--     } deriving (Show)

-- data DescribeAddressesResponseInfo = DescribeAddressesResponseInfo
--     { dariItem :: ![DescribeAddressesResponseItem]
--     } deriving (Show)

-- data DescribeAddressesResponseItem = DescribeAddressesResponseItem
--     { dariPublicIp :: !ByteString
--     , dariAllocationId :: !(Maybe ByteString)
--     , dariDomain :: !ByteString
--     , dariInstanceId :: !(Maybe ByteString)
--     , dariAssociationId :: !(Maybe ByteString)
--     , dariNetworkInterfaceId :: !(Maybe ByteString)
--     , dariNetworkInterfaceOwnerId :: !(Maybe ByteString)
--     , dariPrivateIpAddress :: !(Maybe ByteString)
--     } deriving (Show)

-- data AssociateAddress = AssociateAddress
--     { aa 
--   -- <xs:choice>
--     , aaPublicIp :: !(Maybe ByteString)
--     , aaAllocationId :: !(Maybe ByteString)
--   -- </xs:choice>
--   -- <xs:choice>
--     , aaNetworkInterfaceId :: !(Maybe ByteString)
--     , aaInstanceId :: !(Maybe ByteString)
--   -- </xs:choice>
--     , aaPrivateIpAddress :: !(Maybe ByteString)
--     , aaAllowReassociation :: !(Maybe Boolean)
--     } deriving (Show)

-- data AssociateAddressResponse = AssociateAddressResponse
--     { aarRequestId :: !ByteString
--     , aarReturn :: !Boolean
--     , aarAssociationId :: !(Maybe ByteString)
--     } deriving (Show)

-- data DisassociateAddress = DisassociateAddress
--     { da 
-- -- <xs:choice>
--     , daPublicIp :: !(Maybe ByteString)
--     , daAssociationId :: !(Maybe ByteString)
-- -- </xs:choice>
--      } deriving (Show)

-- data DisassociateAddressResponse = DisassociateAddressResponse
--     { darRequestId :: !ByteString
--     , darReturn :: !Boolean
--     } deriving (Show)

-- data CreateVolume = CreateVolume
--     { cvSize :: !(Maybe ByteString)
--     , cvSnapshotId :: !(Maybe ByteString)
--     , cvAvailabilityZone :: !ByteString
--     , cvVolume :: !(Maybe ByteString)
--     , cvIops :: !(Maybe Int)
--     } deriving (Show)

-- data CreateVolumeResponse = CreateVolumeResponse
--     { cvrRequestId :: !ByteString
--     , cvrVolumeId :: !ByteString
--     , cvrSize :: !ByteString
--     , cvrSnapshotId :: !ByteString
--     , cvrAvailabilityZone :: !ByteString
--     , cvrStatus :: !ByteString
--     , cvrCreateTime :: !DateTime
--     , cvrVolume :: !ByteString
--     , cvrIops :: !(Maybe Int)
--     } deriving (Show)

-- data DeleteVolume = DeleteVolume
--     { dvVolumeId :: !ByteString
--     } deriving (Show)

-- data DeleteVolumeResponse = DeleteVolumeResponse
--     { dvrRequestId :: !ByteString
--     , dvrReturn :: !Boolean
--     } deriving (Show)

-- data DescribeVolumes = DescribeVolumes
--     { dvVolumeSet :: !DescribeVolumesSet
--     , dvFilterSet :: !(Maybe FilterSet)
--     } deriving (Show)

-- data DescribeVolumesSet = DescribeVolumesSet
--     { dvsItem :: ![DescribeVolumesSetItem]
--     } deriving (Show)

-- data DescribeVolumesSetItem = DescribeVolumesSetItem
--     { dvsiVolumeId :: !ByteString
--     } deriving (Show)

-- data DescribeVolumesResponse = DescribeVolumesResponse
--     { dvrRequestId :: !ByteString
--     , dvrVolumeSet :: !DescribeVolumesSetResponse
--     } deriving (Show)

-- data DescribeVolumesSetResponse = DescribeVolumesSetResponse
--     { dvsrItem :: ![DescribeVolumesSetItemResponse]
--     } deriving (Show)

-- data DescribeVolumesSetItemResponse = DescribeVolumesSetItemResponse
--     { dvsirVolumeId :: !ByteString
--     , dvsirSize :: !ByteString
--     , dvsirSnapshotId :: !ByteString
--     , dvsirAvailabilityZone :: !ByteString
--     , dvsirStatus :: !ByteString
--     , dvsirCreateTime :: !DateTime
--     , dvsirAttachmentSet :: !AttachmentSetResponse
--     , dvsirTagSet :: !(Maybe ResourceTagSet)
--     , dvsirVolume :: !ByteString
--     , dvsirIops :: !(Maybe Int)
--     } deriving (Show)

-- data AttachmentSetResponse = AttachmentSetResponse
--     { asrItem :: ![AttachmentSetItemResponse]
--     } deriving (Show)

-- data AttachmentSetItemResponse = AttachmentSetItemResponse
--     { asirVolumeId :: !ByteString
--     , asirInstanceId :: !ByteString
--     , asirDevice :: !ByteString
--     , asirStatus :: !ByteString
--     , asirAttachTime :: !DateTime
--     , asirDeleteOnTermination :: !Boolean
--     } deriving (Show)

-- data AttachVolume = AttachVolume
--     { avVolumeId :: !ByteString
--     , avInstanceId :: !ByteString
--     , avDevice :: !ByteString
--     } deriving (Show)

-- data AttachVolumeResponse = AttachVolumeResponse
--     { avrRequestId :: !ByteString
--     , avrVolumeId :: !ByteString
--     , avrInstanceId :: !ByteString
--     , avrDevice :: !ByteString
--     , avrStatus :: !ByteString
--     , avrAttachTime :: !DateTime
--     } deriving (Show)

-- data DetachVolume = DetachVolume
--     { dvVolumeId :: !ByteString
--     , dvInstanceId :: !(Maybe ByteString)
--     , dvDevice :: !(Maybe ByteString)
--     , dvForce :: !(Maybe Boolean)
--     } deriving (Show)

-- data DetachVolumeResponse = DetachVolumeResponse
--     { dvrRequestId :: !ByteString
--     , dvrVolumeId :: !ByteString
--     , dvrInstanceId :: !ByteString
--     , dvrDevice :: !ByteString
--     , dvrStatus :: !ByteString
--     , dvrAttachTime :: !DateTime
--     } deriving (Show)

-- data CreateSnapshot = CreateSnapshot
--     { csVolumeId :: !ByteString
--     , csDescription :: !(Maybe ByteString)
--     } deriving (Show)

-- data CreateSnapshotResponse = CreateSnapshotResponse
--     { csrRequestId :: !ByteString
--     , csrSnapshotId :: !ByteString
--     , csrVolumeId :: !ByteString
--     , csrStatus :: !ByteString
--     , csrStartTime :: !DateTime
--     , csrProgress :: !ByteString
--     , csrOwnerId :: !ByteString
--     , csrVolumeSize :: !ByteString
--     , csrDescription :: !(Maybe ByteString)
--     } deriving (Show)

-- data CopySnapshot = CopySnapshot
--     { csSourceRegion :: !ByteString
--     , csSourceSnapshotId :: !ByteString
--     , csDescription :: !(Maybe ByteString)
--     } deriving (Show)

-- data CopySnapshotResponse = CopySnapshotResponse
--     { csrRequestId :: !ByteString
--     , csrSnapshotId :: !ByteString
--     } deriving (Show)

-- data DeleteSnapshot = DeleteSnapshot
--     { dsSnapshotId :: !ByteString
--     } deriving (Show)

-- data DeleteSnapshotResponse = DeleteSnapshotResponse
--     { dsrRequestId :: !ByteString
--     , dsrReturn :: !Boolean
--     } deriving (Show)

-- data DescribeSnapshots = DescribeSnapshots
--     { dsSnapshotSet :: !DescribeSnapshotsSet
--     , dsOwnersSet :: !(Maybe DescribeSnapshotsOwners)
--     , dsRestorableBySet :: !(Maybe DescribeSnapshotsRestorableBySet)
--     , dsFilterSet :: !(Maybe FilterSet)
--     } deriving (Show)

-- data DescribeSnapshotsSet = DescribeSnapshotsSet
--     { dssItem :: ![DescribeSnapshotsSetItem]
--     } deriving (Show)

-- data DescribeSnapshotsSetItem = DescribeSnapshotsSetItem
--     { dssiSnapshotId :: !ByteString
--     } deriving (Show)

-- data DescribeSnapshotsOwners = DescribeSnapshotsOwners
--     { dsoItem :: ![DescribeSnapshotsOwner]
--     } deriving (Show)

-- data DescribeSnapshotsOwner = DescribeSnapshotsOwner
--     { dsoOwner :: !ByteString
--     } deriving (Show)

-- data DescribeSnapshotsRestorableBySet = DescribeSnapshotsRestorableBySet
--     { dsrbsItem :: ![DescribeSnapshotsRestorableBy]
--     } deriving (Show)

-- data DescribeSnapshotsRestorableBy = DescribeSnapshotsRestorableBy
--     { dsrbUser :: !ByteString
--     } deriving (Show)

-- data DescribeSnapshotsResponse = DescribeSnapshotsResponse
--     { dsrRequestId :: !ByteString
--     , dsrSnapshotSet :: !DescribeSnapshotsSetResponse
--     } deriving (Show)

-- data DescribeSnapshotsSetResponse = DescribeSnapshotsSetResponse
--     { dssrItem :: ![DescribeSnapshotsSetItemResponse]
--     } deriving (Show)

-- data DescribeSnapshotsSetItemResponse = DescribeSnapshotsSetItemResponse
--     { dssirSnapshotId :: !ByteString
--     , dssirVolumeId :: !ByteString
--     , dssirStatus :: !ByteString
--     , dssirStartTime :: !DateTime
--     , dssirProgress :: !ByteString
--     , dssirOwnerId :: !ByteString
--     , dssirVolumeSize :: !ByteString
--     , dssirDescription :: !(Maybe ByteString)
--     , dssirOwnerAlias :: !(Maybe ByteString)
--     , dssirTagSet :: !(Maybe ResourceTagSet)
--     } deriving (Show)

-- data ModifySnapshotAttribute = ModifySnapshotAttribute
--     { msaSnapshotId :: !ByteString
--     , msaCreateVolumePermission :: !CreateVolumePermissionOperation
--     } deriving (Show)

-- data CreateVolumePermissionOperation = CreateVolumePermissionOperation
--     { cvpo 
-- -- <xs:choice>
--     , cvpoAdd :: !(Maybe CreateVolumePermissionList)
--     , cvpoRemove :: !(Maybe CreateVolumePermissionList)
-- -- </xs:choice>
--     } deriving (Show)

-- data CreateVolumePermissionList = CreateVolumePermissionList
--     { cvplItem :: ![CreateVolumePermissionItem]
--     } deriving (Show)

-- data CreateVolumePermissionItem = CreateVolumePermissionItem
--     { cvpi 
-- -- <xs:choice>
--     , cvpiUserId :: !(Maybe ByteString)
--     , cvpiGroup :: !(Maybe ByteString)
-- -- </xs:choice>
--     } deriving (Show)

-- data ModifySnapshotAttributeResponse = ModifySnapshotAttributeResponse
--     { msarRequestId :: !ByteString
--     , msarReturn :: !Boolean
--     } deriving (Show)

-- data ResetSnapshotAttribute = ResetSnapshotAttribute
--     { rsaSnapshotId :: !ByteString
--     , rsaResetSnapshotAttributesGroup :: !ResetSnapshotAttributesGroup
--     } deriving (Show)

-- data ResetSnapshotAttributesGroup = ResetSnapshotAttributesGroup
--     { rsagCreateVolumePermission :: !EmptyElement
--     } deriving (Show)
-- data ResetSnapshotAttributeResponse = ResetSnapshotAttributeResponse
--     { rsarRequestId :: !ByteString
--     , rsarReturn :: !Boolean
--     } deriving (Show)

-- data DescribeSnapshotAttribute = DescribeSnapshotAttribute
--     { dsaSnapshotId :: !ByteString
--     , dsaDescribeSnapshotAttributesGroup :: !DescribeSnapshotAttributesGroup
--     } deriving (Show)

-- data DescribeSnapshotAttributesGroup = DescribeSnapshotAttributesGroup
--     { dsagCreateVolumePermission :: !EmptyElement
--     , dsagProductCodes :: !EmptyElement
--     } deriving (Show)
-- data DescribeSnapshotAttributeResponse = DescribeSnapshotAttributeResponse
--     { dsarRequestId :: !ByteString
--     , dsarSnapshotId :: !ByteString
--   -- <xs:choice>
--     , dsarCreateVolumePermission :: !(Maybe CreateVolumePermissionList)
--     , dsarProductCodes :: !(Maybe ProductCodesSet)
--   -- </xs:choice>
--     } deriving (Show)

-- data BundleInstance = BundleInstance
--     { biInstanceId :: !ByteString
--     , biStorage :: !BundleInstanceTaskStorage
--     } deriving (Show)

-- data BundleInstanceTaskStorage = BundleInstanceTaskStorage
--     { bitsS3 :: !BundleInstanceS3Storage
--     } deriving (Show)

-- data BundleInstanceS3Storage = BundleInstanceS3Storage
--     { bis3sBucket :: !ByteString
--     , bis3sPrefix :: !ByteString
--     , bis3sAwsAccessKeyId :: !(Maybe ByteString)
--     , bis3sUploadPolicy :: !(Maybe ByteString)
--     , bis3sUploadPolicySignature :: !(Maybe ByteString)
--     } deriving (Show)

-- data BundleInstanceResponse = BundleInstanceResponse
--     { birRequestId :: !ByteString
--     , birBundleInstanceTask :: !BundleInstanceTask
--     } deriving (Show)

-- data BundleInstanceTask = BundleInstanceTask
--     { bitInstanceId :: !ByteString
--     , bitBundleId :: !ByteString
--     , bitState :: !ByteString
--     , bitStartTime :: !DateTime
--     , bitUpdateTime :: !DateTime
--     , bitStorage :: !BundleInstanceTaskStorage
--     , bitProgress :: !(Maybe ByteString)
--     , bitError :: !(Maybe BundleInstanceTaskError)
--     } deriving (Show)

-- data BundleInstanceTaskError = BundleInstanceTaskError
--     { biteCode :: !ByteString
--     , biteMessage :: !ByteString
--     } deriving (Show)

-- data DescribeBundleTasks = DescribeBundleTasks
--     { dbtBundlesSet :: !DescribeBundleTasksInfo
--     , dbtFilterSet :: !(Maybe FilterSet)
--     } deriving (Show)

-- data DescribeBundleTasksInfo = DescribeBundleTasksInfo
--     { dbtiItem :: ![DescribeBundleTasksItem]
--     } deriving (Show)

-- data DescribeBundleTasksItem = DescribeBundleTasksItem
--     { dbtiBundleId :: !ByteString
--     } deriving (Show)

-- data DescribeBundleTasksResponse = DescribeBundleTasksResponse
--     { dbtrRequestId :: !ByteString
--     , dbtrBundleInstanceTasksSet :: !BundleInstanceTasksSet
--     } deriving (Show)

-- data BundleInstanceTasksSet = BundleInstanceTasksSet
--     { bitsItem :: ![BundleInstanceTask]
--     } deriving (Show)

-- data CancelBundleTask = CancelBundleTask
--     { cbtBundleId :: !ByteString
--     } deriving (Show)

-- data CancelBundleTaskResponse = CancelBundleTaskResponse
--     { cbtrRequestId :: !ByteString
--     , cbtrBundleInstanceTask :: !BundleInstanceTask
--     } deriving (Show)

-- data CopyImage = CopyImage
--     { ciSourceRegion :: !ByteString
--     , ciSourceImageId :: !ByteString
--     , ciName :: !ByteString
--     , ciDescription :: !(Maybe ByteString)
--     , ciClientToken :: !(Maybe ByteString)
--     } deriving (Show)

-- data CopyImageResponse = CopyImageResponse
--     { cirRequestId :: !ByteString
--     , cirImageId :: !ByteString
--     } deriving (Show)

-- data DescribeRegions = DescribeRegions
--     { drRegionSet :: !DescribeRegionsSet
--     , drFilterSet :: !(Maybe FilterSet)
--     } deriving (Show)

-- data DescribeRegionsSet = DescribeRegionsSet
--     { drsItem :: ![DescribeRegionsSetItem]
--     } deriving (Show)

-- data DescribeRegionsSetItem = DescribeRegionsSetItem
--     { drsiRegionName :: !ByteString
--     } deriving (Show)

-- data DescribeRegionsResponse = DescribeRegionsResponse
--     { drrRequestId :: !ByteString
--     , drrRegionInfo :: !RegionSet
--     } deriving (Show)

-- data RegionSet = RegionSet
--     { rsItem :: ![RegionItem]
--     } deriving (Show)

-- data RegionItem = RegionItem
--     { riRegionName :: !ByteString
--     , riRegionEndpoint :: !ByteString
--     } deriving (Show)

-- data DescribeReservedInstancesOfferings = DescribeReservedInstancesOfferings
--     { drioReservedInstancesOfferingsSet :: !(Maybe DescribeReservedInstancesOfferingsSet)
--     , drioInstance :: !(Maybe ByteString)
--     , drioAvailabilityZone :: !(Maybe ByteString)
--     , drioProductDescription :: !(Maybe ByteString)
--     , drioFilterSet :: !(Maybe FilterSet)
--     , drioInstanceTenancy :: !(Maybe ByteString)
--     , drioOffering :: !(Maybe ByteString)
--     , drioIncludeMarketplace :: !(Maybe Boolean)
--     , drioMinDuration :: !(Maybe Long)
--     , drioMaxDuration :: !(Maybe Long)
--     , drioMaxInstanceCount :: !(Maybe Int)
--     , drioNextToken :: !(Maybe ByteString)
--     , drioMaxResults :: !(Maybe Int)
--     } deriving (Show)

-- data DescribeReservedInstancesOfferingsSet = DescribeReservedInstancesOfferingsSet
--     { driosItem :: ![DescribeReservedInstancesOfferingsSetItem]
--     } deriving (Show)

-- data DescribeReservedInstancesOfferingsSetItem = DescribeReservedInstancesOfferingsSetItem
--     { driosiReservedInstancesOfferingId :: !ByteString
--     } deriving (Show)

-- data DescribeReservedInstancesOfferingsResponse = DescribeReservedInstancesOfferingsResponse
--     { driorRequestId :: !ByteString
--     , driorReservedInstancesOfferingsSet :: !DescribeReservedInstancesOfferingsResponseSet
--     , driorNextToken :: !(Maybe ByteString)
--     } deriving (Show)

-- data DescribeReservedInstancesOfferingsResponseSet = DescribeReservedInstancesOfferingsResponseSet
--     { driorsItem :: ![DescribeReservedInstancesOfferingsResponseSetItem]
--     } deriving (Show)

-- data DescribeReservedInstancesOfferingsResponseSetItem = DescribeReservedInstancesOfferingsResponseSetItem
--     { driorsiReservedInstancesOfferingId :: !ByteString
--     , driorsiInstance :: !ByteString
--     , driorsiAvailabilityZone :: !ByteString
--     , driorsiDuration :: !Long
--     , driorsiFixedPrice :: !Double
--     , driorsiUsagePrice :: !Double
--     , driorsiProductDescription :: !ByteString
--     , driorsiInstanceTenancy :: !ByteString
--     , driorsiCurrencyCode :: !ByteString
--     , driorsiOffering :: !ByteString
--     , driorsiRecurringCharges :: !RecurringChargesSet
--     , driorsiMarketplace :: !(Maybe Boolean)
--     , driorsiPricingDetailsSet :: !(Maybe PricingDetailsSet)
--     } deriving (Show)

-- data RecurringChargesSet = RecurringChargesSet
--     { rcsItem :: ![RecurringChargesSetItem]
--     } deriving (Show)

-- data RecurringChargesSetItem = RecurringChargesSetItem
--     { rcsiFrequency :: !ByteString
--     , rcsiAmount :: !Double
--     } deriving (Show)

-- data PricingDetailsSet = PricingDetailsSet
--     { pdsItem :: ![PricingDetailsSetItem]
--     } deriving (Show)

-- data PricingDetailsSetItem = PricingDetailsSetItem
--     { pdsiPrice :: !Double
--     , pdsiCount :: !Int
--     } deriving (Show)

-- data PurchaseReservedInstancesOffering = PurchaseReservedInstancesOffering
--     { prioReservedInstancesOfferingId :: !ByteString
--     , prioInstanceCount :: !Int
--     , prioLimitPrice :: !(Maybe ReservedInstanceLimitPrice)
--     } deriving (Show)

-- data ReservedInstanceLimitPrice = ReservedInstanceLimitPrice
--     { rilpAmount :: !Double
--     , rilpCurrencyCode :: !(Maybe ByteString)
--     } deriving (Show)

-- data PurchaseReservedInstancesOfferingResponse = PurchaseReservedInstancesOfferingResponse
--     { priorRequestId :: !ByteString
--     , priorReservedInstancesId :: !ByteString
--     } deriving (Show)

-- data DescribeReservedInstances = DescribeReservedInstances
--     { driReservedInstancesSet :: !(Maybe DescribeReservedInstancesSet)
--     , driFilterSet :: !(Maybe FilterSet)
--     , driOffering :: !(Maybe ByteString)
--     } deriving (Show)

-- data DescribeReservedInstancesSet = DescribeReservedInstancesSet
--     { drisItem :: ![DescribeReservedInstancesSetItem]
--     } deriving (Show)

-- data DescribeReservedInstancesSetItem = DescribeReservedInstancesSetItem
--     { drisiReservedInstancesId :: !ByteString
--     } deriving (Show)

-- data DescribeReservedInstancesResponse = DescribeReservedInstancesResponse
--     { drirRequestId :: !ByteString
--     , drirReservedInstancesSet :: !DescribeReservedInstancesResponseSet
--     } deriving (Show)

-- data DescribeReservedInstancesResponseSet = DescribeReservedInstancesResponseSet
--     { drirsItem :: ![DescribeReservedInstancesResponseSetItem]
--     } deriving (Show)

-- data DescribeReservedInstancesResponseSetItem = DescribeReservedInstancesResponseSetItem
--     { drirsiReservedInstancesId :: !ByteString
--     , drirsiInstance :: !ByteString
--     , drirsiAvailabilityZone :: !ByteString
--     , drirsiStart :: !DateTime
--     , drirsiDuration :: !Long
--     , drirsiFixedPrice :: !Double
--     , drirsiUsagePrice :: !Double
--     , drirsiInstanceCount :: !Integer
--     , drirsiProductDescription :: !ByteString
--     , drirsiState :: !ByteString
--     , drirsiTagSet :: !(Maybe ResourceTagSet)
--     , drirsiInstanceTenancy :: !ByteString
--     , drirsiCurrencyCode :: !ByteString
--     , drirsiOffering :: !ByteString
--     , drirsiRecurringCharges :: !(Maybe RecurringChargesSet)
--     } deriving (Show)

-- data CreateReservedInstancesListing = CreateReservedInstancesListing
--     { crilReservedInstancesId :: !ByteString
--     , crilInstanceCount :: !(Maybe Int)
--     , crilPriceSchedules :: !PriceScheduleRequestSet
--     , crilClientToken :: !ByteString
--     } deriving (Show)

-- data PriceScheduleRequestSet = PriceScheduleRequestSet
--     { psrsItem :: ![PriceScheduleRequestSetItem]
--     } deriving (Show)

-- data PriceScheduleRequestSetItem = PriceScheduleRequestSetItem
--     { psrsiTerm :: !Long
--     , psrsiPrice :: !Double
--     , psrsiCurrencyCode :: !(Maybe ByteString)
--     } deriving (Show)

-- data CreateReservedInstancesListingResponse = CreateReservedInstancesListingResponse
--     { crilrRequestId :: !ByteString
--     , crilrReservedInstancesListingsSet :: !DescribeReservedInstancesListingsResponseSet
--     } deriving (Show)

-- data CancelReservedInstancesListing = CancelReservedInstancesListing
--     { crilReservedInstancesListingId :: !ByteString
--     } deriving (Show)

-- data CancelReservedInstancesListingResponse = CancelReservedInstancesListingResponse
--     { crilrRequestId :: !ByteString
--     , crilrReservedInstancesListingsSet :: !DescribeReservedInstancesListingsResponseSet
--     } deriving (Show)

-- data DescribeReservedInstancesListings = DescribeReservedInstancesListings
--     { drilReservedInstancesListingSet :: !(Maybe DescribeReservedInstancesListingSet)
--     , drilReservedInstancesSet :: !(Maybe DescribeReservedInstancesSet)
--     , drilFilterSet :: !(Maybe FilterSet)
--     } deriving (Show)

-- data DescribeReservedInstancesListingSet = DescribeReservedInstancesListingSet
--     { drilsItem :: ![DescribeReservedInstancesListingSetItem]
--     } deriving (Show)

-- data DescribeReservedInstancesListingSetItem = DescribeReservedInstancesListingSetItem
--     { drilsiReservedInstancesListingId :: !ByteString
--     } deriving (Show)

-- data DescribeReservedInstancesListingsResponse = DescribeReservedInstancesListingsResponse
--     { drilrRequestId :: !ByteString
--     , drilrReservedInstancesListingsSet :: !DescribeReservedInstancesListingsResponseSet
--     } deriving (Show)

-- data DescribeReservedInstancesListingsResponseSet = DescribeReservedInstancesListingsResponseSet
--     { drilrsItem :: ![DescribeReservedInstancesListingsResponseSetItem]
--     } deriving (Show)

-- data DescribeReservedInstancesListingsResponseSetItem = DescribeReservedInstancesListingsResponseSetItem
--     { drilrsiReservedInstancesListingId :: !ByteString
--     , drilrsiReservedInstancesId :: !ByteString
--     , drilrsiCreateDate :: !DateTime
--     , drilrsiUpdateDate :: !DateTime
--     , drilrsiStatus :: !ByteString
--     , drilrsiStatusMessage :: !ByteString
--     , drilrsiInstanceCounts :: !InstanceCountsSet
--     , drilrsiPriceSchedules :: !PriceScheduleSet
--     , drilrsiTagSet :: !(Maybe ResourceTagSet)
--     , drilrsiClientToken :: !(Maybe ByteString)
--     } deriving (Show)

-- data InstanceCountsSet = InstanceCountsSet
--     { icsItem :: ![InstanceCountsSetItem]
--     } deriving (Show)

-- data InstanceCountsSetItem = InstanceCountsSetItem
--     { icsiState :: !ByteString
--     , icsiInstanceCount :: !Int
--     } deriving (Show)

-- data PriceScheduleSet = PriceScheduleSet
--     { pssItem :: ![PriceScheduleSetItem]
--     } deriving (Show)

-- data PriceScheduleSetItem = PriceScheduleSetItem
--     { pssiTerm :: !Long
--     , pssiPrice :: !Double
--     , pssiCurrencyCode :: !(Maybe ByteString)
--     , pssiActive :: !Boolean
--     } deriving (Show)

-- data MonitorInstances = MonitorInstances
--     { miInstancesSet :: !MonitorInstancesSet
--     } deriving (Show)

-- data MonitorInstancesSet = MonitorInstancesSet
--     { misItem :: !(NonEmpty MonitorInstancesSetItem)
--     } deriving (Show)

-- data MonitorInstancesSetItem = MonitorInstancesSetItem
--     { misiInstanceId :: !ByteString
--     } deriving (Show)

-- data MonitorInstancesResponse = MonitorInstancesResponse
--     { mirRequestId :: !ByteString
--     , mirInstancesSet :: !MonitorInstancesResponseSet
--     } deriving (Show)

-- data MonitorInstancesResponseSet = MonitorInstancesResponseSet
--     { mirsItem :: !(NonEmpty MonitorInstancesResponseSetItem)
--     } deriving (Show)

-- data MonitorInstancesResponseSetItem = MonitorInstancesResponseSetItem
--     { mirsiInstanceId :: !ByteString
--     , mirsiMonitoring :: !InstanceMonitoringState
--     } deriving (Show)

-- data InstanceMonitoringState = InstanceMonitoringState
--     { imsState :: !ByteString
--     } deriving (Show)

-- data Attachment = Attachment
--     { aVpcId :: !ByteString
--     , aState :: !ByteString
--     } deriving (Show)

-- data AttachmentSet = AttachmentSet
--     { asItem :: ![Attachment]
--     } deriving (Show)

-- data VpnGateway = VpnGateway
--     { vgVpnGatewayId :: !ByteString
--     , vgState :: !ByteString
--     , vgType :: !ByteString
--     , vgAvailabilityZone :: !(Maybe ByteString)
--     , vgAttachments :: !AttachmentSet
--     , vgTagSet :: !(Maybe ResourceTagSet)
--     } deriving (Show)

-- data CustomerGateway = CustomerGateway
--     { cgCustomerGatewayId :: !ByteString
--     , cgState :: !ByteString
--     , cgType :: !ByteString
--     , cgIpAddress :: !ByteString
--     , cgBgpAsn :: !(Maybe Int)
--     , cgTagSet :: !(Maybe ResourceTagSet)
--     } deriving (Show)

-- data VpnConnection = VpnConnection
--     { vcVpnConnectionId :: !ByteString
--     , vcState :: !ByteString
--     , vcCustomerGatewayConfiguration :: !(Maybe ByteString)
--     , vcType :: !(Maybe ByteString)
--     , vcCustomerGatewayId :: !ByteString
--     , vcVpnGatewayId :: !ByteString
--     , vcTagSet :: !(Maybe ResourceTagSet)
--     , vcVgwTelemetry :: !(Maybe VgwTelemetry)
--     , vcOptions :: !(Maybe VpnConnectionOptionsResponse)
--     , vcRoutes :: !(Maybe VpnStaticRoutesSet)
--     } deriving (Show)

-- data VpnConnectionOptionsResponse = VpnConnectionOptionsResponse
--     { vcorStaticRoutesOnly :: !(Maybe Boolean)
--     } deriving (Show)

-- data VpnStaticRoutesSet = VpnStaticRoutesSet
--     { vsrsItem :: ![VpnStaticRoute]
--     } deriving (Show)

-- data VpnStaticRoute = VpnStaticRoute
--     { vsrDestinationCidrBlock :: !ByteString
--     , vsrSource :: !ByteString
--     , vsrState :: !ByteString
--     } deriving (Show)

-- data VgwTelemetry = VgwTelemetry
--     { vtItem :: ![VpnTunnelTelemetry]
--     } deriving (Show)

-- data VpnTunnelTelemetry = VpnTunnelTelemetry
--     { vttOutsideIpAddress :: !ByteString
--     , vttStatus :: !ByteString
--     , vttLastStatusChange :: !DateTime
--     , vttStatusMessage :: !(Maybe ByteString)
--     , vttAcceptedRouteCount :: !Int
--     } deriving (Show)

-- data Vpc = Vpc
--     { vVpcId :: !ByteString
--     , vState :: !(Maybe ByteString)
--     , vCidrBlock :: !(Maybe ByteString)
--     , vDhcpOptionsId :: !(Maybe ByteString)
--     , vTagSet :: !(Maybe ResourceTagSet)
--     , vInstanceTenancy :: !(Maybe ByteString)
--     , vIsDefault :: !(Maybe Boolean)
--     } deriving (Show)

-- data Subnet = Subnet
--     { sSubnetId :: !ByteString
--     , sState :: !(Maybe ByteString)
--     , sVpcId :: !(Maybe ByteString)
--     , sCidrBlock :: !(Maybe ByteString)
--     , sAvailableIpAddressCount :: !(Maybe Int)
--     , sAvailabilityZone :: !(Maybe ByteString)
--     , sDefaultForAz :: !(Maybe Boolean)
--     , sMapPublicIpOnLaunch :: !(Maybe Boolean)
--     , sTagSet :: !(Maybe ResourceTagSet)
--     } deriving (Show)

-- data CustomerGatewaySet = CustomerGatewaySet
--     { cgsItem :: ![CustomerGateway]
--     } deriving (Show)

-- data VpnGatewaySet = VpnGatewaySet
--     { vgsItem :: ![VpnGateway]
--     } deriving (Show)

-- data VpnConnectionSet = VpnConnectionSet
--     { vcsItem :: ![VpnConnection]
--     } deriving (Show)

-- data VpcSet = VpcSet
--     { vsItem :: ![Vpc]
--     } deriving (Show)

-- data SubnetSet = SubnetSet
--     { ssItem :: ![Subnet]
--     } deriving (Show)

-- data CustomerGatewayIdSetItem = CustomerGatewayIdSetItem
--     { cgisiCustomerGatewayId :: !ByteString
--     } deriving (Show)

-- data CustomerGatewayIdSet = CustomerGatewayIdSet
--     { cgisItem :: ![CustomerGatewayIdSetItem]
--     } deriving (Show)

-- data VpnGatewayIdSetItem = VpnGatewayIdSetItem
--     { vgisiVpnGatewayId :: !ByteString
--     } deriving (Show)

-- data VpnGatewayIdSet = VpnGatewayIdSet
--     { vgisItem :: ![VpnGatewayIdSetItem]
--     } deriving (Show)

-- data VpnConnectionIdSetItem = VpnConnectionIdSetItem
--     { vcisiVpnConnectionId :: !ByteString
--     } deriving (Show)

-- data VpnConnectionIdSet = VpnConnectionIdSet
--     { vcisItem :: ![VpnConnectionIdSetItem]
--     } deriving (Show)

-- data VpcIdSetItem = VpcIdSetItem
--     { visiVpcId :: !ByteString
--     } deriving (Show)

-- data VpcIdSet = VpcIdSet
--     { visItem :: ![VpcIdSetItem]
--     } deriving (Show)

-- data SubnetIdSetItem = SubnetIdSetItem
--     { sisiSubnetId :: !ByteString
--     } deriving (Show)

-- data SubnetIdSet = SubnetIdSet
--     { sisItem :: ![SubnetIdSetItem]
--     } deriving (Show)

-- data DhcpOptionsIdSetItem = DhcpOptionsIdSetItem
--     { doisiDhcpOptionsId :: !ByteString
--     } deriving (Show)

-- data DhcpOptionsIdSet = DhcpOptionsIdSet
--     { doisItem :: ![DhcpOptionsIdSetItem]
--     } deriving (Show)

-- data DhcpConfigurationItemSet = DhcpConfigurationItemSet
--     { dcisItem :: ![DhcpConfigurationItem]
--     } deriving (Show)

-- data DhcpOptionsSet = DhcpOptionsSet
--     { dosItem :: ![DhcpOptions]
--     } deriving (Show)

-- data DhcpConfigurationItem = DhcpConfigurationItem
--     { dciKey :: !ByteString
--     , dciValueSet :: !DhcpValueSet
--     } deriving (Show)

-- data DhcpOptions = DhcpOptions
--     { doDhcpOptionsId :: !ByteString
--     , doDhcpConfigurationSet :: !DhcpConfigurationItemSet
--     , doTagSet :: !(Maybe ResourceTagSet)
--     } deriving (Show)

-- data DhcpValue = DhcpValue
--     { dvValue :: !ByteString
--     } deriving (Show)

-- data DhcpValueSet = DhcpValueSet
--     { dvsItem :: ![DhcpValue]
--     } deriving (Show)

-- data Filter = Filter
--     { fName :: !ByteString
--     , fValueSet :: !ValueSet
--     } deriving (Show)

-- data FilterSet = FilterSet
--     { fsItem :: ![Filter]
--     } deriving (Show)

-- data Value = Value
--     { vValue :: !ByteString
--     } deriving (Show)

-- data ValueSet = ValueSet
--     { vsItem :: ![Value]
--     } deriving (Show)

-- data CreateCustomerGateway = CreateCustomerGateway
--     { ccgType :: !ByteString
--     , ccgIpAddress :: !ByteString
--     , ccgBgpAsn :: !(Maybe Int)
--     } deriving (Show)

-- data CreateCustomerGatewayResponse = CreateCustomerGatewayResponse
--     { ccgrRequestId :: !ByteString
--     , ccgrCustomerGateway :: !CustomerGateway
--     } deriving (Show)

-- data DeleteCustomerGateway = DeleteCustomerGateway
--     { dcgCustomerGatewayId :: !ByteString
--     } deriving (Show)

-- data DeleteCustomerGatewayResponse = DeleteCustomerGatewayResponse
--     { dcgrRequestId :: !ByteString
--     , dcgrReturn :: !Boolean
--     } deriving (Show)

-- data DescribeCustomerGateways = DescribeCustomerGateways
--     { dcgCustomerGatewaySet :: !(Maybe CustomerGatewayIdSet)
--     , dcgFilterSet :: !(Maybe FilterSet)
--     } deriving (Show)

-- data DescribeCustomerGatewaysResponse = DescribeCustomerGatewaysResponse
--     { dcgrRequestId :: !ByteString
--     , dcgrCustomerGatewaySet :: !CustomerGatewaySet
--     } deriving (Show)

-- data CreateVpnGateway = CreateVpnGateway
--     { cvgType :: !ByteString
--     , cvgAvailabilityZone :: !(Maybe ByteString)
--     } deriving (Show)

-- data CreateVpnGatewayResponse = CreateVpnGatewayResponse
--     { cvgrRequestId :: !ByteString
--     , cvgrVpnGateway :: !VpnGateway
--     } deriving (Show)

-- data DeleteVpnGateway = DeleteVpnGateway
--     { dvgVpnGatewayId :: !ByteString
--     } deriving (Show)

-- data DeleteVpnGatewayResponse = DeleteVpnGatewayResponse
--     { dvgrRequestId :: !ByteString
--     , dvgrReturn :: !Boolean
--     } deriving (Show)

-- data DescribeVpnGateways = DescribeVpnGateways
--     { dvgVpnGatewaySet :: !(Maybe VpnGatewayIdSet)
--     , dvgFilterSet :: !(Maybe FilterSet)
--     } deriving (Show)

-- data DescribeVpnGatewaysResponse = DescribeVpnGatewaysResponse
--     { dvgrRequestId :: !ByteString
--     , dvgrVpnGatewaySet :: !VpnGatewaySet
--     } deriving (Show)

-- data CreateVpnConnection = CreateVpnConnection
--     { cvcType :: !ByteString
--     , cvcCustomerGatewayId :: !ByteString
--     , cvcVpnGatewayId :: !ByteString
--     , cvcOptions :: !(Maybe VpnConnectionOptionsRequest)
--     } deriving (Show)

-- data VpnConnectionOptionsRequest = VpnConnectionOptionsRequest
--     { vcorStaticRoutesOnly :: !(Maybe Boolean)
--     } deriving (Show)

-- data CreateVpnConnectionResponse = CreateVpnConnectionResponse
--     { cvcrRequestId :: !ByteString
--     , cvcrVpnConnection :: !VpnConnection
--     } deriving (Show)

-- data CreateVpnConnectionRoute = CreateVpnConnectionRoute
--     { cvcrVpnConnectionId :: !ByteString
--     , cvcrDestinationCidrBlock :: !ByteString
--     } deriving (Show)

-- data CreateVpnConnectionRouteResponse = CreateVpnConnectionRouteResponse
--     { cvcrrRequestId :: !ByteString
--     , cvcrrReturn :: !Boolean
--     } deriving (Show)

-- data DeleteVpnConnectionRoute = DeleteVpnConnectionRoute
--     { dvcrVpnConnectionId :: !ByteString
--     , dvcrDestinationCidrBlock :: !ByteString
--     } deriving (Show)

-- data DeleteVpnConnectionRouteResponse = DeleteVpnConnectionRouteResponse
--     { dvcrrRequestId :: !ByteString
--     , dvcrrReturn :: !Boolean
--     } deriving (Show)

-- data DeleteVpnConnection = DeleteVpnConnection
--     { dvcVpnConnectionId :: !ByteString
--     } deriving (Show)

-- data DeleteVpnConnectionResponse = DeleteVpnConnectionResponse
--     { dvcrRequestId :: !ByteString
--     , dvcrReturn :: !Boolean
--     } deriving (Show)

-- data DescribeVpnConnections = DescribeVpnConnections
--     { dvcVpnConnectionSet :: !(Maybe VpnConnectionIdSet)
--     , dvcFilterSet :: !(Maybe FilterSet)
--     } deriving (Show)

-- data DescribeVpnConnectionsResponse = DescribeVpnConnectionsResponse
--     { dvcrRequestId :: !ByteString
--     , dvcrVpnConnectionSet :: !VpnConnectionSet
--     } deriving (Show)

-- data AttachVpnGateway = AttachVpnGateway
--     { avgVpnGatewayId :: !ByteString
--     , avgVpcId :: !ByteString
--     } deriving (Show)

-- data AttachVpnGatewayResponse = AttachVpnGatewayResponse
--     { avgrRequestId :: !ByteString
--     , avgrAttachment :: !Attachment
--     } deriving (Show)

-- data DetachVpnGateway = DetachVpnGateway
--     { dvgVpnGatewayId :: !ByteString
--     , dvgVpcId :: !ByteString
--     } deriving (Show)

-- data DetachVpnGatewayResponse = DetachVpnGatewayResponse
--     { dvgrRequestId :: !ByteString
--     , dvgrReturn :: !Boolean
--     } deriving (Show)

-- data CreateVpc = CreateVpc
--     { cvCidrBlock :: !ByteString
--     , cvInstanceTenancy :: !(Maybe ByteString)
--     } deriving (Show)

-- data CreateVpcResponse = CreateVpcResponse
--     { cvrRequestId :: !ByteString
--     , cvrVpc :: !Vpc
--     } deriving (Show)

-- data DescribeVpcs = DescribeVpcs
--     { dvVpcSet :: !(Maybe VpcIdSet)
--     , dvFilterSet :: !(Maybe FilterSet)
--     } deriving (Show)

-- data DescribeVpcsResponse = DescribeVpcsResponse
--     { dvrRequestId :: !ByteString
--     , dvrVpcSet :: !VpcSet
--     } deriving (Show)

-- data DeleteVpc = DeleteVpc
--     { dvVpcId :: !ByteString
--     } deriving (Show)

-- data DeleteVpcResponse = DeleteVpcResponse
--     { dvrRequestId :: !ByteString
--     , dvrReturn :: !Boolean
--     } deriving (Show)

-- data CreateSubnet = CreateSubnet
--     { csVpcId :: !ByteString
--     , csCidrBlock :: !ByteString
--     , csAvailabilityZone :: !(Maybe ByteString)
--     } deriving (Show)

-- data CreateSubnetResponse = CreateSubnetResponse
--     { csrRequestId :: !ByteString
--     , csrSubnet :: !Subnet
--     } deriving (Show)

-- data DescribeSubnets = DescribeSubnets
--     { dsSubnetSet :: !(Maybe SubnetIdSet)
--     , dsFilterSet :: !(Maybe FilterSet)
--     } deriving (Show)

-- data DescribeSubnetsResponse = DescribeSubnetsResponse
--     { dsrRequestId :: !ByteString
--     , dsrSubnetSet :: !SubnetSet
--     } deriving (Show)

-- data DeleteSubnet = DeleteSubnet
--     { dsSubnetId :: !ByteString
--     } deriving (Show)

-- data DeleteSubnetResponse = DeleteSubnetResponse
--     { dsrRequestId :: !ByteString
--     , dsrReturn :: !Boolean
--     } deriving (Show)

-- data DeleteDhcpOptions = DeleteDhcpOptions
--     { ddoDhcpOptionsId :: !ByteString
--     } deriving (Show)

-- data DeleteDhcpOptionsResponse = DeleteDhcpOptionsResponse
--     { ddorRequestId :: !ByteString
--     , ddorReturn :: !Boolean
--     } deriving (Show)

-- data DescribeDhcpOptions = DescribeDhcpOptions
--     { ddoDhcpOptionsSet :: !(Maybe DhcpOptionsIdSet)
--     , ddoFilterSet :: !(Maybe FilterSet)
--     } deriving (Show)

-- data DescribeDhcpOptionsResponse = DescribeDhcpOptionsResponse
--     { ddorRequestId :: !ByteString
--     , ddorDhcpOptionsSet :: !DhcpOptionsSet
--     } deriving (Show)

-- data CreateDhcpOptions = CreateDhcpOptions
--     { cdoDhcpConfigurationSet :: !DhcpConfigurationItemSet
--     } deriving (Show)

-- data CreateDhcpOptionsResponse = CreateDhcpOptionsResponse
--     { cdorRequestId :: !ByteString
--     , cdorDhcpOptions :: !DhcpOptions
--     } deriving (Show)

-- data AssociateDhcpOptions = AssociateDhcpOptions
--     { adoDhcpOptionsId :: !ByteString
--     , adoVpcId :: !ByteString
--     } deriving (Show)

-- data AssociateDhcpOptionsResponse = AssociateDhcpOptionsResponse
--     { adorRequestId :: !ByteString
--     , adorReturn :: !Boolean
--     } deriving (Show)

-- data RequestSpotInstances = RequestSpotInstances
--     { rsiSpotPrice :: !ByteString
--     , rsiInstanceCount :: !(Maybe Integer)
--     , rsiType :: !(Maybe ByteString)
--     , rsiValidFrom :: !(Maybe DateTime)
--     , rsiValidUntil :: !(Maybe DateTime)
--     , rsiLaunchGroup :: !(Maybe ByteString)
--     , rsiAvailabilityZoneGroup :: !(Maybe ByteString)
--     , rsiLaunchSpecification :: !LaunchSpecificationRequest
--     } deriving (Show)

-- data LaunchSpecificationRequest = LaunchSpecificationRequest
--     { lsrImageId :: !ByteString
--     , lsrKeyName :: !(Maybe ByteString)
--     , lsrGroupSet :: !GroupSet
--     , lsrUserData :: !(Maybe UserData)
--     , lsrAddressing :: !(Maybe ByteString)
--     , lsrInstance :: !ByteString
--     , lsrPlacement :: !(Maybe SpotPlacementRequest)
--     , lsrKernelId :: !(Maybe ByteString)
--     , lsrRamdiskId :: !(Maybe ByteString)
--     , lsrBlockDeviceMapping :: !(Maybe BlockDeviceMapping)
--     , lsrMonitoring :: !(Maybe MonitoringInstance)
--     , lsrSubnetId :: !(Maybe ByteString)
--     , lsrNetworkInterfaceSet :: !(Maybe InstanceNetworkInterfaceSetRequest)
--     , lsrIamInstanceProfile :: !(Maybe IamInstanceProfileRequest)
--     , lsrEbsOptimized :: !(Maybe Boolean)
--     } deriving (Show)

-- data LaunchSpecificationResponse = LaunchSpecificationResponse
--     { lsrImageId :: !ByteString
--     , lsrKeyName :: !(Maybe ByteString)
--     , lsrGroupSet :: !GroupSet
--     , lsrAddressing :: !(Maybe ByteString)
--     , lsrInstance :: !ByteString
--     , lsrPlacement :: !(Maybe SpotPlacementRequest)
--     , lsrKernelId :: !(Maybe ByteString)
--     , lsrRamdiskId :: !(Maybe ByteString)
--     , lsrBlockDeviceMapping :: !(Maybe BlockDeviceMapping)
--     , lsrMonitoring :: !(Maybe MonitoringInstance)
--     , lsrSubnetId :: !(Maybe ByteString)
--     , lsrNetworkInterfaceSet :: !(Maybe InstanceNetworkInterfaceSetRequest)
--     , lsrIamInstanceProfile :: !(Maybe IamInstanceProfileRequest)
--     , lsrEbsOptimized :: !(Maybe Boolean)
--     } deriving (Show)

-- data SpotInstanceRequestSetItem = SpotInstanceRequestSetItem
--     { sirsiSpotInstanceRequestId :: !ByteString
--     , sirsiSpotPrice :: !ByteString
--     , sirsiType :: !ByteString
--     , sirsiState :: !ByteString
--     , sirsiFault :: !(Maybe SpotInstanceStateFault)
--     , sirsiStatus :: !(Maybe SpotInstanceStatusMessage)
--     , sirsiValidFrom :: !(Maybe DateTime)
--     , sirsiValidUntil :: !(Maybe DateTime)
--     , sirsiLaunchGroup :: !(Maybe ByteString)
--     , sirsiAvailabilityZoneGroup :: !(Maybe ByteString)
--     , sirsiLaunchSpecification :: !(Maybe LaunchSpecificationResponse)
--     , sirsiInstanceId :: !(Maybe ByteString)
--     , sirsiCreateTime :: !(Maybe DateTime)
--     , sirsiProductDescription :: !(Maybe ByteString)
--     , sirsiTagSet :: !(Maybe ResourceTagSet)
--     , sirsiLaunchedAvailabilityZone :: !(Maybe ByteString)
--     } deriving (Show)

-- data SpotInstanceStateFault = SpotInstanceStateFault
--     { sisfCode :: !ByteString
--     , sisfMessage :: !ByteString
--     } deriving (Show)

-- data SpotInstanceStatusMessage = SpotInstanceStatusMessage
--     { sismCode :: !(Maybe ByteString)
--     , sismUpdateTime :: !(Maybe DateTime)
--     , sismMessage :: !(Maybe ByteString)
--     } deriving (Show)

-- data SpotInstanceRequestSet = SpotInstanceRequestSet
--     { sirsItem :: ![SpotInstanceRequestSetItem]
--     } deriving (Show)

-- data RequestSpotInstancesResponse = RequestSpotInstancesResponse
--     { rsirRequestId :: !ByteString
--     , rsirSpotInstanceRequestSet :: !SpotInstanceRequestSet
--     } deriving (Show)

-- data DescribeSpotInstanceRequests = DescribeSpotInstanceRequests
--     { dsirSpotInstanceRequestIdSet :: !SpotInstanceRequestIdSet
--     , dsirFilterSet :: !(Maybe FilterSet)
--     } deriving (Show)

-- data SpotInstanceRequestIdSet = SpotInstanceRequestIdSet
--     { sirisItem :: ![SpotInstanceRequestIdSetItem]
--     } deriving (Show)

-- data SpotInstanceRequestIdSetItem = SpotInstanceRequestIdSetItem
--     { sirisiSpotInstanceRequestId :: !ByteString
--     } deriving (Show)

-- data DescribeSpotInstanceRequestsResponse = DescribeSpotInstanceRequestsResponse
--     { dsirrRequestId :: !ByteString
--     , dsirrSpotInstanceRequestSet :: !SpotInstanceRequestSet
--     } deriving (Show)

-- data CancelSpotInstanceRequests = CancelSpotInstanceRequests
--     { csirSpotInstanceRequestIdSet :: !SpotInstanceRequestIdSet
--     } deriving (Show)

-- data CancelSpotInstanceRequestsResponse = CancelSpotInstanceRequestsResponse
--     { csirrRequestId :: !ByteString
--     , csirrSpotInstanceRequestSet :: !CancelSpotInstanceRequestsResponseSet
--     } deriving (Show)

-- data CancelSpotInstanceRequestsResponseSet = CancelSpotInstanceRequestsResponseSet
--     { csirrsItem :: !(NonEmpty CancelSpotInstanceRequestsResponseSetItem)
--     } deriving (Show)

-- data CancelSpotInstanceRequestsResponseSetItem = CancelSpotInstanceRequestsResponseSetItem
--     { csirrsiSpotInstanceRequestId :: !ByteString
--     , csirrsiState :: !ByteString
--     } deriving (Show)

-- data DescribeSpotPriceHistory = DescribeSpotPriceHistory
--     { dsphStartTime :: !(Maybe DateTime)
--     , dsphEndTime :: !(Maybe DateTime)
--     , dsphInstanceSet :: !(Maybe InstanceSet)
--     , dsphProductDescriptionSet :: !(Maybe ProductDescriptionSet)
--     , dsphFilterSet :: !(Maybe FilterSet)
--     , dsphAvailabilityZone :: !(Maybe ByteString)
--     , dsphMaxResults :: !(Maybe Integer)
--     , dsphNextToken :: !(Maybe ByteString)
--     } deriving (Show)

-- data InstanceSet = InstanceSet
--     { isItem :: !(NonEmpty InstanceSetItem)
--     } deriving (Show)

-- data InstanceSetItem = InstanceSetItem
--     { isiInstance :: !ByteString
--     } deriving (Show)

-- data ProductDescriptionSet = ProductDescriptionSet
--     { pdsItem :: !(NonEmpty ProductDescriptionSetItem)
--     } deriving (Show)

-- data ProductDescriptionSetItem = ProductDescriptionSetItem
--     { pdsiProductDescription :: !ByteString
--     } deriving (Show)

-- data DescribeSpotPriceHistoryResponse = DescribeSpotPriceHistoryResponse
--     { dsphrRequestId :: !ByteString
--     , dsphrSpotPriceHistorySet :: !SpotPriceHistorySet
--     , dsphrNextToken :: !(Maybe ByteString)
--     } deriving (Show)

-- data SpotPriceHistorySet = SpotPriceHistorySet
--     { sphsItem :: ![SpotPriceHistorySetItem]
--     } deriving (Show)

-- data SpotPriceHistorySetItem = SpotPriceHistorySetItem
--     { sphsiInstance :: !ByteString
--     , sphsiProductDescription :: !ByteString
--     , sphsiSpotPrice :: !ByteString
--     , sphsiTimestamp :: !DateTime
--     , sphsiAvailabilityZone :: !(Maybe ByteString)
--     } deriving (Show)

-- data SpotDatafeedSubscription = SpotDatafeedSubscription
--     { sdsOwnerId :: !ByteString
--     , sdsBucket :: !ByteString
--     , sdsPrefix :: !ByteString
--     , sdsState :: !ByteString
--     , sdsFault :: !(Maybe SpotInstanceStateFault)
--     } deriving (Show)

-- data CreateSpotDatafeedSubscription = CreateSpotDatafeedSubscription
--     { csdsBucket :: !ByteString
--     , csdsPrefix :: !ByteString
--     } deriving (Show)

-- data CreateSpotDatafeedSubscriptionResponse = CreateSpotDatafeedSubscriptionResponse
--     { csdsrRequestId :: !ByteString
--     , csdsrSpotDatafeedSubscription :: !SpotDatafeedSubscription
--     } deriving (Show)

-- data DescribeSpotDatafeedSubscription = DescribeSpotDatafeedSubscription

-- data DescribeSpotDatafeedSubscriptionResponse = DescribeSpotDatafeedSubscriptionResponse
--     { dsdsrRequestId :: !ByteString
--     , dsdsrSpotDatafeedSubscription :: !SpotDatafeedSubscription
--     } deriving (Show)

-- data DescribeSpotDatafeedSubscription = DescribeSpotDatafeedSubscription

-- data DeleteSpotDatafeedSubscriptionResponse = DeleteSpotDatafeedSubscriptionResponse
--     { dsdsrRequestId :: !ByteString
--     , dsdsrReturn :: !Boolean
--     } deriving (Show)

-- data DescribeLicenses = DescribeLicenses
--     { dlLicenseIdSet :: !(Maybe LicenseIdSet)
--     , dlFilterSet :: !(Maybe FilterSet)
--     } deriving (Show)

-- data LicenseIdSet = LicenseIdSet
--     { lisItem :: ![LicenseIdSetItem]
--     } deriving (Show)

-- data LicenseIdSetItem = LicenseIdSetItem
--     { lisiLicenseId :: !ByteString
--     } deriving (Show)

-- data DescribeLicensesResponse = DescribeLicensesResponse
--     { dlrRequestId :: !ByteString
--     , dlrLicenseSet :: !LicenseSet
--     } deriving (Show)

-- data LicenseSet = LicenseSet
--     { lsItem :: ![LicenseSetItem]
--     } deriving (Show)

-- data LicenseSetItem = LicenseSetItem
--     { lsiLicenseId :: !ByteString
--     , lsiType :: !ByteString
--     , lsiPool :: !ByteString
--     , lsiCapacitySet :: !LicenseCapacitySet
--     , lsiTagSet :: !(Maybe ResourceTagSet)
--     } deriving (Show)

-- data LicenseCapacitySet = LicenseCapacitySet
--     { lcsItem :: ![LicenseCapacitySetItem]
--     } deriving (Show)

-- data LicenseCapacitySetItem = LicenseCapacitySetItem
--     { lcsiCapacity :: !Int
--     , lcsiInstanceCapacity :: !Int
--     , lcsiState :: !ByteString
--     , lcsiEarliestAllowedDeactivationTime :: !(Maybe DateTime)
--     } deriving (Show)

-- data ActivateLicense = ActivateLicense
--     { alLicenseId :: !ByteString
--     , alCapacity :: !Int
--     } deriving (Show)

-- data ActivateLicenseResponse = ActivateLicenseResponse
--     { alrRequestId :: !ByteString
--     , alrReturn :: !Boolean
--     } deriving (Show)

-- data DeactivateLicense = DeactivateLicense
--     { dlLicenseId :: !ByteString
--     , dlCapacity :: !Int
--     } deriving (Show)

-- data DeactivateLicenseResponse = DeactivateLicenseResponse
--     { dlrRequestId :: !ByteString
--     , dlrReturn :: !Boolean
--     } deriving (Show)

-- data CreatePlacementGroup = CreatePlacementGroup
--     { cpgGroupName :: !ByteString
--     , cpgStrategy :: !ByteString
--     } deriving (Show)

-- data CreatePlacementGroupResponse = CreatePlacementGroupResponse
--     { cpgrRequestId :: !ByteString
--     , cpgrReturn :: !Boolean
--     } deriving (Show)

-- data DeletePlacementGroup = DeletePlacementGroup
--     { dpgGroupName :: !ByteString
--     } deriving (Show)

-- data DeletePlacementGroupResponse = DeletePlacementGroupResponse
--     { dpgrRequestId :: !ByteString
--     , dpgrReturn :: !Boolean
--     } deriving (Show)

-- data DescribePlacementGroupItem = DescribePlacementGroupItem
--     { dpgiGroupName :: !ByteString
--     } deriving (Show)

-- data DescribePlacementGroupsInfo = DescribePlacementGroupsInfo
--     { dpgiItem :: ![DescribePlacementGroupItem]
--     } deriving (Show)

-- data DescribePlacementGroups = DescribePlacementGroups
--     { dpgPlacementGroupSet :: !DescribePlacementGroupsInfo
--     , dpgFilterSet :: !(Maybe FilterSet)
--     } deriving (Show)

-- data PlacementGroupInfo = PlacementGroupInfo
--     { pgiGroupName :: !ByteString
--     , pgiStrategy :: !ByteString
--     , pgiState :: !ByteString
--     } deriving (Show)

-- data PlacementGroupSet = PlacementGroupSet
--     { pgsItem :: ![PlacementGroupInfo]
--     } deriving (Show)

-- data DescribePlacementGroupsResponse = DescribePlacementGroupsResponse
--     { dpgrRequestId :: !ByteString
--     , dpgrPlacementGroupSet :: !PlacementGroupSet
--     } deriving (Show)

-- data ResourceIdSet = ResourceIdSet
--     { risItem :: ![ResourceIdSetItem]
--     } deriving (Show)

-- data ResourceIdSetItem = ResourceIdSetItem
--     { risiResourceId :: !ByteString
--     } deriving (Show)

-- data ResourceTagSetItem = ResourceTagSetItem
--     { rtsiKey :: !ByteString
--     , rtsiValue :: !ByteString
--     } deriving (Show)

-- data ResourceTagSet = ResourceTagSet
--     { rtsItem :: ![ResourceTagSetItem]
--     } deriving (Show)

-- data CreateTags = CreateTags
--     { ctResourcesSet :: !ResourceIdSet
--     , ctTagSet :: !ResourceTagSet
--     } deriving (Show)

-- data CreateTagsResponse = CreateTagsResponse
--     { ctrRequestId :: !ByteString
--     , ctrReturn :: !Boolean
--     } deriving (Show)

-- data TagSetItem = TagSetItem
--     { tsiResourceId :: !(Maybe ByteString)
--     , tsiResource :: !(Maybe ByteString)
--     , tsiKey :: !(Maybe ByteString)
--     , tsiValue :: !(Maybe ByteString)
--     } deriving (Show)

-- data TagSet = TagSet
--     { tsItem :: ![TagSetItem]
--     } deriving (Show)

-- data DescribeTags = DescribeTags
--     { dtFilterSet :: !(Maybe FilterSet)
--     } deriving (Show)

-- data DescribeTagsResponse = DescribeTagsResponse
--     { dtrRequestId :: !ByteString
--     , dtrTagSet :: !TagSet
--     } deriving (Show)

-- data DeleteTagsSetItem = DeleteTagsSetItem
--     { dtsiKey :: !(Maybe ByteString)
--     , dtsiValue :: !(Maybe ByteString)
--     } deriving (Show)

-- data DeleteTagsSet = DeleteTagsSet
--     { dtsItem :: ![DeleteTagsSetItem]
--     } deriving (Show)

-- data DeleteTags = DeleteTags
--     { dtResourcesSet :: !ResourceIdSet
--     , dtTagSet :: !DeleteTagsSet
--     } deriving (Show)

-- data DeleteTagsResponse = DeleteTagsResponse
--     { dtrRequestId :: !ByteString
--     , dtrReturn :: !Boolean
--     } deriving (Show)

-- data ImportInstance = ImportInstance
--     { iiDescription :: !(Maybe ByteString)
--     , iiLaunchSpecification :: !ImportInstanceLaunchSpecification
--     , iiDiskImageSet :: !DiskImageSet
--     , iiKeepPartialImports :: !(Maybe Boolean)
--     , iiPlatform :: !ByteString
--     } deriving (Show)

-- data ImportInstanceResponse = ImportInstanceResponse
--     { iirRequestId :: !ByteString
--     , iirConversionTask :: !ConversionTask
--     } deriving (Show)

-- data ImportInstanceLaunchSpecification = ImportInstanceLaunchSpecification
--     { iilsArchitecture :: !ByteString
--     , iilsGroupSet :: !(Maybe ImportInstanceGroupSet)
--     , iilsUserData :: !(Maybe UserData)
--     , iilsInstance :: !ByteString
--     , iilsPlacement :: !(Maybe InstancePlacement)
--     , iilsMonitoring :: !(Maybe MonitoringInstance)
--     , iilsSubnetId :: !(Maybe ByteString)
--     , iilsInstanceInitiatedShutdownBehavior :: !(Maybe ByteString)
--     , iilsPrivateIpAddress :: !(Maybe ByteString)
--     } deriving (Show)

-- data DiskImageSet = DiskImageSet
--     { disItem :: ![DiskImage]
--     } deriving (Show)

-- data DiskImage = DiskImage
--     { diImage :: !DiskImageDetail
--     , diDescription :: !(Maybe ByteString)
--     , diVolume :: !DiskImageVolume
--     } deriving (Show)

-- data DiskImageDetail = DiskImageDetail
--     { didFormat :: !ByteString
--     , didBytes :: !Long
--     , didImportManifestUrl :: !ByteString
--     } deriving (Show)

-- data DiskImageVolume = DiskImageVolume
--     { divSize :: !Integer
--     } deriving (Show)

-- data ConversionTask = ConversionTask
--     { ctConversionTaskId :: !ByteString
--     , ctExpirationTime :: !(Maybe ByteString)
--   -- <xs:choice>
--     , ctImportVolume :: !(Maybe ImportVolumeTaskDetails)
--     , ctImportInstance :: !(Maybe ImportInstanceTaskDetails)
--   -- </xs:choice>
--     , ctState :: !ByteString
--     , ctStatusMessage :: !(Maybe ByteString)
--     , ctTagSet :: !(Maybe ResourceTagSet)
--     } deriving (Show)

-- data ImportInstanceTaskDetails = ImportInstanceTaskDetails
--     { iitdVolumes :: !ImportInstanceVolumeDetailSet
--     , iitdInstanceId :: !(Maybe ByteString)
--     , iitdPlatform :: !(Maybe ByteString)
--     , iitdDescription :: !(Maybe ByteString)
--     } deriving (Show)

-- data ImportVolumeTaskDetails = ImportVolumeTaskDetails
--     { ivtdBytesConverted :: !Long
--     , ivtdAvailabilityZone :: !ByteString
--     , ivtdDescription :: !(Maybe ByteString)
--     , ivtdImage :: !DiskImageDescription
--     , ivtdVolume :: !DiskImageVolumeDescription
--     } deriving (Show)

-- data ImportInstanceVolumeDetailSet = ImportInstanceVolumeDetailSet
--     { iivdsItem :: ![ImportInstanceVolumeDetailItem]
--     } deriving (Show)

-- data ImportInstanceVolumeDetailItem = ImportInstanceVolumeDetailItem
--     { iivdiBytesConverted :: !Long
--     , iivdiAvailabilityZone :: !ByteString
--     , iivdiImage :: !DiskImageDescription
--     , iivdiDescription :: !(Maybe ByteString)
--     , iivdiVolume :: !DiskImageVolumeDescription
--     , iivdiStatus :: !ByteString
--     , iivdiStatusMessage :: !(Maybe ByteString)
--     } deriving (Show)

-- data DiskImageVolumeDescription = DiskImageVolumeDescription
--     { divdSize :: !Integer
--     , divdId :: !ByteString
--     } deriving (Show)

-- data DiskImageDescription = DiskImageDescription
--     { didFormat :: !ByteString
--     , didSize :: !Long
--     , didImportManifestUrl :: !ByteString
--     , didChecksum :: !(Maybe ByteString)
--     } deriving (Show)

-- data ImportVolume = ImportVolume
--     { ivAvailabilityZone :: !ByteString
--     , ivImage :: !DiskImageDetail
--     , ivDescription :: !(Maybe ByteString)
--     , ivVolume :: !DiskImageVolume
--     } deriving (Show)

-- data ImportVolumeResponse = ImportVolumeResponse
--     { ivrRequestId :: !ByteString
--     , ivrConversionTask :: !ConversionTask
--     } deriving (Show)

-- data DescribeConversionTasks = DescribeConversionTasks
--     { dctConversionTaskIdSet :: !ConversionTaskIdSet
--     } deriving (Show)

-- data DescribeConversionTasksResponse = DescribeConversionTasksResponse
--     { dctrRequestId :: !ByteString
--     , dctrConversionTasks :: !ConversionTaskSet
--     } deriving (Show)

-- data ConversionTaskIdSet = ConversionTaskIdSet
--     { ctisItem :: ![ConversionTaskIdItem]
--     } deriving (Show)

-- data ConversionTaskIdItem = ConversionTaskIdItem
--     { ctiiConversionTaskId :: !ByteString
--     } deriving (Show)

-- data ConversionTaskSet = ConversionTaskSet
--     { ctsItem :: ![ConversionTask]
--     } deriving (Show)

-- data CancelConversionTask = CancelConversionTask
--     { cctConversionTaskId :: !ByteString
--     } deriving (Show)

-- data CancelConversionTaskResponse = CancelConversionTaskResponse
--     { cctrRequestId :: !ByteString
--     , cctrReturn :: !Boolean
--     } deriving (Show)

-- data CreateInstanceExportTask = CreateInstanceExportTask
--     { cietDescription :: !(Maybe ByteString)
--     , cietInstanceId :: !ByteString
--     , cietTargetEnvironment :: !ByteString
--   -- <xs:choice>
--     , cietExportToS3 :: !(Maybe ExportToS3Task)
--   -- </xs:choice>
--     } deriving (Show)

-- data ExportToS3Task = ExportToS3Task
--     { ets3tDiskImageFormat :: !(Maybe ByteString)
--     , ets3tContainerFormat :: !(Maybe ByteString)
--     , ets3tS3Bucket :: !ByteString
--     , ets3tS3Prefix :: !ByteString
--     } deriving (Show)

-- data CreateInstanceExportTaskResponse = CreateInstanceExportTaskResponse
--     { cietrRequestId :: !ByteString
--     , cietrExportTask :: !ExportTaskResponse
--     } deriving (Show)

-- data DescribeExportTasks = DescribeExportTasks
--     { detExportTaskIdSet :: !ExportTaskIdSet
--     } deriving (Show)

-- data ExportTaskIdSet = ExportTaskIdSet
--     { etisItem :: ![ExportTaskId]
--     } deriving (Show)

-- data ExportTaskId = ExportTaskId
--     { etiExportTaskId :: !ByteString
--     } deriving (Show)

-- data DescribeExportTasksResponse = DescribeExportTasksResponse
--     { detrRequestId :: !ByteString
--     , detrExportTaskSet :: !ExportTaskSetResponse
--     } deriving (Show)

-- data ExportTaskSetResponse = ExportTaskSetResponse
--     { etsrItem :: ![ExportTaskResponse]
--     } deriving (Show)

-- data ExportTaskResponse = ExportTaskResponse
--     { etrExportTaskId :: !ByteString
--     , etrDescription :: !(Maybe ByteString)
--     , etrState :: !ByteString
--     , etrStatusMessage :: !(Maybe ByteString)
--   -- <xs:choice>
--     , etrInstanceExport :: !(Maybe InstanceExportTaskResponse)
--   -- </xs:choice>
--   -- <xs:choice>
--     , etrExportToS3 :: !(Maybe ExportToS3TaskResponse)
--   -- </xs:choice>
--     } deriving (Show)

-- data InstanceExportTaskResponse = InstanceExportTaskResponse
--     { ietrInstanceId :: !ByteString
--     , ietrTargetEnvironment :: !(Maybe ByteString)
--     } deriving (Show)

-- data ExportToS3TaskResponse = ExportToS3TaskResponse
--     { ets3trDiskImageFormat :: !ByteString
--     , ets3trContainerFormat :: !(Maybe ByteString)
--     , ets3trS3Bucket :: !ByteString
--     , ets3trS3Key :: !ByteString
--     } deriving (Show)

-- data CancelExportTask = CancelExportTask
--     { cetExportTaskId :: !ByteString
--     } deriving (Show)

-- data CancelExportTaskResponse = CancelExportTaskResponse
--     { cetrRequestId :: !ByteString
--     , cetrReturn :: !Boolean
--     } deriving (Show)

-- data CreateInternetGateway = CreateInternetGateway

-- data InternetGatewayAttachmentSet = InternetGatewayAttachmentSet
--     { igasItem :: ![InternetGatewayAttachment]
--     } deriving (Show)

-- data InternetGatewayAttachment = InternetGatewayAttachment
--     { igaVpcId :: !ByteString
--     , igaState :: !ByteString
--     } deriving (Show)

-- data InternetGateway = InternetGateway
--     { igInternetGatewayId :: !ByteString
--     , igAttachmentSet :: !InternetGatewayAttachmentSet
--     , igTagSet :: !(Maybe ResourceTagSet)
--     } deriving (Show)

-- data CreateInternetGatewayResponse = CreateInternetGatewayResponse
--     { cigrRequestId :: !ByteString
--     , cigrInternetGateway :: !InternetGateway
--     } deriving (Show)

-- data InternetGatewayIdSet = InternetGatewayIdSet
--     { igisItem :: ![InternetGatewayIdSetItem]
--     } deriving (Show)

-- data InternetGatewayIdSetItem = InternetGatewayIdSetItem
--     { igisiInternetGatewayId :: !ByteString
--     } deriving (Show)

-- data DescribeInternetGateways = DescribeInternetGateways
--     { digInternetGatewayIdSet :: !InternetGatewayIdSet
--     , digFilterSet :: !(Maybe FilterSet)
--     } deriving (Show)

-- data InternetGatewaySet = InternetGatewaySet
--     { igsItem :: ![InternetGateway]
--     } deriving (Show)

-- data DescribeInternetGatewaysResponse = DescribeInternetGatewaysResponse
--     { digrRequestId :: !ByteString
--     , digrInternetGatewaySet :: !InternetGatewaySet
--     } deriving (Show)

-- data DeleteInternetGateway = DeleteInternetGateway
--     { digInternetGatewayId :: !ByteString
--     } deriving (Show)

-- data DeleteInternetGatewayResponse = DeleteInternetGatewayResponse
--     { digrRequestId :: !ByteString
--     , digrReturn :: !Boolean
--     } deriving (Show)

-- data AttachInternetGateway = AttachInternetGateway
--     { aigInternetGatewayId :: !ByteString
--     , aigVpcId :: !ByteString
--     } deriving (Show)

-- data AttachInternetGatewayResponse = AttachInternetGatewayResponse
--     { aigrRequestId :: !ByteString
--     , aigrReturn :: !Boolean
--     } deriving (Show)

-- data DetachInternetGateway = DetachInternetGateway
--     { digInternetGatewayId :: !ByteString
--     , digVpcId :: !ByteString
--     } deriving (Show)

-- data DetachInternetGatewayResponse = DetachInternetGatewayResponse
--     { digrRequestId :: !ByteString
--     , digrReturn :: !Boolean
--     } deriving (Show)

-- data CreateRouteTable = CreateRouteTable
--     { crtVpcId :: !ByteString
--     } deriving (Show)

-- data RouteSet = RouteSet
--     { rsItem :: ![Route]
--     } deriving (Show)

-- data Route = Route
--     { rDestinationCidrBlock :: !ByteString
--     , rGatewayId :: !(Maybe ByteString)
--     , rInstanceId :: !(Maybe ByteString)
--     , rInstanceOwnerId :: !(Maybe ByteString)
--     , rNetworkInterfaceId :: !(Maybe ByteString)
--     , rState :: !ByteString
--     , rOrigin :: !ByteString
--     } deriving (Show)

-- data RouteTableAssociationSet = RouteTableAssociationSet
--     { rtasItem :: ![RouteTableAssociation]
--     } deriving (Show)

-- data RouteTableAssociation = RouteTableAssociation
--     { rtaRouteTableAssociationId :: !ByteString
--     , rtaRouteTableId :: !ByteString
--   -- <xs:choice>
--     , rtaSubnetId :: !(Maybe ByteString)
--     , rtaMain :: !(Maybe Boolean)
--   -- </xs:choice>
--     } deriving (Show)

-- data PropagatingVgwSet = PropagatingVgwSet
--     { pvsItem :: ![PropagatingVgw]
--     } deriving (Show)

-- data PropagatingVgw = PropagatingVgw
--     { pvGatewayId :: !ByteString
--     } deriving (Show)

-- data RouteTable = RouteTable
--     { rtRouteTableId :: !ByteString
--     , rtVpcId :: !ByteString
--     , rtRouteSet :: !RouteSet
--     , rtAssociationSet :: !RouteTableAssociationSet
--     , rtPropagatingVgwSet :: !PropagatingVgwSet
--     , rtTagSet :: !(Maybe ResourceTagSet)
--     } deriving (Show)

-- data CreateRouteTableResponse = CreateRouteTableResponse
--     { crtrRequestId :: !ByteString
--     , crtrRouteTable :: !RouteTable
--     } deriving (Show)

-- data RouteTableIdSet = RouteTableIdSet
--     { rtisItem :: ![RouteTableIdSetItem]
--     } deriving (Show)

-- data RouteTableIdSetItem = RouteTableIdSetItem
--     { rtisiRouteTableId :: !ByteString
--     } deriving (Show)

-- data DescribeRouteTables = DescribeRouteTables
--     { drtRouteTableIdSet :: !RouteTableIdSet
--     , drtFilterSet :: !(Maybe FilterSet)
--     } deriving (Show)

-- data RouteTableSet = RouteTableSet
--     { rtsItem :: ![RouteTable]
--     } deriving (Show)

-- data DescribeRouteTablesResponse = DescribeRouteTablesResponse
--     { drtrRequestId :: !ByteString
--     , drtrRouteTableSet :: !RouteTableSet
--     } deriving (Show)

-- data EnableVgwRoutePropagationRequest = EnableVgwRoutePropagationRequest
--     { evrprRouteTableId :: !ByteString
--     , evrprGatewayId :: !ByteString
--     } deriving (Show)

-- data EnableVgwRoutePropagationResponse = EnableVgwRoutePropagationResponse
--     { evrprRequestId :: !ByteString
--     , evrprReturn :: !Boolean
--     } deriving (Show)

-- data DisableVgwRoutePropagationRequest = DisableVgwRoutePropagationRequest
--     { dvrprRouteTableId :: !ByteString
--     , dvrprGatewayId :: !ByteString
--     } deriving (Show)

-- data DisableVgwRoutePropagationResponse = DisableVgwRoutePropagationResponse
--     { dvrprRequestId :: !ByteString
--     , dvrprReturn :: !Boolean
--     } deriving (Show)

-- data DeleteRouteTable = DeleteRouteTable
--     { drtRouteTableId :: !ByteString
--     } deriving (Show)

-- data DeleteRouteTableResponse = DeleteRouteTableResponse
--     { drtrRequestId :: !ByteString
--     , drtrReturn :: !Boolean
--     } deriving (Show)

-- data AssociateRouteTable = AssociateRouteTable
--     { artRouteTableId :: !ByteString
--     , artSubnetId :: !ByteString
--     } deriving (Show)

-- data AssociateRouteTableResponse = AssociateRouteTableResponse
--     { artrRequestId :: !ByteString
--     , artrAssociationId :: !ByteString
--     } deriving (Show)

-- data ReplaceRouteTableAssociation = ReplaceRouteTableAssociation
--     { rrtaAssociationId :: !ByteString
--     , rrtaRouteTableId :: !ByteString
--     } deriving (Show)

-- data ReplaceRouteTableAssociationResponse = ReplaceRouteTableAssociationResponse
--     { rrtarRequestId :: !ByteString
--     , rrtarNewAssociationId :: !ByteString
--     } deriving (Show)

-- data DisassociateRouteTable = DisassociateRouteTable
--     { drtAssociationId :: !ByteString
--     } deriving (Show)

-- data DisassociateRouteTableResponse = DisassociateRouteTableResponse
--     { drtrRequestId :: !ByteString
--     , drtrReturn :: !Boolean
--     } deriving (Show)

-- data CreateRoute = CreateRoute
--     { crRouteTableId :: !ByteString
--     , crDestinationCidrBlock :: !ByteString
--   -- <xs:choice>
--     , crGatewayId :: !(Maybe ByteString)
--     , crInstanceId :: !(Maybe ByteString)
--     , crNetworkInterfaceId :: !(Maybe ByteString)
--   -- </xs:choice>
--     } deriving (Show)

-- data CreateRouteResponse = CreateRouteResponse
--     { crrRequestId :: !ByteString
--     , crrReturn :: !Boolean
--     } deriving (Show)

-- data ReplaceRoute = ReplaceRoute
--     { rrRouteTableId :: !ByteString
--     , rrDestinationCidrBlock :: !ByteString
--   -- <xs:choice>
--     , rrGatewayId :: !(Maybe ByteString)
--     , rrInstanceId :: !(Maybe ByteString)
--     , rrNetworkInterfaceId :: !(Maybe ByteString)
--   -- </xs:choice>
--     } deriving (Show)

-- data ReplaceRouteResponse = ReplaceRouteResponse
--     { rrrRequestId :: !ByteString
--     , rrrReturn :: !Boolean
--     } deriving (Show)

-- data DeleteRoute = DeleteRoute
--     { drRouteTableId :: !ByteString
--     , drDestinationCidrBlock :: !ByteString
--     } deriving (Show)

-- data DeleteRouteResponse = DeleteRouteResponse
--     { drrRequestId :: !ByteString
--     , drrReturn :: !Boolean
--     } deriving (Show)

-- data CreateNetworkAcl = CreateNetworkAcl
--     { cnaVpcId :: !ByteString
--     } deriving (Show)

-- data NetworkAclEntrySet = NetworkAclEntrySet
--     { naesItem :: ![NetworkAclEntry]
--     } deriving (Show)

-- data IcmpCode = IcmpCode
--     { icCode :: !Int
--     , icType :: !Int
--     } deriving (Show)

-- data PortRange = PortRange
--     { prFrom :: !Int
--     , prTo :: !Int
--     } deriving (Show)

-- data NetworkAclEntry = NetworkAclEntry
--     { naeRuleNumber :: !Int
--     , naeProtocol :: !ByteString
--     , naeRuleAction :: !ByteString
--     , naeEgress :: !Boolean
--     , naeCidrBlock :: !ByteString
--     , naeIcmpCode :: !(Maybe IcmpCode)
--     , naePortRange :: !(Maybe PortRange)
--     } deriving (Show)

-- data NetworkAclAssociationSet = NetworkAclAssociationSet
--     { naasItem :: ![NetworkAclAssociation]
--     } deriving (Show)

-- data NetworkAclAssociation = NetworkAclAssociation
--     { naaNetworkAclAssociationId :: !ByteString
--     , naaNetworkAclId :: !ByteString
--     , naaSubnetId :: !ByteString
--     } deriving (Show)

-- data NetworkAcl = NetworkAcl
--     { naNetworkAclId :: !ByteString
--     , naVpcId :: !ByteString
--     , naDefault :: !Boolean
--     , naEntrySet :: !NetworkAclEntrySet
--     , naAssociationSet :: !NetworkAclAssociationSet
--     , naTagSet :: !(Maybe ResourceTagSet)
--     } deriving (Show)

-- data CreateNetworkAclResponse = CreateNetworkAclResponse
--     { cnarRequestId :: !ByteString
--     , cnarNetworkAcl :: !NetworkAcl
--     } deriving (Show)

-- data NetworkAclIdSet = NetworkAclIdSet
--     { naisItem :: ![NetworkAclIdSetItem]
--     } deriving (Show)

-- data NetworkAclIdSetItem = NetworkAclIdSetItem
--     { naisiNetworkAclId :: !ByteString
--     } deriving (Show)

-- data DescribeNetworkAcls = DescribeNetworkAcls
--     { dnaNetworkAclIdSet :: !NetworkAclIdSet
--     , dnaFilterSet :: !(Maybe FilterSet)
--     } deriving (Show)

-- data NetworkAclSet = NetworkAclSet
--     { nasItem :: ![NetworkAcl]
--     } deriving (Show)

-- data DescribeNetworkAclsResponse = DescribeNetworkAclsResponse
--     { dnarRequestId :: !ByteString
--     , dnarNetworkAclSet :: !NetworkAclSet
--     } deriving (Show)

-- data DeleteNetworkAcl = DeleteNetworkAcl
--     { dnaNetworkAclId :: !ByteString
--     } deriving (Show)

-- data DeleteNetworkAclResponse = DeleteNetworkAclResponse
--     { dnarRequestId :: !ByteString
--     , dnarReturn :: !Boolean
--     } deriving (Show)

-- data ReplaceNetworkAclAssociation = ReplaceNetworkAclAssociation
--     { rnaaAssociationId :: !ByteString
--     , rnaaNetworkAclId :: !ByteString
--     } deriving (Show)

-- data ReplaceNetworkAclAssociationResponse = ReplaceNetworkAclAssociationResponse
--     { rnaarRequestId :: !ByteString
--     , rnaarNewAssociationId :: !ByteString
--     } deriving (Show)

-- data CreateNetworkAclEntry = CreateNetworkAclEntry
--     { cnaeNetworkAclId :: !ByteString
--     , cnaeRuleNumber :: !Int
--     , cnaeProtocol :: !ByteString
--     , cnaeRuleAction :: !ByteString
--     , cnaeEgress :: !Boolean
--     , cnaeCidrBlock :: !ByteString
--     , cnaeIcmpCode :: !(Maybe IcmpCode)
--     , cnaePortRange :: !(Maybe PortRange)
--     } deriving (Show)

-- data CreateNetworkAclEntryResponse = CreateNetworkAclEntryResponse
--     { cnaerRequestId :: !ByteString
--     , cnaerReturn :: !Boolean
--     } deriving (Show)

-- data ReplaceNetworkAclEntry = ReplaceNetworkAclEntry
--     { rnaeNetworkAclId :: !ByteString
--     , rnaeRuleNumber :: !Int
--     , rnaeProtocol :: !ByteString
--     , rnaeRuleAction :: !ByteString
--     , rnaeEgress :: !Boolean
--     , rnaeCidrBlock :: !ByteString
--     , rnaeIcmpCode :: !(Maybe IcmpCode)
--     , rnaePortRange :: !(Maybe PortRange)
--     } deriving (Show)

-- data ReplaceNetworkAclEntryResponse = ReplaceNetworkAclEntryResponse
--     { rnaerRequestId :: !ByteString
--     , rnaerReturn :: !Boolean
--     } deriving (Show)

-- data DeleteNetworkAclEntry = DeleteNetworkAclEntry
--     { dnaeNetworkAclId :: !ByteString
--     , dnaeRuleNumber :: !Int
--     , dnaeEgress :: !Boolean
--     } deriving (Show)

-- data DeleteNetworkAclEntryResponse = DeleteNetworkAclEntryResponse
--     { dnaerRequestId :: !ByteString
--     , dnaerReturn :: !Boolean
--     } deriving (Show)

-- data DescribeInstanceStatus = DescribeInstanceStatus
--     { disInstancesSet :: !InstanceIdSet
--     , disFilterSet :: !(Maybe FilterSet)
--     , disNextToken :: !(Maybe ByteString)
--     , disMaxResults :: !(Maybe Int)
--     , disIncludeAllInstances :: !(Maybe Boolean)
--     } deriving (Show)

-- data DescribeInstanceStatusResponse = DescribeInstanceStatusResponse
--     { disrRequestId :: !ByteString
--     , disrInstanceStatusSet :: !InstanceStatusSet
--     , disrNextToken :: !(Maybe ByteString)
--     } deriving (Show)

-- data InstanceStatusSet = InstanceStatusSet
--     { issItem :: ![InstanceStatusItem]
--     } deriving (Show)

-- data InstanceStatus = InstanceStatus
--     { isStatus :: !ByteString
--     , isDetails :: !(Maybe InstanceStatusDetailsSet)
--     } deriving (Show)

-- data InstanceStatusDetailsSet = InstanceStatusDetailsSet
--     { isdsItem :: ![InstanceStatusDetailsSetItem]
--     } deriving (Show)

-- data InstanceStatusDetailsSetItem = InstanceStatusDetailsSetItem
--     { isdsiName :: !ByteString
--     , isdsiStatus :: !ByteString
--     , isdsiImpairedSince :: !(Maybe DateTime)
--     } deriving (Show)

-- data InstanceStatusEvent = InstanceStatusEvent
--     { iseCode :: !ByteString
--     , iseDescription :: !ByteString
--     , iseNotBefore :: !DateTime
--     , iseNotAfter :: !(Maybe DateTime)
--     } deriving (Show)

-- data InstanceStatusEventsSet = InstanceStatusEventsSet
--     { isesItem :: ![InstanceStatusEvent]
--     } deriving (Show)

-- data InstanceStatusItem = InstanceStatusItem
--     { isiInstanceId :: !ByteString
--     , isiAvailabilityZone :: !ByteString
--     , isiEventsSet :: !(Maybe InstanceStatusEventsSet)
--     , isiInstanceState :: !InstanceState
--     , isiSystemStatus :: !InstanceStatus
--     , isiInstanceStatus :: !InstanceStatus
--     } deriving (Show)

-- data ReportInstanceStatus = ReportInstanceStatus
--     { risInstancesSet :: !InstanceIdSet
--     , risStatus :: !ByteString
--     , risStartTime :: !(Maybe DateTime)
--     , risEndTime :: !(Maybe DateTime)
--     , risReasonCodesSet :: !ReportInstanceStatusReasonCodesSet
--     , risDescription :: !(Maybe ByteString)
--     } deriving (Show)

-- data ReportInstanceStatusReasonCodesSet = ReportInstanceStatusReasonCodesSet
--     { risrcsItem :: !(NonEmpty ReportInstanceStatusReasonCodeSetItem)
--     } deriving (Show)

-- data ReportInstanceStatusReasonCodeSetItem = ReportInstanceStatusReasonCodeSetItem
--     { risrcsiReasonCode :: !ByteString
--     } deriving (Show)

-- data ReportInstanceStatusResponse = ReportInstanceStatusResponse
--     { risrRequestId :: !ByteString
--     , risrReturn :: !Boolean
--     } deriving (Show)

-- data CreateNetworkInterface = CreateNetworkInterface
--     { cniSubnetId :: !ByteString
--     , cniDescription :: !(Maybe ByteString)
--     , cniPrivateIpAddress :: !(Maybe ByteString)
--     , cniGroupSet :: !(Maybe SecurityGroupIdSet)
--     , cniPrivateIpAddressesSet :: !(Maybe PrivateIpAddressesSetRequest)
--     , cniSecondaryPrivateIpAddressCount :: !(Maybe Int)
--     } deriving (Show)

-- data CreateNetworkInterfaceResponse = CreateNetworkInterfaceResponse
--     { cnirRequestId :: !ByteString
--     , cnirNetworkInterface :: !NetworkInterface
--     } deriving (Show)

-- data NetworkInterfaceIdSet = NetworkInterfaceIdSet
--     { niisItem :: ![NetworkInterfaceIdSetItem]
--     } deriving (Show)

-- data NetworkInterfaceIdSetItem = NetworkInterfaceIdSetItem
--     { niisiNetworkInterfaceId :: !ByteString
--     } deriving (Show)

-- data DescribeNetworkInterfaces = DescribeNetworkInterfaces
--     { dniNetworkInterfaceIdSet :: !(Maybe NetworkInterfaceIdSet)
--     , dniFilterSet :: !(Maybe FilterSet)
--     } deriving (Show)

-- data NetworkInterface = NetworkInterface
--     { niNetworkInterfaceId :: !ByteString
--     , niSubnetId :: !(Maybe ByteString)
--     , niVpcId :: !(Maybe ByteString)
--     , niAvailabilityZone :: !(Maybe ByteString)
--     , niDescription :: !(Maybe ByteString)
--     , niOwnerId :: !ByteString
--     , niRequesterId :: !(Maybe ByteString)
--     , niRequesterManaged :: !(Maybe Boolean)
--     , niStatus :: !ByteString
--     , niMacAddress :: !ByteString
--     , niPrivateIpAddress :: !ByteString
--     , niPrivateDnsName :: !(Maybe ByteString)
--     , niSourceDestCheck :: !Boolean
--     , niGroupSet :: !GroupSet
--     , niAttachment :: !(Maybe NetworkInterfaceAttachment)
--     , niAssociation :: !(Maybe NetworkInterfaceAssociation)
--     , niTagSet :: !(Maybe ResourceTagSet)
--     , niPrivateIpAddressesSet :: !(Maybe NetworkInterfacePrivateIpAddressesSet)
--     } deriving (Show)

-- data NetworkInterfacePrivateIpAddressesSet = NetworkInterfacePrivateIpAddressesSet
--     { nipiasItem :: ![NetworkInterfacePrivateIpAddressesSetItem]
--     } deriving (Show)

-- data NetworkInterfacePrivateIpAddressesSetItem = NetworkInterfacePrivateIpAddressesSetItem
--     { nipiasiPrivateIpAddress :: !ByteString
--     , nipiasiPrivateDnsName :: !(Maybe ByteString)
--     , nipiasiPrimary :: !Boolean
--     , nipiasiAssociation :: !(Maybe NetworkInterfaceAssociation)
--     } deriving (Show)

-- data NetworkInterfaceAttachment = NetworkInterfaceAttachment
--     { niaAttachmentId :: !ByteString
--     , niaInstanceId :: !(Maybe ByteString)
--     , niaInstanceOwnerId :: !(Maybe ByteString)
--     , niaDeviceIndex :: !Int
--     , niaStatus :: !ByteString
--     , niaAttachTime :: !DateTime
--     , niaDeleteOnTermination :: !Boolean
--     } deriving (Show)

-- data NetworkInterfaceAssociation = NetworkInterfaceAssociation
--     { niaPublicIp :: !ByteString
--     , niaPublicDnsName :: !(Maybe ByteString)
--     , niaIpOwnerId :: !(Maybe ByteString)
--     , niaAllocationId :: !(Maybe ByteString)
--     , niaAssociationId :: !(Maybe ByteString)
--     } deriving (Show)

-- data NetworkInterfaceSet = NetworkInterfaceSet
--     { nisItem :: ![NetworkInterface]
--     } deriving (Show)

-- data DescribeNetworkInterfacesResponse = DescribeNetworkInterfacesResponse
--     { dnirRequestId :: !ByteString
--     , dnirNetworkInterfaceSet :: !NetworkInterfaceSet
--     } deriving (Show)

-- data DeleteNetworkInterface = DeleteNetworkInterface
--     { dniNetworkInterfaceId :: !ByteString
--     } deriving (Show)

-- data DeleteNetworkInterfaceResponse = DeleteNetworkInterfaceResponse
--     { dnirRequestId :: !ByteString
--     , dnirReturn :: !Boolean
--     } deriving (Show)

-- data AttachNetworkInterface = AttachNetworkInterface
--     { aniNetworkInterfaceId :: !ByteString
--     , aniInstanceId :: !ByteString
--     , aniDeviceIndex :: !Int
--     } deriving (Show)

-- data AttachNetworkInterfaceResponse = AttachNetworkInterfaceResponse
--     { anirRequestId :: !ByteString
--     , anirAttachmentId :: !ByteString
--     } deriving (Show)

-- data DetachNetworkInterface = DetachNetworkInterface
--     { dniAttachmentId :: !ByteString
--     , dniForce :: !(Maybe Boolean)
--     } deriving (Show)

-- data DetachNetworkInterfaceResponse = DetachNetworkInterfaceResponse
--     { dnirRequestId :: !ByteString
--     , dnirReturn :: !Boolean
--     } deriving (Show)

-- data DescribeNetworkInterfaceAttribute = DescribeNetworkInterfaceAttribute
--     { dniaNetworkInterfaceId :: !ByteString
--     , dniaDescribeNetworkInterfaceAttributesGroup :: !DescribeNetworkInterfaceAttributesGroup
--     } deriving (Show)

-- data DescribeNetworkInterfaceAttributesGroup = DescribeNetworkInterfaceAttributesGroup
--     { dniagDescription :: !EmptyElement
--     , dniagSourceDestCheck :: !EmptyElement
--     , dniagGroupSet :: !EmptyElement
--     , dniagAttachment :: !EmptyElement
--     } deriving (Show)
-- data DescribeNetworkInterfaceAttributeResponse = DescribeNetworkInterfaceAttributeResponse
--     { dniarRequestId :: !ByteString
--     , dniarNetworkInterfaceId :: !ByteString
--   -- <xs:choice>
--     , dniarDescription :: !(Maybe NullableAttributeValue)
--     , dniarSourceDestCheck :: !(Maybe AttributeBooleanValue)
--     , dniarGroupSet :: !(Maybe GroupSet)
--     , dniarAttachment :: !(Maybe NetworkInterfaceAttachment)
--   -- </xs:choice>
--     } deriving (Show)

-- data ModifyNetworkInterfaceAttribute = ModifyNetworkInterfaceAttribute
--     { mniaNetworkInterfaceId :: !ByteString
--   -- <xs:choice>
--     , mniaDescription :: !(Maybe NullableAttributeValue)
--     , mniaSourceDestCheck :: !(Maybe AttributeBooleanValue)
--     , mniaGroupSet :: !(Maybe SecurityGroupIdSet)
--     , mniaAttachment :: !(Maybe ModifyNetworkInterfaceAttachment)
--   -- </xs:choice>
--     } deriving (Show)

-- data ModifyNetworkInterfaceAttachment = ModifyNetworkInterfaceAttachment
--     { mniaAttachmentId :: !ByteString
--     , mniaDeleteOnTermination :: !Boolean
--     } deriving (Show)

-- data ModifyNetworkInterfaceAttributeResponse = ModifyNetworkInterfaceAttributeResponse
--     { mniarRequestId :: !ByteString
--     , mniarReturn :: !Boolean
--     } deriving (Show)

-- data ResetNetworkInterfaceAttribute = ResetNetworkInterfaceAttribute
--     { rniaNetworkInterfaceId :: !ByteString
--     , rniaResetNetworkInterfaceAttributesGroup :: !ResetNetworkInterfaceAttributesGroup
--     } deriving (Show)

-- data ResetNetworkInterfaceAttributesGroup = ResetNetworkInterfaceAttributesGroup
--     { rniagSourceDestCheck :: !EmptyElement
--     } deriving (Show)
-- data ResetNetworkInterfaceAttributeResponse = ResetNetworkInterfaceAttributeResponse
--     { rniarRequestId :: !ByteString
--     , rniarReturn :: !Boolean
--     } deriving (Show)

-- data AssignPrivateIpAddresses = AssignPrivateIpAddresses
--     { apiaNetworkInterfaceId :: !ByteString
--     , apiaPrivateIpAddressesSet :: !(Maybe AssignPrivateIpAddressesSetRequest)
--     , apiaSecondaryPrivateIpAddressCount :: !(Maybe Int)
--     , apiaAllowReassignment :: !(Maybe Boolean)
--     } deriving (Show)

-- data AssignPrivateIpAddressesResponse = AssignPrivateIpAddressesResponse
--     { apiarRequestId :: !ByteString
--     , apiarReturn :: !Boolean
--     } deriving (Show)

-- data UnassignPrivateIpAddresses = UnassignPrivateIpAddresses
--     { upiaNetworkInterfaceId :: !ByteString
--     , upiaPrivateIpAddressesSet :: !AssignPrivateIpAddressesSetRequest
--     } deriving (Show)

-- data UnassignPrivateIpAddressesResponse = UnassignPrivateIpAddressesResponse
--     { upiarRequestId :: !ByteString
--     , upiarReturn :: !Boolean
--     } deriving (Show)

-- data AssignPrivateIpAddressesSetRequest = AssignPrivateIpAddressesSetRequest
--     { apiasrItem :: !(NonEmpty AssignPrivateIpAddressesSetItemRequest)
--     } deriving (Show)

-- data AssignPrivateIpAddressesSetItemRequest = AssignPrivateIpAddressesSetItemRequest
--     { apiasirPrivateIpAddress :: !ByteString
--     } deriving (Show)

-- data DescribeVolumeStatus = DescribeVolumeStatus
--     { dvsVolumeSet :: !DescribeVolumesSet
--     , dvsFilterSet :: !(Maybe FilterSet)
--     , dvsMaxResults :: !(Maybe Integer)
--     , dvsNextToken :: !(Maybe ByteString)
--     } deriving (Show)

-- data DescribeVolumeStatusResponse = DescribeVolumeStatusResponse
--     { dvsrRequestId :: !ByteString
--     , dvsrVolumeStatusSet :: !VolumeStatusSet
--     , dvsrNextToken :: !(Maybe ByteString)
--     } deriving (Show)

-- data VolumeStatusSet = VolumeStatusSet
--     { vssItem :: ![VolumeStatusItem]
--     } deriving (Show)

-- data VolumeStatusItem = VolumeStatusItem
--     { vsiVolumeId :: !ByteString
--     , vsiAvailabilityZone :: !ByteString
--     , vsiVolumeStatus :: !VolumeStatusInfo
--     , vsiEventsSet :: !VolumeStatusEventsSet
--     , vsiActionsSet :: !VolumeStatusActionsSet
--     } deriving (Show)

-- data VolumeStatusInfo = VolumeStatusInfo
--     { vsiStatus :: !ByteString
--     , vsiDetails :: !VolumeStatusDetailsSet
--     } deriving (Show)

-- data VolumeStatusDetailsSet = VolumeStatusDetailsSet
--     { vsdsItem :: ![VolumeStatusDetailsItem]
--     } deriving (Show)

-- data VolumeStatusDetailsItem = VolumeStatusDetailsItem
--     { vsdiName :: !ByteString
--     , vsdiStatus :: !ByteString
--     } deriving (Show)

-- data VolumeStatusEventsSet = VolumeStatusEventsSet
--     { vsesItem :: ![VolumeStatusEventItem]
--     } deriving (Show)

-- data VolumeStatusEventItem = VolumeStatusEventItem
--     { vseiDescription :: !ByteString
--     , vseiNotBefore :: !DateTime
--     , vseiNotAfter :: !DateTime
--     , vseiEventId :: !ByteString
--     , vseiEvent :: !ByteString
--     } deriving (Show)

-- data VolumeStatusActionsSet = VolumeStatusActionsSet
--     { vsasItem :: ![VolumeStatusActionItem]
--     } deriving (Show)

-- data VolumeStatusActionItem = VolumeStatusActionItem
--     { vsaiDescription :: !ByteString
--     , vsaiCode :: !ByteString
--     , vsaiEventId :: !ByteString
--     , vsaiEvent :: !ByteString
--     } deriving (Show)

-- data EnableVolumeIO = EnableVolumeIO
--     { evioVolumeId :: !ByteString
--     } deriving (Show)

-- data EnableVolumeIOResponse = EnableVolumeIOResponse
--     { eviorRequestId :: !ByteString
--     , eviorReturn :: !Boolean
--     } deriving (Show)

-- data ModifyVolumeAttribute = ModifyVolumeAttribute
--     { mvaVolumeId :: !ByteString
--   -- <xs:choice>
--     , mvaAutoEnableIO :: !AttributeBooleanValue
--   -- </xs:choice>
--     } deriving (Show)

-- data ModifyVolumeAttributeResponse = ModifyVolumeAttributeResponse
--     { mvarRequestId :: !ByteString
--     , mvarReturn :: !Boolean
--     } deriving (Show)

-- data DescribeVolumeAttribute = DescribeVolumeAttribute
--     { dvaVolumeId :: !ByteString
--     , dvaDescribeVolumeAttributesGroup :: !DescribeVolumeAttributesGroup
--     } deriving (Show)

-- data DescribeVolumeAttributesGroup = DescribeVolumeAttributesGroup
--     { dvagAutoEnableIO :: !EmptyElement
--     , dvagProductCodes :: !EmptyElement
--     } deriving (Show)

-- data DescribeVolumeAttributeResponse = DescribeVolumeAttributeResponse
--     { dvarRequestId :: !ByteString
--     , dvarVolumeId :: !ByteString
--   -- <xs:choice>
--     , dvarAutoEnableIO :: !(Maybe NullableAttributeBooleanValue)
--     , dvarProductCodes :: !(Maybe ProductCodesSet)
--   -- </xs:choice>
--     } deriving (Show)
