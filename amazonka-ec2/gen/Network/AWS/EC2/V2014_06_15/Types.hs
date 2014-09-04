{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon Elastic Compute Cloud (Amazon EC2) is a web service that provides
-- resizable compute capacity in the cloud. It is designed to make web-scale
-- computing easier for developers. Amazon EC2’s simple web service interface
-- allows you to obtain and configure capacity with minimal friction. It
-- provides you with complete control of your computing resources and lets you
-- run on Amazon’s proven computing environment. Amazon EC2 reduces the time
-- required to obtain and boot new server instances to minutes, allowing you
-- to quickly scale capacity, both up and down, as your computing requirements
-- change. Amazon EC2 changes the economics of computing by allowing you to
-- pay only for capacity that you actually use. Amazon EC2 provides developers
-- the tools to build failure resilient applications and isolate themselves
-- from common failure scenarios.
module Network.AWS.EC2.V2014_06_15.Types
    (
    -- * Service
      EC2
    -- ** Errors
    , Er (..)
    -- ** XML
    , xmlOptions

    -- * AccountAttributeName
    , AccountAttributeName (..)

    -- * ArchitectureValues
    , ArchitectureValues (..)

    -- * AttachmentStatus
    , AttachmentStatus (..)

    -- * AvailabilityZoneState
    , AvailabilityZoneState (..)

    -- * BundleTaskState
    , BundleTaskState (..)

    -- * CancelSpotInstanceRequestState
    , CancelSpotInstanceRequestState (..)

    -- * ContainerFormat
    , ContainerFormat (..)

    -- * ConversionTaskState
    , ConversionTaskState (..)

    -- * CurrencyCodeValues
    , CurrencyCodeValues (..)

    -- * DatafeedSubscriptionState
    , DatafeedSubscriptionState (..)

    -- * DeviceType
    , DeviceType (..)

    -- * DiskImageFormat
    , DiskImageFormat (..)

    -- * DomainType
    , DomainType (..)

    -- * EventCode
    , EventCode (..)

    -- * ExportEnvironment
    , ExportEnvironment (..)

    -- * ExportTaskState
    , ExportTaskState (..)

    -- * GatewayType
    , GatewayType (..)

    -- * HypervisorType
    , HypervisorType (..)

    -- * ImageAttributeName
    , ImageAttributeName (..)

    -- * ImageState
    , ImageState (..)

    -- * ImageTypeValues
    , ImageTypeValues (..)

    -- * InstanceAttributeName
    , InstanceAttributeName (..)

    -- * InstanceLifecycleType
    , InstanceLifecycleType (..)

    -- * InstanceStateName
    , InstanceStateName (..)

    -- * InstanceType
    , InstanceType (..)

    -- * ListingState
    , ListingState (..)

    -- * ListingStatus
    , ListingStatus (..)

    -- * MonitoringState
    , MonitoringState (..)

    -- * NetworkInterfaceAttribute
    , NetworkInterfaceAttribute (..)

    -- * NetworkInterfaceStatus
    , NetworkInterfaceStatus (..)

    -- * OfferingTypeValues
    , OfferingTypeValues (..)

    -- * PermissionGroup
    , PermissionGroup (..)

    -- * PlacementGroupState
    , PlacementGroupState (..)

    -- * PlacementStrategy
    , PlacementStrategy (..)

    -- * PlatformValues
    , PlatformValues (..)

    -- * ProductCodeValues
    , ProductCodeValues (..)

    -- * RIProductDescription
    , RIProductDescription (..)

    -- * RecurringChargeFrequency
    , RecurringChargeFrequency (..)

    -- * ReportInstanceReasonCodes
    , ReportInstanceReasonCodes (..)

    -- * ReportStatusType
    , ReportStatusType (..)

    -- * ReservedInstanceState
    , ReservedInstanceState (..)

    -- * ResetImageAttributeName
    , ResetImageAttributeName (..)

    -- * ResourceType
    , ResourceType (..)

    -- * RouteOrigin
    , RouteOrigin (..)

    -- * RouteState
    , RouteState (..)

    -- * RuleAction
    , RuleAction (..)

    -- * ShutdownBehavior
    , ShutdownBehavior (..)

    -- * SnapshotAttributeName
    , SnapshotAttributeName (..)

    -- * SnapshotState
    , SnapshotState (..)

    -- * SpotInstanceState
    , SpotInstanceState (..)

    -- * SpotInstanceType
    , SpotInstanceType (..)

    -- * StatusName
    , StatusName (..)

    -- * StatusType
    , StatusType (..)

    -- * SubnetState
    , SubnetState (..)

    -- * SummaryStatus
    , SummaryStatus (..)

    -- * TelemetryStatus
    , TelemetryStatus (..)

    -- * Tenancy
    , Tenancy (..)

    -- * VirtualizationType
    , VirtualizationType (..)

    -- * VolumeAttachmentState
    , VolumeAttachmentState (..)

    -- * VolumeAttributeName
    , VolumeAttributeName (..)

    -- * VolumeState
    , VolumeState (..)

    -- * VolumeStatusInfoStatus
    , VolumeStatusInfoStatus (..)

    -- * VolumeStatusName
    , VolumeStatusName (..)

    -- * VolumeType
    , VolumeType (..)

    -- * VpcAttributeName
    , VpcAttributeName (..)

    -- * VpcState
    , VpcState (..)

    -- * VpnState
    , VpnState (..)

    -- * VpnStaticRouteSource
    , VpnStaticRouteSource (..)

    -- * AccountAttributeValue
    , AccountAttributeValue
    , mkAccountAttributeValue
    , aavAttributeValue

    -- * AttributeBooleanValue
    , AttributeBooleanValue
    , mkAttributeBooleanValue
    , abvValue

    -- * AttributeValue
    , AttributeValue
    , mkAttributeValue
    , axValue

    -- * AvailabilityZoneMessage
    , AvailabilityZoneMessage
    , mkAvailabilityZoneMessage
    , azmMessage

    -- * IpRange
    , IpRange
    , mkIpRange
    , iuCidrIp

    -- * Monitoring
    , Monitoring
    , mkMonitoring
    , mgState

    -- * PropagatingVgw
    , PropagatingVgw
    , mkPropagatingVgw
    , pwGatewayId

    -- * ReservedInstancesId
    , ReservedInstancesId
    , mkReservedInstancesId
    , rijReservedInstancesId

    -- * RunInstancesMonitoringEnabled
    , RunInstancesMonitoringEnabled
    , mkRunInstancesMonitoringEnabled
    , rimeEnabled

    -- * Storage
    , Storage
    , mkStorage
    , seS3

    -- * VolumeDetail
    , VolumeDetail
    , mkVolumeDetail
    , vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvxSize

    -- * VpnConnectionOptions
    , VpnConnectionOptions
    , mkVpnConnectionOptions
    , vcoStaticRoutesOnly

    -- * VpnConnectionOptionsSpecification
    , VpnConnectionOptionsSpecification
    , mkVpnConnectionOptionsSpecification
    , vcosStaticRoutesOnly

    -- * AccountAttribute
    , AccountAttribute
    , aaAttributeName
    , aaAttributeValues

    -- * Address
    , Address
    , awInstanceId
    , awPublicIp
    , awAllocationId
    , awAssociationId
    , awDomain
    , awNetworkInterfaceId
    , awNetworkInterfaceOwnerId
    , awPrivateIpAddress

    -- * AvailabilityZone
    , AvailabilityZone
    , azZoneName
    , azState
    , azRegionName
    , azMessages

    -- * BlockDeviceMapping
    , BlockDeviceMapping
    , mkBlockDeviceMapping
    , bdmVirtualName
    , bdmDeviceName
    , bdmEbs
    , bdmNoDevice

    -- * BundleTask
    , BundleTask
    , btInstanceId
    , btBundleId
    , btState
    , btStartTime
    , btUpdateTime
    , btStorage
    , btProgress
    , btBundleTaskError

    -- * BundleTaskError
    , BundleTaskError
    , mkBundleTaskError
    , bteCode
    , bteMessage

    -- * CancelledSpotInstanceRequest
    , CancelledSpotInstanceRequest
    , csirSpotInstanceRequestId
    , csirState

    -- * ConversionTask
    , ConversionTask
    , ctConversionTaskId
    , ctExpirationTime
    , ctImportInstance
    , ctImportVolume
    , ctState
    , ctStatusMessage
    , ctTags

    -- * CreateVolumePermission
    , CreateVolumePermission
    , mkCreateVolumePermission
    , cvpUserId
    , cvpGroup

    -- * CreateVolumePermissionModifications
    , CreateVolumePermissionModifications
    , mkCreateVolumePermissionModifications
    , cvpmAdd
    , cvpmRemove

    -- * CustomerGateway
    , CustomerGateway
    , cgCustomerGatewayId
    , cgState
    , cgType
    , cgIpAddress
    , cgBgpAsn
    , cgTags

    -- * DhcpConfiguration
    , DhcpConfiguration
    , mkDhcpConfiguration
    , dcKey
    , dcValues

    -- * DhcpOptions
    , DhcpOptions
    , doDhcpOptionsId
    , doDhcpConfigurations
    , doTags

    -- * DiskImage
    , DiskImage
    , mkDiskImage
    , dmImage
    , dmDescription
    , dmVolume

    -- * DiskImageDescription
    , DiskImageDescription
    , mkDiskImageDescription
    , didFormat
    , didSize
    , didImportManifestUrl
    , didChecksum

    -- * DiskImageDetail
    , DiskImageDetail
    , mkDiskImageDetail
    , dikFormat
    , dikBytes
    , dikImportManifestUrl

    -- * DiskImageVolumeDescription
    , DiskImageVolumeDescription
    , mkDiskImageVolumeDescription
    , divdSize
    , divdId

    -- * EbsBlockDevice
    , EbsBlockDevice
    , mkEbsBlockDevice
    , ebdSnapshotId
    , ebdVolumeSize
    , ebdDeleteOnTermination
    , ebdVolumeType
    , ebdIops
    , ebdEncrypted

    -- * EbsInstanceBlockDevice
    , EbsInstanceBlockDevice
    , mkEbsInstanceBlockDevice
    , eibdVolumeId
    , eibdStatus
    , eibdAttachTime
    , eibdDeleteOnTermination

    -- * EbsInstanceBlockDeviceSpecification
    , EbsInstanceBlockDeviceSpecification
    , mkEbsInstanceBlockDeviceSpecification
    , eibdsVolumeId
    , eibdsDeleteOnTermination

    -- * ExportTask
    , ExportTask
    , etExportTaskId
    , etDescription
    , etState
    , etStatusMessage
    , etInstanceExportDetails
    , etExportToS3Task

    -- * ExportToS3Task
    , ExportToS3Task
    , mkExportToS3Task
    , etstDiskImageFormat
    , etstContainerFormat
    , etstS3Bucket
    , etstS3Key

    -- * ExportToS3TaskSpecification
    , ExportToS3TaskSpecification
    , mkExportToS3TaskSpecification
    , etstsDiskImageFormat
    , etstsContainerFormat
    , etstsS3Bucket
    , etstsS3Prefix

    -- * Filter
    , Filter
    , mkFilter
    , frName
    , frValues

    -- * GroupIdentifier
    , GroupIdentifier
    , mkGroupIdentifier
    , giGroupName
    , giGroupId

    -- * IamInstanceProfile
    , IamInstanceProfile
    , mkIamInstanceProfile
    , iipArn
    , iipId

    -- * IamInstanceProfileSpecification
    , IamInstanceProfileSpecification
    , mkIamInstanceProfileSpecification
    , iipsArn
    , iipsName

    -- * IcmpTypeCode
    , IcmpTypeCode
    , mkIcmpTypeCode
    , itcType
    , itcCode

    -- * Image
    , Image
    , ieImageId
    , ieImageLocation
    , ieState
    , ieOwnerId
    , iePublic
    , ieProductCodes
    , ieArchitecture
    , ieImageType
    , ieKernelId
    , ieRamdiskId
    , iePlatform
    , ieSriovNetSupport
    , ieStateReason
    , ieImageOwnerAlias
    , ieName
    , ieDescription
    , ieRootDeviceType
    , ieRootDeviceName
    , ieBlockDeviceMappings
    , ieVirtualizationType
    , ieTags
    , ieHypervisor

    -- * ImportInstanceLaunchSpecification
    , ImportInstanceLaunchSpecification
    , mkImportInstanceLaunchSpecification
    , iilsArchitecture
    , iilsGroupNames
    , iilsAdditionalInfo
    , iilsUserData
    , iilsInstanceType
    , iilsPlacement
    , iilsMonitoring
    , iilsSubnetId
    , iilsInstanceInitiatedShutdownBehavior
    , iilsPrivateIpAddress

    -- * ImportInstanceTaskDetails
    , ImportInstanceTaskDetails
    , mkImportInstanceTaskDetails
    , iitdVolumes
    , iitdInstanceId
    , iitdPlatform
    , iitdDescription

    -- * ImportInstanceVolumeDetailItem
    , ImportInstanceVolumeDetailItem
    , mkImportInstanceVolumeDetailItem
    , iivdiBytesConverted
    , iivdiAvailabilityZone
    , iivdiImage
    , iivdiVolume
    , iivdiStatus
    , iivdiStatusMessage
    , iivdiDescription

    -- * ImportVolumeTaskDetails
    , ImportVolumeTaskDetails
    , mkImportVolumeTaskDetails
    , ivtdBytesConverted
    , ivtdAvailabilityZone
    , ivtdDescription
    , ivtdImage
    , ivtdVolume

    -- * Instance
    , Instance
    , mkInstance
    , ifInstanceId
    , ifImageId
    , ifState
    , ifPrivateDnsName
    , ifPublicDnsName
    , ifStateTransitionReason
    , ifKeyName
    , ifAmiLaunchIndex
    , ifProductCodes
    , ifInstanceType
    , ifLaunchTime
    , ifPlacement
    , ifKernelId
    , ifRamdiskId
    , ifPlatform
    , ifMonitoring
    , ifSubnetId
    , ifVpcId
    , ifPrivateIpAddress
    , ifPublicIpAddress
    , ifStateReason
    , ifArchitecture
    , ifRootDeviceType
    , ifRootDeviceName
    , ifBlockDeviceMappings
    , ifVirtualizationType
    , ifInstanceLifecycle
    , ifSpotInstanceRequestId
    , ifClientToken
    , ifTags
    , ifSecurityGroups
    , ifSourceDestCheck
    , ifHypervisor
    , ifNetworkInterfaces
    , ifIamInstanceProfile
    , ifEbsOptimized
    , ifSriovNetSupport

    -- * InstanceBlockDeviceMapping
    , InstanceBlockDeviceMapping
    , mkInstanceBlockDeviceMapping
    , ibdmDeviceName
    , ibdmEbs

    -- * InstanceBlockDeviceMappingSpecification
    , InstanceBlockDeviceMappingSpecification
    , mkInstanceBlockDeviceMappingSpecification
    , ibdmsDeviceName
    , ibdmsEbs
    , ibdmsVirtualName
    , ibdmsNoDevice

    -- * InstanceCount
    , InstanceCount
    , mkInstanceCount
    , icState
    , icInstanceCount

    -- * InstanceExportDetails
    , InstanceExportDetails
    , mkInstanceExportDetails
    , iedInstanceId
    , iedTargetEnvironment

    -- * InstanceMonitoring
    , InstanceMonitoring
    , inInstanceId
    , inMonitoring

    -- * InstanceNetworkInterface
    , InstanceNetworkInterface
    , mkInstanceNetworkInterface
    , iniNetworkInterfaceId
    , iniSubnetId
    , iniVpcId
    , iniDescription
    , iniOwnerId
    , iniStatus
    , iniPrivateIpAddress
    , iniPrivateDnsName
    , iniSourceDestCheck
    , iniGroups
    , iniAttachment
    , iniAssociation
    , iniPrivateIpAddresses

    -- * InstanceNetworkInterfaceAssociation
    , InstanceNetworkInterfaceAssociation
    , mkInstanceNetworkInterfaceAssociation
    , inibPublicIp
    , inibPublicDnsName
    , inibIpOwnerId

    -- * InstanceNetworkInterfaceAttachment
    , InstanceNetworkInterfaceAttachment
    , mkInstanceNetworkInterfaceAttachment
    , iniaAttachmentId
    , iniaDeviceIndex
    , iniaStatus
    , iniaAttachTime
    , iniaDeleteOnTermination

    -- * InstanceNetworkInterfaceSpecification
    , InstanceNetworkInterfaceSpecification
    , mkInstanceNetworkInterfaceSpecification
    , inisNetworkInterfaceId
    , inisDeviceIndex
    , inisSubnetId
    , inisDescription
    , inisPrivateIpAddress
    , inisGroups
    , inisDeleteOnTermination
    , inisPrivateIpAddresses
    , inisSecondaryPrivateIpAddressCount
    , inisAssociatePublicIpAddress

    -- * InstancePrivateIpAddress
    , InstancePrivateIpAddress
    , mkInstancePrivateIpAddress
    , ipiaPrivateIpAddress
    , ipiaPrivateDnsName
    , ipiaPrimary
    , ipiaAssociation

    -- * InstanceState
    , InstanceState
    , mkInstanceState
    , iifCode
    , iifName

    -- * InstanceStateChange
    , InstanceStateChange
    , iscInstanceId
    , iscCurrentState
    , iscPreviousState

    -- * InstanceStatus
    , InstanceStatus
    , iiiiivInstanceId
    , iiiiivAvailabilityZone
    , iiiiivEvents
    , iiiiivInstanceState
    , iiiiivSystemStatus
    , iiiiivInstanceStatus

    -- * InstanceStatusDetails
    , InstanceStatusDetails
    , mkInstanceStatusDetails
    , isdName
    , isdStatus
    , isdImpairedSince

    -- * InstanceStatusEvent
    , InstanceStatusEvent
    , mkInstanceStatusEvent
    , iseCode
    , iseDescription
    , iseNotBefore
    , iseNotAfter

    -- * InstanceStatusSummary
    , InstanceStatusSummary
    , mkInstanceStatusSummary
    , issStatus
    , issDetails

    -- * InternetGateway
    , InternetGateway
    , igInternetGatewayId
    , igAttachments
    , igTags

    -- * InternetGatewayAttachment
    , InternetGatewayAttachment
    , mkInternetGatewayAttachment
    , igaVpcId
    , igaState

    -- * IpPermission
    , IpPermission
    , mkIpPermission
    , ipIpProtocol
    , ipFromPort
    , ipToPort
    , ipUserIdGroupPairs
    , ipIpRanges

    -- * KeyPairInfo
    , KeyPairInfo
    , kpiKeyName
    , kpiKeyFingerprint

    -- * LaunchPermission
    , LaunchPermission
    , mkLaunchPermission
    , lpUserId
    , lpGroup

    -- * LaunchPermissionModifications
    , LaunchPermissionModifications
    , mkLaunchPermissionModifications
    , lpmAdd
    , lpmRemove

    -- * LaunchSpecification
    , LaunchSpecification
    , mkLaunchSpecification
    , llnImageId
    , llnKeyName
    , llnSecurityGroups
    , llnUserData
    , llnAddressingType
    , llnInstanceType
    , llnPlacement
    , llnKernelId
    , llnRamdiskId
    , llnBlockDeviceMappings
    , llnMonitoringEnabled
    , llnSubnetId
    , llnNetworkInterfaces
    , llnIamInstanceProfile
    , llnEbsOptimized

    -- * NetworkAcl
    , NetworkAcl
    , naNetworkAclId
    , naVpcId
    , naIsDefault
    , naEntries
    , naAssociations
    , naTags

    -- * NetworkAclAssociation
    , NetworkAclAssociation
    , mkNetworkAclAssociation
    , naaNetworkAclAssociationId
    , naaNetworkAclId
    , naaSubnetId

    -- * NetworkAclEntry
    , NetworkAclEntry
    , mkNetworkAclEntry
    , naeRuleNumber
    , naeProtocol
    , naeRuleAction
    , naeEgress
    , naeCidrBlock
    , naeIcmpTypeCode
    , naePortRange

    -- * NetworkInterface
    , NetworkInterface
    , niNetworkInterfaceId
    , niSubnetId
    , niVpcId
    , niAvailabilityZone
    , niDescription
    , niOwnerId
    , niRequesterId
    , niRequesterManaged
    , niStatus
    , niMacAddress
    , niPrivateIpAddress
    , niPrivateDnsName
    , niSourceDestCheck
    , niGroups
    , niAttachment
    , niAssociation
    , niTagSet
    , niPrivateIpAddresses

    -- * NetworkInterfaceAssociation
    , NetworkInterfaceAssociation
    , mkNetworkInterfaceAssociation
    , nibPublicIp
    , nibPublicDnsName
    , nibIpOwnerId
    , nibAllocationId
    , nibAssociationId

    -- * NetworkInterfaceAttachment
    , NetworkInterfaceAttachment
    , mkNetworkInterfaceAttachment
    , niaAttachmentId
    , niaInstanceId
    , niaInstanceOwnerId
    , niaDeviceIndex
    , niaStatus
    , niaAttachTime
    , niaDeleteOnTermination

    -- * NetworkInterfaceAttachmentChanges
    , NetworkInterfaceAttachmentChanges
    , mkNetworkInterfaceAttachmentChanges
    , niacAttachmentId
    , niacDeleteOnTermination

    -- * NetworkInterfacePrivateIpAddress
    , NetworkInterfacePrivateIpAddress
    , mkNetworkInterfacePrivateIpAddress
    , nipiaPrivateIpAddress
    , nipiaPrivateDnsName
    , nipiaPrimary
    , nipiaAssociation

    -- * Placement
    , Placement
    , mkPlacement
    , pzAvailabilityZone
    , pzGroupName
    , pzTenancy

    -- * PlacementGroup
    , PlacementGroup
    , phGroupName
    , phStrategy
    , phState

    -- * PortRange
    , PortRange
    , mkPortRange
    , prFrom
    , prTo

    -- * PriceSchedule
    , PriceSchedule
    , mkPriceSchedule
    , psTerm
    , psPrice
    , psCurrencyCode
    , psActive

    -- * PriceScheduleSpecification
    , PriceScheduleSpecification
    , mkPriceScheduleSpecification
    , pssTerm
    , pssPrice
    , pssCurrencyCode

    -- * PricingDetail
    , PricingDetail
    , mkPricingDetail
    , piPrice
    , piCount

    -- * PrivateIpAddressSpecification
    , PrivateIpAddressSpecification
    , mkPrivateIpAddressSpecification
    , piasPrivateIpAddress
    , piasPrimary

    -- * ProductCode
    , ProductCode
    , mkProductCode
    , pcProductCodeId
    , pcProductCodeType

    -- * RecurringCharge
    , RecurringCharge
    , mkRecurringCharge
    , rdFrequency
    , rdAmount

    -- * Region
    , Region
    , rqRegionName
    , rqEndpoint

    -- * Reservation
    , Reservation
    , rnReservationId
    , rnOwnerId
    , rnRequesterId
    , rnGroups
    , rnInstances

    -- * ReservedInstanceLimitPrice
    , ReservedInstanceLimitPrice
    , mkReservedInstanceLimitPrice
    , rilpAmount
    , rilpCurrencyCode

    -- * ReservedInstances
    , ReservedInstances
    , riReservedInstancesId
    , riInstanceType
    , riAvailabilityZone
    , riStart
    , riEnd
    , riDuration
    , riUsagePrice
    , riFixedPrice
    , riInstanceCount
    , riProductDescription
    , riState
    , riTags
    , riInstanceTenancy
    , riCurrencyCode
    , riOfferingType
    , riRecurringCharges

    -- * ReservedInstancesConfiguration
    , ReservedInstancesConfiguration
    , mkReservedInstancesConfiguration
    , ricAvailabilityZone
    , ricPlatform
    , ricInstanceCount
    , ricInstanceType

    -- * ReservedInstancesListing
    , ReservedInstancesListing
    , rilReservedInstancesListingId
    , rilReservedInstancesId
    , rilCreateDate
    , rilUpdateDate
    , rilStatus
    , rilStatusMessage
    , rilInstanceCounts
    , rilPriceSchedules
    , rilTags
    , rilClientToken

    -- * ReservedInstancesModification
    , ReservedInstancesModification
    , rirReservedInstancesModificationId
    , rirReservedInstancesIds
    , rirModificationResults
    , rirCreateDate
    , rirUpdateDate
    , rirEffectiveDate
    , rirStatus
    , rirStatusMessage
    , rirClientToken

    -- * ReservedInstancesModificationResult
    , ReservedInstancesModificationResult
    , mkReservedInstancesModificationResult
    , rimrReservedInstancesId
    , rimrTargetConfiguration

    -- * ReservedInstancesOffering
    , ReservedInstancesOffering
    , ritReservedInstancesOfferingId
    , ritInstanceType
    , ritAvailabilityZone
    , ritDuration
    , ritUsagePrice
    , ritFixedPrice
    , ritProductDescription
    , ritInstanceTenancy
    , ritCurrencyCode
    , ritOfferingType
    , ritRecurringCharges
    , ritMarketplace
    , ritPricingDetails

    -- * Route
    , Route
    , mkRoute
    , reDestinationCidrBlock
    , reGatewayId
    , reInstanceId
    , reInstanceOwnerId
    , reNetworkInterfaceId
    , reVpcPeeringConnectionId
    , reState
    , reOrigin

    -- * RouteTable
    , RouteTable
    , rtRouteTableId
    , rtVpcId
    , rtRoutes
    , rtAssociations
    , rtTags
    , rtPropagatingVgws

    -- * RouteTableAssociation
    , RouteTableAssociation
    , mkRouteTableAssociation
    , rtaRouteTableAssociationId
    , rtaRouteTableId
    , rtaSubnetId
    , rtaMain

    -- * S3Storage
    , S3Storage
    , mkS3Storage
    , ssBucket
    , ssPrefix
    , ssAWSAccessKeyId
    , ssUploadPolicy
    , ssUploadPolicySignature

    -- * SecurityGroup
    , SecurityGroup
    , siOwnerId
    , siGroupName
    , siGroupId
    , siDescription
    , siIpPermissions
    , siIpPermissionsEgress
    , siVpcId
    , siTags

    -- * Snapshot
    , Snapshot
    , ssuSnapshotId
    , ssuVolumeId
    , ssuState
    , ssuStartTime
    , ssuProgress
    , ssuOwnerId
    , ssuDescription
    , ssuVolumeSize
    , ssuOwnerAlias
    , ssuTags
    , ssuEncrypted

    -- * SpotDatafeedSubscription
    , SpotDatafeedSubscription
    , sdsOwnerId
    , sdsBucket
    , sdsPrefix
    , sdsState
    , sdsFault

    -- * SpotInstanceRequest
    , SpotInstanceRequest
    , sirSpotInstanceRequestId
    , sirSpotPrice
    , sirType
    , sirState
    , sirFault
    , sirStatus
    , sirValidFrom
    , sirValidUntil
    , sirLaunchGroup
    , sirAvailabilityZoneGroup
    , sirLaunchSpecification
    , sirInstanceId
    , sirCreateTime
    , sirProductDescription
    , sirTags
    , sirLaunchedAvailabilityZone

    -- * SpotInstanceStateFault
    , SpotInstanceStateFault
    , mkSpotInstanceStateFault
    , sisfCode
    , sisfMessage

    -- * SpotInstanceStatus
    , SpotInstanceStatus
    , mkSpotInstanceStatus
    , siuCode
    , siuUpdateTime
    , siuMessage

    -- * SpotPlacement
    , SpotPlacement
    , mkSpotPlacement
    , spAvailabilityZone
    , spGroupName

    -- * SpotPrice
    , SpotPrice
    , sqInstanceType
    , sqProductDescription
    , sqSpotPrice
    , sqTimestamp
    , sqAvailabilityZone

    -- * StateReason
    , StateReason
    , mkStateReason
    , srCode
    , srMessage

    -- * Subnet
    , Subnet
    , sxSubnetId
    , sxState
    , sxVpcId
    , sxCidrBlock
    , sxAvailableIpAddressCount
    , sxAvailabilityZone
    , sxDefaultForAz
    , sxMapPublicIpOnLaunch
    , sxTags

    -- * Tag
    , Tag
    , mkTag
    , tgKey
    , tgValue

    -- * TagDescription
    , TagDescription
    , tdResourceId
    , tdResourceType
    , tdKey
    , tdValue

    -- * UserIdGroupPair
    , UserIdGroupPair
    , mkUserIdGroupPair
    , uigpUserId
    , uigpGroupName
    , uigpGroupId

    -- * VgwTelemetry
    , VgwTelemetry
    , mkVgwTelemetry
    , vvvvvvvvvvvvvvyOutsideIpAddress
    , vvvvvvvvvvvvvvyStatus
    , vvvvvvvvvvvvvvyLastStatusChange
    , vvvvvvvvvvvvvvyStatusMessage
    , vvvvvvvvvvvvvvyAcceptedRouteCount

    -- * Volume
    , Volume
    , vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjVolumeId
    , vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjSize
    , vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjSnapshotId
    , vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjAvailabilityZone
    , vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjState
    , vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjCreateTime
    , vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjAttachments
    , vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjTags
    , vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjVolumeType
    , vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjIops
    , vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjEncrypted

    -- * VolumeAttachment
    , VolumeAttachment
    , mkVolumeAttachment
    , vcVolumeId
    , vcInstanceId
    , vcDevice
    , vcState
    , vcAttachTime
    , vcDeleteOnTermination

    -- * VolumeStatusAction
    , VolumeStatusAction
    , mkVolumeStatusAction
    , vsaCode
    , vsaDescription
    , vsaEventType
    , vsaEventId

    -- * VolumeStatusDetails
    , VolumeStatusDetails
    , mkVolumeStatusDetails
    , vsdName
    , vsdStatus

    -- * VolumeStatusEvent
    , VolumeStatusEvent
    , mkVolumeStatusEvent
    , vseEventType
    , vseDescription
    , vseNotBefore
    , vseNotAfter
    , vseEventId

    -- * VolumeStatusInfo
    , VolumeStatusInfo
    , mkVolumeStatusInfo
    , vsjStatus
    , vsjDetails

    -- * VolumeStatusItem
    , VolumeStatusItem
    , vsiVolumeId
    , vsiAvailabilityZone
    , vsiVolumeStatus
    , vsiEvents
    , vsiActions

    -- * Vpc
    , Vpc
    , vdVpcId
    , vdState
    , vdCidrBlock
    , vdDhcpOptionsId
    , vdTags
    , vdInstanceTenancy
    , vdIsDefault

    -- * VpcAttachment
    , VpcAttachment
    , mkVpcAttachment
    , vbVpcId
    , vbState

    -- * VpcPeeringConnection
    , VpcPeeringConnection
    , vpcAccepterVpcInfo
    , vpcExpirationTime
    , vpcRequesterVpcInfo
    , vpcStatus
    , vpcTags
    , vpcVpcPeeringConnectionId

    -- * VpcPeeringConnectionStateReason
    , VpcPeeringConnectionStateReason
    , mkVpcPeeringConnectionStateReason
    , vpcsrCode
    , vpcsrMessage

    -- * VpcPeeringConnectionVpcInfo
    , VpcPeeringConnectionVpcInfo
    , mkVpcPeeringConnectionVpcInfo
    , vpcviCidrBlock
    , vpcviOwnerId
    , vpcviVpcId

    -- * VpnConnection
    , VpnConnection
    , vvvvvvvvvvvvvvoVpnConnectionId
    , vvvvvvvvvvvvvvoState
    , vvvvvvvvvvvvvvoCustomerGatewayConfiguration
    , vvvvvvvvvvvvvvoType
    , vvvvvvvvvvvvvvoCustomerGatewayId
    , vvvvvvvvvvvvvvoVpnGatewayId
    , vvvvvvvvvvvvvvoTags
    , vvvvvvvvvvvvvvoVgwTelemetry
    , vvvvvvvvvvvvvvoOptions
    , vvvvvvvvvvvvvvoRoutes

    -- * VpnGateway
    , VpnGateway
    , vvvvvvvvvvvvvvvyVpnGatewayId
    , vvvvvvvvvvvvvvvyState
    , vvvvvvvvvvvvvvvyType
    , vvvvvvvvvvvvvvvyAvailabilityZone
    , vvvvvvvvvvvvvvvyVpcAttachments
    , vvvvvvvvvvvvvvvyTags

    -- * VpnStaticRoute
    , VpnStaticRoute
    , mkVpnStaticRoute
    , vsrDestinationCidrBlock
    , vsrSource
    , vsrState
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2014-06-15@) of the
-- @Amazon Elastic Compute Cloud@ service.
data EC2 deriving (Typeable)

instance AWSService EC2 where
    type Sg EC2 = V4
    data Er EC2
        = EC2Client HttpException
        | EC2Serializer String
        | EC2Service String

    service = Service'
        { _svcEndpoint = Regional
        , _svcPrefix   = "ec2"
        , _svcVersion  = "2014-06-15"
        , _svcTarget   = Nothing
        }

deriving instance Show    (Er EC2)
deriving instance Generic (Er EC2)

instance AWSError (Er EC2) where
    awsError = const "EC2Error"

instance AWSServiceError (Er EC2) where
    serviceError    = EC2Service
    clientError     = EC2Client
    serializerError = EC2Serializer

instance Exception (Er EC2)

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def
    { xmlNamespace = Just "http://ec2.amazonaws.com/doc/2014-06-15/"
    }

data AccountAttributeName
    = AccountAttributeNameDefaultVpc -- ^ default-vpc
    | AccountAttributeNameSupportedPlatforms -- ^ supported-platforms
      deriving (Eq, Show, Generic)

instance Hashable AccountAttributeName

instance FromText AccountAttributeName where
    parser = match "default-vpc" AccountAttributeNameDefaultVpc
         <|> match "supported-platforms" AccountAttributeNameSupportedPlatforms

instance ToText AccountAttributeName where
    toText AccountAttributeNameDefaultVpc = "default-vpc"
    toText AccountAttributeNameSupportedPlatforms = "supported-platforms"

instance ToByteString AccountAttributeName

instance ToQuery AccountAttributeName where
    toQuery = genericQuery def

-- | The architecture of the image.
data ArchitectureValues
    = ArchitectureValuesI386 -- ^ i386
    | ArchitectureValuesX8664 -- ^ x86_64
      deriving (Eq, Show, Generic)

instance Hashable ArchitectureValues

instance FromText ArchitectureValues where
    parser = match "i386" ArchitectureValuesI386
         <|> match "x86_64" ArchitectureValuesX8664

instance ToText ArchitectureValues where
    toText ArchitectureValuesI386 = "i386"
    toText ArchitectureValuesX8664 = "x86_64"

instance ToByteString ArchitectureValues

instance FromXML ArchitectureValues where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "architecture"

instance ToQuery ArchitectureValues where
    toQuery = genericQuery def

-- | The current state of the attachment.
data AttachmentStatus
    = AttachmentStatusAttached -- ^ attached
    | AttachmentStatusAttaching -- ^ attaching
    | AttachmentStatusDetached -- ^ detached
    | AttachmentStatusDetaching -- ^ detaching
      deriving (Eq, Show, Generic)

instance Hashable AttachmentStatus

instance FromText AttachmentStatus where
    parser = match "attached" AttachmentStatusAttached
         <|> match "attaching" AttachmentStatusAttaching
         <|> match "detached" AttachmentStatusDetached
         <|> match "detaching" AttachmentStatusDetaching

instance ToText AttachmentStatus where
    toText AttachmentStatusAttached = "attached"
    toText AttachmentStatusAttaching = "attaching"
    toText AttachmentStatusDetached = "detached"
    toText AttachmentStatusDetaching = "detaching"

instance ToByteString AttachmentStatus

instance FromXML AttachmentStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "state"

instance ToQuery AttachmentStatus where
    toQuery = genericQuery def

-- | The state of the Availability Zone.
data AvailabilityZoneState
    = AvailabilityZoneStateAvailable -- ^ available
      deriving (Eq, Show, Generic)

instance Hashable AvailabilityZoneState

instance FromText AvailabilityZoneState where
    parser = match "available" AvailabilityZoneStateAvailable

instance ToText AvailabilityZoneState where
    toText AvailabilityZoneStateAvailable = "available"

instance ToByteString AvailabilityZoneState

instance FromXML AvailabilityZoneState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "zoneState"

instance ToQuery AvailabilityZoneState where
    toQuery = genericQuery def

-- | The state of the task.
data BundleTaskState
    = BundleTaskStateBundling -- ^ bundling
    | BundleTaskStateCancelling -- ^ cancelling
    | BundleTaskStateComplete -- ^ complete
    | BundleTaskStateFailed -- ^ failed
    | BundleTaskStatePending -- ^ pending
    | BundleTaskStateStoring -- ^ storing
    | BundleTaskStateWaitingForShutdown -- ^ waiting-for-shutdown
      deriving (Eq, Show, Generic)

instance Hashable BundleTaskState

instance FromText BundleTaskState where
    parser = match "bundling" BundleTaskStateBundling
         <|> match "cancelling" BundleTaskStateCancelling
         <|> match "complete" BundleTaskStateComplete
         <|> match "failed" BundleTaskStateFailed
         <|> match "pending" BundleTaskStatePending
         <|> match "storing" BundleTaskStateStoring
         <|> match "waiting-for-shutdown" BundleTaskStateWaitingForShutdown

instance ToText BundleTaskState where
    toText BundleTaskStateBundling = "bundling"
    toText BundleTaskStateCancelling = "cancelling"
    toText BundleTaskStateComplete = "complete"
    toText BundleTaskStateFailed = "failed"
    toText BundleTaskStatePending = "pending"
    toText BundleTaskStateStoring = "storing"
    toText BundleTaskStateWaitingForShutdown = "waiting-for-shutdown"

instance ToByteString BundleTaskState

instance FromXML BundleTaskState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "state"

instance ToQuery BundleTaskState where
    toQuery = genericQuery def

-- | The state of the Spot Instance request.
data CancelSpotInstanceRequestState
    = CancelSpotInstanceRequestStateActive -- ^ active
    | CancelSpotInstanceRequestStateCancelled -- ^ cancelled
    | CancelSpotInstanceRequestStateClosed -- ^ closed
    | CancelSpotInstanceRequestStateCompleted -- ^ completed
    | CancelSpotInstanceRequestStateOpen -- ^ open
      deriving (Eq, Show, Generic)

instance Hashable CancelSpotInstanceRequestState

instance FromText CancelSpotInstanceRequestState where
    parser = match "active" CancelSpotInstanceRequestStateActive
         <|> match "cancelled" CancelSpotInstanceRequestStateCancelled
         <|> match "closed" CancelSpotInstanceRequestStateClosed
         <|> match "completed" CancelSpotInstanceRequestStateCompleted
         <|> match "open" CancelSpotInstanceRequestStateOpen

instance ToText CancelSpotInstanceRequestState where
    toText CancelSpotInstanceRequestStateActive = "active"
    toText CancelSpotInstanceRequestStateCancelled = "cancelled"
    toText CancelSpotInstanceRequestStateClosed = "closed"
    toText CancelSpotInstanceRequestStateCompleted = "completed"
    toText CancelSpotInstanceRequestStateOpen = "open"

instance ToByteString CancelSpotInstanceRequestState

instance FromXML CancelSpotInstanceRequestState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "state"

instance ToQuery CancelSpotInstanceRequestState where
    toQuery = genericQuery def

-- | 
data ContainerFormat
    = ContainerFormatOva -- ^ ova
      deriving (Eq, Show, Generic)

instance Hashable ContainerFormat

instance FromText ContainerFormat where
    parser = match "ova" ContainerFormatOva

instance ToText ContainerFormat where
    toText ContainerFormatOva = "ova"

instance ToByteString ContainerFormat

instance FromXML ContainerFormat where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ContainerFormat"

instance ToQuery ContainerFormat where
    toQuery = genericQuery def

-- | The state of the conversion task.
data ConversionTaskState
    = ConversionTaskStateActive -- ^ active
    | ConversionTaskStateCancelled -- ^ cancelled
    | ConversionTaskStateCancelling -- ^ cancelling
    | ConversionTaskStateCompleted -- ^ completed
      deriving (Eq, Show, Generic)

instance Hashable ConversionTaskState

instance FromText ConversionTaskState where
    parser = match "active" ConversionTaskStateActive
         <|> match "cancelled" ConversionTaskStateCancelled
         <|> match "cancelling" ConversionTaskStateCancelling
         <|> match "completed" ConversionTaskStateCompleted

instance ToText ConversionTaskState where
    toText ConversionTaskStateActive = "active"
    toText ConversionTaskStateCancelled = "cancelled"
    toText ConversionTaskStateCancelling = "cancelling"
    toText ConversionTaskStateCompleted = "completed"

instance ToByteString ConversionTaskState

instance FromXML ConversionTaskState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "state"

instance ToQuery ConversionTaskState where
    toQuery = genericQuery def

-- | The currency for transacting the Reserved Instance resale. At this time,
-- the only supported currency is USD.
data CurrencyCodeValues
    = CurrencyCodeValuesUsd -- ^ USD
      deriving (Eq, Show, Generic)

instance Hashable CurrencyCodeValues

instance FromText CurrencyCodeValues where
    parser = match "USD" CurrencyCodeValuesUsd

instance ToText CurrencyCodeValues where
    toText CurrencyCodeValuesUsd = "USD"

instance ToByteString CurrencyCodeValues

instance FromXML CurrencyCodeValues where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "currencyCode"

instance ToQuery CurrencyCodeValues where
    toQuery = genericQuery def

-- | The state of the Spot Instance datafeed subscription.
data DatafeedSubscriptionState
    = DatafeedSubscriptionStateActive -- ^ Active
    | DatafeedSubscriptionStateInactive -- ^ Inactive
      deriving (Eq, Show, Generic)

instance Hashable DatafeedSubscriptionState

instance FromText DatafeedSubscriptionState where
    parser = match "Active" DatafeedSubscriptionStateActive
         <|> match "Inactive" DatafeedSubscriptionStateInactive

instance ToText DatafeedSubscriptionState where
    toText DatafeedSubscriptionStateActive = "Active"
    toText DatafeedSubscriptionStateInactive = "Inactive"

instance ToByteString DatafeedSubscriptionState

instance FromXML DatafeedSubscriptionState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "state"

-- | The type of root device used by the AMI. The AMI can use an Amazon EBS
-- volume or an instance store volume.
data DeviceType
    = DeviceTypeEbs -- ^ ebs
    | DeviceTypeInstanceStore -- ^ instance-store
      deriving (Eq, Show, Generic)

instance Hashable DeviceType

instance FromText DeviceType where
    parser = match "ebs" DeviceTypeEbs
         <|> match "instance-store" DeviceTypeInstanceStore

instance ToText DeviceType where
    toText DeviceTypeEbs = "ebs"
    toText DeviceTypeInstanceStore = "instance-store"

instance ToByteString DeviceType

instance FromXML DeviceType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "rootDeviceType"

instance ToQuery DeviceType where
    toQuery = genericQuery def

-- | 
data DiskImageFormat
    = DiskImageFormatRaw -- ^ RAW
    | DiskImageFormatVhd -- ^ VHD
    | DiskImageFormatVmdk -- ^ VMDK
      deriving (Eq, Show, Generic)

instance Hashable DiskImageFormat

instance FromText DiskImageFormat where
    parser = match "RAW" DiskImageFormatRaw
         <|> match "VHD" DiskImageFormatVhd
         <|> match "VMDK" DiskImageFormatVmdk

instance ToText DiskImageFormat where
    toText DiskImageFormatRaw = "RAW"
    toText DiskImageFormatVhd = "VHD"
    toText DiskImageFormatVmdk = "VMDK"

instance ToByteString DiskImageFormat

instance FromXML DiskImageFormat where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DiskImageFormat"

instance ToQuery DiskImageFormat where
    toQuery = genericQuery def

-- | Set to vpc to allocate the address for use with instances in a VPC.
-- Default: The address is for use with instances in EC2-Classic.
data DomainType
    = DomainTypeStandard -- ^ standard
    | DomainTypeVpc -- ^ vpc
      deriving (Eq, Show, Generic)

instance Hashable DomainType

instance FromText DomainType where
    parser = match "standard" DomainTypeStandard
         <|> match "vpc" DomainTypeVpc

instance ToText DomainType where
    toText DomainTypeStandard = "standard"
    toText DomainTypeVpc = "vpc"

instance ToByteString DomainType

instance FromXML DomainType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DomainType"

instance ToQuery DomainType where
    toQuery = genericQuery def

-- | The associated code of the event.
data EventCode
    = EventCodeInstanceReboot -- ^ instance-reboot
    | EventCodeInstanceRetirement -- ^ instance-retirement
    | EventCodeInstanceStop -- ^ instance-stop
    | EventCodeSystemMaintenance -- ^ system-maintenance
    | EventCodeSystemReboot -- ^ system-reboot
      deriving (Eq, Show, Generic)

instance Hashable EventCode

instance FromText EventCode where
    parser = match "instance-reboot" EventCodeInstanceReboot
         <|> match "instance-retirement" EventCodeInstanceRetirement
         <|> match "instance-stop" EventCodeInstanceStop
         <|> match "system-maintenance" EventCodeSystemMaintenance
         <|> match "system-reboot" EventCodeSystemReboot

instance ToText EventCode where
    toText EventCodeInstanceReboot = "instance-reboot"
    toText EventCodeInstanceRetirement = "instance-retirement"
    toText EventCodeInstanceStop = "instance-stop"
    toText EventCodeSystemMaintenance = "system-maintenance"
    toText EventCodeSystemReboot = "system-reboot"

instance ToByteString EventCode

instance FromXML EventCode where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "code"

instance ToQuery EventCode where
    toQuery = genericQuery def

-- | The target virtualization environment.
data ExportEnvironment
    = ExportEnvironmentCitrix -- ^ citrix
    | ExportEnvironmentMicrosoft -- ^ microsoft
    | ExportEnvironmentVmware -- ^ vmware
      deriving (Eq, Show, Generic)

instance Hashable ExportEnvironment

instance FromText ExportEnvironment where
    parser = match "citrix" ExportEnvironmentCitrix
         <|> match "microsoft" ExportEnvironmentMicrosoft
         <|> match "vmware" ExportEnvironmentVmware

instance ToText ExportEnvironment where
    toText ExportEnvironmentCitrix = "citrix"
    toText ExportEnvironmentMicrosoft = "microsoft"
    toText ExportEnvironmentVmware = "vmware"

instance ToByteString ExportEnvironment

instance FromXML ExportEnvironment where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ExportEnvironment"

instance ToQuery ExportEnvironment where
    toQuery = genericQuery def

-- | The state of the conversion task.
data ExportTaskState
    = ExportTaskStateActive -- ^ active
    | ExportTaskStateCancelled -- ^ cancelled
    | ExportTaskStateCancelling -- ^ cancelling
    | ExportTaskStateCompleted -- ^ completed
      deriving (Eq, Show, Generic)

instance Hashable ExportTaskState

instance FromText ExportTaskState where
    parser = match "active" ExportTaskStateActive
         <|> match "cancelled" ExportTaskStateCancelled
         <|> match "cancelling" ExportTaskStateCancelling
         <|> match "completed" ExportTaskStateCompleted

instance ToText ExportTaskState where
    toText ExportTaskStateActive = "active"
    toText ExportTaskStateCancelled = "cancelled"
    toText ExportTaskStateCancelling = "cancelling"
    toText ExportTaskStateCompleted = "completed"

instance ToByteString ExportTaskState

instance FromXML ExportTaskState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "state"

instance ToQuery ExportTaskState where
    toQuery = genericQuery def

-- | The type of VPN connection that this customer gateway supports.
data GatewayType
    = GatewayTypeIpsec1 -- ^ ipsec.1
      deriving (Eq, Show, Generic)

instance Hashable GatewayType

instance FromText GatewayType where
    parser = match "ipsec.1" GatewayTypeIpsec1

instance ToText GatewayType where
    toText GatewayTypeIpsec1 = "ipsec.1"

instance ToByteString GatewayType

instance FromXML GatewayType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "GatewayType"

instance ToQuery GatewayType where
    toQuery = genericQuery def

-- | The hypervisor type of the image.
data HypervisorType
    = HypervisorTypeOvm -- ^ ovm
    | HypervisorTypeXen -- ^ xen
      deriving (Eq, Show, Generic)

instance Hashable HypervisorType

instance FromText HypervisorType where
    parser = match "ovm" HypervisorTypeOvm
         <|> match "xen" HypervisorTypeXen

instance ToText HypervisorType where
    toText HypervisorTypeOvm = "ovm"
    toText HypervisorTypeXen = "xen"

instance ToByteString HypervisorType

instance FromXML HypervisorType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "hypervisor"

instance ToQuery HypervisorType where
    toQuery = genericQuery def

-- | The AMI attribute.
data ImageAttributeName
    = ImageAttributeNameBlockDeviceMapping -- ^ blockDeviceMapping
    | ImageAttributeNameDescription -- ^ description
    | ImageAttributeNameKernel -- ^ kernel
    | ImageAttributeNameLaunchPermission -- ^ launchPermission
    | ImageAttributeNameProductCodes -- ^ productCodes
    | ImageAttributeNameRamdisk -- ^ ramdisk
      deriving (Eq, Show, Generic)

instance Hashable ImageAttributeName

instance FromText ImageAttributeName where
    parser = match "blockDeviceMapping" ImageAttributeNameBlockDeviceMapping
         <|> match "description" ImageAttributeNameDescription
         <|> match "kernel" ImageAttributeNameKernel
         <|> match "launchPermission" ImageAttributeNameLaunchPermission
         <|> match "productCodes" ImageAttributeNameProductCodes
         <|> match "ramdisk" ImageAttributeNameRamdisk

instance ToText ImageAttributeName where
    toText ImageAttributeNameBlockDeviceMapping = "blockDeviceMapping"
    toText ImageAttributeNameDescription = "description"
    toText ImageAttributeNameKernel = "kernel"
    toText ImageAttributeNameLaunchPermission = "launchPermission"
    toText ImageAttributeNameProductCodes = "productCodes"
    toText ImageAttributeNameRamdisk = "ramdisk"

instance ToByteString ImageAttributeName

instance ToQuery ImageAttributeName where
    toQuery = genericQuery def

-- | The current state of the AMI. If the state is available, the image is
-- successfully registered and can be used to launch an instance.
data ImageState
    = ImageStateAvailable -- ^ available
    | ImageStateDeregistered -- ^ deregistered
      deriving (Eq, Show, Generic)

instance Hashable ImageState

instance FromText ImageState where
    parser = match "available" ImageStateAvailable
         <|> match "deregistered" ImageStateDeregistered

instance ToText ImageState where
    toText ImageStateAvailable = "available"
    toText ImageStateDeregistered = "deregistered"

instance ToByteString ImageState

instance FromXML ImageState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "imageState"

instance ToQuery ImageState where
    toQuery = genericQuery def

-- | The type of image.
data ImageTypeValues
    = ImageTypeValuesKernel -- ^ kernel
    | ImageTypeValuesMachine -- ^ machine
    | ImageTypeValuesRamdisk -- ^ ramdisk
      deriving (Eq, Show, Generic)

instance Hashable ImageTypeValues

instance FromText ImageTypeValues where
    parser = match "kernel" ImageTypeValuesKernel
         <|> match "machine" ImageTypeValuesMachine
         <|> match "ramdisk" ImageTypeValuesRamdisk

instance ToText ImageTypeValues where
    toText ImageTypeValuesKernel = "kernel"
    toText ImageTypeValuesMachine = "machine"
    toText ImageTypeValuesRamdisk = "ramdisk"

instance ToByteString ImageTypeValues

instance FromXML ImageTypeValues where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "imageType"

instance ToQuery ImageTypeValues where
    toQuery = genericQuery def

-- | The instance attribute.
data InstanceAttributeName
    = InstanceAttributeNameBlockDeviceMapping -- ^ blockDeviceMapping
    | InstanceAttributeNameDisableApiTermination -- ^ disableApiTermination
    | InstanceAttributeNameEbsOptimized -- ^ ebsOptimized
    | InstanceAttributeNameGroupSet -- ^ groupSet
    | InstanceAttributeNameInstanceInitiatedShutdownBehavior -- ^ instanceInitiatedShutdownBehavior
    | InstanceAttributeNameInstanceType -- ^ instanceType
    | InstanceAttributeNameKernel -- ^ kernel
    | InstanceAttributeNameProductCodes -- ^ productCodes
    | InstanceAttributeNameRamdisk -- ^ ramdisk
    | InstanceAttributeNameRootDeviceName -- ^ rootDeviceName
    | InstanceAttributeNameSourceDestCheck -- ^ sourceDestCheck
    | InstanceAttributeNameSriovNetSupport -- ^ sriovNetSupport
    | InstanceAttributeNameUserData -- ^ userData
      deriving (Eq, Show, Generic)

instance Hashable InstanceAttributeName

instance FromText InstanceAttributeName where
    parser = match "blockDeviceMapping" InstanceAttributeNameBlockDeviceMapping
         <|> match "disableApiTermination" InstanceAttributeNameDisableApiTermination
         <|> match "ebsOptimized" InstanceAttributeNameEbsOptimized
         <|> match "groupSet" InstanceAttributeNameGroupSet
         <|> match "instanceInitiatedShutdownBehavior" InstanceAttributeNameInstanceInitiatedShutdownBehavior
         <|> match "instanceType" InstanceAttributeNameInstanceType
         <|> match "kernel" InstanceAttributeNameKernel
         <|> match "productCodes" InstanceAttributeNameProductCodes
         <|> match "ramdisk" InstanceAttributeNameRamdisk
         <|> match "rootDeviceName" InstanceAttributeNameRootDeviceName
         <|> match "sourceDestCheck" InstanceAttributeNameSourceDestCheck
         <|> match "sriovNetSupport" InstanceAttributeNameSriovNetSupport
         <|> match "userData" InstanceAttributeNameUserData

instance ToText InstanceAttributeName where
    toText InstanceAttributeNameBlockDeviceMapping = "blockDeviceMapping"
    toText InstanceAttributeNameDisableApiTermination = "disableApiTermination"
    toText InstanceAttributeNameEbsOptimized = "ebsOptimized"
    toText InstanceAttributeNameGroupSet = "groupSet"
    toText InstanceAttributeNameInstanceInitiatedShutdownBehavior = "instanceInitiatedShutdownBehavior"
    toText InstanceAttributeNameInstanceType = "instanceType"
    toText InstanceAttributeNameKernel = "kernel"
    toText InstanceAttributeNameProductCodes = "productCodes"
    toText InstanceAttributeNameRamdisk = "ramdisk"
    toText InstanceAttributeNameRootDeviceName = "rootDeviceName"
    toText InstanceAttributeNameSourceDestCheck = "sourceDestCheck"
    toText InstanceAttributeNameSriovNetSupport = "sriovNetSupport"
    toText InstanceAttributeNameUserData = "userData"

instance ToByteString InstanceAttributeName

instance ToQuery InstanceAttributeName where
    toQuery = genericQuery def

-- | Indicates whether this is a Spot Instance.
data InstanceLifecycleType
    = InstanceLifecycleTypeSpot -- ^ spot
      deriving (Eq, Show, Generic)

instance Hashable InstanceLifecycleType

instance FromText InstanceLifecycleType where
    parser = match "spot" InstanceLifecycleTypeSpot

instance ToText InstanceLifecycleType where
    toText InstanceLifecycleTypeSpot = "spot"

instance ToByteString InstanceLifecycleType

instance FromXML InstanceLifecycleType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "instanceLifecycle"

instance ToQuery InstanceLifecycleType where
    toQuery = genericQuery def

-- | The current state of the instance.
data InstanceStateName
    = InstanceStateNamePending -- ^ pending
    | InstanceStateNameRunning -- ^ running
    | InstanceStateNameShuttingDown -- ^ shutting-down
    | InstanceStateNameStopped -- ^ stopped
    | InstanceStateNameStopping -- ^ stopping
    | InstanceStateNameTerminated -- ^ terminated
      deriving (Eq, Show, Generic)

instance Hashable InstanceStateName

instance FromText InstanceStateName where
    parser = match "pending" InstanceStateNamePending
         <|> match "running" InstanceStateNameRunning
         <|> match "shutting-down" InstanceStateNameShuttingDown
         <|> match "stopped" InstanceStateNameStopped
         <|> match "stopping" InstanceStateNameStopping
         <|> match "terminated" InstanceStateNameTerminated

instance ToText InstanceStateName where
    toText InstanceStateNamePending = "pending"
    toText InstanceStateNameRunning = "running"
    toText InstanceStateNameShuttingDown = "shutting-down"
    toText InstanceStateNameStopped = "stopped"
    toText InstanceStateNameStopping = "stopping"
    toText InstanceStateNameTerminated = "terminated"

instance ToByteString InstanceStateName

instance FromXML InstanceStateName where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "name"

instance ToQuery InstanceStateName where
    toQuery = genericQuery def

-- | The instance type.
data InstanceType
    = C1Medium -- ^ c1.medium
    | C1Xlarge -- ^ c1.xlarge
    | C32Xlarge -- ^ c3.2xlarge
    | C34Xlarge -- ^ c3.4xlarge
    | C38Xlarge -- ^ c3.8xlarge
    | C3Large -- ^ c3.large
    | C3Xlarge -- ^ c3.xlarge
    | Cc14Xlarge -- ^ cc1.4xlarge
    | Cc28Xlarge -- ^ cc2.8xlarge
    | Cg14Xlarge -- ^ cg1.4xlarge
    | Cr18Xlarge -- ^ cr1.8xlarge
    | G22Xlarge -- ^ g2.2xlarge
    | Hi14Xlarge -- ^ hi1.4xlarge
    | Hs18Xlarge -- ^ hs1.8xlarge
    | I22Xlarge -- ^ i2.2xlarge
    | I24Xlarge -- ^ i2.4xlarge
    | I28Xlarge -- ^ i2.8xlarge
    | I2Xlarge -- ^ i2.xlarge
    | M1Large -- ^ m1.large
    | M1Medium -- ^ m1.medium
    | M1Small -- ^ m1.small
    | M1Xlarge -- ^ m1.xlarge
    | M22Xlarge -- ^ m2.2xlarge
    | M24Xlarge -- ^ m2.4xlarge
    | M2Xlarge -- ^ m2.xlarge
    | M32Xlarge -- ^ m3.2xlarge
    | M3Large -- ^ m3.large
    | M3Medium -- ^ m3.medium
    | M3Xlarge -- ^ m3.xlarge
    | R32Xlarge -- ^ r3.2xlarge
    | R34Xlarge -- ^ r3.4xlarge
    | R38Xlarge -- ^ r3.8xlarge
    | R3Large -- ^ r3.large
    | R3Xlarge -- ^ r3.xlarge
    | T1Micro -- ^ t1.micro
    | T2Medium -- ^ t2.medium
    | T2Micro -- ^ t2.micro
    | T2Small -- ^ t2.small
      deriving (Eq, Show, Generic)

instance Hashable InstanceType

instance FromText InstanceType where
    parser = match "c1.medium" C1Medium
         <|> match "c1.xlarge" C1Xlarge
         <|> match "c3.2xlarge" C32Xlarge
         <|> match "c3.4xlarge" C34Xlarge
         <|> match "c3.8xlarge" C38Xlarge
         <|> match "c3.large" C3Large
         <|> match "c3.xlarge" C3Xlarge
         <|> match "cc1.4xlarge" Cc14Xlarge
         <|> match "cc2.8xlarge" Cc28Xlarge
         <|> match "cg1.4xlarge" Cg14Xlarge
         <|> match "cr1.8xlarge" Cr18Xlarge
         <|> match "g2.2xlarge" G22Xlarge
         <|> match "hi1.4xlarge" Hi14Xlarge
         <|> match "hs1.8xlarge" Hs18Xlarge
         <|> match "i2.2xlarge" I22Xlarge
         <|> match "i2.4xlarge" I24Xlarge
         <|> match "i2.8xlarge" I28Xlarge
         <|> match "i2.xlarge" I2Xlarge
         <|> match "m1.large" M1Large
         <|> match "m1.medium" M1Medium
         <|> match "m1.small" M1Small
         <|> match "m1.xlarge" M1Xlarge
         <|> match "m2.2xlarge" M22Xlarge
         <|> match "m2.4xlarge" M24Xlarge
         <|> match "m2.xlarge" M2Xlarge
         <|> match "m3.2xlarge" M32Xlarge
         <|> match "m3.large" M3Large
         <|> match "m3.medium" M3Medium
         <|> match "m3.xlarge" M3Xlarge
         <|> match "r3.2xlarge" R32Xlarge
         <|> match "r3.4xlarge" R34Xlarge
         <|> match "r3.8xlarge" R38Xlarge
         <|> match "r3.large" R3Large
         <|> match "r3.xlarge" R3Xlarge
         <|> match "t1.micro" T1Micro
         <|> match "t2.medium" T2Medium
         <|> match "t2.micro" T2Micro
         <|> match "t2.small" T2Small

instance ToText InstanceType where
    toText C1Medium = "c1.medium"
    toText C1Xlarge = "c1.xlarge"
    toText C32Xlarge = "c3.2xlarge"
    toText C34Xlarge = "c3.4xlarge"
    toText C38Xlarge = "c3.8xlarge"
    toText C3Large = "c3.large"
    toText C3Xlarge = "c3.xlarge"
    toText Cc14Xlarge = "cc1.4xlarge"
    toText Cc28Xlarge = "cc2.8xlarge"
    toText Cg14Xlarge = "cg1.4xlarge"
    toText Cr18Xlarge = "cr1.8xlarge"
    toText G22Xlarge = "g2.2xlarge"
    toText Hi14Xlarge = "hi1.4xlarge"
    toText Hs18Xlarge = "hs1.8xlarge"
    toText I22Xlarge = "i2.2xlarge"
    toText I24Xlarge = "i2.4xlarge"
    toText I28Xlarge = "i2.8xlarge"
    toText I2Xlarge = "i2.xlarge"
    toText M1Large = "m1.large"
    toText M1Medium = "m1.medium"
    toText M1Small = "m1.small"
    toText M1Xlarge = "m1.xlarge"
    toText M22Xlarge = "m2.2xlarge"
    toText M24Xlarge = "m2.4xlarge"
    toText M2Xlarge = "m2.xlarge"
    toText M32Xlarge = "m3.2xlarge"
    toText M3Large = "m3.large"
    toText M3Medium = "m3.medium"
    toText M3Xlarge = "m3.xlarge"
    toText R32Xlarge = "r3.2xlarge"
    toText R34Xlarge = "r3.4xlarge"
    toText R38Xlarge = "r3.8xlarge"
    toText R3Large = "r3.large"
    toText R3Xlarge = "r3.xlarge"
    toText T1Micro = "t1.micro"
    toText T2Medium = "t2.medium"
    toText T2Micro = "t2.micro"
    toText T2Small = "t2.small"

instance ToByteString InstanceType

instance FromXML InstanceType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "instanceType"

instance ToQuery InstanceType where
    toQuery = genericQuery def

-- | The states of the listed Reserved Instances.
data ListingState
    = ListingStateAvailable -- ^ available
    | ListingStateCancelled -- ^ cancelled
    | ListingStatePending -- ^ pending
    | ListingStateSold -- ^ sold
      deriving (Eq, Show, Generic)

instance Hashable ListingState

instance FromText ListingState where
    parser = match "available" ListingStateAvailable
         <|> match "cancelled" ListingStateCancelled
         <|> match "pending" ListingStatePending
         <|> match "sold" ListingStateSold

instance ToText ListingState where
    toText ListingStateAvailable = "available"
    toText ListingStateCancelled = "cancelled"
    toText ListingStatePending = "pending"
    toText ListingStateSold = "sold"

instance ToByteString ListingState

instance FromXML ListingState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "state"

instance ToQuery ListingState where
    toQuery = genericQuery def

-- | The status of the Reserved Instance listing.
data ListingStatus
    = ListingStatusActive -- ^ active
    | ListingStatusCancelled -- ^ cancelled
    | ListingStatusClosed -- ^ closed
    | ListingStatusPending -- ^ pending
      deriving (Eq, Show, Generic)

instance Hashable ListingStatus

instance FromText ListingStatus where
    parser = match "active" ListingStatusActive
         <|> match "cancelled" ListingStatusCancelled
         <|> match "closed" ListingStatusClosed
         <|> match "pending" ListingStatusPending

instance ToText ListingStatus where
    toText ListingStatusActive = "active"
    toText ListingStatusCancelled = "cancelled"
    toText ListingStatusClosed = "closed"
    toText ListingStatusPending = "pending"

instance ToByteString ListingStatus

instance FromXML ListingStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "status"

instance ToQuery ListingStatus where
    toQuery = genericQuery def

-- | Indicates whether monitoring is enabled for the instance.
data MonitoringState
    = MonitoringStateDisabled -- ^ disabled
    | MonitoringStateEnabled -- ^ enabled
    | MonitoringStatePending -- ^ pending
      deriving (Eq, Show, Generic)

instance Hashable MonitoringState

instance FromText MonitoringState where
    parser = match "disabled" MonitoringStateDisabled
         <|> match "enabled" MonitoringStateEnabled
         <|> match "pending" MonitoringStatePending

instance ToText MonitoringState where
    toText MonitoringStateDisabled = "disabled"
    toText MonitoringStateEnabled = "enabled"
    toText MonitoringStatePending = "pending"

instance ToByteString MonitoringState

instance FromXML MonitoringState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "state"

instance ToQuery MonitoringState where
    toQuery = genericQuery def

-- | The attribute of the network interface.
data NetworkInterfaceAttribute
    = NetworkInterfaceAttributeAttachment -- ^ attachment
    | NetworkInterfaceAttributeDescription -- ^ description
    | NetworkInterfaceAttributeGroupSet -- ^ groupSet
    | NetworkInterfaceAttributeSourceDestCheck -- ^ sourceDestCheck
      deriving (Eq, Show, Generic)

instance Hashable NetworkInterfaceAttribute

instance FromText NetworkInterfaceAttribute where
    parser = match "attachment" NetworkInterfaceAttributeAttachment
         <|> match "description" NetworkInterfaceAttributeDescription
         <|> match "groupSet" NetworkInterfaceAttributeGroupSet
         <|> match "sourceDestCheck" NetworkInterfaceAttributeSourceDestCheck

instance ToText NetworkInterfaceAttribute where
    toText NetworkInterfaceAttributeAttachment = "attachment"
    toText NetworkInterfaceAttributeDescription = "description"
    toText NetworkInterfaceAttributeGroupSet = "groupSet"
    toText NetworkInterfaceAttributeSourceDestCheck = "sourceDestCheck"

instance ToByteString NetworkInterfaceAttribute

instance ToQuery NetworkInterfaceAttribute where
    toQuery = genericQuery def

-- | The status of the network interface.
data NetworkInterfaceStatus
    = NetworkInterfaceStatusAttaching -- ^ attaching
    | NetworkInterfaceStatusAvailable -- ^ available
    | NetworkInterfaceStatusDetaching -- ^ detaching
    | NetworkInterfaceStatusInUse -- ^ in-use
      deriving (Eq, Show, Generic)

instance Hashable NetworkInterfaceStatus

instance FromText NetworkInterfaceStatus where
    parser = match "attaching" NetworkInterfaceStatusAttaching
         <|> match "available" NetworkInterfaceStatusAvailable
         <|> match "detaching" NetworkInterfaceStatusDetaching
         <|> match "in-use" NetworkInterfaceStatusInUse

instance ToText NetworkInterfaceStatus where
    toText NetworkInterfaceStatusAttaching = "attaching"
    toText NetworkInterfaceStatusAvailable = "available"
    toText NetworkInterfaceStatusDetaching = "detaching"
    toText NetworkInterfaceStatusInUse = "in-use"

instance ToByteString NetworkInterfaceStatus

instance FromXML NetworkInterfaceStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "status"

instance ToQuery NetworkInterfaceStatus where
    toQuery = genericQuery def

-- | The Reserved Instance offering type.
data OfferingTypeValues
    = OfferingTypeValuesHeavyUtilization -- ^ Heavy Utilization
    | OfferingTypeValuesLightUtilization -- ^ Light Utilization
    | OfferingTypeValuesMediumUtilization -- ^ Medium Utilization
      deriving (Eq, Show, Generic)

instance Hashable OfferingTypeValues

instance FromText OfferingTypeValues where
    parser = match "Heavy Utilization" OfferingTypeValuesHeavyUtilization
         <|> match "Light Utilization" OfferingTypeValuesLightUtilization
         <|> match "Medium Utilization" OfferingTypeValuesMediumUtilization

instance ToText OfferingTypeValues where
    toText OfferingTypeValuesHeavyUtilization = "Heavy Utilization"
    toText OfferingTypeValuesLightUtilization = "Light Utilization"
    toText OfferingTypeValuesMediumUtilization = "Medium Utilization"

instance ToByteString OfferingTypeValues

instance FromXML OfferingTypeValues where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "OfferingTypeValues"

instance ToQuery OfferingTypeValues where
    toQuery = genericQuery def

-- | The name of the group.
data PermissionGroup
    = PermissionGroupAll -- ^ all
      deriving (Eq, Show, Generic)

instance Hashable PermissionGroup

instance FromText PermissionGroup where
    parser = match "all" PermissionGroupAll

instance ToText PermissionGroup where
    toText PermissionGroupAll = "all"

instance ToByteString PermissionGroup

instance FromXML PermissionGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "group"

instance ToQuery PermissionGroup where
    toQuery = genericQuery def

-- | The state of the placement group.
data PlacementGroupState
    = PlacementGroupStateAvailable -- ^ available
    | PlacementGroupStateDeleted -- ^ deleted
    | PlacementGroupStateDeleting -- ^ deleting
    | PlacementGroupStatePending -- ^ pending
      deriving (Eq, Show, Generic)

instance Hashable PlacementGroupState

instance FromText PlacementGroupState where
    parser = match "available" PlacementGroupStateAvailable
         <|> match "deleted" PlacementGroupStateDeleted
         <|> match "deleting" PlacementGroupStateDeleting
         <|> match "pending" PlacementGroupStatePending

instance ToText PlacementGroupState where
    toText PlacementGroupStateAvailable = "available"
    toText PlacementGroupStateDeleted = "deleted"
    toText PlacementGroupStateDeleting = "deleting"
    toText PlacementGroupStatePending = "pending"

instance ToByteString PlacementGroupState

instance FromXML PlacementGroupState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "state"

instance ToQuery PlacementGroupState where
    toQuery = genericQuery def

-- | The placement strategy.
data PlacementStrategy
    = PlacementStrategyCluster -- ^ cluster
      deriving (Eq, Show, Generic)

instance Hashable PlacementStrategy

instance FromText PlacementStrategy where
    parser = match "cluster" PlacementStrategyCluster

instance ToText PlacementStrategy where
    toText PlacementStrategyCluster = "cluster"

instance ToByteString PlacementStrategy

instance FromXML PlacementStrategy where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PlacementStrategy"

instance ToQuery PlacementStrategy where
    toQuery = genericQuery def

-- | The instance operating system.
data PlatformValues
    = PlatformValuesWindows -- ^ Windows
      deriving (Eq, Show, Generic)

instance Hashable PlatformValues

instance FromText PlatformValues where
    parser = match "Windows" PlatformValuesWindows

instance ToText PlatformValues where
    toText PlatformValuesWindows = "Windows"

instance ToByteString PlatformValues

instance FromXML PlatformValues where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "platform"

instance ToQuery PlatformValues where
    toQuery = genericQuery def

-- | The type of product code.
data ProductCodeValues
    = ProductCodeValuesDevpay -- ^ devpay
    | ProductCodeValuesMarketplace -- ^ marketplace
      deriving (Eq, Show, Generic)

instance Hashable ProductCodeValues

instance FromText ProductCodeValues where
    parser = match "devpay" ProductCodeValuesDevpay
         <|> match "marketplace" ProductCodeValuesMarketplace

instance ToText ProductCodeValues where
    toText ProductCodeValuesDevpay = "devpay"
    toText ProductCodeValuesMarketplace = "marketplace"

instance ToByteString ProductCodeValues

instance FromXML ProductCodeValues where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "type"

instance ToQuery ProductCodeValues where
    toQuery = genericQuery def

-- | The Reserved Instance description.
data RIProductDescription
    = RIProductDescriptionLinuxUnix -- ^ Linux/UNIX
    | RIProductDescriptionLinuxUnixAmazonVpc -- ^ Linux/UNIX (Amazon VPC)
    | RIProductDescriptionWindows -- ^ Windows
    | RIProductDescriptionWindowsAmazonVpc -- ^ Windows (Amazon VPC)
      deriving (Eq, Show, Generic)

instance Hashable RIProductDescription

instance FromText RIProductDescription where
    parser = match "Linux/UNIX" RIProductDescriptionLinuxUnix
         <|> match "Linux/UNIX (Amazon VPC)" RIProductDescriptionLinuxUnixAmazonVpc
         <|> match "Windows" RIProductDescriptionWindows
         <|> match "Windows (Amazon VPC)" RIProductDescriptionWindowsAmazonVpc

instance ToText RIProductDescription where
    toText RIProductDescriptionLinuxUnix = "Linux/UNIX"
    toText RIProductDescriptionLinuxUnixAmazonVpc = "Linux/UNIX (Amazon VPC)"
    toText RIProductDescriptionWindows = "Windows"
    toText RIProductDescriptionWindowsAmazonVpc = "Windows (Amazon VPC)"

instance ToByteString RIProductDescription

instance FromXML RIProductDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "productDescription"

instance ToQuery RIProductDescription where
    toQuery = genericQuery def

-- | The frequency of the recurring charge.
data RecurringChargeFrequency
    = RecurringChargeFrequencyHourly -- ^ Hourly
      deriving (Eq, Show, Generic)

instance Hashable RecurringChargeFrequency

instance FromText RecurringChargeFrequency where
    parser = match "Hourly" RecurringChargeFrequencyHourly

instance ToText RecurringChargeFrequency where
    toText RecurringChargeFrequencyHourly = "Hourly"

instance ToByteString RecurringChargeFrequency

instance FromXML RecurringChargeFrequency where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "frequency"

instance ToQuery RecurringChargeFrequency where
    toQuery = genericQuery def

data ReportInstanceReasonCodes
    = ReportInstanceReasonCodesInstanceStuckInState -- ^ instance-stuck-in-state
    | ReportInstanceReasonCodesNotAcceptingCredentials -- ^ not-accepting-credentials
    | ReportInstanceReasonCodesOther -- ^ other
    | ReportInstanceReasonCodesPasswordNotAvailable -- ^ password-not-available
    | ReportInstanceReasonCodesPerformanceEbsVolume -- ^ performance-ebs-volume
    | ReportInstanceReasonCodesPerformanceInstanceStore -- ^ performance-instance-store
    | ReportInstanceReasonCodesPerformanceNetwork -- ^ performance-network
    | ReportInstanceReasonCodesPerformanceOther -- ^ performance-other
    | ReportInstanceReasonCodesUnresponsive -- ^ unresponsive
      deriving (Eq, Show, Generic)

instance Hashable ReportInstanceReasonCodes

instance FromText ReportInstanceReasonCodes where
    parser = match "instance-stuck-in-state" ReportInstanceReasonCodesInstanceStuckInState
         <|> match "not-accepting-credentials" ReportInstanceReasonCodesNotAcceptingCredentials
         <|> match "other" ReportInstanceReasonCodesOther
         <|> match "password-not-available" ReportInstanceReasonCodesPasswordNotAvailable
         <|> match "performance-ebs-volume" ReportInstanceReasonCodesPerformanceEbsVolume
         <|> match "performance-instance-store" ReportInstanceReasonCodesPerformanceInstanceStore
         <|> match "performance-network" ReportInstanceReasonCodesPerformanceNetwork
         <|> match "performance-other" ReportInstanceReasonCodesPerformanceOther
         <|> match "unresponsive" ReportInstanceReasonCodesUnresponsive

instance ToText ReportInstanceReasonCodes where
    toText ReportInstanceReasonCodesInstanceStuckInState = "instance-stuck-in-state"
    toText ReportInstanceReasonCodesNotAcceptingCredentials = "not-accepting-credentials"
    toText ReportInstanceReasonCodesOther = "other"
    toText ReportInstanceReasonCodesPasswordNotAvailable = "password-not-available"
    toText ReportInstanceReasonCodesPerformanceEbsVolume = "performance-ebs-volume"
    toText ReportInstanceReasonCodesPerformanceInstanceStore = "performance-instance-store"
    toText ReportInstanceReasonCodesPerformanceNetwork = "performance-network"
    toText ReportInstanceReasonCodesPerformanceOther = "performance-other"
    toText ReportInstanceReasonCodesUnresponsive = "unresponsive"

instance ToByteString ReportInstanceReasonCodes

instance ToQuery ReportInstanceReasonCodes where
    toQuery = genericQuery def

-- | The status of all instances listed.
data ReportStatusType
    = ReportStatusTypeImpaired -- ^ impaired
    | ReportStatusTypeOk -- ^ ok
      deriving (Eq, Show, Generic)

instance Hashable ReportStatusType

instance FromText ReportStatusType where
    parser = match "impaired" ReportStatusTypeImpaired
         <|> match "ok" ReportStatusTypeOk

instance ToText ReportStatusType where
    toText ReportStatusTypeImpaired = "impaired"
    toText ReportStatusTypeOk = "ok"

instance ToByteString ReportStatusType

instance ToQuery ReportStatusType where
    toQuery = genericQuery def

-- | The state of the Reserved Instance purchase.
data ReservedInstanceState
    = ReservedInstanceStateActive -- ^ active
    | ReservedInstanceStatePaymentFailed -- ^ payment-failed
    | ReservedInstanceStatePaymentPending -- ^ payment-pending
    | ReservedInstanceStateRetired -- ^ retired
      deriving (Eq, Show, Generic)

instance Hashable ReservedInstanceState

instance FromText ReservedInstanceState where
    parser = match "active" ReservedInstanceStateActive
         <|> match "payment-failed" ReservedInstanceStatePaymentFailed
         <|> match "payment-pending" ReservedInstanceStatePaymentPending
         <|> match "retired" ReservedInstanceStateRetired

instance ToText ReservedInstanceState where
    toText ReservedInstanceStateActive = "active"
    toText ReservedInstanceStatePaymentFailed = "payment-failed"
    toText ReservedInstanceStatePaymentPending = "payment-pending"
    toText ReservedInstanceStateRetired = "retired"

instance ToByteString ReservedInstanceState

instance FromXML ReservedInstanceState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "state"

instance ToQuery ReservedInstanceState where
    toQuery = genericQuery def

-- | The attribute to reset (currently you can only reset the launch permission
-- attribute).
data ResetImageAttributeName
    = ResetImageAttributeNameLaunchPermission -- ^ launchPermission
      deriving (Eq, Show, Generic)

instance Hashable ResetImageAttributeName

instance FromText ResetImageAttributeName where
    parser = match "launchPermission" ResetImageAttributeNameLaunchPermission

instance ToText ResetImageAttributeName where
    toText ResetImageAttributeNameLaunchPermission = "launchPermission"

instance ToByteString ResetImageAttributeName

instance ToQuery ResetImageAttributeName where
    toQuery = genericQuery def

-- | The type of resource.
data ResourceType
    = ResourceTypeCustomerGateway -- ^ customer-gateway
    | ResourceTypeDhcpOptions -- ^ dhcp-options
    | ResourceTypeImage -- ^ image
    | ResourceTypeInstance -- ^ instance
    | ResourceTypeInternetGateway -- ^ internet-gateway
    | ResourceTypeNetworkAcl -- ^ network-acl
    | ResourceTypeNetworkInterface -- ^ network-interface
    | ResourceTypeReservedInstances -- ^ reserved-instances
    | ResourceTypeRouteTable -- ^ route-table
    | ResourceTypeSecurityGroup -- ^ security-group
    | ResourceTypeSnapshot -- ^ snapshot
    | ResourceTypeSpotInstancesRequest -- ^ spot-instances-request
    | ResourceTypeSubnet -- ^ subnet
    | ResourceTypeVolume -- ^ volume
    | ResourceTypeVpc -- ^ vpc
    | ResourceTypeVpnConnection -- ^ vpn-connection
    | ResourceTypeVpnGateway -- ^ vpn-gateway
      deriving (Eq, Show, Generic)

instance Hashable ResourceType

instance FromText ResourceType where
    parser = match "customer-gateway" ResourceTypeCustomerGateway
         <|> match "dhcp-options" ResourceTypeDhcpOptions
         <|> match "image" ResourceTypeImage
         <|> match "instance" ResourceTypeInstance
         <|> match "internet-gateway" ResourceTypeInternetGateway
         <|> match "network-acl" ResourceTypeNetworkAcl
         <|> match "network-interface" ResourceTypeNetworkInterface
         <|> match "reserved-instances" ResourceTypeReservedInstances
         <|> match "route-table" ResourceTypeRouteTable
         <|> match "security-group" ResourceTypeSecurityGroup
         <|> match "snapshot" ResourceTypeSnapshot
         <|> match "spot-instances-request" ResourceTypeSpotInstancesRequest
         <|> match "subnet" ResourceTypeSubnet
         <|> match "volume" ResourceTypeVolume
         <|> match "vpc" ResourceTypeVpc
         <|> match "vpn-connection" ResourceTypeVpnConnection
         <|> match "vpn-gateway" ResourceTypeVpnGateway

instance ToText ResourceType where
    toText ResourceTypeCustomerGateway = "customer-gateway"
    toText ResourceTypeDhcpOptions = "dhcp-options"
    toText ResourceTypeImage = "image"
    toText ResourceTypeInstance = "instance"
    toText ResourceTypeInternetGateway = "internet-gateway"
    toText ResourceTypeNetworkAcl = "network-acl"
    toText ResourceTypeNetworkInterface = "network-interface"
    toText ResourceTypeReservedInstances = "reserved-instances"
    toText ResourceTypeRouteTable = "route-table"
    toText ResourceTypeSecurityGroup = "security-group"
    toText ResourceTypeSnapshot = "snapshot"
    toText ResourceTypeSpotInstancesRequest = "spot-instances-request"
    toText ResourceTypeSubnet = "subnet"
    toText ResourceTypeVolume = "volume"
    toText ResourceTypeVpc = "vpc"
    toText ResourceTypeVpnConnection = "vpn-connection"
    toText ResourceTypeVpnGateway = "vpn-gateway"

instance ToByteString ResourceType

instance FromXML ResourceType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "resourceType"

instance ToQuery ResourceType where
    toQuery = genericQuery def

-- | Describes how the route was created. CreateRouteTable indicates that route
-- was automatically created when the route table was created. CreateRoute
-- indicates that the route was manually added to the route table.
-- EnableVgwRoutePropagation indicates that the route was propagated by route
-- propagation.
data RouteOrigin
    = RouteOriginCreateRoute -- ^ CreateRoute
    | RouteOriginCreateRouteTable -- ^ CreateRouteTable
    | RouteOriginEnableVgwRoutePropagation -- ^ EnableVgwRoutePropagation
      deriving (Eq, Show, Generic)

instance Hashable RouteOrigin

instance FromText RouteOrigin where
    parser = match "CreateRoute" RouteOriginCreateRoute
         <|> match "CreateRouteTable" RouteOriginCreateRouteTable
         <|> match "EnableVgwRoutePropagation" RouteOriginEnableVgwRoutePropagation

instance ToText RouteOrigin where
    toText RouteOriginCreateRoute = "CreateRoute"
    toText RouteOriginCreateRouteTable = "CreateRouteTable"
    toText RouteOriginEnableVgwRoutePropagation = "EnableVgwRoutePropagation"

instance ToByteString RouteOrigin

instance FromXML RouteOrigin where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "origin"

instance ToQuery RouteOrigin where
    toQuery = genericQuery def

-- | The state of the route. The blackhole state indicates that the route's
-- target isn't available (for example, the specified gateway isn't attached
-- to the VPC, or the specified NAT instance has been terminated).
data RouteState
    = RouteStateActive -- ^ active
    | RouteStateBlackhole -- ^ blackhole
      deriving (Eq, Show, Generic)

instance Hashable RouteState

instance FromText RouteState where
    parser = match "active" RouteStateActive
         <|> match "blackhole" RouteStateBlackhole

instance ToText RouteState where
    toText RouteStateActive = "active"
    toText RouteStateBlackhole = "blackhole"

instance ToByteString RouteState

instance FromXML RouteState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "state"

instance ToQuery RouteState where
    toQuery = genericQuery def

-- | Indicates whether to allow or deny the traffic that matches the rule.
data RuleAction
    = RuleActionAllow -- ^ allow
    | RuleActionDeny -- ^ deny
      deriving (Eq, Show, Generic)

instance Hashable RuleAction

instance FromText RuleAction where
    parser = match "allow" RuleActionAllow
         <|> match "deny" RuleActionDeny

instance ToText RuleAction where
    toText RuleActionAllow = "allow"
    toText RuleActionDeny = "deny"

instance ToByteString RuleAction

instance FromXML RuleAction where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ruleAction"

instance ToQuery RuleAction where
    toQuery = genericQuery def

-- | Indicates whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for system
-- shutdown).
data ShutdownBehavior
    = ShutdownBehaviorStop -- ^ stop
    | ShutdownBehaviorTerminate -- ^ terminate
      deriving (Eq, Show, Generic)

instance Hashable ShutdownBehavior

instance FromText ShutdownBehavior where
    parser = match "stop" ShutdownBehaviorStop
         <|> match "terminate" ShutdownBehaviorTerminate

instance ToText ShutdownBehavior where
    toText ShutdownBehaviorStop = "stop"
    toText ShutdownBehaviorTerminate = "terminate"

instance ToByteString ShutdownBehavior

instance ToQuery ShutdownBehavior where
    toQuery = genericQuery def

-- | The snapshot attribute you would like to view.
data SnapshotAttributeName
    = SnapshotAttributeNameCreateVolumePermission -- ^ createVolumePermission
    | SnapshotAttributeNameProductCodes -- ^ productCodes
      deriving (Eq, Show, Generic)

instance Hashable SnapshotAttributeName

instance FromText SnapshotAttributeName where
    parser = match "createVolumePermission" SnapshotAttributeNameCreateVolumePermission
         <|> match "productCodes" SnapshotAttributeNameProductCodes

instance ToText SnapshotAttributeName where
    toText SnapshotAttributeNameCreateVolumePermission = "createVolumePermission"
    toText SnapshotAttributeNameProductCodes = "productCodes"

instance ToByteString SnapshotAttributeName

instance ToQuery SnapshotAttributeName where
    toQuery = genericQuery def

-- | The snapshot state.
data SnapshotState
    = SnapshotStateCompleted -- ^ completed
    | SnapshotStateError -- ^ error
    | SnapshotStatePending -- ^ pending
      deriving (Eq, Show, Generic)

instance Hashable SnapshotState

instance FromText SnapshotState where
    parser = match "completed" SnapshotStateCompleted
         <|> match "error" SnapshotStateError
         <|> match "pending" SnapshotStatePending

instance ToText SnapshotState where
    toText SnapshotStateCompleted = "completed"
    toText SnapshotStateError = "error"
    toText SnapshotStatePending = "pending"

instance ToByteString SnapshotState

instance FromXML SnapshotState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "status"

instance ToQuery SnapshotState where
    toQuery = genericQuery def

-- | The state of the Spot Instance request. Spot bid status information can
-- help you track your Spot Instance requests. For information, see Tracking
-- Spot Requests with Bid Status Codes in the Amazon Elastic Compute Cloud
-- User Guide.
data SpotInstanceState
    = SpotInstanceStateActive -- ^ active
    | SpotInstanceStateCancelled -- ^ cancelled
    | SpotInstanceStateClosed -- ^ closed
    | SpotInstanceStateFailed -- ^ failed
    | SpotInstanceStateOpen -- ^ open
      deriving (Eq, Show, Generic)

instance Hashable SpotInstanceState

instance FromText SpotInstanceState where
    parser = match "active" SpotInstanceStateActive
         <|> match "cancelled" SpotInstanceStateCancelled
         <|> match "closed" SpotInstanceStateClosed
         <|> match "failed" SpotInstanceStateFailed
         <|> match "open" SpotInstanceStateOpen

instance ToText SpotInstanceState where
    toText SpotInstanceStateActive = "active"
    toText SpotInstanceStateCancelled = "cancelled"
    toText SpotInstanceStateClosed = "closed"
    toText SpotInstanceStateFailed = "failed"
    toText SpotInstanceStateOpen = "open"

instance ToByteString SpotInstanceState

instance FromXML SpotInstanceState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "state"

instance ToQuery SpotInstanceState where
    toQuery = genericQuery def

-- | The Spot Instance request type.
data SpotInstanceType
    = SpotInstanceTypeOneTime -- ^ one-time
    | SpotInstanceTypePersistent -- ^ persistent
      deriving (Eq, Show, Generic)

instance Hashable SpotInstanceType

instance FromText SpotInstanceType where
    parser = match "one-time" SpotInstanceTypeOneTime
         <|> match "persistent" SpotInstanceTypePersistent

instance ToText SpotInstanceType where
    toText SpotInstanceTypeOneTime = "one-time"
    toText SpotInstanceTypePersistent = "persistent"

instance ToByteString SpotInstanceType

instance FromXML SpotInstanceType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "type"

instance ToQuery SpotInstanceType where
    toQuery = genericQuery def

-- | The type of instance status.
data StatusName
    = StatusNameReachability -- ^ reachability
      deriving (Eq, Show, Generic)

instance Hashable StatusName

instance FromText StatusName where
    parser = match "reachability" StatusNameReachability

instance ToText StatusName where
    toText StatusNameReachability = "reachability"

instance ToByteString StatusName

instance FromXML StatusName where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "name"

instance ToQuery StatusName where
    toQuery = genericQuery def

-- | The status.
data StatusType
    = StatusTypeFailed -- ^ failed
    | StatusTypeInsufficientData -- ^ insufficient-data
    | StatusTypePassed -- ^ passed
      deriving (Eq, Show, Generic)

instance Hashable StatusType

instance FromText StatusType where
    parser = match "failed" StatusTypeFailed
         <|> match "insufficient-data" StatusTypeInsufficientData
         <|> match "passed" StatusTypePassed

instance ToText StatusType where
    toText StatusTypeFailed = "failed"
    toText StatusTypeInsufficientData = "insufficient-data"
    toText StatusTypePassed = "passed"

instance ToByteString StatusType

instance FromXML StatusType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "status"

instance ToQuery StatusType where
    toQuery = genericQuery def

-- | The current state of the subnet.
data SubnetState
    = SubnetStateAvailable -- ^ available
    | SubnetStatePending -- ^ pending
      deriving (Eq, Show, Generic)

instance Hashable SubnetState

instance FromText SubnetState where
    parser = match "available" SubnetStateAvailable
         <|> match "pending" SubnetStatePending

instance ToText SubnetState where
    toText SubnetStateAvailable = "available"
    toText SubnetStatePending = "pending"

instance ToByteString SubnetState

instance FromXML SubnetState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "state"

instance ToQuery SubnetState where
    toQuery = genericQuery def

-- | The status.
data SummaryStatus
    = SummaryStatusImpaired -- ^ impaired
    | SummaryStatusInsufficientData -- ^ insufficient-data
    | SummaryStatusNotApplicable -- ^ not-applicable
    | SummaryStatusOk -- ^ ok
      deriving (Eq, Show, Generic)

instance Hashable SummaryStatus

instance FromText SummaryStatus where
    parser = match "impaired" SummaryStatusImpaired
         <|> match "insufficient-data" SummaryStatusInsufficientData
         <|> match "not-applicable" SummaryStatusNotApplicable
         <|> match "ok" SummaryStatusOk

instance ToText SummaryStatus where
    toText SummaryStatusImpaired = "impaired"
    toText SummaryStatusInsufficientData = "insufficient-data"
    toText SummaryStatusNotApplicable = "not-applicable"
    toText SummaryStatusOk = "ok"

instance ToByteString SummaryStatus

instance FromXML SummaryStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "status"

instance ToQuery SummaryStatus where
    toQuery = genericQuery def

-- | The status of the VPN tunnel.
data TelemetryStatus
    = TelemetryStatusDown -- ^ DOWN
    | TelemetryStatusUp -- ^ UP
      deriving (Eq, Show, Generic)

instance Hashable TelemetryStatus

instance FromText TelemetryStatus where
    parser = match "DOWN" TelemetryStatusDown
         <|> match "UP" TelemetryStatusUp

instance ToText TelemetryStatus where
    toText TelemetryStatusDown = "DOWN"
    toText TelemetryStatusUp = "UP"

instance ToByteString TelemetryStatus

instance FromXML TelemetryStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "status"

instance ToQuery TelemetryStatus where
    toQuery = genericQuery def

-- | The supported tenancy options for instances launched into the VPC. A value
-- of default means that instances can be launched with any tenancy; a value
-- of dedicated means all instances launched into the VPC are launched as
-- dedicated tenancy instances regardless of the tenancy assigned to the
-- instance at launch. Dedicated tenancy instances runs on single-tenant
-- hardware. Default: default.
data Tenancy
    = TenancyDedicated -- ^ dedicated
    | TenancyDefault -- ^ default
      deriving (Eq, Show, Generic)

instance Hashable Tenancy

instance FromText Tenancy where
    parser = match "dedicated" TenancyDedicated
         <|> match "default" TenancyDefault

instance ToText Tenancy where
    toText TenancyDedicated = "dedicated"
    toText TenancyDefault = "default"

instance ToByteString Tenancy

instance FromXML Tenancy where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Tenancy"

instance ToQuery Tenancy where
    toQuery = genericQuery def

-- | The type of virtualization of the AMI.
data VirtualizationType
    = VirtualizationTypeHvm -- ^ hvm
    | VirtualizationTypeParavirtual -- ^ paravirtual
      deriving (Eq, Show, Generic)

instance Hashable VirtualizationType

instance FromText VirtualizationType where
    parser = match "hvm" VirtualizationTypeHvm
         <|> match "paravirtual" VirtualizationTypeParavirtual

instance ToText VirtualizationType where
    toText VirtualizationTypeHvm = "hvm"
    toText VirtualizationTypeParavirtual = "paravirtual"

instance ToByteString VirtualizationType

instance FromXML VirtualizationType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "virtualizationType"

instance ToQuery VirtualizationType where
    toQuery = genericQuery def

-- | The attachment state of the volume.
data VolumeAttachmentState
    = VolumeAttachmentStateAttached -- ^ attached
    | VolumeAttachmentStateAttaching -- ^ attaching
    | VolumeAttachmentStateDetached -- ^ detached
    | VolumeAttachmentStateDetaching -- ^ detaching
      deriving (Eq, Show, Generic)

instance Hashable VolumeAttachmentState

instance FromText VolumeAttachmentState where
    parser = match "attached" VolumeAttachmentStateAttached
         <|> match "attaching" VolumeAttachmentStateAttaching
         <|> match "detached" VolumeAttachmentStateDetached
         <|> match "detaching" VolumeAttachmentStateDetaching

instance ToText VolumeAttachmentState where
    toText VolumeAttachmentStateAttached = "attached"
    toText VolumeAttachmentStateAttaching = "attaching"
    toText VolumeAttachmentStateDetached = "detached"
    toText VolumeAttachmentStateDetaching = "detaching"

instance ToByteString VolumeAttachmentState

instance FromXML VolumeAttachmentState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "status"

instance ToQuery VolumeAttachmentState where
    toQuery = genericQuery def

-- | The instance attribute.
data VolumeAttributeName
    = VolumeAttributeNameAutoEnableIO -- ^ autoEnableIO
    | VolumeAttributeNameProductCodes -- ^ productCodes
      deriving (Eq, Show, Generic)

instance Hashable VolumeAttributeName

instance FromText VolumeAttributeName where
    parser = match "autoEnableIO" VolumeAttributeNameAutoEnableIO
         <|> match "productCodes" VolumeAttributeNameProductCodes

instance ToText VolumeAttributeName where
    toText VolumeAttributeNameAutoEnableIO = "autoEnableIO"
    toText VolumeAttributeNameProductCodes = "productCodes"

instance ToByteString VolumeAttributeName

instance ToQuery VolumeAttributeName where
    toQuery = genericQuery def

-- | The volume state.
data VolumeState
    = VolumeStateAvailable -- ^ available
    | VolumeStateCreating -- ^ creating
    | VolumeStateDeleted -- ^ deleted
    | VolumeStateDeleting -- ^ deleting
    | VolumeStateError -- ^ error
    | VolumeStateInUse -- ^ in-use
      deriving (Eq, Show, Generic)

instance Hashable VolumeState

instance FromText VolumeState where
    parser = match "available" VolumeStateAvailable
         <|> match "creating" VolumeStateCreating
         <|> match "deleted" VolumeStateDeleted
         <|> match "deleting" VolumeStateDeleting
         <|> match "error" VolumeStateError
         <|> match "in-use" VolumeStateInUse

instance ToText VolumeState where
    toText VolumeStateAvailable = "available"
    toText VolumeStateCreating = "creating"
    toText VolumeStateDeleted = "deleted"
    toText VolumeStateDeleting = "deleting"
    toText VolumeStateError = "error"
    toText VolumeStateInUse = "in-use"

instance ToByteString VolumeState

instance FromXML VolumeState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "status"

instance ToQuery VolumeState where
    toQuery = genericQuery def

-- | The status of the volume.
data VolumeStatusInfoStatus
    = VolumeStatusInfoStatusImpaired -- ^ impaired
    | VolumeStatusInfoStatusInsufficientData -- ^ insufficient-data
    | VolumeStatusInfoStatusOk -- ^ ok
      deriving (Eq, Show, Generic)

instance Hashable VolumeStatusInfoStatus

instance FromText VolumeStatusInfoStatus where
    parser = match "impaired" VolumeStatusInfoStatusImpaired
         <|> match "insufficient-data" VolumeStatusInfoStatusInsufficientData
         <|> match "ok" VolumeStatusInfoStatusOk

instance ToText VolumeStatusInfoStatus where
    toText VolumeStatusInfoStatusImpaired = "impaired"
    toText VolumeStatusInfoStatusInsufficientData = "insufficient-data"
    toText VolumeStatusInfoStatusOk = "ok"

instance ToByteString VolumeStatusInfoStatus

instance FromXML VolumeStatusInfoStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "status"

instance ToQuery VolumeStatusInfoStatus where
    toQuery = genericQuery def

-- | The name of the volume status.
data VolumeStatusName
    = VolumeStatusNameIoEnabled -- ^ io-enabled
    | VolumeStatusNameIoPerformance -- ^ io-performance
      deriving (Eq, Show, Generic)

instance Hashable VolumeStatusName

instance FromText VolumeStatusName where
    parser = match "io-enabled" VolumeStatusNameIoEnabled
         <|> match "io-performance" VolumeStatusNameIoPerformance

instance ToText VolumeStatusName where
    toText VolumeStatusNameIoEnabled = "io-enabled"
    toText VolumeStatusNameIoPerformance = "io-performance"

instance ToByteString VolumeStatusName

instance FromXML VolumeStatusName where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "name"

instance ToQuery VolumeStatusName where
    toQuery = genericQuery def

-- | The volume type. gp2 for General Purpose (SSD) volumes, io1 for Provisioned
-- IOPS (SSD) volumes, and standard for Magnetic volumes. Default: standard.
data VolumeType
    = VolumeTypeGp2 -- ^ gp2
    | VolumeTypeIo1 -- ^ io1
    | VolumeTypeStandard -- ^ standard
      deriving (Eq, Show, Generic)

instance Hashable VolumeType

instance FromText VolumeType where
    parser = match "gp2" VolumeTypeGp2
         <|> match "io1" VolumeTypeIo1
         <|> match "standard" VolumeTypeStandard

instance ToText VolumeType where
    toText VolumeTypeGp2 = "gp2"
    toText VolumeTypeIo1 = "io1"
    toText VolumeTypeStandard = "standard"

instance ToByteString VolumeType

instance FromXML VolumeType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "VolumeType"

instance ToQuery VolumeType where
    toQuery = genericQuery def

-- | The VPC attribute.
data VpcAttributeName
    = VpcAttributeNameEnableDnsHostnames -- ^ enableDnsHostnames
    | VpcAttributeNameEnableDnsSupport -- ^ enableDnsSupport
      deriving (Eq, Show, Generic)

instance Hashable VpcAttributeName

instance FromText VpcAttributeName where
    parser = match "enableDnsHostnames" VpcAttributeNameEnableDnsHostnames
         <|> match "enableDnsSupport" VpcAttributeNameEnableDnsSupport

instance ToText VpcAttributeName where
    toText VpcAttributeNameEnableDnsHostnames = "enableDnsHostnames"
    toText VpcAttributeNameEnableDnsSupport = "enableDnsSupport"

instance ToByteString VpcAttributeName

instance ToQuery VpcAttributeName where
    toQuery = genericQuery def

-- | The current state of the VPC.
data VpcState
    = VpcStateAvailable -- ^ available
    | VpcStatePending -- ^ pending
      deriving (Eq, Show, Generic)

instance Hashable VpcState

instance FromText VpcState where
    parser = match "available" VpcStateAvailable
         <|> match "pending" VpcStatePending

instance ToText VpcState where
    toText VpcStateAvailable = "available"
    toText VpcStatePending = "pending"

instance ToByteString VpcState

instance FromXML VpcState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "state"

instance ToQuery VpcState where
    toQuery = genericQuery def

-- | The current state of the VPN connection.
data VpnState
    = VpnStateAvailable -- ^ available
    | VpnStateDeleted -- ^ deleted
    | VpnStateDeleting -- ^ deleting
    | VpnStatePending -- ^ pending
      deriving (Eq, Show, Generic)

instance Hashable VpnState

instance FromText VpnState where
    parser = match "available" VpnStateAvailable
         <|> match "deleted" VpnStateDeleted
         <|> match "deleting" VpnStateDeleting
         <|> match "pending" VpnStatePending

instance ToText VpnState where
    toText VpnStateAvailable = "available"
    toText VpnStateDeleted = "deleted"
    toText VpnStateDeleting = "deleting"
    toText VpnStatePending = "pending"

instance ToByteString VpnState

instance FromXML VpnState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "state"

instance ToQuery VpnState where
    toQuery = genericQuery def

-- | Indicates how the routes were provided.
data VpnStaticRouteSource
    = VpnStaticRouteSourceStatic -- ^ Static
      deriving (Eq, Show, Generic)

instance Hashable VpnStaticRouteSource

instance FromText VpnStaticRouteSource where
    parser = match "Static" VpnStaticRouteSourceStatic

instance ToText VpnStaticRouteSource where
    toText VpnStaticRouteSourceStatic = "Static"

instance ToByteString VpnStaticRouteSource

instance FromXML VpnStaticRouteSource where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "source"

instance ToQuery VpnStaticRouteSource where
    toQuery = genericQuery def

-- | Describes a value of an account attribute.
newtype AccountAttributeValue = AccountAttributeValue
    { _aavAttributeValue :: Maybe Text
      -- ^ The value.
    } deriving (Show, Generic)

-- | The value.
aavAttributeValue :: Lens' AccountAttributeValue (Maybe Text)
aavAttributeValue = lens _aavAttributeValue (\s a -> s { _aavAttributeValue = a })
{-# INLINE aavAttributeValue #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AccountAttributeValue' data type to populate a request.
mkAccountAttributeValue :: AccountAttributeValue
mkAccountAttributeValue = AccountAttributeValue
    { _aavAttributeValue = Nothing
    }
{-# INLINE mkAccountAttributeValue #-}

instance FromXML AccountAttributeValue where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery AccountAttributeValue where
    toQuery = genericQuery def

-- | If the value is true, you can't terminate the instance through the Amazon
-- EC2 console, CLI, or API; otherwise, you can.
newtype AttributeBooleanValue = AttributeBooleanValue
    { _abvValue :: Maybe Bool
      -- ^ 
    } deriving (Show, Generic)

-- | 
abvValue :: Lens' AttributeBooleanValue (Maybe Bool)
abvValue = lens _abvValue (\s a -> s { _abvValue = a })
{-# INLINE abvValue #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AttributeBooleanValue' data type to populate a request.
mkAttributeBooleanValue :: AttributeBooleanValue
mkAttributeBooleanValue = AttributeBooleanValue
    { _abvValue = Nothing
    }
{-# INLINE mkAttributeBooleanValue #-}

instance FromXML AttributeBooleanValue where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "disableApiTermination"

instance ToQuery AttributeBooleanValue where
    toQuery = genericQuery def

-- | The kernel ID.
newtype AttributeValue = AttributeValue
    { _axValue :: Maybe Text
      -- ^ 
    } deriving (Show, Generic)

-- | 
axValue :: Lens' AttributeValue (Maybe Text)
axValue = lens _axValue (\s a -> s { _axValue = a })
{-# INLINE axValue #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AttributeValue' data type to populate a request.
mkAttributeValue :: AttributeValue
mkAttributeValue = AttributeValue
    { _axValue = Nothing
    }
{-# INLINE mkAttributeValue #-}

instance FromXML AttributeValue where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "kernel"

instance ToQuery AttributeValue where
    toQuery = genericQuery def

-- | Describes a message about an Availability Zone.
newtype AvailabilityZoneMessage = AvailabilityZoneMessage
    { _azmMessage :: Maybe Text
      -- ^ The message about the Availability Zone.
    } deriving (Show, Generic)

-- | The message about the Availability Zone.
azmMessage :: Lens' AvailabilityZoneMessage (Maybe Text)
azmMessage = lens _azmMessage (\s a -> s { _azmMessage = a })
{-# INLINE azmMessage #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AvailabilityZoneMessage' data type to populate a request.
mkAvailabilityZoneMessage :: AvailabilityZoneMessage
mkAvailabilityZoneMessage = AvailabilityZoneMessage
    { _azmMessage = Nothing
    }
{-# INLINE mkAvailabilityZoneMessage #-}

instance FromXML AvailabilityZoneMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery AvailabilityZoneMessage where
    toQuery = genericQuery def

-- | Describes an IP range.
newtype IpRange = IpRange
    { _iuCidrIp :: Text
      -- ^ The CIDR range. You can either specify a CIDR range or a source
      -- security group, not both.
    } deriving (Show, Generic)

-- | The CIDR range. You can either specify a CIDR range or a source security
-- group, not both.
iuCidrIp :: Lens' IpRange (Text)
iuCidrIp = lens _iuCidrIp (\s a -> s { _iuCidrIp = a })
{-# INLINE iuCidrIp #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'IpRange' data type to populate a request.
mkIpRange :: Text -- ^ 'iuCidrIp'
          -> IpRange
mkIpRange p1 = IpRange
    { _iuCidrIp = p1
    }
{-# INLINE mkIpRange #-}

instance FromXML IpRange where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "IpRange"

instance ToQuery IpRange where
    toQuery = genericQuery def

-- | The monitoring information for the instance.
newtype Monitoring = Monitoring
    { _mgState :: Maybe MonitoringState
      -- ^ Indicates whether monitoring is enabled for the instance.
    } deriving (Show, Generic)

-- | Indicates whether monitoring is enabled for the instance.
mgState :: Lens' Monitoring (Maybe MonitoringState)
mgState = lens _mgState (\s a -> s { _mgState = a })
{-# INLINE mgState #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Monitoring' data type to populate a request.
mkMonitoring :: Monitoring
mkMonitoring = Monitoring
    { _mgState = Nothing
    }
{-# INLINE mkMonitoring #-}

instance FromXML Monitoring where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "monitoring"

instance ToQuery Monitoring where
    toQuery = genericQuery def

-- | Describes a virtual private gateway propagating route.
newtype PropagatingVgw = PropagatingVgw
    { _pwGatewayId :: Maybe Text
      -- ^ The ID of the virtual private gateway (VGW).
    } deriving (Show, Generic)

-- | The ID of the virtual private gateway (VGW).
pwGatewayId :: Lens' PropagatingVgw (Maybe Text)
pwGatewayId = lens _pwGatewayId (\s a -> s { _pwGatewayId = a })
{-# INLINE pwGatewayId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'PropagatingVgw' data type to populate a request.
mkPropagatingVgw :: PropagatingVgw
mkPropagatingVgw = PropagatingVgw
    { _pwGatewayId = Nothing
    }
{-# INLINE mkPropagatingVgw #-}

instance FromXML PropagatingVgw where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery PropagatingVgw where
    toQuery = genericQuery def

-- | Describes the ID of a Reserved Instance.
newtype ReservedInstancesId = ReservedInstancesId
    { _rijReservedInstancesId :: Maybe Text
      -- ^ The ID of the Reserved Instance.
    } deriving (Show, Generic)

-- | The ID of the Reserved Instance.
rijReservedInstancesId :: Lens' ReservedInstancesId (Maybe Text)
rijReservedInstancesId = lens _rijReservedInstancesId (\s a -> s { _rijReservedInstancesId = a })
{-# INLINE rijReservedInstancesId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ReservedInstancesId' data type to populate a request.
mkReservedInstancesId :: ReservedInstancesId
mkReservedInstancesId = ReservedInstancesId
    { _rijReservedInstancesId = Nothing
    }
{-# INLINE mkReservedInstancesId #-}

instance FromXML ReservedInstancesId where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery ReservedInstancesId where
    toQuery = genericQuery def

-- | The monitoring for the instance.
newtype RunInstancesMonitoringEnabled = RunInstancesMonitoringEnabled
    { _rimeEnabled :: Bool
      -- ^ Indicates whether monitoring is enabled for the instance.
    } deriving (Show, Generic)

-- | Indicates whether monitoring is enabled for the instance.
rimeEnabled :: Lens' RunInstancesMonitoringEnabled (Bool)
rimeEnabled = lens _rimeEnabled (\s a -> s { _rimeEnabled = a })
{-# INLINE rimeEnabled #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'RunInstancesMonitoringEnabled' data type to populate a request.
mkRunInstancesMonitoringEnabled :: Bool -- ^ 'rimeEnabled'
                                -> RunInstancesMonitoringEnabled
mkRunInstancesMonitoringEnabled p1 = RunInstancesMonitoringEnabled
    { _rimeEnabled = p1
    }
{-# INLINE mkRunInstancesMonitoringEnabled #-}

instance ToQuery RunInstancesMonitoringEnabled where
    toQuery = genericQuery def

-- | The bucket in which to store the AMI. You can specify a bucket that you
-- already own or a new bucket that Amazon EC2 creates on your behalf. If you
-- specify a bucket that belongs to someone else, Amazon EC2 returns an error.
newtype Storage = Storage
    { _seS3 :: Maybe S3Storage
      -- ^ An Amazon S3 storage location.
    } deriving (Show, Generic)

-- | An Amazon S3 storage location.
seS3 :: Lens' Storage (Maybe S3Storage)
seS3 = lens _seS3 (\s a -> s { _seS3 = a })
{-# INLINE seS3 #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Storage' data type to populate a request.
mkStorage :: Storage
mkStorage = Storage
    { _seS3 = Nothing
    }
{-# INLINE mkStorage #-}

instance FromXML Storage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Storage"

instance ToQuery Storage where
    toQuery = genericQuery def

-- | 
newtype VolumeDetail = VolumeDetail
    { _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvxSize :: Integer
      -- ^ The size of the volume, in GiB.
    } deriving (Show, Generic)

-- | The size of the volume, in GiB.
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvxSize :: Lens' VolumeDetail (Integer)
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvxSize = lens _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvxSize (\s a -> s { _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvxSize = a })
{-# INLINE vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvxSize #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VolumeDetail' data type to populate a request.
mkVolumeDetail :: Integer -- ^ 'vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvxSize'
               -> VolumeDetail
mkVolumeDetail p1 = VolumeDetail
    { _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvxSize = p1
    }
{-# INLINE mkVolumeDetail #-}

instance FromXML VolumeDetail where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "VolumeDetail"

instance ToQuery VolumeDetail where
    toQuery = genericQuery def

-- | The VPN connection options.
newtype VpnConnectionOptions = VpnConnectionOptions
    { _vcoStaticRoutesOnly :: Maybe Bool
      -- ^ Indicates whether the VPN connection uses static routes only.
      -- Static routes must be used for devices that don't support BGP.
    } deriving (Show, Generic)

-- | Indicates whether the VPN connection uses static routes only. Static routes
-- must be used for devices that don't support BGP.
vcoStaticRoutesOnly :: Lens' VpnConnectionOptions (Maybe Bool)
vcoStaticRoutesOnly = lens _vcoStaticRoutesOnly (\s a -> s { _vcoStaticRoutesOnly = a })
{-# INLINE vcoStaticRoutesOnly #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VpnConnectionOptions' data type to populate a request.
mkVpnConnectionOptions :: VpnConnectionOptions
mkVpnConnectionOptions = VpnConnectionOptions
    { _vcoStaticRoutesOnly = Nothing
    }
{-# INLINE mkVpnConnectionOptions #-}

instance FromXML VpnConnectionOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "options"

instance ToQuery VpnConnectionOptions where
    toQuery = genericQuery def

-- | Indicates whether the VPN connection requires static routes. If you are
-- creating a VPN connection for a device that does not support BGP, you must
-- specify true. Default: false.
newtype VpnConnectionOptionsSpecification = VpnConnectionOptionsSpecification
    { _vcosStaticRoutesOnly :: Maybe Bool
      -- ^ Indicates whether the VPN connection uses static routes only.
      -- Static routes must be used for devices that don't support BGP.
    } deriving (Show, Generic)

-- | Indicates whether the VPN connection uses static routes only. Static routes
-- must be used for devices that don't support BGP.
vcosStaticRoutesOnly :: Lens' VpnConnectionOptionsSpecification (Maybe Bool)
vcosStaticRoutesOnly = lens _vcosStaticRoutesOnly (\s a -> s { _vcosStaticRoutesOnly = a })
{-# INLINE vcosStaticRoutesOnly #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VpnConnectionOptionsSpecification' data type to populate a request.
mkVpnConnectionOptionsSpecification :: VpnConnectionOptionsSpecification
mkVpnConnectionOptionsSpecification = VpnConnectionOptionsSpecification
    { _vcosStaticRoutesOnly = Nothing
    }
{-# INLINE mkVpnConnectionOptionsSpecification #-}

instance ToQuery VpnConnectionOptionsSpecification where
    toQuery = genericQuery def

-- | Describes an account attribute.
data AccountAttribute = AccountAttribute
    { _aaAttributeName :: Maybe Text
      -- ^ The name of the account attribute.
    , _aaAttributeValues :: [AccountAttributeValue]
      -- ^ One or more values for the account attribute.
    } deriving (Show, Generic)

-- | The name of the account attribute.
aaAttributeName :: Lens' AccountAttribute (Maybe Text)
aaAttributeName = lens _aaAttributeName (\s a -> s { _aaAttributeName = a })
{-# INLINE aaAttributeName #-}

-- | One or more values for the account attribute.
aaAttributeValues :: Lens' AccountAttribute ([AccountAttributeValue])
aaAttributeValues = lens _aaAttributeValues (\s a -> s { _aaAttributeValues = a })
{-# INLINE aaAttributeValues #-}

instance FromXML AccountAttribute where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes an Elastic IP address.
data Address = Address
    { _awInstanceId :: Maybe Text
      -- ^ The ID of the instance the address is associated with (if any).
    , _awPublicIp :: Maybe Text
      -- ^ The Elastic IP address.
    , _awAllocationId :: Maybe Text
      -- ^ The ID representing the allocation of the address for use with
      -- EC2-VPC.
    , _awAssociationId :: Maybe Text
      -- ^ The ID representing the association of the address with an
      -- instance in a VPC.
    , _awDomain :: Maybe DomainType
      -- ^ Indicates whether this Elastic IP address is for use with
      -- instances in EC2-Classic (standard) or instances in a VPC (vpc).
    , _awNetworkInterfaceId :: Maybe Text
      -- ^ The ID of the network interface.
    , _awNetworkInterfaceOwnerId :: Maybe Text
      -- ^ The ID of the AWS account that owns the network interface.
    , _awPrivateIpAddress :: Maybe Text
      -- ^ The private IP address associated with the Elastic IP address.
    } deriving (Show, Generic)

-- | The ID of the instance the address is associated with (if any).
awInstanceId :: Lens' Address (Maybe Text)
awInstanceId = lens _awInstanceId (\s a -> s { _awInstanceId = a })
{-# INLINE awInstanceId #-}

-- | The Elastic IP address.
awPublicIp :: Lens' Address (Maybe Text)
awPublicIp = lens _awPublicIp (\s a -> s { _awPublicIp = a })
{-# INLINE awPublicIp #-}

-- | The ID representing the allocation of the address for use with EC2-VPC.
awAllocationId :: Lens' Address (Maybe Text)
awAllocationId = lens _awAllocationId (\s a -> s { _awAllocationId = a })
{-# INLINE awAllocationId #-}

-- | The ID representing the association of the address with an instance in a
-- VPC.
awAssociationId :: Lens' Address (Maybe Text)
awAssociationId = lens _awAssociationId (\s a -> s { _awAssociationId = a })
{-# INLINE awAssociationId #-}

-- | Indicates whether this Elastic IP address is for use with instances in
-- EC2-Classic (standard) or instances in a VPC (vpc).
awDomain :: Lens' Address (Maybe DomainType)
awDomain = lens _awDomain (\s a -> s { _awDomain = a })
{-# INLINE awDomain #-}

-- | The ID of the network interface.
awNetworkInterfaceId :: Lens' Address (Maybe Text)
awNetworkInterfaceId = lens _awNetworkInterfaceId (\s a -> s { _awNetworkInterfaceId = a })
{-# INLINE awNetworkInterfaceId #-}

-- | The ID of the AWS account that owns the network interface.
awNetworkInterfaceOwnerId :: Lens' Address (Maybe Text)
awNetworkInterfaceOwnerId = lens _awNetworkInterfaceOwnerId (\s a -> s { _awNetworkInterfaceOwnerId = a })
{-# INLINE awNetworkInterfaceOwnerId #-}

-- | The private IP address associated with the Elastic IP address.
awPrivateIpAddress :: Lens' Address (Maybe Text)
awPrivateIpAddress = lens _awPrivateIpAddress (\s a -> s { _awPrivateIpAddress = a })
{-# INLINE awPrivateIpAddress #-}

instance FromXML Address where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes an Availability Zone.
data AvailabilityZone = AvailabilityZone
    { _azZoneName :: Maybe Text
      -- ^ The name of the Availability Zone.
    , _azState :: Maybe AvailabilityZoneState
      -- ^ The state of the Availability Zone.
    , _azRegionName :: Maybe Text
      -- ^ The name of the region.
    , _azMessages :: [AvailabilityZoneMessage]
      -- ^ Any messages about the Availability Zone.
    } deriving (Show, Generic)

-- | The name of the Availability Zone.
azZoneName :: Lens' AvailabilityZone (Maybe Text)
azZoneName = lens _azZoneName (\s a -> s { _azZoneName = a })
{-# INLINE azZoneName #-}

-- | The state of the Availability Zone.
azState :: Lens' AvailabilityZone (Maybe AvailabilityZoneState)
azState = lens _azState (\s a -> s { _azState = a })
{-# INLINE azState #-}

-- | The name of the region.
azRegionName :: Lens' AvailabilityZone (Maybe Text)
azRegionName = lens _azRegionName (\s a -> s { _azRegionName = a })
{-# INLINE azRegionName #-}

-- | Any messages about the Availability Zone.
azMessages :: Lens' AvailabilityZone ([AvailabilityZoneMessage])
azMessages = lens _azMessages (\s a -> s { _azMessages = a })
{-# INLINE azMessages #-}

instance FromXML AvailabilityZone where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes a block device mapping.
data BlockDeviceMapping = BlockDeviceMapping
    { _bdmVirtualName :: Maybe Text
      -- ^ The virtual device name.
    , _bdmDeviceName :: Text
      -- ^ The device name exposed to the instance (for example, /dev/sdh).
    , _bdmEbs :: Maybe EbsBlockDevice
      -- ^ Parameters used to automatically set up Amazon EBS volumes when
      -- the instance is launched.
    , _bdmNoDevice :: Maybe Text
      -- ^ Suppresses the specified device included in the block device
      -- mapping of the AMI.
    } deriving (Show, Generic)

-- | The virtual device name.
bdmVirtualName :: Lens' BlockDeviceMapping (Maybe Text)
bdmVirtualName = lens _bdmVirtualName (\s a -> s { _bdmVirtualName = a })
{-# INLINE bdmVirtualName #-}

-- | The device name exposed to the instance (for example, /dev/sdh).
bdmDeviceName :: Lens' BlockDeviceMapping (Text)
bdmDeviceName = lens _bdmDeviceName (\s a -> s { _bdmDeviceName = a })
{-# INLINE bdmDeviceName #-}

-- | Parameters used to automatically set up Amazon EBS volumes when the
-- instance is launched.
bdmEbs :: Lens' BlockDeviceMapping (Maybe EbsBlockDevice)
bdmEbs = lens _bdmEbs (\s a -> s { _bdmEbs = a })
{-# INLINE bdmEbs #-}

-- | Suppresses the specified device included in the block device mapping of the
-- AMI.
bdmNoDevice :: Lens' BlockDeviceMapping (Maybe Text)
bdmNoDevice = lens _bdmNoDevice (\s a -> s { _bdmNoDevice = a })
{-# INLINE bdmNoDevice #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'BlockDeviceMapping' data type to populate a request.
mkBlockDeviceMapping :: Text -- ^ 'bdmDeviceName'
                     -> BlockDeviceMapping
mkBlockDeviceMapping p1 = BlockDeviceMapping
    { _bdmVirtualName = Nothing
    , _bdmDeviceName = p2
    , _bdmEbs = Nothing
    , _bdmNoDevice = Nothing
    }
{-# INLINE mkBlockDeviceMapping #-}

instance FromXML BlockDeviceMapping where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "BlockDeviceMapping"

instance ToQuery BlockDeviceMapping where
    toQuery = genericQuery def

-- | Information about the bundle task.
data BundleTask = BundleTask
    { _btInstanceId :: Maybe Text
      -- ^ The ID of the instance associated with this bundle task.
    , _btBundleId :: Maybe Text
      -- ^ The ID for this bundle task.
    , _btState :: Maybe BundleTaskState
      -- ^ The state of the task.
    , _btStartTime :: Maybe ISO8601
      -- ^ The time this task started.
    , _btUpdateTime :: Maybe ISO8601
      -- ^ The time of the most recent update for the task.
    , _btStorage :: Maybe Storage
      -- ^ The Amazon S3 storage locations.
    , _btProgress :: Maybe Text
      -- ^ The level of task completion, as a percent (for example, 20%).
    , _btBundleTaskError :: Maybe BundleTaskError
      -- ^ If the task fails, a description of the error.
    } deriving (Show, Generic)

-- | The ID of the instance associated with this bundle task.
btInstanceId :: Lens' BundleTask (Maybe Text)
btInstanceId = lens _btInstanceId (\s a -> s { _btInstanceId = a })
{-# INLINE btInstanceId #-}

-- | The ID for this bundle task.
btBundleId :: Lens' BundleTask (Maybe Text)
btBundleId = lens _btBundleId (\s a -> s { _btBundleId = a })
{-# INLINE btBundleId #-}

-- | The state of the task.
btState :: Lens' BundleTask (Maybe BundleTaskState)
btState = lens _btState (\s a -> s { _btState = a })
{-# INLINE btState #-}

-- | The time this task started.
btStartTime :: Lens' BundleTask (Maybe ISO8601)
btStartTime = lens _btStartTime (\s a -> s { _btStartTime = a })
{-# INLINE btStartTime #-}

-- | The time of the most recent update for the task.
btUpdateTime :: Lens' BundleTask (Maybe ISO8601)
btUpdateTime = lens _btUpdateTime (\s a -> s { _btUpdateTime = a })
{-# INLINE btUpdateTime #-}

-- | The Amazon S3 storage locations.
btStorage :: Lens' BundleTask (Maybe Storage)
btStorage = lens _btStorage (\s a -> s { _btStorage = a })
{-# INLINE btStorage #-}

-- | The level of task completion, as a percent (for example, 20%).
btProgress :: Lens' BundleTask (Maybe Text)
btProgress = lens _btProgress (\s a -> s { _btProgress = a })
{-# INLINE btProgress #-}

-- | If the task fails, a description of the error.
btBundleTaskError :: Lens' BundleTask (Maybe BundleTaskError)
btBundleTaskError = lens _btBundleTaskError (\s a -> s { _btBundleTaskError = a })
{-# INLINE btBundleTaskError #-}

instance FromXML BundleTask where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "bundleInstanceTask"

-- | If the task fails, a description of the error.
data BundleTaskError = BundleTaskError
    { _bteCode :: Maybe Text
      -- ^ The error code.
    , _bteMessage :: Maybe Text
      -- ^ The error message.
    } deriving (Show, Generic)

-- | The error code.
bteCode :: Lens' BundleTaskError (Maybe Text)
bteCode = lens _bteCode (\s a -> s { _bteCode = a })
{-# INLINE bteCode #-}

-- | The error message.
bteMessage :: Lens' BundleTaskError (Maybe Text)
bteMessage = lens _bteMessage (\s a -> s { _bteMessage = a })
{-# INLINE bteMessage #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'BundleTaskError' data type to populate a request.
mkBundleTaskError :: BundleTaskError
mkBundleTaskError = BundleTaskError
    { _bteCode = Nothing
    , _bteMessage = Nothing
    }
{-# INLINE mkBundleTaskError #-}

instance FromXML BundleTaskError where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "error"

instance ToQuery BundleTaskError where
    toQuery = genericQuery def

-- | Describes a request to cancel a Spot Instance.
data CancelledSpotInstanceRequest = CancelledSpotInstanceRequest
    { _csirSpotInstanceRequestId :: Maybe Text
      -- ^ The ID of the Spot Instance request.
    , _csirState :: Maybe CancelSpotInstanceRequestState
      -- ^ The state of the Spot Instance request.
    } deriving (Show, Generic)

-- | The ID of the Spot Instance request.
csirSpotInstanceRequestId :: Lens' CancelledSpotInstanceRequest (Maybe Text)
csirSpotInstanceRequestId = lens _csirSpotInstanceRequestId (\s a -> s { _csirSpotInstanceRequestId = a })
{-# INLINE csirSpotInstanceRequestId #-}

-- | The state of the Spot Instance request.
csirState :: Lens' CancelledSpotInstanceRequest (Maybe CancelSpotInstanceRequestState)
csirState = lens _csirState (\s a -> s { _csirState = a })
{-# INLINE csirState #-}

instance FromXML CancelledSpotInstanceRequest where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes a conversion task.
data ConversionTask = ConversionTask
    { _ctConversionTaskId :: Text
      -- ^ The ID of the conversion task.
    , _ctExpirationTime :: Maybe Text
      -- ^ The time when the task expires. If the upload isn't complete
      -- before the expiration time, we automatically cancel the task.
    , _ctImportInstance :: Maybe ImportInstanceTaskDetails
      -- ^ If the task is for importing an instance, this contains
      -- information about the import instance task.
    , _ctImportVolume :: Maybe ImportVolumeTaskDetails
      -- ^ If the task is for importing a volume, this contains information
      -- about the import volume task.
    , _ctState :: ConversionTaskState
      -- ^ The state of the conversion task.
    , _ctStatusMessage :: Maybe Text
      -- ^ The status message related to the conversion task.
    , _ctTags :: [Tag]
      -- ^ 
    } deriving (Show, Generic)

-- | The ID of the conversion task.
ctConversionTaskId :: Lens' ConversionTask (Text)
ctConversionTaskId = lens _ctConversionTaskId (\s a -> s { _ctConversionTaskId = a })
{-# INLINE ctConversionTaskId #-}

-- | The time when the task expires. If the upload isn't complete before the
-- expiration time, we automatically cancel the task.
ctExpirationTime :: Lens' ConversionTask (Maybe Text)
ctExpirationTime = lens _ctExpirationTime (\s a -> s { _ctExpirationTime = a })
{-# INLINE ctExpirationTime #-}

-- | If the task is for importing an instance, this contains information about
-- the import instance task.
ctImportInstance :: Lens' ConversionTask (Maybe ImportInstanceTaskDetails)
ctImportInstance = lens _ctImportInstance (\s a -> s { _ctImportInstance = a })
{-# INLINE ctImportInstance #-}

-- | If the task is for importing a volume, this contains information about the
-- import volume task.
ctImportVolume :: Lens' ConversionTask (Maybe ImportVolumeTaskDetails)
ctImportVolume = lens _ctImportVolume (\s a -> s { _ctImportVolume = a })
{-# INLINE ctImportVolume #-}

-- | The state of the conversion task.
ctState :: Lens' ConversionTask (ConversionTaskState)
ctState = lens _ctState (\s a -> s { _ctState = a })
{-# INLINE ctState #-}

-- | The status message related to the conversion task.
ctStatusMessage :: Lens' ConversionTask (Maybe Text)
ctStatusMessage = lens _ctStatusMessage (\s a -> s { _ctStatusMessage = a })
{-# INLINE ctStatusMessage #-}

-- | 
ctTags :: Lens' ConversionTask ([Tag])
ctTags = lens _ctTags (\s a -> s { _ctTags = a })
{-# INLINE ctTags #-}

instance FromXML ConversionTask where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | 
data CreateVolumePermission = CreateVolumePermission
    { _cvpUserId :: Maybe Text
      -- ^ The specific AWS account ID that is to be added or removed from a
      -- volume's list of create volume permissions.
    , _cvpGroup :: Maybe PermissionGroup
      -- ^ The specific group that is to be added or removed from a volume's
      -- list of create volume permissions.
    } deriving (Show, Generic)

-- | The specific AWS account ID that is to be added or removed from a volume's
-- list of create volume permissions.
cvpUserId :: Lens' CreateVolumePermission (Maybe Text)
cvpUserId = lens _cvpUserId (\s a -> s { _cvpUserId = a })
{-# INLINE cvpUserId #-}

-- | The specific group that is to be added or removed from a volume's list of
-- create volume permissions.
cvpGroup :: Lens' CreateVolumePermission (Maybe PermissionGroup)
cvpGroup = lens _cvpGroup (\s a -> s { _cvpGroup = a })
{-# INLINE cvpGroup #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CreateVolumePermission' data type to populate a request.
mkCreateVolumePermission :: CreateVolumePermission
mkCreateVolumePermission = CreateVolumePermission
    { _cvpUserId = Nothing
    , _cvpGroup = Nothing
    }
{-# INLINE mkCreateVolumePermission #-}

instance FromXML CreateVolumePermission where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery CreateVolumePermission where
    toQuery = genericQuery def

-- | A JSON representation of the snapshot attribute modification.
data CreateVolumePermissionModifications = CreateVolumePermissionModifications
    { _cvpmAdd :: [CreateVolumePermission]
      -- ^ Adds a specific AWS account ID or group to a volume's list of
      -- create volume permissions.
    , _cvpmRemove :: [CreateVolumePermission]
      -- ^ Removes a specific AWS account ID or group from a volume's list
      -- of create volume permissions.
    } deriving (Show, Generic)

-- | Adds a specific AWS account ID or group to a volume's list of create volume
-- permissions.
cvpmAdd :: Lens' CreateVolumePermissionModifications ([CreateVolumePermission])
cvpmAdd = lens _cvpmAdd (\s a -> s { _cvpmAdd = a })
{-# INLINE cvpmAdd #-}

-- | Removes a specific AWS account ID or group from a volume's list of create
-- volume permissions.
cvpmRemove :: Lens' CreateVolumePermissionModifications ([CreateVolumePermission])
cvpmRemove = lens _cvpmRemove (\s a -> s { _cvpmRemove = a })
{-# INLINE cvpmRemove #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CreateVolumePermissionModifications' data type to populate a request.
mkCreateVolumePermissionModifications :: CreateVolumePermissionModifications
mkCreateVolumePermissionModifications = CreateVolumePermissionModifications
    { _cvpmAdd = mempty
    , _cvpmRemove = mempty
    }
{-# INLINE mkCreateVolumePermissionModifications #-}

instance ToQuery CreateVolumePermissionModifications where
    toQuery = genericQuery def

-- | Information about the customer gateway.
data CustomerGateway = CustomerGateway
    { _cgCustomerGatewayId :: Maybe Text
      -- ^ The ID of the customer gateway.
    , _cgState :: Maybe Text
      -- ^ The current state of the customer gateway.
    , _cgType :: Maybe Text
      -- ^ The type of VPN connection the customer gateway supports.
    , _cgIpAddress :: Maybe Text
      -- ^ The Internet-routable IP address of the customer gateway's
      -- outside interface.
    , _cgBgpAsn :: Maybe Text
      -- ^ The customer gateway's Border Gateway Protocol (BGP) Autonomous
      -- System Number (ASN).
    , _cgTags :: [Tag]
      -- ^ Any tags assigned to the customer gateway.
    } deriving (Show, Generic)

-- | The ID of the customer gateway.
cgCustomerGatewayId :: Lens' CustomerGateway (Maybe Text)
cgCustomerGatewayId = lens _cgCustomerGatewayId (\s a -> s { _cgCustomerGatewayId = a })
{-# INLINE cgCustomerGatewayId #-}

-- | The current state of the customer gateway.
cgState :: Lens' CustomerGateway (Maybe Text)
cgState = lens _cgState (\s a -> s { _cgState = a })
{-# INLINE cgState #-}

-- | The type of VPN connection the customer gateway supports.
cgType :: Lens' CustomerGateway (Maybe Text)
cgType = lens _cgType (\s a -> s { _cgType = a })
{-# INLINE cgType #-}

-- | The Internet-routable IP address of the customer gateway's outside
-- interface.
cgIpAddress :: Lens' CustomerGateway (Maybe Text)
cgIpAddress = lens _cgIpAddress (\s a -> s { _cgIpAddress = a })
{-# INLINE cgIpAddress #-}

-- | The customer gateway's Border Gateway Protocol (BGP) Autonomous System
-- Number (ASN).
cgBgpAsn :: Lens' CustomerGateway (Maybe Text)
cgBgpAsn = lens _cgBgpAsn (\s a -> s { _cgBgpAsn = a })
{-# INLINE cgBgpAsn #-}

-- | Any tags assigned to the customer gateway.
cgTags :: Lens' CustomerGateway ([Tag])
cgTags = lens _cgTags (\s a -> s { _cgTags = a })
{-# INLINE cgTags #-}

instance FromXML CustomerGateway where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "customerGateway"

-- | Describes a DHCP configuration option.
data DhcpConfiguration = DhcpConfiguration
    { _dcKey :: Maybe Text
      -- ^ The name of a DHCP option.
    , _dcValues :: [Text]
      -- ^ One or more values for the DHCP option.
    } deriving (Show, Generic)

-- | The name of a DHCP option.
dcKey :: Lens' DhcpConfiguration (Maybe Text)
dcKey = lens _dcKey (\s a -> s { _dcKey = a })
{-# INLINE dcKey #-}

-- | One or more values for the DHCP option.
dcValues :: Lens' DhcpConfiguration ([Text])
dcValues = lens _dcValues (\s a -> s { _dcValues = a })
{-# INLINE dcValues #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DhcpConfiguration' data type to populate a request.
mkDhcpConfiguration :: DhcpConfiguration
mkDhcpConfiguration = DhcpConfiguration
    { _dcKey = Nothing
    , _dcValues = mempty
    }
{-# INLINE mkDhcpConfiguration #-}

instance FromXML DhcpConfiguration where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DhcpConfiguration"

instance ToQuery DhcpConfiguration where
    toQuery = genericQuery def

-- | A set of DHCP options.
data DhcpOptions = DhcpOptions
    { _doDhcpOptionsId :: Maybe Text
      -- ^ The ID of the set of DHCP options.
    , _doDhcpConfigurations :: [DhcpConfiguration]
      -- ^ One or more DHCP options in the set.
    , _doTags :: [Tag]
      -- ^ Any tags assigned to the DHCP options set.
    } deriving (Show, Generic)

-- | The ID of the set of DHCP options.
doDhcpOptionsId :: Lens' DhcpOptions (Maybe Text)
doDhcpOptionsId = lens _doDhcpOptionsId (\s a -> s { _doDhcpOptionsId = a })
{-# INLINE doDhcpOptionsId #-}

-- | One or more DHCP options in the set.
doDhcpConfigurations :: Lens' DhcpOptions ([DhcpConfiguration])
doDhcpConfigurations = lens _doDhcpConfigurations (\s a -> s { _doDhcpConfigurations = a })
{-# INLINE doDhcpConfigurations #-}

-- | Any tags assigned to the DHCP options set.
doTags :: Lens' DhcpOptions ([Tag])
doTags = lens _doTags (\s a -> s { _doTags = a })
{-# INLINE doTags #-}

instance FromXML DhcpOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "dhcpOptions"

-- | Describes a disk image.
data DiskImage = DiskImage
    { _dmImage :: Maybe DiskImageDetail
      -- ^ 
    , _dmDescription :: Maybe Text
      -- ^ 
    , _dmVolume :: Maybe VolumeDetail
      -- ^ 
    } deriving (Show, Generic)

-- | 
dmImage :: Lens' DiskImage (Maybe DiskImageDetail)
dmImage = lens _dmImage (\s a -> s { _dmImage = a })
{-# INLINE dmImage #-}

-- | 
dmDescription :: Lens' DiskImage (Maybe Text)
dmDescription = lens _dmDescription (\s a -> s { _dmDescription = a })
{-# INLINE dmDescription #-}

-- | 
dmVolume :: Lens' DiskImage (Maybe VolumeDetail)
dmVolume = lens _dmVolume (\s a -> s { _dmVolume = a })
{-# INLINE dmVolume #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DiskImage' data type to populate a request.
mkDiskImage :: DiskImage
mkDiskImage = DiskImage
    { _dmImage = Nothing
    , _dmDescription = Nothing
    , _dmVolume = Nothing
    }
{-# INLINE mkDiskImage #-}

instance ToQuery DiskImage where
    toQuery = genericQuery def

-- | The image.
data DiskImageDescription = DiskImageDescription
    { _didFormat :: DiskImageFormat
      -- ^ The disk image format.
    , _didSize :: Integer
      -- ^ The size of the disk image.
    , _didImportManifestUrl :: Text
      -- ^ A presigned URL for the import manifest stored in Amazon S3. For
      -- information about creating a presigned URL for an Amazon S3
      -- object, read the "Query String Request Authentication
      -- Alternative" section of the Authenticating REST Requests topic in
      -- the Amazon Simple Storage Service Developer Guide.
    , _didChecksum :: Maybe Text
      -- ^ The checksum computed for the disk image.
    } deriving (Show, Generic)

-- | The disk image format.
didFormat :: Lens' DiskImageDescription (DiskImageFormat)
didFormat = lens _didFormat (\s a -> s { _didFormat = a })
{-# INLINE didFormat #-}

-- | The size of the disk image.
didSize :: Lens' DiskImageDescription (Integer)
didSize = lens _didSize (\s a -> s { _didSize = a })
{-# INLINE didSize #-}

-- | A presigned URL for the import manifest stored in Amazon S3. For
-- information about creating a presigned URL for an Amazon S3 object, read
-- the "Query String Request Authentication Alternative" section of the
-- Authenticating REST Requests topic in the Amazon Simple Storage Service
-- Developer Guide.
didImportManifestUrl :: Lens' DiskImageDescription (Text)
didImportManifestUrl = lens _didImportManifestUrl (\s a -> s { _didImportManifestUrl = a })
{-# INLINE didImportManifestUrl #-}

-- | The checksum computed for the disk image.
didChecksum :: Lens' DiskImageDescription (Maybe Text)
didChecksum = lens _didChecksum (\s a -> s { _didChecksum = a })
{-# INLINE didChecksum #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DiskImageDescription' data type to populate a request.
mkDiskImageDescription :: DiskImageFormat -- ^ 'didFormat'
                       -> Integer -- ^ 'didSize'
                       -> Text -- ^ 'didImportManifestUrl'
                       -> DiskImageDescription
mkDiskImageDescription p1 p2 p3 = DiskImageDescription
    { _didFormat = p1
    , _didSize = p2
    , _didImportManifestUrl = p3
    , _didChecksum = Nothing
    }
{-# INLINE mkDiskImageDescription #-}

instance FromXML DiskImageDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "image"

instance ToQuery DiskImageDescription where
    toQuery = genericQuery def

-- | 
data DiskImageDetail = DiskImageDetail
    { _dikFormat :: DiskImageFormat
      -- ^ The disk image format.
    , _dikBytes :: Integer
      -- ^ 
    , _dikImportManifestUrl :: Text
      -- ^ A presigned URL for the import manifest stored in Amazon S3. For
      -- information about creating a presigned URL for an Amazon S3
      -- object, read the "Query String Request Authentication
      -- Alternative" section of the Authenticating REST Requests topic in
      -- the Amazon Simple Storage Service Developer Guide.
    } deriving (Show, Generic)

-- | The disk image format.
dikFormat :: Lens' DiskImageDetail (DiskImageFormat)
dikFormat = lens _dikFormat (\s a -> s { _dikFormat = a })
{-# INLINE dikFormat #-}

-- | 
dikBytes :: Lens' DiskImageDetail (Integer)
dikBytes = lens _dikBytes (\s a -> s { _dikBytes = a })
{-# INLINE dikBytes #-}

-- | A presigned URL for the import manifest stored in Amazon S3. For
-- information about creating a presigned URL for an Amazon S3 object, read
-- the "Query String Request Authentication Alternative" section of the
-- Authenticating REST Requests topic in the Amazon Simple Storage Service
-- Developer Guide.
dikImportManifestUrl :: Lens' DiskImageDetail (Text)
dikImportManifestUrl = lens _dikImportManifestUrl (\s a -> s { _dikImportManifestUrl = a })
{-# INLINE dikImportManifestUrl #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DiskImageDetail' data type to populate a request.
mkDiskImageDetail :: DiskImageFormat -- ^ 'dikFormat'
                  -> Integer -- ^ 'dikBytes'
                  -> Text -- ^ 'dikImportManifestUrl'
                  -> DiskImageDetail
mkDiskImageDetail p1 p2 p3 = DiskImageDetail
    { _dikFormat = p1
    , _dikBytes = p2
    , _dikImportManifestUrl = p3
    }
{-# INLINE mkDiskImageDetail #-}

instance FromXML DiskImageDetail where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DiskImageDetail"

instance ToQuery DiskImageDetail where
    toQuery = genericQuery def

-- | The volume.
data DiskImageVolumeDescription = DiskImageVolumeDescription
    { _divdSize :: Maybe Integer
      -- ^ The size of the volume.
    , _divdId :: Text
      -- ^ The volume identifier.
    } deriving (Show, Generic)

-- | The size of the volume.
divdSize :: Lens' DiskImageVolumeDescription (Maybe Integer)
divdSize = lens _divdSize (\s a -> s { _divdSize = a })
{-# INLINE divdSize #-}

-- | The volume identifier.
divdId :: Lens' DiskImageVolumeDescription (Text)
divdId = lens _divdId (\s a -> s { _divdId = a })
{-# INLINE divdId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DiskImageVolumeDescription' data type to populate a request.
mkDiskImageVolumeDescription :: Text -- ^ 'divdId'
                             -> DiskImageVolumeDescription
mkDiskImageVolumeDescription p1 = DiskImageVolumeDescription
    { _divdSize = Nothing
    , _divdId = p2
    }
{-# INLINE mkDiskImageVolumeDescription #-}

instance FromXML DiskImageVolumeDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "volume"

instance ToQuery DiskImageVolumeDescription where
    toQuery = genericQuery def

-- | Parameters used to automatically set up Amazon EBS volumes when the
-- instance is launched.
data EbsBlockDevice = EbsBlockDevice
    { _ebdSnapshotId :: Maybe Text
      -- ^ The ID of the snapshot.
    , _ebdVolumeSize :: Maybe Integer
      -- ^ The size of the volume, in GiB. Constraints: If the volume type
      -- is io1, the minimum size of the volume is 10 GiB; otherwise, the
      -- minimum size is 1 GiB. The maximum volume size is 1024 GiB.
      -- Default: If you're creating the volume from a snapshot and don't
      -- specify a volume size, the default is the snapshot size.
    , _ebdDeleteOnTermination :: Maybe Bool
      -- ^ Indicates whether the Amazon EBS volume is deleted on instance
      -- termination.
    , _ebdVolumeType :: Maybe VolumeType
      -- ^ The volume type. gp2 for General Purpose (SSD) volumes, io1 for
      -- Provisioned IOPS (SSD) volumes, and standard for Magnetic
      -- volumes. Default: standard.
    , _ebdIops :: Maybe Integer
      -- ^ The number of I/O operations per second (IOPS) that the volume
      -- supports. For Provisioned IOPS (SSD) volumes, this represents the
      -- number of IOPS that are provisioned for the volume. For General
      -- Purpose (SSD) volumes, this represents the baseline performance
      -- of the volume and the rate at which the volume accumulates I/O
      -- credits for bursting. For more information on General Purpose
      -- (SSD) baseline performance, I/O credits, and bursting, see Amazon
      -- EBS Volume Types in the Amazon Elastic Compute Cloud User Guide.
      -- Constraint: Range is 100 to 4000 for Provisioned IOPS (SSD)
      -- volumes and 3 to 3072 for General Purpose (SSD) volumes.
      -- Condition: This parameter is required for requests to create io1
      -- volumes; it is not used in requests to create standard or gp2
      -- volumes.
    , _ebdEncrypted :: Maybe Bool
      -- ^ Indicates whether the Amazon EBS volume is encrypted.
    } deriving (Show, Generic)

-- | The ID of the snapshot.
ebdSnapshotId :: Lens' EbsBlockDevice (Maybe Text)
ebdSnapshotId = lens _ebdSnapshotId (\s a -> s { _ebdSnapshotId = a })
{-# INLINE ebdSnapshotId #-}

-- | The size of the volume, in GiB. Constraints: If the volume type is io1, the
-- minimum size of the volume is 10 GiB; otherwise, the minimum size is 1 GiB.
-- The maximum volume size is 1024 GiB. Default: If you're creating the volume
-- from a snapshot and don't specify a volume size, the default is the
-- snapshot size.
ebdVolumeSize :: Lens' EbsBlockDevice (Maybe Integer)
ebdVolumeSize = lens _ebdVolumeSize (\s a -> s { _ebdVolumeSize = a })
{-# INLINE ebdVolumeSize #-}

-- | Indicates whether the Amazon EBS volume is deleted on instance termination.
ebdDeleteOnTermination :: Lens' EbsBlockDevice (Maybe Bool)
ebdDeleteOnTermination = lens _ebdDeleteOnTermination (\s a -> s { _ebdDeleteOnTermination = a })
{-# INLINE ebdDeleteOnTermination #-}

-- | The volume type. gp2 for General Purpose (SSD) volumes, io1 for Provisioned
-- IOPS (SSD) volumes, and standard for Magnetic volumes. Default: standard.
ebdVolumeType :: Lens' EbsBlockDevice (Maybe VolumeType)
ebdVolumeType = lens _ebdVolumeType (\s a -> s { _ebdVolumeType = a })
{-# INLINE ebdVolumeType #-}

-- | The number of I/O operations per second (IOPS) that the volume supports.
-- For Provisioned IOPS (SSD) volumes, this represents the number of IOPS that
-- are provisioned for the volume. For General Purpose (SSD) volumes, this
-- represents the baseline performance of the volume and the rate at which the
-- volume accumulates I/O credits for bursting. For more information on
-- General Purpose (SSD) baseline performance, I/O credits, and bursting, see
-- Amazon EBS Volume Types in the Amazon Elastic Compute Cloud User Guide.
-- Constraint: Range is 100 to 4000 for Provisioned IOPS (SSD) volumes and 3
-- to 3072 for General Purpose (SSD) volumes. Condition: This parameter is
-- required for requests to create io1 volumes; it is not used in requests to
-- create standard or gp2 volumes.
ebdIops :: Lens' EbsBlockDevice (Maybe Integer)
ebdIops = lens _ebdIops (\s a -> s { _ebdIops = a })
{-# INLINE ebdIops #-}

-- | Indicates whether the Amazon EBS volume is encrypted.
ebdEncrypted :: Lens' EbsBlockDevice (Maybe Bool)
ebdEncrypted = lens _ebdEncrypted (\s a -> s { _ebdEncrypted = a })
{-# INLINE ebdEncrypted #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'EbsBlockDevice' data type to populate a request.
mkEbsBlockDevice :: EbsBlockDevice
mkEbsBlockDevice = EbsBlockDevice
    { _ebdSnapshotId = Nothing
    , _ebdVolumeSize = Nothing
    , _ebdDeleteOnTermination = Nothing
    , _ebdVolumeType = Nothing
    , _ebdIops = Nothing
    , _ebdEncrypted = Nothing
    }
{-# INLINE mkEbsBlockDevice #-}

instance FromXML EbsBlockDevice where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EbsBlockDevice"

instance ToQuery EbsBlockDevice where
    toQuery = genericQuery def

-- | Parameters used to automatically set up Amazon EBS volumes when the
-- instance is launched.
data EbsInstanceBlockDevice = EbsInstanceBlockDevice
    { _eibdVolumeId :: Maybe Text
      -- ^ The ID of the Amazon EBS volume.
    , _eibdStatus :: Maybe AttachmentStatus
      -- ^ The attachment state.
    , _eibdAttachTime :: Maybe ISO8601
      -- ^ The time stamp when the attachment initiated.
    , _eibdDeleteOnTermination :: Maybe Bool
      -- ^ Indicates whether the volume is deleted on instance termination.
    } deriving (Show, Generic)

-- | The ID of the Amazon EBS volume.
eibdVolumeId :: Lens' EbsInstanceBlockDevice (Maybe Text)
eibdVolumeId = lens _eibdVolumeId (\s a -> s { _eibdVolumeId = a })
{-# INLINE eibdVolumeId #-}

-- | The attachment state.
eibdStatus :: Lens' EbsInstanceBlockDevice (Maybe AttachmentStatus)
eibdStatus = lens _eibdStatus (\s a -> s { _eibdStatus = a })
{-# INLINE eibdStatus #-}

-- | The time stamp when the attachment initiated.
eibdAttachTime :: Lens' EbsInstanceBlockDevice (Maybe ISO8601)
eibdAttachTime = lens _eibdAttachTime (\s a -> s { _eibdAttachTime = a })
{-# INLINE eibdAttachTime #-}

-- | Indicates whether the volume is deleted on instance termination.
eibdDeleteOnTermination :: Lens' EbsInstanceBlockDevice (Maybe Bool)
eibdDeleteOnTermination = lens _eibdDeleteOnTermination (\s a -> s { _eibdDeleteOnTermination = a })
{-# INLINE eibdDeleteOnTermination #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'EbsInstanceBlockDevice' data type to populate a request.
mkEbsInstanceBlockDevice :: EbsInstanceBlockDevice
mkEbsInstanceBlockDevice = EbsInstanceBlockDevice
    { _eibdVolumeId = Nothing
    , _eibdStatus = Nothing
    , _eibdAttachTime = Nothing
    , _eibdDeleteOnTermination = Nothing
    }
{-# INLINE mkEbsInstanceBlockDevice #-}

instance FromXML EbsInstanceBlockDevice where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ebs"

instance ToQuery EbsInstanceBlockDevice where
    toQuery = genericQuery def

-- | Parameters used to automatically set up Amazon EBS volumes when the
-- instance is launched.
data EbsInstanceBlockDeviceSpecification = EbsInstanceBlockDeviceSpecification
    { _eibdsVolumeId :: Maybe Text
      -- ^ The ID of the Amazon EBS volume.
    , _eibdsDeleteOnTermination :: Maybe Bool
      -- ^ Indicates whether the volume is deleted on instance termination.
    } deriving (Show, Generic)

-- | The ID of the Amazon EBS volume.
eibdsVolumeId :: Lens' EbsInstanceBlockDeviceSpecification (Maybe Text)
eibdsVolumeId = lens _eibdsVolumeId (\s a -> s { _eibdsVolumeId = a })
{-# INLINE eibdsVolumeId #-}

-- | Indicates whether the volume is deleted on instance termination.
eibdsDeleteOnTermination :: Lens' EbsInstanceBlockDeviceSpecification (Maybe Bool)
eibdsDeleteOnTermination = lens _eibdsDeleteOnTermination (\s a -> s { _eibdsDeleteOnTermination = a })
{-# INLINE eibdsDeleteOnTermination #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'EbsInstanceBlockDeviceSpecification' data type to populate a request.
mkEbsInstanceBlockDeviceSpecification :: EbsInstanceBlockDeviceSpecification
mkEbsInstanceBlockDeviceSpecification = EbsInstanceBlockDeviceSpecification
    { _eibdsVolumeId = Nothing
    , _eibdsDeleteOnTermination = Nothing
    }
{-# INLINE mkEbsInstanceBlockDeviceSpecification #-}

instance FromXML EbsInstanceBlockDeviceSpecification where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EbsInstanceBlockDeviceSpecification"

instance ToQuery EbsInstanceBlockDeviceSpecification where
    toQuery = genericQuery def

-- | 
data ExportTask = ExportTask
    { _etExportTaskId :: Maybe Text
      -- ^ The ID of the export task.
    , _etDescription :: Maybe Text
      -- ^ A description of the resource being exported.
    , _etState :: Maybe ExportTaskState
      -- ^ The state of the conversion task.
    , _etStatusMessage :: Maybe Text
      -- ^ The status message related to the export task.
    , _etInstanceExportDetails :: Maybe InstanceExportDetails
      -- ^ The instance being exported.
    , _etExportToS3Task :: Maybe ExportToS3Task
      -- ^ 
    } deriving (Show, Generic)

-- | The ID of the export task.
etExportTaskId :: Lens' ExportTask (Maybe Text)
etExportTaskId = lens _etExportTaskId (\s a -> s { _etExportTaskId = a })
{-# INLINE etExportTaskId #-}

-- | A description of the resource being exported.
etDescription :: Lens' ExportTask (Maybe Text)
etDescription = lens _etDescription (\s a -> s { _etDescription = a })
{-# INLINE etDescription #-}

-- | The state of the conversion task.
etState :: Lens' ExportTask (Maybe ExportTaskState)
etState = lens _etState (\s a -> s { _etState = a })
{-# INLINE etState #-}

-- | The status message related to the export task.
etStatusMessage :: Lens' ExportTask (Maybe Text)
etStatusMessage = lens _etStatusMessage (\s a -> s { _etStatusMessage = a })
{-# INLINE etStatusMessage #-}

-- | The instance being exported.
etInstanceExportDetails :: Lens' ExportTask (Maybe InstanceExportDetails)
etInstanceExportDetails = lens _etInstanceExportDetails (\s a -> s { _etInstanceExportDetails = a })
{-# INLINE etInstanceExportDetails #-}

-- | 
etExportToS3Task :: Lens' ExportTask (Maybe ExportToS3Task)
etExportToS3Task = lens _etExportToS3Task (\s a -> s { _etExportToS3Task = a })
{-# INLINE etExportToS3Task #-}

instance FromXML ExportTask where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "exportTask"

-- | 
data ExportToS3Task = ExportToS3Task
    { _etstDiskImageFormat :: Maybe DiskImageFormat
      -- ^ The format for the exported image.
    , _etstContainerFormat :: Maybe ContainerFormat
      -- ^ The container format used to combine disk images with metadata
      -- (such as OVF). If absent, only the disk image is exported.
    , _etstS3Bucket :: Maybe Text
      -- ^ The Amazon S3 bucket for the destination image. The destination
      -- bucket must exist and grant WRITE and READ_ACL permissions to the
      -- AWS account vm-import-export@amazon.com.
    , _etstS3Key :: Maybe Text
      -- ^ 
    } deriving (Show, Generic)

-- | The format for the exported image.
etstDiskImageFormat :: Lens' ExportToS3Task (Maybe DiskImageFormat)
etstDiskImageFormat = lens _etstDiskImageFormat (\s a -> s { _etstDiskImageFormat = a })
{-# INLINE etstDiskImageFormat #-}

-- | The container format used to combine disk images with metadata (such as
-- OVF). If absent, only the disk image is exported.
etstContainerFormat :: Lens' ExportToS3Task (Maybe ContainerFormat)
etstContainerFormat = lens _etstContainerFormat (\s a -> s { _etstContainerFormat = a })
{-# INLINE etstContainerFormat #-}

-- | The Amazon S3 bucket for the destination image. The destination bucket must
-- exist and grant WRITE and READ_ACL permissions to the AWS account
-- vm-import-export@amazon.com.
etstS3Bucket :: Lens' ExportToS3Task (Maybe Text)
etstS3Bucket = lens _etstS3Bucket (\s a -> s { _etstS3Bucket = a })
{-# INLINE etstS3Bucket #-}

-- | 
etstS3Key :: Lens' ExportToS3Task (Maybe Text)
etstS3Key = lens _etstS3Key (\s a -> s { _etstS3Key = a })
{-# INLINE etstS3Key #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ExportToS3Task' data type to populate a request.
mkExportToS3Task :: ExportToS3Task
mkExportToS3Task = ExportToS3Task
    { _etstDiskImageFormat = Nothing
    , _etstContainerFormat = Nothing
    , _etstS3Bucket = Nothing
    , _etstS3Key = Nothing
    }
{-# INLINE mkExportToS3Task #-}

instance FromXML ExportToS3Task where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "exportToS3"

instance ToQuery ExportToS3Task where
    toQuery = genericQuery def

-- | 
data ExportToS3TaskSpecification = ExportToS3TaskSpecification
    { _etstsDiskImageFormat :: Maybe DiskImageFormat
      -- ^ 
    , _etstsContainerFormat :: Maybe ContainerFormat
      -- ^ 
    , _etstsS3Bucket :: Maybe Text
      -- ^ 
    , _etstsS3Prefix :: Maybe Text
      -- ^ The image is written to a single object in the Amazon S3 bucket
      -- at the S3 key s3prefix + exportTaskId + '.' + diskImageFormat.
    } deriving (Show, Generic)

-- | 
etstsDiskImageFormat :: Lens' ExportToS3TaskSpecification (Maybe DiskImageFormat)
etstsDiskImageFormat = lens _etstsDiskImageFormat (\s a -> s { _etstsDiskImageFormat = a })
{-# INLINE etstsDiskImageFormat #-}

-- | 
etstsContainerFormat :: Lens' ExportToS3TaskSpecification (Maybe ContainerFormat)
etstsContainerFormat = lens _etstsContainerFormat (\s a -> s { _etstsContainerFormat = a })
{-# INLINE etstsContainerFormat #-}

-- | 
etstsS3Bucket :: Lens' ExportToS3TaskSpecification (Maybe Text)
etstsS3Bucket = lens _etstsS3Bucket (\s a -> s { _etstsS3Bucket = a })
{-# INLINE etstsS3Bucket #-}

-- | The image is written to a single object in the Amazon S3 bucket at the S3
-- key s3prefix + exportTaskId + '.' + diskImageFormat.
etstsS3Prefix :: Lens' ExportToS3TaskSpecification (Maybe Text)
etstsS3Prefix = lens _etstsS3Prefix (\s a -> s { _etstsS3Prefix = a })
{-# INLINE etstsS3Prefix #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ExportToS3TaskSpecification' data type to populate a request.
mkExportToS3TaskSpecification :: ExportToS3TaskSpecification
mkExportToS3TaskSpecification = ExportToS3TaskSpecification
    { _etstsDiskImageFormat = Nothing
    , _etstsContainerFormat = Nothing
    , _etstsS3Bucket = Nothing
    , _etstsS3Prefix = Nothing
    }
{-# INLINE mkExportToS3TaskSpecification #-}

instance ToQuery ExportToS3TaskSpecification where
    toQuery = genericQuery def

-- | 
data Filter = Filter
    { _frName :: Text
      -- ^ The name of the filter.
    , _frValues :: [Text]
      -- ^ One or more filter values.
    } deriving (Show, Generic)

-- | The name of the filter.
frName :: Lens' Filter (Text)
frName = lens _frName (\s a -> s { _frName = a })
{-# INLINE frName #-}

-- | One or more filter values.
frValues :: Lens' Filter ([Text])
frValues = lens _frValues (\s a -> s { _frValues = a })
{-# INLINE frValues #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Filter' data type to populate a request.
mkFilter :: Text -- ^ 'frName'
         -> Filter
mkFilter p1 = Filter
    { _frName = p1
    , _frValues = mempty
    }
{-# INLINE mkFilter #-}

instance ToQuery Filter where
    toQuery = genericQuery def

-- | Describes a security group.
data GroupIdentifier = GroupIdentifier
    { _giGroupName :: Maybe Text
      -- ^ The name of the security group.
    , _giGroupId :: Maybe Text
      -- ^ The ID of the security group.
    } deriving (Show, Generic)

-- | The name of the security group.
giGroupName :: Lens' GroupIdentifier (Maybe Text)
giGroupName = lens _giGroupName (\s a -> s { _giGroupName = a })
{-# INLINE giGroupName #-}

-- | The ID of the security group.
giGroupId :: Lens' GroupIdentifier (Maybe Text)
giGroupId = lens _giGroupId (\s a -> s { _giGroupId = a })
{-# INLINE giGroupId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'GroupIdentifier' data type to populate a request.
mkGroupIdentifier :: GroupIdentifier
mkGroupIdentifier = GroupIdentifier
    { _giGroupName = Nothing
    , _giGroupId = Nothing
    }
{-# INLINE mkGroupIdentifier #-}

instance FromXML GroupIdentifier where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery GroupIdentifier where
    toQuery = genericQuery def

-- | The IAM instance profile associated with the instance.
data IamInstanceProfile = IamInstanceProfile
    { _iipArn :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the instance profile.
    , _iipId :: Maybe Text
      -- ^ The ID of the instance profile.
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the instance profile.
iipArn :: Lens' IamInstanceProfile (Maybe Text)
iipArn = lens _iipArn (\s a -> s { _iipArn = a })
{-# INLINE iipArn #-}

-- | The ID of the instance profile.
iipId :: Lens' IamInstanceProfile (Maybe Text)
iipId = lens _iipId (\s a -> s { _iipId = a })
{-# INLINE iipId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'IamInstanceProfile' data type to populate a request.
mkIamInstanceProfile :: IamInstanceProfile
mkIamInstanceProfile = IamInstanceProfile
    { _iipArn = Nothing
    , _iipId = Nothing
    }
{-# INLINE mkIamInstanceProfile #-}

instance FromXML IamInstanceProfile where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "iamInstanceProfile"

instance ToQuery IamInstanceProfile where
    toQuery = genericQuery def

-- | The IAM instance profile.
data IamInstanceProfileSpecification = IamInstanceProfileSpecification
    { _iipsArn :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the instance profile.
    , _iipsName :: Maybe Text
      -- ^ The name of the instance profile.
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the instance profile.
iipsArn :: Lens' IamInstanceProfileSpecification (Maybe Text)
iipsArn = lens _iipsArn (\s a -> s { _iipsArn = a })
{-# INLINE iipsArn #-}

-- | The name of the instance profile.
iipsName :: Lens' IamInstanceProfileSpecification (Maybe Text)
iipsName = lens _iipsName (\s a -> s { _iipsName = a })
{-# INLINE iipsName #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'IamInstanceProfileSpecification' data type to populate a request.
mkIamInstanceProfileSpecification :: IamInstanceProfileSpecification
mkIamInstanceProfileSpecification = IamInstanceProfileSpecification
    { _iipsArn = Nothing
    , _iipsName = Nothing
    }
{-# INLINE mkIamInstanceProfileSpecification #-}

instance FromXML IamInstanceProfileSpecification where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "iamInstanceProfile"

instance ToQuery IamInstanceProfileSpecification where
    toQuery = genericQuery def

-- | ICMP protocol: The ICMP type and code.
data IcmpTypeCode = IcmpTypeCode
    { _itcType :: Maybe Integer
      -- ^ The ICMP code. A value of -1 means all codes for the specified
      -- ICMP type.
    , _itcCode :: Maybe Integer
      -- ^ The ICMP type. A value of -1 means all types.
    } deriving (Show, Generic)

-- | The ICMP code. A value of -1 means all codes for the specified ICMP type.
itcType :: Lens' IcmpTypeCode (Maybe Integer)
itcType = lens _itcType (\s a -> s { _itcType = a })
{-# INLINE itcType #-}

-- | The ICMP type. A value of -1 means all types.
itcCode :: Lens' IcmpTypeCode (Maybe Integer)
itcCode = lens _itcCode (\s a -> s { _itcCode = a })
{-# INLINE itcCode #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'IcmpTypeCode' data type to populate a request.
mkIcmpTypeCode :: IcmpTypeCode
mkIcmpTypeCode = IcmpTypeCode
    { _itcType = Nothing
    , _itcCode = Nothing
    }
{-# INLINE mkIcmpTypeCode #-}

instance FromXML IcmpTypeCode where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "icmpTypeCode"

instance ToQuery IcmpTypeCode where
    toQuery = genericQuery def

-- | Describes an image.
data Image = Image
    { _ieImageId :: Text
      -- ^ The ID of the AMI.
    , _ieImageLocation :: Text
      -- ^ The location of the AMI.
    , _ieState :: ImageState
      -- ^ The current state of the AMI. If the state is available, the
      -- image is successfully registered and can be used to launch an
      -- instance.
    , _ieOwnerId :: Text
      -- ^ The AWS account ID of the image owner.
    , _iePublic :: Bool
      -- ^ Indicates whether the image has public launch permissions. The
      -- value is true if this image has public launch permissions or
      -- false if it has only implicit and explicit launch permissions.
    , _ieProductCodes :: [ProductCode]
      -- ^ Any product codes associated with the AMI.
    , _ieArchitecture :: ArchitectureValues
      -- ^ The architecture of the image.
    , _ieImageType :: ImageTypeValues
      -- ^ The type of image.
    , _ieKernelId :: Maybe Text
      -- ^ The kernel associated with the image, if any. Only applicable for
      -- machine images.
    , _ieRamdiskId :: Maybe Text
      -- ^ The RAM disk associated with the image, if any. Only applicable
      -- for machine images.
    , _iePlatform :: Maybe PlatformValues
      -- ^ The value is Windows for Windows AMIs; otherwise blank.
    , _ieSriovNetSupport :: Maybe Text
      -- ^ Specifies whether enhanced networking is enabled.
    , _ieStateReason :: Maybe StateReason
      -- ^ The reason for the state change.
    , _ieImageOwnerAlias :: Maybe Text
      -- ^ The AWS account alias (for example, amazon, self) or the AWS
      -- account ID of the AMI owner.
    , _ieName :: Text
      -- ^ The name of the AMI that was provided during image creation.
    , _ieDescription :: Maybe Text
      -- ^ The description of the AMI that was provided during image
      -- creation.
    , _ieRootDeviceType :: DeviceType
      -- ^ The type of root device used by the AMI. The AMI can use an
      -- Amazon EBS volume or an instance store volume.
    , _ieRootDeviceName :: Maybe Text
      -- ^ The device name of the root device (for example, /dev/sda1 or
      -- xvda).
    , _ieBlockDeviceMappings :: [BlockDeviceMapping]
      -- ^ Any block device mapping entries.
    , _ieVirtualizationType :: VirtualizationType
      -- ^ The type of virtualization of the AMI.
    , _ieTags :: [Tag]
      -- ^ Any tags assigned to the image.
    , _ieHypervisor :: HypervisorType
      -- ^ The hypervisor type of the image.
    } deriving (Show, Generic)

-- | The ID of the AMI.
ieImageId :: Lens' Image (Text)
ieImageId = lens _ieImageId (\s a -> s { _ieImageId = a })
{-# INLINE ieImageId #-}

-- | The location of the AMI.
ieImageLocation :: Lens' Image (Text)
ieImageLocation = lens _ieImageLocation (\s a -> s { _ieImageLocation = a })
{-# INLINE ieImageLocation #-}

-- | The current state of the AMI. If the state is available, the image is
-- successfully registered and can be used to launch an instance.
ieState :: Lens' Image (ImageState)
ieState = lens _ieState (\s a -> s { _ieState = a })
{-# INLINE ieState #-}

-- | The AWS account ID of the image owner.
ieOwnerId :: Lens' Image (Text)
ieOwnerId = lens _ieOwnerId (\s a -> s { _ieOwnerId = a })
{-# INLINE ieOwnerId #-}

-- | Indicates whether the image has public launch permissions. The value is
-- true if this image has public launch permissions or false if it has only
-- implicit and explicit launch permissions.
iePublic :: Lens' Image (Bool)
iePublic = lens _iePublic (\s a -> s { _iePublic = a })
{-# INLINE iePublic #-}

-- | Any product codes associated with the AMI.
ieProductCodes :: Lens' Image ([ProductCode])
ieProductCodes = lens _ieProductCodes (\s a -> s { _ieProductCodes = a })
{-# INLINE ieProductCodes #-}

-- | The architecture of the image.
ieArchitecture :: Lens' Image (ArchitectureValues)
ieArchitecture = lens _ieArchitecture (\s a -> s { _ieArchitecture = a })
{-# INLINE ieArchitecture #-}

-- | The type of image.
ieImageType :: Lens' Image (ImageTypeValues)
ieImageType = lens _ieImageType (\s a -> s { _ieImageType = a })
{-# INLINE ieImageType #-}

-- | The kernel associated with the image, if any. Only applicable for machine
-- images.
ieKernelId :: Lens' Image (Maybe Text)
ieKernelId = lens _ieKernelId (\s a -> s { _ieKernelId = a })
{-# INLINE ieKernelId #-}

-- | The RAM disk associated with the image, if any. Only applicable for machine
-- images.
ieRamdiskId :: Lens' Image (Maybe Text)
ieRamdiskId = lens _ieRamdiskId (\s a -> s { _ieRamdiskId = a })
{-# INLINE ieRamdiskId #-}

-- | The value is Windows for Windows AMIs; otherwise blank.
iePlatform :: Lens' Image (Maybe PlatformValues)
iePlatform = lens _iePlatform (\s a -> s { _iePlatform = a })
{-# INLINE iePlatform #-}

-- | Specifies whether enhanced networking is enabled.
ieSriovNetSupport :: Lens' Image (Maybe Text)
ieSriovNetSupport = lens _ieSriovNetSupport (\s a -> s { _ieSriovNetSupport = a })
{-# INLINE ieSriovNetSupport #-}

-- | The reason for the state change.
ieStateReason :: Lens' Image (Maybe StateReason)
ieStateReason = lens _ieStateReason (\s a -> s { _ieStateReason = a })
{-# INLINE ieStateReason #-}

-- | The AWS account alias (for example, amazon, self) or the AWS account ID of
-- the AMI owner.
ieImageOwnerAlias :: Lens' Image (Maybe Text)
ieImageOwnerAlias = lens _ieImageOwnerAlias (\s a -> s { _ieImageOwnerAlias = a })
{-# INLINE ieImageOwnerAlias #-}

-- | The name of the AMI that was provided during image creation.
ieName :: Lens' Image (Text)
ieName = lens _ieName (\s a -> s { _ieName = a })
{-# INLINE ieName #-}

-- | The description of the AMI that was provided during image creation.
ieDescription :: Lens' Image (Maybe Text)
ieDescription = lens _ieDescription (\s a -> s { _ieDescription = a })
{-# INLINE ieDescription #-}

-- | The type of root device used by the AMI. The AMI can use an Amazon EBS
-- volume or an instance store volume.
ieRootDeviceType :: Lens' Image (DeviceType)
ieRootDeviceType = lens _ieRootDeviceType (\s a -> s { _ieRootDeviceType = a })
{-# INLINE ieRootDeviceType #-}

-- | The device name of the root device (for example, /dev/sda1 or xvda).
ieRootDeviceName :: Lens' Image (Maybe Text)
ieRootDeviceName = lens _ieRootDeviceName (\s a -> s { _ieRootDeviceName = a })
{-# INLINE ieRootDeviceName #-}

-- | Any block device mapping entries.
ieBlockDeviceMappings :: Lens' Image ([BlockDeviceMapping])
ieBlockDeviceMappings = lens _ieBlockDeviceMappings (\s a -> s { _ieBlockDeviceMappings = a })
{-# INLINE ieBlockDeviceMappings #-}

-- | The type of virtualization of the AMI.
ieVirtualizationType :: Lens' Image (VirtualizationType)
ieVirtualizationType = lens _ieVirtualizationType (\s a -> s { _ieVirtualizationType = a })
{-# INLINE ieVirtualizationType #-}

-- | Any tags assigned to the image.
ieTags :: Lens' Image ([Tag])
ieTags = lens _ieTags (\s a -> s { _ieTags = a })
{-# INLINE ieTags #-}

-- | The hypervisor type of the image.
ieHypervisor :: Lens' Image (HypervisorType)
ieHypervisor = lens _ieHypervisor (\s a -> s { _ieHypervisor = a })
{-# INLINE ieHypervisor #-}

instance FromXML Image where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | 
data ImportInstanceLaunchSpecification = ImportInstanceLaunchSpecification
    { _iilsArchitecture :: Maybe ArchitectureValues
      -- ^ The architecture of the instance.
    , _iilsGroupNames :: [Text]
      -- ^ One or more security group names.
    , _iilsAdditionalInfo :: Maybe Text
      -- ^ 
    , _iilsUserData :: Maybe Text
      -- ^ User data to be made available to the instance.
    , _iilsInstanceType :: Maybe InstanceType
      -- ^ The instance type. For more information, see Instance Types in
      -- the Amazon Elastic Compute Cloud User Guide.
    , _iilsPlacement :: Maybe Placement
      -- ^ 
    , _iilsMonitoring :: Maybe Bool
      -- ^ 
    , _iilsSubnetId :: Maybe Text
      -- ^ [EC2-VPC] The ID of the subnet to launch the instance into.
    , _iilsInstanceInitiatedShutdownBehavior :: Maybe ShutdownBehavior
      -- ^ Indicates whether an instance stops or terminates when you
      -- initiate shutdown from the instance (using the operating system
      -- command for system shutdown).
    , _iilsPrivateIpAddress :: Maybe Text
      -- ^ [EC2-VPC] Optionally, you can use this parameter to assign the
      -- instance a specific available IP address from the IP address
      -- range of the subnet.
    } deriving (Show, Generic)

-- | The architecture of the instance.
iilsArchitecture :: Lens' ImportInstanceLaunchSpecification (Maybe ArchitectureValues)
iilsArchitecture = lens _iilsArchitecture (\s a -> s { _iilsArchitecture = a })
{-# INLINE iilsArchitecture #-}

-- | One or more security group names.
iilsGroupNames :: Lens' ImportInstanceLaunchSpecification ([Text])
iilsGroupNames = lens _iilsGroupNames (\s a -> s { _iilsGroupNames = a })
{-# INLINE iilsGroupNames #-}

-- | 
iilsAdditionalInfo :: Lens' ImportInstanceLaunchSpecification (Maybe Text)
iilsAdditionalInfo = lens _iilsAdditionalInfo (\s a -> s { _iilsAdditionalInfo = a })
{-# INLINE iilsAdditionalInfo #-}

-- | User data to be made available to the instance.
iilsUserData :: Lens' ImportInstanceLaunchSpecification (Maybe Text)
iilsUserData = lens _iilsUserData (\s a -> s { _iilsUserData = a })
{-# INLINE iilsUserData #-}

-- | The instance type. For more information, see Instance Types in the Amazon
-- Elastic Compute Cloud User Guide.
iilsInstanceType :: Lens' ImportInstanceLaunchSpecification (Maybe InstanceType)
iilsInstanceType = lens _iilsInstanceType (\s a -> s { _iilsInstanceType = a })
{-# INLINE iilsInstanceType #-}

-- | 
iilsPlacement :: Lens' ImportInstanceLaunchSpecification (Maybe Placement)
iilsPlacement = lens _iilsPlacement (\s a -> s { _iilsPlacement = a })
{-# INLINE iilsPlacement #-}

-- | 
iilsMonitoring :: Lens' ImportInstanceLaunchSpecification (Maybe Bool)
iilsMonitoring = lens _iilsMonitoring (\s a -> s { _iilsMonitoring = a })
{-# INLINE iilsMonitoring #-}

-- | [EC2-VPC] The ID of the subnet to launch the instance into.
iilsSubnetId :: Lens' ImportInstanceLaunchSpecification (Maybe Text)
iilsSubnetId = lens _iilsSubnetId (\s a -> s { _iilsSubnetId = a })
{-# INLINE iilsSubnetId #-}

-- | Indicates whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for system
-- shutdown).
iilsInstanceInitiatedShutdownBehavior :: Lens' ImportInstanceLaunchSpecification (Maybe ShutdownBehavior)
iilsInstanceInitiatedShutdownBehavior = lens _iilsInstanceInitiatedShutdownBehavior (\s a -> s { _iilsInstanceInitiatedShutdownBehavior = a })
{-# INLINE iilsInstanceInitiatedShutdownBehavior #-}

-- | [EC2-VPC] Optionally, you can use this parameter to assign the instance a
-- specific available IP address from the IP address range of the subnet.
iilsPrivateIpAddress :: Lens' ImportInstanceLaunchSpecification (Maybe Text)
iilsPrivateIpAddress = lens _iilsPrivateIpAddress (\s a -> s { _iilsPrivateIpAddress = a })
{-# INLINE iilsPrivateIpAddress #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ImportInstanceLaunchSpecification' data type to populate a request.
mkImportInstanceLaunchSpecification :: ImportInstanceLaunchSpecification
mkImportInstanceLaunchSpecification = ImportInstanceLaunchSpecification
    { _iilsArchitecture = Nothing
    , _iilsGroupNames = mempty
    , _iilsAdditionalInfo = Nothing
    , _iilsUserData = Nothing
    , _iilsInstanceType = Nothing
    , _iilsPlacement = Nothing
    , _iilsMonitoring = Nothing
    , _iilsSubnetId = Nothing
    , _iilsInstanceInitiatedShutdownBehavior = Nothing
    , _iilsPrivateIpAddress = Nothing
    }
{-# INLINE mkImportInstanceLaunchSpecification #-}

instance ToQuery ImportInstanceLaunchSpecification where
    toQuery = genericQuery def

-- | If the task is for importing an instance, this contains information about
-- the import instance task.
data ImportInstanceTaskDetails = ImportInstanceTaskDetails
    { _iitdVolumes :: [ImportInstanceVolumeDetailItem]
      -- ^ 
    , _iitdInstanceId :: Maybe Text
      -- ^ 
    , _iitdPlatform :: Maybe PlatformValues
      -- ^ The instance operating system.
    , _iitdDescription :: Maybe Text
      -- ^ 
    } deriving (Show, Generic)

-- | 
iitdVolumes :: Lens' ImportInstanceTaskDetails ([ImportInstanceVolumeDetailItem])
iitdVolumes = lens _iitdVolumes (\s a -> s { _iitdVolumes = a })
{-# INLINE iitdVolumes #-}

-- | 
iitdInstanceId :: Lens' ImportInstanceTaskDetails (Maybe Text)
iitdInstanceId = lens _iitdInstanceId (\s a -> s { _iitdInstanceId = a })
{-# INLINE iitdInstanceId #-}

-- | The instance operating system.
iitdPlatform :: Lens' ImportInstanceTaskDetails (Maybe PlatformValues)
iitdPlatform = lens _iitdPlatform (\s a -> s { _iitdPlatform = a })
{-# INLINE iitdPlatform #-}

-- | 
iitdDescription :: Lens' ImportInstanceTaskDetails (Maybe Text)
iitdDescription = lens _iitdDescription (\s a -> s { _iitdDescription = a })
{-# INLINE iitdDescription #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ImportInstanceTaskDetails' data type to populate a request.
mkImportInstanceTaskDetails :: [ImportInstanceVolumeDetailItem] -- ^ 'iitdVolumes'
                            -> ImportInstanceTaskDetails
mkImportInstanceTaskDetails p1 = ImportInstanceTaskDetails
    { _iitdVolumes = p1
    , _iitdInstanceId = Nothing
    , _iitdPlatform = Nothing
    , _iitdDescription = Nothing
    }
{-# INLINE mkImportInstanceTaskDetails #-}

instance FromXML ImportInstanceTaskDetails where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "importInstance"

instance ToQuery ImportInstanceTaskDetails where
    toQuery = genericQuery def

-- | Describes an import volume task.
data ImportInstanceVolumeDetailItem = ImportInstanceVolumeDetailItem
    { _iivdiBytesConverted :: Integer
      -- ^ The number of bytes converted so far.
    , _iivdiAvailabilityZone :: Text
      -- ^ The Availability Zone where the resulting instance will reside.
    , _iivdiImage :: DiskImageDescription
      -- ^ The image.
    , _iivdiVolume :: DiskImageVolumeDescription
      -- ^ The volume.
    , _iivdiStatus :: Text
      -- ^ The status of the import of this particular disk image.
    , _iivdiStatusMessage :: Maybe Text
      -- ^ The status information or errors related to the disk image.
    , _iivdiDescription :: Maybe Text
      -- ^ 
    } deriving (Show, Generic)

-- | The number of bytes converted so far.
iivdiBytesConverted :: Lens' ImportInstanceVolumeDetailItem (Integer)
iivdiBytesConverted = lens _iivdiBytesConverted (\s a -> s { _iivdiBytesConverted = a })
{-# INLINE iivdiBytesConverted #-}

-- | The Availability Zone where the resulting instance will reside.
iivdiAvailabilityZone :: Lens' ImportInstanceVolumeDetailItem (Text)
iivdiAvailabilityZone = lens _iivdiAvailabilityZone (\s a -> s { _iivdiAvailabilityZone = a })
{-# INLINE iivdiAvailabilityZone #-}

-- | The image.
iivdiImage :: Lens' ImportInstanceVolumeDetailItem (DiskImageDescription)
iivdiImage = lens _iivdiImage (\s a -> s { _iivdiImage = a })
{-# INLINE iivdiImage #-}

-- | The volume.
iivdiVolume :: Lens' ImportInstanceVolumeDetailItem (DiskImageVolumeDescription)
iivdiVolume = lens _iivdiVolume (\s a -> s { _iivdiVolume = a })
{-# INLINE iivdiVolume #-}

-- | The status of the import of this particular disk image.
iivdiStatus :: Lens' ImportInstanceVolumeDetailItem (Text)
iivdiStatus = lens _iivdiStatus (\s a -> s { _iivdiStatus = a })
{-# INLINE iivdiStatus #-}

-- | The status information or errors related to the disk image.
iivdiStatusMessage :: Lens' ImportInstanceVolumeDetailItem (Maybe Text)
iivdiStatusMessage = lens _iivdiStatusMessage (\s a -> s { _iivdiStatusMessage = a })
{-# INLINE iivdiStatusMessage #-}

-- | 
iivdiDescription :: Lens' ImportInstanceVolumeDetailItem (Maybe Text)
iivdiDescription = lens _iivdiDescription (\s a -> s { _iivdiDescription = a })
{-# INLINE iivdiDescription #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ImportInstanceVolumeDetailItem' data type to populate a request.
mkImportInstanceVolumeDetailItem :: Integer -- ^ 'iivdiBytesConverted'
                                 -> Text -- ^ 'iivdiAvailabilityZone'
                                 -> DiskImageDescription -- ^ 'iivdiImage'
                                 -> DiskImageVolumeDescription -- ^ 'iivdiVolume'
                                 -> Text -- ^ 'iivdiStatus'
                                 -> ImportInstanceVolumeDetailItem
mkImportInstanceVolumeDetailItem p1 p2 p3 p4 p5 = ImportInstanceVolumeDetailItem
    { _iivdiBytesConverted = p1
    , _iivdiAvailabilityZone = p2
    , _iivdiImage = p3
    , _iivdiVolume = p4
    , _iivdiStatus = p5
    , _iivdiStatusMessage = Nothing
    , _iivdiDescription = Nothing
    }
{-# INLINE mkImportInstanceVolumeDetailItem #-}

instance FromXML ImportInstanceVolumeDetailItem where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery ImportInstanceVolumeDetailItem where
    toQuery = genericQuery def

-- | If the task is for importing a volume, this contains information about the
-- import volume task.
data ImportVolumeTaskDetails = ImportVolumeTaskDetails
    { _ivtdBytesConverted :: Integer
      -- ^ The number of bytes converted so far.
    , _ivtdAvailabilityZone :: Text
      -- ^ The Availability Zone where the resulting volume will reside.
    , _ivtdDescription :: Maybe Text
      -- ^ The description you provided when starting the import volume
      -- task.
    , _ivtdImage :: DiskImageDescription
      -- ^ The image.
    , _ivtdVolume :: DiskImageVolumeDescription
      -- ^ The volume.
    } deriving (Show, Generic)

-- | The number of bytes converted so far.
ivtdBytesConverted :: Lens' ImportVolumeTaskDetails (Integer)
ivtdBytesConverted = lens _ivtdBytesConverted (\s a -> s { _ivtdBytesConverted = a })
{-# INLINE ivtdBytesConverted #-}

-- | The Availability Zone where the resulting volume will reside.
ivtdAvailabilityZone :: Lens' ImportVolumeTaskDetails (Text)
ivtdAvailabilityZone = lens _ivtdAvailabilityZone (\s a -> s { _ivtdAvailabilityZone = a })
{-# INLINE ivtdAvailabilityZone #-}

-- | The description you provided when starting the import volume task.
ivtdDescription :: Lens' ImportVolumeTaskDetails (Maybe Text)
ivtdDescription = lens _ivtdDescription (\s a -> s { _ivtdDescription = a })
{-# INLINE ivtdDescription #-}

-- | The image.
ivtdImage :: Lens' ImportVolumeTaskDetails (DiskImageDescription)
ivtdImage = lens _ivtdImage (\s a -> s { _ivtdImage = a })
{-# INLINE ivtdImage #-}

-- | The volume.
ivtdVolume :: Lens' ImportVolumeTaskDetails (DiskImageVolumeDescription)
ivtdVolume = lens _ivtdVolume (\s a -> s { _ivtdVolume = a })
{-# INLINE ivtdVolume #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ImportVolumeTaskDetails' data type to populate a request.
mkImportVolumeTaskDetails :: Integer -- ^ 'ivtdBytesConverted'
                          -> Text -- ^ 'ivtdAvailabilityZone'
                          -> DiskImageDescription -- ^ 'ivtdImage'
                          -> DiskImageVolumeDescription -- ^ 'ivtdVolume'
                          -> ImportVolumeTaskDetails
mkImportVolumeTaskDetails p1 p2 p3 p4 = ImportVolumeTaskDetails
    { _ivtdBytesConverted = p1
    , _ivtdAvailabilityZone = p2
    , _ivtdDescription = Nothing
    , _ivtdImage = p4
    , _ivtdVolume = p5
    }
{-# INLINE mkImportVolumeTaskDetails #-}

instance FromXML ImportVolumeTaskDetails where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "importVolume"

instance ToQuery ImportVolumeTaskDetails where
    toQuery = genericQuery def

-- | Describes an instance.
data Instance = Instance
    { _ifInstanceId :: Maybe Text
      -- ^ The ID of the instance.
    , _ifImageId :: Maybe Text
      -- ^ The ID of the AMI used to launch the instance.
    , _ifState :: Maybe InstanceState
      -- ^ The current state of the instance.
    , _ifPrivateDnsName :: Maybe Text
      -- ^ The private DNS name assigned to the instance. This DNS name can
      -- only be used inside the Amazon EC2 network. This name is not
      -- available until the instance enters the running state.
    , _ifPublicDnsName :: Maybe Text
      -- ^ The public DNS name assigned to the instance. This name is not
      -- available until the instance enters the running state.
    , _ifStateTransitionReason :: Maybe Text
      -- ^ The reason for the most recent state transition. This might be an
      -- empty string.
    , _ifKeyName :: Maybe Text
      -- ^ The name of the key pair, if this instance was launched with an
      -- associated key pair.
    , _ifAmiLaunchIndex :: Maybe Integer
      -- ^ The AMI launch index, which can be used to find this instance in
      -- the launch group.
    , _ifProductCodes :: [ProductCode]
      -- ^ The product codes attached to this instance.
    , _ifInstanceType :: Maybe InstanceType
      -- ^ The instance type.
    , _ifLaunchTime :: Maybe ISO8601
      -- ^ The time the instance was launched.
    , _ifPlacement :: Maybe Placement
      -- ^ The location where the instance launched.
    , _ifKernelId :: Maybe Text
      -- ^ The kernel associated with this instance.
    , _ifRamdiskId :: Maybe Text
      -- ^ The RAM disk associated with this instance.
    , _ifPlatform :: Maybe PlatformValues
      -- ^ The value is Windows for Windows instances; otherwise blank.
    , _ifMonitoring :: Maybe Monitoring
      -- ^ The monitoring information for the instance.
    , _ifSubnetId :: Maybe Text
      -- ^ The ID of the subnet in which the instance is running.
    , _ifVpcId :: Maybe Text
      -- ^ The ID of the VPC in which the instance is running.
    , _ifPrivateIpAddress :: Maybe Text
      -- ^ The private IP address assigned to the instance.
    , _ifPublicIpAddress :: Maybe Text
      -- ^ The public IP address assigned to the instance.
    , _ifStateReason :: Maybe StateReason
      -- ^ The reason for the most recent state transition.
    , _ifArchitecture :: Maybe ArchitectureValues
      -- ^ The architecture of the image.
    , _ifRootDeviceType :: Maybe DeviceType
      -- ^ The root device type used by the AMI. The AMI can use an Amazon
      -- EBS volume or an instance store volume.
    , _ifRootDeviceName :: Maybe Text
      -- ^ The root device name (for example, /dev/sda1).
    , _ifBlockDeviceMappings :: [InstanceBlockDeviceMapping]
      -- ^ Any block device mapping entries for the instance.
    , _ifVirtualizationType :: Maybe VirtualizationType
      -- ^ The virtualization type of the instance.
    , _ifInstanceLifecycle :: Maybe InstanceLifecycleType
      -- ^ Indicates whether this is a Spot Instance.
    , _ifSpotInstanceRequestId :: Maybe Text
      -- ^ The ID of the Spot Instance request.
    , _ifClientToken :: Maybe Text
      -- ^ The idempotency token you provided when you launched the
      -- instance.
    , _ifTags :: [Tag]
      -- ^ Any tags assigned to the instance.
    , _ifSecurityGroups :: [GroupIdentifier]
      -- ^ One or more security groups for the instance.
    , _ifSourceDestCheck :: Maybe Bool
      -- ^ Specifies whether to enable an instance launched in a VPC to
      -- perform NAT. This controls whether source/destination checking is
      -- enabled on the instance. A value of true means checking is
      -- enabled, and false means checking is disabled. The value must be
      -- false for the instance to perform NAT. For more information, see
      -- NAT Instances in the Amazon Virtual Private Cloud User Guide.
    , _ifHypervisor :: Maybe HypervisorType
      -- ^ The hypervisor type of the instance.
    , _ifNetworkInterfaces :: [InstanceNetworkInterface]
      -- ^ [EC2-VPC] One or more network interfaces for the instance.
    , _ifIamInstanceProfile :: Maybe IamInstanceProfile
      -- ^ The IAM instance profile associated with the instance.
    , _ifEbsOptimized :: Maybe Bool
      -- ^ Indicates whether the instance is optimized for EBS I/O. This
      -- optimization provides dedicated throughput to Amazon EBS and an
      -- optimized configuration stack to provide optimal I/O performance.
      -- This optimization isn't available with all instance types.
      -- Additional usage charges apply when using an EBS Optimized
      -- instance.
    , _ifSriovNetSupport :: Maybe Text
      -- ^ Specifies whether enhanced networking is enabled.
    } deriving (Show, Generic)

-- | The ID of the instance.
ifInstanceId :: Lens' Instance (Maybe Text)
ifInstanceId = lens _ifInstanceId (\s a -> s { _ifInstanceId = a })
{-# INLINE ifInstanceId #-}

-- | The ID of the AMI used to launch the instance.
ifImageId :: Lens' Instance (Maybe Text)
ifImageId = lens _ifImageId (\s a -> s { _ifImageId = a })
{-# INLINE ifImageId #-}

-- | The current state of the instance.
ifState :: Lens' Instance (Maybe InstanceState)
ifState = lens _ifState (\s a -> s { _ifState = a })
{-# INLINE ifState #-}

-- | The private DNS name assigned to the instance. This DNS name can only be
-- used inside the Amazon EC2 network. This name is not available until the
-- instance enters the running state.
ifPrivateDnsName :: Lens' Instance (Maybe Text)
ifPrivateDnsName = lens _ifPrivateDnsName (\s a -> s { _ifPrivateDnsName = a })
{-# INLINE ifPrivateDnsName #-}

-- | The public DNS name assigned to the instance. This name is not available
-- until the instance enters the running state.
ifPublicDnsName :: Lens' Instance (Maybe Text)
ifPublicDnsName = lens _ifPublicDnsName (\s a -> s { _ifPublicDnsName = a })
{-# INLINE ifPublicDnsName #-}

-- | The reason for the most recent state transition. This might be an empty
-- string.
ifStateTransitionReason :: Lens' Instance (Maybe Text)
ifStateTransitionReason = lens _ifStateTransitionReason (\s a -> s { _ifStateTransitionReason = a })
{-# INLINE ifStateTransitionReason #-}

-- | The name of the key pair, if this instance was launched with an associated
-- key pair.
ifKeyName :: Lens' Instance (Maybe Text)
ifKeyName = lens _ifKeyName (\s a -> s { _ifKeyName = a })
{-# INLINE ifKeyName #-}

-- | The AMI launch index, which can be used to find this instance in the launch
-- group.
ifAmiLaunchIndex :: Lens' Instance (Maybe Integer)
ifAmiLaunchIndex = lens _ifAmiLaunchIndex (\s a -> s { _ifAmiLaunchIndex = a })
{-# INLINE ifAmiLaunchIndex #-}

-- | The product codes attached to this instance.
ifProductCodes :: Lens' Instance ([ProductCode])
ifProductCodes = lens _ifProductCodes (\s a -> s { _ifProductCodes = a })
{-# INLINE ifProductCodes #-}

-- | The instance type.
ifInstanceType :: Lens' Instance (Maybe InstanceType)
ifInstanceType = lens _ifInstanceType (\s a -> s { _ifInstanceType = a })
{-# INLINE ifInstanceType #-}

-- | The time the instance was launched.
ifLaunchTime :: Lens' Instance (Maybe ISO8601)
ifLaunchTime = lens _ifLaunchTime (\s a -> s { _ifLaunchTime = a })
{-# INLINE ifLaunchTime #-}

-- | The location where the instance launched.
ifPlacement :: Lens' Instance (Maybe Placement)
ifPlacement = lens _ifPlacement (\s a -> s { _ifPlacement = a })
{-# INLINE ifPlacement #-}

-- | The kernel associated with this instance.
ifKernelId :: Lens' Instance (Maybe Text)
ifKernelId = lens _ifKernelId (\s a -> s { _ifKernelId = a })
{-# INLINE ifKernelId #-}

-- | The RAM disk associated with this instance.
ifRamdiskId :: Lens' Instance (Maybe Text)
ifRamdiskId = lens _ifRamdiskId (\s a -> s { _ifRamdiskId = a })
{-# INLINE ifRamdiskId #-}

-- | The value is Windows for Windows instances; otherwise blank.
ifPlatform :: Lens' Instance (Maybe PlatformValues)
ifPlatform = lens _ifPlatform (\s a -> s { _ifPlatform = a })
{-# INLINE ifPlatform #-}

-- | The monitoring information for the instance.
ifMonitoring :: Lens' Instance (Maybe Monitoring)
ifMonitoring = lens _ifMonitoring (\s a -> s { _ifMonitoring = a })
{-# INLINE ifMonitoring #-}

-- | The ID of the subnet in which the instance is running.
ifSubnetId :: Lens' Instance (Maybe Text)
ifSubnetId = lens _ifSubnetId (\s a -> s { _ifSubnetId = a })
{-# INLINE ifSubnetId #-}

-- | The ID of the VPC in which the instance is running.
ifVpcId :: Lens' Instance (Maybe Text)
ifVpcId = lens _ifVpcId (\s a -> s { _ifVpcId = a })
{-# INLINE ifVpcId #-}

-- | The private IP address assigned to the instance.
ifPrivateIpAddress :: Lens' Instance (Maybe Text)
ifPrivateIpAddress = lens _ifPrivateIpAddress (\s a -> s { _ifPrivateIpAddress = a })
{-# INLINE ifPrivateIpAddress #-}

-- | The public IP address assigned to the instance.
ifPublicIpAddress :: Lens' Instance (Maybe Text)
ifPublicIpAddress = lens _ifPublicIpAddress (\s a -> s { _ifPublicIpAddress = a })
{-# INLINE ifPublicIpAddress #-}

-- | The reason for the most recent state transition.
ifStateReason :: Lens' Instance (Maybe StateReason)
ifStateReason = lens _ifStateReason (\s a -> s { _ifStateReason = a })
{-# INLINE ifStateReason #-}

-- | The architecture of the image.
ifArchitecture :: Lens' Instance (Maybe ArchitectureValues)
ifArchitecture = lens _ifArchitecture (\s a -> s { _ifArchitecture = a })
{-# INLINE ifArchitecture #-}

-- | The root device type used by the AMI. The AMI can use an Amazon EBS volume
-- or an instance store volume.
ifRootDeviceType :: Lens' Instance (Maybe DeviceType)
ifRootDeviceType = lens _ifRootDeviceType (\s a -> s { _ifRootDeviceType = a })
{-# INLINE ifRootDeviceType #-}

-- | The root device name (for example, /dev/sda1).
ifRootDeviceName :: Lens' Instance (Maybe Text)
ifRootDeviceName = lens _ifRootDeviceName (\s a -> s { _ifRootDeviceName = a })
{-# INLINE ifRootDeviceName #-}

-- | Any block device mapping entries for the instance.
ifBlockDeviceMappings :: Lens' Instance ([InstanceBlockDeviceMapping])
ifBlockDeviceMappings = lens _ifBlockDeviceMappings (\s a -> s { _ifBlockDeviceMappings = a })
{-# INLINE ifBlockDeviceMappings #-}

-- | The virtualization type of the instance.
ifVirtualizationType :: Lens' Instance (Maybe VirtualizationType)
ifVirtualizationType = lens _ifVirtualizationType (\s a -> s { _ifVirtualizationType = a })
{-# INLINE ifVirtualizationType #-}

-- | Indicates whether this is a Spot Instance.
ifInstanceLifecycle :: Lens' Instance (Maybe InstanceLifecycleType)
ifInstanceLifecycle = lens _ifInstanceLifecycle (\s a -> s { _ifInstanceLifecycle = a })
{-# INLINE ifInstanceLifecycle #-}

-- | The ID of the Spot Instance request.
ifSpotInstanceRequestId :: Lens' Instance (Maybe Text)
ifSpotInstanceRequestId = lens _ifSpotInstanceRequestId (\s a -> s { _ifSpotInstanceRequestId = a })
{-# INLINE ifSpotInstanceRequestId #-}

-- | The idempotency token you provided when you launched the instance.
ifClientToken :: Lens' Instance (Maybe Text)
ifClientToken = lens _ifClientToken (\s a -> s { _ifClientToken = a })
{-# INLINE ifClientToken #-}

-- | Any tags assigned to the instance.
ifTags :: Lens' Instance ([Tag])
ifTags = lens _ifTags (\s a -> s { _ifTags = a })
{-# INLINE ifTags #-}

-- | One or more security groups for the instance.
ifSecurityGroups :: Lens' Instance ([GroupIdentifier])
ifSecurityGroups = lens _ifSecurityGroups (\s a -> s { _ifSecurityGroups = a })
{-# INLINE ifSecurityGroups #-}

-- | Specifies whether to enable an instance launched in a VPC to perform NAT.
-- This controls whether source/destination checking is enabled on the
-- instance. A value of true means checking is enabled, and false means
-- checking is disabled. The value must be false for the instance to perform
-- NAT. For more information, see NAT Instances in the Amazon Virtual Private
-- Cloud User Guide.
ifSourceDestCheck :: Lens' Instance (Maybe Bool)
ifSourceDestCheck = lens _ifSourceDestCheck (\s a -> s { _ifSourceDestCheck = a })
{-# INLINE ifSourceDestCheck #-}

-- | The hypervisor type of the instance.
ifHypervisor :: Lens' Instance (Maybe HypervisorType)
ifHypervisor = lens _ifHypervisor (\s a -> s { _ifHypervisor = a })
{-# INLINE ifHypervisor #-}

-- | [EC2-VPC] One or more network interfaces for the instance.
ifNetworkInterfaces :: Lens' Instance ([InstanceNetworkInterface])
ifNetworkInterfaces = lens _ifNetworkInterfaces (\s a -> s { _ifNetworkInterfaces = a })
{-# INLINE ifNetworkInterfaces #-}

-- | The IAM instance profile associated with the instance.
ifIamInstanceProfile :: Lens' Instance (Maybe IamInstanceProfile)
ifIamInstanceProfile = lens _ifIamInstanceProfile (\s a -> s { _ifIamInstanceProfile = a })
{-# INLINE ifIamInstanceProfile #-}

-- | Indicates whether the instance is optimized for EBS I/O. This optimization
-- provides dedicated throughput to Amazon EBS and an optimized configuration
-- stack to provide optimal I/O performance. This optimization isn't available
-- with all instance types. Additional usage charges apply when using an EBS
-- Optimized instance.
ifEbsOptimized :: Lens' Instance (Maybe Bool)
ifEbsOptimized = lens _ifEbsOptimized (\s a -> s { _ifEbsOptimized = a })
{-# INLINE ifEbsOptimized #-}

-- | Specifies whether enhanced networking is enabled.
ifSriovNetSupport :: Lens' Instance (Maybe Text)
ifSriovNetSupport = lens _ifSriovNetSupport (\s a -> s { _ifSriovNetSupport = a })
{-# INLINE ifSriovNetSupport #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Instance' data type to populate a request.
mkInstance :: Instance
mkInstance = Instance
    { _ifInstanceId = Nothing
    , _ifImageId = Nothing
    , _ifState = Nothing
    , _ifPrivateDnsName = Nothing
    , _ifPublicDnsName = Nothing
    , _ifStateTransitionReason = Nothing
    , _ifKeyName = Nothing
    , _ifAmiLaunchIndex = Nothing
    , _ifProductCodes = mempty
    , _ifInstanceType = Nothing
    , _ifLaunchTime = Nothing
    , _ifPlacement = Nothing
    , _ifKernelId = Nothing
    , _ifRamdiskId = Nothing
    , _ifPlatform = Nothing
    , _ifMonitoring = Nothing
    , _ifSubnetId = Nothing
    , _ifVpcId = Nothing
    , _ifPrivateIpAddress = Nothing
    , _ifPublicIpAddress = Nothing
    , _ifStateReason = Nothing
    , _ifArchitecture = Nothing
    , _ifRootDeviceType = Nothing
    , _ifRootDeviceName = Nothing
    , _ifBlockDeviceMappings = mempty
    , _ifVirtualizationType = Nothing
    , _ifInstanceLifecycle = Nothing
    , _ifSpotInstanceRequestId = Nothing
    , _ifClientToken = Nothing
    , _ifTags = mempty
    , _ifSecurityGroups = mempty
    , _ifSourceDestCheck = Nothing
    , _ifHypervisor = Nothing
    , _ifNetworkInterfaces = mempty
    , _ifIamInstanceProfile = Nothing
    , _ifEbsOptimized = Nothing
    , _ifSriovNetSupport = Nothing
    }
{-# INLINE mkInstance #-}

instance FromXML Instance where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery Instance where
    toQuery = genericQuery def

-- | Describes a block device mapping.
data InstanceBlockDeviceMapping = InstanceBlockDeviceMapping
    { _ibdmDeviceName :: Maybe Text
      -- ^ The device name exposed to the instance (for example, /dev/sdh).
    , _ibdmEbs :: Maybe EbsInstanceBlockDevice
      -- ^ Parameters used to automatically set up Amazon EBS volumes when
      -- the instance is launched.
    } deriving (Show, Generic)

-- | The device name exposed to the instance (for example, /dev/sdh).
ibdmDeviceName :: Lens' InstanceBlockDeviceMapping (Maybe Text)
ibdmDeviceName = lens _ibdmDeviceName (\s a -> s { _ibdmDeviceName = a })
{-# INLINE ibdmDeviceName #-}

-- | Parameters used to automatically set up Amazon EBS volumes when the
-- instance is launched.
ibdmEbs :: Lens' InstanceBlockDeviceMapping (Maybe EbsInstanceBlockDevice)
ibdmEbs = lens _ibdmEbs (\s a -> s { _ibdmEbs = a })
{-# INLINE ibdmEbs #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceBlockDeviceMapping' data type to populate a request.
mkInstanceBlockDeviceMapping :: InstanceBlockDeviceMapping
mkInstanceBlockDeviceMapping = InstanceBlockDeviceMapping
    { _ibdmDeviceName = Nothing
    , _ibdmEbs = Nothing
    }
{-# INLINE mkInstanceBlockDeviceMapping #-}

instance FromXML InstanceBlockDeviceMapping where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery InstanceBlockDeviceMapping where
    toQuery = genericQuery def

-- | Describes a block device mapping entry.
data InstanceBlockDeviceMappingSpecification = InstanceBlockDeviceMappingSpecification
    { _ibdmsDeviceName :: Maybe Text
      -- ^ The device name exposed to the instance (for example, /dev/sdh).
    , _ibdmsEbs :: Maybe EbsInstanceBlockDeviceSpecification
      -- ^ Parameters used to automatically set up Amazon EBS volumes when
      -- the instance is launched.
    , _ibdmsVirtualName :: Maybe Text
      -- ^ The virtual device name.
    , _ibdmsNoDevice :: Maybe Text
      -- ^ suppress the specified device included in the block device
      -- mapping.
    } deriving (Show, Generic)

-- | The device name exposed to the instance (for example, /dev/sdh).
ibdmsDeviceName :: Lens' InstanceBlockDeviceMappingSpecification (Maybe Text)
ibdmsDeviceName = lens _ibdmsDeviceName (\s a -> s { _ibdmsDeviceName = a })
{-# INLINE ibdmsDeviceName #-}

-- | Parameters used to automatically set up Amazon EBS volumes when the
-- instance is launched.
ibdmsEbs :: Lens' InstanceBlockDeviceMappingSpecification (Maybe EbsInstanceBlockDeviceSpecification)
ibdmsEbs = lens _ibdmsEbs (\s a -> s { _ibdmsEbs = a })
{-# INLINE ibdmsEbs #-}

-- | The virtual device name.
ibdmsVirtualName :: Lens' InstanceBlockDeviceMappingSpecification (Maybe Text)
ibdmsVirtualName = lens _ibdmsVirtualName (\s a -> s { _ibdmsVirtualName = a })
{-# INLINE ibdmsVirtualName #-}

-- | suppress the specified device included in the block device mapping.
ibdmsNoDevice :: Lens' InstanceBlockDeviceMappingSpecification (Maybe Text)
ibdmsNoDevice = lens _ibdmsNoDevice (\s a -> s { _ibdmsNoDevice = a })
{-# INLINE ibdmsNoDevice #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceBlockDeviceMappingSpecification' data type to populate a request.
mkInstanceBlockDeviceMappingSpecification :: InstanceBlockDeviceMappingSpecification
mkInstanceBlockDeviceMappingSpecification = InstanceBlockDeviceMappingSpecification
    { _ibdmsDeviceName = Nothing
    , _ibdmsEbs = Nothing
    , _ibdmsVirtualName = Nothing
    , _ibdmsNoDevice = Nothing
    }
{-# INLINE mkInstanceBlockDeviceMappingSpecification #-}

instance ToQuery InstanceBlockDeviceMappingSpecification where
    toQuery = genericQuery def

-- | Describes a Reserved Instance listing state.
data InstanceCount = InstanceCount
    { _icState :: Maybe ListingState
      -- ^ The states of the listed Reserved Instances.
    , _icInstanceCount :: Maybe Integer
      -- ^ he number of listed Reserved Instances in the state specified by
      -- the state.
    } deriving (Show, Generic)

-- | The states of the listed Reserved Instances.
icState :: Lens' InstanceCount (Maybe ListingState)
icState = lens _icState (\s a -> s { _icState = a })
{-# INLINE icState #-}

-- | he number of listed Reserved Instances in the state specified by the state.
icInstanceCount :: Lens' InstanceCount (Maybe Integer)
icInstanceCount = lens _icInstanceCount (\s a -> s { _icInstanceCount = a })
{-# INLINE icInstanceCount #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceCount' data type to populate a request.
mkInstanceCount :: InstanceCount
mkInstanceCount = InstanceCount
    { _icState = Nothing
    , _icInstanceCount = Nothing
    }
{-# INLINE mkInstanceCount #-}

instance FromXML InstanceCount where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery InstanceCount where
    toQuery = genericQuery def

-- | The instance being exported.
data InstanceExportDetails = InstanceExportDetails
    { _iedInstanceId :: Maybe Text
      -- ^ The ID of the resource being exported.
    , _iedTargetEnvironment :: Maybe ExportEnvironment
      -- ^ The target virtualization environment.
    } deriving (Show, Generic)

-- | The ID of the resource being exported.
iedInstanceId :: Lens' InstanceExportDetails (Maybe Text)
iedInstanceId = lens _iedInstanceId (\s a -> s { _iedInstanceId = a })
{-# INLINE iedInstanceId #-}

-- | The target virtualization environment.
iedTargetEnvironment :: Lens' InstanceExportDetails (Maybe ExportEnvironment)
iedTargetEnvironment = lens _iedTargetEnvironment (\s a -> s { _iedTargetEnvironment = a })
{-# INLINE iedTargetEnvironment #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceExportDetails' data type to populate a request.
mkInstanceExportDetails :: InstanceExportDetails
mkInstanceExportDetails = InstanceExportDetails
    { _iedInstanceId = Nothing
    , _iedTargetEnvironment = Nothing
    }
{-# INLINE mkInstanceExportDetails #-}

instance FromXML InstanceExportDetails where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "instanceExport"

instance ToQuery InstanceExportDetails where
    toQuery = genericQuery def

-- | Describes the monitoring information of the instance.
data InstanceMonitoring = InstanceMonitoring
    { _inInstanceId :: Maybe Text
      -- ^ The ID of the instance.
    , _inMonitoring :: Maybe Monitoring
      -- ^ The monitoring information.
    } deriving (Show, Generic)

-- | The ID of the instance.
inInstanceId :: Lens' InstanceMonitoring (Maybe Text)
inInstanceId = lens _inInstanceId (\s a -> s { _inInstanceId = a })
{-# INLINE inInstanceId #-}

-- | The monitoring information.
inMonitoring :: Lens' InstanceMonitoring (Maybe Monitoring)
inMonitoring = lens _inMonitoring (\s a -> s { _inMonitoring = a })
{-# INLINE inMonitoring #-}

instance FromXML InstanceMonitoring where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes a network interface.
data InstanceNetworkInterface = InstanceNetworkInterface
    { _iniNetworkInterfaceId :: Maybe Text
      -- ^ The ID of the network interface.
    , _iniSubnetId :: Maybe Text
      -- ^ The ID of the subnet.
    , _iniVpcId :: Maybe Text
      -- ^ The ID of the VPC.
    , _iniDescription :: Maybe Text
      -- ^ The description.
    , _iniOwnerId :: Maybe Text
      -- ^ The ID of the AWS account that created the network interface.
    , _iniStatus :: Maybe NetworkInterfaceStatus
      -- ^ The status of the network interface.
    , _iniPrivateIpAddress :: Maybe Text
      -- ^ The IP address of the network interface within the subnet.
    , _iniPrivateDnsName :: Maybe Text
      -- ^ The private DNS name.
    , _iniSourceDestCheck :: Maybe Bool
      -- ^ Indicates whether to validate network traffic to or from this
      -- network interface.
    , _iniGroups :: [GroupIdentifier]
      -- ^ One or more security groups.
    , _iniAttachment :: Maybe InstanceNetworkInterfaceAttachment
      -- ^ The network interface attachment.
    , _iniAssociation :: Maybe InstanceNetworkInterfaceAssociation
      -- ^ The association information for an Elastic IP associated with the
      -- network interface.
    , _iniPrivateIpAddresses :: [InstancePrivateIpAddress]
      -- ^ The private IP addresses associated with the network interface.
    } deriving (Show, Generic)

-- | The ID of the network interface.
iniNetworkInterfaceId :: Lens' InstanceNetworkInterface (Maybe Text)
iniNetworkInterfaceId = lens _iniNetworkInterfaceId (\s a -> s { _iniNetworkInterfaceId = a })
{-# INLINE iniNetworkInterfaceId #-}

-- | The ID of the subnet.
iniSubnetId :: Lens' InstanceNetworkInterface (Maybe Text)
iniSubnetId = lens _iniSubnetId (\s a -> s { _iniSubnetId = a })
{-# INLINE iniSubnetId #-}

-- | The ID of the VPC.
iniVpcId :: Lens' InstanceNetworkInterface (Maybe Text)
iniVpcId = lens _iniVpcId (\s a -> s { _iniVpcId = a })
{-# INLINE iniVpcId #-}

-- | The description.
iniDescription :: Lens' InstanceNetworkInterface (Maybe Text)
iniDescription = lens _iniDescription (\s a -> s { _iniDescription = a })
{-# INLINE iniDescription #-}

-- | The ID of the AWS account that created the network interface.
iniOwnerId :: Lens' InstanceNetworkInterface (Maybe Text)
iniOwnerId = lens _iniOwnerId (\s a -> s { _iniOwnerId = a })
{-# INLINE iniOwnerId #-}

-- | The status of the network interface.
iniStatus :: Lens' InstanceNetworkInterface (Maybe NetworkInterfaceStatus)
iniStatus = lens _iniStatus (\s a -> s { _iniStatus = a })
{-# INLINE iniStatus #-}

-- | The IP address of the network interface within the subnet.
iniPrivateIpAddress :: Lens' InstanceNetworkInterface (Maybe Text)
iniPrivateIpAddress = lens _iniPrivateIpAddress (\s a -> s { _iniPrivateIpAddress = a })
{-# INLINE iniPrivateIpAddress #-}

-- | The private DNS name.
iniPrivateDnsName :: Lens' InstanceNetworkInterface (Maybe Text)
iniPrivateDnsName = lens _iniPrivateDnsName (\s a -> s { _iniPrivateDnsName = a })
{-# INLINE iniPrivateDnsName #-}

-- | Indicates whether to validate network traffic to or from this network
-- interface.
iniSourceDestCheck :: Lens' InstanceNetworkInterface (Maybe Bool)
iniSourceDestCheck = lens _iniSourceDestCheck (\s a -> s { _iniSourceDestCheck = a })
{-# INLINE iniSourceDestCheck #-}

-- | One or more security groups.
iniGroups :: Lens' InstanceNetworkInterface ([GroupIdentifier])
iniGroups = lens _iniGroups (\s a -> s { _iniGroups = a })
{-# INLINE iniGroups #-}

-- | The network interface attachment.
iniAttachment :: Lens' InstanceNetworkInterface (Maybe InstanceNetworkInterfaceAttachment)
iniAttachment = lens _iniAttachment (\s a -> s { _iniAttachment = a })
{-# INLINE iniAttachment #-}

-- | The association information for an Elastic IP associated with the network
-- interface.
iniAssociation :: Lens' InstanceNetworkInterface (Maybe InstanceNetworkInterfaceAssociation)
iniAssociation = lens _iniAssociation (\s a -> s { _iniAssociation = a })
{-# INLINE iniAssociation #-}

-- | The private IP addresses associated with the network interface.
iniPrivateIpAddresses :: Lens' InstanceNetworkInterface ([InstancePrivateIpAddress])
iniPrivateIpAddresses = lens _iniPrivateIpAddresses (\s a -> s { _iniPrivateIpAddresses = a })
{-# INLINE iniPrivateIpAddresses #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceNetworkInterface' data type to populate a request.
mkInstanceNetworkInterface :: InstanceNetworkInterface
mkInstanceNetworkInterface = InstanceNetworkInterface
    { _iniNetworkInterfaceId = Nothing
    , _iniSubnetId = Nothing
    , _iniVpcId = Nothing
    , _iniDescription = Nothing
    , _iniOwnerId = Nothing
    , _iniStatus = Nothing
    , _iniPrivateIpAddress = Nothing
    , _iniPrivateDnsName = Nothing
    , _iniSourceDestCheck = Nothing
    , _iniGroups = mempty
    , _iniAttachment = Nothing
    , _iniAssociation = Nothing
    , _iniPrivateIpAddresses = mempty
    }
{-# INLINE mkInstanceNetworkInterface #-}

instance FromXML InstanceNetworkInterface where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery InstanceNetworkInterface where
    toQuery = genericQuery def

-- | The association information for an Elastic IP associated with the network
-- interface.
data InstanceNetworkInterfaceAssociation = InstanceNetworkInterfaceAssociation
    { _inibPublicIp :: Maybe Text
      -- ^ The address of the Elastic IP address bound to the network
      -- interface.
    , _inibPublicDnsName :: Maybe Text
      -- ^ The public DNS name.
    , _inibIpOwnerId :: Maybe Text
      -- ^ The ID of the owner of the Elastic IP address.
    } deriving (Show, Generic)

-- | The address of the Elastic IP address bound to the network interface.
inibPublicIp :: Lens' InstanceNetworkInterfaceAssociation (Maybe Text)
inibPublicIp = lens _inibPublicIp (\s a -> s { _inibPublicIp = a })
{-# INLINE inibPublicIp #-}

-- | The public DNS name.
inibPublicDnsName :: Lens' InstanceNetworkInterfaceAssociation (Maybe Text)
inibPublicDnsName = lens _inibPublicDnsName (\s a -> s { _inibPublicDnsName = a })
{-# INLINE inibPublicDnsName #-}

-- | The ID of the owner of the Elastic IP address.
inibIpOwnerId :: Lens' InstanceNetworkInterfaceAssociation (Maybe Text)
inibIpOwnerId = lens _inibIpOwnerId (\s a -> s { _inibIpOwnerId = a })
{-# INLINE inibIpOwnerId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceNetworkInterfaceAssociation' data type to populate a request.
mkInstanceNetworkInterfaceAssociation :: InstanceNetworkInterfaceAssociation
mkInstanceNetworkInterfaceAssociation = InstanceNetworkInterfaceAssociation
    { _inibPublicIp = Nothing
    , _inibPublicDnsName = Nothing
    , _inibIpOwnerId = Nothing
    }
{-# INLINE mkInstanceNetworkInterfaceAssociation #-}

instance FromXML InstanceNetworkInterfaceAssociation where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "association"

instance ToQuery InstanceNetworkInterfaceAssociation where
    toQuery = genericQuery def

-- | The network interface attachment.
data InstanceNetworkInterfaceAttachment = InstanceNetworkInterfaceAttachment
    { _iniaAttachmentId :: Maybe Text
      -- ^ The ID of the network interface attachment.
    , _iniaDeviceIndex :: Maybe Integer
      -- ^ The index of the device on the instance for the network interface
      -- attachment.
    , _iniaStatus :: Maybe AttachmentStatus
      -- ^ The attachment state.
    , _iniaAttachTime :: Maybe ISO8601
      -- ^ The time stamp when the attachment initiated.
    , _iniaDeleteOnTermination :: Maybe Bool
      -- ^ Indicates whether the network interface is deleted when the
      -- instance is terminated.
    } deriving (Show, Generic)

-- | The ID of the network interface attachment.
iniaAttachmentId :: Lens' InstanceNetworkInterfaceAttachment (Maybe Text)
iniaAttachmentId = lens _iniaAttachmentId (\s a -> s { _iniaAttachmentId = a })
{-# INLINE iniaAttachmentId #-}

-- | The index of the device on the instance for the network interface
-- attachment.
iniaDeviceIndex :: Lens' InstanceNetworkInterfaceAttachment (Maybe Integer)
iniaDeviceIndex = lens _iniaDeviceIndex (\s a -> s { _iniaDeviceIndex = a })
{-# INLINE iniaDeviceIndex #-}

-- | The attachment state.
iniaStatus :: Lens' InstanceNetworkInterfaceAttachment (Maybe AttachmentStatus)
iniaStatus = lens _iniaStatus (\s a -> s { _iniaStatus = a })
{-# INLINE iniaStatus #-}

-- | The time stamp when the attachment initiated.
iniaAttachTime :: Lens' InstanceNetworkInterfaceAttachment (Maybe ISO8601)
iniaAttachTime = lens _iniaAttachTime (\s a -> s { _iniaAttachTime = a })
{-# INLINE iniaAttachTime #-}

-- | Indicates whether the network interface is deleted when the instance is
-- terminated.
iniaDeleteOnTermination :: Lens' InstanceNetworkInterfaceAttachment (Maybe Bool)
iniaDeleteOnTermination = lens _iniaDeleteOnTermination (\s a -> s { _iniaDeleteOnTermination = a })
{-# INLINE iniaDeleteOnTermination #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceNetworkInterfaceAttachment' data type to populate a request.
mkInstanceNetworkInterfaceAttachment :: InstanceNetworkInterfaceAttachment
mkInstanceNetworkInterfaceAttachment = InstanceNetworkInterfaceAttachment
    { _iniaAttachmentId = Nothing
    , _iniaDeviceIndex = Nothing
    , _iniaStatus = Nothing
    , _iniaAttachTime = Nothing
    , _iniaDeleteOnTermination = Nothing
    }
{-# INLINE mkInstanceNetworkInterfaceAttachment #-}

instance FromXML InstanceNetworkInterfaceAttachment where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "attachment"

instance ToQuery InstanceNetworkInterfaceAttachment where
    toQuery = genericQuery def

-- | Describes a network interface.
data InstanceNetworkInterfaceSpecification = InstanceNetworkInterfaceSpecification
    { _inisNetworkInterfaceId :: Maybe Text
      -- ^ The ID of the network interface.
    , _inisDeviceIndex :: Maybe Integer
      -- ^ The index of the device on the instance for the network interface
      -- attachment.
    , _inisSubnetId :: Maybe Text
      -- ^ The ID of the subnet associated with the network string.
    , _inisDescription :: Maybe Text
      -- ^ The description of the network interface.
    , _inisPrivateIpAddress :: Maybe Text
      -- ^ The private IP address of the network interface.
    , _inisGroups :: [Text]
      -- ^ The IDs of the security groups for the network interface.
    , _inisDeleteOnTermination :: Maybe Bool
      -- ^ If set to true, the interface is deleted when the instance is
      -- terminated.
    , _inisPrivateIpAddresses :: [PrivateIpAddressSpecification]
      -- ^ One or more private IP addresses to assign to the network
      -- interface.
    , _inisSecondaryPrivateIpAddressCount :: Maybe Integer
      -- ^ The number of secondary private IP addresses.
    , _inisAssociatePublicIpAddress :: Maybe Bool
      -- ^ Indicates whether to auto-assign a public IP address to an
      -- instance in a VPC. This public IP address can be assigned to the
      -- network interface for eth0 only when you launch the instance. You
      -- must create the network interface instead of using an existing
      -- network interface for eth0, and you must not specify more than
      -- one network interface.
    } deriving (Show, Generic)

-- | The ID of the network interface.
inisNetworkInterfaceId :: Lens' InstanceNetworkInterfaceSpecification (Maybe Text)
inisNetworkInterfaceId = lens _inisNetworkInterfaceId (\s a -> s { _inisNetworkInterfaceId = a })
{-# INLINE inisNetworkInterfaceId #-}

-- | The index of the device on the instance for the network interface
-- attachment.
inisDeviceIndex :: Lens' InstanceNetworkInterfaceSpecification (Maybe Integer)
inisDeviceIndex = lens _inisDeviceIndex (\s a -> s { _inisDeviceIndex = a })
{-# INLINE inisDeviceIndex #-}

-- | The ID of the subnet associated with the network string.
inisSubnetId :: Lens' InstanceNetworkInterfaceSpecification (Maybe Text)
inisSubnetId = lens _inisSubnetId (\s a -> s { _inisSubnetId = a })
{-# INLINE inisSubnetId #-}

-- | The description of the network interface.
inisDescription :: Lens' InstanceNetworkInterfaceSpecification (Maybe Text)
inisDescription = lens _inisDescription (\s a -> s { _inisDescription = a })
{-# INLINE inisDescription #-}

-- | The private IP address of the network interface.
inisPrivateIpAddress :: Lens' InstanceNetworkInterfaceSpecification (Maybe Text)
inisPrivateIpAddress = lens _inisPrivateIpAddress (\s a -> s { _inisPrivateIpAddress = a })
{-# INLINE inisPrivateIpAddress #-}

-- | The IDs of the security groups for the network interface.
inisGroups :: Lens' InstanceNetworkInterfaceSpecification ([Text])
inisGroups = lens _inisGroups (\s a -> s { _inisGroups = a })
{-# INLINE inisGroups #-}

-- | If set to true, the interface is deleted when the instance is terminated.
inisDeleteOnTermination :: Lens' InstanceNetworkInterfaceSpecification (Maybe Bool)
inisDeleteOnTermination = lens _inisDeleteOnTermination (\s a -> s { _inisDeleteOnTermination = a })
{-# INLINE inisDeleteOnTermination #-}

-- | One or more private IP addresses to assign to the network interface.
inisPrivateIpAddresses :: Lens' InstanceNetworkInterfaceSpecification ([PrivateIpAddressSpecification])
inisPrivateIpAddresses = lens _inisPrivateIpAddresses (\s a -> s { _inisPrivateIpAddresses = a })
{-# INLINE inisPrivateIpAddresses #-}

-- | The number of secondary private IP addresses.
inisSecondaryPrivateIpAddressCount :: Lens' InstanceNetworkInterfaceSpecification (Maybe Integer)
inisSecondaryPrivateIpAddressCount = lens _inisSecondaryPrivateIpAddressCount (\s a -> s { _inisSecondaryPrivateIpAddressCount = a })
{-# INLINE inisSecondaryPrivateIpAddressCount #-}

-- | Indicates whether to auto-assign a public IP address to an instance in a
-- VPC. This public IP address can be assigned to the network interface for
-- eth0 only when you launch the instance. You must create the network
-- interface instead of using an existing network interface for eth0, and you
-- must not specify more than one network interface.
inisAssociatePublicIpAddress :: Lens' InstanceNetworkInterfaceSpecification (Maybe Bool)
inisAssociatePublicIpAddress = lens _inisAssociatePublicIpAddress (\s a -> s { _inisAssociatePublicIpAddress = a })
{-# INLINE inisAssociatePublicIpAddress #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceNetworkInterfaceSpecification' data type to populate a request.
mkInstanceNetworkInterfaceSpecification :: InstanceNetworkInterfaceSpecification
mkInstanceNetworkInterfaceSpecification = InstanceNetworkInterfaceSpecification
    { _inisNetworkInterfaceId = Nothing
    , _inisDeviceIndex = Nothing
    , _inisSubnetId = Nothing
    , _inisDescription = Nothing
    , _inisPrivateIpAddress = Nothing
    , _inisGroups = mempty
    , _inisDeleteOnTermination = Nothing
    , _inisPrivateIpAddresses = mempty
    , _inisSecondaryPrivateIpAddressCount = Nothing
    , _inisAssociatePublicIpAddress = Nothing
    }
{-# INLINE mkInstanceNetworkInterfaceSpecification #-}

instance FromXML InstanceNetworkInterfaceSpecification where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery InstanceNetworkInterfaceSpecification where
    toQuery = genericQuery def

-- | Describes a private IP address.
data InstancePrivateIpAddress = InstancePrivateIpAddress
    { _ipiaPrivateIpAddress :: Maybe Text
      -- ^ The private IP address of the network interface.
    , _ipiaPrivateDnsName :: Maybe Text
      -- ^ The private DNS name.
    , _ipiaPrimary :: Maybe Bool
      -- ^ Indicates whether this IP address is the primary private IP
      -- address of the network interface.
    , _ipiaAssociation :: Maybe InstanceNetworkInterfaceAssociation
      -- ^ The association information for an Elastic IP address for the
      -- network interface.
    } deriving (Show, Generic)

-- | The private IP address of the network interface.
ipiaPrivateIpAddress :: Lens' InstancePrivateIpAddress (Maybe Text)
ipiaPrivateIpAddress = lens _ipiaPrivateIpAddress (\s a -> s { _ipiaPrivateIpAddress = a })
{-# INLINE ipiaPrivateIpAddress #-}

-- | The private DNS name.
ipiaPrivateDnsName :: Lens' InstancePrivateIpAddress (Maybe Text)
ipiaPrivateDnsName = lens _ipiaPrivateDnsName (\s a -> s { _ipiaPrivateDnsName = a })
{-# INLINE ipiaPrivateDnsName #-}

-- | Indicates whether this IP address is the primary private IP address of the
-- network interface.
ipiaPrimary :: Lens' InstancePrivateIpAddress (Maybe Bool)
ipiaPrimary = lens _ipiaPrimary (\s a -> s { _ipiaPrimary = a })
{-# INLINE ipiaPrimary #-}

-- | The association information for an Elastic IP address for the network
-- interface.
ipiaAssociation :: Lens' InstancePrivateIpAddress (Maybe InstanceNetworkInterfaceAssociation)
ipiaAssociation = lens _ipiaAssociation (\s a -> s { _ipiaAssociation = a })
{-# INLINE ipiaAssociation #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstancePrivateIpAddress' data type to populate a request.
mkInstancePrivateIpAddress :: InstancePrivateIpAddress
mkInstancePrivateIpAddress = InstancePrivateIpAddress
    { _ipiaPrivateIpAddress = Nothing
    , _ipiaPrivateDnsName = Nothing
    , _ipiaPrimary = Nothing
    , _ipiaAssociation = Nothing
    }
{-# INLINE mkInstancePrivateIpAddress #-}

instance FromXML InstancePrivateIpAddress where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery InstancePrivateIpAddress where
    toQuery = genericQuery def

-- | The intended state of the instance. DescribeInstanceStatus requires that an
-- instance be in the running state.
data InstanceState = InstanceState
    { _iifCode :: Integer
      -- ^ The low byte represents the state. The high byte is an opaque
      -- internal value and should be ignored. 0 : pending 16 : running 32
      -- : shutting-down 48 : terminated 64 : stopping 80 : stopped.
    , _iifName :: InstanceStateName
      -- ^ The current state of the instance.
    } deriving (Show, Generic)

-- | The low byte represents the state. The high byte is an opaque internal
-- value and should be ignored. 0 : pending 16 : running 32 : shutting-down 48
-- : terminated 64 : stopping 80 : stopped.
iifCode :: Lens' InstanceState (Integer)
iifCode = lens _iifCode (\s a -> s { _iifCode = a })
{-# INLINE iifCode #-}

-- | The current state of the instance.
iifName :: Lens' InstanceState (InstanceStateName)
iifName = lens _iifName (\s a -> s { _iifName = a })
{-# INLINE iifName #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceState' data type to populate a request.
mkInstanceState :: Integer -- ^ 'iifCode'
                -> InstanceStateName -- ^ 'iifName'
                -> InstanceState
mkInstanceState p1 p2 = InstanceState
    { _iifCode = p1
    , _iifName = p2
    }
{-# INLINE mkInstanceState #-}

instance FromXML InstanceState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "instanceState"

instance ToQuery InstanceState where
    toQuery = genericQuery def

-- | Describes an instance state change.
data InstanceStateChange = InstanceStateChange
    { _iscInstanceId :: Maybe Text
      -- ^ The ID of the instance.
    , _iscCurrentState :: Maybe InstanceState
      -- ^ The current state of the instance.
    , _iscPreviousState :: Maybe InstanceState
      -- ^ The previous state of the instance.
    } deriving (Show, Generic)

-- | The ID of the instance.
iscInstanceId :: Lens' InstanceStateChange (Maybe Text)
iscInstanceId = lens _iscInstanceId (\s a -> s { _iscInstanceId = a })
{-# INLINE iscInstanceId #-}

-- | The current state of the instance.
iscCurrentState :: Lens' InstanceStateChange (Maybe InstanceState)
iscCurrentState = lens _iscCurrentState (\s a -> s { _iscCurrentState = a })
{-# INLINE iscCurrentState #-}

-- | The previous state of the instance.
iscPreviousState :: Lens' InstanceStateChange (Maybe InstanceState)
iscPreviousState = lens _iscPreviousState (\s a -> s { _iscPreviousState = a })
{-# INLINE iscPreviousState #-}

instance FromXML InstanceStateChange where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes the status of an instance.
data InstanceStatus = InstanceStatus
    { _iiiiivInstanceId :: Maybe Text
      -- ^ The ID of the instance.
    , _iiiiivAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone of the instance.
    , _iiiiivEvents :: [InstanceStatusEvent]
      -- ^ Extra information regarding events associated with the instance.
    , _iiiiivInstanceState :: Maybe InstanceState
      -- ^ The intended state of the instance. DescribeInstanceStatus
      -- requires that an instance be in the running state.
    , _iiiiivSystemStatus :: Maybe InstanceStatusSummary
      -- ^ Reports impaired functionality that stems from issues related to
      -- the systems that support an instance, such as hardware failures
      -- and network connectivity problems.
    , _iiiiivInstanceStatus :: Maybe InstanceStatusSummary
      -- ^ Reports impaired functionality that stems from issues internal to
      -- the instance, such as impaired reachability.
    } deriving (Show, Generic)

-- | The ID of the instance.
iiiiivInstanceId :: Lens' InstanceStatus (Maybe Text)
iiiiivInstanceId = lens _iiiiivInstanceId (\s a -> s { _iiiiivInstanceId = a })
{-# INLINE iiiiivInstanceId #-}

-- | The Availability Zone of the instance.
iiiiivAvailabilityZone :: Lens' InstanceStatus (Maybe Text)
iiiiivAvailabilityZone = lens _iiiiivAvailabilityZone (\s a -> s { _iiiiivAvailabilityZone = a })
{-# INLINE iiiiivAvailabilityZone #-}

-- | Extra information regarding events associated with the instance.
iiiiivEvents :: Lens' InstanceStatus ([InstanceStatusEvent])
iiiiivEvents = lens _iiiiivEvents (\s a -> s { _iiiiivEvents = a })
{-# INLINE iiiiivEvents #-}

-- | The intended state of the instance. DescribeInstanceStatus requires that an
-- instance be in the running state.
iiiiivInstanceState :: Lens' InstanceStatus (Maybe InstanceState)
iiiiivInstanceState = lens _iiiiivInstanceState (\s a -> s { _iiiiivInstanceState = a })
{-# INLINE iiiiivInstanceState #-}

-- | Reports impaired functionality that stems from issues related to the
-- systems that support an instance, such as hardware failures and network
-- connectivity problems.
iiiiivSystemStatus :: Lens' InstanceStatus (Maybe InstanceStatusSummary)
iiiiivSystemStatus = lens _iiiiivSystemStatus (\s a -> s { _iiiiivSystemStatus = a })
{-# INLINE iiiiivSystemStatus #-}

-- | Reports impaired functionality that stems from issues internal to the
-- instance, such as impaired reachability.
iiiiivInstanceStatus :: Lens' InstanceStatus (Maybe InstanceStatusSummary)
iiiiivInstanceStatus = lens _iiiiivInstanceStatus (\s a -> s { _iiiiivInstanceStatus = a })
{-# INLINE iiiiivInstanceStatus #-}

instance FromXML InstanceStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes the instance status.
data InstanceStatusDetails = InstanceStatusDetails
    { _isdName :: Maybe StatusName
      -- ^ The type of instance status.
    , _isdStatus :: Maybe StatusType
      -- ^ The status.
    , _isdImpairedSince :: Maybe ISO8601
      -- ^ The time when a status check failed. For an instance that was
      -- launched and impaired, this is the time when the instance was
      -- launched.
    } deriving (Show, Generic)

-- | The type of instance status.
isdName :: Lens' InstanceStatusDetails (Maybe StatusName)
isdName = lens _isdName (\s a -> s { _isdName = a })
{-# INLINE isdName #-}

-- | The status.
isdStatus :: Lens' InstanceStatusDetails (Maybe StatusType)
isdStatus = lens _isdStatus (\s a -> s { _isdStatus = a })
{-# INLINE isdStatus #-}

-- | The time when a status check failed. For an instance that was launched and
-- impaired, this is the time when the instance was launched.
isdImpairedSince :: Lens' InstanceStatusDetails (Maybe ISO8601)
isdImpairedSince = lens _isdImpairedSince (\s a -> s { _isdImpairedSince = a })
{-# INLINE isdImpairedSince #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceStatusDetails' data type to populate a request.
mkInstanceStatusDetails :: InstanceStatusDetails
mkInstanceStatusDetails = InstanceStatusDetails
    { _isdName = Nothing
    , _isdStatus = Nothing
    , _isdImpairedSince = Nothing
    }
{-# INLINE mkInstanceStatusDetails #-}

instance FromXML InstanceStatusDetails where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery InstanceStatusDetails where
    toQuery = genericQuery def

-- | Describes an instance event.
data InstanceStatusEvent = InstanceStatusEvent
    { _iseCode :: Maybe EventCode
      -- ^ The associated code of the event.
    , _iseDescription :: Maybe Text
      -- ^ A description of the event.
    , _iseNotBefore :: Maybe ISO8601
      -- ^ The earliest scheduled start time for the event.
    , _iseNotAfter :: Maybe ISO8601
      -- ^ The latest scheduled end time for the event.
    } deriving (Show, Generic)

-- | The associated code of the event.
iseCode :: Lens' InstanceStatusEvent (Maybe EventCode)
iseCode = lens _iseCode (\s a -> s { _iseCode = a })
{-# INLINE iseCode #-}

-- | A description of the event.
iseDescription :: Lens' InstanceStatusEvent (Maybe Text)
iseDescription = lens _iseDescription (\s a -> s { _iseDescription = a })
{-# INLINE iseDescription #-}

-- | The earliest scheduled start time for the event.
iseNotBefore :: Lens' InstanceStatusEvent (Maybe ISO8601)
iseNotBefore = lens _iseNotBefore (\s a -> s { _iseNotBefore = a })
{-# INLINE iseNotBefore #-}

-- | The latest scheduled end time for the event.
iseNotAfter :: Lens' InstanceStatusEvent (Maybe ISO8601)
iseNotAfter = lens _iseNotAfter (\s a -> s { _iseNotAfter = a })
{-# INLINE iseNotAfter #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceStatusEvent' data type to populate a request.
mkInstanceStatusEvent :: InstanceStatusEvent
mkInstanceStatusEvent = InstanceStatusEvent
    { _iseCode = Nothing
    , _iseDescription = Nothing
    , _iseNotBefore = Nothing
    , _iseNotAfter = Nothing
    }
{-# INLINE mkInstanceStatusEvent #-}

instance FromXML InstanceStatusEvent where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery InstanceStatusEvent where
    toQuery = genericQuery def

-- | Reports impaired functionality that stems from issues related to the
-- systems that support an instance, such as hardware failures and network
-- connectivity problems.
data InstanceStatusSummary = InstanceStatusSummary
    { _issStatus :: Maybe SummaryStatus
      -- ^ The status.
    , _issDetails :: [InstanceStatusDetails]
      -- ^ The system instance health or application instance health.
    } deriving (Show, Generic)

-- | The status.
issStatus :: Lens' InstanceStatusSummary (Maybe SummaryStatus)
issStatus = lens _issStatus (\s a -> s { _issStatus = a })
{-# INLINE issStatus #-}

-- | The system instance health or application instance health.
issDetails :: Lens' InstanceStatusSummary ([InstanceStatusDetails])
issDetails = lens _issDetails (\s a -> s { _issDetails = a })
{-# INLINE issDetails #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceStatusSummary' data type to populate a request.
mkInstanceStatusSummary :: InstanceStatusSummary
mkInstanceStatusSummary = InstanceStatusSummary
    { _issStatus = Nothing
    , _issDetails = mempty
    }
{-# INLINE mkInstanceStatusSummary #-}

instance FromXML InstanceStatusSummary where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "systemStatus"

instance ToQuery InstanceStatusSummary where
    toQuery = genericQuery def

-- | Information about the Internet gateway.
data InternetGateway = InternetGateway
    { _igInternetGatewayId :: Text
      -- ^ The ID of the Internet gateway.
    , _igAttachments :: [InternetGatewayAttachment]
      -- ^ Any VPCs attached to the Internet gateway.
    , _igTags :: [Tag]
      -- ^ Any tags assigned to the Internet gateway.
    } deriving (Show, Generic)

-- | The ID of the Internet gateway.
igInternetGatewayId :: Lens' InternetGateway (Text)
igInternetGatewayId = lens _igInternetGatewayId (\s a -> s { _igInternetGatewayId = a })
{-# INLINE igInternetGatewayId #-}

-- | Any VPCs attached to the Internet gateway.
igAttachments :: Lens' InternetGateway ([InternetGatewayAttachment])
igAttachments = lens _igAttachments (\s a -> s { _igAttachments = a })
{-# INLINE igAttachments #-}

-- | Any tags assigned to the Internet gateway.
igTags :: Lens' InternetGateway ([Tag])
igTags = lens _igTags (\s a -> s { _igTags = a })
{-# INLINE igTags #-}

instance FromXML InternetGateway where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "internetGateway"

-- | Describes the attachment of a VPC to an Internet gateway.
data InternetGatewayAttachment = InternetGatewayAttachment
    { _igaVpcId :: Text
      -- ^ The ID of the VPC.
    , _igaState :: AttachmentStatus
      -- ^ The current state of the attachment.
    } deriving (Show, Generic)

-- | The ID of the VPC.
igaVpcId :: Lens' InternetGatewayAttachment (Text)
igaVpcId = lens _igaVpcId (\s a -> s { _igaVpcId = a })
{-# INLINE igaVpcId #-}

-- | The current state of the attachment.
igaState :: Lens' InternetGatewayAttachment (AttachmentStatus)
igaState = lens _igaState (\s a -> s { _igaState = a })
{-# INLINE igaState #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InternetGatewayAttachment' data type to populate a request.
mkInternetGatewayAttachment :: Text -- ^ 'igaVpcId'
                            -> AttachmentStatus -- ^ 'igaState'
                            -> InternetGatewayAttachment
mkInternetGatewayAttachment p1 p2 = InternetGatewayAttachment
    { _igaVpcId = p1
    , _igaState = p2
    }
{-# INLINE mkInternetGatewayAttachment #-}

instance FromXML InternetGatewayAttachment where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery InternetGatewayAttachment where
    toQuery = genericQuery def

-- | Describes a security group rule.
data IpPermission = IpPermission
    { _ipIpProtocol :: Text
      -- ^ The protocol. When you call DescribeSecurityGroups, the protocol
      -- value returned is the number. Exception: For TCP, UDP, and ICMP,
      -- the value returned is the name (for example, tcp, udp, or icmp).
      -- For a list of protocol numbers, see Protocol Numbers.
    , _ipFromPort :: Integer
      -- ^ The start of port range for the TCP and UDP protocols, or an ICMP
      -- type number. A value of -1 indicates all ICMP types.
    , _ipToPort :: Integer
      -- ^ The end of port range for the TCP and UDP protocols, or an ICMP
      -- code. A value of -1 indicates all ICMP codes for the specified
      -- ICMP type.
    , _ipUserIdGroupPairs :: [UserIdGroupPair]
      -- ^ One or more security group and AWS account ID pairs.
    , _ipIpRanges :: [IpRange]
      -- ^ One or more IP ranges.
    } deriving (Show, Generic)

-- | The protocol. When you call DescribeSecurityGroups, the protocol value
-- returned is the number. Exception: For TCP, UDP, and ICMP, the value
-- returned is the name (for example, tcp, udp, or icmp). For a list of
-- protocol numbers, see Protocol Numbers.
ipIpProtocol :: Lens' IpPermission (Text)
ipIpProtocol = lens _ipIpProtocol (\s a -> s { _ipIpProtocol = a })
{-# INLINE ipIpProtocol #-}

-- | The start of port range for the TCP and UDP protocols, or an ICMP type
-- number. A value of -1 indicates all ICMP types.
ipFromPort :: Lens' IpPermission (Integer)
ipFromPort = lens _ipFromPort (\s a -> s { _ipFromPort = a })
{-# INLINE ipFromPort #-}

-- | The end of port range for the TCP and UDP protocols, or an ICMP code. A
-- value of -1 indicates all ICMP codes for the specified ICMP type.
ipToPort :: Lens' IpPermission (Integer)
ipToPort = lens _ipToPort (\s a -> s { _ipToPort = a })
{-# INLINE ipToPort #-}

-- | One or more security group and AWS account ID pairs.
ipUserIdGroupPairs :: Lens' IpPermission ([UserIdGroupPair])
ipUserIdGroupPairs = lens _ipUserIdGroupPairs (\s a -> s { _ipUserIdGroupPairs = a })
{-# INLINE ipUserIdGroupPairs #-}

-- | One or more IP ranges.
ipIpRanges :: Lens' IpPermission ([IpRange])
ipIpRanges = lens _ipIpRanges (\s a -> s { _ipIpRanges = a })
{-# INLINE ipIpRanges #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'IpPermission' data type to populate a request.
mkIpPermission :: Text -- ^ 'ipIpProtocol'
               -> Integer -- ^ 'ipFromPort'
               -> Integer -- ^ 'ipToPort'
               -> IpPermission
mkIpPermission p1 p2 p3 = IpPermission
    { _ipIpProtocol = p1
    , _ipFromPort = p2
    , _ipToPort = p3
    , _ipUserIdGroupPairs = mempty
    , _ipIpRanges = mempty
    }
{-# INLINE mkIpPermission #-}

instance FromXML IpPermission where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "IpPermission"

instance ToQuery IpPermission where
    toQuery = genericQuery def

-- | Describes a key pair.
data KeyPairInfo = KeyPairInfo
    { _kpiKeyName :: Maybe Text
      -- ^ The name of the key pair.
    , _kpiKeyFingerprint :: Maybe Text
      -- ^ If you used CreateKeyPair to create the key pair, this is the
      -- SHA-1 digest of the DER encoded private key. If you used
      -- ImportKeyPair to provide AWS the public key, this is the MD5
      -- public key fingerprint as specified in section 4 of RFC4716.
    } deriving (Show, Generic)

-- | The name of the key pair.
kpiKeyName :: Lens' KeyPairInfo (Maybe Text)
kpiKeyName = lens _kpiKeyName (\s a -> s { _kpiKeyName = a })
{-# INLINE kpiKeyName #-}

-- | If you used CreateKeyPair to create the key pair, this is the SHA-1 digest
-- of the DER encoded private key. If you used ImportKeyPair to provide AWS
-- the public key, this is the MD5 public key fingerprint as specified in
-- section 4 of RFC4716.
kpiKeyFingerprint :: Lens' KeyPairInfo (Maybe Text)
kpiKeyFingerprint = lens _kpiKeyFingerprint (\s a -> s { _kpiKeyFingerprint = a })
{-# INLINE kpiKeyFingerprint #-}

instance FromXML KeyPairInfo where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes a launch permission.
data LaunchPermission = LaunchPermission
    { _lpUserId :: Maybe Text
      -- ^ The AWS account ID.
    , _lpGroup :: Maybe PermissionGroup
      -- ^ The name of the group.
    } deriving (Show, Generic)

-- | The AWS account ID.
lpUserId :: Lens' LaunchPermission (Maybe Text)
lpUserId = lens _lpUserId (\s a -> s { _lpUserId = a })
{-# INLINE lpUserId #-}

-- | The name of the group.
lpGroup :: Lens' LaunchPermission (Maybe PermissionGroup)
lpGroup = lens _lpGroup (\s a -> s { _lpGroup = a })
{-# INLINE lpGroup #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'LaunchPermission' data type to populate a request.
mkLaunchPermission :: LaunchPermission
mkLaunchPermission = LaunchPermission
    { _lpUserId = Nothing
    , _lpGroup = Nothing
    }
{-# INLINE mkLaunchPermission #-}

instance FromXML LaunchPermission where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery LaunchPermission where
    toQuery = genericQuery def

-- | 
data LaunchPermissionModifications = LaunchPermissionModifications
    { _lpmAdd :: [LaunchPermission]
      -- ^ The AWS account ID to add to the list of launch permissions for
      -- the AMI.
    , _lpmRemove :: [LaunchPermission]
      -- ^ The AWS account ID to remove from the list of launch permissions
      -- for the AMI.
    } deriving (Show, Generic)

-- | The AWS account ID to add to the list of launch permissions for the AMI.
lpmAdd :: Lens' LaunchPermissionModifications ([LaunchPermission])
lpmAdd = lens _lpmAdd (\s a -> s { _lpmAdd = a })
{-# INLINE lpmAdd #-}

-- | The AWS account ID to remove from the list of launch permissions for the
-- AMI.
lpmRemove :: Lens' LaunchPermissionModifications ([LaunchPermission])
lpmRemove = lens _lpmRemove (\s a -> s { _lpmRemove = a })
{-# INLINE lpmRemove #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'LaunchPermissionModifications' data type to populate a request.
mkLaunchPermissionModifications :: LaunchPermissionModifications
mkLaunchPermissionModifications = LaunchPermissionModifications
    { _lpmAdd = mempty
    , _lpmRemove = mempty
    }
{-# INLINE mkLaunchPermissionModifications #-}

instance ToQuery LaunchPermissionModifications where
    toQuery = genericQuery def

-- | Additional information for launching instances.
data LaunchSpecification = LaunchSpecification
    { _llnImageId :: Maybe Text
      -- ^ The ID of the AMI.
    , _llnKeyName :: Maybe Text
      -- ^ The name of the key pair.
    , _llnSecurityGroups :: [GroupIdentifier]
      -- ^ One or more security groups.
    , _llnUserData :: Maybe Text
      -- ^ The Base64-encoded MIME user data to make available to the
      -- instances.
    , _llnAddressingType :: Maybe Text
      -- ^ 
    , _llnInstanceType :: Maybe InstanceType
      -- ^ The instance type.
    , _llnPlacement :: Maybe SpotPlacement
      -- ^ The placement information for the instance.
    , _llnKernelId :: Maybe Text
      -- ^ The ID of the kernel.
    , _llnRamdiskId :: Maybe Text
      -- ^ The ID of the RAM disk.
    , _llnBlockDeviceMappings :: [BlockDeviceMapping]
      -- ^ One or more block device mapping entries.
    , _llnMonitoringEnabled :: Maybe Bool
      -- ^ Enables monitoring for the instance. Default: Disabled.
    , _llnSubnetId :: Maybe Text
      -- ^ The ID of the subnet in which to launch the Spot Instance.
    , _llnNetworkInterfaces :: [InstanceNetworkInterfaceSpecification]
      -- ^ One or more network interfaces.
    , _llnIamInstanceProfile :: Maybe IamInstanceProfileSpecification
      -- ^ The IAM instance profile.
    , _llnEbsOptimized :: Maybe Bool
      -- ^ Indicates whether the instance is optimized for EBS I/O. This
      -- optimization provides dedicated throughput to Amazon EBS and an
      -- optimized configuration stack to provide optimal EBS I/O
      -- performance. This optimization isn't available with all instance
      -- types. Additional usage charges apply when using an EBS Optimized
      -- instance. Default: false.
    } deriving (Show, Generic)

-- | The ID of the AMI.
llnImageId :: Lens' LaunchSpecification (Maybe Text)
llnImageId = lens _llnImageId (\s a -> s { _llnImageId = a })
{-# INLINE llnImageId #-}

-- | The name of the key pair.
llnKeyName :: Lens' LaunchSpecification (Maybe Text)
llnKeyName = lens _llnKeyName (\s a -> s { _llnKeyName = a })
{-# INLINE llnKeyName #-}

-- | One or more security groups.
llnSecurityGroups :: Lens' LaunchSpecification ([GroupIdentifier])
llnSecurityGroups = lens _llnSecurityGroups (\s a -> s { _llnSecurityGroups = a })
{-# INLINE llnSecurityGroups #-}

-- | The Base64-encoded MIME user data to make available to the instances.
llnUserData :: Lens' LaunchSpecification (Maybe Text)
llnUserData = lens _llnUserData (\s a -> s { _llnUserData = a })
{-# INLINE llnUserData #-}

-- | 
llnAddressingType :: Lens' LaunchSpecification (Maybe Text)
llnAddressingType = lens _llnAddressingType (\s a -> s { _llnAddressingType = a })
{-# INLINE llnAddressingType #-}

-- | The instance type.
llnInstanceType :: Lens' LaunchSpecification (Maybe InstanceType)
llnInstanceType = lens _llnInstanceType (\s a -> s { _llnInstanceType = a })
{-# INLINE llnInstanceType #-}

-- | The placement information for the instance.
llnPlacement :: Lens' LaunchSpecification (Maybe SpotPlacement)
llnPlacement = lens _llnPlacement (\s a -> s { _llnPlacement = a })
{-# INLINE llnPlacement #-}

-- | The ID of the kernel.
llnKernelId :: Lens' LaunchSpecification (Maybe Text)
llnKernelId = lens _llnKernelId (\s a -> s { _llnKernelId = a })
{-# INLINE llnKernelId #-}

-- | The ID of the RAM disk.
llnRamdiskId :: Lens' LaunchSpecification (Maybe Text)
llnRamdiskId = lens _llnRamdiskId (\s a -> s { _llnRamdiskId = a })
{-# INLINE llnRamdiskId #-}

-- | One or more block device mapping entries.
llnBlockDeviceMappings :: Lens' LaunchSpecification ([BlockDeviceMapping])
llnBlockDeviceMappings = lens _llnBlockDeviceMappings (\s a -> s { _llnBlockDeviceMappings = a })
{-# INLINE llnBlockDeviceMappings #-}

-- | Enables monitoring for the instance. Default: Disabled.
llnMonitoringEnabled :: Lens' LaunchSpecification (Maybe Bool)
llnMonitoringEnabled = lens _llnMonitoringEnabled (\s a -> s { _llnMonitoringEnabled = a })
{-# INLINE llnMonitoringEnabled #-}

-- | The ID of the subnet in which to launch the Spot Instance.
llnSubnetId :: Lens' LaunchSpecification (Maybe Text)
llnSubnetId = lens _llnSubnetId (\s a -> s { _llnSubnetId = a })
{-# INLINE llnSubnetId #-}

-- | One or more network interfaces.
llnNetworkInterfaces :: Lens' LaunchSpecification ([InstanceNetworkInterfaceSpecification])
llnNetworkInterfaces = lens _llnNetworkInterfaces (\s a -> s { _llnNetworkInterfaces = a })
{-# INLINE llnNetworkInterfaces #-}

-- | The IAM instance profile.
llnIamInstanceProfile :: Lens' LaunchSpecification (Maybe IamInstanceProfileSpecification)
llnIamInstanceProfile = lens _llnIamInstanceProfile (\s a -> s { _llnIamInstanceProfile = a })
{-# INLINE llnIamInstanceProfile #-}

-- | Indicates whether the instance is optimized for EBS I/O. This optimization
-- provides dedicated throughput to Amazon EBS and an optimized configuration
-- stack to provide optimal EBS I/O performance. This optimization isn't
-- available with all instance types. Additional usage charges apply when
-- using an EBS Optimized instance. Default: false.
llnEbsOptimized :: Lens' LaunchSpecification (Maybe Bool)
llnEbsOptimized = lens _llnEbsOptimized (\s a -> s { _llnEbsOptimized = a })
{-# INLINE llnEbsOptimized #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'LaunchSpecification' data type to populate a request.
mkLaunchSpecification :: LaunchSpecification
mkLaunchSpecification = LaunchSpecification
    { _llnImageId = Nothing
    , _llnKeyName = Nothing
    , _llnSecurityGroups = mempty
    , _llnUserData = Nothing
    , _llnAddressingType = Nothing
    , _llnInstanceType = Nothing
    , _llnPlacement = Nothing
    , _llnKernelId = Nothing
    , _llnRamdiskId = Nothing
    , _llnBlockDeviceMappings = mempty
    , _llnMonitoringEnabled = Nothing
    , _llnSubnetId = Nothing
    , _llnNetworkInterfaces = mempty
    , _llnIamInstanceProfile = Nothing
    , _llnEbsOptimized = Nothing
    }
{-# INLINE mkLaunchSpecification #-}

instance FromXML LaunchSpecification where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "launchSpecification"

instance ToQuery LaunchSpecification where
    toQuery = genericQuery def

-- | Information about the network ACL.
data NetworkAcl = NetworkAcl
    { _naNetworkAclId :: Maybe Text
      -- ^ The ID of the network ACL.
    , _naVpcId :: Maybe Text
      -- ^ The ID of the VPC for the network ACL.
    , _naIsDefault :: Maybe Bool
      -- ^ Indicates whether this is the default network ACL for the VPC.
    , _naEntries :: [NetworkAclEntry]
      -- ^ One or more entries (rules) in the network ACL.
    , _naAssociations :: [NetworkAclAssociation]
      -- ^ Any associations between the network ACL and one or more subnets.
    , _naTags :: [Tag]
      -- ^ Any tags assigned to the network ACL.
    } deriving (Show, Generic)

-- | The ID of the network ACL.
naNetworkAclId :: Lens' NetworkAcl (Maybe Text)
naNetworkAclId = lens _naNetworkAclId (\s a -> s { _naNetworkAclId = a })
{-# INLINE naNetworkAclId #-}

-- | The ID of the VPC for the network ACL.
naVpcId :: Lens' NetworkAcl (Maybe Text)
naVpcId = lens _naVpcId (\s a -> s { _naVpcId = a })
{-# INLINE naVpcId #-}

-- | Indicates whether this is the default network ACL for the VPC.
naIsDefault :: Lens' NetworkAcl (Maybe Bool)
naIsDefault = lens _naIsDefault (\s a -> s { _naIsDefault = a })
{-# INLINE naIsDefault #-}

-- | One or more entries (rules) in the network ACL.
naEntries :: Lens' NetworkAcl ([NetworkAclEntry])
naEntries = lens _naEntries (\s a -> s { _naEntries = a })
{-# INLINE naEntries #-}

-- | Any associations between the network ACL and one or more subnets.
naAssociations :: Lens' NetworkAcl ([NetworkAclAssociation])
naAssociations = lens _naAssociations (\s a -> s { _naAssociations = a })
{-# INLINE naAssociations #-}

-- | Any tags assigned to the network ACL.
naTags :: Lens' NetworkAcl ([Tag])
naTags = lens _naTags (\s a -> s { _naTags = a })
{-# INLINE naTags #-}

instance FromXML NetworkAcl where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "networkAcl"

-- | Describes an association between a network ACL and a subnet.
data NetworkAclAssociation = NetworkAclAssociation
    { _naaNetworkAclAssociationId :: Maybe Text
      -- ^ The ID of the association between a network ACL and a subnet.
    , _naaNetworkAclId :: Maybe Text
      -- ^ The ID of the network ACL.
    , _naaSubnetId :: Maybe Text
      -- ^ The ID of the subnet.
    } deriving (Show, Generic)

-- | The ID of the association between a network ACL and a subnet.
naaNetworkAclAssociationId :: Lens' NetworkAclAssociation (Maybe Text)
naaNetworkAclAssociationId = lens _naaNetworkAclAssociationId (\s a -> s { _naaNetworkAclAssociationId = a })
{-# INLINE naaNetworkAclAssociationId #-}

-- | The ID of the network ACL.
naaNetworkAclId :: Lens' NetworkAclAssociation (Maybe Text)
naaNetworkAclId = lens _naaNetworkAclId (\s a -> s { _naaNetworkAclId = a })
{-# INLINE naaNetworkAclId #-}

-- | The ID of the subnet.
naaSubnetId :: Lens' NetworkAclAssociation (Maybe Text)
naaSubnetId = lens _naaSubnetId (\s a -> s { _naaSubnetId = a })
{-# INLINE naaSubnetId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'NetworkAclAssociation' data type to populate a request.
mkNetworkAclAssociation :: NetworkAclAssociation
mkNetworkAclAssociation = NetworkAclAssociation
    { _naaNetworkAclAssociationId = Nothing
    , _naaNetworkAclId = Nothing
    , _naaSubnetId = Nothing
    }
{-# INLINE mkNetworkAclAssociation #-}

instance FromXML NetworkAclAssociation where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery NetworkAclAssociation where
    toQuery = genericQuery def

-- | Describes an entry in a network ACL.
data NetworkAclEntry = NetworkAclEntry
    { _naeRuleNumber :: Maybe Integer
      -- ^ The rule number for the entry. ACL entries are processed in
      -- ascending order by rule number.
    , _naeProtocol :: Maybe Text
      -- ^ The protocol. A value of -1 means all protocols.
    , _naeRuleAction :: Maybe RuleAction
      -- ^ Indicates whether to allow or deny the traffic that matches the
      -- rule.
    , _naeEgress :: Maybe Bool
      -- ^ Indicates whether the rule is an egress rule (applied to traffic
      -- leaving the subnet).
    , _naeCidrBlock :: Maybe Text
      -- ^ The network range to allow or deny, in CIDR notation.
    , _naeIcmpTypeCode :: Maybe IcmpTypeCode
      -- ^ ICMP protocol: The ICMP type and code.
    , _naePortRange :: Maybe PortRange
      -- ^ TCP or UDP protocols: The range of ports the rule applies to.
    } deriving (Show, Generic)

-- | The rule number for the entry. ACL entries are processed in ascending order
-- by rule number.
naeRuleNumber :: Lens' NetworkAclEntry (Maybe Integer)
naeRuleNumber = lens _naeRuleNumber (\s a -> s { _naeRuleNumber = a })
{-# INLINE naeRuleNumber #-}

-- | The protocol. A value of -1 means all protocols.
naeProtocol :: Lens' NetworkAclEntry (Maybe Text)
naeProtocol = lens _naeProtocol (\s a -> s { _naeProtocol = a })
{-# INLINE naeProtocol #-}

-- | Indicates whether to allow or deny the traffic that matches the rule.
naeRuleAction :: Lens' NetworkAclEntry (Maybe RuleAction)
naeRuleAction = lens _naeRuleAction (\s a -> s { _naeRuleAction = a })
{-# INLINE naeRuleAction #-}

-- | Indicates whether the rule is an egress rule (applied to traffic leaving
-- the subnet).
naeEgress :: Lens' NetworkAclEntry (Maybe Bool)
naeEgress = lens _naeEgress (\s a -> s { _naeEgress = a })
{-# INLINE naeEgress #-}

-- | The network range to allow or deny, in CIDR notation.
naeCidrBlock :: Lens' NetworkAclEntry (Maybe Text)
naeCidrBlock = lens _naeCidrBlock (\s a -> s { _naeCidrBlock = a })
{-# INLINE naeCidrBlock #-}

-- | ICMP protocol: The ICMP type and code.
naeIcmpTypeCode :: Lens' NetworkAclEntry (Maybe IcmpTypeCode)
naeIcmpTypeCode = lens _naeIcmpTypeCode (\s a -> s { _naeIcmpTypeCode = a })
{-# INLINE naeIcmpTypeCode #-}

-- | TCP or UDP protocols: The range of ports the rule applies to.
naePortRange :: Lens' NetworkAclEntry (Maybe PortRange)
naePortRange = lens _naePortRange (\s a -> s { _naePortRange = a })
{-# INLINE naePortRange #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'NetworkAclEntry' data type to populate a request.
mkNetworkAclEntry :: NetworkAclEntry
mkNetworkAclEntry = NetworkAclEntry
    { _naeRuleNumber = Nothing
    , _naeProtocol = Nothing
    , _naeRuleAction = Nothing
    , _naeEgress = Nothing
    , _naeCidrBlock = Nothing
    , _naeIcmpTypeCode = Nothing
    , _naePortRange = Nothing
    }
{-# INLINE mkNetworkAclEntry #-}

instance FromXML NetworkAclEntry where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery NetworkAclEntry where
    toQuery = genericQuery def

-- | Information about the network interface.
data NetworkInterface = NetworkInterface
    { _niNetworkInterfaceId :: Maybe Text
      -- ^ The ID of the network interface.
    , _niSubnetId :: Maybe Text
      -- ^ The ID of the subnet.
    , _niVpcId :: Maybe Text
      -- ^ The ID of the VPC.
    , _niAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone.
    , _niDescription :: Maybe Text
      -- ^ A description.
    , _niOwnerId :: Maybe Text
      -- ^ The AWS account ID of the owner of the network interface.
    , _niRequesterId :: Maybe Text
      -- ^ The ID of the entity that launched the instance on your behalf
      -- (for example, AWS Management Console or Auto Scaling).
    , _niRequesterManaged :: Maybe Bool
      -- ^ Indicates whether the network interface is being managed by AWS.
    , _niStatus :: Maybe NetworkInterfaceStatus
      -- ^ The status of the network interface.
    , _niMacAddress :: Maybe Text
      -- ^ The MAC address.
    , _niPrivateIpAddress :: Maybe Text
      -- ^ The IP address of the network interface within the subnet.
    , _niPrivateDnsName :: Maybe Text
      -- ^ The private DNS name.
    , _niSourceDestCheck :: Maybe Bool
      -- ^ Indicates whether traffic to or from the instance is validated.
    , _niGroups :: [GroupIdentifier]
      -- ^ Any security groups for the network interface.
    , _niAttachment :: Maybe NetworkInterfaceAttachment
      -- ^ The network interface attachment.
    , _niAssociation :: Maybe NetworkInterfaceAssociation
      -- ^ The association information for an Elastic IP associated with the
      -- network interface.
    , _niTagSet :: [Tag]
      -- ^ Any tags assigned to the network interface.
    , _niPrivateIpAddresses :: [NetworkInterfacePrivateIpAddress]
      -- ^ The private IP addresses associated with the network interface.
    } deriving (Show, Generic)

-- | The ID of the network interface.
niNetworkInterfaceId :: Lens' NetworkInterface (Maybe Text)
niNetworkInterfaceId = lens _niNetworkInterfaceId (\s a -> s { _niNetworkInterfaceId = a })
{-# INLINE niNetworkInterfaceId #-}

-- | The ID of the subnet.
niSubnetId :: Lens' NetworkInterface (Maybe Text)
niSubnetId = lens _niSubnetId (\s a -> s { _niSubnetId = a })
{-# INLINE niSubnetId #-}

-- | The ID of the VPC.
niVpcId :: Lens' NetworkInterface (Maybe Text)
niVpcId = lens _niVpcId (\s a -> s { _niVpcId = a })
{-# INLINE niVpcId #-}

-- | The Availability Zone.
niAvailabilityZone :: Lens' NetworkInterface (Maybe Text)
niAvailabilityZone = lens _niAvailabilityZone (\s a -> s { _niAvailabilityZone = a })
{-# INLINE niAvailabilityZone #-}

-- | A description.
niDescription :: Lens' NetworkInterface (Maybe Text)
niDescription = lens _niDescription (\s a -> s { _niDescription = a })
{-# INLINE niDescription #-}

-- | The AWS account ID of the owner of the network interface.
niOwnerId :: Lens' NetworkInterface (Maybe Text)
niOwnerId = lens _niOwnerId (\s a -> s { _niOwnerId = a })
{-# INLINE niOwnerId #-}

-- | The ID of the entity that launched the instance on your behalf (for
-- example, AWS Management Console or Auto Scaling).
niRequesterId :: Lens' NetworkInterface (Maybe Text)
niRequesterId = lens _niRequesterId (\s a -> s { _niRequesterId = a })
{-# INLINE niRequesterId #-}

-- | Indicates whether the network interface is being managed by AWS.
niRequesterManaged :: Lens' NetworkInterface (Maybe Bool)
niRequesterManaged = lens _niRequesterManaged (\s a -> s { _niRequesterManaged = a })
{-# INLINE niRequesterManaged #-}

-- | The status of the network interface.
niStatus :: Lens' NetworkInterface (Maybe NetworkInterfaceStatus)
niStatus = lens _niStatus (\s a -> s { _niStatus = a })
{-# INLINE niStatus #-}

-- | The MAC address.
niMacAddress :: Lens' NetworkInterface (Maybe Text)
niMacAddress = lens _niMacAddress (\s a -> s { _niMacAddress = a })
{-# INLINE niMacAddress #-}

-- | The IP address of the network interface within the subnet.
niPrivateIpAddress :: Lens' NetworkInterface (Maybe Text)
niPrivateIpAddress = lens _niPrivateIpAddress (\s a -> s { _niPrivateIpAddress = a })
{-# INLINE niPrivateIpAddress #-}

-- | The private DNS name.
niPrivateDnsName :: Lens' NetworkInterface (Maybe Text)
niPrivateDnsName = lens _niPrivateDnsName (\s a -> s { _niPrivateDnsName = a })
{-# INLINE niPrivateDnsName #-}

-- | Indicates whether traffic to or from the instance is validated.
niSourceDestCheck :: Lens' NetworkInterface (Maybe Bool)
niSourceDestCheck = lens _niSourceDestCheck (\s a -> s { _niSourceDestCheck = a })
{-# INLINE niSourceDestCheck #-}

-- | Any security groups for the network interface.
niGroups :: Lens' NetworkInterface ([GroupIdentifier])
niGroups = lens _niGroups (\s a -> s { _niGroups = a })
{-# INLINE niGroups #-}

-- | The network interface attachment.
niAttachment :: Lens' NetworkInterface (Maybe NetworkInterfaceAttachment)
niAttachment = lens _niAttachment (\s a -> s { _niAttachment = a })
{-# INLINE niAttachment #-}

-- | The association information for an Elastic IP associated with the network
-- interface.
niAssociation :: Lens' NetworkInterface (Maybe NetworkInterfaceAssociation)
niAssociation = lens _niAssociation (\s a -> s { _niAssociation = a })
{-# INLINE niAssociation #-}

-- | Any tags assigned to the network interface.
niTagSet :: Lens' NetworkInterface ([Tag])
niTagSet = lens _niTagSet (\s a -> s { _niTagSet = a })
{-# INLINE niTagSet #-}

-- | The private IP addresses associated with the network interface.
niPrivateIpAddresses :: Lens' NetworkInterface ([NetworkInterfacePrivateIpAddress])
niPrivateIpAddresses = lens _niPrivateIpAddresses (\s a -> s { _niPrivateIpAddresses = a })
{-# INLINE niPrivateIpAddresses #-}

instance FromXML NetworkInterface where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "networkInterface"

-- | The association information for an Elastic IP associated with the network
-- interface.
data NetworkInterfaceAssociation = NetworkInterfaceAssociation
    { _nibPublicIp :: Maybe Text
      -- ^ The address of the Elastic IP address bound to the network
      -- interface.
    , _nibPublicDnsName :: Maybe Text
      -- ^ The public DNS name.
    , _nibIpOwnerId :: Maybe Text
      -- ^ The ID of the Elastic IP address owner.
    , _nibAllocationId :: Maybe Text
      -- ^ The allocation ID.
    , _nibAssociationId :: Maybe Text
      -- ^ The association ID.
    } deriving (Show, Generic)

-- | The address of the Elastic IP address bound to the network interface.
nibPublicIp :: Lens' NetworkInterfaceAssociation (Maybe Text)
nibPublicIp = lens _nibPublicIp (\s a -> s { _nibPublicIp = a })
{-# INLINE nibPublicIp #-}

-- | The public DNS name.
nibPublicDnsName :: Lens' NetworkInterfaceAssociation (Maybe Text)
nibPublicDnsName = lens _nibPublicDnsName (\s a -> s { _nibPublicDnsName = a })
{-# INLINE nibPublicDnsName #-}

-- | The ID of the Elastic IP address owner.
nibIpOwnerId :: Lens' NetworkInterfaceAssociation (Maybe Text)
nibIpOwnerId = lens _nibIpOwnerId (\s a -> s { _nibIpOwnerId = a })
{-# INLINE nibIpOwnerId #-}

-- | The allocation ID.
nibAllocationId :: Lens' NetworkInterfaceAssociation (Maybe Text)
nibAllocationId = lens _nibAllocationId (\s a -> s { _nibAllocationId = a })
{-# INLINE nibAllocationId #-}

-- | The association ID.
nibAssociationId :: Lens' NetworkInterfaceAssociation (Maybe Text)
nibAssociationId = lens _nibAssociationId (\s a -> s { _nibAssociationId = a })
{-# INLINE nibAssociationId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'NetworkInterfaceAssociation' data type to populate a request.
mkNetworkInterfaceAssociation :: NetworkInterfaceAssociation
mkNetworkInterfaceAssociation = NetworkInterfaceAssociation
    { _nibPublicIp = Nothing
    , _nibPublicDnsName = Nothing
    , _nibIpOwnerId = Nothing
    , _nibAllocationId = Nothing
    , _nibAssociationId = Nothing
    }
{-# INLINE mkNetworkInterfaceAssociation #-}

instance FromXML NetworkInterfaceAssociation where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "association"

instance ToQuery NetworkInterfaceAssociation where
    toQuery = genericQuery def

-- | The network interface attachment.
data NetworkInterfaceAttachment = NetworkInterfaceAttachment
    { _niaAttachmentId :: Maybe Text
      -- ^ The ID of the network interface attachment.
    , _niaInstanceId :: Maybe Text
      -- ^ The ID of the instance.
    , _niaInstanceOwnerId :: Maybe Text
      -- ^ The AWS account ID of the owner of the instance.
    , _niaDeviceIndex :: Maybe Integer
      -- ^ The device index of the network interface attachment on the
      -- instance.
    , _niaStatus :: Maybe AttachmentStatus
      -- ^ The attachment state.
    , _niaAttachTime :: Maybe ISO8601
      -- ^ The timestamp indicating when the attachment initiated.
    , _niaDeleteOnTermination :: Maybe Bool
      -- ^ Indicates whether the network interface is deleted when the
      -- instance is terminated.
    } deriving (Show, Generic)

-- | The ID of the network interface attachment.
niaAttachmentId :: Lens' NetworkInterfaceAttachment (Maybe Text)
niaAttachmentId = lens _niaAttachmentId (\s a -> s { _niaAttachmentId = a })
{-# INLINE niaAttachmentId #-}

-- | The ID of the instance.
niaInstanceId :: Lens' NetworkInterfaceAttachment (Maybe Text)
niaInstanceId = lens _niaInstanceId (\s a -> s { _niaInstanceId = a })
{-# INLINE niaInstanceId #-}

-- | The AWS account ID of the owner of the instance.
niaInstanceOwnerId :: Lens' NetworkInterfaceAttachment (Maybe Text)
niaInstanceOwnerId = lens _niaInstanceOwnerId (\s a -> s { _niaInstanceOwnerId = a })
{-# INLINE niaInstanceOwnerId #-}

-- | The device index of the network interface attachment on the instance.
niaDeviceIndex :: Lens' NetworkInterfaceAttachment (Maybe Integer)
niaDeviceIndex = lens _niaDeviceIndex (\s a -> s { _niaDeviceIndex = a })
{-# INLINE niaDeviceIndex #-}

-- | The attachment state.
niaStatus :: Lens' NetworkInterfaceAttachment (Maybe AttachmentStatus)
niaStatus = lens _niaStatus (\s a -> s { _niaStatus = a })
{-# INLINE niaStatus #-}

-- | The timestamp indicating when the attachment initiated.
niaAttachTime :: Lens' NetworkInterfaceAttachment (Maybe ISO8601)
niaAttachTime = lens _niaAttachTime (\s a -> s { _niaAttachTime = a })
{-# INLINE niaAttachTime #-}

-- | Indicates whether the network interface is deleted when the instance is
-- terminated.
niaDeleteOnTermination :: Lens' NetworkInterfaceAttachment (Maybe Bool)
niaDeleteOnTermination = lens _niaDeleteOnTermination (\s a -> s { _niaDeleteOnTermination = a })
{-# INLINE niaDeleteOnTermination #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'NetworkInterfaceAttachment' data type to populate a request.
mkNetworkInterfaceAttachment :: NetworkInterfaceAttachment
mkNetworkInterfaceAttachment = NetworkInterfaceAttachment
    { _niaAttachmentId = Nothing
    , _niaInstanceId = Nothing
    , _niaInstanceOwnerId = Nothing
    , _niaDeviceIndex = Nothing
    , _niaStatus = Nothing
    , _niaAttachTime = Nothing
    , _niaDeleteOnTermination = Nothing
    }
{-# INLINE mkNetworkInterfaceAttachment #-}

instance FromXML NetworkInterfaceAttachment where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "attachment"

instance ToQuery NetworkInterfaceAttachment where
    toQuery = genericQuery def

-- | The ID of the interface attachment.
data NetworkInterfaceAttachmentChanges = NetworkInterfaceAttachmentChanges
    { _niacAttachmentId :: Maybe Text
      -- ^ The ID of the network interface attachment.
    , _niacDeleteOnTermination :: Maybe Bool
      -- ^ Indicates whether the network interface is deleted when the
      -- instance is terminated.
    } deriving (Show, Generic)

-- | The ID of the network interface attachment.
niacAttachmentId :: Lens' NetworkInterfaceAttachmentChanges (Maybe Text)
niacAttachmentId = lens _niacAttachmentId (\s a -> s { _niacAttachmentId = a })
{-# INLINE niacAttachmentId #-}

-- | Indicates whether the network interface is deleted when the instance is
-- terminated.
niacDeleteOnTermination :: Lens' NetworkInterfaceAttachmentChanges (Maybe Bool)
niacDeleteOnTermination = lens _niacDeleteOnTermination (\s a -> s { _niacDeleteOnTermination = a })
{-# INLINE niacDeleteOnTermination #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'NetworkInterfaceAttachmentChanges' data type to populate a request.
mkNetworkInterfaceAttachmentChanges :: NetworkInterfaceAttachmentChanges
mkNetworkInterfaceAttachmentChanges = NetworkInterfaceAttachmentChanges
    { _niacAttachmentId = Nothing
    , _niacDeleteOnTermination = Nothing
    }
{-# INLINE mkNetworkInterfaceAttachmentChanges #-}

instance ToQuery NetworkInterfaceAttachmentChanges where
    toQuery = genericQuery def

-- | Describes the private IP address of a network interface.
data NetworkInterfacePrivateIpAddress = NetworkInterfacePrivateIpAddress
    { _nipiaPrivateIpAddress :: Maybe Text
      -- ^ The private IP address.
    , _nipiaPrivateDnsName :: Maybe Text
      -- ^ The private DNS name.
    , _nipiaPrimary :: Maybe Bool
      -- ^ Indicates whether this IP address is the primary private IP
      -- address of the network interface.
    , _nipiaAssociation :: Maybe NetworkInterfaceAssociation
      -- ^ The association information for an Elastic IP address associated
      -- with the network interface.
    } deriving (Show, Generic)

-- | The private IP address.
nipiaPrivateIpAddress :: Lens' NetworkInterfacePrivateIpAddress (Maybe Text)
nipiaPrivateIpAddress = lens _nipiaPrivateIpAddress (\s a -> s { _nipiaPrivateIpAddress = a })
{-# INLINE nipiaPrivateIpAddress #-}

-- | The private DNS name.
nipiaPrivateDnsName :: Lens' NetworkInterfacePrivateIpAddress (Maybe Text)
nipiaPrivateDnsName = lens _nipiaPrivateDnsName (\s a -> s { _nipiaPrivateDnsName = a })
{-# INLINE nipiaPrivateDnsName #-}

-- | Indicates whether this IP address is the primary private IP address of the
-- network interface.
nipiaPrimary :: Lens' NetworkInterfacePrivateIpAddress (Maybe Bool)
nipiaPrimary = lens _nipiaPrimary (\s a -> s { _nipiaPrimary = a })
{-# INLINE nipiaPrimary #-}

-- | The association information for an Elastic IP address associated with the
-- network interface.
nipiaAssociation :: Lens' NetworkInterfacePrivateIpAddress (Maybe NetworkInterfaceAssociation)
nipiaAssociation = lens _nipiaAssociation (\s a -> s { _nipiaAssociation = a })
{-# INLINE nipiaAssociation #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'NetworkInterfacePrivateIpAddress' data type to populate a request.
mkNetworkInterfacePrivateIpAddress :: NetworkInterfacePrivateIpAddress
mkNetworkInterfacePrivateIpAddress = NetworkInterfacePrivateIpAddress
    { _nipiaPrivateIpAddress = Nothing
    , _nipiaPrivateDnsName = Nothing
    , _nipiaPrimary = Nothing
    , _nipiaAssociation = Nothing
    }
{-# INLINE mkNetworkInterfacePrivateIpAddress #-}

instance FromXML NetworkInterfacePrivateIpAddress where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery NetworkInterfacePrivateIpAddress where
    toQuery = genericQuery def

-- | The location where the instance launched.
data Placement = Placement
    { _pzAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone of the instance.
    , _pzGroupName :: Maybe Text
      -- ^ The name of the placement group the instance is in (for cluster
      -- compute instances).
    , _pzTenancy :: Maybe Tenancy
      -- ^ The tenancy of the instance (if the instance is running in a
      -- VPC). An instance with a tenancy of dedicated runs on
      -- single-tenant hardware.
    } deriving (Show, Generic)

-- | The Availability Zone of the instance.
pzAvailabilityZone :: Lens' Placement (Maybe Text)
pzAvailabilityZone = lens _pzAvailabilityZone (\s a -> s { _pzAvailabilityZone = a })
{-# INLINE pzAvailabilityZone #-}

-- | The name of the placement group the instance is in (for cluster compute
-- instances).
pzGroupName :: Lens' Placement (Maybe Text)
pzGroupName = lens _pzGroupName (\s a -> s { _pzGroupName = a })
{-# INLINE pzGroupName #-}

-- | The tenancy of the instance (if the instance is running in a VPC). An
-- instance with a tenancy of dedicated runs on single-tenant hardware.
pzTenancy :: Lens' Placement (Maybe Tenancy)
pzTenancy = lens _pzTenancy (\s a -> s { _pzTenancy = a })
{-# INLINE pzTenancy #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Placement' data type to populate a request.
mkPlacement :: Placement
mkPlacement = Placement
    { _pzAvailabilityZone = Nothing
    , _pzGroupName = Nothing
    , _pzTenancy = Nothing
    }
{-# INLINE mkPlacement #-}

instance FromXML Placement where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "placement"

instance ToQuery Placement where
    toQuery = genericQuery def

-- | Describes a placement group.
data PlacementGroup = PlacementGroup
    { _phGroupName :: Maybe Text
      -- ^ The name of the placement group.
    , _phStrategy :: Maybe PlacementStrategy
      -- ^ The placement strategy.
    , _phState :: Maybe PlacementGroupState
      -- ^ The state of the placement group.
    } deriving (Show, Generic)

-- | The name of the placement group.
phGroupName :: Lens' PlacementGroup (Maybe Text)
phGroupName = lens _phGroupName (\s a -> s { _phGroupName = a })
{-# INLINE phGroupName #-}

-- | The placement strategy.
phStrategy :: Lens' PlacementGroup (Maybe PlacementStrategy)
phStrategy = lens _phStrategy (\s a -> s { _phStrategy = a })
{-# INLINE phStrategy #-}

-- | The state of the placement group.
phState :: Lens' PlacementGroup (Maybe PlacementGroupState)
phState = lens _phState (\s a -> s { _phState = a })
{-# INLINE phState #-}

instance FromXML PlacementGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | TCP or UDP protocols: The range of ports the rule applies to.
data PortRange = PortRange
    { _prFrom :: Maybe Integer
      -- ^ The first port in the range.
    , _prTo :: Maybe Integer
      -- ^ The last port in the range.
    } deriving (Show, Generic)

-- | The first port in the range.
prFrom :: Lens' PortRange (Maybe Integer)
prFrom = lens _prFrom (\s a -> s { _prFrom = a })
{-# INLINE prFrom #-}

-- | The last port in the range.
prTo :: Lens' PortRange (Maybe Integer)
prTo = lens _prTo (\s a -> s { _prTo = a })
{-# INLINE prTo #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'PortRange' data type to populate a request.
mkPortRange :: PortRange
mkPortRange = PortRange
    { _prFrom = Nothing
    , _prTo = Nothing
    }
{-# INLINE mkPortRange #-}

instance FromXML PortRange where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "portRange"

instance ToQuery PortRange where
    toQuery = genericQuery def

-- | Describes the price for a Reserved Instance.
data PriceSchedule = PriceSchedule
    { _psTerm :: Maybe Integer
      -- ^ The number of months remaining in the reservation. For example, 2
      -- is the second to the last month before the capacity reservation
      -- expires.
    , _psPrice :: Maybe Double
      -- ^ The fixed price for the term.
    , _psCurrencyCode :: Maybe CurrencyCodeValues
      -- ^ The currency for transacting the Reserved Instance resale. At
      -- this time, the only supported currency is USD.
    , _psActive :: Maybe Bool
      -- ^ The current price schedule, as determined by the term remaining
      -- for the Reserved Instance in the listing. A specific price
      -- schedule is always in effect, but only one price schedule can be
      -- active at any time. Take, for example, a Reserved Instance
      -- listing that has five months remaining in its term. When you
      -- specify price schedules for five months and two months, this
      -- means that schedule 1, covering the first three months of the
      -- remaining term, will be active during months 5, 4, and 3. Then
      -- schedule 2, covering the last two months of the term, will be
      -- active for months 2 and 1.
    } deriving (Show, Generic)

-- | The number of months remaining in the reservation. For example, 2 is the
-- second to the last month before the capacity reservation expires.
psTerm :: Lens' PriceSchedule (Maybe Integer)
psTerm = lens _psTerm (\s a -> s { _psTerm = a })
{-# INLINE psTerm #-}

-- | The fixed price for the term.
psPrice :: Lens' PriceSchedule (Maybe Double)
psPrice = lens _psPrice (\s a -> s { _psPrice = a })
{-# INLINE psPrice #-}

-- | The currency for transacting the Reserved Instance resale. At this time,
-- the only supported currency is USD.
psCurrencyCode :: Lens' PriceSchedule (Maybe CurrencyCodeValues)
psCurrencyCode = lens _psCurrencyCode (\s a -> s { _psCurrencyCode = a })
{-# INLINE psCurrencyCode #-}

-- | The current price schedule, as determined by the term remaining for the
-- Reserved Instance in the listing. A specific price schedule is always in
-- effect, but only one price schedule can be active at any time. Take, for
-- example, a Reserved Instance listing that has five months remaining in its
-- term. When you specify price schedules for five months and two months, this
-- means that schedule 1, covering the first three months of the remaining
-- term, will be active during months 5, 4, and 3. Then schedule 2, covering
-- the last two months of the term, will be active for months 2 and 1.
psActive :: Lens' PriceSchedule (Maybe Bool)
psActive = lens _psActive (\s a -> s { _psActive = a })
{-# INLINE psActive #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'PriceSchedule' data type to populate a request.
mkPriceSchedule :: PriceSchedule
mkPriceSchedule = PriceSchedule
    { _psTerm = Nothing
    , _psPrice = Nothing
    , _psCurrencyCode = Nothing
    , _psActive = Nothing
    }
{-# INLINE mkPriceSchedule #-}

instance FromXML PriceSchedule where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery PriceSchedule where
    toQuery = genericQuery def

-- | Describes the price for a Reserved Instance.
data PriceScheduleSpecification = PriceScheduleSpecification
    { _pssTerm :: Maybe Integer
      -- ^ The number of months remaining in the reservation. For example, 2
      -- is the second to the last month before the capacity reservation
      -- expires.
    , _pssPrice :: Maybe Double
      -- ^ The fixed price for the term.
    , _pssCurrencyCode :: Maybe CurrencyCodeValues
      -- ^ The currency for transacting the Reserved Instance resale. At
      -- this time, the only supported currency is USD.
    } deriving (Show, Generic)

-- | The number of months remaining in the reservation. For example, 2 is the
-- second to the last month before the capacity reservation expires.
pssTerm :: Lens' PriceScheduleSpecification (Maybe Integer)
pssTerm = lens _pssTerm (\s a -> s { _pssTerm = a })
{-# INLINE pssTerm #-}

-- | The fixed price for the term.
pssPrice :: Lens' PriceScheduleSpecification (Maybe Double)
pssPrice = lens _pssPrice (\s a -> s { _pssPrice = a })
{-# INLINE pssPrice #-}

-- | The currency for transacting the Reserved Instance resale. At this time,
-- the only supported currency is USD.
pssCurrencyCode :: Lens' PriceScheduleSpecification (Maybe CurrencyCodeValues)
pssCurrencyCode = lens _pssCurrencyCode (\s a -> s { _pssCurrencyCode = a })
{-# INLINE pssCurrencyCode #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'PriceScheduleSpecification' data type to populate a request.
mkPriceScheduleSpecification :: PriceScheduleSpecification
mkPriceScheduleSpecification = PriceScheduleSpecification
    { _pssTerm = Nothing
    , _pssPrice = Nothing
    , _pssCurrencyCode = Nothing
    }
{-# INLINE mkPriceScheduleSpecification #-}

instance ToQuery PriceScheduleSpecification where
    toQuery = genericQuery def

-- | Describes a Reserved Instance offering.
data PricingDetail = PricingDetail
    { _piPrice :: Maybe Double
      -- ^ The price per instance.
    , _piCount :: Maybe Integer
      -- ^ The number of instances available for the price.
    } deriving (Show, Generic)

-- | The price per instance.
piPrice :: Lens' PricingDetail (Maybe Double)
piPrice = lens _piPrice (\s a -> s { _piPrice = a })
{-# INLINE piPrice #-}

-- | The number of instances available for the price.
piCount :: Lens' PricingDetail (Maybe Integer)
piCount = lens _piCount (\s a -> s { _piCount = a })
{-# INLINE piCount #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'PricingDetail' data type to populate a request.
mkPricingDetail :: PricingDetail
mkPricingDetail = PricingDetail
    { _piPrice = Nothing
    , _piCount = Nothing
    }
{-# INLINE mkPricingDetail #-}

instance FromXML PricingDetail where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery PricingDetail where
    toQuery = genericQuery def

-- | Describes a secondary private IP address for a network interface.
data PrivateIpAddressSpecification = PrivateIpAddressSpecification
    { _piasPrivateIpAddress :: Text
      -- ^ The private IP addresses.
    , _piasPrimary :: Maybe Bool
      -- ^ Indicates whether the private IP address is the primary private
      -- IP address.
    } deriving (Show, Generic)

-- | The private IP addresses.
piasPrivateIpAddress :: Lens' PrivateIpAddressSpecification (Text)
piasPrivateIpAddress = lens _piasPrivateIpAddress (\s a -> s { _piasPrivateIpAddress = a })
{-# INLINE piasPrivateIpAddress #-}

-- | Indicates whether the private IP address is the primary private IP address.
piasPrimary :: Lens' PrivateIpAddressSpecification (Maybe Bool)
piasPrimary = lens _piasPrimary (\s a -> s { _piasPrimary = a })
{-# INLINE piasPrimary #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'PrivateIpAddressSpecification' data type to populate a request.
mkPrivateIpAddressSpecification :: Text -- ^ 'piasPrivateIpAddress'
                                -> PrivateIpAddressSpecification
mkPrivateIpAddressSpecification p1 = PrivateIpAddressSpecification
    { _piasPrivateIpAddress = p1
    , _piasPrimary = Nothing
    }
{-# INLINE mkPrivateIpAddressSpecification #-}

instance FromXML PrivateIpAddressSpecification where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PrivateIpAddressSpecification"

instance ToQuery PrivateIpAddressSpecification where
    toQuery = genericQuery def

-- | Describes a product code.
data ProductCode = ProductCode
    { _pcProductCodeId :: Maybe Text
      -- ^ The product code.
    , _pcProductCodeType :: Maybe ProductCodeValues
      -- ^ The type of product code.
    } deriving (Show, Generic)

-- | The product code.
pcProductCodeId :: Lens' ProductCode (Maybe Text)
pcProductCodeId = lens _pcProductCodeId (\s a -> s { _pcProductCodeId = a })
{-# INLINE pcProductCodeId #-}

-- | The type of product code.
pcProductCodeType :: Lens' ProductCode (Maybe ProductCodeValues)
pcProductCodeType = lens _pcProductCodeType (\s a -> s { _pcProductCodeType = a })
{-# INLINE pcProductCodeType #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ProductCode' data type to populate a request.
mkProductCode :: ProductCode
mkProductCode = ProductCode
    { _pcProductCodeId = Nothing
    , _pcProductCodeType = Nothing
    }
{-# INLINE mkProductCode #-}

instance FromXML ProductCode where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery ProductCode where
    toQuery = genericQuery def

-- | Describes a recurring charge.
data RecurringCharge = RecurringCharge
    { _rdFrequency :: Maybe RecurringChargeFrequency
      -- ^ The frequency of the recurring charge.
    , _rdAmount :: Maybe Double
      -- ^ The amount of the recurring charge.
    } deriving (Show, Generic)

-- | The frequency of the recurring charge.
rdFrequency :: Lens' RecurringCharge (Maybe RecurringChargeFrequency)
rdFrequency = lens _rdFrequency (\s a -> s { _rdFrequency = a })
{-# INLINE rdFrequency #-}

-- | The amount of the recurring charge.
rdAmount :: Lens' RecurringCharge (Maybe Double)
rdAmount = lens _rdAmount (\s a -> s { _rdAmount = a })
{-# INLINE rdAmount #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'RecurringCharge' data type to populate a request.
mkRecurringCharge :: RecurringCharge
mkRecurringCharge = RecurringCharge
    { _rdFrequency = Nothing
    , _rdAmount = Nothing
    }
{-# INLINE mkRecurringCharge #-}

instance FromXML RecurringCharge where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery RecurringCharge where
    toQuery = genericQuery def

-- | Describes a region.
data Region = Region
    { _rqRegionName :: Maybe Text
      -- ^ The name of the region.
    , _rqEndpoint :: Maybe Text
      -- ^ The region service endpoint.
    } deriving (Show, Generic)

-- | The name of the region.
rqRegionName :: Lens' Region (Maybe Text)
rqRegionName = lens _rqRegionName (\s a -> s { _rqRegionName = a })
{-# INLINE rqRegionName #-}

-- | The region service endpoint.
rqEndpoint :: Lens' Region (Maybe Text)
rqEndpoint = lens _rqEndpoint (\s a -> s { _rqEndpoint = a })
{-# INLINE rqEndpoint #-}

instance FromXML Region where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes a reservation.
data Reservation = Reservation
    { _rnReservationId :: Maybe Text
      -- ^ The ID of the reservation.
    , _rnOwnerId :: Maybe Text
      -- ^ The ID of the AWS account that owns the reservation.
    , _rnRequesterId :: Maybe Text
      -- ^ The ID of the requester that launched the instances on your
      -- behalf (for example, AWS Management Console or Auto Scaling).
    , _rnGroups :: [GroupIdentifier]
      -- ^ One or more security groups.
    , _rnInstances :: [Instance]
      -- ^ One or more instances.
    } deriving (Show, Generic)

-- | The ID of the reservation.
rnReservationId :: Lens' Reservation (Maybe Text)
rnReservationId = lens _rnReservationId (\s a -> s { _rnReservationId = a })
{-# INLINE rnReservationId #-}

-- | The ID of the AWS account that owns the reservation.
rnOwnerId :: Lens' Reservation (Maybe Text)
rnOwnerId = lens _rnOwnerId (\s a -> s { _rnOwnerId = a })
{-# INLINE rnOwnerId #-}

-- | The ID of the requester that launched the instances on your behalf (for
-- example, AWS Management Console or Auto Scaling).
rnRequesterId :: Lens' Reservation (Maybe Text)
rnRequesterId = lens _rnRequesterId (\s a -> s { _rnRequesterId = a })
{-# INLINE rnRequesterId #-}

-- | One or more security groups.
rnGroups :: Lens' Reservation ([GroupIdentifier])
rnGroups = lens _rnGroups (\s a -> s { _rnGroups = a })
{-# INLINE rnGroups #-}

-- | One or more instances.
rnInstances :: Lens' Reservation ([Instance])
rnInstances = lens _rnInstances (\s a -> s { _rnInstances = a })
{-# INLINE rnInstances #-}

instance FromXML Reservation where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Specified for Reserved Instance Marketplace offerings to limit the total
-- order and ensure that the Reserved Instances are not purchased at
-- unexpected prices.
data ReservedInstanceLimitPrice = ReservedInstanceLimitPrice
    { _rilpAmount :: Maybe Double
      -- ^ Used for Reserved Instance Marketplace offerings. Specifies the
      -- limit price on the total order (instanceCount * price).
    , _rilpCurrencyCode :: Maybe CurrencyCodeValues
      -- ^ The currency in which the limitPrice amount is specified. At this
      -- time, the only supported currency is USD.
    } deriving (Show, Generic)

-- | Used for Reserved Instance Marketplace offerings. Specifies the limit price
-- on the total order (instanceCount * price).
rilpAmount :: Lens' ReservedInstanceLimitPrice (Maybe Double)
rilpAmount = lens _rilpAmount (\s a -> s { _rilpAmount = a })
{-# INLINE rilpAmount #-}

-- | The currency in which the limitPrice amount is specified. At this time, the
-- only supported currency is USD.
rilpCurrencyCode :: Lens' ReservedInstanceLimitPrice (Maybe CurrencyCodeValues)
rilpCurrencyCode = lens _rilpCurrencyCode (\s a -> s { _rilpCurrencyCode = a })
{-# INLINE rilpCurrencyCode #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ReservedInstanceLimitPrice' data type to populate a request.
mkReservedInstanceLimitPrice :: ReservedInstanceLimitPrice
mkReservedInstanceLimitPrice = ReservedInstanceLimitPrice
    { _rilpAmount = Nothing
    , _rilpCurrencyCode = Nothing
    }
{-# INLINE mkReservedInstanceLimitPrice #-}

instance ToQuery ReservedInstanceLimitPrice where
    toQuery = genericQuery def

-- | Describes a Reserved Instance.
data ReservedInstances = ReservedInstances
    { _riReservedInstancesId :: Maybe Text
      -- ^ The ID of the Reserved Instance.
    , _riInstanceType :: Maybe InstanceType
      -- ^ The instance type on which the Reserved Instance can be used.
    , _riAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone in which the Reserved Instance can be used.
    , _riStart :: Maybe ISO8601
      -- ^ The date and time the Reserved Instance started.
    , _riEnd :: Maybe ISO8601
      -- ^ The time when the Reserved Instance expires.
    , _riDuration :: Maybe Integer
      -- ^ The duration of the Reserved Instance, in seconds.
    , _riUsagePrice :: Maybe Double
      -- ^ The usage price of the Reserved Instance, per hour.
    , _riFixedPrice :: Maybe Double
      -- ^ The purchase price of the Reserved Instance.
    , _riInstanceCount :: Maybe Integer
      -- ^ The number of Reserved Instances purchased.
    , _riProductDescription :: Maybe RIProductDescription
      -- ^ The Reserved Instance description.
    , _riState :: Maybe ReservedInstanceState
      -- ^ The state of the Reserved Instance purchase.
    , _riTags :: [Tag]
      -- ^ Any tags assigned to the resource.
    , _riInstanceTenancy :: Maybe Tenancy
      -- ^ The tenancy of the reserved instance.
    , _riCurrencyCode :: Maybe CurrencyCodeValues
      -- ^ The currency of the Reserved Instance. It's specified using ISO
      -- 4217 standard currency codes. At this time, the only supported
      -- currency is USD.
    , _riOfferingType :: Maybe OfferingTypeValues
      -- ^ The Reserved Instance offering type.
    , _riRecurringCharges :: [RecurringCharge]
      -- ^ The recurring charge tag assigned to the resource.
    } deriving (Show, Generic)

-- | The ID of the Reserved Instance.
riReservedInstancesId :: Lens' ReservedInstances (Maybe Text)
riReservedInstancesId = lens _riReservedInstancesId (\s a -> s { _riReservedInstancesId = a })
{-# INLINE riReservedInstancesId #-}

-- | The instance type on which the Reserved Instance can be used.
riInstanceType :: Lens' ReservedInstances (Maybe InstanceType)
riInstanceType = lens _riInstanceType (\s a -> s { _riInstanceType = a })
{-# INLINE riInstanceType #-}

-- | The Availability Zone in which the Reserved Instance can be used.
riAvailabilityZone :: Lens' ReservedInstances (Maybe Text)
riAvailabilityZone = lens _riAvailabilityZone (\s a -> s { _riAvailabilityZone = a })
{-# INLINE riAvailabilityZone #-}

-- | The date and time the Reserved Instance started.
riStart :: Lens' ReservedInstances (Maybe ISO8601)
riStart = lens _riStart (\s a -> s { _riStart = a })
{-# INLINE riStart #-}

-- | The time when the Reserved Instance expires.
riEnd :: Lens' ReservedInstances (Maybe ISO8601)
riEnd = lens _riEnd (\s a -> s { _riEnd = a })
{-# INLINE riEnd #-}

-- | The duration of the Reserved Instance, in seconds.
riDuration :: Lens' ReservedInstances (Maybe Integer)
riDuration = lens _riDuration (\s a -> s { _riDuration = a })
{-# INLINE riDuration #-}

-- | The usage price of the Reserved Instance, per hour.
riUsagePrice :: Lens' ReservedInstances (Maybe Double)
riUsagePrice = lens _riUsagePrice (\s a -> s { _riUsagePrice = a })
{-# INLINE riUsagePrice #-}

-- | The purchase price of the Reserved Instance.
riFixedPrice :: Lens' ReservedInstances (Maybe Double)
riFixedPrice = lens _riFixedPrice (\s a -> s { _riFixedPrice = a })
{-# INLINE riFixedPrice #-}

-- | The number of Reserved Instances purchased.
riInstanceCount :: Lens' ReservedInstances (Maybe Integer)
riInstanceCount = lens _riInstanceCount (\s a -> s { _riInstanceCount = a })
{-# INLINE riInstanceCount #-}

-- | The Reserved Instance description.
riProductDescription :: Lens' ReservedInstances (Maybe RIProductDescription)
riProductDescription = lens _riProductDescription (\s a -> s { _riProductDescription = a })
{-# INLINE riProductDescription #-}

-- | The state of the Reserved Instance purchase.
riState :: Lens' ReservedInstances (Maybe ReservedInstanceState)
riState = lens _riState (\s a -> s { _riState = a })
{-# INLINE riState #-}

-- | Any tags assigned to the resource.
riTags :: Lens' ReservedInstances ([Tag])
riTags = lens _riTags (\s a -> s { _riTags = a })
{-# INLINE riTags #-}

-- | The tenancy of the reserved instance.
riInstanceTenancy :: Lens' ReservedInstances (Maybe Tenancy)
riInstanceTenancy = lens _riInstanceTenancy (\s a -> s { _riInstanceTenancy = a })
{-# INLINE riInstanceTenancy #-}

-- | The currency of the Reserved Instance. It's specified using ISO 4217
-- standard currency codes. At this time, the only supported currency is USD.
riCurrencyCode :: Lens' ReservedInstances (Maybe CurrencyCodeValues)
riCurrencyCode = lens _riCurrencyCode (\s a -> s { _riCurrencyCode = a })
{-# INLINE riCurrencyCode #-}

-- | The Reserved Instance offering type.
riOfferingType :: Lens' ReservedInstances (Maybe OfferingTypeValues)
riOfferingType = lens _riOfferingType (\s a -> s { _riOfferingType = a })
{-# INLINE riOfferingType #-}

-- | The recurring charge tag assigned to the resource.
riRecurringCharges :: Lens' ReservedInstances ([RecurringCharge])
riRecurringCharges = lens _riRecurringCharges (\s a -> s { _riRecurringCharges = a })
{-# INLINE riRecurringCharges #-}

instance FromXML ReservedInstances where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | The target Reserved Instances configurations supplied as part of the
-- modification request.
data ReservedInstancesConfiguration = ReservedInstancesConfiguration
    { _ricAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone for the modified Reserved Instances.
    , _ricPlatform :: Maybe Text
      -- ^ The network platform of the modified Reserved Instances, which is
      -- either EC2-Classic or EC2-VPC.
    , _ricInstanceCount :: Maybe Integer
      -- ^ The number of modified Reserved Instances.
    , _ricInstanceType :: Maybe InstanceType
      -- ^ The instance type for the modified Reserved Instances.
    } deriving (Show, Generic)

-- | The Availability Zone for the modified Reserved Instances.
ricAvailabilityZone :: Lens' ReservedInstancesConfiguration (Maybe Text)
ricAvailabilityZone = lens _ricAvailabilityZone (\s a -> s { _ricAvailabilityZone = a })
{-# INLINE ricAvailabilityZone #-}

-- | The network platform of the modified Reserved Instances, which is either
-- EC2-Classic or EC2-VPC.
ricPlatform :: Lens' ReservedInstancesConfiguration (Maybe Text)
ricPlatform = lens _ricPlatform (\s a -> s { _ricPlatform = a })
{-# INLINE ricPlatform #-}

-- | The number of modified Reserved Instances.
ricInstanceCount :: Lens' ReservedInstancesConfiguration (Maybe Integer)
ricInstanceCount = lens _ricInstanceCount (\s a -> s { _ricInstanceCount = a })
{-# INLINE ricInstanceCount #-}

-- | The instance type for the modified Reserved Instances.
ricInstanceType :: Lens' ReservedInstancesConfiguration (Maybe InstanceType)
ricInstanceType = lens _ricInstanceType (\s a -> s { _ricInstanceType = a })
{-# INLINE ricInstanceType #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ReservedInstancesConfiguration' data type to populate a request.
mkReservedInstancesConfiguration :: ReservedInstancesConfiguration
mkReservedInstancesConfiguration = ReservedInstancesConfiguration
    { _ricAvailabilityZone = Nothing
    , _ricPlatform = Nothing
    , _ricInstanceCount = Nothing
    , _ricInstanceType = Nothing
    }
{-# INLINE mkReservedInstancesConfiguration #-}

instance FromXML ReservedInstancesConfiguration where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "targetConfiguration"

instance ToQuery ReservedInstancesConfiguration where
    toQuery = genericQuery def

-- | Describes a Reserved Instance listing.
data ReservedInstancesListing = ReservedInstancesListing
    { _rilReservedInstancesListingId :: Maybe Text
      -- ^ The ID of the Reserved Instance listing.
    , _rilReservedInstancesId :: Maybe Text
      -- ^ The ID of the Reserved Instance.
    , _rilCreateDate :: Maybe ISO8601
      -- ^ The time the listing was created.
    , _rilUpdateDate :: Maybe ISO8601
      -- ^ The last modified timestamp of the listing.
    , _rilStatus :: Maybe ListingStatus
      -- ^ The status of the Reserved Instance listing.
    , _rilStatusMessage :: Maybe Text
      -- ^ The reason for the current status of the Reserved Instance
      -- listing. The response can be blank.
    , _rilInstanceCounts :: [InstanceCount]
      -- ^ The number of instances in this state.
    , _rilPriceSchedules :: [PriceSchedule]
      -- ^ The price of the Reserved Instance listing.
    , _rilTags :: [Tag]
      -- ^ Any tags assigned to the resource.
    , _rilClientToken :: Maybe Text
      -- ^ The idempotency token you provided when you created the listing.
    } deriving (Show, Generic)

-- | The ID of the Reserved Instance listing.
rilReservedInstancesListingId :: Lens' ReservedInstancesListing (Maybe Text)
rilReservedInstancesListingId = lens _rilReservedInstancesListingId (\s a -> s { _rilReservedInstancesListingId = a })
{-# INLINE rilReservedInstancesListingId #-}

-- | The ID of the Reserved Instance.
rilReservedInstancesId :: Lens' ReservedInstancesListing (Maybe Text)
rilReservedInstancesId = lens _rilReservedInstancesId (\s a -> s { _rilReservedInstancesId = a })
{-# INLINE rilReservedInstancesId #-}

-- | The time the listing was created.
rilCreateDate :: Lens' ReservedInstancesListing (Maybe ISO8601)
rilCreateDate = lens _rilCreateDate (\s a -> s { _rilCreateDate = a })
{-# INLINE rilCreateDate #-}

-- | The last modified timestamp of the listing.
rilUpdateDate :: Lens' ReservedInstancesListing (Maybe ISO8601)
rilUpdateDate = lens _rilUpdateDate (\s a -> s { _rilUpdateDate = a })
{-# INLINE rilUpdateDate #-}

-- | The status of the Reserved Instance listing.
rilStatus :: Lens' ReservedInstancesListing (Maybe ListingStatus)
rilStatus = lens _rilStatus (\s a -> s { _rilStatus = a })
{-# INLINE rilStatus #-}

-- | The reason for the current status of the Reserved Instance listing. The
-- response can be blank.
rilStatusMessage :: Lens' ReservedInstancesListing (Maybe Text)
rilStatusMessage = lens _rilStatusMessage (\s a -> s { _rilStatusMessage = a })
{-# INLINE rilStatusMessage #-}

-- | The number of instances in this state.
rilInstanceCounts :: Lens' ReservedInstancesListing ([InstanceCount])
rilInstanceCounts = lens _rilInstanceCounts (\s a -> s { _rilInstanceCounts = a })
{-# INLINE rilInstanceCounts #-}

-- | The price of the Reserved Instance listing.
rilPriceSchedules :: Lens' ReservedInstancesListing ([PriceSchedule])
rilPriceSchedules = lens _rilPriceSchedules (\s a -> s { _rilPriceSchedules = a })
{-# INLINE rilPriceSchedules #-}

-- | Any tags assigned to the resource.
rilTags :: Lens' ReservedInstancesListing ([Tag])
rilTags = lens _rilTags (\s a -> s { _rilTags = a })
{-# INLINE rilTags #-}

-- | The idempotency token you provided when you created the listing.
rilClientToken :: Lens' ReservedInstancesListing (Maybe Text)
rilClientToken = lens _rilClientToken (\s a -> s { _rilClientToken = a })
{-# INLINE rilClientToken #-}

instance FromXML ReservedInstancesListing where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes a Reserved Instance modification.
data ReservedInstancesModification = ReservedInstancesModification
    { _rirReservedInstancesModificationId :: Maybe Text
      -- ^ A unique ID for the Reserved Instance modification.
    , _rirReservedInstancesIds :: [ReservedInstancesId]
      -- ^ The IDs of one or more Reserved Instances.
    , _rirModificationResults :: [ReservedInstancesModificationResult]
      -- ^ Contains target configurations along with their corresponding new
      -- Reserved Instance IDs.
    , _rirCreateDate :: Maybe ISO8601
      -- ^ The time when the modification request was created.
    , _rirUpdateDate :: Maybe ISO8601
      -- ^ The time when the modification request was last updated.
    , _rirEffectiveDate :: Maybe ISO8601
      -- ^ The time for the modification to become effective.
    , _rirStatus :: Maybe Text
      -- ^ The status of the Reserved Instances modification request.
    , _rirStatusMessage :: Maybe Text
      -- ^ The reason for the status.
    , _rirClientToken :: Maybe Text
      -- ^ A unique, case-sensitive key supplied by the client to ensure
      -- that the modification request is idempotent.
    } deriving (Show, Generic)

-- | A unique ID for the Reserved Instance modification.
rirReservedInstancesModificationId :: Lens' ReservedInstancesModification (Maybe Text)
rirReservedInstancesModificationId = lens _rirReservedInstancesModificationId (\s a -> s { _rirReservedInstancesModificationId = a })
{-# INLINE rirReservedInstancesModificationId #-}

-- | The IDs of one or more Reserved Instances.
rirReservedInstancesIds :: Lens' ReservedInstancesModification ([ReservedInstancesId])
rirReservedInstancesIds = lens _rirReservedInstancesIds (\s a -> s { _rirReservedInstancesIds = a })
{-# INLINE rirReservedInstancesIds #-}

-- | Contains target configurations along with their corresponding new Reserved
-- Instance IDs.
rirModificationResults :: Lens' ReservedInstancesModification ([ReservedInstancesModificationResult])
rirModificationResults = lens _rirModificationResults (\s a -> s { _rirModificationResults = a })
{-# INLINE rirModificationResults #-}

-- | The time when the modification request was created.
rirCreateDate :: Lens' ReservedInstancesModification (Maybe ISO8601)
rirCreateDate = lens _rirCreateDate (\s a -> s { _rirCreateDate = a })
{-# INLINE rirCreateDate #-}

-- | The time when the modification request was last updated.
rirUpdateDate :: Lens' ReservedInstancesModification (Maybe ISO8601)
rirUpdateDate = lens _rirUpdateDate (\s a -> s { _rirUpdateDate = a })
{-# INLINE rirUpdateDate #-}

-- | The time for the modification to become effective.
rirEffectiveDate :: Lens' ReservedInstancesModification (Maybe ISO8601)
rirEffectiveDate = lens _rirEffectiveDate (\s a -> s { _rirEffectiveDate = a })
{-# INLINE rirEffectiveDate #-}

-- | The status of the Reserved Instances modification request.
rirStatus :: Lens' ReservedInstancesModification (Maybe Text)
rirStatus = lens _rirStatus (\s a -> s { _rirStatus = a })
{-# INLINE rirStatus #-}

-- | The reason for the status.
rirStatusMessage :: Lens' ReservedInstancesModification (Maybe Text)
rirStatusMessage = lens _rirStatusMessage (\s a -> s { _rirStatusMessage = a })
{-# INLINE rirStatusMessage #-}

-- | A unique, case-sensitive key supplied by the client to ensure that the
-- modification request is idempotent.
rirClientToken :: Lens' ReservedInstancesModification (Maybe Text)
rirClientToken = lens _rirClientToken (\s a -> s { _rirClientToken = a })
{-# INLINE rirClientToken #-}

instance FromXML ReservedInstancesModification where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | 
data ReservedInstancesModificationResult = ReservedInstancesModificationResult
    { _rimrReservedInstancesId :: Maybe Text
      -- ^ The ID for the Reserved Instances that were created as part of
      -- the modification request. This field is only available when the
      -- modification is fulfilled.
    , _rimrTargetConfiguration :: Maybe ReservedInstancesConfiguration
      -- ^ The target Reserved Instances configurations supplied as part of
      -- the modification request.
    } deriving (Show, Generic)

-- | The ID for the Reserved Instances that were created as part of the
-- modification request. This field is only available when the modification is
-- fulfilled.
rimrReservedInstancesId :: Lens' ReservedInstancesModificationResult (Maybe Text)
rimrReservedInstancesId = lens _rimrReservedInstancesId (\s a -> s { _rimrReservedInstancesId = a })
{-# INLINE rimrReservedInstancesId #-}

-- | The target Reserved Instances configurations supplied as part of the
-- modification request.
rimrTargetConfiguration :: Lens' ReservedInstancesModificationResult (Maybe ReservedInstancesConfiguration)
rimrTargetConfiguration = lens _rimrTargetConfiguration (\s a -> s { _rimrTargetConfiguration = a })
{-# INLINE rimrTargetConfiguration #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ReservedInstancesModificationResult' data type to populate a request.
mkReservedInstancesModificationResult :: ReservedInstancesModificationResult
mkReservedInstancesModificationResult = ReservedInstancesModificationResult
    { _rimrReservedInstancesId = Nothing
    , _rimrTargetConfiguration = Nothing
    }
{-# INLINE mkReservedInstancesModificationResult #-}

instance FromXML ReservedInstancesModificationResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery ReservedInstancesModificationResult where
    toQuery = genericQuery def

-- | Describes a Reserved Instance offering.
data ReservedInstancesOffering = ReservedInstancesOffering
    { _ritReservedInstancesOfferingId :: Maybe Text
      -- ^ The ID of the Reserved Instance offering.
    , _ritInstanceType :: Maybe InstanceType
      -- ^ The instance type on which the Reserved Instance can be used.
    , _ritAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone in which the Reserved Instance can be used.
    , _ritDuration :: Maybe Integer
      -- ^ The duration of the Reserved Instance, in seconds.
    , _ritUsagePrice :: Maybe Double
      -- ^ The usage price of the Reserved Instance, per hour.
    , _ritFixedPrice :: Maybe Double
      -- ^ The purchase price of the Reserved Instance.
    , _ritProductDescription :: Maybe RIProductDescription
      -- ^ The Reserved Instance description.
    , _ritInstanceTenancy :: Maybe Tenancy
      -- ^ The tenancy of the reserved instance.
    , _ritCurrencyCode :: Maybe CurrencyCodeValues
      -- ^ The currency of the Reserved Instance offering you are
      -- purchasing. It's specified using ISO 4217 standard currency
      -- codes. At this time, the only supported currency is USD.
    , _ritOfferingType :: Maybe OfferingTypeValues
      -- ^ The Reserved Instance offering type.
    , _ritRecurringCharges :: [RecurringCharge]
      -- ^ The recurring charge tag assigned to the resource.
    , _ritMarketplace :: Maybe Bool
      -- ^ Indicates whether the offering is available through the Reserved
      -- Instance Marketplace (resale) or AWS. If it's a Reserved Instance
      -- Marketplace offering, this is true.
    , _ritPricingDetails :: [PricingDetail]
      -- ^ The pricing details of the Reserved Instance offering.
    } deriving (Show, Generic)

-- | The ID of the Reserved Instance offering.
ritReservedInstancesOfferingId :: Lens' ReservedInstancesOffering (Maybe Text)
ritReservedInstancesOfferingId = lens _ritReservedInstancesOfferingId (\s a -> s { _ritReservedInstancesOfferingId = a })
{-# INLINE ritReservedInstancesOfferingId #-}

-- | The instance type on which the Reserved Instance can be used.
ritInstanceType :: Lens' ReservedInstancesOffering (Maybe InstanceType)
ritInstanceType = lens _ritInstanceType (\s a -> s { _ritInstanceType = a })
{-# INLINE ritInstanceType #-}

-- | The Availability Zone in which the Reserved Instance can be used.
ritAvailabilityZone :: Lens' ReservedInstancesOffering (Maybe Text)
ritAvailabilityZone = lens _ritAvailabilityZone (\s a -> s { _ritAvailabilityZone = a })
{-# INLINE ritAvailabilityZone #-}

-- | The duration of the Reserved Instance, in seconds.
ritDuration :: Lens' ReservedInstancesOffering (Maybe Integer)
ritDuration = lens _ritDuration (\s a -> s { _ritDuration = a })
{-# INLINE ritDuration #-}

-- | The usage price of the Reserved Instance, per hour.
ritUsagePrice :: Lens' ReservedInstancesOffering (Maybe Double)
ritUsagePrice = lens _ritUsagePrice (\s a -> s { _ritUsagePrice = a })
{-# INLINE ritUsagePrice #-}

-- | The purchase price of the Reserved Instance.
ritFixedPrice :: Lens' ReservedInstancesOffering (Maybe Double)
ritFixedPrice = lens _ritFixedPrice (\s a -> s { _ritFixedPrice = a })
{-# INLINE ritFixedPrice #-}

-- | The Reserved Instance description.
ritProductDescription :: Lens' ReservedInstancesOffering (Maybe RIProductDescription)
ritProductDescription = lens _ritProductDescription (\s a -> s { _ritProductDescription = a })
{-# INLINE ritProductDescription #-}

-- | The tenancy of the reserved instance.
ritInstanceTenancy :: Lens' ReservedInstancesOffering (Maybe Tenancy)
ritInstanceTenancy = lens _ritInstanceTenancy (\s a -> s { _ritInstanceTenancy = a })
{-# INLINE ritInstanceTenancy #-}

-- | The currency of the Reserved Instance offering you are purchasing. It's
-- specified using ISO 4217 standard currency codes. At this time, the only
-- supported currency is USD.
ritCurrencyCode :: Lens' ReservedInstancesOffering (Maybe CurrencyCodeValues)
ritCurrencyCode = lens _ritCurrencyCode (\s a -> s { _ritCurrencyCode = a })
{-# INLINE ritCurrencyCode #-}

-- | The Reserved Instance offering type.
ritOfferingType :: Lens' ReservedInstancesOffering (Maybe OfferingTypeValues)
ritOfferingType = lens _ritOfferingType (\s a -> s { _ritOfferingType = a })
{-# INLINE ritOfferingType #-}

-- | The recurring charge tag assigned to the resource.
ritRecurringCharges :: Lens' ReservedInstancesOffering ([RecurringCharge])
ritRecurringCharges = lens _ritRecurringCharges (\s a -> s { _ritRecurringCharges = a })
{-# INLINE ritRecurringCharges #-}

-- | Indicates whether the offering is available through the Reserved Instance
-- Marketplace (resale) or AWS. If it's a Reserved Instance Marketplace
-- offering, this is true.
ritMarketplace :: Lens' ReservedInstancesOffering (Maybe Bool)
ritMarketplace = lens _ritMarketplace (\s a -> s { _ritMarketplace = a })
{-# INLINE ritMarketplace #-}

-- | The pricing details of the Reserved Instance offering.
ritPricingDetails :: Lens' ReservedInstancesOffering ([PricingDetail])
ritPricingDetails = lens _ritPricingDetails (\s a -> s { _ritPricingDetails = a })
{-# INLINE ritPricingDetails #-}

instance FromXML ReservedInstancesOffering where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes a route in a route table.
data Route = Route
    { _reDestinationCidrBlock :: Maybe Text
      -- ^ The CIDR block used for the destination match.
    , _reGatewayId :: Maybe Text
      -- ^ The ID of a gateway attached to your VPC.
    , _reInstanceId :: Maybe Text
      -- ^ The ID of a NAT instance in your VPC.
    , _reInstanceOwnerId :: Maybe Text
      -- ^ The AWS account ID of the owner of the instance.
    , _reNetworkInterfaceId :: Maybe Text
      -- ^ The ID of the network interface.
    , _reVpcPeeringConnectionId :: Maybe Text
      -- ^ The ID of the VPC peering connection.
    , _reState :: Maybe RouteState
      -- ^ The state of the route. The blackhole state indicates that the
      -- route's target isn't available (for example, the specified
      -- gateway isn't attached to the VPC, or the specified NAT instance
      -- has been terminated).
    , _reOrigin :: Maybe RouteOrigin
      -- ^ Describes how the route was created. CreateRouteTable indicates
      -- that route was automatically created when the route table was
      -- created. CreateRoute indicates that the route was manually added
      -- to the route table. EnableVgwRoutePropagation indicates that the
      -- route was propagated by route propagation.
    } deriving (Show, Generic)

-- | The CIDR block used for the destination match.
reDestinationCidrBlock :: Lens' Route (Maybe Text)
reDestinationCidrBlock = lens _reDestinationCidrBlock (\s a -> s { _reDestinationCidrBlock = a })
{-# INLINE reDestinationCidrBlock #-}

-- | The ID of a gateway attached to your VPC.
reGatewayId :: Lens' Route (Maybe Text)
reGatewayId = lens _reGatewayId (\s a -> s { _reGatewayId = a })
{-# INLINE reGatewayId #-}

-- | The ID of a NAT instance in your VPC.
reInstanceId :: Lens' Route (Maybe Text)
reInstanceId = lens _reInstanceId (\s a -> s { _reInstanceId = a })
{-# INLINE reInstanceId #-}

-- | The AWS account ID of the owner of the instance.
reInstanceOwnerId :: Lens' Route (Maybe Text)
reInstanceOwnerId = lens _reInstanceOwnerId (\s a -> s { _reInstanceOwnerId = a })
{-# INLINE reInstanceOwnerId #-}

-- | The ID of the network interface.
reNetworkInterfaceId :: Lens' Route (Maybe Text)
reNetworkInterfaceId = lens _reNetworkInterfaceId (\s a -> s { _reNetworkInterfaceId = a })
{-# INLINE reNetworkInterfaceId #-}

-- | The ID of the VPC peering connection.
reVpcPeeringConnectionId :: Lens' Route (Maybe Text)
reVpcPeeringConnectionId = lens _reVpcPeeringConnectionId (\s a -> s { _reVpcPeeringConnectionId = a })
{-# INLINE reVpcPeeringConnectionId #-}

-- | The state of the route. The blackhole state indicates that the route's
-- target isn't available (for example, the specified gateway isn't attached
-- to the VPC, or the specified NAT instance has been terminated).
reState :: Lens' Route (Maybe RouteState)
reState = lens _reState (\s a -> s { _reState = a })
{-# INLINE reState #-}

-- | Describes how the route was created. CreateRouteTable indicates that route
-- was automatically created when the route table was created. CreateRoute
-- indicates that the route was manually added to the route table.
-- EnableVgwRoutePropagation indicates that the route was propagated by route
-- propagation.
reOrigin :: Lens' Route (Maybe RouteOrigin)
reOrigin = lens _reOrigin (\s a -> s { _reOrigin = a })
{-# INLINE reOrigin #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Route' data type to populate a request.
mkRoute :: Route
mkRoute = Route
    { _reDestinationCidrBlock = Nothing
    , _reGatewayId = Nothing
    , _reInstanceId = Nothing
    , _reInstanceOwnerId = Nothing
    , _reNetworkInterfaceId = Nothing
    , _reVpcPeeringConnectionId = Nothing
    , _reState = Nothing
    , _reOrigin = Nothing
    }
{-# INLINE mkRoute #-}

instance FromXML Route where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery Route where
    toQuery = genericQuery def

-- | Information about the route table.
data RouteTable = RouteTable
    { _rtRouteTableId :: Maybe Text
      -- ^ The ID of the route table.
    , _rtVpcId :: Maybe Text
      -- ^ The ID of the VPC.
    , _rtRoutes :: [Route]
      -- ^ The routes in the route table.
    , _rtAssociations :: [RouteTableAssociation]
      -- ^ The associations between the route table and one or more subnets.
    , _rtTags :: [Tag]
      -- ^ Any tags assigned to the route table.
    , _rtPropagatingVgws :: [PropagatingVgw]
      -- ^ Any virtual private gateway (VGW) propagating routes.
    } deriving (Show, Generic)

-- | The ID of the route table.
rtRouteTableId :: Lens' RouteTable (Maybe Text)
rtRouteTableId = lens _rtRouteTableId (\s a -> s { _rtRouteTableId = a })
{-# INLINE rtRouteTableId #-}

-- | The ID of the VPC.
rtVpcId :: Lens' RouteTable (Maybe Text)
rtVpcId = lens _rtVpcId (\s a -> s { _rtVpcId = a })
{-# INLINE rtVpcId #-}

-- | The routes in the route table.
rtRoutes :: Lens' RouteTable ([Route])
rtRoutes = lens _rtRoutes (\s a -> s { _rtRoutes = a })
{-# INLINE rtRoutes #-}

-- | The associations between the route table and one or more subnets.
rtAssociations :: Lens' RouteTable ([RouteTableAssociation])
rtAssociations = lens _rtAssociations (\s a -> s { _rtAssociations = a })
{-# INLINE rtAssociations #-}

-- | Any tags assigned to the route table.
rtTags :: Lens' RouteTable ([Tag])
rtTags = lens _rtTags (\s a -> s { _rtTags = a })
{-# INLINE rtTags #-}

-- | Any virtual private gateway (VGW) propagating routes.
rtPropagatingVgws :: Lens' RouteTable ([PropagatingVgw])
rtPropagatingVgws = lens _rtPropagatingVgws (\s a -> s { _rtPropagatingVgws = a })
{-# INLINE rtPropagatingVgws #-}

instance FromXML RouteTable where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "routeTable"

-- | Describes an association between a route table and a subnet.
data RouteTableAssociation = RouteTableAssociation
    { _rtaRouteTableAssociationId :: Maybe Text
      -- ^ The ID of the association between a route table and a subnet.
    , _rtaRouteTableId :: Maybe Text
      -- ^ The ID of the route table.
    , _rtaSubnetId :: Maybe Text
      -- ^ The ID of the subnet.
    , _rtaMain :: Maybe Bool
      -- ^ Indicates whether this is the main route table.
    } deriving (Show, Generic)

-- | The ID of the association between a route table and a subnet.
rtaRouteTableAssociationId :: Lens' RouteTableAssociation (Maybe Text)
rtaRouteTableAssociationId = lens _rtaRouteTableAssociationId (\s a -> s { _rtaRouteTableAssociationId = a })
{-# INLINE rtaRouteTableAssociationId #-}

-- | The ID of the route table.
rtaRouteTableId :: Lens' RouteTableAssociation (Maybe Text)
rtaRouteTableId = lens _rtaRouteTableId (\s a -> s { _rtaRouteTableId = a })
{-# INLINE rtaRouteTableId #-}

-- | The ID of the subnet.
rtaSubnetId :: Lens' RouteTableAssociation (Maybe Text)
rtaSubnetId = lens _rtaSubnetId (\s a -> s { _rtaSubnetId = a })
{-# INLINE rtaSubnetId #-}

-- | Indicates whether this is the main route table.
rtaMain :: Lens' RouteTableAssociation (Maybe Bool)
rtaMain = lens _rtaMain (\s a -> s { _rtaMain = a })
{-# INLINE rtaMain #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'RouteTableAssociation' data type to populate a request.
mkRouteTableAssociation :: RouteTableAssociation
mkRouteTableAssociation = RouteTableAssociation
    { _rtaRouteTableAssociationId = Nothing
    , _rtaRouteTableId = Nothing
    , _rtaSubnetId = Nothing
    , _rtaMain = Nothing
    }
{-# INLINE mkRouteTableAssociation #-}

instance FromXML RouteTableAssociation where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery RouteTableAssociation where
    toQuery = genericQuery def

-- | An Amazon S3 storage location.
data S3Storage = S3Storage
    { _ssBucket :: Maybe Text
      -- ^ The bucket in which to store the AMI. You can specify a bucket
      -- that you already own or a new bucket that Amazon EC2 creates on
      -- your behalf. If you specify a bucket that belongs to someone
      -- else, Amazon EC2 returns an error.
    , _ssPrefix :: Maybe Text
      -- ^ The beginning of the file name of the AMI.
    , _ssAWSAccessKeyId :: Maybe Text
      -- ^ The access key ID of the owner of the bucket. Before you specify
      -- a value for your access key ID, review and follow the guidance in
      -- Best Practices for Managing AWS Access Keys.
    , _ssUploadPolicy :: Maybe ByteString
      -- ^ A Base64-encoded Amazon S3 upload policy that gives Amazon EC2
      -- permission to upload items into Amazon S3 on your behalf.
    , _ssUploadPolicySignature :: Maybe Text
      -- ^ The signature of the Base64 encoded JSON document.
    } deriving (Show, Generic)

-- | The bucket in which to store the AMI. You can specify a bucket that you
-- already own or a new bucket that Amazon EC2 creates on your behalf. If you
-- specify a bucket that belongs to someone else, Amazon EC2 returns an error.
ssBucket :: Lens' S3Storage (Maybe Text)
ssBucket = lens _ssBucket (\s a -> s { _ssBucket = a })
{-# INLINE ssBucket #-}

-- | The beginning of the file name of the AMI.
ssPrefix :: Lens' S3Storage (Maybe Text)
ssPrefix = lens _ssPrefix (\s a -> s { _ssPrefix = a })
{-# INLINE ssPrefix #-}

-- | The access key ID of the owner of the bucket. Before you specify a value
-- for your access key ID, review and follow the guidance in Best Practices
-- for Managing AWS Access Keys.
ssAWSAccessKeyId :: Lens' S3Storage (Maybe Text)
ssAWSAccessKeyId = lens _ssAWSAccessKeyId (\s a -> s { _ssAWSAccessKeyId = a })
{-# INLINE ssAWSAccessKeyId #-}

-- | A Base64-encoded Amazon S3 upload policy that gives Amazon EC2 permission
-- to upload items into Amazon S3 on your behalf.
ssUploadPolicy :: Lens' S3Storage (Maybe ByteString)
ssUploadPolicy = lens _ssUploadPolicy (\s a -> s { _ssUploadPolicy = a })
{-# INLINE ssUploadPolicy #-}

-- | The signature of the Base64 encoded JSON document.
ssUploadPolicySignature :: Lens' S3Storage (Maybe Text)
ssUploadPolicySignature = lens _ssUploadPolicySignature (\s a -> s { _ssUploadPolicySignature = a })
{-# INLINE ssUploadPolicySignature #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'S3Storage' data type to populate a request.
mkS3Storage :: S3Storage
mkS3Storage = S3Storage
    { _ssBucket = Nothing
    , _ssPrefix = Nothing
    , _ssAWSAccessKeyId = Nothing
    , _ssUploadPolicy = Nothing
    , _ssUploadPolicySignature = Nothing
    }
{-# INLINE mkS3Storage #-}

instance FromXML S3Storage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "S3Storage"

instance ToQuery S3Storage where
    toQuery = genericQuery def

-- | Describes a security group.
data SecurityGroup = SecurityGroup
    { _siOwnerId :: Text
      -- ^ The AWS account ID of the owner of the security group.
    , _siGroupName :: Text
      -- ^ The name of the security group.
    , _siGroupId :: Text
      -- ^ The ID of the security group.
    , _siDescription :: Text
      -- ^ A description of the security group.
    , _siIpPermissions :: [IpPermission]
      -- ^ One or more inbound rules associated with the security group.
    , _siIpPermissionsEgress :: [IpPermission]
      -- ^ [EC2-VPC] One or more outbound rules associated with the security
      -- group.
    , _siVpcId :: Maybe Text
      -- ^ [EC2-VPC] The ID of the VPC for the security group.
    , _siTags :: [Tag]
      -- ^ Any tags assigned to the security group.
    } deriving (Show, Generic)

-- | The AWS account ID of the owner of the security group.
siOwnerId :: Lens' SecurityGroup (Text)
siOwnerId = lens _siOwnerId (\s a -> s { _siOwnerId = a })
{-# INLINE siOwnerId #-}

-- | The name of the security group.
siGroupName :: Lens' SecurityGroup (Text)
siGroupName = lens _siGroupName (\s a -> s { _siGroupName = a })
{-# INLINE siGroupName #-}

-- | The ID of the security group.
siGroupId :: Lens' SecurityGroup (Text)
siGroupId = lens _siGroupId (\s a -> s { _siGroupId = a })
{-# INLINE siGroupId #-}

-- | A description of the security group.
siDescription :: Lens' SecurityGroup (Text)
siDescription = lens _siDescription (\s a -> s { _siDescription = a })
{-# INLINE siDescription #-}

-- | One or more inbound rules associated with the security group.
siIpPermissions :: Lens' SecurityGroup ([IpPermission])
siIpPermissions = lens _siIpPermissions (\s a -> s { _siIpPermissions = a })
{-# INLINE siIpPermissions #-}

-- | [EC2-VPC] One or more outbound rules associated with the security group.
siIpPermissionsEgress :: Lens' SecurityGroup ([IpPermission])
siIpPermissionsEgress = lens _siIpPermissionsEgress (\s a -> s { _siIpPermissionsEgress = a })
{-# INLINE siIpPermissionsEgress #-}

-- | [EC2-VPC] The ID of the VPC for the security group.
siVpcId :: Lens' SecurityGroup (Maybe Text)
siVpcId = lens _siVpcId (\s a -> s { _siVpcId = a })
{-# INLINE siVpcId #-}

-- | Any tags assigned to the security group.
siTags :: Lens' SecurityGroup ([Tag])
siTags = lens _siTags (\s a -> s { _siTags = a })
{-# INLINE siTags #-}

instance FromXML SecurityGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes a snapshot.
data Snapshot = Snapshot
    { _ssuSnapshotId :: Maybe Text
      -- ^ The ID of the snapshot.
    , _ssuVolumeId :: Maybe Text
      -- ^ The ID of the volume.
    , _ssuState :: Maybe SnapshotState
      -- ^ The snapshot state.
    , _ssuStartTime :: Maybe ISO8601
      -- ^ The time stamp when the snapshot was initiated.
    , _ssuProgress :: Maybe Text
      -- ^ The progress of the snapshot, as a percentage.
    , _ssuOwnerId :: Maybe Text
      -- ^ The AWS account ID of the Amazon EBS snapshot owner.
    , _ssuDescription :: Maybe Text
      -- ^ The description for the snapshot.
    , _ssuVolumeSize :: Maybe Integer
      -- ^ The size of the volume, in GiB.
    , _ssuOwnerAlias :: Maybe Text
      -- ^ The AWS account alias (for example, amazon, self) or AWS account
      -- ID that owns the snapshot.
    , _ssuTags :: [Tag]
      -- ^ Any tags assigned to the snapshot.
    , _ssuEncrypted :: Maybe Bool
      -- ^ Indicates whether the snapshot is encrypted.
    } deriving (Show, Generic)

-- | The ID of the snapshot.
ssuSnapshotId :: Lens' Snapshot (Maybe Text)
ssuSnapshotId = lens _ssuSnapshotId (\s a -> s { _ssuSnapshotId = a })
{-# INLINE ssuSnapshotId #-}

-- | The ID of the volume.
ssuVolumeId :: Lens' Snapshot (Maybe Text)
ssuVolumeId = lens _ssuVolumeId (\s a -> s { _ssuVolumeId = a })
{-# INLINE ssuVolumeId #-}

-- | The snapshot state.
ssuState :: Lens' Snapshot (Maybe SnapshotState)
ssuState = lens _ssuState (\s a -> s { _ssuState = a })
{-# INLINE ssuState #-}

-- | The time stamp when the snapshot was initiated.
ssuStartTime :: Lens' Snapshot (Maybe ISO8601)
ssuStartTime = lens _ssuStartTime (\s a -> s { _ssuStartTime = a })
{-# INLINE ssuStartTime #-}

-- | The progress of the snapshot, as a percentage.
ssuProgress :: Lens' Snapshot (Maybe Text)
ssuProgress = lens _ssuProgress (\s a -> s { _ssuProgress = a })
{-# INLINE ssuProgress #-}

-- | The AWS account ID of the Amazon EBS snapshot owner.
ssuOwnerId :: Lens' Snapshot (Maybe Text)
ssuOwnerId = lens _ssuOwnerId (\s a -> s { _ssuOwnerId = a })
{-# INLINE ssuOwnerId #-}

-- | The description for the snapshot.
ssuDescription :: Lens' Snapshot (Maybe Text)
ssuDescription = lens _ssuDescription (\s a -> s { _ssuDescription = a })
{-# INLINE ssuDescription #-}

-- | The size of the volume, in GiB.
ssuVolumeSize :: Lens' Snapshot (Maybe Integer)
ssuVolumeSize = lens _ssuVolumeSize (\s a -> s { _ssuVolumeSize = a })
{-# INLINE ssuVolumeSize #-}

-- | The AWS account alias (for example, amazon, self) or AWS account ID that
-- owns the snapshot.
ssuOwnerAlias :: Lens' Snapshot (Maybe Text)
ssuOwnerAlias = lens _ssuOwnerAlias (\s a -> s { _ssuOwnerAlias = a })
{-# INLINE ssuOwnerAlias #-}

-- | Any tags assigned to the snapshot.
ssuTags :: Lens' Snapshot ([Tag])
ssuTags = lens _ssuTags (\s a -> s { _ssuTags = a })
{-# INLINE ssuTags #-}

-- | Indicates whether the snapshot is encrypted.
ssuEncrypted :: Lens' Snapshot (Maybe Bool)
ssuEncrypted = lens _ssuEncrypted (\s a -> s { _ssuEncrypted = a })
{-# INLINE ssuEncrypted #-}

instance FromXML Snapshot where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | The Spot Instance datafeed subscription.
data SpotDatafeedSubscription = SpotDatafeedSubscription
    { _sdsOwnerId :: Maybe Text
      -- ^ The AWS account ID of the account.
    , _sdsBucket :: Maybe Text
      -- ^ The Amazon S3 bucket where the Spot Instance datafeed is located.
    , _sdsPrefix :: Maybe Text
      -- ^ The prefix that is prepended to datafeed files.
    , _sdsState :: Maybe DatafeedSubscriptionState
      -- ^ The state of the Spot Instance datafeed subscription.
    , _sdsFault :: Maybe SpotInstanceStateFault
      -- ^ The fault codes for the Spot Instance request, if any.
    } deriving (Show, Generic)

-- | The AWS account ID of the account.
sdsOwnerId :: Lens' SpotDatafeedSubscription (Maybe Text)
sdsOwnerId = lens _sdsOwnerId (\s a -> s { _sdsOwnerId = a })
{-# INLINE sdsOwnerId #-}

-- | The Amazon S3 bucket where the Spot Instance datafeed is located.
sdsBucket :: Lens' SpotDatafeedSubscription (Maybe Text)
sdsBucket = lens _sdsBucket (\s a -> s { _sdsBucket = a })
{-# INLINE sdsBucket #-}

-- | The prefix that is prepended to datafeed files.
sdsPrefix :: Lens' SpotDatafeedSubscription (Maybe Text)
sdsPrefix = lens _sdsPrefix (\s a -> s { _sdsPrefix = a })
{-# INLINE sdsPrefix #-}

-- | The state of the Spot Instance datafeed subscription.
sdsState :: Lens' SpotDatafeedSubscription (Maybe DatafeedSubscriptionState)
sdsState = lens _sdsState (\s a -> s { _sdsState = a })
{-# INLINE sdsState #-}

-- | The fault codes for the Spot Instance request, if any.
sdsFault :: Lens' SpotDatafeedSubscription (Maybe SpotInstanceStateFault)
sdsFault = lens _sdsFault (\s a -> s { _sdsFault = a })
{-# INLINE sdsFault #-}

instance FromXML SpotDatafeedSubscription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "spotDatafeedSubscription"

-- | Describe a Spot Instance request.
data SpotInstanceRequest = SpotInstanceRequest
    { _sirSpotInstanceRequestId :: Maybe Text
      -- ^ The ID of the Spot Instance request.
    , _sirSpotPrice :: Maybe Text
      -- ^ The maximum hourly price for any Spot Instance launched to
      -- fulfill the request.
    , _sirType :: Maybe SpotInstanceType
      -- ^ The Spot Instance request type.
    , _sirState :: Maybe SpotInstanceState
      -- ^ The state of the Spot Instance request. Spot bid status
      -- information can help you track your Spot Instance requests. For
      -- information, see Tracking Spot Requests with Bid Status Codes in
      -- the Amazon Elastic Compute Cloud User Guide.
    , _sirFault :: Maybe SpotInstanceStateFault
      -- ^ The fault codes for the Spot Instance request, if any.
    , _sirStatus :: Maybe SpotInstanceStatus
      -- ^ The status code and status message describing the Spot Instance
      -- request.
    , _sirValidFrom :: Maybe ISO8601
      -- ^ The start date of the request. If this is a one-time request, the
      -- request becomes active at this date and time and remains active
      -- until all instances launch, the request expires, or the request
      -- is canceled. If the request is persistent, the request becomes
      -- active at this date and time and remains active until it expires
      -- or is canceled.
    , _sirValidUntil :: Maybe ISO8601
      -- ^ The end date of the request. If this is a one-time request, the
      -- request remains active until all instances launch, the request is
      -- canceled, or this date is reached. If the request is persistent,
      -- it remains active until it is canceled or this date is reached.
    , _sirLaunchGroup :: Maybe Text
      -- ^ The instance launch group. Launch groups are Spot Instances that
      -- launch together and terminate together.
    , _sirAvailabilityZoneGroup :: Maybe Text
      -- ^ The Availability Zone group. If you specify the same Availability
      -- Zone group for all Spot Instance requests, all Spot Instances are
      -- launched in the same Availability Zone.
    , _sirLaunchSpecification :: Maybe LaunchSpecification
      -- ^ Additional information for launching instances.
    , _sirInstanceId :: Maybe Text
      -- ^ The instance ID, if an instance has been launched to fulfill the
      -- Spot Instance request.
    , _sirCreateTime :: Maybe ISO8601
      -- ^ The time stamp when the Spot Instance request was created.
    , _sirProductDescription :: Maybe RIProductDescription
      -- ^ The product description associated with the Spot Instance.
    , _sirTags :: [Tag]
      -- ^ Any tags assigned to the resource.
    , _sirLaunchedAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone in which the bid is launched.
    } deriving (Show, Generic)

-- | The ID of the Spot Instance request.
sirSpotInstanceRequestId :: Lens' SpotInstanceRequest (Maybe Text)
sirSpotInstanceRequestId = lens _sirSpotInstanceRequestId (\s a -> s { _sirSpotInstanceRequestId = a })
{-# INLINE sirSpotInstanceRequestId #-}

-- | The maximum hourly price for any Spot Instance launched to fulfill the
-- request.
sirSpotPrice :: Lens' SpotInstanceRequest (Maybe Text)
sirSpotPrice = lens _sirSpotPrice (\s a -> s { _sirSpotPrice = a })
{-# INLINE sirSpotPrice #-}

-- | The Spot Instance request type.
sirType :: Lens' SpotInstanceRequest (Maybe SpotInstanceType)
sirType = lens _sirType (\s a -> s { _sirType = a })
{-# INLINE sirType #-}

-- | The state of the Spot Instance request. Spot bid status information can
-- help you track your Spot Instance requests. For information, see Tracking
-- Spot Requests with Bid Status Codes in the Amazon Elastic Compute Cloud
-- User Guide.
sirState :: Lens' SpotInstanceRequest (Maybe SpotInstanceState)
sirState = lens _sirState (\s a -> s { _sirState = a })
{-# INLINE sirState #-}

-- | The fault codes for the Spot Instance request, if any.
sirFault :: Lens' SpotInstanceRequest (Maybe SpotInstanceStateFault)
sirFault = lens _sirFault (\s a -> s { _sirFault = a })
{-# INLINE sirFault #-}

-- | The status code and status message describing the Spot Instance request.
sirStatus :: Lens' SpotInstanceRequest (Maybe SpotInstanceStatus)
sirStatus = lens _sirStatus (\s a -> s { _sirStatus = a })
{-# INLINE sirStatus #-}

-- | The start date of the request. If this is a one-time request, the request
-- becomes active at this date and time and remains active until all instances
-- launch, the request expires, or the request is canceled. If the request is
-- persistent, the request becomes active at this date and time and remains
-- active until it expires or is canceled.
sirValidFrom :: Lens' SpotInstanceRequest (Maybe ISO8601)
sirValidFrom = lens _sirValidFrom (\s a -> s { _sirValidFrom = a })
{-# INLINE sirValidFrom #-}

-- | The end date of the request. If this is a one-time request, the request
-- remains active until all instances launch, the request is canceled, or this
-- date is reached. If the request is persistent, it remains active until it
-- is canceled or this date is reached.
sirValidUntil :: Lens' SpotInstanceRequest (Maybe ISO8601)
sirValidUntil = lens _sirValidUntil (\s a -> s { _sirValidUntil = a })
{-# INLINE sirValidUntil #-}

-- | The instance launch group. Launch groups are Spot Instances that launch
-- together and terminate together.
sirLaunchGroup :: Lens' SpotInstanceRequest (Maybe Text)
sirLaunchGroup = lens _sirLaunchGroup (\s a -> s { _sirLaunchGroup = a })
{-# INLINE sirLaunchGroup #-}

-- | The Availability Zone group. If you specify the same Availability Zone
-- group for all Spot Instance requests, all Spot Instances are launched in
-- the same Availability Zone.
sirAvailabilityZoneGroup :: Lens' SpotInstanceRequest (Maybe Text)
sirAvailabilityZoneGroup = lens _sirAvailabilityZoneGroup (\s a -> s { _sirAvailabilityZoneGroup = a })
{-# INLINE sirAvailabilityZoneGroup #-}

-- | Additional information for launching instances.
sirLaunchSpecification :: Lens' SpotInstanceRequest (Maybe LaunchSpecification)
sirLaunchSpecification = lens _sirLaunchSpecification (\s a -> s { _sirLaunchSpecification = a })
{-# INLINE sirLaunchSpecification #-}

-- | The instance ID, if an instance has been launched to fulfill the Spot
-- Instance request.
sirInstanceId :: Lens' SpotInstanceRequest (Maybe Text)
sirInstanceId = lens _sirInstanceId (\s a -> s { _sirInstanceId = a })
{-# INLINE sirInstanceId #-}

-- | The time stamp when the Spot Instance request was created.
sirCreateTime :: Lens' SpotInstanceRequest (Maybe ISO8601)
sirCreateTime = lens _sirCreateTime (\s a -> s { _sirCreateTime = a })
{-# INLINE sirCreateTime #-}

-- | The product description associated with the Spot Instance.
sirProductDescription :: Lens' SpotInstanceRequest (Maybe RIProductDescription)
sirProductDescription = lens _sirProductDescription (\s a -> s { _sirProductDescription = a })
{-# INLINE sirProductDescription #-}

-- | Any tags assigned to the resource.
sirTags :: Lens' SpotInstanceRequest ([Tag])
sirTags = lens _sirTags (\s a -> s { _sirTags = a })
{-# INLINE sirTags #-}

-- | The Availability Zone in which the bid is launched.
sirLaunchedAvailabilityZone :: Lens' SpotInstanceRequest (Maybe Text)
sirLaunchedAvailabilityZone = lens _sirLaunchedAvailabilityZone (\s a -> s { _sirLaunchedAvailabilityZone = a })
{-# INLINE sirLaunchedAvailabilityZone #-}

instance FromXML SpotInstanceRequest where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | The fault codes for the Spot Instance request, if any.
data SpotInstanceStateFault = SpotInstanceStateFault
    { _sisfCode :: Maybe Text
      -- ^ The reason code for the Spot Instance state change.
    , _sisfMessage :: Maybe Text
      -- ^ The message for the Spot Instance state change.
    } deriving (Show, Generic)

-- | The reason code for the Spot Instance state change.
sisfCode :: Lens' SpotInstanceStateFault (Maybe Text)
sisfCode = lens _sisfCode (\s a -> s { _sisfCode = a })
{-# INLINE sisfCode #-}

-- | The message for the Spot Instance state change.
sisfMessage :: Lens' SpotInstanceStateFault (Maybe Text)
sisfMessage = lens _sisfMessage (\s a -> s { _sisfMessage = a })
{-# INLINE sisfMessage #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'SpotInstanceStateFault' data type to populate a request.
mkSpotInstanceStateFault :: SpotInstanceStateFault
mkSpotInstanceStateFault = SpotInstanceStateFault
    { _sisfCode = Nothing
    , _sisfMessage = Nothing
    }
{-# INLINE mkSpotInstanceStateFault #-}

instance FromXML SpotInstanceStateFault where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "fault"

instance ToQuery SpotInstanceStateFault where
    toQuery = genericQuery def

-- | The status code and status message describing the Spot Instance request.
data SpotInstanceStatus = SpotInstanceStatus
    { _siuCode :: Maybe Text
      -- ^ The status code of the request.
    , _siuUpdateTime :: Maybe ISO8601
      -- ^ The time of the most recent status update.
    , _siuMessage :: Maybe Text
      -- ^ The description for the status code for the Spot request.
    } deriving (Show, Generic)

-- | The status code of the request.
siuCode :: Lens' SpotInstanceStatus (Maybe Text)
siuCode = lens _siuCode (\s a -> s { _siuCode = a })
{-# INLINE siuCode #-}

-- | The time of the most recent status update.
siuUpdateTime :: Lens' SpotInstanceStatus (Maybe ISO8601)
siuUpdateTime = lens _siuUpdateTime (\s a -> s { _siuUpdateTime = a })
{-# INLINE siuUpdateTime #-}

-- | The description for the status code for the Spot request.
siuMessage :: Lens' SpotInstanceStatus (Maybe Text)
siuMessage = lens _siuMessage (\s a -> s { _siuMessage = a })
{-# INLINE siuMessage #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'SpotInstanceStatus' data type to populate a request.
mkSpotInstanceStatus :: SpotInstanceStatus
mkSpotInstanceStatus = SpotInstanceStatus
    { _siuCode = Nothing
    , _siuUpdateTime = Nothing
    , _siuMessage = Nothing
    }
{-# INLINE mkSpotInstanceStatus #-}

instance FromXML SpotInstanceStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "status"

instance ToQuery SpotInstanceStatus where
    toQuery = genericQuery def

-- | The placement information for the instance.
data SpotPlacement = SpotPlacement
    { _spAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone.
    , _spGroupName :: Maybe Text
      -- ^ The Availability Zone group name.
    } deriving (Show, Generic)

-- | The Availability Zone.
spAvailabilityZone :: Lens' SpotPlacement (Maybe Text)
spAvailabilityZone = lens _spAvailabilityZone (\s a -> s { _spAvailabilityZone = a })
{-# INLINE spAvailabilityZone #-}

-- | The Availability Zone group name.
spGroupName :: Lens' SpotPlacement (Maybe Text)
spGroupName = lens _spGroupName (\s a -> s { _spGroupName = a })
{-# INLINE spGroupName #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'SpotPlacement' data type to populate a request.
mkSpotPlacement :: SpotPlacement
mkSpotPlacement = SpotPlacement
    { _spAvailabilityZone = Nothing
    , _spGroupName = Nothing
    }
{-# INLINE mkSpotPlacement #-}

instance FromXML SpotPlacement where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "placement"

instance ToQuery SpotPlacement where
    toQuery = genericQuery def

-- | Describes the Spot Price.
data SpotPrice = SpotPrice
    { _sqInstanceType :: Maybe InstanceType
      -- ^ The instance type.
    , _sqProductDescription :: Maybe RIProductDescription
      -- ^ A general description of the AMI.
    , _sqSpotPrice :: Maybe Text
      -- ^ The maximum price you will pay to launch one or more Spot
      -- Instances.
    , _sqTimestamp :: Maybe ISO8601
      -- ^ The date and time the request was created.
    , _sqAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone.
    } deriving (Show, Generic)

-- | The instance type.
sqInstanceType :: Lens' SpotPrice (Maybe InstanceType)
sqInstanceType = lens _sqInstanceType (\s a -> s { _sqInstanceType = a })
{-# INLINE sqInstanceType #-}

-- | A general description of the AMI.
sqProductDescription :: Lens' SpotPrice (Maybe RIProductDescription)
sqProductDescription = lens _sqProductDescription (\s a -> s { _sqProductDescription = a })
{-# INLINE sqProductDescription #-}

-- | The maximum price you will pay to launch one or more Spot Instances.
sqSpotPrice :: Lens' SpotPrice (Maybe Text)
sqSpotPrice = lens _sqSpotPrice (\s a -> s { _sqSpotPrice = a })
{-# INLINE sqSpotPrice #-}

-- | The date and time the request was created.
sqTimestamp :: Lens' SpotPrice (Maybe ISO8601)
sqTimestamp = lens _sqTimestamp (\s a -> s { _sqTimestamp = a })
{-# INLINE sqTimestamp #-}

-- | The Availability Zone.
sqAvailabilityZone :: Lens' SpotPrice (Maybe Text)
sqAvailabilityZone = lens _sqAvailabilityZone (\s a -> s { _sqAvailabilityZone = a })
{-# INLINE sqAvailabilityZone #-}

instance FromXML SpotPrice where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | The reason for the state change.
data StateReason = StateReason
    { _srCode :: Maybe Text
      -- ^ The reason code for the state change.
    , _srMessage :: Maybe Text
      -- ^ The message for the state change. Server.SpotInstanceTermination:
      -- A Spot Instance was terminated due to an increase in the market
      -- price. Server.InternalError: An internal error occurred during
      -- instance launch, resulting in termination.
      -- Server.InsufficientInstanceCapacity: There was insufficient
      -- instance capacity to satisfy the launch request.
      -- Client.InternalError: A client error caused the instance to
      -- terminate on launch. Client.InstanceInitiatedShutdown: The
      -- instance was shut down using the shutdown -h command from the
      -- instance. Client.UserInitiatedShutdown: The instance was shut
      -- down using the Amazon EC2 API. Client.VolumeLimitExceeded: The
      -- volume limit was exceeded. Client.InvalidSnapshot.NotFound: The
      -- specified snapshot was not found.
    } deriving (Show, Generic)

-- | The reason code for the state change.
srCode :: Lens' StateReason (Maybe Text)
srCode = lens _srCode (\s a -> s { _srCode = a })
{-# INLINE srCode #-}

-- | The message for the state change. Server.SpotInstanceTermination: A Spot
-- Instance was terminated due to an increase in the market price.
-- Server.InternalError: An internal error occurred during instance launch,
-- resulting in termination. Server.InsufficientInstanceCapacity: There was
-- insufficient instance capacity to satisfy the launch request.
-- Client.InternalError: A client error caused the instance to terminate on
-- launch. Client.InstanceInitiatedShutdown: The instance was shut down using
-- the shutdown -h command from the instance. Client.UserInitiatedShutdown:
-- The instance was shut down using the Amazon EC2 API.
-- Client.VolumeLimitExceeded: The volume limit was exceeded.
-- Client.InvalidSnapshot.NotFound: The specified snapshot was not found.
srMessage :: Lens' StateReason (Maybe Text)
srMessage = lens _srMessage (\s a -> s { _srMessage = a })
{-# INLINE srMessage #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'StateReason' data type to populate a request.
mkStateReason :: StateReason
mkStateReason = StateReason
    { _srCode = Nothing
    , _srMessage = Nothing
    }
{-# INLINE mkStateReason #-}

instance FromXML StateReason where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "stateReason"

instance ToQuery StateReason where
    toQuery = genericQuery def

-- | Information about the subnet.
data Subnet = Subnet
    { _sxSubnetId :: Maybe Text
      -- ^ The ID of the subnet.
    , _sxState :: Maybe SubnetState
      -- ^ The current state of the subnet.
    , _sxVpcId :: Maybe Text
      -- ^ The ID of the VPC the subnet is in.
    , _sxCidrBlock :: Maybe Text
      -- ^ The CIDR block assigned to the subnet.
    , _sxAvailableIpAddressCount :: Maybe Integer
      -- ^ The number of unused IP addresses in the subnet. Note that the IP
      -- addresses for any stopped instances are considered unavailable.
    , _sxAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone of the subnet.
    , _sxDefaultForAz :: Maybe Bool
      -- ^ Indicates whether this is the default subnet for the Availability
      -- Zone.
    , _sxMapPublicIpOnLaunch :: Maybe Bool
      -- ^ Indicates whether instances launched in this subnet receive a
      -- public IP address.
    , _sxTags :: [Tag]
      -- ^ Any tags assigned to the subnet.
    } deriving (Show, Generic)

-- | The ID of the subnet.
sxSubnetId :: Lens' Subnet (Maybe Text)
sxSubnetId = lens _sxSubnetId (\s a -> s { _sxSubnetId = a })
{-# INLINE sxSubnetId #-}

-- | The current state of the subnet.
sxState :: Lens' Subnet (Maybe SubnetState)
sxState = lens _sxState (\s a -> s { _sxState = a })
{-# INLINE sxState #-}

-- | The ID of the VPC the subnet is in.
sxVpcId :: Lens' Subnet (Maybe Text)
sxVpcId = lens _sxVpcId (\s a -> s { _sxVpcId = a })
{-# INLINE sxVpcId #-}

-- | The CIDR block assigned to the subnet.
sxCidrBlock :: Lens' Subnet (Maybe Text)
sxCidrBlock = lens _sxCidrBlock (\s a -> s { _sxCidrBlock = a })
{-# INLINE sxCidrBlock #-}

-- | The number of unused IP addresses in the subnet. Note that the IP addresses
-- for any stopped instances are considered unavailable.
sxAvailableIpAddressCount :: Lens' Subnet (Maybe Integer)
sxAvailableIpAddressCount = lens _sxAvailableIpAddressCount (\s a -> s { _sxAvailableIpAddressCount = a })
{-# INLINE sxAvailableIpAddressCount #-}

-- | The Availability Zone of the subnet.
sxAvailabilityZone :: Lens' Subnet (Maybe Text)
sxAvailabilityZone = lens _sxAvailabilityZone (\s a -> s { _sxAvailabilityZone = a })
{-# INLINE sxAvailabilityZone #-}

-- | Indicates whether this is the default subnet for the Availability Zone.
sxDefaultForAz :: Lens' Subnet (Maybe Bool)
sxDefaultForAz = lens _sxDefaultForAz (\s a -> s { _sxDefaultForAz = a })
{-# INLINE sxDefaultForAz #-}

-- | Indicates whether instances launched in this subnet receive a public IP
-- address.
sxMapPublicIpOnLaunch :: Lens' Subnet (Maybe Bool)
sxMapPublicIpOnLaunch = lens _sxMapPublicIpOnLaunch (\s a -> s { _sxMapPublicIpOnLaunch = a })
{-# INLINE sxMapPublicIpOnLaunch #-}

-- | Any tags assigned to the subnet.
sxTags :: Lens' Subnet ([Tag])
sxTags = lens _sxTags (\s a -> s { _sxTags = a })
{-# INLINE sxTags #-}

instance FromXML Subnet where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "subnet"

-- | Describes a tag.
data Tag = Tag
    { _tgKey :: Text
      -- ^ The key of the tag. Constraints: Tag keys are case-sensitive and
      -- accept a maximum of 127 Unicode characters. May not begin with
      -- aws:.
    , _tgValue :: Text
      -- ^ The value of the tag. Constraints: Tag values are case-sensitive
      -- and accept a maximum of 255 Unicode characters.
    } deriving (Show, Generic)

-- | The key of the tag. Constraints: Tag keys are case-sensitive and accept a
-- maximum of 127 Unicode characters. May not begin with aws:.
tgKey :: Lens' Tag (Text)
tgKey = lens _tgKey (\s a -> s { _tgKey = a })
{-# INLINE tgKey #-}

-- | The value of the tag. Constraints: Tag values are case-sensitive and accept
-- a maximum of 255 Unicode characters.
tgValue :: Lens' Tag (Text)
tgValue = lens _tgValue (\s a -> s { _tgValue = a })
{-# INLINE tgValue #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Tag' data type to populate a request.
mkTag :: Text -- ^ 'tgKey'
      -> Text -- ^ 'tgValue'
      -> Tag
mkTag p1 p2 = Tag
    { _tgKey = p1
    , _tgValue = p2
    }
{-# INLINE mkTag #-}

instance FromXML Tag where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery Tag where
    toQuery = genericQuery def

-- | Describes a tag.
data TagDescription = TagDescription
    { _tdResourceId :: Text
      -- ^ The ID of the resource. For example, ami-1a2b3c4d.
    , _tdResourceType :: ResourceType
      -- ^ The type of resource.
    , _tdKey :: Text
      -- ^ The key of the tag.
    , _tdValue :: Text
      -- ^ The value of the tag.
    } deriving (Show, Generic)

-- | The ID of the resource. For example, ami-1a2b3c4d.
tdResourceId :: Lens' TagDescription (Text)
tdResourceId = lens _tdResourceId (\s a -> s { _tdResourceId = a })
{-# INLINE tdResourceId #-}

-- | The type of resource.
tdResourceType :: Lens' TagDescription (ResourceType)
tdResourceType = lens _tdResourceType (\s a -> s { _tdResourceType = a })
{-# INLINE tdResourceType #-}

-- | The key of the tag.
tdKey :: Lens' TagDescription (Text)
tdKey = lens _tdKey (\s a -> s { _tdKey = a })
{-# INLINE tdKey #-}

-- | The value of the tag.
tdValue :: Lens' TagDescription (Text)
tdValue = lens _tdValue (\s a -> s { _tdValue = a })
{-# INLINE tdValue #-}

instance FromXML TagDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes a security group and AWS account ID pair for EC2-Classic.
data UserIdGroupPair = UserIdGroupPair
    { _uigpUserId :: Maybe Text
      -- ^ The ID of an AWS account.
    , _uigpGroupName :: Maybe Text
      -- ^ The ID of the security group owned by the specified AWS account.
    , _uigpGroupId :: Maybe Text
      -- ^ The name of the security group in the specified AWS account.
    } deriving (Show, Generic)

-- | The ID of an AWS account.
uigpUserId :: Lens' UserIdGroupPair (Maybe Text)
uigpUserId = lens _uigpUserId (\s a -> s { _uigpUserId = a })
{-# INLINE uigpUserId #-}

-- | The ID of the security group owned by the specified AWS account.
uigpGroupName :: Lens' UserIdGroupPair (Maybe Text)
uigpGroupName = lens _uigpGroupName (\s a -> s { _uigpGroupName = a })
{-# INLINE uigpGroupName #-}

-- | The name of the security group in the specified AWS account.
uigpGroupId :: Lens' UserIdGroupPair (Maybe Text)
uigpGroupId = lens _uigpGroupId (\s a -> s { _uigpGroupId = a })
{-# INLINE uigpGroupId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'UserIdGroupPair' data type to populate a request.
mkUserIdGroupPair :: UserIdGroupPair
mkUserIdGroupPair = UserIdGroupPair
    { _uigpUserId = Nothing
    , _uigpGroupName = Nothing
    , _uigpGroupId = Nothing
    }
{-# INLINE mkUserIdGroupPair #-}

instance FromXML UserIdGroupPair where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Groups"

instance ToQuery UserIdGroupPair where
    toQuery = genericQuery def

-- | Describes telemetry for a VPN tunnel.
data VgwTelemetry = VgwTelemetry
    { _vvvvvvvvvvvvvvyOutsideIpAddress :: Maybe Text
      -- ^ The Internet-routable IP address of the virtual private gateway's
      -- outside interface.
    , _vvvvvvvvvvvvvvyStatus :: Maybe TelemetryStatus
      -- ^ The status of the VPN tunnel.
    , _vvvvvvvvvvvvvvyLastStatusChange :: Maybe ISO8601
      -- ^ The date and time of the last change in status.
    , _vvvvvvvvvvvvvvyStatusMessage :: Maybe Text
      -- ^ If an error occurs, a description of the error.
    , _vvvvvvvvvvvvvvyAcceptedRouteCount :: Maybe Integer
      -- ^ The number of accepted routes.
    } deriving (Show, Generic)

-- | The Internet-routable IP address of the virtual private gateway's outside
-- interface.
vvvvvvvvvvvvvvyOutsideIpAddress :: Lens' VgwTelemetry (Maybe Text)
vvvvvvvvvvvvvvyOutsideIpAddress = lens _vvvvvvvvvvvvvvyOutsideIpAddress (\s a -> s { _vvvvvvvvvvvvvvyOutsideIpAddress = a })
{-# INLINE vvvvvvvvvvvvvvyOutsideIpAddress #-}

-- | The status of the VPN tunnel.
vvvvvvvvvvvvvvyStatus :: Lens' VgwTelemetry (Maybe TelemetryStatus)
vvvvvvvvvvvvvvyStatus = lens _vvvvvvvvvvvvvvyStatus (\s a -> s { _vvvvvvvvvvvvvvyStatus = a })
{-# INLINE vvvvvvvvvvvvvvyStatus #-}

-- | The date and time of the last change in status.
vvvvvvvvvvvvvvyLastStatusChange :: Lens' VgwTelemetry (Maybe ISO8601)
vvvvvvvvvvvvvvyLastStatusChange = lens _vvvvvvvvvvvvvvyLastStatusChange (\s a -> s { _vvvvvvvvvvvvvvyLastStatusChange = a })
{-# INLINE vvvvvvvvvvvvvvyLastStatusChange #-}

-- | If an error occurs, a description of the error.
vvvvvvvvvvvvvvyStatusMessage :: Lens' VgwTelemetry (Maybe Text)
vvvvvvvvvvvvvvyStatusMessage = lens _vvvvvvvvvvvvvvyStatusMessage (\s a -> s { _vvvvvvvvvvvvvvyStatusMessage = a })
{-# INLINE vvvvvvvvvvvvvvyStatusMessage #-}

-- | The number of accepted routes.
vvvvvvvvvvvvvvyAcceptedRouteCount :: Lens' VgwTelemetry (Maybe Integer)
vvvvvvvvvvvvvvyAcceptedRouteCount = lens _vvvvvvvvvvvvvvyAcceptedRouteCount (\s a -> s { _vvvvvvvvvvvvvvyAcceptedRouteCount = a })
{-# INLINE vvvvvvvvvvvvvvyAcceptedRouteCount #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VgwTelemetry' data type to populate a request.
mkVgwTelemetry :: VgwTelemetry
mkVgwTelemetry = VgwTelemetry
    { _vvvvvvvvvvvvvvyOutsideIpAddress = Nothing
    , _vvvvvvvvvvvvvvyStatus = Nothing
    , _vvvvvvvvvvvvvvyLastStatusChange = Nothing
    , _vvvvvvvvvvvvvvyStatusMessage = Nothing
    , _vvvvvvvvvvvvvvyAcceptedRouteCount = Nothing
    }
{-# INLINE mkVgwTelemetry #-}

instance FromXML VgwTelemetry where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery VgwTelemetry where
    toQuery = genericQuery def

-- | Describes a volume.
data Volume = Volume
    { _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjVolumeId :: Maybe Text
      -- ^ The ID of the volume.
    , _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjSize :: Maybe Integer
      -- ^ The size of the volume, in GiBs.
    , _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjSnapshotId :: Maybe Text
      -- ^ The snapshot from which the volume was created, if applicable.
    , _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone for the volume.
    , _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjState :: Maybe VolumeState
      -- ^ The volume state.
    , _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjCreateTime :: Maybe ISO8601
      -- ^ The time stamp when volume creation was initiated.
    , _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjAttachments :: [VolumeAttachment]
      -- ^ 
    , _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjTags :: [Tag]
      -- ^ Any tags assigned to the volume.
    , _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjVolumeType :: Maybe VolumeType
      -- ^ The volume type. This can be gp2 for General Purpose (SSD)
      -- volumes, io1 for Provisioned IOPS (SSD) volumes, or standard for
      -- Magnetic volumes.
    , _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjIops :: Maybe Integer
      -- ^ The number of I/O operations per second (IOPS) that the volume
      -- supports. For Provisioned IOPS (SSD) volumes, this represents the
      -- number of IOPS that are provisioned for the volume. For General
      -- Purpose (SSD) volumes, this represents the baseline performance
      -- of the volume and the rate at which the volume accumulates I/O
      -- credits for bursting. For more information on General Purpose
      -- (SSD) baseline performance, I/O credits, and bursting, see Amazon
      -- EBS Volume Types in the Amazon Elastic Compute Cloud User Guide.
      -- Constraint: Range is 100 to 4000 for Provisioned IOPS (SSD)
      -- volumes and 3 to 3072 for General Purpose (SSD) volumes.
      -- Condition: This parameter is required for requests to create io1
      -- volumes; it is not used in requests to create standard or gp2
      -- volumes.
    , _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjEncrypted :: Maybe Bool
      -- ^ Indicates whether the volume is encrypted.
    } deriving (Show, Generic)

-- | The ID of the volume.
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjVolumeId :: Lens' Volume (Maybe Text)
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjVolumeId = lens _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjVolumeId (\s a -> s { _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjVolumeId = a })
{-# INLINE vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjVolumeId #-}

-- | The size of the volume, in GiBs.
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjSize :: Lens' Volume (Maybe Integer)
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjSize = lens _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjSize (\s a -> s { _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjSize = a })
{-# INLINE vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjSize #-}

-- | The snapshot from which the volume was created, if applicable.
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjSnapshotId :: Lens' Volume (Maybe Text)
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjSnapshotId = lens _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjSnapshotId (\s a -> s { _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjSnapshotId = a })
{-# INLINE vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjSnapshotId #-}

-- | The Availability Zone for the volume.
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjAvailabilityZone :: Lens' Volume (Maybe Text)
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjAvailabilityZone = lens _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjAvailabilityZone (\s a -> s { _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjAvailabilityZone = a })
{-# INLINE vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjAvailabilityZone #-}

-- | The volume state.
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjState :: Lens' Volume (Maybe VolumeState)
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjState = lens _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjState (\s a -> s { _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjState = a })
{-# INLINE vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjState #-}

-- | The time stamp when volume creation was initiated.
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjCreateTime :: Lens' Volume (Maybe ISO8601)
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjCreateTime = lens _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjCreateTime (\s a -> s { _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjCreateTime = a })
{-# INLINE vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjCreateTime #-}

-- | 
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjAttachments :: Lens' Volume ([VolumeAttachment])
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjAttachments = lens _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjAttachments (\s a -> s { _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjAttachments = a })
{-# INLINE vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjAttachments #-}

-- | Any tags assigned to the volume.
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjTags :: Lens' Volume ([Tag])
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjTags = lens _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjTags (\s a -> s { _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjTags = a })
{-# INLINE vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjTags #-}

-- | The volume type. This can be gp2 for General Purpose (SSD) volumes, io1 for
-- Provisioned IOPS (SSD) volumes, or standard for Magnetic volumes.
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjVolumeType :: Lens' Volume (Maybe VolumeType)
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjVolumeType = lens _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjVolumeType (\s a -> s { _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjVolumeType = a })
{-# INLINE vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjVolumeType #-}

-- | The number of I/O operations per second (IOPS) that the volume supports.
-- For Provisioned IOPS (SSD) volumes, this represents the number of IOPS that
-- are provisioned for the volume. For General Purpose (SSD) volumes, this
-- represents the baseline performance of the volume and the rate at which the
-- volume accumulates I/O credits for bursting. For more information on
-- General Purpose (SSD) baseline performance, I/O credits, and bursting, see
-- Amazon EBS Volume Types in the Amazon Elastic Compute Cloud User Guide.
-- Constraint: Range is 100 to 4000 for Provisioned IOPS (SSD) volumes and 3
-- to 3072 for General Purpose (SSD) volumes. Condition: This parameter is
-- required for requests to create io1 volumes; it is not used in requests to
-- create standard or gp2 volumes.
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjIops :: Lens' Volume (Maybe Integer)
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjIops = lens _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjIops (\s a -> s { _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjIops = a })
{-# INLINE vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjIops #-}

-- | Indicates whether the volume is encrypted.
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjEncrypted :: Lens' Volume (Maybe Bool)
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjEncrypted = lens _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjEncrypted (\s a -> s { _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjEncrypted = a })
{-# INLINE vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjEncrypted #-}

instance FromXML Volume where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes volume attachment details.
data VolumeAttachment = VolumeAttachment
    { _vcVolumeId :: Maybe Text
      -- ^ The ID of the volume.
    , _vcInstanceId :: Maybe Text
      -- ^ The ID of the instance.
    , _vcDevice :: Maybe Text
      -- ^ The device name.
    , _vcState :: Maybe VolumeAttachmentState
      -- ^ The attachment state of the volume.
    , _vcAttachTime :: Maybe ISO8601
      -- ^ The time stamp when the attachment initiated.
    , _vcDeleteOnTermination :: Maybe Bool
      -- ^ Indicates whether the Amazon EBS volume is deleted on instance
      -- termination.
    } deriving (Show, Generic)

-- | The ID of the volume.
vcVolumeId :: Lens' VolumeAttachment (Maybe Text)
vcVolumeId = lens _vcVolumeId (\s a -> s { _vcVolumeId = a })
{-# INLINE vcVolumeId #-}

-- | The ID of the instance.
vcInstanceId :: Lens' VolumeAttachment (Maybe Text)
vcInstanceId = lens _vcInstanceId (\s a -> s { _vcInstanceId = a })
{-# INLINE vcInstanceId #-}

-- | The device name.
vcDevice :: Lens' VolumeAttachment (Maybe Text)
vcDevice = lens _vcDevice (\s a -> s { _vcDevice = a })
{-# INLINE vcDevice #-}

-- | The attachment state of the volume.
vcState :: Lens' VolumeAttachment (Maybe VolumeAttachmentState)
vcState = lens _vcState (\s a -> s { _vcState = a })
{-# INLINE vcState #-}

-- | The time stamp when the attachment initiated.
vcAttachTime :: Lens' VolumeAttachment (Maybe ISO8601)
vcAttachTime = lens _vcAttachTime (\s a -> s { _vcAttachTime = a })
{-# INLINE vcAttachTime #-}

-- | Indicates whether the Amazon EBS volume is deleted on instance termination.
vcDeleteOnTermination :: Lens' VolumeAttachment (Maybe Bool)
vcDeleteOnTermination = lens _vcDeleteOnTermination (\s a -> s { _vcDeleteOnTermination = a })
{-# INLINE vcDeleteOnTermination #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VolumeAttachment' data type to populate a request.
mkVolumeAttachment :: VolumeAttachment
mkVolumeAttachment = VolumeAttachment
    { _vcVolumeId = Nothing
    , _vcInstanceId = Nothing
    , _vcDevice = Nothing
    , _vcState = Nothing
    , _vcAttachTime = Nothing
    , _vcDeleteOnTermination = Nothing
    }
{-# INLINE mkVolumeAttachment #-}

instance FromXML VolumeAttachment where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery VolumeAttachment where
    toQuery = genericQuery def

-- | Describes a volume status operation code.
data VolumeStatusAction = VolumeStatusAction
    { _vsaCode :: Maybe Text
      -- ^ The code identifying the operation, for example,
      -- enable-volume-io.
    , _vsaDescription :: Maybe Text
      -- ^ A description of the operation.
    , _vsaEventType :: Maybe Text
      -- ^ The event type associated with this operation.
    , _vsaEventId :: Maybe Text
      -- ^ The ID of the event associated with this operation.
    } deriving (Show, Generic)

-- | The code identifying the operation, for example, enable-volume-io.
vsaCode :: Lens' VolumeStatusAction (Maybe Text)
vsaCode = lens _vsaCode (\s a -> s { _vsaCode = a })
{-# INLINE vsaCode #-}

-- | A description of the operation.
vsaDescription :: Lens' VolumeStatusAction (Maybe Text)
vsaDescription = lens _vsaDescription (\s a -> s { _vsaDescription = a })
{-# INLINE vsaDescription #-}

-- | The event type associated with this operation.
vsaEventType :: Lens' VolumeStatusAction (Maybe Text)
vsaEventType = lens _vsaEventType (\s a -> s { _vsaEventType = a })
{-# INLINE vsaEventType #-}

-- | The ID of the event associated with this operation.
vsaEventId :: Lens' VolumeStatusAction (Maybe Text)
vsaEventId = lens _vsaEventId (\s a -> s { _vsaEventId = a })
{-# INLINE vsaEventId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VolumeStatusAction' data type to populate a request.
mkVolumeStatusAction :: VolumeStatusAction
mkVolumeStatusAction = VolumeStatusAction
    { _vsaCode = Nothing
    , _vsaDescription = Nothing
    , _vsaEventType = Nothing
    , _vsaEventId = Nothing
    }
{-# INLINE mkVolumeStatusAction #-}

instance FromXML VolumeStatusAction where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery VolumeStatusAction where
    toQuery = genericQuery def

-- | Describes a volume status.
data VolumeStatusDetails = VolumeStatusDetails
    { _vsdName :: Maybe VolumeStatusName
      -- ^ The name of the volume status.
    , _vsdStatus :: Maybe Text
      -- ^ The intended status of the volume status.
    } deriving (Show, Generic)

-- | The name of the volume status.
vsdName :: Lens' VolumeStatusDetails (Maybe VolumeStatusName)
vsdName = lens _vsdName (\s a -> s { _vsdName = a })
{-# INLINE vsdName #-}

-- | The intended status of the volume status.
vsdStatus :: Lens' VolumeStatusDetails (Maybe Text)
vsdStatus = lens _vsdStatus (\s a -> s { _vsdStatus = a })
{-# INLINE vsdStatus #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VolumeStatusDetails' data type to populate a request.
mkVolumeStatusDetails :: VolumeStatusDetails
mkVolumeStatusDetails = VolumeStatusDetails
    { _vsdName = Nothing
    , _vsdStatus = Nothing
    }
{-# INLINE mkVolumeStatusDetails #-}

instance FromXML VolumeStatusDetails where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery VolumeStatusDetails where
    toQuery = genericQuery def

-- | Describes a volume status event.
data VolumeStatusEvent = VolumeStatusEvent
    { _vseEventType :: Maybe Text
      -- ^ The type of this event.
    , _vseDescription :: Maybe Text
      -- ^ A description of the event.
    , _vseNotBefore :: Maybe ISO8601
      -- ^ The earliest start time of the event.
    , _vseNotAfter :: Maybe ISO8601
      -- ^ The latest end time of the event.
    , _vseEventId :: Maybe Text
      -- ^ The ID of this event.
    } deriving (Show, Generic)

-- | The type of this event.
vseEventType :: Lens' VolumeStatusEvent (Maybe Text)
vseEventType = lens _vseEventType (\s a -> s { _vseEventType = a })
{-# INLINE vseEventType #-}

-- | A description of the event.
vseDescription :: Lens' VolumeStatusEvent (Maybe Text)
vseDescription = lens _vseDescription (\s a -> s { _vseDescription = a })
{-# INLINE vseDescription #-}

-- | The earliest start time of the event.
vseNotBefore :: Lens' VolumeStatusEvent (Maybe ISO8601)
vseNotBefore = lens _vseNotBefore (\s a -> s { _vseNotBefore = a })
{-# INLINE vseNotBefore #-}

-- | The latest end time of the event.
vseNotAfter :: Lens' VolumeStatusEvent (Maybe ISO8601)
vseNotAfter = lens _vseNotAfter (\s a -> s { _vseNotAfter = a })
{-# INLINE vseNotAfter #-}

-- | The ID of this event.
vseEventId :: Lens' VolumeStatusEvent (Maybe Text)
vseEventId = lens _vseEventId (\s a -> s { _vseEventId = a })
{-# INLINE vseEventId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VolumeStatusEvent' data type to populate a request.
mkVolumeStatusEvent :: VolumeStatusEvent
mkVolumeStatusEvent = VolumeStatusEvent
    { _vseEventType = Nothing
    , _vseDescription = Nothing
    , _vseNotBefore = Nothing
    , _vseNotAfter = Nothing
    , _vseEventId = Nothing
    }
{-# INLINE mkVolumeStatusEvent #-}

instance FromXML VolumeStatusEvent where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery VolumeStatusEvent where
    toQuery = genericQuery def

-- | The volume status.
data VolumeStatusInfo = VolumeStatusInfo
    { _vsjStatus :: Maybe VolumeStatusInfoStatus
      -- ^ The status of the volume.
    , _vsjDetails :: [VolumeStatusDetails]
      -- ^ The details of the volume status.
    } deriving (Show, Generic)

-- | The status of the volume.
vsjStatus :: Lens' VolumeStatusInfo (Maybe VolumeStatusInfoStatus)
vsjStatus = lens _vsjStatus (\s a -> s { _vsjStatus = a })
{-# INLINE vsjStatus #-}

-- | The details of the volume status.
vsjDetails :: Lens' VolumeStatusInfo ([VolumeStatusDetails])
vsjDetails = lens _vsjDetails (\s a -> s { _vsjDetails = a })
{-# INLINE vsjDetails #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VolumeStatusInfo' data type to populate a request.
mkVolumeStatusInfo :: VolumeStatusInfo
mkVolumeStatusInfo = VolumeStatusInfo
    { _vsjStatus = Nothing
    , _vsjDetails = mempty
    }
{-# INLINE mkVolumeStatusInfo #-}

instance FromXML VolumeStatusInfo where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "volumeStatus"

instance ToQuery VolumeStatusInfo where
    toQuery = genericQuery def

-- | Describes the volume status.
data VolumeStatusItem = VolumeStatusItem
    { _vsiVolumeId :: Maybe Text
      -- ^ The volume ID.
    , _vsiAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone of the volume.
    , _vsiVolumeStatus :: Maybe VolumeStatusInfo
      -- ^ The volume status.
    , _vsiEvents :: [VolumeStatusEvent]
      -- ^ A list of events associated with the volume.
    , _vsiActions :: [VolumeStatusAction]
      -- ^ The details of the operation.
    } deriving (Show, Generic)

-- | The volume ID.
vsiVolumeId :: Lens' VolumeStatusItem (Maybe Text)
vsiVolumeId = lens _vsiVolumeId (\s a -> s { _vsiVolumeId = a })
{-# INLINE vsiVolumeId #-}

-- | The Availability Zone of the volume.
vsiAvailabilityZone :: Lens' VolumeStatusItem (Maybe Text)
vsiAvailabilityZone = lens _vsiAvailabilityZone (\s a -> s { _vsiAvailabilityZone = a })
{-# INLINE vsiAvailabilityZone #-}

-- | The volume status.
vsiVolumeStatus :: Lens' VolumeStatusItem (Maybe VolumeStatusInfo)
vsiVolumeStatus = lens _vsiVolumeStatus (\s a -> s { _vsiVolumeStatus = a })
{-# INLINE vsiVolumeStatus #-}

-- | A list of events associated with the volume.
vsiEvents :: Lens' VolumeStatusItem ([VolumeStatusEvent])
vsiEvents = lens _vsiEvents (\s a -> s { _vsiEvents = a })
{-# INLINE vsiEvents #-}

-- | The details of the operation.
vsiActions :: Lens' VolumeStatusItem ([VolumeStatusAction])
vsiActions = lens _vsiActions (\s a -> s { _vsiActions = a })
{-# INLINE vsiActions #-}

instance FromXML VolumeStatusItem where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Information about the VPC.
data Vpc = Vpc
    { _vdVpcId :: Maybe Text
      -- ^ The ID of the VPC.
    , _vdState :: Maybe VpcState
      -- ^ The current state of the VPC.
    , _vdCidrBlock :: Maybe Text
      -- ^ The CIDR block for the VPC.
    , _vdDhcpOptionsId :: Maybe Text
      -- ^ The ID of the set of DHCP options you've associated with the VPC
      -- (or default if the default options are associated with the VPC).
    , _vdTags :: [Tag]
      -- ^ Any tags assigned to the VPC.
    , _vdInstanceTenancy :: Maybe Tenancy
      -- ^ The allowed tenancy of instances launched into the VPC.
    , _vdIsDefault :: Maybe Bool
      -- ^ Indicates whether the VPC is the default VPC.
    } deriving (Show, Generic)

-- | The ID of the VPC.
vdVpcId :: Lens' Vpc (Maybe Text)
vdVpcId = lens _vdVpcId (\s a -> s { _vdVpcId = a })
{-# INLINE vdVpcId #-}

-- | The current state of the VPC.
vdState :: Lens' Vpc (Maybe VpcState)
vdState = lens _vdState (\s a -> s { _vdState = a })
{-# INLINE vdState #-}

-- | The CIDR block for the VPC.
vdCidrBlock :: Lens' Vpc (Maybe Text)
vdCidrBlock = lens _vdCidrBlock (\s a -> s { _vdCidrBlock = a })
{-# INLINE vdCidrBlock #-}

-- | The ID of the set of DHCP options you've associated with the VPC (or
-- default if the default options are associated with the VPC).
vdDhcpOptionsId :: Lens' Vpc (Maybe Text)
vdDhcpOptionsId = lens _vdDhcpOptionsId (\s a -> s { _vdDhcpOptionsId = a })
{-# INLINE vdDhcpOptionsId #-}

-- | Any tags assigned to the VPC.
vdTags :: Lens' Vpc ([Tag])
vdTags = lens _vdTags (\s a -> s { _vdTags = a })
{-# INLINE vdTags #-}

-- | The allowed tenancy of instances launched into the VPC.
vdInstanceTenancy :: Lens' Vpc (Maybe Tenancy)
vdInstanceTenancy = lens _vdInstanceTenancy (\s a -> s { _vdInstanceTenancy = a })
{-# INLINE vdInstanceTenancy #-}

-- | Indicates whether the VPC is the default VPC.
vdIsDefault :: Lens' Vpc (Maybe Bool)
vdIsDefault = lens _vdIsDefault (\s a -> s { _vdIsDefault = a })
{-# INLINE vdIsDefault #-}

instance FromXML Vpc where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "vpc"

-- | Information about the attachment.
data VpcAttachment = VpcAttachment
    { _vbVpcId :: Maybe Text
      -- ^ The ID of the VPC.
    , _vbState :: Maybe AttachmentStatus
      -- ^ The current state of the attachment.
    } deriving (Show, Generic)

-- | The ID of the VPC.
vbVpcId :: Lens' VpcAttachment (Maybe Text)
vbVpcId = lens _vbVpcId (\s a -> s { _vbVpcId = a })
{-# INLINE vbVpcId #-}

-- | The current state of the attachment.
vbState :: Lens' VpcAttachment (Maybe AttachmentStatus)
vbState = lens _vbState (\s a -> s { _vbState = a })
{-# INLINE vbState #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VpcAttachment' data type to populate a request.
mkVpcAttachment :: VpcAttachment
mkVpcAttachment = VpcAttachment
    { _vbVpcId = Nothing
    , _vbState = Nothing
    }
{-# INLINE mkVpcAttachment #-}

instance FromXML VpcAttachment where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "attachment"

instance ToQuery VpcAttachment where
    toQuery = genericQuery def

-- | Information about the VPC peering connection.
data VpcPeeringConnection = VpcPeeringConnection
    { _vpcAccepterVpcInfo :: Maybe VpcPeeringConnectionVpcInfo
      -- ^ The information of the peer VPC.
    , _vpcExpirationTime :: Maybe ISO8601
      -- ^ The time that an unaccepted VPC peering connection will expire.
    , _vpcRequesterVpcInfo :: Maybe VpcPeeringConnectionVpcInfo
      -- ^ The information of the requester VPC.
    , _vpcStatus :: Maybe VpcPeeringConnectionStateReason
      -- ^ The status of the VPC peering connection.
    , _vpcTags :: [Tag]
      -- ^ Any tags assigned to the resource.
    , _vpcVpcPeeringConnectionId :: Maybe Text
      -- ^ The ID of the VPC peering connection.
    } deriving (Show, Generic)

-- | The information of the peer VPC.
vpcAccepterVpcInfo :: Lens' VpcPeeringConnection (Maybe VpcPeeringConnectionVpcInfo)
vpcAccepterVpcInfo = lens _vpcAccepterVpcInfo (\s a -> s { _vpcAccepterVpcInfo = a })
{-# INLINE vpcAccepterVpcInfo #-}

-- | The time that an unaccepted VPC peering connection will expire.
vpcExpirationTime :: Lens' VpcPeeringConnection (Maybe ISO8601)
vpcExpirationTime = lens _vpcExpirationTime (\s a -> s { _vpcExpirationTime = a })
{-# INLINE vpcExpirationTime #-}

-- | The information of the requester VPC.
vpcRequesterVpcInfo :: Lens' VpcPeeringConnection (Maybe VpcPeeringConnectionVpcInfo)
vpcRequesterVpcInfo = lens _vpcRequesterVpcInfo (\s a -> s { _vpcRequesterVpcInfo = a })
{-# INLINE vpcRequesterVpcInfo #-}

-- | The status of the VPC peering connection.
vpcStatus :: Lens' VpcPeeringConnection (Maybe VpcPeeringConnectionStateReason)
vpcStatus = lens _vpcStatus (\s a -> s { _vpcStatus = a })
{-# INLINE vpcStatus #-}

-- | Any tags assigned to the resource.
vpcTags :: Lens' VpcPeeringConnection ([Tag])
vpcTags = lens _vpcTags (\s a -> s { _vpcTags = a })
{-# INLINE vpcTags #-}

-- | The ID of the VPC peering connection.
vpcVpcPeeringConnectionId :: Lens' VpcPeeringConnection (Maybe Text)
vpcVpcPeeringConnectionId = lens _vpcVpcPeeringConnectionId (\s a -> s { _vpcVpcPeeringConnectionId = a })
{-# INLINE vpcVpcPeeringConnectionId #-}

instance FromXML VpcPeeringConnection where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "vpcPeeringConnection"

-- | The status of the VPC peering connection.
data VpcPeeringConnectionStateReason = VpcPeeringConnectionStateReason
    { _vpcsrCode :: Maybe Text
      -- ^ The status of the VPC peering connection.
    , _vpcsrMessage :: Maybe Text
      -- ^ A message that provides more information about the status, if
      -- applicable.
    } deriving (Show, Generic)

-- | The status of the VPC peering connection.
vpcsrCode :: Lens' VpcPeeringConnectionStateReason (Maybe Text)
vpcsrCode = lens _vpcsrCode (\s a -> s { _vpcsrCode = a })
{-# INLINE vpcsrCode #-}

-- | A message that provides more information about the status, if applicable.
vpcsrMessage :: Lens' VpcPeeringConnectionStateReason (Maybe Text)
vpcsrMessage = lens _vpcsrMessage (\s a -> s { _vpcsrMessage = a })
{-# INLINE vpcsrMessage #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VpcPeeringConnectionStateReason' data type to populate a request.
mkVpcPeeringConnectionStateReason :: VpcPeeringConnectionStateReason
mkVpcPeeringConnectionStateReason = VpcPeeringConnectionStateReason
    { _vpcsrCode = Nothing
    , _vpcsrMessage = Nothing
    }
{-# INLINE mkVpcPeeringConnectionStateReason #-}

instance FromXML VpcPeeringConnectionStateReason where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "status"

instance ToQuery VpcPeeringConnectionStateReason where
    toQuery = genericQuery def

-- | The information of the peer VPC.
data VpcPeeringConnectionVpcInfo = VpcPeeringConnectionVpcInfo
    { _vpcviCidrBlock :: Maybe Text
      -- ^ The CIDR block for the VPC.
    , _vpcviOwnerId :: Maybe Text
      -- ^ The AWS account ID of the VPC owner.
    , _vpcviVpcId :: Maybe Text
      -- ^ The ID of the VPC.
    } deriving (Show, Generic)

-- | The CIDR block for the VPC.
vpcviCidrBlock :: Lens' VpcPeeringConnectionVpcInfo (Maybe Text)
vpcviCidrBlock = lens _vpcviCidrBlock (\s a -> s { _vpcviCidrBlock = a })
{-# INLINE vpcviCidrBlock #-}

-- | The AWS account ID of the VPC owner.
vpcviOwnerId :: Lens' VpcPeeringConnectionVpcInfo (Maybe Text)
vpcviOwnerId = lens _vpcviOwnerId (\s a -> s { _vpcviOwnerId = a })
{-# INLINE vpcviOwnerId #-}

-- | The ID of the VPC.
vpcviVpcId :: Lens' VpcPeeringConnectionVpcInfo (Maybe Text)
vpcviVpcId = lens _vpcviVpcId (\s a -> s { _vpcviVpcId = a })
{-# INLINE vpcviVpcId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VpcPeeringConnectionVpcInfo' data type to populate a request.
mkVpcPeeringConnectionVpcInfo :: VpcPeeringConnectionVpcInfo
mkVpcPeeringConnectionVpcInfo = VpcPeeringConnectionVpcInfo
    { _vpcviCidrBlock = Nothing
    , _vpcviOwnerId = Nothing
    , _vpcviVpcId = Nothing
    }
{-# INLINE mkVpcPeeringConnectionVpcInfo #-}

instance FromXML VpcPeeringConnectionVpcInfo where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "accepterVpcInfo"

instance ToQuery VpcPeeringConnectionVpcInfo where
    toQuery = genericQuery def

-- | Information about the VPN connection.
data VpnConnection = VpnConnection
    { _vvvvvvvvvvvvvvoVpnConnectionId :: Maybe Text
      -- ^ The ID of the VPN connection.
    , _vvvvvvvvvvvvvvoState :: Maybe VpnState
      -- ^ The current state of the VPN connection.
    , _vvvvvvvvvvvvvvoCustomerGatewayConfiguration :: Maybe Text
      -- ^ The configuration information for the VPN connection's customer
      -- gateway (in the native XML format). This element is always
      -- present in the CreateVpnConnection response; however, it's
      -- present in the DescribeVpnConnections response only if the VPN
      -- connection is in the pending or available state.
    , _vvvvvvvvvvvvvvoType :: Maybe GatewayType
      -- ^ The type of VPN connection.
    , _vvvvvvvvvvvvvvoCustomerGatewayId :: Maybe Text
      -- ^ The ID of the customer gateway at your end of the VPN connection.
    , _vvvvvvvvvvvvvvoVpnGatewayId :: Maybe Text
      -- ^ The ID of the virtual private gateway at the AWS side of the VPN
      -- connection.
    , _vvvvvvvvvvvvvvoTags :: [Tag]
      -- ^ Any tags assigned to the VPN connection.
    , _vvvvvvvvvvvvvvoVgwTelemetry :: [VgwTelemetry]
      -- ^ Information about the VPN tunnel.
    , _vvvvvvvvvvvvvvoOptions :: Maybe VpnConnectionOptions
      -- ^ The VPN connection options.
    , _vvvvvvvvvvvvvvoRoutes :: [VpnStaticRoute]
      -- ^ The static routes associated with the VPN connection.
    } deriving (Show, Generic)

-- | The ID of the VPN connection.
vvvvvvvvvvvvvvoVpnConnectionId :: Lens' VpnConnection (Maybe Text)
vvvvvvvvvvvvvvoVpnConnectionId = lens _vvvvvvvvvvvvvvoVpnConnectionId (\s a -> s { _vvvvvvvvvvvvvvoVpnConnectionId = a })
{-# INLINE vvvvvvvvvvvvvvoVpnConnectionId #-}

-- | The current state of the VPN connection.
vvvvvvvvvvvvvvoState :: Lens' VpnConnection (Maybe VpnState)
vvvvvvvvvvvvvvoState = lens _vvvvvvvvvvvvvvoState (\s a -> s { _vvvvvvvvvvvvvvoState = a })
{-# INLINE vvvvvvvvvvvvvvoState #-}

-- | The configuration information for the VPN connection's customer gateway (in
-- the native XML format). This element is always present in the
-- CreateVpnConnection response; however, it's present in the
-- DescribeVpnConnections response only if the VPN connection is in the
-- pending or available state.
vvvvvvvvvvvvvvoCustomerGatewayConfiguration :: Lens' VpnConnection (Maybe Text)
vvvvvvvvvvvvvvoCustomerGatewayConfiguration = lens _vvvvvvvvvvvvvvoCustomerGatewayConfiguration (\s a -> s { _vvvvvvvvvvvvvvoCustomerGatewayConfiguration = a })
{-# INLINE vvvvvvvvvvvvvvoCustomerGatewayConfiguration #-}

-- | The type of VPN connection.
vvvvvvvvvvvvvvoType :: Lens' VpnConnection (Maybe GatewayType)
vvvvvvvvvvvvvvoType = lens _vvvvvvvvvvvvvvoType (\s a -> s { _vvvvvvvvvvvvvvoType = a })
{-# INLINE vvvvvvvvvvvvvvoType #-}

-- | The ID of the customer gateway at your end of the VPN connection.
vvvvvvvvvvvvvvoCustomerGatewayId :: Lens' VpnConnection (Maybe Text)
vvvvvvvvvvvvvvoCustomerGatewayId = lens _vvvvvvvvvvvvvvoCustomerGatewayId (\s a -> s { _vvvvvvvvvvvvvvoCustomerGatewayId = a })
{-# INLINE vvvvvvvvvvvvvvoCustomerGatewayId #-}

-- | The ID of the virtual private gateway at the AWS side of the VPN
-- connection.
vvvvvvvvvvvvvvoVpnGatewayId :: Lens' VpnConnection (Maybe Text)
vvvvvvvvvvvvvvoVpnGatewayId = lens _vvvvvvvvvvvvvvoVpnGatewayId (\s a -> s { _vvvvvvvvvvvvvvoVpnGatewayId = a })
{-# INLINE vvvvvvvvvvvvvvoVpnGatewayId #-}

-- | Any tags assigned to the VPN connection.
vvvvvvvvvvvvvvoTags :: Lens' VpnConnection ([Tag])
vvvvvvvvvvvvvvoTags = lens _vvvvvvvvvvvvvvoTags (\s a -> s { _vvvvvvvvvvvvvvoTags = a })
{-# INLINE vvvvvvvvvvvvvvoTags #-}

-- | Information about the VPN tunnel.
vvvvvvvvvvvvvvoVgwTelemetry :: Lens' VpnConnection ([VgwTelemetry])
vvvvvvvvvvvvvvoVgwTelemetry = lens _vvvvvvvvvvvvvvoVgwTelemetry (\s a -> s { _vvvvvvvvvvvvvvoVgwTelemetry = a })
{-# INLINE vvvvvvvvvvvvvvoVgwTelemetry #-}

-- | The VPN connection options.
vvvvvvvvvvvvvvoOptions :: Lens' VpnConnection (Maybe VpnConnectionOptions)
vvvvvvvvvvvvvvoOptions = lens _vvvvvvvvvvvvvvoOptions (\s a -> s { _vvvvvvvvvvvvvvoOptions = a })
{-# INLINE vvvvvvvvvvvvvvoOptions #-}

-- | The static routes associated with the VPN connection.
vvvvvvvvvvvvvvoRoutes :: Lens' VpnConnection ([VpnStaticRoute])
vvvvvvvvvvvvvvoRoutes = lens _vvvvvvvvvvvvvvoRoutes (\s a -> s { _vvvvvvvvvvvvvvoRoutes = a })
{-# INLINE vvvvvvvvvvvvvvoRoutes #-}

instance FromXML VpnConnection where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "vpnConnection"

-- | Information about the virtual private gateway.
data VpnGateway = VpnGateway
    { _vvvvvvvvvvvvvvvyVpnGatewayId :: Maybe Text
      -- ^ The ID of the virtual private gateway.
    , _vvvvvvvvvvvvvvvyState :: Maybe VpnState
      -- ^ The current state of the virtual private gateway.
    , _vvvvvvvvvvvvvvvyType :: Maybe GatewayType
      -- ^ The type of VPN connection the virtual private gateway supports.
    , _vvvvvvvvvvvvvvvyAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone where the virtual private gateway was
      -- created.
    , _vvvvvvvvvvvvvvvyVpcAttachments :: [VpcAttachment]
      -- ^ Any VPCs attached to the virtual private gateway.
    , _vvvvvvvvvvvvvvvyTags :: [Tag]
      -- ^ Any tags assigned to the virtual private gateway.
    } deriving (Show, Generic)

-- | The ID of the virtual private gateway.
vvvvvvvvvvvvvvvyVpnGatewayId :: Lens' VpnGateway (Maybe Text)
vvvvvvvvvvvvvvvyVpnGatewayId = lens _vvvvvvvvvvvvvvvyVpnGatewayId (\s a -> s { _vvvvvvvvvvvvvvvyVpnGatewayId = a })
{-# INLINE vvvvvvvvvvvvvvvyVpnGatewayId #-}

-- | The current state of the virtual private gateway.
vvvvvvvvvvvvvvvyState :: Lens' VpnGateway (Maybe VpnState)
vvvvvvvvvvvvvvvyState = lens _vvvvvvvvvvvvvvvyState (\s a -> s { _vvvvvvvvvvvvvvvyState = a })
{-# INLINE vvvvvvvvvvvvvvvyState #-}

-- | The type of VPN connection the virtual private gateway supports.
vvvvvvvvvvvvvvvyType :: Lens' VpnGateway (Maybe GatewayType)
vvvvvvvvvvvvvvvyType = lens _vvvvvvvvvvvvvvvyType (\s a -> s { _vvvvvvvvvvvvvvvyType = a })
{-# INLINE vvvvvvvvvvvvvvvyType #-}

-- | The Availability Zone where the virtual private gateway was created.
vvvvvvvvvvvvvvvyAvailabilityZone :: Lens' VpnGateway (Maybe Text)
vvvvvvvvvvvvvvvyAvailabilityZone = lens _vvvvvvvvvvvvvvvyAvailabilityZone (\s a -> s { _vvvvvvvvvvvvvvvyAvailabilityZone = a })
{-# INLINE vvvvvvvvvvvvvvvyAvailabilityZone #-}

-- | Any VPCs attached to the virtual private gateway.
vvvvvvvvvvvvvvvyVpcAttachments :: Lens' VpnGateway ([VpcAttachment])
vvvvvvvvvvvvvvvyVpcAttachments = lens _vvvvvvvvvvvvvvvyVpcAttachments (\s a -> s { _vvvvvvvvvvvvvvvyVpcAttachments = a })
{-# INLINE vvvvvvvvvvvvvvvyVpcAttachments #-}

-- | Any tags assigned to the virtual private gateway.
vvvvvvvvvvvvvvvyTags :: Lens' VpnGateway ([Tag])
vvvvvvvvvvvvvvvyTags = lens _vvvvvvvvvvvvvvvyTags (\s a -> s { _vvvvvvvvvvvvvvvyTags = a })
{-# INLINE vvvvvvvvvvvvvvvyTags #-}

instance FromXML VpnGateway where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "vpnGateway"

-- | Describes a static route for a VPN connection.
data VpnStaticRoute = VpnStaticRoute
    { _vsrDestinationCidrBlock :: Maybe Text
      -- ^ The CIDR block associated with the local subnet of the customer
      -- data center.
    , _vsrSource :: Maybe VpnStaticRouteSource
      -- ^ Indicates how the routes were provided.
    , _vsrState :: Maybe VpnState
      -- ^ The current state of the static route.
    } deriving (Show, Generic)

-- | The CIDR block associated with the local subnet of the customer data
-- center.
vsrDestinationCidrBlock :: Lens' VpnStaticRoute (Maybe Text)
vsrDestinationCidrBlock = lens _vsrDestinationCidrBlock (\s a -> s { _vsrDestinationCidrBlock = a })
{-# INLINE vsrDestinationCidrBlock #-}

-- | Indicates how the routes were provided.
vsrSource :: Lens' VpnStaticRoute (Maybe VpnStaticRouteSource)
vsrSource = lens _vsrSource (\s a -> s { _vsrSource = a })
{-# INLINE vsrSource #-}

-- | The current state of the static route.
vsrState :: Lens' VpnStaticRoute (Maybe VpnState)
vsrState = lens _vsrState (\s a -> s { _vsrState = a })
{-# INLINE vsrState #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VpnStaticRoute' data type to populate a request.
mkVpnStaticRoute :: VpnStaticRoute
mkVpnStaticRoute = VpnStaticRoute
    { _vsrDestinationCidrBlock = Nothing
    , _vsrSource = Nothing
    , _vsrState = Nothing
    }
{-# INLINE mkVpnStaticRoute #-}

instance FromXML VpnStaticRoute where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery VpnStaticRoute where
    toQuery = genericQuery def
