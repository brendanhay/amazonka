{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
    , AccountAttributeValue (..)
    , aavAttributeValue

    -- * AttributeBooleanValue
    , AttributeBooleanValue (..)
    , abvValue

    -- * AttributeValue
    , AttributeValue (..)
    , axValue

    -- * AvailabilityZoneMessage
    , AvailabilityZoneMessage (..)
    , azmMessage

    -- * IpRange
    , IpRange (..)
    , iuCidrIp

    -- * Monitoring
    , Monitoring (..)
    , mgState

    -- * PropagatingVgw
    , PropagatingVgw (..)
    , pwGatewayId

    -- * ReservedInstancesId
    , ReservedInstancesId (..)
    , rijReservedInstancesId

    -- * RunInstancesMonitoringEnabled
    , RunInstancesMonitoringEnabled (..)
    , rimeEnabled

    -- * Storage
    , Storage (..)
    , seS3

    -- * VolumeDetail
    , VolumeDetail (..)
    , vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvxSize

    -- * VpnConnectionOptions
    , VpnConnectionOptions (..)
    , vcoStaticRoutesOnly

    -- * VpnConnectionOptionsSpecification
    , VpnConnectionOptionsSpecification (..)
    , vcosStaticRoutesOnly

    -- * AccountAttribute
    , AccountAttribute (..)
    , aaAttributeName
    , aaAttributeValues

    -- * Address
    , Address (..)
    , awInstanceId
    , awPublicIp
    , awAllocationId
    , awAssociationId
    , awDomain
    , awNetworkInterfaceId
    , awNetworkInterfaceOwnerId
    , awPrivateIpAddress

    -- * AvailabilityZone
    , AvailabilityZone (..)
    , azZoneName
    , azState
    , azRegionName
    , azMessages

    -- * BlockDeviceMapping
    , BlockDeviceMapping (..)
    , bdmVirtualName
    , bdmDeviceName
    , bdmEbs
    , bdmNoDevice

    -- * BundleTask
    , BundleTask (..)
    , btInstanceId
    , btBundleId
    , btState
    , btStartTime
    , btUpdateTime
    , btStorage
    , btProgress
    , btBundleTaskError

    -- * BundleTaskError
    , BundleTaskError (..)
    , bteCode
    , bteMessage

    -- * CancelledSpotInstanceRequest
    , CancelledSpotInstanceRequest (..)
    , csirSpotInstanceRequestId
    , csirState

    -- * ConversionTask
    , ConversionTask (..)
    , ctConversionTaskId
    , ctExpirationTime
    , ctImportInstance
    , ctImportVolume
    , ctState
    , ctStatusMessage
    , ctTags

    -- * CreateVolumePermission
    , CreateVolumePermission (..)
    , cvpUserId
    , cvpGroup

    -- * CreateVolumePermissionModifications
    , CreateVolumePermissionModifications (..)
    , cvpmAdd
    , cvpmRemove

    -- * CustomerGateway
    , CustomerGateway (..)
    , cgCustomerGatewayId
    , cgState
    , cgType
    , cgIpAddress
    , cgBgpAsn
    , cgTags

    -- * DhcpConfiguration
    , DhcpConfiguration (..)
    , dcKey
    , dcValues

    -- * DhcpOptions
    , DhcpOptions (..)
    , doDhcpOptionsId
    , doDhcpConfigurations
    , doTags

    -- * DiskImage
    , DiskImage (..)
    , dmImage
    , dmDescription
    , dmVolume

    -- * DiskImageDescription
    , DiskImageDescription (..)
    , didFormat
    , didSize
    , didImportManifestUrl
    , didChecksum

    -- * DiskImageDetail
    , DiskImageDetail (..)
    , dikFormat
    , dikBytes
    , dikImportManifestUrl

    -- * DiskImageVolumeDescription
    , DiskImageVolumeDescription (..)
    , divdSize
    , divdId

    -- * EbsBlockDevice
    , EbsBlockDevice (..)
    , ebdSnapshotId
    , ebdVolumeSize
    , ebdDeleteOnTermination
    , ebdVolumeType
    , ebdIops
    , ebdEncrypted

    -- * EbsInstanceBlockDevice
    , EbsInstanceBlockDevice (..)
    , eibdVolumeId
    , eibdStatus
    , eibdAttachTime
    , eibdDeleteOnTermination

    -- * EbsInstanceBlockDeviceSpecification
    , EbsInstanceBlockDeviceSpecification (..)
    , eibdsVolumeId
    , eibdsDeleteOnTermination

    -- * ExportTask
    , ExportTask (..)
    , etExportTaskId
    , etDescription
    , etState
    , etStatusMessage
    , etInstanceExportDetails
    , etExportToS3Task

    -- * ExportToS3Task
    , ExportToS3Task (..)
    , etstDiskImageFormat
    , etstContainerFormat
    , etstS3Bucket
    , etstS3Key

    -- * ExportToS3TaskSpecification
    , ExportToS3TaskSpecification (..)
    , etstsDiskImageFormat
    , etstsContainerFormat
    , etstsS3Bucket
    , etstsS3Prefix

    -- * Filter
    , Filter (..)
    , frName
    , frValues

    -- * GroupIdentifier
    , GroupIdentifier (..)
    , giGroupName
    , giGroupId

    -- * IamInstanceProfile
    , IamInstanceProfile (..)
    , iipArn
    , iipId

    -- * IamInstanceProfileSpecification
    , IamInstanceProfileSpecification (..)
    , iipsArn
    , iipsName

    -- * IcmpTypeCode
    , IcmpTypeCode (..)
    , itcType
    , itcCode

    -- * Image
    , Image (..)
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
    , ImportInstanceLaunchSpecification (..)
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
    , ImportInstanceTaskDetails (..)
    , iitdVolumes
    , iitdInstanceId
    , iitdPlatform
    , iitdDescription

    -- * ImportInstanceVolumeDetailItem
    , ImportInstanceVolumeDetailItem (..)
    , iivdiBytesConverted
    , iivdiAvailabilityZone
    , iivdiImage
    , iivdiVolume
    , iivdiStatus
    , iivdiStatusMessage
    , iivdiDescription

    -- * ImportVolumeTaskDetails
    , ImportVolumeTaskDetails (..)
    , ivtdBytesConverted
    , ivtdAvailabilityZone
    , ivtdDescription
    , ivtdImage
    , ivtdVolume

    -- * Instance
    , Instance (..)
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
    , InstanceBlockDeviceMapping (..)
    , ibdmDeviceName
    , ibdmEbs

    -- * InstanceBlockDeviceMappingSpecification
    , InstanceBlockDeviceMappingSpecification (..)
    , ibdmsDeviceName
    , ibdmsEbs
    , ibdmsVirtualName
    , ibdmsNoDevice

    -- * InstanceCount
    , InstanceCount (..)
    , icState
    , icInstanceCount

    -- * InstanceExportDetails
    , InstanceExportDetails (..)
    , iedInstanceId
    , iedTargetEnvironment

    -- * InstanceMonitoring
    , InstanceMonitoring (..)
    , inInstanceId
    , inMonitoring

    -- * InstanceNetworkInterface
    , InstanceNetworkInterface (..)
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
    , InstanceNetworkInterfaceAssociation (..)
    , inibPublicIp
    , inibPublicDnsName
    , inibIpOwnerId

    -- * InstanceNetworkInterfaceAttachment
    , InstanceNetworkInterfaceAttachment (..)
    , iniaAttachmentId
    , iniaDeviceIndex
    , iniaStatus
    , iniaAttachTime
    , iniaDeleteOnTermination

    -- * InstanceNetworkInterfaceSpecification
    , InstanceNetworkInterfaceSpecification (..)
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
    , InstancePrivateIpAddress (..)
    , ipiaPrivateIpAddress
    , ipiaPrivateDnsName
    , ipiaPrimary
    , ipiaAssociation

    -- * InstanceState
    , InstanceState (..)
    , iifCode
    , iifName

    -- * InstanceStateChange
    , InstanceStateChange (..)
    , iscInstanceId
    , iscCurrentState
    , iscPreviousState

    -- * InstanceStatus
    , InstanceStatus (..)
    , iiiiivInstanceId
    , iiiiivAvailabilityZone
    , iiiiivEvents
    , iiiiivInstanceState
    , iiiiivSystemStatus
    , iiiiivInstanceStatus

    -- * InstanceStatusDetails
    , InstanceStatusDetails (..)
    , isdName
    , isdStatus
    , isdImpairedSince

    -- * InstanceStatusEvent
    , InstanceStatusEvent (..)
    , iseCode
    , iseDescription
    , iseNotBefore
    , iseNotAfter

    -- * InstanceStatusSummary
    , InstanceStatusSummary (..)
    , issStatus
    , issDetails

    -- * InternetGateway
    , InternetGateway (..)
    , igInternetGatewayId
    , igAttachments
    , igTags

    -- * InternetGatewayAttachment
    , InternetGatewayAttachment (..)
    , igaVpcId
    , igaState

    -- * IpPermission
    , IpPermission (..)
    , ipIpProtocol
    , ipFromPort
    , ipToPort
    , ipUserIdGroupPairs
    , ipIpRanges

    -- * KeyPairInfo
    , KeyPairInfo (..)
    , kpiKeyName
    , kpiKeyFingerprint

    -- * LaunchPermission
    , LaunchPermission (..)
    , lpUserId
    , lpGroup

    -- * LaunchPermissionModifications
    , LaunchPermissionModifications (..)
    , lpmAdd
    , lpmRemove

    -- * LaunchSpecification
    , LaunchSpecification (..)
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
    , NetworkAcl (..)
    , naNetworkAclId
    , naVpcId
    , naIsDefault
    , naEntries
    , naAssociations
    , naTags

    -- * NetworkAclAssociation
    , NetworkAclAssociation (..)
    , naaNetworkAclAssociationId
    , naaNetworkAclId
    , naaSubnetId

    -- * NetworkAclEntry
    , NetworkAclEntry (..)
    , naeRuleNumber
    , naeProtocol
    , naeRuleAction
    , naeEgress
    , naeCidrBlock
    , naeIcmpTypeCode
    , naePortRange

    -- * NetworkInterface
    , NetworkInterface (..)
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
    , NetworkInterfaceAssociation (..)
    , nibPublicIp
    , nibPublicDnsName
    , nibIpOwnerId
    , nibAllocationId
    , nibAssociationId

    -- * NetworkInterfaceAttachment
    , NetworkInterfaceAttachment (..)
    , niaAttachmentId
    , niaInstanceId
    , niaInstanceOwnerId
    , niaDeviceIndex
    , niaStatus
    , niaAttachTime
    , niaDeleteOnTermination

    -- * NetworkInterfaceAttachmentChanges
    , NetworkInterfaceAttachmentChanges (..)
    , niacAttachmentId
    , niacDeleteOnTermination

    -- * NetworkInterfacePrivateIpAddress
    , NetworkInterfacePrivateIpAddress (..)
    , nipiaPrivateIpAddress
    , nipiaPrivateDnsName
    , nipiaPrimary
    , nipiaAssociation

    -- * Placement
    , Placement (..)
    , pzAvailabilityZone
    , pzGroupName
    , pzTenancy

    -- * PlacementGroup
    , PlacementGroup (..)
    , phGroupName
    , phStrategy
    , phState

    -- * PortRange
    , PortRange (..)
    , prFrom
    , prTo

    -- * PriceSchedule
    , PriceSchedule (..)
    , psTerm
    , psPrice
    , psCurrencyCode
    , psActive

    -- * PriceScheduleSpecification
    , PriceScheduleSpecification (..)
    , pssTerm
    , pssPrice
    , pssCurrencyCode

    -- * PricingDetail
    , PricingDetail (..)
    , piPrice
    , piCount

    -- * PrivateIpAddressSpecification
    , PrivateIpAddressSpecification (..)
    , piasPrivateIpAddress
    , piasPrimary

    -- * ProductCode
    , ProductCode (..)
    , pcProductCodeId
    , pcProductCodeType

    -- * RecurringCharge
    , RecurringCharge (..)
    , rdFrequency
    , rdAmount

    -- * Region
    , Region (..)
    , rqRegionName
    , rqEndpoint

    -- * Reservation
    , Reservation (..)
    , rnReservationId
    , rnOwnerId
    , rnRequesterId
    , rnGroups
    , rnInstances

    -- * ReservedInstanceLimitPrice
    , ReservedInstanceLimitPrice (..)
    , rilpAmount
    , rilpCurrencyCode

    -- * ReservedInstances
    , ReservedInstances (..)
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
    , ReservedInstancesConfiguration (..)
    , ricAvailabilityZone
    , ricPlatform
    , ricInstanceCount
    , ricInstanceType

    -- * ReservedInstancesListing
    , ReservedInstancesListing (..)
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
    , ReservedInstancesModification (..)
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
    , ReservedInstancesModificationResult (..)
    , rimrReservedInstancesId
    , rimrTargetConfiguration

    -- * ReservedInstancesOffering
    , ReservedInstancesOffering (..)
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
    , Route (..)
    , reDestinationCidrBlock
    , reGatewayId
    , reInstanceId
    , reInstanceOwnerId
    , reNetworkInterfaceId
    , reVpcPeeringConnectionId
    , reState
    , reOrigin

    -- * RouteTable
    , RouteTable (..)
    , rtRouteTableId
    , rtVpcId
    , rtRoutes
    , rtAssociations
    , rtTags
    , rtPropagatingVgws

    -- * RouteTableAssociation
    , RouteTableAssociation (..)
    , rtaRouteTableAssociationId
    , rtaRouteTableId
    , rtaSubnetId
    , rtaMain

    -- * S3Storage
    , S3Storage (..)
    , ssBucket
    , ssPrefix
    , ssAWSAccessKeyId
    , ssUploadPolicy
    , ssUploadPolicySignature

    -- * SecurityGroup
    , SecurityGroup (..)
    , siOwnerId
    , siGroupName
    , siGroupId
    , siDescription
    , siIpPermissions
    , siIpPermissionsEgress
    , siVpcId
    , siTags

    -- * Snapshot
    , Snapshot (..)
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
    , SpotDatafeedSubscription (..)
    , sdsOwnerId
    , sdsBucket
    , sdsPrefix
    , sdsState
    , sdsFault

    -- * SpotInstanceRequest
    , SpotInstanceRequest (..)
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
    , SpotInstanceStateFault (..)
    , sisfCode
    , sisfMessage

    -- * SpotInstanceStatus
    , SpotInstanceStatus (..)
    , siuCode
    , siuUpdateTime
    , siuMessage

    -- * SpotPlacement
    , SpotPlacement (..)
    , spAvailabilityZone
    , spGroupName

    -- * SpotPrice
    , SpotPrice (..)
    , sqInstanceType
    , sqProductDescription
    , sqSpotPrice
    , sqTimestamp
    , sqAvailabilityZone

    -- * StateReason
    , StateReason (..)
    , srCode
    , srMessage

    -- * Subnet
    , Subnet (..)
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
    , Tag (..)
    , tgKey
    , tgValue

    -- * TagDescription
    , TagDescription (..)
    , tdResourceId
    , tdResourceType
    , tdKey
    , tdValue

    -- * UserIdGroupPair
    , UserIdGroupPair (..)
    , uigpUserId
    , uigpGroupName
    , uigpGroupId

    -- * VgwTelemetry
    , VgwTelemetry (..)
    , vvvvvvvvvvvvvvyOutsideIpAddress
    , vvvvvvvvvvvvvvyStatus
    , vvvvvvvvvvvvvvyLastStatusChange
    , vvvvvvvvvvvvvvyStatusMessage
    , vvvvvvvvvvvvvvyAcceptedRouteCount

    -- * Volume
    , Volume (..)
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
    , VolumeAttachment (..)
    , vcVolumeId
    , vcInstanceId
    , vcDevice
    , vcState
    , vcAttachTime
    , vcDeleteOnTermination

    -- * VolumeStatusAction
    , VolumeStatusAction (..)
    , vsaCode
    , vsaDescription
    , vsaEventType
    , vsaEventId

    -- * VolumeStatusDetails
    , VolumeStatusDetails (..)
    , vsdName
    , vsdStatus

    -- * VolumeStatusEvent
    , VolumeStatusEvent (..)
    , vseEventType
    , vseDescription
    , vseNotBefore
    , vseNotAfter
    , vseEventId

    -- * VolumeStatusInfo
    , VolumeStatusInfo (..)
    , vsjStatus
    , vsjDetails

    -- * VolumeStatusItem
    , VolumeStatusItem (..)
    , vsiVolumeId
    , vsiAvailabilityZone
    , vsiVolumeStatus
    , vsiEvents
    , vsiActions

    -- * Vpc
    , Vpc (..)
    , vdVpcId
    , vdState
    , vdCidrBlock
    , vdDhcpOptionsId
    , vdTags
    , vdInstanceTenancy
    , vdIsDefault

    -- * VpcAttachment
    , VpcAttachment (..)
    , vbVpcId
    , vbState

    -- * VpcPeeringConnection
    , VpcPeeringConnection (..)
    , vpcAccepterVpcInfo
    , vpcExpirationTime
    , vpcRequesterVpcInfo
    , vpcStatus
    , vpcTags
    , vpcVpcPeeringConnectionId

    -- * VpcPeeringConnectionStateReason
    , VpcPeeringConnectionStateReason (..)
    , vpcsrCode
    , vpcsrMessage

    -- * VpcPeeringConnectionVpcInfo
    , VpcPeeringConnectionVpcInfo (..)
    , vpcviCidrBlock
    , vpcviOwnerId
    , vpcviVpcId

    -- * VpnConnection
    , VpnConnection (..)
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
    , VpnGateway (..)
    , vvvvvvvvvvvvvvvyVpnGatewayId
    , vvvvvvvvvvvvvvvyState
    , vvvvvvvvvvvvvvvyType
    , vvvvvvvvvvvvvvvyAvailabilityZone
    , vvvvvvvvvvvvvvvyVpcAttachments
    , vvvvvvvvvvvvvvvyTags

    -- * VpnStaticRoute
    , VpnStaticRoute (..)
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
aavAttributeValue
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AccountAttributeValue
    -> f AccountAttributeValue
aavAttributeValue f x =
    (\y -> x { _aavAttributeValue = y })
       <$> f (_aavAttributeValue x)
{-# INLINE aavAttributeValue #-}

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
abvValue
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> AttributeBooleanValue
    -> f AttributeBooleanValue
abvValue f x =
    (\y -> x { _abvValue = y })
       <$> f (_abvValue x)
{-# INLINE abvValue #-}

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
axValue
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AttributeValue
    -> f AttributeValue
axValue f x =
    (\y -> x { _axValue = y })
       <$> f (_axValue x)
{-# INLINE axValue #-}

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
azmMessage
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AvailabilityZoneMessage
    -> f AvailabilityZoneMessage
azmMessage f x =
    (\y -> x { _azmMessage = y })
       <$> f (_azmMessage x)
{-# INLINE azmMessage #-}

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
iuCidrIp
    :: Functor f
    => (Text
    -> f (Text))
    -> IpRange
    -> f IpRange
iuCidrIp f x =
    (\y -> x { _iuCidrIp = y })
       <$> f (_iuCidrIp x)
{-# INLINE iuCidrIp #-}

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
mgState
    :: Functor f
    => (Maybe MonitoringState
    -> f (Maybe MonitoringState))
    -> Monitoring
    -> f Monitoring
mgState f x =
    (\y -> x { _mgState = y })
       <$> f (_mgState x)
{-# INLINE mgState #-}

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
pwGatewayId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PropagatingVgw
    -> f PropagatingVgw
pwGatewayId f x =
    (\y -> x { _pwGatewayId = y })
       <$> f (_pwGatewayId x)
{-# INLINE pwGatewayId #-}

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
rijReservedInstancesId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedInstancesId
    -> f ReservedInstancesId
rijReservedInstancesId f x =
    (\y -> x { _rijReservedInstancesId = y })
       <$> f (_rijReservedInstancesId x)
{-# INLINE rijReservedInstancesId #-}

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
rimeEnabled
    :: Functor f
    => (Bool
    -> f (Bool))
    -> RunInstancesMonitoringEnabled
    -> f RunInstancesMonitoringEnabled
rimeEnabled f x =
    (\y -> x { _rimeEnabled = y })
       <$> f (_rimeEnabled x)
{-# INLINE rimeEnabled #-}

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
seS3
    :: Functor f
    => (Maybe S3Storage
    -> f (Maybe S3Storage))
    -> Storage
    -> f Storage
seS3 f x =
    (\y -> x { _seS3 = y })
       <$> f (_seS3 x)
{-# INLINE seS3 #-}

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
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvxSize
    :: Functor f
    => (Integer
    -> f (Integer))
    -> VolumeDetail
    -> f VolumeDetail
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvxSize f x =
    (\y -> x { _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvxSize = y })
       <$> f (_vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvxSize x)
{-# INLINE vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvxSize #-}

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
vcoStaticRoutesOnly
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> VpnConnectionOptions
    -> f VpnConnectionOptions
vcoStaticRoutesOnly f x =
    (\y -> x { _vcoStaticRoutesOnly = y })
       <$> f (_vcoStaticRoutesOnly x)
{-# INLINE vcoStaticRoutesOnly #-}

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
vcosStaticRoutesOnly
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> VpnConnectionOptionsSpecification
    -> f VpnConnectionOptionsSpecification
vcosStaticRoutesOnly f x =
    (\y -> x { _vcosStaticRoutesOnly = y })
       <$> f (_vcosStaticRoutesOnly x)
{-# INLINE vcosStaticRoutesOnly #-}

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
aaAttributeName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AccountAttribute
    -> f AccountAttribute
aaAttributeName f x =
    (\y -> x { _aaAttributeName = y })
       <$> f (_aaAttributeName x)
{-# INLINE aaAttributeName #-}

-- | One or more values for the account attribute.
aaAttributeValues
    :: Functor f
    => ([AccountAttributeValue]
    -> f ([AccountAttributeValue]))
    -> AccountAttribute
    -> f AccountAttribute
aaAttributeValues f x =
    (\y -> x { _aaAttributeValues = y })
       <$> f (_aaAttributeValues x)
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
awInstanceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Address
    -> f Address
awInstanceId f x =
    (\y -> x { _awInstanceId = y })
       <$> f (_awInstanceId x)
{-# INLINE awInstanceId #-}

-- | The Elastic IP address.
awPublicIp
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Address
    -> f Address
awPublicIp f x =
    (\y -> x { _awPublicIp = y })
       <$> f (_awPublicIp x)
{-# INLINE awPublicIp #-}

-- | The ID representing the allocation of the address for use with EC2-VPC.
awAllocationId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Address
    -> f Address
awAllocationId f x =
    (\y -> x { _awAllocationId = y })
       <$> f (_awAllocationId x)
{-# INLINE awAllocationId #-}

-- | The ID representing the association of the address with an instance in a
-- VPC.
awAssociationId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Address
    -> f Address
awAssociationId f x =
    (\y -> x { _awAssociationId = y })
       <$> f (_awAssociationId x)
{-# INLINE awAssociationId #-}

-- | Indicates whether this Elastic IP address is for use with instances in
-- EC2-Classic (standard) or instances in a VPC (vpc).
awDomain
    :: Functor f
    => (Maybe DomainType
    -> f (Maybe DomainType))
    -> Address
    -> f Address
awDomain f x =
    (\y -> x { _awDomain = y })
       <$> f (_awDomain x)
{-# INLINE awDomain #-}

-- | The ID of the network interface.
awNetworkInterfaceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Address
    -> f Address
awNetworkInterfaceId f x =
    (\y -> x { _awNetworkInterfaceId = y })
       <$> f (_awNetworkInterfaceId x)
{-# INLINE awNetworkInterfaceId #-}

-- | The ID of the AWS account that owns the network interface.
awNetworkInterfaceOwnerId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Address
    -> f Address
awNetworkInterfaceOwnerId f x =
    (\y -> x { _awNetworkInterfaceOwnerId = y })
       <$> f (_awNetworkInterfaceOwnerId x)
{-# INLINE awNetworkInterfaceOwnerId #-}

-- | The private IP address associated with the Elastic IP address.
awPrivateIpAddress
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Address
    -> f Address
awPrivateIpAddress f x =
    (\y -> x { _awPrivateIpAddress = y })
       <$> f (_awPrivateIpAddress x)
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
azZoneName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AvailabilityZone
    -> f AvailabilityZone
azZoneName f x =
    (\y -> x { _azZoneName = y })
       <$> f (_azZoneName x)
{-# INLINE azZoneName #-}

-- | The state of the Availability Zone.
azState
    :: Functor f
    => (Maybe AvailabilityZoneState
    -> f (Maybe AvailabilityZoneState))
    -> AvailabilityZone
    -> f AvailabilityZone
azState f x =
    (\y -> x { _azState = y })
       <$> f (_azState x)
{-# INLINE azState #-}

-- | The name of the region.
azRegionName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AvailabilityZone
    -> f AvailabilityZone
azRegionName f x =
    (\y -> x { _azRegionName = y })
       <$> f (_azRegionName x)
{-# INLINE azRegionName #-}

-- | Any messages about the Availability Zone.
azMessages
    :: Functor f
    => ([AvailabilityZoneMessage]
    -> f ([AvailabilityZoneMessage]))
    -> AvailabilityZone
    -> f AvailabilityZone
azMessages f x =
    (\y -> x { _azMessages = y })
       <$> f (_azMessages x)
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
bdmVirtualName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> BlockDeviceMapping
    -> f BlockDeviceMapping
bdmVirtualName f x =
    (\y -> x { _bdmVirtualName = y })
       <$> f (_bdmVirtualName x)
{-# INLINE bdmVirtualName #-}

-- | The device name exposed to the instance (for example, /dev/sdh).
bdmDeviceName
    :: Functor f
    => (Text
    -> f (Text))
    -> BlockDeviceMapping
    -> f BlockDeviceMapping
bdmDeviceName f x =
    (\y -> x { _bdmDeviceName = y })
       <$> f (_bdmDeviceName x)
{-# INLINE bdmDeviceName #-}

-- | Parameters used to automatically set up Amazon EBS volumes when the
-- instance is launched.
bdmEbs
    :: Functor f
    => (Maybe EbsBlockDevice
    -> f (Maybe EbsBlockDevice))
    -> BlockDeviceMapping
    -> f BlockDeviceMapping
bdmEbs f x =
    (\y -> x { _bdmEbs = y })
       <$> f (_bdmEbs x)
{-# INLINE bdmEbs #-}

-- | Suppresses the specified device included in the block device mapping of the
-- AMI.
bdmNoDevice
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> BlockDeviceMapping
    -> f BlockDeviceMapping
bdmNoDevice f x =
    (\y -> x { _bdmNoDevice = y })
       <$> f (_bdmNoDevice x)
{-# INLINE bdmNoDevice #-}

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
btInstanceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> BundleTask
    -> f BundleTask
btInstanceId f x =
    (\y -> x { _btInstanceId = y })
       <$> f (_btInstanceId x)
{-# INLINE btInstanceId #-}

-- | The ID for this bundle task.
btBundleId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> BundleTask
    -> f BundleTask
btBundleId f x =
    (\y -> x { _btBundleId = y })
       <$> f (_btBundleId x)
{-# INLINE btBundleId #-}

-- | The state of the task.
btState
    :: Functor f
    => (Maybe BundleTaskState
    -> f (Maybe BundleTaskState))
    -> BundleTask
    -> f BundleTask
btState f x =
    (\y -> x { _btState = y })
       <$> f (_btState x)
{-# INLINE btState #-}

-- | The time this task started.
btStartTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> BundleTask
    -> f BundleTask
btStartTime f x =
    (\y -> x { _btStartTime = y })
       <$> f (_btStartTime x)
{-# INLINE btStartTime #-}

-- | The time of the most recent update for the task.
btUpdateTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> BundleTask
    -> f BundleTask
btUpdateTime f x =
    (\y -> x { _btUpdateTime = y })
       <$> f (_btUpdateTime x)
{-# INLINE btUpdateTime #-}

-- | The Amazon S3 storage locations.
btStorage
    :: Functor f
    => (Maybe Storage
    -> f (Maybe Storage))
    -> BundleTask
    -> f BundleTask
btStorage f x =
    (\y -> x { _btStorage = y })
       <$> f (_btStorage x)
{-# INLINE btStorage #-}

-- | The level of task completion, as a percent (for example, 20%).
btProgress
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> BundleTask
    -> f BundleTask
btProgress f x =
    (\y -> x { _btProgress = y })
       <$> f (_btProgress x)
{-# INLINE btProgress #-}

-- | If the task fails, a description of the error.
btBundleTaskError
    :: Functor f
    => (Maybe BundleTaskError
    -> f (Maybe BundleTaskError))
    -> BundleTask
    -> f BundleTask
btBundleTaskError f x =
    (\y -> x { _btBundleTaskError = y })
       <$> f (_btBundleTaskError x)
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
bteCode
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> BundleTaskError
    -> f BundleTaskError
bteCode f x =
    (\y -> x { _bteCode = y })
       <$> f (_bteCode x)
{-# INLINE bteCode #-}

-- | The error message.
bteMessage
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> BundleTaskError
    -> f BundleTaskError
bteMessage f x =
    (\y -> x { _bteMessage = y })
       <$> f (_bteMessage x)
{-# INLINE bteMessage #-}

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
csirSpotInstanceRequestId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CancelledSpotInstanceRequest
    -> f CancelledSpotInstanceRequest
csirSpotInstanceRequestId f x =
    (\y -> x { _csirSpotInstanceRequestId = y })
       <$> f (_csirSpotInstanceRequestId x)
{-# INLINE csirSpotInstanceRequestId #-}

-- | The state of the Spot Instance request.
csirState
    :: Functor f
    => (Maybe CancelSpotInstanceRequestState
    -> f (Maybe CancelSpotInstanceRequestState))
    -> CancelledSpotInstanceRequest
    -> f CancelledSpotInstanceRequest
csirState f x =
    (\y -> x { _csirState = y })
       <$> f (_csirState x)
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
ctConversionTaskId
    :: Functor f
    => (Text
    -> f (Text))
    -> ConversionTask
    -> f ConversionTask
ctConversionTaskId f x =
    (\y -> x { _ctConversionTaskId = y })
       <$> f (_ctConversionTaskId x)
{-# INLINE ctConversionTaskId #-}

-- | The time when the task expires. If the upload isn't complete before the
-- expiration time, we automatically cancel the task.
ctExpirationTime
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ConversionTask
    -> f ConversionTask
ctExpirationTime f x =
    (\y -> x { _ctExpirationTime = y })
       <$> f (_ctExpirationTime x)
{-# INLINE ctExpirationTime #-}

-- | If the task is for importing an instance, this contains information about
-- the import instance task.
ctImportInstance
    :: Functor f
    => (Maybe ImportInstanceTaskDetails
    -> f (Maybe ImportInstanceTaskDetails))
    -> ConversionTask
    -> f ConversionTask
ctImportInstance f x =
    (\y -> x { _ctImportInstance = y })
       <$> f (_ctImportInstance x)
{-# INLINE ctImportInstance #-}

-- | If the task is for importing a volume, this contains information about the
-- import volume task.
ctImportVolume
    :: Functor f
    => (Maybe ImportVolumeTaskDetails
    -> f (Maybe ImportVolumeTaskDetails))
    -> ConversionTask
    -> f ConversionTask
ctImportVolume f x =
    (\y -> x { _ctImportVolume = y })
       <$> f (_ctImportVolume x)
{-# INLINE ctImportVolume #-}

-- | The state of the conversion task.
ctState
    :: Functor f
    => (ConversionTaskState
    -> f (ConversionTaskState))
    -> ConversionTask
    -> f ConversionTask
ctState f x =
    (\y -> x { _ctState = y })
       <$> f (_ctState x)
{-# INLINE ctState #-}

-- | The status message related to the conversion task.
ctStatusMessage
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ConversionTask
    -> f ConversionTask
ctStatusMessage f x =
    (\y -> x { _ctStatusMessage = y })
       <$> f (_ctStatusMessage x)
{-# INLINE ctStatusMessage #-}

-- | 
ctTags
    :: Functor f
    => ([Tag]
    -> f ([Tag]))
    -> ConversionTask
    -> f ConversionTask
ctTags f x =
    (\y -> x { _ctTags = y })
       <$> f (_ctTags x)
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
cvpUserId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateVolumePermission
    -> f CreateVolumePermission
cvpUserId f x =
    (\y -> x { _cvpUserId = y })
       <$> f (_cvpUserId x)
{-# INLINE cvpUserId #-}

-- | The specific group that is to be added or removed from a volume's list of
-- create volume permissions.
cvpGroup
    :: Functor f
    => (Maybe PermissionGroup
    -> f (Maybe PermissionGroup))
    -> CreateVolumePermission
    -> f CreateVolumePermission
cvpGroup f x =
    (\y -> x { _cvpGroup = y })
       <$> f (_cvpGroup x)
{-# INLINE cvpGroup #-}

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
cvpmAdd
    :: Functor f
    => ([CreateVolumePermission]
    -> f ([CreateVolumePermission]))
    -> CreateVolumePermissionModifications
    -> f CreateVolumePermissionModifications
cvpmAdd f x =
    (\y -> x { _cvpmAdd = y })
       <$> f (_cvpmAdd x)
{-# INLINE cvpmAdd #-}

-- | Removes a specific AWS account ID or group from a volume's list of create
-- volume permissions.
cvpmRemove
    :: Functor f
    => ([CreateVolumePermission]
    -> f ([CreateVolumePermission]))
    -> CreateVolumePermissionModifications
    -> f CreateVolumePermissionModifications
cvpmRemove f x =
    (\y -> x { _cvpmRemove = y })
       <$> f (_cvpmRemove x)
{-# INLINE cvpmRemove #-}

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
cgCustomerGatewayId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CustomerGateway
    -> f CustomerGateway
cgCustomerGatewayId f x =
    (\y -> x { _cgCustomerGatewayId = y })
       <$> f (_cgCustomerGatewayId x)
{-# INLINE cgCustomerGatewayId #-}

-- | The current state of the customer gateway.
cgState
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CustomerGateway
    -> f CustomerGateway
cgState f x =
    (\y -> x { _cgState = y })
       <$> f (_cgState x)
{-# INLINE cgState #-}

-- | The type of VPN connection the customer gateway supports.
cgType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CustomerGateway
    -> f CustomerGateway
cgType f x =
    (\y -> x { _cgType = y })
       <$> f (_cgType x)
{-# INLINE cgType #-}

-- | The Internet-routable IP address of the customer gateway's outside
-- interface.
cgIpAddress
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CustomerGateway
    -> f CustomerGateway
cgIpAddress f x =
    (\y -> x { _cgIpAddress = y })
       <$> f (_cgIpAddress x)
{-# INLINE cgIpAddress #-}

-- | The customer gateway's Border Gateway Protocol (BGP) Autonomous System
-- Number (ASN).
cgBgpAsn
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CustomerGateway
    -> f CustomerGateway
cgBgpAsn f x =
    (\y -> x { _cgBgpAsn = y })
       <$> f (_cgBgpAsn x)
{-# INLINE cgBgpAsn #-}

-- | Any tags assigned to the customer gateway.
cgTags
    :: Functor f
    => ([Tag]
    -> f ([Tag]))
    -> CustomerGateway
    -> f CustomerGateway
cgTags f x =
    (\y -> x { _cgTags = y })
       <$> f (_cgTags x)
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
dcKey
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DhcpConfiguration
    -> f DhcpConfiguration
dcKey f x =
    (\y -> x { _dcKey = y })
       <$> f (_dcKey x)
{-# INLINE dcKey #-}

-- | One or more values for the DHCP option.
dcValues
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> DhcpConfiguration
    -> f DhcpConfiguration
dcValues f x =
    (\y -> x { _dcValues = y })
       <$> f (_dcValues x)
{-# INLINE dcValues #-}

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
doDhcpOptionsId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DhcpOptions
    -> f DhcpOptions
doDhcpOptionsId f x =
    (\y -> x { _doDhcpOptionsId = y })
       <$> f (_doDhcpOptionsId x)
{-# INLINE doDhcpOptionsId #-}

-- | One or more DHCP options in the set.
doDhcpConfigurations
    :: Functor f
    => ([DhcpConfiguration]
    -> f ([DhcpConfiguration]))
    -> DhcpOptions
    -> f DhcpOptions
doDhcpConfigurations f x =
    (\y -> x { _doDhcpConfigurations = y })
       <$> f (_doDhcpConfigurations x)
{-# INLINE doDhcpConfigurations #-}

-- | Any tags assigned to the DHCP options set.
doTags
    :: Functor f
    => ([Tag]
    -> f ([Tag]))
    -> DhcpOptions
    -> f DhcpOptions
doTags f x =
    (\y -> x { _doTags = y })
       <$> f (_doTags x)
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
dmImage
    :: Functor f
    => (Maybe DiskImageDetail
    -> f (Maybe DiskImageDetail))
    -> DiskImage
    -> f DiskImage
dmImage f x =
    (\y -> x { _dmImage = y })
       <$> f (_dmImage x)
{-# INLINE dmImage #-}

-- | 
dmDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DiskImage
    -> f DiskImage
dmDescription f x =
    (\y -> x { _dmDescription = y })
       <$> f (_dmDescription x)
{-# INLINE dmDescription #-}

-- | 
dmVolume
    :: Functor f
    => (Maybe VolumeDetail
    -> f (Maybe VolumeDetail))
    -> DiskImage
    -> f DiskImage
dmVolume f x =
    (\y -> x { _dmVolume = y })
       <$> f (_dmVolume x)
{-# INLINE dmVolume #-}

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
didFormat
    :: Functor f
    => (DiskImageFormat
    -> f (DiskImageFormat))
    -> DiskImageDescription
    -> f DiskImageDescription
didFormat f x =
    (\y -> x { _didFormat = y })
       <$> f (_didFormat x)
{-# INLINE didFormat #-}

-- | The size of the disk image.
didSize
    :: Functor f
    => (Integer
    -> f (Integer))
    -> DiskImageDescription
    -> f DiskImageDescription
didSize f x =
    (\y -> x { _didSize = y })
       <$> f (_didSize x)
{-# INLINE didSize #-}

-- | A presigned URL for the import manifest stored in Amazon S3. For
-- information about creating a presigned URL for an Amazon S3 object, read
-- the "Query String Request Authentication Alternative" section of the
-- Authenticating REST Requests topic in the Amazon Simple Storage Service
-- Developer Guide.
didImportManifestUrl
    :: Functor f
    => (Text
    -> f (Text))
    -> DiskImageDescription
    -> f DiskImageDescription
didImportManifestUrl f x =
    (\y -> x { _didImportManifestUrl = y })
       <$> f (_didImportManifestUrl x)
{-# INLINE didImportManifestUrl #-}

-- | The checksum computed for the disk image.
didChecksum
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DiskImageDescription
    -> f DiskImageDescription
didChecksum f x =
    (\y -> x { _didChecksum = y })
       <$> f (_didChecksum x)
{-# INLINE didChecksum #-}

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
dikFormat
    :: Functor f
    => (DiskImageFormat
    -> f (DiskImageFormat))
    -> DiskImageDetail
    -> f DiskImageDetail
dikFormat f x =
    (\y -> x { _dikFormat = y })
       <$> f (_dikFormat x)
{-# INLINE dikFormat #-}

-- | 
dikBytes
    :: Functor f
    => (Integer
    -> f (Integer))
    -> DiskImageDetail
    -> f DiskImageDetail
dikBytes f x =
    (\y -> x { _dikBytes = y })
       <$> f (_dikBytes x)
{-# INLINE dikBytes #-}

-- | A presigned URL for the import manifest stored in Amazon S3. For
-- information about creating a presigned URL for an Amazon S3 object, read
-- the "Query String Request Authentication Alternative" section of the
-- Authenticating REST Requests topic in the Amazon Simple Storage Service
-- Developer Guide.
dikImportManifestUrl
    :: Functor f
    => (Text
    -> f (Text))
    -> DiskImageDetail
    -> f DiskImageDetail
dikImportManifestUrl f x =
    (\y -> x { _dikImportManifestUrl = y })
       <$> f (_dikImportManifestUrl x)
{-# INLINE dikImportManifestUrl #-}

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
divdSize
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DiskImageVolumeDescription
    -> f DiskImageVolumeDescription
divdSize f x =
    (\y -> x { _divdSize = y })
       <$> f (_divdSize x)
{-# INLINE divdSize #-}

-- | The volume identifier.
divdId
    :: Functor f
    => (Text
    -> f (Text))
    -> DiskImageVolumeDescription
    -> f DiskImageVolumeDescription
divdId f x =
    (\y -> x { _divdId = y })
       <$> f (_divdId x)
{-# INLINE divdId #-}

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
ebdSnapshotId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EbsBlockDevice
    -> f EbsBlockDevice
ebdSnapshotId f x =
    (\y -> x { _ebdSnapshotId = y })
       <$> f (_ebdSnapshotId x)
{-# INLINE ebdSnapshotId #-}

-- | The size of the volume, in GiB. Constraints: If the volume type is io1, the
-- minimum size of the volume is 10 GiB; otherwise, the minimum size is 1 GiB.
-- The maximum volume size is 1024 GiB. Default: If you're creating the volume
-- from a snapshot and don't specify a volume size, the default is the
-- snapshot size.
ebdVolumeSize
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> EbsBlockDevice
    -> f EbsBlockDevice
ebdVolumeSize f x =
    (\y -> x { _ebdVolumeSize = y })
       <$> f (_ebdVolumeSize x)
{-# INLINE ebdVolumeSize #-}

-- | Indicates whether the Amazon EBS volume is deleted on instance termination.
ebdDeleteOnTermination
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> EbsBlockDevice
    -> f EbsBlockDevice
ebdDeleteOnTermination f x =
    (\y -> x { _ebdDeleteOnTermination = y })
       <$> f (_ebdDeleteOnTermination x)
{-# INLINE ebdDeleteOnTermination #-}

-- | The volume type. gp2 for General Purpose (SSD) volumes, io1 for Provisioned
-- IOPS (SSD) volumes, and standard for Magnetic volumes. Default: standard.
ebdVolumeType
    :: Functor f
    => (Maybe VolumeType
    -> f (Maybe VolumeType))
    -> EbsBlockDevice
    -> f EbsBlockDevice
ebdVolumeType f x =
    (\y -> x { _ebdVolumeType = y })
       <$> f (_ebdVolumeType x)
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
ebdIops
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> EbsBlockDevice
    -> f EbsBlockDevice
ebdIops f x =
    (\y -> x { _ebdIops = y })
       <$> f (_ebdIops x)
{-# INLINE ebdIops #-}

-- | Indicates whether the Amazon EBS volume is encrypted.
ebdEncrypted
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> EbsBlockDevice
    -> f EbsBlockDevice
ebdEncrypted f x =
    (\y -> x { _ebdEncrypted = y })
       <$> f (_ebdEncrypted x)
{-# INLINE ebdEncrypted #-}

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
eibdVolumeId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EbsInstanceBlockDevice
    -> f EbsInstanceBlockDevice
eibdVolumeId f x =
    (\y -> x { _eibdVolumeId = y })
       <$> f (_eibdVolumeId x)
{-# INLINE eibdVolumeId #-}

-- | The attachment state.
eibdStatus
    :: Functor f
    => (Maybe AttachmentStatus
    -> f (Maybe AttachmentStatus))
    -> EbsInstanceBlockDevice
    -> f EbsInstanceBlockDevice
eibdStatus f x =
    (\y -> x { _eibdStatus = y })
       <$> f (_eibdStatus x)
{-# INLINE eibdStatus #-}

-- | The time stamp when the attachment initiated.
eibdAttachTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> EbsInstanceBlockDevice
    -> f EbsInstanceBlockDevice
eibdAttachTime f x =
    (\y -> x { _eibdAttachTime = y })
       <$> f (_eibdAttachTime x)
{-# INLINE eibdAttachTime #-}

-- | Indicates whether the volume is deleted on instance termination.
eibdDeleteOnTermination
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> EbsInstanceBlockDevice
    -> f EbsInstanceBlockDevice
eibdDeleteOnTermination f x =
    (\y -> x { _eibdDeleteOnTermination = y })
       <$> f (_eibdDeleteOnTermination x)
{-# INLINE eibdDeleteOnTermination #-}

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
eibdsVolumeId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EbsInstanceBlockDeviceSpecification
    -> f EbsInstanceBlockDeviceSpecification
eibdsVolumeId f x =
    (\y -> x { _eibdsVolumeId = y })
       <$> f (_eibdsVolumeId x)
{-# INLINE eibdsVolumeId #-}

-- | Indicates whether the volume is deleted on instance termination.
eibdsDeleteOnTermination
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> EbsInstanceBlockDeviceSpecification
    -> f EbsInstanceBlockDeviceSpecification
eibdsDeleteOnTermination f x =
    (\y -> x { _eibdsDeleteOnTermination = y })
       <$> f (_eibdsDeleteOnTermination x)
{-# INLINE eibdsDeleteOnTermination #-}

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
etExportTaskId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ExportTask
    -> f ExportTask
etExportTaskId f x =
    (\y -> x { _etExportTaskId = y })
       <$> f (_etExportTaskId x)
{-# INLINE etExportTaskId #-}

-- | A description of the resource being exported.
etDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ExportTask
    -> f ExportTask
etDescription f x =
    (\y -> x { _etDescription = y })
       <$> f (_etDescription x)
{-# INLINE etDescription #-}

-- | The state of the conversion task.
etState
    :: Functor f
    => (Maybe ExportTaskState
    -> f (Maybe ExportTaskState))
    -> ExportTask
    -> f ExportTask
etState f x =
    (\y -> x { _etState = y })
       <$> f (_etState x)
{-# INLINE etState #-}

-- | The status message related to the export task.
etStatusMessage
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ExportTask
    -> f ExportTask
etStatusMessage f x =
    (\y -> x { _etStatusMessage = y })
       <$> f (_etStatusMessage x)
{-# INLINE etStatusMessage #-}

-- | The instance being exported.
etInstanceExportDetails
    :: Functor f
    => (Maybe InstanceExportDetails
    -> f (Maybe InstanceExportDetails))
    -> ExportTask
    -> f ExportTask
etInstanceExportDetails f x =
    (\y -> x { _etInstanceExportDetails = y })
       <$> f (_etInstanceExportDetails x)
{-# INLINE etInstanceExportDetails #-}

-- | 
etExportToS3Task
    :: Functor f
    => (Maybe ExportToS3Task
    -> f (Maybe ExportToS3Task))
    -> ExportTask
    -> f ExportTask
etExportToS3Task f x =
    (\y -> x { _etExportToS3Task = y })
       <$> f (_etExportToS3Task x)
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
etstDiskImageFormat
    :: Functor f
    => (Maybe DiskImageFormat
    -> f (Maybe DiskImageFormat))
    -> ExportToS3Task
    -> f ExportToS3Task
etstDiskImageFormat f x =
    (\y -> x { _etstDiskImageFormat = y })
       <$> f (_etstDiskImageFormat x)
{-# INLINE etstDiskImageFormat #-}

-- | The container format used to combine disk images with metadata (such as
-- OVF). If absent, only the disk image is exported.
etstContainerFormat
    :: Functor f
    => (Maybe ContainerFormat
    -> f (Maybe ContainerFormat))
    -> ExportToS3Task
    -> f ExportToS3Task
etstContainerFormat f x =
    (\y -> x { _etstContainerFormat = y })
       <$> f (_etstContainerFormat x)
{-# INLINE etstContainerFormat #-}

-- | The Amazon S3 bucket for the destination image. The destination bucket must
-- exist and grant WRITE and READ_ACL permissions to the AWS account
-- vm-import-export@amazon.com.
etstS3Bucket
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ExportToS3Task
    -> f ExportToS3Task
etstS3Bucket f x =
    (\y -> x { _etstS3Bucket = y })
       <$> f (_etstS3Bucket x)
{-# INLINE etstS3Bucket #-}

-- | 
etstS3Key
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ExportToS3Task
    -> f ExportToS3Task
etstS3Key f x =
    (\y -> x { _etstS3Key = y })
       <$> f (_etstS3Key x)
{-# INLINE etstS3Key #-}

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
etstsDiskImageFormat
    :: Functor f
    => (Maybe DiskImageFormat
    -> f (Maybe DiskImageFormat))
    -> ExportToS3TaskSpecification
    -> f ExportToS3TaskSpecification
etstsDiskImageFormat f x =
    (\y -> x { _etstsDiskImageFormat = y })
       <$> f (_etstsDiskImageFormat x)
{-# INLINE etstsDiskImageFormat #-}

-- | 
etstsContainerFormat
    :: Functor f
    => (Maybe ContainerFormat
    -> f (Maybe ContainerFormat))
    -> ExportToS3TaskSpecification
    -> f ExportToS3TaskSpecification
etstsContainerFormat f x =
    (\y -> x { _etstsContainerFormat = y })
       <$> f (_etstsContainerFormat x)
{-# INLINE etstsContainerFormat #-}

-- | 
etstsS3Bucket
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ExportToS3TaskSpecification
    -> f ExportToS3TaskSpecification
etstsS3Bucket f x =
    (\y -> x { _etstsS3Bucket = y })
       <$> f (_etstsS3Bucket x)
{-# INLINE etstsS3Bucket #-}

-- | The image is written to a single object in the Amazon S3 bucket at the S3
-- key s3prefix + exportTaskId + '.' + diskImageFormat.
etstsS3Prefix
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ExportToS3TaskSpecification
    -> f ExportToS3TaskSpecification
etstsS3Prefix f x =
    (\y -> x { _etstsS3Prefix = y })
       <$> f (_etstsS3Prefix x)
{-# INLINE etstsS3Prefix #-}

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
frName
    :: Functor f
    => (Text
    -> f (Text))
    -> Filter
    -> f Filter
frName f x =
    (\y -> x { _frName = y })
       <$> f (_frName x)
{-# INLINE frName #-}

-- | One or more filter values.
frValues
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> Filter
    -> f Filter
frValues f x =
    (\y -> x { _frValues = y })
       <$> f (_frValues x)
{-# INLINE frValues #-}

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
giGroupName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GroupIdentifier
    -> f GroupIdentifier
giGroupName f x =
    (\y -> x { _giGroupName = y })
       <$> f (_giGroupName x)
{-# INLINE giGroupName #-}

-- | The ID of the security group.
giGroupId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GroupIdentifier
    -> f GroupIdentifier
giGroupId f x =
    (\y -> x { _giGroupId = y })
       <$> f (_giGroupId x)
{-# INLINE giGroupId #-}

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
iipArn
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> IamInstanceProfile
    -> f IamInstanceProfile
iipArn f x =
    (\y -> x { _iipArn = y })
       <$> f (_iipArn x)
{-# INLINE iipArn #-}

-- | The ID of the instance profile.
iipId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> IamInstanceProfile
    -> f IamInstanceProfile
iipId f x =
    (\y -> x { _iipId = y })
       <$> f (_iipId x)
{-# INLINE iipId #-}

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
iipsArn
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> IamInstanceProfileSpecification
    -> f IamInstanceProfileSpecification
iipsArn f x =
    (\y -> x { _iipsArn = y })
       <$> f (_iipsArn x)
{-# INLINE iipsArn #-}

-- | The name of the instance profile.
iipsName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> IamInstanceProfileSpecification
    -> f IamInstanceProfileSpecification
iipsName f x =
    (\y -> x { _iipsName = y })
       <$> f (_iipsName x)
{-# INLINE iipsName #-}

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
itcType
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> IcmpTypeCode
    -> f IcmpTypeCode
itcType f x =
    (\y -> x { _itcType = y })
       <$> f (_itcType x)
{-# INLINE itcType #-}

-- | The ICMP type. A value of -1 means all types.
itcCode
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> IcmpTypeCode
    -> f IcmpTypeCode
itcCode f x =
    (\y -> x { _itcCode = y })
       <$> f (_itcCode x)
{-# INLINE itcCode #-}

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
ieImageId
    :: Functor f
    => (Text
    -> f (Text))
    -> Image
    -> f Image
ieImageId f x =
    (\y -> x { _ieImageId = y })
       <$> f (_ieImageId x)
{-# INLINE ieImageId #-}

-- | The location of the AMI.
ieImageLocation
    :: Functor f
    => (Text
    -> f (Text))
    -> Image
    -> f Image
ieImageLocation f x =
    (\y -> x { _ieImageLocation = y })
       <$> f (_ieImageLocation x)
{-# INLINE ieImageLocation #-}

-- | The current state of the AMI. If the state is available, the image is
-- successfully registered and can be used to launch an instance.
ieState
    :: Functor f
    => (ImageState
    -> f (ImageState))
    -> Image
    -> f Image
ieState f x =
    (\y -> x { _ieState = y })
       <$> f (_ieState x)
{-# INLINE ieState #-}

-- | The AWS account ID of the image owner.
ieOwnerId
    :: Functor f
    => (Text
    -> f (Text))
    -> Image
    -> f Image
ieOwnerId f x =
    (\y -> x { _ieOwnerId = y })
       <$> f (_ieOwnerId x)
{-# INLINE ieOwnerId #-}

-- | Indicates whether the image has public launch permissions. The value is
-- true if this image has public launch permissions or false if it has only
-- implicit and explicit launch permissions.
iePublic
    :: Functor f
    => (Bool
    -> f (Bool))
    -> Image
    -> f Image
iePublic f x =
    (\y -> x { _iePublic = y })
       <$> f (_iePublic x)
{-# INLINE iePublic #-}

-- | Any product codes associated with the AMI.
ieProductCodes
    :: Functor f
    => ([ProductCode]
    -> f ([ProductCode]))
    -> Image
    -> f Image
ieProductCodes f x =
    (\y -> x { _ieProductCodes = y })
       <$> f (_ieProductCodes x)
{-# INLINE ieProductCodes #-}

-- | The architecture of the image.
ieArchitecture
    :: Functor f
    => (ArchitectureValues
    -> f (ArchitectureValues))
    -> Image
    -> f Image
ieArchitecture f x =
    (\y -> x { _ieArchitecture = y })
       <$> f (_ieArchitecture x)
{-# INLINE ieArchitecture #-}

-- | The type of image.
ieImageType
    :: Functor f
    => (ImageTypeValues
    -> f (ImageTypeValues))
    -> Image
    -> f Image
ieImageType f x =
    (\y -> x { _ieImageType = y })
       <$> f (_ieImageType x)
{-# INLINE ieImageType #-}

-- | The kernel associated with the image, if any. Only applicable for machine
-- images.
ieKernelId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Image
    -> f Image
ieKernelId f x =
    (\y -> x { _ieKernelId = y })
       <$> f (_ieKernelId x)
{-# INLINE ieKernelId #-}

-- | The RAM disk associated with the image, if any. Only applicable for machine
-- images.
ieRamdiskId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Image
    -> f Image
ieRamdiskId f x =
    (\y -> x { _ieRamdiskId = y })
       <$> f (_ieRamdiskId x)
{-# INLINE ieRamdiskId #-}

-- | The value is Windows for Windows AMIs; otherwise blank.
iePlatform
    :: Functor f
    => (Maybe PlatformValues
    -> f (Maybe PlatformValues))
    -> Image
    -> f Image
iePlatform f x =
    (\y -> x { _iePlatform = y })
       <$> f (_iePlatform x)
{-# INLINE iePlatform #-}

-- | Specifies whether enhanced networking is enabled.
ieSriovNetSupport
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Image
    -> f Image
ieSriovNetSupport f x =
    (\y -> x { _ieSriovNetSupport = y })
       <$> f (_ieSriovNetSupport x)
{-# INLINE ieSriovNetSupport #-}

-- | The reason for the state change.
ieStateReason
    :: Functor f
    => (Maybe StateReason
    -> f (Maybe StateReason))
    -> Image
    -> f Image
ieStateReason f x =
    (\y -> x { _ieStateReason = y })
       <$> f (_ieStateReason x)
{-# INLINE ieStateReason #-}

-- | The AWS account alias (for example, amazon, self) or the AWS account ID of
-- the AMI owner.
ieImageOwnerAlias
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Image
    -> f Image
ieImageOwnerAlias f x =
    (\y -> x { _ieImageOwnerAlias = y })
       <$> f (_ieImageOwnerAlias x)
{-# INLINE ieImageOwnerAlias #-}

-- | The name of the AMI that was provided during image creation.
ieName
    :: Functor f
    => (Text
    -> f (Text))
    -> Image
    -> f Image
ieName f x =
    (\y -> x { _ieName = y })
       <$> f (_ieName x)
{-# INLINE ieName #-}

-- | The description of the AMI that was provided during image creation.
ieDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Image
    -> f Image
ieDescription f x =
    (\y -> x { _ieDescription = y })
       <$> f (_ieDescription x)
{-# INLINE ieDescription #-}

-- | The type of root device used by the AMI. The AMI can use an Amazon EBS
-- volume or an instance store volume.
ieRootDeviceType
    :: Functor f
    => (DeviceType
    -> f (DeviceType))
    -> Image
    -> f Image
ieRootDeviceType f x =
    (\y -> x { _ieRootDeviceType = y })
       <$> f (_ieRootDeviceType x)
{-# INLINE ieRootDeviceType #-}

-- | The device name of the root device (for example, /dev/sda1 or xvda).
ieRootDeviceName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Image
    -> f Image
ieRootDeviceName f x =
    (\y -> x { _ieRootDeviceName = y })
       <$> f (_ieRootDeviceName x)
{-# INLINE ieRootDeviceName #-}

-- | Any block device mapping entries.
ieBlockDeviceMappings
    :: Functor f
    => ([BlockDeviceMapping]
    -> f ([BlockDeviceMapping]))
    -> Image
    -> f Image
ieBlockDeviceMappings f x =
    (\y -> x { _ieBlockDeviceMappings = y })
       <$> f (_ieBlockDeviceMappings x)
{-# INLINE ieBlockDeviceMappings #-}

-- | The type of virtualization of the AMI.
ieVirtualizationType
    :: Functor f
    => (VirtualizationType
    -> f (VirtualizationType))
    -> Image
    -> f Image
ieVirtualizationType f x =
    (\y -> x { _ieVirtualizationType = y })
       <$> f (_ieVirtualizationType x)
{-# INLINE ieVirtualizationType #-}

-- | Any tags assigned to the image.
ieTags
    :: Functor f
    => ([Tag]
    -> f ([Tag]))
    -> Image
    -> f Image
ieTags f x =
    (\y -> x { _ieTags = y })
       <$> f (_ieTags x)
{-# INLINE ieTags #-}

-- | The hypervisor type of the image.
ieHypervisor
    :: Functor f
    => (HypervisorType
    -> f (HypervisorType))
    -> Image
    -> f Image
ieHypervisor f x =
    (\y -> x { _ieHypervisor = y })
       <$> f (_ieHypervisor x)
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
iilsArchitecture
    :: Functor f
    => (Maybe ArchitectureValues
    -> f (Maybe ArchitectureValues))
    -> ImportInstanceLaunchSpecification
    -> f ImportInstanceLaunchSpecification
iilsArchitecture f x =
    (\y -> x { _iilsArchitecture = y })
       <$> f (_iilsArchitecture x)
{-# INLINE iilsArchitecture #-}

-- | One or more security group names.
iilsGroupNames
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> ImportInstanceLaunchSpecification
    -> f ImportInstanceLaunchSpecification
iilsGroupNames f x =
    (\y -> x { _iilsGroupNames = y })
       <$> f (_iilsGroupNames x)
{-# INLINE iilsGroupNames #-}

-- | 
iilsAdditionalInfo
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ImportInstanceLaunchSpecification
    -> f ImportInstanceLaunchSpecification
iilsAdditionalInfo f x =
    (\y -> x { _iilsAdditionalInfo = y })
       <$> f (_iilsAdditionalInfo x)
{-# INLINE iilsAdditionalInfo #-}

-- | User data to be made available to the instance.
iilsUserData
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ImportInstanceLaunchSpecification
    -> f ImportInstanceLaunchSpecification
iilsUserData f x =
    (\y -> x { _iilsUserData = y })
       <$> f (_iilsUserData x)
{-# INLINE iilsUserData #-}

-- | The instance type. For more information, see Instance Types in the Amazon
-- Elastic Compute Cloud User Guide.
iilsInstanceType
    :: Functor f
    => (Maybe InstanceType
    -> f (Maybe InstanceType))
    -> ImportInstanceLaunchSpecification
    -> f ImportInstanceLaunchSpecification
iilsInstanceType f x =
    (\y -> x { _iilsInstanceType = y })
       <$> f (_iilsInstanceType x)
{-# INLINE iilsInstanceType #-}

-- | 
iilsPlacement
    :: Functor f
    => (Maybe Placement
    -> f (Maybe Placement))
    -> ImportInstanceLaunchSpecification
    -> f ImportInstanceLaunchSpecification
iilsPlacement f x =
    (\y -> x { _iilsPlacement = y })
       <$> f (_iilsPlacement x)
{-# INLINE iilsPlacement #-}

-- | 
iilsMonitoring
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> ImportInstanceLaunchSpecification
    -> f ImportInstanceLaunchSpecification
iilsMonitoring f x =
    (\y -> x { _iilsMonitoring = y })
       <$> f (_iilsMonitoring x)
{-# INLINE iilsMonitoring #-}

-- | [EC2-VPC] The ID of the subnet to launch the instance into.
iilsSubnetId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ImportInstanceLaunchSpecification
    -> f ImportInstanceLaunchSpecification
iilsSubnetId f x =
    (\y -> x { _iilsSubnetId = y })
       <$> f (_iilsSubnetId x)
{-# INLINE iilsSubnetId #-}

-- | Indicates whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for system
-- shutdown).
iilsInstanceInitiatedShutdownBehavior
    :: Functor f
    => (Maybe ShutdownBehavior
    -> f (Maybe ShutdownBehavior))
    -> ImportInstanceLaunchSpecification
    -> f ImportInstanceLaunchSpecification
iilsInstanceInitiatedShutdownBehavior f x =
    (\y -> x { _iilsInstanceInitiatedShutdownBehavior = y })
       <$> f (_iilsInstanceInitiatedShutdownBehavior x)
{-# INLINE iilsInstanceInitiatedShutdownBehavior #-}

-- | [EC2-VPC] Optionally, you can use this parameter to assign the instance a
-- specific available IP address from the IP address range of the subnet.
iilsPrivateIpAddress
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ImportInstanceLaunchSpecification
    -> f ImportInstanceLaunchSpecification
iilsPrivateIpAddress f x =
    (\y -> x { _iilsPrivateIpAddress = y })
       <$> f (_iilsPrivateIpAddress x)
{-# INLINE iilsPrivateIpAddress #-}

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
iitdVolumes
    :: Functor f
    => ([ImportInstanceVolumeDetailItem]
    -> f ([ImportInstanceVolumeDetailItem]))
    -> ImportInstanceTaskDetails
    -> f ImportInstanceTaskDetails
iitdVolumes f x =
    (\y -> x { _iitdVolumes = y })
       <$> f (_iitdVolumes x)
{-# INLINE iitdVolumes #-}

-- | 
iitdInstanceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ImportInstanceTaskDetails
    -> f ImportInstanceTaskDetails
iitdInstanceId f x =
    (\y -> x { _iitdInstanceId = y })
       <$> f (_iitdInstanceId x)
{-# INLINE iitdInstanceId #-}

-- | The instance operating system.
iitdPlatform
    :: Functor f
    => (Maybe PlatformValues
    -> f (Maybe PlatformValues))
    -> ImportInstanceTaskDetails
    -> f ImportInstanceTaskDetails
iitdPlatform f x =
    (\y -> x { _iitdPlatform = y })
       <$> f (_iitdPlatform x)
{-# INLINE iitdPlatform #-}

-- | 
iitdDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ImportInstanceTaskDetails
    -> f ImportInstanceTaskDetails
iitdDescription f x =
    (\y -> x { _iitdDescription = y })
       <$> f (_iitdDescription x)
{-# INLINE iitdDescription #-}

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
iivdiBytesConverted
    :: Functor f
    => (Integer
    -> f (Integer))
    -> ImportInstanceVolumeDetailItem
    -> f ImportInstanceVolumeDetailItem
iivdiBytesConverted f x =
    (\y -> x { _iivdiBytesConverted = y })
       <$> f (_iivdiBytesConverted x)
{-# INLINE iivdiBytesConverted #-}

-- | The Availability Zone where the resulting instance will reside.
iivdiAvailabilityZone
    :: Functor f
    => (Text
    -> f (Text))
    -> ImportInstanceVolumeDetailItem
    -> f ImportInstanceVolumeDetailItem
iivdiAvailabilityZone f x =
    (\y -> x { _iivdiAvailabilityZone = y })
       <$> f (_iivdiAvailabilityZone x)
{-# INLINE iivdiAvailabilityZone #-}

-- | The image.
iivdiImage
    :: Functor f
    => (DiskImageDescription
    -> f (DiskImageDescription))
    -> ImportInstanceVolumeDetailItem
    -> f ImportInstanceVolumeDetailItem
iivdiImage f x =
    (\y -> x { _iivdiImage = y })
       <$> f (_iivdiImage x)
{-# INLINE iivdiImage #-}

-- | The volume.
iivdiVolume
    :: Functor f
    => (DiskImageVolumeDescription
    -> f (DiskImageVolumeDescription))
    -> ImportInstanceVolumeDetailItem
    -> f ImportInstanceVolumeDetailItem
iivdiVolume f x =
    (\y -> x { _iivdiVolume = y })
       <$> f (_iivdiVolume x)
{-# INLINE iivdiVolume #-}

-- | The status of the import of this particular disk image.
iivdiStatus
    :: Functor f
    => (Text
    -> f (Text))
    -> ImportInstanceVolumeDetailItem
    -> f ImportInstanceVolumeDetailItem
iivdiStatus f x =
    (\y -> x { _iivdiStatus = y })
       <$> f (_iivdiStatus x)
{-# INLINE iivdiStatus #-}

-- | The status information or errors related to the disk image.
iivdiStatusMessage
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ImportInstanceVolumeDetailItem
    -> f ImportInstanceVolumeDetailItem
iivdiStatusMessage f x =
    (\y -> x { _iivdiStatusMessage = y })
       <$> f (_iivdiStatusMessage x)
{-# INLINE iivdiStatusMessage #-}

-- | 
iivdiDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ImportInstanceVolumeDetailItem
    -> f ImportInstanceVolumeDetailItem
iivdiDescription f x =
    (\y -> x { _iivdiDescription = y })
       <$> f (_iivdiDescription x)
{-# INLINE iivdiDescription #-}

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
ivtdBytesConverted
    :: Functor f
    => (Integer
    -> f (Integer))
    -> ImportVolumeTaskDetails
    -> f ImportVolumeTaskDetails
ivtdBytesConverted f x =
    (\y -> x { _ivtdBytesConverted = y })
       <$> f (_ivtdBytesConverted x)
{-# INLINE ivtdBytesConverted #-}

-- | The Availability Zone where the resulting volume will reside.
ivtdAvailabilityZone
    :: Functor f
    => (Text
    -> f (Text))
    -> ImportVolumeTaskDetails
    -> f ImportVolumeTaskDetails
ivtdAvailabilityZone f x =
    (\y -> x { _ivtdAvailabilityZone = y })
       <$> f (_ivtdAvailabilityZone x)
{-# INLINE ivtdAvailabilityZone #-}

-- | The description you provided when starting the import volume task.
ivtdDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ImportVolumeTaskDetails
    -> f ImportVolumeTaskDetails
ivtdDescription f x =
    (\y -> x { _ivtdDescription = y })
       <$> f (_ivtdDescription x)
{-# INLINE ivtdDescription #-}

-- | The image.
ivtdImage
    :: Functor f
    => (DiskImageDescription
    -> f (DiskImageDescription))
    -> ImportVolumeTaskDetails
    -> f ImportVolumeTaskDetails
ivtdImage f x =
    (\y -> x { _ivtdImage = y })
       <$> f (_ivtdImage x)
{-# INLINE ivtdImage #-}

-- | The volume.
ivtdVolume
    :: Functor f
    => (DiskImageVolumeDescription
    -> f (DiskImageVolumeDescription))
    -> ImportVolumeTaskDetails
    -> f ImportVolumeTaskDetails
ivtdVolume f x =
    (\y -> x { _ivtdVolume = y })
       <$> f (_ivtdVolume x)
{-# INLINE ivtdVolume #-}

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
ifInstanceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Instance
    -> f Instance
ifInstanceId f x =
    (\y -> x { _ifInstanceId = y })
       <$> f (_ifInstanceId x)
{-# INLINE ifInstanceId #-}

-- | The ID of the AMI used to launch the instance.
ifImageId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Instance
    -> f Instance
ifImageId f x =
    (\y -> x { _ifImageId = y })
       <$> f (_ifImageId x)
{-# INLINE ifImageId #-}

-- | The current state of the instance.
ifState
    :: Functor f
    => (Maybe InstanceState
    -> f (Maybe InstanceState))
    -> Instance
    -> f Instance
ifState f x =
    (\y -> x { _ifState = y })
       <$> f (_ifState x)
{-# INLINE ifState #-}

-- | The private DNS name assigned to the instance. This DNS name can only be
-- used inside the Amazon EC2 network. This name is not available until the
-- instance enters the running state.
ifPrivateDnsName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Instance
    -> f Instance
ifPrivateDnsName f x =
    (\y -> x { _ifPrivateDnsName = y })
       <$> f (_ifPrivateDnsName x)
{-# INLINE ifPrivateDnsName #-}

-- | The public DNS name assigned to the instance. This name is not available
-- until the instance enters the running state.
ifPublicDnsName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Instance
    -> f Instance
ifPublicDnsName f x =
    (\y -> x { _ifPublicDnsName = y })
       <$> f (_ifPublicDnsName x)
{-# INLINE ifPublicDnsName #-}

-- | The reason for the most recent state transition. This might be an empty
-- string.
ifStateTransitionReason
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Instance
    -> f Instance
ifStateTransitionReason f x =
    (\y -> x { _ifStateTransitionReason = y })
       <$> f (_ifStateTransitionReason x)
{-# INLINE ifStateTransitionReason #-}

-- | The name of the key pair, if this instance was launched with an associated
-- key pair.
ifKeyName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Instance
    -> f Instance
ifKeyName f x =
    (\y -> x { _ifKeyName = y })
       <$> f (_ifKeyName x)
{-# INLINE ifKeyName #-}

-- | The AMI launch index, which can be used to find this instance in the launch
-- group.
ifAmiLaunchIndex
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> Instance
    -> f Instance
ifAmiLaunchIndex f x =
    (\y -> x { _ifAmiLaunchIndex = y })
       <$> f (_ifAmiLaunchIndex x)
{-# INLINE ifAmiLaunchIndex #-}

-- | The product codes attached to this instance.
ifProductCodes
    :: Functor f
    => ([ProductCode]
    -> f ([ProductCode]))
    -> Instance
    -> f Instance
ifProductCodes f x =
    (\y -> x { _ifProductCodes = y })
       <$> f (_ifProductCodes x)
{-# INLINE ifProductCodes #-}

-- | The instance type.
ifInstanceType
    :: Functor f
    => (Maybe InstanceType
    -> f (Maybe InstanceType))
    -> Instance
    -> f Instance
ifInstanceType f x =
    (\y -> x { _ifInstanceType = y })
       <$> f (_ifInstanceType x)
{-# INLINE ifInstanceType #-}

-- | The time the instance was launched.
ifLaunchTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> Instance
    -> f Instance
ifLaunchTime f x =
    (\y -> x { _ifLaunchTime = y })
       <$> f (_ifLaunchTime x)
{-# INLINE ifLaunchTime #-}

-- | The location where the instance launched.
ifPlacement
    :: Functor f
    => (Maybe Placement
    -> f (Maybe Placement))
    -> Instance
    -> f Instance
ifPlacement f x =
    (\y -> x { _ifPlacement = y })
       <$> f (_ifPlacement x)
{-# INLINE ifPlacement #-}

-- | The kernel associated with this instance.
ifKernelId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Instance
    -> f Instance
ifKernelId f x =
    (\y -> x { _ifKernelId = y })
       <$> f (_ifKernelId x)
{-# INLINE ifKernelId #-}

-- | The RAM disk associated with this instance.
ifRamdiskId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Instance
    -> f Instance
ifRamdiskId f x =
    (\y -> x { _ifRamdiskId = y })
       <$> f (_ifRamdiskId x)
{-# INLINE ifRamdiskId #-}

-- | The value is Windows for Windows instances; otherwise blank.
ifPlatform
    :: Functor f
    => (Maybe PlatformValues
    -> f (Maybe PlatformValues))
    -> Instance
    -> f Instance
ifPlatform f x =
    (\y -> x { _ifPlatform = y })
       <$> f (_ifPlatform x)
{-# INLINE ifPlatform #-}

-- | The monitoring information for the instance.
ifMonitoring
    :: Functor f
    => (Maybe Monitoring
    -> f (Maybe Monitoring))
    -> Instance
    -> f Instance
ifMonitoring f x =
    (\y -> x { _ifMonitoring = y })
       <$> f (_ifMonitoring x)
{-# INLINE ifMonitoring #-}

-- | The ID of the subnet in which the instance is running.
ifSubnetId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Instance
    -> f Instance
ifSubnetId f x =
    (\y -> x { _ifSubnetId = y })
       <$> f (_ifSubnetId x)
{-# INLINE ifSubnetId #-}

-- | The ID of the VPC in which the instance is running.
ifVpcId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Instance
    -> f Instance
ifVpcId f x =
    (\y -> x { _ifVpcId = y })
       <$> f (_ifVpcId x)
{-# INLINE ifVpcId #-}

-- | The private IP address assigned to the instance.
ifPrivateIpAddress
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Instance
    -> f Instance
ifPrivateIpAddress f x =
    (\y -> x { _ifPrivateIpAddress = y })
       <$> f (_ifPrivateIpAddress x)
{-# INLINE ifPrivateIpAddress #-}

-- | The public IP address assigned to the instance.
ifPublicIpAddress
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Instance
    -> f Instance
ifPublicIpAddress f x =
    (\y -> x { _ifPublicIpAddress = y })
       <$> f (_ifPublicIpAddress x)
{-# INLINE ifPublicIpAddress #-}

-- | The reason for the most recent state transition.
ifStateReason
    :: Functor f
    => (Maybe StateReason
    -> f (Maybe StateReason))
    -> Instance
    -> f Instance
ifStateReason f x =
    (\y -> x { _ifStateReason = y })
       <$> f (_ifStateReason x)
{-# INLINE ifStateReason #-}

-- | The architecture of the image.
ifArchitecture
    :: Functor f
    => (Maybe ArchitectureValues
    -> f (Maybe ArchitectureValues))
    -> Instance
    -> f Instance
ifArchitecture f x =
    (\y -> x { _ifArchitecture = y })
       <$> f (_ifArchitecture x)
{-# INLINE ifArchitecture #-}

-- | The root device type used by the AMI. The AMI can use an Amazon EBS volume
-- or an instance store volume.
ifRootDeviceType
    :: Functor f
    => (Maybe DeviceType
    -> f (Maybe DeviceType))
    -> Instance
    -> f Instance
ifRootDeviceType f x =
    (\y -> x { _ifRootDeviceType = y })
       <$> f (_ifRootDeviceType x)
{-# INLINE ifRootDeviceType #-}

-- | The root device name (for example, /dev/sda1).
ifRootDeviceName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Instance
    -> f Instance
ifRootDeviceName f x =
    (\y -> x { _ifRootDeviceName = y })
       <$> f (_ifRootDeviceName x)
{-# INLINE ifRootDeviceName #-}

-- | Any block device mapping entries for the instance.
ifBlockDeviceMappings
    :: Functor f
    => ([InstanceBlockDeviceMapping]
    -> f ([InstanceBlockDeviceMapping]))
    -> Instance
    -> f Instance
ifBlockDeviceMappings f x =
    (\y -> x { _ifBlockDeviceMappings = y })
       <$> f (_ifBlockDeviceMappings x)
{-# INLINE ifBlockDeviceMappings #-}

-- | The virtualization type of the instance.
ifVirtualizationType
    :: Functor f
    => (Maybe VirtualizationType
    -> f (Maybe VirtualizationType))
    -> Instance
    -> f Instance
ifVirtualizationType f x =
    (\y -> x { _ifVirtualizationType = y })
       <$> f (_ifVirtualizationType x)
{-# INLINE ifVirtualizationType #-}

-- | Indicates whether this is a Spot Instance.
ifInstanceLifecycle
    :: Functor f
    => (Maybe InstanceLifecycleType
    -> f (Maybe InstanceLifecycleType))
    -> Instance
    -> f Instance
ifInstanceLifecycle f x =
    (\y -> x { _ifInstanceLifecycle = y })
       <$> f (_ifInstanceLifecycle x)
{-# INLINE ifInstanceLifecycle #-}

-- | The ID of the Spot Instance request.
ifSpotInstanceRequestId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Instance
    -> f Instance
ifSpotInstanceRequestId f x =
    (\y -> x { _ifSpotInstanceRequestId = y })
       <$> f (_ifSpotInstanceRequestId x)
{-# INLINE ifSpotInstanceRequestId #-}

-- | The idempotency token you provided when you launched the instance.
ifClientToken
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Instance
    -> f Instance
ifClientToken f x =
    (\y -> x { _ifClientToken = y })
       <$> f (_ifClientToken x)
{-# INLINE ifClientToken #-}

-- | Any tags assigned to the instance.
ifTags
    :: Functor f
    => ([Tag]
    -> f ([Tag]))
    -> Instance
    -> f Instance
ifTags f x =
    (\y -> x { _ifTags = y })
       <$> f (_ifTags x)
{-# INLINE ifTags #-}

-- | One or more security groups for the instance.
ifSecurityGroups
    :: Functor f
    => ([GroupIdentifier]
    -> f ([GroupIdentifier]))
    -> Instance
    -> f Instance
ifSecurityGroups f x =
    (\y -> x { _ifSecurityGroups = y })
       <$> f (_ifSecurityGroups x)
{-# INLINE ifSecurityGroups #-}

-- | Specifies whether to enable an instance launched in a VPC to perform NAT.
-- This controls whether source/destination checking is enabled on the
-- instance. A value of true means checking is enabled, and false means
-- checking is disabled. The value must be false for the instance to perform
-- NAT. For more information, see NAT Instances in the Amazon Virtual Private
-- Cloud User Guide.
ifSourceDestCheck
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> Instance
    -> f Instance
ifSourceDestCheck f x =
    (\y -> x { _ifSourceDestCheck = y })
       <$> f (_ifSourceDestCheck x)
{-# INLINE ifSourceDestCheck #-}

-- | The hypervisor type of the instance.
ifHypervisor
    :: Functor f
    => (Maybe HypervisorType
    -> f (Maybe HypervisorType))
    -> Instance
    -> f Instance
ifHypervisor f x =
    (\y -> x { _ifHypervisor = y })
       <$> f (_ifHypervisor x)
{-# INLINE ifHypervisor #-}

-- | [EC2-VPC] One or more network interfaces for the instance.
ifNetworkInterfaces
    :: Functor f
    => ([InstanceNetworkInterface]
    -> f ([InstanceNetworkInterface]))
    -> Instance
    -> f Instance
ifNetworkInterfaces f x =
    (\y -> x { _ifNetworkInterfaces = y })
       <$> f (_ifNetworkInterfaces x)
{-# INLINE ifNetworkInterfaces #-}

-- | The IAM instance profile associated with the instance.
ifIamInstanceProfile
    :: Functor f
    => (Maybe IamInstanceProfile
    -> f (Maybe IamInstanceProfile))
    -> Instance
    -> f Instance
ifIamInstanceProfile f x =
    (\y -> x { _ifIamInstanceProfile = y })
       <$> f (_ifIamInstanceProfile x)
{-# INLINE ifIamInstanceProfile #-}

-- | Indicates whether the instance is optimized for EBS I/O. This optimization
-- provides dedicated throughput to Amazon EBS and an optimized configuration
-- stack to provide optimal I/O performance. This optimization isn't available
-- with all instance types. Additional usage charges apply when using an EBS
-- Optimized instance.
ifEbsOptimized
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> Instance
    -> f Instance
ifEbsOptimized f x =
    (\y -> x { _ifEbsOptimized = y })
       <$> f (_ifEbsOptimized x)
{-# INLINE ifEbsOptimized #-}

-- | Specifies whether enhanced networking is enabled.
ifSriovNetSupport
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Instance
    -> f Instance
ifSriovNetSupport f x =
    (\y -> x { _ifSriovNetSupport = y })
       <$> f (_ifSriovNetSupport x)
{-# INLINE ifSriovNetSupport #-}

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
ibdmDeviceName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> InstanceBlockDeviceMapping
    -> f InstanceBlockDeviceMapping
ibdmDeviceName f x =
    (\y -> x { _ibdmDeviceName = y })
       <$> f (_ibdmDeviceName x)
{-# INLINE ibdmDeviceName #-}

-- | Parameters used to automatically set up Amazon EBS volumes when the
-- instance is launched.
ibdmEbs
    :: Functor f
    => (Maybe EbsInstanceBlockDevice
    -> f (Maybe EbsInstanceBlockDevice))
    -> InstanceBlockDeviceMapping
    -> f InstanceBlockDeviceMapping
ibdmEbs f x =
    (\y -> x { _ibdmEbs = y })
       <$> f (_ibdmEbs x)
{-# INLINE ibdmEbs #-}

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
ibdmsDeviceName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> InstanceBlockDeviceMappingSpecification
    -> f InstanceBlockDeviceMappingSpecification
ibdmsDeviceName f x =
    (\y -> x { _ibdmsDeviceName = y })
       <$> f (_ibdmsDeviceName x)
{-# INLINE ibdmsDeviceName #-}

-- | Parameters used to automatically set up Amazon EBS volumes when the
-- instance is launched.
ibdmsEbs
    :: Functor f
    => (Maybe EbsInstanceBlockDeviceSpecification
    -> f (Maybe EbsInstanceBlockDeviceSpecification))
    -> InstanceBlockDeviceMappingSpecification
    -> f InstanceBlockDeviceMappingSpecification
ibdmsEbs f x =
    (\y -> x { _ibdmsEbs = y })
       <$> f (_ibdmsEbs x)
{-# INLINE ibdmsEbs #-}

-- | The virtual device name.
ibdmsVirtualName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> InstanceBlockDeviceMappingSpecification
    -> f InstanceBlockDeviceMappingSpecification
ibdmsVirtualName f x =
    (\y -> x { _ibdmsVirtualName = y })
       <$> f (_ibdmsVirtualName x)
{-# INLINE ibdmsVirtualName #-}

-- | suppress the specified device included in the block device mapping.
ibdmsNoDevice
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> InstanceBlockDeviceMappingSpecification
    -> f InstanceBlockDeviceMappingSpecification
ibdmsNoDevice f x =
    (\y -> x { _ibdmsNoDevice = y })
       <$> f (_ibdmsNoDevice x)
{-# INLINE ibdmsNoDevice #-}

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
icState
    :: Functor f
    => (Maybe ListingState
    -> f (Maybe ListingState))
    -> InstanceCount
    -> f InstanceCount
icState f x =
    (\y -> x { _icState = y })
       <$> f (_icState x)
{-# INLINE icState #-}

-- | he number of listed Reserved Instances in the state specified by the state.
icInstanceCount
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> InstanceCount
    -> f InstanceCount
icInstanceCount f x =
    (\y -> x { _icInstanceCount = y })
       <$> f (_icInstanceCount x)
{-# INLINE icInstanceCount #-}

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
iedInstanceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> InstanceExportDetails
    -> f InstanceExportDetails
iedInstanceId f x =
    (\y -> x { _iedInstanceId = y })
       <$> f (_iedInstanceId x)
{-# INLINE iedInstanceId #-}

-- | The target virtualization environment.
iedTargetEnvironment
    :: Functor f
    => (Maybe ExportEnvironment
    -> f (Maybe ExportEnvironment))
    -> InstanceExportDetails
    -> f InstanceExportDetails
iedTargetEnvironment f x =
    (\y -> x { _iedTargetEnvironment = y })
       <$> f (_iedTargetEnvironment x)
{-# INLINE iedTargetEnvironment #-}

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
inInstanceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> InstanceMonitoring
    -> f InstanceMonitoring
inInstanceId f x =
    (\y -> x { _inInstanceId = y })
       <$> f (_inInstanceId x)
{-# INLINE inInstanceId #-}

-- | The monitoring information.
inMonitoring
    :: Functor f
    => (Maybe Monitoring
    -> f (Maybe Monitoring))
    -> InstanceMonitoring
    -> f InstanceMonitoring
inMonitoring f x =
    (\y -> x { _inMonitoring = y })
       <$> f (_inMonitoring x)
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
iniNetworkInterfaceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> InstanceNetworkInterface
    -> f InstanceNetworkInterface
iniNetworkInterfaceId f x =
    (\y -> x { _iniNetworkInterfaceId = y })
       <$> f (_iniNetworkInterfaceId x)
{-# INLINE iniNetworkInterfaceId #-}

-- | The ID of the subnet.
iniSubnetId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> InstanceNetworkInterface
    -> f InstanceNetworkInterface
iniSubnetId f x =
    (\y -> x { _iniSubnetId = y })
       <$> f (_iniSubnetId x)
{-# INLINE iniSubnetId #-}

-- | The ID of the VPC.
iniVpcId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> InstanceNetworkInterface
    -> f InstanceNetworkInterface
iniVpcId f x =
    (\y -> x { _iniVpcId = y })
       <$> f (_iniVpcId x)
{-# INLINE iniVpcId #-}

-- | The description.
iniDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> InstanceNetworkInterface
    -> f InstanceNetworkInterface
iniDescription f x =
    (\y -> x { _iniDescription = y })
       <$> f (_iniDescription x)
{-# INLINE iniDescription #-}

-- | The ID of the AWS account that created the network interface.
iniOwnerId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> InstanceNetworkInterface
    -> f InstanceNetworkInterface
iniOwnerId f x =
    (\y -> x { _iniOwnerId = y })
       <$> f (_iniOwnerId x)
{-# INLINE iniOwnerId #-}

-- | The status of the network interface.
iniStatus
    :: Functor f
    => (Maybe NetworkInterfaceStatus
    -> f (Maybe NetworkInterfaceStatus))
    -> InstanceNetworkInterface
    -> f InstanceNetworkInterface
iniStatus f x =
    (\y -> x { _iniStatus = y })
       <$> f (_iniStatus x)
{-# INLINE iniStatus #-}

-- | The IP address of the network interface within the subnet.
iniPrivateIpAddress
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> InstanceNetworkInterface
    -> f InstanceNetworkInterface
iniPrivateIpAddress f x =
    (\y -> x { _iniPrivateIpAddress = y })
       <$> f (_iniPrivateIpAddress x)
{-# INLINE iniPrivateIpAddress #-}

-- | The private DNS name.
iniPrivateDnsName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> InstanceNetworkInterface
    -> f InstanceNetworkInterface
iniPrivateDnsName f x =
    (\y -> x { _iniPrivateDnsName = y })
       <$> f (_iniPrivateDnsName x)
{-# INLINE iniPrivateDnsName #-}

-- | Indicates whether to validate network traffic to or from this network
-- interface.
iniSourceDestCheck
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> InstanceNetworkInterface
    -> f InstanceNetworkInterface
iniSourceDestCheck f x =
    (\y -> x { _iniSourceDestCheck = y })
       <$> f (_iniSourceDestCheck x)
{-# INLINE iniSourceDestCheck #-}

-- | One or more security groups.
iniGroups
    :: Functor f
    => ([GroupIdentifier]
    -> f ([GroupIdentifier]))
    -> InstanceNetworkInterface
    -> f InstanceNetworkInterface
iniGroups f x =
    (\y -> x { _iniGroups = y })
       <$> f (_iniGroups x)
{-# INLINE iniGroups #-}

-- | The network interface attachment.
iniAttachment
    :: Functor f
    => (Maybe InstanceNetworkInterfaceAttachment
    -> f (Maybe InstanceNetworkInterfaceAttachment))
    -> InstanceNetworkInterface
    -> f InstanceNetworkInterface
iniAttachment f x =
    (\y -> x { _iniAttachment = y })
       <$> f (_iniAttachment x)
{-# INLINE iniAttachment #-}

-- | The association information for an Elastic IP associated with the network
-- interface.
iniAssociation
    :: Functor f
    => (Maybe InstanceNetworkInterfaceAssociation
    -> f (Maybe InstanceNetworkInterfaceAssociation))
    -> InstanceNetworkInterface
    -> f InstanceNetworkInterface
iniAssociation f x =
    (\y -> x { _iniAssociation = y })
       <$> f (_iniAssociation x)
{-# INLINE iniAssociation #-}

-- | The private IP addresses associated with the network interface.
iniPrivateIpAddresses
    :: Functor f
    => ([InstancePrivateIpAddress]
    -> f ([InstancePrivateIpAddress]))
    -> InstanceNetworkInterface
    -> f InstanceNetworkInterface
iniPrivateIpAddresses f x =
    (\y -> x { _iniPrivateIpAddresses = y })
       <$> f (_iniPrivateIpAddresses x)
{-# INLINE iniPrivateIpAddresses #-}

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
inibPublicIp
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> InstanceNetworkInterfaceAssociation
    -> f InstanceNetworkInterfaceAssociation
inibPublicIp f x =
    (\y -> x { _inibPublicIp = y })
       <$> f (_inibPublicIp x)
{-# INLINE inibPublicIp #-}

-- | The public DNS name.
inibPublicDnsName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> InstanceNetworkInterfaceAssociation
    -> f InstanceNetworkInterfaceAssociation
inibPublicDnsName f x =
    (\y -> x { _inibPublicDnsName = y })
       <$> f (_inibPublicDnsName x)
{-# INLINE inibPublicDnsName #-}

-- | The ID of the owner of the Elastic IP address.
inibIpOwnerId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> InstanceNetworkInterfaceAssociation
    -> f InstanceNetworkInterfaceAssociation
inibIpOwnerId f x =
    (\y -> x { _inibIpOwnerId = y })
       <$> f (_inibIpOwnerId x)
{-# INLINE inibIpOwnerId #-}

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
iniaAttachmentId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> InstanceNetworkInterfaceAttachment
    -> f InstanceNetworkInterfaceAttachment
iniaAttachmentId f x =
    (\y -> x { _iniaAttachmentId = y })
       <$> f (_iniaAttachmentId x)
{-# INLINE iniaAttachmentId #-}

-- | The index of the device on the instance for the network interface
-- attachment.
iniaDeviceIndex
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> InstanceNetworkInterfaceAttachment
    -> f InstanceNetworkInterfaceAttachment
iniaDeviceIndex f x =
    (\y -> x { _iniaDeviceIndex = y })
       <$> f (_iniaDeviceIndex x)
{-# INLINE iniaDeviceIndex #-}

-- | The attachment state.
iniaStatus
    :: Functor f
    => (Maybe AttachmentStatus
    -> f (Maybe AttachmentStatus))
    -> InstanceNetworkInterfaceAttachment
    -> f InstanceNetworkInterfaceAttachment
iniaStatus f x =
    (\y -> x { _iniaStatus = y })
       <$> f (_iniaStatus x)
{-# INLINE iniaStatus #-}

-- | The time stamp when the attachment initiated.
iniaAttachTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> InstanceNetworkInterfaceAttachment
    -> f InstanceNetworkInterfaceAttachment
iniaAttachTime f x =
    (\y -> x { _iniaAttachTime = y })
       <$> f (_iniaAttachTime x)
{-# INLINE iniaAttachTime #-}

-- | Indicates whether the network interface is deleted when the instance is
-- terminated.
iniaDeleteOnTermination
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> InstanceNetworkInterfaceAttachment
    -> f InstanceNetworkInterfaceAttachment
iniaDeleteOnTermination f x =
    (\y -> x { _iniaDeleteOnTermination = y })
       <$> f (_iniaDeleteOnTermination x)
{-# INLINE iniaDeleteOnTermination #-}

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
inisNetworkInterfaceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> InstanceNetworkInterfaceSpecification
    -> f InstanceNetworkInterfaceSpecification
inisNetworkInterfaceId f x =
    (\y -> x { _inisNetworkInterfaceId = y })
       <$> f (_inisNetworkInterfaceId x)
{-# INLINE inisNetworkInterfaceId #-}

-- | The index of the device on the instance for the network interface
-- attachment.
inisDeviceIndex
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> InstanceNetworkInterfaceSpecification
    -> f InstanceNetworkInterfaceSpecification
inisDeviceIndex f x =
    (\y -> x { _inisDeviceIndex = y })
       <$> f (_inisDeviceIndex x)
{-# INLINE inisDeviceIndex #-}

-- | The ID of the subnet associated with the network string.
inisSubnetId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> InstanceNetworkInterfaceSpecification
    -> f InstanceNetworkInterfaceSpecification
inisSubnetId f x =
    (\y -> x { _inisSubnetId = y })
       <$> f (_inisSubnetId x)
{-# INLINE inisSubnetId #-}

-- | The description of the network interface.
inisDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> InstanceNetworkInterfaceSpecification
    -> f InstanceNetworkInterfaceSpecification
inisDescription f x =
    (\y -> x { _inisDescription = y })
       <$> f (_inisDescription x)
{-# INLINE inisDescription #-}

-- | The private IP address of the network interface.
inisPrivateIpAddress
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> InstanceNetworkInterfaceSpecification
    -> f InstanceNetworkInterfaceSpecification
inisPrivateIpAddress f x =
    (\y -> x { _inisPrivateIpAddress = y })
       <$> f (_inisPrivateIpAddress x)
{-# INLINE inisPrivateIpAddress #-}

-- | The IDs of the security groups for the network interface.
inisGroups
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> InstanceNetworkInterfaceSpecification
    -> f InstanceNetworkInterfaceSpecification
inisGroups f x =
    (\y -> x { _inisGroups = y })
       <$> f (_inisGroups x)
{-# INLINE inisGroups #-}

-- | If set to true, the interface is deleted when the instance is terminated.
inisDeleteOnTermination
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> InstanceNetworkInterfaceSpecification
    -> f InstanceNetworkInterfaceSpecification
inisDeleteOnTermination f x =
    (\y -> x { _inisDeleteOnTermination = y })
       <$> f (_inisDeleteOnTermination x)
{-# INLINE inisDeleteOnTermination #-}

-- | One or more private IP addresses to assign to the network interface.
inisPrivateIpAddresses
    :: Functor f
    => ([PrivateIpAddressSpecification]
    -> f ([PrivateIpAddressSpecification]))
    -> InstanceNetworkInterfaceSpecification
    -> f InstanceNetworkInterfaceSpecification
inisPrivateIpAddresses f x =
    (\y -> x { _inisPrivateIpAddresses = y })
       <$> f (_inisPrivateIpAddresses x)
{-# INLINE inisPrivateIpAddresses #-}

-- | The number of secondary private IP addresses.
inisSecondaryPrivateIpAddressCount
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> InstanceNetworkInterfaceSpecification
    -> f InstanceNetworkInterfaceSpecification
inisSecondaryPrivateIpAddressCount f x =
    (\y -> x { _inisSecondaryPrivateIpAddressCount = y })
       <$> f (_inisSecondaryPrivateIpAddressCount x)
{-# INLINE inisSecondaryPrivateIpAddressCount #-}

-- | Indicates whether to auto-assign a public IP address to an instance in a
-- VPC. This public IP address can be assigned to the network interface for
-- eth0 only when you launch the instance. You must create the network
-- interface instead of using an existing network interface for eth0, and you
-- must not specify more than one network interface.
inisAssociatePublicIpAddress
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> InstanceNetworkInterfaceSpecification
    -> f InstanceNetworkInterfaceSpecification
inisAssociatePublicIpAddress f x =
    (\y -> x { _inisAssociatePublicIpAddress = y })
       <$> f (_inisAssociatePublicIpAddress x)
{-# INLINE inisAssociatePublicIpAddress #-}

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
ipiaPrivateIpAddress
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> InstancePrivateIpAddress
    -> f InstancePrivateIpAddress
ipiaPrivateIpAddress f x =
    (\y -> x { _ipiaPrivateIpAddress = y })
       <$> f (_ipiaPrivateIpAddress x)
{-# INLINE ipiaPrivateIpAddress #-}

-- | The private DNS name.
ipiaPrivateDnsName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> InstancePrivateIpAddress
    -> f InstancePrivateIpAddress
ipiaPrivateDnsName f x =
    (\y -> x { _ipiaPrivateDnsName = y })
       <$> f (_ipiaPrivateDnsName x)
{-# INLINE ipiaPrivateDnsName #-}

-- | Indicates whether this IP address is the primary private IP address of the
-- network interface.
ipiaPrimary
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> InstancePrivateIpAddress
    -> f InstancePrivateIpAddress
ipiaPrimary f x =
    (\y -> x { _ipiaPrimary = y })
       <$> f (_ipiaPrimary x)
{-# INLINE ipiaPrimary #-}

-- | The association information for an Elastic IP address for the network
-- interface.
ipiaAssociation
    :: Functor f
    => (Maybe InstanceNetworkInterfaceAssociation
    -> f (Maybe InstanceNetworkInterfaceAssociation))
    -> InstancePrivateIpAddress
    -> f InstancePrivateIpAddress
ipiaAssociation f x =
    (\y -> x { _ipiaAssociation = y })
       <$> f (_ipiaAssociation x)
{-# INLINE ipiaAssociation #-}

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
iifCode
    :: Functor f
    => (Integer
    -> f (Integer))
    -> InstanceState
    -> f InstanceState
iifCode f x =
    (\y -> x { _iifCode = y })
       <$> f (_iifCode x)
{-# INLINE iifCode #-}

-- | The current state of the instance.
iifName
    :: Functor f
    => (InstanceStateName
    -> f (InstanceStateName))
    -> InstanceState
    -> f InstanceState
iifName f x =
    (\y -> x { _iifName = y })
       <$> f (_iifName x)
{-# INLINE iifName #-}

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
iscInstanceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> InstanceStateChange
    -> f InstanceStateChange
iscInstanceId f x =
    (\y -> x { _iscInstanceId = y })
       <$> f (_iscInstanceId x)
{-# INLINE iscInstanceId #-}

-- | The current state of the instance.
iscCurrentState
    :: Functor f
    => (Maybe InstanceState
    -> f (Maybe InstanceState))
    -> InstanceStateChange
    -> f InstanceStateChange
iscCurrentState f x =
    (\y -> x { _iscCurrentState = y })
       <$> f (_iscCurrentState x)
{-# INLINE iscCurrentState #-}

-- | The previous state of the instance.
iscPreviousState
    :: Functor f
    => (Maybe InstanceState
    -> f (Maybe InstanceState))
    -> InstanceStateChange
    -> f InstanceStateChange
iscPreviousState f x =
    (\y -> x { _iscPreviousState = y })
       <$> f (_iscPreviousState x)
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
iiiiivInstanceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> InstanceStatus
    -> f InstanceStatus
iiiiivInstanceId f x =
    (\y -> x { _iiiiivInstanceId = y })
       <$> f (_iiiiivInstanceId x)
{-# INLINE iiiiivInstanceId #-}

-- | The Availability Zone of the instance.
iiiiivAvailabilityZone
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> InstanceStatus
    -> f InstanceStatus
iiiiivAvailabilityZone f x =
    (\y -> x { _iiiiivAvailabilityZone = y })
       <$> f (_iiiiivAvailabilityZone x)
{-# INLINE iiiiivAvailabilityZone #-}

-- | Extra information regarding events associated with the instance.
iiiiivEvents
    :: Functor f
    => ([InstanceStatusEvent]
    -> f ([InstanceStatusEvent]))
    -> InstanceStatus
    -> f InstanceStatus
iiiiivEvents f x =
    (\y -> x { _iiiiivEvents = y })
       <$> f (_iiiiivEvents x)
{-# INLINE iiiiivEvents #-}

-- | The intended state of the instance. DescribeInstanceStatus requires that an
-- instance be in the running state.
iiiiivInstanceState
    :: Functor f
    => (Maybe InstanceState
    -> f (Maybe InstanceState))
    -> InstanceStatus
    -> f InstanceStatus
iiiiivInstanceState f x =
    (\y -> x { _iiiiivInstanceState = y })
       <$> f (_iiiiivInstanceState x)
{-# INLINE iiiiivInstanceState #-}

-- | Reports impaired functionality that stems from issues related to the
-- systems that support an instance, such as hardware failures and network
-- connectivity problems.
iiiiivSystemStatus
    :: Functor f
    => (Maybe InstanceStatusSummary
    -> f (Maybe InstanceStatusSummary))
    -> InstanceStatus
    -> f InstanceStatus
iiiiivSystemStatus f x =
    (\y -> x { _iiiiivSystemStatus = y })
       <$> f (_iiiiivSystemStatus x)
{-# INLINE iiiiivSystemStatus #-}

-- | Reports impaired functionality that stems from issues internal to the
-- instance, such as impaired reachability.
iiiiivInstanceStatus
    :: Functor f
    => (Maybe InstanceStatusSummary
    -> f (Maybe InstanceStatusSummary))
    -> InstanceStatus
    -> f InstanceStatus
iiiiivInstanceStatus f x =
    (\y -> x { _iiiiivInstanceStatus = y })
       <$> f (_iiiiivInstanceStatus x)
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
isdName
    :: Functor f
    => (Maybe StatusName
    -> f (Maybe StatusName))
    -> InstanceStatusDetails
    -> f InstanceStatusDetails
isdName f x =
    (\y -> x { _isdName = y })
       <$> f (_isdName x)
{-# INLINE isdName #-}

-- | The status.
isdStatus
    :: Functor f
    => (Maybe StatusType
    -> f (Maybe StatusType))
    -> InstanceStatusDetails
    -> f InstanceStatusDetails
isdStatus f x =
    (\y -> x { _isdStatus = y })
       <$> f (_isdStatus x)
{-# INLINE isdStatus #-}

-- | The time when a status check failed. For an instance that was launched and
-- impaired, this is the time when the instance was launched.
isdImpairedSince
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> InstanceStatusDetails
    -> f InstanceStatusDetails
isdImpairedSince f x =
    (\y -> x { _isdImpairedSince = y })
       <$> f (_isdImpairedSince x)
{-# INLINE isdImpairedSince #-}

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
iseCode
    :: Functor f
    => (Maybe EventCode
    -> f (Maybe EventCode))
    -> InstanceStatusEvent
    -> f InstanceStatusEvent
iseCode f x =
    (\y -> x { _iseCode = y })
       <$> f (_iseCode x)
{-# INLINE iseCode #-}

-- | A description of the event.
iseDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> InstanceStatusEvent
    -> f InstanceStatusEvent
iseDescription f x =
    (\y -> x { _iseDescription = y })
       <$> f (_iseDescription x)
{-# INLINE iseDescription #-}

-- | The earliest scheduled start time for the event.
iseNotBefore
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> InstanceStatusEvent
    -> f InstanceStatusEvent
iseNotBefore f x =
    (\y -> x { _iseNotBefore = y })
       <$> f (_iseNotBefore x)
{-# INLINE iseNotBefore #-}

-- | The latest scheduled end time for the event.
iseNotAfter
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> InstanceStatusEvent
    -> f InstanceStatusEvent
iseNotAfter f x =
    (\y -> x { _iseNotAfter = y })
       <$> f (_iseNotAfter x)
{-# INLINE iseNotAfter #-}

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
issStatus
    :: Functor f
    => (Maybe SummaryStatus
    -> f (Maybe SummaryStatus))
    -> InstanceStatusSummary
    -> f InstanceStatusSummary
issStatus f x =
    (\y -> x { _issStatus = y })
       <$> f (_issStatus x)
{-# INLINE issStatus #-}

-- | The system instance health or application instance health.
issDetails
    :: Functor f
    => ([InstanceStatusDetails]
    -> f ([InstanceStatusDetails]))
    -> InstanceStatusSummary
    -> f InstanceStatusSummary
issDetails f x =
    (\y -> x { _issDetails = y })
       <$> f (_issDetails x)
{-# INLINE issDetails #-}

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
igInternetGatewayId
    :: Functor f
    => (Text
    -> f (Text))
    -> InternetGateway
    -> f InternetGateway
igInternetGatewayId f x =
    (\y -> x { _igInternetGatewayId = y })
       <$> f (_igInternetGatewayId x)
{-# INLINE igInternetGatewayId #-}

-- | Any VPCs attached to the Internet gateway.
igAttachments
    :: Functor f
    => ([InternetGatewayAttachment]
    -> f ([InternetGatewayAttachment]))
    -> InternetGateway
    -> f InternetGateway
igAttachments f x =
    (\y -> x { _igAttachments = y })
       <$> f (_igAttachments x)
{-# INLINE igAttachments #-}

-- | Any tags assigned to the Internet gateway.
igTags
    :: Functor f
    => ([Tag]
    -> f ([Tag]))
    -> InternetGateway
    -> f InternetGateway
igTags f x =
    (\y -> x { _igTags = y })
       <$> f (_igTags x)
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
igaVpcId
    :: Functor f
    => (Text
    -> f (Text))
    -> InternetGatewayAttachment
    -> f InternetGatewayAttachment
igaVpcId f x =
    (\y -> x { _igaVpcId = y })
       <$> f (_igaVpcId x)
{-# INLINE igaVpcId #-}

-- | The current state of the attachment.
igaState
    :: Functor f
    => (AttachmentStatus
    -> f (AttachmentStatus))
    -> InternetGatewayAttachment
    -> f InternetGatewayAttachment
igaState f x =
    (\y -> x { _igaState = y })
       <$> f (_igaState x)
{-# INLINE igaState #-}

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
ipIpProtocol
    :: Functor f
    => (Text
    -> f (Text))
    -> IpPermission
    -> f IpPermission
ipIpProtocol f x =
    (\y -> x { _ipIpProtocol = y })
       <$> f (_ipIpProtocol x)
{-# INLINE ipIpProtocol #-}

-- | The start of port range for the TCP and UDP protocols, or an ICMP type
-- number. A value of -1 indicates all ICMP types.
ipFromPort
    :: Functor f
    => (Integer
    -> f (Integer))
    -> IpPermission
    -> f IpPermission
ipFromPort f x =
    (\y -> x { _ipFromPort = y })
       <$> f (_ipFromPort x)
{-# INLINE ipFromPort #-}

-- | The end of port range for the TCP and UDP protocols, or an ICMP code. A
-- value of -1 indicates all ICMP codes for the specified ICMP type.
ipToPort
    :: Functor f
    => (Integer
    -> f (Integer))
    -> IpPermission
    -> f IpPermission
ipToPort f x =
    (\y -> x { _ipToPort = y })
       <$> f (_ipToPort x)
{-# INLINE ipToPort #-}

-- | One or more security group and AWS account ID pairs.
ipUserIdGroupPairs
    :: Functor f
    => ([UserIdGroupPair]
    -> f ([UserIdGroupPair]))
    -> IpPermission
    -> f IpPermission
ipUserIdGroupPairs f x =
    (\y -> x { _ipUserIdGroupPairs = y })
       <$> f (_ipUserIdGroupPairs x)
{-# INLINE ipUserIdGroupPairs #-}

-- | One or more IP ranges.
ipIpRanges
    :: Functor f
    => ([IpRange]
    -> f ([IpRange]))
    -> IpPermission
    -> f IpPermission
ipIpRanges f x =
    (\y -> x { _ipIpRanges = y })
       <$> f (_ipIpRanges x)
{-# INLINE ipIpRanges #-}

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
kpiKeyName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> KeyPairInfo
    -> f KeyPairInfo
kpiKeyName f x =
    (\y -> x { _kpiKeyName = y })
       <$> f (_kpiKeyName x)
{-# INLINE kpiKeyName #-}

-- | If you used CreateKeyPair to create the key pair, this is the SHA-1 digest
-- of the DER encoded private key. If you used ImportKeyPair to provide AWS
-- the public key, this is the MD5 public key fingerprint as specified in
-- section 4 of RFC4716.
kpiKeyFingerprint
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> KeyPairInfo
    -> f KeyPairInfo
kpiKeyFingerprint f x =
    (\y -> x { _kpiKeyFingerprint = y })
       <$> f (_kpiKeyFingerprint x)
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
lpUserId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> LaunchPermission
    -> f LaunchPermission
lpUserId f x =
    (\y -> x { _lpUserId = y })
       <$> f (_lpUserId x)
{-# INLINE lpUserId #-}

-- | The name of the group.
lpGroup
    :: Functor f
    => (Maybe PermissionGroup
    -> f (Maybe PermissionGroup))
    -> LaunchPermission
    -> f LaunchPermission
lpGroup f x =
    (\y -> x { _lpGroup = y })
       <$> f (_lpGroup x)
{-# INLINE lpGroup #-}

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
lpmAdd
    :: Functor f
    => ([LaunchPermission]
    -> f ([LaunchPermission]))
    -> LaunchPermissionModifications
    -> f LaunchPermissionModifications
lpmAdd f x =
    (\y -> x { _lpmAdd = y })
       <$> f (_lpmAdd x)
{-# INLINE lpmAdd #-}

-- | The AWS account ID to remove from the list of launch permissions for the
-- AMI.
lpmRemove
    :: Functor f
    => ([LaunchPermission]
    -> f ([LaunchPermission]))
    -> LaunchPermissionModifications
    -> f LaunchPermissionModifications
lpmRemove f x =
    (\y -> x { _lpmRemove = y })
       <$> f (_lpmRemove x)
{-# INLINE lpmRemove #-}

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
llnImageId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> LaunchSpecification
    -> f LaunchSpecification
llnImageId f x =
    (\y -> x { _llnImageId = y })
       <$> f (_llnImageId x)
{-# INLINE llnImageId #-}

-- | The name of the key pair.
llnKeyName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> LaunchSpecification
    -> f LaunchSpecification
llnKeyName f x =
    (\y -> x { _llnKeyName = y })
       <$> f (_llnKeyName x)
{-# INLINE llnKeyName #-}

-- | One or more security groups.
llnSecurityGroups
    :: Functor f
    => ([GroupIdentifier]
    -> f ([GroupIdentifier]))
    -> LaunchSpecification
    -> f LaunchSpecification
llnSecurityGroups f x =
    (\y -> x { _llnSecurityGroups = y })
       <$> f (_llnSecurityGroups x)
{-# INLINE llnSecurityGroups #-}

-- | The Base64-encoded MIME user data to make available to the instances.
llnUserData
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> LaunchSpecification
    -> f LaunchSpecification
llnUserData f x =
    (\y -> x { _llnUserData = y })
       <$> f (_llnUserData x)
{-# INLINE llnUserData #-}

-- | 
llnAddressingType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> LaunchSpecification
    -> f LaunchSpecification
llnAddressingType f x =
    (\y -> x { _llnAddressingType = y })
       <$> f (_llnAddressingType x)
{-# INLINE llnAddressingType #-}

-- | The instance type.
llnInstanceType
    :: Functor f
    => (Maybe InstanceType
    -> f (Maybe InstanceType))
    -> LaunchSpecification
    -> f LaunchSpecification
llnInstanceType f x =
    (\y -> x { _llnInstanceType = y })
       <$> f (_llnInstanceType x)
{-# INLINE llnInstanceType #-}

-- | The placement information for the instance.
llnPlacement
    :: Functor f
    => (Maybe SpotPlacement
    -> f (Maybe SpotPlacement))
    -> LaunchSpecification
    -> f LaunchSpecification
llnPlacement f x =
    (\y -> x { _llnPlacement = y })
       <$> f (_llnPlacement x)
{-# INLINE llnPlacement #-}

-- | The ID of the kernel.
llnKernelId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> LaunchSpecification
    -> f LaunchSpecification
llnKernelId f x =
    (\y -> x { _llnKernelId = y })
       <$> f (_llnKernelId x)
{-# INLINE llnKernelId #-}

-- | The ID of the RAM disk.
llnRamdiskId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> LaunchSpecification
    -> f LaunchSpecification
llnRamdiskId f x =
    (\y -> x { _llnRamdiskId = y })
       <$> f (_llnRamdiskId x)
{-# INLINE llnRamdiskId #-}

-- | One or more block device mapping entries.
llnBlockDeviceMappings
    :: Functor f
    => ([BlockDeviceMapping]
    -> f ([BlockDeviceMapping]))
    -> LaunchSpecification
    -> f LaunchSpecification
llnBlockDeviceMappings f x =
    (\y -> x { _llnBlockDeviceMappings = y })
       <$> f (_llnBlockDeviceMappings x)
{-# INLINE llnBlockDeviceMappings #-}

-- | Enables monitoring for the instance. Default: Disabled.
llnMonitoringEnabled
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> LaunchSpecification
    -> f LaunchSpecification
llnMonitoringEnabled f x =
    (\y -> x { _llnMonitoringEnabled = y })
       <$> f (_llnMonitoringEnabled x)
{-# INLINE llnMonitoringEnabled #-}

-- | The ID of the subnet in which to launch the Spot Instance.
llnSubnetId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> LaunchSpecification
    -> f LaunchSpecification
llnSubnetId f x =
    (\y -> x { _llnSubnetId = y })
       <$> f (_llnSubnetId x)
{-# INLINE llnSubnetId #-}

-- | One or more network interfaces.
llnNetworkInterfaces
    :: Functor f
    => ([InstanceNetworkInterfaceSpecification]
    -> f ([InstanceNetworkInterfaceSpecification]))
    -> LaunchSpecification
    -> f LaunchSpecification
llnNetworkInterfaces f x =
    (\y -> x { _llnNetworkInterfaces = y })
       <$> f (_llnNetworkInterfaces x)
{-# INLINE llnNetworkInterfaces #-}

-- | The IAM instance profile.
llnIamInstanceProfile
    :: Functor f
    => (Maybe IamInstanceProfileSpecification
    -> f (Maybe IamInstanceProfileSpecification))
    -> LaunchSpecification
    -> f LaunchSpecification
llnIamInstanceProfile f x =
    (\y -> x { _llnIamInstanceProfile = y })
       <$> f (_llnIamInstanceProfile x)
{-# INLINE llnIamInstanceProfile #-}

-- | Indicates whether the instance is optimized for EBS I/O. This optimization
-- provides dedicated throughput to Amazon EBS and an optimized configuration
-- stack to provide optimal EBS I/O performance. This optimization isn't
-- available with all instance types. Additional usage charges apply when
-- using an EBS Optimized instance. Default: false.
llnEbsOptimized
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> LaunchSpecification
    -> f LaunchSpecification
llnEbsOptimized f x =
    (\y -> x { _llnEbsOptimized = y })
       <$> f (_llnEbsOptimized x)
{-# INLINE llnEbsOptimized #-}

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
naNetworkAclId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> NetworkAcl
    -> f NetworkAcl
naNetworkAclId f x =
    (\y -> x { _naNetworkAclId = y })
       <$> f (_naNetworkAclId x)
{-# INLINE naNetworkAclId #-}

-- | The ID of the VPC for the network ACL.
naVpcId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> NetworkAcl
    -> f NetworkAcl
naVpcId f x =
    (\y -> x { _naVpcId = y })
       <$> f (_naVpcId x)
{-# INLINE naVpcId #-}

-- | Indicates whether this is the default network ACL for the VPC.
naIsDefault
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> NetworkAcl
    -> f NetworkAcl
naIsDefault f x =
    (\y -> x { _naIsDefault = y })
       <$> f (_naIsDefault x)
{-# INLINE naIsDefault #-}

-- | One or more entries (rules) in the network ACL.
naEntries
    :: Functor f
    => ([NetworkAclEntry]
    -> f ([NetworkAclEntry]))
    -> NetworkAcl
    -> f NetworkAcl
naEntries f x =
    (\y -> x { _naEntries = y })
       <$> f (_naEntries x)
{-# INLINE naEntries #-}

-- | Any associations between the network ACL and one or more subnets.
naAssociations
    :: Functor f
    => ([NetworkAclAssociation]
    -> f ([NetworkAclAssociation]))
    -> NetworkAcl
    -> f NetworkAcl
naAssociations f x =
    (\y -> x { _naAssociations = y })
       <$> f (_naAssociations x)
{-# INLINE naAssociations #-}

-- | Any tags assigned to the network ACL.
naTags
    :: Functor f
    => ([Tag]
    -> f ([Tag]))
    -> NetworkAcl
    -> f NetworkAcl
naTags f x =
    (\y -> x { _naTags = y })
       <$> f (_naTags x)
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
naaNetworkAclAssociationId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> NetworkAclAssociation
    -> f NetworkAclAssociation
naaNetworkAclAssociationId f x =
    (\y -> x { _naaNetworkAclAssociationId = y })
       <$> f (_naaNetworkAclAssociationId x)
{-# INLINE naaNetworkAclAssociationId #-}

-- | The ID of the network ACL.
naaNetworkAclId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> NetworkAclAssociation
    -> f NetworkAclAssociation
naaNetworkAclId f x =
    (\y -> x { _naaNetworkAclId = y })
       <$> f (_naaNetworkAclId x)
{-# INLINE naaNetworkAclId #-}

-- | The ID of the subnet.
naaSubnetId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> NetworkAclAssociation
    -> f NetworkAclAssociation
naaSubnetId f x =
    (\y -> x { _naaSubnetId = y })
       <$> f (_naaSubnetId x)
{-# INLINE naaSubnetId #-}

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
naeRuleNumber
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> NetworkAclEntry
    -> f NetworkAclEntry
naeRuleNumber f x =
    (\y -> x { _naeRuleNumber = y })
       <$> f (_naeRuleNumber x)
{-# INLINE naeRuleNumber #-}

-- | The protocol. A value of -1 means all protocols.
naeProtocol
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> NetworkAclEntry
    -> f NetworkAclEntry
naeProtocol f x =
    (\y -> x { _naeProtocol = y })
       <$> f (_naeProtocol x)
{-# INLINE naeProtocol #-}

-- | Indicates whether to allow or deny the traffic that matches the rule.
naeRuleAction
    :: Functor f
    => (Maybe RuleAction
    -> f (Maybe RuleAction))
    -> NetworkAclEntry
    -> f NetworkAclEntry
naeRuleAction f x =
    (\y -> x { _naeRuleAction = y })
       <$> f (_naeRuleAction x)
{-# INLINE naeRuleAction #-}

-- | Indicates whether the rule is an egress rule (applied to traffic leaving
-- the subnet).
naeEgress
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> NetworkAclEntry
    -> f NetworkAclEntry
naeEgress f x =
    (\y -> x { _naeEgress = y })
       <$> f (_naeEgress x)
{-# INLINE naeEgress #-}

-- | The network range to allow or deny, in CIDR notation.
naeCidrBlock
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> NetworkAclEntry
    -> f NetworkAclEntry
naeCidrBlock f x =
    (\y -> x { _naeCidrBlock = y })
       <$> f (_naeCidrBlock x)
{-# INLINE naeCidrBlock #-}

-- | ICMP protocol: The ICMP type and code.
naeIcmpTypeCode
    :: Functor f
    => (Maybe IcmpTypeCode
    -> f (Maybe IcmpTypeCode))
    -> NetworkAclEntry
    -> f NetworkAclEntry
naeIcmpTypeCode f x =
    (\y -> x { _naeIcmpTypeCode = y })
       <$> f (_naeIcmpTypeCode x)
{-# INLINE naeIcmpTypeCode #-}

-- | TCP or UDP protocols: The range of ports the rule applies to.
naePortRange
    :: Functor f
    => (Maybe PortRange
    -> f (Maybe PortRange))
    -> NetworkAclEntry
    -> f NetworkAclEntry
naePortRange f x =
    (\y -> x { _naePortRange = y })
       <$> f (_naePortRange x)
{-# INLINE naePortRange #-}

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
niNetworkInterfaceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> NetworkInterface
    -> f NetworkInterface
niNetworkInterfaceId f x =
    (\y -> x { _niNetworkInterfaceId = y })
       <$> f (_niNetworkInterfaceId x)
{-# INLINE niNetworkInterfaceId #-}

-- | The ID of the subnet.
niSubnetId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> NetworkInterface
    -> f NetworkInterface
niSubnetId f x =
    (\y -> x { _niSubnetId = y })
       <$> f (_niSubnetId x)
{-# INLINE niSubnetId #-}

-- | The ID of the VPC.
niVpcId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> NetworkInterface
    -> f NetworkInterface
niVpcId f x =
    (\y -> x { _niVpcId = y })
       <$> f (_niVpcId x)
{-# INLINE niVpcId #-}

-- | The Availability Zone.
niAvailabilityZone
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> NetworkInterface
    -> f NetworkInterface
niAvailabilityZone f x =
    (\y -> x { _niAvailabilityZone = y })
       <$> f (_niAvailabilityZone x)
{-# INLINE niAvailabilityZone #-}

-- | A description.
niDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> NetworkInterface
    -> f NetworkInterface
niDescription f x =
    (\y -> x { _niDescription = y })
       <$> f (_niDescription x)
{-# INLINE niDescription #-}

-- | The AWS account ID of the owner of the network interface.
niOwnerId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> NetworkInterface
    -> f NetworkInterface
niOwnerId f x =
    (\y -> x { _niOwnerId = y })
       <$> f (_niOwnerId x)
{-# INLINE niOwnerId #-}

-- | The ID of the entity that launched the instance on your behalf (for
-- example, AWS Management Console or Auto Scaling).
niRequesterId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> NetworkInterface
    -> f NetworkInterface
niRequesterId f x =
    (\y -> x { _niRequesterId = y })
       <$> f (_niRequesterId x)
{-# INLINE niRequesterId #-}

-- | Indicates whether the network interface is being managed by AWS.
niRequesterManaged
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> NetworkInterface
    -> f NetworkInterface
niRequesterManaged f x =
    (\y -> x { _niRequesterManaged = y })
       <$> f (_niRequesterManaged x)
{-# INLINE niRequesterManaged #-}

-- | The status of the network interface.
niStatus
    :: Functor f
    => (Maybe NetworkInterfaceStatus
    -> f (Maybe NetworkInterfaceStatus))
    -> NetworkInterface
    -> f NetworkInterface
niStatus f x =
    (\y -> x { _niStatus = y })
       <$> f (_niStatus x)
{-# INLINE niStatus #-}

-- | The MAC address.
niMacAddress
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> NetworkInterface
    -> f NetworkInterface
niMacAddress f x =
    (\y -> x { _niMacAddress = y })
       <$> f (_niMacAddress x)
{-# INLINE niMacAddress #-}

-- | The IP address of the network interface within the subnet.
niPrivateIpAddress
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> NetworkInterface
    -> f NetworkInterface
niPrivateIpAddress f x =
    (\y -> x { _niPrivateIpAddress = y })
       <$> f (_niPrivateIpAddress x)
{-# INLINE niPrivateIpAddress #-}

-- | The private DNS name.
niPrivateDnsName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> NetworkInterface
    -> f NetworkInterface
niPrivateDnsName f x =
    (\y -> x { _niPrivateDnsName = y })
       <$> f (_niPrivateDnsName x)
{-# INLINE niPrivateDnsName #-}

-- | Indicates whether traffic to or from the instance is validated.
niSourceDestCheck
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> NetworkInterface
    -> f NetworkInterface
niSourceDestCheck f x =
    (\y -> x { _niSourceDestCheck = y })
       <$> f (_niSourceDestCheck x)
{-# INLINE niSourceDestCheck #-}

-- | Any security groups for the network interface.
niGroups
    :: Functor f
    => ([GroupIdentifier]
    -> f ([GroupIdentifier]))
    -> NetworkInterface
    -> f NetworkInterface
niGroups f x =
    (\y -> x { _niGroups = y })
       <$> f (_niGroups x)
{-# INLINE niGroups #-}

-- | The network interface attachment.
niAttachment
    :: Functor f
    => (Maybe NetworkInterfaceAttachment
    -> f (Maybe NetworkInterfaceAttachment))
    -> NetworkInterface
    -> f NetworkInterface
niAttachment f x =
    (\y -> x { _niAttachment = y })
       <$> f (_niAttachment x)
{-# INLINE niAttachment #-}

-- | The association information for an Elastic IP associated with the network
-- interface.
niAssociation
    :: Functor f
    => (Maybe NetworkInterfaceAssociation
    -> f (Maybe NetworkInterfaceAssociation))
    -> NetworkInterface
    -> f NetworkInterface
niAssociation f x =
    (\y -> x { _niAssociation = y })
       <$> f (_niAssociation x)
{-# INLINE niAssociation #-}

-- | Any tags assigned to the network interface.
niTagSet
    :: Functor f
    => ([Tag]
    -> f ([Tag]))
    -> NetworkInterface
    -> f NetworkInterface
niTagSet f x =
    (\y -> x { _niTagSet = y })
       <$> f (_niTagSet x)
{-# INLINE niTagSet #-}

-- | The private IP addresses associated with the network interface.
niPrivateIpAddresses
    :: Functor f
    => ([NetworkInterfacePrivateIpAddress]
    -> f ([NetworkInterfacePrivateIpAddress]))
    -> NetworkInterface
    -> f NetworkInterface
niPrivateIpAddresses f x =
    (\y -> x { _niPrivateIpAddresses = y })
       <$> f (_niPrivateIpAddresses x)
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
nibPublicIp
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> NetworkInterfaceAssociation
    -> f NetworkInterfaceAssociation
nibPublicIp f x =
    (\y -> x { _nibPublicIp = y })
       <$> f (_nibPublicIp x)
{-# INLINE nibPublicIp #-}

-- | The public DNS name.
nibPublicDnsName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> NetworkInterfaceAssociation
    -> f NetworkInterfaceAssociation
nibPublicDnsName f x =
    (\y -> x { _nibPublicDnsName = y })
       <$> f (_nibPublicDnsName x)
{-# INLINE nibPublicDnsName #-}

-- | The ID of the Elastic IP address owner.
nibIpOwnerId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> NetworkInterfaceAssociation
    -> f NetworkInterfaceAssociation
nibIpOwnerId f x =
    (\y -> x { _nibIpOwnerId = y })
       <$> f (_nibIpOwnerId x)
{-# INLINE nibIpOwnerId #-}

-- | The allocation ID.
nibAllocationId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> NetworkInterfaceAssociation
    -> f NetworkInterfaceAssociation
nibAllocationId f x =
    (\y -> x { _nibAllocationId = y })
       <$> f (_nibAllocationId x)
{-# INLINE nibAllocationId #-}

-- | The association ID.
nibAssociationId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> NetworkInterfaceAssociation
    -> f NetworkInterfaceAssociation
nibAssociationId f x =
    (\y -> x { _nibAssociationId = y })
       <$> f (_nibAssociationId x)
{-# INLINE nibAssociationId #-}

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
niaAttachmentId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> NetworkInterfaceAttachment
    -> f NetworkInterfaceAttachment
niaAttachmentId f x =
    (\y -> x { _niaAttachmentId = y })
       <$> f (_niaAttachmentId x)
{-# INLINE niaAttachmentId #-}

-- | The ID of the instance.
niaInstanceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> NetworkInterfaceAttachment
    -> f NetworkInterfaceAttachment
niaInstanceId f x =
    (\y -> x { _niaInstanceId = y })
       <$> f (_niaInstanceId x)
{-# INLINE niaInstanceId #-}

-- | The AWS account ID of the owner of the instance.
niaInstanceOwnerId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> NetworkInterfaceAttachment
    -> f NetworkInterfaceAttachment
niaInstanceOwnerId f x =
    (\y -> x { _niaInstanceOwnerId = y })
       <$> f (_niaInstanceOwnerId x)
{-# INLINE niaInstanceOwnerId #-}

-- | The device index of the network interface attachment on the instance.
niaDeviceIndex
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> NetworkInterfaceAttachment
    -> f NetworkInterfaceAttachment
niaDeviceIndex f x =
    (\y -> x { _niaDeviceIndex = y })
       <$> f (_niaDeviceIndex x)
{-# INLINE niaDeviceIndex #-}

-- | The attachment state.
niaStatus
    :: Functor f
    => (Maybe AttachmentStatus
    -> f (Maybe AttachmentStatus))
    -> NetworkInterfaceAttachment
    -> f NetworkInterfaceAttachment
niaStatus f x =
    (\y -> x { _niaStatus = y })
       <$> f (_niaStatus x)
{-# INLINE niaStatus #-}

-- | The timestamp indicating when the attachment initiated.
niaAttachTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> NetworkInterfaceAttachment
    -> f NetworkInterfaceAttachment
niaAttachTime f x =
    (\y -> x { _niaAttachTime = y })
       <$> f (_niaAttachTime x)
{-# INLINE niaAttachTime #-}

-- | Indicates whether the network interface is deleted when the instance is
-- terminated.
niaDeleteOnTermination
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> NetworkInterfaceAttachment
    -> f NetworkInterfaceAttachment
niaDeleteOnTermination f x =
    (\y -> x { _niaDeleteOnTermination = y })
       <$> f (_niaDeleteOnTermination x)
{-# INLINE niaDeleteOnTermination #-}

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
niacAttachmentId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> NetworkInterfaceAttachmentChanges
    -> f NetworkInterfaceAttachmentChanges
niacAttachmentId f x =
    (\y -> x { _niacAttachmentId = y })
       <$> f (_niacAttachmentId x)
{-# INLINE niacAttachmentId #-}

-- | Indicates whether the network interface is deleted when the instance is
-- terminated.
niacDeleteOnTermination
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> NetworkInterfaceAttachmentChanges
    -> f NetworkInterfaceAttachmentChanges
niacDeleteOnTermination f x =
    (\y -> x { _niacDeleteOnTermination = y })
       <$> f (_niacDeleteOnTermination x)
{-# INLINE niacDeleteOnTermination #-}

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
nipiaPrivateIpAddress
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> NetworkInterfacePrivateIpAddress
    -> f NetworkInterfacePrivateIpAddress
nipiaPrivateIpAddress f x =
    (\y -> x { _nipiaPrivateIpAddress = y })
       <$> f (_nipiaPrivateIpAddress x)
{-# INLINE nipiaPrivateIpAddress #-}

-- | The private DNS name.
nipiaPrivateDnsName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> NetworkInterfacePrivateIpAddress
    -> f NetworkInterfacePrivateIpAddress
nipiaPrivateDnsName f x =
    (\y -> x { _nipiaPrivateDnsName = y })
       <$> f (_nipiaPrivateDnsName x)
{-# INLINE nipiaPrivateDnsName #-}

-- | Indicates whether this IP address is the primary private IP address of the
-- network interface.
nipiaPrimary
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> NetworkInterfacePrivateIpAddress
    -> f NetworkInterfacePrivateIpAddress
nipiaPrimary f x =
    (\y -> x { _nipiaPrimary = y })
       <$> f (_nipiaPrimary x)
{-# INLINE nipiaPrimary #-}

-- | The association information for an Elastic IP address associated with the
-- network interface.
nipiaAssociation
    :: Functor f
    => (Maybe NetworkInterfaceAssociation
    -> f (Maybe NetworkInterfaceAssociation))
    -> NetworkInterfacePrivateIpAddress
    -> f NetworkInterfacePrivateIpAddress
nipiaAssociation f x =
    (\y -> x { _nipiaAssociation = y })
       <$> f (_nipiaAssociation x)
{-# INLINE nipiaAssociation #-}

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
pzAvailabilityZone
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Placement
    -> f Placement
pzAvailabilityZone f x =
    (\y -> x { _pzAvailabilityZone = y })
       <$> f (_pzAvailabilityZone x)
{-# INLINE pzAvailabilityZone #-}

-- | The name of the placement group the instance is in (for cluster compute
-- instances).
pzGroupName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Placement
    -> f Placement
pzGroupName f x =
    (\y -> x { _pzGroupName = y })
       <$> f (_pzGroupName x)
{-# INLINE pzGroupName #-}

-- | The tenancy of the instance (if the instance is running in a VPC). An
-- instance with a tenancy of dedicated runs on single-tenant hardware.
pzTenancy
    :: Functor f
    => (Maybe Tenancy
    -> f (Maybe Tenancy))
    -> Placement
    -> f Placement
pzTenancy f x =
    (\y -> x { _pzTenancy = y })
       <$> f (_pzTenancy x)
{-# INLINE pzTenancy #-}

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
phGroupName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PlacementGroup
    -> f PlacementGroup
phGroupName f x =
    (\y -> x { _phGroupName = y })
       <$> f (_phGroupName x)
{-# INLINE phGroupName #-}

-- | The placement strategy.
phStrategy
    :: Functor f
    => (Maybe PlacementStrategy
    -> f (Maybe PlacementStrategy))
    -> PlacementGroup
    -> f PlacementGroup
phStrategy f x =
    (\y -> x { _phStrategy = y })
       <$> f (_phStrategy x)
{-# INLINE phStrategy #-}

-- | The state of the placement group.
phState
    :: Functor f
    => (Maybe PlacementGroupState
    -> f (Maybe PlacementGroupState))
    -> PlacementGroup
    -> f PlacementGroup
phState f x =
    (\y -> x { _phState = y })
       <$> f (_phState x)
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
prFrom
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> PortRange
    -> f PortRange
prFrom f x =
    (\y -> x { _prFrom = y })
       <$> f (_prFrom x)
{-# INLINE prFrom #-}

-- | The last port in the range.
prTo
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> PortRange
    -> f PortRange
prTo f x =
    (\y -> x { _prTo = y })
       <$> f (_prTo x)
{-# INLINE prTo #-}

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
psTerm
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> PriceSchedule
    -> f PriceSchedule
psTerm f x =
    (\y -> x { _psTerm = y })
       <$> f (_psTerm x)
{-# INLINE psTerm #-}

-- | The fixed price for the term.
psPrice
    :: Functor f
    => (Maybe Double
    -> f (Maybe Double))
    -> PriceSchedule
    -> f PriceSchedule
psPrice f x =
    (\y -> x { _psPrice = y })
       <$> f (_psPrice x)
{-# INLINE psPrice #-}

-- | The currency for transacting the Reserved Instance resale. At this time,
-- the only supported currency is USD.
psCurrencyCode
    :: Functor f
    => (Maybe CurrencyCodeValues
    -> f (Maybe CurrencyCodeValues))
    -> PriceSchedule
    -> f PriceSchedule
psCurrencyCode f x =
    (\y -> x { _psCurrencyCode = y })
       <$> f (_psCurrencyCode x)
{-# INLINE psCurrencyCode #-}

-- | The current price schedule, as determined by the term remaining for the
-- Reserved Instance in the listing. A specific price schedule is always in
-- effect, but only one price schedule can be active at any time. Take, for
-- example, a Reserved Instance listing that has five months remaining in its
-- term. When you specify price schedules for five months and two months, this
-- means that schedule 1, covering the first three months of the remaining
-- term, will be active during months 5, 4, and 3. Then schedule 2, covering
-- the last two months of the term, will be active for months 2 and 1.
psActive
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> PriceSchedule
    -> f PriceSchedule
psActive f x =
    (\y -> x { _psActive = y })
       <$> f (_psActive x)
{-# INLINE psActive #-}

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
pssTerm
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> PriceScheduleSpecification
    -> f PriceScheduleSpecification
pssTerm f x =
    (\y -> x { _pssTerm = y })
       <$> f (_pssTerm x)
{-# INLINE pssTerm #-}

-- | The fixed price for the term.
pssPrice
    :: Functor f
    => (Maybe Double
    -> f (Maybe Double))
    -> PriceScheduleSpecification
    -> f PriceScheduleSpecification
pssPrice f x =
    (\y -> x { _pssPrice = y })
       <$> f (_pssPrice x)
{-# INLINE pssPrice #-}

-- | The currency for transacting the Reserved Instance resale. At this time,
-- the only supported currency is USD.
pssCurrencyCode
    :: Functor f
    => (Maybe CurrencyCodeValues
    -> f (Maybe CurrencyCodeValues))
    -> PriceScheduleSpecification
    -> f PriceScheduleSpecification
pssCurrencyCode f x =
    (\y -> x { _pssCurrencyCode = y })
       <$> f (_pssCurrencyCode x)
{-# INLINE pssCurrencyCode #-}

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
piPrice
    :: Functor f
    => (Maybe Double
    -> f (Maybe Double))
    -> PricingDetail
    -> f PricingDetail
piPrice f x =
    (\y -> x { _piPrice = y })
       <$> f (_piPrice x)
{-# INLINE piPrice #-}

-- | The number of instances available for the price.
piCount
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> PricingDetail
    -> f PricingDetail
piCount f x =
    (\y -> x { _piCount = y })
       <$> f (_piCount x)
{-# INLINE piCount #-}

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
piasPrivateIpAddress
    :: Functor f
    => (Text
    -> f (Text))
    -> PrivateIpAddressSpecification
    -> f PrivateIpAddressSpecification
piasPrivateIpAddress f x =
    (\y -> x { _piasPrivateIpAddress = y })
       <$> f (_piasPrivateIpAddress x)
{-# INLINE piasPrivateIpAddress #-}

-- | Indicates whether the private IP address is the primary private IP address.
piasPrimary
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> PrivateIpAddressSpecification
    -> f PrivateIpAddressSpecification
piasPrimary f x =
    (\y -> x { _piasPrimary = y })
       <$> f (_piasPrimary x)
{-# INLINE piasPrimary #-}

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
pcProductCodeId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ProductCode
    -> f ProductCode
pcProductCodeId f x =
    (\y -> x { _pcProductCodeId = y })
       <$> f (_pcProductCodeId x)
{-# INLINE pcProductCodeId #-}

-- | The type of product code.
pcProductCodeType
    :: Functor f
    => (Maybe ProductCodeValues
    -> f (Maybe ProductCodeValues))
    -> ProductCode
    -> f ProductCode
pcProductCodeType f x =
    (\y -> x { _pcProductCodeType = y })
       <$> f (_pcProductCodeType x)
{-# INLINE pcProductCodeType #-}

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
rdFrequency
    :: Functor f
    => (Maybe RecurringChargeFrequency
    -> f (Maybe RecurringChargeFrequency))
    -> RecurringCharge
    -> f RecurringCharge
rdFrequency f x =
    (\y -> x { _rdFrequency = y })
       <$> f (_rdFrequency x)
{-# INLINE rdFrequency #-}

-- | The amount of the recurring charge.
rdAmount
    :: Functor f
    => (Maybe Double
    -> f (Maybe Double))
    -> RecurringCharge
    -> f RecurringCharge
rdAmount f x =
    (\y -> x { _rdAmount = y })
       <$> f (_rdAmount x)
{-# INLINE rdAmount #-}

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
rqRegionName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Region
    -> f Region
rqRegionName f x =
    (\y -> x { _rqRegionName = y })
       <$> f (_rqRegionName x)
{-# INLINE rqRegionName #-}

-- | The region service endpoint.
rqEndpoint
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Region
    -> f Region
rqEndpoint f x =
    (\y -> x { _rqEndpoint = y })
       <$> f (_rqEndpoint x)
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
rnReservationId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Reservation
    -> f Reservation
rnReservationId f x =
    (\y -> x { _rnReservationId = y })
       <$> f (_rnReservationId x)
{-# INLINE rnReservationId #-}

-- | The ID of the AWS account that owns the reservation.
rnOwnerId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Reservation
    -> f Reservation
rnOwnerId f x =
    (\y -> x { _rnOwnerId = y })
       <$> f (_rnOwnerId x)
{-# INLINE rnOwnerId #-}

-- | The ID of the requester that launched the instances on your behalf (for
-- example, AWS Management Console or Auto Scaling).
rnRequesterId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Reservation
    -> f Reservation
rnRequesterId f x =
    (\y -> x { _rnRequesterId = y })
       <$> f (_rnRequesterId x)
{-# INLINE rnRequesterId #-}

-- | One or more security groups.
rnGroups
    :: Functor f
    => ([GroupIdentifier]
    -> f ([GroupIdentifier]))
    -> Reservation
    -> f Reservation
rnGroups f x =
    (\y -> x { _rnGroups = y })
       <$> f (_rnGroups x)
{-# INLINE rnGroups #-}

-- | One or more instances.
rnInstances
    :: Functor f
    => ([Instance]
    -> f ([Instance]))
    -> Reservation
    -> f Reservation
rnInstances f x =
    (\y -> x { _rnInstances = y })
       <$> f (_rnInstances x)
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
rilpAmount
    :: Functor f
    => (Maybe Double
    -> f (Maybe Double))
    -> ReservedInstanceLimitPrice
    -> f ReservedInstanceLimitPrice
rilpAmount f x =
    (\y -> x { _rilpAmount = y })
       <$> f (_rilpAmount x)
{-# INLINE rilpAmount #-}

-- | The currency in which the limitPrice amount is specified. At this time, the
-- only supported currency is USD.
rilpCurrencyCode
    :: Functor f
    => (Maybe CurrencyCodeValues
    -> f (Maybe CurrencyCodeValues))
    -> ReservedInstanceLimitPrice
    -> f ReservedInstanceLimitPrice
rilpCurrencyCode f x =
    (\y -> x { _rilpCurrencyCode = y })
       <$> f (_rilpCurrencyCode x)
{-# INLINE rilpCurrencyCode #-}

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
riReservedInstancesId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedInstances
    -> f ReservedInstances
riReservedInstancesId f x =
    (\y -> x { _riReservedInstancesId = y })
       <$> f (_riReservedInstancesId x)
{-# INLINE riReservedInstancesId #-}

-- | The instance type on which the Reserved Instance can be used.
riInstanceType
    :: Functor f
    => (Maybe InstanceType
    -> f (Maybe InstanceType))
    -> ReservedInstances
    -> f ReservedInstances
riInstanceType f x =
    (\y -> x { _riInstanceType = y })
       <$> f (_riInstanceType x)
{-# INLINE riInstanceType #-}

-- | The Availability Zone in which the Reserved Instance can be used.
riAvailabilityZone
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedInstances
    -> f ReservedInstances
riAvailabilityZone f x =
    (\y -> x { _riAvailabilityZone = y })
       <$> f (_riAvailabilityZone x)
{-# INLINE riAvailabilityZone #-}

-- | The date and time the Reserved Instance started.
riStart
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> ReservedInstances
    -> f ReservedInstances
riStart f x =
    (\y -> x { _riStart = y })
       <$> f (_riStart x)
{-# INLINE riStart #-}

-- | The time when the Reserved Instance expires.
riEnd
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> ReservedInstances
    -> f ReservedInstances
riEnd f x =
    (\y -> x { _riEnd = y })
       <$> f (_riEnd x)
{-# INLINE riEnd #-}

-- | The duration of the Reserved Instance, in seconds.
riDuration
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> ReservedInstances
    -> f ReservedInstances
riDuration f x =
    (\y -> x { _riDuration = y })
       <$> f (_riDuration x)
{-# INLINE riDuration #-}

-- | The usage price of the Reserved Instance, per hour.
riUsagePrice
    :: Functor f
    => (Maybe Double
    -> f (Maybe Double))
    -> ReservedInstances
    -> f ReservedInstances
riUsagePrice f x =
    (\y -> x { _riUsagePrice = y })
       <$> f (_riUsagePrice x)
{-# INLINE riUsagePrice #-}

-- | The purchase price of the Reserved Instance.
riFixedPrice
    :: Functor f
    => (Maybe Double
    -> f (Maybe Double))
    -> ReservedInstances
    -> f ReservedInstances
riFixedPrice f x =
    (\y -> x { _riFixedPrice = y })
       <$> f (_riFixedPrice x)
{-# INLINE riFixedPrice #-}

-- | The number of Reserved Instances purchased.
riInstanceCount
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> ReservedInstances
    -> f ReservedInstances
riInstanceCount f x =
    (\y -> x { _riInstanceCount = y })
       <$> f (_riInstanceCount x)
{-# INLINE riInstanceCount #-}

-- | The Reserved Instance description.
riProductDescription
    :: Functor f
    => (Maybe RIProductDescription
    -> f (Maybe RIProductDescription))
    -> ReservedInstances
    -> f ReservedInstances
riProductDescription f x =
    (\y -> x { _riProductDescription = y })
       <$> f (_riProductDescription x)
{-# INLINE riProductDescription #-}

-- | The state of the Reserved Instance purchase.
riState
    :: Functor f
    => (Maybe ReservedInstanceState
    -> f (Maybe ReservedInstanceState))
    -> ReservedInstances
    -> f ReservedInstances
riState f x =
    (\y -> x { _riState = y })
       <$> f (_riState x)
{-# INLINE riState #-}

-- | Any tags assigned to the resource.
riTags
    :: Functor f
    => ([Tag]
    -> f ([Tag]))
    -> ReservedInstances
    -> f ReservedInstances
riTags f x =
    (\y -> x { _riTags = y })
       <$> f (_riTags x)
{-# INLINE riTags #-}

-- | The tenancy of the reserved instance.
riInstanceTenancy
    :: Functor f
    => (Maybe Tenancy
    -> f (Maybe Tenancy))
    -> ReservedInstances
    -> f ReservedInstances
riInstanceTenancy f x =
    (\y -> x { _riInstanceTenancy = y })
       <$> f (_riInstanceTenancy x)
{-# INLINE riInstanceTenancy #-}

-- | The currency of the Reserved Instance. It's specified using ISO 4217
-- standard currency codes. At this time, the only supported currency is USD.
riCurrencyCode
    :: Functor f
    => (Maybe CurrencyCodeValues
    -> f (Maybe CurrencyCodeValues))
    -> ReservedInstances
    -> f ReservedInstances
riCurrencyCode f x =
    (\y -> x { _riCurrencyCode = y })
       <$> f (_riCurrencyCode x)
{-# INLINE riCurrencyCode #-}

-- | The Reserved Instance offering type.
riOfferingType
    :: Functor f
    => (Maybe OfferingTypeValues
    -> f (Maybe OfferingTypeValues))
    -> ReservedInstances
    -> f ReservedInstances
riOfferingType f x =
    (\y -> x { _riOfferingType = y })
       <$> f (_riOfferingType x)
{-# INLINE riOfferingType #-}

-- | The recurring charge tag assigned to the resource.
riRecurringCharges
    :: Functor f
    => ([RecurringCharge]
    -> f ([RecurringCharge]))
    -> ReservedInstances
    -> f ReservedInstances
riRecurringCharges f x =
    (\y -> x { _riRecurringCharges = y })
       <$> f (_riRecurringCharges x)
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
ricAvailabilityZone
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedInstancesConfiguration
    -> f ReservedInstancesConfiguration
ricAvailabilityZone f x =
    (\y -> x { _ricAvailabilityZone = y })
       <$> f (_ricAvailabilityZone x)
{-# INLINE ricAvailabilityZone #-}

-- | The network platform of the modified Reserved Instances, which is either
-- EC2-Classic or EC2-VPC.
ricPlatform
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedInstancesConfiguration
    -> f ReservedInstancesConfiguration
ricPlatform f x =
    (\y -> x { _ricPlatform = y })
       <$> f (_ricPlatform x)
{-# INLINE ricPlatform #-}

-- | The number of modified Reserved Instances.
ricInstanceCount
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> ReservedInstancesConfiguration
    -> f ReservedInstancesConfiguration
ricInstanceCount f x =
    (\y -> x { _ricInstanceCount = y })
       <$> f (_ricInstanceCount x)
{-# INLINE ricInstanceCount #-}

-- | The instance type for the modified Reserved Instances.
ricInstanceType
    :: Functor f
    => (Maybe InstanceType
    -> f (Maybe InstanceType))
    -> ReservedInstancesConfiguration
    -> f ReservedInstancesConfiguration
ricInstanceType f x =
    (\y -> x { _ricInstanceType = y })
       <$> f (_ricInstanceType x)
{-# INLINE ricInstanceType #-}

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
rilReservedInstancesListingId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedInstancesListing
    -> f ReservedInstancesListing
rilReservedInstancesListingId f x =
    (\y -> x { _rilReservedInstancesListingId = y })
       <$> f (_rilReservedInstancesListingId x)
{-# INLINE rilReservedInstancesListingId #-}

-- | The ID of the Reserved Instance.
rilReservedInstancesId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedInstancesListing
    -> f ReservedInstancesListing
rilReservedInstancesId f x =
    (\y -> x { _rilReservedInstancesId = y })
       <$> f (_rilReservedInstancesId x)
{-# INLINE rilReservedInstancesId #-}

-- | The time the listing was created.
rilCreateDate
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> ReservedInstancesListing
    -> f ReservedInstancesListing
rilCreateDate f x =
    (\y -> x { _rilCreateDate = y })
       <$> f (_rilCreateDate x)
{-# INLINE rilCreateDate #-}

-- | The last modified timestamp of the listing.
rilUpdateDate
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> ReservedInstancesListing
    -> f ReservedInstancesListing
rilUpdateDate f x =
    (\y -> x { _rilUpdateDate = y })
       <$> f (_rilUpdateDate x)
{-# INLINE rilUpdateDate #-}

-- | The status of the Reserved Instance listing.
rilStatus
    :: Functor f
    => (Maybe ListingStatus
    -> f (Maybe ListingStatus))
    -> ReservedInstancesListing
    -> f ReservedInstancesListing
rilStatus f x =
    (\y -> x { _rilStatus = y })
       <$> f (_rilStatus x)
{-# INLINE rilStatus #-}

-- | The reason for the current status of the Reserved Instance listing. The
-- response can be blank.
rilStatusMessage
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedInstancesListing
    -> f ReservedInstancesListing
rilStatusMessage f x =
    (\y -> x { _rilStatusMessage = y })
       <$> f (_rilStatusMessage x)
{-# INLINE rilStatusMessage #-}

-- | The number of instances in this state.
rilInstanceCounts
    :: Functor f
    => ([InstanceCount]
    -> f ([InstanceCount]))
    -> ReservedInstancesListing
    -> f ReservedInstancesListing
rilInstanceCounts f x =
    (\y -> x { _rilInstanceCounts = y })
       <$> f (_rilInstanceCounts x)
{-# INLINE rilInstanceCounts #-}

-- | The price of the Reserved Instance listing.
rilPriceSchedules
    :: Functor f
    => ([PriceSchedule]
    -> f ([PriceSchedule]))
    -> ReservedInstancesListing
    -> f ReservedInstancesListing
rilPriceSchedules f x =
    (\y -> x { _rilPriceSchedules = y })
       <$> f (_rilPriceSchedules x)
{-# INLINE rilPriceSchedules #-}

-- | Any tags assigned to the resource.
rilTags
    :: Functor f
    => ([Tag]
    -> f ([Tag]))
    -> ReservedInstancesListing
    -> f ReservedInstancesListing
rilTags f x =
    (\y -> x { _rilTags = y })
       <$> f (_rilTags x)
{-# INLINE rilTags #-}

-- | The idempotency token you provided when you created the listing.
rilClientToken
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedInstancesListing
    -> f ReservedInstancesListing
rilClientToken f x =
    (\y -> x { _rilClientToken = y })
       <$> f (_rilClientToken x)
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
rirReservedInstancesModificationId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedInstancesModification
    -> f ReservedInstancesModification
rirReservedInstancesModificationId f x =
    (\y -> x { _rirReservedInstancesModificationId = y })
       <$> f (_rirReservedInstancesModificationId x)
{-# INLINE rirReservedInstancesModificationId #-}

-- | The IDs of one or more Reserved Instances.
rirReservedInstancesIds
    :: Functor f
    => ([ReservedInstancesId]
    -> f ([ReservedInstancesId]))
    -> ReservedInstancesModification
    -> f ReservedInstancesModification
rirReservedInstancesIds f x =
    (\y -> x { _rirReservedInstancesIds = y })
       <$> f (_rirReservedInstancesIds x)
{-# INLINE rirReservedInstancesIds #-}

-- | Contains target configurations along with their corresponding new Reserved
-- Instance IDs.
rirModificationResults
    :: Functor f
    => ([ReservedInstancesModificationResult]
    -> f ([ReservedInstancesModificationResult]))
    -> ReservedInstancesModification
    -> f ReservedInstancesModification
rirModificationResults f x =
    (\y -> x { _rirModificationResults = y })
       <$> f (_rirModificationResults x)
{-# INLINE rirModificationResults #-}

-- | The time when the modification request was created.
rirCreateDate
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> ReservedInstancesModification
    -> f ReservedInstancesModification
rirCreateDate f x =
    (\y -> x { _rirCreateDate = y })
       <$> f (_rirCreateDate x)
{-# INLINE rirCreateDate #-}

-- | The time when the modification request was last updated.
rirUpdateDate
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> ReservedInstancesModification
    -> f ReservedInstancesModification
rirUpdateDate f x =
    (\y -> x { _rirUpdateDate = y })
       <$> f (_rirUpdateDate x)
{-# INLINE rirUpdateDate #-}

-- | The time for the modification to become effective.
rirEffectiveDate
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> ReservedInstancesModification
    -> f ReservedInstancesModification
rirEffectiveDate f x =
    (\y -> x { _rirEffectiveDate = y })
       <$> f (_rirEffectiveDate x)
{-# INLINE rirEffectiveDate #-}

-- | The status of the Reserved Instances modification request.
rirStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedInstancesModification
    -> f ReservedInstancesModification
rirStatus f x =
    (\y -> x { _rirStatus = y })
       <$> f (_rirStatus x)
{-# INLINE rirStatus #-}

-- | The reason for the status.
rirStatusMessage
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedInstancesModification
    -> f ReservedInstancesModification
rirStatusMessage f x =
    (\y -> x { _rirStatusMessage = y })
       <$> f (_rirStatusMessage x)
{-# INLINE rirStatusMessage #-}

-- | A unique, case-sensitive key supplied by the client to ensure that the
-- modification request is idempotent.
rirClientToken
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedInstancesModification
    -> f ReservedInstancesModification
rirClientToken f x =
    (\y -> x { _rirClientToken = y })
       <$> f (_rirClientToken x)
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
rimrReservedInstancesId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedInstancesModificationResult
    -> f ReservedInstancesModificationResult
rimrReservedInstancesId f x =
    (\y -> x { _rimrReservedInstancesId = y })
       <$> f (_rimrReservedInstancesId x)
{-# INLINE rimrReservedInstancesId #-}

-- | The target Reserved Instances configurations supplied as part of the
-- modification request.
rimrTargetConfiguration
    :: Functor f
    => (Maybe ReservedInstancesConfiguration
    -> f (Maybe ReservedInstancesConfiguration))
    -> ReservedInstancesModificationResult
    -> f ReservedInstancesModificationResult
rimrTargetConfiguration f x =
    (\y -> x { _rimrTargetConfiguration = y })
       <$> f (_rimrTargetConfiguration x)
{-# INLINE rimrTargetConfiguration #-}

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
ritReservedInstancesOfferingId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedInstancesOffering
    -> f ReservedInstancesOffering
ritReservedInstancesOfferingId f x =
    (\y -> x { _ritReservedInstancesOfferingId = y })
       <$> f (_ritReservedInstancesOfferingId x)
{-# INLINE ritReservedInstancesOfferingId #-}

-- | The instance type on which the Reserved Instance can be used.
ritInstanceType
    :: Functor f
    => (Maybe InstanceType
    -> f (Maybe InstanceType))
    -> ReservedInstancesOffering
    -> f ReservedInstancesOffering
ritInstanceType f x =
    (\y -> x { _ritInstanceType = y })
       <$> f (_ritInstanceType x)
{-# INLINE ritInstanceType #-}

-- | The Availability Zone in which the Reserved Instance can be used.
ritAvailabilityZone
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedInstancesOffering
    -> f ReservedInstancesOffering
ritAvailabilityZone f x =
    (\y -> x { _ritAvailabilityZone = y })
       <$> f (_ritAvailabilityZone x)
{-# INLINE ritAvailabilityZone #-}

-- | The duration of the Reserved Instance, in seconds.
ritDuration
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> ReservedInstancesOffering
    -> f ReservedInstancesOffering
ritDuration f x =
    (\y -> x { _ritDuration = y })
       <$> f (_ritDuration x)
{-# INLINE ritDuration #-}

-- | The usage price of the Reserved Instance, per hour.
ritUsagePrice
    :: Functor f
    => (Maybe Double
    -> f (Maybe Double))
    -> ReservedInstancesOffering
    -> f ReservedInstancesOffering
ritUsagePrice f x =
    (\y -> x { _ritUsagePrice = y })
       <$> f (_ritUsagePrice x)
{-# INLINE ritUsagePrice #-}

-- | The purchase price of the Reserved Instance.
ritFixedPrice
    :: Functor f
    => (Maybe Double
    -> f (Maybe Double))
    -> ReservedInstancesOffering
    -> f ReservedInstancesOffering
ritFixedPrice f x =
    (\y -> x { _ritFixedPrice = y })
       <$> f (_ritFixedPrice x)
{-# INLINE ritFixedPrice #-}

-- | The Reserved Instance description.
ritProductDescription
    :: Functor f
    => (Maybe RIProductDescription
    -> f (Maybe RIProductDescription))
    -> ReservedInstancesOffering
    -> f ReservedInstancesOffering
ritProductDescription f x =
    (\y -> x { _ritProductDescription = y })
       <$> f (_ritProductDescription x)
{-# INLINE ritProductDescription #-}

-- | The tenancy of the reserved instance.
ritInstanceTenancy
    :: Functor f
    => (Maybe Tenancy
    -> f (Maybe Tenancy))
    -> ReservedInstancesOffering
    -> f ReservedInstancesOffering
ritInstanceTenancy f x =
    (\y -> x { _ritInstanceTenancy = y })
       <$> f (_ritInstanceTenancy x)
{-# INLINE ritInstanceTenancy #-}

-- | The currency of the Reserved Instance offering you are purchasing. It's
-- specified using ISO 4217 standard currency codes. At this time, the only
-- supported currency is USD.
ritCurrencyCode
    :: Functor f
    => (Maybe CurrencyCodeValues
    -> f (Maybe CurrencyCodeValues))
    -> ReservedInstancesOffering
    -> f ReservedInstancesOffering
ritCurrencyCode f x =
    (\y -> x { _ritCurrencyCode = y })
       <$> f (_ritCurrencyCode x)
{-# INLINE ritCurrencyCode #-}

-- | The Reserved Instance offering type.
ritOfferingType
    :: Functor f
    => (Maybe OfferingTypeValues
    -> f (Maybe OfferingTypeValues))
    -> ReservedInstancesOffering
    -> f ReservedInstancesOffering
ritOfferingType f x =
    (\y -> x { _ritOfferingType = y })
       <$> f (_ritOfferingType x)
{-# INLINE ritOfferingType #-}

-- | The recurring charge tag assigned to the resource.
ritRecurringCharges
    :: Functor f
    => ([RecurringCharge]
    -> f ([RecurringCharge]))
    -> ReservedInstancesOffering
    -> f ReservedInstancesOffering
ritRecurringCharges f x =
    (\y -> x { _ritRecurringCharges = y })
       <$> f (_ritRecurringCharges x)
{-# INLINE ritRecurringCharges #-}

-- | Indicates whether the offering is available through the Reserved Instance
-- Marketplace (resale) or AWS. If it's a Reserved Instance Marketplace
-- offering, this is true.
ritMarketplace
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> ReservedInstancesOffering
    -> f ReservedInstancesOffering
ritMarketplace f x =
    (\y -> x { _ritMarketplace = y })
       <$> f (_ritMarketplace x)
{-# INLINE ritMarketplace #-}

-- | The pricing details of the Reserved Instance offering.
ritPricingDetails
    :: Functor f
    => ([PricingDetail]
    -> f ([PricingDetail]))
    -> ReservedInstancesOffering
    -> f ReservedInstancesOffering
ritPricingDetails f x =
    (\y -> x { _ritPricingDetails = y })
       <$> f (_ritPricingDetails x)
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
reDestinationCidrBlock
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Route
    -> f Route
reDestinationCidrBlock f x =
    (\y -> x { _reDestinationCidrBlock = y })
       <$> f (_reDestinationCidrBlock x)
{-# INLINE reDestinationCidrBlock #-}

-- | The ID of a gateway attached to your VPC.
reGatewayId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Route
    -> f Route
reGatewayId f x =
    (\y -> x { _reGatewayId = y })
       <$> f (_reGatewayId x)
{-# INLINE reGatewayId #-}

-- | The ID of a NAT instance in your VPC.
reInstanceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Route
    -> f Route
reInstanceId f x =
    (\y -> x { _reInstanceId = y })
       <$> f (_reInstanceId x)
{-# INLINE reInstanceId #-}

-- | The AWS account ID of the owner of the instance.
reInstanceOwnerId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Route
    -> f Route
reInstanceOwnerId f x =
    (\y -> x { _reInstanceOwnerId = y })
       <$> f (_reInstanceOwnerId x)
{-# INLINE reInstanceOwnerId #-}

-- | The ID of the network interface.
reNetworkInterfaceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Route
    -> f Route
reNetworkInterfaceId f x =
    (\y -> x { _reNetworkInterfaceId = y })
       <$> f (_reNetworkInterfaceId x)
{-# INLINE reNetworkInterfaceId #-}

-- | The ID of the VPC peering connection.
reVpcPeeringConnectionId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Route
    -> f Route
reVpcPeeringConnectionId f x =
    (\y -> x { _reVpcPeeringConnectionId = y })
       <$> f (_reVpcPeeringConnectionId x)
{-# INLINE reVpcPeeringConnectionId #-}

-- | The state of the route. The blackhole state indicates that the route's
-- target isn't available (for example, the specified gateway isn't attached
-- to the VPC, or the specified NAT instance has been terminated).
reState
    :: Functor f
    => (Maybe RouteState
    -> f (Maybe RouteState))
    -> Route
    -> f Route
reState f x =
    (\y -> x { _reState = y })
       <$> f (_reState x)
{-# INLINE reState #-}

-- | Describes how the route was created. CreateRouteTable indicates that route
-- was automatically created when the route table was created. CreateRoute
-- indicates that the route was manually added to the route table.
-- EnableVgwRoutePropagation indicates that the route was propagated by route
-- propagation.
reOrigin
    :: Functor f
    => (Maybe RouteOrigin
    -> f (Maybe RouteOrigin))
    -> Route
    -> f Route
reOrigin f x =
    (\y -> x { _reOrigin = y })
       <$> f (_reOrigin x)
{-# INLINE reOrigin #-}

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
rtRouteTableId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RouteTable
    -> f RouteTable
rtRouteTableId f x =
    (\y -> x { _rtRouteTableId = y })
       <$> f (_rtRouteTableId x)
{-# INLINE rtRouteTableId #-}

-- | The ID of the VPC.
rtVpcId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RouteTable
    -> f RouteTable
rtVpcId f x =
    (\y -> x { _rtVpcId = y })
       <$> f (_rtVpcId x)
{-# INLINE rtVpcId #-}

-- | The routes in the route table.
rtRoutes
    :: Functor f
    => ([Route]
    -> f ([Route]))
    -> RouteTable
    -> f RouteTable
rtRoutes f x =
    (\y -> x { _rtRoutes = y })
       <$> f (_rtRoutes x)
{-# INLINE rtRoutes #-}

-- | The associations between the route table and one or more subnets.
rtAssociations
    :: Functor f
    => ([RouteTableAssociation]
    -> f ([RouteTableAssociation]))
    -> RouteTable
    -> f RouteTable
rtAssociations f x =
    (\y -> x { _rtAssociations = y })
       <$> f (_rtAssociations x)
{-# INLINE rtAssociations #-}

-- | Any tags assigned to the route table.
rtTags
    :: Functor f
    => ([Tag]
    -> f ([Tag]))
    -> RouteTable
    -> f RouteTable
rtTags f x =
    (\y -> x { _rtTags = y })
       <$> f (_rtTags x)
{-# INLINE rtTags #-}

-- | Any virtual private gateway (VGW) propagating routes.
rtPropagatingVgws
    :: Functor f
    => ([PropagatingVgw]
    -> f ([PropagatingVgw]))
    -> RouteTable
    -> f RouteTable
rtPropagatingVgws f x =
    (\y -> x { _rtPropagatingVgws = y })
       <$> f (_rtPropagatingVgws x)
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
rtaRouteTableAssociationId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RouteTableAssociation
    -> f RouteTableAssociation
rtaRouteTableAssociationId f x =
    (\y -> x { _rtaRouteTableAssociationId = y })
       <$> f (_rtaRouteTableAssociationId x)
{-# INLINE rtaRouteTableAssociationId #-}

-- | The ID of the route table.
rtaRouteTableId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RouteTableAssociation
    -> f RouteTableAssociation
rtaRouteTableId f x =
    (\y -> x { _rtaRouteTableId = y })
       <$> f (_rtaRouteTableId x)
{-# INLINE rtaRouteTableId #-}

-- | The ID of the subnet.
rtaSubnetId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RouteTableAssociation
    -> f RouteTableAssociation
rtaSubnetId f x =
    (\y -> x { _rtaSubnetId = y })
       <$> f (_rtaSubnetId x)
{-# INLINE rtaSubnetId #-}

-- | Indicates whether this is the main route table.
rtaMain
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> RouteTableAssociation
    -> f RouteTableAssociation
rtaMain f x =
    (\y -> x { _rtaMain = y })
       <$> f (_rtaMain x)
{-# INLINE rtaMain #-}

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
ssBucket
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> S3Storage
    -> f S3Storage
ssBucket f x =
    (\y -> x { _ssBucket = y })
       <$> f (_ssBucket x)
{-# INLINE ssBucket #-}

-- | The beginning of the file name of the AMI.
ssPrefix
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> S3Storage
    -> f S3Storage
ssPrefix f x =
    (\y -> x { _ssPrefix = y })
       <$> f (_ssPrefix x)
{-# INLINE ssPrefix #-}

-- | The access key ID of the owner of the bucket. Before you specify a value
-- for your access key ID, review and follow the guidance in Best Practices
-- for Managing AWS Access Keys.
ssAWSAccessKeyId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> S3Storage
    -> f S3Storage
ssAWSAccessKeyId f x =
    (\y -> x { _ssAWSAccessKeyId = y })
       <$> f (_ssAWSAccessKeyId x)
{-# INLINE ssAWSAccessKeyId #-}

-- | A Base64-encoded Amazon S3 upload policy that gives Amazon EC2 permission
-- to upload items into Amazon S3 on your behalf.
ssUploadPolicy
    :: Functor f
    => (Maybe ByteString
    -> f (Maybe ByteString))
    -> S3Storage
    -> f S3Storage
ssUploadPolicy f x =
    (\y -> x { _ssUploadPolicy = y })
       <$> f (_ssUploadPolicy x)
{-# INLINE ssUploadPolicy #-}

-- | The signature of the Base64 encoded JSON document.
ssUploadPolicySignature
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> S3Storage
    -> f S3Storage
ssUploadPolicySignature f x =
    (\y -> x { _ssUploadPolicySignature = y })
       <$> f (_ssUploadPolicySignature x)
{-# INLINE ssUploadPolicySignature #-}

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
siOwnerId
    :: Functor f
    => (Text
    -> f (Text))
    -> SecurityGroup
    -> f SecurityGroup
siOwnerId f x =
    (\y -> x { _siOwnerId = y })
       <$> f (_siOwnerId x)
{-# INLINE siOwnerId #-}

-- | The name of the security group.
siGroupName
    :: Functor f
    => (Text
    -> f (Text))
    -> SecurityGroup
    -> f SecurityGroup
siGroupName f x =
    (\y -> x { _siGroupName = y })
       <$> f (_siGroupName x)
{-# INLINE siGroupName #-}

-- | The ID of the security group.
siGroupId
    :: Functor f
    => (Text
    -> f (Text))
    -> SecurityGroup
    -> f SecurityGroup
siGroupId f x =
    (\y -> x { _siGroupId = y })
       <$> f (_siGroupId x)
{-# INLINE siGroupId #-}

-- | A description of the security group.
siDescription
    :: Functor f
    => (Text
    -> f (Text))
    -> SecurityGroup
    -> f SecurityGroup
siDescription f x =
    (\y -> x { _siDescription = y })
       <$> f (_siDescription x)
{-# INLINE siDescription #-}

-- | One or more inbound rules associated with the security group.
siIpPermissions
    :: Functor f
    => ([IpPermission]
    -> f ([IpPermission]))
    -> SecurityGroup
    -> f SecurityGroup
siIpPermissions f x =
    (\y -> x { _siIpPermissions = y })
       <$> f (_siIpPermissions x)
{-# INLINE siIpPermissions #-}

-- | [EC2-VPC] One or more outbound rules associated with the security group.
siIpPermissionsEgress
    :: Functor f
    => ([IpPermission]
    -> f ([IpPermission]))
    -> SecurityGroup
    -> f SecurityGroup
siIpPermissionsEgress f x =
    (\y -> x { _siIpPermissionsEgress = y })
       <$> f (_siIpPermissionsEgress x)
{-# INLINE siIpPermissionsEgress #-}

-- | [EC2-VPC] The ID of the VPC for the security group.
siVpcId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SecurityGroup
    -> f SecurityGroup
siVpcId f x =
    (\y -> x { _siVpcId = y })
       <$> f (_siVpcId x)
{-# INLINE siVpcId #-}

-- | Any tags assigned to the security group.
siTags
    :: Functor f
    => ([Tag]
    -> f ([Tag]))
    -> SecurityGroup
    -> f SecurityGroup
siTags f x =
    (\y -> x { _siTags = y })
       <$> f (_siTags x)
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
ssuSnapshotId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Snapshot
    -> f Snapshot
ssuSnapshotId f x =
    (\y -> x { _ssuSnapshotId = y })
       <$> f (_ssuSnapshotId x)
{-# INLINE ssuSnapshotId #-}

-- | The ID of the volume.
ssuVolumeId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Snapshot
    -> f Snapshot
ssuVolumeId f x =
    (\y -> x { _ssuVolumeId = y })
       <$> f (_ssuVolumeId x)
{-# INLINE ssuVolumeId #-}

-- | The snapshot state.
ssuState
    :: Functor f
    => (Maybe SnapshotState
    -> f (Maybe SnapshotState))
    -> Snapshot
    -> f Snapshot
ssuState f x =
    (\y -> x { _ssuState = y })
       <$> f (_ssuState x)
{-# INLINE ssuState #-}

-- | The time stamp when the snapshot was initiated.
ssuStartTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> Snapshot
    -> f Snapshot
ssuStartTime f x =
    (\y -> x { _ssuStartTime = y })
       <$> f (_ssuStartTime x)
{-# INLINE ssuStartTime #-}

-- | The progress of the snapshot, as a percentage.
ssuProgress
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Snapshot
    -> f Snapshot
ssuProgress f x =
    (\y -> x { _ssuProgress = y })
       <$> f (_ssuProgress x)
{-# INLINE ssuProgress #-}

-- | The AWS account ID of the Amazon EBS snapshot owner.
ssuOwnerId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Snapshot
    -> f Snapshot
ssuOwnerId f x =
    (\y -> x { _ssuOwnerId = y })
       <$> f (_ssuOwnerId x)
{-# INLINE ssuOwnerId #-}

-- | The description for the snapshot.
ssuDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Snapshot
    -> f Snapshot
ssuDescription f x =
    (\y -> x { _ssuDescription = y })
       <$> f (_ssuDescription x)
{-# INLINE ssuDescription #-}

-- | The size of the volume, in GiB.
ssuVolumeSize
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> Snapshot
    -> f Snapshot
ssuVolumeSize f x =
    (\y -> x { _ssuVolumeSize = y })
       <$> f (_ssuVolumeSize x)
{-# INLINE ssuVolumeSize #-}

-- | The AWS account alias (for example, amazon, self) or AWS account ID that
-- owns the snapshot.
ssuOwnerAlias
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Snapshot
    -> f Snapshot
ssuOwnerAlias f x =
    (\y -> x { _ssuOwnerAlias = y })
       <$> f (_ssuOwnerAlias x)
{-# INLINE ssuOwnerAlias #-}

-- | Any tags assigned to the snapshot.
ssuTags
    :: Functor f
    => ([Tag]
    -> f ([Tag]))
    -> Snapshot
    -> f Snapshot
ssuTags f x =
    (\y -> x { _ssuTags = y })
       <$> f (_ssuTags x)
{-# INLINE ssuTags #-}

-- | Indicates whether the snapshot is encrypted.
ssuEncrypted
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> Snapshot
    -> f Snapshot
ssuEncrypted f x =
    (\y -> x { _ssuEncrypted = y })
       <$> f (_ssuEncrypted x)
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
sdsOwnerId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SpotDatafeedSubscription
    -> f SpotDatafeedSubscription
sdsOwnerId f x =
    (\y -> x { _sdsOwnerId = y })
       <$> f (_sdsOwnerId x)
{-# INLINE sdsOwnerId #-}

-- | The Amazon S3 bucket where the Spot Instance datafeed is located.
sdsBucket
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SpotDatafeedSubscription
    -> f SpotDatafeedSubscription
sdsBucket f x =
    (\y -> x { _sdsBucket = y })
       <$> f (_sdsBucket x)
{-# INLINE sdsBucket #-}

-- | The prefix that is prepended to datafeed files.
sdsPrefix
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SpotDatafeedSubscription
    -> f SpotDatafeedSubscription
sdsPrefix f x =
    (\y -> x { _sdsPrefix = y })
       <$> f (_sdsPrefix x)
{-# INLINE sdsPrefix #-}

-- | The state of the Spot Instance datafeed subscription.
sdsState
    :: Functor f
    => (Maybe DatafeedSubscriptionState
    -> f (Maybe DatafeedSubscriptionState))
    -> SpotDatafeedSubscription
    -> f SpotDatafeedSubscription
sdsState f x =
    (\y -> x { _sdsState = y })
       <$> f (_sdsState x)
{-# INLINE sdsState #-}

-- | The fault codes for the Spot Instance request, if any.
sdsFault
    :: Functor f
    => (Maybe SpotInstanceStateFault
    -> f (Maybe SpotInstanceStateFault))
    -> SpotDatafeedSubscription
    -> f SpotDatafeedSubscription
sdsFault f x =
    (\y -> x { _sdsFault = y })
       <$> f (_sdsFault x)
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
sirSpotInstanceRequestId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SpotInstanceRequest
    -> f SpotInstanceRequest
sirSpotInstanceRequestId f x =
    (\y -> x { _sirSpotInstanceRequestId = y })
       <$> f (_sirSpotInstanceRequestId x)
{-# INLINE sirSpotInstanceRequestId #-}

-- | The maximum hourly price for any Spot Instance launched to fulfill the
-- request.
sirSpotPrice
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SpotInstanceRequest
    -> f SpotInstanceRequest
sirSpotPrice f x =
    (\y -> x { _sirSpotPrice = y })
       <$> f (_sirSpotPrice x)
{-# INLINE sirSpotPrice #-}

-- | The Spot Instance request type.
sirType
    :: Functor f
    => (Maybe SpotInstanceType
    -> f (Maybe SpotInstanceType))
    -> SpotInstanceRequest
    -> f SpotInstanceRequest
sirType f x =
    (\y -> x { _sirType = y })
       <$> f (_sirType x)
{-# INLINE sirType #-}

-- | The state of the Spot Instance request. Spot bid status information can
-- help you track your Spot Instance requests. For information, see Tracking
-- Spot Requests with Bid Status Codes in the Amazon Elastic Compute Cloud
-- User Guide.
sirState
    :: Functor f
    => (Maybe SpotInstanceState
    -> f (Maybe SpotInstanceState))
    -> SpotInstanceRequest
    -> f SpotInstanceRequest
sirState f x =
    (\y -> x { _sirState = y })
       <$> f (_sirState x)
{-# INLINE sirState #-}

-- | The fault codes for the Spot Instance request, if any.
sirFault
    :: Functor f
    => (Maybe SpotInstanceStateFault
    -> f (Maybe SpotInstanceStateFault))
    -> SpotInstanceRequest
    -> f SpotInstanceRequest
sirFault f x =
    (\y -> x { _sirFault = y })
       <$> f (_sirFault x)
{-# INLINE sirFault #-}

-- | The status code and status message describing the Spot Instance request.
sirStatus
    :: Functor f
    => (Maybe SpotInstanceStatus
    -> f (Maybe SpotInstanceStatus))
    -> SpotInstanceRequest
    -> f SpotInstanceRequest
sirStatus f x =
    (\y -> x { _sirStatus = y })
       <$> f (_sirStatus x)
{-# INLINE sirStatus #-}

-- | The start date of the request. If this is a one-time request, the request
-- becomes active at this date and time and remains active until all instances
-- launch, the request expires, or the request is canceled. If the request is
-- persistent, the request becomes active at this date and time and remains
-- active until it expires or is canceled.
sirValidFrom
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> SpotInstanceRequest
    -> f SpotInstanceRequest
sirValidFrom f x =
    (\y -> x { _sirValidFrom = y })
       <$> f (_sirValidFrom x)
{-# INLINE sirValidFrom #-}

-- | The end date of the request. If this is a one-time request, the request
-- remains active until all instances launch, the request is canceled, or this
-- date is reached. If the request is persistent, it remains active until it
-- is canceled or this date is reached.
sirValidUntil
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> SpotInstanceRequest
    -> f SpotInstanceRequest
sirValidUntil f x =
    (\y -> x { _sirValidUntil = y })
       <$> f (_sirValidUntil x)
{-# INLINE sirValidUntil #-}

-- | The instance launch group. Launch groups are Spot Instances that launch
-- together and terminate together.
sirLaunchGroup
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SpotInstanceRequest
    -> f SpotInstanceRequest
sirLaunchGroup f x =
    (\y -> x { _sirLaunchGroup = y })
       <$> f (_sirLaunchGroup x)
{-# INLINE sirLaunchGroup #-}

-- | The Availability Zone group. If you specify the same Availability Zone
-- group for all Spot Instance requests, all Spot Instances are launched in
-- the same Availability Zone.
sirAvailabilityZoneGroup
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SpotInstanceRequest
    -> f SpotInstanceRequest
sirAvailabilityZoneGroup f x =
    (\y -> x { _sirAvailabilityZoneGroup = y })
       <$> f (_sirAvailabilityZoneGroup x)
{-# INLINE sirAvailabilityZoneGroup #-}

-- | Additional information for launching instances.
sirLaunchSpecification
    :: Functor f
    => (Maybe LaunchSpecification
    -> f (Maybe LaunchSpecification))
    -> SpotInstanceRequest
    -> f SpotInstanceRequest
sirLaunchSpecification f x =
    (\y -> x { _sirLaunchSpecification = y })
       <$> f (_sirLaunchSpecification x)
{-# INLINE sirLaunchSpecification #-}

-- | The instance ID, if an instance has been launched to fulfill the Spot
-- Instance request.
sirInstanceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SpotInstanceRequest
    -> f SpotInstanceRequest
sirInstanceId f x =
    (\y -> x { _sirInstanceId = y })
       <$> f (_sirInstanceId x)
{-# INLINE sirInstanceId #-}

-- | The time stamp when the Spot Instance request was created.
sirCreateTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> SpotInstanceRequest
    -> f SpotInstanceRequest
sirCreateTime f x =
    (\y -> x { _sirCreateTime = y })
       <$> f (_sirCreateTime x)
{-# INLINE sirCreateTime #-}

-- | The product description associated with the Spot Instance.
sirProductDescription
    :: Functor f
    => (Maybe RIProductDescription
    -> f (Maybe RIProductDescription))
    -> SpotInstanceRequest
    -> f SpotInstanceRequest
sirProductDescription f x =
    (\y -> x { _sirProductDescription = y })
       <$> f (_sirProductDescription x)
{-# INLINE sirProductDescription #-}

-- | Any tags assigned to the resource.
sirTags
    :: Functor f
    => ([Tag]
    -> f ([Tag]))
    -> SpotInstanceRequest
    -> f SpotInstanceRequest
sirTags f x =
    (\y -> x { _sirTags = y })
       <$> f (_sirTags x)
{-# INLINE sirTags #-}

-- | The Availability Zone in which the bid is launched.
sirLaunchedAvailabilityZone
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SpotInstanceRequest
    -> f SpotInstanceRequest
sirLaunchedAvailabilityZone f x =
    (\y -> x { _sirLaunchedAvailabilityZone = y })
       <$> f (_sirLaunchedAvailabilityZone x)
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
sisfCode
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SpotInstanceStateFault
    -> f SpotInstanceStateFault
sisfCode f x =
    (\y -> x { _sisfCode = y })
       <$> f (_sisfCode x)
{-# INLINE sisfCode #-}

-- | The message for the Spot Instance state change.
sisfMessage
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SpotInstanceStateFault
    -> f SpotInstanceStateFault
sisfMessage f x =
    (\y -> x { _sisfMessage = y })
       <$> f (_sisfMessage x)
{-# INLINE sisfMessage #-}

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
siuCode
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SpotInstanceStatus
    -> f SpotInstanceStatus
siuCode f x =
    (\y -> x { _siuCode = y })
       <$> f (_siuCode x)
{-# INLINE siuCode #-}

-- | The time of the most recent status update.
siuUpdateTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> SpotInstanceStatus
    -> f SpotInstanceStatus
siuUpdateTime f x =
    (\y -> x { _siuUpdateTime = y })
       <$> f (_siuUpdateTime x)
{-# INLINE siuUpdateTime #-}

-- | The description for the status code for the Spot request.
siuMessage
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SpotInstanceStatus
    -> f SpotInstanceStatus
siuMessage f x =
    (\y -> x { _siuMessage = y })
       <$> f (_siuMessage x)
{-# INLINE siuMessage #-}

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
spAvailabilityZone
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SpotPlacement
    -> f SpotPlacement
spAvailabilityZone f x =
    (\y -> x { _spAvailabilityZone = y })
       <$> f (_spAvailabilityZone x)
{-# INLINE spAvailabilityZone #-}

-- | The Availability Zone group name.
spGroupName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SpotPlacement
    -> f SpotPlacement
spGroupName f x =
    (\y -> x { _spGroupName = y })
       <$> f (_spGroupName x)
{-# INLINE spGroupName #-}

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
sqInstanceType
    :: Functor f
    => (Maybe InstanceType
    -> f (Maybe InstanceType))
    -> SpotPrice
    -> f SpotPrice
sqInstanceType f x =
    (\y -> x { _sqInstanceType = y })
       <$> f (_sqInstanceType x)
{-# INLINE sqInstanceType #-}

-- | A general description of the AMI.
sqProductDescription
    :: Functor f
    => (Maybe RIProductDescription
    -> f (Maybe RIProductDescription))
    -> SpotPrice
    -> f SpotPrice
sqProductDescription f x =
    (\y -> x { _sqProductDescription = y })
       <$> f (_sqProductDescription x)
{-# INLINE sqProductDescription #-}

-- | The maximum price you will pay to launch one or more Spot Instances.
sqSpotPrice
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SpotPrice
    -> f SpotPrice
sqSpotPrice f x =
    (\y -> x { _sqSpotPrice = y })
       <$> f (_sqSpotPrice x)
{-# INLINE sqSpotPrice #-}

-- | The date and time the request was created.
sqTimestamp
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> SpotPrice
    -> f SpotPrice
sqTimestamp f x =
    (\y -> x { _sqTimestamp = y })
       <$> f (_sqTimestamp x)
{-# INLINE sqTimestamp #-}

-- | The Availability Zone.
sqAvailabilityZone
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SpotPrice
    -> f SpotPrice
sqAvailabilityZone f x =
    (\y -> x { _sqAvailabilityZone = y })
       <$> f (_sqAvailabilityZone x)
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
srCode
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StateReason
    -> f StateReason
srCode f x =
    (\y -> x { _srCode = y })
       <$> f (_srCode x)
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
srMessage
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StateReason
    -> f StateReason
srMessage f x =
    (\y -> x { _srMessage = y })
       <$> f (_srMessage x)
{-# INLINE srMessage #-}

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
sxSubnetId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Subnet
    -> f Subnet
sxSubnetId f x =
    (\y -> x { _sxSubnetId = y })
       <$> f (_sxSubnetId x)
{-# INLINE sxSubnetId #-}

-- | The current state of the subnet.
sxState
    :: Functor f
    => (Maybe SubnetState
    -> f (Maybe SubnetState))
    -> Subnet
    -> f Subnet
sxState f x =
    (\y -> x { _sxState = y })
       <$> f (_sxState x)
{-# INLINE sxState #-}

-- | The ID of the VPC the subnet is in.
sxVpcId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Subnet
    -> f Subnet
sxVpcId f x =
    (\y -> x { _sxVpcId = y })
       <$> f (_sxVpcId x)
{-# INLINE sxVpcId #-}

-- | The CIDR block assigned to the subnet.
sxCidrBlock
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Subnet
    -> f Subnet
sxCidrBlock f x =
    (\y -> x { _sxCidrBlock = y })
       <$> f (_sxCidrBlock x)
{-# INLINE sxCidrBlock #-}

-- | The number of unused IP addresses in the subnet. Note that the IP addresses
-- for any stopped instances are considered unavailable.
sxAvailableIpAddressCount
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> Subnet
    -> f Subnet
sxAvailableIpAddressCount f x =
    (\y -> x { _sxAvailableIpAddressCount = y })
       <$> f (_sxAvailableIpAddressCount x)
{-# INLINE sxAvailableIpAddressCount #-}

-- | The Availability Zone of the subnet.
sxAvailabilityZone
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Subnet
    -> f Subnet
sxAvailabilityZone f x =
    (\y -> x { _sxAvailabilityZone = y })
       <$> f (_sxAvailabilityZone x)
{-# INLINE sxAvailabilityZone #-}

-- | Indicates whether this is the default subnet for the Availability Zone.
sxDefaultForAz
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> Subnet
    -> f Subnet
sxDefaultForAz f x =
    (\y -> x { _sxDefaultForAz = y })
       <$> f (_sxDefaultForAz x)
{-# INLINE sxDefaultForAz #-}

-- | Indicates whether instances launched in this subnet receive a public IP
-- address.
sxMapPublicIpOnLaunch
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> Subnet
    -> f Subnet
sxMapPublicIpOnLaunch f x =
    (\y -> x { _sxMapPublicIpOnLaunch = y })
       <$> f (_sxMapPublicIpOnLaunch x)
{-# INLINE sxMapPublicIpOnLaunch #-}

-- | Any tags assigned to the subnet.
sxTags
    :: Functor f
    => ([Tag]
    -> f ([Tag]))
    -> Subnet
    -> f Subnet
sxTags f x =
    (\y -> x { _sxTags = y })
       <$> f (_sxTags x)
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
tgKey
    :: Functor f
    => (Text
    -> f (Text))
    -> Tag
    -> f Tag
tgKey f x =
    (\y -> x { _tgKey = y })
       <$> f (_tgKey x)
{-# INLINE tgKey #-}

-- | The value of the tag. Constraints: Tag values are case-sensitive and accept
-- a maximum of 255 Unicode characters.
tgValue
    :: Functor f
    => (Text
    -> f (Text))
    -> Tag
    -> f Tag
tgValue f x =
    (\y -> x { _tgValue = y })
       <$> f (_tgValue x)
{-# INLINE tgValue #-}

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
tdResourceId
    :: Functor f
    => (Text
    -> f (Text))
    -> TagDescription
    -> f TagDescription
tdResourceId f x =
    (\y -> x { _tdResourceId = y })
       <$> f (_tdResourceId x)
{-# INLINE tdResourceId #-}

-- | The type of resource.
tdResourceType
    :: Functor f
    => (ResourceType
    -> f (ResourceType))
    -> TagDescription
    -> f TagDescription
tdResourceType f x =
    (\y -> x { _tdResourceType = y })
       <$> f (_tdResourceType x)
{-# INLINE tdResourceType #-}

-- | The key of the tag.
tdKey
    :: Functor f
    => (Text
    -> f (Text))
    -> TagDescription
    -> f TagDescription
tdKey f x =
    (\y -> x { _tdKey = y })
       <$> f (_tdKey x)
{-# INLINE tdKey #-}

-- | The value of the tag.
tdValue
    :: Functor f
    => (Text
    -> f (Text))
    -> TagDescription
    -> f TagDescription
tdValue f x =
    (\y -> x { _tdValue = y })
       <$> f (_tdValue x)
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
uigpUserId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UserIdGroupPair
    -> f UserIdGroupPair
uigpUserId f x =
    (\y -> x { _uigpUserId = y })
       <$> f (_uigpUserId x)
{-# INLINE uigpUserId #-}

-- | The ID of the security group owned by the specified AWS account.
uigpGroupName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UserIdGroupPair
    -> f UserIdGroupPair
uigpGroupName f x =
    (\y -> x { _uigpGroupName = y })
       <$> f (_uigpGroupName x)
{-# INLINE uigpGroupName #-}

-- | The name of the security group in the specified AWS account.
uigpGroupId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UserIdGroupPair
    -> f UserIdGroupPair
uigpGroupId f x =
    (\y -> x { _uigpGroupId = y })
       <$> f (_uigpGroupId x)
{-# INLINE uigpGroupId #-}

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
vvvvvvvvvvvvvvyOutsideIpAddress
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VgwTelemetry
    -> f VgwTelemetry
vvvvvvvvvvvvvvyOutsideIpAddress f x =
    (\y -> x { _vvvvvvvvvvvvvvyOutsideIpAddress = y })
       <$> f (_vvvvvvvvvvvvvvyOutsideIpAddress x)
{-# INLINE vvvvvvvvvvvvvvyOutsideIpAddress #-}

-- | The status of the VPN tunnel.
vvvvvvvvvvvvvvyStatus
    :: Functor f
    => (Maybe TelemetryStatus
    -> f (Maybe TelemetryStatus))
    -> VgwTelemetry
    -> f VgwTelemetry
vvvvvvvvvvvvvvyStatus f x =
    (\y -> x { _vvvvvvvvvvvvvvyStatus = y })
       <$> f (_vvvvvvvvvvvvvvyStatus x)
{-# INLINE vvvvvvvvvvvvvvyStatus #-}

-- | The date and time of the last change in status.
vvvvvvvvvvvvvvyLastStatusChange
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> VgwTelemetry
    -> f VgwTelemetry
vvvvvvvvvvvvvvyLastStatusChange f x =
    (\y -> x { _vvvvvvvvvvvvvvyLastStatusChange = y })
       <$> f (_vvvvvvvvvvvvvvyLastStatusChange x)
{-# INLINE vvvvvvvvvvvvvvyLastStatusChange #-}

-- | If an error occurs, a description of the error.
vvvvvvvvvvvvvvyStatusMessage
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VgwTelemetry
    -> f VgwTelemetry
vvvvvvvvvvvvvvyStatusMessage f x =
    (\y -> x { _vvvvvvvvvvvvvvyStatusMessage = y })
       <$> f (_vvvvvvvvvvvvvvyStatusMessage x)
{-# INLINE vvvvvvvvvvvvvvyStatusMessage #-}

-- | The number of accepted routes.
vvvvvvvvvvvvvvyAcceptedRouteCount
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> VgwTelemetry
    -> f VgwTelemetry
vvvvvvvvvvvvvvyAcceptedRouteCount f x =
    (\y -> x { _vvvvvvvvvvvvvvyAcceptedRouteCount = y })
       <$> f (_vvvvvvvvvvvvvvyAcceptedRouteCount x)
{-# INLINE vvvvvvvvvvvvvvyAcceptedRouteCount #-}

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
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjVolumeId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Volume
    -> f Volume
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjVolumeId f x =
    (\y -> x { _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjVolumeId = y })
       <$> f (_vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjVolumeId x)
{-# INLINE vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjVolumeId #-}

-- | The size of the volume, in GiBs.
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjSize
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> Volume
    -> f Volume
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjSize f x =
    (\y -> x { _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjSize = y })
       <$> f (_vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjSize x)
{-# INLINE vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjSize #-}

-- | The snapshot from which the volume was created, if applicable.
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjSnapshotId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Volume
    -> f Volume
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjSnapshotId f x =
    (\y -> x { _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjSnapshotId = y })
       <$> f (_vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjSnapshotId x)
{-# INLINE vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjSnapshotId #-}

-- | The Availability Zone for the volume.
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjAvailabilityZone
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Volume
    -> f Volume
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjAvailabilityZone f x =
    (\y -> x { _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjAvailabilityZone = y })
       <$> f (_vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjAvailabilityZone x)
{-# INLINE vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjAvailabilityZone #-}

-- | The volume state.
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjState
    :: Functor f
    => (Maybe VolumeState
    -> f (Maybe VolumeState))
    -> Volume
    -> f Volume
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjState f x =
    (\y -> x { _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjState = y })
       <$> f (_vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjState x)
{-# INLINE vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjState #-}

-- | The time stamp when volume creation was initiated.
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjCreateTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> Volume
    -> f Volume
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjCreateTime f x =
    (\y -> x { _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjCreateTime = y })
       <$> f (_vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjCreateTime x)
{-# INLINE vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjCreateTime #-}

-- | 
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjAttachments
    :: Functor f
    => ([VolumeAttachment]
    -> f ([VolumeAttachment]))
    -> Volume
    -> f Volume
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjAttachments f x =
    (\y -> x { _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjAttachments = y })
       <$> f (_vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjAttachments x)
{-# INLINE vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjAttachments #-}

-- | Any tags assigned to the volume.
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjTags
    :: Functor f
    => ([Tag]
    -> f ([Tag]))
    -> Volume
    -> f Volume
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjTags f x =
    (\y -> x { _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjTags = y })
       <$> f (_vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjTags x)
{-# INLINE vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjTags #-}

-- | The volume type. This can be gp2 for General Purpose (SSD) volumes, io1 for
-- Provisioned IOPS (SSD) volumes, or standard for Magnetic volumes.
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjVolumeType
    :: Functor f
    => (Maybe VolumeType
    -> f (Maybe VolumeType))
    -> Volume
    -> f Volume
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjVolumeType f x =
    (\y -> x { _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjVolumeType = y })
       <$> f (_vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjVolumeType x)
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
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjIops
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> Volume
    -> f Volume
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjIops f x =
    (\y -> x { _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjIops = y })
       <$> f (_vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjIops x)
{-# INLINE vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjIops #-}

-- | Indicates whether the volume is encrypted.
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjEncrypted
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> Volume
    -> f Volume
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjEncrypted f x =
    (\y -> x { _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjEncrypted = y })
       <$> f (_vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjEncrypted x)
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
vcVolumeId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VolumeAttachment
    -> f VolumeAttachment
vcVolumeId f x =
    (\y -> x { _vcVolumeId = y })
       <$> f (_vcVolumeId x)
{-# INLINE vcVolumeId #-}

-- | The ID of the instance.
vcInstanceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VolumeAttachment
    -> f VolumeAttachment
vcInstanceId f x =
    (\y -> x { _vcInstanceId = y })
       <$> f (_vcInstanceId x)
{-# INLINE vcInstanceId #-}

-- | The device name.
vcDevice
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VolumeAttachment
    -> f VolumeAttachment
vcDevice f x =
    (\y -> x { _vcDevice = y })
       <$> f (_vcDevice x)
{-# INLINE vcDevice #-}

-- | The attachment state of the volume.
vcState
    :: Functor f
    => (Maybe VolumeAttachmentState
    -> f (Maybe VolumeAttachmentState))
    -> VolumeAttachment
    -> f VolumeAttachment
vcState f x =
    (\y -> x { _vcState = y })
       <$> f (_vcState x)
{-# INLINE vcState #-}

-- | The time stamp when the attachment initiated.
vcAttachTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> VolumeAttachment
    -> f VolumeAttachment
vcAttachTime f x =
    (\y -> x { _vcAttachTime = y })
       <$> f (_vcAttachTime x)
{-# INLINE vcAttachTime #-}

-- | Indicates whether the Amazon EBS volume is deleted on instance termination.
vcDeleteOnTermination
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> VolumeAttachment
    -> f VolumeAttachment
vcDeleteOnTermination f x =
    (\y -> x { _vcDeleteOnTermination = y })
       <$> f (_vcDeleteOnTermination x)
{-# INLINE vcDeleteOnTermination #-}

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
vsaCode
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VolumeStatusAction
    -> f VolumeStatusAction
vsaCode f x =
    (\y -> x { _vsaCode = y })
       <$> f (_vsaCode x)
{-# INLINE vsaCode #-}

-- | A description of the operation.
vsaDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VolumeStatusAction
    -> f VolumeStatusAction
vsaDescription f x =
    (\y -> x { _vsaDescription = y })
       <$> f (_vsaDescription x)
{-# INLINE vsaDescription #-}

-- | The event type associated with this operation.
vsaEventType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VolumeStatusAction
    -> f VolumeStatusAction
vsaEventType f x =
    (\y -> x { _vsaEventType = y })
       <$> f (_vsaEventType x)
{-# INLINE vsaEventType #-}

-- | The ID of the event associated with this operation.
vsaEventId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VolumeStatusAction
    -> f VolumeStatusAction
vsaEventId f x =
    (\y -> x { _vsaEventId = y })
       <$> f (_vsaEventId x)
{-# INLINE vsaEventId #-}

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
vsdName
    :: Functor f
    => (Maybe VolumeStatusName
    -> f (Maybe VolumeStatusName))
    -> VolumeStatusDetails
    -> f VolumeStatusDetails
vsdName f x =
    (\y -> x { _vsdName = y })
       <$> f (_vsdName x)
{-# INLINE vsdName #-}

-- | The intended status of the volume status.
vsdStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VolumeStatusDetails
    -> f VolumeStatusDetails
vsdStatus f x =
    (\y -> x { _vsdStatus = y })
       <$> f (_vsdStatus x)
{-# INLINE vsdStatus #-}

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
vseEventType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VolumeStatusEvent
    -> f VolumeStatusEvent
vseEventType f x =
    (\y -> x { _vseEventType = y })
       <$> f (_vseEventType x)
{-# INLINE vseEventType #-}

-- | A description of the event.
vseDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VolumeStatusEvent
    -> f VolumeStatusEvent
vseDescription f x =
    (\y -> x { _vseDescription = y })
       <$> f (_vseDescription x)
{-# INLINE vseDescription #-}

-- | The earliest start time of the event.
vseNotBefore
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> VolumeStatusEvent
    -> f VolumeStatusEvent
vseNotBefore f x =
    (\y -> x { _vseNotBefore = y })
       <$> f (_vseNotBefore x)
{-# INLINE vseNotBefore #-}

-- | The latest end time of the event.
vseNotAfter
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> VolumeStatusEvent
    -> f VolumeStatusEvent
vseNotAfter f x =
    (\y -> x { _vseNotAfter = y })
       <$> f (_vseNotAfter x)
{-# INLINE vseNotAfter #-}

-- | The ID of this event.
vseEventId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VolumeStatusEvent
    -> f VolumeStatusEvent
vseEventId f x =
    (\y -> x { _vseEventId = y })
       <$> f (_vseEventId x)
{-# INLINE vseEventId #-}

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
vsjStatus
    :: Functor f
    => (Maybe VolumeStatusInfoStatus
    -> f (Maybe VolumeStatusInfoStatus))
    -> VolumeStatusInfo
    -> f VolumeStatusInfo
vsjStatus f x =
    (\y -> x { _vsjStatus = y })
       <$> f (_vsjStatus x)
{-# INLINE vsjStatus #-}

-- | The details of the volume status.
vsjDetails
    :: Functor f
    => ([VolumeStatusDetails]
    -> f ([VolumeStatusDetails]))
    -> VolumeStatusInfo
    -> f VolumeStatusInfo
vsjDetails f x =
    (\y -> x { _vsjDetails = y })
       <$> f (_vsjDetails x)
{-# INLINE vsjDetails #-}

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
vsiVolumeId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VolumeStatusItem
    -> f VolumeStatusItem
vsiVolumeId f x =
    (\y -> x { _vsiVolumeId = y })
       <$> f (_vsiVolumeId x)
{-# INLINE vsiVolumeId #-}

-- | The Availability Zone of the volume.
vsiAvailabilityZone
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VolumeStatusItem
    -> f VolumeStatusItem
vsiAvailabilityZone f x =
    (\y -> x { _vsiAvailabilityZone = y })
       <$> f (_vsiAvailabilityZone x)
{-# INLINE vsiAvailabilityZone #-}

-- | The volume status.
vsiVolumeStatus
    :: Functor f
    => (Maybe VolumeStatusInfo
    -> f (Maybe VolumeStatusInfo))
    -> VolumeStatusItem
    -> f VolumeStatusItem
vsiVolumeStatus f x =
    (\y -> x { _vsiVolumeStatus = y })
       <$> f (_vsiVolumeStatus x)
{-# INLINE vsiVolumeStatus #-}

-- | A list of events associated with the volume.
vsiEvents
    :: Functor f
    => ([VolumeStatusEvent]
    -> f ([VolumeStatusEvent]))
    -> VolumeStatusItem
    -> f VolumeStatusItem
vsiEvents f x =
    (\y -> x { _vsiEvents = y })
       <$> f (_vsiEvents x)
{-# INLINE vsiEvents #-}

-- | The details of the operation.
vsiActions
    :: Functor f
    => ([VolumeStatusAction]
    -> f ([VolumeStatusAction]))
    -> VolumeStatusItem
    -> f VolumeStatusItem
vsiActions f x =
    (\y -> x { _vsiActions = y })
       <$> f (_vsiActions x)
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
vdVpcId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Vpc
    -> f Vpc
vdVpcId f x =
    (\y -> x { _vdVpcId = y })
       <$> f (_vdVpcId x)
{-# INLINE vdVpcId #-}

-- | The current state of the VPC.
vdState
    :: Functor f
    => (Maybe VpcState
    -> f (Maybe VpcState))
    -> Vpc
    -> f Vpc
vdState f x =
    (\y -> x { _vdState = y })
       <$> f (_vdState x)
{-# INLINE vdState #-}

-- | The CIDR block for the VPC.
vdCidrBlock
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Vpc
    -> f Vpc
vdCidrBlock f x =
    (\y -> x { _vdCidrBlock = y })
       <$> f (_vdCidrBlock x)
{-# INLINE vdCidrBlock #-}

-- | The ID of the set of DHCP options you've associated with the VPC (or
-- default if the default options are associated with the VPC).
vdDhcpOptionsId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Vpc
    -> f Vpc
vdDhcpOptionsId f x =
    (\y -> x { _vdDhcpOptionsId = y })
       <$> f (_vdDhcpOptionsId x)
{-# INLINE vdDhcpOptionsId #-}

-- | Any tags assigned to the VPC.
vdTags
    :: Functor f
    => ([Tag]
    -> f ([Tag]))
    -> Vpc
    -> f Vpc
vdTags f x =
    (\y -> x { _vdTags = y })
       <$> f (_vdTags x)
{-# INLINE vdTags #-}

-- | The allowed tenancy of instances launched into the VPC.
vdInstanceTenancy
    :: Functor f
    => (Maybe Tenancy
    -> f (Maybe Tenancy))
    -> Vpc
    -> f Vpc
vdInstanceTenancy f x =
    (\y -> x { _vdInstanceTenancy = y })
       <$> f (_vdInstanceTenancy x)
{-# INLINE vdInstanceTenancy #-}

-- | Indicates whether the VPC is the default VPC.
vdIsDefault
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> Vpc
    -> f Vpc
vdIsDefault f x =
    (\y -> x { _vdIsDefault = y })
       <$> f (_vdIsDefault x)
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
vbVpcId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VpcAttachment
    -> f VpcAttachment
vbVpcId f x =
    (\y -> x { _vbVpcId = y })
       <$> f (_vbVpcId x)
{-# INLINE vbVpcId #-}

-- | The current state of the attachment.
vbState
    :: Functor f
    => (Maybe AttachmentStatus
    -> f (Maybe AttachmentStatus))
    -> VpcAttachment
    -> f VpcAttachment
vbState f x =
    (\y -> x { _vbState = y })
       <$> f (_vbState x)
{-# INLINE vbState #-}

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
vpcAccepterVpcInfo
    :: Functor f
    => (Maybe VpcPeeringConnectionVpcInfo
    -> f (Maybe VpcPeeringConnectionVpcInfo))
    -> VpcPeeringConnection
    -> f VpcPeeringConnection
vpcAccepterVpcInfo f x =
    (\y -> x { _vpcAccepterVpcInfo = y })
       <$> f (_vpcAccepterVpcInfo x)
{-# INLINE vpcAccepterVpcInfo #-}

-- | The time that an unaccepted VPC peering connection will expire.
vpcExpirationTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> VpcPeeringConnection
    -> f VpcPeeringConnection
vpcExpirationTime f x =
    (\y -> x { _vpcExpirationTime = y })
       <$> f (_vpcExpirationTime x)
{-# INLINE vpcExpirationTime #-}

-- | The information of the requester VPC.
vpcRequesterVpcInfo
    :: Functor f
    => (Maybe VpcPeeringConnectionVpcInfo
    -> f (Maybe VpcPeeringConnectionVpcInfo))
    -> VpcPeeringConnection
    -> f VpcPeeringConnection
vpcRequesterVpcInfo f x =
    (\y -> x { _vpcRequesterVpcInfo = y })
       <$> f (_vpcRequesterVpcInfo x)
{-# INLINE vpcRequesterVpcInfo #-}

-- | The status of the VPC peering connection.
vpcStatus
    :: Functor f
    => (Maybe VpcPeeringConnectionStateReason
    -> f (Maybe VpcPeeringConnectionStateReason))
    -> VpcPeeringConnection
    -> f VpcPeeringConnection
vpcStatus f x =
    (\y -> x { _vpcStatus = y })
       <$> f (_vpcStatus x)
{-# INLINE vpcStatus #-}

-- | Any tags assigned to the resource.
vpcTags
    :: Functor f
    => ([Tag]
    -> f ([Tag]))
    -> VpcPeeringConnection
    -> f VpcPeeringConnection
vpcTags f x =
    (\y -> x { _vpcTags = y })
       <$> f (_vpcTags x)
{-# INLINE vpcTags #-}

-- | The ID of the VPC peering connection.
vpcVpcPeeringConnectionId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VpcPeeringConnection
    -> f VpcPeeringConnection
vpcVpcPeeringConnectionId f x =
    (\y -> x { _vpcVpcPeeringConnectionId = y })
       <$> f (_vpcVpcPeeringConnectionId x)
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
vpcsrCode
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VpcPeeringConnectionStateReason
    -> f VpcPeeringConnectionStateReason
vpcsrCode f x =
    (\y -> x { _vpcsrCode = y })
       <$> f (_vpcsrCode x)
{-# INLINE vpcsrCode #-}

-- | A message that provides more information about the status, if applicable.
vpcsrMessage
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VpcPeeringConnectionStateReason
    -> f VpcPeeringConnectionStateReason
vpcsrMessage f x =
    (\y -> x { _vpcsrMessage = y })
       <$> f (_vpcsrMessage x)
{-# INLINE vpcsrMessage #-}

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
vpcviCidrBlock
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VpcPeeringConnectionVpcInfo
    -> f VpcPeeringConnectionVpcInfo
vpcviCidrBlock f x =
    (\y -> x { _vpcviCidrBlock = y })
       <$> f (_vpcviCidrBlock x)
{-# INLINE vpcviCidrBlock #-}

-- | The AWS account ID of the VPC owner.
vpcviOwnerId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VpcPeeringConnectionVpcInfo
    -> f VpcPeeringConnectionVpcInfo
vpcviOwnerId f x =
    (\y -> x { _vpcviOwnerId = y })
       <$> f (_vpcviOwnerId x)
{-# INLINE vpcviOwnerId #-}

-- | The ID of the VPC.
vpcviVpcId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VpcPeeringConnectionVpcInfo
    -> f VpcPeeringConnectionVpcInfo
vpcviVpcId f x =
    (\y -> x { _vpcviVpcId = y })
       <$> f (_vpcviVpcId x)
{-# INLINE vpcviVpcId #-}

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
vvvvvvvvvvvvvvoVpnConnectionId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VpnConnection
    -> f VpnConnection
vvvvvvvvvvvvvvoVpnConnectionId f x =
    (\y -> x { _vvvvvvvvvvvvvvoVpnConnectionId = y })
       <$> f (_vvvvvvvvvvvvvvoVpnConnectionId x)
{-# INLINE vvvvvvvvvvvvvvoVpnConnectionId #-}

-- | The current state of the VPN connection.
vvvvvvvvvvvvvvoState
    :: Functor f
    => (Maybe VpnState
    -> f (Maybe VpnState))
    -> VpnConnection
    -> f VpnConnection
vvvvvvvvvvvvvvoState f x =
    (\y -> x { _vvvvvvvvvvvvvvoState = y })
       <$> f (_vvvvvvvvvvvvvvoState x)
{-# INLINE vvvvvvvvvvvvvvoState #-}

-- | The configuration information for the VPN connection's customer gateway (in
-- the native XML format). This element is always present in the
-- CreateVpnConnection response; however, it's present in the
-- DescribeVpnConnections response only if the VPN connection is in the
-- pending or available state.
vvvvvvvvvvvvvvoCustomerGatewayConfiguration
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VpnConnection
    -> f VpnConnection
vvvvvvvvvvvvvvoCustomerGatewayConfiguration f x =
    (\y -> x { _vvvvvvvvvvvvvvoCustomerGatewayConfiguration = y })
       <$> f (_vvvvvvvvvvvvvvoCustomerGatewayConfiguration x)
{-# INLINE vvvvvvvvvvvvvvoCustomerGatewayConfiguration #-}

-- | The type of VPN connection.
vvvvvvvvvvvvvvoType
    :: Functor f
    => (Maybe GatewayType
    -> f (Maybe GatewayType))
    -> VpnConnection
    -> f VpnConnection
vvvvvvvvvvvvvvoType f x =
    (\y -> x { _vvvvvvvvvvvvvvoType = y })
       <$> f (_vvvvvvvvvvvvvvoType x)
{-# INLINE vvvvvvvvvvvvvvoType #-}

-- | The ID of the customer gateway at your end of the VPN connection.
vvvvvvvvvvvvvvoCustomerGatewayId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VpnConnection
    -> f VpnConnection
vvvvvvvvvvvvvvoCustomerGatewayId f x =
    (\y -> x { _vvvvvvvvvvvvvvoCustomerGatewayId = y })
       <$> f (_vvvvvvvvvvvvvvoCustomerGatewayId x)
{-# INLINE vvvvvvvvvvvvvvoCustomerGatewayId #-}

-- | The ID of the virtual private gateway at the AWS side of the VPN
-- connection.
vvvvvvvvvvvvvvoVpnGatewayId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VpnConnection
    -> f VpnConnection
vvvvvvvvvvvvvvoVpnGatewayId f x =
    (\y -> x { _vvvvvvvvvvvvvvoVpnGatewayId = y })
       <$> f (_vvvvvvvvvvvvvvoVpnGatewayId x)
{-# INLINE vvvvvvvvvvvvvvoVpnGatewayId #-}

-- | Any tags assigned to the VPN connection.
vvvvvvvvvvvvvvoTags
    :: Functor f
    => ([Tag]
    -> f ([Tag]))
    -> VpnConnection
    -> f VpnConnection
vvvvvvvvvvvvvvoTags f x =
    (\y -> x { _vvvvvvvvvvvvvvoTags = y })
       <$> f (_vvvvvvvvvvvvvvoTags x)
{-# INLINE vvvvvvvvvvvvvvoTags #-}

-- | Information about the VPN tunnel.
vvvvvvvvvvvvvvoVgwTelemetry
    :: Functor f
    => ([VgwTelemetry]
    -> f ([VgwTelemetry]))
    -> VpnConnection
    -> f VpnConnection
vvvvvvvvvvvvvvoVgwTelemetry f x =
    (\y -> x { _vvvvvvvvvvvvvvoVgwTelemetry = y })
       <$> f (_vvvvvvvvvvvvvvoVgwTelemetry x)
{-# INLINE vvvvvvvvvvvvvvoVgwTelemetry #-}

-- | The VPN connection options.
vvvvvvvvvvvvvvoOptions
    :: Functor f
    => (Maybe VpnConnectionOptions
    -> f (Maybe VpnConnectionOptions))
    -> VpnConnection
    -> f VpnConnection
vvvvvvvvvvvvvvoOptions f x =
    (\y -> x { _vvvvvvvvvvvvvvoOptions = y })
       <$> f (_vvvvvvvvvvvvvvoOptions x)
{-# INLINE vvvvvvvvvvvvvvoOptions #-}

-- | The static routes associated with the VPN connection.
vvvvvvvvvvvvvvoRoutes
    :: Functor f
    => ([VpnStaticRoute]
    -> f ([VpnStaticRoute]))
    -> VpnConnection
    -> f VpnConnection
vvvvvvvvvvvvvvoRoutes f x =
    (\y -> x { _vvvvvvvvvvvvvvoRoutes = y })
       <$> f (_vvvvvvvvvvvvvvoRoutes x)
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
vvvvvvvvvvvvvvvyVpnGatewayId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VpnGateway
    -> f VpnGateway
vvvvvvvvvvvvvvvyVpnGatewayId f x =
    (\y -> x { _vvvvvvvvvvvvvvvyVpnGatewayId = y })
       <$> f (_vvvvvvvvvvvvvvvyVpnGatewayId x)
{-# INLINE vvvvvvvvvvvvvvvyVpnGatewayId #-}

-- | The current state of the virtual private gateway.
vvvvvvvvvvvvvvvyState
    :: Functor f
    => (Maybe VpnState
    -> f (Maybe VpnState))
    -> VpnGateway
    -> f VpnGateway
vvvvvvvvvvvvvvvyState f x =
    (\y -> x { _vvvvvvvvvvvvvvvyState = y })
       <$> f (_vvvvvvvvvvvvvvvyState x)
{-# INLINE vvvvvvvvvvvvvvvyState #-}

-- | The type of VPN connection the virtual private gateway supports.
vvvvvvvvvvvvvvvyType
    :: Functor f
    => (Maybe GatewayType
    -> f (Maybe GatewayType))
    -> VpnGateway
    -> f VpnGateway
vvvvvvvvvvvvvvvyType f x =
    (\y -> x { _vvvvvvvvvvvvvvvyType = y })
       <$> f (_vvvvvvvvvvvvvvvyType x)
{-# INLINE vvvvvvvvvvvvvvvyType #-}

-- | The Availability Zone where the virtual private gateway was created.
vvvvvvvvvvvvvvvyAvailabilityZone
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VpnGateway
    -> f VpnGateway
vvvvvvvvvvvvvvvyAvailabilityZone f x =
    (\y -> x { _vvvvvvvvvvvvvvvyAvailabilityZone = y })
       <$> f (_vvvvvvvvvvvvvvvyAvailabilityZone x)
{-# INLINE vvvvvvvvvvvvvvvyAvailabilityZone #-}

-- | Any VPCs attached to the virtual private gateway.
vvvvvvvvvvvvvvvyVpcAttachments
    :: Functor f
    => ([VpcAttachment]
    -> f ([VpcAttachment]))
    -> VpnGateway
    -> f VpnGateway
vvvvvvvvvvvvvvvyVpcAttachments f x =
    (\y -> x { _vvvvvvvvvvvvvvvyVpcAttachments = y })
       <$> f (_vvvvvvvvvvvvvvvyVpcAttachments x)
{-# INLINE vvvvvvvvvvvvvvvyVpcAttachments #-}

-- | Any tags assigned to the virtual private gateway.
vvvvvvvvvvvvvvvyTags
    :: Functor f
    => ([Tag]
    -> f ([Tag]))
    -> VpnGateway
    -> f VpnGateway
vvvvvvvvvvvvvvvyTags f x =
    (\y -> x { _vvvvvvvvvvvvvvvyTags = y })
       <$> f (_vvvvvvvvvvvvvvvyTags x)
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
vsrDestinationCidrBlock
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VpnStaticRoute
    -> f VpnStaticRoute
vsrDestinationCidrBlock f x =
    (\y -> x { _vsrDestinationCidrBlock = y })
       <$> f (_vsrDestinationCidrBlock x)
{-# INLINE vsrDestinationCidrBlock #-}

-- | Indicates how the routes were provided.
vsrSource
    :: Functor f
    => (Maybe VpnStaticRouteSource
    -> f (Maybe VpnStaticRouteSource))
    -> VpnStaticRoute
    -> f VpnStaticRoute
vsrSource f x =
    (\y -> x { _vsrSource = y })
       <$> f (_vsrSource x)
{-# INLINE vsrSource #-}

-- | The current state of the static route.
vsrState
    :: Functor f
    => (Maybe VpnState
    -> f (Maybe VpnState))
    -> VpnStaticRoute
    -> f VpnStaticRoute
vsrState f x =
    (\y -> x { _vsrState = y })
       <$> f (_vsrState x)
{-# INLINE vsrState #-}

instance FromXML VpnStaticRoute where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery VpnStaticRoute where
    toQuery = genericQuery def
