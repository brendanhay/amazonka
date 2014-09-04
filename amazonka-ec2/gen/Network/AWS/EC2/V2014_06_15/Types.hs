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
aavAttributeValue :: Lens' AccountAttributeValue (Maybe Text)
aavAttributeValue f x =
    f (_aavAttributeValue x) <&> \y -> x { _aavAttributeValue = y }
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
abvValue :: Lens' AttributeBooleanValue (Maybe Bool)
abvValue f x =
    f (_abvValue x) <&> \y -> x { _abvValue = y }
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
axValue :: Lens' AttributeValue (Maybe Text)
axValue f x =
    f (_axValue x) <&> \y -> x { _axValue = y }
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
azmMessage :: Lens' AvailabilityZoneMessage (Maybe Text)
azmMessage f x =
    f (_azmMessage x) <&> \y -> x { _azmMessage = y }
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
iuCidrIp :: Lens' IpRange Text
iuCidrIp f x =
    f (_iuCidrIp x) <&> \y -> x { _iuCidrIp = y }
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
mgState :: Lens' Monitoring (Maybe MonitoringState)
mgState f x =
    f (_mgState x) <&> \y -> x { _mgState = y }
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
pwGatewayId :: Lens' PropagatingVgw (Maybe Text)
pwGatewayId f x =
    f (_pwGatewayId x) <&> \y -> x { _pwGatewayId = y }
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
rijReservedInstancesId :: Lens' ReservedInstancesId (Maybe Text)
rijReservedInstancesId f x =
    f (_rijReservedInstancesId x) <&> \y -> x { _rijReservedInstancesId = y }
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
rimeEnabled :: Lens' RunInstancesMonitoringEnabled Bool
rimeEnabled f x =
    f (_rimeEnabled x) <&> \y -> x { _rimeEnabled = y }
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
seS3 :: Lens' Storage (Maybe S3Storage)
seS3 f x =
    f (_seS3 x) <&> \y -> x { _seS3 = y }
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
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvxSize :: Lens' VolumeDetail Integer
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvxSize f x =
    f (_vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvxSize x) <&> \y -> x { _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvxSize = y }
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
vcoStaticRoutesOnly :: Lens' VpnConnectionOptions (Maybe Bool)
vcoStaticRoutesOnly f x =
    f (_vcoStaticRoutesOnly x) <&> \y -> x { _vcoStaticRoutesOnly = y }
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
vcosStaticRoutesOnly :: Lens' VpnConnectionOptionsSpecification (Maybe Bool)
vcosStaticRoutesOnly f x =
    f (_vcosStaticRoutesOnly x) <&> \y -> x { _vcosStaticRoutesOnly = y }
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
aaAttributeName :: Lens' AccountAttribute (Maybe Text)
aaAttributeName f x =
    f (_aaAttributeName x) <&> \y -> x { _aaAttributeName = y }
{-# INLINE aaAttributeName #-}

-- | One or more values for the account attribute.
aaAttributeValues :: Lens' AccountAttribute [AccountAttributeValue]
aaAttributeValues f x =
    f (_aaAttributeValues x) <&> \y -> x { _aaAttributeValues = y }
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
awInstanceId f x =
    f (_awInstanceId x) <&> \y -> x { _awInstanceId = y }
{-# INLINE awInstanceId #-}

-- | The Elastic IP address.
awPublicIp :: Lens' Address (Maybe Text)
awPublicIp f x =
    f (_awPublicIp x) <&> \y -> x { _awPublicIp = y }
{-# INLINE awPublicIp #-}

-- | The ID representing the allocation of the address for use with EC2-VPC.
awAllocationId :: Lens' Address (Maybe Text)
awAllocationId f x =
    f (_awAllocationId x) <&> \y -> x { _awAllocationId = y }
{-# INLINE awAllocationId #-}

-- | The ID representing the association of the address with an instance in a
-- VPC.
awAssociationId :: Lens' Address (Maybe Text)
awAssociationId f x =
    f (_awAssociationId x) <&> \y -> x { _awAssociationId = y }
{-# INLINE awAssociationId #-}

-- | Indicates whether this Elastic IP address is for use with instances in
-- EC2-Classic (standard) or instances in a VPC (vpc).
awDomain :: Lens' Address (Maybe DomainType)
awDomain f x =
    f (_awDomain x) <&> \y -> x { _awDomain = y }
{-# INLINE awDomain #-}

-- | The ID of the network interface.
awNetworkInterfaceId :: Lens' Address (Maybe Text)
awNetworkInterfaceId f x =
    f (_awNetworkInterfaceId x) <&> \y -> x { _awNetworkInterfaceId = y }
{-# INLINE awNetworkInterfaceId #-}

-- | The ID of the AWS account that owns the network interface.
awNetworkInterfaceOwnerId :: Lens' Address (Maybe Text)
awNetworkInterfaceOwnerId f x =
    f (_awNetworkInterfaceOwnerId x) <&> \y -> x { _awNetworkInterfaceOwnerId = y }
{-# INLINE awNetworkInterfaceOwnerId #-}

-- | The private IP address associated with the Elastic IP address.
awPrivateIpAddress :: Lens' Address (Maybe Text)
awPrivateIpAddress f x =
    f (_awPrivateIpAddress x) <&> \y -> x { _awPrivateIpAddress = y }
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
azZoneName f x =
    f (_azZoneName x) <&> \y -> x { _azZoneName = y }
{-# INLINE azZoneName #-}

-- | The state of the Availability Zone.
azState :: Lens' AvailabilityZone (Maybe AvailabilityZoneState)
azState f x =
    f (_azState x) <&> \y -> x { _azState = y }
{-# INLINE azState #-}

-- | The name of the region.
azRegionName :: Lens' AvailabilityZone (Maybe Text)
azRegionName f x =
    f (_azRegionName x) <&> \y -> x { _azRegionName = y }
{-# INLINE azRegionName #-}

-- | Any messages about the Availability Zone.
azMessages :: Lens' AvailabilityZone [AvailabilityZoneMessage]
azMessages f x =
    f (_azMessages x) <&> \y -> x { _azMessages = y }
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
bdmVirtualName f x =
    f (_bdmVirtualName x) <&> \y -> x { _bdmVirtualName = y }
{-# INLINE bdmVirtualName #-}

-- | The device name exposed to the instance (for example, /dev/sdh).
bdmDeviceName :: Lens' BlockDeviceMapping Text
bdmDeviceName f x =
    f (_bdmDeviceName x) <&> \y -> x { _bdmDeviceName = y }
{-# INLINE bdmDeviceName #-}

-- | Parameters used to automatically set up Amazon EBS volumes when the
-- instance is launched.
bdmEbs :: Lens' BlockDeviceMapping (Maybe EbsBlockDevice)
bdmEbs f x =
    f (_bdmEbs x) <&> \y -> x { _bdmEbs = y }
{-# INLINE bdmEbs #-}

-- | Suppresses the specified device included in the block device mapping of the
-- AMI.
bdmNoDevice :: Lens' BlockDeviceMapping (Maybe Text)
bdmNoDevice f x =
    f (_bdmNoDevice x) <&> \y -> x { _bdmNoDevice = y }
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
btInstanceId :: Lens' BundleTask (Maybe Text)
btInstanceId f x =
    f (_btInstanceId x) <&> \y -> x { _btInstanceId = y }
{-# INLINE btInstanceId #-}

-- | The ID for this bundle task.
btBundleId :: Lens' BundleTask (Maybe Text)
btBundleId f x =
    f (_btBundleId x) <&> \y -> x { _btBundleId = y }
{-# INLINE btBundleId #-}

-- | The state of the task.
btState :: Lens' BundleTask (Maybe BundleTaskState)
btState f x =
    f (_btState x) <&> \y -> x { _btState = y }
{-# INLINE btState #-}

-- | The time this task started.
btStartTime :: Lens' BundleTask (Maybe ISO8601)
btStartTime f x =
    f (_btStartTime x) <&> \y -> x { _btStartTime = y }
{-# INLINE btStartTime #-}

-- | The time of the most recent update for the task.
btUpdateTime :: Lens' BundleTask (Maybe ISO8601)
btUpdateTime f x =
    f (_btUpdateTime x) <&> \y -> x { _btUpdateTime = y }
{-# INLINE btUpdateTime #-}

-- | The Amazon S3 storage locations.
btStorage :: Lens' BundleTask (Maybe Storage)
btStorage f x =
    f (_btStorage x) <&> \y -> x { _btStorage = y }
{-# INLINE btStorage #-}

-- | The level of task completion, as a percent (for example, 20%).
btProgress :: Lens' BundleTask (Maybe Text)
btProgress f x =
    f (_btProgress x) <&> \y -> x { _btProgress = y }
{-# INLINE btProgress #-}

-- | If the task fails, a description of the error.
btBundleTaskError :: Lens' BundleTask (Maybe BundleTaskError)
btBundleTaskError f x =
    f (_btBundleTaskError x) <&> \y -> x { _btBundleTaskError = y }
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
bteCode f x =
    f (_bteCode x) <&> \y -> x { _bteCode = y }
{-# INLINE bteCode #-}

-- | The error message.
bteMessage :: Lens' BundleTaskError (Maybe Text)
bteMessage f x =
    f (_bteMessage x) <&> \y -> x { _bteMessage = y }
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
csirSpotInstanceRequestId :: Lens' CancelledSpotInstanceRequest (Maybe Text)
csirSpotInstanceRequestId f x =
    f (_csirSpotInstanceRequestId x) <&> \y -> x { _csirSpotInstanceRequestId = y }
{-# INLINE csirSpotInstanceRequestId #-}

-- | The state of the Spot Instance request.
csirState :: Lens' CancelledSpotInstanceRequest (Maybe CancelSpotInstanceRequestState)
csirState f x =
    f (_csirState x) <&> \y -> x { _csirState = y }
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
ctConversionTaskId :: Lens' ConversionTask Text
ctConversionTaskId f x =
    f (_ctConversionTaskId x) <&> \y -> x { _ctConversionTaskId = y }
{-# INLINE ctConversionTaskId #-}

-- | The time when the task expires. If the upload isn't complete before the
-- expiration time, we automatically cancel the task.
ctExpirationTime :: Lens' ConversionTask (Maybe Text)
ctExpirationTime f x =
    f (_ctExpirationTime x) <&> \y -> x { _ctExpirationTime = y }
{-# INLINE ctExpirationTime #-}

-- | If the task is for importing an instance, this contains information about
-- the import instance task.
ctImportInstance :: Lens' ConversionTask (Maybe ImportInstanceTaskDetails)
ctImportInstance f x =
    f (_ctImportInstance x) <&> \y -> x { _ctImportInstance = y }
{-# INLINE ctImportInstance #-}

-- | If the task is for importing a volume, this contains information about the
-- import volume task.
ctImportVolume :: Lens' ConversionTask (Maybe ImportVolumeTaskDetails)
ctImportVolume f x =
    f (_ctImportVolume x) <&> \y -> x { _ctImportVolume = y }
{-# INLINE ctImportVolume #-}

-- | The state of the conversion task.
ctState :: Lens' ConversionTask ConversionTaskState
ctState f x =
    f (_ctState x) <&> \y -> x { _ctState = y }
{-# INLINE ctState #-}

-- | The status message related to the conversion task.
ctStatusMessage :: Lens' ConversionTask (Maybe Text)
ctStatusMessage f x =
    f (_ctStatusMessage x) <&> \y -> x { _ctStatusMessage = y }
{-# INLINE ctStatusMessage #-}

-- | 
ctTags :: Lens' ConversionTask [Tag]
ctTags f x =
    f (_ctTags x) <&> \y -> x { _ctTags = y }
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
cvpUserId f x =
    f (_cvpUserId x) <&> \y -> x { _cvpUserId = y }
{-# INLINE cvpUserId #-}

-- | The specific group that is to be added or removed from a volume's list of
-- create volume permissions.
cvpGroup :: Lens' CreateVolumePermission (Maybe PermissionGroup)
cvpGroup f x =
    f (_cvpGroup x) <&> \y -> x { _cvpGroup = y }
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
cvpmAdd :: Lens' CreateVolumePermissionModifications [CreateVolumePermission]
cvpmAdd f x =
    f (_cvpmAdd x) <&> \y -> x { _cvpmAdd = y }
{-# INLINE cvpmAdd #-}

-- | Removes a specific AWS account ID or group from a volume's list of create
-- volume permissions.
cvpmRemove :: Lens' CreateVolumePermissionModifications [CreateVolumePermission]
cvpmRemove f x =
    f (_cvpmRemove x) <&> \y -> x { _cvpmRemove = y }
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
cgCustomerGatewayId :: Lens' CustomerGateway (Maybe Text)
cgCustomerGatewayId f x =
    f (_cgCustomerGatewayId x) <&> \y -> x { _cgCustomerGatewayId = y }
{-# INLINE cgCustomerGatewayId #-}

-- | The current state of the customer gateway.
cgState :: Lens' CustomerGateway (Maybe Text)
cgState f x =
    f (_cgState x) <&> \y -> x { _cgState = y }
{-# INLINE cgState #-}

-- | The type of VPN connection the customer gateway supports.
cgType :: Lens' CustomerGateway (Maybe Text)
cgType f x =
    f (_cgType x) <&> \y -> x { _cgType = y }
{-# INLINE cgType #-}

-- | The Internet-routable IP address of the customer gateway's outside
-- interface.
cgIpAddress :: Lens' CustomerGateway (Maybe Text)
cgIpAddress f x =
    f (_cgIpAddress x) <&> \y -> x { _cgIpAddress = y }
{-# INLINE cgIpAddress #-}

-- | The customer gateway's Border Gateway Protocol (BGP) Autonomous System
-- Number (ASN).
cgBgpAsn :: Lens' CustomerGateway (Maybe Text)
cgBgpAsn f x =
    f (_cgBgpAsn x) <&> \y -> x { _cgBgpAsn = y }
{-# INLINE cgBgpAsn #-}

-- | Any tags assigned to the customer gateway.
cgTags :: Lens' CustomerGateway [Tag]
cgTags f x =
    f (_cgTags x) <&> \y -> x { _cgTags = y }
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
dcKey f x =
    f (_dcKey x) <&> \y -> x { _dcKey = y }
{-# INLINE dcKey #-}

-- | One or more values for the DHCP option.
dcValues :: Lens' DhcpConfiguration [Text]
dcValues f x =
    f (_dcValues x) <&> \y -> x { _dcValues = y }
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
doDhcpOptionsId :: Lens' DhcpOptions (Maybe Text)
doDhcpOptionsId f x =
    f (_doDhcpOptionsId x) <&> \y -> x { _doDhcpOptionsId = y }
{-# INLINE doDhcpOptionsId #-}

-- | One or more DHCP options in the set.
doDhcpConfigurations :: Lens' DhcpOptions [DhcpConfiguration]
doDhcpConfigurations f x =
    f (_doDhcpConfigurations x) <&> \y -> x { _doDhcpConfigurations = y }
{-# INLINE doDhcpConfigurations #-}

-- | Any tags assigned to the DHCP options set.
doTags :: Lens' DhcpOptions [Tag]
doTags f x =
    f (_doTags x) <&> \y -> x { _doTags = y }
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
dmImage f x =
    f (_dmImage x) <&> \y -> x { _dmImage = y }
{-# INLINE dmImage #-}

-- | 
dmDescription :: Lens' DiskImage (Maybe Text)
dmDescription f x =
    f (_dmDescription x) <&> \y -> x { _dmDescription = y }
{-# INLINE dmDescription #-}

-- | 
dmVolume :: Lens' DiskImage (Maybe VolumeDetail)
dmVolume f x =
    f (_dmVolume x) <&> \y -> x { _dmVolume = y }
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
didFormat :: Lens' DiskImageDescription DiskImageFormat
didFormat f x =
    f (_didFormat x) <&> \y -> x { _didFormat = y }
{-# INLINE didFormat #-}

-- | The size of the disk image.
didSize :: Lens' DiskImageDescription Integer
didSize f x =
    f (_didSize x) <&> \y -> x { _didSize = y }
{-# INLINE didSize #-}

-- | A presigned URL for the import manifest stored in Amazon S3. For
-- information about creating a presigned URL for an Amazon S3 object, read
-- the "Query String Request Authentication Alternative" section of the
-- Authenticating REST Requests topic in the Amazon Simple Storage Service
-- Developer Guide.
didImportManifestUrl :: Lens' DiskImageDescription Text
didImportManifestUrl f x =
    f (_didImportManifestUrl x) <&> \y -> x { _didImportManifestUrl = y }
{-# INLINE didImportManifestUrl #-}

-- | The checksum computed for the disk image.
didChecksum :: Lens' DiskImageDescription (Maybe Text)
didChecksum f x =
    f (_didChecksum x) <&> \y -> x { _didChecksum = y }
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
dikFormat :: Lens' DiskImageDetail DiskImageFormat
dikFormat f x =
    f (_dikFormat x) <&> \y -> x { _dikFormat = y }
{-# INLINE dikFormat #-}

-- | 
dikBytes :: Lens' DiskImageDetail Integer
dikBytes f x =
    f (_dikBytes x) <&> \y -> x { _dikBytes = y }
{-# INLINE dikBytes #-}

-- | A presigned URL for the import manifest stored in Amazon S3. For
-- information about creating a presigned URL for an Amazon S3 object, read
-- the "Query String Request Authentication Alternative" section of the
-- Authenticating REST Requests topic in the Amazon Simple Storage Service
-- Developer Guide.
dikImportManifestUrl :: Lens' DiskImageDetail Text
dikImportManifestUrl f x =
    f (_dikImportManifestUrl x) <&> \y -> x { _dikImportManifestUrl = y }
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
divdSize :: Lens' DiskImageVolumeDescription (Maybe Integer)
divdSize f x =
    f (_divdSize x) <&> \y -> x { _divdSize = y }
{-# INLINE divdSize #-}

-- | The volume identifier.
divdId :: Lens' DiskImageVolumeDescription Text
divdId f x =
    f (_divdId x) <&> \y -> x { _divdId = y }
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
ebdSnapshotId :: Lens' EbsBlockDevice (Maybe Text)
ebdSnapshotId f x =
    f (_ebdSnapshotId x) <&> \y -> x { _ebdSnapshotId = y }
{-# INLINE ebdSnapshotId #-}

-- | The size of the volume, in GiB. Constraints: If the volume type is io1, the
-- minimum size of the volume is 10 GiB; otherwise, the minimum size is 1 GiB.
-- The maximum volume size is 1024 GiB. Default: If you're creating the volume
-- from a snapshot and don't specify a volume size, the default is the
-- snapshot size.
ebdVolumeSize :: Lens' EbsBlockDevice (Maybe Integer)
ebdVolumeSize f x =
    f (_ebdVolumeSize x) <&> \y -> x { _ebdVolumeSize = y }
{-# INLINE ebdVolumeSize #-}

-- | Indicates whether the Amazon EBS volume is deleted on instance termination.
ebdDeleteOnTermination :: Lens' EbsBlockDevice (Maybe Bool)
ebdDeleteOnTermination f x =
    f (_ebdDeleteOnTermination x) <&> \y -> x { _ebdDeleteOnTermination = y }
{-# INLINE ebdDeleteOnTermination #-}

-- | The volume type. gp2 for General Purpose (SSD) volumes, io1 for Provisioned
-- IOPS (SSD) volumes, and standard for Magnetic volumes. Default: standard.
ebdVolumeType :: Lens' EbsBlockDevice (Maybe VolumeType)
ebdVolumeType f x =
    f (_ebdVolumeType x) <&> \y -> x { _ebdVolumeType = y }
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
ebdIops f x =
    f (_ebdIops x) <&> \y -> x { _ebdIops = y }
{-# INLINE ebdIops #-}

-- | Indicates whether the Amazon EBS volume is encrypted.
ebdEncrypted :: Lens' EbsBlockDevice (Maybe Bool)
ebdEncrypted f x =
    f (_ebdEncrypted x) <&> \y -> x { _ebdEncrypted = y }
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
eibdVolumeId :: Lens' EbsInstanceBlockDevice (Maybe Text)
eibdVolumeId f x =
    f (_eibdVolumeId x) <&> \y -> x { _eibdVolumeId = y }
{-# INLINE eibdVolumeId #-}

-- | The attachment state.
eibdStatus :: Lens' EbsInstanceBlockDevice (Maybe AttachmentStatus)
eibdStatus f x =
    f (_eibdStatus x) <&> \y -> x { _eibdStatus = y }
{-# INLINE eibdStatus #-}

-- | The time stamp when the attachment initiated.
eibdAttachTime :: Lens' EbsInstanceBlockDevice (Maybe ISO8601)
eibdAttachTime f x =
    f (_eibdAttachTime x) <&> \y -> x { _eibdAttachTime = y }
{-# INLINE eibdAttachTime #-}

-- | Indicates whether the volume is deleted on instance termination.
eibdDeleteOnTermination :: Lens' EbsInstanceBlockDevice (Maybe Bool)
eibdDeleteOnTermination f x =
    f (_eibdDeleteOnTermination x) <&> \y -> x { _eibdDeleteOnTermination = y }
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
eibdsVolumeId :: Lens' EbsInstanceBlockDeviceSpecification (Maybe Text)
eibdsVolumeId f x =
    f (_eibdsVolumeId x) <&> \y -> x { _eibdsVolumeId = y }
{-# INLINE eibdsVolumeId #-}

-- | Indicates whether the volume is deleted on instance termination.
eibdsDeleteOnTermination :: Lens' EbsInstanceBlockDeviceSpecification (Maybe Bool)
eibdsDeleteOnTermination f x =
    f (_eibdsDeleteOnTermination x) <&> \y -> x { _eibdsDeleteOnTermination = y }
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
etExportTaskId :: Lens' ExportTask (Maybe Text)
etExportTaskId f x =
    f (_etExportTaskId x) <&> \y -> x { _etExportTaskId = y }
{-# INLINE etExportTaskId #-}

-- | A description of the resource being exported.
etDescription :: Lens' ExportTask (Maybe Text)
etDescription f x =
    f (_etDescription x) <&> \y -> x { _etDescription = y }
{-# INLINE etDescription #-}

-- | The state of the conversion task.
etState :: Lens' ExportTask (Maybe ExportTaskState)
etState f x =
    f (_etState x) <&> \y -> x { _etState = y }
{-# INLINE etState #-}

-- | The status message related to the export task.
etStatusMessage :: Lens' ExportTask (Maybe Text)
etStatusMessage f x =
    f (_etStatusMessage x) <&> \y -> x { _etStatusMessage = y }
{-# INLINE etStatusMessage #-}

-- | The instance being exported.
etInstanceExportDetails :: Lens' ExportTask (Maybe InstanceExportDetails)
etInstanceExportDetails f x =
    f (_etInstanceExportDetails x) <&> \y -> x { _etInstanceExportDetails = y }
{-# INLINE etInstanceExportDetails #-}

-- | 
etExportToS3Task :: Lens' ExportTask (Maybe ExportToS3Task)
etExportToS3Task f x =
    f (_etExportToS3Task x) <&> \y -> x { _etExportToS3Task = y }
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
etstDiskImageFormat f x =
    f (_etstDiskImageFormat x) <&> \y -> x { _etstDiskImageFormat = y }
{-# INLINE etstDiskImageFormat #-}

-- | The container format used to combine disk images with metadata (such as
-- OVF). If absent, only the disk image is exported.
etstContainerFormat :: Lens' ExportToS3Task (Maybe ContainerFormat)
etstContainerFormat f x =
    f (_etstContainerFormat x) <&> \y -> x { _etstContainerFormat = y }
{-# INLINE etstContainerFormat #-}

-- | The Amazon S3 bucket for the destination image. The destination bucket must
-- exist and grant WRITE and READ_ACL permissions to the AWS account
-- vm-import-export@amazon.com.
etstS3Bucket :: Lens' ExportToS3Task (Maybe Text)
etstS3Bucket f x =
    f (_etstS3Bucket x) <&> \y -> x { _etstS3Bucket = y }
{-# INLINE etstS3Bucket #-}

-- | 
etstS3Key :: Lens' ExportToS3Task (Maybe Text)
etstS3Key f x =
    f (_etstS3Key x) <&> \y -> x { _etstS3Key = y }
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
etstsDiskImageFormat :: Lens' ExportToS3TaskSpecification (Maybe DiskImageFormat)
etstsDiskImageFormat f x =
    f (_etstsDiskImageFormat x) <&> \y -> x { _etstsDiskImageFormat = y }
{-# INLINE etstsDiskImageFormat #-}

-- | 
etstsContainerFormat :: Lens' ExportToS3TaskSpecification (Maybe ContainerFormat)
etstsContainerFormat f x =
    f (_etstsContainerFormat x) <&> \y -> x { _etstsContainerFormat = y }
{-# INLINE etstsContainerFormat #-}

-- | 
etstsS3Bucket :: Lens' ExportToS3TaskSpecification (Maybe Text)
etstsS3Bucket f x =
    f (_etstsS3Bucket x) <&> \y -> x { _etstsS3Bucket = y }
{-# INLINE etstsS3Bucket #-}

-- | The image is written to a single object in the Amazon S3 bucket at the S3
-- key s3prefix + exportTaskId + '.' + diskImageFormat.
etstsS3Prefix :: Lens' ExportToS3TaskSpecification (Maybe Text)
etstsS3Prefix f x =
    f (_etstsS3Prefix x) <&> \y -> x { _etstsS3Prefix = y }
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
frName :: Lens' Filter Text
frName f x =
    f (_frName x) <&> \y -> x { _frName = y }
{-# INLINE frName #-}

-- | One or more filter values.
frValues :: Lens' Filter [Text]
frValues f x =
    f (_frValues x) <&> \y -> x { _frValues = y }
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
giGroupName :: Lens' GroupIdentifier (Maybe Text)
giGroupName f x =
    f (_giGroupName x) <&> \y -> x { _giGroupName = y }
{-# INLINE giGroupName #-}

-- | The ID of the security group.
giGroupId :: Lens' GroupIdentifier (Maybe Text)
giGroupId f x =
    f (_giGroupId x) <&> \y -> x { _giGroupId = y }
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
iipArn :: Lens' IamInstanceProfile (Maybe Text)
iipArn f x =
    f (_iipArn x) <&> \y -> x { _iipArn = y }
{-# INLINE iipArn #-}

-- | The ID of the instance profile.
iipId :: Lens' IamInstanceProfile (Maybe Text)
iipId f x =
    f (_iipId x) <&> \y -> x { _iipId = y }
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
iipsArn :: Lens' IamInstanceProfileSpecification (Maybe Text)
iipsArn f x =
    f (_iipsArn x) <&> \y -> x { _iipsArn = y }
{-# INLINE iipsArn #-}

-- | The name of the instance profile.
iipsName :: Lens' IamInstanceProfileSpecification (Maybe Text)
iipsName f x =
    f (_iipsName x) <&> \y -> x { _iipsName = y }
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
itcType :: Lens' IcmpTypeCode (Maybe Integer)
itcType f x =
    f (_itcType x) <&> \y -> x { _itcType = y }
{-# INLINE itcType #-}

-- | The ICMP type. A value of -1 means all types.
itcCode :: Lens' IcmpTypeCode (Maybe Integer)
itcCode f x =
    f (_itcCode x) <&> \y -> x { _itcCode = y }
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
ieImageId :: Lens' Image Text
ieImageId f x =
    f (_ieImageId x) <&> \y -> x { _ieImageId = y }
{-# INLINE ieImageId #-}

-- | The location of the AMI.
ieImageLocation :: Lens' Image Text
ieImageLocation f x =
    f (_ieImageLocation x) <&> \y -> x { _ieImageLocation = y }
{-# INLINE ieImageLocation #-}

-- | The current state of the AMI. If the state is available, the image is
-- successfully registered and can be used to launch an instance.
ieState :: Lens' Image ImageState
ieState f x =
    f (_ieState x) <&> \y -> x { _ieState = y }
{-# INLINE ieState #-}

-- | The AWS account ID of the image owner.
ieOwnerId :: Lens' Image Text
ieOwnerId f x =
    f (_ieOwnerId x) <&> \y -> x { _ieOwnerId = y }
{-# INLINE ieOwnerId #-}

-- | Indicates whether the image has public launch permissions. The value is
-- true if this image has public launch permissions or false if it has only
-- implicit and explicit launch permissions.
iePublic :: Lens' Image Bool
iePublic f x =
    f (_iePublic x) <&> \y -> x { _iePublic = y }
{-# INLINE iePublic #-}

-- | Any product codes associated with the AMI.
ieProductCodes :: Lens' Image [ProductCode]
ieProductCodes f x =
    f (_ieProductCodes x) <&> \y -> x { _ieProductCodes = y }
{-# INLINE ieProductCodes #-}

-- | The architecture of the image.
ieArchitecture :: Lens' Image ArchitectureValues
ieArchitecture f x =
    f (_ieArchitecture x) <&> \y -> x { _ieArchitecture = y }
{-# INLINE ieArchitecture #-}

-- | The type of image.
ieImageType :: Lens' Image ImageTypeValues
ieImageType f x =
    f (_ieImageType x) <&> \y -> x { _ieImageType = y }
{-# INLINE ieImageType #-}

-- | The kernel associated with the image, if any. Only applicable for machine
-- images.
ieKernelId :: Lens' Image (Maybe Text)
ieKernelId f x =
    f (_ieKernelId x) <&> \y -> x { _ieKernelId = y }
{-# INLINE ieKernelId #-}

-- | The RAM disk associated with the image, if any. Only applicable for machine
-- images.
ieRamdiskId :: Lens' Image (Maybe Text)
ieRamdiskId f x =
    f (_ieRamdiskId x) <&> \y -> x { _ieRamdiskId = y }
{-# INLINE ieRamdiskId #-}

-- | The value is Windows for Windows AMIs; otherwise blank.
iePlatform :: Lens' Image (Maybe PlatformValues)
iePlatform f x =
    f (_iePlatform x) <&> \y -> x { _iePlatform = y }
{-# INLINE iePlatform #-}

-- | Specifies whether enhanced networking is enabled.
ieSriovNetSupport :: Lens' Image (Maybe Text)
ieSriovNetSupport f x =
    f (_ieSriovNetSupport x) <&> \y -> x { _ieSriovNetSupport = y }
{-# INLINE ieSriovNetSupport #-}

-- | The reason for the state change.
ieStateReason :: Lens' Image (Maybe StateReason)
ieStateReason f x =
    f (_ieStateReason x) <&> \y -> x { _ieStateReason = y }
{-# INLINE ieStateReason #-}

-- | The AWS account alias (for example, amazon, self) or the AWS account ID of
-- the AMI owner.
ieImageOwnerAlias :: Lens' Image (Maybe Text)
ieImageOwnerAlias f x =
    f (_ieImageOwnerAlias x) <&> \y -> x { _ieImageOwnerAlias = y }
{-# INLINE ieImageOwnerAlias #-}

-- | The name of the AMI that was provided during image creation.
ieName :: Lens' Image Text
ieName f x =
    f (_ieName x) <&> \y -> x { _ieName = y }
{-# INLINE ieName #-}

-- | The description of the AMI that was provided during image creation.
ieDescription :: Lens' Image (Maybe Text)
ieDescription f x =
    f (_ieDescription x) <&> \y -> x { _ieDescription = y }
{-# INLINE ieDescription #-}

-- | The type of root device used by the AMI. The AMI can use an Amazon EBS
-- volume or an instance store volume.
ieRootDeviceType :: Lens' Image DeviceType
ieRootDeviceType f x =
    f (_ieRootDeviceType x) <&> \y -> x { _ieRootDeviceType = y }
{-# INLINE ieRootDeviceType #-}

-- | The device name of the root device (for example, /dev/sda1 or xvda).
ieRootDeviceName :: Lens' Image (Maybe Text)
ieRootDeviceName f x =
    f (_ieRootDeviceName x) <&> \y -> x { _ieRootDeviceName = y }
{-# INLINE ieRootDeviceName #-}

-- | Any block device mapping entries.
ieBlockDeviceMappings :: Lens' Image [BlockDeviceMapping]
ieBlockDeviceMappings f x =
    f (_ieBlockDeviceMappings x) <&> \y -> x { _ieBlockDeviceMappings = y }
{-# INLINE ieBlockDeviceMappings #-}

-- | The type of virtualization of the AMI.
ieVirtualizationType :: Lens' Image VirtualizationType
ieVirtualizationType f x =
    f (_ieVirtualizationType x) <&> \y -> x { _ieVirtualizationType = y }
{-# INLINE ieVirtualizationType #-}

-- | Any tags assigned to the image.
ieTags :: Lens' Image [Tag]
ieTags f x =
    f (_ieTags x) <&> \y -> x { _ieTags = y }
{-# INLINE ieTags #-}

-- | The hypervisor type of the image.
ieHypervisor :: Lens' Image HypervisorType
ieHypervisor f x =
    f (_ieHypervisor x) <&> \y -> x { _ieHypervisor = y }
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
iilsArchitecture f x =
    f (_iilsArchitecture x) <&> \y -> x { _iilsArchitecture = y }
{-# INLINE iilsArchitecture #-}

-- | One or more security group names.
iilsGroupNames :: Lens' ImportInstanceLaunchSpecification [Text]
iilsGroupNames f x =
    f (_iilsGroupNames x) <&> \y -> x { _iilsGroupNames = y }
{-# INLINE iilsGroupNames #-}

-- | 
iilsAdditionalInfo :: Lens' ImportInstanceLaunchSpecification (Maybe Text)
iilsAdditionalInfo f x =
    f (_iilsAdditionalInfo x) <&> \y -> x { _iilsAdditionalInfo = y }
{-# INLINE iilsAdditionalInfo #-}

-- | User data to be made available to the instance.
iilsUserData :: Lens' ImportInstanceLaunchSpecification (Maybe Text)
iilsUserData f x =
    f (_iilsUserData x) <&> \y -> x { _iilsUserData = y }
{-# INLINE iilsUserData #-}

-- | The instance type. For more information, see Instance Types in the Amazon
-- Elastic Compute Cloud User Guide.
iilsInstanceType :: Lens' ImportInstanceLaunchSpecification (Maybe InstanceType)
iilsInstanceType f x =
    f (_iilsInstanceType x) <&> \y -> x { _iilsInstanceType = y }
{-# INLINE iilsInstanceType #-}

-- | 
iilsPlacement :: Lens' ImportInstanceLaunchSpecification (Maybe Placement)
iilsPlacement f x =
    f (_iilsPlacement x) <&> \y -> x { _iilsPlacement = y }
{-# INLINE iilsPlacement #-}

-- | 
iilsMonitoring :: Lens' ImportInstanceLaunchSpecification (Maybe Bool)
iilsMonitoring f x =
    f (_iilsMonitoring x) <&> \y -> x { _iilsMonitoring = y }
{-# INLINE iilsMonitoring #-}

-- | [EC2-VPC] The ID of the subnet to launch the instance into.
iilsSubnetId :: Lens' ImportInstanceLaunchSpecification (Maybe Text)
iilsSubnetId f x =
    f (_iilsSubnetId x) <&> \y -> x { _iilsSubnetId = y }
{-# INLINE iilsSubnetId #-}

-- | Indicates whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for system
-- shutdown).
iilsInstanceInitiatedShutdownBehavior :: Lens' ImportInstanceLaunchSpecification (Maybe ShutdownBehavior)
iilsInstanceInitiatedShutdownBehavior f x =
    f (_iilsInstanceInitiatedShutdownBehavior x) <&> \y -> x { _iilsInstanceInitiatedShutdownBehavior = y }
{-# INLINE iilsInstanceInitiatedShutdownBehavior #-}

-- | [EC2-VPC] Optionally, you can use this parameter to assign the instance a
-- specific available IP address from the IP address range of the subnet.
iilsPrivateIpAddress :: Lens' ImportInstanceLaunchSpecification (Maybe Text)
iilsPrivateIpAddress f x =
    f (_iilsPrivateIpAddress x) <&> \y -> x { _iilsPrivateIpAddress = y }
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
iitdVolumes :: Lens' ImportInstanceTaskDetails [ImportInstanceVolumeDetailItem]
iitdVolumes f x =
    f (_iitdVolumes x) <&> \y -> x { _iitdVolumes = y }
{-# INLINE iitdVolumes #-}

-- | 
iitdInstanceId :: Lens' ImportInstanceTaskDetails (Maybe Text)
iitdInstanceId f x =
    f (_iitdInstanceId x) <&> \y -> x { _iitdInstanceId = y }
{-# INLINE iitdInstanceId #-}

-- | The instance operating system.
iitdPlatform :: Lens' ImportInstanceTaskDetails (Maybe PlatformValues)
iitdPlatform f x =
    f (_iitdPlatform x) <&> \y -> x { _iitdPlatform = y }
{-# INLINE iitdPlatform #-}

-- | 
iitdDescription :: Lens' ImportInstanceTaskDetails (Maybe Text)
iitdDescription f x =
    f (_iitdDescription x) <&> \y -> x { _iitdDescription = y }
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
iivdiBytesConverted :: Lens' ImportInstanceVolumeDetailItem Integer
iivdiBytesConverted f x =
    f (_iivdiBytesConverted x) <&> \y -> x { _iivdiBytesConverted = y }
{-# INLINE iivdiBytesConverted #-}

-- | The Availability Zone where the resulting instance will reside.
iivdiAvailabilityZone :: Lens' ImportInstanceVolumeDetailItem Text
iivdiAvailabilityZone f x =
    f (_iivdiAvailabilityZone x) <&> \y -> x { _iivdiAvailabilityZone = y }
{-# INLINE iivdiAvailabilityZone #-}

-- | The image.
iivdiImage :: Lens' ImportInstanceVolumeDetailItem DiskImageDescription
iivdiImage f x =
    f (_iivdiImage x) <&> \y -> x { _iivdiImage = y }
{-# INLINE iivdiImage #-}

-- | The volume.
iivdiVolume :: Lens' ImportInstanceVolumeDetailItem DiskImageVolumeDescription
iivdiVolume f x =
    f (_iivdiVolume x) <&> \y -> x { _iivdiVolume = y }
{-# INLINE iivdiVolume #-}

-- | The status of the import of this particular disk image.
iivdiStatus :: Lens' ImportInstanceVolumeDetailItem Text
iivdiStatus f x =
    f (_iivdiStatus x) <&> \y -> x { _iivdiStatus = y }
{-# INLINE iivdiStatus #-}

-- | The status information or errors related to the disk image.
iivdiStatusMessage :: Lens' ImportInstanceVolumeDetailItem (Maybe Text)
iivdiStatusMessage f x =
    f (_iivdiStatusMessage x) <&> \y -> x { _iivdiStatusMessage = y }
{-# INLINE iivdiStatusMessage #-}

-- | 
iivdiDescription :: Lens' ImportInstanceVolumeDetailItem (Maybe Text)
iivdiDescription f x =
    f (_iivdiDescription x) <&> \y -> x { _iivdiDescription = y }
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
ivtdBytesConverted :: Lens' ImportVolumeTaskDetails Integer
ivtdBytesConverted f x =
    f (_ivtdBytesConverted x) <&> \y -> x { _ivtdBytesConverted = y }
{-# INLINE ivtdBytesConverted #-}

-- | The Availability Zone where the resulting volume will reside.
ivtdAvailabilityZone :: Lens' ImportVolumeTaskDetails Text
ivtdAvailabilityZone f x =
    f (_ivtdAvailabilityZone x) <&> \y -> x { _ivtdAvailabilityZone = y }
{-# INLINE ivtdAvailabilityZone #-}

-- | The description you provided when starting the import volume task.
ivtdDescription :: Lens' ImportVolumeTaskDetails (Maybe Text)
ivtdDescription f x =
    f (_ivtdDescription x) <&> \y -> x { _ivtdDescription = y }
{-# INLINE ivtdDescription #-}

-- | The image.
ivtdImage :: Lens' ImportVolumeTaskDetails DiskImageDescription
ivtdImage f x =
    f (_ivtdImage x) <&> \y -> x { _ivtdImage = y }
{-# INLINE ivtdImage #-}

-- | The volume.
ivtdVolume :: Lens' ImportVolumeTaskDetails DiskImageVolumeDescription
ivtdVolume f x =
    f (_ivtdVolume x) <&> \y -> x { _ivtdVolume = y }
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
ifInstanceId :: Lens' Instance (Maybe Text)
ifInstanceId f x =
    f (_ifInstanceId x) <&> \y -> x { _ifInstanceId = y }
{-# INLINE ifInstanceId #-}

-- | The ID of the AMI used to launch the instance.
ifImageId :: Lens' Instance (Maybe Text)
ifImageId f x =
    f (_ifImageId x) <&> \y -> x { _ifImageId = y }
{-# INLINE ifImageId #-}

-- | The current state of the instance.
ifState :: Lens' Instance (Maybe InstanceState)
ifState f x =
    f (_ifState x) <&> \y -> x { _ifState = y }
{-# INLINE ifState #-}

-- | The private DNS name assigned to the instance. This DNS name can only be
-- used inside the Amazon EC2 network. This name is not available until the
-- instance enters the running state.
ifPrivateDnsName :: Lens' Instance (Maybe Text)
ifPrivateDnsName f x =
    f (_ifPrivateDnsName x) <&> \y -> x { _ifPrivateDnsName = y }
{-# INLINE ifPrivateDnsName #-}

-- | The public DNS name assigned to the instance. This name is not available
-- until the instance enters the running state.
ifPublicDnsName :: Lens' Instance (Maybe Text)
ifPublicDnsName f x =
    f (_ifPublicDnsName x) <&> \y -> x { _ifPublicDnsName = y }
{-# INLINE ifPublicDnsName #-}

-- | The reason for the most recent state transition. This might be an empty
-- string.
ifStateTransitionReason :: Lens' Instance (Maybe Text)
ifStateTransitionReason f x =
    f (_ifStateTransitionReason x) <&> \y -> x { _ifStateTransitionReason = y }
{-# INLINE ifStateTransitionReason #-}

-- | The name of the key pair, if this instance was launched with an associated
-- key pair.
ifKeyName :: Lens' Instance (Maybe Text)
ifKeyName f x =
    f (_ifKeyName x) <&> \y -> x { _ifKeyName = y }
{-# INLINE ifKeyName #-}

-- | The AMI launch index, which can be used to find this instance in the launch
-- group.
ifAmiLaunchIndex :: Lens' Instance (Maybe Integer)
ifAmiLaunchIndex f x =
    f (_ifAmiLaunchIndex x) <&> \y -> x { _ifAmiLaunchIndex = y }
{-# INLINE ifAmiLaunchIndex #-}

-- | The product codes attached to this instance.
ifProductCodes :: Lens' Instance [ProductCode]
ifProductCodes f x =
    f (_ifProductCodes x) <&> \y -> x { _ifProductCodes = y }
{-# INLINE ifProductCodes #-}

-- | The instance type.
ifInstanceType :: Lens' Instance (Maybe InstanceType)
ifInstanceType f x =
    f (_ifInstanceType x) <&> \y -> x { _ifInstanceType = y }
{-# INLINE ifInstanceType #-}

-- | The time the instance was launched.
ifLaunchTime :: Lens' Instance (Maybe ISO8601)
ifLaunchTime f x =
    f (_ifLaunchTime x) <&> \y -> x { _ifLaunchTime = y }
{-# INLINE ifLaunchTime #-}

-- | The location where the instance launched.
ifPlacement :: Lens' Instance (Maybe Placement)
ifPlacement f x =
    f (_ifPlacement x) <&> \y -> x { _ifPlacement = y }
{-# INLINE ifPlacement #-}

-- | The kernel associated with this instance.
ifKernelId :: Lens' Instance (Maybe Text)
ifKernelId f x =
    f (_ifKernelId x) <&> \y -> x { _ifKernelId = y }
{-# INLINE ifKernelId #-}

-- | The RAM disk associated with this instance.
ifRamdiskId :: Lens' Instance (Maybe Text)
ifRamdiskId f x =
    f (_ifRamdiskId x) <&> \y -> x { _ifRamdiskId = y }
{-# INLINE ifRamdiskId #-}

-- | The value is Windows for Windows instances; otherwise blank.
ifPlatform :: Lens' Instance (Maybe PlatformValues)
ifPlatform f x =
    f (_ifPlatform x) <&> \y -> x { _ifPlatform = y }
{-# INLINE ifPlatform #-}

-- | The monitoring information for the instance.
ifMonitoring :: Lens' Instance (Maybe Monitoring)
ifMonitoring f x =
    f (_ifMonitoring x) <&> \y -> x { _ifMonitoring = y }
{-# INLINE ifMonitoring #-}

-- | The ID of the subnet in which the instance is running.
ifSubnetId :: Lens' Instance (Maybe Text)
ifSubnetId f x =
    f (_ifSubnetId x) <&> \y -> x { _ifSubnetId = y }
{-# INLINE ifSubnetId #-}

-- | The ID of the VPC in which the instance is running.
ifVpcId :: Lens' Instance (Maybe Text)
ifVpcId f x =
    f (_ifVpcId x) <&> \y -> x { _ifVpcId = y }
{-# INLINE ifVpcId #-}

-- | The private IP address assigned to the instance.
ifPrivateIpAddress :: Lens' Instance (Maybe Text)
ifPrivateIpAddress f x =
    f (_ifPrivateIpAddress x) <&> \y -> x { _ifPrivateIpAddress = y }
{-# INLINE ifPrivateIpAddress #-}

-- | The public IP address assigned to the instance.
ifPublicIpAddress :: Lens' Instance (Maybe Text)
ifPublicIpAddress f x =
    f (_ifPublicIpAddress x) <&> \y -> x { _ifPublicIpAddress = y }
{-# INLINE ifPublicIpAddress #-}

-- | The reason for the most recent state transition.
ifStateReason :: Lens' Instance (Maybe StateReason)
ifStateReason f x =
    f (_ifStateReason x) <&> \y -> x { _ifStateReason = y }
{-# INLINE ifStateReason #-}

-- | The architecture of the image.
ifArchitecture :: Lens' Instance (Maybe ArchitectureValues)
ifArchitecture f x =
    f (_ifArchitecture x) <&> \y -> x { _ifArchitecture = y }
{-# INLINE ifArchitecture #-}

-- | The root device type used by the AMI. The AMI can use an Amazon EBS volume
-- or an instance store volume.
ifRootDeviceType :: Lens' Instance (Maybe DeviceType)
ifRootDeviceType f x =
    f (_ifRootDeviceType x) <&> \y -> x { _ifRootDeviceType = y }
{-# INLINE ifRootDeviceType #-}

-- | The root device name (for example, /dev/sda1).
ifRootDeviceName :: Lens' Instance (Maybe Text)
ifRootDeviceName f x =
    f (_ifRootDeviceName x) <&> \y -> x { _ifRootDeviceName = y }
{-# INLINE ifRootDeviceName #-}

-- | Any block device mapping entries for the instance.
ifBlockDeviceMappings :: Lens' Instance [InstanceBlockDeviceMapping]
ifBlockDeviceMappings f x =
    f (_ifBlockDeviceMappings x) <&> \y -> x { _ifBlockDeviceMappings = y }
{-# INLINE ifBlockDeviceMappings #-}

-- | The virtualization type of the instance.
ifVirtualizationType :: Lens' Instance (Maybe VirtualizationType)
ifVirtualizationType f x =
    f (_ifVirtualizationType x) <&> \y -> x { _ifVirtualizationType = y }
{-# INLINE ifVirtualizationType #-}

-- | Indicates whether this is a Spot Instance.
ifInstanceLifecycle :: Lens' Instance (Maybe InstanceLifecycleType)
ifInstanceLifecycle f x =
    f (_ifInstanceLifecycle x) <&> \y -> x { _ifInstanceLifecycle = y }
{-# INLINE ifInstanceLifecycle #-}

-- | The ID of the Spot Instance request.
ifSpotInstanceRequestId :: Lens' Instance (Maybe Text)
ifSpotInstanceRequestId f x =
    f (_ifSpotInstanceRequestId x) <&> \y -> x { _ifSpotInstanceRequestId = y }
{-# INLINE ifSpotInstanceRequestId #-}

-- | The idempotency token you provided when you launched the instance.
ifClientToken :: Lens' Instance (Maybe Text)
ifClientToken f x =
    f (_ifClientToken x) <&> \y -> x { _ifClientToken = y }
{-# INLINE ifClientToken #-}

-- | Any tags assigned to the instance.
ifTags :: Lens' Instance [Tag]
ifTags f x =
    f (_ifTags x) <&> \y -> x { _ifTags = y }
{-# INLINE ifTags #-}

-- | One or more security groups for the instance.
ifSecurityGroups :: Lens' Instance [GroupIdentifier]
ifSecurityGroups f x =
    f (_ifSecurityGroups x) <&> \y -> x { _ifSecurityGroups = y }
{-# INLINE ifSecurityGroups #-}

-- | Specifies whether to enable an instance launched in a VPC to perform NAT.
-- This controls whether source/destination checking is enabled on the
-- instance. A value of true means checking is enabled, and false means
-- checking is disabled. The value must be false for the instance to perform
-- NAT. For more information, see NAT Instances in the Amazon Virtual Private
-- Cloud User Guide.
ifSourceDestCheck :: Lens' Instance (Maybe Bool)
ifSourceDestCheck f x =
    f (_ifSourceDestCheck x) <&> \y -> x { _ifSourceDestCheck = y }
{-# INLINE ifSourceDestCheck #-}

-- | The hypervisor type of the instance.
ifHypervisor :: Lens' Instance (Maybe HypervisorType)
ifHypervisor f x =
    f (_ifHypervisor x) <&> \y -> x { _ifHypervisor = y }
{-# INLINE ifHypervisor #-}

-- | [EC2-VPC] One or more network interfaces for the instance.
ifNetworkInterfaces :: Lens' Instance [InstanceNetworkInterface]
ifNetworkInterfaces f x =
    f (_ifNetworkInterfaces x) <&> \y -> x { _ifNetworkInterfaces = y }
{-# INLINE ifNetworkInterfaces #-}

-- | The IAM instance profile associated with the instance.
ifIamInstanceProfile :: Lens' Instance (Maybe IamInstanceProfile)
ifIamInstanceProfile f x =
    f (_ifIamInstanceProfile x) <&> \y -> x { _ifIamInstanceProfile = y }
{-# INLINE ifIamInstanceProfile #-}

-- | Indicates whether the instance is optimized for EBS I/O. This optimization
-- provides dedicated throughput to Amazon EBS and an optimized configuration
-- stack to provide optimal I/O performance. This optimization isn't available
-- with all instance types. Additional usage charges apply when using an EBS
-- Optimized instance.
ifEbsOptimized :: Lens' Instance (Maybe Bool)
ifEbsOptimized f x =
    f (_ifEbsOptimized x) <&> \y -> x { _ifEbsOptimized = y }
{-# INLINE ifEbsOptimized #-}

-- | Specifies whether enhanced networking is enabled.
ifSriovNetSupport :: Lens' Instance (Maybe Text)
ifSriovNetSupport f x =
    f (_ifSriovNetSupport x) <&> \y -> x { _ifSriovNetSupport = y }
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
ibdmDeviceName :: Lens' InstanceBlockDeviceMapping (Maybe Text)
ibdmDeviceName f x =
    f (_ibdmDeviceName x) <&> \y -> x { _ibdmDeviceName = y }
{-# INLINE ibdmDeviceName #-}

-- | Parameters used to automatically set up Amazon EBS volumes when the
-- instance is launched.
ibdmEbs :: Lens' InstanceBlockDeviceMapping (Maybe EbsInstanceBlockDevice)
ibdmEbs f x =
    f (_ibdmEbs x) <&> \y -> x { _ibdmEbs = y }
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
ibdmsDeviceName :: Lens' InstanceBlockDeviceMappingSpecification (Maybe Text)
ibdmsDeviceName f x =
    f (_ibdmsDeviceName x) <&> \y -> x { _ibdmsDeviceName = y }
{-# INLINE ibdmsDeviceName #-}

-- | Parameters used to automatically set up Amazon EBS volumes when the
-- instance is launched.
ibdmsEbs :: Lens' InstanceBlockDeviceMappingSpecification (Maybe EbsInstanceBlockDeviceSpecification)
ibdmsEbs f x =
    f (_ibdmsEbs x) <&> \y -> x { _ibdmsEbs = y }
{-# INLINE ibdmsEbs #-}

-- | The virtual device name.
ibdmsVirtualName :: Lens' InstanceBlockDeviceMappingSpecification (Maybe Text)
ibdmsVirtualName f x =
    f (_ibdmsVirtualName x) <&> \y -> x { _ibdmsVirtualName = y }
{-# INLINE ibdmsVirtualName #-}

-- | suppress the specified device included in the block device mapping.
ibdmsNoDevice :: Lens' InstanceBlockDeviceMappingSpecification (Maybe Text)
ibdmsNoDevice f x =
    f (_ibdmsNoDevice x) <&> \y -> x { _ibdmsNoDevice = y }
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
icState :: Lens' InstanceCount (Maybe ListingState)
icState f x =
    f (_icState x) <&> \y -> x { _icState = y }
{-# INLINE icState #-}

-- | he number of listed Reserved Instances in the state specified by the state.
icInstanceCount :: Lens' InstanceCount (Maybe Integer)
icInstanceCount f x =
    f (_icInstanceCount x) <&> \y -> x { _icInstanceCount = y }
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
iedInstanceId :: Lens' InstanceExportDetails (Maybe Text)
iedInstanceId f x =
    f (_iedInstanceId x) <&> \y -> x { _iedInstanceId = y }
{-# INLINE iedInstanceId #-}

-- | The target virtualization environment.
iedTargetEnvironment :: Lens' InstanceExportDetails (Maybe ExportEnvironment)
iedTargetEnvironment f x =
    f (_iedTargetEnvironment x) <&> \y -> x { _iedTargetEnvironment = y }
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
inInstanceId :: Lens' InstanceMonitoring (Maybe Text)
inInstanceId f x =
    f (_inInstanceId x) <&> \y -> x { _inInstanceId = y }
{-# INLINE inInstanceId #-}

-- | The monitoring information.
inMonitoring :: Lens' InstanceMonitoring (Maybe Monitoring)
inMonitoring f x =
    f (_inMonitoring x) <&> \y -> x { _inMonitoring = y }
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
iniNetworkInterfaceId f x =
    f (_iniNetworkInterfaceId x) <&> \y -> x { _iniNetworkInterfaceId = y }
{-# INLINE iniNetworkInterfaceId #-}

-- | The ID of the subnet.
iniSubnetId :: Lens' InstanceNetworkInterface (Maybe Text)
iniSubnetId f x =
    f (_iniSubnetId x) <&> \y -> x { _iniSubnetId = y }
{-# INLINE iniSubnetId #-}

-- | The ID of the VPC.
iniVpcId :: Lens' InstanceNetworkInterface (Maybe Text)
iniVpcId f x =
    f (_iniVpcId x) <&> \y -> x { _iniVpcId = y }
{-# INLINE iniVpcId #-}

-- | The description.
iniDescription :: Lens' InstanceNetworkInterface (Maybe Text)
iniDescription f x =
    f (_iniDescription x) <&> \y -> x { _iniDescription = y }
{-# INLINE iniDescription #-}

-- | The ID of the AWS account that created the network interface.
iniOwnerId :: Lens' InstanceNetworkInterface (Maybe Text)
iniOwnerId f x =
    f (_iniOwnerId x) <&> \y -> x { _iniOwnerId = y }
{-# INLINE iniOwnerId #-}

-- | The status of the network interface.
iniStatus :: Lens' InstanceNetworkInterface (Maybe NetworkInterfaceStatus)
iniStatus f x =
    f (_iniStatus x) <&> \y -> x { _iniStatus = y }
{-# INLINE iniStatus #-}

-- | The IP address of the network interface within the subnet.
iniPrivateIpAddress :: Lens' InstanceNetworkInterface (Maybe Text)
iniPrivateIpAddress f x =
    f (_iniPrivateIpAddress x) <&> \y -> x { _iniPrivateIpAddress = y }
{-# INLINE iniPrivateIpAddress #-}

-- | The private DNS name.
iniPrivateDnsName :: Lens' InstanceNetworkInterface (Maybe Text)
iniPrivateDnsName f x =
    f (_iniPrivateDnsName x) <&> \y -> x { _iniPrivateDnsName = y }
{-# INLINE iniPrivateDnsName #-}

-- | Indicates whether to validate network traffic to or from this network
-- interface.
iniSourceDestCheck :: Lens' InstanceNetworkInterface (Maybe Bool)
iniSourceDestCheck f x =
    f (_iniSourceDestCheck x) <&> \y -> x { _iniSourceDestCheck = y }
{-# INLINE iniSourceDestCheck #-}

-- | One or more security groups.
iniGroups :: Lens' InstanceNetworkInterface [GroupIdentifier]
iniGroups f x =
    f (_iniGroups x) <&> \y -> x { _iniGroups = y }
{-# INLINE iniGroups #-}

-- | The network interface attachment.
iniAttachment :: Lens' InstanceNetworkInterface (Maybe InstanceNetworkInterfaceAttachment)
iniAttachment f x =
    f (_iniAttachment x) <&> \y -> x { _iniAttachment = y }
{-# INLINE iniAttachment #-}

-- | The association information for an Elastic IP associated with the network
-- interface.
iniAssociation :: Lens' InstanceNetworkInterface (Maybe InstanceNetworkInterfaceAssociation)
iniAssociation f x =
    f (_iniAssociation x) <&> \y -> x { _iniAssociation = y }
{-# INLINE iniAssociation #-}

-- | The private IP addresses associated with the network interface.
iniPrivateIpAddresses :: Lens' InstanceNetworkInterface [InstancePrivateIpAddress]
iniPrivateIpAddresses f x =
    f (_iniPrivateIpAddresses x) <&> \y -> x { _iniPrivateIpAddresses = y }
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
inibPublicIp :: Lens' InstanceNetworkInterfaceAssociation (Maybe Text)
inibPublicIp f x =
    f (_inibPublicIp x) <&> \y -> x { _inibPublicIp = y }
{-# INLINE inibPublicIp #-}

-- | The public DNS name.
inibPublicDnsName :: Lens' InstanceNetworkInterfaceAssociation (Maybe Text)
inibPublicDnsName f x =
    f (_inibPublicDnsName x) <&> \y -> x { _inibPublicDnsName = y }
{-# INLINE inibPublicDnsName #-}

-- | The ID of the owner of the Elastic IP address.
inibIpOwnerId :: Lens' InstanceNetworkInterfaceAssociation (Maybe Text)
inibIpOwnerId f x =
    f (_inibIpOwnerId x) <&> \y -> x { _inibIpOwnerId = y }
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
iniaAttachmentId :: Lens' InstanceNetworkInterfaceAttachment (Maybe Text)
iniaAttachmentId f x =
    f (_iniaAttachmentId x) <&> \y -> x { _iniaAttachmentId = y }
{-# INLINE iniaAttachmentId #-}

-- | The index of the device on the instance for the network interface
-- attachment.
iniaDeviceIndex :: Lens' InstanceNetworkInterfaceAttachment (Maybe Integer)
iniaDeviceIndex f x =
    f (_iniaDeviceIndex x) <&> \y -> x { _iniaDeviceIndex = y }
{-# INLINE iniaDeviceIndex #-}

-- | The attachment state.
iniaStatus :: Lens' InstanceNetworkInterfaceAttachment (Maybe AttachmentStatus)
iniaStatus f x =
    f (_iniaStatus x) <&> \y -> x { _iniaStatus = y }
{-# INLINE iniaStatus #-}

-- | The time stamp when the attachment initiated.
iniaAttachTime :: Lens' InstanceNetworkInterfaceAttachment (Maybe ISO8601)
iniaAttachTime f x =
    f (_iniaAttachTime x) <&> \y -> x { _iniaAttachTime = y }
{-# INLINE iniaAttachTime #-}

-- | Indicates whether the network interface is deleted when the instance is
-- terminated.
iniaDeleteOnTermination :: Lens' InstanceNetworkInterfaceAttachment (Maybe Bool)
iniaDeleteOnTermination f x =
    f (_iniaDeleteOnTermination x) <&> \y -> x { _iniaDeleteOnTermination = y }
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
inisNetworkInterfaceId :: Lens' InstanceNetworkInterfaceSpecification (Maybe Text)
inisNetworkInterfaceId f x =
    f (_inisNetworkInterfaceId x) <&> \y -> x { _inisNetworkInterfaceId = y }
{-# INLINE inisNetworkInterfaceId #-}

-- | The index of the device on the instance for the network interface
-- attachment.
inisDeviceIndex :: Lens' InstanceNetworkInterfaceSpecification (Maybe Integer)
inisDeviceIndex f x =
    f (_inisDeviceIndex x) <&> \y -> x { _inisDeviceIndex = y }
{-# INLINE inisDeviceIndex #-}

-- | The ID of the subnet associated with the network string.
inisSubnetId :: Lens' InstanceNetworkInterfaceSpecification (Maybe Text)
inisSubnetId f x =
    f (_inisSubnetId x) <&> \y -> x { _inisSubnetId = y }
{-# INLINE inisSubnetId #-}

-- | The description of the network interface.
inisDescription :: Lens' InstanceNetworkInterfaceSpecification (Maybe Text)
inisDescription f x =
    f (_inisDescription x) <&> \y -> x { _inisDescription = y }
{-# INLINE inisDescription #-}

-- | The private IP address of the network interface.
inisPrivateIpAddress :: Lens' InstanceNetworkInterfaceSpecification (Maybe Text)
inisPrivateIpAddress f x =
    f (_inisPrivateIpAddress x) <&> \y -> x { _inisPrivateIpAddress = y }
{-# INLINE inisPrivateIpAddress #-}

-- | The IDs of the security groups for the network interface.
inisGroups :: Lens' InstanceNetworkInterfaceSpecification [Text]
inisGroups f x =
    f (_inisGroups x) <&> \y -> x { _inisGroups = y }
{-# INLINE inisGroups #-}

-- | If set to true, the interface is deleted when the instance is terminated.
inisDeleteOnTermination :: Lens' InstanceNetworkInterfaceSpecification (Maybe Bool)
inisDeleteOnTermination f x =
    f (_inisDeleteOnTermination x) <&> \y -> x { _inisDeleteOnTermination = y }
{-# INLINE inisDeleteOnTermination #-}

-- | One or more private IP addresses to assign to the network interface.
inisPrivateIpAddresses :: Lens' InstanceNetworkInterfaceSpecification [PrivateIpAddressSpecification]
inisPrivateIpAddresses f x =
    f (_inisPrivateIpAddresses x) <&> \y -> x { _inisPrivateIpAddresses = y }
{-# INLINE inisPrivateIpAddresses #-}

-- | The number of secondary private IP addresses.
inisSecondaryPrivateIpAddressCount :: Lens' InstanceNetworkInterfaceSpecification (Maybe Integer)
inisSecondaryPrivateIpAddressCount f x =
    f (_inisSecondaryPrivateIpAddressCount x) <&> \y -> x { _inisSecondaryPrivateIpAddressCount = y }
{-# INLINE inisSecondaryPrivateIpAddressCount #-}

-- | Indicates whether to auto-assign a public IP address to an instance in a
-- VPC. This public IP address can be assigned to the network interface for
-- eth0 only when you launch the instance. You must create the network
-- interface instead of using an existing network interface for eth0, and you
-- must not specify more than one network interface.
inisAssociatePublicIpAddress :: Lens' InstanceNetworkInterfaceSpecification (Maybe Bool)
inisAssociatePublicIpAddress f x =
    f (_inisAssociatePublicIpAddress x) <&> \y -> x { _inisAssociatePublicIpAddress = y }
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
ipiaPrivateIpAddress :: Lens' InstancePrivateIpAddress (Maybe Text)
ipiaPrivateIpAddress f x =
    f (_ipiaPrivateIpAddress x) <&> \y -> x { _ipiaPrivateIpAddress = y }
{-# INLINE ipiaPrivateIpAddress #-}

-- | The private DNS name.
ipiaPrivateDnsName :: Lens' InstancePrivateIpAddress (Maybe Text)
ipiaPrivateDnsName f x =
    f (_ipiaPrivateDnsName x) <&> \y -> x { _ipiaPrivateDnsName = y }
{-# INLINE ipiaPrivateDnsName #-}

-- | Indicates whether this IP address is the primary private IP address of the
-- network interface.
ipiaPrimary :: Lens' InstancePrivateIpAddress (Maybe Bool)
ipiaPrimary f x =
    f (_ipiaPrimary x) <&> \y -> x { _ipiaPrimary = y }
{-# INLINE ipiaPrimary #-}

-- | The association information for an Elastic IP address for the network
-- interface.
ipiaAssociation :: Lens' InstancePrivateIpAddress (Maybe InstanceNetworkInterfaceAssociation)
ipiaAssociation f x =
    f (_ipiaAssociation x) <&> \y -> x { _ipiaAssociation = y }
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
iifCode :: Lens' InstanceState Integer
iifCode f x =
    f (_iifCode x) <&> \y -> x { _iifCode = y }
{-# INLINE iifCode #-}

-- | The current state of the instance.
iifName :: Lens' InstanceState InstanceStateName
iifName f x =
    f (_iifName x) <&> \y -> x { _iifName = y }
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
iscInstanceId :: Lens' InstanceStateChange (Maybe Text)
iscInstanceId f x =
    f (_iscInstanceId x) <&> \y -> x { _iscInstanceId = y }
{-# INLINE iscInstanceId #-}

-- | The current state of the instance.
iscCurrentState :: Lens' InstanceStateChange (Maybe InstanceState)
iscCurrentState f x =
    f (_iscCurrentState x) <&> \y -> x { _iscCurrentState = y }
{-# INLINE iscCurrentState #-}

-- | The previous state of the instance.
iscPreviousState :: Lens' InstanceStateChange (Maybe InstanceState)
iscPreviousState f x =
    f (_iscPreviousState x) <&> \y -> x { _iscPreviousState = y }
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
iiiiivInstanceId f x =
    f (_iiiiivInstanceId x) <&> \y -> x { _iiiiivInstanceId = y }
{-# INLINE iiiiivInstanceId #-}

-- | The Availability Zone of the instance.
iiiiivAvailabilityZone :: Lens' InstanceStatus (Maybe Text)
iiiiivAvailabilityZone f x =
    f (_iiiiivAvailabilityZone x) <&> \y -> x { _iiiiivAvailabilityZone = y }
{-# INLINE iiiiivAvailabilityZone #-}

-- | Extra information regarding events associated with the instance.
iiiiivEvents :: Lens' InstanceStatus [InstanceStatusEvent]
iiiiivEvents f x =
    f (_iiiiivEvents x) <&> \y -> x { _iiiiivEvents = y }
{-# INLINE iiiiivEvents #-}

-- | The intended state of the instance. DescribeInstanceStatus requires that an
-- instance be in the running state.
iiiiivInstanceState :: Lens' InstanceStatus (Maybe InstanceState)
iiiiivInstanceState f x =
    f (_iiiiivInstanceState x) <&> \y -> x { _iiiiivInstanceState = y }
{-# INLINE iiiiivInstanceState #-}

-- | Reports impaired functionality that stems from issues related to the
-- systems that support an instance, such as hardware failures and network
-- connectivity problems.
iiiiivSystemStatus :: Lens' InstanceStatus (Maybe InstanceStatusSummary)
iiiiivSystemStatus f x =
    f (_iiiiivSystemStatus x) <&> \y -> x { _iiiiivSystemStatus = y }
{-# INLINE iiiiivSystemStatus #-}

-- | Reports impaired functionality that stems from issues internal to the
-- instance, such as impaired reachability.
iiiiivInstanceStatus :: Lens' InstanceStatus (Maybe InstanceStatusSummary)
iiiiivInstanceStatus f x =
    f (_iiiiivInstanceStatus x) <&> \y -> x { _iiiiivInstanceStatus = y }
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
isdName f x =
    f (_isdName x) <&> \y -> x { _isdName = y }
{-# INLINE isdName #-}

-- | The status.
isdStatus :: Lens' InstanceStatusDetails (Maybe StatusType)
isdStatus f x =
    f (_isdStatus x) <&> \y -> x { _isdStatus = y }
{-# INLINE isdStatus #-}

-- | The time when a status check failed. For an instance that was launched and
-- impaired, this is the time when the instance was launched.
isdImpairedSince :: Lens' InstanceStatusDetails (Maybe ISO8601)
isdImpairedSince f x =
    f (_isdImpairedSince x) <&> \y -> x { _isdImpairedSince = y }
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
iseCode :: Lens' InstanceStatusEvent (Maybe EventCode)
iseCode f x =
    f (_iseCode x) <&> \y -> x { _iseCode = y }
{-# INLINE iseCode #-}

-- | A description of the event.
iseDescription :: Lens' InstanceStatusEvent (Maybe Text)
iseDescription f x =
    f (_iseDescription x) <&> \y -> x { _iseDescription = y }
{-# INLINE iseDescription #-}

-- | The earliest scheduled start time for the event.
iseNotBefore :: Lens' InstanceStatusEvent (Maybe ISO8601)
iseNotBefore f x =
    f (_iseNotBefore x) <&> \y -> x { _iseNotBefore = y }
{-# INLINE iseNotBefore #-}

-- | The latest scheduled end time for the event.
iseNotAfter :: Lens' InstanceStatusEvent (Maybe ISO8601)
iseNotAfter f x =
    f (_iseNotAfter x) <&> \y -> x { _iseNotAfter = y }
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
issStatus :: Lens' InstanceStatusSummary (Maybe SummaryStatus)
issStatus f x =
    f (_issStatus x) <&> \y -> x { _issStatus = y }
{-# INLINE issStatus #-}

-- | The system instance health or application instance health.
issDetails :: Lens' InstanceStatusSummary [InstanceStatusDetails]
issDetails f x =
    f (_issDetails x) <&> \y -> x { _issDetails = y }
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
igInternetGatewayId :: Lens' InternetGateway Text
igInternetGatewayId f x =
    f (_igInternetGatewayId x) <&> \y -> x { _igInternetGatewayId = y }
{-# INLINE igInternetGatewayId #-}

-- | Any VPCs attached to the Internet gateway.
igAttachments :: Lens' InternetGateway [InternetGatewayAttachment]
igAttachments f x =
    f (_igAttachments x) <&> \y -> x { _igAttachments = y }
{-# INLINE igAttachments #-}

-- | Any tags assigned to the Internet gateway.
igTags :: Lens' InternetGateway [Tag]
igTags f x =
    f (_igTags x) <&> \y -> x { _igTags = y }
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
igaVpcId :: Lens' InternetGatewayAttachment Text
igaVpcId f x =
    f (_igaVpcId x) <&> \y -> x { _igaVpcId = y }
{-# INLINE igaVpcId #-}

-- | The current state of the attachment.
igaState :: Lens' InternetGatewayAttachment AttachmentStatus
igaState f x =
    f (_igaState x) <&> \y -> x { _igaState = y }
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
ipIpProtocol :: Lens' IpPermission Text
ipIpProtocol f x =
    f (_ipIpProtocol x) <&> \y -> x { _ipIpProtocol = y }
{-# INLINE ipIpProtocol #-}

-- | The start of port range for the TCP and UDP protocols, or an ICMP type
-- number. A value of -1 indicates all ICMP types.
ipFromPort :: Lens' IpPermission Integer
ipFromPort f x =
    f (_ipFromPort x) <&> \y -> x { _ipFromPort = y }
{-# INLINE ipFromPort #-}

-- | The end of port range for the TCP and UDP protocols, or an ICMP code. A
-- value of -1 indicates all ICMP codes for the specified ICMP type.
ipToPort :: Lens' IpPermission Integer
ipToPort f x =
    f (_ipToPort x) <&> \y -> x { _ipToPort = y }
{-# INLINE ipToPort #-}

-- | One or more security group and AWS account ID pairs.
ipUserIdGroupPairs :: Lens' IpPermission [UserIdGroupPair]
ipUserIdGroupPairs f x =
    f (_ipUserIdGroupPairs x) <&> \y -> x { _ipUserIdGroupPairs = y }
{-# INLINE ipUserIdGroupPairs #-}

-- | One or more IP ranges.
ipIpRanges :: Lens' IpPermission [IpRange]
ipIpRanges f x =
    f (_ipIpRanges x) <&> \y -> x { _ipIpRanges = y }
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
kpiKeyName :: Lens' KeyPairInfo (Maybe Text)
kpiKeyName f x =
    f (_kpiKeyName x) <&> \y -> x { _kpiKeyName = y }
{-# INLINE kpiKeyName #-}

-- | If you used CreateKeyPair to create the key pair, this is the SHA-1 digest
-- of the DER encoded private key. If you used ImportKeyPair to provide AWS
-- the public key, this is the MD5 public key fingerprint as specified in
-- section 4 of RFC4716.
kpiKeyFingerprint :: Lens' KeyPairInfo (Maybe Text)
kpiKeyFingerprint f x =
    f (_kpiKeyFingerprint x) <&> \y -> x { _kpiKeyFingerprint = y }
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
lpUserId f x =
    f (_lpUserId x) <&> \y -> x { _lpUserId = y }
{-# INLINE lpUserId #-}

-- | The name of the group.
lpGroup :: Lens' LaunchPermission (Maybe PermissionGroup)
lpGroup f x =
    f (_lpGroup x) <&> \y -> x { _lpGroup = y }
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
lpmAdd :: Lens' LaunchPermissionModifications [LaunchPermission]
lpmAdd f x =
    f (_lpmAdd x) <&> \y -> x { _lpmAdd = y }
{-# INLINE lpmAdd #-}

-- | The AWS account ID to remove from the list of launch permissions for the
-- AMI.
lpmRemove :: Lens' LaunchPermissionModifications [LaunchPermission]
lpmRemove f x =
    f (_lpmRemove x) <&> \y -> x { _lpmRemove = y }
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
llnImageId :: Lens' LaunchSpecification (Maybe Text)
llnImageId f x =
    f (_llnImageId x) <&> \y -> x { _llnImageId = y }
{-# INLINE llnImageId #-}

-- | The name of the key pair.
llnKeyName :: Lens' LaunchSpecification (Maybe Text)
llnKeyName f x =
    f (_llnKeyName x) <&> \y -> x { _llnKeyName = y }
{-# INLINE llnKeyName #-}

-- | One or more security groups.
llnSecurityGroups :: Lens' LaunchSpecification [GroupIdentifier]
llnSecurityGroups f x =
    f (_llnSecurityGroups x) <&> \y -> x { _llnSecurityGroups = y }
{-# INLINE llnSecurityGroups #-}

-- | The Base64-encoded MIME user data to make available to the instances.
llnUserData :: Lens' LaunchSpecification (Maybe Text)
llnUserData f x =
    f (_llnUserData x) <&> \y -> x { _llnUserData = y }
{-# INLINE llnUserData #-}

-- | 
llnAddressingType :: Lens' LaunchSpecification (Maybe Text)
llnAddressingType f x =
    f (_llnAddressingType x) <&> \y -> x { _llnAddressingType = y }
{-# INLINE llnAddressingType #-}

-- | The instance type.
llnInstanceType :: Lens' LaunchSpecification (Maybe InstanceType)
llnInstanceType f x =
    f (_llnInstanceType x) <&> \y -> x { _llnInstanceType = y }
{-# INLINE llnInstanceType #-}

-- | The placement information for the instance.
llnPlacement :: Lens' LaunchSpecification (Maybe SpotPlacement)
llnPlacement f x =
    f (_llnPlacement x) <&> \y -> x { _llnPlacement = y }
{-# INLINE llnPlacement #-}

-- | The ID of the kernel.
llnKernelId :: Lens' LaunchSpecification (Maybe Text)
llnKernelId f x =
    f (_llnKernelId x) <&> \y -> x { _llnKernelId = y }
{-# INLINE llnKernelId #-}

-- | The ID of the RAM disk.
llnRamdiskId :: Lens' LaunchSpecification (Maybe Text)
llnRamdiskId f x =
    f (_llnRamdiskId x) <&> \y -> x { _llnRamdiskId = y }
{-# INLINE llnRamdiskId #-}

-- | One or more block device mapping entries.
llnBlockDeviceMappings :: Lens' LaunchSpecification [BlockDeviceMapping]
llnBlockDeviceMappings f x =
    f (_llnBlockDeviceMappings x) <&> \y -> x { _llnBlockDeviceMappings = y }
{-# INLINE llnBlockDeviceMappings #-}

-- | Enables monitoring for the instance. Default: Disabled.
llnMonitoringEnabled :: Lens' LaunchSpecification (Maybe Bool)
llnMonitoringEnabled f x =
    f (_llnMonitoringEnabled x) <&> \y -> x { _llnMonitoringEnabled = y }
{-# INLINE llnMonitoringEnabled #-}

-- | The ID of the subnet in which to launch the Spot Instance.
llnSubnetId :: Lens' LaunchSpecification (Maybe Text)
llnSubnetId f x =
    f (_llnSubnetId x) <&> \y -> x { _llnSubnetId = y }
{-# INLINE llnSubnetId #-}

-- | One or more network interfaces.
llnNetworkInterfaces :: Lens' LaunchSpecification [InstanceNetworkInterfaceSpecification]
llnNetworkInterfaces f x =
    f (_llnNetworkInterfaces x) <&> \y -> x { _llnNetworkInterfaces = y }
{-# INLINE llnNetworkInterfaces #-}

-- | The IAM instance profile.
llnIamInstanceProfile :: Lens' LaunchSpecification (Maybe IamInstanceProfileSpecification)
llnIamInstanceProfile f x =
    f (_llnIamInstanceProfile x) <&> \y -> x { _llnIamInstanceProfile = y }
{-# INLINE llnIamInstanceProfile #-}

-- | Indicates whether the instance is optimized for EBS I/O. This optimization
-- provides dedicated throughput to Amazon EBS and an optimized configuration
-- stack to provide optimal EBS I/O performance. This optimization isn't
-- available with all instance types. Additional usage charges apply when
-- using an EBS Optimized instance. Default: false.
llnEbsOptimized :: Lens' LaunchSpecification (Maybe Bool)
llnEbsOptimized f x =
    f (_llnEbsOptimized x) <&> \y -> x { _llnEbsOptimized = y }
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
naNetworkAclId :: Lens' NetworkAcl (Maybe Text)
naNetworkAclId f x =
    f (_naNetworkAclId x) <&> \y -> x { _naNetworkAclId = y }
{-# INLINE naNetworkAclId #-}

-- | The ID of the VPC for the network ACL.
naVpcId :: Lens' NetworkAcl (Maybe Text)
naVpcId f x =
    f (_naVpcId x) <&> \y -> x { _naVpcId = y }
{-# INLINE naVpcId #-}

-- | Indicates whether this is the default network ACL for the VPC.
naIsDefault :: Lens' NetworkAcl (Maybe Bool)
naIsDefault f x =
    f (_naIsDefault x) <&> \y -> x { _naIsDefault = y }
{-# INLINE naIsDefault #-}

-- | One or more entries (rules) in the network ACL.
naEntries :: Lens' NetworkAcl [NetworkAclEntry]
naEntries f x =
    f (_naEntries x) <&> \y -> x { _naEntries = y }
{-# INLINE naEntries #-}

-- | Any associations between the network ACL and one or more subnets.
naAssociations :: Lens' NetworkAcl [NetworkAclAssociation]
naAssociations f x =
    f (_naAssociations x) <&> \y -> x { _naAssociations = y }
{-# INLINE naAssociations #-}

-- | Any tags assigned to the network ACL.
naTags :: Lens' NetworkAcl [Tag]
naTags f x =
    f (_naTags x) <&> \y -> x { _naTags = y }
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
naaNetworkAclAssociationId f x =
    f (_naaNetworkAclAssociationId x) <&> \y -> x { _naaNetworkAclAssociationId = y }
{-# INLINE naaNetworkAclAssociationId #-}

-- | The ID of the network ACL.
naaNetworkAclId :: Lens' NetworkAclAssociation (Maybe Text)
naaNetworkAclId f x =
    f (_naaNetworkAclId x) <&> \y -> x { _naaNetworkAclId = y }
{-# INLINE naaNetworkAclId #-}

-- | The ID of the subnet.
naaSubnetId :: Lens' NetworkAclAssociation (Maybe Text)
naaSubnetId f x =
    f (_naaSubnetId x) <&> \y -> x { _naaSubnetId = y }
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
naeRuleNumber :: Lens' NetworkAclEntry (Maybe Integer)
naeRuleNumber f x =
    f (_naeRuleNumber x) <&> \y -> x { _naeRuleNumber = y }
{-# INLINE naeRuleNumber #-}

-- | The protocol. A value of -1 means all protocols.
naeProtocol :: Lens' NetworkAclEntry (Maybe Text)
naeProtocol f x =
    f (_naeProtocol x) <&> \y -> x { _naeProtocol = y }
{-# INLINE naeProtocol #-}

-- | Indicates whether to allow or deny the traffic that matches the rule.
naeRuleAction :: Lens' NetworkAclEntry (Maybe RuleAction)
naeRuleAction f x =
    f (_naeRuleAction x) <&> \y -> x { _naeRuleAction = y }
{-# INLINE naeRuleAction #-}

-- | Indicates whether the rule is an egress rule (applied to traffic leaving
-- the subnet).
naeEgress :: Lens' NetworkAclEntry (Maybe Bool)
naeEgress f x =
    f (_naeEgress x) <&> \y -> x { _naeEgress = y }
{-# INLINE naeEgress #-}

-- | The network range to allow or deny, in CIDR notation.
naeCidrBlock :: Lens' NetworkAclEntry (Maybe Text)
naeCidrBlock f x =
    f (_naeCidrBlock x) <&> \y -> x { _naeCidrBlock = y }
{-# INLINE naeCidrBlock #-}

-- | ICMP protocol: The ICMP type and code.
naeIcmpTypeCode :: Lens' NetworkAclEntry (Maybe IcmpTypeCode)
naeIcmpTypeCode f x =
    f (_naeIcmpTypeCode x) <&> \y -> x { _naeIcmpTypeCode = y }
{-# INLINE naeIcmpTypeCode #-}

-- | TCP or UDP protocols: The range of ports the rule applies to.
naePortRange :: Lens' NetworkAclEntry (Maybe PortRange)
naePortRange f x =
    f (_naePortRange x) <&> \y -> x { _naePortRange = y }
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
niNetworkInterfaceId :: Lens' NetworkInterface (Maybe Text)
niNetworkInterfaceId f x =
    f (_niNetworkInterfaceId x) <&> \y -> x { _niNetworkInterfaceId = y }
{-# INLINE niNetworkInterfaceId #-}

-- | The ID of the subnet.
niSubnetId :: Lens' NetworkInterface (Maybe Text)
niSubnetId f x =
    f (_niSubnetId x) <&> \y -> x { _niSubnetId = y }
{-# INLINE niSubnetId #-}

-- | The ID of the VPC.
niVpcId :: Lens' NetworkInterface (Maybe Text)
niVpcId f x =
    f (_niVpcId x) <&> \y -> x { _niVpcId = y }
{-# INLINE niVpcId #-}

-- | The Availability Zone.
niAvailabilityZone :: Lens' NetworkInterface (Maybe Text)
niAvailabilityZone f x =
    f (_niAvailabilityZone x) <&> \y -> x { _niAvailabilityZone = y }
{-# INLINE niAvailabilityZone #-}

-- | A description.
niDescription :: Lens' NetworkInterface (Maybe Text)
niDescription f x =
    f (_niDescription x) <&> \y -> x { _niDescription = y }
{-# INLINE niDescription #-}

-- | The AWS account ID of the owner of the network interface.
niOwnerId :: Lens' NetworkInterface (Maybe Text)
niOwnerId f x =
    f (_niOwnerId x) <&> \y -> x { _niOwnerId = y }
{-# INLINE niOwnerId #-}

-- | The ID of the entity that launched the instance on your behalf (for
-- example, AWS Management Console or Auto Scaling).
niRequesterId :: Lens' NetworkInterface (Maybe Text)
niRequesterId f x =
    f (_niRequesterId x) <&> \y -> x { _niRequesterId = y }
{-# INLINE niRequesterId #-}

-- | Indicates whether the network interface is being managed by AWS.
niRequesterManaged :: Lens' NetworkInterface (Maybe Bool)
niRequesterManaged f x =
    f (_niRequesterManaged x) <&> \y -> x { _niRequesterManaged = y }
{-# INLINE niRequesterManaged #-}

-- | The status of the network interface.
niStatus :: Lens' NetworkInterface (Maybe NetworkInterfaceStatus)
niStatus f x =
    f (_niStatus x) <&> \y -> x { _niStatus = y }
{-# INLINE niStatus #-}

-- | The MAC address.
niMacAddress :: Lens' NetworkInterface (Maybe Text)
niMacAddress f x =
    f (_niMacAddress x) <&> \y -> x { _niMacAddress = y }
{-# INLINE niMacAddress #-}

-- | The IP address of the network interface within the subnet.
niPrivateIpAddress :: Lens' NetworkInterface (Maybe Text)
niPrivateIpAddress f x =
    f (_niPrivateIpAddress x) <&> \y -> x { _niPrivateIpAddress = y }
{-# INLINE niPrivateIpAddress #-}

-- | The private DNS name.
niPrivateDnsName :: Lens' NetworkInterface (Maybe Text)
niPrivateDnsName f x =
    f (_niPrivateDnsName x) <&> \y -> x { _niPrivateDnsName = y }
{-# INLINE niPrivateDnsName #-}

-- | Indicates whether traffic to or from the instance is validated.
niSourceDestCheck :: Lens' NetworkInterface (Maybe Bool)
niSourceDestCheck f x =
    f (_niSourceDestCheck x) <&> \y -> x { _niSourceDestCheck = y }
{-# INLINE niSourceDestCheck #-}

-- | Any security groups for the network interface.
niGroups :: Lens' NetworkInterface [GroupIdentifier]
niGroups f x =
    f (_niGroups x) <&> \y -> x { _niGroups = y }
{-# INLINE niGroups #-}

-- | The network interface attachment.
niAttachment :: Lens' NetworkInterface (Maybe NetworkInterfaceAttachment)
niAttachment f x =
    f (_niAttachment x) <&> \y -> x { _niAttachment = y }
{-# INLINE niAttachment #-}

-- | The association information for an Elastic IP associated with the network
-- interface.
niAssociation :: Lens' NetworkInterface (Maybe NetworkInterfaceAssociation)
niAssociation f x =
    f (_niAssociation x) <&> \y -> x { _niAssociation = y }
{-# INLINE niAssociation #-}

-- | Any tags assigned to the network interface.
niTagSet :: Lens' NetworkInterface [Tag]
niTagSet f x =
    f (_niTagSet x) <&> \y -> x { _niTagSet = y }
{-# INLINE niTagSet #-}

-- | The private IP addresses associated with the network interface.
niPrivateIpAddresses :: Lens' NetworkInterface [NetworkInterfacePrivateIpAddress]
niPrivateIpAddresses f x =
    f (_niPrivateIpAddresses x) <&> \y -> x { _niPrivateIpAddresses = y }
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
nibPublicIp f x =
    f (_nibPublicIp x) <&> \y -> x { _nibPublicIp = y }
{-# INLINE nibPublicIp #-}

-- | The public DNS name.
nibPublicDnsName :: Lens' NetworkInterfaceAssociation (Maybe Text)
nibPublicDnsName f x =
    f (_nibPublicDnsName x) <&> \y -> x { _nibPublicDnsName = y }
{-# INLINE nibPublicDnsName #-}

-- | The ID of the Elastic IP address owner.
nibIpOwnerId :: Lens' NetworkInterfaceAssociation (Maybe Text)
nibIpOwnerId f x =
    f (_nibIpOwnerId x) <&> \y -> x { _nibIpOwnerId = y }
{-# INLINE nibIpOwnerId #-}

-- | The allocation ID.
nibAllocationId :: Lens' NetworkInterfaceAssociation (Maybe Text)
nibAllocationId f x =
    f (_nibAllocationId x) <&> \y -> x { _nibAllocationId = y }
{-# INLINE nibAllocationId #-}

-- | The association ID.
nibAssociationId :: Lens' NetworkInterfaceAssociation (Maybe Text)
nibAssociationId f x =
    f (_nibAssociationId x) <&> \y -> x { _nibAssociationId = y }
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
niaAttachmentId :: Lens' NetworkInterfaceAttachment (Maybe Text)
niaAttachmentId f x =
    f (_niaAttachmentId x) <&> \y -> x { _niaAttachmentId = y }
{-# INLINE niaAttachmentId #-}

-- | The ID of the instance.
niaInstanceId :: Lens' NetworkInterfaceAttachment (Maybe Text)
niaInstanceId f x =
    f (_niaInstanceId x) <&> \y -> x { _niaInstanceId = y }
{-# INLINE niaInstanceId #-}

-- | The AWS account ID of the owner of the instance.
niaInstanceOwnerId :: Lens' NetworkInterfaceAttachment (Maybe Text)
niaInstanceOwnerId f x =
    f (_niaInstanceOwnerId x) <&> \y -> x { _niaInstanceOwnerId = y }
{-# INLINE niaInstanceOwnerId #-}

-- | The device index of the network interface attachment on the instance.
niaDeviceIndex :: Lens' NetworkInterfaceAttachment (Maybe Integer)
niaDeviceIndex f x =
    f (_niaDeviceIndex x) <&> \y -> x { _niaDeviceIndex = y }
{-# INLINE niaDeviceIndex #-}

-- | The attachment state.
niaStatus :: Lens' NetworkInterfaceAttachment (Maybe AttachmentStatus)
niaStatus f x =
    f (_niaStatus x) <&> \y -> x { _niaStatus = y }
{-# INLINE niaStatus #-}

-- | The timestamp indicating when the attachment initiated.
niaAttachTime :: Lens' NetworkInterfaceAttachment (Maybe ISO8601)
niaAttachTime f x =
    f (_niaAttachTime x) <&> \y -> x { _niaAttachTime = y }
{-# INLINE niaAttachTime #-}

-- | Indicates whether the network interface is deleted when the instance is
-- terminated.
niaDeleteOnTermination :: Lens' NetworkInterfaceAttachment (Maybe Bool)
niaDeleteOnTermination f x =
    f (_niaDeleteOnTermination x) <&> \y -> x { _niaDeleteOnTermination = y }
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
niacAttachmentId :: Lens' NetworkInterfaceAttachmentChanges (Maybe Text)
niacAttachmentId f x =
    f (_niacAttachmentId x) <&> \y -> x { _niacAttachmentId = y }
{-# INLINE niacAttachmentId #-}

-- | Indicates whether the network interface is deleted when the instance is
-- terminated.
niacDeleteOnTermination :: Lens' NetworkInterfaceAttachmentChanges (Maybe Bool)
niacDeleteOnTermination f x =
    f (_niacDeleteOnTermination x) <&> \y -> x { _niacDeleteOnTermination = y }
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
nipiaPrivateIpAddress :: Lens' NetworkInterfacePrivateIpAddress (Maybe Text)
nipiaPrivateIpAddress f x =
    f (_nipiaPrivateIpAddress x) <&> \y -> x { _nipiaPrivateIpAddress = y }
{-# INLINE nipiaPrivateIpAddress #-}

-- | The private DNS name.
nipiaPrivateDnsName :: Lens' NetworkInterfacePrivateIpAddress (Maybe Text)
nipiaPrivateDnsName f x =
    f (_nipiaPrivateDnsName x) <&> \y -> x { _nipiaPrivateDnsName = y }
{-# INLINE nipiaPrivateDnsName #-}

-- | Indicates whether this IP address is the primary private IP address of the
-- network interface.
nipiaPrimary :: Lens' NetworkInterfacePrivateIpAddress (Maybe Bool)
nipiaPrimary f x =
    f (_nipiaPrimary x) <&> \y -> x { _nipiaPrimary = y }
{-# INLINE nipiaPrimary #-}

-- | The association information for an Elastic IP address associated with the
-- network interface.
nipiaAssociation :: Lens' NetworkInterfacePrivateIpAddress (Maybe NetworkInterfaceAssociation)
nipiaAssociation f x =
    f (_nipiaAssociation x) <&> \y -> x { _nipiaAssociation = y }
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
pzAvailabilityZone :: Lens' Placement (Maybe Text)
pzAvailabilityZone f x =
    f (_pzAvailabilityZone x) <&> \y -> x { _pzAvailabilityZone = y }
{-# INLINE pzAvailabilityZone #-}

-- | The name of the placement group the instance is in (for cluster compute
-- instances).
pzGroupName :: Lens' Placement (Maybe Text)
pzGroupName f x =
    f (_pzGroupName x) <&> \y -> x { _pzGroupName = y }
{-# INLINE pzGroupName #-}

-- | The tenancy of the instance (if the instance is running in a VPC). An
-- instance with a tenancy of dedicated runs on single-tenant hardware.
pzTenancy :: Lens' Placement (Maybe Tenancy)
pzTenancy f x =
    f (_pzTenancy x) <&> \y -> x { _pzTenancy = y }
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
phGroupName :: Lens' PlacementGroup (Maybe Text)
phGroupName f x =
    f (_phGroupName x) <&> \y -> x { _phGroupName = y }
{-# INLINE phGroupName #-}

-- | The placement strategy.
phStrategy :: Lens' PlacementGroup (Maybe PlacementStrategy)
phStrategy f x =
    f (_phStrategy x) <&> \y -> x { _phStrategy = y }
{-# INLINE phStrategy #-}

-- | The state of the placement group.
phState :: Lens' PlacementGroup (Maybe PlacementGroupState)
phState f x =
    f (_phState x) <&> \y -> x { _phState = y }
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
prFrom f x =
    f (_prFrom x) <&> \y -> x { _prFrom = y }
{-# INLINE prFrom #-}

-- | The last port in the range.
prTo :: Lens' PortRange (Maybe Integer)
prTo f x =
    f (_prTo x) <&> \y -> x { _prTo = y }
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
psTerm :: Lens' PriceSchedule (Maybe Integer)
psTerm f x =
    f (_psTerm x) <&> \y -> x { _psTerm = y }
{-# INLINE psTerm #-}

-- | The fixed price for the term.
psPrice :: Lens' PriceSchedule (Maybe Double)
psPrice f x =
    f (_psPrice x) <&> \y -> x { _psPrice = y }
{-# INLINE psPrice #-}

-- | The currency for transacting the Reserved Instance resale. At this time,
-- the only supported currency is USD.
psCurrencyCode :: Lens' PriceSchedule (Maybe CurrencyCodeValues)
psCurrencyCode f x =
    f (_psCurrencyCode x) <&> \y -> x { _psCurrencyCode = y }
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
psActive f x =
    f (_psActive x) <&> \y -> x { _psActive = y }
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
pssTerm :: Lens' PriceScheduleSpecification (Maybe Integer)
pssTerm f x =
    f (_pssTerm x) <&> \y -> x { _pssTerm = y }
{-# INLINE pssTerm #-}

-- | The fixed price for the term.
pssPrice :: Lens' PriceScheduleSpecification (Maybe Double)
pssPrice f x =
    f (_pssPrice x) <&> \y -> x { _pssPrice = y }
{-# INLINE pssPrice #-}

-- | The currency for transacting the Reserved Instance resale. At this time,
-- the only supported currency is USD.
pssCurrencyCode :: Lens' PriceScheduleSpecification (Maybe CurrencyCodeValues)
pssCurrencyCode f x =
    f (_pssCurrencyCode x) <&> \y -> x { _pssCurrencyCode = y }
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
piPrice :: Lens' PricingDetail (Maybe Double)
piPrice f x =
    f (_piPrice x) <&> \y -> x { _piPrice = y }
{-# INLINE piPrice #-}

-- | The number of instances available for the price.
piCount :: Lens' PricingDetail (Maybe Integer)
piCount f x =
    f (_piCount x) <&> \y -> x { _piCount = y }
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
piasPrivateIpAddress :: Lens' PrivateIpAddressSpecification Text
piasPrivateIpAddress f x =
    f (_piasPrivateIpAddress x) <&> \y -> x { _piasPrivateIpAddress = y }
{-# INLINE piasPrivateIpAddress #-}

-- | Indicates whether the private IP address is the primary private IP address.
piasPrimary :: Lens' PrivateIpAddressSpecification (Maybe Bool)
piasPrimary f x =
    f (_piasPrimary x) <&> \y -> x { _piasPrimary = y }
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
pcProductCodeId :: Lens' ProductCode (Maybe Text)
pcProductCodeId f x =
    f (_pcProductCodeId x) <&> \y -> x { _pcProductCodeId = y }
{-# INLINE pcProductCodeId #-}

-- | The type of product code.
pcProductCodeType :: Lens' ProductCode (Maybe ProductCodeValues)
pcProductCodeType f x =
    f (_pcProductCodeType x) <&> \y -> x { _pcProductCodeType = y }
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
rdFrequency :: Lens' RecurringCharge (Maybe RecurringChargeFrequency)
rdFrequency f x =
    f (_rdFrequency x) <&> \y -> x { _rdFrequency = y }
{-# INLINE rdFrequency #-}

-- | The amount of the recurring charge.
rdAmount :: Lens' RecurringCharge (Maybe Double)
rdAmount f x =
    f (_rdAmount x) <&> \y -> x { _rdAmount = y }
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
rqRegionName :: Lens' Region (Maybe Text)
rqRegionName f x =
    f (_rqRegionName x) <&> \y -> x { _rqRegionName = y }
{-# INLINE rqRegionName #-}

-- | The region service endpoint.
rqEndpoint :: Lens' Region (Maybe Text)
rqEndpoint f x =
    f (_rqEndpoint x) <&> \y -> x { _rqEndpoint = y }
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
rnReservationId f x =
    f (_rnReservationId x) <&> \y -> x { _rnReservationId = y }
{-# INLINE rnReservationId #-}

-- | The ID of the AWS account that owns the reservation.
rnOwnerId :: Lens' Reservation (Maybe Text)
rnOwnerId f x =
    f (_rnOwnerId x) <&> \y -> x { _rnOwnerId = y }
{-# INLINE rnOwnerId #-}

-- | The ID of the requester that launched the instances on your behalf (for
-- example, AWS Management Console or Auto Scaling).
rnRequesterId :: Lens' Reservation (Maybe Text)
rnRequesterId f x =
    f (_rnRequesterId x) <&> \y -> x { _rnRequesterId = y }
{-# INLINE rnRequesterId #-}

-- | One or more security groups.
rnGroups :: Lens' Reservation [GroupIdentifier]
rnGroups f x =
    f (_rnGroups x) <&> \y -> x { _rnGroups = y }
{-# INLINE rnGroups #-}

-- | One or more instances.
rnInstances :: Lens' Reservation [Instance]
rnInstances f x =
    f (_rnInstances x) <&> \y -> x { _rnInstances = y }
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
rilpAmount f x =
    f (_rilpAmount x) <&> \y -> x { _rilpAmount = y }
{-# INLINE rilpAmount #-}

-- | The currency in which the limitPrice amount is specified. At this time, the
-- only supported currency is USD.
rilpCurrencyCode :: Lens' ReservedInstanceLimitPrice (Maybe CurrencyCodeValues)
rilpCurrencyCode f x =
    f (_rilpCurrencyCode x) <&> \y -> x { _rilpCurrencyCode = y }
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
riReservedInstancesId :: Lens' ReservedInstances (Maybe Text)
riReservedInstancesId f x =
    f (_riReservedInstancesId x) <&> \y -> x { _riReservedInstancesId = y }
{-# INLINE riReservedInstancesId #-}

-- | The instance type on which the Reserved Instance can be used.
riInstanceType :: Lens' ReservedInstances (Maybe InstanceType)
riInstanceType f x =
    f (_riInstanceType x) <&> \y -> x { _riInstanceType = y }
{-# INLINE riInstanceType #-}

-- | The Availability Zone in which the Reserved Instance can be used.
riAvailabilityZone :: Lens' ReservedInstances (Maybe Text)
riAvailabilityZone f x =
    f (_riAvailabilityZone x) <&> \y -> x { _riAvailabilityZone = y }
{-# INLINE riAvailabilityZone #-}

-- | The date and time the Reserved Instance started.
riStart :: Lens' ReservedInstances (Maybe ISO8601)
riStart f x =
    f (_riStart x) <&> \y -> x { _riStart = y }
{-# INLINE riStart #-}

-- | The time when the Reserved Instance expires.
riEnd :: Lens' ReservedInstances (Maybe ISO8601)
riEnd f x =
    f (_riEnd x) <&> \y -> x { _riEnd = y }
{-# INLINE riEnd #-}

-- | The duration of the Reserved Instance, in seconds.
riDuration :: Lens' ReservedInstances (Maybe Integer)
riDuration f x =
    f (_riDuration x) <&> \y -> x { _riDuration = y }
{-# INLINE riDuration #-}

-- | The usage price of the Reserved Instance, per hour.
riUsagePrice :: Lens' ReservedInstances (Maybe Double)
riUsagePrice f x =
    f (_riUsagePrice x) <&> \y -> x { _riUsagePrice = y }
{-# INLINE riUsagePrice #-}

-- | The purchase price of the Reserved Instance.
riFixedPrice :: Lens' ReservedInstances (Maybe Double)
riFixedPrice f x =
    f (_riFixedPrice x) <&> \y -> x { _riFixedPrice = y }
{-# INLINE riFixedPrice #-}

-- | The number of Reserved Instances purchased.
riInstanceCount :: Lens' ReservedInstances (Maybe Integer)
riInstanceCount f x =
    f (_riInstanceCount x) <&> \y -> x { _riInstanceCount = y }
{-# INLINE riInstanceCount #-}

-- | The Reserved Instance description.
riProductDescription :: Lens' ReservedInstances (Maybe RIProductDescription)
riProductDescription f x =
    f (_riProductDescription x) <&> \y -> x { _riProductDescription = y }
{-# INLINE riProductDescription #-}

-- | The state of the Reserved Instance purchase.
riState :: Lens' ReservedInstances (Maybe ReservedInstanceState)
riState f x =
    f (_riState x) <&> \y -> x { _riState = y }
{-# INLINE riState #-}

-- | Any tags assigned to the resource.
riTags :: Lens' ReservedInstances [Tag]
riTags f x =
    f (_riTags x) <&> \y -> x { _riTags = y }
{-# INLINE riTags #-}

-- | The tenancy of the reserved instance.
riInstanceTenancy :: Lens' ReservedInstances (Maybe Tenancy)
riInstanceTenancy f x =
    f (_riInstanceTenancy x) <&> \y -> x { _riInstanceTenancy = y }
{-# INLINE riInstanceTenancy #-}

-- | The currency of the Reserved Instance. It's specified using ISO 4217
-- standard currency codes. At this time, the only supported currency is USD.
riCurrencyCode :: Lens' ReservedInstances (Maybe CurrencyCodeValues)
riCurrencyCode f x =
    f (_riCurrencyCode x) <&> \y -> x { _riCurrencyCode = y }
{-# INLINE riCurrencyCode #-}

-- | The Reserved Instance offering type.
riOfferingType :: Lens' ReservedInstances (Maybe OfferingTypeValues)
riOfferingType f x =
    f (_riOfferingType x) <&> \y -> x { _riOfferingType = y }
{-# INLINE riOfferingType #-}

-- | The recurring charge tag assigned to the resource.
riRecurringCharges :: Lens' ReservedInstances [RecurringCharge]
riRecurringCharges f x =
    f (_riRecurringCharges x) <&> \y -> x { _riRecurringCharges = y }
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
ricAvailabilityZone f x =
    f (_ricAvailabilityZone x) <&> \y -> x { _ricAvailabilityZone = y }
{-# INLINE ricAvailabilityZone #-}

-- | The network platform of the modified Reserved Instances, which is either
-- EC2-Classic or EC2-VPC.
ricPlatform :: Lens' ReservedInstancesConfiguration (Maybe Text)
ricPlatform f x =
    f (_ricPlatform x) <&> \y -> x { _ricPlatform = y }
{-# INLINE ricPlatform #-}

-- | The number of modified Reserved Instances.
ricInstanceCount :: Lens' ReservedInstancesConfiguration (Maybe Integer)
ricInstanceCount f x =
    f (_ricInstanceCount x) <&> \y -> x { _ricInstanceCount = y }
{-# INLINE ricInstanceCount #-}

-- | The instance type for the modified Reserved Instances.
ricInstanceType :: Lens' ReservedInstancesConfiguration (Maybe InstanceType)
ricInstanceType f x =
    f (_ricInstanceType x) <&> \y -> x { _ricInstanceType = y }
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
rilReservedInstancesListingId :: Lens' ReservedInstancesListing (Maybe Text)
rilReservedInstancesListingId f x =
    f (_rilReservedInstancesListingId x) <&> \y -> x { _rilReservedInstancesListingId = y }
{-# INLINE rilReservedInstancesListingId #-}

-- | The ID of the Reserved Instance.
rilReservedInstancesId :: Lens' ReservedInstancesListing (Maybe Text)
rilReservedInstancesId f x =
    f (_rilReservedInstancesId x) <&> \y -> x { _rilReservedInstancesId = y }
{-# INLINE rilReservedInstancesId #-}

-- | The time the listing was created.
rilCreateDate :: Lens' ReservedInstancesListing (Maybe ISO8601)
rilCreateDate f x =
    f (_rilCreateDate x) <&> \y -> x { _rilCreateDate = y }
{-# INLINE rilCreateDate #-}

-- | The last modified timestamp of the listing.
rilUpdateDate :: Lens' ReservedInstancesListing (Maybe ISO8601)
rilUpdateDate f x =
    f (_rilUpdateDate x) <&> \y -> x { _rilUpdateDate = y }
{-# INLINE rilUpdateDate #-}

-- | The status of the Reserved Instance listing.
rilStatus :: Lens' ReservedInstancesListing (Maybe ListingStatus)
rilStatus f x =
    f (_rilStatus x) <&> \y -> x { _rilStatus = y }
{-# INLINE rilStatus #-}

-- | The reason for the current status of the Reserved Instance listing. The
-- response can be blank.
rilStatusMessage :: Lens' ReservedInstancesListing (Maybe Text)
rilStatusMessage f x =
    f (_rilStatusMessage x) <&> \y -> x { _rilStatusMessage = y }
{-# INLINE rilStatusMessage #-}

-- | The number of instances in this state.
rilInstanceCounts :: Lens' ReservedInstancesListing [InstanceCount]
rilInstanceCounts f x =
    f (_rilInstanceCounts x) <&> \y -> x { _rilInstanceCounts = y }
{-# INLINE rilInstanceCounts #-}

-- | The price of the Reserved Instance listing.
rilPriceSchedules :: Lens' ReservedInstancesListing [PriceSchedule]
rilPriceSchedules f x =
    f (_rilPriceSchedules x) <&> \y -> x { _rilPriceSchedules = y }
{-# INLINE rilPriceSchedules #-}

-- | Any tags assigned to the resource.
rilTags :: Lens' ReservedInstancesListing [Tag]
rilTags f x =
    f (_rilTags x) <&> \y -> x { _rilTags = y }
{-# INLINE rilTags #-}

-- | The idempotency token you provided when you created the listing.
rilClientToken :: Lens' ReservedInstancesListing (Maybe Text)
rilClientToken f x =
    f (_rilClientToken x) <&> \y -> x { _rilClientToken = y }
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
rirReservedInstancesModificationId f x =
    f (_rirReservedInstancesModificationId x) <&> \y -> x { _rirReservedInstancesModificationId = y }
{-# INLINE rirReservedInstancesModificationId #-}

-- | The IDs of one or more Reserved Instances.
rirReservedInstancesIds :: Lens' ReservedInstancesModification [ReservedInstancesId]
rirReservedInstancesIds f x =
    f (_rirReservedInstancesIds x) <&> \y -> x { _rirReservedInstancesIds = y }
{-# INLINE rirReservedInstancesIds #-}

-- | Contains target configurations along with their corresponding new Reserved
-- Instance IDs.
rirModificationResults :: Lens' ReservedInstancesModification [ReservedInstancesModificationResult]
rirModificationResults f x =
    f (_rirModificationResults x) <&> \y -> x { _rirModificationResults = y }
{-# INLINE rirModificationResults #-}

-- | The time when the modification request was created.
rirCreateDate :: Lens' ReservedInstancesModification (Maybe ISO8601)
rirCreateDate f x =
    f (_rirCreateDate x) <&> \y -> x { _rirCreateDate = y }
{-# INLINE rirCreateDate #-}

-- | The time when the modification request was last updated.
rirUpdateDate :: Lens' ReservedInstancesModification (Maybe ISO8601)
rirUpdateDate f x =
    f (_rirUpdateDate x) <&> \y -> x { _rirUpdateDate = y }
{-# INLINE rirUpdateDate #-}

-- | The time for the modification to become effective.
rirEffectiveDate :: Lens' ReservedInstancesModification (Maybe ISO8601)
rirEffectiveDate f x =
    f (_rirEffectiveDate x) <&> \y -> x { _rirEffectiveDate = y }
{-# INLINE rirEffectiveDate #-}

-- | The status of the Reserved Instances modification request.
rirStatus :: Lens' ReservedInstancesModification (Maybe Text)
rirStatus f x =
    f (_rirStatus x) <&> \y -> x { _rirStatus = y }
{-# INLINE rirStatus #-}

-- | The reason for the status.
rirStatusMessage :: Lens' ReservedInstancesModification (Maybe Text)
rirStatusMessage f x =
    f (_rirStatusMessage x) <&> \y -> x { _rirStatusMessage = y }
{-# INLINE rirStatusMessage #-}

-- | A unique, case-sensitive key supplied by the client to ensure that the
-- modification request is idempotent.
rirClientToken :: Lens' ReservedInstancesModification (Maybe Text)
rirClientToken f x =
    f (_rirClientToken x) <&> \y -> x { _rirClientToken = y }
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
rimrReservedInstancesId f x =
    f (_rimrReservedInstancesId x) <&> \y -> x { _rimrReservedInstancesId = y }
{-# INLINE rimrReservedInstancesId #-}

-- | The target Reserved Instances configurations supplied as part of the
-- modification request.
rimrTargetConfiguration :: Lens' ReservedInstancesModificationResult (Maybe ReservedInstancesConfiguration)
rimrTargetConfiguration f x =
    f (_rimrTargetConfiguration x) <&> \y -> x { _rimrTargetConfiguration = y }
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
ritReservedInstancesOfferingId :: Lens' ReservedInstancesOffering (Maybe Text)
ritReservedInstancesOfferingId f x =
    f (_ritReservedInstancesOfferingId x) <&> \y -> x { _ritReservedInstancesOfferingId = y }
{-# INLINE ritReservedInstancesOfferingId #-}

-- | The instance type on which the Reserved Instance can be used.
ritInstanceType :: Lens' ReservedInstancesOffering (Maybe InstanceType)
ritInstanceType f x =
    f (_ritInstanceType x) <&> \y -> x { _ritInstanceType = y }
{-# INLINE ritInstanceType #-}

-- | The Availability Zone in which the Reserved Instance can be used.
ritAvailabilityZone :: Lens' ReservedInstancesOffering (Maybe Text)
ritAvailabilityZone f x =
    f (_ritAvailabilityZone x) <&> \y -> x { _ritAvailabilityZone = y }
{-# INLINE ritAvailabilityZone #-}

-- | The duration of the Reserved Instance, in seconds.
ritDuration :: Lens' ReservedInstancesOffering (Maybe Integer)
ritDuration f x =
    f (_ritDuration x) <&> \y -> x { _ritDuration = y }
{-# INLINE ritDuration #-}

-- | The usage price of the Reserved Instance, per hour.
ritUsagePrice :: Lens' ReservedInstancesOffering (Maybe Double)
ritUsagePrice f x =
    f (_ritUsagePrice x) <&> \y -> x { _ritUsagePrice = y }
{-# INLINE ritUsagePrice #-}

-- | The purchase price of the Reserved Instance.
ritFixedPrice :: Lens' ReservedInstancesOffering (Maybe Double)
ritFixedPrice f x =
    f (_ritFixedPrice x) <&> \y -> x { _ritFixedPrice = y }
{-# INLINE ritFixedPrice #-}

-- | The Reserved Instance description.
ritProductDescription :: Lens' ReservedInstancesOffering (Maybe RIProductDescription)
ritProductDescription f x =
    f (_ritProductDescription x) <&> \y -> x { _ritProductDescription = y }
{-# INLINE ritProductDescription #-}

-- | The tenancy of the reserved instance.
ritInstanceTenancy :: Lens' ReservedInstancesOffering (Maybe Tenancy)
ritInstanceTenancy f x =
    f (_ritInstanceTenancy x) <&> \y -> x { _ritInstanceTenancy = y }
{-# INLINE ritInstanceTenancy #-}

-- | The currency of the Reserved Instance offering you are purchasing. It's
-- specified using ISO 4217 standard currency codes. At this time, the only
-- supported currency is USD.
ritCurrencyCode :: Lens' ReservedInstancesOffering (Maybe CurrencyCodeValues)
ritCurrencyCode f x =
    f (_ritCurrencyCode x) <&> \y -> x { _ritCurrencyCode = y }
{-# INLINE ritCurrencyCode #-}

-- | The Reserved Instance offering type.
ritOfferingType :: Lens' ReservedInstancesOffering (Maybe OfferingTypeValues)
ritOfferingType f x =
    f (_ritOfferingType x) <&> \y -> x { _ritOfferingType = y }
{-# INLINE ritOfferingType #-}

-- | The recurring charge tag assigned to the resource.
ritRecurringCharges :: Lens' ReservedInstancesOffering [RecurringCharge]
ritRecurringCharges f x =
    f (_ritRecurringCharges x) <&> \y -> x { _ritRecurringCharges = y }
{-# INLINE ritRecurringCharges #-}

-- | Indicates whether the offering is available through the Reserved Instance
-- Marketplace (resale) or AWS. If it's a Reserved Instance Marketplace
-- offering, this is true.
ritMarketplace :: Lens' ReservedInstancesOffering (Maybe Bool)
ritMarketplace f x =
    f (_ritMarketplace x) <&> \y -> x { _ritMarketplace = y }
{-# INLINE ritMarketplace #-}

-- | The pricing details of the Reserved Instance offering.
ritPricingDetails :: Lens' ReservedInstancesOffering [PricingDetail]
ritPricingDetails f x =
    f (_ritPricingDetails x) <&> \y -> x { _ritPricingDetails = y }
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
reDestinationCidrBlock f x =
    f (_reDestinationCidrBlock x) <&> \y -> x { _reDestinationCidrBlock = y }
{-# INLINE reDestinationCidrBlock #-}

-- | The ID of a gateway attached to your VPC.
reGatewayId :: Lens' Route (Maybe Text)
reGatewayId f x =
    f (_reGatewayId x) <&> \y -> x { _reGatewayId = y }
{-# INLINE reGatewayId #-}

-- | The ID of a NAT instance in your VPC.
reInstanceId :: Lens' Route (Maybe Text)
reInstanceId f x =
    f (_reInstanceId x) <&> \y -> x { _reInstanceId = y }
{-# INLINE reInstanceId #-}

-- | The AWS account ID of the owner of the instance.
reInstanceOwnerId :: Lens' Route (Maybe Text)
reInstanceOwnerId f x =
    f (_reInstanceOwnerId x) <&> \y -> x { _reInstanceOwnerId = y }
{-# INLINE reInstanceOwnerId #-}

-- | The ID of the network interface.
reNetworkInterfaceId :: Lens' Route (Maybe Text)
reNetworkInterfaceId f x =
    f (_reNetworkInterfaceId x) <&> \y -> x { _reNetworkInterfaceId = y }
{-# INLINE reNetworkInterfaceId #-}

-- | The ID of the VPC peering connection.
reVpcPeeringConnectionId :: Lens' Route (Maybe Text)
reVpcPeeringConnectionId f x =
    f (_reVpcPeeringConnectionId x) <&> \y -> x { _reVpcPeeringConnectionId = y }
{-# INLINE reVpcPeeringConnectionId #-}

-- | The state of the route. The blackhole state indicates that the route's
-- target isn't available (for example, the specified gateway isn't attached
-- to the VPC, or the specified NAT instance has been terminated).
reState :: Lens' Route (Maybe RouteState)
reState f x =
    f (_reState x) <&> \y -> x { _reState = y }
{-# INLINE reState #-}

-- | Describes how the route was created. CreateRouteTable indicates that route
-- was automatically created when the route table was created. CreateRoute
-- indicates that the route was manually added to the route table.
-- EnableVgwRoutePropagation indicates that the route was propagated by route
-- propagation.
reOrigin :: Lens' Route (Maybe RouteOrigin)
reOrigin f x =
    f (_reOrigin x) <&> \y -> x { _reOrigin = y }
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
rtRouteTableId :: Lens' RouteTable (Maybe Text)
rtRouteTableId f x =
    f (_rtRouteTableId x) <&> \y -> x { _rtRouteTableId = y }
{-# INLINE rtRouteTableId #-}

-- | The ID of the VPC.
rtVpcId :: Lens' RouteTable (Maybe Text)
rtVpcId f x =
    f (_rtVpcId x) <&> \y -> x { _rtVpcId = y }
{-# INLINE rtVpcId #-}

-- | The routes in the route table.
rtRoutes :: Lens' RouteTable [Route]
rtRoutes f x =
    f (_rtRoutes x) <&> \y -> x { _rtRoutes = y }
{-# INLINE rtRoutes #-}

-- | The associations between the route table and one or more subnets.
rtAssociations :: Lens' RouteTable [RouteTableAssociation]
rtAssociations f x =
    f (_rtAssociations x) <&> \y -> x { _rtAssociations = y }
{-# INLINE rtAssociations #-}

-- | Any tags assigned to the route table.
rtTags :: Lens' RouteTable [Tag]
rtTags f x =
    f (_rtTags x) <&> \y -> x { _rtTags = y }
{-# INLINE rtTags #-}

-- | Any virtual private gateway (VGW) propagating routes.
rtPropagatingVgws :: Lens' RouteTable [PropagatingVgw]
rtPropagatingVgws f x =
    f (_rtPropagatingVgws x) <&> \y -> x { _rtPropagatingVgws = y }
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
rtaRouteTableAssociationId f x =
    f (_rtaRouteTableAssociationId x) <&> \y -> x { _rtaRouteTableAssociationId = y }
{-# INLINE rtaRouteTableAssociationId #-}

-- | The ID of the route table.
rtaRouteTableId :: Lens' RouteTableAssociation (Maybe Text)
rtaRouteTableId f x =
    f (_rtaRouteTableId x) <&> \y -> x { _rtaRouteTableId = y }
{-# INLINE rtaRouteTableId #-}

-- | The ID of the subnet.
rtaSubnetId :: Lens' RouteTableAssociation (Maybe Text)
rtaSubnetId f x =
    f (_rtaSubnetId x) <&> \y -> x { _rtaSubnetId = y }
{-# INLINE rtaSubnetId #-}

-- | Indicates whether this is the main route table.
rtaMain :: Lens' RouteTableAssociation (Maybe Bool)
rtaMain f x =
    f (_rtaMain x) <&> \y -> x { _rtaMain = y }
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
ssBucket :: Lens' S3Storage (Maybe Text)
ssBucket f x =
    f (_ssBucket x) <&> \y -> x { _ssBucket = y }
{-# INLINE ssBucket #-}

-- | The beginning of the file name of the AMI.
ssPrefix :: Lens' S3Storage (Maybe Text)
ssPrefix f x =
    f (_ssPrefix x) <&> \y -> x { _ssPrefix = y }
{-# INLINE ssPrefix #-}

-- | The access key ID of the owner of the bucket. Before you specify a value
-- for your access key ID, review and follow the guidance in Best Practices
-- for Managing AWS Access Keys.
ssAWSAccessKeyId :: Lens' S3Storage (Maybe Text)
ssAWSAccessKeyId f x =
    f (_ssAWSAccessKeyId x) <&> \y -> x { _ssAWSAccessKeyId = y }
{-# INLINE ssAWSAccessKeyId #-}

-- | A Base64-encoded Amazon S3 upload policy that gives Amazon EC2 permission
-- to upload items into Amazon S3 on your behalf.
ssUploadPolicy :: Lens' S3Storage (Maybe ByteString)
ssUploadPolicy f x =
    f (_ssUploadPolicy x) <&> \y -> x { _ssUploadPolicy = y }
{-# INLINE ssUploadPolicy #-}

-- | The signature of the Base64 encoded JSON document.
ssUploadPolicySignature :: Lens' S3Storage (Maybe Text)
ssUploadPolicySignature f x =
    f (_ssUploadPolicySignature x) <&> \y -> x { _ssUploadPolicySignature = y }
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
siOwnerId :: Lens' SecurityGroup Text
siOwnerId f x =
    f (_siOwnerId x) <&> \y -> x { _siOwnerId = y }
{-# INLINE siOwnerId #-}

-- | The name of the security group.
siGroupName :: Lens' SecurityGroup Text
siGroupName f x =
    f (_siGroupName x) <&> \y -> x { _siGroupName = y }
{-# INLINE siGroupName #-}

-- | The ID of the security group.
siGroupId :: Lens' SecurityGroup Text
siGroupId f x =
    f (_siGroupId x) <&> \y -> x { _siGroupId = y }
{-# INLINE siGroupId #-}

-- | A description of the security group.
siDescription :: Lens' SecurityGroup Text
siDescription f x =
    f (_siDescription x) <&> \y -> x { _siDescription = y }
{-# INLINE siDescription #-}

-- | One or more inbound rules associated with the security group.
siIpPermissions :: Lens' SecurityGroup [IpPermission]
siIpPermissions f x =
    f (_siIpPermissions x) <&> \y -> x { _siIpPermissions = y }
{-# INLINE siIpPermissions #-}

-- | [EC2-VPC] One or more outbound rules associated with the security group.
siIpPermissionsEgress :: Lens' SecurityGroup [IpPermission]
siIpPermissionsEgress f x =
    f (_siIpPermissionsEgress x) <&> \y -> x { _siIpPermissionsEgress = y }
{-# INLINE siIpPermissionsEgress #-}

-- | [EC2-VPC] The ID of the VPC for the security group.
siVpcId :: Lens' SecurityGroup (Maybe Text)
siVpcId f x =
    f (_siVpcId x) <&> \y -> x { _siVpcId = y }
{-# INLINE siVpcId #-}

-- | Any tags assigned to the security group.
siTags :: Lens' SecurityGroup [Tag]
siTags f x =
    f (_siTags x) <&> \y -> x { _siTags = y }
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
ssuSnapshotId f x =
    f (_ssuSnapshotId x) <&> \y -> x { _ssuSnapshotId = y }
{-# INLINE ssuSnapshotId #-}

-- | The ID of the volume.
ssuVolumeId :: Lens' Snapshot (Maybe Text)
ssuVolumeId f x =
    f (_ssuVolumeId x) <&> \y -> x { _ssuVolumeId = y }
{-# INLINE ssuVolumeId #-}

-- | The snapshot state.
ssuState :: Lens' Snapshot (Maybe SnapshotState)
ssuState f x =
    f (_ssuState x) <&> \y -> x { _ssuState = y }
{-# INLINE ssuState #-}

-- | The time stamp when the snapshot was initiated.
ssuStartTime :: Lens' Snapshot (Maybe ISO8601)
ssuStartTime f x =
    f (_ssuStartTime x) <&> \y -> x { _ssuStartTime = y }
{-# INLINE ssuStartTime #-}

-- | The progress of the snapshot, as a percentage.
ssuProgress :: Lens' Snapshot (Maybe Text)
ssuProgress f x =
    f (_ssuProgress x) <&> \y -> x { _ssuProgress = y }
{-# INLINE ssuProgress #-}

-- | The AWS account ID of the Amazon EBS snapshot owner.
ssuOwnerId :: Lens' Snapshot (Maybe Text)
ssuOwnerId f x =
    f (_ssuOwnerId x) <&> \y -> x { _ssuOwnerId = y }
{-# INLINE ssuOwnerId #-}

-- | The description for the snapshot.
ssuDescription :: Lens' Snapshot (Maybe Text)
ssuDescription f x =
    f (_ssuDescription x) <&> \y -> x { _ssuDescription = y }
{-# INLINE ssuDescription #-}

-- | The size of the volume, in GiB.
ssuVolumeSize :: Lens' Snapshot (Maybe Integer)
ssuVolumeSize f x =
    f (_ssuVolumeSize x) <&> \y -> x { _ssuVolumeSize = y }
{-# INLINE ssuVolumeSize #-}

-- | The AWS account alias (for example, amazon, self) or AWS account ID that
-- owns the snapshot.
ssuOwnerAlias :: Lens' Snapshot (Maybe Text)
ssuOwnerAlias f x =
    f (_ssuOwnerAlias x) <&> \y -> x { _ssuOwnerAlias = y }
{-# INLINE ssuOwnerAlias #-}

-- | Any tags assigned to the snapshot.
ssuTags :: Lens' Snapshot [Tag]
ssuTags f x =
    f (_ssuTags x) <&> \y -> x { _ssuTags = y }
{-# INLINE ssuTags #-}

-- | Indicates whether the snapshot is encrypted.
ssuEncrypted :: Lens' Snapshot (Maybe Bool)
ssuEncrypted f x =
    f (_ssuEncrypted x) <&> \y -> x { _ssuEncrypted = y }
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
sdsOwnerId f x =
    f (_sdsOwnerId x) <&> \y -> x { _sdsOwnerId = y }
{-# INLINE sdsOwnerId #-}

-- | The Amazon S3 bucket where the Spot Instance datafeed is located.
sdsBucket :: Lens' SpotDatafeedSubscription (Maybe Text)
sdsBucket f x =
    f (_sdsBucket x) <&> \y -> x { _sdsBucket = y }
{-# INLINE sdsBucket #-}

-- | The prefix that is prepended to datafeed files.
sdsPrefix :: Lens' SpotDatafeedSubscription (Maybe Text)
sdsPrefix f x =
    f (_sdsPrefix x) <&> \y -> x { _sdsPrefix = y }
{-# INLINE sdsPrefix #-}

-- | The state of the Spot Instance datafeed subscription.
sdsState :: Lens' SpotDatafeedSubscription (Maybe DatafeedSubscriptionState)
sdsState f x =
    f (_sdsState x) <&> \y -> x { _sdsState = y }
{-# INLINE sdsState #-}

-- | The fault codes for the Spot Instance request, if any.
sdsFault :: Lens' SpotDatafeedSubscription (Maybe SpotInstanceStateFault)
sdsFault f x =
    f (_sdsFault x) <&> \y -> x { _sdsFault = y }
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
sirSpotInstanceRequestId f x =
    f (_sirSpotInstanceRequestId x) <&> \y -> x { _sirSpotInstanceRequestId = y }
{-# INLINE sirSpotInstanceRequestId #-}

-- | The maximum hourly price for any Spot Instance launched to fulfill the
-- request.
sirSpotPrice :: Lens' SpotInstanceRequest (Maybe Text)
sirSpotPrice f x =
    f (_sirSpotPrice x) <&> \y -> x { _sirSpotPrice = y }
{-# INLINE sirSpotPrice #-}

-- | The Spot Instance request type.
sirType :: Lens' SpotInstanceRequest (Maybe SpotInstanceType)
sirType f x =
    f (_sirType x) <&> \y -> x { _sirType = y }
{-# INLINE sirType #-}

-- | The state of the Spot Instance request. Spot bid status information can
-- help you track your Spot Instance requests. For information, see Tracking
-- Spot Requests with Bid Status Codes in the Amazon Elastic Compute Cloud
-- User Guide.
sirState :: Lens' SpotInstanceRequest (Maybe SpotInstanceState)
sirState f x =
    f (_sirState x) <&> \y -> x { _sirState = y }
{-# INLINE sirState #-}

-- | The fault codes for the Spot Instance request, if any.
sirFault :: Lens' SpotInstanceRequest (Maybe SpotInstanceStateFault)
sirFault f x =
    f (_sirFault x) <&> \y -> x { _sirFault = y }
{-# INLINE sirFault #-}

-- | The status code and status message describing the Spot Instance request.
sirStatus :: Lens' SpotInstanceRequest (Maybe SpotInstanceStatus)
sirStatus f x =
    f (_sirStatus x) <&> \y -> x { _sirStatus = y }
{-# INLINE sirStatus #-}

-- | The start date of the request. If this is a one-time request, the request
-- becomes active at this date and time and remains active until all instances
-- launch, the request expires, or the request is canceled. If the request is
-- persistent, the request becomes active at this date and time and remains
-- active until it expires or is canceled.
sirValidFrom :: Lens' SpotInstanceRequest (Maybe ISO8601)
sirValidFrom f x =
    f (_sirValidFrom x) <&> \y -> x { _sirValidFrom = y }
{-# INLINE sirValidFrom #-}

-- | The end date of the request. If this is a one-time request, the request
-- remains active until all instances launch, the request is canceled, or this
-- date is reached. If the request is persistent, it remains active until it
-- is canceled or this date is reached.
sirValidUntil :: Lens' SpotInstanceRequest (Maybe ISO8601)
sirValidUntil f x =
    f (_sirValidUntil x) <&> \y -> x { _sirValidUntil = y }
{-# INLINE sirValidUntil #-}

-- | The instance launch group. Launch groups are Spot Instances that launch
-- together and terminate together.
sirLaunchGroup :: Lens' SpotInstanceRequest (Maybe Text)
sirLaunchGroup f x =
    f (_sirLaunchGroup x) <&> \y -> x { _sirLaunchGroup = y }
{-# INLINE sirLaunchGroup #-}

-- | The Availability Zone group. If you specify the same Availability Zone
-- group for all Spot Instance requests, all Spot Instances are launched in
-- the same Availability Zone.
sirAvailabilityZoneGroup :: Lens' SpotInstanceRequest (Maybe Text)
sirAvailabilityZoneGroup f x =
    f (_sirAvailabilityZoneGroup x) <&> \y -> x { _sirAvailabilityZoneGroup = y }
{-# INLINE sirAvailabilityZoneGroup #-}

-- | Additional information for launching instances.
sirLaunchSpecification :: Lens' SpotInstanceRequest (Maybe LaunchSpecification)
sirLaunchSpecification f x =
    f (_sirLaunchSpecification x) <&> \y -> x { _sirLaunchSpecification = y }
{-# INLINE sirLaunchSpecification #-}

-- | The instance ID, if an instance has been launched to fulfill the Spot
-- Instance request.
sirInstanceId :: Lens' SpotInstanceRequest (Maybe Text)
sirInstanceId f x =
    f (_sirInstanceId x) <&> \y -> x { _sirInstanceId = y }
{-# INLINE sirInstanceId #-}

-- | The time stamp when the Spot Instance request was created.
sirCreateTime :: Lens' SpotInstanceRequest (Maybe ISO8601)
sirCreateTime f x =
    f (_sirCreateTime x) <&> \y -> x { _sirCreateTime = y }
{-# INLINE sirCreateTime #-}

-- | The product description associated with the Spot Instance.
sirProductDescription :: Lens' SpotInstanceRequest (Maybe RIProductDescription)
sirProductDescription f x =
    f (_sirProductDescription x) <&> \y -> x { _sirProductDescription = y }
{-# INLINE sirProductDescription #-}

-- | Any tags assigned to the resource.
sirTags :: Lens' SpotInstanceRequest [Tag]
sirTags f x =
    f (_sirTags x) <&> \y -> x { _sirTags = y }
{-# INLINE sirTags #-}

-- | The Availability Zone in which the bid is launched.
sirLaunchedAvailabilityZone :: Lens' SpotInstanceRequest (Maybe Text)
sirLaunchedAvailabilityZone f x =
    f (_sirLaunchedAvailabilityZone x) <&> \y -> x { _sirLaunchedAvailabilityZone = y }
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
sisfCode f x =
    f (_sisfCode x) <&> \y -> x { _sisfCode = y }
{-# INLINE sisfCode #-}

-- | The message for the Spot Instance state change.
sisfMessage :: Lens' SpotInstanceStateFault (Maybe Text)
sisfMessage f x =
    f (_sisfMessage x) <&> \y -> x { _sisfMessage = y }
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
siuCode :: Lens' SpotInstanceStatus (Maybe Text)
siuCode f x =
    f (_siuCode x) <&> \y -> x { _siuCode = y }
{-# INLINE siuCode #-}

-- | The time of the most recent status update.
siuUpdateTime :: Lens' SpotInstanceStatus (Maybe ISO8601)
siuUpdateTime f x =
    f (_siuUpdateTime x) <&> \y -> x { _siuUpdateTime = y }
{-# INLINE siuUpdateTime #-}

-- | The description for the status code for the Spot request.
siuMessage :: Lens' SpotInstanceStatus (Maybe Text)
siuMessage f x =
    f (_siuMessage x) <&> \y -> x { _siuMessage = y }
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
spAvailabilityZone :: Lens' SpotPlacement (Maybe Text)
spAvailabilityZone f x =
    f (_spAvailabilityZone x) <&> \y -> x { _spAvailabilityZone = y }
{-# INLINE spAvailabilityZone #-}

-- | The Availability Zone group name.
spGroupName :: Lens' SpotPlacement (Maybe Text)
spGroupName f x =
    f (_spGroupName x) <&> \y -> x { _spGroupName = y }
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
sqInstanceType :: Lens' SpotPrice (Maybe InstanceType)
sqInstanceType f x =
    f (_sqInstanceType x) <&> \y -> x { _sqInstanceType = y }
{-# INLINE sqInstanceType #-}

-- | A general description of the AMI.
sqProductDescription :: Lens' SpotPrice (Maybe RIProductDescription)
sqProductDescription f x =
    f (_sqProductDescription x) <&> \y -> x { _sqProductDescription = y }
{-# INLINE sqProductDescription #-}

-- | The maximum price you will pay to launch one or more Spot Instances.
sqSpotPrice :: Lens' SpotPrice (Maybe Text)
sqSpotPrice f x =
    f (_sqSpotPrice x) <&> \y -> x { _sqSpotPrice = y }
{-# INLINE sqSpotPrice #-}

-- | The date and time the request was created.
sqTimestamp :: Lens' SpotPrice (Maybe ISO8601)
sqTimestamp f x =
    f (_sqTimestamp x) <&> \y -> x { _sqTimestamp = y }
{-# INLINE sqTimestamp #-}

-- | The Availability Zone.
sqAvailabilityZone :: Lens' SpotPrice (Maybe Text)
sqAvailabilityZone f x =
    f (_sqAvailabilityZone x) <&> \y -> x { _sqAvailabilityZone = y }
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
srCode f x =
    f (_srCode x) <&> \y -> x { _srCode = y }
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
srMessage f x =
    f (_srMessage x) <&> \y -> x { _srMessage = y }
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
sxSubnetId :: Lens' Subnet (Maybe Text)
sxSubnetId f x =
    f (_sxSubnetId x) <&> \y -> x { _sxSubnetId = y }
{-# INLINE sxSubnetId #-}

-- | The current state of the subnet.
sxState :: Lens' Subnet (Maybe SubnetState)
sxState f x =
    f (_sxState x) <&> \y -> x { _sxState = y }
{-# INLINE sxState #-}

-- | The ID of the VPC the subnet is in.
sxVpcId :: Lens' Subnet (Maybe Text)
sxVpcId f x =
    f (_sxVpcId x) <&> \y -> x { _sxVpcId = y }
{-# INLINE sxVpcId #-}

-- | The CIDR block assigned to the subnet.
sxCidrBlock :: Lens' Subnet (Maybe Text)
sxCidrBlock f x =
    f (_sxCidrBlock x) <&> \y -> x { _sxCidrBlock = y }
{-# INLINE sxCidrBlock #-}

-- | The number of unused IP addresses in the subnet. Note that the IP addresses
-- for any stopped instances are considered unavailable.
sxAvailableIpAddressCount :: Lens' Subnet (Maybe Integer)
sxAvailableIpAddressCount f x =
    f (_sxAvailableIpAddressCount x) <&> \y -> x { _sxAvailableIpAddressCount = y }
{-# INLINE sxAvailableIpAddressCount #-}

-- | The Availability Zone of the subnet.
sxAvailabilityZone :: Lens' Subnet (Maybe Text)
sxAvailabilityZone f x =
    f (_sxAvailabilityZone x) <&> \y -> x { _sxAvailabilityZone = y }
{-# INLINE sxAvailabilityZone #-}

-- | Indicates whether this is the default subnet for the Availability Zone.
sxDefaultForAz :: Lens' Subnet (Maybe Bool)
sxDefaultForAz f x =
    f (_sxDefaultForAz x) <&> \y -> x { _sxDefaultForAz = y }
{-# INLINE sxDefaultForAz #-}

-- | Indicates whether instances launched in this subnet receive a public IP
-- address.
sxMapPublicIpOnLaunch :: Lens' Subnet (Maybe Bool)
sxMapPublicIpOnLaunch f x =
    f (_sxMapPublicIpOnLaunch x) <&> \y -> x { _sxMapPublicIpOnLaunch = y }
{-# INLINE sxMapPublicIpOnLaunch #-}

-- | Any tags assigned to the subnet.
sxTags :: Lens' Subnet [Tag]
sxTags f x =
    f (_sxTags x) <&> \y -> x { _sxTags = y }
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
tgKey :: Lens' Tag Text
tgKey f x =
    f (_tgKey x) <&> \y -> x { _tgKey = y }
{-# INLINE tgKey #-}

-- | The value of the tag. Constraints: Tag values are case-sensitive and accept
-- a maximum of 255 Unicode characters.
tgValue :: Lens' Tag Text
tgValue f x =
    f (_tgValue x) <&> \y -> x { _tgValue = y }
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
tdResourceId :: Lens' TagDescription Text
tdResourceId f x =
    f (_tdResourceId x) <&> \y -> x { _tdResourceId = y }
{-# INLINE tdResourceId #-}

-- | The type of resource.
tdResourceType :: Lens' TagDescription ResourceType
tdResourceType f x =
    f (_tdResourceType x) <&> \y -> x { _tdResourceType = y }
{-# INLINE tdResourceType #-}

-- | The key of the tag.
tdKey :: Lens' TagDescription Text
tdKey f x =
    f (_tdKey x) <&> \y -> x { _tdKey = y }
{-# INLINE tdKey #-}

-- | The value of the tag.
tdValue :: Lens' TagDescription Text
tdValue f x =
    f (_tdValue x) <&> \y -> x { _tdValue = y }
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
uigpUserId f x =
    f (_uigpUserId x) <&> \y -> x { _uigpUserId = y }
{-# INLINE uigpUserId #-}

-- | The ID of the security group owned by the specified AWS account.
uigpGroupName :: Lens' UserIdGroupPair (Maybe Text)
uigpGroupName f x =
    f (_uigpGroupName x) <&> \y -> x { _uigpGroupName = y }
{-# INLINE uigpGroupName #-}

-- | The name of the security group in the specified AWS account.
uigpGroupId :: Lens' UserIdGroupPair (Maybe Text)
uigpGroupId f x =
    f (_uigpGroupId x) <&> \y -> x { _uigpGroupId = y }
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
vvvvvvvvvvvvvvyOutsideIpAddress :: Lens' VgwTelemetry (Maybe Text)
vvvvvvvvvvvvvvyOutsideIpAddress f x =
    f (_vvvvvvvvvvvvvvyOutsideIpAddress x) <&> \y -> x { _vvvvvvvvvvvvvvyOutsideIpAddress = y }
{-# INLINE vvvvvvvvvvvvvvyOutsideIpAddress #-}

-- | The status of the VPN tunnel.
vvvvvvvvvvvvvvyStatus :: Lens' VgwTelemetry (Maybe TelemetryStatus)
vvvvvvvvvvvvvvyStatus f x =
    f (_vvvvvvvvvvvvvvyStatus x) <&> \y -> x { _vvvvvvvvvvvvvvyStatus = y }
{-# INLINE vvvvvvvvvvvvvvyStatus #-}

-- | The date and time of the last change in status.
vvvvvvvvvvvvvvyLastStatusChange :: Lens' VgwTelemetry (Maybe ISO8601)
vvvvvvvvvvvvvvyLastStatusChange f x =
    f (_vvvvvvvvvvvvvvyLastStatusChange x) <&> \y -> x { _vvvvvvvvvvvvvvyLastStatusChange = y }
{-# INLINE vvvvvvvvvvvvvvyLastStatusChange #-}

-- | If an error occurs, a description of the error.
vvvvvvvvvvvvvvyStatusMessage :: Lens' VgwTelemetry (Maybe Text)
vvvvvvvvvvvvvvyStatusMessage f x =
    f (_vvvvvvvvvvvvvvyStatusMessage x) <&> \y -> x { _vvvvvvvvvvvvvvyStatusMessage = y }
{-# INLINE vvvvvvvvvvvvvvyStatusMessage #-}

-- | The number of accepted routes.
vvvvvvvvvvvvvvyAcceptedRouteCount :: Lens' VgwTelemetry (Maybe Integer)
vvvvvvvvvvvvvvyAcceptedRouteCount f x =
    f (_vvvvvvvvvvvvvvyAcceptedRouteCount x) <&> \y -> x { _vvvvvvvvvvvvvvyAcceptedRouteCount = y }
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
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjVolumeId :: Lens' Volume (Maybe Text)
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjVolumeId f x =
    f (_vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjVolumeId x) <&> \y -> x { _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjVolumeId = y }
{-# INLINE vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjVolumeId #-}

-- | The size of the volume, in GiBs.
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjSize :: Lens' Volume (Maybe Integer)
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjSize f x =
    f (_vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjSize x) <&> \y -> x { _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjSize = y }
{-# INLINE vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjSize #-}

-- | The snapshot from which the volume was created, if applicable.
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjSnapshotId :: Lens' Volume (Maybe Text)
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjSnapshotId f x =
    f (_vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjSnapshotId x) <&> \y -> x { _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjSnapshotId = y }
{-# INLINE vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjSnapshotId #-}

-- | The Availability Zone for the volume.
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjAvailabilityZone :: Lens' Volume (Maybe Text)
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjAvailabilityZone f x =
    f (_vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjAvailabilityZone x) <&> \y -> x { _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjAvailabilityZone = y }
{-# INLINE vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjAvailabilityZone #-}

-- | The volume state.
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjState :: Lens' Volume (Maybe VolumeState)
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjState f x =
    f (_vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjState x) <&> \y -> x { _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjState = y }
{-# INLINE vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjState #-}

-- | The time stamp when volume creation was initiated.
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjCreateTime :: Lens' Volume (Maybe ISO8601)
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjCreateTime f x =
    f (_vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjCreateTime x) <&> \y -> x { _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjCreateTime = y }
{-# INLINE vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjCreateTime #-}

-- | 
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjAttachments :: Lens' Volume [VolumeAttachment]
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjAttachments f x =
    f (_vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjAttachments x) <&> \y -> x { _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjAttachments = y }
{-# INLINE vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjAttachments #-}

-- | Any tags assigned to the volume.
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjTags :: Lens' Volume [Tag]
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjTags f x =
    f (_vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjTags x) <&> \y -> x { _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjTags = y }
{-# INLINE vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjTags #-}

-- | The volume type. This can be gp2 for General Purpose (SSD) volumes, io1 for
-- Provisioned IOPS (SSD) volumes, or standard for Magnetic volumes.
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjVolumeType :: Lens' Volume (Maybe VolumeType)
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjVolumeType f x =
    f (_vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjVolumeType x) <&> \y -> x { _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjVolumeType = y }
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
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjIops f x =
    f (_vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjIops x) <&> \y -> x { _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjIops = y }
{-# INLINE vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjIops #-}

-- | Indicates whether the volume is encrypted.
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjEncrypted :: Lens' Volume (Maybe Bool)
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjEncrypted f x =
    f (_vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjEncrypted x) <&> \y -> x { _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvjEncrypted = y }
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
vcVolumeId f x =
    f (_vcVolumeId x) <&> \y -> x { _vcVolumeId = y }
{-# INLINE vcVolumeId #-}

-- | The ID of the instance.
vcInstanceId :: Lens' VolumeAttachment (Maybe Text)
vcInstanceId f x =
    f (_vcInstanceId x) <&> \y -> x { _vcInstanceId = y }
{-# INLINE vcInstanceId #-}

-- | The device name.
vcDevice :: Lens' VolumeAttachment (Maybe Text)
vcDevice f x =
    f (_vcDevice x) <&> \y -> x { _vcDevice = y }
{-# INLINE vcDevice #-}

-- | The attachment state of the volume.
vcState :: Lens' VolumeAttachment (Maybe VolumeAttachmentState)
vcState f x =
    f (_vcState x) <&> \y -> x { _vcState = y }
{-# INLINE vcState #-}

-- | The time stamp when the attachment initiated.
vcAttachTime :: Lens' VolumeAttachment (Maybe ISO8601)
vcAttachTime f x =
    f (_vcAttachTime x) <&> \y -> x { _vcAttachTime = y }
{-# INLINE vcAttachTime #-}

-- | Indicates whether the Amazon EBS volume is deleted on instance termination.
vcDeleteOnTermination :: Lens' VolumeAttachment (Maybe Bool)
vcDeleteOnTermination f x =
    f (_vcDeleteOnTermination x) <&> \y -> x { _vcDeleteOnTermination = y }
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
vsaCode :: Lens' VolumeStatusAction (Maybe Text)
vsaCode f x =
    f (_vsaCode x) <&> \y -> x { _vsaCode = y }
{-# INLINE vsaCode #-}

-- | A description of the operation.
vsaDescription :: Lens' VolumeStatusAction (Maybe Text)
vsaDescription f x =
    f (_vsaDescription x) <&> \y -> x { _vsaDescription = y }
{-# INLINE vsaDescription #-}

-- | The event type associated with this operation.
vsaEventType :: Lens' VolumeStatusAction (Maybe Text)
vsaEventType f x =
    f (_vsaEventType x) <&> \y -> x { _vsaEventType = y }
{-# INLINE vsaEventType #-}

-- | The ID of the event associated with this operation.
vsaEventId :: Lens' VolumeStatusAction (Maybe Text)
vsaEventId f x =
    f (_vsaEventId x) <&> \y -> x { _vsaEventId = y }
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
vsdName :: Lens' VolumeStatusDetails (Maybe VolumeStatusName)
vsdName f x =
    f (_vsdName x) <&> \y -> x { _vsdName = y }
{-# INLINE vsdName #-}

-- | The intended status of the volume status.
vsdStatus :: Lens' VolumeStatusDetails (Maybe Text)
vsdStatus f x =
    f (_vsdStatus x) <&> \y -> x { _vsdStatus = y }
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
vseEventType :: Lens' VolumeStatusEvent (Maybe Text)
vseEventType f x =
    f (_vseEventType x) <&> \y -> x { _vseEventType = y }
{-# INLINE vseEventType #-}

-- | A description of the event.
vseDescription :: Lens' VolumeStatusEvent (Maybe Text)
vseDescription f x =
    f (_vseDescription x) <&> \y -> x { _vseDescription = y }
{-# INLINE vseDescription #-}

-- | The earliest start time of the event.
vseNotBefore :: Lens' VolumeStatusEvent (Maybe ISO8601)
vseNotBefore f x =
    f (_vseNotBefore x) <&> \y -> x { _vseNotBefore = y }
{-# INLINE vseNotBefore #-}

-- | The latest end time of the event.
vseNotAfter :: Lens' VolumeStatusEvent (Maybe ISO8601)
vseNotAfter f x =
    f (_vseNotAfter x) <&> \y -> x { _vseNotAfter = y }
{-# INLINE vseNotAfter #-}

-- | The ID of this event.
vseEventId :: Lens' VolumeStatusEvent (Maybe Text)
vseEventId f x =
    f (_vseEventId x) <&> \y -> x { _vseEventId = y }
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
vsjStatus :: Lens' VolumeStatusInfo (Maybe VolumeStatusInfoStatus)
vsjStatus f x =
    f (_vsjStatus x) <&> \y -> x { _vsjStatus = y }
{-# INLINE vsjStatus #-}

-- | The details of the volume status.
vsjDetails :: Lens' VolumeStatusInfo [VolumeStatusDetails]
vsjDetails f x =
    f (_vsjDetails x) <&> \y -> x { _vsjDetails = y }
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
vsiVolumeId :: Lens' VolumeStatusItem (Maybe Text)
vsiVolumeId f x =
    f (_vsiVolumeId x) <&> \y -> x { _vsiVolumeId = y }
{-# INLINE vsiVolumeId #-}

-- | The Availability Zone of the volume.
vsiAvailabilityZone :: Lens' VolumeStatusItem (Maybe Text)
vsiAvailabilityZone f x =
    f (_vsiAvailabilityZone x) <&> \y -> x { _vsiAvailabilityZone = y }
{-# INLINE vsiAvailabilityZone #-}

-- | The volume status.
vsiVolumeStatus :: Lens' VolumeStatusItem (Maybe VolumeStatusInfo)
vsiVolumeStatus f x =
    f (_vsiVolumeStatus x) <&> \y -> x { _vsiVolumeStatus = y }
{-# INLINE vsiVolumeStatus #-}

-- | A list of events associated with the volume.
vsiEvents :: Lens' VolumeStatusItem [VolumeStatusEvent]
vsiEvents f x =
    f (_vsiEvents x) <&> \y -> x { _vsiEvents = y }
{-# INLINE vsiEvents #-}

-- | The details of the operation.
vsiActions :: Lens' VolumeStatusItem [VolumeStatusAction]
vsiActions f x =
    f (_vsiActions x) <&> \y -> x { _vsiActions = y }
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
vdVpcId f x =
    f (_vdVpcId x) <&> \y -> x { _vdVpcId = y }
{-# INLINE vdVpcId #-}

-- | The current state of the VPC.
vdState :: Lens' Vpc (Maybe VpcState)
vdState f x =
    f (_vdState x) <&> \y -> x { _vdState = y }
{-# INLINE vdState #-}

-- | The CIDR block for the VPC.
vdCidrBlock :: Lens' Vpc (Maybe Text)
vdCidrBlock f x =
    f (_vdCidrBlock x) <&> \y -> x { _vdCidrBlock = y }
{-# INLINE vdCidrBlock #-}

-- | The ID of the set of DHCP options you've associated with the VPC (or
-- default if the default options are associated with the VPC).
vdDhcpOptionsId :: Lens' Vpc (Maybe Text)
vdDhcpOptionsId f x =
    f (_vdDhcpOptionsId x) <&> \y -> x { _vdDhcpOptionsId = y }
{-# INLINE vdDhcpOptionsId #-}

-- | Any tags assigned to the VPC.
vdTags :: Lens' Vpc [Tag]
vdTags f x =
    f (_vdTags x) <&> \y -> x { _vdTags = y }
{-# INLINE vdTags #-}

-- | The allowed tenancy of instances launched into the VPC.
vdInstanceTenancy :: Lens' Vpc (Maybe Tenancy)
vdInstanceTenancy f x =
    f (_vdInstanceTenancy x) <&> \y -> x { _vdInstanceTenancy = y }
{-# INLINE vdInstanceTenancy #-}

-- | Indicates whether the VPC is the default VPC.
vdIsDefault :: Lens' Vpc (Maybe Bool)
vdIsDefault f x =
    f (_vdIsDefault x) <&> \y -> x { _vdIsDefault = y }
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
vbVpcId f x =
    f (_vbVpcId x) <&> \y -> x { _vbVpcId = y }
{-# INLINE vbVpcId #-}

-- | The current state of the attachment.
vbState :: Lens' VpcAttachment (Maybe AttachmentStatus)
vbState f x =
    f (_vbState x) <&> \y -> x { _vbState = y }
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
vpcAccepterVpcInfo :: Lens' VpcPeeringConnection (Maybe VpcPeeringConnectionVpcInfo)
vpcAccepterVpcInfo f x =
    f (_vpcAccepterVpcInfo x) <&> \y -> x { _vpcAccepterVpcInfo = y }
{-# INLINE vpcAccepterVpcInfo #-}

-- | The time that an unaccepted VPC peering connection will expire.
vpcExpirationTime :: Lens' VpcPeeringConnection (Maybe ISO8601)
vpcExpirationTime f x =
    f (_vpcExpirationTime x) <&> \y -> x { _vpcExpirationTime = y }
{-# INLINE vpcExpirationTime #-}

-- | The information of the requester VPC.
vpcRequesterVpcInfo :: Lens' VpcPeeringConnection (Maybe VpcPeeringConnectionVpcInfo)
vpcRequesterVpcInfo f x =
    f (_vpcRequesterVpcInfo x) <&> \y -> x { _vpcRequesterVpcInfo = y }
{-# INLINE vpcRequesterVpcInfo #-}

-- | The status of the VPC peering connection.
vpcStatus :: Lens' VpcPeeringConnection (Maybe VpcPeeringConnectionStateReason)
vpcStatus f x =
    f (_vpcStatus x) <&> \y -> x { _vpcStatus = y }
{-# INLINE vpcStatus #-}

-- | Any tags assigned to the resource.
vpcTags :: Lens' VpcPeeringConnection [Tag]
vpcTags f x =
    f (_vpcTags x) <&> \y -> x { _vpcTags = y }
{-# INLINE vpcTags #-}

-- | The ID of the VPC peering connection.
vpcVpcPeeringConnectionId :: Lens' VpcPeeringConnection (Maybe Text)
vpcVpcPeeringConnectionId f x =
    f (_vpcVpcPeeringConnectionId x) <&> \y -> x { _vpcVpcPeeringConnectionId = y }
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
vpcsrCode f x =
    f (_vpcsrCode x) <&> \y -> x { _vpcsrCode = y }
{-# INLINE vpcsrCode #-}

-- | A message that provides more information about the status, if applicable.
vpcsrMessage :: Lens' VpcPeeringConnectionStateReason (Maybe Text)
vpcsrMessage f x =
    f (_vpcsrMessage x) <&> \y -> x { _vpcsrMessage = y }
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
vpcviCidrBlock :: Lens' VpcPeeringConnectionVpcInfo (Maybe Text)
vpcviCidrBlock f x =
    f (_vpcviCidrBlock x) <&> \y -> x { _vpcviCidrBlock = y }
{-# INLINE vpcviCidrBlock #-}

-- | The AWS account ID of the VPC owner.
vpcviOwnerId :: Lens' VpcPeeringConnectionVpcInfo (Maybe Text)
vpcviOwnerId f x =
    f (_vpcviOwnerId x) <&> \y -> x { _vpcviOwnerId = y }
{-# INLINE vpcviOwnerId #-}

-- | The ID of the VPC.
vpcviVpcId :: Lens' VpcPeeringConnectionVpcInfo (Maybe Text)
vpcviVpcId f x =
    f (_vpcviVpcId x) <&> \y -> x { _vpcviVpcId = y }
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
vvvvvvvvvvvvvvoVpnConnectionId :: Lens' VpnConnection (Maybe Text)
vvvvvvvvvvvvvvoVpnConnectionId f x =
    f (_vvvvvvvvvvvvvvoVpnConnectionId x) <&> \y -> x { _vvvvvvvvvvvvvvoVpnConnectionId = y }
{-# INLINE vvvvvvvvvvvvvvoVpnConnectionId #-}

-- | The current state of the VPN connection.
vvvvvvvvvvvvvvoState :: Lens' VpnConnection (Maybe VpnState)
vvvvvvvvvvvvvvoState f x =
    f (_vvvvvvvvvvvvvvoState x) <&> \y -> x { _vvvvvvvvvvvvvvoState = y }
{-# INLINE vvvvvvvvvvvvvvoState #-}

-- | The configuration information for the VPN connection's customer gateway (in
-- the native XML format). This element is always present in the
-- CreateVpnConnection response; however, it's present in the
-- DescribeVpnConnections response only if the VPN connection is in the
-- pending or available state.
vvvvvvvvvvvvvvoCustomerGatewayConfiguration :: Lens' VpnConnection (Maybe Text)
vvvvvvvvvvvvvvoCustomerGatewayConfiguration f x =
    f (_vvvvvvvvvvvvvvoCustomerGatewayConfiguration x) <&> \y -> x { _vvvvvvvvvvvvvvoCustomerGatewayConfiguration = y }
{-# INLINE vvvvvvvvvvvvvvoCustomerGatewayConfiguration #-}

-- | The type of VPN connection.
vvvvvvvvvvvvvvoType :: Lens' VpnConnection (Maybe GatewayType)
vvvvvvvvvvvvvvoType f x =
    f (_vvvvvvvvvvvvvvoType x) <&> \y -> x { _vvvvvvvvvvvvvvoType = y }
{-# INLINE vvvvvvvvvvvvvvoType #-}

-- | The ID of the customer gateway at your end of the VPN connection.
vvvvvvvvvvvvvvoCustomerGatewayId :: Lens' VpnConnection (Maybe Text)
vvvvvvvvvvvvvvoCustomerGatewayId f x =
    f (_vvvvvvvvvvvvvvoCustomerGatewayId x) <&> \y -> x { _vvvvvvvvvvvvvvoCustomerGatewayId = y }
{-# INLINE vvvvvvvvvvvvvvoCustomerGatewayId #-}

-- | The ID of the virtual private gateway at the AWS side of the VPN
-- connection.
vvvvvvvvvvvvvvoVpnGatewayId :: Lens' VpnConnection (Maybe Text)
vvvvvvvvvvvvvvoVpnGatewayId f x =
    f (_vvvvvvvvvvvvvvoVpnGatewayId x) <&> \y -> x { _vvvvvvvvvvvvvvoVpnGatewayId = y }
{-# INLINE vvvvvvvvvvvvvvoVpnGatewayId #-}

-- | Any tags assigned to the VPN connection.
vvvvvvvvvvvvvvoTags :: Lens' VpnConnection [Tag]
vvvvvvvvvvvvvvoTags f x =
    f (_vvvvvvvvvvvvvvoTags x) <&> \y -> x { _vvvvvvvvvvvvvvoTags = y }
{-# INLINE vvvvvvvvvvvvvvoTags #-}

-- | Information about the VPN tunnel.
vvvvvvvvvvvvvvoVgwTelemetry :: Lens' VpnConnection [VgwTelemetry]
vvvvvvvvvvvvvvoVgwTelemetry f x =
    f (_vvvvvvvvvvvvvvoVgwTelemetry x) <&> \y -> x { _vvvvvvvvvvvvvvoVgwTelemetry = y }
{-# INLINE vvvvvvvvvvvvvvoVgwTelemetry #-}

-- | The VPN connection options.
vvvvvvvvvvvvvvoOptions :: Lens' VpnConnection (Maybe VpnConnectionOptions)
vvvvvvvvvvvvvvoOptions f x =
    f (_vvvvvvvvvvvvvvoOptions x) <&> \y -> x { _vvvvvvvvvvvvvvoOptions = y }
{-# INLINE vvvvvvvvvvvvvvoOptions #-}

-- | The static routes associated with the VPN connection.
vvvvvvvvvvvvvvoRoutes :: Lens' VpnConnection [VpnStaticRoute]
vvvvvvvvvvvvvvoRoutes f x =
    f (_vvvvvvvvvvvvvvoRoutes x) <&> \y -> x { _vvvvvvvvvvvvvvoRoutes = y }
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
vvvvvvvvvvvvvvvyVpnGatewayId f x =
    f (_vvvvvvvvvvvvvvvyVpnGatewayId x) <&> \y -> x { _vvvvvvvvvvvvvvvyVpnGatewayId = y }
{-# INLINE vvvvvvvvvvvvvvvyVpnGatewayId #-}

-- | The current state of the virtual private gateway.
vvvvvvvvvvvvvvvyState :: Lens' VpnGateway (Maybe VpnState)
vvvvvvvvvvvvvvvyState f x =
    f (_vvvvvvvvvvvvvvvyState x) <&> \y -> x { _vvvvvvvvvvvvvvvyState = y }
{-# INLINE vvvvvvvvvvvvvvvyState #-}

-- | The type of VPN connection the virtual private gateway supports.
vvvvvvvvvvvvvvvyType :: Lens' VpnGateway (Maybe GatewayType)
vvvvvvvvvvvvvvvyType f x =
    f (_vvvvvvvvvvvvvvvyType x) <&> \y -> x { _vvvvvvvvvvvvvvvyType = y }
{-# INLINE vvvvvvvvvvvvvvvyType #-}

-- | The Availability Zone where the virtual private gateway was created.
vvvvvvvvvvvvvvvyAvailabilityZone :: Lens' VpnGateway (Maybe Text)
vvvvvvvvvvvvvvvyAvailabilityZone f x =
    f (_vvvvvvvvvvvvvvvyAvailabilityZone x) <&> \y -> x { _vvvvvvvvvvvvvvvyAvailabilityZone = y }
{-# INLINE vvvvvvvvvvvvvvvyAvailabilityZone #-}

-- | Any VPCs attached to the virtual private gateway.
vvvvvvvvvvvvvvvyVpcAttachments :: Lens' VpnGateway [VpcAttachment]
vvvvvvvvvvvvvvvyVpcAttachments f x =
    f (_vvvvvvvvvvvvvvvyVpcAttachments x) <&> \y -> x { _vvvvvvvvvvvvvvvyVpcAttachments = y }
{-# INLINE vvvvvvvvvvvvvvvyVpcAttachments #-}

-- | Any tags assigned to the virtual private gateway.
vvvvvvvvvvvvvvvyTags :: Lens' VpnGateway [Tag]
vvvvvvvvvvvvvvvyTags f x =
    f (_vvvvvvvvvvvvvvvyTags x) <&> \y -> x { _vvvvvvvvvvvvvvvyTags = y }
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
vsrDestinationCidrBlock f x =
    f (_vsrDestinationCidrBlock x) <&> \y -> x { _vsrDestinationCidrBlock = y }
{-# INLINE vsrDestinationCidrBlock #-}

-- | Indicates how the routes were provided.
vsrSource :: Lens' VpnStaticRoute (Maybe VpnStaticRouteSource)
vsrSource f x =
    f (_vsrSource x) <&> \y -> x { _vsrSource = y }
{-# INLINE vsrSource #-}

-- | The current state of the static route.
vsrState :: Lens' VpnStaticRoute (Maybe VpnState)
vsrState f x =
    f (_vsrState x) <&> \y -> x { _vsrState = y }
{-# INLINE vsrState #-}

instance FromXML VpnStaticRoute where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery VpnStaticRoute where
    toQuery = genericQuery def
