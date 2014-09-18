{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.Types
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
module Network.AWS.EC2.Types
    (
    -- * Service
      EC2
    -- ** Errors
    , EC2Error (..)
    , _EC2Client
    , _EC2Serializer
    , _EC2Service
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
    , av1Value

    -- * AvailabilityZoneMessage
    , AvailabilityZoneMessage
    , mkAvailabilityZoneMessage
    , azmMessage

    -- * IpRange
    , IpRange
    , mkIpRange
    , irCidrIp

    -- * Monitoring
    , Monitoring
    , mkMonitoring
    , mState

    -- * PropagatingVgw
    , PropagatingVgw
    , mkPropagatingVgw
    , pvGatewayId

    -- * ReservedInstancesId
    , ReservedInstancesId
    , mkReservedInstancesId
    , riiReservedInstancesId

    -- * RunInstancesMonitoringEnabled
    , RunInstancesMonitoringEnabled
    , mkRunInstancesMonitoringEnabled
    , rimeEnabled

    -- * Storage
    , Storage
    , mkStorage
    , sS3

    -- * VolumeDetail
    , VolumeDetail
    , mkVolumeDetail
    , vdSize

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
    , mkAccountAttribute
    , aa1rAttributeName
    , aa1rAttributeValues

    -- * Address
    , Address
    , mkAddress
    , aInstanceId
    , aPublicIp
    , aAllocationId
    , aAssociationId
    , aDomain
    , aNetworkInterfaceId
    , aNetworkInterfaceOwnerId
    , aPrivateIpAddress

    -- * AvailabilityZone
    , AvailabilityZone
    , mkAvailabilityZone
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
    , mkBundleTask
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
    , mkCancelledSpotInstanceRequest
    , csir1SpotInstanceRequestId
    , csir1State

    -- * ConversionTask
    , ConversionTask
    , mkConversionTask
    , ctrConversionTaskId
    , ctrExpirationTime
    , ctrImportInstance
    , ctrImportVolume
    , ctrState
    , ctrStatusMessage
    , ctrTags

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
    , mkCustomerGateway
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
    , mkDhcpOptions
    , doDhcpOptionsId
    , doDhcpConfigurations
    , doTags

    -- * DiskImage
    , DiskImage
    , mkDiskImage
    , di3Image
    , di3Description
    , di3Volume

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
    , did1Format
    , did1Bytes
    , did1ImportManifestUrl

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
    , mkExportTask
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
    , fName
    , fValues

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
    , mkImage
    , iImageId
    , iImageLocation
    , iState
    , iOwnerId
    , iPublic
    , iProductCodes
    , iArchitecture
    , iImageType
    , iKernelId
    , iRamdiskId
    , iPlatform
    , iSriovNetSupport
    , iStateReason
    , iImageOwnerAlias
    , iName
    , iDescription
    , iRootDeviceType
    , iRootDeviceName
    , iBlockDeviceMappings
    , iVirtualizationType
    , iTags
    , iHypervisor

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
    , i1InstanceId
    , i1ImageId
    , i1State
    , i1PrivateDnsName
    , i1PublicDnsName
    , i1StateTransitionReason
    , i1KeyName
    , i1AmiLaunchIndex
    , i1ProductCodes
    , i1InstanceType
    , i1LaunchTime
    , i1Placement
    , i1KernelId
    , i1RamdiskId
    , i1Platform
    , i1Monitoring
    , i1SubnetId
    , i1VpcId
    , i1PrivateIpAddress
    , i1PublicIpAddress
    , i1StateReason
    , i1Architecture
    , i1RootDeviceType
    , i1RootDeviceName
    , i1BlockDeviceMappings
    , i1VirtualizationType
    , i1InstanceLifecycle
    , i1SpotInstanceRequestId
    , i1ClientToken
    , i1Tags
    , i1SecurityGroups
    , i1SourceDestCheck
    , i1Hypervisor
    , i1NetworkInterfaces
    , i1IamInstanceProfile
    , i1EbsOptimized
    , i1SriovNetSupport

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
    , mkInstanceMonitoring
    , imInstanceId
    , imMonitoring

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
    , inia1PublicIp
    , inia1PublicDnsName
    , inia1IpOwnerId

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
    , is1Code
    , is1Name

    -- * InstanceStateChange
    , InstanceStateChange
    , mkInstanceStateChange
    , iscInstanceId
    , iscCurrentState
    , iscPreviousState

    -- * InstanceStatus
    , InstanceStatus
    , mkInstanceStatus
    , isInstanceId
    , isAvailabilityZone
    , isEvents
    , isInstanceState
    , isSystemStatus
    , isInstanceStatus

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
    , mkInternetGateway
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
    , mkKeyPairInfo
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
    , lsImageId
    , lsKeyName
    , lsSecurityGroups
    , lsUserData
    , lsAddressingType
    , lsInstanceType
    , lsPlacement
    , lsKernelId
    , lsRamdiskId
    , lsBlockDeviceMappings
    , lsMonitoringEnabled
    , lsSubnetId
    , lsNetworkInterfaces
    , lsIamInstanceProfile
    , lsEbsOptimized

    -- * NetworkAcl
    , NetworkAcl
    , mkNetworkAcl
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
    , mkNetworkInterface
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
    , niarPublicIp
    , niarPublicDnsName
    , niarIpOwnerId
    , niarAllocationId
    , niarAssociationId

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
    , pAvailabilityZone
    , pGroupName
    , pTenancy

    -- * PlacementGroup
    , PlacementGroup
    , mkPlacementGroup
    , pgGroupName
    , pgStrategy
    , pgState

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
    , pdPrice
    , pdCount

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
    , rcFrequency
    , rcAmount

    -- * Region
    , Region
    , mkRegion
    , r1RegionName
    , r1Endpoint

    -- * Reservation
    , Reservation
    , mkReservation
    , rrReservationId
    , rrOwnerId
    , rrRequesterId
    , rrGroups
    , rrInstances

    -- * ReservedInstanceLimitPrice
    , ReservedInstanceLimitPrice
    , mkReservedInstanceLimitPrice
    , rilpAmount
    , rilpCurrencyCode

    -- * ReservedInstances
    , ReservedInstances
    , mkReservedInstances
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
    , mkReservedInstancesListing
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
    , mkReservedInstancesModification
    , rimReservedInstancesModificationId
    , rimReservedInstancesIds
    , rimModificationResults
    , rimCreateDate
    , rimUpdateDate
    , rimEffectiveDate
    , rimStatus
    , rimStatusMessage
    , rimClientToken

    -- * ReservedInstancesModificationResult
    , ReservedInstancesModificationResult
    , mkReservedInstancesModificationResult
    , rimrReservedInstancesId
    , rimrTargetConfiguration

    -- * ReservedInstancesOffering
    , ReservedInstancesOffering
    , mkReservedInstancesOffering
    , rioReservedInstancesOfferingId
    , rioInstanceType
    , rioAvailabilityZone
    , rioDuration
    , rioUsagePrice
    , rioFixedPrice
    , rioProductDescription
    , rioInstanceTenancy
    , rioCurrencyCode
    , rioOfferingType
    , rioRecurringCharges
    , rioMarketplace
    , rioPricingDetails

    -- * Route
    , Route
    , mkRoute
    , rDestinationCidrBlock
    , rGatewayId
    , rInstanceId
    , rInstanceOwnerId
    , rNetworkInterfaceId
    , rVpcPeeringConnectionId
    , rState
    , rOrigin

    -- * RouteTable
    , RouteTable
    , mkRouteTable
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
    , mkSecurityGroup
    , sgOwnerId
    , sgGroupName
    , sgGroupId
    , sgDescription
    , sgIpPermissions
    , sgIpPermissionsEgress
    , sgVpcId
    , sgTags

    -- * Snapshot
    , Snapshot
    , mkSnapshot
    , s1SnapshotId
    , s1VolumeId
    , s1State
    , s1StartTime
    , s1Progress
    , s1OwnerId
    , s1Description
    , s1VolumeSize
    , s1OwnerAlias
    , s1Tags
    , s1Encrypted

    -- * SpotDatafeedSubscription
    , SpotDatafeedSubscription
    , mkSpotDatafeedSubscription
    , sdsOwnerId
    , sdsBucket
    , sdsPrefix
    , sdsState
    , sdsFault

    -- * SpotInstanceRequest
    , SpotInstanceRequest
    , mkSpotInstanceRequest
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
    , sisCode
    , sisUpdateTime
    , sisMessage

    -- * SpotPlacement
    , SpotPlacement
    , mkSpotPlacement
    , spAvailabilityZone
    , spGroupName

    -- * SpotPrice
    , SpotPrice
    , mkSpotPrice
    , sprInstanceType
    , sprProductDescription
    , sprSpotPrice
    , sprTimestamp
    , sprAvailabilityZone

    -- * StateReason
    , StateReason
    , mkStateReason
    , sr1Code
    , sr1Message

    -- * Subnet
    , Subnet
    , mkSubnet
    , srSubnetId
    , srState
    , srVpcId
    , srCidrBlock
    , srAvailableIpAddressCount
    , srAvailabilityZone
    , srDefaultForAz
    , srMapPublicIpOnLaunch
    , srTags

    -- * Tag
    , Tag
    , mkTag
    , tKey
    , tValue

    -- * TagDescription
    , TagDescription
    , mkTagDescription
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
    , vtOutsideIpAddress
    , vtStatus
    , vtLastStatusChange
    , vtStatusMessage
    , vtAcceptedRouteCount

    -- * Volume
    , Volume
    , mkVolume
    , vrVolumeId
    , vrSize
    , vrSnapshotId
    , vrAvailabilityZone
    , vrState
    , vrCreateTime
    , vrAttachments
    , vrTags
    , vrVolumeType
    , vrIops
    , vrEncrypted

    -- * VolumeAttachment
    , VolumeAttachment
    , mkVolumeAttachment
    , varVolumeId
    , varInstanceId
    , varDevice
    , varState
    , varAttachTime
    , varDeleteOnTermination

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
    , vsi1Status
    , vsi1Details

    -- * VolumeStatusItem
    , VolumeStatusItem
    , mkVolumeStatusItem
    , vsiVolumeId
    , vsiAvailabilityZone
    , vsiVolumeStatus
    , vsiEvents
    , vsiActions

    -- * Vpc
    , Vpc
    , mkVpc
    , vVpcId
    , vState
    , vCidrBlock
    , vDhcpOptionsId
    , vTags
    , vInstanceTenancy
    , vIsDefault

    -- * VpcAttachment
    , VpcAttachment
    , mkVpcAttachment
    , vaVpcId
    , vaState

    -- * VpcPeeringConnection
    , VpcPeeringConnection
    , mkVpcPeeringConnection
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
    , mkVpnConnection
    , vcVpnConnectionId
    , vcState
    , vcCustomerGatewayConfiguration
    , vcType
    , vcCustomerGatewayId
    , vcVpnGatewayId
    , vcTags
    , vcVgwTelemetry
    , vcOptions
    , vcRoutes

    -- * VpnGateway
    , VpnGateway
    , mkVpnGateway
    , vgVpnGatewayId
    , vgState
    , vgType
    , vgAvailabilityZone
    , vgVpcAttachments
    , vgTags

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
    type Er EC2 = EC2Error

    service = Service'
        { _svcEndpoint = Regional
        , _svcPrefix   = "ec2"
        , _svcVersion  = "2014-06-15"
        , _svcTarget   = Nothing
        }

-- | A sum type representing possible errors returned by the 'EC2' service.
--
-- These typically include 'HTTPException's thrown by the underlying HTTP
-- mechanisms, serialisation errors, and typed errors as specified by the
-- service description where applicable.
data EC2Error
    = EC2Client HttpException
    | EC2Serializer Text
    | EC2Service Text
    deriving (Show, Generic)

instance AWSError EC2Error where
    awsError = const "EC2Error"

instance AWSServiceError EC2Error where
    serviceError    = EC2Service
    clientError     = EC2Client
    serializerError = EC2Serializer

instance Exception EC2Error

-- | See: 'EC2Client'
_EC2Client :: Prism' EC2Error HttpException
_EC2Client = prism'
    EC2Client
    (\case
        EC2Client p1 -> Right p1
        x -> Left x)

-- | See: 'EC2Serializer'
_EC2Serializer :: Prism' EC2Error Text
_EC2Serializer = prism'
    EC2Serializer
    (\case
        EC2Serializer p1 -> Right p1
        x -> Left x)

-- | See: 'EC2Service'
_EC2Service :: Prism' EC2Error Text
_EC2Service = prism'
    EC2Service
    (\case
        EC2Service p1 -> Right p1
        x -> Left x)

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def

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
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AccountAttributeValue' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AttributeValue ::@ @Maybe Text@
--
mkAccountAttributeValue :: AccountAttributeValue
mkAccountAttributeValue = AccountAttributeValue
    { _aavAttributeValue = Nothing
    }

-- | The value.
aavAttributeValue :: Lens' AccountAttributeValue (Maybe Text)
aavAttributeValue =
    lens _aavAttributeValue (\s a -> s { _aavAttributeValue = a })

instance FromXML AccountAttributeValue where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery AccountAttributeValue where
    toQuery = genericQuery def

-- | If the value is true, you can't terminate the instance through the Amazon
-- EC2 console, CLI, or API; otherwise, you can.
newtype AttributeBooleanValue = AttributeBooleanValue
    { _abvValue :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AttributeBooleanValue' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Value ::@ @Maybe Bool@
--
mkAttributeBooleanValue :: AttributeBooleanValue
mkAttributeBooleanValue = AttributeBooleanValue
    { _abvValue = Nothing
    }

-- | 
abvValue :: Lens' AttributeBooleanValue (Maybe Bool)
abvValue = lens _abvValue (\s a -> s { _abvValue = a })

instance FromXML AttributeBooleanValue where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "disableApiTermination"

instance ToQuery AttributeBooleanValue where
    toQuery = genericQuery def

-- | The kernel ID.
newtype AttributeValue = AttributeValue
    { _av1Value :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AttributeValue' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Value ::@ @Maybe Text@
--
mkAttributeValue :: AttributeValue
mkAttributeValue = AttributeValue
    { _av1Value = Nothing
    }

-- | 
av1Value :: Lens' AttributeValue (Maybe Text)
av1Value = lens _av1Value (\s a -> s { _av1Value = a })

instance FromXML AttributeValue where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "kernel"

instance ToQuery AttributeValue where
    toQuery = genericQuery def

-- | Describes a message about an Availability Zone.
newtype AvailabilityZoneMessage = AvailabilityZoneMessage
    { _azmMessage :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AvailabilityZoneMessage' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Message ::@ @Maybe Text@
--
mkAvailabilityZoneMessage :: AvailabilityZoneMessage
mkAvailabilityZoneMessage = AvailabilityZoneMessage
    { _azmMessage = Nothing
    }

-- | The message about the Availability Zone.
azmMessage :: Lens' AvailabilityZoneMessage (Maybe Text)
azmMessage = lens _azmMessage (\s a -> s { _azmMessage = a })

instance FromXML AvailabilityZoneMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery AvailabilityZoneMessage where
    toQuery = genericQuery def

-- | Describes an IP range.
newtype IpRange = IpRange
    { _irCidrIp :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'IpRange' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CidrIp ::@ @Text@
--
mkIpRange :: Text -- ^ 'irCidrIp'
          -> IpRange
mkIpRange p1 = IpRange
    { _irCidrIp = p1
    }

-- | The CIDR range. You can either specify a CIDR range or a source security
-- group, not both.
irCidrIp :: Lens' IpRange Text
irCidrIp = lens _irCidrIp (\s a -> s { _irCidrIp = a })

instance FromXML IpRange where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "IpRange"

instance ToQuery IpRange where
    toQuery = genericQuery def

-- | The monitoring information for the instance.
newtype Monitoring = Monitoring
    { _mState :: Maybe MonitoringState
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Monitoring' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @State ::@ @Maybe MonitoringState@
--
mkMonitoring :: Monitoring
mkMonitoring = Monitoring
    { _mState = Nothing
    }

-- | Indicates whether monitoring is enabled for the instance.
mState :: Lens' Monitoring (Maybe MonitoringState)
mState = lens _mState (\s a -> s { _mState = a })

instance FromXML Monitoring where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "monitoring"

instance ToQuery Monitoring where
    toQuery = genericQuery def

-- | Describes a virtual private gateway propagating route.
newtype PropagatingVgw = PropagatingVgw
    { _pvGatewayId :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'PropagatingVgw' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GatewayId ::@ @Maybe Text@
--
mkPropagatingVgw :: PropagatingVgw
mkPropagatingVgw = PropagatingVgw
    { _pvGatewayId = Nothing
    }

-- | The ID of the virtual private gateway (VGW).
pvGatewayId :: Lens' PropagatingVgw (Maybe Text)
pvGatewayId = lens _pvGatewayId (\s a -> s { _pvGatewayId = a })

instance FromXML PropagatingVgw where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery PropagatingVgw where
    toQuery = genericQuery def

-- | Describes the ID of a Reserved Instance.
newtype ReservedInstancesId = ReservedInstancesId
    { _riiReservedInstancesId :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ReservedInstancesId' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ReservedInstancesId ::@ @Maybe Text@
--
mkReservedInstancesId :: ReservedInstancesId
mkReservedInstancesId = ReservedInstancesId
    { _riiReservedInstancesId = Nothing
    }

-- | The ID of the Reserved Instance.
riiReservedInstancesId :: Lens' ReservedInstancesId (Maybe Text)
riiReservedInstancesId =
    lens _riiReservedInstancesId (\s a -> s { _riiReservedInstancesId = a })

instance FromXML ReservedInstancesId where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery ReservedInstancesId where
    toQuery = genericQuery def

-- | The monitoring for the instance.
newtype RunInstancesMonitoringEnabled = RunInstancesMonitoringEnabled
    { _rimeEnabled :: Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'RunInstancesMonitoringEnabled' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Enabled ::@ @Bool@
--
mkRunInstancesMonitoringEnabled :: Bool -- ^ 'rimeEnabled'
                                -> RunInstancesMonitoringEnabled
mkRunInstancesMonitoringEnabled p1 = RunInstancesMonitoringEnabled
    { _rimeEnabled = p1
    }

-- | Indicates whether monitoring is enabled for the instance.
rimeEnabled :: Lens' RunInstancesMonitoringEnabled Bool
rimeEnabled = lens _rimeEnabled (\s a -> s { _rimeEnabled = a })

instance ToQuery RunInstancesMonitoringEnabled where
    toQuery = genericQuery def

-- | The bucket in which to store the AMI. You can specify a bucket that you
-- already own or a new bucket that Amazon EC2 creates on your behalf. If you
-- specify a bucket that belongs to someone else, Amazon EC2 returns an error.
newtype Storage = Storage
    { _sS3 :: Maybe S3Storage
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Storage' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @S3 ::@ @Maybe S3Storage@
--
mkStorage :: Storage
mkStorage = Storage
    { _sS3 = Nothing
    }

-- | An Amazon S3 storage location.
sS3 :: Lens' Storage (Maybe S3Storage)
sS3 = lens _sS3 (\s a -> s { _sS3 = a })

instance FromXML Storage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Storage"

instance ToQuery Storage where
    toQuery = genericQuery def

newtype VolumeDetail = VolumeDetail
    { _vdSize :: Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VolumeDetail' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Size ::@ @Integer@
--
mkVolumeDetail :: Integer -- ^ 'vdSize'
               -> VolumeDetail
mkVolumeDetail p1 = VolumeDetail
    { _vdSize = p1
    }

-- | The size of the volume, in GiB.
vdSize :: Lens' VolumeDetail Integer
vdSize = lens _vdSize (\s a -> s { _vdSize = a })

instance FromXML VolumeDetail where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "VolumeDetail"

instance ToQuery VolumeDetail where
    toQuery = genericQuery def

-- | The VPN connection options.
newtype VpnConnectionOptions = VpnConnectionOptions
    { _vcoStaticRoutesOnly :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VpnConnectionOptions' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StaticRoutesOnly ::@ @Maybe Bool@
--
mkVpnConnectionOptions :: VpnConnectionOptions
mkVpnConnectionOptions = VpnConnectionOptions
    { _vcoStaticRoutesOnly = Nothing
    }

-- | Indicates whether the VPN connection uses static routes only. Static routes
-- must be used for devices that don't support BGP.
vcoStaticRoutesOnly :: Lens' VpnConnectionOptions (Maybe Bool)
vcoStaticRoutesOnly =
    lens _vcoStaticRoutesOnly (\s a -> s { _vcoStaticRoutesOnly = a })

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
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VpnConnectionOptionsSpecification' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StaticRoutesOnly ::@ @Maybe Bool@
--
mkVpnConnectionOptionsSpecification :: VpnConnectionOptionsSpecification
mkVpnConnectionOptionsSpecification = VpnConnectionOptionsSpecification
    { _vcosStaticRoutesOnly = Nothing
    }

-- | Indicates whether the VPN connection uses static routes only. Static routes
-- must be used for devices that don't support BGP.
vcosStaticRoutesOnly :: Lens' VpnConnectionOptionsSpecification (Maybe Bool)
vcosStaticRoutesOnly =
    lens _vcosStaticRoutesOnly (\s a -> s { _vcosStaticRoutesOnly = a })

instance ToQuery VpnConnectionOptionsSpecification where
    toQuery = genericQuery def

-- | Describes an account attribute.
data AccountAttribute = AccountAttribute
    { _aa1rAttributeName :: Maybe Text
    , _aa1rAttributeValues :: [AccountAttributeValue]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AccountAttribute' data type.
--
-- 'AccountAttribute' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AttributeName ::@ @Maybe Text@
--
-- * @AttributeValues ::@ @[AccountAttributeValue]@
--
mkAccountAttribute :: AccountAttribute
mkAccountAttribute = AccountAttribute
    { _aa1rAttributeName = Nothing
    , _aa1rAttributeValues = mempty
    }

-- | The name of the account attribute.
aa1rAttributeName :: Lens' AccountAttribute (Maybe Text)
aa1rAttributeName =
    lens _aa1rAttributeName (\s a -> s { _aa1rAttributeName = a })

-- | One or more values for the account attribute.
aa1rAttributeValues :: Lens' AccountAttribute [AccountAttributeValue]
aa1rAttributeValues =
    lens _aa1rAttributeValues (\s a -> s { _aa1rAttributeValues = a })

instance FromXML AccountAttribute where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes an Elastic IP address.
data Address = Address
    { _aInstanceId :: Maybe Text
    , _aPublicIp :: Maybe Text
    , _aAllocationId :: Maybe Text
    , _aAssociationId :: Maybe Text
    , _aDomain :: Maybe DomainType
    , _aNetworkInterfaceId :: Maybe Text
    , _aNetworkInterfaceOwnerId :: Maybe Text
    , _aPrivateIpAddress :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Address' data type.
--
-- 'Address' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InstanceId ::@ @Maybe Text@
--
-- * @PublicIp ::@ @Maybe Text@
--
-- * @AllocationId ::@ @Maybe Text@
--
-- * @AssociationId ::@ @Maybe Text@
--
-- * @Domain ::@ @Maybe DomainType@
--
-- * @NetworkInterfaceId ::@ @Maybe Text@
--
-- * @NetworkInterfaceOwnerId ::@ @Maybe Text@
--
-- * @PrivateIpAddress ::@ @Maybe Text@
--
mkAddress :: Address
mkAddress = Address
    { _aInstanceId = Nothing
    , _aPublicIp = Nothing
    , _aAllocationId = Nothing
    , _aAssociationId = Nothing
    , _aDomain = Nothing
    , _aNetworkInterfaceId = Nothing
    , _aNetworkInterfaceOwnerId = Nothing
    , _aPrivateIpAddress = Nothing
    }

-- | The ID of the instance the address is associated with (if any).
aInstanceId :: Lens' Address (Maybe Text)
aInstanceId = lens _aInstanceId (\s a -> s { _aInstanceId = a })

-- | The Elastic IP address.
aPublicIp :: Lens' Address (Maybe Text)
aPublicIp = lens _aPublicIp (\s a -> s { _aPublicIp = a })

-- | The ID representing the allocation of the address for use with EC2-VPC.
aAllocationId :: Lens' Address (Maybe Text)
aAllocationId = lens _aAllocationId (\s a -> s { _aAllocationId = a })

-- | The ID representing the association of the address with an instance in a
-- VPC.
aAssociationId :: Lens' Address (Maybe Text)
aAssociationId = lens _aAssociationId (\s a -> s { _aAssociationId = a })

-- | Indicates whether this Elastic IP address is for use with instances in
-- EC2-Classic (standard) or instances in a VPC (vpc).
aDomain :: Lens' Address (Maybe DomainType)
aDomain = lens _aDomain (\s a -> s { _aDomain = a })

-- | The ID of the network interface.
aNetworkInterfaceId :: Lens' Address (Maybe Text)
aNetworkInterfaceId =
    lens _aNetworkInterfaceId (\s a -> s { _aNetworkInterfaceId = a })

-- | The ID of the AWS account that owns the network interface.
aNetworkInterfaceOwnerId :: Lens' Address (Maybe Text)
aNetworkInterfaceOwnerId =
    lens _aNetworkInterfaceOwnerId
         (\s a -> s { _aNetworkInterfaceOwnerId = a })

-- | The private IP address associated with the Elastic IP address.
aPrivateIpAddress :: Lens' Address (Maybe Text)
aPrivateIpAddress =
    lens _aPrivateIpAddress (\s a -> s { _aPrivateIpAddress = a })

instance FromXML Address where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes an Availability Zone.
data AvailabilityZone = AvailabilityZone
    { _azZoneName :: Maybe Text
    , _azState :: Maybe AvailabilityZoneState
    , _azRegionName :: Maybe Text
    , _azMessages :: [AvailabilityZoneMessage]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AvailabilityZone' data type.
--
-- 'AvailabilityZone' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ZoneName ::@ @Maybe Text@
--
-- * @State ::@ @Maybe AvailabilityZoneState@
--
-- * @RegionName ::@ @Maybe Text@
--
-- * @Messages ::@ @[AvailabilityZoneMessage]@
--
mkAvailabilityZone :: AvailabilityZone
mkAvailabilityZone = AvailabilityZone
    { _azZoneName = Nothing
    , _azState = Nothing
    , _azRegionName = Nothing
    , _azMessages = mempty
    }

-- | The name of the Availability Zone.
azZoneName :: Lens' AvailabilityZone (Maybe Text)
azZoneName = lens _azZoneName (\s a -> s { _azZoneName = a })

-- | The state of the Availability Zone.
azState :: Lens' AvailabilityZone (Maybe AvailabilityZoneState)
azState = lens _azState (\s a -> s { _azState = a })

-- | The name of the region.
azRegionName :: Lens' AvailabilityZone (Maybe Text)
azRegionName = lens _azRegionName (\s a -> s { _azRegionName = a })

-- | Any messages about the Availability Zone.
azMessages :: Lens' AvailabilityZone [AvailabilityZoneMessage]
azMessages = lens _azMessages (\s a -> s { _azMessages = a })

instance FromXML AvailabilityZone where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes a block device mapping.
data BlockDeviceMapping = BlockDeviceMapping
    { _bdmVirtualName :: Maybe Text
    , _bdmDeviceName :: Text
    , _bdmEbs :: Maybe EbsBlockDevice
    , _bdmNoDevice :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'BlockDeviceMapping' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VirtualName ::@ @Maybe Text@
--
-- * @DeviceName ::@ @Text@
--
-- * @Ebs ::@ @Maybe EbsBlockDevice@
--
-- * @NoDevice ::@ @Maybe Text@
--
mkBlockDeviceMapping :: Text -- ^ 'bdmDeviceName'
                     -> BlockDeviceMapping
mkBlockDeviceMapping p2 = BlockDeviceMapping
    { _bdmVirtualName = Nothing
    , _bdmDeviceName = p2
    , _bdmEbs = Nothing
    , _bdmNoDevice = Nothing
    }

-- | The virtual device name.
bdmVirtualName :: Lens' BlockDeviceMapping (Maybe Text)
bdmVirtualName = lens _bdmVirtualName (\s a -> s { _bdmVirtualName = a })

-- | The device name exposed to the instance (for example, /dev/sdh).
bdmDeviceName :: Lens' BlockDeviceMapping Text
bdmDeviceName = lens _bdmDeviceName (\s a -> s { _bdmDeviceName = a })

-- | Parameters used to automatically set up Amazon EBS volumes when the
-- instance is launched.
bdmEbs :: Lens' BlockDeviceMapping (Maybe EbsBlockDevice)
bdmEbs = lens _bdmEbs (\s a -> s { _bdmEbs = a })

-- | Suppresses the specified device included in the block device mapping of the
-- AMI.
bdmNoDevice :: Lens' BlockDeviceMapping (Maybe Text)
bdmNoDevice = lens _bdmNoDevice (\s a -> s { _bdmNoDevice = a })

instance FromXML BlockDeviceMapping where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "BlockDeviceMapping"

instance ToQuery BlockDeviceMapping where
    toQuery = genericQuery def

-- | Information about the bundle task.
data BundleTask = BundleTask
    { _btInstanceId :: Maybe Text
    , _btBundleId :: Maybe Text
    , _btState :: Maybe BundleTaskState
    , _btStartTime :: Maybe ISO8601
    , _btUpdateTime :: Maybe ISO8601
    , _btStorage :: Maybe Storage
    , _btProgress :: Maybe Text
    , _btBundleTaskError :: Maybe BundleTaskError
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'BundleTask' data type.
--
-- 'BundleTask' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InstanceId ::@ @Maybe Text@
--
-- * @BundleId ::@ @Maybe Text@
--
-- * @State ::@ @Maybe BundleTaskState@
--
-- * @StartTime ::@ @Maybe ISO8601@
--
-- * @UpdateTime ::@ @Maybe ISO8601@
--
-- * @Storage ::@ @Maybe Storage@
--
-- * @Progress ::@ @Maybe Text@
--
-- * @BundleTaskError ::@ @Maybe BundleTaskError@
--
mkBundleTask :: BundleTask
mkBundleTask = BundleTask
    { _btInstanceId = Nothing
    , _btBundleId = Nothing
    , _btState = Nothing
    , _btStartTime = Nothing
    , _btUpdateTime = Nothing
    , _btStorage = Nothing
    , _btProgress = Nothing
    , _btBundleTaskError = Nothing
    }

-- | The ID of the instance associated with this bundle task.
btInstanceId :: Lens' BundleTask (Maybe Text)
btInstanceId = lens _btInstanceId (\s a -> s { _btInstanceId = a })

-- | The ID for this bundle task.
btBundleId :: Lens' BundleTask (Maybe Text)
btBundleId = lens _btBundleId (\s a -> s { _btBundleId = a })

-- | The state of the task.
btState :: Lens' BundleTask (Maybe BundleTaskState)
btState = lens _btState (\s a -> s { _btState = a })

-- | The time this task started.
btStartTime :: Lens' BundleTask (Maybe ISO8601)
btStartTime = lens _btStartTime (\s a -> s { _btStartTime = a })

-- | The time of the most recent update for the task.
btUpdateTime :: Lens' BundleTask (Maybe ISO8601)
btUpdateTime = lens _btUpdateTime (\s a -> s { _btUpdateTime = a })

-- | The Amazon S3 storage locations.
btStorage :: Lens' BundleTask (Maybe Storage)
btStorage = lens _btStorage (\s a -> s { _btStorage = a })

-- | The level of task completion, as a percent (for example, 20%).
btProgress :: Lens' BundleTask (Maybe Text)
btProgress = lens _btProgress (\s a -> s { _btProgress = a })

-- | If the task fails, a description of the error.
btBundleTaskError :: Lens' BundleTask (Maybe BundleTaskError)
btBundleTaskError =
    lens _btBundleTaskError (\s a -> s { _btBundleTaskError = a })

instance FromXML BundleTask where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "bundleInstanceTask"

-- | If the task fails, a description of the error.
data BundleTaskError = BundleTaskError
    { _bteCode :: Maybe Text
    , _bteMessage :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'BundleTaskError' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Code ::@ @Maybe Text@
--
-- * @Message ::@ @Maybe Text@
--
mkBundleTaskError :: BundleTaskError
mkBundleTaskError = BundleTaskError
    { _bteCode = Nothing
    , _bteMessage = Nothing
    }

-- | The error code.
bteCode :: Lens' BundleTaskError (Maybe Text)
bteCode = lens _bteCode (\s a -> s { _bteCode = a })

-- | The error message.
bteMessage :: Lens' BundleTaskError (Maybe Text)
bteMessage = lens _bteMessage (\s a -> s { _bteMessage = a })

instance FromXML BundleTaskError where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "error"

instance ToQuery BundleTaskError where
    toQuery = genericQuery def

-- | Describes a request to cancel a Spot Instance.
data CancelledSpotInstanceRequest = CancelledSpotInstanceRequest
    { _csir1SpotInstanceRequestId :: Maybe Text
    , _csir1State :: Maybe CancelSpotInstanceRequestState
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CancelledSpotInstanceRequest' data type.
--
-- 'CancelledSpotInstanceRequest' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SpotInstanceRequestId ::@ @Maybe Text@
--
-- * @State ::@ @Maybe CancelSpotInstanceRequestState@
--
mkCancelledSpotInstanceRequest :: CancelledSpotInstanceRequest
mkCancelledSpotInstanceRequest = CancelledSpotInstanceRequest
    { _csir1SpotInstanceRequestId = Nothing
    , _csir1State = Nothing
    }

-- | The ID of the Spot Instance request.
csir1SpotInstanceRequestId :: Lens' CancelledSpotInstanceRequest (Maybe Text)
csir1SpotInstanceRequestId =
    lens _csir1SpotInstanceRequestId
         (\s a -> s { _csir1SpotInstanceRequestId = a })

-- | The state of the Spot Instance request.
csir1State :: Lens' CancelledSpotInstanceRequest (Maybe CancelSpotInstanceRequestState)
csir1State = lens _csir1State (\s a -> s { _csir1State = a })

instance FromXML CancelledSpotInstanceRequest where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes a conversion task.
data ConversionTask = ConversionTask
    { _ctrConversionTaskId :: Text
    , _ctrExpirationTime :: Maybe Text
    , _ctrImportInstance :: Maybe ImportInstanceTaskDetails
    , _ctrImportVolume :: Maybe ImportVolumeTaskDetails
    , _ctrState :: ConversionTaskState
    , _ctrStatusMessage :: Maybe Text
    , _ctrTags :: [Tag]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ConversionTask' data type.
--
-- 'ConversionTask' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ConversionTaskId ::@ @Text@
--
-- * @ExpirationTime ::@ @Maybe Text@
--
-- * @ImportInstance ::@ @Maybe ImportInstanceTaskDetails@
--
-- * @ImportVolume ::@ @Maybe ImportVolumeTaskDetails@
--
-- * @State ::@ @ConversionTaskState@
--
-- * @StatusMessage ::@ @Maybe Text@
--
-- * @Tags ::@ @[Tag]@
--
mkConversionTask :: Text -- ^ 'ctrConversionTaskId'
                 -> ConversionTaskState -- ^ 'ctrState'
                 -> ConversionTask
mkConversionTask p1 p5 = ConversionTask
    { _ctrConversionTaskId = p1
    , _ctrExpirationTime = Nothing
    , _ctrImportInstance = Nothing
    , _ctrImportVolume = Nothing
    , _ctrState = p5
    , _ctrStatusMessage = Nothing
    , _ctrTags = mempty
    }

-- | The ID of the conversion task.
ctrConversionTaskId :: Lens' ConversionTask Text
ctrConversionTaskId =
    lens _ctrConversionTaskId (\s a -> s { _ctrConversionTaskId = a })

-- | The time when the task expires. If the upload isn't complete before the
-- expiration time, we automatically cancel the task.
ctrExpirationTime :: Lens' ConversionTask (Maybe Text)
ctrExpirationTime =
    lens _ctrExpirationTime (\s a -> s { _ctrExpirationTime = a })

-- | If the task is for importing an instance, this contains information about
-- the import instance task.
ctrImportInstance :: Lens' ConversionTask (Maybe ImportInstanceTaskDetails)
ctrImportInstance =
    lens _ctrImportInstance (\s a -> s { _ctrImportInstance = a })

-- | If the task is for importing a volume, this contains information about the
-- import volume task.
ctrImportVolume :: Lens' ConversionTask (Maybe ImportVolumeTaskDetails)
ctrImportVolume = lens _ctrImportVolume (\s a -> s { _ctrImportVolume = a })

-- | The state of the conversion task.
ctrState :: Lens' ConversionTask ConversionTaskState
ctrState = lens _ctrState (\s a -> s { _ctrState = a })

-- | The status message related to the conversion task.
ctrStatusMessage :: Lens' ConversionTask (Maybe Text)
ctrStatusMessage =
    lens _ctrStatusMessage (\s a -> s { _ctrStatusMessage = a })

-- | 
ctrTags :: Lens' ConversionTask [Tag]
ctrTags = lens _ctrTags (\s a -> s { _ctrTags = a })

instance FromXML ConversionTask where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

data CreateVolumePermission = CreateVolumePermission
    { _cvpUserId :: Maybe Text
    , _cvpGroup :: Maybe PermissionGroup
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CreateVolumePermission' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @UserId ::@ @Maybe Text@
--
-- * @Group ::@ @Maybe PermissionGroup@
--
mkCreateVolumePermission :: CreateVolumePermission
mkCreateVolumePermission = CreateVolumePermission
    { _cvpUserId = Nothing
    , _cvpGroup = Nothing
    }

-- | The specific AWS account ID that is to be added or removed from a volume's
-- list of create volume permissions.
cvpUserId :: Lens' CreateVolumePermission (Maybe Text)
cvpUserId = lens _cvpUserId (\s a -> s { _cvpUserId = a })

-- | The specific group that is to be added or removed from a volume's list of
-- create volume permissions.
cvpGroup :: Lens' CreateVolumePermission (Maybe PermissionGroup)
cvpGroup = lens _cvpGroup (\s a -> s { _cvpGroup = a })

instance FromXML CreateVolumePermission where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery CreateVolumePermission where
    toQuery = genericQuery def

-- | A JSON representation of the snapshot attribute modification.
data CreateVolumePermissionModifications = CreateVolumePermissionModifications
    { _cvpmAdd :: [CreateVolumePermission]
    , _cvpmRemove :: [CreateVolumePermission]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CreateVolumePermissionModifications' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Add ::@ @[CreateVolumePermission]@
--
-- * @Remove ::@ @[CreateVolumePermission]@
--
mkCreateVolumePermissionModifications :: CreateVolumePermissionModifications
mkCreateVolumePermissionModifications = CreateVolumePermissionModifications
    { _cvpmAdd = mempty
    , _cvpmRemove = mempty
    }

-- | Adds a specific AWS account ID or group to a volume's list of create volume
-- permissions.
cvpmAdd :: Lens' CreateVolumePermissionModifications [CreateVolumePermission]
cvpmAdd = lens _cvpmAdd (\s a -> s { _cvpmAdd = a })

-- | Removes a specific AWS account ID or group from a volume's list of create
-- volume permissions.
cvpmRemove :: Lens' CreateVolumePermissionModifications [CreateVolumePermission]
cvpmRemove = lens _cvpmRemove (\s a -> s { _cvpmRemove = a })

instance ToQuery CreateVolumePermissionModifications where
    toQuery = genericQuery def

-- | Information about the customer gateway.
data CustomerGateway = CustomerGateway
    { _cgCustomerGatewayId :: Maybe Text
    , _cgState :: Maybe Text
    , _cgType :: Maybe Text
    , _cgIpAddress :: Maybe Text
    , _cgBgpAsn :: Maybe Text
    , _cgTags :: [Tag]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CustomerGateway' data type.
--
-- 'CustomerGateway' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CustomerGatewayId ::@ @Maybe Text@
--
-- * @State ::@ @Maybe Text@
--
-- * @Type ::@ @Maybe Text@
--
-- * @IpAddress ::@ @Maybe Text@
--
-- * @BgpAsn ::@ @Maybe Text@
--
-- * @Tags ::@ @[Tag]@
--
mkCustomerGateway :: CustomerGateway
mkCustomerGateway = CustomerGateway
    { _cgCustomerGatewayId = Nothing
    , _cgState = Nothing
    , _cgType = Nothing
    , _cgIpAddress = Nothing
    , _cgBgpAsn = Nothing
    , _cgTags = mempty
    }

-- | The ID of the customer gateway.
cgCustomerGatewayId :: Lens' CustomerGateway (Maybe Text)
cgCustomerGatewayId =
    lens _cgCustomerGatewayId (\s a -> s { _cgCustomerGatewayId = a })

-- | The current state of the customer gateway.
cgState :: Lens' CustomerGateway (Maybe Text)
cgState = lens _cgState (\s a -> s { _cgState = a })

-- | The type of VPN connection the customer gateway supports.
cgType :: Lens' CustomerGateway (Maybe Text)
cgType = lens _cgType (\s a -> s { _cgType = a })

-- | The Internet-routable IP address of the customer gateway's outside
-- interface.
cgIpAddress :: Lens' CustomerGateway (Maybe Text)
cgIpAddress = lens _cgIpAddress (\s a -> s { _cgIpAddress = a })

-- | The customer gateway's Border Gateway Protocol (BGP) Autonomous System
-- Number (ASN).
cgBgpAsn :: Lens' CustomerGateway (Maybe Text)
cgBgpAsn = lens _cgBgpAsn (\s a -> s { _cgBgpAsn = a })

-- | Any tags assigned to the customer gateway.
cgTags :: Lens' CustomerGateway [Tag]
cgTags = lens _cgTags (\s a -> s { _cgTags = a })

instance FromXML CustomerGateway where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "customerGateway"

-- | Describes a DHCP configuration option.
data DhcpConfiguration = DhcpConfiguration
    { _dcKey :: Maybe Text
    , _dcValues :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DhcpConfiguration' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Key ::@ @Maybe Text@
--
-- * @Values ::@ @[Text]@
--
mkDhcpConfiguration :: DhcpConfiguration
mkDhcpConfiguration = DhcpConfiguration
    { _dcKey = Nothing
    , _dcValues = mempty
    }

-- | The name of a DHCP option.
dcKey :: Lens' DhcpConfiguration (Maybe Text)
dcKey = lens _dcKey (\s a -> s { _dcKey = a })

-- | One or more values for the DHCP option.
dcValues :: Lens' DhcpConfiguration [Text]
dcValues = lens _dcValues (\s a -> s { _dcValues = a })

instance FromXML DhcpConfiguration where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DhcpConfiguration"

instance ToQuery DhcpConfiguration where
    toQuery = genericQuery def

-- | A set of DHCP options.
data DhcpOptions = DhcpOptions
    { _doDhcpOptionsId :: Maybe Text
    , _doDhcpConfigurations :: [DhcpConfiguration]
    , _doTags :: [Tag]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DhcpOptions' data type.
--
-- 'DhcpOptions' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DhcpOptionsId ::@ @Maybe Text@
--
-- * @DhcpConfigurations ::@ @[DhcpConfiguration]@
--
-- * @Tags ::@ @[Tag]@
--
mkDhcpOptions :: DhcpOptions
mkDhcpOptions = DhcpOptions
    { _doDhcpOptionsId = Nothing
    , _doDhcpConfigurations = mempty
    , _doTags = mempty
    }

-- | The ID of the set of DHCP options.
doDhcpOptionsId :: Lens' DhcpOptions (Maybe Text)
doDhcpOptionsId = lens _doDhcpOptionsId (\s a -> s { _doDhcpOptionsId = a })

-- | One or more DHCP options in the set.
doDhcpConfigurations :: Lens' DhcpOptions [DhcpConfiguration]
doDhcpConfigurations =
    lens _doDhcpConfigurations (\s a -> s { _doDhcpConfigurations = a })

-- | Any tags assigned to the DHCP options set.
doTags :: Lens' DhcpOptions [Tag]
doTags = lens _doTags (\s a -> s { _doTags = a })

instance FromXML DhcpOptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "dhcpOptions"

-- | Describes a disk image.
data DiskImage = DiskImage
    { _di3Image :: Maybe DiskImageDetail
    , _di3Description :: Maybe Text
    , _di3Volume :: Maybe VolumeDetail
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DiskImage' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Image ::@ @Maybe DiskImageDetail@
--
-- * @Description ::@ @Maybe Text@
--
-- * @Volume ::@ @Maybe VolumeDetail@
--
mkDiskImage :: DiskImage
mkDiskImage = DiskImage
    { _di3Image = Nothing
    , _di3Description = Nothing
    , _di3Volume = Nothing
    }

-- | 
di3Image :: Lens' DiskImage (Maybe DiskImageDetail)
di3Image = lens _di3Image (\s a -> s { _di3Image = a })

-- | 
di3Description :: Lens' DiskImage (Maybe Text)
di3Description = lens _di3Description (\s a -> s { _di3Description = a })

-- | 
di3Volume :: Lens' DiskImage (Maybe VolumeDetail)
di3Volume = lens _di3Volume (\s a -> s { _di3Volume = a })

instance ToQuery DiskImage where
    toQuery = genericQuery def

-- | The image.
data DiskImageDescription = DiskImageDescription
    { _didFormat :: DiskImageFormat
    , _didSize :: !Integer
    , _didImportManifestUrl :: Text
    , _didChecksum :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DiskImageDescription' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Format ::@ @DiskImageFormat@
--
-- * @Size ::@ @Integer@
--
-- * @ImportManifestUrl ::@ @Text@
--
-- * @Checksum ::@ @Maybe Text@
--
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

-- | The disk image format.
didFormat :: Lens' DiskImageDescription DiskImageFormat
didFormat = lens _didFormat (\s a -> s { _didFormat = a })

-- | The size of the disk image.
didSize :: Lens' DiskImageDescription Integer
didSize = lens _didSize (\s a -> s { _didSize = a })

-- | A presigned URL for the import manifest stored in Amazon S3. For
-- information about creating a presigned URL for an Amazon S3 object, read
-- the "Query String Request Authentication Alternative" section of the
-- Authenticating REST Requests topic in the Amazon Simple Storage Service
-- Developer Guide.
didImportManifestUrl :: Lens' DiskImageDescription Text
didImportManifestUrl =
    lens _didImportManifestUrl (\s a -> s { _didImportManifestUrl = a })

-- | The checksum computed for the disk image.
didChecksum :: Lens' DiskImageDescription (Maybe Text)
didChecksum = lens _didChecksum (\s a -> s { _didChecksum = a })

instance FromXML DiskImageDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "image"

instance ToQuery DiskImageDescription where
    toQuery = genericQuery def

data DiskImageDetail = DiskImageDetail
    { _did1Format :: DiskImageFormat
    , _did1Bytes :: !Integer
    , _did1ImportManifestUrl :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DiskImageDetail' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Format ::@ @DiskImageFormat@
--
-- * @Bytes ::@ @Integer@
--
-- * @ImportManifestUrl ::@ @Text@
--
mkDiskImageDetail :: DiskImageFormat -- ^ 'did1Format'
                  -> Integer -- ^ 'did1Bytes'
                  -> Text -- ^ 'did1ImportManifestUrl'
                  -> DiskImageDetail
mkDiskImageDetail p1 p2 p3 = DiskImageDetail
    { _did1Format = p1
    , _did1Bytes = p2
    , _did1ImportManifestUrl = p3
    }

-- | The disk image format.
did1Format :: Lens' DiskImageDetail DiskImageFormat
did1Format = lens _did1Format (\s a -> s { _did1Format = a })

-- | 
did1Bytes :: Lens' DiskImageDetail Integer
did1Bytes = lens _did1Bytes (\s a -> s { _did1Bytes = a })

-- | A presigned URL for the import manifest stored in Amazon S3. For
-- information about creating a presigned URL for an Amazon S3 object, read
-- the "Query String Request Authentication Alternative" section of the
-- Authenticating REST Requests topic in the Amazon Simple Storage Service
-- Developer Guide.
did1ImportManifestUrl :: Lens' DiskImageDetail Text
did1ImportManifestUrl =
    lens _did1ImportManifestUrl (\s a -> s { _did1ImportManifestUrl = a })

instance FromXML DiskImageDetail where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DiskImageDetail"

instance ToQuery DiskImageDetail where
    toQuery = genericQuery def

-- | The volume.
data DiskImageVolumeDescription = DiskImageVolumeDescription
    { _divdSize :: Maybe Integer
    , _divdId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DiskImageVolumeDescription' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Size ::@ @Maybe Integer@
--
-- * @Id ::@ @Text@
--
mkDiskImageVolumeDescription :: Text -- ^ 'divdId'
                             -> DiskImageVolumeDescription
mkDiskImageVolumeDescription p2 = DiskImageVolumeDescription
    { _divdSize = Nothing
    , _divdId = p2
    }

-- | The size of the volume.
divdSize :: Lens' DiskImageVolumeDescription (Maybe Integer)
divdSize = lens _divdSize (\s a -> s { _divdSize = a })

-- | The volume identifier.
divdId :: Lens' DiskImageVolumeDescription Text
divdId = lens _divdId (\s a -> s { _divdId = a })

instance FromXML DiskImageVolumeDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "volume"

instance ToQuery DiskImageVolumeDescription where
    toQuery = genericQuery def

-- | Parameters used to automatically set up Amazon EBS volumes when the
-- instance is launched.
data EbsBlockDevice = EbsBlockDevice
    { _ebdSnapshotId :: Maybe Text
    , _ebdVolumeSize :: Maybe Integer
    , _ebdDeleteOnTermination :: Maybe Bool
    , _ebdVolumeType :: Maybe VolumeType
    , _ebdIops :: Maybe Integer
    , _ebdEncrypted :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'EbsBlockDevice' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SnapshotId ::@ @Maybe Text@
--
-- * @VolumeSize ::@ @Maybe Integer@
--
-- * @DeleteOnTermination ::@ @Maybe Bool@
--
-- * @VolumeType ::@ @Maybe VolumeType@
--
-- * @Iops ::@ @Maybe Integer@
--
-- * @Encrypted ::@ @Maybe Bool@
--
mkEbsBlockDevice :: EbsBlockDevice
mkEbsBlockDevice = EbsBlockDevice
    { _ebdSnapshotId = Nothing
    , _ebdVolumeSize = Nothing
    , _ebdDeleteOnTermination = Nothing
    , _ebdVolumeType = Nothing
    , _ebdIops = Nothing
    , _ebdEncrypted = Nothing
    }

-- | The ID of the snapshot.
ebdSnapshotId :: Lens' EbsBlockDevice (Maybe Text)
ebdSnapshotId = lens _ebdSnapshotId (\s a -> s { _ebdSnapshotId = a })

-- | The size of the volume, in GiB. Constraints: If the volume type is io1, the
-- minimum size of the volume is 10 GiB; otherwise, the minimum size is 1 GiB.
-- The maximum volume size is 1024 GiB. Default: If you're creating the volume
-- from a snapshot and don't specify a volume size, the default is the
-- snapshot size.
ebdVolumeSize :: Lens' EbsBlockDevice (Maybe Integer)
ebdVolumeSize = lens _ebdVolumeSize (\s a -> s { _ebdVolumeSize = a })

-- | Indicates whether the Amazon EBS volume is deleted on instance termination.
ebdDeleteOnTermination :: Lens' EbsBlockDevice (Maybe Bool)
ebdDeleteOnTermination =
    lens _ebdDeleteOnTermination (\s a -> s { _ebdDeleteOnTermination = a })

-- | The volume type. gp2 for General Purpose (SSD) volumes, io1 for Provisioned
-- IOPS (SSD) volumes, and standard for Magnetic volumes. Default: standard.
ebdVolumeType :: Lens' EbsBlockDevice (Maybe VolumeType)
ebdVolumeType = lens _ebdVolumeType (\s a -> s { _ebdVolumeType = a })

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

-- | Indicates whether the Amazon EBS volume is encrypted.
ebdEncrypted :: Lens' EbsBlockDevice (Maybe Bool)
ebdEncrypted = lens _ebdEncrypted (\s a -> s { _ebdEncrypted = a })

instance FromXML EbsBlockDevice where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EbsBlockDevice"

instance ToQuery EbsBlockDevice where
    toQuery = genericQuery def

-- | Parameters used to automatically set up Amazon EBS volumes when the
-- instance is launched.
data EbsInstanceBlockDevice = EbsInstanceBlockDevice
    { _eibdVolumeId :: Maybe Text
    , _eibdStatus :: Maybe AttachmentStatus
    , _eibdAttachTime :: Maybe ISO8601
    , _eibdDeleteOnTermination :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'EbsInstanceBlockDevice' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VolumeId ::@ @Maybe Text@
--
-- * @Status ::@ @Maybe AttachmentStatus@
--
-- * @AttachTime ::@ @Maybe ISO8601@
--
-- * @DeleteOnTermination ::@ @Maybe Bool@
--
mkEbsInstanceBlockDevice :: EbsInstanceBlockDevice
mkEbsInstanceBlockDevice = EbsInstanceBlockDevice
    { _eibdVolumeId = Nothing
    , _eibdStatus = Nothing
    , _eibdAttachTime = Nothing
    , _eibdDeleteOnTermination = Nothing
    }

-- | The ID of the Amazon EBS volume.
eibdVolumeId :: Lens' EbsInstanceBlockDevice (Maybe Text)
eibdVolumeId = lens _eibdVolumeId (\s a -> s { _eibdVolumeId = a })

-- | The attachment state.
eibdStatus :: Lens' EbsInstanceBlockDevice (Maybe AttachmentStatus)
eibdStatus = lens _eibdStatus (\s a -> s { _eibdStatus = a })

-- | The time stamp when the attachment initiated.
eibdAttachTime :: Lens' EbsInstanceBlockDevice (Maybe ISO8601)
eibdAttachTime = lens _eibdAttachTime (\s a -> s { _eibdAttachTime = a })

-- | Indicates whether the volume is deleted on instance termination.
eibdDeleteOnTermination :: Lens' EbsInstanceBlockDevice (Maybe Bool)
eibdDeleteOnTermination =
    lens _eibdDeleteOnTermination
         (\s a -> s { _eibdDeleteOnTermination = a })

instance FromXML EbsInstanceBlockDevice where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ebs"

instance ToQuery EbsInstanceBlockDevice where
    toQuery = genericQuery def

-- | Parameters used to automatically set up Amazon EBS volumes when the
-- instance is launched.
data EbsInstanceBlockDeviceSpecification = EbsInstanceBlockDeviceSpecification
    { _eibdsVolumeId :: Maybe Text
    , _eibdsDeleteOnTermination :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'EbsInstanceBlockDeviceSpecification' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VolumeId ::@ @Maybe Text@
--
-- * @DeleteOnTermination ::@ @Maybe Bool@
--
mkEbsInstanceBlockDeviceSpecification :: EbsInstanceBlockDeviceSpecification
mkEbsInstanceBlockDeviceSpecification = EbsInstanceBlockDeviceSpecification
    { _eibdsVolumeId = Nothing
    , _eibdsDeleteOnTermination = Nothing
    }

-- | The ID of the Amazon EBS volume.
eibdsVolumeId :: Lens' EbsInstanceBlockDeviceSpecification (Maybe Text)
eibdsVolumeId = lens _eibdsVolumeId (\s a -> s { _eibdsVolumeId = a })

-- | Indicates whether the volume is deleted on instance termination.
eibdsDeleteOnTermination :: Lens' EbsInstanceBlockDeviceSpecification (Maybe Bool)
eibdsDeleteOnTermination =
    lens _eibdsDeleteOnTermination
         (\s a -> s { _eibdsDeleteOnTermination = a })

instance FromXML EbsInstanceBlockDeviceSpecification where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EbsInstanceBlockDeviceSpecification"

instance ToQuery EbsInstanceBlockDeviceSpecification where
    toQuery = genericQuery def

data ExportTask = ExportTask
    { _etExportTaskId :: Maybe Text
    , _etDescription :: Maybe Text
    , _etState :: Maybe ExportTaskState
    , _etStatusMessage :: Maybe Text
    , _etInstanceExportDetails :: Maybe InstanceExportDetails
    , _etExportToS3Task :: Maybe ExportToS3Task
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ExportTask' data type.
--
-- 'ExportTask' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ExportTaskId ::@ @Maybe Text@
--
-- * @Description ::@ @Maybe Text@
--
-- * @State ::@ @Maybe ExportTaskState@
--
-- * @StatusMessage ::@ @Maybe Text@
--
-- * @InstanceExportDetails ::@ @Maybe InstanceExportDetails@
--
-- * @ExportToS3Task ::@ @Maybe ExportToS3Task@
--
mkExportTask :: ExportTask
mkExportTask = ExportTask
    { _etExportTaskId = Nothing
    , _etDescription = Nothing
    , _etState = Nothing
    , _etStatusMessage = Nothing
    , _etInstanceExportDetails = Nothing
    , _etExportToS3Task = Nothing
    }

-- | The ID of the export task.
etExportTaskId :: Lens' ExportTask (Maybe Text)
etExportTaskId = lens _etExportTaskId (\s a -> s { _etExportTaskId = a })

-- | A description of the resource being exported.
etDescription :: Lens' ExportTask (Maybe Text)
etDescription = lens _etDescription (\s a -> s { _etDescription = a })

-- | The state of the conversion task.
etState :: Lens' ExportTask (Maybe ExportTaskState)
etState = lens _etState (\s a -> s { _etState = a })

-- | The status message related to the export task.
etStatusMessage :: Lens' ExportTask (Maybe Text)
etStatusMessage = lens _etStatusMessage (\s a -> s { _etStatusMessage = a })

-- | The instance being exported.
etInstanceExportDetails :: Lens' ExportTask (Maybe InstanceExportDetails)
etInstanceExportDetails =
    lens _etInstanceExportDetails
         (\s a -> s { _etInstanceExportDetails = a })

-- | 
etExportToS3Task :: Lens' ExportTask (Maybe ExportToS3Task)
etExportToS3Task =
    lens _etExportToS3Task (\s a -> s { _etExportToS3Task = a })

instance FromXML ExportTask where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "exportTask"

data ExportToS3Task = ExportToS3Task
    { _etstDiskImageFormat :: Maybe DiskImageFormat
    , _etstContainerFormat :: Maybe ContainerFormat
    , _etstS3Bucket :: Maybe Text
    , _etstS3Key :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ExportToS3Task' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DiskImageFormat ::@ @Maybe DiskImageFormat@
--
-- * @ContainerFormat ::@ @Maybe ContainerFormat@
--
-- * @S3Bucket ::@ @Maybe Text@
--
-- * @S3Key ::@ @Maybe Text@
--
mkExportToS3Task :: ExportToS3Task
mkExportToS3Task = ExportToS3Task
    { _etstDiskImageFormat = Nothing
    , _etstContainerFormat = Nothing
    , _etstS3Bucket = Nothing
    , _etstS3Key = Nothing
    }

-- | The format for the exported image.
etstDiskImageFormat :: Lens' ExportToS3Task (Maybe DiskImageFormat)
etstDiskImageFormat =
    lens _etstDiskImageFormat (\s a -> s { _etstDiskImageFormat = a })

-- | The container format used to combine disk images with metadata (such as
-- OVF). If absent, only the disk image is exported.
etstContainerFormat :: Lens' ExportToS3Task (Maybe ContainerFormat)
etstContainerFormat =
    lens _etstContainerFormat (\s a -> s { _etstContainerFormat = a })

-- | The Amazon S3 bucket for the destination image. The destination bucket must
-- exist and grant WRITE and READ_ACL permissions to the AWS account
-- vm-import-export@amazon.com.
etstS3Bucket :: Lens' ExportToS3Task (Maybe Text)
etstS3Bucket = lens _etstS3Bucket (\s a -> s { _etstS3Bucket = a })

-- | 
etstS3Key :: Lens' ExportToS3Task (Maybe Text)
etstS3Key = lens _etstS3Key (\s a -> s { _etstS3Key = a })

instance FromXML ExportToS3Task where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "exportToS3"

instance ToQuery ExportToS3Task where
    toQuery = genericQuery def

data ExportToS3TaskSpecification = ExportToS3TaskSpecification
    { _etstsDiskImageFormat :: Maybe DiskImageFormat
    , _etstsContainerFormat :: Maybe ContainerFormat
    , _etstsS3Bucket :: Maybe Text
    , _etstsS3Prefix :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ExportToS3TaskSpecification' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DiskImageFormat ::@ @Maybe DiskImageFormat@
--
-- * @ContainerFormat ::@ @Maybe ContainerFormat@
--
-- * @S3Bucket ::@ @Maybe Text@
--
-- * @S3Prefix ::@ @Maybe Text@
--
mkExportToS3TaskSpecification :: ExportToS3TaskSpecification
mkExportToS3TaskSpecification = ExportToS3TaskSpecification
    { _etstsDiskImageFormat = Nothing
    , _etstsContainerFormat = Nothing
    , _etstsS3Bucket = Nothing
    , _etstsS3Prefix = Nothing
    }

-- | 
etstsDiskImageFormat :: Lens' ExportToS3TaskSpecification (Maybe DiskImageFormat)
etstsDiskImageFormat =
    lens _etstsDiskImageFormat (\s a -> s { _etstsDiskImageFormat = a })

-- | 
etstsContainerFormat :: Lens' ExportToS3TaskSpecification (Maybe ContainerFormat)
etstsContainerFormat =
    lens _etstsContainerFormat (\s a -> s { _etstsContainerFormat = a })

-- | 
etstsS3Bucket :: Lens' ExportToS3TaskSpecification (Maybe Text)
etstsS3Bucket = lens _etstsS3Bucket (\s a -> s { _etstsS3Bucket = a })

-- | The image is written to a single object in the Amazon S3 bucket at the S3
-- key s3prefix + exportTaskId + '.' + diskImageFormat.
etstsS3Prefix :: Lens' ExportToS3TaskSpecification (Maybe Text)
etstsS3Prefix = lens _etstsS3Prefix (\s a -> s { _etstsS3Prefix = a })

instance ToQuery ExportToS3TaskSpecification where
    toQuery = genericQuery def

data Filter = Filter
    { _fName :: Text
    , _fValues :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Filter' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Name ::@ @Text@
--
-- * @Values ::@ @[Text]@
--
mkFilter :: Text -- ^ 'fName'
         -> Filter
mkFilter p1 = Filter
    { _fName = p1
    , _fValues = mempty
    }

-- | The name of the filter.
fName :: Lens' Filter Text
fName = lens _fName (\s a -> s { _fName = a })

-- | One or more filter values.
fValues :: Lens' Filter [Text]
fValues = lens _fValues (\s a -> s { _fValues = a })

instance ToQuery Filter where
    toQuery = genericQuery def

-- | Describes a security group.
data GroupIdentifier = GroupIdentifier
    { _giGroupName :: Maybe Text
    , _giGroupId :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'GroupIdentifier' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GroupName ::@ @Maybe Text@
--
-- * @GroupId ::@ @Maybe Text@
--
mkGroupIdentifier :: GroupIdentifier
mkGroupIdentifier = GroupIdentifier
    { _giGroupName = Nothing
    , _giGroupId = Nothing
    }

-- | The name of the security group.
giGroupName :: Lens' GroupIdentifier (Maybe Text)
giGroupName = lens _giGroupName (\s a -> s { _giGroupName = a })

-- | The ID of the security group.
giGroupId :: Lens' GroupIdentifier (Maybe Text)
giGroupId = lens _giGroupId (\s a -> s { _giGroupId = a })

instance FromXML GroupIdentifier where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery GroupIdentifier where
    toQuery = genericQuery def

-- | The IAM instance profile associated with the instance.
data IamInstanceProfile = IamInstanceProfile
    { _iipArn :: Maybe Text
    , _iipId :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'IamInstanceProfile' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Arn ::@ @Maybe Text@
--
-- * @Id ::@ @Maybe Text@
--
mkIamInstanceProfile :: IamInstanceProfile
mkIamInstanceProfile = IamInstanceProfile
    { _iipArn = Nothing
    , _iipId = Nothing
    }

-- | The Amazon Resource Name (ARN) of the instance profile.
iipArn :: Lens' IamInstanceProfile (Maybe Text)
iipArn = lens _iipArn (\s a -> s { _iipArn = a })

-- | The ID of the instance profile.
iipId :: Lens' IamInstanceProfile (Maybe Text)
iipId = lens _iipId (\s a -> s { _iipId = a })

instance FromXML IamInstanceProfile where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "iamInstanceProfile"

instance ToQuery IamInstanceProfile where
    toQuery = genericQuery def

-- | The IAM instance profile.
data IamInstanceProfileSpecification = IamInstanceProfileSpecification
    { _iipsArn :: Maybe Text
    , _iipsName :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'IamInstanceProfileSpecification' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Arn ::@ @Maybe Text@
--
-- * @Name ::@ @Maybe Text@
--
mkIamInstanceProfileSpecification :: IamInstanceProfileSpecification
mkIamInstanceProfileSpecification = IamInstanceProfileSpecification
    { _iipsArn = Nothing
    , _iipsName = Nothing
    }

-- | The Amazon Resource Name (ARN) of the instance profile.
iipsArn :: Lens' IamInstanceProfileSpecification (Maybe Text)
iipsArn = lens _iipsArn (\s a -> s { _iipsArn = a })

-- | The name of the instance profile.
iipsName :: Lens' IamInstanceProfileSpecification (Maybe Text)
iipsName = lens _iipsName (\s a -> s { _iipsName = a })

instance FromXML IamInstanceProfileSpecification where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "iamInstanceProfile"

instance ToQuery IamInstanceProfileSpecification where
    toQuery = genericQuery def

-- | ICMP protocol: The ICMP type and code.
data IcmpTypeCode = IcmpTypeCode
    { _itcType :: Maybe Integer
    , _itcCode :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'IcmpTypeCode' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Type ::@ @Maybe Integer@
--
-- * @Code ::@ @Maybe Integer@
--
mkIcmpTypeCode :: IcmpTypeCode
mkIcmpTypeCode = IcmpTypeCode
    { _itcType = Nothing
    , _itcCode = Nothing
    }

-- | The ICMP code. A value of -1 means all codes for the specified ICMP type.
itcType :: Lens' IcmpTypeCode (Maybe Integer)
itcType = lens _itcType (\s a -> s { _itcType = a })

-- | The ICMP type. A value of -1 means all types.
itcCode :: Lens' IcmpTypeCode (Maybe Integer)
itcCode = lens _itcCode (\s a -> s { _itcCode = a })

instance FromXML IcmpTypeCode where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "icmpTypeCode"

instance ToQuery IcmpTypeCode where
    toQuery = genericQuery def

-- | Describes an image.
data Image = Image
    { _iImageId :: Text
    , _iImageLocation :: Text
    , _iState :: ImageState
    , _iOwnerId :: Text
    , _iPublic :: Bool
    , _iProductCodes :: [ProductCode]
    , _iArchitecture :: ArchitectureValues
    , _iImageType :: ImageTypeValues
    , _iKernelId :: Maybe Text
    , _iRamdiskId :: Maybe Text
    , _iPlatform :: Maybe PlatformValues
    , _iSriovNetSupport :: Maybe Text
    , _iStateReason :: Maybe StateReason
    , _iImageOwnerAlias :: Maybe Text
    , _iName :: Text
    , _iDescription :: Maybe Text
    , _iRootDeviceType :: DeviceType
    , _iRootDeviceName :: Maybe Text
    , _iBlockDeviceMappings :: [BlockDeviceMapping]
    , _iVirtualizationType :: VirtualizationType
    , _iTags :: [Tag]
    , _iHypervisor :: HypervisorType
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Image' data type.
--
-- 'Image' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ImageId ::@ @Text@
--
-- * @ImageLocation ::@ @Text@
--
-- * @State ::@ @ImageState@
--
-- * @OwnerId ::@ @Text@
--
-- * @Public ::@ @Bool@
--
-- * @ProductCodes ::@ @[ProductCode]@
--
-- * @Architecture ::@ @ArchitectureValues@
--
-- * @ImageType ::@ @ImageTypeValues@
--
-- * @KernelId ::@ @Maybe Text@
--
-- * @RamdiskId ::@ @Maybe Text@
--
-- * @Platform ::@ @Maybe PlatformValues@
--
-- * @SriovNetSupport ::@ @Maybe Text@
--
-- * @StateReason ::@ @Maybe StateReason@
--
-- * @ImageOwnerAlias ::@ @Maybe Text@
--
-- * @Name ::@ @Text@
--
-- * @Description ::@ @Maybe Text@
--
-- * @RootDeviceType ::@ @DeviceType@
--
-- * @RootDeviceName ::@ @Maybe Text@
--
-- * @BlockDeviceMappings ::@ @[BlockDeviceMapping]@
--
-- * @VirtualizationType ::@ @VirtualizationType@
--
-- * @Tags ::@ @[Tag]@
--
-- * @Hypervisor ::@ @HypervisorType@
--
mkImage :: Text -- ^ 'iImageId'
        -> Text -- ^ 'iName'
        -> DeviceType -- ^ 'iRootDeviceType'
        -> Text -- ^ 'iImageLocation'
        -> VirtualizationType -- ^ 'iVirtualizationType'
        -> HypervisorType -- ^ 'iHypervisor'
        -> ImageState -- ^ 'iState'
        -> Text -- ^ 'iOwnerId'
        -> Bool -- ^ 'iPublic'
        -> ArchitectureValues -- ^ 'iArchitecture'
        -> ImageTypeValues -- ^ 'iImageType'
        -> Image
mkImage p1 p15 p17 p2 p20 p22 p3 p4 p5 p7 p8 = Image
    { _iImageId = p1
    , _iImageLocation = p2
    , _iState = p3
    , _iOwnerId = p4
    , _iPublic = p5
    , _iProductCodes = mempty
    , _iArchitecture = p7
    , _iImageType = p8
    , _iKernelId = Nothing
    , _iRamdiskId = Nothing
    , _iPlatform = Nothing
    , _iSriovNetSupport = Nothing
    , _iStateReason = Nothing
    , _iImageOwnerAlias = Nothing
    , _iName = p15
    , _iDescription = Nothing
    , _iRootDeviceType = p17
    , _iRootDeviceName = Nothing
    , _iBlockDeviceMappings = mempty
    , _iVirtualizationType = p20
    , _iTags = mempty
    , _iHypervisor = p22
    }

-- | The ID of the AMI.
iImageId :: Lens' Image Text
iImageId = lens _iImageId (\s a -> s { _iImageId = a })

-- | The location of the AMI.
iImageLocation :: Lens' Image Text
iImageLocation = lens _iImageLocation (\s a -> s { _iImageLocation = a })

-- | The current state of the AMI. If the state is available, the image is
-- successfully registered and can be used to launch an instance.
iState :: Lens' Image ImageState
iState = lens _iState (\s a -> s { _iState = a })

-- | The AWS account ID of the image owner.
iOwnerId :: Lens' Image Text
iOwnerId = lens _iOwnerId (\s a -> s { _iOwnerId = a })

-- | Indicates whether the image has public launch permissions. The value is
-- true if this image has public launch permissions or false if it has only
-- implicit and explicit launch permissions.
iPublic :: Lens' Image Bool
iPublic = lens _iPublic (\s a -> s { _iPublic = a })

-- | Any product codes associated with the AMI.
iProductCodes :: Lens' Image [ProductCode]
iProductCodes = lens _iProductCodes (\s a -> s { _iProductCodes = a })

-- | The architecture of the image.
iArchitecture :: Lens' Image ArchitectureValues
iArchitecture = lens _iArchitecture (\s a -> s { _iArchitecture = a })

-- | The type of image.
iImageType :: Lens' Image ImageTypeValues
iImageType = lens _iImageType (\s a -> s { _iImageType = a })

-- | The kernel associated with the image, if any. Only applicable for machine
-- images.
iKernelId :: Lens' Image (Maybe Text)
iKernelId = lens _iKernelId (\s a -> s { _iKernelId = a })

-- | The RAM disk associated with the image, if any. Only applicable for machine
-- images.
iRamdiskId :: Lens' Image (Maybe Text)
iRamdiskId = lens _iRamdiskId (\s a -> s { _iRamdiskId = a })

-- | The value is Windows for Windows AMIs; otherwise blank.
iPlatform :: Lens' Image (Maybe PlatformValues)
iPlatform = lens _iPlatform (\s a -> s { _iPlatform = a })

-- | Specifies whether enhanced networking is enabled.
iSriovNetSupport :: Lens' Image (Maybe Text)
iSriovNetSupport =
    lens _iSriovNetSupport (\s a -> s { _iSriovNetSupport = a })

-- | The reason for the state change.
iStateReason :: Lens' Image (Maybe StateReason)
iStateReason = lens _iStateReason (\s a -> s { _iStateReason = a })

-- | The AWS account alias (for example, amazon, self) or the AWS account ID of
-- the AMI owner.
iImageOwnerAlias :: Lens' Image (Maybe Text)
iImageOwnerAlias =
    lens _iImageOwnerAlias (\s a -> s { _iImageOwnerAlias = a })

-- | The name of the AMI that was provided during image creation.
iName :: Lens' Image Text
iName = lens _iName (\s a -> s { _iName = a })

-- | The description of the AMI that was provided during image creation.
iDescription :: Lens' Image (Maybe Text)
iDescription = lens _iDescription (\s a -> s { _iDescription = a })

-- | The type of root device used by the AMI. The AMI can use an Amazon EBS
-- volume or an instance store volume.
iRootDeviceType :: Lens' Image DeviceType
iRootDeviceType = lens _iRootDeviceType (\s a -> s { _iRootDeviceType = a })

-- | The device name of the root device (for example, /dev/sda1 or xvda).
iRootDeviceName :: Lens' Image (Maybe Text)
iRootDeviceName = lens _iRootDeviceName (\s a -> s { _iRootDeviceName = a })

-- | Any block device mapping entries.
iBlockDeviceMappings :: Lens' Image [BlockDeviceMapping]
iBlockDeviceMappings =
    lens _iBlockDeviceMappings (\s a -> s { _iBlockDeviceMappings = a })

-- | The type of virtualization of the AMI.
iVirtualizationType :: Lens' Image VirtualizationType
iVirtualizationType =
    lens _iVirtualizationType (\s a -> s { _iVirtualizationType = a })

-- | Any tags assigned to the image.
iTags :: Lens' Image [Tag]
iTags = lens _iTags (\s a -> s { _iTags = a })

-- | The hypervisor type of the image.
iHypervisor :: Lens' Image HypervisorType
iHypervisor = lens _iHypervisor (\s a -> s { _iHypervisor = a })

instance FromXML Image where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | 
data ImportInstanceLaunchSpecification = ImportInstanceLaunchSpecification
    { _iilsArchitecture :: Maybe ArchitectureValues
    , _iilsGroupNames :: [Text]
    , _iilsAdditionalInfo :: Maybe Text
    , _iilsUserData :: Maybe Text
    , _iilsInstanceType :: Maybe InstanceType
    , _iilsPlacement :: Maybe Placement
    , _iilsMonitoring :: Maybe Bool
    , _iilsSubnetId :: Maybe Text
    , _iilsInstanceInitiatedShutdownBehavior :: Maybe ShutdownBehavior
    , _iilsPrivateIpAddress :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ImportInstanceLaunchSpecification' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Architecture ::@ @Maybe ArchitectureValues@
--
-- * @GroupNames ::@ @[Text]@
--
-- * @AdditionalInfo ::@ @Maybe Text@
--
-- * @UserData ::@ @Maybe Text@
--
-- * @InstanceType ::@ @Maybe InstanceType@
--
-- * @Placement ::@ @Maybe Placement@
--
-- * @Monitoring ::@ @Maybe Bool@
--
-- * @SubnetId ::@ @Maybe Text@
--
-- * @InstanceInitiatedShutdownBehavior ::@ @Maybe ShutdownBehavior@
--
-- * @PrivateIpAddress ::@ @Maybe Text@
--
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

-- | The architecture of the instance.
iilsArchitecture :: Lens' ImportInstanceLaunchSpecification (Maybe ArchitectureValues)
iilsArchitecture =
    lens _iilsArchitecture (\s a -> s { _iilsArchitecture = a })

-- | One or more security group names.
iilsGroupNames :: Lens' ImportInstanceLaunchSpecification [Text]
iilsGroupNames = lens _iilsGroupNames (\s a -> s { _iilsGroupNames = a })

-- | 
iilsAdditionalInfo :: Lens' ImportInstanceLaunchSpecification (Maybe Text)
iilsAdditionalInfo =
    lens _iilsAdditionalInfo (\s a -> s { _iilsAdditionalInfo = a })

-- | User data to be made available to the instance.
iilsUserData :: Lens' ImportInstanceLaunchSpecification (Maybe Text)
iilsUserData = lens _iilsUserData (\s a -> s { _iilsUserData = a })

-- | The instance type. For more information, see Instance Types in the Amazon
-- Elastic Compute Cloud User Guide.
iilsInstanceType :: Lens' ImportInstanceLaunchSpecification (Maybe InstanceType)
iilsInstanceType =
    lens _iilsInstanceType (\s a -> s { _iilsInstanceType = a })

-- | 
iilsPlacement :: Lens' ImportInstanceLaunchSpecification (Maybe Placement)
iilsPlacement = lens _iilsPlacement (\s a -> s { _iilsPlacement = a })

-- | 
iilsMonitoring :: Lens' ImportInstanceLaunchSpecification (Maybe Bool)
iilsMonitoring = lens _iilsMonitoring (\s a -> s { _iilsMonitoring = a })

-- | [EC2-VPC] The ID of the subnet to launch the instance into.
iilsSubnetId :: Lens' ImportInstanceLaunchSpecification (Maybe Text)
iilsSubnetId = lens _iilsSubnetId (\s a -> s { _iilsSubnetId = a })

-- | Indicates whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for system
-- shutdown).
iilsInstanceInitiatedShutdownBehavior :: Lens' ImportInstanceLaunchSpecification (Maybe ShutdownBehavior)
iilsInstanceInitiatedShutdownBehavior =
    lens _iilsInstanceInitiatedShutdownBehavior
         (\s a -> s { _iilsInstanceInitiatedShutdownBehavior = a })

-- | [EC2-VPC] Optionally, you can use this parameter to assign the instance a
-- specific available IP address from the IP address range of the subnet.
iilsPrivateIpAddress :: Lens' ImportInstanceLaunchSpecification (Maybe Text)
iilsPrivateIpAddress =
    lens _iilsPrivateIpAddress (\s a -> s { _iilsPrivateIpAddress = a })

instance ToQuery ImportInstanceLaunchSpecification where
    toQuery = genericQuery def

-- | If the task is for importing an instance, this contains information about
-- the import instance task.
data ImportInstanceTaskDetails = ImportInstanceTaskDetails
    { _iitdVolumes :: [ImportInstanceVolumeDetailItem]
    , _iitdInstanceId :: Maybe Text
    , _iitdPlatform :: Maybe PlatformValues
    , _iitdDescription :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ImportInstanceTaskDetails' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Volumes ::@ @[ImportInstanceVolumeDetailItem]@
--
-- * @InstanceId ::@ @Maybe Text@
--
-- * @Platform ::@ @Maybe PlatformValues@
--
-- * @Description ::@ @Maybe Text@
--
mkImportInstanceTaskDetails :: [ImportInstanceVolumeDetailItem] -- ^ 'iitdVolumes'
                            -> ImportInstanceTaskDetails
mkImportInstanceTaskDetails p1 = ImportInstanceTaskDetails
    { _iitdVolumes = p1
    , _iitdInstanceId = Nothing
    , _iitdPlatform = Nothing
    , _iitdDescription = Nothing
    }

-- | 
iitdVolumes :: Lens' ImportInstanceTaskDetails [ImportInstanceVolumeDetailItem]
iitdVolumes = lens _iitdVolumes (\s a -> s { _iitdVolumes = a })

-- | 
iitdInstanceId :: Lens' ImportInstanceTaskDetails (Maybe Text)
iitdInstanceId = lens _iitdInstanceId (\s a -> s { _iitdInstanceId = a })

-- | The instance operating system.
iitdPlatform :: Lens' ImportInstanceTaskDetails (Maybe PlatformValues)
iitdPlatform = lens _iitdPlatform (\s a -> s { _iitdPlatform = a })

-- | 
iitdDescription :: Lens' ImportInstanceTaskDetails (Maybe Text)
iitdDescription = lens _iitdDescription (\s a -> s { _iitdDescription = a })

instance FromXML ImportInstanceTaskDetails where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "importInstance"

instance ToQuery ImportInstanceTaskDetails where
    toQuery = genericQuery def

-- | Describes an import volume task.
data ImportInstanceVolumeDetailItem = ImportInstanceVolumeDetailItem
    { _iivdiBytesConverted :: !Integer
    , _iivdiAvailabilityZone :: Text
    , _iivdiImage :: DiskImageDescription
    , _iivdiVolume :: DiskImageVolumeDescription
    , _iivdiStatus :: Text
    , _iivdiStatusMessage :: Maybe Text
    , _iivdiDescription :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ImportInstanceVolumeDetailItem' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @BytesConverted ::@ @Integer@
--
-- * @AvailabilityZone ::@ @Text@
--
-- * @Image ::@ @DiskImageDescription@
--
-- * @Volume ::@ @DiskImageVolumeDescription@
--
-- * @Status ::@ @Text@
--
-- * @StatusMessage ::@ @Maybe Text@
--
-- * @Description ::@ @Maybe Text@
--
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

-- | The number of bytes converted so far.
iivdiBytesConverted :: Lens' ImportInstanceVolumeDetailItem Integer
iivdiBytesConverted =
    lens _iivdiBytesConverted (\s a -> s { _iivdiBytesConverted = a })

-- | The Availability Zone where the resulting instance will reside.
iivdiAvailabilityZone :: Lens' ImportInstanceVolumeDetailItem Text
iivdiAvailabilityZone =
    lens _iivdiAvailabilityZone (\s a -> s { _iivdiAvailabilityZone = a })

-- | The image.
iivdiImage :: Lens' ImportInstanceVolumeDetailItem DiskImageDescription
iivdiImage = lens _iivdiImage (\s a -> s { _iivdiImage = a })

-- | The volume.
iivdiVolume :: Lens' ImportInstanceVolumeDetailItem DiskImageVolumeDescription
iivdiVolume = lens _iivdiVolume (\s a -> s { _iivdiVolume = a })

-- | The status of the import of this particular disk image.
iivdiStatus :: Lens' ImportInstanceVolumeDetailItem Text
iivdiStatus = lens _iivdiStatus (\s a -> s { _iivdiStatus = a })

-- | The status information or errors related to the disk image.
iivdiStatusMessage :: Lens' ImportInstanceVolumeDetailItem (Maybe Text)
iivdiStatusMessage =
    lens _iivdiStatusMessage (\s a -> s { _iivdiStatusMessage = a })

-- | 
iivdiDescription :: Lens' ImportInstanceVolumeDetailItem (Maybe Text)
iivdiDescription =
    lens _iivdiDescription (\s a -> s { _iivdiDescription = a })

instance FromXML ImportInstanceVolumeDetailItem where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery ImportInstanceVolumeDetailItem where
    toQuery = genericQuery def

-- | If the task is for importing a volume, this contains information about the
-- import volume task.
data ImportVolumeTaskDetails = ImportVolumeTaskDetails
    { _ivtdBytesConverted :: !Integer
    , _ivtdAvailabilityZone :: Text
    , _ivtdDescription :: Maybe Text
    , _ivtdImage :: DiskImageDescription
    , _ivtdVolume :: DiskImageVolumeDescription
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ImportVolumeTaskDetails' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @BytesConverted ::@ @Integer@
--
-- * @AvailabilityZone ::@ @Text@
--
-- * @Description ::@ @Maybe Text@
--
-- * @Image ::@ @DiskImageDescription@
--
-- * @Volume ::@ @DiskImageVolumeDescription@
--
mkImportVolumeTaskDetails :: Integer -- ^ 'ivtdBytesConverted'
                          -> Text -- ^ 'ivtdAvailabilityZone'
                          -> DiskImageDescription -- ^ 'ivtdImage'
                          -> DiskImageVolumeDescription -- ^ 'ivtdVolume'
                          -> ImportVolumeTaskDetails
mkImportVolumeTaskDetails p1 p2 p4 p5 = ImportVolumeTaskDetails
    { _ivtdBytesConverted = p1
    , _ivtdAvailabilityZone = p2
    , _ivtdDescription = Nothing
    , _ivtdImage = p4
    , _ivtdVolume = p5
    }

-- | The number of bytes converted so far.
ivtdBytesConverted :: Lens' ImportVolumeTaskDetails Integer
ivtdBytesConverted =
    lens _ivtdBytesConverted (\s a -> s { _ivtdBytesConverted = a })

-- | The Availability Zone where the resulting volume will reside.
ivtdAvailabilityZone :: Lens' ImportVolumeTaskDetails Text
ivtdAvailabilityZone =
    lens _ivtdAvailabilityZone (\s a -> s { _ivtdAvailabilityZone = a })

-- | The description you provided when starting the import volume task.
ivtdDescription :: Lens' ImportVolumeTaskDetails (Maybe Text)
ivtdDescription = lens _ivtdDescription (\s a -> s { _ivtdDescription = a })

-- | The image.
ivtdImage :: Lens' ImportVolumeTaskDetails DiskImageDescription
ivtdImage = lens _ivtdImage (\s a -> s { _ivtdImage = a })

-- | The volume.
ivtdVolume :: Lens' ImportVolumeTaskDetails DiskImageVolumeDescription
ivtdVolume = lens _ivtdVolume (\s a -> s { _ivtdVolume = a })

instance FromXML ImportVolumeTaskDetails where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "importVolume"

instance ToQuery ImportVolumeTaskDetails where
    toQuery = genericQuery def

-- | Describes an instance.
data Instance = Instance
    { _i1InstanceId :: Maybe Text
    , _i1ImageId :: Maybe Text
    , _i1State :: Maybe InstanceState
    , _i1PrivateDnsName :: Maybe Text
    , _i1PublicDnsName :: Maybe Text
    , _i1StateTransitionReason :: Maybe Text
    , _i1KeyName :: Maybe Text
    , _i1AmiLaunchIndex :: Maybe Integer
    , _i1ProductCodes :: [ProductCode]
    , _i1InstanceType :: Maybe InstanceType
    , _i1LaunchTime :: Maybe ISO8601
    , _i1Placement :: Maybe Placement
    , _i1KernelId :: Maybe Text
    , _i1RamdiskId :: Maybe Text
    , _i1Platform :: Maybe PlatformValues
    , _i1Monitoring :: Maybe Monitoring
    , _i1SubnetId :: Maybe Text
    , _i1VpcId :: Maybe Text
    , _i1PrivateIpAddress :: Maybe Text
    , _i1PublicIpAddress :: Maybe Text
    , _i1StateReason :: Maybe StateReason
    , _i1Architecture :: Maybe ArchitectureValues
    , _i1RootDeviceType :: Maybe DeviceType
    , _i1RootDeviceName :: Maybe Text
    , _i1BlockDeviceMappings :: [InstanceBlockDeviceMapping]
    , _i1VirtualizationType :: Maybe VirtualizationType
    , _i1InstanceLifecycle :: Maybe InstanceLifecycleType
    , _i1SpotInstanceRequestId :: Maybe Text
    , _i1ClientToken :: Maybe Text
    , _i1Tags :: [Tag]
    , _i1SecurityGroups :: [GroupIdentifier]
    , _i1SourceDestCheck :: Maybe Bool
    , _i1Hypervisor :: Maybe HypervisorType
    , _i1NetworkInterfaces :: [InstanceNetworkInterface]
    , _i1IamInstanceProfile :: Maybe IamInstanceProfile
    , _i1EbsOptimized :: Maybe Bool
    , _i1SriovNetSupport :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Instance' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InstanceId ::@ @Maybe Text@
--
-- * @ImageId ::@ @Maybe Text@
--
-- * @State ::@ @Maybe InstanceState@
--
-- * @PrivateDnsName ::@ @Maybe Text@
--
-- * @PublicDnsName ::@ @Maybe Text@
--
-- * @StateTransitionReason ::@ @Maybe Text@
--
-- * @KeyName ::@ @Maybe Text@
--
-- * @AmiLaunchIndex ::@ @Maybe Integer@
--
-- * @ProductCodes ::@ @[ProductCode]@
--
-- * @InstanceType ::@ @Maybe InstanceType@
--
-- * @LaunchTime ::@ @Maybe ISO8601@
--
-- * @Placement ::@ @Maybe Placement@
--
-- * @KernelId ::@ @Maybe Text@
--
-- * @RamdiskId ::@ @Maybe Text@
--
-- * @Platform ::@ @Maybe PlatformValues@
--
-- * @Monitoring ::@ @Maybe Monitoring@
--
-- * @SubnetId ::@ @Maybe Text@
--
-- * @VpcId ::@ @Maybe Text@
--
-- * @PrivateIpAddress ::@ @Maybe Text@
--
-- * @PublicIpAddress ::@ @Maybe Text@
--
-- * @StateReason ::@ @Maybe StateReason@
--
-- * @Architecture ::@ @Maybe ArchitectureValues@
--
-- * @RootDeviceType ::@ @Maybe DeviceType@
--
-- * @RootDeviceName ::@ @Maybe Text@
--
-- * @BlockDeviceMappings ::@ @[InstanceBlockDeviceMapping]@
--
-- * @VirtualizationType ::@ @Maybe VirtualizationType@
--
-- * @InstanceLifecycle ::@ @Maybe InstanceLifecycleType@
--
-- * @SpotInstanceRequestId ::@ @Maybe Text@
--
-- * @ClientToken ::@ @Maybe Text@
--
-- * @Tags ::@ @[Tag]@
--
-- * @SecurityGroups ::@ @[GroupIdentifier]@
--
-- * @SourceDestCheck ::@ @Maybe Bool@
--
-- * @Hypervisor ::@ @Maybe HypervisorType@
--
-- * @NetworkInterfaces ::@ @[InstanceNetworkInterface]@
--
-- * @IamInstanceProfile ::@ @Maybe IamInstanceProfile@
--
-- * @EbsOptimized ::@ @Maybe Bool@
--
-- * @SriovNetSupport ::@ @Maybe Text@
--
mkInstance :: Instance
mkInstance = Instance
    { _i1InstanceId = Nothing
    , _i1ImageId = Nothing
    , _i1State = Nothing
    , _i1PrivateDnsName = Nothing
    , _i1PublicDnsName = Nothing
    , _i1StateTransitionReason = Nothing
    , _i1KeyName = Nothing
    , _i1AmiLaunchIndex = Nothing
    , _i1ProductCodes = mempty
    , _i1InstanceType = Nothing
    , _i1LaunchTime = Nothing
    , _i1Placement = Nothing
    , _i1KernelId = Nothing
    , _i1RamdiskId = Nothing
    , _i1Platform = Nothing
    , _i1Monitoring = Nothing
    , _i1SubnetId = Nothing
    , _i1VpcId = Nothing
    , _i1PrivateIpAddress = Nothing
    , _i1PublicIpAddress = Nothing
    , _i1StateReason = Nothing
    , _i1Architecture = Nothing
    , _i1RootDeviceType = Nothing
    , _i1RootDeviceName = Nothing
    , _i1BlockDeviceMappings = mempty
    , _i1VirtualizationType = Nothing
    , _i1InstanceLifecycle = Nothing
    , _i1SpotInstanceRequestId = Nothing
    , _i1ClientToken = Nothing
    , _i1Tags = mempty
    , _i1SecurityGroups = mempty
    , _i1SourceDestCheck = Nothing
    , _i1Hypervisor = Nothing
    , _i1NetworkInterfaces = mempty
    , _i1IamInstanceProfile = Nothing
    , _i1EbsOptimized = Nothing
    , _i1SriovNetSupport = Nothing
    }

-- | The ID of the instance.
i1InstanceId :: Lens' Instance (Maybe Text)
i1InstanceId = lens _i1InstanceId (\s a -> s { _i1InstanceId = a })

-- | The ID of the AMI used to launch the instance.
i1ImageId :: Lens' Instance (Maybe Text)
i1ImageId = lens _i1ImageId (\s a -> s { _i1ImageId = a })

-- | The current state of the instance.
i1State :: Lens' Instance (Maybe InstanceState)
i1State = lens _i1State (\s a -> s { _i1State = a })

-- | The private DNS name assigned to the instance. This DNS name can only be
-- used inside the Amazon EC2 network. This name is not available until the
-- instance enters the running state.
i1PrivateDnsName :: Lens' Instance (Maybe Text)
i1PrivateDnsName =
    lens _i1PrivateDnsName (\s a -> s { _i1PrivateDnsName = a })

-- | The public DNS name assigned to the instance. This name is not available
-- until the instance enters the running state.
i1PublicDnsName :: Lens' Instance (Maybe Text)
i1PublicDnsName = lens _i1PublicDnsName (\s a -> s { _i1PublicDnsName = a })

-- | The reason for the most recent state transition. This might be an empty
-- string.
i1StateTransitionReason :: Lens' Instance (Maybe Text)
i1StateTransitionReason =
    lens _i1StateTransitionReason
         (\s a -> s { _i1StateTransitionReason = a })

-- | The name of the key pair, if this instance was launched with an associated
-- key pair.
i1KeyName :: Lens' Instance (Maybe Text)
i1KeyName = lens _i1KeyName (\s a -> s { _i1KeyName = a })

-- | The AMI launch index, which can be used to find this instance in the launch
-- group.
i1AmiLaunchIndex :: Lens' Instance (Maybe Integer)
i1AmiLaunchIndex =
    lens _i1AmiLaunchIndex (\s a -> s { _i1AmiLaunchIndex = a })

-- | The product codes attached to this instance.
i1ProductCodes :: Lens' Instance [ProductCode]
i1ProductCodes = lens _i1ProductCodes (\s a -> s { _i1ProductCodes = a })

-- | The instance type.
i1InstanceType :: Lens' Instance (Maybe InstanceType)
i1InstanceType = lens _i1InstanceType (\s a -> s { _i1InstanceType = a })

-- | The time the instance was launched.
i1LaunchTime :: Lens' Instance (Maybe ISO8601)
i1LaunchTime = lens _i1LaunchTime (\s a -> s { _i1LaunchTime = a })

-- | The location where the instance launched.
i1Placement :: Lens' Instance (Maybe Placement)
i1Placement = lens _i1Placement (\s a -> s { _i1Placement = a })

-- | The kernel associated with this instance.
i1KernelId :: Lens' Instance (Maybe Text)
i1KernelId = lens _i1KernelId (\s a -> s { _i1KernelId = a })

-- | The RAM disk associated with this instance.
i1RamdiskId :: Lens' Instance (Maybe Text)
i1RamdiskId = lens _i1RamdiskId (\s a -> s { _i1RamdiskId = a })

-- | The value is Windows for Windows instances; otherwise blank.
i1Platform :: Lens' Instance (Maybe PlatformValues)
i1Platform = lens _i1Platform (\s a -> s { _i1Platform = a })

-- | The monitoring information for the instance.
i1Monitoring :: Lens' Instance (Maybe Monitoring)
i1Monitoring = lens _i1Monitoring (\s a -> s { _i1Monitoring = a })

-- | The ID of the subnet in which the instance is running.
i1SubnetId :: Lens' Instance (Maybe Text)
i1SubnetId = lens _i1SubnetId (\s a -> s { _i1SubnetId = a })

-- | The ID of the VPC in which the instance is running.
i1VpcId :: Lens' Instance (Maybe Text)
i1VpcId = lens _i1VpcId (\s a -> s { _i1VpcId = a })

-- | The private IP address assigned to the instance.
i1PrivateIpAddress :: Lens' Instance (Maybe Text)
i1PrivateIpAddress =
    lens _i1PrivateIpAddress (\s a -> s { _i1PrivateIpAddress = a })

-- | The public IP address assigned to the instance.
i1PublicIpAddress :: Lens' Instance (Maybe Text)
i1PublicIpAddress =
    lens _i1PublicIpAddress (\s a -> s { _i1PublicIpAddress = a })

-- | The reason for the most recent state transition.
i1StateReason :: Lens' Instance (Maybe StateReason)
i1StateReason = lens _i1StateReason (\s a -> s { _i1StateReason = a })

-- | The architecture of the image.
i1Architecture :: Lens' Instance (Maybe ArchitectureValues)
i1Architecture = lens _i1Architecture (\s a -> s { _i1Architecture = a })

-- | The root device type used by the AMI. The AMI can use an Amazon EBS volume
-- or an instance store volume.
i1RootDeviceType :: Lens' Instance (Maybe DeviceType)
i1RootDeviceType =
    lens _i1RootDeviceType (\s a -> s { _i1RootDeviceType = a })

-- | The root device name (for example, /dev/sda1).
i1RootDeviceName :: Lens' Instance (Maybe Text)
i1RootDeviceName =
    lens _i1RootDeviceName (\s a -> s { _i1RootDeviceName = a })

-- | Any block device mapping entries for the instance.
i1BlockDeviceMappings :: Lens' Instance [InstanceBlockDeviceMapping]
i1BlockDeviceMappings =
    lens _i1BlockDeviceMappings (\s a -> s { _i1BlockDeviceMappings = a })

-- | The virtualization type of the instance.
i1VirtualizationType :: Lens' Instance (Maybe VirtualizationType)
i1VirtualizationType =
    lens _i1VirtualizationType (\s a -> s { _i1VirtualizationType = a })

-- | Indicates whether this is a Spot Instance.
i1InstanceLifecycle :: Lens' Instance (Maybe InstanceLifecycleType)
i1InstanceLifecycle =
    lens _i1InstanceLifecycle (\s a -> s { _i1InstanceLifecycle = a })

-- | The ID of the Spot Instance request.
i1SpotInstanceRequestId :: Lens' Instance (Maybe Text)
i1SpotInstanceRequestId =
    lens _i1SpotInstanceRequestId
         (\s a -> s { _i1SpotInstanceRequestId = a })

-- | The idempotency token you provided when you launched the instance.
i1ClientToken :: Lens' Instance (Maybe Text)
i1ClientToken = lens _i1ClientToken (\s a -> s { _i1ClientToken = a })

-- | Any tags assigned to the instance.
i1Tags :: Lens' Instance [Tag]
i1Tags = lens _i1Tags (\s a -> s { _i1Tags = a })

-- | One or more security groups for the instance.
i1SecurityGroups :: Lens' Instance [GroupIdentifier]
i1SecurityGroups =
    lens _i1SecurityGroups (\s a -> s { _i1SecurityGroups = a })

-- | Specifies whether to enable an instance launched in a VPC to perform NAT.
-- This controls whether source/destination checking is enabled on the
-- instance. A value of true means checking is enabled, and false means
-- checking is disabled. The value must be false for the instance to perform
-- NAT. For more information, see NAT Instances in the Amazon Virtual Private
-- Cloud User Guide.
i1SourceDestCheck :: Lens' Instance (Maybe Bool)
i1SourceDestCheck =
    lens _i1SourceDestCheck (\s a -> s { _i1SourceDestCheck = a })

-- | The hypervisor type of the instance.
i1Hypervisor :: Lens' Instance (Maybe HypervisorType)
i1Hypervisor = lens _i1Hypervisor (\s a -> s { _i1Hypervisor = a })

-- | [EC2-VPC] One or more network interfaces for the instance.
i1NetworkInterfaces :: Lens' Instance [InstanceNetworkInterface]
i1NetworkInterfaces =
    lens _i1NetworkInterfaces (\s a -> s { _i1NetworkInterfaces = a })

-- | The IAM instance profile associated with the instance.
i1IamInstanceProfile :: Lens' Instance (Maybe IamInstanceProfile)
i1IamInstanceProfile =
    lens _i1IamInstanceProfile (\s a -> s { _i1IamInstanceProfile = a })

-- | Indicates whether the instance is optimized for EBS I/O. This optimization
-- provides dedicated throughput to Amazon EBS and an optimized configuration
-- stack to provide optimal I/O performance. This optimization isn't available
-- with all instance types. Additional usage charges apply when using an EBS
-- Optimized instance.
i1EbsOptimized :: Lens' Instance (Maybe Bool)
i1EbsOptimized = lens _i1EbsOptimized (\s a -> s { _i1EbsOptimized = a })

-- | Specifies whether enhanced networking is enabled.
i1SriovNetSupport :: Lens' Instance (Maybe Text)
i1SriovNetSupport =
    lens _i1SriovNetSupport (\s a -> s { _i1SriovNetSupport = a })

instance FromXML Instance where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery Instance where
    toQuery = genericQuery def

-- | Describes a block device mapping.
data InstanceBlockDeviceMapping = InstanceBlockDeviceMapping
    { _ibdmDeviceName :: Maybe Text
    , _ibdmEbs :: Maybe EbsInstanceBlockDevice
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceBlockDeviceMapping' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DeviceName ::@ @Maybe Text@
--
-- * @Ebs ::@ @Maybe EbsInstanceBlockDevice@
--
mkInstanceBlockDeviceMapping :: InstanceBlockDeviceMapping
mkInstanceBlockDeviceMapping = InstanceBlockDeviceMapping
    { _ibdmDeviceName = Nothing
    , _ibdmEbs = Nothing
    }

-- | The device name exposed to the instance (for example, /dev/sdh).
ibdmDeviceName :: Lens' InstanceBlockDeviceMapping (Maybe Text)
ibdmDeviceName = lens _ibdmDeviceName (\s a -> s { _ibdmDeviceName = a })

-- | Parameters used to automatically set up Amazon EBS volumes when the
-- instance is launched.
ibdmEbs :: Lens' InstanceBlockDeviceMapping (Maybe EbsInstanceBlockDevice)
ibdmEbs = lens _ibdmEbs (\s a -> s { _ibdmEbs = a })

instance FromXML InstanceBlockDeviceMapping where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery InstanceBlockDeviceMapping where
    toQuery = genericQuery def

-- | Describes a block device mapping entry.
data InstanceBlockDeviceMappingSpecification = InstanceBlockDeviceMappingSpecification
    { _ibdmsDeviceName :: Maybe Text
    , _ibdmsEbs :: Maybe EbsInstanceBlockDeviceSpecification
    , _ibdmsVirtualName :: Maybe Text
    , _ibdmsNoDevice :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceBlockDeviceMappingSpecification' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DeviceName ::@ @Maybe Text@
--
-- * @Ebs ::@ @Maybe EbsInstanceBlockDeviceSpecification@
--
-- * @VirtualName ::@ @Maybe Text@
--
-- * @NoDevice ::@ @Maybe Text@
--
mkInstanceBlockDeviceMappingSpecification :: InstanceBlockDeviceMappingSpecification
mkInstanceBlockDeviceMappingSpecification = InstanceBlockDeviceMappingSpecification
    { _ibdmsDeviceName = Nothing
    , _ibdmsEbs = Nothing
    , _ibdmsVirtualName = Nothing
    , _ibdmsNoDevice = Nothing
    }

-- | The device name exposed to the instance (for example, /dev/sdh).
ibdmsDeviceName :: Lens' InstanceBlockDeviceMappingSpecification (Maybe Text)
ibdmsDeviceName = lens _ibdmsDeviceName (\s a -> s { _ibdmsDeviceName = a })

-- | Parameters used to automatically set up Amazon EBS volumes when the
-- instance is launched.
ibdmsEbs :: Lens' InstanceBlockDeviceMappingSpecification (Maybe EbsInstanceBlockDeviceSpecification)
ibdmsEbs = lens _ibdmsEbs (\s a -> s { _ibdmsEbs = a })

-- | The virtual device name.
ibdmsVirtualName :: Lens' InstanceBlockDeviceMappingSpecification (Maybe Text)
ibdmsVirtualName =
    lens _ibdmsVirtualName (\s a -> s { _ibdmsVirtualName = a })

-- | suppress the specified device included in the block device mapping.
ibdmsNoDevice :: Lens' InstanceBlockDeviceMappingSpecification (Maybe Text)
ibdmsNoDevice = lens _ibdmsNoDevice (\s a -> s { _ibdmsNoDevice = a })

instance ToQuery InstanceBlockDeviceMappingSpecification where
    toQuery = genericQuery def

-- | Describes a Reserved Instance listing state.
data InstanceCount = InstanceCount
    { _icState :: Maybe ListingState
    , _icInstanceCount :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceCount' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @State ::@ @Maybe ListingState@
--
-- * @InstanceCount ::@ @Maybe Integer@
--
mkInstanceCount :: InstanceCount
mkInstanceCount = InstanceCount
    { _icState = Nothing
    , _icInstanceCount = Nothing
    }

-- | The states of the listed Reserved Instances.
icState :: Lens' InstanceCount (Maybe ListingState)
icState = lens _icState (\s a -> s { _icState = a })

-- | he number of listed Reserved Instances in the state specified by the state.
icInstanceCount :: Lens' InstanceCount (Maybe Integer)
icInstanceCount = lens _icInstanceCount (\s a -> s { _icInstanceCount = a })

instance FromXML InstanceCount where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery InstanceCount where
    toQuery = genericQuery def

-- | The instance being exported.
data InstanceExportDetails = InstanceExportDetails
    { _iedInstanceId :: Maybe Text
    , _iedTargetEnvironment :: Maybe ExportEnvironment
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceExportDetails' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InstanceId ::@ @Maybe Text@
--
-- * @TargetEnvironment ::@ @Maybe ExportEnvironment@
--
mkInstanceExportDetails :: InstanceExportDetails
mkInstanceExportDetails = InstanceExportDetails
    { _iedInstanceId = Nothing
    , _iedTargetEnvironment = Nothing
    }

-- | The ID of the resource being exported.
iedInstanceId :: Lens' InstanceExportDetails (Maybe Text)
iedInstanceId = lens _iedInstanceId (\s a -> s { _iedInstanceId = a })

-- | The target virtualization environment.
iedTargetEnvironment :: Lens' InstanceExportDetails (Maybe ExportEnvironment)
iedTargetEnvironment =
    lens _iedTargetEnvironment (\s a -> s { _iedTargetEnvironment = a })

instance FromXML InstanceExportDetails where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "instanceExport"

instance ToQuery InstanceExportDetails where
    toQuery = genericQuery def

-- | Describes the monitoring information of the instance.
data InstanceMonitoring = InstanceMonitoring
    { _imInstanceId :: Maybe Text
    , _imMonitoring :: Maybe Monitoring
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceMonitoring' data type.
--
-- 'InstanceMonitoring' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InstanceId ::@ @Maybe Text@
--
-- * @Monitoring ::@ @Maybe Monitoring@
--
mkInstanceMonitoring :: InstanceMonitoring
mkInstanceMonitoring = InstanceMonitoring
    { _imInstanceId = Nothing
    , _imMonitoring = Nothing
    }

-- | The ID of the instance.
imInstanceId :: Lens' InstanceMonitoring (Maybe Text)
imInstanceId = lens _imInstanceId (\s a -> s { _imInstanceId = a })

-- | The monitoring information.
imMonitoring :: Lens' InstanceMonitoring (Maybe Monitoring)
imMonitoring = lens _imMonitoring (\s a -> s { _imMonitoring = a })

instance FromXML InstanceMonitoring where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes a network interface.
data InstanceNetworkInterface = InstanceNetworkInterface
    { _iniNetworkInterfaceId :: Maybe Text
    , _iniSubnetId :: Maybe Text
    , _iniVpcId :: Maybe Text
    , _iniDescription :: Maybe Text
    , _iniOwnerId :: Maybe Text
    , _iniStatus :: Maybe NetworkInterfaceStatus
    , _iniPrivateIpAddress :: Maybe Text
    , _iniPrivateDnsName :: Maybe Text
    , _iniSourceDestCheck :: Maybe Bool
    , _iniGroups :: [GroupIdentifier]
    , _iniAttachment :: Maybe InstanceNetworkInterfaceAttachment
    , _iniAssociation :: Maybe InstanceNetworkInterfaceAssociation
    , _iniPrivateIpAddresses :: [InstancePrivateIpAddress]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceNetworkInterface' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @NetworkInterfaceId ::@ @Maybe Text@
--
-- * @SubnetId ::@ @Maybe Text@
--
-- * @VpcId ::@ @Maybe Text@
--
-- * @Description ::@ @Maybe Text@
--
-- * @OwnerId ::@ @Maybe Text@
--
-- * @Status ::@ @Maybe NetworkInterfaceStatus@
--
-- * @PrivateIpAddress ::@ @Maybe Text@
--
-- * @PrivateDnsName ::@ @Maybe Text@
--
-- * @SourceDestCheck ::@ @Maybe Bool@
--
-- * @Groups ::@ @[GroupIdentifier]@
--
-- * @Attachment ::@ @Maybe InstanceNetworkInterfaceAttachment@
--
-- * @Association ::@ @Maybe InstanceNetworkInterfaceAssociation@
--
-- * @PrivateIpAddresses ::@ @[InstancePrivateIpAddress]@
--
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

-- | The ID of the network interface.
iniNetworkInterfaceId :: Lens' InstanceNetworkInterface (Maybe Text)
iniNetworkInterfaceId =
    lens _iniNetworkInterfaceId (\s a -> s { _iniNetworkInterfaceId = a })

-- | The ID of the subnet.
iniSubnetId :: Lens' InstanceNetworkInterface (Maybe Text)
iniSubnetId = lens _iniSubnetId (\s a -> s { _iniSubnetId = a })

-- | The ID of the VPC.
iniVpcId :: Lens' InstanceNetworkInterface (Maybe Text)
iniVpcId = lens _iniVpcId (\s a -> s { _iniVpcId = a })

-- | The description.
iniDescription :: Lens' InstanceNetworkInterface (Maybe Text)
iniDescription = lens _iniDescription (\s a -> s { _iniDescription = a })

-- | The ID of the AWS account that created the network interface.
iniOwnerId :: Lens' InstanceNetworkInterface (Maybe Text)
iniOwnerId = lens _iniOwnerId (\s a -> s { _iniOwnerId = a })

-- | The status of the network interface.
iniStatus :: Lens' InstanceNetworkInterface (Maybe NetworkInterfaceStatus)
iniStatus = lens _iniStatus (\s a -> s { _iniStatus = a })

-- | The IP address of the network interface within the subnet.
iniPrivateIpAddress :: Lens' InstanceNetworkInterface (Maybe Text)
iniPrivateIpAddress =
    lens _iniPrivateIpAddress (\s a -> s { _iniPrivateIpAddress = a })

-- | The private DNS name.
iniPrivateDnsName :: Lens' InstanceNetworkInterface (Maybe Text)
iniPrivateDnsName =
    lens _iniPrivateDnsName (\s a -> s { _iniPrivateDnsName = a })

-- | Indicates whether to validate network traffic to or from this network
-- interface.
iniSourceDestCheck :: Lens' InstanceNetworkInterface (Maybe Bool)
iniSourceDestCheck =
    lens _iniSourceDestCheck (\s a -> s { _iniSourceDestCheck = a })

-- | One or more security groups.
iniGroups :: Lens' InstanceNetworkInterface [GroupIdentifier]
iniGroups = lens _iniGroups (\s a -> s { _iniGroups = a })

-- | The network interface attachment.
iniAttachment :: Lens' InstanceNetworkInterface (Maybe InstanceNetworkInterfaceAttachment)
iniAttachment = lens _iniAttachment (\s a -> s { _iniAttachment = a })

-- | The association information for an Elastic IP associated with the network
-- interface.
iniAssociation :: Lens' InstanceNetworkInterface (Maybe InstanceNetworkInterfaceAssociation)
iniAssociation = lens _iniAssociation (\s a -> s { _iniAssociation = a })

-- | The private IP addresses associated with the network interface.
iniPrivateIpAddresses :: Lens' InstanceNetworkInterface [InstancePrivateIpAddress]
iniPrivateIpAddresses =
    lens _iniPrivateIpAddresses (\s a -> s { _iniPrivateIpAddresses = a })

instance FromXML InstanceNetworkInterface where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery InstanceNetworkInterface where
    toQuery = genericQuery def

-- | The association information for an Elastic IP associated with the network
-- interface.
data InstanceNetworkInterfaceAssociation = InstanceNetworkInterfaceAssociation
    { _inia1PublicIp :: Maybe Text
    , _inia1PublicDnsName :: Maybe Text
    , _inia1IpOwnerId :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceNetworkInterfaceAssociation' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @PublicIp ::@ @Maybe Text@
--
-- * @PublicDnsName ::@ @Maybe Text@
--
-- * @IpOwnerId ::@ @Maybe Text@
--
mkInstanceNetworkInterfaceAssociation :: InstanceNetworkInterfaceAssociation
mkInstanceNetworkInterfaceAssociation = InstanceNetworkInterfaceAssociation
    { _inia1PublicIp = Nothing
    , _inia1PublicDnsName = Nothing
    , _inia1IpOwnerId = Nothing
    }

-- | The address of the Elastic IP address bound to the network interface.
inia1PublicIp :: Lens' InstanceNetworkInterfaceAssociation (Maybe Text)
inia1PublicIp = lens _inia1PublicIp (\s a -> s { _inia1PublicIp = a })

-- | The public DNS name.
inia1PublicDnsName :: Lens' InstanceNetworkInterfaceAssociation (Maybe Text)
inia1PublicDnsName =
    lens _inia1PublicDnsName (\s a -> s { _inia1PublicDnsName = a })

-- | The ID of the owner of the Elastic IP address.
inia1IpOwnerId :: Lens' InstanceNetworkInterfaceAssociation (Maybe Text)
inia1IpOwnerId = lens _inia1IpOwnerId (\s a -> s { _inia1IpOwnerId = a })

instance FromXML InstanceNetworkInterfaceAssociation where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "association"

instance ToQuery InstanceNetworkInterfaceAssociation where
    toQuery = genericQuery def

-- | The network interface attachment.
data InstanceNetworkInterfaceAttachment = InstanceNetworkInterfaceAttachment
    { _iniaAttachmentId :: Maybe Text
    , _iniaDeviceIndex :: Maybe Integer
    , _iniaStatus :: Maybe AttachmentStatus
    , _iniaAttachTime :: Maybe ISO8601
    , _iniaDeleteOnTermination :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceNetworkInterfaceAttachment' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AttachmentId ::@ @Maybe Text@
--
-- * @DeviceIndex ::@ @Maybe Integer@
--
-- * @Status ::@ @Maybe AttachmentStatus@
--
-- * @AttachTime ::@ @Maybe ISO8601@
--
-- * @DeleteOnTermination ::@ @Maybe Bool@
--
mkInstanceNetworkInterfaceAttachment :: InstanceNetworkInterfaceAttachment
mkInstanceNetworkInterfaceAttachment = InstanceNetworkInterfaceAttachment
    { _iniaAttachmentId = Nothing
    , _iniaDeviceIndex = Nothing
    , _iniaStatus = Nothing
    , _iniaAttachTime = Nothing
    , _iniaDeleteOnTermination = Nothing
    }

-- | The ID of the network interface attachment.
iniaAttachmentId :: Lens' InstanceNetworkInterfaceAttachment (Maybe Text)
iniaAttachmentId =
    lens _iniaAttachmentId (\s a -> s { _iniaAttachmentId = a })

-- | The index of the device on the instance for the network interface
-- attachment.
iniaDeviceIndex :: Lens' InstanceNetworkInterfaceAttachment (Maybe Integer)
iniaDeviceIndex = lens _iniaDeviceIndex (\s a -> s { _iniaDeviceIndex = a })

-- | The attachment state.
iniaStatus :: Lens' InstanceNetworkInterfaceAttachment (Maybe AttachmentStatus)
iniaStatus = lens _iniaStatus (\s a -> s { _iniaStatus = a })

-- | The time stamp when the attachment initiated.
iniaAttachTime :: Lens' InstanceNetworkInterfaceAttachment (Maybe ISO8601)
iniaAttachTime = lens _iniaAttachTime (\s a -> s { _iniaAttachTime = a })

-- | Indicates whether the network interface is deleted when the instance is
-- terminated.
iniaDeleteOnTermination :: Lens' InstanceNetworkInterfaceAttachment (Maybe Bool)
iniaDeleteOnTermination =
    lens _iniaDeleteOnTermination
         (\s a -> s { _iniaDeleteOnTermination = a })

instance FromXML InstanceNetworkInterfaceAttachment where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "attachment"

instance ToQuery InstanceNetworkInterfaceAttachment where
    toQuery = genericQuery def

-- | Describes a network interface.
data InstanceNetworkInterfaceSpecification = InstanceNetworkInterfaceSpecification
    { _inisNetworkInterfaceId :: Maybe Text
    , _inisDeviceIndex :: Maybe Integer
    , _inisSubnetId :: Maybe Text
    , _inisDescription :: Maybe Text
    , _inisPrivateIpAddress :: Maybe Text
    , _inisGroups :: [Text]
    , _inisDeleteOnTermination :: Maybe Bool
    , _inisPrivateIpAddresses :: [PrivateIpAddressSpecification]
    , _inisSecondaryPrivateIpAddressCount :: Maybe Integer
    , _inisAssociatePublicIpAddress :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceNetworkInterfaceSpecification' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @NetworkInterfaceId ::@ @Maybe Text@
--
-- * @DeviceIndex ::@ @Maybe Integer@
--
-- * @SubnetId ::@ @Maybe Text@
--
-- * @Description ::@ @Maybe Text@
--
-- * @PrivateIpAddress ::@ @Maybe Text@
--
-- * @Groups ::@ @[Text]@
--
-- * @DeleteOnTermination ::@ @Maybe Bool@
--
-- * @PrivateIpAddresses ::@ @[PrivateIpAddressSpecification]@
--
-- * @SecondaryPrivateIpAddressCount ::@ @Maybe Integer@
--
-- * @AssociatePublicIpAddress ::@ @Maybe Bool@
--
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

-- | The ID of the network interface.
inisNetworkInterfaceId :: Lens' InstanceNetworkInterfaceSpecification (Maybe Text)
inisNetworkInterfaceId =
    lens _inisNetworkInterfaceId (\s a -> s { _inisNetworkInterfaceId = a })

-- | The index of the device on the instance for the network interface
-- attachment.
inisDeviceIndex :: Lens' InstanceNetworkInterfaceSpecification (Maybe Integer)
inisDeviceIndex = lens _inisDeviceIndex (\s a -> s { _inisDeviceIndex = a })

-- | The ID of the subnet associated with the network string.
inisSubnetId :: Lens' InstanceNetworkInterfaceSpecification (Maybe Text)
inisSubnetId = lens _inisSubnetId (\s a -> s { _inisSubnetId = a })

-- | The description of the network interface.
inisDescription :: Lens' InstanceNetworkInterfaceSpecification (Maybe Text)
inisDescription = lens _inisDescription (\s a -> s { _inisDescription = a })

-- | The private IP address of the network interface.
inisPrivateIpAddress :: Lens' InstanceNetworkInterfaceSpecification (Maybe Text)
inisPrivateIpAddress =
    lens _inisPrivateIpAddress (\s a -> s { _inisPrivateIpAddress = a })

-- | The IDs of the security groups for the network interface.
inisGroups :: Lens' InstanceNetworkInterfaceSpecification [Text]
inisGroups = lens _inisGroups (\s a -> s { _inisGroups = a })

-- | If set to true, the interface is deleted when the instance is terminated.
inisDeleteOnTermination :: Lens' InstanceNetworkInterfaceSpecification (Maybe Bool)
inisDeleteOnTermination =
    lens _inisDeleteOnTermination
         (\s a -> s { _inisDeleteOnTermination = a })

-- | One or more private IP addresses to assign to the network interface.
inisPrivateIpAddresses :: Lens' InstanceNetworkInterfaceSpecification [PrivateIpAddressSpecification]
inisPrivateIpAddresses =
    lens _inisPrivateIpAddresses (\s a -> s { _inisPrivateIpAddresses = a })

-- | The number of secondary private IP addresses.
inisSecondaryPrivateIpAddressCount :: Lens' InstanceNetworkInterfaceSpecification (Maybe Integer)
inisSecondaryPrivateIpAddressCount =
    lens _inisSecondaryPrivateIpAddressCount
         (\s a -> s { _inisSecondaryPrivateIpAddressCount = a })

-- | Indicates whether to auto-assign a public IP address to an instance in a
-- VPC. This public IP address can be assigned to the network interface for
-- eth0 only when you launch the instance. You must create the network
-- interface instead of using an existing network interface for eth0, and you
-- must not specify more than one network interface.
inisAssociatePublicIpAddress :: Lens' InstanceNetworkInterfaceSpecification (Maybe Bool)
inisAssociatePublicIpAddress =
    lens _inisAssociatePublicIpAddress
         (\s a -> s { _inisAssociatePublicIpAddress = a })

instance FromXML InstanceNetworkInterfaceSpecification where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery InstanceNetworkInterfaceSpecification where
    toQuery = genericQuery def

-- | Describes a private IP address.
data InstancePrivateIpAddress = InstancePrivateIpAddress
    { _ipiaPrivateIpAddress :: Maybe Text
    , _ipiaPrivateDnsName :: Maybe Text
    , _ipiaPrimary :: Maybe Bool
    , _ipiaAssociation :: Maybe InstanceNetworkInterfaceAssociation
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstancePrivateIpAddress' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @PrivateIpAddress ::@ @Maybe Text@
--
-- * @PrivateDnsName ::@ @Maybe Text@
--
-- * @Primary ::@ @Maybe Bool@
--
-- * @Association ::@ @Maybe InstanceNetworkInterfaceAssociation@
--
mkInstancePrivateIpAddress :: InstancePrivateIpAddress
mkInstancePrivateIpAddress = InstancePrivateIpAddress
    { _ipiaPrivateIpAddress = Nothing
    , _ipiaPrivateDnsName = Nothing
    , _ipiaPrimary = Nothing
    , _ipiaAssociation = Nothing
    }

-- | The private IP address of the network interface.
ipiaPrivateIpAddress :: Lens' InstancePrivateIpAddress (Maybe Text)
ipiaPrivateIpAddress =
    lens _ipiaPrivateIpAddress (\s a -> s { _ipiaPrivateIpAddress = a })

-- | The private DNS name.
ipiaPrivateDnsName :: Lens' InstancePrivateIpAddress (Maybe Text)
ipiaPrivateDnsName =
    lens _ipiaPrivateDnsName (\s a -> s { _ipiaPrivateDnsName = a })

-- | Indicates whether this IP address is the primary private IP address of the
-- network interface.
ipiaPrimary :: Lens' InstancePrivateIpAddress (Maybe Bool)
ipiaPrimary = lens _ipiaPrimary (\s a -> s { _ipiaPrimary = a })

-- | The association information for an Elastic IP address for the network
-- interface.
ipiaAssociation :: Lens' InstancePrivateIpAddress (Maybe InstanceNetworkInterfaceAssociation)
ipiaAssociation = lens _ipiaAssociation (\s a -> s { _ipiaAssociation = a })

instance FromXML InstancePrivateIpAddress where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery InstancePrivateIpAddress where
    toQuery = genericQuery def

-- | The intended state of the instance. DescribeInstanceStatus requires that an
-- instance be in the running state.
data InstanceState = InstanceState
    { _is1Code :: Integer
    , _is1Name :: InstanceStateName
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceState' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Code ::@ @Integer@
--
-- * @Name ::@ @InstanceStateName@
--
mkInstanceState :: Integer -- ^ 'is1Code'
                -> InstanceStateName -- ^ 'is1Name'
                -> InstanceState
mkInstanceState p1 p2 = InstanceState
    { _is1Code = p1
    , _is1Name = p2
    }

-- | The low byte represents the state. The high byte is an opaque internal
-- value and should be ignored. 0 : pending 16 : running 32 : shutting-down 48
-- : terminated 64 : stopping 80 : stopped.
is1Code :: Lens' InstanceState Integer
is1Code = lens _is1Code (\s a -> s { _is1Code = a })

-- | The current state of the instance.
is1Name :: Lens' InstanceState InstanceStateName
is1Name = lens _is1Name (\s a -> s { _is1Name = a })

instance FromXML InstanceState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "instanceState"

instance ToQuery InstanceState where
    toQuery = genericQuery def

-- | Describes an instance state change.
data InstanceStateChange = InstanceStateChange
    { _iscInstanceId :: Maybe Text
    , _iscCurrentState :: Maybe InstanceState
    , _iscPreviousState :: Maybe InstanceState
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceStateChange' data type.
--
-- 'InstanceStateChange' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InstanceId ::@ @Maybe Text@
--
-- * @CurrentState ::@ @Maybe InstanceState@
--
-- * @PreviousState ::@ @Maybe InstanceState@
--
mkInstanceStateChange :: InstanceStateChange
mkInstanceStateChange = InstanceStateChange
    { _iscInstanceId = Nothing
    , _iscCurrentState = Nothing
    , _iscPreviousState = Nothing
    }

-- | The ID of the instance.
iscInstanceId :: Lens' InstanceStateChange (Maybe Text)
iscInstanceId = lens _iscInstanceId (\s a -> s { _iscInstanceId = a })

-- | The current state of the instance.
iscCurrentState :: Lens' InstanceStateChange (Maybe InstanceState)
iscCurrentState = lens _iscCurrentState (\s a -> s { _iscCurrentState = a })

-- | The previous state of the instance.
iscPreviousState :: Lens' InstanceStateChange (Maybe InstanceState)
iscPreviousState =
    lens _iscPreviousState (\s a -> s { _iscPreviousState = a })

instance FromXML InstanceStateChange where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes the status of an instance.
data InstanceStatus = InstanceStatus
    { _isInstanceId :: Maybe Text
    , _isAvailabilityZone :: Maybe Text
    , _isEvents :: [InstanceStatusEvent]
    , _isInstanceState :: Maybe InstanceState
    , _isSystemStatus :: Maybe InstanceStatusSummary
    , _isInstanceStatus :: Maybe InstanceStatusSummary
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceStatus' data type.
--
-- 'InstanceStatus' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InstanceId ::@ @Maybe Text@
--
-- * @AvailabilityZone ::@ @Maybe Text@
--
-- * @Events ::@ @[InstanceStatusEvent]@
--
-- * @InstanceState ::@ @Maybe InstanceState@
--
-- * @SystemStatus ::@ @Maybe InstanceStatusSummary@
--
-- * @InstanceStatus ::@ @Maybe InstanceStatusSummary@
--
mkInstanceStatus :: InstanceStatus
mkInstanceStatus = InstanceStatus
    { _isInstanceId = Nothing
    , _isAvailabilityZone = Nothing
    , _isEvents = mempty
    , _isInstanceState = Nothing
    , _isSystemStatus = Nothing
    , _isInstanceStatus = Nothing
    }

-- | The ID of the instance.
isInstanceId :: Lens' InstanceStatus (Maybe Text)
isInstanceId = lens _isInstanceId (\s a -> s { _isInstanceId = a })

-- | The Availability Zone of the instance.
isAvailabilityZone :: Lens' InstanceStatus (Maybe Text)
isAvailabilityZone =
    lens _isAvailabilityZone (\s a -> s { _isAvailabilityZone = a })

-- | Extra information regarding events associated with the instance.
isEvents :: Lens' InstanceStatus [InstanceStatusEvent]
isEvents = lens _isEvents (\s a -> s { _isEvents = a })

-- | The intended state of the instance. DescribeInstanceStatus requires that an
-- instance be in the running state.
isInstanceState :: Lens' InstanceStatus (Maybe InstanceState)
isInstanceState = lens _isInstanceState (\s a -> s { _isInstanceState = a })

-- | Reports impaired functionality that stems from issues related to the
-- systems that support an instance, such as hardware failures and network
-- connectivity problems.
isSystemStatus :: Lens' InstanceStatus (Maybe InstanceStatusSummary)
isSystemStatus = lens _isSystemStatus (\s a -> s { _isSystemStatus = a })

-- | Reports impaired functionality that stems from issues internal to the
-- instance, such as impaired reachability.
isInstanceStatus :: Lens' InstanceStatus (Maybe InstanceStatusSummary)
isInstanceStatus =
    lens _isInstanceStatus (\s a -> s { _isInstanceStatus = a })

instance FromXML InstanceStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes the instance status.
data InstanceStatusDetails = InstanceStatusDetails
    { _isdName :: Maybe StatusName
    , _isdStatus :: Maybe StatusType
    , _isdImpairedSince :: Maybe ISO8601
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceStatusDetails' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Name ::@ @Maybe StatusName@
--
-- * @Status ::@ @Maybe StatusType@
--
-- * @ImpairedSince ::@ @Maybe ISO8601@
--
mkInstanceStatusDetails :: InstanceStatusDetails
mkInstanceStatusDetails = InstanceStatusDetails
    { _isdName = Nothing
    , _isdStatus = Nothing
    , _isdImpairedSince = Nothing
    }

-- | The type of instance status.
isdName :: Lens' InstanceStatusDetails (Maybe StatusName)
isdName = lens _isdName (\s a -> s { _isdName = a })

-- | The status.
isdStatus :: Lens' InstanceStatusDetails (Maybe StatusType)
isdStatus = lens _isdStatus (\s a -> s { _isdStatus = a })

-- | The time when a status check failed. For an instance that was launched and
-- impaired, this is the time when the instance was launched.
isdImpairedSince :: Lens' InstanceStatusDetails (Maybe ISO8601)
isdImpairedSince =
    lens _isdImpairedSince (\s a -> s { _isdImpairedSince = a })

instance FromXML InstanceStatusDetails where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery InstanceStatusDetails where
    toQuery = genericQuery def

-- | Describes an instance event.
data InstanceStatusEvent = InstanceStatusEvent
    { _iseCode :: Maybe EventCode
    , _iseDescription :: Maybe Text
    , _iseNotBefore :: Maybe ISO8601
    , _iseNotAfter :: Maybe ISO8601
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceStatusEvent' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Code ::@ @Maybe EventCode@
--
-- * @Description ::@ @Maybe Text@
--
-- * @NotBefore ::@ @Maybe ISO8601@
--
-- * @NotAfter ::@ @Maybe ISO8601@
--
mkInstanceStatusEvent :: InstanceStatusEvent
mkInstanceStatusEvent = InstanceStatusEvent
    { _iseCode = Nothing
    , _iseDescription = Nothing
    , _iseNotBefore = Nothing
    , _iseNotAfter = Nothing
    }

-- | The associated code of the event.
iseCode :: Lens' InstanceStatusEvent (Maybe EventCode)
iseCode = lens _iseCode (\s a -> s { _iseCode = a })

-- | A description of the event.
iseDescription :: Lens' InstanceStatusEvent (Maybe Text)
iseDescription = lens _iseDescription (\s a -> s { _iseDescription = a })

-- | The earliest scheduled start time for the event.
iseNotBefore :: Lens' InstanceStatusEvent (Maybe ISO8601)
iseNotBefore = lens _iseNotBefore (\s a -> s { _iseNotBefore = a })

-- | The latest scheduled end time for the event.
iseNotAfter :: Lens' InstanceStatusEvent (Maybe ISO8601)
iseNotAfter = lens _iseNotAfter (\s a -> s { _iseNotAfter = a })

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
    , _issDetails :: [InstanceStatusDetails]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceStatusSummary' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Status ::@ @Maybe SummaryStatus@
--
-- * @Details ::@ @[InstanceStatusDetails]@
--
mkInstanceStatusSummary :: InstanceStatusSummary
mkInstanceStatusSummary = InstanceStatusSummary
    { _issStatus = Nothing
    , _issDetails = mempty
    }

-- | The status.
issStatus :: Lens' InstanceStatusSummary (Maybe SummaryStatus)
issStatus = lens _issStatus (\s a -> s { _issStatus = a })

-- | The system instance health or application instance health.
issDetails :: Lens' InstanceStatusSummary [InstanceStatusDetails]
issDetails = lens _issDetails (\s a -> s { _issDetails = a })

instance FromXML InstanceStatusSummary where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "systemStatus"

instance ToQuery InstanceStatusSummary where
    toQuery = genericQuery def

-- | Information about the Internet gateway.
data InternetGateway = InternetGateway
    { _igInternetGatewayId :: Text
    , _igAttachments :: [InternetGatewayAttachment]
    , _igTags :: [Tag]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InternetGateway' data type.
--
-- 'InternetGateway' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InternetGatewayId ::@ @Text@
--
-- * @Attachments ::@ @[InternetGatewayAttachment]@
--
-- * @Tags ::@ @[Tag]@
--
mkInternetGateway :: Text -- ^ 'igInternetGatewayId'
                  -> InternetGateway
mkInternetGateway p1 = InternetGateway
    { _igInternetGatewayId = p1
    , _igAttachments = mempty
    , _igTags = mempty
    }

-- | The ID of the Internet gateway.
igInternetGatewayId :: Lens' InternetGateway Text
igInternetGatewayId =
    lens _igInternetGatewayId (\s a -> s { _igInternetGatewayId = a })

-- | Any VPCs attached to the Internet gateway.
igAttachments :: Lens' InternetGateway [InternetGatewayAttachment]
igAttachments = lens _igAttachments (\s a -> s { _igAttachments = a })

-- | Any tags assigned to the Internet gateway.
igTags :: Lens' InternetGateway [Tag]
igTags = lens _igTags (\s a -> s { _igTags = a })

instance FromXML InternetGateway where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "internetGateway"

-- | Describes the attachment of a VPC to an Internet gateway.
data InternetGatewayAttachment = InternetGatewayAttachment
    { _igaVpcId :: Text
    , _igaState :: AttachmentStatus
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InternetGatewayAttachment' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VpcId ::@ @Text@
--
-- * @State ::@ @AttachmentStatus@
--
mkInternetGatewayAttachment :: Text -- ^ 'igaVpcId'
                            -> AttachmentStatus -- ^ 'igaState'
                            -> InternetGatewayAttachment
mkInternetGatewayAttachment p1 p2 = InternetGatewayAttachment
    { _igaVpcId = p1
    , _igaState = p2
    }

-- | The ID of the VPC.
igaVpcId :: Lens' InternetGatewayAttachment Text
igaVpcId = lens _igaVpcId (\s a -> s { _igaVpcId = a })

-- | The current state of the attachment.
igaState :: Lens' InternetGatewayAttachment AttachmentStatus
igaState = lens _igaState (\s a -> s { _igaState = a })

instance FromXML InternetGatewayAttachment where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery InternetGatewayAttachment where
    toQuery = genericQuery def

-- | Describes a security group rule.
data IpPermission = IpPermission
    { _ipIpProtocol :: Text
    , _ipFromPort :: Integer
    , _ipToPort :: Integer
    , _ipUserIdGroupPairs :: [UserIdGroupPair]
    , _ipIpRanges :: [IpRange]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'IpPermission' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @IpProtocol ::@ @Text@
--
-- * @FromPort ::@ @Integer@
--
-- * @ToPort ::@ @Integer@
--
-- * @UserIdGroupPairs ::@ @[UserIdGroupPair]@
--
-- * @IpRanges ::@ @[IpRange]@
--
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

-- | The protocol. When you call DescribeSecurityGroups, the protocol value
-- returned is the number. Exception: For TCP, UDP, and ICMP, the value
-- returned is the name (for example, tcp, udp, or icmp). For a list of
-- protocol numbers, see Protocol Numbers.
ipIpProtocol :: Lens' IpPermission Text
ipIpProtocol = lens _ipIpProtocol (\s a -> s { _ipIpProtocol = a })

-- | The start of port range for the TCP and UDP protocols, or an ICMP type
-- number. A value of -1 indicates all ICMP types.
ipFromPort :: Lens' IpPermission Integer
ipFromPort = lens _ipFromPort (\s a -> s { _ipFromPort = a })

-- | The end of port range for the TCP and UDP protocols, or an ICMP code. A
-- value of -1 indicates all ICMP codes for the specified ICMP type.
ipToPort :: Lens' IpPermission Integer
ipToPort = lens _ipToPort (\s a -> s { _ipToPort = a })

-- | One or more security group and AWS account ID pairs.
ipUserIdGroupPairs :: Lens' IpPermission [UserIdGroupPair]
ipUserIdGroupPairs =
    lens _ipUserIdGroupPairs (\s a -> s { _ipUserIdGroupPairs = a })

-- | One or more IP ranges.
ipIpRanges :: Lens' IpPermission [IpRange]
ipIpRanges = lens _ipIpRanges (\s a -> s { _ipIpRanges = a })

instance FromXML IpPermission where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "IpPermission"

instance ToQuery IpPermission where
    toQuery = genericQuery def

-- | Describes a key pair.
data KeyPairInfo = KeyPairInfo
    { _kpiKeyName :: Maybe Text
    , _kpiKeyFingerprint :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'KeyPairInfo' data type.
--
-- 'KeyPairInfo' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @KeyName ::@ @Maybe Text@
--
-- * @KeyFingerprint ::@ @Maybe Text@
--
mkKeyPairInfo :: KeyPairInfo
mkKeyPairInfo = KeyPairInfo
    { _kpiKeyName = Nothing
    , _kpiKeyFingerprint = Nothing
    }

-- | The name of the key pair.
kpiKeyName :: Lens' KeyPairInfo (Maybe Text)
kpiKeyName = lens _kpiKeyName (\s a -> s { _kpiKeyName = a })

-- | If you used CreateKeyPair to create the key pair, this is the SHA-1 digest
-- of the DER encoded private key. If you used ImportKeyPair to provide AWS
-- the public key, this is the MD5 public key fingerprint as specified in
-- section 4 of RFC4716.
kpiKeyFingerprint :: Lens' KeyPairInfo (Maybe Text)
kpiKeyFingerprint =
    lens _kpiKeyFingerprint (\s a -> s { _kpiKeyFingerprint = a })

instance FromXML KeyPairInfo where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes a launch permission.
data LaunchPermission = LaunchPermission
    { _lpUserId :: Maybe Text
    , _lpGroup :: Maybe PermissionGroup
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'LaunchPermission' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @UserId ::@ @Maybe Text@
--
-- * @Group ::@ @Maybe PermissionGroup@
--
mkLaunchPermission :: LaunchPermission
mkLaunchPermission = LaunchPermission
    { _lpUserId = Nothing
    , _lpGroup = Nothing
    }

-- | The AWS account ID.
lpUserId :: Lens' LaunchPermission (Maybe Text)
lpUserId = lens _lpUserId (\s a -> s { _lpUserId = a })

-- | The name of the group.
lpGroup :: Lens' LaunchPermission (Maybe PermissionGroup)
lpGroup = lens _lpGroup (\s a -> s { _lpGroup = a })

instance FromXML LaunchPermission where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery LaunchPermission where
    toQuery = genericQuery def

-- | 
data LaunchPermissionModifications = LaunchPermissionModifications
    { _lpmAdd :: [LaunchPermission]
    , _lpmRemove :: [LaunchPermission]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'LaunchPermissionModifications' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Add ::@ @[LaunchPermission]@
--
-- * @Remove ::@ @[LaunchPermission]@
--
mkLaunchPermissionModifications :: LaunchPermissionModifications
mkLaunchPermissionModifications = LaunchPermissionModifications
    { _lpmAdd = mempty
    , _lpmRemove = mempty
    }

-- | The AWS account ID to add to the list of launch permissions for the AMI.
lpmAdd :: Lens' LaunchPermissionModifications [LaunchPermission]
lpmAdd = lens _lpmAdd (\s a -> s { _lpmAdd = a })

-- | The AWS account ID to remove from the list of launch permissions for the
-- AMI.
lpmRemove :: Lens' LaunchPermissionModifications [LaunchPermission]
lpmRemove = lens _lpmRemove (\s a -> s { _lpmRemove = a })

instance ToQuery LaunchPermissionModifications where
    toQuery = genericQuery def

-- | Additional information for launching instances.
data LaunchSpecification = LaunchSpecification
    { _lsImageId :: Maybe Text
    , _lsKeyName :: Maybe Text
    , _lsSecurityGroups :: [GroupIdentifier]
    , _lsUserData :: Maybe Text
    , _lsAddressingType :: Maybe Text
    , _lsInstanceType :: Maybe InstanceType
    , _lsPlacement :: Maybe SpotPlacement
    , _lsKernelId :: Maybe Text
    , _lsRamdiskId :: Maybe Text
    , _lsBlockDeviceMappings :: [BlockDeviceMapping]
    , _lsMonitoringEnabled :: Maybe Bool
    , _lsSubnetId :: Maybe Text
    , _lsNetworkInterfaces :: [InstanceNetworkInterfaceSpecification]
    , _lsIamInstanceProfile :: Maybe IamInstanceProfileSpecification
    , _lsEbsOptimized :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'LaunchSpecification' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ImageId ::@ @Maybe Text@
--
-- * @KeyName ::@ @Maybe Text@
--
-- * @SecurityGroups ::@ @[GroupIdentifier]@
--
-- * @UserData ::@ @Maybe Text@
--
-- * @AddressingType ::@ @Maybe Text@
--
-- * @InstanceType ::@ @Maybe InstanceType@
--
-- * @Placement ::@ @Maybe SpotPlacement@
--
-- * @KernelId ::@ @Maybe Text@
--
-- * @RamdiskId ::@ @Maybe Text@
--
-- * @BlockDeviceMappings ::@ @[BlockDeviceMapping]@
--
-- * @MonitoringEnabled ::@ @Maybe Bool@
--
-- * @SubnetId ::@ @Maybe Text@
--
-- * @NetworkInterfaces ::@ @[InstanceNetworkInterfaceSpecification]@
--
-- * @IamInstanceProfile ::@ @Maybe IamInstanceProfileSpecification@
--
-- * @EbsOptimized ::@ @Maybe Bool@
--
mkLaunchSpecification :: LaunchSpecification
mkLaunchSpecification = LaunchSpecification
    { _lsImageId = Nothing
    , _lsKeyName = Nothing
    , _lsSecurityGroups = mempty
    , _lsUserData = Nothing
    , _lsAddressingType = Nothing
    , _lsInstanceType = Nothing
    , _lsPlacement = Nothing
    , _lsKernelId = Nothing
    , _lsRamdiskId = Nothing
    , _lsBlockDeviceMappings = mempty
    , _lsMonitoringEnabled = Nothing
    , _lsSubnetId = Nothing
    , _lsNetworkInterfaces = mempty
    , _lsIamInstanceProfile = Nothing
    , _lsEbsOptimized = Nothing
    }

-- | The ID of the AMI.
lsImageId :: Lens' LaunchSpecification (Maybe Text)
lsImageId = lens _lsImageId (\s a -> s { _lsImageId = a })

-- | The name of the key pair.
lsKeyName :: Lens' LaunchSpecification (Maybe Text)
lsKeyName = lens _lsKeyName (\s a -> s { _lsKeyName = a })

-- | One or more security groups.
lsSecurityGroups :: Lens' LaunchSpecification [GroupIdentifier]
lsSecurityGroups =
    lens _lsSecurityGroups (\s a -> s { _lsSecurityGroups = a })

-- | The Base64-encoded MIME user data to make available to the instances.
lsUserData :: Lens' LaunchSpecification (Maybe Text)
lsUserData = lens _lsUserData (\s a -> s { _lsUserData = a })

-- | 
lsAddressingType :: Lens' LaunchSpecification (Maybe Text)
lsAddressingType =
    lens _lsAddressingType (\s a -> s { _lsAddressingType = a })

-- | The instance type.
lsInstanceType :: Lens' LaunchSpecification (Maybe InstanceType)
lsInstanceType = lens _lsInstanceType (\s a -> s { _lsInstanceType = a })

-- | The placement information for the instance.
lsPlacement :: Lens' LaunchSpecification (Maybe SpotPlacement)
lsPlacement = lens _lsPlacement (\s a -> s { _lsPlacement = a })

-- | The ID of the kernel.
lsKernelId :: Lens' LaunchSpecification (Maybe Text)
lsKernelId = lens _lsKernelId (\s a -> s { _lsKernelId = a })

-- | The ID of the RAM disk.
lsRamdiskId :: Lens' LaunchSpecification (Maybe Text)
lsRamdiskId = lens _lsRamdiskId (\s a -> s { _lsRamdiskId = a })

-- | One or more block device mapping entries.
lsBlockDeviceMappings :: Lens' LaunchSpecification [BlockDeviceMapping]
lsBlockDeviceMappings =
    lens _lsBlockDeviceMappings (\s a -> s { _lsBlockDeviceMappings = a })

-- | Enables monitoring for the instance. Default: Disabled.
lsMonitoringEnabled :: Lens' LaunchSpecification (Maybe Bool)
lsMonitoringEnabled =
    lens _lsMonitoringEnabled (\s a -> s { _lsMonitoringEnabled = a })

-- | The ID of the subnet in which to launch the Spot Instance.
lsSubnetId :: Lens' LaunchSpecification (Maybe Text)
lsSubnetId = lens _lsSubnetId (\s a -> s { _lsSubnetId = a })

-- | One or more network interfaces.
lsNetworkInterfaces :: Lens' LaunchSpecification [InstanceNetworkInterfaceSpecification]
lsNetworkInterfaces =
    lens _lsNetworkInterfaces (\s a -> s { _lsNetworkInterfaces = a })

-- | The IAM instance profile.
lsIamInstanceProfile :: Lens' LaunchSpecification (Maybe IamInstanceProfileSpecification)
lsIamInstanceProfile =
    lens _lsIamInstanceProfile (\s a -> s { _lsIamInstanceProfile = a })

-- | Indicates whether the instance is optimized for EBS I/O. This optimization
-- provides dedicated throughput to Amazon EBS and an optimized configuration
-- stack to provide optimal EBS I/O performance. This optimization isn't
-- available with all instance types. Additional usage charges apply when
-- using an EBS Optimized instance. Default: false.
lsEbsOptimized :: Lens' LaunchSpecification (Maybe Bool)
lsEbsOptimized = lens _lsEbsOptimized (\s a -> s { _lsEbsOptimized = a })

instance FromXML LaunchSpecification where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "launchSpecification"

instance ToQuery LaunchSpecification where
    toQuery = genericQuery def

-- | Information about the network ACL.
data NetworkAcl = NetworkAcl
    { _naNetworkAclId :: Maybe Text
    , _naVpcId :: Maybe Text
    , _naIsDefault :: Maybe Bool
    , _naEntries :: [NetworkAclEntry]
    , _naAssociations :: [NetworkAclAssociation]
    , _naTags :: [Tag]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'NetworkAcl' data type.
--
-- 'NetworkAcl' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @NetworkAclId ::@ @Maybe Text@
--
-- * @VpcId ::@ @Maybe Text@
--
-- * @IsDefault ::@ @Maybe Bool@
--
-- * @Entries ::@ @[NetworkAclEntry]@
--
-- * @Associations ::@ @[NetworkAclAssociation]@
--
-- * @Tags ::@ @[Tag]@
--
mkNetworkAcl :: NetworkAcl
mkNetworkAcl = NetworkAcl
    { _naNetworkAclId = Nothing
    , _naVpcId = Nothing
    , _naIsDefault = Nothing
    , _naEntries = mempty
    , _naAssociations = mempty
    , _naTags = mempty
    }

-- | The ID of the network ACL.
naNetworkAclId :: Lens' NetworkAcl (Maybe Text)
naNetworkAclId = lens _naNetworkAclId (\s a -> s { _naNetworkAclId = a })

-- | The ID of the VPC for the network ACL.
naVpcId :: Lens' NetworkAcl (Maybe Text)
naVpcId = lens _naVpcId (\s a -> s { _naVpcId = a })

-- | Indicates whether this is the default network ACL for the VPC.
naIsDefault :: Lens' NetworkAcl (Maybe Bool)
naIsDefault = lens _naIsDefault (\s a -> s { _naIsDefault = a })

-- | One or more entries (rules) in the network ACL.
naEntries :: Lens' NetworkAcl [NetworkAclEntry]
naEntries = lens _naEntries (\s a -> s { _naEntries = a })

-- | Any associations between the network ACL and one or more subnets.
naAssociations :: Lens' NetworkAcl [NetworkAclAssociation]
naAssociations = lens _naAssociations (\s a -> s { _naAssociations = a })

-- | Any tags assigned to the network ACL.
naTags :: Lens' NetworkAcl [Tag]
naTags = lens _naTags (\s a -> s { _naTags = a })

instance FromXML NetworkAcl where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "networkAcl"

-- | Describes an association between a network ACL and a subnet.
data NetworkAclAssociation = NetworkAclAssociation
    { _naaNetworkAclAssociationId :: Maybe Text
    , _naaNetworkAclId :: Maybe Text
    , _naaSubnetId :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'NetworkAclAssociation' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @NetworkAclAssociationId ::@ @Maybe Text@
--
-- * @NetworkAclId ::@ @Maybe Text@
--
-- * @SubnetId ::@ @Maybe Text@
--
mkNetworkAclAssociation :: NetworkAclAssociation
mkNetworkAclAssociation = NetworkAclAssociation
    { _naaNetworkAclAssociationId = Nothing
    , _naaNetworkAclId = Nothing
    , _naaSubnetId = Nothing
    }

-- | The ID of the association between a network ACL and a subnet.
naaNetworkAclAssociationId :: Lens' NetworkAclAssociation (Maybe Text)
naaNetworkAclAssociationId =
    lens _naaNetworkAclAssociationId
         (\s a -> s { _naaNetworkAclAssociationId = a })

-- | The ID of the network ACL.
naaNetworkAclId :: Lens' NetworkAclAssociation (Maybe Text)
naaNetworkAclId = lens _naaNetworkAclId (\s a -> s { _naaNetworkAclId = a })

-- | The ID of the subnet.
naaSubnetId :: Lens' NetworkAclAssociation (Maybe Text)
naaSubnetId = lens _naaSubnetId (\s a -> s { _naaSubnetId = a })

instance FromXML NetworkAclAssociation where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery NetworkAclAssociation where
    toQuery = genericQuery def

-- | Describes an entry in a network ACL.
data NetworkAclEntry = NetworkAclEntry
    { _naeRuleNumber :: Maybe Integer
    , _naeProtocol :: Maybe Text
    , _naeRuleAction :: Maybe RuleAction
    , _naeEgress :: Maybe Bool
    , _naeCidrBlock :: Maybe Text
    , _naeIcmpTypeCode :: Maybe IcmpTypeCode
    , _naePortRange :: Maybe PortRange
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'NetworkAclEntry' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @RuleNumber ::@ @Maybe Integer@
--
-- * @Protocol ::@ @Maybe Text@
--
-- * @RuleAction ::@ @Maybe RuleAction@
--
-- * @Egress ::@ @Maybe Bool@
--
-- * @CidrBlock ::@ @Maybe Text@
--
-- * @IcmpTypeCode ::@ @Maybe IcmpTypeCode@
--
-- * @PortRange ::@ @Maybe PortRange@
--
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

-- | The rule number for the entry. ACL entries are processed in ascending order
-- by rule number.
naeRuleNumber :: Lens' NetworkAclEntry (Maybe Integer)
naeRuleNumber = lens _naeRuleNumber (\s a -> s { _naeRuleNumber = a })

-- | The protocol. A value of -1 means all protocols.
naeProtocol :: Lens' NetworkAclEntry (Maybe Text)
naeProtocol = lens _naeProtocol (\s a -> s { _naeProtocol = a })

-- | Indicates whether to allow or deny the traffic that matches the rule.
naeRuleAction :: Lens' NetworkAclEntry (Maybe RuleAction)
naeRuleAction = lens _naeRuleAction (\s a -> s { _naeRuleAction = a })

-- | Indicates whether the rule is an egress rule (applied to traffic leaving
-- the subnet).
naeEgress :: Lens' NetworkAclEntry (Maybe Bool)
naeEgress = lens _naeEgress (\s a -> s { _naeEgress = a })

-- | The network range to allow or deny, in CIDR notation.
naeCidrBlock :: Lens' NetworkAclEntry (Maybe Text)
naeCidrBlock = lens _naeCidrBlock (\s a -> s { _naeCidrBlock = a })

-- | ICMP protocol: The ICMP type and code.
naeIcmpTypeCode :: Lens' NetworkAclEntry (Maybe IcmpTypeCode)
naeIcmpTypeCode = lens _naeIcmpTypeCode (\s a -> s { _naeIcmpTypeCode = a })

-- | TCP or UDP protocols: The range of ports the rule applies to.
naePortRange :: Lens' NetworkAclEntry (Maybe PortRange)
naePortRange = lens _naePortRange (\s a -> s { _naePortRange = a })

instance FromXML NetworkAclEntry where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery NetworkAclEntry where
    toQuery = genericQuery def

-- | Information about the network interface.
data NetworkInterface = NetworkInterface
    { _niNetworkInterfaceId :: Maybe Text
    , _niSubnetId :: Maybe Text
    , _niVpcId :: Maybe Text
    , _niAvailabilityZone :: Maybe Text
    , _niDescription :: Maybe Text
    , _niOwnerId :: Maybe Text
    , _niRequesterId :: Maybe Text
    , _niRequesterManaged :: Maybe Bool
    , _niStatus :: Maybe NetworkInterfaceStatus
    , _niMacAddress :: Maybe Text
    , _niPrivateIpAddress :: Maybe Text
    , _niPrivateDnsName :: Maybe Text
    , _niSourceDestCheck :: Maybe Bool
    , _niGroups :: [GroupIdentifier]
    , _niAttachment :: Maybe NetworkInterfaceAttachment
    , _niAssociation :: Maybe NetworkInterfaceAssociation
    , _niTagSet :: [Tag]
    , _niPrivateIpAddresses :: [NetworkInterfacePrivateIpAddress]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'NetworkInterface' data type.
--
-- 'NetworkInterface' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @NetworkInterfaceId ::@ @Maybe Text@
--
-- * @SubnetId ::@ @Maybe Text@
--
-- * @VpcId ::@ @Maybe Text@
--
-- * @AvailabilityZone ::@ @Maybe Text@
--
-- * @Description ::@ @Maybe Text@
--
-- * @OwnerId ::@ @Maybe Text@
--
-- * @RequesterId ::@ @Maybe Text@
--
-- * @RequesterManaged ::@ @Maybe Bool@
--
-- * @Status ::@ @Maybe NetworkInterfaceStatus@
--
-- * @MacAddress ::@ @Maybe Text@
--
-- * @PrivateIpAddress ::@ @Maybe Text@
--
-- * @PrivateDnsName ::@ @Maybe Text@
--
-- * @SourceDestCheck ::@ @Maybe Bool@
--
-- * @Groups ::@ @[GroupIdentifier]@
--
-- * @Attachment ::@ @Maybe NetworkInterfaceAttachment@
--
-- * @Association ::@ @Maybe NetworkInterfaceAssociation@
--
-- * @TagSet ::@ @[Tag]@
--
-- * @PrivateIpAddresses ::@ @[NetworkInterfacePrivateIpAddress]@
--
mkNetworkInterface :: NetworkInterface
mkNetworkInterface = NetworkInterface
    { _niNetworkInterfaceId = Nothing
    , _niSubnetId = Nothing
    , _niVpcId = Nothing
    , _niAvailabilityZone = Nothing
    , _niDescription = Nothing
    , _niOwnerId = Nothing
    , _niRequesterId = Nothing
    , _niRequesterManaged = Nothing
    , _niStatus = Nothing
    , _niMacAddress = Nothing
    , _niPrivateIpAddress = Nothing
    , _niPrivateDnsName = Nothing
    , _niSourceDestCheck = Nothing
    , _niGroups = mempty
    , _niAttachment = Nothing
    , _niAssociation = Nothing
    , _niTagSet = mempty
    , _niPrivateIpAddresses = mempty
    }

-- | The ID of the network interface.
niNetworkInterfaceId :: Lens' NetworkInterface (Maybe Text)
niNetworkInterfaceId =
    lens _niNetworkInterfaceId (\s a -> s { _niNetworkInterfaceId = a })

-- | The ID of the subnet.
niSubnetId :: Lens' NetworkInterface (Maybe Text)
niSubnetId = lens _niSubnetId (\s a -> s { _niSubnetId = a })

-- | The ID of the VPC.
niVpcId :: Lens' NetworkInterface (Maybe Text)
niVpcId = lens _niVpcId (\s a -> s { _niVpcId = a })

-- | The Availability Zone.
niAvailabilityZone :: Lens' NetworkInterface (Maybe Text)
niAvailabilityZone =
    lens _niAvailabilityZone (\s a -> s { _niAvailabilityZone = a })

-- | A description.
niDescription :: Lens' NetworkInterface (Maybe Text)
niDescription = lens _niDescription (\s a -> s { _niDescription = a })

-- | The AWS account ID of the owner of the network interface.
niOwnerId :: Lens' NetworkInterface (Maybe Text)
niOwnerId = lens _niOwnerId (\s a -> s { _niOwnerId = a })

-- | The ID of the entity that launched the instance on your behalf (for
-- example, AWS Management Console or Auto Scaling).
niRequesterId :: Lens' NetworkInterface (Maybe Text)
niRequesterId = lens _niRequesterId (\s a -> s { _niRequesterId = a })

-- | Indicates whether the network interface is being managed by AWS.
niRequesterManaged :: Lens' NetworkInterface (Maybe Bool)
niRequesterManaged =
    lens _niRequesterManaged (\s a -> s { _niRequesterManaged = a })

-- | The status of the network interface.
niStatus :: Lens' NetworkInterface (Maybe NetworkInterfaceStatus)
niStatus = lens _niStatus (\s a -> s { _niStatus = a })

-- | The MAC address.
niMacAddress :: Lens' NetworkInterface (Maybe Text)
niMacAddress = lens _niMacAddress (\s a -> s { _niMacAddress = a })

-- | The IP address of the network interface within the subnet.
niPrivateIpAddress :: Lens' NetworkInterface (Maybe Text)
niPrivateIpAddress =
    lens _niPrivateIpAddress (\s a -> s { _niPrivateIpAddress = a })

-- | The private DNS name.
niPrivateDnsName :: Lens' NetworkInterface (Maybe Text)
niPrivateDnsName =
    lens _niPrivateDnsName (\s a -> s { _niPrivateDnsName = a })

-- | Indicates whether traffic to or from the instance is validated.
niSourceDestCheck :: Lens' NetworkInterface (Maybe Bool)
niSourceDestCheck =
    lens _niSourceDestCheck (\s a -> s { _niSourceDestCheck = a })

-- | Any security groups for the network interface.
niGroups :: Lens' NetworkInterface [GroupIdentifier]
niGroups = lens _niGroups (\s a -> s { _niGroups = a })

-- | The network interface attachment.
niAttachment :: Lens' NetworkInterface (Maybe NetworkInterfaceAttachment)
niAttachment = lens _niAttachment (\s a -> s { _niAttachment = a })

-- | The association information for an Elastic IP associated with the network
-- interface.
niAssociation :: Lens' NetworkInterface (Maybe NetworkInterfaceAssociation)
niAssociation = lens _niAssociation (\s a -> s { _niAssociation = a })

-- | Any tags assigned to the network interface.
niTagSet :: Lens' NetworkInterface [Tag]
niTagSet = lens _niTagSet (\s a -> s { _niTagSet = a })

-- | The private IP addresses associated with the network interface.
niPrivateIpAddresses :: Lens' NetworkInterface [NetworkInterfacePrivateIpAddress]
niPrivateIpAddresses =
    lens _niPrivateIpAddresses (\s a -> s { _niPrivateIpAddresses = a })

instance FromXML NetworkInterface where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "networkInterface"

-- | The association information for an Elastic IP associated with the network
-- interface.
data NetworkInterfaceAssociation = NetworkInterfaceAssociation
    { _niarPublicIp :: Maybe Text
    , _niarPublicDnsName :: Maybe Text
    , _niarIpOwnerId :: Maybe Text
    , _niarAllocationId :: Maybe Text
    , _niarAssociationId :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'NetworkInterfaceAssociation' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @PublicIp ::@ @Maybe Text@
--
-- * @PublicDnsName ::@ @Maybe Text@
--
-- * @IpOwnerId ::@ @Maybe Text@
--
-- * @AllocationId ::@ @Maybe Text@
--
-- * @AssociationId ::@ @Maybe Text@
--
mkNetworkInterfaceAssociation :: NetworkInterfaceAssociation
mkNetworkInterfaceAssociation = NetworkInterfaceAssociation
    { _niarPublicIp = Nothing
    , _niarPublicDnsName = Nothing
    , _niarIpOwnerId = Nothing
    , _niarAllocationId = Nothing
    , _niarAssociationId = Nothing
    }

-- | The address of the Elastic IP address bound to the network interface.
niarPublicIp :: Lens' NetworkInterfaceAssociation (Maybe Text)
niarPublicIp = lens _niarPublicIp (\s a -> s { _niarPublicIp = a })

-- | The public DNS name.
niarPublicDnsName :: Lens' NetworkInterfaceAssociation (Maybe Text)
niarPublicDnsName =
    lens _niarPublicDnsName (\s a -> s { _niarPublicDnsName = a })

-- | The ID of the Elastic IP address owner.
niarIpOwnerId :: Lens' NetworkInterfaceAssociation (Maybe Text)
niarIpOwnerId = lens _niarIpOwnerId (\s a -> s { _niarIpOwnerId = a })

-- | The allocation ID.
niarAllocationId :: Lens' NetworkInterfaceAssociation (Maybe Text)
niarAllocationId =
    lens _niarAllocationId (\s a -> s { _niarAllocationId = a })

-- | The association ID.
niarAssociationId :: Lens' NetworkInterfaceAssociation (Maybe Text)
niarAssociationId =
    lens _niarAssociationId (\s a -> s { _niarAssociationId = a })

instance FromXML NetworkInterfaceAssociation where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "association"

instance ToQuery NetworkInterfaceAssociation where
    toQuery = genericQuery def

-- | The network interface attachment.
data NetworkInterfaceAttachment = NetworkInterfaceAttachment
    { _niaAttachmentId :: Maybe Text
    , _niaInstanceId :: Maybe Text
    , _niaInstanceOwnerId :: Maybe Text
    , _niaDeviceIndex :: Maybe Integer
    , _niaStatus :: Maybe AttachmentStatus
    , _niaAttachTime :: Maybe ISO8601
    , _niaDeleteOnTermination :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'NetworkInterfaceAttachment' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AttachmentId ::@ @Maybe Text@
--
-- * @InstanceId ::@ @Maybe Text@
--
-- * @InstanceOwnerId ::@ @Maybe Text@
--
-- * @DeviceIndex ::@ @Maybe Integer@
--
-- * @Status ::@ @Maybe AttachmentStatus@
--
-- * @AttachTime ::@ @Maybe ISO8601@
--
-- * @DeleteOnTermination ::@ @Maybe Bool@
--
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

-- | The ID of the network interface attachment.
niaAttachmentId :: Lens' NetworkInterfaceAttachment (Maybe Text)
niaAttachmentId = lens _niaAttachmentId (\s a -> s { _niaAttachmentId = a })

-- | The ID of the instance.
niaInstanceId :: Lens' NetworkInterfaceAttachment (Maybe Text)
niaInstanceId = lens _niaInstanceId (\s a -> s { _niaInstanceId = a })

-- | The AWS account ID of the owner of the instance.
niaInstanceOwnerId :: Lens' NetworkInterfaceAttachment (Maybe Text)
niaInstanceOwnerId =
    lens _niaInstanceOwnerId (\s a -> s { _niaInstanceOwnerId = a })

-- | The device index of the network interface attachment on the instance.
niaDeviceIndex :: Lens' NetworkInterfaceAttachment (Maybe Integer)
niaDeviceIndex = lens _niaDeviceIndex (\s a -> s { _niaDeviceIndex = a })

-- | The attachment state.
niaStatus :: Lens' NetworkInterfaceAttachment (Maybe AttachmentStatus)
niaStatus = lens _niaStatus (\s a -> s { _niaStatus = a })

-- | The timestamp indicating when the attachment initiated.
niaAttachTime :: Lens' NetworkInterfaceAttachment (Maybe ISO8601)
niaAttachTime = lens _niaAttachTime (\s a -> s { _niaAttachTime = a })

-- | Indicates whether the network interface is deleted when the instance is
-- terminated.
niaDeleteOnTermination :: Lens' NetworkInterfaceAttachment (Maybe Bool)
niaDeleteOnTermination =
    lens _niaDeleteOnTermination (\s a -> s { _niaDeleteOnTermination = a })

instance FromXML NetworkInterfaceAttachment where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "attachment"

instance ToQuery NetworkInterfaceAttachment where
    toQuery = genericQuery def

-- | The ID of the interface attachment.
data NetworkInterfaceAttachmentChanges = NetworkInterfaceAttachmentChanges
    { _niacAttachmentId :: Maybe Text
    , _niacDeleteOnTermination :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'NetworkInterfaceAttachmentChanges' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AttachmentId ::@ @Maybe Text@
--
-- * @DeleteOnTermination ::@ @Maybe Bool@
--
mkNetworkInterfaceAttachmentChanges :: NetworkInterfaceAttachmentChanges
mkNetworkInterfaceAttachmentChanges = NetworkInterfaceAttachmentChanges
    { _niacAttachmentId = Nothing
    , _niacDeleteOnTermination = Nothing
    }

-- | The ID of the network interface attachment.
niacAttachmentId :: Lens' NetworkInterfaceAttachmentChanges (Maybe Text)
niacAttachmentId =
    lens _niacAttachmentId (\s a -> s { _niacAttachmentId = a })

-- | Indicates whether the network interface is deleted when the instance is
-- terminated.
niacDeleteOnTermination :: Lens' NetworkInterfaceAttachmentChanges (Maybe Bool)
niacDeleteOnTermination =
    lens _niacDeleteOnTermination
         (\s a -> s { _niacDeleteOnTermination = a })

instance ToQuery NetworkInterfaceAttachmentChanges where
    toQuery = genericQuery def

-- | Describes the private IP address of a network interface.
data NetworkInterfacePrivateIpAddress = NetworkInterfacePrivateIpAddress
    { _nipiaPrivateIpAddress :: Maybe Text
    , _nipiaPrivateDnsName :: Maybe Text
    , _nipiaPrimary :: Maybe Bool
    , _nipiaAssociation :: Maybe NetworkInterfaceAssociation
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'NetworkInterfacePrivateIpAddress' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @PrivateIpAddress ::@ @Maybe Text@
--
-- * @PrivateDnsName ::@ @Maybe Text@
--
-- * @Primary ::@ @Maybe Bool@
--
-- * @Association ::@ @Maybe NetworkInterfaceAssociation@
--
mkNetworkInterfacePrivateIpAddress :: NetworkInterfacePrivateIpAddress
mkNetworkInterfacePrivateIpAddress = NetworkInterfacePrivateIpAddress
    { _nipiaPrivateIpAddress = Nothing
    , _nipiaPrivateDnsName = Nothing
    , _nipiaPrimary = Nothing
    , _nipiaAssociation = Nothing
    }

-- | The private IP address.
nipiaPrivateIpAddress :: Lens' NetworkInterfacePrivateIpAddress (Maybe Text)
nipiaPrivateIpAddress =
    lens _nipiaPrivateIpAddress (\s a -> s { _nipiaPrivateIpAddress = a })

-- | The private DNS name.
nipiaPrivateDnsName :: Lens' NetworkInterfacePrivateIpAddress (Maybe Text)
nipiaPrivateDnsName =
    lens _nipiaPrivateDnsName (\s a -> s { _nipiaPrivateDnsName = a })

-- | Indicates whether this IP address is the primary private IP address of the
-- network interface.
nipiaPrimary :: Lens' NetworkInterfacePrivateIpAddress (Maybe Bool)
nipiaPrimary = lens _nipiaPrimary (\s a -> s { _nipiaPrimary = a })

-- | The association information for an Elastic IP address associated with the
-- network interface.
nipiaAssociation :: Lens' NetworkInterfacePrivateIpAddress (Maybe NetworkInterfaceAssociation)
nipiaAssociation =
    lens _nipiaAssociation (\s a -> s { _nipiaAssociation = a })

instance FromXML NetworkInterfacePrivateIpAddress where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery NetworkInterfacePrivateIpAddress where
    toQuery = genericQuery def

-- | The location where the instance launched.
data Placement = Placement
    { _pAvailabilityZone :: Maybe Text
    , _pGroupName :: Maybe Text
    , _pTenancy :: Maybe Tenancy
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Placement' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AvailabilityZone ::@ @Maybe Text@
--
-- * @GroupName ::@ @Maybe Text@
--
-- * @Tenancy ::@ @Maybe Tenancy@
--
mkPlacement :: Placement
mkPlacement = Placement
    { _pAvailabilityZone = Nothing
    , _pGroupName = Nothing
    , _pTenancy = Nothing
    }

-- | The Availability Zone of the instance.
pAvailabilityZone :: Lens' Placement (Maybe Text)
pAvailabilityZone =
    lens _pAvailabilityZone (\s a -> s { _pAvailabilityZone = a })

-- | The name of the placement group the instance is in (for cluster compute
-- instances).
pGroupName :: Lens' Placement (Maybe Text)
pGroupName = lens _pGroupName (\s a -> s { _pGroupName = a })

-- | The tenancy of the instance (if the instance is running in a VPC). An
-- instance with a tenancy of dedicated runs on single-tenant hardware.
pTenancy :: Lens' Placement (Maybe Tenancy)
pTenancy = lens _pTenancy (\s a -> s { _pTenancy = a })

instance FromXML Placement where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "placement"

instance ToQuery Placement where
    toQuery = genericQuery def

-- | Describes a placement group.
data PlacementGroup = PlacementGroup
    { _pgGroupName :: Maybe Text
    , _pgStrategy :: Maybe PlacementStrategy
    , _pgState :: Maybe PlacementGroupState
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'PlacementGroup' data type.
--
-- 'PlacementGroup' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GroupName ::@ @Maybe Text@
--
-- * @Strategy ::@ @Maybe PlacementStrategy@
--
-- * @State ::@ @Maybe PlacementGroupState@
--
mkPlacementGroup :: PlacementGroup
mkPlacementGroup = PlacementGroup
    { _pgGroupName = Nothing
    , _pgStrategy = Nothing
    , _pgState = Nothing
    }

-- | The name of the placement group.
pgGroupName :: Lens' PlacementGroup (Maybe Text)
pgGroupName = lens _pgGroupName (\s a -> s { _pgGroupName = a })

-- | The placement strategy.
pgStrategy :: Lens' PlacementGroup (Maybe PlacementStrategy)
pgStrategy = lens _pgStrategy (\s a -> s { _pgStrategy = a })

-- | The state of the placement group.
pgState :: Lens' PlacementGroup (Maybe PlacementGroupState)
pgState = lens _pgState (\s a -> s { _pgState = a })

instance FromXML PlacementGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | TCP or UDP protocols: The range of ports the rule applies to.
data PortRange = PortRange
    { _prFrom :: Maybe Integer
    , _prTo :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'PortRange' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @From ::@ @Maybe Integer@
--
-- * @To ::@ @Maybe Integer@
--
mkPortRange :: PortRange
mkPortRange = PortRange
    { _prFrom = Nothing
    , _prTo = Nothing
    }

-- | The first port in the range.
prFrom :: Lens' PortRange (Maybe Integer)
prFrom = lens _prFrom (\s a -> s { _prFrom = a })

-- | The last port in the range.
prTo :: Lens' PortRange (Maybe Integer)
prTo = lens _prTo (\s a -> s { _prTo = a })

instance FromXML PortRange where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "portRange"

instance ToQuery PortRange where
    toQuery = genericQuery def

-- | Describes the price for a Reserved Instance.
data PriceSchedule = PriceSchedule
    { _psTerm :: Maybe Integer
    , _psPrice :: Maybe Double
    , _psCurrencyCode :: Maybe CurrencyCodeValues
    , _psActive :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'PriceSchedule' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Term ::@ @Maybe Integer@
--
-- * @Price ::@ @Maybe Double@
--
-- * @CurrencyCode ::@ @Maybe CurrencyCodeValues@
--
-- * @Active ::@ @Maybe Bool@
--
mkPriceSchedule :: PriceSchedule
mkPriceSchedule = PriceSchedule
    { _psTerm = Nothing
    , _psPrice = Nothing
    , _psCurrencyCode = Nothing
    , _psActive = Nothing
    }

-- | The number of months remaining in the reservation. For example, 2 is the
-- second to the last month before the capacity reservation expires.
psTerm :: Lens' PriceSchedule (Maybe Integer)
psTerm = lens _psTerm (\s a -> s { _psTerm = a })

-- | The fixed price for the term.
psPrice :: Lens' PriceSchedule (Maybe Double)
psPrice = lens _psPrice (\s a -> s { _psPrice = a })

-- | The currency for transacting the Reserved Instance resale. At this time,
-- the only supported currency is USD.
psCurrencyCode :: Lens' PriceSchedule (Maybe CurrencyCodeValues)
psCurrencyCode = lens _psCurrencyCode (\s a -> s { _psCurrencyCode = a })

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

instance FromXML PriceSchedule where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery PriceSchedule where
    toQuery = genericQuery def

-- | Describes the price for a Reserved Instance.
data PriceScheduleSpecification = PriceScheduleSpecification
    { _pssTerm :: Maybe Integer
    , _pssPrice :: Maybe Double
    , _pssCurrencyCode :: Maybe CurrencyCodeValues
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'PriceScheduleSpecification' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Term ::@ @Maybe Integer@
--
-- * @Price ::@ @Maybe Double@
--
-- * @CurrencyCode ::@ @Maybe CurrencyCodeValues@
--
mkPriceScheduleSpecification :: PriceScheduleSpecification
mkPriceScheduleSpecification = PriceScheduleSpecification
    { _pssTerm = Nothing
    , _pssPrice = Nothing
    , _pssCurrencyCode = Nothing
    }

-- | The number of months remaining in the reservation. For example, 2 is the
-- second to the last month before the capacity reservation expires.
pssTerm :: Lens' PriceScheduleSpecification (Maybe Integer)
pssTerm = lens _pssTerm (\s a -> s { _pssTerm = a })

-- | The fixed price for the term.
pssPrice :: Lens' PriceScheduleSpecification (Maybe Double)
pssPrice = lens _pssPrice (\s a -> s { _pssPrice = a })

-- | The currency for transacting the Reserved Instance resale. At this time,
-- the only supported currency is USD.
pssCurrencyCode :: Lens' PriceScheduleSpecification (Maybe CurrencyCodeValues)
pssCurrencyCode = lens _pssCurrencyCode (\s a -> s { _pssCurrencyCode = a })

instance ToQuery PriceScheduleSpecification where
    toQuery = genericQuery def

-- | Describes a Reserved Instance offering.
data PricingDetail = PricingDetail
    { _pdPrice :: Maybe Double
    , _pdCount :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'PricingDetail' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Price ::@ @Maybe Double@
--
-- * @Count ::@ @Maybe Integer@
--
mkPricingDetail :: PricingDetail
mkPricingDetail = PricingDetail
    { _pdPrice = Nothing
    , _pdCount = Nothing
    }

-- | The price per instance.
pdPrice :: Lens' PricingDetail (Maybe Double)
pdPrice = lens _pdPrice (\s a -> s { _pdPrice = a })

-- | The number of instances available for the price.
pdCount :: Lens' PricingDetail (Maybe Integer)
pdCount = lens _pdCount (\s a -> s { _pdCount = a })

instance FromXML PricingDetail where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery PricingDetail where
    toQuery = genericQuery def

-- | Describes a secondary private IP address for a network interface.
data PrivateIpAddressSpecification = PrivateIpAddressSpecification
    { _piasPrivateIpAddress :: Text
    , _piasPrimary :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'PrivateIpAddressSpecification' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @PrivateIpAddress ::@ @Text@
--
-- * @Primary ::@ @Maybe Bool@
--
mkPrivateIpAddressSpecification :: Text -- ^ 'piasPrivateIpAddress'
                                -> PrivateIpAddressSpecification
mkPrivateIpAddressSpecification p1 = PrivateIpAddressSpecification
    { _piasPrivateIpAddress = p1
    , _piasPrimary = Nothing
    }

-- | The private IP addresses.
piasPrivateIpAddress :: Lens' PrivateIpAddressSpecification Text
piasPrivateIpAddress =
    lens _piasPrivateIpAddress (\s a -> s { _piasPrivateIpAddress = a })

-- | Indicates whether the private IP address is the primary private IP address.
piasPrimary :: Lens' PrivateIpAddressSpecification (Maybe Bool)
piasPrimary = lens _piasPrimary (\s a -> s { _piasPrimary = a })

instance FromXML PrivateIpAddressSpecification where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PrivateIpAddressSpecification"

instance ToQuery PrivateIpAddressSpecification where
    toQuery = genericQuery def

-- | Describes a product code.
data ProductCode = ProductCode
    { _pcProductCodeId :: Maybe Text
    , _pcProductCodeType :: Maybe ProductCodeValues
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ProductCode' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ProductCodeId ::@ @Maybe Text@
--
-- * @ProductCodeType ::@ @Maybe ProductCodeValues@
--
mkProductCode :: ProductCode
mkProductCode = ProductCode
    { _pcProductCodeId = Nothing
    , _pcProductCodeType = Nothing
    }

-- | The product code.
pcProductCodeId :: Lens' ProductCode (Maybe Text)
pcProductCodeId = lens _pcProductCodeId (\s a -> s { _pcProductCodeId = a })

-- | The type of product code.
pcProductCodeType :: Lens' ProductCode (Maybe ProductCodeValues)
pcProductCodeType =
    lens _pcProductCodeType (\s a -> s { _pcProductCodeType = a })

instance FromXML ProductCode where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery ProductCode where
    toQuery = genericQuery def

-- | Describes a recurring charge.
data RecurringCharge = RecurringCharge
    { _rcFrequency :: Maybe RecurringChargeFrequency
    , _rcAmount :: Maybe Double
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'RecurringCharge' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Frequency ::@ @Maybe RecurringChargeFrequency@
--
-- * @Amount ::@ @Maybe Double@
--
mkRecurringCharge :: RecurringCharge
mkRecurringCharge = RecurringCharge
    { _rcFrequency = Nothing
    , _rcAmount = Nothing
    }

-- | The frequency of the recurring charge.
rcFrequency :: Lens' RecurringCharge (Maybe RecurringChargeFrequency)
rcFrequency = lens _rcFrequency (\s a -> s { _rcFrequency = a })

-- | The amount of the recurring charge.
rcAmount :: Lens' RecurringCharge (Maybe Double)
rcAmount = lens _rcAmount (\s a -> s { _rcAmount = a })

instance FromXML RecurringCharge where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery RecurringCharge where
    toQuery = genericQuery def

-- | Describes a region.
data Region = Region
    { _r1RegionName :: Maybe Text
    , _r1Endpoint :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Region' data type.
--
-- 'Region' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @RegionName ::@ @Maybe Text@
--
-- * @Endpoint ::@ @Maybe Text@
--
mkRegion :: Region
mkRegion = Region
    { _r1RegionName = Nothing
    , _r1Endpoint = Nothing
    }

-- | The name of the region.
r1RegionName :: Lens' Region (Maybe Text)
r1RegionName = lens _r1RegionName (\s a -> s { _r1RegionName = a })

-- | The region service endpoint.
r1Endpoint :: Lens' Region (Maybe Text)
r1Endpoint = lens _r1Endpoint (\s a -> s { _r1Endpoint = a })

instance FromXML Region where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes a reservation.
data Reservation = Reservation
    { _rrReservationId :: Maybe Text
    , _rrOwnerId :: Maybe Text
    , _rrRequesterId :: Maybe Text
    , _rrGroups :: [GroupIdentifier]
    , _rrInstances :: [Instance]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Reservation' data type.
--
-- 'Reservation' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ReservationId ::@ @Maybe Text@
--
-- * @OwnerId ::@ @Maybe Text@
--
-- * @RequesterId ::@ @Maybe Text@
--
-- * @Groups ::@ @[GroupIdentifier]@
--
-- * @Instances ::@ @[Instance]@
--
mkReservation :: Reservation
mkReservation = Reservation
    { _rrReservationId = Nothing
    , _rrOwnerId = Nothing
    , _rrRequesterId = Nothing
    , _rrGroups = mempty
    , _rrInstances = mempty
    }

-- | The ID of the reservation.
rrReservationId :: Lens' Reservation (Maybe Text)
rrReservationId = lens _rrReservationId (\s a -> s { _rrReservationId = a })

-- | The ID of the AWS account that owns the reservation.
rrOwnerId :: Lens' Reservation (Maybe Text)
rrOwnerId = lens _rrOwnerId (\s a -> s { _rrOwnerId = a })

-- | The ID of the requester that launched the instances on your behalf (for
-- example, AWS Management Console or Auto Scaling).
rrRequesterId :: Lens' Reservation (Maybe Text)
rrRequesterId = lens _rrRequesterId (\s a -> s { _rrRequesterId = a })

-- | One or more security groups.
rrGroups :: Lens' Reservation [GroupIdentifier]
rrGroups = lens _rrGroups (\s a -> s { _rrGroups = a })

-- | One or more instances.
rrInstances :: Lens' Reservation [Instance]
rrInstances = lens _rrInstances (\s a -> s { _rrInstances = a })

instance FromXML Reservation where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Specified for Reserved Instance Marketplace offerings to limit the total
-- order and ensure that the Reserved Instances are not purchased at
-- unexpected prices.
data ReservedInstanceLimitPrice = ReservedInstanceLimitPrice
    { _rilpAmount :: Maybe Double
    , _rilpCurrencyCode :: Maybe CurrencyCodeValues
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ReservedInstanceLimitPrice' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Amount ::@ @Maybe Double@
--
-- * @CurrencyCode ::@ @Maybe CurrencyCodeValues@
--
mkReservedInstanceLimitPrice :: ReservedInstanceLimitPrice
mkReservedInstanceLimitPrice = ReservedInstanceLimitPrice
    { _rilpAmount = Nothing
    , _rilpCurrencyCode = Nothing
    }

-- | Used for Reserved Instance Marketplace offerings. Specifies the limit price
-- on the total order (instanceCount * price).
rilpAmount :: Lens' ReservedInstanceLimitPrice (Maybe Double)
rilpAmount = lens _rilpAmount (\s a -> s { _rilpAmount = a })

-- | The currency in which the limitPrice amount is specified. At this time, the
-- only supported currency is USD.
rilpCurrencyCode :: Lens' ReservedInstanceLimitPrice (Maybe CurrencyCodeValues)
rilpCurrencyCode =
    lens _rilpCurrencyCode (\s a -> s { _rilpCurrencyCode = a })

instance ToQuery ReservedInstanceLimitPrice where
    toQuery = genericQuery def

-- | Describes a Reserved Instance.
data ReservedInstances = ReservedInstances
    { _riReservedInstancesId :: Maybe Text
    , _riInstanceType :: Maybe InstanceType
    , _riAvailabilityZone :: Maybe Text
    , _riStart :: Maybe ISO8601
    , _riEnd :: Maybe ISO8601
    , _riDuration :: Maybe Integer
    , _riUsagePrice :: Maybe Double
    , _riFixedPrice :: Maybe Double
    , _riInstanceCount :: Maybe Integer
    , _riProductDescription :: Maybe RIProductDescription
    , _riState :: Maybe ReservedInstanceState
    , _riTags :: [Tag]
    , _riInstanceTenancy :: Maybe Tenancy
    , _riCurrencyCode :: Maybe CurrencyCodeValues
    , _riOfferingType :: Maybe OfferingTypeValues
    , _riRecurringCharges :: [RecurringCharge]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ReservedInstances' data type.
--
-- 'ReservedInstances' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ReservedInstancesId ::@ @Maybe Text@
--
-- * @InstanceType ::@ @Maybe InstanceType@
--
-- * @AvailabilityZone ::@ @Maybe Text@
--
-- * @Start ::@ @Maybe ISO8601@
--
-- * @End ::@ @Maybe ISO8601@
--
-- * @Duration ::@ @Maybe Integer@
--
-- * @UsagePrice ::@ @Maybe Double@
--
-- * @FixedPrice ::@ @Maybe Double@
--
-- * @InstanceCount ::@ @Maybe Integer@
--
-- * @ProductDescription ::@ @Maybe RIProductDescription@
--
-- * @State ::@ @Maybe ReservedInstanceState@
--
-- * @Tags ::@ @[Tag]@
--
-- * @InstanceTenancy ::@ @Maybe Tenancy@
--
-- * @CurrencyCode ::@ @Maybe CurrencyCodeValues@
--
-- * @OfferingType ::@ @Maybe OfferingTypeValues@
--
-- * @RecurringCharges ::@ @[RecurringCharge]@
--
mkReservedInstances :: ReservedInstances
mkReservedInstances = ReservedInstances
    { _riReservedInstancesId = Nothing
    , _riInstanceType = Nothing
    , _riAvailabilityZone = Nothing
    , _riStart = Nothing
    , _riEnd = Nothing
    , _riDuration = Nothing
    , _riUsagePrice = Nothing
    , _riFixedPrice = Nothing
    , _riInstanceCount = Nothing
    , _riProductDescription = Nothing
    , _riState = Nothing
    , _riTags = mempty
    , _riInstanceTenancy = Nothing
    , _riCurrencyCode = Nothing
    , _riOfferingType = Nothing
    , _riRecurringCharges = mempty
    }

-- | The ID of the Reserved Instance.
riReservedInstancesId :: Lens' ReservedInstances (Maybe Text)
riReservedInstancesId =
    lens _riReservedInstancesId (\s a -> s { _riReservedInstancesId = a })

-- | The instance type on which the Reserved Instance can be used.
riInstanceType :: Lens' ReservedInstances (Maybe InstanceType)
riInstanceType = lens _riInstanceType (\s a -> s { _riInstanceType = a })

-- | The Availability Zone in which the Reserved Instance can be used.
riAvailabilityZone :: Lens' ReservedInstances (Maybe Text)
riAvailabilityZone =
    lens _riAvailabilityZone (\s a -> s { _riAvailabilityZone = a })

-- | The date and time the Reserved Instance started.
riStart :: Lens' ReservedInstances (Maybe ISO8601)
riStart = lens _riStart (\s a -> s { _riStart = a })

-- | The time when the Reserved Instance expires.
riEnd :: Lens' ReservedInstances (Maybe ISO8601)
riEnd = lens _riEnd (\s a -> s { _riEnd = a })

-- | The duration of the Reserved Instance, in seconds.
riDuration :: Lens' ReservedInstances (Maybe Integer)
riDuration = lens _riDuration (\s a -> s { _riDuration = a })

-- | The usage price of the Reserved Instance, per hour.
riUsagePrice :: Lens' ReservedInstances (Maybe Double)
riUsagePrice = lens _riUsagePrice (\s a -> s { _riUsagePrice = a })

-- | The purchase price of the Reserved Instance.
riFixedPrice :: Lens' ReservedInstances (Maybe Double)
riFixedPrice = lens _riFixedPrice (\s a -> s { _riFixedPrice = a })

-- | The number of Reserved Instances purchased.
riInstanceCount :: Lens' ReservedInstances (Maybe Integer)
riInstanceCount = lens _riInstanceCount (\s a -> s { _riInstanceCount = a })

-- | The Reserved Instance description.
riProductDescription :: Lens' ReservedInstances (Maybe RIProductDescription)
riProductDescription =
    lens _riProductDescription (\s a -> s { _riProductDescription = a })

-- | The state of the Reserved Instance purchase.
riState :: Lens' ReservedInstances (Maybe ReservedInstanceState)
riState = lens _riState (\s a -> s { _riState = a })

-- | Any tags assigned to the resource.
riTags :: Lens' ReservedInstances [Tag]
riTags = lens _riTags (\s a -> s { _riTags = a })

-- | The tenancy of the reserved instance.
riInstanceTenancy :: Lens' ReservedInstances (Maybe Tenancy)
riInstanceTenancy =
    lens _riInstanceTenancy (\s a -> s { _riInstanceTenancy = a })

-- | The currency of the Reserved Instance. It's specified using ISO 4217
-- standard currency codes. At this time, the only supported currency is USD.
riCurrencyCode :: Lens' ReservedInstances (Maybe CurrencyCodeValues)
riCurrencyCode = lens _riCurrencyCode (\s a -> s { _riCurrencyCode = a })

-- | The Reserved Instance offering type.
riOfferingType :: Lens' ReservedInstances (Maybe OfferingTypeValues)
riOfferingType = lens _riOfferingType (\s a -> s { _riOfferingType = a })

-- | The recurring charge tag assigned to the resource.
riRecurringCharges :: Lens' ReservedInstances [RecurringCharge]
riRecurringCharges =
    lens _riRecurringCharges (\s a -> s { _riRecurringCharges = a })

instance FromXML ReservedInstances where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | The target Reserved Instances configurations supplied as part of the
-- modification request.
data ReservedInstancesConfiguration = ReservedInstancesConfiguration
    { _ricAvailabilityZone :: Maybe Text
    , _ricPlatform :: Maybe Text
    , _ricInstanceCount :: Maybe Integer
    , _ricInstanceType :: Maybe InstanceType
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ReservedInstancesConfiguration' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AvailabilityZone ::@ @Maybe Text@
--
-- * @Platform ::@ @Maybe Text@
--
-- * @InstanceCount ::@ @Maybe Integer@
--
-- * @InstanceType ::@ @Maybe InstanceType@
--
mkReservedInstancesConfiguration :: ReservedInstancesConfiguration
mkReservedInstancesConfiguration = ReservedInstancesConfiguration
    { _ricAvailabilityZone = Nothing
    , _ricPlatform = Nothing
    , _ricInstanceCount = Nothing
    , _ricInstanceType = Nothing
    }

-- | The Availability Zone for the modified Reserved Instances.
ricAvailabilityZone :: Lens' ReservedInstancesConfiguration (Maybe Text)
ricAvailabilityZone =
    lens _ricAvailabilityZone (\s a -> s { _ricAvailabilityZone = a })

-- | The network platform of the modified Reserved Instances, which is either
-- EC2-Classic or EC2-VPC.
ricPlatform :: Lens' ReservedInstancesConfiguration (Maybe Text)
ricPlatform = lens _ricPlatform (\s a -> s { _ricPlatform = a })

-- | The number of modified Reserved Instances.
ricInstanceCount :: Lens' ReservedInstancesConfiguration (Maybe Integer)
ricInstanceCount =
    lens _ricInstanceCount (\s a -> s { _ricInstanceCount = a })

-- | The instance type for the modified Reserved Instances.
ricInstanceType :: Lens' ReservedInstancesConfiguration (Maybe InstanceType)
ricInstanceType = lens _ricInstanceType (\s a -> s { _ricInstanceType = a })

instance FromXML ReservedInstancesConfiguration where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "targetConfiguration"

instance ToQuery ReservedInstancesConfiguration where
    toQuery = genericQuery def

-- | Describes a Reserved Instance listing.
data ReservedInstancesListing = ReservedInstancesListing
    { _rilReservedInstancesListingId :: Maybe Text
    , _rilReservedInstancesId :: Maybe Text
    , _rilCreateDate :: Maybe ISO8601
    , _rilUpdateDate :: Maybe ISO8601
    , _rilStatus :: Maybe ListingStatus
    , _rilStatusMessage :: Maybe Text
    , _rilInstanceCounts :: [InstanceCount]
    , _rilPriceSchedules :: [PriceSchedule]
    , _rilTags :: [Tag]
    , _rilClientToken :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ReservedInstancesListing' data type.
--
-- 'ReservedInstancesListing' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ReservedInstancesListingId ::@ @Maybe Text@
--
-- * @ReservedInstancesId ::@ @Maybe Text@
--
-- * @CreateDate ::@ @Maybe ISO8601@
--
-- * @UpdateDate ::@ @Maybe ISO8601@
--
-- * @Status ::@ @Maybe ListingStatus@
--
-- * @StatusMessage ::@ @Maybe Text@
--
-- * @InstanceCounts ::@ @[InstanceCount]@
--
-- * @PriceSchedules ::@ @[PriceSchedule]@
--
-- * @Tags ::@ @[Tag]@
--
-- * @ClientToken ::@ @Maybe Text@
--
mkReservedInstancesListing :: ReservedInstancesListing
mkReservedInstancesListing = ReservedInstancesListing
    { _rilReservedInstancesListingId = Nothing
    , _rilReservedInstancesId = Nothing
    , _rilCreateDate = Nothing
    , _rilUpdateDate = Nothing
    , _rilStatus = Nothing
    , _rilStatusMessage = Nothing
    , _rilInstanceCounts = mempty
    , _rilPriceSchedules = mempty
    , _rilTags = mempty
    , _rilClientToken = Nothing
    }

-- | The ID of the Reserved Instance listing.
rilReservedInstancesListingId :: Lens' ReservedInstancesListing (Maybe Text)
rilReservedInstancesListingId =
    lens _rilReservedInstancesListingId
         (\s a -> s { _rilReservedInstancesListingId = a })

-- | The ID of the Reserved Instance.
rilReservedInstancesId :: Lens' ReservedInstancesListing (Maybe Text)
rilReservedInstancesId =
    lens _rilReservedInstancesId (\s a -> s { _rilReservedInstancesId = a })

-- | The time the listing was created.
rilCreateDate :: Lens' ReservedInstancesListing (Maybe ISO8601)
rilCreateDate = lens _rilCreateDate (\s a -> s { _rilCreateDate = a })

-- | The last modified timestamp of the listing.
rilUpdateDate :: Lens' ReservedInstancesListing (Maybe ISO8601)
rilUpdateDate = lens _rilUpdateDate (\s a -> s { _rilUpdateDate = a })

-- | The status of the Reserved Instance listing.
rilStatus :: Lens' ReservedInstancesListing (Maybe ListingStatus)
rilStatus = lens _rilStatus (\s a -> s { _rilStatus = a })

-- | The reason for the current status of the Reserved Instance listing. The
-- response can be blank.
rilStatusMessage :: Lens' ReservedInstancesListing (Maybe Text)
rilStatusMessage =
    lens _rilStatusMessage (\s a -> s { _rilStatusMessage = a })

-- | The number of instances in this state.
rilInstanceCounts :: Lens' ReservedInstancesListing [InstanceCount]
rilInstanceCounts =
    lens _rilInstanceCounts (\s a -> s { _rilInstanceCounts = a })

-- | The price of the Reserved Instance listing.
rilPriceSchedules :: Lens' ReservedInstancesListing [PriceSchedule]
rilPriceSchedules =
    lens _rilPriceSchedules (\s a -> s { _rilPriceSchedules = a })

-- | Any tags assigned to the resource.
rilTags :: Lens' ReservedInstancesListing [Tag]
rilTags = lens _rilTags (\s a -> s { _rilTags = a })

-- | The idempotency token you provided when you created the listing.
rilClientToken :: Lens' ReservedInstancesListing (Maybe Text)
rilClientToken = lens _rilClientToken (\s a -> s { _rilClientToken = a })

instance FromXML ReservedInstancesListing where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes a Reserved Instance modification.
data ReservedInstancesModification = ReservedInstancesModification
    { _rimReservedInstancesModificationId :: Maybe Text
    , _rimReservedInstancesIds :: [ReservedInstancesId]
    , _rimModificationResults :: [ReservedInstancesModificationResult]
    , _rimCreateDate :: Maybe ISO8601
    , _rimUpdateDate :: Maybe ISO8601
    , _rimEffectiveDate :: Maybe ISO8601
    , _rimStatus :: Maybe Text
    , _rimStatusMessage :: Maybe Text
    , _rimClientToken :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ReservedInstancesModification' data type.
--
-- 'ReservedInstancesModification' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ReservedInstancesModificationId ::@ @Maybe Text@
--
-- * @ReservedInstancesIds ::@ @[ReservedInstancesId]@
--
-- * @ModificationResults ::@ @[ReservedInstancesModificationResult]@
--
-- * @CreateDate ::@ @Maybe ISO8601@
--
-- * @UpdateDate ::@ @Maybe ISO8601@
--
-- * @EffectiveDate ::@ @Maybe ISO8601@
--
-- * @Status ::@ @Maybe Text@
--
-- * @StatusMessage ::@ @Maybe Text@
--
-- * @ClientToken ::@ @Maybe Text@
--
mkReservedInstancesModification :: ReservedInstancesModification
mkReservedInstancesModification = ReservedInstancesModification
    { _rimReservedInstancesModificationId = Nothing
    , _rimReservedInstancesIds = mempty
    , _rimModificationResults = mempty
    , _rimCreateDate = Nothing
    , _rimUpdateDate = Nothing
    , _rimEffectiveDate = Nothing
    , _rimStatus = Nothing
    , _rimStatusMessage = Nothing
    , _rimClientToken = Nothing
    }

-- | A unique ID for the Reserved Instance modification.
rimReservedInstancesModificationId :: Lens' ReservedInstancesModification (Maybe Text)
rimReservedInstancesModificationId =
    lens _rimReservedInstancesModificationId
         (\s a -> s { _rimReservedInstancesModificationId = a })

-- | The IDs of one or more Reserved Instances.
rimReservedInstancesIds :: Lens' ReservedInstancesModification [ReservedInstancesId]
rimReservedInstancesIds =
    lens _rimReservedInstancesIds
         (\s a -> s { _rimReservedInstancesIds = a })

-- | Contains target configurations along with their corresponding new Reserved
-- Instance IDs.
rimModificationResults :: Lens' ReservedInstancesModification [ReservedInstancesModificationResult]
rimModificationResults =
    lens _rimModificationResults (\s a -> s { _rimModificationResults = a })

-- | The time when the modification request was created.
rimCreateDate :: Lens' ReservedInstancesModification (Maybe ISO8601)
rimCreateDate = lens _rimCreateDate (\s a -> s { _rimCreateDate = a })

-- | The time when the modification request was last updated.
rimUpdateDate :: Lens' ReservedInstancesModification (Maybe ISO8601)
rimUpdateDate = lens _rimUpdateDate (\s a -> s { _rimUpdateDate = a })

-- | The time for the modification to become effective.
rimEffectiveDate :: Lens' ReservedInstancesModification (Maybe ISO8601)
rimEffectiveDate =
    lens _rimEffectiveDate (\s a -> s { _rimEffectiveDate = a })

-- | The status of the Reserved Instances modification request.
rimStatus :: Lens' ReservedInstancesModification (Maybe Text)
rimStatus = lens _rimStatus (\s a -> s { _rimStatus = a })

-- | The reason for the status.
rimStatusMessage :: Lens' ReservedInstancesModification (Maybe Text)
rimStatusMessage =
    lens _rimStatusMessage (\s a -> s { _rimStatusMessage = a })

-- | A unique, case-sensitive key supplied by the client to ensure that the
-- modification request is idempotent.
rimClientToken :: Lens' ReservedInstancesModification (Maybe Text)
rimClientToken = lens _rimClientToken (\s a -> s { _rimClientToken = a })

instance FromXML ReservedInstancesModification where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

data ReservedInstancesModificationResult = ReservedInstancesModificationResult
    { _rimrReservedInstancesId :: Maybe Text
    , _rimrTargetConfiguration :: Maybe ReservedInstancesConfiguration
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ReservedInstancesModificationResult' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ReservedInstancesId ::@ @Maybe Text@
--
-- * @TargetConfiguration ::@ @Maybe ReservedInstancesConfiguration@
--
mkReservedInstancesModificationResult :: ReservedInstancesModificationResult
mkReservedInstancesModificationResult = ReservedInstancesModificationResult
    { _rimrReservedInstancesId = Nothing
    , _rimrTargetConfiguration = Nothing
    }

-- | The ID for the Reserved Instances that were created as part of the
-- modification request. This field is only available when the modification is
-- fulfilled.
rimrReservedInstancesId :: Lens' ReservedInstancesModificationResult (Maybe Text)
rimrReservedInstancesId =
    lens _rimrReservedInstancesId
         (\s a -> s { _rimrReservedInstancesId = a })

-- | The target Reserved Instances configurations supplied as part of the
-- modification request.
rimrTargetConfiguration :: Lens' ReservedInstancesModificationResult (Maybe ReservedInstancesConfiguration)
rimrTargetConfiguration =
    lens _rimrTargetConfiguration
         (\s a -> s { _rimrTargetConfiguration = a })

instance FromXML ReservedInstancesModificationResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery ReservedInstancesModificationResult where
    toQuery = genericQuery def

-- | Describes a Reserved Instance offering.
data ReservedInstancesOffering = ReservedInstancesOffering
    { _rioReservedInstancesOfferingId :: Maybe Text
    , _rioInstanceType :: Maybe InstanceType
    , _rioAvailabilityZone :: Maybe Text
    , _rioDuration :: Maybe Integer
    , _rioUsagePrice :: Maybe Double
    , _rioFixedPrice :: Maybe Double
    , _rioProductDescription :: Maybe RIProductDescription
    , _rioInstanceTenancy :: Maybe Tenancy
    , _rioCurrencyCode :: Maybe CurrencyCodeValues
    , _rioOfferingType :: Maybe OfferingTypeValues
    , _rioRecurringCharges :: [RecurringCharge]
    , _rioMarketplace :: Maybe Bool
    , _rioPricingDetails :: [PricingDetail]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ReservedInstancesOffering' data type.
--
-- 'ReservedInstancesOffering' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ReservedInstancesOfferingId ::@ @Maybe Text@
--
-- * @InstanceType ::@ @Maybe InstanceType@
--
-- * @AvailabilityZone ::@ @Maybe Text@
--
-- * @Duration ::@ @Maybe Integer@
--
-- * @UsagePrice ::@ @Maybe Double@
--
-- * @FixedPrice ::@ @Maybe Double@
--
-- * @ProductDescription ::@ @Maybe RIProductDescription@
--
-- * @InstanceTenancy ::@ @Maybe Tenancy@
--
-- * @CurrencyCode ::@ @Maybe CurrencyCodeValues@
--
-- * @OfferingType ::@ @Maybe OfferingTypeValues@
--
-- * @RecurringCharges ::@ @[RecurringCharge]@
--
-- * @Marketplace ::@ @Maybe Bool@
--
-- * @PricingDetails ::@ @[PricingDetail]@
--
mkReservedInstancesOffering :: ReservedInstancesOffering
mkReservedInstancesOffering = ReservedInstancesOffering
    { _rioReservedInstancesOfferingId = Nothing
    , _rioInstanceType = Nothing
    , _rioAvailabilityZone = Nothing
    , _rioDuration = Nothing
    , _rioUsagePrice = Nothing
    , _rioFixedPrice = Nothing
    , _rioProductDescription = Nothing
    , _rioInstanceTenancy = Nothing
    , _rioCurrencyCode = Nothing
    , _rioOfferingType = Nothing
    , _rioRecurringCharges = mempty
    , _rioMarketplace = Nothing
    , _rioPricingDetails = mempty
    }

-- | The ID of the Reserved Instance offering.
rioReservedInstancesOfferingId :: Lens' ReservedInstancesOffering (Maybe Text)
rioReservedInstancesOfferingId =
    lens _rioReservedInstancesOfferingId
         (\s a -> s { _rioReservedInstancesOfferingId = a })

-- | The instance type on which the Reserved Instance can be used.
rioInstanceType :: Lens' ReservedInstancesOffering (Maybe InstanceType)
rioInstanceType = lens _rioInstanceType (\s a -> s { _rioInstanceType = a })

-- | The Availability Zone in which the Reserved Instance can be used.
rioAvailabilityZone :: Lens' ReservedInstancesOffering (Maybe Text)
rioAvailabilityZone =
    lens _rioAvailabilityZone (\s a -> s { _rioAvailabilityZone = a })

-- | The duration of the Reserved Instance, in seconds.
rioDuration :: Lens' ReservedInstancesOffering (Maybe Integer)
rioDuration = lens _rioDuration (\s a -> s { _rioDuration = a })

-- | The usage price of the Reserved Instance, per hour.
rioUsagePrice :: Lens' ReservedInstancesOffering (Maybe Double)
rioUsagePrice = lens _rioUsagePrice (\s a -> s { _rioUsagePrice = a })

-- | The purchase price of the Reserved Instance.
rioFixedPrice :: Lens' ReservedInstancesOffering (Maybe Double)
rioFixedPrice = lens _rioFixedPrice (\s a -> s { _rioFixedPrice = a })

-- | The Reserved Instance description.
rioProductDescription :: Lens' ReservedInstancesOffering (Maybe RIProductDescription)
rioProductDescription =
    lens _rioProductDescription (\s a -> s { _rioProductDescription = a })

-- | The tenancy of the reserved instance.
rioInstanceTenancy :: Lens' ReservedInstancesOffering (Maybe Tenancy)
rioInstanceTenancy =
    lens _rioInstanceTenancy (\s a -> s { _rioInstanceTenancy = a })

-- | The currency of the Reserved Instance offering you are purchasing. It's
-- specified using ISO 4217 standard currency codes. At this time, the only
-- supported currency is USD.
rioCurrencyCode :: Lens' ReservedInstancesOffering (Maybe CurrencyCodeValues)
rioCurrencyCode = lens _rioCurrencyCode (\s a -> s { _rioCurrencyCode = a })

-- | The Reserved Instance offering type.
rioOfferingType :: Lens' ReservedInstancesOffering (Maybe OfferingTypeValues)
rioOfferingType = lens _rioOfferingType (\s a -> s { _rioOfferingType = a })

-- | The recurring charge tag assigned to the resource.
rioRecurringCharges :: Lens' ReservedInstancesOffering [RecurringCharge]
rioRecurringCharges =
    lens _rioRecurringCharges (\s a -> s { _rioRecurringCharges = a })

-- | Indicates whether the offering is available through the Reserved Instance
-- Marketplace (resale) or AWS. If it's a Reserved Instance Marketplace
-- offering, this is true.
rioMarketplace :: Lens' ReservedInstancesOffering (Maybe Bool)
rioMarketplace = lens _rioMarketplace (\s a -> s { _rioMarketplace = a })

-- | The pricing details of the Reserved Instance offering.
rioPricingDetails :: Lens' ReservedInstancesOffering [PricingDetail]
rioPricingDetails =
    lens _rioPricingDetails (\s a -> s { _rioPricingDetails = a })

instance FromXML ReservedInstancesOffering where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes a route in a route table.
data Route = Route
    { _rDestinationCidrBlock :: Maybe Text
    , _rGatewayId :: Maybe Text
    , _rInstanceId :: Maybe Text
    , _rInstanceOwnerId :: Maybe Text
    , _rNetworkInterfaceId :: Maybe Text
    , _rVpcPeeringConnectionId :: Maybe Text
    , _rState :: Maybe RouteState
    , _rOrigin :: Maybe RouteOrigin
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Route' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DestinationCidrBlock ::@ @Maybe Text@
--
-- * @GatewayId ::@ @Maybe Text@
--
-- * @InstanceId ::@ @Maybe Text@
--
-- * @InstanceOwnerId ::@ @Maybe Text@
--
-- * @NetworkInterfaceId ::@ @Maybe Text@
--
-- * @VpcPeeringConnectionId ::@ @Maybe Text@
--
-- * @State ::@ @Maybe RouteState@
--
-- * @Origin ::@ @Maybe RouteOrigin@
--
mkRoute :: Route
mkRoute = Route
    { _rDestinationCidrBlock = Nothing
    , _rGatewayId = Nothing
    , _rInstanceId = Nothing
    , _rInstanceOwnerId = Nothing
    , _rNetworkInterfaceId = Nothing
    , _rVpcPeeringConnectionId = Nothing
    , _rState = Nothing
    , _rOrigin = Nothing
    }

-- | The CIDR block used for the destination match.
rDestinationCidrBlock :: Lens' Route (Maybe Text)
rDestinationCidrBlock =
    lens _rDestinationCidrBlock (\s a -> s { _rDestinationCidrBlock = a })

-- | The ID of a gateway attached to your VPC.
rGatewayId :: Lens' Route (Maybe Text)
rGatewayId = lens _rGatewayId (\s a -> s { _rGatewayId = a })

-- | The ID of a NAT instance in your VPC.
rInstanceId :: Lens' Route (Maybe Text)
rInstanceId = lens _rInstanceId (\s a -> s { _rInstanceId = a })

-- | The AWS account ID of the owner of the instance.
rInstanceOwnerId :: Lens' Route (Maybe Text)
rInstanceOwnerId =
    lens _rInstanceOwnerId (\s a -> s { _rInstanceOwnerId = a })

-- | The ID of the network interface.
rNetworkInterfaceId :: Lens' Route (Maybe Text)
rNetworkInterfaceId =
    lens _rNetworkInterfaceId (\s a -> s { _rNetworkInterfaceId = a })

-- | The ID of the VPC peering connection.
rVpcPeeringConnectionId :: Lens' Route (Maybe Text)
rVpcPeeringConnectionId =
    lens _rVpcPeeringConnectionId
         (\s a -> s { _rVpcPeeringConnectionId = a })

-- | The state of the route. The blackhole state indicates that the route's
-- target isn't available (for example, the specified gateway isn't attached
-- to the VPC, or the specified NAT instance has been terminated).
rState :: Lens' Route (Maybe RouteState)
rState = lens _rState (\s a -> s { _rState = a })

-- | Describes how the route was created. CreateRouteTable indicates that route
-- was automatically created when the route table was created. CreateRoute
-- indicates that the route was manually added to the route table.
-- EnableVgwRoutePropagation indicates that the route was propagated by route
-- propagation.
rOrigin :: Lens' Route (Maybe RouteOrigin)
rOrigin = lens _rOrigin (\s a -> s { _rOrigin = a })

instance FromXML Route where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery Route where
    toQuery = genericQuery def

-- | Information about the route table.
data RouteTable = RouteTable
    { _rtRouteTableId :: Maybe Text
    , _rtVpcId :: Maybe Text
    , _rtRoutes :: [Route]
    , _rtAssociations :: [RouteTableAssociation]
    , _rtTags :: [Tag]
    , _rtPropagatingVgws :: [PropagatingVgw]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'RouteTable' data type.
--
-- 'RouteTable' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @RouteTableId ::@ @Maybe Text@
--
-- * @VpcId ::@ @Maybe Text@
--
-- * @Routes ::@ @[Route]@
--
-- * @Associations ::@ @[RouteTableAssociation]@
--
-- * @Tags ::@ @[Tag]@
--
-- * @PropagatingVgws ::@ @[PropagatingVgw]@
--
mkRouteTable :: RouteTable
mkRouteTable = RouteTable
    { _rtRouteTableId = Nothing
    , _rtVpcId = Nothing
    , _rtRoutes = mempty
    , _rtAssociations = mempty
    , _rtTags = mempty
    , _rtPropagatingVgws = mempty
    }

-- | The ID of the route table.
rtRouteTableId :: Lens' RouteTable (Maybe Text)
rtRouteTableId = lens _rtRouteTableId (\s a -> s { _rtRouteTableId = a })

-- | The ID of the VPC.
rtVpcId :: Lens' RouteTable (Maybe Text)
rtVpcId = lens _rtVpcId (\s a -> s { _rtVpcId = a })

-- | The routes in the route table.
rtRoutes :: Lens' RouteTable [Route]
rtRoutes = lens _rtRoutes (\s a -> s { _rtRoutes = a })

-- | The associations between the route table and one or more subnets.
rtAssociations :: Lens' RouteTable [RouteTableAssociation]
rtAssociations = lens _rtAssociations (\s a -> s { _rtAssociations = a })

-- | Any tags assigned to the route table.
rtTags :: Lens' RouteTable [Tag]
rtTags = lens _rtTags (\s a -> s { _rtTags = a })

-- | Any virtual private gateway (VGW) propagating routes.
rtPropagatingVgws :: Lens' RouteTable [PropagatingVgw]
rtPropagatingVgws =
    lens _rtPropagatingVgws (\s a -> s { _rtPropagatingVgws = a })

instance FromXML RouteTable where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "routeTable"

-- | Describes an association between a route table and a subnet.
data RouteTableAssociation = RouteTableAssociation
    { _rtaRouteTableAssociationId :: Maybe Text
    , _rtaRouteTableId :: Maybe Text
    , _rtaSubnetId :: Maybe Text
    , _rtaMain :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'RouteTableAssociation' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @RouteTableAssociationId ::@ @Maybe Text@
--
-- * @RouteTableId ::@ @Maybe Text@
--
-- * @SubnetId ::@ @Maybe Text@
--
-- * @Main ::@ @Maybe Bool@
--
mkRouteTableAssociation :: RouteTableAssociation
mkRouteTableAssociation = RouteTableAssociation
    { _rtaRouteTableAssociationId = Nothing
    , _rtaRouteTableId = Nothing
    , _rtaSubnetId = Nothing
    , _rtaMain = Nothing
    }

-- | The ID of the association between a route table and a subnet.
rtaRouteTableAssociationId :: Lens' RouteTableAssociation (Maybe Text)
rtaRouteTableAssociationId =
    lens _rtaRouteTableAssociationId
         (\s a -> s { _rtaRouteTableAssociationId = a })

-- | The ID of the route table.
rtaRouteTableId :: Lens' RouteTableAssociation (Maybe Text)
rtaRouteTableId = lens _rtaRouteTableId (\s a -> s { _rtaRouteTableId = a })

-- | The ID of the subnet.
rtaSubnetId :: Lens' RouteTableAssociation (Maybe Text)
rtaSubnetId = lens _rtaSubnetId (\s a -> s { _rtaSubnetId = a })

-- | Indicates whether this is the main route table.
rtaMain :: Lens' RouteTableAssociation (Maybe Bool)
rtaMain = lens _rtaMain (\s a -> s { _rtaMain = a })

instance FromXML RouteTableAssociation where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery RouteTableAssociation where
    toQuery = genericQuery def

-- | An Amazon S3 storage location.
data S3Storage = S3Storage
    { _ssBucket :: Maybe Text
    , _ssPrefix :: Maybe Text
    , _ssAWSAccessKeyId :: Maybe Text
    , _ssUploadPolicy :: Maybe ByteString
    , _ssUploadPolicySignature :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'S3Storage' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Bucket ::@ @Maybe Text@
--
-- * @Prefix ::@ @Maybe Text@
--
-- * @AWSAccessKeyId ::@ @Maybe Text@
--
-- * @UploadPolicy ::@ @Maybe ByteString@
--
-- * @UploadPolicySignature ::@ @Maybe Text@
--
mkS3Storage :: S3Storage
mkS3Storage = S3Storage
    { _ssBucket = Nothing
    , _ssPrefix = Nothing
    , _ssAWSAccessKeyId = Nothing
    , _ssUploadPolicy = Nothing
    , _ssUploadPolicySignature = Nothing
    }

-- | The bucket in which to store the AMI. You can specify a bucket that you
-- already own or a new bucket that Amazon EC2 creates on your behalf. If you
-- specify a bucket that belongs to someone else, Amazon EC2 returns an error.
ssBucket :: Lens' S3Storage (Maybe Text)
ssBucket = lens _ssBucket (\s a -> s { _ssBucket = a })

-- | The beginning of the file name of the AMI.
ssPrefix :: Lens' S3Storage (Maybe Text)
ssPrefix = lens _ssPrefix (\s a -> s { _ssPrefix = a })

-- | The access key ID of the owner of the bucket. Before you specify a value
-- for your access key ID, review and follow the guidance in Best Practices
-- for Managing AWS Access Keys.
ssAWSAccessKeyId :: Lens' S3Storage (Maybe Text)
ssAWSAccessKeyId =
    lens _ssAWSAccessKeyId (\s a -> s { _ssAWSAccessKeyId = a })

-- | A Base64-encoded Amazon S3 upload policy that gives Amazon EC2 permission
-- to upload items into Amazon S3 on your behalf.
ssUploadPolicy :: Lens' S3Storage (Maybe ByteString)
ssUploadPolicy = lens _ssUploadPolicy (\s a -> s { _ssUploadPolicy = a })

-- | The signature of the Base64 encoded JSON document.
ssUploadPolicySignature :: Lens' S3Storage (Maybe Text)
ssUploadPolicySignature =
    lens _ssUploadPolicySignature
         (\s a -> s { _ssUploadPolicySignature = a })

instance FromXML S3Storage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "S3Storage"

instance ToQuery S3Storage where
    toQuery = genericQuery def

-- | Describes a security group.
data SecurityGroup = SecurityGroup
    { _sgOwnerId :: Text
    , _sgGroupName :: Text
    , _sgGroupId :: Text
    , _sgDescription :: Text
    , _sgIpPermissions :: [IpPermission]
    , _sgIpPermissionsEgress :: [IpPermission]
    , _sgVpcId :: Maybe Text
    , _sgTags :: [Tag]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'SecurityGroup' data type.
--
-- 'SecurityGroup' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @OwnerId ::@ @Text@
--
-- * @GroupName ::@ @Text@
--
-- * @GroupId ::@ @Text@
--
-- * @Description ::@ @Text@
--
-- * @IpPermissions ::@ @[IpPermission]@
--
-- * @IpPermissionsEgress ::@ @[IpPermission]@
--
-- * @VpcId ::@ @Maybe Text@
--
-- * @Tags ::@ @[Tag]@
--
mkSecurityGroup :: Text -- ^ 'sgOwnerId'
                -> Text -- ^ 'sgGroupName'
                -> Text -- ^ 'sgGroupId'
                -> Text -- ^ 'sgDescription'
                -> SecurityGroup
mkSecurityGroup p1 p2 p3 p4 = SecurityGroup
    { _sgOwnerId = p1
    , _sgGroupName = p2
    , _sgGroupId = p3
    , _sgDescription = p4
    , _sgIpPermissions = mempty
    , _sgIpPermissionsEgress = mempty
    , _sgVpcId = Nothing
    , _sgTags = mempty
    }

-- | The AWS account ID of the owner of the security group.
sgOwnerId :: Lens' SecurityGroup Text
sgOwnerId = lens _sgOwnerId (\s a -> s { _sgOwnerId = a })

-- | The name of the security group.
sgGroupName :: Lens' SecurityGroup Text
sgGroupName = lens _sgGroupName (\s a -> s { _sgGroupName = a })

-- | The ID of the security group.
sgGroupId :: Lens' SecurityGroup Text
sgGroupId = lens _sgGroupId (\s a -> s { _sgGroupId = a })

-- | A description of the security group.
sgDescription :: Lens' SecurityGroup Text
sgDescription = lens _sgDescription (\s a -> s { _sgDescription = a })

-- | One or more inbound rules associated with the security group.
sgIpPermissions :: Lens' SecurityGroup [IpPermission]
sgIpPermissions = lens _sgIpPermissions (\s a -> s { _sgIpPermissions = a })

-- | [EC2-VPC] One or more outbound rules associated with the security group.
sgIpPermissionsEgress :: Lens' SecurityGroup [IpPermission]
sgIpPermissionsEgress =
    lens _sgIpPermissionsEgress (\s a -> s { _sgIpPermissionsEgress = a })

-- | [EC2-VPC] The ID of the VPC for the security group.
sgVpcId :: Lens' SecurityGroup (Maybe Text)
sgVpcId = lens _sgVpcId (\s a -> s { _sgVpcId = a })

-- | Any tags assigned to the security group.
sgTags :: Lens' SecurityGroup [Tag]
sgTags = lens _sgTags (\s a -> s { _sgTags = a })

instance FromXML SecurityGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes a snapshot.
data Snapshot = Snapshot
    { _s1SnapshotId :: Maybe Text
    , _s1VolumeId :: Maybe Text
    , _s1State :: Maybe SnapshotState
    , _s1StartTime :: Maybe ISO8601
    , _s1Progress :: Maybe Text
    , _s1OwnerId :: Maybe Text
    , _s1Description :: Maybe Text
    , _s1VolumeSize :: Maybe Integer
    , _s1OwnerAlias :: Maybe Text
    , _s1Tags :: [Tag]
    , _s1Encrypted :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Snapshot' data type.
--
-- 'Snapshot' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SnapshotId ::@ @Maybe Text@
--
-- * @VolumeId ::@ @Maybe Text@
--
-- * @State ::@ @Maybe SnapshotState@
--
-- * @StartTime ::@ @Maybe ISO8601@
--
-- * @Progress ::@ @Maybe Text@
--
-- * @OwnerId ::@ @Maybe Text@
--
-- * @Description ::@ @Maybe Text@
--
-- * @VolumeSize ::@ @Maybe Integer@
--
-- * @OwnerAlias ::@ @Maybe Text@
--
-- * @Tags ::@ @[Tag]@
--
-- * @Encrypted ::@ @Maybe Bool@
--
mkSnapshot :: Snapshot
mkSnapshot = Snapshot
    { _s1SnapshotId = Nothing
    , _s1VolumeId = Nothing
    , _s1State = Nothing
    , _s1StartTime = Nothing
    , _s1Progress = Nothing
    , _s1OwnerId = Nothing
    , _s1Description = Nothing
    , _s1VolumeSize = Nothing
    , _s1OwnerAlias = Nothing
    , _s1Tags = mempty
    , _s1Encrypted = Nothing
    }

-- | The ID of the snapshot.
s1SnapshotId :: Lens' Snapshot (Maybe Text)
s1SnapshotId = lens _s1SnapshotId (\s a -> s { _s1SnapshotId = a })

-- | The ID of the volume.
s1VolumeId :: Lens' Snapshot (Maybe Text)
s1VolumeId = lens _s1VolumeId (\s a -> s { _s1VolumeId = a })

-- | The snapshot state.
s1State :: Lens' Snapshot (Maybe SnapshotState)
s1State = lens _s1State (\s a -> s { _s1State = a })

-- | The time stamp when the snapshot was initiated.
s1StartTime :: Lens' Snapshot (Maybe ISO8601)
s1StartTime = lens _s1StartTime (\s a -> s { _s1StartTime = a })

-- | The progress of the snapshot, as a percentage.
s1Progress :: Lens' Snapshot (Maybe Text)
s1Progress = lens _s1Progress (\s a -> s { _s1Progress = a })

-- | The AWS account ID of the Amazon EBS snapshot owner.
s1OwnerId :: Lens' Snapshot (Maybe Text)
s1OwnerId = lens _s1OwnerId (\s a -> s { _s1OwnerId = a })

-- | The description for the snapshot.
s1Description :: Lens' Snapshot (Maybe Text)
s1Description = lens _s1Description (\s a -> s { _s1Description = a })

-- | The size of the volume, in GiB.
s1VolumeSize :: Lens' Snapshot (Maybe Integer)
s1VolumeSize = lens _s1VolumeSize (\s a -> s { _s1VolumeSize = a })

-- | The AWS account alias (for example, amazon, self) or AWS account ID that
-- owns the snapshot.
s1OwnerAlias :: Lens' Snapshot (Maybe Text)
s1OwnerAlias = lens _s1OwnerAlias (\s a -> s { _s1OwnerAlias = a })

-- | Any tags assigned to the snapshot.
s1Tags :: Lens' Snapshot [Tag]
s1Tags = lens _s1Tags (\s a -> s { _s1Tags = a })

-- | Indicates whether the snapshot is encrypted.
s1Encrypted :: Lens' Snapshot (Maybe Bool)
s1Encrypted = lens _s1Encrypted (\s a -> s { _s1Encrypted = a })

instance FromXML Snapshot where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | The Spot Instance datafeed subscription.
data SpotDatafeedSubscription = SpotDatafeedSubscription
    { _sdsOwnerId :: Maybe Text
    , _sdsBucket :: Maybe Text
    , _sdsPrefix :: Maybe Text
    , _sdsState :: Maybe DatafeedSubscriptionState
    , _sdsFault :: Maybe SpotInstanceStateFault
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'SpotDatafeedSubscription' data type.
--
-- 'SpotDatafeedSubscription' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @OwnerId ::@ @Maybe Text@
--
-- * @Bucket ::@ @Maybe Text@
--
-- * @Prefix ::@ @Maybe Text@
--
-- * @State ::@ @Maybe DatafeedSubscriptionState@
--
-- * @Fault ::@ @Maybe SpotInstanceStateFault@
--
mkSpotDatafeedSubscription :: SpotDatafeedSubscription
mkSpotDatafeedSubscription = SpotDatafeedSubscription
    { _sdsOwnerId = Nothing
    , _sdsBucket = Nothing
    , _sdsPrefix = Nothing
    , _sdsState = Nothing
    , _sdsFault = Nothing
    }

-- | The AWS account ID of the account.
sdsOwnerId :: Lens' SpotDatafeedSubscription (Maybe Text)
sdsOwnerId = lens _sdsOwnerId (\s a -> s { _sdsOwnerId = a })

-- | The Amazon S3 bucket where the Spot Instance datafeed is located.
sdsBucket :: Lens' SpotDatafeedSubscription (Maybe Text)
sdsBucket = lens _sdsBucket (\s a -> s { _sdsBucket = a })

-- | The prefix that is prepended to datafeed files.
sdsPrefix :: Lens' SpotDatafeedSubscription (Maybe Text)
sdsPrefix = lens _sdsPrefix (\s a -> s { _sdsPrefix = a })

-- | The state of the Spot Instance datafeed subscription.
sdsState :: Lens' SpotDatafeedSubscription (Maybe DatafeedSubscriptionState)
sdsState = lens _sdsState (\s a -> s { _sdsState = a })

-- | The fault codes for the Spot Instance request, if any.
sdsFault :: Lens' SpotDatafeedSubscription (Maybe SpotInstanceStateFault)
sdsFault = lens _sdsFault (\s a -> s { _sdsFault = a })

instance FromXML SpotDatafeedSubscription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "spotDatafeedSubscription"

-- | Describe a Spot Instance request.
data SpotInstanceRequest = SpotInstanceRequest
    { _sirSpotInstanceRequestId :: Maybe Text
    , _sirSpotPrice :: Maybe Text
    , _sirType :: Maybe SpotInstanceType
    , _sirState :: Maybe SpotInstanceState
    , _sirFault :: Maybe SpotInstanceStateFault
    , _sirStatus :: Maybe SpotInstanceStatus
    , _sirValidFrom :: Maybe ISO8601
    , _sirValidUntil :: Maybe ISO8601
    , _sirLaunchGroup :: Maybe Text
    , _sirAvailabilityZoneGroup :: Maybe Text
    , _sirLaunchSpecification :: Maybe LaunchSpecification
    , _sirInstanceId :: Maybe Text
    , _sirCreateTime :: Maybe ISO8601
    , _sirProductDescription :: Maybe RIProductDescription
    , _sirTags :: [Tag]
    , _sirLaunchedAvailabilityZone :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'SpotInstanceRequest' data type.
--
-- 'SpotInstanceRequest' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SpotInstanceRequestId ::@ @Maybe Text@
--
-- * @SpotPrice ::@ @Maybe Text@
--
-- * @Type ::@ @Maybe SpotInstanceType@
--
-- * @State ::@ @Maybe SpotInstanceState@
--
-- * @Fault ::@ @Maybe SpotInstanceStateFault@
--
-- * @Status ::@ @Maybe SpotInstanceStatus@
--
-- * @ValidFrom ::@ @Maybe ISO8601@
--
-- * @ValidUntil ::@ @Maybe ISO8601@
--
-- * @LaunchGroup ::@ @Maybe Text@
--
-- * @AvailabilityZoneGroup ::@ @Maybe Text@
--
-- * @LaunchSpecification ::@ @Maybe LaunchSpecification@
--
-- * @InstanceId ::@ @Maybe Text@
--
-- * @CreateTime ::@ @Maybe ISO8601@
--
-- * @ProductDescription ::@ @Maybe RIProductDescription@
--
-- * @Tags ::@ @[Tag]@
--
-- * @LaunchedAvailabilityZone ::@ @Maybe Text@
--
mkSpotInstanceRequest :: SpotInstanceRequest
mkSpotInstanceRequest = SpotInstanceRequest
    { _sirSpotInstanceRequestId = Nothing
    , _sirSpotPrice = Nothing
    , _sirType = Nothing
    , _sirState = Nothing
    , _sirFault = Nothing
    , _sirStatus = Nothing
    , _sirValidFrom = Nothing
    , _sirValidUntil = Nothing
    , _sirLaunchGroup = Nothing
    , _sirAvailabilityZoneGroup = Nothing
    , _sirLaunchSpecification = Nothing
    , _sirInstanceId = Nothing
    , _sirCreateTime = Nothing
    , _sirProductDescription = Nothing
    , _sirTags = mempty
    , _sirLaunchedAvailabilityZone = Nothing
    }

-- | The ID of the Spot Instance request.
sirSpotInstanceRequestId :: Lens' SpotInstanceRequest (Maybe Text)
sirSpotInstanceRequestId =
    lens _sirSpotInstanceRequestId
         (\s a -> s { _sirSpotInstanceRequestId = a })

-- | The maximum hourly price for any Spot Instance launched to fulfill the
-- request.
sirSpotPrice :: Lens' SpotInstanceRequest (Maybe Text)
sirSpotPrice = lens _sirSpotPrice (\s a -> s { _sirSpotPrice = a })

-- | The Spot Instance request type.
sirType :: Lens' SpotInstanceRequest (Maybe SpotInstanceType)
sirType = lens _sirType (\s a -> s { _sirType = a })

-- | The state of the Spot Instance request. Spot bid status information can
-- help you track your Spot Instance requests. For information, see Tracking
-- Spot Requests with Bid Status Codes in the Amazon Elastic Compute Cloud
-- User Guide.
sirState :: Lens' SpotInstanceRequest (Maybe SpotInstanceState)
sirState = lens _sirState (\s a -> s { _sirState = a })

-- | The fault codes for the Spot Instance request, if any.
sirFault :: Lens' SpotInstanceRequest (Maybe SpotInstanceStateFault)
sirFault = lens _sirFault (\s a -> s { _sirFault = a })

-- | The status code and status message describing the Spot Instance request.
sirStatus :: Lens' SpotInstanceRequest (Maybe SpotInstanceStatus)
sirStatus = lens _sirStatus (\s a -> s { _sirStatus = a })

-- | The start date of the request. If this is a one-time request, the request
-- becomes active at this date and time and remains active until all instances
-- launch, the request expires, or the request is canceled. If the request is
-- persistent, the request becomes active at this date and time and remains
-- active until it expires or is canceled.
sirValidFrom :: Lens' SpotInstanceRequest (Maybe ISO8601)
sirValidFrom = lens _sirValidFrom (\s a -> s { _sirValidFrom = a })

-- | The end date of the request. If this is a one-time request, the request
-- remains active until all instances launch, the request is canceled, or this
-- date is reached. If the request is persistent, it remains active until it
-- is canceled or this date is reached.
sirValidUntil :: Lens' SpotInstanceRequest (Maybe ISO8601)
sirValidUntil = lens _sirValidUntil (\s a -> s { _sirValidUntil = a })

-- | The instance launch group. Launch groups are Spot Instances that launch
-- together and terminate together.
sirLaunchGroup :: Lens' SpotInstanceRequest (Maybe Text)
sirLaunchGroup = lens _sirLaunchGroup (\s a -> s { _sirLaunchGroup = a })

-- | The Availability Zone group. If you specify the same Availability Zone
-- group for all Spot Instance requests, all Spot Instances are launched in
-- the same Availability Zone.
sirAvailabilityZoneGroup :: Lens' SpotInstanceRequest (Maybe Text)
sirAvailabilityZoneGroup =
    lens _sirAvailabilityZoneGroup
         (\s a -> s { _sirAvailabilityZoneGroup = a })

-- | Additional information for launching instances.
sirLaunchSpecification :: Lens' SpotInstanceRequest (Maybe LaunchSpecification)
sirLaunchSpecification =
    lens _sirLaunchSpecification (\s a -> s { _sirLaunchSpecification = a })

-- | The instance ID, if an instance has been launched to fulfill the Spot
-- Instance request.
sirInstanceId :: Lens' SpotInstanceRequest (Maybe Text)
sirInstanceId = lens _sirInstanceId (\s a -> s { _sirInstanceId = a })

-- | The time stamp when the Spot Instance request was created.
sirCreateTime :: Lens' SpotInstanceRequest (Maybe ISO8601)
sirCreateTime = lens _sirCreateTime (\s a -> s { _sirCreateTime = a })

-- | The product description associated with the Spot Instance.
sirProductDescription :: Lens' SpotInstanceRequest (Maybe RIProductDescription)
sirProductDescription =
    lens _sirProductDescription (\s a -> s { _sirProductDescription = a })

-- | Any tags assigned to the resource.
sirTags :: Lens' SpotInstanceRequest [Tag]
sirTags = lens _sirTags (\s a -> s { _sirTags = a })

-- | The Availability Zone in which the bid is launched.
sirLaunchedAvailabilityZone :: Lens' SpotInstanceRequest (Maybe Text)
sirLaunchedAvailabilityZone =
    lens _sirLaunchedAvailabilityZone
         (\s a -> s { _sirLaunchedAvailabilityZone = a })

instance FromXML SpotInstanceRequest where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | The fault codes for the Spot Instance request, if any.
data SpotInstanceStateFault = SpotInstanceStateFault
    { _sisfCode :: Maybe Text
    , _sisfMessage :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'SpotInstanceStateFault' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Code ::@ @Maybe Text@
--
-- * @Message ::@ @Maybe Text@
--
mkSpotInstanceStateFault :: SpotInstanceStateFault
mkSpotInstanceStateFault = SpotInstanceStateFault
    { _sisfCode = Nothing
    , _sisfMessage = Nothing
    }

-- | The reason code for the Spot Instance state change.
sisfCode :: Lens' SpotInstanceStateFault (Maybe Text)
sisfCode = lens _sisfCode (\s a -> s { _sisfCode = a })

-- | The message for the Spot Instance state change.
sisfMessage :: Lens' SpotInstanceStateFault (Maybe Text)
sisfMessage = lens _sisfMessage (\s a -> s { _sisfMessage = a })

instance FromXML SpotInstanceStateFault where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "fault"

instance ToQuery SpotInstanceStateFault where
    toQuery = genericQuery def

-- | The status code and status message describing the Spot Instance request.
data SpotInstanceStatus = SpotInstanceStatus
    { _sisCode :: Maybe Text
    , _sisUpdateTime :: Maybe ISO8601
    , _sisMessage :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'SpotInstanceStatus' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Code ::@ @Maybe Text@
--
-- * @UpdateTime ::@ @Maybe ISO8601@
--
-- * @Message ::@ @Maybe Text@
--
mkSpotInstanceStatus :: SpotInstanceStatus
mkSpotInstanceStatus = SpotInstanceStatus
    { _sisCode = Nothing
    , _sisUpdateTime = Nothing
    , _sisMessage = Nothing
    }

-- | The status code of the request.
sisCode :: Lens' SpotInstanceStatus (Maybe Text)
sisCode = lens _sisCode (\s a -> s { _sisCode = a })

-- | The time of the most recent status update.
sisUpdateTime :: Lens' SpotInstanceStatus (Maybe ISO8601)
sisUpdateTime = lens _sisUpdateTime (\s a -> s { _sisUpdateTime = a })

-- | The description for the status code for the Spot request.
sisMessage :: Lens' SpotInstanceStatus (Maybe Text)
sisMessage = lens _sisMessage (\s a -> s { _sisMessage = a })

instance FromXML SpotInstanceStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "status"

instance ToQuery SpotInstanceStatus where
    toQuery = genericQuery def

-- | The placement information for the instance.
data SpotPlacement = SpotPlacement
    { _spAvailabilityZone :: Maybe Text
    , _spGroupName :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'SpotPlacement' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AvailabilityZone ::@ @Maybe Text@
--
-- * @GroupName ::@ @Maybe Text@
--
mkSpotPlacement :: SpotPlacement
mkSpotPlacement = SpotPlacement
    { _spAvailabilityZone = Nothing
    , _spGroupName = Nothing
    }

-- | The Availability Zone.
spAvailabilityZone :: Lens' SpotPlacement (Maybe Text)
spAvailabilityZone =
    lens _spAvailabilityZone (\s a -> s { _spAvailabilityZone = a })

-- | The Availability Zone group name.
spGroupName :: Lens' SpotPlacement (Maybe Text)
spGroupName = lens _spGroupName (\s a -> s { _spGroupName = a })

instance FromXML SpotPlacement where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "placement"

instance ToQuery SpotPlacement where
    toQuery = genericQuery def

-- | Describes the Spot Price.
data SpotPrice = SpotPrice
    { _sprInstanceType :: Maybe InstanceType
    , _sprProductDescription :: Maybe RIProductDescription
    , _sprSpotPrice :: Maybe Text
    , _sprTimestamp :: Maybe ISO8601
    , _sprAvailabilityZone :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'SpotPrice' data type.
--
-- 'SpotPrice' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InstanceType ::@ @Maybe InstanceType@
--
-- * @ProductDescription ::@ @Maybe RIProductDescription@
--
-- * @SpotPrice ::@ @Maybe Text@
--
-- * @Timestamp ::@ @Maybe ISO8601@
--
-- * @AvailabilityZone ::@ @Maybe Text@
--
mkSpotPrice :: SpotPrice
mkSpotPrice = SpotPrice
    { _sprInstanceType = Nothing
    , _sprProductDescription = Nothing
    , _sprSpotPrice = Nothing
    , _sprTimestamp = Nothing
    , _sprAvailabilityZone = Nothing
    }

-- | The instance type.
sprInstanceType :: Lens' SpotPrice (Maybe InstanceType)
sprInstanceType = lens _sprInstanceType (\s a -> s { _sprInstanceType = a })

-- | A general description of the AMI.
sprProductDescription :: Lens' SpotPrice (Maybe RIProductDescription)
sprProductDescription =
    lens _sprProductDescription (\s a -> s { _sprProductDescription = a })

-- | The maximum price you will pay to launch one or more Spot Instances.
sprSpotPrice :: Lens' SpotPrice (Maybe Text)
sprSpotPrice = lens _sprSpotPrice (\s a -> s { _sprSpotPrice = a })

-- | The date and time the request was created.
sprTimestamp :: Lens' SpotPrice (Maybe ISO8601)
sprTimestamp = lens _sprTimestamp (\s a -> s { _sprTimestamp = a })

-- | The Availability Zone.
sprAvailabilityZone :: Lens' SpotPrice (Maybe Text)
sprAvailabilityZone =
    lens _sprAvailabilityZone (\s a -> s { _sprAvailabilityZone = a })

instance FromXML SpotPrice where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | The reason for the state change.
data StateReason = StateReason
    { _sr1Code :: Maybe Text
    , _sr1Message :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'StateReason' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Code ::@ @Maybe Text@
--
-- * @Message ::@ @Maybe Text@
--
mkStateReason :: StateReason
mkStateReason = StateReason
    { _sr1Code = Nothing
    , _sr1Message = Nothing
    }

-- | The reason code for the state change.
sr1Code :: Lens' StateReason (Maybe Text)
sr1Code = lens _sr1Code (\s a -> s { _sr1Code = a })

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
sr1Message :: Lens' StateReason (Maybe Text)
sr1Message = lens _sr1Message (\s a -> s { _sr1Message = a })

instance FromXML StateReason where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "stateReason"

instance ToQuery StateReason where
    toQuery = genericQuery def

-- | Information about the subnet.
data Subnet = Subnet
    { _srSubnetId :: Maybe Text
    , _srState :: Maybe SubnetState
    , _srVpcId :: Maybe Text
    , _srCidrBlock :: Maybe Text
    , _srAvailableIpAddressCount :: Maybe Integer
    , _srAvailabilityZone :: Maybe Text
    , _srDefaultForAz :: Maybe Bool
    , _srMapPublicIpOnLaunch :: Maybe Bool
    , _srTags :: [Tag]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Subnet' data type.
--
-- 'Subnet' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SubnetId ::@ @Maybe Text@
--
-- * @State ::@ @Maybe SubnetState@
--
-- * @VpcId ::@ @Maybe Text@
--
-- * @CidrBlock ::@ @Maybe Text@
--
-- * @AvailableIpAddressCount ::@ @Maybe Integer@
--
-- * @AvailabilityZone ::@ @Maybe Text@
--
-- * @DefaultForAz ::@ @Maybe Bool@
--
-- * @MapPublicIpOnLaunch ::@ @Maybe Bool@
--
-- * @Tags ::@ @[Tag]@
--
mkSubnet :: Subnet
mkSubnet = Subnet
    { _srSubnetId = Nothing
    , _srState = Nothing
    , _srVpcId = Nothing
    , _srCidrBlock = Nothing
    , _srAvailableIpAddressCount = Nothing
    , _srAvailabilityZone = Nothing
    , _srDefaultForAz = Nothing
    , _srMapPublicIpOnLaunch = Nothing
    , _srTags = mempty
    }

-- | The ID of the subnet.
srSubnetId :: Lens' Subnet (Maybe Text)
srSubnetId = lens _srSubnetId (\s a -> s { _srSubnetId = a })

-- | The current state of the subnet.
srState :: Lens' Subnet (Maybe SubnetState)
srState = lens _srState (\s a -> s { _srState = a })

-- | The ID of the VPC the subnet is in.
srVpcId :: Lens' Subnet (Maybe Text)
srVpcId = lens _srVpcId (\s a -> s { _srVpcId = a })

-- | The CIDR block assigned to the subnet.
srCidrBlock :: Lens' Subnet (Maybe Text)
srCidrBlock = lens _srCidrBlock (\s a -> s { _srCidrBlock = a })

-- | The number of unused IP addresses in the subnet. Note that the IP addresses
-- for any stopped instances are considered unavailable.
srAvailableIpAddressCount :: Lens' Subnet (Maybe Integer)
srAvailableIpAddressCount =
    lens _srAvailableIpAddressCount
         (\s a -> s { _srAvailableIpAddressCount = a })

-- | The Availability Zone of the subnet.
srAvailabilityZone :: Lens' Subnet (Maybe Text)
srAvailabilityZone =
    lens _srAvailabilityZone (\s a -> s { _srAvailabilityZone = a })

-- | Indicates whether this is the default subnet for the Availability Zone.
srDefaultForAz :: Lens' Subnet (Maybe Bool)
srDefaultForAz = lens _srDefaultForAz (\s a -> s { _srDefaultForAz = a })

-- | Indicates whether instances launched in this subnet receive a public IP
-- address.
srMapPublicIpOnLaunch :: Lens' Subnet (Maybe Bool)
srMapPublicIpOnLaunch =
    lens _srMapPublicIpOnLaunch (\s a -> s { _srMapPublicIpOnLaunch = a })

-- | Any tags assigned to the subnet.
srTags :: Lens' Subnet [Tag]
srTags = lens _srTags (\s a -> s { _srTags = a })

instance FromXML Subnet where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "subnet"

-- | Describes a tag.
data Tag = Tag
    { _tKey :: Text
    , _tValue :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Tag' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Key ::@ @Text@
--
-- * @Value ::@ @Text@
--
mkTag :: Text -- ^ 'tKey'
      -> Text -- ^ 'tValue'
      -> Tag
mkTag p1 p2 = Tag
    { _tKey = p1
    , _tValue = p2
    }

-- | The key of the tag. Constraints: Tag keys are case-sensitive and accept a
-- maximum of 127 Unicode characters. May not begin with aws:.
tKey :: Lens' Tag Text
tKey = lens _tKey (\s a -> s { _tKey = a })

-- | The value of the tag. Constraints: Tag values are case-sensitive and accept
-- a maximum of 255 Unicode characters.
tValue :: Lens' Tag Text
tValue = lens _tValue (\s a -> s { _tValue = a })

instance FromXML Tag where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery Tag where
    toQuery = genericQuery def

-- | Describes a tag.
data TagDescription = TagDescription
    { _tdResourceId :: Text
    , _tdResourceType :: ResourceType
    , _tdKey :: Text
    , _tdValue :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'TagDescription' data type.
--
-- 'TagDescription' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ResourceId ::@ @Text@
--
-- * @ResourceType ::@ @ResourceType@
--
-- * @Key ::@ @Text@
--
-- * @Value ::@ @Text@
--
mkTagDescription :: Text -- ^ 'tdResourceId'
                 -> ResourceType -- ^ 'tdResourceType'
                 -> Text -- ^ 'tdKey'
                 -> Text -- ^ 'tdValue'
                 -> TagDescription
mkTagDescription p1 p2 p3 p4 = TagDescription
    { _tdResourceId = p1
    , _tdResourceType = p2
    , _tdKey = p3
    , _tdValue = p4
    }

-- | The ID of the resource. For example, ami-1a2b3c4d.
tdResourceId :: Lens' TagDescription Text
tdResourceId = lens _tdResourceId (\s a -> s { _tdResourceId = a })

-- | The type of resource.
tdResourceType :: Lens' TagDescription ResourceType
tdResourceType = lens _tdResourceType (\s a -> s { _tdResourceType = a })

-- | The key of the tag.
tdKey :: Lens' TagDescription Text
tdKey = lens _tdKey (\s a -> s { _tdKey = a })

-- | The value of the tag.
tdValue :: Lens' TagDescription Text
tdValue = lens _tdValue (\s a -> s { _tdValue = a })

instance FromXML TagDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes a security group and AWS account ID pair for EC2-Classic.
data UserIdGroupPair = UserIdGroupPair
    { _uigpUserId :: Maybe Text
    , _uigpGroupName :: Maybe Text
    , _uigpGroupId :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'UserIdGroupPair' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @UserId ::@ @Maybe Text@
--
-- * @GroupName ::@ @Maybe Text@
--
-- * @GroupId ::@ @Maybe Text@
--
mkUserIdGroupPair :: UserIdGroupPair
mkUserIdGroupPair = UserIdGroupPair
    { _uigpUserId = Nothing
    , _uigpGroupName = Nothing
    , _uigpGroupId = Nothing
    }

-- | The ID of an AWS account.
uigpUserId :: Lens' UserIdGroupPair (Maybe Text)
uigpUserId = lens _uigpUserId (\s a -> s { _uigpUserId = a })

-- | The ID of the security group owned by the specified AWS account.
uigpGroupName :: Lens' UserIdGroupPair (Maybe Text)
uigpGroupName = lens _uigpGroupName (\s a -> s { _uigpGroupName = a })

-- | The name of the security group in the specified AWS account.
uigpGroupId :: Lens' UserIdGroupPair (Maybe Text)
uigpGroupId = lens _uigpGroupId (\s a -> s { _uigpGroupId = a })

instance FromXML UserIdGroupPair where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Groups"

instance ToQuery UserIdGroupPair where
    toQuery = genericQuery def

-- | Describes telemetry for a VPN tunnel.
data VgwTelemetry = VgwTelemetry
    { _vtOutsideIpAddress :: Maybe Text
    , _vtStatus :: Maybe TelemetryStatus
    , _vtLastStatusChange :: Maybe ISO8601
    , _vtStatusMessage :: Maybe Text
    , _vtAcceptedRouteCount :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VgwTelemetry' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @OutsideIpAddress ::@ @Maybe Text@
--
-- * @Status ::@ @Maybe TelemetryStatus@
--
-- * @LastStatusChange ::@ @Maybe ISO8601@
--
-- * @StatusMessage ::@ @Maybe Text@
--
-- * @AcceptedRouteCount ::@ @Maybe Integer@
--
mkVgwTelemetry :: VgwTelemetry
mkVgwTelemetry = VgwTelemetry
    { _vtOutsideIpAddress = Nothing
    , _vtStatus = Nothing
    , _vtLastStatusChange = Nothing
    , _vtStatusMessage = Nothing
    , _vtAcceptedRouteCount = Nothing
    }

-- | The Internet-routable IP address of the virtual private gateway's outside
-- interface.
vtOutsideIpAddress :: Lens' VgwTelemetry (Maybe Text)
vtOutsideIpAddress =
    lens _vtOutsideIpAddress (\s a -> s { _vtOutsideIpAddress = a })

-- | The status of the VPN tunnel.
vtStatus :: Lens' VgwTelemetry (Maybe TelemetryStatus)
vtStatus = lens _vtStatus (\s a -> s { _vtStatus = a })

-- | The date and time of the last change in status.
vtLastStatusChange :: Lens' VgwTelemetry (Maybe ISO8601)
vtLastStatusChange =
    lens _vtLastStatusChange (\s a -> s { _vtLastStatusChange = a })

-- | If an error occurs, a description of the error.
vtStatusMessage :: Lens' VgwTelemetry (Maybe Text)
vtStatusMessage = lens _vtStatusMessage (\s a -> s { _vtStatusMessage = a })

-- | The number of accepted routes.
vtAcceptedRouteCount :: Lens' VgwTelemetry (Maybe Integer)
vtAcceptedRouteCount =
    lens _vtAcceptedRouteCount (\s a -> s { _vtAcceptedRouteCount = a })

instance FromXML VgwTelemetry where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery VgwTelemetry where
    toQuery = genericQuery def

-- | Describes a volume.
data Volume = Volume
    { _vrVolumeId :: Maybe Text
    , _vrSize :: Maybe Integer
    , _vrSnapshotId :: Maybe Text
    , _vrAvailabilityZone :: Maybe Text
    , _vrState :: Maybe VolumeState
    , _vrCreateTime :: Maybe ISO8601
    , _vrAttachments :: [VolumeAttachment]
    , _vrTags :: [Tag]
    , _vrVolumeType :: Maybe VolumeType
    , _vrIops :: Maybe Integer
    , _vrEncrypted :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Volume' data type.
--
-- 'Volume' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VolumeId ::@ @Maybe Text@
--
-- * @Size ::@ @Maybe Integer@
--
-- * @SnapshotId ::@ @Maybe Text@
--
-- * @AvailabilityZone ::@ @Maybe Text@
--
-- * @State ::@ @Maybe VolumeState@
--
-- * @CreateTime ::@ @Maybe ISO8601@
--
-- * @Attachments ::@ @[VolumeAttachment]@
--
-- * @Tags ::@ @[Tag]@
--
-- * @VolumeType ::@ @Maybe VolumeType@
--
-- * @Iops ::@ @Maybe Integer@
--
-- * @Encrypted ::@ @Maybe Bool@
--
mkVolume :: Volume
mkVolume = Volume
    { _vrVolumeId = Nothing
    , _vrSize = Nothing
    , _vrSnapshotId = Nothing
    , _vrAvailabilityZone = Nothing
    , _vrState = Nothing
    , _vrCreateTime = Nothing
    , _vrAttachments = mempty
    , _vrTags = mempty
    , _vrVolumeType = Nothing
    , _vrIops = Nothing
    , _vrEncrypted = Nothing
    }

-- | The ID of the volume.
vrVolumeId :: Lens' Volume (Maybe Text)
vrVolumeId = lens _vrVolumeId (\s a -> s { _vrVolumeId = a })

-- | The size of the volume, in GiBs.
vrSize :: Lens' Volume (Maybe Integer)
vrSize = lens _vrSize (\s a -> s { _vrSize = a })

-- | The snapshot from which the volume was created, if applicable.
vrSnapshotId :: Lens' Volume (Maybe Text)
vrSnapshotId = lens _vrSnapshotId (\s a -> s { _vrSnapshotId = a })

-- | The Availability Zone for the volume.
vrAvailabilityZone :: Lens' Volume (Maybe Text)
vrAvailabilityZone =
    lens _vrAvailabilityZone (\s a -> s { _vrAvailabilityZone = a })

-- | The volume state.
vrState :: Lens' Volume (Maybe VolumeState)
vrState = lens _vrState (\s a -> s { _vrState = a })

-- | The time stamp when volume creation was initiated.
vrCreateTime :: Lens' Volume (Maybe ISO8601)
vrCreateTime = lens _vrCreateTime (\s a -> s { _vrCreateTime = a })

-- | 
vrAttachments :: Lens' Volume [VolumeAttachment]
vrAttachments = lens _vrAttachments (\s a -> s { _vrAttachments = a })

-- | Any tags assigned to the volume.
vrTags :: Lens' Volume [Tag]
vrTags = lens _vrTags (\s a -> s { _vrTags = a })

-- | The volume type. This can be gp2 for General Purpose (SSD) volumes, io1 for
-- Provisioned IOPS (SSD) volumes, or standard for Magnetic volumes.
vrVolumeType :: Lens' Volume (Maybe VolumeType)
vrVolumeType = lens _vrVolumeType (\s a -> s { _vrVolumeType = a })

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
vrIops :: Lens' Volume (Maybe Integer)
vrIops = lens _vrIops (\s a -> s { _vrIops = a })

-- | Indicates whether the volume is encrypted.
vrEncrypted :: Lens' Volume (Maybe Bool)
vrEncrypted = lens _vrEncrypted (\s a -> s { _vrEncrypted = a })

instance FromXML Volume where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Describes volume attachment details.
data VolumeAttachment = VolumeAttachment
    { _varVolumeId :: Maybe Text
    , _varInstanceId :: Maybe Text
    , _varDevice :: Maybe Text
    , _varState :: Maybe VolumeAttachmentState
    , _varAttachTime :: Maybe ISO8601
    , _varDeleteOnTermination :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VolumeAttachment' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VolumeId ::@ @Maybe Text@
--
-- * @InstanceId ::@ @Maybe Text@
--
-- * @Device ::@ @Maybe Text@
--
-- * @State ::@ @Maybe VolumeAttachmentState@
--
-- * @AttachTime ::@ @Maybe ISO8601@
--
-- * @DeleteOnTermination ::@ @Maybe Bool@
--
mkVolumeAttachment :: VolumeAttachment
mkVolumeAttachment = VolumeAttachment
    { _varVolumeId = Nothing
    , _varInstanceId = Nothing
    , _varDevice = Nothing
    , _varState = Nothing
    , _varAttachTime = Nothing
    , _varDeleteOnTermination = Nothing
    }

-- | The ID of the volume.
varVolumeId :: Lens' VolumeAttachment (Maybe Text)
varVolumeId = lens _varVolumeId (\s a -> s { _varVolumeId = a })

-- | The ID of the instance.
varInstanceId :: Lens' VolumeAttachment (Maybe Text)
varInstanceId = lens _varInstanceId (\s a -> s { _varInstanceId = a })

-- | The device name.
varDevice :: Lens' VolumeAttachment (Maybe Text)
varDevice = lens _varDevice (\s a -> s { _varDevice = a })

-- | The attachment state of the volume.
varState :: Lens' VolumeAttachment (Maybe VolumeAttachmentState)
varState = lens _varState (\s a -> s { _varState = a })

-- | The time stamp when the attachment initiated.
varAttachTime :: Lens' VolumeAttachment (Maybe ISO8601)
varAttachTime = lens _varAttachTime (\s a -> s { _varAttachTime = a })

-- | Indicates whether the Amazon EBS volume is deleted on instance termination.
varDeleteOnTermination :: Lens' VolumeAttachment (Maybe Bool)
varDeleteOnTermination =
    lens _varDeleteOnTermination (\s a -> s { _varDeleteOnTermination = a })

instance FromXML VolumeAttachment where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery VolumeAttachment where
    toQuery = genericQuery def

-- | Describes a volume status operation code.
data VolumeStatusAction = VolumeStatusAction
    { _vsaCode :: Maybe Text
    , _vsaDescription :: Maybe Text
    , _vsaEventType :: Maybe Text
    , _vsaEventId :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VolumeStatusAction' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Code ::@ @Maybe Text@
--
-- * @Description ::@ @Maybe Text@
--
-- * @EventType ::@ @Maybe Text@
--
-- * @EventId ::@ @Maybe Text@
--
mkVolumeStatusAction :: VolumeStatusAction
mkVolumeStatusAction = VolumeStatusAction
    { _vsaCode = Nothing
    , _vsaDescription = Nothing
    , _vsaEventType = Nothing
    , _vsaEventId = Nothing
    }

-- | The code identifying the operation, for example, enable-volume-io.
vsaCode :: Lens' VolumeStatusAction (Maybe Text)
vsaCode = lens _vsaCode (\s a -> s { _vsaCode = a })

-- | A description of the operation.
vsaDescription :: Lens' VolumeStatusAction (Maybe Text)
vsaDescription = lens _vsaDescription (\s a -> s { _vsaDescription = a })

-- | The event type associated with this operation.
vsaEventType :: Lens' VolumeStatusAction (Maybe Text)
vsaEventType = lens _vsaEventType (\s a -> s { _vsaEventType = a })

-- | The ID of the event associated with this operation.
vsaEventId :: Lens' VolumeStatusAction (Maybe Text)
vsaEventId = lens _vsaEventId (\s a -> s { _vsaEventId = a })

instance FromXML VolumeStatusAction where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery VolumeStatusAction where
    toQuery = genericQuery def

-- | Describes a volume status.
data VolumeStatusDetails = VolumeStatusDetails
    { _vsdName :: Maybe VolumeStatusName
    , _vsdStatus :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VolumeStatusDetails' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Name ::@ @Maybe VolumeStatusName@
--
-- * @Status ::@ @Maybe Text@
--
mkVolumeStatusDetails :: VolumeStatusDetails
mkVolumeStatusDetails = VolumeStatusDetails
    { _vsdName = Nothing
    , _vsdStatus = Nothing
    }

-- | The name of the volume status.
vsdName :: Lens' VolumeStatusDetails (Maybe VolumeStatusName)
vsdName = lens _vsdName (\s a -> s { _vsdName = a })

-- | The intended status of the volume status.
vsdStatus :: Lens' VolumeStatusDetails (Maybe Text)
vsdStatus = lens _vsdStatus (\s a -> s { _vsdStatus = a })

instance FromXML VolumeStatusDetails where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery VolumeStatusDetails where
    toQuery = genericQuery def

-- | Describes a volume status event.
data VolumeStatusEvent = VolumeStatusEvent
    { _vseEventType :: Maybe Text
    , _vseDescription :: Maybe Text
    , _vseNotBefore :: Maybe ISO8601
    , _vseNotAfter :: Maybe ISO8601
    , _vseEventId :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VolumeStatusEvent' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @EventType ::@ @Maybe Text@
--
-- * @Description ::@ @Maybe Text@
--
-- * @NotBefore ::@ @Maybe ISO8601@
--
-- * @NotAfter ::@ @Maybe ISO8601@
--
-- * @EventId ::@ @Maybe Text@
--
mkVolumeStatusEvent :: VolumeStatusEvent
mkVolumeStatusEvent = VolumeStatusEvent
    { _vseEventType = Nothing
    , _vseDescription = Nothing
    , _vseNotBefore = Nothing
    , _vseNotAfter = Nothing
    , _vseEventId = Nothing
    }

-- | The type of this event.
vseEventType :: Lens' VolumeStatusEvent (Maybe Text)
vseEventType = lens _vseEventType (\s a -> s { _vseEventType = a })

-- | A description of the event.
vseDescription :: Lens' VolumeStatusEvent (Maybe Text)
vseDescription = lens _vseDescription (\s a -> s { _vseDescription = a })

-- | The earliest start time of the event.
vseNotBefore :: Lens' VolumeStatusEvent (Maybe ISO8601)
vseNotBefore = lens _vseNotBefore (\s a -> s { _vseNotBefore = a })

-- | The latest end time of the event.
vseNotAfter :: Lens' VolumeStatusEvent (Maybe ISO8601)
vseNotAfter = lens _vseNotAfter (\s a -> s { _vseNotAfter = a })

-- | The ID of this event.
vseEventId :: Lens' VolumeStatusEvent (Maybe Text)
vseEventId = lens _vseEventId (\s a -> s { _vseEventId = a })

instance FromXML VolumeStatusEvent where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery VolumeStatusEvent where
    toQuery = genericQuery def

-- | The volume status.
data VolumeStatusInfo = VolumeStatusInfo
    { _vsi1Status :: Maybe VolumeStatusInfoStatus
    , _vsi1Details :: [VolumeStatusDetails]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VolumeStatusInfo' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Status ::@ @Maybe VolumeStatusInfoStatus@
--
-- * @Details ::@ @[VolumeStatusDetails]@
--
mkVolumeStatusInfo :: VolumeStatusInfo
mkVolumeStatusInfo = VolumeStatusInfo
    { _vsi1Status = Nothing
    , _vsi1Details = mempty
    }

-- | The status of the volume.
vsi1Status :: Lens' VolumeStatusInfo (Maybe VolumeStatusInfoStatus)
vsi1Status = lens _vsi1Status (\s a -> s { _vsi1Status = a })

-- | The details of the volume status.
vsi1Details :: Lens' VolumeStatusInfo [VolumeStatusDetails]
vsi1Details = lens _vsi1Details (\s a -> s { _vsi1Details = a })

instance FromXML VolumeStatusInfo where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "volumeStatus"

instance ToQuery VolumeStatusInfo where
    toQuery = genericQuery def

-- | Describes the volume status.
data VolumeStatusItem = VolumeStatusItem
    { _vsiVolumeId :: Maybe Text
    , _vsiAvailabilityZone :: Maybe Text
    , _vsiVolumeStatus :: Maybe VolumeStatusInfo
    , _vsiEvents :: [VolumeStatusEvent]
    , _vsiActions :: [VolumeStatusAction]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VolumeStatusItem' data type.
--
-- 'VolumeStatusItem' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VolumeId ::@ @Maybe Text@
--
-- * @AvailabilityZone ::@ @Maybe Text@
--
-- * @VolumeStatus ::@ @Maybe VolumeStatusInfo@
--
-- * @Events ::@ @[VolumeStatusEvent]@
--
-- * @Actions ::@ @[VolumeStatusAction]@
--
mkVolumeStatusItem :: VolumeStatusItem
mkVolumeStatusItem = VolumeStatusItem
    { _vsiVolumeId = Nothing
    , _vsiAvailabilityZone = Nothing
    , _vsiVolumeStatus = Nothing
    , _vsiEvents = mempty
    , _vsiActions = mempty
    }

-- | The volume ID.
vsiVolumeId :: Lens' VolumeStatusItem (Maybe Text)
vsiVolumeId = lens _vsiVolumeId (\s a -> s { _vsiVolumeId = a })

-- | The Availability Zone of the volume.
vsiAvailabilityZone :: Lens' VolumeStatusItem (Maybe Text)
vsiAvailabilityZone =
    lens _vsiAvailabilityZone (\s a -> s { _vsiAvailabilityZone = a })

-- | The volume status.
vsiVolumeStatus :: Lens' VolumeStatusItem (Maybe VolumeStatusInfo)
vsiVolumeStatus = lens _vsiVolumeStatus (\s a -> s { _vsiVolumeStatus = a })

-- | A list of events associated with the volume.
vsiEvents :: Lens' VolumeStatusItem [VolumeStatusEvent]
vsiEvents = lens _vsiEvents (\s a -> s { _vsiEvents = a })

-- | The details of the operation.
vsiActions :: Lens' VolumeStatusItem [VolumeStatusAction]
vsiActions = lens _vsiActions (\s a -> s { _vsiActions = a })

instance FromXML VolumeStatusItem where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

-- | Information about the VPC.
data Vpc = Vpc
    { _vVpcId :: Maybe Text
    , _vState :: Maybe VpcState
    , _vCidrBlock :: Maybe Text
    , _vDhcpOptionsId :: Maybe Text
    , _vTags :: [Tag]
    , _vInstanceTenancy :: Maybe Tenancy
    , _vIsDefault :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Vpc' data type.
--
-- 'Vpc' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VpcId ::@ @Maybe Text@
--
-- * @State ::@ @Maybe VpcState@
--
-- * @CidrBlock ::@ @Maybe Text@
--
-- * @DhcpOptionsId ::@ @Maybe Text@
--
-- * @Tags ::@ @[Tag]@
--
-- * @InstanceTenancy ::@ @Maybe Tenancy@
--
-- * @IsDefault ::@ @Maybe Bool@
--
mkVpc :: Vpc
mkVpc = Vpc
    { _vVpcId = Nothing
    , _vState = Nothing
    , _vCidrBlock = Nothing
    , _vDhcpOptionsId = Nothing
    , _vTags = mempty
    , _vInstanceTenancy = Nothing
    , _vIsDefault = Nothing
    }

-- | The ID of the VPC.
vVpcId :: Lens' Vpc (Maybe Text)
vVpcId = lens _vVpcId (\s a -> s { _vVpcId = a })

-- | The current state of the VPC.
vState :: Lens' Vpc (Maybe VpcState)
vState = lens _vState (\s a -> s { _vState = a })

-- | The CIDR block for the VPC.
vCidrBlock :: Lens' Vpc (Maybe Text)
vCidrBlock = lens _vCidrBlock (\s a -> s { _vCidrBlock = a })

-- | The ID of the set of DHCP options you've associated with the VPC (or
-- default if the default options are associated with the VPC).
vDhcpOptionsId :: Lens' Vpc (Maybe Text)
vDhcpOptionsId = lens _vDhcpOptionsId (\s a -> s { _vDhcpOptionsId = a })

-- | Any tags assigned to the VPC.
vTags :: Lens' Vpc [Tag]
vTags = lens _vTags (\s a -> s { _vTags = a })

-- | The allowed tenancy of instances launched into the VPC.
vInstanceTenancy :: Lens' Vpc (Maybe Tenancy)
vInstanceTenancy =
    lens _vInstanceTenancy (\s a -> s { _vInstanceTenancy = a })

-- | Indicates whether the VPC is the default VPC.
vIsDefault :: Lens' Vpc (Maybe Bool)
vIsDefault = lens _vIsDefault (\s a -> s { _vIsDefault = a })

instance FromXML Vpc where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "vpc"

-- | Information about the attachment.
data VpcAttachment = VpcAttachment
    { _vaVpcId :: Maybe Text
    , _vaState :: Maybe AttachmentStatus
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VpcAttachment' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VpcId ::@ @Maybe Text@
--
-- * @State ::@ @Maybe AttachmentStatus@
--
mkVpcAttachment :: VpcAttachment
mkVpcAttachment = VpcAttachment
    { _vaVpcId = Nothing
    , _vaState = Nothing
    }

-- | The ID of the VPC.
vaVpcId :: Lens' VpcAttachment (Maybe Text)
vaVpcId = lens _vaVpcId (\s a -> s { _vaVpcId = a })

-- | The current state of the attachment.
vaState :: Lens' VpcAttachment (Maybe AttachmentStatus)
vaState = lens _vaState (\s a -> s { _vaState = a })

instance FromXML VpcAttachment where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "attachment"

instance ToQuery VpcAttachment where
    toQuery = genericQuery def

-- | Information about the VPC peering connection.
data VpcPeeringConnection = VpcPeeringConnection
    { _vpcAccepterVpcInfo :: Maybe VpcPeeringConnectionVpcInfo
    , _vpcExpirationTime :: Maybe ISO8601
    , _vpcRequesterVpcInfo :: Maybe VpcPeeringConnectionVpcInfo
    , _vpcStatus :: Maybe VpcPeeringConnectionStateReason
    , _vpcTags :: [Tag]
    , _vpcVpcPeeringConnectionId :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VpcPeeringConnection' data type.
--
-- 'VpcPeeringConnection' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AccepterVpcInfo ::@ @Maybe VpcPeeringConnectionVpcInfo@
--
-- * @ExpirationTime ::@ @Maybe ISO8601@
--
-- * @RequesterVpcInfo ::@ @Maybe VpcPeeringConnectionVpcInfo@
--
-- * @Status ::@ @Maybe VpcPeeringConnectionStateReason@
--
-- * @Tags ::@ @[Tag]@
--
-- * @VpcPeeringConnectionId ::@ @Maybe Text@
--
mkVpcPeeringConnection :: VpcPeeringConnection
mkVpcPeeringConnection = VpcPeeringConnection
    { _vpcAccepterVpcInfo = Nothing
    , _vpcExpirationTime = Nothing
    , _vpcRequesterVpcInfo = Nothing
    , _vpcStatus = Nothing
    , _vpcTags = mempty
    , _vpcVpcPeeringConnectionId = Nothing
    }

-- | The information of the peer VPC.
vpcAccepterVpcInfo :: Lens' VpcPeeringConnection (Maybe VpcPeeringConnectionVpcInfo)
vpcAccepterVpcInfo =
    lens _vpcAccepterVpcInfo (\s a -> s { _vpcAccepterVpcInfo = a })

-- | The time that an unaccepted VPC peering connection will expire.
vpcExpirationTime :: Lens' VpcPeeringConnection (Maybe ISO8601)
vpcExpirationTime =
    lens _vpcExpirationTime (\s a -> s { _vpcExpirationTime = a })

-- | The information of the requester VPC.
vpcRequesterVpcInfo :: Lens' VpcPeeringConnection (Maybe VpcPeeringConnectionVpcInfo)
vpcRequesterVpcInfo =
    lens _vpcRequesterVpcInfo (\s a -> s { _vpcRequesterVpcInfo = a })

-- | The status of the VPC peering connection.
vpcStatus :: Lens' VpcPeeringConnection (Maybe VpcPeeringConnectionStateReason)
vpcStatus = lens _vpcStatus (\s a -> s { _vpcStatus = a })

-- | Any tags assigned to the resource.
vpcTags :: Lens' VpcPeeringConnection [Tag]
vpcTags = lens _vpcTags (\s a -> s { _vpcTags = a })

-- | The ID of the VPC peering connection.
vpcVpcPeeringConnectionId :: Lens' VpcPeeringConnection (Maybe Text)
vpcVpcPeeringConnectionId =
    lens _vpcVpcPeeringConnectionId
         (\s a -> s { _vpcVpcPeeringConnectionId = a })

instance FromXML VpcPeeringConnection where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "vpcPeeringConnection"

-- | The status of the VPC peering connection.
data VpcPeeringConnectionStateReason = VpcPeeringConnectionStateReason
    { _vpcsrCode :: Maybe Text
    , _vpcsrMessage :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VpcPeeringConnectionStateReason' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Code ::@ @Maybe Text@
--
-- * @Message ::@ @Maybe Text@
--
mkVpcPeeringConnectionStateReason :: VpcPeeringConnectionStateReason
mkVpcPeeringConnectionStateReason = VpcPeeringConnectionStateReason
    { _vpcsrCode = Nothing
    , _vpcsrMessage = Nothing
    }

-- | The status of the VPC peering connection.
vpcsrCode :: Lens' VpcPeeringConnectionStateReason (Maybe Text)
vpcsrCode = lens _vpcsrCode (\s a -> s { _vpcsrCode = a })

-- | A message that provides more information about the status, if applicable.
vpcsrMessage :: Lens' VpcPeeringConnectionStateReason (Maybe Text)
vpcsrMessage = lens _vpcsrMessage (\s a -> s { _vpcsrMessage = a })

instance FromXML VpcPeeringConnectionStateReason where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "status"

instance ToQuery VpcPeeringConnectionStateReason where
    toQuery = genericQuery def

-- | The information of the peer VPC.
data VpcPeeringConnectionVpcInfo = VpcPeeringConnectionVpcInfo
    { _vpcviCidrBlock :: Maybe Text
    , _vpcviOwnerId :: Maybe Text
    , _vpcviVpcId :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VpcPeeringConnectionVpcInfo' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CidrBlock ::@ @Maybe Text@
--
-- * @OwnerId ::@ @Maybe Text@
--
-- * @VpcId ::@ @Maybe Text@
--
mkVpcPeeringConnectionVpcInfo :: VpcPeeringConnectionVpcInfo
mkVpcPeeringConnectionVpcInfo = VpcPeeringConnectionVpcInfo
    { _vpcviCidrBlock = Nothing
    , _vpcviOwnerId = Nothing
    , _vpcviVpcId = Nothing
    }

-- | The CIDR block for the VPC.
vpcviCidrBlock :: Lens' VpcPeeringConnectionVpcInfo (Maybe Text)
vpcviCidrBlock = lens _vpcviCidrBlock (\s a -> s { _vpcviCidrBlock = a })

-- | The AWS account ID of the VPC owner.
vpcviOwnerId :: Lens' VpcPeeringConnectionVpcInfo (Maybe Text)
vpcviOwnerId = lens _vpcviOwnerId (\s a -> s { _vpcviOwnerId = a })

-- | The ID of the VPC.
vpcviVpcId :: Lens' VpcPeeringConnectionVpcInfo (Maybe Text)
vpcviVpcId = lens _vpcviVpcId (\s a -> s { _vpcviVpcId = a })

instance FromXML VpcPeeringConnectionVpcInfo where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "accepterVpcInfo"

instance ToQuery VpcPeeringConnectionVpcInfo where
    toQuery = genericQuery def

-- | Information about the VPN connection.
data VpnConnection = VpnConnection
    { _vcVpnConnectionId :: Maybe Text
    , _vcState :: Maybe VpnState
    , _vcCustomerGatewayConfiguration :: Maybe Text
    , _vcType :: Maybe GatewayType
    , _vcCustomerGatewayId :: Maybe Text
    , _vcVpnGatewayId :: Maybe Text
    , _vcTags :: [Tag]
    , _vcVgwTelemetry :: [VgwTelemetry]
    , _vcOptions :: Maybe VpnConnectionOptions
    , _vcRoutes :: [VpnStaticRoute]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VpnConnection' data type.
--
-- 'VpnConnection' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VpnConnectionId ::@ @Maybe Text@
--
-- * @State ::@ @Maybe VpnState@
--
-- * @CustomerGatewayConfiguration ::@ @Maybe Text@
--
-- * @Type ::@ @Maybe GatewayType@
--
-- * @CustomerGatewayId ::@ @Maybe Text@
--
-- * @VpnGatewayId ::@ @Maybe Text@
--
-- * @Tags ::@ @[Tag]@
--
-- * @VgwTelemetry ::@ @[VgwTelemetry]@
--
-- * @Options ::@ @Maybe VpnConnectionOptions@
--
-- * @Routes ::@ @[VpnStaticRoute]@
--
mkVpnConnection :: VpnConnection
mkVpnConnection = VpnConnection
    { _vcVpnConnectionId = Nothing
    , _vcState = Nothing
    , _vcCustomerGatewayConfiguration = Nothing
    , _vcType = Nothing
    , _vcCustomerGatewayId = Nothing
    , _vcVpnGatewayId = Nothing
    , _vcTags = mempty
    , _vcVgwTelemetry = mempty
    , _vcOptions = Nothing
    , _vcRoutes = mempty
    }

-- | The ID of the VPN connection.
vcVpnConnectionId :: Lens' VpnConnection (Maybe Text)
vcVpnConnectionId =
    lens _vcVpnConnectionId (\s a -> s { _vcVpnConnectionId = a })

-- | The current state of the VPN connection.
vcState :: Lens' VpnConnection (Maybe VpnState)
vcState = lens _vcState (\s a -> s { _vcState = a })

-- | The configuration information for the VPN connection's customer gateway (in
-- the native XML format). This element is always present in the
-- CreateVpnConnection response; however, it's present in the
-- DescribeVpnConnections response only if the VPN connection is in the
-- pending or available state.
vcCustomerGatewayConfiguration :: Lens' VpnConnection (Maybe Text)
vcCustomerGatewayConfiguration =
    lens _vcCustomerGatewayConfiguration
         (\s a -> s { _vcCustomerGatewayConfiguration = a })

-- | The type of VPN connection.
vcType :: Lens' VpnConnection (Maybe GatewayType)
vcType = lens _vcType (\s a -> s { _vcType = a })

-- | The ID of the customer gateway at your end of the VPN connection.
vcCustomerGatewayId :: Lens' VpnConnection (Maybe Text)
vcCustomerGatewayId =
    lens _vcCustomerGatewayId (\s a -> s { _vcCustomerGatewayId = a })

-- | The ID of the virtual private gateway at the AWS side of the VPN
-- connection.
vcVpnGatewayId :: Lens' VpnConnection (Maybe Text)
vcVpnGatewayId = lens _vcVpnGatewayId (\s a -> s { _vcVpnGatewayId = a })

-- | Any tags assigned to the VPN connection.
vcTags :: Lens' VpnConnection [Tag]
vcTags = lens _vcTags (\s a -> s { _vcTags = a })

-- | Information about the VPN tunnel.
vcVgwTelemetry :: Lens' VpnConnection [VgwTelemetry]
vcVgwTelemetry = lens _vcVgwTelemetry (\s a -> s { _vcVgwTelemetry = a })

-- | The VPN connection options.
vcOptions :: Lens' VpnConnection (Maybe VpnConnectionOptions)
vcOptions = lens _vcOptions (\s a -> s { _vcOptions = a })

-- | The static routes associated with the VPN connection.
vcRoutes :: Lens' VpnConnection [VpnStaticRoute]
vcRoutes = lens _vcRoutes (\s a -> s { _vcRoutes = a })

instance FromXML VpnConnection where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "vpnConnection"

-- | Information about the virtual private gateway.
data VpnGateway = VpnGateway
    { _vgVpnGatewayId :: Maybe Text
    , _vgState :: Maybe VpnState
    , _vgType :: Maybe GatewayType
    , _vgAvailabilityZone :: Maybe Text
    , _vgVpcAttachments :: [VpcAttachment]
    , _vgTags :: [Tag]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VpnGateway' data type.
--
-- 'VpnGateway' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VpnGatewayId ::@ @Maybe Text@
--
-- * @State ::@ @Maybe VpnState@
--
-- * @Type ::@ @Maybe GatewayType@
--
-- * @AvailabilityZone ::@ @Maybe Text@
--
-- * @VpcAttachments ::@ @[VpcAttachment]@
--
-- * @Tags ::@ @[Tag]@
--
mkVpnGateway :: VpnGateway
mkVpnGateway = VpnGateway
    { _vgVpnGatewayId = Nothing
    , _vgState = Nothing
    , _vgType = Nothing
    , _vgAvailabilityZone = Nothing
    , _vgVpcAttachments = mempty
    , _vgTags = mempty
    }

-- | The ID of the virtual private gateway.
vgVpnGatewayId :: Lens' VpnGateway (Maybe Text)
vgVpnGatewayId = lens _vgVpnGatewayId (\s a -> s { _vgVpnGatewayId = a })

-- | The current state of the virtual private gateway.
vgState :: Lens' VpnGateway (Maybe VpnState)
vgState = lens _vgState (\s a -> s { _vgState = a })

-- | The type of VPN connection the virtual private gateway supports.
vgType :: Lens' VpnGateway (Maybe GatewayType)
vgType = lens _vgType (\s a -> s { _vgType = a })

-- | The Availability Zone where the virtual private gateway was created.
vgAvailabilityZone :: Lens' VpnGateway (Maybe Text)
vgAvailabilityZone =
    lens _vgAvailabilityZone (\s a -> s { _vgAvailabilityZone = a })

-- | Any VPCs attached to the virtual private gateway.
vgVpcAttachments :: Lens' VpnGateway [VpcAttachment]
vgVpcAttachments =
    lens _vgVpcAttachments (\s a -> s { _vgVpcAttachments = a })

-- | Any tags assigned to the virtual private gateway.
vgTags :: Lens' VpnGateway [Tag]
vgTags = lens _vgTags (\s a -> s { _vgTags = a })

instance FromXML VpnGateway where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "vpnGateway"

-- | Describes a static route for a VPN connection.
data VpnStaticRoute = VpnStaticRoute
    { _vsrDestinationCidrBlock :: Maybe Text
    , _vsrSource :: Maybe VpnStaticRouteSource
    , _vsrState :: Maybe VpnState
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VpnStaticRoute' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DestinationCidrBlock ::@ @Maybe Text@
--
-- * @Source ::@ @Maybe VpnStaticRouteSource@
--
-- * @State ::@ @Maybe VpnState@
--
mkVpnStaticRoute :: VpnStaticRoute
mkVpnStaticRoute = VpnStaticRoute
    { _vsrDestinationCidrBlock = Nothing
    , _vsrSource = Nothing
    , _vsrState = Nothing
    }

-- | The CIDR block associated with the local subnet of the customer data
-- center.
vsrDestinationCidrBlock :: Lens' VpnStaticRoute (Maybe Text)
vsrDestinationCidrBlock =
    lens _vsrDestinationCidrBlock
         (\s a -> s { _vsrDestinationCidrBlock = a })

-- | Indicates how the routes were provided.
vsrSource :: Lens' VpnStaticRoute (Maybe VpnStaticRouteSource)
vsrSource = lens _vsrSource (\s a -> s { _vsrSource = a })

-- | The current state of the static route.
vsrState :: Lens' VpnStaticRoute (Maybe VpnState)
vsrState = lens _vsrState (\s a -> s { _vsrState = a })

instance FromXML VpnStaticRoute where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "item"

instance ToQuery VpnStaticRoute where
    toQuery = genericQuery def
