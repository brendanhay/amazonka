{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Network.AWS.EC2.Types
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Network.AWS.EC2.Types
    (
    -- * Service
      EC2

    -- * Errors

    -- * AccountAttributeName
    , AccountAttributeName (..)

    -- * AddressStatus
    , AddressStatus (..)

    -- * ArchitectureValues
    , ArchitectureValues (..)

    -- * AttachmentStatus
    , AttachmentStatus (..)

    -- * AvailabilityZoneState
    , AvailabilityZoneState (..)

    -- * BatchState
    , BatchState (..)

    -- * BundleTaskState
    , BundleTaskState (..)

    -- * CancelBatchErrorCode
    , CancelBatchErrorCode (..)

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

    -- * EventType
    , EventType (..)

    -- * ExportEnvironment
    , ExportEnvironment (..)

    -- * ExportTaskState
    , ExportTaskState (..)

    -- * FlowLogsResourceType
    , FlowLogsResourceType (..)

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

    -- * MoveStatus
    , MoveStatus (..)

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

    -- * State
    , State (..)

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

    -- * TrafficType
    , TrafficType (..)

    -- * VPCAttributeName
    , VPCAttributeName (..)

    -- * VPCState
    , VPCState (..)

    -- * VPNState
    , VPNState (..)

    -- * VPNStaticRouteSource
    , VPNStaticRouteSource (..)

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

    -- * AccountAttribute
    , AccountAttribute
    , accountAttribute
    , aaAttributeValues
    , aaAttributeName

    -- * AccountAttributeValue
    , AccountAttributeValue
    , accountAttributeValue
    , aavAttributeValue

    -- * ActiveInstance
    , ActiveInstance
    , activeInstance
    , aiInstanceId
    , aiInstanceType
    , aiSpotInstanceRequestId

    -- * Address
    , Address
    , address
    , addInstanceId
    , addAssociationId
    , addNetworkInterfaceOwnerId
    , addAllocationId
    , addDomain
    , addNetworkInterfaceId
    , addPrivateIPAddress
    , addPublicIP

    -- * AttributeBooleanValue
    , AttributeBooleanValue
    , attributeBooleanValue
    , abvValue

    -- * AttributeValue
    , AttributeValue
    , attributeValue
    , avValue

    -- * AvailabilityZone
    , AvailabilityZone
    , availabilityZone
    , azRegionName
    , azState
    , azZoneName
    , azMessages

    -- * AvailabilityZoneMessage
    , AvailabilityZoneMessage
    , availabilityZoneMessage
    , azmMessage

    -- * BlobAttributeValue
    , BlobAttributeValue
    , blobAttributeValue
    , bavValue

    -- * BlockDeviceMapping
    , BlockDeviceMapping
    , blockDeviceMapping
    , bdmVirtualName
    , bdmNoDevice
    , bdmEBS
    , bdmDeviceName

    -- * BundleTask
    , BundleTask
    , bundleTask
    , btBundleTaskError
    , btBundleId
    , btInstanceId
    , btProgress
    , btStartTime
    , btState
    , btStorage
    , btUpdateTime

    -- * BundleTaskError
    , BundleTaskError
    , bundleTaskError
    , bteCode
    , bteMessage

    -- * CancelSpotFleetRequestsError
    , CancelSpotFleetRequestsError
    , cancelSpotFleetRequestsError
    , csfreCode
    , csfreMessage

    -- * CancelSpotFleetRequestsErrorItem
    , CancelSpotFleetRequestsErrorItem
    , cancelSpotFleetRequestsErrorItem
    , csfreiSpotFleetRequestId
    , csfreiError

    -- * CancelSpotFleetRequestsSuccessItem
    , CancelSpotFleetRequestsSuccessItem
    , cancelSpotFleetRequestsSuccessItem
    , csfrsiSpotFleetRequestId
    , csfrsiCurrentSpotFleetRequestState
    , csfrsiPreviousSpotFleetRequestState

    -- * CancelledSpotInstanceRequest
    , CancelledSpotInstanceRequest
    , cancelledSpotInstanceRequest
    , csirState
    , csirSpotInstanceRequestId

    -- * ClassicLinkInstance
    , ClassicLinkInstance
    , classicLinkInstance
    , cliInstanceId
    , cliGroups
    , cliVPCId
    , cliTags

    -- * ClientData
    , ClientData
    , clientData
    , cdUploadStart
    , cdUploadSize
    , cdUploadEnd
    , cdComment

    -- * ConversionTask
    , ConversionTask
    , conversionTask
    , ctImportInstance
    , ctStatusMessage
    , ctImportVolume
    , ctExpirationTime
    , ctTags
    , ctConversionTaskId
    , ctState

    -- * CreateVolumePermission
    , CreateVolumePermission
    , createVolumePermission
    , cvpGroup
    , cvpUserId

    -- * CreateVolumePermissionModifications
    , CreateVolumePermissionModifications
    , createVolumePermissionModifications
    , cvpmRemove
    , cvpmAdd

    -- * CustomerGateway
    , CustomerGateway
    , customerGateway
    , cgTags
    , cgBGPASN
    , cgCustomerGatewayId
    , cgIPAddress
    , cgState
    , cgType

    -- * DHCPConfiguration
    , DHCPConfiguration
    , dhcpConfiguration
    , dcValues
    , dcKey

    -- * DHCPOptions
    , DHCPOptions
    , dhcpOptions
    , doDHCPConfigurations
    , doDHCPOptionsId
    , doTags

    -- * DiskImage
    , DiskImage
    , diskImage
    , diImage
    , diVolume
    , diDescription

    -- * DiskImageDescription
    , DiskImageDescription
    , diskImageDescription
    , disChecksum
    , disFormat
    , disSize
    , disImportManifestURL

    -- * DiskImageDetail
    , DiskImageDetail
    , diskImageDetail
    , didFormat
    , didBytes
    , didImportManifestURL

    -- * DiskImageVolumeDescription
    , DiskImageVolumeDescription
    , diskImageVolumeDescription
    , divdSize
    , divdId

    -- * EBSBlockDevice
    , EBSBlockDevice
    , ebsBlockDevice
    , ebdDeleteOnTermination
    , ebdVolumeSize
    , ebdIOPS
    , ebdEncrypted
    , ebdVolumeType
    , ebdSnapshotId

    -- * EBSInstanceBlockDevice
    , EBSInstanceBlockDevice
    , ebsInstanceBlockDevice
    , eibdDeleteOnTermination
    , eibdStatus
    , eibdVolumeId
    , eibdAttachTime

    -- * EBSInstanceBlockDeviceSpecification
    , EBSInstanceBlockDeviceSpecification
    , ebsInstanceBlockDeviceSpecification
    , eibdsDeleteOnTermination
    , eibdsVolumeId

    -- * EventInformation
    , EventInformation
    , eventInformation
    , eiInstanceId
    , eiEventDescription
    , eiEventSubType

    -- * ExportTask
    , ExportTask
    , exportTask
    , etDescription
    , etExportTaskId
    , etExportToS3Task
    , etInstanceExportDetails
    , etState
    , etStatusMessage

    -- * ExportToS3Task
    , ExportToS3Task
    , exportToS3Task
    , etstS3Key
    , etstContainerFormat
    , etstS3Bucket
    , etstDiskImageFormat

    -- * ExportToS3TaskSpecification
    , ExportToS3TaskSpecification
    , exportToS3TaskSpecification
    , etstsContainerFormat
    , etstsS3Prefix
    , etstsS3Bucket
    , etstsDiskImageFormat

    -- * Filter
    , Filter
    , filter'
    , filValues
    , filName

    -- * FlowLog
    , FlowLog
    , flowLog
    , flCreationTime
    , flResourceId
    , flFlowLogStatus
    , flTrafficType
    , flDeliverLogsStatus
    , flDeliverLogsErrorMessage
    , flDeliverLogsPermissionARN
    , flLogGroupName
    , flFlowLogId

    -- * GroupIdentifier
    , GroupIdentifier
    , groupIdentifier
    , giGroupId
    , giGroupName

    -- * HistoryRecord
    , HistoryRecord
    , historyRecord
    , hrTimestamp
    , hrEventType
    , hrEventInformation

    -- * IAMInstanceProfile
    , IAMInstanceProfile
    , iamInstanceProfile
    , iapARN
    , iapId

    -- * IAMInstanceProfileSpecification
    , IAMInstanceProfileSpecification
    , iamInstanceProfileSpecification
    , iapsARN
    , iapsName

    -- * ICMPTypeCode
    , ICMPTypeCode
    , icmpTypeCode
    , itcCode
    , itcType

    -- * IPPermission
    , IPPermission
    , ipPermission
    , ipFromPort
    , ipUserIdGroupPairs
    , ipPrefixListIds
    , ipToPort
    , ipIPRanges
    , ipIPProtocol

    -- * IPRange
    , IPRange
    , ipRange
    , irCIDRIP

    -- * Image
    , Image
    , image
    , imaPlatform
    , imaImageOwnerAlias
    , imaRAMDiskId
    , imaKernelId
    , imaRootDeviceName
    , imaSRIOVNetSupport
    , imaName
    , imaCreationDate
    , imaProductCodes
    , imaStateReason
    , imaBlockDeviceMappings
    , imaDescription
    , imaTags
    , imaImageId
    , imaImageLocation
    , imaState
    , imaOwnerId
    , imaPublic
    , imaArchitecture
    , imaImageType
    , imaRootDeviceType
    , imaVirtualizationType
    , imaHypervisor

    -- * ImageDiskContainer
    , ImageDiskContainer
    , imageDiskContainer
    , idcURL
    , idcFormat
    , idcDeviceName
    , idcUserBucket
    , idcDescription
    , idcSnapshotId

    -- * ImportImageTask
    , ImportImageTask
    , importImageTask
    , iitStatus
    , iitHypervisor
    , iitPlatform
    , iitProgress
    , iitLicenseType
    , iitSnapshotDetails
    , iitStatusMessage
    , iitImageId
    , iitImportTaskId
    , iitArchitecture
    , iitDescription

    -- * ImportInstanceLaunchSpecification
    , ImportInstanceLaunchSpecification
    , importInstanceLaunchSpecification
    , iilsAdditionalInfo
    , iilsGroupNames
    , iilsSubnetId
    , iilsGroupIds
    , iilsInstanceType
    , iilsUserData
    , iilsMonitoring
    , iilsInstanceInitiatedShutdownBehavior
    , iilsPrivateIPAddress
    , iilsArchitecture
    , iilsPlacement

    -- * ImportInstanceTaskDetails
    , ImportInstanceTaskDetails
    , importInstanceTaskDetails
    , iitdInstanceId
    , iitdPlatform
    , iitdDescription
    , iitdVolumes

    -- * ImportInstanceVolumeDetailItem
    , ImportInstanceVolumeDetailItem
    , importInstanceVolumeDetailItem
    , iivdiStatusMessage
    , iivdiDescription
    , iivdiBytesConverted
    , iivdiAvailabilityZone
    , iivdiImage
    , iivdiVolume
    , iivdiStatus

    -- * ImportSnapshotTask
    , ImportSnapshotTask
    , importSnapshotTask
    , istSnapshotTaskDetail
    , istImportTaskId
    , istDescription

    -- * ImportVolumeTaskDetails
    , ImportVolumeTaskDetails
    , importVolumeTaskDetails
    , ivtdDescription
    , ivtdBytesConverted
    , ivtdAvailabilityZone
    , ivtdImage
    , ivtdVolume

    -- * Instance
    , Instance
    , instance'
    , insPublicDNSName
    , insPlatform
    , insSecurityGroups
    , insClientToken
    , insSourceDestCheck
    , insVPCId
    , insNetworkInterfaces
    , insKeyName
    , insRAMDiskId
    , insKernelId
    , insSubnetId
    , insRootDeviceName
    , insSRIOVNetSupport
    , insStateTransitionReason
    , insIAMInstanceProfile
    , insInstanceLifecycle
    , insPrivateIPAddress
    , insProductCodes
    , insSpotInstanceRequestId
    , insPrivateDNSName
    , insStateReason
    , insBlockDeviceMappings
    , insPublicIPAddress
    , insTags
    , insInstanceId
    , insImageId
    , insAMILaunchIndex
    , insInstanceType
    , insLaunchTime
    , insPlacement
    , insMonitoring
    , insArchitecture
    , insRootDeviceType
    , insVirtualizationType
    , insHypervisor
    , insEBSOptimized
    , insState

    -- * InstanceBlockDeviceMapping
    , InstanceBlockDeviceMapping
    , instanceBlockDeviceMapping
    , ibdmEBS
    , ibdmDeviceName

    -- * InstanceBlockDeviceMappingSpecification
    , InstanceBlockDeviceMappingSpecification
    , instanceBlockDeviceMappingSpecification
    , ibdmsVirtualName
    , ibdmsNoDevice
    , ibdmsEBS
    , ibdmsDeviceName

    -- * InstanceCount
    , InstanceCount
    , instanceCount
    , icState
    , icInstanceCount

    -- * InstanceExportDetails
    , InstanceExportDetails
    , instanceExportDetails
    , iedInstanceId
    , iedTargetEnvironment

    -- * InstanceMonitoring
    , InstanceMonitoring
    , instanceMonitoring
    , imInstanceId
    , imMonitoring

    -- * InstanceNetworkInterface
    , InstanceNetworkInterface
    , instanceNetworkInterface
    , iniPrivateIPAddresses
    , iniStatus
    , iniGroups
    , iniSourceDestCheck
    , iniVPCId
    , iniNetworkInterfaceId
    , iniSubnetId
    , iniAttachment
    , iniMACAddress
    , iniOwnerId
    , iniPrivateIPAddress
    , iniPrivateDNSName
    , iniDescription
    , iniAssociation

    -- * InstanceNetworkInterfaceAssociation
    , InstanceNetworkInterfaceAssociation
    , instanceNetworkInterfaceAssociation
    , iniaPublicDNSName
    , iniaIPOwnerId
    , iniaPublicIP

    -- * InstanceNetworkInterfaceAttachment
    , InstanceNetworkInterfaceAttachment
    , instanceNetworkInterfaceAttachment
    , iniaDeleteOnTermination
    , iniaStatus
    , iniaAttachmentId
    , iniaAttachTime
    , iniaDeviceIndex

    -- * InstanceNetworkInterfaceSpecification
    , InstanceNetworkInterfaceSpecification
    , instanceNetworkInterfaceSpecification
    , inisPrivateIPAddresses
    , inisDeleteOnTermination
    , inisGroups
    , inisAssociatePublicIPAddress
    , inisNetworkInterfaceId
    , inisSubnetId
    , inisPrivateIPAddress
    , inisSecondaryPrivateIPAddressCount
    , inisDeviceIndex
    , inisDescription

    -- * InstancePrivateIPAddress
    , InstancePrivateIPAddress
    , instancePrivateIPAddress
    , ipiaPrimary
    , ipiaPrivateIPAddress
    , ipiaPrivateDNSName
    , ipiaAssociation

    -- * InstanceState
    , InstanceState
    , instanceState
    , isName
    , isCode

    -- * InstanceStateChange
    , InstanceStateChange
    , instanceStateChange
    , iscInstanceId
    , iscCurrentState
    , iscPreviousState

    -- * InstanceStatus
    , InstanceStatus
    , instanceStatus
    , isInstanceId
    , isSystemStatus
    , isAvailabilityZone
    , isEvents
    , isInstanceStatus
    , isInstanceState

    -- * InstanceStatusDetails
    , InstanceStatusDetails
    , instanceStatusDetails
    , isdStatus
    , isdImpairedSince
    , isdName

    -- * InstanceStatusEvent
    , InstanceStatusEvent
    , instanceStatusEvent
    , iseNotBefore
    , iseCode
    , iseDescription
    , iseNotAfter

    -- * InstanceStatusSummary
    , InstanceStatusSummary
    , instanceStatusSummary
    , issDetails
    , issStatus

    -- * InternetGateway
    , InternetGateway
    , internetGateway
    , igAttachments
    , igTags
    , igInternetGatewayId

    -- * InternetGatewayAttachment
    , InternetGatewayAttachment
    , internetGatewayAttachment
    , igaState
    , igaVPCId

    -- * KeyPairInfo
    , KeyPairInfo
    , keyPairInfo
    , kpiKeyFingerprint
    , kpiKeyName

    -- * LaunchPermission
    , LaunchPermission
    , launchPermission
    , lpGroup
    , lpUserId

    -- * LaunchPermissionModifications
    , LaunchPermissionModifications
    , launchPermissionModifications
    , lpmRemove
    , lpmAdd

    -- * LaunchSpecification
    , LaunchSpecification
    , launchSpecification
    , lsSecurityGroups
    , lsNetworkInterfaces
    , lsKeyName
    , lsRAMDiskId
    , lsKernelId
    , lsSubnetId
    , lsInstanceType
    , lsEBSOptimized
    , lsUserData
    , lsMonitoring
    , lsIAMInstanceProfile
    , lsImageId
    , lsBlockDeviceMappings
    , lsAddressingType
    , lsPlacement

    -- * Monitoring
    , Monitoring
    , monitoring
    , monState

    -- * MovingAddressStatus
    , MovingAddressStatus
    , movingAddressStatus
    , masMoveStatus
    , masPublicIP

    -- * NetworkACL
    , NetworkACL
    , networkACL
    , naEntries
    , naNetworkACLId
    , naVPCId
    , naAssociations
    , naIsDefault
    , naTags

    -- * NetworkACLAssociation
    , NetworkACLAssociation
    , networkACLAssociation
    , naaNetworkACLId
    , naaSubnetId
    , naaNetworkACLAssociationId

    -- * NetworkACLEntry
    , NetworkACLEntry
    , networkACLEntry
    , naeICMPTypeCode
    , naeRuleNumber
    , naeRuleAction
    , naeProtocol
    , naePortRange
    , naeCIDRBlock
    , naeEgress

    -- * NetworkInterface
    , NetworkInterface
    , networkInterface
    , niPrivateIPAddresses
    , niStatus
    , niGroups
    , niSourceDestCheck
    , niTagSet
    , niVPCId
    , niRequesterManaged
    , niNetworkInterfaceId
    , niSubnetId
    , niAttachment
    , niMACAddress
    , niOwnerId
    , niAvailabilityZone
    , niPrivateIPAddress
    , niPrivateDNSName
    , niRequesterId
    , niDescription
    , niAssociation

    -- * NetworkInterfaceAssociation
    , NetworkInterfaceAssociation
    , networkInterfaceAssociation
    , niaAssociationId
    , niaPublicDNSName
    , niaAllocationId
    , niaIPOwnerId
    , niaPublicIP

    -- * NetworkInterfaceAttachment
    , NetworkInterfaceAttachment
    , networkInterfaceAttachment
    , niaInstanceId
    , niaDeleteOnTermination
    , niaStatus
    , niaAttachmentId
    , niaInstanceOwnerId
    , niaAttachTime
    , niaDeviceIndex

    -- * NetworkInterfaceAttachmentChanges
    , NetworkInterfaceAttachmentChanges
    , networkInterfaceAttachmentChanges
    , niacDeleteOnTermination
    , niacAttachmentId

    -- * NetworkInterfacePrivateIPAddress
    , NetworkInterfacePrivateIPAddress
    , networkInterfacePrivateIPAddress
    , nipiaPrimary
    , nipiaPrivateIPAddress
    , nipiaPrivateDNSName
    , nipiaAssociation

    -- * NewDHCPConfiguration
    , NewDHCPConfiguration
    , newDHCPConfiguration
    , ndcValues
    , ndcKey

    -- * Placement
    , Placement
    , placement
    , plaAvailabilityZone
    , plaTenancy
    , plaGroupName

    -- * PlacementGroup
    , PlacementGroup
    , placementGroup
    , pgState
    , pgStrategy
    , pgGroupName

    -- * PortRange
    , PortRange
    , portRange
    , prTo
    , prFrom

    -- * PrefixList
    , PrefixList
    , prefixList
    , plCIDRs
    , plPrefixListId
    , plPrefixListName

    -- * PrefixListId
    , PrefixListId
    , prefixListId
    , pliPrefixListId

    -- * PriceSchedule
    , PriceSchedule
    , priceSchedule
    , psCurrencyCode
    , psTerm
    , psActive
    , psPrice

    -- * PriceScheduleSpecification
    , PriceScheduleSpecification
    , priceScheduleSpecification
    , pssCurrencyCode
    , pssTerm
    , pssPrice

    -- * PricingDetail
    , PricingDetail
    , pricingDetail
    , pdCount
    , pdPrice

    -- * PrivateIPAddressSpecification
    , PrivateIPAddressSpecification
    , privateIPAddressSpecification
    , piasPrimary
    , piasPrivateIPAddress

    -- * ProductCode
    , ProductCode
    , productCode
    , pcProductCodeType
    , pcProductCodeId

    -- * PropagatingVGW
    , PropagatingVGW
    , propagatingVGW
    , pvGatewayId

    -- * RecurringCharge
    , RecurringCharge
    , recurringCharge
    , rcFrequency
    , rcAmount

    -- * RegionInfo
    , RegionInfo
    , regionInfo
    , riRegionName
    , riEndpoint

    -- * RequestSpotLaunchSpecification
    , RequestSpotLaunchSpecification
    , requestSpotLaunchSpecification
    , rslsSecurityGroupIds
    , rslsSecurityGroups
    , rslsNetworkInterfaces
    , rslsKeyName
    , rslsRAMDiskId
    , rslsKernelId
    , rslsSubnetId
    , rslsInstanceType
    , rslsEBSOptimized
    , rslsUserData
    , rslsMonitoring
    , rslsIAMInstanceProfile
    , rslsImageId
    , rslsBlockDeviceMappings
    , rslsAddressingType
    , rslsPlacement

    -- * Reservation
    , Reservation
    , reservation
    , resGroups
    , resInstances
    , resRequesterId
    , resReservationId
    , resOwnerId

    -- * ReservedInstanceLimitPrice
    , ReservedInstanceLimitPrice
    , reservedInstanceLimitPrice
    , rilpAmount
    , rilpCurrencyCode

    -- * ReservedInstances
    , ReservedInstances
    , reservedInstances
    , riState
    , riCurrencyCode
    , riInstanceCount
    , riProductDescription
    , riStart
    , riInstanceType
    , riAvailabilityZone
    , riEnd
    , riOfferingType
    , riUsagePrice
    , riRecurringCharges
    , riInstanceTenancy
    , riFixedPrice
    , riReservedInstancesId
    , riDuration
    , riTags

    -- * ReservedInstancesConfiguration
    , ReservedInstancesConfiguration
    , reservedInstancesConfiguration
    , ricPlatform
    , ricInstanceCount
    , ricInstanceType
    , ricAvailabilityZone

    -- * ReservedInstancesId
    , ReservedInstancesId
    , reservedInstancesId
    , riiReservedInstancesId

    -- * ReservedInstancesListing
    , ReservedInstancesListing
    , reservedInstancesListing
    , rilStatus
    , rilClientToken
    , rilUpdateDate
    , rilCreateDate
    , rilPriceSchedules
    , rilStatusMessage
    , rilReservedInstancesId
    , rilInstanceCounts
    , rilReservedInstancesListingId
    , rilTags

    -- * ReservedInstancesModification
    , ReservedInstancesModification
    , reservedInstancesModification
    , rimModificationResults
    , rimStatus
    , rimClientToken
    , rimUpdateDate
    , rimCreateDate
    , rimEffectiveDate
    , rimStatusMessage
    , rimReservedInstancesModificationId
    , rimReservedInstancesIds

    -- * ReservedInstancesModificationResult
    , ReservedInstancesModificationResult
    , reservedInstancesModificationResult
    , rimrReservedInstancesId
    , rimrTargetConfiguration

    -- * ReservedInstancesOffering
    , ReservedInstancesOffering
    , reservedInstancesOffering
    , rioMarketplace
    , rioCurrencyCode
    , rioProductDescription
    , rioInstanceType
    , rioAvailabilityZone
    , rioPricingDetails
    , rioOfferingType
    , rioUsagePrice
    , rioRecurringCharges
    , rioReservedInstancesOfferingId
    , rioInstanceTenancy
    , rioFixedPrice
    , rioDuration

    -- * Route
    , Route
    , route
    , rouInstanceId
    , rouOrigin
    , rouVPCPeeringConnectionId
    , rouState
    , rouNetworkInterfaceId
    , rouGatewayId
    , rouInstanceOwnerId
    , rouDestinationPrefixListId
    , rouDestinationCIDRBlock

    -- * RouteTable
    , RouteTable
    , routeTable
    , rtRoutes
    , rtRouteTableId
    , rtVPCId
    , rtPropagatingVGWs
    , rtAssociations
    , rtTags

    -- * RouteTableAssociation
    , RouteTableAssociation
    , routeTableAssociation
    , rtaRouteTableId
    , rtaRouteTableAssociationId
    , rtaMain
    , rtaSubnetId

    -- * RunInstancesMonitoringEnabled
    , RunInstancesMonitoringEnabled
    , runInstancesMonitoringEnabled
    , rimeEnabled

    -- * S3Storage
    , S3Storage
    , s3Storage
    , ssPrefix
    , ssUploadPolicy
    , ssBucket
    , ssUploadPolicySignature
    , ssAWSAccessKeyId

    -- * SecurityGroup
    , SecurityGroup
    , securityGroup
    , sgVPCId
    , sgIPPermissions
    , sgIPPermissionsEgress
    , sgTags
    , sgOwnerId
    , sgGroupId
    , sgGroupName
    , sgDescription

    -- * Snapshot
    , Snapshot
    , snapshot
    , snaOwnerAlias
    , snaKMSKeyId
    , snaTags
    , snaSnapshotId
    , snaOwnerId
    , snaVolumeId
    , snaVolumeSize
    , snaDescription
    , snaStartTime
    , snaProgress
    , snaState
    , snaEncrypted

    -- * SnapshotDetail
    , SnapshotDetail
    , snapshotDetail
    , sdStatus
    , sdProgress
    , sdURL
    , sdFormat
    , sdDeviceName
    , sdUserBucket
    , sdDiskImageSize
    , sdStatusMessage
    , sdDescription
    , sdSnapshotId

    -- * SnapshotDiskContainer
    , SnapshotDiskContainer
    , snapshotDiskContainer
    , sdcURL
    , sdcFormat
    , sdcUserBucket
    , sdcDescription

    -- * SnapshotTaskDetail
    , SnapshotTaskDetail
    , snapshotTaskDetail
    , stdStatus
    , stdProgress
    , stdURL
    , stdFormat
    , stdUserBucket
    , stdDiskImageSize
    , stdStatusMessage
    , stdDescription
    , stdSnapshotId

    -- * SpotDatafeedSubscription
    , SpotDatafeedSubscription
    , spotDatafeedSubscription
    , sdsState
    , sdsPrefix
    , sdsBucket
    , sdsOwnerId
    , sdsFault

    -- * SpotFleetRequestConfig
    , SpotFleetRequestConfig
    , spotFleetRequestConfig
    , sfrcSpotFleetRequestId
    , sfrcSpotFleetRequestState
    , sfrcSpotFleetRequestConfig

    -- * SpotFleetRequestConfigData
    , SpotFleetRequestConfigData
    , spotFleetRequestConfigData
    , sfrcdClientToken
    , sfrcdValidUntil
    , sfrcdTerminateInstancesWithExpiration
    , sfrcdValidFrom
    , sfrcdSpotPrice
    , sfrcdTargetCapacity
    , sfrcdIAMFleetRole
    , sfrcdLaunchSpecifications

    -- * SpotInstanceRequest
    , SpotInstanceRequest
    , spotInstanceRequest
    , sirInstanceId
    , sirStatus
    , sirState
    , sirProductDescription
    , sirSpotPrice
    , sirAvailabilityZoneGroup
    , sirLaunchSpecification
    , sirLaunchedAvailabilityZone
    , sirValidUntil
    , sirFault
    , sirLaunchGroup
    , sirSpotInstanceRequestId
    , sirType
    , sirValidFrom
    , sirTags
    , sirCreateTime

    -- * SpotInstanceStateFault
    , SpotInstanceStateFault
    , spotInstanceStateFault
    , sisfCode
    , sisfMessage

    -- * SpotInstanceStatus
    , SpotInstanceStatus
    , spotInstanceStatus
    , sisUpdateTime
    , sisCode
    , sisMessage

    -- * SpotPlacement
    , SpotPlacement
    , spotPlacement
    , spAvailabilityZone
    , spGroupName

    -- * SpotPrice
    , SpotPrice
    , spotPrice
    , spoProductDescription
    , spoSpotPrice
    , spoInstanceType
    , spoAvailabilityZone
    , spoTimestamp

    -- * StateReason
    , StateReason
    , stateReason
    , srCode
    , srMessage

    -- * Storage
    , Storage
    , storage
    , stoS3

    -- * Subnet
    , Subnet
    , subnet
    , subTags
    , subAvailabilityZone
    , subAvailableIPAddressCount
    , subCIDRBlock
    , subDefaultForAz
    , subMapPublicIPOnLaunch
    , subState
    , subSubnetId
    , subVPCId

    -- * Tag
    , Tag
    , tag
    , tagKey
    , tagValue

    -- * TagDescription
    , TagDescription
    , tagDescription
    , tdResourceId
    , tdResourceType
    , tdKey
    , tdValue

    -- * UnsuccessfulItem
    , UnsuccessfulItem
    , unsuccessfulItem
    , uiResourceId
    , uiError

    -- * UnsuccessfulItemError
    , UnsuccessfulItemError
    , unsuccessfulItemError
    , uieCode
    , uieMessage

    -- * UserBucket
    , UserBucket
    , userBucket
    , ubS3Key
    , ubS3Bucket

    -- * UserBucketDetails
    , UserBucketDetails
    , userBucketDetails
    , ubdS3Key
    , ubdS3Bucket

    -- * UserData
    , UserData
    , userData
    , udData

    -- * UserIdGroupPair
    , UserIdGroupPair
    , userIdGroupPair
    , uigpUserId
    , uigpGroupId
    , uigpGroupName

    -- * VGWTelemetry
    , VGWTelemetry
    , vgwTelemetry
    , vtStatus
    , vtOutsideIPAddress
    , vtLastStatusChange
    , vtAcceptedRouteCount
    , vtStatusMessage

    -- * VPC
    , VPC
    , vpc
    , vpcTags
    , vpcCIDRBlock
    , vpcDHCPOptionsId
    , vpcInstanceTenancy
    , vpcIsDefault
    , vpcState
    , vpcVPCId

    -- * VPCAttachment
    , VPCAttachment
    , vpcAttachment
    , vaState
    , vaVPCId

    -- * VPCClassicLink
    , VPCClassicLink
    , vpcClassicLink
    , vclVPCId
    , vclTags
    , vclClassicLinkEnabled

    -- * VPCEndpoint
    , VPCEndpoint
    , vpcEndpoint
    , vePolicyDocument
    , veState
    , veVPCId
    , veCreationTimestamp
    , veServiceName
    , veVPCEndpointId
    , veRouteTableIds

    -- * VPCPeeringConnection
    , VPCPeeringConnection
    , vpcPeeringConnection
    , vStatus
    , vVPCPeeringConnectionId
    , vAccepterVPCInfo
    , vRequesterVPCInfo
    , vExpirationTime
    , vTags

    -- * VPCPeeringConnectionStateReason
    , VPCPeeringConnectionStateReason
    , vpcPeeringConnectionStateReason
    , vpcsrCode
    , vpcsrMessage

    -- * VPCPeeringConnectionVPCInfo
    , VPCPeeringConnectionVPCInfo
    , vpcPeeringConnectionVPCInfo
    , vpcviVPCId
    , vpcviOwnerId
    , vpcviCIDRBlock

    -- * VPNConnection
    , VPNConnection
    , vpnConnection
    , vcRoutes
    , vcVPNGatewayId
    , vcOptions
    , vcVGWTelemetry
    , vcTags
    , vcVPNConnectionId
    , vcCustomerGatewayId
    , vcCustomerGatewayConfiguration
    , vcState
    , vcType

    -- * VPNConnectionOptions
    , VPNConnectionOptions
    , vpnConnectionOptions
    , vcoStaticRoutesOnly

    -- * VPNConnectionOptionsSpecification
    , VPNConnectionOptionsSpecification
    , vpnConnectionOptionsSpecification
    , vcosStaticRoutesOnly

    -- * VPNGateway
    , VPNGateway
    , vpnGateway
    , vgVPCAttachments
    , vgState
    , vgVPNGatewayId
    , vgAvailabilityZone
    , vgType
    , vgTags

    -- * VPNStaticRoute
    , VPNStaticRoute
    , vpnStaticRoute
    , vsrState
    , vsrSource
    , vsrDestinationCIDRBlock

    -- * Volume
    , Volume
    , volume
    , volAttachments
    , volIOPS
    , volKMSKeyId
    , volTags
    , volAvailabilityZone
    , volCreateTime
    , volEncrypted
    , volSize
    , volSnapshotId
    , volState
    , volVolumeId
    , volVolumeType

    -- * VolumeAttachment
    , VolumeAttachment
    , volumeAttachment
    , vInstanceId
    , vDeleteOnTermination
    , vState
    , vDevice
    , vVolumeId
    , vAttachTime

    -- * VolumeDetail
    , VolumeDetail
    , volumeDetail
    , vdSize

    -- * VolumeStatusAction
    , VolumeStatusAction
    , volumeStatusAction
    , vsaEventType
    , vsaCode
    , vsaDescription
    , vsaEventId

    -- * VolumeStatusDetails
    , VolumeStatusDetails
    , volumeStatusDetails
    , vsdStatus
    , vsdName

    -- * VolumeStatusEvent
    , VolumeStatusEvent
    , volumeStatusEvent
    , vseNotBefore
    , vseEventType
    , vseDescription
    , vseNotAfter
    , vseEventId

    -- * VolumeStatusInfo
    , VolumeStatusInfo
    , volumeStatusInfo
    , vsiStatus
    , vsiDetails

    -- * VolumeStatusItem
    , VolumeStatusItem
    , volumeStatusItem
    , vsiVolumeStatus
    , vsiActions
    , vsiAvailabilityZone
    , vsiEvents
    , vsiVolumeId

    , module Network.AWS.EC2.Internal
    ) where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | Version @2015-04-15@ of the Amazon Elastic Compute Cloud SDK.
data EC2

instance AWSService EC2 where
    type Sg EC2 = V4

    service = const svc
      where
        svc :: Service EC2
        svc = Service
            { _svcAbbrev   = "EC2"
            , _svcPrefix   = "ec2"
            , _svcVersion  = "2015-04-15"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout  = 80000000
            , _svcStatus   = statusSuccess
            , _svcError    = parseXMLError
            , _svcRetry    = retry
            }

        retry :: Retry
        retry = Exponential
            { _retryBase     = 0
            , _retryGrowth   = 0
            , _retryAttempts = 0
            , _retryCheck    = check
            }

        check :: ServiceError -> Bool
        check ServiceError'{..} = error "FIXME: Retry check not implemented."

data AccountAttributeName = SupportedPlatforms | DefaultVPC deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText AccountAttributeName where
    parser = takeLowerText >>= \case
        "default-vpc" -> pure DefaultVPC
        "supported-platforms" -> pure SupportedPlatforms
        e -> fail ("Failure parsing AccountAttributeName from " ++ show e)

instance ToText AccountAttributeName where
    toText = \case
        DefaultVPC -> "default-vpc"
        SupportedPlatforms -> "supported-platforms"

instance Hashable AccountAttributeName
instance ToQuery AccountAttributeName
instance ToHeader AccountAttributeName

data AddressStatus = MoveInProgress | InVPC | InClassic deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText AddressStatus where
    parser = takeLowerText >>= \case
        "InClassic" -> pure InClassic
        "InVpc" -> pure InVPC
        "MoveInProgress" -> pure MoveInProgress
        e -> fail ("Failure parsing AddressStatus from " ++ show e)

instance ToText AddressStatus where
    toText = \case
        InClassic -> "InClassic"
        InVPC -> "InVpc"
        MoveInProgress -> "MoveInProgress"

instance Hashable AddressStatus
instance ToQuery AddressStatus
instance ToHeader AddressStatus

instance FromXML AddressStatus where
    parseXML = parseXMLText "AddressStatus"

data ArchitectureValues = I386 | X8664 deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText ArchitectureValues where
    parser = takeLowerText >>= \case
        "i386" -> pure I386
        "x86_64" -> pure X8664
        e -> fail ("Failure parsing ArchitectureValues from " ++ show e)

instance ToText ArchitectureValues where
    toText = \case
        I386 -> "i386"
        X8664 -> "x86_64"

instance Hashable ArchitectureValues
instance ToQuery ArchitectureValues
instance ToHeader ArchitectureValues

instance FromXML ArchitectureValues where
    parseXML = parseXMLText "ArchitectureValues"

data AttachmentStatus = Detached | Detaching | Attached | Attaching deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText AttachmentStatus where
    parser = takeLowerText >>= \case
        "attached" -> pure Attached
        "attaching" -> pure Attaching
        "detached" -> pure Detached
        "detaching" -> pure Detaching
        e -> fail ("Failure parsing AttachmentStatus from " ++ show e)

instance ToText AttachmentStatus where
    toText = \case
        Attached -> "attached"
        Attaching -> "attaching"
        Detached -> "detached"
        Detaching -> "detaching"

instance Hashable AttachmentStatus
instance ToQuery AttachmentStatus
instance ToHeader AttachmentStatus

instance FromXML AttachmentStatus where
    parseXML = parseXMLText "AttachmentStatus"

data AvailabilityZoneState = AZSAvailable deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText AvailabilityZoneState where
    parser = takeLowerText >>= \case
        "available" -> pure AZSAvailable
        e -> fail ("Failure parsing AvailabilityZoneState from " ++ show e)

instance ToText AvailabilityZoneState where
    toText = \case
        AZSAvailable -> "available"

instance Hashable AvailabilityZoneState
instance ToQuery AvailabilityZoneState
instance ToHeader AvailabilityZoneState

instance FromXML AvailabilityZoneState where
    parseXML = parseXMLText "AvailabilityZoneState"

data BatchState = BSCancelled | BSSubmitted | BSFailed | BSActive | BSCancelledRunning | BSCancelledTerminating deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText BatchState where
    parser = takeLowerText >>= \case
        "active" -> pure BSActive
        "cancelled" -> pure BSCancelled
        "cancelled_running" -> pure BSCancelledRunning
        "cancelled_terminating" -> pure BSCancelledTerminating
        "failed" -> pure BSFailed
        "submitted" -> pure BSSubmitted
        e -> fail ("Failure parsing BatchState from " ++ show e)

instance ToText BatchState where
    toText = \case
        BSActive -> "active"
        BSCancelled -> "cancelled"
        BSCancelledRunning -> "cancelled_running"
        BSCancelledTerminating -> "cancelled_terminating"
        BSFailed -> "failed"
        BSSubmitted -> "submitted"

instance Hashable BatchState
instance ToQuery BatchState
instance ToHeader BatchState

instance FromXML BatchState where
    parseXML = parseXMLText "BatchState"

data BundleTaskState = BTSComplete | BTSStoring | BTSPending | BTSWaitingForShutdown | BTSBundling | BTSCancelling | BTSFailed deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText BundleTaskState where
    parser = takeLowerText >>= \case
        "bundling" -> pure BTSBundling
        "cancelling" -> pure BTSCancelling
        "complete" -> pure BTSComplete
        "failed" -> pure BTSFailed
        "pending" -> pure BTSPending
        "storing" -> pure BTSStoring
        "waiting-for-shutdown" -> pure BTSWaitingForShutdown
        e -> fail ("Failure parsing BundleTaskState from " ++ show e)

instance ToText BundleTaskState where
    toText = \case
        BTSBundling -> "bundling"
        BTSCancelling -> "cancelling"
        BTSComplete -> "complete"
        BTSFailed -> "failed"
        BTSPending -> "pending"
        BTSStoring -> "storing"
        BTSWaitingForShutdown -> "waiting-for-shutdown"

instance Hashable BundleTaskState
instance ToQuery BundleTaskState
instance ToHeader BundleTaskState

instance FromXML BundleTaskState where
    parseXML = parseXMLText "BundleTaskState"

data CancelBatchErrorCode = FleetRequestNotInCancellableState | FleetRequestIdMalformed | UnexpectedError | FleetRequestIdDoesNotExist deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText CancelBatchErrorCode where
    parser = takeLowerText >>= \case
        "fleetRequestIdDoesNotExist" -> pure FleetRequestIdDoesNotExist
        "fleetRequestIdMalformed" -> pure FleetRequestIdMalformed
        "fleetRequestNotInCancellableState" -> pure FleetRequestNotInCancellableState
        "unexpectedError" -> pure UnexpectedError
        e -> fail ("Failure parsing CancelBatchErrorCode from " ++ show e)

instance ToText CancelBatchErrorCode where
    toText = \case
        FleetRequestIdDoesNotExist -> "fleetRequestIdDoesNotExist"
        FleetRequestIdMalformed -> "fleetRequestIdMalformed"
        FleetRequestNotInCancellableState -> "fleetRequestNotInCancellableState"
        UnexpectedError -> "unexpectedError"

instance Hashable CancelBatchErrorCode
instance ToQuery CancelBatchErrorCode
instance ToHeader CancelBatchErrorCode

instance FromXML CancelBatchErrorCode where
    parseXML = parseXMLText "CancelBatchErrorCode"

data CancelSpotInstanceRequestState = CSIRSClosed | CSIRSActive | CSIRSOpen | CSIRSCompleted | CSIRSCancelled deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText CancelSpotInstanceRequestState where
    parser = takeLowerText >>= \case
        "active" -> pure CSIRSActive
        "cancelled" -> pure CSIRSCancelled
        "closed" -> pure CSIRSClosed
        "completed" -> pure CSIRSCompleted
        "open" -> pure CSIRSOpen
        e -> fail ("Failure parsing CancelSpotInstanceRequestState from " ++ show e)

instance ToText CancelSpotInstanceRequestState where
    toText = \case
        CSIRSActive -> "active"
        CSIRSCancelled -> "cancelled"
        CSIRSClosed -> "closed"
        CSIRSCompleted -> "completed"
        CSIRSOpen -> "open"

instance Hashable CancelSpotInstanceRequestState
instance ToQuery CancelSpotInstanceRequestState
instance ToHeader CancelSpotInstanceRequestState

instance FromXML CancelSpotInstanceRequestState where
    parseXML = parseXMLText "CancelSpotInstanceRequestState"

data ContainerFormat = Ova deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText ContainerFormat where
    parser = takeLowerText >>= \case
        "ova" -> pure Ova
        e -> fail ("Failure parsing ContainerFormat from " ++ show e)

instance ToText ContainerFormat where
    toText = \case
        Ova -> "ova"

instance Hashable ContainerFormat
instance ToQuery ContainerFormat
instance ToHeader ContainerFormat

instance FromXML ContainerFormat where
    parseXML = parseXMLText "ContainerFormat"

data ConversionTaskState = CTSCancelled | CTSActive | CTSCancelling | CTSCompleted deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText ConversionTaskState where
    parser = takeLowerText >>= \case
        "active" -> pure CTSActive
        "cancelled" -> pure CTSCancelled
        "cancelling" -> pure CTSCancelling
        "completed" -> pure CTSCompleted
        e -> fail ("Failure parsing ConversionTaskState from " ++ show e)

instance ToText ConversionTaskState where
    toText = \case
        CTSActive -> "active"
        CTSCancelled -> "cancelled"
        CTSCancelling -> "cancelling"
        CTSCompleted -> "completed"

instance Hashable ConversionTaskState
instance ToQuery ConversionTaskState
instance ToHeader ConversionTaskState

instance FromXML ConversionTaskState where
    parseXML = parseXMLText "ConversionTaskState"

data CurrencyCodeValues = Usd deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText CurrencyCodeValues where
    parser = takeLowerText >>= \case
        "USD" -> pure Usd
        e -> fail ("Failure parsing CurrencyCodeValues from " ++ show e)

instance ToText CurrencyCodeValues where
    toText = \case
        Usd -> "USD"

instance Hashable CurrencyCodeValues
instance ToQuery CurrencyCodeValues
instance ToHeader CurrencyCodeValues

instance FromXML CurrencyCodeValues where
    parseXML = parseXMLText "CurrencyCodeValues"

data DatafeedSubscriptionState = DSSInactive | DSSActive deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText DatafeedSubscriptionState where
    parser = takeLowerText >>= \case
        "Active" -> pure DSSActive
        "Inactive" -> pure DSSInactive
        e -> fail ("Failure parsing DatafeedSubscriptionState from " ++ show e)

instance ToText DatafeedSubscriptionState where
    toText = \case
        DSSActive -> "Active"
        DSSInactive -> "Inactive"

instance Hashable DatafeedSubscriptionState
instance ToQuery DatafeedSubscriptionState
instance ToHeader DatafeedSubscriptionState

instance FromXML DatafeedSubscriptionState where
    parseXML = parseXMLText "DatafeedSubscriptionState"

data DeviceType = InstanceStore | EBS deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText DeviceType where
    parser = takeLowerText >>= \case
        "ebs" -> pure EBS
        "instance-store" -> pure InstanceStore
        e -> fail ("Failure parsing DeviceType from " ++ show e)

instance ToText DeviceType where
    toText = \case
        EBS -> "ebs"
        InstanceStore -> "instance-store"

instance Hashable DeviceType
instance ToQuery DeviceType
instance ToHeader DeviceType

instance FromXML DeviceType where
    parseXML = parseXMLText "DeviceType"

data DiskImageFormat = Raw | VHD | VMDK deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText DiskImageFormat where
    parser = takeLowerText >>= \case
        "RAW" -> pure Raw
        "VHD" -> pure VHD
        "VMDK" -> pure VMDK
        e -> fail ("Failure parsing DiskImageFormat from " ++ show e)

instance ToText DiskImageFormat where
    toText = \case
        Raw -> "RAW"
        VHD -> "VHD"
        VMDK -> "VMDK"

instance Hashable DiskImageFormat
instance ToQuery DiskImageFormat
instance ToHeader DiskImageFormat

instance FromXML DiskImageFormat where
    parseXML = parseXMLText "DiskImageFormat"

data DomainType = DTStandard | DTVPC deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText DomainType where
    parser = takeLowerText >>= \case
        "standard" -> pure DTStandard
        "vpc" -> pure DTVPC
        e -> fail ("Failure parsing DomainType from " ++ show e)

instance ToText DomainType where
    toText = \case
        DTStandard -> "standard"
        DTVPC -> "vpc"

instance Hashable DomainType
instance ToQuery DomainType
instance ToHeader DomainType

instance FromXML DomainType where
    parseXML = parseXMLText "DomainType"

data EventCode = InstanceReboot | InstanceRetirement | InstanceStop | SystemReboot | SystemMaintenance deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText EventCode where
    parser = takeLowerText >>= \case
        "instance-reboot" -> pure InstanceReboot
        "instance-retirement" -> pure InstanceRetirement
        "instance-stop" -> pure InstanceStop
        "system-maintenance" -> pure SystemMaintenance
        "system-reboot" -> pure SystemReboot
        e -> fail ("Failure parsing EventCode from " ++ show e)

instance ToText EventCode where
    toText = \case
        InstanceReboot -> "instance-reboot"
        InstanceRetirement -> "instance-retirement"
        InstanceStop -> "instance-stop"
        SystemMaintenance -> "system-maintenance"
        SystemReboot -> "system-reboot"

instance Hashable EventCode
instance ToQuery EventCode
instance ToHeader EventCode

instance FromXML EventCode where
    parseXML = parseXMLText "EventCode"

data EventType = InstanceChange | Error | FleetRequestChange deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText EventType where
    parser = takeLowerText >>= \case
        "error" -> pure Error
        "fleetRequestChange" -> pure FleetRequestChange
        "instanceChange" -> pure InstanceChange
        e -> fail ("Failure parsing EventType from " ++ show e)

instance ToText EventType where
    toText = \case
        Error -> "error"
        FleetRequestChange -> "fleetRequestChange"
        InstanceChange -> "instanceChange"

instance Hashable EventType
instance ToQuery EventType
instance ToHeader EventType

instance FromXML EventType where
    parseXML = parseXMLText "EventType"

data ExportEnvironment = Citrix | Microsoft | VMware deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText ExportEnvironment where
    parser = takeLowerText >>= \case
        "citrix" -> pure Citrix
        "microsoft" -> pure Microsoft
        "vmware" -> pure VMware
        e -> fail ("Failure parsing ExportEnvironment from " ++ show e)

instance ToText ExportEnvironment where
    toText = \case
        Citrix -> "citrix"
        Microsoft -> "microsoft"
        VMware -> "vmware"

instance Hashable ExportEnvironment
instance ToQuery ExportEnvironment
instance ToHeader ExportEnvironment

instance FromXML ExportEnvironment where
    parseXML = parseXMLText "ExportEnvironment"

data ExportTaskState = ETSCompleted | ETSCancelled | ETSCancelling | ETSActive deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText ExportTaskState where
    parser = takeLowerText >>= \case
        "active" -> pure ETSActive
        "cancelled" -> pure ETSCancelled
        "cancelling" -> pure ETSCancelling
        "completed" -> pure ETSCompleted
        e -> fail ("Failure parsing ExportTaskState from " ++ show e)

instance ToText ExportTaskState where
    toText = \case
        ETSActive -> "active"
        ETSCancelled -> "cancelled"
        ETSCancelling -> "cancelling"
        ETSCompleted -> "completed"

instance Hashable ExportTaskState
instance ToQuery ExportTaskState
instance ToHeader ExportTaskState

instance FromXML ExportTaskState where
    parseXML = parseXMLText "ExportTaskState"

data FlowLogsResourceType = FLRTSubnet | FLRTNetworkInterface | FLRTVPC deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText FlowLogsResourceType where
    parser = takeLowerText >>= \case
        "NetworkInterface" -> pure FLRTNetworkInterface
        "Subnet" -> pure FLRTSubnet
        "VPC" -> pure FLRTVPC
        e -> fail ("Failure parsing FlowLogsResourceType from " ++ show e)

instance ToText FlowLogsResourceType where
    toText = \case
        FLRTNetworkInterface -> "NetworkInterface"
        FLRTSubnet -> "Subnet"
        FLRTVPC -> "VPC"

instance Hashable FlowLogsResourceType
instance ToQuery FlowLogsResourceType
instance ToHeader FlowLogsResourceType

data GatewayType = IPsec1 deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText GatewayType where
    parser = takeLowerText >>= \case
        "ipsec.1" -> pure IPsec1
        e -> fail ("Failure parsing GatewayType from " ++ show e)

instance ToText GatewayType where
    toText = \case
        IPsec1 -> "ipsec.1"

instance Hashable GatewayType
instance ToQuery GatewayType
instance ToHeader GatewayType

instance FromXML GatewayType where
    parseXML = parseXMLText "GatewayType"

data HypervisorType = Xen | Ovm deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText HypervisorType where
    parser = takeLowerText >>= \case
        "ovm" -> pure Ovm
        "xen" -> pure Xen
        e -> fail ("Failure parsing HypervisorType from " ++ show e)

instance ToText HypervisorType where
    toText = \case
        Ovm -> "ovm"
        Xen -> "xen"

instance Hashable HypervisorType
instance ToQuery HypervisorType
instance ToHeader HypervisorType

instance FromXML HypervisorType where
    parseXML = parseXMLText "HypervisorType"

data ImageAttributeName = BlockDeviceMapping | RAMDisk | Kernel | LaunchPermission | SRIOVNetSupport | ProductCodes | Description deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText ImageAttributeName where
    parser = takeLowerText >>= \case
        "blockDeviceMapping" -> pure BlockDeviceMapping
        "description" -> pure Description
        "kernel" -> pure Kernel
        "launchPermission" -> pure LaunchPermission
        "productCodes" -> pure ProductCodes
        "ramdisk" -> pure RAMDisk
        "sriovNetSupport" -> pure SRIOVNetSupport
        e -> fail ("Failure parsing ImageAttributeName from " ++ show e)

instance ToText ImageAttributeName where
    toText = \case
        BlockDeviceMapping -> "blockDeviceMapping"
        Description -> "description"
        Kernel -> "kernel"
        LaunchPermission -> "launchPermission"
        ProductCodes -> "productCodes"
        RAMDisk -> "ramdisk"
        SRIOVNetSupport -> "sriovNetSupport"

instance Hashable ImageAttributeName
instance ToQuery ImageAttributeName
instance ToHeader ImageAttributeName

data ImageState = ISAvailable | ISDeregistered | ISFailed | ISError | ISPending | ISInvalid | ISTransient deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText ImageState where
    parser = takeLowerText >>= \case
        "available" -> pure ISAvailable
        "deregistered" -> pure ISDeregistered
        "error" -> pure ISError
        "failed" -> pure ISFailed
        "invalid" -> pure ISInvalid
        "pending" -> pure ISPending
        "transient" -> pure ISTransient
        e -> fail ("Failure parsing ImageState from " ++ show e)

instance ToText ImageState where
    toText = \case
        ISAvailable -> "available"
        ISDeregistered -> "deregistered"
        ISError -> "error"
        ISFailed -> "failed"
        ISInvalid -> "invalid"
        ISPending -> "pending"
        ISTransient -> "transient"

instance Hashable ImageState
instance ToQuery ImageState
instance ToHeader ImageState

instance FromXML ImageState where
    parseXML = parseXMLText "ImageState"

data ImageTypeValues = ITVKernel | ITVMachine | ITVRAMDisk deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText ImageTypeValues where
    parser = takeLowerText >>= \case
        "kernel" -> pure ITVKernel
        "machine" -> pure ITVMachine
        "ramdisk" -> pure ITVRAMDisk
        e -> fail ("Failure parsing ImageTypeValues from " ++ show e)

instance ToText ImageTypeValues where
    toText = \case
        ITVKernel -> "kernel"
        ITVMachine -> "machine"
        ITVRAMDisk -> "ramdisk"

instance Hashable ImageTypeValues
instance ToQuery ImageTypeValues
instance ToHeader ImageTypeValues

instance FromXML ImageTypeValues where
    parseXML = parseXMLText "ImageTypeValues"

data InstanceAttributeName = IANInstanceInitiatedShutdownBehavior | IANProductCodes | IANGroupSet | IANDisableAPITermination | IANSRIOVNetSupport | IANRootDeviceName | IANUserData | IANEBSOptimized | IANInstanceType | IANRAMDisk | IANKernel | IANBlockDeviceMapping | IANSourceDestCheck deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText InstanceAttributeName where
    parser = takeLowerText >>= \case
        "blockDeviceMapping" -> pure IANBlockDeviceMapping
        "disableApiTermination" -> pure IANDisableAPITermination
        "ebsOptimized" -> pure IANEBSOptimized
        "groupSet" -> pure IANGroupSet
        "instanceInitiatedShutdownBehavior" -> pure IANInstanceInitiatedShutdownBehavior
        "instanceType" -> pure IANInstanceType
        "kernel" -> pure IANKernel
        "productCodes" -> pure IANProductCodes
        "ramdisk" -> pure IANRAMDisk
        "rootDeviceName" -> pure IANRootDeviceName
        "sriovNetSupport" -> pure IANSRIOVNetSupport
        "sourceDestCheck" -> pure IANSourceDestCheck
        "userData" -> pure IANUserData
        e -> fail ("Failure parsing InstanceAttributeName from " ++ show e)

instance ToText InstanceAttributeName where
    toText = \case
        IANBlockDeviceMapping -> "blockDeviceMapping"
        IANDisableAPITermination -> "disableApiTermination"
        IANEBSOptimized -> "ebsOptimized"
        IANGroupSet -> "groupSet"
        IANInstanceInitiatedShutdownBehavior -> "instanceInitiatedShutdownBehavior"
        IANInstanceType -> "instanceType"
        IANKernel -> "kernel"
        IANProductCodes -> "productCodes"
        IANRAMDisk -> "ramdisk"
        IANRootDeviceName -> "rootDeviceName"
        IANSRIOVNetSupport -> "sriovNetSupport"
        IANSourceDestCheck -> "sourceDestCheck"
        IANUserData -> "userData"

instance Hashable InstanceAttributeName
instance ToQuery InstanceAttributeName
instance ToHeader InstanceAttributeName

data InstanceLifecycleType = Spot deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText InstanceLifecycleType where
    parser = takeLowerText >>= \case
        "spot" -> pure Spot
        e -> fail ("Failure parsing InstanceLifecycleType from " ++ show e)

instance ToText InstanceLifecycleType where
    toText = \case
        Spot -> "spot"

instance Hashable InstanceLifecycleType
instance ToQuery InstanceLifecycleType
instance ToHeader InstanceLifecycleType

instance FromXML InstanceLifecycleType where
    parseXML = parseXMLText "InstanceLifecycleType"

data InstanceStateName = ISNStopped | ISNPending | ISNStopping | ISNShuttingDown | ISNRunning | ISNTerminated deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText InstanceStateName where
    parser = takeLowerText >>= \case
        "pending" -> pure ISNPending
        "running" -> pure ISNRunning
        "shutting-down" -> pure ISNShuttingDown
        "stopped" -> pure ISNStopped
        "stopping" -> pure ISNStopping
        "terminated" -> pure ISNTerminated
        e -> fail ("Failure parsing InstanceStateName from " ++ show e)

instance ToText InstanceStateName where
    toText = \case
        ISNPending -> "pending"
        ISNRunning -> "running"
        ISNShuttingDown -> "shutting-down"
        ISNStopped -> "stopped"
        ISNStopping -> "stopping"
        ISNTerminated -> "terminated"

instance Hashable InstanceStateName
instance ToQuery InstanceStateName
instance ToHeader InstanceStateName

instance FromXML InstanceStateName where
    parseXML = parseXMLText "InstanceStateName"

data InstanceType = I22XLarge | D24XLarge | CC28XLarge | C1Medium | C48XLarge | C4XLarge | I24XLarge | C4Large | C3XLarge | CR18XLarge | M410XLarge | C42XLarge | CC14XLarge | D28XLarge | M3Large | M3Medium | T2Medium | M24XLarge | R32XLarge | M42XLarge | M1XLarge | M32XLarge | D22XLarge | T2Small | CG14XLarge | R3Large | M4XLarge | M4Large | T2Micro | R3XLarge | R38XLarge | HS18XLarge | C1XLarge | T1Micro | C32XLarge | G22XLarge | M2XLarge | C3Large | C38XLarge | R34XLarge | M44XLarge | M22XLarge | D2XLarge | M1Small | M3XLarge | I2XLarge | C44XLarge | I28XLarge | HI14XLarge | C34XLarge | M1Large | M1Medium deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText InstanceType where
    parser = takeLowerText >>= \case
        "c1.medium" -> pure C1Medium
        "c1.xlarge" -> pure C1XLarge
        "c3.2xlarge" -> pure C32XLarge
        "c3.4xlarge" -> pure C34XLarge
        "c3.8xlarge" -> pure C38XLarge
        "c3.large" -> pure C3Large
        "c3.xlarge" -> pure C3XLarge
        "c4.2xlarge" -> pure C42XLarge
        "c4.4xlarge" -> pure C44XLarge
        "c4.8xlarge" -> pure C48XLarge
        "c4.large" -> pure C4Large
        "c4.xlarge" -> pure C4XLarge
        "cc1.4xlarge" -> pure CC14XLarge
        "cc2.8xlarge" -> pure CC28XLarge
        "cg1.4xlarge" -> pure CG14XLarge
        "cr1.8xlarge" -> pure CR18XLarge
        "d2.2xlarge" -> pure D22XLarge
        "d2.4xlarge" -> pure D24XLarge
        "d2.8xlarge" -> pure D28XLarge
        "d2.xlarge" -> pure D2XLarge
        "g2.2xlarge" -> pure G22XLarge
        "hi1.4xlarge" -> pure HI14XLarge
        "hs1.8xlarge" -> pure HS18XLarge
        "i2.2xlarge" -> pure I22XLarge
        "i2.4xlarge" -> pure I24XLarge
        "i2.8xlarge" -> pure I28XLarge
        "i2.xlarge" -> pure I2XLarge
        "m1.large" -> pure M1Large
        "m1.medium" -> pure M1Medium
        "m1.small" -> pure M1Small
        "m1.xlarge" -> pure M1XLarge
        "m2.2xlarge" -> pure M22XLarge
        "m2.4xlarge" -> pure M24XLarge
        "m2.xlarge" -> pure M2XLarge
        "m3.2xlarge" -> pure M32XLarge
        "m3.large" -> pure M3Large
        "m3.medium" -> pure M3Medium
        "m3.xlarge" -> pure M3XLarge
        "m4.10xlarge" -> pure M410XLarge
        "m4.2xlarge" -> pure M42XLarge
        "m4.4xlarge" -> pure M44XLarge
        "m4.large" -> pure M4Large
        "m4.xlarge" -> pure M4XLarge
        "r3.2xlarge" -> pure R32XLarge
        "r3.4xlarge" -> pure R34XLarge
        "r3.8xlarge" -> pure R38XLarge
        "r3.large" -> pure R3Large
        "r3.xlarge" -> pure R3XLarge
        "t1.micro" -> pure T1Micro
        "t2.medium" -> pure T2Medium
        "t2.micro" -> pure T2Micro
        "t2.small" -> pure T2Small
        e -> fail ("Failure parsing InstanceType from " ++ show e)

instance ToText InstanceType where
    toText = \case
        C1Medium -> "c1.medium"
        C1XLarge -> "c1.xlarge"
        C32XLarge -> "c3.2xlarge"
        C34XLarge -> "c3.4xlarge"
        C38XLarge -> "c3.8xlarge"
        C3Large -> "c3.large"
        C3XLarge -> "c3.xlarge"
        C42XLarge -> "c4.2xlarge"
        C44XLarge -> "c4.4xlarge"
        C48XLarge -> "c4.8xlarge"
        C4Large -> "c4.large"
        C4XLarge -> "c4.xlarge"
        CC14XLarge -> "cc1.4xlarge"
        CC28XLarge -> "cc2.8xlarge"
        CG14XLarge -> "cg1.4xlarge"
        CR18XLarge -> "cr1.8xlarge"
        D22XLarge -> "d2.2xlarge"
        D24XLarge -> "d2.4xlarge"
        D28XLarge -> "d2.8xlarge"
        D2XLarge -> "d2.xlarge"
        G22XLarge -> "g2.2xlarge"
        HI14XLarge -> "hi1.4xlarge"
        HS18XLarge -> "hs1.8xlarge"
        I22XLarge -> "i2.2xlarge"
        I24XLarge -> "i2.4xlarge"
        I28XLarge -> "i2.8xlarge"
        I2XLarge -> "i2.xlarge"
        M1Large -> "m1.large"
        M1Medium -> "m1.medium"
        M1Small -> "m1.small"
        M1XLarge -> "m1.xlarge"
        M22XLarge -> "m2.2xlarge"
        M24XLarge -> "m2.4xlarge"
        M2XLarge -> "m2.xlarge"
        M32XLarge -> "m3.2xlarge"
        M3Large -> "m3.large"
        M3Medium -> "m3.medium"
        M3XLarge -> "m3.xlarge"
        M410XLarge -> "m4.10xlarge"
        M42XLarge -> "m4.2xlarge"
        M44XLarge -> "m4.4xlarge"
        M4Large -> "m4.large"
        M4XLarge -> "m4.xlarge"
        R32XLarge -> "r3.2xlarge"
        R34XLarge -> "r3.4xlarge"
        R38XLarge -> "r3.8xlarge"
        R3Large -> "r3.large"
        R3XLarge -> "r3.xlarge"
        T1Micro -> "t1.micro"
        T2Medium -> "t2.medium"
        T2Micro -> "t2.micro"
        T2Small -> "t2.small"

instance Hashable InstanceType
instance ToQuery InstanceType
instance ToHeader InstanceType

instance FromXML InstanceType where
    parseXML = parseXMLText "InstanceType"

data ListingState = LisAvailable | LisCancelled | LisSold | LisPending deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText ListingState where
    parser = takeLowerText >>= \case
        "available" -> pure LisAvailable
        "cancelled" -> pure LisCancelled
        "pending" -> pure LisPending
        "sold" -> pure LisSold
        e -> fail ("Failure parsing ListingState from " ++ show e)

instance ToText ListingState where
    toText = \case
        LisAvailable -> "available"
        LisCancelled -> "cancelled"
        LisPending -> "pending"
        LisSold -> "sold"

instance Hashable ListingState
instance ToQuery ListingState
instance ToHeader ListingState

instance FromXML ListingState where
    parseXML = parseXMLText "ListingState"

data ListingStatus = LSPending | LSActive | LSCancelled | LSClosed deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText ListingStatus where
    parser = takeLowerText >>= \case
        "active" -> pure LSActive
        "cancelled" -> pure LSCancelled
        "closed" -> pure LSClosed
        "pending" -> pure LSPending
        e -> fail ("Failure parsing ListingStatus from " ++ show e)

instance ToText ListingStatus where
    toText = \case
        LSActive -> "active"
        LSCancelled -> "cancelled"
        LSClosed -> "closed"
        LSPending -> "pending"

instance Hashable ListingStatus
instance ToQuery ListingStatus
instance ToHeader ListingStatus

instance FromXML ListingStatus where
    parseXML = parseXMLText "ListingStatus"

data MonitoringState = MSDisabling | MSEnabled | MSPending | MSDisabled deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText MonitoringState where
    parser = takeLowerText >>= \case
        "disabled" -> pure MSDisabled
        "disabling" -> pure MSDisabling
        "enabled" -> pure MSEnabled
        "pending" -> pure MSPending
        e -> fail ("Failure parsing MonitoringState from " ++ show e)

instance ToText MonitoringState where
    toText = \case
        MSDisabled -> "disabled"
        MSDisabling -> "disabling"
        MSEnabled -> "enabled"
        MSPending -> "pending"

instance Hashable MonitoringState
instance ToQuery MonitoringState
instance ToHeader MonitoringState

instance FromXML MonitoringState where
    parseXML = parseXMLText "MonitoringState"

data MoveStatus = RestoringToClassic | MovingToVPC deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText MoveStatus where
    parser = takeLowerText >>= \case
        "movingToVpc" -> pure MovingToVPC
        "restoringToClassic" -> pure RestoringToClassic
        e -> fail ("Failure parsing MoveStatus from " ++ show e)

instance ToText MoveStatus where
    toText = \case
        MovingToVPC -> "movingToVpc"
        RestoringToClassic -> "restoringToClassic"

instance Hashable MoveStatus
instance ToQuery MoveStatus
instance ToHeader MoveStatus

instance FromXML MoveStatus where
    parseXML = parseXMLText "MoveStatus"

data NetworkInterfaceAttribute = NIAGroupSet | NIAAttachment | NIADescription | NIASourceDestCheck deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText NetworkInterfaceAttribute where
    parser = takeLowerText >>= \case
        "attachment" -> pure NIAAttachment
        "description" -> pure NIADescription
        "groupSet" -> pure NIAGroupSet
        "sourceDestCheck" -> pure NIASourceDestCheck
        e -> fail ("Failure parsing NetworkInterfaceAttribute from " ++ show e)

instance ToText NetworkInterfaceAttribute where
    toText = \case
        NIAAttachment -> "attachment"
        NIADescription -> "description"
        NIAGroupSet -> "groupSet"
        NIASourceDestCheck -> "sourceDestCheck"

instance Hashable NetworkInterfaceAttribute
instance ToQuery NetworkInterfaceAttribute
instance ToHeader NetworkInterfaceAttribute

data NetworkInterfaceStatus = NISINUse | NISAttaching | NISAvailable | NISDetaching deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText NetworkInterfaceStatus where
    parser = takeLowerText >>= \case
        "attaching" -> pure NISAttaching
        "available" -> pure NISAvailable
        "detaching" -> pure NISDetaching
        "in-use" -> pure NISINUse
        e -> fail ("Failure parsing NetworkInterfaceStatus from " ++ show e)

instance ToText NetworkInterfaceStatus where
    toText = \case
        NISAttaching -> "attaching"
        NISAvailable -> "available"
        NISDetaching -> "detaching"
        NISINUse -> "in-use"

instance Hashable NetworkInterfaceStatus
instance ToQuery NetworkInterfaceStatus
instance ToHeader NetworkInterfaceStatus

instance FromXML NetworkInterfaceStatus where
    parseXML = parseXMLText "NetworkInterfaceStatus"

data OfferingTypeValues = MediumUtilization | NOUpfront | AllUpfront | HeavyUtilization | LightUtilization | PartialUpfront deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText OfferingTypeValues where
    parser = takeLowerText >>= \case
        "All Upfront" -> pure AllUpfront
        "Heavy Utilization" -> pure HeavyUtilization
        "Light Utilization" -> pure LightUtilization
        "Medium Utilization" -> pure MediumUtilization
        "No Upfront" -> pure NOUpfront
        "Partial Upfront" -> pure PartialUpfront
        e -> fail ("Failure parsing OfferingTypeValues from " ++ show e)

instance ToText OfferingTypeValues where
    toText = \case
        AllUpfront -> "All Upfront"
        HeavyUtilization -> "Heavy Utilization"
        LightUtilization -> "Light Utilization"
        MediumUtilization -> "Medium Utilization"
        NOUpfront -> "No Upfront"
        PartialUpfront -> "Partial Upfront"

instance Hashable OfferingTypeValues
instance ToQuery OfferingTypeValues
instance ToHeader OfferingTypeValues

instance FromXML OfferingTypeValues where
    parseXML = parseXMLText "OfferingTypeValues"

data PermissionGroup = PGAll deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText PermissionGroup where
    parser = takeLowerText >>= \case
        "all" -> pure PGAll
        e -> fail ("Failure parsing PermissionGroup from " ++ show e)

instance ToText PermissionGroup where
    toText = \case
        PGAll -> "all"

instance Hashable PermissionGroup
instance ToQuery PermissionGroup
instance ToHeader PermissionGroup

instance FromXML PermissionGroup where
    parseXML = parseXMLText "PermissionGroup"

data PlacementGroupState = PGSDeleting | PGSPending | PGSAvailable | PGSDeleted deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText PlacementGroupState where
    parser = takeLowerText >>= \case
        "available" -> pure PGSAvailable
        "deleted" -> pure PGSDeleted
        "deleting" -> pure PGSDeleting
        "pending" -> pure PGSPending
        e -> fail ("Failure parsing PlacementGroupState from " ++ show e)

instance ToText PlacementGroupState where
    toText = \case
        PGSAvailable -> "available"
        PGSDeleted -> "deleted"
        PGSDeleting -> "deleting"
        PGSPending -> "pending"

instance Hashable PlacementGroupState
instance ToQuery PlacementGroupState
instance ToHeader PlacementGroupState

instance FromXML PlacementGroupState where
    parseXML = parseXMLText "PlacementGroupState"

data PlacementStrategy = Cluster deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText PlacementStrategy where
    parser = takeLowerText >>= \case
        "cluster" -> pure Cluster
        e -> fail ("Failure parsing PlacementStrategy from " ++ show e)

instance ToText PlacementStrategy where
    toText = \case
        Cluster -> "cluster"

instance Hashable PlacementStrategy
instance ToQuery PlacementStrategy
instance ToHeader PlacementStrategy

instance FromXML PlacementStrategy where
    parseXML = parseXMLText "PlacementStrategy"

data PlatformValues = PVWindows deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText PlatformValues where
    parser = takeLowerText >>= \case
        "Windows" -> pure PVWindows
        e -> fail ("Failure parsing PlatformValues from " ++ show e)

instance ToText PlatformValues where
    toText = \case
        PVWindows -> "Windows"

instance Hashable PlatformValues
instance ToQuery PlatformValues
instance ToHeader PlatformValues

instance FromXML PlatformValues where
    parseXML = parseXMLText "PlatformValues"

data ProductCodeValues = Marketplace | Devpay deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText ProductCodeValues where
    parser = takeLowerText >>= \case
        "devpay" -> pure Devpay
        "marketplace" -> pure Marketplace
        e -> fail ("Failure parsing ProductCodeValues from " ++ show e)

instance ToText ProductCodeValues where
    toText = \case
        Devpay -> "devpay"
        Marketplace -> "marketplace"

instance Hashable ProductCodeValues
instance ToQuery ProductCodeValues
instance ToHeader ProductCodeValues

instance FromXML ProductCodeValues where
    parseXML = parseXMLText "ProductCodeValues"

data RIProductDescription = WindowsAmazonVPC | LinuxUnix | LinuxUnixAmazonVPC | Windows deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText RIProductDescription where
    parser = takeLowerText >>= \case
        "Linux/UNIX" -> pure LinuxUnix
        "Linux/UNIX (Amazon VPC)" -> pure LinuxUnixAmazonVPC
        "Windows" -> pure Windows
        "Windows (Amazon VPC)" -> pure WindowsAmazonVPC
        e -> fail ("Failure parsing RIProductDescription from " ++ show e)

instance ToText RIProductDescription where
    toText = \case
        LinuxUnix -> "Linux/UNIX"
        LinuxUnixAmazonVPC -> "Linux/UNIX (Amazon VPC)"
        Windows -> "Windows"
        WindowsAmazonVPC -> "Windows (Amazon VPC)"

instance Hashable RIProductDescription
instance ToQuery RIProductDescription
instance ToHeader RIProductDescription

instance FromXML RIProductDescription where
    parseXML = parseXMLText "RIProductDescription"

data RecurringChargeFrequency = Hourly deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText RecurringChargeFrequency where
    parser = takeLowerText >>= \case
        "Hourly" -> pure Hourly
        e -> fail ("Failure parsing RecurringChargeFrequency from " ++ show e)

instance ToText RecurringChargeFrequency where
    toText = \case
        Hourly -> "Hourly"

instance Hashable RecurringChargeFrequency
instance ToQuery RecurringChargeFrequency
instance ToHeader RecurringChargeFrequency

instance FromXML RecurringChargeFrequency where
    parseXML = parseXMLText "RecurringChargeFrequency"

data ReportInstanceReasonCodes = PerformanceOther | Other | Unresponsive | NotAcceptingCredentials | InstanceStuckINState | PerformanceNetwork | PerformanceInstanceStore | PerformanceEBSVolume | PasswordNotAvailable deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText ReportInstanceReasonCodes where
    parser = takeLowerText >>= \case
        "instance-stuck-in-state" -> pure InstanceStuckINState
        "not-accepting-credentials" -> pure NotAcceptingCredentials
        "other" -> pure Other
        "password-not-available" -> pure PasswordNotAvailable
        "performance-ebs-volume" -> pure PerformanceEBSVolume
        "performance-instance-store" -> pure PerformanceInstanceStore
        "performance-network" -> pure PerformanceNetwork
        "performance-other" -> pure PerformanceOther
        "unresponsive" -> pure Unresponsive
        e -> fail ("Failure parsing ReportInstanceReasonCodes from " ++ show e)

instance ToText ReportInstanceReasonCodes where
    toText = \case
        InstanceStuckINState -> "instance-stuck-in-state"
        NotAcceptingCredentials -> "not-accepting-credentials"
        Other -> "other"
        PasswordNotAvailable -> "password-not-available"
        PerformanceEBSVolume -> "performance-ebs-volume"
        PerformanceInstanceStore -> "performance-instance-store"
        PerformanceNetwork -> "performance-network"
        PerformanceOther -> "performance-other"
        Unresponsive -> "unresponsive"

instance Hashable ReportInstanceReasonCodes
instance ToQuery ReportInstanceReasonCodes
instance ToHeader ReportInstanceReasonCodes

data ReportStatusType = OK | Impaired deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText ReportStatusType where
    parser = takeLowerText >>= \case
        "impaired" -> pure Impaired
        "ok" -> pure OK
        e -> fail ("Failure parsing ReportStatusType from " ++ show e)

instance ToText ReportStatusType where
    toText = \case
        Impaired -> "impaired"
        OK -> "ok"

instance Hashable ReportStatusType
instance ToQuery ReportStatusType
instance ToHeader ReportStatusType

data ReservedInstanceState = PaymentPending | Retired | Active | PaymentFailed deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText ReservedInstanceState where
    parser = takeLowerText >>= \case
        "active" -> pure Active
        "payment-failed" -> pure PaymentFailed
        "payment-pending" -> pure PaymentPending
        "retired" -> pure Retired
        e -> fail ("Failure parsing ReservedInstanceState from " ++ show e)

instance ToText ReservedInstanceState where
    toText = \case
        Active -> "active"
        PaymentFailed -> "payment-failed"
        PaymentPending -> "payment-pending"
        Retired -> "retired"

instance Hashable ReservedInstanceState
instance ToQuery ReservedInstanceState
instance ToHeader ReservedInstanceState

instance FromXML ReservedInstanceState where
    parseXML = parseXMLText "ReservedInstanceState"

data ResetImageAttributeName = RIANLaunchPermission deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText ResetImageAttributeName where
    parser = takeLowerText >>= \case
        "launchPermission" -> pure RIANLaunchPermission
        e -> fail ("Failure parsing ResetImageAttributeName from " ++ show e)

instance ToText ResetImageAttributeName where
    toText = \case
        RIANLaunchPermission -> "launchPermission"

instance Hashable ResetImageAttributeName
instance ToQuery ResetImageAttributeName
instance ToHeader ResetImageAttributeName

data ResourceType = Snapshot | DHCPOptions | Image | Volume | NetworkInterface | Subnet | SecurityGroup | CustomerGateway | RouteTable | VPC | NetworkACL | VPNGateway | InternetGateway | SpotInstancesRequest | VPNConnection | ReservedInstances | Instance deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText ResourceType where
    parser = takeLowerText >>= \case
        "customer-gateway" -> pure CustomerGateway
        "dhcp-options" -> pure DHCPOptions
        "image" -> pure Image
        "instance" -> pure Instance
        "internet-gateway" -> pure InternetGateway
        "network-acl" -> pure NetworkACL
        "network-interface" -> pure NetworkInterface
        "reserved-instances" -> pure ReservedInstances
        "route-table" -> pure RouteTable
        "security-group" -> pure SecurityGroup
        "snapshot" -> pure Snapshot
        "spot-instances-request" -> pure SpotInstancesRequest
        "subnet" -> pure Subnet
        "vpc" -> pure VPC
        "vpn-connection" -> pure VPNConnection
        "vpn-gateway" -> pure VPNGateway
        "volume" -> pure Volume
        e -> fail ("Failure parsing ResourceType from " ++ show e)

instance ToText ResourceType where
    toText = \case
        CustomerGateway -> "customer-gateway"
        DHCPOptions -> "dhcp-options"
        Image -> "image"
        Instance -> "instance"
        InternetGateway -> "internet-gateway"
        NetworkACL -> "network-acl"
        NetworkInterface -> "network-interface"
        ReservedInstances -> "reserved-instances"
        RouteTable -> "route-table"
        SecurityGroup -> "security-group"
        Snapshot -> "snapshot"
        SpotInstancesRequest -> "spot-instances-request"
        Subnet -> "subnet"
        VPC -> "vpc"
        VPNConnection -> "vpn-connection"
        VPNGateway -> "vpn-gateway"
        Volume -> "volume"

instance Hashable ResourceType
instance ToQuery ResourceType
instance ToHeader ResourceType

instance FromXML ResourceType where
    parseXML = parseXMLText "ResourceType"

data RouteOrigin = CreateRouteTable | CreateRoute | EnableVGWRoutePropagation deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText RouteOrigin where
    parser = takeLowerText >>= \case
        "CreateRoute" -> pure CreateRoute
        "CreateRouteTable" -> pure CreateRouteTable
        "EnableVgwRoutePropagation" -> pure EnableVGWRoutePropagation
        e -> fail ("Failure parsing RouteOrigin from " ++ show e)

instance ToText RouteOrigin where
    toText = \case
        CreateRoute -> "CreateRoute"
        CreateRouteTable -> "CreateRouteTable"
        EnableVGWRoutePropagation -> "EnableVgwRoutePropagation"

instance Hashable RouteOrigin
instance ToQuery RouteOrigin
instance ToHeader RouteOrigin

instance FromXML RouteOrigin where
    parseXML = parseXMLText "RouteOrigin"

data RouteState = RSActive | RSBlackhole deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText RouteState where
    parser = takeLowerText >>= \case
        "active" -> pure RSActive
        "blackhole" -> pure RSBlackhole
        e -> fail ("Failure parsing RouteState from " ++ show e)

instance ToText RouteState where
    toText = \case
        RSActive -> "active"
        RSBlackhole -> "blackhole"

instance Hashable RouteState
instance ToQuery RouteState
instance ToHeader RouteState

instance FromXML RouteState where
    parseXML = parseXMLText "RouteState"

data RuleAction = Allow | Deny deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText RuleAction where
    parser = takeLowerText >>= \case
        "allow" -> pure Allow
        "deny" -> pure Deny
        e -> fail ("Failure parsing RuleAction from " ++ show e)

instance ToText RuleAction where
    toText = \case
        Allow -> "allow"
        Deny -> "deny"

instance Hashable RuleAction
instance ToQuery RuleAction
instance ToHeader RuleAction

instance FromXML RuleAction where
    parseXML = parseXMLText "RuleAction"

data ShutdownBehavior = Stop | Terminate deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText ShutdownBehavior where
    parser = takeLowerText >>= \case
        "stop" -> pure Stop
        "terminate" -> pure Terminate
        e -> fail ("Failure parsing ShutdownBehavior from " ++ show e)

instance ToText ShutdownBehavior where
    toText = \case
        Stop -> "stop"
        Terminate -> "terminate"

instance Hashable ShutdownBehavior
instance ToQuery ShutdownBehavior
instance ToHeader ShutdownBehavior

data SnapshotAttributeName = SANProductCodes | SANCreateVolumePermission deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText SnapshotAttributeName where
    parser = takeLowerText >>= \case
        "createVolumePermission" -> pure SANCreateVolumePermission
        "productCodes" -> pure SANProductCodes
        e -> fail ("Failure parsing SnapshotAttributeName from " ++ show e)

instance ToText SnapshotAttributeName where
    toText = \case
        SANCreateVolumePermission -> "createVolumePermission"
        SANProductCodes -> "productCodes"

instance Hashable SnapshotAttributeName
instance ToQuery SnapshotAttributeName
instance ToHeader SnapshotAttributeName

data SnapshotState = SSCompleted | SSPending | SSError deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText SnapshotState where
    parser = takeLowerText >>= \case
        "completed" -> pure SSCompleted
        "error" -> pure SSError
        "pending" -> pure SSPending
        e -> fail ("Failure parsing SnapshotState from " ++ show e)

instance ToText SnapshotState where
    toText = \case
        SSCompleted -> "completed"
        SSError -> "error"
        SSPending -> "pending"

instance Hashable SnapshotState
instance ToQuery SnapshotState
instance ToHeader SnapshotState

instance FromXML SnapshotState where
    parseXML = parseXMLText "SnapshotState"

data SpotInstanceState = SISCancelled | SISClosed | SISFailed | SISActive | SISOpen deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText SpotInstanceState where
    parser = takeLowerText >>= \case
        "active" -> pure SISActive
        "cancelled" -> pure SISCancelled
        "closed" -> pure SISClosed
        "failed" -> pure SISFailed
        "open" -> pure SISOpen
        e -> fail ("Failure parsing SpotInstanceState from " ++ show e)

instance ToText SpotInstanceState where
    toText = \case
        SISActive -> "active"
        SISCancelled -> "cancelled"
        SISClosed -> "closed"
        SISFailed -> "failed"
        SISOpen -> "open"

instance Hashable SpotInstanceState
instance ToQuery SpotInstanceState
instance ToHeader SpotInstanceState

instance FromXML SpotInstanceState where
    parseXML = parseXMLText "SpotInstanceState"

data SpotInstanceType = Persistent | OneTime deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText SpotInstanceType where
    parser = takeLowerText >>= \case
        "one-time" -> pure OneTime
        "persistent" -> pure Persistent
        e -> fail ("Failure parsing SpotInstanceType from " ++ show e)

instance ToText SpotInstanceType where
    toText = \case
        OneTime -> "one-time"
        Persistent -> "persistent"

instance Hashable SpotInstanceType
instance ToQuery SpotInstanceType
instance ToHeader SpotInstanceType

instance FromXML SpotInstanceType where
    parseXML = parseXMLText "SpotInstanceType"

data State = Deleting | Pending | Deleted | Available deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText State where
    parser = takeLowerText >>= \case
        "Available" -> pure Available
        "Deleted" -> pure Deleted
        "Deleting" -> pure Deleting
        "Pending" -> pure Pending
        e -> fail ("Failure parsing State from " ++ show e)

instance ToText State where
    toText = \case
        Available -> "Available"
        Deleted -> "Deleted"
        Deleting -> "Deleting"
        Pending -> "Pending"

instance Hashable State
instance ToQuery State
instance ToHeader State

instance FromXML State where
    parseXML = parseXMLText "State"

data StatusName = Reachability deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText StatusName where
    parser = takeLowerText >>= \case
        "reachability" -> pure Reachability
        e -> fail ("Failure parsing StatusName from " ++ show e)

instance ToText StatusName where
    toText = \case
        Reachability -> "reachability"

instance Hashable StatusName
instance ToQuery StatusName
instance ToHeader StatusName

instance FromXML StatusName where
    parseXML = parseXMLText "StatusName"

data StatusType = InsufficientData | Passed | Initializing | Failed deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText StatusType where
    parser = takeLowerText >>= \case
        "failed" -> pure Failed
        "initializing" -> pure Initializing
        "insufficient-data" -> pure InsufficientData
        "passed" -> pure Passed
        e -> fail ("Failure parsing StatusType from " ++ show e)

instance ToText StatusType where
    toText = \case
        Failed -> "failed"
        Initializing -> "initializing"
        InsufficientData -> "insufficient-data"
        Passed -> "passed"

instance Hashable StatusType
instance ToQuery StatusType
instance ToHeader StatusType

instance FromXML StatusType where
    parseXML = parseXMLText "StatusType"

data SubnetState = SubPending | SubAvailable deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText SubnetState where
    parser = takeLowerText >>= \case
        "available" -> pure SubAvailable
        "pending" -> pure SubPending
        e -> fail ("Failure parsing SubnetState from " ++ show e)

instance ToText SubnetState where
    toText = \case
        SubAvailable -> "available"
        SubPending -> "pending"

instance Hashable SubnetState
instance ToQuery SubnetState
instance ToHeader SubnetState

instance FromXML SubnetState where
    parseXML = parseXMLText "SubnetState"

data SummaryStatus = SSInitializing | SSNotApplicable | SSOK | SSImpaired | SSInsufficientData deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText SummaryStatus where
    parser = takeLowerText >>= \case
        "impaired" -> pure SSImpaired
        "initializing" -> pure SSInitializing
        "insufficient-data" -> pure SSInsufficientData
        "not-applicable" -> pure SSNotApplicable
        "ok" -> pure SSOK
        e -> fail ("Failure parsing SummaryStatus from " ++ show e)

instance ToText SummaryStatus where
    toText = \case
        SSImpaired -> "impaired"
        SSInitializing -> "initializing"
        SSInsufficientData -> "insufficient-data"
        SSNotApplicable -> "not-applicable"
        SSOK -> "ok"

instance Hashable SummaryStatus
instance ToQuery SummaryStatus
instance ToHeader SummaryStatus

instance FromXML SummaryStatus where
    parseXML = parseXMLText "SummaryStatus"

data TelemetryStatus = Down | UP deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText TelemetryStatus where
    parser = takeLowerText >>= \case
        "DOWN" -> pure Down
        "UP" -> pure UP
        e -> fail ("Failure parsing TelemetryStatus from " ++ show e)

instance ToText TelemetryStatus where
    toText = \case
        Down -> "DOWN"
        UP -> "UP"

instance Hashable TelemetryStatus
instance ToQuery TelemetryStatus
instance ToHeader TelemetryStatus

instance FromXML TelemetryStatus where
    parseXML = parseXMLText "TelemetryStatus"

data Tenancy = Default | Dedicated deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText Tenancy where
    parser = takeLowerText >>= \case
        "dedicated" -> pure Dedicated
        "default" -> pure Default
        e -> fail ("Failure parsing Tenancy from " ++ show e)

instance ToText Tenancy where
    toText = \case
        Dedicated -> "dedicated"
        Default -> "default"

instance Hashable Tenancy
instance ToQuery Tenancy
instance ToHeader Tenancy

instance FromXML Tenancy where
    parseXML = parseXMLText "Tenancy"

data TrafficType = Reject | Accept | All deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText TrafficType where
    parser = takeLowerText >>= \case
        "ACCEPT" -> pure Accept
        "ALL" -> pure All
        "REJECT" -> pure Reject
        e -> fail ("Failure parsing TrafficType from " ++ show e)

instance ToText TrafficType where
    toText = \case
        Accept -> "ACCEPT"
        All -> "ALL"
        Reject -> "REJECT"

instance Hashable TrafficType
instance ToQuery TrafficType
instance ToHeader TrafficType

instance FromXML TrafficType where
    parseXML = parseXMLText "TrafficType"

data VPCAttributeName = EnableDNSHostnames | EnableDNSSupport deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText VPCAttributeName where
    parser = takeLowerText >>= \case
        "enableDnsHostnames" -> pure EnableDNSHostnames
        "enableDnsSupport" -> pure EnableDNSSupport
        e -> fail ("Failure parsing VPCAttributeName from " ++ show e)

instance ToText VPCAttributeName where
    toText = \case
        EnableDNSHostnames -> "enableDnsHostnames"
        EnableDNSSupport -> "enableDnsSupport"

instance Hashable VPCAttributeName
instance ToQuery VPCAttributeName
instance ToHeader VPCAttributeName

data VPCState = VpcPending | VpcAvailable deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText VPCState where
    parser = takeLowerText >>= \case
        "available" -> pure VpcAvailable
        "pending" -> pure VpcPending
        e -> fail ("Failure parsing VPCState from " ++ show e)

instance ToText VPCState where
    toText = \case
        VpcAvailable -> "available"
        VpcPending -> "pending"

instance Hashable VPCState
instance ToQuery VPCState
instance ToHeader VPCState

instance FromXML VPCState where
    parseXML = parseXMLText "VPCState"

data VPNState = VSPending | VSAvailable | VSDeleted | VSDeleting deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText VPNState where
    parser = takeLowerText >>= \case
        "available" -> pure VSAvailable
        "deleted" -> pure VSDeleted
        "deleting" -> pure VSDeleting
        "pending" -> pure VSPending
        e -> fail ("Failure parsing VPNState from " ++ show e)

instance ToText VPNState where
    toText = \case
        VSAvailable -> "available"
        VSDeleted -> "deleted"
        VSDeleting -> "deleting"
        VSPending -> "pending"

instance Hashable VPNState
instance ToQuery VPNState
instance ToHeader VPNState

instance FromXML VPNState where
    parseXML = parseXMLText "VPNState"

data VPNStaticRouteSource = Static deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText VPNStaticRouteSource where
    parser = takeLowerText >>= \case
        "Static" -> pure Static
        e -> fail ("Failure parsing VPNStaticRouteSource from " ++ show e)

instance ToText VPNStaticRouteSource where
    toText = \case
        Static -> "Static"

instance Hashable VPNStaticRouteSource
instance ToQuery VPNStaticRouteSource
instance ToHeader VPNStaticRouteSource

instance FromXML VPNStaticRouteSource where
    parseXML = parseXMLText "VPNStaticRouteSource"

data VirtualizationType = Paravirtual | HVM deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText VirtualizationType where
    parser = takeLowerText >>= \case
        "hvm" -> pure HVM
        "paravirtual" -> pure Paravirtual
        e -> fail ("Failure parsing VirtualizationType from " ++ show e)

instance ToText VirtualizationType where
    toText = \case
        HVM -> "hvm"
        Paravirtual -> "paravirtual"

instance Hashable VirtualizationType
instance ToQuery VirtualizationType
instance ToHeader VirtualizationType

instance FromXML VirtualizationType where
    parseXML = parseXMLText "VirtualizationType"

data VolumeAttachmentState = VASAttached | VASAttaching | VASDetached | VASDetaching deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText VolumeAttachmentState where
    parser = takeLowerText >>= \case
        "attached" -> pure VASAttached
        "attaching" -> pure VASAttaching
        "detached" -> pure VASDetached
        "detaching" -> pure VASDetaching
        e -> fail ("Failure parsing VolumeAttachmentState from " ++ show e)

instance ToText VolumeAttachmentState where
    toText = \case
        VASAttached -> "attached"
        VASAttaching -> "attaching"
        VASDetached -> "detached"
        VASDetaching -> "detaching"

instance Hashable VolumeAttachmentState
instance ToQuery VolumeAttachmentState
instance ToHeader VolumeAttachmentState

instance FromXML VolumeAttachmentState where
    parseXML = parseXMLText "VolumeAttachmentState"

data VolumeAttributeName = VANProductCodes | VANAutoEnableIO deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText VolumeAttributeName where
    parser = takeLowerText >>= \case
        "autoEnableIO" -> pure VANAutoEnableIO
        "productCodes" -> pure VANProductCodes
        e -> fail ("Failure parsing VolumeAttributeName from " ++ show e)

instance ToText VolumeAttributeName where
    toText = \case
        VANAutoEnableIO -> "autoEnableIO"
        VANProductCodes -> "productCodes"

instance Hashable VolumeAttributeName
instance ToQuery VolumeAttributeName
instance ToHeader VolumeAttributeName

data VolumeState = VolCreating | VolAvailable | VolDeleted | VolDeleting | VolError | VolINUse deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText VolumeState where
    parser = takeLowerText >>= \case
        "available" -> pure VolAvailable
        "creating" -> pure VolCreating
        "deleted" -> pure VolDeleted
        "deleting" -> pure VolDeleting
        "error" -> pure VolError
        "in-use" -> pure VolINUse
        e -> fail ("Failure parsing VolumeState from " ++ show e)

instance ToText VolumeState where
    toText = \case
        VolAvailable -> "available"
        VolCreating -> "creating"
        VolDeleted -> "deleted"
        VolDeleting -> "deleting"
        VolError -> "error"
        VolINUse -> "in-use"

instance Hashable VolumeState
instance ToQuery VolumeState
instance ToHeader VolumeState

instance FromXML VolumeState where
    parseXML = parseXMLText "VolumeState"

data VolumeStatusInfoStatus = VSISInsufficientData | VSISImpaired | VSISOK deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText VolumeStatusInfoStatus where
    parser = takeLowerText >>= \case
        "impaired" -> pure VSISImpaired
        "insufficient-data" -> pure VSISInsufficientData
        "ok" -> pure VSISOK
        e -> fail ("Failure parsing VolumeStatusInfoStatus from " ++ show e)

instance ToText VolumeStatusInfoStatus where
    toText = \case
        VSISImpaired -> "impaired"
        VSISInsufficientData -> "insufficient-data"
        VSISOK -> "ok"

instance Hashable VolumeStatusInfoStatus
instance ToQuery VolumeStatusInfoStatus
instance ToHeader VolumeStatusInfoStatus

instance FromXML VolumeStatusInfoStatus where
    parseXML = parseXMLText "VolumeStatusInfoStatus"

data VolumeStatusName = IOPerformance | IOEnabled deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText VolumeStatusName where
    parser = takeLowerText >>= \case
        "io-enabled" -> pure IOEnabled
        "io-performance" -> pure IOPerformance
        e -> fail ("Failure parsing VolumeStatusName from " ++ show e)

instance ToText VolumeStatusName where
    toText = \case
        IOEnabled -> "io-enabled"
        IOPerformance -> "io-performance"

instance Hashable VolumeStatusName
instance ToQuery VolumeStatusName
instance ToHeader VolumeStatusName

instance FromXML VolumeStatusName where
    parseXML = parseXMLText "VolumeStatusName"

data VolumeType = Standard | IO1 | GP2 deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText VolumeType where
    parser = takeLowerText >>= \case
        "gp2" -> pure GP2
        "io1" -> pure IO1
        "standard" -> pure Standard
        e -> fail ("Failure parsing VolumeType from " ++ show e)

instance ToText VolumeType where
    toText = \case
        GP2 -> "gp2"
        IO1 -> "io1"
        Standard -> "standard"

instance Hashable VolumeType
instance ToQuery VolumeType
instance ToHeader VolumeType

instance FromXML VolumeType where
    parseXML = parseXMLText "VolumeType"

-- | Describes an account attribute.
--
-- /See:/ 'accountAttribute' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aaAttributeValues'
--
-- * 'aaAttributeName'
data AccountAttribute = AccountAttribute'{_aaAttributeValues :: Maybe [AccountAttributeValue], _aaAttributeName :: Maybe Text} deriving (Eq, Read, Show)

-- | 'AccountAttribute' smart constructor.
accountAttribute :: AccountAttribute
accountAttribute = AccountAttribute'{_aaAttributeValues = Nothing, _aaAttributeName = Nothing};

-- | One or more values for the account attribute.
aaAttributeValues :: Lens' AccountAttribute [AccountAttributeValue]
aaAttributeValues = lens _aaAttributeValues (\ s a -> s{_aaAttributeValues = a}) . _Default;

-- | The name of the account attribute.
aaAttributeName :: Lens' AccountAttribute (Maybe Text)
aaAttributeName = lens _aaAttributeName (\ s a -> s{_aaAttributeName = a});

instance FromXML AccountAttribute where
        parseXML x
          = AccountAttribute' <$>
              (may (parseXMLList "item") x) <*>
                (x .@? "attributeName")

-- | Describes a value of an account attribute.
--
-- /See:/ 'accountAttributeValue' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aavAttributeValue'
newtype AccountAttributeValue = AccountAttributeValue'{_aavAttributeValue :: Maybe Text} deriving (Eq, Read, Show)

-- | 'AccountAttributeValue' smart constructor.
accountAttributeValue :: AccountAttributeValue
accountAttributeValue = AccountAttributeValue'{_aavAttributeValue = Nothing};

-- | The value of the attribute.
aavAttributeValue :: Lens' AccountAttributeValue (Maybe Text)
aavAttributeValue = lens _aavAttributeValue (\ s a -> s{_aavAttributeValue = a});

instance FromXML AccountAttributeValue where
        parseXML x
          = AccountAttributeValue' <$> (x .@? "attributeValue")

-- | Describes a running instance in a Spot fleet.
--
-- /See:/ 'activeInstance' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aiInstanceId'
--
-- * 'aiInstanceType'
--
-- * 'aiSpotInstanceRequestId'
data ActiveInstance = ActiveInstance'{_aiInstanceId :: Maybe Text, _aiInstanceType :: Maybe Text, _aiSpotInstanceRequestId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'ActiveInstance' smart constructor.
activeInstance :: ActiveInstance
activeInstance = ActiveInstance'{_aiInstanceId = Nothing, _aiInstanceType = Nothing, _aiSpotInstanceRequestId = Nothing};

-- | The ID of the instance.
aiInstanceId :: Lens' ActiveInstance (Maybe Text)
aiInstanceId = lens _aiInstanceId (\ s a -> s{_aiInstanceId = a});

-- | The instance type.
aiInstanceType :: Lens' ActiveInstance (Maybe Text)
aiInstanceType = lens _aiInstanceType (\ s a -> s{_aiInstanceType = a});

-- | The ID of the Spot Instance request.
aiSpotInstanceRequestId :: Lens' ActiveInstance (Maybe Text)
aiSpotInstanceRequestId = lens _aiSpotInstanceRequestId (\ s a -> s{_aiSpotInstanceRequestId = a});

instance FromXML ActiveInstance where
        parseXML x
          = ActiveInstance' <$>
              (x .@? "instanceId") <*> (x .@? "instanceType") <*>
                (x .@? "spotInstanceRequestId")

-- | Describes an Elastic IP address.
--
-- /See:/ 'address' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'addInstanceId'
--
-- * 'addAssociationId'
--
-- * 'addNetworkInterfaceOwnerId'
--
-- * 'addAllocationId'
--
-- * 'addDomain'
--
-- * 'addNetworkInterfaceId'
--
-- * 'addPrivateIPAddress'
--
-- * 'addPublicIP'
data Address = Address'{_addInstanceId :: Maybe Text, _addAssociationId :: Maybe Text, _addNetworkInterfaceOwnerId :: Maybe Text, _addAllocationId :: Maybe Text, _addDomain :: Maybe DomainType, _addNetworkInterfaceId :: Maybe Text, _addPrivateIPAddress :: Maybe Text, _addPublicIP :: Maybe Text} deriving (Eq, Read, Show)

-- | 'Address' smart constructor.
address :: Address
address = Address'{_addInstanceId = Nothing, _addAssociationId = Nothing, _addNetworkInterfaceOwnerId = Nothing, _addAllocationId = Nothing, _addDomain = Nothing, _addNetworkInterfaceId = Nothing, _addPrivateIPAddress = Nothing, _addPublicIP = Nothing};

-- | The ID of the instance that the address is associated with (if any).
addInstanceId :: Lens' Address (Maybe Text)
addInstanceId = lens _addInstanceId (\ s a -> s{_addInstanceId = a});

-- | The ID representing the association of the address with an instance in a
-- VPC.
addAssociationId :: Lens' Address (Maybe Text)
addAssociationId = lens _addAssociationId (\ s a -> s{_addAssociationId = a});

-- | The ID of the AWS account that owns the network interface.
addNetworkInterfaceOwnerId :: Lens' Address (Maybe Text)
addNetworkInterfaceOwnerId = lens _addNetworkInterfaceOwnerId (\ s a -> s{_addNetworkInterfaceOwnerId = a});

-- | The ID representing the allocation of the address for use with EC2-VPC.
addAllocationId :: Lens' Address (Maybe Text)
addAllocationId = lens _addAllocationId (\ s a -> s{_addAllocationId = a});

-- | Indicates whether this Elastic IP address is for use with instances in
-- EC2-Classic (@standard@) or instances in a VPC (@vpc@).
addDomain :: Lens' Address (Maybe DomainType)
addDomain = lens _addDomain (\ s a -> s{_addDomain = a});

-- | The ID of the network interface.
addNetworkInterfaceId :: Lens' Address (Maybe Text)
addNetworkInterfaceId = lens _addNetworkInterfaceId (\ s a -> s{_addNetworkInterfaceId = a});

-- | The private IP address associated with the Elastic IP address.
addPrivateIPAddress :: Lens' Address (Maybe Text)
addPrivateIPAddress = lens _addPrivateIPAddress (\ s a -> s{_addPrivateIPAddress = a});

-- | The Elastic IP address.
addPublicIP :: Lens' Address (Maybe Text)
addPublicIP = lens _addPublicIP (\ s a -> s{_addPublicIP = a});

instance FromXML Address where
        parseXML x
          = Address' <$>
              (x .@? "instanceId") <*> (x .@? "associationId") <*>
                (x .@? "networkInterfaceOwnerId")
                <*> (x .@? "allocationId")
                <*> (x .@? "domain")
                <*> (x .@? "networkInterfaceId")
                <*> (x .@? "privateIpAddress")
                <*> (x .@? "publicIp")

-- | The value to use when a resource attribute accepts a Boolean value.
--
-- /See:/ 'attributeBooleanValue' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'abvValue'
newtype AttributeBooleanValue = AttributeBooleanValue'{_abvValue :: Maybe Bool} deriving (Eq, Read, Show)

-- | 'AttributeBooleanValue' smart constructor.
attributeBooleanValue :: AttributeBooleanValue
attributeBooleanValue = AttributeBooleanValue'{_abvValue = Nothing};

-- | Valid values are @true@ or @false@.
abvValue :: Lens' AttributeBooleanValue (Maybe Bool)
abvValue = lens _abvValue (\ s a -> s{_abvValue = a});

instance FromXML AttributeBooleanValue where
        parseXML x
          = AttributeBooleanValue' <$> (x .@? "value")

instance ToQuery AttributeBooleanValue where
        toQuery AttributeBooleanValue'{..}
          = mconcat ["Value" =: _abvValue]

-- | The value to use for a resource attribute.
--
-- /See:/ 'attributeValue' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'avValue'
newtype AttributeValue = AttributeValue'{_avValue :: Maybe Text} deriving (Eq, Read, Show)

-- | 'AttributeValue' smart constructor.
attributeValue :: AttributeValue
attributeValue = AttributeValue'{_avValue = Nothing};

-- | Valid values are case-sensitive and vary by action.
avValue :: Lens' AttributeValue (Maybe Text)
avValue = lens _avValue (\ s a -> s{_avValue = a});

instance FromXML AttributeValue where
        parseXML x = AttributeValue' <$> (x .@? "value")

instance ToQuery AttributeValue where
        toQuery AttributeValue'{..}
          = mconcat ["Value" =: _avValue]

-- | Describes an Availability Zone.
--
-- /See:/ 'availabilityZone' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'azRegionName'
--
-- * 'azState'
--
-- * 'azZoneName'
--
-- * 'azMessages'
data AvailabilityZone = AvailabilityZone'{_azRegionName :: Maybe Text, _azState :: Maybe AvailabilityZoneState, _azZoneName :: Maybe Text, _azMessages :: Maybe [AvailabilityZoneMessage]} deriving (Eq, Read, Show)

-- | 'AvailabilityZone' smart constructor.
availabilityZone :: AvailabilityZone
availabilityZone = AvailabilityZone'{_azRegionName = Nothing, _azState = Nothing, _azZoneName = Nothing, _azMessages = Nothing};

-- | The name of the region.
azRegionName :: Lens' AvailabilityZone (Maybe Text)
azRegionName = lens _azRegionName (\ s a -> s{_azRegionName = a});

-- | The state of the Availability Zone (@available@ | @impaired@ |
-- @unavailable@).
azState :: Lens' AvailabilityZone (Maybe AvailabilityZoneState)
azState = lens _azState (\ s a -> s{_azState = a});

-- | The name of the Availability Zone.
azZoneName :: Lens' AvailabilityZone (Maybe Text)
azZoneName = lens _azZoneName (\ s a -> s{_azZoneName = a});

-- | Any messages about the Availability Zone.
azMessages :: Lens' AvailabilityZone [AvailabilityZoneMessage]
azMessages = lens _azMessages (\ s a -> s{_azMessages = a}) . _Default;

instance FromXML AvailabilityZone where
        parseXML x
          = AvailabilityZone' <$>
              (x .@? "regionName") <*> (x .@? "zoneState") <*>
                (x .@? "zoneName")
                <*> (may (parseXMLList "item") x)

-- | Describes a message about an Availability Zone.
--
-- /See:/ 'availabilityZoneMessage' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'azmMessage'
newtype AvailabilityZoneMessage = AvailabilityZoneMessage'{_azmMessage :: Maybe Text} deriving (Eq, Read, Show)

-- | 'AvailabilityZoneMessage' smart constructor.
availabilityZoneMessage :: AvailabilityZoneMessage
availabilityZoneMessage = AvailabilityZoneMessage'{_azmMessage = Nothing};

-- | The message about the Availability Zone.
azmMessage :: Lens' AvailabilityZoneMessage (Maybe Text)
azmMessage = lens _azmMessage (\ s a -> s{_azmMessage = a});

instance FromXML AvailabilityZoneMessage where
        parseXML x
          = AvailabilityZoneMessage' <$> (x .@? "message")

-- | /See:/ 'blobAttributeValue' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bavValue'
newtype BlobAttributeValue = BlobAttributeValue'{_bavValue :: Maybe Base64} deriving (Eq, Read, Show)

-- | 'BlobAttributeValue' smart constructor.
blobAttributeValue :: BlobAttributeValue
blobAttributeValue = BlobAttributeValue'{_bavValue = Nothing};

-- | FIXME: Undocumented member.
bavValue :: Lens' BlobAttributeValue (Maybe Base64)
bavValue = lens _bavValue (\ s a -> s{_bavValue = a});

instance ToQuery BlobAttributeValue where
        toQuery BlobAttributeValue'{..}
          = mconcat ["Value" =: _bavValue]

-- | Describes a block device mapping.
--
-- /See:/ 'blockDeviceMapping' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bdmVirtualName'
--
-- * 'bdmNoDevice'
--
-- * 'bdmEBS'
--
-- * 'bdmDeviceName'
data BlockDeviceMapping = BlockDeviceMapping'{_bdmVirtualName :: Maybe Text, _bdmNoDevice :: Maybe Text, _bdmEBS :: Maybe EBSBlockDevice, _bdmDeviceName :: Text} deriving (Eq, Read, Show)

-- | 'BlockDeviceMapping' smart constructor.
blockDeviceMapping :: Text -> BlockDeviceMapping
blockDeviceMapping pDeviceName = BlockDeviceMapping'{_bdmVirtualName = Nothing, _bdmNoDevice = Nothing, _bdmEBS = Nothing, _bdmDeviceName = pDeviceName};

-- | The virtual device name (@ephemeral@N). Instance store volumes are
-- numbered starting from 0. An instance type with 2 available instance
-- store volumes can specify mappings for @ephemeral0@ and @ephemeral1@.The
-- number of available instance store volumes depends on the instance type.
-- After you connect to the instance, you must mount the volume.
--
-- Constraints: For M3 instances, you must specify instance store volumes
-- in the block device mapping for the instance. When you launch an M3
-- instance, we ignore any instance store volumes specified in the block
-- device mapping for the AMI.
bdmVirtualName :: Lens' BlockDeviceMapping (Maybe Text)
bdmVirtualName = lens _bdmVirtualName (\ s a -> s{_bdmVirtualName = a});

-- | Suppresses the specified device included in the block device mapping of
-- the AMI.
bdmNoDevice :: Lens' BlockDeviceMapping (Maybe Text)
bdmNoDevice = lens _bdmNoDevice (\ s a -> s{_bdmNoDevice = a});

-- | Parameters used to automatically set up EBS volumes when the instance is
-- launched.
bdmEBS :: Lens' BlockDeviceMapping (Maybe EBSBlockDevice)
bdmEBS = lens _bdmEBS (\ s a -> s{_bdmEBS = a});

-- | The device name exposed to the instance (for example, @\/dev\/sdh@ or
-- @xvdh@).
bdmDeviceName :: Lens' BlockDeviceMapping Text
bdmDeviceName = lens _bdmDeviceName (\ s a -> s{_bdmDeviceName = a});

instance FromXML BlockDeviceMapping where
        parseXML x
          = BlockDeviceMapping' <$>
              (x .@? "virtualName") <*> (x .@? "noDevice") <*>
                (x .@? "ebs")
                <*> (x .@ "deviceName")

instance ToQuery BlockDeviceMapping where
        toQuery BlockDeviceMapping'{..}
          = mconcat
              ["VirtualName" =: _bdmVirtualName,
               "NoDevice" =: _bdmNoDevice, "Ebs" =: _bdmEBS,
               "DeviceName" =: _bdmDeviceName]

-- | Describes a bundle task.
--
-- /See:/ 'bundleTask' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'btBundleTaskError'
--
-- * 'btBundleId'
--
-- * 'btInstanceId'
--
-- * 'btProgress'
--
-- * 'btStartTime'
--
-- * 'btState'
--
-- * 'btStorage'
--
-- * 'btUpdateTime'
data BundleTask = BundleTask'{_btBundleTaskError :: Maybe BundleTaskError, _btBundleId :: Text, _btInstanceId :: Text, _btProgress :: Text, _btStartTime :: ISO8601, _btState :: BundleTaskState, _btStorage :: Storage, _btUpdateTime :: ISO8601} deriving (Eq, Read, Show)

-- | 'BundleTask' smart constructor.
bundleTask :: Text -> Text -> Text -> UTCTime -> BundleTaskState -> Storage -> UTCTime -> BundleTask
bundleTask pBundleId pInstanceId pProgress pStartTime pState pStorage pUpdateTime = BundleTask'{_btBundleTaskError = Nothing, _btBundleId = pBundleId, _btInstanceId = pInstanceId, _btProgress = pProgress, _btStartTime = _Time # pStartTime, _btState = pState, _btStorage = pStorage, _btUpdateTime = _Time # pUpdateTime};

-- | If the task fails, a description of the error.
btBundleTaskError :: Lens' BundleTask (Maybe BundleTaskError)
btBundleTaskError = lens _btBundleTaskError (\ s a -> s{_btBundleTaskError = a});

-- | The ID of the bundle task.
btBundleId :: Lens' BundleTask Text
btBundleId = lens _btBundleId (\ s a -> s{_btBundleId = a});

-- | The ID of the instance associated with this bundle task.
btInstanceId :: Lens' BundleTask Text
btInstanceId = lens _btInstanceId (\ s a -> s{_btInstanceId = a});

-- | The level of task completion, as a percent (for example, 20%).
btProgress :: Lens' BundleTask Text
btProgress = lens _btProgress (\ s a -> s{_btProgress = a});

-- | The time this task started.
btStartTime :: Lens' BundleTask UTCTime
btStartTime = lens _btStartTime (\ s a -> s{_btStartTime = a}) . _Time;

-- | The state of the task.
btState :: Lens' BundleTask BundleTaskState
btState = lens _btState (\ s a -> s{_btState = a});

-- | The Amazon S3 storage locations.
btStorage :: Lens' BundleTask Storage
btStorage = lens _btStorage (\ s a -> s{_btStorage = a});

-- | The time of the most recent update for the task.
btUpdateTime :: Lens' BundleTask UTCTime
btUpdateTime = lens _btUpdateTime (\ s a -> s{_btUpdateTime = a}) . _Time;

instance FromXML BundleTask where
        parseXML x
          = BundleTask' <$>
              (x .@? "error") <*> (x .@ "bundleId") <*>
                (x .@ "instanceId")
                <*> (x .@ "progress")
                <*> (x .@ "startTime")
                <*> (x .@ "state")
                <*> (x .@ "storage")
                <*> (x .@ "updateTime")

-- | Describes an error for BundleInstance.
--
-- /See:/ 'bundleTaskError' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bteCode'
--
-- * 'bteMessage'
data BundleTaskError = BundleTaskError'{_bteCode :: Maybe Text, _bteMessage :: Maybe Text} deriving (Eq, Read, Show)

-- | 'BundleTaskError' smart constructor.
bundleTaskError :: BundleTaskError
bundleTaskError = BundleTaskError'{_bteCode = Nothing, _bteMessage = Nothing};

-- | The error code.
bteCode :: Lens' BundleTaskError (Maybe Text)
bteCode = lens _bteCode (\ s a -> s{_bteCode = a});

-- | The error message.
bteMessage :: Lens' BundleTaskError (Maybe Text)
bteMessage = lens _bteMessage (\ s a -> s{_bteMessage = a});

instance FromXML BundleTaskError where
        parseXML x
          = BundleTaskError' <$>
              (x .@? "code") <*> (x .@? "message")

-- | Describes a Spot fleet error.
--
-- /See:/ 'cancelSpotFleetRequestsError' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csfreCode'
--
-- * 'csfreMessage'
data CancelSpotFleetRequestsError = CancelSpotFleetRequestsError'{_csfreCode :: CancelBatchErrorCode, _csfreMessage :: Text} deriving (Eq, Read, Show)

-- | 'CancelSpotFleetRequestsError' smart constructor.
cancelSpotFleetRequestsError :: CancelBatchErrorCode -> Text -> CancelSpotFleetRequestsError
cancelSpotFleetRequestsError pCode pMessage = CancelSpotFleetRequestsError'{_csfreCode = pCode, _csfreMessage = pMessage};

-- | The error code.
csfreCode :: Lens' CancelSpotFleetRequestsError CancelBatchErrorCode
csfreCode = lens _csfreCode (\ s a -> s{_csfreCode = a});

-- | The description for the error code.
csfreMessage :: Lens' CancelSpotFleetRequestsError Text
csfreMessage = lens _csfreMessage (\ s a -> s{_csfreMessage = a});

instance FromXML CancelSpotFleetRequestsError where
        parseXML x
          = CancelSpotFleetRequestsError' <$>
              (x .@ "code") <*> (x .@ "message")

-- | Describes a Spot fleet request that was not successfully canceled.
--
-- /See:/ 'cancelSpotFleetRequestsErrorItem' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csfreiSpotFleetRequestId'
--
-- * 'csfreiError'
data CancelSpotFleetRequestsErrorItem = CancelSpotFleetRequestsErrorItem'{_csfreiSpotFleetRequestId :: Text, _csfreiError :: CancelSpotFleetRequestsError} deriving (Eq, Read, Show)

-- | 'CancelSpotFleetRequestsErrorItem' smart constructor.
cancelSpotFleetRequestsErrorItem :: Text -> CancelSpotFleetRequestsError -> CancelSpotFleetRequestsErrorItem
cancelSpotFleetRequestsErrorItem pSpotFleetRequestId pError = CancelSpotFleetRequestsErrorItem'{_csfreiSpotFleetRequestId = pSpotFleetRequestId, _csfreiError = pError};

-- | The ID of the Spot fleet request.
csfreiSpotFleetRequestId :: Lens' CancelSpotFleetRequestsErrorItem Text
csfreiSpotFleetRequestId = lens _csfreiSpotFleetRequestId (\ s a -> s{_csfreiSpotFleetRequestId = a});

-- | The error.
csfreiError :: Lens' CancelSpotFleetRequestsErrorItem CancelSpotFleetRequestsError
csfreiError = lens _csfreiError (\ s a -> s{_csfreiError = a});

instance FromXML CancelSpotFleetRequestsErrorItem
         where
        parseXML x
          = CancelSpotFleetRequestsErrorItem' <$>
              (x .@ "spotFleetRequestId") <*> (x .@ "error")

-- | Describes a Spot fleet request that was successfully canceled.
--
-- /See:/ 'cancelSpotFleetRequestsSuccessItem' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csfrsiSpotFleetRequestId'
--
-- * 'csfrsiCurrentSpotFleetRequestState'
--
-- * 'csfrsiPreviousSpotFleetRequestState'
data CancelSpotFleetRequestsSuccessItem = CancelSpotFleetRequestsSuccessItem'{_csfrsiSpotFleetRequestId :: Text, _csfrsiCurrentSpotFleetRequestState :: BatchState, _csfrsiPreviousSpotFleetRequestState :: BatchState} deriving (Eq, Read, Show)

-- | 'CancelSpotFleetRequestsSuccessItem' smart constructor.
cancelSpotFleetRequestsSuccessItem :: Text -> BatchState -> BatchState -> CancelSpotFleetRequestsSuccessItem
cancelSpotFleetRequestsSuccessItem pSpotFleetRequestId pCurrentSpotFleetRequestState pPreviousSpotFleetRequestState = CancelSpotFleetRequestsSuccessItem'{_csfrsiSpotFleetRequestId = pSpotFleetRequestId, _csfrsiCurrentSpotFleetRequestState = pCurrentSpotFleetRequestState, _csfrsiPreviousSpotFleetRequestState = pPreviousSpotFleetRequestState};

-- | The ID of the Spot fleet request.
csfrsiSpotFleetRequestId :: Lens' CancelSpotFleetRequestsSuccessItem Text
csfrsiSpotFleetRequestId = lens _csfrsiSpotFleetRequestId (\ s a -> s{_csfrsiSpotFleetRequestId = a});

-- | The current state of the Spot fleet request.
csfrsiCurrentSpotFleetRequestState :: Lens' CancelSpotFleetRequestsSuccessItem BatchState
csfrsiCurrentSpotFleetRequestState = lens _csfrsiCurrentSpotFleetRequestState (\ s a -> s{_csfrsiCurrentSpotFleetRequestState = a});

-- | The previous state of the Spot fleet request.
csfrsiPreviousSpotFleetRequestState :: Lens' CancelSpotFleetRequestsSuccessItem BatchState
csfrsiPreviousSpotFleetRequestState = lens _csfrsiPreviousSpotFleetRequestState (\ s a -> s{_csfrsiPreviousSpotFleetRequestState = a});

instance FromXML CancelSpotFleetRequestsSuccessItem
         where
        parseXML x
          = CancelSpotFleetRequestsSuccessItem' <$>
              (x .@ "spotFleetRequestId") <*>
                (x .@ "currentSpotFleetRequestState")
                <*> (x .@ "previousSpotFleetRequestState")

-- | Describes a request to cancel a Spot Instance.
--
-- /See:/ 'cancelledSpotInstanceRequest' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csirState'
--
-- * 'csirSpotInstanceRequestId'
data CancelledSpotInstanceRequest = CancelledSpotInstanceRequest'{_csirState :: Maybe CancelSpotInstanceRequestState, _csirSpotInstanceRequestId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'CancelledSpotInstanceRequest' smart constructor.
cancelledSpotInstanceRequest :: CancelledSpotInstanceRequest
cancelledSpotInstanceRequest = CancelledSpotInstanceRequest'{_csirState = Nothing, _csirSpotInstanceRequestId = Nothing};

-- | The state of the Spot Instance request.
csirState :: Lens' CancelledSpotInstanceRequest (Maybe CancelSpotInstanceRequestState)
csirState = lens _csirState (\ s a -> s{_csirState = a});

-- | The ID of the Spot Instance request.
csirSpotInstanceRequestId :: Lens' CancelledSpotInstanceRequest (Maybe Text)
csirSpotInstanceRequestId = lens _csirSpotInstanceRequestId (\ s a -> s{_csirSpotInstanceRequestId = a});

instance FromXML CancelledSpotInstanceRequest where
        parseXML x
          = CancelledSpotInstanceRequest' <$>
              (x .@? "state") <*> (x .@? "spotInstanceRequestId")

-- | Describes a linked EC2-Classic instance.
--
-- /See:/ 'classicLinkInstance' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cliInstanceId'
--
-- * 'cliGroups'
--
-- * 'cliVPCId'
--
-- * 'cliTags'
data ClassicLinkInstance = ClassicLinkInstance'{_cliInstanceId :: Maybe Text, _cliGroups :: Maybe [GroupIdentifier], _cliVPCId :: Maybe Text, _cliTags :: Maybe [Tag]} deriving (Eq, Read, Show)

-- | 'ClassicLinkInstance' smart constructor.
classicLinkInstance :: ClassicLinkInstance
classicLinkInstance = ClassicLinkInstance'{_cliInstanceId = Nothing, _cliGroups = Nothing, _cliVPCId = Nothing, _cliTags = Nothing};

-- | The ID of the instance.
cliInstanceId :: Lens' ClassicLinkInstance (Maybe Text)
cliInstanceId = lens _cliInstanceId (\ s a -> s{_cliInstanceId = a});

-- | A list of security groups.
cliGroups :: Lens' ClassicLinkInstance [GroupIdentifier]
cliGroups = lens _cliGroups (\ s a -> s{_cliGroups = a}) . _Default;

-- | The ID of the VPC.
cliVPCId :: Lens' ClassicLinkInstance (Maybe Text)
cliVPCId = lens _cliVPCId (\ s a -> s{_cliVPCId = a});

-- | Any tags assigned to the instance.
cliTags :: Lens' ClassicLinkInstance [Tag]
cliTags = lens _cliTags (\ s a -> s{_cliTags = a}) . _Default;

instance FromXML ClassicLinkInstance where
        parseXML x
          = ClassicLinkInstance' <$>
              (x .@? "instanceId") <*>
                (may (parseXMLList "item") x)
                <*> (x .@? "vpcId")
                <*> (may (parseXMLList "item") x)

-- | Describes the client-specific data.
--
-- /See:/ 'clientData' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdUploadStart'
--
-- * 'cdUploadSize'
--
-- * 'cdUploadEnd'
--
-- * 'cdComment'
data ClientData = ClientData'{_cdUploadStart :: Maybe ISO8601, _cdUploadSize :: Maybe Double, _cdUploadEnd :: Maybe ISO8601, _cdComment :: Maybe Text} deriving (Eq, Read, Show)

-- | 'ClientData' smart constructor.
clientData :: ClientData
clientData = ClientData'{_cdUploadStart = Nothing, _cdUploadSize = Nothing, _cdUploadEnd = Nothing, _cdComment = Nothing};

-- | The time that the disk upload starts.
cdUploadStart :: Lens' ClientData (Maybe UTCTime)
cdUploadStart = lens _cdUploadStart (\ s a -> s{_cdUploadStart = a}) . mapping _Time;

-- | The size of the uploaded disk image, in GiB.
cdUploadSize :: Lens' ClientData (Maybe Double)
cdUploadSize = lens _cdUploadSize (\ s a -> s{_cdUploadSize = a});

-- | The time that the disk upload ends.
cdUploadEnd :: Lens' ClientData (Maybe UTCTime)
cdUploadEnd = lens _cdUploadEnd (\ s a -> s{_cdUploadEnd = a}) . mapping _Time;

-- | A user-defined comment about the disk upload.
cdComment :: Lens' ClientData (Maybe Text)
cdComment = lens _cdComment (\ s a -> s{_cdComment = a});

instance ToQuery ClientData where
        toQuery ClientData'{..}
          = mconcat
              ["UploadStart" =: _cdUploadStart,
               "UploadSize" =: _cdUploadSize,
               "UploadEnd" =: _cdUploadEnd, "Comment" =: _cdComment]

-- | Describes a conversion task.
--
-- /See:/ 'conversionTask' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ctImportInstance'
--
-- * 'ctStatusMessage'
--
-- * 'ctImportVolume'
--
-- * 'ctExpirationTime'
--
-- * 'ctTags'
--
-- * 'ctConversionTaskId'
--
-- * 'ctState'
data ConversionTask = ConversionTask'{_ctImportInstance :: Maybe ImportInstanceTaskDetails, _ctStatusMessage :: Maybe Text, _ctImportVolume :: Maybe ImportVolumeTaskDetails, _ctExpirationTime :: Maybe Text, _ctTags :: Maybe [Tag], _ctConversionTaskId :: Text, _ctState :: ConversionTaskState} deriving (Eq, Read, Show)

-- | 'ConversionTask' smart constructor.
conversionTask :: Text -> ConversionTaskState -> ConversionTask
conversionTask pConversionTaskId pState = ConversionTask'{_ctImportInstance = Nothing, _ctStatusMessage = Nothing, _ctImportVolume = Nothing, _ctExpirationTime = Nothing, _ctTags = Nothing, _ctConversionTaskId = pConversionTaskId, _ctState = pState};

-- | If the task is for importing an instance, this contains information
-- about the import instance task.
ctImportInstance :: Lens' ConversionTask (Maybe ImportInstanceTaskDetails)
ctImportInstance = lens _ctImportInstance (\ s a -> s{_ctImportInstance = a});

-- | The status message related to the conversion task.
ctStatusMessage :: Lens' ConversionTask (Maybe Text)
ctStatusMessage = lens _ctStatusMessage (\ s a -> s{_ctStatusMessage = a});

-- | If the task is for importing a volume, this contains information about
-- the import volume task.
ctImportVolume :: Lens' ConversionTask (Maybe ImportVolumeTaskDetails)
ctImportVolume = lens _ctImportVolume (\ s a -> s{_ctImportVolume = a});

-- | The time when the task expires. If the upload isn\'t complete before the
-- expiration time, we automatically cancel the task.
ctExpirationTime :: Lens' ConversionTask (Maybe Text)
ctExpirationTime = lens _ctExpirationTime (\ s a -> s{_ctExpirationTime = a});

-- | Any tags assigned to the task.
ctTags :: Lens' ConversionTask [Tag]
ctTags = lens _ctTags (\ s a -> s{_ctTags = a}) . _Default;

-- | The ID of the conversion task.
ctConversionTaskId :: Lens' ConversionTask Text
ctConversionTaskId = lens _ctConversionTaskId (\ s a -> s{_ctConversionTaskId = a});

-- | The state of the conversion task.
ctState :: Lens' ConversionTask ConversionTaskState
ctState = lens _ctState (\ s a -> s{_ctState = a});

instance FromXML ConversionTask where
        parseXML x
          = ConversionTask' <$>
              (x .@? "importInstance") <*> (x .@? "statusMessage")
                <*> (x .@? "importVolume")
                <*> (x .@? "expirationTime")
                <*> (may (parseXMLList "item") x)
                <*> (x .@ "conversionTaskId")
                <*> (x .@ "state")

-- | Describes the user or group to be added or removed from the permissions
-- for a volume.
--
-- /See:/ 'createVolumePermission' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cvpGroup'
--
-- * 'cvpUserId'
data CreateVolumePermission = CreateVolumePermission'{_cvpGroup :: Maybe PermissionGroup, _cvpUserId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'CreateVolumePermission' smart constructor.
createVolumePermission :: CreateVolumePermission
createVolumePermission = CreateVolumePermission'{_cvpGroup = Nothing, _cvpUserId = Nothing};

-- | The specific group that is to be added or removed from a volume\'s list
-- of create volume permissions.
cvpGroup :: Lens' CreateVolumePermission (Maybe PermissionGroup)
cvpGroup = lens _cvpGroup (\ s a -> s{_cvpGroup = a});

-- | The specific AWS account ID that is to be added or removed from a
-- volume\'s list of create volume permissions.
cvpUserId :: Lens' CreateVolumePermission (Maybe Text)
cvpUserId = lens _cvpUserId (\ s a -> s{_cvpUserId = a});

instance FromXML CreateVolumePermission where
        parseXML x
          = CreateVolumePermission' <$>
              (x .@? "group") <*> (x .@? "userId")

instance ToQuery CreateVolumePermission where
        toQuery CreateVolumePermission'{..}
          = mconcat
              ["Group" =: _cvpGroup, "UserId" =: _cvpUserId]

-- | Describes modifications to the permissions for a volume.
--
-- /See:/ 'createVolumePermissionModifications' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cvpmRemove'
--
-- * 'cvpmAdd'
data CreateVolumePermissionModifications = CreateVolumePermissionModifications'{_cvpmRemove :: Maybe [CreateVolumePermission], _cvpmAdd :: Maybe [CreateVolumePermission]} deriving (Eq, Read, Show)

-- | 'CreateVolumePermissionModifications' smart constructor.
createVolumePermissionModifications :: CreateVolumePermissionModifications
createVolumePermissionModifications = CreateVolumePermissionModifications'{_cvpmRemove = Nothing, _cvpmAdd = Nothing};

-- | Removes a specific AWS account ID or group from a volume\'s list of
-- create volume permissions.
cvpmRemove :: Lens' CreateVolumePermissionModifications [CreateVolumePermission]
cvpmRemove = lens _cvpmRemove (\ s a -> s{_cvpmRemove = a}) . _Default;

-- | Adds a specific AWS account ID or group to a volume\'s list of create
-- volume permissions.
cvpmAdd :: Lens' CreateVolumePermissionModifications [CreateVolumePermission]
cvpmAdd = lens _cvpmAdd (\ s a -> s{_cvpmAdd = a}) . _Default;

instance ToQuery CreateVolumePermissionModifications
         where
        toQuery CreateVolumePermissionModifications'{..}
          = mconcat
              [toQuery (toQueryList "item" <$> _cvpmRemove),
               toQuery (toQueryList "item" <$> _cvpmAdd)]

-- | Describes a customer gateway.
--
-- /See:/ 'customerGateway' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cgTags'
--
-- * 'cgBGPASN'
--
-- * 'cgCustomerGatewayId'
--
-- * 'cgIPAddress'
--
-- * 'cgState'
--
-- * 'cgType'
data CustomerGateway = CustomerGateway'{_cgTags :: Maybe [Tag], _cgBGPASN :: Text, _cgCustomerGatewayId :: Text, _cgIPAddress :: Text, _cgState :: Text, _cgType :: Text} deriving (Eq, Read, Show)

-- | 'CustomerGateway' smart constructor.
customerGateway :: Text -> Text -> Text -> Text -> Text -> CustomerGateway
customerGateway pBGPASN pCustomerGatewayId pIPAddress pState pType = CustomerGateway'{_cgTags = Nothing, _cgBGPASN = pBGPASN, _cgCustomerGatewayId = pCustomerGatewayId, _cgIPAddress = pIPAddress, _cgState = pState, _cgType = pType};

-- | Any tags assigned to the customer gateway.
cgTags :: Lens' CustomerGateway [Tag]
cgTags = lens _cgTags (\ s a -> s{_cgTags = a}) . _Default;

-- | The customer gateway\'s Border Gateway Protocol (BGP) Autonomous System
-- Number (ASN).
cgBGPASN :: Lens' CustomerGateway Text
cgBGPASN = lens _cgBGPASN (\ s a -> s{_cgBGPASN = a});

-- | The ID of the customer gateway.
cgCustomerGatewayId :: Lens' CustomerGateway Text
cgCustomerGatewayId = lens _cgCustomerGatewayId (\ s a -> s{_cgCustomerGatewayId = a});

-- | The Internet-routable IP address of the customer gateway\'s outside
-- interface.
cgIPAddress :: Lens' CustomerGateway Text
cgIPAddress = lens _cgIPAddress (\ s a -> s{_cgIPAddress = a});

-- | The current state of the customer gateway
-- (@pending | available | deleting | deleted@).
cgState :: Lens' CustomerGateway Text
cgState = lens _cgState (\ s a -> s{_cgState = a});

-- | The type of VPN connection the customer gateway supports (@ipsec.1@).
cgType :: Lens' CustomerGateway Text
cgType = lens _cgType (\ s a -> s{_cgType = a});

instance FromXML CustomerGateway where
        parseXML x
          = CustomerGateway' <$>
              (may (parseXMLList "item") x) <*> (x .@ "bgpAsn") <*>
                (x .@ "customerGatewayId")
                <*> (x .@ "ipAddress")
                <*> (x .@ "state")
                <*> (x .@ "type")

-- | Describes a DHCP configuration option.
--
-- /See:/ 'dhcpConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcValues'
--
-- * 'dcKey'
data DHCPConfiguration = DHCPConfiguration'{_dcValues :: Maybe [AttributeValue], _dcKey :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DHCPConfiguration' smart constructor.
dhcpConfiguration :: DHCPConfiguration
dhcpConfiguration = DHCPConfiguration'{_dcValues = Nothing, _dcKey = Nothing};

-- | One or more values for the DHCP option.
dcValues :: Lens' DHCPConfiguration [AttributeValue]
dcValues = lens _dcValues (\ s a -> s{_dcValues = a}) . _Default;

-- | The name of a DHCP option.
dcKey :: Lens' DHCPConfiguration (Maybe Text)
dcKey = lens _dcKey (\ s a -> s{_dcKey = a});

instance FromXML DHCPConfiguration where
        parseXML x
          = DHCPConfiguration' <$>
              (may (parseXMLList "item") x) <*> (x .@? "key")

-- | Describes a set of DHCP options.
--
-- /See:/ 'dhcpOptions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'doDHCPConfigurations'
--
-- * 'doDHCPOptionsId'
--
-- * 'doTags'
data DHCPOptions = DHCPOptions'{_doDHCPConfigurations :: Maybe [DHCPConfiguration], _doDHCPOptionsId :: Maybe Text, _doTags :: Maybe [Tag]} deriving (Eq, Read, Show)

-- | 'DHCPOptions' smart constructor.
dhcpOptions :: DHCPOptions
dhcpOptions = DHCPOptions'{_doDHCPConfigurations = Nothing, _doDHCPOptionsId = Nothing, _doTags = Nothing};

-- | One or more DHCP options in the set.
doDHCPConfigurations :: Lens' DHCPOptions [DHCPConfiguration]
doDHCPConfigurations = lens _doDHCPConfigurations (\ s a -> s{_doDHCPConfigurations = a}) . _Default;

-- | The ID of the set of DHCP options.
doDHCPOptionsId :: Lens' DHCPOptions (Maybe Text)
doDHCPOptionsId = lens _doDHCPOptionsId (\ s a -> s{_doDHCPOptionsId = a});

-- | Any tags assigned to the DHCP options set.
doTags :: Lens' DHCPOptions [Tag]
doTags = lens _doTags (\ s a -> s{_doTags = a}) . _Default;

instance FromXML DHCPOptions where
        parseXML x
          = DHCPOptions' <$>
              (may (parseXMLList "item") x) <*>
                (x .@? "dhcpOptionsId")
                <*> (may (parseXMLList "item") x)

-- | Describes a disk image.
--
-- /See:/ 'diskImage' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diImage'
--
-- * 'diVolume'
--
-- * 'diDescription'
data DiskImage = DiskImage'{_diImage :: Maybe DiskImageDetail, _diVolume :: Maybe VolumeDetail, _diDescription :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DiskImage' smart constructor.
diskImage :: DiskImage
diskImage = DiskImage'{_diImage = Nothing, _diVolume = Nothing, _diDescription = Nothing};

-- | Information about the disk image.
diImage :: Lens' DiskImage (Maybe DiskImageDetail)
diImage = lens _diImage (\ s a -> s{_diImage = a});

-- | Information about the volume.
diVolume :: Lens' DiskImage (Maybe VolumeDetail)
diVolume = lens _diVolume (\ s a -> s{_diVolume = a});

-- | A description of the disk image.
diDescription :: Lens' DiskImage (Maybe Text)
diDescription = lens _diDescription (\ s a -> s{_diDescription = a});

instance ToQuery DiskImage where
        toQuery DiskImage'{..}
          = mconcat
              ["Image" =: _diImage, "Volume" =: _diVolume,
               "Description" =: _diDescription]

-- | Describes a disk image.
--
-- /See:/ 'diskImageDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'disChecksum'
--
-- * 'disFormat'
--
-- * 'disSize'
--
-- * 'disImportManifestURL'
data DiskImageDescription = DiskImageDescription'{_disChecksum :: Maybe Text, _disFormat :: DiskImageFormat, _disSize :: Integer, _disImportManifestURL :: Text} deriving (Eq, Read, Show)

-- | 'DiskImageDescription' smart constructor.
diskImageDescription :: DiskImageFormat -> Integer -> Text -> DiskImageDescription
diskImageDescription pFormat pSize pImportManifestURL = DiskImageDescription'{_disChecksum = Nothing, _disFormat = pFormat, _disSize = pSize, _disImportManifestURL = pImportManifestURL};

-- | The checksum computed for the disk image.
disChecksum :: Lens' DiskImageDescription (Maybe Text)
disChecksum = lens _disChecksum (\ s a -> s{_disChecksum = a});

-- | The disk image format.
disFormat :: Lens' DiskImageDescription DiskImageFormat
disFormat = lens _disFormat (\ s a -> s{_disFormat = a});

-- | The size of the disk image, in GiB.
disSize :: Lens' DiskImageDescription Integer
disSize = lens _disSize (\ s a -> s{_disSize = a});

-- | A presigned URL for the import manifest stored in Amazon S3. For
-- information about creating a presigned URL for an Amazon S3 object, read
-- the \"Query String Request Authentication Alternative\" section of the
-- <http://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html Authenticating REST Requests>
-- topic in the /Amazon Simple Storage Service Developer Guide/.
disImportManifestURL :: Lens' DiskImageDescription Text
disImportManifestURL = lens _disImportManifestURL (\ s a -> s{_disImportManifestURL = a});

instance FromXML DiskImageDescription where
        parseXML x
          = DiskImageDescription' <$>
              (x .@? "checksum") <*> (x .@ "format") <*>
                (x .@ "size")
                <*> (x .@ "importManifestUrl")

-- | Describes a disk image.
--
-- /See:/ 'diskImageDetail' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'didFormat'
--
-- * 'didBytes'
--
-- * 'didImportManifestURL'
data DiskImageDetail = DiskImageDetail'{_didFormat :: DiskImageFormat, _didBytes :: Integer, _didImportManifestURL :: Text} deriving (Eq, Read, Show)

-- | 'DiskImageDetail' smart constructor.
diskImageDetail :: DiskImageFormat -> Integer -> Text -> DiskImageDetail
diskImageDetail pFormat pBytes pImportManifestURL = DiskImageDetail'{_didFormat = pFormat, _didBytes = pBytes, _didImportManifestURL = pImportManifestURL};

-- | The disk image format.
didFormat :: Lens' DiskImageDetail DiskImageFormat
didFormat = lens _didFormat (\ s a -> s{_didFormat = a});

-- | The size of the disk image, in GiB.
didBytes :: Lens' DiskImageDetail Integer
didBytes = lens _didBytes (\ s a -> s{_didBytes = a});

-- | A presigned URL for the import manifest stored in Amazon S3 and
-- presented here as an Amazon S3 presigned URL. For information about
-- creating a presigned URL for an Amazon S3 object, read the \"Query
-- String Request Authentication Alternative\" section of the
-- <http://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html Authenticating REST Requests>
-- topic in the /Amazon Simple Storage Service Developer Guide/.
didImportManifestURL :: Lens' DiskImageDetail Text
didImportManifestURL = lens _didImportManifestURL (\ s a -> s{_didImportManifestURL = a});

instance ToQuery DiskImageDetail where
        toQuery DiskImageDetail'{..}
          = mconcat
              ["Format" =: _didFormat, "Bytes" =: _didBytes,
               "ImportManifestUrl" =: _didImportManifestURL]

-- | Describes a disk image volume.
--
-- /See:/ 'diskImageVolumeDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'divdSize'
--
-- * 'divdId'
data DiskImageVolumeDescription = DiskImageVolumeDescription'{_divdSize :: Maybe Integer, _divdId :: Text} deriving (Eq, Read, Show)

-- | 'DiskImageVolumeDescription' smart constructor.
diskImageVolumeDescription :: Text -> DiskImageVolumeDescription
diskImageVolumeDescription pId = DiskImageVolumeDescription'{_divdSize = Nothing, _divdId = pId};

-- | The size of the volume, in GiB.
divdSize :: Lens' DiskImageVolumeDescription (Maybe Integer)
divdSize = lens _divdSize (\ s a -> s{_divdSize = a});

-- | The volume identifier.
divdId :: Lens' DiskImageVolumeDescription Text
divdId = lens _divdId (\ s a -> s{_divdId = a});

instance FromXML DiskImageVolumeDescription where
        parseXML x
          = DiskImageVolumeDescription' <$>
              (x .@? "size") <*> (x .@ "id")

-- | Describes a block device for an EBS volume.
--
-- /See:/ 'ebsBlockDevice' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ebdDeleteOnTermination'
--
-- * 'ebdVolumeSize'
--
-- * 'ebdIOPS'
--
-- * 'ebdEncrypted'
--
-- * 'ebdVolumeType'
--
-- * 'ebdSnapshotId'
data EBSBlockDevice = EBSBlockDevice'{_ebdDeleteOnTermination :: Maybe Bool, _ebdVolumeSize :: Maybe Int, _ebdIOPS :: Maybe Int, _ebdEncrypted :: Maybe Bool, _ebdVolumeType :: Maybe VolumeType, _ebdSnapshotId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'EBSBlockDevice' smart constructor.
ebsBlockDevice :: EBSBlockDevice
ebsBlockDevice = EBSBlockDevice'{_ebdDeleteOnTermination = Nothing, _ebdVolumeSize = Nothing, _ebdIOPS = Nothing, _ebdEncrypted = Nothing, _ebdVolumeType = Nothing, _ebdSnapshotId = Nothing};

-- | Indicates whether the EBS volume is deleted on instance termination.
ebdDeleteOnTermination :: Lens' EBSBlockDevice (Maybe Bool)
ebdDeleteOnTermination = lens _ebdDeleteOnTermination (\ s a -> s{_ebdDeleteOnTermination = a});

-- | The size of the volume, in GiB.
--
-- Constraints: @1-1024@ for @standard@ volumes, @1-16384@ for @gp2@
-- volumes, and @4-16384@ for @io1@ volumes. If you specify a snapshot, the
-- volume size must be equal to or larger than the snapshot size.
--
-- Default: If you\'re creating the volume from a snapshot and don\'t
-- specify a volume size, the default is the snapshot size.
ebdVolumeSize :: Lens' EBSBlockDevice (Maybe Int)
ebdVolumeSize = lens _ebdVolumeSize (\ s a -> s{_ebdVolumeSize = a});

-- | The number of I\/O operations per second (IOPS) that the volume
-- supports. For Provisioned IOPS (SSD) volumes, this represents the number
-- of IOPS that are provisioned for the volume. For General Purpose (SSD)
-- volumes, this represents the baseline performance of the volume and the
-- rate at which the volume accumulates I\/O credits for bursting. For more
-- information on General Purpose (SSD) baseline performance, I\/O credits,
-- and bursting, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- Constraint: Range is 100 to 20000 for Provisioned IOPS (SSD) volumes and
-- 3 to 10000 for General Purpose (SSD) volumes.
--
-- Condition: This parameter is required for requests to create @io1@
-- volumes; it is not used in requests to create @standard@ or @gp2@
-- volumes.
ebdIOPS :: Lens' EBSBlockDevice (Maybe Int)
ebdIOPS = lens _ebdIOPS (\ s a -> s{_ebdIOPS = a});

-- | Indicates whether the EBS volume is encrypted. Encrypted Amazon EBS
-- volumes may only be attached to instances that support Amazon EBS
-- encryption.
ebdEncrypted :: Lens' EBSBlockDevice (Maybe Bool)
ebdEncrypted = lens _ebdEncrypted (\ s a -> s{_ebdEncrypted = a});

-- | The volume type. @gp2@ for General Purpose (SSD) volumes, @io1@ for
-- Provisioned IOPS (SSD) volumes, and @standard@ for Magnetic volumes.
--
-- Default: @standard@
ebdVolumeType :: Lens' EBSBlockDevice (Maybe VolumeType)
ebdVolumeType = lens _ebdVolumeType (\ s a -> s{_ebdVolumeType = a});

-- | The ID of the snapshot.
ebdSnapshotId :: Lens' EBSBlockDevice (Maybe Text)
ebdSnapshotId = lens _ebdSnapshotId (\ s a -> s{_ebdSnapshotId = a});

instance FromXML EBSBlockDevice where
        parseXML x
          = EBSBlockDevice' <$>
              (x .@? "deleteOnTermination") <*>
                (x .@? "volumeSize")
                <*> (x .@? "iops")
                <*> (x .@? "encrypted")
                <*> (x .@? "volumeType")
                <*> (x .@? "snapshotId")

instance ToQuery EBSBlockDevice where
        toQuery EBSBlockDevice'{..}
          = mconcat
              ["DeleteOnTermination" =: _ebdDeleteOnTermination,
               "VolumeSize" =: _ebdVolumeSize, "Iops" =: _ebdIOPS,
               "Encrypted" =: _ebdEncrypted,
               "VolumeType" =: _ebdVolumeType,
               "SnapshotId" =: _ebdSnapshotId]

-- | Describes a parameter used to set up an EBS volume in a block device
-- mapping.
--
-- /See:/ 'ebsInstanceBlockDevice' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eibdDeleteOnTermination'
--
-- * 'eibdStatus'
--
-- * 'eibdVolumeId'
--
-- * 'eibdAttachTime'
data EBSInstanceBlockDevice = EBSInstanceBlockDevice'{_eibdDeleteOnTermination :: Maybe Bool, _eibdStatus :: Maybe AttachmentStatus, _eibdVolumeId :: Maybe Text, _eibdAttachTime :: Maybe ISO8601} deriving (Eq, Read, Show)

-- | 'EBSInstanceBlockDevice' smart constructor.
ebsInstanceBlockDevice :: EBSInstanceBlockDevice
ebsInstanceBlockDevice = EBSInstanceBlockDevice'{_eibdDeleteOnTermination = Nothing, _eibdStatus = Nothing, _eibdVolumeId = Nothing, _eibdAttachTime = Nothing};

-- | Indicates whether the volume is deleted on instance termination.
eibdDeleteOnTermination :: Lens' EBSInstanceBlockDevice (Maybe Bool)
eibdDeleteOnTermination = lens _eibdDeleteOnTermination (\ s a -> s{_eibdDeleteOnTermination = a});

-- | The attachment state.
eibdStatus :: Lens' EBSInstanceBlockDevice (Maybe AttachmentStatus)
eibdStatus = lens _eibdStatus (\ s a -> s{_eibdStatus = a});

-- | The ID of the EBS volume.
eibdVolumeId :: Lens' EBSInstanceBlockDevice (Maybe Text)
eibdVolumeId = lens _eibdVolumeId (\ s a -> s{_eibdVolumeId = a});

-- | The time stamp when the attachment initiated.
eibdAttachTime :: Lens' EBSInstanceBlockDevice (Maybe UTCTime)
eibdAttachTime = lens _eibdAttachTime (\ s a -> s{_eibdAttachTime = a}) . mapping _Time;

instance FromXML EBSInstanceBlockDevice where
        parseXML x
          = EBSInstanceBlockDevice' <$>
              (x .@? "deleteOnTermination") <*> (x .@? "status")
                <*> (x .@? "volumeId")
                <*> (x .@? "attachTime")

-- | /See:/ 'ebsInstanceBlockDeviceSpecification' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eibdsDeleteOnTermination'
--
-- * 'eibdsVolumeId'
data EBSInstanceBlockDeviceSpecification = EBSInstanceBlockDeviceSpecification'{_eibdsDeleteOnTermination :: Maybe Bool, _eibdsVolumeId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'EBSInstanceBlockDeviceSpecification' smart constructor.
ebsInstanceBlockDeviceSpecification :: EBSInstanceBlockDeviceSpecification
ebsInstanceBlockDeviceSpecification = EBSInstanceBlockDeviceSpecification'{_eibdsDeleteOnTermination = Nothing, _eibdsVolumeId = Nothing};

-- | Indicates whether the volume is deleted on instance termination.
eibdsDeleteOnTermination :: Lens' EBSInstanceBlockDeviceSpecification (Maybe Bool)
eibdsDeleteOnTermination = lens _eibdsDeleteOnTermination (\ s a -> s{_eibdsDeleteOnTermination = a});

-- | The ID of the EBS volume.
eibdsVolumeId :: Lens' EBSInstanceBlockDeviceSpecification (Maybe Text)
eibdsVolumeId = lens _eibdsVolumeId (\ s a -> s{_eibdsVolumeId = a});

instance ToQuery EBSInstanceBlockDeviceSpecification
         where
        toQuery EBSInstanceBlockDeviceSpecification'{..}
          = mconcat
              ["DeleteOnTermination" =: _eibdsDeleteOnTermination,
               "VolumeId" =: _eibdsVolumeId]

-- | Describes a Spot fleet event.
--
-- /See:/ 'eventInformation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eiInstanceId'
--
-- * 'eiEventDescription'
--
-- * 'eiEventSubType'
data EventInformation = EventInformation'{_eiInstanceId :: Maybe Text, _eiEventDescription :: Maybe Text, _eiEventSubType :: Maybe Text} deriving (Eq, Read, Show)

-- | 'EventInformation' smart constructor.
eventInformation :: EventInformation
eventInformation = EventInformation'{_eiInstanceId = Nothing, _eiEventDescription = Nothing, _eiEventSubType = Nothing};

-- | The ID of the instance. This information is available only for
-- @instanceChange@ events.
eiInstanceId :: Lens' EventInformation (Maybe Text)
eiInstanceId = lens _eiInstanceId (\ s a -> s{_eiInstanceId = a});

-- | The description of the event.
eiEventDescription :: Lens' EventInformation (Maybe Text)
eiEventDescription = lens _eiEventDescription (\ s a -> s{_eiEventDescription = a});

-- | The event.
--
-- The following are the @error@ events.
--
-- -   @iamFleetRoleInvalid@ - Spot fleet did not have the required
--     permissions either to launch or terminate an instance.
--
-- -   @spotFleetRequestConfigurationInvalid@ - The configuration is not
--     valid. For more information, see the description.
-- -   @spotInstanceCountLimitExceeded@ - You\'ve reached the limit on the
--     number of Spot Instances that you can launch.
--
-- The following are the @fleetRequestChange@ events.
--
-- -   @active@ - The Spot fleet has been validated and Amazon EC2 is
--     attempting to maintain the target number of running Spot Instances.
--
-- -   @cancelled@ - The Spot fleet is canceled and has no running Spot
--     Instances. The Spot fleet will be deleted two days after its
--     instances were terminated.
--
-- -   @cancelled_running@ - The Spot fleet is canceled and will not launch
--     additional Spot Instances, but its existing Spot Instances will
--     continue to run until they are interrupted or terminated.
--
-- -   @cancelled_terminating@ - The Spot fleet is canceled and its Spot
--     Instances are terminating.
--
-- -   @expired@ - The Spot fleet request has expired. A subsequent event
--     indicates that the instances were terminated, if the request was
--     created with @terminateInstancesWithExpiration@ set.
--
-- -   @price_update@ - The bid price for a launch configuration was
--     adjusted because it was too high. This change is permanent.
--
-- -   @submitted@ - The Spot fleet request is being evaluated and Amazon
--     EC2 is preparing to launch the target number of Spot Instances.
--
-- The following are the @instanceChange@ events.
--
-- -   @launched@ - A bid was fulfilled and a new instance was launched.
--
-- -   @terminated@ - An instance was terminated by the user.
--
eiEventSubType :: Lens' EventInformation (Maybe Text)
eiEventSubType = lens _eiEventSubType (\ s a -> s{_eiEventSubType = a});

instance FromXML EventInformation where
        parseXML x
          = EventInformation' <$>
              (x .@? "instanceId") <*> (x .@? "eventDescription")
                <*> (x .@? "eventSubType")

-- | Describes an instance export task.
--
-- /See:/ 'exportTask' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'etDescription'
--
-- * 'etExportTaskId'
--
-- * 'etExportToS3Task'
--
-- * 'etInstanceExportDetails'
--
-- * 'etState'
--
-- * 'etStatusMessage'
data ExportTask = ExportTask'{_etDescription :: Text, _etExportTaskId :: Text, _etExportToS3Task :: ExportToS3Task, _etInstanceExportDetails :: InstanceExportDetails, _etState :: ExportTaskState, _etStatusMessage :: Text} deriving (Eq, Read, Show)

-- | 'ExportTask' smart constructor.
exportTask :: Text -> Text -> ExportToS3Task -> InstanceExportDetails -> ExportTaskState -> Text -> ExportTask
exportTask pDescription pExportTaskId pExportToS3Task pInstanceExportDetails pState pStatusMessage = ExportTask'{_etDescription = pDescription, _etExportTaskId = pExportTaskId, _etExportToS3Task = pExportToS3Task, _etInstanceExportDetails = pInstanceExportDetails, _etState = pState, _etStatusMessage = pStatusMessage};

-- | A description of the resource being exported.
etDescription :: Lens' ExportTask Text
etDescription = lens _etDescription (\ s a -> s{_etDescription = a});

-- | The ID of the export task.
etExportTaskId :: Lens' ExportTask Text
etExportTaskId = lens _etExportTaskId (\ s a -> s{_etExportTaskId = a});

-- | Information about the export task.
etExportToS3Task :: Lens' ExportTask ExportToS3Task
etExportToS3Task = lens _etExportToS3Task (\ s a -> s{_etExportToS3Task = a});

-- | Information about the instance to export.
etInstanceExportDetails :: Lens' ExportTask InstanceExportDetails
etInstanceExportDetails = lens _etInstanceExportDetails (\ s a -> s{_etInstanceExportDetails = a});

-- | The state of the export task.
etState :: Lens' ExportTask ExportTaskState
etState = lens _etState (\ s a -> s{_etState = a});

-- | The status message related to the export task.
etStatusMessage :: Lens' ExportTask Text
etStatusMessage = lens _etStatusMessage (\ s a -> s{_etStatusMessage = a});

instance FromXML ExportTask where
        parseXML x
          = ExportTask' <$>
              (x .@ "description") <*> (x .@ "exportTaskId") <*>
                (x .@ "exportToS3")
                <*> (x .@ "instanceExport")
                <*> (x .@ "state")
                <*> (x .@ "statusMessage")

-- | Describes the format and location for an instance export task.
--
-- /See:/ 'exportToS3Task' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'etstS3Key'
--
-- * 'etstContainerFormat'
--
-- * 'etstS3Bucket'
--
-- * 'etstDiskImageFormat'
data ExportToS3Task = ExportToS3Task'{_etstS3Key :: Maybe Text, _etstContainerFormat :: Maybe ContainerFormat, _etstS3Bucket :: Maybe Text, _etstDiskImageFormat :: Maybe DiskImageFormat} deriving (Eq, Read, Show)

-- | 'ExportToS3Task' smart constructor.
exportToS3Task :: ExportToS3Task
exportToS3Task = ExportToS3Task'{_etstS3Key = Nothing, _etstContainerFormat = Nothing, _etstS3Bucket = Nothing, _etstDiskImageFormat = Nothing};

-- | The encryption key for your S3 bucket.
etstS3Key :: Lens' ExportToS3Task (Maybe Text)
etstS3Key = lens _etstS3Key (\ s a -> s{_etstS3Key = a});

-- | The container format used to combine disk images with metadata (such as
-- OVF). If absent, only the disk image is exported.
etstContainerFormat :: Lens' ExportToS3Task (Maybe ContainerFormat)
etstContainerFormat = lens _etstContainerFormat (\ s a -> s{_etstContainerFormat = a});

-- | The S3 bucket for the destination image. The destination bucket must
-- exist and grant WRITE and READ_ACP permissions to the AWS account
-- @vm-import-export\@amazon.com@.
etstS3Bucket :: Lens' ExportToS3Task (Maybe Text)
etstS3Bucket = lens _etstS3Bucket (\ s a -> s{_etstS3Bucket = a});

-- | The format for the exported image.
etstDiskImageFormat :: Lens' ExportToS3Task (Maybe DiskImageFormat)
etstDiskImageFormat = lens _etstDiskImageFormat (\ s a -> s{_etstDiskImageFormat = a});

instance FromXML ExportToS3Task where
        parseXML x
          = ExportToS3Task' <$>
              (x .@? "s3Key") <*> (x .@? "containerFormat") <*>
                (x .@? "s3Bucket")
                <*> (x .@? "diskImageFormat")

-- | Describes an instance export task.
--
-- /See:/ 'exportToS3TaskSpecification' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'etstsContainerFormat'
--
-- * 'etstsS3Prefix'
--
-- * 'etstsS3Bucket'
--
-- * 'etstsDiskImageFormat'
data ExportToS3TaskSpecification = ExportToS3TaskSpecification'{_etstsContainerFormat :: Maybe ContainerFormat, _etstsS3Prefix :: Maybe Text, _etstsS3Bucket :: Maybe Text, _etstsDiskImageFormat :: Maybe DiskImageFormat} deriving (Eq, Read, Show)

-- | 'ExportToS3TaskSpecification' smart constructor.
exportToS3TaskSpecification :: ExportToS3TaskSpecification
exportToS3TaskSpecification = ExportToS3TaskSpecification'{_etstsContainerFormat = Nothing, _etstsS3Prefix = Nothing, _etstsS3Bucket = Nothing, _etstsDiskImageFormat = Nothing};

-- | The container format used to combine disk images with metadata (such as
-- OVF). If absent, only the disk image is exported.
etstsContainerFormat :: Lens' ExportToS3TaskSpecification (Maybe ContainerFormat)
etstsContainerFormat = lens _etstsContainerFormat (\ s a -> s{_etstsContainerFormat = a});

-- | The image is written to a single object in the S3 bucket at the S3 key
-- s3prefix + exportTaskId + \'.\' + diskImageFormat.
etstsS3Prefix :: Lens' ExportToS3TaskSpecification (Maybe Text)
etstsS3Prefix = lens _etstsS3Prefix (\ s a -> s{_etstsS3Prefix = a});

-- | The S3 bucket for the destination image. The destination bucket must
-- exist and grant WRITE and READ_ACP permissions to the AWS account
-- @vm-import-export\@amazon.com@.
etstsS3Bucket :: Lens' ExportToS3TaskSpecification (Maybe Text)
etstsS3Bucket = lens _etstsS3Bucket (\ s a -> s{_etstsS3Bucket = a});

-- | The format for the exported image.
etstsDiskImageFormat :: Lens' ExportToS3TaskSpecification (Maybe DiskImageFormat)
etstsDiskImageFormat = lens _etstsDiskImageFormat (\ s a -> s{_etstsDiskImageFormat = a});

instance ToQuery ExportToS3TaskSpecification where
        toQuery ExportToS3TaskSpecification'{..}
          = mconcat
              ["ContainerFormat" =: _etstsContainerFormat,
               "S3Prefix" =: _etstsS3Prefix,
               "S3Bucket" =: _etstsS3Bucket,
               "DiskImageFormat" =: _etstsDiskImageFormat]

-- | A filter name and value pair that is used to return a more specific list
-- of results. Filters can be used to match a set of resources by various
-- criteria, such as tags, attributes, or IDs.
--
-- /See:/ 'filter'' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'filValues'
--
-- * 'filName'
data Filter = Filter'{_filValues :: Maybe [Text], _filName :: Text} deriving (Eq, Read, Show)

-- | 'Filter' smart constructor.
filter' :: Text -> Filter
filter' pName = Filter'{_filValues = Nothing, _filName = pName};

-- | One or more filter values. Filter values are case-sensitive.
filValues :: Lens' Filter [Text]
filValues = lens _filValues (\ s a -> s{_filValues = a}) . _Default;

-- | The name of the filter. Filter names are case-sensitive.
filName :: Lens' Filter Text
filName = lens _filName (\ s a -> s{_filName = a});

instance ToQuery Filter where
        toQuery Filter'{..}
          = mconcat
              [toQuery (toQueryList "item" <$> _filValues),
               "Name" =: _filName]

-- | Describes a flow log.
--
-- /See:/ 'flowLog' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'flCreationTime'
--
-- * 'flResourceId'
--
-- * 'flFlowLogStatus'
--
-- * 'flTrafficType'
--
-- * 'flDeliverLogsStatus'
--
-- * 'flDeliverLogsErrorMessage'
--
-- * 'flDeliverLogsPermissionARN'
--
-- * 'flLogGroupName'
--
-- * 'flFlowLogId'
data FlowLog = FlowLog'{_flCreationTime :: Maybe ISO8601, _flResourceId :: Maybe Text, _flFlowLogStatus :: Maybe Text, _flTrafficType :: Maybe TrafficType, _flDeliverLogsStatus :: Maybe Text, _flDeliverLogsErrorMessage :: Maybe Text, _flDeliverLogsPermissionARN :: Maybe Text, _flLogGroupName :: Maybe Text, _flFlowLogId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'FlowLog' smart constructor.
flowLog :: FlowLog
flowLog = FlowLog'{_flCreationTime = Nothing, _flResourceId = Nothing, _flFlowLogStatus = Nothing, _flTrafficType = Nothing, _flDeliverLogsStatus = Nothing, _flDeliverLogsErrorMessage = Nothing, _flDeliverLogsPermissionARN = Nothing, _flLogGroupName = Nothing, _flFlowLogId = Nothing};

-- | The date and time the flow log was created.
flCreationTime :: Lens' FlowLog (Maybe UTCTime)
flCreationTime = lens _flCreationTime (\ s a -> s{_flCreationTime = a}) . mapping _Time;

-- | The ID of the resource on which the flow log was created.
flResourceId :: Lens' FlowLog (Maybe Text)
flResourceId = lens _flResourceId (\ s a -> s{_flResourceId = a});

-- | The status of the flow log (@ACTIVE@).
flFlowLogStatus :: Lens' FlowLog (Maybe Text)
flFlowLogStatus = lens _flFlowLogStatus (\ s a -> s{_flFlowLogStatus = a});

-- | The type of traffic captured for the flow log.
flTrafficType :: Lens' FlowLog (Maybe TrafficType)
flTrafficType = lens _flTrafficType (\ s a -> s{_flTrafficType = a});

-- | The status of the logs delivery (@SUCCESS@ | @FAILED@).
flDeliverLogsStatus :: Lens' FlowLog (Maybe Text)
flDeliverLogsStatus = lens _flDeliverLogsStatus (\ s a -> s{_flDeliverLogsStatus = a});

-- | Information about the error that occurred. @Rate limited@ indicates that
-- CloudWatch logs throttling has been applied for one or more network
-- interfaces. @Access error@ indicates that the IAM role associated with
-- the flow log does not have sufficient permissions to publish to
-- CloudWatch Logs. @Unknown error@ indicates an internal error.
flDeliverLogsErrorMessage :: Lens' FlowLog (Maybe Text)
flDeliverLogsErrorMessage = lens _flDeliverLogsErrorMessage (\ s a -> s{_flDeliverLogsErrorMessage = a});

-- | The ARN of the IAM role that posts logs to CloudWatch Logs.
flDeliverLogsPermissionARN :: Lens' FlowLog (Maybe Text)
flDeliverLogsPermissionARN = lens _flDeliverLogsPermissionARN (\ s a -> s{_flDeliverLogsPermissionARN = a});

-- | The name of the flow log group.
flLogGroupName :: Lens' FlowLog (Maybe Text)
flLogGroupName = lens _flLogGroupName (\ s a -> s{_flLogGroupName = a});

-- | The flow log ID.
flFlowLogId :: Lens' FlowLog (Maybe Text)
flFlowLogId = lens _flFlowLogId (\ s a -> s{_flFlowLogId = a});

instance FromXML FlowLog where
        parseXML x
          = FlowLog' <$>
              (x .@? "creationTime") <*> (x .@? "resourceId") <*>
                (x .@? "flowLogStatus")
                <*> (x .@? "trafficType")
                <*> (x .@? "deliverLogsStatus")
                <*> (x .@? "deliverLogsErrorMessage")
                <*> (x .@? "deliverLogsPermissionArn")
                <*> (x .@? "logGroupName")
                <*> (x .@? "flowLogId")

-- | Describes a security group.
--
-- /See:/ 'groupIdentifier' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'giGroupId'
--
-- * 'giGroupName'
data GroupIdentifier = GroupIdentifier'{_giGroupId :: Maybe Text, _giGroupName :: Maybe Text} deriving (Eq, Read, Show)

-- | 'GroupIdentifier' smart constructor.
groupIdentifier :: GroupIdentifier
groupIdentifier = GroupIdentifier'{_giGroupId = Nothing, _giGroupName = Nothing};

-- | The ID of the security group.
giGroupId :: Lens' GroupIdentifier (Maybe Text)
giGroupId = lens _giGroupId (\ s a -> s{_giGroupId = a});

-- | The name of the security group.
giGroupName :: Lens' GroupIdentifier (Maybe Text)
giGroupName = lens _giGroupName (\ s a -> s{_giGroupName = a});

instance FromXML GroupIdentifier where
        parseXML x
          = GroupIdentifier' <$>
              (x .@? "groupId") <*> (x .@? "groupName")

instance ToQuery GroupIdentifier where
        toQuery GroupIdentifier'{..}
          = mconcat
              ["GroupId" =: _giGroupId,
               "GroupName" =: _giGroupName]

-- | Describes an event in the history of the Spot fleet request.
--
-- /See:/ 'historyRecord' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'hrTimestamp'
--
-- * 'hrEventType'
--
-- * 'hrEventInformation'
data HistoryRecord = HistoryRecord'{_hrTimestamp :: ISO8601, _hrEventType :: EventType, _hrEventInformation :: EventInformation} deriving (Eq, Read, Show)

-- | 'HistoryRecord' smart constructor.
historyRecord :: UTCTime -> EventType -> EventInformation -> HistoryRecord
historyRecord pTimestamp pEventType pEventInformation = HistoryRecord'{_hrTimestamp = _Time # pTimestamp, _hrEventType = pEventType, _hrEventInformation = pEventInformation};

-- | The date and time of the event, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
hrTimestamp :: Lens' HistoryRecord UTCTime
hrTimestamp = lens _hrTimestamp (\ s a -> s{_hrTimestamp = a}) . _Time;

-- | The event type.
--
-- -   @error@ - Indicates an error with the Spot fleet request.
--
-- -   @fleetRequestChange@ - Indicates a change in the status or
--     configuration of the Spot fleet request.
--
-- -   @instanceChange@ - Indicates that an instance was launched or
--     terminated.
--
hrEventType :: Lens' HistoryRecord EventType
hrEventType = lens _hrEventType (\ s a -> s{_hrEventType = a});

-- | Information about the event.
hrEventInformation :: Lens' HistoryRecord EventInformation
hrEventInformation = lens _hrEventInformation (\ s a -> s{_hrEventInformation = a});

instance FromXML HistoryRecord where
        parseXML x
          = HistoryRecord' <$>
              (x .@ "timestamp") <*> (x .@ "eventType") <*>
                (x .@ "eventInformation")

-- | Describes an IAM instance profile.
--
-- /See:/ 'iamInstanceProfile' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iapARN'
--
-- * 'iapId'
data IAMInstanceProfile = IAMInstanceProfile'{_iapARN :: Maybe Text, _iapId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'IAMInstanceProfile' smart constructor.
iamInstanceProfile :: IAMInstanceProfile
iamInstanceProfile = IAMInstanceProfile'{_iapARN = Nothing, _iapId = Nothing};

-- | The Amazon Resource Name (ARN) of the instance profile.
iapARN :: Lens' IAMInstanceProfile (Maybe Text)
iapARN = lens _iapARN (\ s a -> s{_iapARN = a});

-- | The ID of the instance profile.
iapId :: Lens' IAMInstanceProfile (Maybe Text)
iapId = lens _iapId (\ s a -> s{_iapId = a});

instance FromXML IAMInstanceProfile where
        parseXML x
          = IAMInstanceProfile' <$>
              (x .@? "arn") <*> (x .@? "id")

-- | Describes an IAM instance profile.
--
-- /See:/ 'iamInstanceProfileSpecification' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iapsARN'
--
-- * 'iapsName'
data IAMInstanceProfileSpecification = IAMInstanceProfileSpecification'{_iapsARN :: Maybe Text, _iapsName :: Maybe Text} deriving (Eq, Read, Show)

-- | 'IAMInstanceProfileSpecification' smart constructor.
iamInstanceProfileSpecification :: IAMInstanceProfileSpecification
iamInstanceProfileSpecification = IAMInstanceProfileSpecification'{_iapsARN = Nothing, _iapsName = Nothing};

-- | The Amazon Resource Name (ARN) of the instance profile.
iapsARN :: Lens' IAMInstanceProfileSpecification (Maybe Text)
iapsARN = lens _iapsARN (\ s a -> s{_iapsARN = a});

-- | The name of the instance profile.
iapsName :: Lens' IAMInstanceProfileSpecification (Maybe Text)
iapsName = lens _iapsName (\ s a -> s{_iapsName = a});

instance FromXML IAMInstanceProfileSpecification
         where
        parseXML x
          = IAMInstanceProfileSpecification' <$>
              (x .@? "arn") <*> (x .@? "name")

instance ToQuery IAMInstanceProfileSpecification
         where
        toQuery IAMInstanceProfileSpecification'{..}
          = mconcat ["Arn" =: _iapsARN, "Name" =: _iapsName]

-- | Describes the ICMP type and code.
--
-- /See:/ 'icmpTypeCode' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'itcCode'
--
-- * 'itcType'
data ICMPTypeCode = ICMPTypeCode'{_itcCode :: Maybe Int, _itcType :: Maybe Int} deriving (Eq, Read, Show)

-- | 'ICMPTypeCode' smart constructor.
icmpTypeCode :: ICMPTypeCode
icmpTypeCode = ICMPTypeCode'{_itcCode = Nothing, _itcType = Nothing};

-- | The ICMP type. A value of -1 means all types.
itcCode :: Lens' ICMPTypeCode (Maybe Int)
itcCode = lens _itcCode (\ s a -> s{_itcCode = a});

-- | The ICMP code. A value of -1 means all codes for the specified ICMP
-- type.
itcType :: Lens' ICMPTypeCode (Maybe Int)
itcType = lens _itcType (\ s a -> s{_itcType = a});

instance FromXML ICMPTypeCode where
        parseXML x
          = ICMPTypeCode' <$> (x .@? "code") <*> (x .@? "type")

instance ToQuery ICMPTypeCode where
        toQuery ICMPTypeCode'{..}
          = mconcat ["Code" =: _itcCode, "Type" =: _itcType]

-- | Describes a security group rule.
--
-- /See:/ 'ipPermission' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ipFromPort'
--
-- * 'ipUserIdGroupPairs'
--
-- * 'ipPrefixListIds'
--
-- * 'ipToPort'
--
-- * 'ipIPRanges'
--
-- * 'ipIPProtocol'
data IPPermission = IPPermission'{_ipFromPort :: Maybe Int, _ipUserIdGroupPairs :: Maybe [UserIdGroupPair], _ipPrefixListIds :: Maybe [PrefixListId], _ipToPort :: Maybe Int, _ipIPRanges :: Maybe [IPRange], _ipIPProtocol :: Text} deriving (Eq, Read, Show)

-- | 'IPPermission' smart constructor.
ipPermission :: Text -> IPPermission
ipPermission pIPProtocol = IPPermission'{_ipFromPort = Nothing, _ipUserIdGroupPairs = Nothing, _ipPrefixListIds = Nothing, _ipToPort = Nothing, _ipIPRanges = Nothing, _ipIPProtocol = pIPProtocol};

-- | The start of port range for the TCP and UDP protocols, or an ICMP type
-- number. A value of @-1@ indicates all ICMP types.
ipFromPort :: Lens' IPPermission (Maybe Int)
ipFromPort = lens _ipFromPort (\ s a -> s{_ipFromPort = a});

-- | One or more security group and AWS account ID pairs.
ipUserIdGroupPairs :: Lens' IPPermission [UserIdGroupPair]
ipUserIdGroupPairs = lens _ipUserIdGroupPairs (\ s a -> s{_ipUserIdGroupPairs = a}) . _Default;

-- | (Valid for AuthorizeSecurityGroupEgress, RevokeSecurityGroupEgress and
-- DescribeSecurityGroups only) One or more prefix list IDs for an AWS
-- service. In an AuthorizeSecurityGroupEgress request, this is the AWS
-- service that you want to access through a VPC endpoint from instances
-- associated with the security group.
ipPrefixListIds :: Lens' IPPermission [PrefixListId]
ipPrefixListIds = lens _ipPrefixListIds (\ s a -> s{_ipPrefixListIds = a}) . _Default;

-- | The end of port range for the TCP and UDP protocols, or an ICMP code. A
-- value of @-1@ indicates all ICMP codes for the specified ICMP type.
ipToPort :: Lens' IPPermission (Maybe Int)
ipToPort = lens _ipToPort (\ s a -> s{_ipToPort = a});

-- | One or more IP ranges.
ipIPRanges :: Lens' IPPermission [IPRange]
ipIPRanges = lens _ipIPRanges (\ s a -> s{_ipIPRanges = a}) . _Default;

-- | The protocol.
--
-- When you call DescribeSecurityGroups, the protocol value returned is the
-- number. Exception: For TCP, UDP, and ICMP, the value returned is the
-- name (for example, @tcp@, @udp@, or @icmp@). For a list of protocol
-- numbers, see
-- <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers>.
-- (VPC only) When you call AuthorizeSecurityGroupIngress, you can use @-1@
-- to specify all.
ipIPProtocol :: Lens' IPPermission Text
ipIPProtocol = lens _ipIPProtocol (\ s a -> s{_ipIPProtocol = a});

instance FromXML IPPermission where
        parseXML x
          = IPPermission' <$>
              (x .@? "fromPort") <*> (may (parseXMLList "item") x)
                <*> (may (parseXMLList "item") x)
                <*> (x .@? "toPort")
                <*> (may (parseXMLList "item") x)
                <*> (x .@ "ipProtocol")

instance ToQuery IPPermission where
        toQuery IPPermission'{..}
          = mconcat
              ["FromPort" =: _ipFromPort,
               toQuery (toQueryList "item" <$> _ipUserIdGroupPairs),
               toQuery (toQueryList "item" <$> _ipPrefixListIds),
               "ToPort" =: _ipToPort,
               toQuery (toQueryList "item" <$> _ipIPRanges),
               "IpProtocol" =: _ipIPProtocol]

-- | Describes an IP range.
--
-- /See:/ 'ipRange' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'irCIDRIP'
newtype IPRange = IPRange'{_irCIDRIP :: Text} deriving (Eq, Read, Show)

-- | 'IPRange' smart constructor.
ipRange :: Text -> IPRange
ipRange pCIDRIP = IPRange'{_irCIDRIP = pCIDRIP};

-- | The CIDR range. You can either specify a CIDR range or a source security
-- group, not both.
irCIDRIP :: Lens' IPRange Text
irCIDRIP = lens _irCIDRIP (\ s a -> s{_irCIDRIP = a});

instance FromXML IPRange where
        parseXML x = IPRange' <$> (x .@ "cidrIp")

instance ToQuery IPRange where
        toQuery IPRange'{..}
          = mconcat ["CidrIp" =: _irCIDRIP]

-- | Describes an image.
--
-- /See:/ 'image' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'imaPlatform'
--
-- * 'imaImageOwnerAlias'
--
-- * 'imaRAMDiskId'
--
-- * 'imaKernelId'
--
-- * 'imaRootDeviceName'
--
-- * 'imaSRIOVNetSupport'
--
-- * 'imaName'
--
-- * 'imaCreationDate'
--
-- * 'imaProductCodes'
--
-- * 'imaStateReason'
--
-- * 'imaBlockDeviceMappings'
--
-- * 'imaDescription'
--
-- * 'imaTags'
--
-- * 'imaImageId'
--
-- * 'imaImageLocation'
--
-- * 'imaState'
--
-- * 'imaOwnerId'
--
-- * 'imaPublic'
--
-- * 'imaArchitecture'
--
-- * 'imaImageType'
--
-- * 'imaRootDeviceType'
--
-- * 'imaVirtualizationType'
--
-- * 'imaHypervisor'
data Image = Image'{_imaPlatform :: Maybe PlatformValues, _imaImageOwnerAlias :: Maybe Text, _imaRAMDiskId :: Maybe Text, _imaKernelId :: Maybe Text, _imaRootDeviceName :: Maybe Text, _imaSRIOVNetSupport :: Maybe Text, _imaName :: Maybe Text, _imaCreationDate :: Maybe Text, _imaProductCodes :: Maybe [ProductCode], _imaStateReason :: Maybe StateReason, _imaBlockDeviceMappings :: Maybe [BlockDeviceMapping], _imaDescription :: Maybe Text, _imaTags :: Maybe [Tag], _imaImageId :: Text, _imaImageLocation :: Text, _imaState :: ImageState, _imaOwnerId :: Text, _imaPublic :: Bool, _imaArchitecture :: ArchitectureValues, _imaImageType :: ImageTypeValues, _imaRootDeviceType :: DeviceType, _imaVirtualizationType :: VirtualizationType, _imaHypervisor :: HypervisorType} deriving (Eq, Read, Show)

-- | 'Image' smart constructor.
image :: Text -> Text -> ImageState -> Text -> Bool -> ArchitectureValues -> ImageTypeValues -> DeviceType -> VirtualizationType -> HypervisorType -> Image
image pImageId pImageLocation pState pOwnerId pPublic pArchitecture pImageType pRootDeviceType pVirtualizationType pHypervisor = Image'{_imaPlatform = Nothing, _imaImageOwnerAlias = Nothing, _imaRAMDiskId = Nothing, _imaKernelId = Nothing, _imaRootDeviceName = Nothing, _imaSRIOVNetSupport = Nothing, _imaName = Nothing, _imaCreationDate = Nothing, _imaProductCodes = Nothing, _imaStateReason = Nothing, _imaBlockDeviceMappings = Nothing, _imaDescription = Nothing, _imaTags = Nothing, _imaImageId = pImageId, _imaImageLocation = pImageLocation, _imaState = pState, _imaOwnerId = pOwnerId, _imaPublic = pPublic, _imaArchitecture = pArchitecture, _imaImageType = pImageType, _imaRootDeviceType = pRootDeviceType, _imaVirtualizationType = pVirtualizationType, _imaHypervisor = pHypervisor};

-- | The value is @Windows@ for Windows AMIs; otherwise blank.
imaPlatform :: Lens' Image (Maybe PlatformValues)
imaPlatform = lens _imaPlatform (\ s a -> s{_imaPlatform = a});

-- | The AWS account alias (for example, @amazon@, @self@) or the AWS account
-- ID of the AMI owner.
imaImageOwnerAlias :: Lens' Image (Maybe Text)
imaImageOwnerAlias = lens _imaImageOwnerAlias (\ s a -> s{_imaImageOwnerAlias = a});

-- | The RAM disk associated with the image, if any. Only applicable for
-- machine images.
imaRAMDiskId :: Lens' Image (Maybe Text)
imaRAMDiskId = lens _imaRAMDiskId (\ s a -> s{_imaRAMDiskId = a});

-- | The kernel associated with the image, if any. Only applicable for
-- machine images.
imaKernelId :: Lens' Image (Maybe Text)
imaKernelId = lens _imaKernelId (\ s a -> s{_imaKernelId = a});

-- | The device name of the root device (for example, @\/dev\/sda1@ or
-- @\/dev\/xvda@).
imaRootDeviceName :: Lens' Image (Maybe Text)
imaRootDeviceName = lens _imaRootDeviceName (\ s a -> s{_imaRootDeviceName = a});

-- | Specifies whether enhanced networking is enabled.
imaSRIOVNetSupport :: Lens' Image (Maybe Text)
imaSRIOVNetSupport = lens _imaSRIOVNetSupport (\ s a -> s{_imaSRIOVNetSupport = a});

-- | The name of the AMI that was provided during image creation.
imaName :: Lens' Image (Maybe Text)
imaName = lens _imaName (\ s a -> s{_imaName = a});

-- | The date and time the image was created.
imaCreationDate :: Lens' Image (Maybe Text)
imaCreationDate = lens _imaCreationDate (\ s a -> s{_imaCreationDate = a});

-- | Any product codes associated with the AMI.
imaProductCodes :: Lens' Image [ProductCode]
imaProductCodes = lens _imaProductCodes (\ s a -> s{_imaProductCodes = a}) . _Default;

-- | The reason for the state change.
imaStateReason :: Lens' Image (Maybe StateReason)
imaStateReason = lens _imaStateReason (\ s a -> s{_imaStateReason = a});

-- | Any block device mapping entries.
imaBlockDeviceMappings :: Lens' Image [BlockDeviceMapping]
imaBlockDeviceMappings = lens _imaBlockDeviceMappings (\ s a -> s{_imaBlockDeviceMappings = a}) . _Default;

-- | The description of the AMI that was provided during image creation.
imaDescription :: Lens' Image (Maybe Text)
imaDescription = lens _imaDescription (\ s a -> s{_imaDescription = a});

-- | Any tags assigned to the image.
imaTags :: Lens' Image [Tag]
imaTags = lens _imaTags (\ s a -> s{_imaTags = a}) . _Default;

-- | The ID of the AMI.
imaImageId :: Lens' Image Text
imaImageId = lens _imaImageId (\ s a -> s{_imaImageId = a});

-- | The location of the AMI.
imaImageLocation :: Lens' Image Text
imaImageLocation = lens _imaImageLocation (\ s a -> s{_imaImageLocation = a});

-- | The current state of the AMI. If the state is @available@, the image is
-- successfully registered and can be used to launch an instance.
imaState :: Lens' Image ImageState
imaState = lens _imaState (\ s a -> s{_imaState = a});

-- | The AWS account ID of the image owner.
imaOwnerId :: Lens' Image Text
imaOwnerId = lens _imaOwnerId (\ s a -> s{_imaOwnerId = a});

-- | Indicates whether the image has public launch permissions. The value is
-- @true@ if this image has public launch permissions or @false@ if it has
-- only implicit and explicit launch permissions.
imaPublic :: Lens' Image Bool
imaPublic = lens _imaPublic (\ s a -> s{_imaPublic = a});

-- | The architecture of the image.
imaArchitecture :: Lens' Image ArchitectureValues
imaArchitecture = lens _imaArchitecture (\ s a -> s{_imaArchitecture = a});

-- | The type of image.
imaImageType :: Lens' Image ImageTypeValues
imaImageType = lens _imaImageType (\ s a -> s{_imaImageType = a});

-- | The type of root device used by the AMI. The AMI can use an EBS volume
-- or an instance store volume.
imaRootDeviceType :: Lens' Image DeviceType
imaRootDeviceType = lens _imaRootDeviceType (\ s a -> s{_imaRootDeviceType = a});

-- | The type of virtualization of the AMI.
imaVirtualizationType :: Lens' Image VirtualizationType
imaVirtualizationType = lens _imaVirtualizationType (\ s a -> s{_imaVirtualizationType = a});

-- | The hypervisor type of the image.
imaHypervisor :: Lens' Image HypervisorType
imaHypervisor = lens _imaHypervisor (\ s a -> s{_imaHypervisor = a});

instance FromXML Image where
        parseXML x
          = Image' <$>
              (x .@? "platform") <*> (x .@? "imageOwnerAlias") <*>
                (x .@? "ramdiskId")
                <*> (x .@? "kernelId")
                <*> (x .@? "rootDeviceName")
                <*> (x .@? "sriovNetSupport")
                <*> (x .@? "name")
                <*> (x .@? "creationDate")
                <*> (may (parseXMLList "item") x)
                <*> (x .@? "stateReason")
                <*> (may (parseXMLList "item") x)
                <*> (x .@? "description")
                <*> (may (parseXMLList "item") x)
                <*> (x .@ "imageId")
                <*> (x .@ "imageLocation")
                <*> (x .@ "imageState")
                <*> (x .@ "imageOwnerId")
                <*> (x .@ "isPublic")
                <*> (x .@ "architecture")
                <*> (x .@ "imageType")
                <*> (x .@ "rootDeviceType")
                <*> (x .@ "virtualizationType")
                <*> (x .@ "hypervisor")

-- | Describes the disk container object for an import image task.
--
-- /See:/ 'imageDiskContainer' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'idcURL'
--
-- * 'idcFormat'
--
-- * 'idcDeviceName'
--
-- * 'idcUserBucket'
--
-- * 'idcDescription'
--
-- * 'idcSnapshotId'
data ImageDiskContainer = ImageDiskContainer'{_idcURL :: Maybe Text, _idcFormat :: Maybe Text, _idcDeviceName :: Maybe Text, _idcUserBucket :: Maybe UserBucket, _idcDescription :: Maybe Text, _idcSnapshotId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'ImageDiskContainer' smart constructor.
imageDiskContainer :: ImageDiskContainer
imageDiskContainer = ImageDiskContainer'{_idcURL = Nothing, _idcFormat = Nothing, _idcDeviceName = Nothing, _idcUserBucket = Nothing, _idcDescription = Nothing, _idcSnapshotId = Nothing};

-- | The URL to the Amazon S3-based disk image being imported. The URL can
-- either be a https URL (https:\/\/..) or an Amazon S3 URL (s3:\/\/..)
idcURL :: Lens' ImageDiskContainer (Maybe Text)
idcURL = lens _idcURL (\ s a -> s{_idcURL = a});

-- | The format of the disk image being imported.
--
-- Valid values: @RAW@ | @VHD@ | @VMDK@ | @OVA@
idcFormat :: Lens' ImageDiskContainer (Maybe Text)
idcFormat = lens _idcFormat (\ s a -> s{_idcFormat = a});

-- | The block device mapping for the disk.
idcDeviceName :: Lens' ImageDiskContainer (Maybe Text)
idcDeviceName = lens _idcDeviceName (\ s a -> s{_idcDeviceName = a});

-- | The S3 bucket for the disk image.
idcUserBucket :: Lens' ImageDiskContainer (Maybe UserBucket)
idcUserBucket = lens _idcUserBucket (\ s a -> s{_idcUserBucket = a});

-- | The description of the disk image.
idcDescription :: Lens' ImageDiskContainer (Maybe Text)
idcDescription = lens _idcDescription (\ s a -> s{_idcDescription = a});

-- | The ID of the EBS snapshot to be used for importing the snapshot.
idcSnapshotId :: Lens' ImageDiskContainer (Maybe Text)
idcSnapshotId = lens _idcSnapshotId (\ s a -> s{_idcSnapshotId = a});

instance ToQuery ImageDiskContainer where
        toQuery ImageDiskContainer'{..}
          = mconcat
              ["Url" =: _idcURL, "Format" =: _idcFormat,
               "DeviceName" =: _idcDeviceName,
               "UserBucket" =: _idcUserBucket,
               "Description" =: _idcDescription,
               "SnapshotId" =: _idcSnapshotId]

-- | Describes an import image task.
--
-- /See:/ 'importImageTask' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iitStatus'
--
-- * 'iitHypervisor'
--
-- * 'iitPlatform'
--
-- * 'iitProgress'
--
-- * 'iitLicenseType'
--
-- * 'iitSnapshotDetails'
--
-- * 'iitStatusMessage'
--
-- * 'iitImageId'
--
-- * 'iitImportTaskId'
--
-- * 'iitArchitecture'
--
-- * 'iitDescription'
data ImportImageTask = ImportImageTask'{_iitStatus :: Maybe Text, _iitHypervisor :: Maybe Text, _iitPlatform :: Maybe Text, _iitProgress :: Maybe Text, _iitLicenseType :: Maybe Text, _iitSnapshotDetails :: Maybe [SnapshotDetail], _iitStatusMessage :: Maybe Text, _iitImageId :: Maybe Text, _iitImportTaskId :: Maybe Text, _iitArchitecture :: Maybe Text, _iitDescription :: Maybe Text} deriving (Eq, Read, Show)

-- | 'ImportImageTask' smart constructor.
importImageTask :: ImportImageTask
importImageTask = ImportImageTask'{_iitStatus = Nothing, _iitHypervisor = Nothing, _iitPlatform = Nothing, _iitProgress = Nothing, _iitLicenseType = Nothing, _iitSnapshotDetails = Nothing, _iitStatusMessage = Nothing, _iitImageId = Nothing, _iitImportTaskId = Nothing, _iitArchitecture = Nothing, _iitDescription = Nothing};

-- | A brief status for the import image task.
iitStatus :: Lens' ImportImageTask (Maybe Text)
iitStatus = lens _iitStatus (\ s a -> s{_iitStatus = a});

-- | The target hypervisor for the import task.
--
-- Valid values: @xen@
iitHypervisor :: Lens' ImportImageTask (Maybe Text)
iitHypervisor = lens _iitHypervisor (\ s a -> s{_iitHypervisor = a});

-- | The description string for the import image task.
iitPlatform :: Lens' ImportImageTask (Maybe Text)
iitPlatform = lens _iitPlatform (\ s a -> s{_iitPlatform = a});

-- | The percentage of progress of the import image task.
iitProgress :: Lens' ImportImageTask (Maybe Text)
iitProgress = lens _iitProgress (\ s a -> s{_iitProgress = a});

-- | The license type of the virtual machine.
iitLicenseType :: Lens' ImportImageTask (Maybe Text)
iitLicenseType = lens _iitLicenseType (\ s a -> s{_iitLicenseType = a});

-- | Information about the snapshots.
iitSnapshotDetails :: Lens' ImportImageTask [SnapshotDetail]
iitSnapshotDetails = lens _iitSnapshotDetails (\ s a -> s{_iitSnapshotDetails = a}) . _Default;

-- | A descriptive status message for the import image task.
iitStatusMessage :: Lens' ImportImageTask (Maybe Text)
iitStatusMessage = lens _iitStatusMessage (\ s a -> s{_iitStatusMessage = a});

-- | The ID of the Amazon Machine Image (AMI) of the imported virtual
-- machine.
iitImageId :: Lens' ImportImageTask (Maybe Text)
iitImageId = lens _iitImageId (\ s a -> s{_iitImageId = a});

-- | The ID of the import image task.
iitImportTaskId :: Lens' ImportImageTask (Maybe Text)
iitImportTaskId = lens _iitImportTaskId (\ s a -> s{_iitImportTaskId = a});

-- | The architecture of the virtual machine.
--
-- Valid values: @i386@ | @x86_64@
iitArchitecture :: Lens' ImportImageTask (Maybe Text)
iitArchitecture = lens _iitArchitecture (\ s a -> s{_iitArchitecture = a});

-- | A description of the import task.
iitDescription :: Lens' ImportImageTask (Maybe Text)
iitDescription = lens _iitDescription (\ s a -> s{_iitDescription = a});

instance FromXML ImportImageTask where
        parseXML x
          = ImportImageTask' <$>
              (x .@? "status") <*> (x .@? "hypervisor") <*>
                (x .@? "platform")
                <*> (x .@? "progress")
                <*> (x .@? "licenseType")
                <*> (may (parseXMLList "item") x)
                <*> (x .@? "statusMessage")
                <*> (x .@? "imageId")
                <*> (x .@? "importTaskId")
                <*> (x .@? "architecture")
                <*> (x .@? "description")

-- | Describes the launch specification for VM import.
--
-- /See:/ 'importInstanceLaunchSpecification' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iilsAdditionalInfo'
--
-- * 'iilsGroupNames'
--
-- * 'iilsSubnetId'
--
-- * 'iilsGroupIds'
--
-- * 'iilsInstanceType'
--
-- * 'iilsUserData'
--
-- * 'iilsMonitoring'
--
-- * 'iilsInstanceInitiatedShutdownBehavior'
--
-- * 'iilsPrivateIPAddress'
--
-- * 'iilsArchitecture'
--
-- * 'iilsPlacement'
data ImportInstanceLaunchSpecification = ImportInstanceLaunchSpecification'{_iilsAdditionalInfo :: Maybe Text, _iilsGroupNames :: Maybe [Text], _iilsSubnetId :: Maybe Text, _iilsGroupIds :: Maybe [Text], _iilsInstanceType :: Maybe InstanceType, _iilsUserData :: Maybe UserData, _iilsMonitoring :: Maybe Bool, _iilsInstanceInitiatedShutdownBehavior :: Maybe ShutdownBehavior, _iilsPrivateIPAddress :: Maybe Text, _iilsArchitecture :: Maybe ArchitectureValues, _iilsPlacement :: Maybe Placement} deriving (Eq, Read, Show)

-- | 'ImportInstanceLaunchSpecification' smart constructor.
importInstanceLaunchSpecification :: ImportInstanceLaunchSpecification
importInstanceLaunchSpecification = ImportInstanceLaunchSpecification'{_iilsAdditionalInfo = Nothing, _iilsGroupNames = Nothing, _iilsSubnetId = Nothing, _iilsGroupIds = Nothing, _iilsInstanceType = Nothing, _iilsUserData = Nothing, _iilsMonitoring = Nothing, _iilsInstanceInitiatedShutdownBehavior = Nothing, _iilsPrivateIPAddress = Nothing, _iilsArchitecture = Nothing, _iilsPlacement = Nothing};

-- | Reserved.
iilsAdditionalInfo :: Lens' ImportInstanceLaunchSpecification (Maybe Text)
iilsAdditionalInfo = lens _iilsAdditionalInfo (\ s a -> s{_iilsAdditionalInfo = a});

-- | One or more security group names.
iilsGroupNames :: Lens' ImportInstanceLaunchSpecification [Text]
iilsGroupNames = lens _iilsGroupNames (\ s a -> s{_iilsGroupNames = a}) . _Default;

-- | [EC2-VPC] The ID of the subnet in which to launch the instance.
iilsSubnetId :: Lens' ImportInstanceLaunchSpecification (Maybe Text)
iilsSubnetId = lens _iilsSubnetId (\ s a -> s{_iilsSubnetId = a});

-- | One or more security group IDs.
iilsGroupIds :: Lens' ImportInstanceLaunchSpecification [Text]
iilsGroupIds = lens _iilsGroupIds (\ s a -> s{_iilsGroupIds = a}) . _Default;

-- | The instance type. For more information about the instance types that
-- you can import, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/VMImportPrerequisites.html Before You Get Started>
-- in the Amazon Elastic Compute Cloud User Guide.
iilsInstanceType :: Lens' ImportInstanceLaunchSpecification (Maybe InstanceType)
iilsInstanceType = lens _iilsInstanceType (\ s a -> s{_iilsInstanceType = a});

-- | The Base64-encoded MIME user data to be made available to the instance.
iilsUserData :: Lens' ImportInstanceLaunchSpecification (Maybe UserData)
iilsUserData = lens _iilsUserData (\ s a -> s{_iilsUserData = a});

-- | Indicates whether monitoring is enabled.
iilsMonitoring :: Lens' ImportInstanceLaunchSpecification (Maybe Bool)
iilsMonitoring = lens _iilsMonitoring (\ s a -> s{_iilsMonitoring = a});

-- | Indicates whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for
-- system shutdown).
iilsInstanceInitiatedShutdownBehavior :: Lens' ImportInstanceLaunchSpecification (Maybe ShutdownBehavior)
iilsInstanceInitiatedShutdownBehavior = lens _iilsInstanceInitiatedShutdownBehavior (\ s a -> s{_iilsInstanceInitiatedShutdownBehavior = a});

-- | [EC2-VPC] An available IP address from the IP address range of the
-- subnet.
iilsPrivateIPAddress :: Lens' ImportInstanceLaunchSpecification (Maybe Text)
iilsPrivateIPAddress = lens _iilsPrivateIPAddress (\ s a -> s{_iilsPrivateIPAddress = a});

-- | The architecture of the instance.
iilsArchitecture :: Lens' ImportInstanceLaunchSpecification (Maybe ArchitectureValues)
iilsArchitecture = lens _iilsArchitecture (\ s a -> s{_iilsArchitecture = a});

-- | The placement information for the instance.
iilsPlacement :: Lens' ImportInstanceLaunchSpecification (Maybe Placement)
iilsPlacement = lens _iilsPlacement (\ s a -> s{_iilsPlacement = a});

instance ToQuery ImportInstanceLaunchSpecification
         where
        toQuery ImportInstanceLaunchSpecification'{..}
          = mconcat
              ["AdditionalInfo" =: _iilsAdditionalInfo,
               toQuery
                 (toQueryList "SecurityGroup" <$> _iilsGroupNames),
               "SubnetId" =: _iilsSubnetId,
               toQuery
                 (toQueryList "SecurityGroupId" <$> _iilsGroupIds),
               "InstanceType" =: _iilsInstanceType,
               "UserData" =: _iilsUserData,
               "Monitoring" =: _iilsMonitoring,
               "InstanceInitiatedShutdownBehavior" =:
                 _iilsInstanceInitiatedShutdownBehavior,
               "PrivateIpAddress" =: _iilsPrivateIPAddress,
               "Architecture" =: _iilsArchitecture,
               "Placement" =: _iilsPlacement]

-- | Describes an import instance task.
--
-- /See:/ 'importInstanceTaskDetails' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iitdInstanceId'
--
-- * 'iitdPlatform'
--
-- * 'iitdDescription'
--
-- * 'iitdVolumes'
data ImportInstanceTaskDetails = ImportInstanceTaskDetails'{_iitdInstanceId :: Maybe Text, _iitdPlatform :: Maybe PlatformValues, _iitdDescription :: Maybe Text, _iitdVolumes :: [ImportInstanceVolumeDetailItem]} deriving (Eq, Read, Show)

-- | 'ImportInstanceTaskDetails' smart constructor.
importInstanceTaskDetails :: ImportInstanceTaskDetails
importInstanceTaskDetails = ImportInstanceTaskDetails'{_iitdInstanceId = Nothing, _iitdPlatform = Nothing, _iitdDescription = Nothing, _iitdVolumes = mempty};

-- | The ID of the instance.
iitdInstanceId :: Lens' ImportInstanceTaskDetails (Maybe Text)
iitdInstanceId = lens _iitdInstanceId (\ s a -> s{_iitdInstanceId = a});

-- | The instance operating system.
iitdPlatform :: Lens' ImportInstanceTaskDetails (Maybe PlatformValues)
iitdPlatform = lens _iitdPlatform (\ s a -> s{_iitdPlatform = a});

-- | A description of the task.
iitdDescription :: Lens' ImportInstanceTaskDetails (Maybe Text)
iitdDescription = lens _iitdDescription (\ s a -> s{_iitdDescription = a});

-- | One or more volumes.
iitdVolumes :: Lens' ImportInstanceTaskDetails [ImportInstanceVolumeDetailItem]
iitdVolumes = lens _iitdVolumes (\ s a -> s{_iitdVolumes = a});

instance FromXML ImportInstanceTaskDetails where
        parseXML x
          = ImportInstanceTaskDetails' <$>
              (x .@? "instanceId") <*> (x .@? "platform") <*>
                (x .@? "description")
                <*> (parseXMLList "item" x)

-- | Describes an import volume task.
--
-- /See:/ 'importInstanceVolumeDetailItem' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iivdiStatusMessage'
--
-- * 'iivdiDescription'
--
-- * 'iivdiBytesConverted'
--
-- * 'iivdiAvailabilityZone'
--
-- * 'iivdiImage'
--
-- * 'iivdiVolume'
--
-- * 'iivdiStatus'
data ImportInstanceVolumeDetailItem = ImportInstanceVolumeDetailItem'{_iivdiStatusMessage :: Maybe Text, _iivdiDescription :: Maybe Text, _iivdiBytesConverted :: Integer, _iivdiAvailabilityZone :: Text, _iivdiImage :: DiskImageDescription, _iivdiVolume :: DiskImageVolumeDescription, _iivdiStatus :: Text} deriving (Eq, Read, Show)

-- | 'ImportInstanceVolumeDetailItem' smart constructor.
importInstanceVolumeDetailItem :: Integer -> Text -> DiskImageDescription -> DiskImageVolumeDescription -> Text -> ImportInstanceVolumeDetailItem
importInstanceVolumeDetailItem pBytesConverted pAvailabilityZone pImage pVolume pStatus = ImportInstanceVolumeDetailItem'{_iivdiStatusMessage = Nothing, _iivdiDescription = Nothing, _iivdiBytesConverted = pBytesConverted, _iivdiAvailabilityZone = pAvailabilityZone, _iivdiImage = pImage, _iivdiVolume = pVolume, _iivdiStatus = pStatus};

-- | The status information or errors related to the disk image.
iivdiStatusMessage :: Lens' ImportInstanceVolumeDetailItem (Maybe Text)
iivdiStatusMessage = lens _iivdiStatusMessage (\ s a -> s{_iivdiStatusMessage = a});

-- | A description of the task.
iivdiDescription :: Lens' ImportInstanceVolumeDetailItem (Maybe Text)
iivdiDescription = lens _iivdiDescription (\ s a -> s{_iivdiDescription = a});

-- | The number of bytes converted so far.
iivdiBytesConverted :: Lens' ImportInstanceVolumeDetailItem Integer
iivdiBytesConverted = lens _iivdiBytesConverted (\ s a -> s{_iivdiBytesConverted = a});

-- | The Availability Zone where the resulting instance will reside.
iivdiAvailabilityZone :: Lens' ImportInstanceVolumeDetailItem Text
iivdiAvailabilityZone = lens _iivdiAvailabilityZone (\ s a -> s{_iivdiAvailabilityZone = a});

-- | The image.
iivdiImage :: Lens' ImportInstanceVolumeDetailItem DiskImageDescription
iivdiImage = lens _iivdiImage (\ s a -> s{_iivdiImage = a});

-- | The volume.
iivdiVolume :: Lens' ImportInstanceVolumeDetailItem DiskImageVolumeDescription
iivdiVolume = lens _iivdiVolume (\ s a -> s{_iivdiVolume = a});

-- | The status of the import of this particular disk image.
iivdiStatus :: Lens' ImportInstanceVolumeDetailItem Text
iivdiStatus = lens _iivdiStatus (\ s a -> s{_iivdiStatus = a});

instance FromXML ImportInstanceVolumeDetailItem where
        parseXML x
          = ImportInstanceVolumeDetailItem' <$>
              (x .@? "statusMessage") <*> (x .@? "description") <*>
                (x .@ "bytesConverted")
                <*> (x .@ "availabilityZone")
                <*> (x .@ "image")
                <*> (x .@ "volume")
                <*> (x .@ "status")

-- | Describes an import snapshot task.
--
-- /See:/ 'importSnapshotTask' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'istSnapshotTaskDetail'
--
-- * 'istImportTaskId'
--
-- * 'istDescription'
data ImportSnapshotTask = ImportSnapshotTask'{_istSnapshotTaskDetail :: Maybe SnapshotTaskDetail, _istImportTaskId :: Maybe Text, _istDescription :: Maybe Text} deriving (Eq, Read, Show)

-- | 'ImportSnapshotTask' smart constructor.
importSnapshotTask :: ImportSnapshotTask
importSnapshotTask = ImportSnapshotTask'{_istSnapshotTaskDetail = Nothing, _istImportTaskId = Nothing, _istDescription = Nothing};

-- | Describes an import snapshot task.
istSnapshotTaskDetail :: Lens' ImportSnapshotTask (Maybe SnapshotTaskDetail)
istSnapshotTaskDetail = lens _istSnapshotTaskDetail (\ s a -> s{_istSnapshotTaskDetail = a});

-- | The ID of the import snapshot task.
istImportTaskId :: Lens' ImportSnapshotTask (Maybe Text)
istImportTaskId = lens _istImportTaskId (\ s a -> s{_istImportTaskId = a});

-- | A description of the import snapshot task.
istDescription :: Lens' ImportSnapshotTask (Maybe Text)
istDescription = lens _istDescription (\ s a -> s{_istDescription = a});

instance FromXML ImportSnapshotTask where
        parseXML x
          = ImportSnapshotTask' <$>
              (x .@? "snapshotTaskDetail") <*>
                (x .@? "importTaskId")
                <*> (x .@? "description")

-- | Describes an import volume task.
--
-- /See:/ 'importVolumeTaskDetails' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ivtdDescription'
--
-- * 'ivtdBytesConverted'
--
-- * 'ivtdAvailabilityZone'
--
-- * 'ivtdImage'
--
-- * 'ivtdVolume'
data ImportVolumeTaskDetails = ImportVolumeTaskDetails'{_ivtdDescription :: Maybe Text, _ivtdBytesConverted :: Integer, _ivtdAvailabilityZone :: Text, _ivtdImage :: DiskImageDescription, _ivtdVolume :: DiskImageVolumeDescription} deriving (Eq, Read, Show)

-- | 'ImportVolumeTaskDetails' smart constructor.
importVolumeTaskDetails :: Integer -> Text -> DiskImageDescription -> DiskImageVolumeDescription -> ImportVolumeTaskDetails
importVolumeTaskDetails pBytesConverted pAvailabilityZone pImage pVolume = ImportVolumeTaskDetails'{_ivtdDescription = Nothing, _ivtdBytesConverted = pBytesConverted, _ivtdAvailabilityZone = pAvailabilityZone, _ivtdImage = pImage, _ivtdVolume = pVolume};

-- | The description you provided when starting the import volume task.
ivtdDescription :: Lens' ImportVolumeTaskDetails (Maybe Text)
ivtdDescription = lens _ivtdDescription (\ s a -> s{_ivtdDescription = a});

-- | The number of bytes converted so far.
ivtdBytesConverted :: Lens' ImportVolumeTaskDetails Integer
ivtdBytesConverted = lens _ivtdBytesConverted (\ s a -> s{_ivtdBytesConverted = a});

-- | The Availability Zone where the resulting volume will reside.
ivtdAvailabilityZone :: Lens' ImportVolumeTaskDetails Text
ivtdAvailabilityZone = lens _ivtdAvailabilityZone (\ s a -> s{_ivtdAvailabilityZone = a});

-- | The image.
ivtdImage :: Lens' ImportVolumeTaskDetails DiskImageDescription
ivtdImage = lens _ivtdImage (\ s a -> s{_ivtdImage = a});

-- | The volume.
ivtdVolume :: Lens' ImportVolumeTaskDetails DiskImageVolumeDescription
ivtdVolume = lens _ivtdVolume (\ s a -> s{_ivtdVolume = a});

instance FromXML ImportVolumeTaskDetails where
        parseXML x
          = ImportVolumeTaskDetails' <$>
              (x .@? "description") <*> (x .@ "bytesConverted") <*>
                (x .@ "availabilityZone")
                <*> (x .@ "image")
                <*> (x .@ "volume")

-- | Describes an instance.
--
-- /See:/ 'instance'' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'insPublicDNSName'
--
-- * 'insPlatform'
--
-- * 'insSecurityGroups'
--
-- * 'insClientToken'
--
-- * 'insSourceDestCheck'
--
-- * 'insVPCId'
--
-- * 'insNetworkInterfaces'
--
-- * 'insKeyName'
--
-- * 'insRAMDiskId'
--
-- * 'insKernelId'
--
-- * 'insSubnetId'
--
-- * 'insRootDeviceName'
--
-- * 'insSRIOVNetSupport'
--
-- * 'insStateTransitionReason'
--
-- * 'insIAMInstanceProfile'
--
-- * 'insInstanceLifecycle'
--
-- * 'insPrivateIPAddress'
--
-- * 'insProductCodes'
--
-- * 'insSpotInstanceRequestId'
--
-- * 'insPrivateDNSName'
--
-- * 'insStateReason'
--
-- * 'insBlockDeviceMappings'
--
-- * 'insPublicIPAddress'
--
-- * 'insTags'
--
-- * 'insInstanceId'
--
-- * 'insImageId'
--
-- * 'insAMILaunchIndex'
--
-- * 'insInstanceType'
--
-- * 'insLaunchTime'
--
-- * 'insPlacement'
--
-- * 'insMonitoring'
--
-- * 'insArchitecture'
--
-- * 'insRootDeviceType'
--
-- * 'insVirtualizationType'
--
-- * 'insHypervisor'
--
-- * 'insEBSOptimized'
--
-- * 'insState'
data Instance = Instance'{_insPublicDNSName :: Maybe Text, _insPlatform :: Maybe PlatformValues, _insSecurityGroups :: Maybe [GroupIdentifier], _insClientToken :: Maybe Text, _insSourceDestCheck :: Maybe Bool, _insVPCId :: Maybe Text, _insNetworkInterfaces :: Maybe [InstanceNetworkInterface], _insKeyName :: Maybe Text, _insRAMDiskId :: Maybe Text, _insKernelId :: Maybe Text, _insSubnetId :: Maybe Text, _insRootDeviceName :: Maybe Text, _insSRIOVNetSupport :: Maybe Text, _insStateTransitionReason :: Maybe Text, _insIAMInstanceProfile :: Maybe IAMInstanceProfile, _insInstanceLifecycle :: Maybe InstanceLifecycleType, _insPrivateIPAddress :: Maybe Text, _insProductCodes :: Maybe [ProductCode], _insSpotInstanceRequestId :: Maybe Text, _insPrivateDNSName :: Maybe Text, _insStateReason :: Maybe StateReason, _insBlockDeviceMappings :: Maybe [InstanceBlockDeviceMapping], _insPublicIPAddress :: Maybe Text, _insTags :: Maybe [Tag], _insInstanceId :: Text, _insImageId :: Text, _insAMILaunchIndex :: Int, _insInstanceType :: InstanceType, _insLaunchTime :: ISO8601, _insPlacement :: Placement, _insMonitoring :: Monitoring, _insArchitecture :: ArchitectureValues, _insRootDeviceType :: DeviceType, _insVirtualizationType :: VirtualizationType, _insHypervisor :: HypervisorType, _insEBSOptimized :: Bool, _insState :: InstanceState} deriving (Eq, Read, Show)

-- | 'Instance' smart constructor.
instance' :: Text -> Text -> Int -> InstanceType -> UTCTime -> Placement -> Monitoring -> ArchitectureValues -> DeviceType -> VirtualizationType -> HypervisorType -> Bool -> InstanceState -> Instance
instance' pInstanceId pImageId pAMILaunchIndex pInstanceType pLaunchTime pPlacement pMonitoring pArchitecture pRootDeviceType pVirtualizationType pHypervisor pEBSOptimized pState = Instance'{_insPublicDNSName = Nothing, _insPlatform = Nothing, _insSecurityGroups = Nothing, _insClientToken = Nothing, _insSourceDestCheck = Nothing, _insVPCId = Nothing, _insNetworkInterfaces = Nothing, _insKeyName = Nothing, _insRAMDiskId = Nothing, _insKernelId = Nothing, _insSubnetId = Nothing, _insRootDeviceName = Nothing, _insSRIOVNetSupport = Nothing, _insStateTransitionReason = Nothing, _insIAMInstanceProfile = Nothing, _insInstanceLifecycle = Nothing, _insPrivateIPAddress = Nothing, _insProductCodes = Nothing, _insSpotInstanceRequestId = Nothing, _insPrivateDNSName = Nothing, _insStateReason = Nothing, _insBlockDeviceMappings = Nothing, _insPublicIPAddress = Nothing, _insTags = Nothing, _insInstanceId = pInstanceId, _insImageId = pImageId, _insAMILaunchIndex = pAMILaunchIndex, _insInstanceType = pInstanceType, _insLaunchTime = _Time # pLaunchTime, _insPlacement = pPlacement, _insMonitoring = pMonitoring, _insArchitecture = pArchitecture, _insRootDeviceType = pRootDeviceType, _insVirtualizationType = pVirtualizationType, _insHypervisor = pHypervisor, _insEBSOptimized = pEBSOptimized, _insState = pState};

-- | The public DNS name assigned to the instance. This name is not available
-- until the instance enters the @running@ state.
insPublicDNSName :: Lens' Instance (Maybe Text)
insPublicDNSName = lens _insPublicDNSName (\ s a -> s{_insPublicDNSName = a});

-- | The value is @Windows@ for Windows instances; otherwise blank.
insPlatform :: Lens' Instance (Maybe PlatformValues)
insPlatform = lens _insPlatform (\ s a -> s{_insPlatform = a});

-- | One or more security groups for the instance.
insSecurityGroups :: Lens' Instance [GroupIdentifier]
insSecurityGroups = lens _insSecurityGroups (\ s a -> s{_insSecurityGroups = a}) . _Default;

-- | The idempotency token you provided when you launched the instance.
insClientToken :: Lens' Instance (Maybe Text)
insClientToken = lens _insClientToken (\ s a -> s{_insClientToken = a});

-- | Specifies whether to enable an instance launched in a VPC to perform
-- NAT. This controls whether source\/destination checking is enabled on
-- the instance. A value of @true@ means checking is enabled, and @false@
-- means checking is disabled. The value must be @false@ for the instance
-- to perform NAT. For more information, see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_NAT_Instance.html NAT Instances>
-- in the /Amazon Virtual Private Cloud User Guide/.
insSourceDestCheck :: Lens' Instance (Maybe Bool)
insSourceDestCheck = lens _insSourceDestCheck (\ s a -> s{_insSourceDestCheck = a});

-- | The ID of the VPC in which the instance is running.
insVPCId :: Lens' Instance (Maybe Text)
insVPCId = lens _insVPCId (\ s a -> s{_insVPCId = a});

-- | [EC2-VPC] One or more network interfaces for the instance.
insNetworkInterfaces :: Lens' Instance [InstanceNetworkInterface]
insNetworkInterfaces = lens _insNetworkInterfaces (\ s a -> s{_insNetworkInterfaces = a}) . _Default;

-- | The name of the key pair, if this instance was launched with an
-- associated key pair.
insKeyName :: Lens' Instance (Maybe Text)
insKeyName = lens _insKeyName (\ s a -> s{_insKeyName = a});

-- | The RAM disk associated with this instance.
insRAMDiskId :: Lens' Instance (Maybe Text)
insRAMDiskId = lens _insRAMDiskId (\ s a -> s{_insRAMDiskId = a});

-- | The kernel associated with this instance.
insKernelId :: Lens' Instance (Maybe Text)
insKernelId = lens _insKernelId (\ s a -> s{_insKernelId = a});

-- | The ID of the subnet in which the instance is running.
insSubnetId :: Lens' Instance (Maybe Text)
insSubnetId = lens _insSubnetId (\ s a -> s{_insSubnetId = a});

-- | The root device name (for example, @\/dev\/sda1@ or @\/dev\/xvda@).
insRootDeviceName :: Lens' Instance (Maybe Text)
insRootDeviceName = lens _insRootDeviceName (\ s a -> s{_insRootDeviceName = a});

-- | Specifies whether enhanced networking is enabled.
insSRIOVNetSupport :: Lens' Instance (Maybe Text)
insSRIOVNetSupport = lens _insSRIOVNetSupport (\ s a -> s{_insSRIOVNetSupport = a});

-- | The reason for the most recent state transition. This might be an empty
-- string.
insStateTransitionReason :: Lens' Instance (Maybe Text)
insStateTransitionReason = lens _insStateTransitionReason (\ s a -> s{_insStateTransitionReason = a});

-- | The IAM instance profile associated with the instance.
insIAMInstanceProfile :: Lens' Instance (Maybe IAMInstanceProfile)
insIAMInstanceProfile = lens _insIAMInstanceProfile (\ s a -> s{_insIAMInstanceProfile = a});

-- | Indicates whether this is a Spot Instance.
insInstanceLifecycle :: Lens' Instance (Maybe InstanceLifecycleType)
insInstanceLifecycle = lens _insInstanceLifecycle (\ s a -> s{_insInstanceLifecycle = a});

-- | The private IP address assigned to the instance.
insPrivateIPAddress :: Lens' Instance (Maybe Text)
insPrivateIPAddress = lens _insPrivateIPAddress (\ s a -> s{_insPrivateIPAddress = a});

-- | The product codes attached to this instance.
insProductCodes :: Lens' Instance [ProductCode]
insProductCodes = lens _insProductCodes (\ s a -> s{_insProductCodes = a}) . _Default;

-- | The ID of the Spot Instance request.
insSpotInstanceRequestId :: Lens' Instance (Maybe Text)
insSpotInstanceRequestId = lens _insSpotInstanceRequestId (\ s a -> s{_insSpotInstanceRequestId = a});

-- | The private DNS name assigned to the instance. This DNS name can only be
-- used inside the Amazon EC2 network. This name is not available until the
-- instance enters the @running@ state.
insPrivateDNSName :: Lens' Instance (Maybe Text)
insPrivateDNSName = lens _insPrivateDNSName (\ s a -> s{_insPrivateDNSName = a});

-- | The reason for the most recent state transition.
insStateReason :: Lens' Instance (Maybe StateReason)
insStateReason = lens _insStateReason (\ s a -> s{_insStateReason = a});

-- | Any block device mapping entries for the instance.
insBlockDeviceMappings :: Lens' Instance [InstanceBlockDeviceMapping]
insBlockDeviceMappings = lens _insBlockDeviceMappings (\ s a -> s{_insBlockDeviceMappings = a}) . _Default;

-- | The public IP address assigned to the instance.
insPublicIPAddress :: Lens' Instance (Maybe Text)
insPublicIPAddress = lens _insPublicIPAddress (\ s a -> s{_insPublicIPAddress = a});

-- | Any tags assigned to the instance.
insTags :: Lens' Instance [Tag]
insTags = lens _insTags (\ s a -> s{_insTags = a}) . _Default;

-- | The ID of the instance.
insInstanceId :: Lens' Instance Text
insInstanceId = lens _insInstanceId (\ s a -> s{_insInstanceId = a});

-- | The ID of the AMI used to launch the instance.
insImageId :: Lens' Instance Text
insImageId = lens _insImageId (\ s a -> s{_insImageId = a});

-- | The AMI launch index, which can be used to find this instance in the
-- launch group.
insAMILaunchIndex :: Lens' Instance Int
insAMILaunchIndex = lens _insAMILaunchIndex (\ s a -> s{_insAMILaunchIndex = a});

-- | The instance type.
insInstanceType :: Lens' Instance InstanceType
insInstanceType = lens _insInstanceType (\ s a -> s{_insInstanceType = a});

-- | The time the instance was launched.
insLaunchTime :: Lens' Instance UTCTime
insLaunchTime = lens _insLaunchTime (\ s a -> s{_insLaunchTime = a}) . _Time;

-- | The location where the instance launched.
insPlacement :: Lens' Instance Placement
insPlacement = lens _insPlacement (\ s a -> s{_insPlacement = a});

-- | The monitoring information for the instance.
insMonitoring :: Lens' Instance Monitoring
insMonitoring = lens _insMonitoring (\ s a -> s{_insMonitoring = a});

-- | The architecture of the image.
insArchitecture :: Lens' Instance ArchitectureValues
insArchitecture = lens _insArchitecture (\ s a -> s{_insArchitecture = a});

-- | The root device type used by the AMI. The AMI can use an EBS volume or
-- an instance store volume.
insRootDeviceType :: Lens' Instance DeviceType
insRootDeviceType = lens _insRootDeviceType (\ s a -> s{_insRootDeviceType = a});

-- | The virtualization type of the instance.
insVirtualizationType :: Lens' Instance VirtualizationType
insVirtualizationType = lens _insVirtualizationType (\ s a -> s{_insVirtualizationType = a});

-- | The hypervisor type of the instance.
insHypervisor :: Lens' Instance HypervisorType
insHypervisor = lens _insHypervisor (\ s a -> s{_insHypervisor = a});

-- | Indicates whether the instance is optimized for EBS I\/O. This
-- optimization provides dedicated throughput to Amazon EBS and an
-- optimized configuration stack to provide optimal I\/O performance. This
-- optimization isn\'t available with all instance types. Additional usage
-- charges apply when using an EBS Optimized instance.
insEBSOptimized :: Lens' Instance Bool
insEBSOptimized = lens _insEBSOptimized (\ s a -> s{_insEBSOptimized = a});

-- | The current state of the instance.
insState :: Lens' Instance InstanceState
insState = lens _insState (\ s a -> s{_insState = a});

instance FromXML Instance where
        parseXML x
          = Instance' <$>
              (x .@? "dnsName") <*> (x .@? "platform") <*>
                (may (parseXMLList "item") x)
                <*> (x .@? "clientToken")
                <*> (x .@? "sourceDestCheck")
                <*> (x .@? "vpcId")
                <*> (may (parseXMLList "item") x)
                <*> (x .@? "keyName")
                <*> (x .@? "ramdiskId")
                <*> (x .@? "kernelId")
                <*> (x .@? "subnetId")
                <*> (x .@? "rootDeviceName")
                <*> (x .@? "sriovNetSupport")
                <*> (x .@? "reason")
                <*> (x .@? "iamInstanceProfile")
                <*> (x .@? "instanceLifecycle")
                <*> (x .@? "privateIpAddress")
                <*> (may (parseXMLList "item") x)
                <*> (x .@? "spotInstanceRequestId")
                <*> (x .@? "privateDnsName")
                <*> (x .@? "stateReason")
                <*> (may (parseXMLList "item") x)
                <*> (x .@? "ipAddress")
                <*> (may (parseXMLList "item") x)
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
                <*> (x .@ "ebsOptimized")
                <*> (x .@ "instanceState")

-- | Describes a block device mapping.
--
-- /See:/ 'instanceBlockDeviceMapping' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ibdmEBS'
--
-- * 'ibdmDeviceName'
data InstanceBlockDeviceMapping = InstanceBlockDeviceMapping'{_ibdmEBS :: Maybe EBSInstanceBlockDevice, _ibdmDeviceName :: Maybe Text} deriving (Eq, Read, Show)

-- | 'InstanceBlockDeviceMapping' smart constructor.
instanceBlockDeviceMapping :: InstanceBlockDeviceMapping
instanceBlockDeviceMapping = InstanceBlockDeviceMapping'{_ibdmEBS = Nothing, _ibdmDeviceName = Nothing};

-- | Parameters used to automatically set up EBS volumes when the instance is
-- launched.
ibdmEBS :: Lens' InstanceBlockDeviceMapping (Maybe EBSInstanceBlockDevice)
ibdmEBS = lens _ibdmEBS (\ s a -> s{_ibdmEBS = a});

-- | The device name exposed to the instance (for example, @\/dev\/sdh@ or
-- @xvdh@).
ibdmDeviceName :: Lens' InstanceBlockDeviceMapping (Maybe Text)
ibdmDeviceName = lens _ibdmDeviceName (\ s a -> s{_ibdmDeviceName = a});

instance FromXML InstanceBlockDeviceMapping where
        parseXML x
          = InstanceBlockDeviceMapping' <$>
              (x .@? "ebs") <*> (x .@? "deviceName")

-- | Describes a block device mapping entry.
--
-- /See:/ 'instanceBlockDeviceMappingSpecification' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ibdmsVirtualName'
--
-- * 'ibdmsNoDevice'
--
-- * 'ibdmsEBS'
--
-- * 'ibdmsDeviceName'
data InstanceBlockDeviceMappingSpecification = InstanceBlockDeviceMappingSpecification'{_ibdmsVirtualName :: Maybe Text, _ibdmsNoDevice :: Maybe Text, _ibdmsEBS :: Maybe EBSInstanceBlockDeviceSpecification, _ibdmsDeviceName :: Maybe Text} deriving (Eq, Read, Show)

-- | 'InstanceBlockDeviceMappingSpecification' smart constructor.
instanceBlockDeviceMappingSpecification :: InstanceBlockDeviceMappingSpecification
instanceBlockDeviceMappingSpecification = InstanceBlockDeviceMappingSpecification'{_ibdmsVirtualName = Nothing, _ibdmsNoDevice = Nothing, _ibdmsEBS = Nothing, _ibdmsDeviceName = Nothing};

-- | The virtual device name.
ibdmsVirtualName :: Lens' InstanceBlockDeviceMappingSpecification (Maybe Text)
ibdmsVirtualName = lens _ibdmsVirtualName (\ s a -> s{_ibdmsVirtualName = a});

-- | suppress the specified device included in the block device mapping.
ibdmsNoDevice :: Lens' InstanceBlockDeviceMappingSpecification (Maybe Text)
ibdmsNoDevice = lens _ibdmsNoDevice (\ s a -> s{_ibdmsNoDevice = a});

-- | Parameters used to automatically set up EBS volumes when the instance is
-- launched.
ibdmsEBS :: Lens' InstanceBlockDeviceMappingSpecification (Maybe EBSInstanceBlockDeviceSpecification)
ibdmsEBS = lens _ibdmsEBS (\ s a -> s{_ibdmsEBS = a});

-- | The device name exposed to the instance (for example, @\/dev\/sdh@ or
-- @xvdh@).
ibdmsDeviceName :: Lens' InstanceBlockDeviceMappingSpecification (Maybe Text)
ibdmsDeviceName = lens _ibdmsDeviceName (\ s a -> s{_ibdmsDeviceName = a});

instance ToQuery
         InstanceBlockDeviceMappingSpecification where
        toQuery InstanceBlockDeviceMappingSpecification'{..}
          = mconcat
              ["VirtualName" =: _ibdmsVirtualName,
               "NoDevice" =: _ibdmsNoDevice, "Ebs" =: _ibdmsEBS,
               "DeviceName" =: _ibdmsDeviceName]

-- | Describes a Reserved Instance listing state.
--
-- /See:/ 'instanceCount' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'icState'
--
-- * 'icInstanceCount'
data InstanceCount = InstanceCount'{_icState :: Maybe ListingState, _icInstanceCount :: Maybe Int} deriving (Eq, Read, Show)

-- | 'InstanceCount' smart constructor.
instanceCount :: InstanceCount
instanceCount = InstanceCount'{_icState = Nothing, _icInstanceCount = Nothing};

-- | The states of the listed Reserved Instances.
icState :: Lens' InstanceCount (Maybe ListingState)
icState = lens _icState (\ s a -> s{_icState = a});

-- | The number of listed Reserved Instances in the state specified by the
-- @state@.
icInstanceCount :: Lens' InstanceCount (Maybe Int)
icInstanceCount = lens _icInstanceCount (\ s a -> s{_icInstanceCount = a});

instance FromXML InstanceCount where
        parseXML x
          = InstanceCount' <$>
              (x .@? "state") <*> (x .@? "instanceCount")

-- | Describes an instance to export.
--
-- /See:/ 'instanceExportDetails' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iedInstanceId'
--
-- * 'iedTargetEnvironment'
data InstanceExportDetails = InstanceExportDetails'{_iedInstanceId :: Maybe Text, _iedTargetEnvironment :: Maybe ExportEnvironment} deriving (Eq, Read, Show)

-- | 'InstanceExportDetails' smart constructor.
instanceExportDetails :: InstanceExportDetails
instanceExportDetails = InstanceExportDetails'{_iedInstanceId = Nothing, _iedTargetEnvironment = Nothing};

-- | The ID of the resource being exported.
iedInstanceId :: Lens' InstanceExportDetails (Maybe Text)
iedInstanceId = lens _iedInstanceId (\ s a -> s{_iedInstanceId = a});

-- | The target virtualization environment.
iedTargetEnvironment :: Lens' InstanceExportDetails (Maybe ExportEnvironment)
iedTargetEnvironment = lens _iedTargetEnvironment (\ s a -> s{_iedTargetEnvironment = a});

instance FromXML InstanceExportDetails where
        parseXML x
          = InstanceExportDetails' <$>
              (x .@? "instanceId") <*> (x .@? "targetEnvironment")

-- | Describes the monitoring information of the instance.
--
-- /See:/ 'instanceMonitoring' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'imInstanceId'
--
-- * 'imMonitoring'
data InstanceMonitoring = InstanceMonitoring'{_imInstanceId :: Maybe Text, _imMonitoring :: Maybe Monitoring} deriving (Eq, Read, Show)

-- | 'InstanceMonitoring' smart constructor.
instanceMonitoring :: InstanceMonitoring
instanceMonitoring = InstanceMonitoring'{_imInstanceId = Nothing, _imMonitoring = Nothing};

-- | The ID of the instance.
imInstanceId :: Lens' InstanceMonitoring (Maybe Text)
imInstanceId = lens _imInstanceId (\ s a -> s{_imInstanceId = a});

-- | The monitoring information.
imMonitoring :: Lens' InstanceMonitoring (Maybe Monitoring)
imMonitoring = lens _imMonitoring (\ s a -> s{_imMonitoring = a});

instance FromXML InstanceMonitoring where
        parseXML x
          = InstanceMonitoring' <$>
              (x .@? "instanceId") <*> (x .@? "monitoring")

-- | Describes a network interface.
--
-- /See:/ 'instanceNetworkInterface' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iniPrivateIPAddresses'
--
-- * 'iniStatus'
--
-- * 'iniGroups'
--
-- * 'iniSourceDestCheck'
--
-- * 'iniVPCId'
--
-- * 'iniNetworkInterfaceId'
--
-- * 'iniSubnetId'
--
-- * 'iniAttachment'
--
-- * 'iniMACAddress'
--
-- * 'iniOwnerId'
--
-- * 'iniPrivateIPAddress'
--
-- * 'iniPrivateDNSName'
--
-- * 'iniDescription'
--
-- * 'iniAssociation'
data InstanceNetworkInterface = InstanceNetworkInterface'{_iniPrivateIPAddresses :: Maybe [InstancePrivateIPAddress], _iniStatus :: Maybe NetworkInterfaceStatus, _iniGroups :: Maybe [GroupIdentifier], _iniSourceDestCheck :: Maybe Bool, _iniVPCId :: Maybe Text, _iniNetworkInterfaceId :: Maybe Text, _iniSubnetId :: Maybe Text, _iniAttachment :: Maybe InstanceNetworkInterfaceAttachment, _iniMACAddress :: Maybe Text, _iniOwnerId :: Maybe Text, _iniPrivateIPAddress :: Maybe Text, _iniPrivateDNSName :: Maybe Text, _iniDescription :: Maybe Text, _iniAssociation :: Maybe InstanceNetworkInterfaceAssociation} deriving (Eq, Read, Show)

-- | 'InstanceNetworkInterface' smart constructor.
instanceNetworkInterface :: InstanceNetworkInterface
instanceNetworkInterface = InstanceNetworkInterface'{_iniPrivateIPAddresses = Nothing, _iniStatus = Nothing, _iniGroups = Nothing, _iniSourceDestCheck = Nothing, _iniVPCId = Nothing, _iniNetworkInterfaceId = Nothing, _iniSubnetId = Nothing, _iniAttachment = Nothing, _iniMACAddress = Nothing, _iniOwnerId = Nothing, _iniPrivateIPAddress = Nothing, _iniPrivateDNSName = Nothing, _iniDescription = Nothing, _iniAssociation = Nothing};

-- | The private IP addresses associated with the network interface.
iniPrivateIPAddresses :: Lens' InstanceNetworkInterface [InstancePrivateIPAddress]
iniPrivateIPAddresses = lens _iniPrivateIPAddresses (\ s a -> s{_iniPrivateIPAddresses = a}) . _Default;

-- | The status of the network interface.
iniStatus :: Lens' InstanceNetworkInterface (Maybe NetworkInterfaceStatus)
iniStatus = lens _iniStatus (\ s a -> s{_iniStatus = a});

-- | One or more security groups.
iniGroups :: Lens' InstanceNetworkInterface [GroupIdentifier]
iniGroups = lens _iniGroups (\ s a -> s{_iniGroups = a}) . _Default;

-- | Indicates whether to validate network traffic to or from this network
-- interface.
iniSourceDestCheck :: Lens' InstanceNetworkInterface (Maybe Bool)
iniSourceDestCheck = lens _iniSourceDestCheck (\ s a -> s{_iniSourceDestCheck = a});

-- | The ID of the VPC.
iniVPCId :: Lens' InstanceNetworkInterface (Maybe Text)
iniVPCId = lens _iniVPCId (\ s a -> s{_iniVPCId = a});

-- | The ID of the network interface.
iniNetworkInterfaceId :: Lens' InstanceNetworkInterface (Maybe Text)
iniNetworkInterfaceId = lens _iniNetworkInterfaceId (\ s a -> s{_iniNetworkInterfaceId = a});

-- | The ID of the subnet.
iniSubnetId :: Lens' InstanceNetworkInterface (Maybe Text)
iniSubnetId = lens _iniSubnetId (\ s a -> s{_iniSubnetId = a});

-- | The network interface attachment.
iniAttachment :: Lens' InstanceNetworkInterface (Maybe InstanceNetworkInterfaceAttachment)
iniAttachment = lens _iniAttachment (\ s a -> s{_iniAttachment = a});

-- | The MAC address.
iniMACAddress :: Lens' InstanceNetworkInterface (Maybe Text)
iniMACAddress = lens _iniMACAddress (\ s a -> s{_iniMACAddress = a});

-- | The ID of the AWS account that created the network interface.
iniOwnerId :: Lens' InstanceNetworkInterface (Maybe Text)
iniOwnerId = lens _iniOwnerId (\ s a -> s{_iniOwnerId = a});

-- | The IP address of the network interface within the subnet.
iniPrivateIPAddress :: Lens' InstanceNetworkInterface (Maybe Text)
iniPrivateIPAddress = lens _iniPrivateIPAddress (\ s a -> s{_iniPrivateIPAddress = a});

-- | The private DNS name.
iniPrivateDNSName :: Lens' InstanceNetworkInterface (Maybe Text)
iniPrivateDNSName = lens _iniPrivateDNSName (\ s a -> s{_iniPrivateDNSName = a});

-- | The description.
iniDescription :: Lens' InstanceNetworkInterface (Maybe Text)
iniDescription = lens _iniDescription (\ s a -> s{_iniDescription = a});

-- | The association information for an Elastic IP associated with the
-- network interface.
iniAssociation :: Lens' InstanceNetworkInterface (Maybe InstanceNetworkInterfaceAssociation)
iniAssociation = lens _iniAssociation (\ s a -> s{_iniAssociation = a});

instance FromXML InstanceNetworkInterface where
        parseXML x
          = InstanceNetworkInterface' <$>
              (may (parseXMLList "item") x) <*> (x .@? "status")
                <*> (may (parseXMLList "item") x)
                <*> (x .@? "sourceDestCheck")
                <*> (x .@? "vpcId")
                <*> (x .@? "networkInterfaceId")
                <*> (x .@? "subnetId")
                <*> (x .@? "attachment")
                <*> (x .@? "macAddress")
                <*> (x .@? "ownerId")
                <*> (x .@? "privateIpAddress")
                <*> (x .@? "privateDnsName")
                <*> (x .@? "description")
                <*> (x .@? "association")

-- | Describes association information for an Elastic IP address.
--
-- /See:/ 'instanceNetworkInterfaceAssociation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iniaPublicDNSName'
--
-- * 'iniaIPOwnerId'
--
-- * 'iniaPublicIP'
data InstanceNetworkInterfaceAssociation = InstanceNetworkInterfaceAssociation'{_iniaPublicDNSName :: Maybe Text, _iniaIPOwnerId :: Maybe Text, _iniaPublicIP :: Maybe Text} deriving (Eq, Read, Show)

-- | 'InstanceNetworkInterfaceAssociation' smart constructor.
instanceNetworkInterfaceAssociation :: InstanceNetworkInterfaceAssociation
instanceNetworkInterfaceAssociation = InstanceNetworkInterfaceAssociation'{_iniaPublicDNSName = Nothing, _iniaIPOwnerId = Nothing, _iniaPublicIP = Nothing};

-- | The public DNS name.
iniaPublicDNSName :: Lens' InstanceNetworkInterfaceAssociation (Maybe Text)
iniaPublicDNSName = lens _iniaPublicDNSName (\ s a -> s{_iniaPublicDNSName = a});

-- | The ID of the owner of the Elastic IP address.
iniaIPOwnerId :: Lens' InstanceNetworkInterfaceAssociation (Maybe Text)
iniaIPOwnerId = lens _iniaIPOwnerId (\ s a -> s{_iniaIPOwnerId = a});

-- | The public IP address or Elastic IP address bound to the network
-- interface.
iniaPublicIP :: Lens' InstanceNetworkInterfaceAssociation (Maybe Text)
iniaPublicIP = lens _iniaPublicIP (\ s a -> s{_iniaPublicIP = a});

instance FromXML InstanceNetworkInterfaceAssociation
         where
        parseXML x
          = InstanceNetworkInterfaceAssociation' <$>
              (x .@? "publicDnsName") <*> (x .@? "ipOwnerId") <*>
                (x .@? "publicIp")

-- | Describes a network interface attachment.
--
-- /See:/ 'instanceNetworkInterfaceAttachment' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iniaDeleteOnTermination'
--
-- * 'iniaStatus'
--
-- * 'iniaAttachmentId'
--
-- * 'iniaAttachTime'
--
-- * 'iniaDeviceIndex'
data InstanceNetworkInterfaceAttachment = InstanceNetworkInterfaceAttachment'{_iniaDeleteOnTermination :: Maybe Bool, _iniaStatus :: Maybe AttachmentStatus, _iniaAttachmentId :: Maybe Text, _iniaAttachTime :: Maybe ISO8601, _iniaDeviceIndex :: Maybe Int} deriving (Eq, Read, Show)

-- | 'InstanceNetworkInterfaceAttachment' smart constructor.
instanceNetworkInterfaceAttachment :: InstanceNetworkInterfaceAttachment
instanceNetworkInterfaceAttachment = InstanceNetworkInterfaceAttachment'{_iniaDeleteOnTermination = Nothing, _iniaStatus = Nothing, _iniaAttachmentId = Nothing, _iniaAttachTime = Nothing, _iniaDeviceIndex = Nothing};

-- | Indicates whether the network interface is deleted when the instance is
-- terminated.
iniaDeleteOnTermination :: Lens' InstanceNetworkInterfaceAttachment (Maybe Bool)
iniaDeleteOnTermination = lens _iniaDeleteOnTermination (\ s a -> s{_iniaDeleteOnTermination = a});

-- | The attachment state.
iniaStatus :: Lens' InstanceNetworkInterfaceAttachment (Maybe AttachmentStatus)
iniaStatus = lens _iniaStatus (\ s a -> s{_iniaStatus = a});

-- | The ID of the network interface attachment.
iniaAttachmentId :: Lens' InstanceNetworkInterfaceAttachment (Maybe Text)
iniaAttachmentId = lens _iniaAttachmentId (\ s a -> s{_iniaAttachmentId = a});

-- | The time stamp when the attachment initiated.
iniaAttachTime :: Lens' InstanceNetworkInterfaceAttachment (Maybe UTCTime)
iniaAttachTime = lens _iniaAttachTime (\ s a -> s{_iniaAttachTime = a}) . mapping _Time;

-- | The index of the device on the instance for the network interface
-- attachment.
iniaDeviceIndex :: Lens' InstanceNetworkInterfaceAttachment (Maybe Int)
iniaDeviceIndex = lens _iniaDeviceIndex (\ s a -> s{_iniaDeviceIndex = a});

instance FromXML InstanceNetworkInterfaceAttachment
         where
        parseXML x
          = InstanceNetworkInterfaceAttachment' <$>
              (x .@? "deleteOnTermination") <*> (x .@? "status")
                <*> (x .@? "attachmentId")
                <*> (x .@? "attachTime")
                <*> (x .@? "deviceIndex")

-- | Describes a network interface.
--
-- /See:/ 'instanceNetworkInterfaceSpecification' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'inisPrivateIPAddresses'
--
-- * 'inisDeleteOnTermination'
--
-- * 'inisGroups'
--
-- * 'inisAssociatePublicIPAddress'
--
-- * 'inisNetworkInterfaceId'
--
-- * 'inisSubnetId'
--
-- * 'inisPrivateIPAddress'
--
-- * 'inisSecondaryPrivateIPAddressCount'
--
-- * 'inisDeviceIndex'
--
-- * 'inisDescription'
data InstanceNetworkInterfaceSpecification = InstanceNetworkInterfaceSpecification'{_inisPrivateIPAddresses :: Maybe [PrivateIPAddressSpecification], _inisDeleteOnTermination :: Maybe Bool, _inisGroups :: Maybe [Text], _inisAssociatePublicIPAddress :: Maybe Bool, _inisNetworkInterfaceId :: Maybe Text, _inisSubnetId :: Maybe Text, _inisPrivateIPAddress :: Maybe Text, _inisSecondaryPrivateIPAddressCount :: Maybe Int, _inisDeviceIndex :: Maybe Int, _inisDescription :: Maybe Text} deriving (Eq, Read, Show)

-- | 'InstanceNetworkInterfaceSpecification' smart constructor.
instanceNetworkInterfaceSpecification :: InstanceNetworkInterfaceSpecification
instanceNetworkInterfaceSpecification = InstanceNetworkInterfaceSpecification'{_inisPrivateIPAddresses = Nothing, _inisDeleteOnTermination = Nothing, _inisGroups = Nothing, _inisAssociatePublicIPAddress = Nothing, _inisNetworkInterfaceId = Nothing, _inisSubnetId = Nothing, _inisPrivateIPAddress = Nothing, _inisSecondaryPrivateIPAddressCount = Nothing, _inisDeviceIndex = Nothing, _inisDescription = Nothing};

-- | One or more private IP addresses to assign to the network interface.
-- Only one private IP address can be designated as primary.
inisPrivateIPAddresses :: Lens' InstanceNetworkInterfaceSpecification [PrivateIPAddressSpecification]
inisPrivateIPAddresses = lens _inisPrivateIPAddresses (\ s a -> s{_inisPrivateIPAddresses = a}) . _Default;

-- | If set to @true@, the interface is deleted when the instance is
-- terminated. You can specify @true@ only if creating a new network
-- interface when launching an instance.
inisDeleteOnTermination :: Lens' InstanceNetworkInterfaceSpecification (Maybe Bool)
inisDeleteOnTermination = lens _inisDeleteOnTermination (\ s a -> s{_inisDeleteOnTermination = a});

-- | The IDs of the security groups for the network interface. Applies only
-- if creating a network interface when launching an instance.
inisGroups :: Lens' InstanceNetworkInterfaceSpecification [Text]
inisGroups = lens _inisGroups (\ s a -> s{_inisGroups = a}) . _Default;

-- | Indicates whether to assign a public IP address to an instance you
-- launch in a VPC. The public IP address can only be assigned to a network
-- interface for eth0, and can only be assigned to a new network interface,
-- not an existing one. You cannot specify more than one network interface
-- in the request. If launching into a default subnet, the default value is
-- @true@.
inisAssociatePublicIPAddress :: Lens' InstanceNetworkInterfaceSpecification (Maybe Bool)
inisAssociatePublicIPAddress = lens _inisAssociatePublicIPAddress (\ s a -> s{_inisAssociatePublicIPAddress = a});

-- | The ID of the network interface.
inisNetworkInterfaceId :: Lens' InstanceNetworkInterfaceSpecification (Maybe Text)
inisNetworkInterfaceId = lens _inisNetworkInterfaceId (\ s a -> s{_inisNetworkInterfaceId = a});

-- | The ID of the subnet associated with the network string. Applies only if
-- creating a network interface when launching an instance.
inisSubnetId :: Lens' InstanceNetworkInterfaceSpecification (Maybe Text)
inisSubnetId = lens _inisSubnetId (\ s a -> s{_inisSubnetId = a});

-- | The private IP address of the network interface. Applies only if
-- creating a network interface when launching an instance.
inisPrivateIPAddress :: Lens' InstanceNetworkInterfaceSpecification (Maybe Text)
inisPrivateIPAddress = lens _inisPrivateIPAddress (\ s a -> s{_inisPrivateIPAddress = a});

-- | The number of secondary private IP addresses. You can\'t specify this
-- option and specify more than one private IP address using the private IP
-- addresses option.
inisSecondaryPrivateIPAddressCount :: Lens' InstanceNetworkInterfaceSpecification (Maybe Int)
inisSecondaryPrivateIPAddressCount = lens _inisSecondaryPrivateIPAddressCount (\ s a -> s{_inisSecondaryPrivateIPAddressCount = a});

-- | The index of the device on the instance for the network interface
-- attachment. If you are specifying a network interface in a RunInstances
-- request, you must provide the device index.
inisDeviceIndex :: Lens' InstanceNetworkInterfaceSpecification (Maybe Int)
inisDeviceIndex = lens _inisDeviceIndex (\ s a -> s{_inisDeviceIndex = a});

-- | The description of the network interface. Applies only if creating a
-- network interface when launching an instance.
inisDescription :: Lens' InstanceNetworkInterfaceSpecification (Maybe Text)
inisDescription = lens _inisDescription (\ s a -> s{_inisDescription = a});

instance FromXML
         InstanceNetworkInterfaceSpecification where
        parseXML x
          = InstanceNetworkInterfaceSpecification' <$>
              (may (parseXMLList "item") x) <*>
                (x .@? "deleteOnTermination")
                <*> (may (parseXMLList "SecurityGroupId") x)
                <*> (x .@? "associatePublicIpAddress")
                <*> (x .@? "networkInterfaceId")
                <*> (x .@? "subnetId")
                <*> (x .@? "privateIpAddress")
                <*> (x .@? "secondaryPrivateIpAddressCount")
                <*> (x .@? "deviceIndex")
                <*> (x .@? "description")

instance ToQuery
         InstanceNetworkInterfaceSpecification where
        toQuery InstanceNetworkInterfaceSpecification'{..}
          = mconcat
              [toQuery
                 (toQueryList "item" <$> _inisPrivateIPAddresses),
               "DeleteOnTermination" =: _inisDeleteOnTermination,
               toQuery
                 (toQueryList "SecurityGroupId" <$> _inisGroups),
               "AssociatePublicIpAddress" =:
                 _inisAssociatePublicIPAddress,
               "NetworkInterfaceId" =: _inisNetworkInterfaceId,
               "SubnetId" =: _inisSubnetId,
               "PrivateIpAddress" =: _inisPrivateIPAddress,
               "SecondaryPrivateIpAddressCount" =:
                 _inisSecondaryPrivateIPAddressCount,
               "DeviceIndex" =: _inisDeviceIndex,
               "Description" =: _inisDescription]

-- | Describes a private IP address.
--
-- /See:/ 'instancePrivateIPAddress' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ipiaPrimary'
--
-- * 'ipiaPrivateIPAddress'
--
-- * 'ipiaPrivateDNSName'
--
-- * 'ipiaAssociation'
data InstancePrivateIPAddress = InstancePrivateIPAddress'{_ipiaPrimary :: Maybe Bool, _ipiaPrivateIPAddress :: Maybe Text, _ipiaPrivateDNSName :: Maybe Text, _ipiaAssociation :: Maybe InstanceNetworkInterfaceAssociation} deriving (Eq, Read, Show)

-- | 'InstancePrivateIPAddress' smart constructor.
instancePrivateIPAddress :: InstancePrivateIPAddress
instancePrivateIPAddress = InstancePrivateIPAddress'{_ipiaPrimary = Nothing, _ipiaPrivateIPAddress = Nothing, _ipiaPrivateDNSName = Nothing, _ipiaAssociation = Nothing};

-- | Indicates whether this IP address is the primary private IP address of
-- the network interface.
ipiaPrimary :: Lens' InstancePrivateIPAddress (Maybe Bool)
ipiaPrimary = lens _ipiaPrimary (\ s a -> s{_ipiaPrimary = a});

-- | The private IP address of the network interface.
ipiaPrivateIPAddress :: Lens' InstancePrivateIPAddress (Maybe Text)
ipiaPrivateIPAddress = lens _ipiaPrivateIPAddress (\ s a -> s{_ipiaPrivateIPAddress = a});

-- | The private DNS name.
ipiaPrivateDNSName :: Lens' InstancePrivateIPAddress (Maybe Text)
ipiaPrivateDNSName = lens _ipiaPrivateDNSName (\ s a -> s{_ipiaPrivateDNSName = a});

-- | The association information for an Elastic IP address for the network
-- interface.
ipiaAssociation :: Lens' InstancePrivateIPAddress (Maybe InstanceNetworkInterfaceAssociation)
ipiaAssociation = lens _ipiaAssociation (\ s a -> s{_ipiaAssociation = a});

instance FromXML InstancePrivateIPAddress where
        parseXML x
          = InstancePrivateIPAddress' <$>
              (x .@? "primary") <*> (x .@? "privateIpAddress") <*>
                (x .@? "privateDnsName")
                <*> (x .@? "association")

-- | Describes the current state of the instance.
--
-- /See:/ 'instanceState' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'isName'
--
-- * 'isCode'
data InstanceState = InstanceState'{_isName :: InstanceStateName, _isCode :: Int} deriving (Eq, Read, Show)

-- | 'InstanceState' smart constructor.
instanceState :: InstanceStateName -> Int -> InstanceState
instanceState pName pCode = InstanceState'{_isName = pName, _isCode = pCode};

-- | The current state of the instance.
isName :: Lens' InstanceState InstanceStateName
isName = lens _isName (\ s a -> s{_isName = a});

-- | The low byte represents the state. The high byte is an opaque internal
-- value and should be ignored.
--
-- -   @0@ : @pending@
--
-- -   @16@ : @running@
--
-- -   @32@ : @shutting-down@
--
-- -   @48@ : @terminated@
--
-- -   @64@ : @stopping@
--
-- -   @80@ : @stopped@
--
isCode :: Lens' InstanceState Int
isCode = lens _isCode (\ s a -> s{_isCode = a});

instance FromXML InstanceState where
        parseXML x
          = InstanceState' <$> (x .@ "name") <*> (x .@ "code")

-- | Describes an instance state change.
--
-- /See:/ 'instanceStateChange' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iscInstanceId'
--
-- * 'iscCurrentState'
--
-- * 'iscPreviousState'
data InstanceStateChange = InstanceStateChange'{_iscInstanceId :: Maybe Text, _iscCurrentState :: Maybe InstanceState, _iscPreviousState :: Maybe InstanceState} deriving (Eq, Read, Show)

-- | 'InstanceStateChange' smart constructor.
instanceStateChange :: InstanceStateChange
instanceStateChange = InstanceStateChange'{_iscInstanceId = Nothing, _iscCurrentState = Nothing, _iscPreviousState = Nothing};

-- | The ID of the instance.
iscInstanceId :: Lens' InstanceStateChange (Maybe Text)
iscInstanceId = lens _iscInstanceId (\ s a -> s{_iscInstanceId = a});

-- | The current state of the instance.
iscCurrentState :: Lens' InstanceStateChange (Maybe InstanceState)
iscCurrentState = lens _iscCurrentState (\ s a -> s{_iscCurrentState = a});

-- | The previous state of the instance.
iscPreviousState :: Lens' InstanceStateChange (Maybe InstanceState)
iscPreviousState = lens _iscPreviousState (\ s a -> s{_iscPreviousState = a});

instance FromXML InstanceStateChange where
        parseXML x
          = InstanceStateChange' <$>
              (x .@? "instanceId") <*> (x .@? "currentState") <*>
                (x .@? "previousState")

-- | Describes the status of an instance.
--
-- /See:/ 'instanceStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'isInstanceId'
--
-- * 'isSystemStatus'
--
-- * 'isAvailabilityZone'
--
-- * 'isEvents'
--
-- * 'isInstanceStatus'
--
-- * 'isInstanceState'
data InstanceStatus = InstanceStatus'{_isInstanceId :: Maybe Text, _isSystemStatus :: Maybe InstanceStatusSummary, _isAvailabilityZone :: Maybe Text, _isEvents :: Maybe [InstanceStatusEvent], _isInstanceStatus :: Maybe InstanceStatusSummary, _isInstanceState :: Maybe InstanceState} deriving (Eq, Read, Show)

-- | 'InstanceStatus' smart constructor.
instanceStatus :: InstanceStatus
instanceStatus = InstanceStatus'{_isInstanceId = Nothing, _isSystemStatus = Nothing, _isAvailabilityZone = Nothing, _isEvents = Nothing, _isInstanceStatus = Nothing, _isInstanceState = Nothing};

-- | The ID of the instance.
isInstanceId :: Lens' InstanceStatus (Maybe Text)
isInstanceId = lens _isInstanceId (\ s a -> s{_isInstanceId = a});

-- | Reports impaired functionality that stems from issues related to the
-- systems that support an instance, such as hardware failures and network
-- connectivity problems.
isSystemStatus :: Lens' InstanceStatus (Maybe InstanceStatusSummary)
isSystemStatus = lens _isSystemStatus (\ s a -> s{_isSystemStatus = a});

-- | The Availability Zone of the instance.
isAvailabilityZone :: Lens' InstanceStatus (Maybe Text)
isAvailabilityZone = lens _isAvailabilityZone (\ s a -> s{_isAvailabilityZone = a});

-- | Any scheduled events associated with the instance.
isEvents :: Lens' InstanceStatus [InstanceStatusEvent]
isEvents = lens _isEvents (\ s a -> s{_isEvents = a}) . _Default;

-- | Reports impaired functionality that stems from issues internal to the
-- instance, such as impaired reachability.
isInstanceStatus :: Lens' InstanceStatus (Maybe InstanceStatusSummary)
isInstanceStatus = lens _isInstanceStatus (\ s a -> s{_isInstanceStatus = a});

-- | The intended state of the instance. DescribeInstanceStatus requires that
-- an instance be in the @running@ state.
isInstanceState :: Lens' InstanceStatus (Maybe InstanceState)
isInstanceState = lens _isInstanceState (\ s a -> s{_isInstanceState = a});

instance FromXML InstanceStatus where
        parseXML x
          = InstanceStatus' <$>
              (x .@? "instanceId") <*> (x .@? "systemStatus") <*>
                (x .@? "availabilityZone")
                <*> (may (parseXMLList "item") x)
                <*> (x .@? "instanceStatus")
                <*> (x .@? "instanceState")

-- | Describes the instance status.
--
-- /See:/ 'instanceStatusDetails' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'isdStatus'
--
-- * 'isdImpairedSince'
--
-- * 'isdName'
data InstanceStatusDetails = InstanceStatusDetails'{_isdStatus :: Maybe StatusType, _isdImpairedSince :: Maybe ISO8601, _isdName :: Maybe StatusName} deriving (Eq, Read, Show)

-- | 'InstanceStatusDetails' smart constructor.
instanceStatusDetails :: InstanceStatusDetails
instanceStatusDetails = InstanceStatusDetails'{_isdStatus = Nothing, _isdImpairedSince = Nothing, _isdName = Nothing};

-- | The status.
isdStatus :: Lens' InstanceStatusDetails (Maybe StatusType)
isdStatus = lens _isdStatus (\ s a -> s{_isdStatus = a});

-- | The time when a status check failed. For an instance that was launched
-- and impaired, this is the time when the instance was launched.
isdImpairedSince :: Lens' InstanceStatusDetails (Maybe UTCTime)
isdImpairedSince = lens _isdImpairedSince (\ s a -> s{_isdImpairedSince = a}) . mapping _Time;

-- | The type of instance status.
isdName :: Lens' InstanceStatusDetails (Maybe StatusName)
isdName = lens _isdName (\ s a -> s{_isdName = a});

instance FromXML InstanceStatusDetails where
        parseXML x
          = InstanceStatusDetails' <$>
              (x .@? "status") <*> (x .@? "impairedSince") <*>
                (x .@? "name")

-- | Describes a scheduled event for an instance.
--
-- /See:/ 'instanceStatusEvent' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iseNotBefore'
--
-- * 'iseCode'
--
-- * 'iseDescription'
--
-- * 'iseNotAfter'
data InstanceStatusEvent = InstanceStatusEvent'{_iseNotBefore :: Maybe ISO8601, _iseCode :: Maybe EventCode, _iseDescription :: Maybe Text, _iseNotAfter :: Maybe ISO8601} deriving (Eq, Read, Show)

-- | 'InstanceStatusEvent' smart constructor.
instanceStatusEvent :: InstanceStatusEvent
instanceStatusEvent = InstanceStatusEvent'{_iseNotBefore = Nothing, _iseCode = Nothing, _iseDescription = Nothing, _iseNotAfter = Nothing};

-- | The earliest scheduled start time for the event.
iseNotBefore :: Lens' InstanceStatusEvent (Maybe UTCTime)
iseNotBefore = lens _iseNotBefore (\ s a -> s{_iseNotBefore = a}) . mapping _Time;

-- | The event code.
iseCode :: Lens' InstanceStatusEvent (Maybe EventCode)
iseCode = lens _iseCode (\ s a -> s{_iseCode = a});

-- | A description of the event.
--
-- After a scheduled event is completed, it can still be described for up
-- to a week. If the event has been completed, this description starts with
-- the following text: [Completed].
iseDescription :: Lens' InstanceStatusEvent (Maybe Text)
iseDescription = lens _iseDescription (\ s a -> s{_iseDescription = a});

-- | The latest scheduled end time for the event.
iseNotAfter :: Lens' InstanceStatusEvent (Maybe UTCTime)
iseNotAfter = lens _iseNotAfter (\ s a -> s{_iseNotAfter = a}) . mapping _Time;

instance FromXML InstanceStatusEvent where
        parseXML x
          = InstanceStatusEvent' <$>
              (x .@? "notBefore") <*> (x .@? "code") <*>
                (x .@? "description")
                <*> (x .@? "notAfter")

-- | Describes the status of an instance.
--
-- /See:/ 'instanceStatusSummary' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'issDetails'
--
-- * 'issStatus'
data InstanceStatusSummary = InstanceStatusSummary'{_issDetails :: Maybe [InstanceStatusDetails], _issStatus :: SummaryStatus} deriving (Eq, Read, Show)

-- | 'InstanceStatusSummary' smart constructor.
instanceStatusSummary :: SummaryStatus -> InstanceStatusSummary
instanceStatusSummary pStatus = InstanceStatusSummary'{_issDetails = Nothing, _issStatus = pStatus};

-- | The system instance health or application instance health.
issDetails :: Lens' InstanceStatusSummary [InstanceStatusDetails]
issDetails = lens _issDetails (\ s a -> s{_issDetails = a}) . _Default;

-- | The status.
issStatus :: Lens' InstanceStatusSummary SummaryStatus
issStatus = lens _issStatus (\ s a -> s{_issStatus = a});

instance FromXML InstanceStatusSummary where
        parseXML x
          = InstanceStatusSummary' <$>
              (may (parseXMLList "item") x) <*> (x .@ "status")

-- | Describes an Internet gateway.
--
-- /See:/ 'internetGateway' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'igAttachments'
--
-- * 'igTags'
--
-- * 'igInternetGatewayId'
data InternetGateway = InternetGateway'{_igAttachments :: Maybe [InternetGatewayAttachment], _igTags :: Maybe [Tag], _igInternetGatewayId :: Text} deriving (Eq, Read, Show)

-- | 'InternetGateway' smart constructor.
internetGateway :: Text -> InternetGateway
internetGateway pInternetGatewayId = InternetGateway'{_igAttachments = Nothing, _igTags = Nothing, _igInternetGatewayId = pInternetGatewayId};

-- | Any VPCs attached to the Internet gateway.
igAttachments :: Lens' InternetGateway [InternetGatewayAttachment]
igAttachments = lens _igAttachments (\ s a -> s{_igAttachments = a}) . _Default;

-- | Any tags assigned to the Internet gateway.
igTags :: Lens' InternetGateway [Tag]
igTags = lens _igTags (\ s a -> s{_igTags = a}) . _Default;

-- | The ID of the Internet gateway.
igInternetGatewayId :: Lens' InternetGateway Text
igInternetGatewayId = lens _igInternetGatewayId (\ s a -> s{_igInternetGatewayId = a});

instance FromXML InternetGateway where
        parseXML x
          = InternetGateway' <$>
              (may (parseXMLList "item") x) <*>
                (may (parseXMLList "item") x)
                <*> (x .@ "internetGatewayId")

-- | Describes the attachment of a VPC to an Internet gateway.
--
-- /See:/ 'internetGatewayAttachment' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'igaState'
--
-- * 'igaVPCId'
data InternetGatewayAttachment = InternetGatewayAttachment'{_igaState :: AttachmentStatus, _igaVPCId :: Text} deriving (Eq, Read, Show)

-- | 'InternetGatewayAttachment' smart constructor.
internetGatewayAttachment :: AttachmentStatus -> Text -> InternetGatewayAttachment
internetGatewayAttachment pState pVPCId = InternetGatewayAttachment'{_igaState = pState, _igaVPCId = pVPCId};

-- | The current state of the attachment.
igaState :: Lens' InternetGatewayAttachment AttachmentStatus
igaState = lens _igaState (\ s a -> s{_igaState = a});

-- | The ID of the VPC.
igaVPCId :: Lens' InternetGatewayAttachment Text
igaVPCId = lens _igaVPCId (\ s a -> s{_igaVPCId = a});

instance FromXML InternetGatewayAttachment where
        parseXML x
          = InternetGatewayAttachment' <$>
              (x .@ "state") <*> (x .@ "vpcId")

-- | Describes a key pair.
--
-- /See:/ 'keyPairInfo' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'kpiKeyFingerprint'
--
-- * 'kpiKeyName'
data KeyPairInfo = KeyPairInfo'{_kpiKeyFingerprint :: Maybe Text, _kpiKeyName :: Maybe Text} deriving (Eq, Read, Show)

-- | 'KeyPairInfo' smart constructor.
keyPairInfo :: KeyPairInfo
keyPairInfo = KeyPairInfo'{_kpiKeyFingerprint = Nothing, _kpiKeyName = Nothing};

-- | If you used CreateKeyPair to create the key pair, this is the SHA-1
-- digest of the DER encoded private key. If you used ImportKeyPair to
-- provide AWS the public key, this is the MD5 public key fingerprint as
-- specified in section 4 of RFC4716.
kpiKeyFingerprint :: Lens' KeyPairInfo (Maybe Text)
kpiKeyFingerprint = lens _kpiKeyFingerprint (\ s a -> s{_kpiKeyFingerprint = a});

-- | The name of the key pair.
kpiKeyName :: Lens' KeyPairInfo (Maybe Text)
kpiKeyName = lens _kpiKeyName (\ s a -> s{_kpiKeyName = a});

instance FromXML KeyPairInfo where
        parseXML x
          = KeyPairInfo' <$>
              (x .@? "keyFingerprint") <*> (x .@? "keyName")

-- | Describes a launch permission.
--
-- /See:/ 'launchPermission' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lpGroup'
--
-- * 'lpUserId'
data LaunchPermission = LaunchPermission'{_lpGroup :: Maybe PermissionGroup, _lpUserId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'LaunchPermission' smart constructor.
launchPermission :: LaunchPermission
launchPermission = LaunchPermission'{_lpGroup = Nothing, _lpUserId = Nothing};

-- | The name of the group.
lpGroup :: Lens' LaunchPermission (Maybe PermissionGroup)
lpGroup = lens _lpGroup (\ s a -> s{_lpGroup = a});

-- | The AWS account ID.
lpUserId :: Lens' LaunchPermission (Maybe Text)
lpUserId = lens _lpUserId (\ s a -> s{_lpUserId = a});

instance FromXML LaunchPermission where
        parseXML x
          = LaunchPermission' <$>
              (x .@? "group") <*> (x .@? "userId")

instance ToQuery LaunchPermission where
        toQuery LaunchPermission'{..}
          = mconcat
              ["Group" =: _lpGroup, "UserId" =: _lpUserId]

-- | Describes a launch permission modification.
--
-- /See:/ 'launchPermissionModifications' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lpmRemove'
--
-- * 'lpmAdd'
data LaunchPermissionModifications = LaunchPermissionModifications'{_lpmRemove :: Maybe [LaunchPermission], _lpmAdd :: Maybe [LaunchPermission]} deriving (Eq, Read, Show)

-- | 'LaunchPermissionModifications' smart constructor.
launchPermissionModifications :: LaunchPermissionModifications
launchPermissionModifications = LaunchPermissionModifications'{_lpmRemove = Nothing, _lpmAdd = Nothing};

-- | The AWS account ID to remove from the list of launch permissions for the
-- AMI.
lpmRemove :: Lens' LaunchPermissionModifications [LaunchPermission]
lpmRemove = lens _lpmRemove (\ s a -> s{_lpmRemove = a}) . _Default;

-- | The AWS account ID to add to the list of launch permissions for the AMI.
lpmAdd :: Lens' LaunchPermissionModifications [LaunchPermission]
lpmAdd = lens _lpmAdd (\ s a -> s{_lpmAdd = a}) . _Default;

instance ToQuery LaunchPermissionModifications where
        toQuery LaunchPermissionModifications'{..}
          = mconcat
              [toQuery (toQueryList "item" <$> _lpmRemove),
               toQuery (toQueryList "item" <$> _lpmAdd)]

-- | Describes the launch specification for an instance.
--
-- /See:/ 'launchSpecification' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsSecurityGroups'
--
-- * 'lsNetworkInterfaces'
--
-- * 'lsKeyName'
--
-- * 'lsRAMDiskId'
--
-- * 'lsKernelId'
--
-- * 'lsSubnetId'
--
-- * 'lsInstanceType'
--
-- * 'lsEBSOptimized'
--
-- * 'lsUserData'
--
-- * 'lsMonitoring'
--
-- * 'lsIAMInstanceProfile'
--
-- * 'lsImageId'
--
-- * 'lsBlockDeviceMappings'
--
-- * 'lsAddressingType'
--
-- * 'lsPlacement'
data LaunchSpecification = LaunchSpecification'{_lsSecurityGroups :: Maybe [GroupIdentifier], _lsNetworkInterfaces :: Maybe [InstanceNetworkInterfaceSpecification], _lsKeyName :: Maybe Text, _lsRAMDiskId :: Maybe Text, _lsKernelId :: Maybe Text, _lsSubnetId :: Maybe Text, _lsInstanceType :: Maybe InstanceType, _lsEBSOptimized :: Maybe Bool, _lsUserData :: Maybe Text, _lsMonitoring :: Maybe RunInstancesMonitoringEnabled, _lsIAMInstanceProfile :: Maybe IAMInstanceProfileSpecification, _lsImageId :: Maybe Text, _lsBlockDeviceMappings :: Maybe [BlockDeviceMapping], _lsAddressingType :: Maybe Text, _lsPlacement :: Maybe SpotPlacement} deriving (Eq, Read, Show)

-- | 'LaunchSpecification' smart constructor.
launchSpecification :: LaunchSpecification
launchSpecification = LaunchSpecification'{_lsSecurityGroups = Nothing, _lsNetworkInterfaces = Nothing, _lsKeyName = Nothing, _lsRAMDiskId = Nothing, _lsKernelId = Nothing, _lsSubnetId = Nothing, _lsInstanceType = Nothing, _lsEBSOptimized = Nothing, _lsUserData = Nothing, _lsMonitoring = Nothing, _lsIAMInstanceProfile = Nothing, _lsImageId = Nothing, _lsBlockDeviceMappings = Nothing, _lsAddressingType = Nothing, _lsPlacement = Nothing};

-- | One or more security groups. To request an instance in a nondefault VPC,
-- you must specify the ID of the security group. To request an instance in
-- EC2-Classic or a default VPC, you can specify the name or the ID of the
-- security group.
lsSecurityGroups :: Lens' LaunchSpecification [GroupIdentifier]
lsSecurityGroups = lens _lsSecurityGroups (\ s a -> s{_lsSecurityGroups = a}) . _Default;

-- | One or more network interfaces.
lsNetworkInterfaces :: Lens' LaunchSpecification [InstanceNetworkInterfaceSpecification]
lsNetworkInterfaces = lens _lsNetworkInterfaces (\ s a -> s{_lsNetworkInterfaces = a}) . _Default;

-- | The name of the key pair.
lsKeyName :: Lens' LaunchSpecification (Maybe Text)
lsKeyName = lens _lsKeyName (\ s a -> s{_lsKeyName = a});

-- | The ID of the RAM disk.
lsRAMDiskId :: Lens' LaunchSpecification (Maybe Text)
lsRAMDiskId = lens _lsRAMDiskId (\ s a -> s{_lsRAMDiskId = a});

-- | The ID of the kernel.
lsKernelId :: Lens' LaunchSpecification (Maybe Text)
lsKernelId = lens _lsKernelId (\ s a -> s{_lsKernelId = a});

-- | The ID of the subnet in which to launch the instance.
lsSubnetId :: Lens' LaunchSpecification (Maybe Text)
lsSubnetId = lens _lsSubnetId (\ s a -> s{_lsSubnetId = a});

-- | The instance type.
lsInstanceType :: Lens' LaunchSpecification (Maybe InstanceType)
lsInstanceType = lens _lsInstanceType (\ s a -> s{_lsInstanceType = a});

-- | Indicates whether the instance is optimized for EBS I\/O. This
-- optimization provides dedicated throughput to Amazon EBS and an
-- optimized configuration stack to provide optimal EBS I\/O performance.
-- This optimization isn\'t available with all instance types. Additional
-- usage charges apply when using an EBS Optimized instance.
--
-- Default: @false@
lsEBSOptimized :: Lens' LaunchSpecification (Maybe Bool)
lsEBSOptimized = lens _lsEBSOptimized (\ s a -> s{_lsEBSOptimized = a});

-- | The Base64-encoded MIME user data to make available to the instances.
lsUserData :: Lens' LaunchSpecification (Maybe Text)
lsUserData = lens _lsUserData (\ s a -> s{_lsUserData = a});

-- | FIXME: Undocumented member.
lsMonitoring :: Lens' LaunchSpecification (Maybe RunInstancesMonitoringEnabled)
lsMonitoring = lens _lsMonitoring (\ s a -> s{_lsMonitoring = a});

-- | The IAM instance profile.
lsIAMInstanceProfile :: Lens' LaunchSpecification (Maybe IAMInstanceProfileSpecification)
lsIAMInstanceProfile = lens _lsIAMInstanceProfile (\ s a -> s{_lsIAMInstanceProfile = a});

-- | The ID of the AMI.
lsImageId :: Lens' LaunchSpecification (Maybe Text)
lsImageId = lens _lsImageId (\ s a -> s{_lsImageId = a});

-- | One or more block device mapping entries.
lsBlockDeviceMappings :: Lens' LaunchSpecification [BlockDeviceMapping]
lsBlockDeviceMappings = lens _lsBlockDeviceMappings (\ s a -> s{_lsBlockDeviceMappings = a}) . _Default;

-- | Deprecated.
lsAddressingType :: Lens' LaunchSpecification (Maybe Text)
lsAddressingType = lens _lsAddressingType (\ s a -> s{_lsAddressingType = a});

-- | The placement information for the instance.
lsPlacement :: Lens' LaunchSpecification (Maybe SpotPlacement)
lsPlacement = lens _lsPlacement (\ s a -> s{_lsPlacement = a});

instance FromXML LaunchSpecification where
        parseXML x
          = LaunchSpecification' <$>
              (may (parseXMLList "item") x) <*>
                (may (parseXMLList "item") x)
                <*> (x .@? "keyName")
                <*> (x .@? "ramdiskId")
                <*> (x .@? "kernelId")
                <*> (x .@? "subnetId")
                <*> (x .@? "instanceType")
                <*> (x .@? "ebsOptimized")
                <*> (x .@? "userData")
                <*> (x .@? "monitoring")
                <*> (x .@? "iamInstanceProfile")
                <*> (x .@? "imageId")
                <*> (may (parseXMLList "item") x)
                <*> (x .@? "addressingType")
                <*> (x .@? "placement")

instance ToQuery LaunchSpecification where
        toQuery LaunchSpecification'{..}
          = mconcat
              [toQuery (toQueryList "item" <$> _lsSecurityGroups),
               toQuery
                 (toQueryList "item" <$> _lsNetworkInterfaces),
               "KeyName" =: _lsKeyName, "RamdiskId" =: _lsRAMDiskId,
               "KernelId" =: _lsKernelId, "SubnetId" =: _lsSubnetId,
               "InstanceType" =: _lsInstanceType,
               "EbsOptimized" =: _lsEBSOptimized,
               "UserData" =: _lsUserData,
               "Monitoring" =: _lsMonitoring,
               "IamInstanceProfile" =: _lsIAMInstanceProfile,
               "ImageId" =: _lsImageId,
               toQuery
                 (toQueryList "item" <$> _lsBlockDeviceMappings),
               "AddressingType" =: _lsAddressingType,
               "Placement" =: _lsPlacement]

-- | Describes the monitoring for the instance.
--
-- /See:/ 'monitoring' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'monState'
newtype Monitoring = Monitoring'{_monState :: Maybe MonitoringState} deriving (Eq, Read, Show)

-- | 'Monitoring' smart constructor.
monitoring :: Monitoring
monitoring = Monitoring'{_monState = Nothing};

-- | Indicates whether monitoring is enabled for the instance.
monState :: Lens' Monitoring (Maybe MonitoringState)
monState = lens _monState (\ s a -> s{_monState = a});

instance FromXML Monitoring where
        parseXML x = Monitoring' <$> (x .@? "state")

-- | Describes the status of a moving Elastic IP address.
--
-- /See:/ 'movingAddressStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'masMoveStatus'
--
-- * 'masPublicIP'
data MovingAddressStatus = MovingAddressStatus'{_masMoveStatus :: Maybe MoveStatus, _masPublicIP :: Maybe Text} deriving (Eq, Read, Show)

-- | 'MovingAddressStatus' smart constructor.
movingAddressStatus :: MovingAddressStatus
movingAddressStatus = MovingAddressStatus'{_masMoveStatus = Nothing, _masPublicIP = Nothing};

-- | The status of the Elastic IP address that\'s being moved to the EC2-VPC
-- platform, or restored to the EC2-Classic platform.
masMoveStatus :: Lens' MovingAddressStatus (Maybe MoveStatus)
masMoveStatus = lens _masMoveStatus (\ s a -> s{_masMoveStatus = a});

-- | The Elastic IP address.
masPublicIP :: Lens' MovingAddressStatus (Maybe Text)
masPublicIP = lens _masPublicIP (\ s a -> s{_masPublicIP = a});

instance FromXML MovingAddressStatus where
        parseXML x
          = MovingAddressStatus' <$>
              (x .@? "moveStatus") <*> (x .@? "publicIp")

-- | Describes a network ACL.
--
-- /See:/ 'networkACL' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'naEntries'
--
-- * 'naNetworkACLId'
--
-- * 'naVPCId'
--
-- * 'naAssociations'
--
-- * 'naIsDefault'
--
-- * 'naTags'
data NetworkACL = NetworkACL'{_naEntries :: Maybe [NetworkACLEntry], _naNetworkACLId :: Maybe Text, _naVPCId :: Maybe Text, _naAssociations :: Maybe [NetworkACLAssociation], _naIsDefault :: Maybe Bool, _naTags :: Maybe [Tag]} deriving (Eq, Read, Show)

-- | 'NetworkACL' smart constructor.
networkACL :: NetworkACL
networkACL = NetworkACL'{_naEntries = Nothing, _naNetworkACLId = Nothing, _naVPCId = Nothing, _naAssociations = Nothing, _naIsDefault = Nothing, _naTags = Nothing};

-- | One or more entries (rules) in the network ACL.
naEntries :: Lens' NetworkACL [NetworkACLEntry]
naEntries = lens _naEntries (\ s a -> s{_naEntries = a}) . _Default;

-- | The ID of the network ACL.
naNetworkACLId :: Lens' NetworkACL (Maybe Text)
naNetworkACLId = lens _naNetworkACLId (\ s a -> s{_naNetworkACLId = a});

-- | The ID of the VPC for the network ACL.
naVPCId :: Lens' NetworkACL (Maybe Text)
naVPCId = lens _naVPCId (\ s a -> s{_naVPCId = a});

-- | Any associations between the network ACL and one or more subnets
naAssociations :: Lens' NetworkACL [NetworkACLAssociation]
naAssociations = lens _naAssociations (\ s a -> s{_naAssociations = a}) . _Default;

-- | Indicates whether this is the default network ACL for the VPC.
naIsDefault :: Lens' NetworkACL (Maybe Bool)
naIsDefault = lens _naIsDefault (\ s a -> s{_naIsDefault = a});

-- | Any tags assigned to the network ACL.
naTags :: Lens' NetworkACL [Tag]
naTags = lens _naTags (\ s a -> s{_naTags = a}) . _Default;

instance FromXML NetworkACL where
        parseXML x
          = NetworkACL' <$>
              (may (parseXMLList "item") x) <*>
                (x .@? "networkAclId")
                <*> (x .@? "vpcId")
                <*> (may (parseXMLList "item") x)
                <*> (x .@? "default")
                <*> (may (parseXMLList "item") x)

-- | Describes an association between a network ACL and a subnet.
--
-- /See:/ 'networkACLAssociation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'naaNetworkACLId'
--
-- * 'naaSubnetId'
--
-- * 'naaNetworkACLAssociationId'
data NetworkACLAssociation = NetworkACLAssociation'{_naaNetworkACLId :: Maybe Text, _naaSubnetId :: Maybe Text, _naaNetworkACLAssociationId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'NetworkACLAssociation' smart constructor.
networkACLAssociation :: NetworkACLAssociation
networkACLAssociation = NetworkACLAssociation'{_naaNetworkACLId = Nothing, _naaSubnetId = Nothing, _naaNetworkACLAssociationId = Nothing};

-- | The ID of the network ACL.
naaNetworkACLId :: Lens' NetworkACLAssociation (Maybe Text)
naaNetworkACLId = lens _naaNetworkACLId (\ s a -> s{_naaNetworkACLId = a});

-- | The ID of the subnet.
naaSubnetId :: Lens' NetworkACLAssociation (Maybe Text)
naaSubnetId = lens _naaSubnetId (\ s a -> s{_naaSubnetId = a});

-- | The ID of the association between a network ACL and a subnet.
naaNetworkACLAssociationId :: Lens' NetworkACLAssociation (Maybe Text)
naaNetworkACLAssociationId = lens _naaNetworkACLAssociationId (\ s a -> s{_naaNetworkACLAssociationId = a});

instance FromXML NetworkACLAssociation where
        parseXML x
          = NetworkACLAssociation' <$>
              (x .@? "networkAclId") <*> (x .@? "subnetId") <*>
                (x .@? "networkAclAssociationId")

-- | Describes an entry in a network ACL.
--
-- /See:/ 'networkACLEntry' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'naeICMPTypeCode'
--
-- * 'naeRuleNumber'
--
-- * 'naeRuleAction'
--
-- * 'naeProtocol'
--
-- * 'naePortRange'
--
-- * 'naeCIDRBlock'
--
-- * 'naeEgress'
data NetworkACLEntry = NetworkACLEntry'{_naeICMPTypeCode :: Maybe ICMPTypeCode, _naeRuleNumber :: Maybe Int, _naeRuleAction :: Maybe RuleAction, _naeProtocol :: Maybe Text, _naePortRange :: Maybe PortRange, _naeCIDRBlock :: Maybe Text, _naeEgress :: Maybe Bool} deriving (Eq, Read, Show)

-- | 'NetworkACLEntry' smart constructor.
networkACLEntry :: NetworkACLEntry
networkACLEntry = NetworkACLEntry'{_naeICMPTypeCode = Nothing, _naeRuleNumber = Nothing, _naeRuleAction = Nothing, _naeProtocol = Nothing, _naePortRange = Nothing, _naeCIDRBlock = Nothing, _naeEgress = Nothing};

-- | ICMP protocol: The ICMP type and code.
naeICMPTypeCode :: Lens' NetworkACLEntry (Maybe ICMPTypeCode)
naeICMPTypeCode = lens _naeICMPTypeCode (\ s a -> s{_naeICMPTypeCode = a});

-- | The rule number for the entry. ACL entries are processed in ascending
-- order by rule number.
naeRuleNumber :: Lens' NetworkACLEntry (Maybe Int)
naeRuleNumber = lens _naeRuleNumber (\ s a -> s{_naeRuleNumber = a});

-- | Indicates whether to allow or deny the traffic that matches the rule.
naeRuleAction :: Lens' NetworkACLEntry (Maybe RuleAction)
naeRuleAction = lens _naeRuleAction (\ s a -> s{_naeRuleAction = a});

-- | The protocol. A value of @-1@ means all protocols.
naeProtocol :: Lens' NetworkACLEntry (Maybe Text)
naeProtocol = lens _naeProtocol (\ s a -> s{_naeProtocol = a});

-- | TCP or UDP protocols: The range of ports the rule applies to.
naePortRange :: Lens' NetworkACLEntry (Maybe PortRange)
naePortRange = lens _naePortRange (\ s a -> s{_naePortRange = a});

-- | The network range to allow or deny, in CIDR notation.
naeCIDRBlock :: Lens' NetworkACLEntry (Maybe Text)
naeCIDRBlock = lens _naeCIDRBlock (\ s a -> s{_naeCIDRBlock = a});

-- | Indicates whether the rule is an egress rule (applied to traffic leaving
-- the subnet).
naeEgress :: Lens' NetworkACLEntry (Maybe Bool)
naeEgress = lens _naeEgress (\ s a -> s{_naeEgress = a});

instance FromXML NetworkACLEntry where
        parseXML x
          = NetworkACLEntry' <$>
              (x .@? "icmpTypeCode") <*> (x .@? "ruleNumber") <*>
                (x .@? "ruleAction")
                <*> (x .@? "protocol")
                <*> (x .@? "portRange")
                <*> (x .@? "cidrBlock")
                <*> (x .@? "egress")

-- | Describes a network interface.
--
-- /See:/ 'networkInterface' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'niPrivateIPAddresses'
--
-- * 'niStatus'
--
-- * 'niGroups'
--
-- * 'niSourceDestCheck'
--
-- * 'niTagSet'
--
-- * 'niVPCId'
--
-- * 'niRequesterManaged'
--
-- * 'niNetworkInterfaceId'
--
-- * 'niSubnetId'
--
-- * 'niAttachment'
--
-- * 'niMACAddress'
--
-- * 'niOwnerId'
--
-- * 'niAvailabilityZone'
--
-- * 'niPrivateIPAddress'
--
-- * 'niPrivateDNSName'
--
-- * 'niRequesterId'
--
-- * 'niDescription'
--
-- * 'niAssociation'
data NetworkInterface = NetworkInterface'{_niPrivateIPAddresses :: Maybe [NetworkInterfacePrivateIPAddress], _niStatus :: Maybe NetworkInterfaceStatus, _niGroups :: Maybe [GroupIdentifier], _niSourceDestCheck :: Maybe Bool, _niTagSet :: Maybe [Tag], _niVPCId :: Maybe Text, _niRequesterManaged :: Maybe Bool, _niNetworkInterfaceId :: Maybe Text, _niSubnetId :: Maybe Text, _niAttachment :: Maybe NetworkInterfaceAttachment, _niMACAddress :: Maybe Text, _niOwnerId :: Maybe Text, _niAvailabilityZone :: Maybe Text, _niPrivateIPAddress :: Maybe Text, _niPrivateDNSName :: Maybe Text, _niRequesterId :: Maybe Text, _niDescription :: Maybe Text, _niAssociation :: Maybe NetworkInterfaceAssociation} deriving (Eq, Read, Show)

-- | 'NetworkInterface' smart constructor.
networkInterface :: NetworkInterface
networkInterface = NetworkInterface'{_niPrivateIPAddresses = Nothing, _niStatus = Nothing, _niGroups = Nothing, _niSourceDestCheck = Nothing, _niTagSet = Nothing, _niVPCId = Nothing, _niRequesterManaged = Nothing, _niNetworkInterfaceId = Nothing, _niSubnetId = Nothing, _niAttachment = Nothing, _niMACAddress = Nothing, _niOwnerId = Nothing, _niAvailabilityZone = Nothing, _niPrivateIPAddress = Nothing, _niPrivateDNSName = Nothing, _niRequesterId = Nothing, _niDescription = Nothing, _niAssociation = Nothing};

-- | The private IP addresses associated with the network interface.
niPrivateIPAddresses :: Lens' NetworkInterface [NetworkInterfacePrivateIPAddress]
niPrivateIPAddresses = lens _niPrivateIPAddresses (\ s a -> s{_niPrivateIPAddresses = a}) . _Default;

-- | The status of the network interface.
niStatus :: Lens' NetworkInterface (Maybe NetworkInterfaceStatus)
niStatus = lens _niStatus (\ s a -> s{_niStatus = a});

-- | Any security groups for the network interface.
niGroups :: Lens' NetworkInterface [GroupIdentifier]
niGroups = lens _niGroups (\ s a -> s{_niGroups = a}) . _Default;

-- | Indicates whether traffic to or from the instance is validated.
niSourceDestCheck :: Lens' NetworkInterface (Maybe Bool)
niSourceDestCheck = lens _niSourceDestCheck (\ s a -> s{_niSourceDestCheck = a});

-- | Any tags assigned to the network interface.
niTagSet :: Lens' NetworkInterface [Tag]
niTagSet = lens _niTagSet (\ s a -> s{_niTagSet = a}) . _Default;

-- | The ID of the VPC.
niVPCId :: Lens' NetworkInterface (Maybe Text)
niVPCId = lens _niVPCId (\ s a -> s{_niVPCId = a});

-- | Indicates whether the network interface is being managed by AWS.
niRequesterManaged :: Lens' NetworkInterface (Maybe Bool)
niRequesterManaged = lens _niRequesterManaged (\ s a -> s{_niRequesterManaged = a});

-- | The ID of the network interface.
niNetworkInterfaceId :: Lens' NetworkInterface (Maybe Text)
niNetworkInterfaceId = lens _niNetworkInterfaceId (\ s a -> s{_niNetworkInterfaceId = a});

-- | The ID of the subnet.
niSubnetId :: Lens' NetworkInterface (Maybe Text)
niSubnetId = lens _niSubnetId (\ s a -> s{_niSubnetId = a});

-- | The network interface attachment.
niAttachment :: Lens' NetworkInterface (Maybe NetworkInterfaceAttachment)
niAttachment = lens _niAttachment (\ s a -> s{_niAttachment = a});

-- | The MAC address.
niMACAddress :: Lens' NetworkInterface (Maybe Text)
niMACAddress = lens _niMACAddress (\ s a -> s{_niMACAddress = a});

-- | The AWS account ID of the owner of the network interface.
niOwnerId :: Lens' NetworkInterface (Maybe Text)
niOwnerId = lens _niOwnerId (\ s a -> s{_niOwnerId = a});

-- | The Availability Zone.
niAvailabilityZone :: Lens' NetworkInterface (Maybe Text)
niAvailabilityZone = lens _niAvailabilityZone (\ s a -> s{_niAvailabilityZone = a});

-- | The IP address of the network interface within the subnet.
niPrivateIPAddress :: Lens' NetworkInterface (Maybe Text)
niPrivateIPAddress = lens _niPrivateIPAddress (\ s a -> s{_niPrivateIPAddress = a});

-- | The private DNS name.
niPrivateDNSName :: Lens' NetworkInterface (Maybe Text)
niPrivateDNSName = lens _niPrivateDNSName (\ s a -> s{_niPrivateDNSName = a});

-- | The ID of the entity that launched the instance on your behalf (for
-- example, AWS Management Console or Auto Scaling).
niRequesterId :: Lens' NetworkInterface (Maybe Text)
niRequesterId = lens _niRequesterId (\ s a -> s{_niRequesterId = a});

-- | A description.
niDescription :: Lens' NetworkInterface (Maybe Text)
niDescription = lens _niDescription (\ s a -> s{_niDescription = a});

-- | The association information for an Elastic IP associated with the
-- network interface.
niAssociation :: Lens' NetworkInterface (Maybe NetworkInterfaceAssociation)
niAssociation = lens _niAssociation (\ s a -> s{_niAssociation = a});

instance FromXML NetworkInterface where
        parseXML x
          = NetworkInterface' <$>
              (may (parseXMLList "item") x) <*> (x .@? "status")
                <*> (may (parseXMLList "item") x)
                <*> (x .@? "sourceDestCheck")
                <*> (may (parseXMLList "item") x)
                <*> (x .@? "vpcId")
                <*> (x .@? "requesterManaged")
                <*> (x .@? "networkInterfaceId")
                <*> (x .@? "subnetId")
                <*> (x .@? "attachment")
                <*> (x .@? "macAddress")
                <*> (x .@? "ownerId")
                <*> (x .@? "availabilityZone")
                <*> (x .@? "privateIpAddress")
                <*> (x .@? "privateDnsName")
                <*> (x .@? "requesterId")
                <*> (x .@? "description")
                <*> (x .@? "association")

-- | Describes association information for an Elastic IP address.
--
-- /See:/ 'networkInterfaceAssociation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'niaAssociationId'
--
-- * 'niaPublicDNSName'
--
-- * 'niaAllocationId'
--
-- * 'niaIPOwnerId'
--
-- * 'niaPublicIP'
data NetworkInterfaceAssociation = NetworkInterfaceAssociation'{_niaAssociationId :: Maybe Text, _niaPublicDNSName :: Maybe Text, _niaAllocationId :: Maybe Text, _niaIPOwnerId :: Maybe Text, _niaPublicIP :: Maybe Text} deriving (Eq, Read, Show)

-- | 'NetworkInterfaceAssociation' smart constructor.
networkInterfaceAssociation :: NetworkInterfaceAssociation
networkInterfaceAssociation = NetworkInterfaceAssociation'{_niaAssociationId = Nothing, _niaPublicDNSName = Nothing, _niaAllocationId = Nothing, _niaIPOwnerId = Nothing, _niaPublicIP = Nothing};

-- | The association ID.
niaAssociationId :: Lens' NetworkInterfaceAssociation (Maybe Text)
niaAssociationId = lens _niaAssociationId (\ s a -> s{_niaAssociationId = a});

-- | The public DNS name.
niaPublicDNSName :: Lens' NetworkInterfaceAssociation (Maybe Text)
niaPublicDNSName = lens _niaPublicDNSName (\ s a -> s{_niaPublicDNSName = a});

-- | The allocation ID.
niaAllocationId :: Lens' NetworkInterfaceAssociation (Maybe Text)
niaAllocationId = lens _niaAllocationId (\ s a -> s{_niaAllocationId = a});

-- | The ID of the Elastic IP address owner.
niaIPOwnerId :: Lens' NetworkInterfaceAssociation (Maybe Text)
niaIPOwnerId = lens _niaIPOwnerId (\ s a -> s{_niaIPOwnerId = a});

-- | The address of the Elastic IP address bound to the network interface.
niaPublicIP :: Lens' NetworkInterfaceAssociation (Maybe Text)
niaPublicIP = lens _niaPublicIP (\ s a -> s{_niaPublicIP = a});

instance FromXML NetworkInterfaceAssociation where
        parseXML x
          = NetworkInterfaceAssociation' <$>
              (x .@? "associationId") <*> (x .@? "publicDnsName")
                <*> (x .@? "allocationId")
                <*> (x .@? "ipOwnerId")
                <*> (x .@? "publicIp")

-- | Describes a network interface attachment.
--
-- /See:/ 'networkInterfaceAttachment' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'niaInstanceId'
--
-- * 'niaDeleteOnTermination'
--
-- * 'niaStatus'
--
-- * 'niaAttachmentId'
--
-- * 'niaInstanceOwnerId'
--
-- * 'niaAttachTime'
--
-- * 'niaDeviceIndex'
data NetworkInterfaceAttachment = NetworkInterfaceAttachment'{_niaInstanceId :: Maybe Text, _niaDeleteOnTermination :: Maybe Bool, _niaStatus :: Maybe AttachmentStatus, _niaAttachmentId :: Maybe Text, _niaInstanceOwnerId :: Maybe Text, _niaAttachTime :: Maybe ISO8601, _niaDeviceIndex :: Maybe Int} deriving (Eq, Read, Show)

-- | 'NetworkInterfaceAttachment' smart constructor.
networkInterfaceAttachment :: NetworkInterfaceAttachment
networkInterfaceAttachment = NetworkInterfaceAttachment'{_niaInstanceId = Nothing, _niaDeleteOnTermination = Nothing, _niaStatus = Nothing, _niaAttachmentId = Nothing, _niaInstanceOwnerId = Nothing, _niaAttachTime = Nothing, _niaDeviceIndex = Nothing};

-- | The ID of the instance.
niaInstanceId :: Lens' NetworkInterfaceAttachment (Maybe Text)
niaInstanceId = lens _niaInstanceId (\ s a -> s{_niaInstanceId = a});

-- | Indicates whether the network interface is deleted when the instance is
-- terminated.
niaDeleteOnTermination :: Lens' NetworkInterfaceAttachment (Maybe Bool)
niaDeleteOnTermination = lens _niaDeleteOnTermination (\ s a -> s{_niaDeleteOnTermination = a});

-- | The attachment state.
niaStatus :: Lens' NetworkInterfaceAttachment (Maybe AttachmentStatus)
niaStatus = lens _niaStatus (\ s a -> s{_niaStatus = a});

-- | The ID of the network interface attachment.
niaAttachmentId :: Lens' NetworkInterfaceAttachment (Maybe Text)
niaAttachmentId = lens _niaAttachmentId (\ s a -> s{_niaAttachmentId = a});

-- | The AWS account ID of the owner of the instance.
niaInstanceOwnerId :: Lens' NetworkInterfaceAttachment (Maybe Text)
niaInstanceOwnerId = lens _niaInstanceOwnerId (\ s a -> s{_niaInstanceOwnerId = a});

-- | The timestamp indicating when the attachment initiated.
niaAttachTime :: Lens' NetworkInterfaceAttachment (Maybe UTCTime)
niaAttachTime = lens _niaAttachTime (\ s a -> s{_niaAttachTime = a}) . mapping _Time;

-- | The device index of the network interface attachment on the instance.
niaDeviceIndex :: Lens' NetworkInterfaceAttachment (Maybe Int)
niaDeviceIndex = lens _niaDeviceIndex (\ s a -> s{_niaDeviceIndex = a});

instance FromXML NetworkInterfaceAttachment where
        parseXML x
          = NetworkInterfaceAttachment' <$>
              (x .@? "instanceId") <*>
                (x .@? "deleteOnTermination")
                <*> (x .@? "status")
                <*> (x .@? "attachmentId")
                <*> (x .@? "instanceOwnerId")
                <*> (x .@? "attachTime")
                <*> (x .@? "deviceIndex")

-- | Describes an attachment change.
--
-- /See:/ 'networkInterfaceAttachmentChanges' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'niacDeleteOnTermination'
--
-- * 'niacAttachmentId'
data NetworkInterfaceAttachmentChanges = NetworkInterfaceAttachmentChanges'{_niacDeleteOnTermination :: Maybe Bool, _niacAttachmentId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'NetworkInterfaceAttachmentChanges' smart constructor.
networkInterfaceAttachmentChanges :: NetworkInterfaceAttachmentChanges
networkInterfaceAttachmentChanges = NetworkInterfaceAttachmentChanges'{_niacDeleteOnTermination = Nothing, _niacAttachmentId = Nothing};

-- | Indicates whether the network interface is deleted when the instance is
-- terminated.
niacDeleteOnTermination :: Lens' NetworkInterfaceAttachmentChanges (Maybe Bool)
niacDeleteOnTermination = lens _niacDeleteOnTermination (\ s a -> s{_niacDeleteOnTermination = a});

-- | The ID of the network interface attachment.
niacAttachmentId :: Lens' NetworkInterfaceAttachmentChanges (Maybe Text)
niacAttachmentId = lens _niacAttachmentId (\ s a -> s{_niacAttachmentId = a});

instance ToQuery NetworkInterfaceAttachmentChanges
         where
        toQuery NetworkInterfaceAttachmentChanges'{..}
          = mconcat
              ["DeleteOnTermination" =: _niacDeleteOnTermination,
               "AttachmentId" =: _niacAttachmentId]

-- | Describes the private IP address of a network interface.
--
-- /See:/ 'networkInterfacePrivateIPAddress' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'nipiaPrimary'
--
-- * 'nipiaPrivateIPAddress'
--
-- * 'nipiaPrivateDNSName'
--
-- * 'nipiaAssociation'
data NetworkInterfacePrivateIPAddress = NetworkInterfacePrivateIPAddress'{_nipiaPrimary :: Maybe Bool, _nipiaPrivateIPAddress :: Maybe Text, _nipiaPrivateDNSName :: Maybe Text, _nipiaAssociation :: Maybe NetworkInterfaceAssociation} deriving (Eq, Read, Show)

-- | 'NetworkInterfacePrivateIPAddress' smart constructor.
networkInterfacePrivateIPAddress :: NetworkInterfacePrivateIPAddress
networkInterfacePrivateIPAddress = NetworkInterfacePrivateIPAddress'{_nipiaPrimary = Nothing, _nipiaPrivateIPAddress = Nothing, _nipiaPrivateDNSName = Nothing, _nipiaAssociation = Nothing};

-- | Indicates whether this IP address is the primary private IP address of
-- the network interface.
nipiaPrimary :: Lens' NetworkInterfacePrivateIPAddress (Maybe Bool)
nipiaPrimary = lens _nipiaPrimary (\ s a -> s{_nipiaPrimary = a});

-- | The private IP address.
nipiaPrivateIPAddress :: Lens' NetworkInterfacePrivateIPAddress (Maybe Text)
nipiaPrivateIPAddress = lens _nipiaPrivateIPAddress (\ s a -> s{_nipiaPrivateIPAddress = a});

-- | The private DNS name.
nipiaPrivateDNSName :: Lens' NetworkInterfacePrivateIPAddress (Maybe Text)
nipiaPrivateDNSName = lens _nipiaPrivateDNSName (\ s a -> s{_nipiaPrivateDNSName = a});

-- | The association information for an Elastic IP address associated with
-- the network interface.
nipiaAssociation :: Lens' NetworkInterfacePrivateIPAddress (Maybe NetworkInterfaceAssociation)
nipiaAssociation = lens _nipiaAssociation (\ s a -> s{_nipiaAssociation = a});

instance FromXML NetworkInterfacePrivateIPAddress
         where
        parseXML x
          = NetworkInterfacePrivateIPAddress' <$>
              (x .@? "primary") <*> (x .@? "privateIpAddress") <*>
                (x .@? "privateDnsName")
                <*> (x .@? "association")

-- | /See:/ 'newDHCPConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ndcValues'
--
-- * 'ndcKey'
data NewDHCPConfiguration = NewDHCPConfiguration'{_ndcValues :: Maybe [Text], _ndcKey :: Maybe Text} deriving (Eq, Read, Show)

-- | 'NewDHCPConfiguration' smart constructor.
newDHCPConfiguration :: NewDHCPConfiguration
newDHCPConfiguration = NewDHCPConfiguration'{_ndcValues = Nothing, _ndcKey = Nothing};

-- | FIXME: Undocumented member.
ndcValues :: Lens' NewDHCPConfiguration [Text]
ndcValues = lens _ndcValues (\ s a -> s{_ndcValues = a}) . _Default;

-- | FIXME: Undocumented member.
ndcKey :: Lens' NewDHCPConfiguration (Maybe Text)
ndcKey = lens _ndcKey (\ s a -> s{_ndcKey = a});

instance ToQuery NewDHCPConfiguration where
        toQuery NewDHCPConfiguration'{..}
          = mconcat
              [toQuery (toQueryList "item" <$> _ndcValues),
               "Key" =: _ndcKey]

-- | Describes the placement for the instance.
--
-- /See:/ 'placement' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'plaAvailabilityZone'
--
-- * 'plaTenancy'
--
-- * 'plaGroupName'
data Placement = Placement'{_plaAvailabilityZone :: Maybe Text, _plaTenancy :: Maybe Tenancy, _plaGroupName :: Maybe Text} deriving (Eq, Read, Show)

-- | 'Placement' smart constructor.
placement :: Placement
placement = Placement'{_plaAvailabilityZone = Nothing, _plaTenancy = Nothing, _plaGroupName = Nothing};

-- | The Availability Zone of the instance.
plaAvailabilityZone :: Lens' Placement (Maybe Text)
plaAvailabilityZone = lens _plaAvailabilityZone (\ s a -> s{_plaAvailabilityZone = a});

-- | The tenancy of the instance (if the instance is running in a VPC). An
-- instance with a tenancy of @dedicated@ runs on single-tenant hardware.
plaTenancy :: Lens' Placement (Maybe Tenancy)
plaTenancy = lens _plaTenancy (\ s a -> s{_plaTenancy = a});

-- | The name of the placement group the instance is in (for cluster compute
-- instances).
plaGroupName :: Lens' Placement (Maybe Text)
plaGroupName = lens _plaGroupName (\ s a -> s{_plaGroupName = a});

instance FromXML Placement where
        parseXML x
          = Placement' <$>
              (x .@? "availabilityZone") <*> (x .@? "tenancy") <*>
                (x .@? "groupName")

instance ToQuery Placement where
        toQuery Placement'{..}
          = mconcat
              ["AvailabilityZone" =: _plaAvailabilityZone,
               "Tenancy" =: _plaTenancy,
               "GroupName" =: _plaGroupName]

-- | Describes a placement group.
--
-- /See:/ 'placementGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pgState'
--
-- * 'pgStrategy'
--
-- * 'pgGroupName'
data PlacementGroup = PlacementGroup'{_pgState :: Maybe PlacementGroupState, _pgStrategy :: Maybe PlacementStrategy, _pgGroupName :: Maybe Text} deriving (Eq, Read, Show)

-- | 'PlacementGroup' smart constructor.
placementGroup :: PlacementGroup
placementGroup = PlacementGroup'{_pgState = Nothing, _pgStrategy = Nothing, _pgGroupName = Nothing};

-- | The state of the placement group.
pgState :: Lens' PlacementGroup (Maybe PlacementGroupState)
pgState = lens _pgState (\ s a -> s{_pgState = a});

-- | The placement strategy.
pgStrategy :: Lens' PlacementGroup (Maybe PlacementStrategy)
pgStrategy = lens _pgStrategy (\ s a -> s{_pgStrategy = a});

-- | The name of the placement group.
pgGroupName :: Lens' PlacementGroup (Maybe Text)
pgGroupName = lens _pgGroupName (\ s a -> s{_pgGroupName = a});

instance FromXML PlacementGroup where
        parseXML x
          = PlacementGroup' <$>
              (x .@? "state") <*> (x .@? "strategy") <*>
                (x .@? "groupName")

-- | Describes a range of ports.
--
-- /See:/ 'portRange' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prTo'
--
-- * 'prFrom'
data PortRange = PortRange'{_prTo :: Maybe Int, _prFrom :: Maybe Int} deriving (Eq, Read, Show)

-- | 'PortRange' smart constructor.
portRange :: PortRange
portRange = PortRange'{_prTo = Nothing, _prFrom = Nothing};

-- | The last port in the range.
prTo :: Lens' PortRange (Maybe Int)
prTo = lens _prTo (\ s a -> s{_prTo = a});

-- | The first port in the range.
prFrom :: Lens' PortRange (Maybe Int)
prFrom = lens _prFrom (\ s a -> s{_prFrom = a});

instance FromXML PortRange where
        parseXML x
          = PortRange' <$> (x .@? "to") <*> (x .@? "from")

instance ToQuery PortRange where
        toQuery PortRange'{..}
          = mconcat ["To" =: _prTo, "From" =: _prFrom]

-- | Describes prefixes for AWS services.
--
-- /See:/ 'prefixList' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'plCIDRs'
--
-- * 'plPrefixListId'
--
-- * 'plPrefixListName'
data PrefixList = PrefixList'{_plCIDRs :: Maybe [Text], _plPrefixListId :: Maybe Text, _plPrefixListName :: Maybe Text} deriving (Eq, Read, Show)

-- | 'PrefixList' smart constructor.
prefixList :: PrefixList
prefixList = PrefixList'{_plCIDRs = Nothing, _plPrefixListId = Nothing, _plPrefixListName = Nothing};

-- | The IP address range of the AWS service.
plCIDRs :: Lens' PrefixList [Text]
plCIDRs = lens _plCIDRs (\ s a -> s{_plCIDRs = a}) . _Default;

-- | The ID of the prefix.
plPrefixListId :: Lens' PrefixList (Maybe Text)
plPrefixListId = lens _plPrefixListId (\ s a -> s{_plPrefixListId = a});

-- | The name of the prefix.
plPrefixListName :: Lens' PrefixList (Maybe Text)
plPrefixListName = lens _plPrefixListName (\ s a -> s{_plPrefixListName = a});

instance FromXML PrefixList where
        parseXML x
          = PrefixList' <$>
              (may (parseXMLList "item") x) <*>
                (x .@? "prefixListId")
                <*> (x .@? "prefixListName")

-- | The ID of the prefix.
--
-- /See:/ 'prefixListId' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pliPrefixListId'
newtype PrefixListId = PrefixListId'{_pliPrefixListId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'PrefixListId' smart constructor.
prefixListId :: PrefixListId
prefixListId = PrefixListId'{_pliPrefixListId = Nothing};

-- | The ID of the prefix.
pliPrefixListId :: Lens' PrefixListId (Maybe Text)
pliPrefixListId = lens _pliPrefixListId (\ s a -> s{_pliPrefixListId = a});

instance FromXML PrefixListId where
        parseXML x = PrefixListId' <$> (x .@? "prefixListId")

instance ToQuery PrefixListId where
        toQuery PrefixListId'{..}
          = mconcat ["PrefixListId" =: _pliPrefixListId]

-- | Describes the price for a Reserved Instance.
--
-- /See:/ 'priceSchedule' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'psCurrencyCode'
--
-- * 'psTerm'
--
-- * 'psActive'
--
-- * 'psPrice'
data PriceSchedule = PriceSchedule'{_psCurrencyCode :: Maybe CurrencyCodeValues, _psTerm :: Maybe Integer, _psActive :: Maybe Bool, _psPrice :: Maybe Double} deriving (Eq, Read, Show)

-- | 'PriceSchedule' smart constructor.
priceSchedule :: PriceSchedule
priceSchedule = PriceSchedule'{_psCurrencyCode = Nothing, _psTerm = Nothing, _psActive = Nothing, _psPrice = Nothing};

-- | The currency for transacting the Reserved Instance resale. At this time,
-- the only supported currency is @USD@.
psCurrencyCode :: Lens' PriceSchedule (Maybe CurrencyCodeValues)
psCurrencyCode = lens _psCurrencyCode (\ s a -> s{_psCurrencyCode = a});

-- | The number of months remaining in the reservation. For example, 2 is the
-- second to the last month before the capacity reservation expires.
psTerm :: Lens' PriceSchedule (Maybe Integer)
psTerm = lens _psTerm (\ s a -> s{_psTerm = a});

-- | The current price schedule, as determined by the term remaining for the
-- Reserved Instance in the listing.
--
-- A specific price schedule is always in effect, but only one price
-- schedule can be active at any time. Take, for example, a Reserved
-- Instance listing that has five months remaining in its term. When you
-- specify price schedules for five months and two months, this means that
-- schedule 1, covering the first three months of the remaining term, will
-- be active during months 5, 4, and 3. Then schedule 2, covering the last
-- two months of the term, will be active for months 2 and 1.
psActive :: Lens' PriceSchedule (Maybe Bool)
psActive = lens _psActive (\ s a -> s{_psActive = a});

-- | The fixed price for the term.
psPrice :: Lens' PriceSchedule (Maybe Double)
psPrice = lens _psPrice (\ s a -> s{_psPrice = a});

instance FromXML PriceSchedule where
        parseXML x
          = PriceSchedule' <$>
              (x .@? "currencyCode") <*> (x .@? "term") <*>
                (x .@? "active")
                <*> (x .@? "price")

-- | Describes the price for a Reserved Instance.
--
-- /See:/ 'priceScheduleSpecification' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pssCurrencyCode'
--
-- * 'pssTerm'
--
-- * 'pssPrice'
data PriceScheduleSpecification = PriceScheduleSpecification'{_pssCurrencyCode :: Maybe CurrencyCodeValues, _pssTerm :: Maybe Integer, _pssPrice :: Maybe Double} deriving (Eq, Read, Show)

-- | 'PriceScheduleSpecification' smart constructor.
priceScheduleSpecification :: PriceScheduleSpecification
priceScheduleSpecification = PriceScheduleSpecification'{_pssCurrencyCode = Nothing, _pssTerm = Nothing, _pssPrice = Nothing};

-- | The currency for transacting the Reserved Instance resale. At this time,
-- the only supported currency is @USD@.
pssCurrencyCode :: Lens' PriceScheduleSpecification (Maybe CurrencyCodeValues)
pssCurrencyCode = lens _pssCurrencyCode (\ s a -> s{_pssCurrencyCode = a});

-- | The number of months remaining in the reservation. For example, 2 is the
-- second to the last month before the capacity reservation expires.
pssTerm :: Lens' PriceScheduleSpecification (Maybe Integer)
pssTerm = lens _pssTerm (\ s a -> s{_pssTerm = a});

-- | The fixed price for the term.
pssPrice :: Lens' PriceScheduleSpecification (Maybe Double)
pssPrice = lens _pssPrice (\ s a -> s{_pssPrice = a});

instance ToQuery PriceScheduleSpecification where
        toQuery PriceScheduleSpecification'{..}
          = mconcat
              ["CurrencyCode" =: _pssCurrencyCode,
               "Term" =: _pssTerm, "Price" =: _pssPrice]

-- | Describes a Reserved Instance offering.
--
-- /See:/ 'pricingDetail' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pdCount'
--
-- * 'pdPrice'
data PricingDetail = PricingDetail'{_pdCount :: Maybe Int, _pdPrice :: Maybe Double} deriving (Eq, Read, Show)

-- | 'PricingDetail' smart constructor.
pricingDetail :: PricingDetail
pricingDetail = PricingDetail'{_pdCount = Nothing, _pdPrice = Nothing};

-- | The number of instances available for the price.
pdCount :: Lens' PricingDetail (Maybe Int)
pdCount = lens _pdCount (\ s a -> s{_pdCount = a});

-- | The price per instance.
pdPrice :: Lens' PricingDetail (Maybe Double)
pdPrice = lens _pdPrice (\ s a -> s{_pdPrice = a});

instance FromXML PricingDetail where
        parseXML x
          = PricingDetail' <$>
              (x .@? "count") <*> (x .@? "price")

-- | Describes a secondary private IP address for a network interface.
--
-- /See:/ 'privateIPAddressSpecification' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'piasPrimary'
--
-- * 'piasPrivateIPAddress'
data PrivateIPAddressSpecification = PrivateIPAddressSpecification'{_piasPrimary :: Maybe Bool, _piasPrivateIPAddress :: Text} deriving (Eq, Read, Show)

-- | 'PrivateIPAddressSpecification' smart constructor.
privateIPAddressSpecification :: Text -> PrivateIPAddressSpecification
privateIPAddressSpecification pPrivateIPAddress = PrivateIPAddressSpecification'{_piasPrimary = Nothing, _piasPrivateIPAddress = pPrivateIPAddress};

-- | Indicates whether the private IP address is the primary private IP
-- address. Only one IP address can be designated as primary.
piasPrimary :: Lens' PrivateIPAddressSpecification (Maybe Bool)
piasPrimary = lens _piasPrimary (\ s a -> s{_piasPrimary = a});

-- | The private IP addresses.
piasPrivateIPAddress :: Lens' PrivateIPAddressSpecification Text
piasPrivateIPAddress = lens _piasPrivateIPAddress (\ s a -> s{_piasPrivateIPAddress = a});

instance FromXML PrivateIPAddressSpecification where
        parseXML x
          = PrivateIPAddressSpecification' <$>
              (x .@? "primary") <*> (x .@ "privateIpAddress")

instance ToQuery PrivateIPAddressSpecification where
        toQuery PrivateIPAddressSpecification'{..}
          = mconcat
              ["Primary" =: _piasPrimary,
               "PrivateIpAddress" =: _piasPrivateIPAddress]

-- | Describes a product code.
--
-- /See:/ 'productCode' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pcProductCodeType'
--
-- * 'pcProductCodeId'
data ProductCode = ProductCode'{_pcProductCodeType :: Maybe ProductCodeValues, _pcProductCodeId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'ProductCode' smart constructor.
productCode :: ProductCode
productCode = ProductCode'{_pcProductCodeType = Nothing, _pcProductCodeId = Nothing};

-- | The type of product code.
pcProductCodeType :: Lens' ProductCode (Maybe ProductCodeValues)
pcProductCodeType = lens _pcProductCodeType (\ s a -> s{_pcProductCodeType = a});

-- | The product code.
pcProductCodeId :: Lens' ProductCode (Maybe Text)
pcProductCodeId = lens _pcProductCodeId (\ s a -> s{_pcProductCodeId = a});

instance FromXML ProductCode where
        parseXML x
          = ProductCode' <$>
              (x .@? "type") <*> (x .@? "productCode")

-- | Describes a virtual private gateway propagating route.
--
-- /See:/ 'propagatingVGW' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pvGatewayId'
newtype PropagatingVGW = PropagatingVGW'{_pvGatewayId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'PropagatingVGW' smart constructor.
propagatingVGW :: PropagatingVGW
propagatingVGW = PropagatingVGW'{_pvGatewayId = Nothing};

-- | The ID of the virtual private gateway (VGW).
pvGatewayId :: Lens' PropagatingVGW (Maybe Text)
pvGatewayId = lens _pvGatewayId (\ s a -> s{_pvGatewayId = a});

instance FromXML PropagatingVGW where
        parseXML x = PropagatingVGW' <$> (x .@? "gatewayId")

-- | Describes a recurring charge.
--
-- /See:/ 'recurringCharge' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcFrequency'
--
-- * 'rcAmount'
data RecurringCharge = RecurringCharge'{_rcFrequency :: Maybe RecurringChargeFrequency, _rcAmount :: Maybe Double} deriving (Eq, Read, Show)

-- | 'RecurringCharge' smart constructor.
recurringCharge :: RecurringCharge
recurringCharge = RecurringCharge'{_rcFrequency = Nothing, _rcAmount = Nothing};

-- | The frequency of the recurring charge.
rcFrequency :: Lens' RecurringCharge (Maybe RecurringChargeFrequency)
rcFrequency = lens _rcFrequency (\ s a -> s{_rcFrequency = a});

-- | The amount of the recurring charge.
rcAmount :: Lens' RecurringCharge (Maybe Double)
rcAmount = lens _rcAmount (\ s a -> s{_rcAmount = a});

instance FromXML RecurringCharge where
        parseXML x
          = RecurringCharge' <$>
              (x .@? "frequency") <*> (x .@? "amount")

-- | Describes a region.
--
-- /See:/ 'regionInfo' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'riRegionName'
--
-- * 'riEndpoint'
data RegionInfo = RegionInfo'{_riRegionName :: Maybe Text, _riEndpoint :: Maybe Text} deriving (Eq, Read, Show)

-- | 'RegionInfo' smart constructor.
regionInfo :: RegionInfo
regionInfo = RegionInfo'{_riRegionName = Nothing, _riEndpoint = Nothing};

-- | The name of the region.
riRegionName :: Lens' RegionInfo (Maybe Text)
riRegionName = lens _riRegionName (\ s a -> s{_riRegionName = a});

-- | The region service endpoint.
riEndpoint :: Lens' RegionInfo (Maybe Text)
riEndpoint = lens _riEndpoint (\ s a -> s{_riEndpoint = a});

instance FromXML RegionInfo where
        parseXML x
          = RegionInfo' <$>
              (x .@? "regionName") <*> (x .@? "regionEndpoint")

-- | Describes the launch specification for an instance.
--
-- /See:/ 'requestSpotLaunchSpecification' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rslsSecurityGroupIds'
--
-- * 'rslsSecurityGroups'
--
-- * 'rslsNetworkInterfaces'
--
-- * 'rslsKeyName'
--
-- * 'rslsRAMDiskId'
--
-- * 'rslsKernelId'
--
-- * 'rslsSubnetId'
--
-- * 'rslsInstanceType'
--
-- * 'rslsEBSOptimized'
--
-- * 'rslsUserData'
--
-- * 'rslsMonitoring'
--
-- * 'rslsIAMInstanceProfile'
--
-- * 'rslsImageId'
--
-- * 'rslsBlockDeviceMappings'
--
-- * 'rslsAddressingType'
--
-- * 'rslsPlacement'
data RequestSpotLaunchSpecification = RequestSpotLaunchSpecification'{_rslsSecurityGroupIds :: Maybe [Text], _rslsSecurityGroups :: Maybe [Text], _rslsNetworkInterfaces :: Maybe [InstanceNetworkInterfaceSpecification], _rslsKeyName :: Maybe Text, _rslsRAMDiskId :: Maybe Text, _rslsKernelId :: Maybe Text, _rslsSubnetId :: Maybe Text, _rslsInstanceType :: Maybe InstanceType, _rslsEBSOptimized :: Maybe Bool, _rslsUserData :: Maybe Text, _rslsMonitoring :: Maybe RunInstancesMonitoringEnabled, _rslsIAMInstanceProfile :: Maybe IAMInstanceProfileSpecification, _rslsImageId :: Maybe Text, _rslsBlockDeviceMappings :: Maybe [BlockDeviceMapping], _rslsAddressingType :: Maybe Text, _rslsPlacement :: Maybe SpotPlacement} deriving (Eq, Read, Show)

-- | 'RequestSpotLaunchSpecification' smart constructor.
requestSpotLaunchSpecification :: RequestSpotLaunchSpecification
requestSpotLaunchSpecification = RequestSpotLaunchSpecification'{_rslsSecurityGroupIds = Nothing, _rslsSecurityGroups = Nothing, _rslsNetworkInterfaces = Nothing, _rslsKeyName = Nothing, _rslsRAMDiskId = Nothing, _rslsKernelId = Nothing, _rslsSubnetId = Nothing, _rslsInstanceType = Nothing, _rslsEBSOptimized = Nothing, _rslsUserData = Nothing, _rslsMonitoring = Nothing, _rslsIAMInstanceProfile = Nothing, _rslsImageId = Nothing, _rslsBlockDeviceMappings = Nothing, _rslsAddressingType = Nothing, _rslsPlacement = Nothing};

-- | FIXME: Undocumented member.
rslsSecurityGroupIds :: Lens' RequestSpotLaunchSpecification [Text]
rslsSecurityGroupIds = lens _rslsSecurityGroupIds (\ s a -> s{_rslsSecurityGroupIds = a}) . _Default;

-- | FIXME: Undocumented member.
rslsSecurityGroups :: Lens' RequestSpotLaunchSpecification [Text]
rslsSecurityGroups = lens _rslsSecurityGroups (\ s a -> s{_rslsSecurityGroups = a}) . _Default;

-- | One or more network interfaces.
rslsNetworkInterfaces :: Lens' RequestSpotLaunchSpecification [InstanceNetworkInterfaceSpecification]
rslsNetworkInterfaces = lens _rslsNetworkInterfaces (\ s a -> s{_rslsNetworkInterfaces = a}) . _Default;

-- | The name of the key pair.
rslsKeyName :: Lens' RequestSpotLaunchSpecification (Maybe Text)
rslsKeyName = lens _rslsKeyName (\ s a -> s{_rslsKeyName = a});

-- | The ID of the RAM disk.
rslsRAMDiskId :: Lens' RequestSpotLaunchSpecification (Maybe Text)
rslsRAMDiskId = lens _rslsRAMDiskId (\ s a -> s{_rslsRAMDiskId = a});

-- | The ID of the kernel.
rslsKernelId :: Lens' RequestSpotLaunchSpecification (Maybe Text)
rslsKernelId = lens _rslsKernelId (\ s a -> s{_rslsKernelId = a});

-- | The ID of the subnet in which to launch the instance.
rslsSubnetId :: Lens' RequestSpotLaunchSpecification (Maybe Text)
rslsSubnetId = lens _rslsSubnetId (\ s a -> s{_rslsSubnetId = a});

-- | The instance type.
rslsInstanceType :: Lens' RequestSpotLaunchSpecification (Maybe InstanceType)
rslsInstanceType = lens _rslsInstanceType (\ s a -> s{_rslsInstanceType = a});

-- | Indicates whether the instance is optimized for EBS I\/O. This
-- optimization provides dedicated throughput to Amazon EBS and an
-- optimized configuration stack to provide optimal EBS I\/O performance.
-- This optimization isn\'t available with all instance types. Additional
-- usage charges apply when using an EBS Optimized instance.
--
-- Default: @false@
rslsEBSOptimized :: Lens' RequestSpotLaunchSpecification (Maybe Bool)
rslsEBSOptimized = lens _rslsEBSOptimized (\ s a -> s{_rslsEBSOptimized = a});

-- | The Base64-encoded MIME user data to make available to the instances.
rslsUserData :: Lens' RequestSpotLaunchSpecification (Maybe Text)
rslsUserData = lens _rslsUserData (\ s a -> s{_rslsUserData = a});

-- | FIXME: Undocumented member.
rslsMonitoring :: Lens' RequestSpotLaunchSpecification (Maybe RunInstancesMonitoringEnabled)
rslsMonitoring = lens _rslsMonitoring (\ s a -> s{_rslsMonitoring = a});

-- | The IAM instance profile.
rslsIAMInstanceProfile :: Lens' RequestSpotLaunchSpecification (Maybe IAMInstanceProfileSpecification)
rslsIAMInstanceProfile = lens _rslsIAMInstanceProfile (\ s a -> s{_rslsIAMInstanceProfile = a});

-- | The ID of the AMI.
rslsImageId :: Lens' RequestSpotLaunchSpecification (Maybe Text)
rslsImageId = lens _rslsImageId (\ s a -> s{_rslsImageId = a});

-- | One or more block device mapping entries.
rslsBlockDeviceMappings :: Lens' RequestSpotLaunchSpecification [BlockDeviceMapping]
rslsBlockDeviceMappings = lens _rslsBlockDeviceMappings (\ s a -> s{_rslsBlockDeviceMappings = a}) . _Default;

-- | Deprecated.
rslsAddressingType :: Lens' RequestSpotLaunchSpecification (Maybe Text)
rslsAddressingType = lens _rslsAddressingType (\ s a -> s{_rslsAddressingType = a});

-- | The placement information for the instance.
rslsPlacement :: Lens' RequestSpotLaunchSpecification (Maybe SpotPlacement)
rslsPlacement = lens _rslsPlacement (\ s a -> s{_rslsPlacement = a});

instance ToQuery RequestSpotLaunchSpecification where
        toQuery RequestSpotLaunchSpecification'{..}
          = mconcat
              [toQuery
                 (toQueryList "item" <$> _rslsSecurityGroupIds),
               toQuery (toQueryList "item" <$> _rslsSecurityGroups),
               toQuery
                 (toQueryList "item" <$> _rslsNetworkInterfaces),
               "KeyName" =: _rslsKeyName,
               "RamdiskId" =: _rslsRAMDiskId,
               "KernelId" =: _rslsKernelId,
               "SubnetId" =: _rslsSubnetId,
               "InstanceType" =: _rslsInstanceType,
               "EbsOptimized" =: _rslsEBSOptimized,
               "UserData" =: _rslsUserData,
               "Monitoring" =: _rslsMonitoring,
               "IamInstanceProfile" =: _rslsIAMInstanceProfile,
               "ImageId" =: _rslsImageId,
               toQuery
                 (toQueryList "item" <$> _rslsBlockDeviceMappings),
               "AddressingType" =: _rslsAddressingType,
               "Placement" =: _rslsPlacement]

-- | Describes a reservation.
--
-- /See:/ 'reservation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'resGroups'
--
-- * 'resInstances'
--
-- * 'resRequesterId'
--
-- * 'resReservationId'
--
-- * 'resOwnerId'
data Reservation = Reservation'{_resGroups :: Maybe [GroupIdentifier], _resInstances :: Maybe [Instance], _resRequesterId :: Maybe Text, _resReservationId :: Text, _resOwnerId :: Text} deriving (Eq, Read, Show)

-- | 'Reservation' smart constructor.
reservation :: Text -> Text -> Reservation
reservation pReservationId pOwnerId = Reservation'{_resGroups = Nothing, _resInstances = Nothing, _resRequesterId = Nothing, _resReservationId = pReservationId, _resOwnerId = pOwnerId};

-- | One or more security groups.
resGroups :: Lens' Reservation [GroupIdentifier]
resGroups = lens _resGroups (\ s a -> s{_resGroups = a}) . _Default;

-- | One or more instances.
resInstances :: Lens' Reservation [Instance]
resInstances = lens _resInstances (\ s a -> s{_resInstances = a}) . _Default;

-- | The ID of the requester that launched the instances on your behalf (for
-- example, AWS Management Console or Auto Scaling).
resRequesterId :: Lens' Reservation (Maybe Text)
resRequesterId = lens _resRequesterId (\ s a -> s{_resRequesterId = a});

-- | The ID of the reservation.
resReservationId :: Lens' Reservation Text
resReservationId = lens _resReservationId (\ s a -> s{_resReservationId = a});

-- | The ID of the AWS account that owns the reservation.
resOwnerId :: Lens' Reservation Text
resOwnerId = lens _resOwnerId (\ s a -> s{_resOwnerId = a});

instance FromXML Reservation where
        parseXML x
          = Reservation' <$>
              (may (parseXMLList "item") x) <*>
                (may (parseXMLList "item") x)
                <*> (x .@? "requesterId")
                <*> (x .@ "reservationId")
                <*> (x .@ "ownerId")

-- | Describes the limit price of a Reserved Instance offering.
--
-- /See:/ 'reservedInstanceLimitPrice' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rilpAmount'
--
-- * 'rilpCurrencyCode'
data ReservedInstanceLimitPrice = ReservedInstanceLimitPrice'{_rilpAmount :: Maybe Double, _rilpCurrencyCode :: Maybe CurrencyCodeValues} deriving (Eq, Read, Show)

-- | 'ReservedInstanceLimitPrice' smart constructor.
reservedInstanceLimitPrice :: ReservedInstanceLimitPrice
reservedInstanceLimitPrice = ReservedInstanceLimitPrice'{_rilpAmount = Nothing, _rilpCurrencyCode = Nothing};

-- | Used for Reserved Instance Marketplace offerings. Specifies the limit
-- price on the total order (instanceCount * price).
rilpAmount :: Lens' ReservedInstanceLimitPrice (Maybe Double)
rilpAmount = lens _rilpAmount (\ s a -> s{_rilpAmount = a});

-- | The currency in which the @limitPrice@ amount is specified. At this
-- time, the only supported currency is @USD@.
rilpCurrencyCode :: Lens' ReservedInstanceLimitPrice (Maybe CurrencyCodeValues)
rilpCurrencyCode = lens _rilpCurrencyCode (\ s a -> s{_rilpCurrencyCode = a});

instance ToQuery ReservedInstanceLimitPrice where
        toQuery ReservedInstanceLimitPrice'{..}
          = mconcat
              ["Amount" =: _rilpAmount,
               "CurrencyCode" =: _rilpCurrencyCode]

-- | Describes a Reserved Instance.
--
-- /See:/ 'reservedInstances' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'riState'
--
-- * 'riCurrencyCode'
--
-- * 'riInstanceCount'
--
-- * 'riProductDescription'
--
-- * 'riStart'
--
-- * 'riInstanceType'
--
-- * 'riAvailabilityZone'
--
-- * 'riEnd'
--
-- * 'riOfferingType'
--
-- * 'riUsagePrice'
--
-- * 'riRecurringCharges'
--
-- * 'riInstanceTenancy'
--
-- * 'riFixedPrice'
--
-- * 'riReservedInstancesId'
--
-- * 'riDuration'
--
-- * 'riTags'
data ReservedInstances = ReservedInstances'{_riState :: Maybe ReservedInstanceState, _riCurrencyCode :: Maybe CurrencyCodeValues, _riInstanceCount :: Maybe Int, _riProductDescription :: Maybe RIProductDescription, _riStart :: Maybe ISO8601, _riInstanceType :: Maybe InstanceType, _riAvailabilityZone :: Maybe Text, _riEnd :: Maybe ISO8601, _riOfferingType :: Maybe OfferingTypeValues, _riUsagePrice :: Maybe Double, _riRecurringCharges :: Maybe [RecurringCharge], _riInstanceTenancy :: Maybe Tenancy, _riFixedPrice :: Maybe Double, _riReservedInstancesId :: Maybe Text, _riDuration :: Maybe Integer, _riTags :: Maybe [Tag]} deriving (Eq, Read, Show)

-- | 'ReservedInstances' smart constructor.
reservedInstances :: ReservedInstances
reservedInstances = ReservedInstances'{_riState = Nothing, _riCurrencyCode = Nothing, _riInstanceCount = Nothing, _riProductDescription = Nothing, _riStart = Nothing, _riInstanceType = Nothing, _riAvailabilityZone = Nothing, _riEnd = Nothing, _riOfferingType = Nothing, _riUsagePrice = Nothing, _riRecurringCharges = Nothing, _riInstanceTenancy = Nothing, _riFixedPrice = Nothing, _riReservedInstancesId = Nothing, _riDuration = Nothing, _riTags = Nothing};

-- | The state of the Reserved Instance purchase.
riState :: Lens' ReservedInstances (Maybe ReservedInstanceState)
riState = lens _riState (\ s a -> s{_riState = a});

-- | The currency of the Reserved Instance. It\'s specified using ISO 4217
-- standard currency codes. At this time, the only supported currency is
-- @USD@.
riCurrencyCode :: Lens' ReservedInstances (Maybe CurrencyCodeValues)
riCurrencyCode = lens _riCurrencyCode (\ s a -> s{_riCurrencyCode = a});

-- | The number of Reserved Instances purchased.
riInstanceCount :: Lens' ReservedInstances (Maybe Int)
riInstanceCount = lens _riInstanceCount (\ s a -> s{_riInstanceCount = a});

-- | The Reserved Instance product platform description.
riProductDescription :: Lens' ReservedInstances (Maybe RIProductDescription)
riProductDescription = lens _riProductDescription (\ s a -> s{_riProductDescription = a});

-- | The date and time the Reserved Instance started.
riStart :: Lens' ReservedInstances (Maybe UTCTime)
riStart = lens _riStart (\ s a -> s{_riStart = a}) . mapping _Time;

-- | The instance type on which the Reserved Instance can be used.
riInstanceType :: Lens' ReservedInstances (Maybe InstanceType)
riInstanceType = lens _riInstanceType (\ s a -> s{_riInstanceType = a});

-- | The Availability Zone in which the Reserved Instance can be used.
riAvailabilityZone :: Lens' ReservedInstances (Maybe Text)
riAvailabilityZone = lens _riAvailabilityZone (\ s a -> s{_riAvailabilityZone = a});

-- | The time when the Reserved Instance expires.
riEnd :: Lens' ReservedInstances (Maybe UTCTime)
riEnd = lens _riEnd (\ s a -> s{_riEnd = a}) . mapping _Time;

-- | The Reserved Instance offering type.
riOfferingType :: Lens' ReservedInstances (Maybe OfferingTypeValues)
riOfferingType = lens _riOfferingType (\ s a -> s{_riOfferingType = a});

-- | The usage price of the Reserved Instance, per hour.
riUsagePrice :: Lens' ReservedInstances (Maybe Double)
riUsagePrice = lens _riUsagePrice (\ s a -> s{_riUsagePrice = a});

-- | The recurring charge tag assigned to the resource.
riRecurringCharges :: Lens' ReservedInstances [RecurringCharge]
riRecurringCharges = lens _riRecurringCharges (\ s a -> s{_riRecurringCharges = a}) . _Default;

-- | The tenancy of the reserved instance.
riInstanceTenancy :: Lens' ReservedInstances (Maybe Tenancy)
riInstanceTenancy = lens _riInstanceTenancy (\ s a -> s{_riInstanceTenancy = a});

-- | The purchase price of the Reserved Instance.
riFixedPrice :: Lens' ReservedInstances (Maybe Double)
riFixedPrice = lens _riFixedPrice (\ s a -> s{_riFixedPrice = a});

-- | The ID of the Reserved Instance.
riReservedInstancesId :: Lens' ReservedInstances (Maybe Text)
riReservedInstancesId = lens _riReservedInstancesId (\ s a -> s{_riReservedInstancesId = a});

-- | The duration of the Reserved Instance, in seconds.
riDuration :: Lens' ReservedInstances (Maybe Integer)
riDuration = lens _riDuration (\ s a -> s{_riDuration = a});

-- | Any tags assigned to the resource.
riTags :: Lens' ReservedInstances [Tag]
riTags = lens _riTags (\ s a -> s{_riTags = a}) . _Default;

instance FromXML ReservedInstances where
        parseXML x
          = ReservedInstances' <$>
              (x .@? "state") <*> (x .@? "currencyCode") <*>
                (x .@? "instanceCount")
                <*> (x .@? "productDescription")
                <*> (x .@? "start")
                <*> (x .@? "instanceType")
                <*> (x .@? "availabilityZone")
                <*> (x .@? "end")
                <*> (x .@? "offeringType")
                <*> (x .@? "usagePrice")
                <*> (may (parseXMLList "item") x)
                <*> (x .@? "instanceTenancy")
                <*> (x .@? "fixedPrice")
                <*> (x .@? "reservedInstancesId")
                <*> (x .@? "duration")
                <*> (may (parseXMLList "item") x)

-- | Describes the configuration settings for the modified Reserved
-- Instances.
--
-- /See:/ 'reservedInstancesConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ricPlatform'
--
-- * 'ricInstanceCount'
--
-- * 'ricInstanceType'
--
-- * 'ricAvailabilityZone'
data ReservedInstancesConfiguration = ReservedInstancesConfiguration'{_ricPlatform :: Maybe Text, _ricInstanceCount :: Maybe Int, _ricInstanceType :: Maybe InstanceType, _ricAvailabilityZone :: Maybe Text} deriving (Eq, Read, Show)

-- | 'ReservedInstancesConfiguration' smart constructor.
reservedInstancesConfiguration :: ReservedInstancesConfiguration
reservedInstancesConfiguration = ReservedInstancesConfiguration'{_ricPlatform = Nothing, _ricInstanceCount = Nothing, _ricInstanceType = Nothing, _ricAvailabilityZone = Nothing};

-- | The network platform of the modified Reserved Instances, which is either
-- EC2-Classic or EC2-VPC.
ricPlatform :: Lens' ReservedInstancesConfiguration (Maybe Text)
ricPlatform = lens _ricPlatform (\ s a -> s{_ricPlatform = a});

-- | The number of modified Reserved Instances.
ricInstanceCount :: Lens' ReservedInstancesConfiguration (Maybe Int)
ricInstanceCount = lens _ricInstanceCount (\ s a -> s{_ricInstanceCount = a});

-- | The instance type for the modified Reserved Instances.
ricInstanceType :: Lens' ReservedInstancesConfiguration (Maybe InstanceType)
ricInstanceType = lens _ricInstanceType (\ s a -> s{_ricInstanceType = a});

-- | The Availability Zone for the modified Reserved Instances.
ricAvailabilityZone :: Lens' ReservedInstancesConfiguration (Maybe Text)
ricAvailabilityZone = lens _ricAvailabilityZone (\ s a -> s{_ricAvailabilityZone = a});

instance FromXML ReservedInstancesConfiguration where
        parseXML x
          = ReservedInstancesConfiguration' <$>
              (x .@? "platform") <*> (x .@? "instanceCount") <*>
                (x .@? "instanceType")
                <*> (x .@? "availabilityZone")

instance ToQuery ReservedInstancesConfiguration where
        toQuery ReservedInstancesConfiguration'{..}
          = mconcat
              ["Platform" =: _ricPlatform,
               "InstanceCount" =: _ricInstanceCount,
               "InstanceType" =: _ricInstanceType,
               "AvailabilityZone" =: _ricAvailabilityZone]

-- | Describes the ID of a Reserved Instance.
--
-- /See:/ 'reservedInstancesId' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'riiReservedInstancesId'
newtype ReservedInstancesId = ReservedInstancesId'{_riiReservedInstancesId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'ReservedInstancesId' smart constructor.
reservedInstancesId :: ReservedInstancesId
reservedInstancesId = ReservedInstancesId'{_riiReservedInstancesId = Nothing};

-- | The ID of the Reserved Instance.
riiReservedInstancesId :: Lens' ReservedInstancesId (Maybe Text)
riiReservedInstancesId = lens _riiReservedInstancesId (\ s a -> s{_riiReservedInstancesId = a});

instance FromXML ReservedInstancesId where
        parseXML x
          = ReservedInstancesId' <$>
              (x .@? "reservedInstancesId")

-- | Describes a Reserved Instance listing.
--
-- /See:/ 'reservedInstancesListing' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rilStatus'
--
-- * 'rilClientToken'
--
-- * 'rilUpdateDate'
--
-- * 'rilCreateDate'
--
-- * 'rilPriceSchedules'
--
-- * 'rilStatusMessage'
--
-- * 'rilReservedInstancesId'
--
-- * 'rilInstanceCounts'
--
-- * 'rilReservedInstancesListingId'
--
-- * 'rilTags'
data ReservedInstancesListing = ReservedInstancesListing'{_rilStatus :: Maybe ListingStatus, _rilClientToken :: Maybe Text, _rilUpdateDate :: Maybe ISO8601, _rilCreateDate :: Maybe ISO8601, _rilPriceSchedules :: Maybe [PriceSchedule], _rilStatusMessage :: Maybe Text, _rilReservedInstancesId :: Maybe Text, _rilInstanceCounts :: Maybe [InstanceCount], _rilReservedInstancesListingId :: Maybe Text, _rilTags :: Maybe [Tag]} deriving (Eq, Read, Show)

-- | 'ReservedInstancesListing' smart constructor.
reservedInstancesListing :: ReservedInstancesListing
reservedInstancesListing = ReservedInstancesListing'{_rilStatus = Nothing, _rilClientToken = Nothing, _rilUpdateDate = Nothing, _rilCreateDate = Nothing, _rilPriceSchedules = Nothing, _rilStatusMessage = Nothing, _rilReservedInstancesId = Nothing, _rilInstanceCounts = Nothing, _rilReservedInstancesListingId = Nothing, _rilTags = Nothing};

-- | The status of the Reserved Instance listing.
rilStatus :: Lens' ReservedInstancesListing (Maybe ListingStatus)
rilStatus = lens _rilStatus (\ s a -> s{_rilStatus = a});

-- | A unique, case-sensitive key supplied by the client to ensure that the
-- request is idempotent. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
rilClientToken :: Lens' ReservedInstancesListing (Maybe Text)
rilClientToken = lens _rilClientToken (\ s a -> s{_rilClientToken = a});

-- | The last modified timestamp of the listing.
rilUpdateDate :: Lens' ReservedInstancesListing (Maybe UTCTime)
rilUpdateDate = lens _rilUpdateDate (\ s a -> s{_rilUpdateDate = a}) . mapping _Time;

-- | The time the listing was created.
rilCreateDate :: Lens' ReservedInstancesListing (Maybe UTCTime)
rilCreateDate = lens _rilCreateDate (\ s a -> s{_rilCreateDate = a}) . mapping _Time;

-- | The price of the Reserved Instance listing.
rilPriceSchedules :: Lens' ReservedInstancesListing [PriceSchedule]
rilPriceSchedules = lens _rilPriceSchedules (\ s a -> s{_rilPriceSchedules = a}) . _Default;

-- | The reason for the current status of the Reserved Instance listing. The
-- response can be blank.
rilStatusMessage :: Lens' ReservedInstancesListing (Maybe Text)
rilStatusMessage = lens _rilStatusMessage (\ s a -> s{_rilStatusMessage = a});

-- | The ID of the Reserved Instance.
rilReservedInstancesId :: Lens' ReservedInstancesListing (Maybe Text)
rilReservedInstancesId = lens _rilReservedInstancesId (\ s a -> s{_rilReservedInstancesId = a});

-- | The number of instances in this state.
rilInstanceCounts :: Lens' ReservedInstancesListing [InstanceCount]
rilInstanceCounts = lens _rilInstanceCounts (\ s a -> s{_rilInstanceCounts = a}) . _Default;

-- | The ID of the Reserved Instance listing.
rilReservedInstancesListingId :: Lens' ReservedInstancesListing (Maybe Text)
rilReservedInstancesListingId = lens _rilReservedInstancesListingId (\ s a -> s{_rilReservedInstancesListingId = a});

-- | Any tags assigned to the resource.
rilTags :: Lens' ReservedInstancesListing [Tag]
rilTags = lens _rilTags (\ s a -> s{_rilTags = a}) . _Default;

instance FromXML ReservedInstancesListing where
        parseXML x
          = ReservedInstancesListing' <$>
              (x .@? "status") <*> (x .@? "clientToken") <*>
                (x .@? "updateDate")
                <*> (x .@? "createDate")
                <*> (may (parseXMLList "item") x)
                <*> (x .@? "statusMessage")
                <*> (x .@? "reservedInstancesId")
                <*> (may (parseXMLList "item") x)
                <*> (x .@? "reservedInstancesListingId")
                <*> (may (parseXMLList "item") x)

-- | Describes a Reserved Instance modification.
--
-- /See:/ 'reservedInstancesModification' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rimModificationResults'
--
-- * 'rimStatus'
--
-- * 'rimClientToken'
--
-- * 'rimUpdateDate'
--
-- * 'rimCreateDate'
--
-- * 'rimEffectiveDate'
--
-- * 'rimStatusMessage'
--
-- * 'rimReservedInstancesModificationId'
--
-- * 'rimReservedInstancesIds'
data ReservedInstancesModification = ReservedInstancesModification'{_rimModificationResults :: Maybe [ReservedInstancesModificationResult], _rimStatus :: Maybe Text, _rimClientToken :: Maybe Text, _rimUpdateDate :: Maybe ISO8601, _rimCreateDate :: Maybe ISO8601, _rimEffectiveDate :: Maybe ISO8601, _rimStatusMessage :: Maybe Text, _rimReservedInstancesModificationId :: Maybe Text, _rimReservedInstancesIds :: Maybe [ReservedInstancesId]} deriving (Eq, Read, Show)

-- | 'ReservedInstancesModification' smart constructor.
reservedInstancesModification :: ReservedInstancesModification
reservedInstancesModification = ReservedInstancesModification'{_rimModificationResults = Nothing, _rimStatus = Nothing, _rimClientToken = Nothing, _rimUpdateDate = Nothing, _rimCreateDate = Nothing, _rimEffectiveDate = Nothing, _rimStatusMessage = Nothing, _rimReservedInstancesModificationId = Nothing, _rimReservedInstancesIds = Nothing};

-- | Contains target configurations along with their corresponding new
-- Reserved Instance IDs.
rimModificationResults :: Lens' ReservedInstancesModification [ReservedInstancesModificationResult]
rimModificationResults = lens _rimModificationResults (\ s a -> s{_rimModificationResults = a}) . _Default;

-- | The status of the Reserved Instances modification request.
rimStatus :: Lens' ReservedInstancesModification (Maybe Text)
rimStatus = lens _rimStatus (\ s a -> s{_rimStatus = a});

-- | A unique, case-sensitive key supplied by the client to ensure that the
-- request is idempotent. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
rimClientToken :: Lens' ReservedInstancesModification (Maybe Text)
rimClientToken = lens _rimClientToken (\ s a -> s{_rimClientToken = a});

-- | The time when the modification request was last updated.
rimUpdateDate :: Lens' ReservedInstancesModification (Maybe UTCTime)
rimUpdateDate = lens _rimUpdateDate (\ s a -> s{_rimUpdateDate = a}) . mapping _Time;

-- | The time when the modification request was created.
rimCreateDate :: Lens' ReservedInstancesModification (Maybe UTCTime)
rimCreateDate = lens _rimCreateDate (\ s a -> s{_rimCreateDate = a}) . mapping _Time;

-- | The time for the modification to become effective.
rimEffectiveDate :: Lens' ReservedInstancesModification (Maybe UTCTime)
rimEffectiveDate = lens _rimEffectiveDate (\ s a -> s{_rimEffectiveDate = a}) . mapping _Time;

-- | The reason for the status.
rimStatusMessage :: Lens' ReservedInstancesModification (Maybe Text)
rimStatusMessage = lens _rimStatusMessage (\ s a -> s{_rimStatusMessage = a});

-- | A unique ID for the Reserved Instance modification.
rimReservedInstancesModificationId :: Lens' ReservedInstancesModification (Maybe Text)
rimReservedInstancesModificationId = lens _rimReservedInstancesModificationId (\ s a -> s{_rimReservedInstancesModificationId = a});

-- | The IDs of one or more Reserved Instances.
rimReservedInstancesIds :: Lens' ReservedInstancesModification [ReservedInstancesId]
rimReservedInstancesIds = lens _rimReservedInstancesIds (\ s a -> s{_rimReservedInstancesIds = a}) . _Default;

instance FromXML ReservedInstancesModification where
        parseXML x
          = ReservedInstancesModification' <$>
              (may (parseXMLList "item") x) <*> (x .@? "status")
                <*> (x .@? "clientToken")
                <*> (x .@? "updateDate")
                <*> (x .@? "createDate")
                <*> (x .@? "effectiveDate")
                <*> (x .@? "statusMessage")
                <*> (x .@? "reservedInstancesModificationId")
                <*> (may (parseXMLList "item") x)

-- | /See:/ 'reservedInstancesModificationResult' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rimrReservedInstancesId'
--
-- * 'rimrTargetConfiguration'
data ReservedInstancesModificationResult = ReservedInstancesModificationResult'{_rimrReservedInstancesId :: Maybe Text, _rimrTargetConfiguration :: Maybe ReservedInstancesConfiguration} deriving (Eq, Read, Show)

-- | 'ReservedInstancesModificationResult' smart constructor.
reservedInstancesModificationResult :: ReservedInstancesModificationResult
reservedInstancesModificationResult = ReservedInstancesModificationResult'{_rimrReservedInstancesId = Nothing, _rimrTargetConfiguration = Nothing};

-- | The ID for the Reserved Instances that were created as part of the
-- modification request. This field is only available when the modification
-- is fulfilled.
rimrReservedInstancesId :: Lens' ReservedInstancesModificationResult (Maybe Text)
rimrReservedInstancesId = lens _rimrReservedInstancesId (\ s a -> s{_rimrReservedInstancesId = a});

-- | The target Reserved Instances configurations supplied as part of the
-- modification request.
rimrTargetConfiguration :: Lens' ReservedInstancesModificationResult (Maybe ReservedInstancesConfiguration)
rimrTargetConfiguration = lens _rimrTargetConfiguration (\ s a -> s{_rimrTargetConfiguration = a});

instance FromXML ReservedInstancesModificationResult
         where
        parseXML x
          = ReservedInstancesModificationResult' <$>
              (x .@? "reservedInstancesId") <*>
                (x .@? "targetConfiguration")

-- | Describes a Reserved Instance offering.
--
-- /See:/ 'reservedInstancesOffering' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rioMarketplace'
--
-- * 'rioCurrencyCode'
--
-- * 'rioProductDescription'
--
-- * 'rioInstanceType'
--
-- * 'rioAvailabilityZone'
--
-- * 'rioPricingDetails'
--
-- * 'rioOfferingType'
--
-- * 'rioUsagePrice'
--
-- * 'rioRecurringCharges'
--
-- * 'rioReservedInstancesOfferingId'
--
-- * 'rioInstanceTenancy'
--
-- * 'rioFixedPrice'
--
-- * 'rioDuration'
data ReservedInstancesOffering = ReservedInstancesOffering'{_rioMarketplace :: Maybe Bool, _rioCurrencyCode :: Maybe CurrencyCodeValues, _rioProductDescription :: Maybe RIProductDescription, _rioInstanceType :: Maybe InstanceType, _rioAvailabilityZone :: Maybe Text, _rioPricingDetails :: Maybe [PricingDetail], _rioOfferingType :: Maybe OfferingTypeValues, _rioUsagePrice :: Maybe Double, _rioRecurringCharges :: Maybe [RecurringCharge], _rioReservedInstancesOfferingId :: Maybe Text, _rioInstanceTenancy :: Maybe Tenancy, _rioFixedPrice :: Maybe Double, _rioDuration :: Maybe Integer} deriving (Eq, Read, Show)

-- | 'ReservedInstancesOffering' smart constructor.
reservedInstancesOffering :: ReservedInstancesOffering
reservedInstancesOffering = ReservedInstancesOffering'{_rioMarketplace = Nothing, _rioCurrencyCode = Nothing, _rioProductDescription = Nothing, _rioInstanceType = Nothing, _rioAvailabilityZone = Nothing, _rioPricingDetails = Nothing, _rioOfferingType = Nothing, _rioUsagePrice = Nothing, _rioRecurringCharges = Nothing, _rioReservedInstancesOfferingId = Nothing, _rioInstanceTenancy = Nothing, _rioFixedPrice = Nothing, _rioDuration = Nothing};

-- | Indicates whether the offering is available through the Reserved
-- Instance Marketplace (resale) or AWS. If it\'s a Reserved Instance
-- Marketplace offering, this is @true@.
rioMarketplace :: Lens' ReservedInstancesOffering (Maybe Bool)
rioMarketplace = lens _rioMarketplace (\ s a -> s{_rioMarketplace = a});

-- | The currency of the Reserved Instance offering you are purchasing. It\'s
-- specified using ISO 4217 standard currency codes. At this time, the only
-- supported currency is @USD@.
rioCurrencyCode :: Lens' ReservedInstancesOffering (Maybe CurrencyCodeValues)
rioCurrencyCode = lens _rioCurrencyCode (\ s a -> s{_rioCurrencyCode = a});

-- | The Reserved Instance product platform description.
rioProductDescription :: Lens' ReservedInstancesOffering (Maybe RIProductDescription)
rioProductDescription = lens _rioProductDescription (\ s a -> s{_rioProductDescription = a});

-- | The instance type on which the Reserved Instance can be used.
rioInstanceType :: Lens' ReservedInstancesOffering (Maybe InstanceType)
rioInstanceType = lens _rioInstanceType (\ s a -> s{_rioInstanceType = a});

-- | The Availability Zone in which the Reserved Instance can be used.
rioAvailabilityZone :: Lens' ReservedInstancesOffering (Maybe Text)
rioAvailabilityZone = lens _rioAvailabilityZone (\ s a -> s{_rioAvailabilityZone = a});

-- | The pricing details of the Reserved Instance offering.
rioPricingDetails :: Lens' ReservedInstancesOffering [PricingDetail]
rioPricingDetails = lens _rioPricingDetails (\ s a -> s{_rioPricingDetails = a}) . _Default;

-- | The Reserved Instance offering type.
rioOfferingType :: Lens' ReservedInstancesOffering (Maybe OfferingTypeValues)
rioOfferingType = lens _rioOfferingType (\ s a -> s{_rioOfferingType = a});

-- | The usage price of the Reserved Instance, per hour.
rioUsagePrice :: Lens' ReservedInstancesOffering (Maybe Double)
rioUsagePrice = lens _rioUsagePrice (\ s a -> s{_rioUsagePrice = a});

-- | The recurring charge tag assigned to the resource.
rioRecurringCharges :: Lens' ReservedInstancesOffering [RecurringCharge]
rioRecurringCharges = lens _rioRecurringCharges (\ s a -> s{_rioRecurringCharges = a}) . _Default;

-- | The ID of the Reserved Instance offering.
rioReservedInstancesOfferingId :: Lens' ReservedInstancesOffering (Maybe Text)
rioReservedInstancesOfferingId = lens _rioReservedInstancesOfferingId (\ s a -> s{_rioReservedInstancesOfferingId = a});

-- | The tenancy of the reserved instance.
rioInstanceTenancy :: Lens' ReservedInstancesOffering (Maybe Tenancy)
rioInstanceTenancy = lens _rioInstanceTenancy (\ s a -> s{_rioInstanceTenancy = a});

-- | The purchase price of the Reserved Instance.
rioFixedPrice :: Lens' ReservedInstancesOffering (Maybe Double)
rioFixedPrice = lens _rioFixedPrice (\ s a -> s{_rioFixedPrice = a});

-- | The duration of the Reserved Instance, in seconds.
rioDuration :: Lens' ReservedInstancesOffering (Maybe Integer)
rioDuration = lens _rioDuration (\ s a -> s{_rioDuration = a});

instance FromXML ReservedInstancesOffering where
        parseXML x
          = ReservedInstancesOffering' <$>
              (x .@? "marketplace") <*> (x .@? "currencyCode") <*>
                (x .@? "productDescription")
                <*> (x .@? "instanceType")
                <*> (x .@? "availabilityZone")
                <*> (may (parseXMLList "item") x)
                <*> (x .@? "offeringType")
                <*> (x .@? "usagePrice")
                <*> (may (parseXMLList "item") x)
                <*> (x .@? "reservedInstancesOfferingId")
                <*> (x .@? "instanceTenancy")
                <*> (x .@? "fixedPrice")
                <*> (x .@? "duration")

-- | Describes a route in a route table.
--
-- /See:/ 'route' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rouInstanceId'
--
-- * 'rouOrigin'
--
-- * 'rouVPCPeeringConnectionId'
--
-- * 'rouState'
--
-- * 'rouNetworkInterfaceId'
--
-- * 'rouGatewayId'
--
-- * 'rouInstanceOwnerId'
--
-- * 'rouDestinationPrefixListId'
--
-- * 'rouDestinationCIDRBlock'
data Route = Route'{_rouInstanceId :: Maybe Text, _rouOrigin :: Maybe RouteOrigin, _rouVPCPeeringConnectionId :: Maybe Text, _rouState :: Maybe RouteState, _rouNetworkInterfaceId :: Maybe Text, _rouGatewayId :: Maybe Text, _rouInstanceOwnerId :: Maybe Text, _rouDestinationPrefixListId :: Maybe Text, _rouDestinationCIDRBlock :: Maybe Text} deriving (Eq, Read, Show)

-- | 'Route' smart constructor.
route :: Route
route = Route'{_rouInstanceId = Nothing, _rouOrigin = Nothing, _rouVPCPeeringConnectionId = Nothing, _rouState = Nothing, _rouNetworkInterfaceId = Nothing, _rouGatewayId = Nothing, _rouInstanceOwnerId = Nothing, _rouDestinationPrefixListId = Nothing, _rouDestinationCIDRBlock = Nothing};

-- | The ID of a NAT instance in your VPC.
rouInstanceId :: Lens' Route (Maybe Text)
rouInstanceId = lens _rouInstanceId (\ s a -> s{_rouInstanceId = a});

-- | Describes how the route was created.
--
-- -   @CreateRouteTable@ indicates that route was automatically created
--     when the route table was created.
-- -   @CreateRoute@ indicates that the route was manually added to the
--     route table.
-- -   @EnableVgwRoutePropagation@ indicates that the route was propagated
--     by route propagation.
rouOrigin :: Lens' Route (Maybe RouteOrigin)
rouOrigin = lens _rouOrigin (\ s a -> s{_rouOrigin = a});

-- | The ID of the VPC peering connection.
rouVPCPeeringConnectionId :: Lens' Route (Maybe Text)
rouVPCPeeringConnectionId = lens _rouVPCPeeringConnectionId (\ s a -> s{_rouVPCPeeringConnectionId = a});

-- | The state of the route. The @blackhole@ state indicates that the
-- route\'s target isn\'t available (for example, the specified gateway
-- isn\'t attached to the VPC, or the specified NAT instance has been
-- terminated).
rouState :: Lens' Route (Maybe RouteState)
rouState = lens _rouState (\ s a -> s{_rouState = a});

-- | The ID of the network interface.
rouNetworkInterfaceId :: Lens' Route (Maybe Text)
rouNetworkInterfaceId = lens _rouNetworkInterfaceId (\ s a -> s{_rouNetworkInterfaceId = a});

-- | The ID of a gateway attached to your VPC.
rouGatewayId :: Lens' Route (Maybe Text)
rouGatewayId = lens _rouGatewayId (\ s a -> s{_rouGatewayId = a});

-- | The AWS account ID of the owner of the instance.
rouInstanceOwnerId :: Lens' Route (Maybe Text)
rouInstanceOwnerId = lens _rouInstanceOwnerId (\ s a -> s{_rouInstanceOwnerId = a});

-- | The prefix of the AWS service.
rouDestinationPrefixListId :: Lens' Route (Maybe Text)
rouDestinationPrefixListId = lens _rouDestinationPrefixListId (\ s a -> s{_rouDestinationPrefixListId = a});

-- | The CIDR block used for the destination match.
rouDestinationCIDRBlock :: Lens' Route (Maybe Text)
rouDestinationCIDRBlock = lens _rouDestinationCIDRBlock (\ s a -> s{_rouDestinationCIDRBlock = a});

instance FromXML Route where
        parseXML x
          = Route' <$>
              (x .@? "instanceId") <*> (x .@? "origin") <*>
                (x .@? "vpcPeeringConnectionId")
                <*> (x .@? "state")
                <*> (x .@? "networkInterfaceId")
                <*> (x .@? "gatewayId")
                <*> (x .@? "instanceOwnerId")
                <*> (x .@? "destinationPrefixListId")
                <*> (x .@? "destinationCidrBlock")

-- | Describes a route table.
--
-- /See:/ 'routeTable' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtRoutes'
--
-- * 'rtRouteTableId'
--
-- * 'rtVPCId'
--
-- * 'rtPropagatingVGWs'
--
-- * 'rtAssociations'
--
-- * 'rtTags'
data RouteTable = RouteTable'{_rtRoutes :: Maybe [Route], _rtRouteTableId :: Maybe Text, _rtVPCId :: Maybe Text, _rtPropagatingVGWs :: Maybe [PropagatingVGW], _rtAssociations :: Maybe [RouteTableAssociation], _rtTags :: Maybe [Tag]} deriving (Eq, Read, Show)

-- | 'RouteTable' smart constructor.
routeTable :: RouteTable
routeTable = RouteTable'{_rtRoutes = Nothing, _rtRouteTableId = Nothing, _rtVPCId = Nothing, _rtPropagatingVGWs = Nothing, _rtAssociations = Nothing, _rtTags = Nothing};

-- | The routes in the route table.
rtRoutes :: Lens' RouteTable [Route]
rtRoutes = lens _rtRoutes (\ s a -> s{_rtRoutes = a}) . _Default;

-- | The ID of the route table.
rtRouteTableId :: Lens' RouteTable (Maybe Text)
rtRouteTableId = lens _rtRouteTableId (\ s a -> s{_rtRouteTableId = a});

-- | The ID of the VPC.
rtVPCId :: Lens' RouteTable (Maybe Text)
rtVPCId = lens _rtVPCId (\ s a -> s{_rtVPCId = a});

-- | Any virtual private gateway (VGW) propagating routes.
rtPropagatingVGWs :: Lens' RouteTable [PropagatingVGW]
rtPropagatingVGWs = lens _rtPropagatingVGWs (\ s a -> s{_rtPropagatingVGWs = a}) . _Default;

-- | The associations between the route table and one or more subnets.
rtAssociations :: Lens' RouteTable [RouteTableAssociation]
rtAssociations = lens _rtAssociations (\ s a -> s{_rtAssociations = a}) . _Default;

-- | Any tags assigned to the route table.
rtTags :: Lens' RouteTable [Tag]
rtTags = lens _rtTags (\ s a -> s{_rtTags = a}) . _Default;

instance FromXML RouteTable where
        parseXML x
          = RouteTable' <$>
              (may (parseXMLList "item") x) <*>
                (x .@? "routeTableId")
                <*> (x .@? "vpcId")
                <*> (may (parseXMLList "item") x)
                <*> (may (parseXMLList "item") x)
                <*> (may (parseXMLList "item") x)

-- | Describes an association between a route table and a subnet.
--
-- /See:/ 'routeTableAssociation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtaRouteTableId'
--
-- * 'rtaRouteTableAssociationId'
--
-- * 'rtaMain'
--
-- * 'rtaSubnetId'
data RouteTableAssociation = RouteTableAssociation'{_rtaRouteTableId :: Maybe Text, _rtaRouteTableAssociationId :: Maybe Text, _rtaMain :: Maybe Bool, _rtaSubnetId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'RouteTableAssociation' smart constructor.
routeTableAssociation :: RouteTableAssociation
routeTableAssociation = RouteTableAssociation'{_rtaRouteTableId = Nothing, _rtaRouteTableAssociationId = Nothing, _rtaMain = Nothing, _rtaSubnetId = Nothing};

-- | The ID of the route table.
rtaRouteTableId :: Lens' RouteTableAssociation (Maybe Text)
rtaRouteTableId = lens _rtaRouteTableId (\ s a -> s{_rtaRouteTableId = a});

-- | The ID of the association between a route table and a subnet.
rtaRouteTableAssociationId :: Lens' RouteTableAssociation (Maybe Text)
rtaRouteTableAssociationId = lens _rtaRouteTableAssociationId (\ s a -> s{_rtaRouteTableAssociationId = a});

-- | Indicates whether this is the main route table.
rtaMain :: Lens' RouteTableAssociation (Maybe Bool)
rtaMain = lens _rtaMain (\ s a -> s{_rtaMain = a});

-- | The ID of the subnet.
rtaSubnetId :: Lens' RouteTableAssociation (Maybe Text)
rtaSubnetId = lens _rtaSubnetId (\ s a -> s{_rtaSubnetId = a});

instance FromXML RouteTableAssociation where
        parseXML x
          = RouteTableAssociation' <$>
              (x .@? "routeTableId") <*>
                (x .@? "routeTableAssociationId")
                <*> (x .@? "main")
                <*> (x .@? "subnetId")

-- | Describes the monitoring for the instance.
--
-- /See:/ 'runInstancesMonitoringEnabled' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rimeEnabled'
newtype RunInstancesMonitoringEnabled = RunInstancesMonitoringEnabled'{_rimeEnabled :: Bool} deriving (Eq, Read, Show)

-- | 'RunInstancesMonitoringEnabled' smart constructor.
runInstancesMonitoringEnabled :: Bool -> RunInstancesMonitoringEnabled
runInstancesMonitoringEnabled pEnabled = RunInstancesMonitoringEnabled'{_rimeEnabled = pEnabled};

-- | Indicates whether monitoring is enabled for the instance.
rimeEnabled :: Lens' RunInstancesMonitoringEnabled Bool
rimeEnabled = lens _rimeEnabled (\ s a -> s{_rimeEnabled = a});

instance FromXML RunInstancesMonitoringEnabled where
        parseXML x
          = RunInstancesMonitoringEnabled' <$> (x .@ "enabled")

instance ToQuery RunInstancesMonitoringEnabled where
        toQuery RunInstancesMonitoringEnabled'{..}
          = mconcat ["Enabled" =: _rimeEnabled]

-- | Describes the storage parameters for S3 and S3 buckets for an instance
-- store-backed AMI.
--
-- /See:/ 's3Storage' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ssPrefix'
--
-- * 'ssUploadPolicy'
--
-- * 'ssBucket'
--
-- * 'ssUploadPolicySignature'
--
-- * 'ssAWSAccessKeyId'
data S3Storage = S3Storage'{_ssPrefix :: Maybe Text, _ssUploadPolicy :: Maybe Base64, _ssBucket :: Maybe Text, _ssUploadPolicySignature :: Maybe Text, _ssAWSAccessKeyId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'S3Storage' smart constructor.
s3Storage :: S3Storage
s3Storage = S3Storage'{_ssPrefix = Nothing, _ssUploadPolicy = Nothing, _ssBucket = Nothing, _ssUploadPolicySignature = Nothing, _ssAWSAccessKeyId = Nothing};

-- | The beginning of the file name of the AMI.
ssPrefix :: Lens' S3Storage (Maybe Text)
ssPrefix = lens _ssPrefix (\ s a -> s{_ssPrefix = a});

-- | A Base64-encoded Amazon S3 upload policy that gives Amazon EC2
-- permission to upload items into Amazon S3 on your behalf.
ssUploadPolicy :: Lens' S3Storage (Maybe Base64)
ssUploadPolicy = lens _ssUploadPolicy (\ s a -> s{_ssUploadPolicy = a});

-- | The bucket in which to store the AMI. You can specify a bucket that you
-- already own or a new bucket that Amazon EC2 creates on your behalf. If
-- you specify a bucket that belongs to someone else, Amazon EC2 returns an
-- error.
ssBucket :: Lens' S3Storage (Maybe Text)
ssBucket = lens _ssBucket (\ s a -> s{_ssBucket = a});

-- | The signature of the Base64 encoded JSON document.
ssUploadPolicySignature :: Lens' S3Storage (Maybe Text)
ssUploadPolicySignature = lens _ssUploadPolicySignature (\ s a -> s{_ssUploadPolicySignature = a});

-- | The access key ID of the owner of the bucket. Before you specify a value
-- for your access key ID, review and follow the guidance in
-- <http://docs.aws.amazon.com/general/latest/gr/aws-access-keys-best-practices.html Best Practices for Managing AWS Access Keys>.
ssAWSAccessKeyId :: Lens' S3Storage (Maybe Text)
ssAWSAccessKeyId = lens _ssAWSAccessKeyId (\ s a -> s{_ssAWSAccessKeyId = a});

instance FromXML S3Storage where
        parseXML x
          = S3Storage' <$>
              (x .@? "prefix") <*> (x .@? "uploadPolicy") <*>
                (x .@? "bucket")
                <*> (x .@? "uploadPolicySignature")
                <*> (x .@? "AWSAccessKeyId")

instance ToQuery S3Storage where
        toQuery S3Storage'{..}
          = mconcat
              ["Prefix" =: _ssPrefix,
               "UploadPolicy" =: _ssUploadPolicy,
               "Bucket" =: _ssBucket,
               "UploadPolicySignature" =: _ssUploadPolicySignature,
               "AWSAccessKeyId" =: _ssAWSAccessKeyId]

-- | Describes a security group
--
-- /See:/ 'securityGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sgVPCId'
--
-- * 'sgIPPermissions'
--
-- * 'sgIPPermissionsEgress'
--
-- * 'sgTags'
--
-- * 'sgOwnerId'
--
-- * 'sgGroupId'
--
-- * 'sgGroupName'
--
-- * 'sgDescription'
data SecurityGroup = SecurityGroup'{_sgVPCId :: Maybe Text, _sgIPPermissions :: Maybe [IPPermission], _sgIPPermissionsEgress :: Maybe [IPPermission], _sgTags :: Maybe [Tag], _sgOwnerId :: Text, _sgGroupId :: Text, _sgGroupName :: Text, _sgDescription :: Text} deriving (Eq, Read, Show)

-- | 'SecurityGroup' smart constructor.
securityGroup :: Text -> Text -> Text -> Text -> SecurityGroup
securityGroup pOwnerId pGroupId pGroupName pDescription = SecurityGroup'{_sgVPCId = Nothing, _sgIPPermissions = Nothing, _sgIPPermissionsEgress = Nothing, _sgTags = Nothing, _sgOwnerId = pOwnerId, _sgGroupId = pGroupId, _sgGroupName = pGroupName, _sgDescription = pDescription};

-- | [EC2-VPC] The ID of the VPC for the security group.
sgVPCId :: Lens' SecurityGroup (Maybe Text)
sgVPCId = lens _sgVPCId (\ s a -> s{_sgVPCId = a});

-- | One or more inbound rules associated with the security group.
sgIPPermissions :: Lens' SecurityGroup [IPPermission]
sgIPPermissions = lens _sgIPPermissions (\ s a -> s{_sgIPPermissions = a}) . _Default;

-- | [EC2-VPC] One or more outbound rules associated with the security group.
sgIPPermissionsEgress :: Lens' SecurityGroup [IPPermission]
sgIPPermissionsEgress = lens _sgIPPermissionsEgress (\ s a -> s{_sgIPPermissionsEgress = a}) . _Default;

-- | Any tags assigned to the security group.
sgTags :: Lens' SecurityGroup [Tag]
sgTags = lens _sgTags (\ s a -> s{_sgTags = a}) . _Default;

-- | The AWS account ID of the owner of the security group.
sgOwnerId :: Lens' SecurityGroup Text
sgOwnerId = lens _sgOwnerId (\ s a -> s{_sgOwnerId = a});

-- | The ID of the security group.
sgGroupId :: Lens' SecurityGroup Text
sgGroupId = lens _sgGroupId (\ s a -> s{_sgGroupId = a});

-- | The name of the security group.
sgGroupName :: Lens' SecurityGroup Text
sgGroupName = lens _sgGroupName (\ s a -> s{_sgGroupName = a});

-- | A description of the security group.
sgDescription :: Lens' SecurityGroup Text
sgDescription = lens _sgDescription (\ s a -> s{_sgDescription = a});

instance FromXML SecurityGroup where
        parseXML x
          = SecurityGroup' <$>
              (x .@? "vpcId") <*> (may (parseXMLList "item") x) <*>
                (may (parseXMLList "item") x)
                <*> (may (parseXMLList "item") x)
                <*> (x .@ "ownerId")
                <*> (x .@ "groupId")
                <*> (x .@ "groupName")
                <*> (x .@ "groupDescription")

-- | Describes a snapshot.
--
-- /See:/ 'snapshot' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'snaOwnerAlias'
--
-- * 'snaKMSKeyId'
--
-- * 'snaTags'
--
-- * 'snaSnapshotId'
--
-- * 'snaOwnerId'
--
-- * 'snaVolumeId'
--
-- * 'snaVolumeSize'
--
-- * 'snaDescription'
--
-- * 'snaStartTime'
--
-- * 'snaProgress'
--
-- * 'snaState'
--
-- * 'snaEncrypted'
data Snapshot = Snapshot'{_snaOwnerAlias :: Maybe Text, _snaKMSKeyId :: Maybe Text, _snaTags :: Maybe [Tag], _snaSnapshotId :: Text, _snaOwnerId :: Text, _snaVolumeId :: Text, _snaVolumeSize :: Int, _snaDescription :: Text, _snaStartTime :: ISO8601, _snaProgress :: Text, _snaState :: SnapshotState, _snaEncrypted :: Bool} deriving (Eq, Read, Show)

-- | 'Snapshot' smart constructor.
snapshot :: Text -> Text -> Text -> Int -> Text -> UTCTime -> Text -> SnapshotState -> Bool -> Snapshot
snapshot pSnapshotId pOwnerId pVolumeId pVolumeSize pDescription pStartTime pProgress pState pEncrypted = Snapshot'{_snaOwnerAlias = Nothing, _snaKMSKeyId = Nothing, _snaTags = Nothing, _snaSnapshotId = pSnapshotId, _snaOwnerId = pOwnerId, _snaVolumeId = pVolumeId, _snaVolumeSize = pVolumeSize, _snaDescription = pDescription, _snaStartTime = _Time # pStartTime, _snaProgress = pProgress, _snaState = pState, _snaEncrypted = pEncrypted};

-- | The AWS account alias (for example, @amazon@, @self@) or AWS account ID
-- that owns the snapshot.
snaOwnerAlias :: Lens' Snapshot (Maybe Text)
snaOwnerAlias = lens _snaOwnerAlias (\ s a -> s{_snaOwnerAlias = a});

-- | The full ARN of the AWS Key Management Service (KMS) master key that was
-- used to protect the volume encryption key for the parent volume.
snaKMSKeyId :: Lens' Snapshot (Maybe Text)
snaKMSKeyId = lens _snaKMSKeyId (\ s a -> s{_snaKMSKeyId = a});

-- | Any tags assigned to the snapshot.
snaTags :: Lens' Snapshot [Tag]
snaTags = lens _snaTags (\ s a -> s{_snaTags = a}) . _Default;

-- | The ID of the snapshot.
snaSnapshotId :: Lens' Snapshot Text
snaSnapshotId = lens _snaSnapshotId (\ s a -> s{_snaSnapshotId = a});

-- | The AWS account ID of the EBS snapshot owner.
snaOwnerId :: Lens' Snapshot Text
snaOwnerId = lens _snaOwnerId (\ s a -> s{_snaOwnerId = a});

-- | The ID of the volume.
snaVolumeId :: Lens' Snapshot Text
snaVolumeId = lens _snaVolumeId (\ s a -> s{_snaVolumeId = a});

-- | The size of the volume, in GiB.
snaVolumeSize :: Lens' Snapshot Int
snaVolumeSize = lens _snaVolumeSize (\ s a -> s{_snaVolumeSize = a});

-- | The description for the snapshot.
snaDescription :: Lens' Snapshot Text
snaDescription = lens _snaDescription (\ s a -> s{_snaDescription = a});

-- | The time stamp when the snapshot was initiated.
snaStartTime :: Lens' Snapshot UTCTime
snaStartTime = lens _snaStartTime (\ s a -> s{_snaStartTime = a}) . _Time;

-- | The progress of the snapshot, as a percentage.
snaProgress :: Lens' Snapshot Text
snaProgress = lens _snaProgress (\ s a -> s{_snaProgress = a});

-- | The snapshot state.
snaState :: Lens' Snapshot SnapshotState
snaState = lens _snaState (\ s a -> s{_snaState = a});

-- | Indicates whether the snapshot is encrypted.
snaEncrypted :: Lens' Snapshot Bool
snaEncrypted = lens _snaEncrypted (\ s a -> s{_snaEncrypted = a});

instance FromXML Snapshot where
        parseXML x
          = Snapshot' <$>
              (x .@? "ownerAlias") <*> (x .@? "kmsKeyId") <*>
                (may (parseXMLList "item") x)
                <*> (x .@ "snapshotId")
                <*> (x .@ "ownerId")
                <*> (x .@ "volumeId")
                <*> (x .@ "volumeSize")
                <*> (x .@ "description")
                <*> (x .@ "startTime")
                <*> (x .@ "progress")
                <*> (x .@ "status")
                <*> (x .@ "encrypted")

-- | Describes the snapshot created from the imported disk.
--
-- /See:/ 'snapshotDetail' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sdStatus'
--
-- * 'sdProgress'
--
-- * 'sdURL'
--
-- * 'sdFormat'
--
-- * 'sdDeviceName'
--
-- * 'sdUserBucket'
--
-- * 'sdDiskImageSize'
--
-- * 'sdStatusMessage'
--
-- * 'sdDescription'
--
-- * 'sdSnapshotId'
data SnapshotDetail = SnapshotDetail'{_sdStatus :: Maybe Text, _sdProgress :: Maybe Text, _sdURL :: Maybe Text, _sdFormat :: Maybe Text, _sdDeviceName :: Maybe Text, _sdUserBucket :: Maybe UserBucketDetails, _sdDiskImageSize :: Maybe Double, _sdStatusMessage :: Maybe Text, _sdDescription :: Maybe Text, _sdSnapshotId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'SnapshotDetail' smart constructor.
snapshotDetail :: SnapshotDetail
snapshotDetail = SnapshotDetail'{_sdStatus = Nothing, _sdProgress = Nothing, _sdURL = Nothing, _sdFormat = Nothing, _sdDeviceName = Nothing, _sdUserBucket = Nothing, _sdDiskImageSize = Nothing, _sdStatusMessage = Nothing, _sdDescription = Nothing, _sdSnapshotId = Nothing};

-- | A brief status of the snapshot creation.
sdStatus :: Lens' SnapshotDetail (Maybe Text)
sdStatus = lens _sdStatus (\ s a -> s{_sdStatus = a});

-- | The percentage of progress for the task.
sdProgress :: Lens' SnapshotDetail (Maybe Text)
sdProgress = lens _sdProgress (\ s a -> s{_sdProgress = a});

-- | The URL used to access the disk image.
sdURL :: Lens' SnapshotDetail (Maybe Text)
sdURL = lens _sdURL (\ s a -> s{_sdURL = a});

-- | The format of the disk image from which the snapshot is created.
sdFormat :: Lens' SnapshotDetail (Maybe Text)
sdFormat = lens _sdFormat (\ s a -> s{_sdFormat = a});

-- | The block device mapping for the snapshot.
sdDeviceName :: Lens' SnapshotDetail (Maybe Text)
sdDeviceName = lens _sdDeviceName (\ s a -> s{_sdDeviceName = a});

-- | FIXME: Undocumented member.
sdUserBucket :: Lens' SnapshotDetail (Maybe UserBucketDetails)
sdUserBucket = lens _sdUserBucket (\ s a -> s{_sdUserBucket = a});

-- | The size of the disk in the snapshot, in GiB.
sdDiskImageSize :: Lens' SnapshotDetail (Maybe Double)
sdDiskImageSize = lens _sdDiskImageSize (\ s a -> s{_sdDiskImageSize = a});

-- | A detailed status message for the snapshot creation.
sdStatusMessage :: Lens' SnapshotDetail (Maybe Text)
sdStatusMessage = lens _sdStatusMessage (\ s a -> s{_sdStatusMessage = a});

-- | A description for the snapshot.
sdDescription :: Lens' SnapshotDetail (Maybe Text)
sdDescription = lens _sdDescription (\ s a -> s{_sdDescription = a});

-- | The snapshot ID of the disk being imported.
sdSnapshotId :: Lens' SnapshotDetail (Maybe Text)
sdSnapshotId = lens _sdSnapshotId (\ s a -> s{_sdSnapshotId = a});

instance FromXML SnapshotDetail where
        parseXML x
          = SnapshotDetail' <$>
              (x .@? "status") <*> (x .@? "progress") <*>
                (x .@? "url")
                <*> (x .@? "format")
                <*> (x .@? "deviceName")
                <*> (x .@? "userBucket")
                <*> (x .@? "diskImageSize")
                <*> (x .@? "statusMessage")
                <*> (x .@? "description")
                <*> (x .@? "snapshotId")

-- | The disk container object for the import snapshot request.
--
-- /See:/ 'snapshotDiskContainer' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sdcURL'
--
-- * 'sdcFormat'
--
-- * 'sdcUserBucket'
--
-- * 'sdcDescription'
data SnapshotDiskContainer = SnapshotDiskContainer'{_sdcURL :: Maybe Text, _sdcFormat :: Maybe Text, _sdcUserBucket :: Maybe UserBucket, _sdcDescription :: Maybe Text} deriving (Eq, Read, Show)

-- | 'SnapshotDiskContainer' smart constructor.
snapshotDiskContainer :: SnapshotDiskContainer
snapshotDiskContainer = SnapshotDiskContainer'{_sdcURL = Nothing, _sdcFormat = Nothing, _sdcUserBucket = Nothing, _sdcDescription = Nothing};

-- | The URL to the Amazon S3-based disk image being imported. It can either
-- be a https URL (https:\/\/..) or an Amazon S3 URL (s3:\/\/..).
sdcURL :: Lens' SnapshotDiskContainer (Maybe Text)
sdcURL = lens _sdcURL (\ s a -> s{_sdcURL = a});

-- | The format of the disk image being imported.
--
-- Valid values: @RAW@ | @VHD@ | @VMDK@ | @OVA@
sdcFormat :: Lens' SnapshotDiskContainer (Maybe Text)
sdcFormat = lens _sdcFormat (\ s a -> s{_sdcFormat = a});

-- | FIXME: Undocumented member.
sdcUserBucket :: Lens' SnapshotDiskContainer (Maybe UserBucket)
sdcUserBucket = lens _sdcUserBucket (\ s a -> s{_sdcUserBucket = a});

-- | The description of the disk image being imported.
sdcDescription :: Lens' SnapshotDiskContainer (Maybe Text)
sdcDescription = lens _sdcDescription (\ s a -> s{_sdcDescription = a});

instance ToQuery SnapshotDiskContainer where
        toQuery SnapshotDiskContainer'{..}
          = mconcat
              ["Url" =: _sdcURL, "Format" =: _sdcFormat,
               "UserBucket" =: _sdcUserBucket,
               "Description" =: _sdcDescription]

-- | Details about the import snapshot task.
--
-- /See:/ 'snapshotTaskDetail' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'stdStatus'
--
-- * 'stdProgress'
--
-- * 'stdURL'
--
-- * 'stdFormat'
--
-- * 'stdUserBucket'
--
-- * 'stdDiskImageSize'
--
-- * 'stdStatusMessage'
--
-- * 'stdDescription'
--
-- * 'stdSnapshotId'
data SnapshotTaskDetail = SnapshotTaskDetail'{_stdStatus :: Maybe Text, _stdProgress :: Maybe Text, _stdURL :: Maybe Text, _stdFormat :: Maybe Text, _stdUserBucket :: Maybe UserBucketDetails, _stdDiskImageSize :: Maybe Double, _stdStatusMessage :: Maybe Text, _stdDescription :: Maybe Text, _stdSnapshotId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'SnapshotTaskDetail' smart constructor.
snapshotTaskDetail :: SnapshotTaskDetail
snapshotTaskDetail = SnapshotTaskDetail'{_stdStatus = Nothing, _stdProgress = Nothing, _stdURL = Nothing, _stdFormat = Nothing, _stdUserBucket = Nothing, _stdDiskImageSize = Nothing, _stdStatusMessage = Nothing, _stdDescription = Nothing, _stdSnapshotId = Nothing};

-- | A brief status for the import snapshot task.
stdStatus :: Lens' SnapshotTaskDetail (Maybe Text)
stdStatus = lens _stdStatus (\ s a -> s{_stdStatus = a});

-- | The percentage of completion for the import snapshot task.
stdProgress :: Lens' SnapshotTaskDetail (Maybe Text)
stdProgress = lens _stdProgress (\ s a -> s{_stdProgress = a});

-- | The URL of the disk image from which the snapshot is created.
stdURL :: Lens' SnapshotTaskDetail (Maybe Text)
stdURL = lens _stdURL (\ s a -> s{_stdURL = a});

-- | The format of the disk image from which the snapshot is created.
stdFormat :: Lens' SnapshotTaskDetail (Maybe Text)
stdFormat = lens _stdFormat (\ s a -> s{_stdFormat = a});

-- | The S3 bucket for the disk image.
stdUserBucket :: Lens' SnapshotTaskDetail (Maybe UserBucketDetails)
stdUserBucket = lens _stdUserBucket (\ s a -> s{_stdUserBucket = a});

-- | The size of the disk in the snapshot, in GiB.
stdDiskImageSize :: Lens' SnapshotTaskDetail (Maybe Double)
stdDiskImageSize = lens _stdDiskImageSize (\ s a -> s{_stdDiskImageSize = a});

-- | A detailed status message for the import snapshot task.
stdStatusMessage :: Lens' SnapshotTaskDetail (Maybe Text)
stdStatusMessage = lens _stdStatusMessage (\ s a -> s{_stdStatusMessage = a});

-- | The description of the snapshot.
stdDescription :: Lens' SnapshotTaskDetail (Maybe Text)
stdDescription = lens _stdDescription (\ s a -> s{_stdDescription = a});

-- | The snapshot ID of the disk being imported.
stdSnapshotId :: Lens' SnapshotTaskDetail (Maybe Text)
stdSnapshotId = lens _stdSnapshotId (\ s a -> s{_stdSnapshotId = a});

instance FromXML SnapshotTaskDetail where
        parseXML x
          = SnapshotTaskDetail' <$>
              (x .@? "status") <*> (x .@? "progress") <*>
                (x .@? "url")
                <*> (x .@? "format")
                <*> (x .@? "userBucket")
                <*> (x .@? "diskImageSize")
                <*> (x .@? "statusMessage")
                <*> (x .@? "description")
                <*> (x .@? "snapshotId")

-- | Describes the data feed for a Spot Instance.
--
-- /See:/ 'spotDatafeedSubscription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sdsState'
--
-- * 'sdsPrefix'
--
-- * 'sdsBucket'
--
-- * 'sdsOwnerId'
--
-- * 'sdsFault'
data SpotDatafeedSubscription = SpotDatafeedSubscription'{_sdsState :: Maybe DatafeedSubscriptionState, _sdsPrefix :: Maybe Text, _sdsBucket :: Maybe Text, _sdsOwnerId :: Maybe Text, _sdsFault :: Maybe SpotInstanceStateFault} deriving (Eq, Read, Show)

-- | 'SpotDatafeedSubscription' smart constructor.
spotDatafeedSubscription :: SpotDatafeedSubscription
spotDatafeedSubscription = SpotDatafeedSubscription'{_sdsState = Nothing, _sdsPrefix = Nothing, _sdsBucket = Nothing, _sdsOwnerId = Nothing, _sdsFault = Nothing};

-- | The state of the Spot Instance data feed subscription.
sdsState :: Lens' SpotDatafeedSubscription (Maybe DatafeedSubscriptionState)
sdsState = lens _sdsState (\ s a -> s{_sdsState = a});

-- | The prefix that is prepended to data feed files.
sdsPrefix :: Lens' SpotDatafeedSubscription (Maybe Text)
sdsPrefix = lens _sdsPrefix (\ s a -> s{_sdsPrefix = a});

-- | The Amazon S3 bucket where the Spot Instance data feed is located.
sdsBucket :: Lens' SpotDatafeedSubscription (Maybe Text)
sdsBucket = lens _sdsBucket (\ s a -> s{_sdsBucket = a});

-- | The AWS account ID of the account.
sdsOwnerId :: Lens' SpotDatafeedSubscription (Maybe Text)
sdsOwnerId = lens _sdsOwnerId (\ s a -> s{_sdsOwnerId = a});

-- | The fault codes for the Spot Instance request, if any.
sdsFault :: Lens' SpotDatafeedSubscription (Maybe SpotInstanceStateFault)
sdsFault = lens _sdsFault (\ s a -> s{_sdsFault = a});

instance FromXML SpotDatafeedSubscription where
        parseXML x
          = SpotDatafeedSubscription' <$>
              (x .@? "state") <*> (x .@? "prefix") <*>
                (x .@? "bucket")
                <*> (x .@? "ownerId")
                <*> (x .@? "fault")

-- | Describes a Spot fleet request.
--
-- /See:/ 'spotFleetRequestConfig' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sfrcSpotFleetRequestId'
--
-- * 'sfrcSpotFleetRequestState'
--
-- * 'sfrcSpotFleetRequestConfig'
data SpotFleetRequestConfig = SpotFleetRequestConfig'{_sfrcSpotFleetRequestId :: Text, _sfrcSpotFleetRequestState :: BatchState, _sfrcSpotFleetRequestConfig :: SpotFleetRequestConfigData} deriving (Eq, Read, Show)

-- | 'SpotFleetRequestConfig' smart constructor.
spotFleetRequestConfig :: Text -> BatchState -> SpotFleetRequestConfigData -> SpotFleetRequestConfig
spotFleetRequestConfig pSpotFleetRequestId pSpotFleetRequestState pSpotFleetRequestConfig = SpotFleetRequestConfig'{_sfrcSpotFleetRequestId = pSpotFleetRequestId, _sfrcSpotFleetRequestState = pSpotFleetRequestState, _sfrcSpotFleetRequestConfig = pSpotFleetRequestConfig};

-- | The ID of the Spot fleet request.
sfrcSpotFleetRequestId :: Lens' SpotFleetRequestConfig Text
sfrcSpotFleetRequestId = lens _sfrcSpotFleetRequestId (\ s a -> s{_sfrcSpotFleetRequestId = a});

-- | The state of the Spot fleet request.
sfrcSpotFleetRequestState :: Lens' SpotFleetRequestConfig BatchState
sfrcSpotFleetRequestState = lens _sfrcSpotFleetRequestState (\ s a -> s{_sfrcSpotFleetRequestState = a});

-- | Information about the configuration of the Spot fleet request.
sfrcSpotFleetRequestConfig :: Lens' SpotFleetRequestConfig SpotFleetRequestConfigData
sfrcSpotFleetRequestConfig = lens _sfrcSpotFleetRequestConfig (\ s a -> s{_sfrcSpotFleetRequestConfig = a});

instance FromXML SpotFleetRequestConfig where
        parseXML x
          = SpotFleetRequestConfig' <$>
              (x .@ "spotFleetRequestId") <*>
                (x .@ "spotFleetRequestState")
                <*> (x .@ "spotFleetRequestConfig")

-- | Describes the configuration of a Spot fleet request.
--
-- /See:/ 'spotFleetRequestConfigData' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sfrcdClientToken'
--
-- * 'sfrcdValidUntil'
--
-- * 'sfrcdTerminateInstancesWithExpiration'
--
-- * 'sfrcdValidFrom'
--
-- * 'sfrcdSpotPrice'
--
-- * 'sfrcdTargetCapacity'
--
-- * 'sfrcdIAMFleetRole'
--
-- * 'sfrcdLaunchSpecifications'
data SpotFleetRequestConfigData = SpotFleetRequestConfigData'{_sfrcdClientToken :: Maybe Text, _sfrcdValidUntil :: Maybe ISO8601, _sfrcdTerminateInstancesWithExpiration :: Maybe Bool, _sfrcdValidFrom :: Maybe ISO8601, _sfrcdSpotPrice :: Text, _sfrcdTargetCapacity :: Int, _sfrcdIAMFleetRole :: Text, _sfrcdLaunchSpecifications :: List1 LaunchSpecification} deriving (Eq, Read, Show)

-- | 'SpotFleetRequestConfigData' smart constructor.
spotFleetRequestConfigData :: Text -> Int -> Text -> NonEmpty LaunchSpecification -> SpotFleetRequestConfigData
spotFleetRequestConfigData pSpotPrice pTargetCapacity pIAMFleetRole pLaunchSpecifications = SpotFleetRequestConfigData'{_sfrcdClientToken = Nothing, _sfrcdValidUntil = Nothing, _sfrcdTerminateInstancesWithExpiration = Nothing, _sfrcdValidFrom = Nothing, _sfrcdSpotPrice = pSpotPrice, _sfrcdTargetCapacity = pTargetCapacity, _sfrcdIAMFleetRole = pIAMFleetRole, _sfrcdLaunchSpecifications = _List1 # pLaunchSpecifications};

-- | A unique, case-sensitive identifier you provide to ensure idempotency of
-- your listings. This helps avoid duplicate listings. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
sfrcdClientToken :: Lens' SpotFleetRequestConfigData (Maybe Text)
sfrcdClientToken = lens _sfrcdClientToken (\ s a -> s{_sfrcdClientToken = a});

-- | The end date and time of the request, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). At this point, no new Spot Instance
-- requests are placed or enabled to fulfill the request.
sfrcdValidUntil :: Lens' SpotFleetRequestConfigData (Maybe UTCTime)
sfrcdValidUntil = lens _sfrcdValidUntil (\ s a -> s{_sfrcdValidUntil = a}) . mapping _Time;

-- | Indicates whether running instances should be terminated when the Spot
-- fleet request expires.
sfrcdTerminateInstancesWithExpiration :: Lens' SpotFleetRequestConfigData (Maybe Bool)
sfrcdTerminateInstancesWithExpiration = lens _sfrcdTerminateInstancesWithExpiration (\ s a -> s{_sfrcdTerminateInstancesWithExpiration = a});

-- | The start date and time of the request, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). The default is to start fulfilling
-- the request immediately.
sfrcdValidFrom :: Lens' SpotFleetRequestConfigData (Maybe UTCTime)
sfrcdValidFrom = lens _sfrcdValidFrom (\ s a -> s{_sfrcdValidFrom = a}) . mapping _Time;

-- | The maximum hourly price (bid) for any Spot Instance launched to fulfill
-- the request.
sfrcdSpotPrice :: Lens' SpotFleetRequestConfigData Text
sfrcdSpotPrice = lens _sfrcdSpotPrice (\ s a -> s{_sfrcdSpotPrice = a});

-- | The maximum number of Spot Instances to launch.
sfrcdTargetCapacity :: Lens' SpotFleetRequestConfigData Int
sfrcdTargetCapacity = lens _sfrcdTargetCapacity (\ s a -> s{_sfrcdTargetCapacity = a});

-- | Grants the Spot fleet service permission to terminate instances on your
-- behalf when you cancel a Spot fleet request using
-- CancelSpotFleetRequests or when the Spot fleet request expires, if you
-- set @terminateInstancesWithExpiration@.
sfrcdIAMFleetRole :: Lens' SpotFleetRequestConfigData Text
sfrcdIAMFleetRole = lens _sfrcdIAMFleetRole (\ s a -> s{_sfrcdIAMFleetRole = a});

-- | Information about the launch specifications for the instances.
sfrcdLaunchSpecifications :: Lens' SpotFleetRequestConfigData (NonEmpty LaunchSpecification)
sfrcdLaunchSpecifications = lens _sfrcdLaunchSpecifications (\ s a -> s{_sfrcdLaunchSpecifications = a}) . _List1;

instance FromXML SpotFleetRequestConfigData where
        parseXML x
          = SpotFleetRequestConfigData' <$>
              (x .@? "clientToken") <*> (x .@? "validUntil") <*>
                (x .@? "terminateInstancesWithExpiration")
                <*> (x .@? "validFrom")
                <*> (x .@ "spotPrice")
                <*> (x .@ "targetCapacity")
                <*> (x .@ "iamFleetRole")
                <*> (parseXMLList1 "item" x)

instance ToQuery SpotFleetRequestConfigData where
        toQuery SpotFleetRequestConfigData'{..}
          = mconcat
              ["ClientToken" =: _sfrcdClientToken,
               "ValidUntil" =: _sfrcdValidUntil,
               "TerminateInstancesWithExpiration" =:
                 _sfrcdTerminateInstancesWithExpiration,
               "ValidFrom" =: _sfrcdValidFrom,
               "SpotPrice" =: _sfrcdSpotPrice,
               "TargetCapacity" =: _sfrcdTargetCapacity,
               "IamFleetRole" =: _sfrcdIAMFleetRole,
               toQueryList "item" _sfrcdLaunchSpecifications]

-- | Describe a Spot Instance request.
--
-- /See:/ 'spotInstanceRequest' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sirInstanceId'
--
-- * 'sirStatus'
--
-- * 'sirState'
--
-- * 'sirProductDescription'
--
-- * 'sirSpotPrice'
--
-- * 'sirAvailabilityZoneGroup'
--
-- * 'sirLaunchSpecification'
--
-- * 'sirLaunchedAvailabilityZone'
--
-- * 'sirValidUntil'
--
-- * 'sirFault'
--
-- * 'sirLaunchGroup'
--
-- * 'sirSpotInstanceRequestId'
--
-- * 'sirType'
--
-- * 'sirValidFrom'
--
-- * 'sirTags'
--
-- * 'sirCreateTime'
data SpotInstanceRequest = SpotInstanceRequest'{_sirInstanceId :: Maybe Text, _sirStatus :: Maybe SpotInstanceStatus, _sirState :: Maybe SpotInstanceState, _sirProductDescription :: Maybe RIProductDescription, _sirSpotPrice :: Maybe Text, _sirAvailabilityZoneGroup :: Maybe Text, _sirLaunchSpecification :: Maybe LaunchSpecification, _sirLaunchedAvailabilityZone :: Maybe Text, _sirValidUntil :: Maybe ISO8601, _sirFault :: Maybe SpotInstanceStateFault, _sirLaunchGroup :: Maybe Text, _sirSpotInstanceRequestId :: Maybe Text, _sirType :: Maybe SpotInstanceType, _sirValidFrom :: Maybe ISO8601, _sirTags :: Maybe [Tag], _sirCreateTime :: Maybe ISO8601} deriving (Eq, Read, Show)

-- | 'SpotInstanceRequest' smart constructor.
spotInstanceRequest :: SpotInstanceRequest
spotInstanceRequest = SpotInstanceRequest'{_sirInstanceId = Nothing, _sirStatus = Nothing, _sirState = Nothing, _sirProductDescription = Nothing, _sirSpotPrice = Nothing, _sirAvailabilityZoneGroup = Nothing, _sirLaunchSpecification = Nothing, _sirLaunchedAvailabilityZone = Nothing, _sirValidUntil = Nothing, _sirFault = Nothing, _sirLaunchGroup = Nothing, _sirSpotInstanceRequestId = Nothing, _sirType = Nothing, _sirValidFrom = Nothing, _sirTags = Nothing, _sirCreateTime = Nothing};

-- | The instance ID, if an instance has been launched to fulfill the Spot
-- Instance request.
sirInstanceId :: Lens' SpotInstanceRequest (Maybe Text)
sirInstanceId = lens _sirInstanceId (\ s a -> s{_sirInstanceId = a});

-- | The status code and status message describing the Spot Instance request.
sirStatus :: Lens' SpotInstanceRequest (Maybe SpotInstanceStatus)
sirStatus = lens _sirStatus (\ s a -> s{_sirStatus = a});

-- | The state of the Spot Instance request. Spot bid status information can
-- help you track your Spot Instance requests. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-bid-status.html Spot Bid Status>
-- in the /Amazon Elastic Compute Cloud User Guide/.
sirState :: Lens' SpotInstanceRequest (Maybe SpotInstanceState)
sirState = lens _sirState (\ s a -> s{_sirState = a});

-- | The product description associated with the Spot Instance.
sirProductDescription :: Lens' SpotInstanceRequest (Maybe RIProductDescription)
sirProductDescription = lens _sirProductDescription (\ s a -> s{_sirProductDescription = a});

-- | The maximum hourly price (bid) for any Spot Instance launched to fulfill
-- the request.
sirSpotPrice :: Lens' SpotInstanceRequest (Maybe Text)
sirSpotPrice = lens _sirSpotPrice (\ s a -> s{_sirSpotPrice = a});

-- | The Availability Zone group. If you specify the same Availability Zone
-- group for all Spot Instance requests, all Spot Instances are launched in
-- the same Availability Zone.
sirAvailabilityZoneGroup :: Lens' SpotInstanceRequest (Maybe Text)
sirAvailabilityZoneGroup = lens _sirAvailabilityZoneGroup (\ s a -> s{_sirAvailabilityZoneGroup = a});

-- | Additional information for launching instances.
sirLaunchSpecification :: Lens' SpotInstanceRequest (Maybe LaunchSpecification)
sirLaunchSpecification = lens _sirLaunchSpecification (\ s a -> s{_sirLaunchSpecification = a});

-- | The Availability Zone in which the bid is launched.
sirLaunchedAvailabilityZone :: Lens' SpotInstanceRequest (Maybe Text)
sirLaunchedAvailabilityZone = lens _sirLaunchedAvailabilityZone (\ s a -> s{_sirLaunchedAvailabilityZone = a});

-- | The end date of the request, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). If this is a one-time request, the
-- request remains active until all instances launch, the request is
-- canceled, or this date is reached. If the request is persistent, it
-- remains active until it is canceled or this date is reached.
sirValidUntil :: Lens' SpotInstanceRequest (Maybe UTCTime)
sirValidUntil = lens _sirValidUntil (\ s a -> s{_sirValidUntil = a}) . mapping _Time;

-- | The fault codes for the Spot Instance request, if any.
sirFault :: Lens' SpotInstanceRequest (Maybe SpotInstanceStateFault)
sirFault = lens _sirFault (\ s a -> s{_sirFault = a});

-- | The instance launch group. Launch groups are Spot Instances that launch
-- together and terminate together.
sirLaunchGroup :: Lens' SpotInstanceRequest (Maybe Text)
sirLaunchGroup = lens _sirLaunchGroup (\ s a -> s{_sirLaunchGroup = a});

-- | The ID of the Spot Instance request.
sirSpotInstanceRequestId :: Lens' SpotInstanceRequest (Maybe Text)
sirSpotInstanceRequestId = lens _sirSpotInstanceRequestId (\ s a -> s{_sirSpotInstanceRequestId = a});

-- | The Spot Instance request type.
sirType :: Lens' SpotInstanceRequest (Maybe SpotInstanceType)
sirType = lens _sirType (\ s a -> s{_sirType = a});

-- | The start date of the request, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). If this is a one-time request, the
-- request becomes active at this date and time and remains active until
-- all instances launch, the request expires, or the request is canceled.
-- If the request is persistent, the request becomes active at this date
-- and time and remains active until it expires or is canceled.
sirValidFrom :: Lens' SpotInstanceRequest (Maybe UTCTime)
sirValidFrom = lens _sirValidFrom (\ s a -> s{_sirValidFrom = a}) . mapping _Time;

-- | Any tags assigned to the resource.
sirTags :: Lens' SpotInstanceRequest [Tag]
sirTags = lens _sirTags (\ s a -> s{_sirTags = a}) . _Default;

-- | The date and time when the Spot Instance request was created, in UTC
-- format (for example, /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
sirCreateTime :: Lens' SpotInstanceRequest (Maybe UTCTime)
sirCreateTime = lens _sirCreateTime (\ s a -> s{_sirCreateTime = a}) . mapping _Time;

instance FromXML SpotInstanceRequest where
        parseXML x
          = SpotInstanceRequest' <$>
              (x .@? "instanceId") <*> (x .@? "status") <*>
                (x .@? "state")
                <*> (x .@? "productDescription")
                <*> (x .@? "spotPrice")
                <*> (x .@? "availabilityZoneGroup")
                <*> (x .@? "launchSpecification")
                <*> (x .@? "launchedAvailabilityZone")
                <*> (x .@? "validUntil")
                <*> (x .@? "fault")
                <*> (x .@? "launchGroup")
                <*> (x .@? "spotInstanceRequestId")
                <*> (x .@? "type")
                <*> (x .@? "validFrom")
                <*> (may (parseXMLList "item") x)
                <*> (x .@? "createTime")

-- | Describes a Spot Instance state change.
--
-- /See:/ 'spotInstanceStateFault' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sisfCode'
--
-- * 'sisfMessage'
data SpotInstanceStateFault = SpotInstanceStateFault'{_sisfCode :: Maybe Text, _sisfMessage :: Maybe Text} deriving (Eq, Read, Show)

-- | 'SpotInstanceStateFault' smart constructor.
spotInstanceStateFault :: SpotInstanceStateFault
spotInstanceStateFault = SpotInstanceStateFault'{_sisfCode = Nothing, _sisfMessage = Nothing};

-- | The reason code for the Spot Instance state change.
sisfCode :: Lens' SpotInstanceStateFault (Maybe Text)
sisfCode = lens _sisfCode (\ s a -> s{_sisfCode = a});

-- | The message for the Spot Instance state change.
sisfMessage :: Lens' SpotInstanceStateFault (Maybe Text)
sisfMessage = lens _sisfMessage (\ s a -> s{_sisfMessage = a});

instance FromXML SpotInstanceStateFault where
        parseXML x
          = SpotInstanceStateFault' <$>
              (x .@? "code") <*> (x .@? "message")

-- | Describes the status of a Spot Instance request.
--
-- /See:/ 'spotInstanceStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sisUpdateTime'
--
-- * 'sisCode'
--
-- * 'sisMessage'
data SpotInstanceStatus = SpotInstanceStatus'{_sisUpdateTime :: Maybe ISO8601, _sisCode :: Maybe Text, _sisMessage :: Maybe Text} deriving (Eq, Read, Show)

-- | 'SpotInstanceStatus' smart constructor.
spotInstanceStatus :: SpotInstanceStatus
spotInstanceStatus = SpotInstanceStatus'{_sisUpdateTime = Nothing, _sisCode = Nothing, _sisMessage = Nothing};

-- | The date and time of the most recent status update, in UTC format (for
-- example, /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
sisUpdateTime :: Lens' SpotInstanceStatus (Maybe UTCTime)
sisUpdateTime = lens _sisUpdateTime (\ s a -> s{_sisUpdateTime = a}) . mapping _Time;

-- | The status code.
sisCode :: Lens' SpotInstanceStatus (Maybe Text)
sisCode = lens _sisCode (\ s a -> s{_sisCode = a});

-- | The description for the status code.
sisMessage :: Lens' SpotInstanceStatus (Maybe Text)
sisMessage = lens _sisMessage (\ s a -> s{_sisMessage = a});

instance FromXML SpotInstanceStatus where
        parseXML x
          = SpotInstanceStatus' <$>
              (x .@? "updateTime") <*> (x .@? "code") <*>
                (x .@? "message")

-- | Describes Spot Instance placement.
--
-- /See:/ 'spotPlacement' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'spAvailabilityZone'
--
-- * 'spGroupName'
data SpotPlacement = SpotPlacement'{_spAvailabilityZone :: Maybe Text, _spGroupName :: Maybe Text} deriving (Eq, Read, Show)

-- | 'SpotPlacement' smart constructor.
spotPlacement :: SpotPlacement
spotPlacement = SpotPlacement'{_spAvailabilityZone = Nothing, _spGroupName = Nothing};

-- | The Availability Zone.
spAvailabilityZone :: Lens' SpotPlacement (Maybe Text)
spAvailabilityZone = lens _spAvailabilityZone (\ s a -> s{_spAvailabilityZone = a});

-- | The name of the placement group (for cluster instances).
spGroupName :: Lens' SpotPlacement (Maybe Text)
spGroupName = lens _spGroupName (\ s a -> s{_spGroupName = a});

instance FromXML SpotPlacement where
        parseXML x
          = SpotPlacement' <$>
              (x .@? "availabilityZone") <*> (x .@? "groupName")

instance ToQuery SpotPlacement where
        toQuery SpotPlacement'{..}
          = mconcat
              ["AvailabilityZone" =: _spAvailabilityZone,
               "GroupName" =: _spGroupName]

-- | Describes the maximum hourly price (bid) for any Spot Instance launched
-- to fulfill the request.
--
-- /See:/ 'spotPrice' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'spoProductDescription'
--
-- * 'spoSpotPrice'
--
-- * 'spoInstanceType'
--
-- * 'spoAvailabilityZone'
--
-- * 'spoTimestamp'
data SpotPrice = SpotPrice'{_spoProductDescription :: Maybe RIProductDescription, _spoSpotPrice :: Maybe Text, _spoInstanceType :: Maybe InstanceType, _spoAvailabilityZone :: Maybe Text, _spoTimestamp :: Maybe ISO8601} deriving (Eq, Read, Show)

-- | 'SpotPrice' smart constructor.
spotPrice :: SpotPrice
spotPrice = SpotPrice'{_spoProductDescription = Nothing, _spoSpotPrice = Nothing, _spoInstanceType = Nothing, _spoAvailabilityZone = Nothing, _spoTimestamp = Nothing};

-- | A general description of the AMI.
spoProductDescription :: Lens' SpotPrice (Maybe RIProductDescription)
spoProductDescription = lens _spoProductDescription (\ s a -> s{_spoProductDescription = a});

-- | The maximum price (bid) that you are willing to pay for a Spot Instance.
spoSpotPrice :: Lens' SpotPrice (Maybe Text)
spoSpotPrice = lens _spoSpotPrice (\ s a -> s{_spoSpotPrice = a});

-- | The instance type.
spoInstanceType :: Lens' SpotPrice (Maybe InstanceType)
spoInstanceType = lens _spoInstanceType (\ s a -> s{_spoInstanceType = a});

-- | The Availability Zone.
spoAvailabilityZone :: Lens' SpotPrice (Maybe Text)
spoAvailabilityZone = lens _spoAvailabilityZone (\ s a -> s{_spoAvailabilityZone = a});

-- | The date and time the request was created, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
spoTimestamp :: Lens' SpotPrice (Maybe UTCTime)
spoTimestamp = lens _spoTimestamp (\ s a -> s{_spoTimestamp = a}) . mapping _Time;

instance FromXML SpotPrice where
        parseXML x
          = SpotPrice' <$>
              (x .@? "productDescription") <*> (x .@? "spotPrice")
                <*> (x .@? "instanceType")
                <*> (x .@? "availabilityZone")
                <*> (x .@? "timestamp")

-- | Describes a state change.
--
-- /See:/ 'stateReason' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'srCode'
--
-- * 'srMessage'
data StateReason = StateReason'{_srCode :: Maybe Text, _srMessage :: Maybe Text} deriving (Eq, Read, Show)

-- | 'StateReason' smart constructor.
stateReason :: StateReason
stateReason = StateReason'{_srCode = Nothing, _srMessage = Nothing};

-- | The reason code for the state change.
srCode :: Lens' StateReason (Maybe Text)
srCode = lens _srCode (\ s a -> s{_srCode = a});

-- | The message for the state change.
--
-- -   @Server.SpotInstanceTermination@: A Spot Instance was terminated due
--     to an increase in the market price.
--
-- -   @Server.InternalError@: An internal error occurred during instance
--     launch, resulting in termination.
--
-- -   @Server.InsufficientInstanceCapacity@: There was insufficient
--     instance capacity to satisfy the launch request.
--
-- -   @Client.InternalError@: A client error caused the instance to
--     terminate on launch.
--
-- -   @Client.InstanceInitiatedShutdown@: The instance was shut down using
--     the @shutdown -h@ command from the instance.
--
-- -   @Client.UserInitiatedShutdown@: The instance was shut down using the
--     Amazon EC2 API.
--
-- -   @Client.VolumeLimitExceeded@: The volume limit was exceeded.
--
-- -   @Client.InvalidSnapshot.NotFound@: The specified snapshot was not
--     found.
--
srMessage :: Lens' StateReason (Maybe Text)
srMessage = lens _srMessage (\ s a -> s{_srMessage = a});

instance FromXML StateReason where
        parseXML x
          = StateReason' <$>
              (x .@? "code") <*> (x .@? "message")

-- | Describes the storage location for an instance store-backed AMI.
--
-- /See:/ 'storage' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'stoS3'
newtype Storage = Storage'{_stoS3 :: Maybe S3Storage} deriving (Eq, Read, Show)

-- | 'Storage' smart constructor.
storage :: Storage
storage = Storage'{_stoS3 = Nothing};

-- | An Amazon S3 storage location.
stoS3 :: Lens' Storage (Maybe S3Storage)
stoS3 = lens _stoS3 (\ s a -> s{_stoS3 = a});

instance FromXML Storage where
        parseXML x = Storage' <$> (x .@? "S3")

instance ToQuery Storage where
        toQuery Storage'{..} = mconcat ["S3" =: _stoS3]

-- | Describes a subnet.
--
-- /See:/ 'subnet' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'subTags'
--
-- * 'subAvailabilityZone'
--
-- * 'subAvailableIPAddressCount'
--
-- * 'subCIDRBlock'
--
-- * 'subDefaultForAz'
--
-- * 'subMapPublicIPOnLaunch'
--
-- * 'subState'
--
-- * 'subSubnetId'
--
-- * 'subVPCId'
data Subnet = Subnet'{_subTags :: Maybe [Tag], _subAvailabilityZone :: Text, _subAvailableIPAddressCount :: Int, _subCIDRBlock :: Text, _subDefaultForAz :: Bool, _subMapPublicIPOnLaunch :: Bool, _subState :: SubnetState, _subSubnetId :: Text, _subVPCId :: Text} deriving (Eq, Read, Show)

-- | 'Subnet' smart constructor.
subnet :: Text -> Int -> Text -> Bool -> Bool -> SubnetState -> Text -> Text -> Subnet
subnet pAvailabilityZone pAvailableIPAddressCount pCIDRBlock pDefaultForAz pMapPublicIPOnLaunch pState pSubnetId pVPCId = Subnet'{_subTags = Nothing, _subAvailabilityZone = pAvailabilityZone, _subAvailableIPAddressCount = pAvailableIPAddressCount, _subCIDRBlock = pCIDRBlock, _subDefaultForAz = pDefaultForAz, _subMapPublicIPOnLaunch = pMapPublicIPOnLaunch, _subState = pState, _subSubnetId = pSubnetId, _subVPCId = pVPCId};

-- | Any tags assigned to the subnet.
subTags :: Lens' Subnet [Tag]
subTags = lens _subTags (\ s a -> s{_subTags = a}) . _Default;

-- | The Availability Zone of the subnet.
subAvailabilityZone :: Lens' Subnet Text
subAvailabilityZone = lens _subAvailabilityZone (\ s a -> s{_subAvailabilityZone = a});

-- | The number of unused IP addresses in the subnet. Note that the IP
-- addresses for any stopped instances are considered unavailable.
subAvailableIPAddressCount :: Lens' Subnet Int
subAvailableIPAddressCount = lens _subAvailableIPAddressCount (\ s a -> s{_subAvailableIPAddressCount = a});

-- | The CIDR block assigned to the subnet.
subCIDRBlock :: Lens' Subnet Text
subCIDRBlock = lens _subCIDRBlock (\ s a -> s{_subCIDRBlock = a});

-- | Indicates whether this is the default subnet for the Availability Zone.
subDefaultForAz :: Lens' Subnet Bool
subDefaultForAz = lens _subDefaultForAz (\ s a -> s{_subDefaultForAz = a});

-- | Indicates whether instances launched in this subnet receive a public IP
-- address.
subMapPublicIPOnLaunch :: Lens' Subnet Bool
subMapPublicIPOnLaunch = lens _subMapPublicIPOnLaunch (\ s a -> s{_subMapPublicIPOnLaunch = a});

-- | The current state of the subnet.
subState :: Lens' Subnet SubnetState
subState = lens _subState (\ s a -> s{_subState = a});

-- | The ID of the subnet.
subSubnetId :: Lens' Subnet Text
subSubnetId = lens _subSubnetId (\ s a -> s{_subSubnetId = a});

-- | The ID of the VPC the subnet is in.
subVPCId :: Lens' Subnet Text
subVPCId = lens _subVPCId (\ s a -> s{_subVPCId = a});

instance FromXML Subnet where
        parseXML x
          = Subnet' <$>
              (may (parseXMLList "item") x) <*>
                (x .@ "availabilityZone")
                <*> (x .@ "availableIpAddressCount")
                <*> (x .@ "cidrBlock")
                <*> (x .@ "defaultForAz")
                <*> (x .@ "mapPublicIpOnLaunch")
                <*> (x .@ "state")
                <*> (x .@ "subnetId")
                <*> (x .@ "vpcId")

-- | Describes a tag.
--
-- /See:/ 'tag' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tagKey'
--
-- * 'tagValue'
data Tag = Tag'{_tagKey :: Text, _tagValue :: Text} deriving (Eq, Read, Show)

-- | 'Tag' smart constructor.
tag :: Text -> Text -> Tag
tag pKey pValue = Tag'{_tagKey = pKey, _tagValue = pValue};

-- | The key of the tag.
--
-- Constraints: Tag keys are case-sensitive and accept a maximum of 127
-- Unicode characters. May not begin with @aws:@
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\ s a -> s{_tagKey = a});

-- | The value of the tag.
--
-- Constraints: Tag values are case-sensitive and accept a maximum of 255
-- Unicode characters.
tagValue :: Lens' Tag Text
tagValue = lens _tagValue (\ s a -> s{_tagValue = a});

instance FromXML Tag where
        parseXML x = Tag' <$> (x .@ "key") <*> (x .@ "value")

instance ToQuery Tag where
        toQuery Tag'{..}
          = mconcat ["Key" =: _tagKey, "Value" =: _tagValue]

-- | Describes a tag.
--
-- /See:/ 'tagDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tdResourceId'
--
-- * 'tdResourceType'
--
-- * 'tdKey'
--
-- * 'tdValue'
data TagDescription = TagDescription'{_tdResourceId :: Text, _tdResourceType :: ResourceType, _tdKey :: Text, _tdValue :: Text} deriving (Eq, Read, Show)

-- | 'TagDescription' smart constructor.
tagDescription :: Text -> ResourceType -> Text -> Text -> TagDescription
tagDescription pResourceId pResourceType pKey pValue = TagDescription'{_tdResourceId = pResourceId, _tdResourceType = pResourceType, _tdKey = pKey, _tdValue = pValue};

-- | The ID of the resource. For example, @ami-1a2b3c4d@.
tdResourceId :: Lens' TagDescription Text
tdResourceId = lens _tdResourceId (\ s a -> s{_tdResourceId = a});

-- | The resource type.
tdResourceType :: Lens' TagDescription ResourceType
tdResourceType = lens _tdResourceType (\ s a -> s{_tdResourceType = a});

-- | The tag key.
tdKey :: Lens' TagDescription Text
tdKey = lens _tdKey (\ s a -> s{_tdKey = a});

-- | The tag value.
tdValue :: Lens' TagDescription Text
tdValue = lens _tdValue (\ s a -> s{_tdValue = a});

instance FromXML TagDescription where
        parseXML x
          = TagDescription' <$>
              (x .@ "resourceId") <*> (x .@ "resourceType") <*>
                (x .@ "key")
                <*> (x .@ "value")

-- | Information about items that were not successfully processed in a batch
-- call.
--
-- /See:/ 'unsuccessfulItem' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uiResourceId'
--
-- * 'uiError'
data UnsuccessfulItem = UnsuccessfulItem'{_uiResourceId :: Maybe Text, _uiError :: UnsuccessfulItemError} deriving (Eq, Read, Show)

-- | 'UnsuccessfulItem' smart constructor.
unsuccessfulItem :: UnsuccessfulItemError -> UnsuccessfulItem
unsuccessfulItem pError = UnsuccessfulItem'{_uiResourceId = Nothing, _uiError = pError};

-- | The ID of the resource.
uiResourceId :: Lens' UnsuccessfulItem (Maybe Text)
uiResourceId = lens _uiResourceId (\ s a -> s{_uiResourceId = a});

-- | Information about the error.
uiError :: Lens' UnsuccessfulItem UnsuccessfulItemError
uiError = lens _uiError (\ s a -> s{_uiError = a});

instance FromXML UnsuccessfulItem where
        parseXML x
          = UnsuccessfulItem' <$>
              (x .@? "resourceId") <*> (x .@ "error")

-- | Information about the error that occured. For more information about
-- errors, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html Error Codes>.
--
-- /See:/ 'unsuccessfulItemError' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uieCode'
--
-- * 'uieMessage'
data UnsuccessfulItemError = UnsuccessfulItemError'{_uieCode :: Text, _uieMessage :: Text} deriving (Eq, Read, Show)

-- | 'UnsuccessfulItemError' smart constructor.
unsuccessfulItemError :: Text -> Text -> UnsuccessfulItemError
unsuccessfulItemError pCode pMessage = UnsuccessfulItemError'{_uieCode = pCode, _uieMessage = pMessage};

-- | The error code.
uieCode :: Lens' UnsuccessfulItemError Text
uieCode = lens _uieCode (\ s a -> s{_uieCode = a});

-- | The error message accompanying the error code.
uieMessage :: Lens' UnsuccessfulItemError Text
uieMessage = lens _uieMessage (\ s a -> s{_uieMessage = a});

instance FromXML UnsuccessfulItemError where
        parseXML x
          = UnsuccessfulItemError' <$>
              (x .@ "code") <*> (x .@ "message")

-- | Describes the S3 bucket for the disk image.
--
-- /See:/ 'userBucket' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ubS3Key'
--
-- * 'ubS3Bucket'
data UserBucket = UserBucket'{_ubS3Key :: Maybe Text, _ubS3Bucket :: Maybe Text} deriving (Eq, Read, Show)

-- | 'UserBucket' smart constructor.
userBucket :: UserBucket
userBucket = UserBucket'{_ubS3Key = Nothing, _ubS3Bucket = Nothing};

-- | The key for the disk image.
ubS3Key :: Lens' UserBucket (Maybe Text)
ubS3Key = lens _ubS3Key (\ s a -> s{_ubS3Key = a});

-- | The name of the S3 bucket where the disk image is located.
ubS3Bucket :: Lens' UserBucket (Maybe Text)
ubS3Bucket = lens _ubS3Bucket (\ s a -> s{_ubS3Bucket = a});

instance ToQuery UserBucket where
        toQuery UserBucket'{..}
          = mconcat
              ["S3Key" =: _ubS3Key, "S3Bucket" =: _ubS3Bucket]

-- | Describes the S3 bucket for the disk image.
--
-- /See:/ 'userBucketDetails' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ubdS3Key'
--
-- * 'ubdS3Bucket'
data UserBucketDetails = UserBucketDetails'{_ubdS3Key :: Maybe Text, _ubdS3Bucket :: Maybe Text} deriving (Eq, Read, Show)

-- | 'UserBucketDetails' smart constructor.
userBucketDetails :: UserBucketDetails
userBucketDetails = UserBucketDetails'{_ubdS3Key = Nothing, _ubdS3Bucket = Nothing};

-- | The key from which the disk image was created.
ubdS3Key :: Lens' UserBucketDetails (Maybe Text)
ubdS3Key = lens _ubdS3Key (\ s a -> s{_ubdS3Key = a});

-- | The S3 bucket from which the disk image was created.
ubdS3Bucket :: Lens' UserBucketDetails (Maybe Text)
ubdS3Bucket = lens _ubdS3Bucket (\ s a -> s{_ubdS3Bucket = a});

instance FromXML UserBucketDetails where
        parseXML x
          = UserBucketDetails' <$>
              (x .@? "s3Key") <*> (x .@? "s3Bucket")

-- | Describes the user data to be made available to an instance.
--
-- /See:/ 'userData' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'udData'
newtype UserData = UserData'{_udData :: Maybe Text} deriving (Eq, Read, Show)

-- | 'UserData' smart constructor.
userData :: UserData
userData = UserData'{_udData = Nothing};

-- | The Base64-encoded MIME user data for the instance.
udData :: Lens' UserData (Maybe Text)
udData = lens _udData (\ s a -> s{_udData = a});

instance ToQuery UserData where
        toQuery UserData'{..} = mconcat ["Data" =: _udData]

-- | Describes a security group and AWS account ID pair.
--
-- /See:/ 'userIdGroupPair' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uigpUserId'
--
-- * 'uigpGroupId'
--
-- * 'uigpGroupName'
data UserIdGroupPair = UserIdGroupPair'{_uigpUserId :: Maybe Text, _uigpGroupId :: Maybe Text, _uigpGroupName :: Maybe Text} deriving (Eq, Read, Show)

-- | 'UserIdGroupPair' smart constructor.
userIdGroupPair :: UserIdGroupPair
userIdGroupPair = UserIdGroupPair'{_uigpUserId = Nothing, _uigpGroupId = Nothing, _uigpGroupName = Nothing};

-- | The ID of an AWS account. EC2-Classic only.
uigpUserId :: Lens' UserIdGroupPair (Maybe Text)
uigpUserId = lens _uigpUserId (\ s a -> s{_uigpUserId = a});

-- | The ID of the security group.
uigpGroupId :: Lens' UserIdGroupPair (Maybe Text)
uigpGroupId = lens _uigpGroupId (\ s a -> s{_uigpGroupId = a});

-- | The name of the security group. In a request, use this parameter for a
-- security group in EC2-Classic or a default VPC only. For a security
-- group in a nondefault VPC, use @GroupId@.
uigpGroupName :: Lens' UserIdGroupPair (Maybe Text)
uigpGroupName = lens _uigpGroupName (\ s a -> s{_uigpGroupName = a});

instance FromXML UserIdGroupPair where
        parseXML x
          = UserIdGroupPair' <$>
              (x .@? "userId") <*> (x .@? "groupId") <*>
                (x .@? "groupName")

instance ToQuery UserIdGroupPair where
        toQuery UserIdGroupPair'{..}
          = mconcat
              ["UserId" =: _uigpUserId, "GroupId" =: _uigpGroupId,
               "GroupName" =: _uigpGroupName]

-- | Describes telemetry for a VPN tunnel.
--
-- /See:/ 'vgwTelemetry' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vtStatus'
--
-- * 'vtOutsideIPAddress'
--
-- * 'vtLastStatusChange'
--
-- * 'vtAcceptedRouteCount'
--
-- * 'vtStatusMessage'
data VGWTelemetry = VGWTelemetry'{_vtStatus :: Maybe TelemetryStatus, _vtOutsideIPAddress :: Maybe Text, _vtLastStatusChange :: Maybe ISO8601, _vtAcceptedRouteCount :: Maybe Int, _vtStatusMessage :: Maybe Text} deriving (Eq, Read, Show)

-- | 'VGWTelemetry' smart constructor.
vgwTelemetry :: VGWTelemetry
vgwTelemetry = VGWTelemetry'{_vtStatus = Nothing, _vtOutsideIPAddress = Nothing, _vtLastStatusChange = Nothing, _vtAcceptedRouteCount = Nothing, _vtStatusMessage = Nothing};

-- | The status of the VPN tunnel.
vtStatus :: Lens' VGWTelemetry (Maybe TelemetryStatus)
vtStatus = lens _vtStatus (\ s a -> s{_vtStatus = a});

-- | The Internet-routable IP address of the virtual private gateway\'s
-- outside interface.
vtOutsideIPAddress :: Lens' VGWTelemetry (Maybe Text)
vtOutsideIPAddress = lens _vtOutsideIPAddress (\ s a -> s{_vtOutsideIPAddress = a});

-- | The date and time of the last change in status.
vtLastStatusChange :: Lens' VGWTelemetry (Maybe UTCTime)
vtLastStatusChange = lens _vtLastStatusChange (\ s a -> s{_vtLastStatusChange = a}) . mapping _Time;

-- | The number of accepted routes.
vtAcceptedRouteCount :: Lens' VGWTelemetry (Maybe Int)
vtAcceptedRouteCount = lens _vtAcceptedRouteCount (\ s a -> s{_vtAcceptedRouteCount = a});

-- | If an error occurs, a description of the error.
vtStatusMessage :: Lens' VGWTelemetry (Maybe Text)
vtStatusMessage = lens _vtStatusMessage (\ s a -> s{_vtStatusMessage = a});

instance FromXML VGWTelemetry where
        parseXML x
          = VGWTelemetry' <$>
              (x .@? "status") <*> (x .@? "outsideIpAddress") <*>
                (x .@? "lastStatusChange")
                <*> (x .@? "acceptedRouteCount")
                <*> (x .@? "statusMessage")

-- | Describes a VPC.
--
-- /See:/ 'vpc' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vpcTags'
--
-- * 'vpcCIDRBlock'
--
-- * 'vpcDHCPOptionsId'
--
-- * 'vpcInstanceTenancy'
--
-- * 'vpcIsDefault'
--
-- * 'vpcState'
--
-- * 'vpcVPCId'
data VPC = VPC'{_vpcTags :: Maybe [Tag], _vpcCIDRBlock :: Text, _vpcDHCPOptionsId :: Text, _vpcInstanceTenancy :: Tenancy, _vpcIsDefault :: Bool, _vpcState :: VPCState, _vpcVPCId :: Text} deriving (Eq, Read, Show)

-- | 'VPC' smart constructor.
vpc :: Text -> Text -> Tenancy -> Bool -> VPCState -> Text -> VPC
vpc pCIDRBlock pDHCPOptionsId pInstanceTenancy pIsDefault pState pVPCId = VPC'{_vpcTags = Nothing, _vpcCIDRBlock = pCIDRBlock, _vpcDHCPOptionsId = pDHCPOptionsId, _vpcInstanceTenancy = pInstanceTenancy, _vpcIsDefault = pIsDefault, _vpcState = pState, _vpcVPCId = pVPCId};

-- | Any tags assigned to the VPC.
vpcTags :: Lens' VPC [Tag]
vpcTags = lens _vpcTags (\ s a -> s{_vpcTags = a}) . _Default;

-- | The CIDR block for the VPC.
vpcCIDRBlock :: Lens' VPC Text
vpcCIDRBlock = lens _vpcCIDRBlock (\ s a -> s{_vpcCIDRBlock = a});

-- | The ID of the set of DHCP options you\'ve associated with the VPC (or
-- @default@ if the default options are associated with the VPC).
vpcDHCPOptionsId :: Lens' VPC Text
vpcDHCPOptionsId = lens _vpcDHCPOptionsId (\ s a -> s{_vpcDHCPOptionsId = a});

-- | The allowed tenancy of instances launched into the VPC.
vpcInstanceTenancy :: Lens' VPC Tenancy
vpcInstanceTenancy = lens _vpcInstanceTenancy (\ s a -> s{_vpcInstanceTenancy = a});

-- | Indicates whether the VPC is the default VPC.
vpcIsDefault :: Lens' VPC Bool
vpcIsDefault = lens _vpcIsDefault (\ s a -> s{_vpcIsDefault = a});

-- | The current state of the VPC.
vpcState :: Lens' VPC VPCState
vpcState = lens _vpcState (\ s a -> s{_vpcState = a});

-- | The ID of the VPC.
vpcVPCId :: Lens' VPC Text
vpcVPCId = lens _vpcVPCId (\ s a -> s{_vpcVPCId = a});

instance FromXML VPC where
        parseXML x
          = VPC' <$>
              (may (parseXMLList "item") x) <*> (x .@ "cidrBlock")
                <*> (x .@ "dhcpOptionsId")
                <*> (x .@ "instanceTenancy")
                <*> (x .@ "isDefault")
                <*> (x .@ "state")
                <*> (x .@ "vpcId")

-- | Describes an attachment between a virtual private gateway and a VPC.
--
-- /See:/ 'vpcAttachment' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vaState'
--
-- * 'vaVPCId'
data VPCAttachment = VPCAttachment'{_vaState :: Maybe AttachmentStatus, _vaVPCId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'VPCAttachment' smart constructor.
vpcAttachment :: VPCAttachment
vpcAttachment = VPCAttachment'{_vaState = Nothing, _vaVPCId = Nothing};

-- | The current state of the attachment.
vaState :: Lens' VPCAttachment (Maybe AttachmentStatus)
vaState = lens _vaState (\ s a -> s{_vaState = a});

-- | The ID of the VPC.
vaVPCId :: Lens' VPCAttachment (Maybe Text)
vaVPCId = lens _vaVPCId (\ s a -> s{_vaVPCId = a});

instance FromXML VPCAttachment where
        parseXML x
          = VPCAttachment' <$>
              (x .@? "state") <*> (x .@? "vpcId")

-- | Describes whether a VPC is enabled for ClassicLink.
--
-- /See:/ 'vpcClassicLink' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vclVPCId'
--
-- * 'vclTags'
--
-- * 'vclClassicLinkEnabled'
data VPCClassicLink = VPCClassicLink'{_vclVPCId :: Maybe Text, _vclTags :: Maybe [Tag], _vclClassicLinkEnabled :: Maybe Bool} deriving (Eq, Read, Show)

-- | 'VPCClassicLink' smart constructor.
vpcClassicLink :: VPCClassicLink
vpcClassicLink = VPCClassicLink'{_vclVPCId = Nothing, _vclTags = Nothing, _vclClassicLinkEnabled = Nothing};

-- | The ID of the VPC.
vclVPCId :: Lens' VPCClassicLink (Maybe Text)
vclVPCId = lens _vclVPCId (\ s a -> s{_vclVPCId = a});

-- | Any tags assigned to the VPC.
vclTags :: Lens' VPCClassicLink [Tag]
vclTags = lens _vclTags (\ s a -> s{_vclTags = a}) . _Default;

-- | Indicates whether the VPC is enabled for ClassicLink.
vclClassicLinkEnabled :: Lens' VPCClassicLink (Maybe Bool)
vclClassicLinkEnabled = lens _vclClassicLinkEnabled (\ s a -> s{_vclClassicLinkEnabled = a});

instance FromXML VPCClassicLink where
        parseXML x
          = VPCClassicLink' <$>
              (x .@? "vpcId") <*> (may (parseXMLList "item") x) <*>
                (x .@? "classicLinkEnabled")

-- | Describes a VPC endpoint.
--
-- /See:/ 'vpcEndpoint' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vePolicyDocument'
--
-- * 'veState'
--
-- * 'veVPCId'
--
-- * 'veCreationTimestamp'
--
-- * 'veServiceName'
--
-- * 'veVPCEndpointId'
--
-- * 'veRouteTableIds'
data VPCEndpoint = VPCEndpoint'{_vePolicyDocument :: Maybe Text, _veState :: Maybe State, _veVPCId :: Maybe Text, _veCreationTimestamp :: Maybe ISO8601, _veServiceName :: Maybe Text, _veVPCEndpointId :: Maybe Text, _veRouteTableIds :: Maybe [Text]} deriving (Eq, Read, Show)

-- | 'VPCEndpoint' smart constructor.
vpcEndpoint :: VPCEndpoint
vpcEndpoint = VPCEndpoint'{_vePolicyDocument = Nothing, _veState = Nothing, _veVPCId = Nothing, _veCreationTimestamp = Nothing, _veServiceName = Nothing, _veVPCEndpointId = Nothing, _veRouteTableIds = Nothing};

-- | The policy document associated with the endpoint.
vePolicyDocument :: Lens' VPCEndpoint (Maybe Text)
vePolicyDocument = lens _vePolicyDocument (\ s a -> s{_vePolicyDocument = a});

-- | The state of the VPC endpoint.
veState :: Lens' VPCEndpoint (Maybe State)
veState = lens _veState (\ s a -> s{_veState = a});

-- | The ID of the VPC to which the endpoint is associated.
veVPCId :: Lens' VPCEndpoint (Maybe Text)
veVPCId = lens _veVPCId (\ s a -> s{_veVPCId = a});

-- | The date and time the VPC endpoint was created.
veCreationTimestamp :: Lens' VPCEndpoint (Maybe UTCTime)
veCreationTimestamp = lens _veCreationTimestamp (\ s a -> s{_veCreationTimestamp = a}) . mapping _Time;

-- | The name of the AWS service to which the endpoint is associated.
veServiceName :: Lens' VPCEndpoint (Maybe Text)
veServiceName = lens _veServiceName (\ s a -> s{_veServiceName = a});

-- | The ID of the VPC endpoint.
veVPCEndpointId :: Lens' VPCEndpoint (Maybe Text)
veVPCEndpointId = lens _veVPCEndpointId (\ s a -> s{_veVPCEndpointId = a});

-- | One or more route tables associated with the endpoint.
veRouteTableIds :: Lens' VPCEndpoint [Text]
veRouteTableIds = lens _veRouteTableIds (\ s a -> s{_veRouteTableIds = a}) . _Default;

instance FromXML VPCEndpoint where
        parseXML x
          = VPCEndpoint' <$>
              (x .@? "policyDocument") <*> (x .@? "state") <*>
                (x .@? "vpcId")
                <*> (x .@? "creationTimestamp")
                <*> (x .@? "serviceName")
                <*> (x .@? "vpcEndpointId")
                <*> (may (parseXMLList "item") x)

-- | Describes a VPC peering connection.
--
-- /See:/ 'vpcPeeringConnection' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vStatus'
--
-- * 'vVPCPeeringConnectionId'
--
-- * 'vAccepterVPCInfo'
--
-- * 'vRequesterVPCInfo'
--
-- * 'vExpirationTime'
--
-- * 'vTags'
data VPCPeeringConnection = VPCPeeringConnection'{_vStatus :: Maybe VPCPeeringConnectionStateReason, _vVPCPeeringConnectionId :: Maybe Text, _vAccepterVPCInfo :: Maybe VPCPeeringConnectionVPCInfo, _vRequesterVPCInfo :: Maybe VPCPeeringConnectionVPCInfo, _vExpirationTime :: Maybe ISO8601, _vTags :: Maybe [Tag]} deriving (Eq, Read, Show)

-- | 'VPCPeeringConnection' smart constructor.
vpcPeeringConnection :: VPCPeeringConnection
vpcPeeringConnection = VPCPeeringConnection'{_vStatus = Nothing, _vVPCPeeringConnectionId = Nothing, _vAccepterVPCInfo = Nothing, _vRequesterVPCInfo = Nothing, _vExpirationTime = Nothing, _vTags = Nothing};

-- | The status of the VPC peering connection.
vStatus :: Lens' VPCPeeringConnection (Maybe VPCPeeringConnectionStateReason)
vStatus = lens _vStatus (\ s a -> s{_vStatus = a});

-- | The ID of the VPC peering connection.
vVPCPeeringConnectionId :: Lens' VPCPeeringConnection (Maybe Text)
vVPCPeeringConnectionId = lens _vVPCPeeringConnectionId (\ s a -> s{_vVPCPeeringConnectionId = a});

-- | The information of the peer VPC.
vAccepterVPCInfo :: Lens' VPCPeeringConnection (Maybe VPCPeeringConnectionVPCInfo)
vAccepterVPCInfo = lens _vAccepterVPCInfo (\ s a -> s{_vAccepterVPCInfo = a});

-- | The information of the requester VPC.
vRequesterVPCInfo :: Lens' VPCPeeringConnection (Maybe VPCPeeringConnectionVPCInfo)
vRequesterVPCInfo = lens _vRequesterVPCInfo (\ s a -> s{_vRequesterVPCInfo = a});

-- | The time that an unaccepted VPC peering connection will expire.
vExpirationTime :: Lens' VPCPeeringConnection (Maybe UTCTime)
vExpirationTime = lens _vExpirationTime (\ s a -> s{_vExpirationTime = a}) . mapping _Time;

-- | Any tags assigned to the resource.
vTags :: Lens' VPCPeeringConnection [Tag]
vTags = lens _vTags (\ s a -> s{_vTags = a}) . _Default;

instance FromXML VPCPeeringConnection where
        parseXML x
          = VPCPeeringConnection' <$>
              (x .@? "status") <*> (x .@? "vpcPeeringConnectionId")
                <*> (x .@? "accepterVpcInfo")
                <*> (x .@? "requesterVpcInfo")
                <*> (x .@? "expirationTime")
                <*> (may (parseXMLList "item") x)

-- | Describes the status of a VPC peering connection.
--
-- /See:/ 'vpcPeeringConnectionStateReason' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vpcsrCode'
--
-- * 'vpcsrMessage'
data VPCPeeringConnectionStateReason = VPCPeeringConnectionStateReason'{_vpcsrCode :: Maybe Text, _vpcsrMessage :: Maybe Text} deriving (Eq, Read, Show)

-- | 'VPCPeeringConnectionStateReason' smart constructor.
vpcPeeringConnectionStateReason :: VPCPeeringConnectionStateReason
vpcPeeringConnectionStateReason = VPCPeeringConnectionStateReason'{_vpcsrCode = Nothing, _vpcsrMessage = Nothing};

-- | The status of the VPC peering connection.
vpcsrCode :: Lens' VPCPeeringConnectionStateReason (Maybe Text)
vpcsrCode = lens _vpcsrCode (\ s a -> s{_vpcsrCode = a});

-- | A message that provides more information about the status, if
-- applicable.
vpcsrMessage :: Lens' VPCPeeringConnectionStateReason (Maybe Text)
vpcsrMessage = lens _vpcsrMessage (\ s a -> s{_vpcsrMessage = a});

instance FromXML VPCPeeringConnectionStateReason
         where
        parseXML x
          = VPCPeeringConnectionStateReason' <$>
              (x .@? "code") <*> (x .@? "message")

-- | Describes a VPC in a VPC peering connection.
--
-- /See:/ 'vpcPeeringConnectionVPCInfo' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vpcviVPCId'
--
-- * 'vpcviOwnerId'
--
-- * 'vpcviCIDRBlock'
data VPCPeeringConnectionVPCInfo = VPCPeeringConnectionVPCInfo'{_vpcviVPCId :: Maybe Text, _vpcviOwnerId :: Maybe Text, _vpcviCIDRBlock :: Maybe Text} deriving (Eq, Read, Show)

-- | 'VPCPeeringConnectionVPCInfo' smart constructor.
vpcPeeringConnectionVPCInfo :: VPCPeeringConnectionVPCInfo
vpcPeeringConnectionVPCInfo = VPCPeeringConnectionVPCInfo'{_vpcviVPCId = Nothing, _vpcviOwnerId = Nothing, _vpcviCIDRBlock = Nothing};

-- | The ID of the VPC.
vpcviVPCId :: Lens' VPCPeeringConnectionVPCInfo (Maybe Text)
vpcviVPCId = lens _vpcviVPCId (\ s a -> s{_vpcviVPCId = a});

-- | The AWS account ID of the VPC owner.
vpcviOwnerId :: Lens' VPCPeeringConnectionVPCInfo (Maybe Text)
vpcviOwnerId = lens _vpcviOwnerId (\ s a -> s{_vpcviOwnerId = a});

-- | The CIDR block for the VPC.
vpcviCIDRBlock :: Lens' VPCPeeringConnectionVPCInfo (Maybe Text)
vpcviCIDRBlock = lens _vpcviCIDRBlock (\ s a -> s{_vpcviCIDRBlock = a});

instance FromXML VPCPeeringConnectionVPCInfo where
        parseXML x
          = VPCPeeringConnectionVPCInfo' <$>
              (x .@? "vpcId") <*> (x .@? "ownerId") <*>
                (x .@? "cidrBlock")

-- | Describes a VPN connection.
--
-- /See:/ 'vpnConnection' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vcRoutes'
--
-- * 'vcVPNGatewayId'
--
-- * 'vcOptions'
--
-- * 'vcVGWTelemetry'
--
-- * 'vcTags'
--
-- * 'vcVPNConnectionId'
--
-- * 'vcCustomerGatewayId'
--
-- * 'vcCustomerGatewayConfiguration'
--
-- * 'vcState'
--
-- * 'vcType'
data VPNConnection = VPNConnection'{_vcRoutes :: Maybe [VPNStaticRoute], _vcVPNGatewayId :: Maybe Text, _vcOptions :: Maybe VPNConnectionOptions, _vcVGWTelemetry :: Maybe [VGWTelemetry], _vcTags :: Maybe [Tag], _vcVPNConnectionId :: Text, _vcCustomerGatewayId :: Text, _vcCustomerGatewayConfiguration :: Text, _vcState :: VPNState, _vcType :: GatewayType} deriving (Eq, Read, Show)

-- | 'VPNConnection' smart constructor.
vpnConnection :: Text -> Text -> Text -> VPNState -> GatewayType -> VPNConnection
vpnConnection pVPNConnectionId pCustomerGatewayId pCustomerGatewayConfiguration pState pType = VPNConnection'{_vcRoutes = Nothing, _vcVPNGatewayId = Nothing, _vcOptions = Nothing, _vcVGWTelemetry = Nothing, _vcTags = Nothing, _vcVPNConnectionId = pVPNConnectionId, _vcCustomerGatewayId = pCustomerGatewayId, _vcCustomerGatewayConfiguration = pCustomerGatewayConfiguration, _vcState = pState, _vcType = pType};

-- | The static routes associated with the VPN connection.
vcRoutes :: Lens' VPNConnection [VPNStaticRoute]
vcRoutes = lens _vcRoutes (\ s a -> s{_vcRoutes = a}) . _Default;

-- | The ID of the virtual private gateway at the AWS side of the VPN
-- connection.
vcVPNGatewayId :: Lens' VPNConnection (Maybe Text)
vcVPNGatewayId = lens _vcVPNGatewayId (\ s a -> s{_vcVPNGatewayId = a});

-- | The VPN connection options.
vcOptions :: Lens' VPNConnection (Maybe VPNConnectionOptions)
vcOptions = lens _vcOptions (\ s a -> s{_vcOptions = a});

-- | Information about the VPN tunnel.
vcVGWTelemetry :: Lens' VPNConnection [VGWTelemetry]
vcVGWTelemetry = lens _vcVGWTelemetry (\ s a -> s{_vcVGWTelemetry = a}) . _Default;

-- | Any tags assigned to the VPN connection.
vcTags :: Lens' VPNConnection [Tag]
vcTags = lens _vcTags (\ s a -> s{_vcTags = a}) . _Default;

-- | The ID of the VPN connection.
vcVPNConnectionId :: Lens' VPNConnection Text
vcVPNConnectionId = lens _vcVPNConnectionId (\ s a -> s{_vcVPNConnectionId = a});

-- | The ID of the customer gateway at your end of the VPN connection.
vcCustomerGatewayId :: Lens' VPNConnection Text
vcCustomerGatewayId = lens _vcCustomerGatewayId (\ s a -> s{_vcCustomerGatewayId = a});

-- | The configuration information for the VPN connection\'s customer gateway
-- (in the native XML format). This element is always present in the
-- CreateVpnConnection response; however, it\'s present in the
-- DescribeVpnConnections response only if the VPN connection is in the
-- @pending@ or @available@ state.
vcCustomerGatewayConfiguration :: Lens' VPNConnection Text
vcCustomerGatewayConfiguration = lens _vcCustomerGatewayConfiguration (\ s a -> s{_vcCustomerGatewayConfiguration = a});

-- | The current state of the VPN connection.
vcState :: Lens' VPNConnection VPNState
vcState = lens _vcState (\ s a -> s{_vcState = a});

-- | The type of VPN connection.
vcType :: Lens' VPNConnection GatewayType
vcType = lens _vcType (\ s a -> s{_vcType = a});

instance FromXML VPNConnection where
        parseXML x
          = VPNConnection' <$>
              (may (parseXMLList "item") x) <*>
                (x .@? "vpnGatewayId")
                <*> (x .@? "options")
                <*> (may (parseXMLList "item") x)
                <*> (may (parseXMLList "item") x)
                <*> (x .@ "vpnConnectionId")
                <*> (x .@ "customerGatewayId")
                <*> (x .@ "customerGatewayConfiguration")
                <*> (x .@ "state")
                <*> (x .@ "type")

-- | Describes VPN connection options.
--
-- /See:/ 'vpnConnectionOptions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vcoStaticRoutesOnly'
newtype VPNConnectionOptions = VPNConnectionOptions'{_vcoStaticRoutesOnly :: Maybe Bool} deriving (Eq, Read, Show)

-- | 'VPNConnectionOptions' smart constructor.
vpnConnectionOptions :: VPNConnectionOptions
vpnConnectionOptions = VPNConnectionOptions'{_vcoStaticRoutesOnly = Nothing};

-- | Indicates whether the VPN connection uses static routes only. Static
-- routes must be used for devices that don\'t support BGP.
vcoStaticRoutesOnly :: Lens' VPNConnectionOptions (Maybe Bool)
vcoStaticRoutesOnly = lens _vcoStaticRoutesOnly (\ s a -> s{_vcoStaticRoutesOnly = a});

instance FromXML VPNConnectionOptions where
        parseXML x
          = VPNConnectionOptions' <$>
              (x .@? "staticRoutesOnly")

-- | Describes VPN connection options.
--
-- /See:/ 'vpnConnectionOptionsSpecification' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vcosStaticRoutesOnly'
newtype VPNConnectionOptionsSpecification = VPNConnectionOptionsSpecification'{_vcosStaticRoutesOnly :: Maybe Bool} deriving (Eq, Read, Show)

-- | 'VPNConnectionOptionsSpecification' smart constructor.
vpnConnectionOptionsSpecification :: VPNConnectionOptionsSpecification
vpnConnectionOptionsSpecification = VPNConnectionOptionsSpecification'{_vcosStaticRoutesOnly = Nothing};

-- | Indicates whether the VPN connection uses static routes only. Static
-- routes must be used for devices that don\'t support BGP.
vcosStaticRoutesOnly :: Lens' VPNConnectionOptionsSpecification (Maybe Bool)
vcosStaticRoutesOnly = lens _vcosStaticRoutesOnly (\ s a -> s{_vcosStaticRoutesOnly = a});

instance ToQuery VPNConnectionOptionsSpecification
         where
        toQuery VPNConnectionOptionsSpecification'{..}
          = mconcat
              ["StaticRoutesOnly" =: _vcosStaticRoutesOnly]

-- | Describes a virtual private gateway.
--
-- /See:/ 'vpnGateway' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vgVPCAttachments'
--
-- * 'vgState'
--
-- * 'vgVPNGatewayId'
--
-- * 'vgAvailabilityZone'
--
-- * 'vgType'
--
-- * 'vgTags'
data VPNGateway = VPNGateway'{_vgVPCAttachments :: Maybe [VPCAttachment], _vgState :: Maybe VPNState, _vgVPNGatewayId :: Maybe Text, _vgAvailabilityZone :: Maybe Text, _vgType :: Maybe GatewayType, _vgTags :: Maybe [Tag]} deriving (Eq, Read, Show)

-- | 'VPNGateway' smart constructor.
vpnGateway :: VPNGateway
vpnGateway = VPNGateway'{_vgVPCAttachments = Nothing, _vgState = Nothing, _vgVPNGatewayId = Nothing, _vgAvailabilityZone = Nothing, _vgType = Nothing, _vgTags = Nothing};

-- | Any VPCs attached to the virtual private gateway.
vgVPCAttachments :: Lens' VPNGateway [VPCAttachment]
vgVPCAttachments = lens _vgVPCAttachments (\ s a -> s{_vgVPCAttachments = a}) . _Default;

-- | The current state of the virtual private gateway.
vgState :: Lens' VPNGateway (Maybe VPNState)
vgState = lens _vgState (\ s a -> s{_vgState = a});

-- | The ID of the virtual private gateway.
vgVPNGatewayId :: Lens' VPNGateway (Maybe Text)
vgVPNGatewayId = lens _vgVPNGatewayId (\ s a -> s{_vgVPNGatewayId = a});

-- | The Availability Zone where the virtual private gateway was created.
vgAvailabilityZone :: Lens' VPNGateway (Maybe Text)
vgAvailabilityZone = lens _vgAvailabilityZone (\ s a -> s{_vgAvailabilityZone = a});

-- | The type of VPN connection the virtual private gateway supports.
vgType :: Lens' VPNGateway (Maybe GatewayType)
vgType = lens _vgType (\ s a -> s{_vgType = a});

-- | Any tags assigned to the virtual private gateway.
vgTags :: Lens' VPNGateway [Tag]
vgTags = lens _vgTags (\ s a -> s{_vgTags = a}) . _Default;

instance FromXML VPNGateway where
        parseXML x
          = VPNGateway' <$>
              (may (parseXMLList "item") x) <*> (x .@? "state") <*>
                (x .@? "vpnGatewayId")
                <*> (x .@? "availabilityZone")
                <*> (x .@? "type")
                <*> (may (parseXMLList "item") x)

-- | Describes a static route for a VPN connection.
--
-- /See:/ 'vpnStaticRoute' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vsrState'
--
-- * 'vsrSource'
--
-- * 'vsrDestinationCIDRBlock'
data VPNStaticRoute = VPNStaticRoute'{_vsrState :: Maybe VPNState, _vsrSource :: Maybe VPNStaticRouteSource, _vsrDestinationCIDRBlock :: Maybe Text} deriving (Eq, Read, Show)

-- | 'VPNStaticRoute' smart constructor.
vpnStaticRoute :: VPNStaticRoute
vpnStaticRoute = VPNStaticRoute'{_vsrState = Nothing, _vsrSource = Nothing, _vsrDestinationCIDRBlock = Nothing};

-- | The current state of the static route.
vsrState :: Lens' VPNStaticRoute (Maybe VPNState)
vsrState = lens _vsrState (\ s a -> s{_vsrState = a});

-- | Indicates how the routes were provided.
vsrSource :: Lens' VPNStaticRoute (Maybe VPNStaticRouteSource)
vsrSource = lens _vsrSource (\ s a -> s{_vsrSource = a});

-- | The CIDR block associated with the local subnet of the customer data
-- center.
vsrDestinationCIDRBlock :: Lens' VPNStaticRoute (Maybe Text)
vsrDestinationCIDRBlock = lens _vsrDestinationCIDRBlock (\ s a -> s{_vsrDestinationCIDRBlock = a});

instance FromXML VPNStaticRoute where
        parseXML x
          = VPNStaticRoute' <$>
              (x .@? "state") <*> (x .@? "source") <*>
                (x .@? "destinationCidrBlock")

-- | Describes a volume.
--
-- /See:/ 'volume' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'volAttachments'
--
-- * 'volIOPS'
--
-- * 'volKMSKeyId'
--
-- * 'volTags'
--
-- * 'volAvailabilityZone'
--
-- * 'volCreateTime'
--
-- * 'volEncrypted'
--
-- * 'volSize'
--
-- * 'volSnapshotId'
--
-- * 'volState'
--
-- * 'volVolumeId'
--
-- * 'volVolumeType'
data Volume = Volume'{_volAttachments :: Maybe [VolumeAttachment], _volIOPS :: Maybe Int, _volKMSKeyId :: Maybe Text, _volTags :: Maybe [Tag], _volAvailabilityZone :: Text, _volCreateTime :: ISO8601, _volEncrypted :: Bool, _volSize :: Int, _volSnapshotId :: Text, _volState :: VolumeState, _volVolumeId :: Text, _volVolumeType :: VolumeType} deriving (Eq, Read, Show)

-- | 'Volume' smart constructor.
volume :: Text -> UTCTime -> Bool -> Int -> Text -> VolumeState -> Text -> VolumeType -> Volume
volume pAvailabilityZone pCreateTime pEncrypted pSize pSnapshotId pState pVolumeId pVolumeType = Volume'{_volAttachments = Nothing, _volIOPS = Nothing, _volKMSKeyId = Nothing, _volTags = Nothing, _volAvailabilityZone = pAvailabilityZone, _volCreateTime = _Time # pCreateTime, _volEncrypted = pEncrypted, _volSize = pSize, _volSnapshotId = pSnapshotId, _volState = pState, _volVolumeId = pVolumeId, _volVolumeType = pVolumeType};

-- | Information about the volume attachments.
volAttachments :: Lens' Volume [VolumeAttachment]
volAttachments = lens _volAttachments (\ s a -> s{_volAttachments = a}) . _Default;

-- | The number of I\/O operations per second (IOPS) that the volume
-- supports. For Provisioned IOPS (SSD) volumes, this represents the number
-- of IOPS that are provisioned for the volume. For General Purpose (SSD)
-- volumes, this represents the baseline performance of the volume and the
-- rate at which the volume accumulates I\/O credits for bursting. For more
-- information on General Purpose (SSD) baseline performance, I\/O credits,
-- and bursting, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- Constraint: Range is 100 to 20000 for Provisioned IOPS (SSD) volumes and
-- 3 to 10000 for General Purpose (SSD) volumes.
--
-- Condition: This parameter is required for requests to create @io1@
-- volumes; it is not used in requests to create @standard@ or @gp2@
-- volumes.
volIOPS :: Lens' Volume (Maybe Int)
volIOPS = lens _volIOPS (\ s a -> s{_volIOPS = a});

-- | The full ARN of the AWS Key Management Service (KMS) master key that was
-- used to protect the volume encryption key for the volume.
volKMSKeyId :: Lens' Volume (Maybe Text)
volKMSKeyId = lens _volKMSKeyId (\ s a -> s{_volKMSKeyId = a});

-- | Any tags assigned to the volume.
volTags :: Lens' Volume [Tag]
volTags = lens _volTags (\ s a -> s{_volTags = a}) . _Default;

-- | The Availability Zone for the volume.
volAvailabilityZone :: Lens' Volume Text
volAvailabilityZone = lens _volAvailabilityZone (\ s a -> s{_volAvailabilityZone = a});

-- | The time stamp when volume creation was initiated.
volCreateTime :: Lens' Volume UTCTime
volCreateTime = lens _volCreateTime (\ s a -> s{_volCreateTime = a}) . _Time;

-- | Indicates whether the volume will be encrypted.
volEncrypted :: Lens' Volume Bool
volEncrypted = lens _volEncrypted (\ s a -> s{_volEncrypted = a});

-- | The size of the volume, in GiBs.
volSize :: Lens' Volume Int
volSize = lens _volSize (\ s a -> s{_volSize = a});

-- | The snapshot from which the volume was created, if applicable.
volSnapshotId :: Lens' Volume Text
volSnapshotId = lens _volSnapshotId (\ s a -> s{_volSnapshotId = a});

-- | The volume state.
volState :: Lens' Volume VolumeState
volState = lens _volState (\ s a -> s{_volState = a});

-- | The ID of the volume.
volVolumeId :: Lens' Volume Text
volVolumeId = lens _volVolumeId (\ s a -> s{_volVolumeId = a});

-- | The volume type. This can be @gp2@ for General Purpose (SSD) volumes,
-- @io1@ for Provisioned IOPS (SSD) volumes, or @standard@ for Magnetic
-- volumes.
volVolumeType :: Lens' Volume VolumeType
volVolumeType = lens _volVolumeType (\ s a -> s{_volVolumeType = a});

instance FromXML Volume where
        parseXML x
          = Volume' <$>
              (may (parseXMLList "item") x) <*> (x .@? "iops") <*>
                (x .@? "kmsKeyId")
                <*> (may (parseXMLList "item") x)
                <*> (x .@ "availabilityZone")
                <*> (x .@ "createTime")
                <*> (x .@ "encrypted")
                <*> (x .@ "size")
                <*> (x .@ "snapshotId")
                <*> (x .@ "status")
                <*> (x .@ "volumeId")
                <*> (x .@ "volumeType")

-- | Describes volume attachment details.
--
-- /See:/ 'volumeAttachment' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vInstanceId'
--
-- * 'vDeleteOnTermination'
--
-- * 'vState'
--
-- * 'vDevice'
--
-- * 'vVolumeId'
--
-- * 'vAttachTime'
data VolumeAttachment = VolumeAttachment'{_vInstanceId :: Maybe Text, _vDeleteOnTermination :: Maybe Bool, _vState :: Maybe VolumeAttachmentState, _vDevice :: Maybe Text, _vVolumeId :: Maybe Text, _vAttachTime :: Maybe ISO8601} deriving (Eq, Read, Show)

-- | 'VolumeAttachment' smart constructor.
volumeAttachment :: VolumeAttachment
volumeAttachment = VolumeAttachment'{_vInstanceId = Nothing, _vDeleteOnTermination = Nothing, _vState = Nothing, _vDevice = Nothing, _vVolumeId = Nothing, _vAttachTime = Nothing};

-- | The ID of the instance.
vInstanceId :: Lens' VolumeAttachment (Maybe Text)
vInstanceId = lens _vInstanceId (\ s a -> s{_vInstanceId = a});

-- | Indicates whether the EBS volume is deleted on instance termination.
vDeleteOnTermination :: Lens' VolumeAttachment (Maybe Bool)
vDeleteOnTermination = lens _vDeleteOnTermination (\ s a -> s{_vDeleteOnTermination = a});

-- | The attachment state of the volume.
vState :: Lens' VolumeAttachment (Maybe VolumeAttachmentState)
vState = lens _vState (\ s a -> s{_vState = a});

-- | The device name.
vDevice :: Lens' VolumeAttachment (Maybe Text)
vDevice = lens _vDevice (\ s a -> s{_vDevice = a});

-- | The ID of the volume.
vVolumeId :: Lens' VolumeAttachment (Maybe Text)
vVolumeId = lens _vVolumeId (\ s a -> s{_vVolumeId = a});

-- | The time stamp when the attachment initiated.
vAttachTime :: Lens' VolumeAttachment (Maybe UTCTime)
vAttachTime = lens _vAttachTime (\ s a -> s{_vAttachTime = a}) . mapping _Time;

instance FromXML VolumeAttachment where
        parseXML x
          = VolumeAttachment' <$>
              (x .@? "instanceId") <*>
                (x .@? "deleteOnTermination")
                <*> (x .@? "status")
                <*> (x .@? "device")
                <*> (x .@? "volumeId")
                <*> (x .@? "attachTime")

-- | Describes an EBS volume.
--
-- /See:/ 'volumeDetail' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vdSize'
newtype VolumeDetail = VolumeDetail'{_vdSize :: Integer} deriving (Eq, Read, Show)

-- | 'VolumeDetail' smart constructor.
volumeDetail :: Integer -> VolumeDetail
volumeDetail pSize = VolumeDetail'{_vdSize = pSize};

-- | The size of the volume, in GiB.
vdSize :: Lens' VolumeDetail Integer
vdSize = lens _vdSize (\ s a -> s{_vdSize = a});

instance ToQuery VolumeDetail where
        toQuery VolumeDetail'{..}
          = mconcat ["Size" =: _vdSize]

-- | Describes a volume status operation code.
--
-- /See:/ 'volumeStatusAction' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vsaEventType'
--
-- * 'vsaCode'
--
-- * 'vsaDescription'
--
-- * 'vsaEventId'
data VolumeStatusAction = VolumeStatusAction'{_vsaEventType :: Maybe Text, _vsaCode :: Maybe Text, _vsaDescription :: Maybe Text, _vsaEventId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'VolumeStatusAction' smart constructor.
volumeStatusAction :: VolumeStatusAction
volumeStatusAction = VolumeStatusAction'{_vsaEventType = Nothing, _vsaCode = Nothing, _vsaDescription = Nothing, _vsaEventId = Nothing};

-- | The event type associated with this operation.
vsaEventType :: Lens' VolumeStatusAction (Maybe Text)
vsaEventType = lens _vsaEventType (\ s a -> s{_vsaEventType = a});

-- | The code identifying the operation, for example, @enable-volume-io@.
vsaCode :: Lens' VolumeStatusAction (Maybe Text)
vsaCode = lens _vsaCode (\ s a -> s{_vsaCode = a});

-- | A description of the operation.
vsaDescription :: Lens' VolumeStatusAction (Maybe Text)
vsaDescription = lens _vsaDescription (\ s a -> s{_vsaDescription = a});

-- | The ID of the event associated with this operation.
vsaEventId :: Lens' VolumeStatusAction (Maybe Text)
vsaEventId = lens _vsaEventId (\ s a -> s{_vsaEventId = a});

instance FromXML VolumeStatusAction where
        parseXML x
          = VolumeStatusAction' <$>
              (x .@? "eventType") <*> (x .@? "code") <*>
                (x .@? "description")
                <*> (x .@? "eventId")

-- | Describes a volume status.
--
-- /See:/ 'volumeStatusDetails' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vsdStatus'
--
-- * 'vsdName'
data VolumeStatusDetails = VolumeStatusDetails'{_vsdStatus :: Maybe Text, _vsdName :: Maybe VolumeStatusName} deriving (Eq, Read, Show)

-- | 'VolumeStatusDetails' smart constructor.
volumeStatusDetails :: VolumeStatusDetails
volumeStatusDetails = VolumeStatusDetails'{_vsdStatus = Nothing, _vsdName = Nothing};

-- | The intended status of the volume status.
vsdStatus :: Lens' VolumeStatusDetails (Maybe Text)
vsdStatus = lens _vsdStatus (\ s a -> s{_vsdStatus = a});

-- | The name of the volume status.
vsdName :: Lens' VolumeStatusDetails (Maybe VolumeStatusName)
vsdName = lens _vsdName (\ s a -> s{_vsdName = a});

instance FromXML VolumeStatusDetails where
        parseXML x
          = VolumeStatusDetails' <$>
              (x .@? "status") <*> (x .@? "name")

-- | Describes a volume status event.
--
-- /See:/ 'volumeStatusEvent' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vseNotBefore'
--
-- * 'vseEventType'
--
-- * 'vseDescription'
--
-- * 'vseNotAfter'
--
-- * 'vseEventId'
data VolumeStatusEvent = VolumeStatusEvent'{_vseNotBefore :: Maybe ISO8601, _vseEventType :: Maybe Text, _vseDescription :: Maybe Text, _vseNotAfter :: Maybe ISO8601, _vseEventId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'VolumeStatusEvent' smart constructor.
volumeStatusEvent :: VolumeStatusEvent
volumeStatusEvent = VolumeStatusEvent'{_vseNotBefore = Nothing, _vseEventType = Nothing, _vseDescription = Nothing, _vseNotAfter = Nothing, _vseEventId = Nothing};

-- | The earliest start time of the event.
vseNotBefore :: Lens' VolumeStatusEvent (Maybe UTCTime)
vseNotBefore = lens _vseNotBefore (\ s a -> s{_vseNotBefore = a}) . mapping _Time;

-- | The type of this event.
vseEventType :: Lens' VolumeStatusEvent (Maybe Text)
vseEventType = lens _vseEventType (\ s a -> s{_vseEventType = a});

-- | A description of the event.
vseDescription :: Lens' VolumeStatusEvent (Maybe Text)
vseDescription = lens _vseDescription (\ s a -> s{_vseDescription = a});

-- | The latest end time of the event.
vseNotAfter :: Lens' VolumeStatusEvent (Maybe UTCTime)
vseNotAfter = lens _vseNotAfter (\ s a -> s{_vseNotAfter = a}) . mapping _Time;

-- | The ID of this event.
vseEventId :: Lens' VolumeStatusEvent (Maybe Text)
vseEventId = lens _vseEventId (\ s a -> s{_vseEventId = a});

instance FromXML VolumeStatusEvent where
        parseXML x
          = VolumeStatusEvent' <$>
              (x .@? "notBefore") <*> (x .@? "eventType") <*>
                (x .@? "description")
                <*> (x .@? "notAfter")
                <*> (x .@? "eventId")

-- | Describes the status of a volume.
--
-- /See:/ 'volumeStatusInfo' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vsiStatus'
--
-- * 'vsiDetails'
data VolumeStatusInfo = VolumeStatusInfo'{_vsiStatus :: Maybe VolumeStatusInfoStatus, _vsiDetails :: Maybe [VolumeStatusDetails]} deriving (Eq, Read, Show)

-- | 'VolumeStatusInfo' smart constructor.
volumeStatusInfo :: VolumeStatusInfo
volumeStatusInfo = VolumeStatusInfo'{_vsiStatus = Nothing, _vsiDetails = Nothing};

-- | The status of the volume.
vsiStatus :: Lens' VolumeStatusInfo (Maybe VolumeStatusInfoStatus)
vsiStatus = lens _vsiStatus (\ s a -> s{_vsiStatus = a});

-- | The details of the volume status.
vsiDetails :: Lens' VolumeStatusInfo [VolumeStatusDetails]
vsiDetails = lens _vsiDetails (\ s a -> s{_vsiDetails = a}) . _Default;

instance FromXML VolumeStatusInfo where
        parseXML x
          = VolumeStatusInfo' <$>
              (x .@? "status") <*> (may (parseXMLList "item") x)

-- | Describes the volume status.
--
-- /See:/ 'volumeStatusItem' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vsiVolumeStatus'
--
-- * 'vsiActions'
--
-- * 'vsiAvailabilityZone'
--
-- * 'vsiEvents'
--
-- * 'vsiVolumeId'
data VolumeStatusItem = VolumeStatusItem'{_vsiVolumeStatus :: Maybe VolumeStatusInfo, _vsiActions :: Maybe [VolumeStatusAction], _vsiAvailabilityZone :: Maybe Text, _vsiEvents :: Maybe [VolumeStatusEvent], _vsiVolumeId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'VolumeStatusItem' smart constructor.
volumeStatusItem :: VolumeStatusItem
volumeStatusItem = VolumeStatusItem'{_vsiVolumeStatus = Nothing, _vsiActions = Nothing, _vsiAvailabilityZone = Nothing, _vsiEvents = Nothing, _vsiVolumeId = Nothing};

-- | The volume status.
vsiVolumeStatus :: Lens' VolumeStatusItem (Maybe VolumeStatusInfo)
vsiVolumeStatus = lens _vsiVolumeStatus (\ s a -> s{_vsiVolumeStatus = a});

-- | The details of the operation.
vsiActions :: Lens' VolumeStatusItem [VolumeStatusAction]
vsiActions = lens _vsiActions (\ s a -> s{_vsiActions = a}) . _Default;

-- | The Availability Zone of the volume.
vsiAvailabilityZone :: Lens' VolumeStatusItem (Maybe Text)
vsiAvailabilityZone = lens _vsiAvailabilityZone (\ s a -> s{_vsiAvailabilityZone = a});

-- | A list of events associated with the volume.
vsiEvents :: Lens' VolumeStatusItem [VolumeStatusEvent]
vsiEvents = lens _vsiEvents (\ s a -> s{_vsiEvents = a}) . _Default;

-- | The volume ID.
vsiVolumeId :: Lens' VolumeStatusItem (Maybe Text)
vsiVolumeId = lens _vsiVolumeId (\ s a -> s{_vsiVolumeId = a});

instance FromXML VolumeStatusItem where
        parseXML x
          = VolumeStatusItem' <$>
              (x .@? "volumeStatus") <*>
                (may (parseXMLList "item") x)
                <*> (x .@? "availabilityZone")
                <*> (may (parseXMLList "item") x)
                <*> (x .@? "volumeId")
