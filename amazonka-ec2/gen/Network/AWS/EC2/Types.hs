{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
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

    -- * ModifySnapshotAttributeName
    , ModifySnapshotAttributeName (..)

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

    -- * VPCPeeringConnectionStateReasonCode
    , VPCPeeringConnectionStateReasonCode (..)

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
    , aInstanceId
    , aAssociationId
    , aNetworkInterfaceOwnerId
    , aAllocationId
    , aDomain
    , aNetworkInterfaceId
    , aPrivateIPAddress
    , aPublicIP

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
    , dChecksum
    , dFormat
    , dSize
    , dImportManifestURL

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
    , fValues
    , fName

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
    , irCIdRIP

    -- * Image
    , Image
    , image
    , iPlatform
    , iImageOwnerAlias
    , iRAMDiskId
    , iKernelId
    , iRootDeviceName
    , iSRIOVNetSupport
    , iName
    , iCreationDate
    , iProductCodes
    , iStateReason
    , iBlockDeviceMappings
    , iDescription
    , iTags
    , iImageId
    , iImageLocation
    , iState
    , iOwnerId
    , iPublic
    , iArchitecture
    , iImageType
    , iRootDeviceType
    , iVirtualizationType
    , iHypervisor

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
    , insEBSOptimized
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
    , mState

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
    , naeCIdRBlock
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
    , pAvailabilityZone
    , pTenancy
    , pGroupName

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
    , plCIdRs
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
    , rGroups
    , rInstances
    , rRequesterId
    , rReservationId
    , rOwnerId

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
    , rInstanceId
    , rOrigin
    , rVPCPeeringConnectionId
    , rState
    , rNetworkInterfaceId
    , rGatewayId
    , rInstanceOwnerId
    , rDestinationPrefixListId
    , rDestinationCIdRBlock

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
    , sOwnerAlias
    , sKMSKeyId
    , sTags
    , sSnapshotId
    , sOwnerId
    , sVolumeId
    , sVolumeSize
    , sDescription
    , sStartTime
    , sProgress
    , sState
    , sEncrypted

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
    , sProductDescription
    , sSpotPrice
    , sInstanceType
    , sAvailabilityZone
    , sTimestamp

    -- * StateReason
    , StateReason
    , stateReason
    , srCode
    , srMessage

    -- * Storage
    , Storage
    , storage
    , sS3

    -- * Subnet
    , Subnet
    , subnet
    , subTags
    , subAvailabilityZone
    , subAvailableIPAddressCount
    , subCIdRBlock
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
    , vpcCIdRBlock
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
    , vpcpcStatus
    , vpcpcVPCPeeringConnectionId
    , vpcpcAccepterVPCInfo
    , vpcpcRequesterVPCInfo
    , vpcpcExpirationTime
    , vpcpcTags

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
    , vpcviCIdRBlock

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
    , vsrDestinationCIdRBlock

    -- * Volume
    , Volume
    , volume
    , vAttachments
    , vIOPS
    , vKMSKeyId
    , vTags
    , vAvailabilityZone
    , vCreateTime
    , vEncrypted
    , vSize
    , vSnapshotId
    , vState
    , vVolumeId
    , vVolumeType

    -- * VolumeAttachment
    , VolumeAttachment
    , volumeAttachment
    , volInstanceId
    , volDeleteOnTermination
    , volState
    , volDevice
    , volVolumeId
    , volAttachTime

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
    ) where

import           Network.AWS.EC2.Types.Product
import           Network.AWS.EC2.Types.Sum
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | Version @2015-04-15@ of the Amazon Elastic Compute Cloud SDK.
data EC2

instance AWSService EC2 where
    type Sg EC2 = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "EC2"
            , _svcPrefix = "ec2"
            , _svcVersion = "2015-04-15"
            , _svcEndpoint = defaultEndpoint svc
            , _svcPreflight = id
            , _svcTimeout = Just 70000000
            , _svcStatus = statusSuccess
            , _svcError = parseXMLError
            , _svcRetry = retry
            }
        retry =
            Exponential
            { _retryBase = 5.0e-2
            , _retryGrowth = 2
            , _retryAttempts = 5
            , _retryCheck = check
            }
        check e
          | has (hasCode "RequestLimitExceeded" . hasStatus 503) e =
              Just "request_limit_exceeded"
          | has (hasCode "ThrottlingException" . hasStatus 400) e =
              Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing
