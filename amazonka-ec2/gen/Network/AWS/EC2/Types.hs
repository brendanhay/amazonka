{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types
    (
    -- * Service Configuration
      ec2

    -- * Errors

    -- * Re-exported Types
    , module Network.AWS.EC2.Internal

    -- * AccountAttributeName
    , AccountAttributeName (..)

    -- * ActivityStatus
    , ActivityStatus (..)

    -- * AddressStatus
    , AddressStatus (..)

    -- * Affinity
    , Affinity (..)

    -- * AllocationState
    , AllocationState (..)

    -- * AllocationStrategy
    , AllocationStrategy (..)

    -- * ArchitectureValues
    , ArchitectureValues (..)

    -- * AttachmentStatus
    , AttachmentStatus (..)

    -- * AutoPlacement
    , AutoPlacement (..)

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

    -- * ExcessCapacityTerminationPolicy
    , ExcessCapacityTerminationPolicy (..)

    -- * ExportEnvironment
    , ExportEnvironment (..)

    -- * ExportTaskState
    , ExportTaskState (..)

    -- * FleetType
    , FleetType (..)

    -- * FlowLogsResourceType
    , FlowLogsResourceType (..)

    -- * GatewayType
    , GatewayType (..)

    -- * HostTenancy
    , HostTenancy (..)

    -- * HypervisorType
    , HypervisorType (..)

    -- * IAMInstanceProfileAssociationState
    , IAMInstanceProfileAssociationState (..)

    -- * ImageAttributeName
    , ImageAttributeName (..)

    -- * ImageState
    , ImageState (..)

    -- * ImageTypeValues
    , ImageTypeValues (..)

    -- * InstanceAttributeName
    , InstanceAttributeName (..)

    -- * InstanceHealthStatus
    , InstanceHealthStatus (..)

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

    -- * NatGatewayState
    , NatGatewayState (..)

    -- * NetworkInterfaceAttribute
    , NetworkInterfaceAttribute (..)

    -- * NetworkInterfaceStatus
    , NetworkInterfaceStatus (..)

    -- * NetworkInterfaceType
    , NetworkInterfaceType (..)

    -- * OfferingClassType
    , OfferingClassType (..)

    -- * OfferingTypeValues
    , OfferingTypeValues (..)

    -- * OperationType
    , OperationType (..)

    -- * PaymentOption
    , PaymentOption (..)

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

    -- * ReservationState
    , ReservationState (..)

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

    -- * Scope
    , Scope (..)

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

    -- * SubnetCidrBlockStateCode
    , SubnetCidrBlockStateCode (..)

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

    -- * VPCCidrBlockStateCode
    , VPCCidrBlockStateCode (..)

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

    -- * VolumeModificationState
    , VolumeModificationState (..)

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
    , aiInstanceHealth
    , aiInstanceType
    , aiSpotInstanceRequestId

    -- * Address
    , Address
    , address
    , aAssociationId
    , aInstanceId
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
    , azState
    , azRegionName
    , azZoneName
    , azMessages

    -- * AvailabilityZoneMessage
    , AvailabilityZoneMessage
    , availabilityZoneMessage
    , azmMessage

    -- * AvailableCapacity
    , AvailableCapacity
    , availableCapacity
    , acAvailableInstanceCapacity
    , acAvailableVCPUs

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

    -- * ClassicLinkDNSSupport
    , ClassicLinkDNSSupport
    , classicLinkDNSSupport
    , cldsVPCId
    , cldsClassicLinkDNSSupported

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
    , eibdStatus
    , eibdDeleteOnTermination
    , eibdVolumeId
    , eibdAttachTime

    -- * EBSInstanceBlockDeviceSpecification
    , EBSInstanceBlockDeviceSpecification
    , ebsInstanceBlockDeviceSpecification
    , eibdsDeleteOnTermination
    , eibdsVolumeId

    -- * EgressOnlyInternetGateway
    , EgressOnlyInternetGateway
    , egressOnlyInternetGateway
    , eoigEgressOnlyInternetGatewayId
    , eoigAttachments

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
    , flLogGroupName
    , flDeliverLogsPermissionARN
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

    -- * Host
    , Host
    , host
    , hState
    , hClientToken
    , hHostId
    , hAvailableCapacity
    , hHostReservationId
    , hHostProperties
    , hAvailabilityZone
    , hInstances
    , hAutoPlacement

    -- * HostInstance
    , HostInstance
    , hostInstance
    , hiInstanceId
    , hiInstanceType

    -- * HostOffering
    , HostOffering
    , hostOffering
    , hoInstanceFamily
    , hoCurrencyCode
    , hoHourlyPrice
    , hoUpfrontPrice
    , hoOfferingId
    , hoDuration
    , hoPaymentOption

    -- * HostProperties
    , HostProperties
    , hostProperties
    , hpInstanceType
    , hpTotalVCPUs
    , hpCores
    , hpSockets

    -- * HostReservation
    , HostReservation
    , hostReservation
    , hrState
    , hrInstanceFamily
    , hrCurrencyCode
    , hrHostReservationId
    , hrStart
    , hrHourlyPrice
    , hrCount
    , hrUpfrontPrice
    , hrEnd
    , hrHostIdSet
    , hrOfferingId
    , hrDuration
    , hrPaymentOption

    -- * IAMInstanceProfile
    , IAMInstanceProfile
    , iamInstanceProfile
    , iapARN
    , iapId

    -- * IAMInstanceProfileAssociation
    , IAMInstanceProfileAssociation
    , iamInstanceProfileAssociation
    , iapaAssociationId
    , iapaInstanceId
    , iapaState
    , iapaIAMInstanceProfile
    , iapaTimestamp

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
    , ipIPv6Ranges
    , ipIPRanges
    , ipIPProtocol

    -- * IPRange
    , IPRange
    , ipRange
    , irCidrIP

    -- * IPv6CidrBlock
    , IPv6CidrBlock
    , ipv6CidrBlock
    , icbIPv6CidrBlock

    -- * IPv6Range
    , IPv6Range
    , ipv6Range
    , irCidrIPv6

    -- * IdFormat
    , IdFormat
    , idFormat
    , ifUseLongIds
    , ifDeadline
    , ifResource

    -- * Image
    , Image
    , image
    , iPlatform
    , iEnaSupport
    , iImageOwnerAlias
    , iRAMDiskId
    , iKernelId
    , iRootDeviceName
    , iSRIOVNetSupport
    , iName
    , iCreationDate
    , iProductCodes
    , iStateReason
    , iDescription
    , iBlockDeviceMappings
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
    , idcFormat
    , idcURL
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
    , iilsInstanceType
    , iilsGroupIds
    , iilsUserData
    , iilsMonitoring
    , iilsPrivateIPAddress
    , iilsInstanceInitiatedShutdownBehavior
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
    , insEnaSupport
    , insSourceDestCheck
    , insVPCId
    , insKeyName
    , insNetworkInterfaces
    , insRAMDiskId
    , insSubnetId
    , insKernelId
    , insRootDeviceName
    , insSRIOVNetSupport
    , insEBSOptimized
    , insStateTransitionReason
    , insInstanceLifecycle
    , insIAMInstanceProfile
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

    -- * InstanceCapacity
    , InstanceCapacity
    , instanceCapacity
    , icAvailableCapacity
    , icInstanceType
    , icTotalCapacity

    -- * InstanceCount
    , InstanceCount
    , instanceCount
    , icState
    , icInstanceCount

    -- * InstanceExportDetails
    , InstanceExportDetails
    , instanceExportDetails
    , iedTargetEnvironment
    , iedInstanceId

    -- * InstanceIPv6Address
    , InstanceIPv6Address
    , instanceIPv6Address
    , iiaIPv6Address

    -- * InstanceMonitoring
    , InstanceMonitoring
    , instanceMonitoring
    , imInstanceId
    , imMonitoring

    -- * InstanceNetworkInterface
    , InstanceNetworkInterface
    , instanceNetworkInterface
    , iniGroups
    , iniStatus
    , iniPrivateIPAddresses
    , iniSourceDestCheck
    , iniVPCId
    , iniNetworkInterfaceId
    , iniSubnetId
    , iniMACAddress
    , iniAttachment
    , iniOwnerId
    , iniPrivateIPAddress
    , iniPrivateDNSName
    , iniDescription
    , iniAssociation
    , iniIPv6Addresses

    -- * InstanceNetworkInterfaceAssociation
    , InstanceNetworkInterfaceAssociation
    , instanceNetworkInterfaceAssociation
    , iniaPublicDNSName
    , iniaIPOwnerId
    , iniaPublicIP

    -- * InstanceNetworkInterfaceAttachment
    , InstanceNetworkInterfaceAttachment
    , instanceNetworkInterfaceAttachment
    , iniaStatus
    , iniaDeleteOnTermination
    , iniaAttachmentId
    , iniaAttachTime
    , iniaDeviceIndex

    -- * InstanceNetworkInterfaceSpecification
    , InstanceNetworkInterfaceSpecification
    , instanceNetworkInterfaceSpecification
    , inisGroups
    , inisPrivateIPAddresses
    , inisDeleteOnTermination
    , inisAssociatePublicIPAddress
    , inisNetworkInterfaceId
    , inisSubnetId
    , inisIPv6AddressCount
    , inisPrivateIPAddress
    , inisSecondaryPrivateIPAddressCount
    , inisDescription
    , inisDeviceIndex
    , inisIPv6Addresses

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
    , isEvents
    , isAvailabilityZone
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
    , lsKeyName
    , lsNetworkInterfaces
    , lsRAMDiskId
    , lsSubnetId
    , lsKernelId
    , lsInstanceType
    , lsEBSOptimized
    , lsUserData
    , lsMonitoring
    , lsIAMInstanceProfile
    , lsImageId
    , lsAddressingType
    , lsBlockDeviceMappings
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

    -- * NatGateway
    , NatGateway
    , natGateway
    , ngState
    , ngFailureCode
    , ngVPCId
    , ngFailureMessage
    , ngNatGatewayId
    , ngSubnetId
    , ngDeleteTime
    , ngProvisionedBandwidth
    , ngNatGatewayAddresses
    , ngCreateTime

    -- * NatGatewayAddress
    , NatGatewayAddress
    , natGatewayAddress
    , ngaPrivateIP
    , ngaAllocationId
    , ngaNetworkInterfaceId
    , ngaPublicIP

    -- * NetworkACL
    , NetworkACL
    , networkACL
    , naEntries
    , naNetworkACLId
    , naVPCId
    , naAssociations
    , naTags
    , naIsDefault

    -- * NetworkACLAssociation
    , NetworkACLAssociation
    , networkACLAssociation
    , naaNetworkACLId
    , naaSubnetId
    , naaNetworkACLAssociationId

    -- * NetworkACLEntry
    , NetworkACLEntry
    , networkACLEntry
    , naeIPv6CidrBlock
    , naeICMPTypeCode
    , naeRuleNumber
    , naeRuleAction
    , naeProtocol
    , naePortRange
    , naeCidrBlock
    , naeEgress

    -- * NetworkInterface
    , NetworkInterface
    , networkInterface
    , niGroups
    , niStatus
    , niPrivateIPAddresses
    , niSourceDestCheck
    , niInterfaceType
    , niVPCId
    , niTagSet
    , niRequesterManaged
    , niNetworkInterfaceId
    , niSubnetId
    , niMACAddress
    , niAttachment
    , niOwnerId
    , niAvailabilityZone
    , niPrivateIPAddress
    , niPrivateDNSName
    , niRequesterId
    , niDescription
    , niAssociation
    , niIPv6Addresses

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
    , niaStatus
    , niaDeleteOnTermination
    , niaAttachmentId
    , niaInstanceOwnerId
    , niaAttachTime
    , niaDeviceIndex

    -- * NetworkInterfaceAttachmentChanges
    , NetworkInterfaceAttachmentChanges
    , networkInterfaceAttachmentChanges
    , niacDeleteOnTermination
    , niacAttachmentId

    -- * NetworkInterfaceIPv6Address
    , NetworkInterfaceIPv6Address
    , networkInterfaceIPv6Address
    , niiaIPv6Address

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

    -- * PeeringConnectionOptions
    , PeeringConnectionOptions
    , peeringConnectionOptions
    , pcoAllowEgressFromLocalVPCToRemoteClassicLink
    , pcoAllowEgressFromLocalClassicLinkToRemoteVPC
    , pcoAllowDNSResolutionFromRemoteVPC

    -- * PeeringConnectionOptionsRequest
    , PeeringConnectionOptionsRequest
    , peeringConnectionOptionsRequest
    , pcorAllowEgressFromLocalVPCToRemoteClassicLink
    , pcorAllowEgressFromLocalClassicLinkToRemoteVPC
    , pcorAllowDNSResolutionFromRemoteVPC

    -- * Placement
    , Placement
    , placement
    , pAffinity
    , pHostId
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
    , plCidrs
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

    -- * ProvisionedBandwidth
    , ProvisionedBandwidth
    , provisionedBandwidth
    , pbStatus
    , pbRequested
    , pbProvisioned
    , pbRequestTime
    , pbProvisionTime

    -- * Purchase
    , Purchase
    , purchase
    , pInstanceFamily
    , pCurrencyCode
    , pHostReservationId
    , pHourlyPrice
    , pUpfrontPrice
    , pHostIdSet
    , pDuration
    , pPaymentOption

    -- * PurchaseRequest
    , PurchaseRequest
    , purchaseRequest
    , prPurchaseToken
    , prInstanceCount

    -- * RecurringCharge
    , RecurringCharge
    , recurringCharge
    , rcAmount
    , rcFrequency

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
    , rslsKeyName
    , rslsNetworkInterfaces
    , rslsRAMDiskId
    , rslsSubnetId
    , rslsKernelId
    , rslsInstanceType
    , rslsEBSOptimized
    , rslsUserData
    , rslsMonitoring
    , rslsIAMInstanceProfile
    , rslsImageId
    , rslsAddressingType
    , rslsBlockDeviceMappings
    , rslsPlacement

    -- * Reservation
    , Reservation
    , reservation
    , rGroups
    , rInstances
    , rRequesterId
    , rReservationId
    , rOwnerId

    -- * ReservationValue
    , ReservationValue
    , reservationValue
    , rvHourlyPrice
    , rvRemainingTotalValue
    , rvRemainingUpfrontValue

    -- * ReservedInstanceLimitPrice
    , ReservedInstanceLimitPrice
    , reservedInstanceLimitPrice
    , rilpAmount
    , rilpCurrencyCode

    -- * ReservedInstanceReservationValue
    , ReservedInstanceReservationValue
    , reservedInstanceReservationValue
    , rirvReservationValue
    , rirvReservedInstanceId

    -- * ReservedInstances
    , ReservedInstances
    , reservedInstances
    , riState
    , riCurrencyCode
    , riInstanceCount
    , riProductDescription
    , riStart
    , riInstanceType
    , riEnd
    , riAvailabilityZone
    , riScope
    , riRecurringCharges
    , riOfferingType
    , riUsagePrice
    , riFixedPrice
    , riReservedInstancesId
    , riInstanceTenancy
    , riOfferingClass
    , riDuration
    , riTags

    -- * ReservedInstancesConfiguration
    , ReservedInstancesConfiguration
    , reservedInstancesConfiguration
    , ricPlatform
    , ricInstanceCount
    , ricInstanceType
    , ricAvailabilityZone
    , ricScope

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
    , rilTags
    , rilInstanceCounts
    , rilReservedInstancesListingId

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
    , rioScope
    , rioRecurringCharges
    , rioOfferingType
    , rioUsagePrice
    , rioFixedPrice
    , rioInstanceTenancy
    , rioReservedInstancesOfferingId
    , rioOfferingClass
    , rioDuration

    -- * Route
    , Route
    , route
    , rVPCPeeringConnectionId
    , rInstanceId
    , rOrigin
    , rState
    , rEgressOnlyInternetGatewayId
    , rDestinationIPv6CidrBlock
    , rNatGatewayId
    , rNetworkInterfaceId
    , rGatewayId
    , rInstanceOwnerId
    , rDestinationPrefixListId
    , rDestinationCidrBlock

    -- * RouteTable
    , RouteTable
    , routeTable
    , rtRouteTableId
    , rtRoutes
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

    -- * ScheduledInstance
    , ScheduledInstance
    , scheduledInstance
    , siPreviousSlotEndTime
    , siPlatform
    , siTermStartDate
    , siInstanceCount
    , siScheduledInstanceId
    , siHourlyPrice
    , siCreateDate
    , siSlotDurationInHours
    , siTotalScheduledInstanceHours
    , siInstanceType
    , siRecurrence
    , siAvailabilityZone
    , siTermEndDate
    , siNextSlotStartTime
    , siNetworkPlatform

    -- * ScheduledInstanceAvailability
    , ScheduledInstanceAvailability
    , scheduledInstanceAvailability
    , siaMaxTermDurationInDays
    , siaPlatform
    , siaPurchaseToken
    , siaHourlyPrice
    , siaAvailableInstanceCount
    , siaSlotDurationInHours
    , siaTotalScheduledInstanceHours
    , siaInstanceType
    , siaRecurrence
    , siaAvailabilityZone
    , siaMinTermDurationInDays
    , siaFirstSlotStartTime
    , siaNetworkPlatform

    -- * ScheduledInstanceRecurrence
    , ScheduledInstanceRecurrence
    , scheduledInstanceRecurrence
    , sirFrequency
    , sirOccurrenceRelativeToEnd
    , sirOccurrenceUnit
    , sirInterval
    , sirOccurrenceDaySet

    -- * ScheduledInstanceRecurrenceRequest
    , ScheduledInstanceRecurrenceRequest
    , scheduledInstanceRecurrenceRequest
    , sirrFrequency
    , sirrOccurrenceRelativeToEnd
    , sirrOccurrenceDays
    , sirrOccurrenceUnit
    , sirrInterval

    -- * ScheduledInstancesBlockDeviceMapping
    , ScheduledInstancesBlockDeviceMapping
    , scheduledInstancesBlockDeviceMapping
    , sibdmVirtualName
    , sibdmNoDevice
    , sibdmEBS
    , sibdmDeviceName

    -- * ScheduledInstancesEBS
    , ScheduledInstancesEBS
    , scheduledInstancesEBS
    , sieDeleteOnTermination
    , sieVolumeSize
    , sieIOPS
    , sieEncrypted
    , sieVolumeType
    , sieSnapshotId

    -- * ScheduledInstancesIAMInstanceProfile
    , ScheduledInstancesIAMInstanceProfile
    , scheduledInstancesIAMInstanceProfile
    , siiapARN
    , siiapName

    -- * ScheduledInstancesIPv6Address
    , ScheduledInstancesIPv6Address
    , scheduledInstancesIPv6Address
    , siiaIPv6Address

    -- * ScheduledInstancesLaunchSpecification
    , ScheduledInstancesLaunchSpecification
    , scheduledInstancesLaunchSpecification
    , silsSecurityGroupIds
    , silsKeyName
    , silsNetworkInterfaces
    , silsRAMDiskId
    , silsSubnetId
    , silsKernelId
    , silsInstanceType
    , silsEBSOptimized
    , silsUserData
    , silsMonitoring
    , silsIAMInstanceProfile
    , silsBlockDeviceMappings
    , silsPlacement
    , silsImageId

    -- * ScheduledInstancesMonitoring
    , ScheduledInstancesMonitoring
    , scheduledInstancesMonitoring
    , simEnabled

    -- * ScheduledInstancesNetworkInterface
    , ScheduledInstancesNetworkInterface
    , scheduledInstancesNetworkInterface
    , siniGroups
    , siniDeleteOnTermination
    , siniAssociatePublicIPAddress
    , siniPrivateIPAddressConfigs
    , siniNetworkInterfaceId
    , siniSubnetId
    , siniIPv6AddressCount
    , siniPrivateIPAddress
    , siniSecondaryPrivateIPAddressCount
    , siniDescription
    , siniDeviceIndex
    , siniIPv6Addresses

    -- * ScheduledInstancesPlacement
    , ScheduledInstancesPlacement
    , scheduledInstancesPlacement
    , sipAvailabilityZone
    , sipGroupName

    -- * ScheduledInstancesPrivateIPAddressConfig
    , ScheduledInstancesPrivateIPAddressConfig
    , scheduledInstancesPrivateIPAddressConfig
    , sipiacPrimary
    , sipiacPrivateIPAddress

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

    -- * SecurityGroupReference
    , SecurityGroupReference
    , securityGroupReference
    , sgrVPCPeeringConnectionId
    , sgrGroupId
    , sgrReferencingVPCId

    -- * SlotDateTimeRangeRequest
    , SlotDateTimeRangeRequest
    , slotDateTimeRangeRequest
    , sdtrrEarliestTime
    , sdtrrLatestTime

    -- * SlotStartTimeRangeRequest
    , SlotStartTimeRangeRequest
    , slotStartTimeRangeRequest
    , sstrrLatestTime
    , sstrrEarliestTime

    -- * Snapshot
    , Snapshot
    , snapshot
    , sStateMessage
    , sOwnerAlias
    , sDataEncryptionKeyId
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
    , sdFormat
    , sdURL
    , sdDeviceName
    , sdStatusMessage
    , sdUserBucket
    , sdDiskImageSize
    , sdDescription
    , sdSnapshotId

    -- * SnapshotDiskContainer
    , SnapshotDiskContainer
    , snapshotDiskContainer
    , sdcFormat
    , sdcURL
    , sdcUserBucket
    , sdcDescription

    -- * SnapshotTaskDetail
    , SnapshotTaskDetail
    , snapshotTaskDetail
    , stdStatus
    , stdProgress
    , stdFormat
    , stdURL
    , stdStatusMessage
    , stdUserBucket
    , stdDiskImageSize
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

    -- * SpotFleetLaunchSpecification
    , SpotFleetLaunchSpecification
    , spotFleetLaunchSpecification
    , sflsSecurityGroups
    , sflsSpotPrice
    , sflsWeightedCapacity
    , sflsKeyName
    , sflsNetworkInterfaces
    , sflsRAMDiskId
    , sflsSubnetId
    , sflsKernelId
    , sflsInstanceType
    , sflsEBSOptimized
    , sflsUserData
    , sflsMonitoring
    , sflsIAMInstanceProfile
    , sflsImageId
    , sflsAddressingType
    , sflsBlockDeviceMappings
    , sflsPlacement

    -- * SpotFleetMonitoring
    , SpotFleetMonitoring
    , spotFleetMonitoring
    , sfmEnabled

    -- * SpotFleetRequestConfig
    , SpotFleetRequestConfig
    , spotFleetRequestConfig
    , sfrcActivityStatus
    , sfrcSpotFleetRequestId
    , sfrcSpotFleetRequestState
    , sfrcSpotFleetRequestConfig
    , sfrcCreateTime

    -- * SpotFleetRequestConfigData
    , SpotFleetRequestConfigData
    , spotFleetRequestConfigData
    , sfrcdClientToken
    , sfrcdExcessCapacityTerminationPolicy
    , sfrcdValidUntil
    , sfrcdTerminateInstancesWithExpiration
    , sfrcdFulfilledCapacity
    , sfrcdType
    , sfrcdValidFrom
    , sfrcdReplaceUnhealthyInstances
    , sfrcdAllocationStrategy
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
    , sirActualBlockHourlyPrice
    , sirBlockDurationMinutes
    , sirProductDescription
    , sirSpotPrice
    , sirLaunchSpecification
    , sirAvailabilityZoneGroup
    , sirLaunchedAvailabilityZone
    , sirValidUntil
    , sirLaunchGroup
    , sirFault
    , sirSpotInstanceRequestId
    , sirType
    , sirValidFrom
    , sirCreateTime
    , sirTags

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
    , spTenancy
    , spGroupName

    -- * SpotPrice
    , SpotPrice
    , spotPrice
    , sProductDescription
    , sSpotPrice
    , sInstanceType
    , sAvailabilityZone
    , sTimestamp

    -- * StaleIPPermission
    , StaleIPPermission
    , staleIPPermission
    , sipFromPort
    , sipUserIdGroupPairs
    , sipPrefixListIds
    , sipIPProtocol
    , sipToPort
    , sipIPRanges

    -- * StaleSecurityGroup
    , StaleSecurityGroup
    , staleSecurityGroup
    , ssgVPCId
    , ssgGroupName
    , ssgStaleIPPermissionsEgress
    , ssgStaleIPPermissions
    , ssgDescription
    , ssgGroupId

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
    , subIPv6CidrBlockAssociationSet
    , subAssignIPv6AddressOnCreation
    , subMapPublicIPOnLaunch
    , subDefaultForAz
    , subTags
    , subAvailabilityZone
    , subAvailableIPAddressCount
    , subCidrBlock
    , subState
    , subSubnetId
    , subVPCId

    -- * SubnetCidrBlockState
    , SubnetCidrBlockState
    , subnetCidrBlockState
    , scbsState
    , scbsStatusMessage

    -- * SubnetIPv6CidrBlockAssociation
    , SubnetIPv6CidrBlockAssociation
    , subnetIPv6CidrBlockAssociation
    , sicbaAssociationId
    , sicbaIPv6CidrBlock
    , sicbaIPv6CidrBlockState

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

    -- * TargetConfiguration
    , TargetConfiguration
    , targetConfiguration
    , tcInstanceCount
    , tcOfferingId

    -- * TargetConfigurationRequest
    , TargetConfigurationRequest
    , targetConfigurationRequest
    , tcrInstanceCount
    , tcrOfferingId

    -- * TargetReservationValue
    , TargetReservationValue
    , targetReservationValue
    , trvReservationValue
    , trvTargetConfiguration

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
    , uigpVPCPeeringConnectionId
    , uigpVPCId
    , uigpUserId
    , uigpGroupId
    , uigpGroupName
    , uigpPeeringStatus

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
    , vpcIPv6CidrBlockAssociationSet
    , vpcTags
    , vpcIsDefault
    , vpcCidrBlock
    , vpcDHCPOptionsId
    , vpcInstanceTenancy
    , vpcState
    , vpcVPCId

    -- * VPCAttachment
    , VPCAttachment
    , vpcAttachment
    , vaState
    , vaVPCId

    -- * VPCCidrBlockState
    , VPCCidrBlockState
    , vpcCidrBlockState
    , vcbsState
    , vcbsStatusMessage

    -- * VPCClassicLink
    , VPCClassicLink
    , vpcClassicLink
    , vclVPCId
    , vclTags
    , vclClassicLinkEnabled

    -- * VPCEndpoint
    , VPCEndpoint
    , vpcEndpoint
    , veState
    , vePolicyDocument
    , veVPCId
    , veCreationTimestamp
    , veServiceName
    , veVPCEndpointId
    , veRouteTableIds

    -- * VPCIPv6CidrBlockAssociation
    , VPCIPv6CidrBlockAssociation
    , vpcIPv6CidrBlockAssociation
    , vicbaAssociationId
    , vicbaIPv6CidrBlock
    , vicbaIPv6CidrBlockState

    -- * VPCPeeringConnection
    , VPCPeeringConnection
    , vpcPeeringConnection
    , vpcpcVPCPeeringConnectionId
    , vpcpcStatus
    , vpcpcAccepterVPCInfo
    , vpcpcRequesterVPCInfo
    , vpcpcExpirationTime
    , vpcpcTags

    -- * VPCPeeringConnectionOptionsDescription
    , VPCPeeringConnectionOptionsDescription
    , vpcPeeringConnectionOptionsDescription
    , vpcodAllowEgressFromLocalVPCToRemoteClassicLink
    , vpcodAllowEgressFromLocalClassicLinkToRemoteVPC
    , vpcodAllowDNSResolutionFromRemoteVPC

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
    , vpcviPeeringOptions
    , vpcviCidrBlock
    , vpcviIPv6CidrBlockSet

    -- * VPNConnection
    , VPNConnection
    , vpnConnection
    , vcCustomerGatewayConfiguration
    , vcRoutes
    , vcVPNGatewayId
    , vcOptions
    , vcTags
    , vcVGWTelemetry
    , vcVPNConnectionId
    , vcCustomerGatewayId
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
    , vgState
    , vgVPCAttachments
    , vgVPNGatewayId
    , vgAvailabilityZone
    , vgType
    , vgTags

    -- * VPNStaticRoute
    , VPNStaticRoute
    , vpnStaticRoute
    , vsrState
    , vsrSource
    , vsrDestinationCidrBlock

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

    -- * VolumeModification
    , VolumeModification
    , volumeModification
    , vmProgress
    , vmStartTime
    , vmModificationState
    , vmTargetVolumeType
    , vmOriginalVolumeType
    , vmTargetSize
    , vmTargetIOPS
    , vmOriginalSize
    , vmOriginalIOPS
    , vmStatusMessage
    , vmEndTime
    , vmVolumeId

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
    , vsiEvents
    , vsiAvailabilityZone
    , vsiVolumeId
    ) where

import           Network.AWS.EC2.Internal
import           Network.AWS.EC2.Types.Product
import           Network.AWS.EC2.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | API version @2016-11-15@ of the Amazon Elastic Compute Cloud SDK configuration.
ec2 :: Service
ec2 =
    Service
    { _svcAbbrev = "EC2"
    , _svcSigner = v4
    , _svcPrefix = "ec2"
    , _svcVersion = "2016-11-15"
    , _svcEndpoint = defaultEndpoint ec2
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseXMLError "EC2"
    , _svcRetry = retry
    }
  where
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
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
          Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
