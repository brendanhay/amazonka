{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
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

module Network.AWS.EC2.Types
    (
    -- * Service
      EC2
    -- ** Error
    , RESTError

    -- * ImageAttributeName
    , ImageAttributeName (..)

    -- * PermissionGroup
    , PermissionGroup (..)

    -- * NetworkAclEntry
    , NetworkAclEntry
    , networkAclEntry
    , naeCidrBlock
    , naeEgress
    , naeIcmpTypeCode
    , naePortRange
    , naeProtocol
    , naeRuleAction
    , naeRuleNumber

    -- * BlobAttributeValue
    , BlobAttributeValue
    , blobAttributeValue
    , bavValue

    -- * ImportInstanceLaunchSpecification
    , ImportInstanceLaunchSpecification
    , importInstanceLaunchSpecification
    , iilsAdditionalInfo
    , iilsArchitecture
    , iilsGroupIds
    , iilsGroupNames
    , iilsInstanceInitiatedShutdownBehavior
    , iilsInstanceType
    , iilsMonitoring
    , iilsPlacement
    , iilsPrivateIpAddress
    , iilsSubnetId
    , iilsUserData

    -- * Snapshot
    , Snapshot
    , snapshot
    , sDescription
    , sEncrypted
    , sOwnerAlias
    , sOwnerId
    , sProgress
    , sSnapshotId
    , sStartTime
    , sState
    , sTags
    , sVolumeId
    , sVolumeSize

    -- * SpotInstanceStateFault
    , SpotInstanceStateFault
    , spotInstanceStateFault
    , sisfCode
    , sisfMessage

    -- * TagDescription
    , TagDescription
    , tagDescription
    , tdKey
    , tdResourceId
    , tdResourceType
    , tdValue

    -- * GroupIdentifier
    , GroupIdentifier
    , groupIdentifier
    , giGroupId
    , giGroupName

    -- * VpnStaticRouteSource
    , VpnStaticRouteSource (..)

    -- * ReservedInstancesListing
    , ReservedInstancesListing
    , reservedInstancesListing
    , rilClientToken
    , rilCreateDate
    , rilInstanceCounts
    , rilPriceSchedules
    , rilReservedInstancesId
    , rilReservedInstancesListingId
    , rilStatus
    , rilStatusMessage
    , rilTags
    , rilUpdateDate

    -- * InstanceLifecycleType
    , InstanceLifecycleType (..)

    -- * VirtualizationType
    , VirtualizationType (..)

    -- * NetworkInterfaceStatus
    , NetworkInterfaceStatus (..)

    -- * PlatformValues
    , PlatformValues (..)

    -- * CreateVolumePermission
    , CreateVolumePermission
    , createVolumePermission
    , cvpGroup
    , cvpUserId

    -- * NetworkInterfaceAttachmentChanges
    , NetworkInterfaceAttachmentChanges
    , networkInterfaceAttachmentChanges
    , niacAttachmentId
    , niacDeleteOnTermination

    -- * RecurringChargeFrequency
    , RecurringChargeFrequency (..)

    -- * DhcpOptions
    , DhcpOptions
    , dhcpOptions
    , doDhcpConfigurations
    , doDhcpOptionsId
    , doTags

    -- * InstanceNetworkInterfaceSpecification
    , InstanceNetworkInterfaceSpecification
    , instanceNetworkInterfaceSpecification
    , inisAssociatePublicIpAddress
    , inisDeleteOnTermination
    , inisDescription
    , inisDeviceIndex
    , inisGroups
    , inisNetworkInterfaceId
    , inisPrivateIpAddress
    , inisPrivateIpAddresses
    , inisSecondaryPrivateIpAddressCount
    , inisSubnetId

    -- * VolumeState
    , VolumeState (..)

    -- * AttributeValue
    , AttributeValue
    , attributeValue
    , avValue

    -- * PrivateIpAddressSpecification
    , PrivateIpAddressSpecification
    , privateIpAddressSpecification
    , piasPrimary
    , piasPrivateIpAddress

    -- * Image
    , Image
    , image
    , iArchitecture
    , iBlockDeviceMappings
    , iDescription
    , iHypervisor
    , iImageId
    , iImageLocation
    , iImageOwnerAlias
    , iImageType
    , iKernelId
    , iName
    , iOwnerId
    , iPlatform
    , iProductCodes
    , iPublic
    , iRamdiskId
    , iRootDeviceName
    , iRootDeviceType
    , iSriovNetSupport
    , iState
    , iStateReason
    , iTags
    , iVirtualizationType

    -- * DhcpConfiguration
    , DhcpConfiguration
    , dhcpConfiguration
    , dcKey
    , dcValues

    -- * Tag
    , Tag
    , tag
    , tagKey
    , tagValue

    -- * AccountAttributeName
    , AccountAttributeName (..)

    -- * NetworkInterfaceAttachment
    , NetworkInterfaceAttachment
    , networkInterfaceAttachment
    , niaAttachTime
    , niaAttachmentId
    , niaDeleteOnTermination
    , niaDeviceIndex
    , niaInstanceId
    , niaInstanceOwnerId
    , niaStatus

    -- * RunInstancesMonitoringEnabled
    , RunInstancesMonitoringEnabled
    , runInstancesMonitoringEnabled
    , rimeEnabled

    -- * VolumeStatusInfo
    , VolumeStatusInfo
    , volumeStatusInfo
    , vsiDetails
    , vsiStatus

    -- * NetworkInterfaceAssociation
    , NetworkInterfaceAssociation
    , networkInterfaceAssociation
    , niaAllocationId
    , niaAssociationId
    , niaIpOwnerId
    , niaPublicDnsName
    , niaPublicIp

    -- * CreateVolumePermissionModifications
    , CreateVolumePermissionModifications
    , createVolumePermissionModifications
    , cvpmAdd
    , cvpmRemove

    -- * VpcState
    , VpcState (..)

    -- * ResourceType
    , ResourceType (..)

    -- * ReportStatusType
    , ReportStatusType (..)

    -- * CurrencyCodeValues
    , CurrencyCodeValues (..)

    -- * IcmpTypeCode
    , IcmpTypeCode
    , icmpTypeCode
    , itcCode
    , itcType

    -- * InstanceCount
    , InstanceCount
    , instanceCount
    , icInstanceCount
    , icState

    -- * ExportToS3Task
    , ExportToS3Task
    , exportToS3Task
    , etstContainerFormat
    , etstDiskImageFormat
    , etstS3Bucket
    , etstS3Key

    -- * BlockDeviceMapping
    , BlockDeviceMapping
    , blockDeviceMapping
    , bdmDeviceName
    , bdmEbs
    , bdmNoDevice
    , bdmVirtualName

    -- * ConversionTask
    , ConversionTask
    , conversionTask
    , ctConversionTaskId
    , ctExpirationTime
    , ctImportInstance
    , ctImportVolume
    , ctState
    , ctStatusMessage
    , ctTags

    -- * AttachmentStatus
    , AttachmentStatus (..)

    -- * RouteOrigin
    , RouteOrigin (..)

    -- * ListingState
    , ListingState (..)

    -- * SpotPrice
    , SpotPrice
    , spotPrice
    , spAvailabilityZone
    , spInstanceType
    , spProductDescription
    , spSpotPrice
    , spTimestamp

    -- * InstanceMonitoring
    , InstanceMonitoring
    , instanceMonitoring
    , imInstanceId
    , imMonitoring

    -- * PriceScheduleSpecification
    , PriceScheduleSpecification
    , priceScheduleSpecification
    , pssCurrencyCode
    , pssPrice
    , pssTerm

    -- * SpotInstanceStatus
    , SpotInstanceStatus
    , spotInstanceStatus
    , sisCode
    , sisMessage
    , sisUpdateTime

    -- * AvailabilityZoneState
    , AvailabilityZoneState (..)

    -- * SpotInstanceRequest
    , SpotInstanceRequest
    , spotInstanceRequest
    , siAvailabilityZoneGroup
    , siCreateTime
    , siFault
    , siInstanceId
    , siLaunchGroup
    , siLaunchSpecification
    , siLaunchedAvailabilityZone
    , siProductDescription
    , siSpotInstanceRequestId
    , siSpotPrice
    , siState
    , siStatus
    , siTags
    , siType
    , siValidFrom
    , siValidUntil

    -- * LaunchSpecification
    , LaunchSpecification
    , launchSpecification
    , lsAddressingType
    , lsBlockDeviceMappings
    , lsEbsOptimized
    , lsIamInstanceProfile
    , lsImageId
    , lsInstanceType
    , lsKernelId
    , lsKeyName
    , lsMonitoring
    , lsNetworkInterfaces
    , lsPlacement
    , lsRamdiskId
    , lsSecurityGroups
    , lsSubnetId
    , lsUserData

    -- * VolumeStatusEvent
    , VolumeStatusEvent
    , volumeStatusEvent
    , vseDescription
    , vseEventId
    , vseEventType
    , vseNotAfter
    , vseNotBefore

    -- * Volume
    , Volume
    , volume
    , vAttachments
    , vAvailabilityZone
    , vCreateTime
    , vEncrypted
    , vIops
    , vSize
    , vSnapshotId
    , vState
    , vTags
    , vVolumeId
    , vVolumeType

    -- * Reservation
    , Reservation
    , reservation
    , rGroups
    , rInstances
    , rOwnerId
    , rRequesterId
    , rReservationId

    -- * ImportInstanceVolumeDetailItem
    , ImportInstanceVolumeDetailItem
    , importInstanceVolumeDetailItem
    , iivdiAvailabilityZone
    , iivdiBytesConverted
    , iivdiDescription
    , iivdiImage
    , iivdiStatus
    , iivdiStatusMessage
    , iivdiVolume

    -- * SummaryStatus
    , SummaryStatus (..)

    -- * ReservedInstancesModification
    , ReservedInstancesModification
    , reservedInstancesModification
    , rimClientToken
    , rimCreateDate
    , rimEffectiveDate
    , rimModificationResults
    , rimReservedInstancesIds
    , rimReservedInstancesModificationId
    , rimStatus
    , rimStatusMessage
    , rimUpdateDate

    -- * RuleAction
    , RuleAction (..)

    -- * NetworkInterface
    , NetworkInterface
    , networkInterface
    , niAssociation
    , niAttachment
    , niAvailabilityZone
    , niDescription
    , niGroups
    , niMacAddress
    , niNetworkInterfaceId
    , niOwnerId
    , niPrivateDnsName
    , niPrivateIpAddress
    , niPrivateIpAddresses
    , niRequesterId
    , niRequesterManaged
    , niSourceDestCheck
    , niStatus
    , niSubnetId
    , niTagSet
    , niVpcId

    -- * TelemetryStatus
    , TelemetryStatus (..)

    -- * Subnet
    , Subnet
    , subnet
    , s1AvailabilityZone
    , s1AvailableIpAddressCount
    , s1CidrBlock
    , s1DefaultForAz
    , s1MapPublicIpOnLaunch
    , s1State
    , s1SubnetId
    , s1Tags
    , s1VpcId

    -- * KeyPairInfo
    , KeyPairInfo
    , keyPairInfo
    , kpiKeyFingerprint
    , kpiKeyName

    -- * LaunchPermissionModifications
    , LaunchPermissionModifications
    , launchPermissionModifications
    , lpmAdd
    , lpmRemove

    -- * SnapshotState
    , SnapshotState (..)

    -- * InstanceNetworkInterfaceAssociation
    , InstanceNetworkInterfaceAssociation
    , instanceNetworkInterfaceAssociation
    , iniaIpOwnerId
    , iniaPublicDnsName
    , iniaPublicIp

    -- * DiskImageDetail
    , DiskImageDetail
    , diskImageDetail
    , didBytes
    , didFormat
    , didImportManifestUrl

    -- * InstancePrivateIpAddress
    , InstancePrivateIpAddress
    , instancePrivateIpAddress
    , ipiaAssociation
    , ipiaPrimary
    , ipiaPrivateDnsName
    , ipiaPrivateIpAddress

    -- * CancelledSpotInstanceRequest
    , CancelledSpotInstanceRequest
    , cancelledSpotInstanceRequest
    , csiSpotInstanceRequestId
    , csiState

    -- * VpnConnectionOptionsSpecification
    , VpnConnectionOptionsSpecification
    , vpnConnectionOptionsSpecification
    , vcosStaticRoutesOnly

    -- * Address
    , Address
    , address
    , aAllocationId
    , aAssociationId
    , aDomain
    , aInstanceId
    , aNetworkInterfaceId
    , aNetworkInterfaceOwnerId
    , aPrivateIpAddress
    , aPublicIp

    -- * VolumeAttachmentState
    , VolumeAttachmentState (..)

    -- * LaunchPermission
    , LaunchPermission
    , launchPermission
    , lpGroup
    , lpUserId

    -- * RouteState
    , RouteState (..)

    -- * RouteTableAssociation
    , RouteTableAssociation
    , routeTableAssociation
    , rtaMain
    , rtaRouteTableAssociationId
    , rtaRouteTableId
    , rtaSubnetId

    -- * BundleTaskState
    , BundleTaskState (..)

    -- * PortRange
    , PortRange
    , portRange
    , prFrom
    , prTo

    -- * VpcAttributeName
    , VpcAttributeName (..)

    -- * ReservedInstancesConfiguration
    , ReservedInstancesConfiguration
    , reservedInstancesConfiguration
    , ricAvailabilityZone
    , ricInstanceCount
    , ricInstanceType
    , ricPlatform

    -- * VolumeStatusDetails
    , VolumeStatusDetails
    , volumeStatusDetails
    , vsdName
    , vsdStatus

    -- * SpotInstanceState
    , SpotInstanceState (..)

    -- * VpnConnectionOptions
    , VpnConnectionOptions
    , vpnConnectionOptions
    , vcoStaticRoutesOnly

    -- * UserIdGroupPair
    , UserIdGroupPair
    , userIdGroupPair
    , uigpGroupId
    , uigpGroupName
    , uigpUserId

    -- * InstanceStatusSummary
    , InstanceStatusSummary
    , instanceStatusSummary
    , issDetails
    , issStatus

    -- * SpotPlacement
    , SpotPlacement
    , spotPlacement
    , sp1AvailabilityZone
    , sp1GroupName

    -- * EbsInstanceBlockDeviceSpecification
    , EbsInstanceBlockDeviceSpecification
    , ebsInstanceBlockDeviceSpecification
    , eibdsDeleteOnTermination
    , eibdsVolumeId

    -- * NetworkAclAssociation
    , NetworkAclAssociation
    , networkAclAssociation
    , naaNetworkAclAssociationId
    , naaNetworkAclId
    , naaSubnetId

    -- * BundleTask
    , BundleTask
    , bundleTask
    , btBundleId
    , btBundleTaskError
    , btInstanceId
    , btProgress
    , btStartTime
    , btState
    , btStorage
    , btUpdateTime

    -- * InstanceStatusEvent
    , InstanceStatusEvent
    , instanceStatusEvent
    , iseCode
    , iseDescription
    , iseNotAfter
    , iseNotBefore

    -- * InstanceType
    , InstanceType (..)

    -- * Route
    , Route
    , route
    , rDestinationCidrBlock
    , rGatewayId
    , rInstanceId
    , rInstanceOwnerId
    , rNetworkInterfaceId
    , rOrigin
    , rState
    , rVpcPeeringConnectionId

    -- * SpotDatafeedSubscription
    , SpotDatafeedSubscription
    , spotDatafeedSubscription
    , sdsBucket
    , sdsFault
    , sdsOwnerId
    , sdsPrefix
    , sdsState

    -- * Storage
    , Storage
    , storage
    , sS3

    -- * SecurityGroup
    , SecurityGroup
    , securityGroup
    , sgDescription
    , sgGroupId
    , sgGroupName
    , sgIpPermissions
    , sgIpPermissionsEgress
    , sgOwnerId
    , sgTags
    , sgVpcId

    -- * CancelSpotInstanceRequestState
    , CancelSpotInstanceRequestState (..)

    -- * PlacementGroupState
    , PlacementGroupState (..)

    -- * ReservedInstancesModificationResult
    , ReservedInstancesModificationResult
    , reservedInstancesModificationResult
    , rimrReservedInstancesId
    , rimrTargetConfiguration

    -- * InstanceBlockDeviceMappingSpecification
    , InstanceBlockDeviceMappingSpecification
    , instanceBlockDeviceMappingSpecification
    , ibdmsDeviceName
    , ibdmsEbs
    , ibdmsNoDevice
    , ibdmsVirtualName

    -- * ExportEnvironment
    , ExportEnvironment (..)

    -- * VolumeAttachment
    , VolumeAttachment
    , volumeAttachment
    , vaAttachTime
    , vaDeleteOnTermination
    , vaDevice
    , vaInstanceId
    , vaState
    , vaVolumeId

    -- * CustomerGateway
    , CustomerGateway
    , customerGateway
    , cgBgpAsn
    , cgCustomerGatewayId
    , cgIpAddress
    , cgState
    , cgTags
    , cgType

    -- * EbsInstanceBlockDevice
    , EbsInstanceBlockDevice
    , ebsInstanceBlockDevice
    , eibdAttachTime
    , eibdDeleteOnTermination
    , eibdStatus
    , eibdVolumeId

    -- * ShutdownBehavior
    , ShutdownBehavior (..)

    -- * DiskImageDescription
    , DiskImageDescription
    , diskImageDescription
    , did1Checksum
    , did1Format
    , did1ImportManifestUrl
    , did1Size

    -- * DiskImageVolumeDescription
    , DiskImageVolumeDescription
    , diskImageVolumeDescription
    , divdId
    , divdSize

    -- * Monitoring
    , Monitoring
    , monitoring
    , mState

    -- * SubnetState
    , SubnetState (..)

    -- * ContainerFormat
    , ContainerFormat (..)

    -- * AvailabilityZoneMessage
    , AvailabilityZoneMessage
    , availabilityZoneMessage
    , azmMessage

    -- * VpcAttachment
    , VpcAttachment
    , vpcAttachment
    , va1State
    , va1VpcId

    -- * InstanceBlockDeviceMapping
    , InstanceBlockDeviceMapping
    , instanceBlockDeviceMapping
    , ibdmDeviceName
    , ibdmEbs

    -- * StatusType
    , StatusType (..)

    -- * ExportToS3TaskSpecification
    , ExportToS3TaskSpecification
    , exportToS3TaskSpecification
    , etstsContainerFormat
    , etstsDiskImageFormat
    , etstsS3Bucket
    , etstsS3Prefix

    -- * NetworkInterfaceAttribute
    , NetworkInterfaceAttribute (..)

    -- * ImageTypeValues
    , ImageTypeValues (..)

    -- * InstanceExportDetails
    , InstanceExportDetails
    , instanceExportDetails
    , iedInstanceId
    , iedTargetEnvironment

    -- * SnapshotAttributeName
    , SnapshotAttributeName (..)

    -- * AvailabilityZone
    , AvailabilityZone
    , availabilityZone
    , azMessages
    , azRegionName
    , azState
    , azZoneName

    -- * VpnState
    , VpnState (..)

    -- * RouteTable
    , RouteTable
    , routeTable
    , rtAssociations
    , rtPropagatingVgws
    , rtRouteTableId
    , rtRoutes
    , rtTags
    , rtVpcId

    -- * HypervisorType
    , HypervisorType (..)

    -- * InstanceStatusDetails
    , InstanceStatusDetails
    , instanceStatusDetails
    , isdImpairedSince
    , isdName
    , isdStatus

    -- * IamInstanceProfile
    , IamInstanceProfile
    , iamInstanceProfile
    , iipArn
    , iipId

    -- * InternetGatewayAttachment
    , InternetGatewayAttachment
    , internetGatewayAttachment
    , igaState
    , igaVpcId

    -- * ReservedInstanceState
    , ReservedInstanceState (..)

    -- * InstanceAttributeName
    , InstanceAttributeName (..)

    -- * IpPermission
    , IpPermission
    , ipPermission
    , ipFromPort
    , ipIpProtocol
    , ipIpRanges
    , ipToPort
    , ipUserIdGroupPairs

    -- * ConversionTaskState
    , ConversionTaskState (..)

    -- * DiskImage
    , DiskImage
    , diskImage
    , diDescription
    , diImage
    , diVolume

    -- * Tenancy
    , Tenancy (..)

    -- * VpcPeeringConnectionStateReason
    , VpcPeeringConnectionStateReason
    , vpcPeeringConnectionStateReason
    , vpcsrCode
    , vpcsrMessage

    -- * IamInstanceProfileSpecification
    , IamInstanceProfileSpecification
    , iamInstanceProfileSpecification
    , iipsArn
    , iipsName

    -- * ImportVolumeTaskDetails
    , ImportVolumeTaskDetails
    , importVolumeTaskDetails
    , ivtdAvailabilityZone
    , ivtdBytesConverted
    , ivtdDescription
    , ivtdImage
    , ivtdVolume

    -- * PlacementStrategy
    , PlacementStrategy (..)

    -- * InstanceNetworkInterface
    , InstanceNetworkInterface
    , instanceNetworkInterface
    , iniAssociation
    , iniAttachment
    , iniDescription
    , iniGroups
    , iniMacAddress
    , iniNetworkInterfaceId
    , iniOwnerId
    , iniPrivateDnsName
    , iniPrivateIpAddress
    , iniPrivateIpAddresses
    , iniSourceDestCheck
    , iniStatus
    , iniSubnetId
    , iniVpcId

    -- * VolumeStatusAction
    , VolumeStatusAction
    , volumeStatusAction
    , vsaCode
    , vsaDescription
    , vsaEventId
    , vsaEventType

    -- * VpcPeeringConnectionVpcInfo
    , VpcPeeringConnectionVpcInfo
    , vpcPeeringConnectionVpcInfo
    , vpcviCidrBlock
    , vpcviOwnerId
    , vpcviVpcId

    -- * ReservedInstanceLimitPrice
    , ReservedInstanceLimitPrice
    , reservedInstanceLimitPrice
    , rilpAmount
    , rilpCurrencyCode

    -- * Vpc
    , Vpc
    , vpc
    , vpcCidrBlock
    , vpcDhcpOptionsId
    , vpcInstanceTenancy
    , vpcIsDefault
    , vpcState
    , vpcTags
    , vpcVpcId

    -- * InstanceStatus
    , InstanceStatus
    , instanceStatus
    , isAvailabilityZone
    , isEvents
    , isInstanceId
    , isInstanceState
    , isInstanceStatus
    , isSystemStatus

    -- * ArchitectureValues
    , ArchitectureValues (..)

    -- * ReportInstanceReasonCodes
    , ReportInstanceReasonCodes (..)

    -- * EbsBlockDevice
    , EbsBlockDevice
    , ebsBlockDevice
    , ebdDeleteOnTermination
    , ebdEncrypted
    , ebdIops
    , ebdSnapshotId
    , ebdVolumeSize
    , ebdVolumeType

    -- * AccountAttribute
    , AccountAttribute
    , accountAttribute
    , aaAttributeName
    , aaAttributeValues

    -- * PriceSchedule
    , PriceSchedule
    , priceSchedule
    , psActive
    , psCurrencyCode
    , psPrice
    , psTerm

    -- * DeviceType
    , DeviceType (..)

    -- * DomainType
    , DomainType (..)

    -- * Region
    , Region
    , region
    , rEndpoint
    , rRegionName

    -- * PropagatingVgw
    , PropagatingVgw
    , propagatingVgw
    , pvGatewayId

    -- * OfferingTypeValues
    , OfferingTypeValues (..)

    -- * VpnGateway
    , VpnGateway
    , vpnGateway
    , vgAvailabilityZone
    , vgState
    , vgTags
    , vgType
    , vgVpcAttachments
    , vgVpnGatewayId

    -- * Filter
    , Filter
    , filter'
    , fName
    , fValues

    -- * VolumeType
    , VolumeType (..)

    -- * InstanceStateChange
    , InstanceStateChange
    , instanceStateChange
    , iscCurrentState
    , iscInstanceId
    , iscPreviousState

    -- * NetworkAcl
    , NetworkAcl
    , networkAcl
    , naAssociations
    , naEntries
    , naIsDefault
    , naNetworkAclId
    , naTags
    , naVpcId

    -- * ImageState
    , ImageState (..)

    -- * GatewayType
    , GatewayType (..)

    -- * InstanceNetworkInterfaceAttachment
    , InstanceNetworkInterfaceAttachment
    , instanceNetworkInterfaceAttachment
    , iniaAttachTime
    , iniaAttachmentId
    , iniaDeleteOnTermination
    , iniaDeviceIndex
    , iniaStatus

    -- * AttributeBooleanValue
    , AttributeBooleanValue
    , attributeBooleanValue
    , abvValue

    -- * RecurringCharge
    , RecurringCharge
    , recurringCharge
    , rcAmount
    , rcFrequency

    -- * NewDhcpConfiguration
    , NewDhcpConfiguration
    , newDhcpConfiguration
    , ndcKey
    , ndcValues

    -- * StateReason
    , StateReason
    , stateReason
    , srCode
    , srMessage

    -- * MonitoringState
    , MonitoringState (..)

    -- * ReservedInstancesId
    , ReservedInstancesId
    , reservedInstancesId
    , riiReservedInstancesId

    -- * StatusName
    , StatusName (..)

    -- * InternetGateway
    , InternetGateway
    , internetGateway
    , igAttachments
    , igInternetGatewayId
    , igTags

    -- * VolumeStatusName
    , VolumeStatusName (..)

    -- * VolumeAttributeName
    , VolumeAttributeName (..)

    -- * ImportInstanceTaskDetails
    , ImportInstanceTaskDetails
    , importInstanceTaskDetails
    , iitdDescription
    , iitdInstanceId
    , iitdPlatform
    , iitdVolumes

    -- * PlacementGroup
    , PlacementGroup
    , placementGroup
    , pgGroupName
    , pgState
    , pgStrategy

    -- * ProductCode
    , ProductCode
    , productCode
    , pcProductCodeId
    , pcProductCodeType

    -- * ListingStatus
    , ListingStatus (..)

    -- * IpRange
    , IpRange
    , ipRange
    , irCidrIp

    -- * VolumeStatusInfoStatus
    , VolumeStatusInfoStatus (..)

    -- * AccountAttributeValue
    , AccountAttributeValue
    , accountAttributeValue
    , aavAttributeValue

    -- * RIProductDescription
    , RIProductDescription (..)

    -- * ReservedInstancesOffering
    , ReservedInstancesOffering
    , reservedInstancesOffering
    , rioAvailabilityZone
    , rioCurrencyCode
    , rioDuration
    , rioFixedPrice
    , rioInstanceTenancy
    , rioInstanceType
    , rioMarketplace
    , rioOfferingType
    , rioPricingDetails
    , rioProductDescription
    , rioRecurringCharges
    , rioReservedInstancesOfferingId
    , rioUsagePrice

    -- * ReservedInstances
    , ReservedInstances
    , reservedInstances
    , ri1AvailabilityZone
    , ri1CurrencyCode
    , ri1Duration
    , ri1End
    , ri1FixedPrice
    , ri1InstanceCount
    , ri1InstanceTenancy
    , ri1InstanceType
    , ri1OfferingType
    , ri1ProductDescription
    , ri1RecurringCharges
    , ri1ReservedInstancesId
    , ri1Start
    , ri1State
    , ri1Tags
    , ri1UsagePrice

    -- * DatafeedSubscriptionState
    , DatafeedSubscriptionState (..)

    -- * ExportTaskState
    , ExportTaskState (..)

    -- * ProductCodeValues
    , ProductCodeValues (..)

    -- * VpnConnection
    , VpnConnection
    , vpnConnection
    , vcCustomerGatewayConfiguration
    , vcCustomerGatewayId
    , vcOptions
    , vcRoutes
    , vcState
    , vcTags
    , vcType
    , vcVgwTelemetry
    , vcVpnConnectionId
    , vcVpnGatewayId

    -- * InstanceState
    , InstanceState
    , instanceState
    , isCode
    , isName

    -- * Placement
    , Placement
    , placement
    , pAvailabilityZone
    , pGroupName
    , pTenancy

    -- * EventCode
    , EventCode (..)

    -- * SpotInstanceType
    , SpotInstanceType (..)

    -- * VpcPeeringConnection
    , VpcPeeringConnection
    , vpcPeeringConnection
    , vpc1AccepterVpcInfo
    , vpc1ExpirationTime
    , vpc1RequesterVpcInfo
    , vpc1Status
    , vpc1Tags
    , vpc1VpcPeeringConnectionId

    -- * S3Storage
    , S3Storage
    , s3Storage
    , ssAWSAccessKeyId
    , ssBucket
    , ssPrefix
    , ssUploadPolicy
    , ssUploadPolicySignature

    -- * VgwTelemetry
    , VgwTelemetry
    , vgwTelemetry
    , vtAcceptedRouteCount
    , vtLastStatusChange
    , vtOutsideIpAddress
    , vtStatus
    , vtStatusMessage

    -- * VpnStaticRoute
    , VpnStaticRoute
    , vpnStaticRoute
    , vsrDestinationCidrBlock
    , vsrSource
    , vsrState

    -- * InstanceStateName
    , InstanceStateName (..)

    -- * Instance
    , Instance
    , instance'
    , i1AmiLaunchIndex
    , i1Architecture
    , i1BlockDeviceMappings
    , i1ClientToken
    , i1EbsOptimized
    , i1Hypervisor
    , i1IamInstanceProfile
    , i1ImageId
    , i1InstanceId
    , i1InstanceLifecycle
    , i1InstanceType
    , i1KernelId
    , i1KeyName
    , i1LaunchTime
    , i1Monitoring
    , i1NetworkInterfaces
    , i1Placement
    , i1Platform
    , i1PrivateDnsName
    , i1PrivateIpAddress
    , i1ProductCodes
    , i1PublicDnsName
    , i1PublicIpAddress
    , i1RamdiskId
    , i1RootDeviceName
    , i1RootDeviceType
    , i1SecurityGroups
    , i1SourceDestCheck
    , i1SpotInstanceRequestId
    , i1SriovNetSupport
    , i1State
    , i1StateReason
    , i1StateTransitionReason
    , i1SubnetId
    , i1Tags
    , i1VirtualizationType
    , i1VpcId

    -- * ExportTask
    , ExportTask
    , exportTask
    , etDescription
    , etExportTaskId
    , etExportToS3Task
    , etInstanceExportDetails
    , etState
    , etStatusMessage

    -- * ResetImageAttributeName
    , ResetImageAttributeName (..)

    -- * RequestSpotLaunchSpecification
    , RequestSpotLaunchSpecification
    , requestSpotLaunchSpecification
    , rslsAddressingType
    , rslsBlockDeviceMappings
    , rslsEbsOptimized
    , rslsIamInstanceProfile
    , rslsImageId
    , rslsInstanceType
    , rslsKernelId
    , rslsKeyName
    , rslsMonitoring
    , rslsNetworkInterfaces
    , rslsPlacement
    , rslsRamdiskId
    , rslsSecurityGroupIds
    , rslsSecurityGroups
    , rslsSubnetId
    , rslsUserData

    -- * VolumeDetail
    , VolumeDetail
    , volumeDetail
    , vdSize

    -- * PricingDetail
    , PricingDetail
    , pricingDetail
    , pdCount
    , pdPrice

    -- * NetworkInterfacePrivateIpAddress
    , NetworkInterfacePrivateIpAddress
    , networkInterfacePrivateIpAddress
    , nipiaAssociation
    , nipiaPrimary
    , nipiaPrivateDnsName
    , nipiaPrivateIpAddress

    -- * DiskImageFormat
    , DiskImageFormat (..)

    -- * BundleTaskError
    , BundleTaskError
    , bundleTaskError
    , bteCode
    , bteMessage

    -- * VolumeStatusItem
    , VolumeStatusItem
    , volumeStatusItem
    , vsiActions
    , vsiAvailabilityZone
    , vsiEvents
    , vsiVolumeId
    , vsiVolumeStatus

    -- * Common
    , module Network.AWS.EC2.Internal
    ) where

import Network.AWS.Error
import Network.AWS.Prelude
import Network.AWS.Signing.V4
import Network.AWS.EC2.Internal
import qualified GHC.Exts

-- | Supported version (@2014-09-01@) of the Amazon Elastic Compute Cloud.
data EC2 deriving (Typeable)

instance AWSService EC2 where
    type Sg EC2 = V4
    type Er EC2 = RESTError

    service = Service
        { _svcEndpoint     = regional
        , _svcAbbrev       = "EC2"
        , _svcPrefix       = "ec2"
        , _svcVersion      = "2014-09-01"
        , _svcTargetPrefix = Nothing
        , _svcJSONVersion  = Nothing
        }

    handle = restError alwaysFail

data ImageAttributeName
    = ImageBlockDeviceMapping -- ^ blockDeviceMapping
    | ImageDescription        -- ^ description
    | ImageKernel             -- ^ kernel
    | ImageLaunchPermission   -- ^ launchPermission
    | ImageProductCodes       -- ^ productCodes
    | ImageRamdisk            -- ^ ramdisk
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable ImageAttributeName

instance FromText ImageAttributeName where
    parser = match "blockDeviceMapping" ImageBlockDeviceMapping
         <|> match "description"        ImageDescription
         <|> match "kernel"             ImageKernel
         <|> match "launchPermission"   ImageLaunchPermission
         <|> match "productCodes"       ImageProductCodes
         <|> match "ramdisk"            ImageRamdisk

instance ToText ImageAttributeName where
    toText = \case
        ImageBlockDeviceMapping -> "blockDeviceMapping"
        ImageDescription        -> "description"
        ImageKernel             -> "kernel"
        ImageLaunchPermission   -> "launchPermission"
        ImageProductCodes       -> "productCodes"
        ImageRamdisk            -> "ramdisk"

instance FromXML ImageAttributeName where
    parseXML = fromXMLText "ImageAttributeName"

instance ToQuery ImageAttributeName

data PermissionGroup
    = All -- ^ all
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable PermissionGroup

instance FromText PermissionGroup where
    parser = match "all" All

instance ToText PermissionGroup where
    toText All = "all"

instance FromXML PermissionGroup where
    parseXML = fromXMLText "PermissionGroup"

instance ToQuery PermissionGroup

data NetworkAclEntry = NetworkAclEntry
    { _naeCidrBlock    :: Maybe Text
    , _naeEgress       :: Maybe Bool
    , _naeIcmpTypeCode :: Maybe IcmpTypeCode
    , _naePortRange    :: Maybe PortRange
    , _naeProtocol     :: Maybe Text
    , _naeRuleAction   :: Maybe Text
    , _naeRuleNumber   :: Maybe Int
    } deriving (Eq, Show, Generic)

-- | 'NetworkAclEntry' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'naeCidrBlock' @::@ 'Maybe' 'Text'
--
-- * 'naeEgress' @::@ 'Maybe' 'Bool'
--
-- * 'naeIcmpTypeCode' @::@ 'Maybe' 'IcmpTypeCode'
--
-- * 'naePortRange' @::@ 'Maybe' 'PortRange'
--
-- * 'naeProtocol' @::@ 'Maybe' 'Text'
--
-- * 'naeRuleAction' @::@ 'Maybe' 'Text'
--
-- * 'naeRuleNumber' @::@ 'Maybe' 'Int'
--
networkAclEntry :: NetworkAclEntry
networkAclEntry = NetworkAclEntry
    { _naeRuleNumber   = Nothing
    , _naeProtocol     = Nothing
    , _naeRuleAction   = Nothing
    , _naeEgress       = Nothing
    , _naeCidrBlock    = Nothing
    , _naeIcmpTypeCode = Nothing
    , _naePortRange    = Nothing
    }

-- | The network range to allow or deny, in CIDR notation.
naeCidrBlock :: Lens' NetworkAclEntry (Maybe Text)
naeCidrBlock = lens _naeCidrBlock (\s a -> s { _naeCidrBlock = a })

-- | Indicates whether the rule is an egress rule (applied to traffic leaving
-- the subnet).
naeEgress :: Lens' NetworkAclEntry (Maybe Bool)
naeEgress = lens _naeEgress (\s a -> s { _naeEgress = a })

-- | ICMP protocol: The ICMP type and code.
naeIcmpTypeCode :: Lens' NetworkAclEntry (Maybe IcmpTypeCode)
naeIcmpTypeCode = lens _naeIcmpTypeCode (\s a -> s { _naeIcmpTypeCode = a })

-- | TCP or UDP protocols: The range of ports the rule applies to.
naePortRange :: Lens' NetworkAclEntry (Maybe PortRange)
naePortRange = lens _naePortRange (\s a -> s { _naePortRange = a })

-- | The protocol. A value of -1 means all protocols.
naeProtocol :: Lens' NetworkAclEntry (Maybe Text)
naeProtocol = lens _naeProtocol (\s a -> s { _naeProtocol = a })

-- | Indicates whether to allow or deny the traffic that matches the rule.
naeRuleAction :: Lens' NetworkAclEntry (Maybe Text)
naeRuleAction = lens _naeRuleAction (\s a -> s { _naeRuleAction = a })

-- | The rule number for the entry. ACL entries are processed in ascending
-- order by rule number.
naeRuleNumber :: Lens' NetworkAclEntry (Maybe Int)
naeRuleNumber = lens _naeRuleNumber (\s a -> s { _naeRuleNumber = a })

instance FromXML NetworkAclEntry where
    parseXML c = NetworkAclEntry
        <$> c .:? "cidrBlock"
        <*> c .:? "egress"
        <*> c .:? "icmpTypeCode"
        <*> c .:? "portRange"
        <*> c .:? "protocol"
        <*> c .:? "ruleAction"
        <*> c .:? "ruleNumber"

instance ToQuery NetworkAclEntry

newtype BlobAttributeValue = BlobAttributeValue
    { _bavValue :: Maybe Base64
    } deriving (Eq, Show, Generic)

-- | 'BlobAttributeValue' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bavValue' @::@ 'Maybe' 'Base64'
--
blobAttributeValue :: BlobAttributeValue
blobAttributeValue = BlobAttributeValue
    { _bavValue = Nothing
    }

bavValue :: Lens' BlobAttributeValue (Maybe Base64)
bavValue = lens _bavValue (\s a -> s { _bavValue = a })

instance FromXML BlobAttributeValue where
    parseXML c = BlobAttributeValue
        <$> c .:? "value"

instance ToQuery BlobAttributeValue

data ImportInstanceLaunchSpecification = ImportInstanceLaunchSpecification
    { _iilsAdditionalInfo                    :: Maybe Text
    , _iilsArchitecture                      :: Maybe Text
    , _iilsGroupIds                          :: [Text]
    , _iilsGroupNames                        :: [Text]
    , _iilsInstanceInitiatedShutdownBehavior :: Maybe Text
    , _iilsInstanceType                      :: Maybe Text
    , _iilsMonitoring                        :: Maybe Bool
    , _iilsPlacement                         :: Maybe Placement
    , _iilsPrivateIpAddress                  :: Maybe Text
    , _iilsSubnetId                          :: Maybe Text
    , _iilsUserData                          :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'ImportInstanceLaunchSpecification' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iilsAdditionalInfo' @::@ 'Maybe' 'Text'
--
-- * 'iilsArchitecture' @::@ 'Maybe' 'Text'
--
-- * 'iilsGroupIds' @::@ ['Text']
--
-- * 'iilsGroupNames' @::@ ['Text']
--
-- * 'iilsInstanceInitiatedShutdownBehavior' @::@ 'Maybe' 'Text'
--
-- * 'iilsInstanceType' @::@ 'Maybe' 'Text'
--
-- * 'iilsMonitoring' @::@ 'Maybe' 'Bool'
--
-- * 'iilsPlacement' @::@ 'Maybe' 'Placement'
--
-- * 'iilsPrivateIpAddress' @::@ 'Maybe' 'Text'
--
-- * 'iilsSubnetId' @::@ 'Maybe' 'Text'
--
-- * 'iilsUserData' @::@ 'Maybe' 'Text'
--
importInstanceLaunchSpecification :: ImportInstanceLaunchSpecification
importInstanceLaunchSpecification = ImportInstanceLaunchSpecification
    { _iilsArchitecture                      = Nothing
    , _iilsGroupNames                        = mempty
    , _iilsGroupIds                          = mempty
    , _iilsAdditionalInfo                    = Nothing
    , _iilsUserData                          = Nothing
    , _iilsInstanceType                      = Nothing
    , _iilsPlacement                         = Nothing
    , _iilsMonitoring                        = Nothing
    , _iilsSubnetId                          = Nothing
    , _iilsInstanceInitiatedShutdownBehavior = Nothing
    , _iilsPrivateIpAddress                  = Nothing
    }

iilsAdditionalInfo :: Lens' ImportInstanceLaunchSpecification (Maybe Text)
iilsAdditionalInfo =
    lens _iilsAdditionalInfo (\s a -> s { _iilsAdditionalInfo = a })

-- | The architecture of the instance.
iilsArchitecture :: Lens' ImportInstanceLaunchSpecification (Maybe Text)
iilsArchitecture = lens _iilsArchitecture (\s a -> s { _iilsArchitecture = a })

iilsGroupIds :: Lens' ImportInstanceLaunchSpecification [Text]
iilsGroupIds = lens _iilsGroupIds (\s a -> s { _iilsGroupIds = a })

-- | One or more security group names.
iilsGroupNames :: Lens' ImportInstanceLaunchSpecification [Text]
iilsGroupNames = lens _iilsGroupNames (\s a -> s { _iilsGroupNames = a })

-- | Indicates whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for system
-- shutdown).
iilsInstanceInitiatedShutdownBehavior :: Lens' ImportInstanceLaunchSpecification (Maybe Text)
iilsInstanceInitiatedShutdownBehavior =
    lens _iilsInstanceInitiatedShutdownBehavior
        (\s a -> s { _iilsInstanceInitiatedShutdownBehavior = a })

-- | The instance type. For more information, see Instance Types in the Amazon
-- Elastic Compute Cloud User Guide.
iilsInstanceType :: Lens' ImportInstanceLaunchSpecification (Maybe Text)
iilsInstanceType = lens _iilsInstanceType (\s a -> s { _iilsInstanceType = a })

iilsMonitoring :: Lens' ImportInstanceLaunchSpecification (Maybe Bool)
iilsMonitoring = lens _iilsMonitoring (\s a -> s { _iilsMonitoring = a })

iilsPlacement :: Lens' ImportInstanceLaunchSpecification (Maybe Placement)
iilsPlacement = lens _iilsPlacement (\s a -> s { _iilsPlacement = a })

-- | [EC2-VPC] Optionally, you can use this parameter to assign the instance a
-- specific available IP address from the IP address range of the subnet.
iilsPrivateIpAddress :: Lens' ImportInstanceLaunchSpecification (Maybe Text)
iilsPrivateIpAddress =
    lens _iilsPrivateIpAddress (\s a -> s { _iilsPrivateIpAddress = a })

-- | [EC2-VPC] The ID of the subnet to launch the instance into.
iilsSubnetId :: Lens' ImportInstanceLaunchSpecification (Maybe Text)
iilsSubnetId = lens _iilsSubnetId (\s a -> s { _iilsSubnetId = a })

-- | User data to be made available to the instance.
iilsUserData :: Lens' ImportInstanceLaunchSpecification (Maybe Text)
iilsUserData = lens _iilsUserData (\s a -> s { _iilsUserData = a })

instance FromXML ImportInstanceLaunchSpecification where
    parseXML c = ImportInstanceLaunchSpecification
        <$> c .:? "additionalInfo"
        <*> c .:? "architecture"
        <*> c .: "GroupId"
        <*> c .: "GroupName"
        <*> c .:? "instanceInitiatedShutdownBehavior"
        <*> c .:? "instanceType"
        <*> c .:? "monitoring"
        <*> c .:? "placement"
        <*> c .:? "privateIpAddress"
        <*> c .:? "subnetId"
        <*> c .:? "userData"

instance ToQuery ImportInstanceLaunchSpecification

data Snapshot = Snapshot
    { _sDescription :: Maybe Text
    , _sEncrypted   :: Maybe Bool
    , _sOwnerAlias  :: Maybe Text
    , _sOwnerId     :: Maybe Text
    , _sProgress    :: Maybe Text
    , _sSnapshotId  :: Maybe Text
    , _sStartTime   :: Maybe RFC822
    , _sState       :: Maybe Text
    , _sTags        :: [Tag]
    , _sVolumeId    :: Maybe Text
    , _sVolumeSize  :: Maybe Int
    } deriving (Eq, Show, Generic)

-- | 'Snapshot' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sDescription' @::@ 'Maybe' 'Text'
--
-- * 'sEncrypted' @::@ 'Maybe' 'Bool'
--
-- * 'sOwnerAlias' @::@ 'Maybe' 'Text'
--
-- * 'sOwnerId' @::@ 'Maybe' 'Text'
--
-- * 'sProgress' @::@ 'Maybe' 'Text'
--
-- * 'sSnapshotId' @::@ 'Maybe' 'Text'
--
-- * 'sStartTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'sState' @::@ 'Maybe' 'Text'
--
-- * 'sTags' @::@ ['Tag']
--
-- * 'sVolumeId' @::@ 'Maybe' 'Text'
--
-- * 'sVolumeSize' @::@ 'Maybe' 'Int'
--
snapshot :: Snapshot
snapshot = Snapshot
    { _sSnapshotId  = Nothing
    , _sVolumeId    = Nothing
    , _sState       = Nothing
    , _sStartTime   = Nothing
    , _sProgress    = Nothing
    , _sOwnerId     = Nothing
    , _sDescription = Nothing
    , _sVolumeSize  = Nothing
    , _sOwnerAlias  = Nothing
    , _sTags        = mempty
    , _sEncrypted   = Nothing
    }

-- | The description for the snapshot.
sDescription :: Lens' Snapshot (Maybe Text)
sDescription = lens _sDescription (\s a -> s { _sDescription = a })

-- | Indicates whether the snapshot is encrypted.
sEncrypted :: Lens' Snapshot (Maybe Bool)
sEncrypted = lens _sEncrypted (\s a -> s { _sEncrypted = a })

-- | The AWS account alias (for example, amazon, self) or AWS account ID that
-- owns the snapshot.
sOwnerAlias :: Lens' Snapshot (Maybe Text)
sOwnerAlias = lens _sOwnerAlias (\s a -> s { _sOwnerAlias = a })

-- | The AWS account ID of the Amazon EBS snapshot owner.
sOwnerId :: Lens' Snapshot (Maybe Text)
sOwnerId = lens _sOwnerId (\s a -> s { _sOwnerId = a })

-- | The progress of the snapshot, as a percentage.
sProgress :: Lens' Snapshot (Maybe Text)
sProgress = lens _sProgress (\s a -> s { _sProgress = a })

-- | The ID of the snapshot.
sSnapshotId :: Lens' Snapshot (Maybe Text)
sSnapshotId = lens _sSnapshotId (\s a -> s { _sSnapshotId = a })

-- | The time stamp when the snapshot was initiated.
sStartTime :: Lens' Snapshot (Maybe UTCTime)
sStartTime = lens _sStartTime (\s a -> s { _sStartTime = a })
    . mapping _Time

-- | The snapshot state.
sState :: Lens' Snapshot (Maybe Text)
sState = lens _sState (\s a -> s { _sState = a })

-- | Any tags assigned to the snapshot.
sTags :: Lens' Snapshot [Tag]
sTags = lens _sTags (\s a -> s { _sTags = a })

-- | The ID of the volume.
sVolumeId :: Lens' Snapshot (Maybe Text)
sVolumeId = lens _sVolumeId (\s a -> s { _sVolumeId = a })

-- | The size of the volume, in GiB.
sVolumeSize :: Lens' Snapshot (Maybe Int)
sVolumeSize = lens _sVolumeSize (\s a -> s { _sVolumeSize = a })

instance FromXML Snapshot where
    parseXML c = Snapshot
        <$> c .:? "description"
        <*> c .:? "encrypted"
        <*> c .:? "ownerAlias"
        <*> c .:? "ownerId"
        <*> c .:? "progress"
        <*> c .:? "snapshotId"
        <*> c .:? "startTime"
        <*> c .:? "status"
        <*> c .: "tagSet"
        <*> c .:? "volumeId"
        <*> c .:? "volumeSize"

instance ToQuery Snapshot

data SpotInstanceStateFault = SpotInstanceStateFault
    { _sisfCode    :: Maybe Text
    , _sisfMessage :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'SpotInstanceStateFault' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sisfCode' @::@ 'Maybe' 'Text'
--
-- * 'sisfMessage' @::@ 'Maybe' 'Text'
--
spotInstanceStateFault :: SpotInstanceStateFault
spotInstanceStateFault = SpotInstanceStateFault
    { _sisfCode    = Nothing
    , _sisfMessage = Nothing
    }

-- | The reason code for the Spot Instance state change.
sisfCode :: Lens' SpotInstanceStateFault (Maybe Text)
sisfCode = lens _sisfCode (\s a -> s { _sisfCode = a })

-- | The message for the Spot Instance state change.
sisfMessage :: Lens' SpotInstanceStateFault (Maybe Text)
sisfMessage = lens _sisfMessage (\s a -> s { _sisfMessage = a })

instance FromXML SpotInstanceStateFault where
    parseXML c = SpotInstanceStateFault
        <$> c .:? "code"
        <*> c .:? "message"

instance ToQuery SpotInstanceStateFault

data TagDescription = TagDescription
    { _tdKey          :: Text
    , _tdResourceId   :: Text
    , _tdResourceType :: Text
    , _tdValue        :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'TagDescription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tdKey' @::@ 'Text'
--
-- * 'tdResourceId' @::@ 'Text'
--
-- * 'tdResourceType' @::@ 'Text'
--
-- * 'tdValue' @::@ 'Text'
--
tagDescription :: Text -- ^ 'tdResourceId'
               -> Text -- ^ 'tdResourceType'
               -> Text -- ^ 'tdKey'
               -> Text -- ^ 'tdValue'
               -> TagDescription
tagDescription p1 p2 p3 p4 = TagDescription
    { _tdResourceId   = p1
    , _tdResourceType = p2
    , _tdKey          = p3
    , _tdValue        = p4
    }

-- | The key of the tag.
tdKey :: Lens' TagDescription Text
tdKey = lens _tdKey (\s a -> s { _tdKey = a })

-- | The ID of the resource. For example, ami-1a2b3c4d.
tdResourceId :: Lens' TagDescription Text
tdResourceId = lens _tdResourceId (\s a -> s { _tdResourceId = a })

-- | The type of resource.
tdResourceType :: Lens' TagDescription Text
tdResourceType = lens _tdResourceType (\s a -> s { _tdResourceType = a })

-- | The value of the tag.
tdValue :: Lens' TagDescription Text
tdValue = lens _tdValue (\s a -> s { _tdValue = a })

instance FromXML TagDescription where
    parseXML c = TagDescription
        <$> c .: "key"
        <*> c .: "resourceId"
        <*> c .: "resourceType"
        <*> c .: "value"

instance ToQuery TagDescription

data GroupIdentifier = GroupIdentifier
    { _giGroupId   :: Maybe Text
    , _giGroupName :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'GroupIdentifier' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'giGroupId' @::@ 'Maybe' 'Text'
--
-- * 'giGroupName' @::@ 'Maybe' 'Text'
--
groupIdentifier :: GroupIdentifier
groupIdentifier = GroupIdentifier
    { _giGroupName = Nothing
    , _giGroupId   = Nothing
    }

-- | The ID of the security group.
giGroupId :: Lens' GroupIdentifier (Maybe Text)
giGroupId = lens _giGroupId (\s a -> s { _giGroupId = a })

-- | The name of the security group.
giGroupName :: Lens' GroupIdentifier (Maybe Text)
giGroupName = lens _giGroupName (\s a -> s { _giGroupName = a })

instance FromXML GroupIdentifier where
    parseXML c = GroupIdentifier
        <$> c .:? "groupId"
        <*> c .:? "groupName"

instance ToQuery GroupIdentifier

data VpnStaticRouteSource
    = Static -- ^ Static
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable VpnStaticRouteSource

instance FromText VpnStaticRouteSource where
    parser = match "Static" Static

instance ToText VpnStaticRouteSource where
    toText Static = "Static"

instance FromXML VpnStaticRouteSource where
    parseXML = fromXMLText "VpnStaticRouteSource"

instance ToQuery VpnStaticRouteSource

data ReservedInstancesListing = ReservedInstancesListing
    { _rilClientToken                :: Maybe Text
    , _rilCreateDate                 :: Maybe RFC822
    , _rilInstanceCounts             :: [InstanceCount]
    , _rilPriceSchedules             :: [PriceSchedule]
    , _rilReservedInstancesId        :: Maybe Text
    , _rilReservedInstancesListingId :: Maybe Text
    , _rilStatus                     :: Maybe Text
    , _rilStatusMessage              :: Maybe Text
    , _rilTags                       :: [Tag]
    , _rilUpdateDate                 :: Maybe RFC822
    } deriving (Eq, Show, Generic)

-- | 'ReservedInstancesListing' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rilClientToken' @::@ 'Maybe' 'Text'
--
-- * 'rilCreateDate' @::@ 'Maybe' 'UTCTime'
--
-- * 'rilInstanceCounts' @::@ ['InstanceCount']
--
-- * 'rilPriceSchedules' @::@ ['PriceSchedule']
--
-- * 'rilReservedInstancesId' @::@ 'Maybe' 'Text'
--
-- * 'rilReservedInstancesListingId' @::@ 'Maybe' 'Text'
--
-- * 'rilStatus' @::@ 'Maybe' 'Text'
--
-- * 'rilStatusMessage' @::@ 'Maybe' 'Text'
--
-- * 'rilTags' @::@ ['Tag']
--
-- * 'rilUpdateDate' @::@ 'Maybe' 'UTCTime'
--
reservedInstancesListing :: ReservedInstancesListing
reservedInstancesListing = ReservedInstancesListing
    { _rilReservedInstancesListingId = Nothing
    , _rilReservedInstancesId        = Nothing
    , _rilCreateDate                 = Nothing
    , _rilUpdateDate                 = Nothing
    , _rilStatus                     = Nothing
    , _rilStatusMessage              = Nothing
    , _rilInstanceCounts             = mempty
    , _rilPriceSchedules             = mempty
    , _rilTags                       = mempty
    , _rilClientToken                = Nothing
    }

-- | The idempotency token you provided when you created the listing.
rilClientToken :: Lens' ReservedInstancesListing (Maybe Text)
rilClientToken = lens _rilClientToken (\s a -> s { _rilClientToken = a })

-- | The time the listing was created.
rilCreateDate :: Lens' ReservedInstancesListing (Maybe UTCTime)
rilCreateDate = lens _rilCreateDate (\s a -> s { _rilCreateDate = a })
    . mapping _Time

-- | The number of instances in this state.
rilInstanceCounts :: Lens' ReservedInstancesListing [InstanceCount]
rilInstanceCounts =
    lens _rilInstanceCounts (\s a -> s { _rilInstanceCounts = a })

-- | The price of the Reserved Instance listing.
rilPriceSchedules :: Lens' ReservedInstancesListing [PriceSchedule]
rilPriceSchedules =
    lens _rilPriceSchedules (\s a -> s { _rilPriceSchedules = a })

-- | The ID of the Reserved Instance.
rilReservedInstancesId :: Lens' ReservedInstancesListing (Maybe Text)
rilReservedInstancesId =
    lens _rilReservedInstancesId (\s a -> s { _rilReservedInstancesId = a })

-- | The ID of the Reserved Instance listing.
rilReservedInstancesListingId :: Lens' ReservedInstancesListing (Maybe Text)
rilReservedInstancesListingId =
    lens _rilReservedInstancesListingId
        (\s a -> s { _rilReservedInstancesListingId = a })

-- | The status of the Reserved Instance listing.
rilStatus :: Lens' ReservedInstancesListing (Maybe Text)
rilStatus = lens _rilStatus (\s a -> s { _rilStatus = a })

-- | The reason for the current status of the Reserved Instance listing. The
-- response can be blank.
rilStatusMessage :: Lens' ReservedInstancesListing (Maybe Text)
rilStatusMessage = lens _rilStatusMessage (\s a -> s { _rilStatusMessage = a })

-- | Any tags assigned to the resource.
rilTags :: Lens' ReservedInstancesListing [Tag]
rilTags = lens _rilTags (\s a -> s { _rilTags = a })

-- | The last modified timestamp of the listing.
rilUpdateDate :: Lens' ReservedInstancesListing (Maybe UTCTime)
rilUpdateDate = lens _rilUpdateDate (\s a -> s { _rilUpdateDate = a })
    . mapping _Time

instance FromXML ReservedInstancesListing where
    parseXML c = ReservedInstancesListing
        <$> c .:? "clientToken"
        <*> c .:? "createDate"
        <*> c .: "instanceCounts"
        <*> c .: "priceSchedules"
        <*> c .:? "reservedInstancesId"
        <*> c .:? "reservedInstancesListingId"
        <*> c .:? "status"
        <*> c .:? "statusMessage"
        <*> c .: "tagSet"
        <*> c .:? "updateDate"

instance ToQuery ReservedInstancesListing

data InstanceLifecycleType
    = Spot -- ^ spot
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable InstanceLifecycleType

instance FromText InstanceLifecycleType where
    parser = match "spot" Spot

instance ToText InstanceLifecycleType where
    toText Spot = "spot"

instance FromXML InstanceLifecycleType where
    parseXML = fromXMLText "InstanceLifecycleType"

instance ToQuery InstanceLifecycleType

data VirtualizationType
    = Hvm         -- ^ hvm
    | Paravirtual -- ^ paravirtual
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable VirtualizationType

instance FromText VirtualizationType where
    parser = match "hvm"         Hvm
         <|> match "paravirtual" Paravirtual

instance ToText VirtualizationType where
    toText = \case
        Hvm         -> "hvm"
        Paravirtual -> "paravirtual"

instance FromXML VirtualizationType where
    parseXML = fromXMLText "VirtualizationType"

instance ToQuery VirtualizationType

data NetworkInterfaceStatus
    = Attaching -- ^ attaching
    | Available -- ^ available
    | Detaching -- ^ detaching
    | InUse     -- ^ in-use
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable NetworkInterfaceStatus

instance FromText NetworkInterfaceStatus where
    parser = match "attaching" Attaching
         <|> match "available" Available
         <|> match "detaching" Detaching
         <|> match "in-use"    InUse

instance ToText NetworkInterfaceStatus where
    toText = \case
        Attaching -> "attaching"
        Available -> "available"
        Detaching -> "detaching"
        InUse     -> "in-use"

instance FromXML NetworkInterfaceStatus where
    parseXML = fromXMLText "NetworkInterfaceStatus"

instance ToQuery NetworkInterfaceStatus

data PlatformValues
    = Windows -- ^ Windows
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable PlatformValues

instance FromText PlatformValues where
    parser = match "Windows" Windows

instance ToText PlatformValues where
    toText Windows = "Windows"

instance FromXML PlatformValues where
    parseXML = fromXMLText "PlatformValues"

instance ToQuery PlatformValues

data CreateVolumePermission = CreateVolumePermission
    { _cvpGroup  :: Maybe Text
    , _cvpUserId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateVolumePermission' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cvpGroup' @::@ 'Maybe' 'Text'
--
-- * 'cvpUserId' @::@ 'Maybe' 'Text'
--
createVolumePermission :: CreateVolumePermission
createVolumePermission = CreateVolumePermission
    { _cvpUserId = Nothing
    , _cvpGroup  = Nothing
    }

-- | The specific group that is to be added or removed from a volume's list of
-- create volume permissions.
cvpGroup :: Lens' CreateVolumePermission (Maybe Text)
cvpGroup = lens _cvpGroup (\s a -> s { _cvpGroup = a })

-- | The specific AWS account ID that is to be added or removed from a
-- volume's list of create volume permissions.
cvpUserId :: Lens' CreateVolumePermission (Maybe Text)
cvpUserId = lens _cvpUserId (\s a -> s { _cvpUserId = a })

instance FromXML CreateVolumePermission where
    parseXML c = CreateVolumePermission
        <$> c .:? "group"
        <*> c .:? "userId"

instance ToQuery CreateVolumePermission

data NetworkInterfaceAttachmentChanges = NetworkInterfaceAttachmentChanges
    { _niacAttachmentId        :: Maybe Text
    , _niacDeleteOnTermination :: Maybe Bool
    } deriving (Eq, Ord, Show, Generic)

-- | 'NetworkInterfaceAttachmentChanges' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'niacAttachmentId' @::@ 'Maybe' 'Text'
--
-- * 'niacDeleteOnTermination' @::@ 'Maybe' 'Bool'
--
networkInterfaceAttachmentChanges :: NetworkInterfaceAttachmentChanges
networkInterfaceAttachmentChanges = NetworkInterfaceAttachmentChanges
    { _niacAttachmentId        = Nothing
    , _niacDeleteOnTermination = Nothing
    }

-- | The ID of the network interface attachment.
niacAttachmentId :: Lens' NetworkInterfaceAttachmentChanges (Maybe Text)
niacAttachmentId = lens _niacAttachmentId (\s a -> s { _niacAttachmentId = a })

-- | Indicates whether the network interface is deleted when the instance is
-- terminated.
niacDeleteOnTermination :: Lens' NetworkInterfaceAttachmentChanges (Maybe Bool)
niacDeleteOnTermination =
    lens _niacDeleteOnTermination (\s a -> s { _niacDeleteOnTermination = a })

instance FromXML NetworkInterfaceAttachmentChanges where
    parseXML c = NetworkInterfaceAttachmentChanges
        <$> c .:? "attachmentId"
        <*> c .:? "deleteOnTermination"

instance ToQuery NetworkInterfaceAttachmentChanges

data RecurringChargeFrequency
    = Hourly -- ^ Hourly
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable RecurringChargeFrequency

instance FromText RecurringChargeFrequency where
    parser = match "Hourly" Hourly

instance ToText RecurringChargeFrequency where
    toText Hourly = "Hourly"

instance FromXML RecurringChargeFrequency where
    parseXML = fromXMLText "RecurringChargeFrequency"

instance ToQuery RecurringChargeFrequency

data DhcpOptions = DhcpOptions
    { _doDhcpConfigurations :: [DhcpConfiguration]
    , _doDhcpOptionsId      :: Maybe Text
    , _doTags               :: [Tag]
    } deriving (Eq, Show, Generic)

-- | 'DhcpOptions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'doDhcpConfigurations' @::@ ['DhcpConfiguration']
--
-- * 'doDhcpOptionsId' @::@ 'Maybe' 'Text'
--
-- * 'doTags' @::@ ['Tag']
--
dhcpOptions :: DhcpOptions
dhcpOptions = DhcpOptions
    { _doDhcpOptionsId      = Nothing
    , _doDhcpConfigurations = mempty
    , _doTags               = mempty
    }

-- | One or more DHCP options in the set.
doDhcpConfigurations :: Lens' DhcpOptions [DhcpConfiguration]
doDhcpConfigurations =
    lens _doDhcpConfigurations (\s a -> s { _doDhcpConfigurations = a })

-- | The ID of the set of DHCP options.
doDhcpOptionsId :: Lens' DhcpOptions (Maybe Text)
doDhcpOptionsId = lens _doDhcpOptionsId (\s a -> s { _doDhcpOptionsId = a })

-- | Any tags assigned to the DHCP options set.
doTags :: Lens' DhcpOptions [Tag]
doTags = lens _doTags (\s a -> s { _doTags = a })

instance FromXML DhcpOptions where
    parseXML c = DhcpOptions
        <$> c .: "dhcpConfigurationSet"
        <*> c .:? "dhcpOptionsId"
        <*> c .: "tagSet"

instance ToQuery DhcpOptions

data InstanceNetworkInterfaceSpecification = InstanceNetworkInterfaceSpecification
    { _inisAssociatePublicIpAddress       :: Maybe Bool
    , _inisDeleteOnTermination            :: Maybe Bool
    , _inisDescription                    :: Maybe Text
    , _inisDeviceIndex                    :: Maybe Int
    , _inisGroups                         :: [Text]
    , _inisNetworkInterfaceId             :: Maybe Text
    , _inisPrivateIpAddress               :: Maybe Text
    , _inisPrivateIpAddresses             :: [PrivateIpAddressSpecification]
    , _inisSecondaryPrivateIpAddressCount :: Maybe Int
    , _inisSubnetId                       :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'InstanceNetworkInterfaceSpecification' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'inisAssociatePublicIpAddress' @::@ 'Maybe' 'Bool'
--
-- * 'inisDeleteOnTermination' @::@ 'Maybe' 'Bool'
--
-- * 'inisDescription' @::@ 'Maybe' 'Text'
--
-- * 'inisDeviceIndex' @::@ 'Maybe' 'Int'
--
-- * 'inisGroups' @::@ ['Text']
--
-- * 'inisNetworkInterfaceId' @::@ 'Maybe' 'Text'
--
-- * 'inisPrivateIpAddress' @::@ 'Maybe' 'Text'
--
-- * 'inisPrivateIpAddresses' @::@ ['PrivateIpAddressSpecification']
--
-- * 'inisSecondaryPrivateIpAddressCount' @::@ 'Maybe' 'Int'
--
-- * 'inisSubnetId' @::@ 'Maybe' 'Text'
--
instanceNetworkInterfaceSpecification :: InstanceNetworkInterfaceSpecification
instanceNetworkInterfaceSpecification = InstanceNetworkInterfaceSpecification
    { _inisNetworkInterfaceId             = Nothing
    , _inisDeviceIndex                    = Nothing
    , _inisSubnetId                       = Nothing
    , _inisDescription                    = Nothing
    , _inisPrivateIpAddress               = Nothing
    , _inisGroups                         = mempty
    , _inisDeleteOnTermination            = Nothing
    , _inisPrivateIpAddresses             = mempty
    , _inisSecondaryPrivateIpAddressCount = Nothing
    , _inisAssociatePublicIpAddress       = Nothing
    }

-- | Indicates whether to assign a public IP address to an instance you launch
-- in a VPC. The public IP address can only be assigned to a network
-- interface for eth0, and can only be assigned to a new network interface,
-- not an existing one. You cannot specify more than one network interface
-- in the request. If luanching into a default subnet, the default value is
-- true.
inisAssociatePublicIpAddress :: Lens' InstanceNetworkInterfaceSpecification (Maybe Bool)
inisAssociatePublicIpAddress =
    lens _inisAssociatePublicIpAddress
        (\s a -> s { _inisAssociatePublicIpAddress = a })

-- | If set to true, the interface is deleted when the instance is terminated.
-- You can specify true only if creating a new network interface when
-- launching an instance.
inisDeleteOnTermination :: Lens' InstanceNetworkInterfaceSpecification (Maybe Bool)
inisDeleteOnTermination =
    lens _inisDeleteOnTermination (\s a -> s { _inisDeleteOnTermination = a })

-- | The description of the network interface. Applies only if creating a
-- network interface when launching an instance.
inisDescription :: Lens' InstanceNetworkInterfaceSpecification (Maybe Text)
inisDescription = lens _inisDescription (\s a -> s { _inisDescription = a })

-- | The index of the device on the instance for the network interface
-- attachment. If you are specifying a network interface in a RunInstances
-- request, you must provide the device index.
inisDeviceIndex :: Lens' InstanceNetworkInterfaceSpecification (Maybe Int)
inisDeviceIndex = lens _inisDeviceIndex (\s a -> s { _inisDeviceIndex = a })

-- | The IDs of the security groups for the network interface. Applies only if
-- creating a network interface when launching an instance.
inisGroups :: Lens' InstanceNetworkInterfaceSpecification [Text]
inisGroups = lens _inisGroups (\s a -> s { _inisGroups = a })

-- | The ID of the network interface.
inisNetworkInterfaceId :: Lens' InstanceNetworkInterfaceSpecification (Maybe Text)
inisNetworkInterfaceId =
    lens _inisNetworkInterfaceId (\s a -> s { _inisNetworkInterfaceId = a })

-- | The private IP address of the network interface. Applies only if creating
-- a network interface when launching an instance.
inisPrivateIpAddress :: Lens' InstanceNetworkInterfaceSpecification (Maybe Text)
inisPrivateIpAddress =
    lens _inisPrivateIpAddress (\s a -> s { _inisPrivateIpAddress = a })

-- | One or more private IP addresses to assign to the network interface. Only
-- one private IP address can be designated as primary.
inisPrivateIpAddresses :: Lens' InstanceNetworkInterfaceSpecification [PrivateIpAddressSpecification]
inisPrivateIpAddresses =
    lens _inisPrivateIpAddresses (\s a -> s { _inisPrivateIpAddresses = a })

-- | The number of secondary private IP addresses. You can't specify this
-- option and specify more than one private IP address using the private IP
-- addresses option.
inisSecondaryPrivateIpAddressCount :: Lens' InstanceNetworkInterfaceSpecification (Maybe Int)
inisSecondaryPrivateIpAddressCount =
    lens _inisSecondaryPrivateIpAddressCount
        (\s a -> s { _inisSecondaryPrivateIpAddressCount = a })

-- | The ID of the subnet associated with the network string. Applies only if
-- creating a network interface when launching an instance.
inisSubnetId :: Lens' InstanceNetworkInterfaceSpecification (Maybe Text)
inisSubnetId = lens _inisSubnetId (\s a -> s { _inisSubnetId = a })

instance FromXML InstanceNetworkInterfaceSpecification where
    parseXML c = InstanceNetworkInterfaceSpecification
        <$> c .:? "associatePublicIpAddress"
        <*> c .:? "deleteOnTermination"
        <*> c .:? "description"
        <*> c .:? "deviceIndex"
        <*> c .: "SecurityGroupId"
        <*> c .:? "networkInterfaceId"
        <*> c .:? "privateIpAddress"
        <*> c .: "privateIpAddressesSet"
        <*> c .:? "secondaryPrivateIpAddressCount"
        <*> c .:? "subnetId"

instance ToQuery InstanceNetworkInterfaceSpecification

data VolumeState
    = VSAvailable -- ^ available
    | VSCreating  -- ^ creating
    | VSDeleted   -- ^ deleted
    | VSDeleting  -- ^ deleting
    | VSError     -- ^ error
    | VSInUse     -- ^ in-use
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable VolumeState

instance FromText VolumeState where
    parser = match "available" VSAvailable
         <|> match "creating"  VSCreating
         <|> match "deleted"   VSDeleted
         <|> match "deleting"  VSDeleting
         <|> match "error"     VSError
         <|> match "in-use"    VSInUse

instance ToText VolumeState where
    toText = \case
        VSAvailable -> "available"
        VSCreating  -> "creating"
        VSDeleted   -> "deleted"
        VSDeleting  -> "deleting"
        VSError     -> "error"
        VSInUse     -> "in-use"

instance FromXML VolumeState where
    parseXML = fromXMLText "VolumeState"

instance ToQuery VolumeState

newtype AttributeValue = AttributeValue
    { _avValue :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'AttributeValue' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'avValue' @::@ 'Maybe' 'Text'
--
attributeValue :: AttributeValue
attributeValue = AttributeValue
    { _avValue = Nothing
    }

avValue :: Lens' AttributeValue (Maybe Text)
avValue = lens _avValue (\s a -> s { _avValue = a })

instance FromXML AttributeValue where
    parseXML c = AttributeValue
        <$> c .:? "value"

instance ToQuery AttributeValue

data PrivateIpAddressSpecification = PrivateIpAddressSpecification
    { _piasPrimary          :: Maybe Bool
    , _piasPrivateIpAddress :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'PrivateIpAddressSpecification' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'piasPrimary' @::@ 'Maybe' 'Bool'
--
-- * 'piasPrivateIpAddress' @::@ 'Text'
--
privateIpAddressSpecification :: Text -- ^ 'piasPrivateIpAddress'
                              -> PrivateIpAddressSpecification
privateIpAddressSpecification p1 = PrivateIpAddressSpecification
    { _piasPrivateIpAddress = p1
    , _piasPrimary          = Nothing
    }

-- | Indicates whether the private IP address is the primary private IP
-- address. Only one IP address can be designated as primary.
piasPrimary :: Lens' PrivateIpAddressSpecification (Maybe Bool)
piasPrimary = lens _piasPrimary (\s a -> s { _piasPrimary = a })

-- | The private IP addresses.
piasPrivateIpAddress :: Lens' PrivateIpAddressSpecification Text
piasPrivateIpAddress =
    lens _piasPrivateIpAddress (\s a -> s { _piasPrivateIpAddress = a })

instance FromXML PrivateIpAddressSpecification where
    parseXML c = PrivateIpAddressSpecification
        <$> c .:? "primary"
        <*> c .: "privateIpAddress"

instance ToQuery PrivateIpAddressSpecification

data Image = Image
    { _iArchitecture        :: Text
    , _iBlockDeviceMappings :: [BlockDeviceMapping]
    , _iDescription         :: Maybe Text
    , _iHypervisor          :: Text
    , _iImageId             :: Text
    , _iImageLocation       :: Text
    , _iImageOwnerAlias     :: Maybe Text
    , _iImageType           :: Text
    , _iKernelId            :: Maybe Text
    , _iName                :: Text
    , _iOwnerId             :: Text
    , _iPlatform            :: Maybe Text
    , _iProductCodes        :: [ProductCode]
    , _iPublic              :: Bool
    , _iRamdiskId           :: Maybe Text
    , _iRootDeviceName      :: Maybe Text
    , _iRootDeviceType      :: Text
    , _iSriovNetSupport     :: Maybe Text
    , _iState               :: Text
    , _iStateReason         :: Maybe StateReason
    , _iTags                :: [Tag]
    , _iVirtualizationType  :: Text
    } deriving (Eq, Show, Generic)

-- | 'Image' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iArchitecture' @::@ 'Text'
--
-- * 'iBlockDeviceMappings' @::@ ['BlockDeviceMapping']
--
-- * 'iDescription' @::@ 'Maybe' 'Text'
--
-- * 'iHypervisor' @::@ 'Text'
--
-- * 'iImageId' @::@ 'Text'
--
-- * 'iImageLocation' @::@ 'Text'
--
-- * 'iImageOwnerAlias' @::@ 'Maybe' 'Text'
--
-- * 'iImageType' @::@ 'Text'
--
-- * 'iKernelId' @::@ 'Maybe' 'Text'
--
-- * 'iName' @::@ 'Text'
--
-- * 'iOwnerId' @::@ 'Text'
--
-- * 'iPlatform' @::@ 'Maybe' 'Text'
--
-- * 'iProductCodes' @::@ ['ProductCode']
--
-- * 'iPublic' @::@ 'Bool'
--
-- * 'iRamdiskId' @::@ 'Maybe' 'Text'
--
-- * 'iRootDeviceName' @::@ 'Maybe' 'Text'
--
-- * 'iRootDeviceType' @::@ 'Text'
--
-- * 'iSriovNetSupport' @::@ 'Maybe' 'Text'
--
-- * 'iState' @::@ 'Text'
--
-- * 'iStateReason' @::@ 'Maybe' 'StateReason'
--
-- * 'iTags' @::@ ['Tag']
--
-- * 'iVirtualizationType' @::@ 'Text'
--
image :: Text -- ^ 'iImageId'
      -> Text -- ^ 'iImageLocation'
      -> Text -- ^ 'iState'
      -> Text -- ^ 'iOwnerId'
      -> Bool -- ^ 'iPublic'
      -> Text -- ^ 'iArchitecture'
      -> Text -- ^ 'iImageType'
      -> Text -- ^ 'iName'
      -> Text -- ^ 'iRootDeviceType'
      -> Text -- ^ 'iVirtualizationType'
      -> Text -- ^ 'iHypervisor'
      -> Image
image p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 = Image
    { _iImageId             = p1
    , _iImageLocation       = p2
    , _iState               = p3
    , _iOwnerId             = p4
    , _iPublic              = p5
    , _iArchitecture        = p6
    , _iImageType           = p7
    , _iName                = p8
    , _iRootDeviceType      = p9
    , _iVirtualizationType  = p10
    , _iHypervisor          = p11
    , _iProductCodes        = mempty
    , _iKernelId            = Nothing
    , _iRamdiskId           = Nothing
    , _iPlatform            = Nothing
    , _iSriovNetSupport     = Nothing
    , _iStateReason         = Nothing
    , _iImageOwnerAlias     = Nothing
    , _iDescription         = Nothing
    , _iRootDeviceName      = Nothing
    , _iBlockDeviceMappings = mempty
    , _iTags                = mempty
    }

-- | The architecture of the image.
iArchitecture :: Lens' Image Text
iArchitecture = lens _iArchitecture (\s a -> s { _iArchitecture = a })

-- | Any block device mapping entries.
iBlockDeviceMappings :: Lens' Image [BlockDeviceMapping]
iBlockDeviceMappings =
    lens _iBlockDeviceMappings (\s a -> s { _iBlockDeviceMappings = a })

-- | The description of the AMI that was provided during image creation.
iDescription :: Lens' Image (Maybe Text)
iDescription = lens _iDescription (\s a -> s { _iDescription = a })

-- | The hypervisor type of the image.
iHypervisor :: Lens' Image Text
iHypervisor = lens _iHypervisor (\s a -> s { _iHypervisor = a })

-- | The ID of the AMI.
iImageId :: Lens' Image Text
iImageId = lens _iImageId (\s a -> s { _iImageId = a })

-- | The location of the AMI.
iImageLocation :: Lens' Image Text
iImageLocation = lens _iImageLocation (\s a -> s { _iImageLocation = a })

-- | The AWS account alias (for example, amazon, self) or the AWS account ID
-- of the AMI owner.
iImageOwnerAlias :: Lens' Image (Maybe Text)
iImageOwnerAlias = lens _iImageOwnerAlias (\s a -> s { _iImageOwnerAlias = a })

-- | The type of image.
iImageType :: Lens' Image Text
iImageType = lens _iImageType (\s a -> s { _iImageType = a })

-- | The kernel associated with the image, if any. Only applicable for machine
-- images.
iKernelId :: Lens' Image (Maybe Text)
iKernelId = lens _iKernelId (\s a -> s { _iKernelId = a })

-- | The name of the AMI that was provided during image creation.
iName :: Lens' Image Text
iName = lens _iName (\s a -> s { _iName = a })

-- | The AWS account ID of the image owner.
iOwnerId :: Lens' Image Text
iOwnerId = lens _iOwnerId (\s a -> s { _iOwnerId = a })

-- | The value is Windows for Windows AMIs; otherwise blank.
iPlatform :: Lens' Image (Maybe Text)
iPlatform = lens _iPlatform (\s a -> s { _iPlatform = a })

-- | Any product codes associated with the AMI.
iProductCodes :: Lens' Image [ProductCode]
iProductCodes = lens _iProductCodes (\s a -> s { _iProductCodes = a })

-- | Indicates whether the image has public launch permissions. The value is
-- true if this image has public launch permissions or false if it has only
-- implicit and explicit launch permissions.
iPublic :: Lens' Image Bool
iPublic = lens _iPublic (\s a -> s { _iPublic = a })

-- | The RAM disk associated with the image, if any. Only applicable for
-- machine images.
iRamdiskId :: Lens' Image (Maybe Text)
iRamdiskId = lens _iRamdiskId (\s a -> s { _iRamdiskId = a })

-- | The device name of the root device (for example, /dev/sda1 or xvda).
iRootDeviceName :: Lens' Image (Maybe Text)
iRootDeviceName = lens _iRootDeviceName (\s a -> s { _iRootDeviceName = a })

-- | The type of root device used by the AMI. The AMI can use an Amazon EBS
-- volume or an instance store volume.
iRootDeviceType :: Lens' Image Text
iRootDeviceType = lens _iRootDeviceType (\s a -> s { _iRootDeviceType = a })

-- | Specifies whether enhanced networking is enabled.
iSriovNetSupport :: Lens' Image (Maybe Text)
iSriovNetSupport = lens _iSriovNetSupport (\s a -> s { _iSriovNetSupport = a })

-- | The current state of the AMI. If the state is available, the image is
-- successfully registered and can be used to launch an instance.
iState :: Lens' Image Text
iState = lens _iState (\s a -> s { _iState = a })

-- | The reason for the state change.
iStateReason :: Lens' Image (Maybe StateReason)
iStateReason = lens _iStateReason (\s a -> s { _iStateReason = a })

-- | Any tags assigned to the image.
iTags :: Lens' Image [Tag]
iTags = lens _iTags (\s a -> s { _iTags = a })

-- | The type of virtualization of the AMI.
iVirtualizationType :: Lens' Image Text
iVirtualizationType =
    lens _iVirtualizationType (\s a -> s { _iVirtualizationType = a })

instance FromXML Image where
    parseXML c = Image
        <$> c .: "architecture"
        <*> c .: "blockDeviceMapping"
        <*> c .:? "description"
        <*> c .: "hypervisor"
        <*> c .: "imageId"
        <*> c .: "imageLocation"
        <*> c .:? "imageOwnerAlias"
        <*> c .: "imageType"
        <*> c .:? "kernelId"
        <*> c .: "name"
        <*> c .: "imageOwnerId"
        <*> c .:? "platform"
        <*> c .: "productCodes"
        <*> c .: "isPublic"
        <*> c .:? "ramdiskId"
        <*> c .:? "rootDeviceName"
        <*> c .: "rootDeviceType"
        <*> c .:? "sriovNetSupport"
        <*> c .: "imageState"
        <*> c .:? "stateReason"
        <*> c .: "tagSet"
        <*> c .: "virtualizationType"

instance ToQuery Image

data DhcpConfiguration = DhcpConfiguration
    { _dcKey    :: Maybe Text
    , _dcValues :: [AttributeValue]
    } deriving (Eq, Show, Generic)

-- | 'DhcpConfiguration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcKey' @::@ 'Maybe' 'Text'
--
-- * 'dcValues' @::@ ['AttributeValue']
--
dhcpConfiguration :: DhcpConfiguration
dhcpConfiguration = DhcpConfiguration
    { _dcKey    = Nothing
    , _dcValues = mempty
    }

-- | The name of a DHCP option.
dcKey :: Lens' DhcpConfiguration (Maybe Text)
dcKey = lens _dcKey (\s a -> s { _dcKey = a })

-- | One or more values for the DHCP option.
dcValues :: Lens' DhcpConfiguration [AttributeValue]
dcValues = lens _dcValues (\s a -> s { _dcValues = a })

instance FromXML DhcpConfiguration where
    parseXML c = DhcpConfiguration
        <$> c .:? "key"
        <*> c .: "valueSet"

instance ToQuery DhcpConfiguration

data Tag = Tag
    { _tagKey   :: Text
    , _tagValue :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'Tag' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tagKey' @::@ 'Text'
--
-- * 'tagValue' @::@ 'Text'
--
tag :: Text -- ^ 'tagKey'
    -> Text -- ^ 'tagValue'
    -> Tag
tag p1 p2 = Tag
    { _tagKey   = p1
    , _tagValue = p2
    }

-- | The key of the tag. Constraints: Tag keys are case-sensitive and accept a
-- maximum of 127 Unicode characters. May not begin with aws:.
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\s a -> s { _tagKey = a })

-- | The value of the tag. Constraints: Tag values are case-sensitive and
-- accept a maximum of 255 Unicode characters.
tagValue :: Lens' Tag Text
tagValue = lens _tagValue (\s a -> s { _tagValue = a })

instance FromXML Tag where
    parseXML c = Tag
        <$> c .: "key"
        <*> c .: "value"

instance ToQuery Tag

data AccountAttributeName
    = DefaultVpc         -- ^ default-vpc
    | SupportedPlatforms -- ^ supported-platforms
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable AccountAttributeName

instance FromText AccountAttributeName where
    parser = match "default-vpc"         DefaultVpc
         <|> match "supported-platforms" SupportedPlatforms

instance ToText AccountAttributeName where
    toText = \case
        DefaultVpc         -> "default-vpc"
        SupportedPlatforms -> "supported-platforms"

instance FromXML AccountAttributeName where
    parseXML = fromXMLText "AccountAttributeName"

instance ToQuery AccountAttributeName

data NetworkInterfaceAttachment = NetworkInterfaceAttachment
    { _niaAttachTime          :: Maybe RFC822
    , _niaAttachmentId        :: Maybe Text
    , _niaDeleteOnTermination :: Maybe Bool
    , _niaDeviceIndex         :: Maybe Int
    , _niaInstanceId          :: Maybe Text
    , _niaInstanceOwnerId     :: Maybe Text
    , _niaStatus              :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'NetworkInterfaceAttachment' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'niaAttachTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'niaAttachmentId' @::@ 'Maybe' 'Text'
--
-- * 'niaDeleteOnTermination' @::@ 'Maybe' 'Bool'
--
-- * 'niaDeviceIndex' @::@ 'Maybe' 'Int'
--
-- * 'niaInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'niaInstanceOwnerId' @::@ 'Maybe' 'Text'
--
-- * 'niaStatus' @::@ 'Maybe' 'Text'
--
networkInterfaceAttachment :: NetworkInterfaceAttachment
networkInterfaceAttachment = NetworkInterfaceAttachment
    { _niaAttachmentId        = Nothing
    , _niaInstanceId          = Nothing
    , _niaInstanceOwnerId     = Nothing
    , _niaDeviceIndex         = Nothing
    , _niaStatus              = Nothing
    , _niaAttachTime          = Nothing
    , _niaDeleteOnTermination = Nothing
    }

-- | The timestamp indicating when the attachment initiated.
niaAttachTime :: Lens' NetworkInterfaceAttachment (Maybe UTCTime)
niaAttachTime = lens _niaAttachTime (\s a -> s { _niaAttachTime = a })
    . mapping _Time

-- | The ID of the network interface attachment.
niaAttachmentId :: Lens' NetworkInterfaceAttachment (Maybe Text)
niaAttachmentId = lens _niaAttachmentId (\s a -> s { _niaAttachmentId = a })

-- | Indicates whether the network interface is deleted when the instance is
-- terminated.
niaDeleteOnTermination :: Lens' NetworkInterfaceAttachment (Maybe Bool)
niaDeleteOnTermination =
    lens _niaDeleteOnTermination (\s a -> s { _niaDeleteOnTermination = a })

-- | The device index of the network interface attachment on the instance.
niaDeviceIndex :: Lens' NetworkInterfaceAttachment (Maybe Int)
niaDeviceIndex = lens _niaDeviceIndex (\s a -> s { _niaDeviceIndex = a })

-- | The ID of the instance.
niaInstanceId :: Lens' NetworkInterfaceAttachment (Maybe Text)
niaInstanceId = lens _niaInstanceId (\s a -> s { _niaInstanceId = a })

-- | The AWS account ID of the owner of the instance.
niaInstanceOwnerId :: Lens' NetworkInterfaceAttachment (Maybe Text)
niaInstanceOwnerId =
    lens _niaInstanceOwnerId (\s a -> s { _niaInstanceOwnerId = a })

-- | The attachment state.
niaStatus :: Lens' NetworkInterfaceAttachment (Maybe Text)
niaStatus = lens _niaStatus (\s a -> s { _niaStatus = a })

instance FromXML NetworkInterfaceAttachment where
    parseXML c = NetworkInterfaceAttachment
        <$> c .:? "attachTime"
        <*> c .:? "attachmentId"
        <*> c .:? "deleteOnTermination"
        <*> c .:? "deviceIndex"
        <*> c .:? "instanceId"
        <*> c .:? "instanceOwnerId"
        <*> c .:? "status"

instance ToQuery NetworkInterfaceAttachment

newtype RunInstancesMonitoringEnabled = RunInstancesMonitoringEnabled
    { _rimeEnabled :: Bool
    } deriving (Eq, Ord, Show, Generic, Enum)

-- | 'RunInstancesMonitoringEnabled' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rimeEnabled' @::@ 'Bool'
--
runInstancesMonitoringEnabled :: Bool -- ^ 'rimeEnabled'
                              -> RunInstancesMonitoringEnabled
runInstancesMonitoringEnabled p1 = RunInstancesMonitoringEnabled
    { _rimeEnabled = p1
    }

-- | Indicates whether monitoring is enabled for the instance.
rimeEnabled :: Lens' RunInstancesMonitoringEnabled Bool
rimeEnabled = lens _rimeEnabled (\s a -> s { _rimeEnabled = a })

instance FromXML RunInstancesMonitoringEnabled where
    parseXML c = RunInstancesMonitoringEnabled
        <$> c .: "enabled"

instance ToQuery RunInstancesMonitoringEnabled

data VolumeStatusInfo = VolumeStatusInfo
    { _vsiDetails :: [VolumeStatusDetails]
    , _vsiStatus  :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'VolumeStatusInfo' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vsiDetails' @::@ ['VolumeStatusDetails']
--
-- * 'vsiStatus' @::@ 'Maybe' 'Text'
--
volumeStatusInfo :: VolumeStatusInfo
volumeStatusInfo = VolumeStatusInfo
    { _vsiStatus  = Nothing
    , _vsiDetails = mempty
    }

-- | The details of the volume status.
vsiDetails :: Lens' VolumeStatusInfo [VolumeStatusDetails]
vsiDetails = lens _vsiDetails (\s a -> s { _vsiDetails = a })

-- | The status of the volume.
vsiStatus :: Lens' VolumeStatusInfo (Maybe Text)
vsiStatus = lens _vsiStatus (\s a -> s { _vsiStatus = a })

instance FromXML VolumeStatusInfo where
    parseXML c = VolumeStatusInfo
        <$> c .: "details"
        <*> c .:? "status"

instance ToQuery VolumeStatusInfo

data NetworkInterfaceAssociation = NetworkInterfaceAssociation
    { _niaAllocationId  :: Maybe Text
    , _niaAssociationId :: Maybe Text
    , _niaIpOwnerId     :: Maybe Text
    , _niaPublicDnsName :: Maybe Text
    , _niaPublicIp      :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'NetworkInterfaceAssociation' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'niaAllocationId' @::@ 'Maybe' 'Text'
--
-- * 'niaAssociationId' @::@ 'Maybe' 'Text'
--
-- * 'niaIpOwnerId' @::@ 'Maybe' 'Text'
--
-- * 'niaPublicDnsName' @::@ 'Maybe' 'Text'
--
-- * 'niaPublicIp' @::@ 'Maybe' 'Text'
--
networkInterfaceAssociation :: NetworkInterfaceAssociation
networkInterfaceAssociation = NetworkInterfaceAssociation
    { _niaPublicIp      = Nothing
    , _niaPublicDnsName = Nothing
    , _niaIpOwnerId     = Nothing
    , _niaAllocationId  = Nothing
    , _niaAssociationId = Nothing
    }

-- | The allocation ID.
niaAllocationId :: Lens' NetworkInterfaceAssociation (Maybe Text)
niaAllocationId = lens _niaAllocationId (\s a -> s { _niaAllocationId = a })

-- | The association ID.
niaAssociationId :: Lens' NetworkInterfaceAssociation (Maybe Text)
niaAssociationId = lens _niaAssociationId (\s a -> s { _niaAssociationId = a })

-- | The ID of the Elastic IP address owner.
niaIpOwnerId :: Lens' NetworkInterfaceAssociation (Maybe Text)
niaIpOwnerId = lens _niaIpOwnerId (\s a -> s { _niaIpOwnerId = a })

-- | The public DNS name.
niaPublicDnsName :: Lens' NetworkInterfaceAssociation (Maybe Text)
niaPublicDnsName = lens _niaPublicDnsName (\s a -> s { _niaPublicDnsName = a })

-- | The address of the Elastic IP address bound to the network interface.
niaPublicIp :: Lens' NetworkInterfaceAssociation (Maybe Text)
niaPublicIp = lens _niaPublicIp (\s a -> s { _niaPublicIp = a })

instance FromXML NetworkInterfaceAssociation where
    parseXML c = NetworkInterfaceAssociation
        <$> c .:? "allocationId"
        <*> c .:? "associationId"
        <*> c .:? "ipOwnerId"
        <*> c .:? "publicDnsName"
        <*> c .:? "publicIp"

instance ToQuery NetworkInterfaceAssociation

data CreateVolumePermissionModifications = CreateVolumePermissionModifications
    { _cvpmAdd    :: [CreateVolumePermission]
    , _cvpmRemove :: [CreateVolumePermission]
    } deriving (Eq, Show, Generic)

-- | 'CreateVolumePermissionModifications' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cvpmAdd' @::@ ['CreateVolumePermission']
--
-- * 'cvpmRemove' @::@ ['CreateVolumePermission']
--
createVolumePermissionModifications :: CreateVolumePermissionModifications
createVolumePermissionModifications = CreateVolumePermissionModifications
    { _cvpmAdd    = mempty
    , _cvpmRemove = mempty
    }

-- | Adds a specific AWS account ID or group to a volume's list of create
-- volume permissions.
cvpmAdd :: Lens' CreateVolumePermissionModifications [CreateVolumePermission]
cvpmAdd = lens _cvpmAdd (\s a -> s { _cvpmAdd = a })

-- | Removes a specific AWS account ID or group from a volume's list of create
-- volume permissions.
cvpmRemove :: Lens' CreateVolumePermissionModifications [CreateVolumePermission]
cvpmRemove = lens _cvpmRemove (\s a -> s { _cvpmRemove = a })

instance FromXML CreateVolumePermissionModifications where
    parseXML c = CreateVolumePermissionModifications
        <$> c .: "Add"
        <*> c .: "Remove"

instance ToQuery CreateVolumePermissionModifications

data VpcState
    = VpcStateAvailable -- ^ available
    | VpcStatePending   -- ^ pending
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable VpcState

instance FromText VpcState where
    parser = match "available" VpcStateAvailable
         <|> match "pending"   VpcStatePending

instance ToText VpcState where
    toText = \case
        VpcStateAvailable -> "available"
        VpcStatePending   -> "pending"

instance FromXML VpcState where
    parseXML = fromXMLText "VpcState"

instance ToQuery VpcState

data ResourceType
    = RTCustomerGateway      -- ^ customer-gateway
    | RTDhcpOptions          -- ^ dhcp-options
    | RTImage                -- ^ image
    | RTInstance'            -- ^ instance
    | RTInternetGateway      -- ^ internet-gateway
    | RTNetworkAcl           -- ^ network-acl
    | RTNetworkInterface     -- ^ network-interface
    | RTReservedInstances    -- ^ reserved-instances
    | RTRouteTable           -- ^ route-table
    | RTSecurityGroup        -- ^ security-group
    | RTSnapshot             -- ^ snapshot
    | RTSpotInstancesRequest -- ^ spot-instances-request
    | RTSubnet               -- ^ subnet
    | RTVolume               -- ^ volume
    | RTVpc                  -- ^ vpc
    | RTVpnConnection        -- ^ vpn-connection
    | RTVpnGateway           -- ^ vpn-gateway
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable ResourceType

instance FromText ResourceType where
    parser = match "customer-gateway"       RTCustomerGateway
         <|> match "dhcp-options"           RTDhcpOptions
         <|> match "image"                  RTImage
         <|> match "instance"               RTInstance'
         <|> match "internet-gateway"       RTInternetGateway
         <|> match "network-acl"            RTNetworkAcl
         <|> match "network-interface"      RTNetworkInterface
         <|> match "reserved-instances"     RTReservedInstances
         <|> match "route-table"            RTRouteTable
         <|> match "security-group"         RTSecurityGroup
         <|> match "snapshot"               RTSnapshot
         <|> match "spot-instances-request" RTSpotInstancesRequest
         <|> match "subnet"                 RTSubnet
         <|> match "volume"                 RTVolume
         <|> match "vpc"                    RTVpc
         <|> match "vpn-connection"         RTVpnConnection
         <|> match "vpn-gateway"            RTVpnGateway

instance ToText ResourceType where
    toText = \case
        RTCustomerGateway      -> "customer-gateway"
        RTDhcpOptions          -> "dhcp-options"
        RTImage                -> "image"
        RTInstance'            -> "instance"
        RTInternetGateway      -> "internet-gateway"
        RTNetworkAcl           -> "network-acl"
        RTNetworkInterface     -> "network-interface"
        RTReservedInstances    -> "reserved-instances"
        RTRouteTable           -> "route-table"
        RTSecurityGroup        -> "security-group"
        RTSnapshot             -> "snapshot"
        RTSpotInstancesRequest -> "spot-instances-request"
        RTSubnet               -> "subnet"
        RTVolume               -> "volume"
        RTVpc                  -> "vpc"
        RTVpnConnection        -> "vpn-connection"
        RTVpnGateway           -> "vpn-gateway"

instance FromXML ResourceType where
    parseXML = fromXMLText "ResourceType"

instance ToQuery ResourceType

data ReportStatusType
    = Impaired -- ^ impaired
    | Ok       -- ^ ok
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable ReportStatusType

instance FromText ReportStatusType where
    parser = match "impaired" Impaired
         <|> match "ok"       Ok

instance ToText ReportStatusType where
    toText = \case
        Impaired -> "impaired"
        Ok       -> "ok"

instance FromXML ReportStatusType where
    parseXML = fromXMLText "ReportStatusType"

instance ToQuery ReportStatusType

data CurrencyCodeValues
    = Usd -- ^ USD
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable CurrencyCodeValues

instance FromText CurrencyCodeValues where
    parser = match "USD" Usd

instance ToText CurrencyCodeValues where
    toText Usd = "USD"

instance FromXML CurrencyCodeValues where
    parseXML = fromXMLText "CurrencyCodeValues"

instance ToQuery CurrencyCodeValues

data IcmpTypeCode = IcmpTypeCode
    { _itcCode :: Maybe Int
    , _itcType :: Maybe Int
    } deriving (Eq, Ord, Show, Generic)

-- | 'IcmpTypeCode' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'itcCode' @::@ 'Maybe' 'Int'
--
-- * 'itcType' @::@ 'Maybe' 'Int'
--
icmpTypeCode :: IcmpTypeCode
icmpTypeCode = IcmpTypeCode
    { _itcType = Nothing
    , _itcCode = Nothing
    }

-- | The ICMP type. A value of -1 means all types.
itcCode :: Lens' IcmpTypeCode (Maybe Int)
itcCode = lens _itcCode (\s a -> s { _itcCode = a })

-- | The ICMP code. A value of -1 means all codes for the specified ICMP type.
itcType :: Lens' IcmpTypeCode (Maybe Int)
itcType = lens _itcType (\s a -> s { _itcType = a })

instance FromXML IcmpTypeCode where
    parseXML c = IcmpTypeCode
        <$> c .:? "code"
        <*> c .:? "type"

instance ToQuery IcmpTypeCode

data InstanceCount = InstanceCount
    { _icInstanceCount :: Maybe Int
    , _icState         :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'InstanceCount' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'icInstanceCount' @::@ 'Maybe' 'Int'
--
-- * 'icState' @::@ 'Maybe' 'Text'
--
instanceCount :: InstanceCount
instanceCount = InstanceCount
    { _icState         = Nothing
    , _icInstanceCount = Nothing
    }

-- | he number of listed Reserved Instances in the state specified by the
-- state.
icInstanceCount :: Lens' InstanceCount (Maybe Int)
icInstanceCount = lens _icInstanceCount (\s a -> s { _icInstanceCount = a })

-- | The states of the listed Reserved Instances.
icState :: Lens' InstanceCount (Maybe Text)
icState = lens _icState (\s a -> s { _icState = a })

instance FromXML InstanceCount where
    parseXML c = InstanceCount
        <$> c .:? "instanceCount"
        <*> c .:? "state"

instance ToQuery InstanceCount

data ExportToS3Task = ExportToS3Task
    { _etstContainerFormat :: Maybe Text
    , _etstDiskImageFormat :: Maybe Text
    , _etstS3Bucket        :: Maybe Text
    , _etstS3Key           :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ExportToS3Task' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'etstContainerFormat' @::@ 'Maybe' 'Text'
--
-- * 'etstDiskImageFormat' @::@ 'Maybe' 'Text'
--
-- * 'etstS3Bucket' @::@ 'Maybe' 'Text'
--
-- * 'etstS3Key' @::@ 'Maybe' 'Text'
--
exportToS3Task :: ExportToS3Task
exportToS3Task = ExportToS3Task
    { _etstDiskImageFormat = Nothing
    , _etstContainerFormat = Nothing
    , _etstS3Bucket        = Nothing
    , _etstS3Key           = Nothing
    }

-- | The container format used to combine disk images with metadata (such as
-- OVF). If absent, only the disk image is exported.
etstContainerFormat :: Lens' ExportToS3Task (Maybe Text)
etstContainerFormat =
    lens _etstContainerFormat (\s a -> s { _etstContainerFormat = a })

-- | The format for the exported image.
etstDiskImageFormat :: Lens' ExportToS3Task (Maybe Text)
etstDiskImageFormat =
    lens _etstDiskImageFormat (\s a -> s { _etstDiskImageFormat = a })

-- | The Amazon S3 bucket for the destination image. The destination bucket
-- must exist and grant WRITE and READ_ACL permissions to the AWS account
-- vm-import-export@amazon.com.
etstS3Bucket :: Lens' ExportToS3Task (Maybe Text)
etstS3Bucket = lens _etstS3Bucket (\s a -> s { _etstS3Bucket = a })

etstS3Key :: Lens' ExportToS3Task (Maybe Text)
etstS3Key = lens _etstS3Key (\s a -> s { _etstS3Key = a })

instance FromXML ExportToS3Task where
    parseXML c = ExportToS3Task
        <$> c .:? "containerFormat"
        <*> c .:? "diskImageFormat"
        <*> c .:? "s3Bucket"
        <*> c .:? "s3Key"

instance ToQuery ExportToS3Task

data BlockDeviceMapping = BlockDeviceMapping
    { _bdmDeviceName  :: Text
    , _bdmEbs         :: Maybe EbsBlockDevice
    , _bdmNoDevice    :: Maybe Text
    , _bdmVirtualName :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'BlockDeviceMapping' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bdmDeviceName' @::@ 'Text'
--
-- * 'bdmEbs' @::@ 'Maybe' 'EbsBlockDevice'
--
-- * 'bdmNoDevice' @::@ 'Maybe' 'Text'
--
-- * 'bdmVirtualName' @::@ 'Maybe' 'Text'
--
blockDeviceMapping :: Text -- ^ 'bdmDeviceName'
                   -> BlockDeviceMapping
blockDeviceMapping p1 = BlockDeviceMapping
    { _bdmDeviceName  = p1
    , _bdmVirtualName = Nothing
    , _bdmEbs         = Nothing
    , _bdmNoDevice    = Nothing
    }

-- | The device name exposed to the instance (for example, /dev/sdh).
bdmDeviceName :: Lens' BlockDeviceMapping Text
bdmDeviceName = lens _bdmDeviceName (\s a -> s { _bdmDeviceName = a })

-- | Parameters used to automatically set up Amazon EBS volumes when the
-- instance is launched.
bdmEbs :: Lens' BlockDeviceMapping (Maybe EbsBlockDevice)
bdmEbs = lens _bdmEbs (\s a -> s { _bdmEbs = a })

-- | Suppresses the specified device included in the block device mapping of
-- the AMI.
bdmNoDevice :: Lens' BlockDeviceMapping (Maybe Text)
bdmNoDevice = lens _bdmNoDevice (\s a -> s { _bdmNoDevice = a })

-- | The virtual device name (ephemeral[0..3]). The number of available
-- instance store volumes depends on the instance type. After you connect to
-- the instance, you must mount the volume. Constraints: For M3 instances,
-- you must specify instance store volumes in the block device mapping for
-- the instance. When you launch an M3 instance, we ignore any instance
-- store volumes specified in the block device mapping for the AMI.
bdmVirtualName :: Lens' BlockDeviceMapping (Maybe Text)
bdmVirtualName = lens _bdmVirtualName (\s a -> s { _bdmVirtualName = a })

instance FromXML BlockDeviceMapping where
    parseXML c = BlockDeviceMapping
        <$> c .: "deviceName"
        <*> c .:? "ebs"
        <*> c .:? "noDevice"
        <*> c .:? "virtualName"

instance ToQuery BlockDeviceMapping

data ConversionTask = ConversionTask
    { _ctConversionTaskId :: Text
    , _ctExpirationTime   :: Maybe Text
    , _ctImportInstance   :: Maybe ImportInstanceTaskDetails
    , _ctImportVolume     :: Maybe ImportVolumeTaskDetails
    , _ctState            :: Text
    , _ctStatusMessage    :: Maybe Text
    , _ctTags             :: [Tag]
    } deriving (Eq, Show, Generic)

-- | 'ConversionTask' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ctConversionTaskId' @::@ 'Text'
--
-- * 'ctExpirationTime' @::@ 'Maybe' 'Text'
--
-- * 'ctImportInstance' @::@ 'Maybe' 'ImportInstanceTaskDetails'
--
-- * 'ctImportVolume' @::@ 'Maybe' 'ImportVolumeTaskDetails'
--
-- * 'ctState' @::@ 'Text'
--
-- * 'ctStatusMessage' @::@ 'Maybe' 'Text'
--
-- * 'ctTags' @::@ ['Tag']
--
conversionTask :: Text -- ^ 'ctConversionTaskId'
               -> Text -- ^ 'ctState'
               -> ConversionTask
conversionTask p1 p2 = ConversionTask
    { _ctConversionTaskId = p1
    , _ctState            = p2
    , _ctExpirationTime   = Nothing
    , _ctImportInstance   = Nothing
    , _ctImportVolume     = Nothing
    , _ctStatusMessage    = Nothing
    , _ctTags             = mempty
    }

-- | The ID of the conversion task.
ctConversionTaskId :: Lens' ConversionTask Text
ctConversionTaskId =
    lens _ctConversionTaskId (\s a -> s { _ctConversionTaskId = a })

-- | The time when the task expires. If the upload isn't complete before the
-- expiration time, we automatically cancel the task.
ctExpirationTime :: Lens' ConversionTask (Maybe Text)
ctExpirationTime = lens _ctExpirationTime (\s a -> s { _ctExpirationTime = a })

-- | If the task is for importing an instance, this contains information about
-- the import instance task.
ctImportInstance :: Lens' ConversionTask (Maybe ImportInstanceTaskDetails)
ctImportInstance = lens _ctImportInstance (\s a -> s { _ctImportInstance = a })

-- | If the task is for importing a volume, this contains information about
-- the import volume task.
ctImportVolume :: Lens' ConversionTask (Maybe ImportVolumeTaskDetails)
ctImportVolume = lens _ctImportVolume (\s a -> s { _ctImportVolume = a })

-- | The state of the conversion task.
ctState :: Lens' ConversionTask Text
ctState = lens _ctState (\s a -> s { _ctState = a })

-- | The status message related to the conversion task.
ctStatusMessage :: Lens' ConversionTask (Maybe Text)
ctStatusMessage = lens _ctStatusMessage (\s a -> s { _ctStatusMessage = a })

ctTags :: Lens' ConversionTask [Tag]
ctTags = lens _ctTags (\s a -> s { _ctTags = a })

instance FromXML ConversionTask where
    parseXML c = ConversionTask
        <$> c .: "conversionTaskId"
        <*> c .:? "expirationTime"
        <*> c .:? "importInstance"
        <*> c .:? "importVolume"
        <*> c .: "state"
        <*> c .:? "statusMessage"
        <*> c .: "tagSet"

instance ToQuery ConversionTask

data AttachmentStatus
    = ASAttached  -- ^ attached
    | ASAttaching -- ^ attaching
    | ASDetached  -- ^ detached
    | ASDetaching -- ^ detaching
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable AttachmentStatus

instance FromText AttachmentStatus where
    parser = match "attached"  ASAttached
         <|> match "attaching" ASAttaching
         <|> match "detached"  ASDetached
         <|> match "detaching" ASDetaching

instance ToText AttachmentStatus where
    toText = \case
        ASAttached  -> "attached"
        ASAttaching -> "attaching"
        ASDetached  -> "detached"
        ASDetaching -> "detaching"

instance FromXML AttachmentStatus where
    parseXML = fromXMLText "AttachmentStatus"

instance ToQuery AttachmentStatus

data RouteOrigin
    = OriginCreateRoute               -- ^ CreateRoute
    | OriginCreateRouteTable          -- ^ CreateRouteTable
    | OriginEnableVgwRoutePropagation -- ^ EnableVgwRoutePropagation
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable RouteOrigin

instance FromText RouteOrigin where
    parser = match "CreateRoute"               OriginCreateRoute
         <|> match "CreateRouteTable"          OriginCreateRouteTable
         <|> match "EnableVgwRoutePropagation" OriginEnableVgwRoutePropagation

instance ToText RouteOrigin where
    toText = \case
        OriginCreateRoute               -> "CreateRoute"
        OriginCreateRouteTable          -> "CreateRouteTable"
        OriginEnableVgwRoutePropagation -> "EnableVgwRoutePropagation"

instance FromXML RouteOrigin where
    parseXML = fromXMLText "RouteOrigin"

instance ToQuery RouteOrigin

data ListingState
    = LSAvailable -- ^ available
    | LSCancelled -- ^ cancelled
    | LSPending   -- ^ pending
    | LSSold      -- ^ sold
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable ListingState

instance FromText ListingState where
    parser = match "available" LSAvailable
         <|> match "cancelled" LSCancelled
         <|> match "pending"   LSPending
         <|> match "sold"      LSSold

instance ToText ListingState where
    toText = \case
        LSAvailable -> "available"
        LSCancelled -> "cancelled"
        LSPending   -> "pending"
        LSSold      -> "sold"

instance FromXML ListingState where
    parseXML = fromXMLText "ListingState"

instance ToQuery ListingState

data SpotPrice = SpotPrice
    { _spAvailabilityZone   :: Maybe Text
    , _spInstanceType       :: Maybe Text
    , _spProductDescription :: Maybe Text
    , _spSpotPrice          :: Maybe Text
    , _spTimestamp          :: Maybe RFC822
    } deriving (Eq, Ord, Show, Generic)

-- | 'SpotPrice' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'spAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'spInstanceType' @::@ 'Maybe' 'Text'
--
-- * 'spProductDescription' @::@ 'Maybe' 'Text'
--
-- * 'spSpotPrice' @::@ 'Maybe' 'Text'
--
-- * 'spTimestamp' @::@ 'Maybe' 'UTCTime'
--
spotPrice :: SpotPrice
spotPrice = SpotPrice
    { _spInstanceType       = Nothing
    , _spProductDescription = Nothing
    , _spSpotPrice          = Nothing
    , _spTimestamp          = Nothing
    , _spAvailabilityZone   = Nothing
    }

-- | The Availability Zone.
spAvailabilityZone :: Lens' SpotPrice (Maybe Text)
spAvailabilityZone =
    lens _spAvailabilityZone (\s a -> s { _spAvailabilityZone = a })

-- | The instance type.
spInstanceType :: Lens' SpotPrice (Maybe Text)
spInstanceType = lens _spInstanceType (\s a -> s { _spInstanceType = a })

-- | A general description of the AMI.
spProductDescription :: Lens' SpotPrice (Maybe Text)
spProductDescription =
    lens _spProductDescription (\s a -> s { _spProductDescription = a })

-- | The maximum price you will pay to launch one or more Spot Instances.
spSpotPrice :: Lens' SpotPrice (Maybe Text)
spSpotPrice = lens _spSpotPrice (\s a -> s { _spSpotPrice = a })

-- | The date and time the request was created.
spTimestamp :: Lens' SpotPrice (Maybe UTCTime)
spTimestamp = lens _spTimestamp (\s a -> s { _spTimestamp = a })
    . mapping _Time

instance FromXML SpotPrice where
    parseXML c = SpotPrice
        <$> c .:? "availabilityZone"
        <*> c .:? "instanceType"
        <*> c .:? "productDescription"
        <*> c .:? "spotPrice"
        <*> c .:? "timestamp"

instance ToQuery SpotPrice

data InstanceMonitoring = InstanceMonitoring
    { _imInstanceId :: Maybe Text
    , _imMonitoring :: Maybe Monitoring
    } deriving (Eq, Show, Generic)

-- | 'InstanceMonitoring' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'imInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'imMonitoring' @::@ 'Maybe' 'Monitoring'
--
instanceMonitoring :: InstanceMonitoring
instanceMonitoring = InstanceMonitoring
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
    parseXML c = InstanceMonitoring
        <$> c .:? "instanceId"
        <*> c .:? "monitoring"

instance ToQuery InstanceMonitoring

data PriceScheduleSpecification = PriceScheduleSpecification
    { _pssCurrencyCode :: Maybe Text
    , _pssPrice        :: Maybe Double
    , _pssTerm         :: Maybe Integer
    } deriving (Eq, Ord, Show, Generic)

-- | 'PriceScheduleSpecification' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pssCurrencyCode' @::@ 'Maybe' 'Text'
--
-- * 'pssPrice' @::@ 'Maybe' 'Double'
--
-- * 'pssTerm' @::@ 'Maybe' 'Integer'
--
priceScheduleSpecification :: PriceScheduleSpecification
priceScheduleSpecification = PriceScheduleSpecification
    { _pssTerm         = Nothing
    , _pssPrice        = Nothing
    , _pssCurrencyCode = Nothing
    }

-- | The currency for transacting the Reserved Instance resale. At this time,
-- the only supported currency is USD.
pssCurrencyCode :: Lens' PriceScheduleSpecification (Maybe Text)
pssCurrencyCode = lens _pssCurrencyCode (\s a -> s { _pssCurrencyCode = a })

-- | The fixed price for the term.
pssPrice :: Lens' PriceScheduleSpecification (Maybe Double)
pssPrice = lens _pssPrice (\s a -> s { _pssPrice = a })

-- | The number of months remaining in the reservation. For example, 2 is the
-- second to the last month before the capacity reservation expires.
pssTerm :: Lens' PriceScheduleSpecification (Maybe Integer)
pssTerm = lens _pssTerm (\s a -> s { _pssTerm = a })

instance FromXML PriceScheduleSpecification where
    parseXML c = PriceScheduleSpecification
        <$> c .:? "currencyCode"
        <*> c .:? "price"
        <*> c .:? "term"

instance ToQuery PriceScheduleSpecification

data SpotInstanceStatus = SpotInstanceStatus
    { _sisCode       :: Maybe Text
    , _sisMessage    :: Maybe Text
    , _sisUpdateTime :: Maybe RFC822
    } deriving (Eq, Ord, Show, Generic)

-- | 'SpotInstanceStatus' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sisCode' @::@ 'Maybe' 'Text'
--
-- * 'sisMessage' @::@ 'Maybe' 'Text'
--
-- * 'sisUpdateTime' @::@ 'Maybe' 'UTCTime'
--
spotInstanceStatus :: SpotInstanceStatus
spotInstanceStatus = SpotInstanceStatus
    { _sisCode       = Nothing
    , _sisUpdateTime = Nothing
    , _sisMessage    = Nothing
    }

-- | The status code of the request.
sisCode :: Lens' SpotInstanceStatus (Maybe Text)
sisCode = lens _sisCode (\s a -> s { _sisCode = a })

-- | The description for the status code for the Spot request.
sisMessage :: Lens' SpotInstanceStatus (Maybe Text)
sisMessage = lens _sisMessage (\s a -> s { _sisMessage = a })

-- | The time of the most recent status update.
sisUpdateTime :: Lens' SpotInstanceStatus (Maybe UTCTime)
sisUpdateTime = lens _sisUpdateTime (\s a -> s { _sisUpdateTime = a })
    . mapping _Time

instance FromXML SpotInstanceStatus where
    parseXML c = SpotInstanceStatus
        <$> c .:? "code"
        <*> c .:? "message"
        <*> c .:? "updateTime"

instance ToQuery SpotInstanceStatus

data AvailabilityZoneState
    = AZSAvailable -- ^ available
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable AvailabilityZoneState

instance FromText AvailabilityZoneState where
    parser = match "available" AZSAvailable

instance ToText AvailabilityZoneState where
    toText AZSAvailable = "available"

instance FromXML AvailabilityZoneState where
    parseXML = fromXMLText "AvailabilityZoneState"

instance ToQuery AvailabilityZoneState

data SpotInstanceRequest = SpotInstanceRequest
    { _siAvailabilityZoneGroup    :: Maybe Text
    , _siCreateTime               :: Maybe RFC822
    , _siFault                    :: Maybe SpotInstanceStateFault
    , _siInstanceId               :: Maybe Text
    , _siLaunchGroup              :: Maybe Text
    , _siLaunchSpecification      :: Maybe LaunchSpecification
    , _siLaunchedAvailabilityZone :: Maybe Text
    , _siProductDescription       :: Maybe Text
    , _siSpotInstanceRequestId    :: Maybe Text
    , _siSpotPrice                :: Maybe Text
    , _siState                    :: Maybe Text
    , _siStatus                   :: Maybe SpotInstanceStatus
    , _siTags                     :: [Tag]
    , _siType                     :: Maybe Text
    , _siValidFrom                :: Maybe RFC822
    , _siValidUntil               :: Maybe RFC822
    } deriving (Eq, Show, Generic)

-- | 'SpotInstanceRequest' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'siAvailabilityZoneGroup' @::@ 'Maybe' 'Text'
--
-- * 'siCreateTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'siFault' @::@ 'Maybe' 'SpotInstanceStateFault'
--
-- * 'siInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'siLaunchGroup' @::@ 'Maybe' 'Text'
--
-- * 'siLaunchSpecification' @::@ 'Maybe' 'LaunchSpecification'
--
-- * 'siLaunchedAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'siProductDescription' @::@ 'Maybe' 'Text'
--
-- * 'siSpotInstanceRequestId' @::@ 'Maybe' 'Text'
--
-- * 'siSpotPrice' @::@ 'Maybe' 'Text'
--
-- * 'siState' @::@ 'Maybe' 'Text'
--
-- * 'siStatus' @::@ 'Maybe' 'SpotInstanceStatus'
--
-- * 'siTags' @::@ ['Tag']
--
-- * 'siType' @::@ 'Maybe' 'Text'
--
-- * 'siValidFrom' @::@ 'Maybe' 'UTCTime'
--
-- * 'siValidUntil' @::@ 'Maybe' 'UTCTime'
--
spotInstanceRequest :: SpotInstanceRequest
spotInstanceRequest = SpotInstanceRequest
    { _siSpotInstanceRequestId    = Nothing
    , _siSpotPrice                = Nothing
    , _siType                     = Nothing
    , _siState                    = Nothing
    , _siFault                    = Nothing
    , _siStatus                   = Nothing
    , _siValidFrom                = Nothing
    , _siValidUntil               = Nothing
    , _siLaunchGroup              = Nothing
    , _siAvailabilityZoneGroup    = Nothing
    , _siLaunchSpecification      = Nothing
    , _siInstanceId               = Nothing
    , _siCreateTime               = Nothing
    , _siProductDescription       = Nothing
    , _siTags                     = mempty
    , _siLaunchedAvailabilityZone = Nothing
    }

-- | The Availability Zone group. If you specify the same Availability Zone
-- group for all Spot Instance requests, all Spot Instances are launched in
-- the same Availability Zone.
siAvailabilityZoneGroup :: Lens' SpotInstanceRequest (Maybe Text)
siAvailabilityZoneGroup =
    lens _siAvailabilityZoneGroup (\s a -> s { _siAvailabilityZoneGroup = a })

-- | The time stamp when the Spot Instance request was created.
siCreateTime :: Lens' SpotInstanceRequest (Maybe UTCTime)
siCreateTime = lens _siCreateTime (\s a -> s { _siCreateTime = a })
    . mapping _Time

-- | The fault codes for the Spot Instance request, if any.
siFault :: Lens' SpotInstanceRequest (Maybe SpotInstanceStateFault)
siFault = lens _siFault (\s a -> s { _siFault = a })

-- | The instance ID, if an instance has been launched to fulfill the Spot
-- Instance request.
siInstanceId :: Lens' SpotInstanceRequest (Maybe Text)
siInstanceId = lens _siInstanceId (\s a -> s { _siInstanceId = a })

-- | The instance launch group. Launch groups are Spot Instances that launch
-- together and terminate together.
siLaunchGroup :: Lens' SpotInstanceRequest (Maybe Text)
siLaunchGroup = lens _siLaunchGroup (\s a -> s { _siLaunchGroup = a })

-- | Additional information for launching instances.
siLaunchSpecification :: Lens' SpotInstanceRequest (Maybe LaunchSpecification)
siLaunchSpecification =
    lens _siLaunchSpecification (\s a -> s { _siLaunchSpecification = a })

-- | The Availability Zone in which the bid is launched.
siLaunchedAvailabilityZone :: Lens' SpotInstanceRequest (Maybe Text)
siLaunchedAvailabilityZone =
    lens _siLaunchedAvailabilityZone
        (\s a -> s { _siLaunchedAvailabilityZone = a })

-- | The product description associated with the Spot Instance.
siProductDescription :: Lens' SpotInstanceRequest (Maybe Text)
siProductDescription =
    lens _siProductDescription (\s a -> s { _siProductDescription = a })

-- | The ID of the Spot Instance request.
siSpotInstanceRequestId :: Lens' SpotInstanceRequest (Maybe Text)
siSpotInstanceRequestId =
    lens _siSpotInstanceRequestId (\s a -> s { _siSpotInstanceRequestId = a })

-- | The maximum hourly price for any Spot Instance launched to fulfill the
-- request.
siSpotPrice :: Lens' SpotInstanceRequest (Maybe Text)
siSpotPrice = lens _siSpotPrice (\s a -> s { _siSpotPrice = a })

-- | The state of the Spot Instance request. Spot bid status information can
-- help you track your Spot Instance requests. For information, see Tracking
-- Spot Requests with Bid Status Codes in the Amazon Elastic Compute Cloud
-- User Guide.
siState :: Lens' SpotInstanceRequest (Maybe Text)
siState = lens _siState (\s a -> s { _siState = a })

-- | The status code and status message describing the Spot Instance request.
siStatus :: Lens' SpotInstanceRequest (Maybe SpotInstanceStatus)
siStatus = lens _siStatus (\s a -> s { _siStatus = a })

-- | Any tags assigned to the resource.
siTags :: Lens' SpotInstanceRequest [Tag]
siTags = lens _siTags (\s a -> s { _siTags = a })

-- | The Spot Instance request type.
siType :: Lens' SpotInstanceRequest (Maybe Text)
siType = lens _siType (\s a -> s { _siType = a })

-- | The start date of the request. If this is a one-time request, the request
-- becomes active at this date and time and remains active until all
-- instances launch, the request expires, or the request is canceled. If the
-- request is persistent, the request becomes active at this date and time
-- and remains active until it expires or is canceled.
siValidFrom :: Lens' SpotInstanceRequest (Maybe UTCTime)
siValidFrom = lens _siValidFrom (\s a -> s { _siValidFrom = a })
    . mapping _Time

-- | The end date of the request. If this is a one-time request, the request
-- remains active until all instances launch, the request is canceled, or
-- this date is reached. If the request is persistent, it remains active
-- until it is canceled or this date is reached.
siValidUntil :: Lens' SpotInstanceRequest (Maybe UTCTime)
siValidUntil = lens _siValidUntil (\s a -> s { _siValidUntil = a })
    . mapping _Time

instance FromXML SpotInstanceRequest where
    parseXML c = SpotInstanceRequest
        <$> c .:? "availabilityZoneGroup"
        <*> c .:? "createTime"
        <*> c .:? "fault"
        <*> c .:? "instanceId"
        <*> c .:? "launchGroup"
        <*> c .:? "launchSpecification"
        <*> c .:? "launchedAvailabilityZone"
        <*> c .:? "productDescription"
        <*> c .:? "spotInstanceRequestId"
        <*> c .:? "spotPrice"
        <*> c .:? "state"
        <*> c .:? "status"
        <*> c .: "tagSet"
        <*> c .:? "type"
        <*> c .:? "validFrom"
        <*> c .:? "validUntil"

instance ToQuery SpotInstanceRequest

data LaunchSpecification = LaunchSpecification
    { _lsAddressingType      :: Maybe Text
    , _lsBlockDeviceMappings :: [BlockDeviceMapping]
    , _lsEbsOptimized        :: Maybe Bool
    , _lsIamInstanceProfile  :: Maybe IamInstanceProfileSpecification
    , _lsImageId             :: Maybe Text
    , _lsInstanceType        :: Maybe Text
    , _lsKernelId            :: Maybe Text
    , _lsKeyName             :: Maybe Text
    , _lsMonitoring          :: Maybe RunInstancesMonitoringEnabled
    , _lsNetworkInterfaces   :: [InstanceNetworkInterfaceSpecification]
    , _lsPlacement           :: Maybe SpotPlacement
    , _lsRamdiskId           :: Maybe Text
    , _lsSecurityGroups      :: [GroupIdentifier]
    , _lsSubnetId            :: Maybe Text
    , _lsUserData            :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'LaunchSpecification' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsAddressingType' @::@ 'Maybe' 'Text'
--
-- * 'lsBlockDeviceMappings' @::@ ['BlockDeviceMapping']
--
-- * 'lsEbsOptimized' @::@ 'Maybe' 'Bool'
--
-- * 'lsIamInstanceProfile' @::@ 'Maybe' 'IamInstanceProfileSpecification'
--
-- * 'lsImageId' @::@ 'Maybe' 'Text'
--
-- * 'lsInstanceType' @::@ 'Maybe' 'Text'
--
-- * 'lsKernelId' @::@ 'Maybe' 'Text'
--
-- * 'lsKeyName' @::@ 'Maybe' 'Text'
--
-- * 'lsMonitoring' @::@ 'Maybe' 'RunInstancesMonitoringEnabled'
--
-- * 'lsNetworkInterfaces' @::@ ['InstanceNetworkInterfaceSpecification']
--
-- * 'lsPlacement' @::@ 'Maybe' 'SpotPlacement'
--
-- * 'lsRamdiskId' @::@ 'Maybe' 'Text'
--
-- * 'lsSecurityGroups' @::@ ['GroupIdentifier']
--
-- * 'lsSubnetId' @::@ 'Maybe' 'Text'
--
-- * 'lsUserData' @::@ 'Maybe' 'Text'
--
launchSpecification :: LaunchSpecification
launchSpecification = LaunchSpecification
    { _lsImageId             = Nothing
    , _lsKeyName             = Nothing
    , _lsSecurityGroups      = mempty
    , _lsUserData            = Nothing
    , _lsAddressingType      = Nothing
    , _lsInstanceType        = Nothing
    , _lsPlacement           = Nothing
    , _lsKernelId            = Nothing
    , _lsRamdiskId           = Nothing
    , _lsBlockDeviceMappings = mempty
    , _lsSubnetId            = Nothing
    , _lsNetworkInterfaces   = mempty
    , _lsIamInstanceProfile  = Nothing
    , _lsEbsOptimized        = Nothing
    , _lsMonitoring          = Nothing
    }

-- | Deprecated.
lsAddressingType :: Lens' LaunchSpecification (Maybe Text)
lsAddressingType = lens _lsAddressingType (\s a -> s { _lsAddressingType = a })

-- | One or more block device mapping entries.
lsBlockDeviceMappings :: Lens' LaunchSpecification [BlockDeviceMapping]
lsBlockDeviceMappings =
    lens _lsBlockDeviceMappings (\s a -> s { _lsBlockDeviceMappings = a })

-- | Indicates whether the instance is optimized for EBS I/O. This
-- optimization provides dedicated throughput to Amazon EBS and an optimized
-- configuration stack to provide optimal EBS I/O performance. This
-- optimization isn't available with all instance types. Additional usage
-- charges apply when using an EBS Optimized instance. Default: false.
lsEbsOptimized :: Lens' LaunchSpecification (Maybe Bool)
lsEbsOptimized = lens _lsEbsOptimized (\s a -> s { _lsEbsOptimized = a })

-- | The IAM instance profile.
lsIamInstanceProfile :: Lens' LaunchSpecification (Maybe IamInstanceProfileSpecification)
lsIamInstanceProfile =
    lens _lsIamInstanceProfile (\s a -> s { _lsIamInstanceProfile = a })

-- | The ID of the AMI.
lsImageId :: Lens' LaunchSpecification (Maybe Text)
lsImageId = lens _lsImageId (\s a -> s { _lsImageId = a })

-- | The instance type. Default: m1.small.
lsInstanceType :: Lens' LaunchSpecification (Maybe Text)
lsInstanceType = lens _lsInstanceType (\s a -> s { _lsInstanceType = a })

-- | The ID of the kernel.
lsKernelId :: Lens' LaunchSpecification (Maybe Text)
lsKernelId = lens _lsKernelId (\s a -> s { _lsKernelId = a })

-- | The name of the key pair.
lsKeyName :: Lens' LaunchSpecification (Maybe Text)
lsKeyName = lens _lsKeyName (\s a -> s { _lsKeyName = a })

lsMonitoring :: Lens' LaunchSpecification (Maybe RunInstancesMonitoringEnabled)
lsMonitoring = lens _lsMonitoring (\s a -> s { _lsMonitoring = a })

-- | One or more network interfaces.
lsNetworkInterfaces :: Lens' LaunchSpecification [InstanceNetworkInterfaceSpecification]
lsNetworkInterfaces =
    lens _lsNetworkInterfaces (\s a -> s { _lsNetworkInterfaces = a })

-- | The placement information for the instance.
lsPlacement :: Lens' LaunchSpecification (Maybe SpotPlacement)
lsPlacement = lens _lsPlacement (\s a -> s { _lsPlacement = a })

-- | The ID of the RAM disk.
lsRamdiskId :: Lens' LaunchSpecification (Maybe Text)
lsRamdiskId = lens _lsRamdiskId (\s a -> s { _lsRamdiskId = a })

-- | One or more security groups. If requesting a Spot Instance in a
-- nondefault VPC, you must specify the security group ID. If requesting a
-- Spot Instance in EC2-Classic or a default VPC, you can specify either the
-- security group name or ID.
lsSecurityGroups :: Lens' LaunchSpecification [GroupIdentifier]
lsSecurityGroups = lens _lsSecurityGroups (\s a -> s { _lsSecurityGroups = a })

-- | The ID of the subnet in which to launch the Spot Instance.
lsSubnetId :: Lens' LaunchSpecification (Maybe Text)
lsSubnetId = lens _lsSubnetId (\s a -> s { _lsSubnetId = a })

-- | The Base64-encoded MIME user data to make available to the instances.
lsUserData :: Lens' LaunchSpecification (Maybe Text)
lsUserData = lens _lsUserData (\s a -> s { _lsUserData = a })

instance FromXML LaunchSpecification where
    parseXML c = LaunchSpecification
        <$> c .:? "addressingType"
        <*> c .: "blockDeviceMapping"
        <*> c .:? "ebsOptimized"
        <*> c .:? "iamInstanceProfile"
        <*> c .:? "imageId"
        <*> c .:? "instanceType"
        <*> c .:? "kernelId"
        <*> c .:? "keyName"
        <*> c .:? "monitoring"
        <*> c .: "networkInterfaceSet"
        <*> c .:? "placement"
        <*> c .:? "ramdiskId"
        <*> c .: "groupSet"
        <*> c .:? "subnetId"
        <*> c .:? "userData"

instance ToQuery LaunchSpecification

data VolumeStatusEvent = VolumeStatusEvent
    { _vseDescription :: Maybe Text
    , _vseEventId     :: Maybe Text
    , _vseEventType   :: Maybe Text
    , _vseNotAfter    :: Maybe RFC822
    , _vseNotBefore   :: Maybe RFC822
    } deriving (Eq, Ord, Show, Generic)

-- | 'VolumeStatusEvent' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vseDescription' @::@ 'Maybe' 'Text'
--
-- * 'vseEventId' @::@ 'Maybe' 'Text'
--
-- * 'vseEventType' @::@ 'Maybe' 'Text'
--
-- * 'vseNotAfter' @::@ 'Maybe' 'UTCTime'
--
-- * 'vseNotBefore' @::@ 'Maybe' 'UTCTime'
--
volumeStatusEvent :: VolumeStatusEvent
volumeStatusEvent = VolumeStatusEvent
    { _vseEventType   = Nothing
    , _vseDescription = Nothing
    , _vseNotBefore   = Nothing
    , _vseNotAfter    = Nothing
    , _vseEventId     = Nothing
    }

-- | A description of the event.
vseDescription :: Lens' VolumeStatusEvent (Maybe Text)
vseDescription = lens _vseDescription (\s a -> s { _vseDescription = a })

-- | The ID of this event.
vseEventId :: Lens' VolumeStatusEvent (Maybe Text)
vseEventId = lens _vseEventId (\s a -> s { _vseEventId = a })

-- | The type of this event.
vseEventType :: Lens' VolumeStatusEvent (Maybe Text)
vseEventType = lens _vseEventType (\s a -> s { _vseEventType = a })

-- | The latest end time of the event.
vseNotAfter :: Lens' VolumeStatusEvent (Maybe UTCTime)
vseNotAfter = lens _vseNotAfter (\s a -> s { _vseNotAfter = a })
    . mapping _Time

-- | The earliest start time of the event.
vseNotBefore :: Lens' VolumeStatusEvent (Maybe UTCTime)
vseNotBefore = lens _vseNotBefore (\s a -> s { _vseNotBefore = a })
    . mapping _Time

instance FromXML VolumeStatusEvent where
    parseXML c = VolumeStatusEvent
        <$> c .:? "description"
        <*> c .:? "eventId"
        <*> c .:? "eventType"
        <*> c .:? "notAfter"
        <*> c .:? "notBefore"

instance ToQuery VolumeStatusEvent

data Volume = Volume
    { _vAttachments      :: [VolumeAttachment]
    , _vAvailabilityZone :: Maybe Text
    , _vCreateTime       :: Maybe RFC822
    , _vEncrypted        :: Maybe Bool
    , _vIops             :: Maybe Int
    , _vSize             :: Maybe Int
    , _vSnapshotId       :: Maybe Text
    , _vState            :: Maybe Text
    , _vTags             :: [Tag]
    , _vVolumeId         :: Maybe Text
    , _vVolumeType       :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'Volume' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vAttachments' @::@ ['VolumeAttachment']
--
-- * 'vAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'vCreateTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'vEncrypted' @::@ 'Maybe' 'Bool'
--
-- * 'vIops' @::@ 'Maybe' 'Int'
--
-- * 'vSize' @::@ 'Maybe' 'Int'
--
-- * 'vSnapshotId' @::@ 'Maybe' 'Text'
--
-- * 'vState' @::@ 'Maybe' 'Text'
--
-- * 'vTags' @::@ ['Tag']
--
-- * 'vVolumeId' @::@ 'Maybe' 'Text'
--
-- * 'vVolumeType' @::@ 'Maybe' 'Text'
--
volume :: Volume
volume = Volume
    { _vVolumeId         = Nothing
    , _vSize             = Nothing
    , _vSnapshotId       = Nothing
    , _vAvailabilityZone = Nothing
    , _vState            = Nothing
    , _vCreateTime       = Nothing
    , _vAttachments      = mempty
    , _vTags             = mempty
    , _vVolumeType       = Nothing
    , _vIops             = Nothing
    , _vEncrypted        = Nothing
    }

vAttachments :: Lens' Volume [VolumeAttachment]
vAttachments = lens _vAttachments (\s a -> s { _vAttachments = a })

-- | The Availability Zone for the volume.
vAvailabilityZone :: Lens' Volume (Maybe Text)
vAvailabilityZone =
    lens _vAvailabilityZone (\s a -> s { _vAvailabilityZone = a })

-- | The time stamp when volume creation was initiated.
vCreateTime :: Lens' Volume (Maybe UTCTime)
vCreateTime = lens _vCreateTime (\s a -> s { _vCreateTime = a })
    . mapping _Time

-- | Indicates whether the volume is encrypted.
vEncrypted :: Lens' Volume (Maybe Bool)
vEncrypted = lens _vEncrypted (\s a -> s { _vEncrypted = a })

-- | The number of I/O operations per second (IOPS) that the volume supports.
-- For Provisioned IOPS (SSD) volumes, this represents the number of IOPS
-- that are provisioned for the volume. For General Purpose (SSD) volumes,
-- this represents the baseline performance of the volume and the rate at
-- which the volume accumulates I/O credits for bursting. For more
-- information on General Purpose (SSD) baseline performance, I/O credits,
-- and bursting, see Amazon EBS Volume Types in the Amazon Elastic Compute
-- Cloud User Guide. Constraint: Range is 100 to 4000 for Provisioned IOPS
-- (SSD) volumes and 3 to 3072 for General Purpose (SSD) volumes. Condition:
-- This parameter is required for requests to create io1 volumes; it is not
-- used in requests to create standard or gp2 volumes.
vIops :: Lens' Volume (Maybe Int)
vIops = lens _vIops (\s a -> s { _vIops = a })

-- | The size of the volume, in GiBs.
vSize :: Lens' Volume (Maybe Int)
vSize = lens _vSize (\s a -> s { _vSize = a })

-- | The snapshot from which the volume was created, if applicable.
vSnapshotId :: Lens' Volume (Maybe Text)
vSnapshotId = lens _vSnapshotId (\s a -> s { _vSnapshotId = a })

-- | The volume state.
vState :: Lens' Volume (Maybe Text)
vState = lens _vState (\s a -> s { _vState = a })

-- | Any tags assigned to the volume.
vTags :: Lens' Volume [Tag]
vTags = lens _vTags (\s a -> s { _vTags = a })

-- | The ID of the volume.
vVolumeId :: Lens' Volume (Maybe Text)
vVolumeId = lens _vVolumeId (\s a -> s { _vVolumeId = a })

-- | The volume type. This can be gp2 for General Purpose (SSD) volumes, io1
-- for Provisioned IOPS (SSD) volumes, or standard for Magnetic volumes.
vVolumeType :: Lens' Volume (Maybe Text)
vVolumeType = lens _vVolumeType (\s a -> s { _vVolumeType = a })

instance FromXML Volume where
    parseXML c = Volume
        <$> c .: "attachmentSet"
        <*> c .:? "availabilityZone"
        <*> c .:? "createTime"
        <*> c .:? "encrypted"
        <*> c .:? "iops"
        <*> c .:? "size"
        <*> c .:? "snapshotId"
        <*> c .:? "status"
        <*> c .: "tagSet"
        <*> c .:? "volumeId"
        <*> c .:? "volumeType"

instance ToQuery Volume

data Reservation = Reservation
    { _rGroups        :: [GroupIdentifier]
    , _rInstances     :: [Instance]
    , _rOwnerId       :: Maybe Text
    , _rRequesterId   :: Maybe Text
    , _rReservationId :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'Reservation' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rGroups' @::@ ['GroupIdentifier']
--
-- * 'rInstances' @::@ ['Instance']
--
-- * 'rOwnerId' @::@ 'Maybe' 'Text'
--
-- * 'rRequesterId' @::@ 'Maybe' 'Text'
--
-- * 'rReservationId' @::@ 'Maybe' 'Text'
--
reservation :: Reservation
reservation = Reservation
    { _rReservationId = Nothing
    , _rOwnerId       = Nothing
    , _rRequesterId   = Nothing
    , _rGroups        = mempty
    , _rInstances     = mempty
    }

-- | One or more security groups.
rGroups :: Lens' Reservation [GroupIdentifier]
rGroups = lens _rGroups (\s a -> s { _rGroups = a })

-- | One or more instances.
rInstances :: Lens' Reservation [Instance]
rInstances = lens _rInstances (\s a -> s { _rInstances = a })

-- | The ID of the AWS account that owns the reservation.
rOwnerId :: Lens' Reservation (Maybe Text)
rOwnerId = lens _rOwnerId (\s a -> s { _rOwnerId = a })

-- | The ID of the requester that launched the instances on your behalf (for
-- example, AWS Management Console or Auto Scaling).
rRequesterId :: Lens' Reservation (Maybe Text)
rRequesterId = lens _rRequesterId (\s a -> s { _rRequesterId = a })

-- | The ID of the reservation.
rReservationId :: Lens' Reservation (Maybe Text)
rReservationId = lens _rReservationId (\s a -> s { _rReservationId = a })

instance FromXML Reservation where
    parseXML c = Reservation
        <$> c .: "groupSet"
        <*> c .: "instancesSet"
        <*> c .:? "ownerId"
        <*> c .:? "requesterId"
        <*> c .:? "reservationId"

instance ToQuery Reservation

data ImportInstanceVolumeDetailItem = ImportInstanceVolumeDetailItem
    { _iivdiAvailabilityZone :: Text
    , _iivdiBytesConverted   :: Integer
    , _iivdiDescription      :: Maybe Text
    , _iivdiImage            :: DiskImageDescription
    , _iivdiStatus           :: Text
    , _iivdiStatusMessage    :: Maybe Text
    , _iivdiVolume           :: DiskImageVolumeDescription
    } deriving (Eq, Show, Generic)

-- | 'ImportInstanceVolumeDetailItem' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iivdiAvailabilityZone' @::@ 'Text'
--
-- * 'iivdiBytesConverted' @::@ 'Integer'
--
-- * 'iivdiDescription' @::@ 'Maybe' 'Text'
--
-- * 'iivdiImage' @::@ 'DiskImageDescription'
--
-- * 'iivdiStatus' @::@ 'Text'
--
-- * 'iivdiStatusMessage' @::@ 'Maybe' 'Text'
--
-- * 'iivdiVolume' @::@ 'DiskImageVolumeDescription'
--
importInstanceVolumeDetailItem :: Integer -- ^ 'iivdiBytesConverted'
                               -> Text -- ^ 'iivdiAvailabilityZone'
                               -> DiskImageDescription -- ^ 'iivdiImage'
                               -> DiskImageVolumeDescription -- ^ 'iivdiVolume'
                               -> Text -- ^ 'iivdiStatus'
                               -> ImportInstanceVolumeDetailItem
importInstanceVolumeDetailItem p1 p2 p3 p4 p5 = ImportInstanceVolumeDetailItem
    { _iivdiBytesConverted   = p1
    , _iivdiAvailabilityZone = p2
    , _iivdiImage            = p3
    , _iivdiVolume           = p4
    , _iivdiStatus           = p5
    , _iivdiStatusMessage    = Nothing
    , _iivdiDescription      = Nothing
    }

-- | The Availability Zone where the resulting instance will reside.
iivdiAvailabilityZone :: Lens' ImportInstanceVolumeDetailItem Text
iivdiAvailabilityZone =
    lens _iivdiAvailabilityZone (\s a -> s { _iivdiAvailabilityZone = a })

-- | The number of bytes converted so far.
iivdiBytesConverted :: Lens' ImportInstanceVolumeDetailItem Integer
iivdiBytesConverted =
    lens _iivdiBytesConverted (\s a -> s { _iivdiBytesConverted = a })

iivdiDescription :: Lens' ImportInstanceVolumeDetailItem (Maybe Text)
iivdiDescription = lens _iivdiDescription (\s a -> s { _iivdiDescription = a })

-- | The image.
iivdiImage :: Lens' ImportInstanceVolumeDetailItem DiskImageDescription
iivdiImage = lens _iivdiImage (\s a -> s { _iivdiImage = a })

-- | The status of the import of this particular disk image.
iivdiStatus :: Lens' ImportInstanceVolumeDetailItem Text
iivdiStatus = lens _iivdiStatus (\s a -> s { _iivdiStatus = a })

-- | The status information or errors related to the disk image.
iivdiStatusMessage :: Lens' ImportInstanceVolumeDetailItem (Maybe Text)
iivdiStatusMessage =
    lens _iivdiStatusMessage (\s a -> s { _iivdiStatusMessage = a })

-- | The volume.
iivdiVolume :: Lens' ImportInstanceVolumeDetailItem DiskImageVolumeDescription
iivdiVolume = lens _iivdiVolume (\s a -> s { _iivdiVolume = a })

instance FromXML ImportInstanceVolumeDetailItem where
    parseXML c = ImportInstanceVolumeDetailItem
        <$> c .: "availabilityZone"
        <*> c .: "bytesConverted"
        <*> c .:? "description"
        <*> c .: "image"
        <*> c .: "status"
        <*> c .:? "statusMessage"
        <*> c .: "volume"

instance ToQuery ImportInstanceVolumeDetailItem

data SummaryStatus
    = SSImpaired         -- ^ impaired
    | SSInsufficientData -- ^ insufficient-data
    | SSNotApplicable    -- ^ not-applicable
    | SSOk               -- ^ ok
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable SummaryStatus

instance FromText SummaryStatus where
    parser = match "impaired"          SSImpaired
         <|> match "insufficient-data" SSInsufficientData
         <|> match "not-applicable"    SSNotApplicable
         <|> match "ok"                SSOk

instance ToText SummaryStatus where
    toText = \case
        SSImpaired         -> "impaired"
        SSInsufficientData -> "insufficient-data"
        SSNotApplicable    -> "not-applicable"
        SSOk               -> "ok"

instance FromXML SummaryStatus where
    parseXML = fromXMLText "SummaryStatus"

instance ToQuery SummaryStatus

data ReservedInstancesModification = ReservedInstancesModification
    { _rimClientToken                     :: Maybe Text
    , _rimCreateDate                      :: Maybe RFC822
    , _rimEffectiveDate                   :: Maybe RFC822
    , _rimModificationResults             :: [ReservedInstancesModificationResult]
    , _rimReservedInstancesIds            :: [ReservedInstancesId]
    , _rimReservedInstancesModificationId :: Maybe Text
    , _rimStatus                          :: Maybe Text
    , _rimStatusMessage                   :: Maybe Text
    , _rimUpdateDate                      :: Maybe RFC822
    } deriving (Eq, Show, Generic)

-- | 'ReservedInstancesModification' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rimClientToken' @::@ 'Maybe' 'Text'
--
-- * 'rimCreateDate' @::@ 'Maybe' 'UTCTime'
--
-- * 'rimEffectiveDate' @::@ 'Maybe' 'UTCTime'
--
-- * 'rimModificationResults' @::@ ['ReservedInstancesModificationResult']
--
-- * 'rimReservedInstancesIds' @::@ ['ReservedInstancesId']
--
-- * 'rimReservedInstancesModificationId' @::@ 'Maybe' 'Text'
--
-- * 'rimStatus' @::@ 'Maybe' 'Text'
--
-- * 'rimStatusMessage' @::@ 'Maybe' 'Text'
--
-- * 'rimUpdateDate' @::@ 'Maybe' 'UTCTime'
--
reservedInstancesModification :: ReservedInstancesModification
reservedInstancesModification = ReservedInstancesModification
    { _rimReservedInstancesModificationId = Nothing
    , _rimReservedInstancesIds            = mempty
    , _rimModificationResults             = mempty
    , _rimCreateDate                      = Nothing
    , _rimUpdateDate                      = Nothing
    , _rimEffectiveDate                   = Nothing
    , _rimStatus                          = Nothing
    , _rimStatusMessage                   = Nothing
    , _rimClientToken                     = Nothing
    }

-- | A unique, case-sensitive key supplied by the client to ensure that the
-- modification request is idempotent.
rimClientToken :: Lens' ReservedInstancesModification (Maybe Text)
rimClientToken = lens _rimClientToken (\s a -> s { _rimClientToken = a })

-- | The time when the modification request was created.
rimCreateDate :: Lens' ReservedInstancesModification (Maybe UTCTime)
rimCreateDate = lens _rimCreateDate (\s a -> s { _rimCreateDate = a })
    . mapping _Time

-- | The time for the modification to become effective.
rimEffectiveDate :: Lens' ReservedInstancesModification (Maybe UTCTime)
rimEffectiveDate = lens _rimEffectiveDate (\s a -> s { _rimEffectiveDate = a })
    . mapping _Time

-- | Contains target configurations along with their corresponding new
-- Reserved Instance IDs.
rimModificationResults :: Lens' ReservedInstancesModification [ReservedInstancesModificationResult]
rimModificationResults =
    lens _rimModificationResults (\s a -> s { _rimModificationResults = a })

-- | The IDs of one or more Reserved Instances.
rimReservedInstancesIds :: Lens' ReservedInstancesModification [ReservedInstancesId]
rimReservedInstancesIds =
    lens _rimReservedInstancesIds (\s a -> s { _rimReservedInstancesIds = a })

-- | A unique ID for the Reserved Instance modification.
rimReservedInstancesModificationId :: Lens' ReservedInstancesModification (Maybe Text)
rimReservedInstancesModificationId =
    lens _rimReservedInstancesModificationId
        (\s a -> s { _rimReservedInstancesModificationId = a })

-- | The status of the Reserved Instances modification request.
rimStatus :: Lens' ReservedInstancesModification (Maybe Text)
rimStatus = lens _rimStatus (\s a -> s { _rimStatus = a })

-- | The reason for the status.
rimStatusMessage :: Lens' ReservedInstancesModification (Maybe Text)
rimStatusMessage = lens _rimStatusMessage (\s a -> s { _rimStatusMessage = a })

-- | The time when the modification request was last updated.
rimUpdateDate :: Lens' ReservedInstancesModification (Maybe UTCTime)
rimUpdateDate = lens _rimUpdateDate (\s a -> s { _rimUpdateDate = a })
    . mapping _Time

instance FromXML ReservedInstancesModification where
    parseXML c = ReservedInstancesModification
        <$> c .:? "clientToken"
        <*> c .:? "createDate"
        <*> c .:? "effectiveDate"
        <*> c .: "modificationResultSet"
        <*> c .: "reservedInstancesSet"
        <*> c .:? "reservedInstancesModificationId"
        <*> c .:? "status"
        <*> c .:? "statusMessage"
        <*> c .:? "updateDate"

instance ToQuery ReservedInstancesModification

data RuleAction
    = Allow -- ^ allow
    | Deny  -- ^ deny
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable RuleAction

instance FromText RuleAction where
    parser = match "allow" Allow
         <|> match "deny"  Deny

instance ToText RuleAction where
    toText = \case
        Allow -> "allow"
        Deny  -> "deny"

instance FromXML RuleAction where
    parseXML = fromXMLText "RuleAction"

instance ToQuery RuleAction

data NetworkInterface = NetworkInterface
    { _niAssociation        :: Maybe NetworkInterfaceAssociation
    , _niAttachment         :: Maybe NetworkInterfaceAttachment
    , _niAvailabilityZone   :: Maybe Text
    , _niDescription        :: Maybe Text
    , _niGroups             :: [GroupIdentifier]
    , _niMacAddress         :: Maybe Text
    , _niNetworkInterfaceId :: Maybe Text
    , _niOwnerId            :: Maybe Text
    , _niPrivateDnsName     :: Maybe Text
    , _niPrivateIpAddress   :: Maybe Text
    , _niPrivateIpAddresses :: [NetworkInterfacePrivateIpAddress]
    , _niRequesterId        :: Maybe Text
    , _niRequesterManaged   :: Maybe Bool
    , _niSourceDestCheck    :: Maybe Bool
    , _niStatus             :: Maybe Text
    , _niSubnetId           :: Maybe Text
    , _niTagSet             :: [Tag]
    , _niVpcId              :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'NetworkInterface' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'niAssociation' @::@ 'Maybe' 'NetworkInterfaceAssociation'
--
-- * 'niAttachment' @::@ 'Maybe' 'NetworkInterfaceAttachment'
--
-- * 'niAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'niDescription' @::@ 'Maybe' 'Text'
--
-- * 'niGroups' @::@ ['GroupIdentifier']
--
-- * 'niMacAddress' @::@ 'Maybe' 'Text'
--
-- * 'niNetworkInterfaceId' @::@ 'Maybe' 'Text'
--
-- * 'niOwnerId' @::@ 'Maybe' 'Text'
--
-- * 'niPrivateDnsName' @::@ 'Maybe' 'Text'
--
-- * 'niPrivateIpAddress' @::@ 'Maybe' 'Text'
--
-- * 'niPrivateIpAddresses' @::@ ['NetworkInterfacePrivateIpAddress']
--
-- * 'niRequesterId' @::@ 'Maybe' 'Text'
--
-- * 'niRequesterManaged' @::@ 'Maybe' 'Bool'
--
-- * 'niSourceDestCheck' @::@ 'Maybe' 'Bool'
--
-- * 'niStatus' @::@ 'Maybe' 'Text'
--
-- * 'niSubnetId' @::@ 'Maybe' 'Text'
--
-- * 'niTagSet' @::@ ['Tag']
--
-- * 'niVpcId' @::@ 'Maybe' 'Text'
--
networkInterface :: NetworkInterface
networkInterface = NetworkInterface
    { _niNetworkInterfaceId = Nothing
    , _niSubnetId           = Nothing
    , _niVpcId              = Nothing
    , _niAvailabilityZone   = Nothing
    , _niDescription        = Nothing
    , _niOwnerId            = Nothing
    , _niRequesterId        = Nothing
    , _niRequesterManaged   = Nothing
    , _niStatus             = Nothing
    , _niMacAddress         = Nothing
    , _niPrivateIpAddress   = Nothing
    , _niPrivateDnsName     = Nothing
    , _niSourceDestCheck    = Nothing
    , _niGroups             = mempty
    , _niAttachment         = Nothing
    , _niAssociation        = Nothing
    , _niTagSet             = mempty
    , _niPrivateIpAddresses = mempty
    }

-- | The association information for an Elastic IP associated with the network
-- interface.
niAssociation :: Lens' NetworkInterface (Maybe NetworkInterfaceAssociation)
niAssociation = lens _niAssociation (\s a -> s { _niAssociation = a })

-- | The network interface attachment.
niAttachment :: Lens' NetworkInterface (Maybe NetworkInterfaceAttachment)
niAttachment = lens _niAttachment (\s a -> s { _niAttachment = a })

-- | The Availability Zone.
niAvailabilityZone :: Lens' NetworkInterface (Maybe Text)
niAvailabilityZone =
    lens _niAvailabilityZone (\s a -> s { _niAvailabilityZone = a })

-- | A description.
niDescription :: Lens' NetworkInterface (Maybe Text)
niDescription = lens _niDescription (\s a -> s { _niDescription = a })

-- | Any security groups for the network interface.
niGroups :: Lens' NetworkInterface [GroupIdentifier]
niGroups = lens _niGroups (\s a -> s { _niGroups = a })

-- | The MAC address.
niMacAddress :: Lens' NetworkInterface (Maybe Text)
niMacAddress = lens _niMacAddress (\s a -> s { _niMacAddress = a })

-- | The ID of the network interface.
niNetworkInterfaceId :: Lens' NetworkInterface (Maybe Text)
niNetworkInterfaceId =
    lens _niNetworkInterfaceId (\s a -> s { _niNetworkInterfaceId = a })

-- | The AWS account ID of the owner of the network interface.
niOwnerId :: Lens' NetworkInterface (Maybe Text)
niOwnerId = lens _niOwnerId (\s a -> s { _niOwnerId = a })

-- | The private DNS name.
niPrivateDnsName :: Lens' NetworkInterface (Maybe Text)
niPrivateDnsName = lens _niPrivateDnsName (\s a -> s { _niPrivateDnsName = a })

-- | The IP address of the network interface within the subnet.
niPrivateIpAddress :: Lens' NetworkInterface (Maybe Text)
niPrivateIpAddress =
    lens _niPrivateIpAddress (\s a -> s { _niPrivateIpAddress = a })

-- | The private IP addresses associated with the network interface.
niPrivateIpAddresses :: Lens' NetworkInterface [NetworkInterfacePrivateIpAddress]
niPrivateIpAddresses =
    lens _niPrivateIpAddresses (\s a -> s { _niPrivateIpAddresses = a })

-- | The ID of the entity that launched the instance on your behalf (for
-- example, AWS Management Console or Auto Scaling).
niRequesterId :: Lens' NetworkInterface (Maybe Text)
niRequesterId = lens _niRequesterId (\s a -> s { _niRequesterId = a })

-- | Indicates whether the network interface is being managed by AWS.
niRequesterManaged :: Lens' NetworkInterface (Maybe Bool)
niRequesterManaged =
    lens _niRequesterManaged (\s a -> s { _niRequesterManaged = a })

-- | Indicates whether traffic to or from the instance is validated.
niSourceDestCheck :: Lens' NetworkInterface (Maybe Bool)
niSourceDestCheck =
    lens _niSourceDestCheck (\s a -> s { _niSourceDestCheck = a })

-- | The status of the network interface.
niStatus :: Lens' NetworkInterface (Maybe Text)
niStatus = lens _niStatus (\s a -> s { _niStatus = a })

-- | The ID of the subnet.
niSubnetId :: Lens' NetworkInterface (Maybe Text)
niSubnetId = lens _niSubnetId (\s a -> s { _niSubnetId = a })

-- | Any tags assigned to the network interface.
niTagSet :: Lens' NetworkInterface [Tag]
niTagSet = lens _niTagSet (\s a -> s { _niTagSet = a })

-- | The ID of the VPC.
niVpcId :: Lens' NetworkInterface (Maybe Text)
niVpcId = lens _niVpcId (\s a -> s { _niVpcId = a })

instance FromXML NetworkInterface where
    parseXML c = NetworkInterface
        <$> c .:? "association"
        <*> c .:? "attachment"
        <*> c .:? "availabilityZone"
        <*> c .:? "description"
        <*> c .: "groupSet"
        <*> c .:? "macAddress"
        <*> c .:? "networkInterfaceId"
        <*> c .:? "ownerId"
        <*> c .:? "privateDnsName"
        <*> c .:? "privateIpAddress"
        <*> c .: "privateIpAddressesSet"
        <*> c .:? "requesterId"
        <*> c .:? "requesterManaged"
        <*> c .:? "sourceDestCheck"
        <*> c .:? "status"
        <*> c .:? "subnetId"
        <*> c .: "tagSet"
        <*> c .:? "vpcId"

instance ToQuery NetworkInterface

data TelemetryStatus
    = Down -- ^ DOWN
    | Up   -- ^ UP
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable TelemetryStatus

instance FromText TelemetryStatus where
    parser = match "DOWN" Down
         <|> match "UP"   Up

instance ToText TelemetryStatus where
    toText = \case
        Down -> "DOWN"
        Up   -> "UP"

instance FromXML TelemetryStatus where
    parseXML = fromXMLText "TelemetryStatus"

instance ToQuery TelemetryStatus

data Subnet = Subnet
    { _s1AvailabilityZone        :: Maybe Text
    , _s1AvailableIpAddressCount :: Maybe Int
    , _s1CidrBlock               :: Maybe Text
    , _s1DefaultForAz            :: Maybe Bool
    , _s1MapPublicIpOnLaunch     :: Maybe Bool
    , _s1State                   :: Maybe Text
    , _s1SubnetId                :: Maybe Text
    , _s1Tags                    :: [Tag]
    , _s1VpcId                   :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'Subnet' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 's1AvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 's1AvailableIpAddressCount' @::@ 'Maybe' 'Int'
--
-- * 's1CidrBlock' @::@ 'Maybe' 'Text'
--
-- * 's1DefaultForAz' @::@ 'Maybe' 'Bool'
--
-- * 's1MapPublicIpOnLaunch' @::@ 'Maybe' 'Bool'
--
-- * 's1State' @::@ 'Maybe' 'Text'
--
-- * 's1SubnetId' @::@ 'Maybe' 'Text'
--
-- * 's1Tags' @::@ ['Tag']
--
-- * 's1VpcId' @::@ 'Maybe' 'Text'
--
subnet :: Subnet
subnet = Subnet
    { _s1SubnetId                = Nothing
    , _s1State                   = Nothing
    , _s1VpcId                   = Nothing
    , _s1CidrBlock               = Nothing
    , _s1AvailableIpAddressCount = Nothing
    , _s1AvailabilityZone        = Nothing
    , _s1DefaultForAz            = Nothing
    , _s1MapPublicIpOnLaunch     = Nothing
    , _s1Tags                    = mempty
    }

-- | The Availability Zone of the subnet.
s1AvailabilityZone :: Lens' Subnet (Maybe Text)
s1AvailabilityZone =
    lens _s1AvailabilityZone (\s a -> s { _s1AvailabilityZone = a })

-- | The number of unused IP addresses in the subnet. Note that the IP
-- addresses for any stopped instances are considered unavailable.
s1AvailableIpAddressCount :: Lens' Subnet (Maybe Int)
s1AvailableIpAddressCount =
    lens _s1AvailableIpAddressCount
        (\s a -> s { _s1AvailableIpAddressCount = a })

-- | The CIDR block assigned to the subnet.
s1CidrBlock :: Lens' Subnet (Maybe Text)
s1CidrBlock = lens _s1CidrBlock (\s a -> s { _s1CidrBlock = a })

-- | Indicates whether this is the default subnet for the Availability Zone.
s1DefaultForAz :: Lens' Subnet (Maybe Bool)
s1DefaultForAz = lens _s1DefaultForAz (\s a -> s { _s1DefaultForAz = a })

-- | Indicates whether instances launched in this subnet receive a public IP
-- address.
s1MapPublicIpOnLaunch :: Lens' Subnet (Maybe Bool)
s1MapPublicIpOnLaunch =
    lens _s1MapPublicIpOnLaunch (\s a -> s { _s1MapPublicIpOnLaunch = a })

-- | The current state of the subnet.
s1State :: Lens' Subnet (Maybe Text)
s1State = lens _s1State (\s a -> s { _s1State = a })

-- | The ID of the subnet.
s1SubnetId :: Lens' Subnet (Maybe Text)
s1SubnetId = lens _s1SubnetId (\s a -> s { _s1SubnetId = a })

-- | Any tags assigned to the subnet.
s1Tags :: Lens' Subnet [Tag]
s1Tags = lens _s1Tags (\s a -> s { _s1Tags = a })

-- | The ID of the VPC the subnet is in.
s1VpcId :: Lens' Subnet (Maybe Text)
s1VpcId = lens _s1VpcId (\s a -> s { _s1VpcId = a })

instance FromXML Subnet where
    parseXML c = Subnet
        <$> c .:? "availabilityZone"
        <*> c .:? "availableIpAddressCount"
        <*> c .:? "cidrBlock"
        <*> c .:? "defaultForAz"
        <*> c .:? "mapPublicIpOnLaunch"
        <*> c .:? "state"
        <*> c .:? "subnetId"
        <*> c .: "tagSet"
        <*> c .:? "vpcId"

instance ToQuery Subnet

data KeyPairInfo = KeyPairInfo
    { _kpiKeyFingerprint :: Maybe Text
    , _kpiKeyName        :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'KeyPairInfo' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'kpiKeyFingerprint' @::@ 'Maybe' 'Text'
--
-- * 'kpiKeyName' @::@ 'Maybe' 'Text'
--
keyPairInfo :: KeyPairInfo
keyPairInfo = KeyPairInfo
    { _kpiKeyName        = Nothing
    , _kpiKeyFingerprint = Nothing
    }

-- | If you used CreateKeyPair to create the key pair, this is the SHA-1
-- digest of the DER encoded private key. If you used ImportKeyPair to
-- provide AWS the public key, this is the MD5 public key fingerprint as
-- specified in section 4 of RFC4716.
kpiKeyFingerprint :: Lens' KeyPairInfo (Maybe Text)
kpiKeyFingerprint =
    lens _kpiKeyFingerprint (\s a -> s { _kpiKeyFingerprint = a })

-- | The name of the key pair.
kpiKeyName :: Lens' KeyPairInfo (Maybe Text)
kpiKeyName = lens _kpiKeyName (\s a -> s { _kpiKeyName = a })

instance FromXML KeyPairInfo where
    parseXML c = KeyPairInfo
        <$> c .:? "keyFingerprint"
        <*> c .:? "keyName"

instance ToQuery KeyPairInfo

data LaunchPermissionModifications = LaunchPermissionModifications
    { _lpmAdd    :: [LaunchPermission]
    , _lpmRemove :: [LaunchPermission]
    } deriving (Eq, Show, Generic)

-- | 'LaunchPermissionModifications' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lpmAdd' @::@ ['LaunchPermission']
--
-- * 'lpmRemove' @::@ ['LaunchPermission']
--
launchPermissionModifications :: LaunchPermissionModifications
launchPermissionModifications = LaunchPermissionModifications
    { _lpmAdd    = mempty
    , _lpmRemove = mempty
    }

-- | The AWS account ID to add to the list of launch permissions for the AMI.
lpmAdd :: Lens' LaunchPermissionModifications [LaunchPermission]
lpmAdd = lens _lpmAdd (\s a -> s { _lpmAdd = a })

-- | The AWS account ID to remove from the list of launch permissions for the
-- AMI.
lpmRemove :: Lens' LaunchPermissionModifications [LaunchPermission]
lpmRemove = lens _lpmRemove (\s a -> s { _lpmRemove = a })

instance FromXML LaunchPermissionModifications where
    parseXML c = LaunchPermissionModifications
        <$> c .: "Add"
        <*> c .: "Remove"

instance ToQuery LaunchPermissionModifications

data SnapshotState
    = Completed -- ^ completed
    | Error     -- ^ error
    | Pending   -- ^ pending
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable SnapshotState

instance FromText SnapshotState where
    parser = match "completed" Completed
         <|> match "error"     Error
         <|> match "pending"   Pending

instance ToText SnapshotState where
    toText = \case
        Completed -> "completed"
        Error     -> "error"
        Pending   -> "pending"

instance FromXML SnapshotState where
    parseXML = fromXMLText "SnapshotState"

instance ToQuery SnapshotState

data InstanceNetworkInterfaceAssociation = InstanceNetworkInterfaceAssociation
    { _iniaIpOwnerId     :: Maybe Text
    , _iniaPublicDnsName :: Maybe Text
    , _iniaPublicIp      :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'InstanceNetworkInterfaceAssociation' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iniaIpOwnerId' @::@ 'Maybe' 'Text'
--
-- * 'iniaPublicDnsName' @::@ 'Maybe' 'Text'
--
-- * 'iniaPublicIp' @::@ 'Maybe' 'Text'
--
instanceNetworkInterfaceAssociation :: InstanceNetworkInterfaceAssociation
instanceNetworkInterfaceAssociation = InstanceNetworkInterfaceAssociation
    { _iniaPublicIp      = Nothing
    , _iniaPublicDnsName = Nothing
    , _iniaIpOwnerId     = Nothing
    }

-- | The ID of the owner of the Elastic IP address.
iniaIpOwnerId :: Lens' InstanceNetworkInterfaceAssociation (Maybe Text)
iniaIpOwnerId = lens _iniaIpOwnerId (\s a -> s { _iniaIpOwnerId = a })

-- | The public DNS name.
iniaPublicDnsName :: Lens' InstanceNetworkInterfaceAssociation (Maybe Text)
iniaPublicDnsName =
    lens _iniaPublicDnsName (\s a -> s { _iniaPublicDnsName = a })

-- | The public IP address or Elastic IP address bound to the network
-- interface.
iniaPublicIp :: Lens' InstanceNetworkInterfaceAssociation (Maybe Text)
iniaPublicIp = lens _iniaPublicIp (\s a -> s { _iniaPublicIp = a })

instance FromXML InstanceNetworkInterfaceAssociation where
    parseXML c = InstanceNetworkInterfaceAssociation
        <$> c .:? "ipOwnerId"
        <*> c .:? "publicDnsName"
        <*> c .:? "publicIp"

instance ToQuery InstanceNetworkInterfaceAssociation

data DiskImageDetail = DiskImageDetail
    { _didBytes             :: Integer
    , _didFormat            :: Text
    , _didImportManifestUrl :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DiskImageDetail' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'didBytes' @::@ 'Integer'
--
-- * 'didFormat' @::@ 'Text'
--
-- * 'didImportManifestUrl' @::@ 'Text'
--
diskImageDetail :: Text -- ^ 'didFormat'
                -> Integer -- ^ 'didBytes'
                -> Text -- ^ 'didImportManifestUrl'
                -> DiskImageDetail
diskImageDetail p1 p2 p3 = DiskImageDetail
    { _didFormat            = p1
    , _didBytes             = p2
    , _didImportManifestUrl = p3
    }

didBytes :: Lens' DiskImageDetail Integer
didBytes = lens _didBytes (\s a -> s { _didBytes = a })

-- | The disk image format.
didFormat :: Lens' DiskImageDetail Text
didFormat = lens _didFormat (\s a -> s { _didFormat = a })

-- | A presigned URL for the import manifest stored in Amazon S3. For
-- information about creating a presigned URL for an Amazon S3 object, read
-- the "Query String Request Authentication Alternative" section of the
-- Authenticating REST Requests topic in the Amazon Simple Storage Service
-- Developer Guide.
didImportManifestUrl :: Lens' DiskImageDetail Text
didImportManifestUrl =
    lens _didImportManifestUrl (\s a -> s { _didImportManifestUrl = a })

instance FromXML DiskImageDetail where
    parseXML c = DiskImageDetail
        <$> c .: "bytes"
        <*> c .: "format"
        <*> c .: "importManifestUrl"

instance ToQuery DiskImageDetail

data InstancePrivateIpAddress = InstancePrivateIpAddress
    { _ipiaAssociation      :: Maybe InstanceNetworkInterfaceAssociation
    , _ipiaPrimary          :: Maybe Bool
    , _ipiaPrivateDnsName   :: Maybe Text
    , _ipiaPrivateIpAddress :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'InstancePrivateIpAddress' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ipiaAssociation' @::@ 'Maybe' 'InstanceNetworkInterfaceAssociation'
--
-- * 'ipiaPrimary' @::@ 'Maybe' 'Bool'
--
-- * 'ipiaPrivateDnsName' @::@ 'Maybe' 'Text'
--
-- * 'ipiaPrivateIpAddress' @::@ 'Maybe' 'Text'
--
instancePrivateIpAddress :: InstancePrivateIpAddress
instancePrivateIpAddress = InstancePrivateIpAddress
    { _ipiaPrivateIpAddress = Nothing
    , _ipiaPrivateDnsName   = Nothing
    , _ipiaPrimary          = Nothing
    , _ipiaAssociation      = Nothing
    }

-- | The association information for an Elastic IP address for the network
-- interface.
ipiaAssociation :: Lens' InstancePrivateIpAddress (Maybe InstanceNetworkInterfaceAssociation)
ipiaAssociation = lens _ipiaAssociation (\s a -> s { _ipiaAssociation = a })

-- | Indicates whether this IP address is the primary private IP address of
-- the network interface.
ipiaPrimary :: Lens' InstancePrivateIpAddress (Maybe Bool)
ipiaPrimary = lens _ipiaPrimary (\s a -> s { _ipiaPrimary = a })

-- | The private DNS name.
ipiaPrivateDnsName :: Lens' InstancePrivateIpAddress (Maybe Text)
ipiaPrivateDnsName =
    lens _ipiaPrivateDnsName (\s a -> s { _ipiaPrivateDnsName = a })

-- | The private IP address of the network interface.
ipiaPrivateIpAddress :: Lens' InstancePrivateIpAddress (Maybe Text)
ipiaPrivateIpAddress =
    lens _ipiaPrivateIpAddress (\s a -> s { _ipiaPrivateIpAddress = a })

instance FromXML InstancePrivateIpAddress where
    parseXML c = InstancePrivateIpAddress
        <$> c .:? "association"
        <*> c .:? "primary"
        <*> c .:? "privateDnsName"
        <*> c .:? "privateIpAddress"

instance ToQuery InstancePrivateIpAddress

data CancelledSpotInstanceRequest = CancelledSpotInstanceRequest
    { _csiSpotInstanceRequestId :: Maybe Text
    , _csiState                 :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CancelledSpotInstanceRequest' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csiSpotInstanceRequestId' @::@ 'Maybe' 'Text'
--
-- * 'csiState' @::@ 'Maybe' 'Text'
--
cancelledSpotInstanceRequest :: CancelledSpotInstanceRequest
cancelledSpotInstanceRequest = CancelledSpotInstanceRequest
    { _csiSpotInstanceRequestId = Nothing
    , _csiState                 = Nothing
    }

-- | The ID of the Spot Instance request.
csiSpotInstanceRequestId :: Lens' CancelledSpotInstanceRequest (Maybe Text)
csiSpotInstanceRequestId =
    lens _csiSpotInstanceRequestId
        (\s a -> s { _csiSpotInstanceRequestId = a })

-- | The state of the Spot Instance request.
csiState :: Lens' CancelledSpotInstanceRequest (Maybe Text)
csiState = lens _csiState (\s a -> s { _csiState = a })

instance FromXML CancelledSpotInstanceRequest where
    parseXML c = CancelledSpotInstanceRequest
        <$> c .:? "spotInstanceRequestId"
        <*> c .:? "state"

instance ToQuery CancelledSpotInstanceRequest

newtype VpnConnectionOptionsSpecification = VpnConnectionOptionsSpecification
    { _vcosStaticRoutesOnly :: Maybe Bool
    } deriving (Eq, Ord, Show, Generic)

-- | 'VpnConnectionOptionsSpecification' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vcosStaticRoutesOnly' @::@ 'Maybe' 'Bool'
--
vpnConnectionOptionsSpecification :: VpnConnectionOptionsSpecification
vpnConnectionOptionsSpecification = VpnConnectionOptionsSpecification
    { _vcosStaticRoutesOnly = Nothing
    }

-- | Indicates whether the VPN connection uses static routes only. Static
-- routes must be used for devices that don't support BGP.
vcosStaticRoutesOnly :: Lens' VpnConnectionOptionsSpecification (Maybe Bool)
vcosStaticRoutesOnly =
    lens _vcosStaticRoutesOnly (\s a -> s { _vcosStaticRoutesOnly = a })

instance FromXML VpnConnectionOptionsSpecification where
    parseXML c = VpnConnectionOptionsSpecification
        <$> c .:? "staticRoutesOnly"

instance ToQuery VpnConnectionOptionsSpecification

data Address = Address
    { _aAllocationId            :: Maybe Text
    , _aAssociationId           :: Maybe Text
    , _aDomain                  :: Maybe Text
    , _aInstanceId              :: Maybe Text
    , _aNetworkInterfaceId      :: Maybe Text
    , _aNetworkInterfaceOwnerId :: Maybe Text
    , _aPrivateIpAddress        :: Maybe Text
    , _aPublicIp                :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'Address' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aAllocationId' @::@ 'Maybe' 'Text'
--
-- * 'aAssociationId' @::@ 'Maybe' 'Text'
--
-- * 'aDomain' @::@ 'Maybe' 'Text'
--
-- * 'aInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'aNetworkInterfaceId' @::@ 'Maybe' 'Text'
--
-- * 'aNetworkInterfaceOwnerId' @::@ 'Maybe' 'Text'
--
-- * 'aPrivateIpAddress' @::@ 'Maybe' 'Text'
--
-- * 'aPublicIp' @::@ 'Maybe' 'Text'
--
address :: Address
address = Address
    { _aInstanceId              = Nothing
    , _aPublicIp                = Nothing
    , _aAllocationId            = Nothing
    , _aAssociationId           = Nothing
    , _aDomain                  = Nothing
    , _aNetworkInterfaceId      = Nothing
    , _aNetworkInterfaceOwnerId = Nothing
    , _aPrivateIpAddress        = Nothing
    }

-- | The ID representing the allocation of the address for use with EC2-VPC.
aAllocationId :: Lens' Address (Maybe Text)
aAllocationId = lens _aAllocationId (\s a -> s { _aAllocationId = a })

-- | The ID representing the association of the address with an instance in a
-- VPC.
aAssociationId :: Lens' Address (Maybe Text)
aAssociationId = lens _aAssociationId (\s a -> s { _aAssociationId = a })

-- | Indicates whether this Elastic IP address is for use with instances in
-- EC2-Classic (standard) or instances in a VPC (vpc).
aDomain :: Lens' Address (Maybe Text)
aDomain = lens _aDomain (\s a -> s { _aDomain = a })

-- | The ID of the instance the address is associated with (if any).
aInstanceId :: Lens' Address (Maybe Text)
aInstanceId = lens _aInstanceId (\s a -> s { _aInstanceId = a })

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

-- | The Elastic IP address.
aPublicIp :: Lens' Address (Maybe Text)
aPublicIp = lens _aPublicIp (\s a -> s { _aPublicIp = a })

instance FromXML Address where
    parseXML c = Address
        <$> c .:? "allocationId"
        <*> c .:? "associationId"
        <*> c .:? "domain"
        <*> c .:? "instanceId"
        <*> c .:? "networkInterfaceId"
        <*> c .:? "networkInterfaceOwnerId"
        <*> c .:? "privateIpAddress"
        <*> c .:? "publicIp"

instance ToQuery Address

data VolumeAttachmentState
    = VASAttached  -- ^ attached
    | VASAttaching -- ^ attaching
    | VASDetached  -- ^ detached
    | VASDetaching -- ^ detaching
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable VolumeAttachmentState

instance FromText VolumeAttachmentState where
    parser = match "attached"  VASAttached
         <|> match "attaching" VASAttaching
         <|> match "detached"  VASDetached
         <|> match "detaching" VASDetaching

instance ToText VolumeAttachmentState where
    toText = \case
        VASAttached  -> "attached"
        VASAttaching -> "attaching"
        VASDetached  -> "detached"
        VASDetaching -> "detaching"

instance FromXML VolumeAttachmentState where
    parseXML = fromXMLText "VolumeAttachmentState"

instance ToQuery VolumeAttachmentState

data LaunchPermission = LaunchPermission
    { _lpGroup  :: Maybe Text
    , _lpUserId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'LaunchPermission' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lpGroup' @::@ 'Maybe' 'Text'
--
-- * 'lpUserId' @::@ 'Maybe' 'Text'
--
launchPermission :: LaunchPermission
launchPermission = LaunchPermission
    { _lpUserId = Nothing
    , _lpGroup  = Nothing
    }

-- | The name of the group.
lpGroup :: Lens' LaunchPermission (Maybe Text)
lpGroup = lens _lpGroup (\s a -> s { _lpGroup = a })

-- | The AWS account ID.
lpUserId :: Lens' LaunchPermission (Maybe Text)
lpUserId = lens _lpUserId (\s a -> s { _lpUserId = a })

instance FromXML LaunchPermission where
    parseXML c = LaunchPermission
        <$> c .:? "group"
        <*> c .:? "userId"

instance ToQuery LaunchPermission

data RouteState
    = Active    -- ^ active
    | Blackhole -- ^ blackhole
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable RouteState

instance FromText RouteState where
    parser = match "active"    Active
         <|> match "blackhole" Blackhole

instance ToText RouteState where
    toText = \case
        Active    -> "active"
        Blackhole -> "blackhole"

instance FromXML RouteState where
    parseXML = fromXMLText "RouteState"

instance ToQuery RouteState

data RouteTableAssociation = RouteTableAssociation
    { _rtaMain                    :: Maybe Bool
    , _rtaRouteTableAssociationId :: Maybe Text
    , _rtaRouteTableId            :: Maybe Text
    , _rtaSubnetId                :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'RouteTableAssociation' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtaMain' @::@ 'Maybe' 'Bool'
--
-- * 'rtaRouteTableAssociationId' @::@ 'Maybe' 'Text'
--
-- * 'rtaRouteTableId' @::@ 'Maybe' 'Text'
--
-- * 'rtaSubnetId' @::@ 'Maybe' 'Text'
--
routeTableAssociation :: RouteTableAssociation
routeTableAssociation = RouteTableAssociation
    { _rtaRouteTableAssociationId = Nothing
    , _rtaRouteTableId            = Nothing
    , _rtaSubnetId                = Nothing
    , _rtaMain                    = Nothing
    }

-- | Indicates whether this is the main route table.
rtaMain :: Lens' RouteTableAssociation (Maybe Bool)
rtaMain = lens _rtaMain (\s a -> s { _rtaMain = a })

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

instance FromXML RouteTableAssociation where
    parseXML c = RouteTableAssociation
        <$> c .:? "main"
        <*> c .:? "routeTableAssociationId"
        <*> c .:? "routeTableId"
        <*> c .:? "subnetId"

instance ToQuery RouteTableAssociation

data BundleTaskState
    = BTSBundling           -- ^ bundling
    | BTSCancelling         -- ^ cancelling
    | BTSComplete           -- ^ complete
    | BTSFailed             -- ^ failed
    | BTSPending            -- ^ pending
    | BTSStoring            -- ^ storing
    | BTSWaitingForShutdown -- ^ waiting-for-shutdown
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable BundleTaskState

instance FromText BundleTaskState where
    parser = match "bundling"             BTSBundling
         <|> match "cancelling"           BTSCancelling
         <|> match "complete"             BTSComplete
         <|> match "failed"               BTSFailed
         <|> match "pending"              BTSPending
         <|> match "storing"              BTSStoring
         <|> match "waiting-for-shutdown" BTSWaitingForShutdown

instance ToText BundleTaskState where
    toText = \case
        BTSBundling           -> "bundling"
        BTSCancelling         -> "cancelling"
        BTSComplete           -> "complete"
        BTSFailed             -> "failed"
        BTSPending            -> "pending"
        BTSStoring            -> "storing"
        BTSWaitingForShutdown -> "waiting-for-shutdown"

instance FromXML BundleTaskState where
    parseXML = fromXMLText "BundleTaskState"

instance ToQuery BundleTaskState

data PortRange = PortRange
    { _prFrom :: Maybe Int
    , _prTo   :: Maybe Int
    } deriving (Eq, Ord, Show, Generic)

-- | 'PortRange' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prFrom' @::@ 'Maybe' 'Int'
--
-- * 'prTo' @::@ 'Maybe' 'Int'
--
portRange :: PortRange
portRange = PortRange
    { _prFrom = Nothing
    , _prTo   = Nothing
    }

-- | The first port in the range.
prFrom :: Lens' PortRange (Maybe Int)
prFrom = lens _prFrom (\s a -> s { _prFrom = a })

-- | The last port in the range.
prTo :: Lens' PortRange (Maybe Int)
prTo = lens _prTo (\s a -> s { _prTo = a })

instance FromXML PortRange where
    parseXML c = PortRange
        <$> c .:? "from"
        <*> c .:? "to"

instance ToQuery PortRange

data VpcAttributeName
    = EnableDnsHostnames -- ^ enableDnsHostnames
    | EnableDnsSupport   -- ^ enableDnsSupport
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable VpcAttributeName

instance FromText VpcAttributeName where
    parser = match "enableDnsHostnames" EnableDnsHostnames
         <|> match "enableDnsSupport"   EnableDnsSupport

instance ToText VpcAttributeName where
    toText = \case
        EnableDnsHostnames -> "enableDnsHostnames"
        EnableDnsSupport   -> "enableDnsSupport"

instance FromXML VpcAttributeName where
    parseXML = fromXMLText "VpcAttributeName"

instance ToQuery VpcAttributeName

data ReservedInstancesConfiguration = ReservedInstancesConfiguration
    { _ricAvailabilityZone :: Maybe Text
    , _ricInstanceCount    :: Maybe Int
    , _ricInstanceType     :: Maybe Text
    , _ricPlatform         :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ReservedInstancesConfiguration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ricAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'ricInstanceCount' @::@ 'Maybe' 'Int'
--
-- * 'ricInstanceType' @::@ 'Maybe' 'Text'
--
-- * 'ricPlatform' @::@ 'Maybe' 'Text'
--
reservedInstancesConfiguration :: ReservedInstancesConfiguration
reservedInstancesConfiguration = ReservedInstancesConfiguration
    { _ricAvailabilityZone = Nothing
    , _ricPlatform         = Nothing
    , _ricInstanceCount    = Nothing
    , _ricInstanceType     = Nothing
    }

-- | The Availability Zone for the modified Reserved Instances.
ricAvailabilityZone :: Lens' ReservedInstancesConfiguration (Maybe Text)
ricAvailabilityZone =
    lens _ricAvailabilityZone (\s a -> s { _ricAvailabilityZone = a })

-- | The number of modified Reserved Instances.
ricInstanceCount :: Lens' ReservedInstancesConfiguration (Maybe Int)
ricInstanceCount = lens _ricInstanceCount (\s a -> s { _ricInstanceCount = a })

-- | The instance type for the modified Reserved Instances.
ricInstanceType :: Lens' ReservedInstancesConfiguration (Maybe Text)
ricInstanceType = lens _ricInstanceType (\s a -> s { _ricInstanceType = a })

-- | The network platform of the modified Reserved Instances, which is either
-- EC2-Classic or EC2-VPC.
ricPlatform :: Lens' ReservedInstancesConfiguration (Maybe Text)
ricPlatform = lens _ricPlatform (\s a -> s { _ricPlatform = a })

instance FromXML ReservedInstancesConfiguration where
    parseXML c = ReservedInstancesConfiguration
        <$> c .:? "availabilityZone"
        <*> c .:? "instanceCount"
        <*> c .:? "instanceType"
        <*> c .:? "platform"

instance ToQuery ReservedInstancesConfiguration

data VolumeStatusDetails = VolumeStatusDetails
    { _vsdName   :: Maybe Text
    , _vsdStatus :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'VolumeStatusDetails' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vsdName' @::@ 'Maybe' 'Text'
--
-- * 'vsdStatus' @::@ 'Maybe' 'Text'
--
volumeStatusDetails :: VolumeStatusDetails
volumeStatusDetails = VolumeStatusDetails
    { _vsdName   = Nothing
    , _vsdStatus = Nothing
    }

-- | The name of the volume status.
vsdName :: Lens' VolumeStatusDetails (Maybe Text)
vsdName = lens _vsdName (\s a -> s { _vsdName = a })

-- | The intended status of the volume status.
vsdStatus :: Lens' VolumeStatusDetails (Maybe Text)
vsdStatus = lens _vsdStatus (\s a -> s { _vsdStatus = a })

instance FromXML VolumeStatusDetails where
    parseXML c = VolumeStatusDetails
        <$> c .:? "name"
        <*> c .:? "status"

instance ToQuery VolumeStatusDetails

data SpotInstanceState
    = SISActive    -- ^ active
    | SISCancelled -- ^ cancelled
    | SISClosed    -- ^ closed
    | SISFailed    -- ^ failed
    | SISOpen      -- ^ open
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable SpotInstanceState

instance FromText SpotInstanceState where
    parser = match "active"    SISActive
         <|> match "cancelled" SISCancelled
         <|> match "closed"    SISClosed
         <|> match "failed"    SISFailed
         <|> match "open"      SISOpen

instance ToText SpotInstanceState where
    toText = \case
        SISActive    -> "active"
        SISCancelled -> "cancelled"
        SISClosed    -> "closed"
        SISFailed    -> "failed"
        SISOpen      -> "open"

instance FromXML SpotInstanceState where
    parseXML = fromXMLText "SpotInstanceState"

instance ToQuery SpotInstanceState

newtype VpnConnectionOptions = VpnConnectionOptions
    { _vcoStaticRoutesOnly :: Maybe Bool
    } deriving (Eq, Ord, Show, Generic)

-- | 'VpnConnectionOptions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vcoStaticRoutesOnly' @::@ 'Maybe' 'Bool'
--
vpnConnectionOptions :: VpnConnectionOptions
vpnConnectionOptions = VpnConnectionOptions
    { _vcoStaticRoutesOnly = Nothing
    }

-- | Indicates whether the VPN connection uses static routes only. Static
-- routes must be used for devices that don't support BGP.
vcoStaticRoutesOnly :: Lens' VpnConnectionOptions (Maybe Bool)
vcoStaticRoutesOnly =
    lens _vcoStaticRoutesOnly (\s a -> s { _vcoStaticRoutesOnly = a })

instance FromXML VpnConnectionOptions where
    parseXML c = VpnConnectionOptions
        <$> c .:? "staticRoutesOnly"

instance ToQuery VpnConnectionOptions

data UserIdGroupPair = UserIdGroupPair
    { _uigpGroupId   :: Maybe Text
    , _uigpGroupName :: Maybe Text
    , _uigpUserId    :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'UserIdGroupPair' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uigpGroupId' @::@ 'Maybe' 'Text'
--
-- * 'uigpGroupName' @::@ 'Maybe' 'Text'
--
-- * 'uigpUserId' @::@ 'Maybe' 'Text'
--
userIdGroupPair :: UserIdGroupPair
userIdGroupPair = UserIdGroupPair
    { _uigpUserId    = Nothing
    , _uigpGroupName = Nothing
    , _uigpGroupId   = Nothing
    }

-- | The name of the security group in the specified AWS account.
uigpGroupId :: Lens' UserIdGroupPair (Maybe Text)
uigpGroupId = lens _uigpGroupId (\s a -> s { _uigpGroupId = a })

-- | The ID of the security group owned by the specified AWS account.
uigpGroupName :: Lens' UserIdGroupPair (Maybe Text)
uigpGroupName = lens _uigpGroupName (\s a -> s { _uigpGroupName = a })

-- | The ID of an AWS account.
uigpUserId :: Lens' UserIdGroupPair (Maybe Text)
uigpUserId = lens _uigpUserId (\s a -> s { _uigpUserId = a })

instance FromXML UserIdGroupPair where
    parseXML c = UserIdGroupPair
        <$> c .:? "groupId"
        <*> c .:? "groupName"
        <*> c .:? "userId"

instance ToQuery UserIdGroupPair

data InstanceStatusSummary = InstanceStatusSummary
    { _issDetails :: [InstanceStatusDetails]
    , _issStatus  :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'InstanceStatusSummary' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'issDetails' @::@ ['InstanceStatusDetails']
--
-- * 'issStatus' @::@ 'Maybe' 'Text'
--
instanceStatusSummary :: InstanceStatusSummary
instanceStatusSummary = InstanceStatusSummary
    { _issStatus  = Nothing
    , _issDetails = mempty
    }

-- | The system instance health or application instance health.
issDetails :: Lens' InstanceStatusSummary [InstanceStatusDetails]
issDetails = lens _issDetails (\s a -> s { _issDetails = a })

-- | The status.
issStatus :: Lens' InstanceStatusSummary (Maybe Text)
issStatus = lens _issStatus (\s a -> s { _issStatus = a })

instance FromXML InstanceStatusSummary where
    parseXML c = InstanceStatusSummary
        <$> c .: "details"
        <*> c .:? "status"

instance ToQuery InstanceStatusSummary

data SpotPlacement = SpotPlacement
    { _sp1AvailabilityZone :: Maybe Text
    , _sp1GroupName        :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'SpotPlacement' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sp1AvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'sp1GroupName' @::@ 'Maybe' 'Text'
--
spotPlacement :: SpotPlacement
spotPlacement = SpotPlacement
    { _sp1AvailabilityZone = Nothing
    , _sp1GroupName        = Nothing
    }

-- | The Availability Zone.
sp1AvailabilityZone :: Lens' SpotPlacement (Maybe Text)
sp1AvailabilityZone =
    lens _sp1AvailabilityZone (\s a -> s { _sp1AvailabilityZone = a })

-- | The name of the placement group (for cluster instances).
sp1GroupName :: Lens' SpotPlacement (Maybe Text)
sp1GroupName = lens _sp1GroupName (\s a -> s { _sp1GroupName = a })

instance FromXML SpotPlacement where
    parseXML c = SpotPlacement
        <$> c .:? "availabilityZone"
        <*> c .:? "groupName"

instance ToQuery SpotPlacement

data EbsInstanceBlockDeviceSpecification = EbsInstanceBlockDeviceSpecification
    { _eibdsDeleteOnTermination :: Maybe Bool
    , _eibdsVolumeId            :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'EbsInstanceBlockDeviceSpecification' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eibdsDeleteOnTermination' @::@ 'Maybe' 'Bool'
--
-- * 'eibdsVolumeId' @::@ 'Maybe' 'Text'
--
ebsInstanceBlockDeviceSpecification :: EbsInstanceBlockDeviceSpecification
ebsInstanceBlockDeviceSpecification = EbsInstanceBlockDeviceSpecification
    { _eibdsVolumeId            = Nothing
    , _eibdsDeleteOnTermination = Nothing
    }

-- | Indicates whether the volume is deleted on instance termination.
eibdsDeleteOnTermination :: Lens' EbsInstanceBlockDeviceSpecification (Maybe Bool)
eibdsDeleteOnTermination =
    lens _eibdsDeleteOnTermination
        (\s a -> s { _eibdsDeleteOnTermination = a })

-- | The ID of the Amazon EBS volume.
eibdsVolumeId :: Lens' EbsInstanceBlockDeviceSpecification (Maybe Text)
eibdsVolumeId = lens _eibdsVolumeId (\s a -> s { _eibdsVolumeId = a })

instance FromXML EbsInstanceBlockDeviceSpecification where
    parseXML c = EbsInstanceBlockDeviceSpecification
        <$> c .:? "deleteOnTermination"
        <*> c .:? "volumeId"

instance ToQuery EbsInstanceBlockDeviceSpecification

data NetworkAclAssociation = NetworkAclAssociation
    { _naaNetworkAclAssociationId :: Maybe Text
    , _naaNetworkAclId            :: Maybe Text
    , _naaSubnetId                :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'NetworkAclAssociation' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'naaNetworkAclAssociationId' @::@ 'Maybe' 'Text'
--
-- * 'naaNetworkAclId' @::@ 'Maybe' 'Text'
--
-- * 'naaSubnetId' @::@ 'Maybe' 'Text'
--
networkAclAssociation :: NetworkAclAssociation
networkAclAssociation = NetworkAclAssociation
    { _naaNetworkAclAssociationId = Nothing
    , _naaNetworkAclId            = Nothing
    , _naaSubnetId                = Nothing
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
    parseXML c = NetworkAclAssociation
        <$> c .:? "networkAclAssociationId"
        <*> c .:? "networkAclId"
        <*> c .:? "subnetId"

instance ToQuery NetworkAclAssociation

data BundleTask = BundleTask
    { _btBundleId        :: Maybe Text
    , _btBundleTaskError :: Maybe BundleTaskError
    , _btInstanceId      :: Maybe Text
    , _btProgress        :: Maybe Text
    , _btStartTime       :: Maybe RFC822
    , _btState           :: Maybe Text
    , _btStorage         :: Maybe Storage
    , _btUpdateTime      :: Maybe RFC822
    } deriving (Eq, Show, Generic)

-- | 'BundleTask' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'btBundleId' @::@ 'Maybe' 'Text'
--
-- * 'btBundleTaskError' @::@ 'Maybe' 'BundleTaskError'
--
-- * 'btInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'btProgress' @::@ 'Maybe' 'Text'
--
-- * 'btStartTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'btState' @::@ 'Maybe' 'Text'
--
-- * 'btStorage' @::@ 'Maybe' 'Storage'
--
-- * 'btUpdateTime' @::@ 'Maybe' 'UTCTime'
--
bundleTask :: BundleTask
bundleTask = BundleTask
    { _btInstanceId      = Nothing
    , _btBundleId        = Nothing
    , _btState           = Nothing
    , _btStartTime       = Nothing
    , _btUpdateTime      = Nothing
    , _btStorage         = Nothing
    , _btProgress        = Nothing
    , _btBundleTaskError = Nothing
    }

-- | The ID for this bundle task.
btBundleId :: Lens' BundleTask (Maybe Text)
btBundleId = lens _btBundleId (\s a -> s { _btBundleId = a })

-- | If the task fails, a description of the error.
btBundleTaskError :: Lens' BundleTask (Maybe BundleTaskError)
btBundleTaskError =
    lens _btBundleTaskError (\s a -> s { _btBundleTaskError = a })

-- | The ID of the instance associated with this bundle task.
btInstanceId :: Lens' BundleTask (Maybe Text)
btInstanceId = lens _btInstanceId (\s a -> s { _btInstanceId = a })

-- | The level of task completion, as a percent (for example, 20%).
btProgress :: Lens' BundleTask (Maybe Text)
btProgress = lens _btProgress (\s a -> s { _btProgress = a })

-- | The time this task started.
btStartTime :: Lens' BundleTask (Maybe UTCTime)
btStartTime = lens _btStartTime (\s a -> s { _btStartTime = a })
    . mapping _Time

-- | The state of the task.
btState :: Lens' BundleTask (Maybe Text)
btState = lens _btState (\s a -> s { _btState = a })

-- | The Amazon S3 storage locations.
btStorage :: Lens' BundleTask (Maybe Storage)
btStorage = lens _btStorage (\s a -> s { _btStorage = a })

-- | The time of the most recent update for the task.
btUpdateTime :: Lens' BundleTask (Maybe UTCTime)
btUpdateTime = lens _btUpdateTime (\s a -> s { _btUpdateTime = a })
    . mapping _Time

instance FromXML BundleTask where
    parseXML c = BundleTask
        <$> c .:? "bundleId"
        <*> c .:? "error"
        <*> c .:? "instanceId"
        <*> c .:? "progress"
        <*> c .:? "startTime"
        <*> c .:? "state"
        <*> c .:? "storage"
        <*> c .:? "updateTime"

instance ToQuery BundleTask

data InstanceStatusEvent = InstanceStatusEvent
    { _iseCode        :: Maybe Text
    , _iseDescription :: Maybe Text
    , _iseNotAfter    :: Maybe RFC822
    , _iseNotBefore   :: Maybe RFC822
    } deriving (Eq, Ord, Show, Generic)

-- | 'InstanceStatusEvent' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iseCode' @::@ 'Maybe' 'Text'
--
-- * 'iseDescription' @::@ 'Maybe' 'Text'
--
-- * 'iseNotAfter' @::@ 'Maybe' 'UTCTime'
--
-- * 'iseNotBefore' @::@ 'Maybe' 'UTCTime'
--
instanceStatusEvent :: InstanceStatusEvent
instanceStatusEvent = InstanceStatusEvent
    { _iseCode        = Nothing
    , _iseDescription = Nothing
    , _iseNotBefore   = Nothing
    , _iseNotAfter    = Nothing
    }

-- | The associated code of the event.
iseCode :: Lens' InstanceStatusEvent (Maybe Text)
iseCode = lens _iseCode (\s a -> s { _iseCode = a })

-- | A description of the event.
iseDescription :: Lens' InstanceStatusEvent (Maybe Text)
iseDescription = lens _iseDescription (\s a -> s { _iseDescription = a })

-- | The latest scheduled end time for the event.
iseNotAfter :: Lens' InstanceStatusEvent (Maybe UTCTime)
iseNotAfter = lens _iseNotAfter (\s a -> s { _iseNotAfter = a })
    . mapping _Time

-- | The earliest scheduled start time for the event.
iseNotBefore :: Lens' InstanceStatusEvent (Maybe UTCTime)
iseNotBefore = lens _iseNotBefore (\s a -> s { _iseNotBefore = a })
    . mapping _Time

instance FromXML InstanceStatusEvent where
    parseXML c = InstanceStatusEvent
        <$> c .:? "code"
        <*> c .:? "description"
        <*> c .:? "notAfter"
        <*> c .:? "notBefore"

instance ToQuery InstanceStatusEvent

data InstanceType
    = C1Medium   -- ^ c1.medium
    | C1Xlarge   -- ^ c1.xlarge
    | C32xlarge  -- ^ c3.2xlarge
    | C34xlarge  -- ^ c3.4xlarge
    | C38xlarge  -- ^ c3.8xlarge
    | C3Large    -- ^ c3.large
    | C3Xlarge   -- ^ c3.xlarge
    | Cc14xlarge -- ^ cc1.4xlarge
    | Cc28xlarge -- ^ cc2.8xlarge
    | Cg14xlarge -- ^ cg1.4xlarge
    | Cr18xlarge -- ^ cr1.8xlarge
    | G22xlarge  -- ^ g2.2xlarge
    | Hi14xlarge -- ^ hi1.4xlarge
    | Hs18xlarge -- ^ hs1.8xlarge
    | I22xlarge  -- ^ i2.2xlarge
    | I24xlarge  -- ^ i2.4xlarge
    | I28xlarge  -- ^ i2.8xlarge
    | I2Xlarge   -- ^ i2.xlarge
    | M1Large    -- ^ m1.large
    | M1Medium   -- ^ m1.medium
    | M1Small    -- ^ m1.small
    | M1Xlarge   -- ^ m1.xlarge
    | M22xlarge  -- ^ m2.2xlarge
    | M24xlarge  -- ^ m2.4xlarge
    | M2Xlarge   -- ^ m2.xlarge
    | M32xlarge  -- ^ m3.2xlarge
    | M3Large    -- ^ m3.large
    | M3Medium   -- ^ m3.medium
    | M3Xlarge   -- ^ m3.xlarge
    | R32xlarge  -- ^ r3.2xlarge
    | R34xlarge  -- ^ r3.4xlarge
    | R38xlarge  -- ^ r3.8xlarge
    | R3Large    -- ^ r3.large
    | R3Xlarge   -- ^ r3.xlarge
    | T1Micro    -- ^ t1.micro
    | T2Medium   -- ^ t2.medium
    | T2Micro    -- ^ t2.micro
    | T2Small    -- ^ t2.small
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable InstanceType

instance FromText InstanceType where
    parser = match "c1.medium"   C1Medium
         <|> match "c1.xlarge"   C1Xlarge
         <|> match "c3.2xlarge"  C32xlarge
         <|> match "c3.4xlarge"  C34xlarge
         <|> match "c3.8xlarge"  C38xlarge
         <|> match "c3.large"    C3Large
         <|> match "c3.xlarge"   C3Xlarge
         <|> match "cc1.4xlarge" Cc14xlarge
         <|> match "cc2.8xlarge" Cc28xlarge
         <|> match "cg1.4xlarge" Cg14xlarge
         <|> match "cr1.8xlarge" Cr18xlarge
         <|> match "g2.2xlarge"  G22xlarge
         <|> match "hi1.4xlarge" Hi14xlarge
         <|> match "hs1.8xlarge" Hs18xlarge
         <|> match "i2.2xlarge"  I22xlarge
         <|> match "i2.4xlarge"  I24xlarge
         <|> match "i2.8xlarge"  I28xlarge
         <|> match "i2.xlarge"   I2Xlarge
         <|> match "m1.large"    M1Large
         <|> match "m1.medium"   M1Medium
         <|> match "m1.small"    M1Small
         <|> match "m1.xlarge"   M1Xlarge
         <|> match "m2.2xlarge"  M22xlarge
         <|> match "m2.4xlarge"  M24xlarge
         <|> match "m2.xlarge"   M2Xlarge
         <|> match "m3.2xlarge"  M32xlarge
         <|> match "m3.large"    M3Large
         <|> match "m3.medium"   M3Medium
         <|> match "m3.xlarge"   M3Xlarge
         <|> match "r3.2xlarge"  R32xlarge
         <|> match "r3.4xlarge"  R34xlarge
         <|> match "r3.8xlarge"  R38xlarge
         <|> match "r3.large"    R3Large
         <|> match "r3.xlarge"   R3Xlarge
         <|> match "t1.micro"    T1Micro
         <|> match "t2.medium"   T2Medium
         <|> match "t2.micro"    T2Micro
         <|> match "t2.small"    T2Small

instance ToText InstanceType where
    toText = \case
        C1Medium   -> "c1.medium"
        C1Xlarge   -> "c1.xlarge"
        C32xlarge  -> "c3.2xlarge"
        C34xlarge  -> "c3.4xlarge"
        C38xlarge  -> "c3.8xlarge"
        C3Large    -> "c3.large"
        C3Xlarge   -> "c3.xlarge"
        Cc14xlarge -> "cc1.4xlarge"
        Cc28xlarge -> "cc2.8xlarge"
        Cg14xlarge -> "cg1.4xlarge"
        Cr18xlarge -> "cr1.8xlarge"
        G22xlarge  -> "g2.2xlarge"
        Hi14xlarge -> "hi1.4xlarge"
        Hs18xlarge -> "hs1.8xlarge"
        I22xlarge  -> "i2.2xlarge"
        I24xlarge  -> "i2.4xlarge"
        I28xlarge  -> "i2.8xlarge"
        I2Xlarge   -> "i2.xlarge"
        M1Large    -> "m1.large"
        M1Medium   -> "m1.medium"
        M1Small    -> "m1.small"
        M1Xlarge   -> "m1.xlarge"
        M22xlarge  -> "m2.2xlarge"
        M24xlarge  -> "m2.4xlarge"
        M2Xlarge   -> "m2.xlarge"
        M32xlarge  -> "m3.2xlarge"
        M3Large    -> "m3.large"
        M3Medium   -> "m3.medium"
        M3Xlarge   -> "m3.xlarge"
        R32xlarge  -> "r3.2xlarge"
        R34xlarge  -> "r3.4xlarge"
        R38xlarge  -> "r3.8xlarge"
        R3Large    -> "r3.large"
        R3Xlarge   -> "r3.xlarge"
        T1Micro    -> "t1.micro"
        T2Medium   -> "t2.medium"
        T2Micro    -> "t2.micro"
        T2Small    -> "t2.small"

instance FromXML InstanceType where
    parseXML = fromXMLText "InstanceType"

instance ToQuery InstanceType

data Route = Route
    { _rDestinationCidrBlock   :: Maybe Text
    , _rGatewayId              :: Maybe Text
    , _rInstanceId             :: Maybe Text
    , _rInstanceOwnerId        :: Maybe Text
    , _rNetworkInterfaceId     :: Maybe Text
    , _rOrigin                 :: Maybe Text
    , _rState                  :: Maybe Text
    , _rVpcPeeringConnectionId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'Route' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rDestinationCidrBlock' @::@ 'Maybe' 'Text'
--
-- * 'rGatewayId' @::@ 'Maybe' 'Text'
--
-- * 'rInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'rInstanceOwnerId' @::@ 'Maybe' 'Text'
--
-- * 'rNetworkInterfaceId' @::@ 'Maybe' 'Text'
--
-- * 'rOrigin' @::@ 'Maybe' 'Text'
--
-- * 'rState' @::@ 'Maybe' 'Text'
--
-- * 'rVpcPeeringConnectionId' @::@ 'Maybe' 'Text'
--
route :: Route
route = Route
    { _rDestinationCidrBlock   = Nothing
    , _rGatewayId              = Nothing
    , _rInstanceId             = Nothing
    , _rInstanceOwnerId        = Nothing
    , _rNetworkInterfaceId     = Nothing
    , _rVpcPeeringConnectionId = Nothing
    , _rState                  = Nothing
    , _rOrigin                 = Nothing
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
rInstanceOwnerId = lens _rInstanceOwnerId (\s a -> s { _rInstanceOwnerId = a })

-- | The ID of the network interface.
rNetworkInterfaceId :: Lens' Route (Maybe Text)
rNetworkInterfaceId =
    lens _rNetworkInterfaceId (\s a -> s { _rNetworkInterfaceId = a })

-- | Describes how the route was created. CreateRouteTable indicates that
-- route was automatically created when the route table was created.
-- CreateRoute indicates that the route was manually added to the route
-- table. EnableVgwRoutePropagation indicates that the route was propagated
-- by route propagation.
rOrigin :: Lens' Route (Maybe Text)
rOrigin = lens _rOrigin (\s a -> s { _rOrigin = a })

-- | The state of the route. The blackhole state indicates that the route's
-- target isn't available (for example, the specified gateway isn't attached
-- to the VPC, or the specified NAT instance has been terminated).
rState :: Lens' Route (Maybe Text)
rState = lens _rState (\s a -> s { _rState = a })

-- | The ID of the VPC peering connection.
rVpcPeeringConnectionId :: Lens' Route (Maybe Text)
rVpcPeeringConnectionId =
    lens _rVpcPeeringConnectionId (\s a -> s { _rVpcPeeringConnectionId = a })

instance FromXML Route where
    parseXML c = Route
        <$> c .:? "destinationCidrBlock"
        <*> c .:? "gatewayId"
        <*> c .:? "instanceId"
        <*> c .:? "instanceOwnerId"
        <*> c .:? "networkInterfaceId"
        <*> c .:? "origin"
        <*> c .:? "state"
        <*> c .:? "vpcPeeringConnectionId"

instance ToQuery Route

data SpotDatafeedSubscription = SpotDatafeedSubscription
    { _sdsBucket  :: Maybe Text
    , _sdsFault   :: Maybe SpotInstanceStateFault
    , _sdsOwnerId :: Maybe Text
    , _sdsPrefix  :: Maybe Text
    , _sdsState   :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'SpotDatafeedSubscription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sdsBucket' @::@ 'Maybe' 'Text'
--
-- * 'sdsFault' @::@ 'Maybe' 'SpotInstanceStateFault'
--
-- * 'sdsOwnerId' @::@ 'Maybe' 'Text'
--
-- * 'sdsPrefix' @::@ 'Maybe' 'Text'
--
-- * 'sdsState' @::@ 'Maybe' 'Text'
--
spotDatafeedSubscription :: SpotDatafeedSubscription
spotDatafeedSubscription = SpotDatafeedSubscription
    { _sdsOwnerId = Nothing
    , _sdsBucket  = Nothing
    , _sdsPrefix  = Nothing
    , _sdsState   = Nothing
    , _sdsFault   = Nothing
    }

-- | The Amazon S3 bucket where the Spot Instance datafeed is located.
sdsBucket :: Lens' SpotDatafeedSubscription (Maybe Text)
sdsBucket = lens _sdsBucket (\s a -> s { _sdsBucket = a })

-- | The fault codes for the Spot Instance request, if any.
sdsFault :: Lens' SpotDatafeedSubscription (Maybe SpotInstanceStateFault)
sdsFault = lens _sdsFault (\s a -> s { _sdsFault = a })

-- | The AWS account ID of the account.
sdsOwnerId :: Lens' SpotDatafeedSubscription (Maybe Text)
sdsOwnerId = lens _sdsOwnerId (\s a -> s { _sdsOwnerId = a })

-- | The prefix that is prepended to datafeed files.
sdsPrefix :: Lens' SpotDatafeedSubscription (Maybe Text)
sdsPrefix = lens _sdsPrefix (\s a -> s { _sdsPrefix = a })

-- | The state of the Spot Instance datafeed subscription.
sdsState :: Lens' SpotDatafeedSubscription (Maybe Text)
sdsState = lens _sdsState (\s a -> s { _sdsState = a })

instance FromXML SpotDatafeedSubscription where
    parseXML c = SpotDatafeedSubscription
        <$> c .:? "bucket"
        <*> c .:? "fault"
        <*> c .:? "ownerId"
        <*> c .:? "prefix"
        <*> c .:? "state"

instance ToQuery SpotDatafeedSubscription

newtype Storage = Storage
    { _sS3 :: Maybe S3Storage
    } deriving (Eq, Show, Generic)

-- | 'Storage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sS3' @::@ 'Maybe' 'S3Storage'
--
storage :: Storage
storage = Storage
    { _sS3 = Nothing
    }

-- | An Amazon S3 storage location.
sS3 :: Lens' Storage (Maybe S3Storage)
sS3 = lens _sS3 (\s a -> s { _sS3 = a })

instance FromXML Storage where
    parseXML c = Storage
        <$> c .:? "S3"

instance ToQuery Storage

data SecurityGroup = SecurityGroup
    { _sgDescription         :: Text
    , _sgGroupId             :: Text
    , _sgGroupName           :: Text
    , _sgIpPermissions       :: [IpPermission]
    , _sgIpPermissionsEgress :: [IpPermission]
    , _sgOwnerId             :: Text
    , _sgTags                :: [Tag]
    , _sgVpcId               :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'SecurityGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sgDescription' @::@ 'Text'
--
-- * 'sgGroupId' @::@ 'Text'
--
-- * 'sgGroupName' @::@ 'Text'
--
-- * 'sgIpPermissions' @::@ ['IpPermission']
--
-- * 'sgIpPermissionsEgress' @::@ ['IpPermission']
--
-- * 'sgOwnerId' @::@ 'Text'
--
-- * 'sgTags' @::@ ['Tag']
--
-- * 'sgVpcId' @::@ 'Maybe' 'Text'
--
securityGroup :: Text -- ^ 'sgOwnerId'
              -> Text -- ^ 'sgGroupName'
              -> Text -- ^ 'sgGroupId'
              -> Text -- ^ 'sgDescription'
              -> SecurityGroup
securityGroup p1 p2 p3 p4 = SecurityGroup
    { _sgOwnerId             = p1
    , _sgGroupName           = p2
    , _sgGroupId             = p3
    , _sgDescription         = p4
    , _sgIpPermissions       = mempty
    , _sgIpPermissionsEgress = mempty
    , _sgVpcId               = Nothing
    , _sgTags                = mempty
    }

-- | A description of the security group.
sgDescription :: Lens' SecurityGroup Text
sgDescription = lens _sgDescription (\s a -> s { _sgDescription = a })

-- | The ID of the security group.
sgGroupId :: Lens' SecurityGroup Text
sgGroupId = lens _sgGroupId (\s a -> s { _sgGroupId = a })

-- | The name of the security group.
sgGroupName :: Lens' SecurityGroup Text
sgGroupName = lens _sgGroupName (\s a -> s { _sgGroupName = a })

-- | One or more inbound rules associated with the security group.
sgIpPermissions :: Lens' SecurityGroup [IpPermission]
sgIpPermissions = lens _sgIpPermissions (\s a -> s { _sgIpPermissions = a })

-- | [EC2-VPC] One or more outbound rules associated with the security group.
sgIpPermissionsEgress :: Lens' SecurityGroup [IpPermission]
sgIpPermissionsEgress =
    lens _sgIpPermissionsEgress (\s a -> s { _sgIpPermissionsEgress = a })

-- | The AWS account ID of the owner of the security group.
sgOwnerId :: Lens' SecurityGroup Text
sgOwnerId = lens _sgOwnerId (\s a -> s { _sgOwnerId = a })

-- | Any tags assigned to the security group.
sgTags :: Lens' SecurityGroup [Tag]
sgTags = lens _sgTags (\s a -> s { _sgTags = a })

-- | [EC2-VPC] The ID of the VPC for the security group.
sgVpcId :: Lens' SecurityGroup (Maybe Text)
sgVpcId = lens _sgVpcId (\s a -> s { _sgVpcId = a })

instance FromXML SecurityGroup where
    parseXML c = SecurityGroup
        <$> c .: "groupDescription"
        <*> c .: "groupId"
        <*> c .: "groupName"
        <*> c .: "ipPermissions"
        <*> c .: "ipPermissionsEgress"
        <*> c .: "ownerId"
        <*> c .: "tagSet"
        <*> c .:? "vpcId"

instance ToQuery SecurityGroup

data CancelSpotInstanceRequestState
    = CSIRSActive    -- ^ active
    | CSIRSCancelled -- ^ cancelled
    | CSIRSClosed    -- ^ closed
    | CSIRSCompleted -- ^ completed
    | CSIRSOpen      -- ^ open
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable CancelSpotInstanceRequestState

instance FromText CancelSpotInstanceRequestState where
    parser = match "active"    CSIRSActive
         <|> match "cancelled" CSIRSCancelled
         <|> match "closed"    CSIRSClosed
         <|> match "completed" CSIRSCompleted
         <|> match "open"      CSIRSOpen

instance ToText CancelSpotInstanceRequestState where
    toText = \case
        CSIRSActive    -> "active"
        CSIRSCancelled -> "cancelled"
        CSIRSClosed    -> "closed"
        CSIRSCompleted -> "completed"
        CSIRSOpen      -> "open"

instance FromXML CancelSpotInstanceRequestState where
    parseXML = fromXMLText "CancelSpotInstanceRequestState"

instance ToQuery CancelSpotInstanceRequestState

data PlacementGroupState
    = PGSAvailable -- ^ available
    | PGSDeleted   -- ^ deleted
    | PGSDeleting  -- ^ deleting
    | PGSPending   -- ^ pending
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable PlacementGroupState

instance FromText PlacementGroupState where
    parser = match "available" PGSAvailable
         <|> match "deleted"   PGSDeleted
         <|> match "deleting"  PGSDeleting
         <|> match "pending"   PGSPending

instance ToText PlacementGroupState where
    toText = \case
        PGSAvailable -> "available"
        PGSDeleted   -> "deleted"
        PGSDeleting  -> "deleting"
        PGSPending   -> "pending"

instance FromXML PlacementGroupState where
    parseXML = fromXMLText "PlacementGroupState"

instance ToQuery PlacementGroupState

data ReservedInstancesModificationResult = ReservedInstancesModificationResult
    { _rimrReservedInstancesId :: Maybe Text
    , _rimrTargetConfiguration :: Maybe ReservedInstancesConfiguration
    } deriving (Eq, Show, Generic)

-- | 'ReservedInstancesModificationResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rimrReservedInstancesId' @::@ 'Maybe' 'Text'
--
-- * 'rimrTargetConfiguration' @::@ 'Maybe' 'ReservedInstancesConfiguration'
--
reservedInstancesModificationResult :: ReservedInstancesModificationResult
reservedInstancesModificationResult = ReservedInstancesModificationResult
    { _rimrReservedInstancesId = Nothing
    , _rimrTargetConfiguration = Nothing
    }

-- | The ID for the Reserved Instances that were created as part of the
-- modification request. This field is only available when the modification
-- is fulfilled.
rimrReservedInstancesId :: Lens' ReservedInstancesModificationResult (Maybe Text)
rimrReservedInstancesId =
    lens _rimrReservedInstancesId (\s a -> s { _rimrReservedInstancesId = a })

-- | The target Reserved Instances configurations supplied as part of the
-- modification request.
rimrTargetConfiguration :: Lens' ReservedInstancesModificationResult (Maybe ReservedInstancesConfiguration)
rimrTargetConfiguration =
    lens _rimrTargetConfiguration (\s a -> s { _rimrTargetConfiguration = a })

instance FromXML ReservedInstancesModificationResult where
    parseXML c = ReservedInstancesModificationResult
        <$> c .:? "reservedInstancesId"
        <*> c .:? "targetConfiguration"

instance ToQuery ReservedInstancesModificationResult

data InstanceBlockDeviceMappingSpecification = InstanceBlockDeviceMappingSpecification
    { _ibdmsDeviceName  :: Maybe Text
    , _ibdmsEbs         :: Maybe EbsInstanceBlockDeviceSpecification
    , _ibdmsNoDevice    :: Maybe Text
    , _ibdmsVirtualName :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'InstanceBlockDeviceMappingSpecification' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ibdmsDeviceName' @::@ 'Maybe' 'Text'
--
-- * 'ibdmsEbs' @::@ 'Maybe' 'EbsInstanceBlockDeviceSpecification'
--
-- * 'ibdmsNoDevice' @::@ 'Maybe' 'Text'
--
-- * 'ibdmsVirtualName' @::@ 'Maybe' 'Text'
--
instanceBlockDeviceMappingSpecification :: InstanceBlockDeviceMappingSpecification
instanceBlockDeviceMappingSpecification = InstanceBlockDeviceMappingSpecification
    { _ibdmsDeviceName  = Nothing
    , _ibdmsEbs         = Nothing
    , _ibdmsVirtualName = Nothing
    , _ibdmsNoDevice    = Nothing
    }

-- | The device name exposed to the instance (for example, /dev/sdh).
ibdmsDeviceName :: Lens' InstanceBlockDeviceMappingSpecification (Maybe Text)
ibdmsDeviceName = lens _ibdmsDeviceName (\s a -> s { _ibdmsDeviceName = a })

-- | Parameters used to automatically set up Amazon EBS volumes when the
-- instance is launched.
ibdmsEbs :: Lens' InstanceBlockDeviceMappingSpecification (Maybe EbsInstanceBlockDeviceSpecification)
ibdmsEbs = lens _ibdmsEbs (\s a -> s { _ibdmsEbs = a })

-- | suppress the specified device included in the block device mapping.
ibdmsNoDevice :: Lens' InstanceBlockDeviceMappingSpecification (Maybe Text)
ibdmsNoDevice = lens _ibdmsNoDevice (\s a -> s { _ibdmsNoDevice = a })

-- | The virtual device name.
ibdmsVirtualName :: Lens' InstanceBlockDeviceMappingSpecification (Maybe Text)
ibdmsVirtualName = lens _ibdmsVirtualName (\s a -> s { _ibdmsVirtualName = a })

instance FromXML InstanceBlockDeviceMappingSpecification where
    parseXML c = InstanceBlockDeviceMappingSpecification
        <$> c .:? "deviceName"
        <*> c .:? "ebs"
        <*> c .:? "noDevice"
        <*> c .:? "virtualName"

instance ToQuery InstanceBlockDeviceMappingSpecification

data ExportEnvironment
    = Citrix    -- ^ citrix
    | Microsoft -- ^ microsoft
    | Vmware    -- ^ vmware
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable ExportEnvironment

instance FromText ExportEnvironment where
    parser = match "citrix"    Citrix
         <|> match "microsoft" Microsoft
         <|> match "vmware"    Vmware

instance ToText ExportEnvironment where
    toText = \case
        Citrix    -> "citrix"
        Microsoft -> "microsoft"
        Vmware    -> "vmware"

instance FromXML ExportEnvironment where
    parseXML = fromXMLText "ExportEnvironment"

instance ToQuery ExportEnvironment

data VolumeAttachment = VolumeAttachment
    { _vaAttachTime          :: Maybe RFC822
    , _vaDeleteOnTermination :: Maybe Bool
    , _vaDevice              :: Maybe Text
    , _vaInstanceId          :: Maybe Text
    , _vaState               :: Maybe Text
    , _vaVolumeId            :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'VolumeAttachment' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vaAttachTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'vaDeleteOnTermination' @::@ 'Maybe' 'Bool'
--
-- * 'vaDevice' @::@ 'Maybe' 'Text'
--
-- * 'vaInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'vaState' @::@ 'Maybe' 'Text'
--
-- * 'vaVolumeId' @::@ 'Maybe' 'Text'
--
volumeAttachment :: VolumeAttachment
volumeAttachment = VolumeAttachment
    { _vaVolumeId            = Nothing
    , _vaInstanceId          = Nothing
    , _vaDevice              = Nothing
    , _vaState               = Nothing
    , _vaAttachTime          = Nothing
    , _vaDeleteOnTermination = Nothing
    }

-- | The time stamp when the attachment initiated.
vaAttachTime :: Lens' VolumeAttachment (Maybe UTCTime)
vaAttachTime = lens _vaAttachTime (\s a -> s { _vaAttachTime = a })
    . mapping _Time

-- | Indicates whether the Amazon EBS volume is deleted on instance
-- termination.
vaDeleteOnTermination :: Lens' VolumeAttachment (Maybe Bool)
vaDeleteOnTermination =
    lens _vaDeleteOnTermination (\s a -> s { _vaDeleteOnTermination = a })

-- | The device name.
vaDevice :: Lens' VolumeAttachment (Maybe Text)
vaDevice = lens _vaDevice (\s a -> s { _vaDevice = a })

-- | The ID of the instance.
vaInstanceId :: Lens' VolumeAttachment (Maybe Text)
vaInstanceId = lens _vaInstanceId (\s a -> s { _vaInstanceId = a })

-- | The attachment state of the volume.
vaState :: Lens' VolumeAttachment (Maybe Text)
vaState = lens _vaState (\s a -> s { _vaState = a })

-- | The ID of the volume.
vaVolumeId :: Lens' VolumeAttachment (Maybe Text)
vaVolumeId = lens _vaVolumeId (\s a -> s { _vaVolumeId = a })

instance FromXML VolumeAttachment where
    parseXML c = VolumeAttachment
        <$> c .:? "attachTime"
        <*> c .:? "deleteOnTermination"
        <*> c .:? "device"
        <*> c .:? "instanceId"
        <*> c .:? "status"
        <*> c .:? "volumeId"

instance ToQuery VolumeAttachment

data CustomerGateway = CustomerGateway
    { _cgBgpAsn            :: Maybe Text
    , _cgCustomerGatewayId :: Maybe Text
    , _cgIpAddress         :: Maybe Text
    , _cgState             :: Maybe Text
    , _cgTags              :: [Tag]
    , _cgType              :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'CustomerGateway' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cgBgpAsn' @::@ 'Maybe' 'Text'
--
-- * 'cgCustomerGatewayId' @::@ 'Maybe' 'Text'
--
-- * 'cgIpAddress' @::@ 'Maybe' 'Text'
--
-- * 'cgState' @::@ 'Maybe' 'Text'
--
-- * 'cgTags' @::@ ['Tag']
--
-- * 'cgType' @::@ 'Maybe' 'Text'
--
customerGateway :: CustomerGateway
customerGateway = CustomerGateway
    { _cgCustomerGatewayId = Nothing
    , _cgState             = Nothing
    , _cgType              = Nothing
    , _cgIpAddress         = Nothing
    , _cgBgpAsn            = Nothing
    , _cgTags              = mempty
    }

-- | The customer gateway's Border Gateway Protocol (BGP) Autonomous System
-- Number (ASN).
cgBgpAsn :: Lens' CustomerGateway (Maybe Text)
cgBgpAsn = lens _cgBgpAsn (\s a -> s { _cgBgpAsn = a })

-- | The ID of the customer gateway.
cgCustomerGatewayId :: Lens' CustomerGateway (Maybe Text)
cgCustomerGatewayId =
    lens _cgCustomerGatewayId (\s a -> s { _cgCustomerGatewayId = a })

-- | The Internet-routable IP address of the customer gateway's outside
-- interface.
cgIpAddress :: Lens' CustomerGateway (Maybe Text)
cgIpAddress = lens _cgIpAddress (\s a -> s { _cgIpAddress = a })

-- | The current state of the customer gateway (pending | available | deleting
-- | deleted).
cgState :: Lens' CustomerGateway (Maybe Text)
cgState = lens _cgState (\s a -> s { _cgState = a })

-- | Any tags assigned to the customer gateway.
cgTags :: Lens' CustomerGateway [Tag]
cgTags = lens _cgTags (\s a -> s { _cgTags = a })

-- | The type of VPN connection the customer gateway supports (ipsec.1).
cgType :: Lens' CustomerGateway (Maybe Text)
cgType = lens _cgType (\s a -> s { _cgType = a })

instance FromXML CustomerGateway where
    parseXML c = CustomerGateway
        <$> c .:? "bgpAsn"
        <*> c .:? "customerGatewayId"
        <*> c .:? "ipAddress"
        <*> c .:? "state"
        <*> c .: "tagSet"
        <*> c .:? "type"

instance ToQuery CustomerGateway

data EbsInstanceBlockDevice = EbsInstanceBlockDevice
    { _eibdAttachTime          :: Maybe RFC822
    , _eibdDeleteOnTermination :: Maybe Bool
    , _eibdStatus              :: Maybe Text
    , _eibdVolumeId            :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'EbsInstanceBlockDevice' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eibdAttachTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'eibdDeleteOnTermination' @::@ 'Maybe' 'Bool'
--
-- * 'eibdStatus' @::@ 'Maybe' 'Text'
--
-- * 'eibdVolumeId' @::@ 'Maybe' 'Text'
--
ebsInstanceBlockDevice :: EbsInstanceBlockDevice
ebsInstanceBlockDevice = EbsInstanceBlockDevice
    { _eibdVolumeId            = Nothing
    , _eibdStatus              = Nothing
    , _eibdAttachTime          = Nothing
    , _eibdDeleteOnTermination = Nothing
    }

-- | The time stamp when the attachment initiated.
eibdAttachTime :: Lens' EbsInstanceBlockDevice (Maybe UTCTime)
eibdAttachTime = lens _eibdAttachTime (\s a -> s { _eibdAttachTime = a })
    . mapping _Time

-- | Indicates whether the volume is deleted on instance termination.
eibdDeleteOnTermination :: Lens' EbsInstanceBlockDevice (Maybe Bool)
eibdDeleteOnTermination =
    lens _eibdDeleteOnTermination (\s a -> s { _eibdDeleteOnTermination = a })

-- | The attachment state.
eibdStatus :: Lens' EbsInstanceBlockDevice (Maybe Text)
eibdStatus = lens _eibdStatus (\s a -> s { _eibdStatus = a })

-- | The ID of the Amazon EBS volume.
eibdVolumeId :: Lens' EbsInstanceBlockDevice (Maybe Text)
eibdVolumeId = lens _eibdVolumeId (\s a -> s { _eibdVolumeId = a })

instance FromXML EbsInstanceBlockDevice where
    parseXML c = EbsInstanceBlockDevice
        <$> c .:? "attachTime"
        <*> c .:? "deleteOnTermination"
        <*> c .:? "status"
        <*> c .:? "volumeId"

instance ToQuery EbsInstanceBlockDevice

data ShutdownBehavior
    = Stop      -- ^ stop
    | Terminate -- ^ terminate
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable ShutdownBehavior

instance FromText ShutdownBehavior where
    parser = match "stop"      Stop
         <|> match "terminate" Terminate

instance ToText ShutdownBehavior where
    toText = \case
        Stop      -> "stop"
        Terminate -> "terminate"

instance FromXML ShutdownBehavior where
    parseXML = fromXMLText "ShutdownBehavior"

instance ToQuery ShutdownBehavior

data DiskImageDescription = DiskImageDescription
    { _did1Checksum          :: Maybe Text
    , _did1Format            :: Text
    , _did1ImportManifestUrl :: Text
    , _did1Size              :: Integer
    } deriving (Eq, Ord, Show, Generic)

-- | 'DiskImageDescription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'did1Checksum' @::@ 'Maybe' 'Text'
--
-- * 'did1Format' @::@ 'Text'
--
-- * 'did1ImportManifestUrl' @::@ 'Text'
--
-- * 'did1Size' @::@ 'Integer'
--
diskImageDescription :: Text -- ^ 'did1Format'
                     -> Integer -- ^ 'did1Size'
                     -> Text -- ^ 'did1ImportManifestUrl'
                     -> DiskImageDescription
diskImageDescription p1 p2 p3 = DiskImageDescription
    { _did1Format            = p1
    , _did1Size              = p2
    , _did1ImportManifestUrl = p3
    , _did1Checksum          = Nothing
    }

-- | The checksum computed for the disk image.
did1Checksum :: Lens' DiskImageDescription (Maybe Text)
did1Checksum = lens _did1Checksum (\s a -> s { _did1Checksum = a })

-- | The disk image format.
did1Format :: Lens' DiskImageDescription Text
did1Format = lens _did1Format (\s a -> s { _did1Format = a })

-- | A presigned URL for the import manifest stored in Amazon S3. For
-- information about creating a presigned URL for an Amazon S3 object, read
-- the "Query String Request Authentication Alternative" section of the
-- Authenticating REST Requests topic in the Amazon Simple Storage Service
-- Developer Guide.
did1ImportManifestUrl :: Lens' DiskImageDescription Text
did1ImportManifestUrl =
    lens _did1ImportManifestUrl (\s a -> s { _did1ImportManifestUrl = a })

-- | The size of the disk image.
did1Size :: Lens' DiskImageDescription Integer
did1Size = lens _did1Size (\s a -> s { _did1Size = a })

instance FromXML DiskImageDescription where
    parseXML c = DiskImageDescription
        <$> c .:? "checksum"
        <*> c .: "format"
        <*> c .: "importManifestUrl"
        <*> c .: "size"

instance ToQuery DiskImageDescription

data DiskImageVolumeDescription = DiskImageVolumeDescription
    { _divdId   :: Text
    , _divdSize :: Maybe Integer
    } deriving (Eq, Ord, Show, Generic)

-- | 'DiskImageVolumeDescription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'divdId' @::@ 'Text'
--
-- * 'divdSize' @::@ 'Maybe' 'Integer'
--
diskImageVolumeDescription :: Text -- ^ 'divdId'
                           -> DiskImageVolumeDescription
diskImageVolumeDescription p1 = DiskImageVolumeDescription
    { _divdId   = p1
    , _divdSize = Nothing
    }

-- | The volume identifier.
divdId :: Lens' DiskImageVolumeDescription Text
divdId = lens _divdId (\s a -> s { _divdId = a })

-- | The size of the volume.
divdSize :: Lens' DiskImageVolumeDescription (Maybe Integer)
divdSize = lens _divdSize (\s a -> s { _divdSize = a })

instance FromXML DiskImageVolumeDescription where
    parseXML c = DiskImageVolumeDescription
        <$> c .: "id"
        <*> c .:? "size"

instance ToQuery DiskImageVolumeDescription

newtype Monitoring = Monitoring
    { _mState :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'Monitoring' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mState' @::@ 'Maybe' 'Text'
--
monitoring :: Monitoring
monitoring = Monitoring
    { _mState = Nothing
    }

-- | Indicates whether monitoring is enabled for the instance.
mState :: Lens' Monitoring (Maybe Text)
mState = lens _mState (\s a -> s { _mState = a })

instance FromXML Monitoring where
    parseXML c = Monitoring
        <$> c .:? "state"

instance ToQuery Monitoring

data SubnetState
    = SSAvailable -- ^ available
    | SSPending   -- ^ pending
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable SubnetState

instance FromText SubnetState where
    parser = match "available" SSAvailable
         <|> match "pending"   SSPending

instance ToText SubnetState where
    toText = \case
        SSAvailable -> "available"
        SSPending   -> "pending"

instance FromXML SubnetState where
    parseXML = fromXMLText "SubnetState"

instance ToQuery SubnetState

data ContainerFormat
    = Ova -- ^ ova
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable ContainerFormat

instance FromText ContainerFormat where
    parser = match "ova" Ova

instance ToText ContainerFormat where
    toText Ova = "ova"

instance FromXML ContainerFormat where
    parseXML = fromXMLText "ContainerFormat"

instance ToQuery ContainerFormat

newtype AvailabilityZoneMessage = AvailabilityZoneMessage
    { _azmMessage :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'AvailabilityZoneMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'azmMessage' @::@ 'Maybe' 'Text'
--
availabilityZoneMessage :: AvailabilityZoneMessage
availabilityZoneMessage = AvailabilityZoneMessage
    { _azmMessage = Nothing
    }

-- | The message about the Availability Zone.
azmMessage :: Lens' AvailabilityZoneMessage (Maybe Text)
azmMessage = lens _azmMessage (\s a -> s { _azmMessage = a })

instance FromXML AvailabilityZoneMessage where
    parseXML c = AvailabilityZoneMessage
        <$> c .:? "message"

instance ToQuery AvailabilityZoneMessage

data VpcAttachment = VpcAttachment
    { _va1State :: Maybe Text
    , _va1VpcId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'VpcAttachment' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'va1State' @::@ 'Maybe' 'Text'
--
-- * 'va1VpcId' @::@ 'Maybe' 'Text'
--
vpcAttachment :: VpcAttachment
vpcAttachment = VpcAttachment
    { _va1VpcId = Nothing
    , _va1State = Nothing
    }

-- | The current state of the attachment.
va1State :: Lens' VpcAttachment (Maybe Text)
va1State = lens _va1State (\s a -> s { _va1State = a })

-- | The ID of the VPC.
va1VpcId :: Lens' VpcAttachment (Maybe Text)
va1VpcId = lens _va1VpcId (\s a -> s { _va1VpcId = a })

instance FromXML VpcAttachment where
    parseXML c = VpcAttachment
        <$> c .:? "state"
        <*> c .:? "vpcId"

instance ToQuery VpcAttachment

data InstanceBlockDeviceMapping = InstanceBlockDeviceMapping
    { _ibdmDeviceName :: Maybe Text
    , _ibdmEbs        :: Maybe EbsInstanceBlockDevice
    } deriving (Eq, Show, Generic)

-- | 'InstanceBlockDeviceMapping' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ibdmDeviceName' @::@ 'Maybe' 'Text'
--
-- * 'ibdmEbs' @::@ 'Maybe' 'EbsInstanceBlockDevice'
--
instanceBlockDeviceMapping :: InstanceBlockDeviceMapping
instanceBlockDeviceMapping = InstanceBlockDeviceMapping
    { _ibdmDeviceName = Nothing
    , _ibdmEbs        = Nothing
    }

-- | The device name exposed to the instance (for example, /dev/sdh).
ibdmDeviceName :: Lens' InstanceBlockDeviceMapping (Maybe Text)
ibdmDeviceName = lens _ibdmDeviceName (\s a -> s { _ibdmDeviceName = a })

-- | Parameters used to automatically set up Amazon EBS volumes when the
-- instance is launched.
ibdmEbs :: Lens' InstanceBlockDeviceMapping (Maybe EbsInstanceBlockDevice)
ibdmEbs = lens _ibdmEbs (\s a -> s { _ibdmEbs = a })

instance FromXML InstanceBlockDeviceMapping where
    parseXML c = InstanceBlockDeviceMapping
        <$> c .:? "deviceName"
        <*> c .:? "ebs"

instance ToQuery InstanceBlockDeviceMapping

data StatusType
    = Failed           -- ^ failed
    | InsufficientData -- ^ insufficient-data
    | Passed           -- ^ passed
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable StatusType

instance FromText StatusType where
    parser = match "failed"            Failed
         <|> match "insufficient-data" InsufficientData
         <|> match "passed"            Passed

instance ToText StatusType where
    toText = \case
        Failed           -> "failed"
        InsufficientData -> "insufficient-data"
        Passed           -> "passed"

instance FromXML StatusType where
    parseXML = fromXMLText "StatusType"

instance ToQuery StatusType

data ExportToS3TaskSpecification = ExportToS3TaskSpecification
    { _etstsContainerFormat :: Maybe Text
    , _etstsDiskImageFormat :: Maybe Text
    , _etstsS3Bucket        :: Maybe Text
    , _etstsS3Prefix        :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ExportToS3TaskSpecification' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'etstsContainerFormat' @::@ 'Maybe' 'Text'
--
-- * 'etstsDiskImageFormat' @::@ 'Maybe' 'Text'
--
-- * 'etstsS3Bucket' @::@ 'Maybe' 'Text'
--
-- * 'etstsS3Prefix' @::@ 'Maybe' 'Text'
--
exportToS3TaskSpecification :: ExportToS3TaskSpecification
exportToS3TaskSpecification = ExportToS3TaskSpecification
    { _etstsDiskImageFormat = Nothing
    , _etstsContainerFormat = Nothing
    , _etstsS3Bucket        = Nothing
    , _etstsS3Prefix        = Nothing
    }

etstsContainerFormat :: Lens' ExportToS3TaskSpecification (Maybe Text)
etstsContainerFormat =
    lens _etstsContainerFormat (\s a -> s { _etstsContainerFormat = a })

etstsDiskImageFormat :: Lens' ExportToS3TaskSpecification (Maybe Text)
etstsDiskImageFormat =
    lens _etstsDiskImageFormat (\s a -> s { _etstsDiskImageFormat = a })

etstsS3Bucket :: Lens' ExportToS3TaskSpecification (Maybe Text)
etstsS3Bucket = lens _etstsS3Bucket (\s a -> s { _etstsS3Bucket = a })

-- | The image is written to a single object in the Amazon S3 bucket at the S3
-- key s3prefix + exportTaskId + '.' + diskImageFormat.
etstsS3Prefix :: Lens' ExportToS3TaskSpecification (Maybe Text)
etstsS3Prefix = lens _etstsS3Prefix (\s a -> s { _etstsS3Prefix = a })

instance FromXML ExportToS3TaskSpecification where
    parseXML c = ExportToS3TaskSpecification
        <$> c .:? "containerFormat"
        <*> c .:? "diskImageFormat"
        <*> c .:? "s3Bucket"
        <*> c .:? "s3Prefix"

instance ToQuery ExportToS3TaskSpecification

data NetworkInterfaceAttribute
    = Attachment      -- ^ attachment
    | Description     -- ^ description
    | GroupSet        -- ^ groupSet
    | SourceDestCheck -- ^ sourceDestCheck
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable NetworkInterfaceAttribute

instance FromText NetworkInterfaceAttribute where
    parser = match "attachment"      Attachment
         <|> match "description"     Description
         <|> match "groupSet"        GroupSet
         <|> match "sourceDestCheck" SourceDestCheck

instance ToText NetworkInterfaceAttribute where
    toText = \case
        Attachment      -> "attachment"
        Description     -> "description"
        GroupSet        -> "groupSet"
        SourceDestCheck -> "sourceDestCheck"

instance FromXML NetworkInterfaceAttribute where
    parseXML = fromXMLText "NetworkInterfaceAttribute"

instance ToQuery NetworkInterfaceAttribute

data ImageTypeValues
    = Kernel  -- ^ kernel
    | Machine -- ^ machine
    | Ramdisk -- ^ ramdisk
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable ImageTypeValues

instance FromText ImageTypeValues where
    parser = match "kernel"  Kernel
         <|> match "machine" Machine
         <|> match "ramdisk" Ramdisk

instance ToText ImageTypeValues where
    toText = \case
        Kernel  -> "kernel"
        Machine -> "machine"
        Ramdisk -> "ramdisk"

instance FromXML ImageTypeValues where
    parseXML = fromXMLText "ImageTypeValues"

instance ToQuery ImageTypeValues

data InstanceExportDetails = InstanceExportDetails
    { _iedInstanceId        :: Maybe Text
    , _iedTargetEnvironment :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'InstanceExportDetails' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iedInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'iedTargetEnvironment' @::@ 'Maybe' 'Text'
--
instanceExportDetails :: InstanceExportDetails
instanceExportDetails = InstanceExportDetails
    { _iedInstanceId        = Nothing
    , _iedTargetEnvironment = Nothing
    }

-- | The ID of the resource being exported.
iedInstanceId :: Lens' InstanceExportDetails (Maybe Text)
iedInstanceId = lens _iedInstanceId (\s a -> s { _iedInstanceId = a })

-- | The target virtualization environment.
iedTargetEnvironment :: Lens' InstanceExportDetails (Maybe Text)
iedTargetEnvironment =
    lens _iedTargetEnvironment (\s a -> s { _iedTargetEnvironment = a })

instance FromXML InstanceExportDetails where
    parseXML c = InstanceExportDetails
        <$> c .:? "instanceId"
        <*> c .:? "targetEnvironment"

instance ToQuery InstanceExportDetails

data SnapshotAttributeName
    = SANCreateVolumePermission -- ^ createVolumePermission
    | SANProductCodes           -- ^ productCodes
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable SnapshotAttributeName

instance FromText SnapshotAttributeName where
    parser = match "createVolumePermission" SANCreateVolumePermission
         <|> match "productCodes"           SANProductCodes

instance ToText SnapshotAttributeName where
    toText = \case
        SANCreateVolumePermission -> "createVolumePermission"
        SANProductCodes           -> "productCodes"

instance FromXML SnapshotAttributeName where
    parseXML = fromXMLText "SnapshotAttributeName"

instance ToQuery SnapshotAttributeName

data AvailabilityZone = AvailabilityZone
    { _azMessages   :: [AvailabilityZoneMessage]
    , _azRegionName :: Maybe Text
    , _azState      :: Maybe Text
    , _azZoneName   :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'AvailabilityZone' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'azMessages' @::@ ['AvailabilityZoneMessage']
--
-- * 'azRegionName' @::@ 'Maybe' 'Text'
--
-- * 'azState' @::@ 'Maybe' 'Text'
--
-- * 'azZoneName' @::@ 'Maybe' 'Text'
--
availabilityZone :: AvailabilityZone
availabilityZone = AvailabilityZone
    { _azZoneName   = Nothing
    , _azState      = Nothing
    , _azRegionName = Nothing
    , _azMessages   = mempty
    }

-- | Any messages about the Availability Zone.
azMessages :: Lens' AvailabilityZone [AvailabilityZoneMessage]
azMessages = lens _azMessages (\s a -> s { _azMessages = a })

-- | The name of the region.
azRegionName :: Lens' AvailabilityZone (Maybe Text)
azRegionName = lens _azRegionName (\s a -> s { _azRegionName = a })

-- | The state of the Availability Zone (available | impaired | unavailable).
azState :: Lens' AvailabilityZone (Maybe Text)
azState = lens _azState (\s a -> s { _azState = a })

-- | The name of the Availability Zone.
azZoneName :: Lens' AvailabilityZone (Maybe Text)
azZoneName = lens _azZoneName (\s a -> s { _azZoneName = a })

instance FromXML AvailabilityZone where
    parseXML c = AvailabilityZone
        <$> c .: "messageSet"
        <*> c .:? "regionName"
        <*> c .:? "zoneState"
        <*> c .:? "zoneName"

instance ToQuery AvailabilityZone

data VpnState
    = VpnStateAvailable -- ^ available
    | VpnStateDeleted   -- ^ deleted
    | VpnStateDeleting  -- ^ deleting
    | VpnStatePending   -- ^ pending
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable VpnState

instance FromText VpnState where
    parser = match "available" VpnStateAvailable
         <|> match "deleted"   VpnStateDeleted
         <|> match "deleting"  VpnStateDeleting
         <|> match "pending"   VpnStatePending

instance ToText VpnState where
    toText = \case
        VpnStateAvailable -> "available"
        VpnStateDeleted   -> "deleted"
        VpnStateDeleting  -> "deleting"
        VpnStatePending   -> "pending"

instance FromXML VpnState where
    parseXML = fromXMLText "VpnState"

instance ToQuery VpnState

data RouteTable = RouteTable
    { _rtAssociations    :: [RouteTableAssociation]
    , _rtPropagatingVgws :: [PropagatingVgw]
    , _rtRouteTableId    :: Maybe Text
    , _rtRoutes          :: [Route]
    , _rtTags            :: [Tag]
    , _rtVpcId           :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'RouteTable' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtAssociations' @::@ ['RouteTableAssociation']
--
-- * 'rtPropagatingVgws' @::@ ['PropagatingVgw']
--
-- * 'rtRouteTableId' @::@ 'Maybe' 'Text'
--
-- * 'rtRoutes' @::@ ['Route']
--
-- * 'rtTags' @::@ ['Tag']
--
-- * 'rtVpcId' @::@ 'Maybe' 'Text'
--
routeTable :: RouteTable
routeTable = RouteTable
    { _rtRouteTableId    = Nothing
    , _rtVpcId           = Nothing
    , _rtRoutes          = mempty
    , _rtAssociations    = mempty
    , _rtTags            = mempty
    , _rtPropagatingVgws = mempty
    }

-- | The associations between the route table and one or more subnets.
rtAssociations :: Lens' RouteTable [RouteTableAssociation]
rtAssociations = lens _rtAssociations (\s a -> s { _rtAssociations = a })

-- | Any virtual private gateway (VGW) propagating routes.
rtPropagatingVgws :: Lens' RouteTable [PropagatingVgw]
rtPropagatingVgws =
    lens _rtPropagatingVgws (\s a -> s { _rtPropagatingVgws = a })

-- | The ID of the route table.
rtRouteTableId :: Lens' RouteTable (Maybe Text)
rtRouteTableId = lens _rtRouteTableId (\s a -> s { _rtRouteTableId = a })

-- | The routes in the route table.
rtRoutes :: Lens' RouteTable [Route]
rtRoutes = lens _rtRoutes (\s a -> s { _rtRoutes = a })

-- | Any tags assigned to the route table.
rtTags :: Lens' RouteTable [Tag]
rtTags = lens _rtTags (\s a -> s { _rtTags = a })

-- | The ID of the VPC.
rtVpcId :: Lens' RouteTable (Maybe Text)
rtVpcId = lens _rtVpcId (\s a -> s { _rtVpcId = a })

instance FromXML RouteTable where
    parseXML c = RouteTable
        <$> c .: "associationSet"
        <*> c .: "propagatingVgwSet"
        <*> c .:? "routeTableId"
        <*> c .: "routeSet"
        <*> c .: "tagSet"
        <*> c .:? "vpcId"

instance ToQuery RouteTable

data HypervisorType
    = Ovm -- ^ ovm
    | Xen -- ^ xen
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable HypervisorType

instance FromText HypervisorType where
    parser = match "ovm" Ovm
         <|> match "xen" Xen

instance ToText HypervisorType where
    toText = \case
        Ovm -> "ovm"
        Xen -> "xen"

instance FromXML HypervisorType where
    parseXML = fromXMLText "HypervisorType"

instance ToQuery HypervisorType

data InstanceStatusDetails = InstanceStatusDetails
    { _isdImpairedSince :: Maybe RFC822
    , _isdName          :: Maybe Text
    , _isdStatus        :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'InstanceStatusDetails' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'isdImpairedSince' @::@ 'Maybe' 'UTCTime'
--
-- * 'isdName' @::@ 'Maybe' 'Text'
--
-- * 'isdStatus' @::@ 'Maybe' 'Text'
--
instanceStatusDetails :: InstanceStatusDetails
instanceStatusDetails = InstanceStatusDetails
    { _isdName          = Nothing
    , _isdStatus        = Nothing
    , _isdImpairedSince = Nothing
    }

-- | The time when a status check failed. For an instance that was launched
-- and impaired, this is the time when the instance was launched.
isdImpairedSince :: Lens' InstanceStatusDetails (Maybe UTCTime)
isdImpairedSince = lens _isdImpairedSince (\s a -> s { _isdImpairedSince = a })
    . mapping _Time

-- | The type of instance status.
isdName :: Lens' InstanceStatusDetails (Maybe Text)
isdName = lens _isdName (\s a -> s { _isdName = a })

-- | The status.
isdStatus :: Lens' InstanceStatusDetails (Maybe Text)
isdStatus = lens _isdStatus (\s a -> s { _isdStatus = a })

instance FromXML InstanceStatusDetails where
    parseXML c = InstanceStatusDetails
        <$> c .:? "impairedSince"
        <*> c .:? "name"
        <*> c .:? "status"

instance ToQuery InstanceStatusDetails

data IamInstanceProfile = IamInstanceProfile
    { _iipArn :: Maybe Text
    , _iipId  :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'IamInstanceProfile' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iipArn' @::@ 'Maybe' 'Text'
--
-- * 'iipId' @::@ 'Maybe' 'Text'
--
iamInstanceProfile :: IamInstanceProfile
iamInstanceProfile = IamInstanceProfile
    { _iipArn = Nothing
    , _iipId  = Nothing
    }

-- | The Amazon Resource Name (ARN) of the instance profile.
iipArn :: Lens' IamInstanceProfile (Maybe Text)
iipArn = lens _iipArn (\s a -> s { _iipArn = a })

-- | The ID of the instance profile.
iipId :: Lens' IamInstanceProfile (Maybe Text)
iipId = lens _iipId (\s a -> s { _iipId = a })

instance FromXML IamInstanceProfile where
    parseXML c = IamInstanceProfile
        <$> c .:? "arn"
        <*> c .:? "id"

instance ToQuery IamInstanceProfile

data InternetGatewayAttachment = InternetGatewayAttachment
    { _igaState :: Text
    , _igaVpcId :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'InternetGatewayAttachment' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'igaState' @::@ 'Text'
--
-- * 'igaVpcId' @::@ 'Text'
--
internetGatewayAttachment :: Text -- ^ 'igaVpcId'
                          -> Text -- ^ 'igaState'
                          -> InternetGatewayAttachment
internetGatewayAttachment p1 p2 = InternetGatewayAttachment
    { _igaVpcId = p1
    , _igaState = p2
    }

-- | The current state of the attachment.
igaState :: Lens' InternetGatewayAttachment Text
igaState = lens _igaState (\s a -> s { _igaState = a })

-- | The ID of the VPC.
igaVpcId :: Lens' InternetGatewayAttachment Text
igaVpcId = lens _igaVpcId (\s a -> s { _igaVpcId = a })

instance FromXML InternetGatewayAttachment where
    parseXML c = InternetGatewayAttachment
        <$> c .: "state"
        <*> c .: "vpcId"

instance ToQuery InternetGatewayAttachment

data ReservedInstanceState
    = RISActive         -- ^ active
    | RISPaymentFailed  -- ^ payment-failed
    | RISPaymentPending -- ^ payment-pending
    | RISRetired        -- ^ retired
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable ReservedInstanceState

instance FromText ReservedInstanceState where
    parser = match "active"          RISActive
         <|> match "payment-failed"  RISPaymentFailed
         <|> match "payment-pending" RISPaymentPending
         <|> match "retired"         RISRetired

instance ToText ReservedInstanceState where
    toText = \case
        RISActive         -> "active"
        RISPaymentFailed  -> "payment-failed"
        RISPaymentPending -> "payment-pending"
        RISRetired        -> "retired"

instance FromXML ReservedInstanceState where
    parseXML = fromXMLText "ReservedInstanceState"

instance ToQuery ReservedInstanceState

data InstanceAttributeName
    = IANInstanceBlockDeviceMapping                -- ^ blockDeviceMapping
    | IANInstanceDisableApiTermination             -- ^ disableApiTermination
    | IANInstanceEbsOptimized                      -- ^ ebsOptimized
    | IANInstanceGroupSet                          -- ^ groupSet
    | IANInstanceInstanceInitiatedShutdownBehavior -- ^ instanceInitiatedShutdownBehavior
    | IANInstanceInstanceType                      -- ^ instanceType
    | IANInstanceKernel                            -- ^ kernel
    | IANInstanceProductCodes                      -- ^ productCodes
    | IANInstanceRamdisk                           -- ^ ramdisk
    | IANInstanceRootDeviceName                    -- ^ rootDeviceName
    | IANInstanceSourceDestCheck                   -- ^ sourceDestCheck
    | IANInstanceSriovNetSupport                   -- ^ sriovNetSupport
    | IANInstanceUserData                          -- ^ userData
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable InstanceAttributeName

instance FromText InstanceAttributeName where
    parser = match "blockDeviceMapping"                IANInstanceBlockDeviceMapping
         <|> match "disableApiTermination"             IANInstanceDisableApiTermination
         <|> match "ebsOptimized"                      IANInstanceEbsOptimized
         <|> match "groupSet"                          IANInstanceGroupSet
         <|> match "instanceInitiatedShutdownBehavior" IANInstanceInstanceInitiatedShutdownBehavior
         <|> match "instanceType"                      IANInstanceInstanceType
         <|> match "kernel"                            IANInstanceKernel
         <|> match "productCodes"                      IANInstanceProductCodes
         <|> match "ramdisk"                           IANInstanceRamdisk
         <|> match "rootDeviceName"                    IANInstanceRootDeviceName
         <|> match "sourceDestCheck"                   IANInstanceSourceDestCheck
         <|> match "sriovNetSupport"                   IANInstanceSriovNetSupport
         <|> match "userData"                          IANInstanceUserData

instance ToText InstanceAttributeName where
    toText = \case
        IANInstanceBlockDeviceMapping                -> "blockDeviceMapping"
        IANInstanceDisableApiTermination             -> "disableApiTermination"
        IANInstanceEbsOptimized                      -> "ebsOptimized"
        IANInstanceGroupSet                          -> "groupSet"
        IANInstanceInstanceInitiatedShutdownBehavior -> "instanceInitiatedShutdownBehavior"
        IANInstanceInstanceType                      -> "instanceType"
        IANInstanceKernel                            -> "kernel"
        IANInstanceProductCodes                      -> "productCodes"
        IANInstanceRamdisk                           -> "ramdisk"
        IANInstanceRootDeviceName                    -> "rootDeviceName"
        IANInstanceSourceDestCheck                   -> "sourceDestCheck"
        IANInstanceSriovNetSupport                   -> "sriovNetSupport"
        IANInstanceUserData                          -> "userData"

instance FromXML InstanceAttributeName where
    parseXML = fromXMLText "InstanceAttributeName"

instance ToQuery InstanceAttributeName

data IpPermission = IpPermission
    { _ipFromPort         :: Int
    , _ipIpProtocol       :: Text
    , _ipIpRanges         :: [IpRange]
    , _ipToPort           :: Int
    , _ipUserIdGroupPairs :: [UserIdGroupPair]
    } deriving (Eq, Show, Generic)

-- | 'IpPermission' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ipFromPort' @::@ 'Int'
--
-- * 'ipIpProtocol' @::@ 'Text'
--
-- * 'ipIpRanges' @::@ ['IpRange']
--
-- * 'ipToPort' @::@ 'Int'
--
-- * 'ipUserIdGroupPairs' @::@ ['UserIdGroupPair']
--
ipPermission :: Text -- ^ 'ipIpProtocol'
             -> Int -- ^ 'ipFromPort'
             -> Int -- ^ 'ipToPort'
             -> IpPermission
ipPermission p1 p2 p3 = IpPermission
    { _ipIpProtocol       = p1
    , _ipFromPort         = p2
    , _ipToPort           = p3
    , _ipUserIdGroupPairs = mempty
    , _ipIpRanges         = mempty
    }

-- | The start of port range for the TCP and UDP protocols, or an ICMP type
-- number. A value of -1 indicates all ICMP types.
ipFromPort :: Lens' IpPermission Int
ipFromPort = lens _ipFromPort (\s a -> s { _ipFromPort = a })

-- | The protocol. When you call DescribeSecurityGroups, the protocol value
-- returned is the number. Exception: For TCP, UDP, and ICMP, the value
-- returned is the name (for example, tcp, udp, or icmp). For a list of
-- protocol numbers, see Protocol Numbers.
ipIpProtocol :: Lens' IpPermission Text
ipIpProtocol = lens _ipIpProtocol (\s a -> s { _ipIpProtocol = a })

-- | One or more IP ranges.
ipIpRanges :: Lens' IpPermission [IpRange]
ipIpRanges = lens _ipIpRanges (\s a -> s { _ipIpRanges = a })

-- | The end of port range for the TCP and UDP protocols, or an ICMP code. A
-- value of -1 indicates all ICMP codes for the specified ICMP type.
ipToPort :: Lens' IpPermission Int
ipToPort = lens _ipToPort (\s a -> s { _ipToPort = a })

-- | One or more security group and AWS account ID pairs.
ipUserIdGroupPairs :: Lens' IpPermission [UserIdGroupPair]
ipUserIdGroupPairs =
    lens _ipUserIdGroupPairs (\s a -> s { _ipUserIdGroupPairs = a })

instance FromXML IpPermission where
    parseXML c = IpPermission
        <$> c .: "fromPort"
        <*> c .: "ipProtocol"
        <*> c .: "ipRanges"
        <*> c .: "toPort"
        <*> c .: "groups"

instance ToQuery IpPermission

data ConversionTaskState
    = CTSActive     -- ^ active
    | CTSCancelled  -- ^ cancelled
    | CTSCancelling -- ^ cancelling
    | CTSCompleted  -- ^ completed
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable ConversionTaskState

instance FromText ConversionTaskState where
    parser = match "active"     CTSActive
         <|> match "cancelled"  CTSCancelled
         <|> match "cancelling" CTSCancelling
         <|> match "completed"  CTSCompleted

instance ToText ConversionTaskState where
    toText = \case
        CTSActive     -> "active"
        CTSCancelled  -> "cancelled"
        CTSCancelling -> "cancelling"
        CTSCompleted  -> "completed"

instance FromXML ConversionTaskState where
    parseXML = fromXMLText "ConversionTaskState"

instance ToQuery ConversionTaskState

data DiskImage = DiskImage
    { _diDescription :: Maybe Text
    , _diImage       :: Maybe DiskImageDetail
    , _diVolume      :: Maybe VolumeDetail
    } deriving (Eq, Show, Generic)

-- | 'DiskImage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diDescription' @::@ 'Maybe' 'Text'
--
-- * 'diImage' @::@ 'Maybe' 'DiskImageDetail'
--
-- * 'diVolume' @::@ 'Maybe' 'VolumeDetail'
--
diskImage :: DiskImage
diskImage = DiskImage
    { _diImage       = Nothing
    , _diDescription = Nothing
    , _diVolume      = Nothing
    }

diDescription :: Lens' DiskImage (Maybe Text)
diDescription = lens _diDescription (\s a -> s { _diDescription = a })

diImage :: Lens' DiskImage (Maybe DiskImageDetail)
diImage = lens _diImage (\s a -> s { _diImage = a })

diVolume :: Lens' DiskImage (Maybe VolumeDetail)
diVolume = lens _diVolume (\s a -> s { _diVolume = a })

instance FromXML DiskImage where
    parseXML c = DiskImage
        <$> c .:? "Description"
        <*> c .:? "Image"
        <*> c .:? "Volume"

instance ToQuery DiskImage

data Tenancy
    = Dedicated -- ^ dedicated
    | Default'  -- ^ default
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable Tenancy

instance FromText Tenancy where
    parser = match "dedicated" Dedicated
         <|> match "default"   Default'

instance ToText Tenancy where
    toText = \case
        Dedicated -> "dedicated"
        Default'  -> "default"

instance FromXML Tenancy where
    parseXML = fromXMLText "Tenancy"

instance ToQuery Tenancy

data VpcPeeringConnectionStateReason = VpcPeeringConnectionStateReason
    { _vpcsrCode    :: Maybe Text
    , _vpcsrMessage :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'VpcPeeringConnectionStateReason' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vpcsrCode' @::@ 'Maybe' 'Text'
--
-- * 'vpcsrMessage' @::@ 'Maybe' 'Text'
--
vpcPeeringConnectionStateReason :: VpcPeeringConnectionStateReason
vpcPeeringConnectionStateReason = VpcPeeringConnectionStateReason
    { _vpcsrCode    = Nothing
    , _vpcsrMessage = Nothing
    }

-- | The status of the VPC peering connection.
vpcsrCode :: Lens' VpcPeeringConnectionStateReason (Maybe Text)
vpcsrCode = lens _vpcsrCode (\s a -> s { _vpcsrCode = a })

-- | A message that provides more information about the status, if applicable.
vpcsrMessage :: Lens' VpcPeeringConnectionStateReason (Maybe Text)
vpcsrMessage = lens _vpcsrMessage (\s a -> s { _vpcsrMessage = a })

instance FromXML VpcPeeringConnectionStateReason where
    parseXML c = VpcPeeringConnectionStateReason
        <$> c .:? "code"
        <*> c .:? "message"

instance ToQuery VpcPeeringConnectionStateReason

data IamInstanceProfileSpecification = IamInstanceProfileSpecification
    { _iipsArn  :: Maybe Text
    , _iipsName :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'IamInstanceProfileSpecification' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iipsArn' @::@ 'Maybe' 'Text'
--
-- * 'iipsName' @::@ 'Maybe' 'Text'
--
iamInstanceProfileSpecification :: IamInstanceProfileSpecification
iamInstanceProfileSpecification = IamInstanceProfileSpecification
    { _iipsArn  = Nothing
    , _iipsName = Nothing
    }

-- | The Amazon Resource Name (ARN) of the instance profile.
iipsArn :: Lens' IamInstanceProfileSpecification (Maybe Text)
iipsArn = lens _iipsArn (\s a -> s { _iipsArn = a })

-- | The name of the instance profile.
iipsName :: Lens' IamInstanceProfileSpecification (Maybe Text)
iipsName = lens _iipsName (\s a -> s { _iipsName = a })

instance FromXML IamInstanceProfileSpecification where
    parseXML c = IamInstanceProfileSpecification
        <$> c .:? "arn"
        <*> c .:? "name"

instance ToQuery IamInstanceProfileSpecification

data ImportVolumeTaskDetails = ImportVolumeTaskDetails
    { _ivtdAvailabilityZone :: Text
    , _ivtdBytesConverted   :: Integer
    , _ivtdDescription      :: Maybe Text
    , _ivtdImage            :: DiskImageDescription
    , _ivtdVolume           :: DiskImageVolumeDescription
    } deriving (Eq, Show, Generic)

-- | 'ImportVolumeTaskDetails' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ivtdAvailabilityZone' @::@ 'Text'
--
-- * 'ivtdBytesConverted' @::@ 'Integer'
--
-- * 'ivtdDescription' @::@ 'Maybe' 'Text'
--
-- * 'ivtdImage' @::@ 'DiskImageDescription'
--
-- * 'ivtdVolume' @::@ 'DiskImageVolumeDescription'
--
importVolumeTaskDetails :: Integer -- ^ 'ivtdBytesConverted'
                        -> Text -- ^ 'ivtdAvailabilityZone'
                        -> DiskImageDescription -- ^ 'ivtdImage'
                        -> DiskImageVolumeDescription -- ^ 'ivtdVolume'
                        -> ImportVolumeTaskDetails
importVolumeTaskDetails p1 p2 p3 p4 = ImportVolumeTaskDetails
    { _ivtdBytesConverted   = p1
    , _ivtdAvailabilityZone = p2
    , _ivtdImage            = p3
    , _ivtdVolume           = p4
    , _ivtdDescription      = Nothing
    }

-- | The Availability Zone where the resulting volume will reside.
ivtdAvailabilityZone :: Lens' ImportVolumeTaskDetails Text
ivtdAvailabilityZone =
    lens _ivtdAvailabilityZone (\s a -> s { _ivtdAvailabilityZone = a })

-- | The number of bytes converted so far.
ivtdBytesConverted :: Lens' ImportVolumeTaskDetails Integer
ivtdBytesConverted =
    lens _ivtdBytesConverted (\s a -> s { _ivtdBytesConverted = a })

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
    parseXML c = ImportVolumeTaskDetails
        <$> c .: "availabilityZone"
        <*> c .: "bytesConverted"
        <*> c .:? "description"
        <*> c .: "image"
        <*> c .: "volume"

instance ToQuery ImportVolumeTaskDetails

data PlacementStrategy
    = Cluster -- ^ cluster
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable PlacementStrategy

instance FromText PlacementStrategy where
    parser = match "cluster" Cluster

instance ToText PlacementStrategy where
    toText Cluster = "cluster"

instance FromXML PlacementStrategy where
    parseXML = fromXMLText "PlacementStrategy"

instance ToQuery PlacementStrategy

data InstanceNetworkInterface = InstanceNetworkInterface
    { _iniAssociation        :: Maybe InstanceNetworkInterfaceAssociation
    , _iniAttachment         :: Maybe InstanceNetworkInterfaceAttachment
    , _iniDescription        :: Maybe Text
    , _iniGroups             :: [GroupIdentifier]
    , _iniMacAddress         :: Maybe Text
    , _iniNetworkInterfaceId :: Maybe Text
    , _iniOwnerId            :: Maybe Text
    , _iniPrivateDnsName     :: Maybe Text
    , _iniPrivateIpAddress   :: Maybe Text
    , _iniPrivateIpAddresses :: [InstancePrivateIpAddress]
    , _iniSourceDestCheck    :: Maybe Bool
    , _iniStatus             :: Maybe Text
    , _iniSubnetId           :: Maybe Text
    , _iniVpcId              :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'InstanceNetworkInterface' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iniAssociation' @::@ 'Maybe' 'InstanceNetworkInterfaceAssociation'
--
-- * 'iniAttachment' @::@ 'Maybe' 'InstanceNetworkInterfaceAttachment'
--
-- * 'iniDescription' @::@ 'Maybe' 'Text'
--
-- * 'iniGroups' @::@ ['GroupIdentifier']
--
-- * 'iniMacAddress' @::@ 'Maybe' 'Text'
--
-- * 'iniNetworkInterfaceId' @::@ 'Maybe' 'Text'
--
-- * 'iniOwnerId' @::@ 'Maybe' 'Text'
--
-- * 'iniPrivateDnsName' @::@ 'Maybe' 'Text'
--
-- * 'iniPrivateIpAddress' @::@ 'Maybe' 'Text'
--
-- * 'iniPrivateIpAddresses' @::@ ['InstancePrivateIpAddress']
--
-- * 'iniSourceDestCheck' @::@ 'Maybe' 'Bool'
--
-- * 'iniStatus' @::@ 'Maybe' 'Text'
--
-- * 'iniSubnetId' @::@ 'Maybe' 'Text'
--
-- * 'iniVpcId' @::@ 'Maybe' 'Text'
--
instanceNetworkInterface :: InstanceNetworkInterface
instanceNetworkInterface = InstanceNetworkInterface
    { _iniNetworkInterfaceId = Nothing
    , _iniSubnetId           = Nothing
    , _iniVpcId              = Nothing
    , _iniDescription        = Nothing
    , _iniOwnerId            = Nothing
    , _iniStatus             = Nothing
    , _iniMacAddress         = Nothing
    , _iniPrivateIpAddress   = Nothing
    , _iniPrivateDnsName     = Nothing
    , _iniSourceDestCheck    = Nothing
    , _iniGroups             = mempty
    , _iniAttachment         = Nothing
    , _iniAssociation        = Nothing
    , _iniPrivateIpAddresses = mempty
    }

-- | The association information for an Elastic IP associated with the network
-- interface.
iniAssociation :: Lens' InstanceNetworkInterface (Maybe InstanceNetworkInterfaceAssociation)
iniAssociation = lens _iniAssociation (\s a -> s { _iniAssociation = a })

-- | The network interface attachment.
iniAttachment :: Lens' InstanceNetworkInterface (Maybe InstanceNetworkInterfaceAttachment)
iniAttachment = lens _iniAttachment (\s a -> s { _iniAttachment = a })

-- | The description.
iniDescription :: Lens' InstanceNetworkInterface (Maybe Text)
iniDescription = lens _iniDescription (\s a -> s { _iniDescription = a })

-- | One or more security groups.
iniGroups :: Lens' InstanceNetworkInterface [GroupIdentifier]
iniGroups = lens _iniGroups (\s a -> s { _iniGroups = a })

-- | The MAC address.
iniMacAddress :: Lens' InstanceNetworkInterface (Maybe Text)
iniMacAddress = lens _iniMacAddress (\s a -> s { _iniMacAddress = a })

-- | The ID of the network interface.
iniNetworkInterfaceId :: Lens' InstanceNetworkInterface (Maybe Text)
iniNetworkInterfaceId =
    lens _iniNetworkInterfaceId (\s a -> s { _iniNetworkInterfaceId = a })

-- | The ID of the AWS account that created the network interface.
iniOwnerId :: Lens' InstanceNetworkInterface (Maybe Text)
iniOwnerId = lens _iniOwnerId (\s a -> s { _iniOwnerId = a })

-- | The private DNS name.
iniPrivateDnsName :: Lens' InstanceNetworkInterface (Maybe Text)
iniPrivateDnsName =
    lens _iniPrivateDnsName (\s a -> s { _iniPrivateDnsName = a })

-- | The IP address of the network interface within the subnet.
iniPrivateIpAddress :: Lens' InstanceNetworkInterface (Maybe Text)
iniPrivateIpAddress =
    lens _iniPrivateIpAddress (\s a -> s { _iniPrivateIpAddress = a })

-- | The private IP addresses associated with the network interface.
iniPrivateIpAddresses :: Lens' InstanceNetworkInterface [InstancePrivateIpAddress]
iniPrivateIpAddresses =
    lens _iniPrivateIpAddresses (\s a -> s { _iniPrivateIpAddresses = a })

-- | Indicates whether to validate network traffic to or from this network
-- interface.
iniSourceDestCheck :: Lens' InstanceNetworkInterface (Maybe Bool)
iniSourceDestCheck =
    lens _iniSourceDestCheck (\s a -> s { _iniSourceDestCheck = a })

-- | The status of the network interface.
iniStatus :: Lens' InstanceNetworkInterface (Maybe Text)
iniStatus = lens _iniStatus (\s a -> s { _iniStatus = a })

-- | The ID of the subnet.
iniSubnetId :: Lens' InstanceNetworkInterface (Maybe Text)
iniSubnetId = lens _iniSubnetId (\s a -> s { _iniSubnetId = a })

-- | The ID of the VPC.
iniVpcId :: Lens' InstanceNetworkInterface (Maybe Text)
iniVpcId = lens _iniVpcId (\s a -> s { _iniVpcId = a })

instance FromXML InstanceNetworkInterface where
    parseXML c = InstanceNetworkInterface
        <$> c .:? "association"
        <*> c .:? "attachment"
        <*> c .:? "description"
        <*> c .: "groupSet"
        <*> c .:? "macAddress"
        <*> c .:? "networkInterfaceId"
        <*> c .:? "ownerId"
        <*> c .:? "privateDnsName"
        <*> c .:? "privateIpAddress"
        <*> c .: "privateIpAddressesSet"
        <*> c .:? "sourceDestCheck"
        <*> c .:? "status"
        <*> c .:? "subnetId"
        <*> c .:? "vpcId"

instance ToQuery InstanceNetworkInterface

data VolumeStatusAction = VolumeStatusAction
    { _vsaCode        :: Maybe Text
    , _vsaDescription :: Maybe Text
    , _vsaEventId     :: Maybe Text
    , _vsaEventType   :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'VolumeStatusAction' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vsaCode' @::@ 'Maybe' 'Text'
--
-- * 'vsaDescription' @::@ 'Maybe' 'Text'
--
-- * 'vsaEventId' @::@ 'Maybe' 'Text'
--
-- * 'vsaEventType' @::@ 'Maybe' 'Text'
--
volumeStatusAction :: VolumeStatusAction
volumeStatusAction = VolumeStatusAction
    { _vsaCode        = Nothing
    , _vsaDescription = Nothing
    , _vsaEventType   = Nothing
    , _vsaEventId     = Nothing
    }

-- | The code identifying the operation, for example, enable-volume-io.
vsaCode :: Lens' VolumeStatusAction (Maybe Text)
vsaCode = lens _vsaCode (\s a -> s { _vsaCode = a })

-- | A description of the operation.
vsaDescription :: Lens' VolumeStatusAction (Maybe Text)
vsaDescription = lens _vsaDescription (\s a -> s { _vsaDescription = a })

-- | The ID of the event associated with this operation.
vsaEventId :: Lens' VolumeStatusAction (Maybe Text)
vsaEventId = lens _vsaEventId (\s a -> s { _vsaEventId = a })

-- | The event type associated with this operation.
vsaEventType :: Lens' VolumeStatusAction (Maybe Text)
vsaEventType = lens _vsaEventType (\s a -> s { _vsaEventType = a })

instance FromXML VolumeStatusAction where
    parseXML c = VolumeStatusAction
        <$> c .:? "code"
        <*> c .:? "description"
        <*> c .:? "eventId"
        <*> c .:? "eventType"

instance ToQuery VolumeStatusAction

data VpcPeeringConnectionVpcInfo = VpcPeeringConnectionVpcInfo
    { _vpcviCidrBlock :: Maybe Text
    , _vpcviOwnerId   :: Maybe Text
    , _vpcviVpcId     :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'VpcPeeringConnectionVpcInfo' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vpcviCidrBlock' @::@ 'Maybe' 'Text'
--
-- * 'vpcviOwnerId' @::@ 'Maybe' 'Text'
--
-- * 'vpcviVpcId' @::@ 'Maybe' 'Text'
--
vpcPeeringConnectionVpcInfo :: VpcPeeringConnectionVpcInfo
vpcPeeringConnectionVpcInfo = VpcPeeringConnectionVpcInfo
    { _vpcviCidrBlock = Nothing
    , _vpcviOwnerId   = Nothing
    , _vpcviVpcId     = Nothing
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
    parseXML c = VpcPeeringConnectionVpcInfo
        <$> c .:? "cidrBlock"
        <*> c .:? "ownerId"
        <*> c .:? "vpcId"

instance ToQuery VpcPeeringConnectionVpcInfo

data ReservedInstanceLimitPrice = ReservedInstanceLimitPrice
    { _rilpAmount       :: Maybe Double
    , _rilpCurrencyCode :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ReservedInstanceLimitPrice' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rilpAmount' @::@ 'Maybe' 'Double'
--
-- * 'rilpCurrencyCode' @::@ 'Maybe' 'Text'
--
reservedInstanceLimitPrice :: ReservedInstanceLimitPrice
reservedInstanceLimitPrice = ReservedInstanceLimitPrice
    { _rilpAmount       = Nothing
    , _rilpCurrencyCode = Nothing
    }

-- | Used for Reserved Instance Marketplace offerings. Specifies the limit
-- price on the total order (instanceCount * price).
rilpAmount :: Lens' ReservedInstanceLimitPrice (Maybe Double)
rilpAmount = lens _rilpAmount (\s a -> s { _rilpAmount = a })

-- | The currency in which the limitPrice amount is specified. At this time,
-- the only supported currency is USD.
rilpCurrencyCode :: Lens' ReservedInstanceLimitPrice (Maybe Text)
rilpCurrencyCode = lens _rilpCurrencyCode (\s a -> s { _rilpCurrencyCode = a })

instance FromXML ReservedInstanceLimitPrice where
    parseXML c = ReservedInstanceLimitPrice
        <$> c .:? "amount"
        <*> c .:? "currencyCode"

instance ToQuery ReservedInstanceLimitPrice

data Vpc = Vpc
    { _vpcCidrBlock       :: Maybe Text
    , _vpcDhcpOptionsId   :: Maybe Text
    , _vpcInstanceTenancy :: Maybe Text
    , _vpcIsDefault       :: Maybe Bool
    , _vpcState           :: Maybe Text
    , _vpcTags            :: [Tag]
    , _vpcVpcId           :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'Vpc' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vpcCidrBlock' @::@ 'Maybe' 'Text'
--
-- * 'vpcDhcpOptionsId' @::@ 'Maybe' 'Text'
--
-- * 'vpcInstanceTenancy' @::@ 'Maybe' 'Text'
--
-- * 'vpcIsDefault' @::@ 'Maybe' 'Bool'
--
-- * 'vpcState' @::@ 'Maybe' 'Text'
--
-- * 'vpcTags' @::@ ['Tag']
--
-- * 'vpcVpcId' @::@ 'Maybe' 'Text'
--
vpc :: Vpc
vpc = Vpc
    { _vpcVpcId           = Nothing
    , _vpcState           = Nothing
    , _vpcCidrBlock       = Nothing
    , _vpcDhcpOptionsId   = Nothing
    , _vpcTags            = mempty
    , _vpcInstanceTenancy = Nothing
    , _vpcIsDefault       = Nothing
    }

-- | The CIDR block for the VPC.
vpcCidrBlock :: Lens' Vpc (Maybe Text)
vpcCidrBlock = lens _vpcCidrBlock (\s a -> s { _vpcCidrBlock = a })

-- | The ID of the set of DHCP options you've associated with the VPC (or
-- default if the default options are associated with the VPC).
vpcDhcpOptionsId :: Lens' Vpc (Maybe Text)
vpcDhcpOptionsId = lens _vpcDhcpOptionsId (\s a -> s { _vpcDhcpOptionsId = a })

-- | The allowed tenancy of instances launched into the VPC.
vpcInstanceTenancy :: Lens' Vpc (Maybe Text)
vpcInstanceTenancy =
    lens _vpcInstanceTenancy (\s a -> s { _vpcInstanceTenancy = a })

-- | Indicates whether the VPC is the default VPC.
vpcIsDefault :: Lens' Vpc (Maybe Bool)
vpcIsDefault = lens _vpcIsDefault (\s a -> s { _vpcIsDefault = a })

-- | The current state of the VPC.
vpcState :: Lens' Vpc (Maybe Text)
vpcState = lens _vpcState (\s a -> s { _vpcState = a })

-- | Any tags assigned to the VPC.
vpcTags :: Lens' Vpc [Tag]
vpcTags = lens _vpcTags (\s a -> s { _vpcTags = a })

-- | The ID of the VPC.
vpcVpcId :: Lens' Vpc (Maybe Text)
vpcVpcId = lens _vpcVpcId (\s a -> s { _vpcVpcId = a })

instance FromXML Vpc where
    parseXML c = Vpc
        <$> c .:? "cidrBlock"
        <*> c .:? "dhcpOptionsId"
        <*> c .:? "instanceTenancy"
        <*> c .:? "isDefault"
        <*> c .:? "state"
        <*> c .: "tagSet"
        <*> c .:? "vpcId"

instance ToQuery Vpc

data InstanceStatus = InstanceStatus
    { _isAvailabilityZone :: Maybe Text
    , _isEvents           :: [InstanceStatusEvent]
    , _isInstanceId       :: Maybe Text
    , _isInstanceState    :: Maybe InstanceState
    , _isInstanceStatus   :: Maybe InstanceStatusSummary
    , _isSystemStatus     :: Maybe InstanceStatusSummary
    } deriving (Eq, Show, Generic)

-- | 'InstanceStatus' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'isAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'isEvents' @::@ ['InstanceStatusEvent']
--
-- * 'isInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'isInstanceState' @::@ 'Maybe' 'InstanceState'
--
-- * 'isInstanceStatus' @::@ 'Maybe' 'InstanceStatusSummary'
--
-- * 'isSystemStatus' @::@ 'Maybe' 'InstanceStatusSummary'
--
instanceStatus :: InstanceStatus
instanceStatus = InstanceStatus
    { _isInstanceId       = Nothing
    , _isAvailabilityZone = Nothing
    , _isEvents           = mempty
    , _isInstanceState    = Nothing
    , _isSystemStatus     = Nothing
    , _isInstanceStatus   = Nothing
    }

-- | The Availability Zone of the instance.
isAvailabilityZone :: Lens' InstanceStatus (Maybe Text)
isAvailabilityZone =
    lens _isAvailabilityZone (\s a -> s { _isAvailabilityZone = a })

-- | Extra information regarding events associated with the instance.
isEvents :: Lens' InstanceStatus [InstanceStatusEvent]
isEvents = lens _isEvents (\s a -> s { _isEvents = a })

-- | The ID of the instance.
isInstanceId :: Lens' InstanceStatus (Maybe Text)
isInstanceId = lens _isInstanceId (\s a -> s { _isInstanceId = a })

-- | The intended state of the instance. DescribeInstanceStatus requires that
-- an instance be in the running state.
isInstanceState :: Lens' InstanceStatus (Maybe InstanceState)
isInstanceState = lens _isInstanceState (\s a -> s { _isInstanceState = a })

-- | Reports impaired functionality that stems from issues internal to the
-- instance, such as impaired reachability.
isInstanceStatus :: Lens' InstanceStatus (Maybe InstanceStatusSummary)
isInstanceStatus = lens _isInstanceStatus (\s a -> s { _isInstanceStatus = a })

-- | Reports impaired functionality that stems from issues related to the
-- systems that support an instance, such as hardware failures and network
-- connectivity problems.
isSystemStatus :: Lens' InstanceStatus (Maybe InstanceStatusSummary)
isSystemStatus = lens _isSystemStatus (\s a -> s { _isSystemStatus = a })

instance FromXML InstanceStatus where
    parseXML c = InstanceStatus
        <$> c .:? "availabilityZone"
        <*> c .: "eventsSet"
        <*> c .:? "instanceId"
        <*> c .:? "instanceState"
        <*> c .:? "instanceStatus"
        <*> c .:? "systemStatus"

instance ToQuery InstanceStatus

data ArchitectureValues
    = I386  -- ^ i386
    | X8664 -- ^ x86_64
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable ArchitectureValues

instance FromText ArchitectureValues where
    parser = match "i386"   I386
         <|> match "x86_64" X8664

instance ToText ArchitectureValues where
    toText = \case
        I386  -> "i386"
        X8664 -> "x86_64"

instance FromXML ArchitectureValues where
    parseXML = fromXMLText "ArchitectureValues"

instance ToQuery ArchitectureValues

data ReportInstanceReasonCodes
    = InstanceStuckInState     -- ^ instance-stuck-in-state
    | NotAcceptingCredentials  -- ^ not-accepting-credentials
    | Other                    -- ^ other
    | PasswordNotAvailable     -- ^ password-not-available
    | PerformanceEbsVolume     -- ^ performance-ebs-volume
    | PerformanceInstanceStore -- ^ performance-instance-store
    | PerformanceNetwork       -- ^ performance-network
    | PerformanceOther         -- ^ performance-other
    | Unresponsive             -- ^ unresponsive
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable ReportInstanceReasonCodes

instance FromText ReportInstanceReasonCodes where
    parser = match "instance-stuck-in-state"    InstanceStuckInState
         <|> match "not-accepting-credentials"  NotAcceptingCredentials
         <|> match "other"                      Other
         <|> match "password-not-available"     PasswordNotAvailable
         <|> match "performance-ebs-volume"     PerformanceEbsVolume
         <|> match "performance-instance-store" PerformanceInstanceStore
         <|> match "performance-network"        PerformanceNetwork
         <|> match "performance-other"          PerformanceOther
         <|> match "unresponsive"               Unresponsive

instance ToText ReportInstanceReasonCodes where
    toText = \case
        InstanceStuckInState     -> "instance-stuck-in-state"
        NotAcceptingCredentials  -> "not-accepting-credentials"
        Other                    -> "other"
        PasswordNotAvailable     -> "password-not-available"
        PerformanceEbsVolume     -> "performance-ebs-volume"
        PerformanceInstanceStore -> "performance-instance-store"
        PerformanceNetwork       -> "performance-network"
        PerformanceOther         -> "performance-other"
        Unresponsive             -> "unresponsive"

instance FromXML ReportInstanceReasonCodes where
    parseXML = fromXMLText "ReportInstanceReasonCodes"

instance ToQuery ReportInstanceReasonCodes

data EbsBlockDevice = EbsBlockDevice
    { _ebdDeleteOnTermination :: Maybe Bool
    , _ebdEncrypted           :: Maybe Bool
    , _ebdIops                :: Maybe Int
    , _ebdSnapshotId          :: Maybe Text
    , _ebdVolumeSize          :: Maybe Int
    , _ebdVolumeType          :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'EbsBlockDevice' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ebdDeleteOnTermination' @::@ 'Maybe' 'Bool'
--
-- * 'ebdEncrypted' @::@ 'Maybe' 'Bool'
--
-- * 'ebdIops' @::@ 'Maybe' 'Int'
--
-- * 'ebdSnapshotId' @::@ 'Maybe' 'Text'
--
-- * 'ebdVolumeSize' @::@ 'Maybe' 'Int'
--
-- * 'ebdVolumeType' @::@ 'Maybe' 'Text'
--
ebsBlockDevice :: EbsBlockDevice
ebsBlockDevice = EbsBlockDevice
    { _ebdSnapshotId          = Nothing
    , _ebdVolumeSize          = Nothing
    , _ebdDeleteOnTermination = Nothing
    , _ebdVolumeType          = Nothing
    , _ebdIops                = Nothing
    , _ebdEncrypted           = Nothing
    }

-- | Indicates whether the Amazon EBS volume is deleted on instance
-- termination.
ebdDeleteOnTermination :: Lens' EbsBlockDevice (Maybe Bool)
ebdDeleteOnTermination =
    lens _ebdDeleteOnTermination (\s a -> s { _ebdDeleteOnTermination = a })

-- | Indicates whether the Amazon EBS volume is encrypted. Encrypted Amazon
-- EBS volumes may only be attached to instances that support Amazon EBS
-- encryption.
ebdEncrypted :: Lens' EbsBlockDevice (Maybe Bool)
ebdEncrypted = lens _ebdEncrypted (\s a -> s { _ebdEncrypted = a })

-- | The number of I/O operations per second (IOPS) that the volume supports.
-- For Provisioned IOPS (SSD) volumes, this represents the number of IOPS
-- that are provisioned for the volume. For General Purpose (SSD) volumes,
-- this represents the baseline performance of the volume and the rate at
-- which the volume accumulates I/O credits for bursting. For more
-- information on General Purpose (SSD) baseline performance, I/O credits,
-- and bursting, see Amazon EBS Volume Types in the Amazon Elastic Compute
-- Cloud User Guide. Constraint: Range is 100 to 4000 for Provisioned IOPS
-- (SSD) volumes and 3 to 3072 for General Purpose (SSD) volumes. Condition:
-- This parameter is required for requests to create io1 volumes; it is not
-- used in requests to create standard or gp2 volumes.
ebdIops :: Lens' EbsBlockDevice (Maybe Int)
ebdIops = lens _ebdIops (\s a -> s { _ebdIops = a })

-- | The ID of the snapshot.
ebdSnapshotId :: Lens' EbsBlockDevice (Maybe Text)
ebdSnapshotId = lens _ebdSnapshotId (\s a -> s { _ebdSnapshotId = a })

-- | The size of the volume, in GiB. Constraints: If the volume type is io1,
-- the minimum size of the volume is 10 GiB; otherwise, the minimum size is
-- 1 GiB. The maximum volume size is 1024 GiB. If you specify a snapshot,
-- the volume size must be equal to or larger than the snapshot size.
-- Default: If you're creating the volume from a snapshot and don't specify
-- a volume size, the default is the snapshot size.
ebdVolumeSize :: Lens' EbsBlockDevice (Maybe Int)
ebdVolumeSize = lens _ebdVolumeSize (\s a -> s { _ebdVolumeSize = a })

-- | The volume type. gp2 for General Purpose (SSD) volumes, io1 for
-- Provisioned IOPS (SSD) volumes, and standard for Magnetic volumes.
-- Default: standard.
ebdVolumeType :: Lens' EbsBlockDevice (Maybe Text)
ebdVolumeType = lens _ebdVolumeType (\s a -> s { _ebdVolumeType = a })

instance FromXML EbsBlockDevice where
    parseXML c = EbsBlockDevice
        <$> c .:? "deleteOnTermination"
        <*> c .:? "encrypted"
        <*> c .:? "iops"
        <*> c .:? "snapshotId"
        <*> c .:? "volumeSize"
        <*> c .:? "volumeType"

instance ToQuery EbsBlockDevice

data AccountAttribute = AccountAttribute
    { _aaAttributeName   :: Maybe Text
    , _aaAttributeValues :: [AccountAttributeValue]
    } deriving (Eq, Show, Generic)

-- | 'AccountAttribute' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aaAttributeName' @::@ 'Maybe' 'Text'
--
-- * 'aaAttributeValues' @::@ ['AccountAttributeValue']
--
accountAttribute :: AccountAttribute
accountAttribute = AccountAttribute
    { _aaAttributeName   = Nothing
    , _aaAttributeValues = mempty
    }

-- | The name of the account attribute.
aaAttributeName :: Lens' AccountAttribute (Maybe Text)
aaAttributeName = lens _aaAttributeName (\s a -> s { _aaAttributeName = a })

-- | One or more values for the account attribute.
aaAttributeValues :: Lens' AccountAttribute [AccountAttributeValue]
aaAttributeValues =
    lens _aaAttributeValues (\s a -> s { _aaAttributeValues = a })

instance FromXML AccountAttribute where
    parseXML c = AccountAttribute
        <$> c .:? "attributeName"
        <*> c .: "attributeValueSet"

instance ToQuery AccountAttribute

data PriceSchedule = PriceSchedule
    { _psActive       :: Maybe Bool
    , _psCurrencyCode :: Maybe Text
    , _psPrice        :: Maybe Double
    , _psTerm         :: Maybe Integer
    } deriving (Eq, Ord, Show, Generic)

-- | 'PriceSchedule' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'psActive' @::@ 'Maybe' 'Bool'
--
-- * 'psCurrencyCode' @::@ 'Maybe' 'Text'
--
-- * 'psPrice' @::@ 'Maybe' 'Double'
--
-- * 'psTerm' @::@ 'Maybe' 'Integer'
--
priceSchedule :: PriceSchedule
priceSchedule = PriceSchedule
    { _psTerm         = Nothing
    , _psPrice        = Nothing
    , _psCurrencyCode = Nothing
    , _psActive       = Nothing
    }

-- | The current price schedule, as determined by the term remaining for the
-- Reserved Instance in the listing. A specific price schedule is always in
-- effect, but only one price schedule can be active at any time. Take, for
-- example, a Reserved Instance listing that has five months remaining in
-- its term. When you specify price schedules for five months and two
-- months, this means that schedule 1, covering the first three months of
-- the remaining term, will be active during months 5, 4, and 3. Then
-- schedule 2, covering the last two months of the term, will be active for
-- months 2 and 1.
psActive :: Lens' PriceSchedule (Maybe Bool)
psActive = lens _psActive (\s a -> s { _psActive = a })

-- | The currency for transacting the Reserved Instance resale. At this time,
-- the only supported currency is USD.
psCurrencyCode :: Lens' PriceSchedule (Maybe Text)
psCurrencyCode = lens _psCurrencyCode (\s a -> s { _psCurrencyCode = a })

-- | The fixed price for the term.
psPrice :: Lens' PriceSchedule (Maybe Double)
psPrice = lens _psPrice (\s a -> s { _psPrice = a })

-- | The number of months remaining in the reservation. For example, 2 is the
-- second to the last month before the capacity reservation expires.
psTerm :: Lens' PriceSchedule (Maybe Integer)
psTerm = lens _psTerm (\s a -> s { _psTerm = a })

instance FromXML PriceSchedule where
    parseXML c = PriceSchedule
        <$> c .:? "active"
        <*> c .:? "currencyCode"
        <*> c .:? "price"
        <*> c .:? "term"

instance ToQuery PriceSchedule

data DeviceType
    = Ebs           -- ^ ebs
    | InstanceStore -- ^ instance-store
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable DeviceType

instance FromText DeviceType where
    parser = match "ebs"            Ebs
         <|> match "instance-store" InstanceStore

instance ToText DeviceType where
    toText = \case
        Ebs           -> "ebs"
        InstanceStore -> "instance-store"

instance FromXML DeviceType where
    parseXML = fromXMLText "DeviceType"

instance ToQuery DeviceType

data DomainType
    = DTStandard -- ^ standard
    | DTVpc      -- ^ vpc
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable DomainType

instance FromText DomainType where
    parser = match "standard" DTStandard
         <|> match "vpc"      DTVpc

instance ToText DomainType where
    toText = \case
        DTStandard -> "standard"
        DTVpc      -> "vpc"

instance FromXML DomainType where
    parseXML = fromXMLText "DomainType"

instance ToQuery DomainType

data Region = Region
    { _rEndpoint   :: Maybe Text
    , _rRegionName :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'Region' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rEndpoint' @::@ 'Maybe' 'Text'
--
-- * 'rRegionName' @::@ 'Maybe' 'Text'
--
region :: Region
region = Region
    { _rRegionName = Nothing
    , _rEndpoint   = Nothing
    }

-- | The region service endpoint.
rEndpoint :: Lens' Region (Maybe Text)
rEndpoint = lens _rEndpoint (\s a -> s { _rEndpoint = a })

-- | The name of the region.
rRegionName :: Lens' Region (Maybe Text)
rRegionName = lens _rRegionName (\s a -> s { _rRegionName = a })

instance FromXML Region where
    parseXML c = Region
        <$> c .:? "regionEndpoint"
        <*> c .:? "regionName"

instance ToQuery Region

newtype PropagatingVgw = PropagatingVgw
    { _pvGatewayId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'PropagatingVgw' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pvGatewayId' @::@ 'Maybe' 'Text'
--
propagatingVgw :: PropagatingVgw
propagatingVgw = PropagatingVgw
    { _pvGatewayId = Nothing
    }

-- | The ID of the virtual private gateway (VGW).
pvGatewayId :: Lens' PropagatingVgw (Maybe Text)
pvGatewayId = lens _pvGatewayId (\s a -> s { _pvGatewayId = a })

instance FromXML PropagatingVgw where
    parseXML c = PropagatingVgw
        <$> c .:? "gatewayId"

instance ToQuery PropagatingVgw

data OfferingTypeValues
    = HeavyUtilization  -- ^ Heavy Utilization
    | LightUtilization  -- ^ Light Utilization
    | MediumUtilization -- ^ Medium Utilization
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable OfferingTypeValues

instance FromText OfferingTypeValues where
    parser = match "Heavy Utilization"  HeavyUtilization
         <|> match "Light Utilization"  LightUtilization
         <|> match "Medium Utilization" MediumUtilization

instance ToText OfferingTypeValues where
    toText = \case
        HeavyUtilization  -> "Heavy Utilization"
        LightUtilization  -> "Light Utilization"
        MediumUtilization -> "Medium Utilization"

instance FromXML OfferingTypeValues where
    parseXML = fromXMLText "OfferingTypeValues"

instance ToQuery OfferingTypeValues

data VpnGateway = VpnGateway
    { _vgAvailabilityZone :: Maybe Text
    , _vgState            :: Maybe Text
    , _vgTags             :: [Tag]
    , _vgType             :: Maybe Text
    , _vgVpcAttachments   :: [VpcAttachment]
    , _vgVpnGatewayId     :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'VpnGateway' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vgAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'vgState' @::@ 'Maybe' 'Text'
--
-- * 'vgTags' @::@ ['Tag']
--
-- * 'vgType' @::@ 'Maybe' 'Text'
--
-- * 'vgVpcAttachments' @::@ ['VpcAttachment']
--
-- * 'vgVpnGatewayId' @::@ 'Maybe' 'Text'
--
vpnGateway :: VpnGateway
vpnGateway = VpnGateway
    { _vgVpnGatewayId     = Nothing
    , _vgState            = Nothing
    , _vgType             = Nothing
    , _vgAvailabilityZone = Nothing
    , _vgVpcAttachments   = mempty
    , _vgTags             = mempty
    }

-- | The Availability Zone where the virtual private gateway was created.
vgAvailabilityZone :: Lens' VpnGateway (Maybe Text)
vgAvailabilityZone =
    lens _vgAvailabilityZone (\s a -> s { _vgAvailabilityZone = a })

-- | The current state of the virtual private gateway.
vgState :: Lens' VpnGateway (Maybe Text)
vgState = lens _vgState (\s a -> s { _vgState = a })

-- | Any tags assigned to the virtual private gateway.
vgTags :: Lens' VpnGateway [Tag]
vgTags = lens _vgTags (\s a -> s { _vgTags = a })

-- | The type of VPN connection the virtual private gateway supports.
vgType :: Lens' VpnGateway (Maybe Text)
vgType = lens _vgType (\s a -> s { _vgType = a })

-- | Any VPCs attached to the virtual private gateway.
vgVpcAttachments :: Lens' VpnGateway [VpcAttachment]
vgVpcAttachments = lens _vgVpcAttachments (\s a -> s { _vgVpcAttachments = a })

-- | The ID of the virtual private gateway.
vgVpnGatewayId :: Lens' VpnGateway (Maybe Text)
vgVpnGatewayId = lens _vgVpnGatewayId (\s a -> s { _vgVpnGatewayId = a })

instance FromXML VpnGateway where
    parseXML c = VpnGateway
        <$> c .:? "availabilityZone"
        <*> c .:? "state"
        <*> c .: "tagSet"
        <*> c .:? "type"
        <*> c .: "attachments"
        <*> c .:? "vpnGatewayId"

instance ToQuery VpnGateway

data Filter = Filter
    { _fName   :: Text
    , _fValues :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'Filter' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'fName' @::@ 'Text'
--
-- * 'fValues' @::@ ['Text']
--
filter' :: Text -- ^ 'fName'
        -> Filter
filter' p1 = Filter
    { _fName   = p1
    , _fValues = mempty
    }

-- | The name of the filter.
fName :: Lens' Filter Text
fName = lens _fName (\s a -> s { _fName = a })

-- | One or more filter values.
fValues :: Lens' Filter [Text]
fValues = lens _fValues (\s a -> s { _fValues = a })

instance FromXML Filter where
    parseXML c = Filter
        <$> c .: "Name"
        <*> c .: "Value"

instance ToQuery Filter

data VolumeType
    = Gp2      -- ^ gp2
    | Io1      -- ^ io1
    | Standard -- ^ standard
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable VolumeType

instance FromText VolumeType where
    parser = match "gp2"      Gp2
         <|> match "io1"      Io1
         <|> match "standard" Standard

instance ToText VolumeType where
    toText = \case
        Gp2      -> "gp2"
        Io1      -> "io1"
        Standard -> "standard"

instance FromXML VolumeType where
    parseXML = fromXMLText "VolumeType"

instance ToQuery VolumeType

data InstanceStateChange = InstanceStateChange
    { _iscCurrentState  :: Maybe InstanceState
    , _iscInstanceId    :: Maybe Text
    , _iscPreviousState :: Maybe InstanceState
    } deriving (Eq, Show, Generic)

-- | 'InstanceStateChange' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iscCurrentState' @::@ 'Maybe' 'InstanceState'
--
-- * 'iscInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'iscPreviousState' @::@ 'Maybe' 'InstanceState'
--
instanceStateChange :: InstanceStateChange
instanceStateChange = InstanceStateChange
    { _iscInstanceId    = Nothing
    , _iscCurrentState  = Nothing
    , _iscPreviousState = Nothing
    }

-- | The current state of the instance.
iscCurrentState :: Lens' InstanceStateChange (Maybe InstanceState)
iscCurrentState = lens _iscCurrentState (\s a -> s { _iscCurrentState = a })

-- | The ID of the instance.
iscInstanceId :: Lens' InstanceStateChange (Maybe Text)
iscInstanceId = lens _iscInstanceId (\s a -> s { _iscInstanceId = a })

-- | The previous state of the instance.
iscPreviousState :: Lens' InstanceStateChange (Maybe InstanceState)
iscPreviousState = lens _iscPreviousState (\s a -> s { _iscPreviousState = a })

instance FromXML InstanceStateChange where
    parseXML c = InstanceStateChange
        <$> c .:? "currentState"
        <*> c .:? "instanceId"
        <*> c .:? "previousState"

instance ToQuery InstanceStateChange

data NetworkAcl = NetworkAcl
    { _naAssociations :: [NetworkAclAssociation]
    , _naEntries      :: [NetworkAclEntry]
    , _naIsDefault    :: Maybe Bool
    , _naNetworkAclId :: Maybe Text
    , _naTags         :: [Tag]
    , _naVpcId        :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'NetworkAcl' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'naAssociations' @::@ ['NetworkAclAssociation']
--
-- * 'naEntries' @::@ ['NetworkAclEntry']
--
-- * 'naIsDefault' @::@ 'Maybe' 'Bool'
--
-- * 'naNetworkAclId' @::@ 'Maybe' 'Text'
--
-- * 'naTags' @::@ ['Tag']
--
-- * 'naVpcId' @::@ 'Maybe' 'Text'
--
networkAcl :: NetworkAcl
networkAcl = NetworkAcl
    { _naNetworkAclId = Nothing
    , _naVpcId        = Nothing
    , _naIsDefault    = Nothing
    , _naEntries      = mempty
    , _naAssociations = mempty
    , _naTags         = mempty
    }

-- | Any associations between the network ACL and one or more subnets.
naAssociations :: Lens' NetworkAcl [NetworkAclAssociation]
naAssociations = lens _naAssociations (\s a -> s { _naAssociations = a })

-- | One or more entries (rules) in the network ACL.
naEntries :: Lens' NetworkAcl [NetworkAclEntry]
naEntries = lens _naEntries (\s a -> s { _naEntries = a })

-- | Indicates whether this is the default network ACL for the VPC.
naIsDefault :: Lens' NetworkAcl (Maybe Bool)
naIsDefault = lens _naIsDefault (\s a -> s { _naIsDefault = a })

-- | The ID of the network ACL.
naNetworkAclId :: Lens' NetworkAcl (Maybe Text)
naNetworkAclId = lens _naNetworkAclId (\s a -> s { _naNetworkAclId = a })

-- | Any tags assigned to the network ACL.
naTags :: Lens' NetworkAcl [Tag]
naTags = lens _naTags (\s a -> s { _naTags = a })

-- | The ID of the VPC for the network ACL.
naVpcId :: Lens' NetworkAcl (Maybe Text)
naVpcId = lens _naVpcId (\s a -> s { _naVpcId = a })

instance FromXML NetworkAcl where
    parseXML c = NetworkAcl
        <$> c .: "associationSet"
        <*> c .: "entrySet"
        <*> c .:? "default"
        <*> c .:? "networkAclId"
        <*> c .: "tagSet"
        <*> c .:? "vpcId"

instance ToQuery NetworkAcl

data ImageState
    = ISAvailable    -- ^ available
    | ISDeregistered -- ^ deregistered
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable ImageState

instance FromText ImageState where
    parser = match "available"    ISAvailable
         <|> match "deregistered" ISDeregistered

instance ToText ImageState where
    toText = \case
        ISAvailable    -> "available"
        ISDeregistered -> "deregistered"

instance FromXML ImageState where
    parseXML = fromXMLText "ImageState"

instance ToQuery ImageState

data GatewayType
    = Ipsec1 -- ^ ipsec.1
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable GatewayType

instance FromText GatewayType where
    parser = match "ipsec.1" Ipsec1

instance ToText GatewayType where
    toText Ipsec1 = "ipsec.1"

instance FromXML GatewayType where
    parseXML = fromXMLText "GatewayType"

instance ToQuery GatewayType

data InstanceNetworkInterfaceAttachment = InstanceNetworkInterfaceAttachment
    { _iniaAttachTime          :: Maybe RFC822
    , _iniaAttachmentId        :: Maybe Text
    , _iniaDeleteOnTermination :: Maybe Bool
    , _iniaDeviceIndex         :: Maybe Int
    , _iniaStatus              :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'InstanceNetworkInterfaceAttachment' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iniaAttachTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'iniaAttachmentId' @::@ 'Maybe' 'Text'
--
-- * 'iniaDeleteOnTermination' @::@ 'Maybe' 'Bool'
--
-- * 'iniaDeviceIndex' @::@ 'Maybe' 'Int'
--
-- * 'iniaStatus' @::@ 'Maybe' 'Text'
--
instanceNetworkInterfaceAttachment :: InstanceNetworkInterfaceAttachment
instanceNetworkInterfaceAttachment = InstanceNetworkInterfaceAttachment
    { _iniaAttachmentId        = Nothing
    , _iniaDeviceIndex         = Nothing
    , _iniaStatus              = Nothing
    , _iniaAttachTime          = Nothing
    , _iniaDeleteOnTermination = Nothing
    }

-- | The time stamp when the attachment initiated.
iniaAttachTime :: Lens' InstanceNetworkInterfaceAttachment (Maybe UTCTime)
iniaAttachTime = lens _iniaAttachTime (\s a -> s { _iniaAttachTime = a })
    . mapping _Time

-- | The ID of the network interface attachment.
iniaAttachmentId :: Lens' InstanceNetworkInterfaceAttachment (Maybe Text)
iniaAttachmentId = lens _iniaAttachmentId (\s a -> s { _iniaAttachmentId = a })

-- | Indicates whether the network interface is deleted when the instance is
-- terminated.
iniaDeleteOnTermination :: Lens' InstanceNetworkInterfaceAttachment (Maybe Bool)
iniaDeleteOnTermination =
    lens _iniaDeleteOnTermination (\s a -> s { _iniaDeleteOnTermination = a })

-- | The index of the device on the instance for the network interface
-- attachment.
iniaDeviceIndex :: Lens' InstanceNetworkInterfaceAttachment (Maybe Int)
iniaDeviceIndex = lens _iniaDeviceIndex (\s a -> s { _iniaDeviceIndex = a })

-- | The attachment state.
iniaStatus :: Lens' InstanceNetworkInterfaceAttachment (Maybe Text)
iniaStatus = lens _iniaStatus (\s a -> s { _iniaStatus = a })

instance FromXML InstanceNetworkInterfaceAttachment where
    parseXML c = InstanceNetworkInterfaceAttachment
        <$> c .:? "attachTime"
        <*> c .:? "attachmentId"
        <*> c .:? "deleteOnTermination"
        <*> c .:? "deviceIndex"
        <*> c .:? "status"

instance ToQuery InstanceNetworkInterfaceAttachment

newtype AttributeBooleanValue = AttributeBooleanValue
    { _abvValue :: Maybe Bool
    } deriving (Eq, Ord, Show, Generic)

-- | 'AttributeBooleanValue' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'abvValue' @::@ 'Maybe' 'Bool'
--
attributeBooleanValue :: AttributeBooleanValue
attributeBooleanValue = AttributeBooleanValue
    { _abvValue = Nothing
    }

abvValue :: Lens' AttributeBooleanValue (Maybe Bool)
abvValue = lens _abvValue (\s a -> s { _abvValue = a })

instance FromXML AttributeBooleanValue where
    parseXML c = AttributeBooleanValue
        <$> c .:? "value"

instance ToQuery AttributeBooleanValue

data RecurringCharge = RecurringCharge
    { _rcAmount    :: Maybe Double
    , _rcFrequency :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'RecurringCharge' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcAmount' @::@ 'Maybe' 'Double'
--
-- * 'rcFrequency' @::@ 'Maybe' 'Text'
--
recurringCharge :: RecurringCharge
recurringCharge = RecurringCharge
    { _rcFrequency = Nothing
    , _rcAmount    = Nothing
    }

-- | The amount of the recurring charge.
rcAmount :: Lens' RecurringCharge (Maybe Double)
rcAmount = lens _rcAmount (\s a -> s { _rcAmount = a })

-- | The frequency of the recurring charge.
rcFrequency :: Lens' RecurringCharge (Maybe Text)
rcFrequency = lens _rcFrequency (\s a -> s { _rcFrequency = a })

instance FromXML RecurringCharge where
    parseXML c = RecurringCharge
        <$> c .:? "amount"
        <*> c .:? "frequency"

instance ToQuery RecurringCharge

data NewDhcpConfiguration = NewDhcpConfiguration
    { _ndcKey    :: Maybe Text
    , _ndcValues :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'NewDhcpConfiguration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ndcKey' @::@ 'Maybe' 'Text'
--
-- * 'ndcValues' @::@ ['Text']
--
newDhcpConfiguration :: NewDhcpConfiguration
newDhcpConfiguration = NewDhcpConfiguration
    { _ndcKey    = Nothing
    , _ndcValues = mempty
    }

ndcKey :: Lens' NewDhcpConfiguration (Maybe Text)
ndcKey = lens _ndcKey (\s a -> s { _ndcKey = a })

ndcValues :: Lens' NewDhcpConfiguration [Text]
ndcValues = lens _ndcValues (\s a -> s { _ndcValues = a })

instance FromXML NewDhcpConfiguration where
    parseXML c = NewDhcpConfiguration
        <$> c .:? "key"
        <*> c .: "Value"

instance ToQuery NewDhcpConfiguration

data StateReason = StateReason
    { _srCode    :: Maybe Text
    , _srMessage :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'StateReason' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'srCode' @::@ 'Maybe' 'Text'
--
-- * 'srMessage' @::@ 'Maybe' 'Text'
--
stateReason :: StateReason
stateReason = StateReason
    { _srCode    = Nothing
    , _srMessage = Nothing
    }

-- | The reason code for the state change.
srCode :: Lens' StateReason (Maybe Text)
srCode = lens _srCode (\s a -> s { _srCode = a })

-- | The message for the state change. Server.SpotInstanceTermination: A Spot
-- Instance was terminated due to an increase in the market price.
-- Server.InternalError: An internal error occurred during instance launch,
-- resulting in termination. Server.InsufficientInstanceCapacity: There was
-- insufficient instance capacity to satisfy the launch request.
-- Client.InternalError: A client error caused the instance to terminate on
-- launch. Client.InstanceInitiatedShutdown: The instance was shut down
-- using the shutdown -h command from the instance.
-- Client.UserInitiatedShutdown: The instance was shut down using the Amazon
-- EC2 API. Client.VolumeLimitExceeded: The volume limit was exceeded.
-- Client.InvalidSnapshot.NotFound: The specified snapshot was not found.
srMessage :: Lens' StateReason (Maybe Text)
srMessage = lens _srMessage (\s a -> s { _srMessage = a })

instance FromXML StateReason where
    parseXML c = StateReason
        <$> c .:? "code"
        <*> c .:? "message"

instance ToQuery StateReason

data MonitoringState
    = MSDisabled -- ^ disabled
    | MSEnabled  -- ^ enabled
    | MSPending  -- ^ pending
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable MonitoringState

instance FromText MonitoringState where
    parser = match "disabled" MSDisabled
         <|> match "enabled"  MSEnabled
         <|> match "pending"  MSPending

instance ToText MonitoringState where
    toText = \case
        MSDisabled -> "disabled"
        MSEnabled  -> "enabled"
        MSPending  -> "pending"

instance FromXML MonitoringState where
    parseXML = fromXMLText "MonitoringState"

instance ToQuery MonitoringState

newtype ReservedInstancesId = ReservedInstancesId
    { _riiReservedInstancesId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'ReservedInstancesId' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'riiReservedInstancesId' @::@ 'Maybe' 'Text'
--
reservedInstancesId :: ReservedInstancesId
reservedInstancesId = ReservedInstancesId
    { _riiReservedInstancesId = Nothing
    }

-- | The ID of the Reserved Instance.
riiReservedInstancesId :: Lens' ReservedInstancesId (Maybe Text)
riiReservedInstancesId =
    lens _riiReservedInstancesId (\s a -> s { _riiReservedInstancesId = a })

instance FromXML ReservedInstancesId where
    parseXML c = ReservedInstancesId
        <$> c .:? "reservedInstancesId"

instance ToQuery ReservedInstancesId

data StatusName
    = Reachability -- ^ reachability
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable StatusName

instance FromText StatusName where
    parser = match "reachability" Reachability

instance ToText StatusName where
    toText Reachability = "reachability"

instance FromXML StatusName where
    parseXML = fromXMLText "StatusName"

instance ToQuery StatusName

data InternetGateway = InternetGateway
    { _igAttachments       :: [InternetGatewayAttachment]
    , _igInternetGatewayId :: Text
    , _igTags              :: [Tag]
    } deriving (Eq, Show, Generic)

-- | 'InternetGateway' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'igAttachments' @::@ ['InternetGatewayAttachment']
--
-- * 'igInternetGatewayId' @::@ 'Text'
--
-- * 'igTags' @::@ ['Tag']
--
internetGateway :: Text -- ^ 'igInternetGatewayId'
                -> InternetGateway
internetGateway p1 = InternetGateway
    { _igInternetGatewayId = p1
    , _igAttachments       = mempty
    , _igTags              = mempty
    }

-- | Any VPCs attached to the Internet gateway.
igAttachments :: Lens' InternetGateway [InternetGatewayAttachment]
igAttachments = lens _igAttachments (\s a -> s { _igAttachments = a })

-- | The ID of the Internet gateway.
igInternetGatewayId :: Lens' InternetGateway Text
igInternetGatewayId =
    lens _igInternetGatewayId (\s a -> s { _igInternetGatewayId = a })

-- | Any tags assigned to the Internet gateway.
igTags :: Lens' InternetGateway [Tag]
igTags = lens _igTags (\s a -> s { _igTags = a })

instance FromXML InternetGateway where
    parseXML c = InternetGateway
        <$> c .: "attachmentSet"
        <*> c .: "internetGatewayId"
        <*> c .: "tagSet"

instance ToQuery InternetGateway

data VolumeStatusName
    = IoEnabled     -- ^ io-enabled
    | IoPerformance -- ^ io-performance
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable VolumeStatusName

instance FromText VolumeStatusName where
    parser = match "io-enabled"     IoEnabled
         <|> match "io-performance" IoPerformance

instance ToText VolumeStatusName where
    toText = \case
        IoEnabled     -> "io-enabled"
        IoPerformance -> "io-performance"

instance FromXML VolumeStatusName where
    parseXML = fromXMLText "VolumeStatusName"

instance ToQuery VolumeStatusName

data VolumeAttributeName
    = AutoEnableIO -- ^ autoEnableIO
    | ProductCodes -- ^ productCodes
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable VolumeAttributeName

instance FromText VolumeAttributeName where
    parser = match "autoEnableIO" AutoEnableIO
         <|> match "productCodes" ProductCodes

instance ToText VolumeAttributeName where
    toText = \case
        AutoEnableIO -> "autoEnableIO"
        ProductCodes -> "productCodes"

instance FromXML VolumeAttributeName where
    parseXML = fromXMLText "VolumeAttributeName"

instance ToQuery VolumeAttributeName

data ImportInstanceTaskDetails = ImportInstanceTaskDetails
    { _iitdDescription :: Maybe Text
    , _iitdInstanceId  :: Maybe Text
    , _iitdPlatform    :: Maybe Text
    , _iitdVolumes     :: [ImportInstanceVolumeDetailItem]
    } deriving (Eq, Show, Generic)

-- | 'ImportInstanceTaskDetails' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iitdDescription' @::@ 'Maybe' 'Text'
--
-- * 'iitdInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'iitdPlatform' @::@ 'Maybe' 'Text'
--
-- * 'iitdVolumes' @::@ ['ImportInstanceVolumeDetailItem']
--
importInstanceTaskDetails :: ImportInstanceTaskDetails
importInstanceTaskDetails = ImportInstanceTaskDetails
    { _iitdVolumes     = mempty
    , _iitdInstanceId  = Nothing
    , _iitdPlatform    = Nothing
    , _iitdDescription = Nothing
    }

iitdDescription :: Lens' ImportInstanceTaskDetails (Maybe Text)
iitdDescription = lens _iitdDescription (\s a -> s { _iitdDescription = a })

iitdInstanceId :: Lens' ImportInstanceTaskDetails (Maybe Text)
iitdInstanceId = lens _iitdInstanceId (\s a -> s { _iitdInstanceId = a })

-- | The instance operating system.
iitdPlatform :: Lens' ImportInstanceTaskDetails (Maybe Text)
iitdPlatform = lens _iitdPlatform (\s a -> s { _iitdPlatform = a })

iitdVolumes :: Lens' ImportInstanceTaskDetails [ImportInstanceVolumeDetailItem]
iitdVolumes = lens _iitdVolumes (\s a -> s { _iitdVolumes = a })

instance FromXML ImportInstanceTaskDetails where
    parseXML c = ImportInstanceTaskDetails
        <$> c .:? "description"
        <*> c .:? "instanceId"
        <*> c .:? "platform"
        <*> c .: "volumes"

instance ToQuery ImportInstanceTaskDetails

data PlacementGroup = PlacementGroup
    { _pgGroupName :: Maybe Text
    , _pgState     :: Maybe Text
    , _pgStrategy  :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'PlacementGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pgGroupName' @::@ 'Maybe' 'Text'
--
-- * 'pgState' @::@ 'Maybe' 'Text'
--
-- * 'pgStrategy' @::@ 'Maybe' 'Text'
--
placementGroup :: PlacementGroup
placementGroup = PlacementGroup
    { _pgGroupName = Nothing
    , _pgStrategy  = Nothing
    , _pgState     = Nothing
    }

-- | The name of the placement group.
pgGroupName :: Lens' PlacementGroup (Maybe Text)
pgGroupName = lens _pgGroupName (\s a -> s { _pgGroupName = a })

-- | The state of the placement group.
pgState :: Lens' PlacementGroup (Maybe Text)
pgState = lens _pgState (\s a -> s { _pgState = a })

-- | The placement strategy.
pgStrategy :: Lens' PlacementGroup (Maybe Text)
pgStrategy = lens _pgStrategy (\s a -> s { _pgStrategy = a })

instance FromXML PlacementGroup where
    parseXML c = PlacementGroup
        <$> c .:? "groupName"
        <*> c .:? "state"
        <*> c .:? "strategy"

instance ToQuery PlacementGroup

data ProductCode = ProductCode
    { _pcProductCodeId   :: Maybe Text
    , _pcProductCodeType :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ProductCode' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pcProductCodeId' @::@ 'Maybe' 'Text'
--
-- * 'pcProductCodeType' @::@ 'Maybe' 'Text'
--
productCode :: ProductCode
productCode = ProductCode
    { _pcProductCodeId   = Nothing
    , _pcProductCodeType = Nothing
    }

-- | The product code.
pcProductCodeId :: Lens' ProductCode (Maybe Text)
pcProductCodeId = lens _pcProductCodeId (\s a -> s { _pcProductCodeId = a })

-- | The type of product code.
pcProductCodeType :: Lens' ProductCode (Maybe Text)
pcProductCodeType =
    lens _pcProductCodeType (\s a -> s { _pcProductCodeType = a })

instance FromXML ProductCode where
    parseXML c = ProductCode
        <$> c .:? "productCode"
        <*> c .:? "type"

instance ToQuery ProductCode

data ListingStatus
    = ListingStatusActive    -- ^ active
    | ListingStatusCancelled -- ^ cancelled
    | ListingStatusClosed    -- ^ closed
    | ListingStatusPending   -- ^ pending
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable ListingStatus

instance FromText ListingStatus where
    parser = match "active"    ListingStatusActive
         <|> match "cancelled" ListingStatusCancelled
         <|> match "closed"    ListingStatusClosed
         <|> match "pending"   ListingStatusPending

instance ToText ListingStatus where
    toText = \case
        ListingStatusActive    -> "active"
        ListingStatusCancelled -> "cancelled"
        ListingStatusClosed    -> "closed"
        ListingStatusPending   -> "pending"

instance FromXML ListingStatus where
    parseXML = fromXMLText "ListingStatus"

instance ToQuery ListingStatus

newtype IpRange = IpRange
    { _irCidrIp :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'IpRange' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'irCidrIp' @::@ 'Text'
--
ipRange :: Text -- ^ 'irCidrIp'
        -> IpRange
ipRange p1 = IpRange
    { _irCidrIp = p1
    }

-- | The CIDR range. You can either specify a CIDR range or a source security
-- group, not both.
irCidrIp :: Lens' IpRange Text
irCidrIp = lens _irCidrIp (\s a -> s { _irCidrIp = a })

instance FromXML IpRange where
    parseXML c = IpRange
        <$> c .: "cidrIp"

instance ToQuery IpRange

data VolumeStatusInfoStatus
    = VSISImpaired         -- ^ impaired
    | VSISInsufficientData -- ^ insufficient-data
    | VSISOk               -- ^ ok
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable VolumeStatusInfoStatus

instance FromText VolumeStatusInfoStatus where
    parser = match "impaired"          VSISImpaired
         <|> match "insufficient-data" VSISInsufficientData
         <|> match "ok"                VSISOk

instance ToText VolumeStatusInfoStatus where
    toText = \case
        VSISImpaired         -> "impaired"
        VSISInsufficientData -> "insufficient-data"
        VSISOk               -> "ok"

instance FromXML VolumeStatusInfoStatus where
    parseXML = fromXMLText "VolumeStatusInfoStatus"

instance ToQuery VolumeStatusInfoStatus

newtype AccountAttributeValue = AccountAttributeValue
    { _aavAttributeValue :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'AccountAttributeValue' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aavAttributeValue' @::@ 'Maybe' 'Text'
--
accountAttributeValue :: AccountAttributeValue
accountAttributeValue = AccountAttributeValue
    { _aavAttributeValue = Nothing
    }

-- | The value of the attribute.
aavAttributeValue :: Lens' AccountAttributeValue (Maybe Text)
aavAttributeValue =
    lens _aavAttributeValue (\s a -> s { _aavAttributeValue = a })

instance FromXML AccountAttributeValue where
    parseXML c = AccountAttributeValue
        <$> c .:? "attributeValue"

instance ToQuery AccountAttributeValue

data RIProductDescription
    = RIPDLinuxUNIX          -- ^ Linux/UNIX
    | RIPDLinuxUNIXAmazonVPC -- ^ Linux/UNIX (Amazon VPC)
    | RIPDWindows            -- ^ Windows
    | RIPDWindowsAmazonVPC   -- ^ Windows (Amazon VPC)
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable RIProductDescription

instance FromText RIProductDescription where
    parser = match "Linux/UNIX"              RIPDLinuxUNIX
         <|> match "Linux/UNIX (Amazon VPC)" RIPDLinuxUNIXAmazonVPC
         <|> match "Windows"                 RIPDWindows
         <|> match "Windows (Amazon VPC)"    RIPDWindowsAmazonVPC

instance ToText RIProductDescription where
    toText = \case
        RIPDLinuxUNIX          -> "Linux/UNIX"
        RIPDLinuxUNIXAmazonVPC -> "Linux/UNIX (Amazon VPC)"
        RIPDWindows            -> "Windows"
        RIPDWindowsAmazonVPC   -> "Windows (Amazon VPC)"

instance FromXML RIProductDescription where
    parseXML = fromXMLText "RIProductDescription"

instance ToQuery RIProductDescription

data ReservedInstancesOffering = ReservedInstancesOffering
    { _rioAvailabilityZone            :: Maybe Text
    , _rioCurrencyCode                :: Maybe Text
    , _rioDuration                    :: Maybe Integer
    , _rioFixedPrice                  :: Maybe Double
    , _rioInstanceTenancy             :: Maybe Text
    , _rioInstanceType                :: Maybe Text
    , _rioMarketplace                 :: Maybe Bool
    , _rioOfferingType                :: Maybe Text
    , _rioPricingDetails              :: [PricingDetail]
    , _rioProductDescription          :: Maybe Text
    , _rioRecurringCharges            :: [RecurringCharge]
    , _rioReservedInstancesOfferingId :: Maybe Text
    , _rioUsagePrice                  :: Maybe Double
    } deriving (Eq, Show, Generic)

-- | 'ReservedInstancesOffering' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rioAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'rioCurrencyCode' @::@ 'Maybe' 'Text'
--
-- * 'rioDuration' @::@ 'Maybe' 'Integer'
--
-- * 'rioFixedPrice' @::@ 'Maybe' 'Double'
--
-- * 'rioInstanceTenancy' @::@ 'Maybe' 'Text'
--
-- * 'rioInstanceType' @::@ 'Maybe' 'Text'
--
-- * 'rioMarketplace' @::@ 'Maybe' 'Bool'
--
-- * 'rioOfferingType' @::@ 'Maybe' 'Text'
--
-- * 'rioPricingDetails' @::@ ['PricingDetail']
--
-- * 'rioProductDescription' @::@ 'Maybe' 'Text'
--
-- * 'rioRecurringCharges' @::@ ['RecurringCharge']
--
-- * 'rioReservedInstancesOfferingId' @::@ 'Maybe' 'Text'
--
-- * 'rioUsagePrice' @::@ 'Maybe' 'Double'
--
reservedInstancesOffering :: ReservedInstancesOffering
reservedInstancesOffering = ReservedInstancesOffering
    { _rioReservedInstancesOfferingId = Nothing
    , _rioInstanceType                = Nothing
    , _rioAvailabilityZone            = Nothing
    , _rioDuration                    = Nothing
    , _rioUsagePrice                  = Nothing
    , _rioFixedPrice                  = Nothing
    , _rioProductDescription          = Nothing
    , _rioInstanceTenancy             = Nothing
    , _rioCurrencyCode                = Nothing
    , _rioOfferingType                = Nothing
    , _rioRecurringCharges            = mempty
    , _rioMarketplace                 = Nothing
    , _rioPricingDetails              = mempty
    }

-- | The Availability Zone in which the Reserved Instance can be used.
rioAvailabilityZone :: Lens' ReservedInstancesOffering (Maybe Text)
rioAvailabilityZone =
    lens _rioAvailabilityZone (\s a -> s { _rioAvailabilityZone = a })

-- | The currency of the Reserved Instance offering you are purchasing. It's
-- specified using ISO 4217 standard currency codes. At this time, the only
-- supported currency is USD.
rioCurrencyCode :: Lens' ReservedInstancesOffering (Maybe Text)
rioCurrencyCode = lens _rioCurrencyCode (\s a -> s { _rioCurrencyCode = a })

-- | The duration of the Reserved Instance, in seconds.
rioDuration :: Lens' ReservedInstancesOffering (Maybe Integer)
rioDuration = lens _rioDuration (\s a -> s { _rioDuration = a })

-- | The purchase price of the Reserved Instance.
rioFixedPrice :: Lens' ReservedInstancesOffering (Maybe Double)
rioFixedPrice = lens _rioFixedPrice (\s a -> s { _rioFixedPrice = a })

-- | The tenancy of the reserved instance.
rioInstanceTenancy :: Lens' ReservedInstancesOffering (Maybe Text)
rioInstanceTenancy =
    lens _rioInstanceTenancy (\s a -> s { _rioInstanceTenancy = a })

-- | The instance type on which the Reserved Instance can be used.
rioInstanceType :: Lens' ReservedInstancesOffering (Maybe Text)
rioInstanceType = lens _rioInstanceType (\s a -> s { _rioInstanceType = a })

-- | Indicates whether the offering is available through the Reserved Instance
-- Marketplace (resale) or AWS. If it's a Reserved Instance Marketplace
-- offering, this is true.
rioMarketplace :: Lens' ReservedInstancesOffering (Maybe Bool)
rioMarketplace = lens _rioMarketplace (\s a -> s { _rioMarketplace = a })

-- | The Reserved Instance offering type.
rioOfferingType :: Lens' ReservedInstancesOffering (Maybe Text)
rioOfferingType = lens _rioOfferingType (\s a -> s { _rioOfferingType = a })

-- | The pricing details of the Reserved Instance offering.
rioPricingDetails :: Lens' ReservedInstancesOffering [PricingDetail]
rioPricingDetails =
    lens _rioPricingDetails (\s a -> s { _rioPricingDetails = a })

-- | The Reserved Instance description.
rioProductDescription :: Lens' ReservedInstancesOffering (Maybe Text)
rioProductDescription =
    lens _rioProductDescription (\s a -> s { _rioProductDescription = a })

-- | The recurring charge tag assigned to the resource.
rioRecurringCharges :: Lens' ReservedInstancesOffering [RecurringCharge]
rioRecurringCharges =
    lens _rioRecurringCharges (\s a -> s { _rioRecurringCharges = a })

-- | The ID of the Reserved Instance offering.
rioReservedInstancesOfferingId :: Lens' ReservedInstancesOffering (Maybe Text)
rioReservedInstancesOfferingId =
    lens _rioReservedInstancesOfferingId
        (\s a -> s { _rioReservedInstancesOfferingId = a })

-- | The usage price of the Reserved Instance, per hour.
rioUsagePrice :: Lens' ReservedInstancesOffering (Maybe Double)
rioUsagePrice = lens _rioUsagePrice (\s a -> s { _rioUsagePrice = a })

instance FromXML ReservedInstancesOffering where
    parseXML c = ReservedInstancesOffering
        <$> c .:? "availabilityZone"
        <*> c .:? "currencyCode"
        <*> c .:? "duration"
        <*> c .:? "fixedPrice"
        <*> c .:? "instanceTenancy"
        <*> c .:? "instanceType"
        <*> c .:? "marketplace"
        <*> c .:? "offeringType"
        <*> c .: "pricingDetailsSet"
        <*> c .:? "productDescription"
        <*> c .: "recurringCharges"
        <*> c .:? "reservedInstancesOfferingId"
        <*> c .:? "usagePrice"

instance ToQuery ReservedInstancesOffering

data ReservedInstances = ReservedInstances
    { _ri1AvailabilityZone    :: Maybe Text
    , _ri1CurrencyCode        :: Maybe Text
    , _ri1Duration            :: Maybe Integer
    , _ri1End                 :: Maybe RFC822
    , _ri1FixedPrice          :: Maybe Double
    , _ri1InstanceCount       :: Maybe Int
    , _ri1InstanceTenancy     :: Maybe Text
    , _ri1InstanceType        :: Maybe Text
    , _ri1OfferingType        :: Maybe Text
    , _ri1ProductDescription  :: Maybe Text
    , _ri1RecurringCharges    :: [RecurringCharge]
    , _ri1ReservedInstancesId :: Maybe Text
    , _ri1Start               :: Maybe RFC822
    , _ri1State               :: Maybe Text
    , _ri1Tags                :: [Tag]
    , _ri1UsagePrice          :: Maybe Double
    } deriving (Eq, Show, Generic)

-- | 'ReservedInstances' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ri1AvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'ri1CurrencyCode' @::@ 'Maybe' 'Text'
--
-- * 'ri1Duration' @::@ 'Maybe' 'Integer'
--
-- * 'ri1End' @::@ 'Maybe' 'UTCTime'
--
-- * 'ri1FixedPrice' @::@ 'Maybe' 'Double'
--
-- * 'ri1InstanceCount' @::@ 'Maybe' 'Int'
--
-- * 'ri1InstanceTenancy' @::@ 'Maybe' 'Text'
--
-- * 'ri1InstanceType' @::@ 'Maybe' 'Text'
--
-- * 'ri1OfferingType' @::@ 'Maybe' 'Text'
--
-- * 'ri1ProductDescription' @::@ 'Maybe' 'Text'
--
-- * 'ri1RecurringCharges' @::@ ['RecurringCharge']
--
-- * 'ri1ReservedInstancesId' @::@ 'Maybe' 'Text'
--
-- * 'ri1Start' @::@ 'Maybe' 'UTCTime'
--
-- * 'ri1State' @::@ 'Maybe' 'Text'
--
-- * 'ri1Tags' @::@ ['Tag']
--
-- * 'ri1UsagePrice' @::@ 'Maybe' 'Double'
--
reservedInstances :: ReservedInstances
reservedInstances = ReservedInstances
    { _ri1ReservedInstancesId = Nothing
    , _ri1InstanceType        = Nothing
    , _ri1AvailabilityZone    = Nothing
    , _ri1Start               = Nothing
    , _ri1End                 = Nothing
    , _ri1Duration            = Nothing
    , _ri1UsagePrice          = Nothing
    , _ri1FixedPrice          = Nothing
    , _ri1InstanceCount       = Nothing
    , _ri1ProductDescription  = Nothing
    , _ri1State               = Nothing
    , _ri1Tags                = mempty
    , _ri1InstanceTenancy     = Nothing
    , _ri1CurrencyCode        = Nothing
    , _ri1OfferingType        = Nothing
    , _ri1RecurringCharges    = mempty
    }

-- | The Availability Zone in which the Reserved Instance can be used.
ri1AvailabilityZone :: Lens' ReservedInstances (Maybe Text)
ri1AvailabilityZone =
    lens _ri1AvailabilityZone (\s a -> s { _ri1AvailabilityZone = a })

-- | The currency of the Reserved Instance. It's specified using ISO 4217
-- standard currency codes. At this time, the only supported currency is
-- USD.
ri1CurrencyCode :: Lens' ReservedInstances (Maybe Text)
ri1CurrencyCode = lens _ri1CurrencyCode (\s a -> s { _ri1CurrencyCode = a })

-- | The duration of the Reserved Instance, in seconds.
ri1Duration :: Lens' ReservedInstances (Maybe Integer)
ri1Duration = lens _ri1Duration (\s a -> s { _ri1Duration = a })

-- | The time when the Reserved Instance expires.
ri1End :: Lens' ReservedInstances (Maybe UTCTime)
ri1End = lens _ri1End (\s a -> s { _ri1End = a })
    . mapping _Time

-- | The purchase price of the Reserved Instance.
ri1FixedPrice :: Lens' ReservedInstances (Maybe Double)
ri1FixedPrice = lens _ri1FixedPrice (\s a -> s { _ri1FixedPrice = a })

-- | The number of Reserved Instances purchased.
ri1InstanceCount :: Lens' ReservedInstances (Maybe Int)
ri1InstanceCount = lens _ri1InstanceCount (\s a -> s { _ri1InstanceCount = a })

-- | The tenancy of the reserved instance.
ri1InstanceTenancy :: Lens' ReservedInstances (Maybe Text)
ri1InstanceTenancy =
    lens _ri1InstanceTenancy (\s a -> s { _ri1InstanceTenancy = a })

-- | The instance type on which the Reserved Instance can be used.
ri1InstanceType :: Lens' ReservedInstances (Maybe Text)
ri1InstanceType = lens _ri1InstanceType (\s a -> s { _ri1InstanceType = a })

-- | The Reserved Instance offering type.
ri1OfferingType :: Lens' ReservedInstances (Maybe Text)
ri1OfferingType = lens _ri1OfferingType (\s a -> s { _ri1OfferingType = a })

-- | The Reserved Instance description.
ri1ProductDescription :: Lens' ReservedInstances (Maybe Text)
ri1ProductDescription =
    lens _ri1ProductDescription (\s a -> s { _ri1ProductDescription = a })

-- | The recurring charge tag assigned to the resource.
ri1RecurringCharges :: Lens' ReservedInstances [RecurringCharge]
ri1RecurringCharges =
    lens _ri1RecurringCharges (\s a -> s { _ri1RecurringCharges = a })

-- | The ID of the Reserved Instance.
ri1ReservedInstancesId :: Lens' ReservedInstances (Maybe Text)
ri1ReservedInstancesId =
    lens _ri1ReservedInstancesId (\s a -> s { _ri1ReservedInstancesId = a })

-- | The date and time the Reserved Instance started.
ri1Start :: Lens' ReservedInstances (Maybe UTCTime)
ri1Start = lens _ri1Start (\s a -> s { _ri1Start = a })
    . mapping _Time

-- | The state of the Reserved Instance purchase.
ri1State :: Lens' ReservedInstances (Maybe Text)
ri1State = lens _ri1State (\s a -> s { _ri1State = a })

-- | Any tags assigned to the resource.
ri1Tags :: Lens' ReservedInstances [Tag]
ri1Tags = lens _ri1Tags (\s a -> s { _ri1Tags = a })

-- | The usage price of the Reserved Instance, per hour.
ri1UsagePrice :: Lens' ReservedInstances (Maybe Double)
ri1UsagePrice = lens _ri1UsagePrice (\s a -> s { _ri1UsagePrice = a })

instance FromXML ReservedInstances where
    parseXML c = ReservedInstances
        <$> c .:? "availabilityZone"
        <*> c .:? "currencyCode"
        <*> c .:? "duration"
        <*> c .:? "end"
        <*> c .:? "fixedPrice"
        <*> c .:? "instanceCount"
        <*> c .:? "instanceTenancy"
        <*> c .:? "instanceType"
        <*> c .:? "offeringType"
        <*> c .:? "productDescription"
        <*> c .: "recurringCharges"
        <*> c .:? "reservedInstancesId"
        <*> c .:? "start"
        <*> c .:? "state"
        <*> c .: "tagSet"
        <*> c .:? "usagePrice"

instance ToQuery ReservedInstances

data DatafeedSubscriptionState
    = DSSActive   -- ^ Active
    | DSSInactive -- ^ Inactive
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable DatafeedSubscriptionState

instance FromText DatafeedSubscriptionState where
    parser = match "Active"   DSSActive
         <|> match "Inactive" DSSInactive

instance ToText DatafeedSubscriptionState where
    toText = \case
        DSSActive   -> "Active"
        DSSInactive -> "Inactive"

instance FromXML DatafeedSubscriptionState where
    parseXML = fromXMLText "DatafeedSubscriptionState"

instance ToQuery DatafeedSubscriptionState

data ExportTaskState
    = ETSActive     -- ^ active
    | ETSCancelled  -- ^ cancelled
    | ETSCancelling -- ^ cancelling
    | ETSCompleted  -- ^ completed
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable ExportTaskState

instance FromText ExportTaskState where
    parser = match "active"     ETSActive
         <|> match "cancelled"  ETSCancelled
         <|> match "cancelling" ETSCancelling
         <|> match "completed"  ETSCompleted

instance ToText ExportTaskState where
    toText = \case
        ETSActive     -> "active"
        ETSCancelled  -> "cancelled"
        ETSCancelling -> "cancelling"
        ETSCompleted  -> "completed"

instance FromXML ExportTaskState where
    parseXML = fromXMLText "ExportTaskState"

instance ToQuery ExportTaskState

data ProductCodeValues
    = Devpay      -- ^ devpay
    | Marketplace -- ^ marketplace
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable ProductCodeValues

instance FromText ProductCodeValues where
    parser = match "devpay"      Devpay
         <|> match "marketplace" Marketplace

instance ToText ProductCodeValues where
    toText = \case
        Devpay      -> "devpay"
        Marketplace -> "marketplace"

instance FromXML ProductCodeValues where
    parseXML = fromXMLText "ProductCodeValues"

instance ToQuery ProductCodeValues

data VpnConnection = VpnConnection
    { _vcCustomerGatewayConfiguration :: Maybe Text
    , _vcCustomerGatewayId            :: Maybe Text
    , _vcOptions                      :: Maybe VpnConnectionOptions
    , _vcRoutes                       :: [VpnStaticRoute]
    , _vcState                        :: Maybe Text
    , _vcTags                         :: [Tag]
    , _vcType                         :: Maybe Text
    , _vcVgwTelemetry                 :: [VgwTelemetry]
    , _vcVpnConnectionId              :: Maybe Text
    , _vcVpnGatewayId                 :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'VpnConnection' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vcCustomerGatewayConfiguration' @::@ 'Maybe' 'Text'
--
-- * 'vcCustomerGatewayId' @::@ 'Maybe' 'Text'
--
-- * 'vcOptions' @::@ 'Maybe' 'VpnConnectionOptions'
--
-- * 'vcRoutes' @::@ ['VpnStaticRoute']
--
-- * 'vcState' @::@ 'Maybe' 'Text'
--
-- * 'vcTags' @::@ ['Tag']
--
-- * 'vcType' @::@ 'Maybe' 'Text'
--
-- * 'vcVgwTelemetry' @::@ ['VgwTelemetry']
--
-- * 'vcVpnConnectionId' @::@ 'Maybe' 'Text'
--
-- * 'vcVpnGatewayId' @::@ 'Maybe' 'Text'
--
vpnConnection :: VpnConnection
vpnConnection = VpnConnection
    { _vcVpnConnectionId              = Nothing
    , _vcState                        = Nothing
    , _vcCustomerGatewayConfiguration = Nothing
    , _vcType                         = Nothing
    , _vcCustomerGatewayId            = Nothing
    , _vcVpnGatewayId                 = Nothing
    , _vcTags                         = mempty
    , _vcVgwTelemetry                 = mempty
    , _vcOptions                      = Nothing
    , _vcRoutes                       = mempty
    }

-- | The configuration information for the VPN connection's customer gateway
-- (in the native XML format). This element is always present in the
-- CreateVpnConnection response; however, it's present in the
-- DescribeVpnConnections response only if the VPN connection is in the
-- pending or available state.
vcCustomerGatewayConfiguration :: Lens' VpnConnection (Maybe Text)
vcCustomerGatewayConfiguration =
    lens _vcCustomerGatewayConfiguration
        (\s a -> s { _vcCustomerGatewayConfiguration = a })

-- | The ID of the customer gateway at your end of the VPN connection.
vcCustomerGatewayId :: Lens' VpnConnection (Maybe Text)
vcCustomerGatewayId =
    lens _vcCustomerGatewayId (\s a -> s { _vcCustomerGatewayId = a })

-- | The VPN connection options.
vcOptions :: Lens' VpnConnection (Maybe VpnConnectionOptions)
vcOptions = lens _vcOptions (\s a -> s { _vcOptions = a })

-- | The static routes associated with the VPN connection.
vcRoutes :: Lens' VpnConnection [VpnStaticRoute]
vcRoutes = lens _vcRoutes (\s a -> s { _vcRoutes = a })

-- | The current state of the VPN connection.
vcState :: Lens' VpnConnection (Maybe Text)
vcState = lens _vcState (\s a -> s { _vcState = a })

-- | Any tags assigned to the VPN connection.
vcTags :: Lens' VpnConnection [Tag]
vcTags = lens _vcTags (\s a -> s { _vcTags = a })

-- | The type of VPN connection.
vcType :: Lens' VpnConnection (Maybe Text)
vcType = lens _vcType (\s a -> s { _vcType = a })

-- | Information about the VPN tunnel.
vcVgwTelemetry :: Lens' VpnConnection [VgwTelemetry]
vcVgwTelemetry = lens _vcVgwTelemetry (\s a -> s { _vcVgwTelemetry = a })

-- | The ID of the VPN connection.
vcVpnConnectionId :: Lens' VpnConnection (Maybe Text)
vcVpnConnectionId =
    lens _vcVpnConnectionId (\s a -> s { _vcVpnConnectionId = a })

-- | The ID of the virtual private gateway at the AWS side of the VPN
-- connection.
vcVpnGatewayId :: Lens' VpnConnection (Maybe Text)
vcVpnGatewayId = lens _vcVpnGatewayId (\s a -> s { _vcVpnGatewayId = a })

instance FromXML VpnConnection where
    parseXML c = VpnConnection
        <$> c .:? "customerGatewayConfiguration"
        <*> c .:? "customerGatewayId"
        <*> c .:? "options"
        <*> c .: "routes"
        <*> c .:? "state"
        <*> c .: "tagSet"
        <*> c .:? "type"
        <*> c .: "vgwTelemetry"
        <*> c .:? "vpnConnectionId"
        <*> c .:? "vpnGatewayId"

instance ToQuery VpnConnection

data InstanceState = InstanceState
    { _isCode :: Int
    , _isName :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'InstanceState' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'isCode' @::@ 'Int'
--
-- * 'isName' @::@ 'Text'
--
instanceState :: Int -- ^ 'isCode'
              -> Text -- ^ 'isName'
              -> InstanceState
instanceState p1 p2 = InstanceState
    { _isCode = p1
    , _isName = p2
    }

-- | The low byte represents the state. The high byte is an opaque internal
-- value and should be ignored. 0 : pending 16 : running 32 : shutting-down
-- 48 : terminated 64 : stopping 80 : stopped.
isCode :: Lens' InstanceState Int
isCode = lens _isCode (\s a -> s { _isCode = a })

-- | The current state of the instance.
isName :: Lens' InstanceState Text
isName = lens _isName (\s a -> s { _isName = a })

instance FromXML InstanceState where
    parseXML c = InstanceState
        <$> c .: "code"
        <*> c .: "name"

instance ToQuery InstanceState

data Placement = Placement
    { _pAvailabilityZone :: Maybe Text
    , _pGroupName        :: Maybe Text
    , _pTenancy          :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'Placement' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'pGroupName' @::@ 'Maybe' 'Text'
--
-- * 'pTenancy' @::@ 'Maybe' 'Text'
--
placement :: Placement
placement = Placement
    { _pAvailabilityZone = Nothing
    , _pGroupName        = Nothing
    , _pTenancy          = Nothing
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
pTenancy :: Lens' Placement (Maybe Text)
pTenancy = lens _pTenancy (\s a -> s { _pTenancy = a })

instance FromXML Placement where
    parseXML c = Placement
        <$> c .:? "availabilityZone"
        <*> c .:? "groupName"
        <*> c .:? "tenancy"

instance ToQuery Placement

data EventCode
    = InstanceReboot     -- ^ instance-reboot
    | InstanceRetirement -- ^ instance-retirement
    | InstanceStop       -- ^ instance-stop
    | SystemMaintenance  -- ^ system-maintenance
    | SystemReboot       -- ^ system-reboot
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable EventCode

instance FromText EventCode where
    parser = match "instance-reboot"     InstanceReboot
         <|> match "instance-retirement" InstanceRetirement
         <|> match "instance-stop"       InstanceStop
         <|> match "system-maintenance"  SystemMaintenance
         <|> match "system-reboot"       SystemReboot

instance ToText EventCode where
    toText = \case
        InstanceReboot     -> "instance-reboot"
        InstanceRetirement -> "instance-retirement"
        InstanceStop       -> "instance-stop"
        SystemMaintenance  -> "system-maintenance"
        SystemReboot       -> "system-reboot"

instance FromXML EventCode where
    parseXML = fromXMLText "EventCode"

instance ToQuery EventCode

data SpotInstanceType
    = OneTime    -- ^ one-time
    | Persistent -- ^ persistent
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable SpotInstanceType

instance FromText SpotInstanceType where
    parser = match "one-time"   OneTime
         <|> match "persistent" Persistent

instance ToText SpotInstanceType where
    toText = \case
        OneTime    -> "one-time"
        Persistent -> "persistent"

instance FromXML SpotInstanceType where
    parseXML = fromXMLText "SpotInstanceType"

instance ToQuery SpotInstanceType

data VpcPeeringConnection = VpcPeeringConnection
    { _vpc1AccepterVpcInfo        :: Maybe VpcPeeringConnectionVpcInfo
    , _vpc1ExpirationTime         :: Maybe RFC822
    , _vpc1RequesterVpcInfo       :: Maybe VpcPeeringConnectionVpcInfo
    , _vpc1Status                 :: Maybe VpcPeeringConnectionStateReason
    , _vpc1Tags                   :: [Tag]
    , _vpc1VpcPeeringConnectionId :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'VpcPeeringConnection' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vpc1AccepterVpcInfo' @::@ 'Maybe' 'VpcPeeringConnectionVpcInfo'
--
-- * 'vpc1ExpirationTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'vpc1RequesterVpcInfo' @::@ 'Maybe' 'VpcPeeringConnectionVpcInfo'
--
-- * 'vpc1Status' @::@ 'Maybe' 'VpcPeeringConnectionStateReason'
--
-- * 'vpc1Tags' @::@ ['Tag']
--
-- * 'vpc1VpcPeeringConnectionId' @::@ 'Maybe' 'Text'
--
vpcPeeringConnection :: VpcPeeringConnection
vpcPeeringConnection = VpcPeeringConnection
    { _vpc1AccepterVpcInfo        = Nothing
    , _vpc1ExpirationTime         = Nothing
    , _vpc1RequesterVpcInfo       = Nothing
    , _vpc1Status                 = Nothing
    , _vpc1Tags                   = mempty
    , _vpc1VpcPeeringConnectionId = Nothing
    }

-- | The information of the peer VPC.
vpc1AccepterVpcInfo :: Lens' VpcPeeringConnection (Maybe VpcPeeringConnectionVpcInfo)
vpc1AccepterVpcInfo =
    lens _vpc1AccepterVpcInfo (\s a -> s { _vpc1AccepterVpcInfo = a })

-- | The time that an unaccepted VPC peering connection will expire.
vpc1ExpirationTime :: Lens' VpcPeeringConnection (Maybe UTCTime)
vpc1ExpirationTime =
    lens _vpc1ExpirationTime (\s a -> s { _vpc1ExpirationTime = a })
        . mapping _Time

-- | The information of the requester VPC.
vpc1RequesterVpcInfo :: Lens' VpcPeeringConnection (Maybe VpcPeeringConnectionVpcInfo)
vpc1RequesterVpcInfo =
    lens _vpc1RequesterVpcInfo (\s a -> s { _vpc1RequesterVpcInfo = a })

-- | The status of the VPC peering connection.
vpc1Status :: Lens' VpcPeeringConnection (Maybe VpcPeeringConnectionStateReason)
vpc1Status = lens _vpc1Status (\s a -> s { _vpc1Status = a })

-- | Any tags assigned to the resource.
vpc1Tags :: Lens' VpcPeeringConnection [Tag]
vpc1Tags = lens _vpc1Tags (\s a -> s { _vpc1Tags = a })

-- | The ID of the VPC peering connection.
vpc1VpcPeeringConnectionId :: Lens' VpcPeeringConnection (Maybe Text)
vpc1VpcPeeringConnectionId =
    lens _vpc1VpcPeeringConnectionId
        (\s a -> s { _vpc1VpcPeeringConnectionId = a })

instance FromXML VpcPeeringConnection where
    parseXML c = VpcPeeringConnection
        <$> c .:? "accepterVpcInfo"
        <*> c .:? "expirationTime"
        <*> c .:? "requesterVpcInfo"
        <*> c .:? "status"
        <*> c .: "tagSet"
        <*> c .:? "vpcPeeringConnectionId"

instance ToQuery VpcPeeringConnection

data S3Storage = S3Storage
    { _ssAWSAccessKeyId        :: Maybe Text
    , _ssBucket                :: Maybe Text
    , _ssPrefix                :: Maybe Text
    , _ssUploadPolicy          :: Maybe Base64
    , _ssUploadPolicySignature :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'S3Storage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ssAWSAccessKeyId' @::@ 'Maybe' 'Text'
--
-- * 'ssBucket' @::@ 'Maybe' 'Text'
--
-- * 'ssPrefix' @::@ 'Maybe' 'Text'
--
-- * 'ssUploadPolicy' @::@ 'Maybe' 'Base64'
--
-- * 'ssUploadPolicySignature' @::@ 'Maybe' 'Text'
--
s3Storage :: S3Storage
s3Storage = S3Storage
    { _ssBucket                = Nothing
    , _ssPrefix                = Nothing
    , _ssAWSAccessKeyId        = Nothing
    , _ssUploadPolicy          = Nothing
    , _ssUploadPolicySignature = Nothing
    }

-- | The access key ID of the owner of the bucket. Before you specify a value
-- for your access key ID, review and follow the guidance in Best Practices
-- for Managing AWS Access Keys.
ssAWSAccessKeyId :: Lens' S3Storage (Maybe Text)
ssAWSAccessKeyId = lens _ssAWSAccessKeyId (\s a -> s { _ssAWSAccessKeyId = a })

-- | The bucket in which to store the AMI. You can specify a bucket that you
-- already own or a new bucket that Amazon EC2 creates on your behalf. If
-- you specify a bucket that belongs to someone else, Amazon EC2 returns an
-- error.
ssBucket :: Lens' S3Storage (Maybe Text)
ssBucket = lens _ssBucket (\s a -> s { _ssBucket = a })

-- | The beginning of the file name of the AMI.
ssPrefix :: Lens' S3Storage (Maybe Text)
ssPrefix = lens _ssPrefix (\s a -> s { _ssPrefix = a })

-- | A Base64-encoded Amazon S3 upload policy that gives Amazon EC2 permission
-- to upload items into Amazon S3 on your behalf.
ssUploadPolicy :: Lens' S3Storage (Maybe Base64)
ssUploadPolicy = lens _ssUploadPolicy (\s a -> s { _ssUploadPolicy = a })

-- | The signature of the Base64 encoded JSON document.
ssUploadPolicySignature :: Lens' S3Storage (Maybe Text)
ssUploadPolicySignature =
    lens _ssUploadPolicySignature (\s a -> s { _ssUploadPolicySignature = a })

instance FromXML S3Storage where
    parseXML c = S3Storage
        <$> c .:? "AWSAccessKeyId"
        <*> c .:? "bucket"
        <*> c .:? "prefix"
        <*> c .:? "uploadPolicy"
        <*> c .:? "uploadPolicySignature"

instance ToQuery S3Storage

data VgwTelemetry = VgwTelemetry
    { _vtAcceptedRouteCount :: Maybe Int
    , _vtLastStatusChange   :: Maybe RFC822
    , _vtOutsideIpAddress   :: Maybe Text
    , _vtStatus             :: Maybe Text
    , _vtStatusMessage      :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'VgwTelemetry' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vtAcceptedRouteCount' @::@ 'Maybe' 'Int'
--
-- * 'vtLastStatusChange' @::@ 'Maybe' 'UTCTime'
--
-- * 'vtOutsideIpAddress' @::@ 'Maybe' 'Text'
--
-- * 'vtStatus' @::@ 'Maybe' 'Text'
--
-- * 'vtStatusMessage' @::@ 'Maybe' 'Text'
--
vgwTelemetry :: VgwTelemetry
vgwTelemetry = VgwTelemetry
    { _vtOutsideIpAddress   = Nothing
    , _vtStatus             = Nothing
    , _vtLastStatusChange   = Nothing
    , _vtStatusMessage      = Nothing
    , _vtAcceptedRouteCount = Nothing
    }

-- | The number of accepted routes.
vtAcceptedRouteCount :: Lens' VgwTelemetry (Maybe Int)
vtAcceptedRouteCount =
    lens _vtAcceptedRouteCount (\s a -> s { _vtAcceptedRouteCount = a })

-- | The date and time of the last change in status.
vtLastStatusChange :: Lens' VgwTelemetry (Maybe UTCTime)
vtLastStatusChange =
    lens _vtLastStatusChange (\s a -> s { _vtLastStatusChange = a })
        . mapping _Time

-- | The Internet-routable IP address of the virtual private gateway's outside
-- interface.
vtOutsideIpAddress :: Lens' VgwTelemetry (Maybe Text)
vtOutsideIpAddress =
    lens _vtOutsideIpAddress (\s a -> s { _vtOutsideIpAddress = a })

-- | The status of the VPN tunnel.
vtStatus :: Lens' VgwTelemetry (Maybe Text)
vtStatus = lens _vtStatus (\s a -> s { _vtStatus = a })

-- | If an error occurs, a description of the error.
vtStatusMessage :: Lens' VgwTelemetry (Maybe Text)
vtStatusMessage = lens _vtStatusMessage (\s a -> s { _vtStatusMessage = a })

instance FromXML VgwTelemetry where
    parseXML c = VgwTelemetry
        <$> c .:? "acceptedRouteCount"
        <*> c .:? "lastStatusChange"
        <*> c .:? "outsideIpAddress"
        <*> c .:? "status"
        <*> c .:? "statusMessage"

instance ToQuery VgwTelemetry

data VpnStaticRoute = VpnStaticRoute
    { _vsrDestinationCidrBlock :: Maybe Text
    , _vsrSource               :: Maybe Text
    , _vsrState                :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'VpnStaticRoute' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vsrDestinationCidrBlock' @::@ 'Maybe' 'Text'
--
-- * 'vsrSource' @::@ 'Maybe' 'Text'
--
-- * 'vsrState' @::@ 'Maybe' 'Text'
--
vpnStaticRoute :: VpnStaticRoute
vpnStaticRoute = VpnStaticRoute
    { _vsrDestinationCidrBlock = Nothing
    , _vsrSource               = Nothing
    , _vsrState                = Nothing
    }

-- | The CIDR block associated with the local subnet of the customer data
-- center.
vsrDestinationCidrBlock :: Lens' VpnStaticRoute (Maybe Text)
vsrDestinationCidrBlock =
    lens _vsrDestinationCidrBlock (\s a -> s { _vsrDestinationCidrBlock = a })

-- | Indicates how the routes were provided.
vsrSource :: Lens' VpnStaticRoute (Maybe Text)
vsrSource = lens _vsrSource (\s a -> s { _vsrSource = a })

-- | The current state of the static route.
vsrState :: Lens' VpnStaticRoute (Maybe Text)
vsrState = lens _vsrState (\s a -> s { _vsrState = a })

instance FromXML VpnStaticRoute where
    parseXML c = VpnStaticRoute
        <$> c .:? "destinationCidrBlock"
        <*> c .:? "source"
        <*> c .:? "state"

instance ToQuery VpnStaticRoute

data InstanceStateName
    = ISNPending      -- ^ pending
    | ISNRunning      -- ^ running
    | ISNShuttingDown -- ^ shutting-down
    | ISNStopped      -- ^ stopped
    | ISNStopping     -- ^ stopping
    | ISNTerminated   -- ^ terminated
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable InstanceStateName

instance FromText InstanceStateName where
    parser = match "pending"       ISNPending
         <|> match "running"       ISNRunning
         <|> match "shutting-down" ISNShuttingDown
         <|> match "stopped"       ISNStopped
         <|> match "stopping"      ISNStopping
         <|> match "terminated"    ISNTerminated

instance ToText InstanceStateName where
    toText = \case
        ISNPending      -> "pending"
        ISNRunning      -> "running"
        ISNShuttingDown -> "shutting-down"
        ISNStopped      -> "stopped"
        ISNStopping     -> "stopping"
        ISNTerminated   -> "terminated"

instance FromXML InstanceStateName where
    parseXML = fromXMLText "InstanceStateName"

instance ToQuery InstanceStateName

data Instance = Instance
    { _i1AmiLaunchIndex        :: Maybe Int
    , _i1Architecture          :: Maybe Text
    , _i1BlockDeviceMappings   :: [InstanceBlockDeviceMapping]
    , _i1ClientToken           :: Maybe Text
    , _i1EbsOptimized          :: Maybe Bool
    , _i1Hypervisor            :: Maybe Text
    , _i1IamInstanceProfile    :: Maybe IamInstanceProfile
    , _i1ImageId               :: Maybe Text
    , _i1InstanceId            :: Maybe Text
    , _i1InstanceLifecycle     :: Maybe Text
    , _i1InstanceType          :: Maybe Text
    , _i1KernelId              :: Maybe Text
    , _i1KeyName               :: Maybe Text
    , _i1LaunchTime            :: Maybe RFC822
    , _i1Monitoring            :: Maybe Monitoring
    , _i1NetworkInterfaces     :: [InstanceNetworkInterface]
    , _i1Placement             :: Maybe Placement
    , _i1Platform              :: Maybe Text
    , _i1PrivateDnsName        :: Maybe Text
    , _i1PrivateIpAddress      :: Maybe Text
    , _i1ProductCodes          :: [ProductCode]
    , _i1PublicDnsName         :: Maybe Text
    , _i1PublicIpAddress       :: Maybe Text
    , _i1RamdiskId             :: Maybe Text
    , _i1RootDeviceName        :: Maybe Text
    , _i1RootDeviceType        :: Maybe Text
    , _i1SecurityGroups        :: [GroupIdentifier]
    , _i1SourceDestCheck       :: Maybe Bool
    , _i1SpotInstanceRequestId :: Maybe Text
    , _i1SriovNetSupport       :: Maybe Text
    , _i1State                 :: Maybe InstanceState
    , _i1StateReason           :: Maybe StateReason
    , _i1StateTransitionReason :: Maybe Text
    , _i1SubnetId              :: Maybe Text
    , _i1Tags                  :: [Tag]
    , _i1VirtualizationType    :: Maybe Text
    , _i1VpcId                 :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'Instance' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'i1AmiLaunchIndex' @::@ 'Maybe' 'Int'
--
-- * 'i1Architecture' @::@ 'Maybe' 'Text'
--
-- * 'i1BlockDeviceMappings' @::@ ['InstanceBlockDeviceMapping']
--
-- * 'i1ClientToken' @::@ 'Maybe' 'Text'
--
-- * 'i1EbsOptimized' @::@ 'Maybe' 'Bool'
--
-- * 'i1Hypervisor' @::@ 'Maybe' 'Text'
--
-- * 'i1IamInstanceProfile' @::@ 'Maybe' 'IamInstanceProfile'
--
-- * 'i1ImageId' @::@ 'Maybe' 'Text'
--
-- * 'i1InstanceId' @::@ 'Maybe' 'Text'
--
-- * 'i1InstanceLifecycle' @::@ 'Maybe' 'Text'
--
-- * 'i1InstanceType' @::@ 'Maybe' 'Text'
--
-- * 'i1KernelId' @::@ 'Maybe' 'Text'
--
-- * 'i1KeyName' @::@ 'Maybe' 'Text'
--
-- * 'i1LaunchTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'i1Monitoring' @::@ 'Maybe' 'Monitoring'
--
-- * 'i1NetworkInterfaces' @::@ ['InstanceNetworkInterface']
--
-- * 'i1Placement' @::@ 'Maybe' 'Placement'
--
-- * 'i1Platform' @::@ 'Maybe' 'Text'
--
-- * 'i1PrivateDnsName' @::@ 'Maybe' 'Text'
--
-- * 'i1PrivateIpAddress' @::@ 'Maybe' 'Text'
--
-- * 'i1ProductCodes' @::@ ['ProductCode']
--
-- * 'i1PublicDnsName' @::@ 'Maybe' 'Text'
--
-- * 'i1PublicIpAddress' @::@ 'Maybe' 'Text'
--
-- * 'i1RamdiskId' @::@ 'Maybe' 'Text'
--
-- * 'i1RootDeviceName' @::@ 'Maybe' 'Text'
--
-- * 'i1RootDeviceType' @::@ 'Maybe' 'Text'
--
-- * 'i1SecurityGroups' @::@ ['GroupIdentifier']
--
-- * 'i1SourceDestCheck' @::@ 'Maybe' 'Bool'
--
-- * 'i1SpotInstanceRequestId' @::@ 'Maybe' 'Text'
--
-- * 'i1SriovNetSupport' @::@ 'Maybe' 'Text'
--
-- * 'i1State' @::@ 'Maybe' 'InstanceState'
--
-- * 'i1StateReason' @::@ 'Maybe' 'StateReason'
--
-- * 'i1StateTransitionReason' @::@ 'Maybe' 'Text'
--
-- * 'i1SubnetId' @::@ 'Maybe' 'Text'
--
-- * 'i1Tags' @::@ ['Tag']
--
-- * 'i1VirtualizationType' @::@ 'Maybe' 'Text'
--
-- * 'i1VpcId' @::@ 'Maybe' 'Text'
--
instance' :: Instance
instance' = Instance
    { _i1InstanceId            = Nothing
    , _i1ImageId               = Nothing
    , _i1State                 = Nothing
    , _i1PrivateDnsName        = Nothing
    , _i1PublicDnsName         = Nothing
    , _i1StateTransitionReason = Nothing
    , _i1KeyName               = Nothing
    , _i1AmiLaunchIndex        = Nothing
    , _i1ProductCodes          = mempty
    , _i1InstanceType          = Nothing
    , _i1LaunchTime            = Nothing
    , _i1Placement             = Nothing
    , _i1KernelId              = Nothing
    , _i1RamdiskId             = Nothing
    , _i1Platform              = Nothing
    , _i1Monitoring            = Nothing
    , _i1SubnetId              = Nothing
    , _i1VpcId                 = Nothing
    , _i1PrivateIpAddress      = Nothing
    , _i1PublicIpAddress       = Nothing
    , _i1StateReason           = Nothing
    , _i1Architecture          = Nothing
    , _i1RootDeviceType        = Nothing
    , _i1RootDeviceName        = Nothing
    , _i1BlockDeviceMappings   = mempty
    , _i1VirtualizationType    = Nothing
    , _i1InstanceLifecycle     = Nothing
    , _i1SpotInstanceRequestId = Nothing
    , _i1ClientToken           = Nothing
    , _i1Tags                  = mempty
    , _i1SecurityGroups        = mempty
    , _i1SourceDestCheck       = Nothing
    , _i1Hypervisor            = Nothing
    , _i1NetworkInterfaces     = mempty
    , _i1IamInstanceProfile    = Nothing
    , _i1EbsOptimized          = Nothing
    , _i1SriovNetSupport       = Nothing
    }

-- | The AMI launch index, which can be used to find this instance in the
-- launch group.
i1AmiLaunchIndex :: Lens' Instance (Maybe Int)
i1AmiLaunchIndex = lens _i1AmiLaunchIndex (\s a -> s { _i1AmiLaunchIndex = a })

-- | The architecture of the image.
i1Architecture :: Lens' Instance (Maybe Text)
i1Architecture = lens _i1Architecture (\s a -> s { _i1Architecture = a })

-- | Any block device mapping entries for the instance.
i1BlockDeviceMappings :: Lens' Instance [InstanceBlockDeviceMapping]
i1BlockDeviceMappings =
    lens _i1BlockDeviceMappings (\s a -> s { _i1BlockDeviceMappings = a })

-- | The idempotency token you provided when you launched the instance.
i1ClientToken :: Lens' Instance (Maybe Text)
i1ClientToken = lens _i1ClientToken (\s a -> s { _i1ClientToken = a })

-- | Indicates whether the instance is optimized for EBS I/O. This
-- optimization provides dedicated throughput to Amazon EBS and an optimized
-- configuration stack to provide optimal I/O performance. This optimization
-- isn't available with all instance types. Additional usage charges apply
-- when using an EBS Optimized instance.
i1EbsOptimized :: Lens' Instance (Maybe Bool)
i1EbsOptimized = lens _i1EbsOptimized (\s a -> s { _i1EbsOptimized = a })

-- | The hypervisor type of the instance.
i1Hypervisor :: Lens' Instance (Maybe Text)
i1Hypervisor = lens _i1Hypervisor (\s a -> s { _i1Hypervisor = a })

-- | The IAM instance profile associated with the instance.
i1IamInstanceProfile :: Lens' Instance (Maybe IamInstanceProfile)
i1IamInstanceProfile =
    lens _i1IamInstanceProfile (\s a -> s { _i1IamInstanceProfile = a })

-- | The ID of the AMI used to launch the instance.
i1ImageId :: Lens' Instance (Maybe Text)
i1ImageId = lens _i1ImageId (\s a -> s { _i1ImageId = a })

-- | The ID of the instance.
i1InstanceId :: Lens' Instance (Maybe Text)
i1InstanceId = lens _i1InstanceId (\s a -> s { _i1InstanceId = a })

-- | Indicates whether this is a Spot Instance.
i1InstanceLifecycle :: Lens' Instance (Maybe Text)
i1InstanceLifecycle =
    lens _i1InstanceLifecycle (\s a -> s { _i1InstanceLifecycle = a })

-- | The instance type.
i1InstanceType :: Lens' Instance (Maybe Text)
i1InstanceType = lens _i1InstanceType (\s a -> s { _i1InstanceType = a })

-- | The kernel associated with this instance.
i1KernelId :: Lens' Instance (Maybe Text)
i1KernelId = lens _i1KernelId (\s a -> s { _i1KernelId = a })

-- | The name of the key pair, if this instance was launched with an
-- associated key pair.
i1KeyName :: Lens' Instance (Maybe Text)
i1KeyName = lens _i1KeyName (\s a -> s { _i1KeyName = a })

-- | The time the instance was launched.
i1LaunchTime :: Lens' Instance (Maybe UTCTime)
i1LaunchTime = lens _i1LaunchTime (\s a -> s { _i1LaunchTime = a })
    . mapping _Time

-- | The monitoring information for the instance.
i1Monitoring :: Lens' Instance (Maybe Monitoring)
i1Monitoring = lens _i1Monitoring (\s a -> s { _i1Monitoring = a })

-- | [EC2-VPC] One or more network interfaces for the instance.
i1NetworkInterfaces :: Lens' Instance [InstanceNetworkInterface]
i1NetworkInterfaces =
    lens _i1NetworkInterfaces (\s a -> s { _i1NetworkInterfaces = a })

-- | The location where the instance launched.
i1Placement :: Lens' Instance (Maybe Placement)
i1Placement = lens _i1Placement (\s a -> s { _i1Placement = a })

-- | The value is Windows for Windows instances; otherwise blank.
i1Platform :: Lens' Instance (Maybe Text)
i1Platform = lens _i1Platform (\s a -> s { _i1Platform = a })

-- | The private DNS name assigned to the instance. This DNS name can only be
-- used inside the Amazon EC2 network. This name is not available until the
-- instance enters the running state.
i1PrivateDnsName :: Lens' Instance (Maybe Text)
i1PrivateDnsName = lens _i1PrivateDnsName (\s a -> s { _i1PrivateDnsName = a })

-- | The private IP address assigned to the instance.
i1PrivateIpAddress :: Lens' Instance (Maybe Text)
i1PrivateIpAddress =
    lens _i1PrivateIpAddress (\s a -> s { _i1PrivateIpAddress = a })

-- | The product codes attached to this instance.
i1ProductCodes :: Lens' Instance [ProductCode]
i1ProductCodes = lens _i1ProductCodes (\s a -> s { _i1ProductCodes = a })

-- | The public DNS name assigned to the instance. This name is not available
-- until the instance enters the running state.
i1PublicDnsName :: Lens' Instance (Maybe Text)
i1PublicDnsName = lens _i1PublicDnsName (\s a -> s { _i1PublicDnsName = a })

-- | The public IP address assigned to the instance.
i1PublicIpAddress :: Lens' Instance (Maybe Text)
i1PublicIpAddress =
    lens _i1PublicIpAddress (\s a -> s { _i1PublicIpAddress = a })

-- | The RAM disk associated with this instance.
i1RamdiskId :: Lens' Instance (Maybe Text)
i1RamdiskId = lens _i1RamdiskId (\s a -> s { _i1RamdiskId = a })

-- | The root device name (for example, /dev/sda1).
i1RootDeviceName :: Lens' Instance (Maybe Text)
i1RootDeviceName = lens _i1RootDeviceName (\s a -> s { _i1RootDeviceName = a })

-- | The root device type used by the AMI. The AMI can use an Amazon EBS
-- volume or an instance store volume.
i1RootDeviceType :: Lens' Instance (Maybe Text)
i1RootDeviceType = lens _i1RootDeviceType (\s a -> s { _i1RootDeviceType = a })

-- | One or more security groups for the instance.
i1SecurityGroups :: Lens' Instance [GroupIdentifier]
i1SecurityGroups = lens _i1SecurityGroups (\s a -> s { _i1SecurityGroups = a })

-- | Specifies whether to enable an instance launched in a VPC to perform NAT.
-- This controls whether source/destination checking is enabled on the
-- instance. A value of true means checking is enabled, and false means
-- checking is disabled. The value must be false for the instance to perform
-- NAT. For more information, see NAT Instances in the Amazon Virtual
-- Private Cloud User Guide.
i1SourceDestCheck :: Lens' Instance (Maybe Bool)
i1SourceDestCheck =
    lens _i1SourceDestCheck (\s a -> s { _i1SourceDestCheck = a })

-- | The ID of the Spot Instance request.
i1SpotInstanceRequestId :: Lens' Instance (Maybe Text)
i1SpotInstanceRequestId =
    lens _i1SpotInstanceRequestId (\s a -> s { _i1SpotInstanceRequestId = a })

-- | Specifies whether enhanced networking is enabled.
i1SriovNetSupport :: Lens' Instance (Maybe Text)
i1SriovNetSupport =
    lens _i1SriovNetSupport (\s a -> s { _i1SriovNetSupport = a })

-- | The current state of the instance.
i1State :: Lens' Instance (Maybe InstanceState)
i1State = lens _i1State (\s a -> s { _i1State = a })

-- | The reason for the most recent state transition.
i1StateReason :: Lens' Instance (Maybe StateReason)
i1StateReason = lens _i1StateReason (\s a -> s { _i1StateReason = a })

-- | The reason for the most recent state transition. This might be an empty
-- string.
i1StateTransitionReason :: Lens' Instance (Maybe Text)
i1StateTransitionReason =
    lens _i1StateTransitionReason (\s a -> s { _i1StateTransitionReason = a })

-- | The ID of the subnet in which the instance is running.
i1SubnetId :: Lens' Instance (Maybe Text)
i1SubnetId = lens _i1SubnetId (\s a -> s { _i1SubnetId = a })

-- | Any tags assigned to the instance.
i1Tags :: Lens' Instance [Tag]
i1Tags = lens _i1Tags (\s a -> s { _i1Tags = a })

-- | The virtualization type of the instance.
i1VirtualizationType :: Lens' Instance (Maybe Text)
i1VirtualizationType =
    lens _i1VirtualizationType (\s a -> s { _i1VirtualizationType = a })

-- | The ID of the VPC in which the instance is running.
i1VpcId :: Lens' Instance (Maybe Text)
i1VpcId = lens _i1VpcId (\s a -> s { _i1VpcId = a })

instance FromXML Instance where
    parseXML c = Instance
        <$> c .:? "amiLaunchIndex"
        <*> c .:? "architecture"
        <*> c .: "blockDeviceMapping"
        <*> c .:? "clientToken"
        <*> c .:? "ebsOptimized"
        <*> c .:? "hypervisor"
        <*> c .:? "iamInstanceProfile"
        <*> c .:? "imageId"
        <*> c .:? "instanceId"
        <*> c .:? "instanceLifecycle"
        <*> c .:? "instanceType"
        <*> c .:? "kernelId"
        <*> c .:? "keyName"
        <*> c .:? "launchTime"
        <*> c .:? "monitoring"
        <*> c .: "networkInterfaceSet"
        <*> c .:? "placement"
        <*> c .:? "platform"
        <*> c .:? "privateDnsName"
        <*> c .:? "privateIpAddress"
        <*> c .: "productCodes"
        <*> c .:? "dnsName"
        <*> c .:? "ipAddress"
        <*> c .:? "ramdiskId"
        <*> c .:? "rootDeviceName"
        <*> c .:? "rootDeviceType"
        <*> c .: "groupSet"
        <*> c .:? "sourceDestCheck"
        <*> c .:? "spotInstanceRequestId"
        <*> c .:? "sriovNetSupport"
        <*> c .:? "instanceState"
        <*> c .:? "stateReason"
        <*> c .:? "reason"
        <*> c .:? "subnetId"
        <*> c .: "tagSet"
        <*> c .:? "virtualizationType"
        <*> c .:? "vpcId"

instance ToQuery Instance

data ExportTask = ExportTask
    { _etDescription           :: Maybe Text
    , _etExportTaskId          :: Maybe Text
    , _etExportToS3Task        :: Maybe ExportToS3Task
    , _etInstanceExportDetails :: Maybe InstanceExportDetails
    , _etState                 :: Maybe Text
    , _etStatusMessage         :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'ExportTask' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'etDescription' @::@ 'Maybe' 'Text'
--
-- * 'etExportTaskId' @::@ 'Maybe' 'Text'
--
-- * 'etExportToS3Task' @::@ 'Maybe' 'ExportToS3Task'
--
-- * 'etInstanceExportDetails' @::@ 'Maybe' 'InstanceExportDetails'
--
-- * 'etState' @::@ 'Maybe' 'Text'
--
-- * 'etStatusMessage' @::@ 'Maybe' 'Text'
--
exportTask :: ExportTask
exportTask = ExportTask
    { _etExportTaskId          = Nothing
    , _etDescription           = Nothing
    , _etState                 = Nothing
    , _etStatusMessage         = Nothing
    , _etInstanceExportDetails = Nothing
    , _etExportToS3Task        = Nothing
    }

-- | A description of the resource being exported.
etDescription :: Lens' ExportTask (Maybe Text)
etDescription = lens _etDescription (\s a -> s { _etDescription = a })

-- | The ID of the export task.
etExportTaskId :: Lens' ExportTask (Maybe Text)
etExportTaskId = lens _etExportTaskId (\s a -> s { _etExportTaskId = a })

etExportToS3Task :: Lens' ExportTask (Maybe ExportToS3Task)
etExportToS3Task = lens _etExportToS3Task (\s a -> s { _etExportToS3Task = a })

-- | The instance being exported.
etInstanceExportDetails :: Lens' ExportTask (Maybe InstanceExportDetails)
etInstanceExportDetails =
    lens _etInstanceExportDetails (\s a -> s { _etInstanceExportDetails = a })

-- | The state of the conversion task.
etState :: Lens' ExportTask (Maybe Text)
etState = lens _etState (\s a -> s { _etState = a })

-- | The status message related to the export task.
etStatusMessage :: Lens' ExportTask (Maybe Text)
etStatusMessage = lens _etStatusMessage (\s a -> s { _etStatusMessage = a })

instance FromXML ExportTask where
    parseXML c = ExportTask
        <$> c .:? "description"
        <*> c .:? "exportTaskId"
        <*> c .:? "exportToS3"
        <*> c .:? "instanceExport"
        <*> c .:? "state"
        <*> c .:? "statusMessage"

instance ToQuery ExportTask

data ResetImageAttributeName
    = RIANLaunchPermission -- ^ launchPermission
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable ResetImageAttributeName

instance FromText ResetImageAttributeName where
    parser = match "launchPermission" RIANLaunchPermission

instance ToText ResetImageAttributeName where
    toText RIANLaunchPermission = "launchPermission"

instance FromXML ResetImageAttributeName where
    parseXML = fromXMLText "ResetImageAttributeName"

instance ToQuery ResetImageAttributeName

data RequestSpotLaunchSpecification = RequestSpotLaunchSpecification
    { _rslsAddressingType      :: Maybe Text
    , _rslsBlockDeviceMappings :: [BlockDeviceMapping]
    , _rslsEbsOptimized        :: Maybe Bool
    , _rslsIamInstanceProfile  :: Maybe IamInstanceProfileSpecification
    , _rslsImageId             :: Maybe Text
    , _rslsInstanceType        :: Maybe Text
    , _rslsKernelId            :: Maybe Text
    , _rslsKeyName             :: Maybe Text
    , _rslsMonitoring          :: Maybe RunInstancesMonitoringEnabled
    , _rslsNetworkInterfaces   :: [InstanceNetworkInterfaceSpecification]
    , _rslsPlacement           :: Maybe SpotPlacement
    , _rslsRamdiskId           :: Maybe Text
    , _rslsSecurityGroupIds    :: [Text]
    , _rslsSecurityGroups      :: [Text]
    , _rslsSubnetId            :: Maybe Text
    , _rslsUserData            :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'RequestSpotLaunchSpecification' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rslsAddressingType' @::@ 'Maybe' 'Text'
--
-- * 'rslsBlockDeviceMappings' @::@ ['BlockDeviceMapping']
--
-- * 'rslsEbsOptimized' @::@ 'Maybe' 'Bool'
--
-- * 'rslsIamInstanceProfile' @::@ 'Maybe' 'IamInstanceProfileSpecification'
--
-- * 'rslsImageId' @::@ 'Maybe' 'Text'
--
-- * 'rslsInstanceType' @::@ 'Maybe' 'Text'
--
-- * 'rslsKernelId' @::@ 'Maybe' 'Text'
--
-- * 'rslsKeyName' @::@ 'Maybe' 'Text'
--
-- * 'rslsMonitoring' @::@ 'Maybe' 'RunInstancesMonitoringEnabled'
--
-- * 'rslsNetworkInterfaces' @::@ ['InstanceNetworkInterfaceSpecification']
--
-- * 'rslsPlacement' @::@ 'Maybe' 'SpotPlacement'
--
-- * 'rslsRamdiskId' @::@ 'Maybe' 'Text'
--
-- * 'rslsSecurityGroupIds' @::@ ['Text']
--
-- * 'rslsSecurityGroups' @::@ ['Text']
--
-- * 'rslsSubnetId' @::@ 'Maybe' 'Text'
--
-- * 'rslsUserData' @::@ 'Maybe' 'Text'
--
requestSpotLaunchSpecification :: RequestSpotLaunchSpecification
requestSpotLaunchSpecification = RequestSpotLaunchSpecification
    { _rslsImageId             = Nothing
    , _rslsKeyName             = Nothing
    , _rslsSecurityGroups      = mempty
    , _rslsUserData            = Nothing
    , _rslsAddressingType      = Nothing
    , _rslsInstanceType        = Nothing
    , _rslsPlacement           = Nothing
    , _rslsKernelId            = Nothing
    , _rslsRamdiskId           = Nothing
    , _rslsBlockDeviceMappings = mempty
    , _rslsSubnetId            = Nothing
    , _rslsNetworkInterfaces   = mempty
    , _rslsIamInstanceProfile  = Nothing
    , _rslsEbsOptimized        = Nothing
    , _rslsMonitoring          = Nothing
    , _rslsSecurityGroupIds    = mempty
    }

-- | Deprecated.
rslsAddressingType :: Lens' RequestSpotLaunchSpecification (Maybe Text)
rslsAddressingType =
    lens _rslsAddressingType (\s a -> s { _rslsAddressingType = a })

-- | One or more block device mapping entries.
rslsBlockDeviceMappings :: Lens' RequestSpotLaunchSpecification [BlockDeviceMapping]
rslsBlockDeviceMappings =
    lens _rslsBlockDeviceMappings (\s a -> s { _rslsBlockDeviceMappings = a })

-- | Indicates whether the instance is optimized for EBS I/O. This
-- optimization provides dedicated throughput to Amazon EBS and an optimized
-- configuration stack to provide optimal EBS I/O performance. This
-- optimization isn't available with all instance types. Additional usage
-- charges apply when using an EBS Optimized instance. Default: false.
rslsEbsOptimized :: Lens' RequestSpotLaunchSpecification (Maybe Bool)
rslsEbsOptimized = lens _rslsEbsOptimized (\s a -> s { _rslsEbsOptimized = a })

-- | The IAM instance profile.
rslsIamInstanceProfile :: Lens' RequestSpotLaunchSpecification (Maybe IamInstanceProfileSpecification)
rslsIamInstanceProfile =
    lens _rslsIamInstanceProfile (\s a -> s { _rslsIamInstanceProfile = a })

-- | The ID of the AMI.
rslsImageId :: Lens' RequestSpotLaunchSpecification (Maybe Text)
rslsImageId = lens _rslsImageId (\s a -> s { _rslsImageId = a })

-- | The instance type. Default: m1.small.
rslsInstanceType :: Lens' RequestSpotLaunchSpecification (Maybe Text)
rslsInstanceType = lens _rslsInstanceType (\s a -> s { _rslsInstanceType = a })

-- | The ID of the kernel.
rslsKernelId :: Lens' RequestSpotLaunchSpecification (Maybe Text)
rslsKernelId = lens _rslsKernelId (\s a -> s { _rslsKernelId = a })

-- | The name of the key pair.
rslsKeyName :: Lens' RequestSpotLaunchSpecification (Maybe Text)
rslsKeyName = lens _rslsKeyName (\s a -> s { _rslsKeyName = a })

rslsMonitoring :: Lens' RequestSpotLaunchSpecification (Maybe RunInstancesMonitoringEnabled)
rslsMonitoring = lens _rslsMonitoring (\s a -> s { _rslsMonitoring = a })

-- | One or more network interfaces.
rslsNetworkInterfaces :: Lens' RequestSpotLaunchSpecification [InstanceNetworkInterfaceSpecification]
rslsNetworkInterfaces =
    lens _rslsNetworkInterfaces (\s a -> s { _rslsNetworkInterfaces = a })

-- | The placement information for the instance.
rslsPlacement :: Lens' RequestSpotLaunchSpecification (Maybe SpotPlacement)
rslsPlacement = lens _rslsPlacement (\s a -> s { _rslsPlacement = a })

-- | The ID of the RAM disk.
rslsRamdiskId :: Lens' RequestSpotLaunchSpecification (Maybe Text)
rslsRamdiskId = lens _rslsRamdiskId (\s a -> s { _rslsRamdiskId = a })

rslsSecurityGroupIds :: Lens' RequestSpotLaunchSpecification [Text]
rslsSecurityGroupIds =
    lens _rslsSecurityGroupIds (\s a -> s { _rslsSecurityGroupIds = a })

rslsSecurityGroups :: Lens' RequestSpotLaunchSpecification [Text]
rslsSecurityGroups =
    lens _rslsSecurityGroups (\s a -> s { _rslsSecurityGroups = a })

-- | The ID of the subnet in which to launch the Spot Instance.
rslsSubnetId :: Lens' RequestSpotLaunchSpecification (Maybe Text)
rslsSubnetId = lens _rslsSubnetId (\s a -> s { _rslsSubnetId = a })

-- | The Base64-encoded MIME user data to make available to the instances.
rslsUserData :: Lens' RequestSpotLaunchSpecification (Maybe Text)
rslsUserData = lens _rslsUserData (\s a -> s { _rslsUserData = a })

instance FromXML RequestSpotLaunchSpecification where
    parseXML c = RequestSpotLaunchSpecification
        <$> c .:? "addressingType"
        <*> c .: "blockDeviceMapping"
        <*> c .:? "ebsOptimized"
        <*> c .:? "iamInstanceProfile"
        <*> c .:? "imageId"
        <*> c .:? "instanceType"
        <*> c .:? "kernelId"
        <*> c .:? "keyName"
        <*> c .:? "monitoring"
        <*> c .: "NetworkInterface"
        <*> c .:? "placement"
        <*> c .:? "ramdiskId"
        <*> c .: "SecurityGroupId"
        <*> c .: "SecurityGroup"
        <*> c .:? "subnetId"
        <*> c .:? "userData"

instance ToQuery RequestSpotLaunchSpecification

newtype VolumeDetail = VolumeDetail
    { _vdSize :: Integer
    } deriving (Eq, Ord, Show, Generic, Enum, Num, Integral, Real)

-- | 'VolumeDetail' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vdSize' @::@ 'Integer'
--
volumeDetail :: Integer -- ^ 'vdSize'
             -> VolumeDetail
volumeDetail p1 = VolumeDetail
    { _vdSize = p1
    }

-- | The size of the volume, in GiB.
vdSize :: Lens' VolumeDetail Integer
vdSize = lens _vdSize (\s a -> s { _vdSize = a })

instance FromXML VolumeDetail where
    parseXML c = VolumeDetail
        <$> c .: "size"

instance ToQuery VolumeDetail

data PricingDetail = PricingDetail
    { _pdCount :: Maybe Int
    , _pdPrice :: Maybe Double
    } deriving (Eq, Ord, Show, Generic)

-- | 'PricingDetail' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pdCount' @::@ 'Maybe' 'Int'
--
-- * 'pdPrice' @::@ 'Maybe' 'Double'
--
pricingDetail :: PricingDetail
pricingDetail = PricingDetail
    { _pdPrice = Nothing
    , _pdCount = Nothing
    }

-- | The number of instances available for the price.
pdCount :: Lens' PricingDetail (Maybe Int)
pdCount = lens _pdCount (\s a -> s { _pdCount = a })

-- | The price per instance.
pdPrice :: Lens' PricingDetail (Maybe Double)
pdPrice = lens _pdPrice (\s a -> s { _pdPrice = a })

instance FromXML PricingDetail where
    parseXML c = PricingDetail
        <$> c .:? "count"
        <*> c .:? "price"

instance ToQuery PricingDetail

data NetworkInterfacePrivateIpAddress = NetworkInterfacePrivateIpAddress
    { _nipiaAssociation      :: Maybe NetworkInterfaceAssociation
    , _nipiaPrimary          :: Maybe Bool
    , _nipiaPrivateDnsName   :: Maybe Text
    , _nipiaPrivateIpAddress :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'NetworkInterfacePrivateIpAddress' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'nipiaAssociation' @::@ 'Maybe' 'NetworkInterfaceAssociation'
--
-- * 'nipiaPrimary' @::@ 'Maybe' 'Bool'
--
-- * 'nipiaPrivateDnsName' @::@ 'Maybe' 'Text'
--
-- * 'nipiaPrivateIpAddress' @::@ 'Maybe' 'Text'
--
networkInterfacePrivateIpAddress :: NetworkInterfacePrivateIpAddress
networkInterfacePrivateIpAddress = NetworkInterfacePrivateIpAddress
    { _nipiaPrivateIpAddress = Nothing
    , _nipiaPrivateDnsName   = Nothing
    , _nipiaPrimary          = Nothing
    , _nipiaAssociation      = Nothing
    }

-- | The association information for an Elastic IP address associated with the
-- network interface.
nipiaAssociation :: Lens' NetworkInterfacePrivateIpAddress (Maybe NetworkInterfaceAssociation)
nipiaAssociation = lens _nipiaAssociation (\s a -> s { _nipiaAssociation = a })

-- | Indicates whether this IP address is the primary private IP address of
-- the network interface.
nipiaPrimary :: Lens' NetworkInterfacePrivateIpAddress (Maybe Bool)
nipiaPrimary = lens _nipiaPrimary (\s a -> s { _nipiaPrimary = a })

-- | The private DNS name.
nipiaPrivateDnsName :: Lens' NetworkInterfacePrivateIpAddress (Maybe Text)
nipiaPrivateDnsName =
    lens _nipiaPrivateDnsName (\s a -> s { _nipiaPrivateDnsName = a })

-- | The private IP address.
nipiaPrivateIpAddress :: Lens' NetworkInterfacePrivateIpAddress (Maybe Text)
nipiaPrivateIpAddress =
    lens _nipiaPrivateIpAddress (\s a -> s { _nipiaPrivateIpAddress = a })

instance FromXML NetworkInterfacePrivateIpAddress where
    parseXML c = NetworkInterfacePrivateIpAddress
        <$> c .:? "association"
        <*> c .:? "primary"
        <*> c .:? "privateDnsName"
        <*> c .:? "privateIpAddress"

instance ToQuery NetworkInterfacePrivateIpAddress

data DiskImageFormat
    = Raw  -- ^ RAW
    | Vhd  -- ^ VHD
    | Vmdk -- ^ VMDK
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable DiskImageFormat

instance FromText DiskImageFormat where
    parser = match "RAW"  Raw
         <|> match "VHD"  Vhd
         <|> match "VMDK" Vmdk

instance ToText DiskImageFormat where
    toText = \case
        Raw  -> "RAW"
        Vhd  -> "VHD"
        Vmdk -> "VMDK"

instance FromXML DiskImageFormat where
    parseXML = fromXMLText "DiskImageFormat"

instance ToQuery DiskImageFormat

data BundleTaskError = BundleTaskError
    { _bteCode    :: Maybe Text
    , _bteMessage :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'BundleTaskError' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bteCode' @::@ 'Maybe' 'Text'
--
-- * 'bteMessage' @::@ 'Maybe' 'Text'
--
bundleTaskError :: BundleTaskError
bundleTaskError = BundleTaskError
    { _bteCode    = Nothing
    , _bteMessage = Nothing
    }

-- | The error code.
bteCode :: Lens' BundleTaskError (Maybe Text)
bteCode = lens _bteCode (\s a -> s { _bteCode = a })

-- | The error message.
bteMessage :: Lens' BundleTaskError (Maybe Text)
bteMessage = lens _bteMessage (\s a -> s { _bteMessage = a })

instance FromXML BundleTaskError where
    parseXML c = BundleTaskError
        <$> c .:? "code"
        <*> c .:? "message"

instance ToQuery BundleTaskError

data VolumeStatusItem = VolumeStatusItem
    { _vsiActions          :: [VolumeStatusAction]
    , _vsiAvailabilityZone :: Maybe Text
    , _vsiEvents           :: [VolumeStatusEvent]
    , _vsiVolumeId         :: Maybe Text
    , _vsiVolumeStatus     :: Maybe VolumeStatusInfo
    } deriving (Eq, Show, Generic)

-- | 'VolumeStatusItem' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vsiActions' @::@ ['VolumeStatusAction']
--
-- * 'vsiAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'vsiEvents' @::@ ['VolumeStatusEvent']
--
-- * 'vsiVolumeId' @::@ 'Maybe' 'Text'
--
-- * 'vsiVolumeStatus' @::@ 'Maybe' 'VolumeStatusInfo'
--
volumeStatusItem :: VolumeStatusItem
volumeStatusItem = VolumeStatusItem
    { _vsiVolumeId         = Nothing
    , _vsiAvailabilityZone = Nothing
    , _vsiVolumeStatus     = Nothing
    , _vsiEvents           = mempty
    , _vsiActions          = mempty
    }

-- | The details of the operation.
vsiActions :: Lens' VolumeStatusItem [VolumeStatusAction]
vsiActions = lens _vsiActions (\s a -> s { _vsiActions = a })

-- | The Availability Zone of the volume.
vsiAvailabilityZone :: Lens' VolumeStatusItem (Maybe Text)
vsiAvailabilityZone =
    lens _vsiAvailabilityZone (\s a -> s { _vsiAvailabilityZone = a })

-- | A list of events associated with the volume.
vsiEvents :: Lens' VolumeStatusItem [VolumeStatusEvent]
vsiEvents = lens _vsiEvents (\s a -> s { _vsiEvents = a })

-- | The volume ID.
vsiVolumeId :: Lens' VolumeStatusItem (Maybe Text)
vsiVolumeId = lens _vsiVolumeId (\s a -> s { _vsiVolumeId = a })

-- | The volume status.
vsiVolumeStatus :: Lens' VolumeStatusItem (Maybe VolumeStatusInfo)
vsiVolumeStatus = lens _vsiVolumeStatus (\s a -> s { _vsiVolumeStatus = a })

instance FromXML VolumeStatusItem where
    parseXML c = VolumeStatusItem
        <$> c .: "actionsSet"
        <*> c .:? "availabilityZone"
        <*> c .: "eventsSet"
        <*> c .:? "volumeId"
        <*> c .:? "volumeStatus"

instance ToQuery VolumeStatusItem
