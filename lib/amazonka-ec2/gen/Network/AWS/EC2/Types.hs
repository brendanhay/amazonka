-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors

    -- * CarrierGateway
    CarrierGateway (..),
    mkCarrierGateway,
    cgCarrierGatewayId,
    cgOwnerId,
    cgState,
    cgTags,
    cgVpcId,

    -- * FleetCapacityReservationUsageStrategy
    FleetCapacityReservationUsageStrategy (..),

    -- * InstanceMarketOptionsRequest
    InstanceMarketOptionsRequest (..),
    mkInstanceMarketOptionsRequest,
    imorMarketType,
    imorSpotOptions,

    -- * ImageAttributeName
    ImageAttributeName (..),

    -- * VpcPeeringConnectionId
    VpcPeeringConnectionId (..),

    -- * FleetLaunchTemplateOverrides
    FleetLaunchTemplateOverrides (..),
    mkFleetLaunchTemplateOverrides,
    fltoAvailabilityZone,
    fltoInstanceType,
    fltoMaxPrice,
    fltoPlacement,
    fltoPriority,
    fltoSubnetId,
    fltoWeightedCapacity,

    -- * ExportTaskS3Location
    ExportTaskS3Location (..),
    mkExportTaskS3Location,
    etslS3Bucket,
    etslS3Prefix,

    -- * PrincipalIdFormat
    PrincipalIdFormat (..),
    mkPrincipalIdFormat,
    pifArn,
    pifStatuses,

    -- * Ipv6CidrBlock
    Ipv6CidrBlock (..),
    mkIpv6CidrBlock,
    icbIpv6CidrBlock,

    -- * PermissionGroup
    PermissionGroup (..),

    -- * MarketType
    MarketType (..),

    -- * LocalGatewayVirtualInterfaceId
    LocalGatewayVirtualInterfaceId (..),

    -- * TransitGatewayAttachmentResourceType
    TransitGatewayAttachmentResourceType (..),

    -- * StorageLocation
    StorageLocation (..),
    mkStorageLocation,
    slBucket,
    slKey,

    -- * InstanceId
    InstanceId (..),

    -- * LoadPermissionRequest
    LoadPermissionRequest (..),
    mkLoadPermissionRequest,
    lprGroup,
    lprUserId,

    -- * RouteTableAssociationStateCode
    RouteTableAssociationStateCode (..),

    -- * ClientVpnRouteStatus
    ClientVpnRouteStatus (..),
    mkClientVpnRouteStatus,
    cvrsCode,
    cvrsMessage,

    -- * ExportTaskId
    ExportTaskId (..),

    -- * AddressStatus
    AddressStatus (..),

    -- * NetworkAclEntry
    NetworkAclEntry (..),
    mkNetworkAclEntry,
    naeCidrBlock,
    naeEgress,
    naeIcmpTypeCode,
    naeIpv6CidrBlock,
    naePortRange,
    naeProtocol,
    naeRuleAction,
    naeRuleNumber,

    -- * CapacityReservationOptions
    CapacityReservationOptions (..),
    mkCapacityReservationOptions,
    croUsageStrategy,

    -- * Phase1DHGroupNumbersListValue
    Phase1DHGroupNumbersListValue (..),
    mkPhase1DHGroupNumbersListValue,
    pdhgnlvValue,

    -- * BlobAttributeValue
    BlobAttributeValue (..),
    mkBlobAttributeValue,
    bavValue,

    -- * Phase1IntegrityAlgorithmsListValue
    Phase1IntegrityAlgorithmsListValue (..),
    mkPhase1IntegrityAlgorithmsListValue,
    pialvValue,

    -- * ServiceTypeDetail
    ServiceTypeDetail (..),
    mkServiceTypeDetail,
    stdServiceType,

    -- * LaunchTemplateElasticInferenceAccelerator
    LaunchTemplateElasticInferenceAccelerator (..),
    mkLaunchTemplateElasticInferenceAccelerator,
    lteiaType,
    lteiaCount,

    -- * TransitGatewayPrefixListAttachment
    TransitGatewayPrefixListAttachment (..),
    mkTransitGatewayPrefixListAttachment,
    tgplaResourceId,
    tgplaResourceType,
    tgplaTransitGatewayAttachmentId,

    -- * ImportInstanceLaunchSpecification
    ImportInstanceLaunchSpecification (..),
    mkImportInstanceLaunchSpecification,
    iilsAdditionalInfo,
    iilsArchitecture,
    iilsGroupIds,
    iilsGroupNames,
    iilsInstanceInitiatedShutdownBehavior,
    iilsInstanceType,
    iilsMonitoring,
    iilsPlacement,
    iilsPrivateIpAddress,
    iilsSubnetId,
    iilsUserData,

    -- * RequestLaunchTemplateData
    RequestLaunchTemplateData (..),
    mkRequestLaunchTemplateData,
    rltdBlockDeviceMappings,
    rltdCapacityReservationSpecification,
    rltdCpuOptions,
    rltdCreditSpecification,
    rltdDisableApiTermination,
    rltdEbsOptimized,
    rltdElasticGpuSpecifications,
    rltdElasticInferenceAccelerators,
    rltdEnclaveOptions,
    rltdHibernationOptions,
    rltdIamInstanceProfile,
    rltdImageId,
    rltdInstanceInitiatedShutdownBehavior,
    rltdInstanceMarketOptions,
    rltdInstanceType,
    rltdKernelId,
    rltdKeyName,
    rltdLicenseSpecifications,
    rltdMetadataOptions,
    rltdMonitoring,
    rltdNetworkInterfaces,
    rltdPlacement,
    rltdRamDiskId,
    rltdSecurityGroupIds,
    rltdSecurityGroups,
    rltdTagSpecifications,
    rltdUserData,

    -- * Snapshot
    Snapshot (..),
    mkSnapshot,
    sDataEncryptionKeyId,
    sDescription,
    sEncrypted,
    sKmsKeyId,
    sOwnerAlias,
    sOwnerId,
    sProgress,
    sSnapshotId,
    sStartTime,
    sState,
    sStateMessage,
    sTags,
    sVolumeId,
    sVolumeSize,

    -- * ConnectionNotificationState
    ConnectionNotificationState (..),

    -- * SpotInstanceStateFault
    SpotInstanceStateFault (..),
    mkSpotInstanceStateFault,
    sisfCode,
    sisfMessage,

    -- * InferenceDeviceName
    InferenceDeviceName (..),

    -- * PeeringConnectionOptions
    PeeringConnectionOptions (..),
    mkPeeringConnectionOptions,
    pcoAllowDnsResolutionFromRemoteVpc,
    pcoAllowEgressFromLocalClassicLinkToRemoteVpc,
    pcoAllowEgressFromLocalVpcToRemoteClassicLink,

    -- * ClientConnectResponseOptions
    ClientConnectResponseOptions (..),
    mkClientConnectResponseOptions,
    ccroEnabled,
    ccroLambdaFunctionArn,
    ccroStatus,

    -- * TagDescription
    TagDescription (..),
    mkTagDescription,
    tdKey,
    tdResourceId,
    tdResourceType,
    tdValue,

    -- * ImportSnapshotTask
    ImportSnapshotTask (..),
    mkImportSnapshotTask,
    istDescription,
    istImportTaskId,
    istSnapshotTaskDetail,
    istTags,

    -- * GroupIdentifier
    GroupIdentifier (..),
    mkGroupIdentifier,
    giGroupId,
    giGroupName,

    -- * RouteGatewayId
    RouteGatewayId (..),

    -- * VpnStaticRouteSource
    VpnStaticRouteSource (..),

    -- * ScheduledInstanceRecurrence
    ScheduledInstanceRecurrence (..),
    mkScheduledInstanceRecurrence,
    sirFrequency,
    sirInterval,
    sirOccurrenceDaySet,
    sirOccurrenceRelativeToEnd,
    sirOccurrenceUnit,

    -- * LocalGatewayRoute
    LocalGatewayRoute (..),
    mkLocalGatewayRoute,
    lgrDestinationCidrBlock,
    lgrLocalGatewayRouteTableArn,
    lgrLocalGatewayRouteTableId,
    lgrLocalGatewayVirtualInterfaceGroupId,
    lgrOwnerId,
    lgrState,
    lgrType,

    -- * TrafficMirrorRuleAction
    TrafficMirrorRuleAction (..),

    -- * Phase1EncryptionAlgorithmsListValue
    Phase1EncryptionAlgorithmsListValue (..),
    mkPhase1EncryptionAlgorithmsListValue,
    pealvfValue,

    -- * DirectoryServiceAuthentication
    DirectoryServiceAuthentication (..),
    mkDirectoryServiceAuthentication,
    dsaDirectoryId,

    -- * AssociatedRole
    AssociatedRole (..),
    mkAssociatedRole,
    arAssociatedRoleArn,
    arCertificateS3BucketName,
    arCertificateS3ObjectKey,
    arEncryptionKmsKeyId,

    -- * ReservedInstancesListing
    ReservedInstancesListing (..),
    mkReservedInstancesListing,
    rilClientToken,
    rilCreateDate,
    rilInstanceCounts,
    rilPriceSchedules,
    rilReservedInstancesId,
    rilReservedInstancesListingId,
    rilStatus,
    rilStatusMessage,
    rilTags,
    rilUpdateDate,

    -- * AssociatedTargetNetwork
    AssociatedTargetNetwork (..),
    mkAssociatedTargetNetwork,
    atnNetworkId,
    atnNetworkType,

    -- * PciId
    PciId (..),
    mkPciId,
    piDeviceId,
    piSubsystemId,
    piSubsystemVendorId,
    piVendorId,

    -- * InstanceLifecycleType
    InstanceLifecycleType (..),

    -- * ExportVmTaskId
    ExportVmTaskId (..),

    -- * RegisterInstanceTagAttributeRequest
    RegisterInstanceTagAttributeRequest (..),
    mkRegisterInstanceTagAttributeRequest,
    ritarIncludeAllTagsOfInstance,
    ritarInstanceTagKeys,

    -- * ManagedPrefixList
    ManagedPrefixList (..),
    mkManagedPrefixList,
    mplAddressFamily,
    mplMaxEntries,
    mplOwnerId,
    mplPrefixListArn,
    mplPrefixListId,
    mplPrefixListName,
    mplState,
    mplStateMessage,
    mplTags,
    mplVersion,

    -- * State
    State (..),

    -- * EbsOptimizedInfo
    EbsOptimizedInfo (..),
    mkEbsOptimizedInfo,
    eoiBaselineBandwidthInMbps,
    eoiBaselineIops,
    eoiBaselineThroughputInMBps,
    eoiMaximumBandwidthInMbps,
    eoiMaximumIops,
    eoiMaximumThroughputInMBps,

    -- * VirtualizationType
    VirtualizationType (..),

    -- * PeeringAttachmentStatus
    PeeringAttachmentStatus (..),
    mkPeeringAttachmentStatus,
    pasCode,
    pasMessage,

    -- * NetworkInterfaceStatus
    NetworkInterfaceStatus (..),

    -- * PlatformValues
    PlatformValues (..),

    -- * Phase2EncryptionAlgorithmsListValue
    Phase2EncryptionAlgorithmsListValue (..),
    mkPhase2EncryptionAlgorithmsListValue,
    pealvValue,

    -- * VpcPeeringConnectionOptionsDescription
    VpcPeeringConnectionOptionsDescription (..),
    mkVpcPeeringConnectionOptionsDescription,
    vpcodAllowDnsResolutionFromRemoteVpc,
    vpcodAllowEgressFromLocalClassicLinkToRemoteVpc,
    vpcodAllowEgressFromLocalVpcToRemoteClassicLink,

    -- * EgressOnlyInternetGatewayId
    EgressOnlyInternetGatewayId (..),

    -- * EnclaveOptionsRequest
    EnclaveOptionsRequest (..),
    mkEnclaveOptionsRequest,
    eorEnabled,

    -- * TunnelInsideIpVersion
    TunnelInsideIpVersion (..),

    -- * AutoAcceptSharedAttachmentsValue
    AutoAcceptSharedAttachmentsValue (..),

    -- * VpnEcmpSupportValue
    VpnEcmpSupportValue (..),

    -- * TransitGatewayOptions
    TransitGatewayOptions (..),
    mkTransitGatewayOptions,
    tgoAmazonSideAsn,
    tgoAssociationDefaultRouteTableId,
    tgoAutoAcceptSharedAttachments,
    tgoDefaultRouteTableAssociation,
    tgoDefaultRouteTablePropagation,
    tgoDnsSupport,
    tgoMulticastSupport,
    tgoPropagationDefaultRouteTableId,
    tgoVpnEcmpSupport,

    -- * CreateVolumePermission
    CreateVolumePermission (..),
    mkCreateVolumePermission,
    cvpGroup,
    cvpUserId,

    -- * InstanceFamilyCreditSpecification
    InstanceFamilyCreditSpecification (..),
    mkInstanceFamilyCreditSpecification,
    ifcsCpuCredits,
    ifcsInstanceFamily,

    -- * EnableFastSnapshotRestoreStateErrorItem
    EnableFastSnapshotRestoreStateErrorItem (..),
    mkEnableFastSnapshotRestoreStateErrorItem,
    efsrseiAvailabilityZone,
    efsrseiError,

    -- * NetworkInterfaceAttachmentChanges
    NetworkInterfaceAttachmentChanges (..),
    mkNetworkInterfaceAttachmentChanges,
    niacAttachmentId,
    niacDeleteOnTermination,

    -- * MemoryInfo
    MemoryInfo (..),
    mkMemoryInfo,
    miSizeInMiB,

    -- * RecurringChargeFrequency
    RecurringChargeFrequency (..),

    -- * ScheduledInstancesEbs
    ScheduledInstancesEbs (..),
    mkScheduledInstancesEbs,
    sieDeleteOnTermination,
    sieEncrypted,
    sieIops,
    sieSnapshotId,
    sieVolumeSize,
    sieVolumeType,

    -- * ModifyAvailabilityZoneOptInStatus
    ModifyAvailabilityZoneOptInStatus (..),

    -- * DhcpOptions
    DhcpOptions (..),
    mkDhcpOptions,
    doDhcpConfigurations,
    doDhcpOptionsId,
    doOwnerId,
    doTags,

    -- * SlotStartTimeRangeRequest
    SlotStartTimeRangeRequest (..),
    mkSlotStartTimeRangeRequest,
    sstrrEarliestTime,
    sstrrLatestTime,

    -- * InstanceNetworkInterfaceSpecification
    InstanceNetworkInterfaceSpecification (..),
    mkInstanceNetworkInterfaceSpecification,
    inisAssociateCarrierIpAddress,
    inisAssociatePublicIpAddress,
    inisDeleteOnTermination,
    inisDescription,
    inisDeviceIndex,
    inisGroups,
    inisInterfaceType,
    inisIpv6AddressCount,
    inisIpv6Addresses,
    inisNetworkCardIndex,
    inisNetworkInterfaceId,
    inisPrivateIpAddress,
    inisPrivateIpAddresses,
    inisSecondaryPrivateIpAddressCount,
    inisSubnetId,

    -- * FleetLaunchTemplateSpecificationRequest
    FleetLaunchTemplateSpecificationRequest (..),
    mkFleetLaunchTemplateSpecificationRequest,
    fltsrLaunchTemplateId,
    fltsrLaunchTemplateName,
    fltsrVersion,

    -- * OnDemandOptionsRequest
    OnDemandOptionsRequest (..),
    mkOnDemandOptionsRequest,
    odorAllocationStrategy,
    odorCapacityReservationOptions,
    odorMaxTotalPrice,
    odorMinTargetCapacity,
    odorSingleAvailabilityZone,
    odorSingleInstanceType,

    -- * InstanceTypeInfo
    InstanceTypeInfo (..),
    mkInstanceTypeInfo,
    itiAutoRecoverySupported,
    itiBareMetal,
    itiBurstablePerformanceSupported,
    itiCurrentGeneration,
    itiDedicatedHostsSupported,
    itiEbsInfo,
    itiFpgaInfo,
    itiFreeTierEligible,
    itiGpuInfo,
    itiHibernationSupported,
    itiHypervisor,
    itiInferenceAcceleratorInfo,
    itiInstanceStorageInfo,
    itiInstanceStorageSupported,
    itiInstanceType,
    itiMemoryInfo,
    itiNetworkInfo,
    itiPlacementGroupInfo,
    itiProcessorInfo,
    itiSupportedRootDeviceTypes,
    itiSupportedUsageClasses,
    itiSupportedVirtualizationTypes,
    itiVCpuInfo,

    -- * Phase2DHGroupNumbersListValue
    Phase2DHGroupNumbersListValue (..),
    mkPhase2DHGroupNumbersListValue,
    pValue,

    -- * ReservationState
    ReservationState (..),

    -- * FpgaDeviceName
    FpgaDeviceName (..),

    -- * TransitGateway
    TransitGateway (..),
    mkTransitGateway,
    tgCreationTime,
    tgDescription,
    tgOptions,
    tgOwnerId,
    tgState,
    tgTags,
    tgTransitGatewayArn,
    tgTransitGatewayId,

    -- * VolumeState
    VolumeState (..),

    -- * AddPrefixListEntry
    AddPrefixListEntry (..),
    mkAddPrefixListEntry,
    apleCidr,
    apleDescription,

    -- * CpuOptionsRequest
    CpuOptionsRequest (..),
    mkCpuOptionsRequest,
    corCoreCount,
    corThreadsPerCore,

    -- * ClientCertificateRevocationListStatusCode
    ClientCertificateRevocationListStatusCode (..),

    -- * TransitGatewayMulticastRegisteredGroupMembers
    TransitGatewayMulticastRegisteredGroupMembers (..),
    mkTransitGatewayMulticastRegisteredGroupMembers,
    tgmrgmGroupIpAddress,
    tgmrgmRegisteredNetworkInterfaceIds,
    tgmrgmTransitGatewayMulticastDomainId,

    -- * VpcPeeringConnectionStateReasonCode
    VpcPeeringConnectionStateReasonCode (..),

    -- * AttributeValue
    AttributeValue (..),
    mkAttributeValue,
    avValue,

    -- * LaunchTemplateElasticInferenceAcceleratorResponse
    LaunchTemplateElasticInferenceAcceleratorResponse (..),
    mkLaunchTemplateElasticInferenceAcceleratorResponse,
    lteiarCount,
    lteiarType,

    -- * ClientVpnConnection
    ClientVpnConnection (..),
    mkClientVpnConnection,
    cvcClientIp,
    cvcClientVpnEndpointId,
    cvcCommonName,
    cvcConnectionEndTime,
    cvcConnectionEstablishedTime,
    cvcConnectionId,
    cvcEgressBytes,
    cvcEgressPackets,
    cvcIngressBytes,
    cvcIngressPackets,
    cvcPostureComplianceStatuses,
    cvcStatus,
    cvcTimestamp,
    cvcUsername,

    -- * PrivateIpAddressSpecification
    PrivateIpAddressSpecification (..),
    mkPrivateIpAddressSpecification,
    piasPrimary,
    piasPrivateIpAddress,

    -- * Ipv6Address
    Ipv6Address (..),

    -- * DeleteQueuedReservedInstancesErrorCode
    DeleteQueuedReservedInstancesErrorCode (..),

    -- * Image
    Image (..),
    mkImage,
    ifArchitecture,
    ifBlockDeviceMappings,
    ifCreationDate,
    ifDescription,
    ifEnaSupport,
    ifHypervisor,
    ifImageId,
    ifImageLocation,
    ifImageOwnerAlias,
    ifImageType,
    ifKernelId,
    ifName,
    ifOwnerId,
    ifPlatform,
    ifPlatformDetails,
    ifProductCodes,
    ifPublic,
    ifRamdiskId,
    ifRootDeviceName,
    ifRootDeviceType,
    ifSriovNetSupport,
    ifState,
    ifStateReason,
    ifTags,
    ifUsageOperation,
    ifVirtualizationType,

    -- * DhcpConfiguration
    DhcpConfiguration (..),
    mkDhcpConfiguration,
    dcKey,
    dcValues,

    -- * CancelSpotFleetRequestsError
    CancelSpotFleetRequestsError (..),
    mkCancelSpotFleetRequestsError,
    csfreCode,
    csfreMessage,

    -- * InstanceTagNotificationAttribute
    InstanceTagNotificationAttribute (..),
    mkInstanceTagNotificationAttribute,
    itnaIncludeAllTagsOfInstance,
    itnaInstanceTagKeys,

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * CapacityReservationSpecificationResponse
    CapacityReservationSpecificationResponse (..),
    mkCapacityReservationSpecificationResponse,
    crsrCapacityReservationPreference,
    crsrCapacityReservationTarget,

    -- * NetworkInterfacePermissionStateCode
    NetworkInterfacePermissionStateCode (..),

    -- * DeleteFleetErrorItem
    DeleteFleetErrorItem (..),
    mkDeleteFleetErrorItem,
    dfeiError,
    dfeiFleetId,

    -- * AccountAttributeName
    AccountAttributeName (..),

    -- * LaunchTemplateAndOverridesResponse
    LaunchTemplateAndOverridesResponse (..),
    mkLaunchTemplateAndOverridesResponse,
    ltaorLaunchTemplateSpecification,
    ltaorOverrides,

    -- * TransitGatewayMulticastDomainAssociation
    TransitGatewayMulticastDomainAssociation (..),
    mkTransitGatewayMulticastDomainAssociation,
    tgmdaResourceId,
    tgmdaResourceType,
    tgmdaSubnet,
    tgmdaTransitGatewayAttachmentId,

    -- * LaunchTemplateHibernationOptions
    LaunchTemplateHibernationOptions (..),
    mkLaunchTemplateHibernationOptions,
    lthoConfigured,

    -- * NetworkInterfaceAttachment
    NetworkInterfaceAttachment (..),
    mkNetworkInterfaceAttachment,
    niaAttachTime,
    niaAttachmentId,
    niaDeleteOnTermination,
    niaDeviceIndex,
    niaInstanceId,
    niaInstanceOwnerId,
    niaNetworkCardIndex,
    niaStatus,

    -- * ConnectionLogOptions
    ConnectionLogOptions (..),
    mkConnectionLogOptions,
    cloCloudwatchLogGroup,
    cloCloudwatchLogStream,
    cloEnabled,

    -- * SpotFleetMonitoring
    SpotFleetMonitoring (..),
    mkSpotFleetMonitoring,
    sfmEnabled,

    -- * RunInstancesMonitoringEnabled
    RunInstancesMonitoringEnabled (..),
    mkRunInstancesMonitoringEnabled,
    rimeEnabled,

    -- * VolumeStatusInfo
    VolumeStatusInfo (..),
    mkVolumeStatusInfo,
    vsiDetails,
    vsiStatus,

    -- * RouteTableId
    RouteTableId (..),

    -- * CapacityReservationInstancePlatform
    CapacityReservationInstancePlatform (..),

    -- * NetworkInterfaceAssociation
    NetworkInterfaceAssociation (..),
    mkNetworkInterfaceAssociation,
    niaAllocationId,
    niaAssociationId,
    niaCarrierIp,
    niaCustomerOwnedIp,
    niaIpOwnerId,
    niaPublicDnsName,
    niaPublicIp,

    -- * LaunchTemplateOverrides
    LaunchTemplateOverrides (..),
    mkLaunchTemplateOverrides,
    ltoAvailabilityZone,
    ltoInstanceType,
    ltoPriority,
    ltoSpotPrice,
    ltoSubnetId,
    ltoWeightedCapacity,

    -- * SecurityGroupIdentifier
    SecurityGroupIdentifier (..),
    mkSecurityGroupIdentifier,
    sgiGroupId,
    sgiGroupName,

    -- * TrafficMirrorSession
    TrafficMirrorSession (..),
    mkTrafficMirrorSession,
    tmsDescription,
    tmsNetworkInterfaceId,
    tmsOwnerId,
    tmsPacketLength,
    tmsSessionNumber,
    tmsTags,
    tmsTrafficMirrorFilterId,
    tmsTrafficMirrorSessionId,
    tmsTrafficMirrorTargetId,
    tmsVirtualNetworkId,

    -- * DisableFastSnapshotRestoreStateErrorItem
    DisableFastSnapshotRestoreStateErrorItem (..),
    mkDisableFastSnapshotRestoreStateErrorItem,
    dfsrseiAvailabilityZone,
    dfsrseiError,

    -- * CreateVolumePermissionModifications
    CreateVolumePermissionModifications (..),
    mkCreateVolumePermissionModifications,
    cvpmAdd,
    cvpmRemove,

    -- * SubnetAssociation
    SubnetAssociation (..),
    mkSubnetAssociation,
    saState,
    saSubnetId,

    -- * SnapshotInfo
    SnapshotInfo (..),
    mkSnapshotInfo,
    siDescription,
    siEncrypted,
    siOwnerId,
    siProgress,
    siSnapshotId,
    siStartTime,
    siState,
    siTags,
    siVolumeId,
    siVolumeSize,

    -- * ScheduledInstancesPlacement
    ScheduledInstancesPlacement (..),
    mkScheduledInstancesPlacement,
    sipAvailabilityZone,
    sipGroupName,

    -- * LocalGatewayRouteTable
    LocalGatewayRouteTable (..),
    mkLocalGatewayRouteTable,
    lgrtLocalGatewayId,
    lgrtLocalGatewayRouteTableArn,
    lgrtLocalGatewayRouteTableId,
    lgrtOutpostArn,
    lgrtOwnerId,
    lgrtState,
    lgrtTags,

    -- * ModifyTransitGatewayVpcAttachmentRequestOptions
    ModifyTransitGatewayVpcAttachmentRequestOptions (..),
    mkModifyTransitGatewayVpcAttachmentRequestOptions,
    mtgvaroApplianceModeSupport,
    mtgvaroDnsSupport,
    mtgvaroIpv6Support,

    -- * ElasticGpuId
    ElasticGpuId (..),

    -- * LaunchTemplateName
    LaunchTemplateName (..),

    -- * VpcState
    VpcState (..),

    -- * ResourceType
    ResourceType (..),

    -- * ReportStatusType
    ReportStatusType (..),

    -- * ImportImageTaskId
    ImportImageTaskId (..),

    -- * TrafficMirrorFilterRule
    TrafficMirrorFilterRule (..),
    mkTrafficMirrorFilterRule,
    tmfrDescription,
    tmfrDestinationCidrBlock,
    tmfrDestinationPortRange,
    tmfrProtocol,
    tmfrRuleAction,
    tmfrRuleNumber,
    tmfrSourceCidrBlock,
    tmfrSourcePortRange,
    tmfrTrafficDirection,
    tmfrTrafficMirrorFilterId,
    tmfrTrafficMirrorFilterRuleId,

    -- * Affinity
    Affinity (..),

    -- * CurrencyCodeValues
    CurrencyCodeValues (..),

    -- * FleetSpotCapacityRebalanceRequest
    FleetSpotCapacityRebalanceRequest (..),
    mkFleetSpotCapacityRebalanceRequest,
    fscrrReplacementStrategy,

    -- * IcmpTypeCode
    IcmpTypeCode (..),
    mkIcmpTypeCode,
    itcCode,
    itcType,

    -- * LaunchTemplateCpuOptionsRequest
    LaunchTemplateCpuOptionsRequest (..),
    mkLaunchTemplateCpuOptionsRequest,
    ltcorCoreCount,
    ltcorThreadsPerCore,

    -- * DeleteFleetErrorCode
    DeleteFleetErrorCode (..),

    -- * LaunchTemplateInstanceNetworkInterfaceSpecification
    LaunchTemplateInstanceNetworkInterfaceSpecification (..),
    mkLaunchTemplateInstanceNetworkInterfaceSpecification,
    ltinisAssociateCarrierIpAddress,
    ltinisAssociatePublicIpAddress,
    ltinisDeleteOnTermination,
    ltinisDescription,
    ltinisDeviceIndex,
    ltinisGroups,
    ltinisInterfaceType,
    ltinisIpv6AddressCount,
    ltinisIpv6Addresses,
    ltinisNetworkCardIndex,
    ltinisNetworkInterfaceId,
    ltinisPrivateIpAddress,
    ltinisPrivateIpAddresses,
    ltinisSecondaryPrivateIpAddressCount,
    ltinisSubnetId,

    -- * AllowedPrincipal
    AllowedPrincipal (..),
    mkAllowedPrincipal,
    apPrincipal,
    apPrincipalType,

    -- * LaunchTemplatesMonitoring
    LaunchTemplatesMonitoring (..),
    mkLaunchTemplatesMonitoring,
    ltmEnabled,

    -- * PrincipalType
    PrincipalType (..),

    -- * LaunchTemplateTagSpecification
    LaunchTemplateTagSpecification (..),
    mkLaunchTemplateTagSpecification,
    lttsResourceType,
    lttsTags,

    -- * ClientVpnEndpoint
    ClientVpnEndpoint (..),
    mkClientVpnEndpoint,
    cveAssociatedTargetNetworks,
    cveAuthenticationOptions,
    cveClientCidrBlock,
    cveClientConnectOptions,
    cveClientVpnEndpointId,
    cveConnectionLogOptions,
    cveCreationTime,
    cveDeletionTime,
    cveDescription,
    cveDnsName,
    cveDnsServers,
    cveSecurityGroupIds,
    cveSelfServicePortalUrl,
    cveServerCertificateArn,
    cveSplitTunnel,
    cveStatus,
    cveTags,
    cveTransportProtocol,
    cveVpcId,
    cveVpnPort,
    cveVpnProtocol,

    -- * HostTenancy
    HostTenancy (..),

    -- * GpuDeviceManufacturerName
    GpuDeviceManufacturerName (..),

    -- * FpgaImageId
    FpgaImageId (..),

    -- * InstanceCount
    InstanceCount (..),
    mkInstanceCount,
    icInstanceCount,
    icState,

    -- * ClientVpnEndpointAttributeStatus
    ClientVpnEndpointAttributeStatus (..),
    mkClientVpnEndpointAttributeStatus,
    cveasCode,
    cveasMessage,

    -- * ExportToS3Task
    ExportToS3Task (..),
    mkExportToS3Task,
    etstContainerFormat,
    etstDiskImageFormat,
    etstS3Bucket,
    etstS3Key,

    -- * VpcCidrBlockStateCode
    VpcCidrBlockStateCode (..),

    -- * PrefixList
    PrefixList (..),
    mkPrefixList,
    plCidrs,
    plPrefixListId,
    plPrefixListName,

    -- * LaunchTemplateEnclaveOptionsRequest
    LaunchTemplateEnclaveOptionsRequest (..),
    mkLaunchTemplateEnclaveOptionsRequest,
    lteorEnabled,

    -- * InstanceInterruptionBehavior
    InstanceInterruptionBehavior (..),

    -- * Location
    Location (..),

    -- * FleetExcessCapacityTerminationPolicy
    FleetExcessCapacityTerminationPolicy (..),

    -- * InstanceMetadataOptionsRequest
    InstanceMetadataOptionsRequest (..),
    mkInstanceMetadataOptionsRequest,
    iHttpEndpoint,
    iHttpPutResponseHopLimit,
    iHttpTokens,

    -- * ClientVpnAuthorizationRuleStatusCode
    ClientVpnAuthorizationRuleStatusCode (..),

    -- * BlockDeviceMapping
    BlockDeviceMapping (..),
    mkBlockDeviceMapping,
    bdmDeviceName,
    bdmEbs,
    bdmNoDevice,
    bdmVirtualName,

    -- * InstanceCapacity
    InstanceCapacity (..),
    mkInstanceCapacity,
    icAvailableCapacity,
    icInstanceType,
    icTotalCapacity,

    -- * TransitGatewayRouteTableId
    TransitGatewayRouteTableId (..),

    -- * DnsSupportValue
    DnsSupportValue (..),

    -- * NetworkAclId
    NetworkAclId (..),

    -- * LaunchTemplateInstanceMetadataEndpointState
    LaunchTemplateInstanceMetadataEndpointState (..),

    -- * AllocationId
    AllocationId (..),

    -- * SensitiveUserData
    SensitiveUserData (..),

    -- * ConversionTask
    ConversionTask (..),
    mkConversionTask,
    ctConversionTaskId,
    ctExpirationTime,
    ctImportInstance,
    ctImportVolume,
    ctState,
    ctStatusMessage,
    ctTags,

    -- * AttachmentStatus
    AttachmentStatus (..),

    -- * SpotCapacityRebalance
    SpotCapacityRebalance (..),
    mkSpotCapacityRebalance,
    scrReplacementStrategy,

    -- * GpuDeviceName
    GpuDeviceName (..),

    -- * FederatedAuthenticationRequest
    FederatedAuthenticationRequest (..),
    mkFederatedAuthenticationRequest,
    farSAMLProviderArn,
    farSelfServiceSAMLProviderArn,

    -- * ClassicLinkInstance
    ClassicLinkInstance (..),
    mkClassicLinkInstance,
    cliGroups,
    cliInstanceId,
    cliTags,
    cliVpcId,

    -- * String
    String (..),

    -- * TransportProtocol
    TransportProtocol (..),

    -- * RouteOrigin
    RouteOrigin (..),

    -- * PrefixListState
    PrefixListState (..),

    -- * ListingState
    ListingState (..),

    -- * SpotPrice
    SpotPrice (..),
    mkSpotPrice,
    sAvailabilityZone,
    sInstanceType,
    sProductDescription,
    sSpotPrice,
    sTimestamp,

    -- * ActiveInstance
    ActiveInstance (..),
    mkActiveInstance,
    aiInstanceHealth,
    aiInstanceId,
    aiInstanceType,
    aiSpotInstanceRequestId,

    -- * TrafficType
    TrafficType (..),

    -- * LaunchTemplatePlacementRequest
    LaunchTemplatePlacementRequest (..),
    mkLaunchTemplatePlacementRequest,
    ltprAffinity,
    ltprAvailabilityZone,
    ltprGroupName,
    ltprHostId,
    ltprHostResourceGroupArn,
    ltprPartitionNumber,
    ltprSpreadDomain,
    ltprTenancy,

    -- * LaunchTemplateCapacityReservationSpecificationRequest
    LaunchTemplateCapacityReservationSpecificationRequest (..),
    mkLaunchTemplateCapacityReservationSpecificationRequest,
    ltcrsrCapacityReservationPreference,
    ltcrsrCapacityReservationTarget,

    -- * VpnGatewayId
    VpnGatewayId (..),

    -- * SpotFleetRequestConfigData
    SpotFleetRequestConfigData (..),
    mkSpotFleetRequestConfigData,
    sfrcdIamFleetRole,
    sfrcdTargetCapacity,
    sfrcdAllocationStrategy,
    sfrcdClientToken,
    sfrcdExcessCapacityTerminationPolicy,
    sfrcdFulfilledCapacity,
    sfrcdInstanceInterruptionBehavior,
    sfrcdInstancePoolsToUseCount,
    sfrcdLaunchSpecifications,
    sfrcdLaunchTemplateConfigs,
    sfrcdLoadBalancersConfig,
    sfrcdOnDemandAllocationStrategy,
    sfrcdOnDemandFulfilledCapacity,
    sfrcdOnDemandMaxTotalPrice,
    sfrcdOnDemandTargetCapacity,
    sfrcdReplaceUnhealthyInstances,
    sfrcdSpotMaintenanceStrategies,
    sfrcdSpotMaxTotalPrice,
    sfrcdSpotPrice,
    sfrcdTagSpecifications,
    sfrcdTerminateInstancesWithExpiration,
    sfrcdType,
    sfrcdValidFrom,
    sfrcdValidUntil,

    -- * ReplacementStrategy
    ReplacementStrategy (..),

    -- * ArchitectureType
    ArchitectureType (..),

    -- * EnaSupport
    EnaSupport (..),

    -- * AvailableCapacity
    AvailableCapacity (..),
    mkAvailableCapacity,
    acAvailableInstanceCapacity,
    acAvailableVCpus,

    -- * NatGatewayAddress
    NatGatewayAddress (..),
    mkNatGatewayAddress,
    ngaAllocationId,
    ngaNetworkInterfaceId,
    ngaPrivateIp,
    ngaPublicIp,

    -- * InstanceMonitoring
    InstanceMonitoring (..),
    mkInstanceMonitoring,
    imInstanceId,
    imMonitoring,

    -- * ScheduledInstanceId
    ScheduledInstanceId (..),

    -- * HostReservationId
    HostReservationId (..),

    -- * PlacementGroupStrategy
    PlacementGroupStrategy (..),

    -- * TrafficMirrorTargetType
    TrafficMirrorTargetType (..),

    -- * CapacityReservationTarget
    CapacityReservationTarget (..),
    mkCapacityReservationTarget,
    crtCapacityReservationId,
    crtCapacityReservationResourceGroupArn,

    -- * ModifyVpnTunnelOptionsSpecification
    ModifyVpnTunnelOptionsSpecification (..),
    mkModifyVpnTunnelOptionsSpecification,
    mvtosDPDTimeoutAction,
    mvtosDPDTimeoutSeconds,
    mvtosIKEVersions,
    mvtosPhase1DHGroupNumbers,
    mvtosPhase1EncryptionAlgorithms,
    mvtosPhase1IntegrityAlgorithms,
    mvtosPhase1LifetimeSeconds,
    mvtosPhase2DHGroupNumbers,
    mvtosPhase2EncryptionAlgorithms,
    mvtosPhase2IntegrityAlgorithms,
    mvtosPhase2LifetimeSeconds,
    mvtosPreSharedKey,
    mvtosRekeyFuzzPercentage,
    mvtosRekeyMarginTimeSeconds,
    mvtosReplayWindowSize,
    mvtosStartupAction,
    mvtosTunnelInsideCidr,
    mvtosTunnelInsideIpv6Cidr,

    -- * ServiceConfiguration
    ServiceConfiguration (..),
    mkServiceConfiguration,
    scAcceptanceRequired,
    scAvailabilityZones,
    scBaseEndpointDnsNames,
    scGatewayLoadBalancerArns,
    scManagesVpcEndpoints,
    scNetworkLoadBalancerArns,
    scPrivateDnsName,
    scPrivateDnsNameConfiguration,
    scServiceId,
    scServiceName,
    scServiceState,
    scServiceType,
    scTags,

    -- * LoadPermission
    LoadPermission (..),
    mkLoadPermission,
    lpGroup,
    lpUserId,

    -- * NetworkInterfacePermissionId
    NetworkInterfacePermissionId (..),

    -- * PriceScheduleSpecification
    PriceScheduleSpecification (..),
    mkPriceScheduleSpecification,
    pssCurrencyCode,
    pssPrice,
    pssTerm,

    -- * StaleIpPermission
    StaleIpPermission (..),
    mkStaleIpPermission,
    sipFromPort,
    sipIpProtocol,
    sipIpRanges,
    sipPrefixListIds,
    sipToPort,
    sipUserIdGroupPairs,

    -- * SpotFleetRequestConfig
    SpotFleetRequestConfig (..),
    mkSpotFleetRequestConfig,
    sfrcActivityStatus,
    sfrcCreateTime,
    sfrcSpotFleetRequestConfig,
    sfrcSpotFleetRequestId,
    sfrcSpotFleetRequestState,
    sfrcTags,

    -- * PrefixListResourceId
    PrefixListResourceId (..),

    -- * SpotInstanceStatus
    SpotInstanceStatus (..),
    mkSpotInstanceStatus,
    sisCode,
    sisMessage,
    sisUpdateTime,

    -- * ElasticGpuState
    ElasticGpuState (..),

    -- * TargetCapacitySpecification
    TargetCapacitySpecification (..),
    mkTargetCapacitySpecification,
    tcsDefaultTargetCapacityType,
    tcsOnDemandTargetCapacity,
    tcsSpotTargetCapacity,
    tcsTotalTargetCapacity,

    -- * SnapshotTaskDetail
    SnapshotTaskDetail (..),
    mkSnapshotTaskDetail,
    stdDescription,
    stdDiskImageSize,
    stdEncrypted,
    stdFormat,
    stdKmsKeyId,
    stdProgress,
    stdSnapshotId,
    stdStatus,
    stdStatusMessage,
    stdUrl,
    stdUserBucket,

    -- * VpcIpv6CidrBlockAssociation
    VpcIpv6CidrBlockAssociation (..),
    mkVpcIpv6CidrBlockAssociation,
    vicbaAssociationId,
    vicbaIpv6CidrBlock,
    vicbaIpv6CidrBlockState,
    vicbaIpv6Pool,
    vicbaNetworkBorderGroup,

    -- * EnableFastSnapshotRestoreErrorItem
    EnableFastSnapshotRestoreErrorItem (..),
    mkEnableFastSnapshotRestoreErrorItem,
    efsreiFastSnapshotRestoreStateErrors,
    efsreiSnapshotId,

    -- * TrafficMirrorSessionField
    TrafficMirrorSessionField (..),

    -- * SpotOptions
    SpotOptions (..),
    mkSpotOptions,
    soAllocationStrategy,
    soInstanceInterruptionBehavior,
    soInstancePoolsToUseCount,
    soMaintenanceStrategies,
    soMaxTotalPrice,
    soMinTargetCapacity,
    soSingleAvailabilityZone,
    soSingleInstanceType,

    -- * VpcId
    VpcId (..),

    -- * AvailabilityZoneState
    AvailabilityZoneState (..),

    -- * AllowsMultipleInstanceTypes
    AllowsMultipleInstanceTypes (..),

    -- * InstanceMetadataEndpointState
    InstanceMetadataEndpointState (..),

    -- * ElasticInferenceAcceleratorAssociation
    ElasticInferenceAcceleratorAssociation (..),
    mkElasticInferenceAcceleratorAssociation,
    eiaaElasticInferenceAcceleratorArn,
    eiaaElasticInferenceAcceleratorAssociationId,
    eiaaElasticInferenceAcceleratorAssociationState,
    eiaaElasticInferenceAcceleratorAssociationTime,

    -- * SpotInstanceRequest
    SpotInstanceRequest (..),
    mkSpotInstanceRequest,
    sirActualBlockHourlyPrice,
    sirAvailabilityZoneGroup,
    sirBlockDurationMinutes,
    sirCreateTime,
    sirFault,
    sirInstanceId,
    sirInstanceInterruptionBehavior,
    sirLaunchGroup,
    sirLaunchSpecification,
    sirLaunchedAvailabilityZone,
    sirProductDescription,
    sirSpotInstanceRequestId,
    sirSpotPrice,
    sirState,
    sirStatus,
    sirTags,
    sirType,
    sirValidFrom,
    sirValidUntil,

    -- * LaunchTemplateId
    LaunchTemplateId (..),

    -- * AssociationStatusCode
    AssociationStatusCode (..),

    -- * RemovePrefixListEntry
    RemovePrefixListEntry (..),
    mkRemovePrefixListEntry,
    rpleCidr,

    -- * PeeringTgwInfo
    PeeringTgwInfo (..),
    mkPeeringTgwInfo,
    ptiOwnerId,
    ptiRegion,
    ptiTransitGatewayId,

    -- * ScheduledInstancesNetworkInterface
    ScheduledInstancesNetworkInterface (..),
    mkScheduledInstancesNetworkInterface,
    siniAssociatePublicIpAddress,
    siniDeleteOnTermination,
    siniDescription,
    siniDeviceIndex,
    siniGroups,
    siniIpv6AddressCount,
    siniIpv6Addresses,
    siniNetworkInterfaceId,
    siniPrivateIpAddress,
    siniPrivateIpAddressConfigs,
    siniSecondaryPrivateIpAddressCount,
    siniSubnetId,

    -- * LaunchSpecification
    LaunchSpecification (..),
    mkLaunchSpecification,
    lsAddressingType,
    lsBlockDeviceMappings,
    lsEbsOptimized,
    lsIamInstanceProfile,
    lsImageId,
    lsInstanceType,
    lsKernelId,
    lsKeyName,
    lsMonitoring,
    lsNetworkInterfaces,
    lsPlacement,
    lsRamdiskId,
    lsSecurityGroups,
    lsSubnetId,
    lsUserData,

    -- * TrafficDirection
    TrafficDirection (..),

    -- * VolumeStatusEvent
    VolumeStatusEvent (..),
    mkVolumeStatusEvent,
    vseDescription,
    vseEventId,
    vseEventType,
    vseInstanceId,
    vseNotAfter,
    vseNotBefore,

    -- * SpotMaintenanceStrategies
    SpotMaintenanceStrategies (..),
    mkSpotMaintenanceStrategies,
    smsCapacityRebalance,

    -- * Volume
    Volume (..),
    mkVolume,
    vAttachments,
    vAvailabilityZone,
    vCreateTime,
    vEncrypted,
    vFastRestored,
    vIops,
    vKmsKeyId,
    vMultiAttachEnabled,
    vOutpostArn,
    vSize,
    vSnapshotId,
    vState,
    vTags,
    vVolumeId,
    vVolumeType,

    -- * Reservation
    Reservation (..),
    mkReservation,
    rGroups,
    rInstances,
    rOwnerId,
    rRequesterId,
    rReservationId,

    -- * TrafficMirrorTargetId
    TrafficMirrorTargetId (..),

    -- * PlacementGroupInfo
    PlacementGroupInfo (..),
    mkPlacementGroupInfo,
    pgiSupportedStrategies,

    -- * FpgaDeviceManufacturerName
    FpgaDeviceManufacturerName (..),

    -- * CidrAuthorizationContext
    CidrAuthorizationContext (..),
    mkCidrAuthorizationContext,
    cacMessage,
    cacSignature,

    -- * ClientVpnRoute
    ClientVpnRoute (..),
    mkClientVpnRoute,
    cvrClientVpnEndpointId,
    cvrDescription,
    cvrDestinationCidr,
    cvrOrigin,
    cvrStatus,
    cvrTargetSubnet,
    cvrType,

    -- * LaunchTemplateInstanceMarketOptions
    LaunchTemplateInstanceMarketOptions (..),
    mkLaunchTemplateInstanceMarketOptions,
    ltimoMarketType,
    ltimoSpotOptions,

    -- * TrafficMirrorFilterRuleField
    TrafficMirrorFilterRuleField (..),

    -- * LoadPermissionModifications
    LoadPermissionModifications (..),
    mkLoadPermissionModifications,
    lpmAdd,
    lpmRemove,

    -- * LaunchTemplateEbsBlockDevice
    LaunchTemplateEbsBlockDevice (..),
    mkLaunchTemplateEbsBlockDevice,
    ltebdDeleteOnTermination,
    ltebdEncrypted,
    ltebdIops,
    ltebdKmsKeyId,
    ltebdSnapshotId,
    ltebdVolumeSize,
    ltebdVolumeType,

    -- * FpgaImageState
    FpgaImageState (..),
    mkFpgaImageState,
    fisCode,
    fisMessage,

    -- * EphemeralNvmeSupport
    EphemeralNvmeSupport (..),

    -- * MulticastSupportValue
    MulticastSupportValue (..),

    -- * ImportInstanceVolumeDetailItem
    ImportInstanceVolumeDetailItem (..),
    mkImportInstanceVolumeDetailItem,
    iivdiAvailabilityZone,
    iivdiBytesConverted,
    iivdiDescription,
    iivdiImage,
    iivdiStatus,
    iivdiStatusMessage,
    iivdiVolume,

    -- * LocalGatewayRouteTableVirtualInterfaceGroupAssociation
    LocalGatewayRouteTableVirtualInterfaceGroupAssociation (..),
    mkLocalGatewayRouteTableVirtualInterfaceGroupAssociation,
    lgrtvigaLocalGatewayId,
    lgrtvigaLocalGatewayRouteTableArn,
    lgrtvigaLocalGatewayRouteTableId,
    lgrtvigaLocalGatewayRouteTableVirtualInterfaceGroupAssociationId,
    lgrtvigaLocalGatewayVirtualInterfaceGroupId,
    lgrtvigaOwnerId,
    lgrtvigaState,
    lgrtvigaTags,

    -- * LocalGatewayId
    LocalGatewayId (..),

    -- * SummaryStatus
    SummaryStatus (..),

    -- * ReservedInstancesModification
    ReservedInstancesModification (..),
    mkReservedInstancesModification,
    rimClientToken,
    rimCreateDate,
    rimEffectiveDate,
    rimModificationResults,
    rimReservedInstancesIds,
    rimReservedInstancesModificationId,
    rimStatus,
    rimStatusMessage,
    rimUpdateDate,

    -- * TransitGatewayAttachmentPropagation
    TransitGatewayAttachmentPropagation (..),
    mkTransitGatewayAttachmentPropagation,
    tgapState,
    tgapTransitGatewayRouteTableId,

    -- * RuleAction
    RuleAction (..),

    -- * CarrierGatewayState
    CarrierGatewayState (..),

    -- * IamInstanceProfileAssociationId
    IamInstanceProfileAssociationId (..),

    -- * BatchState
    BatchState (..),

    -- * DiskInfo
    DiskInfo (..),
    mkDiskInfo,
    diCount,
    diSizeInGB,
    diType,

    -- * LocalGatewayRouteTableVpcAssociationId
    LocalGatewayRouteTableVpcAssociationId (..),

    -- * SubnetCidrAssociationId
    SubnetCidrAssociationId (..),

    -- * ConnectionNotificationType
    ConnectionNotificationType (..),

    -- * NetworkInterface
    NetworkInterface (..),
    mkNetworkInterface,
    niAssociation,
    niAttachment,
    niAvailabilityZone,
    niDescription,
    niGroups,
    niInterfaceType,
    niIpv6Addresses,
    niMacAddress,
    niNetworkInterfaceId,
    niOutpostArn,
    niOwnerId,
    niPrivateDnsName,
    niPrivateIpAddress,
    niPrivateIpAddresses,
    niRequesterId,
    niRequesterManaged,
    niSourceDestCheck,
    niStatus,
    niSubnetId,
    niTagSet,
    niVpcId,

    -- * EbsNvmeSupport
    EbsNvmeSupport (..),

    -- * ClientVpnConnectionStatus
    ClientVpnConnectionStatus (..),
    mkClientVpnConnectionStatus,
    cvcsCode,
    cvcsMessage,

    -- * InferenceDeviceManufacturerName
    InferenceDeviceManufacturerName (..),

    -- * ExportImageTaskId
    ExportImageTaskId (..),

    -- * EnableFastSnapshotRestoreSuccessItem
    EnableFastSnapshotRestoreSuccessItem (..),
    mkEnableFastSnapshotRestoreSuccessItem,
    efsrsiAvailabilityZone,
    efsrsiDisabledTime,
    efsrsiDisablingTime,
    efsrsiEnabledTime,
    efsrsiEnablingTime,
    efsrsiOptimizingTime,
    efsrsiOwnerAlias,
    efsrsiOwnerId,
    efsrsiSnapshotId,
    efsrsiState,
    efsrsiStateTransitionReason,

    -- * TransitAssociationGatewayId
    TransitAssociationGatewayId (..),

    -- * TelemetryStatus
    TelemetryStatus (..),

    -- * LaunchTemplateCpuOptions
    LaunchTemplateCpuOptions (..),
    mkLaunchTemplateCpuOptions,
    ltcoCoreCount,
    ltcoThreadsPerCore,

    -- * FleetSpotCapacityRebalance
    FleetSpotCapacityRebalance (..),
    mkFleetSpotCapacityRebalance,
    fscrReplacementStrategy,

    -- * RouteTableAssociationId
    RouteTableAssociationId (..),

    -- * Subnet
    Subnet (..),
    mkSubnet,
    sfAssignIpv6AddressOnCreation,
    sfAvailabilityZone,
    sfAvailabilityZoneId,
    sfAvailableIpAddressCount,
    sfCidrBlock,
    sfCustomerOwnedIpv4Pool,
    sfDefaultForAz,
    sfIpv6CidrBlockAssociationSet,
    sfMapCustomerOwnedIpOnLaunch,
    sfMapPublicIpOnLaunch,
    sfOutpostArn,
    sfOwnerId,
    sfState,
    sfSubnetArn,
    sfSubnetId,
    sfTags,
    sfVpcId,

    -- * LocalGatewayRouteState
    LocalGatewayRouteState (..),

    -- * TerminateConnectionStatus
    TerminateConnectionStatus (..),
    mkTerminateConnectionStatus,
    tcsConnectionId,
    tcsCurrentStatus,
    tcsPreviousStatus,

    -- * CreateFleetInstance
    CreateFleetInstance (..),
    mkCreateFleetInstance,
    cfiInstanceIds,
    cfiInstanceType,
    cfiLaunchTemplateAndOverrides,
    cfiLifecycle,
    cfiPlatform,

    -- * FpgaImageAttribute
    FpgaImageAttribute (..),
    mkFpgaImageAttribute,
    fiaDescription,
    fiaFpgaImageId,
    fiaLoadPermissions,
    fiaName,
    fiaProductCodes,

    -- * KeyPairInfo
    KeyPairInfo (..),
    mkKeyPairInfo,
    kpiKeyFingerprint,
    kpiKeyName,
    kpiKeyPairId,
    kpiTags,

    -- * LaunchTemplateEnclaveOptions
    LaunchTemplateEnclaveOptions (..),
    mkLaunchTemplateEnclaveOptions,
    lteoEnabled,

    -- * TransitGatewayMulticastDomain
    TransitGatewayMulticastDomain (..),
    mkTransitGatewayMulticastDomain,
    tgmdCreationTime,
    tgmdState,
    tgmdTags,
    tgmdTransitGatewayId,
    tgmdTransitGatewayMulticastDomainId,

    -- * NatGateway
    NatGateway (..),
    mkNatGateway,
    ngCreateTime,
    ngDeleteTime,
    ngFailureCode,
    ngFailureMessage,
    ngNatGatewayAddresses,
    ngNatGatewayId,
    ngProvisionedBandwidth,
    ngState,
    ngSubnetId,
    ngTags,
    ngVpcId,

    -- * LaunchPermissionModifications
    LaunchPermissionModifications (..),
    mkLaunchPermissionModifications,
    lAdd,
    lRemove,

    -- * TrafficMirrorPortRangeRequest
    TrafficMirrorPortRangeRequest (..),
    mkTrafficMirrorPortRangeRequest,
    tmprrFromPort,
    tmprrToPort,

    -- * TransitGatewayVpcAttachmentOptions
    TransitGatewayVpcAttachmentOptions (..),
    mkTransitGatewayVpcAttachmentOptions,
    tgvaoApplianceModeSupport,
    tgvaoDnsSupport,
    tgvaoIpv6Support,

    -- * ElasticGpuHealth
    ElasticGpuHealth (..),
    mkElasticGpuHealth,
    eghStatus,

    -- * LoadBalancersConfig
    LoadBalancersConfig (..),
    mkLoadBalancersConfig,
    lbcClassicLoadBalancersConfig,
    lbcTargetGroupsConfig,

    -- * ExcessCapacityTerminationPolicy
    ExcessCapacityTerminationPolicy (..),

    -- * SnapshotState
    SnapshotState (..),

    -- * InstanceNetworkInterfaceAssociation
    InstanceNetworkInterfaceAssociation (..),
    mkInstanceNetworkInterfaceAssociation,
    iniaCarrierIp,
    iniaIpOwnerId,
    iniaPublicDnsName,
    iniaPublicIp,

    -- * ReservationValue
    ReservationValue (..),
    mkReservationValue,
    rvHourlyPrice,
    rvRemainingTotalValue,
    rvRemainingUpfrontValue,

    -- * FleetReplacementStrategy
    FleetReplacementStrategy (..),

    -- * DiskImageDetail
    DiskImageDetail (..),
    mkDiskImageDetail,
    dBytes,
    dFormat,
    dImportManifestUrl,

    -- * ValidationError
    ValidationError (..),
    mkValidationError,
    veCode,
    veMessage,

    -- * TransitGatewayVpcAttachment
    TransitGatewayVpcAttachment (..),
    mkTransitGatewayVpcAttachment,
    tgvaCreationTime,
    tgvaOptions,
    tgvaState,
    tgvaSubnetIds,
    tgvaTags,
    tgvaTransitGatewayAttachmentId,
    tgvaTransitGatewayId,
    tgvaVpcId,
    tgvaVpcOwnerId,

    -- * SpotAllocationStrategy
    SpotAllocationStrategy (..),

    -- * UnsuccessfulInstanceCreditSpecificationItemError
    UnsuccessfulInstanceCreditSpecificationItemError (..),
    mkUnsuccessfulInstanceCreditSpecificationItemError,
    uicsieCode,
    uicsieMessage,

    -- * InstancePrivateIpAddress
    InstancePrivateIpAddress (..),
    mkInstancePrivateIpAddress,
    ipiaAssociation,
    ipiaPrimary,
    ipiaPrivateDnsName,
    ipiaPrivateIpAddress,

    -- * CancelledSpotInstanceRequest
    CancelledSpotInstanceRequest (..),
    mkCancelledSpotInstanceRequest,
    csirSpotInstanceRequestId,
    csirState,

    -- * VpnConnectionOptionsSpecification
    VpnConnectionOptionsSpecification (..),
    mkVpnConnectionOptionsSpecification,
    vcosEnableAcceleration,
    vcosLocalIpv4NetworkCidr,
    vcosLocalIpv6NetworkCidr,
    vcosRemoteIpv4NetworkCidr,
    vcosRemoteIpv6NetworkCidr,
    vcosStaticRoutesOnly,
    vcosTunnelInsideIpVersion,
    vcosTunnelOptions,

    -- * Address
    Address (..),
    mkAddress,
    aAllocationId,
    aAssociationId,
    aCarrierIp,
    aCustomerOwnedIp,
    aCustomerOwnedIpv4Pool,
    aDomain,
    aInstanceId,
    aNetworkBorderGroup,
    aNetworkInterfaceId,
    aNetworkInterfaceOwnerId,
    aPrivateIpAddress,
    aPublicIp,
    aPublicIpv4Pool,
    aTags,

    -- * NatGatewayId
    NatGatewayId (..),

    -- * FleetType
    FleetType (..),

    -- * TransitGatewayPrefixListReference
    TransitGatewayPrefixListReference (..),
    mkTransitGatewayPrefixListReference,
    tgplrBlackhole,
    tgplrPrefixListId,
    tgplrPrefixListOwnerId,
    tgplrState,
    tgplrTransitGatewayAttachment,
    tgplrTransitGatewayRouteTableId,

    -- * VolumeAttachmentState
    VolumeAttachmentState (..),

    -- * TransitGatewayMulticastDomainId
    TransitGatewayMulticastDomainId (..),

    -- * IamInstanceProfileAssociation
    IamInstanceProfileAssociation (..),
    mkIamInstanceProfileAssociation,
    iipaAssociationId,
    iipaIamInstanceProfile,
    iipaInstanceId,
    iipaState,
    iipaTimestamp,

    -- * EnclaveOptions
    EnclaveOptions (..),
    mkEnclaveOptions,
    eoEnabled,

    -- * PrivateDnsDetails
    PrivateDnsDetails (..),
    mkPrivateDnsDetails,
    pddPrivateDnsName,

    -- * CoipAddressUsage
    CoipAddressUsage (..),
    mkCoipAddressUsage,
    cauAllocationId,
    cauAwsAccountId,
    cauAwsService,
    cauCoIp,

    -- * AssociatedNetworkType
    AssociatedNetworkType (..),

    -- * NetworkInterfaceId
    NetworkInterfaceId (..),

    -- * MovingAddressStatus
    MovingAddressStatus (..),
    mkMovingAddressStatus,
    masMoveStatus,
    masPublicIp,

    -- * LaunchPermission
    LaunchPermission (..),
    mkLaunchPermission,
    lGroup,
    lUserId,

    -- * NetworkPerformance
    NetworkPerformance (..),

    -- * RouteState
    RouteState (..),

    -- * DefaultRouteTableAssociationValue
    DefaultRouteTableAssociationValue (..),

    -- * OfferingClassType
    OfferingClassType (..),

    -- * InterfacePermissionType
    InterfacePermissionType (..),

    -- * SecurityGroupName
    SecurityGroupName (..),

    -- * ExportImageTask
    ExportImageTask (..),
    mkExportImageTask,
    eitDescription,
    eitExportImageTaskId,
    eitImageId,
    eitProgress,
    eitS3ExportLocation,
    eitStatus,
    eitStatusMessage,
    eitTags,

    -- * RouteTableAssociation
    RouteTableAssociation (..),
    mkRouteTableAssociation,
    rtaAssociationState,
    rtaGatewayId,
    rtaMain,
    rtaRouteTableAssociationId,
    rtaRouteTableId,
    rtaSubnetId,

    -- * VpcEndpointType
    VpcEndpointType (..),

    -- * RamdiskId
    RamdiskId (..),

    -- * TransitGatewayRequestOptions
    TransitGatewayRequestOptions (..),
    mkTransitGatewayRequestOptions,
    tgroAmazonSideAsn,
    tgroAutoAcceptSharedAttachments,
    tgroDefaultRouteTableAssociation,
    tgroDefaultRouteTablePropagation,
    tgroDnsSupport,
    tgroMulticastSupport,
    tgroVpnEcmpSupport,

    -- * LocalGatewayRouteTableVpcAssociation
    LocalGatewayRouteTableVpcAssociation (..),
    mkLocalGatewayRouteTableVpcAssociation,
    lgrtvaLocalGatewayId,
    lgrtvaLocalGatewayRouteTableArn,
    lgrtvaLocalGatewayRouteTableId,
    lgrtvaLocalGatewayRouteTableVpcAssociationId,
    lgrtvaOwnerId,
    lgrtvaState,
    lgrtvaTags,
    lgrtvaVpcId,

    -- * FpgaDeviceInfo
    FpgaDeviceInfo (..),
    mkFpgaDeviceInfo,
    fdiCount,
    fdiManufacturer,
    fdiMemoryInfo,
    fdiName,

    -- * CpuOptions
    CpuOptions (..),
    mkCpuOptions,
    coCoreCount,
    coThreadsPerCore,

    -- * VpcEndpointServiceId
    VpcEndpointServiceId (..),

    -- * BundleTaskState
    BundleTaskState (..),

    -- * LaunchTemplateIamInstanceProfileSpecificationRequest
    LaunchTemplateIamInstanceProfileSpecificationRequest (..),
    mkLaunchTemplateIamInstanceProfileSpecificationRequest,
    ltiipsrArn,
    ltiipsrName,

    -- * FleetLaunchTemplateSpecification
    FleetLaunchTemplateSpecification (..),
    mkFleetLaunchTemplateSpecification,
    fltsLaunchTemplateId,
    fltsLaunchTemplateName,
    fltsVersion,

    -- * OnDemandOptions
    OnDemandOptions (..),
    mkOnDemandOptions,
    odoAllocationStrategy,
    odoCapacityReservationOptions,
    odoMaxTotalPrice,
    odoMinTargetCapacity,
    odoSingleAvailabilityZone,
    odoSingleInstanceType,

    -- * TransitGatewayRoute
    TransitGatewayRoute (..),
    mkTransitGatewayRoute,
    tgrDestinationCidrBlock,
    tgrPrefixListId,
    tgrState,
    tgrTransitGatewayAttachments,
    tgrType,

    -- * DeregisterInstanceTagAttributeRequest
    DeregisterInstanceTagAttributeRequest (..),
    mkDeregisterInstanceTagAttributeRequest,
    ditarIncludeAllTagsOfInstance,
    ditarInstanceTagKeys,

    -- * HibernationOptionsRequest
    HibernationOptionsRequest (..),
    mkHibernationOptionsRequest,
    horConfigured,

    -- * PortRange
    PortRange (..),
    mkPortRange,
    prFrom,
    prTo,

    -- * VpcAttributeName
    VpcAttributeName (..),

    -- * TransitGatewayRouteTablePropagation
    TransitGatewayRouteTablePropagation (..),
    mkTransitGatewayRouteTablePropagation,
    tgrtpResourceId,
    tgrtpResourceType,
    tgrtpState,
    tgrtpTransitGatewayAttachmentId,

    -- * IdFormat
    IdFormat (..),
    mkIdFormat,
    ifDeadline,
    ifResource,
    ifUseLongIds,

    -- * SubnetIpv6CidrBlockAssociation
    SubnetIpv6CidrBlockAssociation (..),
    mkSubnetIpv6CidrBlockAssociation,
    sicbaAssociationId,
    sicbaIpv6CidrBlock,
    sicbaIpv6CidrBlockState,

    -- * ReservedInstancesConfiguration
    ReservedInstancesConfiguration (..),
    mkReservedInstancesConfiguration,
    ricAvailabilityZone,
    ricInstanceCount,
    ricInstanceType,
    ricPlatform,
    ricScope,

    -- * EbsOptimizedSupport
    EbsOptimizedSupport (..),

    -- * IKEVersionsRequestListValue
    IKEVersionsRequestListValue (..),
    mkIKEVersionsRequestListValue,
    ikevrlvValue,

    -- * FpgaInfo
    FpgaInfo (..),
    mkFpgaInfo,
    fiFpgas,
    fiTotalFpgaMemoryInMiB,

    -- * PublicIpv4Pool
    PublicIpv4Pool (..),
    mkPublicIpv4Pool,
    pipDescription,
    pipNetworkBorderGroup,
    pipPoolAddressRanges,
    pipPoolId,
    pipTags,
    pipTotalAddressCount,
    pipTotalAvailableAddressCount,

    -- * DefaultTargetCapacityType
    DefaultTargetCapacityType (..),

    -- * DisableFastSnapshotRestoreSuccessItem
    DisableFastSnapshotRestoreSuccessItem (..),
    mkDisableFastSnapshotRestoreSuccessItem,
    dAvailabilityZone,
    dDisabledTime,
    dDisablingTime,
    dEnabledTime,
    dEnablingTime,
    dOptimizingTime,
    dOwnerAlias,
    dOwnerId,
    dSnapshotId,
    dState,
    dStateTransitionReason,

    -- * ResponseLaunchTemplateData
    ResponseLaunchTemplateData (..),
    mkResponseLaunchTemplateData,
    rBlockDeviceMappings,
    rCapacityReservationSpecification,
    rCpuOptions,
    rCreditSpecification,
    rDisableApiTermination,
    rEbsOptimized,
    rElasticGpuSpecifications,
    rElasticInferenceAccelerators,
    rEnclaveOptions,
    rHibernationOptions,
    rIamInstanceProfile,
    rImageId,
    rInstanceInitiatedShutdownBehavior,
    rInstanceMarketOptions,
    rInstanceType,
    rKernelId,
    rKeyName,
    rLicenseSpecifications,
    rMetadataOptions,
    rMonitoring,
    rNetworkInterfaces,
    rPlacement,
    rRamDiskId,
    rSecurityGroupIds,
    rSecurityGroups,
    rTagSpecifications,
    rUserData,

    -- * ElasticGpuAssociation
    ElasticGpuAssociation (..),
    mkElasticGpuAssociation,
    egaElasticGpuAssociationId,
    egaElasticGpuAssociationState,
    egaElasticGpuAssociationTime,
    egaElasticGpuId,

    -- * VpcFlowLogId
    VpcFlowLogId (..),

    -- * VolumeStatusDetails
    VolumeStatusDetails (..),
    mkVolumeStatusDetails,
    vsdName,
    vsdStatus,

    -- * SubnetId
    SubnetId (..),

    -- * SpotInstanceState
    SpotInstanceState (..),

    -- * VpnConnectionOptions
    VpnConnectionOptions (..),
    mkVpnConnectionOptions,
    vcoEnableAcceleration,
    vcoLocalIpv4NetworkCidr,
    vcoLocalIpv6NetworkCidr,
    vcoRemoteIpv4NetworkCidr,
    vcoRemoteIpv6NetworkCidr,
    vcoStaticRoutesOnly,
    vcoTunnelInsideIpVersion,
    vcoTunnelOptions,

    -- * SpotFleetLaunchSpecification
    SpotFleetLaunchSpecification (..),
    mkSpotFleetLaunchSpecification,
    sflsAddressingType,
    sflsBlockDeviceMappings,
    sflsEbsOptimized,
    sflsIamInstanceProfile,
    sflsImageId,
    sflsInstanceType,
    sflsKernelId,
    sflsKeyName,
    sflsMonitoring,
    sflsNetworkInterfaces,
    sflsPlacement,
    sflsRamdiskId,
    sflsSecurityGroups,
    sflsSpotPrice,
    sflsSubnetId,
    sflsTagSpecifications,
    sflsUserData,
    sflsWeightedCapacity,

    -- * ClientVpnConnectionStatusCode
    ClientVpnConnectionStatusCode (..),

    -- * PrefixListEntry
    PrefixListEntry (..),
    mkPrefixListEntry,
    pleCidr,
    pleDescription,

    -- * Phase2IntegrityAlgorithmsRequestListValue
    Phase2IntegrityAlgorithmsRequestListValue (..),
    mkPhase2IntegrityAlgorithmsRequestListValue,
    piarlvValue,

    -- * KernelId
    KernelId (..),

    -- * ClientVpnAuthenticationType
    ClientVpnAuthenticationType (..),

    -- * InstanceMatchCriteria
    InstanceMatchCriteria (..),

    -- * VolumeModificationState
    VolumeModificationState (..),

    -- * UserIdGroupPair
    UserIdGroupPair (..),
    mkUserIdGroupPair,
    uigpDescription,
    uigpGroupId,
    uigpGroupName,
    uigpPeeringStatus,
    uigpUserId,
    uigpVpcId,
    uigpVpcPeeringConnectionId,

    -- * InstanceStatusSummary
    InstanceStatusSummary (..),
    mkInstanceStatusSummary,
    issDetails,
    issStatus,

    -- * InstanceHealthStatus
    InstanceHealthStatus (..),

    -- * SpotPlacement
    SpotPlacement (..),
    mkSpotPlacement,
    spAvailabilityZone,
    spGroupName,
    spTenancy,

    -- * PoolCidrBlock
    PoolCidrBlock (..),
    mkPoolCidrBlock,
    pcbCidr,

    -- * EbsInstanceBlockDeviceSpecification
    EbsInstanceBlockDeviceSpecification (..),
    mkEbsInstanceBlockDeviceSpecification,
    eibdsDeleteOnTermination,
    eibdsVolumeId,

    -- * CapacityReservationId
    CapacityReservationId (..),

    -- * NetworkAclAssociation
    NetworkAclAssociation (..),
    mkNetworkAclAssociation,
    naaNetworkAclAssociationId,
    naaNetworkAclId,
    naaSubnetId,

    -- * DeleteFleetSuccessItem
    DeleteFleetSuccessItem (..),
    mkDeleteFleetSuccessItem,
    dfsiCurrentFleetState,
    dfsiFleetId,
    dfsiPreviousFleetState,

    -- * InstanceTypeOffering
    InstanceTypeOffering (..),
    mkInstanceTypeOffering,
    itoInstanceType,
    itoLocation,
    itoLocationType,

    -- * BundleTask
    BundleTask (..),
    mkBundleTask,
    btBundleId,
    btBundleTaskError,
    btInstanceId,
    btProgress,
    btStartTime,
    btState,
    btStorage,
    btUpdateTime,

    -- * ElasticInferenceAccelerator
    ElasticInferenceAccelerator (..),
    mkElasticInferenceAccelerator,
    eiaType,
    eiaCount,

    -- * InstanceStatusEvent
    InstanceStatusEvent (..),
    mkInstanceStatusEvent,
    iseCode,
    iseDescription,
    iseInstanceEventId,
    iseNotAfter,
    iseNotBefore,
    iseNotBeforeDeadline,

    -- * SubnetCidrBlockState
    SubnetCidrBlockState (..),
    mkSubnetCidrBlockState,
    scbsState,
    scbsStatusMessage,

    -- * InstanceType
    InstanceType (..),

    -- * TrafficMirrorFilterId
    TrafficMirrorFilterId (..),

    -- * HostInstance
    HostInstance (..),
    mkHostInstance,
    hiInstanceId,
    hiInstanceType,
    hiOwnerId,

    -- * CapacityReservationSpecification
    CapacityReservationSpecification (..),
    mkCapacityReservationSpecification,
    crsCapacityReservationPreference,
    crsCapacityReservationTarget,

    -- * CoipPool
    CoipPool (..),
    mkCoipPool,
    cpLocalGatewayRouteTableId,
    cpPoolArn,
    cpPoolCidrs,
    cpPoolId,
    cpTags,

    -- * Route
    Route (..),
    mkRoute,
    rCarrierGatewayId,
    rDestinationCidrBlock,
    rDestinationIpv6CidrBlock,
    rDestinationPrefixListId,
    rEgressOnlyInternetGatewayId,
    rGatewayId,
    rInstanceId,
    rInstanceOwnerId,
    rLocalGatewayId,
    rNatGatewayId,
    rNetworkInterfaceId,
    rOrigin,
    rState,
    rTransitGatewayId,
    rVpcPeeringConnectionId,

    -- * DnsNameState
    DnsNameState (..),

    -- * SpotDatafeedSubscription
    SpotDatafeedSubscription (..),
    mkSpotDatafeedSubscription,
    sdsBucket,
    sdsFault,
    sdsOwnerId,
    sdsPrefix,
    sdsState,

    -- * CreditSpecificationRequest
    CreditSpecificationRequest (..),
    mkCreditSpecificationRequest,
    csrCpuCredits,

    -- * TransitGatewayRouteTableAssociation
    TransitGatewayRouteTableAssociation (..),
    mkTransitGatewayRouteTableAssociation,
    tgrtaResourceId,
    tgrtaResourceType,
    tgrtaState,
    tgrtaTransitGatewayAttachmentId,

    -- * HostProperties
    HostProperties (..),
    mkHostProperties,
    hpCores,
    hpInstanceFamily,
    hpInstanceType,
    hpSockets,
    hpTotalVCpus,

    -- * VolumeStatusAttachmentStatus
    VolumeStatusAttachmentStatus (..),
    mkVolumeStatusAttachmentStatus,
    vsasInstanceId,
    vsasIoPerformance,

    -- * LaunchTemplateBlockDeviceMappingRequest
    LaunchTemplateBlockDeviceMappingRequest (..),
    mkLaunchTemplateBlockDeviceMappingRequest,
    ltbdmrDeviceName,
    ltbdmrEbs,
    ltbdmrNoDevice,
    ltbdmrVirtualName,

    -- * SpotFleetTagSpecification
    SpotFleetTagSpecification (..),
    mkSpotFleetTagSpecification,
    sftsResourceType,
    sftsTags,

    -- * BundleId
    BundleId (..),

    -- * VpcCidrBlockAssociation
    VpcCidrBlockAssociation (..),
    mkVpcCidrBlockAssociation,
    vcbaAssociationId,
    vcbaCidrBlock,
    vcbaCidrBlockState,

    -- * Storage
    Storage (..),
    mkStorage,
    sS3,

    -- * SecurityGroup
    SecurityGroup (..),
    mkSecurityGroup,
    sgDescription,
    sgGroupId,
    sgGroupName,
    sgIpPermissions,
    sgIpPermissionsEgress,
    sgOwnerId,
    sgTags,
    sgVpcId,

    -- * CancelSpotInstanceRequestState
    CancelSpotInstanceRequestState (..),

    -- * FlowLogResourceId
    FlowLogResourceId (..),

    -- * TransitGatewayAttachmentState
    TransitGatewayAttachmentState (..),

    -- * PlacementGroupState
    PlacementGroupState (..),

    -- * ReservedInstancesModificationResult
    ReservedInstancesModificationResult (..),
    mkReservedInstancesModificationResult,
    rimrReservedInstancesId,
    rimrTargetConfiguration,

    -- * InstanceBlockDeviceMappingSpecification
    InstanceBlockDeviceMappingSpecification (..),
    mkInstanceBlockDeviceMappingSpecification,
    ibdmsDeviceName,
    ibdmsEbs,
    ibdmsNoDevice,
    ibdmsVirtualName,

    -- * PublicIpv4PoolRange
    PublicIpv4PoolRange (..),
    mkPublicIpv4PoolRange,
    piprAddressCount,
    piprAvailableAddressCount,
    piprFirstAddress,
    piprLastAddress,

    -- * ExportEnvironment
    ExportEnvironment (..),

    -- * CustomerGatewayId
    CustomerGatewayId (..),

    -- * UserData
    UserData (..),
    mkUserData,
    udData,

    -- * ResetFpgaImageAttributeName
    ResetFpgaImageAttributeName (..),

    -- * FailedQueuedPurchaseDeletion
    FailedQueuedPurchaseDeletion (..),
    mkFailedQueuedPurchaseDeletion,
    fqpdError,
    fqpdReservedInstancesId,

    -- * TransitGatewayAssociationState
    TransitGatewayAssociationState (..),

    -- * VolumeAttachment
    VolumeAttachment (..),
    mkVolumeAttachment,
    vaAttachTime,
    vaDeleteOnTermination,
    vaDevice,
    vaInstanceId,
    vaState,
    vaVolumeId,

    -- * ScheduledInstancesIpv6Address
    ScheduledInstancesIpv6Address (..),
    mkScheduledInstancesIpv6Address,
    siiaIpv6Address,

    -- * FpgaImageAttributeName
    FpgaImageAttributeName (..),

    -- * GpuInfo
    GpuInfo (..),
    mkGpuInfo,
    giGpus,
    giTotalGpuMemoryInMiB,

    -- * SecurityGroupId
    SecurityGroupId (..),

    -- * NatGatewayState
    NatGatewayState (..),

    -- * CustomerGateway
    CustomerGateway (..),
    mkCustomerGateway,
    cBgpAsn,
    cCertificateArn,
    cCustomerGatewayId,
    cDeviceName,
    cIpAddress,
    cState,
    cTags,
    cType,

    -- * TransitGatewayMulticastDomainState
    TransitGatewayMulticastDomainState (..),

    -- * TransitGatewayPeeringAttachment
    TransitGatewayPeeringAttachment (..),
    mkTransitGatewayPeeringAttachment,
    tgpaAccepterTgwInfo,
    tgpaCreationTime,
    tgpaRequesterTgwInfo,
    tgpaState,
    tgpaStatus,
    tgpaTags,
    tgpaTransitGatewayAttachmentId,

    -- * InstanceUsage
    InstanceUsage (..),
    mkInstanceUsage,
    iuAccountId,
    iuUsedInstanceCount,

    -- * EbsInstanceBlockDevice
    EbsInstanceBlockDevice (..),
    mkEbsInstanceBlockDevice,
    eibdAttachTime,
    eibdDeleteOnTermination,
    eibdStatus,
    eibdVolumeId,

    -- * CertificateAuthenticationRequest
    CertificateAuthenticationRequest (..),
    mkCertificateAuthenticationRequest,
    carClientRootCertificateChainArn,

    -- * Ipv6CidrAssociation
    Ipv6CidrAssociation (..),
    mkIpv6CidrAssociation,
    icaAssociatedResource,
    icaIpv6Cidr,

    -- * SubnetCidrBlockStateCode
    SubnetCidrBlockStateCode (..),

    -- * ShutdownBehavior
    ShutdownBehavior (..),

    -- * DiskImageDescription
    DiskImageDescription (..),
    mkDiskImageDescription,
    didChecksum,
    didFormat,
    didImportManifestUrl,
    didSize,

    -- * ElasticGpus
    ElasticGpus (..),
    mkElasticGpus,
    egAvailabilityZone,
    egElasticGpuHealth,
    egElasticGpuId,
    egElasticGpuState,
    egElasticGpuType,
    egInstanceId,
    egTags,

    -- * NextToken
    NextToken (..),

    -- * DiskImageVolumeDescription
    DiskImageVolumeDescription (..),
    mkDiskImageVolumeDescription,
    divdId,
    divdSize,

    -- * Monitoring
    Monitoring (..),
    mkMonitoring,
    mState,

    -- * CapacityReservation
    CapacityReservation (..),
    mkCapacityReservation,
    crAvailabilityZone,
    crAvailabilityZoneId,
    crAvailableInstanceCount,
    crCapacityReservationArn,
    crCapacityReservationId,
    crCreateDate,
    crEbsOptimized,
    crEndDate,
    crEndDateType,
    crEphemeralStorage,
    crInstanceMatchCriteria,
    crInstancePlatform,
    crInstanceType,
    crOwnerId,
    crState,
    crTags,
    crTenancy,
    crTotalInstanceCount,

    -- * SubnetState
    SubnetState (..),

    -- * VersionDescription
    VersionDescription (..),

    -- * CancelSpotFleetRequestsSuccessItem
    CancelSpotFleetRequestsSuccessItem (..),
    mkCancelSpotFleetRequestsSuccessItem,
    csfrsiCurrentSpotFleetRequestState,
    csfrsiPreviousSpotFleetRequestState,
    csfrsiSpotFleetRequestId,

    -- * FederatedAuthentication
    FederatedAuthentication (..),
    mkFederatedAuthentication,
    faSamlProviderArn,
    faSelfServiceSamlProviderArn,

    -- * NetworkCardInfo
    NetworkCardInfo (..),
    mkNetworkCardInfo,
    nciMaximumNetworkInterfaces,
    nciNetworkCardIndex,
    nciNetworkPerformance,

    -- * ContainerFormat
    ContainerFormat (..),

    -- * TrafficMirrorFilter
    TrafficMirrorFilter (..),
    mkTrafficMirrorFilter,
    tmfDescription,
    tmfEgressFilterRules,
    tmfIngressFilterRules,
    tmfNetworkServices,
    tmfTags,
    tmfTrafficMirrorFilterId,

    -- * AvailabilityZoneMessage
    AvailabilityZoneMessage (..),
    mkAvailabilityZoneMessage,
    azmMessage,

    -- * TransitGatewayMulticastGroup
    TransitGatewayMulticastGroup (..),
    mkTransitGatewayMulticastGroup,
    tgmgGroupIpAddress,
    tgmgGroupMember,
    tgmgGroupSource,
    tgmgMemberType,
    tgmgNetworkInterfaceId,
    tgmgResourceId,
    tgmgResourceType,
    tgmgSourceType,
    tgmgSubnetId,
    tgmgTransitGatewayAttachmentId,

    -- * VpcAttachment
    VpcAttachment (..),
    mkVpcAttachment,
    vafState,
    vafVpcId,

    -- * EbsEncryptionSupport
    EbsEncryptionSupport (..),

    -- * ScheduledInstancesIamInstanceProfile
    ScheduledInstancesIamInstanceProfile (..),
    mkScheduledInstancesIamInstanceProfile,
    siiipArn,
    siiipName,

    -- * EventType
    EventType (..),

    -- * LaunchTemplatePlacement
    LaunchTemplatePlacement (..),
    mkLaunchTemplatePlacement,
    ltpAffinity,
    ltpAvailabilityZone,
    ltpGroupName,
    ltpHostId,
    ltpHostResourceGroupArn,
    ltpPartitionNumber,
    ltpSpreadDomain,
    ltpTenancy,

    -- * LocalGatewayRouteType
    LocalGatewayRouteType (..),

    -- * GpuDeviceInfo
    GpuDeviceInfo (..),
    mkGpuDeviceInfo,
    gdiCount,
    gdiManufacturer,
    gdiMemoryInfo,
    gdiName,

    -- * InstanceBlockDeviceMapping
    InstanceBlockDeviceMapping (..),
    mkInstanceBlockDeviceMapping,
    ibdmDeviceName,
    ibdmEbs,

    -- * FleetStateCode
    FleetStateCode (..),

    -- * SpotMarketOptions
    SpotMarketOptions (..),
    mkSpotMarketOptions,
    smoBlockDurationMinutes,
    smoInstanceInterruptionBehavior,
    smoMaxPrice,
    smoSpotInstanceType,
    smoValidUntil,

    -- * CoipPoolId
    CoipPoolId (..),

    -- * ServiceDetail
    ServiceDetail (..),
    mkServiceDetail,
    sdAcceptanceRequired,
    sdAvailabilityZones,
    sdBaseEndpointDnsNames,
    sdManagesVpcEndpoints,
    sdOwner,
    sdPrivateDnsName,
    sdPrivateDnsNameVerificationState,
    sdPrivateDnsNames,
    sdServiceId,
    sdServiceName,
    sdServiceType,
    sdTags,
    sdVpcEndpointPolicySupported,

    -- * Ipv6PoolEc2Id
    Ipv6PoolEc2Id (..),

    -- * InstanceSpecification
    InstanceSpecification (..),
    mkInstanceSpecification,
    isExcludeBootVolume,
    isInstanceId,

    -- * StatusType
    StatusType (..),

    -- * NetworkInterfaceIpv6Address
    NetworkInterfaceIpv6Address (..),
    mkNetworkInterfaceIpv6Address,
    niiaIpv6Address,

    -- * ExportToS3TaskSpecification
    ExportToS3TaskSpecification (..),
    mkExportToS3TaskSpecification,
    etstsContainerFormat,
    etstsDiskImageFormat,
    etstsS3Bucket,
    etstsS3Prefix,

    -- * InstanceMetadataOptionsResponse
    InstanceMetadataOptionsResponse (..),
    mkInstanceMetadataOptionsResponse,
    imorHttpEndpoint,
    imorHttpPutResponseHopLimit,
    imorHttpTokens,
    imorState,

    -- * NetworkInterfacePermissionState
    NetworkInterfacePermissionState (..),
    mkNetworkInterfacePermissionState,
    nipsState,
    nipsStatusMessage,

    -- * FleetSpotMaintenanceStrategiesRequest
    FleetSpotMaintenanceStrategiesRequest (..),
    mkFleetSpotMaintenanceStrategiesRequest,
    fsmsrCapacityRebalance,

    -- * LaunchTemplateIamInstanceProfileSpecification
    LaunchTemplateIamInstanceProfileSpecification (..),
    mkLaunchTemplateIamInstanceProfileSpecification,
    ltiipsArn,
    ltiipsName,

    -- * CancelBatchErrorCode
    CancelBatchErrorCode (..),

    -- * FlowLogsResourceType
    FlowLogsResourceType (..),

    -- * VpcCidrBlockState
    VpcCidrBlockState (..),
    mkVpcCidrBlockState,
    vcbsState,
    vcbsStatusMessage,

    -- * HttpTokensState
    HttpTokensState (..),

    -- * TrafficMirrorFilterRuleId
    TrafficMirrorFilterRuleId (..),

    -- * TagSpecification
    TagSpecification (..),
    mkTagSpecification,
    tsResourceType,
    tsTags,

    -- * AvailabilityZoneOptInStatus
    AvailabilityZoneOptInStatus (..),

    -- * InstanceIpv6AddressRequest
    InstanceIpv6AddressRequest (..),
    mkInstanceIpv6AddressRequest,
    iiarIpv6Address,

    -- * PrefixListId
    PrefixListId (..),
    mkPrefixListId,
    pliDescription,
    pliPrefixListId,

    -- * KmsKeyId
    KmsKeyId (..),

    -- * CopyTagsFromSource
    CopyTagsFromSource (..),

    -- * ResourceArn
    ResourceArn (..),

    -- * NetworkInterfaceAttribute
    NetworkInterfaceAttribute (..),

    -- * Phase2IntegrityAlgorithmsListValue
    Phase2IntegrityAlgorithmsListValue (..),
    mkPhase2IntegrityAlgorithmsListValue,
    pialvfValue,

    -- * LaunchTemplateVersion
    LaunchTemplateVersion (..),
    mkLaunchTemplateVersion,
    ltvCreateTime,
    ltvCreatedBy,
    ltvDefaultVersion,
    ltvLaunchTemplateData,
    ltvLaunchTemplateId,
    ltvLaunchTemplateName,
    ltvVersionDescription,
    ltvVersionNumber,

    -- * ClassicLoadBalancersConfig
    ClassicLoadBalancersConfig (..),
    mkClassicLoadBalancersConfig,
    clbcClassicLoadBalancers,

    -- * TransitGatewayRouteAttachment
    TransitGatewayRouteAttachment (..),
    mkTransitGatewayRouteAttachment,
    tgraResourceId,
    tgraResourceType,
    tgraTransitGatewayAttachmentId,

    -- * ModifyTransitGatewayOptions
    ModifyTransitGatewayOptions (..),
    mkModifyTransitGatewayOptions,
    mtgoAssociationDefaultRouteTableId,
    mtgoAutoAcceptSharedAttachments,
    mtgoDefaultRouteTableAssociation,
    mtgoDefaultRouteTablePropagation,
    mtgoDnsSupport,
    mtgoPropagationDefaultRouteTableId,
    mtgoVpnEcmpSupport,

    -- * ScheduledInstancesMonitoring
    ScheduledInstancesMonitoring (..),
    mkScheduledInstancesMonitoring,
    simEnabled,

    -- * ImageTypeValues
    ImageTypeValues (..),

    -- * InstanceExportDetails
    InstanceExportDetails (..),
    mkInstanceExportDetails,
    iedInstanceId,
    iedTargetEnvironment,

    -- * SnapshotAttributeName
    SnapshotAttributeName (..),

    -- * HibernationOptions
    HibernationOptions (..),
    mkHibernationOptions,
    hoConfigured,

    -- * FpgaImage
    FpgaImage (..),
    mkFpgaImage,
    fiCreateTime,
    fiDataRetentionSupport,
    fiDescription,
    fiFpgaImageGlobalId,
    fiFpgaImageId,
    fiName,
    fiOwnerAlias,
    fiOwnerId,
    fiPciId,
    fiProductCodes,
    fiPublic,
    fiShellVersion,
    fiState,
    fiTags,
    fiUpdateTime,

    -- * FpgaDeviceMemoryInfo
    FpgaDeviceMemoryInfo (..),
    mkFpgaDeviceMemoryInfo,
    fdmiSizeInMiB,

    -- * CapacityReservationPreference
    CapacityReservationPreference (..),

    -- * LocalGatewayRoutetableId
    LocalGatewayRoutetableId (..),

    -- * NetworkInterfaceAttachmentId
    NetworkInterfaceAttachmentId (..),

    -- * IKEVersionsListValue
    IKEVersionsListValue (..),
    mkIKEVersionsListValue,
    ikevlvValue,

    -- * DescribeFastSnapshotRestoreSuccessItem
    DescribeFastSnapshotRestoreSuccessItem (..),
    mkDescribeFastSnapshotRestoreSuccessItem,
    dfsrsiAvailabilityZone,
    dfsrsiDisabledTime,
    dfsrsiDisablingTime,
    dfsrsiEnabledTime,
    dfsrsiEnablingTime,
    dfsrsiOptimizingTime,
    dfsrsiOwnerAlias,
    dfsrsiOwnerId,
    dfsrsiSnapshotId,
    dfsrsiState,
    dfsrsiStateTransitionReason,

    -- * AvailabilityZone
    AvailabilityZone (..),
    mkAvailabilityZone,
    azGroupName,
    azMessages,
    azNetworkBorderGroup,
    azOptInStatus,
    azParentZoneId,
    azParentZoneName,
    azRegionName,
    azState,
    azZoneId,
    azZoneName,
    azZoneType,

    -- * TargetNetwork
    TargetNetwork (..),
    mkTargetNetwork,
    tnAssociationId,
    tnClientVpnEndpointId,
    tnSecurityGroups,
    tnStatus,
    tnTargetNetworkId,
    tnVpcId,

    -- * TransitGatewayAttachmentAssociation
    TransitGatewayAttachmentAssociation (..),
    mkTransitGatewayAttachmentAssociation,
    tgaaState,
    tgaaTransitGatewayRouteTableId,

    -- * DisableFastSnapshotRestoreStateError
    DisableFastSnapshotRestoreStateError (..),
    mkDisableFastSnapshotRestoreStateError,
    dfsrseCode,
    dfsrseMessage,

    -- * InstanceLifecycle
    InstanceLifecycle (..),

    -- * HistoryRecord
    HistoryRecord (..),
    mkHistoryRecord,
    hrEventInformation,
    hrEventType,
    hrTimestamp,

    -- * ClientVpnEndpointAttributeStatusCode
    ClientVpnEndpointAttributeStatusCode (..),

    -- * ImportImageTask
    ImportImageTask (..),
    mkImportImageTask,
    iitArchitecture,
    iitDescription,
    iitEncrypted,
    iitHypervisor,
    iitImageId,
    iitImportTaskId,
    iitKmsKeyId,
    iitLicenseSpecifications,
    iitLicenseType,
    iitPlatform,
    iitProgress,
    iitSnapshotDetails,
    iitStatus,
    iitStatusMessage,
    iitTags,

    -- * LaunchTemplateLicenseConfigurationRequest
    LaunchTemplateLicenseConfigurationRequest (..),
    mkLaunchTemplateLicenseConfigurationRequest,
    ltlcrLicenseConfigurationArn,

    -- * VpnState
    VpnState (..),

    -- * DeleteFleetError
    DeleteFleetError (..),
    mkDeleteFleetError,
    dfeCode,
    dfeMessage,

    -- * RouteTable
    RouteTable (..),
    mkRouteTable,
    rtAssociations,
    rtOwnerId,
    rtPropagatingVgws,
    rtRouteTableId,
    rtRoutes,
    rtTags,
    rtVpcId,

    -- * UserBucket
    UserBucket (..),
    mkUserBucket,
    ubS3Bucket,
    ubS3Key,

    -- * TrafficMirrorSessionId
    TrafficMirrorSessionId (..),

    -- * HypervisorType
    HypervisorType (..),

    -- * TargetGroupsConfig
    TargetGroupsConfig (..),
    mkTargetGroupsConfig,
    tgcTargetGroups,

    -- * AllocationState
    AllocationState (..),

    -- * VpcTenancy
    VpcTenancy (..),

    -- * CancelSpotFleetRequestsErrorItem
    CancelSpotFleetRequestsErrorItem (..),
    mkCancelSpotFleetRequestsErrorItem,
    csfreiError,
    csfreiSpotFleetRequestId,

    -- * InstanceStatusDetails
    InstanceStatusDetails (..),
    mkInstanceStatusDetails,
    isdImpairedSince,
    isdName,
    isdStatus,

    -- * ProvisionedBandwidth
    ProvisionedBandwidth (..),
    mkProvisionedBandwidth,
    pbProvisionTime,
    pbProvisioned,
    pbRequestTime,
    pbRequested,
    pbStatus,

    -- * FleetActivityStatus
    FleetActivityStatus (..),

    -- * ClientVpnEndpointId
    ClientVpnEndpointId (..),

    -- * IamInstanceProfile
    IamInstanceProfile (..),
    mkIamInstanceProfile,
    iipArn,
    iipId,

    -- * ClientConnectOptions
    ClientConnectOptions (..),
    mkClientConnectOptions,
    ccoEnabled,
    ccoLambdaFunctionArn,

    -- * LaunchTemplateConfig
    LaunchTemplateConfig (..),
    mkLaunchTemplateConfig,
    ltcLaunchTemplateSpecification,
    ltcOverrides,

    -- * TrafficMirrorNetworkService
    TrafficMirrorNetworkService (..),

    -- * TransitGatewayRouteTableState
    TransitGatewayRouteTableState (..),

    -- * LaunchTemplateCapacityReservationSpecificationResponse
    LaunchTemplateCapacityReservationSpecificationResponse (..),
    mkLaunchTemplateCapacityReservationSpecificationResponse,
    lCapacityReservationPreference,
    lCapacityReservationTarget,

    -- * UnsuccessfulItem
    UnsuccessfulItem (..),
    mkUnsuccessfulItem,
    uiError,
    uiResourceId,

    -- * ImportImageLicenseConfigurationResponse
    ImportImageLicenseConfigurationResponse (..),
    mkImportImageLicenseConfigurationResponse,
    iLicenseConfigurationArn,

    -- * ElasticGpuSpecification
    ElasticGpuSpecification (..),
    mkElasticGpuSpecification,
    egsType,

    -- * InternetGatewayAttachment
    InternetGatewayAttachment (..),
    mkInternetGatewayAttachment,
    igaState,
    igaVpcId,

    -- * Scope
    Scope (..),

    -- * ImageId
    ImageId (..),

    -- * LicenseConfigurationRequest
    LicenseConfigurationRequest (..),
    mkLicenseConfigurationRequest,
    lcrLicenseConfigurationArn,

    -- * PurchaseRequest
    PurchaseRequest (..),
    mkPurchaseRequest,
    prInstanceCount,
    prPurchaseToken,

    -- * EbsInfo
    EbsInfo (..),
    mkEbsInfo,
    eiEbsOptimizedInfo,
    eiEbsOptimizedSupport,
    eiEncryptionSupport,
    eiNvmeSupport,

    -- * ReservedInstanceState
    ReservedInstanceState (..),

    -- * InstanceAttributeName
    InstanceAttributeName (..),

    -- * IpPermission
    IpPermission (..),
    mkIpPermission,
    ipFromPort,
    ipIpProtocol,
    ipIpRanges,
    ipIpv6Ranges,
    ipPrefixListIds,
    ipToPort,
    ipUserIdGroupPairs,

    -- * TrafficMirrorPortRange
    TrafficMirrorPortRange (..),
    mkTrafficMirrorPortRange,
    tmprFromPort,
    tmprToPort,

    -- * DeleteQueuedReservedInstancesError
    DeleteQueuedReservedInstancesError (..),
    mkDeleteQueuedReservedInstancesError,
    dqrieCode,
    dqrieMessage,

    -- * ConversionTaskState
    ConversionTaskState (..),

    -- * DiskImage
    DiskImage (..),
    mkDiskImage,
    diDescription,
    diImage,
    diVolume,

    -- * ApplianceModeSupportValue
    ApplianceModeSupportValue (..),

    -- * NetworkInterfaceCreationType
    NetworkInterfaceCreationType (..),

    -- * Tenancy
    Tenancy (..),

    -- * ClientCertificateRevocationListStatus
    ClientCertificateRevocationListStatus (..),
    mkClientCertificateRevocationListStatus,
    ccrlsCode,
    ccrlsMessage,

    -- * SecurityGroupReference
    SecurityGroupReference (..),
    mkSecurityGroupReference,
    sgrGroupId,
    sgrReferencingVpcId,
    sgrVpcPeeringConnectionId,

    -- * SpotInstanceInterruptionBehavior
    SpotInstanceInterruptionBehavior (..),

    -- * UnsuccessfulItemError
    UnsuccessfulItemError (..),
    mkUnsuccessfulItemError,
    uieCode,
    uieMessage,

    -- * InferenceAcceleratorInfo
    InferenceAcceleratorInfo (..),
    mkInferenceAcceleratorInfo,
    iaiAccelerators,

    -- * DhcpOptionsId
    DhcpOptionsId (..),

    -- * ClassicLinkDnsSupport
    ClassicLinkDnsSupport (..),
    mkClassicLinkDnsSupport,
    cldsClassicLinkDnsSupported,
    cldsVpcId,

    -- * KeyPairName
    KeyPairName (..),

    -- * EgressOnlyInternetGateway
    EgressOnlyInternetGateway (..),
    mkEgressOnlyInternetGateway,
    eoigAttachments,
    eoigEgressOnlyInternetGatewayId,
    eoigTags,

    -- * EnableFastSnapshotRestoreStateError
    EnableFastSnapshotRestoreStateError (..),
    mkEnableFastSnapshotRestoreStateError,
    efsrseCode,
    efsrseMessage,

    -- * CreateFleetError
    CreateFleetError (..),
    mkCreateFleetError,
    cfeErrorCode,
    cfeErrorMessage,
    cfeLaunchTemplateAndOverrides,
    cfeLifecycle,

    -- * VpcPeeringConnectionStateReason
    VpcPeeringConnectionStateReason (..),
    mkVpcPeeringConnectionStateReason,
    vpcsrCode,
    vpcsrMessage,

    -- * LaunchTemplateHttpTokensState
    LaunchTemplateHttpTokensState (..),

    -- * IamInstanceProfileSpecification
    IamInstanceProfileSpecification (..),
    mkIamInstanceProfileSpecification,
    iipsArn,
    iipsName,

    -- * ImportVolumeTaskDetails
    ImportVolumeTaskDetails (..),
    mkImportVolumeTaskDetails,
    ivtdAvailabilityZone,
    ivtdBytesConverted,
    ivtdDescription,
    ivtdImage,
    ivtdVolume,

    -- * LastError
    LastError (..),
    mkLastError,
    leCode,
    leMessage,

    -- * PlacementStrategy
    PlacementStrategy (..),

    -- * DescribeFleetsInstances
    DescribeFleetsInstances (..),
    mkDescribeFleetsInstances,
    dfiInstanceIds,
    dfiInstanceType,
    dfiLaunchTemplateAndOverrides,
    dfiLifecycle,
    dfiPlatform,

    -- * DnsServersOptionsModifyStructure
    DnsServersOptionsModifyStructure (..),
    mkDnsServersOptionsModifyStructure,
    dsomsCustomDnsServers,
    dsomsEnabled,

    -- * InstanceNetworkInterface
    InstanceNetworkInterface (..),
    mkInstanceNetworkInterface,
    iniAssociation,
    iniAttachment,
    iniDescription,
    iniGroups,
    iniInterfaceType,
    iniIpv6Addresses,
    iniMacAddress,
    iniNetworkInterfaceId,
    iniOwnerId,
    iniPrivateDnsName,
    iniPrivateIpAddress,
    iniPrivateIpAddresses,
    iniSourceDestCheck,
    iniStatus,
    iniSubnetId,
    iniVpcId,

    -- * AssignedPrivateIpAddress
    AssignedPrivateIpAddress (..),
    mkAssignedPrivateIpAddress,
    apiaPrivateIpAddress,

    -- * TargetReservationValue
    TargetReservationValue (..),
    mkTargetReservationValue,
    trvReservationValue,
    trvTargetConfiguration,

    -- * SpotFleetRequestId
    SpotFleetRequestId (..),

    -- * MembershipType
    MembershipType (..),

    -- * VolumeStatusAction
    VolumeStatusAction (..),
    mkVolumeStatusAction,
    vsaCode,
    vsaDescription,
    vsaEventId,
    vsaEventType,

    -- * PlacementResponse
    PlacementResponse (..),
    mkPlacementResponse,
    prGroupName,

    -- * TransitGatewayId
    TransitGatewayId (..),

    -- * VpcPeeringConnectionVpcInfo
    VpcPeeringConnectionVpcInfo (..),
    mkVpcPeeringConnectionVpcInfo,
    vpcviCidrBlock,
    vpcviCidrBlockSet,
    vpcviIpv6CidrBlockSet,
    vpcviOwnerId,
    vpcviPeeringOptions,
    vpcviRegion,
    vpcviVpcId,

    -- * ServiceState
    ServiceState (..),

    -- * TransitGatewayMulticastRegisteredGroupSources
    TransitGatewayMulticastRegisteredGroupSources (..),
    mkTransitGatewayMulticastRegisteredGroupSources,
    tgmrgsGroupIpAddress,
    tgmrgsRegisteredNetworkInterfaceIds,
    tgmrgsTransitGatewayMulticastDomainId,

    -- * FleetEventType
    FleetEventType (..),

    -- * Phase1IntegrityAlgorithmsRequestListValue
    Phase1IntegrityAlgorithmsRequestListValue (..),
    mkPhase1IntegrityAlgorithmsRequestListValue,
    piarlvfValue,

    -- * ReservationId
    ReservationId (..),

    -- * ServiceType
    ServiceType (..),

    -- * ExportTaskS3LocationRequest
    ExportTaskS3LocationRequest (..),
    mkExportTaskS3LocationRequest,
    etslrS3Bucket,
    etslrS3Prefix,

    -- * UserBucketDetails
    UserBucketDetails (..),
    mkUserBucketDetails,
    ubdS3Bucket,
    ubdS3Key,

    -- * VolumeId
    VolumeId (..),

    -- * OfferingId
    OfferingId (..),

    -- * SelfServicePortal
    SelfServicePortal (..),

    -- * DisableFastSnapshotRestoreErrorItem
    DisableFastSnapshotRestoreErrorItem (..),
    mkDisableFastSnapshotRestoreErrorItem,
    dfsreiFastSnapshotRestoreStateErrors,
    dfsreiSnapshotId,

    -- * CidrBlock
    CidrBlock (..),
    mkCidrBlock,
    cbCidrBlock,

    -- * ReservedInstancesModificationId
    ReservedInstancesModificationId (..),

    -- * LaunchTemplate
    LaunchTemplate (..),
    mkLaunchTemplate,
    ltCreateTime,
    ltCreatedBy,
    ltDefaultVersionNumber,
    ltLatestVersionNumber,
    ltLaunchTemplateId,
    ltLaunchTemplateName,
    ltTags,

    -- * ReservedInstanceLimitPrice
    ReservedInstanceLimitPrice (..),
    mkReservedInstanceLimitPrice,
    rilpAmount,
    rilpCurrencyCode,

    -- * DnsEntry
    DnsEntry (..),
    mkDnsEntry,
    deDnsName,
    deHostedZoneId,

    -- * AssociationStatus
    AssociationStatus (..),
    mkAssociationStatus,
    asCode,
    asMessage,

    -- * DiskType
    DiskType (..),

    -- * Vpc
    Vpc (..),
    mkVpc,
    vfCidrBlock,
    vfCidrBlockAssociationSet,
    vfDhcpOptionsId,
    vfInstanceTenancy,
    vfIpv6CidrBlockAssociationSet,
    vfIsDefault,
    vfOwnerId,
    vfState,
    vfTags,
    vfVpcId,

    -- * AuthorizationRule
    AuthorizationRule (..),
    mkAuthorizationRule,
    arAccessAll,
    arClientVpnEndpointId,
    arDescription,
    arDestinationCidr,
    arGroupId,
    arStatus,

    -- * DirectoryServiceAuthenticationRequest
    DirectoryServiceAuthenticationRequest (..),
    mkDirectoryServiceAuthenticationRequest,
    dsarDirectoryId,

    -- * ImageDiskContainer
    ImageDiskContainer (..),
    mkImageDiskContainer,
    idcDescription,
    idcDeviceName,
    idcFormat,
    idcSnapshotId,
    idcUrl,
    idcUserBucket,

    -- * Ipv6Pool
    Ipv6Pool (..),
    mkIpv6Pool,
    ipDescription,
    ipPoolCidrBlocks,
    ipPoolId,
    ipTags,

    -- * OperationType
    OperationType (..),

    -- * LocalGatewayRouteTableVirtualInterfaceGroupAssociationId
    LocalGatewayRouteTableVirtualInterfaceGroupAssociationId (..),

    -- * CertificateAuthentication
    CertificateAuthentication (..),
    mkCertificateAuthentication,
    caClientRootCertificateChain,

    -- * InferenceDeviceInfo
    InferenceDeviceInfo (..),
    mkInferenceDeviceInfo,
    idiCount,
    idiManufacturer,
    idiName,

    -- * ImportTaskId
    ImportTaskId (..),

    -- * OnDemandAllocationStrategy
    OnDemandAllocationStrategy (..),

    -- * TrafficMirrorTarget
    TrafficMirrorTarget (..),
    mkTrafficMirrorTarget,
    tmtDescription,
    tmtNetworkInterfaceId,
    tmtNetworkLoadBalancerArn,
    tmtOwnerId,
    tmtTags,
    tmtTrafficMirrorTargetId,
    tmtType,

    -- * Phase1EncryptionAlgorithmsRequestListValue
    Phase1EncryptionAlgorithmsRequestListValue (..),
    mkPhase1EncryptionAlgorithmsRequestListValue,
    pearlvValue,

    -- * InstanceStatus
    InstanceStatus (..),
    mkInstanceStatus,
    issAvailabilityZone,
    issEvents,
    issInstanceId,
    issInstanceState,
    issInstanceStatus,
    issOutpostArn,
    issSystemStatus,

    -- * TransitGatewayMulitcastDomainAssociationState
    TransitGatewayMulitcastDomainAssociationState (..),

    -- * CapacityReservationOptionsRequest
    CapacityReservationOptionsRequest (..),
    mkCapacityReservationOptionsRequest,
    crorUsageStrategy,

    -- * GpuDeviceMemoryInfo
    GpuDeviceMemoryInfo (..),
    mkGpuDeviceMemoryInfo,
    gdmiSizeInMiB,

    -- * ArchitectureValues
    ArchitectureValues (..),

    -- * ReportInstanceReasonCodes
    ReportInstanceReasonCodes (..),

    -- * LocationType
    LocationType (..),

    -- * PeeringConnectionOptionsRequest
    PeeringConnectionOptionsRequest (..),
    mkPeeringConnectionOptionsRequest,
    pcorAllowDnsResolutionFromRemoteVpc,
    pcorAllowEgressFromLocalClassicLinkToRemoteVpc,
    pcorAllowEgressFromLocalVpcToRemoteClassicLink,

    -- * MoveStatus
    MoveStatus (..),

    -- * EbsBlockDevice
    EbsBlockDevice (..),
    mkEbsBlockDevice,
    ebdDeleteOnTermination,
    ebdEncrypted,
    ebdIops,
    ebdKmsKeyId,
    ebdSnapshotId,
    ebdVolumeSize,
    ebdVolumeType,

    -- * VpnTunnelOptionsSpecification
    VpnTunnelOptionsSpecification (..),
    mkVpnTunnelOptionsSpecification,
    vtosDPDTimeoutAction,
    vtosDPDTimeoutSeconds,
    vtosIKEVersions,
    vtosPhase1DHGroupNumbers,
    vtosPhase1EncryptionAlgorithms,
    vtosPhase1IntegrityAlgorithms,
    vtosPhase1LifetimeSeconds,
    vtosPhase2DHGroupNumbers,
    vtosPhase2EncryptionAlgorithms,
    vtosPhase2IntegrityAlgorithms,
    vtosPhase2LifetimeSeconds,
    vtosPreSharedKey,
    vtosRekeyFuzzPercentage,
    vtosRekeyMarginTimeSeconds,
    vtosReplayWindowSize,
    vtosStartupAction,
    vtosTunnelInsideCidr,
    vtosTunnelInsideIpv6Cidr,

    -- * FleetLaunchTemplateOverridesRequest
    FleetLaunchTemplateOverridesRequest (..),
    mkFleetLaunchTemplateOverridesRequest,
    fltorAvailabilityZone,
    fltorInstanceType,
    fltorMaxPrice,
    fltorPlacement,
    fltorPriority,
    fltorSubnetId,
    fltorWeightedCapacity,

    -- * LocalGateway
    LocalGateway (..),
    mkLocalGateway,
    lgLocalGatewayId,
    lgOutpostArn,
    lgOwnerId,
    lgState,
    lgTags,

    -- * AccountAttribute
    AccountAttribute (..),
    mkAccountAttribute,
    aaAttributeName,
    aaAttributeValues,

    -- * FpgaImageStateCode
    FpgaImageStateCode (..),

    -- * Ipv6SupportValue
    Ipv6SupportValue (..),

    -- * Phase1DHGroupNumbersRequestListValue
    Phase1DHGroupNumbersRequestListValue (..),
    mkPhase1DHGroupNumbersRequestListValue,
    pdhgnrlvValue,

    -- * SnapshotDetail
    SnapshotDetail (..),
    mkSnapshotDetail,
    sdDescription,
    sdDeviceName,
    sdDiskImageSize,
    sdFormat,
    sdProgress,
    sdSnapshotId,
    sdStatus,
    sdStatusMessage,
    sdUrl,
    sdUserBucket,

    -- * SpotInstanceRequestId
    SpotInstanceRequestId (..),

    -- * ScheduledInstanceRecurrenceRequest
    ScheduledInstanceRecurrenceRequest (..),
    mkScheduledInstanceRecurrenceRequest,
    sirrFrequency,
    sirrInterval,
    sirrOccurrenceDays,
    sirrOccurrenceRelativeToEnd,
    sirrOccurrenceUnit,

    -- * VpcEndpointConnection
    VpcEndpointConnection (..),
    mkVpcEndpointConnection,
    vecCreationTimestamp,
    vecDnsEntries,
    vecGatewayLoadBalancerArns,
    vecNetworkLoadBalancerArns,
    vecServiceId,
    vecVpcEndpointId,
    vecVpcEndpointOwner,
    vecVpcEndpointState,

    -- * PriceSchedule
    PriceSchedule (..),
    mkPriceSchedule,
    psActive,
    psCurrencyCode,
    psPrice,
    psTerm,

    -- * DeviceType
    DeviceType (..),

    -- * DomainType
    DomainType (..),

    -- * RegionInfo
    RegionInfo (..),
    mkRegionInfo,
    riEndpoint,
    riOptInStatus,
    riRegionName,

    -- * TransitGatewayState
    TransitGatewayState (..),

    -- * CreditSpecification
    CreditSpecification (..),
    mkCreditSpecification,
    csCpuCredits,

    -- * DeleteLaunchTemplateVersionsResponseSuccessItem
    DeleteLaunchTemplateVersionsResponseSuccessItem (..),
    mkDeleteLaunchTemplateVersionsResponseSuccessItem,
    dltvrsiLaunchTemplateId,
    dltvrsiLaunchTemplateName,
    dltvrsiVersionNumber,

    -- * TransitGatewayRouteTable
    TransitGatewayRouteTable (..),
    mkTransitGatewayRouteTable,
    tgrtCreationTime,
    tgrtDefaultAssociationRouteTable,
    tgrtDefaultPropagationRouteTable,
    tgrtState,
    tgrtTags,
    tgrtTransitGatewayId,
    tgrtTransitGatewayRouteTableId,

    -- * ImportImageLicenseConfigurationRequest
    ImportImageLicenseConfigurationRequest (..),
    mkImportImageLicenseConfigurationRequest,
    iilcrLicenseConfigurationArn,

    -- * NetworkInfo
    NetworkInfo (..),
    mkNetworkInfo,
    niDefaultNetworkCardIndex,
    niEfaSupported,
    niEnaSupport,
    niIpv4AddressesPerInterface,
    niIpv6AddressesPerInterface,
    niIpv6Supported,
    niMaximumNetworkCards,
    niMaximumNetworkInterfaces,
    niNetworkCards,
    niNetworkPerformance,

    -- * LaunchTemplateSpecification
    LaunchTemplateSpecification (..),
    mkLaunchTemplateSpecification,
    ltsLaunchTemplateId,
    ltsLaunchTemplateName,
    ltsVersion,

    -- * LaunchTemplateBlockDeviceMapping
    LaunchTemplateBlockDeviceMapping (..),
    mkLaunchTemplateBlockDeviceMapping,
    ltbdmDeviceName,
    ltbdmEbs,
    ltbdmNoDevice,
    ltbdmVirtualName,

    -- * InstanceEventId
    InstanceEventId (..),

    -- * InstanceCreditSpecificationRequest
    InstanceCreditSpecificationRequest (..),
    mkInstanceCreditSpecificationRequest,
    icsrCpuCredits,
    icsrInstanceId,

    -- * Host
    Host (..),
    mkHost,
    hAllocationTime,
    hAllowsMultipleInstanceTypes,
    hAutoPlacement,
    hAvailabilityZone,
    hAvailabilityZoneId,
    hAvailableCapacity,
    hClientToken,
    hHostId,
    hHostProperties,
    hHostRecovery,
    hHostReservationId,
    hInstances,
    hMemberOfServiceLinkedResourceGroup,
    hOwnerId,
    hReleaseTime,
    hState,
    hTags,

    -- * PropagatingVgw
    PropagatingVgw (..),
    mkPropagatingVgw,
    pvGatewayId,

    -- * ProcessorInfo
    ProcessorInfo (..),
    mkProcessorInfo,
    piSupportedArchitectures,
    piSustainedClockSpeedInGhz,

    -- * ClientVpnAuthenticationRequest
    ClientVpnAuthenticationRequest (..),
    mkClientVpnAuthenticationRequest,
    cvarActiveDirectory,
    cvarFederatedAuthentication,
    cvarMutualAuthentication,
    cvarType,

    -- * OfferingTypeValues
    OfferingTypeValues (..),

    -- * TargetConfigurationRequest
    TargetConfigurationRequest (..),
    mkTargetConfigurationRequest,
    tcrOfferingId,
    tcrInstanceCount,

    -- * ScheduledInstance
    ScheduledInstance (..),
    mkScheduledInstance,
    siAvailabilityZone,
    siCreateDate,
    siHourlyPrice,
    siInstanceCount,
    siInstanceType,
    siNetworkPlatform,
    siNextSlotStartTime,
    siPlatform,
    siPreviousSlotEndTime,
    siRecurrence,
    siScheduledInstanceId,
    siSlotDurationInHours,
    siTermEndDate,
    siTermStartDate,
    siTotalScheduledInstanceHours,

    -- * HostReservation
    HostReservation (..),
    mkHostReservation,
    hrCount,
    hrCurrencyCode,
    hrDuration,
    hrEnd,
    hrHostIdSet,
    hrHostReservationId,
    hrHourlyPrice,
    hrInstanceFamily,
    hrOfferingId,
    hrPaymentOption,
    hrStart,
    hrState,
    hrTags,
    hrUpfrontPrice,

    -- * NetworkInterfacePermission
    NetworkInterfacePermission (..),
    mkNetworkInterfacePermission,
    nipAwsAccountId,
    nipAwsService,
    nipNetworkInterfaceId,
    nipNetworkInterfacePermissionId,
    nipPermission,
    nipPermissionState,

    -- * FleetLaunchTemplateConfigRequest
    FleetLaunchTemplateConfigRequest (..),
    mkFleetLaunchTemplateConfigRequest,
    fltcrLaunchTemplateSpecification,
    fltcrOverrides,

    -- * VpnGateway
    VpnGateway (..),
    mkVpnGateway,
    vgAmazonSideAsn,
    vgAvailabilityZone,
    vgState,
    vgTags,
    vgType,
    vgVpcAttachments,
    vgVpnGatewayId,

    -- * EventInformation
    EventInformation (..),
    mkEventInformation,
    eiEventDescription,
    eiEventSubType,
    eiInstanceId,

    -- * Filter
    Filter (..),
    mkFilter,
    fName,
    fValues,

    -- * LaunchTemplateErrorCode
    LaunchTemplateErrorCode (..),

    -- * LaunchTemplateSpotMarketOptionsRequest
    LaunchTemplateSpotMarketOptionsRequest (..),
    mkLaunchTemplateSpotMarketOptionsRequest,
    ltsmorBlockDurationMinutes,
    ltsmorInstanceInterruptionBehavior,
    ltsmorMaxPrice,
    ltsmorSpotInstanceType,
    ltsmorValidUntil,

    -- * VolumeType
    VolumeType (..),

    -- * ConversionTaskId
    ConversionTaskId (..),

    -- * InstanceStateChange
    InstanceStateChange (..),
    mkInstanceStateChange,
    iscCurrentState,
    iscInstanceId,
    iscPreviousState,

    -- * NetworkAcl
    NetworkAcl (..),
    mkNetworkAcl,
    naAssociations,
    naEntries,
    naIsDefault,
    naNetworkAclId,
    naOwnerId,
    naTags,
    naVpcId,

    -- * LogDestinationType
    LogDestinationType (..),

    -- * ImageState
    ImageState (..),

    -- * HostOffering
    HostOffering (..),
    mkHostOffering,
    hoCurrencyCode,
    hoDuration,
    hoHourlyPrice,
    hoInstanceFamily,
    hoOfferingId,
    hoPaymentOption,
    hoUpfrontPrice,

    -- * SuccessfulInstanceCreditSpecificationItem
    SuccessfulInstanceCreditSpecificationItem (..),
    mkSuccessfulInstanceCreditSpecificationItem,
    sicsiInstanceId,

    -- * TaggableResourceId
    TaggableResourceId (..),

    -- * ClientVpnAuthorizationRuleStatus
    ClientVpnAuthorizationRuleStatus (..),
    mkClientVpnAuthorizationRuleStatus,
    cvarsCode,
    cvarsMessage,

    -- * GatewayType
    GatewayType (..),

    -- * KeyPairId
    KeyPairId (..),

    -- * LaunchTemplateInstanceMetadataOptionsRequest
    LaunchTemplateInstanceMetadataOptionsRequest (..),
    mkLaunchTemplateInstanceMetadataOptionsRequest,
    ltimorHttpEndpoint,
    ltimorHttpPutResponseHopLimit,
    ltimorHttpTokens,

    -- * UnsuccessfulInstanceCreditSpecificationErrorCode
    UnsuccessfulInstanceCreditSpecificationErrorCode (..),

    -- * InstanceNetworkInterfaceAttachment
    InstanceNetworkInterfaceAttachment (..),
    mkInstanceNetworkInterfaceAttachment,
    iniaAttachTime,
    iniaAttachmentId,
    iniaDeleteOnTermination,
    iniaDeviceIndex,
    iniaNetworkCardIndex,
    iniaStatus,

    -- * UnsuccessfulInstanceCreditSpecificationItem
    UnsuccessfulInstanceCreditSpecificationItem (..),
    mkUnsuccessfulInstanceCreditSpecificationItem,
    uicsiError,
    uicsiInstanceId,

    -- * EndDateType
    EndDateType (..),

    -- * VpnConnectionId
    VpnConnectionId (..),

    -- * AttributeBooleanValue
    AttributeBooleanValue (..),
    mkAttributeBooleanValue,
    abvValue,

    -- * NetworkInterfaceType
    NetworkInterfaceType (..),

    -- * TunnelOption
    TunnelOption (..),
    mkTunnelOption,
    toDpdTimeoutAction,
    toDpdTimeoutSeconds,
    toIkeVersions,
    toOutsideIpAddress,
    toPhase1DHGroupNumbers,
    toPhase1EncryptionAlgorithms,
    toPhase1IntegrityAlgorithms,
    toPhase1LifetimeSeconds,
    toPhase2DHGroupNumbers,
    toPhase2EncryptionAlgorithms,
    toPhase2IntegrityAlgorithms,
    toPhase2LifetimeSeconds,
    toPreSharedKey,
    toRekeyFuzzPercentage,
    toRekeyMarginTimeSeconds,
    toReplayWindowSize,
    toStartupAction,
    toTunnelInsideCidr,
    toTunnelInsideIpv6Cidr,

    -- * DeleteLaunchTemplateVersionsResponseErrorItem
    DeleteLaunchTemplateVersionsResponseErrorItem (..),
    mkDeleteLaunchTemplateVersionsResponseErrorItem,
    dltvreiLaunchTemplateId,
    dltvreiLaunchTemplateName,
    dltvreiResponseError,
    dltvreiVersionNumber,

    -- * RecurringCharge
    RecurringCharge (..),
    mkRecurringCharge,
    rcAmount,
    rcFrequency,

    -- * ScheduledInstancesLaunchSpecification
    ScheduledInstancesLaunchSpecification (..),
    mkScheduledInstancesLaunchSpecification,
    silsImageId,
    silsBlockDeviceMappings,
    silsEbsOptimized,
    silsIamInstanceProfile,
    silsInstanceType,
    silsKernelId,
    silsKeyName,
    silsMonitoring,
    silsNetworkInterfaces,
    silsPlacement,
    silsRamdiskId,
    silsSecurityGroupIds,
    silsSubnetId,
    silsUserData,

    -- * ConnectionNotification
    ConnectionNotification (..),
    mkConnectionNotification,
    cnConnectionEvents,
    cnConnectionNotificationArn,
    cnConnectionNotificationId,
    cnConnectionNotificationState,
    cnConnectionNotificationType,
    cnServiceId,
    cnVpcEndpointId,

    -- * LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
    LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (..),
    mkLaunchTemplateInstanceNetworkInterfaceSpecificationRequest,
    ltinisrAssociateCarrierIpAddress,
    ltinisrAssociatePublicIpAddress,
    ltinisrDeleteOnTermination,
    ltinisrDescription,
    ltinisrDeviceIndex,
    ltinisrGroups,
    ltinisrInterfaceType,
    ltinisrIpv6AddressCount,
    ltinisrIpv6Addresses,
    ltinisrNetworkCardIndex,
    ltinisrNetworkInterfaceId,
    ltinisrPrivateIpAddress,
    ltinisrPrivateIpAddresses,
    ltinisrSecondaryPrivateIpAddressCount,
    ltinisrSubnetId,

    -- * UsageClassType
    UsageClassType (..),

    -- * LocalGatewayVirtualInterfaceGroupId
    LocalGatewayVirtualInterfaceGroupId (..),

    -- * LaunchTemplatesMonitoringRequest
    LaunchTemplatesMonitoringRequest (..),
    mkLaunchTemplatesMonitoringRequest,
    ltmrEnabled,

    -- * NewDhcpConfiguration
    NewDhcpConfiguration (..),
    mkNewDhcpConfiguration,
    ndcKey,
    ndcValues,

    -- * InstanceMetadataOptionsState
    InstanceMetadataOptionsState (..),

    -- * StateReason
    StateReason (..),
    mkStateReason,
    srCode,
    srMessage,

    -- * CapacityReservationState
    CapacityReservationState (..),

    -- * MonitoringState
    MonitoringState (..),

    -- * ElasticGpuStatus
    ElasticGpuStatus (..),

    -- * LicenseConfiguration
    LicenseConfiguration (..),
    mkLicenseConfiguration,
    lcLicenseConfigurationArn,

    -- * HostRecovery
    HostRecovery (..),

    -- * FleetOnDemandAllocationStrategy
    FleetOnDemandAllocationStrategy (..),

    -- * SlotDateTimeRangeRequest
    SlotDateTimeRangeRequest (..),
    mkSlotDateTimeRangeRequest,
    sdtrrEarliestTime,
    sdtrrLatestTime,

    -- * Purchase
    Purchase (..),
    mkPurchase,
    pCurrencyCode,
    pDuration,
    pHostIdSet,
    pHostReservationId,
    pHourlyPrice,
    pInstanceFamily,
    pPaymentOption,
    pUpfrontPrice,

    -- * ScheduledInstancesPrivateIpAddressConfig
    ScheduledInstancesPrivateIpAddressConfig (..),
    mkScheduledInstancesPrivateIpAddressConfig,
    sipiacPrimary,
    sipiacPrivateIpAddress,

    -- * LaunchTemplateHibernationOptionsRequest
    LaunchTemplateHibernationOptionsRequest (..),
    mkLaunchTemplateHibernationOptionsRequest,
    lthorConfigured,

    -- * TransitGatewayAttachmentId
    TransitGatewayAttachmentId (..),

    -- * PrefixListAssociation
    PrefixListAssociation (..),
    mkPrefixListAssociation,
    plaResourceId,
    plaResourceOwner,

    -- * FleetId
    FleetId (..),

    -- * ReservedInstancesId
    ReservedInstancesId (..),
    mkReservedInstancesId,
    riiReservedInstancesId,

    -- * StatusName
    StatusName (..),

    -- * InternetGateway
    InternetGateway (..),
    mkInternetGateway,
    igAttachments,
    igInternetGatewayId,
    igOwnerId,
    igTags,

    -- * PlacementGroupId
    PlacementGroupId (..),

    -- * HistoryRecordEntry
    HistoryRecordEntry (..),
    mkHistoryRecordEntry,
    hreEventInformation,
    hreEventType,
    hreTimestamp,

    -- * ResponseError
    ResponseError (..),
    mkResponseError,
    reCode,
    reMessage,

    -- * VolumeStatusName
    VolumeStatusName (..),

    -- * ReservedInstancesOfferingId
    ReservedInstancesOfferingId (..),

    -- * AllocationStrategy
    AllocationStrategy (..),

    -- * DescribeFleetError
    DescribeFleetError (..),
    mkDescribeFleetError,
    dfeErrorCode,
    dfeErrorMessage,
    dfeLaunchTemplateAndOverrides,
    dfeLifecycle,

    -- * CapacityReservationGroup
    CapacityReservationGroup (..),
    mkCapacityReservationGroup,
    crgGroupArn,
    crgOwnerId,

    -- * VolumeAttributeName
    VolumeAttributeName (..),

    -- * VpcEndpointId
    VpcEndpointId (..),

    -- * TransitGatewayMulticastDeregisteredGroupSources
    TransitGatewayMulticastDeregisteredGroupSources (..),
    mkTransitGatewayMulticastDeregisteredGroupSources,
    tgmdgsDeregisteredNetworkInterfaceIds,
    tgmdgsGroupIpAddress,
    tgmdgsTransitGatewayMulticastDomainId,

    -- * LaunchTemplateTagSpecificationRequest
    LaunchTemplateTagSpecificationRequest (..),
    mkLaunchTemplateTagSpecificationRequest,
    lttsrResourceType,
    lttsrTags,

    -- * PrivateDnsNameConfiguration
    PrivateDnsNameConfiguration (..),
    mkPrivateDnsNameConfiguration,
    pdncName,
    pdncState,
    pdncType,
    pdncValue,

    -- * ImportInstanceTaskDetails
    ImportInstanceTaskDetails (..),
    mkImportInstanceTaskDetails,
    iitdDescription,
    iitdInstanceId,
    iitdPlatform,
    iitdVolumes,

    -- * ClientVpnEndpointStatusCode
    ClientVpnEndpointStatusCode (..),

    -- * PlacementGroup
    PlacementGroup (..),
    mkPlacementGroup,
    pgGroupId,
    pgGroupName,
    pgPartitionCount,
    pgState,
    pgStrategy,
    pgTags,

    -- * UnlimitedSupportedInstanceFamily
    UnlimitedSupportedInstanceFamily (..),

    -- * AutoPlacement
    AutoPlacement (..),

    -- * RootDeviceType
    RootDeviceType (..),

    -- * ByoipCidrState
    ByoipCidrState (..),

    -- * VolumeModification
    VolumeModification (..),
    mkVolumeModification,
    vmEndTime,
    vmModificationState,
    vmOriginalIops,
    vmOriginalSize,
    vmOriginalVolumeType,
    vmProgress,
    vmStartTime,
    vmStatusMessage,
    vmTargetIops,
    vmTargetSize,
    vmTargetVolumeType,
    vmVolumeId,

    -- * TransitGatewayRouteType
    TransitGatewayRouteType (..),

    -- * FleetSpotMaintenanceStrategies
    FleetSpotMaintenanceStrategies (..),
    mkFleetSpotMaintenanceStrategies,
    fsmsCapacityRebalance,

    -- * ProductCode
    ProductCode (..),
    mkProductCode,
    pcProductCodeId,
    pcProductCodeType,

    -- * TransitGatewayMulticastDeregisteredGroupMembers
    TransitGatewayMulticastDeregisteredGroupMembers (..),
    mkTransitGatewayMulticastDeregisteredGroupMembers,
    tgmdgmDeregisteredNetworkInterfaceIds,
    tgmdgmGroupIpAddress,
    tgmdgmTransitGatewayMulticastDomainId,

    -- * FlowLog
    FlowLog (..),
    mkFlowLog,
    flCreationTime,
    flDeliverLogsErrorMessage,
    flDeliverLogsPermissionArn,
    flDeliverLogsStatus,
    flFlowLogId,
    flFlowLogStatus,
    flLogDestination,
    flLogDestinationType,
    flLogFormat,
    flLogGroupName,
    flMaxAggregationInterval,
    flResourceId,
    flTags,
    flTrafficType,

    -- * VCpuInfo
    VCpuInfo (..),
    mkVCpuInfo,
    vciDefaultCores,
    vciDefaultThreadsPerCore,
    vciDefaultVCpus,
    vciValidCores,
    vciValidThreadsPerCore,

    -- * StaleSecurityGroup
    StaleSecurityGroup (..),
    mkStaleSecurityGroup,
    ssgDescription,
    ssgGroupId,
    ssgGroupName,
    ssgStaleIpPermissions,
    ssgStaleIpPermissionsEgress,
    ssgVpcId,

    -- * LaunchTemplateLicenseConfiguration
    LaunchTemplateLicenseConfiguration (..),
    mkLaunchTemplateLicenseConfiguration,
    ltlcLicenseConfigurationArn,

    -- * ListingStatus
    ListingStatus (..),

    -- * CreateTransitGatewayVpcAttachmentRequestOptions
    CreateTransitGatewayVpcAttachmentRequestOptions (..),
    mkCreateTransitGatewayVpcAttachmentRequestOptions,
    ctgvaroApplianceModeSupport,
    ctgvaroDnsSupport,
    ctgvaroIpv6Support,

    -- * Ipv4PoolEc2Id
    Ipv4PoolEc2Id (..),

    -- * Phase2EncryptionAlgorithmsRequestListValue
    Phase2EncryptionAlgorithmsRequestListValue (..),
    mkPhase2EncryptionAlgorithmsRequestListValue,
    pearlvfValue,

    -- * IpRange
    IpRange (..),
    mkIpRange,
    iCidrIp,
    iDescription,

    -- * LocalGatewayVirtualInterfaceGroup
    LocalGatewayVirtualInterfaceGroup (..),
    mkLocalGatewayVirtualInterfaceGroup,
    lgvigLocalGatewayId,
    lgvigLocalGatewayVirtualInterfaceGroupId,
    lgvigLocalGatewayVirtualInterfaceIds,
    lgvigOwnerId,
    lgvigTags,

    -- * VolumeStatusInfoStatus
    VolumeStatusInfoStatus (..),

    -- * AccountAttributeValue
    AccountAttributeValue (..),
    mkAccountAttributeValue,
    aavAttributeValue,

    -- * TransitGatewayAttachment
    TransitGatewayAttachment (..),
    mkTransitGatewayAttachment,
    tgaAssociation,
    tgaCreationTime,
    tgaResourceId,
    tgaResourceOwnerId,
    tgaResourceType,
    tgaState,
    tgaTags,
    tgaTransitGatewayAttachmentId,
    tgaTransitGatewayId,
    tgaTransitGatewayOwnerId,

    -- * SnapshotDiskContainer
    SnapshotDiskContainer (..),
    mkSnapshotDiskContainer,
    sdcDescription,
    sdcFormat,
    sdcUrl,
    sdcUserBucket,

    -- * Ipv6Range
    Ipv6Range (..),
    mkIpv6Range,
    irCidrIpv6,
    irDescription,

    -- * ReservedInstanceReservationValue
    ReservedInstanceReservationValue (..),
    mkReservedInstanceReservationValue,
    rirvReservationValue,
    rirvReservedInstanceId,

    -- * ConnectionNotificationId
    ConnectionNotificationId (..),

    -- * LaunchTemplateInstanceMetadataOptionsState
    LaunchTemplateInstanceMetadataOptionsState (..),

    -- * InstanceStorageInfo
    InstanceStorageInfo (..),
    mkInstanceStorageInfo,
    isiDisks,
    isiNvmeSupport,
    isiTotalSizeInGB,

    -- * FastSnapshotRestoreStateCode
    FastSnapshotRestoreStateCode (..),

    -- * ClientVpnEndpointStatus
    ClientVpnEndpointStatus (..),
    mkClientVpnEndpointStatus,
    cvesCode,
    cvesMessage,

    -- * RIProductDescription
    RIProductDescription (..),

    -- * ReservedInstancesOffering
    ReservedInstancesOffering (..),
    mkReservedInstancesOffering,
    rioAvailabilityZone,
    rioCurrencyCode,
    rioDuration,
    rioFixedPrice,
    rioInstanceTenancy,
    rioInstanceType,
    rioMarketplace,
    rioOfferingClass,
    rioOfferingType,
    rioPricingDetails,
    rioProductDescription,
    rioRecurringCharges,
    rioReservedInstancesOfferingId,
    rioScope,
    rioUsagePrice,

    -- * TransitGatewayAssociation
    TransitGatewayAssociation (..),
    mkTransitGatewayAssociation,
    tResourceId,
    tResourceType,
    tState,
    tTransitGatewayAttachmentId,
    tTransitGatewayRouteTableId,

    -- * InstanceTypeHypervisor
    InstanceTypeHypervisor (..),

    -- * VpnProtocol
    VpnProtocol (..),

    -- * ClassicLoadBalancer
    ClassicLoadBalancer (..),
    mkClassicLoadBalancer,
    clbName,

    -- * InstanceIpv6Address
    InstanceIpv6Address (..),
    mkInstanceIpv6Address,
    iiaIpv6Address,

    -- * Phase2DHGroupNumbersRequestListValue
    Phase2DHGroupNumbersRequestListValue (..),
    mkPhase2DHGroupNumbersRequestListValue,
    pdhgnrlvfValue,

    -- * ReservedInstances
    ReservedInstances (..),
    mkReservedInstances,
    riAvailabilityZone,
    riCurrencyCode,
    riDuration,
    riEnd,
    riFixedPrice,
    riInstanceCount,
    riInstanceTenancy,
    riInstanceType,
    riOfferingClass,
    riOfferingType,
    riProductDescription,
    riRecurringCharges,
    riReservedInstancesId,
    riScope,
    riStart,
    riState,
    riTags,
    riUsagePrice,

    -- * FleetData
    FleetData (..),
    mkFleetData,
    fdActivityStatus,
    fdClientToken,
    fdCreateTime,
    fdErrors,
    fdExcessCapacityTerminationPolicy,
    fdFleetId,
    fdFleetState,
    fdFulfilledCapacity,
    fdFulfilledOnDemandCapacity,
    fdInstances,
    fdLaunchTemplateConfigs,
    fdOnDemandOptions,
    fdReplaceUnhealthyInstances,
    fdSpotOptions,
    fdTags,
    fdTargetCapacitySpecification,
    fdTerminateInstancesWithExpiration,
    fdType,
    fdValidFrom,
    fdValidUntil,

    -- * TransitGatewayPropagationState
    TransitGatewayPropagationState (..),

    -- * DedicatedHostId
    DedicatedHostId (..),

    -- * DatafeedSubscriptionState
    DatafeedSubscriptionState (..),

    -- * ExportTaskState
    ExportTaskState (..),

    -- * ProductCodeValues
    ProductCodeValues (..),

    -- * CapacityReservationTargetResponse
    CapacityReservationTargetResponse (..),
    mkCapacityReservationTargetResponse,
    crtrCapacityReservationId,
    crtrCapacityReservationResourceGroupArn,

    -- * ScheduledInstancesBlockDeviceMapping
    ScheduledInstancesBlockDeviceMapping (..),
    mkScheduledInstancesBlockDeviceMapping,
    sibdmDeviceName,
    sibdmEbs,
    sibdmNoDevice,
    sibdmVirtualName,

    -- * VpnConnection
    VpnConnection (..),
    mkVpnConnection,
    vcCategory,
    vcCustomerGatewayConfiguration,
    vcCustomerGatewayId,
    vcOptions,
    vcRoutes,
    vcState,
    vcTags,
    vcTransitGatewayId,
    vcType,
    vcVgwTelemetry,
    vcVpnConnectionId,
    vcVpnGatewayId,

    -- * CapacityReservationTenancy
    CapacityReservationTenancy (..),

    -- * InstanceState
    InstanceState (..),
    mkInstanceState,
    isCode,
    isName,

    -- * InternetGatewayId
    InternetGatewayId (..),

    -- * VpcEndpoint
    VpcEndpoint (..),
    mkVpcEndpoint,
    veCreationTimestamp,
    veDnsEntries,
    veGroups,
    veLastError,
    veNetworkInterfaceIds,
    veOwnerId,
    vePolicyDocument,
    vePrivateDnsEnabled,
    veRequesterManaged,
    veRouteTableIds,
    veServiceName,
    veState,
    veSubnetIds,
    veTags,
    veVpcEndpointId,
    veVpcEndpointType,
    veVpcId,

    -- * PublicIpAddress
    PublicIpAddress (..),

    -- * TargetGroup
    TargetGroup (..),
    mkTargetGroup,
    tgArn,

    -- * SuccessfulQueuedPurchaseDeletion
    SuccessfulQueuedPurchaseDeletion (..),
    mkSuccessfulQueuedPurchaseDeletion,
    sqpdReservedInstancesId,

    -- * LaunchTemplateInstanceMarketOptionsRequest
    LaunchTemplateInstanceMarketOptionsRequest (..),
    mkLaunchTemplateInstanceMarketOptionsRequest,
    ltimorMarketType,
    ltimorSpotOptions,

    -- * ConnectionLogResponseOptions
    ConnectionLogResponseOptions (..),
    mkConnectionLogResponseOptions,
    clroCloudwatchLogGroup,
    clroCloudwatchLogStream,
    clroEnabled,

    -- * TransitGatewayPropagation
    TransitGatewayPropagation (..),
    mkTransitGatewayPropagation,
    tgpResourceId,
    tgpResourceType,
    tgpState,
    tgpTransitGatewayAttachmentId,
    tgpTransitGatewayRouteTableId,

    -- * ClientData
    ClientData (..),
    mkClientData,
    cdComment,
    cdUploadEnd,
    cdUploadSize,
    cdUploadStart,

    -- * Placement
    Placement (..),
    mkPlacement,
    pAffinity,
    pAvailabilityZone,
    pGroupName,
    pHostId,
    pHostResourceGroupArn,
    pPartitionNumber,
    pSpreadDomain,
    pTenancy,

    -- * TransitGatewayMulticastDomainAssociations
    TransitGatewayMulticastDomainAssociations (..),
    mkTransitGatewayMulticastDomainAssociations,
    tgmdasResourceId,
    tgmdasResourceType,
    tgmdasSubnets,
    tgmdasTransitGatewayAttachmentId,
    tgmdasTransitGatewayMulticastDomainId,

    -- * EventCode
    EventCode (..),

    -- * SpotInstanceType
    SpotInstanceType (..),

    -- * VpcPeeringConnection
    VpcPeeringConnection (..),
    mkVpcPeeringConnection,
    vpcAccepterVpcInfo,
    vpcExpirationTime,
    vpcRequesterVpcInfo,
    vpcStatus,
    vpcTags,
    vpcVpcPeeringConnectionId,

    -- * LaunchTemplateInstanceMetadataOptions
    LaunchTemplateInstanceMetadataOptions (..),
    mkLaunchTemplateInstanceMetadataOptions,
    ltimoHttpEndpoint,
    ltimoHttpPutResponseHopLimit,
    ltimoHttpTokens,
    ltimoState,

    -- * LaunchTemplateSpotMarketOptions
    LaunchTemplateSpotMarketOptions (..),
    mkLaunchTemplateSpotMarketOptions,
    ltsmoBlockDurationMinutes,
    ltsmoInstanceInterruptionBehavior,
    ltsmoMaxPrice,
    ltsmoSpotInstanceType,
    ltsmoValidUntil,

    -- * ByoipCidr
    ByoipCidr (..),
    mkByoipCidr,
    bcCidr,
    bcDescription,
    bcState,
    bcStatusMessage,

    -- * TargetCapacitySpecificationRequest
    TargetCapacitySpecificationRequest (..),
    mkTargetCapacitySpecificationRequest,
    tcsrTotalTargetCapacity,
    tcsrDefaultTargetCapacityType,
    tcsrOnDemandTargetCapacity,
    tcsrSpotTargetCapacity,

    -- * S3Storage
    S3Storage (..),
    mkS3Storage,
    ssAWSAccessKeyId,
    ssBucket,
    ssPrefix,
    ssUploadPolicy,
    ssUploadPolicySignature,

    -- * DefaultRouteTablePropagationValue
    DefaultRouteTablePropagationValue (..),

    -- * PlacementGroupName
    PlacementGroupName (..),

    -- * VgwTelemetry
    VgwTelemetry (..),
    mkVgwTelemetry,
    vtAcceptedRouteCount,
    vtCertificateArn,
    vtLastStatusChange,
    vtOutsideIpAddress,
    vtStatus,
    vtStatusMessage,

    -- * VpnStaticRoute
    VpnStaticRoute (..),
    mkVpnStaticRoute,
    vsrDestinationCidrBlock,
    vsrSource,
    vsrState,

    -- * InstanceStateName
    InstanceStateName (..),

    -- * Instance
    Instance (..),
    mkInstance,
    iAmiLaunchIndex,
    iArchitecture,
    iBlockDeviceMappings,
    iCapacityReservationId,
    iCapacityReservationSpecification,
    iClientToken,
    iCpuOptions,
    iEbsOptimized,
    iElasticGpuAssociations,
    iElasticInferenceAcceleratorAssociations,
    iEnaSupport,
    iEnclaveOptions,
    iHibernationOptions,
    iHypervisor,
    iIamInstanceProfile,
    iImageId,
    iInstanceId,
    iInstanceLifecycle,
    iInstanceType,
    iKernelId,
    iKeyName,
    iLaunchTime,
    iLicenses,
    iMetadataOptions,
    iMonitoring,
    iNetworkInterfaces,
    iOutpostArn,
    iPlacement,
    iPlatform,
    iPrivateDnsName,
    iPrivateIpAddress,
    iProductCodes,
    iPublicDnsName,
    iPublicIpAddress,
    iRamdiskId,
    iRootDeviceName,
    iRootDeviceType,
    iSecurityGroups,
    iSourceDestCheck,
    iSpotInstanceRequestId,
    iSriovNetSupport,
    iState,
    iStateReason,
    iStateTransitionReason,
    iSubnetId,
    iTags,
    iVirtualizationType,
    iVpcId,

    -- * RouteTableAssociationState
    RouteTableAssociationState (..),
    mkRouteTableAssociationState,
    rtasState,
    rtasStatusMessage,

    -- * ImportSnapshotTaskId
    ImportSnapshotTaskId (..),

    -- * ExportTask
    ExportTask (..),
    mkExportTask,
    etDescription,
    etExportTaskId,
    etExportToS3Task,
    etInstanceExportDetails,
    etState,
    etStatusMessage,
    etTags,

    -- * InstanceCreditSpecification
    InstanceCreditSpecification (..),
    mkInstanceCreditSpecification,
    icsCpuCredits,
    icsInstanceId,

    -- * LaunchTemplateEbsBlockDeviceRequest
    LaunchTemplateEbsBlockDeviceRequest (..),
    mkLaunchTemplateEbsBlockDeviceRequest,
    ltebdrDeleteOnTermination,
    ltebdrEncrypted,
    ltebdrIops,
    ltebdrKmsKeyId,
    ltebdrSnapshotId,
    ltebdrVolumeSize,
    ltebdrVolumeType,

    -- * CarrierGatewayId
    CarrierGatewayId (..),

    -- * ResetImageAttributeName
    ResetImageAttributeName (..),

    -- * RequestSpotLaunchSpecification
    RequestSpotLaunchSpecification (..),
    mkRequestSpotLaunchSpecification,
    rslsAddressingType,
    rslsBlockDeviceMappings,
    rslsEbsOptimized,
    rslsIamInstanceProfile,
    rslsImageId,
    rslsInstanceType,
    rslsKernelId,
    rslsKeyName,
    rslsMonitoring,
    rslsNetworkInterfaces,
    rslsPlacement,
    rslsRamdiskId,
    rslsSecurityGroupIds,
    rslsSecurityGroups,
    rslsSubnetId,
    rslsUserData,

    -- * ClientVpnRouteStatusCode
    ClientVpnRouteStatusCode (..),

    -- * LocalGatewayVirtualInterface
    LocalGatewayVirtualInterface (..),
    mkLocalGatewayVirtualInterface,
    lgviLocalAddress,
    lgviLocalBgpAsn,
    lgviLocalGatewayId,
    lgviLocalGatewayVirtualInterfaceId,
    lgviOwnerId,
    lgviPeerAddress,
    lgviPeerBgpAsn,
    lgviTags,
    lgviVlan,

    -- * ValidationWarning
    ValidationWarning (..),
    mkValidationWarning,
    vwErrors,

    -- * SnapshotId
    SnapshotId (..),

    -- * VolumeDetail
    VolumeDetail (..),
    mkVolumeDetail,
    vdSize,

    -- * TargetConfiguration
    TargetConfiguration (..),
    mkTargetConfiguration,
    tcInstanceCount,
    tcOfferingId,

    -- * PaymentOption
    PaymentOption (..),

    -- * PricingDetail
    PricingDetail (..),
    mkPricingDetail,
    pdCount,
    pdPrice,

    -- * ClientVpnAuthentication
    ClientVpnAuthentication (..),
    mkClientVpnAuthentication,
    cvaActiveDirectory,
    cvaFederatedAuthentication,
    cvaMutualAuthentication,
    cvaType,

    -- * TransitGatewayPrefixListReferenceState
    TransitGatewayPrefixListReferenceState (..),

    -- * IamInstanceProfileAssociationState
    IamInstanceProfileAssociationState (..),

    -- * ReservedInstancesListingId
    ReservedInstancesListingId (..),

    -- * NetworkInterfacePrivateIpAddress
    NetworkInterfacePrivateIpAddress (..),
    mkNetworkInterfacePrivateIpAddress,
    nipiaAssociation,
    nipiaPrimary,
    nipiaPrivateDnsName,
    nipiaPrivateIpAddress,

    -- * DiskImageFormat
    DiskImageFormat (..),

    -- * ActivityStatus
    ActivityStatus (..),

    -- * TransitGatewayRouteState
    TransitGatewayRouteState (..),

    -- * FleetLaunchTemplateConfig
    FleetLaunchTemplateConfig (..),
    mkFleetLaunchTemplateConfig,
    fltcLaunchTemplateSpecification,
    fltcOverrides,

    -- * BundleTaskError
    BundleTaskError (..),
    mkBundleTaskError,
    bteCode,
    bteMessage,

    -- * VpcClassicLink
    VpcClassicLink (..),
    mkVpcClassicLink,
    vclClassicLinkEnabled,
    vclTags,
    vclVpcId,

    -- * SpotOptionsRequest
    SpotOptionsRequest (..),
    mkSpotOptionsRequest,
    sorAllocationStrategy,
    sorInstanceInterruptionBehavior,
    sorInstancePoolsToUseCount,
    sorMaintenanceStrategies,
    sorMaxTotalPrice,
    sorMinTargetCapacity,
    sorSingleAvailabilityZone,
    sorSingleInstanceType,

    -- * VolumeStatusItem
    VolumeStatusItem (..),
    mkVolumeStatusItem,
    vsiActions,
    vsiAttachmentStatuses,
    vsiAvailabilityZone,
    vsiEvents,
    vsiOutpostArn,
    vsiVolumeId,
    vsiVolumeStatus,

    -- * ScheduledInstanceAvailability
    ScheduledInstanceAvailability (..),
    mkScheduledInstanceAvailability,
    siaAvailabilityZone,
    siaAvailableInstanceCount,
    siaFirstSlotStartTime,
    siaHourlyPrice,
    siaInstanceType,
    siaMaxTermDurationInDays,
    siaMinTermDurationInDays,
    siaNetworkPlatform,
    siaPlatform,
    siaPurchaseToken,
    siaRecurrence,
    siaSlotDurationInHours,
    siaTotalScheduledInstanceHours,

    -- * ElasticGpuSpecificationResponse
    ElasticGpuSpecificationResponse (..),
    mkElasticGpuSpecificationResponse,
    egsrType,

    -- * OwnerId
    OwnerId (..),

    -- * DestinationCidrBlock
    DestinationCidrBlock (..),

    -- * DestinationIpv6CidrBlock
    DestinationIpv6CidrBlock (..),

    -- * DestinationPrefixListId
    DestinationPrefixListId (..),

    -- * GatewayId
    GatewayId (..),

    -- * PrincipalArn
    PrincipalArn (..),

    -- * Resource
    Resource (..),

    -- * MaxPrice
    MaxPrice (..),

    -- * S3Bucket
    S3Bucket (..),

    -- * S3Prefix
    S3Prefix (..),

    -- * Arn
    Arn (..),

    -- * AssociationId
    AssociationId (..),

    -- * NewAssociationId
    NewAssociationId (..),

    -- * Bucket
    Bucket (..),

    -- * Key
    Key (..),

    -- * UserId
    UserId (..),

    -- * Message
    Message (..),

    -- * Protocol
    Protocol (..),

    -- * ClientToken
    ClientToken (..),

    -- * DefaultVersion
    DefaultVersion (..),

    -- * ConnectionNotificationArn
    ConnectionNotificationArn (..),

    -- * Value
    Value (..),

    -- * Type
    Type (..),

    -- * ResourceId
    ResourceId (..),

    -- * AdditionalInfo
    AdditionalInfo (..),

    -- * PrivateIpAddress
    PrivateIpAddress (..),

    -- * KeyName
    KeyName (..),

    -- * RamDiskId
    RamDiskId (..),

    -- * DataEncryptionKeyId
    DataEncryptionKeyId (..),

    -- * Description
    Description (..),

    -- * OwnerAlias
    OwnerAlias (..),

    -- * Progress
    Progress (..),

    -- * StateMessage
    StateMessage (..),

    -- * CidrIp
    CidrIp (..),

    -- * GroupId
    GroupId (..),

    -- * GroupName
    GroupName (..),

    -- * IpProtocol
    IpProtocol (..),

    -- * SourceSecurityGroupName
    SourceSecurityGroupName (..),

    -- * SourceSecurityGroupOwnerId
    SourceSecurityGroupOwnerId (..),

    -- * PeerOwnerId
    PeerOwnerId (..),

    -- * PeerRegion
    PeerRegion (..),

    -- * PeerVpcId
    PeerVpcId (..),

    -- * Code
    Code (..),

    -- * LambdaFunctionArn
    LambdaFunctionArn (..),

    -- * Frequency
    Frequency (..),

    -- * OccurrenceUnit
    OccurrenceUnit (..),

    -- * LocalGatewayRouteTableArn
    LocalGatewayRouteTableArn (..),

    -- * LocalGatewayRouteTableId
    LocalGatewayRouteTableId (..),

    -- * DirectoryId
    DirectoryId (..),

    -- * AssociatedRoleArn
    AssociatedRoleArn (..),

    -- * CertificateS3BucketName
    CertificateS3BucketName (..),

    -- * CertificateS3ObjectKey
    CertificateS3ObjectKey (..),

    -- * EncryptionKmsKeyId
    EncryptionKmsKeyId (..),

    -- * StatusMessage
    StatusMessage (..),

    -- * NetworkId
    NetworkId (..),

    -- * DeviceId
    DeviceId (..),

    -- * SubsystemId
    SubsystemId (..),

    -- * SubsystemVendorId
    SubsystemVendorId (..),

    -- * VendorId
    VendorId (..),

    -- * AddressFamily
    AddressFamily (..),

    -- * PrefixListArn
    PrefixListArn (..),

    -- * PrefixListName
    PrefixListName (..),

    -- * SourceCidrBlock
    SourceCidrBlock (..),

    -- * ReasonMessage
    ReasonMessage (..),

    -- * AssociationDefaultRouteTableId
    AssociationDefaultRouteTableId (..),

    -- * PropagationDefaultRouteTableId
    PropagationDefaultRouteTableId (..),

    -- * CpuCredits
    CpuCredits (..),

    -- * AttachmentId
    AttachmentId (..),

    -- * PreviousState
    PreviousState (..),

    -- * InstanceFamily
    InstanceFamily (..),

    -- * Cidr
    Cidr (..),

    -- * InterfaceType
    InterfaceType (..),

    -- * Name
    Name (..),

    -- * ImageLocation
    ImageLocation (..),

    -- * RootDeviceName
    RootDeviceName (..),

    -- * SriovNetSupport
    SriovNetSupport (..),

    -- * Version
    Version (..),

    -- * MaxTotalPrice
    MaxTotalPrice (..),

    -- * TransitGatewayArn
    TransitGatewayArn (..),

    -- * Output
    Output (..),

    -- * ServiceId
    ServiceId (..),

    -- * GroupIpAddress
    GroupIpAddress (..),

    -- * CertificateArn
    CertificateArn (..),

    -- * RoleArn
    RoleArn (..),

    -- * PolicyDocument
    PolicyDocument (..),

    -- * ClientIp
    ClientIp (..),

    -- * CommonName
    CommonName (..),

    -- * ConnectionEndTime
    ConnectionEndTime (..),

    -- * ConnectionEstablishedTime
    ConnectionEstablishedTime (..),

    -- * ConnectionId
    ConnectionId (..),

    -- * EgressBytes
    EgressBytes (..),

    -- * EgressPackets
    EgressPackets (..),

    -- * IngressBytes
    IngressBytes (..),

    -- * IngressPackets
    IngressPackets (..),

    -- * Timestamp
    Timestamp (..),

    -- * Username
    Username (..),

    -- * CreationDate
    CreationDate (..),

    -- * ImageOwnerAlias
    ImageOwnerAlias (..),

    -- * PlatformDetails
    PlatformDetails (..),

    -- * UsageOperation
    UsageOperation (..),

    -- * ClientCidrBlock
    ClientCidrBlock (..),

    -- * ServerCertificateArn
    ServerCertificateArn (..),

    -- * CarrierIp
    CarrierIp (..),

    -- * CustomerOwnedIp
    CustomerOwnedIp (..),

    -- * CustomerOwnedIpv4Pool
    CustomerOwnedIpv4Pool (..),

    -- * NetworkBorderGroup
    NetworkBorderGroup (..),

    -- * PublicIp
    PublicIp (..),

    -- * RoleName
    RoleName (..),

    -- * InstanceOwnerId
    InstanceOwnerId (..),

    -- * CloudwatchLogGroup
    CloudwatchLogGroup (..),

    -- * CloudwatchLogStream
    CloudwatchLogStream (..),

    -- * FpgaImageGlobalId
    FpgaImageGlobalId (..),

    -- * AvailabilityZoneGroup
    AvailabilityZoneGroup (..),

    -- * LaunchGroup
    LaunchGroup (..),

    -- * Device
    Device (..),

    -- * IpOwnerId
    IpOwnerId (..),

    -- * PublicDnsName
    PublicDnsName (..),

    -- * OutpostArn
    OutpostArn (..),

    -- * Attribute
    Attribute (..),

    -- * Principal
    Principal (..),

    -- * SourceVersion
    SourceVersion (..),

    -- * CreationTime
    CreationTime (..),

    -- * DeletionTime
    DeletionTime (..),

    -- * DnsName
    DnsName (..),

    -- * SelfServicePortalUrl
    SelfServicePortalUrl (..),

    -- * S3Key
    S3Key (..),

    -- * DeviceName
    DeviceName (..),

    -- * NoDevice
    NoDevice (..),

    -- * VirtualName
    VirtualName (..),

    -- * TargetVpcSubnetId
    TargetVpcSubnetId (..),

    -- * ExpirationTime
    ExpirationTime (..),

    -- * SAMLProviderArn
    SAMLProviderArn (..),

    -- * SelfServiceSAMLProviderArn
    SelfServiceSAMLProviderArn (..),

    -- * HostId
    HostId (..),

    -- * PoolId
    PoolId (..),

    -- * PoolArn
    PoolArn (..),
  )
where

import Network.AWS.EC2.Types.AccountAttribute
import Network.AWS.EC2.Types.AccountAttributeName
import Network.AWS.EC2.Types.AccountAttributeValue
import Network.AWS.EC2.Types.ActiveInstance
import Network.AWS.EC2.Types.ActivityStatus
import Network.AWS.EC2.Types.AddPrefixListEntry
import Network.AWS.EC2.Types.AdditionalInfo
import Network.AWS.EC2.Types.Address
import Network.AWS.EC2.Types.AddressFamily
import Network.AWS.EC2.Types.AddressStatus
import Network.AWS.EC2.Types.Affinity
import Network.AWS.EC2.Types.AllocationId
import Network.AWS.EC2.Types.AllocationState
import Network.AWS.EC2.Types.AllocationStrategy
import Network.AWS.EC2.Types.AllowedPrincipal
import Network.AWS.EC2.Types.AllowsMultipleInstanceTypes
import Network.AWS.EC2.Types.ApplianceModeSupportValue
import Network.AWS.EC2.Types.ArchitectureType
import Network.AWS.EC2.Types.ArchitectureValues
import Network.AWS.EC2.Types.Arn
import Network.AWS.EC2.Types.AssignedPrivateIpAddress
import Network.AWS.EC2.Types.AssociatedNetworkType
import Network.AWS.EC2.Types.AssociatedRole
import Network.AWS.EC2.Types.AssociatedRoleArn
import Network.AWS.EC2.Types.AssociatedTargetNetwork
import Network.AWS.EC2.Types.AssociationDefaultRouteTableId
import Network.AWS.EC2.Types.AssociationId
import Network.AWS.EC2.Types.AssociationStatus
import Network.AWS.EC2.Types.AssociationStatusCode
import Network.AWS.EC2.Types.AttachmentId
import Network.AWS.EC2.Types.AttachmentStatus
import Network.AWS.EC2.Types.Attribute
import Network.AWS.EC2.Types.AttributeBooleanValue
import Network.AWS.EC2.Types.AttributeValue
import Network.AWS.EC2.Types.AuthorizationRule
import Network.AWS.EC2.Types.AutoAcceptSharedAttachmentsValue
import Network.AWS.EC2.Types.AutoPlacement
import Network.AWS.EC2.Types.AvailabilityZone
import Network.AWS.EC2.Types.AvailabilityZoneGroup
import Network.AWS.EC2.Types.AvailabilityZoneMessage
import Network.AWS.EC2.Types.AvailabilityZoneOptInStatus
import Network.AWS.EC2.Types.AvailabilityZoneState
import Network.AWS.EC2.Types.AvailableCapacity
import Network.AWS.EC2.Types.BatchState
import Network.AWS.EC2.Types.BlobAttributeValue
import Network.AWS.EC2.Types.BlockDeviceMapping
import Network.AWS.EC2.Types.Bucket
import Network.AWS.EC2.Types.BundleId
import Network.AWS.EC2.Types.BundleTask
import Network.AWS.EC2.Types.BundleTaskError
import Network.AWS.EC2.Types.BundleTaskState
import Network.AWS.EC2.Types.ByoipCidr
import Network.AWS.EC2.Types.ByoipCidrState
import Network.AWS.EC2.Types.CancelBatchErrorCode
import Network.AWS.EC2.Types.CancelSpotFleetRequestsError
import Network.AWS.EC2.Types.CancelSpotFleetRequestsErrorItem
import Network.AWS.EC2.Types.CancelSpotFleetRequestsSuccessItem
import Network.AWS.EC2.Types.CancelSpotInstanceRequestState
import Network.AWS.EC2.Types.CancelledSpotInstanceRequest
import Network.AWS.EC2.Types.CapacityReservation
import Network.AWS.EC2.Types.CapacityReservationGroup
import Network.AWS.EC2.Types.CapacityReservationId
import Network.AWS.EC2.Types.CapacityReservationInstancePlatform
import Network.AWS.EC2.Types.CapacityReservationOptions
import Network.AWS.EC2.Types.CapacityReservationOptionsRequest
import Network.AWS.EC2.Types.CapacityReservationPreference
import Network.AWS.EC2.Types.CapacityReservationSpecification
import Network.AWS.EC2.Types.CapacityReservationSpecificationResponse
import Network.AWS.EC2.Types.CapacityReservationState
import Network.AWS.EC2.Types.CapacityReservationTarget
import Network.AWS.EC2.Types.CapacityReservationTargetResponse
import Network.AWS.EC2.Types.CapacityReservationTenancy
import Network.AWS.EC2.Types.CarrierGateway
import Network.AWS.EC2.Types.CarrierGatewayId
import Network.AWS.EC2.Types.CarrierGatewayState
import Network.AWS.EC2.Types.CarrierIp
import Network.AWS.EC2.Types.CertificateArn
import Network.AWS.EC2.Types.CertificateAuthentication
import Network.AWS.EC2.Types.CertificateAuthenticationRequest
import Network.AWS.EC2.Types.CertificateS3BucketName
import Network.AWS.EC2.Types.CertificateS3ObjectKey
import Network.AWS.EC2.Types.Cidr
import Network.AWS.EC2.Types.CidrAuthorizationContext
import Network.AWS.EC2.Types.CidrBlock
import Network.AWS.EC2.Types.CidrIp
import Network.AWS.EC2.Types.ClassicLinkDnsSupport
import Network.AWS.EC2.Types.ClassicLinkInstance
import Network.AWS.EC2.Types.ClassicLoadBalancer
import Network.AWS.EC2.Types.ClassicLoadBalancersConfig
import Network.AWS.EC2.Types.ClientCertificateRevocationListStatus
import Network.AWS.EC2.Types.ClientCertificateRevocationListStatusCode
import Network.AWS.EC2.Types.ClientCidrBlock
import Network.AWS.EC2.Types.ClientConnectOptions
import Network.AWS.EC2.Types.ClientConnectResponseOptions
import Network.AWS.EC2.Types.ClientData
import Network.AWS.EC2.Types.ClientIp
import Network.AWS.EC2.Types.ClientToken
import Network.AWS.EC2.Types.ClientVpnAuthentication
import Network.AWS.EC2.Types.ClientVpnAuthenticationRequest
import Network.AWS.EC2.Types.ClientVpnAuthenticationType
import Network.AWS.EC2.Types.ClientVpnAuthorizationRuleStatus
import Network.AWS.EC2.Types.ClientVpnAuthorizationRuleStatusCode
import Network.AWS.EC2.Types.ClientVpnConnection
import Network.AWS.EC2.Types.ClientVpnConnectionStatus
import Network.AWS.EC2.Types.ClientVpnConnectionStatusCode
import Network.AWS.EC2.Types.ClientVpnEndpoint
import Network.AWS.EC2.Types.ClientVpnEndpointAttributeStatus
import Network.AWS.EC2.Types.ClientVpnEndpointAttributeStatusCode
import Network.AWS.EC2.Types.ClientVpnEndpointId
import Network.AWS.EC2.Types.ClientVpnEndpointStatus
import Network.AWS.EC2.Types.ClientVpnEndpointStatusCode
import Network.AWS.EC2.Types.ClientVpnRoute
import Network.AWS.EC2.Types.ClientVpnRouteStatus
import Network.AWS.EC2.Types.ClientVpnRouteStatusCode
import Network.AWS.EC2.Types.CloudwatchLogGroup
import Network.AWS.EC2.Types.CloudwatchLogStream
import Network.AWS.EC2.Types.Code
import Network.AWS.EC2.Types.CoipAddressUsage
import Network.AWS.EC2.Types.CoipPool
import Network.AWS.EC2.Types.CoipPoolId
import Network.AWS.EC2.Types.CommonName
import Network.AWS.EC2.Types.ConnectionEndTime
import Network.AWS.EC2.Types.ConnectionEstablishedTime
import Network.AWS.EC2.Types.ConnectionId
import Network.AWS.EC2.Types.ConnectionLogOptions
import Network.AWS.EC2.Types.ConnectionLogResponseOptions
import Network.AWS.EC2.Types.ConnectionNotification
import Network.AWS.EC2.Types.ConnectionNotificationArn
import Network.AWS.EC2.Types.ConnectionNotificationId
import Network.AWS.EC2.Types.ConnectionNotificationState
import Network.AWS.EC2.Types.ConnectionNotificationType
import Network.AWS.EC2.Types.ContainerFormat
import Network.AWS.EC2.Types.ConversionTask
import Network.AWS.EC2.Types.ConversionTaskId
import Network.AWS.EC2.Types.ConversionTaskState
import Network.AWS.EC2.Types.CopyTagsFromSource
import Network.AWS.EC2.Types.CpuCredits
import Network.AWS.EC2.Types.CpuOptions
import Network.AWS.EC2.Types.CpuOptionsRequest
import Network.AWS.EC2.Types.CreateFleetError
import Network.AWS.EC2.Types.CreateFleetInstance
import Network.AWS.EC2.Types.CreateTransitGatewayVpcAttachmentRequestOptions
import Network.AWS.EC2.Types.CreateVolumePermission
import Network.AWS.EC2.Types.CreateVolumePermissionModifications
import Network.AWS.EC2.Types.CreationDate
import Network.AWS.EC2.Types.CreationTime
import Network.AWS.EC2.Types.CreditSpecification
import Network.AWS.EC2.Types.CreditSpecificationRequest
import Network.AWS.EC2.Types.CurrencyCodeValues
import Network.AWS.EC2.Types.CustomerGateway
import Network.AWS.EC2.Types.CustomerGatewayId
import Network.AWS.EC2.Types.CustomerOwnedIp
import Network.AWS.EC2.Types.CustomerOwnedIpv4Pool
import Network.AWS.EC2.Types.DataEncryptionKeyId
import Network.AWS.EC2.Types.DatafeedSubscriptionState
import Network.AWS.EC2.Types.DedicatedHostId
import Network.AWS.EC2.Types.DefaultRouteTableAssociationValue
import Network.AWS.EC2.Types.DefaultRouteTablePropagationValue
import Network.AWS.EC2.Types.DefaultTargetCapacityType
import Network.AWS.EC2.Types.DefaultVersion
import Network.AWS.EC2.Types.DeleteFleetError
import Network.AWS.EC2.Types.DeleteFleetErrorCode
import Network.AWS.EC2.Types.DeleteFleetErrorItem
import Network.AWS.EC2.Types.DeleteFleetSuccessItem
import Network.AWS.EC2.Types.DeleteLaunchTemplateVersionsResponseErrorItem
import Network.AWS.EC2.Types.DeleteLaunchTemplateVersionsResponseSuccessItem
import Network.AWS.EC2.Types.DeleteQueuedReservedInstancesError
import Network.AWS.EC2.Types.DeleteQueuedReservedInstancesErrorCode
import Network.AWS.EC2.Types.DeletionTime
import Network.AWS.EC2.Types.DeregisterInstanceTagAttributeRequest
import Network.AWS.EC2.Types.DescribeFastSnapshotRestoreSuccessItem
import Network.AWS.EC2.Types.DescribeFleetError
import Network.AWS.EC2.Types.DescribeFleetsInstances
import Network.AWS.EC2.Types.Description
import Network.AWS.EC2.Types.DestinationCidrBlock
import Network.AWS.EC2.Types.DestinationIpv6CidrBlock
import Network.AWS.EC2.Types.DestinationPrefixListId
import Network.AWS.EC2.Types.Device
import Network.AWS.EC2.Types.DeviceId
import Network.AWS.EC2.Types.DeviceName
import Network.AWS.EC2.Types.DeviceType
import Network.AWS.EC2.Types.DhcpConfiguration
import Network.AWS.EC2.Types.DhcpOptions
import Network.AWS.EC2.Types.DhcpOptionsId
import Network.AWS.EC2.Types.DirectoryId
import Network.AWS.EC2.Types.DirectoryServiceAuthentication
import Network.AWS.EC2.Types.DirectoryServiceAuthenticationRequest
import Network.AWS.EC2.Types.DisableFastSnapshotRestoreErrorItem
import Network.AWS.EC2.Types.DisableFastSnapshotRestoreStateError
import Network.AWS.EC2.Types.DisableFastSnapshotRestoreStateErrorItem
import Network.AWS.EC2.Types.DisableFastSnapshotRestoreSuccessItem
import Network.AWS.EC2.Types.DiskImage
import Network.AWS.EC2.Types.DiskImageDescription
import Network.AWS.EC2.Types.DiskImageDetail
import Network.AWS.EC2.Types.DiskImageFormat
import Network.AWS.EC2.Types.DiskImageVolumeDescription
import Network.AWS.EC2.Types.DiskInfo
import Network.AWS.EC2.Types.DiskType
import Network.AWS.EC2.Types.DnsEntry
import Network.AWS.EC2.Types.DnsName
import Network.AWS.EC2.Types.DnsNameState
import Network.AWS.EC2.Types.DnsServersOptionsModifyStructure
import Network.AWS.EC2.Types.DnsSupportValue
import Network.AWS.EC2.Types.DomainType
import Network.AWS.EC2.Types.EbsBlockDevice
import Network.AWS.EC2.Types.EbsEncryptionSupport
import Network.AWS.EC2.Types.EbsInfo
import Network.AWS.EC2.Types.EbsInstanceBlockDevice
import Network.AWS.EC2.Types.EbsInstanceBlockDeviceSpecification
import Network.AWS.EC2.Types.EbsNvmeSupport
import Network.AWS.EC2.Types.EbsOptimizedInfo
import Network.AWS.EC2.Types.EbsOptimizedSupport
import Network.AWS.EC2.Types.EgressBytes
import Network.AWS.EC2.Types.EgressOnlyInternetGateway
import Network.AWS.EC2.Types.EgressOnlyInternetGatewayId
import Network.AWS.EC2.Types.EgressPackets
import Network.AWS.EC2.Types.ElasticGpuAssociation
import Network.AWS.EC2.Types.ElasticGpuHealth
import Network.AWS.EC2.Types.ElasticGpuId
import Network.AWS.EC2.Types.ElasticGpuSpecification
import Network.AWS.EC2.Types.ElasticGpuSpecificationResponse
import Network.AWS.EC2.Types.ElasticGpuState
import Network.AWS.EC2.Types.ElasticGpuStatus
import Network.AWS.EC2.Types.ElasticGpus
import Network.AWS.EC2.Types.ElasticInferenceAccelerator
import Network.AWS.EC2.Types.ElasticInferenceAcceleratorAssociation
import Network.AWS.EC2.Types.EnaSupport
import Network.AWS.EC2.Types.EnableFastSnapshotRestoreErrorItem
import Network.AWS.EC2.Types.EnableFastSnapshotRestoreStateError
import Network.AWS.EC2.Types.EnableFastSnapshotRestoreStateErrorItem
import Network.AWS.EC2.Types.EnableFastSnapshotRestoreSuccessItem
import Network.AWS.EC2.Types.EnclaveOptions
import Network.AWS.EC2.Types.EnclaveOptionsRequest
import Network.AWS.EC2.Types.EncryptionKmsKeyId
import Network.AWS.EC2.Types.EndDateType
import Network.AWS.EC2.Types.EphemeralNvmeSupport
import Network.AWS.EC2.Types.EventCode
import Network.AWS.EC2.Types.EventInformation
import Network.AWS.EC2.Types.EventType
import Network.AWS.EC2.Types.ExcessCapacityTerminationPolicy
import Network.AWS.EC2.Types.ExpirationTime
import Network.AWS.EC2.Types.ExportEnvironment
import Network.AWS.EC2.Types.ExportImageTask
import Network.AWS.EC2.Types.ExportImageTaskId
import Network.AWS.EC2.Types.ExportTask
import Network.AWS.EC2.Types.ExportTaskId
import Network.AWS.EC2.Types.ExportTaskS3Location
import Network.AWS.EC2.Types.ExportTaskS3LocationRequest
import Network.AWS.EC2.Types.ExportTaskState
import Network.AWS.EC2.Types.ExportToS3Task
import Network.AWS.EC2.Types.ExportToS3TaskSpecification
import Network.AWS.EC2.Types.ExportVmTaskId
import Network.AWS.EC2.Types.FailedQueuedPurchaseDeletion
import Network.AWS.EC2.Types.FastSnapshotRestoreStateCode
import Network.AWS.EC2.Types.FederatedAuthentication
import Network.AWS.EC2.Types.FederatedAuthenticationRequest
import Network.AWS.EC2.Types.Filter
import Network.AWS.EC2.Types.FleetActivityStatus
import Network.AWS.EC2.Types.FleetCapacityReservationUsageStrategy
import Network.AWS.EC2.Types.FleetData
import Network.AWS.EC2.Types.FleetEventType
import Network.AWS.EC2.Types.FleetExcessCapacityTerminationPolicy
import Network.AWS.EC2.Types.FleetId
import Network.AWS.EC2.Types.FleetLaunchTemplateConfig
import Network.AWS.EC2.Types.FleetLaunchTemplateConfigRequest
import Network.AWS.EC2.Types.FleetLaunchTemplateOverrides
import Network.AWS.EC2.Types.FleetLaunchTemplateOverridesRequest
import Network.AWS.EC2.Types.FleetLaunchTemplateSpecification
import Network.AWS.EC2.Types.FleetLaunchTemplateSpecificationRequest
import Network.AWS.EC2.Types.FleetOnDemandAllocationStrategy
import Network.AWS.EC2.Types.FleetReplacementStrategy
import Network.AWS.EC2.Types.FleetSpotCapacityRebalance
import Network.AWS.EC2.Types.FleetSpotCapacityRebalanceRequest
import Network.AWS.EC2.Types.FleetSpotMaintenanceStrategies
import Network.AWS.EC2.Types.FleetSpotMaintenanceStrategiesRequest
import Network.AWS.EC2.Types.FleetStateCode
import Network.AWS.EC2.Types.FleetType
import Network.AWS.EC2.Types.FlowLog
import Network.AWS.EC2.Types.FlowLogResourceId
import Network.AWS.EC2.Types.FlowLogsResourceType
import Network.AWS.EC2.Types.FpgaDeviceInfo
import Network.AWS.EC2.Types.FpgaDeviceManufacturerName
import Network.AWS.EC2.Types.FpgaDeviceMemoryInfo
import Network.AWS.EC2.Types.FpgaDeviceName
import Network.AWS.EC2.Types.FpgaImage
import Network.AWS.EC2.Types.FpgaImageAttribute
import Network.AWS.EC2.Types.FpgaImageAttributeName
import Network.AWS.EC2.Types.FpgaImageGlobalId
import Network.AWS.EC2.Types.FpgaImageId
import Network.AWS.EC2.Types.FpgaImageState
import Network.AWS.EC2.Types.FpgaImageStateCode
import Network.AWS.EC2.Types.FpgaInfo
import Network.AWS.EC2.Types.Frequency
import Network.AWS.EC2.Types.GatewayId
import Network.AWS.EC2.Types.GatewayType
import Network.AWS.EC2.Types.GpuDeviceInfo
import Network.AWS.EC2.Types.GpuDeviceManufacturerName
import Network.AWS.EC2.Types.GpuDeviceMemoryInfo
import Network.AWS.EC2.Types.GpuDeviceName
import Network.AWS.EC2.Types.GpuInfo
import Network.AWS.EC2.Types.GroupId
import Network.AWS.EC2.Types.GroupIdentifier
import Network.AWS.EC2.Types.GroupIpAddress
import Network.AWS.EC2.Types.GroupName
import Network.AWS.EC2.Types.HibernationOptions
import Network.AWS.EC2.Types.HibernationOptionsRequest
import Network.AWS.EC2.Types.HistoryRecord
import Network.AWS.EC2.Types.HistoryRecordEntry
import Network.AWS.EC2.Types.Host
import Network.AWS.EC2.Types.HostId
import Network.AWS.EC2.Types.HostInstance
import Network.AWS.EC2.Types.HostOffering
import Network.AWS.EC2.Types.HostProperties
import Network.AWS.EC2.Types.HostRecovery
import Network.AWS.EC2.Types.HostReservation
import Network.AWS.EC2.Types.HostReservationId
import Network.AWS.EC2.Types.HostTenancy
import Network.AWS.EC2.Types.HttpTokensState
import Network.AWS.EC2.Types.HypervisorType
import Network.AWS.EC2.Types.IKEVersionsListValue
import Network.AWS.EC2.Types.IKEVersionsRequestListValue
import Network.AWS.EC2.Types.IamInstanceProfile
import Network.AWS.EC2.Types.IamInstanceProfileAssociation
import Network.AWS.EC2.Types.IamInstanceProfileAssociationId
import Network.AWS.EC2.Types.IamInstanceProfileAssociationState
import Network.AWS.EC2.Types.IamInstanceProfileSpecification
import Network.AWS.EC2.Types.IcmpTypeCode
import Network.AWS.EC2.Types.IdFormat
import Network.AWS.EC2.Types.Image
import Network.AWS.EC2.Types.ImageAttributeName
import Network.AWS.EC2.Types.ImageDiskContainer
import Network.AWS.EC2.Types.ImageId
import Network.AWS.EC2.Types.ImageLocation
import Network.AWS.EC2.Types.ImageOwnerAlias
import Network.AWS.EC2.Types.ImageState
import Network.AWS.EC2.Types.ImageTypeValues
import Network.AWS.EC2.Types.ImportImageLicenseConfigurationRequest
import Network.AWS.EC2.Types.ImportImageLicenseConfigurationResponse
import Network.AWS.EC2.Types.ImportImageTask
import Network.AWS.EC2.Types.ImportImageTaskId
import Network.AWS.EC2.Types.ImportInstanceLaunchSpecification
import Network.AWS.EC2.Types.ImportInstanceTaskDetails
import Network.AWS.EC2.Types.ImportInstanceVolumeDetailItem
import Network.AWS.EC2.Types.ImportSnapshotTask
import Network.AWS.EC2.Types.ImportSnapshotTaskId
import Network.AWS.EC2.Types.ImportTaskId
import Network.AWS.EC2.Types.ImportVolumeTaskDetails
import Network.AWS.EC2.Types.InferenceAcceleratorInfo
import Network.AWS.EC2.Types.InferenceDeviceInfo
import Network.AWS.EC2.Types.InferenceDeviceManufacturerName
import Network.AWS.EC2.Types.InferenceDeviceName
import Network.AWS.EC2.Types.IngressBytes
import Network.AWS.EC2.Types.IngressPackets
import Network.AWS.EC2.Types.Instance
import Network.AWS.EC2.Types.InstanceAttributeName
import Network.AWS.EC2.Types.InstanceBlockDeviceMapping
import Network.AWS.EC2.Types.InstanceBlockDeviceMappingSpecification
import Network.AWS.EC2.Types.InstanceCapacity
import Network.AWS.EC2.Types.InstanceCount
import Network.AWS.EC2.Types.InstanceCreditSpecification
import Network.AWS.EC2.Types.InstanceCreditSpecificationRequest
import Network.AWS.EC2.Types.InstanceEventId
import Network.AWS.EC2.Types.InstanceExportDetails
import Network.AWS.EC2.Types.InstanceFamily
import Network.AWS.EC2.Types.InstanceFamilyCreditSpecification
import Network.AWS.EC2.Types.InstanceHealthStatus
import Network.AWS.EC2.Types.InstanceId
import Network.AWS.EC2.Types.InstanceInterruptionBehavior
import Network.AWS.EC2.Types.InstanceIpv6Address
import Network.AWS.EC2.Types.InstanceIpv6AddressRequest
import Network.AWS.EC2.Types.InstanceLifecycle
import Network.AWS.EC2.Types.InstanceLifecycleType
import Network.AWS.EC2.Types.InstanceMarketOptionsRequest
import Network.AWS.EC2.Types.InstanceMatchCriteria
import Network.AWS.EC2.Types.InstanceMetadataEndpointState
import Network.AWS.EC2.Types.InstanceMetadataOptionsRequest
import Network.AWS.EC2.Types.InstanceMetadataOptionsResponse
import Network.AWS.EC2.Types.InstanceMetadataOptionsState
import Network.AWS.EC2.Types.InstanceMonitoring
import Network.AWS.EC2.Types.InstanceNetworkInterface
import Network.AWS.EC2.Types.InstanceNetworkInterfaceAssociation
import Network.AWS.EC2.Types.InstanceNetworkInterfaceAttachment
import Network.AWS.EC2.Types.InstanceNetworkInterfaceSpecification
import Network.AWS.EC2.Types.InstanceOwnerId
import Network.AWS.EC2.Types.InstancePrivateIpAddress
import Network.AWS.EC2.Types.InstanceSpecification
import Network.AWS.EC2.Types.InstanceState
import Network.AWS.EC2.Types.InstanceStateChange
import Network.AWS.EC2.Types.InstanceStateName
import Network.AWS.EC2.Types.InstanceStatus
import Network.AWS.EC2.Types.InstanceStatusDetails
import Network.AWS.EC2.Types.InstanceStatusEvent
import Network.AWS.EC2.Types.InstanceStatusSummary
import Network.AWS.EC2.Types.InstanceStorageInfo
import Network.AWS.EC2.Types.InstanceTagNotificationAttribute
import Network.AWS.EC2.Types.InstanceType
import Network.AWS.EC2.Types.InstanceTypeHypervisor
import Network.AWS.EC2.Types.InstanceTypeInfo
import Network.AWS.EC2.Types.InstanceTypeOffering
import Network.AWS.EC2.Types.InstanceUsage
import Network.AWS.EC2.Types.InterfacePermissionType
import Network.AWS.EC2.Types.InterfaceType
import Network.AWS.EC2.Types.InternetGateway
import Network.AWS.EC2.Types.InternetGatewayAttachment
import Network.AWS.EC2.Types.InternetGatewayId
import Network.AWS.EC2.Types.IpOwnerId
import Network.AWS.EC2.Types.IpPermission
import Network.AWS.EC2.Types.IpProtocol
import Network.AWS.EC2.Types.IpRange
import Network.AWS.EC2.Types.Ipv4PoolEc2Id
import Network.AWS.EC2.Types.Ipv6Address
import Network.AWS.EC2.Types.Ipv6CidrAssociation
import Network.AWS.EC2.Types.Ipv6CidrBlock
import Network.AWS.EC2.Types.Ipv6Pool
import Network.AWS.EC2.Types.Ipv6PoolEc2Id
import Network.AWS.EC2.Types.Ipv6Range
import Network.AWS.EC2.Types.Ipv6SupportValue
import Network.AWS.EC2.Types.KernelId
import Network.AWS.EC2.Types.Key
import Network.AWS.EC2.Types.KeyName
import Network.AWS.EC2.Types.KeyPairId
import Network.AWS.EC2.Types.KeyPairInfo
import Network.AWS.EC2.Types.KeyPairName
import Network.AWS.EC2.Types.KmsKeyId
import Network.AWS.EC2.Types.LambdaFunctionArn
import Network.AWS.EC2.Types.LastError
import Network.AWS.EC2.Types.LaunchGroup
import Network.AWS.EC2.Types.LaunchPermission
import Network.AWS.EC2.Types.LaunchPermissionModifications
import Network.AWS.EC2.Types.LaunchSpecification
import Network.AWS.EC2.Types.LaunchTemplate
import Network.AWS.EC2.Types.LaunchTemplateAndOverridesResponse
import Network.AWS.EC2.Types.LaunchTemplateBlockDeviceMapping
import Network.AWS.EC2.Types.LaunchTemplateBlockDeviceMappingRequest
import Network.AWS.EC2.Types.LaunchTemplateCapacityReservationSpecificationRequest
import Network.AWS.EC2.Types.LaunchTemplateCapacityReservationSpecificationResponse
import Network.AWS.EC2.Types.LaunchTemplateConfig
import Network.AWS.EC2.Types.LaunchTemplateCpuOptions
import Network.AWS.EC2.Types.LaunchTemplateCpuOptionsRequest
import Network.AWS.EC2.Types.LaunchTemplateEbsBlockDevice
import Network.AWS.EC2.Types.LaunchTemplateEbsBlockDeviceRequest
import Network.AWS.EC2.Types.LaunchTemplateElasticInferenceAccelerator
import Network.AWS.EC2.Types.LaunchTemplateElasticInferenceAcceleratorResponse
import Network.AWS.EC2.Types.LaunchTemplateEnclaveOptions
import Network.AWS.EC2.Types.LaunchTemplateEnclaveOptionsRequest
import Network.AWS.EC2.Types.LaunchTemplateErrorCode
import Network.AWS.EC2.Types.LaunchTemplateHibernationOptions
import Network.AWS.EC2.Types.LaunchTemplateHibernationOptionsRequest
import Network.AWS.EC2.Types.LaunchTemplateHttpTokensState
import Network.AWS.EC2.Types.LaunchTemplateIamInstanceProfileSpecification
import Network.AWS.EC2.Types.LaunchTemplateIamInstanceProfileSpecificationRequest
import Network.AWS.EC2.Types.LaunchTemplateId
import Network.AWS.EC2.Types.LaunchTemplateInstanceMarketOptions
import Network.AWS.EC2.Types.LaunchTemplateInstanceMarketOptionsRequest
import Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataEndpointState
import Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataOptions
import Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataOptionsRequest
import Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataOptionsState
import Network.AWS.EC2.Types.LaunchTemplateInstanceNetworkInterfaceSpecification
import Network.AWS.EC2.Types.LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
import Network.AWS.EC2.Types.LaunchTemplateLicenseConfiguration
import Network.AWS.EC2.Types.LaunchTemplateLicenseConfigurationRequest
import Network.AWS.EC2.Types.LaunchTemplateName
import Network.AWS.EC2.Types.LaunchTemplateOverrides
import Network.AWS.EC2.Types.LaunchTemplatePlacement
import Network.AWS.EC2.Types.LaunchTemplatePlacementRequest
import Network.AWS.EC2.Types.LaunchTemplateSpecification
import Network.AWS.EC2.Types.LaunchTemplateSpotMarketOptions
import Network.AWS.EC2.Types.LaunchTemplateSpotMarketOptionsRequest
import Network.AWS.EC2.Types.LaunchTemplateTagSpecification
import Network.AWS.EC2.Types.LaunchTemplateTagSpecificationRequest
import Network.AWS.EC2.Types.LaunchTemplateVersion
import Network.AWS.EC2.Types.LaunchTemplatesMonitoring
import Network.AWS.EC2.Types.LaunchTemplatesMonitoringRequest
import Network.AWS.EC2.Types.LicenseConfiguration
import Network.AWS.EC2.Types.LicenseConfigurationRequest
import Network.AWS.EC2.Types.ListingState
import Network.AWS.EC2.Types.ListingStatus
import Network.AWS.EC2.Types.LoadBalancersConfig
import Network.AWS.EC2.Types.LoadPermission
import Network.AWS.EC2.Types.LoadPermissionModifications
import Network.AWS.EC2.Types.LoadPermissionRequest
import Network.AWS.EC2.Types.LocalGateway
import Network.AWS.EC2.Types.LocalGatewayId
import Network.AWS.EC2.Types.LocalGatewayRoute
import Network.AWS.EC2.Types.LocalGatewayRouteState
import Network.AWS.EC2.Types.LocalGatewayRouteTable
import Network.AWS.EC2.Types.LocalGatewayRouteTableArn
import Network.AWS.EC2.Types.LocalGatewayRouteTableId
import Network.AWS.EC2.Types.LocalGatewayRouteTableVirtualInterfaceGroupAssociation
import Network.AWS.EC2.Types.LocalGatewayRouteTableVirtualInterfaceGroupAssociationId
import Network.AWS.EC2.Types.LocalGatewayRouteTableVpcAssociation
import Network.AWS.EC2.Types.LocalGatewayRouteTableVpcAssociationId
import Network.AWS.EC2.Types.LocalGatewayRouteType
import Network.AWS.EC2.Types.LocalGatewayRoutetableId
import Network.AWS.EC2.Types.LocalGatewayVirtualInterface
import Network.AWS.EC2.Types.LocalGatewayVirtualInterfaceGroup
import Network.AWS.EC2.Types.LocalGatewayVirtualInterfaceGroupId
import Network.AWS.EC2.Types.LocalGatewayVirtualInterfaceId
import Network.AWS.EC2.Types.Location
import Network.AWS.EC2.Types.LocationType
import Network.AWS.EC2.Types.LogDestinationType
import Network.AWS.EC2.Types.ManagedPrefixList
import Network.AWS.EC2.Types.MarketType
import Network.AWS.EC2.Types.MaxPrice
import Network.AWS.EC2.Types.MaxTotalPrice
import Network.AWS.EC2.Types.MembershipType
import Network.AWS.EC2.Types.MemoryInfo
import Network.AWS.EC2.Types.Message
import Network.AWS.EC2.Types.ModifyAvailabilityZoneOptInStatus
import Network.AWS.EC2.Types.ModifyTransitGatewayOptions
import Network.AWS.EC2.Types.ModifyTransitGatewayVpcAttachmentRequestOptions
import Network.AWS.EC2.Types.ModifyVpnTunnelOptionsSpecification
import Network.AWS.EC2.Types.Monitoring
import Network.AWS.EC2.Types.MonitoringState
import Network.AWS.EC2.Types.MoveStatus
import Network.AWS.EC2.Types.MovingAddressStatus
import Network.AWS.EC2.Types.MulticastSupportValue
import Network.AWS.EC2.Types.Name
import Network.AWS.EC2.Types.NatGateway
import Network.AWS.EC2.Types.NatGatewayAddress
import Network.AWS.EC2.Types.NatGatewayId
import Network.AWS.EC2.Types.NatGatewayState
import Network.AWS.EC2.Types.NetworkAcl
import Network.AWS.EC2.Types.NetworkAclAssociation
import Network.AWS.EC2.Types.NetworkAclEntry
import Network.AWS.EC2.Types.NetworkAclId
import Network.AWS.EC2.Types.NetworkBorderGroup
import Network.AWS.EC2.Types.NetworkCardInfo
import Network.AWS.EC2.Types.NetworkId
import Network.AWS.EC2.Types.NetworkInfo
import Network.AWS.EC2.Types.NetworkInterface
import Network.AWS.EC2.Types.NetworkInterfaceAssociation
import Network.AWS.EC2.Types.NetworkInterfaceAttachment
import Network.AWS.EC2.Types.NetworkInterfaceAttachmentChanges
import Network.AWS.EC2.Types.NetworkInterfaceAttachmentId
import Network.AWS.EC2.Types.NetworkInterfaceAttribute
import Network.AWS.EC2.Types.NetworkInterfaceCreationType
import Network.AWS.EC2.Types.NetworkInterfaceId
import Network.AWS.EC2.Types.NetworkInterfaceIpv6Address
import Network.AWS.EC2.Types.NetworkInterfacePermission
import Network.AWS.EC2.Types.NetworkInterfacePermissionId
import Network.AWS.EC2.Types.NetworkInterfacePermissionState
import Network.AWS.EC2.Types.NetworkInterfacePermissionStateCode
import Network.AWS.EC2.Types.NetworkInterfacePrivateIpAddress
import Network.AWS.EC2.Types.NetworkInterfaceStatus
import Network.AWS.EC2.Types.NetworkInterfaceType
import Network.AWS.EC2.Types.NetworkPerformance
import Network.AWS.EC2.Types.NewAssociationId
import Network.AWS.EC2.Types.NewDhcpConfiguration
import Network.AWS.EC2.Types.NextToken
import Network.AWS.EC2.Types.NoDevice
import Network.AWS.EC2.Types.OccurrenceUnit
import Network.AWS.EC2.Types.OfferingClassType
import Network.AWS.EC2.Types.OfferingId
import Network.AWS.EC2.Types.OfferingTypeValues
import Network.AWS.EC2.Types.OnDemandAllocationStrategy
import Network.AWS.EC2.Types.OnDemandOptions
import Network.AWS.EC2.Types.OnDemandOptionsRequest
import Network.AWS.EC2.Types.OperationType
import Network.AWS.EC2.Types.OutpostArn
import Network.AWS.EC2.Types.Output
import Network.AWS.EC2.Types.OwnerAlias
import Network.AWS.EC2.Types.OwnerId
import Network.AWS.EC2.Types.PaymentOption
import Network.AWS.EC2.Types.PciId
import Network.AWS.EC2.Types.PeerOwnerId
import Network.AWS.EC2.Types.PeerRegion
import Network.AWS.EC2.Types.PeerVpcId
import Network.AWS.EC2.Types.PeeringAttachmentStatus
import Network.AWS.EC2.Types.PeeringConnectionOptions
import Network.AWS.EC2.Types.PeeringConnectionOptionsRequest
import Network.AWS.EC2.Types.PeeringTgwInfo
import Network.AWS.EC2.Types.PermissionGroup
import Network.AWS.EC2.Types.Phase1DHGroupNumbersListValue
import Network.AWS.EC2.Types.Phase1DHGroupNumbersRequestListValue
import Network.AWS.EC2.Types.Phase1EncryptionAlgorithmsListValue
import Network.AWS.EC2.Types.Phase1EncryptionAlgorithmsRequestListValue
import Network.AWS.EC2.Types.Phase1IntegrityAlgorithmsListValue
import Network.AWS.EC2.Types.Phase1IntegrityAlgorithmsRequestListValue
import Network.AWS.EC2.Types.Phase2DHGroupNumbersListValue
import Network.AWS.EC2.Types.Phase2DHGroupNumbersRequestListValue
import Network.AWS.EC2.Types.Phase2EncryptionAlgorithmsListValue
import Network.AWS.EC2.Types.Phase2EncryptionAlgorithmsRequestListValue
import Network.AWS.EC2.Types.Phase2IntegrityAlgorithmsListValue
import Network.AWS.EC2.Types.Phase2IntegrityAlgorithmsRequestListValue
import Network.AWS.EC2.Types.Placement
import Network.AWS.EC2.Types.PlacementGroup
import Network.AWS.EC2.Types.PlacementGroupId
import Network.AWS.EC2.Types.PlacementGroupInfo
import Network.AWS.EC2.Types.PlacementGroupName
import Network.AWS.EC2.Types.PlacementGroupState
import Network.AWS.EC2.Types.PlacementGroupStrategy
import Network.AWS.EC2.Types.PlacementResponse
import Network.AWS.EC2.Types.PlacementStrategy
import Network.AWS.EC2.Types.PlatformDetails
import Network.AWS.EC2.Types.PlatformValues
import Network.AWS.EC2.Types.PolicyDocument
import Network.AWS.EC2.Types.PoolArn
import Network.AWS.EC2.Types.PoolCidrBlock
import Network.AWS.EC2.Types.PoolId
import Network.AWS.EC2.Types.PortRange
import Network.AWS.EC2.Types.PrefixList
import Network.AWS.EC2.Types.PrefixListArn
import Network.AWS.EC2.Types.PrefixListAssociation
import Network.AWS.EC2.Types.PrefixListEntry
import Network.AWS.EC2.Types.PrefixListId
import Network.AWS.EC2.Types.PrefixListName
import Network.AWS.EC2.Types.PrefixListResourceId
import Network.AWS.EC2.Types.PrefixListState
import Network.AWS.EC2.Types.PreviousState
import Network.AWS.EC2.Types.PriceSchedule
import Network.AWS.EC2.Types.PriceScheduleSpecification
import Network.AWS.EC2.Types.PricingDetail
import Network.AWS.EC2.Types.Principal
import Network.AWS.EC2.Types.PrincipalArn
import Network.AWS.EC2.Types.PrincipalIdFormat
import Network.AWS.EC2.Types.PrincipalType
import Network.AWS.EC2.Types.PrivateDnsDetails
import Network.AWS.EC2.Types.PrivateDnsNameConfiguration
import Network.AWS.EC2.Types.PrivateIpAddress
import Network.AWS.EC2.Types.PrivateIpAddressSpecification
import Network.AWS.EC2.Types.ProcessorInfo
import Network.AWS.EC2.Types.ProductCode
import Network.AWS.EC2.Types.ProductCodeValues
import Network.AWS.EC2.Types.Progress
import Network.AWS.EC2.Types.PropagatingVgw
import Network.AWS.EC2.Types.PropagationDefaultRouteTableId
import Network.AWS.EC2.Types.Protocol
import Network.AWS.EC2.Types.ProvisionedBandwidth
import Network.AWS.EC2.Types.PublicDnsName
import Network.AWS.EC2.Types.PublicIp
import Network.AWS.EC2.Types.PublicIpAddress
import Network.AWS.EC2.Types.PublicIpv4Pool
import Network.AWS.EC2.Types.PublicIpv4PoolRange
import Network.AWS.EC2.Types.Purchase
import Network.AWS.EC2.Types.PurchaseRequest
import Network.AWS.EC2.Types.RIProductDescription
import Network.AWS.EC2.Types.RamDiskId
import Network.AWS.EC2.Types.RamdiskId
import Network.AWS.EC2.Types.ReasonMessage
import Network.AWS.EC2.Types.RecurringCharge
import Network.AWS.EC2.Types.RecurringChargeFrequency
import Network.AWS.EC2.Types.RegionInfo
import Network.AWS.EC2.Types.RegisterInstanceTagAttributeRequest
import Network.AWS.EC2.Types.RemovePrefixListEntry
import Network.AWS.EC2.Types.ReplacementStrategy
import Network.AWS.EC2.Types.ReportInstanceReasonCodes
import Network.AWS.EC2.Types.ReportStatusType
import Network.AWS.EC2.Types.RequestLaunchTemplateData
import Network.AWS.EC2.Types.RequestSpotLaunchSpecification
import Network.AWS.EC2.Types.Reservation
import Network.AWS.EC2.Types.ReservationId
import Network.AWS.EC2.Types.ReservationState
import Network.AWS.EC2.Types.ReservationValue
import Network.AWS.EC2.Types.ReservedInstanceLimitPrice
import Network.AWS.EC2.Types.ReservedInstanceReservationValue
import Network.AWS.EC2.Types.ReservedInstanceState
import Network.AWS.EC2.Types.ReservedInstances
import Network.AWS.EC2.Types.ReservedInstancesConfiguration
import Network.AWS.EC2.Types.ReservedInstancesId
import Network.AWS.EC2.Types.ReservedInstancesListing
import Network.AWS.EC2.Types.ReservedInstancesListingId
import Network.AWS.EC2.Types.ReservedInstancesModification
import Network.AWS.EC2.Types.ReservedInstancesModificationId
import Network.AWS.EC2.Types.ReservedInstancesModificationResult
import Network.AWS.EC2.Types.ReservedInstancesOffering
import Network.AWS.EC2.Types.ReservedInstancesOfferingId
import Network.AWS.EC2.Types.ResetFpgaImageAttributeName
import Network.AWS.EC2.Types.ResetImageAttributeName
import Network.AWS.EC2.Types.Resource
import Network.AWS.EC2.Types.ResourceArn
import Network.AWS.EC2.Types.ResourceId
import Network.AWS.EC2.Types.ResourceType
import Network.AWS.EC2.Types.ResponseError
import Network.AWS.EC2.Types.ResponseLaunchTemplateData
import Network.AWS.EC2.Types.RoleArn
import Network.AWS.EC2.Types.RoleName
import Network.AWS.EC2.Types.RootDeviceName
import Network.AWS.EC2.Types.RootDeviceType
import Network.AWS.EC2.Types.Route
import Network.AWS.EC2.Types.RouteGatewayId
import Network.AWS.EC2.Types.RouteOrigin
import Network.AWS.EC2.Types.RouteState
import Network.AWS.EC2.Types.RouteTable
import Network.AWS.EC2.Types.RouteTableAssociation
import Network.AWS.EC2.Types.RouteTableAssociationId
import Network.AWS.EC2.Types.RouteTableAssociationState
import Network.AWS.EC2.Types.RouteTableAssociationStateCode
import Network.AWS.EC2.Types.RouteTableId
import Network.AWS.EC2.Types.RuleAction
import Network.AWS.EC2.Types.RunInstancesMonitoringEnabled
import Network.AWS.EC2.Types.S3Bucket
import Network.AWS.EC2.Types.S3Key
import Network.AWS.EC2.Types.S3Prefix
import Network.AWS.EC2.Types.S3Storage
import Network.AWS.EC2.Types.SAMLProviderArn
import Network.AWS.EC2.Types.ScheduledInstance
import Network.AWS.EC2.Types.ScheduledInstanceAvailability
import Network.AWS.EC2.Types.ScheduledInstanceId
import Network.AWS.EC2.Types.ScheduledInstanceRecurrence
import Network.AWS.EC2.Types.ScheduledInstanceRecurrenceRequest
import Network.AWS.EC2.Types.ScheduledInstancesBlockDeviceMapping
import Network.AWS.EC2.Types.ScheduledInstancesEbs
import Network.AWS.EC2.Types.ScheduledInstancesIamInstanceProfile
import Network.AWS.EC2.Types.ScheduledInstancesIpv6Address
import Network.AWS.EC2.Types.ScheduledInstancesLaunchSpecification
import Network.AWS.EC2.Types.ScheduledInstancesMonitoring
import Network.AWS.EC2.Types.ScheduledInstancesNetworkInterface
import Network.AWS.EC2.Types.ScheduledInstancesPlacement
import Network.AWS.EC2.Types.ScheduledInstancesPrivateIpAddressConfig
import Network.AWS.EC2.Types.Scope
import Network.AWS.EC2.Types.SecurityGroup
import Network.AWS.EC2.Types.SecurityGroupId
import Network.AWS.EC2.Types.SecurityGroupIdentifier
import Network.AWS.EC2.Types.SecurityGroupName
import Network.AWS.EC2.Types.SecurityGroupReference
import Network.AWS.EC2.Types.SelfServicePortal
import Network.AWS.EC2.Types.SelfServicePortalUrl
import Network.AWS.EC2.Types.SelfServiceSAMLProviderArn
import Network.AWS.EC2.Types.SensitiveUserData
import Network.AWS.EC2.Types.ServerCertificateArn
import Network.AWS.EC2.Types.ServiceConfiguration
import Network.AWS.EC2.Types.ServiceDetail
import Network.AWS.EC2.Types.ServiceId
import Network.AWS.EC2.Types.ServiceState
import Network.AWS.EC2.Types.ServiceType
import Network.AWS.EC2.Types.ServiceTypeDetail
import Network.AWS.EC2.Types.ShutdownBehavior
import Network.AWS.EC2.Types.SlotDateTimeRangeRequest
import Network.AWS.EC2.Types.SlotStartTimeRangeRequest
import Network.AWS.EC2.Types.Snapshot
import Network.AWS.EC2.Types.SnapshotAttributeName
import Network.AWS.EC2.Types.SnapshotDetail
import Network.AWS.EC2.Types.SnapshotDiskContainer
import Network.AWS.EC2.Types.SnapshotId
import Network.AWS.EC2.Types.SnapshotInfo
import Network.AWS.EC2.Types.SnapshotState
import Network.AWS.EC2.Types.SnapshotTaskDetail
import Network.AWS.EC2.Types.SourceCidrBlock
import Network.AWS.EC2.Types.SourceSecurityGroupName
import Network.AWS.EC2.Types.SourceSecurityGroupOwnerId
import Network.AWS.EC2.Types.SourceVersion
import Network.AWS.EC2.Types.SpotAllocationStrategy
import Network.AWS.EC2.Types.SpotCapacityRebalance
import Network.AWS.EC2.Types.SpotDatafeedSubscription
import Network.AWS.EC2.Types.SpotFleetLaunchSpecification
import Network.AWS.EC2.Types.SpotFleetMonitoring
import Network.AWS.EC2.Types.SpotFleetRequestConfig
import Network.AWS.EC2.Types.SpotFleetRequestConfigData
import Network.AWS.EC2.Types.SpotFleetRequestId
import Network.AWS.EC2.Types.SpotFleetTagSpecification
import Network.AWS.EC2.Types.SpotInstanceInterruptionBehavior
import Network.AWS.EC2.Types.SpotInstanceRequest
import Network.AWS.EC2.Types.SpotInstanceRequestId
import Network.AWS.EC2.Types.SpotInstanceState
import Network.AWS.EC2.Types.SpotInstanceStateFault
import Network.AWS.EC2.Types.SpotInstanceStatus
import Network.AWS.EC2.Types.SpotInstanceType
import Network.AWS.EC2.Types.SpotMaintenanceStrategies
import Network.AWS.EC2.Types.SpotMarketOptions
import Network.AWS.EC2.Types.SpotOptions
import Network.AWS.EC2.Types.SpotOptionsRequest
import Network.AWS.EC2.Types.SpotPlacement
import Network.AWS.EC2.Types.SpotPrice
import Network.AWS.EC2.Types.SriovNetSupport
import Network.AWS.EC2.Types.StaleIpPermission
import Network.AWS.EC2.Types.StaleSecurityGroup
import Network.AWS.EC2.Types.State
import Network.AWS.EC2.Types.StateMessage
import Network.AWS.EC2.Types.StateReason
import Network.AWS.EC2.Types.StatusMessage
import Network.AWS.EC2.Types.StatusName
import Network.AWS.EC2.Types.StatusType
import Network.AWS.EC2.Types.Storage
import Network.AWS.EC2.Types.StorageLocation
import Network.AWS.EC2.Types.String
import Network.AWS.EC2.Types.Subnet
import Network.AWS.EC2.Types.SubnetAssociation
import Network.AWS.EC2.Types.SubnetCidrAssociationId
import Network.AWS.EC2.Types.SubnetCidrBlockState
import Network.AWS.EC2.Types.SubnetCidrBlockStateCode
import Network.AWS.EC2.Types.SubnetId
import Network.AWS.EC2.Types.SubnetIpv6CidrBlockAssociation
import Network.AWS.EC2.Types.SubnetState
import Network.AWS.EC2.Types.SubsystemId
import Network.AWS.EC2.Types.SubsystemVendorId
import Network.AWS.EC2.Types.SuccessfulInstanceCreditSpecificationItem
import Network.AWS.EC2.Types.SuccessfulQueuedPurchaseDeletion
import Network.AWS.EC2.Types.SummaryStatus
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.TagDescription
import Network.AWS.EC2.Types.TagSpecification
import Network.AWS.EC2.Types.TaggableResourceId
import Network.AWS.EC2.Types.TargetCapacitySpecification
import Network.AWS.EC2.Types.TargetCapacitySpecificationRequest
import Network.AWS.EC2.Types.TargetConfiguration
import Network.AWS.EC2.Types.TargetConfigurationRequest
import Network.AWS.EC2.Types.TargetGroup
import Network.AWS.EC2.Types.TargetGroupsConfig
import Network.AWS.EC2.Types.TargetNetwork
import Network.AWS.EC2.Types.TargetReservationValue
import Network.AWS.EC2.Types.TargetVpcSubnetId
import Network.AWS.EC2.Types.TelemetryStatus
import Network.AWS.EC2.Types.Tenancy
import Network.AWS.EC2.Types.TerminateConnectionStatus
import Network.AWS.EC2.Types.Timestamp
import Network.AWS.EC2.Types.TrafficDirection
import Network.AWS.EC2.Types.TrafficMirrorFilter
import Network.AWS.EC2.Types.TrafficMirrorFilterId
import Network.AWS.EC2.Types.TrafficMirrorFilterRule
import Network.AWS.EC2.Types.TrafficMirrorFilterRuleField
import Network.AWS.EC2.Types.TrafficMirrorFilterRuleId
import Network.AWS.EC2.Types.TrafficMirrorNetworkService
import Network.AWS.EC2.Types.TrafficMirrorPortRange
import Network.AWS.EC2.Types.TrafficMirrorPortRangeRequest
import Network.AWS.EC2.Types.TrafficMirrorRuleAction
import Network.AWS.EC2.Types.TrafficMirrorSession
import Network.AWS.EC2.Types.TrafficMirrorSessionField
import Network.AWS.EC2.Types.TrafficMirrorSessionId
import Network.AWS.EC2.Types.TrafficMirrorTarget
import Network.AWS.EC2.Types.TrafficMirrorTargetId
import Network.AWS.EC2.Types.TrafficMirrorTargetType
import Network.AWS.EC2.Types.TrafficType
import Network.AWS.EC2.Types.TransitAssociationGatewayId
import Network.AWS.EC2.Types.TransitGateway
import Network.AWS.EC2.Types.TransitGatewayArn
import Network.AWS.EC2.Types.TransitGatewayAssociation
import Network.AWS.EC2.Types.TransitGatewayAssociationState
import Network.AWS.EC2.Types.TransitGatewayAttachment
import Network.AWS.EC2.Types.TransitGatewayAttachmentAssociation
import Network.AWS.EC2.Types.TransitGatewayAttachmentId
import Network.AWS.EC2.Types.TransitGatewayAttachmentPropagation
import Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType
import Network.AWS.EC2.Types.TransitGatewayAttachmentState
import Network.AWS.EC2.Types.TransitGatewayId
import Network.AWS.EC2.Types.TransitGatewayMulitcastDomainAssociationState
import Network.AWS.EC2.Types.TransitGatewayMulticastDeregisteredGroupMembers
import Network.AWS.EC2.Types.TransitGatewayMulticastDeregisteredGroupSources
import Network.AWS.EC2.Types.TransitGatewayMulticastDomain
import Network.AWS.EC2.Types.TransitGatewayMulticastDomainAssociation
import Network.AWS.EC2.Types.TransitGatewayMulticastDomainAssociations
import Network.AWS.EC2.Types.TransitGatewayMulticastDomainId
import Network.AWS.EC2.Types.TransitGatewayMulticastDomainState
import Network.AWS.EC2.Types.TransitGatewayMulticastGroup
import Network.AWS.EC2.Types.TransitGatewayMulticastRegisteredGroupMembers
import Network.AWS.EC2.Types.TransitGatewayMulticastRegisteredGroupSources
import Network.AWS.EC2.Types.TransitGatewayOptions
import Network.AWS.EC2.Types.TransitGatewayPeeringAttachment
import Network.AWS.EC2.Types.TransitGatewayPrefixListAttachment
import Network.AWS.EC2.Types.TransitGatewayPrefixListReference
import Network.AWS.EC2.Types.TransitGatewayPrefixListReferenceState
import Network.AWS.EC2.Types.TransitGatewayPropagation
import Network.AWS.EC2.Types.TransitGatewayPropagationState
import Network.AWS.EC2.Types.TransitGatewayRequestOptions
import Network.AWS.EC2.Types.TransitGatewayRoute
import Network.AWS.EC2.Types.TransitGatewayRouteAttachment
import Network.AWS.EC2.Types.TransitGatewayRouteState
import Network.AWS.EC2.Types.TransitGatewayRouteTable
import Network.AWS.EC2.Types.TransitGatewayRouteTableAssociation
import Network.AWS.EC2.Types.TransitGatewayRouteTableId
import Network.AWS.EC2.Types.TransitGatewayRouteTablePropagation
import Network.AWS.EC2.Types.TransitGatewayRouteTableState
import Network.AWS.EC2.Types.TransitGatewayRouteType
import Network.AWS.EC2.Types.TransitGatewayState
import Network.AWS.EC2.Types.TransitGatewayVpcAttachment
import Network.AWS.EC2.Types.TransitGatewayVpcAttachmentOptions
import Network.AWS.EC2.Types.TransportProtocol
import Network.AWS.EC2.Types.TunnelInsideIpVersion
import Network.AWS.EC2.Types.TunnelOption
import Network.AWS.EC2.Types.Type
import Network.AWS.EC2.Types.UnlimitedSupportedInstanceFamily
import Network.AWS.EC2.Types.UnsuccessfulInstanceCreditSpecificationErrorCode
import Network.AWS.EC2.Types.UnsuccessfulInstanceCreditSpecificationItem
import Network.AWS.EC2.Types.UnsuccessfulInstanceCreditSpecificationItemError
import Network.AWS.EC2.Types.UnsuccessfulItem
import Network.AWS.EC2.Types.UnsuccessfulItemError
import Network.AWS.EC2.Types.UsageClassType
import Network.AWS.EC2.Types.UsageOperation
import Network.AWS.EC2.Types.UserBucket
import Network.AWS.EC2.Types.UserBucketDetails
import Network.AWS.EC2.Types.UserData
import Network.AWS.EC2.Types.UserId
import Network.AWS.EC2.Types.UserIdGroupPair
import Network.AWS.EC2.Types.Username
import Network.AWS.EC2.Types.VCpuInfo
import Network.AWS.EC2.Types.ValidationError
import Network.AWS.EC2.Types.ValidationWarning
import Network.AWS.EC2.Types.Value
import Network.AWS.EC2.Types.VendorId
import Network.AWS.EC2.Types.Version
import Network.AWS.EC2.Types.VersionDescription
import Network.AWS.EC2.Types.VgwTelemetry
import Network.AWS.EC2.Types.VirtualName
import Network.AWS.EC2.Types.VirtualizationType
import Network.AWS.EC2.Types.Volume
import Network.AWS.EC2.Types.VolumeAttachment
import Network.AWS.EC2.Types.VolumeAttachmentState
import Network.AWS.EC2.Types.VolumeAttributeName
import Network.AWS.EC2.Types.VolumeDetail
import Network.AWS.EC2.Types.VolumeId
import Network.AWS.EC2.Types.VolumeModification
import Network.AWS.EC2.Types.VolumeModificationState
import Network.AWS.EC2.Types.VolumeState
import Network.AWS.EC2.Types.VolumeStatusAction
import Network.AWS.EC2.Types.VolumeStatusAttachmentStatus
import Network.AWS.EC2.Types.VolumeStatusDetails
import Network.AWS.EC2.Types.VolumeStatusEvent
import Network.AWS.EC2.Types.VolumeStatusInfo
import Network.AWS.EC2.Types.VolumeStatusInfoStatus
import Network.AWS.EC2.Types.VolumeStatusItem
import Network.AWS.EC2.Types.VolumeStatusName
import Network.AWS.EC2.Types.VolumeType
import Network.AWS.EC2.Types.Vpc
import Network.AWS.EC2.Types.VpcAttachment
import Network.AWS.EC2.Types.VpcAttributeName
import Network.AWS.EC2.Types.VpcCidrBlockAssociation
import Network.AWS.EC2.Types.VpcCidrBlockState
import Network.AWS.EC2.Types.VpcCidrBlockStateCode
import Network.AWS.EC2.Types.VpcClassicLink
import Network.AWS.EC2.Types.VpcEndpoint
import Network.AWS.EC2.Types.VpcEndpointConnection
import Network.AWS.EC2.Types.VpcEndpointId
import Network.AWS.EC2.Types.VpcEndpointServiceId
import Network.AWS.EC2.Types.VpcEndpointType
import Network.AWS.EC2.Types.VpcFlowLogId
import Network.AWS.EC2.Types.VpcId
import Network.AWS.EC2.Types.VpcIpv6CidrBlockAssociation
import Network.AWS.EC2.Types.VpcPeeringConnection
import Network.AWS.EC2.Types.VpcPeeringConnectionId
import Network.AWS.EC2.Types.VpcPeeringConnectionOptionsDescription
import Network.AWS.EC2.Types.VpcPeeringConnectionStateReason
import Network.AWS.EC2.Types.VpcPeeringConnectionStateReasonCode
import Network.AWS.EC2.Types.VpcPeeringConnectionVpcInfo
import Network.AWS.EC2.Types.VpcState
import Network.AWS.EC2.Types.VpcTenancy
import Network.AWS.EC2.Types.VpnConnection
import Network.AWS.EC2.Types.VpnConnectionId
import Network.AWS.EC2.Types.VpnConnectionOptions
import Network.AWS.EC2.Types.VpnConnectionOptionsSpecification
import Network.AWS.EC2.Types.VpnEcmpSupportValue
import Network.AWS.EC2.Types.VpnGateway
import Network.AWS.EC2.Types.VpnGatewayId
import Network.AWS.EC2.Types.VpnProtocol
import Network.AWS.EC2.Types.VpnState
import Network.AWS.EC2.Types.VpnStaticRoute
import Network.AWS.EC2.Types.VpnStaticRouteSource
import Network.AWS.EC2.Types.VpnTunnelOptionsSpecification
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-11-15@ of the Amazon Elastic Compute Cloud SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "EC2",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "ec2",
      Core._svcVersion = "2016-11-15",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseXMLError "EC2",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Lens.has
          (Core.hasCode "RequestLimitExceeded" Core.. Core.hasStatus 503)
          e =
        Core.Just "request_limit_exceeded"
      | Lens.has
          (Core.hasCode "EC2ThrottledException" Core.. Core.hasStatus 503)
          e =
        Core.Just "ec2_throttled_exception"
      | Core.otherwise = Core.Nothing
